from collections import deque, defaultdict
import difflib
import logging
from pathlib import Path
import re
import subprocess

logging.basicConfig(level=logging.INFO, format="%(levelname)s: %(message)s")
logger = logging.getLogger(__name__)


R_WRAPPER_CODE = """
# --- Injected Wrappers 
.auto_mkdir_wrapper <- function(orig_func) {
  function(x, file, ...) {
    if (missing(file)) {
        # Handle cases where file is passed as first arg (x) implicitly
        if (is.character(x)) file <- x
    }
    if (!is.null(file) && is.character(file)) {
      dir_path <- dirname(file)
      if (!dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
      }
    }
    orig_func(x, file, ...)
  }
}

.auto_read_wrapper <- function(orig_func) {
  function(file, ...) {
    orig_func(file, ..., fileEncoding = "UTF-8-BOM")
  }
}

read.csv   <- .auto_read_wrapper(utils::read.csv)
read.csv2  <- .auto_read_wrapper(utils::read.csv2)

write.csv  <- .auto_mkdir_wrapper(utils::write.csv)
write.csv2 <- .auto_mkdir_wrapper(utils::write.csv2)
saveRDS    <- .auto_mkdir_wrapper(base::saveRDS)
# --- End Injection
"""

IGNORED_PACKAGES: set[str] = {"base", "utils", "stats", "methods", "graphics", "grDevices", "datasets"}


def parse_r_dependencies(script_text: str) -> set[str]:
  """
  Find all library dependencies of the R script. libraries can be given as follows: library(...), x <- c("...", ...); lapply(x, require|library, ...)
  """
  dependencies = set()

  std_pattern = r'(?:library|require)\s*\(\s*["\']?([a-zA-Z0-9\._]+)["\']?\s*\)'
  dependencies.update(re.findall(std_pattern, script_text))

  ns_pattern = r"([a-zA-Z0-9\._]+)::"
  dependencies.update(re.findall(ns_pattern, script_text))

  lapply_calls = re.findall(r"(?:lapply|sapply|vapply)\s*\(\s*(.*?)\s*,\s*(?:require|library)", script_text, re.DOTALL)
  print(f"Lapply_calls: {lapply_calls}")

  for first_arg in lapply_calls:
    first_arg = first_arg.strip()

    if first_arg.startswith("c("):
      pkgs = re.findall(r'["\'](.*?)["\']', first_arg)
      dependencies.update(pkgs)

    else:
      var_name = re.escape(first_arg)
      var_pattern = rf"{var_name}\s*(?:<-|=)\s*c\s*\((.*?)\)"
      match = re.search(var_pattern, script_text, re.DOTALL)
      if match:
        pkgs = re.findall(r'["\'](.*?)["\']', match.group(1))
        dependencies.update(pkgs)

  return {d for d in dependencies if d not in IGNORED_PACKAGES}


def parse_io_schedule(script_text: str) -> list[dict[str, str]]:
  read_funcs = r"read\.csv2?|read\.delim|read\.table|readRDS|read_csv|read_delim|fread"
  write_funcs = r"write\.csv2?|write\.table|saveRDS|write_csv|fwrite"

  pattern = rf"\b({read_funcs}|{write_funcs})\s*\(\s*(?:[^,\n]+,\s*)?(?:file\s*=\s*)?[\"']([^\"']+)[\"']"

  schedule = []
  for match in re.finditer(pattern, script_text):
    func_name, file_path = match.groups()
    op_type = "read" if re.match(read_funcs, func_name) else "write"

    schedule.append({"type": op_type, "function": func_name, "path": file_path})

  return schedule


def gather_all_scripts_io(repo_root: Path, r_files: list[Path]) -> dict:
  """
  Map all files to their r/w schedule.
  """
  scripts_info = {}

  for file_path in r_files:
    content = file_path.read_text(errors="ignore")

    raw_schedule = parse_io_schedule(content)

    relative_name = str(file_path.relative_to(repo_root))
    scripts_info[relative_name] = {
      "reads": {item["path"] for item in raw_schedule if item["type"] == "read"},
      "writes": {item["path"] for item in raw_schedule if item["type"] == "write"},
    }

  return scripts_info


def build_script_graph(scripts_info: dict):
  """
  scripts_info: { "script_a.R": {"reads": [...], "writes": [...]}, ... }
  """
  adj_list = defaultdict(list)
  in_degree = {script: 0 for script in scripts_info}

  file_producers = {}
  for script, io in scripts_info.items():
    for out_file in io["writes"]:
      file_producers[out_file] = script

  for consumer_script, io in scripts_info.items():
    for in_file in io["reads"]:
      if in_file in file_producers:
        producer_script = file_producers[in_file]
        if producer_script != consumer_script:
          adj_list[producer_script].append(consumer_script)
          in_degree[consumer_script] += 1

  return adj_list, in_degree


def topological_sort(adj_list, in_degree):
  queue = deque([u for u in in_degree if in_degree[u] == 0])
  ordered_list = []

  while queue:
    u = queue.popleft()
    ordered_list.append(u)
    for v in adj_list[u]:
      in_degree[v] -= 1
      if in_degree[v] == 0:
        queue.append(v)

  return ordered_list


# def find_matching_data(target:str, schedule : dict[str,str], data_files : list[str]) -> str:


def discover_data_files(repo_path: Path) -> list[str]:  # TODO: return a dict {analysis: [R,py,etc..], data : [csv, xls, ..], supp : [pdf, docx]}
  DATA_EXTS = {".csv", ".rds", ".Rda", ".xlsx", ".xls", ".txt", ".tsv"}
  return [str(p.relative_to(repo_path)) for p in repo_path.rglob("*") if p.suffix.lower() in DATA_EXTS]


def map_schedule_to_data(schedule: list[dict], available_data: list[str], threshold: float = 0.5):
  """Matches R code paths to a provided list of filtered data files."""
  for item in schedule:
    if item["type"] == "read":
      target = item["path"]
      matches = difflib.get_close_matches(target, available_data, n=1, cutoff=threshold)

      item["actual_rel_path"] = matches[0] if matches else None
  return schedule


def get_io_lineage(schedule: list[dict]) -> tuple[set[str], set[str]]:
  """Separates external dependencies from internal intermediates (i.e. read write cancels itself.)"""
  seen_w, ext, intl = set(), set(), set()
  for i in schedule:
    p, is_r = i["path"], i["type"] == "read"
    if is_r:
      (intl if p in seen_w else ext).add(p)
    else:
      seen_w.add(p)
  return ext, intl


def remove_setwd(content: str) -> str:
  """Removes hardcoded setwd() calls to prevent path errors."""
  return re.sub(r"^[ \t]*setwd\(.*?\).*\n?", "", content, flags=re.MULTILINE)


def inject_wrapper_code(content: str) -> str:
  """
  Injects wrapper code to ensure dir creation and reading consistency.
  """
  lines = content.splitlines()
  insert_index = 0

  for i, line in enumerate(lines):
    clean_line = line.replace(" ", "")
    if "rm(list=ls())" in clean_line or "rm(list=ls(all=TRUE))" in clean_line:
      insert_index = i + 1
      break

  lines.insert(insert_index, R_WRAPPER_CODE)
  return "\n".join(lines)


def process_r_file(file_path: Path) -> set[str]:
  """
  Reads an R file, parses deps, cleans setwd, injects wrapper, and rewrites the file.
  Returns found dependencies.
  """
  try:
    content = file_path.read_text(errors="ignore")

    deps = parse_r_dependencies(content)

    content = remove_setwd(content)
    content = inject_wrapper_code(content)

    file_path.write_text(content, encoding="utf-8")
    return deps
  except Exception as e:
    logger.error(f"Failed to process {file_path}: {e}")
    return set()


def generate_nix_shell(repo_path: Path, packages: list[str]) -> Path:
  """Generates a shell.nix file in the repo path."""

  nix_pkgs_str = "\n    ".join([f"rPackages.{p}" for p in sorted(packages)])

  nix_content = f"""
{{ pkgs ? import <nixpkgs> {{}} }}:

pkgs.mkShell {{
  buildInputs = with pkgs; [
    R
    {nix_pkgs_str}
  ];

  shellHook = ''
    export R_LIBS_USER="/var/empty"
    export R_LIBS=""
  '';
}}
"""
  shell_path = repo_path / "shell.nix"
  shell_path.write_text(nix_content)
  logger.info(f"Generated Nix shell at {shell_path}")
  return shell_path


def run_script_in_nix(repo_path: Path, script_path: Path):
  relative_script = script_path.relative_to(repo_path)

  logger.info(f"Running {relative_script} in Nix environment...")

  cmd = ["nix-shell", "--run", f"Rscript '{relative_script}'"]

  try:
    subprocess.run(cmd, cwd=repo_path, check=True)
    logger.info(f"Success: {relative_script}")
  except subprocess.CalledProcessError as e:
    logger.error(f"Execution failed for {relative_script}: {e}")


def main(osf_id: str, base_dir: str = "."):
  base_path = Path(base_dir) / osf_id

  if not base_path.exists():
    logger.error(f"Directory {base_path} does not exist.")
    return

  try:
    repo_root = next(p for p in base_path.iterdir() if p.is_dir())
  except StopIteration:
    logger.error("No subdirectories found in OSF folder.")
    return

  logger.info(f"Processing repository at: {repo_root}")

  # I think we sort alphabetically, but   we would also have to do a topological sort based on the r/w order.
  r_files = sorted(list(repo_root.rglob("*.R")) + list(repo_root.rglob("*.Rmd")))

  scripts_io_map = gather_all_scripts_io(repo_root, r_files)
  adj_list, in_degree = build_script_graph(scripts_io_map)
  execution_order = topological_sort(adj_list, in_degree)

  print(execution_order)

  if not r_files:
    logger.warning("No R files found.")
    return

  all_dependencies = set()
  for r_file in r_files:
    file_deps = process_r_file(r_file)
    all_dependencies.update(file_deps)

  logger.info(f"Identified dependencies: {all_dependencies}")

  generate_nix_shell(repo_root, list(all_dependencies))

  for r_file in r_files:
    run_script_in_nix(repo_root, r_file)


if __name__ == "__main__":
  # Ensure you have a folder structure: ./wrgkb/<repo_folder>/script.R
  main(osf_id="5hrgp", base_dir="examples")
