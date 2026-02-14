import pathlib
from importlib.resources import path
import re
import subprocess
from pathlib import Path
import os
import difflib


def parse_all_r_deps(script_text : str):
    """
    Parses calls such as library, library(c(...)), lapply(x, require, ...) etc..
    """
    dependencies = set()

    lapply_match = re.search(
        r"lapply\(\s*([a-zA-Z0-9._]+)\s*,\s*(?:require|library)", script_text
    )
    if lapply_match:
        dep_var = re.escape(lapply_match.group(1))
        vector_pattern = rf"{dep_var}\s*(?:<-|=)\s*c\s*\((.*?)\)"
        vector_match = re.search(vector_pattern, script_text, re.DOTALL)
        if vector_match:
            pkgs = re.findall(r'["\'](.*?)["\']', vector_match.group(1))
            dependencies.update(pkgs)

    standard_calls = re.findall(
        r'(?:library|require)\(([a-zA-Z0-9._"\'\s]+)\)', script_text
    )
    for call in standard_calls:
        clean_pkg = call.strip().strip('"').strip("'")
        if clean_pkg and "=" not in clean_pkg:
            dependencies.add(clean_pkg)

    namespaced_calls = re.findall(r"([a-zA-Z0-9._]+)::", script_text)
    dependencies.update(namespaced_calls)

    return sorted(list(dependencies))


def set_wd(filepath : str) -> None:
    with open(filepath, "r") as f:
        content = f.read()
    clean_content = re.sub(r"^[ \t]*setwd\(.*?\).*\n?", "", content, flags=re.MULTILINE)
    with open(filepath, "w") as f:
        f.write(clean_content)


def get_files_by_ext(dir: str, ext: str = ".csv"):
    files = []
    for f in os.listdir(dir):
        if ext in f:
            files.append(f)
    return files


def map_r_imports_to_files(r_script_content : str, actual_csv_files : list[str]):
    pattern = r'(?:read\.csv2?|read_csv|read\.delim|fread)\s*\(\s*["\']([^"\']+)["\']'

    extracted_paths = re.findall(pattern, r_script_content)

    results = {}
    for path in extracted_paths:
        filename_only = os.path.basename(path)

        match = difflib.get_close_matches(
            filename_only, actual_csv_files, n=1, cutoff=0.4
        )

        results[path] = match[0] if match else "No Match Found"

    return results


def identify_r_dependencies(repo_path):
    print("--- Scanning for R dependencies ---")
    pkgs = set()
    r_files = list(Path(repo_path).rglob("*.R")) + list(Path(repo_path).rglob("*.Rmd"))

    lib_regex = re.compile(r"(?:library|require)\((['\"]?)([a-zA-Z0-9.]+)\1\)")

    for r_file in r_files:
        set_wd(r_file)
        with open(r_file, "r", errors="ignore") as f:
            content = f.read()
            matches = lib_regex.findall(content)
            libs = parse_all_r_deps(content)
            print(libs)
            for pkg in libs:
                pkgs.add(pkg)
    print(f"Found dependencies {pkgs} for files {r_files}")
    return list(pkgs), r_files


def generate_nix_shell(repo_path: str, packages: list[str]):
    print("--- Generating shell.nix ---")
    nix_pkgs = [f"rPackages.{p}" for p in packages]

    nix_content = f"""
{{ pkgs ? import <nixpkgs> {{}} }}:

pkgs.mkShell {{
  buildInputs = with pkgs; [
    R
    {" ".join(nix_pkgs)}
  ];

  shellHook = ''
    export R_LIBS_USER="/var/empty"
    export R_LIBS=""
  '';
}}
"""
    shell_path = Path(repo_path) / "shell.nix"
    shell_path.write_text(nix_content)
    return shell_path


def run_with_nix(repo_path, r_script):
    print(f"--- Attempting to run {r_script.name} via Nix ---")
    r_script = Path(r_script).relative_to(repo_path)
    innder_cmd = f"Rscript '{r_script}'"
    try:
        subprocess.run(["nix-shell", "--run", innder_cmd], cwd=repo_path, check=True)
    except subprocess.CalledProcessError as e:
        print(f"Execution failed: {e}")


def main(osf_id):
    deps, files = identify_r_dependencies(osf_id)
    if not files:
        print("No R code found. Exiting.")
        return

    print(f"Found packages: {deps}")
    repo_path = osf_id + "/" + os.listdir(osf_id)[0]
    generate_nix_shell(repo_path, deps)

    if files:
        run_with_nix(repo_path, files[0]) # TODO: detect a starting point, and add support for multiple files

    # TODO: read errors produced by R.
    # TODO: support for polyglot scripts.


if __name__ == "__main__":
    # main("5hrgp")
    main(osf_id="b476z")
    # with open("5hrgp/1023-0224 Comparing Adaptive Testing Algorithms for Inclusive Education/CAT simulations estimated and binary search - OSF.R", mode = "r") as f:
    #     content = f.read()
    # libs = parse_vectorized_r_deps(content)
    # print(libs)
