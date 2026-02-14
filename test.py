import pytest
from pathlib import Path
from unittest.mock import mock_open, patch
from main import parse_r_dependencies, inject_wrapper_code, remove_setwd


def test_dependency_parsing_various_formats():
  """Tests that all common R library loading patterns are caught."""
  r_content = """
    library(ggplot2)
    require("dplyr")
    lapply(c("tidyr", "purrr"), library, character.only=T)
    x <- c("tidyverse", "pairwise", "catR") # select packages
    lapply(x, require, character.only = TRUE) # load packages
    data <- data.table::fread("file.csv")
    """
  deps = parse_r_dependencies(r_content)

  for d in ["ggplot2", "dplyr", "tidyr", "purrr", "tidyverse", "pairwise", "catR", "data.table"]:
    assert d in deps
  assert "base" not in deps  # Should be filtered out


def test_remove_setwd_logic():
  """Ensures setwd calls are stripped regardless of spacing."""
  r_content = 'print("start")\n  setwd("/home/user/data") \nread.csv("test.csv")'
  cleaned = remove_setwd(r_content)

  assert "setwd" not in cleaned
  assert 'read.csv("test.csv")' in cleaned


def test_wrapper_injection_placement_default():
  """Checks that wrapper is prepended at the top for standard scripts."""
  r_content = 'library(utils)\nwrite.csv(df, "out.csv")'
  injected = inject_wrapper_code(r_content)

  assert injected.startswith("\n# --- Injected")
  assert "utils::write.csv" in injected


def test_data_extraction():
  from main import parse_io_schedule

  r_content = """
    raw_data <- read.csv("input/raw_metrics.csv")
    clean_df <- some_processing(raw_data)
    write.csv(clean_df, "output/processed_results.csv")
    saveRDS(clean_df, "models/final_model.rds")
    validation <- fread("data/val_set.csv")
  """
  schedule = parse_io_schedule(r_content)
  assert len(schedule) == 4
  assert schedule[0]["path"] == "input/raw_metrics.csv"
  assert schedule[0]["type"] == "read"
  
  assert schedule[1]["function"] == "write.csv"
  assert schedule[1]["type"] == "write"
  
  assert schedule[2]["path"] == "models/final_model.rds"
  
  assert schedule[3]["function"] == "fread"
  assert schedule[3]["type"] == "read"


def test_data_discovery():
  from main import discover_data_files
  pass

def test_schedule_matching():
  from main import parse_io_schedule

def test_pure_logic_mapping():
  from main import map_schedule_to_data
  available = ["raw/2023_data_FINAL.csv", "meta/labels.xlsx"]
    
  schedule = [{"type": "read", "path": "data/data.csv"}]
    
  result = map_schedule_to_data(schedule, available, threshold=0.4)
    
  assert result[0]["actual_rel_path"] == "raw/2023_data_FINAL.csv"


@patch("pathlib.Path.read_text")
@patch("pathlib.Path.write_text")
def test_file_processing_workflow(mock_write, mock_read):
  """Mocks the entire file lifecycle to ensure code is read, modified, and saved."""
  from main import process_r_file

  mock_path = Path("test_script.R")
  mock_read.return_value = 'library(MASS)\nwrite.csv(x, "f.csv")'

  deps = process_r_file(mock_path)

  assert "MASS" in deps

  written_content = mock_write.call_args[0][0]
  assert "auto_mkdir_wrapper" in written_content
  assert "library(MASS)" in written_content
