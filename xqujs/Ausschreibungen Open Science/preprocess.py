import csv
import os
from pathlib import Path

try:
  from langdetect import detect, DetectorFactory

  DetectorFactory.seed = 0
  HAS_LANGDETECT = True
except ImportError as e:
  print(f"Error importing langdetect: {e}")
  HAS_LANGDETECT = False

CONTENT_SEPARATOR = "=" * 65  # There is another sligthly longer content separator that subsumes this one.
HEADER_SEPARATOR = "-" * 65


def clean_text(text: str) -> str:
  return text.strip().replace("\n", " ").replace("\r", " ")


def detect_language(text: str) -> str:
  if not text.strip():
    return "empty"

  if not HAS_LANGDETECT:
    return "NA"

  try:
    return detect(text)
  except Exception:
    return "unknown"


def classify_subject(text: str) -> str:  # TODO: this does not work optimally, fix.
  text = text.lower()

  mapping = {
    "Education": ["education", "pädagogik", "lehramt", "didaktik", "bildung"],
    "Special Education": [
      "special education",
      "sonderpädagogik",
      "förderpädagogik",
      "heilpädagogik",
      "inclusion",
      "inklusion",
    ],
    "Psychology": ["psychology", "psychologie", "psychologisch"],
    "Social Work": ["social work", "soziale arbeit", "sozialarbeit"],
  }

  for category, keywords in mapping.items():
    if any(kw in text for kw in keywords):
      return category

  return "Other"


def parse(filepath: str):
  path_obj = Path(filepath)

  with open(path_obj, "r", encoding="utf-8") as file:
    content = file.read()

  year = f"20{path_obj.parent.name}"
  num = path_obj.stem

  if CONTENT_SEPARATOR not in content:
    print(f"Warning: Separator not found in {filepath}")
    return []

  sections = content.split(CONTENT_SEPARATOR)

  if len(sections) < 6:
    print(f"Warning: Unexpected file structure in {filepath}")
    return []

  entries = []
  toc_lines = sections[4].strip().split("\n")
  for line in toc_lines:
    if line.startswith("*"):
      entries.append(line[2:].strip())

  all_text_blocks = []
  for section in sections[5:]:
    blocks = [b for b in section.split(HEADER_SEPARATOR) if b.strip()]
    all_text_blocks.extend(blocks)

  results = []
  for header, raw_text in zip(entries, all_text_blocks):
    fields = header.split(" - ", 1)
    university = fields[0]
    position = fields[1] if len(fields) > 1 else "Unknown"

    lines = raw_text.strip().split("\n")
    actual_content = " ".join(lines[1:]) if len(lines) > 2 else "".join(lines)
    cleaned_content = clean_text(actual_content)
    subject_area = classify_subject(position)

    results.append(
      {
        "university": university,
        "position": position,
        "subject_area": subject_area,
        "fulltext": cleaned_content,
        "year": year,
        "language": detect_language(cleaned_content),
        "num": num,
      }
    )

  return results


documents = []
for j in [24, 25, 26]:
  for i in range(1, 30):
    filepath = f"data/{j}/{i}.txt"
    if os.path.exists(filepath):
      documents.append(filepath)


if __name__ == "__main__":
  all_results = []
  for doc in documents:
    all_results.extend(parse(doc))

  fieldnames = [
    "university",
    "position",
    "subject_area",
    "fulltext",
    "year",
    "language",
    "num",
  ]
  with open("data/all_data.csv", "w", encoding="utf-8", newline="") as csvfile:
    writer = csv.DictWriter(csvfile, fieldnames=fieldnames, delimiter=";")
    writer.writeheader()
    for row in all_results:
      writer.writerow(row)
  print(f"Saved {len(all_results)} entries")
