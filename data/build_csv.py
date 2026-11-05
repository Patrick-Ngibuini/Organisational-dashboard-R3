"""Flatten seed.json into one CSV per node table and per edge table.

Run from the repository root or from this folder:

    python assignment/data/build_csv.py

The CSV files are a convenience for students who prefer a relational start.
seed.json stays the source of truth; see ../DATAMODEL.md for the mapping.
"""
from __future__ import annotations

import csv
import json
from pathlib import Path

HERE = Path(__file__).resolve().parent
SEED = HERE / "seed.json"
OUT = HERE / "csv"


def write(name: str, rows: list[dict], columns: list[str]) -> None:
    OUT.mkdir(parents=True, exist_ok=True)
    with (OUT / f"{name}.csv").open("w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=columns, extrasaction="ignore")
        w.writeheader()
        for r in rows:
            w.writerow({c: ("" if r.get(c) is None else r.get(c)) for c in columns})
    print(f"{name}.csv: {len(rows)} rows")


def main() -> None:
    d = json.loads(SEED.read_text(encoding="utf-8"))

    # --- node tables ------------------------------------------------------
    write("research_groups", d["research_groups"], ["abbreviation", "name", "description"])
    write("skills", d["skills"], ["name", "category"])
    write("employees", d["employees"],
          ["email", "first_name", "last_name", "job_title", "bio", "active"])
    write("companies", d["companies"],
          ["name", "type", "description", "website", "city", "country"])

    departments = [
        {"company": c["name"], "name": dep["name"], "description": dep.get("description"),
         "key_goals": "; ".join(dep.get("key_goals") or [])}
        for c in d["companies"] for dep in c.get("departments", [])
    ]
    write("company_departments", departments, ["company", "name", "description", "key_goals"])

    write("external_contacts", d["contacts"],
          ["email", "first_name", "last_name", "job_title", "notes"])
    write("projects", d["projects"],
          ["acronym", "title", "description", "funding_programme", "status",
           "application_date", "approval_date", "start_date", "end_date"])

    # --- edge tables ------------------------------------------------------
    write("member_of",
          [{"employee": e["email"], "research_group": g["group"], "percentage": g["percentage"]}
           for e in d["employees"] for g in e.get("groups", [])],
          ["employee", "research_group", "percentage"])
    write("has_skill",
          [{"employee": e["email"], "skill": s["skill"], "level": s["level"]}
           for e in d["employees"] for s in e.get("skills", [])],
          ["employee", "skill", "level"])
    write("works_on",
          [{"employee": t["employee"], "project": p["acronym"], "role": t["role"],
            "fte_percentage": t.get("fte_percentage")}
           for p in d["projects"] for t in p.get("team", [])],
          ["employee", "project", "role", "fte_percentage"])
    write("funds_group",
          [{"project": p["acronym"], "research_group": g["group"], "budget": g["budget"]}
           for p in d["projects"] for g in p.get("groups", [])],
          ["project", "research_group", "budget"])
    write("participates_in",
          [{"external_contact": c["contact"], "project": p["acronym"], "role": c["role"]}
           for p in d["projects"] for c in p.get("contacts", [])],
          ["external_contact", "project", "role"])
    write("works_at",
          [{"external_contact": c["email"], "company": c["company"], "department": c["department"]}
           for c in d["contacts"]],
          ["external_contact", "company", "department"])


if __name__ == "__main__":
    main()
