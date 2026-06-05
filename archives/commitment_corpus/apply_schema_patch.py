"""Apply the additive kernel_context schema patch.

Adds ONE optional free-text string property `kernel_context` to commentary,
parallel to directionality_logic. Purely additive: commentary has no `required`
list, so absence remains valid and every existing constraint file still validates.

The patch is idempotent (safe to run twice) and refuses to run if the field
already exists with a different spec.

Usage:
    python3 apply_schema_patch.py path/to/constraint_story_schema.json [--dry-run]

Run against the canonical schema (relocated 2026-06-05; the former
agent/data/ copy was a stale orphan and has been deleted):
    schemas/constraint_story_schema.json
"""
import argparse
import json
import sys
from pathlib import Path

KERNEL_CONTEXT_SPEC = {
    "type": "string",
    "description": (
        "Optional. Free-text note naming the contested kernel this constraint is a "
        "reading of, which reading it instantiates, and the sibling readings. Parallel "
        "to directionality_logic. Absent for ordinary (non-kernel) constraints."
    ),
}


def patch_schema(path: Path, dry_run: bool = False) -> bool:
    schema = json.loads(path.read_text(encoding="utf-8"))
    comm = schema.get("properties", {}).get("commentary")
    if comm is None:
        print(f"  ERROR {path.name}: no commentary object found; aborting")
        return False

    props = comm.setdefault("properties", {})
    existing = props.get("kernel_context")
    if existing is not None:
        if existing.get("type") == "string":
            print(f"  OK {path.name}: kernel_context already present (string); no change")
            return True
        print(f"  ERROR {path.name}: kernel_context exists with non-string spec; refusing")
        return False

    # Guard: commentary must not have a `required` list that we'd violate.
    if "required" in comm and "kernel_context" in comm.get("required", []):
        print(f"  ERROR {path.name}: kernel_context unexpectedly in required; refusing")
        return False

    props["kernel_context"] = KERNEL_CONTEXT_SPEC
    # additionalProperties stays False; we added the property explicitly so it validates.

    if dry_run:
        print(f"  DRY-RUN {path.name}: would add commentary.kernel_context (optional string)")
        return True

    path.write_text(json.dumps(schema, indent=2, ensure_ascii=False) + "\n", encoding="utf-8")
    print(f"  PATCHED {path.name}: added commentary.kernel_context (optional string)")
    return True


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("schema_path")
    ap.add_argument("--dry-run", action="store_true")
    args = ap.parse_args()
    p = Path(args.schema_path)
    if not p.exists():
        print(f"ERROR: {p} not found")
        sys.exit(1)
    ok = patch_schema(p, args.dry_run)
    sys.exit(0 if ok else 1)


if __name__ == "__main__":
    main()
