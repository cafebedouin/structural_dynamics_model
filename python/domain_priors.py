import os
import re
import glob
import sys
import argparse
from pathlib import Path


def _read_config_thresholds():
    """Read param/2 threshold values from prolog/config.pl.

    Uses the same pattern as structural_linter.py to ensure a single source
    of truth for all classification thresholds.
    """
    config_path = os.path.join(os.path.dirname(os.path.dirname(__file__)), 'prolog', 'config.pl')
    thresholds = {}
    try:
        with open(config_path, 'r', encoding='utf-8') as f:
            for line in f:
                match = re.search(r"param\((\w+),\s*([\d.]+)\)", line)
                if match:
                    param_name = match.group(1)
                    if param_name.endswith('_metric_name'):
                        continue
                    try:
                        thresholds[param_name] = float(match.group(2))
                    except ValueError:
                        pass
    except Exception as e:
        print(f"Warning: Could not read thresholds from {config_path}: {e}", file=sys.stderr)
    return thresholds


# Explicit mapping: Python constant name → config.pl param name → fallback default
_PARAM_MAP = {
    'ROPE_EXTRACTION_CEILING':        ('rope_extraction_ceiling',              0.15),
    'TANGLED_ROPE_EXTRACTION_FLOOR':  ('tangled_rope_extraction_floor',        0.16),
    'TANGLED_ROPE_EXTRACTION_CEIL':   ('tangled_rope_extraction_ceil',         0.90),
    'SNARE_EXTRACTION_FLOOR':         ('snare_epsilon_floor',                  0.46),
    'FALSE_MOUNTAIN_THRESHOLD':       ('false_mountain_extraction_threshold',  0.90),
}

_config = _read_config_thresholds()
ROPE_EXTRACTION_CEILING        = _config.get(_PARAM_MAP['ROPE_EXTRACTION_CEILING'][0],        _PARAM_MAP['ROPE_EXTRACTION_CEILING'][1])
TANGLED_ROPE_EXTRACTION_FLOOR  = _config.get(_PARAM_MAP['TANGLED_ROPE_EXTRACTION_FLOOR'][0],  _PARAM_MAP['TANGLED_ROPE_EXTRACTION_FLOOR'][1])
TANGLED_ROPE_EXTRACTION_CEIL   = _config.get(_PARAM_MAP['TANGLED_ROPE_EXTRACTION_CEIL'][0],   _PARAM_MAP['TANGLED_ROPE_EXTRACTION_CEIL'][1])
SNARE_EXTRACTION_FLOOR         = _config.get(_PARAM_MAP['SNARE_EXTRACTION_FLOOR'][0],         _PARAM_MAP['SNARE_EXTRACTION_FLOOR'][1])
FALSE_MOUNTAIN_THRESHOLD       = _config.get(_PARAM_MAP['FALSE_MOUNTAIN_THRESHOLD'][0],       _PARAM_MAP['FALSE_MOUNTAIN_THRESHOLD'][1])

def infer_type_from_score(score):
    """Infer constraint type from base extractiveness score.

    Aligned with config.pl thresholds:
      Rope:         ε ≤ 0.15
      Tangled Rope: 0.16 ≤ ε ≤ 0.90  (requires coordination markers in Prolog)
      Snare:        ε ≥ 0.46

    Note: The Python registry cannot detect coordination function markers
    (those are Prolog-side checks), so we use a simplified heuristic:
      - Below rope ceiling -> rope
      - In tangled rope zone but below snare floor -> tangled_rope
      - At or above snare floor -> snare
    """
    if score <= ROPE_EXTRACTION_CEILING:
        return "rope"
    elif score < SNARE_EXTRACTION_FLOOR:
        return "tangled_rope"
    else:
        return "snare"


def quote_prolog_atom(atom):
    """Quote a Prolog atom if it doesn't start with a lowercase letter or contains non-alphanumeric chars."""
    if re.match(r'^[a-z][a-z0-9_]*$', atom):
        return atom
    return f"'{atom}'"


def generate_domain_registry(testsets_dir, output_path):
    # Regex updated to handle optional module prefixes
    id_pattern = re.compile(r"(?:constraint_id|Checking Interval|EXECUTING):\s*(?:\s*)?([a-zA-Z0-9_]+)")

    # Matches: constraint_classification(id, type, ...) or constraint_claim(id, type)
    explicit_pattern = re.compile(
        r"(?:\w+:)?(?:constraint_classification|constraint_claim)\s*\(\s*(?:\s*)?([a-zA-Z0-9_]+),\s*([a-zA-Z0-9_]+)"
    )

    extract_pattern = re.compile(r"base_extractiveness\((?:\s*)?[^,]+,\s*([\d\.]+)\)")
    suppress_pattern = re.compile(r"suppression_score\((?:\s*)?[^,]+,\s*([\d\.]+)\)")

    # Valid constraint types (six categories from core.md)
    valid_types = {"mountain", "rope", "tangled_rope", "snare", "scaffold", "piton"}

    registry = {}
    pl_files = glob.glob(os.path.join(testsets_dir, "*.pl"))

    for file_path in pl_files:
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()
                ids = id_pattern.findall(content)
                if not ids:
                    continue

                # 1. Get score-based inference (aligned with config.pl)
                e_match = extract_pattern.search(content)
                score = float(e_match.group(1)) if e_match else 0.0
                score_type = infer_type_from_score(score)

                # 2. Get explicit claim if present
                tag_match = explicit_pattern.search(content)
                tag = tag_match.group(2).strip() if tag_match else None

                # 3. Audit Logic: Synchronized with drl_core.pl and config.pl
                if tag == "mountain":
                    # Flag naturalization fraud if extraction is extreme
                    if score > FALSE_MOUNTAIN_THRESHOLD:
                        # Reclassify: claimed mountain with extreme extraction = snare
                        category = "snare"
                    else:
                        category = "mountain"
                elif tag == "tangled_rope":
                    # If effective extraction hits snare threshold, reclassify
                    if score >= SNARE_EXTRACTION_FLOOR:
                        category = "snare"
                    else:
                        category = "tangled_rope"
                elif tag == "scaffold":
                    category = "scaffold"
                elif tag == "piton":
                    category = "piton"
                elif tag in valid_types:
                    category = tag
                else:
                    # Default to the inferred type from metrics
                    category = score_type

                for identifier in set(ids):
                    registry[identifier] = category

        except Exception as e:
            print(f"Error processing {file_path}: {e}")

    header = [
        ":- module(domain_registry, [domain_category/2]).",
        "% --- AUTOMATICALLY GENERATED DOMAIN REGISTRY ---",
        f"% Thresholds aligned with config.pl: Rope <= {ROPE_EXTRACTION_CEILING}, "
        f"Snare >= {SNARE_EXTRACTION_FLOOR}, Tangled Rope {TANGLED_ROPE_EXTRACTION_FLOOR}-{TANGLED_ROPE_EXTRACTION_CEIL}.",
        f"% Total entries: {len(registry)}",
        ""
    ]

    with open(output_path, 'w', encoding='utf-8') as f:
        f.write("\n".join(header))
        f.write("\n")
        for k, v in sorted(registry.items()):
            f.write(f"domain_category({quote_prolog_atom(k)}, {v}).\n")

    print(f"Registry generated at {output_path} with {len(registry)} entries.")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Generate domain_registry.pl from testset .pl files")
    parser.add_argument("--input", default="/home/scott/bin/structural_dynamics_model/prolog/testsets", help="Path to .pl files")
    parser.add_argument("--output", default="/home/scott/bin/structural_dynamics_model/prolog/domain_registry.pl", help="Path to output")
    args = parser.parse_args()

    generate_domain_registry(args.input, args.output)
