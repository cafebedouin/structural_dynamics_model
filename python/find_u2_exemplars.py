"""U2 Exemplar Finder — Scan corpus for well-instantiated moderate atoms.

Scans prolog/testsets/ for constraint stories with agent_power(moderate),
filters by linter pass, selects 5 diverse exemplars, and generates
seed preamble files for the U2 seeding experiment.

Usage:
    python3 python/find_u2_exemplars.py
    python3 python/find_u2_exemplars.py --dry-run   # scan only, no file writes
"""

import argparse
import json
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))
if str(REPO_ROOT / "python") not in sys.path:
    sys.path.insert(0, str(REPO_ROOT / "python"))

from python.linter import lint_file

TESTSETS_DIR = REPO_ROOT / "prolog" / "testsets"
PREAMBLE_DIR = REPO_ROOT / "prompts" / "perspective_preambles"
RESULTS_DIR = REPO_ROOT / "results" / "perspective_experiment"

# Target constraints for cross-domain exclusion
TARGET_CONSTRAINTS = [
    "academic_peer_review_gatekeeping",
    "subscription_economy_model",
    "26usc469_real_estate_exemption",
    "antifragility",
    "epistemic_process_of_verification",
]

# ---------------------------------------------------------------------------
# Prolog extraction regexes
# ---------------------------------------------------------------------------

_RE_CLASSIFICATION = re.compile(
    r"constraint_indexing:constraint_classification\(\s*"
    r"['\"]?(\w+)['\"]?\s*,\s*(\w+)\s*,\s*"
    r"context\(\s*agent_power\(moderate\)\s*,\s*"
    r"time_horizon\((\w+)\)\s*,\s*"
    r"exit_options\((\w+)\)\s*,\s*"
    r"spatial_scope\((\w+)\)\s*\)\s*\)",
    re.DOTALL,
)

_RE_EXTRACTIVENESS = re.compile(
    r"domain_priors:base_extractiveness\(\s*['\"]?(\w+)['\"]?\s*,\s*([\d.]+)\s*\)"
)

_RE_TOPIC_DOMAIN = re.compile(
    r"narrative_ontology:topic_domain\(\s*['\"]?(\w+)['\"]?\s*,\s*\"([^\"]+)\"\s*\)"
)

_RE_HUMAN_READABLE = re.compile(
    r"narrative_ontology:human_readable\(\s*['\"]?(\w+)['\"]?\s*,\s*\"([^\"]+)\"\s*\)"
)

_RE_CLAIM = re.compile(
    r"narrative_ontology:constraint_claim\(\s*['\"]?(\w+)['\"]?\s*,\s*(\w+)\s*\)"
)

# Fallback: extract domain from comment block
_RE_COMMENT_DOMAIN = re.compile(
    r"\*\s*domain:\s*(.+)", re.IGNORECASE
)

# Perspective label from comment above classification fact
_RE_PERSPECTIVE_LABEL = re.compile(
    r"%\s*PERSPECTIVE\s+\d+:\s*(.+)"
)


# ---------------------------------------------------------------------------
# Extraction helpers
# ---------------------------------------------------------------------------

def extract_moderate_info(pl_content: str, filepath: Path) -> dict | None:
    """Extract moderate atom info from a Prolog testset file.

    Returns dict with all metadata or None if no moderate atom found.
    """
    match = _RE_CLASSIFICATION.search(pl_content)
    if not match:
        return None

    constraint_id = match.group(1)
    classification_type = match.group(2)
    time_horizon = match.group(3)
    exit_options = match.group(4)
    spatial_scope = match.group(5)

    # Extract base_extractiveness
    eps_match = _RE_EXTRACTIVENESS.search(pl_content)
    epsilon = float(eps_match.group(2)) if eps_match else None

    # Extract topic_domain (Prolog fact)
    domain_match = _RE_TOPIC_DOMAIN.search(pl_content)
    if domain_match:
        domain = domain_match.group(2)
    else:
        # Fallback: comment block
        comment_match = _RE_COMMENT_DOMAIN.search(pl_content)
        domain = comment_match.group(1).strip() if comment_match else "unknown"

    # Extract human_readable
    hr_match = _RE_HUMAN_READABLE.search(pl_content)
    human_readable = hr_match.group(2) if hr_match else constraint_id.replace("_", " ").title()

    # Extract claimed_type
    claim_match = _RE_CLAIM.search(pl_content)
    claimed_type = claim_match.group(2) if claim_match else "unknown"

    # Extract perspective label from comment above the classification fact
    label = _extract_perspective_label(pl_content, match.start())

    # Build raw Prolog excerpt (the full classification fact)
    excerpt_start = _find_fact_start(pl_content, match.start())
    excerpt_end = match.end() + 1  # include closing period
    prolog_excerpt = pl_content[excerpt_start:excerpt_end].strip()

    # Synthesize comment from domain + classification
    comment = f"Moderate actor navigating {human_readable} as a {classification_type}"

    return {
        "constraint_id": constraint_id,
        "human_readable": human_readable,
        "domain": domain,
        "domain_family": domain.split("/")[0].strip().lower(),
        "claimed_type": claimed_type,
        "epsilon": epsilon,
        "moderate_atom": {
            "classification_type": classification_type,
            "agent_power": "moderate",
            "time_horizon": time_horizon,
            "exit_options": exit_options,
            "spatial_scope": spatial_scope,
            "label": label,
            "comment": comment,
        },
        "prolog_excerpt": prolog_excerpt,
        "filepath": str(filepath),
    }


def _extract_perspective_label(content: str, match_pos: int) -> str:
    """Find the % PERSPECTIVE N: LABEL comment above a classification fact."""
    # Look at lines before the match position
    preceding = content[:match_pos]
    lines = preceding.split("\n")

    # Search backwards through the last 10 lines for a PERSPECTIVE label
    for line in reversed(lines[-10:]):
        m = _RE_PERSPECTIVE_LABEL.match(line.strip())
        if m:
            label = m.group(1).strip()
            # Clean up: remove trailing classification type in parens
            label = re.sub(r"\s*\([A-Z ]+\)\s*$", "", label)
            return label

    return "Moderate Observer"


def _find_fact_start(content: str, match_pos: int) -> int:
    """Find the start of the constraint_classification fact before match_pos."""
    # Search backwards for 'constraint_indexing:constraint_classification'
    prefix = "constraint_indexing:constraint_classification"
    idx = content.rfind(prefix, 0, match_pos + len(prefix) + 50)
    return idx if idx >= 0 else match_pos


def build_json_excerpt(info: dict) -> str:
    """Build a JSON perspective excerpt from extracted moderate atom info."""
    perspective = {
        "classification_type": info["moderate_atom"]["classification_type"],
        "agent_power": "moderate",
        "time_horizon": info["moderate_atom"]["time_horizon"],
        "exit_options": info["moderate_atom"]["exit_options"],
        "spatial_scope": info["moderate_atom"]["spatial_scope"],
        "label": info["moderate_atom"]["label"],
        "comment": info["moderate_atom"]["comment"],
    }
    return json.dumps(perspective, indent=2)


# ---------------------------------------------------------------------------
# Exemplar selection
# ---------------------------------------------------------------------------

def scan_corpus() -> list[dict]:
    """Scan all testset .pl files for moderate atoms, lint-filter, return candidates."""
    candidates = []
    pl_files = sorted(TESTSETS_DIR.glob("*.pl"))

    # Exclude known non-story files
    exclude = {"validation_suite.pl"}

    total = 0
    lint_pass = 0
    moderate_found = 0

    for filepath in pl_files:
        if filepath.name in exclude:
            continue
        total += 1

        content = filepath.read_text(encoding="utf-8", errors="replace")
        info = extract_moderate_info(content, filepath)
        if not info:
            continue
        moderate_found += 1

        # Skip target constraints (don't use them as exemplars)
        if info["constraint_id"] in TARGET_CONSTRAINTS:
            continue

        # Lint check
        errors = lint_file(str(filepath))
        if errors:
            continue
        lint_pass += 1

        # Build JSON excerpt
        info["json_excerpt"] = build_json_excerpt(info)
        candidates.append(info)

    print(f"\nCorpus scan complete:")
    print(f"  Total .pl files:         {total}")
    print(f"  With moderate atom:      {moderate_found}")
    print(f"  Lint-clean (non-target): {lint_pass}")
    print(f"  Candidates:              {len(candidates)}")

    return candidates


def select_exemplars(candidates: list[dict], n: int = 5) -> list[dict]:
    """Select n diverse exemplars from candidates, maximizing domain diversity."""
    if len(candidates) <= n:
        for i, c in enumerate(candidates):
            c["role"] = "seed" if i < 3 else "holdout"
        return candidates

    # Group by domain family
    by_family: dict[str, list[dict]] = {}
    for c in candidates:
        family = c["domain_family"]
        by_family.setdefault(family, []).append(c)

    selected = []
    used_families = set()

    # First pass: pick one from each unique family
    for family in sorted(by_family, key=lambda f: len(by_family[f])):
        if len(selected) >= n:
            break
        # Prefer candidates with higher epsilon (more interesting constraints)
        pool = sorted(by_family[family], key=lambda c: c["epsilon"] or 0, reverse=True)
        selected.append(pool[0])
        used_families.add(family)

    # Second pass: if we still need more, pick from remaining families
    if len(selected) < n:
        remaining = [c for c in candidates if c not in selected]
        # Sort by epsilon descending for interesting examples
        remaining.sort(key=lambda c: c["epsilon"] or 0, reverse=True)

        for c in remaining:
            if len(selected) >= n:
                break
            # Prefer unused families
            if c["domain_family"] not in used_families:
                selected.append(c)
                used_families.add(c["domain_family"])

    # Fill any remaining slots
    if len(selected) < n:
        remaining = [c for c in candidates if c not in selected]
        remaining.sort(key=lambda c: c["epsilon"] or 0, reverse=True)
        for c in remaining:
            if len(selected) >= n:
                break
            selected.append(c)

    # Assign roles: first 3 are seeds, rest are holdouts
    for i, s in enumerate(selected):
        s["role"] = "seed" if i < 3 else "holdout"

    return selected


# ---------------------------------------------------------------------------
# Preamble generation
# ---------------------------------------------------------------------------

def load_base_preamble() -> str:
    """Load the u2_experiential.md base preamble."""
    path = PREAMBLE_DIR / "u2_experiential.md"
    return path.read_text(encoding="utf-8").strip()


def build_seeded_preamble(base_preamble: str, exemplars: list[dict]) -> str:
    """Build the seeded preamble with exemplar excerpts."""
    parts = [base_preamble, "", ""]
    parts.append(
        "Here are examples of how the moderate observer position has been "
        "successfully instantiated in other constraint analyses:"
    )
    parts.append("")

    for i, ex in enumerate(exemplars, 1):
        parts.append(f"**Example {i}: {ex['human_readable']} ({ex['domain']})**")
        parts.append(f"Constraint type: {ex['claimed_type']}, "
                     f"Base extractiveness: {ex['epsilon']}")
        parts.append("")
        parts.append("```json")
        parts.append(ex["json_excerpt"])
        parts.append("```")
        parts.append("")

    return "\n".join(parts)


def get_target_domains() -> dict[str, str]:
    """Get domain families for each target constraint."""
    target_domains = {}
    for tc in TARGET_CONSTRAINTS:
        filepath = TESTSETS_DIR / f"{tc}.pl"
        if not filepath.exists():
            target_domains[tc] = "unknown"
            continue
        content = filepath.read_text(encoding="utf-8", errors="replace")

        domain_match = _RE_TOPIC_DOMAIN.search(content)
        if domain_match:
            domain = domain_match.group(2)
        else:
            comment_match = _RE_COMMENT_DOMAIN.search(content)
            domain = comment_match.group(1).strip() if comment_match else "unknown"

        target_domains[tc] = domain.split("/")[0].strip().lower()

    return target_domains


def generate_cross_domain_preambles(
    base_preamble: str,
    all_seeds: list[dict],
    holdouts: list[dict],
    target_domains: dict[str, str],
) -> dict[str, str]:
    """Generate per-target-constraint cross-domain preambles.

    For each target constraint, select 3 exemplars whose domain_family
    differs from the target's domain_family.
    """
    # Pool of all available exemplars (seeds + holdouts)
    pool = all_seeds + holdouts
    preambles = {}

    for tc in TARGET_CONSTRAINTS:
        tc_family = target_domains.get(tc, "unknown")

        # Filter to exemplars from different domain families
        cross_domain = [ex for ex in pool if ex["domain_family"] != tc_family]

        if len(cross_domain) < 3:
            # Fallback: use all available, even if some share domain
            print(f"  WARNING: Only {len(cross_domain)} cross-domain exemplars "
                  f"for {tc} (family: {tc_family}). Using all available.")
            cross_domain = [ex for ex in pool]
            # At least exclude exact domain match
            cross_domain = [ex for ex in cross_domain
                           if ex["domain_family"] != tc_family] or pool[:3]

        # Take first 3
        selected = cross_domain[:3]
        preambles[tc] = build_seeded_preamble(base_preamble, selected)

    return preambles


# ---------------------------------------------------------------------------
# Output
# ---------------------------------------------------------------------------

def save_exemplars(exemplars: list[dict], output_path: Path) -> None:
    """Save exemplar data to JSON."""
    # Strip filepath (internal) before saving
    output = []
    for ex in exemplars:
        entry = {k: v for k, v in ex.items() if k != "filepath"}
        output.append(entry)

    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(
        json.dumps(output, indent=2, ensure_ascii=False) + "\n",
        encoding="utf-8",
    )
    print(f"\nExemplars saved to: {output_path}")


def save_preamble(content: str, path: Path) -> None:
    """Write a preamble file."""
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content + "\n", encoding="utf-8")


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description="Scan corpus for U2 (moderate) exemplars and generate seed preambles"
    )
    parser.add_argument(
        "--dry-run", action="store_true",
        help="Scan only, print candidates, do not write files"
    )
    parser.add_argument(
        "--count", "-n", type=int, default=5,
        help="Number of exemplars to select (default: 5)"
    )
    args = parser.parse_args()

    print("=" * 60)
    print("U2 EXEMPLAR FINDER")
    print("=" * 60)

    # Phase 1: Scan corpus
    print("\nScanning prolog/testsets/ for moderate atoms...")
    candidates = scan_corpus()

    if not candidates:
        print("\nERROR: No lint-clean candidates found!")
        sys.exit(1)

    # Phase 2: Select diverse exemplars
    print(f"\nSelecting {args.count} diverse exemplars...")
    exemplars = select_exemplars(candidates, n=args.count)

    # Print selected exemplars for review
    seeds = [ex for ex in exemplars if ex["role"] == "seed"]
    holdouts = [ex for ex in exemplars if ex["role"] == "holdout"]

    print(f"\n{'=' * 60}")
    print(f"SELECTED EXEMPLARS ({len(seeds)} seeds, {len(holdouts)} holdouts)")
    print(f"{'=' * 60}")

    for ex in exemplars:
        print(f"\n  [{ex['role'].upper()}] {ex['constraint_id']}")
        print(f"    Human readable: {ex['human_readable']}")
        print(f"    Domain:         {ex['domain']} (family: {ex['domain_family']})")
        print(f"    Claimed type:   {ex['claimed_type']}")
        print(f"    Epsilon:        {ex['epsilon']}")
        print(f"    Moderate as:    {ex['moderate_atom']['classification_type']}")
        print(f"    Context:        P=moderate, T={ex['moderate_atom']['time_horizon']}, "
              f"E={ex['moderate_atom']['exit_options']}, S={ex['moderate_atom']['spatial_scope']}")
        print(f"    Label:          {ex['moderate_atom']['label']}")

    if args.dry_run:
        print("\n[DRY RUN] No files written.")
        return

    # Phase 3: Save exemplars JSON
    output_path = RESULTS_DIR / "u2_exemplars.json"
    save_exemplars(exemplars, output_path)

    # Phase 4: Generate preamble files
    print("\nGenerating preamble files...")
    base_preamble = load_base_preamble()

    # Condition B: Standard seeded preamble (3 seed exemplars)
    seeded_preamble = build_seeded_preamble(base_preamble, seeds)
    seeded_path = PREAMBLE_DIR / "u2_seeded.md"
    save_preamble(seeded_preamble, seeded_path)
    print(f"  Condition B preamble: {seeded_path}")

    # Condition C: Per-target cross-domain preambles
    target_domains = get_target_domains()
    print(f"\n  Target constraint domains:")
    for tc, family in target_domains.items():
        print(f"    {tc}: {family}")

    cross_preambles = generate_cross_domain_preambles(
        base_preamble, seeds, holdouts, target_domains
    )
    for tc, content in cross_preambles.items():
        path = PREAMBLE_DIR / f"u2_seedx_{tc}.md"
        save_preamble(content, path)
        print(f"  Condition C preamble: {path}")

    print(f"\n{'=' * 60}")
    print("DONE — Review exemplars and preambles before running experiment")
    print(f"{'=' * 60}")


if __name__ == "__main__":
    main()
