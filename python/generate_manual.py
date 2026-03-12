#!/usr/bin/env python3
"""Generate Manual entries from JSON constraint story files.

Transforms structured constraint analyses into entries for The Manual —
a practical guide to recognizing institutional extraction. Each entry has
three parts: The Tell, Action under indeterminacy, Revision condition.

Usage:
    python generate_manual.py                          # full generation
    python generate_manual.py --dry-run                # stats + 3 samples
    python generate_manual.py --json-dir path/to/json  # custom input dir

Requires: Python 3.8+ (stdlib only).
"""

import argparse
import json
import os
import re
import sys
from pathlib import Path


# ---------------------------------------------------------------------------
# Inclusion filter
# ---------------------------------------------------------------------------

def should_include(data):
    """Include if extractiveness > 0.25 and at least one victim declared."""
    bp = data.get("base_properties", {})
    eps = bp.get("extractiveness", 0)
    victims = bp.get("victims", [])
    return eps > 0.25 and len(victims) > 0


# ---------------------------------------------------------------------------
# The Tell
# ---------------------------------------------------------------------------

EXIT_RANK = {
    "trapped": 0,
    "identity_locked": 1,
    "constrained": 2,
    "mobile": 3,
    "arbitrage": 4,
    "analytical": 5,
}

TYPE_RANK = {"snare": 0, "tangled_rope": 1, "piton": 2, "rope": 3, "scaffold": 4, "mountain": 5}


def _pick_perspective(data):
    """Select the best perspective for The Tell.

    Prefers trapped/snare. Falls back through exit_rank and type_rank.
    """
    perspectives = data.get("perspectives", [])
    if not perspectives:
        return None

    def sort_key(p):
        exit_r = EXIT_RANK.get(p.get("exit_options", ""), 99)
        type_r = TYPE_RANK.get(p.get("classification_type", ""), 99)
        return (exit_r, type_r)

    return min(perspectives, key=sort_key)


def _extract_first_sentence(comment):
    """Extract the first concrete sentence from a perspective comment.

    Strips the PERSPECTIVE N: LABEL (TYPE) — prefix, then takes the first
    sentence. Handles missing dash, fragments, and overlong sentences.
    """
    if not comment:
        return None

    # Strip prefix: everything before ' — ' or ' -- '
    for sep in [" \u2014 ", " — ", " -- ", "— ", "-- "]:
        if sep in comment:
            _, _, comment = comment.partition(sep)
            break

    # If prefix stripping didn't fire, try stripping PERSPECTIVE N: ... pattern
    comment = re.sub(r"^PERSPECTIVE\s+\d+:\s*[^—\-]+[—\-]+\s*", "", comment)

    comment = comment.strip()
    if not comment:
        return None

    # Strip leading "Trapped ..." preamble — prefer the concrete detail after it
    # Pattern: "Trapped in/by X, Y." or "Trapped within X with Y."
    # If there's a second sentence, it's usually more concrete.
    sentences_raw = re.split(r"(?<=[.!?])\s+(?=[A-Z])", comment)

    # If first sentence starts with "Trapped" and there's a better sentence after,
    # skip to the second sentence (which typically has the concrete observable)
    if (len(sentences_raw) > 1
            and sentences_raw[0].strip().startswith("Trapped")
            and len(sentences_raw[0].split()) < 20):
        sentences = sentences_raw[1:]
    else:
        sentences = sentences_raw

    if not sentences:
        return None

    sent = sentences[0].strip()
    # Ensure it ends with a period
    if sent and sent[-1] not in ".!?":
        sent += "."

    # If >30 words, truncate at clause boundary after word 15
    words = sent.split()
    if len(words) > 30:
        # Find a clause boundary (;, — or , followed by space) after word 15
        partial = " ".join(words[:30])
        for delim in ["; ", ", ", " — ", " -- "]:
            idx = partial.find(delim, len(" ".join(words[:15])))
            if idx != -1:
                sent = partial[:idx].rstrip(",;") + "."
                break
        else:
            sent = " ".join(words[:25]) + "."

    return sent


def _has_theater_drift(data):
    """Check if theater_ratio drifts upward by >0.15 over measurement interval."""
    measurements = data.get("measurements", [])
    theater = [(m["time_point"], m["value"]) for m in measurements
               if m.get("metric") == "theater_ratio"]
    if len(theater) < 2:
        return False
    theater.sort()
    return (theater[-1][1] - theater[0][1]) > 0.15


def extract_tell(data):
    """Extract The Tell — one observable sentence about extraction."""
    bp = data.get("base_properties", {})
    perspective = _pick_perspective(data)

    if perspective:
        sent = _extract_first_sentence(perspective.get("comment", ""))
        if sent:
            if _has_theater_drift(data):
                sent = sent.rstrip(".") + " (and increasingly theatrical)."
            return sent

    # Fallback: template from human_readable + first victim
    human = bp.get("human_readable", "this constraint")
    victims = bp.get("victims", [])
    victim = humanize_victim(victims[0]) if victims else "those subject to it"
    return f"When {human} operates, {victim} bears the cost without alternative."


# ---------------------------------------------------------------------------
# Action under indeterminacy
# ---------------------------------------------------------------------------

def _posture_clause(data):
    """Select posture clause based on type and extractiveness."""
    bp = data.get("base_properties", {})
    ctype = bp.get("claimed_type", "")
    eps = bp.get("extractiveness", 0)

    if ctype == "piton":
        return "The theater is the mechanism."
    if ctype == "snare" or eps >= 0.65:
        return "Assume extraction until shown otherwise."
    if eps >= 0.55:
        return "Lean toward extraction."
    if eps >= 0.35:
        return "Hold both possibilities."
    return "Assume coordination but verify."


def _compress_text(text, max_words):
    """Compress text to max_words by taking sentences greedily."""
    if not text:
        return ""

    # Strip leading all-caps label
    text = re.sub(r"^[A-Z][A-Z_ ]{3,}:\s*", "", text)

    sentences = re.split(r"(?<=[.!?])\s+(?=[A-Z])", text)
    result_words = []
    for sent in sentences:
        words = sent.split()
        if len(result_words) + len(words) <= max_words:
            result_words.extend(words)
        else:
            if not result_words:
                # First sentence alone exceeds limit — truncate it
                result_words = words[:max_words]
                last = result_words[-1].rstrip(".,;:!?")
                result_words[-1] = last + "..."
            break

    result = " ".join(result_words)
    # Ensure ends with punctuation
    if result and result[-1] not in ".!?":
        if not result.endswith("..."):
            result += "."
    return result


def extract_action(data):
    """Extract Action under indeterminacy — ≤60 words, posture + compressed mandatrophy."""
    commentary = data.get("commentary", {})

    # Try mandatrophy_analysis, then logic_rationale, then narrative_context
    source_text = (commentary.get("mandatrophy_analysis")
                   or commentary.get("logic_rationale")
                   or commentary.get("narrative_context")
                   or "")

    posture = _posture_clause(data)
    posture_words = len(posture.split())
    remaining = 60 - posture_words

    if source_text and remaining > 0:
        body = _compress_text(source_text, remaining)
        if body:
            return f"{posture} {body}"

    if posture:
        return posture

    return "Observe who benefits from ambiguity. That is your answer."


# ---------------------------------------------------------------------------
# Revision condition
# ---------------------------------------------------------------------------

COORDINATION_KEYWORDS = [
    "coordination", "rope", "genuine", "serves", "justified",
    "protects", "legitimate", "benefit", "lower", "confirmed",
    "viable", "sustainable", "effective", "succeeds", "transition",
]


def _pick_omega(data):
    """Select the best omega — highest confidence, prefer empirical."""
    omegas = data.get("omegas", [])
    if not omegas:
        return None

    conf_rank = {"high": 0, "medium": 1, "low": 2}
    type_rank = {"empirical": 0, "conceptual": 1, "preference": 2}

    def sort_key(o):
        return (
            conf_rank.get(o.get("confidence", ""), 99),
            type_rank.get(o.get("type_class", ""), 99),
        )

    return min(omegas, key=sort_key)


def _extract_coordination_clause(impact):
    """Find the If-clause in impact that points toward coordination.

    Returns the full clause (condition + consequence) when the condition
    alone is too terse (≤3 words), since short conditions like "If viable:"
    or "If necessary:" are meaningless without their consequence.
    """
    if not impact:
        return None

    # Split into If-clauses
    clauses = re.split(r"(?=If )", impact)
    for clause in clauses:
        clause = clause.strip()
        if not clause.startswith("If"):
            continue
        lower = clause.lower()
        if any(kw in lower for kw in COORDINATION_KEYWORDS):
            # Extract condition part (before colon or consequence marker)
            m = re.match(
                r"If\s+(.+?)(?::\s*|,\s+(?:the|constraint|classification|this|current|extraction|it))",
                clause,
            )
            if m:
                condition = m.group(1).strip().rstrip(".,;:")
                if len(condition.split()) > 3:
                    return condition
                # Condition too terse — use the full clause up to first period
                end = clause.find(".")
                if end > 0:
                    full = clause[3:end].strip().rstrip(".,;:")
                    # Clean up mid-string colons (artifact of "If X: consequence")
                    full = re.sub(r":\s+", " — ", full, count=1)
                    if len(full.split()) > 3:
                        return full

            # Fallback: take everything up to first period
            end = clause.find(".")
            if end > 0:
                full = clause[3:end].strip().rstrip(".,;:")
                full = re.sub(r":\s+", " — ", full, count=1)

    return None


def _question_to_revision(question):
    """Convert an omega question into a revision condition sentence.

    Extracts the core testable claim from the question, truncates to
    keep the revision condition to one readable sentence.
    """
    if not question:
        return None
    # Take text up to the first question mark
    core = question.split("?")[0].strip()
    if not core:
        return None
    # Truncate if too long (>25 words)
    words = core.split()
    if len(words) > 25:
        core = " ".join(words[:25])
    # Lowercase the first word for flow
    if core[0].isupper():
        core = core[0].lower() + core[1:]
    return f"Reclassify when you can answer: {core}?"


def extract_revision(data):
    """Extract Revision condition — one sentence from best omega."""
    omega = _pick_omega(data)
    if omega:
        condition = _extract_coordination_clause(omega.get("impact", ""))
        if condition:
            # Check if it's substantive enough
            if len(condition.split()) > 3:
                if condition[0].isupper():
                    condition = condition[0].lower() + condition[1:]
                return f"This reclassifies if {condition}."

        # Fall back to the omega question
        rev = _question_to_revision(omega.get("question", ""))
        if rev:
            return rev

    return "No observable condition currently identified that would reclassify this constraint."


# ---------------------------------------------------------------------------
# Formatting
# ---------------------------------------------------------------------------

def humanize_victim(snake_name):
    """Convert snake_case victim name to readable form."""
    return snake_name.replace("_", " ")


def format_entry(data, tell, action, revision):
    """Format a single Manual entry as markdown."""
    bp = data.get("base_properties", {})
    header = data.get("header", {})

    title = bp.get("human_readable", header.get("constraint_id", "Unknown"))
    victims = bp.get("victims", [])
    victim_str = ", ".join(humanize_victim(v) for v in victims)
    cid = header.get("constraint_id", "unknown")
    eps = bp.get("extractiveness", 0)
    theta = bp.get("theater_ratio", 0)
    ctype = bp.get("claimed_type", "unknown")

    lines = [
        f"# {title}",
        "",
        f"*For: {victim_str}*",
        "",
        f"**The Tell.** {tell}",
        "",
        f"**Action under indeterminacy.** {action}",
        "",
        f"**Revision condition.** {revision}",
        "",
        "---",
        f"*{cid} | \u03b5={eps:.2f} | \u03b8={theta:.2f} | {ctype}*",
    ]
    return "\n".join(lines)


# ---------------------------------------------------------------------------
# I/O
# ---------------------------------------------------------------------------

def load_json_files(json_dir):
    """Load all JSON files from directory, returning list of parsed dicts."""
    entries = []
    json_path = Path(json_dir)
    for fp in sorted(json_path.glob("*.json")):
        try:
            with open(fp, "r", encoding="utf-8") as f:
                data = json.load(f)
            entries.append(data)
        except (json.JSONDecodeError, OSError) as e:
            print(f"WARNING: skipping {fp.name}: {e}", file=sys.stderr)
    return entries


def _topic_group(data):
    """Extract top-level topic domain for grouping."""
    td = data.get("base_properties", {}).get("topic_domain", "uncategorized")
    return td.split("/")[0]


def write_entries(entries_with_text, output_dir):
    """Write individual markdown files for each entry."""
    out = Path(output_dir)
    out.mkdir(parents=True, exist_ok=True)
    for data, text in entries_with_text:
        cid = data.get("header", {}).get("constraint_id", "unknown")
        fp = out / f"{cid}.md"
        with open(fp, "w", encoding="utf-8") as f:
            f.write(text + "\n")


def write_manual_draft(entries_with_text, output_path):
    """Write aggregated manual with TOC grouped by topic domain."""
    # Group by topic
    groups = {}
    for data, text in entries_with_text:
        group = _topic_group(data)
        eps = data.get("base_properties", {}).get("extractiveness", 0)
        title = data.get("base_properties", {}).get("human_readable", "")
        cid = data.get("header", {}).get("constraint_id", "")
        groups.setdefault(group, []).append((eps, title, cid, text))

    # Sort within groups by extractiveness descending
    for g in groups:
        groups[g].sort(key=lambda x: -x[0])

    out = Path(output_path)
    out.parent.mkdir(parents=True, exist_ok=True)

    with open(out, "w", encoding="utf-8") as f:
        f.write("# The Manual\n\n")
        f.write("*A practical guide to recognizing institutional extraction.*\n\n")
        f.write("---\n\n")

        # TOC
        f.write("## Contents\n\n")
        for group in sorted(groups.keys()):
            display = group.replace("_", " ").title()
            anchor = group.lower().replace(" ", "-").replace("/", "-")
            f.write(f"- [{display}](#{anchor}) ({len(groups[group])} entries)\n")
        f.write("\n---\n\n")

        # Entries by group
        for group in sorted(groups.keys()):
            display = group.replace("_", " ").title()
            f.write(f"## {display}\n\n")
            for _, _, _, text in groups[group]:
                f.write(text + "\n\n")


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    script_dir = Path(__file__).resolve().parent
    project_dir = script_dir.parent

    parser = argparse.ArgumentParser(description="Generate Manual entries from constraint JSON files.")
    parser.add_argument("--json-dir", default=str(project_dir / "json"),
                        help="Directory containing JSON constraint files")
    parser.add_argument("--output-dir", default=str(project_dir / "agent" / "manual" / "entries"),
                        help="Directory for individual entry markdown files")
    parser.add_argument("--manual-path", default=str(project_dir / "agent" / "manual" / "manual_draft.md"),
                        help="Path for aggregated manual draft")
    parser.add_argument("--dry-run", action="store_true",
                        help="Print stats and first 3 entries without writing files")
    args = parser.parse_args()

    # Load
    all_data = load_json_files(args.json_dir)
    print(f"Loaded {len(all_data)} JSON files", file=sys.stderr)

    # Filter
    included = [d for d in all_data if should_include(d)]
    excluded = len(all_data) - len(included)
    print(f"Included: {len(included)}, Excluded: {excluded}", file=sys.stderr)

    # Process
    entries_with_text = []
    stats = {"tell_fallback": 0, "action_fallback": 0, "revision_fallback": 0,
             "theater_drift": 0, "over_60": 0}

    for data in included:
        tell = extract_tell(data)
        action = extract_action(data)
        revision = extract_revision(data)

        # Track fallback usage
        bp = data.get("base_properties", {})
        persp = _pick_perspective(data)
        if not persp or not persp.get("comment"):
            stats["tell_fallback"] += 1
        commentary = data.get("commentary", {})
        if not commentary.get("mandatrophy_analysis"):
            stats["action_fallback"] += 1
        if not data.get("omegas"):
            stats["revision_fallback"] += 1
        if _has_theater_drift(data):
            stats["theater_drift"] += 1

        # Check word count
        action_words = len(action.split())
        if action_words > 60:
            stats["over_60"] += 1

        text = format_entry(data, tell, action, revision)
        entries_with_text.append((data, text))

    # Stats
    print(f"\nStats:", file=sys.stderr)
    print(f"  Tell fallbacks:     {stats['tell_fallback']}", file=sys.stderr)
    print(f"  Action fallbacks:   {stats['action_fallback']}", file=sys.stderr)
    print(f"  Revision fallbacks: {stats['revision_fallback']}", file=sys.stderr)
    print(f"  Theater drift:      {stats['theater_drift']}", file=sys.stderr)
    print(f"  Action >60 words:   {stats['over_60']}", file=sys.stderr)

    if args.dry_run:
        print(f"\n--- DRY RUN: first 3 entries ---\n", file=sys.stderr)
        for _, text in entries_with_text[:3]:
            print(text)
            print()
        return

    # Write
    write_entries(entries_with_text, args.output_dir)
    print(f"\nWrote {len(entries_with_text)} entries to {args.output_dir}", file=sys.stderr)

    write_manual_draft(entries_with_text, args.manual_path)
    print(f"Wrote manual draft to {args.manual_path}", file=sys.stderr)


if __name__ == "__main__":
    main()
