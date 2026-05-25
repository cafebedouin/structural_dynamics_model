"""B1 — Omega mining pass on run_01 kernel corpus.

Reads all JSON files in json/run_01/. For each constraint, extracts all omegas
and commentary.kernel_context. Reports:
  1. Omega ID clusters (recurring IDs across files)
  2. type_class distribution
  3. Question structure classification
  4. kernel_context template analysis

Output: outputs/kernel_manifests/run_01/harvest_b1_omega.json + .md
"""
import json
import re
import sys
from collections import Counter, defaultdict
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
DATA_DIR = REPO_ROOT / "json" / "run_01"
OUT_DIR = REPO_ROOT / "outputs" / "kernel_manifests" / "run_01"

TEMPLATE_ELEMENTS = [
    ("named_kernel", r"kernel"),
    ("sibling_readings", r"reading|readings|sibling"),
    ("authority_grounding", r"authority"),
    ("extraction_mechanism", r"extract"),
    ("perspectival_gap", r"perspect|gap|view|observer"),
]


def classify_question(question: str) -> str:
    q = question.lower()
    if any(w in q for w in ["which reading", "what reading", "reading applies", "applies here"]):
        return "reading_differentiation"
    if any(w in q for w in ["who adjudicates", "who decides", "who determines", "who rules"]):
        return "authority_ambiguity"
    if any(w in q for w in ["who benefits", "who captures", "who gains", "beneficiar"]):
        return "beneficiary_ambiguity"
    if any(w in q for w in ["over time", "trajectory", "historically", "future", "temporal", "change"]):
        return "temporal_trajectory"
    if any(w in q for w in ["false summit", "false mountain", "naturalized", "disguised", "appears"]):
        return "false_summit_detection"
    if any(w in q for w in ["kernel", "commitment", "reading", "interpretation"]):
        return "commitment_system_structure"
    return "other"


def check_kernel_context_template(text: str) -> dict[str, bool]:
    result = {}
    for name, pattern in TEMPLATE_ELEMENTS:
        result[name] = bool(re.search(pattern, text, re.IGNORECASE))
    return result


def main():
    files = sorted(DATA_DIR.glob("*.json"))
    if not files:
        print(f"No files in {DATA_DIR}", file=sys.stderr)
        sys.exit(1)

    all_omegas = []
    omega_id_by_file: dict[str, list[str]] = {}
    kernel_contexts = []
    id_file_counts: Counter = Counter()
    type_class_counts: Counter = Counter()
    question_type_counts: Counter = Counter()

    for fp in files:
        try:
            data = json.loads(fp.read_text(encoding="utf-8"))
        except Exception:
            continue

        omegas = data.get("omegas") or []
        file_ids = []
        for omega in omegas:
            oid = omega.get("id", "")
            tc = omega.get("type_class", "missing")
            question = omega.get("question", "")
            qtype = classify_question(question)

            all_omegas.append({
                "file": fp.name,
                "constraint_id": data.get("constraint_id") or fp.stem,
                "id": oid,
                "type_class": tc,
                "question_type": qtype,
                "question": question,
                "description": omega.get("description", ""),
                "impact": omega.get("impact", ""),
            })
            file_ids.append(oid)
            id_file_counts[oid] += 1
            type_class_counts[tc] += 1
            question_type_counts[qtype] += 1

        omega_id_by_file[fp.name] = file_ids

        kc = (data.get("commentary") or {}).get("kernel_context", "")
        if kc:
            kernel_contexts.append({
                "file": fp.name,
                "constraint_id": data.get("constraint_id") or fp.stem,
                "kernel_context": kc,
                "template_check": check_kernel_context_template(kc),
            })

    # Cluster: IDs appearing in ≥5 files
    candidate_templates = {k: v for k, v in id_file_counts.items() if v >= 5}

    # Template conformance: kernel_context with all 5 elements
    full_template_count = sum(
        1 for kc in kernel_contexts
        if all(kc["template_check"].values())
    )
    partial_counts = Counter()
    for kc in kernel_contexts:
        n = sum(kc["template_check"].values())
        partial_counts[n] += 1

    template_pct = full_template_count / len(kernel_contexts) * 100 if kernel_contexts else 0

    report = {
        "files_scanned": len(files),
        "total_omegas": len(all_omegas),
        "files_with_omegas": len(omega_id_by_file),
        "files_with_kernel_context": len(kernel_contexts),
        "omega_id_clusters": {
            "threshold": 5,
            "candidates": dict(sorted(candidate_templates.items(), key=lambda x: -x[1])),
            "all_id_counts": dict(sorted(id_file_counts.items(), key=lambda x: -x[1])[:50]),
        },
        "type_class_distribution": dict(sorted(type_class_counts.items(), key=lambda x: -x[1])),
        "question_type_distribution": dict(sorted(question_type_counts.items(), key=lambda x: -x[1])),
        "kernel_context_template": {
            "total_with_kc": len(kernel_contexts),
            "full_template_count": full_template_count,
            "full_template_pct": round(template_pct, 1),
            "formalization_candidate": template_pct >= 80,
            "partial_element_counts": dict(sorted(partial_counts.items())),
            "element_presence": {
                name: sum(1 for kc in kernel_contexts if kc["template_check"][name])
                for name, _ in TEMPLATE_ELEMENTS
            },
        },
        "all_omegas": all_omegas,
        "kernel_contexts": kernel_contexts,
    }

    OUT_DIR.mkdir(parents=True, exist_ok=True)
    json_path = OUT_DIR / "harvest_b1_omega.json"
    json_path.write_text(json.dumps(report, indent=2, ensure_ascii=False), encoding="utf-8")

    # Text summary
    lines = [
        "# B1 — Omega Mining Pass (run_01 Kernel Corpus)",
        "",
        f"**Files scanned**: {len(files)}  |  **Total omegas**: {len(all_omegas)}  |  "
        f"**Files with omegas**: {len(omega_id_by_file)}  |  "
        f"**Files with kernel_context**: {len(kernel_contexts)}",
        "",
        "## 1. Omega ID Clusters (recurring IDs, threshold ≥ 5 files)",
        "",
    ]
    if candidate_templates:
        lines.append("| Omega ID | Files containing it |")
        lines.append("|---|---|")
        for oid, count in sorted(candidate_templates.items(), key=lambda x: -x[1]):
            lines.append(f"| `{oid}` | {count} |")
    else:
        lines.append("No omega ID appears in ≥5 files — no candidate template fields.")
    lines.append("")

    lines.append("### Full ID frequency table (top 30)")
    lines.append("")
    lines.append("| Omega ID | Count |")
    lines.append("|---|---|")
    for oid, cnt in sorted(id_file_counts.items(), key=lambda x: -x[1])[:30]:
        marker = " ⭐" if cnt >= 5 else ""
        lines.append(f"| `{oid}` | {cnt}{marker} |")
    lines.append("")

    lines.append("## 2. type_class Distribution")
    lines.append("")
    lines.append("| type_class | Count | % |")
    lines.append("|---|---|---|")
    total_omegas = len(all_omegas)
    for tc, cnt in sorted(type_class_counts.items(), key=lambda x: -x[1]):
        pct = cnt / total_omegas * 100 if total_omegas else 0
        lines.append(f"| `{tc}` | {cnt} | {pct:.1f}% |")
    lines.append("")

    lines.append("## 3. Question Structure Classification")
    lines.append("")
    lines.append("| Question type | Count | % |")
    lines.append("|---|---|---|")
    for qt, cnt in sorted(question_type_counts.items(), key=lambda x: -x[1]):
        pct = cnt / total_omegas * 100 if total_omegas else 0
        lines.append(f"| `{qt}` | {cnt} | {pct:.1f}% |")
    lines.append("")

    lines.append("## 4. kernel_context Template Analysis")
    lines.append("")
    kc_report = report["kernel_context_template"]
    lines.append(f"**Files with kernel_context**: {kc_report['total_with_kc']}  |  "
                 f"**Full 5-element template**: {kc_report['full_template_count']} "
                 f"({kc_report['full_template_pct']}%)  |  "
                 f"**Formalization candidate** (≥80%): {kc_report['formalization_candidate']}")
    lines.append("")
    lines.append("Template elements (named kernel → sibling readings → authority grounding → "
                 "extraction mechanism → perspectival gap):")
    lines.append("")
    lines.append("| Element | Present in N kernel_context entries |")
    lines.append("|---|---|")
    for name, count in kc_report["element_presence"].items():
        pct = count / kc_report["total_with_kc"] * 100 if kc_report["total_with_kc"] else 0
        lines.append(f"| {name} | {count} ({pct:.0f}%) |")
    lines.append("")
    lines.append("### Element count distribution")
    lines.append("")
    lines.append("| Elements present (of 5) | Count of kernel_context entries |")
    lines.append("|---|---|")
    for n, cnt in sorted(partial_counts.items()):
        lines.append(f"| {n} | {cnt} |")

    md_path = OUT_DIR / "harvest_b1_omega.md"
    md_path.write_text("\n".join(lines) + "\n", encoding="utf-8")

    print(f"B1 complete.")
    print(f"  Files: {len(files)}, Omegas: {len(all_omegas)}")
    print(f"  Candidate template IDs (≥5 files): {len(candidate_templates)}")
    print(f"  kernel_context full template: {full_template_count}/{len(kernel_contexts)} "
          f"({template_pct:.0f}%)")
    print(f"  → {json_path}")
    print(f"  → {md_path}")


if __name__ == "__main__":
    main()
