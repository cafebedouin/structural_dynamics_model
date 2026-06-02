#!/usr/bin/env python3
"""Two-arm partition probe: kernel-vs-plain, cheap header read vs expensive full read.

The corpus rebuild needs to know whether a *cheap* representation of an archived story
(its harvested header) lets a model tell a contested KERNEL from a PLAIN constraint as
well as the *expensive* full story does. This script measures that on a sample.

  Arm A (expensive, ground truth): the full archived `.pl` story.
  Arm B (cheap, the at-scale candidate): the harvested header = the `beta_seeds.json`
         record (constraint_id + human_readable + topic_domain + full SUMMARY block),
         which is producible for every story in the archive.

Decision rule (per plan): the FALSE-NEGATIVE cell — A=kernel, B=plain — kernels the
cheap arm would MISS. Agreement % is reported but is NOT the decision rule.

Both arms run as one Haiku batch (custom_id `<id>::A` / `<id>::B`) with a prompt-cached
rubric. Outputs land in outputs/partition_probe/. This is a ONE-SHOT validation: it
prints the report and stops. It does not scale to the full archive and does not generate.

Usage:
    python3 python/partition_probe.py                 # N=100, seed=0
    python3 python/partition_probe.py --n 100 --seed 0
    python3 python/partition_probe.py --dry-run       # build requests + sample, no API
"""
import argparse
import json
import re
import sys
from pathlib import Path

import anthropic

REPO_ROOT = Path(__file__).resolve().parent.parent
ARCHIVE = REPO_ROOT / "prolog" / "archives" / "prolog_v5"
SEEDS = REPO_ROOT / "prolog" / "beta_seeds.json"
OUTDIR = REPO_ROOT / "outputs" / "partition_probe"
MODEL = "claude-haiku-4-5-20251001"

# Reuse the batch poller from the haiku generator (same client object shape).
sys.path.insert(0, str(REPO_ROOT))
from agent.generate_json_haiku import poll_batch  # noqa: E402

RUBRIC = """You classify ONE constraint story as a KERNEL or a PLAIN constraint. The
distinction is narrow — read it carefully.

A KERNEL (committer-axis contestation) requires ALL THREE:
  1. ONE shared commitment — a single standard, text, principle, rule, or term that
     multiple parties all invoke and treat as authoritative.
  2. Those parties hold MUTUALLY INCOMPATIBLE INTERPRETATIONS of what that commitment MEANS
     or REQUIRES — from the same words they would author different rules. Accepting one
     reading's account of the commitment denies another's (the readings forecloses one
     another, or coexist as rival interpretations held by different parties).
  3. The dispute is about the CONTENT of the commitment, not about who is advantaged by it.
Kernel examples: what 'free speech' protects; what counts as a 'living language'; what the
original text of a constitution requires; what 'abolition' demands; what makes a translation
'faithful'.

A PLAIN constraint is everything else — INCLUDING the common look-alike that is NOT a kernel:

  *** OBSERVER PERSPECTIVE IS NOT A KERNEL. *** If different parties merely EXPERIENCE the
  same constraint differently because of their power position — the powerful experience it
  as coordination/benefit while the weak experience it as extraction/harm, or an analyst
  calls it a 'hybrid' (tangled rope) while a participant calls it pure extraction (snare) —
  that is observer-axis perspective. Everyone AGREES what the constraint is and does; they
  sit at different ends of it. That is PLAIN.

  A single extraction scheme, tax rule, market mechanism, or coordination device with one
  operative meaning is PLAIN even when it has clear beneficiaries and victims and even when
  participants judge its fairness differently.

Decisive test: do the parties disagree about WHAT THE COMMITMENT MEANS (kernel), or do they
agree on what it is and only differ in how it affects them / how they judge it (plain)?

Respond with ONLY a JSON object — no prose, no code fences:
{"label": "kernel" | "plain", "confidence": <0.0-1.0>, "rationale": "<if kernel: name the ONE shared commitment and the >=2 incompatible readings of it; if plain: say whether it is observer-perspective-only>"}"""


def select_sample(n, seed):
    """Deterministic sample of constraint_ids from the archive (sorted then seeded)."""
    import random
    stems = sorted(p.stem for p in ARCHIVE.glob("*.pl"))
    rng = random.Random(seed)
    if n >= len(stems):
        return stems
    return sorted(rng.sample(stems, n))


def arm_a_content(cid):
    p = ARCHIVE / f"{cid}.pl"
    if not p.exists():
        return None
    return f"=== FULL CONSTRAINT STORY (Prolog source) for `{cid}` ===\n\n" + \
        p.read_text(encoding="utf-8", errors="replace")


def arm_b_content(seed):
    return (
        f"=== CONSTRAINT HEADER for `{seed['constraint_id']}` ===\n"
        f"TITLE: {seed['human_readable']}\n"
        f"DOMAIN: {seed.get('topic_domain', 'General')}\n"
        f"SUMMARY:\n{seed.get('summary', '') or ''}"
    )


def build_requests(sample, seeds_by_id):
    # custom_id must match ^[a-zA-Z0-9_-]{1,64}$ — constraint_ids can exceed 64 and the
    # arm separator must be alnum, so key by sample index ("<i>A"/"<i>B") + a map back.
    system = [{"type": "text", "text": RUBRIC, "cache_control": {"type": "ephemeral"}}]
    reqs, skipped, idmap = [], [], {}
    for i, cid in enumerate(sample):
        a = arm_a_content(cid)
        seed = seeds_by_id.get(cid)
        if a is None or seed is None:
            skipped.append(cid)
            continue
        for arm, content in (("A", a), ("B", arm_b_content(seed))):
            custom_id = f"{i}{arm}"
            idmap[custom_id] = {"cid": cid, "arm": arm}
            reqs.append({
                "custom_id": custom_id,
                "params": {
                    "model": MODEL,
                    "max_tokens": 400,
                    "system": system,
                    "messages": [{"role": "user", "content": content}],
                },
            })
    return reqs, skipped, idmap


def parse_label(raw):
    """Extract {label, confidence, rationale} from model text; tolerant of fences."""
    m = re.search(r"\{.*\}", raw, re.DOTALL)
    if not m:
        return {"label": None, "confidence": None, "rationale": "PARSE_FAIL", "raw": raw[:200]}
    try:
        obj = json.loads(m.group(0))
    except json.JSONDecodeError:
        return {"label": None, "confidence": None, "rationale": "JSON_FAIL", "raw": raw[:200]}
    lab = str(obj.get("label", "")).strip().lower()
    if lab not in ("kernel", "plain"):
        lab = None
    return {"label": lab, "confidence": obj.get("confidence"),
            "rationale": obj.get("rationale", "")}


def collect_results(client, batch_id):
    out = {}
    for result in client.messages.batches.results(batch_id):
        cid = result.custom_id
        if result.result.type != "succeeded":
            out[cid] = {"label": None, "confidence": None,
                        "rationale": f"API_{result.result.type}"}
            continue
        raw = "".join(b.text for b in result.result.message.content if b.type == "text")
        out[cid] = parse_label(raw)
    return out


def write_report(sample, A, B, skipped, n, seed):
    OUTDIR.mkdir(parents=True, exist_ok=True)

    # Confusion cells (only ids where BOTH arms returned a valid label).
    cells = {"kk": [], "kp": [], "pk": [], "pp": []}
    errors = []
    for cid in sample:
        a = A.get(cid, {}).get("label")
        b = B.get(cid, {}).get("label")
        if a is None or b is None:
            errors.append(cid)
            continue
        key = {("kernel", "kernel"): "kk", ("kernel", "plain"): "kp",
               ("plain", "kernel"): "pk", ("plain", "plain"): "pp"}[(a, b)]
        cells[key].append(cid)

    scored = len(cells["kk"]) + len(cells["kp"]) + len(cells["pk"]) + len(cells["pp"])
    a_kernels = len(cells["kk"]) + len(cells["kp"])
    agree = len(cells["kk"]) + len(cells["pp"])

    lines = []
    lines.append("# Partition probe — two-arm (full story vs harvested header)\n")
    lines.append(f"- model: `{MODEL}`  |  sample N={n} (seed={seed})  |  "
                 f"scored (both arms valid)={scored}\n")
    lines.append(f"- Arm A = full `.pl` (ground truth) · Arm B = harvested header "
                 f"(id+title+domain+SUMMARY)\n")
    if skipped:
        lines.append(f"- skipped (no .pl or no seed): {len(skipped)} — {skipped[:10]}\n")
    if errors:
        lines.append(f"- unscored (an arm returned no valid label): {len(errors)} — "
                     f"{errors[:10]}\n")

    # LEAD with the decision-relevant cell.
    lines.append("\n## Decision cell — FALSE NEGATIVES (A=kernel, B=plain)\n")
    lines.append(f"**{len(cells['kp'])} of {a_kernels} A-kernels missed by the cheap arm.** "
                 f"This is the decision rule (not agreement %).\n")
    if cells["kp"]:
        lines.append("\n| constraint_id | A rationale (kernel) | B rationale (plain) |\n")
        lines.append("|---|---|---|\n")
        for cid in cells["kp"]:
            ar = (A[cid].get("rationale") or "").replace("|", "/")[:160]
            br = (B[cid].get("rationale") or "").replace("|", "/")[:160]
            lines.append(f"| `{cid}` | {ar} | {br} |\n")
    else:
        lines.append("_(none — the cheap arm missed no A-kernels in this sample)_\n")

    lines.append("\n## 2x2 confusion (rows = Arm A truth, cols = Arm B)\n")
    lines.append("| A \\\\ B | B=kernel | B=plain |\n|---|---|---|\n")
    lines.append(f"| **A=kernel** | {len(cells['kk'])} | {len(cells['kp'])} (false neg) |\n")
    lines.append(f"| **A=plain** | {len(cells['pk'])} (false pos) | {len(cells['pp'])} |\n")
    lines.append(f"\n- agreement: {agree}/{scored} "
                 f"({100*agree/scored:.1f}% — reported, NOT the decision rule)\n")
    lines.append(f"- A-kernel base rate: {a_kernels}/{scored} "
                 f"({100*a_kernels/scored:.1f}%)\n")
    lines.append(f"- false positives (A=plain, B=kernel): {len(cells['pk'])}\n")

    lines.append("\n## Recommendation\n")
    if a_kernels == 0:
        lines.append("- Arm A found **no kernels** in this sample — inconclusive; "
                     "consider a larger N or check the rubric against known kernels.\n")
    else:
        miss = 100 * len(cells["kp"]) / a_kernels
        verdict = ("TRUSTWORTHY" if miss == 0 else
                   "MARGINAL" if miss <= 15 else "NOT trustworthy")
        lines.append(f"- Cheap arm misses {len(cells['kp'])}/{a_kernels} kernels "
                     f"({miss:.0f}% false-negative rate) → **{verdict}** for at-scale use, "
                     f"pending your read of the missed cases above.\n")
    lines.append("- One-shot validation: STOP here for feedback before any at-scale run.\n")

    (OUTDIR / "report.md").write_text("".join(lines), encoding="utf-8")
    (OUTDIR / "raw_results.json").write_text(
        json.dumps({"arm_a": A, "arm_b": B, "cells": cells,
                    "skipped": skipped, "errors": errors}, indent=2, ensure_ascii=False),
        encoding="utf-8")
    return "".join(lines)


def run_control(path, poll_interval):
    """Positive control: run authored items (e.g. kernel_seeds.json) through the SAME
    rubric, header-style. A 'kernel'-finding probe must label known kernels 'kernel';
    otherwise a 0-count over the archive is an unfalsified diagnostic, not a finding."""
    items = json.loads(Path(path).read_text(encoding="utf-8"))
    system = [{"type": "text", "text": RUBRIC, "cache_control": {"type": "ephemeral"}}]
    reqs, idmap = [], {}
    for i, it in enumerate(items):
        cid = it.get("kernel_id") or it.get("constraint_id") or f"item{i}"
        content = (f"=== CONSTRAINT HEADER for `{cid}` ===\n"
                   f"TITLE: {it.get('human_readable', '')}\n"
                   f"DOMAIN: {it.get('topic_domain', 'General')}\n"
                   f"SUMMARY:\n{it.get('summary', '') or ''}")
        custom_id = f"{i}C"
        idmap[custom_id] = cid
        reqs.append({"custom_id": custom_id, "params": {
            "model": MODEL, "max_tokens": 400, "system": system,
            "messages": [{"role": "user", "content": content}]}})
    client = anthropic.Anthropic()
    batch = client.messages.batches.create(requests=reqs)
    print(f"control batch: {batch.id} ({len(reqs)} authored items from {Path(path).name})")
    poll_batch(client, batch.id, poll_interval)
    res = collect_results(client, batch.id)
    labels = {idmap[k]: v for k, v in res.items() if k in idmap}
    k = sum(1 for v in labels.values() if v["label"] == "kernel")
    p = sum(1 for v in labels.values() if v["label"] == "plain")
    e = sum(1 for v in labels.values() if v["label"] is None)
    OUTDIR.mkdir(parents=True, exist_ok=True)
    (OUTDIR / "control_results.json").write_text(
        json.dumps(labels, indent=2, ensure_ascii=False), encoding="utf-8")
    print(f"\nPOSITIVE CONTROL ({Path(path).name}): kernel={k} plain={p} error={e} "
          f"of {len(labels)} — detection rate {100*k/max(k+p,1):.0f}%")
    for cid, v in sorted(labels.items()):
        print(f"  {(v['label'] or 'ERR'):6s}  {cid}")
    return labels


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--n", type=int, default=100)
    ap.add_argument("--seed", type=int, default=0)
    ap.add_argument("--poll-interval", type=int, default=30)
    ap.add_argument("--dry-run", action="store_true",
                    help="build sample + requests, write sample.json, no API call")
    ap.add_argument("--control-seeds", default=None,
                    help="positive control: run authored items (e.g. prolog/kernel_seeds.json) "
                         "through the rubric and report kernel-detection rate, then stop")
    args = ap.parse_args()

    if args.control_seeds:
        run_control(args.control_seeds, args.poll_interval)
        return

    seeds = json.loads(SEEDS.read_text(encoding="utf-8"))
    seeds_by_id = {s["constraint_id"]: s for s in seeds}

    sample = select_sample(args.n, args.seed)
    OUTDIR.mkdir(parents=True, exist_ok=True)
    (OUTDIR / "sample.json").write_text(
        json.dumps({"n": args.n, "seed": args.seed, "ids": sample}, indent=2),
        encoding="utf-8")

    reqs, skipped, idmap = build_requests(sample, seeds_by_id)
    print(f"sample={len(sample)}  requests={len(reqs)}  skipped={len(skipped)}")
    if args.dry_run:
        print("DRY RUN — no batch submitted. sample.json written.")
        for r in reqs[:2]:
            print(f"  example custom_id={r['custom_id']}  "
                  f"chars={len(r['params']['messages'][0]['content'])}")
        return

    client = anthropic.Anthropic()
    batch = client.messages.batches.create(requests=reqs)
    print(f"batch created: {batch.id}")
    poll_batch(client, batch.id, args.poll_interval)

    results = collect_results(client, batch.id)
    A, B = {}, {}
    for custom_id, parsed in results.items():
        meta = idmap.get(custom_id)
        if not meta:
            continue
        (A if meta["arm"] == "A" else B)[meta["cid"]] = parsed

    report = write_report(sample, A, B, skipped, args.n, args.seed)
    print("\n" + report)
    print(f"\nWritten: {OUTDIR}/report.md, raw_results.json, sample.json")


if __name__ == "__main__":
    main()
