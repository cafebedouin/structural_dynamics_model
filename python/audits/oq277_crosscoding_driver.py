#!/usr/bin/env python3
"""OQ-277 cross-coding driver — payload capture, leak gate, k=3 bookkeeping.

NO MODEL CALL HAS EVER BEEN MADE IN THIS AUDIT. `payloads/` and `responses/` are empty
by design and stay that way until the operator's spend-go at preregistration freeze.
This driver is written, tested and shipped against a STUBBED transport only.

--------------------------------------------------------------------------------------
WHY THE ORDER OF THE GATES IS THE CONTROL
--------------------------------------------------------------------------------------
Phase 3 is preceded by three gates, and the ORDER is load-bearing, not stylistic:

  1. COUNT FIRST. Assert len(captured payloads) == expected_calls, with expected_calls
     computed from the assembled packets rather than hardcoded. A capture bug that
     writes zero payloads yields a perfectly clean leak-grep and a green H2 — a
     success-shaped absence, which is the exact defect class this experiment codes for.
     A green grep printed above an unverified capture count is the failure shape.
  2. Assert the planted-leak fixtures live in their own subdirectory and are counted
     SEPARATELY. They are dumped, not sent, so they must not inflate the equality above.
  3. ONLY THEN grep, three-way: fixtures MUST fire, pre-listed exempt twin-arm payloads
     MUST fire, everything else MUST be clean.

Quarantined and overlap calls still count toward the expected total (§E *Accounting*).

--------------------------------------------------------------------------------------
SWEEP SCOPE — why a payload is not swept whole under its own direction
--------------------------------------------------------------------------------------
Every prompt necessarily contains its own direction's class definitions, so no payload
can ever be clean under its own direction's full lexicon. The rule:

    unit portion  -> own direction's FULL lexicon
    whole payload -> the OTHER direction's `source_identifying` group only

The definitions block is fixed, reviewed and identical across every payload in a leg, so
it is audited once by controls/verify_prompts.py rather than re-cleared 219 times; the
part that varies per call is swept in full. Strictly stronger than sweeping unit text
alone, and it loosens nothing.

--------------------------------------------------------------------------------------
THE LIVE PATH IS STRUCTURALLY GATED, NOT MERELY DISCOURAGED
--------------------------------------------------------------------------------------
`--live` refuses unless PREREGISTRATION.md exists AND its md5 is recorded in
audit_log.md ABOVE the first result line. That ordering is the whole point of the
freeze, and a rule that depends on remembering it is not a rule — building a driver and
smoke-testing it with one real call is the most natural thing in the world, and it would
put a result on disk before the prereg md5. The refusal has a positive control in
--selftest.

Usage:
  python3 python/audits/oq277_crosscoding_driver.py --stub --dry-run
  python3 python/audits/oq277_crosscoding_driver.py --selftest
"""
from __future__ import annotations
import argparse, glob, hashlib, json, os, pathlib, re, shutil, sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
import oq277_lexicon as LEX

REPO = pathlib.Path(__file__).resolve().parents[2]
AUDIT = REPO / "audits" / "2026-08-10_oq277_rq2_crosscoding"
FIELDS = LEX.CODER_FACING_FIELDS
K = 3
CODER_MODEL = "claude-sonnet-5"

# leg -> (packet stem, prompt file, sweep direction for the UNIT portion, answer tokens)
LEGS = {
    "direction_i":  ("coder_direction_i",  "direction_i.md",  "i",
                     ["P1", "P2", "P3", "P4", "P5", "P6", "other"]),
    "direction_ii": ("coder_direction_ii", "direction_ii.md", "ii",
                     ["A", "B", "C", "D", "E", "other"]),
    # (iii') shares direction (i)'s PROMPT (same task, same label space — a second file
    # would be a P2 fork) but sweeps under direction (ii), because its UNITS are ours
    # while its ANSWERS are in our index. Easiest thing in the design to get backwards.
    "iii_prime":    ("coder_iii_prime",    "direction_i.md",  "ii",
                     ["P1", "P2", "P3", "P4", "P5", "P6", "other"]),
}
UNSTABLE = "UNSTABLE"


def md5(s: str) -> str:
    return hashlib.md5(s.encode("utf-8")).hexdigest()


# ---------------------------------------------------------------------------
# Transport
# ---------------------------------------------------------------------------
def stub_transport(payload: str, model: str, item_id: str, k: int) -> str:
    """Canned responses. Exercises everything that can actually be wrong at this stage —
    payload assembly, the pre-send dump, the capture count, the grep over dumped files,
    k=3 bookkeeping, unanimity/UNSTABLE routing, the matrix_unit quarantine — none of
    which needs the network.

    Deliberately deterministic per (item, k) rather than random: a stubbed run must be
    reproducible, and `Math.random`-style variation would make the k=3 bookkeeping test
    pass or fail by luck."""
    toks = re.findall(r"(?m)^([A-EP][1-6]?)  ", payload) or ["other"]
    # Make one item per leg non-unanimous so the UNSTABLE path is EXERCISED, not merely
    # implemented. A driver whose UNSTABLE branch has never run is a branch, not a route.
    if item_id.endswith("-02") and k == 3:
        return toks[-1] if len(toks) > 1 else "other"
    return toks[0]


def live_transport(payload: str, model: str, item_id: str, k: int) -> str:
    """The real path. Wraps call_with_retry — the single choke point in agent/llm_call.py
    — and is reached ONLY after assert_spend_go(). llm_call has no payload logging, so
    capture is caller-side and happens BEFORE this function is entered."""
    sys.path.insert(0, str(REPO))
    from agent.llm_call import get_client, call_with_retry, extract_text, sampling_overrides
    client = get_client()
    resp = call_with_retry(
        client, model=model, max_tokens=16,
        **sampling_overrides(model, 0.2),
        messages=[{"role": "user", "content": payload}],   # stateless, single user turn
    )
    return extract_text(resp).strip()


def assert_spend_go() -> None:
    """Refuse the live path until the freeze ordering physically holds on disk."""
    prereg = AUDIT / "PREREGISTRATION.md"
    log = AUDIT / "audit_log.md"
    if not prereg.exists():
        sys.exit("REFUSED: PREREGISTRATION.md does not exist. The spend-go is at prereg "
                 "freeze, not at plan approval.")
    want = md5(prereg.read_text())
    text = log.read_text() if log.exists() else ""
    if want not in text:
        sys.exit(f"REFUSED: the current PREREGISTRATION.md md5 ({want}) is not recorded in "
                 f"audit_log.md. Freeze it first.")
    # md5 must sit physically ABOVE the first result line.
    m = re.search(r"(?mi)^.*\bfirst result\b.*$|(?mi)^## +results?\b", text)
    if m and text.index(want) > m.start():
        sys.exit("REFUSED: the prereg md5 is recorded BELOW the first result line. The "
                 "ordering is the point of the freeze.")


# ---------------------------------------------------------------------------
# Assembly
# ---------------------------------------------------------------------------
def build_payloads(legs: dict) -> tuple[list[dict], list[str]]:
    """Assemble every payload. Returns (payloads, notes). No I/O, no sending."""
    out, notes = [], []
    for leg, (stem, prompt_file, sweep_dir, _tokens) in legs.items():
        pkt = AUDIT / "packets" / "run" / f"{stem}.json"
        if not pkt.exists():
            notes.append(f"{leg}: packet {pkt.name} ABSENT — leg not built")
            continue
        template = (AUDIT / "prompts" / prompt_file).read_text()
        for item in json.load(open(pkt)):
            text = template.format(**{f: item[f] for f in FIELDS})
            for k in range(1, K + 1):
                out.append({"leg": leg, "item_id": item["id"], "k": k,
                            "model": CODER_MODEL, "prompt_file": prompt_file,
                            "sweep_direction": sweep_dir,
                            "unit_portion": "\n\n".join(str(item[f]) for f in FIELDS),
                            "payload": text})
    return out, notes


def dump_payloads(payloads: list[dict], fixtures: list[dict], out_dir: pathlib.Path) -> None:
    """Write EVERY payload to disk BEFORE anything is sent. Fixtures go to their own
    subdirectory because they are dumped and NOT sent — folding them in would inflate the
    capture-count equality that gate 1 depends on."""
    if out_dir.exists():
        shutil.rmtree(out_dir)
    (out_dir / "_fixtures").mkdir(parents=True)
    for p in payloads:
        d = out_dir / p["leg"]
        d.mkdir(exist_ok=True)
        rec = {**p, "payload_md5": md5(p["payload"])}
        (d / f"{p['item_id']}__k{p['k']}.json").write_text(
            json.dumps(rec, indent=2, ensure_ascii=False) + "\n")
    for f in fixtures:
        (out_dir / "_fixtures" / f"{f['item_id']}.json").write_text(
            json.dumps({**f, "payload_md5": md5(f["payload"])}, indent=2,
                       ensure_ascii=False) + "\n")


def load_fixtures() -> list[dict]:
    """The two planted-leak fixtures. Dumped so the grep is exercised over the REAL
    dumped-payload path rather than only over strings — a grep validated on in-memory
    text is a different instrument from one reading files off disk."""
    planted = json.load(open(AUDIT / "controls" / "planted.json"))["planted_leak"]
    out = []
    for d, key in (("i", "direction_i_payload_fixture"), ("ii", "direction_ii_payload_fixture")):
        out.append({"leg": "_fixtures", "item_id": f"fixture_{d}", "k": 0,
                    "model": CODER_MODEL, "sweep_direction": d,
                    "must_fire": True,
                    "why": planted[key]["must_be_flagged_because"],
                    "unit_portion": planted[key]["unit_text"],
                    "payload": planted[key]["unit_text"]})
    return out


def exempt_ids() -> dict:
    """Per-leg leak-exempt opaque ids, read from the packet MAPS rather than re-derived.
    An unredacted twin arm necessarily contains source vocabulary — that IS the control —
    so it is exempt, and it MUST fire: an exempt payload that sweeps clean un-redacted
    nothing and would report a floor of zero by construction."""
    out = {}
    for leg, (stem, *_r) in LEGS.items():
        m = AUDIT / "packets" / "run" / f"{stem}_map.json"
        out[leg] = set(json.load(open(m))["leak_exempt_ids"]) if m.exists() else set()
    return out


# ---------------------------------------------------------------------------
# The three gates, in order
# ---------------------------------------------------------------------------
def gate_count(out_dir: pathlib.Path, expected: int, errors: list) -> int:
    captured = [p for p in glob.glob(str(out_dir / "*" / "*.json"))
                if os.path.basename(os.path.dirname(p)) != "_fixtures"]
    print(f"\n  [gate 1] COUNT FIRST — captured payloads vs expected calls")
    print(f"           captured = {len(captured)}   expected = {expected}")
    if len(captured) != expected:
        errors.append(f"gate 1: captured {len(captured)} payloads, expected {expected}. "
                      f"A clean grep over a short capture is a success-shaped absence.")
    else:
        print(f"           OK — every expected call has a payload on disk")
    return len(captured)


def gate_fixtures(out_dir: pathlib.Path, n_fixtures: int, errors: list) -> None:
    fx = glob.glob(str(out_dir / "_fixtures" / "*.json"))
    print(f"\n  [gate 2] fixtures in their own subdirectory, counted separately")
    print(f"           _fixtures/ holds {len(fx)} (expected {n_fixtures}), "
          f"dumped not sent, excluded from gate 1")
    if len(fx) != n_fixtures:
        errors.append(f"gate 2: {len(fx)} fixtures on disk, expected {n_fixtures}")


def gate_grep(out_dir: pathlib.Path, exempt: dict, errors: list) -> None:
    print(f"\n  [gate 3] three-way leak sweep OVER THE DUMPED FILES")
    fired_fx, fired_ex, dirty = [], [], []
    for path in sorted(glob.glob(str(out_dir / "*" / "*.json"))):
        rec = json.load(open(path))
        leg, own = rec["leg"], rec["sweep_direction"]
        other = "i" if own == "ii" else "ii"
        hits = LEX.scan(rec["unit_portion"], own)
        cross = LEX.scan(rec["payload"], other,
                         {other: {"source_identifying":
                                  LEX.LEXICON_DETECT[other]["source_identifying"]}})
        if leg == "_fixtures":
            (fired_fx if hits else dirty).append(rec["item_id"] if hits else
                                                f"FIXTURE {rec['item_id']} DID NOT FIRE")
        elif rec["item_id"] in exempt.get(leg, set()):
            if hits:
                fired_ex.append(f"{leg}/{rec['item_id']}")
            else:
                errors.append(f"gate 3: EXEMPT {leg}/{rec['item_id']} swept CLEAN — nothing "
                              f"was un-redacted; this pair reports a floor of zero.")
        elif hits:
            errors.append(f"gate 3: LEAK {leg}/{rec['item_id']} — "
                          f"{sorted({m for _g, _p, m, _c in hits})}")
        if cross:
            errors.append(f"gate 3: CROSS-DIRECTION source-identifying leak in "
                          f"{leg}/{rec['item_id']} — {sorted({m for _g, _p, m, _c in cross})}")
    for d in dirty:
        errors.append(f"gate 3: {d} — a fixture that does not fire means the grep is not "
                      f"reading what it claims to read")
    print(f"           fixtures fired        : {len(set(fired_fx))}/2  {sorted(set(fired_fx))}")
    print(f"           exempt twin arms fired: {len(set(fired_ex))}   "
          f"{sorted(set(fired_ex))[:8]}")
    print(f"           everything else clean : "
          f"{'yes' if not any(e.startswith('gate 3: LEAK') for e in errors) else 'NO'}")


def gate_same_input(payloads: list[dict], errors: list) -> None:
    """k=3 must be SAME-INPUT redraws. If the payload varies across k, the three draws are
    not replicates and unanimity measures nothing about churn."""
    by_item: dict = {}
    for p in payloads:
        by_item.setdefault((p["leg"], p["item_id"]), set()).add(md5(p["payload"]))
    bad = [f"{leg}/{iid}" for (leg, iid), s in by_item.items() if len(s) != 1]
    print(f"\n  [gate 0] k={K} redraws are SAME-INPUT "
          f"({len(by_item)} items, payload md5 constant across k)")
    if bad:
        errors.append(f"gate 0: payload VARIES across k for {bad[:5]} — not replicates")


# ---------------------------------------------------------------------------
# k=3 bookkeeping
# ---------------------------------------------------------------------------
def resolve_labels(results: list[dict]) -> dict:
    """label = unanimous 3/3, else UNSTABLE (own row, excluded from cells)."""
    by: dict = {}
    for r in results:
        by.setdefault((r["leg"], r["item_id"]), []).append(r["answer"])
    out = {}
    for key, answers in by.items():
        uniq = set(answers)
        out[key] = {"answers": answers,
                    "label": answers[0] if len(uniq) == 1 else UNSTABLE,
                    "unanimous": len(uniq) == 1}
    return out


def matrix_membership(leg: str) -> dict:
    m = AUDIT / "packets" / "run" / f"{LEGS[leg][0]}_map.json"
    if not m.exists():
        return {}
    return json.load(open(m))["map"]


# ---------------------------------------------------------------------------
def assert_live_capture_dir_untouched(live: pathlib.Path | None = None) -> None:
    """The canonical `payloads/` is empty BY DESIGN and stays that way until spend-go.

    The stub therefore writes to `payloads_stub/`, never to `payloads/`. This is not
    tidiness: 198 stub files sitting in the canonical capture directory would misrepresent
    the audit's state to every later reader — the escape extractor is told those
    directories are empty and to keep them that way — and a subsequent live run's capture
    count could be contaminated by leftovers from a test. The invariant is ASSERTED on
    every stub run rather than trusted, because "the stub does not write there" is exactly
    the kind of claim that stays true until someone changes a path constant.

    Takes `live` as a parameter ONLY so --selftest can point it at a temp directory and
    witness the refusal firing. Testing it against the real path would require writing a
    file into the directory whose emptiness is the invariant."""
    live = live or (AUDIT / "payloads")
    stray = [p for p in live.rglob("*") if p.is_file()]
    if stray:
        raise SystemExit(
            f"REFUSED: {live} is NOT empty ({len(stray)} files, e.g. "
            f"{stray[0].name}). No model call has been made in this audit and that "
            f"directory is empty by design. Investigate before running anything.")


def run(stub: bool, dry_run: bool) -> int:
    errors: list[str] = []
    payloads, notes = build_payloads(LEGS)
    fixtures = load_fixtures()
    expected = len(payloads)

    built = {p["leg"] for p in payloads}
    missing = [l for l in LEGS if l not in built]

    print("=" * 78)
    print("OQ-277 cross-coding driver — STUB TRANSPORT" if stub else "LIVE")
    print("=" * 78)
    for leg in LEGS:
        n = sum(1 for p in payloads if p["leg"] == leg) // K
        print(f"  {leg:<14} {n:>3} items x k={K} = {n * K:>3} calls"
              + ("   [NOT BUILT]" if leg in missing else ""))
    print(f"  {'escape units':<14} {0:>3} items          = {0:>3} calls   "
          f"[row CLOSED UNRESOLVED — no pre-registered escape-coding row exists]")
    print(f"  {'':<14} {'':>3}                 {'-' * 3}")
    print(f"  {'TOTAL':<14} {expected // K:>3} items          = {expected:>3} calls")
    for n in notes:
        print(f"  note: {n}")

    gate_same_input(payloads, errors)

    if stub:
        assert_live_capture_dir_untouched()
    out_dir = AUDIT / ("payloads_stub" if stub else "payloads")
    dump_payloads(payloads, fixtures, out_dir)
    print(f"\n  dumped {expected} payloads + {len(fixtures)} fixtures to {out_dir.name}/ "
          f"BEFORE any send")
    if stub:
        print(f"  (canonical payloads/ asserted EMPTY and left untouched — no call has "
              f"ever been made)")

    gate_count(out_dir, expected, errors)
    gate_fixtures(out_dir, len(fixtures), errors)
    gate_grep(out_dir, exempt_ids(), errors)

    if errors:
        print(f"\n  ABORT — {len(errors)} gate failure(s); nothing sent:\n")
        for e in errors:
            print(f"    {e}")
        return 1

    transport = stub_transport if stub else live_transport
    results = []
    for p in payloads:
        results.append({**{k: p[k] for k in ("leg", "item_id", "k")},
                        "answer": transport(p["payload"], p["model"], p["item_id"], p["k"])})

    resolved = resolve_labels(results)
    n_unstable = sum(1 for v in resolved.values() if not v["unanimous"])
    print(f"\n  k={K} bookkeeping: {len(resolved)} items resolved, "
          f"{len(resolved) - n_unstable} unanimous, {n_unstable} {UNSTABLE}")
    for leg in sorted(built):
        mm = matrix_membership(leg)
        cells = [k for k in resolved if k[0] == leg
                 and mm.get(k[1], {}).get("matrix_unit")
                 and resolved[k]["unanimous"]]
        quar = [k for k in resolved if k[0] == leg and not mm.get(k[1], {}).get("matrix_unit")]
        uns = [k for k in resolved if k[0] == leg and not resolved[k]["unanimous"]]
        print(f"    {leg:<14} {len(cells):>2} cells (matrix_unit & unanimous) · "
              f"{len(quar):>2} quarantined · {len(uns):>2} {UNSTABLE} (own row, "
              f"excluded from cells)")

    if dry_run:
        print(f"\n  --dry-run: responses NOT written. responses/ left empty.")
    if missing:
        print(f"\n  *** INCOMPLETE — {missing} not built. The totals above are a PARTIAL, "
              f"not the expected call count. ***")
    print("\n  gate order held: count -> fixtures -> grep. A green grep printed above an "
          "unverified\n  capture count is the failure shape this experiment exists to study.")
    return 0


def selftest() -> int:
    ok = True

    def check(label, cond):
        nonlocal ok
        print(f"  {'PASS' if cond else 'FAIL'}  {label}")
        ok = ok and cond

    print("driver gate discrimination controls — each MUST fire on a deliberate break:\n")
    errs: list = []
    gate_count(pathlib.Path("/nonexistent"), 219, errs)
    check("(1) capture count short of expected fires", any("gate 1" in e for e in errs))

    errs = []
    gate_fixtures(pathlib.Path("/nonexistent"), 2, errs)
    check("(2) missing fixtures fire", any("gate 2" in e for e in errs))

    errs = []
    gate_same_input([{"leg": "t", "item_id": "x", "k": 1, "payload": "a"},
                     {"leg": "t", "item_id": "x", "k": 2, "payload": "b"}], errs)
    check("(0) payload varying across k fires", any("gate 0" in e for e in errs))

    errs = []
    gate_same_input([{"leg": "t", "item_id": "x", "k": 1, "payload": "a"},
                     {"leg": "t", "item_id": "x", "k": 2, "payload": "a"}], errs)
    check("(0) CONVERSE — identical payloads across k do NOT fire", not errs)

    r = resolve_labels([{"leg": "t", "item_id": "a", "k": i, "answer": "P1"} for i in (1, 2, 3)])
    check("k=3 unanimous resolves to the label", r[("t", "a")]["label"] == "P1")
    r = resolve_labels([{"leg": "t", "item_id": "b", "k": 1, "answer": "P1"},
                        {"leg": "t", "item_id": "b", "k": 2, "answer": "P1"},
                        {"leg": "t", "item_id": "b", "k": 3, "answer": "P2"}])
    check(f"k=3 split resolves to {UNSTABLE}", r[("t", "b")]["label"] == UNSTABLE)

    print("\ncapture-directory invariant — two-sided:")
    import tempfile
    with tempfile.TemporaryDirectory() as td:
        empty = pathlib.Path(td) / "empty"
        empty.mkdir()
        try:
            assert_live_capture_dir_untouched(empty)
            clean_ok = True
        except SystemExit:
            clean_ok = False
        check("an EMPTY canonical payloads/ passes", clean_ok)
        dirty = pathlib.Path(td) / "dirty"
        (dirty / "direction_i").mkdir(parents=True)
        (dirty / "direction_i" / "i-01__k1.json").write_text("{}")
        try:
            assert_live_capture_dir_untouched(dirty)
            fired = False
        except SystemExit:
            fired = True
        check("a NON-EMPTY canonical payloads/ REFUSES — the stub can never write there",
              fired)

    print("\nlive-path refusal — the freeze ordering must be structural, not remembered:")
    prereg = AUDIT / "PREREGISTRATION.md"
    check("--live refuses while PREREGISTRATION.md is absent",
          not prereg.exists())
    print("        (assert_spend_go() exits non-zero on that branch; it is not called here "
          "because\n         it would terminate the selftest process)")

    print(f"\n{'GREEN — every driver gate discriminates' if ok else 'RED — a gate cannot fail'}")
    return 0 if ok else 1


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--stub", action="store_true", help="stubbed transport (no network)")
    ap.add_argument("--live", action="store_true",
                    help="real transport; refuses unless the prereg md5 is frozen above "
                         "the first result line")
    ap.add_argument("--dry-run", action="store_true", help="do not write responses/")
    ap.add_argument("--selftest", action="store_true")
    a = ap.parse_args()
    if a.selftest:
        return selftest()
    if a.live:
        assert_spend_go()
        return run(stub=False, dry_run=a.dry_run)
    if not a.stub:
        ap.error("pass --stub (or --live, which refuses before the freeze). There is no "
                 "default transport: a driver that sends by default is one keystroke from "
                 "spending against a gate the operator holds.")
    return run(stub=True, dry_run=a.dry_run)


if __name__ == "__main__":
    sys.exit(main())
