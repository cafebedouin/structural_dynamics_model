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
import argparse, glob, hashlib, json, os, pathlib, re, shutil, sys, tempfile

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
#: Explicit boundary in audit_log.md marking where CODING results begin. The prereg md5 must
#: appear above it. A sentinel rather than a prose match, so the check cannot pass by failing
#: to find anything.
SENTINEL = "<!--OQ277-FIRST-CODING-RESULT-->"


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
    # (0) Every designed leg must be BUILT. Checked before the md5, because an md5 over an
    # incomplete design is a success-shaped token: it looks exactly like a freeze and would
    # pass every check below it. A prereg frozen while a leg is missing would silently
    # re-pre-register a smaller experiment as though it were the designed one.
    unbuilt = [leg for leg, (stem, *_r) in LEGS.items()
               if not (AUDIT / "packets" / "run" / f"{stem}.json").exists()]
    if unbuilt:
        sys.exit(f"REFUSED: {unbuilt} not built. The freeze covers the DESIGNED experiment, "
                 f"not the subset that happens to be assembled. Build every leg, re-assemble "
                 f"PREREGISTRATION.md, and re-stamp its md5 before any call.")
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
    # The md5 must sit physically ABOVE the first coding result. That boundary is marked by
    # an explicit sentinel rather than inferred from prose: an earlier version searched for a
    # line saying "first result", the log contained no such line, and the check therefore
    # passed VACUOUSLY — a gate satisfied by the absence of its own input, which is the
    # defect class this experiment codes for. Absent sentinel now FAILS CLOSED.
    if SENTINEL not in text:
        sys.exit(f"REFUSED: audit_log.md carries no {SENTINEL} marker, so 'the md5 is above "
                 f"the first result line' cannot be checked. A boundary that cannot be "
                 f"located is not a boundary. Add the sentinel where coding results will "
                 f"begin.")
    if text.index(want) > text.index(SENTINEL):
        sys.exit("REFUSED: the prereg md5 is recorded BELOW the first-result sentinel. The "
                 "ordering is the entire point of the freeze.")


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


def write_response(resp_dir: pathlib.Path, leg: str, item_id: str, k: int, raw: str) -> pathlib.Path:
    """Persist ONE raw response and verify it landed, BEFORE the next call issues.

    Raw text is the datum; the resolved label is derived. This writes the text with no
    parsing, no normalisation and no aggregation in front of it, so a parse bug, an
    adjudication bug or a later capture bug degrades to RECOVERABLE rather than total.

    Write-then-verify is per CALL, not per run: a run that dies at call 140 leaves 140
    recoverable answers instead of zero. Verifying at the end would have the same failure
    profile as not verifying at all for every call that never got made.
    """
    d = resp_dir / leg
    d.mkdir(parents=True, exist_ok=True)
    p = d / f"{item_id}__k{k}.json"
    p.write_text(json.dumps(
        {"leg": leg, "item_id": item_id, "k": k, "raw": raw}, ensure_ascii=False) + "\n")
    if not p.exists() or p.stat().st_size == 0:
        sys.exit(f"ABORT: response for {item_id} k={k} did not land at {p}. Halting before "
                 f"the next call — a run that cannot persist must not keep spending.")
    return p


def gate_responses(resp_dir: pathlib.Path, expected: int, errors: list) -> None:
    """The mirror of gate 1, on the OUTPUT side. Gate 1 counts payloads — inputs. Nothing
    counted responses, so a run could (and did, 2026-08-11) spend 219 calls, pass every
    gate, and persist nothing.

    Count alone is NOT sufficient: it passes if every file is written empty. So this
    asserts three things — the file is there, it is non-empty, and its answer parses to a
    token in that leg's fixed vocabulary. The third is what makes it a check on the datum
    rather than on the filesystem.

    Out-of-vocabulary answers are reported, never coerced, and this gate runs AFTER every
    response is on disk — so a failure here is a finding with its evidence retained, not a
    second loss.
    """
    print(f"\n  [gate 4] OUTPUT SIDE — persisted responses vs expected calls")
    files = glob.glob(str(resp_dir / "*" / "*.json"))
    print(f"           persisted = {len(files)}   expected = {expected}")
    if len(files) != expected:
        errors.append(f"gate 4: persisted {len(files)} responses, expected {expected}. "
                      f"Gate the output, not only the input — a pipeline verified end-to-end "
                      f"on what it CONSUMES can produce nothing and report green.")
    empty, bad_vocab = [], []
    for f in files:
        leg = os.path.basename(os.path.dirname(f))
        if os.path.getsize(f) == 0:
            empty.append(os.path.basename(f))
            continue
        try:
            rec = json.loads(open(f).read())
            raw = (rec.get("raw") or "").strip()
        except Exception:                                                  # noqa: BLE001
            empty.append(os.path.basename(f))
            continue
        if not raw:
            empty.append(os.path.basename(f))
        elif leg in LEGS and raw not in LEGS[leg][3]:
            bad_vocab.append(f"{os.path.basename(f)}={raw!r}")
    if empty:
        errors.append(f"gate 4: {len(empty)} response file(s) empty or unparseable "
                      f"({empty[:5]}). A file count alone passes on zero-byte writes.")
    if bad_vocab:
        errors.append(f"gate 4: {len(bad_vocab)} response(s) outside the fixed vocabulary "
                      f"({bad_vocab[:5]}). Reported, never coerced; the raw text is on disk.")
    if not (len(files) != expected or empty or bad_vocab):
        print(f"           OK — every call left a non-empty, in-vocabulary answer on disk")


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
def assert_live_response_dir_untouched(live: pathlib.Path | None = None) -> None:
    """Same invariant as the payload directory, on the output side: the stub writes to
    `responses_stub/` and must never touch canonical `responses/`.

    Added 2026-08-11 with the capture path itself. The output side had no directory
    discipline because it had no writer at all — every protection in this driver was built
    for inputs. `live` is a parameter for the same reason as its sibling: so the selftest
    can witness the refusal without dirtying the directory whose emptiness is the invariant.
    """
    live = live or (AUDIT / "responses")
    stray = [p for p in live.rglob("*") if p.is_file()] if live.exists() else []
    if stray:
        raise SystemExit(
            f"REFUSED: {live} is NOT empty ({len(stray)} files, e.g. "
            f"{stray[0].name}). The stub must never write to the canonical response "
            f"directory, and a live run must not mix with a prior run's data.")


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

    resp_dir = AUDIT / ("responses_stub" if stub else "responses")
    if stub:
        assert_live_response_dir_untouched()
    if not stub and glob.glob(str(resp_dir / "*" / "*.json")):
        print(f"\n  ABORT — {resp_dir.name}/ already holds responses. Refusing to overwrite "
              f"or mix a prior run's data; move it aside first.")
        return 1
    shutil.rmtree(resp_dir, ignore_errors=True)
    resp_dir.mkdir(parents=True, exist_ok=True)

    transport = stub_transport if stub else live_transport
    results = []
    for p in payloads:
        raw = transport(p["payload"], p["model"], p["item_id"], p["k"])
        # PERSIST FIRST, verify, and only then let the next call issue. Nothing is parsed,
        # aggregated or resolved ahead of the write — the 2026-08-11 loss was labels
        # computed in memory from text that was never written down.
        write_response(resp_dir, p["leg"], p["item_id"], p["k"], raw)
        results.append({**{k: p[k] for k in ("leg", "item_id", "k")}, "answer": raw})
    # Count from DISK, never from len(results). Reporting the in-memory count here would be
    # a claim about persistence sourced from the thing that is not persistence — the same
    # substitution that let the 2026-08-11 run report its totals while writing nothing.
    on_disk = len(glob.glob(str(resp_dir / "*" / "*.json")))
    print(f"\n  persisted {on_disk} raw response file(s) to {resp_dir.name}/ "
          f"(counted on disk; written and verified per call, before the next call issued)")

    gate_responses(resp_dir, expected, errors)
    if errors:
        kept = len(glob.glob(str(resp_dir / "*" / "*.json")))
        print(f"\n  {len(errors)} OUTPUT-GATE failure(s); {kept} response file(s) on disk"
              + (" — recoverable\n" if kept else " — NOTHING WAS RETAINED\n"))
        for e in errors:
            print(f"    {e}")
        return 1

    # Resolution reads back from the PERSISTED files, not from the in-memory list, so the
    # aggregate cannot succeed over data that failed to land.
    results = [json.loads(open(f).read()) | {"answer": json.loads(open(f).read())["raw"]}
               for f in sorted(glob.glob(str(resp_dir / "*" / "*.json")))]
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
        # NOTE (2026-08-11): this flag used to print exactly this line while the driver had
        # no response writer in EITHER mode. Both the message and the --help text were true
        # sentences describing a distinction the code did not implement — documentation of
        # an intended architecture wearing a switch's clothes. The writer now exists, and
        # --dry-run means what it says: assembly, gates and capture run; nothing is sent.
        print(f"\n  --dry-run: no transport was invoked; no responses were requested.")
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

    # output-side capture. ADDED 2026-08-11, after a live run spent 219 calls and persisted
    # nothing. Every gate in this driver was an INPUT gate; the output side had no writer,
    # so there was nothing to gate and no signal that anything was missing.
    print("\noutput capture — gate 4 must fail on each way a response can be lost:")

    def responses_under(build) -> list:
        """Build a response tree, run gate 4 over it, return its error list."""
        d = pathlib.Path(tempfile.mkdtemp())
        try:
            build(d)
            errs: list = []
            gate_responses(d, 3, errs)
            return errs
        finally:
            shutil.rmtree(d, ignore_errors=True)

    def resp(d, leg, item, k, raw, blank=False):
        (d / leg).mkdir(parents=True, exist_ok=True)
        f = d / leg / f"{item}__k{k}.json"
        f.write_text("" if blank else json.dumps(
            {"leg": leg, "item_id": item, "k": k, "raw": raw}) + "\n")

    def complete(d):
        for k in (1, 2, 3):
            resp(d, "direction_i", "i-01", k, "P4")

    check("REFUSES: a response file is MISSING (2 of 3 present)",
          bool(responses_under(lambda d: [resp(d, "direction_i", "i-01", k, "P4")
                                          for k in (1, 2)])))
    check("REFUSES: all files present but one is ZERO-BYTE — count alone would pass",
          bool(responses_under(lambda d: (complete(d),
                                          resp(d, "direction_i", "i-01", 3, "", blank=True)))))
    check("REFUSES: a response outside the leg's fixed vocabulary",
          bool(responses_under(lambda d: (complete(d),
                                          resp(d, "direction_i", "i-01", 3, "Pattern Four")))))
    check("REFUSES: a file that exists and is non-empty but does not parse",
          bool(responses_under(lambda d: (complete(d),
                                          (d / "direction_i" / "i-01__k3.json")
                                          .write_text("not json")))))
    check("CONVERSE — a complete, in-vocabulary capture PASSES (gate not stuck closed)",
          not responses_under(complete))
    def write_response_lands() -> bool:
        d = pathlib.Path(tempfile.mkdtemp())
        try:
            p = write_response(d, "direction_i", "i-99", 1, "P1")
            return p.exists() and p.stat().st_size > 0 and json.loads(p.read_text())["raw"] == "P1"
        finally:
            shutil.rmtree(d, ignore_errors=True)

    def dirty_response_dir_refuses() -> bool:
        d = pathlib.Path(tempfile.mkdtemp())
        try:
            (d / "direction_i").mkdir(parents=True)
            (d / "direction_i" / "x__k1.json").write_text("{}")
            assert_live_response_dir_untouched(d)
            return False
        except SystemExit:
            return True
        finally:
            shutil.rmtree(d, ignore_errors=True)

    def clean_response_dir_allowed() -> bool:
        d = pathlib.Path(tempfile.mkdtemp())
        try:
            assert_live_response_dir_untouched(d)
            return True
        except SystemExit:
            return False
        finally:
            shutil.rmtree(d, ignore_errors=True)

    check("write_response() persists the raw text and verifies it landed", write_response_lands())
    check("the stub NEVER writes to canonical responses/ — refusal fires on a dirty dir",
          dirty_response_dir_refuses())
    check("CONVERSE — an empty response dir is allowed (not stuck closed)",
          clean_response_dir_allowed())

    # live-path refusal. REWRITTEN 2026-08-11, before the live run, on an operator ruling.
    #
    # What was here asserted `bool(unbuilt)` under the label "--live refuses while a leg is
    # unbuilt". It never called assert_spend_go(). So it passed for its entire life because
    # legs happened to be unbuilt — an ambient precondition, not a verified refusal — and it
    # inverted to permanently-RED at exactly the moment every leg was built, which is the
    # moment the system becomes ready to run. A check that could not PASS, wearing the label
    # of a test never performed: the mirror of the vacuous checks that cannot FAIL.
    #
    # Beside it, a second one: `stamped` was computed and then discarded, with `True` passed
    # as the condition. It could not fail either.
    #
    # The general shape, worth more than either instance: a gate that only fires under a
    # condition the system has never reached is UNTESTED BY CONSTRUCTION until the system
    # reaches it. assert_spend_go()'s fail-closed path existed unexercised through this whole
    # arc and was first exercised on the day it was first needed. A vacuity audit therefore
    # has to cover checks whose PRECONDITIONS have never been satisfied, not only checks
    # whose logic cannot fail.
    #
    # The replacement constructs each bad state in a throwaway AUDIT root and calls the real
    # assert_spend_go(), catching its SystemExit. Two-sided: four refusals AND the converse.
    print("\nlive-path refusal — assert_spend_go() called against constructed states:")
    real_audit = globals()["AUDIT"]
    body = "frozen prereg body"
    want = md5(body)

    def spend_go_under(state) -> str:
        """Return 'refused' | 'passed', with AUDIT pointed at a throwaway root."""
        d = pathlib.Path(tempfile.mkdtemp())
        (d / "packets" / "run").mkdir(parents=True)
        state(d)
        globals()["AUDIT"] = d
        try:
            assert_spend_go()
            return "passed"
        except SystemExit:
            return "refused"
        finally:
            globals()["AUDIT"] = real_audit
            shutil.rmtree(d, ignore_errors=True)

    def all_legs(d):
        for stem, *_r in LEGS.values():
            (d / "packets" / "run" / f"{stem}.json").write_text("{}")

    def prereg_at(d, log_text):
        all_legs(d)
        (d / "PREREGISTRATION.md").write_text(body)
        (d / "audit_log.md").write_text(log_text)

    for label, state in [
        ("a leg unbuilt", lambda d: None),
        ("legs built but no PREREGISTRATION.md", all_legs),
        ("prereg md5 absent from audit_log.md", lambda d: prereg_at(d, "no stamp here")),
        ("md5 stamped but NO sentinel", lambda d: prereg_at(d, f"stamp {want}")),
        ("md5 stamped BELOW the sentinel", lambda d: prereg_at(d, f"{SENTINEL}\nstamp {want}")),
    ]:
        check(f"REFUSES: {label}", spend_go_under(state) == "refused")
    check("CONVERSE — a correctly frozen state is ALLOWED (the gate is not stuck closed)",
          spend_go_under(lambda d: prereg_at(d, f"stamp {want}\n{SENTINEL}\n")) == "passed")

    prereg, log = AUDIT / "PREREGISTRATION.md", AUDIT / "audit_log.md"
    stamped = prereg.exists() and md5(prereg.read_text()) in (
        log.read_text() if log.exists() else "")
    check("THIS repository's current prereg md5 is stamped in audit_log.md", stamped)

    print(f"\n{'GREEN — every driver gate discriminates' if ok else 'RED — a gate cannot fail'}")
    return 0 if ok else 1


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--stub", action="store_true", help="stubbed transport (no network)")
    ap.add_argument("--live", action="store_true",
                    help="real transport; refuses unless the prereg md5 is frozen above "
                         "the first result line")
    ap.add_argument("--dry-run", action="store_true",
                    help="assemble, gate and dump payloads; do not call the transport")
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
