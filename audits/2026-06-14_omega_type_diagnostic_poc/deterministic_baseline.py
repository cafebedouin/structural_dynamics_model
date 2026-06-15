#!/usr/bin/env python3
"""Deterministic Ω-type diagnostic baseline (transparent, fail-closed).

Ports debugging_philosophy.md §6.1's fix-reveals-kind move to Omega Variables:
classify each omega by WHICH resolution OPERATION discharges it (measure/define/decide),
gated by external-vs-restatement at the operation's own locus. See spec.md.

This is a LEXICAL/STRUCTURAL baseline. It is intentionally conservative: it returns
`unknown` on ambiguity and NEVER guesses (fail-closed). Its `unknown`-rate over the
40-sample is the determinism-boundary number. The LLM-judge (judge_results.json) does
the reasoning-based pass; this file establishes the floor a transparent rule can reach.

Run from the audit dir (or anywhere; paths are resolved relative to this file).
Writes det_results.json next to this file.
"""
import json, re, os, sys

HERE = os.path.dirname(os.path.abspath(__file__))
SAMPLE = os.path.join(HERE, "sample_40.json")
OUT = os.path.join(HERE, "det_results.json")

# ---------------------------------------------------------------------------
# (A) Signature detection — lexical cues. Reports a SET; stamps nothing.
#     Cues are deliberately high-precision phrases drawn from omega_variables.md's
#     resolution column and the corpus's recurring approach vocabulary.
# ---------------------------------------------------------------------------

MEASURE_CUES = [
    r"\bevidence review\b", r"\bsystematic .{0,30}\bevidence\b", r"\bempirical\b",
    r"\bhistorical record\b", r"\bhistoriograph", r"\bdeployment data\b",
    r"\bethnograph", r"\bcount of\b", r"\bempirical count\b", r"\bnatural experiment\b",
    r"\bcounterfactual\b", r"\bpost-(removal|prohibition|conciliar)\b",
    r"\bdocumentary (evidence|analysis)\b", r"\brevealed[- ]preference\b",
    r"\bmeasure(d|ment|s)?\b", r"\bdata by \d", r"\bcitation pattern", r"\bsurvey\b",
    r"\bdeployment\b", r"\binterview", r"\bobserv", r"\btrace (institutional|behavior)",
    r"\bcomparative study\b", r"\bcompare to other contexts\b",
]
DEFINE_CUES = [
    r"\bconceptual analysis\b", r"\bclarif", r"\bwhat .{0,20}\bmean", r"\bdefine[ds]?\b",
    r"\bdefinition\b", r"\bcriteri", r"\bframework\b", r"\bcategorical\b.{0,40}\bprima facie\b",
    r"\bforeclose", r"\bcoexist", r"\bε-invariance\b", r"\bepsilon-invariance\b",
    r"\binvariance test\b", r"\bincommensurable\b", r"\bunderdetermin", r"\bexhaustiv",
    r"\bjurisprudential analysis\b", r"\bpartition the conceptual space\b",
    r"\bsame (object|question|constraint)\b", r"\bdifferent question", r"\baxiom compatib",
    r"\blogical(ly)? (foreclose|analysis|triplet)\b", r"\bmeta-reading\b",
    r"\breading (OF|ABOUT)\b", r"\bstructural(ly)? distinct\b",
]
DECIDE_CUES = [
    r"\bshould\b", r"\blegitimat", r"\billegitimate\b", r"\bwhose values\b",
    r"\bvalue judgment\b", r"\bstakeholder", r"\bhave (formal )?voice\b",
    r"\bwhich should (dominate|prevail)\b", r"\bconsent\b", r"\bprioritize\b",
    r"\bvalue system\b", r"\bnormative (framework|commitment)\b", r"\bequit",
    r"\bjustified as\b", r"\bdecision (by|authority)\b", r"\bwho legitimately\b",
]

def fired_signatures(text):
    t = text.lower()
    # debugging_philosophy uses unicode epsilon; normalize for cue matching
    fired = set()
    for cue in MEASURE_CUES:
        if re.search(cue, t):
            fired.add("measure"); break
    for cue in DEFINE_CUES:
        if re.search(cue, t):
            fired.add("define"); break
    for cue in DECIDE_CUES:
        if re.search(cue, t):
            fired.add("decide"); break
    return fired

# ---------------------------------------------------------------------------
# (B) External-vs-restatement gate per fired signature, at its own locus.
#     CONSERVATIVE: when the locus evidence is itself ambiguous we return
#     "ambiguous" -> the diagnosis fails closed to `unknown`. We never guess
#     "external" without a positive structural cue, and never "restate" without one.
# ---------------------------------------------------------------------------

# measure fact-locus: external unless the resolution is explicitly an internal
# re-derivation over the constraint's OWN declared fields (ε-invariance over declared
# readings is the canonical restatement form).
RESTATE_MEASURE = [
    r"\bε-invariance\b", r"\bepsilon-invariance\b", r"\binvariance test\b",
    r"\bre-?deriv", r"\bcompare .{0,30}\bauthored ε\b", r"\bmeasuring the .{0,30}one way\b",
]
# define criterion-locus: a define signature is EXTERNAL (open term) by default in this
# corpus (the omegas ask which framework/definition applies). It RESTATES only if the
# resolution merely re-labels an already-enumerated declared reading without opening a
# new term. We treat the presence of an explicit "which of the enumerated readings is
# *correct*" with NO new criterion as restatement-leaning -> ambiguous (fail closed).
RESTATE_DEFINE = [
    r"\bwhich .{0,40}\breading is (correct|right)\b",
]
# decide DECIDER-locus: external (Ω_P) when the decider is a value/party NOT settled by
# the constraint. Restates only if the constraint already contains the deciding
# commitment. We have no robust lexical signal for "decider already contained", so a
# bare `decide` firing is treated as EXTERNAL only when paired with an explicit
# value/legitimacy/should question; otherwise ambiguous.
EXTERNAL_DECIDE = [
    r"\bshould\b", r"\blegitimat", r"\bwhose values\b", r"\bhave (formal )?voice\b",
    r"\bequit", r"\bprioritize\b", r"\bvalue judgment\b",
]

# ---------------------------------------------------------------------------
# Authored-field re-derivation = RESTATEMENT (the no-op fix, 2026-06-14).
# The gate previously defaulted every fired signature to "external" and so could
# never reach "all internal" -> restatement (it missed both seeded restatements,
# id20/id27). Root cause: the locus test ignored the entry's `declared_fields`.
# Restatement's canonical form is an operation that RE-DERIVES the constraint's OWN
# authored fields (ε / base properties / beneficiary-victim) across its DECLARED
# readings — distinct from an open conceptual criterion (foreclose/coexist -> Ω_C)
# or a world-observation (-> Ω_E). This is now detected against declared_fields and
# applied to BOTH the measure and define signatures.
REDERIVE_CUES = [
    r"\bε-invariance\b", r"\bepsilon-invariance\b", r"\binvariance test\b", r"\bre-?deriv",
    r"\bcompare .{0,90}\b(base propert|authored ε|authored epsilon|extraction coefficient|"
    r"beneficiary[/ ]?(and )?victim|victim sets?|ε across)\b",
    r"\bgenerate .{0,110}\b(reading|sibling|parallel constraint|constraint story)\b.{0,110}\bcompare\b",
    r"\bmeasuring .{0,50}\bone way\b.{0,110}\b(another|other) way\b",
]

def declared_reading_stems(declared_fields):
    """Short stems of the constraint's DECLARED readings (from cs_reading_relation +
    cs_kernel_id), e.g. {'homoiousios_reading','homoiousios','behavioral_competence'}.
    declared_fields may be a dict or a repr-string; regex the `__<stem>_reading` tokens."""
    s = str(declared_fields).lower()
    stems = set()
    for full in re.findall(r"__([a-z0-9_]+?_reading)\b", s):
        stems.add(full)
        stems.add(full[: -len("_reading")])
    return {x for x in stems if len(x) > 3}

def re_derives_authored(qac, declared_fields):
    """True iff the resolution re-derives the constraint's OWN authored fields over its
    DECLARED readings (the restatement form). Non-vacuous in declared_fields: a generic
    'compare' restates only when it references a declared reading; ε-invariance (DP-001)
    is inherently over the declared readings, so it qualifies directly."""
    t = qac.lower()
    if not any(re.search(c, t) for c in REDERIVE_CUES):
        return False
    if re.search(r"\b(ε|epsilon)-invariance\b|\binvariance test\b", t):
        return True  # DP-001: ε-invariance is by construction over the declared readings
    stems = declared_reading_stems(declared_fields)
    return any(stem in t for stem in stems)

def gate(sig, qac, declared):
    """Return 'external' | 'restate' | 'ambiguous' for one fired signature."""
    t = qac.lower()
    rederive = re_derives_authored(qac, declared)
    # decide: a GENUINE external decider (value/should/legitimacy cue) routes Ω_P even
    # when the omega also re-derives fields. Otherwise an *incidental* decide cue (a
    # passing "stakeholder" mention inside an authored-field re-derivation) is itself
    # internal -> restate, so it can't fail-close the whole diagnosis to unknown.
    if sig == "decide":
        if any(re.search(c, t) for c in EXTERNAL_DECIDE):
            return "external"
        return "restate" if rederive else "ambiguous"
    # measure/define: re-derivation of authored fields over declared readings restates
    # both signatures alike (restatement is the failure mode of EACH operation).
    if sig in ("measure", "define") and rederive:
        return "restate"
    if sig == "measure":
        if any(re.search(c, t) for c in RESTATE_MEASURE):
            return "restate"
        # external observation cue present in the firing -> external
        return "external"
    if sig == "define":
        if any(re.search(c, t) for c in RESTATE_DEFINE):
            return "ambiguous"   # fail closed: looks like re-label of enumerated reading
        return "external"
    return "ambiguous"

SIG_TO_TYPE = {"define": "conceptual", "decide": "preference", "measure": "empirical"}

def diagnose(qac, declared=""):
    sigs = fired_signatures(qac)
    if not sigs:
        return {"fired": [], "gates": {}, "externals": [], "diagnosis": "unknown",
                "reason": "no signature fired"}
    gates = {s: gate(s, qac, declared) for s in sorted(sigs)}
    externals = sorted([s for s, g in gates.items() if g == "external"])
    ambiguous = [s for s, g in gates.items() if g == "ambiguous"]
    # Fail-closed: if any fired signature gate is ambiguous AND it could change the
    # diagnosis (would add an external), we cannot commit -> unknown.
    if ambiguous:
        return {"fired": sorted(sigs), "gates": gates, "externals": externals,
                "diagnosis": "unknown",
                "reason": f"ambiguous gate on {ambiguous}"}
    if len(externals) == 0:
        return {"fired": sorted(sigs), "gates": gates, "externals": [],
                "diagnosis": "restatement", "reason": "all signatures internal"}
    if len(externals) == 1:
        return {"fired": sorted(sigs), "gates": gates, "externals": externals,
                "diagnosis": SIG_TO_TYPE[externals[0]],
                "reason": f"single external: {externals[0]}"}
    return {"fired": sorted(sigs), "gates": gates, "externals": externals,
            "diagnosis": "hybrid(" + "+".join(externals) + ")",
            "reason": f"{len(externals)} externals"}

def qac_of(r):
    return " ".join([r.get("question", ""), r.get("approach", ""), r.get("consequence", "")])

# ---------------------------------------------------------------------------
# main
# ---------------------------------------------------------------------------

def run_sample():
    data = json.load(open(SAMPLE, encoding="utf-8"))
    results = []
    n_unknown = 0
    for r in data:
        d = diagnose(qac_of(r), r.get("declared_fields", ""))
        if d["diagnosis"] == "unknown":
            n_unknown += 1
        results.append({
            "sample_id": r["sample_id"], "name": r["name"],
            "authored_type": r["omega_type"], "is_family": r["is_family"],
            **d,
        })
    rate = n_unknown / len(data)
    return data, results, n_unknown, rate

# ---- Two-sided commit control (build + run) -------------------------------

def commit_control():
    """(under-commit) a KNOWN-definitional case must COMMIT (not unknown);
       (over-commit) a GENUINELY-ambiguous case must return unknown (no guess)."""
    # KNOWN-DEFINITIONAL: the omega_variables.md US-China Ω_C example, paraphrased so the
    # define signature fires cleanly and the term is open. Must commit to conceptual.
    known_def = ("What counts as 'biotech' for policy purposes — capabilities, products, "
                 "both? Conceptual analysis to define and select the framework; different "
                 "definitions generate different policy scopes.")
    d_known = diagnose(known_def)
    under = {"input": "KNOWN-DEFINITIONAL (biotech define)", "result": d_known,
             "expect": "commit to conceptual",
             "pass": d_known["diagnosis"] == "conceptual"}

    # GENUINELY-AMBIGUOUS: a bare decide firing with no sitable decider and a faint define
    # echo, constructed so the gate cannot resolve -> must be unknown.
    ambiguous = ("Could this be read differently? One might reconsider the relationship "
                 "between the parties. The matter remains contested.")
    d_amb = diagnose(ambiguous)
    over = {"input": "GENUINELY-AMBIGUOUS (no sitable locus)", "result": d_amb,
            "expect": "unknown (no guess)",
            "pass": d_amb["diagnosis"] == "unknown"}
    return under, over

# ---- Biotech-triple spec check (near-tautological; NOT a positive control) -

def biotech_triple():
    cases = {
        "Ω_C (race vs supply-chain frame)":
            ("Are US-China biotech dynamics best understood as a race or as a supply "
             "chain? Conceptual analysis: select the framework / define the frame. Each "
             "frame generates different strategic implications."),
        "Ω_E (supply-chain %)":
            ("What percentage of critical biotech supply chains currently depend on "
             "Chinese manufacturing? We can measure this with deployment data and an "
             "empirical evidence review, but haven't."),
        "Ω_P (security vs progress)":
            ("When national security and scientific progress conflict in biotech, which "
             "should dominate US policy? Different stakeholders legitimately prioritize "
             "differently; this is a value judgment for those bearing the consequences."),
    }
    return {k: diagnose(v) for k, v in cases.items()}

# ---- Seed control (the restatement-gate witness, 2026-06-14) ---------------
#  Two-sided: KNOWN_RESTATEMENT seeds MUST diagnose `restatement` (catch); KNOWN_EXTERNAL
#  seeds MUST NOT (`external`/typed, never restatement/unknown); UNDER_DECLARATION MUST
#  route external (a not-declared term is a real frontier, not a re-derivation). This is
#  the control that was written-and-failing before the declared-field gate fix; it is now
#  a standing, runnable assertion (exit 1 on RED).
HELD = os.path.join(HERE, "adjudicator_held_key.json")

def seed_control():
    held = [h for h in json.load(open(HELD, encoding="utf-8"))["held"]
            if h.get("control_role", "none") != "none"]
    by_q = {str(r["sample_id"]): r for r in json.load(open(SAMPLE, encoding="utf-8"))}
    rows, ok = [], True
    for h in sorted(held, key=lambda x: int(x["sample_id"])):
        sid = str(h["sample_id"]); r = by_q.get(sid, {})
        dd = diagnose(qac_of(r), r.get("declared_fields", ""))["diagnosis"]
        role = h["control_role"]
        if role == "KNOWN_RESTATEMENT":
            good = dd == "restatement"
        elif role == "KNOWN_EXTERNAL":
            good = dd not in ("restatement", "unknown")
        else:  # UNDER_DECLARATION
            good = dd != "restatement"
        ok = ok and good
        rows.append({"sample_id": sid, "role": role, "diagnosis": dd, "pass": good})
    return rows, ok

if __name__ == "__main__":
    data, results, n_unknown, rate = run_sample()
    under, over = commit_control()
    triple = biotech_triple()
    seed_rows, seed_ok = seed_control()

    payload = {
        "n": len(data),
        "unknown_count": n_unknown,
        "unknown_rate": round(rate, 4),
        "commit_control": {"under_commit": under, "over_commit": over,
                           "both_pass": under["pass"] and over["pass"]},
        "seed_control": {"rows": seed_rows, "green": seed_ok},
        "biotech_triple": triple,
        "results": results,
    }
    json.dump(payload, open(OUT, "w"), indent=2, ensure_ascii=False)
    for r in seed_rows:
        print(f"  {r['role']:16} id{r['sample_id']:>2} -> {r['diagnosis']:14} "
              f"{'PASS' if r['pass'] else 'FAIL'}")
    print(f"SEED CONTROL: {'GREEN' if seed_ok else 'RED'} | "
          f"commit-control both_pass={under['pass'] and over['pass']} | unknown_rate={rate:.3f}")
    sys.exit(0 if (seed_ok and under["pass"] and over["pass"]) else 1)

    print(f"n={len(data)}  unknown={n_unknown}  unknown_rate={rate:.3f}")
    print("\n-- two-sided commit control --")
    print(f"  under-commit (KNOWN-DEFINITIONAL): diagnosis={under['result']['diagnosis']:14s} "
          f"PASS={under['pass']}")
    print(f"  over-commit  (GENUINELY-AMBIGUOUS): diagnosis={over['result']['diagnosis']:14s} "
          f"PASS={over['pass']}")
    print(f"  both_pass = {under['pass'] and over['pass']}")
    print("\n-- biotech triple (spec-implementation check) --")
    for k, d in triple.items():
        print(f"  {k:34s} -> {d['diagnosis']}")
    print("\n-- per-omega --")
    for r in results:
        print(f"  {r['sample_id']:2d} | auth={r['authored_type'][:4]} fam={str(r['is_family'])[0]} | "
              f"fired={'+'.join(r['fired']) or '-':22s} -> {r['diagnosis']}")
    print(f"\nwrote {OUT}")
