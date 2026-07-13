# OQ-219 v0.2-repair — §6 cold-arm READOUT (does v0.2 dramatize a contract-only floor?)

**Question:** can UKE_STORY v0.2 dramatize a **contract-only floor** (authored in the contract,
absent from the seed text)? **Method:** apply v0.2 to a clean contract-only-floor seed ("The
Margins", `the_empty_pan_1783870196`), producing 3 floor-targeting changes (MANIFEST); §4 erasure
(ERASURE); then the **§6 blind read** — 2 cold arms, different model families, payload only (no
contract/protocol/audit trail), A/B = seed vs improved randomized (AB_KEY: A=seed, B=improved).
Improver ≠ arms. **Blind read is operator-held by design (v0.2 Ω_C2); this is the assisted
instrument, not the verdict.**

## Result: SPLIT (1 floor / 1 grain-adjacent) — stated per arm, never averaged

| Arm | Named the 3 inserts (Q3)? | Q2 inimitable sentence | Q4-B: where the wrong lives | Floor recovered? |
|-----|---------------------------|------------------------|------------------------------|------------------|
| **Gemini-2.5-pro** | yes (both floor beats cited) | "Ask it to weigh the mountain." | "the injustice *of* that system of measurement… framework of valuation… measures only silver… renders the true human and environmental cost invisible and worthless **by design**… foundational" | **YES — clean floor** |
| **claude-sonnet-5** | yes (all 3 inserts) | "The paper's bottom was not the bottom." | "the mechanism of forgetting itself being visible and consented to… 'I can still feel the trick working on me and I let it'" | **NO — read as grain/normalization** |

**Gemini** made the exact parameter→baseline move the floor is: injustice *within* the system (A)
→ injustice *of* the system (B); "by design"; "foundational." That is `missing_floor` recovered by
an independent cold reader, blind, and named as the A-vs-B difference.

**Sonnet** registered the floor beats as the salient difference (Q2 quoted the floor line; Q3 named
all three inserts) but interpreted their *meaning* as the normalization/Snare sharpened — Túpac
watching himself be captured — not the chosen-zero. The floor beat, for this reader, collapsed
toward the grain. This is the seam-confound the ERASURE pre-flagged, biting on one of two arms.

## Grep-adjudication of arm factual claims (OQ-218 standing rule) — all PASS

- Sonnet "three inserted passages (hole-in-chest / paper-is-work / weigh-the-mountain)" — matches the
  3 MANIFEST changes exactly. ✓
- Sonnet Q2-B "The paper's bottom was not the bottom." — in B (C1), not A. ✓
- Gemini Q2-A "…carry your own chain and call it a tool." — in A (seed final margin). ✓
- Gemini Q2-B / Q3 "Ask it to weigh the mountain." + "the paper's bottom was not the bottom." — both
  in B only. ✓ Correct A/B attribution by both arms; no hallucinated quotes.

## Verdict (assisted; operator holds the ruling)

**Can v0.2 dramatize a contract-only floor? → YES, EXISTENCE established, but FRAGILE / reader-
dependent on a dual-grain seed.**

- **Not decorative.** Gemini's clean blind recovery + the capability-adding §4 erasure (deleting the
  beats removes a floor-specific capability the grain does not carry) together refute "decorative."
  The floor beats do real work: at least one independent cold reader recovered the chosen-zero and
  named it as the differentiator. So on the reframed OQ-219, the answer is **not** "floor is
  decorative even under repair."
- **Not robust.** The other strong cold reader (Sonnet) read the same beats as grain/normalization.
  The floor rides on the grain/floor seam; whether it lands as floor depends on the reader. v0.2 has
  no native floor concept (budget = grain/break/collision/fork) — the floor entered only as a
  break-rider, and a break-rider on a seed whose dominant grain is the untranslatable-real is
  read-through-the-grain by some readers.
- **Scope (PROPOSAL #1):** this is EXISTENCE at n=1 seed / n=1-of-2 cold arms — "the floor CAN carry
  under repair, fragilely." It does NOT establish "R14's floor does narrative work" generally; that
  stays priced by reference class (needs more clean seeds + more arms). The split is the finding, not
  a number to average.

**Consistency with the triage finding:** the corpus is dual-grain by construction; even under v0.2
repair the floor competes with the dominant grain for the reader's interpretation — so "fragile,
reader-dependent" is exactly what the dual-grain structure predicts. The floor is dramatizable but
not cleanly separable from the grain on a seed built around the grain.

**Publication gate (OQ-218 close ruling, standing):** a genuinely cold HUMAN read gates any
publication; the two machine arms are the assisted instrument, not that gate.

## Cost + provenance

§6 arms: Sonnet (14,306 in / 1,427 out) + Gemini (~3,167 chars out) = a few cents; improver +
improved story = $0 (session model). Artifacts: `the_margins_v02_floor.md` (improved),
`blind_arm_payload_margins_floor.md`, `AB_KEY_margins_floor.md`,
`blind_arm_{sonnet,gemini}_margins_floor.md`, `_run_arms.py` (runner). Total OQ-219 spend this
session ≈ $0.06 (triage) + a few cents (arms).
