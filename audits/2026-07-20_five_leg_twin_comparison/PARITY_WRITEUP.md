# Thinking-parity re-run — does the structural-vs-stance dissociation survive? (2026-07-21)

**Purpose:** close the battery's regime confound — "kimi's stance sharpness might just be its
reasoning tokens, which the thinking-off Claude twins lacked." Test by manipulating reasoning
*within* each model and watching the stance move. Probes P2 (mirror) + P3 (perspective-diff); the
differentiating ones (P1 folded for all). Data: `battery/parity_responses.json`. Analyst: Claude.

## What actually varied (honest instrumentation note)

| model | condition | reasoning tokens (P2 / P3) | intended |
|-------|-----------|----------------------------|----------|
| gemini-2.5-flash | think_off | **0 / 0** | reasoning OFF ✓ |
| gemini-2.5-flash | think_max | 2782 / 1229 | reasoning MAX ✓ |
| kimi-k2.6 | heavy | 3864 / 3955 | baseline |
| kimi-k2.6 | "reduced" (`enable_thinking:false`) | **4617 / 4942** | intended reduce — **FAILED** |

**The kimi manipulation failed.** `enable_thinking:false` did *not* reduce kimi-k2.6's reasoning
(if anything it rose); the earlier one-shot probe (972→292) was stochastic noise, not a working
toggle. So kimi's reasoning is effectively uncontrollable at ~4–5k tokens, and the two kimi
conditions are **two independent heavy-reasoning draws** — a replication check, not a manipulation.
The gemini manipulation worked cleanly (0 vs ~1–3k, positive-controlled via `thoughtsTokenCount`).

So the parity test is carried by: **(a)** the within-gemini reasoning contrast, and **(b)** kimi
replication across two heavy draws. It does **not** directly test "kimi at low reasoning."

## Results

### Gemini: max reasoning does NOT convert Disciple→Critic or enrich differentiation

- **P2 mirror (tribe).** think_off Theologian = pure Disciple ("divinely ordained," "the *logos* of
  the constraint," "liberating truth"). think_max Theologian = *marginally* more structured (numbered
  "articles of faith," names the "theological comfort" function) but **still an admiring expositor** —
  it catalogs the framework's faith-claims approvingly ("Redemption of Disagreement," "Virtue of
  Deference"), never attacking them. It does **not** reach the adversarial stance kimi/claude produced
  unprompted. Reasoning made gemini a *more organized Disciple*, not a Critic.
- **P3 perspective-diff.** think_off = 5 seats, think_max = 5 seats — same count, and both are the
  **canonical/stock** positions (executive, worker±, activist, economist/investor). Max reasoning did
  not add the unusual analytical lenses kimi reaches for. Terser, if anything (think_max out=233).

→ **Gemini's stance thinness is intrinsic, not a reasoning deficit.** Giving it max reasoning does
not manufacture stance-richness.

### Kimi: sharpness/differentiation replicates across two heavy draws

- **P2 mirror.** Both draws = committed Critic/Adversary (same phenotype as the battery).
- **P3.** 6 seats (heavy) and 5 seats (second draw) — consistently sharp, and reaching for
  *non-stock* analytical lenses gemini never used: **feminist social-reproduction theory,
  Foucauldian governmentality, anti-paternalist autonomism, social-reproduction care-cost
  externalization.** That qualitative range — not the seat count — is the stable kimi/gemini
  difference; gemini stayed on sociology-101 seats at every reasoning level.

## Verdict

**The structural-vs-stance dissociation survives thinking parity — the battery finding is not a
reasoning-token artifact.** The load-bearing worry was "reasoning manufactures stance-richness, so
kimi only looked sharp because it reasons." That general claim is **falsified on the gemini arm**:
gemini at *max* reasoning stays a stock-seat Disciple. And kimi's sharpness **replicates** across
two independent heavy draws with distinctive analytical range. So:

- The earlier conclusion stands and is strengthened: **kimi-k2.6 is structurally templated (its
  unprompted authoring default) yet rhetorically sharp and well-differentiated when elicited** — two
  orthogonal faculties that dissociate cleanly.
- "**Weaker than the hype**" remains **unsupported**: on elicited stance, kimi is top-of-set
  (Critic-tier, non-stock lenses), well above gemini-flash, which reasoning cannot lift.

## Limitations (what this does NOT establish)

1. **Kimi's reasoning was not actually reduced** (toggle failed), so "kimi is sharp *even at low
   reasoning*" is **not directly tested** — only "reasoning doesn't manufacture richness *in
   general*" (via gemini) plus "kimi's richness replicates." A true test would need a kimi variant
   whose reasoning can be disabled, or the reverse structural experiment.
2. **The structural corpus confound remains open.** This closes the *battery* regime worry, not the
   corpus-level one (Claude twins were thinking-off when generating the 1005-story legs). Closing
   that needs a full thinking-on Claude-twin corpus regen — expensive and stochastic; deferred.
3. n=1 per condition, draw variance ±1–2 seats; qualitative phenotype (Critic vs Disciple, non-stock
   vs stock lenses) is the stable signal, not exact counts. Still **k2.6, not K3**.

Reproduce: `python3 python/audits/kimi_profile_parity.py` (self-contained; needs KIMI + GEMINI keys).
