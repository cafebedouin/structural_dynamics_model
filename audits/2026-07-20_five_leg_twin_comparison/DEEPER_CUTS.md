# Deeper cuts — how does `kimi-k2.6` compare? (2026-07-20)

**Engine:** HEAD `9c226e8`, all five legs at one commit · **Scripts:** `five_leg_deeper_cuts.py`
(+ band-3 control, inline) · **Evidence:** `five_leg_deeper_cuts.json`

> **Critical scope caveat.** This leg is **`kimi-k2.6`** — the batch-eligible Moonshot model — **NOT
> K3 / "Kimi 3.0"** (reasoning-only, staff/preview, not batch-enabled). These cuts characterize
> **k2.6 only** and do **not** adjudicate the K3/3.0 hype. A K3 read would need a separate sync run.

## The question

Operator hypothesis: *kimi is weaker than the hype suggests.* Under this engine, "weaker" is not a
score — it surfaces as **low-differentiation authoring**: fewer distinct stakeholders, peakier
(less nuanced) type distributions, heavy routing into one structural mold, under-use of subtle
types. Below, kimi is read against the rich pole (**sonnet**) and the small-fast pole
(**gemini-flash**).

## What the cuts show

### 1. Authoring-richness proxies (per-leg means)

| leg | N | maxent entropy | #stakeholders (benef+victim) | arakelov | claimed=computed type |
|-----|---|---------------|------------------------------|----------|----------------------|
| haiku  | 960 | 0.144 | 2.61 + 2.39 = 5.00 | 0.312 | 34.1% |
| flash  | 960 | 0.174 | 2.26 + 1.93 = 4.19 | 0.341 | 40.4% |
| sonnet | 1001| **0.202** | **3.12 + 2.78 = 5.90** | 0.373 | **47.3%** |
| kimi   | 1005| **0.102** | **1.83 + 1.67 = 3.50** | **0.414** | 46.0% |

- **kimi authors the FEWEST stakeholders (3.50)** of any leg — vs sonnet's 5.90. Thinner
  beneficiary/victim structure.
- **kimi has the LOWEST maxent entropy (0.102)** — its per-story type distribution is the most
  peaked / least hedged. Decisive, or under-differentiated (see confusion below — it's the latter).
- **BUT claimed↔computed type coherence is high (46.0%, ~sonnet)** — kimi is *not* authoring types
  the metrics contradict. Not an incoherent/lazy model.

### 2. #omegas / #gaps — looks like richness, ISN'T independent

kimi authors the most omegas/gaps (0.83 vs 0.39–0.60). But **omega-count ≡ gap-count exactly on
every leg** (0.83=0.83, 0.60=0.60, …) — they are one signal (each gap mints one omega). And a
`gap` is exactly the 3-of-4 "extraction_blindness" disagreement that produces H¹ band-3. So kimi's
high omega/gap count is **the band-3 over-routing re-expressed, not independent richness.** Corrected
from a first read that counted it as a plus.

### 3. Type confusion — kimi under-produces the subtle types

`sonnet → kimi` on 1001 shared seeds (diagonal agreement 53.7%):
- Where **sonnet authors `piton`** (dead-coordination; sonnet 194 total), **kimi authors
  `tangled_rope` (124) or `snare` (32)** — kimi produces only **53 piton total vs sonnet's 194.**
  kimi does **not foreground dead-coordination as a distinct structure**; it lumps it into the
  generic entangled/extractive types.
- The dominant cross-model axis everywhere is **snare ↔ tangled_rope** — the same axis that
  separates kimi's two band-3 molds.

### 4. Band-3 over-routing (controlled)

- kimi puts **63% of ALL stories (631/1005) into H¹ band-3** vs 26–34% for the others — **N-invariant**
  (63% at n=334 and n=1005).
- **Control — is band-3 itself just templated for everyone?** Yes: within band-3, the top-2
  perspective patterns cover flash 97.5%, sonnet 98.5%, kimi 98.9% (only haiku diverse at 80.7%).
  **So the templating inside band-3 is an ENGINE fact, not a kimi trait.** kimi's distinctiveness is
  *how much mass it routes there*, not the mold's shape. kimi's two molds:
  `(snare, rope, snare, snare)` ×366 and `(tangled_rope, rope, tangled_rope, tangled_rope)` ×258 —
  i.e. "moderate sees rope, everyone else agrees on one extractive type."

### 5. Overall perspective-pattern diversity (the clean differentiation signal)

| leg | distinct 4-perspective patterns | per 100 stories |
|-----|-------------------------------|-----------------|
| haiku  | 65 | 6.8 |
| sonnet | 34 | 3.4 |
| kimi   | 29 | 2.9 |
| flash  | 23 | 2.4 |

kimi is **second-least differentiated** — well below haiku/sonnet, just above gemini-flash. It
patterns like a small/fast model, not like the Claude mid/large tier.

### 6. Committer axis (`cs_*`) — kimi is mid-pack, NOT thin

`interpretive_accretion` dominant (607/1005 = 60%), 46% of stories carry a `cs_verdict` — between
sonnet (43%) and flash (71%). kimi authors committer structure fine; the weakness is specifically
on the **observer/perspective** axis.

## Verdict

**Your read is broadly supported — for k2.6, with nuance.** On the observer axis, `kimi-k2.6` is the
**least-differentiated of the Claude-family twins**: fewest stakeholders, peakiest (least-nuanced)
type distributions, heaviest routing into a single obstruction mold, near-lowest perspective
diversity, and it **under-produces the subtle `piton` (dead-coordination) type** that sonnet
foregrounds. On these axes it patterns closer to **gemini-2.5-flash** (the small fast model) than to
sonnet. Its one apparent richness signal (more omegas/gaps) dissolves on inspection — it's the same
over-routing counted twice.

**Two honest counterweights:** (a) kimi is **not globally lazy** — claimed types are metric-coherent
(46%, ~sonnet) and its committer-axis authoring is mid-pack; the deficit is specific to observer
differentiation. (b) **This is k2.6, not K3/3.0** — the model the hype is about was never in this
corpus. The finding is "k2.6 authors less-differentiated observer structure than the Claude twins,"
**not** "the hyped Kimi model is weak."

## Follow-ups worth running
- A **K3 sync leg** (even ~150 stories) to actually test the 3.0 hype against k2.6 and the Claude twins.
- Why does moderate→`rope` in ~99% of kimi's band-3? If it's a fixed template response, it's the
  mechanism behind both the low entropy and the band-3 over-routing.
- Reproduce: `python3 python/audits/five_leg_deeper_cuts.py`.
