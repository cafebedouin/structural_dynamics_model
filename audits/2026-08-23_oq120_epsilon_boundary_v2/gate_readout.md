# gate_readout — OQ-120 Phase 0 **v2**, Step C

**Executed:** 2026-08-23 · **OPEN/CLOSE HEAD** `f88c8c3c` (unmoved) · **v2 prereg md5**
`7d0a85d93ae1b9e540ac54d2d4cc4ba7` — **not frozen before results; see the banner in
`PREREGISTRATION.md`.** v1's genuinely-prior prereg (`b181e1a2a9cd42b86d190be09f61d400`) survives
unedited at `audits/2026-08-21_oq120_epsilon_boundary/`.

---

## BRANCH: **G1b — UNCORROBORATED**

> floor met; **1** `snare_epsilon_floor`-DECISIVE transition, from **1** model stratum
> (`claude-haiku-4-5-20251001`) — fails the ≥2-distinct-MODELS criterion.

**Same branch as v1, now reached through a gate that can come out false.** That is the whole point
of the re-run: v1 landed G1b via a subtype whose literal text ("a single leg") didn't fit, against a
floor cleared 900× over, with "attributes to" undefined. v2 lands it through a defined
(decisive) test, an exhaustive subtype set, and a floor that **11 of 23 strata fail**.

## Determinism check — clean

v2's substrate was identical to v1's (18 legs, same counts, HEAD unmoved), so the sweep is a
re-derivation under final code. **All 19 corpora reproduced v1's transition data exactly** — same
count, same (id, seat, ε bracket, MT/FT pair, gate set) on every row, 0 legs differing. This
matters because v1's fork was patched mid-flight (the double-emission fix); the check confirms the
published numbers come from the code that shipped.

## The re-set floor — and the evidence it is not decoration

`N_rail(stratum) ≥ 10` **AND** `rate ≥ 0.5%` of (stories × 4 seats), in ≥2 distinct MODEL strata.
**12 of 23 strata pass; 6 distinct models. FLOOR MET.**

| stratum | stories | N_rail | rate | |
|---|---|---|---|---|
| `claude-sonnet-5\|off\|e03e2210\|685ed7cf` | 2006 | 531 | 6.62% | ✓ |
| `claude-haiku-4-5\|off\|e03e2210\|685ed7cf` | 2444 | 272 | 2.78% | ✓ |
| `stealth/ox-alpha\|on\|e03e2210\|685ed7cf` | 2046 | 231 | 2.82% | ✓ |
| `claude-sonnet-5\|off\|8080348c\|becd0f87` | 1006 | 204 | 5.07% | ✓ |
| `stealth/ox-alpha\|on\|685ed7cf\|685ed7cf` | 969 | 115 | 2.97% | ✓ |
| `nemotron-3-ultra\|off\|685ed7cf\|685ed7cf` | 852 | 113 | 3.32% | ✓ |
| `kimi-k2.6\|on\|e03e2210\|685ed7cf` | 1005 | 102 | 2.54% | ✓ |
| `gemini-2.5-flash\|on\|685ed7cf\|685ed7cf` | 1980 | 101 | 1.28% | ✓ |
| `kimi-k2.6\|on\|8080348c\|becd0f87` | 1005 | 77 | 1.92% | ✓ |
| `claude-haiku-4-5\|unknown\|22843cdf\|2e9dff2f` | 533 | 63 | 2.95% | ✓ |
| `nemotron-3-ultra\|off\|e03e2210\|685ed7cf` | 144 | 17 | 2.95% | ✓ |
| `claude-sonnet-5\|unknown\|685ed7cf\|685ed7cf` | 54 | 12 | 5.56% | ✓ |
| **`gemini-2.5-flash\|off\|685ed7cf\|685ed7cf`** | **1902** | **0** | **0.00%** | ✗ |
| **`gemini-2.5-flash\|off\|22843cdf\|2e9dff2f`** | **765** | **5** | **0.16%** | ✗ |
| `gemini-2.5-flash\|off\|e03e2210\|685ed7cf` | 206 | 0 | 0.00% | ✗ |
| `claude-sonnet-5\|unknown\|8080348c\|becd0f87` | 55 | 7 | 3.18% | ✗ (count) |
| `claude-sonnet-4-5\|unknown\|22843cdf\|2e9dff2f` | 35 | 1 | 0.71% | ✗ |
| `claude-sonnet-5\|unknown\|8080348c\|f1436bd4` | 18 | 1 | 1.39% | ✗ |
| `claude-sonnet-4-5\|unknown\|8080348c\|becd0f87` | 35 | 0 | — | ✗ |
| `claude-sonnet-4\|unknown\|046e0a40\|c6d6880c` | 7 | 0 | — | ✗ |
| `claude-sonnet-4-5\|unknown\|046e0a40\|2e9dff2f` | 5 | 0 | — | ✗ |
| `kimi-k3\|unknown\|8080348c\|becd0f87` | 5 | 0 | — | ✗ |
| `unprovenanced` (kernel_v1) | 1106 | 0 | — | ✗ |

**The strongest evidence the floor discriminates: `gemini-2.5-flash|off` has essentially no
decisive ε transitions at all — 5 across 2,873 stories (0.04% pooled) — while
`gemini-2.5-flash|on` has 101 across 1,980 (1.28%).** A thinking-regime effect of ~30× on this
axis, in the one model where both arms are large. The old pooled `≥10` could never have surfaced
that; it would have counted flash's 5 inside a total of 9,191 and called the floor met.

## Pooled memo line — satisfies nothing on its own

```
N_eps 1919   N_reach 1917   N_rail 1852        (decisive-scored)
qualifying transitions 1919  (+0 unknown-endpoint)
all located live transitions 122031
MT-invariant / FT-only among qualifying 1163 = 60.6%
```

v1's pooled figures were `9351 / 9344 / 9191` under the MOVED reading. **The decisive filter cuts
N_rail ~5×**, which is the size of the difference between "the bit moved" and "the gate decided".

## MOVED vs DECISIVE — both retained

```
snare_epsilon_floor    MOVED 4717    DECISIVE 1
rope_epsilon_ceiling                 DECISIVE 1936
snare-DECISIVE models  ['claude-haiku-4-5-20251001']
FT pair exactly {rope, snare} among snare-DECISIVE: 0
```

The single decisive case is unchanged from v1: `testsets_haiku3`,
`equal_protection_kernel__antisubordination_reading`, analytical seat, ε 0.4599→0.4600,
`tangled_rope → snare`, χ steady at ≈0.680, sole changed gate `snare_epsilon_floor`; **1 of 3**
same-model redraws, on the noisiest floor in OQ-347's table (haiku, 65%). `kernel_v1`: 0 decisive
in 10,215.

## FT pairs among N_rail (decisive-scored)

```
piton -> rope           1188        rope -> piton          19
rope -> naturalized      615        piton -> naturalized   15
rope -> rope              14        tangled_rope -> snare   1
```

`piton → rope` is modal by ~2:1 over `rope → naturalized` — **confirming v1's `:593` finding at
full decisive resolution**, and confirming that the plan's predicted `rope, naturalized`
replacement label is not what the corpus exhibits.

## The backfill split — correct in principle, no verdict moved here

Reported honestly rather than credited with more than it did. Splitting the stratum key on
`(prompt_hash, schema_hash)` is required by OQ-78 ruling 5 (*"not across generation regimes within
one model either"*), and `testsets_haiku` really is 47% re-authored 70 days later under a different
prompt **and** schema. **But the counterfactual says it changed nothing that mattered here:**

- 4-part key: **6** models pass the floor. 2-part key: **the same 6**. No model-level flip.
- Branch identical either way.
- Exactly **two** small cell verdicts depend on the split, both in tiny mixed cells:
  `claude-sonnet-5|unknown|8080348c|becd0f87` (55 stories) and `…|8080348c|f1436bd4` (18) each fail
  on their own and would pass when merged.

So: keep the finer key because ruling 5 requires it and because it makes `gemini-2.5-flash|off`'s
three-way split legible — but **do not cite it as having rescued a conclusion.** It did not.

## Controls

Unchanged from v1 and re-run on all 19 corpora: **C1 PASS ×19** (natural carrier), **C2 PASS ×19**
(transition vanishes at floor 0.90, restore verified), **C3 PASS ×19**, **C4 PASS ×10 /
SKIPPED-precondition ×8 / SKIPPED-carrier-absent ×1, 0 FAIL** — now using the three-way bookkeeping
the plan supplies rather than inventing it at runtime. C1 fired and C2 declined on every corpus, so
the branches are interpretable. The DECISIVE predicate's own two-sided control asserts in-code and
additionally has a naturally-arising positive (the one live case).

## Write-set

`git diff --stat -- outputs/ python/ prolog/` **empty**. Under `[G1-D]` Step D lands no commit, so
it stays empty through Step E.
