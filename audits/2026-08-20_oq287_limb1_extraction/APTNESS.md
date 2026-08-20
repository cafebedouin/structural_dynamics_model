# Aptness notes — every citation in `docs/practice/practice_paper_v0_1.md`

**Why this file exists.** `claim_cite_check` verifies that a pin matches its row. It **cannot**
verify that the row is the *right* one to cite at that site. A citation aimed at `A2` where the
argument needs `A4` reads green forever. The gate's tick therefore reads as verification of the
citation set and is not — it verifies one relation and is silent on the one that carries the meaning.
`EXTRACTION_PROMPT.md` §8/R2 requires this hand-written substitute: **which claim does each citation
lean on, and why that row rather than a sibling.** Not machine-checkable; *reviewable*, which is what
is available.

## Pinned claim citations (digest-checked)

| site | pin | leans on | why THAT row, not a sibling |
|---|---|---|---|
| §II.3, the checker's blind spot — *"a positive control is the instrument by which a search states its own scope"* appears at §III.3 | `CWC:A2@31548228` | **A2 — selections are standpoints.** The point being made is that *"no occurrences found"* is a **compression over a frame**, so the frame must be stated. | **Not `A1`** (working-set finitude): A1 says the selection must happen; the claim here is about what the selection *conceals*, which is A2's territory. **Not `A3`** (what a holder of a compression can do): A3 is about the holder's options, not about the search's own scope. **Not `A4`** (warrant transfer): no warrant crosses a boundary in this sentence. Digest re-derived with `claim_digest.sh A2` on 2026-08-20; matches. |

**Only one pinned citation is made.** That is deliberate and is itself an aptness decision: the paper
cites the derivation exactly where its argument *rests* on a derivation claim, and nowhere else. The
temptation in an extracted paper is to sprinkle pins to look well-grounded; each unearned pin is a
site where the aptness question can go wrong at no benefit, and R5's unpinnable count grows at the
edge while every gate stays green. **Restraint here is cheaper than review later.**

## Cross-document section citations (NOT pinnable — declared class, R5)

These reference `amnesiac_institution_v0_6.md`, which has no Appendix A and therefore no digest
scheme. They are **cited as authority**, because v0.6 remains canonical for all of it (`docs/practice/README.md`).

| site | cites | leans on | why that section |
|---|---|---|---|
| §I.2 workforce properties | v0.6 §3.1 | the five properties that force the method | §3.1 is the canonical statement; restating it would recreate the OQ-287 duplication on a second axis |
| §II.1 the five records | v0.6 §3.4 | the differential-retention table | cited, not reproduced — the paper needs the *principle*, and the table is v0.6's |
| §II.2 promotion test + its two gaps + the 2026-08-18 draw | v0.6 §8.2 | the test's wording, the frequency-not-severity gap, the one-draw-per-arm existence witness | §8.2 carries the ruling AND the pre-registered draw; §8.5's channel cap is cited separately at §V.1 because it is a different mechanism |
| §IV.3 the eleventh instance, `partition_check: 186 == 185` | v0.6 §7.4 / §10.5 | the one gate-caught instance, and the narrow generalisation about structural invariants | §7.4 holds the instance table; §10.5 holds the generalisation. **Both cited, because citing only §7.4 would give the count without the reading that makes it informative.** |
| §IV.4 the two unstated selection rules | v0.6 §7.4.1 | the sixteen-texts manifest and the 46×-headroom canary, plus the held-out third | §7.4.1 is where the *pattern* is established at two instances; §7.4's table is the raw instances |
| §V.4 the human's second jurisdiction as structural | v0.6 §9.2 | cross-session continuity, not authority, is what creates the jurisdiction | **Not §9.1** (serial write-lock): that is a concurrency mechanism. **Not §9.3** (economics). §9.2 is the only section making the *positional* argument §V.4 needs. |
| §V.1/§V.5 the catch bit, the cap, the refusal-in-place-of-a-rate | v0.6 §10.1, §10.2, §8.5, §10.5 | the hazard, the instrument, the cap, and the no-decline limit | four sections because they are four distinct mechanisms; collapsing them to one cite would misattribute |

## One citation deliberately NOT made

**`the_perturbation_principle.md` is not cited in the practice paper**, although v0.6 §2.8 and §2.9
both open by citing it. The material extracted here is v0.6's *application* of that principle to the
development domain, which is what moved; the principle itself did not move and is not this paper's to
carry. Citing it would have implied this paper is canonical for it. Recorded because the absence of a
citation is otherwise indistinguishable from an oversight.
