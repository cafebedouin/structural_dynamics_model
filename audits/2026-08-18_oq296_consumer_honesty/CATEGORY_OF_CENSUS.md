# `domain_priors:category_of/2` — two-leg census + dispatch control (OQ-296 spawn evidence)

Executed 2026-08-18. Run BEFORE minting the spawned OQ, per operator ruling: a one-leg
constancy claim would reproduce inside this spawn the exact error the parent OQ's
scope-widening block warns about (`natural_law_without_beneficiary/1` looks dead on the
live leg and has 30 firings on kernel_v1 — the live-leg zero was a corpus property).

## Census — constant on BOTH measured legs

```
live leg (testsets):              N=279   distribution=[unknown_novel-279]
archives/datasets/kernel_v1:      N=1106  distribution=[unknown_novel-1106]
```

Overlay used `asserta` (per CLAUDE.md Corpus Loading — plain `assertz` appends after
config.pl's default clause and silently loads `testsets`); the loader's own
`[corpus] Loaded 1106 testset files` line confirms the overlay took.

## Discrimination control — the predicate is NOT broken

```
[control] planted constraint_claim(_, natural_law)  -> physical_natural
[control] planted constraint_claim(_, physical_law) -> physical_natural
[control] no claim at all (decline)                 -> unknown_novel
```

Two-sided: fires on both accepted inputs, declines on absence.

## Authored-input census — the actual mechanism

`constraint_claim(_, natural_law | physical_law)` occurrences:

```
testsets:                     0
testsets_haiku:               0
testsets_flash:               0
testsets_kimi:                0
testsets_sonnet:              0
archives/datasets/kernel_v1:  0
```

0 across ~5,311 story files.

## Finding — corrects the plan's characterization

The plan (and this session's first summary of it) described `category_of/2` as a
"collapsed classifier returning a degraded near-constant." That is not what the evidence
says. Two facts, both true, with different consequences:

1. It **collapsed structurally** — the `domain_registry` deletion (Feb 2026) removed the
   clauses that keyed other categories, leaving two.
2. Of the two survivors, the non-fallback clause **dispatches correctly but has zero
   authored input** in any corpus. The constant is absence-of-input, not misclassification.

So this is Pattern 5 (absence satisfies the gate) at the consumer end, not a broken
predicate. The distinction matters for the spawned OQ's disposition: "restore the
classifier" and "author the claim vocabulary, or retire the category axis" are different
repairs, and the evidence points at the second. The OQ is minted with this framing rather
than the plan's, and still decides nothing — per ruling, the disposition is not settled
while holding the delete key.

**Consumers (live, unenumerated by the plan):** `isomorphism_report.pl:26,27,62,64`,
`constraint_bridge.pl:127`, `data_validation.pl:165,266,268,511,518`.
