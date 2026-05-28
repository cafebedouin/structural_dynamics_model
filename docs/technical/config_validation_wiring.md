# config_validation.pl — Wiring and Extension Notes

Generated 2026-05-28 during OQ-25 implementation. Documents implementation-level
details that are non-obvious from reading the code and that caused real bugs in
the session that produced the OQ-25 guard.

---

## How the Validation Chain Works

```
corpus_loader:load_all_testsets/0
  └─ assertz(corpus_loaded)
  └─ config_validation:validate_config_postcorpus/0          ← wire point
       └─ findall(Msg, config_violation(Msg), Violations)
            └─ [every config_violation/1 clause backtracks here]
       └─ if Violations non-empty → halt(1)                  ← real rejected load
```

Adding a new post-corpus check: add a `config_violation/1` clause to
`config_validation.pl`. No changes to `corpus_loader.pl` needed. The check fires
automatically on every load.

`validate_config` (at module load time, line 182) runs the same clause collection
but BEFORE testsets are loaded — so any check gated on testset data must use
`current_predicate/1` to guard against empty-DB calls.

---

## The cs_story_uid Guard Pattern

Every CS-corpus check should open with:

```prolog
config_violation(Msg) :-
    current_predicate(narrative_ontology:cs_story_uid/2),
    ...
```

Reason: `cs_story_uid/2` is declared `:- dynamic` in `narrative_ontology.pl` (line 95),
so `current_predicate/1` returns `true` even when no CS stories are loaded. But
`findall(C, narrative_ontology:cs_story_uid(C, _), Cs0)` returns `[]` in that case,
so the clause still safely fails. The guard is belt-and-suspenders, consistent with
the existing UID uniqueness check pattern.

---

## setof vs findall+sort for Deduplication (Operator Precedence Bug)

**Do NOT use `setof(C, UID^narrative_ontology:cs_story_uid(C, UID), AllCs)`.**

Both `^` and `:` have SWI-Prolog operator priority 200 (xfy). In this context:

```
UID^narrative_ontology:cs_story_uid(C, UID)
```

may parse as `(UID^narrative_ontology):cs_story_uid(C, UID)` — a module call to the
compound term `UID^narrative_ontology` — which doesn't exist. SWI-Prolog reports this
as `Unknown procedure: '$bags':cs_story_uid/2` (the `$bags` module is setof/bagof's
internal execution context). The error is misleading — it looks like a module path
issue but it's a parse issue.

**Use instead:**

```prolog
findall(C, narrative_ontology:cs_story_uid(C, _), Cs0),
sort(Cs0, AllCs),
AllCs \= [],
member(C, AllCs),
```

`findall` doesn't have the `^` quantification syntax, so there's no ambiguity.
`sort/2` deduplicates. `AllCs \= []` guards against the empty case before `member/2`
would simply fail silently.

If you need `setof` with module-qualified goals, parenthesize explicitly:
```prolog
setof(C, UID^(narrative_ontology:cs_story_uid(C, UID)), AllCs)
```

---

## The Three CS Fact Adapters

For any check over CS stories, these are the three facts to use:

| Role | Fact | Notes |
|------|------|-------|
| CS-run marker | `narrative_ontology:cs_story_uid(C, UUID)` | Unique per generation event; presence distinguishes CS from non-CS stories |
| ε (what engine reads) | `narrative_ontology:constraint_metric(C, extractiveness, E)` | Via `constraint_data` → `drl_core:base_extractiveness/2`; this is the fact the sigmoid pipeline uses |
| Kernel link | `narrative_ontology:cs_kernel_id(C, K)` | N:1, many readings → one kernel atom |

Note: `domain_priors:base_extractiveness(C, E)` also exists in each testset file and
mirrors `constraint_metric`. `data_validation.pl` flags mismatches > 0.01. For engine
correctness checks, use `constraint_metric` (the path the engine actually takes).

---

## Divergence Count: Counting the Right Thing

`cs_kernel_registry:cs_kernel_divergence/4` has signature `(K, Ctx, UID1-C1, UID2-C2)`.
It generates one solution per `(K, Ctx, reading_pair)` combination — one per context
that shows divergence. With ~300 contexts per pair, total solutions are ~100× the
"reading-pair divergence count" in §5.11.

**§5.11 number (79/34) = unique (K, UID1, UID2) triples, NOT (K, Ctx, UID1, UID2) tuples.**

To reproduce the §5.11 count:

```prolog
findall(K-UID1-UID2,
        cs_kernel_registry:cs_kernel_divergence(K, _, UID1, UID2),
        Raw),
sort(Raw, Unique),          % collapse cross-context duplicates
length(Unique, NPairs),     % → 79
findall(K, member(K-_-_, Unique), Ks0),
sort(Ks0, UniqueKernels),
length(UniqueKernels, NKernels).  % → 34
```

`aggregate_all(count, cs_kernel_divergence(K,_,_,_), N)` counts ALL solutions
including context duplicates — will give ~379,000, not 79.

---

## Injection Testing Pattern

To test that a `config_violation/1` clause fires correctly, use `assertz` directly
rather than consulting a temp file:

```bash
swipl -g "
    use_module(narrative_ontology),
    use_module(config),
    use_module(config_validation),
    assertz(narrative_ontology:cs_story_uid(test_reading, 'fake-uuid')),
    assertz(narrative_ontology:constraint_metric(test_reading, extractiveness, 0.58)),
    assertz(narrative_ontology:constraint_metric(test_reading, extractiveness, 0.42)),
    validate_config_postcorpus,
    halt
" -t "halt(0)"
```

Consulting a temp file with `:- multifile narrative_ontology:cs_story_uid/2` headers
fails when run without the full stack pre-loaded — the `$bags` unknown-procedure error
reappears. Direct `assertz` bypasses the multifile declaration requirement.

Run from the `prolog/` directory so `use_module(narrative_ontology)` resolves.

---

## UID Uniqueness Check vs ε Coherence Check (Relationship)

The existing check (lines 167–176) is **UID-side**: detects when two `cs_story_uid`
facts share the same UUID value. It does NOT catch the chimera failure mode.

The OQ-25 guard is **reading-side**: detects when the same ConstraintAtom appears with
conflicting ε. A chimera produces two files with `cs_story_uid(C, uuid_a)` and
`cs_story_uid(C, uuid_b)` — distinct UUIDs, so the UID check passes — but two
`constraint_metric(C, extractiveness, E)` facts with different E, which the OQ-25
guard catches.

The two checks are complementary, not redundant.

---

## Grouping Key: Why ConstraintAtom, Not KernelAtom

`cs_kernel_id(ConstraintAtom, KernelAtom)` is N:1. A kernel with three readings
legitimately has three different ε values (OQ-26 resolved: ε is reading-relative).
Grouping by KernelAtom would false-positive on correct multi-reading kernels.

The invariant is per-reading: one ConstraintAtom → one ε in the DB (DP-001).
The chimera failure mode is same-ConstraintAtom-from-two-runs, not same-kernel.
