# Build Discipline — Recurring Failure Modes

Implementation note. Scope: two defect *patterns* that have appeared in multiple
unrelated subsystems, with diagnostics for catching them. This is not general
architecture; it is the specific shape of mistakes this repo keeps making, recorded so
they stop. Pointer in `CLAUDE.md` → Build Discipline.

The root cause is structural, not careless: the repo was built fast by one person, the
*producing* step of any feature is the interesting part, and the *reconciling* step —
wiring the output to a consumer, collapsing a fork back to one canonical copy — has no
payoff in the moment and is infinitely deferrable. So it gets deferred, and the deferral
is invisible because the producer looks finished. Both patterns below are that one cause
wearing two faces.

---

## Pattern 1 — Produced-but-not-consumed (the dangling wire)

**Shape:** data is correctly generated, written to disk, and never read back into the
thing that needs it. Every check on the *producer* passes; the gap is in the absent
consumer, so nothing fails — the information just sits unused.

**Known instances:**
- Sensitivity sweeps in `python/sweeps/` write `*_sensitivity_results.json`; no consumer
  reads them. The fold-tightness data the engine produces about its own parameters is
  measured and discarded.
- SCOPE writes `outputs/kernel_manifests/<run_tag>/kernel_grouping.json` (the authoritative
  kernel→readings grouping) but the grouping was not stamped into the generated `.pl`
  files. Result: ~83 stories carried `cs_story_uid` and no `cs_kernel_id` — the linkage
  existed in the manifest and in filename convention, but not as a fact the engine could
  query. The cross-reading machinery therefore could not gather a kernel's readings.
- The pipeline manifest convention exists so audits *can* cite provenance, but nothing
  enforces that an audit actually does.

**Rule:** a producer is not done until something consumes its output. When you add a step
that writes data, in the same change either wire the consumer or add a check that fails
loudly when the output is left unconsumed.

**Diagnostic — find orphaned outputs:**
```bash
# JSON written by some script but grepped-for by none
for f in $(find outputs python -name "*.json" 2>/dev/null); do
  base=$(basename "$f")
  consumers=$(grep -rl "$base" python prolog agent --include=*.py --include=*.pl 2>/dev/null \
              | grep -v "$(dirname $f)" | wc -l)
  [ "$consumers" -eq 0 ] && echo "ORPHAN: $f"
done

# stories with identity but no fold-membership (the linkage gap)
for f in prolog/testsets/*.pl; do
  grep -q cs_story_uid "$f" && ! grep -q cs_kernel_id "$f" && basename "$f"
done
```

---

## Pattern 2 — One-canonical-thing-became-two (the silent fork)

**Shape:** a file or record is copied to a scratch/test location, possibly edited, and now
two versions coexist with no queryable fact stating which is canonical. The knowledge
lives only in memory ("I put it there to test it; a model moved the other one"). A
downstream step that targets the wrong copy produces results that look correct and are not.

**Known instances:**
- `generate_kernel_corpus.py` exists in both `commitment_corpus/` (test copy) and `agent/`.
  Targeting the non-canonical copy with a linkage join would stamp facts into a file
  generation does not use — a "fix" that lies.
- Historically: ISSUES.md / AGENDA.md / PRIORITIES.md / TODO.md were all tracking
  surfaces, but the end-of-session update protocol named only some, so the unnamed ones
  silently drifted (TODO.md held a live work item the protocol never reconciled).

**Rule:** one canonical location per thing, and which one is canonical must be a *checked
fact* — a documented path, a CI assertion — not a memory. Resolve a discovered fork by
evidence, not preference:
1. Which path do the documented run-commands actually invoke? (`grep` READMEs, CLAUDE.md,
   AGENTS.md, Makefile, and the module's own usage string.)
2. Which copy's imports resolve from its location?
3. `git log` recency / which is the move-destination.

Record the verdict in CLAUDE.md `Known State` and grep for references to the retired path
before deleting it (a retired copy with live references is Pattern 1 one layer up).

**Diagnostic — find forks:**
```bash
# same basename in 2+ locations (excluding archives)
find . -name "*.py" -not -path "*/archive*" -not -path "*/node_modules/*" \
  | xargs -n1 basename | sort | uniq -d

# duplicate Prolog module declarations (a hard load collision)
grep -rhoE "^:- module\([a-z0-9_]+" prolog --include=*.pl | sort | uniq -d
```

---

## Pattern 3 — Bound-probe bypasses clause-order (query-binding-bypasses-cut)

**Shape:** a probe enumerates a class by *binding* the selecting argument —
`findall(C, constraint_signature(C, natural_law), Cs)` — and receives constraints the
engine never actually classifies as that class.

**Mechanism:** `signature_detection:constraint_signature/2` resolves by clause order, with
lock clauses that fire first under a cut when the engine calls with the second argument
*unbound*:

```prolog
% prolog/signature_detection.pl
:70  constraint_signature(C, false_natural_law)    :- false_natural_law(C, _), !.
:77  constraint_signature(C, false_ci_rope)        :- false_ci_rope(C, _), !.
:87  constraint_signature(C, false_summit_mountain) :- false_summit_mountain(C, _), !.
:97  constraint_signature(C, natural_law)           :-
         domain_priors:emerges_naturally(C),
         get_constraint_profile(C, Profile),
         natural_law_signature(Profile), !.
```

When the probe binds the second arg to `natural_law`, the lock clause heads
(`false_natural_law`, `false_ci_rope`, `false_summit_mountain`) fail to unify (wrong atom),
their cuts *never execute*, and Prolog falls through to the `:97` clause. The probe answers
"satisfies the `natural_law` clause body in isolation," not "the engine assigns
`natural_law`" — they differ exactly when a lock would have fired.

**Live demonstration (223-constraint corpus, 2026-05-30):**

```
findall(C, signature_detection:constraint_signature(C, natural_law), BoundCs)
  → [behavioral_competence_reading]   % bound form: 1 result

findall(C, (signature_detection:constraint_signature(C, Sig), Sig == natural_law), UnboundCs)
  → []                                % unbound+post-filter: 0 results
```

`behavioral_competence_reading` satisfies the `:97` clause body but the engine actually
assigns it `false_summit_mountain` (lock at `:87`). The bound probe manufactured a false
witness.

**Fix:** query unbound, take the engine's first solution, post-filter by equality:

```prolog
findall(C, (signature_detection:constraint_signature(C, Sig), Sig == natural_law), Cs).
```

**Diagnostic:** any `findall`/`forall` over a cut-ordered predicate with the *selecting*
argument bound is suspect. Re-run unbound + post-filter; if the count drops, the bound form
over-counted.

**Where it recurs:** a probe that queries signature membership directly to build a witness
set for kernel readings will over-count exactly the constraints the locks were installed to
protect. Welfare-reading / false_natural_law (OQ-30) is the live case: a bound probe there
manufactures false natural-law witnesses on the constraints `false_natural_law` was designed
to intercept.

---

## The shared root: build for the corpus you want, not the one you have

Both patterns are special cases of designing against the present sample instead of the
intended target. The corpus on disk is one generation; naming schemes, linkage rules, and
reports must be correct for the corpus you are *heading toward* — thousands of stories,
regeneration under schema change, found-article ingestion, adversarial input. Checking a
design against today's corpus is confirmation, not perturbation: a naming scheme that
*happens not to* collide in 223 files is not the same as one that *cannot* collide by
construction.

Concrete application — reading names. A reading named from its interpretive label alone
(`hybrid_reading`, `autonomy_reading`) is unique today but not unique across kernels; the
moment two kernels each want a "hybrid" reading, the bare name denotes two stories. The
collision-proof scheme namespaces the name under its kernel (`<kernel>__<reading>`), making
the module name, filename, and predicate base unique by construction and making
"readings of kernel K" a prefix query. Identity stays on `cs_story_uid` (UUID, stable
through regeneration); the compound name is the human- and load-facing handle the UUID
cannot be.

Kernel membership follows SCOPE's judgment, recorded in the manifest, not a generation-time
heuristic: if SCOPE marked the seed `is_contested_kernel: true`, every reading gets a
`cs_kernel_id` even when it is the only reading so far (a one-cut fold the next reading
attaches to); if `false`, the story is a genuine standalone and gets no kernel_id. The
join step transcribes that decision from `kernel_grouping.json` into the `.pl`; it does
not re-derive it.

## Pattern 4: Fabricated default — missing-data fallback that emits a real-looking value

A predicate that lacks its input fabricates a plausible constant rather than failing or
returning `unknown`. Downstream callers receive a real-looking value and treat it as a
measurement. The fabrication fires silently — no error, no warning, no coverage flag —
and is distinguishable from a genuine measurement only by perturbation (tripwiring the
fallback to an obviously wrong value and observing what flips).

**Sibling of produced-but-not-consumed:** P-b-n-c leaves a wire dangling; the fabricated
default connects the wire to a made-up signal so nothing looks broken. The defect is
harder to see because the system appears to work.

**Live instance (OQ-33, 2026-05-30):** `classify_at_time` (`drl_composition.pl:179`)
falls back to `Supp=0.5` when `suppression_requirement` is absent. That measurement is
absent in 190/190 live testsets — the fallback fires 100% of the temporal path.
Tripwire confirmed: 443/519 non-unknown temporal classifications flip to `unknown` when
`Supp` is poisoned at source, proving the fabricated default is **LOAD-BEARING-WRONG**.
Secondary finding: the static path (`get_raw_suppression`, `drl_core.pl:96`) fabricates
the same gap as `Supp=0`, not `0.5` — two surfaces invent different fillers for the same
missing data, producing divergence that is artifact, not observational signal. See OQ-33
for resolution options and blocks.

**Diagnostic:** if a predicate has a catch-all clause that binds a metric variable to a
constant — `(measurement(..., V) -> true ; V = 0.5)` — ask whether the fallback is ever
reached in the actual corpus. If it fires on more than a handful of constraints, it is a
fabrication, not a safety net. Tripwire it: replace the constant with an obviously
out-of-range value and count the flips.

**Where it recurs:** any Surface-3 (temporal) predicate that reads authored measurements
from testsets; authored fields are sparse by construction (authors fill what they
understand), so temporal surfaces are structurally exposed to this pattern.

---

## When reasoning has run out

A corollary, since both patterns above were diagnosed by *running greps*, not by thinking:
design reasoning has a stopping point past which the next real information comes only from
building and testing. Claims like "the UUID survives regeneration" or "this naming scheme
holds at scale" cannot be settled by argument — they are settled by regenerating a small
corpus and watching what breaks. When a design question has been reasoned to the point
where further turns produce elaboration rather than resolution, that is the signal to build
the thinnest real version and test it, not to think harder.
