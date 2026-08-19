# audit_log — bound-caller re-witness (OQ-303 arm (a) premise)

Chronological. Every result line below the PREREGISTRATION md5 line was produced after
that freeze. Nothing above it is a result.

---

## OPEN — 2026-08-18

**Directory date note.** The plan named `2026-08-19_bound_caller_rewitness`; the audit
location mandate says **date = execution date**, and execution is 2026-08-18
(`Tue Aug 18 21:21:09 CDT 2026`). Directory dated to the run, not to the plan.

**HEAD stamp (OPEN):**

```
$ git rev-parse HEAD
0300be246bb28b5b173f41b5ce3f9d1636a9e7a4
$ git log --oneline -1
0300be24 OQ-308: the disposition check's second direction, witnessed
$ git status --porcelain
(clean)
```

**Prior-art grep — is there already a codewalk instrument?**

```
$ grep -rn "prolog_codewalk\|prolog_walk_code" --include=*.md --include=*.py --include=*.pl --include=*.sh . | grep -v '^./.venv\|^./.git'
(no output)
```

Zero hits: `library(prolog_codewalk)` has never been used in tracked substrate. Not a
Pattern-2 fork risk; this is a first instrument, not a second walker.

```
$ grep -rln "caller_sweep" --include=*.md --include=*.py . | grep -v .venv
audits/2026-08-17_bound_dispatch_hardening/RECON.md
audits/2026-08-17_bound_dispatch_hardening/WRITEUP.md
audits/2026-08-17_bound_dispatch_hardening/caller_sweep.py
```

The regex arm exists in exactly one place and is reused, not reimplemented.

**Baseline `./scripts/gate.sh` — GREEN, 24 rows, before any file in this audit existed:**

```
# Gate checks  [interpreter: /home/scott/bin/structural_dynamics_model/.venv/bin/python]
  ✓ python env       python env: GREEN — interpreter .../.venv/bin/python (py3.12.3); 15 third-party import(s) required, 0 missing, 1/1 declared-optional absent, 3/3 hook scripts stdlib-only, 0 unparseable
  ✓ python env st    python_env_check selftest: OK (12/12 controls)
  ✓ issues_status    324 parsed, 0 malformed
  ✓ omega check      0 problems
  ✓ omega selftest   selftest: all positive controls fired (10/10)
  ✓ omega index      index --check: fresh (324 rows, 137 active / 187 archive)
  ✓ spec enums       spec_enum_check: GREEN — 8 enums in sync (selftest 3/3 red-capable)
  ✓ doc patterns     doc_pattern_check: GREEN — 8 indices, 0 declared collisions — every index compared, nothing exempted (OQ-278 ruled 2026-08-17); names checked CLAUDE.md vs BD headings, spine table index-set only; selftest 7/7
  ✓ bound selector   bound_selector_check: GREEN — 4455 files, 1 cut-ordered predicate(s) registered, 2 exemption(s), 8 declared path exclusion(s) (.claude, .git, .venv, archives, audits, node_modules, outputs, testsets), selftest 11/11
  ✓ dispatch head    dispatch_head_check: GREEN — 125 engine files, 70 shape hit(s) all declared (70 declared + 3 must-not-fire), 0 file(s) with read errors, selftest OK
  ✓ displaced cites  pattern_citation_check: GREEN — declared consumers of displaced members — 'bound-probe' (renumbered 3 -> 7): 17 declared citation(s) across 7 files; 'destructive-replace' (vacated): SWEPT CLEAN, 0 remaining; selftest 5/5
  ✓ module bounds    module_boundary_check: GREEN — 180 engine files, 104 modules, 737 bypass site(s) over 99 predicate(s), all declared in 117 allowlist row(s); arms C/F/G scanned leg(s): testsets, testsets_haiku, testsets_flash, testsets_kimi, testsets_sonnet (40 schema heads over 4205 story files); register 63 declared predicate(s) / 63 schema_shape row(s); 0 stale-row note(s); 1 unwired schema predicate(s) watched (flat_control_of/2); selftest 50/50
  ✓ claim cites      claim_cite_check: 63 live citation(s), 18 recorded (superseded, not checked), 6 section-only in docs/ + 17 in apparatus (UNPINNABLE, declared class — counted, not checked)
  ✓ claim cites st     PASS  checker's own fixtures are records, not live citations (no self-fire)
  ✓ known_state      308 entries parsed, 0 problems
  ✓ axis boundary    [AXIS-SELFTEST] ALL PASS
  ✓ audit cites      ERRORS: 0
  ✓ paper carriage   amnesiac carriage check: GREEN — 15 carriage invariants asserted (expected counts published in the manifest); selftest 6/6, every plant verified to land
  ✓ audit writeup    audit writeup gate: OK (190 dirs, 22 enforced, 0 problems)
  ✓ apparatus        apparatus: catch-rate 15L/1l/0n of 16 bits (16 of 65 writeups carry one; forward-only from 2026-08-10) — NO DECLINE EVER RECORDED: readout not yet interpretable (OQ-276); channel 33/33; GREEN
  ✓ gap surfaces     gap surfaces check: 3/3 human surfaces distinguish no_gap vs undetermined (self-test OK)
  ✓ cli selftest     cli selftest: OK (260 commands across 17 groups)
  ✓ tripwire hook    SELFTEST GREEN
  ✓ oq277 freeze     prereg freeze: GREEN — stamp verified; selftest 7/7

GATE: GREEN
```

**Registry read — the worklist size, from the registry, not from prose.**

```
$ .venv/bin/python -c "
import sys; sys.path.insert(0,'python')
from dispatch_head_check import DECLARED
from collections import Counter
c=Counter(DECLARED.values())
print('TOTAL', len(DECLARED))
for k,v in sorted(c.items()): print(k,v)
"
TOTAL 73
MUST-NOT-FIRE 3
finding 3
input-key 3
latent-B 58
wrapper 6
```

**`N_latentB = 58`** (registry total 73). Every later count in this unit refers to this
number. OQ-303 prose says "~60"; the registry says 58. The verification line "all N
classified" means **58**.

---

## PREREGISTRATION FROZEN — 2026-08-18, before the first Phase-1 run

```
$ md5sum audits/2026-08-18_bound_caller_rewitness/PREREGISTRATION.md
900d4a4267da5ef16ccd79dcede61364  audits/2026-08-18_bound_caller_rewitness/PREREGISTRATION.md
```

**Everything below this line is a result.**

---

## RESULTS — 2026-08-18

### R1. Instrument built; selftest green (fixtures swept by the real walker, same path)

```
$ .venv/bin/python python/codewalk_caller_check.py --selftest
codewalk_caller_check: selftest OK (4 regex-blind positive shapes, module-resolution control, unification-stratum control both ways [evaluate true/false], runtime-residue control both ways, free-call looked-and-declined control, comment negative, unresolved-spec control, empty-scan control)
```

### R2. Two-sided discrimination record, BOTH halves in the same process

```
$ swipl -q -g "[stack]" -l codewalk_caller.pl -g "run_codewalk_caller('spec_ctl.txt'), halt"
CWC_PRED: drl_core.pl dr_type/3 module=drl_core sites=67 bound=19
CWC_PRED: signature_detection.pl constraint_signature/2 module=signature_detection sites=18 bound=0
CWC_WALKED: 184
CWC_SCANNED: 2
CWC_MODULES: 91
```

FIRES on `dr_type/3` (19 bound sites). DECLINES on `constraint_signature/2` — and the
informative part is `sites=18`: the arm LOOKED and found the predicate's call sites, then
reported zero bound among them. A declines-control reporting `sites=0` would be a
didn't-look, and `check_controls()` fails the run on exactly that.

### R3. THE PREREG'S §6 FALSIFIER FIRED — the "one shared residue class" clause is FALSE

The unit was built on the claim that a selector bound by unification before the call
(`T = rope, ..., p(C, T)`) is invisible to BOTH arms. The selftest control for that shape
came back BOUND, not free. Cause, witnessed directly rather than inferred:

```
$ swipl -q -l inline_witness.pl -g "forall(member(P,[d_inline,d_runtime,d_helper,d_arith]), (clause(iw:P,B), format('~w :- ~q~n',[P,B]))), halt"
d_inline :- _12542=alpha,q(a,_12542)
d_runtime :- member(_12548,[beta]),q(a,_12548)
d_helper :- pick(_12542),r(a,_12542)
d_arith :- _12548 is 1+1,nth0(_12548,[gamma,delta,epsilon],_12576),r(a,_12576)
```

So SWI does **not** compile `T = alpha` away — the stored clause body still holds it. The
binding comes from the walker:

```
$ sed -n '658,665p' /usr/lib/swi-prolog/library/prolog_codewalk.pl
evaluate(Goal, Module, OTerm) :-
    walk_option_evaluate(OTerm, Evaluate),
    Evaluate \== false,
    evaluate(Goal, Module).

evaluate(A=B, _) :-
    unify_with_occurs_check(A, B).
```

`library(prolog_codewalk)` EXECUTES `A=B` as it walks, default ON, and the binding
propagates into later goals of the same body. The named shape is therefore a **third
codewalk-only capability**, not shared residue. The genuine shared residue is narrower:
selectors bound by RUNTIME COMPUTATION (`member/2`, a helper predicate's output,
arithmetic) — `evaluate/2` handles unification only. Both are now selftest controls, and
`run_codewalk_caller/2` takes the flag so the stratum is MEASURED, not declared.

### R4. Load-chain measurement — `[stack]` alone cannot score 17 of the 58 rows

```
$ swipl -q -g "[stack]" -g "forall(member(F,[...11 registry files...]), (source_file(P0), file_base_name(P0,F) -> format('LOADED ~w~n',[F]) ; format('not-loaded ~w~n',[F])))" -g halt
not-loaded json_report.pl
not-loaded fpn_report.pl
not-loaded maxent_report.pl
not-loaded orbit_report.pl
not-loaded diagnostic_summary.pl
not-loaded routing_sink.pl
not-loaded giant_component_analysis.pl
not-loaded invertibility_analysis.pl
not-loaded gap_diagnostic.pl
not-loaded context_profile_mining.pl
not-loaded probe_oq197_controls.pl
```

Under `[stack]` the arm resolved 54 of 74 specs (20 unresolved, 17 of them `latent-B`).
Four of the eleven files carry **no `:- module/2` header at all** (`json_report`,
`fpn_report`, `maxent_report`, `orbit_report`), so a file→module lookup can never resolve
them; resolution was moved to `source_file/2`, which handles the headerless case (their
predicates live in `user`). The load chain was then extended with `ensure_loaded/1` over
every registry file.

### R5. The extended chain hangs on exactly one file — and the cause is `evaluate` itself

Per-file budget sweep, `[stack]` + one file, full 74-spec walk, 90 s cap:

```
context_profile_mining         rc=0 t=607ms
diagnostic_summary             rc=0 t=567ms
fpn_report                     rc=0 t=550ms
gap_diagnostic                 rc=0 t=566ms
giant_component_analysis       rc=0 t=591ms
invertibility_analysis         rc=0 t=698ms
json_report                    rc=124 t=90004ms
maxent_report                  rc=0 t=590ms
orbit_report                   rc=0 t=578ms
probe_oq197_controls           rc=0 t=607ms
routing_sink                   rc=0 t=682ms
```

```
$ swipl ... -g "run_codewalk_caller('spec_full.txt', false), halt"   # json_report loaded
json+evaluate(false) rc=0            # 0.6s
```

`json_report.pl` does not terminate under `evaluate(true)` and walks in 0.6 s under
`evaluate(false)`. The non-termination is in the `A=B` propagation — **the same feature
that produced R3**. It is a DECLARED, PRINTED load exclusion (`LOAD_EXCLUSIONS` in
`codewalk_caller_check.py`), costing 2 `latent-B` rows (`boltzmann_label/2`,
`live_index_label/3`) plus `write_json_number/2` (input-key).

### R6. Live sweep — one `latent-B` row has a live bound caller

```
$ .venv/bin/python python/codewalk_caller_check.py --check
  latent-B signature_detection.pl signature_grade/2: 1 bound call site(s) under the codewalk arm — the class label says none was found. Adjudicate before converting.
codewalk_caller_check: RED — 1 problem(s)
```

RED is the correct state of this row, not a checker defect: the registry says `latent-B`
("no live bound caller found") and one exists. It goes GREEN when the row is converted or
reclassified.

### R7. The unification-bound stratum on the live engine is measured EMPTY

```
eval=true : preds 71 unres 3 sites 214 bound 35
eval=false: preds 71 unres 3 sites 214 bound 35
```

Identical. Zero registry call sites in the engine bind their selector by a same-clause
unification — and the selftest's `fx_unify` control proves the measurement discriminates
(bound=1 under evaluate(true), bound=0 under evaluate(false) on planted code). A measured
zero with a firing control, not a didn't-look.

### R8. The partition — 58 rows, all classified, row count asserted against the registry

```
$ .venv/bin/python audits/2026-08-18_bound_caller_rewitness/partition.py
PARTITION: 58 latent-B rows (registry N_latentB=58), all classified
  converts-clean   55
  live-output-path 1
  regex-only       2
  unification-bound sites (evaluate true minus false): 0
  codewalk unresolved: 2
  (machine, pre-adjudication: converts-clean=53, not-latent=3, regex-only=2)
  wrote partition.tsv, partition.md, regex_sweep_raw.txt, codewalk_evaluate_{true,false}.json
```

### R9. The three disagreements, each a DIFFERENT mechanism — read one by one

**`composition_rule/3` — rx 5, cw 0. Regex FALSE POSITIVES.** All five sit inside block
comments:

```
$ grep -n "/\*\|\*/" prolog/dirac_classification.pl | awk -F: '$1>150 && $1<500'
224:/* ================================================================
239:   ================================================================ */
329:/* ...
429:/* ================================================================
455:   ================================================================ */
```

Sites are at `:235` and `:450-454`, inside 224–239 and 429–455. `caller_sweep.py`'s
`is_comment()` skips only lines starting `%` or `*`; these start `behavior:` and
`- composition_rule(`. Already adjudicated identically at
`audits/2026-08-17_bound_dispatch_hardening/RECON.md:95` — this run reproduces that
adjudication mechanically instead of by reading.

**`claimed_natural/2` — rx 3, cw 0. Regex TRUE POSITIVES the codewalk arm cannot see.**

```
$ sed -n '70,95p' python/audits/oq49_override_remeasure.py
      -> ( signature_detection:claimed_natural(C, explicit_mountain_claim)
           -> Src = source1
           ; signature_detection:claimed_natural(C, natural_law_signature_match)
$ git log --diff-filter=A --format="%h %ad" --date=short -- python/audits/oq49_override_remeasure.py
55da4de6 2026-06-14
```

Real bound goals inside a Prolog goal string embedded in a `.py` file, present at the
2026-08-17 sweep. **This is the regex arm's genuine unique capability, witnessed.**
Adjudicated class B with note at `RECON.md:96` (point-in-time audit probe, no live output
path); conversion must update that probe in the same change.

**`signature_grade/2` — rx 0, cw 1. NEW. A codewalk TRUE POSITIVE the regex missed, on a
live output path.**

```
$ sed -n '1945,1953p' prolog/signature_detection.pl
signature_severity(C, Sev) :-
    constraint_signature(C, Sig),
    converted_at_seat(C, Sig),
    !,
    signature_diagnostic_severity(C, Sig, Sev).
signature_severity(C, moderate) :-
    signature_grade(C, correction).

$ grep -n "signature_grade" audits/2026-08-17_bound_dispatch_hardening/caller_sweep_output.txt
251:== signature_grade/2 (signature_detection.pl): 0 bound call site(s)

$ git show 9a5d8526:prolog/signature_detection.pl | grep -n "signature_grade(C, correction)"
1879:signature_grade(C, correction) :-
1901:    signature_grade(C, correction).
```

The call is the clause's LAST goal on its own line, so `caller_sweep.py`'s
`is_clause_head()` reads the terminating `.` as a fact head and skips it. Line 1901 shows
the call existed at the census HEAD, so this is a **true false negative of the regex arm at
sweep time**, not a later addition. It is on a live output path:

```
$ sed -n '749,752p' prolog/diagnostic_summary.pl
    (   signature_detection:signature_severity(C, SigSev)
    ->  Alerts = [alert(signature_correction, SigSev, signature_grade)|MisAlerts]
```

`signature_grade/2` → `signature_severity/2` → `diagnostic_summary:join_alerts/2` → the
OQ-98 `verdict_join` headline. Conversion owes the full six-leg clean-vs-edited pair.

### R10. Prereg gap surfaced at execution — a fifth disposition was needed

`PREREGISTRATION.md` §3 fixes four dispositions and requires that no row be left
unclassified. `converts-clean` is defined as "zero bound call sites under **both**
instruments". Two rows (`json_report.pl boltzmann_label/2`, `live_index_label/3`) have no
codewalk verdict at all — the arm could not resolve them (R5). They are therefore neither
`converts-clean` nor any of the other three. They are reported as **`regex-only`**, named
in `partition.py` and in the table as NOT PRE-REGISTERED. The prereg is not amended; the
gap is recorded here and in the writeup, which is the honest record.

## RESULTS (second pass, same day) — operator directives 2026-08-18

### R11. `signature_grade/2`'s bound caller is BENIGN at its atom — and the control fires

R6/R9 established that a live bound caller exists where the class label says none does. What
they did **not** establish is whether the bound form answers differently. Checked, all five
live legs (`signature_grade_agreement.txt`, probe `sg_probe.pl`):

```
testsets         corpus=279  bound=45  unbound_filtered=45  only_bound=0 only_unbound=0  control(commentary): bound=263 unbound_filtered=234
testsets_haiku   corpus=960  bound=61  unbound_filtered=61  only_bound=0 only_unbound=0  control(commentary): bound=932 unbound_filtered=899
testsets_flash   corpus=960  bound=126 unbound_filtered=126 only_bound=0 only_unbound=0  control(commentary): bound=886 unbound_filtered=834
testsets_kimi    corpus=1005 bound=73  unbound_filtered=73  only_bound=0 only_unbound=0  control(commentary): bound=993 unbound_filtered=932
testsets_sonnet  corpus=1001 bound=216 unbound_filtered=216 only_bound=0 only_unbound=0  control(commentary): bound=952 unbound_filtered=785
```

Exact agreement at `correction`; the **control at the sibling atom `commentary` diverges on
every leg** (29/33/52/61/167). So the probe discriminates, and over-permissiveness on this
predicate is ATOM-SPECIFIC. Structural reason, from the clause set: clause 1 (`:1924`) has a
fresh-variable head so its cut always runs, and a bound `correction` query skips only clause
3's cut, whose atom cannot match; a bound `commentary` query skips clause 2's cut, and clause 2
binds `correction` — the answer it steals.

**This corrects R6's framing**, which read as "a live hazard on the headline path". The class
label is wrong; the caller is benign at its atom. Conversion still owes the six-leg pair (live
output path, semantics-changing by construction), but this is not a firing defect.

### R12. Allowlisted and gate-wired GREEN — discrimination record, both directions

`prolog/codewalk_caller_allowlist.txt`, grammar
`<file.pl>:<name>/<arity>  ATOMS=<a[,b]>  REMOVE=<condition>  <reason>`, all four columns
required, malformed row RED, missing file RED.

```
$ grep -v "^signature_detection.pl:signature_grade" allow.bak > prolog/codewalk_caller_allowlist.txt
$ .venv/bin/python python/codewalk_caller_check.py --check
  latent-B signature_detection.pl signature_grade/2: 1 bound call site(s) under the codewalk arm — the class label says none was found. Adjudicate and allowlist (with ATOMS + REMOVE) in the same change, or convert.
codewalk_caller_check: RED — 1 problem(s)

$ sed 's/ATOMS=correction/ATOMS=commentary/' allow.bak > prolog/codewalk_caller_allowlist.txt
$ .venv/bin/python python/codewalk_caller_check.py --check
  latent-B signature_detection.pl signature_grade/2: bound caller(s) on atom(s) ['correction'], which the allowlist (codewalk_caller_allowlist.txt:28) does NOT cover — it adjudicates only ['commentary']. Over-permissiveness is atom-specific; the listed atom's evidence does not transfer. Adjudicate this atom.
codewalk_caller_check: RED — 1 problem(s)

$ cp allow.bak prolog/codewalk_caller_allowlist.txt
$ .venv/bin/python python/codewalk_caller_check.py --check
codewalk_caller_check: GREEN — ...
```

The rogue-atom half is the one worth having: a predicate-level allowlist would have covered the
hazardous atom with the benign atom's evidence.

### R13. `evaluate(false)` recovery — the two single-instrument rows are no longer single-instrument

The `json_report.pl` exclusion is specific to `evaluate(true)`. A recovery pass re-walks the
rows it costs at `evaluate(false)` — module-resolved, multi-line bodies, meta-called goals,
minus only the unification-bound stratum:

```
$ .venv/bin/python audits/2026-08-18_bound_caller_rewitness/partition.py
PARTITION: 58 latent-B rows (registry N_latentB=58), all classified
  converts-clean   55
  converts-clean-minus-dataflow 2
  live-output-path 1
  unification-bound sites (evaluate true minus false): 0
  codewalk unresolved: 2
  (machine, pre-adjudication: converts-clean=53, converts-clean-minus-dataflow=2, not-latent=3)
```

`regex-only` is now empty. The missing stratum is named in the grade rather than rounded off —
these rows would otherwise have ridden into conversion on the evidence of the one instrument
that just produced a proven false negative on the headline path.

### R14. Gate at 25 rows, GREEN

```
  ✓ codewalk caller  codewalk_caller_check: GREEN — 74 registry spec(s) (58 latent-B), 99 loaded module(s), 1772 traced goal(s), 71 resolved / 3 unresolved, controls two-sided (fires dr_type/3 bound=20, declines constraint_signature/2 sites=29 bound=0); 1 adjudicated allowlist row(s) (signature_grade/2@['correction']); 1 declared load exclusion(s) under evaluate(true) (json_report.pl), 3 row(s) recovered at evaluate(false) as converts-clean-minus-dataflow; declared blind spots: ...; selftest OK
GATE: GREEN
```

Note the control figures moved (`dr_type/3` 67/19 → 80/20, `constraint_signature/2` 18/0 →
29/0) because the checker's load chain is wider than `[stack]`. **The figures are
chain-relative and both are now recorded in the module header** — a reader reproducing one on
the other chain would otherwise read a real difference as drift.

---

## CLOSE — 2026-08-18

**Commit-ordering note, stated rather than glossed.** `audits/README.md` asks that
`audit_log.md` be committed FIRST, before any code commit, so git testifies to the ordering
of pre-edit reads. That did not happen here — the instrument and the log were written in one
working session and land in the same commit. The exposure this rule guards is narrow in this
audit and is checkable anyway: every file this unit *read and quoted*
(`signature_detection.pl`, `dirac_classification.pl`, `diagnostic_summary.pl`,
`caller_sweep.py`, the 2026-08-17 audit artifacts) is **unmodified** by this unit, so the
quoted text can be re-verified against HEAD directly rather than reconstructed from a parent.
The files this unit wrote are all new. Recorded as a deviation, not a clean run.

**HEAD stamp (CLOSE):**

```
$ git diff --stat 0300be24 -- prolog/signature_detection.pl prolog/dirac_classification.pl prolog/diagnostic_summary.pl audits/2026-08-17_bound_dispatch_hardening/
(no output)
```

An empty diff over the read-set — which is a different thing from never having looked, and
is why the command is pasted rather than asserted.

**Head-stamp comparison.** OPEN stamp `0300be24` / working tree clean. No other writer
committed during the session (single-instance operation); the close stamp is this unit's own
first commit, and the diff between OPEN and CLOSE over this unit's read-set is therefore
whatever this unit wrote — nothing in the read-set. Verified with
`git diff --stat 0300be24 HEAD -- prolog/signature_detection.pl prolog/dirac_classification.pl prolog/diagnostic_summary.pl audits/2026-08-17_bound_dispatch_hardening/`
(pasted at the commit).

**Gate at close — GREEN, 24 rows (Unit A adds none):**

```
  ✓ issues_status    324 parsed, 0 malformed
  ✓ omega check      0 problems
  ✓ omega index      index --check: fresh (324 rows, 137 active / 187 archive)
  ✓ dispatch head    dispatch_head_check: GREEN — 126 engine files, 70 shape hit(s) all declared
  ✓ audit writeup    audit writeup gate: OK (191 dirs, 23 enforced, 0 problems)
  ✓ cli selftest     cli selftest: OK (261 commands across 17 groups)
GATE: GREEN
```

**SUPERSEDED by R12 (same day).** This section originally recorded the checker as deliberately
RED and not gate-wired. Operator ruled otherwise 2026-08-18: allowlist it and wire it GREEN —
a knowingly-red row trains its readers to ignore the channel, and an unwired checker strands
the discrimination. Final state: gate row `codewalk caller`, GREEN, 25 rows.

**Hard stop.** No conversion performed; no `prolog/*.pl` semantics changed; Unit B not begun.
