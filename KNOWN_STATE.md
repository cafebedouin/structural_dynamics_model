# Known State — Session Changelog

This is the dated session log split out of `CLAUDE.md` (2026-05-31) to cut the
auto-loaded instruction file's per-session token cost (~3,050 tokens / 45% of CLAUDE.md
were this section). **It is NOT auto-loaded** — read it on demand, and prefer the
query below to reading the whole file.

**Entry grammar (machine-readable, added 2026-06-04).** Every entry is:

```
## YYYY-MM-DD — <title>
**Files:** <comma-separated paths the entry concerns>
**Tier:** tripwire | correction-key | landed | history
```

Tiers: `tripwire` = standing do-not / silent-mistake warning; `correction-key` =
corrects prior claims or qualifies how results may be cited; `landed` = change/audit
shipped and witnessed; `history` = narrative/archival (roll-off candidate). Checker:
`python3 python/known_state_status.py --check` (run after editing this file; sibling of
`issues_status.py`).

**Before touching a file, query instead of reading everything:**
`python3 python/known_state_status.py --file <path>` lists the entries whose `Files:`
line names it — read those. (The old hand-maintained "read before touching" list is
superseded by the `Files:` lines; high-traffic files currently include
`signature_detection.pl`, `drl_composition.pl`, `json_report.pl`,
`generate_kernel_corpus.py`, `enhanced_report.py`.)

**Roll-off rule (monthly, with the CLAUDE.md "Memory Consolidation Review"):** entries
older than ~30 days get the CLAUDE.md promotion test once more, then are **compressed in
place** — keep the header + `Files:`/`Tier:` lines + a 2–4 line verdict + pointers
(commit hash, `audits/<date>_<slug>/`, OQ number); drop the body. Full text stays in
this file's git history; never create a separate archive file (Build Discipline
Pattern 2). `tripwire` entries are compressed only if their warning is promoted to an
always-loaded CLAUDE.md section or superseded.

**Standing warnings lifted into auto-loaded `CLAUDE.md` sections** (the tripwire lives there;
full provenance stays here):
- Green cut `product_site_export.pl:75–77` → `CLAUDE.md` Architecture Invariants.
- Run-tagged subdir glob isolation → `CLAUDE.md` Corpus Loading.
- Corpus is 223 not 3,337 / cite the manifest → `CLAUDE.md` Critical Distinctions.

Entries are roughly chronological. New session findings go here (see `CLAUDE.md`
End-of-Session Documentation Review), not in CLAUDE.md.

---

## 2026-08-20 — LANDED: OQ-310 walked — the §7.4 rule survives with its first witness, and TWO tripwires about what a gate row actually asserts
**Files:** scripts/gate.sh, python/omega_resolver.py, python/check_axis_boundary.py, python/amnesiac_carriage_check.py, python/apparatus_instrument.py, docs/amnesiac_institution/amnesiac_institution_v0_6.md, docs/technical/build_discipline.md, docs/technical/omega_resolver.md, CLAUDE.md, ISSUES.md
**Tier:** tripwire

`audits/2026-08-20_oq310_gate_row_walk/`. Neither half of §7.4's falsifier fires. The rule gains
**one** independence-screened, non-retrodictive witness — the `apparatus` row RED on a
*present-but-unparseable* `**Fired:**` bit (2026-08-19), by an arm authored **2026-08-10**, eight
days before the rule existed. Freeze order: prereg `49988021c27e0b46063f5c830d971637` → prior
`8f2380b58824641d6afefc2bf3f5b3ca` → classification `c6604cc6bd0625253788045179a6035e`.

**TRIPWIRE 1 — `axis boundary`'s gate row runs `--selftest` ONLY** (`scripts/gate.sh:126`). The
live reachability sweep — the arm that would catch a **new committer→observer read** — is **not in
the gate**. A green `axis boundary` row means *the detector discriminates*, NOT *the code is
clean*, and reading it as the latter is the silent mistake. **Six of 26 rows are instrument-only**
and assert nothing about the substrate: `python env st`, `omega selftest`, `claim cites st`,
`axis boundary`, `cli selftest`, `tripwire hook`. Adding the live arm was deliberately NOT done
here.

**TRIPWIRE 2 — never publish a literal control count; derive it.** `omega_resolver.py:1040` printed
`all positive controls fired (10/10)` as a hardcoded string against **9** numbered controls,
diverging at `4e423165` (2026-06-14, 7 controls under an `8/8` banner) and staying wrong for **67
days** while the row read GREEN every run. Fixed at `45e06430`: each control registers itself and
the banner prints `len(ran)`, witnessed two-sided (9/9 → 10/10 with a planted control → 9/9
restored). The literal was also removed from `CLAUDE.md` and `docs/technical/omega_resolver.md`
rather than corrected in place — a corrected constant re-seeds the same drift.

**CORRECTION-KEY — how the shape counts may be cited.** Stratum 1 (26 exit-coded gate rows):
**18 invariant / 7 mixed / 1 value**. Stratum 2 (141 printed integrity lines in executable audit
artifacts, from 228 candidates): **117 value / 23 invariant / 1 mixed**. **Opposite distributions
by stratum — do not transfer a generalization from one to the other.** §7.4's premise *"most of
them check values"* is FALSE of the gate rows; the one clean value check is
`amnesiac_carriage_check.py`, the checker written for §7.4, whose docstring calls itself
*"INVARIANT-asserting"* ten lines above describing a value check. Counts are as of HEAD `58039b6a`,
2026-08-20 — re-count, never recall (the gate grew 22 → 26 in two days).

**The pre-registered strata-disagreement prediction came out BACKWARDS**, and that is the finding:
the exit-coded stratum produced the witness while `partition_check` — a non-exiting `echo` —
printed `186 == 185` and needed a reader. Promoted to `build_discipline.md` as **State an invariant
AND exit on it**: the invariant makes the failure legible, the exit code makes it unmissable, and
stating one does not buy the other.

**A third cell §7.4's binary has no room for: an invariant that FALSE-FIRES** — the OQ-242
frozen-corpus md5-of-md5s fingerprint hashes `md5sum`'s output *lines*, which carry path strings, so
a relative-vs-absolute mismatch reads as *"corpus moved."* Catch/miss is not the whole space.

**Instance 12 ADMITTED** (`fde20893`): §7.4's self-contradicting gate-row count is the twelfth
numbered instance; the property is restated **eleven of twelve** at all five sites; `7.4 numbered
rows` 11 → 12 landed in its own commit after scoring, with the classification md5 beside it.
Screen 0, calibrated over the eleven derivation instances, **eliminates 0 of 11** — the
pre-registered "base is inflated" outcome is scored NEGATIVE.

**And a self-instance this pass committed inside its own instrument:** the first stratum-2
enumeration command silently excluded `partition_check` — the rule's ONLY positive — because its
token set required `== [0-9]` and `partition_check` compares against a shell variable. Caught by
asserting a known member **before** pinning the command. (There are **two** `partition_check` lines
in the repo; §7.4 cites one.)

## 2026-08-20 — LANDED: OQ-328 resolved, all eight V04 residue rows in the paper — and row A's zero survived enumeration while its stated REASON did not
**Files:** docs/amnesiac_institution/amnesiac_institution_v0_6.md, ISSUES.md, audits/2026-08-18_appendix_b_discharge/crosswalk_v04_to_v06.md, docs/technical/swipl_load_path_and_probe_gotchas.md
**Tier:** correction-key

**All eight rows landed** (`audits/2026-08-20_oq328_v04_residue/`, commits `db06fc79`,
`60e6bac0`, `1e5f3e26`). Rows 8/9+10/23/28(b) into §7.3, row 11 into §5.4, row 19 into §7.4,
row 22-A at §6.2 + Appendix B. Carriage check 15/15 throughout; gate GREEN at every package
boundary; every pinned count unmoved (`5.4 correction count` 1, `7.4 numbered rows` 11,
`7.4 property restated` 2, `8.2 framing carried` 3).

**How row 22-A's result may be cited — it split.** The zero (*§6.2's row A: no taxonomy member
expresses Wu's class A*) is now a **tested absence** in §7.8's terms: eight indices enumerated
against the criterion, 0 of 8 environment-indexed, and the reason is better than the row's own —
every member of this taxonomy indexes on a **layer of the value's journey**, Wu's A indexes on
**which machine the journey is running on**, so no member of a layer-indexed set can express an
environment-indexed class. Cite that (axis-level) rather than *no member* (recognition).

**But the row's stated REASON is falsified and must not be cited.** The cell claims *"this
institution has no dev/prod split of the relevant kind."* It has at least four, all in the
project's own always-loaded rules:

1. **OQ-57 — suite load path vs pipeline load path.** A wrong-module-qualified call **threw** on
   the suite path and **silently resolved** through `json_report.pl`'s user-imports on the
   pipeline path, producing correct-by-accident drift events **for months**
   (`swipl_load_path_and_probe_gotchas.md` §1: *"the two paths witnessed opposite behaviors for
   the same line of code"*). This is Wu's class A almost verbatim, polarity inverted.
2. **Fresh worktree vs main checkout** — gitignored `outputs/` makes pre-computed-value probes
   *"read empty/stale and look fine"* (CLAUDE.md, already typed Pattern-6 there).
3. **`[stack]` REPL vs `run_pipeline`** — MaxEnt unfitted, reads **fail soft** (OQ-66).
4. **Edit/Write tool channel vs Bash channel** — the `PreToolUse` matcher is `Edit|Write` only.

The institution has the incidents; it has no *index* for them. **The gap is axis-level, not
incident-level.** Corrected in the paper in the house form, with the against-interest paragraph
left intact above the correction.

**The cell's SECOND clause was mistyped on the same axis error, and the first landing missed it**
(caught on operator review, same day; both clauses now retired). The clause offered *regime
boundaries and corpus resets, typed **Type A drift**,* as the analogue. §2.8 glosses **Type A as
*"the framing expired"*** — spec-vs-code drift, an undated corpus, repaired by as-of stamps: a
**temporal** discontinuity. Wu's A is **locational**. A time-indexed analogue for a
location-indexed class is the identical confusion, one clause earlier, as its own remedy.
**The four splits are Type B** (*two framings contradict inside one system*), and the typing is
confirmed by a route nobody chose for it — Type B's declared repair is *machine-enforced invariants
in the standing gate*, and OQ-57's actual **2026-06-04** repair was `prolog/check_stack.pl` run as a
standing baselined command. **How this may be cited: the gap NARROWED and did not soften.** Do not
cite it as *the institution cannot see these failures* — it can, under the trifurcation. Cite it as
**the eight-index taxonomy has no index for what the institution's own trifurcation can type.**

**A correction to the governing plan, refused rather than executed.** The plan reported that
`crosswalk_v04_to_v06.md:69` and ISSUES OQ-328 quote a phrase absent from the paper — *"the
losses are independent"*, 0 hits — and directed both citations be fixed. **The phrase is
present** at §5.4, split across a hard wrap inside a blockquote; line-oriented grep returns 0,
the wrap-safe normaliser returns 1. **Both citations are correct and were left untouched.**
This is **wrap-trap instance 7** — the sixth of the *storage-form* species a normaliser closes;
item 31's *paraphrase* instance holds number 6 (2026-08-20 correction-key entry below draws that line). Notable only for
where it happened: in the plan written to discharge item 31's class, with the fix prescribed
four sections below the finding that needed it. Evidence + two-sided probe control:
`audits/2026-08-20_oq328_v04_residue/phantom_quotation_refused.md`.

**§7.4 stated its own gate-row population twice, with different values.** *"twenty-one gate
rows"* and, seventeen lines later, *"twenty-two"* — unmarked, undated, inside the sentence
registering OQ-310, live count **26**. Marked and dated with the count carried as-of; **whether
it is a twelfth §7.4 self-instance is deferred to OQ-310** and deliberately not decided at the
landing, because `7.4 numbered rows: 11` is itself a value-checking gate row of exactly the class
the rule under test predicts is blind to self-instances.

**Row 19 carries no 11 → 12 obligation.** The unit-06 leak was a **catch**, not a self-instance
— §7.4's table enumerates defects committed inside repairs — so it landed as prose with the
running tally (two untracked paths, one real leak, **zero of five instrument defects**).

---

## 2026-08-20 — TRIPWIRE: "no session log exists" is true of the REPO and false of the harness — 110 transcripts sit outside it, and KNOWN_STATE.md is a DAY log
**Files:** docs/amnesiac_institution/amnesiac_institution_v0_6.md, KNOWN_STATE.md, ISSUES.md
**Tier:** tripwire

**Before re-verifying the paper's sessions/instances denominator, or editing §6.1's ancestor
table, read this.** Appendix B row 1.1 has been re-verified twice as `[UNWITNESSED]` — *"no session
log exists"* — and **both re-verifications checked the repository and stopped at its edge.**
`~/.claude/projects/-home-scott-bin-structural-dynamics-model/` holds **110 session transcripts**
(`*.jsonl`, one per session). The datum is partially recoverable; it is just not in git.

**The silent mistake this prevents** (made on 2026-08-18, recorded in OQ-309 as *"blocker
re-verified: still none"*): concluding from a repo-only search that the denominator is
unobtainable. It is unobtainable **before 2026-07-19** and recoverable after — session starts
extracted from the files span 2026-07-19 → 2026-08-20 (26 July / 84 August, 23 session-days), while
git records **1,221 commits in 2026-02-01…2026-07-18**, so the pre-window absence is the harness's
retention policy, not inactivity. The store is machine-local, unversioned, and **decaying**: the
recoverable stratum shrinks silently, which is why the counting half should not wait on the ruling.

**The second half of the tripwire: do not call `KNOWN_STATE.md` a session log.** The paper's §6.1
ancestor table maps *"dated, tiered session log"* to the laboratory notebook while Appendix B says
none exists — a collision in the **word**, not in the facts. Measured: this file carries 320 entries
over **67 distinct dates**; across the overlap window (2026-07-19…2026-08-20) it has **24 dated
days** against **110 sessions on 23 session-days**. It tracks days-on-which-something-landed almost
exactly and **undercounts sessions ~4.6×**. It is a day log. A reader who takes "session log" at
face value concludes row 1.1 is fillable from the repository, which is how the row survives.

**Also: `sessions` ≠ `instances`** (subagents, compaction, `--resume`), the store is one machine,
and publishing anything from it is governed by **OQ-279's tier rule** — whose tier-2(a) is ambiguous
about whether a windowed session total is a pure aggregate or a work-pattern disclosure. That
ambiguity is the operator's to settle and is flagged at the OQ, not resolved here.

Full record, controls on the probe, and the three dispositions: **ISSUES OQ-331** (minted today,
`splits_from OQ-309`).

---

## 2026-08-20 — TRIPWIRE: the PreToolUse tripwire hook matches `Edit|Write` ONLY, so every Bash-driven edit bypasses it silently — and auto mode instructs Bash-first editing
**Files:** .claude/settings.json, python/pretooluse_tripwires.py, ISSUES.md, KNOWN_STATE.md
**Tier:** tripwire

**Witnessed this session.** `.claude/settings.json`'s PreToolUse matcher is exactly `Edit|Write`.
Every ISSUES.md and paper edit in the OQ-280 pass was made with `python3` heredocs and `sed` through
the **Bash** tool, because the session's auto mode instructs *"make file changes with sed, heredocs,
or short scripts, rather than using the dedicated Read, Edit, or Write tools."* The hook therefore
never fired once, across four commits touching the repository's two most tripwired files.

**What was missed, concretely.** Simulating the payload the hook would have received shows it was
holding the exactly-relevant warning, first in its list:

```
$ echo '{"tool_name":"Edit","tool_input":{"file_path":"ISSUES.md"}}' | python pretooluse_tripwires.py
KNOWN_STATE.md has 64 standing entries for ISSUES.md…
  KNOWN_STATE.md:48  [tripwire]  2026-08-20 — TRIPWIRE: minting an OQ at the tail of `ISSUES.md`
                                  overwrites the footer's opening line, and no gate sees it
```

That entry was written **the same day**, and OQ-329 was minted at the tail of `ISSUES.md` a few
hours later without it ever being delivered. The footer survived only because the mint anchored on
`*Last updated:` and inserted *before* it — the prescribed move, arrived at independently.
`/usr/bin/grep -c 'Add new items with sequential OQ-NN' ISSUES.md` returns **1**.

**Two-sided control on the instrument, so the miss is not confused with a broken script.** The
script is fine and discriminates: it FIRES on `ISSUES.md` (64 entries) and on `README.md` (3), and
DECLINES silently on `LICENSE` (0 entries). The failure is entirely in the **matcher**, one level
above the code that works.

**Why it is silent, and why it earns a tripwire.** Hook silence is documented in `CLAUDE.md` as
meaning *"queried, matched nothing"* — a reassuring reading that is exactly wrong here, where the
truth is *never queried*. An instance editing through Bash gets the same absence of output as an
instance editing a file with no warnings, and cannot tell the two apart. This is the repository's
own Pattern 6 (measured-empty vs didn't-look collapsing at a channel boundary), living in the
apparatus built to prevent it, and reached through a *mode setting* rather than through any code
change — which is why no gate, sweep or selftest could have caught it.

**Do this instead, until the matcher question is ruled.** When editing `ISSUES.md`, `KNOWN_STATE.md`,
or any high-traffic engine file **through Bash**, run the query the hook would have run:
`python3 python/known_state_status.py --file <path>` — before the edit, not after.

**THE NEAR-MISS CAME OUT CLEAN BY LUCK, AND THAT IS THE POINT — `Fired: latent`.** Nothing
downstream was corrupted. The footer survived because the OQ-329 mint happened to anchor on
`*Last updated:` and insert above it — the prescribed move, arrived at independently and without
ever seeing the prescription. So `/usr/bin/grep -c 'Add new items with sequential OQ-NN'
ISSUES.md` returning **1** is a **success-shaped reading with no control behind it**, and a later
grep will produce exactly that reading on its own. It is not evidence the channel works. Recorded
explicitly because "the footer is intact" is the sentence this incident will otherwise leave behind.

**OPERATOR RULING 2026-08-20 — do NOT fix the matcher first; make the hook's silence LEGIBLE, then
rule coverage.** Adding `|Bash` was correctly rejected (a Bash payload carries no
`tool_input.file_path`, so the script would have to parse arbitrary shell for targets — a new
false-negative surface on a channel whose entire failure mode is false negatives). But the
alternatives considered alongside it — narrow the auto-mode instruction, or change hook semantics —
**all accept the framing that COVERAGE is the problem, and it is not, or not first.** The problem is
that this hook **has no call-witness**: *fired-and-matched-nothing* and *never-queried* emit
identical output, which is the whole reason a tripwire written that same day sat dark through four
commits against the file it was written about. The repository already minted the discipline for
exactly this — `build_discipline.md` → *A control must witness that it is CALLED* — and did not
apply it to its own hook.

**The ruled build (OQ-330).** Have the gate derive **from git** whether files matching the hook's
own matcher changed in the commit range, and compare that against a **per-session fire count the
hook writes**. Zero fires against a changed `ISSUES.md` is a **red row**. No shell parsing, no
workflow change, and it is **matcher-agnostic** — whatever coverage is eventually chosen, the
counter says whether coverage matched reality. **Coverage then becomes a second, cheaper ruling,
made by looking at the gap instead of inferring it from a near-miss.**

**A SECOND INSTANCE THE SAME SESSION, and it is the same shape one level down: a partial check
passing where the full gate caught it.** Consolidated here rather than filed separately, because
both are what the undelivered tripwire would have spoken to.

| # | what was run | what it said | what the FULL gate then caught |
|---|---|---|---|
| 1 | `issues_status --check` + `omega check` after the OQ-294 flag | 328 parsed / 0 malformed; 0 problems | `displaced cites` **RED** — the pasted TSV witness quoted `Pattern 3`, so an artifact excerpt read as two undeclared consumers of the renumbered member (`d98e0679`) |
| 2 | `issues_status --check` after the ISSUES.md row-swap | 328 parsed / 0 malformed | `omega index` **RED** — the derived router had gone stale under a script edit that never regenerated it |

**A THIRD instance, different shape, same session — and it fired while writing THIS ruling up.**
Minting OQ-330 quoted the footer check's own `grep -c` literal, which drove that check's count from
1 to 2: an entry *about* the footer check registering *as* a footer line. Same mechanism as row 1
above (a pasted TSV witness reading as a citation), and the same mechanism `claim_cite_check`
sentinel-wraps its own example for. **Rule: whenever a counted population includes prose, quoting
the counter is an edit to the count — cite the check, never reproduce its literal.** Caught and
reworded before commit; the count is back to 1.

**The shared mechanism, stated so it is checkable:** a narrow checker passes **on the dimension it
owns**, while a *different* row — one the same edit also perturbed — goes red. `issues_status` owns
status-grammar; it knows nothing about citation namespaces or router freshness. **So a narrow green
is not a subset-witness for the gate, and running one is not a cheaper version of running the
other.** Both instances arrived through the same door as the hook finding: `ISSUES.md` edited
through Bash, in a session where the apparatus that would have said so could not fire. `[GATE]`
before committing anything touching `ISSUES.md`/`KNOWN_STATE.md` is already the standing rule; this
is what it costs to skip it twice in one session.

## 2026-08-20 — LANDED: OQ-280 resolved by amend; the RQ2 retirement carried into the paper at four sites; OQ-294's premise flagged; OQ-329 minted
**Files:** docs/amnesiac_institution/amnesiac_institution_v0_6.md, docs/amnesiac_institution/README.md, ISSUES.md
**Tier:** landed

Commits `d49a41bb`, `d98e0679`, `d8bb9522`, `6f8c014d`. Gate GREEN at each.

**OQ-280 — amend, phrased historically** (operator ruling). §4.3 takes one marked correction:
the retrospective classification left no artifact, so §5.1's patterns rest on named exemplars
(§4.4) and not on a coded corpus; and *"a weekend of work"* is retracted in the same block. The
correction is historical on a ruling — a coded corpus arriving later is an **addition** to the
record, not a correction of the correction.

**The re-scoping, which is the part that will look like an error to a future reader.** OQ-280's
literal 2026-08-10 wording (*"no file assigns a P-label … as a data row"*) was overtaken **one day
later**: `audits/2026-08-10_oq277_rq2_crosscoding/packets/iii_prime_units/*.json` and
`.../controls/anchors.json` carry `true_label` fields — **n=10**. They are **not** a counterexample:
every one records its label as *"Read off, not assigned"* in its own `label_source`, so they derive
FROM the taxonomy rather than evidencing it. Both paths are named inside the closed entry precisely
so compression cannot strip them.

**The retirement was ruled 2026-08-12 and the paper carried it NOWHERE for eight days.** Now at
four sites — §6.2 (its four PROPOSED rows have no named settling experiment, and it is not a
resource deferral), §14 (a new dated block **beneath** the `[COST CORRECTED]` box, box text intact),
§4.3, and Appendix D.5. The D.5 colophon was amended against the drafting default on the operator's
argument: it **instructs** successor drafts rather than describing, so it is the one site a v0.7
drafter would act on.

**Two plan figures did not reproduce, and the measured ones were used.** The plan said four content
edits had landed against `README.md`'s *"pointer-only … no content edits"* policy; `git log` shows
**nine** since `96db0124` (2026-08-14). The plan also predicted a pre-existing `gap surfaces` red;
the gate was **fully GREEN** at baseline, so nothing red could be attributed away.

**A pasted witness is shaped like a citation.** The OQ-294 flag's sample TSV rows quoted
`Pattern 3` and turned gate row `displaced cites` RED — two undeclared consumers of the renumbered
member. Fixed by choosing rows carrying no renumbered index (`d98e0679`), not by declaring an
artifact excerpt in the consumer manifest; same shape `claim_cite_check` sentinel-wraps its own
example for. **This was one of TWO partial-check-passes-where-the-full-gate-caught-it in this
session; both are tabled in the sibling tripwire entry above** (the hook-matcher one), because both
arrived through the door that entry describes.

## 2026-08-20 — TRIPWIRE: minting an OQ at the tail of `ISSUES.md` overwrites the footer's opening line, and no gate sees it
**Files:** ISSUES.md, python/issues_status.py, issues/INDEX.md
**Tier:** tripwire

**Witnessed.** `ISSUES.md`'s footer began mid-sentence — *"resolved items with a status change and
a resolution note rather than deleting — provenance matters.*" — with no opening. `git log -S`
locates the loss at **`e673f4c8` (2026-08-19, OQ-327 minted)**: the new entry was inserted *over*
the line `*Last updated: 2026-08-10. Add new items with sequential OQ-NN labels. Mark`, which is
the last line before the footer block and reads like body text at a glance.

**Why it is silent, and why it is worth a tripwire.** `python/issues_status.py --check` parses
`## OQ-` headers and `**Status:**` lines; **it does not parse footer prose**, so the file lost a
standing instruction and the gate row stayed green through two sessions. The deleted line was the
instruction *for the very operation that deleted it* — a new-OQ author is exactly the reader who
needed it and exactly the actor who removes it.

**Do this instead.** Mint by anchoring on the footer's own text and inserting *before* it (the
`*Last updated:` line is the boundary), not by appending to the tail or matching the last body
paragraph. After any ISSUES.md edit, `/usr/bin/grep -c 'Add new items with sequential OQ-NN'
ISSUES.md` must return 1. Restored 2026-08-20 (`b86803db`), stamped to the current date.

**Flagged, NOT done — a genuine ruling.** The structural fix is an invariant assertion in
`issues_status.py` (footer opener present, exactly once), which is cheap and is the
invariant-over-value move the apparatus argues for. **It was not made**, because `issues_status` is
one of the gate rows **OQ-310** is registered to walk as an *unedited* population, and converting a
value-checking row into an invariant-asserting one perturbs the very distribution that falsifier
tests. Sequencing, not reluctance: land it after OQ-310's walk, or rule that the walk records the
edit as a declared perturbation.

## 2026-08-20 — LANDED: OQ-287 BOTH LIMBS — the practice paper is extracted, and Limb 2's whole justification was an exposure that was never created
**Files:** docs/practice/practice_paper_v0_1.md, docs/practice/README.md, docs/amnesiac_institution/amnesiac_institution_v0_6.md, docs/amnesiac_institution/README.md, docs/concealment/concealment_without_a_concealer_v0_4.md, audits/2026-08-13_oq287_defork/EXTRACTION_PROMPT.md, audits/2026-08-13_oq287_defork/checks.sh, audits/2026-08-10_oq277_rq2_crosscoding/LETTER_2026-08-11_wu.md, ISSUES.md, AGENTS.md
**Tier:** tripwire

**Five things a fresh agent would get silently wrong.**

1. **THREE papers now.** `docs/practice/practice_paper_v0_1.md` (the practice) joins the concealment
   paper (the derivation) and v0.6 (the institution). Each directory has a `README.md` naming its
   canonical file; the ordering is acyclic; none restates another.

2. **v0.6 §2.8/§2.9 are the SUPERSEDED side.** They keep their numbers, §2.9 keeps its (a)/(b)
   letters, and both carry forward pointers to practice §III/§V. **Cite the practice paper for that
   material.** §7–§10 did **not** move — v0.6 stays canonical for them, and citing *those* as
   superseded is the mirror error, equally wrong.

3. **v0.6 admits CONTENT edits, marked — not "pointer-only."** That sentence stood in the README and
   in the paper's front matter while nine content-edit commits landed against it. The README was
   corrected 2026-08-20; **the paper was not, until today** — a ruling that landed at one site and
   not its sibling, which is the same failure v0.6 §14 records against the RQ2 retirement.

4. **Limb 2's justification was WRONG, and the shape is worth more than the fix.** The entry said a
   sub-item redirect table was owed because the Wu letter cites `§2.9(b)` and *"cannot be edited."*
   **Uneditability generates no repository obligation.** The exposure that would have justified it —
   a reader following `§2.9(b)` out of a **published** artifact — required the letter to be a
   published appendix, and **that designation was never executed** (v0.6's appendices are A/B/C/D;
   the letter occurs once, as a repo path). A true premise carried a false conclusion for eight days
   and had a calendar deadline attached to it. Retired with its reason; **the reversion trigger lives
   in the letter's own header**, because the person contemplating the promotion is looking at the
   letter, not at the tracker.

5. **`EXTRACTION_PROMPT.md` §3 was headed *"what moves, from where"* over rows that mostly do not
   move — and it had already moved a real receiver.** One word in a heading produced a live
   `§9.2` "exposure" in a plan. Now split into **3a MOVES** (exactly two rows, with a mechanical
   membership test) and **3b DRAWS ON**. The citation rule **inverts** across the two tables, and
   the old single bullet had it backwards for half the material.

**TWO INSTRUMENT DEFECTS, BOTH CAUGHT BY A CONTROL ARM AND NEITHER BY READING — and both would have
biased the result toward the author's preferred verdict.**

- **A check that could not fail.** New arms added to `checks.sh` row 3 were written against `$norm`,
  which is `normalized()`'s **whole-file** collapse to one line — so both phrases matched elsewhere
  in the paper and the arms were green no matter what §2.9's marker said. A two-sided control fired
  **PASS twice** where it should have gone red. Now scoped to the extracted marker block, with a
  vacuity guard; controls A/B/C each discriminate. *The original `at sub-item granularity` arm had
  the same whole-file scope and was falsifiable only by accident — that phrase happened to be
  unique.* **Before adding an arm to that file, check whether `$norm` is the right scope.**
- **The acceptance test's own probe was wrong twice.** Its control arm reported anchors ABSENT from
  the **intact** paper: once from a capitalisation mismatch (pattern copied from v0.6's `EXCLUDE`
  against a paper writing it lowercase), once from the **wrap trap** (a phrase straddling a hard
  line-break, invisible to a line-based grep; normaliser added — the fix for the *storage-form*
  species, and explicitly **not** for the paraphrase one). Read on the test arm alone, each ABSENT is
  indistinguishable from the deletion having worked.

**NO CHECKER NAMES THE PRACTICE PAPER, and v0.6's forward pointers had no far-end check.**
`amnesiac_carriage_check` covers v0.6 (15 invariants) and `checks.sh` covers v0.6 + concealment;
**`practice_paper_v0_1.md` is named by neither.** Its only coverage is `claim_cite_check`, which
finds it **by construction** (that checker scans the whole repo) — coverage by accident of design,
not by enrolment. Consequence, now fixed: v0.6 §2.8/§2.9 pointed at `practice_paper_v0_1.md`
§III/§V and **nothing asserted those existed** — renumbering §III dangled both markers with the gate
green. **Pattern 1 on the pointer substrate, committed by the pass that built the pointers**, because
the pointers were written after the check was. `checks.sh` row 3 now asserts the destination file,
§III, §V, the canonicity README, and that both markers name that path — **addressability, not
content**, so a closed audit does not become a live checker for a document it does not own. Controls
G/H/I wired into its selftest; G/H are the free git pair (destination absent at `c3667f75`, present
at `HEAD`). **If you add a fourth paper, check what names it before assuming the gate covers it.**

**The acceptance condition was tested by a REAL DELETION and is MET.** 37 + 11 + 63 lines cut from a
scratch copy; four anchors absent, eight surviving references dangling.
Witness: `audits/2026-08-20_oq287_limb1_extraction/dangle_count.sh` + `deletion_test_arm.md`.

**OQ-287 is RESOLVED.** Limb 1's landing falsified `concealment_..._v0_4.md:34` (*"declared-temporary
pending the practice paper"*); routed to the operator rather than silently repaired, and **RULED the
same day: amend.** Two reasons worth carrying:

- **The edit was arrangement, not content** — V7 had already classified `:34` as naming *the
  destination, not the content*, which puts it with the provenance lines and outside everything
  concealment v0.4 is canonical for. No claim, digest, or reviewed argument is touched.
- **The prohibition is RECEIVER-SCOPED, and this misread has now been caught TWICE.**
  `EXTRACTION_PROMPT.md` §1b forbids **the receiver** editing v0.4 *while executing the extraction*
  — a write-lock during a task, not a property of the file. Structurally identical to the `:242`
  *"pointer-only edits, which is all v0.6 admits"* misread that OQ-287 resolved on 2026-08-17: same
  prompt, sibling paper, same shape. **Both catches were made by the operator, not by the instance
  reading it — so treat a task-scoped restriction quoted without its task as a live hazard; the
  third occurrence is the one nobody will notice.**
  **The fix is in the ARTIFACT, not the reader.** This is not a wording defect a third careful pass
  fixes; it is **a reading an instance reliably produces from that wording** (twice, two different
  sentences, same prompt, sibling papers, different readers). Reading it as an authoring slip invites
  a third local correction — the wrong response, wearing the right response's clothes.
  **Pre-registered escalation:** a third trip is worth a **structural change to how task-scoped
  write-locks are phrased repo-wide** — each states its scope *in the sentence that states the
  restriction* — minted as its own OQ, **not** folded into a local fix. Kill condition: if the third
  is caught by the *instance* rather than the operator, the reading is not reliably produced and this
  downgrades to an authoring note.

**Accepting the stale sentence was the weaker branch on a substantive ground, not a tidiness one:**
*"pending the practice paper"* does not age, it becomes **affirmatively false** — it tells an external
reader the practice paper does not exist.

**THE REPOSITORY CANNOT ANSWER "HAS CONCEALMENT v0.4 GONE OUT," AND A FRESH AGENT WILL THINK IT
CAN.** Three facts, third one load-bearing: the circulation **GO ruled 2026-08-19 covers
`amnesiac_institution_v0_6.md` ONLY** (OQ-309 — Appendix B + the V04 manifest), and no GO or send is
recorded for v0.4 anywhere; OQ-309's *"no recipients"* line **forecloses its own reuse in its own
text** (*"a fact about 2026-08-12, not a standing property… if the question returns, it returns
live"*) and is *"an attestation, not a checked distribution log"*, about a different paper; and **the
repo holds no distribution log at all.** Positive control: the probe *does* find a circulation-status
line where one exists — `LETTER_2026-08-11_wu.md`'s annotation header — and **that letter is the only
artifact in the repo carrying one.** So the absence for v0.4 means *no channel exists*, not *nothing
was sent*. A `**Circulation:**` note was added to v0.4's header recording what is **recorded** rather
than what is **true**. **Declared residue: if a copy did leave, the repo copy now diverges from the
circulated one and owes a header note riding to the next version bump** — R-A applied to concealment
exactly as it was applied to the Wu letter.

**Two plan premises that were stale at execution, reported as observed.** OQ-280 was **already
resolved** (`d8bb9522`) rather than in flight, so its `blocked_on` edge was deliberately not authored
— it would record a satisfied constraint as live. And the plan predicted a 14-row gate; the observed
baseline is **26 rows**. Line pins had all drifted (§2.8 `:590→:628`, §2.9 `:632→:670`, §4.3
`:988→:1026`, KNOWN_STATE `:1731→:1766`).

Records: `audits/2026-08-20_oq287_limb2_discharge/WRITEUP.md` and
`audits/2026-08-20_oq287_limb1_extraction/WRITEUP.md` (both `Fired: live`). Commits `71de3b67`
`c3667f75` + this one.

## 2026-08-20 — CORRECTION-KEY: a PARAPHRASE false absence is a distinct species from the wrap-trap class, and no normaliser fixes it
**Files:** docs/technical/build_discipline.md, audits/2026-08-18_appendix_b_discharge/crosswalk_v04_to_v06.md, docs/amnesiac_institution/amnesiac_institution_v0_6.md
**Tier:** correction-key

**How prior results may be cited.** `crosswalk_v04_to_v06.md`'s **item 31 verdict was WRONG** and is
corrected in place (original quoted, not overwritten). It published *"Git is not identified as the
cross-type instrument"*; §3.3 identifies it — *"One instrument works across every row: version
control and dated records… Every instrument in the table is amnesia-type-specific except that
one"* — and has since v0.6's first commit (`1265d0c1`). **The row was never residue.** Any citation
of the walk's "ten open rows" or of item 31 as absent is superseded; the corrected count is
**eight** (OQ-328).

**The mechanism, and why it needed a new name.** The probe searched the literal string `cross-type`
against a claim made *in other words*. Wrap-trap instances 1–5 are **storage-form** false absences
(hard wrap, blockquote markers) and a normaliser closes them; this is a **paraphrase** false absence
and **the normaliser is no help — it returns the same 0 while raising confidence in it.** Filed at
`build_discipline.md` → *A textual probe's zero is a fact about the probe*, with the disjoint-fix
table. **Scope rule that generalises: an absence verdict licensed by keyword hits alone is scoped to
the keywords, and the keywords are the author's, not the document's.**

**What caught it: a positive control, not a re-read** (`"sixteen texts"` = 1, itself wrapped and
blockquoted, so the normaliser was exercised, while `cross-type` = 0). Same shape as the same OQ's
audit-directory row, where only *re-running the command* — not re-reading the row — showed the value
came from a different instrument than the command beside it.

**Second correction in the same document.** The crosswalk's roll-up published the **superseded**
row-count regex `'^\| [0-9]+ \|'` beside the number **35**. That regex returns **32**; the correct
form is `'^\| [0-9]+ (‡ )?\|'` (the three `‡`-marked rows escape the old one). The pass that wrote
the crosswalk is the same pass that *fixed* this regex in the manifest — number carried from the
corrected run, command from the defective one, nothing re-ran the pair. Both sites corrected
2026-08-20 (`da6de5b2`).

## 2026-08-19 — LANDED: the 18-ruling BLOCKED-ON-YOU session — and its two recurring shapes, recorded ahead of the individual rulings
**Files:** ISSUES.md, python/issues_status.py, python/sunset_check.py, python/apparatus_instrument.py, audits/INVESTIGATIONS.md, docs/design/design_gaps.md, docs/technical/build_discipline.md, docs/commitment_systems/commitment_systems_sketch_v6.md
**Tier:** correction-key

All 18 BLOCKED-ON-YOU items ruled 2026-08-19 (each second-instance reviewed; per-ruling records
in the OQ entries and per-commit witnesses). **The thing worth recording is not the individual
rulings but that FOUR of them turned on the same distinction — an UNTESTED instrument vs a
FAILED one (OQ-127's 0-TP-by-no-material; OQ-276's no-decline-ever; OQ-292's unrecheckable
disposition; OQ-281's corpus-scoped zero) — and THREE MORE on whether a stated trigger could
actually FIRE (OQ-295's prevention-invisible ratchet; OQ-297's unfireable branch (b); OQ-299's
re-discovery window).** Those two shapes were the session's real content, and they are a
reasonable pair of opening questions for any future ruling pass: (1) is this zero a tested
absence or an untested instrument? (2) can this trigger's falsifying branch actually fire, and
who is reading what when it does? Mechanisms minted for them: `sunset_check.py` (dated
obligations gate-enforced), named read-sites (undated triggers), `audits/INVESTIGATIONS.md`
(registration making `no` reachable), the `**Disposition:**` sub-field (retraction countable).
OQ-291's landing also produced two live catches of its own instruments: the checker's first run
flagged five legacy `**Disposition (…)**` prose headings (regex tightened), and the ledger
counter counted its own header examples (fencing-aware fix, two-sided control pasted).

## 2026-08-19 — LANDED: OQ-317 trigger enforced — sunset_check.py gate row; the disposition itself deliberately NOT ruled early
**Files:** python/sunset_check.py, scripts/gate.sh, ISSUES.md, docs/technical/build_discipline.md
**Tier:** landed

Operator ruling (second-instance reviewed): the 2026-11-17 socket disposition is NOT ruled early
(ruling today would foreclose the window set 2026-08-18 or renew by fiat); instead the passive
trigger is made ACTIVE. New gate row `sunset` (`python/sunset_check.py`): scans allowlist
`REVIEW-BY` tokens AND ISSUES `**Sunset:**` lines on active entries — two carriers on purpose so
the October row-removal cannot silence the November socket review; fires ON the day (same-day
boundary is a selftest control, 5/5); licensed responses pre-committed (review, or extend BY
RECORDED OPERATOR RULING, never a silent date edit). OQ-317 carries `**Sunset:** 2026-11-17`,
the blocked_on_human is discharged as not-ripe, and the November desk is told plainly that the
allowlist containment is CALLER-scoped while the definition stays unguarded. General rule minted
at build_discipline.md → *A passive trigger never fires — date it and gate it, or name its
read-site* (the session's four passive triggers cited; STANDING candidate).

## 2026-08-19 — LANDED: OQ-285 resolved as GAP-40 (no expressive-capacity instrument); the fresh-instance gate CAUGHT the entry's own wrong binary
**Files:** ISSUES.md, docs/design/design_gaps.md, docs/commitment_systems/commitment_systems_sketch_v6.md, audits/2026-08-17_oq285_mode3_measurement_arm/STEP2_WEB_REVIEW.md, audits/2026-08-19_oq285_mode3_measurement_arm/
**Tier:** landed

OQ-285's three-step gate ran to completion and WORKED: the independent re-derivation found the
entry's own FAILS-vs-unknown binary wrong (three routes; the only populated route (c), 1,934
seats, is a produced reading, not an absence) while the concept's live carrier
(extraction_blindness, 134/279) sat on the surviving side of the laddered filter — false-absence
sub-rule (c) instantiated and caught by the gate built for it. Disposition: GAP-40 declared
absence, NO code (signature-joined token killed by measurement); category
unsupported-on-evidence, not inadmissible; routes (a)/(b) zero is CORPUS-SCOPED (decoy-grade
control: detector works, corpus has no population — does not survive a rebuild as a claim about
the new corpus); cheaper routes enumerated with measured statuses; passive revival hinge with its
read-site named (CS sketch §2.5 pointer added). Claude-web step-2 check (endorse + 10
amendments) persisted verbatim beside the 2026-08-17 WRITEUP, which stands point-in-time. Both
audits ran on code_dirty trees — numbers not reproducible from a hash. Sketch changelog row
updated. Sweep: the falsified plan-claim ("signature layer never converts unknown") propagated
nowhere (positive control fired on the WRITEUP's own quotation).

## 2026-08-19 — LANDED: OQ-242 ruled as a principle — distance components compute only over measured inputs; purity 0.5 midpoint retired; OQ-327 minted for the sibling fallbacks
**Files:** prolog/context_profile_mining.pl, ISSUES.md, audits/2026-08-19_oq242_absence_semantics/
**Tier:** landed

Operator ruling (second-instance reviewed), recorded as a PRINCIPLE: a distance component is
computed only over inputs that were measured — absence drops the component and renormalizes,
never substitutes a value. `normalize_purity/2` (0.5 for both OQ-60 absence tokens, live on
56/279 rows) retired from `stability_distance/3`; `purity_scored/1` guard (number/1 first).
Graduation step run BEFORE the ruling: family partition invariant (21/4 byte-identical; +1 twin
pair 2828 vs 2827) on frozen corpus n=279 — ruling free by the entry's own criterion. Ruling
record keeps the (c)-symmetry argument (absence-as-max-distance = same defect, sign flipped,
more expensive because it moves rows) and the renormalization arity limitation. Sibling
fallbacks (coupling→0.0 identical, boltzmann-inconclusive→0.5, preservation catch-all→1.0)
split to OQ-327 — application of the settled principle, per-site occurrence count + own
diff-pair owed; boltzmann `inconclusive` is a measured abstention, whether "absence" covers it
is part of that adjudication. Commits `a440a310` (code+audit, split-and-first) + this one.

## 2026-08-19 — LANDED: OQ-127 ruled — SDZ demoted to advisory ((b)+(c) compound), untested-not-disproven citation rule, passive revival
**Files:** python/linter.py, ISSUES.md
**Tier:** landed

Operator ruling (second-instance reviewed): SCAFFOLD_DANGER_ZONE demoted from error grade to
advisory with reworded text ("authoring-time predictor of a perspective-dependent gate"; points
at OQ-127, no maintained claims hardcoded); stays in `THRESHOLD_COUPLED_LINT`, OQ-116 chokepoint
untouched (`test_deleak_chokepoint.py` 4/4 PASS post-change). Ruling record carries three
amendments in the OQ's still-operative block: (1) the OQ-221 row-10 "0 TP" is n=0 by NO MATERIAL
(PROPOSED-capped zero-D rule), so it is untested-not-disproven and may not be cited for a future
disarm; (2) the revival condition (witnessed genuine gate misfire re-promotes) is declared
PASSIVE — no surveillance path exists, accepted; (3) the ruling rationale on record is
error-grade protection (71% FP at error grade trains reflex-dismissal of error lines generally),
not attention economics. Witness commit + OQ-127 resolved entry.

## 2026-08-19 — LANDED: OQ-302, the bound-`false` call at `boltzmann_compliance.pl:577`; and the spec that prescribed it
**Files:** prolog/boltzmann_compliance.pl, python/dispatch_head_check.py, prolog/codewalk_caller_allowlist.txt, docs/logic_extensions.md, docs/logic_thresholds.md, docs/noether_implementation.md, docs/lawvere_implementation_notes.md, docs/lawvere_glossary.md, docs/grothendieck_framing.md, docs/v8/foundations/logic_extensions.md, docs/v8/foundations/logic_thresholds.md, docs/technical/build_discipline.md, audits/2026-08-19_oq302_bound_false_repair/
**Tier:** landed

`boltzmann_invariant_mountain/2` clause 1 called `epistemic_access_check(C, false)` with `false`
BOUND — the idiom the file's own header warns against 79 lines above (`:470–477`, written
2026-06-03 when the sibling `structural_purity/2` instance was repaired and this one was not
swept). The catch-all unifies with everything, so clause 1 fired for every constraint and the
four-test body had **never executed on any corpus**. Repaired to
`once(epistemic_access_check(C, S)), S == false, !` (`fb10708a`).

**Measured before landing** (`audits/2026-08-19_oq302_bound_false_repair/`; prereg md5
`c7a7345c…` frozen before the probe was written; six legs — testsets 279 counted at run time,
haiku 960, flash 960, kimi 1005, sonnet 1001, kernel_v1 1106 = **5,311**; every leg's md5
identical before and after both arms):

- *fires* control: `arm(defect)` = `inconclusive(insufficient_data)` **5311/5311**. Reported as
  **a wiring check with a known answer, NOT discrimination** — it follows from head unification,
  which is the finding itself.
- *declines* control (the two-sided one): **753/753** genuinely access-insufficient constraints
  still report `inconclusive` post-repair. Subject present on every leg; `testsets_sonnet` n=1,
  declared thin.
- **Verdict stays constant in KIND; payload does not.** `invariant(_)` is unreachable —
  `T4 = fail(natural_law_signature)` on **5311/5311**, because `has_viable_alternatives/2`'s
  range is `{true, unknown}` (OQ-113). `N_reaching` = 4,558, giving **129–270 distinct
  `(T1,T2,T3)` tuples per leg** where every constraint previously got the same token.
- Throws: **0**. Transcription cross-check: 4,558 match / 0 mismatch.
- OQ-137 totality 10/10; `run_dynamic_suite` GOOD; gate GREEN.

**Blast radius zero live consumers** (OQ-38 census rows 110/620 both `STATIC_ORPHAN`), so the
pipeline clean-vs-edited pair is byte-identical by construction and was deliberately not run —
declared in the audit rather than left looking like an oversight.

**Three things a later reader should not have to re-derive:**

1. **`probe_harness:with_overlay/3` CANNOT REPORT THAT IT OVERLAID NOTHING → minted as OQ-326.**
   It snapshots FACTS only (`probe_harness.pl:91–100`, `clause(M:T, true)`); a template matching a
   RULE produces a *warning*, not an error, and asserted facts land AFTER the existing clauses —
   so an overlay "counterfactual" on a cut-ordered rule silently measures the unmodified program.
   An empty snapshot for ANY other reason (wrong arity, undefined predicate, unloaded corpus,
   absent id) is equally silent. **The harness verifies RESTORE; nothing verifies INSTALL** — so
   an overlay pair is not a witness unless the probe asserts, inside the overlay, that the change
   took effect. The plan specified this mechanism; it was refused and replaced with
   `clause/2`-fetch-and-call of the engine's own clause body (`PREREGISTRATION.md` §0a).
   **Retroactive census DONE (OQ-326 Phase 1), and it clears the record:** 44 call sites / 27
   files / 13 distinct retract-side templates; 12 rule-free; the 1 rule-bearing template
   (`constraint_indexing:constraint_classification/3` at
   `audits/2026-06-07_stakeholder_layer_migration/a1_probe.pl:77`) is safe by BINDING — its rule
   clauses are hard-keyed to the two engine demo constraints. **No prior audit is voided.** The
   live trap is that `probe_harness.pl`'s OWN header example is the unsafe form of that same call
   (first argument unbound). Method note for a re-runner: parse ARGUMENT POSITIONS — a
   functor-proximity grep flags goal-body predicates and produced six rule-bearing false
   positives. Evidence:
   `audits/2026-08-19_oq302_bound_false_repair/overlay_template_census.md`.
   **`clause/2` on a static predicate is legal only while `protect_static_code` is `false`** (SWI
   default; `access_level=user`), and the refusal path is real — a foreign built-in still raises
   `permission_error`. That idiom is **n=1** and deliberately lives in gotchas §12, not CLAUDE.md.
2. **The spec prescribed the defect.** `docs/logic_extensions.md`'s "How to Activate" instructed
   `boltzmann_invariant_mountain(C, true)` — unsatisfiable against every result shape. Corrected
   at the origin (`4f8f0e3f`), together with `logic_thresholds.md:260` (Stage-7 enhancement now
   marked **unimplemented and dark**, not deleted) and four docs that described T4 as working.
   `docs/v8/` was **annotated, not rewritten** — it is v8 source material for OQ-135, so the
   erroneous text is preserved with the correction beside it. New sub-shape appended to the
   Pattern-7 incidence ledger in `build_discipline.md`.
3. **`latent-B` is 0 → 1 again**, one day after OQ-303 recorded it empty. `epistemic_access_check/2`
   left `finding` for `latent-B` (`5f9ec36d`), which turned `codewalk caller` RED — the arm found
   a bound caller the Phase-0 enumeration had adjudicated but the class label denies. It is
   `boltzmann_compliant/2` at `:94–95`, bound-**`true`**, safe by head unification and now
   allowlisted with `ATOMS=true` on a naturally-arising two-sided record: it declines on exactly
   the 753 the unbound call declines on and fires on exactly the other 4,558. The row is a
   genuine class-B **conversion** candidate (output last arg, reached, 5 callers on live output
   paths) → routed to OQ-303 arm (a); conversion owes the six-leg pair, not the template.
   **And the count edit did NOT reach a published OQ-303 conclusion.**
   `audits/2026-08-18_classb_conversion_rollout/clause_order_census.md:19` — *"No latent-B
   predicate carries a nonzero steal-risk at any atom"* — was a set-level claim over a set that
   has since gained a member. Re-running that arm's own tool (its pre-registered naturally-arising
   control firing first: `signature_grade/2` correction=0, commentary=1 at `6c1bfa44`) makes it
   **FALSE: 1 of 1**, `epistemic_access_check/2` at atom `false`, steal-risk 1, skipped `[true]`,
   zero at `true`. **A set-level claim is re-witnessed by RE-RUNNING the instrument, not by editing
   the count the claim was computed over.** The 2026-08-18 dir was not edited (point-in-time,
   restored byte-identical); re-run filed at `oq303_steal_risk_recensus.md`. That standing
   condition — a `latent-B` predicate stealable at `false` — is tracked at OQ-303 arm (a), not
   only as the allowlist's scope justification.

---

## 2026-08-19 — class-B conversion rollout: the worklist premise was unchecked, and the fact that would have caught it was in the mode comments
**Files:** python/dispatch_head_check.py, prolog/signature_detection.pl, prolog/abductive_helpers.pl, prolog/boltzmann_compliance.pl, prolog/report_generator.pl, prolog/logical_fingerprint.pl, python/run_pipeline.py, audits/2026-08-18_classb_conversion_rollout/
**Tier:** tripwire

**The tripwire, for anyone converting or auditing bound-dispatch predicates.** The conversion
template (fresh-variable heads + unify-after-cut) is valid ONLY where the last argument is an
OUTPUT. Applied to a predicate whose last argument is an INPUT the caller supplies, the first
clause matches every call, cuts, and **renders every later clause unreachable** — silently, and
with every structural check still green. Witnessed: converting the 55-row batch moved 129/279
`testsets` up to 1106/1106 `kernel_v1`, attributed by per-file bisect to
`abductive_helpers.pl seat_overrides/2` and `boltzmann_compliance.pl expected_power_divergence/4`.

**And the fact was already authored.** Both carried a hand-written mode line three lines above
their clauses (`%% seat_overrides(+C, +Signature)`,
`%% expected_power_divergence(+P1, +P2, +T1, +T2)`) — last argument `+`. **No instrument in the
chain read `%%` mode lines**: not the regex caller sweep, not the codewalk arm, not the
clause-order census, not the cut-first screen. Each answers a version of "is this called with its
last argument bound?", and each presupposes the last argument is an answer.
`dispatch_head_check.pl:9-11` states that assumption in its header; a header is where a violation
sits indefinitely, and the checker's own worklist held two.

**Now checked, not assumed:** `dispatch_head_check.py` carries `LAST_ARG` — one row per registry
entry, its verdict (`output` / `input` / `generator`) and the evidence that settled it. A
`latent-B` / `unreached` / `generator` row with no fact is RED; one recorded `input`, or a
`generator` filed under another class, is RED. **`latent-B` is now EMPTY.** Two new classes:
`unreached` (called zero times on all six legs — a different fact from "no bound caller", with a
different remedy) and `generator` (never cut-ordered dispatch; its caller enumerates it).

**Second tripwire — a conversion loop needs a completeness check that does not come from the
loop.** The driver reported 0 failures while a missing trailing newline made `while read` drop
its last row, with a six-leg run already loading the half-converted file. A truncated list makes
a smaller loop that succeeds completely; only an out-of-loop re-scan caught it. General form:
*the instrument that reports success cannot be the instrument that verifies coverage.*

**Also landed:** `classify_corpus` now sizes its swipl ceiling from the corpus
(`_classify_timeout_for`); the old fixed 300 s was sized on the live leg (~35 s) and cost three
full-length attempts then a refusal on the twins (flash 530 s, sonnet 734 s, kernel_v1 581 s).
21 predicates converted, each with a six-leg pair at 0 changed constraints over 5,311.
Detail: `audits/2026-08-18_classb_conversion_rollout/WRITEUP.md`; tracker OQ-303, OQ-325.

## 2026-08-18 — TRIPWIRE: `prolog/schema_shape.txt` is anchored on the DECLARATION set (63), not on the 40 corpus-schema rows
**Files:** prolog/schema_shape.txt, python/module_boundary_check.py, prolog/module_boundary_allowlist.txt, prolog/narrative_ontology.pl, prolog/scenario_manager.pl, scripts/gate.sh
**Tier:** tripwire

**The silent mistake this prevents:** adding a `:- multifile narrative_ontology:P/N` or `:- dynamic
narrative_ontology:P/N` declaration ANYWHERE in the repo without adding a `schema_shape.txt` row in
the same change. The gate goes RED and the message is clear, so this is loud once you run it — but
the tripwire is for the step BEFORE that: **do not assume the register is the 40 `ROLE=corpus-schema`
allowlist rows.** It is the repo-wide resolved declaration set, 63 members, and 23 of them have no
allowlist row *and correctly should not*. Adding a corpus-schema row for one of those 23 to "fix"
a perceived gap breaks arm E's derivation check, which enforces `row IFF a story file writes that
qualified head`, keyed name/arity (`measurement/5` says nothing about `measurement/2`).

**Second silent mistake:** the register is COMPUTED by scanning every non-tests module, never read
from a list. Do not "optimize" it into a named-module scan. Measured: a `narrative_ontology.pl`-only
anchor finds 57 and misses six — `measurement/2` and `intent_fact/4` (`scenario_manager.pl`),
`cs_authority_grounding/2`, `cs_interpretation_layer_present/1`, `cs_kernel_codification/2`
(`cs_pattern_detection.pl`), `cs_kernel_id/2` (`cs_kernel_registry.pl`). That totality is the whole
escape-removal guarantee; a named list re-opens the opt-in escape invisibly.

**Third:** `--full` is RETIRED. `--check` scans all five legs and is a strict superset. It still
accepts `--full` with a note rather than dying, so an old invocation works.

**What is enforced vs documented:** 54 of 162 declared argument positions are checked by arm F
(closed `{a,b,c}` sets, `number`, `text`); the other 108 (`open`/`cid`/`atom`/`compound`) are
documentation. Do not cite a green arm F as evidence that an `open` position was validated.

**And arms F and G are DRIFT RATCHETS, not specifications** — transcribed from the corpus as it
stands. Green means the schema has not changed unnoticed, not that it is right.

Provenance: OQ-308 (resolved), `audits/2026-08-18_oq308_schema_shape/`, commits `c2aa6a67`,
`3d9c221c`, `1db3ba01`, `1b2f9d9c`, `6d0e020f`. Residues: OQ-321/322/323/324.

## 2026-08-18 — CORRECTION-KEY: how the OQ-308 numbers may be cited, and three plan counts that did not reproduce
**Files:** prolog/schema_shape.txt, python/module_boundary_check.py, ISSUES.md
**Tier:** correction-key

**Citable as measured (2026-08-18, HEAD `d0caef57`→close):** register 63; 40 written / 23 rowless;
15 multi-module members (10 `scenario_manager`, 4 `cs_axiom_engine`, 1 `data_repair`); 162 declared
argument positions, 54 enforced; 60,642 distinct authored values compared; 4,205 story files over
five legs (279/960/960/1005/1001); allowlist 117 rows; selftest 50.

**Three counts in the OQ-308 plan are WRONG and must not be re-cited:**
1. *"only 3 readerless: `coupling_profile/2`, `input_vector/2`, `theater_ratio/2`"* — it is **five**;
   `attribute/3` and `intent_fact/4` join them. Verified with a positive control (the same probe
   fires on `update_authority`).
2. *"a named-module scan gives 61; the delta is `cs_axiom_engine.pl` (4) and `intent_fact/4`"* —
   `cs_axiom_engine`'s four are ALSO declared in `narrative_ontology.pl`, so excluding that module
   loses DECL sites and **zero members**. The real delta of a `narrative_ontology`-only anchor is
   **six members from three other modules**. The escape is wider than the plan estimated.
3. *"arm H fires on two real in-tree sites (`constraint_claim/3`, `measurement/2`)"* — neither is an
   arity skew; both arities are genuinely declared, so arm H never sees them. Arm H's one live
   finding is a different fact: `tests/axis_boundary_ctl_payload_widen.pl:12` calling
   `cs_axiom_foreclosed/2`, which resolves at **no** arity anywhere, and is a deliberate fixture.
   The exemption registry is therefore `SCHEMA_ARITY_EXEMPT`, **not** `SCHEMA_ARITY_ALIASES`.

**Also did not reproduce:** the plan's `--full` timing ("4.3s / 36s; the 1.4s/13s comment is stale")
measured 1.35s/14.4s at open — the existing comment was nearly right. And the `PATH=` prefix the plan
called moot was already absent from `gate.sh`.

**How arm E's control quality may be cited.** Arm E has two checks at two altitudes and the strong one
does not cover the weak one. Anchor resolution: **naturally arising, two-sided** (57 vs 63, declining
on nothing). Derivation: **fixtures only, and unavoidably so** — the rule was derived from the
substrate it checks, so an in-tree positive is unavailable BY CONSTRUCTION, the same circularity as
arm F's declared sets. Cite the derivation half at fixture altitude, never at the anchor's.

**Step 0 is the pass's only falsifier.** Correction (b)'s IFF rule replaced a first rule that was
wrong; the register design rests on it. A non-empty disagreement set on the 40 means the FOUNDATION
is broken, not a row — re-derive the rule, do not repair rows. Measured 0/0 on sets, committed at
`c2aa6a67` before any authoring.

## 2026-08-18 — LANDED: three parser defects in `module_boundary_check.py`, one of them a disarmed control
**Files:** python/module_boundary_check.py
**Tier:** landed

1. **Bracket blindness** (`1db3ba01`). `_scan_args`'s `saw` flag was not set by `[` or `]`, so a term
   whose arguments are all bracket structures — `adjacent_pairs([], [])` — scanned as arity **0**.
   32 of 29,447 engine open-parens; 17 phantom `/0` entries left `defined_preds`, 0 true arities were
   added (each predicate's other clauses already supplied them). **Latent, not live:** no reported set
   moved, but the phantoms are load-bearing for `closure_arity`, whose first branch is
   `if (pred, 0) in pool: return 0`.
2. **A clause-body GOAL counted as a clause HEAD** (`1b2f9d9c`). 810 occurrences across 270 files, all
   `constraint_metric/3`, all inside plunit test bodies. Arm C never noticed — it asks only whether
   the pair appears, and that predicate has 20,895 genuine heads, so the key set and every per-file
   count are unchanged. Arm F **would** have: it harvested the Prolog variable `ExtMetricName` as an
   authored value of argument 2.
3. **An existing control was DISARMED by fix 2, not broken by it.** The naive-parser fixture's only
   discriminating element (`ghost_pred` in a block comment) became preceded by `:` rather than `.`,
   so the clause-start guard excluded it from BOTH sides and the two parsers agreed. It would have
   stayed green while testing nothing. Repaired with a second head at a genuine clause start, and
   re-checked to actually differ. **Its label was also wrong** and is corrected: it read "mis-parses
   the multi-line fact", but the multi-line fact parses identically under both strippers — the block
   comment was always its whole content.

**The general lesson, which is why this is `landed` and not `history`:** a change that tightens a
parser can silently disarm a control that depended on the looser behaviour, and a disarmed control is
indistinguishable from a passing one. When tightening a shared parser, re-check that every fixture
depending on it still DISCRIMINATES — not merely that it still passes.

## 2026-08-18 — OQ-311 Item 1: §2.3's type-concentration claim withdrawn as unwitnessed; range-robustness survives
**Files:** docs/observers_not_humans_v6.md, python/sweeps/range_sweep.py, docs/project_orientation.md, docs/project_orientation_web.md, docs/lawvere_glossary.md, ISSUES.md
**Tier:** landed

`observers_not_humans_v6.md` §2.3 fused two claims; only one had a witness. Commits `5d548413`,
`ec860a2e`, `2a52e1f0`; `audits/2026-08-18_oq311_universality_class/` (`Fired: live`).

**Withdrawn (unwitnessed, NOT refuted):** "+0.21 in tangled_rope (N=2,245) vs +0.014 in snare+rope
(N=1,169)", the 14.6× figure, "concentrates in a single constraint family". Two independent grounds.
**(i), sufficient alone:** the cited witness `outputs/range_sweep_results.json` is produced by
`python/sweeps/range_sweep.py`, whose entire output is
`{arm_a_jacs, arm_b_jacs, mean_ab_gap, max_span_drop_a, max_span_drop_b}` — `jaccard_stats()` takes
two *whole* presheaf id sets and returns one scalar, no code path subsets either by type or
condition, and the dict is **byte-identical across all four commits** in the file's history. The
citation was never satisfiable. **(ii), corroborating:** 2,245 + 1,169 = 3,414 > 3,380 (file count =
JSON `"total"` = **measured** `corpus_constraint/1`) and > 3,314; the tracked census gives rope 55 +
snare 571 = 626 vs a published 1,169. **Survives:** Jaccard 0.6966–0.8327 over six forms, witness
re-pointed from the gone `outputs/` path to the tracked `python/alt_power_transform_results_3k.json`.
H0's scope OPEN at OQ-311 Item 2 (prereg authored in full, **unfunded**, gated on operator spend-go).

**`range_sweep.py` repaired:** `:24` named `archives/prolog_v5`, deleted, so the sweep threw
`corpus_empty` at HEAD. Repaired to `archives/datasets/original_v6`, justified by a **byte-identical
rename in all 3,380 cases** (3,380/3,380 R100). Load check discriminates: repaired exit 0 / "Loaded
3380", pre-repair exit 2 / `corpus_empty`.

**Two method notes worth more than the finding.** (1) **A count gate on a marked correction is the
wrong instrument.** The marking principle *requires* the block to quote the withdrawn sentences, so
the sweep count necessarily rises (7 → 15) and a count gate fails against its own baseline — with
the obvious fix being to delete the quotes, exactly what the correction forbids. A **property**
gate (every hit inside or adjacent to a dated marker, checked *per line*) caught six stale sites a
"15 > 7, pass" would have shipped. Count gates are meaningful only where the expected value is zero.
(2) **The sweep pattern used alternation, so under BRE `|` is a literal and the whole sweep returns
clean** — a false negative across every file. Demonstrated same-pattern/same-file: **BRE 0, ERE 7**.
Use `-E`, and pin `/usr/bin/grep`.

**Also repaired, kept separate from the withdrawal** (different predicate, different claim):
`lawvere_glossary.md:31` pinned `metric_based_type_indexed/3` at `drl_core.pl:356`; it is `:532` at
HEAD, and OQ-22 pinned it at `:479–483` on 2026-06-28 — three positions, which is the argument for
citing by predicate. The rope-clause bypass moved likewise: `:356` → `:384` → **`:432`**
(`classify_from_metrics/6` clause `:426–436`).

**Follow-on, same day — OQ-320 (filed and closed).** The correction above rewrote
`project_orientation.md` §5.5's *Summary* with v6 content and left its heading naming v5, i.e. **one
edit made that section internally inconsistent** — a different defect class from drift, since it has
a named author and a dated commit, and it reads as ordinary staleness unless the tracker says
otherwise. Repaired: §5.5 retitled/restatused to v6; the `:430–526` pin **verified WRONG at both
ends** (began 25 lines after `derive_directionality/3`, ran ~40 past the last
`power_role_heuristic/4` into `exit_modulation/2`) and re-cited by predicate; `d_zero ≈ 0.1642`
**verified at HEAD** by deriving it from live config (`0.50 − ln(7.5)/6.00 = 0.164183`); `N=3,314`
labelled corpus-bound. **A close readback sweep found two more sites the OQ never named**, including
web-facing `project_orientation_web.md:61`, which called v5 canonical *and* asserted "the
universality-class framing" the same day it was withdrawn. **A doc-currency repair is swept by
claim, not by filename.** The `[STALE HEADING]` marker was pulled in the same change that repaired
its defect — a marker outliving its OQ becomes furniture, and worse, points at an unfindable
resolution (the `docs/open_questions.md` shape).

**Promotion test: NO PROMOTION.** Each failure here is **loud**, not silent — a dead corpus path
throws `corpus_empty` (exit 2), and a withdrawn claim is visible in the paper. The standing
tripwires this rides on are already always-loaded in CLAUDE.md (cite-the-manifest-not-a-memorized-
count; `asserta` not `assertz` for `corpus_path`; pin `/usr/bin/grep` in any script computing a
reported count). Bias to history.

## 2026-08-18 — TRIPWIRE: `prolog/testsets/` carries no literal file count anywhere; the four static legs may
**Files:** CLAUDE.md, AGENTS.md, ISSUES.md, docs/project_orientation.md
**Tier:** tripwire

**Operator ruling 2026-08-18.** `prolog/testsets/` is the leg the operator works in continuously —
topic runs land stories mid-session — so **any literal count for it is stale the moment it is
written, while still reading as authoritative.** Do not publish one in CLAUDE.md, AGENTS.md, or any
standing reference; count it when you need it (`manifest.n_constraints` from the run's own
`pipeline_output.json`, or `ls prolog/testsets/*.pl | wc -l`) and state the date alongside.

**The four other legs are STATIC and their counts ARE usable** — `testsets_haiku` 960,
`testsets_flash` 960, `testsets_kimi` 1005, `testsets_sonnet` 1001 (disk-verified 2026-08-18). If a
piece of work needs a concrete corpus size, take it from one of those, not from the live leg.

**The drift that produced the ruling:** CLAUDE.md published `testsets 259` stamped "disk-verified
2026-08-12". On 2026-08-18 — six days later — it was **279**, and the stamp still read as verified.
The stale figure sat in an always-loaded file that is read at every session start by something with
no way to check it. Corrected in the same change; the static-leg figures were kept because they are
safe for the reason the live-leg figure is not.

**Dated measurements in closed OQs and audits are NOT covered by this** — an OQ body citing "0/276
live" is a historical witness of what was measured then, and rewriting it would falsify the record.
The rule governs *standing reference* counts, not evidence.

## 2026-08-18 — OQ-296 closed: consumers of the dead natural_law/coordination_scaffold detector made honest
**Files:** python/linter.py, python/extract_corpus_data.py, python/container_typology_analysis.py, prolog/signature_detection.pl, prolog/dirac_classification.pl, prolog/reading_registry.pl, prolog/domain_priors.pl, prolog/cs_pattern_detection.pl, prolog/signature_mapper.pl, prolog/abductive_helpers.pl, prolog/maxent_classifier.pl, prolog/context_profile_mining.pl, prolog/isomorphism_report.pl, prolog/constraint_bridge.pl, prolog/data_validation.pl, prolog/axiom_reachability.py, python/shared/maxent.py, docs/design/design_gaps.md, ISSUES.md
**Tier:** landed

OQ-296 resolved at CONSUMER-HONESTY altitude (commits `d989520c`, `a8aa7284`, `65230466`,
`fe64033d`; `audits/2026-08-18_oq296_consumer_honesty/`). Operator ruling D1: keep the unpowered
sockets, make the readers honest — no repair-by-authorship, no retirement — **bounded by a sunset**
(OQ-317, re-review 2026-11-17, retire-by-default, burden on whoever wants to keep).

Shipped: linter advisory rescoped (it cited a mechanism stale since `966d53c8` and never named the
binding conjunct, so it instructed authors down a path that cannot work); provenance siblings on
`is_constructed` and an input-provenance stamp on `container_candidates.json`; 18 verified dark
declarations, comment-only; `test_oq113_dead_natural_law.pl` extended 3→8 green; the `domain_priors`
expectation trio retired with its table preserved as **GAP-38**. Spawned OQ-313…OQ-319.

Witnesses: kill-condition re-witness (HALT gate) green; `per_constraint` byte-identical across the
clean-vs-edited pipeline pair (exit 0 + mtime advanced both halves, corpus md5-frozen per leg);
`[stack]` load 3 warnings/3 allowlisted/0 unexpected after the deletion.

## 2026-08-18 — TRIPWIRE: `outputs/container_candidates.json` is served from a 2026-05-16 artifact over a RETIRED corpus
**Files:** python/container_typology_analysis.py, python/container_typology_recon.py, outputs/container_typology_recon_data.json
**Tier:** tripwire

**Do not read any value in `outputs/container_candidates.json` as a measurement of the live corpus.**
Every field — `type_distribution`, `signature_distribution`, `mountain_pct`, `natural_law_pct`,
`mean_extractiveness`, orbit stats, all derived axes — is computed from
`outputs/container_typology_recon_data.json`, whose mtime is **2026-05-16** and whose own
`total_constraints` is **3369**: the chimera-era `original_v6` corpus (ID reuse across runs, OQ-25),
not the live post-reset corpus (n=279). **Neither script is wired into `run_pipeline.py`**, so the
artifact has never refreshed across the 2026-06-05 reset. Pattern 1, *consumed-once is not
kept-fresh*.

**The silent mistake this prevents:** `natural_law_pct` reads up to **0.9808** there
(mathematics 68/72, mathematical_logic 51/52). At HEAD that signature is 0-firing by construction,
so the file reads like a falsifier hit against OQ-113/OQ-296 — it is not, it is pre-reset data. A
`_input_provenance` stamp was added 2026-08-18 stating exactly this; it is the first thing to read.
Tracked as **OQ-319**. Lead hypothesis for the recon-era firings (pre-OQ-44 pass-open regime,
`signature_detection.pl:249-255`) is deliberately NOT asserted at the site — it is a reconstruction
of a regime that no longer governs.

**Corollary worth carrying:** the site was on OQ-296's roster as "spot-verified by direct read"
reading a constant zero. It was verified — by reading the CODE. Nobody read the OUTPUT. A code-read
and an output-read are both "direct reads" and only one shows what a consumer serves.

## 2026-08-18 — CORRECTION-KEY: three OQ-296 roster entries were wrong; D2's stated rationale corrected in two places
**Files:** ISSUES.md, audits/2026-08-17_oq251_natural_law_reachability/audit_log.md, python/extract_corpus_data.py, python/reports/queries/classification_audit.py, prolog/diagnostic_summary.pl, prolog/domain_priors.pl
**Tier:** correction-key

**How the OQ-296 / OQ-251 consumer roster may be cited.** 3 of ~20 entries were wrong, all found by
inspecting sites rather than inheriting them:
- `classification_audit.py:61` — validates authored `constraint_claim` values; legitimately non-empty.
- `container_typology_analysis.py:151-157,331` — does not read a constant zero (see the tripwire above).
- `diagnostic_summary.pl:437` — already fully annotated; the "partial note" needing extension was not partial.

**Do not cite the remaining roster entries as verified by this pass** beyond the 18 that carry
edit-time confirmation in their annotations.

**D2's rationale corrected (the ruling survives; two stated reasons were wrong).**
1. *"No third state exists at HEAD"* — false. The **26/279** rows carrying the `unknown`
   honest-abstain signature are a third state; `sig not in ('natural_law',)` reports them as
   asserted `constructed`. Tri-valued is still refused, for a better reason: consumers coerce, and
   `bool(None)` is `False`, so emitting None delivers abstains as asserted NEGATIVES
   (`boolean_independence.py:169`). Half a tri-state into a two-valued consumer is worse than not
   starting. → OQ-318.
2. *D2's site-1 half is superseded entirely* — it reasoned about a `formalization` axis computed on
   live data. The axis is not computed at all; it is served from a frozen file.

**Also corrected:** the plan's "5 of 7 `expected_signature` rows unreachable" is three-layered —
5/7 unreachable by construction, **6/7** unreachable on any authored corpus, and the one surviving
row expects `ambiguous` while `constructed_constraint` (the expectation for four rows) **is not in
the live signature vocabulary at all**.

## 2026-08-18 — [correction-key] Appendix B discharged: the incidence figure is 83/185 (45%) as of 2026-08-18, it carries FIVE instrument defects, and its monthly rate is NON-STATIONARY — so it may not be compared across time
**Files:** docs/amnesiac_institution/amnesiac_institution_v0_6.md, docs/amnesiac_institution/V04_CONSOLIDATION_MANIFEST.md, audits/2026-08-18_appendix_b_discharge/
**Tier:** correction-key

**What changed.** OQ-309 (`partial`), `audits/2026-08-18_appendix_b_discharge/`, commits
`d9941e3e`…`0f8ecf74`. Both halves of the paper's self-declared circulation blocker discharged: every
Appendix B row re-run under one as-of stamp, and the 35-item V04 manifest pass completed by anchor
text (23 landed / 3 partial / 2 superseded / 7 not landed), its one U-BLOCKING item resolved.

**How the incidence figure must now be cited — THE POOLED SCALAR IS WITHDRAWN.** Do not cite
83/185, 45%, 73/174 or 42% as *the* incidence figure; the paper no longer publishes one. **Cite per
window:** June 2026 36/98 (36.7%), July 26/45 (57.8%), August 1–18 16/20 (80.0%). The rate is
**non-stationary**, so a pooled ratio over the observation window measures the denominator's age
distribution as much as the record — which is why the pooled value rose 42% → 45% on eight days of
growth with no command changed. The pooled value survives in Appendix B only as a *computed*
quantity marked not-a-summary, so the old scalar stays locatable. Four qualifications:

1. **The movement is GROWTH, not correction.** +11 directories, 10 of them into the numerator, 0
   removed, 0 numerator members lost. Every prior movement of this figure was a repair.
2. **A FIFTH instrument defect — the precision direction, measured for the first time.** 13 of the
   83 numerator members (15.7%) are *hygiene-only*: their sole keyword hit is this project's own
   reporting prose (*"recorded rather than silently deleted"*), an author describing their own
   discipline rather than a defect they found. That is a **floor over one mechanism**; a second
   false-positive mode (bare *"was never"* in ordinary prose) is visible and unmeasured.
3. **A SIXTH instrument defect, and the fix belongs in the instrument.** The census pipes
   `grep -rl ... audits/` into `cut -d/ -f2`, so a keyword hit in **`audits/README.md`** — the audit
   index, a file at the top level — enters the numerator as `README.md`, a member that is not a
   directory and cannot be in the denominator. There was never a membership filter; the instrument
   was correct only because that file had never used the census vocabulary. **Appending an audit-index
   row describing this very pass put the words there**, the numerator read 84, and the frame's
   `partition_check` printed `186 == 185`. **Tripwire for anyone re-running the §5.4 census: the
   numerator must be intersected with the directory population (`comm -12`), or the command returns
   84 and one of its members is a file.** Published value unaffected: 83/185 stands.
4. **The pooled figure is a mixture over a non-stationary rate:** 36.7% (June) → 57.8% (July) →
   80.0% (Aug 1–18). A pooled value moves when the denominator's age distribution shifts, with no
   correction and no change in the world. **Do not compare this figure across time.** Two readings
   (genuine phase change; lexicon adoption) are stated in §5.4 and neither is picked; the hygiene
   probe bounds one channel of the second and does not carry it (monthly hygiene share 19.4 / 7.7 /
   25.0, no trend).

**The row-level defect worth remembering.** The Appendix B audit-directory row read **174** while its
stated command `ls -d audits/*/ | wc -l` returned **175**: 174 was the *census frame* after
self-excluding an in-progress arc directory, and the scale figure and the frame had been merged into
one cell. A manifest row is the pairing of a value with the command that produces it; this one had
stopped being that, and only *running* the command could show it. Now two separate rows (187 on
disk, 185 in frame). **Related unresolvable, declared rather than smoothed:** §5.4 attributes
175 → 174 to removing an empty untracked directory while the frozen 08-10 frame reaches 174 by
self-exclusion. Both are consistent with 174; the directory was untracked, so git cannot adjudicate.

**A self-check that was published, instructed-to-be-re-run, and never compared.** The V04 manifest's
item-count check read **32 against a documented 35** from the moment §4b landed, because three
`‡`-marked rows do not match `^\| [0-9]+ \|`. The instruction *"re-run after ANY edit"* existed; the
instruction *"and compare the output to 35"* did not. Fixed, with the grade recount promoted from a
prose instruction to a runnable command and `/usr/bin/grep` pinned.

**New standing gate row: `paper carriage`** (`python3 python/amnesiac_carriage_check.py --check`).
It asserts **expected hit counts at enumerated carriage sites** in the paper and fails on a MISS as
loudly as on an EXTRA, so a normalisation bug turns it RED rather than green. **Editing a carriage
site without updating its expectation in the checker's manifest turns the gate red — deliberately**,
the same opt-in-with-teeth shape as `spec_enum_check.py`. It replaced a pattern-grep sweep that
mis-fired twice.

**Tripwire for any textual probe over `amnesiac_institution_v0_6.md` (or any hard-wrapped,
blockquoted markdown): a probe's zero is a fact about the probe.** Five instances of this class
landed in one pass, and the fifth was **inside the instrument built to close the first four** — its
selftest planted fixtures with a plain `str.replace` against wrapped source, every plant silently
no-opped, and the "did the check go red?" assertions passed a document that had never been damaged.
Chasing normalisation variants loses; the fix is an invariant (assert expected counts) plus a
perturbation helper that **raises** when it fails to land. Full class:
`docs/technical/build_discipline.md` → *A textual probe's zero is a fact about the probe*.

**Circulation reframed (operator ruling, 2026-08-18): GO is a jurisdictional act, not a quality
certification.** Nothing about circulating makes the incidence figure more citable — withdrawing the
pooled scalar and reporting per window did that. Do not read the paper's front-matter box as a
quality gate; it states what the artifact is.

**Not discharged, and a cold reader should not assume otherwise.** §10.4's standing-gate catch series
stays `[UNWITNESSED]` (nobody has collected it; the cadence row is a different quantity). §3.5's
truncation row stays `[UNWITNESSED]`, re-pointed from the retracted always-loaded carrier (OQ-286) to
the recall channel (OQ-289). The sessions/instances row stays `[UNWITNESSED]` — blocker re-verified,
no session log exists. **The circulation GO is the operator's call** and is recorded as
`blocked_on_human` on OQ-309.

---

## 2026-08-18 — [tripwire] The OS upgrade moved Python 3.10 → 3.12 and stranded every third-party package; the gate ran the EMPTY interpreter while the work ran in `.venv` — RESOLVED same day: `.venv` is canonical, one resolution point, and a `python env` gate row that derives its own required set
**Files:** scripts/gate.sh, python/python_env_check.py, pyproject.toml, .claude/settings.json, agent/c-orchestrator.py, agent/perspective_experiment.py, agent/uke_narrative_orchestrator.py, python/check_gap_status_surfaces.py
**Tier:** tripwire

**Symptom, and why it under-reports.** `./scripts/gate.sh` shows ONE red for this
(`gap surfaces  ModuleNotFoundError: No module named 'pandas'`). That single row badly
understates the blast radius, because most affected tools are not gate rows — the gate is
green on 21 rows while the import base is missing. Discriminating witness (2026-08-18):

    $ python3      python/check_gap_status_surfaces.py   → ModuleNotFoundError: pandas
    $ .venv/bin/python python/check_gap_status_surfaces.py → 3/3 human surfaces … (self-test OK)

Same file, same arguments, opposite verdicts. **The red is interpreter selection, not
content.**

**Cause.** The jammy→noble upgrade (2026-08-18 ~01:55–02:50, same window as the swipl
package swap — see the sibling entry) moved the system interpreter from **3.10 to 3.12**.
Residue that shows it: `python/__pycache__/` holds **81 `cpython-310.pyc` next to 37
`cpython-312.pyc`**. Everything pip-installed against 3.10 was stranded. Import sites now
unsatisfiable under system `python3`: **scipy ×25, anthropic ×20, numpy ×17, sklearn ×7,
networkx ×3, pandas ×2** (`python/` + `agent/`). The `anthropic` count matters most — that
is the whole generation path (`agent/c-orchestrator.py`), so a topic run under system
`python3` fails at import, not mid-run.

**RESOLVED the same day (operator: "fix the Python gate using .venv and whatever makes
sense for the project"). `.venv` is canonical; the fix is below and the gate is GREEN.**

1. **`scripts/gate.sh` resolves the interpreter ONCE**, not at 22 call sites:
   `$SDM_PYTHON` → `.venv/bin/python` → `python3`, aborting if the winner is not runnable.
   Every row runs `"$PY"`, and the banner prints the resolved path, so a gate transcript
   now says which interpreter produced it.
2. **New FIRST gate row `python env`** (`python/python_env_check.py`, +`python env st`
   selftest, 12/12 controls). It AST-scans `python/` + `agent/` and asserts the *running*
   interpreter can import everything they import. Required set is **derived, not declared** —
   a hand manifest would be a second canonical list (Pattern 2) and would rot. It is first on
   purpose: if it is red, later reds may be downstream of a missing import rather than real
   findings. Two-sided by construction: GREEN under `.venv`, RED under `python3` naming all
   11 missing modules with consumer counts.
3. **Interpreter propagation fixed.** Three sites spawned a literal `["python3", …]` child
   (`agent/c-orchestrator.py:905`, `agent/perspective_experiment.py:477`,
   `agent/uke_narrative_orchestrator.py:2696`), so running a parent under `.venv` handed the
   child the EMPTY interpreter. Now `sys.executable`, the idiom 6 other files already used.
4. **Deps installed into `.venv`:** jsonschema, networkx, diptest, feedparser, nltk.
   `sentence_transformers` deliberately NOT (pulls torch, GB-scale, 2 audit scripts) —
   declared optional, so its absence prints as a note.
5. **`pyproject.toml` reconciled** with the census: it declared 2 modules while the code
   imported 15. Now 5 core + extras `stats`/`ai`/`graph`/`nlp`/`embeddings`. The manifest is
   not the authority — the gate row derives from imports and goes red on drift.

**The one thing left as a memory, so it is checked instead.** The `.claude/settings.json`
hooks still run bare `python3` deliberately — a JSON-string hook cannot reliably resolve a
venv and `$CLAUDE_PROJECT_DIR` may not be cwd. That is safe ONLY while the three hook
scripts (`omega_resolver.py`, `pretooluse_tripwires.py`, `issues_status.py`) stay
stdlib-only. **Adding a third-party import to any of them would break the hook silently in a
fresh shell while every venv-run check stayed green** — so `python_env_check.py` asserts the
invariant (`HOOK_SCRIPTS`), with a planted-violation control proving the detector fires.
If you must add one, fix the hook command in the same change.

**Still operator-owned:** `pyproject.toml`'s `requires-python` 3.10→3.12 + `[build-system]`
+ `[tool.setuptools]` edits and `structural_dynamics_model.egg-info/` were in-flight
uncommitted work (an `pip install -e .`); the dependency reconciliation above was layered on
top of them, not instead of them.

---

## 2026-08-18 — [tripwire] swipl is 10.0.2, not 9.2.9 — and every witness between 2026-06-25 and 2026-08-18 has an UNPINNED interpreter; Mercury was evaluated and rejected, deliberately without an OQ
**Files:** docs/technical/swipl_load_path_and_probe_gotchas.md, docs/trajectory_implementation_notes.md, prolog/giant_component_analysis.pl, audits/2026-08-17_giant_comp_segv_hang/PREREGISTRATION.md, audits/2026-08-17_bound_dispatch_hardening/WRITEUP.md, ISSUES.md
**Tier:** tripwire

**Standing answer, so the question stops costing a search.** "Which OQ is looking into the
Mercury extensions for swipl?" was asked 2026-08-18 and cost a full-repo sweep to answer,
because the ruling lived only in a machine-local plan file
(`~/.claude/plans/if-it-is-recon-mossy-beacon.md`, "the Mercury salvage") and a session
transcript — **the word `Mercury` had zero occurrences in tracked substrate** outside corpus
stories about the planet and the metal. Now landed in three places, routing table in
`swipl_load_path_and_probe_gotchas.md` **§16**:

- **Mercury port: evaluated 2026-08-17, REJECTED, and it gets NO OQ by operator ruling**
  ("an OQ whose resolution is 'no' is a record without a reader"). Three grounds, none
  version-dependent: the dynamic database *is* the architecture (`asserta` overlays,
  `probe_harness`, `cache_registry`, MaxEnt fitted state); the REPL probe methodology has no
  Mercury equivalent (this file's §§2–7 are the workflow it deletes); the LLM co-dev loop
  degrades. Conceded in the same assessment: `build_discipline.md` is largely a
  hand-maintained substitute for a static mode/determinism checker. **Do not re-propose
  pre-beta; do not mint the OQ.** Full text: `audits/2026-08-17_bound_dispatch_hardening/`
  WRITEUP.md → *Applicability verdicts* (appended 2026-08-18 — the plan owed this line and
  it had not landed).
- **The salvage routes to existing OQs:** SSU (`=>`) and `:- det/1` → **OQ-303**, both
  already carrying a scoped NEGATIVE verdict (SSU's fail-loud never fires against a
  catch-all; `det/1` is wrong for legitimately-semidet MaxEnt reads). Incremental tabling →
  **OQ-166**. All three verified working on 10.0.2 with two-sided controls, 2026-08-18 —
  availability was never the blocker.

**The tripwire (this is the silent-mistake half).** `swipl --version` is now
**10.0.2** (`swi-prolog-nox 10.0.2-1-gb8d8f931a-nobleppa2`, installed 2026-08-18 02:50:43
per `/var/log/dpkg.log`, replacing `10.0.2-0-jammyppa2` removed 01:55:39 in an OS upgrade).
Four documents stamp reproducibility claims to **SWI 9.2.9** (`ISSUES.md` OQ-182 P95,
`docs/trajectory_implementation_notes.md:23`, `audits/2026-06-25_oq182_trajectory_revive/`
×2). Those are correct as point-in-time records and must NOT be rewritten — but citing any
of them as "the version we run" is now wrong. Worse: **`/var/log/dpkg.log` is unrotated and
begins 2026-08-18, so the 9.2.9 → 10.0.2 transition point is unrecoverable** — every witness
taken between 2026-06-25 and 2026-08-18, *including the entire 2026-08-17 bound-dispatch
audit*, has an unpinned interpreter. Not a reason to distrust them; a reason that a
version-sensitive result from that window owes a re-run rather than an argument.

**Live consequence — OQ-301 arm F.** Its stated prerequisite ("a source-built swipl 9.3.x")
is **satisfied** by the system 10.0.2; no source build needed. The flip side matters more:
round 1's 7/100 giant_comp failure rate was measured on the older interpreter, so arms A–D
now measure a **different system**. Re-run a round-1-equivalent baseline on 10.0.2 before
reading A–D against round 1's rate, or the comparison confounds interpreter version with the
effect under test — the same single-variable-isolation error OQ-251 closed on.

---

## 2026-08-18 — [tripwire] OQ-68 resolved by write-ownership: accessors where the module asserts, declared bypass where it only hosts — and a corpus-schema predicate is defended by the CENTRAL `:- multifile`, never by its writers
**Files:** prolog/narrative_ontology.pl, prolog/maxent_classifier.pl, prolog/diagnostic_summary.pl, prolog/json_report.pl, prolog/corpus_loader.pl, prolog/constraint_indexing.pl, prolog/grothendieck_cohomology.pl, prolog/context_profile_report.pl, prolog/maxent_diagnostic.pl, prolog/abductive_helpers.pl, prolog/module_boundary_allowlist.txt, python/module_boundary_check.py, scripts/gate.sh, ISSUES.md, AGENTS.md
**Tier:** tripwire

**OQ-68 resolved** (accessors `7e810d52`, gate `917bcc5f`). The ruling and the full inventory
are in ISSUES OQ-68; what follows is what a future editor can get silently wrong.

**THE TRIPWIRE — a `tests/` `:- multifile` defends nothing, and neither does a writer's own.**
`narrative_ontology`'s corpus-schema predicates are written by testset files as qualified
heads. Whether they load COMPLETELY depends on a `:- multifile` declaration existing at load
time, and there are three places one can appear, only one of which works:

| declared in | defends the production chain? |
|---|---|
| `prolog/narrative_ontology.pl` (central) | **yes** — this is the only one that counts |
| a `tests/` file | **no** — the production load chain never consults those files |
| every writing testset (self-declaration) | **accidentally**, until one generator revision drops it |

Two members were living on the third row until this change: `flat_control_of/2` was declared
NOWHERE engine-side, and `has_sunset_clause/1` was `:- dynamic` but never `:- multifile`
(its only `multifile` declarations were in two `tests/` files). Both loaded complete, so
nothing was red — the set was simply not closed.

**The forcing witness** (scratch copy of the leg, local declaration stripped from all 28
loaded `flat_control_of` writers; the ONLY variable is the central declaration):

```
no central :- multifile  ->  loaded 1/28, multifile=no, 27 x "Redefined static procedure"
with it                  ->  loaded 28/28, multifile=yes, 0 warnings
```

That is the `story_provenance/8` tombstone (narrative_ontology.pl, fixed 2026-06-13)
reproduced on demand. **Note the control that does NOT work:** stripping ONE writer's local
declaration changes nothing, because `multifile` is a property of the PREDICATE, set by
whichever file declares it first — 27 self-declaring writers still cover it. The plan this
work executed proposed exactly that one-writer control; it is not discriminating.

**Consequence when you add a corpus-schema predicate:** it needs a central `:- multifile` AND
an allowlist row in the same change, or gate row `module bounds` goes red. Registration is
opt-in — the same silent-escape shape as reading-registry registration and the spec-enum
sentinels.

**Second tripwire — arm C is a TYPO DETECTOR, and it exists because we spent one.** Once a
predicate is `multifile`, SWI stops warning on redefinition. That warning had been doing
accidental duty as the only check that a qualified head in a story file is spelled right.
Closing the schema set costs it; arm C buys it back. So do not "simplify" arm C away on the
grounds that arm B already covers the schema set — they check opposite directions
(B: row → declaration; C: authored head → row).

**The axis has FOUR dispositions, not three (operator, 2026-08-18).**
`diagnostic_summary:maxent_attempted/1` was first logged as a *correction* to the plan's table;
it is better read as a new branch. An outsider writes (`json_report` does the
retractall/assertz), **but the owner enforces an invariant** over what was written
(`maxent_stage_attempted_but_void/2` interprets the marker, so an unrecognised stage would sit
unread and the void gate would report a clean run over it). The hosting test is therefore not
"who asserts" alone but whether the module merely holds the facts or also means something by
them. Disposition: **write accessor, fail-loud** — `maxent_attempt_reset/0` +
`maxent_mark_attempted/1`, no allowlist row. A pure host takes a row; a host with an invariant
takes a write accessor. Do not apply the axis as a three-way branch.

**Three parser defects the checker had before it was trusted**, each now fixture-controlled —
worth knowing if you write another source-text sweep over Prolog: predicate INDICATORS
(`mod:pred/2` in a directive) are not 0-arity calls (they manufactured 20+ phantom rows
naming predicates that exist at no arity); CLOSURES passed to meta-predicates
(`maplist(maxent_classifier:pair_snd, ...)`) carry their real arity elsewhere; and FACADE
modules re-export (`drl_lifecycle` declares an EMPTY export list and `:- reexport`s four
modules, so every call through it looked like a bypass — the first live run reported
`drl_lifecycle:generate_drift_report` as a wrong-qualifier defect, and it is not one).

**Latent defect found by the sweep, recorded not fixed → OQ-307.** `drl_core.pl:710` reads
`constraint_claim(C, theater_ratio, TR), TR >= 0.70`, but `constraint_claim/3` is a view whose
third argument the rule discards, and no `constraint_claim(_, theater_ratio)` fact exists in
any leg — so `type_5_piton_as_snare` has never fired, and would throw on `TR` if it did.

**Five-leg witness, and one declared side effect.** The engine-change rule was discharged by
comparing load counts on all five legs against `5c37b3e4`. Counts are identical on both sides
(`corpus` 279/960/960/1005/1001; `has_sunset_clause` 11/14/8/16/23; `flat_control_of`
28/0/0/0/0). But the four twin legs have **zero** `flat_control_of/2` writers, so there the
predicate went from **undefined** (call throws `existence_error`) to **defined-but-empty**
(call fails silently). Nothing consumes it, so nothing observes it — but the direction is loud
→ quiet, and a future consumer must not read the empty result as "no flat control exists".
Noted at the declaration site in `narrative_ontology.pl` — **and, since a comment does not
survive the arrival of the first consumer, made gate-visible**: `module_boundary_check` arm D
watches `flat_control_of/2` and goes RED the moment it acquires a reference anywhere in engine
code (verified red-capable by plant-and-restore). The arm's green state and its red state are
the two states of the fact it watches, so its two-sidedness is structural rather than
incidental. Per-leg non-emptiness expectations — the part arm D cannot check — are folded into
OQ-308's scope. Two probe lessons worth carrying:
the first version of this probe was non-total and died on exactly this undefined case while
printing nothing a grep would catch, and the second labelled everything `defined` because
`St0=UNDEFINED` binds a *variable*, not an atom — a capitalised-atom slip that made the label
unable to fail. The shipped probe carries its own red-capability control.

**Breadth leg.** `classify_corpus('archives/datasets/kernel_v1', ...)` at HEAD completes clean
at **1106/1106** (`per_constraint` == glob == `manifest.n_constraints`), so all five of its
built-in refusals — zero-glob, load-completeness, single-model fingerprint, raw freshness,
seen==classified — passed with the swapped accessors on the load chain, at ten times the
default leg's size and with an older corpus's reading-sets. The canonical
`outputs/pipeline_output.json` was not touched. **Declared residue: this is a COMPLETENESS
witness, not a diff** — no baseline breadth run was made, so the claim is "the swaps do not
break at n=1106", not "output is identical there". The identity claim rests on the default
leg's byte-identical `per_constraint`. Two gotchas for whoever runs the next one:
`classify_corpus` calls `run_prolog` with the hardcoded default `timeout=300` and kernel_v1
needs more (raise it in a throwaway driver, not in `run_pipeline.py`); and a first attempt
reported `[exited with code 0]` while the Python underneath had raised `TimeoutExpired` — the
`| tail` in the invocation swallowed the exception status, success-shaped absorption at the
shell boundary.

**Census reconciliation, because arm A claims CLOSURE.** Two different bypass counts with no
bridge would leave that claim resting on air. This instrument's own stages:
239 naive → 132 (predicate INDICATORS excluded) → 115 (meta-predicate CLOSURES resolved) → 98
(FACADE reexports resolved) + 18 write-only corpus-schema heads = **116 rows**. The plan's
recon reported **279** and is **superseded, not bridged**: scope does not explain the gap
(including `probsets/` moves this instrument 239 → 242, not 279), so the difference is the
parser, and that recon documented its own arity defects — a mis-arity'd parse inflates the
count of distinct (module, predicate, arity) triples. Do not cite the 279.

**Also witnessed (invocation, not a defect):** a bare `./scripts/gate.sh` reports a FALSE RED
on `gap surfaces` — `ModuleNotFoundError: No module named 'pandas'`, because the row shells out
to `python/query.py` and `gate.sh` calls bare `python3`. Run it as
`PATH="$PWD/.venv/bin:$PATH" ./scripts/gate.sh`. The repo `.venv` has pandas 3.0.5 and the row
is green under it. `module_boundary_check.py` is stdlib-only and unaffected.

**Housekeeping note for the operator:** a pre-existing `stash@{0}` ("WIP on main: 2ad08ed1")
is present in this clone. It was accidentally popped and immediately reverted during this
session (`ISSUES.md` / `issues/INDEX.*` restored to HEAD); **the stash entry itself was not
dropped and is intact**. Nothing of it was kept.

## 2026-08-17 — [correction-key] The cohort-zero stability table's `status: stable` is NOT positive-stable: `victims` is 0/6 positive (its 4 stables are shared ABSENCE), and the cast blast radius reaches classification at NAME-IDENTITY grade via `constraint_captured/1`
**Files:** audits/2026-06-12_cohort_zero/stability_table.json, python/cohort_stability.py, prolog/narrative_ontology.pl, prolog/drl_core.pl, prolog/signature_detection.pl, prolog/constraint_indexing.pl, prolog/logical_fingerprint.pl, prolog/stakeholder_seats.pl, prolog/commentary_census.pl, python/audits/oq190_blast_radius.py, prolog/probe_oq190_edge_admission.pl, ISSUES.md, CLAUDE.md
**Tier:** correction-key

**OQ-190 closed scoped** (`audits/2026-08-17_oq190_blast_radius/`; PREREGISTRATION md5
`875605570f7413d1bf88a56e664f88f3` logged above the first result line; HEAD OPEN `f80bc3eb` /
CLOSE `8c34157f`, read-set diff empty). Headline per the frozen Amendment-A arithmetic:
**SUSPECT = 17 + 221 + 209 = 447 rows across 86 files**; `-and-survives` = 0; `cleared` = 0.

**How prior citations must change (the correction-key part):**

1. **Never read `status` off `stability_table.json` without `agreement_kind`.** A `stable` cell
   whose kind is `absence` was absent in every draw — the gate passed on absence (Pattern 5), not
   on agreement. `base_properties.victims` reads 4/6 stable and is **0/6 positive**;
   `vindicated_propositions`' lone stable is likewise absence. Cast positive-stability is **3 cells
   of 54**, and it is *not* uniform: `roster_card` 2/6 and `time_horizon_multiset` 1/6 are genuine.
   So "cast is 0/6 draw-stable" is a **population** claim, not a per-row theorem — cite it that way.
   Tripwire promoted to `CLAUDE.md` (Generation-is-stochastic block).
2. **The verdict bucket is a different stability regime from the cast bucket** — 7/12 positive-stable
   vs 3/54. A disposition treating "cast/verdict" as one radius gets the arithmetic wrong.
3. **`constraint_captured/1` is a NAME-IDENTITY join on the classification path.**
   `narrative_ontology.pl:332–335` matches `stakeholder_gain_flow`'s receiver against the
   `constraint_stakeholder` roster **by name**, and gates `drl_core.pl:420` (scaffold),
   `signature_detection.pl:1220/1378/1477` and the maxent mirror. The predicted [EDGE] outcome —
   classification is cast-dependent — holds at a *harder* grade than presence/cardinality.
4. **`check_indexical_relativity/1` does not exist** (two prose comments only), and
   `has_mandatrophy_declaration/1` has **zero consumers** — T5b, not a closure hop. Do not go
   looking for that predicate and do not read its absence as an unfinished sweep.
5. **`founding_problem_corroboration_class/2` is 0/4205 authored** across all five live legs while
   having two readers — T5 inert-*unexercised*, the opposite condition from `constraint_vindicates/2`
   (authored, no reader = T5b inert-*unconsumed*). Neither is `cleared`.

**Rejected edges are as citable as admitted ones** (`derivation_graph.tsv`, decided by perturbation
against live controls): `constraint_beneficiary` does **not** reach `constraint_captured` (the join
reads the roster), `disappearance_verdict` does **not** reach `q6_cell` (q6 reads
`founding_problem_status` only), and **neither verdict atom reaches `dr_type`**.

**Limb-3 CLOSED resolved-by-corpus (operator re-reserved 2026-08-17; the ~$1.5–2 reserve is
RELEASED and does not carry forward).** **Tripwire — `story_provenance/8` arg 8 (`sampling_params`)
is a comma-separated `k=v` BAG inside a comma-delimited Prolog term, so a naive arg-split truncates
it at the first comma and yields a plausible, well-formed, silently wrong value.** This audit's own
first probe did exactly that, dropped every temperature term in a compound value, and inverted the
aliasing verdict; corrected read `limb3_temperature_aliasing_CORRECTED.out`. **Consumers today:
ZERO** — every reader takes arg 7 (`Model`): `json_report.pl:1134`,
`oq136_bucket_provenance.py:70`, `run_pipeline.py:199`. T5b inert-unconsumed; the hazard is armed
for its first reader. Corrected numbers: **80/279 (29%) carry a temperature term** (42 numeric, 38
symbolic), and the deciding haiku cell is **28/28 at one value**
(`max_tokens=16384,temperature=api_default`) — temperature is CONSTANT there, so the clustering
attribution is clean and unaliased. It closes on that plus a measured mechanism: over the full
28-file haiku stratum, files with `founding_problem_status` are 12/12 carrying
`constraint_stakeholder` (median 96 facts) and files without are **0/16** (median 58) — categorical,
**non-overlapping**, two fact families co-missing. A whole-block emission failure (truncation or
schema-branch miss), not a temperature continuum. Owned by **OQ-202**, already open. No residual within-cell sweep: it
would hold constant the one variable already constant there. **"Released" is partly
UNSPENDABLE-AS-DESIGNED — 199/279 stories (71%) carry no temperature term, so no
sampling-parameter attribution is possible from substrate for most of the corpus, and the reserved
sweep could never have been purchased against that material. Declared as GAP-37** so the next
person reaching for a temperature explanation learns it before scoping, not after. Do NOT read the
close as answering temperature corpus-wide: it was answered on one 28-story cell where temperature
is fixed.

**Limb-3 addendum (the earlier same-day ruling: YES accepted, spend-go approved IN PRINCIPLE,
DO NOT SPEND — the reserved design targets verdict *value* stability, OQ-136 needs *presence*
attribution).** The required free Ω_E precursor ran: **26 of the live corpus's 43
`founding_problem_status` absences are not stories.** Every `*_contradictions` file carries exactly
one predicate (`cs_axiom_contradiction`) and sits in the `n_constraints=279` denominator — known
axiom meta-files, but the stratum grew 9 (OQ-136) → 26 and now dominates the absence count.
**Tripwire for anyone sizing a probe off `n_constraints`:** the story-level absence rate is
**17/253 = 6.7%**, not 15.4%, and **16 of the 17 sit in one stratum** (haiku-4.5 @ prompt_commit
`22843cdf`, 16/28 = 57%) while five of seven strata are 0% across 225 stories. A temperature sweep
cannot reach the 26 (no `six_questions` block to populate, and no `story_provenance` to sweep at).
Evidence: `limb3_incidence_recon.out`.

**Instrument-reach check (the one place the conflation could have reached the CONTROLS, not just
the findings): CLEAN.** The three control-eligible apparatus-presence rows are 18/18 literal
`PRESENT`, zero absence cells — `classify_field` returns `("stable","absence")` before the branch
that yields `positive`, so a uniformly-absent field cannot read positive. 8 of 38 fields ARE
absence-inflated (`has_sunset_clause` and `coercion_grid.presence` are 6/6 pure absence); both
machine consumers already filter absence, so the reach is to human readers only. Map:
`absence_agreement_exposure.tsv`.

**Also recorded, so nobody re-derives it by running it:** `shared_agent_link/4` has **zero** edges
on the frozen cohort in every draw-slice — the six stories share no actor name — so cohort zero
cannot re-witness contamination-network claims in EITHER direction. Use a live or twin leg.

**Routed:** OQ-118 Limb 3 escalated to a spend-go, then CLOSED resolved-by-corpus (see above);
**OQ-306** — `n_constraints` silently includes 26 non-stories, so every corpus rate against that
denominator is off by ~10%, and the stratum **grew 9 → 26** while reading stable, which makes
historical rates non-comparable to current ones even when each was correct at its own time. The
general shape is now a build-discipline section (*A denominator that silently admits non-members
gets worse while reading stable*);
OQ-304 (re-score the frozen draws at presence/cardinality grade — Ω_E, zero spend, converts the 221
grade-unmeasured rows); OQ-305 (20 resolved OQs closed on a quoted cast-presence count).

**Method note worth keeping:** three instrument defects were found by *hunting positive controls*,
not by review — the 3d extractor returned 0 because it required a resolved entry to *announce* its
stability premise (all 20 real ones merely quote a count); the documentary control passed by matching
**OQ-190's own body** while missing the one claim it was declared to flag; and the sweep censused its
own probe once committed. Each is the same shape: an instrument that looks like it passed.

## 2026-08-17 — [tripwire] Dispatch heads are CONVERTED: a bound call to classify_from_metrics/6, constraint_signature/2, or classify_by_signature/3 now means "the engine assigns" — reverting a head to the atom form re-arms Pattern 7 and gate row `dispatch head` goes RED
**Files:** prolog/drl_core.pl, prolog/signature_detection.pl, prolog/isomorphism_engine.pl, prolog/dispatch_head_check.pl, python/dispatch_head_check.py, python/bound_selector_check.py, python/check_logic_symbolic_drift.py, scripts/gate.sh, prolog/tests/test_dispatch_bound_call.pl, prolog/boltzmann_compliance.pl
**Tier:** tripwire

**The conversion (fresh-variable heads + unify-after-cut, the dr_type/3 idiom; audit
`audits/2026-08-17_bound_dispatch_hardening/`, fix commit `6c40a0bb`).** Every clause of
the three predicates binds its output AFTER the cut, never in the head. Consequences a
future editor must know: (1) **do not "simplify" a head back to
`classify_from_metrics(..., snare) :- ...`** — that re-creates the bound-probe shape
(bound calls skip earlier clauses' cuts and answer "body holds in isolation"; 311
manufactured cells were witnessed live on `testsets/` alone, 4,694 across five legs);
the gate row `dispatch head` (definition-site walker + MUST-NOT-FIRE registry) and
`tests/test_dispatch_bound_call.pl` both go red on the revert. (2) The zero-diff pilot
witness is **output-preserving on the witness set, semantics-changing by construction**
— never cite it as "behavior-preserving". (3) `check_logic_symbolic_drift.py`'s span
extraction is anchored to the NEW terminal clause text — restructuring the clause set
again means re-anchoring it (it fails loud). (4) `cluster_by_signature` now enumerates
`corpus_constraint/1` — with fresh-variable heads an UNBOUND-C findall over a cut
cascade is pruned to the first generated solution, so never write
`findall(C, converted_pred(C, boundatom), L)` expecting a census. (5) The live
`boltzmann_invariant_mountain` bound-`false` defect (unconditionally inconclusive since
its clause 1 landed) is OQ-302, NOT fixed here — do not cite mountain invariance
results until it lands. Follow-on rollout (class-B conversions, cs_verdict repair,
bound_selector_check retirement proposal, MaxEnt catch+default arms): OQ-303.

## 2026-08-17 — [tripwire] OQ-278 RESOLVED — one taxonomy, seven members at eight indices; the citation freeze is LIFTED, and renumbering a member is a CONSUMER SWEEP
**Files:** CLAUDE.md, docs/technical/build_discipline.md, AGENTS.md, README.md, python/doc_pattern_check.py, python/pattern_citation_check.py, python/bound_selector_check.py, docs/technical/doc_pattern_check.md, scripts/gate.sh, docs/amnesiac_institution/amnesiac_institution_v0_6.md, ISSUES.md, audits/2026-08-14_oq278_index_collision/WRITEUP.md, audits/2026-08-14_oq278_index_collision/PREREGISTRATION.md
**Tier:** tripwire

**THE RULING.** R2 = C2, R1b′ executed. `CLAUDE.md` and `docs/technical/build_discipline.md` now
publish the same member at the same index: 1 produced-but-not-consumed, 2
one-canonical-thing-became-two, **3 VACANT (never reused)**, 4 fabricated-default, 5
absence-satisfies-the-gate, 6 success-shaped-absorption, 7 bound-probe-bypasses-clause-order, 8
recap-as-witness-substitution. **Ground: read-site weight** — nine of eleven publishing sites are
internal to the detail doc; the two on the other side include the always-loaded file. The 5:1
edit-count measure is explicitly not the ground. Symmetry is **unified indices, asymmetric depth**:
a document may carry a member as a pointer entry, but **every pointer must state the TELL, not the
member** — a name-drop is indistinguishable from absence at the read site, which is exactly what
the pre-repair "bound-probe" mention was. The interim `CM-Pn`/`BD-Pn` freeze is **lifted**;
`Pattern N`/`PN` is still seven-way overloaded outside this taxonomy, so name the taxonomy when
context does not.

**TRIPWIRE 1 — RENUMBERING A MEMBER IS A CONSUMER SWEEP, NOT AN EDIT, AND NOTHING GOES RED IF YOU
SKIP IT.** Moving `bound-probe` off index 3 strands every citation that resolved *correctly* to
`BD-P3` — a second stale class, created by this ruling rather than by the 2026-08-11 vacating.
Declare the population in `pattern_citation_check.DISPLACED` **before** the move (gate row
`displaced cites`, renamed from `vacated cites`; one manifest block per displaced member, carrying
the STATE that says why its citations are stale, because the state decides the repair). Do it
first, not after: mechanism recovery is what distinguishes those citations and it gets harder once
index 3 resolves to `bound-probe` in **neither** document. The 2026-08-11 vacating skipped this and
stranded nine pointers — the third time repairing this taxonomy committed the defect it
instantiates; declaring first is what kept this close from being the fourth.

**TRIPWIRE 2 — A SELF-COUNTING DOCUMENT IS CORRECT ONLY AT A FIXED POINT, AND REACHING ONE
REQUIRES THE LAST WRITE TO BE TOKEN-FREE.** The audit's `WRITEUP.md` publishes a census of a
line-keyed sweep that scans `WRITEUP.md`. Its published 750 rows was **stale at its own commit**
(`fd73ec9e`'s own TSV held 755; `c06bcb26` took it to 761 untouched) — not degraded by later
drift, which is the more forgiving story the plan assumed. Method that works: land all prose,
`--sweep`, patch **only the numerals** (a digit carries no countable token), `--sweep` again and
confirm the count did not move. And **stamp no md5 of the artifact inside it** — the label set is
line-keyed, so a content hash invalidates itself as it is written, which is the audit's own
*pin the PRODUCER, never the artifact's content* corollary applied to the file that recorded it.

**TRIPWIRE 3 — A POSITIVE CONTROL PINNED TO A SITE ON ITS OWN INSTRUMENT'S REPAIR LIST RETIRES
ITSELF THE DAY THE INSTRUMENT WORKS.** `pattern_citation_check`'s second control was anchored on
`docs/design/design_gaps.md`'s cross-wired citation. Repairing that citation turned the control RED
and blocked the sweep from writing a label set at all. It was **re-anchored, not deleted**, onto
artifacts nothing is licensed to edit — a dated audit finding (`fabricated-default`), a frozen
`_BEFORE` snapshot (`recap-as-witness`), a completed audit log (`bound-probe`) — one per
recoverable mechanism, strictly more coverage than the two it replaced. Loud, so it stays history
rather than a promotion, but worth knowing before anchoring the next control.

**Also landed, and checkable rather than remembered.** `DECLARED_COLLISIONS` and
`DECLARED_SPINE_LAG` are now **EMPTY — the strong state**, nothing exempted, so any divergence is a
new fork and reds immediately; two controls that could only be forced by contradicting a live
declaration are now forced by a **synthetic manifest** rather than dropped (selftest 6/6 → 7/7).
The discrimination record was re-run on the new 8-slot shape: `--pairwise 220739b8` FIRES,
`--pairwise 4f623017` DECLINES. The record repair walked all four §4.4 "wrong labels" against the
new numbering and **all four are retroactively correct** — the class emptied itself; the principle
that decided the rest is *claims are point-in-time, pointers are navigation*. `bound_selector_check`
no longer prints `BD-P3` at users. R4 applied: `amnesiac_institution_v0_6.md` §5.1 amended and a
new §5.2.1 records the delay's measured cost. Full account:
`audits/2026-08-14_oq278_index_collision/` (`WRITEUP.md` §7, `PREREGISTRATION.md` AMENDMENT 3);
ISSUES OQ-278 resolved; OQ-294 and OQ-287 unblocked.

---

## 2026-08-17 — [tripwire] OQ-251 RESOLVED: maxwell's `natural_law` loss was `8b5a34b8`, NOT OQ-70 — a single-variable isolation over a 126-commit span cannot name a commit
**Files:** prolog/narrative_ontology.pl, prolog/signature_detection.pl, prolog/tests/test_oq113_dead_natural_law.pl, prolog/drl_core.pl, prolog/routing_sink.pl, python/container_typology_analysis.py, python/extract_corpus_data.py, python/linter.py, audits/2026-07-25_oq66_nlwb_filter_cutover/GATE2_REWITNESS.md, ISSUES.md
**Tier:** tripwire

**TRIPWIRE 1 — a controlled comparison can be right about WHAT it measured and still not license
WHICH CAUSE.** `GATE2_REWITNESS.md`'s A-vs-C isolation (era engine vs HEAD, same `kernel_v1`
corpus) correctly established *engine regime, not corpus*. It then named **OQ-70 / `72ec2cdd`** as
the cause. That span contains **126 commits including both candidate commits**, so it could never
discriminate them — the attribution was an inference the isolation did not license, and it
propagated into `prolog/narrative_ontology.pl` (the gate-2 ruling's stated basis), into OQ-251's
own framing, and into OQ-251's "over-determined by TWO independent blockers" claim. **Before
attributing a behavior change to a commit, bisect it.** The tell is a comparison whose two arms are
separated by more than one change.

**The correction, behaviorally witnessed** (three-point bisect, `git archive` scratch trees, corpus
byte-identical at every point — maxwell md5 `9178deb2…` at all three commits *and* the live
worktree, no churn in-window):

| point | commit | `has_viable_alternatives` | `constraint_signature(maxwell, S)` |
|---|---|---|---|
| 1 | `f600599b` (pre-both) | `false` | `[natural_law]` |
| 2 | `8b5a34b8^` = `a4297632` (**post-`72ec2cdd`**) | `false` | `[natural_law]` |
| 3 | `8b5a34b8` (post-fail-close) | `unknown` | `[coupling_invariant_rope]` |

Binding commit: **`8b5a34b8`** (2026-06-11, OQ-43/OQ-44 fail-close). Point 1 REPRODUCES arm C, so
the apparatus was validated against a known result before anything downstream was read. Two further
independent kills of the old attribution: `claimed_natural/2` was never on the `natural_law` path
(both producers gate on `natural_law_signature/1`; era-wide its only executable consumer is
`false_natural_law/2` — the arrow runs the *other* way, clause 3 calls `natural_law_signature`); and
maxwell authors an explicit story-level claim at `:114`, so `claimed_natural` source 1 fires
regardless and removing source 2 was **inert for maxwell** even on the claims side.

**TRIPWIRE 2 — `drl_core:natural_law_without_beneficiary/1` is NOT the `natural_law` signature
atom, and a `grep natural_law` sweep silently conflates them.** Different predicate, different
module, never reads the signature; and it is **LIVE** (30 firings on kernel_v1, 0 on the live leg —
two-sided, so its live-leg zero is a corpus property, not a dead path). Its ~15 consumers are OUT
of scope for any "the natural_law detector is dead" claim. Folding them in inflates the finding by
roughly double. Scoping guard recorded in OQ-296's body.

**LANDED (all comment-only; 0 non-comment lines changed across the three `.pl` files; OQ-113
regression suite 3/3 green before AND after; live `signature_detection.pl` md5 unchanged
`1c58deb9…`).** Commit `1b63ba09`: `narrative_ontology.pl` registry-note attribution corrected and
marked AS a correction + kill-condition disposition added; `GATE2_REWITNESS.md` dated **Correction**
block appended (original point-in-time, not rewritten); three drifted cites refreshed, each target
re-verified at edit time (`signature_detection.pl` `:112`→`:117`, `boltzmann_compliance.pl`
`:580`→`:607`; `test_oq113_dead_natural_law.pl` `:359`→`signature_detection.pl:378`).

**Q1/Q2 verdicts.** Q1: **no path** — `natural_law_signature/1` is unsatisfiable **BY
CONSTRUCTION** (both `has_viable_alternatives/2` clauses bind arg 2 to a head literal, `true` /
`unknown`; runtime `listing/1`, static, non-multifile; two-sided control — authoring the only input
clause 1 reads yields `true`, never `false`). Exactly ONE conjunct blocks maxwell;
`BeneficiaryCount == 0` **passes**, so OQ-251's two-blocker text was wrong. Q2: the scope was ruled
three times (OQ-70 claims-side only; OQ-43/OQ-44 fail-close with casualty accepted; OQ-113 fork (b)
documented-not-changed) — chosen, never a side effect. **OQ-248's kill condition evaluated, did NOT
trip.**

**Promotion test: NO CLAUDE.md promotion.** Verified by direct read, not assumed: CLAUDE.md's OQ-70
paragraph (`:898-905`) is entirely about FNL prevalence and `claimed_natural` source 2 — it makes no
`natural_law`-certification attribution and stays correct. (My probes confirm it: FNL fires 0/1106
on kernel_v1 post-fix.) Tripwire 1's general shape is close enough to existing build_discipline
material that promoting it would be over-promotion; it lives here.

**Also found, filed not fixed:** `python/linter.py:684,719` `MISSING_NL_PROFILE` promises a remedy
that cannot work (the binding conjunct is one it never mentions) and cites a stale mechanism
(`get_metric_average` no longer "defaults to 0.5" — OQ-44 disposition (1), `966d53c8`, made it the
`unknown` sentinel). `domain_priors:should_be_natural_law/1` is dead in both senses — 0 firings on
both corpora and 0 consumers repo-wide. One OQ-266 red (`gm_reverse_natural_fires`, `:427-429`) is
dead-by-range, **not** fixture rot, so refreshing fixtures will leave it red. **OQ-296 minted** for
the ~20-consumer surface.

**FOR THE OPERATOR (E5, surfaced not decided):** the 2026-07-25 gate-2 ruling's *stated evidential
basis* was corrected post-hoc. Its substance (narrative/omega-aboutness discriminator) is untouched
and its kill condition did not trip. Whether that warrants an explicit re-affirmation of
`non_agent_beneficiary(entropic_universe_hypothesis)` is the operator's call (OQ-252 instance).
Default recommendation: **the ruling stands.** Surfaced at the site and in OQ-251/OQ-248.

**CORRECTION-KEY — every `natural_law` certification that ever existed rode a FAIL-OPEN DEFAULT, so
OQ-43/OQ-44's "un-certification accepted" describes an event that did not occur.** (Operator-raised
at close; verified.) `has_viable_alternatives/2` clause 1 needs BOTH `affects_constraint/2` AND
`intent_viable_alternative/3`. The latter is authored by **0 of 4,762 story files** — testsets
0/276, kernel_v1 0/1106, original_v6 0/3380, kernel_v2_test2 0/62. (The only repo-wide occurrences
are 3 fixtures in `test_cs_pattern_detection.pl` and some **unqualified v3.1-era** facts in
`archives/datasets/original_v1/` that could not reach `narrative_ontology:intent_viable_alternative/3`
even if loaded — code-read.) maxwell authors 4 `affects_constraint` facts, reaches the second
conjunct, and dies on the empty table; at bisect points 1–2 its `false` is the era catch-all
`has_viable_alternatives(_, false).`. The ledger's named casualty
`thermal_dissipation_constraint` authors **neither** field. **So the fail-close did not COST a
certification, it REVEALED that none was ever earned.** The FAIL-CLOSED disposition is unchanged and
correct — this is Pattern 5 working. What is wrong is the *description*, and anyone citing the
casualty ledger as a cost-of-fail-closing is citing a non-event. Correction filed at OQ-43;
consequence (repairable only by explicit authorship ⇒ "structure or declaration?" from the other
side) routed to OQ-296 as an operator question.

**TRIPWIRE 4 — `has_viable_alternatives/2` is a CONSTANT FUNCTION, so `coordination_scaffold` is
dead too; this is not a `natural_law` story.** (Operator-raised at review; the audit had the datum
and had not drawn the inference.) Range is the singleton `[unknown]` on **all seven legs — 8,688
constraints** (testsets 276, haiku 960, flash 960, kimi 1005, sonnet 1001, kernel_v1 1106,
original_v6 3380), `count(true)=0` and `count(false)=0` everywhere. **Two different deaths:** `false`
by CONSTRUCTION (no clause emits it), `true` by EMPTY TABLE (`intent_viable_alternative/3` has 0
facts anywhere — GAP-08). `affects_constraint` is richly authored (9,523 facts), so clause 1's first
conjunct succeeds abundantly and the predicate dies wholly on the second. **Consequence:**
`coordination_scaffold_signature/1` requires `HasAlternatives == true` (`signature_detection.pl:458`)
and fires **0/276 live, 0/1106 kernel_v1** — a second named cascade signature that cannot fire.
`reading_registry.pl:115` registers the predicate `total_on_domain`, so the OQ-137 totality gate
passes VACUOUSLY on a constant. Scope widened at OQ-296; GAP-08 amended at its own site (its
casualty language stated the claim independently and an OQ-43 correction does not reach it).

**TRIPWIRE 5 — a BOUND second argument to `constraint_signature/2` is OVER-PERMISSIVE; use `once/1`
UNBOUND for any census.** Binding the signature atom skips earlier clauses on head unification and
so bypasses their cuts: `constraint_signature(C, ambiguous)` returns **276** on the live leg versus
**0** through the real cascade. This audit made the mistake on itself — it reported `natural_law` 0
"while 273/273 and 1106/1106 constraints carry *some* signature," implying a healthy signature layer.
Real distribution: **kernel_v1 is 739/1106 `unknown` (67%)**; live is 26/276 `unknown` with
`constructed_high_extraction` 142 dominant. **The zeros survive and are strengthened** — a zero from
an over-permissive query is conservative — but a bound-arg NONZERO is an artifact until checked, and
(coverage review below: this tripwire is a RE-DISCOVERY of `build_discipline.md` Pattern 3, dated
2026-05-30, whose worked example is this exact query),
the denominator gloss hid a bigger fact than the finding it was supporting.

**THE SESSION'S ACTUAL RECURRING DEFECT — and it is NOT the routing story this entry otherwise
tells (operator correction, 2026-08-17; recorded because the routing framing is the one a later
reader would otherwise inherit).**

Four claims in this session were wrong or unsupported when made: *"wrong query, zero output
change"* (`:450`), *"RED at dcde9591"* as a durable record, *"the carve-outs are fine"*, and
*"no recorded finding cites this"* (before OQ-298's enumeration ran). It is tempting — and I did
frame it this way — to call these *true sentences read as stronger claims*. **That framing is a
notch generous and it fits only two of them.** `the carve-outs are fine` was not a true sentence
read strongly: it was **an unverified property asserted as checked, and two of the four
carve-outs turned out to be false.** Same for the consumer claim.

**What all four share is narrower and more useful: the second side was never run.** No
plant-and-restore on the carve-outs. No grep for consumers. No test of whether the anchor
survived a rebase. No per-id check of what the excess conjunct actually blocked. **Each was
cheap — every one took a single command when it was finally run — and each was skipped at the
moment the claim was written, not later.**

**This is the same category as the one-sided FNL control earlier in the arc, which this audit
correctly classified as NOT a routing failure** (the rule reached the instance; it was applied
late). So the defect that actually recurred four or five times this session is the one the
audit *diagnosed and then proposed nothing for*. The score is uneven and worth stating plainly:
bound-probe got a gate row; gating-count got a same-pass clause in `audits/README.md`;
**control-written-after-the-probe got a diagnosis and no fix, and it is the one still firing.**

**The conclusion the record should carry: this session's recurring defect is VERIFICATION ORDER
IN PROSE, not rules in files.** Rules-in-files was the visible story because it produced
artifacts (a promoted rule, a gate row, three OQs). Verification-order produced no artifact,
which is precisely why it kept recurring and why it would otherwise close unrecorded. Deliberately
NOT converted into more apparatus here — the fix is ordering, and naming it is the intervention.

**COVERAGE REVIEW (operator asked: "probably already in docs/technical/ and CLAUDE.md but review
anyway"). Answer: docs/technical/ YES — BOTH rules. CLAUDE.md: NEITHER.**
- `build_discipline.md:601` **Pattern 3 — Bound-probe bypasses clause-order**, dated **2026-05-30**,
  whose worked example is `constraint_signature(C, natural_law)` — the exact query form. TRIPWIRE 5
  above is a **re-discovery**, not a discovery.
- `build_discipline.md:1669` **"A gating count is not a finding without its composition"** names the
  error precisely, *and* specifies the half that was missed: the breakdown is computed **in the same
  pass** as the count, "**never as a follow-on when someone doubts the headline**." The 67%-unknown
  breakdown was computed exactly as a follow-on when the operator doubted the headline — textbook,
  including the timing.
- **CLAUDE.md carried neither.** Its only occurrence of "bound-probe" is inside the CITATION FREEZE
  (naming the colliding index, never teaching the rule); its index 3 is the vacated grave. **The rule
  that would have prevented the error was unreachable from the always-loaded file**, which is why the
  instance made it. **Repair landed:** BD-P3 promoted into CLAUDE.md as a **NAMED, explicitly
  unnumbered** rule — sidesteps the OQ-278 freeze (a named rule needs no migration whatever P4 is
  ruled) — carrying the over-permissive asymmetry (bound zero conservative / bound nonzero artifact)
  that BD-P3 itself does not state. Evidence filed on OQ-278: **vacate-and-leave-empty is not
  cost-free even when the demotion is right** — "the number is retired" and "the mechanism is still
  taught" are separate questions, and only the first was asked.
- **Deliberately NOT promoted:** the gating-count rule. Its general form is already always-loaded in
  the governing stance ("Distrust the aggregate… a count… can read as success while concealing the
  opposite"), so promoting the specific rule would be the over-promotion CLAUDE.md warns against.
  **Flagged for the operator instead:** the **same-pass** clause is the operative half, and the
  governing stance does not carry it — distrusting a count does not tell you to compute its
  composition before it becomes a headline.

**TRIPWIRE 6 — BD-P3 is now a GATE ROW (`bound selector`), and it found 5 live call sites the
documentation had not.** `python/bound_selector_check.py` in `scripts/gate.sh`. Registry-driven
(`CUT_ORDERED`, opt-in — an unregistered predicate is NOT checked and NOT reported safe);
exemptions require a **reason string**, never a bare path, or the list decays into "sites someone
silenced" (currently 0 exemptions — the 5 sites were REPAIRED, not silenced).
**Discrimination record (naturally-arising pair, gate written BEFORE the fixes, deliberately):**
RED with exactly 5 sites at **`dcde9591`**, GREEN at the repair commit. The sequencing was the
point — fixing first would have shipped a row that had never gone red, i.e. indistinguishable from
a matcher that silently misses (OQ-297's own argument).
**The checker had the defect it was built to catch, on its first run:** it reported 16 "violations",
of which 11 were the predicate's own clause HEADS (definitions, not calls) and its own docstring +
fixtures (self-fire). An introduced instrument inherits the discipline; the count was fixed before
it was believed. Both carve-outs are now selftest rows (11 fixtures: 4 violation shapes, 7 negative
controls incl. clause-head and comment-line).
**`test_reading_totality.pl:139` — the repair did NOT go red, and that is the informative
outcome.** This is the one site where the repair changes *what is proven* rather than *what is
queried*, so it carries its own discrimination event. Measured:

```
ALL solutions (unbound) for tst_rt_unauthored = [unknown]
once/1 cascade winner                          = unknown
OLD bound form                                 = succeeds
```

**Newly-proven fact, worth writing down because the test's name now implies it and nothing
established it before:** the engine genuinely lands on `unknown` for `tst_rt_unauthored` — a
single solution, the honest-abstain fallback, reached as the real cascade winner rather than
merely satisfiable. The old bound form passed *only because nothing earlier fires on that
fixture*, which is contingent on the fixture, not guaranteed by anything. **Forward value of the
strengthened form:** if a future schema or clause change made an earlier lock fire on
`tst_rt_unauthored`, the old form would still have passed (wrongly, proving only that the
fallback body is satisfiable) while the new form goes red. The test was not masking a defect
today; it was incapable of detecting one tomorrow.

**Repairs, all unbound + post-filter:** `diagnostic_summary.pl:424` (latent),
`diagnostic_summary.pl:450` (**LATENT, MASKED BY AN UNRELATED CONJUNCT — not "harmless"**: its P6
pattern ANDs with `excess_extraction < 0.05` and every over-counted id carries excess 0.13–0.28,
witnessed per-id on 3 legs, so blast radius is zero *today*. Nothing ties that threshold to this
defect and nothing guarantees it stays at 0.05; written as "no output change" it reads as harmless
and the next reader treats a masked defect as a non-defect), `routing_sink.pl:120` (latent), `test_reading_totality.pl:139` (the bound form was the
WEAKER assertion — `unknown` is the LAST clause, so binding it proved only that the fallback body
is satisfiable), `python/fcr_ablation.py:75` (**NOT insulated → OQ-298**).

**TRIPWIRE 3 — a dirty→clean transition in `git status` does NOT mean "stale index stat"; another
instance may have just committed.** This audit made that mistake on itself and recorded "no
concurrent writer" in its own log: `git status --porcelain` showed ` M KNOWN_STATE.md` etc., a
follow-up `git diff --stat` came back empty, and the empty diff was read as a stale stat. It was
`f64384d3` (OQ-285 review round 2) landing at **03:12:00**, the same minute. Two live readings,
one command run, the convenient one reported. **Separate them with `git rev-parse HEAD` / `git log`,
not with a second content read** — content reads cannot distinguish "stat was stale" from "someone
just committed it." No damage here (engine tree byte-identical across the two commits, verified;
the only write-set overlap was `KNOWN_STATE.md`, edited after and anchored on the new content,
committed clean, checker 0 problems), but the *inference* was exactly the kind this entry's
TRIPWIRE 1 is about, made by the instance correcting it. Correction block: top of the audit log.

**Operator's diagnosis (2026-08-17), recorded because it locates the weak link precisely:** the
plan's sequencing constraint was *"check with the operator if unclear"* — **which is not a check.**
And the inference ran backwards: a dirty→clean transition is *affirmative evidence of a writer*, not
of its absence. **PROPOSED, NOT BUILT (needs a ruling — it touches committed apparatus):** a
standing pre-flight probe replacing the prose instruction — stamp `git rev-parse HEAD` at session
start, re-read before the first write, and fail loud if it moved. Home would be
`.claude/settings.json` (SessionStart + PreToolUse), which is versioned project apparatus, so this
is above the fix-simple-errors threshold and is flagged rather than implemented. A cheaper variant
needing no hook: add the HEAD stamp to the audit-log template alongside the prereg md5, so the
comparison is forced at close even when nobody thought to check at start.

Witness: `audits/2026-08-17_oq251_natural_law_reachability/` (WRITEUP.md, audit_log.md,
PREREGISTRATION.md md5 `f7336ee7…` recorded above the first result line, two probe `.pl` files).

---

## 2026-08-17 — [tripwire] A static clause-coverage read is a hypothesis, not a witness — 2 for 2 wrong in one session; and `total_on_domain` is not a totality proof
**Files:** docs/technical/build_discipline.md, prolog/signature_detection.pl, prolog/stakeholder_seats.pl, prolog/reading_registry.pl, prolog/drl_core.pl, python/run_pipeline.py
**Tier:** tripwire

**The warning.** *"I enumerated the clauses; every one with first argument X returns Y, therefore
the branch is unreachable"* reads like verification and is not. Two such claims were made and both
falsified in one session (2026-08-17, OQ-285 review,
`audits/2026-08-17_oq285_mode3_measurement_arm/`) — corrected by **running** the entry point, not by
reading harder. Full entry, mechanisms, and the discharge template:
`build_discipline.md` → *A static clause-coverage read is a HYPOTHESIS, not a witness*.

1. `derive_directionality_for_stakeholder/3` is registered `total_on_domain`
   (`reading_registry.pl:110`) and is nonetheless **partial**: a malformed `exit_options` under a
   *well-formed* role fails at `stakeholder_seats.pl:76` inside the `->` then-branch, so the
   canonical-power fallback at `:79` never sees it. Latent, not live (0 malformed atoms in 19,414
   agent seats across five legs).
2. *"Every reachable `resolve_modal_signature_conflict/3` clause with first arg `unknown` returns
   `unknown`"* — wrong twice. `integrate_signature_with_modal/3`
   (`signature_detection.pl:812-814`) calls **`resolve_with_perspectival_check/4`**, whose
   `false_ci_rope` clause at `:846` **cuts before** the clause being read at `:946`; and two
   reachable paths (`:843-845` piton refinement, `:939` CI-rope via the `:862-863` fallthrough) map
   `unknown` to a real type on **29 live seats**.

**Why here specifically:** cuts as position-encoded cascade priority, if-then-else else-arms
unreachable from inside the then-arm, and a wrapper layer that intercepts before the predicate you
are reading. Source order ≠ dispatch order, and a clause table encodes none of the three.

**Discharge template (cheap):** split the *consumer's own* call chain at the boundary in question,
emit both halves per item over the corpus, cross-tabulate. `seat_dump.pl` in the audit dir is the
worked example — it answered in one run what the clause read got backwards, and its zero cell got an
ablation-lever control (0 → 4 → 0).

**Corollary — `total_on_domain` means "total on the data we have."** The OQ-137 suite checks
exactly-one-solution per domain key over the *loaded* corpus, so a predicate total only because no
adversarial input was authored registers green. Cite such registrations at that altitude.

**Related, same session:** `residual_signature_firing/1` (`signature_detection.pl:1028-1032`) — the
monitor behind `_prolog_residual_signature_gate()` (`run_pipeline.py:840-861`) — evaluates at
`default_context` while the demotion it guards happens per seat; 559 seats sit outside its domain
(0 currently on a residual pattern). Filed as incidental item 9 in the audit dir, unminted.

---

## 2026-08-15 — [correction-key] Two standing rules migrated out of closed OQs (OQ-34 MI-threshold visibility, OQ-54 axis-irreducibility); general rules are the ones the file-keyed channel cannot carry
**Files:** prompts/constraint_story_generation_prompt_json.md, prompts/uke_scope_v2_json.md, agent/story_generator_base.py, agent/generate_kernel_corpus.py, agent/c-orchestrator.py, prolog/cs_kernel_registry.pl, python/enhanced_report.py, ISSUES.md
**Tier:** correction-key

**Rule 1 (from OQ-34, resolved).** Thresholds that are *definitional bounds* on the substrate may
be author-visible; **measurement-independent decision rules the engine applies must NOT be** — an
author who sees an MI threshold satisfies it mechanically and the classification stops carrying
information. χ thresholds are MI by construction. Three-way rubric when editing any authoring
prompt: D-both → keep; D-direction/MI-value → keep the direction, strip the number; MI-both →
strip. This is the GENERAL form of the specific `epsilon_bin` prohibition at KNOWN_STATE
2026-06-12 / OQ-117, and it was written first.

**Rule 2 (from OQ-54, mitigated).** `cs_kernel_obstruction/4` is built **observer-blind** — it
reads only `cs_reading_relation`, never χ or `live_index` — because Theorem 7 / detection
independence makes the reading-axis obstruction gradient-orthogonal to the observer one. It
**must not be reduced to** the observer obstruction. Consequence for report surfaces: a
`dr_type`-derived statistic (pairwise Jaccard, H¹-across-readings) is **not** evidence about
reading distinctness, and two readings can be axiomatically contradictory while computing
identically.

**Why these two and not the other 22.** A sweep of resolved OQs carrying imperative prohibitions
found ~90% already named somewhere in KNOWN_STATE; these were the exceptions. They are the
exceptions **because they are general** — a cross-cutting methodology rule has no natural
`**Files:**` line, so the `PreToolUse` file-keyed channel has nothing to key on. The channel
systematically under-covers exactly the rules with the widest scope. (Count caveat: the sweep's
size moved 32→36→24 across three regex cuts, so treat "how many" as fuzzy and the ~90% coverage
ratio as the robust part.)

**Both rules were violated in one session (2026-08-14/15) by an instance that had not read them** —
Rule 1 by proposing to wire `epsilon_bin` into the authoring prompt, Rule 2 by reading a pairwise
Jaccard as a fact about reading relationships and carrying it into an essay draft. Rule 1 was
caught only because the instance later happened to edit a file OQ-117's entry names; Rule 2 was
caught by replication, not by any rule. **Delivery is edit-time; both decisions were
proposal-time.** Surfacing design (a STANDING section on the resolver menu) is unbuilt — see the
OQ minted for it.

## 2026-08-15 — [tripwire] c-orchestrator reconciles declared-vs-landed and can now exit non-zero; the kernel report block is axis-grouped
**Files:** agent/c-orchestrator.py, python/enhanced_report.py, audits/2026-08-14_cheap_confession_codraw_replication/WRITEUP.md, ISSUES.md
**Tier:** tripwire

**The silent mistake this prevents (two, both restoring a success-shaped summary).**

**(1) Do not drop the RECONCILIATION block from either generate path, and do not remove the
`sys.exit(1)` at the end of `main()`.** Before 2026-08-15 `main()` fell off the end, so the
process exited 0 no matter what happened — there was no failure channel out of the program.
A run that lost a story printed a diagnosable cause (`FAIL <cid>: 'stakeholders' is a required
property`; `FAIL <cid>: JSON_PARSE_ERROR`), then reported `Generated 6 stories` and exited 0.
**The defect was never detection — it was reconciliation:** the error was announced and the
summary reported success anyway. Both paths now compare the declared seed set against what
LANDED, counted from json+pl **on disk** via the same predicate `--close-gaps` retries on —
never from the backend's `succeeded` set, which is a claim about persistence sourced from
something that is not persistence. A shortfall sets `status="partial"` (new StepResult value;
no step aborts on it — partial work is kept). Discrimination record, naturally-arising, no
authored fixture: `codraw_01` DECLINES 7/7, `codraw_02` FIRES on `menu_curation_capture`,
`codraw_03` FIRES on `proceduralist_reading`.

**(2) Do not un-group the `--- KERNEL: ---` block.** It renders two axes the architecture keeps
separate: authored committer structure (readings, axiom conflicts, obstruction, terminals) and
computed observer quantities (reading-robustness, H1-across-readings, pairwise Jaccard — all
`dr_type`-derived). `cs_kernel_obstruction` is observer-blind BY CONSTRUCTION
(`commitment_systems_sketch_v6.md` §8.1 / Theorem 7), so the two can disagree by design — two
readings can be axiomatically contradictory and computationally identical (witnessed: J=1.000
between the declared-contradictory pair). Rendered unmarked under one heading, this invited
reading an observer statistic as a committer fact about reading distinctness, which is how a
cross-axis claim reached a published essay draft. Groups are `[committer axis — authored]` and
`[observer axis — computed dr_type; SINGLE DRAW, unreplicated]`.

**Caveat on (2), recorded because it bears on whether to extend the approach:** across this whole
arc the durable catches were **replication and operator rulings, twice each; annotation caught
nothing.** The reader who made the cross-axis error had already quoted the `dr_type` provenance
back before making it. Treat the labels as cheap and possibly-helpful, **not** as the fix — the
fix for a claim that should not be load-bearing is re-running it. Provenance:
`audits/2026-08-14_cheap_confession_codraw_replication/` (k=3 co-draw, `Fired: live`).

---

## 2026-08-14 — [correction-key] The interim `CM-P4`/`BD-P4` namespacing freeze (SUPERSEDED 2026-08-17), and the corrections to OQ-278's own figures
**Files:** audits/2026-08-14_oq278_index_collision/WRITEUP.md, audits/2026-08-14_oq278_index_collision/PREREGISTRATION.md
**Tier:** correction-key

> **SUPERSEDED 2026-08-17 — DO NOT FOLLOW THE NAMESPACING INSTRUCTION BELOW.** OQ-278 ruled: both
> documents publish the same member at every index and the freeze is lifted, so writing `CM-P4` /
> `BD-P4` now asserts a distinction that does not exist. See the 2026-08-17 entry *"OQ-278
> RESOLVED — one taxonomy…"*. **Retiered `tripwire` → `correction-key` and its `Files:` line
> narrowed to the audit artifacts on purpose:** `Tier:` is a ROUTING decision, and leaving this
> entry as a `tripwire` naming `CLAUDE.md` would keep the `PreToolUse` hook delivering a retired
> instruction to every future editor of that file. What survives here is the *figure* corrections
> and the archaeology, which is why it is kept rather than deleted. Two of its own numbers have
> since moved: the overload count is **seven**, not six, and the published census figures are
> corrected in the audit's §2.1.

**The silent mistake this prevents:** writing `Pattern 4` in a new document, or reading one in
an old one, as if it named a single mechanism. `CLAUDE.md` and `docs/technical/build_discipline.md`
publish the same numbered defect taxonomy and **disagree at indices 3 and 4** — `CM-P4` is
*recap-as-witness*, `BD-P4` is *fabricated default*. Both documents read as a complete, coherent
six. Write `CM-P4` / `BD-P4`; treat a bare index found in the record as UNRESOLVED, not
interpreted. Freeze lifts when OQ-278's index-4 ruling lands.

**Why nothing caught it for 151 commits — worth knowing before building any doc-vs-doc check.**
The two lists were born divergent (`7af6b945`, 2026-05-29: CLAUDE=3, BD=2) and their **member
COUNTS converged at `220739b8` (2026-05-30), the exact commit where their CONTENTS diverged**,
matching at every append since. Any check comparing cardinalities reads green from that day
forever. `python/doc_pattern_check.py` therefore compares **names per index**, never totals; gate
row `doc patterns`. Collisions 3 and 4 are allowlisted **with their state** (`ruled_pending_R1b`
vs `unruled`), so a *silent resolution* goes red as well as a new fork. Discrimination record:
`--pairwise 220739b8` FIRES at the defect's own commit, `--pairwise 4f623017` DECLINES at its
immediate parent — naturally arising, both sides.

**A `DOTALL` footgun in that checker, recorded because it read GREEN while measuring nothing.**
`CLAUDE.md`'s items 3 and 6 hard-wrap **inside** the bold run, so the closing `**` is on the next
line. Without `re.DOTALL` those two extract to nothing and the check passes on four of six
indices — the checker's own absence-satisfies-the-gate. If you touch `CM_BOLD_RE`, run `--list`
and confirm all six resolve on both sides.

**Corrections to OQ-278's own figures** (`audits/2026-08-14_oq278_index_collision/`,
Fired: live): the dating rule is **VOID** — a date cannot tell you which document the author had
open, since both lists were live simultaneously for every citation that exists; recovery is by
**mechanism** (55 of 66). `Pattern N`/`PN` is **six-way** overloaded, not four — added `CWC:P3`
(concealment claim rows) and decompose-manifest `candidate_pattern`, **both found as the sweep's
own false positives**; two further populations are *pinned, not ambiguous* (the md5-frozen OQ-277
prereg; OQ-278's own body). Wrong labels **1 → 4** (one is in CODE, `prolog/coercion_projection.pl:86`).
**Stale pointers created by the 2026-08-11 vacating: 9, not 3** — nobody swept, which is
`build_discipline.md:1392`/`:2558` firing on the taxonomy's own repair for the third time.

**R1a RULED 2026-08-14 — A1, `fabricated-default` is a PEER MEMBER** (operator). **R1b VOIDED and
RE-REGISTERED as R1b′ in the same review:** both its branches conditioned on a *generality*
criterion ("language-specific instance" / "distinct at the taxonomy's altitude") that appears
**nowhere in `build_discipline.md`** — and `Pattern 5` is **already language-specific in its
tells** (`Count == 0`, `forall(P,Q)` vacuous — `:735–737`), so generality is demonstrably not a
membership criterion. Applying it would have been inventing the discriminator at ruling time.
R1b′ runs on the file's stated criterion only. R2's ground was also corrected: the frozen prereg
is **point-in-time like every other**, repaired by the label set, not "permanently unrepairable" —
its real ground is a priced ~5:1 cost asymmetry.

**SECOND INSTRUMENT, `python/pattern_citation_check.py`, gate row `vacated cites`** — the
archaeology sweep moved to `python/` (not copied) and given a gate mode, because
`build_discipline.md:1392`/`:2558` have now fired **three times on this one taxonomy** and three
instances wants an instrument, not a fourth note (operator). Declaration-based: the 9 stale
citations can't be repaired until R2, so they're declared — RED on a tenth, RED on a silent
repair. **It earned its keep on its first run:** it disagreed with the hand-adjudicated list on
**7 of 9** sites, and the hand list was right — `faith merge` never matched `faith-merge`,
`old-vs-new diff` never matched `Old-vs-new OUTPUT diff`. **Under-recovery is SILENT (it presents
as `unrecoverable`, which reads like a result)**, so `LABEL_SET.tsv` had been shipping
under-recovered rows to OQ-294. Two further traps recorded there: the sweep **read its own
committed output** (census compounded 671 → 1421; a producer consuming its own artifact reports
growth as discovery), and a regex literal containing an index made the checker **cite itself**.

**R1b′ RULED 2026-08-14 — B1′, PEER MEMBER AT INDEX 7** (3 stays vacant; `CLAUDE.md:506` is a
standing do-not-reuse instruction). The criterion is conjunctive and only the corrective half had
been checked; the tell half is answered by `build_discipline.md`'s **own spine table**, which gives
bound-probe and the diagnostic layer separate rows with **opposite-polarity tokens** — "a solution
came back" (over-count, 432 vs 404) against "a clean/empty result" (under-read) — and tells visible
statically (`:648`) vs only by running a known-positive (`:1007`).

**TRIPWIRE — THE `P3 = 0` FIGURE IS INADMISSIBLE AND I CITED IT ANYWAY.** The frozen OQ-277 prereg
states, four lines below the figure, that P3's entry is **`no members — uncalibrated`**, that
per-pattern (iii′) agreement **is not reportable**, and (`:244` row 6) that reading a per-pattern
(iii′) figure as a finding is a **pre-registered error**. **A frozen, gate-enforced artifact
guarantees its TEXT is unaltered, never that your READING of it is apt** — the exact residual
`claim_cite_check.py` declares it cannot close. The freeze's authority is what made the figure feel
load-bearing. Struck.

**R2's ground corrected to READ-SITE WEIGHT, not the 5:1 edit count** — 9 of 11 sites are in a
detail doc read on demand; the 2 on the other side include the always-loaded `CLAUDE.md`, so the
asymmetry runs in opposite directions on the two measures. Operator lean C2 (fabricated-default
keeps 4); **R2 not formally ruled.**

**OQ-294 MUST CONSUME THE CORRECTED LABEL SET** — produced at commit `fd73ec9e` or later,
**never the `6e7df53f` copy it was minted against**, which is under-recovered. **Pin the PRODUCER,
not the content:** the label set is a line-keyed census of tracked files, so it changes whenever
any scanned file is edited — a content hash pinned in a scanned file (`ISSUES.md`, this file)
invalidates itself as it is written. Verify by regenerating: `--sweep` then `git status` clean.

**Still not ruled:** R2, R4.
The citation repair is deliberately held until R2 — "append the slug in place" is invalidated if
the ruling renumbers. **OQ-294 minted** (`splits_from OQ-278`) for the taxonomy
self-reproducibility study; it consumes `LABEL_SET.tsv`, which is keyed on `mechanism_slug` so it
survives a renumbering.

## 2026-08-14 — [tripwire] OQ-287 Pass A: the derivation left v0.6. §2.1–2.7 are VACATED and never reused; citations into concealment are DIGEST-PINNED and a digest fire means re-read, not bump
**Files:** docs/amnesiac_institution/amnesiac_institution_v0_6.md, docs/concealment/concealment_without_a_concealer_v0_4.md, docs/concealment/README.md, docs/amnesiac_institution/README.md, python/claim_cite_check.py, audits/2026-08-13_oq287_defork/claim_digest.sh, audits/2026-08-13_oq287_defork/checks.sh, scripts/gate.sh, ISSUES.md
**Tier:** tripwire

**Four things a fresh agent would get silently wrong.**

1. **`amnesiac_institution_v0_6.md` no longer carries the derivation.** §2.1–2.7 were vacated
   2026-08-13; §2 is a one-sentence conclusion quoted from `CWC:C1@…` plus pinned citations, then
   preserved repository material at **`2.A`–`2.D`**, then §2.8/§2.9 at their old numbers. **The
   numbers 2.1–2.7 are deliberately empty and must never be reused** (P3's precedent: a visible gap
   is a checked fact; a silent renumber is a fork). Editing v0.6 at all: it admits **pointer-only**
   edits — forward pointers and redirect notes — and no content edits.

2. **Citations carry a content digest over the WHOLE Appendix A row, kill condition included.**
   `CWC:A2@31548228`. Editing a row's *kill condition* moves the pin and fires every citing site
   even though the quoted claim is untouched — **that is the mechanism working, not a false alarm.**
   Witnessed 2026-08-14: correcting `E1`'s Owed cell moved `884ea0b6 → 911a4db5` and fired six
   sites; all six were re-read and one improved. Fifteen other digests recomputed identical, which
   is the discrimination — a scheme that fires on everything is indistinguishable from one that
   fires on nothing. **On a fire: RE-READ the site and decide, never bump the hex.** Recipe is a
   script, never prose: `audits/2026-08-13_oq287_defork/claim_digest.sh` (a prose recipe was
   implemented two incompatible ways in one turn and every digest was wrong).

3. **`claim_cite_check` scans the WHOLE repo including untracked files, and is blind to aptness.**
   Scope is `git ls-files --cached --others --exclude-standard`: tracked-only once shipped as a
   silent hole where a new document's stale pin was invisible until committed. A pin inside a
   `PIN-RECORD` sentinel is a *record* of a past state and is counted, not checked. **What it cannot
   see: whether the cited row is the RIGHT one.** A citation aimed at `A2` where the argument needs
   `A4` reads green forever. Section-only refs (`CWC` §5.1/§5.4/§9.1/§3.2) have no row and are
   unpinnable by construction — counted via `--list --unpinnable`, never checked. **Do not "fix"
   that by minting Appendix A rows for those sections**: that is the instrument reshaping the
   substrate to fit itself.

4. **§2.8/§2.9 are DECLARED TEMPORARY, and §2.9(b) is cited in correspondence already sent.**
   They keep their numbers and their text; when the practice paper lands they become the superseded
   side and gain a **forward pointer**. §2.9(b) keeps its letter, so the sent letter's citation
   resolves correctly before and after.

   > **[CORRECTED 2026-08-20 — original preserved, per the corrections-on-close rule.]** This item
   > read: *"`LETTER_2026-08-11_wu.md` cites `§2.9(b)` and cannot be edited, so the redirect owed
   > when the practice paper lands is needed **at sub-item granularity**. That limb has **no owner**
   > and carries a review date of **2026-09-14**."*
   >
   > **Wrong in its inference, not its facts.** The letter does cite `§2.9(b)` and is uneditable —
   > but **uneditability generates no repository obligation**; nothing here can compel an edit to a
   > letter in someone else's inbox. The exposure that would have justified sub-item granularity was
   > **a reader following `§2.9(b)` out of a PUBLISHED artifact**, and that designation was never
   > executed: v0.6's appendices are A/B/C/D, and the letter occurs once, as a repo path. **Limb 2
   > was DISCHARGED 2026-08-20 and its 2026-09-14 review date retired** (ISSUES OQ-287, R-A/R-B/R-C;
   > `audits/2026-08-20_oq287_limb2_discharge/WRITEUP.md`). The reversion trigger — promoting the
   > letter to a published appendix re-instates the sub-item table — is recorded in the letter's own
   > annotation header, where the person making that decision is actually looking.

**Three residuals that are NOT checkable and will not announce themselves:** `COVERAGE_DIFF.md`'s
coverage calls are unverified and pre-`C1` (*the re-check verifies the anchors, not the coverage
calls*); aptness is unguarded (above); finding 4's twelve-instance count is a **floor**, self-observed
with no denominator — never cite it as a rate or against §7.4's nine.

Provenance and full record: `audits/2026-08-13_oq287_defork/WRITEUP.md` (`Fired: live`), ISSUES
OQ-287. Commits `fb0cbb86` → `96db0124`.

## 2026-08-13 — [tripwire] KNOWN_STATE tripwires are now DELIVERED at edit time by a PreToolUse hook — the `Tier:` you author decides whether an editor ever sees your warning
**Files:** `python/pretooluse_tripwires.py`, `python/known_state_status.py`, `.claude/settings.json`, `scripts/gate.sh`, `.gitignore`, `CLAUDE.md`
**Tier:** tripwire

**What changed.** A `PreToolUse` hook on `Edit|Write` (`.claude/settings.json`) runs
`python/pretooluse_tripwires.py`, which injects the KNOWN_STATE entries whose `Files:` line
names the file about to be edited. `known_state_status.py --file` had existed since the
`Files:` grammar landed with only `gate.sh` and `audit_citation_status.py` as consumers — a
worker had to *remember* to run it, which is the one thing a non-persistent worker cannot be
relied on to do. The query was right; its delivery time was wrong.

**THE SILENT MISTAKE THIS CREATES, and why it is a tripwire.** The hook delivers **only
`tripwire` and `correction-key`** tiers. So **authoring a genuine warning at `landed` tier now
means no editor is ever shown it** — the entry looks filed, the file looks covered, and the
delivery never happens. Before this change the tier was a sorting convenience; it is now a
routing decision. Tier a standing do-not as `tripwire`, not `landed`, or it is invisible where
it matters. (Live example of the filter: `scripts/gate.sh` has 6 entries, 1 passes.)

**Hook silence means EXACTLY "queried, matched nothing."** A query that cannot run emits a loud
`DELIVERY FAILED` context instead — never silence. Do not read an absent injection as "this file
has no KNOWN_STATE entries" without that guarantee in mind, and do not "simplify" the failure
path to a bare `except: pass`: a broken instrument that emits nothing is byte-identical at the
read site to a working one that found nothing, and nobody ever sees the injection that did not
happen (Pattern 6, on a channel where it would never be noticed).

**Matching is canonical, not copied.** `known_state_status.entries_for_file` was extracted from
`main()` and is now called by BOTH `--file` and the hook; a second copy of the predicate would
be a silent fork of the matching rule (Pattern 2). Refactor witnessed byte-identical across 7
files (md5 `5d00a10f` before and after) and `--check` green at 286 entries.

**Discrimination record (2026-08-13).** Selftest `--selftest` runs 5 controls, wired into
`scripts/gate.sh` as `tripwire hook` so the control is *called*, not merely correct: fires on
`signature_detection.pl` (17 of 38 matched entries pass the filter); declines on `python/cli.py`
(no entry names it); declines on `python/spec_enum_check.py` **by the tier filter with
matched=1** — the discriminating case, since a path miss and a filter decline look identical at
the output; emits loud failure when the scan raises; and the emitted payload parses as
`PreToolUse` hook JSON. Harness-level witness: two `Write` calls, sentinel recorded **2**
invocations, **1** injection — the wiring fires on both and the filter declines on one.

**Not covered.** CLAUDE.md's own tripwires are NOT delivered by this — they have no `Files:`
index to query. Building one is the open follow-on (per-rule "name the tool call this must fire
before"); until then CLAUDE.md tripwires still ride the always-loaded path.

**RULED (2026-08-13, operator delegated the call): `.claude/settings.json` is VERSIONED; all
other `.claude/` contents stay machine-local.** The three hooks are apparatus, not preference —
`CLAUDE.md` documents behaviour that depends on them, so they belong in the repo the way
`scripts/gate.sh` does. `settings.local.json` (70 personal permission rules) and `worktrees/`
stay ignored: permissions are a machine-specific trust decision a fresh clone must not silently
inherit. Scope audit at the time of the ruling found nothing misplaced — `~/.claude/settings.json`
holds only personal preferences and defines **no** hooks; the local file defines none either.

**TRIPWIRE — the obvious `.gitignore` fix is SILENTLY INERT.** `.gitignore` now reads `.claude/*`
plus `!.claude/settings.json`. **The trailing `/*` is load-bearing:** git does not descend into an
excluded *directory*, so a negation written under a bare `.claude/` is never consulted. Witnessed
2026-08-13 — with `!.claude/settings.json` added under `.claude/`, `git check-ignore -v` still
returned the `.claude/` rule as the winner and the file stayed ignored. Nothing errors, `git
status` shows no new file, and the `.gitignore` reads exactly like a working one. If you ever
re-tighten this, verify with `git add -An .claude/` — it must print exactly
`add '.claude/settings.json'` and nothing else. That dry run is the output-level gate; inspecting
the rules is only the input-level one.

**DEFECT FOUND AND FIXED WHILE WIRING THIS — backticked `Files:` tokens were invisible to
ABSOLUTE paths.** `**Files:** \`python/x.py\`` parsed with the backticks retained. The match rule
is substring-both-ways, so a **relative** target (`--file CLAUDE.md`) still matched — the plain
name is a substring of the ticked token — and `--file` looked perfectly healthy. An **absolute**
path, which is what a tool payload carries and therefore all the hook ever sees, matched
nothing. The backtick style entered around 2026-08-10 and had silenced **every entry written
since**: 61 tokens across 11 entries, 4 of them `tripwire` tier. Fixed at parse
(`known_state_status.scan` strips backticks). Witnessed A/B on absolute paths, delivered
entries: `build_discipline.md` 2→5, `CLAUDE.md` 9→13, `apparatus_instrument.py` **0→2**,
`issues/INDEX.md` **0→1** — the last two had been delivering *nothing at all*. **The general
shape, worth more than the fix:** the healthy-looking call and the broken one differed only in
path form, so the query that a human runs by hand (relative) and the query the machine runs
(absolute) were not the same query, and only the hand-run one was ever observed. Guarded by a
red-capable selftest control (#6).

**Pre-existing scope of the gap.** The same blanket line had left the `SessionStart`
activation-menu hook unversioned since it was written, so `CLAUDE.md`'s `[NEXT]`/`[GATE]`/`[PUSH]`
surface was machine-local too (degrading rather than breaking — the tokens still work from
CLAUDE.md, only the auto-printed menu was lost). Fixed by the same ruling.

## 2026-08-12 — [landed] OQ-290 RULED front-load by DOMINANCE (not by evidence); OQ-289 demoted to background; 5 of 19 memory files front-loaded and the method passed its own read-site test
**Files:** `ISSUES.md`, `python/apparatus_instrument.py`, `docs/technical/build_discipline.md`, `python/audits/oq290_frontload_check/`
**Tier:** landed

**The ruling and its TYPE.** Front-load every over-cap sibling memory file: first 4,096 B is a
self-sufficient summary plus a pointer. **Decided by DOMINANCE, not by evidence — OQ-289 is NOT
discharged and is not cited as support.** Front-load survives every branch (a summary in the first
4 KB sits inside the first ~50 lines, so it survives a 200-line cut too, pointer or no pointer), and
the exposed sets NEST (`NSp` 19 ⊃ `kae` 1), so acting on the 19 is correct without knowing which
constant binds. A ruling that reads as evidence-driven while its evidence is pending is the
mistyping §8.4 names. **OQ-289 demoted to Priority 6 background; no transport is to be built for it.**

**The method was tested at the read site, because "self-sufficient" is an authorial claim.** First
4,096 B of the rewritten `feedback_prereg_review_riders.md` → 3 fresh instances, no other context,
tools off: **13/13 topics, 3/3 instances, zero misses**, `turns=1` throughout. **Verdict carries its
asymmetry: *self-sufficient by unanimous instance report, NO DECLINE ARM.* ** One-sided positive —
nothing in the design separates a genuinely sufficient block from an instance reconstructing
plausible topics from an incomplete one. The negative arm (same probe over a deliberately
insufficient prefix) was not built, deliberately; a future campaign claiming the method *generally*
owes it.

**TRANSFERABLE DESIGN RULE from that `turns=1`:** the block's pointer is **descriptive** ("you can
act correctly on this block alone"), not **instruction-shaped** ("consult it whenever asked about
X") — and instruction-shaped wording is exactly what suppressed reports in OQ-289 smoke run 2
(OQ-292). **Front-load blocks must tell the reader they may act, never send them to fetch.**

**Two instrument corrections, both found only while wiring something adjacent:**
- The delivery readout computed delivered fraction from the **byte cap alone**, reporting **99% for
  a file delivering 61%** — the same byte-only error we had just corrected in three documents, still
  compiled into the instrument. Now line-cut-first, with arithmetic controls. Class recorded at
  `build_discipline.md` → *A correction landed in PROSE is not landed until every instrument
  encoding the same assumption is checked* (third instance of the same shape in one arc).
- Front-loading **lowers** delivered fraction (file grows, prefix does not) and pushed two files past
  200 lines, raising raw `kae` exposure 1 → 3. Without a flag the readout would have reported the
  ruling's own execution as a worsening. Files now carry a `front_loaded:` frontmatter stamp and the
  readout labels them and says the fraction is not a health metric for them.

**Declared cost, not mitigated:** front-loading rewards a good first 4 KB and lets the tail rot
unread — the accretion the 2026-08-10 consolidation cleared, now hidden below a cut line instead of
spread across files. **The readout cannot catch it: every file will be compliant.** Owned by the
monthly consolidation pass. If a future pass finds rotted tails, that is this ruling's falsifier and
should reopen it rather than be absorbed.

**Remaining: 14 files at 43–89% delivered, staged not abandoned**, threshold declared (everything
below ~45% is done; the rest goes to the monthly pass).

**CLAUDE.md SCANNED for the same defect on the largest channel (read-only, 2026-08-12).** The
pointer-wording rule generalizes past memory files to any always-loaded instruction, so `CLAUDE.md`
was checked for constructions that send a reader to fetch instead of letting them act. **Result:
substantially clean — the dominant pattern is the correct one.** The exemplar is the `[stack]`
load-chain tripwire: it says *"Each suite's correct chain is in its own file header — read it
first,"* **and then lists the three known extended chains**, so an instance that never opens a
header can still act. Tripwire-plus-pointer, as the file's own lean-docs rule requires.

**One weak instance, reported and NOT changed — it is a standing session-start directive and that
is the operator's seat.** *"Both are living documents — consult at session start, amend as decisions
are recorded"* (`## Project Context`, design-intent paragraph) is a fetch directive carrying **no
operative content of its own**. It has half the smoke-run-2 shape: a pointer with nothing actionable
attached. It lacks the other half — it fires at session start, when there is no task to displace,
whereas the punishing case competed with a specific request (*"consult it whenever asked about X"*).
So it is a **weaker instance, not the same defect**, and the honest reading is an attention cost
(two large documents unconditionally, most sessions will not need them) rather than a suppression
hazard. Named here so a future consolidation can weigh it; no edit made.

**RE-RUN 2026-08-12 with a POSITIVE CONTROL, after the file gained two more entries** (the memory
tripwire and synthesis move (7)) — the first scan used a hand heuristic with no control, and a
different hand heuristic in the same session had already produced a false negative (the Priority
splice: "exactly 1" was really 4). Detector separates **fetch-on-question** (the smoke-run-2 defect:
`consult X whenever asked about Y`) from **action-gated fetch** (the correct shape: `read X before
modifying Y`). **All four controls passed** — fires on the literal smoke-run-2 string, declines on a
known-good gate, declines on a bare pointer, and fires on a gate whose path contains dots (that last
control caught the detector's own first version, where `[^.\n]` excluded dotted paths and the gate
arm silently under-fired). **Result: 0 fetch-on-question, 6 action-gated.** CLAUDE.md is clean on
this defect, including both new entries — each states operative content with a trailing provenance
pointer rather than sending the reader to fetch.

---

## 2026-08-12 — [correction-key] OQ-286 RETRACTED (carrier misidentified): `CLAUDE.md` is not truncated — it is SKIPPED WHOLE above 4 MB, at 46× headroom. The recall channel is where a limit plausibly binds (OQ-289/OQ-290)
**Files:** `ISSUES.md`, `docs/amnesiac_institution/amnesiac_institution_v0_6.md`, `python/audits/oq289_recall_canary.py`, `python/audits/oq289_prereg_draft.md`, `python/apparatus_instrument.py`
**Tier:** correction-key

**What is corrected, and how any prior claim about it may now be cited.** OQ-286 asked whether the
always-loaded instruction set is silently truncated at load time. It is not, and the mechanism was
wrong as well as the margin:

| Path | Constant | Behaviour above it | Today |
|---|---|---|---|
| `CLAUDE.md` ("Project" read) | `R9o` = 4,194,304 B | **file SKIPPED WHOLE** + debug log + `context_claude_md_load` / `file_skipped_special_or_oversize` telemetry; **no partial content** | 91,029 B = **2.17%**, 46× headroom |
| `WEr(content, tag)` memory branch | `kae` = 25,000 B / `iJ` = 200 lines | truncated, `contentDiffersFromDisk: true` | `MEMORY.md` 9,906 B / 83 lines; **1 of 53** siblings over |
| `relevant_memories` (`QSp` → `PIe(…,{truncateOnByteLimit:true})`) | `NSp` = 4,096 B / `Npa` = 200 lines | truncated + appended notice + a `Read` pointer | **19 of 53** siblings over |

**There is no regime in which the rules at the end of `CLAUDE.md` are dropped while the rest
arrives** — the failure is all-or-nothing and it is *logged*, i.e. loud, i.e. not this program's
subject matter. Delivered always-loaded set measured at **102,695 B** ≈ 27.0k tokens (CLAUDE.md
91,029 / MEMORY.md 9,906 / global 718 / SessionStart hook 1,042), CLI **2.1.229**,
`~/.claude/settings.json` md5 `bc56274c`.

**Two arithmetic corrections to the Phase-0 read that produced this** (both found on re-witness,
both recorded because the numbers were about to enter the record): the delivered total is
**102,695 B, not 101,695** (the share column already implied the larger denominator), and the
sibling exposure is **19 of 53 siblings**, not "20 of 54" — that figure counted `MEMORY.md` as a
sibling, and `MEMORY.md` travels the *always-loaded* path, not the recall path.

**THE FINDING THAT SHARPENED THE QUESTION, and it is why OQ-289 refuses to close on a code-read.**
`WEr` has a **non-index call site** — `WEr(s.content,"memory")` — so the 25,000/200 pair has a
memory-*content* branch and not only an index one. **The two candidate pairs disagree by a factor
of nineteen about live exposure.** Which binds is unsettled, and the disposition of nineteen live
files rides on it.

**The asymmetry, recorded so it does not read as budget rationalization.** OQ-286 closes on a
code-read; OQ-289 refuses one. Justified by **consequence, not cost**: a code-read error at 46×
headroom must be wrong by a factor of forty-six to change the verdict, while a code-read error on
the memory channel misdirects the disposition of nineteen live files.

**How to cite this going forward.** "The always-loaded set may be truncating" is **retracted** —
do not repeat it, in the papers or anywhere else. "Recalled memory files may be arriving truncated"
is **`[UNWITNESSED]` with a named test** (OQ-289). The binary constants are **predictions to be
falsified**, not findings: they witness shipped code, not the path taken. Any verdict from the run
carries its altitude — *"truncates at N bytes per file, model M, CLI 2.1.229"* — because **five CLI
versions shipped in six days.**

**Credit, reduced and load-bearing.** The catch came from a **substrate read — observation from
outside the frame — not from the method.** The method's contribution was the `[UNWITNESSED]` tag
that got the read scheduled. *"Our discipline caught us"* is the Θ-7 sentence and is not available
here.

Landed: `c573fa0c` (ISSUES: retraction + OQ-289 + OQ-290), `76f96ecc` (paper §3.5 / §8.5 / §7.4.1
corrections and the dependent-site sweep), `17c4a599` (driver + staged prereg + reporting-only
delivery readout). Gate GREEN at each.

---

## 2026-08-12 — [tripwire] `python/audits/oq289_recall_canary.py` has spent NOTHING; the pre-registration is STAGED outside `audits/` and must be MOVED, not copied
**Files:** `python/audits/oq289_recall_canary.py`, `python/audits/oq289_prereg_draft.md`, `python/apparatus_instrument.py`
**Tier:** tripwire

**THE TRIPWIRE — three ways a fresh instance silently gets this wrong.**

1. **The prereg is at `python/audits/oq289_prereg_draft.md`, NOT in `audits/`.** A post-adoption
   audit dir with no `WRITEUP.md` turns `audit_writeup_gate` **red**, and a check red by
   construction at introduction teaches the institution to route around it. On run day, **`git mv`
   it** into `audits/<date>_oq289_recall_canary/PREREGISTRATION.md` — *do not copy*.
   `assert_spend_go()` refuses to spend while the staging file still exists, because two live
   copies of a frozen document with no queryable fact of canonicity is Pattern 2 performed on the
   freeze itself.
2. **The driver's header says NO MODEL CALL HAS EVER BEEN MADE. If that stops being true, change
   the header in the same commit.** The predecessor driver's header read exactly that for a week
   after a 219-call run — every word true when written, every word false the moment the run
   happened, in the file that guards against it.
3. **The delivery readout in `apparatus_instrument.py` is REPORTING ONLY and must stay that way
   until OQ-290 lands.** It contributes nothing to the return code by design. Promoting it to
   enforcing today would go red on 19 files and stay red until an Ω_P ruling nobody has scheduled.

**SMOKE RUN 2 IS SPENT (9 calls) AND ANSWERED THE FEASIBILITY QUESTION: NO.** Pre-registered row
fired — **`index n/n, sibling 0/n`: the recall ATTACHMENT path does not deliver sibling memory
files under `claude -p`.** `--tools ""` is not the cause; sibling content failed on every arm, and
on the `Read`-enabled arm it arrived only because the model **fetched it with a tool call**. **Arm A
as designed is NOT RUNNABLE and the pre-registration is NOT ready to freeze** — which is the
ordering ruling (smoke before freeze) paying for itself exactly as intended. Evidence:
`python/audits/oq289_smoke_run2/`.

**THE PROOF IS BEHAVIOURAL AND THE NUMERIC METRIC READ IT BACKWARDS — carry this one, it is the
inverse of our usual defect.** The readout printed `index 0/3`. The raw text has models emitting the
**exact absolute path of their own scratch memory dir and the sibling filename** — strings present
only inside the `MEMORY.md` we wrote, which a model cannot emit unless it was shown them. The index
WAS delivered. The metric scored a false ABSENT because the index entry's relevance wording
("consult it whenever asked about delivery-check tokens") reads as an **instruction**: models obeyed
it, went to fetch, and never reported the marker on line 1 of the file they were reading from.
**A prompt can suppress the report of canaries the model can plainly see.** We spend most of our
discipline on success-shaped absence; this is **absence-shaped success**, and it would have scored
as `DROPPED` in the outcome table.

**Two more instrument defects, both pre-freeze:** `observed_tool_calls()` returned **0 for every
unit including the three that demonstrably called a tool**, because `--output-format json` carries
no message stream — **a check that could not fire**, the exact converse of the `cache_read` gate
that could not pass, in the same driver on the same day (now returns `None` = UNMEASURED, with
`num_turns` as a declared proxy). And **Arm A′ is unimplementable as specified** — "measured as an
observed tool call" has no referent under `json`; `stream-json` is now a precondition of the arm.
Confirmed for free: `project_key()` is character-for-character correct, echoed back by the
harness's own naming.

**SMOKE RUN 1 IS SPENT (6 calls, 2026-08-12) AND DID NOT DISCHARGE ITS ITEM.** 0/3 on both arms,
replies well-formed. **The null was uninterpretable and the fault was in the probe**: it had no
positive control showing the memory channel could deliver anything, so "the marker did not arrive"
could not be separated from "we never gave the recall system anything to find" — the scratch memory
dir had **no `MEMORY.md` index** while the live dir has one, and `relevant_memories` is
relevance-selected per turn. Run 2 adds `SMOKE_INDEX`, the always-loaded positive control. Evidence
retained at `python/audits/oq289_smoke_run1/` **because it is the naturally-arising negative for
that control** — *fires at run 2, declined at run 1*, drawn from this project's own history rather
than an authored decoy. Two things run 1 did establish, both about the instrument:
- **`cache_read_input_tokens == 0` is UNSATISFIABLE under this transport** (all six units: 3,289 /
  4,479, with `input_tokens = 2` — the CLI caches the system prompt). Frozen as written that HALT
  would have voided **every rung** of the real run. Replaced by `DELIVERED_UNSTABLE_ACROSS_K`.
- **The token-slope instrument is sound and sensitive**: `delivered` perfectly stable across k=3
  within arm (9,002 ×3, 10,262 ×3, zero variance), inter-arm gap 1,260 tokens = the `Read` tool
  definitions exactly.

**Also from the full `WEr` body (read 2026-08-12) — two facts that change the kae branch:**
`WEr` applies the **line cut FIRST**, then the byte cut to the result. So
`feedback_prereg_review_riders.md` (25,373 B, **359 lines**) is bound by the 200-line cap, not the
373-byte overage: it delivers **15,451 B = 60.9% by bytes / 55.7% by lines**, losing ~39%. **Not a
hairline case.** And **`WEr` appends NO `Read` pointer** (only `PIe` does) — so on that branch
OQ-290's "accept truncate-plus-pointer" option **does not exist**, and Arm A′'s question is not
well-posed. Both are pre-registered as an interpretation commitment in
`python/audits/oq289_prereg_draft.md` §2b, written before the data.

**THE DRIVER'S CONTROLS FOUND FOUR DEFECTS IN THE DRIVER, and that is the strongest evidence in
this arc** — each fixed by wiring rather than exempting, each now two-sided:
(1) `orphaned_controls()` named `classify`/`slope`/`slope_band` on its first run — verdict
assignment had been deferred to the writeup, which left the instrument unwired **and** would have
let the analyst assign verdicts after seeing data; (2) the isolation guard compared dicts of
different shapes, so one clause **could never pass** — visible only to a converse control it did
not have; (3) gate 0 caught the filler generator unable to hit an exact byte target at high line
counts, i.e. exactly the rungs that de-confound `Npa` from `NSp`; (4) a full run minted one
`~/.claude/projects/<key>` per unit and removed none. **This belongs here and not in the paper —
the §3.5 credit claim is already correctly reduced and must not creep back.**

**Isolation fact a fresh instance will not guess: `--add-dir` is an instruction-injection channel,
by default.** `CLAUDE_CODE_ADDITIONAL_DIRECTORIES_CLAUDE_MD=1` is in `~/.claude/settings.json` as
of 2026-08-12 (verified by a three-arm before/after test) and gates `.claude/CLAUDE.md` and
`.claude/rules/` in the added directory as well as the top-level `CLAUDE.md`. Under a token-slope
instrument that is an uncontrolled payload landing **in the exact quantity being measured**.
**Context assembly is now a function of TWO files and only one of them was being watched** — hence
the settings md5 is pinned per unit on the same footing as the live `CLAUDE.md` guard. Do **not**
relocate `CLAUDE_CONFIG_DIR`: `~/.claude/.credentials.json` lives there and moving it likely breaks
auth, producing a failure unrelated to the hypothesis. A fresh scratch **cwd** is sufficient — the
harness keys the memory dir off the cwd, so a scratch cwd gets its own **empty** memory dir.

**Four defects were found in the driver by the controls built to catch them, and all four are the
reason the control set is what it is** (each now has a two-sided test): `orphaned_controls()` named
`classify`/`slope`/`slope_band` on its first run (verdict assignment had been deferred to the
writeup, leaving the instrument unwired **and** letting the analyst assign verdicts after seeing
data — fixed by wiring, not exempting); the isolation guard compared dicts of different shapes so
one clause **could never pass**, visible only to its converse control, which it did not have;
gate 0 caught the filler generator unable to hit an exact byte target at high line counts — i.e.
exactly the rungs that de-confound `Npa` from `NSp`; and a full run minted one
`~/.claude/projects/<key>` per unit and removed none.

**Ordering ruling (operator, 2026-08-12): SMOKE RUNS BEFORE THE FREEZE.** Smoke settles whether
Arm A is runnable at all; freezing a prereg that names an unrunnable test forces an amendment, and
an amended freeze is weaker than one frozen a day later. Legitimate only because smoke carries no
threshold information — 512 B / 10 lines, far under every candidate constant, one marker, no
position structure, scope declared in advance at `python/audits/oq289_smoke_scope.md` (md5
persisted with every artifact; `assert_smoke_go()` refuses without it), and **no verdict from the
outcome table is computed in `--smoke` mode**. Order is: smoke → resolve §10 → freeze → sweep.

Witnesses: driver selftest **88 PASS / 0 FAIL**; stub run 36/36 responses persisted, scratch dirs
cleaned to 0, live memory dir intact at 54 files; smoke run 1 6/6 persisted with usage blocks;
`apparatus selftest GREEN`; `./scripts/gate.sh` **GATE: GREEN**. Commits `17c4a599`, `e1c53d93`.

---

## 2026-08-12 — [tripwire] New protocol `agent/uke_referee.md` — do NOT name it UKE_AUDIT; OQ-277 evidence completed (Wu letter, empty-by-defect marker, stub run)
**Files:** `agent/uke_referee.md`, `agent/analysis/uke_audit.md`, `audits/2026-08-10_oq277_rq2_crosscoding/`
**Tier:** tripwire

**THE TRIPWIRE — two protocols, nearly one name.** `agent/uke_referee.md` (UKE_REFEREE) referees
**someone else's** substantive work — a paper or program not in our publication pipeline — and ends
in a letter to its author. `agent/analysis/uke_audit.md` (UKE_AUDIT v1.4) verifies **our own**
artifact's protocol adherence and grounding integrity against a UKE_G metadata block, and ends in a
compliance verdict. **Different objects; do not merge, and do not reuse the name.** The referee
protocol was first written *as* `uke_audit_architecture.md` and renamed on discovering the
collision — the OQ-278 shape, nearly minted inside a review protocol. It routes findings to the
canonical F01–F36 matrix in `uke_audit.md` Appendix A and mints no rival codes; §0.1 of the referee
file carries the comparison table. The wider suite (`agent/analysis/`: grounding → editing →
reality → audit → review) is a pipeline on our own artifacts; the referee protocol sits outside it.

**Validation posture, recorded once so it is not re-litigated (operator, 2026-08-12):** a protocol
is proven by the next person who finds it useful, not by an accumulated evidence base. No
provenance/outcome file is kept for it, deliberately — that would be apparatus about apparatus
(`build_discipline.md` → *Don't answer "does the apparatus pay for itself?" by producing more
apparatus*). Expect churn; its §11 carries only what changed and where it is thin.

**OQ-277 evidence completed** (detail in that entry and the audit dir, not repeated here): the
2026-08-11 letter to Wu filed with the evidence it reports and wired into the WRITEUP evidence map
— its **55% catalog-vs-dataset self-agreement** finding is a scoping constraint on the whole
experiment, since a confusion matrix against a 55%-self-agreeing reference cannot separate *our
taxonomy disagrees with Wu's* from *Wu's disagrees with itself*. `responses/EMPTY_BY_DEFECT.md`
added because **git does not track empty directories** — without it the 219-call failure vanished
from a fresh clone and became indistinguishable from "never reached this phase." The full-scale
**stub** run committed as the witness for SPEC §2.1 row 6, with a fail-closed requirement rather
than a warning label: any scorer must read the sibling `_run.json` and assert `mode == "live"`,
refusing on `stub` and on a missing/unparseable file.

---

## 2026-08-12 — [tripwire] P3 VACATED from the defect taxonomy (index 3 empty, never reused) — the ruling lived only in the paper for a day while CLAUDE.md published the old six; OQ-286/287/288 minted from the v0.6 review
**Files:** `CLAUDE.md`, `ISSUES.md`, `docs/amnesiac_institution/amnesiac_institution_v0_6.md`, `docs/concealment/concealment_without_a_concealer_v0_4.md`, `docs/technical/build_discipline.md`
**Tier:** tripwire

**THE TRIPWIRE — the taxonomy is FIVE patterns and index 3 is deliberately EMPTY.**
*Destructive-replace without proof* was demoted by operator ruling **2026-08-11** from a defect
pattern to a **witness rule** (*prove before you replace*, which survives intact — it is a
discipline, a thing one does, not a way systems fail silently). Four converging lines: no mechanism
text in `build_discipline.md`; no dated exemplar in any version, alone among the six; a shared
index; and **zero instances on a search built to find one** (`d0c3c5fb`). **Do not reuse or
renumber index 3** — every dated audit citing P4/P5/P6 would become ambiguous against its own
history; a visible gap is a checked fact, a silent renumber is a fork. `CLAUDE.md:492` now carries
the vacated marker; the §145 pointer reads "five live, index 3 vacated."

**The ruling also settles the scope question the OQ-278 sweep left open, on better grounds.**
Pre-discipline delete→restore episodes are excluded **independently of date**: *a deletion that gets
undone announced itself — the restoration IS the notice*, so a loud failure is not a member of a
taxonomy of absences-that-present-as-presences. Scoping to the post-discipline window would have
been circular; this is not. Per v0.6 §2.9 the account's exclusion and the ruling were reached
independently, before the account was written.

**WHY THIS IS A TRIPWIRE AND NOT JUST HISTORY.** The ruling lived **only in
`amnesiac_institution_v0_6.md` §5.2** for a day. `CLAUDE.md` — the file every instance loads — went
on publishing six patterns with P3 as a defect shape. Two documents, one taxonomy, no queryable fact
of canonicity, both internally coherent: **Pattern 2, committed by the ruling that resolved a
Pattern-2 instance.** The standing lesson: **a ruling recorded in a paper is not recorded.** Papers,
`ISSUES.md`, and the always-loaded rules are separate read sites; a ruling has to land at each, and
the always-loaded one is the one that changes behavior.

**Left un-acted, needs one line from the operator.** Vacating CLAUDE.md's index 3 leaves
`build_discipline.md`'s Pattern 3 (*Bound-probe bypasses clause-order*, a different mechanism with
worked text) as the sole claimant at that index — so "Pattern 3" arguably now resolves
unambiguously to bound-probe. **That is an inference from the ruling, not part of it**, and it was
not acted on. Index 4 remains genuinely collided (two claimants). Both in OQ-278, now `partial`.

**Minted from the v0.6 review.** **OQ-286** (Priority 1, edge-free, runnable today) — the canary
test: the always-loaded set may be silently truncated at load time, and the party positioned to
notice is the one that structurally cannot; v0.6 §3.5 carries it `[UNWITNESSED]`. Enlarging
CLAUDE.md makes it *more* likely the tail is dropped, which makes rule ORDER a design variable.
**OQ-287** — two live papers claim the same contribution (v0.6 §2 and the concealment paper, whose
v0.1 was extracted from it); canonicity markers added to both headers pending the split ruling.
**OQ-288** — Prediction 4 / the instrument stratum, with RQ-d folded in rather than minted twice.

**One correction landed in v0.6 §14.** RQ2 called blind cross-coding *"a weekend of work"*. That is
OQ-277: 219 calls spent, nothing persisted, new stamp required, scorer nonexistent. Corrected in
place — an agenda item priced before the attempt and never re-priced after is a staleness-ladder
instance in the paper that teaches the staleness ladder.

---

## 2026-08-12 — [landed] Concealment papers committed; OQ-283 control returns SEPARATION (scoped); OQ-284/285 minted; CS sketch §2.5 formalization claim corrected; v8 gains Oracle-Gap Corollary 4.1
**Files:** `docs/concealment/`, `docs/amnesiac_institution/amnesiac_institution_v0_5.md`, `docs/amnesiac_institution/amnesiac_institution_v0_6.md`, `ISSUES.md`, `docs/commitment_systems/commitment_systems_sketch_v6.md`, `docs/deferential_realism_paper_v8.md`, `audits/2026-08-12_oq283_framing_boundary_discrimination/`, `audits/README.md`
**Tier:** landed

**Step zero.** `docs/concealment/` was entirely untracked (4 versions + 2 diffs, 0 files in the
index), as were amnesiac v0.5/v0.6 — so any OQ citing them would have pointed a cold reader at
nothing. Committed `1265d0c1`. The operative document is
`concealment_without_a_concealer_v0_4.md`.

**OQ-283 (the gate on everything else).** The concealment material reads as a candidate third axis,
but `seat-theorem-v1.md` **§8 already makes framing Π a seat** and **Corollary 2a** already makes
declared-vs-concealed the operative distinction — so the unguarded-axis-swap rule owed a
pre-registered discriminating control before any relabel. Criterion frozen (md5
`f060250f6b6f22745809963b86eb727b`) before any classification: *at production time, did any party
hold the framing as a datum they could have written down?* Six naturally-arising in-repo instances
→ **3 NOT-HELD, 2 declines, 1 seam**. **Verdict SEPARATION**, scoped: 2a's imperative has no
addressee when the framing *was* the operation.

**The scope limit is load-bearing and a cold reader will get it wrong.** SEPARATION is separation
from Corollary 2a — **NOT** the existence of a third axis. **v8 §5.2's declared exterior** (the
relational layer) is at least as good a home, since a boundary is an *edge between positions*, not
a position; and §5.2 pre-committed that the exterior declaration *"cannot later be invoked only to
dismiss an inconvenient axis,"* so that clause now binds. **Any edit to v8 §5 or the CS sketch's
axis sections is gated on an operator Ω_C ruling.**

**Correction owed to the PAPER (the audit's only novel finding).** `system_gradient`'s `[] → 0.0`
classifies HELD under the criterion (someone held "this is a default") but sits in Mode 2 in the
paper's §5.3, defined as *"no channel existed at the boundary."* Datum-possession and
channel-existence come apart; the paper draws its Mode 1/2 line on the first and defines Mode 2 by
the second. Filed in OQ-283, not applied — the paper is the operator's document.

**Engine finding, now tracked as OQ-285.** `stakeholder_seats.pl:337-341` maps a seat whose type
derivation fails to `unknown`; `is_real_type/1` filters it, so the H¹ *"counts it as neither
agreeing nor disagreeing"* — and the same token carries a seat deriving a literal `unknown`, both
collapsed as *"untypeable for pair-counting purposes."* **The disagreement measure is silent
exactly where a position is blind.** Coverage survives (`NSeats`/`NReal` in-band); the *kind* of
non-reality does not. Repair pattern already exists one layer over:
`sheaf_undetermined_reason/2`.

**CS sketch corrected in place (v6.1 delta, one section + a lineage row, per its own §1.2 rule).**
§2.5 asserted that positions are *"systematically unable to see"* drift and cited presheaf
machinery as the formalization. It is not one — H¹ measures disagreement among positions that each
produced a reading. The unable-to-see claim is now marked **UNFORMALIZED**.

**v8 → v8.0.1, addition only.** Corollary 4.1 under Theorem 4: *a local section masquerades as a
global one whenever the positions have not varied enough* — H¹ = 0 is a statement about the site,
not the world. Euclid was already there as the formal instance (§5.9); scurvy is added as the
empirical one, where the failure to glue was misread as *the claim was never true* rather than *we
found the boundary of a local section*. No prior claim revised.

**Ω_C RULING TAKEN SAME DAY — EXTERIOR.** Boundary framing-loss is **not** a third axis; it is
exterior to seat/gauge/orientation. Recorded at v8 §5.2 **with its reason**, because that section
pre-committed the exterior declaration *"cannot later be invoked only to dismiss an inconvenient
axis"* — and the order is the point: exterior was chosen only **after** OQ-283's control
disqualified the easy dismissal. Reason: a boundary is an **edge between read-sites**, not a
position over content; the audit-direction discriminator returns nothing on it. **Two precisions a
later instance will get wrong without them:** exterior is a **category, not a location** — this
does NOT place framing-loss in `affects_constraint` or the contamination network (the exterior now
has two members of one kind: relations between *constraints*, relations between *read-sites*, and
wiring the second into the first is a category error with a plausible implementation); and
exterior ≠ unimportant — it means outside this ontology's jurisdiction.

**The seam is now documented AT THE READ SITE, not only in the tracker.**
`concealment_without_a_concealer_v0_4.md` §5.3 carries a marked editorial note: the three-mode
table sorts by *who could state it* (datum-possession) while Mode 2 defines itself by *"no channel
existed at the boundary"* (channel-existence), and the two come apart on the paper's own `0.0`
case. Repairs differ — a tagged union fixes held-but-no-channel, only stepping outside the
operation fixes held-by-nobody — so one cell routes two prices to one budget. Two candidate
dispositions offered, **neither adopted**; disposition is the author's.

**OQ-285's code half is GATED (operator ruling).** No code — not the reason token — until (1) a
**fresh instance** reviews the evidence and produces a *recommendation*, (2) the operator checks it
against **Claude web** (independent, no repo access, which is the instrument not a formality), (3)
only then the code question opens. The receiver's prompt is written out in the OQ with six
enumerated actions and the **license to refuse** stated, per *Write the receiver's prompt* and *The
receiver's license to refuse*. A fresh instance is required because the one that wrote the entry
read the paper before the code and arrived holding the paper's categories — the concept→surface
mapping is the most likely thing to be wrong.

**Process defect, instance seven.** A result block was written into the audit log **before any
instance was classified** — a prediction in a finding's costume. Caught pre-commit, struck, and
preserved in `audit_log.md` (catching early destroys the free before-commit control, so the text
is kept deliberately). It was wrong in the informative direction: it claimed 3 clean declines where
the evidence gave 2 and a seam — and the seam is the audit's only correction to the source paper.

---

## 2026-08-11 — [landed] OQ-277 live run SPENT 219 CALLS AND PERSISTED NOTHING; capture path built, two orphaned controls removed, NEW-STAMP ruling; OQ-278 P3 sweep executed; paper v0.4 assembled
**Files:** `ISSUES.md`, `CLAUDE.md`, `docs/technical/build_discipline.md`, `python/audits/oq277_crosscoding_driver.py`, `python/audits/oq277_build_prereg.py`, `scripts/gate.sh`, `audits/2026-08-10_oq277_rq2_crosscoding/`, `docs/amnesiac_institution/amnesiac_institution_v0.4.md`, `docs/amnesiac_institution/V04_CONSOLIDATION_MANIFEST.md`
**Tier:** landed

**The event.** Spend-go was given at the frozen prereg (`a7327e33`, md5 `4118f64e`, 73 items /
219 calls). The live run (`edc90409`) made **all 219 calls and wrote zero answers** — the driver
had no response-writing code path at all. All three gates it passed were **input** gates (count
captured payloads, count fixtures, leak-sweep); the run printed its expected totals and reported
green throughout. Two true sentences described a distinction the code never implemented (the
`--dry-run` help said "do not write responses/"; the console said "responses/ left empty"), so a
reader checking for persistence found evidence the question had been considered and no persistence.

**Repairs, all witnessed.** `cb1b33e5` capture path + output gate (raw text persisted per call
before parsing, write-then-verify per unit, gate 4 asserts count AND non-emptiness AND
parses-to-vocabulary); `4e0d8725` capture-dir invariant relaxed from emptiness to run-id
provenance; `a3deae1d` removed two controls whose functions `run()` no longer called — **four
green selftest lines wired to nothing**; `508222ab` post-freeze mode for the prereg checker (before
it, expected source drift and actual tampering both printed RED); `c2304218` gate row detecting
that the frozen prereg is unaltered (expires at OQ-277's close). Driver selftest: **27 controls,
0 failures**. Gate GREEN including `oq277 freeze` (stamp verified, selftest 7/7).

**Three disciplines minted into `CLAUDE.md` + `build_discipline.md` from this arc** — *Gate the
output, not only the input* (`cb1b33e5`, priced at 219 calls); *A control must witness that it is
CALLED* (`46ba44ce`); *When a defect is found, its before-commit is a free negative control*
(`ea936d2c`). Also `03fda56e` *The receiver's license to refuse* and `f95fc857` *A positive control
demonstrates DISCRIMINATION, not detection*, both with their own entries below.

**Operator ruling — NEW STAMP (`d4946b90`).** Evidence in `59e5bab5`: the driver appears 0 times in
the freeze's PINNED manifest (the freeze never reached the instrument, so its GREEN carried no
information about whether the run could produce data); the analysis half — H5 scorer, overlap-pair
identification, matrix construction, redaction-floor scoring, (iii′) row — **does not exist in code
or in the frozen design**, so even a perfect capture run would have produced 219 unscoreable
answers; two pinned sources have drifted. `4118f64e` is **retained, not superseded**, as the record
of a design that could not produce its own result, and the next stamp must cite it by md5. The
gating artifact is `SPEC_next_preregistration.md`; **no spend is requested until its §3 is
discharged.** Its §1 names the mechanism: the old manifest pinned sixteen artifacts, every one a
text, because the unstated selection rule was **genre** — pin what reads like a specification, not
what runs.

**Correction, same day (`19bc3418`):** commit `f0e91cc0`'s message claimed "verified GREEN" when
the check it ran printed RED. The reasoning was sound, the check was run, and the *result* was not
read before the claim was committed — in the very commit recording five prior instances of that
shape. Sixth instance; caught the same way as the other five, by comparing a claimed state against
the artifact.

**OQ-278 (`d0c3c5fb`).** P3 failure-shape sweep, branches pre-registered before the run: 19
destructive commits (5 post-discipline, 4 prevention records, 1 non-deletion), 3 delete→restore
episodes **all pre-discipline**, zero post-discipline. Lands on branch 2 for the post-discipline
population, but the disposition ruling stays **open** — the branches never said whether
pre-discipline instances count, which is decision-relevant (the 133-file Feb episode is a witnessed
instance of the shape if they do). Tracked as `blocked_on_human oq278-p3-prediscipline-scope-ruling`.

**Paper.** `docs/amnesiac_institution/amnesiac_institution_v0.4.md` assembled (`acdd1b73`) off a
consolidation manifest (`eeab8a33` 28 items / 8 producers, second-reader pass `ede866c7` → 35 items
after the frame was found to exclude a producer). §6.4 takes the recursion from the **results**
section of `audit_log.md`, never from `verdict_grammar_amendment.md` — that file is incorporated
verbatim into the frozen prereg and editing it now that results exist would invalidate the freeze
retrospectively.

---

## 2026-08-11 — [tripwire] A positive control demonstrates DISCRIMINATION, not detection — the grades of decline, the record-not-per-run rule, and the role-reuse silent failure
**Files:** `CLAUDE.md`, `docs/technical/build_discipline.md`, `docs/design/design_discipline.md`
**Tier:** tripwire

**Ruling (operator, 2026-08-11).** Named as a consistent failure mode of Claude models (and likely
not only them): the demand for a positive control is *heard* and then discharged with a **plant** —
construct the case the probe must flag, watch it flag, declare the instrument controlled. But
planting the target shows only that the instrument **can fire**; the claim being made rests on
discrimination. **Only a case the instrument DECLINED shows that its firing carries information.** A
control with no decline available is one-sided and licenses nothing, however well the plant worked.

**Grades of control, strongest first** (the operative part — a control is reported at its grade, not
as a binary "controlled"):
1. a case the instrument **declined in its own history** (not designed to be declinable, so it cannot
   be tuned to the instrument's known weaknesses);
2. a **naturally-arising negative** drawn from the population (carries the real near-misses);
3. an **authored decoy** — weakest, and precisely bounded: *a decoy shows only that the instrument
   can reject authored decoys*, because its author writes to their own model of the boundary.

**Attachment point (changes what is owed per run).** The control attaches to the instrument's
**discrimination record**, not to each run: cite the record and show the current application is *in
distribution* for it (same input shape, population, role), rather than re-planting every turn.
Re-planting is busywork that re-proves detection. The record **lapses** when the application drifts
out of distribution — new population, new input shape, changed engine tokens (cf. *Instrument
vocabulary rots*), or a changed role.

**Two verdicts that were previously handled as caveats.** No decline available anywhere in the
population ⇒ **the question is unanswerable from this corpus** — declare it / route to a typed Ω or
`design_gaps.md`, do not ship the finding under a noted-absence caveat. No positive control possible
**even in principle** ⇒ a verdict on **whether the category may be added at all**, not a limitation
to work around (a category nothing could fail to be routes nothing — the liveness test applied to the
taxonomy; landed as `design_discipline.md` §5 → *A category whose positive control cannot exist is
not admissible*).

**Silent failure — validated in one ROLE, reused in another.** *The error profile is a property of
the role, not the instrument.* A matcher validated as a **detector** (false positives conservative:
they widen a net a human then reads) becomes **silently decisive** as a **selection metric**, where
the same false positives now choose with no downstream reader. Nothing about the instrument changed;
the reuse looks like thrift rather than a new claim. A cross-role reuse is a NEW instrument owing its
own decline under the new role's error profile — the time-displaced twin of the pipeline-local
*screen-controlled ≠ rubric-controlled* rule. Repo-side instance to watch: commentary-grade →
correction-grade promotion in the override layer.

**Landed:** full section in `build_discipline.md` (*A positive control demonstrates DISCRIMINATION,
not detection*, placed between *Every diagnostic needs a positive control* and *A consistency check
is not a discrimination check*, with a forward pointer from the former); compact tripwire promoted
into the `CLAUDE.md` Build Discipline "Diagnostics are not exempt" paragraph; admissibility clause in
`design_discipline.md` §5. No code touched.

---

## 2026-08-11 — [tripwire] "Write the receiver's prompt" minted as a construction: a handoff is a specification test (stated-versus-instructed)
**Files:** `CLAUDE.md`, `docs/technical/build_discipline.md`, `audits/2026-08-10_oq277_rq2_crosscoding/packets/escape_units/PREREGISTRATION_threshold_calibration.md`
**Tier:** tripwire

**Ruling (operator, 2026-08-11).** Asking the outgoing instance to write the incoming one's prompt
is a **control, not a habit** — minted rather than left as one instance's personal practice, because
it is transferable and was undocumented. Before declaring a plan, prereg, ruling, or design done,
write the prompt the next instance would need to execute it.

**Mechanism (the transferable part).** Re-reading a rule exercises *recognition* — a rule correct in
prose passes every time. Writing an instruction exercises *enumeration* — it forces the operational
half the design never named. Same shape as the OQ-277 ledger's §L *stated-versus-counted* table, one
layer up: **stated-versus-instructed**.

**Fired 3× in one session** (OQ-277 escape stratum): (1) writing the assembler prompt surfaced a
pre-registration that pinned *what is judged* and never *what is shown*, leaving item presentation —
including whether the judge sees the extractor's own reasoning and the stratification key — to the
assembler, where it would have changed the result (fixed as prereg Amendment 4, before assembly);
(2) converting the prose ruling "two candidates and two primaries" into an executable draw exposed
that at n=2 an unstratified draw could make placement indistinguishable from the threshold under
test (fixed by stratifying); (3) running the handoff's own stated self-check command showed it could
not consume the format that same handoff specified (the adjacent rung, *stated-versus-executed*).

**The way it fails silently:** a prompt that says "read the design and execute it" performs no
enumeration and catches nothing, while feeling like the rule was discharged. The operative test is
*could a receiver who read only my prompt take a wrong-but-reasonable action the design forbids?* —
every yes is a DESIGN gap. Does not work when the receiver is yourself (no enumeration pressure),
which is why it is a handoff rule; and it finds underspecification, never wrongness.

**Corollary — the terminal is a channel.** When the receiver's output is visible to a party the
design blinds (an assembler working in the judge's terminal), the prompt must state what may be
PRINTED, not only what may be done; a correctly assembled blind packet echoed to the screen is
unblinded before the pass starts, and no downstream control recovers it.

Full exposition + witnessed instances: `docs/technical/build_discipline.md` → *Write the receiver's
prompt*. Tripwire promoted to CLAUDE.md Build Discipline.

---

## 2026-08-10 — [correction-key] OQ-78 idiom half read (pre-registered, zero-spend): CELL 2, no close — the `.x8` rail is FAMILY-bound not model-bound; the 0.68 point mass dissolved while the rail held
**Files:** `ISSUES.md`, `issues/INDEX.md`, `issues/INDEX.json`, `audits/2026-08-10_oq78_idiom_close/`, `python/audits/oq78_railband_crosstab.py`, `outputs/pipeline_output.json`, `prolog/testsets_haiku/`, `prolog/testsets_flash/`, `prolog/testsets_kimi/`, `prolog/testsets_sonnet/`, `prolog/archives/datasets/kernel_v2_test2/`
**Tier:** correction-key

**Verdict.** Condition (ii) HOLDS (worst-pair AUC 0.886 vs threshold 0.635); **condition (i) FAILS**
— sonnet-5 concentrates on `.x8` *within* its claimed_type bands (pooled concentration 0.331 vs
pre-committed floor 0.25) on all three scored types. Cell 2 fired: **no close licensed**, OQ-78 stays
`partial` and now routes BLOCKED ON YOU for a close-semantics ruling. Prereg md5
`384e68bbac80e0959dba1294a6f6ee87`; commit `bfbe52ea`.

**How ε-prevalence figures may now be cited.**
- **Never pool ε statistics across author strata, and never across generation regimes within one
  model.** Witnessed: default-leg sonnet-5 (c-orchestrator topic runs) and `testsets_sonnet` (bulk
  kernel build) were kept separate and proved concordant (0.342 vs 0.331) — that was *measured*, not
  assumed, and is the only reason the pooling question is settled for this pair.
- **A model swap is a re-baseline event on the ε axis.** Every figure below is leg-and-model-indexed
  and transfers across nothing.
- **The idiom half is TWO things with opposite trajectories** — cite them separately from now on. The
  0.68 point mass **dissolved** (archive 30.0% → sonnet-5 **7.3%**; distinct ε 13 → 52); the
  `.x8`/`.x2` rail **held** (91.7% → 78.2%) with mass shifting between its arms (`.x8` 76.7→42.8,
  `.x2` 15.0→35.5). sonnet-5's marginal is a **flattening within the rail, not a rail absence**.
- **Model-boundedness is measured, not inferred**, on 957 matched seeds (one seed set re-authored per
  model — topic and claimed_type mix held fixed by construction): four models, three argmax digits —
  Claude→8, kimi→2, flash→5; `tv_model_digit` 0.365 vs label-permutation null max 0.045; all-agree
  rate 1.25%. Every Claude-family population lands on 8, incl. two with a **zero-story id
  intersection** (archive 0.767 / sonnet-4.5 derived 0.766). Only flash leaves the rail.
- **This was matched-seed ACROSS MODELS, never a "matched-seed arm."** Seed fixed, model varied,
  feeding constant on all four legs. The hypothesis-withholding falsifier (Ω_E) stays **unpurchased**.
- **Dead premise corrected:** OQ-78's pinned graduation (zero-spend cross-arm read over OQ-109 Phase
  C) assumed a 60-seed regen that was **descoped to a 5-seed pilot** (OQ-109, 2026-06-13). OQ-117's
  "SEQUENCED AFTER this read" is discharged.

**Two instrument defects caught pre-freeze (both Pattern 5 — gate passes because its input is
degenerate).** (a) Condition (ii)'s originally-pinned p10–p90 interval-overlap measure scored **1.0
on the comparator itself** (archive rope p90 = 0.68, dragged by the three documented kernel-reading
exceptions), so its bootstrap threshold calibrated to 1.0 and *every possible banding would have
passed*; the p25–p75 variant is vacuous the other way (0.000 on all four non-test legs). Replaced by
worst-pair AUC — the only candidate that varies across legs. (b) The pinned minimum cell n=5 sits
**below the uniform null median** (p50 0.300 at n=5, 0.200 at n=10), so small cells fire on noise;
raised to 50, with the floor set to just admit the weakest true positive (kimi 0.281 vs floor 0.25).

**SPLIT RULING (operator, same day) — OQ-78 closes, the rail moves to OQ-281.** The idiom half was
tracking **two objects with opposite trajectories** under one name, so it splits rather than closing
whole. **(a) The point-mass component is RESOLVED** — tracking question (a) answered in the
direction the OQ posed it: the 0.68 mode **diluted, not entrenched**. **This retires the ε-axis
compression caveat as originally stated**: "a third of the corpus shares one ε value, so the axis is
low-variance and χ is f(d)/σ(S)-driven" was true at 30–34% and is **not** the operative constraint
at 7.3% with 52 distinct values. Text still citing ε compression as a live limit on ε-keyed
denominators must be re-read against that number; what survives is the *bin-resolution* limit, a
construction fact rather than a prevalence one. **(b) The rail component is OQ-281**, re-scoped from
"is this rail the model's own?" (answered: no, inherited) to "where is the boundary of the family
that shares it?" — held open because the family map contains **no Claude model off 8**, so
family-sharing is consistent with the data and un-falsified by it; closing there would be a close on
an absence.

**Arm-structure characterization (exploratory, no falsifier).** `python/audits/oq78_rail_arm_structure.py`.
The `.x2` arm is **not** the dissolved point mass spreading — if it were, its gain would sit at
0.62/0.72, and for sonnet-5 only 14.1% does (its `.x2` mass is at 0.42 and 0.52). Arm concordance on
matched seeds, **restricted to same-claimed_type seeds and shuffled within type** to partial out the
type channel: **haiku × sonnet-5 excess +0.055, above p99**; **kimi × sonnet-5 excess exactly
+0.000**. So arm choice carries seed-level signal *within* the Claude family and none across
families — modest in size (56.5% vs a 51.0% baseline), and to be reported as real, not strong.

**CORPUS PROPERTY (2026-08-10) — marginal-independence and paired-comparability are MUTUALLY
EXCLUSIVE outside the twin legs.** Promoted to CLAUDE.md Critical Distinctions; provenance here. A
population independent enough to be a clean **marginal** known-positive is, by that same
independence, **unusable for a paired read** — a structural property of how the corpus was built,
not a gap to fill. Witnessed both directions in one pass: archive (n=60) and default-leg derived
sonnet-4.5 (n=64) landing on the same ε digit at nearly the same concentration from a **zero-story-id
intersection** was the strongest evidence in the OQ-78 calibration pass, and that same zero
forecloses any paired within-family check between them. Every non-twin Claude population shares **0**
ids with the twin legs (sole exception: default-leg haiku-4.5 × sonnet leg, 28 ids / 20 both-on-rail,
under the n=50 floor). **`testsets_haiku|flash|kimi|sonnet` (957 four-way matched) are the ONLY
matched-seed structure in the project** — which is the only reason OQ-78's primary paired statistic
existed at all. **Check the id intersection FIRST when designing any paired probe;** outside the twin
legs a paired design is dead on arrival, and a new matched-seed leg is a generation spend, never a
re-read. (This corrected a live recommendation mid-session: the OQ-281 arm route was proposed as
"readable on data already on disk," true of the characterization and false of the strengthening step.)
**Declared as GAP-35** in `docs/design/design_gaps.md`, which also records the join basis: the twins'
pairing is a `constraint_id` filename join; `cs_story_uid` deliberately does NOT join (0/956 across
twins); `seeded_from` is unemitted; and the kernel-level `cs_kernel_id` join reaches 331/331 across
twins but **0** against every non-twin population (default-leg sonnet-4.5 8 kernel ids / 0 shared;
archive `kernel_v2_test2` has no kernel ids at all). Pairing fails at BOTH join levels outside the
twins, which is why OQ-281's arm route needs a generation spend rather than a re-read.

**GENERALIZABLE CHECK (operator, 2026-08-10) — evaluate a measure on the comparator itself before
freezing it.** Both Pattern-5 catches this session came from the same move: running the candidate
measure on the known-positive/known-negative populations **at the actual cell sizes** before the
freeze. **A separability or concentration measure that has not been evaluated that way is not a
pinned condition yet — it is a name for one.** It caught two live defects in one pass: a measure
that scored its own pass-value on the comparator (so every input would have passed) and a floor
sitting below the null median at the smallest admitted cell (so noise would have fired it). Cheap,
mechanical, and it runs before any test datum is visible. Detail:
`docs/technical/build_discipline.md` → *Every diagnostic needs a positive control*.

**Method notes for a re-run.** All four twin legs were classified zero-spend via serialized
`classify_corpus` (~60–90s each; they share the raw artifact — do not parallelize). Leg-model
attribution is verified from `story_provenance` and `classify_corpus`'s fingerprint refusal, **never
the directory name**: `testsets_kimi` is `kimi-k2.6` while the default leg's kimi stratum is
`kimi-k3` — different models, never pooled. The default leg moved **243 → 249 mid-session** under an
operator topic run while all four twin-leg md5s stayed byte-identical, so it was frozen as a slim
slice (`evidence/pipeline_output.frozen.slim.json`) before it could shift the band grid.

---

## 2026-08-10 — [landed] OQ-277..280 minted + RQ2 cross-coding Phase 1: taxonomy fork and phantom §2.3 coding verified; Wu's own taxonomy fails to reproduce against itself (55%)
**Files:** `ISSUES.md`, `issues/INDEX.md`, `issues/INDEX.json`, `audits/2026-08-10_oq277_rq2_crosscoding/`, `docs/amnesiac_institution/amnesiac_institution_v0.3.md`, `docs/technical/build_discipline.md`, `CLAUDE.md`
**Tier:** landed

Executed step 0, step 1, and Phase 1 direction (i) of the RQ2 blind cross-coding plan
(Wu, arXiv:2606.14589v1 × our published P1–P6). **No model call was made; the experiment
is pre-spend.** Handoff to a fresh extractor at
`audits/2026-08-10_oq277_rq2_crosscoding/HANDOFF.md` — direction (ii) needs ~184 KB of
audit prose read carefully, and skimming to fit would thin the units, biasing them toward
`other` and confounding the very redaction-bias control (c) that exists to measure it.

**Two plan-stage findings VERIFIED before minting** (not restated from the plan):
- **OQ-278, the taxonomy is FORKED.** `CLAUDE.md` and `docs/technical/build_discipline.md`
  share P1/P2/P5/P6 but disagree on P3 and P4 (destructive-replace + recap-as-witness vs
  bound-probe-bypasses-clause-order + fabricated-default). Four disjoint members, all with
  dated exemplars. v0.3 §4.3 publishes the CLAUDE.md set. Pattern 2 on the pattern list.
- **OQ-280, §2.3 describes a coding that produced nothing.** No file assigns a P-label to
  an audit dir / OQ / incident as a data row; the only multi-P files are the taxonomy's own
  prose plus three unrelated vocabularies (blind-pass position roles, numbered predictions,
  a 2026-05 trifurcation audit). Appendix B's §4.5 row is a keyword proxy assigning no
  pattern. Positive control: the same searches fire on §4.3's own P-table (6 labels / 20
  occurrences), so the null is about the repository, not the probe.

**Three catches before any spend** (this is why the audit's `Fired:` bit is `live`):
(1) the audit's own directory landed in the escape-check sampling stratum — the coder
would have been asked to code the experiment coding it, and the payload could never have
passed its own leak-grep; (2) an empty untracked placeholder directory sat in Appendix B's
§4.5 DENOMINATOR (73/175 → 73/174; headline 42% unchanged); (3) the leak-matcher's first
pass fired a false positive — "permission *class b*y default" matched banned `Class B` by
substring — fixed to word-boundary regex, positive control 4/4 on a planted leak.

**R2 (the result, and it did not need the experiment).** Wu's `failure_modes_catalog.md`
and `llm_observer_ground_truth.yaml` carry the SAME 22 incident ids and assign different
classes to 10 — **55% self-agreement**, single author, own system, own taxonomy, complete
postmortems in hand. Per-class totals diverge on every class, both summing to 22. The
disagreement is **systematic, not noise**: only 5 of 10 possible class pairs are occupied,
**E is the hub at 8/10** (bidirectional — absorbs 3, sheds 5), B/E modal at 4/10, D most
stable at 1/10. A pre-registered guess that C/D would be the seam was WRONG and is recorded
as wrong. Mechanism: E is defined by *declared ≠ runtime*, which cross-cuts the
failure-mechanism axis the other four sort on. **Consequence for v0.4:** §12 rests on
institutional novelty because Wu was the near-twin threatening the taxonomy claim; if Wu's
taxonomy does not reproduce against itself, the twin is far less threatening — not because
our six are better, but because neither set is stable enough to support a priority contest.
That supports the §5.3 convergence framing from an unexpected direction. Scoped honestly:
the two records' relative authority is unstated, so the claim is *two records that each
assign one class per incident, over an identical set, agree on 55%*.

Frame frozen (174 dirs = 73 + 101, partition exact, md5-asserted sampler, self-excluded);
seed 20260810; Wu catalog + dataset frozen under `packets/wu_source/` with md5s. Gate GREEN
13/13 at close. Commits `8d7e5aba`, `ced94432`, `9df378ad`.

## 2026-08-10 — [landed] Monthly consolidation MAJOR PRUNE + apparatus instrument: memory 113→53 files (Feedback channel capped at 33, gated exchange), KNOWN_STATE window residue 52/52 drained, ISSUES backlog 100/139 drained, Fired: catch bit adopted — OQ-276 minted
**Files:** CLAUDE.md, KNOWN_STATE.md, ISSUES.md, python/apparatus_instrument.py, scripts/gate.sh, audits/README.md, python/check_gap_status_surfaces.py
**Tier:** landed

Session opened with the operator's question "is the confirmatory apparatus earning its keep
or is it LLM make-work?" Assessment verdict: the apparatus FIRES — 73/175 audit dirs document
a silent/never-fired defect (CORRECTED same day: first stated 77/175, a `cut -d/ -f2` unit
error counting unique filenames not directories; caught discharging the v0.3 numbers
manifest); the July–Aug commit log carries kills, retractions, and
corrections of the apparatus's own headlines — but the discipline stack accreted monotonically
with no catch-rate accounting, making the ritual unfalsifiable by its own standards. Operator
ruled: run the monthly compression, then instrument the apparatus with a finite channel +
gated exchange sized by the prune's end state.

- **Memory prune** (out-of-repo, `~/.claude/projects/.../memory/`): 113 content files → 53.
  ~25 deleted as promoted-to-repo-docs or stale (incl. `invariant_analysis`, which still
  stated the retired χ=ε×π law); ~35 merged into 11 cluster files (biggest: 12 prereg/gate
  rider files from the 2026-08 arc). Dominant excess: DUPLICATION-AFTER-PROMOTION (rules
  promoted into repo docs whose memory copy was never retired) + per-OQ rider accretion.
  Pre-prune backup beside the memory dir. Feedback end state = 33 entries = the channel cap.
- **Apparatus instrument** (commit `bfee1b06`): `python/apparatus_instrument.py` in
  `scripts/gate.sh` — per-audit `**Fired:** live|latent|no` catch bit (dirs > 2026-08-10;
  spec in `audits/README.md`), feedback-channel cap 33 (gated exchange), selftest rides every
  run; rate reported never gated. Standing readout: OQ-276.
- **KNOWN_STATE roll-off residue** (commit `011fc2bb`): all 52 window tripwire/correction-key
  entries promotion-tested and compressed (5,710→4,796 lines, token-preservation checked);
  2 promoted first — moderate-cap re-rule trigger → CLAUDE.md Architecture Invariants;
  gap-renderer add-here obligation → `check_gap_status_surfaces.py` docstring.
- **ISSUES compress-on-close drain** (commit `ed45ada5`): 100/139 backlog entries compressed
  (13,142→10,563 lines), still-operative rulings kept verbatim per footer exception, statuses
  byte-identical before/after, router regenerated, all checks green. 40 skipped with reasons
  (declared residue in CLAUDE.md Memory Consolidation Review).

## 2026-08-09 — [landed] OQ-151 resolved: typed empty-chair detector + dual-gauge crosstab; role-gauge H¹ declined; 0.245 label corrected (was power-keyed) — OQ-275 minted

**Files:** prolog/stakeholder_seats.pl, prolog/reading_registry.pl, prolog/commentary_census.pl, prolog/tests/test_empty_chair.pl, python/audits/oq151_dual_gauge_crosstab.py, docs/design/design_discipline.md, ISSUES.md
**Tier:** landed

Three-commit arc (`e07fba7b` detector / `3607086f`+`9f7f6f85` audit / this close).
- **Detector:** `stakeholder_seats:empty_chair_state/2` — TOTAL 8-token typed refinement of the
  mcc candidate set (dissent-wins multi-chair semantics; `excluded_untyped` fires iff NO chair
  derives real, closing OQ-151's 4/5 false-positive trap at source). Census-only serialization
  (`empty_chair` source; `pipeline_output.json` untouched — per_constraint byte-identical
  clean-vs-edited pair over fp-frozen corpus). +5 registry entries; suite 14/14 + totality 10/10
  on all five legs; kernel_v1 overlay = 100% no_excluded_seat with the two corpus-facing vacuity
  guards firing (negative-domain control live). **Argument-asymmetry tripwire:** in
  `empty_chair_dissent*(T, DissentTypes, AllTypedExNames)` the third arg includes CONCURRING
  chairs — stated at clause + registry; do not read it as "the chairs that dissented".
- **Audit** (`audits/2026-08-09_oq151_dual_gauge/`, prereg md5-logged pre-run, five manifests at
  shared commit `3607086f`): 613 mcc candidates → 9 typed-dissent (~1.5%) / 219 excluded_untyped
  (~36%, the old false-positive class) / 385 concurs; Σ identity exact + expected-zero cells zero
  on every leg; crosstab off-diagonals realized, 47/47 sampled per-item re-derivations clean.
  `radiative_levitation_stratification` (OQ-136's text-ruled FP) now machine-reads
  `excluded_concurs(scaffold)`. **Citation ceiling: n=9 dissent supports existence + the OQ-199
  stratum only — no rate/distribution/comparative claims; the 385 concurs is equally readable as
  type-derivation coarseness washing out sub-type-resolution disagreement (undistinguished).**
- **Declined:** the full role-gauge H¹ (operator ruling; config-forced geometry / coarsening of
  `h1_stakeholder` / OQ-56 tripwire) — decline does NOT cite 0.245.
- **Correction-key ripple:** the 0.245 twin-agreement in design_discipline §0.1 + OQ-56/OQ-150
  was measured on the POWER-keyed vector (`_k_seat_role_vector` keys on `SEATS`); corrected at
  all three sites; OQ-275 owns the role-keyed re-measurement with the operator's pre-registered
  disqualifier (near-unity = config-forcing artifact, not draw-robustness).
- Untracked prototypes `probe_mc_cases.pl`/`probe_seat_sweep.pl` retired into the audit dir with
  defect headers.

---

## 2026-08-09 — [landed] World-bible back-flow review: OQ-270..274 + GAP-32 minted; negative results recorded (which bible claims are already in v8/spec)

**Files:** agent/narrative_transform/THE-GRAIN-world-bible.md, ISSUES.md, docs/design/design_gaps.md
**Tier:** landed

Reviewed `agent/narrative_transform/THE-GRAIN-world-bible.md` (1,941 lines, now
committed as tracked provenance, `33a2a156`) against `deferential_realism_paper_v8.md`
and CS spec v6 for reverse flow (bible → theory). Four extension candidates + one
taxonomy note minted as OQ-270..274; the cost-redistribution-at-constant-terminal
absence declared as GAP-32 (orthogonal to the terminal axis — NOT a seventh attractor
row). All hypothesis-grade: the bible is a downstream artifact of the framework, so
every entry carries the confirmation-loop caveat (bible Appendix A §1b).

**Negative results (checked, already present — do not re-derive or re-mint):** the
bible's "no synchronic signature" ≈ v8 §9.4 (lines 1141–1152); "measurement and
commitment are the same act" ≈ v8 §5.9 adaptive-preference argument (~726–741); law
of acknowledgment ≈ spec v6 ~line 141; drift-rate vs acknowledgment-capacity ≈ spec
~378–381; cast-size disagreement quantization = the OQ-195 H(n) partition law (v8
~247); the drift/ending-generator tables = spec §6 attractor table. The bible's
Module 5 §3 architectures and Module 11 §2 seat/gauge are direct applications.

## 2026-08-09 — [landed] OQ-262 resolved: severance/intrinsicness audit executed (R2-signed prereg, Arm B); two raw-match edge consumers routed — cs_corpus_analysis had read 40/40 live conflicts as "no typed edge"; OQ-268/OQ-269 minted

**Files:** prolog/cs_pattern_detection.pl, prolog/cs_corpus_analysis.pl, prolog/reading_registry.pl, prolog/drl_composition.pl
**Tier:** landed

Full record: `audits/2026-08-09_oq262_coexists_severance/WRITEUP.md` (commits
`7de8e5f9`→`c26a5b69`). Judged tier: 13 fiat coexists pairs annotated under a frozen
grammar (`edge_audit.json`; 6 severed / 5 intrinsic / 2 genuine) at the
pre-registered DOWNGRADED altitude "reading of the authored text" (RULED minority);
blind CP gate passed 4/4; expected-genuine control FAILED → `genuine` class
uncalibrated. **Behavior changes (output-changing, console/detection surfaces only —
pipeline_output.json byte-identical, witnessed):**
- `cs_pattern_detection:cs_displaced_beneficiary/1` now routes its forecloses target
  through `cs_kernel_registry:cs_edge_target_member/4` (`cs_resolve_edge_target/3`,
  raw fallback for non-kernel sources). Was dark on the live corpus for its whole
  life; now fires (first: `textualist_severability_reading`). reading_registry note
  updated from "dark on the live corpus".
- `cs_corpus_analysis` trifurcation conflict split routes through the resolver
  (`cs_conflict_pair_edge/6`): live corpus closure/plurality/neither moved
  0/0/40 → 11/28/9. Any prior citation of that console section's closure/plurality
  numbers is regime-bound to the raw-match era (they measured the naming skew, not
  the corpus).
- NOT routed: `drl_composition.pl:122` `detect_necessity_inheritance/2` → OQ-268
  (wrong-key Source binding + raw target; can essentially never fire). Census
  residue (48 live-leg orphan-source edges, undiagnosed) → OQ-269, which also
  records the pre-existing standalone-chain crash in `run_cs_corpus_analysis`
  (`metric_drift_events` unloaded outside `[stack]`).
Prereg discipline notes for future audits: a pinned mechanical control failed
AS WRITTEN by over-scoping its quantifier beyond its recon basis (scoped witness
pasted separately, prereg not amended — gate-spec class); the live corpus moved
mid-phase twice (235→240; every witness pair md5-bracketed, fiat substrate
blob-identical to HEAD across the judged tier).

## 2026-08-08 — [landed] c-orchestrator --close-gaps: gap-closing mode over a frozen manifest; frozen-manifest retry filter widened to json-AND-pl

**Files:** agent/c-orchestrator.py, agent/generate_kernel_corpus.py
**Tier:** landed

New `--close-gaps` flag (requires `--manifest-file`): regenerates ONLY the stories in
the frozen manifest's seed set whose `json/<cid>.json` OR `prolog/testsets/<cid>.pl` is
missing (e.g. a generation failure from a prior run), then continues into the normal
corpus update/reports/commit; skips research (its context only feeds the bypassed
decompose step) and exits early when nothing is missing. Composes with `--dry-run`:
lists the gaps with zero API spend. Seed derivation and gap predicate are factored into
one canonical trio (`_seed_set` / `_is_story_gap` / `story_gaps`,
generate_kernel_corpus.py) shared by the pre-flight probe and the frozen-manifest retry
filter in `generate_from_manifests` — and that filter was WIDENED: it previously skipped
any seed whose `.json` existed, silently never retrying a json-without-`.pl` story (the
stamp pass's PRODUCER GAP shape); both artifacts must now exist to skip. Witnessed
this session: guard control (`--close-gaps` alone → parser error); positive control
(c2_monetary_architecture manifest → exactly the 1 known failed reading,
`issuance_as_endogenous_credit_multiplication`); negative controls (the 3 landed fiat
readings not listed; "No gaps to close" early exit on 2 fully-landed manifests).
**Caveat:** a gap census over `agent/decompose_manifests/flat/` is NOT a to-do list —
several 08-03/08-05/08-06 manifests are decompose-only drafts whose stories were never
generated at all, and reading names churn across redraws (OQ-264), so point
`--close-gaps` only at the manifest of the run being completed, never at a superseded
draft of an already-landed family (it would mint new-draw siblings into that family).

## 2026-08-08 — [landed] OQ-261 resolved: forced-gluing experiment executed (R2-signed prereg) — performance presheaf OBSTRUCTS; discard minimum is exactly one bloc; 253/468 registry record stale (live 164)

**Files:** audits/2026-08-07_oq261_forced_gluing/, prolog/cs_kernel_registry.pl, prolog/stakeholder_seats.pl, ISSUES.md
**Tier:** landed

OQ-261 closed via the R2-signed pre-registration (`audits/2026-08-07_oq261_forced_gluing/`,
compute-only). Headline: **H_perf ("the ballot's performance presheaf always admits a
global section") contradicted wherever decidable** — the fiat family's pooled
performance-seat vector obstructs under both above-floor partition variants (H¹=45/55;
restrictive NULL by sparsity); seat types are story-derived (no cross-story seat
identity), so the topic's two-bloc structure penetrates the performance frame. Positive
product: the **discard minimum for a topic-presheaf total verdict is exactly one bloc**
(3 readings; both blocs achieve it). Base rate: 15/16 `real_closure` families obstruct on
the mechanical agent/excluded proxy (`fetterley_transfer_kernel` glues — probe can read
glue). No engine promotion (placement rule: "substitute-presheaf verdict channel" not
named an engine object). **Correction-key items:** (a) the registry's capital-punishment
divergence record 253/468 did NOT reproduce — live count 164, obstructed 97/156; 253 is
engine-state-stale (auto-memory annotated); (b) standing obligation on OQ-266: re-run
`positive_control_probe.pl` after the fixture-rot fix (the control's criterion 2 compares
against `cs_kernel_divergence/4` while its test is red — passed non-independently, R2
rider 1). Corpus md5-frozen through C3, witnessed in-dir.

**Post-hoc follow-ups (same day, operator review — labeled POST-HOC in the audit dir):**
(1) **Cell-1 demotion:** symmetric read shows every pooled sub-vector of the fiat family
(performance / topic-community / all-agent) carries H¹ = (#rope)·(#scaffold) exactly
(densities 0.4945/0.60/0.4952) — the obstruction is bloc structure penetrating ANY
pooled read via story-derived seat typing; Cell 1 carries no independent information
about performance seats specifically. The scoped H_perf verdict stands; its weight
against the performance-presheaf CONCEPT is minimal. (2) **253→164 diagnosed:**
engine-regime drift (fixture files byte-stable since the record era; divergence
predicate edge-free so Item B ruled out by mechanism; live type sets add `snare`, drop
`unknown`) — the OQ-266 re-run is a fixture question, not a corpus question. (3)
**OQ-267 minted** (sharpened successor: ballot totality = institutional forcing over a
SECOND obstructed presheaf; needs an identity-controlled substrate with cross-story seat
identity). (4) **Candidate OQ-264 standard #7** recorded (operator-proposed, adoption
pending): no pooled-across-story H¹ claim without an identity-controlled comparator.
(5) The remaining untracked `testsets/*_contradictions.pl` files committed post-C3 —
17 files at `543e2f9a` (the earlier "~20" was an estimate; with the fiat one at
`f724379d`, 0 contradictions files remain untracked, witnessed by `git status`).
Git-state only; disk content unchanged, so corpus md5 fingerprints unaffected. Six
untracked story testsets (`collective_refusal…`, `voice_without_leverage`, …) + their
`json/` twins remain untracked — separate item, not ruled on.

## 2026-08-07 — [landed] Edge-naming reconciliation: canonical cs_reading_relation target form is BARE cids (operator ruling); three-form resolver lands; 22 kernels leave `untyped`; OQ-260 resolved

**Files:** prolog/cs_kernel_registry.pl, prolog/json_report.pl, agent/generate_kernel_corpus.py, python/audits/reading_reference_linter.py, prolog/testsets/fiat_efficacy_kernel_contradictions.pl, agent/c-orchestrator.py
**Tier:** landed

**Still-operative ruling (operator, 2026-08-07): the canonical `cs_reading_relation`
target form is BARE cids.** The registry resolves both legacy authored forms —
bare-against-prefixed-corpus (old quarantine-view rescue) and prefixed-against-bare-corpus
(the 2026-08 generator skew) — via the new exported
`cs_kernel_registry:cs_edge_target_member/4` (exact | bare→prefixed | prefixed→bare strip;
exact atom equation modulo the kernel's own `__` prefix, never similarity).
`cs_kernel_obstruction/4` pair-matching and `cs_reading_relation_unresolved/4` (now the
resolver's exact complement) route through it, as do `json_report.pl`'s two `cs_axiom_*`
join blocks (commit `4f646665`). Generator emits bare (`snap_sibling_id`), validator and
`reading_reference_linter.py` accept all three forms (`8d509bdc`).

**Blast radius was corpus-wide, not fiat-local.** The plan hypothesized only
fiat + visual would move; the B0 witness falsified that: 22 of 67 live-testsets kernels
left `untyped` (**13 → `real_closure`, 9 → `licensed_plurality`** — CORRECTION
2026-08-07, same day: this entry and the session report first said "11/11", conflating
the `cs_axiom_real_closure=11` join count with the kernel counts; 13/9 is the witnessed
split, full `real_closure` set now 16 incl. 3 pre-existing), engine table matching an
independent raw-text pre-derivation 67/67. `cs_axiom_real_closure` 0→11,
`cs_axiom_licensed_plurality` 0→28 (both joins were dead for their whole life — every
authored target was prefixed while cids are bare). Census: n_dangling 358→139,
n_noncanonical 0→219, defensible backlog 5→17 rows. Twins/kimi/sonnet byte-identical;
kernel_v1 moved ONE cell (`constitutional_interpretive_authority` Plur 2→3, status
unchanged) — a bare-target/prefixed-member edge whose rescue previously existed only in
the quarantine view, now reaching pair-matching. Same-session frozen-corpus diff pair
(md5-identical legs both halves; run exit 0 + mtime advanced); the pipeline diff contained
ONLY the pre-derived paths.

**OQ-260 resolved** (`00f8cf32`): `_step_commit` repo-anchors relative
`_last_manifest_path` before `relative_to(REPO_ROOT)`; 3-case harness (HEAD positive
control reproduced the false `not_staged: manifest outside repo`; post-fix relative +
absolute in-repo stage; outside-repo still refuses).

**Residue (declared):** `cs_pattern_detection.pl:355` and `drl_composition.pl:122` still
raw-match edge targets (out of scope this session — noted on OQ-58/OQ-262);
~20 sibling `testsets/*_contradictions.pl` files remain untracked (only the fiat one was
committed, `f724379d`); pre-existing suite rot unchanged byte-identical on baseline code
(test_cs_kernel_registry 24/25, test_contradiction_signatures 12/17, test_cs_axiom_engine
11/14 — the axiom suite's header load chain points at fixtures removed in the 2026-06-05
reset; OQ-266 class).

## 2026-08-06 — [landed] CS spec v6 full reissue adopted (supersedes v4+v5+v5.1+v5.2); spec-enum tripwire in the gate; OQ-265 (identity endpoints) + OQ-266 (pattern-suite rot) minted

**Files:** docs/commitment_systems/commitment_systems_sketch_v6.md, python/spec_enum_check.py, scripts/gate.sh, prolog/cs_pattern_detection.pl, prolog/cs_drift_engine.pl, prolog/cs_axiom_engine.pl, prolog/tests/test_cs_pattern_detection.pl, python/enhanced_report.py, ISSUES.md
**Tier:** landed

**v6 adoption** (commits `81d561bc` checker, `01fad750` doc, `932f577a` OQ-265, `1f360e31`
touch-ups, `50d6ecc3` OQ-266). `commitment_systems_sketch_v6.md` is the current CS spec —
self-contained, ends the 4-deep delta chain; v4/v5/v5.1/v5.2 carry superseded-by headers and
stay as history. Absorbs the six post-v5.2 code-only subsystems (drift terminals, axiom layer,
kernel obstruction, trifurcation router, cross-axis mismatch, `scaffold_suppression_escalating`)
plus the Type-B adjudication ruling; states the versioning rule (delta = one marked revision to
one section; reissue at >1 section or chain depth ≥3); carries a correction-lineage table.
Axiom-enum mismatch resolved as documentation: authored vocabulary is {holdable, overridden},
`foreclosed` computed-only (v6 §7 + comment touch-ups, no code change).

**Spec-enum tripwire** (`python/spec_enum_check.py`, wired into `scripts/gate.sh` as "spec
enums"): 8 enums (terminals, directions, magnitudes, patterns, verdicts, obstruction statuses,
trifurcation types, normalized attractor table) in sentinel blocks, diffed against code pins;
fails loud on a deleted sentinel; positive controls (add/remove/deleted-sentinel) ride every
run. Witnessed RED all three ways on mutated copies, GREEN on the real doc. **If you edit any
of these enumerations in code, the gate goes red until the v6 block is updated — by design.**

**New finding (identity precondition):** the six drift terminals presuppose continuous
single-continuant identity in addition to OQ-227's surviving referent — schism and
absorption/syncretism are unhandled endpoint regimes. Declared absence, OQ-265 (per-candidate
detectability verdicts required at resolution; `terminal_set_pinned` stays green).

**Found defect:** `tests/test_cs_pattern_detection.pl` is silently red 13/37 AND its runner
exits 0 on failure — pre-existing fixture rot (corpus reset + UID re-key), byte-identical
failure lists pre/post the v6 comment edits. OQ-266. Do not cite that suite as a witness.

Stale-gap corrections landed in v6 vs the old satellite surveys: `cs_displaced_beneficiary/1`
is `forecloses`-edge-keyed (v5.2 said `affects_constraint/2`); `predicted_terminal_state/3`
is at `transition_paths.pl:231–256` with vocabulary {piton, snare, tangled_rope, stable}.

**v8 follow-through (2026-08-07, operator-directed):** three CS-facing currency fixes in
`deferential_realism_paper_v8.md` (`785d090e`: §4.2 self-exemplification extended to the v6
event; §4.3 foreclosure gains the non-minor condition the code always had; module inventory
4→6), then the **blocked-B gate absorbed into v8** on operator instruction (new §4.2 paragraph:
fix-individuation ruling, revisable-B/blocked-B split, gate-not-type, Stage 0; forward pointer
in §2; Stage-0 rider on §5.9's Type-B leg; three-valued gate kept PROPOSED/OQ-235). v8 cites
v6 §3 (Position in the trifurcation) as the ruling's record.

## 2026-08-06 — [landed] OQ-259 item-2 Part C executed: T Framework GRADUATED second meta-layer file (P1 3/3 + blinded presence 3/3, planted control valid); B1 closed without spend (operator ruling)

**Files:** audits/2026-08-06_oq259_item2_tframework/, ISSUES.md, docs/drafts/shanahan_kritik.md
**Tier:** landed

Operator rulings resolved both checkpoints: B1 NOT run (B0's STRICT 0/2 at the origin
made every fresh-file outcome uninformative; item-3 verification arm closed on B0's
evidence, quote takes the pre-authorized one-off framing; essay framing FINAL at
`docs/drafts/shanahan_kritik.md`); Part C ran under P1-only promotion (P2 judged-step
rejected). Three serialized `--dry-run --skip-search` draws of the re-minted baseline
(`a365da8a…`, ~220,720 tok/run): P1 PASS 3/3, kernel ids churned 3/3, reading counts
3/6/5; presence clause adjudicated blind (packet `afbaaa55…`, withheld mapping
`e6a26bed…`, calls `7581cf98` before reveal) with planted AT Fiat control called
DIFFERENT 3/3 → three draws one SAME subject+stance group → **graduated per the
frozen grammar** (prereg `4f862bee`). Scope: presence result, one file,
emphasis-blind regime — not detection, not ingestion (a T Framework story ingest is a
separate operator spend, permitted not compelled) — and P1 base-rate-bounded
post-review: AT Fiat's existing OQ-264 draws also measure P1 3/3 (zero-spend
comparator), so P1 3/3 is meta-layer-general, not T-Framework-specific; the
graduation rests on the conjunction with the blinded presence call. OQ-259 CLOSED
resolved same day (item 3 closed-as-superseded — essay carries the one-off framing,
not the original verified-flag intent); entry compressed per the footer rule with
operative blocks kept. Symmetric confirmatory-draw
staging (run 2 iff run 1 mints; run 3 iff both) adopted over declared-n=1 — strictly
dominant given the 3/3 grammar. Full record:
`audits/2026-08-06_oq259_item2_tframework/` (WRITEUP.md, C_RESULTS.md).

---

## 2026-08-06 — [tripwire] Unblinded single-draw reads of generated-manifest features run GENEROUS: twice now the instrument was less stable than the unblinded read suggested — blind the call before citing stability

**Files:** audits/2026-08-06_oq259_item3_genreflag/B0_TALLY.md, audits/2026-08-06_oq264_kredraw_variance/WRITEUP.md
**Tier:** tripwire

General finding, promoted from a B0 detail by operator instruction (2026-08-06). Two
independent instances, same direction: (1) OQ-264 — the 2026-08-05 "no Arm-0
measurement needed" expectation inverted when measured; (2) OQ-259 B0 — the unblinded
session read hypothesized strict reproduction 1/2–2/2; blinded adjudication measured
0/2 (the specific miss: an unblinded read credited a structure-import clause as a
fidelity clause). The failure mode is not random error — unblinded reads OVER-credit
stability/reproduction. Standing rule: before citing any stability, reproduction, or
presence claim about generated-manifest features, run the call blind (packet with ids
stripped + commit-order + withheld-mapping md5; template
`audits/2026-08-06_oq259_item3_genreflag/`). An unblinded read is a hypothesis to file
in RECON, never a citable rate. Also the B1-closure precedent: an origin-stability
null can retire a planned fresh-specimen spend arm whole — check the free
origin-stability measurement BEFORE pricing a fresh-file arm.

**The inverse direction (operator, 2026-08-06, post-Part-C review):** the tripwire
above covers stability CLAIMS; the same discipline applies to instruments — **any new
observable introduced into a verdict grammar needs a churn measurement BEFORE it can
carry weight**, not after it produces a result you want to rely on. Witnessed: P1
(`is_contested_kernel`) entered Part C's graduation grammar unmeasured — it is the
observable Cap K churned 1/2 on — and its base-rate bound had to be added in
post-verdict review; the bound's comparator (AT Fiat's existing k=3 draws, P1 3/3)
had been sitting on disk at zero cost the whole time. Corollary: before pricing a
comparator run for a base rate, check whether already-committed draws measure it for
free. Program-level tally of the pattern (4 instances, one direction): item-1
readings, B0's strict flag, Cap K's kernel mint, T Framework's kernel ids — every
observable measured for stability came back less stable than the single-draw read
suggested.

---

## 2026-08-06 — [correction-key] OQ-259 items 2–3 free arms: B0 blinded measurement (STRICT 0/2, TERRITORY 2/2, origin blind STRICT), P2 unbuildable as mechanical gate (two-sided calibration inverted), essay drafted — B1 and C await operator checkpoints

**Files:** ISSUES.md, audits/2026-08-06_oq259_item3_genreflag/, audits/2026-08-06_oq259_item2_tframework/P2_CALIBRATION.md, docs/drafts/shanahan_kritik.md
**Tier:** correction-key

Full records: the two audit-dir WRITEUPs. Pre-registered blinded adjudication
(prereg+packet `db708cc7` → calls `12ee7f55` → mapping reveal `536263bc`, md5s pinned
forward) of all 14 candidate carriers across the origin Biopower manifest + its two
Arm-0 redraws. **How results may be cited:** (1) `omega_debate_genre_distortion` is an
OBSERVATION whose strict (i)+(ii) form is redraw-brittle (0/2) and whose genre-territory
form is redraw-stable (2/2, name churned both times) — n=2 figures are two draws, never
a rate; the pre-registered disclosure (the origin file itself did not clear the strict
bar at n=2) attaches to any future rows-1–3 reading. (2) The unblinded hypothesis
(strict 1/2–2/2) was MORE generous than the blind calls — cite the blind result only.
(3) Omega-class floor: name/territory/strict are DIFFERENT observable classes with
different churn floors on the same file; stability constants do not transfer across
classes (cross-ref OQ-264). (4) P2: ceiling never exceeded floor across three
token-strictness variants (v1/v2 inverted, v3 collapsed) — cite as "unbuildable as a
mechanical gate, judged-level quantity only"; no threshold exists; Part C must not
start with a fake gate (operator picks the P2 form at the C checkpoint). (5)
`capitalism_kritik_ndi2026_20260805_145128.manifest.json` has an EMPTY
`commitment_system_recognition` — a P1-fail draw, not a P2 zero. Part D draft at
`docs/drafts/shanahan_kritik.md` (location = operator's call; B1 placeholder bracket
must be resolved before publication).

---

## 2026-08-06 — [landed] OQ-259 item-2 note (a): T Framework baseline re-minted from pinned recipe (non-reproduction confirmed, markup-shape diff, no cause verdict)

**Files:** agent/analysis/originals/k_files/T Framework - Michigan 2026 BCFP.md, audits/2026-08-06_oq259_item2_tframework/
**Tier:** landed

The committed baseline (597,374 B, md5 `51caeb369d147849d07b45f1ba0926b6`, sole commit
`1bd57a84`, worktree clean) is NOT reproduced by the pinned recipe `pandoc -f docx -t
gfm --wrap=none` (pandoc 2.9.2.1): repro is 672,832 B, md5
`a365da8aa11e5039807275bcc662f956`. Diff shape is purely markup representation
(underline `<span>` runs present in repro absent in baseline — stripping them leaves an
8-line diff; residual is `<sup>NN</sup>` vs Unicode superscripts), line counts
identical; cause classification deliberately withheld per plan. Fresh baseline minted
from the pinned recipe (live `.md` now md5 `a365da8a…`); superseded file retained at
`audits/2026-08-06_oq259_item2_tframework/superseded_baseline_51caeb36.md`. **The new
md5 `a365da8a…` is the pinned input for any OQ-259 item-2 Part C prereg.** Full record:
`audits/2026-08-06_oq259_item2_tframework/WRITEUP.md`.

---

## 2026-08-06 — [correction-key] OQ-264 RESOLVED (standard-only): per-reading redraw stability is file-structure-dependent (0.33–1.00); pooled share does NOT repair churn (denominator artifact); k=3-unanimous presence standard minted

**Files:** ISSUES.md, CLAUDE.md, audits/2026-08-06_oq264_kredraw_variance/, audits/2026-08-03_kritik_ingest/WRITEUP.md, python/audits/oq264_idiom_share.py
**Tier:** correction-key

Full record `audits/2026-08-06_oq264_kredraw_variance/WRITEUP.md`. Pre-registered
(PROPOSAL `fd58d3a1` before scoring; calls `0a28d7ca` before mapping `e4c293d4`) blinded
pooled scoring of the six free kritik manifests, then an operator-review correction pass
(`241ec42d`/`13999d9c`), then the sole spend: AT Fiat k=3 same-input redraws
(`ac2650ae`/`b418b632`, ≈101K tok, corpus untouched every run). **How results may be
cited:** (1) the share gate's PASS(sens1) is on record but SUPERSEDED in meaning — its
entire 0.25 range fell between numerator-identical draws (TAG=3/6 vs 3/4), i.e.
denominator churn at fixed judgment, with the perverse direction fewer-readings→higher-
share; cite the specification finding, not the pass. (2) Reproduce-rates now span
2/6–3/6 (Cap 340K arsenal), 4/6–5/6 (Biopower 103K), **6/6 ×3 (AT Fiat 34K single-voice
— the 2026-08-05 "no Arm-0 measurement" rider closed, expectation inverted)**: no global
churn floor exists; never cite one number as "the" floor. (3) Presence claims need k=3
unanimous same-input redraws (1–2/3 = observation); names are never identity (kernel ids
churned at reproduce-rate 1.0). (4) 0/n control agreement is a binomial bound (0/6 →
95% UB 0.393), never "zero variance." (5) Plan-rule recalibration precedent: the rev-1
sensitivity modifier was rejected in Phase A by its own quantization simulation
(CALIBRATION.txt witnesses both rules) — gate rules must pass the stable-null
simulation before numbers commit. Propagated: OQ-259 items 2–3 unblocked (item 3 k=3
concrete; AT Fiat cannot serve it), Amendment 6 on the 2026-08-03 WRITEUP (plan said
"Amendment 5"; one already existed), CLAUDE.md Generation-is-stochastic block updated.

---

## 2026-08-06 — [landed] Monthly consolidation pass (2026-08): KNOWN_STATE roll-off 129 entries compressed; residues declared

**Files:** KNOWN_STATE.md, CLAUDE.md, ISSUES.md
**Tier:** landed

Roll-off: 129 landed/history entries in the 2026-06-05..2026-07-06 due window compressed
in place (7,906→5,310 lines, `4e0efd5b`; headers/Files/Tier verbatim, pointers kept;
checker 261/0 green; 51 tripwire/correction-key entries in window deliberately left —
they need the promotion test, next pass). ISSUES compress-on-close CHECK ran: ~130
closed entries >14 lines (worst OQ-138/153/62/219); bulk compression deferred with the
still-operative-ruling exemption noted. Memory dir verified consistent (103 files, 0
orphans, no merge candidates). CLAUDE.md: dates advanced (next pass ≥2026-09-06),
residues declared in the review section, OQ-264 churn-floor tripwire promoted into the
"Generation is stochastic" block (promotion-test hit from the 2026-08-05 finding).

---

## 2026-08-05 — [correction-key] OQ-259 item 1: emphasis discriminator HALTED by its pre-registered Arm-0 gate — per-reading presence is NOT redraw-stable within a file; "replicate-stable (f)" is cross-file only

**Files:** python/audits/emphasis_extract.py, audits/2026-08-05_oq259_emphasis_discriminator/, agent/analysis/originals/k_files/, audits/2026-08-03_kritik_ingest/SCORING.md
**Tier:** correction-key

Full apparatus built and witnessed before the gate: pre-registration addendum
`c4785da7` (per-file thresholds; quantified HALT reproduce-rate < 2/3; Arm-2
selection rule + seed 259; commit-order blinding protocol) committed BEFORE any run;
`python/audits/emphasis_extract.py` (raw-string docx splice — ET re-serialization
breaks pandoc image extraction; byte-copy rezip; scramble mode) + three
emphasis-aware conversions committed `b8a44661`, strip-restore BYTE-EXACT vs the
`1bd57a84` baselines, marker pairs exactly as predicted (AT Fiat 208/175, Biopower
829/1014, Cap 2535/2589). Arm 0 (same-input re-runs ×2 per payload file, inputs
md5-pinned, `70c458f9`): **Cap K reproduce-rate 3/6 then 2/6 — HALT fired (< 2/3 in
both; the second Cap re-run minted NO contested kernel at all); Biopower passed 4/6 +
5/6. Read-through churn control: 3 of the 4 discriminator-target readings churned at
byte-identical input** (world_system, growth_process, coalition_governmentality;
only the insurance deferred axis stable 2/2) → Cap K P1 effective n=0, Biopower n=1.
Arm 1/Arm 2 NOT run; ~884K input tok spent (4 of ≤8 calls); corpus untouched every
run (listing-diff witness). **Citation consequences:** (1) the 2026-08-03 "(f)
partial recovery, replicate-stable" verdict is CROSS-FILE stability — do NOT cite it
as within-file redraw stability of any individual reading; (2) SCORING.md's one
CONTESTABLE hit (`reformist_iatrogenic`) churned 0/2 — treat that baseline row as
draw-fragile; (3) any future per-reading-presence design on ~100K–340K-token inputs
needs a churn arm sized to the effect or a churn-robust observable (idiom SHARE over
pooled redraws). **Ruling landed same day (option 1, extended): item 1 CLOSED — the
Arm-0 churn floor IS the finding; options 2/3 ruled dead (n=1 anecdote / instrument
failure).** Sampling check witnessed with the ruling: Sonnet-5 decompose runs with
temperature OMITTED by design (`llm_call.py:112`; 400 on non-default) and the API has
no seed — churn is the production regime's own magnitude, not an unpinned knob.
Program-wide propagation minted as **OQ-264** (single-draw per-reading findings carry
unquantified error bars; k-redraw variance-floor standard; fewer-files × more-draws;
Cap K out of scope for per-reading measurement); OQ-259 items 2–3 blocked_on OQ-264;
item 3's genre-flag standard restated to appearance-across-k-redraws. Extractor +
conversions stay ready for an OQ-264-compliant redesign.

---

## 2026-08-04 — [correction-key] OQ-258 discriminator: referent ambiguity did NOT own the channel-legibility finding; reader-position survives; ε referent now fixed in the contract

**Files:** prompts/constraint_story_generation_prompt_json.md, prompts/constraint_story_generation_prompt.md, schemas/constraint_story_schema.json, audits/2026-08-03_oq258_referent_discriminator/, audits/2026-07-27_cross_author_epsilon_probe/RESULTS_legibility_coding.md
**Tier:** correction-key

Null+fix discriminator over the 18 top-spread tacit/none_apparent items × 4 legs
(pre-registered `74e74e35` BEFORE spend; contract fix `685ed7cf`). Old-contract redraw (Arm B)
mean spread 0.4633 vs referent-fixed (Arm A) 0.5167, Wilcoxon p=0.328 — pinned row 3:
**A ≈ B with B elevated. The 2026-07-27 channel-legibility finding may now be cited WITHOUT
the OQ-258 degeneracy caveat, and the channel-conditional reliability caveat (2026-07-27
entry) HARDENS from "artifact-or-real, undecided" to "survives its first real test."**
The phenomenon replicates on redraw (B retains ~77% of baseline elevation, no
regression-to-mean collapse). Named witness failed informatively: haiku re-authored ε=0.00
on `animal_status__abolitionist_reading` under the fixed referent, rationale explicitly
refusing the quantitative question; kimi FLIPPED 0.82→0.02 under the fix, scoring the
reading-as-constraint itself ("the constraint itself blocks rather than extracts") —
the live ambiguity is CONSTRAINT IDENTITY in kernel-reading stories (standing arrangement
vs reading-as-constraint), upstream of the (a)/(b) referent choice OQ-258 posited; tracked
as OQ-263 (three-valued declared-referent-field fallback). The referent-(b) contract fix
STANDS regardless (ruling fixes the rebuild's contract): kernel-reading ε names the standing
arrangement under contest, assessed by the reading's own lights (OQ-26 untouched). Do not
expect prompt-side referent language ALONE to force cross-author ε agreement — witnessed
insufficient. Kimi Moonshot batch stalled 0/18 ~8h → declared sync fallback used (identical
sampling params). OQ-258 resolved; evidence archived under the audit dir (`generated/`),
baseline legs md5-unchanged throughout.

---

## 2026-08-03 — [landed] Kritik ingest probe: arsenal-format K-files score (f) partial recovery replicate-stable; AT Fiat K graduated (7 stories, n=225)

**Files:** audits/2026-08-03_kritik_ingest/, agent/analysis/originals/k_files/, agent/c-orchestrator.py, agent/decompose_manifests/flat/, prolog/testsets/
**Tier:** landed

Pre-registered probe (`PROPOSAL.md` committed `1bd57a84` before any run) on whether SCOPE
recovers coherent structure from debate-camp card files. Three dry-runs + fresh emotives
control, ALL `--skip-search` (Cap K NW measured 339,501 tok — largest witnessed SCOPE
ingest — vs the ~187.9k research cap; uniform flags required). Verdict per the
pre-registered rule: **both arsenal replicates (f) partial recovery, replicate-stable** —
coherent, precision/recall pass (b)'s bars, idiom MIXED (SCOPE scaffolds subjects/stances
on the editorial block layer but populates readings from the card literature, minting
definitional-contest kernels plus ~2 pure read-through readings per file no block names).
Emphasis ruling (A): all claims are properties of emphasis-blind ingestion (pandoc drops
highlight/font-size = the read/unread layer; bold survives), never of the format —
extractor re-run is the named discriminator (OQ-259). **[CORRECTED same-day, operator
review]** AT Fiat K produced a 6-reading grounds-contest kernel; the first draft of this
entry read that as "single-voice did NOT flat-route" against the KNOWN_STATE 2026-06-08
under-routing tripwire — wrong premise: the file is single-STANCE but multi-voice (six
attributed authors), so the single-voice tripwire was never applicable and no-flat-route
is expected behavior (no tripwire-regression reading is licensed; WRITEUP.md Correction 2).
The confound also splits directionally (WRITEUP Correction 1): the scaffold half
(precision despite 10× tag-layer dilution) is a-fortiori STRONGER than stated; the
read-through half is confound-predicted and is what OQ-259 can kill. Principal finding
(WRITEUP Amendment 4, INFERRED from the scoring tables, symmetric across both
replicates): **SCOPE's reading granularity sits at the theoretical position** —
position-staking sections surface as readings regardless of side; Link/Impact machinery
absorbs into the parent reading's `expected_structural_delta` (present one level down,
not lost). Every strict recall miss was this altitude conflation in the predicted
denominator, so read the (f) label with the WRITEUP's verdict qualifier (nearer (b) than
the bare label; classification unchanged — the pre-registered rule stands). Granularity
prediction registered on OQ-259 (machinery stays absorbed under emphasis-aware
ingestion), independent of the tag-idiom prediction. Phase 3: AT Fiat K
full run on the frozen manifest →
7 stories (`69db90a1`), pipeline 49/49, n_constraints 217→225 (+1 untracked
`fiat_efficacy_kernel_contradictions.pl`, standing convention), ε-referent uniform over
the defended practice (weak OQ-258 evidence — wrong specimen class, logged on the OQ).
Side-finding OQ-260: `_step_commit` manifest staging mislabels relative in-repo
`--manifest-file` paths as "outside repo" and silently skips staging
(c-orchestrator.py:965). Scoring + witnesses: `SCORING.md`/`WRITEUP.md` in the audit dir.

## 2026-07-27 — [landed] Cross-author ε probe: 4-leg divergence sorts by channel legibility, not topic heat; twins never ε-harmonized; haiku exact-0.00 is authored

**Files:** audits/2026-07-27_cross_author_epsilon_probe/, prolog/testsets_haiku/, prolog/testsets_flash/, prolog/testsets_kimi/, prolog/testsets_sonnet/
**Tier:** landed

Commits `90de6e91` (probe) / `bcba5d4d`+`be76062c` (pre-registration, committed BEFORE coding).
Part 1: over the 957 readings shared by all four twin legs, ε was never harmonized at
reconciliation (haiku~flash 3.9% identical, mean |Δε| 0.105); author-level means differ
systematically (kimi 0.589 > haiku 0.565 > flash 0.508 > sonnet 0.490); only 1.0% of readings
carry identical ε 4-ways. Exact-0.00 ε is an authored value, not a null (haiku 14/960 with
in-file justification; sonnet 0). Part 2 (blind coding, 7 subagents, seed-pool substrate,
controls 7/8): top-vs-bottom spread deciles sort by channel legibility (Fisher p=0.023,
predicted direction), NOT topic heat (p=0.27, reversed); biggest post hoc asymmetry is
referent-weak items (`none_apparent` 8 vs 1). Instrument note: per-story ε on tacit or
referent-weak constraints is low-reliability across authors — treat cross-leg ε agreement as
channel-conditional. Establishes variance, NOT directional bias (needs a non-LLM reference
leg; operator ruled 2026-07-27 no human ε leg — reader-profile plan steps 2–3 dead unless
revived). MECHANISM is degenerate (reader-position vs ε-referent ambiguity in the generation
contract) — see **OQ-258** (witnessed contract defect + pre-specified discriminator); do not
cite the channel sort as reader-profile evidence until it discriminates. Full method,
caveats, per-item codes: the audit dir.

---

## 2026-07-25 — [landed] OQ-216 stage-2 contract guard redesigned: header-proxy → content-level; floating_city false-negative corrected

**Files:** agent/uke_narrative_orchestrator.py, python/tests/test_stage2_contract_extraction.py, ISSUES.md
**Tier:** landed

Commit `4878df78`; evidence `audits/2026-07-25_oq216_contract_extractor_redesign/`; full
correction block on ISSUES OQ-216. The 2026-07-13 guard checked header-string-at-position — a
proxy that failed both directions: blocked four drifted-but-complete Sonnet-5 stage-2 outputs
(prometheus ×2, quellcrist, ergodocity), and on `the_floating_city_xixi_1784000706` passed while
over-capturing to EOF — stages 9/10 consumed an 18,266-byte blob as "the contract" and the story
shipped (run-dir `invariant_contract_output.md` is the witness). Redesign
(`_extract_invariant_contract_checked`): canonical + drifted headings accepted; bound at next
same-or-higher heading; EOF-termination always fails (SECTION 0 mandated first); negative
(no SECTION 1/2/OMEGA LOG) + positive (invariant/falsifier/substrate/inhabitation,
token-censused over 13 good blocks; `break` deliberately excluded) content assertions.
Witnesses: 7/7 fixtures; rotation_seven old==new byte-identical; floating_city 18,049→1,693ch;
prometheus 0→2,159ch; fresh-draw set 3/4 pass (the fail is the no-block shape where fail-loud is
correct). Standing: drift is Sonnet-5-endemic on floor-primary sources (3/5 prometheus draws);
OQ-219 clause amplifies, does not cause (ergodocity predates it) — extractor+content-guard is
the load-bearing layer, prompt fixes are hygiene.

## 2026-07-25 — [landed] OQ-254 RESOLVED: Q-provenance wired (join key, self-stamp, tracked manifests, standing readout); headline corrected on close

**Files:** agent/c-orchestrator.py, agent/generate_kernel_corpus.py, python/generate_constraint_pl.py, schemas/constraint_story_schema.json, python/run_pipeline.py, python/q_provenance_readout.py, agent/decompose_manifests/, ISSUES.md
**Tier:** landed

Audit `audits/2026-07-25_oq254_q_provenance/`; commits `01d503aa`/`f1436bd4`/`2d7432a0`/
`7f29bfea`/`c200fcd2`. The OQ's headline was FALSE as written (marked on the entry, second
v8-prose inference corrected by code contact): the Q-choice was richly declared
(selection_reason 2596/2598 axes, deferral_reason 1022/1022, kernel verdict 486/515 over the
515-manifest census) but unreachable — gitignored, unstamped, unjoined (Pattern 6). Landed:
(1) `generation_run_id` = manifest filename stem, minted at decompose, threaded through all
THREE scope-manifest write paths (c-orch `_persist_manifest`, gkc batch decompose, legacy
`--scope`) into story provenance → `epsilon_provenance/5` arg 4 (schema-optional field —
never required; `'none'` = declared pre-wiring stratum, NEVER a defect and never backfilled);
(2) manifest `_provenance` self-stamp; (3) manifests now write to tracked
`agent/decompose_manifests/<run_tag|flat|decompose>/`, the 515 pre-existing ones archived at
`archive_pre_2026-07-25/` (archive-not-read-surface; readout token
`joined_archive_not_authoritative`, never `joined`/`unreachable`); (4) standing
`q_provenance_readout.py` + run_pipeline Phase 9d (planted two-sided controls every run;
behavior-preservation witnessed: per_constraint byte-equal over md5-frozen corpus). Close
records WIRED not JOINED: all 205 live stories `no_run_id_authored`; e2e join graduates at
the next operator topic run. `_step_commit` extension landed after operator review, amended:
manifest stages only on join-key match (filename stem == every committed story's
`generation_run_id`); all non-staged outcomes recorded in `StepResult.data`. Follow-up
minted: OQ-256 (§3 foreclosure as structure; waits on first exercise of OQ-255's
hand-enumerated branch).

---

## 2026-07-25 — [landed] Seat-theorem v2.5 ADOPTED (OQ-253 ruled): Q enters the formalism; interrogative type exemption struck at all three sites
**Files:** docs/seat-theorem-v1.md, docs/deferential_realism_paper_v8.md, docs/the-few-seats-worth-choosing-v2.md, ISSUES.md
**Tier:** landed

Operator ruling (option 1 + two riders), commit `fdc502ec`. seat-theorem v2.4→v2.5: §1(3)
signature V = 𝔙_Q (completed to 𝔙_{Q,Π} in §8, which now states the chain Q → Π → σ →
liveness); §7 seat is the pair (Q, Π); §6.2 type exemption replaced by graded seat-cost
(foreclosure-set, enumerable) with the guard requirement promoted to the v1.1 gate — carried
WITH its scope inline as a **standing probe** (rider 1: the gate encodes the claim the
discriminator tests; one run, engine tokens, n=199 — a future discriminator run that lands the
other way weakens the gate back toward admission). v8 §6.3 tracks the priced form. The essay's
site-3 sentence ("commits to nothing the world could refute" — its strongest form) was REWRITTEN
with an authored replacement paragraph + fourth-pass note (rider 2), not struck. Sweep witness:
`grep "commits to nothing"` over docs hits only the two strike-records quoting the dead claim.
Tripwire for future doc cites: **the interrogative exemption is dead law** — do not quote
"a question commits to nothing..." from memory of pre-v2.5 text; questions price low, not zero.
Ruling record: ISSUES OQ-253 (resolved, compressed).

---

## 2026-07-25 — [landed] OQ-255 seat-cost measure delivered; two corpus facts: `emerges_naturally`'s true-pole holds FIVE types, and `coordination_vitality` is authored-empty on every live leg
**Files:** audits/2026-07-25_oq255_seat_cost_measure/, prolog/drl_core.pl, docs/seat-theorem-v1.md, docs/deferential_realism_paper_v8.md, docs/the-few-seats-worth-choosing-v2.md, ISSUES.md
**Tier:** landed

OQ-253/254/255 minted and OQ-255 executed same day (commits `03e57ec3`, `2fa3eca2`, this one) —
the seat-theorem Q-upstream-of-Π revision proposal, its Q-provenance extension, and the seat-cost
measure (gate + grade over foreclosure-sets; kill condition did NOT fire — exhibited Q3/Q5 pair;
audit has the census, controls, and scope declarations). OQ-253 now rides purely on the operator
ruling (adopt/reject the three edits + the three-site exemption-sentence sweep:
seat-theorem-v1.md:138, v8:796, few-seats-v2:31 — the last is the STRONGEST form).

**Two corpus facts worth knowing independent of the philosophy** (manifest
2026-07-25T07:20:20Z, n=199, ee025a0; slots = constraint × context):

1. **`emerges_naturally=true` does NOT mean mountain.** The true-pole is inhabited by five
   types — mountain 6, rope 32 (the `drl_core.pl:423` bypass, inhabited), scaffold 18, snare 8
   (nlwb blocked by `agent_beneficiary` alone), unknown 8. A probe or report treating
   `emerges_naturally` as a mountain proxy misreads 66/72 of the flag's true-pole slots.
2. **`coordination_vitality/2` is authored by ZERO stories on all five live legs and
   kernel_v1** — only the legacy `original_json` archive authors it (grep control ladder in the
   audit). The dead-coordination piton path (`drl_core.pl:354-357`, clause 1 at `:381-388`) is a
   live gate over an authored-empty table; ALL 32 live piton slots ride the theater fallback
   (`:440-448`). Fails closed (dormant, not defective). **Same-day correction (audit §8): do NOT
   read this as "the engine has no genealogical/drift channel."** The live genealogy surfaces
   are `founding_problem_status/2` (authored 164/199 live-leg: 89 contested/14 dead/61 live) and
   `disappearance_verdict/2` (165/199), with the drift conjunction wired at
   `narrative_ontology.pl:168-170` (`has_mandatrophy_declaration ← fps(dead) ∧
   dv(world_rearranges)`). A future probe hunting "drift verdict" via `coordination_vitality`
   alone repeats this session's channel misidentification — check fps/dv first (two-axis lesson:
   check both surfaces before "engine has no X layer").

Also of record: `per_constraint.classifications` empty corpus-wide (known OQ-148) — type
censuses must read `per_constraint.perspectives`.

---

## 2026-07-25 — [correction-key] OQ-67 CLOSED: the legacy χ = ε × π path is fully drained; it was UNREACHABLE, not merely deprecated
**Files:** prolog/drl_audit_core.pl, prolog/drl_composition.pl, prolog/stack.pl, prolog/constraint_indexing.pl, prolog/config.pl, python/sweeps/bifurcation_sweep.py, docs/design/design_gaps.md, docs/lawvere_glossary.md, ISSUES.md, audits/2026-07-25_oq67_legacy_chi_retire/
**Tier:** correction-key

Commit `a8ec22f0`. `drl_audit_core.pl` deleted; `constraint_indexing:power_modifier/2` deleted with
it (sole reader). **The χ = ε × π path no longer exists anywhere in the engine** — every χ in the
tree is now the canonical sigmoid χ = ε × f(d) × σ(S).

**The correction this entry carries:** OQ-67 posed a two-way fork — (a) last unmigrated caller, or
(b) deliberately-separate audit path needing a declared-exemption comment. **Both premises were
wrong.** The path was *unreachable*: `stack.pl` loaded the module with an empty import list, its
only importer was `drl_composition`, and all five call sites there sat behind `constraint_data/2` /
`agent_index/2`, both ending in unconditional fail-stubs nothing in the live tree ever asserted.
Migrating dead code, or exempting a path that never runs, would both have been wrong answers to a
correctly-posed question. Three long-standing comments compounded this by naming
**`transition_paths.pl`** as a legacy-path member (`config.pl`, `bifurcation_sweep.py`,
`lawvere_glossary.md`) — already FALSE at HEAD: it computes `derive_directionality_at → sigmoid_f →
scope_modifier` and contains zero `power_modifier` references. All three corrected.

**How citations must change:** citing `power_modifier_*` as a live classifier input is now wrong.
The six params REMAIN in `config.pl:57-62` (specs `config_schema.pl:43-48`) but have **no reader at
all** — they survive as the calibration anchors the `canonical_d_*` values are fitted to
approximate. Consequence for sweeps: a null/zero-flip sensitivity result for those six now means
"no consumer," **not** "no sensitivity" — unperturbable by construction. Pre-2026-07-25 sweep
outputs that report them as inert are describing a different (still-read) regime.

**Disposal was by product, not wiring** (Build Discipline *Unwired ≠ worthless*): 3 of 4 exports
were duplicates (`structural_signature/3` ≡ `omega1_audit:determine_primary_gate/11`; `fm_alert` ≡
`drl_core:type_1_false_summit`; `omega_risk/4` ≡ `drl_core` + `transition_paths`). The 4th was
unique and is preserved as **GAP-29** — the no-exit corner (ε≈1 ∧ χ≈1), inexpressible because
`snare` is unbounded above (`drl_core.pl:389-398` gates on three floors, no ceiling). Two defects
in the deleted code are recorded in ISSUES OQ-67 so they die with it: `fm_alert` bound
`suppression_score` where `logic.md:749` Rule FM specifies ε (and dropped the `∃I(¬■C[I])` leg),
and `omega_risk`'s `type_vi` label is Type **I** per `logic.md:3293`. **Out of scope:**
`omega1_audit.pl` is itself uncalled and retains the surviving χ-only bander — not adjudicated here.

**Method note worth reusing.** The load-bearing witness was the *stub removal*, not the
reachability probe. While the fail-stubs existed, the predicates were defined-and-failing, so any
caller built by `call/N`, `=..`, or meta-dispatch — which a `forall` probe structurally cannot see
— failed silently. Deleting the stubs made them **undefined**, so a post-deletion exit-0 pipeline
run is a *positive* result rather than a null diff. That property was itself witnessed (KILL #2:
all six goals throw `existence_error`) rather than assumed. Breadth was bought on the cheap
instrument: the probe ran against all six corpora (199/960/960/1005/1001/1106, per-process
controls in each leg), the pipeline pair only on `testsets/`.

**Witnesses:** `per_constraint` byte-identical at n=199 across the run pair (exit 0 both, mtime
advanced 02:17:58 → 02:20:47, corpus md5 legs re-checked identical); `check_stack` byte-identical
to a pristine HEAD extract via `git archive` (no worktree); `load_warning_gate` 3/3 allowlisted, 0
unexpected; `./scripts/gate.sh` GREEN.

---

## 2026-07-25 — [correction-key] Gate 2 for `entropic_universe_hypothesis` RE-RULED; its June basis was void two days after it was made
**Files:** prolog/narrative_ontology.pl, prolog/signature_detection.pl, ISSUES.md, audits/2026-07-25_oq66_nlwb_filter_cutover/GATE2_REWITNESS.md
**Tier:** correction-key

**Do not cite the June gate-2 note.** The provenance question OQ-248 raised was RESOLVED by running
the discriminator (era engine extracted via `git archive`, no worktree). Three arms, A vs C being the
single-variable isolation:

| arm | engine | corpus | signature | dr_type | shadow |
|---|---|---|---|---|---|
| A | HEAD | kernel_v1 (1106) | `coupling_invariant_rope` | rope | rope-0.95 |
| B | `f600599b` | era testsets (1103) | `natural_law` | mountain | mountain-0.95 |
| C | `f600599b` | kernel_v1 (1106) | `natural_law` | mountain | mountain-0.95 |

Same corpus, different engine, opposite answer ⇒ **corpus regime REFUTED, engine regime CONFIRMED.**
The 2026-06-03 read was substantively CORRECT in its own regime. Cause of the change: **OQ-70**
(`72ec2cdd`, 2026-06-05) removed the `claimed_natural/2` source maxwell's certification rode.
**The gate-2 premise expired two days after it was ruled, and the entry it licensed was never
re-checked** — it then operated as certified for ~7 weeks on a void basis.

**RULED (operator, 2026-07-25): KEEP the entry, as a NEW DATED ruling — not a re-citation.** The
entry was NOT continuously certified 2026-06-03 → 2026-07-25. Recording it as a re-citation would
hand the next reader a pass that was never re-taken.

**METHOD TRIPWIRE — gate-2 reads must name the DISCRIMINATING surface.** The June pass cited
evidence that could not discriminate: the PASS case (maxwell) and the HELD case
(tech_inevitability) carry **identical** ε=0.08 and suppression=0.02, and maxwell-identical shadows.
Metrics and shadow were **decorative**. The June note's "omegas authored empty" is also factually
wrong — maxwell authors 11. The real discriminator is **what the omegas are ABOUT**: maxwell's bear
on physics grounding (is the second law fundamental or emergent); the held case's bear directly on
agency (*"does deployment require intentional beneficiary strategy?"*) — which is gate 2's own
question. **Rule now in the two-gate block: state which surface DISCRIMINATES and check it against a
known gate-2 FAIL; a surface shared with the failing case is corroboration at best.** The host's own
"no human agent benefits" is admitted as HOST TESTIMONY, not independent evidence — it is authored
by the story that gains from the release.

**Two OQs minted.** **OQ-251** (Priority 2): post-OQ-70, does ANY path exist by which a paradigm
natural law certifies `natural_law` absent an explicit story-level claim — *and did OQ-70 intend
that scope?* Removing an over-broad bait clause and eliminating every route are different rulings.
If `natural_law_signature` now fires only on explicit declarations, it has drifted from measuring
**structural naturality** to measuring **authorial declaration**, and every downstream consumer
inherits that silently. Gates the OQ-248 kill condition. **OQ-252**: rulings carry no back-reference
to what they license — witnessed twice this session (the reset at 7 weeks, OQ-70 at 2 days).

**`Licenses:` is FORWARD-FACING ONLY (operator ruling, 2026-07-25).** Added when a ruling is created
or revisited from this date onward; **never backfilled**, no sweep. **Corollary: absence on an older
ruling means "predates the convention," NEVER "licenses nothing"** — reading a missing field as an
assertion of no dependents is Pattern 5. First instance is on the re-ruled gate 2 in
`narrative_ontology.pl`.

---

## 2026-07-25 — [tripwire] OQ-66 CLOSED: nlwb agent-filter landed; a plain `[stack]` load leaves MaxEnt UNFITTED while reads fail soft
**Files:** prolog/drl_core.pl, prolog/tests/test_agent_beneficiary.pl, prolog/tests/fixtures/nlwb_controls/, python/run_pipeline.py, prolog/maxent_classifier.pl, prolog/abductive_triggers.pl, prolog/narrative_ontology.pl, ISSUES.md, CLAUDE.md
**Tier:** tripwire

**THE TRIPWIRE (the reason this is not just history).** **A plain `[stack]` + corpus load leaves
MaxEnt UNFITTED** — `maxent_dist/3` is empty, `maxent_run_info/3` is empty — and
**`maxent_entropy/3` / `maxent_top_type/3` FAIL rather than throw** on the missing fact. Witnessed:

```
MAXENT maxent_dist_facts_after_stack_load=0
MAXENT sample=abrahamic_covenant__isaac_covenant_reading top=FAILED(no fit)
ENTROPY FAILED (no exception) -- catch/3 does NOT intercept
```

Two consequences a fresh instance will otherwise hit silently:

1. **`catch/3` around a MaxEnt read does not intercept the unfitted case.** It fails. A
   `catch(maxent_entropy(...), _, (H = 0.0))` recovery goal never runs; the enclosing clause
   just fails.
2. **Any probe or suite that reads MaxEnt observables under `[stack]` alone measures NOTHING,
   and a soft-failure mapped to a placeholder makes that indistinguishable from a real
   result.** This is the defect that made the OQ-66 guard vacuous for its whole life: the old
   `test_agent_beneficiary.pl` mapped the failure to `no_top` in BOTH arms of a raw-vs-filtered
   diff, so it compared `[no_top,no_top,no_top,no_top]` against itself and presented as
   zero-diff. Pattern 6, inside the instrument.

**To read MaxEnt at all:** `maxent_classifier:maxent_cleanup, maxent_classifier:maxent_multi_run(Ctxs, _)`
first, then ASSERT `maxent_dist/3` is non-empty before any read. MaxEnt is corpus-fitted state
deliberately OUTSIDE `cache_registry`, so `clear_all_caches/0` does not touch it — a cache clear is
not a refit. Template: `audits/2026-07-25_oq66_nlwb_filter_cutover/nlwb_diff_harness.pl`.

**Scoped to the class, and the wider claim was CHECKED AND REFUTED.** The suspicion that
`abductive_triggers.pl`'s six plausible-value fallbacks (`HNorm = 0.0` at `:86,:135,:358,:711,:771`,
`ShadowTop = unknown` at `:188`) are a live Pattern-6 — an entropy of 0.0 reading as maximal
certainty — **is not real, twice over.** (a) Those sites are bare `catch/3`, and the reads fail
rather than throw, so the recovery goal never runs. (b) Every one of those clauses is gated at its
FIRST goal by `subsystem_available(maxent)` (`:75,:126,:177,:231`), which checks `maxent_run_info/3`
— empty under a plain `[stack]` load, so the clause fails before reaching the fallback.
`abductive_triggers.pl` already carries the provenance guard. **No OQ minted for those sites.**

**LANDED (OQ-66 resolved).** `drl_core:natural_law_without_beneficiary/1` now reads
`narrative_ontology:agent_beneficiary/2` instead of raw `constraint_beneficiary/2` — ruling 63-A,
operator Q1 2026-07-25. It was the last unmigrated consumer of that class.

**Say the result at the right quantity — "behaviourally free" is the WRONG label and it is the
one a later reader will reuse.** **ZERO OBSERVABLE DIFF on six legs** (five live + `kernel_v1`),
under cache-cleared and MaxEnt-refitted arms, with a planted-flip fixture leg proving the harness
can see a change. **But ONE PREDICATE-TRUTH FLIP** at `maxwell_demon_impossibility` (kernel_v1) —
downstream-invisible only because it classifies `rope` in both arms. And the no-op is **STRUCTURAL
on the five live legs** (forced by `registry_hits=0` ⇒ extensional identity) and **CONTINGENT on
`kernel_v1`** (holds only because one constraint's metrics land in rope territory). Forward
statement to cite: *no observable change on the checked corpora; the first live constraint carrying
a registered non-agent beneficiary with snare-range metrics will classify differently than it would
have pre-cutover.* Consumer surface + declared residue (the tangled_rope block has no dedicated
fixture — `nlwb` forbids `requires_active_enforcement` by construction):
`audits/2026-07-25_oq66_nlwb_filter_cutover/RELEASE_NOTE.md`.

**METHOD NOTE (carry forward).** The plan's stop point was specced to fire on a non-zero *diff*,
but what it protected — the operator's seat on the release note and the consumer re-audit scope —
is triggered by a *predicate flip*. The flip happened, the trigger did not fire, and the release
note got written after the commit instead of before. **Key a stop point on the quantity that
carries the meaning, not the one the harness happens to emit.**

New standing pipeline
gate `_prolog_agency_gate()`; its FIXTURE pass is what makes it non-vacuous, because the live legs
carry zero registered beneficiary values and a revert of `drl_core.pl` keeps the live-corpus suite
GREEN. Break control witnessed: reverting throws `agency_nlwb_set([nlwb_ctl_no_beneficiary])`.

**CORRECTION-KEY rider — the maxwell gate-2 evidence does not re-witness.** The registry entry for
`entropic_universe_hypothesis` records its gate-2 justification as "MaxEnt shadow 0.990 mountain /
entropy 0.031" (2026-06-03). The first properly-fitted read of `maxwell_demon_impossibility` on
`kernel_v1` gives **shadow rope 0.95 / entropy 0.156 / mountain 0.010**, signature
`coupling_invariant_rope`, `dr_type` rope at all four contexts. Controlled against a degenerate fit
(same run spans all six shadow types, `mountain-39 … tangled_rope-641`, entropy 0.0011–0.6111).
**Scope: this says the numbers do not reproduce on `kernel_v1` at HEAD — NOT that the 2026-06-03
read was wrong.** That read was on the then-live pre-reset corpus, MaxEnt is corpus-fitted, and the
signature layer has changed repeatedly since; attributing the gap to corpus vs. engine regime needs
a stage-hash diff, not run. **Not acted on** — re-ruling a `non_agent_beneficiary/1` entry is a
gate-2 ruling and the operator's seat. Routed to OQ-248 as its opening datum and flagged.

**Ledger.** Both gate-two items close **moot-by-reset** (`technological_inevitability_interpretation`
absent from all five live legs; the `statutory_debt_ceiling` names in `haiku`/`flash` are new draws,
not the measured story). Findings relocated, not folded: shadow separability → **OQ-248** (Ω_E,
GAP-19 cross-link in prose — `Deps:` edges take OQ targets only); (ε, theater) × type census →
**OQ-249** (Ω_E, gates OQ-90).

**Five live legs, not three** — see the CLAUDE.md Critical Distinctions correction in this session.

Evidence: `audits/2026-07-25_oq66_nlwb_filter_cutover/FINDINGS.md`, commit `1613c3cc`.

---

## 2026-07-25 — [tripwire] OQ-62 CLOSED: four purity banders renamed to disjoint vocabularies; exactly one `purity_zone/2` survives
**Files:** prolog/logical_fingerprint.pl, prolog/fpn_report.pl, prolog/giant_component_analysis.pl, prolog/abductive_helpers.pl, prolog/abductive_triggers.pl, prolog/signature_detection.pl, prolog/purity_scoring.pl, prolog/tests/test_purity_bands.pl, prolog/tests/test_purity_absence.pl, python/husk_signature_read.py, python/enhanced_report.py, docs/logic_extensions.md
**Tier:** tripwire

**The tripwire.** There is now **exactly one** bander named `purity_zone/2` and it is the
canonical spec one (`logical_fingerprint.pl:614`, logic_extensions.md §2.3). The other three are
`fpn_report:ep_band/2`, `giant_component_analysis:action_band/2` and
`abductive_helpers:fpn_band/2`, and they are **not interchangeable with it or each other** —
different quantities, different cut points. A future agent who unifies them, or who "restores"
the shared name, reintroduces a defect that fails *silently*: the bands still compute, the
reports still render, and the numbers are wrong by one cut-point. Convention table with the
quantity each one bands: `docs/logic_extensions.md` §2.3.1.

**Second tripwire, opposite direction.** All four banders return the SAME `unknown` token, which
is a literal overlap against the disjointness rule and is **deliberate** — unlike the colliding
words, `unknown` means the same thing everywhere (input absent or out of range, fail closed).
Do not "fix" it; doing so undoes the fail-closed guarantee. The guard clause order is also
load-bearing: `\+ number(S)` must precede `S < 0.0`, because the comparison throws on the atom.
Exactly 0.0 is a real score, not an absence, and still bands worst.

**What was wrong.** Three modules each defined `purity_zone/2`; three words collided, not the one
OQ-62 recorded — `contaminated` ([0.30,0.50) vs [0.40,0.60)), `degraded` (<0.30 vs [0.30,0.50)),
`critical` (<0.30 vs <0.20). With the categorical `contaminated(Reasons)` (now `purity_fail`),
one word meant four things. All four banders also mapped the −1.0 epistemic-gate-fail sentinel to
their WORST zone, and two threw `type_error(evaluable, unknown/0)` on the OQ-60 no-data atom.

**Three premise corrections** (detail + witnesses in ISSUES OQ-62 and
`audits/2026-07-25_oq62_band_vocabulary_fork/CALL_SITE_CENSUS.md`):
1. *The fork was 4 banders, not 2.* The authoring audit (2026-06-03) never mentions `fpn_report`
   or `giant_component`, and does not cite `audits/2025-05-15_recon_2/`, which had already
   recorded three `purity_zone/2` implementations. **Predicts sibling undercounts from the same
   audit** — treat its other counts as floors, not totals.
2. *The sentinel path is structurally unfiltered but empirically inert — three claims, three
   warrants, do not merge them.* **(a)** unfiltered = code read (only the intrinsic is gated).
   **(b)** no leg exercises it = WITNESSED, six corpora, measured at the bander INPUT, pure
   `value` on every leg (testsets 153 rows / haiku 492 / flash 668 / kimi 700 / sonnet 930 /
   kernel_v1 1102). **(c)** *why* = DATA on one leg, NOT traced: IP-absence and EP-absence are
   set-equal on testsets (28 ≡ 28, membership not cardinality), so the `IP >= 0.0` filter is
   co-extensive with EP-absence *here*; whether it structurally guarantees exclusion was never
   traced. Under (c) the path is **unexercised, not unreachable** — the guard's real value is that
   it converts a data-dependent property into a code-guaranteed one. Do not cite a 0-`critical`
   count on one leg as evidence about reachability; it is that leg's purity distribution.
3. *The throw was never loud.* `abductive_engine.pl:145` wraps every trigger in
   `catch(_, _, true)`, so the `type_error` was already being discarded. Guarding converted one
   silent path into another.

**Two method traps hit and recorded** (both produced confident wrong answers before the control
caught them):
- A reachability probe using the atom `default` as context, instead of
  `constraint_indexing:default_context/1`, landed off the authored grid: `fpn_run/3` failed and
  every accessor reported 0 successes — which reads exactly like "the path is unreachable"
  (OQ-178 dual). The `fpn_run` success count is now the probe's positive control.
- **In-process multi-leg iteration is unsound.** Retracting `corpus_loaded/0` and
  `corpus_constraint/1` does NOT retract the `narrative_ontology` facts the testset files
  asserted, so legs accumulate and `sort/2` masks it behind ID dedup. The tell was kimi and
  sonnet returning byte-identical counts; re-run one leg per **process**, they differ (700 vs 930
  rows). Any future multi-leg sweep must fork per leg. **Blast radius is not local: this
  invalidates any prior in-process multi-leg measurement in this project** — now **OQ-246**,
  Priority 1, carrying the detection recipe (two distinct legs agreeing to the row is the
  signature; a contaminated leg reports a SUPERSET, so "found X on leg L" may be "found X on legs
  1..L"). The six-leg table above was measured per-process AFTER this discovery — verifiable from
  the numbers, since the in-process run put haiku at 642 rows and the table carries 492.

**The rename check byte-identity could not provide (and the defect behind it).** `fpn_band/2`'s
only consumer is trigger 6, which fires 0, and `abductive_engine.pl:145` swallows every trigger
exception — so a missed call site would have left 0 firings, a byte-identical
`abductive_report.md` and a green gate, exactly as a correct rename does. Closed by
`trigger6_control.pl`: T6 called directly outside the catch on all 181 constraints → **0
exceptions**; reach-depth then shows control actually arrives at the renamed goals
(`:525 fpn_band/2 → unknown`, `:526 one_hop_band/3 → failed cleanly`; a missing predicate throws
rather than fails, so a cleanly-failing goal resolved). The overlay route was unavailable — both
blockers are static procedures, so `assertz` raises `permission_error`. `:534`'s `evidence_line`
key is term data, not a goal, so it is read-verified only. **Incidental:** `:525 → unknown` is the
Phase-1b guard firing live in the real trigger path (pre-guard `fpn_critical`), so the guard does
change an intermediate value at the 28 `-1.0` constraints — "inert" is exact about output, not
evaluation. **The blanket `catch(_, _, true)` is now OQ-247:** all ten trigger firing counts are
ambiguous between "didn't fire" and "errored," which means the 0-firing count for
`accelerating_pathology` cited when OQ-62 opened was never a witness of non-firing.

**Straggler class worth remembering.** `python/husk_signature_read.py` parses
`outputs/fpn_report.md` and gated `proxy_husk` on the literal string `"critical"`. Post-rename
that matches nothing and reports zero proxy husks — success-shaped, reads like a finding. It is
**not wired into `run_pipeline`**, so no pipeline diff would ever have caught it; only the
unfiltered Pass-B token sweep did. Its columns were also named `fpn_zone`/`one_hop_zone` while
holding `fpn_report` values, i.e. named after the wrong bander.

**Witnesses.** `a2ef8147` (docs) · `a1902cb1` (guard) · `295260e7` (2a renames) · `13877a0c`
(2b categorical). `test_purity_bands.pl` RED at HEAD (7 failed / 9 passed, both throws captured)
→ GREEN 16/16 with 7 positive controls. Pipeline exit 0 + mtime advanced at each phase;
`per_constraint` byte-identical throughout; `fpn_report.md` byte-identical after back-substituting
the new atoms; the other two reports byte-identical untouched. `structural_purity` verdict mix
preserved exactly across the rename (purity_fail 151 / inconclusive 35 / inconclusive_nodata 4 /
pure_coordination 9). purity_absence 7/7, reading-totality 10/10, `[GATE]` GREEN.
Follow-ons minted: **OQ-244** (scalar identity — do any two band the same quantity?),
**OQ-245** (is the ≤0.05 excess bar calibrated, or is 96.6% failure the finding?), **OQ-246**
(in-process leg accumulation), **OQ-247** (blanket trigger catch-all).

---

## 2026-07-25 — [landed] schemas.py caught up to three producer landings; the drift warning is the only thing that noticed
**Files:** python/shared/schemas.py, prolog/json_report.pl, python/enrich_pipeline_json.py
**Tier:** landed

`PIPELINE_FIELDS` now registers `epsilon_provenance` (OQ-205), `fingerprint_shift`
(OQ-53/GAP-04) and `repair_transitions` (OQ-91), plus the two enriched ε-stability fields.
**OQ-205 was already RESOLVED (build landed 2026-07-03)** — this was its missing last step.

**The lag is the finding.** json_report.pl emitted all three for ~3 weeks while the schema
contract did not list them, and the only signal was `validate_pipeline_output`'s "unexpected
field" drift warning — firing 3× on each of 199 rows, into stderr, every enrich run, noticed
by nobody. A drift warning that never escalates is a Pattern-6 channel: it distinguishes
*contract-complete* from *contract-lagging* correctly and then emits both into the same
ignored stream. **Registering a field in `python/shared/schemas.py` belongs in the same
commit as the `json_report.pl` emit** — the contract is a consumer of the emit, and Pattern 1
("a producer is not done until something consumes its output") covers it.

**Nullability method (reusable).** A wrong NON-nullable declaration makes `enrich_pipeline_json
.py` hard-exit — same failure class as the stale `purity_class` that broke the chain the day
before. So each declaration was witnessed at two altitudes: emit-site structure (which branch
can write what) AND branch coverage on the live leg. `epsilon_provenance` non-null because
`write_epsilon_provenance/2` is a total if-then-else with both arms writing `{...}` (all four
paths fired: 71 authored / 110 derived / 18 unknown_author); `repair_transitions` non-null
**by construction** — the emit writes literal `[` / `]` around `write_repair_array`, so no
corpus can make it null (196 empty / 3 non-empty); `fingerprint_shift` nullable via its
explicit `FsList = null` arm (`json_report.pl:313`) — that arm did NOT fire here (199/199
lists), noted because it is the permissive direction and so cannot break a consumer.
Presence/nullability is emit-structure-determined, not corpus-determined, which is why this
was not run across the other four legs.

Witnesses (manifest 2026-07-25T05:34:25Z, commit `13877a0`, n=199): both validators 0 errors,
and **0 unexpected-field drift warnings** on either artifact — the contract is now complete
w.r.t. what the engine emits.

## 2026-07-24 — [landed] OQ-60 consumer sweep came due: `unknown` crashed the trajectory step; Prolog stderr reporting was masking it
**Files:** prolog/context_profile_mining.pl, python/run_pipeline.py, python/shared/schemas.py
**Tier:** landed

Pipeline went 47/48 → **48/48** (`python3 python/run_pipeline.py`, exit 0, 28.6s;
`trajectory ok [3.1s]`, `outputs/context_profile_report.md` 7,125 bytes — previously
written EMPTY because the step errored).

**The crash (commit `ab748fc6`).** `context_profile_mining.pl:434` read
`normalize_purity(P, 0.5) :- (P =:= -1.0 ; \+ number(P)), !.` Both disjuncts intend to
map an absent purity to 0.5, but the guards are in the fatal order — `=:=` evaluates its
args, so `P =:= -1.0` **throws** on the atom `unknown` before `\+ number(P)` is tried:
`ERROR: =:=/2: Arithmetic: 'unknown/0' is not a function`, at
`[trajectory] Computing 16290 pairwise distances`. Reordered (non-number guard first),
split into two clauses so each OQ-60 token is named at its own site. This is the **OQ-60
consumer sweep coming due, not a new defect**: `purity_scoring.pl:49-55` introduced
`Score = unknown` with the comment "propagate `unknown` rather than feeding it to the
weighted sum (which would throw)" and marked the path "inert until a producer emits
`unknown`" — a producer has now landed (live corpus: `purity_class` = 153 scored / 35
gate_fail / **11 no_data**), and this consumer one level down did exactly that throw.
`normalize_purity/2` is the sole chokepoint (line 426 `PurDiff` is the only purity
arithmetic in the trajectory path; swept `context_profile_mining.pl` +
`context_profile_report.pl` for other `Pur*` arithmetic — none).

**Why it was hard to see (commit `55c8b242`).** `run_prolog`'s failure path did
`result.stderr[:300]`. SWI emits load-time warnings for hundreds of lines before the
ERROR, so a head-slice is structurally guaranteed to be noise **on every failure across
all 12 Prolog steps**. The real stderr here was 259,426 chars / 2,311 lines; the head-300
showed two "Local definition ... overrides weak import" warnings and cut off mid-word, so
the summary reported a warning and never mentioned the exception that ended the run.
Added `salient_stderr()`: prefer ERROR lines, fall back to the **tail**, never the head
(Build Discipline Pattern 6 — a channel that cannot tell payload from noise emits
noise-shaped output either way).

**Checked, NOT a defect.** `json_report.pl:1347/1349` (`write_one_neighbor`) filters
neighbor purity with a bare `NP \= -1.0` — no `number/1` guard — where its twin
`write_contamination_network:1282` uses `number(IP1), IP1 \= -1.0`. `unknown \= -1.0`
succeeds, so the atom does pass that filter. It is **defended at the emit boundary**:
`write_json_number/2:2549` has an explicit `unknown → null` clause plus a non-numeric
catch-all. Verified on output, with the positive control that the site is genuinely
reached: 26 neighbor-writes involve `no_data` constraints, and all 26 emitted `null`
(neighbor purity values: 188 float / 28 null / **0 string**). The asymmetry is
redundancy, not a bug — do not "fix" it expecting a behavior change.

**Open (needs a ruling), not filed as an OQ yet:** `normalize_purity` maps `unknown` to
**0.5** — a fabricated plausible value inside an HAC distance component (Pattern 6). The
fix preserved the clause's evident pre-existing intent; excluding the purity component
and re-weighting when either side is absent would change clustering output. Also unswept:
~50 other `purity_score/2` call sites across ~15 modules. The loud shape (arithmetic)
would crash and the pipeline is green, so none of the *reached* sites throw on this
corpus — but that is "didn't find it," not "isn't there."

## 2026-07-24 — [landed] OQ-61 CLOSED: header purity/cascade three rulings (severe fraction + type×band tab + gate_fail/no_data split); "purity restates type composition" premise WITHDRAWN; residual → OQ-239/240/241
**Files:** prolog/json_report.pl, prolog/network_dynamics.pl, python/enhanced_report.py, prolog/tests/test_purity_absence_class.pl, python/tests/test_oq61_network_render.py, ISSUES.md, python/shared/schemas.py
**Tier:** landed

Commit `ae9b0848`. Three operator rulings on the corpus header, **report/aggregation only** —
proven additive (behavior-preservation: two changed-code runs canonicalize identical; HEAD-vs-changed
adds 6 diagnostic keys + per-row `purity_class`, ZERO changed shared values, `network_stability`
token byte-identical). Q1: header severe **fraction** (four fail-closed branches from clause order)
replaces the saturated categorical (633/643 severe at absolute threshold 3); shared helpers
`network_drifting_constraints/2`+`network_severe_constraints/3` extracted (behavior-preserving);
`severity_by_type` backstop tab (severe total == `network_n_severe`, asserted). Q2: type×band tab
(render-only, marginal-asserted) headlining the off-diagonal residual. Q3: unscored split into
`gate_fail`(−1.0)/`no_data`(`unknown`); `malformed`(out-of-range) is a **fail-closed guard-class,
NOT a fifth token** — the emit halts on it. Reproduces the census sentinel+flip split exactly on
all four target legs (testsets 46=35+11, haiku 468=466+2, flash 292=212+80, kernel_v1 4=2+2);
kimi/sonnet fresh. Fixtures: 14 Prolog classifier tests + 16 Python render tests, existing 17
purity-absence green.

**Correction (operator ruling, escalated call):** the plan pre-registered a rule gating Q1 on
"the corpus-stability line has no residual signal beyond type composition." It **FAILS across the
corpus family** — off-diagonal severe mass (severe rope+mountain / n_drifting) > 5% on all six legs
(scored-denominator: 5/6, only kimi passes); mountain within-type under-severity real on large-n
legs (flash n=51). The premise is **WITHDRAWN** — do NOT cite OQ-61 as establishing that purity
restates type composition. The header change stands on **saturation grounds alone** (633/643 is
information-free regardless), and the residual *strengthens* the ruling's per-component future.
Nothing implemented reverses. Residual split into OQ-239 (per-component severity home + the rule's
two defects: no n-floor, unprincipled 5%), OQ-240 (off-diagonal cover-story population — a
classification/calibration question, not report-text), OQ-241 (`ep_base_severity` fixed-0.70 cut
type-interaction). Audit: `audits/2026-07-24_oq61_header_purity_cascade/`.

**Follow-ups (both done at operator request, 2026-07-24):** (1) `purity_class` registered in
`python/shared/schemas.py` `PIPELINE_FIELDS` (non-nullable) + `PipelineConstraint` — commit
`ae3090d2`, isolated from the operator's uncommitted OQ-205 schema WIP via stash/commit/restore
(non-overlapping region; verified 0 validation errors, no drift warning). (2) CLAUDE.md updated
**THREE→FIVE LIVE LEGS** (added `testsets_kimi` 1005, `testsets_sonnet` 1001; disk-verified
2026-07-24); MEMORY.md `project_corpus_reset_2026_06_05` note refreshed. NB: the operator's
uncommitted set (`schemas.py`, `validation_suite.pl`, `cs_reading_relation_quarantine.json`,
`oracle_gap_results.json`) is OQ-205 ε-provenance engine WIP, **not** essay constraint stories —
left intact.

## 2026-07-24 — [landed] OQ-152 / OQ-153 / OQ-227 husk bundle CLOSED; `update_authority` field is validated-but-dormant; two unfireable reviver conditions
**Files:** ISSUES.md, docs/design/design_gaps.md, docs/design/update_authority_rubric.md, prolog/narrative_ontology.pl, prolog/data_validation.pl, prolog/cs_drift_engine.pl, prolog/tests/test_cs_drift_engine.pl
**Tier:** landed

The three closes (reasoning in ISSUES.md; evidence in `audits/2026-07-24_oq152_seat_crosssection/` and
`audits/2026-07-24_oq153_step3_blind_pass/`):
- **OQ-152 disposed** — the per-seat naturalization-collapse cross-section is unfingerprintable:
  suppression is a constraint-level *gate* (not a seat dial) and the seat-χ ordering is config-fixed
  by `role→d` (0/158 within-constraint crossings). Spun out **GAP-27** (`agent_power` is inert for
  seat χ under δ=0).
- **OQ-153 resolved → (c) decline** the five-condition husk annotation. Two findings drove it:
  `dead∧frozen = 0/8` under enrichment (the husk conjunction is empty — a *mechanism* for
  `husk_signature_read.py` K=0), and condition-5 independence is *untested* (the corpus cannot populate
  non-canon `frozen`; 3/4 shape-test items failed to instantiate the shape — not "proxy," untested).
- **OQ-227 resolved on C1+C2** — surviving-referent precondition is a structural tripwire
  (`test_cs_drift_engine.pl terminal_set_pinned`); `acknowledgment_collapse` routed to the standing
  trigger; `sealed_closure` row OPEN.

**`update_authority` — the item most likely to be wrongly deleted.** A validated institutional field
(enum `{licensed_revisable, frozen, absent_diffuse}` + authoring token `unauthored`; rubric
`docs/design/update_authority_rubric.md`) with **ZERO authored facts, no consumer, no generation-schema
emission — declared-dormant by design** (authored-on-demand by audit passes). Surface = the dynamic
fact in `narrative_ontology.pl` + validators in `data_validation.pl` (enum/uniqueness/orphan +
measure-only `inconsistent_update_authority/2`). It is NOT dead schema — provenance is the OQ-153 close.
Do not remove it as "unused."

**Two UNFIREABLE conditions — reviver documentation, nothing monitors them** (do not read as tracked):
GAP-28's reopening condition (≥3 non-canon live-foreclosed-amendment instances — only checkable by
authoring the dormant field) and OQ-227's `sealed_closure` near-miss (no authored referent-dissolution
signal exists to search on). Both are surfaced only by a **corpus-expansion / generation-scope
decision**, not a query.

**Pattern for the next instrument build:** *selection on the outcome variable* recurred four times
across this arc (all caught) — enriching a sample so a needed value is present severs the sample from
the question. Guard: pre-register a selected-for value as supply-only and exclude it from any test that
reads presence as evidence; only *absence under enrichment* carries information. Full note:
`audits/2026-07-24_oq153_step3_blind_pass/RESULTS.md` → "PATTERN FOR THE NEXT INSTRUMENT BUILD".

---

## 2026-07-23 — [tripwire] OQ-60 RESOLVED: no-data purity is `unknown`/JSON null (never 1.0); fabricated boltzmann_floor_default removed; two absence tokens must never be coerced or averaged
**Files:** prolog/purity_scoring.pl, prolog/boltzmann_compliance.pl, prolog/signature_detection.pl, prolog/json_report.pl, prolog/network_dynamics.pl, prolog/giant_component_analysis.pl, prolog/maxent_report.pl, prolog/grothendieck_cohomology.pl, prolog/drl_boltzmann_analysis.pl, prolog/context_profile_mining.pl, prolog/tests/test_purity_absence.pl, prolog/tests/test_coexists_fpn_canary.pl, python/enhanced_report.py, prolog/config.pl, docs/logic_extensions.md
**Tier:** tripwire

OQ-60 deliberate pass completed (rulings R1–R4; commits `bc9bffde`→`d051d06c`; full witnesses in
`audits/2026-07-17_oq60_purity_absence/*_2026-07-23.md`).

**READ rule (promoted to CLAUDE.md Architecture Invariants):** purity now carries TWO absence
tokens — engine `unknown` (no-data) and `-1.0` (epistemic-gate-fail sentinel); JSON serializes
BOTH as `null`. Never coerce either to a number, never average them in, never read `.get(...,0)`.
`purity_zone(unknown)=unknown`; a `purity_band` of JSON null covers both causes.

**WRITE rule:** a clean/dispositive aggregate over purity (pristine/stable/pure_*) gates at
coverage 1.0 → distinct abstention token (`inconclusive(no_data)`, `undetermined`); positive
existentials (contaminated/cascading/drift) fire through unknown members; every descriptive
purity stat carries `n_scored/n_total` unconditionally (json `diagnostic.purity_n_scored/_n_total`;
report coverage lines).

**Ordering trap:** atoms sort BEFORE numbers — an `unknown` reaching msort/max_member silently
heads the list. Guarded at the two cache boundaries (fpn/gc precompute collapse unknown→-1.0 for
their `>= 0.0` filters); new sorts over purity must guard `number/1` (ordering audit:
`ORDERING_AUDIT_2026-07-23.md`; tests 6–7 of test_purity_absence.pl).

**Fixture rule:** synthetic test constraints that need SCORABLE purity must now AUTHOR
`coordination_type` (+ extractiveness) — the engine no longer fabricates a floor (witnessed: the
FPN canary fixtures and the preflight non-target control both broke on this and were repaired).

**Ripple (declared, attributed):** removing 93 fabricated floors moved corpus-relative layers
(maxent empirical-profile fits, wasserstein, arakelov, signature_pressure, FPN contamination) —
headline `classifications` changed on ZERO rows; 12 near-boundary rows flipped shadow
maxent_top_type, 9 downgraded verdict_join red→yellow via the maxent-divergence alert; 1
gate-fail flash row lost its fabricated excess_above_floor FCR failure. Scorable-mean purity by
leg: testsets 0.5450, haiku 0.4916, flash 0.5711 (moved DOWN as predicted — the operator's
falsifier), kernel_v1 0.4813. Cross-leg scorable means NOT comparable (OQ-236).

---

## 2026-07-23 — [correction-key] *Hearts of Glass* is NOT a pipeline artifact (provenance witnessed); Commitment Systems adjudicated as blocked-B refinement of the debugging-philosophy taxonomy
**Files:** blog/2026-07/hearts-of-glass.md, blog/2026-07/implied_machine_reader.md, agent/analysis/originals/machine_reader.md, prolog/cs_drift_engine.pl, docs/commitment_systems/type_b_adjudication_2026-07-23.md, docs/debugging_philosophy.md, ISSUES.md
**Tier:** correction-key

Two rulings from a Claude-web planning conversation (2026-07-23), landed to substrate.

**1. *Hearts of Glass* (`blog/2026-07/hearts-of-glass.md`, commit `50d7ddab`) sits OUTSIDE
the narrative_transform/uke pipeline — do not cite it as pipeline output.** The conversation's
kill condition ("a stage-1 constraint spec or a .pl behind it locally would refute this") was
run this session and does NOT fire. Witness: no `agent/narrative_transform/uke/hearts*` run
dir, no `originals/`/`stories/` entry, no `prolog/testsets/*.pl`, no `json/` spec; positive
control `quellcrist` found across all those surfaces, so the probe finds pipeline artifacts.
Corroborated three ways by the operator: (a) not written with agent/uke-narrative (where a uke
artifact would appear); (b) the process leaves a characteristic artifact shape (uke run dir +
`source_story.txt` + staged outputs) the story lacks; (c) the pipeline requires a seed — the
story's actual seed was a conversation about human hibernation. **Distinction to preserve:
"the system" (Prolog/uke) ≠ LLM assistance** — the commit carries a Claude co-author line, so
model collaboration happened; pipeline involvement did not. Operator framing: "the system is
my creating Prolog that thinks like I do." FLAG RESOLVED same day (operator authorized the
edit): the provenance sentence — "from this site's pipeline — grown ... through the
multi-model process" — corrected in BOTH copies (`blog/2026-07/implied_machine_reader.md`,
the later revision staged to publish, and the older draft
`agent/analysis/originals/machine_reader.md`) to affirm multi-model involvement while denying
pipeline involvement. The story remains OQ-227's *test-rig* (the 2026-07-18 entry below).
Sharper description than "outside the system" (operator, same day): the story is an
*instrument* for the thing the engine cannot yet formalize — `acknowledgment_collapse` (the
ratifying authority that can no longer tell faith from its performance) is the Keeper's
terminal state near verbatim. Not pipeline output; aimed at the pipeline's frontier.

**2. Commitment Systems classified: a refinement on Type B, by the fix-identity criterion
(operator ruling) — full adjudication in
`docs/commitment_systems/type_b_adjudication_2026-07-23.md`.** Types are individuated by
their FIX, not their generating mechanism ("drift-generated" labels the A bucket, doesn't
gate entry; frame-fixing passes trivially-and-uninformatively on a drifting institution,
which is not a confused reasoner). The refinement precisely: debugging_philosophy's Type B
silently assumed the reviser exists (Russell→ZF, Liar→Tarski); drop that and B splits into
revisable-B vs **blocked-B** — Commitment Systems is the theory of blocked-B, the five
patterns its map. A proposed "Type D (extraction-generated)" collapses into B (standing is
constitution-level). Two marked open notes in the adjudication file: the Euclid exception's
seat at axiom-set choice, and remedy-identity making the framework a theory of repair rather
than drift. **Round-2 sharpening (same day, adjudication Addendum 1): blockage is a GATE on
remedy execution, not a type** — restoring standing is the enabling condition for running a
fix, not a fix (nobody resolves Sorites by acquiring authority); trifurcation stays at three;
Commitment Systems is the theory of the gate; Stage 0 in precise form = "does an agent exist
who can execute the fix and wants to." **Round 3 (Addendum 2 + OQ-235 minted):** three-valued
gate proposed, unruled (vacancy/capture/bandwidth — treatments disjoint: build authority /
change incentives / add throughput); the §5.9 closable-by-citation check returned the
OPPOSITE of the Euclid case — v8:700–706 names the same wall (acknowledged bit authored-not-
detected, evidence thinnest where formation completed) as its own honest residual. The open
capability question — a detected (vs authored) acknowledgment surface via self-account/
non-report-practice divergence, plus conversion-rate predictors — is **OQ-235**.

---

## 2026-07-23 — [correction-key] Axiom 2's empirical anchor currently has no runnable falsifier (OQ-232 resolved; falsifiers rescoped in v8 §9.5 + v6.13.1:88; class OQ-234 minted)
**Files:** docs/deferential_realism_paper_v8.md, docs/deferential_realism_paper_v6.13.1.md, ISSUES.md, audits/2026-07-23_oq232_falsifier_redesign/
**Tier:** correction-key

**Headline: Axiom 2's empirical anchor currently has no runnable falsifier: the E-arm is
instantiable on its categorical read (its magnitude criterion is sub-resolution at the
powerless position the kill names, on realistic rating scales) but refutes only (Axiom 2 ∧
no P-channel adaptation); the P-arm is specified and uninstantiated — sign-discriminating
only positive-vs-flat at realistic instruments, so its instantiation spec is a floor-free or
fine-resolution measure. Do not cite either paper's pre-amendment wording as the
falsifiability warrant.**

Arm bookkeeping (below the headline, not in place of it). OQ-232 rev-4 redesign landed
(commit `e9ab87ac`; probe + writeup in `audits/2026-07-23_oq232_falsifier_redesign/`): the
single confounded kill (controlled information-access and position, not preference
adaptation) replaced by two scoped falsifiers, each refuting an explicit conjunction —
E-arm 2×2 Cell 4 (position-matched inside/outside cohorts; candidates: FTC non-compete
window, H-1B portability, plant closures with pre-period surveys); P-arm sign read
(position change at held entrapment; candidates: promotion studies, union-officer
elections, tenure cases; OPEN-instantiation). Probe findings that scope any future
citation: (a) the cancellation band's lower edge is −23.6% of the axiom's own P-slope and
the band is UNBOUNDED ABOVE at 7-point resolution — the bounded scale floors the
inversion, so the sign table's negative row is unreachable under proportional adaptation
(flat = axiom-false ∨ cancellation ∨ dominant-adaptation-floored); (b) the E-arm magnitude
criterion's 1× bound sits below one 7-point scale step at every held position and its
3×-inflated bound crosses resolution at mid-slope positions — the 1×–3× indeterminacy is
live; the criterion is licensed only under an approximately linear reporting channel.
Part B: Axiom 7's falsifier carries the same confound class (PRESENT per pre-registered
criterion); its v8 kill is now conditioned on structural witnesses (consistent negation,
present beneficiary, §5.9) via explicit cross-reference. Old falsifier text survives only
in archived v6.x versions and docs/v8/foundations/ snapshots — historical records,
contained by pointer, not amended. Class tracking: OQ-234 (evidentiary-bridge class;
channel enumeration partially discharged — site indices are exactly {P, T, E, S}, only
{P, E} adaptation-indexable; open question sharpened to index-set completeness).

## 2026-07-23 — [landed] Recall limit + borrowed-example rule (uke_write §5.4, OQ-233); Axiom-2 falsifier confound (OQ-232); perspectival-gaps essay landed (blog/, GAP-04/06 notes)
**Files:** agent/uke_write_v2.2.md, ISSUES.md, blog/2026-05_or_before/diagnostic_value_of_perspectival_gaps.md, docs/design/design_gaps.md
**Tier:** landed

Day's three verified Claude-web critiques, each landed at its right altitude: (1) **OQ-232** —
Axiom 2's falsifier (v8 §9.5 / v6.13.1:88) is confounded by adaptive preference (controls
information-access and position but not preference-adaptation-from-entrapment, a mechanism the
framework itself models); candidate repair is perturbation-shaped (hold position, vary
entrapment) per docs/the_perturbation_principle.md; papers' kill condition not citable as
falsifiability warrant until amended. (2) **Perspectival-gaps essay** (2026-02-11, was
published-never-landed) → blog/; three-feature test → GAP-06 as upstream
diagnostic-vs-erroneous pre-filter (GAP-06's stale "Deferred" corrected — router was built
2026-06-20 via OQ-55); GAP-04 population-layer note, operator-framed as generative not factual
(orbit = computed attitude-range; whether assigned position moves authoring routes to
OQ-73/OQ-228). (3) **OQ-233 + uke_write §5.4** — verification is precision-shaped
(draft-as-query-source; fresh reader inherits the seat from the artifact; recall needs a
different question, not a different reader); borrowed-example rule (query the example in its
home field) landed as protocol with checklist items + §6 "Corpus consulted" declaration
(essay-layer sibling of OQ-230's coverage field); pattern claim pre-registered off n=2 with the
reviewer's kill condition and the egocenter probe recorded before running. §5.4 placement chosen
to keep the OQ-185 uke_write:180 anchor stable (verified).

**OQ-233 probe RUN same day (n=1 datum, operator's separate-corpus seat; load-bearing citation
independently spot-verified here via WebSearch — the one external element per the declared
seam).** Scored precisely: the borrowed-example rule **HIT on location** (counterexample at the
import point, in vision science's home field, as predicted) and **MISSED on substance** (the
pre-registered egocenter-shift hypothesis was half-wrong; the real counterexample is Erkelens &
van Ee, Vision Research 2002, "sometimes inappropriate, always irrelevant" — the cyclopean eye as
theorist's bookkeeping, not brain-synthesized, which would degrade the essay's Type-B isomorphism
to analogy). But home-field-UNSETTLED (Ono/Mapp/Howard counter-camp, 20+ yrs unconverged,
verified), so by the essay's own persistence-criterion the honest move is retype settled→live and
take persistence as support FROM the borrowed field. **Repo exposure corrected:**
`docs/cyclopean-point.md:12` typed binocular-vision facts as `bedrock`; added a marked grounding
correction (geometry stands; the *manufacture* claim the isomorphism rests on is home-field
contested, not bedrock) — annotation, not rewrite, matching the essay's own declaration
discipline. **Fork noted (not annotated):** `agent/analysis/originals/cyclopean-point.md` is a
variant copy (different header/subtitle) carrying the same 7 manufacture/isomorphism references;
left as analysis-input material (originals/ = sources to analyze, operator 2026-07-23), NOT
canonical — the canonical doc cited by `unknown_reading_review.md` + the perturbation principle is
`docs/cyclopean-point.md`. If the originals copy is ever promoted, carry the correction.

## 2026-07-22 — [landed] uke_write v2.1→v2.2 (Forecast Register) + uke_score v0.1 companion rubric + OQ-229 minted (pre-registered fragility-bias hypothesis)
**Files:** agent/uke_write_v2.2.md, agent/uke_score_v0.1.md, agent/analysis.py, quick_start.md, agent/manual/prompts.md, docs/technical/generator_emission_map.md, ISSUES.md, issues/INDEX.md
**Tier:** landed

Operator-relayed review feedback on the planned essay-archive grading pass (scoreability is
authored, not conferred by time; mechanism-ID and magnitude-estimation fail independently;
fragility-bias hypothesis + kill condition) was landed as: (1) **uke_write v2.2** — renamed
from v2.1 (single-file lineage convention; `git mv`), adding §1.6 Scoreable Prediction
Requirement, §6.1 machine-extractable `FORECAST REGISTER v1` YAML block (two-column
mechanism/magnitude pairs, absolute dates, named resolvers, `p_essay`+`p_baseline`,
fragility/stability direction tags), §5.7 Scoreability Gate, F-UNSCOREABLE-PREDICTION;
(2) **`agent/uke_score_v0.1.md`** — the register's consumer, a standard rubric a subsequent
model applies to the register block alone (producer+consumer in one change per Build
Discipline Pattern 1); (3) **OQ-229** — both arms (forward instrument landed-unwitnessed;
retrospective triage-then-grade pass not started) + the pre-registered hypothesis and kill
condition, recorded before any grading run. All 8 live `uke_write_v2.1.md` references updated
(analysis.py:4,80 hardcoded load path; quick_start; manual/prompts; generator_emission_map;
OQ-185's three citations — its `:173` scaffold-row line anchor verified still valid post-edit,
and its `grep suppression_requirement` empty-witness still returns 0). Audit-dir references
left historical. **Protocol change is under model-swap discipline: unwitnessed until the
first v2.2 essay run passes the §5.7 gate on real output** (OQ-229 graduation step).
Checks: issues_status 229/0 malformed, omega check 0 problems, index regenerated fresh.

Same-session extension (still v2.2 — landed unpushed/unwitnessed, so extended rather than
bumped): **§2.4 Tensegrity Architecture for the multi-seat essay** (from the operator's
stereo-pair essay design cycle) — seats as compression struts at measured strength (χ),
declared disparities with kill conditions as the tension net (§1.4 extended to disparities;
tensegrity-without-tension = both-sides filler), no privileged front (author's seat declared
as one strut), hydrostatic local commitment (declared/costed/temporary, emits a Forecast
Register pair when outcome-shaped). §2.3 scoped as the convergent mode with an explicit
mode-choice rule; §1.5.2 gauge-variant bullet routes to §2.4 (same-line edit — the
uke_write:173 scaffold-row anchor re-verified intact); new F-MANUFACTURED-CENTER
anti-pattern (seatless synthesis voice / fake tensegrity). Aligns with OQ-101
(plurality collapses by form) and the verdict-omits-seat finding.

Third same-session extension — **§2.4 absent-strut provenance rule** (consumer-found: the
writing model, applying §2.4 live on Draft 4, hit the gap that a seat nobody authored
renders at a χ magnitude as if measured — beneficiary seats inference-only, `powerful=0,
organized=0` — so the χ spread over present struts presents as a complete stereo picture).
Fix is the Build Discipline spine applied at the essay layer (carry the provenance bit with
the value; h1-null rule analog): per-strut provenance measured/inferred/absent;
flat-without-measured = UNDETERMINED (two indistinguishable causes: genuinely-flat vs
suppressed-below-hearing); absent struts named in-body; eye-selection decision attributed.
Plus F-ABSENT-STRUT anti-pattern + a §6 metadata "Strut provenance" line so the declaration
can't be silently skipped. uke_write:173 anchor re-verified.

Consumer round-trip refinement (same session, writing model's correction accepted): the
attribution clause was a defect as first written — "attribute the eye-selection decision"
invited laundering a method-side absence (the author's own sourcing limits) into a
world-side suppression claim, re-importing the verdict through the provenance rule. Amended
to method-level attribution only; world-side cause = a second undetermined one level up
(suppressed eye and unsourced eye present identically from the author's seat). §9 now also
scopes the rule plainly: representational fix, not epistemic (makes the writer say they
can't tell; doesn't let them tell). NOT added, deliberately: a rule against the
second-order no-seat pose (scrupulous UNDETERMINED-tagging as a reconstructed
view-from-nowhere) — the writing model flagged it as its own likeliest Draft-4 failure and
correctly called it writer-vigilance, not protocol; carried here as a Draft-4 REVIEW
watch-item instead of an over-promoted rule.

Sibling-protocol propagation (same session, operator request): **uke_think v1.1→v1.2** —
new §4.4 Multi-Seat Architecture (tensegrity import: positions as struts at argued
strength, kill-conditioned disparities, writer's position as one declared strut, declared
flattening w/ falsifier + Forecast Register pair when outcome-shaped) + position-provenance
rule (occupied/constructed/absent; silence = UNDETERMINED between no-objection and
never-occupied — §0's counterexample principle made structural) + §8 multi-seat gate +
F-MANUFACTURED-CENTER; **uke_opinion v1.3→v1.4** — tensegrity NOT imported (the verdict
form is the protocol's purpose); instead §1.5 "The Declared Flattening": collapse legit
only when indexed to a named seat where readings are position-indexed, falsifier attached,
consensus-absence ≠ assent (absent seat attributed to citation base, method level), plus a
routing rule (divergence-is-the-finding → UKE_THINK §4.4 / UKE_W §2.4); stray trailing
fences removed. Versions are in-file only (filenames unversioned, no code loads them);
essays' historical UKE_META stamps naming v1.1/v1.3 left as provenance records.

Second consumer-found correction (Draft 4 → Draft 5, operator-relayed): the writing model
put χ values in prose while claiming Mode B — §2.4's "rendered at its measured strength
(χ magnitude)" read as a Mode B exception. Closed in uke_write v2.2: χ calibrates prose
intensity, the number never appears; §5.5 checklist hardened (χ magnitudes named; "§2.4 is
not an exception to Mode B"). Plus three additions from the same failure: the **Rashomon
rule** in §2.4 (+ uke_think §4.4) — integration lives in the READER; seats are inhabitable
accounts, not exhibits; a reconciling fifth voice is the manufactured center returning;
depth-fusion (single recoverable answer) reserved for hydrostatic joints with a named
resolver — the stereo-pair geometry over-promised fusion where no fact-of-the-matter
exists; the **replicate-stability rule** (§1.5.2): run-stability/spreads are evidence about
the authoring (authored corpus + engine determinism), never about the world — the
essay-layer form of the report-scalars-are-not-measurement finding; and
**F-ENGINE-AS-TRUTH** (auditing the reports instead of writing the story the reports
pointed at). Register placement question answered from the existing §6 rule: trim from the
public copy at will, the ARCHIVED essay copy retains the register (single carrier — a
separate scoring file would fork the artifact, Pattern 2).

Third reviewer-found correction (Draft 6, second-Claude review, operator-relayed):
**snapping was ungoverned** — the Rashomon rule gives the writer authorship of every
account, so any account can be scripted to self-destruct and the collapse looks inevitable
rather than sound (witnessed: the grower seat's margin-claim silently swapped for a
refutable floor-claim, concession ventriloquized, under form-pressure to snap something).
uke_write §2.4 snapping rules landed (fired pre-stated kill condition only; engage the
seat's stated claim; holder-signs-whole-account test; zero snaps = complete; like joints
on the same missing resolver share status) + F-VENTRILOQUIZED-CONCESSION; the agency line
(discretion ≠ constraint; symmetric form must not launder operators into weather — the
"no one is responsible" view-from-nowhere via equal treatment); the shared-instrument
convergence rule (plurality as fingerprint of a missing resolver; per-instance attribution
of instrument retirements, no coordinator inferred from the pattern); "the architecture is
scaffolding too" (frame machinery invisible to the reader; excess frames dropped).
uke_think §4.4 mirrored compactly (earned snaps + agency line). Essay-side calls NOT made
here (operator/writer's): monopsony-commit vs hold-open on the grower joint — the
like-joints rule now forces consistency either way — and the measurement-destruction
reframe for Draft 7.

Claude-web ENGINE critique (emotives.md run) verified per agent-inventory discipline —
scorecard 1 confirmed / 1 refuted / 1 sharpened / 1 rerouted: (1) CONFIRMED missing
`golden_rule_consistency_reading` (mandated by the kernel's own decomposition note at
authority_vacuum_incommensurability.pl:262; both siblings dangle cs_reading_relation edges
to it) → **OQ-231** (P2; also records the REFUTED half so the fix isn't executed);
(2) REFUTED "flat control not registered to its kernel": flat_control_of/2 present
(…flat_control.pl:110), 17/17 corpus-wide, and cs_kernel_id-on-flat-controls is excluded by
the 2026-06-05 operator ruling; (3) SHARPENED "no source-provenance field":
`provenance.source_essay` EXISTS and is emitted (generate_constraint_pl.py:856) but reads
`'unspecified'` in all three stories despite the run being invoked with the source file as
argv — no filler, no consumer, no coverage field (source's self-declared skip at
emotives.md:7) → **OQ-230** (P3); (4) REROUTED "showing-face field unauthored": the
declared-vs-concealed instrument exists on the CS axis (cs_drift_ack_witness); topic-run
stories author 0 cs_* facts — OQ-223-class question, recorded in OQ-230 cross-refs, not
re-minted.

## 2026-07-20 — [landed] Kimi-k2.6 twin COMPLETE at n=1005; five-leg cross-model comparison
**Files:** prolog/testsets_kimi/, json_kimi/, prolog/beta_processed_kimi.txt, agent/run_no_scope_kimi.py, python/audits/five_leg_twin_comparison.py, audits/2026-07-20_five_leg_twin_comparison/
**Tier:** landed

The balance-blocked full run (below) was completed after a recharge landed. **Key operational
finding: batch tail latency is batch-SIZE dependent.** A 350-request batch stalled at ~332/350 for
hours (30 reqs stuck at +2/hr, rode toward the 24h window); **335/336-request batches completed
335/335 and 336/336 with NO stall.** So keep kimi batches ≤ ~335. **Cancel returns completed rows**
in the output file (output_file_id populates on cancel) — `--resume-batch <id> --n <same>` harvests
them with no regen; used once to recover 329 from the stalled 350-batch. Path to full n=1005: pilot
5 + harvest 329 + round-1 335 + round-2 336. Actual cost ~$0.043/story batch (round-1: $14.3/335);
balance drew ~$65 total from the ~$150 recharged pool. `testsets_kimi/` is now the FIFTH full leg.

**Five-leg comparison** (`audits/2026-07-20_five_leg_twin_comparison/`, all 5 legs classified at one
HEAD `9c226e8`): (1) **kimi-k2.6 is strikingly homogeneous — 63% of stories in H¹ band-3** (vs
26–34% others), N-invariant (63% at both n=334 and n=1005) — the sharpest single-model signature.
(2) **H¹ obstruction is overwhelmingly model-dependent**: across 957 shared seeds, all-4 twins agree
on h1_band only 14.5% (maxent type 35.1%) — empirical support for seat-indexed verdicts. (3)
**CORRECTION:** the partial-N (334) "kimi is cleanest, 0.3% red" claim was a first-334-seeds artifact
— at full N kimi red% = 2.7%, comparable to sonnet/haiku. Lesson: marginals over a non-random slice
mislead; paired agreement rates were N-stable. sonnet remains the type outlier (only tangled_rope >
snare leg, high piton).

---

## 2026-07-19 — [correction-key] Kimi batch WORKS on kimi-k2.6 (was model-gated, not account-gated); status_code==0 batch-extraction bug fixed; twin retargeted k2.6; 5 pilot landed
**Files:** agent/run_no_scope_kimi.py, prolog/testsets_kimi/, json_kimi/, prolog/beta_processed_kimi.txt, docs/technical/bulk_corpus_generation.md
**Tier:** correction-key

Resuming the Kimi twin after a machine restart (prior instance's `testsets_kimi/` etc. were empty
by design — the 5 pilot stories had been relocated to `testsets/`, 2026-07-18 entry below).
**Corrects two claims from 2026-07-18:**
1. **Batch is NOT account-blocked — it was MODEL-gated.** `POST /v1/batches` returns **200 on
   `kimi-k2.6`**; `kimi-k2.7-code` and `kimi-k3` 404 "resource_not_found". The 2026-07-18 "account-
   level block" tested only the two non-eligible models. Live-verified 2026-07-19 (our pilot batch
   `batch_6a5d1f28…` completed 5/5). `completion_window` must be an h-unit Go duration ("24h"; "1d"
   rejected). Twin **retargeted to `kimi-k2.6` --batch** (DEFAULT_MODEL).
2. **k2.6 is reasoning-HEAVY too**, NOT the "cheaper non-thinking / fairer twin" the k2.7-code note
   assumed. Measured k2.6 batch: **input ≈29.6k / output ≈15.5k tok/story, of which ~11.7k are
   reasoning tokens** (prompt caching fires, ~28.7k cached in). Stays a *thinking-model* twin.

**Bug fixed (was actively billing):** Moonshot's batch output rows carry `response.status_code == 0`
on SUCCESS (not 200), completion in `body`, null row-level `error`. The driver gated on
`status_code == 200`, so it **rejected all 5 valid results and auto-looped into a 2nd batch**
(cancelled before it billed inference). Fixed in `_batch_row_to_result` (gate on payload, not
status_code) — **do not reinstate a `== 200` check.** Added `--resume-batch <id>` (reprocess a
completed batch, no regeneration); used it to recover the already-paid pilot → **5/5 into
`testsets_kimi/`, classify_corpus GREEN on model kimi-k2.6, h1_band populated (2,3,3,3,5)**. Also
`_api_key()` now accepts `KIMI_API_KEY` (the .bashrc export) as well as `MOONSHOT_API_KEY`.

**Full-run attempt BLOCKED on account balance (2026-07-19).** With a spend-go, the full 1000-seed
run was launched. Two mechanical issues found + fixed: (a) Moonshot `/files` hard limit is **100 MB**
and each request inlines the ~139 KB prompt, so a 1000-request jsonl is ~143 MB → 400 "File size is
too large"; fixed by size-chunking `run_batch` into <90 MB batches (`_chunk_lines`; 1000 → 630+370;
commit d92b3cb7). (b) The 630-request batch then **failed on `failed_precondition: user has
insufficient balance`** — Moonshot reserves cost against `max_tokens` (32000), and the reservation
for 630 requests exceeds the account's **available_balance = $51.85** ($50 cash + $1.85 voucher). The
370-request batch cleared the reservation but was cancelled for a clean slate. **The full ~1000-story
k2.6 batch needs a recharge** (reservation ~$82–140 at max_tokens=32000; ACTUAL spend lower since
output ≈15.5k vs reserved 32k). Kimi leg stays at **n=5** until funded. Resume after recharge: one
clean `--batch` run (ladder skips the 5). Balance endpoint: `GET /v1/users/me/balance`. Runbook §7b.

---

## 2026-07-18 — [landed] Kimi (K3) twin driver built + 5-seed pilot PASSED; batch unprovisioned → sync-only; PAUSED pending batch enablement
**Files:** agent/run_no_scope_kimi.py, prolog/testsets_kimi/, json_kimi/, prolog/beta_processed_kimi.txt, docs/technical/bulk_corpus_generation.md
**Tier:** landed

New Moonshot/Kimi twin driver `agent/run_no_scope_kimi.py` (same Anthropic-result-shaped shim as
`run_no_scope_gemini.py`; reuses `build_cached_messages` + `process_batch_results` from the canonical
`agent/generate_kernel_corpus.py`; dest `testsets_kimi/` + `json_kimi/` + `beta_processed_kimi.txt`,
registry scoped to the kimi dir per runbook §6). **Pilot (5 seeds, sync) PASSED:** 5/5 valid `.pl`,
engine-load OK, `reading_relations` resolved, provenance stamped `kimi-k3` (five-defect fix intact),
0 rejections/failures. **Two findings (detail: runbook §7b):** (1) `kimi-k3` is REASONING-ONLY
(`supports_thinking_type:"only"`, effort only `["max"]`) — thinking can't be disabled, so this is a
*thinking-model* twin, asymmetric to the haiku/flash/sonnet twins (output ~16.5k tok/story). (2)
**batch-create is NOT provisioned on the staff/preview key** — file-upload + batch-list work, but a
fully valid `POST /v1/batches` 404s "resource_not_found" (endpoint/duration/file all verified valid),
so the full run is **sync-only at interactive rate, measured $0.289/story** (operator-confirmed
$1.44677/5 pilot), ≈ $291 for the 1005-seed pool vs ~$145 if batch is enabled. **PAUSED at 5 pilot
stories** (operator ruling: enable batch first). RESUME: `python3 -m agent.run_no_scope_kimi --seeds
prolog/kernels/rebuild_2026-06-13/never_generated_seeds.json --batch` once create works (ladder skips
the 5 done), or `--sync` now. Needs `MOONSHOT_API_KEY` in env (never repo). **Key hygiene:** the API
key was pasted in chat — operator should ROTATE it.

**Update (same day):** the 5 kimi-k3 pilot stories were **relocated into the live heterogeneous
`prolog/testsets/`** (+ json into `json/`; operator ruling — testsets/ tolerates mixed models), so
the LIVE corpus is now **150** (was 145; loads clean; local `pipeline_output.json` manifest stale at
145 until a `run_pipeline` folds them in). The twin (`testsets_kimi/`) is **retargeted to
`kimi-k2.7-code`** (the batch-eligible production model; DEFAULT_MODEL changed) and cleared to empty,
ladder reset. **Batch is STILL account-blocked even with k2.7-code** — POST /v1/batches 404s
identically across every model + completion_window, and Moonshot's own OpenAPI documents only
400/401/500 here, so the 404 is out-of-contract = account-level batch access, NOT model/request
(re-verified 2026-07-18). **k2.7-code cost finding:** 2-story sync sample → **output ~8.0k tok/story,
HALF of k3's 16.5k** (input ~29.6k, same) → k2.7-code sync is markedly cheaper than k3 sync; it is
also NOT reasoning-forced (a fairer twin than k3). Provenance sampling_params label corrected to
`reasoning=model_default` (we only set max_tokens). PENDING operator decision: full twin via
**sync-k2.7-code now** vs **wait for batch access**.

---

## 2026-07-18 — [correction-key] commitment-drift terminals are complete only under a SURVIVING-REFERENT precondition (OQ-227, from the *Hearts of Glass* fiction test-rig)
**Files:** prolog/cs_drift_engine.pl, ISSUES.md, docs/design/design_gaps.md, docs/deferential_realism_paper_v8.md, blog/2026-07/no-four-oclock-v8.md
**Tier:** correction-key

The six `cs_terminal_attractor/4` terminals (`stable_pattern`, `husk`, `extinction`, `revival`,
`repudiation`, `axiom_foreclosure`) each **presuppose a surviving referent** (a Temple to be hollow
about; a cosmology to foreclose; a text to depart from) — and all their examples are time-bounded.
Do NOT treat the six as exhaustive: deep-time **referent-dissolution** (the referent a commitment is
*about* decays while its internal form and grounding persist perfectly) is unhandled state-space.
OQ-227 (minted this session, `bundled_with OQ-153`) logs three candidates: **`sealed_closure`**
(commitment axis — warm, complete, "checks against nothing"), **`referent_dissolution`** (a new
`cs_drift_state/3` direction), and **`acknowledgment_collapse`** (standing axis — the ratifying
authority can no longer tell faith from its perfect performance; the terminal form of the
`design_gaps.md` self-consuming-standing trigger). **Load-bearing discriminator = recognizability,
not referent-presence:** `husk`/Kodashim is a *recognized* hollowing (the community knows the Temple
is gone); `sealed_closure` is unrecognizable by any observer incl. the keeper — the commitment-scale
instance of the essay's terminal fox (v8 §5.9). It is undetectable by construction (glues from every
synchronic angle → H¹=0, per the v8 §9.4 synchronic-invariant prohibition added same session; no
pre-closure snapshot), so **authored-only if ever built** — the drift analog of `h1_band=null`, same
shape as OQ-51's ruled-not-yet-built 4th sheaf value. Provenance: fiction as eon-scale test rig
(Claude-web review). Zero live-corpus cases — the additive path stays deferred; the recorded reasoning
is for a reviver. Also landed same session: v8 §9.4 H¹-synchronic citation prohibition; `design_gaps.md`
self-consuming-standing trigger; blog Part IV "The otherwise you can't erase" (temporal/Euclid repair).

---

## 2026-07-17 — [correction-key] blog essay "Everything Becomes Taste" → three-legs = trifurcation; forced-vs-chosen invariant; the synchronic mountain/naturalized read is WRONG
**Files:** docs/deferential_realism_paper_v8.md, docs/design/design_gaps.md, prolog/cs_drift_engine.pl, docs/debugging_philosophy.md
**Tier:** correction-key

Reading of `blog/2026-07/no-four-oclock-v8.md` (adaptive-preference / "is this preference *yours*")
against the engine, corrected mid-session by the operator. **The correction:** a first pass claimed
completed naturalization is indistinguishable from a genuine mountain — true **only synchronically**
(single time-slice), which is exactly the essay's own trap (its three dead probes each fix one leg
and vary within it). The engine's three legs are the paradox trifurcation of
`docs/debugging_philosophy.md`: observer/gauge = Type-C index, temporal/drift (`classify_at_time`,
`snapshot_type`, `drift_trajectory`, `cs_drift_engine`) = Type-A frame, axiom (`cs_axiom_engine`,
`axiom_foreclosure`) = Type-B structure. Naturalization is a **Type-A drift**, seeable by
frame-fixing (hold ε at t0) and tracing — the analytical/sub-specie position does exactly this.
**Operator's sharpening (Euclid):** temporal invariance is necessary-not-sufficient for mountain;
a *held choice* is invariant too (Euclidean geometry read as a mountain for 2000y; the parallel
postulate was a chosen axiom, demoted by a consistent otherwise, NOT by drift). Three-way partition,
each boundary cut by a different leg: **mountain** (forced — no beneficiary/no consistent negation,
honest no-seat pose) vs **declared choice** (Euclid/librarian — seated, otherwise live, acknowledged)
vs **naturalized foreclosure** (fox/Euclid-that-forgot — seated but posed-as-fact, unacknowledged).
mountain/choice is cut by the **beneficiary leg** (`false_natural_law`, a *structural* otherwise —
fires with no authored history at all); choice/foreclosure is cut ONLY by the **acknowledged bit**
(`cs_drift`: rescue-to-`stable_pattern` vs `husk`). Euclid's demotion = Oracle Gap (Theorem 4): a
site lacking the hyperbolic observer; widen the site → H¹>0. **The residual** (essay's real earned
limit) is choice-vs-foreclosure, and it is an **evidence-base limit not a meter limit**: the
acknowledged bit is authored, and the most-foreclosed positions author the least ("record density
tracks power"). Preference↔constraint mapping ruled **analogical only** (operator: "probably a
category error") — informs framing/docs, not asserted as a structural bridge; no H¹ experiment run.
Applied: v8 §5.9 (new), design_gaps.md GAP-01 reframe, cs_drift_engine.pl header cross-ref.

---

## 2026-07-16 — [correction-key] OQ-221 partition run: the merit-independent-signature law is DEPLOYMENT-RELATIVE (F1 — counting fires on earned external prose); OQ-221 mitigated, OQ-226 build queue minted
**Files:** agent/uke_narrative_orchestrator.py, ISSUES.md, audits/2026-07-16_oq221_meter_partition/
**Tier:** correction-key

The OQ-221 two-corpus partition (PREREG `a823cd47` + checkpoint ratification `ad132911` predate
every run; results `a6820230`) falsified the H1 earned-side prediction: `_numeric_inventory`'s
counting condition (≥10.0/1000) fired on **4/12 ratified earned texts** — ordinary earned prose
runs ~10–16/1000 number-words, and operator-approved **rift3 measured 46.04/1000, inside the
recorded defect band** (its vent-logging register IS the craft). Diagnosis witnessed: prediction
error, not instrument error (positive controls passed; fires are real tokens; threshold 10.0 was
variance-calibrated on pipeline output, never human-prose base rates). **Correction to how the
OQ-214 law may be cited: "counting is gateable" is true only relative to the pipeline's own
output distribution (defect band 37.6–50.6 vs improved ≤0.5) — never as a universal craft meter.
A gateable verdict must name its denominator.** Doc restatement for build_discipline.md /
design_discipline.md §11b proposed-and-flagged (operator ratifies), not applied. Partition:
rows 2/7/8 RULED reader-held; 3/4 UNSPECIFIABLE (P3 confirmed); 5 PROPOSED; 6/9/10
PROPOSED-capped (defect n=0 — notably SDZ has NO witnessed genuine misfire, earned 5/7 →
cross-note on OQ-127); 11 BLOCKED-ON-SEAT (OQ-185). Secondary witnessed gap: rev5's
operator-adjudicated earned word-arithmetic is percentage-form and EVADES `_WORD_ARITH_RE`
(recall narrower than the defect-class name) — extension queued in OQ-226 item 2. Floor claim
re-scoped: the defect roster contains only reader-noticed defects (R5 selection check: counting
was noticed before the meter existed).

---

## 2026-07-14 — logic_symbolic.md §IV reconciled to the ENGINE + gate-context drift guard (silent-fork + Stage-5-role fork resolved)
**Files:** agent/narrative_transform/logic_symbolic.md, agent/narrative_transform/logic_narrative_translation.md, agent/uke_narrative_orchestrator.py, python/check_logic_symbolic_drift.py, ISSUES.md
**Tier:** landed

Reconciled the narrative pipeline's constraint-logic references to `prolog/config.pl` +
`drl_core.pl:classify_from_metrics/6` (**the engine wins**, not `docs/logic*` — those are stale too;
see correction-key below). Two forks resolved:

- **Silent fork (Build Discipline Pattern 2).** `logic_symbolic.md §IV` hand-mirrored the gate
  thresholds and had drifted (Snare `χ>0.70` → real `χ≥0.66 ∧ ε≥0.46 ∧ Supp≥0.60`; Tangled
  `0.46≤χ≤0.70` → `0.35<χ≤0.90 ∧ ε≥0.30 ∧ Supp≥0.40`; Scaffold `χ≤0.35/theater≤0.40` →
  `χ≤0.45/theater≤0.70`; Piton fallback rewritten to `χ≤0.45 ∧ ε>0.10 ∧ theater≥0.70`; Naturalized
  `χ<0.40` → `χ<0.35`; **added** the dead-coordination piton pre-check `ε>0.10 ∧ theater≥0.70`; cascade
  relabelled the **metric** cascade, pre-signature-override). Anti-fork mechanism:
  `python/check_logic_symbolic_drift.py` derives its checklist from the `config:param` calls in
  `classify_from_metrics/6`, reads values from `config.pl`, asserts each on its §IV gate line
  (value-in-context; catches right-number-wrong-gate). GREEN (15 params) → RED on a wrong-gate swap
  (scaffold 0.45↔snare 0.66, presence-grep stays green) → GREEN reverted. **NOT wired into a pipeline
  gate** (operator say-so required). Deferral tracked: **OQ-222** (guard now, load-time injection on a
  churn reopen trigger). Guard scope stated: structural predicate gates + the hardcoded scaffold
  theater `TR>0.70` literal are numeric-unguardable by design (RECURSE-IF-REFACTORED note in-file).

- **Stage-5-role fork.** The doc header claimed §IV serves "verification (Stage 5)" and the
  orchestrator comment `uke_narrative_orchestrator.py:1471` said "stages 0, 1, 5" — both wrong.
  `STAGE_INPUTS["narrative"]` wires `dr_logic_symbolic` to **stage_0 (classification) + stage_1
  (formalization) only**; stage_5 is a narrative-critique Discovery pass (`["stage_4",
  "constraint_reports"]`) with no logic ref. Both corrected. Assembled-prompt probe confirms the
  corrected §IV reaches the stage-0 prompt in gate-line context.

- **Part B (committer/CS axis):** confirmed absent from the narrative pipeline (observer-axis only);
  recorded as **OQ-223**, held pending corpus graduation (committer dimension is a standing null vs 319
  omegas per `commitment_systems_sketch_v5_1.md`; only the has-beneficiaries bit graduated, already fed
  via `d`). No code.

## 2026-07-14 — [correction-key] docs/logic.md diverges from config.pl (OQ-37 doc-lag); logic_thresholds.md does NOT → OQ-224
**Files:** docs/logic.md
**Tier:** correction-key

Surfaced while reconciling `logic_symbolic.md` (OQ-222); do NOT reconcile the narrative docs *to*
these — the engine is the source of truth. **Witnessed (grep, not the plan's restatement — the plan
overstated):** the divergences are all in `docs/logic.md`, from OQ-37 Move 1 (2026-06-01,
`tangled_rope_chi_floor` 0.40→0.35) being only partially propagated: `:1695` param dump still `0.40`;
Naturalized `χ<0.40` (`:2077`/`:2083`, code `<0.35`); a quick-ref table `:2565` says piton `χ≤0.25`
while `logic.md`'s OWN prose (`:1966`/`:2012`/`:1995`) correctly says `0.45` (internally inconsistent).
**`docs/logic_thresholds.md` does NOT diverge** for these params (its table `:197` is correct with the
OQ-37 note); **no "Scaffold χ≤0.30" exists** in `logic.md` (its scaffold dump `:1893` is correct at
0.45). Not edited here (separate canonical surface). **Now ticketed: OQ-224** (bundled_with OQ-222).

## 2026-07-13 — OQ-214 Phase A LANDED: `_theme_inventory` theme-naming meter (mitigated; Phase B spend-gated)
**Files:** agent/uke_narrative_orchestrator.py, agent/narrative_transform/stage7.md, agent/narrative_transform/stage8.md, agent/narrative_transform/stage10.md, agent/uke_narrative_architecture_v0_3.md, python/tests/test_theme_inventory.py, audits/2026-07-13_oq214_theme_meter/, ISSUES.md
**Tier:** landed

`_theme_inventory(text)` built on the `_numeric_inventory` (OQ-215) template — the last waivable
absence-claim in the editorial audit layer (theme-naming) gets a deterministic backstop. Six kinds
split by the **bucket rule** (density-bearing ⟺ flagging it in rift3.md is NOT a false positive):
**density-bearing** = `anaphora` (≥2 consecutive sentences sharing a ≥3-word initial phrase),
`causal_chain` (stacked/near because-therefore formulas); **adjudication-only** = `refrain`,
`aphorism`, `resonant_closer`, `word_arithmetic`. `density_per_1000` uses the two density-bearing
kinds ONLY; the full list is injected into stages 7/8; the post-stage-8 gate escalates OPEN, **never
auto-rejects** (protected INVARIANT + kill condition written verbatim in-source; refrain-doesn't-gate
locked by `python/tests/test_theme_inventory.py`).

**The finding is a NULL, and the null IS the deliverable (operator ruling — see WRITEUP.md).** The
density-bearing kinds do NOT separate the OQ-218 defect from its v0.2 fix. Runs 1&2 SEED vs IMPROVED
have IDENTICAL anaphora/causal counts (18/10, 14/5) — the improvement lived entirely in refrain
(40→20), the kind the ruling forbids gating on. Earned-dense rift3 = 5.12 outscores two of three
SEED defects (3.64, 3.31). The gateable axis is orthogonal to the defect. `THEME_DENSITY_THRESHOLD =
8.0` sits above everything good and will essentially never fire — correct for a gate that cannot
discriminate and would damage craft if it tried.

**GENERAL LAW (correction-key, learned twice): a defect is mechanically gateable IFF it has a
merit-INDEPENDENT signature.** Counting had one (a digit means the same in a defect and a
masterpiece) → the meter worked and could gate. Theme-naming does NOT (its surface IS the surface
earned prose uses on purpose) → the meter cannot gate and collapses to a candidate list under
adjudication. Not a difference of meter quality. **The bucket rule turned a false success into a
true null:** had refrain stayed on the gate, calibration would have LOOKED successful (defect-high,
fix-low) while suppressing rift3's creed and every earned refrain. **Honest close: the last waivable
absence-claim is now a mechanical candidate list, but explanation-over-run is NOT metered and cannot
be by this approach — the register problem stays reader-held (the Q2 double-No), unreachable by
regex. The meter's existence does not move it.**

**Two design claims RULED (operator, 2026-07-13):** (1) **Mechanization floor — provisionally yes,
with falsifier:** *no remaining KNOWN defect has a merit-independent signature; the falsifier is
finding one that does* — if it surfaces the meter approach revives for it and the architecture
extends (same shape as every threshold ruling: provisional, reopens). (2) **Assisted posture —
PERMANENT architecture, by the structure of the problem** (not a maturity stage): the mechanization
boundary runs along merit-correlation; above it is instrumented (counting/invariant/break-address,
R1–R14), below it is definitionally reader-held (arm-2 D9 model-judge rubber-stamped the negative
control, only the reader discriminated; the Q2 double-No). So "improve the pipeline" now means
improve the READERS in the loop, not build more meters — the cold human read is the first instance
of the actual remaining work, not a gate before resuming it. Repo-wide test of the law = **OQ-221**
(does the meterable-vs-reader-held partition apply beyond this defect).

**Tripwire (promotion candidate → judged history):** a future optimizer must NOT arm the theme gate
on refrain/aphorism/closer/word_arithmetic "for determinism" — that turns the meter into a
craft-suppressor (the hard-ban failure in a third costume). This is enforced in-substrate three ways
(in-source invariant comment, `THEME_CAVEAT` rendered at every read site, the regression test), so it
is a LOUD failure, not a silent one → stays history, not promoted to CLAUDE.md.

**Graduation — human read FIRST, may MOOT Phase B:** the cold human read answers whether the
register problem is even the right target (if the improved stories read machine-made at the sentence
level, the meter treated a symptom). Do the read; THEN decide — one Phase-B run (engine-change
witness; low-information now the gate barely fires) OR close OQ-214 as "candidate list shipped;
explanation-over-run confirmed un-meterable; register work reassigned to the read layer." No tokens
spent yet.

## 2026-07-13 — OQ-219 RESOLVED: Stage-2 dominance clause implemented + validated (routing outcome a); no v0.3
**Files:** ISSUES.md, agent/narrative_transform/stage0.md, agent/uke_narrative_orchestrator.py, python/tests/test_stage2_dominance_gate.py, agent/uke_story_v0.2.md, audits/2026-07-13_oq219_missing_floor/READOUT_dominance_clause.md, audits/2026-07-13_oq219_missing_floor/READOUT_datum_stone.md
**Tier:** landed

**Clause implemented + validated (commits `83ecf045` impl+fixture, + clause run/READOUT).** stage0.md
authors `primary="yes/no"` on the invariant contract (at most one invariant primary; never inferred
downstream). Orchestrator `_contract_marks_floor_primary` / `_stage2_dominance_suffix` inject the
dominance-ordering clause into the Stage-2 prompt IFF `missing_floor present="yes" primary="yes"` —
STRUCTURAL gate (R3(b) third application), behavior-preserving on all committed contracts (none carry
`primary=` → INERT → identical prompt; output-changing only on newly-flagged floor-primary sources).
Free negative-control fixture `python/tests/test_stage2_dominance_gate.py` 5/5 PASS (over-fire on
grain-primary structurally impossible — the hard-ban mistake relocated, guarded by a fixture not a
run). Paired re-run (control = no-clause Datum Stone `a02246f7`) met all pre-registered criteria:
subordination beat nameable, cold recovery 3/3 (Sonnet+Gemini+Haiku, same arms held constant) ≥ 2.5/3
baseline, **Haiku rescued partial→full floor** (predicted sensitive indicator), grain preserved (kill
condition unmet). Confound noted: clause run = different stochastic surface (determinism frontier
forbids same-story on/off); clean signals = subordination beat + Haiku rescue + grain-preservation +
fixture. NOTE the OQ-216 intermittent stage-2 SECTION-0 guard fired on the first clause draw (also
witnessed pre-clause) and cleared on re-draw — not clause-induced. Below is the mitigation-stage record.

**Plan-file "wobbly-torvalds" typed VOID (not a deferred task).** The plan filename
`review-oq-219-and-present-wobbly-torvalds.md` carries no second task — its 21KB body is OQ-219
only. `wobbly-<name>` is the operator's plan-slug naming convention (cf. sibling
`~/.claude/plans/can-you-review-oq-91-wobbly-cray.md`), not a "present X" deliverable. The plan is
fully discharged by this resolution; no open item remains from it.

## 2026-07-13 — OQ-219 (mitigation record): floor-recovery tracks dominance (routing outcome a)
**Files:** ISSUES.md, agent/narrative_transform/stage2.md, audits/2026-07-13_oq219_missing_floor/READOUT_datum_stone.md, audits/2026-07-13_oq219_missing_floor/READOUT_v02repair.md
**Tier:** landed

Commits: pilot `86c36f29`/`30120d32`, routing pre-commit `354ef198`, resolution (this). Reframed
OQ-219 (v0.2-repair: can v0.2 dramatize a contract-only floor?) resolved via TWO runs. **Pilot**
(Margins, grain-primary, contract-only floor): v0.2 CAN dramatize the floor (existence) but FRAGILE
— §6 cold arms split 1/2 (Gemini floor, Sonnet grain). Cause unassignable at n=1. **Isolating run**
(Datum Stone, floor-PRIMARY, ~$1.65, "The Long Breath"): cold floor-recovery ≈2.5/3, and **Sonnet
FLIPPED** grain→floor with dominance. **Floor-recovery TRACKS DOMINANCE** → the fragility is
**dual-grain competition, NOT a missing protocol socket** → pre-committed **routing outcome (a): NO
v0.3.** Fix = **seed-side Stage-2 dominance-ordering clause** (when the contract marks the floor
primary, Stage 2 subordinates the grain on-screen — the §1a two-reals machinery applied to
floor-vs-grain); implementation-pending, gated on operator go for a generation-protocol change.
**Standing taxonomy ruling (operator):** the floor is the grain's **structural sibling, not a
break-species** (presupposition vs unreadability; fairer-authority vs better-instrument falsifier;
instrument's-honest-operation vs character's-knowing carrier) — the §1b break-rider was the correct
vehicle, only the §1 structural home is debated, and outcome (a) says it's not even missing.
**Label-delta method (reusable):** record primed (stage-9, contract-threaded) vs cold (§6 blind)
floor-recovery; cold recovery is presuppositional so it runs BELOW grain-recovery and is
measured-and-reported, not required, in the load-bearing bar. Not a tripwire (loud; the reframe +
fix live in the OQ-219 entry).

## 2026-07-13 — OQ-219 REFRAMED: UKE originals corpus is architected dual-grain — no corpus-drawn pure-Detector-B source
**Files:** ISSUES.md, audits/2026-07-13_oq219_missing_floor/PROPOSAL.md, audits/2026-07-13_oq219_missing_floor/TRIAGE.md, agent/narrative_transform/originals/
**Tier:** landed

Commits `90631e4e` (pre-registration) + `6ff9f480` (Step-0 triage + raw artifacts). OQ-219 asked
whether R14's floor-contract (Detector-B "missing floor") is LOAD-BEARING in output, needing a
**pure-Detector-B leg B** as the naming probe's POSITIVE CONTROL (without it a dual-grain leg-A
null is uninterpretable, sharpening #2). **Finding: no corpus-drawn pure-B source exists.** Prose
pre-screen across 11 `originals/` sources + Stage-0 `--dry-run` engine certs on the two best shots
(`rift3` "Insufficient", `rift2` "Load-Bearing") ALL certify `untranslatable_real present="yes"`
alongside `missing_floor present="yes"`. **The UKE originals corpus is architected dual-grain by
construction** — every source authored for the detector schema pairs a Detector-B codifying
instrument with a live Detector-A untranslatable-real (the datum/table/book/commentaries/baseline
vs the walking/boat-grammar/live-judgment/palm-to-stone), and the untranslatable-real is typically
dominant. This is almost certainly WHY every OQ-215 arm-3 source led with a Detector-A grain.
**Operator ruled (this session): corpus-drawn only, no authoring → reframe.** R14's floor is a
**co-presence grain** here (never authored standalone-primary); the graded existence-run is retired
and OQ-219 is reframed to its own alternative — the **v0.2-repair** question (can UKE_STORY v0.2
dramatize a contract-only floor?), still spend-gated. Triage spend ~$0.06 (2 Stage-0 dry-runs,
gemini-2.5-pro, exit 0). Not a tripwire (loud: any future pure-B hunt re-runs the same triage);
the reframe lives in the OQ-219 entry.

## 2026-07-13 — OQ-217 RESOLVED: consensus_provenance/2 real-seat verdicts; verdict⟺H¹ now an EXACT biconditional
**Files:** prolog/stakeholder_seats.pl, prolog/commentary_census.pl, prolog/tests/test_h1_stakeholder_spectrum.pl, prolog/tests/test_seat_totality.pl, python/audits/oq207_stakeholder_h1_census.py, python/audits/oq217_movement_diff.py
**Tier:** landed

Commits `871e69ac` (tightening, output-changing) + `cb60bd0a` (movement census). Operator ruled
option 3 (filter `unknown` from verdict computation everywhere + annotated unanimity; rationale
at the clause header: unanimity is universal — untypeable seats weaken it, so the caveat rides in
the TOKEN; plurality is existential — unknowns can't undermine it) + the
`manufactured_consensus_candidate_untypeable/1` extension (ruled in session, distinct from the
2026-07-11 D4 ruling). Verdict set grew by three tokens: `insufficient_real_seats` (<2 real-typed
seats — absorbs retired divergence cells (a)/(b) and the 1-real mixed cell),
`unanimous_with_untypeable_seats`, `manufactured_consensus_candidate_untypeable/1`.
`consensus_bucket/2` gained the three rows (insufficient = MEASURED bucket, not absence —
declared choice, revisable). Witnesses: plunit 37/37 + 19/19 + 20/20; census PASS on 4 legs
(retired cells (a)/(b)/mixed = 0 via KEPT-LIVE detectors; all controls fire); per-id movement
diff == the pre-derived prediction, 0 mismatches (4 cell-(b) + 1 cell-(a) stories →
insufficiency; full named table `audits/2026-07-12_oq217_consensus_tightening/README.md`);
pipeline per_constraint byte-identical (same-session clean-vs-edited runs, both exit 0).

Two correction-grade riders: (1) `mcc_untypeable` is HEAVILY LIVE (12/50/39 across
testsets/haiku/flash), NOT the predicted-zero cell — any OQ-204-era consumer reading only the
bare `manufactured_consensus_candidate` token silently drops the larger untypeable stratum
(constraint recorded in OQ-204). (2) The v1 movement prediction was under-determined because the
pre-OQ-217 census dump AUTHORED `n_excluded: 0` for non-mcc verdicts instead of measuring it
(Pattern 5, caught by the diff comparator on real data — `movement_diff.v1_flagged.json`); the
dump now measures `n_excluded` per record by direct fact query.

---

## 2026-07-13 — OQ-218 RESOLVED (operator ruling): rev6 = variance not class; R3(b) STANDING; watch closed w/ travelling reopen
**Files:** ISSUES.md, audits/2026-07-12_oq218_scored_snare/READOUT_STAGE2.md, agent/uke_story_v0.2.md, docs/the_taught_hole.md
**Tier:** landed

Ruling filed VERBATIM in OQ-218 (D9 precedent) + mirrored in the READOUT appendix. Stage 1:
repair confirmed both legs; v0.2 Ω_E1 resolved (possibility, not guarantee) — propagated into
the protocol file. Class: variance not class (1 weak in 8, ending-saved), scoped to
certified-grain sources; R3(b) conditional → STANDING (hard-ban fallback documented, not armed);
reopen condition travels (one Type-B seed post-re-baseline = re-arm; two in five = reopen).
Type-A over-run formally = OQ-214's mandate. NOT ruled: v0.2 §5 untested (Ω_E2 open); register
ceiling; human gold arm gates PUBLICATION (Clean Small Song first) not the close; improved
stories are audit artifacts (rev6-improved carries the Cliffside 806-812 flag). OQ-220
pre-registration hardened before spend: cold-recovery AND delta as separate measurements
(closes the absent-pole alias) + one 4.5-pinned rerun as the model-confound disambiguator.
Taught-hole doc corrected: transmission-through-the-protocol, not resonance.

## 2026-07-13 — OQ-218 sweep witness + OQ-214 promoted + stage-2 SECTION-0 guard (first Sonnet-5 production run fired OQ-216's candidate)
**Files:** ISSUES.md, agent/uke_narrative_orchestrator.py, audits/2026-07-12_oq218_scored_snare/READOUT_STAGE2.md
**Tier:** landed

- **Absence claim discharged:** correctable-vocabulary sweep over the three Stage-2 seeds with
  rev6 as positive control (fired: 16 hits, the rebuild subplot recovered exactly); seeds
  0 / 1-homograph / 0. READOUT addendum carries the paste. Operator per-source leg staged as
  `OPERATOR_LEG_BUNDLE.md` (six files, one upload).
- **OQ-214 PROMOTED (Priority 4→2)** with the witnessed calibration corpus: 3/3 Type-A seed
  instances (manifest anchors), false-positive class (rev5 + run-1 earned lines), Sonnet-r3's
  blind pattern enumeration, and the resonant-closer tic (operator Web-Claude read of the
  ergodicity story: units landing on "the way X" images, ×4 in 2,280 words). Residue-inversion
  design warning recorded: Q2 nominates, register gate vetoes, neither rules alone.
- **First Sonnet-5-default production run** (`112_ergodocity_kids_1783916200`, operator-invoked):
  ran end-to-end (migration held; density point 4.84/1000), but **stage 2 omitted SECTION 0**
  (folded into SECTION 1 "Step 0") — OQ-216's stage-2 census candidate fired in production; R13
  threading ran dead behind a warning. Guard built (warn → fail-loud StepResult error), two-sided
  witness: fires on the ergodocity stage_2, passes on all three batch stage_2s. OQ-216 updated.
- Standing instrument rules recorded on OQ-218: blind payloads get a "these may be identical —
  say so" escape; arm factual claims get grep-adjudicated before filing. Ruling-scope line
  recorded (certified-grain sources; watch reopens lightly at the Sonnet-5 re-baseline).
  OQ-219 remains gated on its own spend-go.
- **OQ-220 minted (label-delta blind read; math_stories stress-test class, 5 sources, spend
  held):** the operator's Web-Claude read of the ergodicity story REVERSED in a differentiated
  way when handed the word "ergodicity" (retraction/sharpening/intact) — reader-seat-indexing at
  the vocabulary level. Design: cold read then concept-primed re-read; the delta measures how
  much of a formal concept survives naturalization without its label. The sharpened register
  finding (an ergodic prose voice metabolizing a non-ergodic subject back into pattern) recorded
  on OQ-214 — the resonant-closer class can be subject-interfering, not merely mannered.
  math_stories Zone.Identifier sidecars removed (same artifact class as the agent/ pair).
- **OQ-220 pilot artifact 2:** operator had Claude web run UKE_STORY v0.2 on the ergodicity
  story WITH the label → `blog/2026-07/the_clean_small_song.md` (committed). It exercised the
  protocol's reject-and-regenerate branch (first time anywhere) and re-translated the concept
  one layer deeper: the ergodic hypothesis + non-stationary collapse (fast convergence as danger
  signal; absence satisfying the measurement) — Build Discipline Patterns 5/6 as fiction, with
  the taught "hole" as carry-the-provenance-bit. Recorded on OQ-220 with honest costs
  (single-voice; closer tic persists at the final image).
- **OQ-220 pilot datum 3 (Perplexity, two-stage on the rewrite): both poles of the label-delta
  scale witnessed.** Cold read recovered the concept structurally without the word ("convergence
  condition indistinguishable from impoverishment"; "loss function has silently changed");
  label produced sharpening-only (no retraction) — vs the seed pilot's reorganization. Delta
  magnitude tracked dramatization as pre-registered. Confound stated (different readers,
  different stories; both operator-adjacent). Convergent editorial datum: the unprimed reader
  independently requested the error-vs-incommensurability sharpening = §1a/F-CORRECTABLE-REAL
  from outside the protocol's vocabulary.
- **`docs/the_taught_hole.md` written (operator request):** the story read back as method for a
  general audience — coming-to-true = convergence-is-not-adequacy (Pattern 5/6), any-teller-
  any-route = consensus-needs-positive-controls, three reefs = seat-indexed verdicts, Lo = the
  fresh instance (KNOWN_STATE/tripwire rationale), the taught hole = carry-the-provenance-bit
  (nullable h1_band as "a held rest with a schema"). Standalone; v8-note-or-standalone left as
  the operator's editorial call, stated in the doc.

## 2026-07-12 — OQ-218 Stage 2 batch RUN (3 sources, 4.5-pinned): rev6 weakness 0/3; residual defect is Type-A register-level
**Files:** ISSUES.md, audits/2026-07-12_oq218_scored_snare/READOUT_STAGE2.md, agent/uke_narrative_orchestrator.py
**Tier:** landed

Stage-1 ruling (repair confirmed, both legs) opened the gate; batch ran same day, all Anthropic
stages pinned to `claude-sonnet-4-5-20250929` per operator instruction (R12 confound dissolved).
Full detail: `READOUT_STAGE2.md` + per-run manifests/arms in the audit dir. Headlines: (1) the
rev6 Type-B scored-Snare weakness appeared on **0/3** fresh sources — all seeds HOLDS on
structural grounds mid-story; maps to pre-registered outcome 3, ruling pending (operator's
seat); (2) all three v0.2 triages = **Type A explanation over-run** — the pipeline's residual
template signature is register-level (blind Sonnet r3 enumerated the house patterns), the
OQ-214 meter's target class, now witnessed 3/3; (3) both addressed deformations named blind by
both arms; Q4 re-confirmed as THE discrimination instrument; (4) subtraction-only deltas sit
below Gemini's detection floor — it confabulated a difference (grep-witnessed false claim),
so near-identical A/B pairs need an "identical? say so" escape or strong-arm-only Q4;
(5) residue-inversion: cold readers Q2-pick F39 framework residue as inimitable — Q2 alone
must never adjudicate keeps; (6) one OQ-216 cap-hit fire (stage_3 12288, run-2 attempt 1) —
guard fail-loud worked, cap → 16384 (`25b27343`), loud retry clean.

## 2026-07-12 — Sonnet 4.5 → Sonnet 5 across the three agent entry points; sampling params gated per model
**Files:** agent/llm_call.py, agent/c-orchestrator.py, agent/generate_kernel_corpus.py, agent/uke_narrative_orchestrator.py
**Tier:** landed

Operator request. `claude-sonnet-4-5-20250929` → `claude-sonnet-5` (exact ID, no date suffix) in
c-orchestrator architect, generate_kernel_corpus SCOPE_MODEL, and all ten uke stage models
(Haiku researcher/GEN_MODEL and Gemini stage-0 unchanged). Sonnet 5 rejects non-default
`temperature` (400) and runs ADAPTIVE thinking when the field is omitted (would spend the
calibrated per-stage max_tokens caps on thinking) — new `llm_call.sampling_overrides(model,
temperature)` gates both per model (Sonnet 5: drop temperature + pin `thinking: disabled`;
Opus 4.7+/Fable: drop temperature; legacy: unchanged), consumed by llm_call.call, the
generate_kernel_corpus single-call AND batch-wave paths, and duplicated locally in the uke
AnthropicProvider (self-contained module). `MODEL_CONTEXT_WINDOW` gains claude-sonnet-5 = 1M
(old entries kept — still served). Witness: py_compile clean; live `OK` round-trip on all three
call paths with claude-sonnet-5 at non-default temperature (no 400). OPEN/[TUNE]: Sonnet 5's
new tokenizer runs ~30% more tokens for the same text — per-stage max_tokens caps and the
0.48–47.6 density baselines were calibrated on Sonnet 4.5 output; the cap-hit guard fails loud
if a cap binds, and the next pipeline run is the re-baseline.

## 2026-07-12 — OQ-215 CLOSED on operator read; posture ruled assisted-by-design; OQ-218/OQ-219 minted (spend HELD); probe sources staged + Stage-0 certified
**Files:** ISSUES.md, agent/uke_narrative_orchestrator.py, agent/uke_narrative_architecture_v0_3.md, agent/uke_story_v0.1.md, agent/uke_story_v0.2.md, docs/design/design_discipline.md, agent/narrative_transform/originals/the_good_name_book.md, agent/narrative_transform/originals/the_eighth_commentary.md, agent/narrative_transform/originals/the_table_of_winters.md, agent/narrative_transform/originals/the_datum_stone.md
**Tier:** landed

- **OQ-215 resolved** (commit `0e353a24`): no kill fired; counting dissolved with R2 live;
  invariant 4/5 strong + rev6 partial-via-ending; flinch withdrawn; D9-adversarial conforming.
  **Run↔rev correction:** arm-3 runs are rev3–rev7 (rev6 = "The Platform Knows" = run 4,
  `the_empty_pan_1783872143`); rev2 is arm 1. **rev6 stage-9 HOLDS hand-checked GENUINE** —
  the correctable-reading pressure (Kiran's representative-sample rebuild) is foreclosed by the
  ending (Sokol's compliance reframe; "changes nothing"). Compressed entry keeps the operative
  rulings (D9 composed fixes + HOLDS-guard; Forty-Hertz partial; rift3 class; R3(b) trail).
  OQ-214 gained the rev5 EARNED word-arithmetic calibration datum.
- **Carried flag wired** (commit `e96b2bf3`): `DENSITY_CAVEAT` ("density measures counting only;
  invariant survival is adjudicated by blind stage-9 + operator read; 0.0 is not evidence the
  invariant held") now renders in the sidecar JSON, the inventory prompt block, and a new
  always-emitted numeric_gate summary line (pass path was silent before). Also in the
  architecture doc, which previously had no density-gate section at all.
- **Posture ruled assisted-by-design** (commit `9d08165f`): design_discipline.md §11 — the
  sharpening judgment is operator-held BY NECESSITY (arm-2 witness: stage-10 D9 scored the
  negative control 5/5); autonomous is structurally foreclosed, not deferred. `--edit FILE`
  documented as the first-class assisted mode. **Protocol files renamed** `uke_resleeve_v0.*.md`
  → `agent/uke_story_v0.1.md` (superseded draft) / `agent/uke_story_v0.2.md` (current): the
  pipeline is the TRANSLATION instrument, UKE_STORY is the IMPROVEMENT protocol — no translation
  protocol file exists; v0.2 footer + v0.1 Ω-NAME note corrected; Zone.Identifier sidecars deleted.
- **OQ-218/OQ-219 minted, spend HELD** (commit `b59ec941`): scored-Snare reframed to "can
  UKE_STORY v0.2 repair a Type-B seed?" (rev6 = the gating Ω_E1 control; fresh sources are a
  conditional Stage 2; executor separation binding — improver ≠ blind reader); missing-floor
  probe with "load-bearing" pre-registered. Both `blocked_on_human operator-spend-go`.
- **OQ-218 Stage 1 RUN (spend-go granted same day; operator concurred with the Phase-0
  hand-check at closer range).** Pre-registration committed BEFORE the pass
  (`audits/2026-07-12_oq218_scored_snare/PROPOSAL.md`, commit `b2c2c542`). Improvement executed
  by this instance under v0.2 Path B: platform-record seat (4 interstitials; the §III fail-count
  moved into the instrument's own grammar — the stage-9 weakness repaired with the seed's device),
  grain scene drafted in isolation (sensor crossing: the trace records the preparation, cannot
  record that it was preparation), falsifier granted with indifference (notebook: "reads her
  exactly as deep as the cruel one did"), stage-10 flinch line cut, consolation level 1 (terminal
  beat = intake record scheduling the daughter). **Paused pre-§6 per executor separation** —
  manifest carries blind_read PENDING; unlabeled randomized A/B payload staged for two fresh
  arms of different model families (+ human gold arm; Q3 delayed; Q4 added). Adjudication:
  blind arms test the break; operator takes the contaminated §1a audit; operator rules.
  Also landed: `[EDGE]` convention added to CLAUDE.md (operator instruction).
- **Four probe sources authored + certified** (commit `434ec74d`, ~$0.12 total dry-run spend):
  three scored-Snare in distinct instrument classes (credit standing / examination / actuarial
  table) + one Detector-B-primary. All four Stage-0 dry-runs witnessed: `inherent_instrument
  value="yes"` (the snares), `missing_floor present="yes"` (datum stone, primary),
  `untranslatable_real present="yes"` on ALL FOUR, break contracts authored. **Full probe runs
  did not run** — held for operator spend-go, against the seed→UKE_STORY chain.

## 2026-07-12 — Break-contract threading landed: stage 0 authors the break's ADDRESS; carried to stages 2/9/10 (rides R13/R14 plumbing)
**Files:** agent/uke_narrative_orchestrator.py, agent/narrative_transform/stage0.md, agent/narrative_transform/stage2.md, agent/narrative_transform/stage9.md, agent/narrative_transform/stage10.md, python/tests/test_break_contract_threading.py
**Tier:** landed

Posture ruling (operator, 2026-07-12): the pipeline AUTHORS the break's address;
EXECUTION belongs to the story stages downstream. No break-execution instructions in
stage4; no auto-reject on break-absence anywhere.

- **stage0.md B6a:** source-sighted `<break_contract>` — `original_break` (expectation
  the source violated for its first readers), `prior_status` (LIVE/DEAD), `target_prior`
  (if DEAD, nearest living expectation the resleeve must violate; if LIVE, restated in
  current terms). All surface-free; same phrasing test as the invariant contract.
- **Orchestrator:** `_extract_stage0_break_contract` + `_STAGE0_BREAK_RE`; anonymized
  via `_anonymize_stage_1` (air-gap backstop) and saved as sidecar `break_contract`;
  cache-restore recomputes from a cached stage_0 when the sidecar predates threading.
  STAGE_INPUTS: stage_9 blind assert is NOW
  `["stage_8","invariant_contract","break_contract"]` (supersedes the two-element list
  cited in the 2026-07-11 R1–R14 entry); stage_10 gains the key; stage_2 receives it
  via `_run_stage_2(…, stage0_break)`. Generic runner has a NOT AVAILABLE fallback
  (break presence UNVERIFIED, never N/A). `_S9_FALSIFIER_RE` lookahead now includes
  BREAK — without it the new stage-9 BREAK section is swallowed into the D9 payload.
- **stage2.md:** one affordance-gate line — reject a naturalization whose substrate
  FORECLOSES the target_prior violation (world leaves the break executable; need not
  execute it). **stage9.md:** BREAK NAMING as a reader (name from text FIRST, then
  compare to target_prior); non-naming is a FINDING, not a failure. **stage10.md:**
  D10 Break Presence — informational only: reported, never summed, exempt from every
  override including the =1 rule.
- **Witnesses:** `python/tests/test_break_contract_threading.py` 5/5 PASS (extraction
  ± negative control; anonymization surface-free with two-sided map-driven control;
  STAGE_INPUTS threading; falsifier-stops-before-BREAK). Dry-run Stage 0 on
  the-empty-pan.md: `<break_contract>` authored, prior_status ruled DEAD, zero
  source-vocabulary hits in the block (probe positive control: same sweep hits
  Verrel/King/seal in the source-sighted remainder of the output).

## 2026-07-12 — OQ-207 RESOLVED: stakeholder-frame H¹ built, emitted, censused; D4 kill condition FIRED → OQ-217 minted; TWO ABSENCE TOKENS tripwire
**Files:** prolog/stakeholder_seats.pl, prolog/reading_registry.pl, prolog/tests/test_h1_stakeholder_spectrum.pl, prolog/json_report.pl, python/shared/schemas.py, python/audits/oq207_stakeholder_h1_census.py, docs/h1_gap_spectrum_general_n.md
**Tier:** tripwire

**TRIPWIRE — two absence tokens coexist BY DESIGN in the per-seat type surfaces; never
unify them.** `untyped` is CENSUS-FACING (`seat_perceived_vs_real/4`'s Computed when the
per-seat derivation fails); `unknown` is KERNEL-FACING (`stakeholder_type_vector/2`'s
token for the same failure, and the literal `dr_type_with_d/4` fallback type). The
kernel's OQ-51 filter `is_real_type/1` tests `\== unknown` ONLY — an `untyped` leaking
into the H¹ vector is counted as a REAL DISAGREEING TYPE and silently inflates
`h1_stakeholder`. Conversely a reader "normalizing" the vector's `unknown` to `untyped`
breaks the null rule. Positive control on the actual failure path:
`test_h1_stakeholder_spectrum.pl` `no_untyped_in_vector`.

Landed (commits `8048a568`, `96047f19`, `cbd44d19` + docs): `stakeholder_obstruction/5`
(memoized; cache_registry hook; coverage in-band; domain = `stakeholder_agent_seats/2`
extracted from `consensus_provenance/2` — no fork), three OQ-137 registrations,
per-constraint `h1_stakeholder`/`_n_seats`/`_n_real` (null = UNDETERMINED, never 0 —
same OQ-51 read rule as `h1_band`), schemas.py contract + consistency check, census
`audits/2026-07-12_oq207_stakeholder_h1/`: 0 spectrum violations / 1,316 numbered H¹s,
kernel_v1 all-null PASS, planted-violation FLAGGED, zero-seat = OQ-202 mint exactly.
**Cell (b) live population 4 → the pre-committed tightening is now OBLIGATORY →
OQ-217** (also scopes the newly-pinned mixed `plural([T,unknown])` cell, 19/66/129
live). D4 case table lives at the `consensus_provenance/2` clause header + the plunit
`coherence_case/5` — OQ-217 must update BOTH in its commit or the suite goes red.

---

## 2026-07-12 — OQ-215 arm 3 COMPLETE: 5/5 variance runs, R2 live for the first time, composed D9 conforming, invariant HOLDS 5/5; threshold recalibrated 25→10
**Files:** agent/uke_narrative_orchestrator.py, python/audits/oq215_arm3_variance.py, audits/2026-07-12_oq215_arm3_variance/
**Tier:** landed

Five serial full-pipeline runs of the-empty-pan.md at the post-ruling instrument state
(pre-registered PROPOSAL.md; driver-enforced kill conditions; neither fired). Read separately:
**M1** `<numeric_register>` complete 5/5 (first live firings — arm 1's was truncated off);
stage-8 densities 0.0/0.12/0.0/0.47/0.0 vs anchored 47.6; six surviving number-words total,
all ordinary prose. **M2** 4/4 stage-10 runs produced conforming D9 (both witness subsections,
hostile own candidates, explicit stage-9 adjudication; zero bare-5s); run 5 exited at review
(STRATEGY at cycle limit — designed). **M3** blind falsifier HOLDS 5/5, floor authored 5/5,
five distinct instrument-unreadable substrates. **NUMERIC_DENSITY_THRESHOLD recalibrated
25.0 → 10.0** per the 2026-07-11 ruling (improved ceiling ~0.5/1000 over six improved runs;
defect band 37.6–47.6). Run-3 word-arithmetic logged to OQ-214's calibration set. Remaining
for OQ-215 close: operator reads (refutation quality, foam-class substrates, ≥1 full story) +
close-out. Readout: `audits/2026-07-12_oq215_arm3_variance/READOUT.md`.

## 2026-07-12 — OQ-215 arms 1–2 run: R3(b) holds (operator-witnessed); blind falsifier discriminates, stage-10 D9 does not; truncation class fixed (caps + cap-hit guard + mode injection)
**Files:** agent/uke_narrative_orchestrator.py, agent/narrative_transform/stories/the-empty-pan_rev2.md, audits/2026-07-12_oq215_arm2_d9_control/
**Tier:** landed

**Arm 1 (R3(b) symmetric control): PASS, operator-witnessed by direct read.** Run
`uke/the_empty_pan_1783838645/` → `stories/the-empty-pan_rev2.md`. Stage 0 authored the R14
contract unprompted (inherent_instrument: yes); instrument survived as mechanism (SSA
certification world) with page density 0.48/1000 vs baseline 47.6 (3 numerals, all
institution-held certificate IDs — earned); blind stage-9 falsifier HOLDS; D9=5. Operator
watch-item: word-arithmetic in prose ("Quota minus rejections…") — invisible to the digit meter,
OQ-214's calibration case. **Caveat: stage_2 AND stage_3 hit their caps exactly (silent
truncation, OQ-216) — stage_3's blueprint lost `<numeric_register>`, so R2's field never reached
generation; arm 1's density win is attributable to the stage-2 gate + R1 exemplars + R6 only.**

**Arm 2 (D9 discrimination control): SPLIT.** Blind stage-9 DISCRIMINATES (Assessment → LOST,
kill passage named + grep-verified, ROUTE STRATEGY as pre-registered). Stage-10 D9 scored 5 on
everything including the negative control — quote-and-rationalize witnessed (cited a
by-eye-recoverable value as proof of unrecoverability), plus FULL-mode hallucinated with no spec
on both runs. **Consequence: any stage-10 D9=5 (incl. rev2's) is weak evidence alone; the
discriminating witnesses are the blind stage-9 falsifier + an operator read.** Readout + proposed
D9 adversarial-obligation fix (awaiting ruling): `audits/2026-07-12_oq215_arm2_d9_control/`.

**Infrastructure (the silent-green class, OQ-216):** stage-0 Gemini truncation guard (`b715f3dc`);
stage_1/2/3 caps raised 16384/16384/12288; universal cap-hit guard in `_call` (tout ≥ cap → fail
loud; Gemini exempt-by-accounting — needs semantic closure checks, noted in OQ-216); stage-10
validation mode now orchestrator-injected (never model judgment). All guards witnessed two-sided
offline.

## 2026-07-12 — CORRECTIONS to the R1–R14 landing: density threshold PROVISIONAL; rift3 "exclusion" retracted (witnessed meter false-positive class); prior D5/origin-obfuscation scores taken with the air gap partially open
**Files:** agent/uke_narrative_orchestrator.py, agent/narrative_transform/originals/rift3.md, ISSUES.md
**Tier:** correction-key

Three qualifications on the 2026-07-11 entry below (operator review, 2026-07-12):

1. **`NUMERIC_DENSITY_THRESHOLD = 25.0` is PROVISIONAL, not settled.** Two-point calibration with
   an 8-point dead band above the cleanest human prose (16.3) and 12 more below the nearest
   positive (37.6) — it passes stories counting 2x any clean control. To be RECALIBRATED from the
   OQ-215 arm-3 variance densities (the first real data on the improved-vs-anchored boundary for
   generated output). Do not cite 25.0 as a validated boundary.
2. **The rift3.md calibration "exclusion" was UNSOUND and is retracted.** Calling it
   "counting-saturated by design" was the agent pre-deciding the verdict the instrument exists to
   deliver. Witnessed read (no spend, `_numeric_inventory` + per-line contexts): 46.0/1000 — above
   both positives — but the narrator is a gauge-owning institutional POV whose every reading is
   taken and acted on in-scene (the 12.1%-logged / 11.6%-read discrepancy IS the story's hinge),
   plus numeric proper nouns ("Vent Fourteen" ×10). rift3 is therefore the witnessed
   FALSE-POSITIVE CLASS of the meter: density measures numeric REGISTER, narrower than the defect
   (UNEARNED counting); the discriminator is R2's positional access, applied only by the R6
   per-instance adjudication + OPEN-flag escalation (the gate never auto-rejects — correct). Full
   read + protocol implications in OQ-215; OQ-214 inherits it as the template problem (a character
   who EARNS a thesis-shaped line).
3. **Prior D5/origin-obfuscation scores were taken with the air gap partially open.** The
   stage_1_anon ANONYMIZATION note listed ORIGINAL character names into stages 2/3 for the whole
   life of the anonymizer (fixed in `a3d0fdc4`). Invariant/topology findings are unaffected (those
   were about structure, not names), but any pre-2026-07-11 D5/origin-obfuscation score or
   displacement read was measured over a pipeline that leaked source names into setting design —
   treat those numbers as upper bounds on obfuscation, not clean measurements. Applies to all runs
   in `agent/narrative_transform/uke/` predating `a3d0fdc4`.

OQ-215 protocol also REORDERED (operator ruling): R3(b) symmetric control runs FIRST (highest
information, most likely to fail; its failure collapses the conditional ruling to the hard ban),
then the R13 D9 positive control, then the 5-run variance only if both hold.

## 2026-07-11 — UKE narrative pipeline: counting-defect plan R1–R14 landed (deterministic numeric meter, computed word counts, invariant threading, counting-incentive prompt fixes)
**Files:** agent/uke_narrative_orchestrator.py, agent/narrative_transform/stage0.md, agent/narrative_transform/stage2.md, agent/narrative_transform/stage3.md, agent/narrative_transform/stage4.md, agent/narrative_transform/stage5.md, agent/narrative_transform/stage6.md, agent/narrative_transform/stage7.md, agent/narrative_transform/stage8.md, agent/narrative_transform/stage9.md, agent/narrative_transform/stage10.md
**Tier:** landed

Implemented the full counting-defect plan (`~/.claude/plans/we-are-evaluating-the-zany-biscuit.md`
rev 2; Claude-web comments reconciled) in five committed phases starting at `600abbae` (precursor:
Claude-web's stage2.md invariant-recovery rewrite + the evidence runs, incl. the witnessed
"Forty-Hertz" counting run `the_empty_pan_1783821245`).

- **Instruments (load-bearing, landed before prompt edits):** `_numeric_inventory()` — deterministic
  extraction of numerals/number-words/count-verbs/monotone sequences, injected complete into
  stage 7/8 prompts; model only adjudicates per instance. Post-stage-8 density gate: one targeted
  revision call, then `NUMERIC_DENSITY_OPEN.md` (fail-visible). **Threshold 25/1000 words,
  calibrated:** positives (must flag) stage_4=37.6, stage_8=47.6; clean human originals 2.3–16.3
  pass; inherent-instrument source `the-empty-pan.md` 18.8 passes; rift3 (46.0, counting-saturated
  draft) and the_waste_land (33.1, line-number artifact) excluded from calibration. All story
  word counts now orchestrator-computed and injected ("any other figure is wrong"); stage-8 manifest
  WORD COUNT line overwritten (model had fabricated 13,400 over a 5,927-word file and stage 9
  reasoned from it).
- **Invariant threading:** stage-2 SECTION 0 INVARIANT CONTRACT extracted and fed to stages 9
  (blind falsifier; assertion now `["stage_8","invariant_contract"]`) and 10 (new D9, reported not
  summed, D9≤2 → cannot PUBLISH; UNVERIFIED never N/A in craft mode; F50 fracture code). stage0.md
  authors the source-sighted Detector-A/B contract + `inherent_instrument` flag (surface-free),
  carried into stage 2.
- **Counting incentives killed at source (R3 per operator ruling (b)):** stage3.md exemplars
  rewritten so narrators structurally can't count (doctrine kept); numeric-register blueprint field
  (gauge-indexed access; numbers only when acted on in-scene); stage2.md Scored-Snare
  default-reject, exception gated on the Stage-0 flag only; stage5/6/9 stop rewarding counting
  (ANCHOR CHECK + invariant probe).
- **Mechanical:** final story ships without the EDIT MANIFEST (sidecar); stage4/stage7 prompt fork
  deduped (orchestrator appends canonical craft directives at load time); dead
  `_run_stage_5_narrative` + no-op `--skip-final-audit` removed; per-model cost table; stage-6
  feasible-range injection from `MAX_TOKENS` (16,384 tokens ⇒ ceiling ~11,468 words).
- **Fix-on-sight:** the stage_1_anon ANONYMIZATION note listed ORIGINAL character names (source
  leak into stages 2/3); now labels only. Cycle-2 stage_4 slot gets story-only (manifest stripped).
  Byte-identical dupes deleted: `stage2-original.md` (== HEAD stage2.md), `originals/the_empty_pan.md`
  (would double-process in --batch).

All wiring witnessed offline with fake providers (no spend). **Verification runs NOT yet done** —
protocol pre-registered as **OQ-215** (blocked_on_human: spend-go): 5-run variance vs the 37.6/47.6
baseline, R13 D9 positive control (pre-rewrite "Assessment" story must fail D9), R3(b) symmetric
control (Empty Pan under the flag must clear the meter AND pass D9). Theme-naming meter gap minted
as **OQ-214**.

## 2026-07-11 — OQ-188 + OQ-186 RESOLVED: pre-registered read-site flags (role-flip standing glyph; common-cause independence bit); false-cartel defensibility ruling downgraded
**Files:** python/shared/role_flip.py, python/shared/independence.py, python/enhanced_report.py, python/tensions_ledger.py, python/evaluative_convergence.py, prolog/config.pl, prolog/tests/test_oq186_common_cause_clique.pl, python/tests/test_role_flip_flag.py
**Tier:** landed

Full pre-registration (`PREREG.md` committed `57159a36` BEFORE any run) → evidence →
read-site fix, all in `audits/2026-07-11_oq186_oq188_readsite/`.

- **OQ-188 (98.1% branch).** Fire-rate census (manifest 2026-07-05T19:55:12Z, n=130):
  103/105 matched institutional seats flip under a single authored role change
  (agenda_setter 0.12 ↔ beneficiary 0.25 straddle the f(d) root d\*≈0.16418) →
  pre-registered ≥50% STANDING form: one legend sentence + per-line `‡` glyph
  (`shared/role_flip.py`, zero free parameters — role ladder, sigmoid, and root all
  from the SERIALIZED config). Buckets surfaced (unmatched=16 incl. 0.15×6, null=9);
  1 powerless firing → glyph is per-fired-seat (declared deviation, audit README).
  `config.pl:156-160` straddle note is comment-only; d values + OQ-01 bypass untouched
  (flag is commentary-grade — annotates, never overrides).
- **OQ-186 (outcome (a)).** A/B probe: co-authored slices form the full 3-clique,
  distinct-agent topic forms zero edges — node independence is not expressible
  Prolog-side (dedup keeps one edge per pair); discriminator `shared/independence.py`
  (≥1 shared beneficiary AND ≥1 shared victim AND |Δε|≤0.02; ε clause kept by census,
  9/21=42.9% of non-both-sides pairs inside margin). The one live both-sides pair IS
  the witnessed `moral_causation_locus` family.
- **Joint defect fixed.** `evaluative_convergence.py` `build_defensibility` no longer
  rules "coordinated rather than independent operation" on artifact-channel sets
  (either `all_members_knife_edge` or `members_common_cause_clique` true) — downgraded
  to a caveated constrained position; XCON elevation suppressed likewise. Two-sided
  witness: the dispositional_reading set fires both booleans and is caveated;
  network_2638bfb4 / network_a6b8a722 stay knife=False clique=False with the original
  ruling byte-unchanged.
- **Fix-on-sight (tensions_ledger).** Serialized neighbor keys are
  `constraint_id`/`edge_strength` — the old `id`/`edge_contamination` lookups rendered
  EVERY ledger edge as `? [...; strength ?]`; fixed, and the stale "provenance NOT
  CARRIED — OQ-103 open" note dropped (OQ-103 resolved 2026-06-12).

Regressions: plunit `test_oq186_common_cause_clique` (3/3) +
`python/tests/test_role_flip_flag.py` (6/6) + existing OQ-103 salience test still
green. No new Prolog reading predicate → `reading_registry.pl` obligation N/A.

## 2026-07-06 — OQ-213(a): `twin_comparison.py` graduated to N-general (Sonnet now a full paired leg); 3-leg run at HEAD, intersection 957
**Files:** python/audits/twin_comparison.py, outputs/pipeline_output.haiku.json, outputs/pipeline_output.flash.json, outputs/pipeline_output.sonnet.json, ISSUES.md, AGENTS.md
**Tier:** landed

OQ-213(a) RESOLVED: `twin_comparison.py` graduated to N-general (all-pairs guards, per-pair salted RNG, `analyse_agreement_nway` with missingness carried); all three legs re-classified in ONE serialized batch at HEAD `1169170` (legs had straddled `bbf5c92`/`ea8ed72`); 3-leg intersection n=957 — the 3 missing ids are exactly the treaty/legal seeds (residual (b)), not a behavior change.
Witnesses V1–V4 (split behavior-preservation, partition closure, delta-trace, ingestion): `audits/2026-07-06_oq213a_twin_sonnet_leg/`. Leg JSONs are at HEAD and regenerable — cite BOTH corpus and commit. (b) stays open kill-conditioned; interpretation rides OQ-123/124.

## 2026-07-05 — THIRD model-twin leg built: `testsets_sonnet/` (claude-sonnet-5, 1001 stories) — matched triple 957/960; unblocks the 3-model divergence OQs
**Files:** agent/run_no_scope_sonnet.py, prolog/testsets_sonnet/, json_sonnet/, prolog/beta_processed_sonnet.txt, prolog/testsets/, json/, python/audits/twin_comparison.py, ISSUES.md
**Tier:** landed

Third matched twin built over the SAME 1005-seed pool (`prolog/kernels/rebuild_2026-06-13/never_generated_seeds.json`): `testsets_sonnet/` = 1001 claude-sonnet-5 stories via `agent/run_no_scope_sonnet.py` (byte-identical prompt, thinking DISABLED, twin recipe `bulk_corpus_generation.md` §6); matched triple sonnet∩haiku∩flash = 957/960; `outputs/pipeline_output.sonnet.json` passed all four classify_corpus refusals; ~$48 spend.
5 genuinely-new extras merged into `testsets/` (130→135); the 39 collisions are the twins' own extras, NOT overwritten. 4 persistent Sonnet-specific schema-fail treaty/legal seeds + the marginals-only wiring gap → OQ-213; unblocks the 3-model divergence OQs (OQ-123/124/149/211/212).

## 2026-07-04 — OQ-88 MITIGATED: false-mountain detector sweep (positive WITNESSED N=2; D′ discriminator SATURATES; Ω_P auto-route ruling handed to operator)
**Files:** python/audits/oq88_false_mountain_detector.py, ISSUES.md, audits/2026-07-04_oq88_false_mountain_detector/
**Tier:** landed

OQ-88 MITIGATED: detector D (flat-routed ∧ engine-false-mountain) positive WITNESSED N=2 (cell 1); the pre-registered D′ regime-omega discriminator SATURATES (D′≡D; replicates 69/69 corpus-family-wide) → verdict per the pre-registered cells: gate-signal CANDIDATE, not a witnessed gate. Ω_P RULED (operator): review-prompt light seat, NO auto-route.
kernel_v1 fresh classify at `e438723b`; twins at `8a529c73` — D fires on ZERO twin stories by construction (100% kernel-linked); new instrument: in-file `cs_kernel_id` is a second Layer-A routing source the manifest walk misses; positive control = N=1 per engine regime (`411db0e`/`23b7faa`); pre-router archive stories are D-INAPPLICABLE, not D-negative.
Full record + TWINS_ADDENDUM.md + KERNEL_V1_ADDENDUM.md + FINDINGS.md post-review amendments: `audits/2026-07-04_oq88_false_mountain_detector/`.

## 2026-07-04 — OQ-125 RESOLVED (value-invariance beyond H1) + OQ-123 MITIGATED ((a) refuted; (b)-or-(c2) live): conditioned twin re-analyses
**Files:** python/audits/twin_comparison.py, ISSUES.md, audits/2026-07-04_twin_conditioned/
**Tier:** correction-key

Pre-registered conditioned re-analyses on the `bbf5c92` twin pair (prereg `bc04d809` before any
run; results `1314fecf`). OQ-125: the below-band |Δχ| tail survives same-side conditioning at all
4 typed χ seats — real value-invariance beyond H1; cite as "value-invariance confirmed (4 typed
seats)", never unqualified. OQ-123: imputation-drag (a) REFUTED; live remainder (b)-or-(c2), B4
not armed. Correction-keys: the "imputed ⇒ d==0.90" tell is FALSE (never classify imputation from
d); the 2026-06-13 twin tables are `8126231`-regime — `bbf5c92` unconditioned tables live in
`audits/2026-07-04_twin_conditioned/unconditioned_bbf5c92/`; an (a)-style drag claim needs the
comparative clause, not band-clearing alone.

---

## 2026-07-04 — OQ-140 RESOLVED: `author_engine_divergence` characterized (confound re-ranks kinds; one kind — Ω_E stratum reproduces on both twins, Ω_C reading 3/3 twin-confirmed)
**Files:** ISSUES.md, prolog/routing_sink.pl, python/audits/oq140_divergence_extract.py, audits/2026-07-04_oq140_divergence_characterization/
**Tier:** landed

OQ-140 RESOLVED (no engine edits; commits `e90bf3db` Phase 0/1, `9d7baf07` Phase 2): partialling the mechanical confound BEFORE decomposing re-ranks the population (confound-free 56/277 = 20.2%; pre-confound lead `tangled_rope→snare` dissolves; surviving lead `rope→scaffold`). One operator-ruled kind — `naturalization-over-claim (rope→scaffold correction)` [Ω_E] — reproduces on BOTH twins; Ω_C reading 3/3 twin-confirmed; one UNRESOLVED Ω_P prose-signal parked in OQ-211(d).
Scope pin: kind name + counts valid only at `route_address/5` HEAD `7762b2c0` (OQ-211 carries it, bundled_with OQ-138). Controls: emit-independence byte-agreement 277/277, D-ladder 49, mountain 0-count w/ positive control. Residuals → OQ-211. Evidence: `audits/2026-07-04_oq140_divergence_characterization/`.

---

## 2026-07-04 — Drone-report audit (Claude-web critique): d-header fixed, signature wording softened, OQ-209/210 minted, regulatory_lag H¹ fracture witnessed ROBUST
**Files:** python/enhanced_report.py, prolog/signature_detection.pl, ISSUES.md
**Tier:** landed

Drone-report critique triaged: FIXED the factually-false d-comparability header (`enhanced_report.py:356` — the common d path is AUTHORED via `derive_directionality/3` precedence, `constraint_indexing.pl:408`; only the fallback is a config lookup, so same-seat cross-constraint d is NOT apples-to-apples); FIXED `coupling_invariant_rope` overclaim (`signature_detection.pl:769,772`, display-only; = OQ-210 resolved); OQ-209 minted (single-constraint reports render corpus-scope metrics as success-shaped defaults; Pattern-6, bundled_with OQ-97).
Falsifier run: regulatory_lag H¹=4 is ROBUST — the 2+2 powerless≡institutional ≠ moderate≡analytical structure survives ε∈[0.50,0.90] and d_offset∈[−0.15,+0.20]. Caveats: ε/d authored (OQ-102a), Fisher/persistence STALE (OQ-29); probe scripts scratchpad-only.

---

## 2026-07-04 — OQ-193 report-surface build: giant_comp provenance split (pooled + cross-kernel stratum)
**Files:** prolog/giant_component_analysis.pl, python/run_pipeline.py, python/enhanced_report.py
**Tier:** landed

OQ-193 report-surface build (RULED (c)) landed at ZERO engine-behavior change (`per_constraint` sha256-identical): giant_comp `## Provenance split (OQ-193)` md section + same-run `giant_component_analysis.raw.json` co-product + manifest sidecar stamped ONLY on status==ok, and enhanced_report per-constraint NETWORK POSITION section with same-run guard. Strip method = retract-recompute, dead-last, never restored.
Witnessed (testsets): 68 sibling edges stripped; pooled giant 12/72 → stratum 9/95; `same_kernel_edges_surviving=0` (cross_kernel label HONEST); node set = 119 extractiveness-bearing subset of the 128-corpus. Tripwire: giant_comp intermittently hits the 900s Phase-2 timeout (OQ-182-class co-residency, not a regression) and degrades cleanly — check that before suspecting the code. Frozen probe (not edited): `audits/2026-07-02_oq193_giant_comp_ruling/probe_giant_ripple.pl`.

---

## 2026-07-04 — OQ-75(b) grain precursor probe: throw LARGE, cell-count non-monotone under coarsening (statistic-spec inputs)
**Files:** python/audits/oq75b_grain_probe.py, prolog/axiom_concept_registry.pl
**Tier:** landed

Pre-registered grain arms over the tranche-1 registry (10 kernels, 42 pairs, both legs; `audits/2026-07-04_oq75b_grain_probe/`): one refinement step throws LARGE (cells 47→21, contradiction-pair co-slotting 3/3→0/3); coarsen-max grows alignment mass but the raw cell count FELL 47→42 — a cell/vantage-count invariance statistic reads coarsening with the WRONG SIGN; verdicts grain-labile both directions (key_fragile 26→38→12).
Constraints recorded in OQ-75's ruled sub-item (grain normalization load-bearing; no raw-count statistic; contradiction-pair reads refinement-brittle). Controls fired (overlay-took-effect, known-changer, A0==OQ-72 sweep 47==47); canonical registry never edited. Stage 1 proper NOT discharged — statistic unbuilt.

## 2026-07-04 — OQ-72 consumer wiring: axiom concept alignment section in tensions_ledger (three-valued coverage); baker emits tranche-kernel facts
**Files:** python/tensions_ledger.py, python/axiom_concept_bake.py, prolog/axiom_concept_registry.pl
**Tier:** landed

Operator-directed post-close wiring: `tensions_ledger.py` appends a kernel-level "Axiom concept alignment" section (fresh swipl compute each run; a disparity cell = a tension by construction) with THREE-VALUED per-kernel coverage never collapsed (RATIFIED / NOT-YET-RATIFIED — blind BY DESIGN, GAP-24 / single-reading named); fails LOUD on swipl error; in-run TWO-SIDED join control closes the CLEAN-EMPTY hole (halt(3); both arms witnessed; the control's own falsifier caught a format/2-vs-format/3 bug in its first version).
Baker also emits `axiom_diff:axiom_concept_tranche_kernel/1` (coverage provenance travels in the registry; regen byte-identical + C6 refusal re-run). Mixed-scope and full-128 runs pasted in-session 2026-07-04; new tension surfaced immediately (moral_causation_locus disparity [deontological]|[instrumental]).

## 2026-07-04 — OQ-72 resolved: ratified concept key for the axiom axis (pilot); axiom_concept_registry born; westphalia tests re-frozen
**Files:** prolog/axiom_concept_registry.pl, python/axiom_concept_bake.py, prolog/stack.pl, prolog/tests/test_axiom_diff.pl, prolog/axiom_diff.pl, ISSUES.md, docs/the_perturbation_principle.md, docs/design/design_gaps.md
**Tier:** landed

OQ-72 closed at the scoped altitude "mechanism demonstrated" (mixed 10-kernel pilot; `audits/2026-07-03_oq72_concept_key_pilot/` WRITEUP.md has the control table): `prolog/axiom_concept_registry.pl` is the NEW CANONICAL populator of `axiom_diff:axiom_concept/2` (71 ratified facts, tranche 1), loaded from stack.pl; regenerate ONLY via `python/axiom_concept_bake.py` (fail-closed on unratified rows). All six pre-registered controls passed; false-merge 0/71.
Standing cautions: the registry is NAME-keyed (applies on ANY leg); `cs_axiom_contradiction` is not universally same-subject; the key makes the axiom axis RATIFIED-legible, not discovered (§7.1 amendment, `docs/the_perturbation_principle.md`). Also fixed en route: westphalia tests in `tests/test_axiom_diff.pl` were silently unrunnable-green since the 2026-06-20 regime swap (now fixture-local), and their blanket retractall would have wiped the baked registry (now scoped; post-run 71 witnessed). SCOPE-time slot emission = GAP-24; scale-up = separate spend-go (recipe in the OQ-72 resolution).

## 2026-07-03 — OQ-03 RESOLVED: operator declared DR's own seat (extraction-seeking skepticism); 03b mooted; self-application run snapshotted
**Files:** ISSUES.md, audits/2026-07-03_oq03_self_application/
**Tier:** landed

OQ-03 RESOLVED by operator ruling: 03b MOOTED — where DR sits is the declared seat itself, not a redraw-measurable fact. Declaration (operative text in ISSUES.md OQ-03): DR is a variety of philosophical skepticism seated to look for extraction everywhere — a lens, not the truth (`docs/seat-theorem-v1.md`, `docs/commitment_systems/*`, `docs/debugging_philosophy.md`; known limit `essays/2026-06/the_same_paper.md`).
Datum: operator ran `docs/deferential_realism_paper_v8.md` through c-orchestrator same day (5 stories, commit `72ab7663`, manifest n=128) — seat-indexed plurality, kernel siblings diverged; single LLM draw, illustrative seated datum only, never "DR is X". Ledger + reports snapshotted: `audits/2026-07-03_oq03_self_application/`.

---

## 2026-07-03 — OQ-205 RESOLVED: ε declaration discipline BUILT (11 units, Controls P/S green through the recurring gate)
**Files:** prolog/constraint_indexing.pl, prolog/boltzmann_compliance.pl, prolog/narrative_ontology.pl, prolog/data_validation.pl, prolog/json_report.pl, prolog/reading_registry.pl, prolog/tests/test_epsilon_declaration.pl, prolog/tests/fixtures/eps_controls/, python/generate_constraint_pl.py, python/run_pipeline.py, python/enrich_pipeline_json.py, python/enhanced_report.py, python/sweeps/epsilon_stability.py, python/epsilon_authorship_readout.py, docs/design/epsilon_declaration_discipline.md, docs/deferential_realism_paper_v8.md, ISSUES.md, audits/2026-07-03_oq205_build/
**Tier:** landed

Build U1–U11 landed same-day as the spec (commits `e9041905`…close; unit→commit map + transcripts: `audits/2026-07-03_oq205_build/README.md`); all five §9 graduation criteria met, OQ-205 resolved. Both §3 fabrication fallbacks DEAD (fail-closed; the first U2 cut emitting computed-looking `scope_violations: 0` was REJECTED as Pattern 6); no-backfill ruling recorded (pre-build corpus = declared loud-null stratum, `"none_authored"`); new recurring gates `_prolog_epsilon_declaration_gate` + ε-stability sweep, deliberate-break controls witnessed for both.
Sweep tripwire: `drl_core:base_extractiveness/2` is multifile STATIC — took-effect guards must `once/1` the read (an unpinned guard "passed" under the shadow). Corpus finding: `unstable_off_grid` is the largest flag class on every leg (43/110 live, 452/1106 kernel_v1) — ε-sensitivity is mostly NOT threshold proximity; routed to OQ-78/OQ-48; standing readout `python/epsilon_authorship_readout.py` (pipeline Phase 9c) reproduces the census exactly.

---

## 2026-07-03 — OQ-205 spec landed: ε declaration discipline (provenance + stability), read-only census with control PASS
**Files:** docs/design/epsilon_declaration_discipline.md, docs/design/design_discipline.md, ISSUES.md, audits/2026-07-03_oq205_epsilon_census/
**Tier:** landed

Spec-only session: `docs/design/epsilon_declaration_discipline.md` authored (disambiguation vs DP-001/OQ-26 — never title anything "ε invariance"; `epsilon_provenance/5` R2; read-site table anchored `6c59615e`; stability protocol r=0.02 R3 with two kill conditions; R4 commentary-grade); OQ-205 → partial; design_discipline §7 cross-pointer same-commit.
Census (`audits/2026-07-03_oq205_epsilon_census/`, 4 legs, planted control PASS): flash authors ε exactly ON thresholds 218/960; the (0.45,0.46) interval is EMPTY on all legs; OQ-78 re-baseline 41.8% (46/110), last-digit rail model-specific. Recon corrections: threshold set includes `mountain_extractiveness_max` 0.25; SECOND fabrication fallback found at `boltzmann_compliance.pl:248–252` (BaseEps=0.5) beside `constraint_indexing.pl:902–903`; every story authors ε TWICE (silent-fork surface — spec §3 requires equality-check or declared canonical).
Same-day ratification: R2–R4 RATIFIED with the three-site equality check + two-class stability flag (`on_threshold_grid` vs `near_threshold`) amendments; R4 gained its promotion trigger; audit-dir tracking witnessed (8 files in `a2a87dc5`).

---

## 2026-07-03 — OQ-138 FNL sub-part BUILT: RECLASSIFY→ROUTE landed (d248a6b1 + 82aa372e), consumers keyed on the lever, census type-inert was default-context-scoped
**Files:** prolog/signature_detection.pl, prolog/config.pl, prolog/config_schema.pl, prolog/abductive_helpers.pl, prolog/maxent_classifier.pl, ISSUES.md, audits/2026-07-02_oq138_fnl_evidence/
**Tier:** landed
The OQ-138 FNL CONVERT ruling's owed build, in two commits with the twin-diff hard gate between them (operator approved with one condition, folded in). U1 (`d248a6b1`, output-changing): `:925` overwrite → route behind NEW `false_natural_law_override_enabled` (0=route default, 1=legacy; schema spec added — config_schema.pl gate fails loud on a spec-less param); `fnl_routed/1` outcome-keyed (dr_type/3 non-circularity TRACED at HEAD: 152-pred closure, 3 positive controls); victim-discriminated severity (vic>0→moderate). U2 (`82aa372e`, wiring): seat_overrides + maxent boost keyed on the LEVER, a deliberate departure from the plan's `\+ fnl_routed` shape.
- **Tripwire-grade finding: `fnl_routed/1` (and `fcr_routed/1`-style seat predicates generally) are DEFAULT-CONTEXT-keyed while `resolve_modal_signature_conflict` overwrites are ORBIT-wide.** Witnessed: `organization_floor_c0` ("type-inert" in the census) routes tangled_rope→scaffold at the INSTITUTIONAL position while default-context unknown — the census's type-inert column was default-context-scoped only. Consumers that would lie under default-keying (probe_signature via seat_overrides; the PER-CONTEXT maxent boost — apply_signature_override fires at all 4 Wasserstein contexts) were therefore keyed on the lever: at lever=0 NO seat overwrites (typed seats route, unknown seats abstain), so override-liveness IS the lever state, orbit-safe. FCR reconciliation: FCR's non-routed seats keep their boost because `fcr_override_enabled` defaults 1 (override still LIVE there) — one rule, "boost mirrors live overwrite," two outcomes. **Re-open condition (the SPECIFIC kill, not the general fact):** default-keying is FINE for the grade/severity consumers (`converted_at_seat` → SigGrade/severity are default-headlined by architecture, like verdict_join itself); what trips this is a FUTURE consumer that reads `fnl_routed`/`*_routed` for ORBIT-SENSITIVE override-liveness (anything evaluated per-context or aggregated over the orbit — a maxent-style per-context injector, an orbit-walking exporter). Such a consumer must key on the lever (or a per-context predicate), never on the default-keyed seat predicate.
- **Twin diff (THE behavior witness, `FNL_CONVERSION_DIFF.md`):** 8/14 routed seats render RED (census predicted green→yellow — prediction vs measurement, the FSM lesson again): type_1_false_summit informational→severe on routed snare + **h1 0→3 / sheaf→manifest** — the overwrite applied at every context and flattened the whole orbit into a manufactured global section (pasted orbit: competence_occupation OLD tangled_rope×4 → NEW {snare,snare,scaffold,snare}). Determinism control NEW-vs-NEW2 0/960; OLD arm byte-identical to the pre-conversion canonical baseline; twin spillover (8+31 seats) all maxent/ensemble refit, zero signature/type/grade changes; live leg 0 verdict changes (89 records move in wasserstein/arakelov/signature_pressure only — one seat's orbit change re-centers corpus-relative ensembles).
- **Gates:** 5-corpus sweep routed 0/6/8/0/0 with routed∩piton=0 retained as positive control; per-context consumer probe (org_floor_c0@institutional = scaffold + no_boost + agrees, BOOST-CONTROL fires on every leg); two-sided ablation (lever=1 restores legacy at every context, incl. the legacy computed-but-unrendered override_mismatch at org_floor_c0 — proving route-mode's `agrees` is an improvement, not a hidden artifact); gate.sh GREEN.
- **Correction-key (pre-existing failures, attributed NOT-mine by identical failure at HEAD-files+lever=1):** validation_suite has 119 PASS + 1 FAIL (`lycurgan_laws__demographic_trap_reading` BCE interval 480>330). Two-axis dating of that fail: the interval fact is byte-identical since pilot_05 (`f4c7b13d`, 2026-06-13) and the file WAS in the 2026-06-21 suite that read 92/0/0 — so the interval-validity check entered the regenerated suite AFTER 06-21; the plan-era "92/0/0" denominator is the 06-21 corpus size (suite is auto-generated 1 unit/file; corpus grew 92→119 via topic runs). Open corpus-content question flagged, not fixed (needs a BCE-encoding ruling, other BCE stories may share it): should BCE intervals be authored as negative years? `test_agent_beneficiary` fails 35/94 (per-testset threshold/profile validation units on the current corpus — the "green" expectation in the plan was stale); `test_contradiction_signatures` same 5-name set as its known baseline. Cite these as baselines, not regressions.

---

## 2026-07-03 — OQ-87 twins characterization DONE (zero-spend): committer axis byte-stable, magnitude convention model-idiosyncratic, existence proof re-scoped to de-baited rate; OQ-208 minted (CA-2 split)
**Files:** ISSUES.md, audits/2026-07-03_oq87_twins_ca3/, prompts/constraint_story_generation_prompt_DRIFTNEUTRAL.md
**Tier:** landed
Pre-registered read-only run (PLAN.md committed before any arm; four serialized swipl runs: kernel_v1 906-pool / testsets 89 / haiku 960 / flash 960; seven controls all discharged incl. two kill conditions). Full record: `audits/2026-07-03_oq87_twins_ca3/FINDINGS.md`; commits `8ac24afc`→`e99ccaf5` + this landing.
- **Committer axis byte-stable across 26 days of observer-engine evolution:** banked (2026-06-07) vs HEAD `dfe10734` on the 906 pool = **0 committer-verdict flips vs 42 observer-bucket changes** (same differ read both columns — internal positive control). Anchor diverge-A 74→82 wholly observer-side (11 gained / 3 lost, all stayed `dead`; OQ-51 null-exclusion = 0). Theorem-7-consistent characterization, NOT a proof (bait-bearing substrate).
- **fired = grep-candidate exactly on all four corpora** (16/16, 129/129, 136/136, 18/18): the `cs_axiom_foreclosed` conjunction is file-locally decidable at HEAD — grep-candidate counts may be cited as fired counts *at this code state*.
- **Magnitude convention is model-idiosyncratic:** substantial-rate haiku 0.870 vs flash 0.505 (|Δ|=0.365). Foreclosure-shaped authoring (`axiom_overriding`+non-minor+unack) clusters ≈0.21 on the three Anthropic-era corpora (0.213/0.206/0.211) and collapses on the Gemini twin (0.027). Flash fired-core 18 < pre-registered floor 20 → flash-side and shared-core rates are DESCRIPTIVE-ONLY (flash-rate 1,067-story and shared-rate 2,133-pair sizing figures may NOT license a spend; only the haiku-rate 141 is citable). Conditioned direction agreement 0.734 vs chance 0.687 — near-chance cross-model content under bait.
- **Rulings (operator):** (1) FOLD-IN — OQ-87's proof limb `blocked_on OQ-75`, with the edge tracking the MEASUREMENT (a de-baited fired-core rate), not the rebuild artifact; **DRIFTNEUTRAL pin currently ABSENT repo-wide** (controlled grep; only banked audit scripts reference the prompt) — pin note added to OQ-75; ~150-story pilot recorded as the sooner-option, trigger = a named downstream forcing function. (2) CA-2 SPLIT → **OQ-208** (Priority 3, `splits_from OQ-87`), Deps authored with BOTH exit branches: construct a framing-sensitive positive control OR prove none can exist (negative-by-construction close admitted; code-level branch (b) first, near-zero spend).
- **Tripwire (probe adaptation, carried in the audit dir):** the banked ca3 probes bucket `H0==1 else incoherent` — post-OQ-51 that silently misbuckets `H0=null` as incoherent (77/91 stories per twin are undetermined). Any reuse of pre-2026-06-25 H0-consuming probes needs the 3-way bucket.

---

## 2026-07-02 — Four blocked_on_human rulings landed: OQ-138 (CI-rope KEEP+close, FNL CONVERT), OQ-193 (giant_comp additive-split), OQ-75 (Stage-2 scoped-go)
**Files:** ISSUES.md, audits/2026-06-21_oq138_fsm_route_conversion/CIROPE_RED_ADJUDICATION.md, audits/2026-07-02_oq138_fnl_evidence/, audits/2026-07-02_oq193_giant_comp_ruling/, audits/2026-07-02_oq75_stage2_preflight/
**Tier:** landed
Witness-gathering + rulings for four blocked items (probes read-only w.r.t. engine substrate — reversible corpus overlays via `retractall+assertz`, verified restore, per-probe positive controls). No engine behavior changed; two CONVERT/build obligations recorded in ISSUES.md, not started.
- **OQ-138 CI-rope route-purity — RULED KEEP-as-written, limb CLOSED.** 5 rope-consumers re-witnessed at HEAD. Inherited neutron_star RED sub-item RESOLVED MOOT: at HEAD neither neutron_star nor superheavy is RED (OQ-128 discriminated severity + FCR-9 conversion each independently removed the cap). superheavy is a DOCUMENTED FCR-inert seat (CONSTRUCTED3_FINDINGS.md:21; 0-hit in FCR9_live_diff), verdict-absent because unknown-surfaced — absence discriminated by neutron_star's present verdict in the same dump. Kill condition stays live. Witness: `CIROPE_RED_ADJUDICATION.md`.
- **OQ-138 false_natural_law — WITNESSED + RULED CONVERT (build OWED, not started).** 4-leg census (testsets 1 inert / haiku 13, 6 changed / flash 8, 8 changed / kernel_v1 0). The 14 type-changers repeat the FSM/FCR shape (scaffold/snare→tangled_rope, green→yellow unmask, correction grade, claim+vic discriminant). ALL 22 firings source-1 explicit_mountain_claim, ZERO source-2 (OQ-70 fix holds). kernel_v1=0 is measured-empty (41 claims × 973 non-compliant, intersection 0). Both census + diff positive controls passed. Build owed: conversion + 5-corpus sweep + abductive_helpers/maxent consumer fixes. Near-free on the live leg. Witness: `audits/2026-07-02_oq138_fnl_evidence/FNL_EVIDENCE.md`.
- **OQ-193 giant_comp — RULED (c) additive provenance split (topology ruling, report-build OWED).** 3-leg ripple confirmed at HEAD (giant 12→9 / 549→47 / 334→70). Per-consumer price: FPN NO-DIFF (OQ-23 guard already zeroes sibling contamination — two-sided controlled: planted cross-kernel strip DOES move purity on testsets; haiku vacuous-but-consistent); json_report/network_dynamics/severity DO change (15/282 hub flips). Headline has zero downstream consumers. (c) = siblings stay in topology for all 5 consumers + giant_comp reports both pooled & cross-kernel counts. NOT zero-cost — it rules siblings intended topology. Witness: `audits/2026-07-02_oq193_giant_comp_ruling/RULING_EVIDENCE.md`.
- **OQ-75 Stage-2 — RULED SCOPED GO (a).** Part (a) diff-distribution authorized; part (b) cross-axis correlation (the OQ's headline staked falsifier) stays UNTESTED (standalone build; OQ-15 resolved 2026-06-24 so NOT gated on a mediator layer). Construction-pair stratum N/A this cohort (twins carry 0 flat_control facts vs testsets' 10; recorded in OQ-76). Preconditions before citable numbers: build the prevalence counter (harness-reuse extension of `oq49_override_remeasure.py` — confirm it counts prevalence not override-firing) + clean-tree twin reclassify (both twin manifests code_dirty). Witness: `audits/2026-07-02_oq75_stage2_preflight/PREFLIGHT.md`.
- **Correction-key:** two exploration-record errors corrected in the OQ-75 preflight — OQ-15 is RESOLVED (2026-06-24, `279d7c24`) not open; and a `false_*`/`dr_claim_mismatch` prevalence counter does NOT exist from scratch but CAN be built as an extension of `oq49_override_remeasure.py` (which counts override firing, not prevalence — confirm before citing the cost as cheap).

## 2026-07-02 — OQ-126 RESOLVED: drift terminal carries its authored-ack provenance (witness-not-verdict); external-anchoring tier ladder promoted to design_discipline.md §10
**Files:** prolog/json_report.pl, prolog/cs_drift_engine.pl, prolog/tests/test_cs_drift_engine.pl, python/shared/schemas.py, python/enhanced_report.py, docs/design/design_discipline.md, ISSUES.md
**Tier:** landed

OQ-126 RESOLVED (`ee51cdff`): the drift terminal now carries its authored-ack provenance as witness-not-verdict — new fields at every terminal surface incl. the no-CS-UID default branch (missed on the first edited run, 30/119; test w3 pins it): `cs_drift_terminal_basis` + `cs_drift_ack_witness`, with `confrontation_path: "none_exists"` a NO-PATH sentinel (OQ-107 `future`), NOT "checked, none found"; enhanced_report renders the terminal conditional (decoration kill-condition control).
RED control witnessed both ways (24/24 restored; test_cs_trifurcation 19/19, OQ-55 twin untouched); clean-vs-edited diff n=119 additive-only; twins n=960×2, 0 missing/unfaithful. Item (c): external-anchoring tier ladder promoted into `design_discipline.md` §10. Ω_P core (honor/reabsorb seated, never engine-certifiable) closed DECLARED, not solved; stale OQ-74 cross-ref corrected (resolved 2026-06-14).

## 2026-07-02 — OQ-195 RESOLVED: general-n H¹ gap spectrum proven at every cardinality; stakeholder frame makes it the live law; OQ-207 minted
**Files:** docs/h1_gap_spectrum_general_n.md, python/audits/oq195_h1_spectrum_check.py, prolog/tests/test_h1_spectrum.pl, prolog/grothendieck_cohomology.pl, docs/deferential_realism_paper_v8.md, docs/deferential_realism_paper_v7.md, docs/deferential_realism_paper_v6.13.1.md, ISSUES.md, audits/2026-07-02_oq195_general_n_gap/
**Tier:** landed

OQ-195 RESOLVED: new proof doc `docs/h1_gap_spectrum_general_n.md` (commit `5d052990` + close) — min nonzero H¹ = n−1 at every cardinality, exact band decomposition, unconditional band-floor lemma, inter-band gap iff n ≥ j+3+C(j+1,2), type-token bound T=7; LIVE via the stakeholder frame (3–12 seats/story on the live legs). Verified under pre-registered BLOCKING criteria n≤40 with PER-BAND bookkeeping (a union-only check cannot discriminate — the unconstrained classifier ran as control, bands mismatch 38/39); engine witness `test_h1_spectrum.pl` 23/23.
Propagated: v8 §3.4/§9.6/Appendix, v7 dated amendment, v6.13.1 changelog, `grothendieck_cohomology.pl` comments (behavior-preserving). Line-drift correction-key: cite the stale-range flag by predicate header, never `grothendieck_cohomology.pl:158`. OQ-207 minted (stakeholder-frame H¹ build; `consensus_provenance/2` its H¹=0 special case). Evidence: `audits/2026-07-02_oq195_general_n_gap/`.

---

## 2026-07-02 — OQ-70 premise-rot correction: canon said "until ruled" for 27 days after the ruling; v8/README inherited it on authoring day
**Files:** CLAUDE.md, README.md, docs/deferential_realism_paper_v8.md, ISSUES.md
**Tier:** correction-key

OQ-70 was RESOLVED 2026-06-05 (`72ec2cdd`) but CLAUDE.md kept the pre-ruling "until ruled" framing
for 27 days, and v8 §9.4/§9.6/Appendix + README inherited it on authoring day. All surfaces
corrected; the surviving canon (regime-bound archives / claims-statistic-only / double statistics
reset, discount per `audits/2026-06-11_oq109_phase_b/EXAMPLE_INHERITED_SIGNATURES.md`) now lives
in CLAUDE.md Critical Distinctions. Downstream unblock: OQ-138's false_natural_law member was
stale-deferred "pending OQ-70" — gate-expiry annotated; v8 open-lists → {OQ-195, OQ-205}. Probe
note: stale-phrase greps need wrap-proof patterns (the v8 instances line-wrapped; the control
caught it).

---

## 2026-07-02 — OQ-135 RESOLVED: v8 adopted (seat/gauge/orientation); v8 paper authored; README/CLAUDE.md refreshed; vocabulary migration wave
**Files:** docs/deferential_realism_paper_v8.md, README.md, CLAUDE.md, AGENTS.md, ISSUES.md, docs/seat-theorem-v1.md, docs/one_seat_audited.md, docs/design/design_discipline.md, docs/metrics_as_routing.md, docs/technical/paper_versioning.md, docs/v8/foundations/README.md, docs/logic.md, docs/logic_thresholds.md
**Tier:** landed

OQ-135 RESOLVED: v8 adopted wholesale (operator ruling) in four phased commits — `4ea2c2d5` (v8 paper: entry point, canonical seat/gauge/orientation vocabulary, §5.4 bridge table), `16143c15` (review-response Appendix, Perplexity point only), `7c4cca6f` (README rewrite, claims re-witnessed), `64a44514` (CLAUDE.md refresh); Phase-4 close incl. dead-hash note (`fd1ee561` does not resolve) and fresh-agent self-containedness control 7/7.
Near-fork DECLARED, not resolved (Pattern 2): `docs/v8/foundations/` is source material; `docs/` + `config.pl` stay canonical (8 byte-identical copies, 4 STALE snapshots, which-v6.9 unresolved; the seven-category framing incl. "Naturalized" is historical — live taxonomy is six types + naturalized as cascade outcome, v8 §3.3; contradiction noted at core_v4.3.md:46,117).
Mojibake REPAIRED (operator-ruled): `docs/logic.md` 1,791 + `docs/logic_thresholds.md` 172 sequences — the Feb-2026 repair was PARTIAL and persisted continuously (per-revision counts flat since ≥2026-02-15; NOT a recent regression); method = per-run cp1252 round-trip + 5 hand mappings, 5 positive controls (scratchpad `moji_fix.py`); residual audit zero. The `⤠` (U+2920) rope-gate bypass symbol flagged as likely ancient corruption of `⊤` — cosmetic, not decided.

---

## 2026-07-02 — OQ-137 RESOLVED (reading registry + totality suite + pipeline gate + sweep fixes); OQ-136 evidence in (haiku/contradictions authoring artifact vs genuine mcc)
**Files:** prolog/reading_registry.pl, prolog/tests/test_reading_totality.pl, prolog/commentary_census.pl, prolog/signature_detection.pl, prolog/report_generator.pl, prolog/cs_drift_engine.pl, prolog/cs_axiom_engine.pl, prolog/tests/test_cs_drift_engine.pl, python/run_pipeline.py, python/audits/oq136_bucket_provenance.py, audits/2026-07-02_oq136_census_bucket_provenance/, audits/2026-07-02_oq137_reading_totality/
**Tier:** landed

OQ-137 RESOLVED (slice `a81d4c83`/`2453b922`; close `486756fe`/`ed851eb7`+gate): `reading_registry.pl` (`aggregatable_reading/3` + `census_source_backing/2`) + registry-driven `test_reading_totality.pl` now open `run_pipeline._phase_prolog` as a sequential fail-fast gate (wiring control: planted broken entry → red, clean → green, per_constraint byte-identical). Defects fixed: explain_signature missing `unknown` clause (silent report truncation; planted witness 0/110→111/111), cs_terminal_attractor overlapping rows, cs_has_axioms/+C→+UID doc keys; test_cs_drift_engine rebuilt (RED since the reset), 11/11.
Tripwires: `[C]-m:g(...)` / `V^m:g(...)` templates parse WRONG (`:` is priority 600) — parenthesize `(m:g(...))`; the first sweep passed VACUOUSLY until planted controls caught it. Register any new aggregate-consumable reading predicate in `reading_registry.pl` same-change (opt-in).
OQ-136 evidence in (PROPOSAL frozen `0ba48b4c` before the join; execution `2b66dedc`): q6_unmeasured + no_agent_seats cluster = ONE haiku/contradictions generation-path artifact (p_holm=8e-4); mcc hand-read 8/9 genuine. Rulings executed → OQ-202/OQ-203/OQ-204 minted; R3 one-legged caveat kept; `no_agent_seats` out-of-domain RATIFIED; OQ-136 resolved. Evidence: `audits/2026-07-02_oq136_census_bucket_provenance/` + `audits/2026-07-02_oq137_reading_totality/`.

## 2026-07-02 — Cross-leg check: OQ-52 replicates member-level; OQ-45's phenomenon recurs via DISJOINT members (draw-variance); live-leg hidden-winner exists
**Files:** audits/2026-07-01_oq45_oq52_hidden_winners/, prolog/testsets_haiku/temple_sacrifice_commitment__performance_only.pl
**Tier:** landed

Cross-leg check of the 2026-07-01 closes (B5 of `audits/2026-07-01_oq45_oq52_hidden_winners/`): OQ-52's authored-channel finding replicates member-level 100% on every live leg (haiku 113/113, flash 83/83, live 8/8); OQ-45's phenomenon RECURS via DISJOINT members — expected draw-variance (OQ-26), no member-level replication claimed; one hidden-winner on a LIVE leg (`prolog/testsets_haiku/temple_sacrifice_commitment__performance_only.pl`). kernel_v1 NL population 26 matches the 2026-06-10 matrix (aggregate control PASS); all dispatch controls PASS.
CITATION AMBIGUITY WITNESSED: "HEAD yields strict=235" was the HEAD engine on kernel_v1, not a canonical-corpus count — when citing counts across classify_corpus runs, name BOTH corpus and code state (rule promoted to CLAUDE.md Running the System).


## 2026-07-01 — OQ-45 RESOLVED (YES: hidden winners in the 404) + OQ-52 RESOLVED (W1 leg delivered; population counts are engine-regime-relative)
**Files:** python/w1_sheaf_join.py, prolog/signature_detection.pl, audits/2026-07-01_oq45_oq52_hidden_winners/
**Tier:** correction-key

Both closed as the presents-as-natural / hidden-winner pair (OQ-52 beneficiary-authored, OQ-45
beneficiary-silent; answer YES per-story only — chimera corpus, OQ-70/OQ-25, no prevalence
claims); `w1_sheaf_join.py` rows gained incomparable_mass + material (freezes OQ-51's "~0.05" as a
LABEL; stale prose retired `e8189d10`). Citation corrections: the OQ-52 "16 of 98" count is
engine-regime-relative (HEAD yields strict=235/loose=58 of 944, matching the OQ-197 controls
`34ff919f`) — save member LISTS, not counts; "both authored channels" is now 289/293; a naive NL
sweep on HEAD returns 0 (`8b5a34b8`/OQ-113) — the 404 population is recoverable only via the
overlay recipe in `b1_nl404_probe.pl`. Bucket-(ii) design note → GAP-08 §7; method note: pre-
flight a content rubric on known positives (v1 failed 0/3, v2 3/3). Full record:
`audits/2026-07-01_oq45_oq52_hidden_winners/WRITEUP.md`; branch `oq45-oq52-hidden-winners`.

## 2026-07-01 — OQ-41 RESOLVED (row-26 five-site expansion) + OQ-40 RESOLVED (doc lift) + OQ-201 minted (row-22 spin-out)
**Files:** ISSUES.md, docs/design/two_axis_architecture_v7.md, audits/2026-07-01_oq41_row26_expansion/, prolog/signature_detection.pl, prolog/drl_fpn.pl, prolog/covering_analysis.pl, prolog/gap_diagnostic.pl, prolog/omega1_audit.pl
**Tier:** landed
Row-26 five-site expansion at HEAD `27afde7a` (behavior-preserving; gate GREEN, validation 0 errors): `drl_fpn:197` is a sentinel pass-through CARVED OUT of row-26 (prior entry conflated it with `:206`); `covering:490`'s 0.5 is a presence guard (the off-grid trigger-class had zero members); `covering:490`/`gap:120`/`omega1:102` = DORMANT/LOCKED (OQ-44 once-for-class); `drl_fpn:206 Immunity=0.5` = NEUTRAL-by-corpus (0 natural fires on testsets(119)+kernel_v1(1106), positive control fires; sink diagnostic-only).
OQ-40 RESOLVED: rows 19–20 split RULED-INTENDED, lifted into `two_axis_architecture_v7.md` §"Representation grounding". Row-22 → OQ-201 minted: `compute_temporal_stability` reads the scalar store, not `measurement/5`; 107/110 and 934/1106 reach-the-gate constraints author an ignored temporal series; >1 scalar level = 0 on both corpora → variance path dead, gate is a degenerate presence-check (positive control catches a known series). Evidence: `audits/2026-07-01_oq41_row26_expansion/`.

## 2026-07-01 — R4 RULED → OQ-200: detector_calibration carried as corpus-level OQ, NOT wired; module now TRACKED-but-unwired
**Files:** ISSUES.md, docs/design/detector_calibration_omega_proposal.md, prolog/detector_calibration.pl, audits/2026-07-01_oq197_r4_recompute/
**Tier:** tripwire

R4 RULED after the recompute retracted the ~3× inflation (net-new 39/41 determinable): the net-new
is low-KIND-entropy — false-summit `mountain→tangled_rope` (the OQ-70/FNL axis) + a
`tangled_rope→rope` author-over-claims-contestation residual — so it is carried as corpus-level
OQ-200, NOT wired per-constraint; reporting condition (same as OQ-199): "directional disagreement,
calibration open (Ω_E), FP-rate unset (Ω_P)", never "miscalibration detected". TRIPWIRE (now
covered at the edit site — the full do-not-wire-without-reopening-R4 warning is in
`prolog/detector_calibration.pl`'s header, and OQ-200 carries it): the module is TRACKED-but-
unwired reference, loaded by nothing. Evidence: `audits/2026-07-01_oq197_r4_recompute/`.

---

## 2026-07-01 — gate check added: human gap surfaces must distinguish no_gap from undetermined (Pattern-6 guard)
**Files:** scripts/gate.sh, python/check_gap_status_surfaces.py, python/query.py
**Tier:** tripwire

Covered at the edit site 2026-08-10: the add-here obligation (a NEW human-facing gap/omega
renderer is unguarded until added to the surface list) now lives in
`check_gap_status_surfaces.py`'s own docstring. Verdict: gate check `gap surfaces` asserts the
three renderers (`tensions_ledger.build_block`, `enhanced_report.build_omega_section`,
`query.format_gaps_block`) distinguish no_gap from undetermined; positive-controlled (goes RED
on a collapsing renderer). `query.py` gap block extracted to `format_gaps_block/1`.

---

## 2026-07-01 — OQ-197 ruling (a) bound to OQ-199 reporting-condition; R4 recompute retracts the ~3× inflation
**Files:** ISSUES.md, audits/2026-07-01_oq197_r4_recompute/, prolog/detector_calibration.pl
**Tier:** correction-key

Ruling (a) (keep the stakeholder source) finalized as non-redundancy-established / reliability-
UNRESOLVED — bound to OQ-199 as a BINDING reporting condition ("authored-stakeholder
disagreement", never "validated cover-story detection"). R4 recomputed READ-ONLY: net-new = 39/41
determinable; genuine undetermined-inflation only 4/12 — the "~3× inflation" is RETRACTED (it
mislabeled the no_gap bucket as artifact, the same conflation OQ-197 fixed). Do not cite the old
14/12 or 3× forward. Evidence: `audits/2026-07-01_oq197_r4_recompute/`.

---

## 2026-07-01 — OQ-197 acceptance controls PASS (kernel_v1 944 + twins 29/41 reproduced from substrate); case-(ii) refinement
**Files:** audits/2026-07-01_oq197_acceptance_controls/, prolog/report_generator.pl
**Tier:** landed

Graduation witness for the OQ-197 chain — counts reproduced from substrate, not the doc: kernel_v1 canonical-varying = 944 exactly (stakeholder_facts=0); twin detector_calibration net-new = 43/53 and net-new ∩ stakeholders-present ∩ detect_gap_pattern-fails = 29/41 exactly. Case (i): the 944 read undetermined(no_seats) under source (a), never silent 0. Case (ii) REFINEMENT: the 29/41 split three-valued (haiku 4 undetermined + 25 no_gap; flash 12 + 29) — the doc's "uniformly insufficient" premise was imprecise; none silent 0. Negative controls same-run both cases.
OQ-197 fix witnessed end-to-end; only (5) R4 recompute remained (held on the detector_calibration proposal ruling). Evidence: `audits/2026-07-01_oq197_acceptance_controls/`.

---

## 2026-07-01 — OQ-197 consumer wiring landed (4 live sites, labeled); detector_calibration.pl is UNTRACKED/unwired WIP
**Files:** prolog/json_report.pl, python/shared/schemas.py, python/query.py, python/tensions_ledger.py, prolog/detector_calibration.pl
**Tier:** tripwire

Commit `fffca9d1`: the OQ-197 three-valued `gap_status` wired through every live read site with
the human-readable LABEL (json_report fields + corpus counts; `query.py --detail` incl. a latent
`len(None)` crash fix; tensions_ledger line; enhanced_report `build_omega_section` — a 5th site
first cleared WRONG by a proxy grep, caught by the operator's question). Witnessed at the JSON
boundary (exit 0, mtime advanced; behavior preserved 57/57; 0 consistency violations). The
detector_calibration.pl UNTRACKED tripwire here is SUPERSEDED by the 2026-07-01 R4-ruling entry
(tracked-but-unwired; warning in the module header).

---

## 2026-07-01 — OQ-197 (a)/(b) cross-tab: canonical (b) ≡ h1_band, stakeholder (a) distinct; canonical-source bug fixed
**Files:** prolog/report_generator.pl, audits/2026-07-01_oq197_source_h1_crosstab/
**Tier:** correction-key

Commit `6bda83ec`: `gap_status`/`detect_gap_pattern` made source-explicit. Canonical (b) is
EXACTLY coextensive with `h1_band>0` (0 off-diagonal on the both-determinate testsets+twins n=1197
— definitional duplicate); stakeholder (a) distinct on 36/1197. Twins are correlated (one
independent corpus + one pair): non-redundancy ONLY; reliability = OQ-199. RULING (a): keep the
stakeholder source (no code change). Correction to `b616e625`: its canonical seat clause used
`constraint_classification/3` with an UNBOUND context → 0 seats, a dead unwitnessed branch — fixed
to `drl_core:dr_type/3` via `standard_context_for_power/2`; a branch dead under the default config
still needs its own witness. Evidence: `audits/2026-07-01_oq197_source_h1_crosstab/`.

---

## 2026-07-01 — OQ-197 three-valued gap operability CONTRACT landed (branch, behavior-preserving); 6th consumer found
**Files:** prolog/report_generator.pl, prolog/tests/test_gap_operability.pl, python/tensions_ledger.py, python/json_report.pl, prolog/detector_calibration.pl
**Tier:** landed

Branch `oq197-three-valued-gap-operability`, commit `b616e625`: `report_generator:gap_status/2` → gap | no_gap | undetermined(no_seats|single_seat|single_power_position), closing the Pattern-6 collapse in the gap detector; source-parameterized (`gap_seat_source/1`, default stakeholder) so the (a)/(b) ruling is a one-line change; `detect_gap_pattern/2` firing UNCHANGED (57=57 diff-empty); gap_status total/deterministic 119/119 (gap=57 no_gap=32 undetermined=30); 9 two-sided plunit controls pass; 0 new corpus-suite failures.
Finding: `tensions_ledger.py:131` is a SIXTH consumer with its own bug — it computes index-mismatch from `perspectives` and counts `unknown` as a diverging value (needs repointing to gap_status or an unknown filter). Full 6-site consumer map + sequencing: ISSUES.md OQ-197 (Progress 2026-07-01).

---

## 2026-06-30 — detector self-assessment: Slice A (author×engine cross-tab) LANDED; Slice B (calibration omega) proposal awaiting ruling
**Files:** prolog/routing_sink.pl, docs/design/detector_calibration_omega_proposal.md, outputs/routing_sink.json
**Tier:** landed

From the Elias-Thorne report review, the "is the detector calibrated" question split three ways. Slice A LANDED (`routing_sink.pl`, commit `f6921ac1`): `author_engine_crosstab(_summary)` added to `routing_sink.json` — (authored_type × engine_type) confusion cross-tab over per-seat `seat_diff`; hard label SEAT-AGREEMENT, NOT calibration (divergence_rate 0.77 is a two-seat disagreement rate, never a detector FP rate); positive control reconciled (diagonal 91==no_route; 396+36+44=476=119×4; an unbound-key Pattern-5 vacuous guard caught pre-ship, nonvar/2 added).
Slice B PROPOSAL awaiting ruling (`docs/design/detector_calibration_omega_proposal.md`, commit `c4864999`): a `detector_calibration` omega the engine MINTS OPEN but never closes, typed as an Ω_E + Ω_P PAIR; R1–R4 are the operator's seat, NOT wired/fired. (C) auto-closing the verdict = category error (no ground truth; seat theorem).

## 2026-06-30 — perspective_chi d/f_d fork fixed (resolved-context derivation); report frame added
**Files:** prolog/constraint_indexing.pl, prolog/json_report.pl, python/enhanced_report.py
**Tier:** landed

Fixed the d/f_d fork (`6d1df7d1`): `write_one_perspective_chi` derived d/f_d on the UNRESOLVED canonical power atom while chi coalition-resolves internally — 40/119 live constraints had a `powerless` row where chi ≠ ε·f_d·σ (surfaced by web-Claude; both its hypotheses falsified — d is observer-position-keyed, `constraint_indexing.pl:478-487 power_role_heuristic/4`). Fix: factored `constraint_indexing:agent_resolved_directionality/4`, used by BOTH the chi path and the JSON writer; behavior-preserving (0 type/chi changes; forked rows 40→0/440).
Also `5e5830df`: "HOW TO READ THIS REPORT" frame prepended to `enhanced_report.build_header` (seats surface; divergence is the finding; RED = authored direction, OQ-187). Tripwire: any NEW consumer reporting d/f(d) beside chi must derive via `agent_resolved_directionality/4`, never `derive_directionality/3` on the raw canonical context — else the fork reopens silently.

## 2026-06-30 — OQ-38 RESOLVED: reproducible orphan-xref tool built; four calibration orphans stripped; OQ-196 minted
**Files:** prolog/orphan_xref.pl, python/audits/oq38_orphan_sweep.py, prolog/drl_composition.pl, prolog/utils.pl, ISSUES.md, AGENTS.md, audits/2026-06-30_oq38_orphan_xref/
**Tier:** landed

OQ-38 RESOLVED: reproducible tool-native funnel replaced the discredited 2026-05-31 grep sweep — `prolog/orphan_xref.pl` (library(prolog_xref); diagnostic, NOT a pipeline gate; conservative caller matching) + driver `python/audits/oq38_orphan_sweep.py` (self-exclusion gotcha witnessed + fixed). Funnel over 121 sources: 614 exports (grep undercounted by 86), 201 STATIC_ORPHAN, 29 dynamic-masked, M=170 real-orphan upper bound. Stage-1 hard gate: `cs_reference_frame/2` LIVE (the OQ-35 adversarial case); `non_monotonic_trajectory/2` LIVE in `metric_drift_report.pl` (stale `drift_report.pl:164` cite corrected in ISSUES.md).
Four calibration orphans stripped (commits `736783e4`, `6a3acf1d`; tool `c9be12ca`) — behavior-preserving (load gate exit 0; validation suite byte-identical; per_constraint sha256 `d9c85bec…` unchanged, mtime advanced). Cascade: `safe_get_category/3` newly orphaned → OQ-196 minted (value-adjudicate the M=170), NOT stripped. Writeup: `audits/2026-06-30_oq38_orphan_xref/WRITEUP.md`.

## 2026-06-30 — OQ-37 RESOLVED (read-but-unauthored metric census re-dispositioned); GAP-23 minted
**Files:** ISSUES.md, docs/design/design_gaps.md, prolog/data_validation.pl, python/generate_constraint_pl.py
**Tier:** landed

OQ-37 RESOLVED at its root: all six read-but-unauthored `constraint_metric` names trace to the fixed compiler emit set (`generate_constraint_pl.py:608-635`); authoritative cross-corpus census — all 6 are 0 facts on testsets/haiku/flash/kernel_v1 = 3,142 stories, controls fire on every leg (`audits/2026-06-30_oq37_census_redispose/`). Dispositions routed (OQ-93 grids; χ-partition closed `3ab3ace4`; helpers → OQ-38; supp/ε-floor → OQ-48); the two genuine deferred capability livens (`sunset_time`, `internalization_depth` + never-loaded `psych_bridge`) → GAP-23 (priced, operator-seat).
One behavior-preserving edit (`5b7a8b95`): dropped never-authored `resistance_to_change` from the `data_validation.pl` extreme-value monitor (provably byte-identical, validation-channel only). Correction-key (OQ-64 instance): `resistance` ≠ `resistance_to_change` — distinct referents (`grid_first_contact_gate.py:48`); the proposed `metric_drift_events.pl:174,247` repoint was DECLINED (`safe_metric/3` fails silently, repoint buys zero behavior while baking a wrong-metric identification) — liven both detector inputs together (GAP-23) or leave dark; never repoint by name-stem.

## 2026-06-30 — OQ-27 RESOLVED (signature-resolved H¹ disclosure); OQ-195 minted (general-n gap)
**Files:** prolog/grothendieck_cohomology.pl, docs/deferential_realism_paper_v6.13.1.md, ISSUES.md, issues/INDEX.*, CLAUDE.md, KNOWN_STATE.md
**Tier:** correction-key

Ruling: disclosure, not redefinition — the engine already computes H¹ over the SIGNATURE-RESOLVED
`dr_type` orbit; precision landed in v6.13.1 + a `grothendieck_cohomology.pl` comment (append-
versioning; v7 Thm 7 already carried it). Witnesses (manifest 2026-06-30T00:08:22Z, n=116): 65/86
four-real-seat at H¹>0; 116/116 orbit-reproduction control. The rule is PROMOTED to CLAUDE.md
Architecture Invariants. Discovery: Theorem 2's gap was proven only at |real seats|=4
(n=3→{0,2,3}, n=2→{0,1}; the four h1_band=2 constraints are 3-real-seat under the OQ-51 N/A rule,
not counterexamples) → OQ-195 minted (general-n law; resolved 2026-07-02). Cite the stale-range
flag by predicate header, never `grothendieck_cohomology.pl:158`.

## 2026-06-30 — OQ-194 RESOLVED: embedded mountain/nl "failures" are correct commentary; one rotted phantom fixture fixed
**Files:** ISSUES.md, prolog/tests/test_phantom_neighbor_filter.pl, python/generate_constraint_pl.py, prolog/testsets/*.pl (16 claim=mountain files), KNOWN_STATE.md
**Tier:** correction-key

The 21 corpus-suite fails were two unrelated things: 20 embedded mountain_threshold/nl_profile
validation units CORRECTLY commenting that claim=mountain stories lack true-mountain metrics
(claim ≠ actual — not regressions; the red-as-signal reading is conditional on these staying NON-
GATING, cf. OQ-116's linter analog), and 1 genuine fixture-rot defect: the phantom-filter
positive-control names rotted out at the 2026-06-05 reset, so the OQ-95 guard passed VACUOUSLY —
fixed by self-selecting targets + throwing `insufficient_real_targets` on under-supply (silent rot
now unreachable). Generator now emits the explanatory comment (backfilled into all 16
claim=mountain testsets; header signpost in `test_phantom_neighbor_filter.pl`); hardcoded bars →
OQ-48. OQ-194 closed (two commits).

## 2026-06-29 — OQ-23/OQ-24 RESOLVED (narrow same-kernel contamination guard); OQ-193 deferred
**Files:** prolog/drl_purity_network.pl, prolog/tests/test_coexists_fpn_canary.pl, prolog/giant_component_analysis.pl, ISSUES.md, audits/2026-06-29_oq23_coexists_fpn_canary/
**Tier:** landed

OQ-23/OQ-24 RESOLVED: the positive-controlled canary (`prolog/tests/test_coexists_fpn_canary.pl`) FALSIFIED the premise it was built to backstop — the `coexists_with` "zero contamination by definition" exclusion was ALREADY VIOLATED on every populated leg (testsets/ 2, haiku 178, flash 361, kernel_v1 662) via the authored `affects_constraint` side channel (the DP-001 ε-linkage instruction; forecloses leaked likewise); only FPN `effective_purity` and the coupling baseline reach a shipped product.
FIX: same-kernel-donor guard as first clause of `compute_edge_contamination/7` — contamination-LOCAL by design, giant_comp topology deliberately untouched so OQ-193 (sibling-strip collapses the giant 334→70 on kernel_v1 — unsettled Ω_C ruling) can be ruled on its own evidence; coupling-baseline ship noted in HOLD_FINDINGS. Witnessed: leaked 2→0 live (forecloses 1→0), cross-leg post-fix leaked=0, connectivity zero-change control, plunit regression gate GREEN. Tripwires file-local in `drl_purity_network.pl` (do NOT extend the guard into `constraint_neighbors_existing/2` without OQ-193). Evidence: `audits/2026-06-29_oq23_coexists_fpn_canary/`.

## 2026-06-27 — OQ-124/OQ-149 committer-axis convention control: A=SIGNAL, B=CONVENTION, C=OPEN
**Files:** ISSUES.md, prolog/signature_detection.pl, python/story_repair.py, agent/run_no_scope_gemini.py
**Tier:** landed

Ran the OQ-70-style bait-confound control on the three cross-model-divergent fields, per-field pre-registered (`audits/2026-06-27_oq124_oq149_committer_convention_control/`); twins re-classified at one commit `bbf5c92` (on-disk outputs had straddled 20fab78/8126231); positive controls held. Verdicts: Field A (CHE↔FCR signature fork) = SIGNAL — ~13:1 asymmetric, dominant lean a continuous extraction-magnitude difference (ext Spearman 0.86, flash systematically lower; two-sided `with_retracted` control discharged) → signature lean carries a model index (v8 §3/OQ-72). Field B (`cs_reading_relation`) = CONVENTION — fails to covary with settled substrate on disagreeing slots → needs a provenance bucket (precedent `becd0f87`). Field C (`overridden` 51-vs-4) = OPEN-pending-instrumentation.
Enrichment: `overridden` is coercion-invariant (missing `cs_axiom_status` KeyErrors generation, `generate_constraint_pl.py:672` — NOT silently defaulted; the `contested/foreclosed→holdable` remap `story_repair.py:89-90` IS silent — needs raw pre-repair capture via `story_repair._normalize_axiom_status` cid logging). Third-model spend warranted (A=signal), operator-gated.
**Files:** python/cohort_stability.py, python/cohort_sigma_seat_eval.py
**Tier:** tripwire

A `stable`/`match` verdict in a per-field comparison table can mean three things, two hollow:
content reproduced, presence-only matched, or constant field — aggregating without splitting
silently inflates "stable" and can INVERT a partition statistic (worked instance, the OQ-118 re-
probe: removing presence-hollow fields dropped consistency 47.9%→39.7%; the degeneracy sweep
caught four constant "stable" fields). Rule: witness what each comparator actually compares (read
the extractor, not the column name) + run a between-item variance check. The per-comparator face
of Build Discipline Patterns 5/6 — always-loaded form exists, deliberately NOT promoted. Witness +
re-runnable probe: `audits/2026-06-27_oq118_reprobe/` (commit `fc57e833`); ruling `82c0693c`.

---

## 2026-06-27 — OQ-182 family product SHIPPED: trajectory serialized + trajectory_enabled 0→1
**Files:** python/run_pipeline.py, prolog/config.pl, CLAUDE.md, AGENTS.md, ISSUES.md
**Tier:** landed

Flipped `config.pl:571 trajectory_enabled` 0→1. Root cause of the intermittent flag=1 stall: concurrency memory pressure — the O(N²) `trajectory` (HAC) stage co-resident with O(N²) `giant_comp` in the 4-worker Phase-2 pool (NOT a giant_comp bug; OQ-77). Fix (surgical, Python-only): `run_pipeline._phase_prolog` pulls `trajectory` out of the parallel tasks and runs it sequentially after `_run_parallel`; the 11 remaining real stages stay parallel; order correctness-irrelevant (C0: `context_profile_report.md` has no downstream consumer).
Witnessed (`audits/2026-06-27_oq182_trajectory_serialization/`): ps/RSS sampler captures PRE-FIX co-residency vs CURED disjoint windows; N=10 liveness battery 10/10 GREEN; freshness positive control PASS; C0 zero classification diff; 300s timeout held (≥175× margin); `validate_config` PASS at flag=1. Tripwire promoted to CLAUDE.md (Running the System): never re-fold trajectory into the parallel tasks list.

---

## 2026-06-26 — OQ-91 resolved: commentary-grade repair-transition detector + report surface
**Files:** prolog/transition_paths.pl, prolog/json_report.pl, python/enhanced_report.py, docs/repair_dynamics.md, ISSUES.md
**Tier:** landed

OQ-91 resolved: new `repair_transition/4` in `transition_paths.pl` — the upward dual of the 8 decay heads (transitive closure of decay edges read backwards, `unknown` excluded; reuses `degradation_chain/3`), 4th arg a named op (maintain/splice/replace rope line-ops; scaffold_struck). COMMENTARY-GRADE — must never feed `classify_from_metrics/6`, the signature layer, or `verdict_join`. Serialized as the `repair_transitions` per-constraint field (`json_report.pl`, hermetic globals wrapper), rendered by `enhanced_report.build_repair_section` (silent on decay-only = honest absence). Doc: `docs/repair_dynamics.md`.
Witnessed (`audits/2026-06-26_oq91_repair/`): B1-scan non-empty (testsets/ 2, kernel_v1 30, incl. multi-step homoousios/versailles); B4 invariant PASS (classification fields byte-identical). Bug found+fixed: `repair_op` clause selection must key on from/to/pre, not a bound 4th arg. No promotion (wiring repair into classification would fail LOUD).

## 2026-06-26 — OQ-182 C-gen: family product is generation-EXPRESSIVE (A4 flip still operator-gated)
**Files:** prolog/config.pl, prolog/context_profile_mining.pl, ISSUES.md, audits/2026-06-25_oq182_trajectory_revive/
**Tier:** correction-key

A1/C0 PASS (flag flip changes only the config echo; classification byte-identical, positive-
controlled); A3 C-prov PASS on kernel_v1 (1106). A2 C-gen FAILED its locked bar (haiku↔flash
family ARI=0.117 < 0.50); operator ruled option-2 re-specify (no laundering): the granularity-
insensitive substrate read gives TRACK=162/162 — every inter-leg family split is backed by a real
fingerprint_shift difference, zero cut-height artifact. Dual finding stands: the global partition
does NOT recover across generation AND that failure is generation-EXPRESSIVE, not clustering
noise. A4 stayed operator-gated here — SUPERSEDED by the 2026-06-27 ship entry (trajectory_enabled
0→1 with the serialization fix). Evidence: `audits/2026-06-25_oq182_trajectory_revive/`.

## 2026-06-26 — OQ-104 resolved (scoped): gate.sh gains a 7th check (audit-citation frozen-evidence)
**Files:** scripts/gate.sh, python/audit_citation_status.py, audits/2026-06-18_oq104_citation_checker/controls.py, audits/2026-06-18_oq104_citation_checker/controls_run.sh, ISSUES.md
**Tier:** landed

Operator ruled: gate the OQ-104 danger class by REGENERABILITY. `audit_citation_status.py:classify()` splits untracked cited paths into `untracked-frozen-evidence` (non-`outputs/`; GATING intrinsic ERROR) vs `untracked-regenerable` (top-level `outputs/`; non-gating WARN); `scripts/gate.sh` gains the 7th check `audit cites` (`--check` exits 1 iff frozen-evidence non-empty or parse problems). Controls 23/23 → 25/25 incl. a matched-pair isolating the prefix as the deciding variable; witnessed RED-on-frozen / GREEN-on-removal; full gate 7/7 GREEN (all 39 untracked paths under `outputs/`).
Scope (do not over-read "resolved"): one of two origin routes mechanized; two residuals stay non-gating with kill conditions in ISSUES.md OQ-104 (a typo'd path lands `missing-pending-M`; a frozen artifact parked under `outputs/` reads regenerable). Controls: `audits/2026-06-18_oq104_citation_checker/controls.py` + `controls_run.sh`.

## 2026-06-26 — GAP-04/OQ-53 increment: cross-kernel reading-stance transpose (fingerprint_shift spine)
**Files:** prolog/cs_kernel_registry.pl, prolog/json_report.pl, python/cross_kernel_stance_report.py, prolog/tests/test_cs_kernel_registry.pl, docs/design/design_gaps.md, ISSUES.md
**Tier:** landed

Built the reading-stance transpose GAP-04 named absent (OQ-53's close had deferred it): `cs_kernel_registry.pl` gains `declared_stance/2` (THE SEAT — hand-declared cohort table), `reading_stance/2` (declared-only authority; morphology never a fallback), `stance_cohort/2`, `cross_kernel_stance_profile/2` (per-position majority consensus, convergent/divergent partition, verdict WITH cohort provenance) + report/JSON export; `json_report.pl` now serializes `fingerprint_shift` per per_constraint entry (0 pre-change → 104 post-run); consumer `python/cross_kernel_stance_report.py` → `outputs/cross_kernel_stance.{json,md}`.
Cohort DECLARED, not derived (Seat-Theorem Cor 2b): morphology unreliable both ways on the 7-member abolition cohort (exact-stem 4/7; substring over-admits a *rejection* of abolitionism). Witnessed both twins: abolition convergent 5/7 on BOTH (draw-stable); deterrence flips across twins (seat-expressive); read the split as a σ/seat partition. Pinned by 5 corpus-free `transpose_*` tests in `test_cs_kernel_registry.pl`; the pre-existing `divergence_silent_at_observed_agreement_context` failure is documented archive-draw fragility, not this change. Provenance: ISSUES.md OQ-53 addendum, GAP-04 status.

## 2026-06-26 — OQ-21(b) CLOSED as a recorded design absence: the single-instance barrier is the module-collision, not DP-001
**Files:** ISSUES.md, prolog/corpus_loader.pl, prolog/config_validation.pl, prolog/json_report.pl
**Tier:** correction-key

Corrects two prior OQ-21(b) framings by RUNNING it (two real abolition_reading draws co-loaded
from `archives/datasets/kernel_test/`): the operative single-instance barrier is the per-story `:-
module(constraint_<name>,[])` collision — the second file is silently dropped at exit 0 and DP-001
never fires; DP-001 is the complementary observer-axis one-ε seal (fires exit 1 on a renamed-
module chimera load, OQ-25 message witnessed). (b) CLOSED as a declared design absence: A12's
trigger set (one name → N UIDs, ONE ε) has no demonstrated populator — stochastic generation gives
each draw a different ε (OQ-26/Axiom 2), exactly the chimera DP-001 rejects. Reopen condition: a
generation mode that canonicalizes ε per reading. Shipped test:
`prolog/tests/test_a12_multi_instance_render.pl`; no code change.

## 2026-06-25 — OQ-21(a) RESOLVED: A12 multi-instance selector — dead recency clause fixed, @< pinned
**Files:** prolog/json_report.pl, prolog/tests/test_a12_multi_instance_render.pl, ISSUES.md
**Tier:** landed

OQ-21(a) RESOLVED (`cfb5fa03`): the documented pick-latest-by-`cs_created_at` path in `write_per_constraint_entry/4` was DEAD — `aggregate_all(max(T-U),…)` evaluates `T-U` arithmetically, throws on atom UIDs, swallowed by `catch/…fail`, so selection was always the `@<` msort/last fallback for the branch's whole life. Operator RULED `@<` canonical (instances are parallel draws, not versions — no canonical-latest; sole live consumer `orbit_operator.py` needs determinism, not timestamps); dead clause removed; test pins `@<` + bundle coherence, positive control witnessed RED under reintroduced recency.
Reusable tripwire: the `aggregate_all(max(Key-Val))` argmax idiom evaluates Key-Val arithmetically and throws on non-numeric keys — a surrounding catch silently degrades to the fallback; witness BOTH arms. (b) left open gated on a future multi-instance load (OQ-17 pointer stale — disposed); `[GATE]` GREEN.

---

## 2026-06-25 — OQ-19 RESOLVED: drift-trajectory trigger thresholds made durable + fail-loud
**Files:** python/enhanced_report.py, python/tests/test_drift_trajectory_granularity.py, ISSUES.md
**Tier:** landed

OQ-19 RESOLVED (single-file, behavior-preserving): the 6 `build_drift_trajectory_section` thresholds hoisted into a named `_DRIFT_*` block keyed to `_DRIFT_MEASUREMENT_GRANULARITY = 0.01` (Trigger A derived, IEEE-754 byte-identical to the literal — witnessed); `_series_granularity` guard prepends `[CALIBRATION WARNING]` on finer-than-floor series (positive-control test `python/tests/test_drift_trajectory_granularity.py`).
Premise correction worth a cold read: "live data is 2-decimal" was FALSE — 4 constraints already author 3-decimal values, so the feared finer-granularity regime is partly present (guard currently inert: 29 rendered sections, 0 warnings). Witnesses (float kill-condition, grep 7→0, per-trigger A/B/C diff, positive control) in the ISSUES.md OQ-19 resolution block. History-only; no promotion.

---

## 2026-06-25 — OQ-182 C-null PASS: HAC structural families validated as MEANING-bearing (testsets/ leg)
**Files:** audits/2026-06-25_oq182_trajectory_revive/c_null_harness.pl, audits/2026-06-25_oq182_trajectory_revive/c_null_results.log, audits/2026-06-25_oq182_trajectory_revive/c_null_distribution.json, audits/2026-06-25_oq182_trajectory_revive/c_null_protocol_FROZEN.md, audits/2026-06-25_oq182_trajectory_revive/c2_domain_finding.md, ISSUES.md
**Tier:** landed

C-null PASS — HAC structural families validated MEANING-bearing on the testsets/ leg (no engine edits; `trajectory_enabled` stays 0; plan `~/.claude/plans/bright-jumping-cocke.md`): RealSil 0.161119 (97 constraints, 11 families) > P95(null) −0.026436 over 200 per-component-independent shuffle draws; 0/200 null draws reach real; TEETH PASS (+5.01σ); null centers 15 families vs real 11 (conservative direction); reproducible under seed `20260625`. All controls pasted BEFORE the verdict (INTERNAL-CHECK, GROUPING-FIDELITY, FIDELITY, JOINT-TOOTHLESS — demonstrating the false-PASS the per-component design avoids — TIE-BREAK).
MECHANISM CORRECTION (frozen quantities unchanged): the frozen "Chimera surgery map" was mechanically wrong — `group_by_shift/2` keys on constraint identity, ignoring `trajectory_cached`, so the harness builds shift-groups itself; erratum recorded in `c_null_protocol_FROZEN.md`. Scope: twins remain OPEN (near-vacuous cross-domain gate; deferred to rebuild); remaining legs C0/C-gen/kernel_v1 then the gate flip. Evidence: `audits/2026-06-25_oq182_trajectory_revive/` (c_null_harness.pl, c_null_results.log, c2_domain_finding.md).

---

## 2026-06-25 — OQ-182 minted: revive + validate the dormant HAC trajectory-mining subsystem (cheap tier)
**Files:** prolog/context_profile_mining.pl, prolog/config.pl, prolog/isomorphism_engine.pl, prolog/constraint_bridge.pl, prolog/report_generator.pl, ISSUES.md, audits/2026-06-25_oq182_trajectory_revive/
**Tier:** landed

OQ-182 minted — cheap tier of the trajectory-revive plan (`~/.claude/plans/fancy-splashing-pancake.md`; the plan's "OQ-180" label was already taken by the OQ-51 build, commit `cef5dc6e`). C-prov PASS on testsets/ (witnessed, `c_prov_runtime.log`): `trajectory_run/2` (97 trajectories → 11 families, 448 twins) leaves both `classify_at_time_*` globals UNSET — no imputed BaseX coupling; positive control sensitive, but note the OQ-178 trap hit live: the control only proves sensitivity when fed a Time ON the constraint's authored grid. C-prov re-runs on kernel_v1 in the spend tier.
Fork verdict: `context_profile_mining:cross_domain_twins/3` is canonical; `isomorphism_engine.pl` is a loaded-but-non-executing Pattern-2 fork (all callers dead; positive-controlled grep) — NOT deleted, see `design_gaps.md` GAP-20. Spend tier (C0/C1/C2/C3/C-null, gate flip) operator-gated. Audit: `audits/2026-06-25_oq182_trajectory_revive/`.

## 2026-06-25 — OQ-51 main build: `unknown` is N/A on the canonical sheaf/H1 path (commits `f8ae0c9c` + `15cca7ed`)
**Files:** prolog/grothendieck_cohomology.pl, prolog/sheaf_analysis.pl, prolog/json_report.pl, prolog/product_site_export.pl, python/shared/schemas.py, python/shared/loader.py, python/w1_sheaf_join.py, python/enhanced_report.py, python/orbit_characterization.py, python/run_drift_mismatch.py, python/sweeps/epsilon_sensitivity.py, python/sweeps/range_sweep.py, python/sweeps/product_site_delta_sweep.py
**Tier:** tripwire

OQ-51 main build (commits `f8ae0c9c` + `15cca7ed`; the cs_kernel_comparison site was `f456896b`):
`unknown` is N/A — `h1_band` NULLABLE (null = undetermined, <2 real seats), `sheaf_status` gains
`undetermined` via two routes (`sheaf_undetermined_reason`); partition manifest⟺h1>0,
genuine/fragile⟹h1==0, undetermined⟹h1∈{null,0}. All standing tripwires PROMOTED to CLAUDE.md
Architecture Invariants (OQ-51 block); the maxent/arakelov route-2 ordering hazard is carried by
`docs/technical/sheaf_status_maxent_ordering.md` + `tests/test_sheaf_na.pl` + w1_sheaf_join
Control 2b. Witness: test_sheaf_na 10/10; live route-1=15, route-2 dormant; 0 partition violations
on haiku(960)/flash(960)/kernel_v1(1106); branch `oq51-sheaf-na-canonical`. Residuals: OQ-180,
OQ-181.

---

## 2026-06-25 — fix: OQ-57-class wrong-qualifier in the dormant trajectory-mining path (commit `fc9b4688`)
**Files:** prolog/context_profile_mining.pl, prolog/check_stack.pl
**Tier:** landed

Fixed the OQ-57-class rotted qualifier in the dormant trajectory-mining path (commit `fc9b4688`, surfaced during the OQ-16 rename, rename-independent): `standard_contexts/1` called `dirac_classification:standard_context/1`, deleted 2026-06-02 (`dirac_classification.pl:115`) — re-qualified to `drl_core:standard_context/1` (identical 4-context generator, verified). Witness: the report generator on run_pipeline's exact load chain now exits 0 producing a 135-line report (was crash → empty); production unchanged (`trajectory_enabled=0`).
Why unnoticed: `context_profile_mining.pl` is NOT loaded by `[stack]`, so `check_stack.pl` never saw it. Gap closed (commit `a82d7ed0`): check_stack now loads the trajectory chain faithfully — positive-controlled, baseline unchanged (same 5 known undefineds); honest boundary recorded in-file (other standalone report chains remain uncovered). Validating/reviving the subsystem became the OQ-182 arc.

---

## 2026-06-25 — OQ-16 RESOLVED: temporal vocabulary rename pass (name-only, 5 renames, 3 commits)
**Files:** prolog/metric_drift_events.pl, prolog/metric_drift_report.pl, prolog/context_profile_mining.pl, prolog/context_profile_report.pl, prolog/network_dynamics.pl, prolog/stack.pl, prolog/drl_lifecycle.pl, prolog/transition_paths.pl, prolog/cs_pattern_detection.pl, prolog/cache_registry.pl, python/run_pipeline.py, scripts/pipeline_dashboard.sh, ISSUES.md
**Tier:** landed

OQ-16 RESOLVED: name-only rename pass (no logic/threshold moved), 5 renames in 3 commits — `0a204af1` (`detect_network_drift/3` → `detect_network_contamination/3`), `1d861cee` (file+module renames drift_events→metric_drift_events, drift_report→metric_drift_report, trajectory_mining→context_profile_mining, trajectory_report→context_profile_report + output path `context_profile_report.md`), `1bcc07c5` (doc code-pointer tokens); doc-scope refinement `76eae0c1` (4 dated recon/essay docs keep bodies as dated records + per-doc end-notes); close-out `fb45c0e3`. Operator ruling: `metric_*` over `dr_*`.
Deliberately out of scope (logged): JSON output field `drift_events`, internal `run_trajectory_report`, doc filenames. Final-grep exclusion list recorded (full entry in git history): remaining old-token hits in the drift_events JSON surface, the 4 historical docs, and verbatim review transcripts are intentional-preserved, not missed renames. Witness: `[stack]` loads, check_stack clean, full run_pipeline exit 0 writing the renamed path. Side-finding (fixed separately, `fc9b4688`/`a82d7ed0`): the dangling `dirac_classification:standard_context/1` call — see the entry above + `swipl_load_path_and_probe_gotchas.md` §1. Pass interleaved with concurrent instances on main; outcomes converged (multi-writer hazard per CLAUDE.md).

---

## 2026-06-25 — OQ-39 RESOLVED: scaffold rising-suppression gets a COMMENTARY verdict (rows 14–18 disposed)
**Files:** prolog/cs_pattern_detection.pl, prolog/tests/test_oq39_scaffold_escalation.pl, ISSUES.md
**Tier:** tripwire

OQ-39 row 14 resolved by COMMENTARY, not gate-vs-drop (operator ruling): new `cs_verdict(C,
scaffold_suppression_escalating)` (commentary-grade, annotate-only) fires on a scaffold
certification + rising authored suppression series — 14 live firings witnessed (independent probe
agrees); cross-leg rising:falling ≈ 5–6:1, i.e. the generation prompt's own suppression-declines
rule is systematically not honored (strengthens the commentary case). Rows 15–18 closed (no
validator / enforcer exists via `coordination_dead/1` / diagnostic-only / linter surface). The
clause placement/cut gotcha (an orthogonally-gated cs_verdict clause MUST be FIRST + `once/1`, no
trailing `!`) is covered in-code at `cs_pattern_detection.pl` (~:200) + the cut-regression control
in `tests/test_oq39_scaffold_escalation.pl`; `metric_trend/3` is time-independent, moot to OQ-178.

## 2026-06-25 — OQ-178/179 SUPERSEDED/RESOLVED: cs_kernel_divergence reverts to static `dr_type/3` (time-neutral)
**Files:** prolog/cs_kernel_registry.pl, prolog/json_report.pl, ISSUES.md
**Tier:** tripwire

`cs_kernel_divergence/4` + the `compare_kernel_readings/3` JOIN reverted to static `dr_type/3`
(commit `5b069ae1`, reverting the interim OQ-178 probe-fix `9fde36c9`): a `cs_*` cross-reading
comparator's moving axis is reading, NOT time — never wire it to `measurement/5`/
`classify_at_time` (latest-snapshot reads a collapsing constraint at its terminus;
unknown==unknown masks real divergence). NOT promoted — the warning is covered at the edit site in
the `cs_kernel_registry.pl` header. Witness: live divergence 16→18 genuine pairs; twins
corroborate (haiku 861→893, flash 813→846); zero unknown-pairings (the OQ-37 artifact did not
occur). OQ-179 closed mis-premised, its DR-axis observation re-homed to the DR temporal subsystem
(OQ-110 family); the OQ-105 BC-encoding fold is moot for this path.

---

## 2026-06-25 — OQ-51 build-extension RESOLVED: `unknown` is N/A in cs_kernel_comparison (trichotomy + divergence enumeration)
**Files:** prolog/cs_kernel_registry.pl, prolog/json_report.pl, python/enhanced_report.py, prolog/tests/test_cs_kernel_registry.pl
**Tier:** landed

Applied OQ-51's N/A rule (`unknown` = not-agree, not-diverge) to `cs_kernel_comparison` — the site the original build never enumerated (OQ-178 audit: all-unknown contexts scored `agree(unknown)`, inflating robustness). Verdict trichotomy `ctx_reading_verdict/2` agree/diverge/undetermined each carrying NUnk; LENIENT operator ruling (≥2 real readings ⇒ verdict over the reals); `cs_kernel_divergence/4` + `pair_reading_agreement/7` require BOTH types real via shared `is_real_type/1` — load-bearing for the join invariant Σ DivergeN == #cs_kernel_divergence, never refactor back to bare `\=`; Jaccard = null when no both-real context; JSON gains divergent/undetermined/abstaining_context_count + `divergence_patterns`; report renders the enumeration.
Witnessed: unit suite 20/20 (6 synthetic N/A controls + join invariant), partition invariant 9/9, `cs_kernel_divergence_count` 20→16 (actinide's only "divergence" was unknown-vs-real). Two silent footguns fixed (`~6f` threw on null Jaccard aborting the whole JSON write; `agree(_)` arity fail-match → RobustN=0).
Do not misread a no-op diff: the OQ-178 all-unknown inflation case fires on 0 serialized kernels (witnessed by the synthetic control + 13 non-serialized singletons excluded by the `L>=2` filter, `json_report.pl:1734`); the live serialized effect is the abstention-tolerant RISE (performance_legitimacy robust 21→147 — the ruling applied). Join invariant holds 9/9 live (the plan's 42/42 was the haiku twin). Scope: this surface only; the original OQ-51 `count_disagreeing_pairs`/`sheaf_status`/H1 sites stayed OQ-51's separate item. Console/OQ-119 count drops expected.

## 2026-06-24 — OQ-37..41 census Pass 1: 2 strips landed; OQ-41 BaseX=0.5 is off-grid, not absence; OQ-178 minted
**Files:** prolog/data_validation.pl, prolog/drl_composition.pl, prolog/cs_kernel_registry.pl, ISSUES.md, CLAUDE.md, audits/2026-06-24_oq41_basex_t0/
**Tier:** correction-key

Census Pass 1 (behavior-preserving strips, commit `1eacd2fc`): vacuous
`resistance_to_change`-keyed piton sub-check (OQ-90 supersession) + `predict_transformation/3`
stripped (orphaned helpers → OQ-38 candidates). Correction-key: the temporal path is LIVE, not
dormant — `classify_at_time` is consumed at Time=0 by cs_kernel_registry (+
temporal_residual/boltzmann_compliance/drl_core), and the OQ-41 BaseX=0.5 site is OFF-GRID
PROBING, not absence: fail-closing (the OQ-44 reflex, attempted then reverted) erased a real
snare-vs-scaffold divergence. Fixed via OQ-178 (`9fde36c9`, per-reading latest authored time;
divergence 17→20, invariant 42/42); trajectory successor OQ-179; OQ-39 row 14 reopened; the OQ-51
build-extension logged. The false-identical pipeline-diff tripwire (gate aborts before rewrite) is
PROMOTED to CLAUDE.md Running the System. Audit: `audits/2026-06-24_oq41_basex_t0/`.

---

## 2026-06-23 — OQ-15 RESOLVED (core): cross-axis taint guard LANDED, Phase 2 ruled policed-in-place
**Files:** prolog/check_axis_boundary.pl, prolog/axis_boundary_allowlist.txt, python/check_axis_boundary.py, python/run_pipeline.py, prolog/tests/axis_boundary_ctl_run1.pl, prolog/tests/axis_boundary_ctl_run2.pl, prolog/tests/axis_boundary_ctl_payload_widen.pl, prolog/tests/axis_boundary_ctl_nonbridge_seam.pl, scripts/gate.sh, ISSUES.md, docs/design/design_gaps.md
**Tier:** landed

OQ-15 core RESOLVED (closes GAP-12; v8 §8 item 1 / OQ-135 priority-1): static cross-axis reachability
guard (`check_axis_boundary.pl` + `axis_boundary_allowlist.txt`) wired into `scripts/gate.sh` + `run_pipeline.py`;
8 boundary edges censused, exactly one committer→observer `influences` bridge confirmed; Phase 2 RULED
policed-in-place, v7 synthesis PRESERVED (trigger: a SECOND bridge fires the guard RED). Commits `c6fe7edb`
(Phase 0a/0b) + `fd1ee561` (guard); witnesses `audits/2026-06-23_oq15_crossaxis_witnesses/` (incl.
`bc_rewitness.txt`). Bundled OQ-15 ↔ OQ-135.

---

## 2026-06-23 — OQ-06 RESOLVED: off-case fixtures witnessed for cs_drift_unacknowledged / cs_axiom_foreclosed
**Files:** prolog/cs_pattern_detection.pl, prolog/cs_axiom_engine.pl, prolog/narrative_ontology.pl, ISSUES.md
**Tier:** correction-key

All four off-case conjuncts witnessed in BOTH directions (two-sided planted controls per corpus +
a transient matched-pair matrix; evidence `audits/2026-06-23_oq06_offcase_fixtures/`). Carried:
the real definitions are `cs_pattern_detection.pl:412–416` and `cs_axiom_engine.pl:137–141`
(ISSUES stale-Files fixed in place); `cs_axiom/3` is multifile-but-STATIC (a probe assert needs a
dynamic declaration — fails LOUD, stays history); drift-C3 is a structural absence (no synthetic
fixture belongs in testsets/); sequential multi-corpus scans must be one-corpus-per-process.
Promotion test applied in-entry: nothing promoted (loud/local).

## 2026-06-23 — OQ-10 RESOLVED: reading-robustness as first-class report output (+ OQ-176 spawned)
**Files:** prolog/cs_kernel_registry.pl, prolog/json_report.pl, python/enhanced_report.py, prolog/tests/test_cs_kernel_registry.pl, ISSUES.md
**Tier:** landed

OQ-10 RESOLVED: `compare_kernel_readings/3` + `reading_robustness` object in `pipeline_output.json` +
`enhanced_report` kernel-reading section; witnessed on twin `end_of_life_decision_authority` (156 ctx → 73
robust / 83 specific, Jaccard 0.63/0.53/0.31; two-sided control passed). Commit `d2cb9bb7`. Verdict tokens
and field names SUPERSEDED 2026-06-25 by the OQ-51 trichotomy (see that entry). OQ-176 spawned
(`cohomological_obstruction/3` returns H1=0 for an ABSENT constraint — Pattern-5, logged not patched).

---

## 2026-06-23 — OQ-112 RESOLVED (close-out): arc is latent-hardening, structurally latent across all three live legs
**Files:** ISSUES.md, audits/2026-06-23_oq112_closeout/
**Tier:** landed

OQ-112 RESOLVED (close-out, no engine edits): only item 1 touched live output (13/92 abductive
`agrees`→`unavailable`, headline-neutral); items 2/4/7 latent-hardened; items 3/5/6/8 declared-not-landed
(fix-shapes recorded). Masking is STRUCTURAL — 0 live bites on all three legs (testsets/haiku/flash);
archives not swept (declared boundary, OQ-89 pattern). Two reusable tripwires (guard-count over-reports a
Pattern-6 firing; latent-on-92 ≠ latent engine-wide) in `audits/2026-06-23_oq112_closeout/`.

---

## 2026-06-23 — OQ-112 item 4 RESOLVED (Round 3, Commit 1 alone): maxent-local accessors fail-closed; Commits 2/3 falsified
**Files:** prolog/maxent_classifier.pl, docs/design/design_gaps.md, ISSUES.md, audits/2026-06-23_oq112_round3/
**Tier:** landed

OQ-112 item 4 RESOLVED (Commit 1 alone): the four maxent-local accessors return `unknown` on metric absence
instead of fabricated `0.0` (+ `number/1` guard in `maxent_threshold_proximity/4`); blast radius contained to
`maxent_classifier.pl`; LATENT on 92 (0 sentinels live — not a live catch). Round 0 falsified Commit 2 (loud
throw; item-2 gate already floors it) and Commit 3 (dissolved into 1; `maxent_boundary_analysis` → GAP-19 in
`design_gaps.md`). Evidence: `audits/2026-06-23_oq112_round3/`; Round-4 gate installed in the ISSUES OQ-112 entry.

---

## 2026-06-23 — OQ-112 item 7 RESOLVED → ROUND 2 COMPLETE: wasserstein incomparable-mass provenance tokens
**Files:** prolog/json_report.pl, python/shared/schemas.py, ISSUES.md, audits/2026-06-22_oq112_round2/
**Tier:** landed

OQ-112 item 7 RESOLVED → ROUND 2 COMPLETE: `json_report.pl:438–442` three-states-into-`0.0` collapse replaced
by `wm_token/3`/`wm_emit/3` (float | `null` | `"errored"`, + unbound-M fourth-state guard); output-identical on
the live 92 (344/344 cells genuine float; absent/errored arms 0-firing) — contract widening forced-witnessed,
live-UNEXERCISED; `schemas.py:228` widened in-comment only; out-of-repo float readers unwitnessed. Witnesses:
`audits/2026-06-22_oq112_round2/` (4-state controls; item-7-isolated diff at HEAD `a5593f7`; schema validation 0 errors).

---

## 2026-06-23 — OQ-112 item 2 RESOLVED: completion-witness-or-fail-closed gate (maxent stages)
**Files:** prolog/diagnostic_summary.pl, prolog/json_report.pl, prolog/maxent_classifier.pl, AGENTS.md, ISSUES.md, audits/2026-06-22_oq112_round2/
**Tier:** landed

OQ-112 item 2 RESOLVED: completion-witness-or-fail-closed gate for maxent stages — distinct
`maxent_indexed_run_info/3` completion fact, `maxent_attempted/1` + `maxent_void_alerts/1` fail-closed in
`verdict_join` (yellow/moderate, operator ruling), absorbers widened so a stage FAILURE continues the run.
Commits `d69d5d39`/`4ee4ce08`/`0ef5bf6d`; matrix + witnesses `audits/2026-06-22_oq112_round2/` (GATE.md).
Forced-witnessed, live-UNEXERCISED (0/92 voided) — do not cite as "verified live on 92"; two falsifiers named.
Invariant promoted to AGENTS.md ("completion-witness-or-fail-closed").

## 2026-06-22 — OQ-112 item-1 (C4a) RESOLVED: diagnostic_summary data-absence else-branches fail closed
**Files:** prolog/diagnostic_summary.pl, ISSUES.md, audits/2026-06-22_oq112_round1/
**Tier:** landed

OQ-112 item-1 (C4a) RESOLVED: the 13 `; Signal = agrees` else-branches in `diagnostic_summary.pl` sorted 10
sound / 3 defects; `:198`/`:212`/`:163` `agrees`→`unavailable` (commit `4e6cf6e9`). Only `:198`
(probe_abductive) is LIVE — 13/92 constraints with no `abd_triggers` fact no longer count as agreement;
output-changing at the agreements list, HEADLINE-NEUTRAL (witness `probe_before.tsv`/`probe_after.tsv`,
`audits/2026-06-22_oq112_round1/`). Tripwire kept: a missing `abductive_data.json` used to read as universal
agreement (Python side already split absence at `enrich_pipeline_json.py:164–169`). Items 2–8 staged in ISSUES.md.

## 2026-06-22 — OQ-20 + OQ-174 RESOLVED: DR baseline code/data diff (PERTURBED, stable core)
**Files:** ISSUES.md, prolog/json_report.pl, prolog/drl_purity_network.pl, python/audits/oq20_strip_cs.py, python/audits/oq20_dr_diff.py, python/audits/oq20_make_rekey.py, python/audits/oq20_analyze.py, audits/2026-06-22_oq20_dr_baseline_diff/
**Tier:** correction-key

Corpus-fixed / code-varied diff, tag `v3-dev-baseline` (`3e75f90b`) vs HEAD via run_json_report
only (method + controls: `audits/2026-06-22_oq20_dr_baseline_diff/WRITEUP.md`). OQ-20 = PERTURBED,
replicated: the priority-cascade classification is BYTE-STABLE; MaxEnt `maxent_top_type` is NOT
(29%/73% flips, concentrated `tangled_rope→snare` → OQ-175); `gaps` list→null is the OQ-109 B3
coverage bit, not a regression; noise floor positive-controlled empty. Cite-keys: the original
checkout-and-byte-diff mechanism is CONFOUNDED (the tag swaps the corpus); the id relabeling is
commit `801390a5`, not the UUID migration; the filename≠in-file-id null-DR-output tripwire is
PROMOTED to CLAUDE.md Critical Distinctions. OQ-174 RESOLVED benign carve-out:
`contamination_network` reads the AUTHORED `cs_reading_relation` fact — shared-input dependency,
not detection-dependence; Theorem 7 intact.

---

## 2026-06-21 — OQ-35 RESOLVED: wiring-gap census rows 1–6 adjudicated (cruft-vs-wire)
**Files:** ISSUES.md, docs/design/design_gaps.md, prolog/probe_oq35_field_counterfactual.pl, python/audits/oq35_field_counterfactual.py, prolog/narrative_ontology.pl, audits/2026-06-21_oq35_field_counterfactual/
**Tier:** correction-key

Six authored-field wiring gaps adjudicated
(`audits/2026-06-21_oq35_field_counterfactual/writeup.md`). Rows 2–3
`accessibility_collapse`/`resistance`: RETAIN, load-bearing — the 2026-05-31 "cosmetic" census
REVERSED (post-OQ-128/OQ-138 routing they move the signature/verdict/alerts/grade observable, not
`dr_type`; never cite "cosmetic" for these). Row 1 `is_mandatrophy_resolved/1`: dead facts
STRIPPED diff-proven; its dangling consumer surface → `design_gaps.md` GAP-18. Row 4
`cs_reference_frame/2`: RETAIN on the OQ-133 bet (GAP-17; corrects OQ-38's stale "confirmed dead"
— `json_report.pl:590` is a real read site). Rows 5–6 by-design. No engine behavior changed.

## 2026-06-21 — OQ-173 RESOLVED: MaxEnt signature-override boost made seat-aware (OQ-138 maxent residual)
**Files:** prolog/maxent_classifier.pl, prolog/load_warning_allowlist.txt, ISSUES.md, docs/design/design_gaps.md, docs/technical/signature_detection_wiring.md, audits/2026-06-21_maxent_seat_aware/FINDINGS.md
**Tier:** landed

OQ-173 RESOLVED: `apply_override_for_sig/3→/4` (maxent_classifier.pl:318) made seat-aware — routed seats
(`fcr_routed/1`, `constructed_routed/1`) skip the MaxEnt boost. Witness (`audits/2026-06-21_maxent_seat_aware/`
diff_witness.out + FINDINGS.md): exactly the 12 routed seats revert to raw, 0 non-routed move, 1 categorical
flip (`shinbutsu`), 0 `verdict_join` changes; 21-corpus sweep `routed_STILL_boosted=0` (original_v5 PARTIAL,
pre-existing failure). Premise refined: the ×3 boost never flips a CLASSICAL top — manufacturing was
classical-mass + the indexed top. Future-conversion recipe: `signature_detection_wiring.md` §4.

## 2026-06-21 — OQ-138 constructed-3 sub-part RESOLVED: claim-discriminant conversion (keeps #2's floor)
**Files:** prolog/signature_detection.pl, prolog/abductive_helpers.pl, ISSUES.md, AGENTS.md, audits/2026-06-21_oq138_fsm_route_conversion/CONSTRUCTED3_FINDINGS.md
**Tier:** landed

OQ-138 constructed-3 RESOLVED: the 3 live `constructed_high_extraction` unknown→snare seats routed to honest
`unknown`; NEW claim discriminant (mountain claim→severe, else→informational) keeps #2's RED floor — kill
condition MET (institutional_trust_erosion byte-identical RED; 47 inert + non-constructed byte-identical;
5-corpus mountain-routed→severe holds). `constructed_routed`/`fcr_routed` keyed on the UNBOUND cascade winner
(caught `superheavy_decay`). Maxent residual (`maxent_classifier:341` boost flips #1/#3 maxent_top) benign,
tracked as shared GAP. validation_suite 92/0/0. Full: `audits/2026-06-21_oq138_fsm_route_conversion/CONSTRUCTED3_FINDINGS.md`.

## 2026-06-21 — OQ-138 FCR-9 sub-part RESOLVED: false_ci_rope SEAT-AWARE conversion (template didn't transfer)
**Files:** prolog/signature_detection.pl, prolog/abductive_helpers.pl, prolog/diagnostic_summary.pl, ISSUES.md, AGENTS.md, audits/2026-06-21_oq138_fsm_route_conversion/FCR9_FINDINGS.md
**Tier:** landed

OQ-138 FCR-9 RESOLVED: `false_ci_rope` is SEAT-SPLIT (9 routed / 3 piton / 13 inert) so the FSM signature-level
template did NOT transfer — built seat-aware: `fcr_routed/1` keyed on dispatch gates + the dr_type OUTCOME
(the metric proxy diverged on 2 haiku + 4 flash seats, caught by the 5-corpus sweep); `seat_overrides/2`
threaded through diagnostic_summary. 9 seats route tangled_rope→scaffold/snare (6 verdicts change); piton-3 +
13 inert + non-FCR byte-identical; statutory_debt yellow→red via the maxent ensemble (Position-A, OQ-90 not
relitigated). Residual: maxent FCR boost (maxent_classifier:331) signature-level, logged. Full:
`audits/2026-06-21_oq138_fsm_route_conversion/FCR9_FINDINGS.md`.

## 2026-06-21 — OQ-138 FSM sub-part RESOLVED: false_summit_mountain converted RECLASSIFY→ROUTE; routed false-summits read RED
**Files:** prolog/signature_detection.pl, prolog/abductive_helpers.pl, prolog/config.pl, ISSUES.md, AGENTS.md, docs/technical/signature_detection_wiring.md, audits/2026-06-21_oq138_fsm_route_conversion/
**Tier:** landed

OQ-138 FSM RESOLVED: `false_summit_mountain` no longer overwrites `dr_type` (config
`false_summit_override_target`; victim-discriminant severity via `converted_signature/1` +
`signature_diagnostic_severity/3`; FSM removed from `known_override_signature/1`/`override_target/2`).
Full-pipeline diff: only the 3 live FSM seats change, 89 byte-identical (`PIPELINE_OLD.txt`/`PIPELINE_NEW.txt`);
verdict goes yellow→RED, ruled Position A (red is honest — 82 FSM seats across 5 corpora all carry the unmasked
dirac/cohomology tensions); protein_anabolic_resistance keeps `correction` via the discriminant. OQ-138 stays
partial (FCR/constructed/CI-rope OPEN; FNL deferred OQ-70). Full: `audits/2026-06-21_oq138_fsm_route_conversion/FINDINGS.md`.

## 2026-06-21 — OQ-119 RESOLVED: feeding moves the verdict layer, committer invariant (Theorem-7); + the cs_-facts generator tripwire
**Files:** ISSUES.md, agent/cohort_replicate_batch.py, agent/generate_kernel_corpus.py, python/audits/oq119_spend_driver.py, python/audits/oq119_analyze.py, prolog/export_oq119_corpus_join.pl, audits/2026-06-21_oq119/, audits/2026-06-21_oq119_gate0/
**Tier:** tripwire

Tripwire (carried in ISSUES OQ-119's resolution + `bulk_corpus_generation.md`): the single-story
generation path (`cohort_replicate_batch.py`/`build_prompt_parts`) authors NO `cs_` facts
(witnessed `audits/2026-06-13_oq117_within_arm_proxy/fed_arm/`); the committer/CS axis exists only
on the kernel-generation path — a fed/withheld or perturbation experiment needing it MUST use
kernel-regen, else it silently measures ≤2.5 axes; GEN_MODEL Haiku intermittently drops
`stakeholders[]` (the OQ-149 gate fires loud) — override to Sonnet for precision spends. Result
(`audits/2026-06-21_oq119/WRITEUP.md` + `audits/2026-06-21_oq119_gate0/`): feeding moves the
DIAGNOSTIC VERDICT layer (4/5 kernels, FNL commentary→correction) and leaves the COMMITTER
obstruction/divergence INVARIANT 0/5 (Theorem-7 measured, not assumed); committer generation-noise
routed to OQ-149. Correction-key: the schema's mountain no-parties `stakeholders` exemption
(`allOf[0]`, `becd0f87`, OQ-83) is DELIBERATE — never tighten it to rescue a weak generator.

## 2026-06-20 — OQ-71 depth-lineage: Phase A closes the design question (mitigated, no spend)
**Files:** ISSUES.md, docs/design/a_hypothesis_about_corpus_size.md, python/audits/oq71_a2_richness_alldims.py, audits/2026-06-04_oq71_depth_lineage/, python/build_lineage_seeds.py, agent/generate_kernel_corpus.py
**Tier:** correction-key

Phase A (zero-spend, read-only) closed the design question → OQ-71 MITIGATED: the kernel-nesting
relationship never reaches the Haiku generator (deliberate fork in `build_lineage_seeds.py`; the
breadth-arm reading-(a) is a provable no-op, so branch 1 was never in the experiment). Claim
widths: the 1.5× excess is the authorship-BUNDLE (Opus identity and/or lineage-structured
authoring, undistinguished), not generator-visible parent-nesting; `sibling_reading_ids` co-
channel bounded; "156>118" is color. A2 closed all 5 dims (joint excess +38.7, positive-
controlled; `audits/2026-06-04_oq71_depth_lineage/a2_richness_alldims_results.json`). Watch-out:
`outputs/completion_seeds/never_generated_seeds.json` DRIFTED 2026-06-13 —
`control_membership.json` is the durable authority. Graduation (flat Opus seeds) deferred;
construct-validity gap → OQ-171 (the SCOPE path is untested — do not read mitigated as "§3
tested").

## 2026-06-20 — OQ-69 research-frontier ledger DRAINED → OQ-154–170; OQ-69 closed
**Files:** ISSUES.md, issues/INDEX.md, issues/INDEX.json, CLAUDE.md, audits/2026-06-20_oq69_ledger_drain/
**Tier:** landed

OQ-69 (a backlog ledger, Ω_P) DRAINED into 17 new OQs (OQ-154–170; OQ-170 `blocked_on` OQ-160; prior
check_stack item already → OQ-142–145) and closed resolved with a provenance map; no engine code changed.
Two operator rulings (cluster F/G split; distinct-within-band priorities, all 17 provisional). Corrections:
priority parser accepts 1–99 (omega_resolver.py:69); δ (OQ-162) is live-but-zeroed, not unwired; close-vs-keep
ruled from `omega_resolver.py:244–258`. Witnesses (issues_status 170/0, omega check + selftest 10/10, menu
arrival/departure incl. control OQ-63, gate GREEN) + δ probe: `audits/2026-06-20_oq69_ledger_drain/`.

## 2026-06-20 — OQ-58 cross-corpus census, non-gating linter wired, three-leg/beta corpus ruling
**Files:** python/run_pipeline.py, python/audits/reading_reference_linter.py, agent/generate_kernel_corpus.py, ISSUES.md, docs/design/design_gaps.md, CLAUDE.md, audits/2026-06-20_oq58_cross_corpus_incompleteness/
**Tier:** tripwire

OQ-58 re-measured post-reset; referential-integrity linter wired as a non-gating `reading_linter`
step in run_pipeline (commits `1c5c97a7` code, `9532ffe4` docs). Census
(`audits/2026-06-20_oq58_cross_corpus_incompleteness/`): live 93.5% dangling is a SPARSITY
artifact (singleton working set); twins 3.7%/2.3%, kernel_v1 4.8%; GAP-07 split answer (rate
bounded ~2–5%; defensible id≥2 ~40 within-lineage, NOT tri-lineage). Regime swap witnessed in git:
`0ccc03cf` moved the 960-file rebuild OUT to the twins — the clobber fear is falsified. The three-
leg/beta-posture ruling is PROMOTED to CLAUDE.md Critical Distinctions (now the FIVE LIVE LEGS
block). OQ-58 partial→mitigated, Priority 1→3; quarantine JSON is a per-run artifact, not the
backlog.

---

## 2026-06-20 — grid-diet display: one-informative-line-when-absent + stale "unauthorable" fixed (OQ-93)
**Files:** prolog/report_generator.pl, prolog/data_repair.pl
**Tier:** landed

Two OQ-93 grid-display fixes: fully-absent grids (0/32) now print ONE plain informative line (supersedes the
same-day `[CONDITIONAL]`-token form, commit `5c23830e`; OQ-98 ruling 1 preserved for PARTIAL grids), and the
stale `data_repair.pl:356`/`:291` "unauthorable" message reworded (grid is opt-in by story focus). Hinge
witnessed: `grid_provenance` reaches `pipeline_output.json` (86/92), so display trimming cannot drop
provenance. STILL OPEN: `assemble_report` embeds ~12 grid-absent DEV-preamble lines in the model-facing .md —
decluttering pending operator go (sibling: `intent_engine.pl:75`).

---

## 2026-06-20 — OQ-56 + OQ-53 closed: canonical cross-kernel reading-stance vocabulary ruled
**Files:** python/orbit_operator.py, docs/design/design_discipline.md, ISSUES.md
**Tier:** landed

OQ-56 RESOLVED (Ω_P ruling): canonical cross-kernel vocabulary = the two Tier-1 draw-robust keys
`observer_signature` (0.722) + `obstruction_class` (0.734), made a checked fact (`CANONICAL_VOCABULARY` +
per-record `canonical` flag in `orbit_operator.py`); kill condition recorded NOT armed (manual reopen check);
seat owned in `design_discipline.md` §0.1. Headline Ω_E: the semantic-stance transpose is
foreclosed-as-draw-robust (`seat_role_vector` 0.245, model-relative). OQ-53 transpose leg resolved
(a-restricted): `constructed_high_extraction` spans 25 multi-reading kernels, `false_ci_rope` 11. Consumer
wired: the report `Signature:` line carries the canonical/twin-agreement tag via `orbit_operator.KEY_META`
(`enhanced_report.py:_signature_robustness_tag`).

---

## 2026-06-20 — orbit regeneration wired into the pipeline (was a manual pre-step; OQ-29 follow-up)
**Files:** python/run_pipeline.py, python/sweeps/regenerate_orbits.py
**Tier:** landed

`regenerate_orbits.py` wired into `run_pipeline.py` as sequential Phase 1b (operator ruling 2026-06-20:
regen every run — ~1.3s beats the recurring stale-orbits error); `manifest_inject`'s corpus_hash check kept as
the fail-closed backstop (OQ-29 Thread-C guard unchanged). Subprocess (the script `sys.exit()`s); sequential
to avoid racing the parallel Phase-2 analyses. Witness: pipeline 0 errors (was 1), `regenerate_orbits ok
[1.3s]`. Caveat: always exports the DEFAULT `testsets/` corpus.

## 2026-06-20 — within-kernel trifurcation router built + wired (OQ-55 resolved; OQ-53 within-kernel leg closed)
**Files:** prolog/cs_trifurcation.pl, prolog/json_report.pl, prolog/tests/test_cs_trifurcation.pl, prolog/stack.pl, ISSUES.md
**Tier:** landed

OQ-55 RESOLVED / OQ-53 within-kernel leg closed: new `cs_trifurcation.pl` (`cs_reading_trifurcation/3`)
routes within-kernel reading disagreement into the `debugging_philosophy.md` §6 trifurcation (Type A/B/C;
`unknown` fail-closed; singleton no-verdict); commentary-grade, serialized as `reading_trifurcation` in
`cs_kernel_comparison` and rendered by `enhanced_report.py`. Witnesses: `test_cs_trifurcation.pl` 8/8 (incl.
single-bit drift-ack discriminator + cross-kernel-leak control); live corpus all 9 multi-reading kernels
non-null (type_a×5, type_b×1, type_c×2, unknown×1). Re-scope ruling: the OQ-56 edge dropped (input-boundary
trace is the witness); transpose leg stays `blocked_on OQ-56`. Pre-existing `manifest_inject` staleness error
orthogonal (OQ-29).

## 2026-06-20 — kernel/reading orbit operator built + wired (OQ-150/OQ-53 Phase 3)
**Files:** python/orbit_operator.py, prolog/kernel_orbit_export.pl, python/run_pipeline.py, outputs/reading_orbits.json, outputs/kernel_orbits.json
**Tier:** landed

Cross-kernel orbit operator built + wired (commit `0c488468`): `orbit_operator.py` joins
`pipeline_output.json` (6 keys) + `kernel_obstruction.json` (from `kernel_orbit_export.pl`) →
`outputs/{reading,kernel}_orbits.json`; wired into `run_pipeline.py` dependency-ordered, non-critical, with a
same-run fail-closed n_constraints guard (positive-controlled). Two tripwires: live output is sparse BY DESIGN
(~3 multi-reading kernels — use `--twin haiku` for real orbit populations); only Tier-1 keys
(observer-signature 0.722, obstruction-class 0.734) are draw-robust — never cite a Tier-2 orbit membership as
a stable finding.

## 2026-06-20 — orbit-key declarability: judge against the extraction baseline, NOT the permutation null
**Files:** audits/2026-06-20_kernel_reading_orbits/, ISSUES.md
**Tier:** correction-key

OQ-150 cross-twin orbit measurement (8 keys, haiku/flash n=960; controls pass incl. `claimed_type`
0.7208 and K1 reproducing the 2026-06-18 M3 0.134). Citation correction: clearing the permutation
`band95` means beats-random-labels, NOT declarable-as-vocabulary — judge against the extraction
baseline (~0.72, the substrate's own reproducibility); only `kernel-obstruction-class` (0.734) and
`observer-signature` (0.722) reproduce there; the other 6 are membership-fragile (0.13–0.57). Ω_E
findings: the committer axis is fragile FINE but reproducible COARSE (granularity governs
declarability); the apparatus orbit is gradient-orthogonal to observer (MI 0.063, Theorem 7).
OQ-56/OQ-53 operator picks were reserved (ruled in the 2026-06-20 OQ-56 entry). Commits
`b07e84f1`, `17dba90e`, `0fdc9d7a`; evidence `audits/2026-06-20_kernel_reading_orbits/`.

## 2026-06-19 — the orbits-staleness warning is EXPECTED after every c-orchestrator run (not a bug)
**Files:** python/run_pipeline.py, python/sweeps/regenerate_orbits.py, agent/c-orchestrator.py
**Tier:** history

The `manifest_inject` `check_orbits_corpus_hash` staleness error (`run_pipeline.py:1133`) is EXPECTED after
every c-orchestrator run — non-critical (42/43 steps OK), live classification unaffected (runs on
`orbit_data.json`). Run `python3 python/sweeps/regenerate_orbits.py` before any sweep needing
`product_site_orbits.json`. Operator ruling 2026-06-19: orbits DECOUPLED, regen on demand. Lineage: OQ-29.
(Superseded 2026-06-20: regeneration was wired into the pipeline as Phase 1b — see that entry.)

## 2026-06-19 — the engine's "H1" is a disagreement tally, not a cohomology rank (citation correction)
**Files:** prolog/grothendieck_cohomology.pl, ISSUES.md
**Tier:** correction-key

`cohomological_obstruction`'s H1 = `count_disagreeing_pairs`, by its own comment a "Cech 1-cocycle
proxy" — the count of disagreeing context-pairs over the 4-point site (range 0..6), NOT dim H¹ / a
Betti number (witness: [naturalized,snare,snare,snare] tallies 3; the Betti number of that star
graph is 0). H⁰ (global section) is legitimate; do not cite `H1`/`contextuality_fraction`
(=H1/6)/`sheaf_status` as cohomology results without the tally caveat (the code's own comment
carries it; a real Čech H¹ needs an overlapping cover — reading_diff's vantage alignment is the
candidate). Lineage: OQ-151, OQ-51.

## 2026-06-19 — schema: conditional stakeholder-coverage gate (the false-negative root cause)
**Files:** schemas/constraint_story_schema.json, prompts/constraint_story_generation_prompt_json.md
**Tier:** landed

OQ-149 root cause fixed: 423/466 haiku no-stakeholder stories had authored parties — the schema omitted
`stakeholders` from `required` and its description called it optional, contradicting the prompt prose
(diagnosis: `audits/2026-06-18_oq56_*`). Fix (commit `becd0f87`): CONDITIONAL `allOf` gate — non-empty
beneficiaries/victims ⇒ require `stakeholders` (minItems 1); a true no-party mountain stays EXEMPT.
Forward-only (existing corpus untouched); witnessed under Draft7 (example passes, example−stakeholders caught,
gravity exempt). Prompt-prose reinforcement is the operator's edit; the schema is the binding gate.

---

## 2026-06-19 — reading_diff un-stranded onto the live stakeholder-seat schema + stale test corpus
**Files:** prolog/reading_diff.pl, prolog/tests/test_reading_diff.pl, ISSUES.md
**Tier:** landed

reading_diff un-stranded (commit `01cff6a7`): `reading_cells/2` now UNIONS authored
`constraint_classification/3` (archives) with the live stakeholder-seat path (`stakeholder_context/3` +
`dr_type_for_stakeholder/3`) — haiku census went 0/0/954 vacuous → 136 binocular / 111 fragile / 707 MEASURED
coverage gaps; non-regressive on kernel_v1 (suite 10/10 with archive overlaid). Twin both-stakeholder pair
coverage 26% haiku / 61% flash → folds into OQ-149. Tripwire: `tests/test_reading_diff.pl` fixtures are
pre-reset westphalia — 7/10 FAIL on the default corpus; overlay `archives/datasets/kernel_v1` → 10/10
(stale-fixture repoint unfiled, candidate OQ). Filed under OQ-56 D1.

---

## 2026-06-18 — OQ-147 crash floor + OQ-148: classifications regression (corpus-wide producer break)
**Files:** python/audits/sheaf_audit.py, python/audits/tests/test_sheaf_audit.py, ISSUES.md
**Tier:** landed

OQ-147 RESOLVED (loud): `sheaf_audit.py:515` ZeroDivisionError on an empty working set fixed with one
`insufficient` predicate reused on three surfaces (JSON null rates + `verdict: insufficient_data`; the naive
0.0 fallback rejected as Pattern 5/6); fixture `python/audits/tests/test_sheaf_audit.py` 4/4 PASS; witnesses
pre-fix crash / post-fix exit 0. OQ-148 OPEN (the real bug): `pipeline_output.json` carries
`classifications: []` for all 80 constraints vs populated 2026-06-11 snapshots — a producer regression; the
quiet-consumer blast radius (~40 python files read the field) is OQ-148's spine and a candidate
Critical-Distinctions tripwire. Pointer: ISSUES.md OQ-147/OQ-148.

## 2026-06-18 — OQ-146: orbits metadata-key landmine — single-source `load_orbits_constraints`
**Files:** python/shared/loader.py, python/oracle_gap_analysis.py, python/game_theory_nash.py, python/sweeps/product_site_delta_sweep.py, python/sweeps/structural_config_sensitivity.py, python/tests/alt_power_transform_test.py, python/tests/alt_power_transform_test_3k.py, ISSUES.md
**Tier:** landed

OQ-146 RESOLVED: the OQ-29 top-level `corpus_hash` stamp crashed every consumer iterating
`product_site_orbits.json` keys as constraints (census with positive control → 6 exposed consumers). Fix: one
fail-loud `shared.loader.load_orbits_constraints` (partition-and-assert; `_ORBITS_METADATA_KEYS`; raises on any
unclassifiable top-level key); all 6 repointed (incl. the inline `7b5801f0` oracle_gap filter). Crash-over-drop
safe by producer construction (`product_site_export.pl:80–96` emits `contexts` unconditionally). Rule: never
raw `json.load`+`.items()` on orbits files; bump the metadata set AND the loader's hardcoded literal together.
Witnesses in ISSUES.md OQ-146; `sheaf_audit.py:515` out of scope (different class).

## 2026-06-18 — OQ-104: audit_citation_status.py built (standing checker, ungated)
**Files:** python/audit_citation_status.py, ISSUES.md, audits/2026-06-18_oq104_citation_checker/
**Tier:** landed

Built `python/audit_citation_status.py` (sibling of `issues_status.py`/`known_state_status.py`; NOT in
`scripts/gate.sh` until FP rate ruled): verifies audit-cited paths exist-and-tracked or allowlisted-ephemeral;
three WARN sublabels with distinct promote flags; a gitignored in-repo path is never allowlisted (it IS the
OQ-104 signature). Census: 1224 citations / 85 dirs; untracked-pending=35 (all `outputs/*`, operator ruled
leave-flagged non-gating); missing-pending-M=66, no live broken citation. Controls 23/23 + idempotence/
rot-sensitivity. Evidence: `audits/2026-06-18_oq104_citation_checker/FINDINGS.md`. OQ-104 stays open.

## 2026-06-18 — OQ-29 RESOLVED: corpus_hash single-sourced; 14 producers stamp; consumers fail-closed
**Files:** python/corpus_hash.py, python/run_pipeline.py, python/enhanced_report.py, python/sweeps/perturb.py, python/sweeps/census_sweep.py, python/sweeps/persistence_sweep.py, python/axiom_reachability.py, python/sweeps/epsilon_sensitivity.py, python/audits/metric_audit.py, python/audits/sheaf_audit.py, AGENTS.md, ISSUES.md
**Tier:** landed

OQ-29 RESOLVED: four silent-fork `_compute_corpus_hash` copies (Pattern 2) consolidated into
`python/corpus_hash.py` (identity witness: every path `d2b3ec9429f1`); 14 producers stamp (incl.
`persistence_sweep` + its `parents[2]`→`parents[1]` fix, and the 4 formerly scoped-out audit scripts);
consumers fail-closed (orbits presence→match upgrade; persistence/Fisher STALE surfacing, four-sided witness).
Commits `b6aefb5a`/`4ab980ff`/`7b016978`. Thread-D set-probe corrected the plan twice (only 2 clean deletes;
2 live write-only test producers kept). Convention promoted to AGENTS.md. Two pre-existing bugs surfaced, not
fixed here: `sheaf_audit.py:515`, `oracle_gap_analysis.py:143`.

## 2026-06-18 — OQ-115 RESOLVED: abductive_helpers phantom under [stack] fixed; check_stack back to 4-finding baseline
**Files:** prolog/stack.pl, prolog/signature_detection.pl, prolog/check_stack.pl, ISSUES.md (OQ-115, OQ-142/143/144/145)
**Tier:** landed

OQ-115 RESOLVED: the `abductive_helpers` phantom under bare `[stack]` (existence_error from
`signature_detection.pl:1624`) fixed by `:- use_module(abductive_helpers, []).` in `stack.pl`; Option 1
rejected by evidence (tighter import cycle than the in-file comment claimed; comment corrected). Class sweep
partitioned the remaining baseline findings (phantom × guarded × reachable) → OQ-142 parent +
OQ-143/144/145. OQ-145 RESOLVED same session: `drift_events.pl:175` wrong qualifier
`narrative_ontology:`→`domain_priors:` (witnessed THREW→SUCCEEDED_CLEAN, control-backed reachability).
check_stack baseline now 3 (was 4); OQ-143/144 remain annotate-only.

## 2026-06-18 — OQ-111 RESOLVED: dead data_repair omega bridge retired (zero-diff removal)
**Files:** prolog/data_repair.pl, ISSUES.md (OQ-111), docs/design/design_gaps.md (GAP-13)
**Tier:** landed

OQ-111 RESOLVED: `bridge_omega_variables_pure/3` keyed on the bare interval id vs `constraint_<id>` modules —
always imported zero omegas (Pattern 6; OQ-99's wrong-module twin). RETIRED, not fixed (operator ruled
archives out of scope; live corpus 100% paired, omegas render via `report_generator.pl:709`/`:776-794`);
also removed `bridge_v34_data/2` call, the dead persist clause, and the /5 fabricated-`empirical` defect.
Deferred capability → GAP-13 with re-introduction recipe. Witness: pre-removal no-op probe + ZERO DIFF on
three omega-authoring reports; dynamic suite 80/0/0; [GATE] GREEN.

## 2026-06-18 — OQ-48 recalibration-readiness audit: 0 thresholds recalibratable against the twins (all MODEL-CONFOUNDED)
**Files:** ISSUES.md (OQ-48), audits/2026-06-18_oq48_recalibration/, python/audits/oq48_threshold_distributions.py, python/audits/oq48_analyze.py, python/audits/oq48_triangulate_kernel_v1.py
**Tier:** landed

Read-only distribution-break audit of the 7 in-scope χ/ε/suppression cuts against the twins (960 each),
pre-registered verdict rule: **all 7 → MODEL-CONFOUNDED, 0 proposed values, no `config.pl` edit** (flash
antimodes fail bandwidth-robustness while tracking haiku's). Haiku alone corroborates `snare_chi_floor`
(0.66≈0.666) + `snare_epsilon_floor` (0.46≈0.484; the latter also cross-regime via the kernel_v1 arm).
Controls pass (LOADCOUNT 960/960/1106, byte-identical re-run, planted-gap 0.4506). OQ-48 stays open pending
a same-regime third corpus or the ~700-story live rebuild. Evidence: `audits/2026-06-18_oq48_recalibration/`;
twin TSV sha256 haiku `7039d37b…` / flash `3c24b1d2…`, metric-code commit `0a629077`.

---

## 2026-06-18 — OQ-122 CLOSED: physics-RED fixed by OQ-128; FSM victim-gate DROPPED, discriminant handed to OQ-138
**Files:** ISSUES.md, prolog/drl_core.pl (witness only, no edit), outputs/pipeline_output.json (witness)
**Tier:** landed

OQ-122 CLOSED: the physics false-RED is FIXED by OQ-128's type_1 discrimination — live witness at commit
`2172d55`: both physics controls read `verdict_join.verdict=yellow`, `cap_applied:none`, type_1 informational
at every seat. The held FSM victim-gate branch (`oq122-fsm-victim-gate`, `ab1e9b26`) DROPPED — superseded by
engine-ROUTES-never-RECLASSIFIES (OQ-128); its insight (vic=0→informational / vic>0→moderate) handed to
OQ-138 with the pre-witnessed discriminant
(`audits/2026-06-13_oq122_retype_discriminator/breadth_sweep_results.txt`). neutron_star/FCR stays under OQ-70.

---

## 2026-06-17 — OQ-128 type_1 cap RULED + BUILT: discriminated severity (withhold high-ε snare, route low-ε artifact)
**Files:** prolog/drl_core.pl, ISSUES.md (OQ-128)
**Tier:** landed

OQ-128 type_1 cap RULED + BUILT: the overloaded `severe` split in `drl_core.pl` — degrade→snare = `severe`
(RED floor, withhold), degrade→other = `informational` (routes via the sink); rests on a witnessed clean ε gap
(mountain-claimed snare-at-seat ε≥0.50 vs rope ε≤0.25, KILL=0 across six corpora ~7000). Acceptance: RED
389→102, all 10 v5 mountain-claimed snares STAY RED, `dr_type` byte-identical; type_3/type_5 `severe`
untouched. Tripwire: do NOT re-collapse to a single `severe` (re-launders genuine math/physics mountains into
RED). KILL: a mountain-claimed snare-at-analytical at 0.25<ε<0.50 → re-run the χ-decomposition.

## 2026-06-17 — OQ-128 routing sink BUILT (engine ROUTES the author↔engine diff, never reclassifies)
**Files:** prolog/routing_sink.pl, prolog/signature_detection.pl, python/run_pipeline.py, python/enhanced_report.py, ISSUES.md (OQ-128), audits/2026-06-17_mountain_authoring_sweep/ROUTING_SINK_DESIGN.md
**Tier:** landed

OQ-128 routing sink BUILT (`audits/2026-06-17_mountain_authoring_sweep/ROUTING_SINK_DESIGN.md`): (1)
`signature_detection.pl:867` natural_law→mountain overwrite RETIRED, detector left intact as a socketed router
input (behavior-neutral: `dr_type` + `dr_claim_mismatch` byte-identical); (2) `routing_sink.pl` per-SEAT
`seat_diff/7` router with seven typed MECE addresses → `outputs/routing_sink.json`; (3) wired into
`run_pipeline.py` Phase 2, rendered by `enhanced_report.py`. Tripwire: the leaf is per-SEAT — any collapse of
seats to one constraint verdict is the KILL (§9b.4). Controls reproduced the arc's witness files (thermo →
`engine_exit_table_review`; topological → `generation_gap` + `author_engine_divergence` + `authoring_review`).

## 2026-06-16 — Typed-absence corollary added to design canon + OQ-137 (reading-layer census)
**Files:** docs/design/design_discipline.md, ISSUES.md (OQ-137)
**Tier:** landed

Promoted the OQ-121 typed-absence convention to design canon: `design_discipline.md` §5 "Typed absence — a
reading's silence is itself a declaration" (aggregate-consumable readings return a typed token, never fail
silently; NOT "every predicate is total"). Templates: `constraint_signature/2`, `q6_cell/2`. OQ-137 minted to
census the whole reading layer against the convention (scope discriminator + positive-control requirement in the OQ).

## 2026-06-16 — `census_sweep.py`: commentary census as a perturb measurement surface + denominator caveat
**Files:** python/sweeps/census_sweep.py, ISSUES.md (OQ-136)
**Tier:** correction-key

New tool pairing the perturb overlay method with the commentary census as measurement surface
(built-in null-perturbation positive control; commentary-grade, pure observation; `--corpus` to
overlay a twin). CORRECTION-KEY: a census RATE can move purely by domain-shrink (witnessed:
prevalence +12% while `extraction_blindspot_fired` held at 3 — a denominator artifact) — report
raw `fired` + `n_in_domain` beside any rate, or hold the domain fixed; the OQ-136 clustering test
must use raw counts. Also: q6 coverage decomposes into config-INVARIANT (authoring) vs config-
VARIANT (computational) parts; the two census surfaces have orthogonal config-sensitivity;
`config_validation` bounds the reachable sweep surface. Witnesses:
`audits/2026-06-16_census_sweep/`.

## 2026-06-16 — Partial-silent commentary predicates totalized (`consensus_provenance/2`, `seat_perceived_vs_real/4`) + OQ-136 minted
**Files:** prolog/stakeholder_seats.pl, prolog/tests/test_seat_totality.pl, ISSUES.md
**Tier:** landed

OQ-121 follow-up: the two remaining partial-silent R3 commentary predicates totalized (zero blast radius — no
external consumers). `consensus_provenance/2` gains `no_agent_seats`/`seats_untyped`; live plural 37 /
no_agent_seats 21 / manufactured 8 / unanimous 6 (Σ=72). `seat_perceived_vs_real/4` returns
`Computed = untyped` (0 live triggers, 370 seats total). Regression `test_seat_totality.pl` 8/8;
commentary_census 40/40; oq86 14/14. OQ-136 minted (interpret the honest absence buckets; pre-registered
provenance-clustering test). Witnesses: `audits/2026-06-16_partial_silent_totalization/`.

## 2026-06-16 — OQ-121 RESOLVED: totalize the commentary family + domain-relative census coverage
**Files:** prolog/stakeholder_seats.pl, prolog/commentary_census.pl, prolog/tests/test_commentary_census.pl, python/run_pipeline.py, outputs/commentary_census.json
**Tier:** tripwire

OQ-121 RESOLVED: the R3 commentary family totalized on the engine's existing never-fail discipline
— new TOTAL `stakeholder_seats:extraction_state/2` (out_of_domain / extraction_clear /
extraction_unnameable / extraction_fired; the new unnameable bucket surfaced 5 live constraints
the silent failure had hidden); census `coverage` made DOMAIN-relative with `prevalence` a
distinct number. The new-census-source tripwire (total hook, declared out-of-domain buckets,
coverage ≠ prevalence ≠ corpus-fraction, `commentary_coverage_decidable/1`) is covered at the edit
site — full convention in `commentary_census.pl`'s header. Witnesses:
`audits/2026-06-16_oq121_totalization/`; plunit 40/40; resolution in ISSUES OQ-121.

## 2026-06-16 — OQ-134 RESOLVED: generic commentary-grade corpus census (`commentary_census.pl` + pipeline wire)
**Files:** prolog/commentary_census.pl, prolog/tests/test_commentary_census.pl, python/run_pipeline.py, outputs/commentary_census.json, outputs/commentary_census.md
**Tier:** landed

OQ-134 RESOLVED: generic commentary-grade census — `commentary_census.pl` (multifile `commentary_cell/3` hook,
`commentary_absence_bucket/2`, `commentary_coverage_decidable/1`; sources q6 + extraction_reading) wired into
`run_pipeline.py` → `outputs/commentary_census.{json,md}` with a corpus-identity manifest. Design facts: the
sum invariant (Σ buckets == n_corpus ∧ n>0) is the contract enforcer; coverage = both-sides-MEASURED;
extraction coverage shipped null [SUPERSEDED same day by OQ-121 totalization → 1.0 over its 50-constraint
domain, prevalence 0.06]; absence buckets fail-closed (archives 100% `q6_unmeasured`). Extension = one
`commentary_cell/3` clause. Witnesses: `audits/2026-06-16_oq134_commentary_census/`; resolution in ISSUES.md OQ-134.

## 2026-06-16 — OQ-86 RESOLVED: `extraction_reading/2` R3 commentary (no-authored-victim blindspot)
**Files:** prolog/stakeholder_seats.pl, prolog/report_generator.pl, python/enhanced_report.py, prolog/tests/test_oq86_extraction_commentary.pl, prolog/data_repair.pl
**Tier:** tripwire

OQ-86 shipped: `stakeholder_seats:extraction_reading/2` R3 commentary (NEVER a classifier input)
firing on the blindspot shape — snare/tangled_rope ∧ no AUTHORED victim ∧ ≥1 beneficiary-side
seat; 24/24 plunit. TRIPWIRE (now covered in-code at the `stakeholder_seats.pl`
`authored_victim/1` comment): `data_repair.pl:153` FABRICATES `constraint_victim(C,
inferred_subject)` on the exact blindspot metric profile, so a naive `\+ constraint_victim(C,_)`
guard is INERT on every real report — any "story authored no victim" predicate must exclude the
`inferred_subject` sentinel (P5/P6). Census: fires 3 live / 10 haiku / 34 flash, ALL tangled_rope;
0 across the pre-stakeholder archives (guard C fail-closes, correct). Sets the table for OQ-134.

## 2026-06-16 — Seat/orientation invariant audit + v8 "seat/gauge/orientation" design spec (engine votes one-seat)
**Files:** docs/design/v8_seat_gauge_orientation_design_spec.md, audits/2026-06-16_seat_invariant_vs_prolog/, docs/seat-theorem-v1.md, docs/deferential_realism_paper_v7.md
**Tier:** landed

Read-only seat/orientation audit (`audits/2026-06-16_seat_invariant_vs_prolog/`, merges `c58611a8`/`864c961d`;
R3 probe merge `77e33bca`): `cs_pattern`/`cs_classify` is a pure function of authored presentation, audited
one-directionally → the engine votes ONE seat; the committer/CS axis is the orientation (showing) face, not a
second content-seat (R3 declaration = operator's seat). v8 design spec drafted rev3
(`docs/design/v8_seat_gauge_orientation_design_spec.md`, merges `403375e4`/`f6c22b81`/`1e81bc0f`):
seat/gauge/orientation + the transitive cross-axis taint invariant; DRAFT pre-implementation, adoption blocked
on operator. Tripwire (hard on v8 adoption): v7 "seat" = v8 "gauge" — use the spec's §4 bridge table. Operator
docs untracked: `docs/one_seat_audited.md`; `docs/provenance_is_not_proof.md` (NOT for commit).

## 2026-06-16 — Orientation is a deferred Ω_E, NOT Ω_P (OQ-133 relabel) + verification-depth discipline
**Files:** ISSUES.md, docs/technical/build_discipline.md, CLAUDE.md
**Tier:** correction-key

OQ-133 relabeled: orientation (a concealment's enclosure vs survival vs defense) is a DEFERRED Ω_E
— a fact about the actor's stance, resolved by world-observation (the longitudinal Cor-3
confrontation-response signature) — NEVER Ω_P, because Ω_P-routing licenses the encloser to self-
certify as defender by fiat; under strategic gaming it falls outside the framework entirely. Do
not collapse with `contested_open` (a genuine Ω_P/Ω_C — same surface OPEN, opposite
type/operation). Both the relabel and the stop-verifying discipline are PROMOTED (CLAUDE.md
synthesis item (5) + `build_discipline.md` → When to stop verifying); provenance was the
`q6_crosscheck` review arc.

---

## 2026-06-16 — R5 Q6 synchronic crosscheck completed: `q6_crosscheck/3` replaces `zombie_piton_crosscheck/2`
**Files:** prolog/stakeholder_seats.pl, prolog/report_generator.pl, prolog/narrative_ontology.pl, python/generate_constraint_pl.py, python/linter.py, ISSUES.md
**Tier:** landed

`zombie_piton_crosscheck/2` GONE — replaced by `q6_crosscheck(C, Cell, Daylight)`, the full status×signature
matrix; commentary-grade (sole caller `report_generator.pl`; classification byte-identical by construction);
four non-verdict buckets kept distinct; `q6_cell` is a mode-robust if-then-else (an unguarded catch-all had
spuriously matched all 71 — caught by its own positive control). `q6_unclassified` witnessed 0 live, reachable
on twins (haiku=1, flash=5). Daylight axis (`founding_problem_corroboration_class/2`) SHIPS INERT pending a
bounded R5 backfill (OPEN graduation step). Audit: `audits/2026-06-16_q6_crosscheck_completion/`; tracking
OQ-83; deferred diachronic tier → OQ-133.

---

## 2026-06-16 — `python/paths.py` is the canonical path source (depth-agnostic); 3 absolute-path bugs fixed
**Files:** python/paths.py, python/domain_priors_expander.py, python/sweeps/range_sweep.py, python/tests/diff_cut_proof.py, AGENTS.md, ISSUES.md
**Tier:** tripwire

New code imports filesystem roots from `python/paths.py` (walks up to the `pyproject.toml` marker
— depth-agnostic, worktree/CI-safe); nested scripts use the byte-identical bootstrap in AGENTS.md
§3; never `Path(__file__).parents[N]` or hardcoded `/home/...`. The 3 hardcoding files fixed
(domain_priors_expander, sweeps/range_sweep, tests/diff_cut_proof); witnessed: paths.py resolves
== old values, bootstrap finds the root from 6 depths. Convention covered in AGENTS.md. ~69
scripts still re-derive inline — bulk migration held on OQ-132 (do not bulk-migrate before the
A-vs-B packaging ruling).

## 2026-06-15 — OQ-131 Q1 (Ω_E) measured: 6-vs-4 observer site is consonant-suppressing, NOT a combinatorial artifact
**Files:** prolog/constraint_indexing.pl, prolog/config.pl, prolog/config_schema.pl, prolog/config_validation.pl, python/audits/oq131_six_observer_probe.py, audits/2026-06-15_oq131_six_observer/, ISSUES.md
**Tier:** landed

Three ADDITIVE observer site modes added to `site_contexts_for_mode/2` (`canonical_6`/`power_only_4`/
`power_only_6`, commit `a06b5c7f`; canonical/product byte-identical; canonical-first ordering is LOAD-BEARING
for the `(H¹₆−H¹₄)/9` conditioning — don't reorder). Finding (pre-registered,
`audits/2026-06-15_oq131_six_observer/`): the observed delta falls BELOW the permutation band on live/haiku/
flash → the new seats are consonant-suppressing; the combinatorial artifact is FALSIFIED; power-atom-driven,
bundle-robust. Config gotcha: every param needs a `config_schema.pl` spec or `[stack]` halts at load
(3 witnessed). OQ-131 stays `future` (Q2/Ω_C corpus-adoption deferred); scope walls: H⁰/H¹ only, seat-bundle-dependent.

---

## 2026-06-15 — OQ-108 resolved: per-position witness coverage shipped; OQ-107 closed `future`; new `future` status token
**Files:** prolog/stakeholder_seats.pl, prolog/json_report.pl, python/tensions_ledger.py, python/issues_status.py, ISSUES.md
**Tier:** landed

OQ-108 RESOLVED: per-position witness coverage over the 6-atom power vocabulary
(`power_witness_count/3`/`power_witness_map/2`, reusing `canonical_d_for_power/2` as enumerator; serialized as
`perspective_witness` 64/64; rendered in the tensions ledger — a 0 = inference-only, Pattern-6 zeros SHOWN;
witness: `geopolitical_settlement_competition`). Tensions ledger now suppresses the grid line when fully
absent (report .md generators deliberately unchanged). New status token `future` (operator ruling 2026-06-15)
added to `issues_status.py` + the ISSUES.md footer grammar; OQ-107 closed `future` and its wrong
`blocked_on` dep on OQ-108 dropped.

## 2026-06-14 — corpus omega soundness POC (OQ-130 scale arm): authored omegas 80% sound, NOT §8-class; identity is three orthogonal axes
**Files:** audits/2026-06-14_corpus_omega_soundness_poc/, ISSUES.md, docs/design/design_gaps.md, prolog/testsets_haiku/
**Tier:** correction-key

§C soundness POC under a two-party independence protocol (sealed adjudicator key `acc27d22` BEFORE
the blind executor; read-only over testsets_haiku): soundness 24/30 = 80% — identity-
overstatement, not fabrication; OQ-130's blocking precondition discharged (the authored omegas are
NOT §8-class). Identity measured as three orthogonal axes (KIND ⊥ topic at ARI≈0; frontier ⊥
topic); the unsound class = the kernel-contest family → GAP-11 (frontier-identity organ missing;
embeddings the real instrument). CITATION RULE: "80% sound on a 30-omega sample", never "the
corpus is 80% sound" — and the external adjudication
(`audits/2026-06-14_corpus_omega_soundness_poc/adjudication_external.md`) corrects id-20 → ≈77%
(23/30), notes probe-3 independence was within-instance and probe 1b≡1a (one KIND surface); the
family's noise-vs-legitimate-committer-Ω_P-frontier reading stays CONTESTED/OPEN (Seat Theorem Cor
2b).

## 2026-06-14 — omega-resolver pilot validated on ISSUES.md (OQ-130 minted); §8 landed into OQ-129 OPEN-A
**Files:** python/omega_resolver.py, ISSUES.md, audits/2026-06-14_omega_resolver_pilot/, audits/2026-06-14_extraction_blindness_existential_label/
**Tier:** landed

Omega-resolver pilot validated: `python/omega_resolver.py` (loader / authority control / SCC frontier /
checker / selftest 8/8), read-only, NOT a pipeline gate. §8 re-witnessed — `extraction_blindness` is an
existential-labeling artifact (live 16/20=80%, haiku 258/358=72.1%) → landed into OQ-129 OPEN-A
(`audits/2026-06-14_extraction_blindness_existential_label/`). §E verdict: 57 confirm / 7 contradict /
0 standoff, each contradict settled by an external fact — pilot criterion met. `blocked_on_human` relator
added; 16 `Deps:` edges authored; `issues_status --check` intact (129). OQ-130 minted (corpus scale arm,
gated on an omega-soundness spot-check). Evidence: `audits/2026-06-14_omega_resolver_pilot/`.

## 2026-06-14 — OQ-129: perspectival-gap feeder rewired onto authored stakeholder seats (was reading the retired constraint_classification)
**Files:** prolog/report_generator.pl, prolog/json_report.pl, ISSUES.md, audits/2026-06-14_omega_gap_reconstruction/
**Tier:** tripwire

`omega_from_gap/5` had been silently dead corpus-wide since the 2026-06-05 rebuild — stranded: its
feeder `detect_gap_pattern/2` queried the RETIRED `constraint_classification/3` surface (0 live
facts, so a probe over it reads "no gaps" when it means "no facts"). Rewired onto authored
stakeholder seats (`constraint_stakeholder/7` via `dr_type_for_stakeholder/3`, witnessed verdict-
equivalent to the plan's inline path); labeling made bypass-proof; the json_report gaps guard
repointed. Mechanism since SUPERSEDED by the OQ-197 three-valued `gap_status` rebuild (see the
2026-07-01 entries). Gap-Ω prevalence inherits the OQ-70 authoring-convention caveat. OPEN-A..D
carried on OQ-129. Witnesses: `audits/2026-06-14_omega_gap_reconstruction/`.

## 2026-06-14 — OQ-50 closed (explainer rebased on dr_type + type_3/type_5 per-context); OQ-74 core ruled reading-relative; OQ-122 fixture-blocker found STALE; OQ-128 minted
**Files:** prolog/report_generator.pl, prolog/drl_core.pl, ISSUES.md, docs/logic_extensions.md, audits/2026-06-14_oq122_fixture_triage/, audits/2026-06-14_oq49_remeasure/coord0_conjunction_positive_control.txt
**Tier:** landed

OQ-50 closed: `forensic_explain_false_mountain/2` headlines the post-signature `dr_type` (heuristic demoted to
a metric-level annotation; fail-closed unbound guard); `type_3`/`type_5` (`drl_core.pl:622,629`) lead with
`standard_context` + cut dropped (unbound-Ctx trap gone; caller census clears the multiplicity falsifier;
validation_suite 57/0). OQ-74 core RULED reading-relative (coordination_type a seventh authored field; the 55%
sibling disagreement is signal); OQ-49 hand-up limb MOOT (coord=0 subset positive-controlled empty —
`audits/2026-06-14_oq49_remeasure/coord0_conjunction_positive_control.txt`). OQ-122 fixture-blocker found
STALE (gate adds zero new failures; evidence `audits/2026-06-14_oq122_fixture_triage/`); OQ-128 minted; OQ-122 stays open.

---

## 2026-06-14 — OQ-116 split-closed: de-leak lint chokepoint (linter.py SSOT); MMC = non-collapsing seat divergence; SDZ → OQ-127
**Files:** python/linter.py, python/regenerate_stories.py, agent/cohort_zero_regen.py, python/tests/test_deleak_chokepoint.py, audits/2026-06-12_cohort_zero/pilot_witness.py, docs/design/design_discipline.md, ISSUES.md
**Tier:** tripwire

OQ-116 split-closed (operator: the linter is for the operator, not the engine; linting stories =
orchestrated bias): threshold-coupled lint codes must never reach the authoring LLM (de-leak-in-
reverse, OQ-74); SSOT in `linter.py` (`THRESHOLD_COUPLED_LINT`, `build_author_feedback`), the
regenerate_stories fork + cohort_zero_regen routed through it; MMC reworded as a claim-vs-metric
seat divergence that need not collapse. Engine witness
(`audits/2026-06-14_oq116_mmc_engine_witness/`): 9/9 live MMC firings diverge at the metric seat;
the "FSM exists for it" premise was WRONG (ε ranges disjoint). SDZ calibration half → OQ-127.
Promotion test applied in-entry: the mistake is made LOUD (`test_deleak_chokepoint.py` census
tripwire + design_discipline §4a) — deliberately NOT promoted.

## 2026-06-14 — Engine reads ε from constraint_metric, NOT the testset's domain_priors:base_extractiveness (corrupt-test / ε-trace tripwire)
**Files:** prolog/drl_core.pl, prolog/constraint_data.pl, prolog/domain_priors.pl
**Tier:** tripwire

The verified classification ε path is `drl_core:base_extractiveness/2` → `constraint_data` →
`config:param(extractiveness_metric_name)` → `narrative_ontology:constraint_metric(C,
extractiveness, V)`; the testset's `domain_priors:base_extractiveness/2` fact is a SEPARATE prior
path the classifier does not read for corpus constraints. Corrupt-testing or tracing ε must edit
`constraint_metric` — editing `base_extractiveness` shows no effect and wrongly reads ε as inert
(witnessed building the twin negative control, `audits/2026-06-13_twin_comparison/`). Since
covered by the OQ-205 ε declaration discipline (`docs/design/epsilon_declaration_discipline.md`:
read-site table + the three-site equality check).

## 2026-06-14 — OQ-49 SPLIT-CLOSE: signature-override re-measure on live corpora; FNL collapse witnessed by source-attribution
**Files:** python/audits/oq49_override_remeasure.py, audits/2026-06-14_oq49_remeasure/, ISSUES.md, prolog/signature_detection.pl
**Tier:** landed

OQ-49 SPLIT-CLOSE (plan `review-oq-49-in-issues-md-twinkly-mochi.md`): the (a)/(b) ruling was un-answerable as
posed (testsets_3000 dead since the reset; FNL bait driver deleted, OQ-70 `72ec2cdd`); re-measured read-only
on live corpora. Collapse witness is STRUCTURAL: every FNL firing tags source-1, zero source-2/unaccounted
(kill condition not triggered); FNL override-effective 0/6/8; the dominant live override is
`false_ci_rope→tangled_rope` (~10×); the 14-firing residual all carry coord+asym (coord=0 laundering subset
EMPTY, positive-controlled) → handed to OQ-74. Citation qualifier: never cite the testsets_3000 1730/1661
numbers as live. Evidence: `audits/2026-06-14_oq49_remeasure/`; OQ-49 resolved.

## 2026-06-13 — Twin cross-model comparison harness + two generation-quality fixes (classify_corpus driver; Fix A axiom-status, Fix B sibling snap)
**Files:** python/run_pipeline.py, python/story_repair.py, agent/generate_kernel_corpus.py, python/audits/twin_comparison.py, audits/2026-06-13_twin_comparison/
**Tier:** landed

Plan `federated-toasting-sedgewick.md` landed in four commits: Fix A (prompt offers only
`holdable`/`overridden`; `story_repair.py` coerces out-of-enum, counts + escalates — forward-only, twins
unaffected); Fix B (`snap_sibling_id()` unique-confident snap, else quarantine per OQ-58, never
wrong-snapped); B1 `classify_corpus(corpus_path, output_name, expected_model)` in `run_pipeline.py`
(non-default-corpus driver with zero-glob / load-complete / model-fingerprint / stale-raw refusals; canonical
outputs untouched). B-result (`audits/2026-06-13_twin_comparison/`, twins classified at commit `8126231`,
N=1000 permutations): H1 structural — all 7 fields HOLD; H2 drift FALSIFIED (below-band result EXPLORATORY
only, earlier over-claim corrected). Forward work → OQ-123/OQ-124/OQ-125.

## 2026-06-13 — Branch cleanup: merged oq117-evidence-block into main; landed the China-legitimacy topic-run artifacts; gitignored *.pdf
**Files:** KNOWN_STATE.md, ISSUES.md (merge), .gitignore, prolog/testsets/{demographic_resource_allocation,livelihood_security_reading,performance_legitimacy_contradictions,performance_legitimacy_flat_control,property_sector_overhang,qualitative_development_reading,quantitative_growth_reading,techno_nationalist_reading}.pl, json/ (7 matching), essays/2026-06/captive_on_both_ends_v3.md
**Tier:** landed

Merged `oq117-evidence-block` (diverged at `f3f347fe`) into main `--no-ff`; only conflict KNOWN_STATE.md (both
dated sections kept); `issues_status.py --check` 120/0. Committed the China-legitimacy topic-run artifacts
(8 testsets + 7 json + `essays/2026-06/captive_on_both_ends_v3.md`); discarded stale local edits to the two
pipeline-regenerated files; `.gitignore` now excludes `*.pdf`. Branch deleted post-merge. NEXT STEP (operator's
call): run `python3 python/run_pipeline.py` — the 8 testsets were committed generate-only, pipeline outputs
stale w.r.t. them until a run.

---

## 2026-06-13 — Two-model TWIN CORPUS: full never-generated rebuild (Haiku, 988) + Gemini Flash twin (971) reconciled into testsets_haiku/ + testsets_flash/ + testsets/ (branch corpus-rebuild-fresh, merged to main)
**Files:** agent/run_no_scope_gemini.py, agent/_pilot_ladder_strip.py, agent/generate_kernel_corpus.py, prolog/testsets_haiku/, prolog/testsets_flash/, prolog/testsets/, json_haiku/, json_flash/, prolog/beta_processed_flash.txt, ISSUES.md (OQ-75), CLAUDE.md (Corpus Loading)
**Tier:** landed

On branch `corpus-rebuild-fresh` (five-defect provenance fix cherry-picked `2e3e1998`→`dc12bf5a`): full
never-generated pool (1005 readings / 331 kernels — not the remembered 304/101) → Haiku 988/1005 (~$27 batch)
+ Gemini Flash twin 971 via `agent/run_no_scope_gemini.py` (faithful port, adapter-shaped, thinking_budget=0);
reconciled INTERSECTION = `testsets_haiku/` (960) + `testsets_flash/` (960); `testsets/` (44) reserved for the
orchestrator corpus. All five provenance defects held at scale; one grid-gate firing regenerated, not waived.
Tripwire promoted to CLAUDE.md Corpus Loading: overlay `corpus_path` with `asserta`/`retractall`-first, never
plain `assertz` (silently ignored — witnessed 44-vs-960). Residuals (ISSUES OQ-75): 17+34 redraws, dominant
cause the `status:'contested'` enum violation; run_pipeline's JSON_DIR hardcoded to `json/`.

---

## 2026-06-13 — Essay-synthesis read-site: report scalars over a propaganda-artifact testset are formalization-of-a-reading, not measurement; OQ-102(a)/OQ-103 are RESOLVED, not open
**Files:** outputs/constraint_reports/{demographic_resource_allocation,livelihood_security_reading,performance_legitimacy_flat_control,property_sector_overhang,qualitative_development_reading,quantitative_growth_reading,techno_nationalist_reading}_report.md, essays/2026-06/captive_on_both_ends_v3.md, docs/technical/build_discipline.md (Instrument-richness section), ISSUES.md (OQ-102, OQ-103)
**Tier:** correction-key

Claude-web's `captive_on_both_ends_v3.md` reading is report-witnessed (grid 0/32, INDEX VACUOUS,
OPEN(no_gradient_data), drift authored-as-PROJECTED), but the confident scalars over a regime-
self-presentation artifact are a formalization of ONE analyst's reading, never measurement of
China — rule PROMOTED to `build_discipline.md` → *Instrument richness is gated on substrate
instrumentation*. Correction (citation-staleness): it cited OQ-102(a)/OQ-103 as open; both were
RESOLVED (2026-06-11 / 2026-06-12) — the flags it leaned on ARE those fixes working; lean on a
contamination edge only when its `Provenance` reads `authored`. The "dominant Western frame"
contrast was characterized from general knowledge — needs a real-coverage check before it is
rigorous (operator's call).

---

## 2026-06-13 — OQ-109 RESOLVED: replicate spend ran (15 draws, batch), σ/seat prediction FALSIFIED-AS-TESTED (Fisher p=0.649) → discharged to OQ-118 (draw-stability tracks field-construction-type, not the σ/seat line)
**Files:** agent/cohort_replicate_batch.py, python/cohort_stability.py, python/cohort_sigma_seat_eval.py, audits/2026-06-12_cohort_zero/, ISSUES.md (OQ-109 resolved, OQ-118 filed)
**Tier:** landed

OQ-109 RESOLVED: gated replicate spend executed (15 draws = 5 contested kernels × 3, sonnet-4-5 @ temp 0.2,
batch `msgbatch_01UbfPq13BcHgJKxcsqK549i`, commit `dcfaea97`; frozen seed-spec so SIGMA_SEAT_PREDICTION
`5f2a626c` applies; Fisher instrument validated vs scipy before use). σ/seat partition FAILED its
pre-registered falsifier: 188 cells, 47.87% consistent, Fisher two-sided p=0.649 = NO SEPARATION. Operator
split ruling: ROBUST (apparatus-presence bucketing + scoped null) vs CONFOUNDED-HELD (cast/σ exact-match;
verdict-stability n=6/temp confound) with graduations. META-FINDING: draw-stability tracks
FIELD-CONSTRUCTION-TYPE, not the σ/seat line → discharged to OQ-118 (filed). Evidence: `audits/2026-06-12_cohort_zero/`.

## 2026-06-13 — OQ-109 Phase C analytical tail CLOSED to partial: population correction (Iran pair → separate cohort, n=7→n=5) + stability/σ-seat instruments wired & witnessed; two named residuals (gated σ/seat spend, cohort-one reading_diff)
**Files:** prolog/testsets/ (n=5 restored), prolog/archives/datasets/iran_essay_2026-06-11/, python/cohort_stability.py, python/cohort_sigma_seat_eval.py, ISSUES.md, audits/2026-06-12_cohort_zero/
**Tier:** landed

Phase C wire-only close (branch `oq109-phasec-closeout`; WRITEUP `audits/2026-06-12_cohort_zero/WRITEUP.md`).
Population corrected: the two untracked Iran-essay stories (different generation regime, NOT
cohort-zero-homogeneous) archived to `prolog/archives/datasets/iran_essay_2026-06-11/` (commit `d26d04a2`,
byte-identity proven), corpus restored n=7→5 (manifest `1f517a0`); Iran-count fork closed positive-controlled.
Instruments landed (commit `1f517a08`): `cohort_stability.py` (Pattern-5 absence-split; selftest PASS) +
`cohort_sigma_seat_eval.py` (zero-drift parse of the frozen prediction; REFUSES verdicts below 3 stories × 2
draws). Two named residuals (status partial): the gated σ/seat replicate spend; cohort-one `reading_diff`
re-point (no live positive control until a stakeholder-cell story lands).

---

## 2026-06-12 — design_discipline v1.3: §9 recorded — engine's pipeline seat is discovery not justification; no-verdict-skips-adjudication; benign-constraint bias control independently re-derived
**Files:** docs/design/design_discipline.md, essays/2026-06/marked_to_market.md
**Tier:** landed

§9 recorded in design_discipline.md (v1.2→v1.3): the engine sits in the context of discovery —
contribution is well-formed questions, not calibrated scores; standing condition: no verdict skips
adjudication; surviving risk is systematic bias (proposed benign-constraint control independently
re-derives open item (b)). Wiring-state claims in §9 attributed to the review, not independently
witnessed; stray marked_to_market.md:Zone.Identifier artifact removed from essays/2026-06/.

## 2026-06-12 — OQ-78 evidence pass: ε clustering two-layer; bin boundaries EQUAL config thresholds; circularity → OQ-117; THEN probe HALTED pre-spend — epsilon_bin channel DEAD at the generation interface (hypothesis is the live channel)
**Files:** prompts/uke_scope_v2_json.md, prompts/constraint_story_generation_prompt_json.md, prolog/config.pl, agent/story_generator_base.py, agent/generate_kernel_corpus.py, agent/c-orchestrator.py, ISSUES.md
**Tier:** correction-key

ε↔claimed_type banding is AUTHORING CONVENTION (OQ-70-analog, never a detection result); the
disclosed bin boundaries 0.10/0.30 EQUAL `piton_epsilon_floor`/`tangled_rope_epsilon_floor` (SCOPE
bin-assigner only; the 0.45/0.46 rope/snare split is not transmitted). Ruling: the quantization
half CLOSED working-as-designed; idiom half OPEN (re-baselined on cohort zero); independence
circularity → OQ-117. The greenlit probe then HALTED PRE-SPEND: NO production path feeds
epsilon_bin to the authoring model — the historical numeric channel was scrubbed at `b6c4e113`
(2026-06-05), recorded uke_scope blocks are MODEL-FABRICATED, so the 15/15 bin-conformance was
self-labeling; epsilon_bin is a Pattern-1 dangling wire, hypothesis-feeding is the live channel
(disposition in OQ-117). Fate-2 graduation re-routed through OQ-109 Phase C at zero marginal
spend; direction-of-fix: no target-ε disclosure, no tightening bins toward thresholds. Witnesses
W1–W3: `audits/2026-06-12_oq78_dead_bin_channel/`.

## 2026-06-12 — OQ-106 RESOLVED: RETIRE ruled and landed — `structural_coercive_intent` top verdict deleted (range-dead, producerless, consumerless); capture-as-design ratified as piton intension with recorded kill condition; GAP-08 revival stays generic
**Files:** prolog/intent_engine.pl, prolog/config.pl, prolog/config_schema.pl, ISSUES.md, docs/design/design_gaps.md, audits/2026-06-12_oq106_retire/
**Tier:** landed

RETIRE ruled + landed: `structural_coercive_intent` top verdict deleted (range-dead, producerless,
and consumerless — report_generator.pl:22 imports intent_engine except classify_interval/3);
capture-as-design ratified as the piton intension (constraint_captured/1), kill condition recorded
in the OQ-106 close arming GAP-08 revival; option (ii) declined (OQ-36 misread risk). Witness
(*prove before you replace*): full suite before/after byte-identical on substantive lines (5 [INTENT] lines); GAP-08
stale residual paragraph updated to the 2026-06-11 fail-closed ruling. Worktree oq106-retire from
`f3f1e99f`; evidence audits/2026-06-12_oq106_retire/.

## 2026-06-12 — OQ-105 RESOLVED: operator ruled fork (a) ALONE; alignment rule landed (prompt + fail-closed validate_json gate); live exposure 0 after the cohort-zero swap retired all 11 hosts
**Files:** ISSUES.md, prompts/constraint_story_generation_prompt_json.md, python/generate_constraint_pl.py, audits/2026-06-12_oq105_alignment_gate/
**Tier:** landed

Operator ruled fork (a) ALONE — grid alignment at generation: prompt rule "One time grid per story"
+ fail-closed `_grid_alignment_errors` in validate_json (both jsonschema and fallback paths); the
cohort-zero swap (`7ca48e0b`) retired all 11 hosts → live misaligned rows 0. Witnesses: W1 synthetic
misalignment fires; W2 5/5 live _c0 JSONs clean; W3 gate over the 60 archived JSONs flags EXACTLY
the 11 known hosts, 0 false positives. Reopen conditions for (b) recorded in the OQ; worktree
oq105-alignment-rule; evidence audits/2026-06-12_oq105_alignment_gate/.

## 2026-06-11 — OQ-105 per-row sweep: PREDICTED bucket discharged — 4/23 misaligned rows timing-distorted, all one snare-floor mechanism; fork ruling still open
**Files:** ISSUES.md, audits/2026-06-11_oq105_row_sweep/
**Tier:** landed

Interpolation counterfactual over all 23 grid-misaligned suppression rows: 4/23 rows (181/3588
cells, 5.0%) timing-distorted, all the one predicted mechanism (endpoint scalar ≥ snare suppression
floor 0.60, interp below → snare dated early); 19/23 substitution-robust at every context; OQ-105
(a)/(b) fork left open, (b)'s live payoff bounded to the 4 rows. Worktree oq105-row-sweep from
`37ea069f`; evidence audits/2026-06-11_oq105_row_sweep/.
## 2026-06-12 — SIGNATURE-IDENTITY WITNESS: the engine types KINDS, not stories — naming-drift triple probed in fingerprint space; identity-by-signature ruled out for the Phase C regen; seeded_from + draw index added to cohort-zero provenance spec
**Files:** audits/2026-06-12_signature_identity_witness/, ISSUES.md, CLAUDE.md, prolog/logical_fingerprint.pl, agent/c-orchestrator.py
**Tier:** correction-key

The kernel_v1 press/Reformation naming-drift triple + 3 controls probed pairwise in fingerprint
space (`audits/2026-06-12_signature_identity_witness/`, raw output pasted): both directions
witnessed — same-material draws can escape their kind (draw 2 a different mechanism class) and
different-material stories can share one (control pair 6/7). KIND-level meta-analysis survives
generation stochasticity; STORY-level identity must be authored forward (`seeded_from` + draw
index, schema-required for cohort zero — OQ-109 item 4), never recovered backward by signature
matching. Operator ruling appended (seat-theorem): a category shift on redraw is the mechanism
WORKING; the "identity does not survive" valence WITHDRAWN; the stability table reads as a σ/seat
partition (OQ-26). PROMOTED to CLAUDE.md Critical Distinctions (Generation is stochastic block).
**OQ-190 correction (2026-08-17):** the table's `status: stable` is not `positive`-stable — cells
with `agreement_kind: absence` witness nothing (`victims` 0/6 positive, not 4/6; cast bucket 3/54
positive). Filter on `agreement_kind` before citing. Tripwire promoted to CLAUDE.md; witness
`audits/2026-08-17_oq190_blast_radius/stability_positive_grade.tsv`.

## 2026-06-12 — COHORT ZERO LIVE: pilot 7/7 generated, swap executed (live corpus = 5 _c0 stories; pre-cohort set retired to kernel_v2_test2); C-arm first live decisions witnessed; trio falsifier RESOLVED (filters on new regime); OQ-116 filed
**Files:** prolog/testsets/ (corpus swap), json/, prolog/guard_exclusions.pl, prolog/archives/datasets/kernel_v2_test2/, agent/cohort_zero_regen.py, ISSUES.md, audits/2026-06-12_cohort_zero/
**Tier:** landed

Pilot 7/7; swap executed — live corpus = 5 _c0 stories, pre-cohort set retired to
prolog/archives/datasets/kernel_v2_test2/ (renamed from pre_cohort_zero_2026-06-12, manifest carries
both names). Trio falsifier RESOLVED: filters on the new regime (1/4 mountain-claims certify);
trust_erosion_c0 excluded AND chain-false; C-arm first live decisions witnessed
(battery_witnesses.out); OQ-116 filed (scaffold-zone calibration, MOUNTAIN_METRIC_CONFLICT).
Pipeline green n=5 (manifest 2026-06-12T17:48:34Z); remaining OQ-109 tail: reading_diff re-point,
stability table, σ/seat eval (frozen prediction `5f2a626c`), close-out. Evidence
audits/2026-06-12_cohort_zero/.

## 2026-06-12 — DETERMINISM-FRONTIER ruling promoted to CLAUDE.md; Phase C removal commit (schema perspectives[]/mandatrophy_resolved OUT, provenance/8 REQUIRED incl. model+sampling); archive-before-removal executed; replicate probe folded into cohort zero
**Files:** CLAUDE.md, schemas/constraint_story_schema.json, python/generate_constraint_pl.py, prolog/narrative_ontology.pl, prolog/guard_exclusions.pl, prolog/signature_detection.pl, prolog/stack.pl, agent/example_platform_commission.json, ISSUES.md, prolog/archives/datasets/pre_cohort_zero_2026-06-12/
**Tier:** landed

Ruling promoted to CLAUDE.md Critical Distinctions: generation NEVER reproduces; committed JSON is
the CHECKED determinism frontier; regens are NEW DRAWS; attribute same-material-different-results by
stage-hash diff, never assumption (record check witnessed OQ-26, the press/Reformation triplet,
OQ-112 class). Phase C removal commit: perspectives[]/mandatrophy_resolved out of schema,
provenance/8 REQUIRED (model + sampling_params); archive-before-removal executed
(prolog/archives/datasets/pre_cohort_zero_2026-06-12/: 62 pl + 60 json, schema-pinned at
`046e0a40`); replicate probe folded into cohort zero. Witnesses W1–W5
(c_removal_commit_witnesses.out) + GATE-0 W1–W3 (c_gate0_exclusion_witnesses.out).

## 2026-06-12 — OQ-114 RESOLVED: archive probe under frozen criterion → OUTCOME 3 (mixed) → operator ruled the live 3 SPLIT (2 in / trust_erosion out, kill conditions + fail-closed exclusion + named re-witness); rider: no-beneficiary conjunct WRONG
**Files:** ISSUES.md, audits/2026-06-12_oq114_archive_probe/
**Tier:** landed

Archive probe under frozen criterion (`c64f32a6`): kernel_v1 41 / v6 430 mountain-claimed → OUTCOME
3 (both duplicate-seat artifact and substantive distinct-seat dissent in both archives; NL trio
filters NOTHING on archives — C ≡ claim-mountain there). Operator ruled the live 3 SPLIT:
organization_floor + demographic_skill_mismatch IN (named re-witness at Phase C);
institutional_trust_erosion OUT with kill conditions both directions + a FAIL-CLOSED per-story
exclusion as the Phase C build item. Rider: option 4's no-beneficiary conjunct was WRONG, not over-
restrictive. Evidence audits/2026-06-12_oq114_archive_probe/.

## 2026-06-12 — OQ-109 B4 gauntlet PASS against a pre-compiled expected-divergence manifest; Phase C ordering pinned (OQ-114 first); OQ-115 filed (check_stack divergence attributed pre-Phase-B)
**Files:** ISSUES.md, audits/2026-06-11_oq109_phase_b/B4_EXPECTED_DIVERGENCE_MANIFEST.md
**Tier:** landed

Gauntlet PASS against the pre-compiled manifest
(audits/2026-06-11_oq109_phase_b/B4_EXPECTED_DIVERGENCE_MANIFEST.md): pipeline green, plunit 14/14,
rows 1–10 reconciled; the one unmanifested check_stack finding (abductive_helpers phantom-module
under [stack], OQ-57 class, present at pre-Phase-B `c22ec561`) attributed → OQ-115, not Phase-B-
attributable, non-blocking. Phase B COMPLETE; Phase C ordering pinned: OQ-114 ruling → C-arm
extension confirmed → regen.

## 2026-06-12 — OQ-109 B3: empty-table census CLOSED (A1–A6, B1–B3 all discharged); narrative_ontology A3/A4 detectors retired; linter migrated to agent-surface dispatch; gaps key carries coverage bit
**Files:** prolog/narrative_ontology.pl, python/linter.py, prolog/test_harness.pl, prolog/json_report.pl, prolog/report_generator.pl, python/shared/schemas.py, audits/2026-06-11_oq109_phase_b/
**Tier:** landed

Census A1–A6/B1–B3 all discharged (closure table b3_empty_table_census.md,
audits/2026-06-11_oq109_phase_b/): five zero-consumer narrative_ontology detectors retired (grep
positive-controlled, successors named; has_mandatrophy_declaration KEPT); linter migrated to agent-
surface dispatch (B2 example lints 5→0; corpus sweep 92→80 fully decomposed); A5 gaps made nullable
(null=didn't-look vs []=measured-empty; the enrich validator caught it loudly first); A2/A6 carry
ran-witnesses. Remaining B3: none — next is B4, then Phase C.

## 2026-06-12 — OQ-109 B3: R5 zombie consumer LANDED (A7 seam recovered, first consumer of zombie_piton_crosscheck/2); CLAUDE.md mandatrophy note retired per its own condition; presence gates + emission seam landed same day
**Files:** prolog/report_generator.pl, prolog/data_validation.pl, python/generate_constraint_pl.py, CLAUDE.md, ISSUES.md, audits/2026-06-11_oq109_phase_b/
**Tier:** landed

r5_zombie_crosscheck_line/1 landed as the FIRST consumer of
stakeholder_seats:zombie_piton_crosscheck/2; pre-registered witness shape held exactly (6 live
firings, quiet control clean, pipeline JSON untouched; corroborated_zombie=0 witnessed ONLY by the
overlay control). CLAUDE.md mandatrophy tripwire RETIRED per its own condition; presence gates (5
two-sided controls) + census-B1 emission seam landed same day. Residual: mandatrophy_resolved still
a dangling schema field until Phase C. Gotchas: Section 7 is subject-scoped; data_validation NOT
loaded by [stack]. Evidence audits/2026-06-11_oq109_phase_b/.

## 2026-06-12 — SPEC CORRECTION: unanimity bridge disjunction → conditional dispatch; extension change fully reverted (byte-identical witness); OQ-114 exposure window recorded; ensemble-decomposition practice note banked
**Files:** prolog/signature_detection.pl, ISSUES.md, docs/technical/build_discipline.md, audits/2026-06-11_oq109_phase_b/
**Tier:** correction-key

The `790bb009` bridge landed as old ∨ C — but C ⊇ old, so the union IS C's extension: the 3-story
protection, FCR un-fire, and regulatory_measurement_gap yellow→red were LIVE on main for a same-
day window, pre-answering OQ-114 (spec under-specification: "ordered so the authored path decides"
meant DISPATCH, was written disjunction; the 9/62 extension witness was in hand and not read).
Fix: conditional dispatch (authored cells present → old semantics verbatim); pipeline diff vs the
PRE-BRIDGE baseline BYTE-IDENTICAL (`b3_unanimity_dispatch_diff.out`,
`audits/2026-06-11_oq109_phase_b/`). OQ-114 carries the exposure-window note. Banked:
`build_discipline.md` → "Extension-touching diffs decompose into direct targets vs ensemble refit"
(determinism control the standard companion).

## 2026-06-12 — OQ-109 B3 unanimity guard RULED+LANDED: option-2 bridge (authored-cells ∨ nl_certification_chain); census A1 seam closed; OQ-113/OQ-114 filed; output-changing (3 targets + ensemble cascade)
**Files:** prolog/signature_detection.pl, ISSUES.md, audits/2026-06-11_oq109_phase_b/
**Tier:** landed

Both named criterion candidates FAILED the pinned gauntlet (natural_law_signature unsatisfiable by
construction → OQ-113); operator-ruled option-2 bridge landed: authored-cells arm first (dies at
Phase C, named retirement point) ∨ fail-closed nl_certification_chain/1. Extension 9/62 = old 6 + 3
(OQ-114 adjudicates the 3). Output-changing commit alone: institutional_trust_erosion FCR un-fired
(→ coupling_invariant_rope), 3 maxent_top_type piton→rope, regulatory_measurement_gap verdict_join
yellow→red; 57-story ensemble cascade determinism-controlled (same-code rerun byte-identical).
Gotchas: ε lives in domain_priors:base_extractiveness/2; emerges_naturally is static+multifile
(overlay via consulted scratch testset). Evidence audits/2026-06-11_oq109_phase_b/.

## 2026-06-11 — OQ-109 Phase B1+B2 LANDED: prompt cutover to stakeholder surface; new one-shot example (FNL statistics reset No. 2); schema/compiler perspectives-optionality (guard-not-delete)
**Files:** prompts/constraint_story_generation_prompt_json.md, agent/example_platform_commission.json, agent/story_generator_base.py, schemas/constraint_story_schema.json, python/generate_constraint_pl.py, ISSUES.md, audits/2026-06-11_oq109_phase_b/
**Tier:** landed

B1: prompt cutover to the stakeholder surface (P/T/E/S tuple + Indexed Classifications dropped,
1008→872 lines; 11 tuple terms 0 post — b1_vocab_grep_witness.out). B2: new one-shot example
app_store_commission (FNL statistics reset No. 2; minimum-prevalence pick, example_prevalence.out;
mutated per EXAMPLE_INHERITED_SIGNATURES.md); B2 changed perspectives OPTIONALITY only (guard-not-
delete; existing corpus compiles byte-identical). Known pre-B3: linter fires on the example
(b2_example_validation.out); 12/60 live-paired JSONs fail schema in BOTH states
(b2_schema_failset_diff.out). Worktree oq109-phase-b; evidence audits/2026-06-11_oq109_phase_b/.

## 2026-06-12 — OQ-103 RESOLVED: contamination-edge provenance made load-bearing + count-based salience floor at the read site
**Files:** ISSUES.md, prolog/json_report.pl, python/enhanced_report.py, python/tests/test_contamination_provenance_salience.py
**Tier:** landed

Scope-corrected at close: the provenance bit already existed (`edge_type == explicit` is the
authored-vs-derived bit); defects were an inert bit + no salience floor. Read-site fix: json_report
emits shared_agent_count per neighbor; enhanced_report gains Provenance/Salience columns + legend +
floor (authored always salient; derived agent edge iff count≥2; inferred_coupling strength≥0.6).
Witness: pipeline n=62, 82/106 (77%) edges demote to low-salience; unit test 5/5. Commit `ded4969d`
(merge `1bb6e535`); synthesis enforcement stays OQ-101.

---

## 2026-06-11 — OQ-112 item-4 sentinel trace: verdict SILENT (three mechanisms); absorber-boundary class elevated to item 2; maxent_indexed_run order dependency found
**Files:** ISSUES.md, audits/2026-06-11_oq112_item4_sentinel_trace/, prolog/maxent_classifier.pl, prolog/json_report.pl
**Tier:** landed

Verdict SILENT via three absorber mechanisms: catch-true (json_report.pl:72/:76,
trajectory_mining.pl:912), catch-fail row drops (maxent_report.pl:211, maxent_diagnostic.pl:395),
and clause-failure-before-arithmetic (W12a — the sink a catch-grep cannot see); firing set EMPTY on
the live corpus. Absorber-boundary class elevated to OQ-112 item 2; maxent_indexed_run hidden order
dependency on maxent_run found. Tripwire: maxent_profile/4 is empty until maxent_run(Ctx) runs in-
process — witness profile-present before trusting a sink result. Worktree oq112-item4-trace from
`009c793a`; evidence audits/2026-06-11_oq112_item4_sentinel_trace/.

## 2026-06-11 — OQ-97 RESOLVED: Pattern-6 census executed (160/227/210 raw lines, 19 classes); 8 candidate classes filed as OQ-112; classification path clean
**Files:** ISSUES.md, audits/2026-06-11_oq97_pattern6_census/
**Tier:** landed

Bounded grep census over 106 top-level prolog/*.pl (160/227/210 raw lines, 19 classes; all 7 pinned
positive controls fired — two grep iterations rejected by them). No confirmed candidate on the
dr_type path (drl_core.pl zero Shape-A hits — the census itself witnesses the OQ-44 commit-C fix); 8
candidate classes filed as OQ-112 (top: diagnostic_summary absence-of-alert, 13 sites, feeding the
OQ-98 join; item 4 = the unknown sentinel into maxent). Worktree oq97-pattern6-census from
`1bfd0b72`; evidence audits/2026-06-11_oq97_pattern6_census/.

## 2026-06-11 — OQ-110 RESOLVED: residual join + pinned counterfactuals; operator ruled D-fork branch b NO-OPEN (derived-d stands); Backed deposit chain discharged
**Files:** ISSUES.md, python/audits/oq110_residual_join.py, audits/2026-06-11_oq110_residual_join/, prolog/temporal_residual.pl, prolog/drl_composition.pl, prolog/json_report.pl
**Tier:** landed

Backed verified end-to-end — the OQ-33 → OQ-46 → OQ-83 → OQ-110 deposit chain TERMINATES; join
coverage both=11/62, flips_only=23, stages_only=4; all 91 backed flips pinned: 82 ε-explained / 9
supp-explained / 0 genuinely unexplained. Operator ruled D-fork branch b NO-OPEN (derived-d stands;
reopen = a backed flip surviving BOTH pins on a future join). Fresh pipeline manifest
2026-06-12T00:59:49Z at `c22ec561` (prior dirty-tree `25d6a637`; flip totals identical). Gotcha:
json_report.pl is a NON-module script — predicates live in user. Evidence
audits/2026-06-11_oq110_residual_join/.

## 2026-06-11 — OQ-99 + OQ-100(a–c) RESOLVED: omega scenarios render authored protocols (subject-bound, fail-loud); report register coherence (qualified confidence labels, rival-P-graded disagreement, self-consistency header)
**Files:** prolog/report_generator.pl, python/enhanced_report.py, python/enrich_pipeline_json.py, agent/orchestrator.py, ISSUES.md
**Tier:** landed

OQ-99: omega scenarios render authored 5-arity protocols, subject-bound + fail-loud
unresolved_source (never Constraint: unknown); plan-correction kept — the facts live in
constraint_<id> modules, NOT user. OQ-100(a–c): qualified confidence labels, rival-P-graded
disagreement header (BAND_DEEP/BAND_MODERATE constants in enrich_pipeline_json.py), fraud header →
DECLARED-TYPE self-consistency; orchestrator regex updated and verified. Commits `6b1092c0` +
`e9872538` (worktree oq99-omega-scenarios); wrong-module sweep filed one finding as OQ-111;
OQ-100(d) subsumed by OQ-101.

---

## 2026-06-11 — OQ-83 RESOLVED: measurement close-out; snapshot_type determinism guard; v7 §4.5 (A)/(B) census; OQ-109/OQ-110 filed
**Files:** ISSUES.md, prolog/transition_paths.pl, docs/deferential_realism_paper_v7.md, audits/2026-06-11_oq83_close/
**Tier:** landed

Operator-gated measurement close-out: R4 ruled SATISFIED (n=6 pilot diff = produced-and-preserved);
Ω_P transferred, not answered (observer Type-B foreclosed per TWO_AXIS; committer C/B → OQ-87).
Classifier-sync item 5: snapshot_type/3 now clears the classify_at_time nb-globals at entry
(determinism-fix-plus-document; witnesses + controls pasted, suite 0 warnings); new ε-sourcing
mismatch challenge_as_commons_maintenance T=5 recorded. v7 §4.5 amended: one (A) data bridge vs ≥3
(B) seam diagnostics. Spin-offs OQ-109 + OQ-110; census substrate = archives/datasets/kernel_v2_test
(archived at `00c639da`). Evidence audits/2026-06-11_oq83_close/.

## 2026-06-11 — Pew-typology review exchange landed: hedging-as-rigor dual, false-summit authoring discipline, OQ-107/OQ-108 filed, OQ-103 escalated
**Files:** docs/technical/build_discipline.md, CLAUDE.md, docs/design/design_discipline.md, ISSUES.md, prolog/testsets/institutional_trust_erosion.pl
**Tier:** landed

Landed: hedging-as-rigor (the under-confident dual) → build_discipline.md + CLAUDE.md synthesis item
(4); false-summit authoring discipline → design_discipline.md §4 (witness:
institutional_trust_erosion.pl:125 authored constraint_claim mountain, engine refused — the refusal
became the essay's spine); OQ-107 (survey-wave witness adapter) + OQ-108 (per-position witness-
coverage report) filed; OQ-103 escalated to load-bearing (trust↔representation shared_victim edge is
corpus-topology, institutional_trust_erosion_report.md:142); "the mint" queued as an OQ-69 ledger
item. Source: agent/analysis/originals/Pew_2026.5.10_political-typology_topline.txt.

## 2026-06-11 — OQ-90 RESOLVED: capture-keyed piton refinement in the FCR branch (piton un-darkened)
**Files:** prolog/signature_detection.pl, prolog/narrative_ontology.pl, prolog/config.pl, prolog/config_schema.pl, prolog/signature_mapper.pl, prompts/constraint_story_generation_prompt_json.md, ISSUES.md
**Tier:** landed

Piton un-darkened: capture-keyed refinement in the FCR branch — piton_candidate/1 (uncaptured ∧
prohibitive fixing_cost) gates a new resolve_with_perspectival_check clause; dr_signature stays
false_ci_rope, only dr_type becomes piton; `piton_refinement_enabled` is a SEPARATE axis from
fcr_override_enabled (dedicated kill-switch). Output delta 0→1: exactly 2 rows tangled_rope→piton
(regulatory_measurement_gap, institutional_trust_erosion); the 4 untracked working-tree testsets
must be committed for the 2-row result to reproduce. Superseded-pending: drl_core.pl:344,403 theater
piton clauses, maxent_classifier.pl:153–155 default_profile, axiom_reachability.py:171,207. Commits
`f2368073`/`64448411`/`fc724ab2`/`3a4e0209`; audit audits/2026-06-11_oq90_piton_refinement/.

## 2026-06-11 — OQ-44 RESOLVED: fail-closed-on-absence ruled (statute for new gates, marker carve-out, common-law for existing); OQ-43 closed; thermal_dissipation_constraint un-certified
**Files:** prolog/signature_detection.pl, prolog/drl_core.pl, python/shared/schemas.py, ISSUES.md
**Tier:** landed

Ruled: STATUTE — new/modified gates fail closed on absence; carve-out — absence→authored provenance
only via positive-control inference at authoring/compile time (suppression_profile precedent), never
emptiness-inference at the read site; existing gates common-law per-instance. Dispositions:
has_viable_alternatives false→unknown (`8b5a34b8`, output-changing — thermal_dissipation_constraint
UN-CERTIFIED, verdict green→red, all 277 diffs single-cause); get_raw_suppression 0→unknown sentinel
+ number/1 guard (`966d53c8`; shared/schemas.py suppression nullable); report-layer 0.0 defaults
conforming as-is. OQ-43 resolved in the same stroke. Witnesses audits/2026-06-11_oq44_policy_close/.

## 2026-06-12 — First-contact gate C-range corrected: slot-count!=32 removed (partial grids are LEGAL); first misfire had halted the pipeline on an OQ-90 flip target
**Files:** python/grid_first_contact_gate.py, python/grid_audit_ledger.json
**Tier:** landed

slot-count!=32 removed from the standing gate (the BATCH addendum's full-grid mandate had leaked in;
partial grids are operator-confirmed LEGAL): C-range = value outside [0,1] OR duplicate slots;
C-flat evaluates the slot-groups present; partial grids pass with a coverage field + prompt-
compliance NOTE. The misfire had HALTED run_pipeline on institutional_trust_erosion (Pew run, 12/32
all-valid — an OQ-90 flip target); now passes as legal partial, OQ-90's two-row delta preserved;
pipeline exit 0 on the 62-corpus. Witness 6/6:
audits/2026-06-12_gate_partial_fix/gate_partial_fix_witness.txt.

## 2026-06-11 — OQ-93 FLIP RULED + EXECUTED: live prompt opt-in grid section; κ gate → first-contact gate; 10 batch stories promoted (corpus 48→58); two latent defects found by promotion
**Files:** prompts/constraint_story_generation_prompt_json.md, prompts/grid_batch_addendum.md, python/grid_first_contact_gate.py, python/grid_audit_ledger.json, python/run_pipeline.py, python/python_test_suite.py, prolog/data_repair.pl, prolog/validation_suite.pl, json/, prolog/testsets/
**Tier:** landed

Flip ruled + executed: live prompt opt-in grid section; the one-time κ gate became the standing
FIRST-CONTACT gate (per-story fail-closed, ledgered in python/grid_audit_ledger.json; C-echo halts
run_pipeline); 10 batch stories promoted, corpus 48→58 — first live-corpus grid consumption
(witnesses first_contact_gate_witness.txt, flip_promotion_witness.txt, flip_promotion_suite.txt).
Two latent defects found by promotion and fixed: data_repair:grid_provenance read measurement/5 with
the interval ANONYMOUS (56/58 cross-reads once ten grids coexisted; now interval-scoped) and
python_test_suite's unanchored interval regex matched prose (phantom test IDs; now anchored to the
compiled fact form). TRIPWIRE: every pre-promotion 0-diff baseline is of-its-substrate (143→153
json, 48→58 corpus) — re-run before reuse.

## 2026-06-11 — OQ-93 grid migration LANDED end-to-end (stages A–D + coverage read + shim retirement); OQ-96/OQ-101/OQ-102 closed with it; intent sub-fork filed as OQ-106
**Files:** schemas/constraint_story_schema.json, python/generate_constraint_pl.py, prolog/coercion_projection.pl, prolog/pattern_analysis.pl, prolog/intent_engine.pl, prolog/report_generator.pl, prolog/signature_detection.pl, prolog/drift_report.pl, prolog/diagnostic_summary.pl, prolog/json_report.pl, prolog/narrative_ontology.pl, prolog/config.pl, prolog/config_schema.pl, prolog/scenario_manager.pl, prolog/data_repair.pl, prolog/data_verification.pl, prolog/domain_priors.pl, python/enhanced_report.py, python/run_pipeline.py, python/domain_priors.py, python/shared/schemas.py, python/tensions_ledger.py, agent/c-orchestrator.py, agent/generate_grid_batch.py, prompts/grid_batch_addendum.md
**Tier:** landed

Stages A–D landed end-to-end: schema coercion_grid + basis rider; compiler *_grid_NN emission with
fail-loud integrity (143/143 byte-identical); coverage read — system_gradient's []→0.0 fabricated
default KILLED, empty reads → OPEN; Stage C N=10 batch PASS 0/10 excluded (gradient compound-guard
bug fixed en route); Stage D level_gradient_divergence wired into FCR/FSM + extraction-blindness
omega; shim retirement closes OQ-96 (per-class counts identical, justified-wording diff recorded).
OQ-102 closed (basis chain + drift-severity confidence) and OQ-101 closed (tensions_ledger.py
replaces _step_essay); intent sub-fork → OQ-106. Pending-at-entry: the live-prompt flip (batch
parked in audits/2026-06-11_oq93_grid_migration/grid_batch/). Audit package
audits/2026-06-11_oq93_grid_migration/; branch oq93-grid-migration, commits `bc41e8f4..`.

## 2026-06-11 — Backed semantics BUCKETED (follow-on to the OQ-46 close): compiler-stamped suppression_profile(static) sanction marker; OQ-105 filed; OQ-37 piton vacuous-green fixed
**Files:** prolog/drl_composition.pl, prolog/narrative_ontology.pl, python/generate_constraint_pl.py, prolog/data_validation.pl, prolog/testsets/thermal_dissipation_constraint.pl, ISSUES.md
**Tier:** landed

SuppBacked bucketed, keyed on the compiler-stamped suppression_profile(C, static) sanction marker
(never emptiness-inference): marker-sanctioned scalar backs / grid-misalignment substitution
excluded (→ OQ-105) / unmarked seriesless fails closed. Decision witness: bucketed = 59 flips / 20
fab_adjacent vs blanket 79/0 (laundering); scalar==series-endpoint 37/39 (0 violations); pipeline
A/B 30 diffs = 28 backed_times + 2 manifest. Also fixed the OQ-37 piton vacuous-green (unconditional
"No pitons detected" → VACUOUS notice + joined-table sizes). Commits
`00040bb9`/`b0a0e380`/`609dbb47`; evidence audits/2026-06-11_oq46_backed_reconciliation/.

## 2026-06-11 — OQ-46 RESOLVED: the classify_at_time scalar suppression fallback is SANCTIONED (operator ruling), not a retirable stopgap; OQ-46's premise contradicted the live generation prompt
**Files:** prolog/drl_composition.pl, docs/technical/classify_at_time_wiring.md, prompts/constraint_story_generation_prompt_json.md, ISSUES.md
**Tier:** landed

Operator ruled: accept the prompt's design — since `220739b8` the prompt
(constraint_story_generation_prompt_json.md:457) deliberately authors scalar-only suppression for
static-enforcement stories, so the retirement premise never terminates. Read ladder permanent:
temporal at T → scalar-as-constant Backed=false → fail-closed unknown; deletion counterfactual would
flip 16/46 timelines; snapshot_type/degradation_chain have zero consumers (positive-controlled).
Comment-only edits (STOPGAP → sanctioned); wiring doc §1 re-ruled; OQ-33/OQ-40/OQ-41 cross-refs
updated; the two *_contradictions files explain the 48/46 denominator gap. Evidence
audits/2026-06-11_oq46_close/ (branch oq46-ruling).

## 2026-06-11 — Tripwire: the moderate→yellow verdict cap is confirmed-but-never-stressed; re-rule evidence arrives with the first correction-grade signature on a base-GREEN constraint
**Files:** prolog/diagnostic_summary.pl, prolog/signature_detection.pl
**Tier:** tripwire

Promoted to CLAUDE.md Architecture Invariants 2026-08-10 (beside the verdict_join rule).
Verdict: zero moderate caps have ever shipped (all 13 correction carriers base ≥ yellow); the
first correction-grade firing on a base-GREEN constraint is the deferred re-rule evidence —
re-run `audits/2026-06-11_oq98_verdict_join/histogram_gate.pl` and surface to the operator.
Cross-listed in OQ-93's fire-on-migration witnesses (kappa CONDITIONAL tail is the other
dormant OQ-98 path).

## 2026-06-11 — OQ-98 RESOLVED: report headline verdict is now verdict_join (Prolog-side join over alerts + provenance, serialized with raw inputs); schema_version 1→2
**Files:** prolog/diagnostic_summary.pl, prolog/signature_detection.pl, prolog/json_report.pl, prolog/report_generator.pl, python/enhanced_report.py, python/run_pipeline.py, python/shared/schemas.py, ISSUES.md, audits/2026-06-11_oq98_verdict_join/
**Tier:** landed

Headline = diagnostic_summary:verdict_join/3 (base verdict + severity-floored alerts via
signature_grade/severity + grid/measurement provenance), serialized as a SIBLING of
diagnostic_verdict; enhanced_report headlines the join, prints per-alert reconciliation, renders
[UNJOINED] on stale artifacts; schema_version 1→2. Corpus effect: 8/48 headlines changed (6
green→red, 2 yellow→red, all severe claim-mismatch), zero moderate caps; P1 probe ruled BRANCH A —
no diagnostic subsystem is grid-fed, so grid-diet lines carry [CONDITIONAL] tags. Tripwire promoted
to CLAUDE.md Architecture Invariants. Commits `e8ab707b` → `170db693` → `ce9a26ec`; witnesses W1–W4
+ 2 falsifiers audits/2026-06-11_oq98_verdict_join/.

## 2026-06-10 — OQ-95 resolved: constraint_neighbors/3 now fail-closed on phantom (zero-fact) constraints; giant_comp edges scoped to enumerated nodes; domain_registry throw hit independently (folded into OQ-96 at merge)
**Files:** prolog/drl_purity_network.pl, prolog/giant_component_analysis.pl, prolog/tests/test_phantom_neighbor_filter.pl, prolog/tests/test_forecloses_fpn_injection.pl, ISSUES.md, audits/2026-06-10_oq95_phantom_node_fix/writeup.md
**Tier:** landed

constraint_neighbors/3 made symmetric fail-closed on phantoms via phantom_subject/1 (neither
constraint_claim/2 nor constraint_metric/3); giant_comp edges scoped to the enumerated node set
(component > node-count impossible by construction). Witnesses: largest component 118.9%→56.8% live,
259.9%→89.2% on original_v6; gc edges 75→49 = exactly the 26 dangling affects_constraint/2 facts;
test_phantom_neighbor_filter.pl 4/4; fpn_injection 6/6; suite 39/39 (re-witnessed post-merge).
Contract change: a synthetic test constraint now needs a constraint_claim/2 to join the network.
domain_registry throw hit independently — folded into OQ-96 at merge (.gitignore:8 fossil,
run_pipeline.py:268 Pattern-1 producer, domain_priors.py --output absolute default). Evidence
audits/2026-06-10_oq95_phantom_node_fix/writeup.md.
## 2026-06-11 — OQ-33 RESOLVED: row-23 fail-close re-witnessed clean on live + kernel_v1; halt→disposition→control-gated clean re-scan; .gitignore unanchored-outputs tripwire found
**Files:** ISSUES.md, audits/2026-06-11_oq33_close/, prolog/drl_composition.pl, prolog/archives/pre_reset_outputs/, audits/2026-05-30_authoring_closure_fabricated_defaults/tripwire_fabricated_defaults_results.json, .gitignore
**Tier:** tripwire

Close re-witnessed clean: live 209 rows and kernel_v1-overlay 3,497 rows both 0 unknown-floor / 0
residual-0.5, every census carrying its own positive controls; OQ-46 annotated with live coverage
(7/46 scalar-only). Correction-key: the `drl_composition.pl:191-197` figures 471/562/91/0 are from
a never-archived 562-testset working tree (`b5ccee0d`), NOT kernel_v1 — pin the substrate (corpus
+ commit) for any exact-match expectation. Close path: HALTED on Probe D (4 pre-reset artifacts in
`outputs/`); operator disposition executed sha256-verified (archives →
`prolog/archives/pre_reset_outputs/`; the tripwire JSON moved into
`audits/2026-05-30_authoring_closure_fabricated_defaults/`); re-scan of 1,055 JSONs witnessed-
clean with in-run archive-side controls. The unanchored-`outputs/` .gitignore tripwire was
RESOLVED same day (`09390f0f`, rule anchored `/outputs/`; the 25
`audits/2026-02-25_spectral_laplacian/outputs/` evidence files, gitignored since creation,
recovered); residual citation-dangling routes → OQ-104. Evidence: `audits/2026-06-11_oq33_close/`.

## 2026-06-10 — External-review triage (two batches): OQ-98–103 filed; auto-essay synthesis ruled out (ledger replaces it); two topic runs committed under a live-witnessed gate
**Files:** ISSUES.md, audits/2026-06-10_external_review_vote_market/, audits/2026-06-10_external_review_xprize/, KNOWN_STATE.md, prolog/validation_suite.pl, agent/c-orchestrator.py
**Tier:** landed

Two batches triaged (external output = hypothesis, verified first): batch 1 (vote-market,
`2d54826c`) → OQ-98/99/100 + OQ-44/OQ-93 notes; batch 2 (XPrize, `96113b05`) → OQ-101/102/103 +
OQ-94 cross-ref. Load-bearing ruling: CUT orchestrator step 6 (Sonnet auto-essay) — the essay FORM
collapses plurality; deterministic tensions ledger replaces it (OQ-101); synthesis-fidelity
checklist lives in audits/2026-06-10_external_review_xprize/README.md. Runs committed under a live-
witnessed gate (run_dynamic_suite over 48, exit 0 —
audits/2026-06-10_external_review_vote_market/gate_witness.txt); essays/2026-06/who_owns_younger.md
left untracked; staged plan ~/.claude/plans/i-ran-an-article-merry-lagoon.md.

## 2026-06-10 — OQ-92 RESOLVED: gain_flow receipt surface live end-to-end (schema→compiler→prompt→batch→gates); GAP-10 closed; OQ-90 Steps 2–4 unblocked
**Files:** ISSUES.md, docs/design/design_gaps.md, prompts/constraint_story_generation_prompt_json.md, prolog/narrative_ontology.pl, prolog/drl_core.pl, prolog/maxent_classifier.pl, prolog/signature_detection.pl, prolog/data_repair.pl, prolog/testsets/gfbatch1/, audits/2026-06-10_oq92_step3_preregistration/
**Tier:** landed

gain_flow receipt surface live end-to-end: Stage C promoted stakeholders[] + six_questions + the
receipt surface into the LIVE generation prompt (additive; OQ-83 R4 control arm intact); first batch
gfbatch1 6/6 author gain_flow + fixing_cost, 0 diffuse, referential integrity clean; diffuse audit
0/0 observed — vacuous pass stated as vacuous. Stage D: constraint_captured/1 + OQ-94 benignity
gates rows 1–3 + maxent scaffold spec, two-sided controls all landed; fabrication-ban grep witnessed
in data_repair.pl; warning gate fired on a deliberate drift (allowlist 849→852). GAP-10 closed;
OQ-90 Steps 2–4 unblocked. Prereg audits/2026-06-10_oq92_step3_preregistration/.

## 2026-06-10 — OQ-96 interim landed (shim OFF, suite green, warning gate wired) + OQ-93 viability probe: gradient cut-bug found and fixed; all pinned values exact post-fix; intent top verdict range-dead witnessed
**Files:** prolog/config.pl, prolog/config_schema.pl, prolog/scenario_manager.pl, prolog/data_repair.pl, prolog/data_verification.pl, prolog/domain_priors.pl, prolog/coercion_projection.pl, python/run_pipeline.py, python/load_warning_gate.py, prolog/load_warning_allowlist.txt, audits/2026-06-10_oq93_grid_viability_probe/
**Tier:** tripwire

Standing: `grid_shim_enabled=false` (no injection/imputation; the throw-only `domain_registry`
refs removed); `python/load_warning_gate.py` + `load_warning_allowlist.txt` wired into
run_pipeline — never `grep -v Warning`. The OQ-93 probe (prereg `e7e78a1b`; FINDINGS in
`audits/2026-06-10_oq93_grid_viability_probe/`) found and fixed the `time_point_in_interval/2`
"(Optimized)" cut that made EVERY gradient ever computed fail into the `[] → 0.0` fabricated
default; post-fix all pinned values exact; `structural_coercive_intent` witnessed RANGE-DEAD;
rulings: intent top verdict RETIRED, keep-and-migrate for the masking/naturalization family,
imputation killed; interval scoping landed (leakage 312.5→0). Both channel pathologies ([]→0.0,
grep -v Warning) are PROMOTED — CLAUDE.md Build Discipline Pattern 6. Riders: once/1 is defense-
in-depth never primary semantics (stage-2 compiler enforces the duplicate-slot contract); partial-
grid answer = coverage-carrying G_sys + consumer-named-level requirements (witnessed 8/32 grid
read as full-system increasing_coercion).

## 2026-06-10 — OQ-94 read-site pass complete: rule sorted 12-file consumer surface; benignity-certification family escalated; prior 7-file census was head-truncated
**Files:** ISSUES.md, audits/2026-06-10_oq94_readsite_pass/READSITE_PASS.md, prolog/drl_core.pl, prolog/maxent_classifier.pl, prolog/signature_detection.pl, python/issues_status.py
**Tier:** correction-key

Census correction first: the recorded "seven-consumer list" was `head -15`-TRUNCATED — the
untruncated census finds 12 files / 33 sites, the concealed ones most load-bearing
(`drl_core.pl:346`/`:373`, the maxent boolean_spec mirror, omega1_audit); a probe-scope statement
must name its output limits. Sort: the NL/FSM mountain-likeness gates SOUND; the tangled_rope
cell, decay detection, `separability_factor`, and two NAF-voids FORBIDDEN; benignity-certification
ESCALATED then RULED GATE (rows 1–3; estimator-classifier congruence: any drl_core ruling lands in
maxent's boolean_spec same change; Q1 "0/N observed" never "clean"). Stages A–B landed same day
(8/8 schema cases, 0-diff 134/134); standing fact: 91/134 `json/` specs fail the CURRENT schema
(latent, expected residue). Bonus finds: `data_repair.pl:124-168` fabricates
`constraint_beneficiary` from metrics (OQ-93 circularity); `constraint_bridge.pl:96` first
gain_flow-migration candidate; `issues_status.py` now fails on duplicate OQ labels. Evidence:
`audits/2026-06-10_oq94_readsite_pass/READSITE_PASS.md`,
`audits/2026-06-10_oq92_step3_preregistration/PREREGISTRATION.md`.

## 2026-06-10 — OQ-81 ruled SUPPRESS and wired: reading-typed wave-upstreams dropped at seed build; A/B finds verdict import in the gradable channel (theater_ratio), absorbed before the categorical
**Files:** agent/generate_kernel_corpus.py, agent/c-orchestrator.py, agent/story_generator_base.py, ISSUES.md, audits/2026-06-10_oq81_reading_upstream_recon/
**Tier:** landed

Ruled SUPPRESS and wired: _flat_seeds_from_manifest drops reading-typed deps at both read sites (+
same predicate in the serial escape hatch, code-read sync only). Recon: ZERO historical exposure.
A/B (3 arms × 3 reps, injected verdict ≠ hypothesis): claimed_type held 9/9 snare, but the injected
verdict pulled authored theater_ratio 0.690→0.513 — verdict import in the GRADABLE channel, absorbed
before the categorical (categorical sticky, not safe; R-arm prose = positive control). Standing
cautions: injection-channel asymmetry is an instance (n=3), not an effect size; the CSR line poisons
vocabulary-based leakage probes in ALL arms. Full chain
audits/2026-06-10_oq81_reading_upstream_recon/ (RECON → AB_PLAN → AB_RESULTS → WIREUP).

## 2026-06-10 — OQ-77 closed: giant_comp SIGSEGV not serially reproducible (10/10 at exact crash size n=39; archives to n=3380) — concurrency artifact, operational rule promoted; OQ-95 filed (phantom network nodes)
**Files:** ISSUES.md, CLAUDE.md, prolog/giant_component_analysis.pl, prolog/drl_purity_network.pl, python/run_pipeline.py, audits/2026-06-10_oq77_serial_kill_condition/writeup.md
**Tier:** landed

Pre-registered kill condition executed: serial 10/10 rc=0 at the exact crash size n=39 (byte-
identical), 12/12 under 12-way co-residency, serial archive runs at kernel_v1 n=1106 + original_v6
n=3380 ×3 byte-identical → resolved as a concurrency artifact; one-pipeline-at-a-time rule promoted
to CLAUDE.md Running the System. Reopen path: any SERIAL segfault. Side-finding filed as OQ-95:
phantom network nodes (25 phantom atoms live, component 118.9%; ~2.6× on original_v6). Evidence
audits/2026-06-10_oq77_serial_kill_condition/writeup.md.

## 2026-06-10 — OQ-92 rulings recorded + step-2 gain-flow prototype PASSED 8/8: capture and fixing_cost separate on authored fields; step-3 surface build unblocked (OQ-92/OQ-90/GAP-10)
**Files:** ISSUES.md, docs/design/design_gaps.md, audits/2026-06-10_gain_flow_prototype/PREREGISTRATION.md, audits/2026-06-10_gain_flow_prototype/FINDINGS.md
**Tier:** landed

Rulings recorded (`4e04c2dc`): (a) build the authored gain-flow surface prototype-first; (b) ONE
authoring surface, TWO fields (gain_flow + fixing_cost; the binary-bit argument recorded as REJECTED
to prevent re-citation); tri-valued provenance authored-gain-to-NAMED-seat / explicit-diffuse /
absent-fails-closed; malformed-gain absorbs into fail-closed with a schema-rejection validation
item. Step-2 prototype (prereg `eb24a927`, eight-control battery): Outcome 1 PASS 8/8 — case 5 vs 4
witnessed fixing_cost load-bearing, OQ-90's decisive pre-wiring control discharged. Promotions:
scaffold-push collision homed as OQ-94; diffuse-gate tolerance/sample RESERVED for operator at
step-3 prereg. Evidence audits/2026-06-10_gain_flow_prototype/ (PREREGISTRATION.md, FINDINGS.md).

## 2026-06-10 — OQ-57 re-witnessed post-reset: resolution holds; original behavioral witnesses were pre-reset/corpus-specific, now superseded by a corpus-independent positive control
**Files:** prolog/drift_events.pl, ISSUES.md, audits/2026-06-10_oq57_live_rewitness/FINDINGS.md
**Tier:** correction-key

OQ-57's fix re-witnessed (the original behavioral witnesses ran on the corpus reset 2026-06-05):
the `drift_events.pl:236` `domain_priors:` qualifier is durable — the pre-fix qualifier still
throws (diagnostic positive control, probe not vacuously clean); the original emitter set
reproduced exactly on kernel_v1 (kodashim byte-identical); a corpus-independent synthetic positive
control added (the witness the original entry lacked); 0/4,525 throws across live + archives.
Tripwire carried, NOT promoted (corpus-state-specific, self-resolving): the `internalized_piton`
clause is UNREACHED on the live corpus — "no drift throw" is a Pattern-5 vacuous pass until a low-
extraction/high-theater constraint re-enters; check reachability before claiming exercised.
Evidence: `audits/2026-06-10_oq57_live_rewitness/FINDINGS.md`.

## 2026-06-09 — OQ-93 opened + mitigated: imputation shim diagnosed (unmigrated v3.4 grid contract) and made visible via three-bucket provenance threading
**Files:** prolog/data_repair.pl, prolog/scenario_manager.pl, prolog/test_harness.pl, prolog/intent_engine.pl, prolog/report_generator.pl, ISSUES.md, audits/2026-06-09_imputation_shim_census/census.md
**Tier:** landed

Shim diagnosed as an unmigrated v3.4 grid contract: empty intersection — 0/32 grid points
authorable, ever, corpus-wide; fires only via scenario_manager:load_and_run; fabrication-fed
products are [INTENT] / verification gate / κ (MaxEnt is authored-fed). Phase 2 visibility landed
(witnessed): three-bucket [PROVENANCE] line — authored / injected-0.5 / imputed (a binary split
would launder injection into authored, operator correction) — plus stray-anchor [WARN] and diet
flags; report regen diff = provenance-lines-only; run_dynamic_suite 0 errors / 0 warnings. Producer-
vs consumer-side migration left as the unruled OQ-93 fork. Census
audits/2026-06-09_imputation_shim_census/census.md.

---

## 2026-06-09 — OQ-80 + OQ-08 closed: generate-step token totals threaded (hard-0 retired); DR/CS Π-asymmetry annotated in both mismatch report layers
**Files:** agent/generate_kernel_corpus.py, agent/c-orchestrator.py, prolog/json_report.pl, python/enhanced_report.py, python/tests/test_token_acc_threading.py
**Tier:** landed

OQ-80: token totals threaded via an optional token_acc out-param (None = NOT measured, never 0;
summed at receipt incl. parse failures); the hard 0 + "unthreaded" note retired. Witness:
python/tests/test_token_acc_threading.py (all three cases pass). OQ-08: cs_drift_mismatch_note
emitted (json_report.pl) + rendered (enhanced_report.py) — Π-asymmetric by design (DR instance-blind
at the fixed analytical context, CS context-free authored facts); witnessed both directions on each
layer; eventual permanent home = the OQ-15 mediator.

---

## 2026-06-09 — Three doc-sync OQs closed with witnesses: OQ-07 (mismatch candidate runtime-probed SILENT, blocking conjunct named), OQ-28 (seat-theorem amendment provenance), OQ-14 (bridge unblessed; mediator is the decided join)
**Files:** ISSUES.md, docs/seat-theorem-v1.md, docs/design/two_axis_architecture_v7.md, prolog/cs_drift_mismatch.pl
**Tier:** landed

OQ-07: candidate UID runtime-probed SILENT on archives/datasets/kernel_test (positive control: 11
corpus-wide firings); cs_is_metric_stable FAILS — the blocking conjunct named; verdict
architecturally-possible-but-not-this-case (audits/2026-06-09_oq07_mismatch_runtime_probe/). OQ-28:
docs/seat-theorem-v1.md Amendment-provenance section (witness asymmetry: §3 run-grounded via
test_forecloses_fpn_injection.pl; §5/§8 scope-declarations). OQ-14:
docs/design/two_axis_architecture_v7.md amended — the influences bridge unblessed (16 cross-axis
surfaces in 7 modules); the OQ-15 mediator is the decided-but-unbuilt join; three grep-enforceable
invariants recorded.

---

## 2026-06-09 — Capture-cut discriminating control HALTED (Outcome 2): `has_computed_capturer` proxy false-positives; capture needs an authored gain-flow surface (OQ-92 / GAP-10, gates OQ-90)
**Files:** ISSUES.md, docs/design/design_gaps.md, prolog/stakeholder_seats.pl, prolog/constraint_indexing.pl
**Tier:** correction-key

The pre-registered Step-1 control for OQ-90's capture cut → Outcome 2 HALT: the cut fires TRUE on
a mild-favorable non-capturer and an uncaptured DMV agenda_setter — χ
(`extractiveness_for_agent_d/4`) is extraction-FROM-seat, not gain-TO-seat, and every beneficiary-
side role gets low d, so the cut degenerates into "has a beneficiary-side-role seat at all";
`constraint_beneficiary/2` even pushes a capturer toward scaffold. Capture is NOT computed-
representable from current signals — needs the authored gain-flow surface (OQ-92 / GAP-10, gating
OQ-90 Steps 2–4); the proxy was NOT shipped. Promotion test applied in-entry: correction-key only
(the cut is rejected, not pending); covered at `stakeholder_seats.pl:86–88` + GAP-10/OQ-92.
Witnesses: `audits/2026-06-09_capture_axis_cut_control/` (PREREGISTRATION.md, FINDINGS.md).

---

## 2026-06-10 — Cell controls (witnessed): snare is capture-blind (`Supp ≤ 0.2` is not the piton discriminator); coordination "non-rope" cases scatter (FSM vs FCR) — falsification-grade; rebuild held (OQ-90/OQ-91)
**Files:** prolog/signature_detection.pl, docs/repair_dynamics.md, ISSUES.md
**Tier:** correction-key

Three pre-registered cell controls settled two theory claims BEFORE they landed: the DMV witness
(designed, enforced Supp 0.5, no concentrated capturer → snare) proves `Supp ≤ 0.2` is NOT the
piton discriminator and snare is capture-blind — capture and suppression separable on this witness
("orthogonal across the range" is the opened hypothesis, not established). The coordination non-
rope cases SCATTER (emergent → `false_summit_mountain`, designed-but-unmaintained →
`false_ci_rope`): "emergent coordination = one cell / piton's mirror" FALSIFIED; FSM home-vs-
shadow stays OPEN. Rebuild held for operator go (OQ-90/OQ-91); method: each control pre-registered
before the bash call. Witnesses: `audits/2026-06-
10_signature_liveness_crosscorpus/{dmv_cell_control.out,desirepath_cell_control.out}`.

## 2026-06-10 — Piton: agenda_setter is a BETTER proxy (the fixer role), but extraction<fixing_cost stays uncheckable; build as computed false_ci_rope refinement — OPEN pending the fixing_cost control (OQ-90)
**Files:** prolog/signature_detection.pl, prolog/stakeholder_seats.pl, prompts/constraint_story_generation_prompt_json.md
**Tier:** correction-key

Two in-conversation overreaches corrected: the piton gate's `resistance > 0.2` is CORRECT (a lossy
symptom-proxy, not sign-inverted), and `agenda_setter` (d=0.12, populated 22/57) is a better fixer
proxy but carries only ONE of the two piton terms — `extraction < fixing_cost` stays uncheckable,
so "representable via the stakeholder layer" was OPEN (lossy both directions). Design (operator-
ruled; spec in OQ-90, landed 2026-06-11): piton ⊂ `false_ci_rope` refined in-branch; the no-
capture test is COMPUTED per-seat χ, never authored beneficiary-absence (Pattern-5 / OQ-83 R3);
prompt fix non-leaky (drop the `theater_ratio ≥ 0.70` recitation — threshold-leakage). The build
tripwire (verify the `chi_for_stakeholder/3` sign convention before `seat_captures/1`, DMV-vs-
capturing positive control) was discharged by the OQ-90 build.

## 2026-06-10 — Cross-corpus signature-liveness sweep: 7/12 signatures LIVE, 5 dark everywhere; the fail-closed fix makes archive sweeps runnable (OQ-89)
**Files:** prolog/signature_detection.pl, prolog/corpus_loader.pl, audits/2026-06-10_signature_liveness_crosscorpus/
**Tier:** correction-key

Corrects the naive "doesn't fire on live n=34 ⇒ dead" read: swept `constraint_signature/2` across
four corpora (~5,222 stories, 0 throws — the 2026-06-09 fail-closed fix makes archive sweeps
safe). 7/12 signatures LIVE (natural_law 404 on v6 / 26 on kernel_v1, false_summit_mountain,
false_natural_law 15 on v5 — all zero on live = live-but-narrow); 5 DARK everywhere
(`coordination_scaffold`, `piton_signature`, `constructed_low_extraction`,
`constructed_constraint`, `ambiguous`) — evidence feeding the value question, NOT a cruft verdict
(Unwired ≠ worthless). Consistency: NL=404 reproduces the OQ-43 figure; FNL=0 on kernel_v1
corroborates the OQ-70 fix. Counts are liveness, never prevalence (OQ-70 bait, OQ-25 ID-reuse,
schema-drift abstention); the overlay recipe is PROMOTED to CLAUDE.md Corpus Loading. Matrix:
`audits/2026-06-10_signature_liveness_crosscorpus/MATRIX.md`.

## 2026-06-09 — `accessibility_collapse`/`resistance` now REQUIRED for all constraint types; `get_metric_average` fail-closes to `unknown` (was 0.5); 3 articles regenerated (OQ-89)
**Files:** prolog/signature_detection.pl, schemas/constraint_story_schema.json, prompts/constraint_story_generation_prompt_json.md, python/generate_constraint_pl.py, agent/c-orchestrator.py
**Tier:** landed

Root cause (audits/2026-06-08_coordination_washing_clean_pass/): the metrics were never authored for
non-mountains and get_metric_average defaulted to 0.5 > snare_epsilon_floor 0.46 — fabricating
constructed_high_extraction from no data. Landed: schema + prompt REQUIRE both metrics for ALL types
(fallback validator made consistent); engine fail-closes — 0.5→unknown, abstain clause, number/1
guards (0 throws; anti-over-abstain control byte-identical); 3 articles regenerated
(world3/magnifica/china; V5 substitution B==C for all 16 — fix is structural, not verdict-changing).
Residuals (OQ-89): full re-runs RE-DECOMPOSE into different axes (orphans left in place per operator
ruling; 9 corpus members abstain to unknown); ~94/116 legacy json/ lack the metrics; class
generalization deferred (cross-ref OQ-43/44).

## 2026-06-08 — Flat router stably under-routes a COUPLED methodological kernel (World3); false-mountain (mountain→rope) is a candidate missed-kernel signal (OQ-88)
**Files:** agent/c-orchestrator.py, agent/generate_kernel_corpus.py, ISSUES.md
**Tier:** correction-key

Routing discrimination works (china + World3 → FLAT; magnifica → KERNEL, seat layer reaching the
essay), but the flat path has a witnessed blind spot: World3's load-bearing policy-REGIME seat
routed stably FLAT across 2 samples — the gate decomposes a coupled kernel into independent axes +
epistemic omegas, dropping the coupling that makes it a kernel. Actionable finding: flat-routed ∧
false-mountain (authored mountain → computed rope, conf 0.01, witnessed 2× incl.
`demographic_skill_mismatch`) = candidate kernel false-negative → OQ-88 (N=2 is the positive
control; a negative-control sweep REQUIRED before auto-routing, else OQ-79's kernel-liberal over-
routing one level up; OQ-88 mitigated 2026-07-04). Also logged: a magnifica reading carrying 3
DANGLING `cs_reading_relation` edges (naming drift — the OQ-58 integrity sweep is skipped on the
no-scope/kernel path).

## 2026-06-08 — Register OQ-83 committer-stage-time / observer-residual fields in pipeline schema
**Files:** python/shared/schemas.py, prolog/json_report.pl
**Tier:** landed

The four OQ-83 emissions — cs_reference_frame, cs_drift_moment, cs_drift_gap (commit ef5a9188) and
temporal_residual (de3736a6) — added to PIPELINE_FIELDS as nullable declarations, ending ~280 [WARN]
unexpected-field lines per run. Witness: validate_pipeline_output + validate_enriched_pipeline both
0 errors / 0 warnings; producer side (json_report.pl) unchanged — schema caught up to the emitter,
not the reverse.

## 2026-06-08 — make_brief: source-abstraction tool for oversized/refusing inputs (canonical llm_call; measured ingest ceiling; STOP-by-default refusal)
**Files:** agent/llm_call.py, agent/make_brief.py, agent/c-orchestrator.py
**Tier:** tripwire

Built `agent/llm_call.py` (THE canonical Anthropic call path, consolidating the fix-#1 refusal
detection `7e85b261` into one spot — new callers import `agent.llm_call`, never the orchestrator,
whose hyphenated filename blocks import), `agent/make_brief.py` (NEUTRAL structural compression,
no READINGS partition, map-reduce above ~250 KB), and asymmetric orchestrator triggers: SIZE
auto-briefs only above the MEASURED ingest ceiling (`_ingest_decision`; decompose binds, ~175K
tok); REFUSAL stops by default, `--auto-bypass-refusal` opt-in with logged witness. Tripwires now covered at the edit sites (make_brief.py
header; `_ingest_decision` comments): a brief is LOSSY — never feed one when the doc fits whole
(don't reintroduce a KB default below the measured ceiling; magnifica feeds WHOLE); a neutral
brief of a SINGLE-VOICE source under-routes to flat without research grounding (witnessed spacex
S-1 — research is load-bearing for kernels from single-voice docs, and a `--skip-search` manifest
comparison is not apples-to-apples).

## 2026-06-08 — Type-A snapshot floor + observer residual detector landed (time-aware d; ε-driven flips are NOT empty — 56/100)
**Files:** prolog/constraint_indexing.pl, prolog/drl_composition.pl, prolog/transition_paths.pl, prolog/temporal_residual.pl, prolog/json_report.pl, prolog/stack.pl, audits/2026-06-08_typea_template_extensibility/, docs/deferential_realism_paper_v7.md
**Tier:** landed

Strict Tier-2 schema-deferred build: derive_directionality_at/4 + effective_time/3 (+ empty C1
hook), classify_at_time/5 surfacing snap(D, Backed, Eps, Supp, Theater), snapshot_type sync, and NEW
temporal_residual.pl (read-only category-B seam diagnostic, emitted per constraint by json_report).
Finding: the residual is NOT empty — 56/100 constraints, 155 counted flips, all observer-metric-
driven at frozen d (|Δε| median 0.07; bears on the D-fork: emptiness does not force branch b). V1–V9
verifications pass; V3 caveat: full classify_at_time ≡ snapshot_type is FALSE (3 mismatch points at
default context, sync OPEN; 2/52 flips flagged classifier-sensitive). v7 §4.5 corrected (one (A)
data bridge vs ≥3 (B) seam diagnostics); same-day committer stage-time enrichment landed
(cs_reference_frame/cs_drift_moment/cs_drift_gap beside cs_drift_terminal). Audit
audits/2026-06-08_typea_template_extensibility/.

## 2026-06-08 — Observer-side temporal review: the DR "trajectory" is mostly dark; three "defects" dissolved; three deferrals are ONE coupled ruling gated on time-varying-d
**Files:** prolog/drl_composition.pl, prolog/transition_paths.pl, prolog/drift_events.pl, prolog/cs_kernel_registry.pl, ISSUES.md
**Tier:** correction-key

Pre-rebuild observer-temporal review (merged late from worktree `sdm-temporal-records`; superseded
in part: scalar-as-constant SANCTIONED / OQ-46 resolved 2026-06-11; OQ-83 resolved →
OQ-109/OQ-110; the time-varying-d D-fork ruled NO-OPEN at OQ-110). Still-current findings: the
prior 471/562 coverage figure was pre-reset STALE; `BaseX=0.5` is REACHABLE-BUT-LOCKED (OQ-41 rows
24–25 corrected); the DR trajectory classifier was DORMANT positive-controlled
(`constraint_history`/`snapshot_type`/`degradation_chain` zero callers — later the OQ-182 revive
arc); fail-closed-vs-impute is the OQ-44 once-for-class ruling; the three deferred temporal
threads were ONE coupled ruling gated on time-varying-d (recorded on OQ-83, with the
check_capture_between launder mechanism). Meta (OQ-85 instance): the live load-bearing surface is
smaller than the activity around it — the rebuild is the carry-forward-vs-shed decision point.

## 2026-06-07 — Stakeholder-layer migration Pass-1 audit: computed path ignores authored perspectives (controlled null); straitjacket witnessed; mandatrophy surface is a dangling wire
**Files:** prolog/constraint_indexing.pl, prolog/drl_core.pl, prolog/constraint_data.pl, prolog/probe_harness.pl, prolog/inferred_coupling_protocol.pl, prolog/drl_purity_network.pl, prolog/reading_diff.pl, prolog/narrative_ontology.pl, python/generate_constraint_pl.py, schemas/constraint_story_schema.json, audits/2026-06-07_stakeholder_layer_migration/
**Tier:** landed

A1 keystone (controlled null): the computed classification path ignores authored perspectives —
flipping an authored classification is byte-identical 162/162 while the ε-overlay control moves
every register → the stakeholder layer is an additive refactor. A7: mandatrophy surface is a
dangling wire (zero compiler emissions; abandonment git-witnessed at `6f997d71`/`3641ae71`); A6
guard asymmetry split out as OQ-84 (guard landed with step 3). Phase A steps 1–3 + 4b/4c all
landed/ran same day: schema stakeholders[] + six_questions, compiler constraint_stakeholder/7 +
role-derived beneficiary/victim, engine seat layer (extractiveness_for_agent_d/4 byte-identical); 4b
fired RENAMED-NOT-ESCAPED → OQ-85 filed then RESOLVED silence-is-correct (in_contention feeds no
classifier; residual → OQ-86); 4c pilot n=6 — both flips a victim-count × critical_mass_threshold
resolution artifact; claim-layer framing effect the robust separate signal. Committer thread
banked/parked → OQ-87 (COMMITTER_THREAD_HANDOFF.md). Full report + evidence
audits/2026-06-07_stakeholder_layer_migration/ (AUDIT.md, STEP4_4b_RENAMED_NOT_ESCAPED.md,
OQ85_DECOMPOSITION_AUDIT.md, STEP4C_PARTITION.md); tracker OQ-83 rulings R1–R5.

---

## 2026-06-06 — Kernel-first router: `_step_decompose` now uses the PRIMED scope prompt (construction-as-classifier)
**Files:** agent/c-orchestrator.py, agent/generate_kernel_corpus.py, outputs/kernel_first_phase0/PHASE0_READOUT.md
**Tier:** tripwire

`_step_decompose` no longer builds the unprimed §3-independence prompt — it calls gkc
`_scope_user_prompt` (the PRIMED kernel-question prompt; single source, both front-ends share it),
closing OQ-79 mechanism-2 (the flat-miss that silently flattened magnifica). Witnessed: magnifica
→ is_contested_kernel=true (3 readings) where the unprimed path flattened; flat topic → reasoned
rejection; A3 grounding-leg dropped (wrong instrument). Both tripwires PROMOTED to CLAUDE.md
Critical Distinctions: never revert to the unprimed prompt (silently re-flattens genuine kernels),
and a kernel-positive means "admits a foundational construction", dominance UNJUDGED (kernel-
LIBERAL, uncurated accrual; seated dominance stage deferred). Phase 0 + widen evidence + ruling:
`outputs/kernel_first_phase0/PHASE0_READOUT.md`.

## 2026-06-06 — Generation-backend unification: c-orchestrator routed through the shared backend; the kernel-dropping fork DELETED
**Files:** agent/c-orchestrator.py, agent/generate_kernel_corpus.py, agent/story_generator_base.py, python/audits/capture_generation_payloads*.py
**Tier:** landed

The kernel-dropping fork (OQ-79 mech-1) healed by DELETION:
generate_kernel_corpus.generate_from_manifests is the single manifest→corpus path (seed-type
dispatch; c-orch _step_generate calls it; forked _step_generate_batch + delegators + dead imports
removed, grep 0; serial escape hatch kept). Witness ladder P0/W1/W2/P3/P4 in commits `0f61517c`,
`099066c4`, `a7d56a14`, `ed2ec212`: flat path byte-identical across cold processes and after the
splice; Zionism readings land with cs_kernel_id; seed-dup bug caught pre-live. TRIPWIRE: gkc --scope
still runs its own kernel generation (OQ-82); new OQ-80 (token totals) + OQ-81 (reading-upstream
appropriateness); OQ-76 still uncovered.

## 2026-06-05 — Pre-build ruling session executed: OQ-70/64/63 ruled and landed, intent_* declared GAP-08, perturbation-principle §1.1 added
**Files:** prolog/signature_detection.pl, prolog/constraint_indexing.pl, prolog/narrative_ontology.pl, schemas/constraint_story_schema.json, python/generate_constraint_pl.py, prompts/constraint_story_generation_prompt_json.md, docs/design/design_gaps.md, docs/the_perturbation_principle.md
**Tier:** landed

One principle (the_perturbation_principle.md §1.1, operator-authored): the authored layer's
definition is authoritative — the computed layer must never consume what the author did not assert.
OQ-70-A ruled as the CLASS (`72ec2cdd`: no signature reads a single authored perspective as a story-
level claim; live-20 FCR 16→5, FNL 3→1, positive control manpower_exhaustion_trap still fires);
OQ-64-A (`e5fbc2e8`: vindicated_propositions → constraint_vindicates/2, feeds no metric/gate);
OQ-63-A (`28f2dfc8`: d consumes agent_beneficiary, zero-diff 80/80 cutover + guard positive
control); intent_* declared design_gaps GAP-08 (`f618c1f1`) — residual pass-open noted as the OQ-43
fifth instance, fail-close deliberately deferred to its own ruling.

## 2026-06-05 — CORPUS RESET: live testsets/ rebuilt from scratch under the de-leaked pipeline; all previous corpora consolidated to prolog/archives/datasets/
**Files:** prolog/testsets/, prolog/archives/datasets/, CLAUDE.md, AGENTS.md
**Tier:** tripwire

Operator reorganization (by hand; commit `29889e50`, 13,532 renames): the pre-reset live corpus
(1,106 + stage1_probe/flatctl_probe/lineage_probe_01 run-tags) →
`prolog/archives/datasets/kernel_v1/`; testsets_3000 (3,380 chimera-era) → `original_v6/`;
testsets_sotu (189) → `sotu/`; new `testsets/` seeded with the first three post-de-leak topic
runs; `json/` reset to match same day (`1a0acfb8`, pre-reset specs → `kernel_v1_json/`). Tripwires
PROMOTED to CLAUDE.md Critical Distinctions: all pre-2026-06-05 empirical findings were measured
on kernel_v1 or its ancestors (re-witness before citing against live; retrospective audits overlay
`corpus_path`); run_pipeline reports n_sotu=0 (graceful — sotu analyses overlay the archive). The
first-pass new-vs-old comparison (3/20 new mountain claims all firing type_1_false_summit; 0.68
idiom 11/20; old comparison biased) stays in git history.

## 2026-06-05 — c-orchestrator batch generation (dependency waves); repair de-fanged; report highlights authored-vs-computed divergence
**Files:** agent/c-orchestrator.py, agent/story_generator_base.py, python/story_repair.py, python/enhanced_report.py
**Tier:** landed

_step_generate dispatches to a BATCH path by default (one Anthropic batch per §5.1 dependency wave;
--serial-generate / DR_SERIAL_GENERATE=1 keeps the legacy loop); build_prompt → build_prompt_parts
with a byte-parity witness; offline simulation witnessed wave partitioning, upstream context
injection, cache_control, 5/5 saved. Operator ruling folded in: stories are NOT linted at generation
and the authored side is never "fixed" — story_repair.py no longer fabricates mandatrophy_resolved
(witnessed) and enhanced_report.py renders the explicit Authored-vs-Computed divergence line
(witnessed both directions); batch path contains zero lint calls (grep = 0).

## 2026-06-05 — Generate-both landed: forced-flat control on every kernel, mechanical alignment key flat_control_of/2 (OQ-76 mitigated)
**Files:** agent/generate_kernel_corpus.py, python/generate_constraint_pl.py, prolog/testsets/flatctl_probe/, ISSUES.md
**Tier:** landed

Generate-both promoted to PRIMARY fix for the stochastic kernel/flat gate: flatten_manifests auto-
emits a <kernel_id>_flat_control seed per kernel (reading set never shown to the flat author);
compiler emits narrative_ontology:flat_control_of/2 outside the cs_structure gate (flat controls
carry no cs_kernel_id/cs_reading_relation); ASYMMETRIC by design (flat-on-every-kernel only).
Witnesses: compiler emission + negative control; seed/prompt independence on a real K1 manifest; E2E
run-tag flatctl_probe — first construction-pair diff: computed dr_type construction-ROBUST
(tangled_rope ×4 seats), authored layer divergent (snare ε=0.65 vs tangled_rope ε=0.48). Stage-2
residue: the readout stratum (OQ-76 Remaining). Writeup + probe + seed
audits/2026-06-05_flat_control_generate_both/.

## 2026-06-05 — K1 kernel-gate replication: real topic-classed boundary band; under-firing misses against explicit §1.3-K criteria (OQ-76 filed; Stage-2 condition)
**Files:** python/audits/kernel_gate_replication_probe.py, prompts/uke_scope_v2_json.md, ISSUES.md
**Tier:** landed

K3 hand-adjudication: gig classification and content moderation both pass all three §1.3-K criteria
→ flat takes are gate MISSES, not definitional ambiguity. K1 (k=8 × 5 topics, 40/40 calls, pre-
registered invalidation conditions): controls 0/8 and 8/8 (instrument valid); affirmative action
8/8, gig 5/8, content moderation 3/8 — the boundary band is real and topic-classed; noise localized
to the binary gate (conditional reading counts stable 4/3/3). Dispositions recorded in OQ-76
(interim kernel-bias hedge; generate-both candidate fix; K2 licensed); Stage-2 (OQ-75) carries the
routing condition. Writeup + 40 manifests + driver audits/2026-06-05_kernel_gate_replication/.

## 2026-06-05 — SCOPE count-distribution probe: 7-7-7 was coincidence + run noise, NOT an implicit target (OQ-75 watch resolved)
**Files:** python/audits/scope_count_distribution_probe.py, prompts/uke_scope_v2_json.md, agent/c-orchestrator.py, ISSUES.md
**Tier:** landed

Two-arm (current vs pre-`d179423d` SCOPE prompt) 8-topic battery, 16/16 calls, pre-registered
signatures incl. the masked-target sub-criterion: selected counts 3→11 track richness; upper tiers
spread among themselves (A: 5/6/6/11, B: 5/7/6/9); deferrals fire; replicate noise ±1; arms agree;
bridge replicate gig-economy 7→5 — the original 7-7-7 uniformity was mid-richness coincidence +
temp-0.2 run noise. Stage-2 (OQ-75) NOT gated on a SCOPE-framing fix; axis-count distribution at
scale is a readout, not a gate. Side observation: kernel-recognition itself noisy (T5 kernel in one
arm only). Writeup + 16 raw manifests + driver audits/2026-06-05_scope_count_distribution/.

## 2026-06-05 — Generation-pipeline de-leak: schema/prompt/feedback boundaries no longer hand the author the engine's bands (audit brief F1–F9)
**Files:** schemas/constraint_story_schema.json, prompts/constraint_story_generation_prompt_json.md, prompts/uke_scope_v2_json.md, python/linter.py, python/regenerate_stories.py, python/generate_constraint_pl.py, agent/c-orchestrator.py, agent/orchestrator.py, agent/uke_narrative_orchestrator.py, agent/story_generator_base.py, agent/generate_kernel_corpus.py, docs/logic_extensions.md, docs/technical/generation_path_resolution.md
**Tier:** landed

Binding leak was the SCHEMA, not the prompt: allOf conditionals tied claimed_type to numeric bands
and shipped verbatim in build_prompt — a claimed-mountain/high-ε story was literally unauthorable.
Landed with same-turn witnesses: `29cd45d4` (linter coordination_type 4→6, 286 false codes cleared;
canonical table → docs/logic_extensions.md), `9f2d050a` (schema de-leak; false summit authorable
after), `b6c4e113` (prompt maximal scrub; assembled-payload greps 19→0 and 28→0), `7ad86c5a` (axes
cap → optional ceiling; 7-7-7 uniformity → OQ-75 watch), `07f7b1c0` (regenerate_stories filters
THRESHOLD_COUPLED lint codes at the choke point), `d179423d` (lens-diversity SCOPE instruction —
SEPARATE change variable). Engine reads the claim only via diff detectors (drl_core.pl:566
dr_claim_mismatch/4; probe controls incl. synthetic false summit firing type_1_false_summit-severe);
new OQ-72/73/74 (55% = 158/286 re-witnessed). Canonical schema relocated same day to
schemas/constraint_story_schema.json (stale agent/data orphan deleted; all loaders witnessed;
docs/technical/generation_path_resolution.md + AGENTS.md updated; band grep re-run post-move: 0).

## 2026-06-04 — OQ-71 depth-lineage probe: SCALE RUN COMPLETE — H1/H3 falsified beyond noise (boundedness is within-regime only)
**Files:** prolog/testsets/lineage_probe_01/, docs/design/a_hypothesis_about_corpus_size.md, ISSUES.md
**Tier:** correction-key

[Compressed 2026-07-05; full text in git history.] The 438-story depth-lineage arm minted
distinct 5-dim structural classes at ~1.5× the same-generator breadth control at every matched n.
**Citation discipline: falsifies UNCONDITIONAL boundedness only — depth was confounded with seed
authorship; do not cite as depth-specific discovery until OQ-71's authorship-controlled arm runs.**
Full record: ISSUES.md OQ-71 (partial); finding in `a_hypothesis_about_corpus_size.md` §10.

## 2026-06-04 — OQ-71 depth-lineage probe: machinery pilot (generator run-tag routing, fingerprint probe validated by exact reproduction)
**Files:** agent/generate_kernel_corpus.py, python/lineage_fingerprint_probe.py, audits/2026-06-04_oq71_depth_lineage/
**Tier:** tripwire

[Compressed 2026-07-05; warnings retained; full text in git history.]
- **The regression gate for the no-scope request path is REQUEST-PAYLOAD identity, not story
  bytes** (generation is stochastic) — stubbed-client capture harness in
  `audits/2026-06-04_oq71_depth_lineage/gate2_capture.py`; re-gate any edit the same way.
- **`validate_reading_relation_integrity` writes its quarantine to the FLAT path**
  (`prolog/testsets/cs_reading_relation_quarantine.json`) even on run-tagged dirs — a run-tagged
  sweep silently clobbers a flat-corpus quarantine.
- `python/lineage_fingerprint_probe.py` is a validated six-dim fingerprint dumper (reproduced the
  v5 dump exactly; salvaged originals + md5s in the audit dir / OQ-71).

## 2026-06-04 — Probe/loading infrastructure hardening (gotchas → utilities; two commits)
**Files:** prolog/corpus_loader.pl, prolog/cache_registry.pl, prolog/probe_harness.pl, prolog/check_stack.pl, prolog/json_report.pl, python/run_pipeline.py
**Tier:** tripwire

[Compressed 2026-07-05; most warnings promoted → CLAUDE.md Corpus Loading / Running the System;
full text in git history.] Commits `1460e873` (behavior-preserving) + `801390a5`
(output-affecting): cwd-independent corpus loading, `corpus_empty` throw, `corpus_constraint/1`
registry, `cache_registry:clear_all_caches/0`, `probe_harness` overlay utilities, manifest
single-writer convention. **check_stack BASELINE (cited by CLAUDE.md; UPDATED 2026-06-18,
engine-only): 3 undefined-predicate references** — `data_repair:constraint_beneficiary/2`
(:134, :174), `data_repair:constraint_victim/2` (:147), `validation_suite:test_case/4`
(test_harness.pl:26) — plus load warnings; findings beyond this list = regressions; each tracked
with a non-bite witness under OQ-142 (OQ-143/OQ-144 annotate-only). Not a pipeline gate while
the baseline is non-empty.

## 2026-06-04 — OQ-65 detector-bait census COMPLETE: bait=2 (no new), omega-routed=75, 6/10 firings expectation-authored
**Files:** python/audits/oq65_bait_census.py, audits/2026-06-04_oq65_bait_census/
**Tier:** landed

[Compressed 2026-07-05; full text in git history.] Per-file census (7 channels, 10-assertion
self-test, blind decoys): explicit_bait 2/1106 (no new — OQ-63 scope qualifier CLOSED);
omega_routed 75 (6.8%); expectation-authored union 87 (7.9%); 6/10 FSM firings
expectation-authored. Method corrections (truncating omega regex; dual-anchor windows) recorded
in the audit dir. Artifacts: `audits/2026-06-04_oq65_bait_census/`; memory
`project_oq65_bait_census`; OQ-65 mitigated.

## 2026-06-04 — Audit corpus consolidated into `audits/<YYYY-MM-DD>_<slug>/` (location mandate)
**Files:** audits/, python/audits/false_ci_rope_audit.py, python/audits/scaffold_piton_gate_audit.py, python/audits/bc_coupling_audit.py
**Tier:** tripwire

[Compressed 2026-07-05; mandate promoted → CLAUDE.md Audit Methodology; move map + conventions
→ `audits/README.md`; full text in git history.] 22 subdirectories consolidated from docs/,
root packages, and gitignored outputs/. Convention: `outputs/` = live workspace, `audits/` =
dated archive. Consumers of `outputs/bc_coupling_audit.json` need
`python/audits/bc_coupling_audit.py` run first on a fresh clone.

## 2026-06-04 — FNL prevalence is template-bait-confounded (OQ-70): mechanism witnessed end-to-end, counterfactual run
**Files:** prolog/signature_detection.pl, agent/verification_bottleneck.json, audits/2026-06-04_fnl_bait_confound/
**Tier:** tripwire

[Compressed 2026-07-05; warning promoted → CLAUDE.md Critical Distinctions (OQ-70 block, resolved
2026-06-05); full text in git history + ISSUES.md OQ-70.] The 827/1106 FNL era rode
`claimed_natural/2` source 2 reading ANY authored mountain perspective as a naturality claim
(counterfactual: retraction → FNL→FCR 809, zero mass to genuine NL/CI_rope). Probe evidence:
`audits/2026-06-04_fnl_bait_confound/`. The `catholic_church_1200` demo-exclusion rule is also
in Critical Distinctions.

## 2026-06-04 — sheaf_status provenance traced end-to-end; arakelov_threshold now emitted + cited
**Files:** prolog/json_report.pl, prolog/arakelov_height.pl, prolog/sheaf_analysis.pl, python/enhanced_report.py
**Tier:** tripwire

[Compressed 2026-07-05; the "don't patch sheaf_analysis piecemeal, OQ-51 moves consumers
together" warning is SUPERSEDED by OQ-51's resolution (2026-06-25, promoted → CLAUDE.md
Architecture Invariants h1_band block); full text in git history.] Chain coherent (H¹, heights,
sheaf_status on one site); `arakelov_threshold` (corpus p75) now emitted as
`diagnostic.arakelov_threshold` and cited by enhanced_report on height-dependent regimes;
witnessed against independent recompute.

## 2026-06-04 — Schema drift fixed: `sheaf_status` added to `PIPELINE_FIELDS` (schemas.py)
**Files:** python/shared/schemas.py, prolog/json_report.pl
**Tier:** landed

[Compressed 2026-07-05; full text in git history.] Producer-side emission (205a8187) had no
validator-side whitelist entry → 1107 warnings/run (Pattern 1 in miniature: additive-to-producer
requires same-change schema sync). Fixed + enum check; witnessed clean run + positive controls.

## 2026-06-04 — Engine/shadow split anatomy (debt-ceiling probe): confidence-0 is wiring-determined for victim-less FSM hosts; filed on OQ-65/OQ-66
**Files:** prolog/maxent_classifier.pl, prolog/config.pl, prolog/signature_detection.pl
**Tier:** correction-key

[Compressed 2026-07-05; full text in git history.] For every victim-less FSM host,
engine=tangled_rope vs shadow p(tangled_rope)≈0 is structurally guaranteed (the shadow's
tangled_rope requires `has_asymmetric_extraction` ← `constraint_victim/2`) — confidence≈0 is
WIRING, not per-item calibration evidence. Residual signal is the shadow's TOP type. Recorded
as OQ-65 evidence; debt-ceiling scope-out at OQ-66.

## 2026-06-04 — Tracking-surface consolidation: AGENDA.md, AUDIT.md, TODO.md deleted; ISSUES.md is the single tracker
**Files:** ISSUES.md, CLAUDE.md, AGENTS.md, README.md
**Tier:** landed

[Compressed 2026-07-05; rule promoted → CLAUDE.md End-of-Session (single-tracker, Pattern 2);
full text in git history, deleted files last at `a1140d0d`.] Item-by-item substrate review
before deletion; still-live items became OQ-67/68/69.

## 2026-06-04 — Ledger sweep: five trivial OQs closed (11, 12, 13, 24, 42)
**Files:** ISSUES.md, prolog/config.pl, prolog/drl_purity_network.pl
**Tier:** landed

[Compressed 2026-07-05; full text in git history.] Four were already done in substrate with
stale ledger entries; OQ-24 needed one comment. Lesson (kept): "open" in ISSUES.md is a claim
about the substrate that can go stale in BOTH directions — a closure sweep needs the same
witness discipline as a fix.

## 2026-06-04 — OQ-57 RESOLVED: drift_events.pl:230 wrong-module qualifier (one-token fix, land-alone)
**Files:** prolog/drift_events.pl, prolog/json_report.pl
**Tier:** tripwire

[Compressed 2026-07-05; mechanism promoted → CLAUDE.md (wrong-qualifier/load-path) +
`swipl_load_path_and_probe_gotchas.md`; full story in ISSUES.md OQ-57.] Wrong-qualifier bugs
can throw in the REPL and silently work in the pipeline (json_report.pl is a non-module file;
imports land in `user`) — diagnose on the consumer's exact load path.

## 2026-06-04 — OQ-63 diagnostic run: directionality's beneficiary read measured (read-only; no engine change)
**Files:** prolog/constraint_indexing.pl, ISSUES.md
**Tier:** correction-key

[Compressed 2026-07-05; full evidence in ISSUES.md OQ-63; morphology rule promoted → CLAUDE.md
Cross-Sibling Disambiguation + OQ-64.] The d→χ mis-derivation on proposition-kind beneficiary
values is REAL but χ-immaterial today (all |Δχ| ≤ 0.022, zero band crossings; suffix-probe
population known to undercount). Escalation ruled AGENT on in-file witness; OQ-63 →
"consumer working correctly."

## 2026-06-03 — FSM agency gate: agent_beneficiary/2 two-site narrowing (maxwell un-stripped; one-row manifest diff, derived then confirmed)
**Files:** prolog/narrative_ontology.pl, prolog/signature_detection.pl, prolog/tests/test_agent_beneficiary.pl
**Tier:** tripwire

[Compressed 2026-07-05; the TWO-GATE non-agent-registry principle is documented at the registry
itself (narrative_ontology.pl) and in memory `project_fsm_agency_gate`; full text in git
history.] `non_agent_beneficiary/1` registry (2 ruled entries; unlisted = agent, fail-open) +
`agent_beneficiary/2`; FSM gate + `count_power_beneficiaries/2` narrowed. Manifest diff exactly
1 row (maxwell → mountain×4/natural_law), derived pre-write then confirmed. Guard tests in
`prolog/tests/test_agent_beneficiary.pl` incl. the :287 inertness tripwire (fails loudly when
the OQ-66 deferral goes stale). Gotcha (kept): `setup_call_cleanup/3` defers cleanup while the
goal holds choicepoints — wrap the goal in `once/1`.

## 2026-06-03 — Purity audit: structural_purity/2 was dead (bound-probe bug, now fixed); correction key for purity readings
**Files:** prolog/signature_detection.pl, prolog/boltzmann_compliance.pl, prolog/purity_scoring.pl, docs/logic_extensions.md
**Tier:** tripwire

[Compressed 2026-07-05; the bound-arg warning is a comment at the `epistemic_access_check/2`
definition (boltzmann_compliance.pl) — promoted to substrate; full audit
`audits/2026-06-03_purity/`.] `structural_purity/2` returned `inconclusive` unconditionally for
its whole life (bound-probe `epistemic_access_check(C, false)` satisfied by the catch-all);
fixed to unbound + `Access == false`; post-fix 96.6% contaminated, 0 scalar scores moved.
Open findings: OQ-60 (absence-reward), OQ-61 (purity restates type composition), OQ-62 (band
vocabulary fork — do not auto-unify).

## 2026-06-03 — never-generated kernels generated (300/304); corpus 803→1103
**Files:** agent/generate_kernel_corpus.py, agent/build_never_generated_seeds.py, prolog/validation_suite.pl
**Tier:** tripwire

[Compressed 2026-07-05; both warnings live in the memory index
(`reference_no_scope_skips_integrity_sweep`, `reference_validation_suite_autogenerated`);
corpus superseded by the 2026-06-05 reset; full text in git history.] The ~102 never-generated
kernels were naming drift, not missing content; generated per the sibling-kernels-are-distinct
ruling (commit `64cc249a`). Warnings (kept): **no-scope mode does NOT run the OQ-58
reading-relation integrity sweep** — run it manually after any no-scope batch; **a modified
`validation_suite.pl` after a pipeline run is expected regeneration, not a hand edit.**

## 2026-06-03 — `reading_diff.pl`: the cyclopean disparity operator (OQ-59 disposition)
**Files:** prolog/reading_diff.pl, prolog/axiom_diff.pl, prolog/stack.pl, prolog/reading_diff_census.pl
**Tier:** tripwire

[Compressed 2026-07-05; invariants carried in memory `project_reading_diff_operator` +
`feedback_verdict_omits_seat` and in-module docs; census pre-reset; full text in git history.]
OQ-59 ruled preserve-and-diff, not merge. Invariants (kept): authored-cells-only (never the
computed export); regime is pair × key with an order-independent stability verdict; counts over
vantage-groups, not pairs; `weighted` keys throw on `reading_diff/6`. Census (pre-reset corpus):
53.7% key_fragile. Axiom layer: 0/935 reading-pairs share an axiom NAME — `exact_name`
structurally all-blind; don't compare `cs_axiom_status` across readings; don't bake
`axiom_concept` (superseded 2026-07-02: OQ-72 baked a RATIFIED registry —
`axiom_concept_registry.pl`, see AGENTS.md Rule 3c). westphalia/westphalian are distinct
sibling kernels, not a spelling dup.

## 2026-06-02 — Reading-reference linter + the "complete kernels, not patch edges" finding
**Files:** python/audits/reading_reference_linter.py
**Tier:** landed

[Compressed 2026-07-05; full text in git history.] Linter (reporter, 3 rules, each with a
synthetic positive control): the dangling-edge problem was kernel-COMPLETENESS (119 missing
readings across 69 kernels), not edge-patching → OQ-58. `affects_constraint` targets may be
abstract nodes — its "dangling" refs are NOT an integrity signal. R3 over-flags by design.

## 2026-06-02 — Reading-axis structural obstruction built + cs_reading_relation name-form repair
**Files:** prolog/cs_kernel_registry.pl, agent/generate_constraint_pl.py, agent/generate_kernel_corpus.py, prolog/cs_corpus_analysis.pl, prolog/json_report.pl
**Tier:** tripwire

[Compressed 2026-07-05; full text in git history.] Built `cs_kernel_obstruction/4` (committer
H¹ analog, observer-blind, fail-closed on untyped pairs). Repaired 86 short-form
`cs_reading_relation` targets → canonical `<kernel>__<short>` across 47 files (predicted-delta
control passed exactly); generator now canonicalizes at emit (`generate_constraint_pl.py:482`)
+ hard-fail integrity check with quarantine (OQ-58 policy: attach-or-quarantine, NO auto-rewrite
tier, NO plausible-form tier). **Tripwire (kept): consumers stay EXACT-MATCH; do NOT add a
read-time short→full resolver — it re-hides the defect.**

## 2026-06-02 — Coupling liveness profile wired into per-constraint JSON (seat structure, not just verdict)
**Files:** prolog/boltzmann_compliance.pl, prolog/json_report.pl, python/query.py, python/enhanced_report.py
**Tier:** landed

[Compressed 2026-07-05; full text in git history.] `coupling.scope_violations` /
`power_violations` / `live_index` now emitted per constraint (violation logic single-sourced in
`coupling_violation_components/5`); score path byte-identical pre/post (773/773). Framing:
`live_index=none` is Mountain-consistent (seat-free verdict), not a pathology flag.

## 2026-06-02 — Toy corpus finished 769/770; generator repair + 3 robustness fixes
**Files:** agent/generate_kernel_corpus.py, python/story_repair.py, prompts/constraint_story_generation_prompt_json.md
**Tier:** tripwire

[Compressed 2026-07-05; toy corpus superseded by the 2026-06-05 reset; generator fixes live on;
full text in git history.] Fixes: `overwrite=True` in the no-scope path (**tripwire, kept: do
not revert to skip-on-exists — the ladder `beta_processed.txt` is the idempotence source, and
`json/` held a stale pre-rebuild corpus**); `poll_batch` transient-error retry; plain-seed
summaries capped ≤500; `python/story_repair.py` canonical deterministic repair (never touches
conditional `allOf/then` bounds — clamping would fabricate).

## 2026-06-02 — `sheaf_status` now persisted (W1×sheaf join built); orbit provenance is a sidecar
**Files:** prolog/json_report.pl, python/run_pipeline.py, python/w1_sheaf_join.py, prolog/sheaf_analysis.pl
**Tier:** tripwire

[Compressed 2026-07-05; the sidecar rule is GAP-03 in `design_gaps.md` (promoted) and the
freshness rule is Build Discipline Pattern 1; full text in git history.] `sheaf_status` emitted
per constraint; `orbit_data.manifest.json` sidecar asserts same-run. Warnings (kept):
**`orbit_data.json` provenance lives in the SIDECAR — do not inject a `"manifest"` key in-file**
(7 consumers iterate it with bare `.items()`); **`sheaf_status` recomputed on a bare `[stack]`
(no maxent run) is VACUOUS** — heights degenerate, fragile count reads 0.

## 2026-06-02 — Dirac Axis-1 (`derived_from/3`) removed → design gap; `gauge_fixed/3` straggler fixed
**Files:** prolog/dirac_classification.pl, docs/design/design_gaps.md
**Tier:** tripwire

[Compressed 2026-07-05; warning promoted → GAP-01 in `design_gaps.md` (cited by CLAUDE.md Design
intent); full text in git history.] `derived_from/3` had zero producers corpus-wide (Pattern 5:
every constraint read `primary` via the `\+` cut) — removed; do NOT re-add unfed.
`gauge_fixed/3` straggler migrated off deleted `standard_context/1`. `full_dirac_report/3`
still has no consumers (candidate for the same treatment).

## 2026-06-02 — False-summit forensic detector repaired (was vacuous) + two report bugs + stale comment
**Files:** prolog/drl_core.pl, prolog/report_generator.pl, prolog/drl_composition.pl
**Tier:** tripwire

[Compressed 2026-07-05; the dr_claim_mismatch region now carries the OQ-128 severity split +
in-code rationale (drl_core.pl:625-644); follow-ups OQ-50; full text in git history.]
`dr_claim_mismatch(_,_,type_1_false_summit,_)` had NEVER functioned (`is_mountain(C,Ctx,fail)`
satisfied by the unconditional catch-all clause; the report queried a nonexistent atom on top —
doubly dormant, Pattern 5). Fixed: negate post-signature `dr_type/3`, enumerate contexts, no
cut. **Warnings (kept): do not "simplify" back to `is_mountain` (pre-signature — flags genuine
mountains at mid-power contexts) and do not re-add the cut.** Sibling type_3/type_5 clauses
silently no-op on unbound Context (OQ-50).

## 2026-06-02 — Removed superseded observer-axis husk (saturation_floor) — commit ef92a61d
**Files:** prolog/drl_composition.pl, python/enrich_pipeline_json.py, python/enhanced_report.py, python/run_pipeline.py, python/shared/schemas.py
**Tier:** tripwire

[Compressed 2026-07-05; full text in git history.] **If you are looking for `--- HUSK
SIGNATURE ---` / `saturation_floor` / `husk_metrics`: deleted deliberately (commit `ef92a61d`),
do not re-add.** Two husks existed; only the observer-axis draft (zero engine consumers,
superseded within 4h of landing) was removed — **the CS husk (`cs_terminal_attractor(...,
husk)` + 9 consumers) is live and design-endorsed; the §5.11 "husk 57" count is that one.**
The real underlying finding (static ε understates series peak, 70/499 one-sided) was never
opened as an OQ.

## 2026-06-01 — Corpus rebuild pipeline built + validated on N=1 (decompose → no-scope gen)
**Files:** agent/generate_kernel_corpus.py, python/merge_kernels.py, python/partition_probe.py
**Tier:** tripwire

[Compressed 2026-07-05; modes + recipes promoted → `docs/technical/bulk_corpus_generation.md`;
full text in git history.] Three CLI modes (no-scope default / --decompose / --scope);
collision-proof naming; 3× retry. **Warnings (kept): `story_uid` is ALWAYS overwritten with a
fresh uuid4 (`generate_kernel_corpus.py` — do NOT revert to `setdefault`; Haiku copies the
example's placeholder and duplicates halt the corpus);** reading ids >64 chars are skipped
fail-loud (batch custom_id limit). Probe: the v5 archive is observer-axis (0 committer kernels
/ 99, 74% positive control) — kernels come from authored kernel files, archive supplies plain
seeds.

## 2026-06-01 — Corpus rebuild Phase 0: old corpora archived, `testsets/` emptied
**Files:** prolog/testsets/, prolog/archives/, python/sweeps/range_sweep.py
**Tier:** tripwire

[Compressed 2026-07-05; superseded by the archive map in CLAUDE.md Critical Distinctions (the
archives have since moved to `prolog/archives/datasets/<name>/`); full text in git history.]
Start of the rebuild: old corpora archived (v5 = 3,380, v6 = 229), fresh empty `testsets/`;
4 hardcoded `testsets_3000` overlays retargeted with a positive control.

## 2026-06-01 — `signature_detection.pl`: honest `unknown` now SURFACES (override removed, OQ-37)
**Files:** prolog/signature_detection.pl, python/sweeps/regenerate_orbits.py, python/enhanced_report.py
**Tier:** tripwire

[Compressed 2026-07-05; warning carried in memory index (`project_oq37_unknown_surfaces`); full
text in git history.] Commit `c90c5482`: FNL/FCR overrides no longer launder honest `unknown`
into tangled_rope (guards at :738 and :685). **Warnings (kept): do NOT reinstate "never
preserve unknown" — removed by ruling; `unknown` surfacing is load-bearing for OQ-37.** Also:
perturb.py's staleness guard checks only the testsets hash, NOT engine state — after an engine
edit that changes classifications, regenerate `product_site_orbits.json` manually or
stability-band comparisons silently read a stale baseline. `coordination_type_offset` is
per-constraint, not perturb-sweepable — keep it out of `_WITNESSED_PARAMS`.

## 2026-05-31 — Surface-2 primitive built; lock hypothesis witnessed (lever was misnamed)
**Files:** python/sweeps/surface2_lock_sweep.py, prolog/boltzmann_compliance.pl, prolog/signature_detection.pl
**Tier:** correction-key

[Compressed 2026-07-05; carried in memory `project_surface2_lock_primitive`; full text in git
history.] The handoff/OQ-30 lock lever was wrong: `boltzmann_floor_*` moves excess but not the
lock; the gate is `boltzmann_compliant` via `boltzmann_coupling_threshold` (+
`coordination_type_offset`), which flips 48/56 load-bearing locked readings (floor flips 5/96).
Floor hypothesis FALSIFIED, coupling-threshold WITNESSED. Row-level witness tier:
structure-closed + regenerable from `outputs/surface2_lock_sweep_results.json` (`db66cc53`),
not pasted.

## 2026-05-31 — Commit A: row-23 fail-close in `drl_composition.pl` `classify_at_time` (OQ-41)
**Files:** prolog/drl_composition.pl
**Tier:** tripwire

[Compressed 2026-07-05; full text in git history.] `classify_at_time/4` no longer fabricates
`Supp=0.5`: temporal series → authored scalar → `unknown` (scalar fallback, not literal
`unknown` — 650/650 no-series rows carried an authored scalar; returning `unknown` would
discard authored data). 268 rows corrected. **Warning (kept): the scalar clause is a labeled
STOPGAP — retired by OQ-46 (temporal series authoring), gated behind OQ-47; do not harden or
build an equivalence check on it.** Downstream audit: one live consumer
(`cs_kernel_divergence`), persisted counts invariant; the per-context divergence set grew +642
(real divergence the fabricated 0.5 homogenized).

## 2026-05-31 — Commit B LANDED (behavior-preserving batch behind Commit A)
**Files:** prolog/signature_detection.pl, prolog/constraint_bridge.pl, python/constraint_story_schema.json, prompts/constraint_story_generation_prompt_json.md
**Tier:** tripwire

[Compressed 2026-07-05; full text in git history.] B1 NL-gate fail-close (reads authored
`constraint_beneficiary`, not the empty `intent_power_change` join; live NL certs 5→2 — a
correct decline, not a regression); B2–B4 dead-clause/schema strips. Deferred with reasons
(kept): **`psych_bridge` is a dead UNLOADED module — remove/revive deliberately, don't strip
its reads (OQ-38 family); `resistance_to_change` is NOT a free strip (live report paths);
`python/constraint_story_schema.json` (canonical) vs `agent/data/constraint_story_schema.json`
is an unreconciled Pattern-2 fork** — B4 edited only the canonical one.

## 2026-05-31 — Legacy bullets imported from CLAUDE.md (2026-05-28 → 2026-05-31 items)
**Files:** prolog/product_site_export.pl, prolog/config_validation.pl, python/sweeps/perturb.py, python/sweeps/demotion_pass.py, python/enhanced_report.py, agent/generate_kernel_corpus.py, prolog/signature_detection.pl, prolog/drl_composition.pl
**Tier:** history

[Compressed 2026-07-05; full text in git history.] Verbatim import of the CLAUDE.md Known State
section at the 2026-05-31 split. Everything here has a later home: the LCO-critical cut →
CLAUDE.md Architecture Invariants + OQ-02; the OQ-25 ε-coherence load guard →
`config_validation_wiring.md`; kernel-linkage join → memory `project_kernel_linkage_join` +
bulk runbook; perturb()/stability-band/191-param sweep → memory (`project_sweep_primitive`,
`project_stability_band`, `project_perturbable_parameter_surface`) + OQ-29/OQ-30; bound-probe
Pattern 3 [now Pattern 7 — OQ-278, 2026-08-17] → `build_discipline.md` + `signature_detection_wiring.md`; OQ-43 satisfy-on-absence →
CLAUDE.md Build Discipline #5; the NL-gate "diagnostic-decline ≠ classification-changing"
correction and the "demotion_pass.py is engine-blind — route verification through perturb.py,
not its static buckets" caveat live in git history and OQ-30/OQ-33.
