# audit_log.md — OQ-301 round 2, execution record

**Executed:** 2026-08-23 (dedicated session, THIS box `SOMA-PC`, WSL2 kernel
6.18.33.2-microsoft-standard-WSL2, `/dev/sdd` 251G).
**Plan:** `~/.claude/plans/can-you-review-oq-301-distributed-toucan.md` (RULED 2026-08-23).
**Run row:** `.claude/skills/plan-review/RUNS.md` id `2026-08-23-1` (allocated at append; both
pinned instruments agreed at 5 pre-existing rows; `ledger grammar` gate row green after append).

Nothing below Step 0 is counted evidence until its own section says so.

---

## Step 0.0 — Assumed substrate, verified line by line (2026-08-23 15:54 −05:00)

Command output is pasted; a line that did not match the plan is marked **MISMATCH** and reported,
not adapted.

### PRE-AMENDMENT lines (expected to change at Step 1)

| # | check | plan pin | observed | verdict |
|---|---|---|---|---|
| S1 | `md5sum PREREGISTRATION.md` | `a9aa0705d29d3506c6505c1eada5e340` | `a9aa0705d29d3506c6505c1eada5e340` | ✓ |
| S2 | `grep -c '2>/dev/null' round2_arms.sh` | 1 | **3** | **MISMATCH — F1** |
| S3 | `grep -cE '^\s*(GOAL\|WRAP\|TAG)=' round2_arms.sh` | 0 | 0 | ✓ |
| S4 | `grep -n 'N=${N:-150}\|TMO=${TMO:-25}'` | 2 | 2 (lines 12, 13) | ✓ |

**F1 (finding about the plan, not the substrate).** `2>/dev/null` occurs at **three** sites, not
one:

```
15:corpus_fp () { ls testsets/*.pl | sort | xargs md5sum 2>/dev/null | md5sum | cut -d' ' -f1; }
25:          -g "${pre}run_giant_component_analysis, halt." 2>/dev/null)
33:  echo "    swipl:  $($SWIPL --version 2>/dev/null | head -1)"
```

Only **line 25** is the counted invocation's stderr suppression that Step 1 removes (removing it
is what makes `raw/<TAG>/<i>.stderr` possible). Lines 15 and 33 are unrelated (an `xargs md5sum`
suppression and a `--version` suppression) and are **left alone**. The Step-1 instruction
"`2>/dev/null` is removed" is executed **scoped to line 25**, and the post-amendment count is
therefore expected to be 2, not 0.

### Stable lines

| # | check | plan pin | observed | verdict |
|---|---|---|---|---|
| S5 | `ls $AUDIT` | `PREREGISTRATION.md WRITEUP.md round2_arms.sh` | exactly those three | ✓ |
| S6 | `grep -n 'cd "$(dirname'` | line 10 → `/prolog` | `10:cd "$(dirname "$0")/../../prolog" \|\| exit 1` | ✓ |
| S7 | `swipl --version` | 10.0.2 | `SWI-Prolog version 10.0.2 for x86_64-linux` | ✓ |
| S8 | `timeout --version` | coreutils 9.x | `timeout (GNU coreutils) 9.4` | ✓ |
| S9 | swi-prolog-nox ` upgrade ` in dpkg.log | one line, 2026-08-18 00:13:15 | `2026-08-18 00:13:15 upgrade swi-prolog-nox:amd64 9.2.9-2-g2a3e80b8e-jammyppa2 10.0.2-0-jammyppa2` | ✓ |
| S9b | `head -1 /var/log/dpkg.log` | 2022 | `2022-02-16 00:46:31 startup packages remove` | ✓ |
| S10 | gdb / elfutils / valgrind installs | three lines 2026-08-18 | gdb 11:29:33, elfutils 11:52:09, valgrind 11:52:10 | ✓ |
| S11 | `ptrace_scope` / `core_pattern` | `0`, `/tmp/core.%e.%p` | `0`, `/tmp/core.%e.%p` | ✓ |
| S11b | `50-debug.conf` | those two | `kernel.yama.ptrace_scope=0`, `kernel.core_pattern=/tmp/core.%e.%p` | ✓ |
| S11c | `ulimit -c` | 0 | 0 | ✓ |
| S12 | `command -v gdb eu-stack valgrind` | three `/usr/bin/…` | `/usr/bin/gdb`, `/usr/bin/eu-stack`, `/usr/bin/valgrind` | ✓ |
| S13 | corpus, three dated observations | 285 / 292 / 0 | **285 / 292 / 0** (2026-08-23) | ✓ |
| S14 | `git status --short run_pipeline.py giant_component_analysis.pl` | empty | empty | ✓ |
| S14b | `git log -1 dcde95919` | crash-trace fix | `dcde95919 fix(run_pipeline): keep the crash trace when a Prolog step dies on a signal` | ✓ |
| S15 | run_pipeline names the module + goal | both present | `959:["stack.pl", "giant_component_analysis.pl"]`, `960:"run_giant_component_analysis"` | ✓ |
| S16 | `run_prolog` / `_log_prolog_child` / `prolog_children.log` | 3 | `554`, `569`, `575` | ✓ |
| S17 | children-log giant_comp rows | ≥15, all rc=0 | `15 rc=0` | ✓ |
| S18 | gate rows | present | `43:issues_status`, `46:omega index`, `140:audit writeup` | ✓ |
| S19 | OQ-77 / OQ-182 / OQ-301 statuses | resolved / mitigated / partial | resolved / mitigated / partial; OQ-301 `Priority: 2`, **no `Deps:` line** | ✓ |
| S20 | driver arms == prereg arms by mapping | `B)`/`C)`/`D)` labels | see below | ✓ |

**S20, the three `case` labels pasted with their lines** (arm A's prefix is empty, line 45–46):

```
45	    A) run_arm "ARM A  baseline (default flags)" \
46	               "" "-l giant_component_analysis.pl" "$N" ;;
47	    B) run_arm "ARM B  gc_thread=false (AGC in the calling thread)" \
48	               "set_prolog_flag(gc_thread,false), " "-l giant_component_analysis.pl" "$N" ;;
49	    C) run_arm "ARM C  agc_margin enormous (AGC effectively never fires)" \
50	               "set_prolog_flag(agc_margin,1000000000), " "-l giant_component_analysis.pl" "$N" ;;
51	    D) run_arm "ARM D  line-buffered stdout (localize the true death point)" \
52	               "set_stream(user_output,buffer(line)), " "-l giant_component_analysis.pl" "$N" ;;
```

These are the prereg's rows B / C / D verbatim (`PREREGISTRATION.md:40–42`). Mapping holds.

### Idle check (plan step 2 of the executor prompt)

**F2 (finding about the plan — instrument self-match).** The pinned idle check
`pgrep -af 'swipl|c-orchestrator|run_pipeline'` **cannot come out empty when run from a shell whose
own command line contains the pattern** — the tool-invoked `bash -c …` string embeds the literal
words, so `pgrep -f` matches the checking process itself:

```
$ pgrep -af 'swipl|c-orchestrator|run_pipeline'
643580 /bin/bash -c source /home/scott/.claude/shell-snapshots/… pgrep -af 'swipl|c-orchestrator|run_pipeline' …
```

That is a false positive of the checker, not a co-resident process. Re-run in the standard
self-match-proof form:

```
$ pgrep -af '[s]wipl|[c]-orchestrator|[r]un_pipeline'
(no output)   rc=1
```

**Box is idle.** The same substitution is carried into the driver's per-row stray check and its
arm-start/arm-end census, where the identical self-match would otherwise VOID every arm at row 1
under the plan's own "non-zero pre-launch stray VOIDs the arm" rule (F19). Recorded as an
amendment, not an adaptation of the rule: the rule stands, the instrument is fixed.

### Additional observations (not plan pins; feed the Step-4 record corrections)

- **Two** swipl events on 2026-08-18, not one: `00:13:15 upgrade … 10.0.2-0-jammyppa2` (the S9 pin)
  and `02:50:35 install swi-prolog-nox:amd64 <none> 10.0.2-1-gb8d8f931a-nobleppa2`. The **installed**
  package is `10.0.2-1-gb8d8f931a-nobleppa2` (`dpkg -l`), reporting `SWI-Prolog version 10.0.2`.
  OQ-301's in-entry claim of a `02:50:43` install is off by 8 s and names only the second event.
- OQ-301's claim "`/var/log/dpkg.log` is unrotated, beginning 2026-08-18" is **false**: the log
  begins `2022-02-16` and is a single unrotated 5.0 MB file, so the 9.2.9 → 10.0.2 transition IS
  recoverable — and is recovered above. (This is record-correction #2 of the three the plan
  carries; written at Step 4 from these witnesses, not from the plan's prose.)
- `/tmp` and `$AUDIT` are the **same filesystem** (`stat -c %d` → `2096` for both; `/dev/sdd`
  mounted `/`, 219G available, 9% used). One budget for cores and the 150 stderr files, as the
  plan states.
- `/usr/bin/time` present (GNU time, for the out-of-band peak-RSS measurement at Step 0.3).

---

## Step 0.3 — frozen failure detector, core bound, budget

**Session start (Step 0.0 first command):** 2026-08-23 15:54 −05:00.
**Step-1b apparatus budget:** 90 min from session start → **17:24 −05:00**. If the Step-1b controls
are not passing by then, the apparatus is landed as-built and arm A runs under Set R as it stands.

### Corpus, three dated observations (2026-08-23)

| observation | command | count |
|---|---|---|
| live-loadable (what the non-recursive loader glob sees) | `ls prolog/testsets/*.pl \| wc -l` | **285** |
| git-tracked (includes run-tagged subdirs the glob excludes) | `git ls-files prolog/testsets/ \| grep -c '\.pl$'` | **292** |
| untracked | `git ls-files --others --exclude-standard prolog/testsets/ \| grep -c '\.pl$'` | **0** |

The 285↔292 gap is run-tagged subdirs, not deletions; 0 untracked ⇒ the snapshot's contents are all
tracked. **The snapshot copies the 285-file live-loadable set** (the loader's own glob), never 292.

`df -h /tmp` → `/dev/sdd 251G total, 20G used, 219G avail, 9%`, mounted `/`. `$AUDIT` and `/tmp`
share device `2096` — **one budget for cores and the 150 `.stderr` files**.

`pgrep -af '[s]wipl|[c]-orchestrator|[r]un_pipeline'` → empty (rc=1). Box idle.

### Core bound, by the RULING-block formula

Out-of-band peak-RSS measurement, no exit-status constraint (cwd `prolog/`):

```
$ /usr/bin/time -v swipl -l stack.pl -l giant_component_analysis.pl \
      -g "run_giant_component_analysis, halt."
	Elapsed (wall clock) time (h:mm:ss or m:ss): 0:03.46
	Maximum resident set size (kbytes): 25136
	Exit status: 0
   stdout: 7747 bytes
```

- `peak_rss_kb` = **25136**
- `BOUND_blocks = 4 × peak_rss_kb` = **100544** (512-byte blocks) = **51 478 528 bytes** (= 2 × peak RSS)
- `/` free = 458 977 088 blocks; 25 % cap = 114 744 272 blocks (58 749 067 264 B) → **BOUND is UNCAPPED**
  (100 544 ≪ 114 744 272). `ulimit -c 100544`.
- Incidental, recorded because it bears on the clean reference: a complete run emits **7 747 bytes**
  of stdout at n=285 (round 1 reported 7 745 at n=279 — descriptive only, different corpus AND
  different interpreter).

### The frozen failure detector (frozen AFTER the Step-0.4 KILL-cell control, per F6)

`TMO=25`, `timeout -k 5`, so **`KTHRESH = (TMO + kill_after)·1000 − 1000 = (25+5)·1000 − 1000 = 29000`**.

Row format, tab-separated `key=value`, no header, no bare numbers:

```
i=<n>	rc=<n>	bytes=<n>	wall_ms=<n>	rss_kb=<n|na>	stray=<n>	pid=<n|na>
```

Counting expressions, pinned (`awk -v KTHRESH=29000 -F'\t'`, exact-token or numeric on the parsed
value, never a regex on the prefix):

| bucket | expression |
|---|---|
| rows | `wc -l` |
| clean | `$2=="rc=0"` |
| failures | `$2!="rc=0"` |
| did-not-complete | `$2=="rc=124" \|\| ($2=="rc=137" && substr($4,9)+0>=KTHRESH)` |
| segv | `$2=="rc=139"` |
| external-kill/OOM | `$2=="rc=137" && substr($4,9)+0<KTHRESH` |
| faults (driver/env) | `{v=substr($2,4)+0} v>=1 && v<=127` |

**Reconciliation** (the tautology `clean+failures==rows` catches nothing):
assert `did-not-complete + segv + external-kill + faults + other_ge128 == failures`, **plus** a shape
check that every row has `NF==7` and fields 1–7 begin `i= rc= bytes= wall_ms= rss_kb= stray= pid=`.

`rc` in 1..127 on a counted row = driver/env fault: STOP, fix, re-run under a fresh TAG.

---

## Step 0.4 — positive controls, pasted

### Control 1 — the KILL-path cell, bare shell, BEFORE the detector was frozen (F6). **PASS, two-sided.**

```
$ timeout -k 5 25 bash -c 'trap "" TERM; sleep 60'        # TERM-ignoring
  644888 Killed
  rc=137  wall_ms=30019          <- 30019 >= KTHRESH 29000 ✓ (KILL-required)

$ timeout -k 5 25 bash -c 'sleep 60'                       # TERM-responsive
  rc=124  wall_ms=25003          <- 25003 <  KTHRESH        ✓ (TERM-responsive)
```

Both cells of the R4 split are witnessed on this box with this `timeout` (coreutils 9.4). The
detector above is frozen on this observation. The swipl plants at Step 1b are confirmation, not the
pin. **No amendment needed — the cells agree with the plan's witnessed shape.**

### Control 2 — core written, AT THE BOUND. **PASS, with a naming finding (F3).**

First attempt (the plan's literal recipe) reported `(core dumped)` and left **no `core.sleep.*`**:

```
$ bash -c "ulimit -c 100544; sleep 30 & kill -SEGV \$!; wait"
  645055 Segmentation fault      (core dumped) sleep 30
$ ls /tmp/core.sleep.*
  ls: cannot access '/tmp/core.sleep.*': No such file or directory
$ ls /tmp | grep -i core
  core.bash.645055   (565248 bytes)
```

**F3 — the core exists but `%e` is not the name you expect.** `kill -SEGV $!` fires before bash's
child has finished `exec`ing `sleep`, so the crashing process's `comm` is still `bash` and
`core_pattern`'s `%e` writes `core.bash.<pid>`. Inserting a 0.5 s settle reproduces the intended
name — and confirms the channel end to end:

```
$ bash -c 'ulimit -c 100544; sleep 8 & P=$!; sleep 0.5;
           echo "comm=$(cat /proc/$P/comm) exe=$(readlink /proc/$P/exe)"; kill -SEGV $P; wait'
  comm=sleep exe=/usr/bin/sleep
  645434 Segmentation fault      (core dumped) sleep 8
$ ls -l /tmp/core.sleep.645434
  -rw------- 1 scott scott 450560 Aug 23 16:01 /tmp/core.sleep.645434
$ file /tmp/core.sleep.645434
  ELF 64-bit LSB core file, x86-64, SVR4-style, from 'sleep 8', execfn: '/usr/bin/sleep'
```

**Core channel PASSES: a core is written, under `ulimit -c 100544` (the bound the driver uses, not
`unlimited`), at 450 560 B ≪ BOUND 51 478 528 B — not truncated.** Deleted, and the deletion is
recorded here: `rm -f /tmp/core.sleep.645434` (and the two earlier `core.bash.*` probes).

**Consequence carried into the driver.** `%e` is the crashing process's `comm` at crash time, so
`/tmp/core.swipl.<pid>` is the right key only for a process that has already exec'd swipl (which a
counted row always has). The driver therefore uses the **mtime-window sweep over `/tmp/core.*` as
the PRIMARY** collector and the `core.swipl.<pid>` name as confirmation — the plan's own fallback,
promoted to primary on this witness.

**A-priori rule, restated (NOT control-confirmed — the `sleep` core is sub-MB and never exercises
the bound; see Declared limits): a core whose size == BOUND is presumed TRUNCATED and is recorded
as such.**

### Control 3 — attach. **`eu-stack` FAILS on this box; `gdb` PASSES. (F4)**

```
$ sleep 40 & SP=$!; timeout 20 eu-stack -p $SP
  (no output)
  eu-stack rc=124            <- hung until the 20 s bound
```

Observed **twice**: the first attempt (unbounded) hung the enclosing shell to its 2-minute limit;
the second, bounded at 20 s, returned rc=124 with no frames. The target was a plain
`sleep` with `comm=sleep`, `ptrace_scope=0`, same uid — i.e. the easiest possible attach.

The same target, same moment, under gdb:

```
$ timeout 30 gdb -p $SP -batch -ex 'thread apply all bt'
  Thread 1 (Thread 0x7b5bc437c740 (LWP 645516) "sleep"):
  #0  0x00007b5bc40ecb7a in __GI___clock_nanosleep (...) at ../sysdeps/unix/sysv/linux/clock_nanosleep.c:78
  #1  0x00007b5bc40f9b27 in __GI___nanosleep (...) at ../sysdeps/unix/sysv/linux/nanosleep.c:25
  #2  0x0000639ec150da7f in ?? ()
  #3  0x00007b5bc402a1ca in __libc_start_call_main (...)
  ...
  [Inferior 1 (process 645516) detached]
  gdb rc=0
```

**F4 — the plan names the wrong attach instrument for this box.** `eu-stack -p` (elfutils 0.190) does
not return on this WSL2 kernel; `gdb -p <pid> -batch -ex 'thread apply all bt'` returns symbolized
frames in seconds, including the libc frames that carry the futex/parked shape the hang question
needs. **The attach channel is therefore AVAILABLE, via gdb** — this is an instrument substitution
recorded as an amendment, not a degraded control.

**Consequence:** arm A (Set R only) does not attach at all, so this does not touch Step 2. If Step 3
is reached, **Set W's sampler uses `gdb -p … -batch -ex 'thread apply all bt'`, not `eu-stack -p`**,
and the plan's F6 attach-vs-kill race check travels with it unchanged.

**Go/no-go:** all three controls are recorded. Controls 1 and 2 PASS; control 3 passes under a
substituted instrument. Per the plan these gate Step 3, never Step 2 — arm A proceeds regardless.

---

## Step 0.5 — the `prolog_children.log` census (naturally arising, underpowered, IN-PIPELINE)

Not round-1-equivalent (in-pipeline, not standalone-serial) and it moves nothing; recorded so the
zero it contains is not later mistaken for evidence.

```
$ awk -F'\t' '/run_giant_component_analysis/{print $5}' outputs/prolog_children.log | sort | uniq -c
     15 rc=0
```

15 rows, **all rc=0**; P(15 clean | p₀=.07) = 0.34 — uninformative. Split at the interpreter upgrade
(2026-08-18 00:13:15): **1 pre-upgrade, 14 post-upgrade.**

Incidental, and worth the line because it bears on the detector-headroom guard: the single
pre-upgrade row (`2026-08-17T21:58:01`, inside round 1's own ~21:58 window) took **5.27 s**; all 14
post-upgrade rows sit at **1.24–1.66 s**. The in-pipeline wall time fell ~3.5× across the upgrade.
Whatever the arm-A warm-up measures, TMO=25 has more headroom now than it did when it was chosen.

---

## Step 1 — driver amended for arm A = Set R only

| artifact | pre-amendment md5 | post-amendment md5 |
|---|---|---|
| `round2_arms.sh` | `9b3afcf7487655a38d779cb85d798562` | **`a79f74f3465198ed470421cc6496ac3c`** |
| `PREREGISTRATION.md` | `a9aa0705d29d3506c6505c1eada5e340` (S1 pin) | **`775b7064290f38dc999d9026cff02734`** |

`bash -n round2_arms.sh` → syntax OK.

### The counted invocation, before and after

```
PRE  (round 1):
    o=$(timeout "$TMO" $SWIPL -l stack.pl $mods \
          -g "${pre}run_giant_component_analysis, halt." 2>/dev/null)

POST (amended):
    o=$(timeout -k "$KILL_AFTER" "$TMO" $WRAP $SWIPL -l stack.pl $mods \
          -g "$goalstr" 2> "$RAW/$tag/$i.stderr")
```

Same `$(...)` capture, never backgrounded, never piped. `$WRAP` is empty for every counted row and
sits INSIDE `timeout`, so no wrapper is in the exit-status chain. **F1 consequence:** the removal
of `2>/dev/null` is scoped to this one site; the whole-file count is not the witness (it is 13 now,
because the matcher, the core sweep and the census each suppress their own `/proc` and `ls` noise)
— the witness is the diff above.

### Goal string (the frozen overlay)

```
asserta(config:param(corpus_path,'<AUDIT>/corpus_snapshot'), ) ${arm_prefix}${GOAL}, halt.
   -> asserta(...), run_giant_component_analysis, halt.        (counted row)
   -> asserta(...), sleep(60), halt.                            (GOAL=sleep(60) plant)
```

`asserta`, never `assertz` (config.pl:509 defines `param(corpus_path,'testsets')` first and the
loader takes the first solution). Verified that this lands in time: `giant_component_analysis.pl`
has **no load-time corpus directive** — `load_all_testsets` is called from inside the phases
(`:383`, `:664`, `:835`, `:1292`), i.e. at goal-run time, strictly after the `-g` overlay.
`config:param/2` is `:- dynamic` (config.pl:9).

`GOAL` replaces **only** the analysis call; the overlay prefix, the arm's flag prefix and `halt.`
are kept, so a plant runs the same load path as a counted row.

### Per-machine artifacts, git-excluded

`corpus_snapshot/` and the **per-row subdirectories** of `raw/` (the `.stderr` files, 56 MB for
arm A alone, and any cores) are per-machine and excluded from version control via the
repository's machine-local git exclude file — deliberately NOT cited as a path here: it is
untracked by construction, so citing it would be citing unversioned evidence, which is exactly
what the `audit cites` gate row flags (and did).

**F5 — RA over-excluded, corrected before commit 2.** The plan's RA ("committed digests,
artifacts per-machine") excluded `raw/` wholesale. The `audit cites` gate row disagreed, and it
is right: `raw/arm_A.tsv` **is** the frozen evidence for every count in this log, it is 12 KB,
and an audit that cites it while leaving it untracked is citing evidence a fresh clone cannot
read. The exclusion was narrowed to `raw/*/`, and the six row-level artifacts
(`arm_A.tsv`, `arm_A.console`, `warmup.tsv`, `warmup2.tsv`, `ctl_hang.tsv`,
`ctl_hang_kill.tsv` — 32 KB total) are COMMITTED. Verified staged-set contains no `core.*` and
no `.stderr`. Core binaries would still be recorded as `path + size + md5 + readelf -n` and
never committed; none exist, because nothing crashed.

---

## Step 1b — the driver witnessed through itself

All controls below ran BEFORE any counted row. Session start 15:54; the 90-minute apparatus budget
(→ 17:24) was not approached — Step 1b completed by 16:20.

### Warm-up ×2 — the clean reference

```
$ TAG=warmup  N=1 ./round2_arms.sh A
    snapshot: CREATED …/corpus_snapshot     285 files  fp=ea0c60a50e6d7b063b7a51d9154f5893
    goal: asserta(config:param(corpus_path,'…/corpus_snapshot')), run_giant_component_analysis, halt.
  raw/warmup.tsv:   i=1	rc=0	bytes=7740	wall_ms=1411	rss_kb=na	stray=0	pid=na
  raw/warmup/1.stderr:3051: [corpus] Loaded 285 testsets successfully.

$ TAG=warmup2 N=1 ./round2_arms.sh A
    snapshot: reusing frozen …/corpus_snapshot   285 files  fp=ea0c60a50e6d7b063b7a51d9154f5893
  raw/warmup2.tsv:  i=1	rc=0	bytes=7740	wall_ms=1385	rss_kb=na	stray=0	pid=na
  raw/warmup2/1.stderr: [corpus] Loaded 285 testsets successfully.   (1 match)
```

- **Loaded N == snapshot count (285) on both runs.** The overlay reaches the loader; the corpus did
  not move.
- **`corpus_fp` = `ea0c60a50e6d7b063b7a51d9154f5893`** over the 285-file snapshot — stable across
  both runs and used as the frozen fingerprint for arm A.
- **Clean reference: `bytes = 7740`.** (The out-of-band Step-0.3 run measured 7 747; the 7-byte
  difference is `$(...)` stripping trailing newlines. The reference is the driver's own number.)
- **F10 margin, WITNESSED not guessed: the two clean runs are byte-IDENTICAL, so the margin is
  pinned at EXACT EQUALITY.** Any `rc=0` arm-A row with `bytes != 7740` is REPORTED as a finding.
- **`warm_up_wall = 1411 ms`** (max of the two).

### F5 detector-headroom guard

`warm_up_wall / (TMO·1000) = 1411 / 25000 = 0.056`, far below the 0.5 stop-and-ask threshold. The
detector still separates *did-not-complete* from *slow*. **PASS** — `rc=124` may be read as
did-not-complete-in-TMO.

### F4 arm-A ceilings, computed from the warm-up (no fixed-minute figure)

- clean-branch expectation `150 × warm_up_wall` = **211.7 s ≈ 3.5 min** of swipl time (plus per-row
  matcher/core-sweep overhead);
- absolute ceiling `150 × (TMO + kill_after + slack)` = 150 × ~31 s ≈ **77.5 min**. **Exceeding the
  absolute ceiling is stop-and-ask.**

### Control — a second start under the same TAG is refused

```
$ TAG=warmup N=1 ./round2_arms.sh A
REFUSED: …/raw/warmup.tsv already exists — pick a fresh TAG (outputs are never shared)
driver rc=3
```

### Control — the matcher, two-sided

```
PRESENT half (one invocation at TMO−3 s, control TAGs only):
  raw/ctl_hang/1.matcher       -> 650242
  raw/ctl_hang_kill/1.matcher  -> 650734
ABSENT half (post-row survivor check on every row):
  no VOID(survivor) line on any control or warm-up row; stray=0 on every row.
```

Both halves fire. Until this pair existed, "stray=0 / no survivor" was uninformative; it is now a
read from an instrument witnessed able to see a live process. Resolution is by
`/proc/<pid>/comm == swipl` **with parent comm `timeout`** — the pid recorded is the row's own
swipl, not the `timeout` wrapper.

### Plant 1 — TERM-responsive hang

```
$ TAG=ctl_hang GOAL="sleep(60)" N=1 ./round2_arms.sh A
  goal: asserta(config:param(corpus_path,'…')), sleep(60), halt.
  i=1	rc=124	bytes=299	wall_ms=25029	rss_kb=na	stray=0	pid=650242
```

**rc=124 at 25 029 ms** — as pinned. `wall_ms < KTHRESH`, so this row is TERM-responsive
did-not-complete.

### Plant 2 — TERM-ignoring hang (confirms the Step-0.4 KILL cell through the driver)

```
$ TAG=ctl_hang_kill GOAL="on_signal(term,_,ignore), sleep(60)" N=1 ./round2_arms.sh A
      row 1: rc=137, NO new /tmp/core.* in [1787519856900,1787519886904]
  i=1	rc=137	bytes=299	wall_ms=30004	rss_kb=na	stray=0	pid=650734

$ awk -v K=29000 -F'\t' '…' raw/ctl_hang_kill.tsv
  rc=137 wall_ms=30004 -> KILL-required did-not-complete (>=KTHRESH)
```

**The KILL path is reachable through the driver**, and the classifier puts the row in the right
bucket. The split stays "a-priori threshold, confirmed reachable by control" — it is not UNPINNED.

Incidentally two-sided: the `rc≥128` core-collection branch **ran** on this row and correctly
reported *no new core* (SIGKILL produces none) rather than silently passing. The branch is
exercised; it is the swipl-segv case that remains untested (declared).

### Control — the `dmesg` channel is readable (so its empty result is a tested absence)

```
$ dmesg -T > /tmp/dmesg_probe.txt 2>/tmp/dmesg_probe.err ; echo rc=$?
  rc=0        stdout lines: 1195      stderr: (empty)
  last line: [Sun Aug 23 16:01:41 2026] FS:  0000785fbb962740 GS:  0000000000000000
  grep -ci 'killed process' -> 0
```

Without this the driver's silent `dmesg` line would have been indistinguishable between *no OOM
kill* and *cannot read the ring buffer*. It reads; there are no OOM kills.

### Partial-arm rule (restated before the arm runs)

An arm interrupted before row 150 is VOID (cause `interrupted`); its rows stay as evidence; it is
re-run from row 1 under a **fresh TAG**, and the authoritative TAG is named in this log.

---

## Step 2 — arm A, Set R, n=150. **0 failures.**

Pre-assert: `raw/arm_A.tsv` absent (`ls` → No such file). Idle re-check immediately before launch:
`pgrep -af '[s]wipl|[c]-orchestrator|[r]un_pipeline'` empty (rc=1).

Started **16:19:00 −05:00**, finished **16:23:13 −05:00** — **4 min 13 s**, against a clean-branch
expectation of ~3.5 min of swipl time and an absolute ceiling of ~77.5 min. Ceiling not approached.

### Driver console (whole)

```
### ARM A  baseline (default flags)   [TAG=arm_A]
    swipl:  SWI-Prolog version 10.0.2 for x86_64-linux
    detector: TMO=25  kill_after=5  KTHRESH=29000
    ulimit -c: 100544 blocks
    snapshot: reusing frozen …/corpus_snapshot
    snapshot: 285 files  fp=ea0c60a50e6d7b063b7a51d9154f5893
    census(start):                       <- empty
    goal:   asserta(config:param(corpus_path,'…/corpus_snapshot')), run_giant_component_analysis, halt.
    census(end):                         <- empty
    dmesg(killed process):               <- empty (channel proven readable at Step 1b)
    snapshot fp stable across the arm: ea0c60a50e6d7b063b7a51d9154f5893
    rows: 150
```

**No VOID of any cause.** Start and end census empty; snapshot fingerprint stable (a consistency
check under R5, not a guard); no OOM kill in `dmesg`.

### Counts, re-derived from `raw/arm_A.tsv` with the pinned expressions (never the driver summary)

| bucket | expression | count |
|---|---|---|
| rows | `wc -l` | **150** |
| clean | `$2=="rc=0"` | **150** |
| failures | `$2!="rc=0"` | **0** |
| did-not-complete-in-TMO | `$2=="rc=124" \|\| ($2=="rc=137" && substr($4,9)+0>=29000)` | 0 |
| segv | `$2=="rc=139"` | 0 |
| external kill / OOM | `$2=="rc=137" && substr($4,9)+0<29000` | 0 |
| driver/env fault | `1..127` | 0 |
| other `≥128` | — | 0 |

**Reconciliation** (the real check — `clean+failures==rows` is a tautology):
`dnc 0 + segv 0 + extkill 0 + faults 0 + other≥128 0 = 0 = failures` → **RECONCILES**.

**Shape check:** 150 rows, `NF==7` on every row, all seven prefixes (`i= rc= bytes= wall_ms=
rss_kb= stray= pid=`) present → **SHAPE OK, 0 malformed**. `i` sequence complete 1..150, no gaps.

**F10 silent-corruption flag:** clean rows with `bytes != 7740` (the exact-equality margin witnessed
at Step 1b) → **0**. All 150 clean runs emitted byte-identical output. The 0/150 is not "150 runs
that returned 0 while quietly producing nothing different" — the output is the same complete report
each time.

**Invariants:** `stray=0` on all 150 rows; `rss_kb=na` on all 150 (Set R, by construction);
`pid=na` on all 150 (no matcher invocation on a counted row, by construction).

**Wall time:** n=150, min 1369 ms, mean 1395.6 ms, max **1470 ms** — the slowest row used **5.9 %**
of `TMO`. Nothing came within 23 s of the timeout.

### Zero classification (what the 0 is and is not evidence about)

- **0 failures = a TESTED absence.** The detector's `rc=124` and `rc=137`/`≥KTHRESH` cells were
  each fired by a plant through this driver at Step 1b, and the `rc≥128` core branch ran. A failure
  of either did-not-complete kind would have been recorded.
- **No cores and no stacks = UNTESTED INSTRUMENTS.** The plants exercised the *channel* (a core is
  writable at the bound; gdb attaches) — not the failure. No swipl core was ever produced here, so
  the "core size == BOUND ⇒ truncated" rule remains a-priori, and RA's `path+size+md5+readelf -n`
  ledger is **empty because nothing crashed**, not because collection was skipped.
- **`rss_kb` is unmeasured** for the whole arm (`na`, Set R): swap/OOM behaviour is not addressed.
- **Round 1 is unrecheckable.** Its evidence never reached this directory and its binary is purged.

### Reading against the pre-committed table

`0/150` → the ruled branch: **CLOSE `resolved`** (RB). Exact one-sided 95 % upper bound on the
per-run failure rate given 0/150, under an independence assumption: **1.98 %**.
`P(0/150 | p₀ = .07) = 1.9 × 10⁻⁵`. Arms B–F, the valgrind pass and the symbol question are
**moot**; Step 3 is not entered.
