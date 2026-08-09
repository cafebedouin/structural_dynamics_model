# A5 blinding-leak check — performed BEFORE subagent launch, 2026-08-09

Per PREREGISTRATION §E: read the grammar text exactly as the subagent receives it;
confirm no CP pair name, kernel name, or expected direction survives in it.
Operator performed their own pass at R2 (§J passes; noted §J omits
`miscoded_asymmetry` — blind judges run a slightly narrower grammar than the main
instance; recorded as a writeup obligation). This is the executing instance's own
paste, covering the FULL prompt (wrapper + instrument), not §J alone.

## Exact prompt — judge 1 (triplet substrate)

> You are a blind judge in a pre-registered audit. Your entire task is defined by
> the instrument below, applied to the listed files. You have no other context and
> you should not seek any: read ONLY the listed files (with the Read tool), use no
> other tools, write no files, and do not search the repository or read any other
> file.
>
> Files to read, each in full:
> 1. /home/scott/bin/structural_dynamics_model/prolog/archives/datasets/kernel_test/abolition_reading.pl
> 2. /home/scott/bin/structural_dynamics_model/prolog/archives/datasets/kernel_test/retributive_reading.pl
> 3. /home/scott/bin/structural_dynamics_model/prolog/archives/datasets/kernel_test/deterrence_reading.pl
>
> INSTRUMENT (apply exactly as written):
>
> [PREREGISTRATION §J, verbatim — the block quoted in that section]
>
> Return your final answer as one markdown section per qualifying pair, each
> containing exactly: pair, verdict class, mutation text, witness quotes, footing.

## Exact prompt — judge 2 (axiom-bearing family substrate)

Identical wrapper and instrument; file list:

> 1. /home/scott/bin/structural_dynamics_model/prolog/archives/datasets/kernel_test/retributive_desert.pl
> 2. /home/scott/bin/structural_dynamics_model/prolog/archives/datasets/kernel_test/categorical_abolition.pl
> 3. /home/scott/bin/structural_dynamics_model/prolog/archives/datasets/kernel_test/deterrence_instrument.pl
> 4. /home/scott/bin/structural_dynamics_model/prolog/archives/datasets/kernel_test/state_killing_authority_contradictions.pl
>    (operator edit 1: the family's contradictions file is part of this judge's
>    evidence base)

## Check result

- Instrument text (§J): names NO reading, NO kernel, NO pair, NO expected
  direction, NO expected verdict-class distribution. The only substrate-shaped
  clause is "where a file authors no axioms, a commitment carried in the file's
  own prose" — a grammar rule, not a direction (it does not say which files, or
  that any listed file is such a file). PASS.
- Wrapper: task framing ("blind judge in a pre-registered audit") carries no
  expectation; the read-only constraint carries none. PASS.
- File paths: necessarily name the files (and therefore the readings/kernel whose
  facts they carry) — equivalent to file content the judge must read anyway; the
  A5 target is the grammar text, which is clean. No expected direction is
  derivable from a path. PASS.

**A5: PASS — launched only after this paste.**
