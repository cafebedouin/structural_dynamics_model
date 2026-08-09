# READOUT — blind CP judging (Phase C step 3), 2026-08-09

Two clean-context read-only subagents (general-purpose, Read-only instruction),
launched AFTER the A5 leak check paste (`A5_leak_check.md`). Prompts = wrapper +
frozen §J instrument, exactly as pasted there. Verdicts recorded verbatim below
BEFORE gate comparison; gate evaluation follows at the end.

## Judge 1 (triplet substrate: abolition_reading.pl, retributive_reading.pl, deterrence_reading.pl) — VERBATIM

All three files were read in full. Edge check: all three unordered pairs are connected by `coexists_with` in both directions (abolition→{retributive, deterrence}; retributive→{deterrence, abolition}; deterrence→{retributive, abolition}), so all three pairs qualify. None of the three files authors `cs_axiom`/`cs_axiom_status`/`cs_axiom_grounding` facts; per the instrument, commitments are taken from each file's own prose (SUMMARY, commentary, omegas, drift annotations).

### Pair {abolition_reading, retributive_reading}

**Verdict class:** `severed(one_categorical_permissibility_claim_demoted)`

**Mutation text:** The two readings author flatly contradictory categoricals over the identical domain (permissibility of state execution, same kernel `state_execution_authority`, whole-domain quantification on both sides), so exactly one of the two core claims — abolition's "impermissible regardless" or retributive's "execution is the only exit from the proportionality requirement" — must be DROPped (demoted from assertable to non-assertable). No RESTRICT/partition route exists because both claims quantify over all capital crimes ("regardless of crime severity" vs "the heinousness of the crime demands"), and abolition additionally asserts the negation of retributive's ground outright ("false legitimacy claim"). The choice of which side to demote is symmetric: both files author the contest as foundational and exclusive, and neither authors a basis for privileging a side — but either single demotion suffices, so the minimal mutation is one DROP.

**Witness quotes:**
- abolition: "the ABOLITION READING: the categorical claim that state execution is impermissible regardless of crime severity or procedural safeguards"; "The abolition reading rejects both the retributive claim (that proportional punishment including death is a natural law of justice) and the deterrence claim"; "rejects the retributive natural-law frame as a false legitimacy claim layered atop extractive institutional practice"; omega kernel_reading_contest: "The contest is not empirically resolvable; it is a foundational disagreement… If retributive reading holds: state execution is a rope or mountain… If abolition reading holds: state execution is a snare… and retributivism is a false legitimacy claim."
- retributive: "the heinousness of the crime demands a response of equivalent gravity, making execution the only exit from the proportionality requirement"; "no lesser penalty can satisfy the proportionality requirement"; "the framework cannot reduce extractiveness without abandoning its core claim."

**Footing:** RULED — the contradiction, its whole-domain scope, and its authored irresolvability are all carried verbatim in the two files.

### Pair {abolition_reading, deterrence_reading}

**Verdict class:** `severed(deterrence_efficacy_premise_demoted)`

**Mutation text:** One DROP suffices: demote the deterrence reading's operative empirical premise — that capital punishment deters beyond LWOP — from assertable to non-assertable; the deterrence file's own conditional apparatus then routes the constraint to snare (ε → 0.70+), converging with abolition's authored classification. Deterrence's conditional "if efficacious then justified" can remain assertable alongside abolition's prohibition, because abolition itself authors that its snare classification "depends on rejecting deterrence as a legitimate justification" and concedes the shift toward tangled_rope were deterrence real — so the joint framework is coherent once the antecedent is unasserted. The deterrence file's own drift annotation already records the premise as "substantially challenged" but "not acknowledged as dispositive" (engine: axiom_foreclosure); the mutation is completing an acknowledgment the file authors as withheld.

**Witness quotes:**
- abolition: "deterrence is empirically falsified — capital punishment does not deter murder at rates superior to life imprisonment"; omega deterrence_claim_empirical_status: "The abolition reading's snare classification depends on rejecting deterrence as a legitimate justification; if deterrence were real, the constraint would shift toward tangled_rope."
- deterrence: "The empirical efficacy of deterrence is contested, introducing omega variables that determine whether the constraint's extractiveness is justified (ε → 0.30, Rope) or excessive (ε → 0.70+, Snare)"; "systematic meta-analyses have substantially challenged the empirical premise (that CP deters more than LWOP). Criminal justice authority structure has not acknowledged this as dispositive. Engine computes: axiom_overriding + substantial + false → axiom_foreclosure"; "They benefit from reduced homicide risk IF deterrence efficacy is real."

**Footing:** RULED — the single demoted premise, the conditional routing to snare, and abolition's own contingency concession are all in-file text; no analogical step is load-bearing.

### Pair {retributive_reading, deterrence_reading}

**Verdict class:** `severed(exclusive_ground_of_legitimacy — deterrence's justificatory ground demoted)`

**Mutation text:** Each file authors an exclusive ground of legitimacy for the same act — retributive: proportional desert, explicitly "not from preventing future crimes"; deterrence: prevention of future murders, carrying a substitution license (execution substitutable if LWOP equally deters) that directly contradicts retributive's substitution prohibition — so any joint framework must violate one exclusivity clause. The minimal mutation is one DROP: demote the deterrence reading's ground-claim (legitimacy-from-prevention, including its conditional substitution license) from assertable; retributive's own commentary authors exactly this route ("the framework's only functional justification beyond retribution must be abandoned"). The symmetric mixed-theory alternative — dropping retributive's exclusion clause and keeping both grounds — additionally requires an ADDed priority rule neither file authors to resolve the mandatory-execution-vs-substitutability conflict, i.e., two mutations, so it is not minimal (the epistemic residue "evidence fails to support" vs "meta-analyses produce conflicting results" is jointly readable as "not established" and needs no mutation).

**Witness quotes:**
- retributive: "This constraint story instantiates ONLY the retributive reading: execution's legitimacy derives from its role in proportionately responding to grave moral wrongs, not from preventing future crimes or from incapacitating danger"; "the retributive framework cannot substitute imprisonment for execution without abandoning its core claim"; "as evidence fails to support deterrent effects (the framework's only functional justification beyond retribution must be abandoned, leaving pure extraction)."
- deterrence: "the state executes capital offenders to raise the cost of capital crimes and thereby prevent future murders"; "has arbitrage to substitute alternative punishments if equally deterring"; "If LWOP is equally deterring: execution is substitutable"; omega kernel_reading_contestation: "Which reading is adopted changes which future murder victims appear in the beneficiary set, whether the executed person is an instrumental cost or a rights-bearing end."

**Footing:** RULED — the dual exclusivity clauses, the substitution conflict, and the in-file abandonment route are all quoted from the files; the mixed-theory alternative is invoked only to show it is non-minimal, not as a load-bearing step.

## Judge 2 (axiom-bearing family: retributive_desert.pl, categorical_abolition.pl, deterrence_instrument.pl + state_killing_authority_contradictions.pl) — VERBATIM

### Pair: {retributive_desert, deterrence_instrument}

**Pair:** retributive_desert ↔ deterrence_instrument (coexists_with authored in both directions: `cs_reading_relation('c664083f-...', deterrence_instrument, coexists_with)` and `cs_reading_relation('b7bc7bf2-...', retributive_desert, coexists_with)`)

**Verdict class:** `severed(deterrence_exclusivity)`

**Mutation text:** The four authored axioms (murderers_forfeit_rights, proportionality_equals_justice, deterrence_empirical_efficacy, instrumental_life_cost_justified) are mutually assertable — the kernel's own contradiction file declares no contradiction between any RD and DI axiom — but both readings attach to the same kernel (`cs_kernel_id(..., state_killing_authority)` in both files), and DI's authored commitment that the practice is "justified specifically and exclusively through deterrence logic" cannot be true of a kernel of which RD's desert-justification is simultaneously true. Minimal mutation: DEMOTE/RESTRICT that single prose-carried exclusivity commitment in deterrence_instrument from assertable (deterrence becomes a sufficient-but-not-exclusive ground), leaving all four axioms and RD's commitments intact; the route touching RD instead ("the legitimate form of state killing") would be a second, unnecessary mutation provided RD's "form" claim is read as the death-for-death shape, which deterrence executions of murderers also satisfy. Note the files themselves author coexistence only distributively — "across different institutional actors" — not within one framework, so the zero-mutation reading is not authored.

**Witness quotes:**
- deterrence_instrument: "This constraint models capital punishment justified specifically and exclusively through deterrence logic"
- retributive_desert: "murderers forfeit their right to life and that proportional punishment (lex talionis: death for death) is the legitimate form of state killing"
- retributive_desert (dual formulation note): "deterrence_instrument coexists with retributive desert across different institutional actors"
- deterrence_instrument (false_positive_catastrophe omega): "If reframe to retribution: the reading loses its distinctive structure (becomes retributive_desert reading)"
- state_killing_authority_contradictions: declares only "murderers_forfeit_rights ↔ inalienable_life" and "deterrence_empirical_efficacy ↔ inalienable_life" — no RD↔DI axiom contradiction appears in the kernel's enumerated contradiction facts.

**Footing:** INFERRED. The incompatibility step is ruled by in-file text (both files key to the same kernel; the exclusivity clause is quoted), but two load-bearing steps of the minimal-repair sufficiency are not carried by the files: (a) reading RD's "the legitimate form" as a form-claim (death-for-death shape) rather than an exclusive-ground claim, and (b) treating the contradiction file's enumerative silence on RD↔DI as consistency of the four axioms. A different judge reading RD's sentence as an exclusive-ground claim would return a two-sided mutation (both exclusivities demoted) rather than one.

### Pair: {categorical_abolition, deterrence_instrument}

**Pair:** categorical_abolition ↔ deterrence_instrument (coexists_with authored in both directions: `cs_reading_relation('0ac32b16-...', deterrence_instrument, coexists_with)` and `cs_reading_relation('b7bc7bf2-...', categorical_abolition, coexists_with)`)

**Verdict class:** `severed(cs_axiom_grounding(inalienable_life): deontological → consequentialist)`

**Mutation text:** The kernel's contradiction file explicitly authors an axiom contradiction between this pair ("No single coherent framework can hold both that persons may be instrumentalized for collective benefit and that persons are categorically non-instrumentalizable"), so genuine(zero_mutation) is foreclosed by in-file fact. The minimal mutation is an OVERRIDE of one authored grounding: re-read categorical_abolition's foundational axiom inalienable_life from `deontological` to consequentialist, a route CA's own axiom_grounding_contest omega authors as the condition under which "the reading coexists_with deterrence_instrument if empirics change" — no axiom is dropped and no bridge added, and DI's side already authors its own conditionality (deterrence_empirical_efficacy is grounded `empirically_contingent`; "justified AS an instrument IF deterrence works at acceptable cost"), making the joint framework a single consequentialist frame in which the two readings are rival empirical bets. The equally single-commitment alternative — DROP deterrence_instrument's instrumental_life_cost_justified — removes an axiom outright and so touches more; note also that the declared contradiction facts name deterrence_empirical_efficacy against inalienable_life while the accompanying prose describes the instrumentalization clash (which maps to instrumental_life_cost_justified), a fact/prose mismatch a re-evaluating judge should see but which leaves the verdict unchanged, since the contradiction is authored under either axiom assignment.

**Witness quotes:**
- state_killing_authority_contradictions: "Deterrence reading treats the condemned person as an instrumental cost in a utilitarian calculus (permissible to kill if net lives saved); abolition reading treats persons as ends-in-themselves whose lives cannot be traded off for aggregate benefit. No single coherent framework can hold both..."
- state_killing_authority_contradictions: `cs_axiom_contradiction(deterrence_empirical_efficacy, inalienable_life).`
- categorical_abolition: "state execution, regardless of the crime or potential consequences (deterrence, desert, closure), constitutes a rights violation"
- categorical_abolition (axiom_grounding_contest omega): "If primarily consequentialist: the reading coexists_with deterrence_instrument if empirics change"
- deterrence_instrument: `cs_axiom_grounding('b7bc7bf2-...', deterrence_empirical_efficacy, empirically_contingent).` and (mandatrophy): "The reading declares: 'it is justified AS an instrument IF deterrence works at acceptable cost.'"
- categorical_abolition: `cs_axiom_grounding('0ac32b16-...', inalienable_life, deontological).` — the authored grounding the override touches.

**Footing:** RULED. Every load-bearing step cites in-file text: the impossibility of zero-mutation coexistence is a declared fact plus declared prose in the contradiction file; the specific minimal repair and its sufficiency are authored in CA's own omega (the consequentialist-grounding conditional); and the joint framework's viability on DI's side rests on DI's authored empirically_contingent grounding and conditional self-description.

---

Note on coverage: the third unordered pair, {retributive_desert, categorical_abolition}, is connected only by `forecloses` edges in both directions (no `coexists_with` in either direction), so under the instrument it is not a qualifying pair and no row is emitted for it.

## Gate evaluation (frozen §E semantics, Arm B, k=4 expected-nonzero + 1 expected-genuine)

| row | expectation | verdict returned | gate result |
|---|---|---|---|
| abolition \| retributive (triplet) | NONZERO | `severed(...)`, RULED | pass |
| abolition \| deterrence (triplet) | NONZERO | `severed(...)`, RULED | pass |
| retributive \| deterrence (triplet) | NONZERO | `severed(...)`, RULED | pass |
| categorical_abolition \| deterrence_instrument | NONZERO | `severed(...)`, RULED | pass |
| retributive_desert \| deterrence_instrument | `genuine(zero_mutation)` | `severed(deterrence_exclusivity)`, INFERRED | **FAILED against pre-registration** |

**Expected-nonzero gate: 4/4 nonzero — CONTROL PASSES. Fiat judging proceeds.**
No `undetermined` occurred; the edit-2 reduced-k fallback was not needed (noted:
the triplet rows came back RULED from prose despite the zero-axiom base — the
thin-base worry did not materialize).

**Expected-genuine control: FAILED** (nonzero, and on the exact shape operator
edit 3 predicted — an exclusivity/subordination conflict; the judge's own footing
is INFERRED and it flags that another judge would return a two-sided mutation).
Per frozen §E/§F: NOT HALT-bearing; reported against its pre-registration; the
`genuine` class remains **UNCALIBRATED everywhere**; and if the fiat table comes
back uniformly nonzero, the edit-3 pre-commitment applies verbatim: "an
oversensitive grammar and a genuinely mutation-laden corpus are not distinguished
by this audit, and the fiat annotations stand as annotations only."

Side-finding for the writeup (judge 2): fact/prose mismatch in
`state_killing_authority_contradictions.pl` — the contradiction FACT names
`deterrence_empirical_efficacy ↔ inalienable_life` while its rationale PROSE
describes the instrumentalization clash, which maps to
`instrumental_life_cost_justified`.
