% ============================================================================
% CONSTRAINT STORY: state_killing_authority__retributive_desert
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_authority__retributive_desert, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: state_killing_authority__retributive_desert
 *   human_readable: Retributive-Desert Capital Punishment (Lex Talionis Reading of State Killing Authority)
 *   domain: criminal justice/political philosophy/constitutional law
 *
 * SUMMARY:
 *   In retentionist jurisdictions, a person convicted of capital murder may
 *   be executed by the state on the ground that the crime forfeited their
 *   right to life and that proportional punishment — death for death — is
 *   owed as a matter of justice. This story instantiates the
 *   retributive-desert reading of the state-killing kernel (see
 *   commentary.kernel_context): the arrangement described is the standing
 *   capital-punishment apparatus — death-eligible murder definitions,
 *   individualized sentencing before capital juries, appellate and clemency
 *   machinery, scheduled executions — assessed by this reading's own lights.
 *   The reading counts the execution of the genuinely deserving murderer as
 *   the discharge of a debt rather than a taking; what it counts as
 *   cost-bearing is the arrangement's documented misapplication: wrongful
 *   convictions (exoneration arriving after decades, sometimes too late),
 *   arbitrary and disparate application that fails to track desert, the years
 *   of death-row existence under warrant, and the collateral grief imposed on
 *   the condemned's families with no standing in the process. The victim
 *   enters the beneficiary set posthumously through vindication; the
 *   condemned exits the rights-holder set through forfeiture; the state's
 *   authority is grounded in the proportionality norm rather than in claimed
 *   outcomes. The claim and the metrics are authored independently: the
 *   claimed type (tangled_rope) is what I believe structurally true of the
 *   arrangement — a genuine coordination function (monopolized,
 *   proportionality-bounded response to the gravest crime) carrying
 *   asymmetric costs (the condemned's life, the error borne by the innocent)
 *   under active enforcement — while the metrics describe the arrangement's
 *   operation as this reading's own lights assess it.
 *
 * KEY AGENTS:
 *   - murder_victims_posthumously: beneficiary seat (powerless/trapped) — the party in whose name the execution is administered; collects vindication posthumously
 *   - murder_victim_survivors: beneficiary seat (moderate/constrained) — receive the expressive good of vindication, unevenly
 *   - retentionist_state_authorities: agenda_setter seat (institutional/arbitrage) — defines, administers, and can dissolve the arrangement; collects the vindication-of-authority payoff (secondary beneficiary)
 *   - condemned_murderers: primary target seat (powerless/trapped) — bears the arrangement's ultimate cost
 *   - wrongly_convicted_capital_defendants: target seat (powerless/trapped) — bear the error rate at full severity
 *   - condemned_persons_families: target seat (powerless/trapped) — bear collateral grief with no standing in the process
 *   - capital_juries: case-level agenda_setter (moderate/constrained) — render the individualized desert findings
 *   - criminal_law_scholars: analytical observer (analytical/analytical) — sees the full structure across jurisdictions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__retributive_desert, 0.46).
domain_priors:suppression_score(state_killing_authority__retributive_desert, 0.72).
domain_priors:theater_ratio(state_killing_authority__retributive_desert, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, extractiveness, 0.46).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__retributive_desert, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__retributive_desert, "Retributive-Desert Capital Punishment (Lex Talionis Reading of State Killing Authority)").
narrative_ontology:topic_domain(state_killing_authority__retributive_desert, "criminal justice/political philosophy/constitutional law").

domain_priors:requires_active_enforcement(state_killing_authority__retributive_desert).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__retributive_desert, '10610ca3-67b6-46ab-b42d-9c9644ede6cf').
narrative_ontology:cs_kernel_codification('10610ca3-67b6-46ab-b42d-9c9644ede6cf', formalized).
narrative_ontology:cs_authority_grounding('10610ca3-67b6-46ab-b42d-9c9644ede6cf', lineage).
narrative_ontology:cs_interpretation_layer_present('10610ca3-67b6-46ab-b42d-9c9644ede6cf').
narrative_ontology:cs_reading_relation('10610ca3-67b6-46ab-b42d-9c9644ede6cf', state_killing_authority__deterrence_instrument, coexists_with).
narrative_ontology:cs_reading_relation('10610ca3-67b6-46ab-b42d-9c9644ede6cf', state_killing_authority__categorical_abolition, forecloses).
narrative_ontology:cs_axiom('10610ca3-67b6-46ab-b42d-9c9644ede6cf', foundational, rights_forfeitable_through_capital_crime).
narrative_ontology:cs_axiom_status(rights_forfeitable_through_capital_crime, holdable).
narrative_ontology:cs_axiom_grounding('10610ca3-67b6-46ab-b42d-9c9644ede6cf', rights_forfeitable_through_capital_crime, deontological).
narrative_ontology:cs_axiom('10610ca3-67b6-46ab-b42d-9c9644ede6cf', foundational, proportional_death_owed_for_murder).
narrative_ontology:cs_axiom_status(proportional_death_owed_for_murder, holdable).
narrative_ontology:cs_axiom_grounding('10610ca3-67b6-46ab-b42d-9c9644ede6cf', proportional_death_owed_for_murder, deontological).
narrative_ontology:cs_axiom('10610ca3-67b6-46ab-b42d-9c9644ede6cf', secondary, state_monopoly_on_talionic_execution).
narrative_ontology:cs_axiom_status(state_monopoly_on_talionic_execution, holdable).
narrative_ontology:cs_axiom_grounding('10610ca3-67b6-46ab-b42d-9c9644ede6cf', state_monopoly_on_talionic_execution, conventional).
narrative_ontology:cs_reference_frame('10610ca3-67b6-46ab-b42d-9c9644ede6cf', lex_talionis_desert_settlement).
narrative_ontology:cs_drift_state('10610ca3-67b6-46ab-b42d-9c9644ede6cf', post_innocence_movement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('10610ca3-67b6-46ab-b42d-9c9644ede6cf', '').
narrative_ontology:cs_kernel_id(state_killing_authority__retributive_desert, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, murder_victims_posthumously).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, murder_victim_survivors).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, retentionist_state_authorities).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, condemned_murderers).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, wrongly_convicted_capital_defendants).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, condemned_persons_families).
narrative_ontology:constraint_vindicates(state_killing_authority__retributive_desert, lex_talionis_proportionality_doctrine).
narrative_ontology:constraint_vindicates(state_killing_authority__retributive_desert, rights_forfeiture_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% People killed deliberately by another. The arrangement administers the killer's death in their name and treats the execution as the public vindication of their worth. They collect this vindication only posthumously, through survivors and state pronouncements; they cannot act, consent, refuse, or revise what is done in their name.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, murder_victims_posthumously, beneficiary,
    powerless, biographical, trapped, national).

% Family members of murder victims. The arrangement offers them the killer's death as vindication and closure; many attend executions and advocate for them. The good is expressive and unevenly received — a substantial minority of survivors report no relief and some actively oppose execution — and once the crime has occurred they cannot leave their position as survivors; the process, not they, controls its pacing across decades of appeals.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, murder_victim_survivors, beneficiary,
    moderate, biographical, constrained, national).

% People convicted of capital murder and sentenced to death. The arrangement holds that their crime forfeited their right to life and administers their execution, preceded by years to decades confined on death row under a death warrant. Their only ways out — appellate reversal, commutation, clemency — are controlled by the same authorities that sentenced them and rarely open; they cannot leave the jurisdiction of the sentence.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, condemned_murderers, payer,
    powerless, immediate, trapped, national).

% People convicted and death-sentenced for murders they did not commit. They bear the arrangement's error at full severity: exoneration, when it comes, typically arrives after decades, and for some it arrives too late or never. By the reading's own proportionality standard their treatment is the gravest misfire the arrangement can produce.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, wrongly_convicted_capital_defendants, payer,
    powerless, biographical, trapped, national).

% Parents, children, and siblings of the condemned. They bear the execution's collateral costs — the loss, the stigma, the witnessing — while having no standing in the desert determination and no veto over it; the vindication framing that justifies the killing gives their grief no weight in the accounting.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, condemned_persons_families, payer,
    powerless, biographical, trapped, national).

% Legislatures, governors, courts, and prosecutors of jurisdictions that retain capital punishment. They define death-eligible crimes, seek and impose death sentences, run the appeals and clemency machinery, and schedule executions. They ground the authority in the proportionality norm — death for death as owed desert — rather than in claimed crime-prevention outcomes, and they can expand, contract, suspend, or abolish the arrangement by statute, executive act, or judicial decision. The expressive payoff of answering the gravest crime with the gravest penalty, and of discharging the public demand for desert, accrues to them.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, retentionist_state_authorities, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(state_killing_authority__retributive_desert, retentionist_state_authorities, beneficiary).

% Citizens compelled to serve in capital trials. They make the individualized determination of whether the defendant's crime warrants death, guided by aggravating and mitigating instructions. Their service is bounded by the statute and the judge's instructions; they cannot decline once empaneled, and their verdicts are the arrangement's desert findings at the case level.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, capital_juries, agenda_setter,
    moderate, immediate, constrained, regional).

% Comparative criminal-law and sentencing researchers who study capital arrangements across jurisdictions. They document error rates, application disparities, and the historical genealogy of proportionality limits, and they neither impose nor receive the arrangement's costs or benefits.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, criminal_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_authority__retributive_desert, retentionist_state_authorities).
narrative_ontology:fixing_cost_class(state_killing_authority__retributive_desert, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels the community's response to its gravest crime into a single bounded, state-administered procedure: it monopolizes retaliation (superseding private vengeance and blood feud), caps the response at the proportionality limit (death for death, not escalating vendetta), and renders a public finding that the victim's life counted and the killing is answered.
% TRANSFER_FUNCTION: Moves the condemned murderer's life — and the years of death-row existence preceding it — from the condemned to the vindication of the victim's name and the state's proportionality claim; secondarily moves the risk of error onto the wrongly convicted and grief onto the condemned's families, neither of whom consented to carry it.
% ABSENT_VOICES: The condemned's own account of their desert is never solicited — the forfeiture judgment is rendered about them without them, and a last statement is the only speech the process permits them. Murder victims who opposed execution, a documented minority of survivors, are sidelined by the vindication framing that claims to speak for them. The wrongly convicted cannot testify to the arrangement's error until exoneration, which for some arrives only posthumously. None of these seats is in the room where death-eligibility is defined.
% DISAPPEARANCE_RATIONALE: Capital statutes, death rows, execution protocols, clemency machinery, and the proportionality doctrine's application would dissolve or repurpose within years; death-eligible murders would draw life sentences; the vindication channel would reroute toward memorialization and survivor advocacy; the state would lose the expressive instrument by which it answers the gravest crime with the gravest penalty. The broader settlement the arrangement once served — monopolized retaliation — would persist, because it is carried by the criminal law as a whole rather than by the capital statute specifically.
% FOUNDING_PROBLEM: Unbounded private retaliation for homicide: before centralized punishment, killing was answered by kin-group vengeance in escalating cycles, and the earliest talionic codes capped retaliation at exact proportion — a life for a life — while vesting its administration in authority. The modern arrangement inherits that settlement: the state monopolizes the answer to murder and caps it at the proportionality limit.
% FOUNDING_PROBLEM_CORROBORATION: Criminal-law historians attest the genealogy from outside the arrangement's benefiting parties: talionic provisions appear in the earliest written codes as caps on retaliation (the Code of Hammurabi's scaled penalties, the Roman Twelve Tables, the commutation of blood feud into weregild), not as endorsements of killing. Comparative criminology attests that the feud-suppression settlement now rests on the state's general violence monopoly: long-abolitionist jurisdictions show no resurgence of private vengeance. No source outside the benefiting parties attests that the feud problem persists in retentionist democracies; retentionist authorities instead defend the arrangement on desert grounds, which is a successor rationale rather than the founding one.
narrative_ontology:disappearance_verdict(state_killing_authority__retributive_desert, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__retributive_desert, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__retributive_desert, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_killing_authority__retributive_desert, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__retributive_desert, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__retributive_desert_tests).
:- end_tests(state_killing_authority__retributive_desert_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.46: the reading's own lights count the deserved core of executions as debt discharge rather than taking, but the standing arrangement's documented error rate, arbitrary application, and death-row confinement register as cost-bearing even on those lights — hence a moderate value rather than a near-zero one. Suppression is authored at 0.72 as a raw structural property (the engine scales only extractiveness, by directionality and scope): the condemned has no exit from the sentence — appeals and clemency are controlled by the same authorities that imposed it and rarely open — and persistence depends on the state's continuous enforcement machinery rather than participant preference. Theater is 0.34 and rising across the interval: the individualized-desert apparatus (aggravating factors, mitigation, proportionality review) is presented as tracking who deserves death, while the accumulated record of wrongful convictions and geographic and racial disparities shows that tracking function failing — the ritual persists as the function atrophies. Accessibility collapse is 0.38: alternatives (life imprisonment, abolition) remain live and practiced in most of the world's jurisdictions; the constraint does not close them. Resistance is 0.70: the arrangement meets sustained organized resistance — capital defense litigation, innocence projects, clemency campaigns, repeal movements, international pressure. The measurement series run on one shared grid (T=0 to T=48, one year per unit; T=0 corresponds to 1976, when the modern guided-discretion era opened, T=48 to 2024): base_extractiveness rises as desert-mismatch documentation accumulated; theater_ratio rises on the same evidence; suppression_requirement traces the enforcement-capacity arc — build-up through the 1980s and 1990s (expanded death-eligibility, habeas restriction), peak around 2000, partial decay thereafter (moratoria, execution-method litigation, repeal waves) while the core lack of exit for the condemned persists.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda-setter seats experience the same structure differently. From the condemned's position the arrangement is a closed trap with maximal stakes: no exit, no consent, death as the cost. From the state's position it is a proportionality settlement it administers and from which it collects expressive authority; from the survivors' position it is vindication, received unevenly. The posthumous victim seat is the reading's own construction — a beneficiary who cannot act, consent, or refuse — and its classification depends on whether posthumous vindication is a real collection (see the posthumous_vindication_beneficiary omega). The engine computes these divergences from the structural data; the axioms below carry the reading's normative gloss (deserved, therefore not taken) and do not bind the computation.
 *
 * DIRECTIONALITY LOGIC:
 *   The condemned murderer is the full structural target: declared victim, powerless, no exit — the arrangement takes their life, so d sits near 1.0 and effective extraction is maximal for that seat. The wrongly convicted and the condemned's families are also targets: they bear costs (death by error; collateral grief) they were never found to deserve. The posthumous victim sits near the beneficiary end by declaration — vindication flows in their name — though they are a trapped, non-acting beneficiary, an unusual seat the derivation handles through the beneficiary declaration and whose status is flagged in the omegas. Survivors are partial beneficiaries: the vindication good is real but unevenly received, so their d sits above the pure-beneficiary end. The state is a low-d seat with arbitrage-grade exit: it administers the arrangement and collects its expressive payoff, and it can exit by statute at any time — the seat that could fix the arrangement and bears none of its costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The arrangement's founding problem — suppressing unbounded private retaliation for killing — is dead in retentionist democracies: the state's general violence monopoly, not capital punishment specifically, holds that settlement, and long-abolitionist jurisdictions show no resurgence of private vengeance. What persists is maintained on the desert rationale, actively administered and fiercely contested rather than left to inertia. The classification keeps both facts visible: claiming tangled_rope rather than snare preserves the genuine coordination function (monopolized, capped retaliation) so the arrangement's costs are not misread as pure predation; authoring the victims and the rising theater series keeps the costs and the desert-tracking failure visible so the arrangement is not misread as pure coordination. The R5 mismatch (dead founding problem, world-rearranging presence) is the mandatrophy signal here — but the arrangement is not a piton: it has concentrated collectors (the state's expressive payoff) and concentrated targets, and it is maintained deliberately, not by default.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint instantiates the retributive_desert reading of the state_killing_authority kernel; how would the sibling readings (state_killing_authority__deterrence_instrument, state_killing_authority__categorical_abolition) restructure the beneficiary/victim sets and epsilon if adopted instead?',
    'Read alongside the sibling constraint stories: compare beneficiary/victim sets, vindicated propositions, and epsilon values across the three files. The disagreement is located in whether the right to life is forfeitable through wrongdoing, and whether posthumous vindication can ground state killing.',
    'Under the deterrence_instrument reading the victim''s posthumous vindication drops out of the beneficiary set (outcomes, not vindication, justify) and the wrongly convicted become a cost input rather than a desert violation; under the categorical_abolition reading the condemned never exits the rights-holder set, the vindication grounds empty out of the beneficiary set, and the standing arrangement is assessed as maximally extractive by the abolitionist reading''s own lights.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story is one reading of the state-killing kernel among three, not the kernel itself.').

omega_variable(
    forfeiture_desert_trackability,
    'Can a state apparatus track desert accurately enough that only those who deserve death are executed — or is desert-matching beyond institutional capability, making the error rate a permanent cost even under this reading''s own lights?',
    'Longitudinal exoneration and error-rate studies of capital convictions: post-DNA exonerations, innocence-commission findings, and comparative error rates between capital and non-capital murder convictions.',
    'If desert-tracking is institutionally impossible, the reading''s own proportionality standard condemns the standing arrangement and effective extraction rises toward the maximal end for every seat; if the error rate is compressible toward zero, the coordination-plus-cost structure stabilizes as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(forfeiture_desert_trackability, empirical, 'Whether the arrangement can track the desert its own justification requires.').

omega_variable(
    posthumous_vindication_beneficiary,
    'Can a murdered person be a beneficiary — collecting vindication posthumously — or does all vindication actually accrue to survivors and the state, leaving the posthumous beneficiary entry a construction of the reading itself?',
    'Survivor-outcome research (whether executions deliver relief), philosophical analysis of posthumous interests, and comparison with arrangements that honor murder victims without killing (memorialization, restorative processes).',
    'If posthumous benefit is rejected, the beneficiary set reduces to survivors (whose benefit is partial and unevenly received) plus the state''s expressive payoff, thinning the coordination side of the structure and raising the share of the arrangement that operates as pure cost-bearing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(posthumous_vindication_beneficiary, conceptual, 'Whether the posthumous beneficiary entry is a real collection seat or a reading-internal construction.').

omega_variable(
    proportionality_cap_or_mandate,
    'Does lex talionis require death for death (proportionality as a mandate the state must execute) or merely cap punishment at death (proportionality as a limit permitting lesser penalties)?',
    'Historical-philological analysis of the talionic codes (Code of Hammurabi, Mosaic law, Roman Twelve Tables) and their reception, plus doctrinal analysis of whether modern desert theory reads proportionality as mandatory or as a ceiling.',
    'If proportionality is only a cap, the reading permits life imprisonment as fully proportional and epsilon falls sharply; if it is a mandate, clemency and commutation become proportionality failures and the arrangement''s enforcement burden is higher than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_cap_or_mandate, conceptual, 'Whether the talionic norm binds upward (mandate) or downward (cap).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__retributive_desert, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(retributive_desert_tr_t0, state_killing_authority__retributive_desert, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(retributive_desert_tr_t0, observed).
narrative_ontology:measurement(retributive_desert_tr_t8, state_killing_authority__retributive_desert, theater_ratio, 8, 0.21).
narrative_ontology:measurement_basis(retributive_desert_tr_t8, observed).
narrative_ontology:measurement(retributive_desert_tr_t16, state_killing_authority__retributive_desert, theater_ratio, 16, 0.25).
narrative_ontology:measurement_basis(retributive_desert_tr_t16, observed).
narrative_ontology:measurement(retributive_desert_tr_t24, state_killing_authority__retributive_desert, theater_ratio, 24, 0.28).
narrative_ontology:measurement_basis(retributive_desert_tr_t24, observed).
narrative_ontology:measurement(retributive_desert_tr_t32, state_killing_authority__retributive_desert, theater_ratio, 32, 0.31).
narrative_ontology:measurement_basis(retributive_desert_tr_t32, observed).
narrative_ontology:measurement(retributive_desert_tr_t40, state_killing_authority__retributive_desert, theater_ratio, 40, 0.33).
narrative_ontology:measurement_basis(retributive_desert_tr_t40, observed).
narrative_ontology:measurement(retributive_desert_tr_t48, state_killing_authority__retributive_desert, theater_ratio, 48, 0.34).
narrative_ontology:measurement_basis(retributive_desert_tr_t48, observed).

% Extraction over time
narrative_ontology:measurement(retributive_desert_be_t0, state_killing_authority__retributive_desert, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(retributive_desert_be_t0, observed).
narrative_ontology:measurement(retributive_desert_be_t8, state_killing_authority__retributive_desert, base_extractiveness, 8, 0.36).
narrative_ontology:measurement_basis(retributive_desert_be_t8, observed).
narrative_ontology:measurement(retributive_desert_be_t16, state_killing_authority__retributive_desert, base_extractiveness, 16, 0.39).
narrative_ontology:measurement_basis(retributive_desert_be_t16, observed).
narrative_ontology:measurement(retributive_desert_be_t24, state_killing_authority__retributive_desert, base_extractiveness, 24, 0.42).
narrative_ontology:measurement_basis(retributive_desert_be_t24, observed).
narrative_ontology:measurement(retributive_desert_be_t32, state_killing_authority__retributive_desert, base_extractiveness, 32, 0.44).
narrative_ontology:measurement_basis(retributive_desert_be_t32, observed).
narrative_ontology:measurement(retributive_desert_be_t40, state_killing_authority__retributive_desert, base_extractiveness, 40, 0.45).
narrative_ontology:measurement_basis(retributive_desert_be_t40, observed).
narrative_ontology:measurement(retributive_desert_be_t48, state_killing_authority__retributive_desert, base_extractiveness, 48, 0.46).
narrative_ontology:measurement_basis(retributive_desert_be_t48, observed).

% Suppression requirement over time
narrative_ontology:measurement(retributive_desert_su_t0, state_killing_authority__retributive_desert, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(retributive_desert_su_t0, observed).
narrative_ontology:measurement(retributive_desert_su_t8, state_killing_authority__retributive_desert, suppression_requirement, 8, 0.56).
narrative_ontology:measurement_basis(retributive_desert_su_t8, observed).
narrative_ontology:measurement(retributive_desert_su_t16, state_killing_authority__retributive_desert, suppression_requirement, 16, 0.74).
narrative_ontology:measurement_basis(retributive_desert_su_t16, observed).
narrative_ontology:measurement(retributive_desert_su_t24, state_killing_authority__retributive_desert, suppression_requirement, 24, 0.82).
narrative_ontology:measurement_basis(retributive_desert_su_t24, observed).
narrative_ontology:measurement(retributive_desert_su_t32, state_killing_authority__retributive_desert, suppression_requirement, 32, 0.78).
narrative_ontology:measurement_basis(retributive_desert_su_t32, observed).
narrative_ontology:measurement(retributive_desert_su_t40, state_killing_authority__retributive_desert, suppression_requirement, 40, 0.74).
narrative_ontology:measurement_basis(retributive_desert_su_t40, observed).
narrative_ontology:measurement(retributive_desert_su_t48, state_killing_authority__retributive_desert, suppression_requirement, 48, 0.72).
narrative_ontology:measurement_basis(retributive_desert_su_t48, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__retributive_desert, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, state_killing_authority__deterrence_instrument).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, state_killing_authority__categorical_abolition).

% DUAL FORMULATION NOTE:
% Kernel family decomposition: the colloquial label 'the capital punishment debate' covers three structurally distinct constraints instantiating one kernel (state_killing_authority). This file is the retributive_desert reading; the deterrence_instrument and categorical_abolition readings are separate stories with their own epsilon values, beneficiary/victim sets, and axioms, linked here via affects_constraints. Within any single normative framework that holds this reading's forfeiture premise, the categorical_abolition premise (inalienability) is ruled out — the two readings are related as forecloses — while the deterrence_instrument reading coexists with this one (parties can hold both desert and deterrence grounds simultaneously).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
