% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__abolition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_legitimacy__abolition_reading, []).

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
 *   constraint_id: state_killing_legitimacy__abolition_reading
 *   human_readable: Capital Punishment as Categorical Dignity Violation (Abolitionist Reading)
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   Capital punishment persists in a subset of jurisdictions as a sentencing
 *   option for the most severe crimes. The abolitionist reading of the
 *   underlying legitimacy kernel holds that the practice is not merely
 *   imprudent or occasionally mis-administered but categorically wrong: it
 *   treats the condemned person's inviolable dignity as a side-constraint
 *   that cannot be traded off against any quantity of desert or deterrent
 *   benefit. Under this reading, the state's power to kill is itself the
 *   object under scrutiny — not a neutral tool that might be well or badly
 *   used, but a structurally illegitimate power whose exercise extracts an
 *   irreversible harm from a powerless population to serve institutional and
 *   political ends (prosecutorial closure rates, electoral signaling) that
 *   could be achieved by non-lethal means.
 *
 * KEY AGENTS:
 *   - condemned_persons: primary payer (powerless/trapped) — bears the irreversible extraction
 *   - wrongfully_convicted_death_row_inmates: sharpened payer subset — bears extraction that cannot be corrected post-execution
 *   - state_prosecutorial_apparatus: agenda-setter (institutional/arbitrage) — administers the killing power at full discretion
 *   - political_actors_running_on_toughness: beneficiary (powerful/mobile) — converts the power's existence into electoral capital
 *   - abolitionist_legal_advocates: excluded (organized/constrained) — voices the categorical objection without doctrinal power to end the practice
 *   - constitutional_courts: analytical observer (institutional/analytical) — adjudicates procedure without settling the categorical question
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__abolition_reading, 0.81).
domain_priors:suppression_score(state_killing_legitimacy__abolition_reading, 0.72).
domain_priors:theater_ratio(state_killing_legitimacy__abolition_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__abolition_reading, snare).
narrative_ontology:human_readable(state_killing_legitimacy__abolition_reading, "Capital Punishment as Categorical Dignity Violation (Abolitionist Reading)").
narrative_ontology:topic_domain(state_killing_legitimacy__abolition_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__abolition_reading, 'd8b1425b-f5fb-4f05-aeb3-fc23f6709af7').
narrative_ontology:cs_kernel_codification('d8b1425b-f5fb-4f05-aeb3-fc23f6709af7', distributed).
narrative_ontology:cs_authority_grounding('d8b1425b-f5fb-4f05-aeb3-fc23f6709af7', distributed).
narrative_ontology:cs_reading_relation('d8b1425b-f5fb-4f05-aeb3-fc23f6709af7', state_killing_legitimacy__retributive_reading, forecloses).
narrative_ontology:cs_reading_relation('d8b1425b-f5fb-4f05-aeb3-fc23f6709af7', state_killing_legitimacy__deterrence_reading, coexists_with).
narrative_ontology:cs_axiom('d8b1425b-f5fb-4f05-aeb3-fc23f6709af7', foundational, dignity_is_inviolable_regardless_of_desert).
narrative_ontology:cs_axiom_status(dignity_is_inviolable_regardless_of_desert, holdable).
narrative_ontology:cs_axiom_grounding('d8b1425b-f5fb-4f05-aeb3-fc23f6709af7', dignity_is_inviolable_regardless_of_desert, deontological).
narrative_ontology:cs_axiom('d8b1425b-f5fb-4f05-aeb3-fc23f6709af7', foundational, no_utility_calculus_can_license_state_killing).
narrative_ontology:cs_axiom_status(no_utility_calculus_can_license_state_killing, holdable).
narrative_ontology:cs_axiom_grounding('d8b1425b-f5fb-4f05-aeb3-fc23f6709af7', no_utility_calculus_can_license_state_killing, deontological).
narrative_ontology:cs_reference_frame('d8b1425b-f5fb-4f05-aeb3-fc23f6709af7', categorical_dignity_prohibition).
narrative_ontology:cs_drift_state('d8b1425b-f5fb-4f05-aeb3-fc23f6709af7', contemporary_abolition_movement_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('d8b1425b-f5fb-4f05-aeb3-fc23f6709af7', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__abolition_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__abolition_reading, state_prosecutorial_apparatus).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__abolition_reading, political_actors_running_on_toughness).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, condemned_persons).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, wrongfully_convicted_death_row_inmates).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, families_of_the_condemned).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__abolition_reading, crime_victims_families_seeking_closure).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__abolition_reading, inviolable_human_dignity_doctrine).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__abolition_reading, categorical_prohibition_of_state_killing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Held under sentence of death by the state. Have no exit from the constraint's application short of clemency, exoneration, or successful appeal — all of which are discretionary and rare. Under the abolitionist reading, this population is the rights-bearing party whose irreducible dignity the state killing power violates regardless of the crime committed.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, condemned_persons, payer,
    powerless, immediate, trapped, national).

% A documented subset of the condemned population later shown to be actually innocent, some after execution. Their existence is the abolitionist reading's central empirical anchor: irreversibility converts any error rate, however small, into an unrectifiable violation.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, wrongfully_convicted_death_row_inmates, payer,
    powerless, immediate, trapped, national).

% Bear the reputational, financial, and psychological cost of a state execution carried out against a relative. Have no standing to object to the sentence and no exit from the consequences once the arrangement is administratively activated.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, families_of_the_condemned, payer,
    powerless, biographical, trapped, national).

% Seeks, obtains, and administers death sentences; controls charging decisions, plea leverage, and the execution protocol itself. Retains full discretion over whether and how the killing power is exercised and can decline to seek it without institutional cost, giving it arbitrage-grade exit from any individual case.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, state_prosecutorial_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Campaign on capital punishment as a signal of toughness on crime, converting the killing power's continued existence into electoral capital independent of its actual effect on crime rates. Can shift positions or jurisdictions freely without personal exposure to the machinery they defend.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, political_actors_running_on_toughness, beneficiary,
    powerful, biographical, mobile, national).

% Some report the death sentence as providing closure or validating the severity of harm done to them; the abolitionist reading does not deny this experience but holds it cannot ground a categorical entitlement to kill, since it would make legitimacy contingent on the psychological state of survivors rather than on the condemned person's status as a rights-bearer. Their felt stake is real but excluded from the categorical argument on principle, not by procedural accident.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, crime_victims_families_seeking_closure, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_killing_legitimacy__abolition_reading, crime_victims_families_seeking_closure, excluded).

% Litigate against individual sentences and against the practice categorically, but operate within a legal system whose controlling doctrine (in most retentionist jurisdictions) treats the practice as constitutionally permissible. Their categorical objection is heard in courts but structurally cannot override legislative retention absent a constitutional ruling foreclosing the practice.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, abolitionist_legal_advocates, excluded,
    organized, generational, constrained, national).

% Adjudicate specific procedural challenges (method of execution, proportionality, intellectual disability) but in most retentionist systems have declined to hold the practice categorically unconstitutional, leaving the abolitionist claim live in argument but unvindicated in binding doctrine.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_legitimacy__abolition_reading, diffuse).
narrative_ontology:fixing_cost_class(state_killing_legitimacy__abolition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None recognized under this reading at the level of the individual killing — the abolitionist position holds that whatever coordination problem the death penalty purports to solve (deterrence, retribution, closure) can be achieved by non-lethal punishment, so the categorical prohibition itself is the coordination mechanism: a bright-line rule removing state killing from the available policy toolkit entirely, preventing case-by-case erosion of the dignity floor.
% TRANSFER_FUNCTION: Moves the condemned person's life, and the irreversible finality of that loss, from the individual to the state's punitive and political apparatus; converts a human life into an instrument of institutional legitimacy, deterrent signaling, or electoral positioning under the retentionist practice this reading condemns.
% ABSENT_VOICES: The condemned person's claim to inviolable dignity is voiced by advocates and dissenting jurists but is structurally absent from the retentionist legislative and prosecutorial process itself, which treats desert and utility as sufficient without engaging the categorical objection. Wrongfully convicted individuals who were executed cannot testify to their own innocence at all — the most probative absent voice is permanently silenced by the practice under review.
% DISAPPEARANCE_RATIONALE: If the state killing power were abolished under this reading, condemned populations would be resentenced to incapacitation short of death, prosecutorial charging calculus would shift away from capital-eligible charges, and political actors would lose a category of toughness signaling; the practical machinery of death rows, execution protocols, and capital appellate litigation would be dismantled entirely, a substantial institutional rearrangement.
% FOUNDING_PROBLEM: Historically, capital punishment was defended as necessary for public order, retribution proportionate to the gravest crimes, and deterrence of future violence in the absence of reliable long-term incapacitation. The abolitionist reading holds that modern incapacitation (life imprisonment without parole) has resolved the public-safety problem the practice was originally invoked to solve, leaving only retributive and symbolic functions that cannot, on this reading, override the categorical dignity claim.
% FOUNDING_PROBLEM_CORROBORATION: International human rights bodies (UN Human Rights Committee, regional human rights courts) and empirical criminology research from outside the prosecutorial and political beneficiary set attest that reliable incapacitation alternatives exist and that deterrence effects are not robustly demonstrated, supporting the abolitionist claim that the founding public-safety problem is functionally dead; retentionist governments and elected prosecutors dispute this and maintain the problem is live.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__abolition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__abolition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__abolition_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_killing_legitimacy__abolition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__abolition_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_legitimacy__abolition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_legitimacy__abolition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_legitimacy__abolition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81) because the reading treats every execution as a categorical, irreversible harm regardless of the crime's severity — there is no discount for desert under this reading, which is precisely what distinguishes it from the retributive sibling. Suppression is substantial (0.72) because the practice depends on active state machinery (charging discretion, appellate exhaustion requirements, execution protocols) to proceed against a population with no exit. Theater ratio is moderate (0.40) reflecting that procedural safeguards (appeals, clemency review, method litigation) perform legitimacy without engaging the categorical objection the abolitionist reading raises. Resistance is high (0.70) given active, organized abolitionist advocacy; accessibility_collapse is moderate (0.50) because non-lethal alternatives (life imprisonment) are demonstrably available and already administratively substitutable, distinguishing this from a true mountain where no alternative exists.
 *
 * DIRECTIONALITY LOGIC:
 *   Condemned persons and the wrongfully convicted are declared victims with powerless/trapped structural position — under the abolitionist reading they are rights-bearing beneficiaries of the categorical claim being asserted (i.e., the dignity doctrine benefits them if honored) but victims of the actual extraction when the state killing power is exercised against them; this dual structure is why the story frames the killing power itself, not the condemned person, as failing the legitimacy test. The state prosecutorial apparatus and toughness-signaling political actors are declared beneficiaries with institutional/powerful positions and mobile-to-arbitrage exit, consistent with the engine's expectation that agenda-setters with discretion sit near the beneficiary end of directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem framing separates the historical public-safety rationale (incapacitating dangerous offenders in the absence of reliable long-term imprisonment) from the practice's current operation. Corroboration from international human rights bodies and criminological research outside the prosecutorial beneficiary set supports reading the founding problem as functionally dead — reliable incapacitation now exists — while the practice persists for retributive and political-signaling reasons the abolitionist reading holds insufficient. This prevents the classification from either treating the practice as untouchable natural law (it manifestly has an administratively substitutable alternative) or dismissing retentionist argument as pure bad faith (the founding_problem_status is authored as contested, not resolved, because retentionist actors dispute the corroborating research in good faith).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_state_killing,
    'Is the legitimacy of state killing correctly evaluated under the categorical dignity frame (this reading), the proportional-desert frame (retributive_reading), or the forward-looking-signal frame (deterrence_reading)? Each reading instantiates a structurally different constraint with a different beneficiary/victim map and different ε.',
    'This is not empirically resolvable within a single framework — it depends on which normative premise (inviolable dignity vs. proportional forfeiture vs. aggregate welfare) is taken as foundational. Track it via which reading a given jurisdiction''s constitutional doctrine or legislative record actually encodes, and note where doctrine shifts between readings over time.',
    'Under the abolitionist reading, ε is high and the practice is snare-shaped (categorical extraction from a powerless population). Under the retributive reading, ε would be computed against a desert-satisfaction baseline that could show much lower extraction for cases of accurate conviction. Under the deterrence reading, ε turns on contested empirical deterrence data. The three readings are linked as siblings, not merged, and this omega documents that the kernel choice — not new facts — is what would move the classification between them.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_state_killing, conceptual, 'Which of the three sibling readings of the state_killing_legitimacy kernel governs the practice''s actual legitimacy.').

omega_variable(
    irreversibility_error_rate_interaction,
    'Does the categorical dignity violation claimed by this reading depend at all on the wrongful-conviction error rate, or would it hold even under a counterfactual zero-error regime?',
    'Distinguish the pure abolitionist argument (killing violates dignity even for the guilty) from the error-contingent argument (killing is wrong because errors are irreversible) by examining whether abolitionist advocacy persists in jurisdictions with documented near-zero wrongful execution and whether the argument''s force changes.',
    'If the categorical claim is independent of error rate, ε should not be treated as sensitive to forensic reform; if the claim is substantially error-rate-driven, then improved conviction accuracy would lower the effective ε under this reading, which would blur its distinctness from a narrower wrongful-conviction-focused reform position rather than a full abolitionist one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irreversibility_error_rate_interaction, conceptual, 'Whether the categorical claim is error-rate-independent or partially contingent on wrongful-conviction risk.').

omega_variable(
    cross_reading_coupling_political_incentive,
    'Do political actors who benefit from toughness signaling under this reading also structurally benefit under the retributive and deterrence readings, such that the beneficiary set is stable across all three sibling constraints regardless of which normative frame is invoked?',
    'Compare the beneficiaries[] declared across the three sibling stories; if political_actors_running_on_toughness or its structural equivalent recurs in all three, that indicates the political beneficiary structure is invariant to the normative framing chosen, which is itself evidence for a cynical-instrumentalization reading layered atop all three normative readings.',
    'If the beneficiary structure is stable across readings, it suggests the normative debate (dignity vs. desert vs. deterrence) may function partly as legitimating cover for a stable political-extraction structure underneath — relevant to, but distinct from, the kernel contest itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cross_reading_coupling_political_incentive, empirical, 'Whether political beneficiaries are stable across all three sibling readings of the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__abolition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_legitimacy__abolition_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(stat_tr_t10, state_killing_legitimacy__abolition_reading, theater_ratio, 10, 0.27).
narrative_ontology:measurement(stat_tr_t20, state_killing_legitimacy__abolition_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement(stat_tr_t30, state_killing_legitimacy__abolition_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(stat_tr_t40, state_killing_legitimacy__abolition_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(stat_tr_t50, state_killing_legitimacy__abolition_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_legitimacy__abolition_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(stat_be_t10, state_killing_legitimacy__abolition_reading, base_extractiveness, 10, 0.73).
narrative_ontology:measurement(stat_be_t20, state_killing_legitimacy__abolition_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(stat_be_t30, state_killing_legitimacy__abolition_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement(stat_be_t40, state_killing_legitimacy__abolition_reading, base_extractiveness, 40, 0.8).
narrative_ontology:measurement(stat_be_t50, state_killing_legitimacy__abolition_reading, base_extractiveness, 50, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_legitimacy__abolition_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(stat_su_t10, state_killing_legitimacy__abolition_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(stat_su_t20, state_killing_legitimacy__abolition_reading, suppression_requirement, 20, 0.64).
narrative_ontology:measurement(stat_su_t30, state_killing_legitimacy__abolition_reading, suppression_requirement, 30, 0.67).
narrative_ontology:measurement(stat_su_t40, state_killing_legitimacy__abolition_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(stat_su_t50, state_killing_legitimacy__abolition_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__abolition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_legitimacy__abolition_reading, state_killing_legitimacy__retributive_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__abolition_reading, state_killing_legitimacy__deterrence_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language 'death penalty legitimacy' claim per the ε-invariance principle: abolition_reading (this file, categorical dignity violation, high ε, snare-shaped), retributive_reading (proportional forfeiture through lex talionis, distinct beneficiary/victim structure keyed to desert-satisfaction), and deterrence_reading (rational-actor crime-prevention signal, ε keyed to contested empirical deterrence data). Each carries its own claimed_type and metrics; they are linked here rather than merged because measuring 'the death penalty' under each normative frame yields materially different ε values — exactly the decomposition the framework requires rather than tolerating an observer-relative single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
