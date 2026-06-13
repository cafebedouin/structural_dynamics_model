% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__absolute_prohibition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__absolute_prohibition, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: humane_treatment_standard__absolute_prohibition
 *   human_readable: Absolute Prohibition of Torture and Degrading Treatment (Common Article 3 Reading)
 *   domain: international_humanitarian_law/human_rights
 *
 * SUMMARY:
 *   This constraint represents the 'absolute prohibition' reading of Common
 *   Article 3 of the Geneva Conventions, which establishes non-derogable
 *   minimum standards for humane treatment in armed conflict, explicitly
 *   forbidding torture and degrading treatment under any circumstances. This
 *   reading asserts that these standards are universal and absolute, forming
 *   a foundational 'mountain' of international law that cannot be
 *   circumvented by claims of national security or military necessity. The
 *   structural delta for this reading is that detainees are full
 *   rights-holders, state interrogation methods are absolutely constrained,
 *   and no security exception permits crossing the threshold of humane
 *   treatment.
 *
 * KEY AGENTS:
 *   - detainees: Primary beneficiary (powerless/trapped) — protected absolutely.
 *   - state_parties_to_geneva_conventions: Agenda-setter (institutional/constrained) — bound to uphold and enforce.
 *   - human_rights_advocates: Beneficiary (organized/mobile) — empowered by the clarity of the standard.
 *   - interrogators_and_security_personnel: Payer (moderate/constrained) — methods strictly limited.
 *   - national_security_apparatus: Payer (institutional/constrained) — operational methods constrained.
 *   - international_criminal_courts: Agenda-setter (institutional/analytical) — enforce the prohibition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__absolute_prohibition, 0.1).
domain_priors:suppression_score(humane_treatment_standard__absolute_prohibition, 0.2).
domain_priors:theater_ratio(humane_treatment_standard__absolute_prohibition, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, extractiveness, 0.1).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__absolute_prohibition, mountain).
narrative_ontology:human_readable(humane_treatment_standard__absolute_prohibition, "Absolute Prohibition of Torture and Degrading Treatment (Common Article 3 Reading)").
narrative_ontology:topic_domain(humane_treatment_standard__absolute_prohibition, "international_humanitarian_law/human_rights").

domain_priors:requires_active_enforcement(humane_treatment_standard__absolute_prohibition).
domain_priors:emerges_naturally(humane_treatment_standard__absolute_prohibition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__absolute_prohibition, '3ecc99b2-ad31-4f4c-943e-a626157957bc').
narrative_ontology:cs_kernel_codification('3ecc99b2-ad31-4f4c-943e-a626157957bc', fixed_text).
narrative_ontology:cs_authority_grounding('3ecc99b2-ad31-4f4c-943e-a626157957bc', lineage).
narrative_ontology:cs_interpretation_layer_present('3ecc99b2-ad31-4f4c-943e-a626157957bc').
narrative_ontology:cs_reading_relation('3ecc99b2-ad31-4f4c-943e-a626157957bc', humane_treatment_standard__contextual_necessity, forecloses).
narrative_ontology:cs_reading_relation('3ecc99b2-ad31-4f4c-943e-a626157957bc', humane_treatment_standard__proportionality_balancing, forecloses).
narrative_ontology:cs_axiom('3ecc99b2-ad31-4f4c-943e-a626157957bc', foundational, human_dignity_is_absolute).
narrative_ontology:cs_axiom_status(human_dignity_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('3ecc99b2-ad31-4f4c-943e-a626157957bc', human_dignity_is_absolute, deontological).
narrative_ontology:cs_axiom('3ecc99b2-ad31-4f4c-943e-a626157957bc', foundational, no_exceptions_to_inhumane_treatment).
narrative_ontology:cs_axiom_status(no_exceptions_to_inhumane_treatment, holdable).
narrative_ontology:cs_axiom_grounding('3ecc99b2-ad31-4f4c-943e-a626157957bc', no_exceptions_to_inhumane_treatment, deontological).
narrative_ontology:cs_reference_frame('3ecc99b2-ad31-4f4c-943e-a626157957bc', post_wwii_universal_human_rights).
narrative_ontology:cs_drift_state('3ecc99b2-ad31-4f4c-943e-a626157957bc', post_9_11_security_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('3ecc99b2-ad31-4f4c-943e-a626157957bc', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__absolute_prohibition, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, detainees).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, human_rights_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(humane_treatment_standard__absolute_prohibition, interrogators_and_security_personnel).
narrative_ontology:constraint_victim(humane_treatment_standard__absolute_prohibition, national_security_apparatus).
narrative_ontology:constraint_vindicates(humane_treatment_standard__absolute_prohibition, universal_human_dignity).
narrative_ontology:constraint_vindicates(humane_treatment_standard__absolute_prohibition, rule_of_law_in_armed_conflict).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals held in armed conflict, whose fundamental human dignity and right to humane treatment are protected by this standard, regardless of their status or alleged actions. They are physically constrained and rely entirely on the enforcement of this prohibition for their safety.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, detainees, beneficiary,
    powerless, immediate, trapped, global).

% Signatories to the Geneva Conventions, legally bound to uphold Common Article 3. They are responsible for implementing and enforcing the absolute prohibition within their military and security forces, even when facing internal or external pressures to deviate.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, state_parties_to_geneva_conventions, agenda_setter,
    institutional, generational, constrained, global).

% Organizations and individuals who monitor compliance with international humanitarian law, document violations, and advocate for the strict adherence to the absolute prohibition. Their work is empowered by the clarity and non-derogable nature of this standard.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, human_rights_advocates, beneficiary,
    organized, generational, mobile, global).

% Individuals directly involved in detention and interrogation. This reading of the standard imposes strict limits on their methods, requiring them to operate within a framework that absolutely forbids torture or degrading treatment, even when under pressure to extract information.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, interrogators_and_security_personnel, payer,
    moderate, biographical, constrained, national).

% The state agencies responsible for intelligence gathering and counter-terrorism. This reading of Common Article 3 constrains their operational methods, preventing the use of 'enhanced interrogation techniques' and requiring adherence to humane standards, which they may perceive as hindering their mission.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, national_security_apparatus, payer,
    institutional, generational, constrained, national).

% Judicial bodies that prosecute individuals for war crimes, including torture and inhumane treatment. This reading provides a clear, non-negotiable legal basis for their jurisdiction and judgments, reinforcing the universal nature of the prohibition.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, international_criminal_courts, agenda_setter,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal, non-derogable baseline for humane treatment in armed conflict, ensuring that all parties adhere to a common standard and preventing a race to the bottom in detainee treatment.
% TRANSFER_FUNCTION: Transfers the absolute right to humane treatment to detainees, and imposes an absolute duty on state parties and their agents to refrain from torture or degrading treatment, regardless of circumstances.
% ABSENT_VOICES: Those who advocate for 'ticking time bomb' exceptions or 'enhanced interrogation' as a necessary evil for national security are structurally excluded from the interpretive framework of absolute prohibition; they would argue for a more flexible, context-dependent application of the standard.
% DISAPPEARANCE_RATIONALE: If the absolute prohibition vanished, the legal and moral landscape of armed conflict would fundamentally shift. Detainees would lose a critical layer of protection, states would face immense pressure to adopt more coercive methods, and the international human rights framework would suffer a catastrophic blow, leading to a rapid degradation of treatment standards globally.
% FOUNDING_PROBLEM: The widespread atrocities and inhumane treatment of prisoners and civilians during World War II, which demonstrated the urgent need for universal, non-derogable standards for the protection of individuals in armed conflict.
% FOUNDING_PROBLEM_CORROBORATION: International human rights organizations, legal scholars, and the ongoing work of international criminal courts consistently corroborate that the problem of inhumane treatment in conflict remains live, and the standard's absolute nature is crucial for preventing its recurrence. This is attested by independent monitoring bodies and legal precedents, not just benefiting parties.
narrative_ontology:disappearance_verdict(humane_treatment_standard__absolute_prohibition, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__absolute_prohibition, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__absolute_prohibition, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(humane_treatment_standard__absolute_prohibition, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__absolute_prohibition_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, ExtMetricName, E),
    domain_priors:suppression_score(humane_treatment_standard__absolute_prohibition, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(humane_treatment_standard__absolute_prohibition),
    narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(humane_treatment_standard__absolute_prohibition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as a Mountain because this reading asserts its non-derogable, universal nature, akin to a natural law of human dignity in conflict. Extractiveness is low (0.1) because the standard primarily protects, rather than extracts from, its targets. Suppression is low (0.2) as it's a legal norm, not a coercive mechanism, though some enforcement is required against states that might deviate. Theater ratio is low (0.05) as its function is direct protection, not performance. Accessibility collapse is high (0.9) because, within this reading, there are no legitimate alternatives to humane treatment. Resistance is low (0.1) because, while some states may violate it, the principle itself is widely accepted as fundamental.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of detainees and human rights advocates, this is a clear, protective Mountain. From the perspective of national security apparatuses and interrogators, it is a constraint that limits their operational flexibility, potentially seen as a 'cost' to their mission, though its absolute nature means there is no legitimate 'exit' from its demands.
 *
 * DIRECTIONALITY LOGIC:
 *   Detainees are full beneficiaries (d=0.0) as the constraint directly protects them. Human rights advocates are also beneficiaries (d=0.1) as the standard provides the legal basis for their work. State parties and international courts are agenda-setters (d=0.2-0.3) as they are responsible for its maintenance and enforcement. Interrogators and national security apparatuses are payers (d=0.8-0.9) as the constraint directly limits their methods and imposes costs on their operations.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mandatrophy by asserting the timeless and non-derogable nature of the standard. The founding problem (atrocities in WWII) remains live, and the absolute prohibition is seen as the only way to prevent its recurrence. The 'contested' status of the founding problem (Q5) acknowledges that some actors argue the problem has changed or that the standard is no longer fit for purpose, but this reading rejects such arguments as attempts to erode a fundamental principle, not as evidence of mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_norm,
    'Is the absolute prohibition of torture a genuine natural law of human dignity, or a constructed international legal norm that benefits identifiable agents?',
    'Philosophical and legal analysis of the grounding of human rights, and empirical observation of its universal acceptance and enforcement across diverse legal systems.',
    'If purely a constructed norm, its ''mountain'' classification is a false summit, and its persistence depends on active enforcement by beneficiaries, reclassifying it as a Tangled Rope. If a natural law, its persistence is inherent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_norm, conceptual, 'Ambiguity between inherent moral truth and legal construct.').

omega_variable(
    enforcement_capacity_vs_normative_force,
    'To what extent does the persistence of this absolute prohibition rely on the inherent normative force of the principle, versus the active enforcement capacity of international bodies and state parties?',
    'Analysis of compliance rates in the absence of direct enforcement, and the impact of judicial rulings and sanctions on state behavior.',
    'If persistence is primarily due to enforcement, its ''mountain'' classification is weaker, and it leans towards a Rope or Tangled Rope, depending on beneficiary structure. If normative force is primary, the Mountain classification is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_vs_normative_force, empirical, 'Reliance on inherent principle vs. active enforcement.').

omega_variable(
    reading_contest_impact,
    'How would the classification of this constraint change if a sibling reading (e.g., ''contextual_necessity'') gained dominance in international legal interpretation?',
    'Analysis of shifts in state practice, judicial precedent, and the discourse of international legal bodies following a hypothetical or actual shift in interpretive dominance.',
    'If ''contextual_necessity'' became dominant, this constraint would likely reclassify from Mountain to Tangled Rope or Snare, as it would permit asymmetric extraction (torture) under the guise of security, with identifiable victims and beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_impact, conceptual, 'Impact of alternative kernel readings on classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__absolute_prohibition, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t1949, humane_treatment_standard__absolute_prohibition, theater_ratio, 1949, 0.01).
narrative_ontology:measurement(huma_tr_t1970, humane_treatment_standard__absolute_prohibition, theater_ratio, 1970, 0.02).
narrative_ontology:measurement(huma_tr_t1990, humane_treatment_standard__absolute_prohibition, theater_ratio, 1990, 0.02).
narrative_ontology:measurement(huma_tr_t2001, humane_treatment_standard__absolute_prohibition, theater_ratio, 2001, 0.08).
narrative_ontology:measurement(huma_tr_t2010, humane_treatment_standard__absolute_prohibition, theater_ratio, 2010, 0.06).
narrative_ontology:measurement(huma_tr_t2024, humane_treatment_standard__absolute_prohibition, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(huma_be_t1949, humane_treatment_standard__absolute_prohibition, base_extractiveness, 1949, 0.05).
narrative_ontology:measurement(huma_be_t1970, humane_treatment_standard__absolute_prohibition, base_extractiveness, 1970, 0.08).
narrative_ontology:measurement(huma_be_t1990, humane_treatment_standard__absolute_prohibition, base_extractiveness, 1990, 0.07).
narrative_ontology:measurement(huma_be_t2001, humane_treatment_standard__absolute_prohibition, base_extractiveness, 2001, 0.15).
narrative_ontology:measurement(huma_be_t2010, humane_treatment_standard__absolute_prohibition, base_extractiveness, 2010, 0.12).
narrative_ontology:measurement(huma_be_t2024, humane_treatment_standard__absolute_prohibition, base_extractiveness, 2024, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t1949, humane_treatment_standard__absolute_prohibition, suppression_requirement, 1949, 0.1).
narrative_ontology:measurement(huma_su_t1970, humane_treatment_standard__absolute_prohibition, suppression_requirement, 1970, 0.15).
narrative_ontology:measurement(huma_su_t1990, humane_treatment_standard__absolute_prohibition, suppression_requirement, 1990, 0.12).
narrative_ontology:measurement(huma_su_t2001, humane_treatment_standard__absolute_prohibition, suppression_requirement, 2001, 0.25).
narrative_ontology:measurement(huma_su_t2010, humane_treatment_standard__absolute_prohibition, suppression_requirement, 2010, 0.2).
narrative_ontology:measurement(huma_su_t2024, humane_treatment_standard__absolute_prohibition, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__absolute_prohibition, enforcement_mechanism).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, humane_treatment_standard__contextual_necessity).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, humane_treatment_standard__proportionality_balancing).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, state_sovereignty_over_detention_policy).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'humane_treatment_standard' kernel (Common Article 3). Its absolute prohibition directly influences and is in tension with other readings that seek to introduce exceptions or balancing tests.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
