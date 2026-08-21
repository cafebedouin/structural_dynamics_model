% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__state_centric_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: common_article_3_scope__state_centric_reading
 *   human_readable: Common Article 3 Scope (State-Centric Reading)
 *   domain: international_humanitarian_law/law_of_armed_conflict
 *
 * SUMMARY:
 *   This constraint represents the state-centric reading of Common Article 3
 *   (CA3) of the Geneva Conventions, which limits its application to internal
 *   armed conflicts that meet specific intensity and organization thresholds.
 *   This interpretation excludes low-level violence and law enforcement
 *   operations from IHL's scope, granting states greater discretion. This
 *   story is one reading of the 'common_article_3_scope' kernel, which is
 *   contested by more expansive human rights and customary law
 *   interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, 0.65).
domain_priors:suppression_score(common_article_3_scope__state_centric_reading, 0.8).
domain_priors:theater_ratio(common_article_3_scope__state_centric_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__state_centric_reading, "Common Article 3 Scope (State-Centric Reading)").
narrative_ontology:topic_domain(common_article_3_scope__state_centric_reading, "international_humanitarian_law/law_of_armed_conflict").

domain_priors:requires_active_enforcement(common_article_3_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__state_centric_reading, '97926797-8636-4126-8ef5-ea0d32aa177e').
narrative_ontology:cs_kernel_codification('97926797-8636-4126-8ef5-ea0d32aa177e', fixed_text).
narrative_ontology:cs_authority_grounding('97926797-8636-4126-8ef5-ea0d32aa177e', lineage).
narrative_ontology:cs_interpretation_layer_present('97926797-8636-4126-8ef5-ea0d32aa177e').
narrative_ontology:cs_reading_relation('97926797-8636-4126-8ef5-ea0d32aa177e', common_article_3_scope__expansive_human_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('97926797-8636-4126-8ef5-ea0d32aa177e', common_article_3_scope__icrc_customary_reading, coexists_with).
narrative_ontology:cs_axiom('97926797-8636-4126-8ef5-ea0d32aa177e', foundational, state_sovereignty_primacy_in_internal_affairs).
narrative_ontology:cs_axiom_status(state_sovereignty_primacy_in_internal_affairs, holdable).
narrative_ontology:cs_axiom_grounding('97926797-8636-4126-8ef5-ea0d32aa177e', state_sovereignty_primacy_in_internal_affairs, conventional).
narrative_ontology:cs_axiom('97926797-8636-4126-8ef5-ea0d32aa177e', foundational, threshold_based_application_of_ihl).
narrative_ontology:cs_axiom_status(threshold_based_application_of_ihl, holdable).
narrative_ontology:cs_axiom_grounding('97926797-8636-4126-8ef5-ea0d32aa177e', threshold_based_application_of_ihl, conventional).
narrative_ontology:cs_reference_frame('97926797-8636-4126-8ef5-ea0d32aa177e', post_westphalian_state_sovereignty).
narrative_ontology:cs_drift_state('97926797-8636-4126-8ef5-ea0d32aa177e', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('97926797-8636-4126-8ef5-ea0d32aa177e', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__state_centric_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, state_military_forces).
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, national_governments).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, irregular_combatants).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, civilian_populations_in_low_intensity_conflicts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from a narrow interpretation of CA3, which grants them greater operational discretion in low-intensity conflicts and counter-terrorism operations, reducing legal constraints on their actions against non-state armed groups.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, state_military_forces, beneficiary,
    institutional, generational, arbitrage, global).

% Advocates for and enforces the state-centric reading, preserving sovereignty over internal security matters and limiting international humanitarian law's reach into domestic law enforcement or low-level violence. They define the thresholds for conflict classification.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, national_governments, agenda_setter,
    institutional, generational, constrained, global).

% Excluded from CA3 protections if their conflict does not meet state-defined intensity and organization thresholds. They face treatment as criminals or terrorists without prisoner-of-war status or minimum humanitarian safeguards, increasing their vulnerability.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, irregular_combatants, payer,
    powerless, immediate, trapped, local).

% Suffer from the lack of CA3 application in conflicts deemed below threshold, leading to reduced protection from violence, arbitrary detention, and lack of due process, as state forces operate with fewer legal constraints.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, civilian_populations_in_low_intensity_conflicts, payer,
    powerless, immediate, trapped, local).

% Analyze the legal implications and practical effects of the state-centric reading, often critiquing its restrictive nature and its impact on human rights and protection for vulnerable populations. Their work informs policy debates but does not directly enforce the constraint.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, international_humanitarian_law_scholars, observer,
    analytical, generational, analytical, global).

% Actively challenge the state-centric reading, arguing for a broader application of CA3 to protect all victims of armed violence. Their arguments are often marginalized in state-led discussions on conflict classification, but they exert pressure through public campaigns and legal challenges.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, human_rights_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for states to coordinate their understanding of when international humanitarian law applies to internal conflicts, aiming for legal clarity and predictability in military operations.
% TRANSFER_FUNCTION: Transfers operational discretion and reduced legal accountability to state military forces and national governments, by limiting the scope of international humanitarian law, at the cost of protections for irregular combatants and civilians in low-intensity conflicts.
% ABSENT_VOICES: Irregular combatants and human rights advocates are largely excluded from the formal processes of defining and applying CA3 thresholds; they would argue for universal application of minimum humanitarian standards regardless of conflict intensity.
% DISAPPEARANCE_RATIONALE: If the state-centric reading of CA3's scope vanished, states would face immediate pressure to apply IHL more broadly, increasing legal constraints on their internal security operations. Irregular combatants and civilians would gain greater protections, fundamentally altering the legal landscape of internal conflicts.
% FOUNDING_PROBLEM: The original intent of CA3 was to establish a minimum humanitarian floor for internal armed conflicts, where full Geneva Conventions might not apply, balancing state sovereignty with humanitarian concerns.
% FOUNDING_PROBLEM_CORROBORATION: National governments and their military forces attest that the problem of balancing sovereignty with IHL application is still live, requiring careful threshold definitions. Human rights organizations and some IHL scholars attest that the original problem has been distorted, and the current reading serves primarily to limit accountability rather than balance concerns; their corroboration comes from field reports and legal analysis.
narrative_ontology:disappearance_verdict(common_article_3_scope__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__state_centric_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(common_article_3_scope__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__state_centric_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_article_3_scope__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the significant legal and humanitarian costs borne by irregular combatants and civilians when CA3 is not applied. Suppression (0.80) is high because states actively resist broader interpretations and enforce their narrow view through legal arguments, diplomatic pressure, and military doctrine. The theater ratio (0.20) is relatively low, as the state-centric reading is a genuinely held legal position, though its application often serves to reduce accountability rather than purely coordinate. The metrics show a gradual increase in extractiveness and suppression over time, reflecting the hardening of state positions, particularly in response to asymmetric warfare and counter-terrorism.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of national governments, this reading provides necessary clarity and preserves state sovereignty, making it appear as a coordination mechanism. From the perspective of irregular combatants and human rights advocates, it functions as an extractive mechanism that denies fundamental protections under the guise of legal precision. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   National governments and their military forces are clear beneficiaries, gaining operational freedom and reduced accountability (low directionality). Irregular combatants and civilian populations in affected areas are the primary targets, bearing the costs of reduced protection (high directionality). IHL scholars and human rights advocates act as observers or excluded parties, attempting to shift the interpretation but lacking direct enforcement power.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_objectivity_ambiguity,
    'Are the ''intensity'' and ''organization'' thresholds for CA3 application objectively measurable, or are they subject to political interpretation by states?',
    'Independent, non-state-aligned expert panels developing universally accepted, quantifiable metrics for conflict classification, and their consistent application across diverse conflicts.',
    'If thresholds are subjective, the state-centric reading''s claimed coordination function is undermined by its potential for arbitrary application, increasing its effective extractiveness. If objective, the reading gains legitimacy as a genuine coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_objectivity_ambiguity, empirical, 'Ambiguity regarding the objectivity of conflict classification thresholds.').

omega_variable(
    mandate_drift_vs_necessity,
    'Has the state-centric reading drifted from its original mandate of balancing sovereignty and humanitarianism, to primarily serving state interests in avoiding accountability?',
    'Historical analysis of state justifications for applying (or not applying) CA3 over time, correlated with changes in conflict types and international legal norms, assessed by a neutral body.',
    'If drift is confirmed, the constraint''s claimed type as a ''rope'' or ''tangled_rope'' would shift towards ''snare'' due to the erosion of its coordination function and increased extraction. If necessity is confirmed, its coordination function remains robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_drift_vs_necessity, conceptual, 'Whether the state-centric reading''s function has drifted from its original intent.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal frameworks, diplomatic pressure) or internalized (states'' self-perception of sovereignty, military culture)?',
    'Analysis of state behavior in response to international legal challenges: if states consistently resist even when legal arguments are weak, internalized suppression is higher. If they adapt to strong legal arguments, structural suppression is dominant.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as states carry the resistance to broader application within their own legal and military cultures. If structural, external barriers are the primary mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in state adherence to narrow CA3 scope.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__state_centric_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1949, common_article_3_scope__state_centric_reading, theater_ratio, 1949, 0.1).
narrative_ontology:measurement(comm_tr_t1970, common_article_3_scope__state_centric_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(comm_tr_t1990, common_article_3_scope__state_centric_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(comm_tr_t2010, common_article_3_scope__state_centric_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(comm_tr_t2024, common_article_3_scope__state_centric_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(comm_be_t1949, common_article_3_scope__state_centric_reading, base_extractiveness, 1949, 0.5).
narrative_ontology:measurement(comm_be_t1970, common_article_3_scope__state_centric_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(comm_be_t1990, common_article_3_scope__state_centric_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(comm_be_t2010, common_article_3_scope__state_centric_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(comm_be_t2024, common_article_3_scope__state_centric_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1949, common_article_3_scope__state_centric_reading, suppression_requirement, 1949, 0.65).
narrative_ontology:measurement(comm_su_t1970, common_article_3_scope__state_centric_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(comm_su_t1990, common_article_3_scope__state_centric_reading, suppression_requirement, 1990, 0.75).
narrative_ontology:measurement(comm_su_t2010, common_article_3_scope__state_centric_reading, suppression_requirement, 2010, 0.78).
narrative_ontology:measurement(comm_su_t2024, common_article_3_scope__state_centric_reading, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, international_criminal_court_jurisdiction).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, rules_of_engagement_doctrine).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
