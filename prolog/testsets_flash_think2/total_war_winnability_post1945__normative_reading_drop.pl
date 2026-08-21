% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__normative_reading_drop
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__normative_reading_drop, []).

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
 *   constraint_id: total_war_winnability_post1945__normative_reading_drop
 *   human_readable: Normative Illegitimacy of Total War (Post-1945)
 *   domain: international_relations/strategic_studies/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint story analyzes the normative illegitimacy of total war
 *   after 1945, as established by Article 2(4) of the UN Charter and the
 *   development of international humanitarian law. It is a reading of the
 *   'total_war_winnability_post1945' kernel, focusing on the ideational and
 *   legal shift rather than structural (nuclear weapons) or cultural
 *   (strategic culture drift) explanations. The constraint is classified as a
 *   Rope, as it solves a genuine coordination problem (preventing global
 *   catastrophe) with identifiable beneficiaries (global civilian
 *   populations) and victims (revisionist powers whose options are
 *   constrained by international norms).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__normative_reading_drop, 0.25).
domain_priors:suppression_score(total_war_winnability_post1945__normative_reading_drop, 0.4).
domain_priors:theater_ratio(total_war_winnability_post1945__normative_reading_drop, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, extractiveness, 0.25).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__normative_reading_drop, rope).
narrative_ontology:human_readable(total_war_winnability_post1945__normative_reading_drop, "Normative Illegitimacy of Total War (Post-1945)").
narrative_ontology:topic_domain(total_war_winnability_post1945__normative_reading_drop, "international_relations/strategic_studies/commitment_system_analysis").

domain_priors:requires_active_enforcement(total_war_winnability_post1945__normative_reading_drop).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__normative_reading_drop, '9663c7a9-9cdd-4a46-a6f7-552043adebff').
narrative_ontology:cs_kernel_codification('9663c7a9-9cdd-4a46-a6f7-552043adebff', formalized).
narrative_ontology:cs_authority_grounding('9663c7a9-9cdd-4a46-a6f7-552043adebff', lineage).
narrative_ontology:cs_interpretation_layer_present('9663c7a9-9cdd-4a46-a6f7-552043adebff').
narrative_ontology:cs_reading_relation('9663c7a9-9cdd-4a46-a6f7-552043adebff', total_war_winnability_post1945__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('9663c7a9-9cdd-4a46-a6f7-552043adebff', total_war_winnability_post1945__strategic_culture_drift, coexists_with).
narrative_ontology:cs_axiom('9663c7a9-9cdd-4a46-a6f7-552043adebff', foundational, jus_ad_bellum_limits_total_war).
narrative_ontology:cs_axiom_status(jus_ad_bellum_limits_total_war, holdable).
narrative_ontology:cs_axiom_grounding('9663c7a9-9cdd-4a46-a6f7-552043adebff', jus_ad_bellum_limits_total_war, deontological).
narrative_ontology:cs_axiom('9663c7a9-9cdd-4a46-a6f7-552043adebff', foundational, jus_in_bello_prohibits_indiscriminate_violence).
narrative_ontology:cs_axiom_status(jus_in_bello_prohibits_indiscriminate_violence, holdable).
narrative_ontology:cs_axiom_grounding('9663c7a9-9cdd-4a46-a6f7-552043adebff', jus_in_bello_prohibits_indiscriminate_violence, conventional).
narrative_ontology:cs_reference_frame('9663c7a9-9cdd-4a46-a6f7-552043adebff', post_wwii_normative_order).
narrative_ontology:cs_drift_state('9663c7a9-9cdd-4a46-a6f7-552043adebff', contemporary_geopolitical_shifts, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('9663c7a9-9cdd-4a46-a6f7-552043adebff', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__normative_reading_drop, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, humanitarian_organizations).
narrative_ontology:constraint_victim(total_war_winnability_post1945__normative_reading_drop, revisionist_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the normative constraint against total war, which reduces the likelihood of widespread devastation and civilian casualties. They are trapped by the consequences of war if the constraint fails.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations, beneficiary,
    powerless, generational, trapped, global).

% Are constrained by international norms and laws (Article 2(4) of the UN Charter, humanitarian law) from pursuing total war. They bear the cost of diplomatic isolation, potential sanctions, and loss of legitimacy if they violate these norms. Their exit options are limited by the global normative order.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, revisionist_powers, payer,
    institutional, biographical, constrained, global).

% Acts as a primary enforcer and interpreter of international law, including the prohibition on the use of force and humanitarian law. Its legitimacy and authority are tied to upholding these norms.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, un_security_council, agenda_setter,
    institutional, biographical, analytical, global).

% Analyze, interpret, and contribute to the development of international law and norms, providing the intellectual framework for the constraint's legitimacy and application. They observe its effectiveness and challenges.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, international_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% Benefit from the normative framework that limits the scope and brutality of warfare, making their work of aid and protection more feasible. They also advocate for the strengthening and adherence to these norms.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, humanitarian_organizations, beneficiary,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_winnability_post1945__normative_reading_drop, diffuse).
narrative_ontology:fixing_cost_class(total_war_winnability_post1945__normative_reading_drop, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state behavior by establishing a shared understanding that total war is an illegitimate instrument of policy, thereby encouraging states to seek limited conflict or peaceful resolution.
% TRANSFER_FUNCTION: Transfers the 'right' to wage total war from individual states to a globally constrained normative space, imposing costs (diplomatic, economic, reputational) on those who violate this constraint.
% ABSENT_VOICES: Historical proponents of total war (e.g., theorists of unlimited warfare) and future revisionist actors who might seek to re-legitimize total war would object to these constraints, but are currently marginalized by the prevailing normative order.
% DISAPPEARANCE_RATIONALE: If the normative illegitimacy of total war vanished overnight, states would lose a critical moral and legal barrier against escalating conflicts. The risk of widespread, indiscriminate violence would increase dramatically, reorganizing international relations around a more brutal logic of power.
% FOUNDING_PROBLEM: The catastrophic human cost, economic devastation, and global instability caused by two World Wars, which demonstrated the unacceptability of total war as a policy instrument.
% FOUNDING_PROBLEM_CORROBORATION: The problem of preventing total war remains live, corroborated by ongoing regional conflicts, the existence of weapons of mass destruction, and continuous diplomatic efforts to uphold international law. This is attested by UN resolutions, international court rulings, and historical analyses from independent scholars.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__normative_reading_drop, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__normative_reading_drop, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__normative_reading_drop, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(total_war_winnability_post1945__normative_reading_drop, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__normative_reading_drop, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__normative_reading_drop_tests).
:- end_tests(total_war_winnability_post1945__normative_reading_drop_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint's extractiveness is low (0.25) because it primarily functions to coordinate states away from a mutually destructive outcome, rather than to extract resources. However, it is not zero, as it imposes real costs on states that might otherwise pursue total war. Suppression (0.4) is moderate, relying on diplomatic pressure, international condemnation, and the threat of sanctions rather than direct physical coercion. The theater ratio is low (0.1) because the normative framework is genuinely active and widely, though not universally, adhered to. Accessibility collapse (0.6) is moderate, as total war remains physically possible but is normatively delegitimized. Resistance (0.3) is present from states that challenge the international order but is generally contained.
 *
 * PERSPECTIVAL GAP:
 *   While the constraint is broadly beneficial for global stability, revisionist powers may perceive it as an extractive mechanism that limits their sovereignty and strategic freedom. The engine's per-seat classification will reflect this divergence, with beneficiaries experiencing it as a Rope and victims as a more extractive form of constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Global civilian populations and humanitarian organizations are beneficiaries, as the constraint directly reduces their exposure to the harms of total war. Revisionist powers are victims, as their strategic options are constrained by the normative framework, incurring costs for non-compliance. The UN Security Council acts as an agenda-setter, enforcing and interpreting these norms.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_mechanism_ambiguity,
    'Is the absence of total war primarily due to normative illegitimacy, structural constraints (e.g., nuclear deterrence), or shifts in strategic culture?',
    'Comparative historical analysis of state decision-making, counterfactual modeling of international crises, and detailed examination of diplomatic and military doctrines across different eras.',
    'If structural or cultural factors are dominant, this normative reading''s classification as a Rope might be too benign, as the coordination function would be secondary to other, more coercive or deterministic forces. If normative factors are primary, the Rope classification is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(causal_mechanism_ambiguity, conceptual, 'Ambiguity regarding the primary causal mechanism for the absence of total war.').

omega_variable(
    norm_internalization_vs_compliance,
    'To what extent have states internalized the normative illegitimacy of total war, versus merely complying due to external pressure (e.g., fear of sanctions, reputational costs)?',
    'Analysis of state rhetoric, military doctrine, and actual behavior in crises, particularly when external enforcement mechanisms are weak or absent. Examination of domestic political discourse regarding the use of force.',
    'If internalization is low and compliance is primarily external, the constraint''s effective suppression might be higher than measured, and its stability more precarious, potentially shifting its classification towards a Tangled Rope or Snare if the ''coordination'' is merely a cover for coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_internalization_vs_compliance, empirical, 'Degree of state internalization of total war norms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__normative_reading_drop, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(tota_tr_t1955, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1955, 0.1).
narrative_ontology:measurement(tota_tr_t1965, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1965, 0.1).
narrative_ontology:measurement(tota_tr_t1975, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1975, 0.1).
narrative_ontology:measurement(tota_tr_t1985, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1985, 0.1).
narrative_ontology:measurement(tota_tr_t1995, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(tota_tr_t2005, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(tota_tr_t2015, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(tota_tr_t2025, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1945, 0.2).
narrative_ontology:measurement(tota_be_t1955, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1955, 0.22).
narrative_ontology:measurement(tota_be_t1965, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1965, 0.23).
narrative_ontology:measurement(tota_be_t1975, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1975, 0.24).
narrative_ontology:measurement(tota_be_t1985, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1985, 0.24).
narrative_ontology:measurement(tota_be_t1995, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1995, 0.25).
narrative_ontology:measurement(tota_be_t2005, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 2005, 0.25).
narrative_ontology:measurement(tota_be_t2015, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 2015, 0.26).
narrative_ontology:measurement(tota_be_t2025, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 2025, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1945, 0.35).
narrative_ontology:measurement(tota_su_t1955, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1955, 0.37).
narrative_ontology:measurement(tota_su_t1965, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1965, 0.38).
narrative_ontology:measurement(tota_su_t1975, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1975, 0.39).
narrative_ontology:measurement(tota_su_t1985, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1985, 0.39).
narrative_ontology:measurement(tota_su_t1995, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1995, 0.4).
narrative_ontology:measurement(tota_su_t2005, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 2005, 0.4).
narrative_ontology:measurement(tota_su_t2015, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 2015, 0.41).
narrative_ontology:measurement(tota_su_t2025, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 2025, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__normative_reading_drop, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, international_criminal_court_jurisdiction).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, arms_control_treaties).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'total_war_winnability_post1945' kernel, focusing on the normative dimension. Sibling readings include 'structural_contraction_reading' (nuclear deterrence) and 'strategic_culture_drift' (ideational shifts).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
