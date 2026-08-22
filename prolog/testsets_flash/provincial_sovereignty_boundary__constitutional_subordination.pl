% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__constitutional_subordination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_sovereignty_boundary__constitutional_subordination, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: provincial_sovereignty_boundary__constitutional_subordination
 *   human_readable: Constitutional Subordination of Provinces
 *   domain: political_economy/federalism/resource_governance
 *
 * SUMMARY:
 *   This constraint represents the 'constitutional subordination' reading of
 *   provincial sovereignty within a federal system, where provinces are
 *   considered creations of the federal constitution with no inherent
 *   sovereignty. This reading asserts that provincial exit requires federal
 *   consent and legitimizes federal authority over national policies like
 *   equalization and climate. It is presented as a 'mountain' due to its
 *   foundational constitutional nature, but with identifiable beneficiaries
 *   (federal government, national unity advocates) and victims (separatist
 *   provincial governments), triggering False Summit Mountain analysis. The
 *   metrics reflect a low but present extractiveness (federal over provincial
 *   autonomy) and high suppression (legal barriers to provincial
 *   self-determination).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__constitutional_subordination, 0.25).
domain_priors:suppression_score(provincial_sovereignty_boundary__constitutional_subordination, 0.7).
domain_priors:theater_ratio(provincial_sovereignty_boundary__constitutional_subordination, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, extractiveness, 0.25).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__constitutional_subordination, mountain).
narrative_ontology:human_readable(provincial_sovereignty_boundary__constitutional_subordination, "Constitutional Subordination of Provinces").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__constitutional_subordination, "political_economy/federalism/resource_governance").

domain_priors:emerges_naturally(provincial_sovereignty_boundary__constitutional_subordination).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__constitutional_subordination, 'fc6e5ebb-ee46-447c-b76d-6001ab5e9574').
narrative_ontology:cs_kernel_codification('fc6e5ebb-ee46-447c-b76d-6001ab5e9574', fixed_text).
narrative_ontology:cs_authority_grounding('fc6e5ebb-ee46-447c-b76d-6001ab5e9574', lineage).
narrative_ontology:cs_interpretation_layer_present('fc6e5ebb-ee46-447c-b76d-6001ab5e9574').
narrative_ontology:cs_reading_relation('fc6e5ebb-ee46-447c-b76d-6001ab5e9574', provincial_sovereignty_boundary__compact_federalism, forecloses).
narrative_ontology:cs_reading_relation('fc6e5ebb-ee46-447c-b76d-6001ab5e9574', provincial_sovereignty_boundary__resource_sovereignty_primacy, influences).
narrative_ontology:cs_axiom('fc6e5ebb-ee46-447c-b76d-6001ab5e9574', foundational, federal_paramountcy_is_foundational).
narrative_ontology:cs_axiom_status(federal_paramountcy_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('fc6e5ebb-ee46-447c-b76d-6001ab5e9574', federal_paramountcy_is_foundational, deontological).
narrative_ontology:cs_axiom('fc6e5ebb-ee46-447c-b76d-6001ab5e9574', foundational, provinces_are_creatures_of_constitution).
narrative_ontology:cs_axiom_status(provinces_are_creatures_of_constitution, holdable).
narrative_ontology:cs_axiom_grounding('fc6e5ebb-ee46-447c-b76d-6001ab5e9574', provinces_are_creatures_of_constitution, conventional).
narrative_ontology:cs_reference_frame('fc6e5ebb-ee46-447c-b76d-6001ab5e9574', constitutional_act_1867_framework).
narrative_ontology:cs_drift_state('fc6e5ebb-ee46-447c-b76d-6001ab5e9574', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('fc6e5ebb-ee46-447c-b76d-6001ab5e9574', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, federal_government).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, national_unity_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, provincial_governments_aligned).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, provincial_governments_separatist).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__constitutional_subordination, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__constitutional_subordination, federal_paramountcy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the constitution as establishing federal paramountcy, granting it ultimate authority over provincial actions, especially regarding secession and national policy areas like equalization and climate. Benefits from a stable, unified federation.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, federal_government, agenda_setter,
    institutional, generational, analytical, national).

% Seeks greater autonomy or outright secession, but is legally bound by the federal constitution. Bears the cost of being unable to unilaterally pursue its political agenda, facing federal vetoes and legal challenges. Identity-locked by the political mandate to represent a distinct national group.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, provincial_governments_separatist, payer,
    organized, generational, identity_locked, regional).

% Operates within the federal framework, benefiting from federal transfers (equalization) and the stability of the national system. Accepts federal authority in areas like climate policy, seeing it as legitimate national coordination.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, provincial_governments_aligned, beneficiary,
    organized, biographical, constrained, regional).

% Actively supports the constitutional subordination reading, viewing it as essential for national cohesion and the effective functioning of federal programs. Benefits from the perceived stability and indivisibility of the nation-state.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, national_unity_advocates, beneficiary,
    moderate, generational, mobile, national).

% Analyzes the legal and political implications of federal-provincial relations, particularly in cases of secession attempts. Their analysis is detached from direct participation but influences international perceptions of the constraint's legitimacy.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, international_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear hierarchy of legal authority, ensuring a unified national legal framework and preventing unilateral provincial actions that could destabilize the federation or undermine national policy objectives.
% TRANSFER_FUNCTION: Legitimizes the flow of authority and resources from provinces to the federal center in key policy areas, and prevents the transfer of territorial control or sovereign powers from the federal state to provinces.
% ABSENT_VOICES: Indigenous nations, whose inherent sovereignty predates the federal constitution, are often excluded from the framing of provincial-federal relations as a purely internal matter. They would argue for a nation-to-nation relationship that transcends both provincial and federal constitutional claims.
% DISAPPEARANCE_RATIONALE: If this constitutional principle vanished, the federal system would immediately fragment. Provinces would assert full sovereignty, potentially leading to multiple secessions, a collapse of national equalization programs, and a complete reordering of resource governance and climate policy. The nation-state as currently constituted would cease to exist.
% FOUNDING_PROBLEM: The problem of creating a unified, stable nation-state from disparate colonial entities, ensuring national cohesion and preventing internal fragmentation while allowing for regional governance.
% FOUNDING_PROBLEM_CORROBORATION: The federal government and national unity advocates attest the problem is live, citing ongoing separatist movements and the need for national policy coherence. While provincial governments aligned with federalism also acknowledge the need for stability, separatist provincial governments contest the federal framing of the founding problem, arguing for a different historical interpretation of confederation.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__constitutional_subordination, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__constitutional_subordination, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__constitutional_subordination, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(provincial_sovereignty_boundary__constitutional_subordination, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__constitutional_subordination, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__constitutional_subordination_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, ExtMetricName, E),
    domain_priors:suppression_score(provincial_sovereignty_boundary__constitutional_subordination, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(provincial_sovereignty_boundary__constitutional_subordination),
    narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(provincial_sovereignty_boundary__constitutional_subordination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.25) reflects that while provinces are constrained, the federal system also provides benefits (e.g., equalization payments, national stability). The high suppression (0.7) is due to the legal and political barriers to provincial secession or assertion of absolute sovereignty, which are actively maintained by federal institutions. The theater ratio is low (0.1) as the constitutional principle is genuinely operative, not merely performative. Accessibility collapse is high (0.85) because the constitutional framework severely limits alternatives to federal authority. Resistance is moderate (0.3) reflecting ongoing but largely unsuccessful provincial challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the federal government's perspective, this is a foundational, unchangeable principle (a mountain). From the perspective of separatist provincial governments, it is a highly extractive snare that suppresses their legitimate aspirations. The engine's classification will highlight this divergence, showing how a claimed mountain operates as a snare for those it governs.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government and national unity advocates are beneficiaries, as this reading solidifies their power and vision for the nation. Separatist provincial governments are payers, as their aspirations for greater autonomy or independence are directly curtailed. Aligned provincial governments are also beneficiaries, as they operate within and benefit from the stable federal framework. International observers are analytical, assessing the system from an external perspective.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the constitutional subordination of provinces a genuine natural law of federalism, or a constructed constraint that benefits identifiable agents (the federal center)?',
    'Comparative analysis of other federal systems and their constitutional evolution, examining whether similar structures emerge universally or are contingent on specific historical and political choices.',
    'If genuinely natural, its classification as a mountain would be robust. If constructed, the presence of beneficiaries would strongly support reclassification as a tangled_rope or snare, highlighting its extractive nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, conceptual, 'Ambiguity between natural constitutional principle and political construct.').

omega_variable(
    secession_legitimacy_ambiguity,
    'Does the constitutional requirement for federal consent to provincial exit genuinely reflect a foundational legal principle, or is it a political assertion designed to suppress legitimate self-determination?',
    'International legal precedent on self-determination, and the outcome of any future referenda or negotiations on secession. If a clear majority in a province votes to secede and international law supports their right, the ''consent'' requirement''s legitimacy would be severely undermined.',
    'If consent is deemed a political assertion, the suppression metric would be re-evaluated as more coercive, and the constraint''s classification for separatist provinces would shift more strongly towards snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(secession_legitimacy_ambiguity, preference, 'Ambiguity over the legitimacy of federal veto on secession.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__constitutional_subordination, 1867, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t1867, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 1867, 0.05).
narrative_ontology:measurement(prov_tr_t1920, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 1920, 0.08).
narrative_ontology:measurement(prov_tr_t1982, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 1982, 0.1).
narrative_ontology:measurement(prov_tr_t2024, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(prov_be_t1867, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 1867, 0.2).
narrative_ontology:measurement(prov_be_t1920, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 1920, 0.22).
narrative_ontology:measurement(prov_be_t1982, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 1982, 0.25).
narrative_ontology:measurement(prov_be_t2024, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t1867, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 1867, 0.6).
narrative_ontology:measurement(prov_su_t1920, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 1920, 0.65).
narrative_ontology:measurement(prov_su_t1982, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 1982, 0.7).
narrative_ontology:measurement(prov_su_t2024, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__constitutional_subordination, enforcement_mechanism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary__compact_federalism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary__resource_sovereignty_primacy).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, federal_equalization_payments).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, national_climate_policy_framework).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
