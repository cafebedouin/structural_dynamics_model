% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__conceptualization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_emergence_boundary__conceptualization_reading, []).

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
 *   constraint_id: digital_money_emergence_boundary__conceptualization_reading
 *   human_readable: Digital Money Emergence Boundary: Conceptualization Reading
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'conceptualization' reading of the digital
 *   money emergence boundary. It posits that digital money emerged when its
 *   theoretical underpinnings became clear, marked by advances in
 *   telecommunications and formal cryptographic methods (e.g., Chaum's work
 *   in 1985). This reading emphasizes intellectual history and the role of
 *   academic research in defining the field. It is claimed as a Mountain
 *   because the theoretical possibility, once established, is an unchangeable
 *   fact, though its *interpretation* as the 'emergence boundary' is
 *   contested.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__conceptualization_reading, 0.05).
domain_priors:suppression_score(digital_money_emergence_boundary__conceptualization_reading, 0.02).
domain_priors:theater_ratio(digital_money_emergence_boundary__conceptualization_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__conceptualization_reading, mountain).
narrative_ontology:human_readable(digital_money_emergence_boundary__conceptualization_reading, "Digital Money Emergence Boundary: Conceptualization Reading").
narrative_ontology:topic_domain(digital_money_emergence_boundary__conceptualization_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:emerges_naturally(digital_money_emergence_boundary__conceptualization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__conceptualization_reading, '6cdf87d6-f1ed-4e75-a984-b4797f0374c2').
narrative_ontology:cs_kernel_codification('6cdf87d6-f1ed-4e75-a984-b4797f0374c2', formalized).
narrative_ontology:cs_authority_grounding('6cdf87d6-f1ed-4e75-a984-b4797f0374c2', expertise).
narrative_ontology:cs_interpretation_layer_present('6cdf87d6-f1ed-4e75-a984-b4797f0374c2').
narrative_ontology:cs_reading_relation('6cdf87d6-f1ed-4e75-a984-b4797f0374c2', digital_money_emergence_boundary__infrastructure_reading, influences).
narrative_ontology:cs_reading_relation('6cdf87d6-f1ed-4e75-a984-b4797f0374c2', digital_money_emergence_boundary__consumer_holdings_reading, influences).
narrative_ontology:cs_axiom('6cdf87d6-f1ed-4e75-a984-b4797f0374c2', foundational, theoretical_possibility_precedes_actuality).
narrative_ontology:cs_axiom_status(theoretical_possibility_precedes_actuality, holdable).
narrative_ontology:cs_axiom_grounding('6cdf87d6-f1ed-4e75-a984-b4797f0374c2', theoretical_possibility_precedes_actuality, deontological).
narrative_ontology:cs_reference_frame('6cdf87d6-f1ed-4e75-a984-b4797f0374c2', chaumian_formalization_as_origin).
narrative_ontology:cs_drift_state('6cdf87d6-f1ed-4e75-a984-b4797f0374c2', contemporary_cryptocurrency_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('6cdf87d6-f1ed-4e75-a984-b4797f0374c2', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, academic_researchers).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, computer_scientists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from establishing priority claims and foundational theoretical contributions in the field of digital money. Their careers and reputations are built on these conceptual breakthroughs.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, academic_researchers, beneficiary,
    institutional, generational, mobile, global).

% Benefit from the recognition of their theoretical work (e.g., David Chaum's formalization) as the origin point for digital money, validating their research trajectory and influence.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, computer_scientists, beneficiary,
    institutional, generational, mobile, global).

% Analyze and interpret the historical origins of digital money, often engaging with the debate over its 'true' emergence point. Their work is to document and contextualize these conceptual milestones.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, financial_historians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared intellectual lineage and a common starting point for academic discourse and research in digital money, coordinating theoretical development.
% TRANSFER_FUNCTION: Transfers intellectual credit and foundational status to early conceptualizers and their institutions, influencing subsequent research agendas and funding.
% ABSENT_VOICES: Practitioners and policymakers focused on implementation might argue this conceptual boundary is too abstract to be meaningful for their work, but their voices are not central to the academic debate over origins.
% DISAPPEARANCE_RATIONALE: The historical facts of theoretical development (1960s telecommunications, 1985 Chaum) would remain, but the *interpretation* of these as the 'emergence boundary' would vanish, leading to a re-evaluation of intellectual priority and historical narratives.
% FOUNDING_PROBLEM: To define the theoretical possibility and formal properties of digital money, distinguishing it from mere electronic record-keeping.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing academic discourse and the continued citation of foundational papers by computer scientists and economists corroborate the enduring relevance of these conceptual problems, even as implementation evolves.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__conceptualization_reading, world_unchanged).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__conceptualization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__conceptualization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(digital_money_emergence_boundary__conceptualization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__conceptualization_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__conceptualization_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, ExtMetricName, E),
    domain_priors:suppression_score(digital_money_emergence_boundary__conceptualization_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(digital_money_emergence_boundary__conceptualization_reading),
    narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(digital_money_emergence_boundary__conceptualization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very low because this boundary primarily confers intellectual credit rather than direct financial gain. Suppression is minimal as the theoretical development was largely open academic inquiry. Accessibility collapse is high because, once the theoretical possibility is understood, the 'emergence' at this conceptual level is largely settled. Resistance is low as the debate is more about interpretation than active opposition to the facts.
 *
 * PERSPECTIVAL GAP:
 *   While the conceptual emergence is a 'mountain' of intellectual history, other readings (e.g., infrastructure or consumer holdings) would classify the emergence boundary differently, leading to different beneficiaries and potentially different constraint types for those later stages of development.
 *
 * DIRECTIONALITY LOGIC:
 *   Academic researchers and computer scientists are beneficiaries as this reading validates their foundational work and intellectual priority. Financial historians act as observers, analyzing the conceptual shifts without directly benefiting or being extracted from.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    emergence_definition_ambiguity,
    'Is ''emergence'' best defined by theoretical possibility, infrastructural capability, or consumer adoption?',
    'A consensus among financial historians and economists on a single, dominant definition of ''emergence'' for monetary phenomena.',
    'If a later definition (e.g., consumer holdings) becomes dominant, this conceptualization reading would be reclassified as a ''historical precursor'' rather than the ''emergence boundary'', shifting its significance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(emergence_definition_ambiguity, conceptual, 'Ambiguity in the definition of ''emergence'' for digital money.').

omega_variable(
    natural_vs_constructed_boundary,
    'Is the conceptualization boundary a natural consequence of intellectual progress, or a constructed narrative that benefits academic priority claims?',
    'Analysis of historical narratives to identify whether the emphasis on theoretical origins was actively promoted by the benefiting academic communities, or if it arose organically from the historical record.',
    'If found to be a constructed narrative, the ''emerges_naturally'' flag would be re-evaluated, potentially reclassifying this as a ''Tangled Rope'' that coordinates academic discourse while extracting priority claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_boundary, empirical, 'Whether the conceptual emergence boundary is a natural fact or a constructed narrative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__conceptualization_reading, 1960, 1985).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1960, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1960, 0.0).
narrative_ontology:measurement(digi_tr_t1970, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1970, 0.0).
narrative_ontology:measurement(digi_tr_t1985, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1985, 0.01).

% Extraction over time
narrative_ontology:measurement(digi_be_t1960, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1960, 0.01).
narrative_ontology:measurement(digi_be_t1970, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1970, 0.03).
narrative_ontology:measurement(digi_be_t1985, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1985, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1960, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1960, 0.01).
narrative_ontology:measurement(digi_su_t1970, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1970, 0.01).
narrative_ontology:measurement(digi_su_t1985, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1985, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
