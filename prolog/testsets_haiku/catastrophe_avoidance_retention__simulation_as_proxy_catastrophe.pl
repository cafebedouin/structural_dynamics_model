% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__simulation_as_proxy_catastrophe
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: catastrophe_avoidance_retention__simulation_as_proxy_catastrophe
 *   human_readable: Simulation-as-Proxy-Catastrophe: High-Fidelity Drills as Competence Maintenance Mechanism
 *   domain: safety/organizational/institutional
 *
 * SUMMARY:
 *   In high-reliability industries (nuclear power, aviation, maritime,
 *   healthcare), organizations must maintain competence in responding to
 *   catastrophic, rare events. This constraint asserts that high-fidelity
 *   simulation constitutes genuine practice—that drills and simulators are
 *   functionally equivalent to actual catastrophic events for competence
 *   maintenance purposes. This reading claims that scheduled, controlled
 *   simulation can substitute for learning-from-catastrophe as a competence
 *   maintenance mechanism. Regulatory agencies, simulation operators, and
 *   large organizations with resources to build infrastructure benefit from
 *   and enforce this reading. Front-line operators, resource-constrained
 *   organizations, and accident investigators who encounter
 *   simulation-trained crews failing in genuinely novel catastrophes
 *   experience this reading as extractive—a costly infrastructure obligation
 *   justified by a claim about transfer that field evidence increasingly
 *   contests.
 *
 * KEY AGENTS:
 *   - regulatory_agencies: institutional agenda-setters mandating simulation standards; benefit by avoiding catastrophe-learning political cost
 *   - simulation_infrastructure_operators: institutional beneficiaries collecting fees for simulator access and maintenance; have financial interest in persistence
 *   - front_line_operators: moderate-power payers bearing training time and psychological cost; identity-locked into competence performance in simulations
 *   - resource_constrained_organizations: powerless victims; trapped by regulatory requirement, unable to afford infrastructure
 *   - organizations_avoiding_catastrophe_cost: institutional beneficiaries; shift from catastrophe-driven learning to managed, expensive infrastructure
 *   - accident_inquiry_commissions: institutional observers; enter only after failure, documenting simulation-to-reality gaps
 *   - catastrophe_learning_researchers: powerful analytical observers; produce evidence about transfer validity and brittleness under novelty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.58).
domain_priors:suppression_score(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.42).
domain_priors:theater_ratio(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, extractiveness, 0.58).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, accessibility_collapse, 0.67).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, resistance, 0.51).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, tangled_rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, "Simulation-as-Proxy-Catastrophe: High-Fidelity Drills as Competence Maintenance Mechanism").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, "safety/organizational/institutional").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, '37babb72-626b-421b-a20a-42ba3145975b').
narrative_ontology:cs_kernel_codification('37babb72-626b-421b-a20a-42ba3145975b', fixed_text).
narrative_ontology:cs_authority_grounding('37babb72-626b-421b-a20a-42ba3145975b', extraction).
narrative_ontology:cs_interpretation_layer_present('37babb72-626b-421b-a20a-42ba3145975b').
narrative_ontology:cs_reading_relation('37babb72-626b-421b-a20a-42ba3145975b', catastrophe_avoidance_retention__catastrophe_as_necessary_selector, forecloses).
narrative_ontology:cs_reading_relation('37babb72-626b-421b-a20a-42ba3145975b', catastrophe_avoidance_retention__hybrid_near_miss_learning, influences).
narrative_ontology:cs_axiom('37babb72-626b-421b-a20a-42ba3145975b', foundational, simulation_functional_equivalence).
narrative_ontology:cs_axiom_status(simulation_functional_equivalence, holdable).
narrative_ontology:cs_axiom_grounding('37babb72-626b-421b-a20a-42ba3145975b', simulation_functional_equivalence, empirically_contingent).
narrative_ontology:cs_axiom('37babb72-626b-421b-a20a-42ba3145975b', foundational, rare_event_competence_without_catastrophe).
narrative_ontology:cs_axiom_status(rare_event_competence_without_catastrophe, holdable).
narrative_ontology:cs_axiom_grounding('37babb72-626b-421b-a20a-42ba3145975b', rare_event_competence_without_catastrophe, deontological).
narrative_ontology:cs_reference_frame('37babb72-626b-421b-a20a-42ba3145975b', managed_simulation_infrastructure_framework).
narrative_ontology:cs_drift_state('37babb72-626b-421b-a20a-42ba3145975b', contemporary_post_fukushima_covid_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('37babb72-626b-421b-a20a-42ba3145975b', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, regulatory_agencies).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, simulation_infrastructure_operators).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, organizations_avoiding_catastrophe_cost).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, front_line_operators).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, field_personnel).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, resource_constrained_organizations).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.38 to 0.58 over the interval, indicating cumulative cost accumulation and increasing regulatory tightness around simulation standards. Theater ratio rises to 0.31 and plateaus, indicating that as simulation infrastructure matures, increasing share of effort goes toward maintaining the compliance appearance (audit documentation, certification paperwork, scenario design that satisfies regulatory checklist rather than maximum realism) rather than toward improving actual transfer to real catastrophes. Suppression requirement rises to 0.42 and plateaus, reflecting the steady enforcement needed to maintain the simulation-equivalence claim against accumulating field evidence of gaps—accident inquiries, near-miss investigations, and foreign incidents increasingly document that real catastrophes exceed simulation parameters. The plateau suggests suppression has stabilized at a level sufficient to maintain regulatory compliance despite contested transfer validity.
 *
 * PERSPECTIVAL GAP:
 *   From the regulatory and infrastructure-operator seats, this is rope: a genuine coordination solution to a real problem (competence maintenance without catastrophic learning). From the front-line and resource-constrained seats, this is tangled_rope or snare: the coordination benefit is to others (large organizations, regulators avoid cost), the extraction is direct (time, money, regulatory burden), and enforcement is required (you cannot opt out of simulation as competence proof). The engine should compute different types across these seats, reflecting the structural asymmetry: beneficiaries see coordination, payers see enforced extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory agencies: d near 0.0 (full beneficiary—they set the standard, collect political benefit of appearing rigorous without catastrophe-learning cost, suffer no direct cost). Simulation operators: d near 0.2 (beneficiary—they collect fees, though they bear some infrastructure cost). Front-line operators: d near 0.8 (target—they pay time and psychological cost, are identity-locked so exit is existential threat, constrained by professional identity). Resource-constrained organizations: d near 1.0 (full target—they are trapped by regulatory requirement, cannot afford infrastructure, cannot exit or voice objection). Accident investigators: d at 0.5 (symmetric—they benefit from competence-maintenance goal, but are harmed by the constraint's suppression of learning-from-catastrophe alternative). No directionality overrides needed; the structural derivation chain captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is NOT mandatrophic: it has a live founding problem (competence maintenance in catastrophe-prone systems) and a contested but present functioning (simulation does maintain some competence for anticipated failure modes). However, the constraint exhibits strong signs of extraction layering onto genuine coordination: the founding problem (maintain competence) is solved PARTIALLY (simulation trains for anticipated modes), but the constraint's persistence depends increasingly on suppressing alternative competence-maintenance mechanisms (learning from near-misses, indigenous knowledge, catastrophe-driven learning). The rising theater ratio indicates that as the infrastructure matures, maintenance effort increasingly goes toward regulatory compliance theater rather than transfer-validity improvement. This is the signature of tangled_rope: genuine coordination (competence maintenance) + asymmetric extraction (shifting cost to payers while beneficiaries collect both safety benefit and infrastructure rents).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_transfer_validity,
    'Do competencies trained in high-fidelity simulation transfer reliably to real catastrophic events, especially when the real event exceeds simulation parameters (novel failure mode, coupled failures, unprecedented scale)?',
    'Meta-analysis of accident inquiries comparing pre-catastrophe simulation records with post-catastrophe performance; studies of crews trained in simulation vs. alternative modalities when both face genuine novel catastrophes; longitudinal competence assessment in organizations experiencing rare-event incidents.',
    'If transfer is high and robust to novelty, the constraint''s core claim is validated and it functions as rope (genuine competence coordination). If transfer is partial or brittle under novelty, the constraint is tangled_rope or snare—simulation maintains competence for anticipated modes only, at cost, while suppressing alternative learning mechanisms (catastrophe-driven, near-miss distributed learning) that might provide robustness to novelty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_transfer_validity, empirical, 'Does simulation-trained competence transfer reliably to real, novel catastrophes?').

omega_variable(
    catastrophe_learning_irreplaceability,
    'Is there a selection-pressure component to catastrophe-driven learning (organizational trauma, mortality salience, absolute stakes) that simulation cannot replicate, even with highest fidelity?',
    'Comparative study of organizational learning mechanisms post-catastrophe vs. post-simulation; analysis of institutional memory persistence and behavioral change following actual vs. simulated crises; psychophysiological research on stress-driven learning vs. volitional training.',
    'If catastrophe-learning pressure is irreplaceable, simulation is an incomplete substitute and the constraint suppresses a necessary learning mechanism—pure extraction justifying itself through a false equivalence claim. If pressure is replicable (or unnecessary for competence maintenance), then simulation''s substitute function is valid.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(catastrophe_learning_irreplaceability, conceptual, 'Whether catastrophic pressure contains irreplaceable learning mechanisms simulation cannot substitute for.').

omega_variable(
    resource_constraint_exclusion,
    'Are small, resource-constrained organizations systematically excluded from competence certification, or do alternative competence-maintenance pathways remain accessible?',
    'Audit of regulatory competence-proof standards: are high-fidelity simulation and expensive infrastructure the ONLY documented path, or do near-miss learning, tabletop scenarios, indigenous knowledge, and low-cost drills remain valid alternatives? Longitudinal study of competence-maintenance costs across organization sizes.',
    'If small organizations are trapped (simulation is mandatory), the constraint is snare-extractive. If alternatives exist, it is rope with equity externalities. If alternatives are nominally permitted but practically disadvantaged (audit bias toward infrastructure evidence), it is tangled_rope with suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resource_constraint_exclusion, empirical, 'Whether simulation is the only competence-proof pathway or alternatives remain accessible.').

omega_variable(
    identity_lock_internalization,
    'Do front-line operators internalize the simulation-equivalence claim (incorporating it into professional identity, accepting simulation performance as competence proof), or do they maintain skepticism and independence in competence self-assessment?',
    'Qualitative interviews with operators before/after high-profile accidents that contradicted simulation training; ethnographic study of operator culture and competence narratives; analysis of accident inquiry testimony from certified operators who failed in novel catastrophes.',
    'If internalized, the constraint''s suppression is higher than measured—operators carry the simulation-equivalence assumption into real decisions, creating brittleness. If skepticism persists, operators may maintain corrective learning despite the constraint. If post-accident interviews show identity shock (''I thought I was competent because I passed simulation''), the identity-lock mechanism is partially destructive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_internalization, empirical, 'Whether front-line operators internalize the simulation-equivalence claim into their competence identity.').

omega_variable(
    regulatory_motivation_asymmetry,
    'Do regulatory agencies mandate high-fidelity simulation primarily because it best maintains competence, or primarily because it provides documented, auditable compliance evidence while avoiding the political cost of learning-from-catastrophe regimes?',
    'Analysis of regulatory guidance and standard-setting processes: are standards justified on competence-maintenance grounds or on auditability/compliance grounds? Comparative study of countries/sectors with different catastrophe-learning acceptance (some accept learning-from-catastrophe as valid competence proof) vs. simulation-only regimes.',
    'If motivation is competence-maintenance, the constraint is rooted in genuine coordination. If motivation is political risk-avoidance and auditability, the constraint is extractive justification theater—it looks rigorous while shifting cost to operators and small organizations, and suppressing evidence that alternative mechanisms (catastrophe learning, near-miss analysis) might be competence-superior.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_motivation_asymmetry, empirical, 'Whether regulatory mandate for simulation is competence-driven or risk-avoidance-driven.').

omega_variable(
    kernel_reading_contest,
    'This constraint instantiates one of three competing readings of the kernel catastrophe_avoidance_retention. Which reading is structurally true: simulation IS functionally equivalent to catastrophe (this reading), OR catastrophe provides irreplaceable selection pressure (catastrophe_as_necessary_selector), OR neither alone suffices and competence requires hybrid learning (hybrid_near_miss_learning)?',
    'The empirical omegas above (simulation_transfer_validity, catastrophe_learning_irreplaceability) directly resolve this. If simulation transfers reliably and catastrophe pressure is replicable, this reading''s core premise holds. If simulation has systematic gaps to novelty and catastrophe pressure is irreplaceable, catastrophe_as_necessary_selector''s core premise holds. If both simulation and catastrophe have complementary functions (simulation for anticipated modes, catastrophe for novel scenarios), hybrid_near_miss_learning''s core premise holds.',
    'This is the master omega: it determines which constraint reading is structurally sound. The three readings coexist as live positions across different parties; none forecloses the others within a single framework (all three could be true for different competence domains or organization types). However, a single organization''s competence-maintenance strategy can hold only ONE reading as operative. The contest is jurisdictional: which reading governs regulatory standard-setting, organizational investment, and competence proof?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which kernel reading is structurally true: simulation-equivalence, catastrophe-necessity, or hybrid-learning?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 0, 0.18).
narrative_ontology:measurement(cata_tr_t5, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 5, 0.2).
narrative_ontology:measurement(cata_tr_t10, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 10, 0.24).
narrative_ontology:measurement(cata_tr_t15, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 15, 0.27).
narrative_ontology:measurement(cata_tr_t20, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 20, 0.3).
narrative_ontology:measurement(cata_tr_t25, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 25, 0.31).
narrative_ontology:measurement(cata_tr_t30, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 30, 0.31).
narrative_ontology:measurement(cata_tr_t35, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 35, 0.31).
narrative_ontology:measurement(cata_tr_t40, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 40, 0.31).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(cata_be_t5, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(cata_be_t10, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(cata_be_t15, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 15, 0.53).
narrative_ontology:measurement(cata_be_t20, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(cata_be_t25, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 25, 0.57).
narrative_ontology:measurement(cata_be_t30, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(cata_be_t35, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 35, 0.58).
narrative_ontology:measurement(cata_be_t40, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(cata_su_t5, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 5, 0.32).
narrative_ontology:measurement(cata_su_t10, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 10, 0.36).
narrative_ontology:measurement(cata_su_t15, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 15, 0.39).
narrative_ontology:measurement(cata_su_t20, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(cata_su_t25, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 25, 0.42).
narrative_ontology:measurement(cata_su_t30, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(cata_su_t35, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 35, 0.42).
narrative_ontology:measurement(cata_su_t40, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.12).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_as_necessary_selector).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, hybrid_near_miss_learning).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel catastrophe_avoidance_retention. All three readings address the same founding problem (competence maintenance in catastrophe-prone systems) but propose different mechanisms. simulation_as_proxy_catastrophe (this constraint) claims high-fidelity simulation is functionally equivalent to catastrophe for competence purposes, enabling infrastructure-based competence maintenance. catastrophe_as_necessary_selector claims only actual catastrophic events provide the selection pressure (chaos, mortality, organizational trauma) necessary for competence, making simulation a false substitute. hybrid_near_miss_learning claims neither simulation alone nor catastrophe alone suffices; competence requires integrated learning from near-misses, foreign incidents, and drills. The three readings have different ε profiles: this reading moderates extraction (simulation is managed, predictable infrastructure cost) while suppressing catastrophe-learning (high suppression_requirement); catastrophe_as_necessary_selector inverts the extraction profile (catastrophe-learning accepts episodic, massive cost but no suppression of learning mechanisms); hybrid_near_miss_learning distributes cost and learning across multiple mechanisms (lower extraction, lower suppression). The three are distinct constraints with distinct structural properties, not perspectives on a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
