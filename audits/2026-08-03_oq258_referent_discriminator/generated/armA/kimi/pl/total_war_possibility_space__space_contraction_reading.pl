% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__space_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__space_contraction_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: total_war_possibility_space__space_contraction_reading
 *   human_readable: Nuclear Contraction of Total War Strategic Possibility Space
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the space_contraction_reading of the
 *   total_war_possibility_space kernel. The reading holds that nuclear
 *   weapons did not merely deter or taboo total war, but removed it from the
 *   strategically thinkable — rendering it a categorical impossibility rather
 *   than a costly option. The constraint operates by collapsing the strategic
 *   planning space: general staffs cease war-gaming for great-power total
 *   war, mobilization doctrine disappears, and strategic studies reorients to
 *   sub-nuclear domains. The coordination function is the prevention of
 *   nuclear annihilation; the cost is borne by military institutions whose
 *   traditional planning function becomes obsolete. The claim is rope
 *   (structural coordination) but the metrics acknowledge mild extraction
 *   through institutional capture by the deterrence complex.
 *
 * KEY AGENTS:
 *   - great_power_general_staffs: Primary payer (institutional/identity_locked) — bears institutional atrophy and loss of doctrinal function
 *   - nuclear_deterrence_complex: Primary beneficiary (organized/mobile) — gains mandate and resources from managing the nuclear peace
 *   - nuclear_armed_populations: Net beneficiary (institutional/constrained) — avoids nuclear war
 *   - sub_nuclear_planners: Secondary beneficiary (organized/mobile) — inherits resources and attention
 *   - classical_realist_theorists: Excluded voice (moderate/constrained) — argues total war remains possible
 *   - strategic_studies_field: Analytical observer (organized/analytical) — documents the shift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__space_contraction_reading, 0.18).
domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, 0.12).
domain_priors:theater_ratio(total_war_possibility_space__space_contraction_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__space_contraction_reading, rope).
narrative_ontology:human_readable(total_war_possibility_space__space_contraction_reading, "Nuclear Contraction of Total War Strategic Possibility Space").
narrative_ontology:topic_domain(total_war_possibility_space__space_contraction_reading, "international_relations/strategic_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__space_contraction_reading, 'e472337a-1be8-4fa4-93b5-d3b155cfcb4b').
narrative_ontology:cs_kernel_codification('e472337a-1be8-4fa4-93b5-d3b155cfcb4b', implicit).
narrative_ontology:cs_authority_grounding('e472337a-1be8-4fa4-93b5-d3b155cfcb4b', distributed).
narrative_ontology:cs_reading_relation('e472337a-1be8-4fa4-93b5-d3b155cfcb4b', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('e472337a-1be8-4fa4-93b5-d3b155cfcb4b', total_war_possibility_space__nuclear_taboo_reading, coexists_with).
narrative_ontology:cs_axiom('e472337a-1be8-4fa4-93b5-d3b155cfcb4b', foundational, total_war_strategically_unthinkable).
narrative_ontology:cs_axiom_status(total_war_strategically_unthinkable, holdable).
narrative_ontology:cs_axiom_grounding('e472337a-1be8-4fa4-93b5-d3b155cfcb4b', total_war_strategically_unthinkable, empirically_contingent).
narrative_ontology:cs_axiom('e472337a-1be8-4fa4-93b5-d3b155cfcb4b', secondary, institutional_atrophy_inevitable).
narrative_ontology:cs_axiom_status(institutional_atrophy_inevitable, holdable).
narrative_ontology:cs_axiom_grounding('e472337a-1be8-4fa4-93b5-d3b155cfcb4b', institutional_atrophy_inevitable, empirically_contingent).
narrative_ontology:cs_reference_frame('e472337a-1be8-4fa4-93b5-d3b155cfcb4b', nuclear_strategic_unthinkability).
narrative_ontology:cs_drift_state('e472337a-1be8-4fa4-93b5-d3b155cfcb4b', post_cold_war_multipolarity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e472337a-1be8-4fa4-93b5-d3b155cfcb4b', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__space_contraction_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, nuclear_deterrence_complex).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, nuclear_armed_populations).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, sub_nuclear_planners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(total_war_possibility_space__space_contraction_reading, great_power_general_staffs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Military planning institutions responsible for preparing great powers for large-scale industrial war. Under this constraint, their traditional mobilization doctrine and war-gaming functions for peer conflict become operationally irrelevant; they are reoriented toward limited conflict, deterrence operations, or nuclear command-and-control functions that depart from their inherited institutional identity and professional formation.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, great_power_general_staffs, payer,
    institutional, generational, identity_locked, global).

% Comprises think tanks, arms control institutions, and academic strategic studies focused on nuclear strategy and arms control. Receives funding, personnel, and policy attention from governments to manage the strategic stability created by the contraction of total-war possibility space.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, nuclear_deterrence_complex, beneficiary,
    organized, generational, mobile, global).

% Populations of nuclear-armed states whose daily existence is organized around a great-power peace maintained by the strategic incoherence of total war under nuclear conditions; they do not choose this arrangement but are its primary intended beneficiaries.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, nuclear_armed_populations, beneficiary,
    institutional, civilizational, constrained, global).

% Military and strategic enterprises specializing in limited war, gray-zone conflict, and sub-nuclear deterrence. Absorb resources, personnel, and doctrinal attention that previously flowed to total-war planning institutions as strategic studies shifts domain.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, sub_nuclear_planners, beneficiary,
    organized, biographical, mobile, global).

% Strategic theorists who maintain that total war between great powers remains structurally possible and strategically relevant. Their research programs and policy access are marginalized in deterrence-era strategic studies curricula and funding priorities.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, classical_realist_theorists, excluded,
    moderate, generational, constrained, global).

% The academic discipline of strategic studies, which shifted its research core from total-war mobilization and grand strategy to deterrence theory, arms control, and sub-nuclear conflict. Documents and theorizes the contraction without directly enforcing or bearing its costs.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, strategic_studies_field, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents nuclear-armed great powers from engaging in total war by rendering such war strategically incoherent — solving the collective-action problem of mutual annihilation under conditions of assured destruction.
% TRANSFER_FUNCTION: Moves institutional resources, personnel, and doctrinal attention away from total-war planning apparatuses and toward deterrence management and sub-nuclear conflict enterprises; moves the risk of great-power thermonuclear war toward strategic irrelevance.
% ABSENT_VOICES: Classical realist strategists and total-war traditionalists within general staffs who argue that peer great-power war remains structurally possible and strategically necessary; they are institutionally displaced and marginalized in strategic studies funding and military education rather than incorporated into the planning consensus.
% DISAPPEARANCE_RATIONALE: If total war returned to strategic thinkability, general staffs would rebuild mobilization and war-gaming apparatuses, strategic studies would reallocate intellectual and financial capital to peer conflict, and the international system would reorganize around renewed great-power war planning.
% FOUNDING_PROBLEM: Industrialization and nuclearization made great-power total war existentially catastrophic; the arrangement was built to continue great-power rivalry without triggering mutual annihilation.
% FOUNDING_PROBLEM_CORROBORATION: The nuclear deterrence establishment attests the problem is live. Classical realist theorists and some military historians contest that the unthinkability frame is a constructed narrative rather than a solved problem, noting that great-power competition persists and that the absence of planning is institutional atrophy, not logical necessity. Corroboration from outside the benefiting parties is present but contested.
narrative_ontology:disappearance_verdict(total_war_possibility_space__space_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__space_contraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__space_contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_possibility_space__space_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__space_contraction_reading, 0.18, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__space_contraction_reading_tests).
:- end_tests(total_war_possibility_space__space_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the constraint's primary effect is coordination for mutual survival. Suppression is low (0.12) because total war is not coercively excluded but rendered strategically incoherent. Accessibility collapse is high (0.88) because the alternative (total war planning) has virtually disappeared from strategic thought. Theater is moderate-low (0.22) — some deterrence posturing is performative, but the institutional atrophy of total-war planning is genuine. Resistance is low (0.15) because the constraint is self-enforcing through strategic logic, though classical realists offer marginal dissent.
 *
 * PERSPECTIVAL GAP:
 *   From the nuclear deterrence complex seat, the constraint is a structural boundary preventing catastrophe; from the general staff seat, it is the dissolution of their core institutional function. The engine computes this divergence from identical structural data via directionality: beneficiaries face damped extraction, payers face amplified costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear deterrence complex and sub-nuclear planners are beneficiaries (low d) because the constraint generates their institutional mandate. General staffs are payers (high d) because they bear the atrophy cost. Populations are beneficiaries (low d) through survival. Classical realists, though not formal victims, face exclusion (high d).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy mislabeling by distinguishing institutional displacement from extraction. General staffs lose function not because rents are extracted from them, but because the strategic environment that warranted their function dissolved. If the constraint were a snare, we would expect a capturer receiving the extracted value; instead, the gains are diffuse (survival) or accrue to new institutional forms without a clear extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Which reading of the total war possibility space kernel correctly describes the current strategic environment — categorical unthinkability, deterrence equilibrium, or normative taboo?',
    'Comparative institutional analysis of planning documents and strategic curricula across nuclear and pre-nuclear eras, combined with survey of practitioner beliefs.',
    'If deterrence equilibrium or taboo readings are correct, the constraint''s type shifts from structural boundary to enforced or normative mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Uncertainty between competing kernel readings').

omega_variable(
    naturalness_of_unthinkability,
    'Is the unthinkability of total war under nuclear conditions a logical necessity of strategic rationality, or a constructed institutional narrative maintained by the deterrence complex?',
    'Historical counterfactual analysis and examination of planning documents from nuclear-armed states for residual total-war contingency planning.',
    'If constructed, the constraint is not a structural given but a maintained narrative, potentially reclassifying toward tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_unthinkability, conceptual, 'Natural law vs constructed narrative ambiguity').

omega_variable(
    institutional_atrophy_as_extraction,
    'Does the atrophy of total-war planning apparatus represent benign adaptation or resource capture by the deterrence and sub-nuclear complexes?',
    'Budgetary and personnel flow analysis from total-war to deterrence and sub-nuclear institutions over the interval.',
    'If capture is demonstrated, effective extraction is higher than the coordination story suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_atrophy_as_extraction, empirical, 'Whether institutional atrophy masks extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__space_contraction_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t0, total_war_possibility_space__space_contraction_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(tota_tr_t20, total_war_possibility_space__space_contraction_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(tota_tr_t40, total_war_possibility_space__space_contraction_reading, theater_ratio, 40, 0.14).
narrative_ontology:measurement(tota_tr_t60, total_war_possibility_space__space_contraction_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement(tota_tr_t80, total_war_possibility_space__space_contraction_reading, theater_ratio, 80, 0.22).

% Extraction over time
narrative_ontology:measurement(tota_be_t0, total_war_possibility_space__space_contraction_reading, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(tota_be_t20, total_war_possibility_space__space_contraction_reading, base_extractiveness, 20, 0.08).
narrative_ontology:measurement(tota_be_t40, total_war_possibility_space__space_contraction_reading, base_extractiveness, 40, 0.1).
narrative_ontology:measurement(tota_be_t60, total_war_possibility_space__space_contraction_reading, base_extractiveness, 60, 0.13).
narrative_ontology:measurement(tota_be_t80, total_war_possibility_space__space_contraction_reading, base_extractiveness, 80, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(total_war_possibility_space__space_contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, nuclear_taboo_reading).

% DUAL FORMULATION NOTE:
% This constraint is the space_contraction_reading of the total_war_possibility_space kernel, which decomposes into three structurally distinct readings: deterrence_equilibrium_reading (total war reachable but deterred), nuclear_taboo_reading (normative prohibition), and this reading (categorical unthinkability). Each reading instantiates a different constraint with different epsilon, stakeholders, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
