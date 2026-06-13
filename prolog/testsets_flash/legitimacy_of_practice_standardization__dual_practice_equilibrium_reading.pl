% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
 *   human_readable: Dual Practice Equilibrium in Legitimacy Standardization
 *   domain: political_history/modernization_studies/institutional_change
 *
 * SUMMARY:
 *   This constraint describes a 'dual practice equilibrium' where state
 *   authority governs public and administrative domains (e.g., Gregorian
 *   calendar for official business, Western legal codes) while traditional
 *   authority retains legitimacy in private and ritual domains (e.g., lunar
 *   calendar for festivals, customary law for family matters). This reading
 *   posits a stable, long-term partitioning of legitimacy, with strategic
 *   compliance rather than deep internalization of a single, unified system.
 *   It is one reading of the broader kernel
 *   'legitimacy_of_practice_standardization', which explores how different
 *   societies manage the tension between traditional and modern practices.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.3).
domain_priors:suppression_score(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.4).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "Dual Practice Equilibrium in Legitimacy Standardization").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "political_history/modernization_studies/institutional_change").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 'b38e5ab1-2122-487b-8176-1e86729f866f').
narrative_ontology:cs_kernel_codification('b38e5ab1-2122-487b-8176-1e86729f866f', formalized).
narrative_ontology:cs_authority_grounding('b38e5ab1-2122-487b-8176-1e86729f866f', lineage).
narrative_ontology:cs_interpretation_layer_present('b38e5ab1-2122-487b-8176-1e86729f866f').
narrative_ontology:cs_reading_relation('b38e5ab1-2122-487b-8176-1e86729f866f', legitimacy_of_practice_standardization__endogenous_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('b38e5ab1-2122-487b-8176-1e86729f866f', legitimacy_of_practice_standardization__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('b38e5ab1-2122-487b-8176-1e86729f866f', foundational, functional_differentiation_of_legitimacy).
narrative_ontology:cs_axiom_status(functional_differentiation_of_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('b38e5ab1-2122-487b-8176-1e86729f866f', functional_differentiation_of_legitimacy, conventional).
narrative_ontology:cs_axiom('b38e5ab1-2122-487b-8176-1e86729f866f', foundational, cultural_pluralism_as_stable_state).
narrative_ontology:cs_axiom_status(cultural_pluralism_as_stable_state, holdable).
narrative_ontology:cs_axiom_grounding('b38e5ab1-2122-487b-8176-1e86729f866f', cultural_pluralism_as_stable_state, conventional).
narrative_ontology:cs_reference_frame('b38e5ab1-2122-487b-8176-1e86729f866f', post_colonial_dual_system).
narrative_ontology:cs_drift_state('b38e5ab1-2122-487b-8176-1e86729f866f', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b38e5ab1-2122-487b-8176-1e86729f866f', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, state_administrators).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_authorities).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, citizens_in_dual_systems).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, citizens_in_dual_systems).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, cultural_pluralism).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, functional_differentiation_of_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a clear, predictable framework for public administration, where state-mandated practices (e.g., Gregorian calendar for taxes) are accepted without deep cultural conflict. They enforce these norms in their domain but do not seek to displace traditional practices in private life.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, state_administrators, agenda_setter,
    institutional, generational, constrained, national).

% Maintain legitimacy and social cohesion by preserving traditional practices (e.g., lunar calendar for festivals, customary dress for rituals). They operate within their recognized domains, often with tacit state approval, and resist attempts to impose state norms on private/ritual life.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_authorities, agenda_setter,
    organized, generational, constrained, local).

% Navigate both state-mandated and traditional practices, benefiting from the stability of both systems. They strategically comply with each set of norms in its appropriate domain (e.g., Western suits for work, traditional attire for home). The cost is the cognitive load of code-switching and potential friction at domain boundaries.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, citizens_in_dual_systems, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, citizens_in_dual_systems, payer).

% Argue for a unified, rationalized system of practices across all domains, viewing dual systems as inefficient or a barrier to progress. Their calls for comprehensive standardization are largely unheeded in this equilibrium, as both state and traditional authorities find the dual system functional.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, modernization_advocates, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, non-conflicting coexistence between state-mandated and traditional practices by partitioning their domains of legitimacy, allowing different social functions to be served by different normative systems.
% TRANSFER_FUNCTION: Transfers social stability and reduced conflict to both state and traditional authorities, and to citizens, by avoiding direct confrontation over practice legitimacy. It transfers the burden of code-switching and potential ambiguity to citizens.
% ABSENT_VOICES: Modernization advocates and universalist reformers are largely excluded; they would argue for a single, rationalized system of practices and challenge the legitimacy of maintaining 'outdated' traditions in any domain. Their arguments are not integrated into the prevailing dual-practice framework.
% DISAPPEARANCE_RATIONALE: If this dual-practice equilibrium vanished, it would likely lead to increased conflict between state and traditional authorities, as each would attempt to assert universal legitimacy for their preferred practices. Citizens would face greater pressure to choose or reconcile conflicting norms, leading to social instability and potentially resistance movements.
% FOUNDING_PROBLEM: The challenge of integrating diverse traditional societies into modern nation-states without provoking widespread cultural resistance or civil unrest, particularly regarding daily practices like calendars, dress codes, and legal norms.
% FOUNDING_PROBLEM_CORROBORATION: Historians and anthropologists widely corroborate the historical challenge of cultural integration in post-colonial and modernizing states. Contemporary sociological studies and political analyses from outside the immediate state or traditional authorities continue to document the ongoing functionality of such dual systems in managing cultural diversity and preventing conflict.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading_tests).
:- end_tests(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.3) because the system aims for coexistence rather than total displacement, minimizing direct costs of non-compliance in private spheres. Suppression is moderate (0.4) as the state enforces its norms in public domains, but generally tolerates traditional practices elsewhere, avoiding overt coercion unless public order is threatened. Theater ratio is low (0.1) because both systems are genuinely functional within their respective domains; there's little performative maintenance of a non-functional norm. Accessibility collapse is moderate (0.6) as alternatives are not fully collapsed but are strictly partitioned by domain. Resistance is low (0.2) because the equilibrium itself is a mechanism to reduce conflict.
 *
 * PERSPECTIVAL GAP:
 *   State administrators and traditional authorities both experience this as a functional 'rope' that allows them to maintain their respective spheres of influence without constant conflict. Citizens, while benefiting from stability, may experience a low level of extraction due to the cognitive burden of navigating dual systems, making it a 'tangled rope' from their perspective, though the overall system is stable.
 *
 * DIRECTIONALITY LOGIC:
 *   State administrators and traditional authorities are beneficiaries, as the constraint allows them to maintain their authority in distinct, non-overlapping domains. Citizens are also beneficiaries of the reduced conflict, but bear some costs of code-switching, placing them closer to symmetric. Modernization advocates are excluded, as their vision of unified practice is not accommodated by this equilibrium.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a 'rope' (from the system perspective) with low extractiveness and suppression prevents mislabeling a functional, albeit complex, coordination mechanism as pure extraction. The 'live' status of the founding problem (managing cultural integration without conflict) further supports that the mandate has not atrophied; the dual equilibrium is an ongoing solution to an ongoing problem, not a vestigial structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stability_of_domain_partition,
    'Is the domain partition between state and traditional authority truly stable, or is it a temporary phase in a longer process of displacement or integration?',
    'Longitudinal ethnographic studies and legal analyses over multiple generations, observing shifts in practice adoption and legal recognition across domains.',
    'If the partition proves unstable, the constraint might reclassify towards ''endogenous_displacement_reading'' (if change is voluntary) or ''exogenous_override_reading'' (if state-driven), indicating a more dynamic and potentially extractive process than a stable equilibrium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stability_of_domain_partition, empirical, 'Assesses the long-term viability of the dual-practice equilibrium.').

omega_variable(
    internalization_vs_strategic_compliance,
    'To what extent is compliance with dual practices internalized by citizens, versus being a strategic adaptation to avoid conflict or gain benefits?',
    'Sociological surveys and psychological studies on identity formation and cultural adherence in dual-system contexts, distinguishing between genuine belief and instrumental adherence.',
    'If compliance is primarily strategic, the ''rope'' classification might shift towards ''tangled_rope'' for citizens, as the perceived benefits of coordination are lower and the burden of adaptation higher, even if the system remains stable overall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_vs_strategic_compliance, empirical, 'Distinguishes between genuine cultural adherence and instrumental adaptation to dual norms.').

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a genuine ''dual_practice_equilibrium_reading'' of the ''legitimacy_of_practice_standardization'' kernel, or is it better understood as a phase within the ''endogenous_displacement_reading'' or ''exogenous_override_reading''?',
    'Comparative historical analysis across multiple societies, identifying cases where dual systems persist for centuries versus those that transition to unified systems, and the mechanisms driving those transitions.',
    'If resolved towards a displacement reading, the classification would shift to reflect the underlying dynamic of change and potential extraction, rather than a stable coexistence. This would alter the interpretation of extractiveness and suppression over time.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity in which reading of the kernel best describes the observed reality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1950, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(legi_tr_t1970, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(legi_tr_t1990, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(legi_tr_t2020, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(legi_be_t1950, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 1950, 0.25).
narrative_ontology:measurement(legi_be_t1970, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 1970, 0.28).
narrative_ontology:measurement(legi_be_t1990, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(legi_be_t2020, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 2020, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1950, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 1950, 0.35).
narrative_ontology:measurement(legi_su_t1970, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 1970, 0.38).
narrative_ontology:measurement(legi_su_t1990, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(legi_su_t2020, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 2020, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
