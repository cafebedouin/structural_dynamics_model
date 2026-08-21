% ============================================================================
% CONSTRAINT STORY: human_transcendence_pathway__jerusalem_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_human_transcendence_pathway__jerusalem_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: human_transcendence_pathway__jerusalem_reading
 *   human_readable: Jerusalem Pathway to Authentic Community
 *   domain: political_theology/social_ethics
 *
 * SUMMARY:
 *   This constraint describes the 'Jerusalem reading' of the human
 *   transcendence pathway, emphasizing the patient, participatory rebuilding
 *   of authentic human community under divine blessing. It integrates
 *   plurality into communion rather than uniformity, requiring sacrifice of
 *   efficiency for solidarity. The metrics reflect a low-extraction,
 *   low-suppression model, where costs are primarily those of voluntary
 *   participation and formation, rather than coercion. It is claimed as a
 *   Rope due to its net beneficial coordination function for the community as
 *   a whole.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_transcendence_pathway__jerusalem_reading, 0.25).
domain_priors:suppression_score(human_transcendence_pathway__jerusalem_reading, 0.15).
domain_priors:theater_ratio(human_transcendence_pathway__jerusalem_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_transcendence_pathway__jerusalem_reading, rope).
narrative_ontology:human_readable(human_transcendence_pathway__jerusalem_reading, "Jerusalem Pathway to Authentic Community").
narrative_ontology:topic_domain(human_transcendence_pathway__jerusalem_reading, "political_theology/social_ethics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(human_transcendence_pathway__jerusalem_reading, '56091022-7fb1-45d3-8e69-b98e87e4b060').
narrative_ontology:cs_kernel_codification('56091022-7fb1-45d3-8e69-b98e87e4b060', implicit).
narrative_ontology:cs_authority_grounding('56091022-7fb1-45d3-8e69-b98e87e4b060', lineage).
narrative_ontology:cs_interpretation_layer_present('56091022-7fb1-45d3-8e69-b98e87e4b060').
narrative_ontology:cs_reading_relation('56091022-7fb1-45d3-8e69-b98e87e4b060', human_transcendence_pathway__babel_reading, coexists_with).
narrative_ontology:cs_reading_relation('56091022-7fb1-45d3-8e69-b98e87e4b060', human_transcendence_pathway__technocratic_vs_incarnational_reading, coexists_with).
narrative_ontology:cs_axiom('56091022-7fb1-45d3-8e69-b98e87e4b060', foundational, plurality_in_communion_is_divine_will).
narrative_ontology:cs_axiom_status(plurality_in_communion_is_divine_will, holdable).
narrative_ontology:cs_axiom_grounding('56091022-7fb1-45d3-8e69-b98e87e4b060', plurality_in_communion_is_divine_will, theological).
narrative_ontology:cs_axiom('56091022-7fb1-45d3-8e69-b98e87e4b060', foundational, human_labor_cooperates_with_grace).
narrative_ontology:cs_axiom_status(human_labor_cooperates_with_grace, holdable).
narrative_ontology:cs_axiom_grounding('56091022-7fb1-45d3-8e69-b98e87e4b060', human_labor_cooperates_with_grace, theological).
narrative_ontology:cs_reference_frame('56091022-7fb1-45d3-8e69-b98e87e4b060', eschatological_communion).
narrative_ontology:cs_drift_state('56091022-7fb1-45d3-8e69-b98e87e4b060', contemporary_secular_age, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('56091022-7fb1-45d3-8e69-b98e87e4b060', '').
narrative_ontology:cs_kernel_id(human_transcendence_pathway__jerusalem_reading, human_transcendence_pathway).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, the_community_as_whole).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, marginalized_exiles).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(human_transcendence_pathway__jerusalem_reading, individual_members).
narrative_ontology:constraint_vindicates(human_transcendence_pathway__jerusalem_reading, subsidiarity_principle).
narrative_ontology:constraint_vindicates(human_transcendence_pathway__jerusalem_reading, solidarity_principle).
narrative_ontology:constraint_vindicates(human_transcendence_pathway__jerusalem_reading, common_good_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives the benefits of integrated diversity, mutual support, and shared purpose, leading to flourishing and resilience. Requires collective commitment and effort to maintain.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, the_community_as_whole, beneficiary,
    moderate, generational, constrained, global).

% Finds belonging, dignity, and a voice within the community, moving from exclusion to full participation. Their hope and identity become deeply intertwined with the community's success.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, marginalized_exiles, beneficiary,
    powerless, biographical, identity_locked, local).

% The ultimate source of blessing, guidance, and grace that enables the community's patient labor and integration of diversity. Its 'agenda' is understood through revelation and spiritual discernment.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, divine_providence, agenda_setter,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(human_transcendence_pathway__jerusalem_reading, divine_providence).

% Facilitates participatory processes, fosters solidarity, and guides the community's efforts in alignment with the transcendent vision. Bears the responsibility of formation and persuasion.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, community_leaders, agenda_setter,
    organized, biographical, constrained, local).

% Contributes patient labor, personal sacrifice, and commitment to the common good. Benefits from the community but also bears the costs of active participation and foregoing individualistic pursuits.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, individual_members, payer,
    moderate, biographical, mobile, local).

% Represents an alternative approach to community building focused on efficiency, technological optimization, and centralized control. Their methods are structurally incompatible with the participatory and divinely-blessed nature of this pathway, leading to their exclusion from its internal logic.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, technocratic_planners, excluded,
    powerful, biographical, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To integrate diverse individuals and groups into a cohesive, flourishing community through shared values, mutual responsibility, and patient effort, guided by a transcendent vision, fostering communion rather than uniformity.
% TRANSFER_FUNCTION: Transfers individual effort, resources, and commitment into collective well-being and spiritual growth, from individuals to the common good, facilitated by divine grace and shared responsibility.
% ABSENT_VOICES: Those who prioritize efficiency, technological solutions, or centralized control over participatory processes and spiritual formation would object, as their methods are antithetical to this pathway. They are excluded by the very premises of this approach to community.
% DISAPPEARANCE_RATIONALE: If this pathway vanished overnight, communities would likely fragment, succumb to purely secular or technocratic solutions, or fail to integrate diversity into communion, leading to social decay, coercive uniformity, or a loss of transcendent meaning in collective life.
% FOUNDING_PROBLEM: The fragmentation of human society, the alienation of individuals, and the pursuit of false forms of unity (e.g., through coercion or technological control) that fail to respect human dignity and diversity, leading to spiritual and social impoverishment.
% FOUNDING_PROBLEM_CORROBORATION: Theological traditions, social philosophers, and historical accounts of successful and failed community-building efforts, as well as contemporary sociological analyses of social fragmentation, corroborate the ongoing challenge of building authentic community.
narrative_ontology:disappearance_verdict(human_transcendence_pathway__jerusalem_reading, world_rearranges).
narrative_ontology:founding_problem_status(human_transcendence_pathway__jerusalem_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(human_transcendence_pathway__jerusalem_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(human_transcendence_pathway__jerusalem_reading, 'none', 1).
narrative_ontology:epsilon_provenance(human_transcendence_pathway__jerusalem_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_transcendence_pathway__jerusalem_reading_tests).
:- end_tests(human_transcendence_pathway__jerusalem_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the 'costs' are primarily voluntary contributions and the 'sacrifice of efficiency' for solidarity, rather than coercive extraction. Suppression is low (0.15) as the pathway relies on persuasion, formation, and divine grace, not active enforcement against dissent. Theater ratio is low (0.10) because the emphasis is on authentic, patient rebuilding, not performative maintenance. The gradual increase in extractiveness and suppression over the long interval reflects the ongoing effort required to maintain such a community against internal and external pressures.
 *
 * PERSPECTIVAL GAP:
 *   While this reading presents a path of net benefit, the 'sacrifice of efficiency' might be perceived as a higher cost by those accustomed to more utilitarian or individualistic approaches. However, from the perspective of this reading, these costs are integral to achieving authentic communion and are not considered 'extraction' in a negative sense.
 *
 * DIRECTIONALITY LOGIC:
 *   The community as a whole and marginalized exiles are beneficiaries, receiving the fruits of communion and belonging. Divine Providence and community leaders act as agenda-setters, guiding the process. Individual members are payers, contributing labor and sacrifice. Technocratic planners are excluded, as their methods are antithetical to this pathway's core tenets.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_blessing_empirical_status,
    'How is ''divine blessing'' empirically manifested or verified in the process of community building, and what is its causal role?',
    'Theological and philosophical inquiry into the nature of grace and human agency, combined with qualitative sociological studies of communities that explicitly invoke such blessings.',
    'If ''divine blessing'' is interpreted as purely metaphorical or non-causal, the constraint''s reliance on non-coercive formation might be re-evaluated as less robust, potentially increasing perceived extractiveness or suppression from a secular perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_blessing_empirical_status, conceptual, 'Ambiguity regarding the empirical status and causal efficacy of divine blessing in community formation.').

omega_variable(
    sacrifice_of_efficiency_as_extraction,
    'At what point does the ''sacrifice of efficiency for solidarity'' transition from a voluntary cost of coordination to an extractive burden on participants?',
    'Empirical studies of participant satisfaction, exit rates, and perceived burden, alongside a clear definition of ''efficiency'' within this theological framework.',
    'If the sacrifice is perceived as an undue burden by a significant portion of participants, the constraint''s extractiveness might be re-evaluated upward, potentially shifting its classification towards a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacrifice_of_efficiency_as_extraction, empirical, 'Boundary between voluntary sacrifice and involuntary extraction in community building.').

omega_variable(
    plurality_vs_uniformity_boundary,
    'How is the ''integration of plurality into communion'' distinguished from a subtle pressure towards uniformity or assimilation, particularly for marginalized groups?',
    'Qualitative sociological research focusing on the lived experiences of diverse members, particularly marginalized exiles, assessing the degree of genuine voice, autonomy, and cultural preservation within the ''communion''.',
    'If the ''communion'' is found to subtly suppress genuine plurality, the constraint''s suppression metric would increase, and the classification might shift towards a Tangled Rope or even Snare for specific groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plurality_vs_uniformity_boundary, empirical, 'Distinction between authentic integration and coercive assimilation of diverse elements.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_transcendence_pathway__jerusalem_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, human_transcendence_pathway__jerusalem_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(huma_tr_t20, human_transcendence_pathway__jerusalem_reading, theater_ratio, 20, 0.06).
narrative_ontology:measurement(huma_tr_t40, human_transcendence_pathway__jerusalem_reading, theater_ratio, 40, 0.07).
narrative_ontology:measurement(huma_tr_t60, human_transcendence_pathway__jerusalem_reading, theater_ratio, 60, 0.08).
narrative_ontology:measurement(huma_tr_t80, human_transcendence_pathway__jerusalem_reading, theater_ratio, 80, 0.09).
narrative_ontology:measurement(huma_tr_t100, human_transcendence_pathway__jerusalem_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(huma_be_t20, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement(huma_be_t40, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 40, 0.21).
narrative_ontology:measurement(huma_be_t60, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 60, 0.23).
narrative_ontology:measurement(huma_be_t80, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 80, 0.24).
narrative_ontology:measurement(huma_be_t100, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 100, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(huma_su_t20, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 20, 0.08).
narrative_ontology:measurement(huma_su_t40, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 40, 0.1).
narrative_ontology:measurement(huma_su_t60, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 60, 0.12).
narrative_ontology:measurement(huma_su_t80, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 80, 0.14).
narrative_ontology:measurement(huma_su_t100, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 100, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_transcendence_pathway__jerusalem_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is the 'Jerusalem reading' of the 'human_transcendence_pathway' kernel, distinct from the 'babel_reading' and 'technocratic_vs_incarnational_reading' which represent alternative or opposing approaches to human flourishing and community.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
