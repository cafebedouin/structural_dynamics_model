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
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: Jerusalem Reading: Patient, Participatory Community Building
 *   domain: political_theology/social_ethics
 *
 * SUMMARY:
 *   This constraint describes the 'Jerusalem Reading' of the
 *   human_transcendence_pathway kernel, emphasizing authentic community built
 *   through patient, participatory labor under divine blessing. It integrates
 *   plurality into communion, valuing diversity as a resource rather than
 *   seeking uniformity. The constraint is framed as a 'Rope' due to its
 *   genuine coordination function and low extraction, though it demands
 *   significant personal sacrifice and commitment. It stands in contrast to
 *   readings that prioritize technological solutions or coercive unity.
 *
 * KEY AGENTS:
 *   - the_community_as_whole: Primary beneficiary (organized/constrained)
 *   - marginalized_members: Primary beneficiary (powerless/identity_locked)
 *   - divine_blessing: Agenda setter (institutional/analytical)
 *   - individual_members: Payer (moderate/constrained)
 *   - technocratic_planners: Excluded (powerful/mobile)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_transcendence_pathway__jerusalem_reading, 0.2).
domain_priors:suppression_score(human_transcendence_pathway__jerusalem_reading, 0.1).
domain_priors:theater_ratio(human_transcendence_pathway__jerusalem_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_transcendence_pathway__jerusalem_reading, rope).
narrative_ontology:human_readable(human_transcendence_pathway__jerusalem_reading, "Jerusalem Reading: Patient, Participatory Community Building").
narrative_ontology:topic_domain(human_transcendence_pathway__jerusalem_reading, "political_theology/social_ethics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(human_transcendence_pathway__jerusalem_reading, 'bb3bea8c-274c-4e30-afe5-ef077e148286').
narrative_ontology:cs_kernel_codification('bb3bea8c-274c-4e30-afe5-ef077e148286', formalized).
narrative_ontology:cs_authority_grounding('bb3bea8c-274c-4e30-afe5-ef077e148286', lineage).
narrative_ontology:cs_interpretation_layer_present('bb3bea8c-274c-4e30-afe5-ef077e148286').
narrative_ontology:cs_reading_relation('bb3bea8c-274c-4e30-afe5-ef077e148286', human_transcendence_pathway__technocratic_vs_incarnational_reading, coexists_with).
narrative_ontology:cs_reading_relation('bb3bea8c-274c-4e30-afe5-ef077e148286', human_transcendence_pathway__babel_reading, coexists_with).
narrative_ontology:cs_axiom('bb3bea8c-274c-4e30-afe5-ef077e148286', foundational, plurality_in_communion_is_divine_will).
narrative_ontology:cs_axiom_status(plurality_in_communion_is_divine_will, holdable).
narrative_ontology:cs_axiom_grounding('bb3bea8c-274c-4e30-afe5-ef077e148286', plurality_in_communion_is_divine_will, theological).
narrative_ontology:cs_axiom('bb3bea8c-274c-4e30-afe5-ef077e148286', foundational, patient_participatory_labor_is_path_to_flourishing).
narrative_ontology:cs_axiom_status(patient_participatory_labor_is_path_to_flourishing, holdable).
narrative_ontology:cs_axiom_grounding('bb3bea8c-274c-4e30-afe5-ef077e148286', patient_participatory_labor_is_path_to_flourishing, instrumental).
narrative_ontology:cs_reference_frame('bb3bea8c-274c-4e30-afe5-ef077e148286', divinely_blessed_communion_of_diversity).
narrative_ontology:cs_drift_state('bb3bea8c-274c-4e30-afe5-ef077e148286', contemporary_individualistic_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bb3bea8c-274c-4e30-afe5-ef077e148286', '').
narrative_ontology:cs_kernel_id(human_transcendence_pathway__jerusalem_reading, human_transcendence_pathway).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, the_community_as_whole).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, marginalized_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(human_transcendence_pathway__jerusalem_reading, individual_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the integration of diverse members into a cohesive, supportive whole, fostering solidarity and shared purpose. Requires active participation and sacrifice of individual efficiency for collective good.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, the_community_as_whole, beneficiary,
    organized, generational, constrained, local).

% Are actively sought out and integrated, finding dignity and belonging within the community. Their unique contributions are valued, transforming their vulnerability into a source of strength for the whole. Their identity is deeply tied to this communal belonging.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, marginalized_members, beneficiary,
    powerless, biographical, identity_locked, local).

% Provides the transcendent framework and grace that enables the community's patient labor and integration of plurality. It is the ultimate source of legitimacy and hope for the community's flourishing, understood as a gift rather than a human achievement.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, divine_blessing, agenda_setter,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(human_transcendence_pathway__jerusalem_reading, divine_blessing).

% Are called to patient, participatory labor, which involves personal sacrifice, humility, and a willingness to prioritize communal good over individual gain or efficiency. This 'cost' is understood as a path to deeper communion.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, individual_members, payer,
    moderate, biographical, constrained, local).

% Would prioritize efficiency, top-down control, and technological solutions for social problems, viewing patient, participatory labor as inefficient and divine blessing as irrelevant. Their methods are antithetical to this reading's core tenets.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, technocratic_planners, excluded,
    powerful, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates diverse individuals and groups into a unified, yet pluralistic, community through shared values, mutual respect, and a common transcendent vision, fostering solidarity and resilience.
% TRANSFER_FUNCTION: Transfers spiritual and social capital, mutual support, and a sense of belonging to all members, especially the marginalized, in exchange for their patient labor, participation, and sacrifice of individualistic pursuits.
% ABSENT_VOICES: Technocratic planners and those who prioritize efficiency and uniformity would object, arguing that this approach is slow, inefficient, and lacks scalable solutions. They are excluded by the very premise of patient, participatory, and divinely-blessed community building.
% DISAPPEARANCE_RATIONALE: If this pathway vanished, communities would likely fragment, prioritize individual gain or technocratic efficiency, and lose the capacity for integrating diversity into communion. The social fabric would weaken, and marginalized members would lose a vital source of support and dignity.
% FOUNDING_PROBLEM: The problem of human fragmentation, alienation, and the temptation to achieve unity through coercive uniformity or technological control, leading to loss of dignity and authentic communion.
% FOUNDING_PROBLEM_CORROBORATION: Theological traditions, social ethicists, and grassroots community organizers attest to the ongoing challenge of building authentic community in a fragmented world, corroborating the problem's live status from outside purely theological beneficiaries.
narrative_ontology:disappearance_verdict(human_transcendence_pathway__jerusalem_reading, world_rearranges).
narrative_ontology:founding_problem_status(human_transcendence_pathway__jerusalem_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(human_transcendence_pathway__jerusalem_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(human_transcendence_pathway__jerusalem_reading, 'none', 1).
narrative_ontology:epsilon_provenance(human_transcendence_pathway__jerusalem_reading, 0.2, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is low (0.2) because the 'cost' to individual members is framed as a voluntary sacrifice for a greater good (communion), rather than an imposed burden. Suppression is low (0.1) as participation is based on persuasion and formation, not coercion; alternatives are not actively suppressed, but rather seen as less desirable paths to human flourishing. Theater ratio is very low (0.05) as the emphasis is on genuine, lived practice rather than performative displays. The slight fluctuations in metrics reflect the organic, non-linear nature of community building.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'the_community_as_whole' and 'marginalized_members', this is a pure Rope, offering profound benefits. For 'individual_members', it might feel more like a Tangled Rope due to the demands of patient labor and sacrifice, though the ultimate benefit is shared. 'Technocratic_planners' would likely see it as an inefficient Piton, lacking clear metrics and scalable outcomes.
 *
 * DIRECTIONALITY LOGIC:
 *   'The_community_as_whole' and 'marginalized_members' are clear beneficiaries, receiving support and integration. 'Divine_blessing' is the ultimate agenda-setter, providing the framework. 'Individual_members' are payers, contributing labor and sacrifice. 'Technocratic_planners' are excluded, as their worldview is incompatible with this approach.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is designed to prevent mandatrophy by continuously renewing its mandate through active participation and responsiveness to the needs of its members, especially the marginalized. Its 'patient labor' aspect means it resists the temptation to declare problems 'solved' prematurely, thus avoiding the atrophy of its core function. The low theater ratio indicates that its purpose remains genuine and not merely performative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_blessing_natural_or_constructed,
    'Is ''divine_blessing'' a genuine, external force enabling community, or a constructed narrative that legitimizes specific forms of social organization?',
    'Theological inquiry, lived experience of faith communities, and philosophical analysis of the nature of grace and human agency.',
    'If genuinely external, the constraint''s ''Rope'' classification is robust. If primarily a constructed narrative, the constraint might lean towards a ''Tangled Rope'' or ''Snare'' if it is found to extract from those who internalize the narrative without receiving commensurate benefits.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_blessing_natural_or_constructed, conceptual, 'Ambiguity of divine agency in community building.').

omega_variable(
    sacrifice_vs_extraction_boundary,
    'At what point does the ''patient, participatory labor'' required from individual members cross the line from voluntary sacrifice for communion to an unacknowledged form of extraction?',
    'Qualitative sociological studies of member experience, analysis of power dynamics within the community, and comparison with secular models of voluntary association.',
    'If the ''sacrifice'' is found to be disproportionate or coerced, the constraint''s extractiveness would be re-evaluated upward, potentially shifting its classification towards ''Tangled Rope'' for individual members.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacrifice_vs_extraction_boundary, empirical, 'Distinction between voluntary sacrifice and hidden extraction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (external barriers) or internalized (cognitive patterns that persist after barrier removal)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_transcendence_pathway__jerusalem_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, human_transcendence_pathway__jerusalem_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(huma_tr_t20, human_transcendence_pathway__jerusalem_reading, theater_ratio, 20, 0.04).
narrative_ontology:measurement(huma_tr_t40, human_transcendence_pathway__jerusalem_reading, theater_ratio, 40, 0.03).
narrative_ontology:measurement(huma_tr_t60, human_transcendence_pathway__jerusalem_reading, theater_ratio, 60, 0.04).
narrative_ontology:measurement(huma_tr_t80, human_transcendence_pathway__jerusalem_reading, theater_ratio, 80, 0.05).
narrative_ontology:measurement(huma_tr_t100, human_transcendence_pathway__jerusalem_reading, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(huma_be_t20, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement(huma_be_t40, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(huma_be_t60, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 60, 0.17).
narrative_ontology:measurement(huma_be_t80, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 80, 0.19).
narrative_ontology:measurement(huma_be_t100, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 100, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(huma_su_t20, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 20, 0.08).
narrative_ontology:measurement(huma_su_t40, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 40, 0.07).
narrative_ontology:measurement(huma_su_t60, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 60, 0.09).
narrative_ontology:measurement(huma_su_t80, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 80, 0.1).
narrative_ontology:measurement(huma_su_t100, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 100, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_transcendence_pathway__jerusalem_reading, identity_coordination).
narrative_ontology:affects_constraint(human_transcendence_pathway__jerusalem_reading, human_transcendence_pathway__babel_reading).
narrative_ontology:affects_constraint(human_transcendence_pathway__jerusalem_reading, human_transcendence_pathway__technocratic_vs_incarnational_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'human_transcendence_pathway' kernel. This 'Jerusalem Reading' emphasizes patient, participatory community building under divine blessing, integrating plurality into communion. It contrasts with the 'Babel Reading' (collective human power through unified tech/linguistic systems) and the 'Technocratic vs. Incarnational Reading' (transcendence via tech optimization vs. divine grace in vulnerability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
