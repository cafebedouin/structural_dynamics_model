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
 *   human_readable: Authentic Human Community (Jerusalem Reading)
 *   domain: political_theology/social_ethics
 *
 * SUMMARY:
 *   This constraint describes the 'Jerusalem Reading' of the
 *   'human_transcendence_pathway' kernel, emphasizing the patient,
 *   participatory rebuilding of authentic human community under divine
 *   blessing. It integrates plurality into communion, valuing diversity as a
 *   resource rather than seeking uniformity. This reading stands in contrast
 *   to technocratic or purely human-powered approaches to community building.
 *   The constraint is claimed as a Rope, reflecting its coordination function
 *   and low extraction, primarily relying on persuasion and formation rather
 *   than coercion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_transcendence_pathway__jerusalem_reading, 0.25).
domain_priors:suppression_score(human_transcendence_pathway__jerusalem_reading, 0.1).
domain_priors:theater_ratio(human_transcendence_pathway__jerusalem_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_transcendence_pathway__jerusalem_reading, rope).
narrative_ontology:human_readable(human_transcendence_pathway__jerusalem_reading, "Authentic Human Community (Jerusalem Reading)").
narrative_ontology:topic_domain(human_transcendence_pathway__jerusalem_reading, "political_theology/social_ethics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(human_transcendence_pathway__jerusalem_reading, '09f3f6c0-c606-4963-aa40-eb398608714c').
narrative_ontology:cs_kernel_codification('09f3f6c0-c606-4963-aa40-eb398608714c', implicit).
narrative_ontology:cs_authority_grounding('09f3f6c0-c606-4963-aa40-eb398608714c', lineage).
narrative_ontology:cs_interpretation_layer_present('09f3f6c0-c606-4963-aa40-eb398608714c').
narrative_ontology:cs_reading_relation('09f3f6c0-c606-4963-aa40-eb398608714c', human_transcendence_pathway__technocratic_vs_incarnational_reading, coexists_with).
narrative_ontology:cs_reading_relation('09f3f6c0-c606-4963-aa40-eb398608714c', human_transcendence_pathway__babel_reading, coexists_with).
narrative_ontology:cs_axiom('09f3f6c0-c606-4963-aa40-eb398608714c', foundational, plurality_in_communion_is_divinely_willed).
narrative_ontology:cs_axiom_status(plurality_in_communion_is_divinely_willed, holdable).
narrative_ontology:cs_axiom_grounding('09f3f6c0-c606-4963-aa40-eb398608714c', plurality_in_communion_is_divinely_willed, theological).
narrative_ontology:cs_axiom('09f3f6c0-c606-4963-aa40-eb398608714c', foundational, patient_participatory_labor_is_essential_to_authentic_community).
narrative_ontology:cs_axiom_status(patient_participatory_labor_is_essential_to_authentic_community, holdable).
narrative_ontology:cs_axiom_grounding('09f3f6c0-c606-4963-aa40-eb398608714c', patient_participatory_labor_is_essential_to_authentic_community, conventional).
narrative_ontology:cs_reference_frame('09f3f6c0-c606-4963-aa40-eb398608714c', divinely_blessed_participatory_communion).
narrative_ontology:cs_drift_state('09f3f6c0-c606-4963-aa40-eb398608714c', contemporary_technological_society, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('09f3f6c0-c606-4963-aa40-eb398608714c', '').
narrative_ontology:cs_kernel_id(human_transcendence_pathway__jerusalem_reading, human_transcendence_pathway).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, the_community_as_whole).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, marginalized_exiles).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the integration of diverse members into a cohesive, supportive whole, fostering solidarity and shared responsibility. Requires active participation and patience from all members.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, the_community_as_whole, beneficiary,
    organized, generational, constrained, local).

% Finds belonging and dignity within the community, having been previously excluded or displaced. Their unique perspectives are valued as resources for the community's richness, but their integration requires vulnerability and trust.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, marginalized_exiles, beneficiary,
    powerless, biographical, identity_locked, local).

% Provides the transcendent framework and grace that enables the community's patient labor and integration of plurality. Not an agent in the human sense, but the ultimate source of the constraint's legitimacy and efficacy.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, divine_blessing, agenda_setter,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(human_transcendence_pathway__jerusalem_reading, divine_blessing).

% Would prioritize efficiency, uniformity, and rapid technological solutions over patient, participatory labor and the integration of diverse, sometimes inefficient, human elements. Their methods are antithetical to this reading's core tenets.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, technocratic_planners, excluded,
    powerful, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates diverse individuals and groups into a unified, yet pluralistic, community through shared values, patient dialogue, and mutual responsibility, preventing fragmentation and alienation.
% TRANSFER_FUNCTION: Transfers spiritual and social capital, mutual support, and a sense of belonging among community members, especially from the more established to the marginalized, in exchange for their unique contributions and participation.
% ABSENT_VOICES: Those who prioritize efficiency, technological solutions, or centralized control over participatory processes and the embrace of human vulnerability would object. They are excluded by the very nature of the patient, relational, and divinely-oriented approach.
% DISAPPEARANCE_RATIONALE: If this pathway vanished, communities would likely revert to more uniform, less participatory, or more technologically-driven models, losing the richness of integrated plurality and the emphasis on patient, divinely-blessed labor. The social fabric would fray, and marginalized groups would lose a specific avenue for authentic integration.
% FOUNDING_PROBLEM: Humanity's tendency towards fragmentation, alienation, and the imposition of uniformity, often exacerbated by attempts to build community through purely human power or technological means, leading to a loss of genuine communion and dignity.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested by ongoing social fragmentation, the rise of isolating technologies, and historical failures of purely human-centered community building efforts, as documented by various theological and sociological analyses from outside the immediate beneficiaries.
narrative_ontology:disappearance_verdict(human_transcendence_pathway__jerusalem_reading, world_rearranges).
narrative_ontology:founding_problem_status(human_transcendence_pathway__jerusalem_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(human_transcendence_pathway__jerusalem_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   Extractiveness is low (0.25) because the constraint primarily involves voluntary participation, mutual sacrifice for the common good, and the embrace of vulnerability rather than direct extraction. Suppression is also low (0.1) as adherence is driven by shared conviction and divine grace, not active enforcement or coercion. Theater ratio is minimal (0.05) as the focus is on genuine, lived community building. Accessibility collapse is low (0.2) because alternative, less participatory or more technocratic paths to community always exist, but this reading posits them as less authentic. Resistance is low (0.15) because those who embrace this path do so willingly, though the demands of patient labor can be challenging.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the community members, this is a beneficial and enabling constraint (Rope). From the perspective of those who prioritize efficiency or technocratic solutions (excluded stakeholders), it might appear as an inefficient or even naive approach, but it does not actively extract from them.
 *
 * DIRECTIONALITY LOGIC:
 *   The community as a whole and marginalized exiles are clear beneficiaries, receiving support and integration. Divine blessing, while not an agent, is the ultimate source of the constraint's enabling power. Technocratic planners are excluded, as their methods are incompatible with this pathway, but they are not victims of extraction by this constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is inherently resistant to mandatrophy because its 'mandate' is a continuous process of patient, participatory labor and divine reliance, which is always 'live' as long as human community exists and seeks authentic communion. Its justification is not a problem to be 'solved' and then abandoned, but a way of being that must be perpetually cultivated. The low extractiveness and suppression also mean there's little incentive for it to become a 'theater' or 'snare' for rent-seeking.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_agency_empirical_status,
    'To what extent can ''divine blessing'' be empirically observed or distinguished from collective human effort and psychological phenomena?',
    'Theological and philosophical inquiry, combined with qualitative sociological studies of communities operating under this framework, focusing on emergent properties not reducible to human-only factors.',
    'If divine agency is deemed purely metaphorical or unobservable, the constraint''s grounding shifts from theological to purely conventional or instrumental, potentially altering its perceived legitimacy and the nature of its ''enabling'' function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_agency_empirical_status, conceptual, 'Ambiguity regarding the empirical status of divine intervention in community building.').

omega_variable(
    plurality_vs_uniformity_boundary,
    'At what point does the integration of plurality risk dissolving into a new form of uniformity, or conversely, fail to achieve genuine communion and remain fragmented?',
    'Ongoing sociological and theological assessment of community practices, focusing on decision-making processes, power dynamics, and the lived experience of diverse members over time.',
    'If the balance is lost, the constraint could drift towards a more extractive (imposing uniformity) or ineffective (failing to cohere) form, potentially reclassifying as a Tangled Rope or even a Piton if the ''communion'' becomes mere performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plurality_vs_uniformity_boundary, empirical, 'The dynamic tension between integrating diversity and maintaining cohesion without imposing uniformity.').

omega_variable(
    kernel_reading_comparison_babel,
    'How does the ''Jerusalem Reading'' structurally differ from the ''Babel Reading'' (collective human power through unified technological/linguistic systems)?',
    'Comparative analysis of the core axioms and their implications for community structure, authority, and the role of technology. The Babel reading emphasizes human self-sufficiency and technological unity, while Jerusalem emphasizes divine blessing, participatory labor, and integrated plurality.',
    'The Babel reading would likely show higher extractiveness (from those who resist technological uniformity) and suppression, and a different set of beneficiaries (those who control the unified systems). Its claimed type would likely be a Snare or Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_comparison_babel, conceptual, 'Structural differences between Jerusalem and Babel readings of human transcendence.').

omega_variable(
    kernel_reading_comparison_technocratic_incarnational,
    'How does the ''Jerusalem Reading'' structurally differ from the ''Technocratic vs. Incarnational Reading'' (transcendence through technological optimization vs. divine grace in vulnerability)?',
    'Comparative analysis of the core axioms, particularly regarding the source and nature of transcendence. The Technocratic reading would prioritize efficiency and elimination of limits, leading to different beneficiaries (e.g., those who control advanced technology) and higher extractiveness/suppression. The Incarnational aspect of that reading shares some common ground with Jerusalem''s emphasis on vulnerability and grace, but Jerusalem''s focus is on community building rather than individual transcendence.',
    'The Technocratic reading would likely be a Snare or Tangled Rope, with high extractiveness and suppression. The Incarnational aspect, if isolated, might be a Rope, but its community dimension is less explicit than Jerusalem''s.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_comparison_technocratic_incarnational, conceptual, 'Structural differences between Jerusalem and Technocratic/Incarnational readings of human transcendence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_transcendence_pathway__jerusalem_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(huma_be_t0, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(huma_be_t10, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(huma_be_t20, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 20, 0.23).
narrative_ontology:measurement(huma_be_t30, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 30, 0.24).
narrative_ontology:measurement(huma_be_t40, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 40, 0.25).
narrative_ontology:measurement(huma_be_t50, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 50, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(huma_su_t10, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 10, 0.09).
narrative_ontology:measurement(huma_su_t20, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 20, 0.1).
narrative_ontology:measurement(huma_su_t30, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 30, 0.1).
narrative_ontology:measurement(huma_su_t40, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 40, 0.1).
narrative_ontology:measurement(huma_su_t50, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 50, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_transcendence_pathway__jerusalem_reading, attachment_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
