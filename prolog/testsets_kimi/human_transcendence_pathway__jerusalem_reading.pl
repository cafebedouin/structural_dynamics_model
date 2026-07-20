% ============================================================================
% CONSTRAINT STORY: human_transcendence_pathway__jerusalem_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   human_readable: Jerusalem Reading: Participatory Communion in Plurality
 *   domain: theological/social
 *
 * SUMMARY:
 *   This constraint instantiates the Jerusalem reading of the
 *   human_transcendence_pathway kernel: authentic human community is rebuilt
 *   through patient, participatory labor under divine blessing, integrating
 *   plurality into communion rather than uniformity. It functions as a
 *   coordination mechanism within Catholic social doctrine and political
 *   theology, proposing that marginalized returnees and host communities
 *   achieve solidarity through shared sacrifice of efficiency. The reading is
 *   structurally opposed to the Babel reading (technological self-sufficiency
 *   without transcendence) and influences the technocratic-vs-incarnational
 *   dialectic by concretely instantiating the incarnational pole. It is
 *   claimed as rope because no structural victims are declared and coercion
 *   is minimal; the metrics are authored independently to describe a
 *   low-extraction, persuasion-based arrangement.
 *
 * KEY AGENTS:
 *   - marginalized_returnees: Primary beneficiary (powerless/constrained) â receive non-assimilative welcome
 *   - host_community: Primary beneficiary (moderate/constrained) â bears coordination costs of participatory solidarity
 *   - ecclesial_teachers: Agenda-setter (institutional/constrained) â teaches and sustains the doctrinal frame
 *   - technocratic_planners: Excluded voice (powerful/mobile) â advocates efficiency-driven alternatives absent from this conversation
 *   - communion_theologians: Observer (organized/analytical) â evaluates whether plurality is preserved in practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_transcendence_pathway__jerusalem_reading, 0.32).
domain_priors:suppression_score(human_transcendence_pathway__jerusalem_reading, 0.18).
domain_priors:theater_ratio(human_transcendence_pathway__jerusalem_reading, 0.13).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, theater_ratio, 0.13).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_transcendence_pathway__jerusalem_reading, rope).
narrative_ontology:human_readable(human_transcendence_pathway__jerusalem_reading, "Jerusalem Reading: Participatory Communion in Plurality").
narrative_ontology:topic_domain(human_transcendence_pathway__jerusalem_reading, "theological/social").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(human_transcendence_pathway__jerusalem_reading, '242958cc-dd84-472a-95be-7dd36650076e').
narrative_ontology:cs_kernel_codification('242958cc-dd84-472a-95be-7dd36650076e', fixed_text).
narrative_ontology:cs_authority_grounding('242958cc-dd84-472a-95be-7dd36650076e', lineage).
narrative_ontology:cs_interpretation_layer_present('242958cc-dd84-472a-95be-7dd36650076e').
narrative_ontology:cs_reading_relation('242958cc-dd84-472a-95be-7dd36650076e', human_transcendence_pathway__babel_reading, forecloses).
narrative_ontology:cs_reading_relation('242958cc-dd84-472a-95be-7dd36650076e', human_transcendence_pathway__technocratic_vs_incarnational_reading, influences).
narrative_ontology:cs_axiom('242958cc-dd84-472a-95be-7dd36650076e', foundational, communion_through_difference).
narrative_ontology:cs_axiom_status(communion_through_difference, holdable).
narrative_ontology:cs_axiom_grounding('242958cc-dd84-472a-95be-7dd36650076e', communion_through_difference, theological).
narrative_ontology:cs_axiom('242958cc-dd84-472a-95be-7dd36650076e', foundational, solidarity_over_efficiency).
narrative_ontology:cs_axiom_status(solidarity_over_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('242958cc-dd84-472a-95be-7dd36650076e', solidarity_over_efficiency, deontological).
narrative_ontology:cs_reference_frame('242958cc-dd84-472a-95be-7dd36650076e', jerusalem_communion_template).
narrative_ontology:cs_drift_state('242958cc-dd84-472a-95be-7dd36650076e', digital_globalization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('242958cc-dd84-472a-95be-7dd36650076e', '').
narrative_ontology:cs_kernel_id(human_transcendence_pathway__jerusalem_reading, human_transcendence_pathway).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, marginalized_returnees).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, host_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Those returning from exile or marginalization who are integrated into community through patient, participatory labor rather than assimilated into uniformity. They receive solidarity and membership but depend on the host community's willingness to bear the inefficiency of genuine welcome.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, marginalized_returnees, beneficiary,
    powerless, biographical, constrained, local).

% Established members who sacrifice administrative efficiency and centralized control to welcome returning exiles through distributed, face-to-face labor. They bear coordination costsâslower decisions, shared resources, emotional laborâbut gain authentic communion across difference.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, host_community, beneficiary,
    moderate, biographical, constrained, local).

% Bishops, pastors, and theologians who articulate and transmit the doctrine that community must be rebuilt through divine blessing and participatory solidarity rather than technocratic optimization. They shape liturgical and moral expectations but rely on persuasion and formation rather than direct coercion.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, ecclesial_teachers, agenda_setter,
    institutional, generational, constrained, global).

% Advocates of centralized, efficient systems for managing populations who would argue for unified technological or administrative solutions to social fragmentation. They are structurally absent from the theological conversation that defines this constraint, though their frameworks dominate broader culture.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, technocratic_planners, excluded,
    powerful, biographical, mobile, global).

% Scholars who analyze whether communities practicing this model actually preserve plurality or drift toward assimilation, and whether the sacrifice of efficiency is borne symmetrically or falls disproportionately on the host community.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, communion_theologians, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(human_transcendence_pathway__jerusalem_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reintegrates marginalized members into community without destroying cultural, social, or spiritual plurality, solving the collective action problem of solidarity across difference by grounding mutual responsibility in divine blessing and shared labor rather than in contract or administrative fiat.
% TRANSFER_FUNCTION: Moves time, material resources, and administrative capacity from efficient, centralized solutions toward slow, participatory labor of welcome; the cost is distributed across the host community as sacrificed efficiency, while the benefit is communion-in-difference for all members.
% ABSENT_VOICES: Technocratic planners and transhumanist optimizers, who would argue for efficiency, unified systems, and technological transcendence of limits, are not present in the theological conversation that articulates this constraint; their absence is structural because the reading frames the problem as spiritual-pastoral rather than technical.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared, communities would lose the normative basis for non-assimilative welcome; solidarity would collapse into either charitable paternalism (efficiency without communion) or forced assimilation (uniformity without plurality), and returning exiles would face renewed marginalization.
% FOUNDING_PROBLEM: The fragmentation of human community after collective traumaâexile, dispersion, marginalizationâwhere efficiency-driven or coercive reunification destroys the very plurality that makes community authentically human.
% FOUNDING_PROBLEM_CORROBORATION: Marginalized returnees attest to the ongoing need for non-assimilative welcome; secular sociological and anthropological research on refugee integration corroborates that forced uniformity produces worse social outcomes than participatory integration. The Magisterium attests to the problem from within the tradition, while outside corroboration comes from independent community-studies scholarship.
narrative_ontology:disappearance_verdict(human_transcendence_pathway__jerusalem_reading, world_rearranges).
narrative_ontology:founding_problem_status(human_transcendence_pathway__jerusalem_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(human_transcendence_pathway__jerusalem_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(human_transcendence_pathway__jerusalem_reading, 'none', 1).
narrative_ontology:epsilon_provenance(human_transcendence_pathway__jerusalem_reading, 0.32, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is low-moderate (0.32) because the only 'cost' is sacrificed efficiency; there is no concentrated rent extraction. Suppression is low (0.18) because persistence relies on persuasion, liturgical formation, and moral suasion rather than coercion. Theater ratio is low (0.13) because the participatory labor is functionally genuine. Accessibility collapse is moderate (0.38): alternatives such as technocratic refugee management or assimilationist integration remain visible and operable in the broader culture. Resistance is low-moderate (0.22): friction exists from those who prefer efficiency and from marginalized who distrust slow processes, but the arrangement is not actively fought by its participants.
 *
 * PERSPECTIVAL GAP:
 *   The marginalized returnee seat experiences the constraint as gift and restorative justice; the host community seat experiences it as moral duty and material inconvenience; the ecclesial teacher seat experiences it as fidelity to revelation. The technocratic planner, excluded from the conversation, would read the same behaviors as irrational inefficiency and missed optimization opportunities. The engine should compute divergent directionalities: returnees and host community as beneficiaries (low d), while the technocratic planner would be a beneficiary of alternative constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   Both marginalized_returnees and host_community are declared beneficiaries with constrained exit, placing their directionality near the beneficiary pole. The ecclesial_teachers are agenda-setters with constrained exit; they subsidize rather than extract. No victim group is declared. The low derived directionality for all seated agents means effective extraction chi remains low, consistent with rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as rope rather than snare is protected by the absence of declared victims and the low suppression score: there is no coercion machinery extracting from one party for another. The classification as rope rather than mountain is protected by emerges_naturally: false and the acknowledged interpretive tradition. The classification as rope rather than tangled rope is protected by the absence of active enforcement and the symmetric beneficiary structure. If future analysis reveals that host communities bear asymmetric costs while returnees capture disproportionate benefit, the constraint would migrate toward tangled rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    solidarity_cost_distribution,
    'Is the efficiency sacrificed for solidarity borne symmetrically by all community members, or does it concentrate on the host community while returnees are net recipients?',
    'Ethnographic and economic study of participant communities to measure labor hours, material redistribution, and subjective burden across host and returnee populations.',
    'If concentrated, the constraint is a tangled rope with hidden asymmetric extraction beneath its communal rhetoric; if symmetric, it remains a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(solidarity_cost_distribution, empirical, 'Whether coordination costs are symmetrically distributed').

omega_variable(
    persuasion_coercion_boundary,
    'At what point does pastoral formation and persuasion become identity-locked coercion, particularly for members raised within this tradition?',
    'Analysis of exit patterns and post-exit narratives of individuals who leave communities practicing this model.',
    'If exit is identity-locked rather than mobile, effective extraction rises substantially and the constraint may compute as tangled rope or snare for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persuasion_coercion_boundary, conceptual, 'Boundary between persuasion and internalized coercion').

omega_variable(
    communion_uniformity_drift,
    'Does integration into communion in practice require doctrinal or cultural uniformity despite the theoretical commitment to plurality?',
    'Comparative analysis of communities claiming this model to measure actual diversity preservation versus assimilation pressure.',
    'If assimilation occurs, the constraint''s coordination function is undermined by hidden suppression of difference, raising theater_ratio and extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(communion_uniformity_drift, empirical, 'Plurality preservation versus assimilation in practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_transcendence_pathway__jerusalem_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, human_transcendence_pathway__jerusalem_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(huma_tr_t10, human_transcendence_pathway__jerusalem_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(huma_tr_t20, human_transcendence_pathway__jerusalem_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(huma_tr_t30, human_transcendence_pathway__jerusalem_reading, theater_ratio, 30, 0.11).
narrative_ontology:measurement(huma_tr_t40, human_transcendence_pathway__jerusalem_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement(huma_tr_t50, human_transcendence_pathway__jerusalem_reading, theater_ratio, 50, 0.13).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(huma_be_t10, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 10, 0.24).
narrative_ontology:measurement(huma_be_t20, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 20, 0.26).
narrative_ontology:measurement(huma_be_t30, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 30, 0.28).
narrative_ontology:measurement(huma_be_t40, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 40, 0.3).
narrative_ontology:measurement(huma_be_t50, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 50, 0.32).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(human_transcendence_pathway__jerusalem_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_transcendence_pathway__jerusalem_reading, attachment_coordination).
narrative_ontology:affects_constraint(human_transcendence_pathway__jerusalem_reading, babel_reading).
narrative_ontology:affects_constraint(human_transcendence_pathway__jerusalem_reading, technocratic_vs_incarnational_reading).

% DUAL FORMULATION NOTE:
% The human_transcendence_pathway kernel decomposes into at least three structurally distinct constraints: the Jerusalem reading (rope, low extraction, communion-in-plurality), the Babel reading (snare/mountain candidate, technological self-sufficiency), and the technocratic-vs-incarnational reading (tangled rope or commitment system, internal contest between optimization and grace). Each has a different epsilon, beneficiary structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
