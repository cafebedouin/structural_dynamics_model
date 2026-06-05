% ============================================================================
% CONSTRAINT STORY: direct_democracy__plebiscitary_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_direct_democracy__plebiscitary_capture, []).

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
 *   constraint_id: direct_democracy__plebiscitary_capture
 *   human_readable: Plebiscitary Capture of Direct Democracy
 *   domain: political/comparative_governance
 *
 * SUMMARY:
 *   The plebiscite represents direct democracy captured by executive agenda
 *   control. Unlike referenda (questions initiated by popular petition or
 *   legislature) or town meetings (where the agenda emerges from assembled
 *   deliberation), the plebiscite is the ruler's mechanism: the executive
 *   frames the question, chooses the timing, controls the information
 *   environment, and converts a yes-or-no outcome into popular mandate.
 *   Citizens experience this as empowerment — they are being consulted! —
 *   while bearing the cost of decision deprivation: the alternatives not
 *   asked, the nuance collapsed into binary, the agenda set before the people
 *   even gathered. The constraint exhibits the signature of a snare: high
 *   suppression (the executive controls what can be asked), high
 *   extractiveness (consent is manufactured from a constrained choice set),
 *   high theater (participation appears democratic while substantive decision
 *   power remains concentrated). The suppressed alternatives — the policy
 *   questions the executive chose not to pose — are the most powerless agents
 *   in the system: they have no mechanism to surface, no representation in
 *   deliberation, no voice in the binary choice. This reading is one
 *   interpretation of the contested kernel 'direct democracy': the
 *   plebiscitary reading emphasizes the concentration of agenda power within
 *   democratic forms; sibling readings (Swiss referenda, town meetings)
 *   represent structurally different mechanisms that distribute
 *   agenda-setting authority to collective bodies rather than executives.
 *
 * KEY AGENTS:
 *   - Executive Authority: Primary beneficiary (institutional/arbitrage) — frames the question, chooses timing, controls information flow, converts yes-or-no into mandate. Experiences the plebiscite as coordination and legitimation, not extraction.
 *   - Citizen Voters: Primary victim (powerless/trapped) — offered binary choice they did not construct; their formal participation masks suppression of the deliberative agenda. Bear the cost of decision deprivation.
 *   - Opposition Coalition: Secondary victim (moderate/constrained) — constrained by the binary frame; can campaign against but cannot reframe or add alternatives; their political capacity is converted into a yes-or-no litmus test.
 *   - Suppressed Alternatives: Tertiary victim (powerless/trapped) — the policy questions the executive chose not to ask; the nuance flattened into yes-or-no; have no mechanism to surface or be deliberated. Cannot even be named as victims because they were never recognized as possible.
 *   - Popular Sovereignty Principle: Organized actor (organized/constrained) — provides the legitimating ideal for the plebiscite; experienced as both coordination (people do decide on what they're asked) and extraction (their sovereign power is converted into veto-or-ratify function).
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the plebiscite as inherent to democracy at scale, missing the contingent institutional design that concentrates executive power.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(direct_democracy__plebiscitary_capture, 0.58).
domain_priors:suppression_score(direct_democracy__plebiscitary_capture, 0.68).
domain_priors:theater_ratio(direct_democracy__plebiscitary_capture, 0.74).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(direct_democracy__plebiscitary_capture, extractiveness, 0.58).
narrative_ontology:constraint_metric(direct_democracy__plebiscitary_capture, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(direct_democracy__plebiscitary_capture, theater_ratio, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(direct_democracy__plebiscitary_capture, snare).
narrative_ontology:human_readable(direct_democracy__plebiscitary_capture, "Plebiscitary Capture of Direct Democracy").
narrative_ontology:topic_domain(direct_democracy__plebiscitary_capture, "political/comparative_governance").

domain_priors:requires_active_enforcement(direct_democracy__plebiscitary_capture).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(direct_democracy__plebiscitary_capture, '9108c047-84aa-4c9b-bf61-ed5b8ddd899e').
narrative_ontology:cs_kernel_codification('9108c047-84aa-4c9b-bf61-ed5b8ddd899e', formalized).
narrative_ontology:cs_authority_grounding('9108c047-84aa-4c9b-bf61-ed5b8ddd899e', extraction).
narrative_ontology:cs_interpretation_layer_present('9108c047-84aa-4c9b-bf61-ed5b8ddd899e').
narrative_ontology:cs_reading_relation('9108c047-84aa-4c9b-bf61-ed5b8ddd899e', direct_democracy__swiss_referendum_system, coexists_with).
narrative_ontology:cs_reading_relation('9108c047-84aa-4c9b-bf61-ed5b8ddd899e', direct_democracy__town_meeting_tradition, coexists_with).
narrative_ontology:cs_axiom('9108c047-84aa-4c9b-bf61-ed5b8ddd899e', foundational, executive_controls_democratic_question).
narrative_ontology:cs_axiom_status(executive_controls_democratic_question, holdable).
narrative_ontology:cs_axiom_grounding('9108c047-84aa-4c9b-bf61-ed5b8ddd899e', executive_controls_democratic_question, empirically_contingent).
narrative_ontology:cs_axiom('9108c047-84aa-4c9b-bf61-ed5b8ddd899e', foundational, binary_choice_masks_agenda_suppression).
narrative_ontology:cs_axiom_status(binary_choice_masks_agenda_suppression, holdable).
narrative_ontology:cs_axiom_grounding('9108c047-84aa-4c9b-bf61-ed5b8ddd899e', binary_choice_masks_agenda_suppression, empirically_contingent).
narrative_ontology:cs_reference_frame('9108c047-84aa-4c9b-bf61-ed5b8ddd899e', popular_sovereignty_through_collective_decision).
narrative_ontology:cs_drift_state('9108c047-84aa-4c9b-bf61-ed5b8ddd899e', contemporary_executive_plebiscitary_strategy, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9108c047-84aa-4c9b-bf61-ed5b8ddd899e', '').
narrative_ontology:cs_kernel_id(direct_democracy__plebiscitary_capture, direct_democracy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(direct_democracy__plebiscitary_capture, question_framer).
narrative_ontology:constraint_beneficiary(direct_democracy__plebiscitary_capture, executive_authority).
narrative_ontology:constraint_victim(direct_democracy__plebiscitary_capture, suppressed_alternatives).
narrative_ontology:constraint_victim(direct_democracy__plebiscitary_capture, deliberative_process).
narrative_ontology:constraint_victim(direct_democracy__plebiscitary_capture, minority_positions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE CITIZEN VOTER (SNARE) — Presented with a binary choice framed by the executive; no mechanism to exit the vote or alter the question. The formal participation masks suppression of the deliberative agenda. Maximum extraction: consent is manufactured from a constrained choice set that the voter did not construct. The voter experiences this as empowerment (they chose!) while bearing the cost of decision deprivation (they chose from what they were offered).
constraint_indexing:constraint_classification(direct_democracy__plebiscitary_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE OPPOSITION COALITION (SNARE) — Constrained by the binary frame: they can campaign against the executive's proposal, but they cannot reframe the question or add alternatives to the ballot. If they lose the plebiscite, the decision is locked in with the force of 'the people have spoken.' If they win, they have only vetoed, not governed. High extraction: the constraint converts their political capacity into a yes-or-no litmus test that measures opposition to a specific executive question, not capacity to propose alternatives.
constraint_indexing:constraint_classification(direct_democracy__plebiscitary_capture, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE EXECUTIVE AUTHORITY (ROPE) — Frames the question, chooses timing, controls information flow, and converts the outcome into mandate. The executive experiences the plebiscite as coordination (the people have been consulted!), not extraction. The mechanism guarantees legitimacy for the executive's preferred outcome and locks in the decision with popular sanction. Low experienced extraction because the executive is the beneficiary of the design itself — they have full agency within the constraint.
constraint_indexing:constraint_classification(direct_democracy__plebiscitary_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE SUPPRESSED DELIBERATIVE AGENDA (SNARE) — The set of policy alternatives that the executive chose not to ask about, the questions reframed into binary form, the nuance flattened into yes-or-no. These alternatives are powerless and trapped — they have no mechanism to surface, no way to be placed on the ballot, no venue for deliberation. They bear extraction: the executive's choice to ask 'Should we build the dam?' rather than 'How should we manage water resources?' has allocated decision power entirely to the executive's framing. The suppressed alternatives cannot even be named as victims because they were never recognized as possible.
constraint_indexing:constraint_classification(direct_democracy__plebiscitary_capture, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 5: THE POPULAR SOVEREIGNTY PRINCIPLE (TANGLED ROPE) — From the legitimating ideal that democracy means the people decide, the plebiscite provides genuine coordination: a mechanism for aggregating public will on a specific question. But the coordination comes embedded in asymmetric extraction: the executive controls the question, the timing, and the binary frame. The plebiscite both realizes popular sovereignty and subordinates it to executive agenda-setting. Real coordination function (the people do decide on what they are asked) coexists with extractive agenda control. Moderately high extracted value: the people bear the cost of having their sovereign power converted into a veto-or-ratify function.
constraint_indexing:constraint_classification(direct_democracy__plebiscitary_capture, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the binary nature of democratic decisions might appear to be an inherent feature of collective choice: voting mechanisms always reduce continuous preferences to discrete outcomes, and this seems natural and necessary. The observer might classify the plebiscite's structure as arising inevitably from the logic of majority rule itself. However, the structural data (beneficiaries framing the question, victims consisting of suppressed alternatives, high suppression of deliberation) contradicts the mountain classification — the engine will detect this as a false summit, revealing that what appears as natural constraint is actually a contingent institutional design that concentrates decision power in the executive.
constraint_indexing:constraint_classification(direct_democracy__plebiscitary_capture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(direct_democracy__plebiscitary_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(direct_democracy__plebiscitary_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(direct_democracy__plebiscitary_capture, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(direct_democracy__plebiscitary_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(direct_democracy__plebiscitary_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The executive captures agenda-setting authority and manufactures consent through framing and timing. However, the extraction is not absolute because the citizen voter does have formal agency — they can vote yes or no, and the outcome is genuinely affected by their choice. The extractiveness reflects that the executive's control of the question is the dominant structural feature, converting decision-making power into a veto-or-ratify function. The measurement trajectory (0.45 → 0.58) models increasing executive sophistication in plebiscitary design and information control. Suppression (0.68): High. The executive controls what can be asked (alternatives cannot be posed), controls timing (the plebiscite is announced when politically advantageous), and controls information framing (the executive's campaign apparatus dominates the deliberative environment). Suppression is not total (citizens can campaign, opposition can organize) but is structurally asymmetric: the executive starts with agenda control. The measurement trajectory (0.55 → 0.68) models increasing professionalization of plebiscitary suppression mechanics. Theater ratio (0.74): High. The plebiscite is substantially performative: the appearance of consultation and popular decision-making masks the prior concentration of agenda power. Citizens experience voting as empowering while the substantive decision (what questions deserve deliberation?) has already been made by the executive. The measurement trajectory (0.68 → 0.74) models the theater increasing as executives learn to stage-manage the plebiscitary process.
 *
 * PERSPECTIVAL GAP:
 *   The plebiscitary reading exhibits a sharp perspectival gap between the beneficiary and victims. The executive sees a coordination mechanism: the people have been consulted, the outcome has popular sanction, the decision is legitimate. Citizens see empowerment followed by constraint: they chose! They also chose from what they were offered, and what was offered was the executive's question. Opposition sees effective suppression: they can campaign against a specific proposal but cannot propose alternatives or reframe the decision. The suppressed alternatives experience the maximum perspectival gap: they were never recognized as possible choices, so they have no voice in the binary vote. The analytical observer at civilizational scale risks seeing the plebiscite as natural — voting mechanisms always reduce preferences to discrete outcomes — when the structural data reveals a contingent institutional design: the question could be set by petition, the timing could be decoupled from executive political cycles, deliberation could precede the binary vote (as in town meetings or Swiss referenda). The gap between what the plebiscite claims to do (let the people decide) and what it actually does (let the executive decide what the people decide about) is the core extraction mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from structural position relative to the constraint. The executive authority benefits from the constraint (they control the question and convert yes-or-no into mandate) and has arbitrage options (they can reframe future questions), resulting in low d → negative effective extraction from their perspective. Citizens and opposition are trapped or constrained (they cannot exit the binary choice and have no mechanism to alter the question), resulting in high d → high effective extraction from their perspectives. The suppressed alternatives are powerless and trapped with no mechanism to surface, resulting in maximum d. The popular sovereignty principle experiences asymmetric extraction (genuine coordination function shadowed by executive agenda control), resulting in moderate-to-high d. The analytical observer risks d=0.5 (symmetric) by seeing the plebiscite as natural and necessary, which the false summit detector flags as misclassifying a contingent institutional design.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by distinguishing plebiscitary capture (snare: executive agenda control + binary suppression) from direct democracy itself (which could be rope, tangled_rope, or even mountain depending on mechanism and structural embedding). The snare classification applies to plebiscites specifically — mechanisms where the executive controls question framing, timing, and binary choice. The tangled_rope perspective from organized opposition and the popular sovereignty principle recognizes genuine coordination (the people do aggregate preferences on the question asked) embedded in asymmetric extraction (the executive set the question). The mountain perspective (natural law view) risks false summarization by treating the plebiscite's structure as inherent to democracy at scale. The swiss_referendum_system and town_meeting_tradition readings represent structurally different mechanisms (popular initiative, distributed agenda-setting) that organize direct democracy without plebiscitary capture. The mandatrophy is resolved by showing that direct democracy has multiple institutional manifestations with different extraction profiles — the choice of mechanism is the prior structural question that determines whether direct democracy becomes an executive tool or a populist check on executive power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    question_frame_manipulation_threshold,
    'At what point does executive question-framing cross from legitimate choice of policy emphasis to suppression of genuine alternatives?',
    'Comparative analysis of plebiscite questions across democracies and autocracies; measurement of reframing rates by opposition; correlation between question frame and outcome predictability',
    'If threshold is high (broad framing discretion is normal): plebiscitary capture is a moderate constraint. If threshold is low (any non-neutral framing counts as suppressive): plebiscitary capture is a severe snare. This determines whether the executive''s question-setting is legitimate agenda-control or illegitimate agenda-monopoly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(question_frame_manipulation_threshold, conceptual, 'Threshold distinguishing legitimate question emphasis from suppressive agenda control').

omega_variable(
    deliberative_sufficiency_of_plebiscite,
    'Can a plebiscite campaign period provide genuine deliberation about the question framed, or does the binary constraint inherently foreclose substantive deliberation?',
    'Content analysis of campaign discourse: proportion of time spent on the executive''s question vs. alternative framings, depth of issue exploration, representation of minority perspectives in campaign coverage',
    'If deliberation is sufficient: the plebiscite is a tangled rope with robust coordination function. If deliberation is suppressed: the plebiscite is a snare masquerading as participation. This determines whether the constraint provides meaningful citizen input despite the binary frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberative_sufficiency_of_plebiscite, empirical, 'Whether plebiscite campaigns enable genuine deliberation or suppress it').

omega_variable(
    referendum_vs_plebiscite_structural_difference,
    'What structural features distinguish a plebiscite (executive question, executive timing, binary choice) from a referendum (question initiated by popular petition or legislature, structured alternatives, deliberative agenda-setting)?',
    'Institutional analysis of decision-making authority: Who controls the question? Who controls timing? Are alternatives or abstention options available? What deliberative processes precede the vote?',
    'If the distinction is real and substantial: plebiscitary capture is a specific institutional failure, not inherent to direct democracy. This omega routes toward the swiss_referendum_system and town_meeting_tradition readings — alternative mechanisms that supply agenda control to collective bodies rather than executives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(referendum_vs_plebiscite_structural_difference, conceptual, 'Structural differences between plebiscites and referenda as decision mechanisms').

omega_variable(
    consent_manufacturing_vs_consent_aggregation,
    'Is the plebiscite aggregating pre-existing preferences, or manufacturing consent through the framing, timing, and information control?',
    'Pre-plebiscite polling vs. post-vote analysis; measurement of opinion shift during campaign; analysis of information asymmetries between executive and opposition; temporal analysis of question announcement and timing (does timing correlate with political opportunity for the executive?)',
    'If manufacturing: extraction is high, snare classification is robust. If aggregating: extraction is moderate, tangled_rope classification is warranted. This determines whether the plebiscite is an extractive mechanism or a legitimate decision procedure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_manufacturing_vs_consent_aggregation, empirical, 'Whether plebiscites manufacture or aggregate consent').

omega_variable(
    kernel_reading_contest_natural_law_vs_contingent,
    'Is the plebiscite a natural consequence of trying to aggregate preferences in a large democracy, or a contingent institutional design that concentrates executive power?',
    'Comparative institutional analysis: What decision mechanisms do other direct democracies use (Swiss referenda, town meetings, liquid democracy)? What outcomes result from different agenda-setting structures? Historical analysis: Was the plebiscite introduced as a deliberate power-consolidation tool or as a natural evolution of democratic practice?',
    'If natural: the mountain perspective is correct, and plebiscitary capture is an inherent feature of democracy at scale. If contingent: the false summit detector fires, and the swiss_referendum_system and town_meeting_tradition readings represent structurally different (less extractive) ways of organizing direct democracy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_natural_law_vs_contingent, conceptual, 'Kernel contest: Is plebiscitary capture inevitable or a contingent institutional design?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(direct_democracy__plebiscitary_capture, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plebi_tr_t0, direct_democracy__plebiscitary_capture, theater_ratio, 0, 0.68).
narrative_ontology:measurement(plebi_tr_t3, direct_democracy__plebiscitary_capture, theater_ratio, 3, 0.71).
narrative_ontology:measurement(plebi_tr_t6, direct_democracy__plebiscitary_capture, theater_ratio, 6, 0.74).

% Extraction over time
narrative_ontology:measurement(plebi_be_t0, direct_democracy__plebiscitary_capture, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(plebi_be_t3, direct_democracy__plebiscitary_capture, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(plebi_be_t6, direct_democracy__plebiscitary_capture, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(plebi_su_t0, direct_democracy__plebiscitary_capture, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(plebi_su_t3, direct_democracy__plebiscitary_capture, suppression_requirement, 3, 0.62).
narrative_ontology:measurement(plebi_su_t6, direct_democracy__plebiscitary_capture, suppression_requirement, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(direct_democracy__plebiscitary_capture, resource_allocation).
narrative_ontology:affects_constraint(direct_democracy__plebiscitary_capture, direct_democracy__swiss_referendum_system).
narrative_ontology:affects_constraint(direct_democracy__plebiscitary_capture, direct_democracy__town_meeting_tradition).
narrative_ontology:affects_constraint(direct_democracy__plebiscitary_capture, executive_power_concentration).
narrative_ontology:affects_constraint(direct_democracy__plebiscitary_capture, agenda_setting_monopoly).

% DUAL FORMULATION NOTE:
% The plebiscitary capture reading is one pole of the direct democracy kernel contest. The swiss_referendum_system and town_meeting_tradition readings represent structurally different mechanisms for organizing direct democracy without concentrating agenda power in the executive. All three constraints share the kernel ('direct democracy') but have different ε values reflecting different institutional structures: plebiscitary capture (ε≈0.58, snare) models executive agenda control; Swiss referenda (ε≈0.25, rope) model distributed agenda-setting; town meetings (ε≈0.15, rope) model emergent deliberative agendas. The network links these siblings and upstream constraints (executive power concentration, agenda monopoly) that create the conditions for plebiscitary capture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
