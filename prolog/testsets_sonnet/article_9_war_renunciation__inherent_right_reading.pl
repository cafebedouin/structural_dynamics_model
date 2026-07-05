% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__inherent_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__inherent_right_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: article_9_war_renunciation__inherent_right_reading
 *   human_readable: Article 9 Inherent-Right-to-Self-Defense Reading (Minimum Necessary Capacity Threshold)
 *   domain: constitutional_law/security_policy/institutional_legitimacy
 *
 * SUMMARY:
 *   This story instantiates the inherent-right reading of the Article 9
 *   kernel: sovereign states retain a natural-law right to self-defense that
 *   Article 9's renunciation of 'war' does not extinguish, so long as
 *   maintained forces are kept to the 'minimum necessary' for territorial
 *   defense. Under this reading the textual prohibition becomes a threshold
 *   test rather than a categorical ban, and the Self-Defense Forces are
 *   organizationally legitimate but scope-limited. This is a distinct
 *   constraint from the strict pacifist reading (which finds the text
 *   categorical and the SDF unconstitutional in any form) and from the
 *   collective self-defense reading (which extends the inherent right to
 *   defense of allies absent direct attack on Japan) — each reading has a
 *   different ε, a different beneficiary/victim structure, and is authored as
 *   its own story per the ε-invariance principle, linked via
 *   network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__inherent_right_reading, 0.32).
domain_priors:suppression_score(article_9_war_renunciation__inherent_right_reading, 0.28).
domain_priors:theater_ratio(article_9_war_renunciation__inherent_right_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__inherent_right_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__inherent_right_reading, "Article 9 Inherent-Right-to-Self-Defense Reading (Minimum Necessary Capacity Threshold)").
narrative_ontology:topic_domain(article_9_war_renunciation__inherent_right_reading, "constitutional_law/security_policy/institutional_legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__inherent_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__inherent_right_reading, 'e3227ee9-225b-499e-90a2-491e23b123f0').
narrative_ontology:cs_kernel_codification('e3227ee9-225b-499e-90a2-491e23b123f0', fixed_text).
narrative_ontology:cs_authority_grounding('e3227ee9-225b-499e-90a2-491e23b123f0', extraction).
narrative_ontology:cs_interpretation_layer_present('e3227ee9-225b-499e-90a2-491e23b123f0').
narrative_ontology:cs_reading_relation('e3227ee9-225b-499e-90a2-491e23b123f0', article_9_war_renunciation__strict_pacifist_reading, forecloses).
narrative_ontology:cs_reading_relation('e3227ee9-225b-499e-90a2-491e23b123f0', article_9_war_renunciation__collective_self_defense_reading, influences).
narrative_ontology:cs_axiom('e3227ee9-225b-499e-90a2-491e23b123f0', foundational, customary_self_defense_right_survives_textual_renunciation).
narrative_ontology:cs_axiom_status(customary_self_defense_right_survives_textual_renunciation, holdable).
narrative_ontology:cs_axiom_grounding('e3227ee9-225b-499e-90a2-491e23b123f0', customary_self_defense_right_survives_textual_renunciation, conventional).
narrative_ontology:cs_axiom('e3227ee9-225b-499e-90a2-491e23b123f0', foundational, minimum_necessary_capacity_is_a_threshold_not_a_prohibition).
narrative_ontology:cs_axiom_status(minimum_necessary_capacity_is_a_threshold_not_a_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('e3227ee9-225b-499e-90a2-491e23b123f0', minimum_necessary_capacity_is_a_threshold_not_a_prohibition, instrumental).
narrative_ontology:cs_reference_frame('e3227ee9-225b-499e-90a2-491e23b123f0', sovereign_customary_right_baseline).
narrative_ontology:cs_drift_state('e3227ee9-225b-499e-90a2-491e23b123f0', post_2014_reinterpretation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e3227ee9-225b-499e-90a2-491e23b123f0', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, self_defense_forces_institution).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, ruling_coalition_governments).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, defense_industry_contractors).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, united_states_alliance_planners).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, strict_pacifist_constituency).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, war_generation_survivors).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, neighboring_states_wary_of_remilitarization).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__inherent_right_reading, cabinet_legal_bureau_interpretive_authority).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__inherent_right_reading, minimum_necessary_force_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues and revises the authoritative interpretation of Article 9 that permits 'minimum necessary' defensive forces without formal constitutional amendment. Controls the interpretive threshold itself, adjusting it incrementally (collective self-defense reinterpretation in 2014, expanded SDF roles since) while insisting the textual constraint remains intact. Its interpretive output is what all other seats must operate within.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, cabinet_legislation_bureau, agenda_setter,
    institutional, generational, arbitrage, national).

% Exists as a large, professionalized, budget-receiving military-equivalent organization whose entire legal legitimacy rests on this reading. Gains institutional continuity, funding growth, and expanding operational scope each time 'minimum necessary' is redefined upward. Has no incentive to see the threshold interpreted more narrowly.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, self_defense_forces_institution, beneficiary,
    institutional, generational, arbitrage, national).

% Successive LDP-led governments use the inherent-right reading to expand defense capability and deepen the US alliance without submitting the politically costly, procedurally difficult formal amendment process to a national referendum. Collects diplomatic and electoral benefit from appearing simultaneously constitutional and militarily credible.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, ruling_coalition_governments, beneficiary,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__inherent_right_reading, ruling_coalition_governments, agenda_setter).

% Benefit directly from an increasingly capable SDF that shares defense burden in the Indo-Pacific under the bilateral security treaty. Have consistently pressured Tokyo toward broader readings of the inherent-right doctrine and supply doctrine, equipment, and diplomatic cover for its expansion.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, united_states_alliance_planners, beneficiary,
    institutional, generational, mobile, regional).

% Domestic and allied defense manufacturers supply the equipment that constitutes 'minimum necessary' capacity, a category that has expanded to include long-range strike systems and larger naval assets. Revenue grows in direct proportion to how permissively the threshold is read.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, defense_industry_contractors, beneficiary,
    organized, biographical, arbitrage, national).

% Citizens, legal scholars, and opposition parties who hold that the text's plain language forecloses any armed force. Bear the cost of watching the constitution's most literal reading be administratively bypassed without amendment; their only recourse is litigation (largely unsuccessful under the political-question doctrine) or electoral mobilization, which has not produced a supermajority for either amendment or reversal.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, strict_pacifist_constituency, payer,
    moderate, generational, constrained, national).

% The dwindling population who experienced the war and the drafting of Article 9 as a direct, categorical repudiation of Japanese militarism. Experience each expansion of SDF scope as an erosion of the postwar settlement they lived through, with no meaningful institutional channel to arrest the drift.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, war_generation_survivors, payer,
    powerless, biographical, trapped, national).

% South Korea, China, and other states with historical memory of Japanese military aggression treat each redefinition of 'minimum necessary' as a security-dilemma trigger, prompting their own military buildups or diplomatic protest. Cannot compel Japan's internal constitutional interpretation and can only respond after the fact.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, neighboring_states_wary_of_remilitarization, payer,
    organized, generational, constrained, regional).

% The Japanese judiciary has repeatedly declined to rule on the substantive constitutionality of the SDF or security legislation, invoking the political-question doctrine. This non-adjudication is itself a structural choice that leaves the inherent-right reading uncontested by the one body positioned to arbitrate it.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides Japan with a legally cognizable basis for territorial defense capacity and alliance participation without requiring the two-thirds Diet supermajority and national referendum that formal Article 96 amendment would demand — solving the genuine problem that a purely pacifist state faces credible external threats it has no textual permission to address.
% TRANSFER_FUNCTION: Moves interpretive authority over the constitution's most contested clause from the amendment process (requiring broad democratic consent) to the executive's legal bureau and successive cabinets; moves budgetary and political capital toward the SDF and defense industry; moves security anxiety toward neighboring states and moves constitutional fidelity costs onto the pacifist constituency and war generation, who see the textual commitment hollowed out without ever having their objection tested at a referendum.
% ABSENT_VOICES: The war generation survivors and strict pacifist constituency would object that 'never be maintained' admits no threshold-reading at all; they are structurally absent from the interpretive process because the Cabinet Legislation Bureau, not the electorate, issues the controlling interpretation, and the courts decline to review it.
% DISAPPEARANCE_RATIONALE: If the inherent-right reading were abandoned, the SDF's core legal basis would need reconstruction through either formal amendment or a reversion to the strict pacifist reading disbanding the force — a genuinely destabilizing rearrangement for defense policy and the US alliance. But pacifist advocates argue the 'disappearance' would simply restore the constitution's original textual meaning and the world would rearrange toward what the drafters intended, not away from a natural state. Which characterization is correct is exactly the kernel dispute.
% FOUNDING_PROBLEM: Post-surrender Japan needed a constitutional mechanism to renounce the aggressive militarism that had produced catastrophic war, while the state simultaneously needed some means of asserting sovereign continuity and, over time, addressing external security threats (Cold War tensions, then China's and North Korea's rise) that a wholly disarmed state could not textually answer without amendment.
% FOUNDING_PROBLEM_CORROBORATION: The Cabinet Legislation Bureau and successive governments attest the inherent right to self-defense was always latent in the sovereign-state premise of international law and the founding problem (external threat response) remains live. Independent constitutional scholars, some sitting Diet opposition members, and comparative international law analysts outside the government attest that the 1946 drafters' own recorded intent (Yoshida's Diet testimony renouncing even self-defense forces) undercuts this reading — corroboration exists on both sides of the contest, which is itself the structural fact worth recording.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__inherent_right_reading, contested).
narrative_ontology:founding_problem_status(article_9_war_renunciation__inherent_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__inherent_right_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_9_war_renunciation__inherent_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__inherent_right_reading, 0.32, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__inherent_right_reading_tests).
:- end_tests(article_9_war_renunciation__inherent_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.32) and has drifted upward slowly since 1946 as the operative definition of 'minimum necessary' has expanded (1954 SDF founding, 1991 Gulf War logistics support, 2003 Iraq reconstruction dispatch, 2014 collective self-defense cabinet reinterpretation) without formal amendment — each step reallocates a little more interpretive authority from the electorate to the executive's legal bureau. Theater ratio is meaningfully elevated (0.42) because much of the public discourse around 'defense-only' posture and equipment naming (e.g., calling capital ships 'helicopter destroyers') performs textual fidelity while substantive capability has grown; the performance is not costless, it is the mechanism that lets the reading persist without triggering the referendum that would resolve the underlying contest. Suppression is comparatively low (0.28) because no one is coercively silenced — the mechanism is interpretive capture and judicial non-review (political-question doctrine) rather than force.
 *
 * PERSPECTIVAL GAP:
 *   From the Cabinet Legislation Bureau's seat this looks like principled, incremental constitutional interpretation consistent with international law's baseline sovereign right — a rope solving a real problem (credible defense without amendment gridlock). From the pacifist constituency's seat the same structure looks like a tangled rope: real coordination function (defense capacity, alliance credibility) bundled with real extraction (executive capture of an interpretive question that constitutionally belongs to the amendment process, imposed on those who reject the reading with no working mechanism to contest it). The engine computing divergent per-seat types from the same structural data is exactly the point — this story does not adjudicate which seat is 'right.'
 *
 * DIRECTIONALITY LOGIC:
 *   The Cabinet Legislation Bureau and ruling coalitions sit at the agenda-setting end: they produce and revise the controlling interpretation, so directionality places them near the beneficiary pole. The SDF, defense contractors, and US alliance planners are structural beneficiaries who gain institutional continuity, revenue, and burden-sharing respectively as the threshold is read more permissively — low d. The strict pacifist constituency and war generation survivors are targets: the text they read as absolute is administratively bypassed, and their exit options are constrained (litigation fails on justiciability grounds, and no supermajority exists for amendment in either direction) — high d. Neighboring states bear externalized security-dilemma costs without any voice in Japan's internal interpretation — also high d despite their organized regional power, because the constraint operates entirely outside their jurisdiction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how does a war-renouncing state survive Cold War and post-Cold War security threats) is genuinely still live for the beneficiary seats, which is why this does not collapse into a pure snare — there is a real coordination function being solved. But the founding problem as originally stated by the 1946 drafters (a categorical, disarmed peace state) is dead by this reading's own operation, and that death was never ratified by the amendment process the constitution itself specifies for changing that commitment. The tangled_rope classification captures this: coordination is real, but it rides on an unresolved, uncorroborated-outside-government asymmetric transfer of interpretive authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_vs_natural_law_ground,
    'Does the inherent right to self-defense derive from customary international law and sovereign-state status independent of Article 9''s text (making the article a limitation on an otherwise-existing right), or does Article 9''s plain text override any prior customary right entirely for Japan specifically?',
    'Comparative constitutional analysis of how the Diet''s 1946 ratification debates and Yoshida''s own contemporaneous testimony treated the relationship between customary international law and the new text; examination of whether any drafting-history evidence shows the drafters intended to preserve or extinguish the customary right.',
    'If the customary right is held to survive the text, this reading''s core premise is strongly grounded and the threshold approach is the natural implementation; if the text is held to fully displace customary international law for Japan, this reading''s foundational axiom is significantly weakened relative to the strict pacifist reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_vs_natural_law_ground, conceptual, 'Whether Article 9 limits a pre-existing customary right or extinguishes it entirely.').

omega_variable(
    minimum_necessary_definitional_drift,
    'Is there a principled, externally verifiable definition of ''minimum necessary for territorial defense'' that could stop the threshold from being redefined upward indefinitely, or is the category inherently elastic to whatever capability the government of the day wants?',
    'Track whether any capability category has ever been ruled OUT under this doctrine by the Cabinet Legislation Bureau or courts, versus tracking only expansions; the absence of any contraction across 70+ years is itself evidence about elasticity.',
    'If the category has no operative ceiling, this reading structurally cannot be distinguished from unlimited rearmament dressed in defensive language, which would push the computed type toward snare rather than tangled_rope over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minimum_necessary_definitional_drift, empirical, 'Whether ''minimum necessary'' has any actual ceiling or only ever expands.').

omega_variable(
    judicial_non_review_as_structural_choice,
    'Is the judiciary''s consistent invocation of the political-question doctrine to avoid ruling on SDF constitutionality itself a form of tacit endorsement of the inherent-right reading, or genuine institutional restraint agnostic to the merits?',
    'Compare Japanese courts'' political-question invocations on this issue to their invocation rate on other genuinely non-justiciable questions; look for any dicta suggesting substantive views on the merits despite formal non-review.',
    'If non-review functions as tacit endorsement, the interpretive authority vindicated by this constraint is less independently corroborated than it appears, strengthening the case that founding_problem_corroboration is thinner than claimed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_non_review_as_structural_choice, conceptual, 'Whether judicial non-review is neutral restraint or de facto validation of one reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__inherent_right_reading, 1946, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1946, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1946, 0.2).
narrative_ontology:measurement(arti_tr_t1954, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1954, 0.28).
narrative_ontology:measurement(arti_tr_t1972, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1972, 0.32).
narrative_ontology:measurement(arti_tr_t1991, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1991, 0.35).
narrative_ontology:measurement(arti_tr_t2003, article_9_war_renunciation__inherent_right_reading, theater_ratio, 2003, 0.38).
narrative_ontology:measurement(arti_tr_t2014, article_9_war_renunciation__inherent_right_reading, theater_ratio, 2014, 0.4).
narrative_ontology:measurement(arti_tr_t2024, article_9_war_renunciation__inherent_right_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(arti_be_t1946, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1946, 0.1).
narrative_ontology:measurement(arti_be_t1954, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1954, 0.18).
narrative_ontology:measurement(arti_be_t1972, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1972, 0.22).
narrative_ontology:measurement(arti_be_t1991, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1991, 0.24).
narrative_ontology:measurement(arti_be_t2003, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2003, 0.27).
narrative_ontology:measurement(arti_be_t2014, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2014, 0.29).
narrative_ontology:measurement(arti_be_t2024, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2024, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1946, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1946, 0.1).
narrative_ontology:measurement(arti_su_t1954, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1954, 0.15).
narrative_ontology:measurement(arti_su_t1972, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1972, 0.18).
narrative_ontology:measurement(arti_su_t1991, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1991, 0.2).
narrative_ontology:measurement(arti_su_t2003, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2003, 0.22).
narrative_ontology:measurement(arti_su_t2014, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2014, 0.26).
narrative_ontology:measurement(arti_su_t2024, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2024, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__inherent_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_9_war_renunciation__inherent_right_reading, 0.12).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation__strict_pacifist_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation__collective_self_defense_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories decompose the natural-language 'Article 9 war renunciation' claim, each a distinct reading of the article_9_war_renunciation kernel with its own ε: strict_pacifist_reading (categorical prohibition, near-mountain textualism, SDF itself unconstitutional), inherent_right_reading (this story — threshold/proportionality approach, tangled_rope), and collective_self_defense_reading (extends the inherent right further to ally defense, the most extractive/contested of the three, likely tangled_rope trending toward snare given the 2014 reinterpretation's bypass of normal amendment process). inherent_right_reading is the foundational reading the collective_self_defense_reading builds on and extends; strict_pacifist_reading stands as the foreclosed textual alternative this reading's axioms displace.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
