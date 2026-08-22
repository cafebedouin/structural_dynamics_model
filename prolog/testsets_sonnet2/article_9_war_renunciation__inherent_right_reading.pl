% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__inherent_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Article 9 as Threshold on Inherent Self-Defense Capacity (Minimum Necessary Force Doctrine)
 *   domain: constitutional_law/security_policy/institutional_legitimacy
 *
 * SUMMARY:
 *   This story instantiates the inherent-right reading of Article 9 of
 *   Japan's postwar constitution: sovereign states possess an inherent,
 *   pre-constitutional right to self-defense that the text's renunciation of
 *   'war' does not and cannot extinguish, so long as defensive capacity is
 *   kept to the 'minimum necessary.' This reading converts the clause's plain
 *   categorical language into a proportionality threshold rather than an
 *   absolute prohibition, and it is the reading the Japanese executive and
 *   Cabinet Legislation Bureau have relied on since the 1950s to establish
 *   and expand the Self-Defense Forces without constitutional amendment. The
 *   extraction measured here is the executive's capture of interpretive
 *   authority away from the amendment process the constitution itself
 *   specifies, plus the steady drift of 'minimum necessary' toward larger and
 *   more capable forces over seven decades. Two sibling readings exist as
 *   separate constraints: the strict pacifist reading (which holds the
 *   categorical text controls and treats any armed force as unconstitutional)
 *   and the collective self-defense reading (which extends the inherent right
 *   to defending allies, a further expansion this story's threshold logic
 *   makes possible but does not itself assert).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__inherent_right_reading, 0.38).
domain_priors:suppression_score(article_9_war_renunciation__inherent_right_reading, 0.42).
domain_priors:theater_ratio(article_9_war_renunciation__inherent_right_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__inherent_right_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__inherent_right_reading, "Article 9 as Threshold on Inherent Self-Defense Capacity (Minimum Necessary Force Doctrine)").
narrative_ontology:topic_domain(article_9_war_renunciation__inherent_right_reading, "constitutional_law/security_policy/institutional_legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__inherent_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__inherent_right_reading, 'b95eea61-25d7-43b8-8043-becd826b7fe8').
narrative_ontology:cs_kernel_codification('b95eea61-25d7-43b8-8043-becd826b7fe8', fixed_text).
narrative_ontology:cs_authority_grounding('b95eea61-25d7-43b8-8043-becd826b7fe8', extraction).
narrative_ontology:cs_interpretation_layer_present('b95eea61-25d7-43b8-8043-becd826b7fe8').
narrative_ontology:cs_reading_relation('b95eea61-25d7-43b8-8043-becd826b7fe8', article_9_war_renunciation__strict_pacifist_reading, forecloses).
narrative_ontology:cs_reading_relation('b95eea61-25d7-43b8-8043-becd826b7fe8', article_9_war_renunciation__collective_self_defense_reading, influences).
narrative_ontology:cs_axiom('b95eea61-25d7-43b8-8043-becd826b7fe8', foundational, sovereign_states_retain_inherent_defense_right).
narrative_ontology:cs_axiom_status(sovereign_states_retain_inherent_defense_right, holdable).
narrative_ontology:cs_axiom_grounding('b95eea61-25d7-43b8-8043-becd826b7fe8', sovereign_states_retain_inherent_defense_right, conventional).
narrative_ontology:cs_axiom('b95eea61-25d7-43b8-8043-becd826b7fe8', foundational, textual_renunciation_targets_aggression_only).
narrative_ontology:cs_axiom_status(textual_renunciation_targets_aggression_only, holdable).
narrative_ontology:cs_axiom_grounding('b95eea61-25d7-43b8-8043-becd826b7fe8', textual_renunciation_targets_aggression_only, conventional).
narrative_ontology:cs_axiom('b95eea61-25d7-43b8-8043-becd826b7fe8', secondary, capacity_bounded_by_minimum_necessity).
narrative_ontology:cs_axiom_status(capacity_bounded_by_minimum_necessity, holdable).
narrative_ontology:cs_axiom_grounding('b95eea61-25d7-43b8-8043-becd826b7fe8', capacity_bounded_by_minimum_necessity, instrumental).
narrative_ontology:cs_reference_frame('b95eea61-25d7-43b8-8043-becd826b7fe8', id_1950s_cabinet_legislation_bureau_settlement).
narrative_ontology:cs_drift_state('b95eea61-25d7-43b8-8043-becd826b7fe8', post_2015_security_legislation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b95eea61-25d7-43b8-8043-becd826b7fe8', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, japanese_government_executive).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, self_defense_forces_institution).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, defense_industrial_base).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, united_states_alliance_planners).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, strict_pacifist_constituencies).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, regional_neighbors_wary_of_remilitarization).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, constitutional_textualist_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cabinet Legislation Bureau interpretations and successive administrations have maintained since the 1950s that Article 9 does not renounce the inherent right of self-defense, only aggressive war. This reading lets the executive authorize, fund, and expand the Self-Defense Forces through reinterpretation and legislation rather than constitutional amendment, avoiding the supermajority and referendum required by Article 96.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, japanese_government_executive, agenda_setter,
    institutional, generational, arbitrage, national).

% Exists, recruits, procures equipment, and operates under this reading's legitimacy; would not exist in its current organizational form under the strict pacifist reading. Its scope-limited mandate ('minimum necessary for defense') is simultaneously its legal shield and the ceiling that constrains its budget and mission expansion relative to a normalized military.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, self_defense_forces_institution, beneficiary,
    institutional, generational, mobile, national).

% Domestic defense contractors and their supply chains depend on sustained SDF procurement, which the inherent-right reading legitimizes and the strict pacifist reading would eliminate. They lobby for looser 'minimum necessary' thresholds to expand permissible acquisitions (e.g., counterstrike capability, aircraft carriers reclassified as helicopter destroyers).
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, defense_industrial_base, beneficiary,
    organized, biographical, mobile, national).

% US strategic planning under the bilateral security treaty depends on Japan maintaining credible defensive capacity to share basing costs and regional deterrence burden. The inherent-right reading gives the US a capable, standing military partner without needing to negotiate Article 9's amendment, which would be domestically and diplomatically costly.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, united_states_alliance_planners, beneficiary,
    institutional, generational, arbitrage, regional).

% War-generation survivors, peace movement organizations, and constitutional scholars who read 'never be maintained' as categorical see the inherent-right reading as a slow-motion nullification of the clause's plain text through executive reinterpretation rather than the amendment process the constitution itself specifies. They bear the cost of watching the constraint they organized their political identity around be redefined out from under them without a vote.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, strict_pacifist_constituencies, payer,
    organized, generational, constrained, national).

% South Korea, China, and other states with historical memory of Japanese militarism experience the gradual expansion of 'minimum necessary' capacity as erosion of a postwar settlement they relied on for regional stability assurances. They have no vote in Japan's internal constitutional interpretation and can only respond diplomatically or through their own military buildups.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, regional_neighbors_wary_of_remilitarization, payer,
    moderate, generational, constrained, regional).

% Scholars committed to constitutional textualism bear a credibility cost: the inherent-right reading requires treating explicit, categorical language ('land, sea, and air forces, as well as other war potential, will never be maintained') as implicitly qualified by an unwritten sovereignty doctrine. They can publish and litigate but cannot force amendment or reinterpretation reversal.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, constitutional_textualist_scholars, payer,
    moderate, civilizational, constrained, national).

% Has repeatedly declined to rule squarely on SDF constitutionality (invoking political question doctrine in cases like Sunagawa and Naganuma), leaving the inherent-right reading uncontested by judicial review. Its abstention is itself a structural feature that lets the executive's interpretation stand unchallenged.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, supreme_court_of_japan, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_9_war_renunciation__inherent_right_reading, japanese_government_executive).
narrative_ontology:fixing_cost_class(article_9_war_renunciation__inherent_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides Japan with a legally cognizable basis for organized territorial defense and alliance participation without requiring the two-thirds parliamentary supermajority and national referendum that formal constitutional amendment demands — coordinating security provision within an amendment-resistant constitutional structure.
% TRANSFER_FUNCTION: Moves interpretive authority over the constitution's most contested clause from the amendment process (requiring broad democratic consensus) to the executive and its Cabinet Legislation Bureau, and moves fiscal and political capital toward SDF expansion and defense procurement, at the expense of the pacifist settlement's textual integrity and neighboring states' stability assurances.
% ABSENT_VOICES: Regional neighbors with direct historical stake in Japanese remilitarization have no formal role in Japan's internal constitutional interpretation process. Domestic pacifist constituencies have electoral voice but have not been able to force a referendum on the interpretation itself, since reinterpretation bypasses the amendment mechanism where their vote would matter most.
% DISAPPEARANCE_RATIONALE: If the inherent-right reading were abandoned in favor of the strict pacifist reading, the SDF's legal foundation would collapse, forcing either disbandment, radical reduction, or formal constitutional amendment — a live political process that has been attempted and failed for decades. The US-Japan alliance's burden-sharing structure would require renegotiation. Conversely, if the reading disappeared in favor of unrestricted normalization, the 'minimum necessary' threshold would vanish entirely, removing the sole textual constraint on defense expansion.
% FOUNDING_PROBLEM: In the immediate postwar period, the Allied occupation and framers sought to prevent Japan from ever again possessing the capacity for aggressive war, while occupation planners and later Cold War strategists recognized Japan needed some capacity to resist invasion and eventually to share regional defense burdens as US strategic priorities shifted toward containment.
% FOUNDING_PROBLEM_CORROBORATION: Cabinet Legislation Bureau opinions and successive LDP administrations attest the founding problem (need for lawful defensive capacity within a peace constitution) remains live and requires this reading. Independent constitutional historians and comparative-law scholars outside government note the postwar drafters' own contemporaneous statements (including Prime Minister Yoshida's 1946 Diet testimony explicitly denying any self-defense exception) support the strict pacifist reading, making the inherent-right reading's genealogy contested rather than settled even among non-beneficiary observers.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__inherent_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__inherent_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__inherent_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_9_war_renunciation__inherent_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__inherent_right_reading, 0.38, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is moderate and rising (0.18 to 0.38) because the reading's cost is not direct material extraction but the erosion of textual constraint through reinterpretation rather than democratic amendment — a slow transfer of constitutional authority from the electorate (via Article 96's supermajority-and-referendum requirement) to the executive branch. Suppression is moderate (0.42 at endpoint) reflecting the Supreme Court's persistent refusal to adjudicate SDF constitutionality directly (political question doctrine), which forecloses judicial correction as an exit valve for textualist challengers. Theater ratio is moderate-rising (0.40) because a substantial share of the 'minimum necessary' framing has become performative — reclassifying carriers as 'helicopter destroyers,' for instance — while the underlying capability expansion proceeds regardless of the label. Accessibility collapse (0.45) and resistance (0.50) are both moderate: unlike a mountain, real alternatives (formal amendment, judicial invalidation) remain theoretically available and have been actively pursued by pacifist constituencies and scholars, they have simply not succeeded.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive and the SDF are structural beneficiaries: the reading grants them the legal and institutional basis to exist, expand, and receive sustained procurement funding without the political cost of amendment. The US alliance planning apparatus benefits similarly by inheriting a capable partner without renegotiating the treaty framework. Strict pacifist constituencies and textualist scholars are targets: their preferred textual reading has been organizationally sidelined for seventy years despite never being formally repudiated by amendment. Regional neighbors are targets at one remove — they bear the stability cost of gradual remilitarization without any voice in the interpretive process that produces it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to reconcile a categorical anti-militarism text with the practical need for territorial defense) was live in 1954 and arguably remains live given persistent regional security pressures (North Korea, China's military modernization) — this is not a pure zombie mandate. However, the founding problem's SCOPE has drifted: the original CLB rationale was framed narrowly around minimal territorial defense, and 'minimum necessary' has proven elastic enough to accommodate steadily expanding capability without the threshold ever being tested to failure. Classifying this as tangled_rope rather than snare respects the genuine coordination function (an amendment-resistant constitution needed SOME mechanism to authorize any defensive capacity at all) while registering the asymmetric extraction (democratic voice bypassed, textualist commitments overridden) that the same interpretive mechanism produces.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    drafting_intent_ambiguity,
    'Did the 1946 framers (including MacArthur''s occupation staff and the Japanese Diet) intend Article 9 to permit any defensive military capacity, or did they intend categorical demilitarization later revised for Cold War strategic necessity?',
    'Comparative analysis of contemporaneous drafting records, including Prime Minister Yoshida''s 1946 Diet testimony explicitly disclaiming any self-defense exception, against later Cabinet Legislation Bureau reinterpretations issued after US strategic priorities shifted with the Korean War and the founding of the National Police Reserve in 1950.',
    'If drafting intent was categorical and the inherent-right reading is a later strategic accommodation rather than a discovery of pre-existing textual meaning, the reading''s legitimacy rests on functional necessity rather than interpretive fidelity, strengthening the case that this constraint is closer to tangled_rope (coordination dressed as textual discovery) than a straightforward good-faith reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drafting_intent_ambiguity, empirical, 'Whether the inherent-right reading reflects original drafting intent or a later Cold War-driven reinterpretation.').

omega_variable(
    judicial_abstention_as_structural_choice,
    'Is the Supreme Court of Japan''s persistent refusal to rule on SDF constitutionality a neutral application of political question doctrine, or a structural accommodation that allows the executive''s inherent-right reading to stand unchallenged by design?',
    'Comparative analysis of how the Court has handled other separation-of-powers political questions versus its specific handling of Article 9 cases (Sunagawa 1959, Naganuma 1976, and subsequent SDF deployment challenges), including examination of whether the doctrine''s application here is consistent with its application elsewhere in Japanese constitutional jurisprudence.',
    'If abstention is a structural accommodation rather than neutral doctrine, the suppression metric understates the constraint''s actual coercive architecture — judicial review as an exit valve for textualist challengers is foreclosed by institutional design, not incidental restraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_abstention_as_structural_choice, conceptual, 'Whether judicial non-review is neutral doctrine or a structural feature protecting the inherent-right reading from challenge.').

omega_variable(
    minimum_necessary_elasticity,
    'Is there a principled, non-arbitrary content to ''minimum necessary for defense'' that could in practice constrain SDF expansion, or is the threshold infinitely elastic to whatever capability the executive judges strategically necessary at a given moment?',
    'Track whether any proposed SDF capability (e.g., long-range counterstrike missiles adopted in 2022, aircraft carrier-capable destroyers) has ever been rejected by the CLB or Diet as exceeding the threshold, versus the historical pattern of the threshold accommodating every proposed expansion.',
    'If no capability has ever been rejected as exceeding ''minimum necessary,'' the threshold is performative rather than substantively limiting, which would push the theater_ratio and extractiveness trajectory higher than currently authored and would strengthen a case for reclassification toward snare in future measurement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minimum_necessary_elasticity, empirical, 'Whether the ''minimum necessary'' threshold has ever substantively constrained defense expansion or has proven infinitely accommodating.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__inherent_right_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1954, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1954, 0.25).
narrative_ontology:measurement(arti_tr_t1970, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1970, 0.28).
narrative_ontology:measurement(arti_tr_t1990, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1990, 0.32).
narrative_ontology:measurement(arti_tr_t2005, article_9_war_renunciation__inherent_right_reading, theater_ratio, 2005, 0.35).
narrative_ontology:measurement(arti_tr_t2015, article_9_war_renunciation__inherent_right_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(arti_tr_t2024, article_9_war_renunciation__inherent_right_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(arti_be_t1954, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1954, 0.18).
narrative_ontology:measurement(arti_be_t1970, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1970, 0.22).
narrative_ontology:measurement(arti_be_t1990, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1990, 0.27).
narrative_ontology:measurement(arti_be_t2005, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2005, 0.3).
narrative_ontology:measurement(arti_be_t2015, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2015, 0.34).
narrative_ontology:measurement(arti_be_t2024, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2024, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1954, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1954, 0.3).
narrative_ontology:measurement(arti_su_t1970, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1970, 0.33).
narrative_ontology:measurement(arti_su_t1990, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(arti_su_t2005, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2005, 0.37).
narrative_ontology:measurement(arti_su_t2015, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2015, 0.4).
narrative_ontology:measurement(arti_su_t2024, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__inherent_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation__strict_pacifist_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation__collective_self_defense_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposing the natural-language 'Article 9 war renunciation' kernel per the ε-invariance principle: strict_pacifist_reading (categorical prohibition, ε near-mountain from its own lights), inherent_right_reading (this story — proportionality threshold, moderate tangled_rope extraction), and collective_self_defense_reading (extends this reading's inherent-right premise to ally defense, expected higher ε due to greater contestation and 2015 legislation controversy). The inherent_right_reading is structurally upstream of collective_self_defense_reading — the latter cannot exist without first establishing this reading's core premise that Article 9 does not eliminate the inherent right to self-defense. It stands in direct logical tension with strict_pacifist_reading over what the categorical text ('never be maintained') actually forecloses.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
