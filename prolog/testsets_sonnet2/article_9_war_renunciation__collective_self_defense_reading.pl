% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__collective_self_defense_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__collective_self_defense_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: article_9_war_renunciation__collective_self_defense_reading
 *   human_readable: Article 9 — Collective Self-Defense (2014 Reinterpretation) Reading
 *   domain: constitutional_law/security_policy
 *
 * SUMMARY:
 *   This story instantiates the collective self-defense reading of the
 *   Article 9 kernel: the 2014 Cabinet reinterpretation and 2015 security
 *   legislation, holding that Japan's inherent right to self-defense extends
 *   to collective self-defense of allies when a 'survival-threatening
 *   situation' arises, even absent direct armed attack on Japan itself. This
 *   is a distinct constraint from the narrower inherent_right_reading
 *   (individual self-defense only, minimum necessary force) and from the
 *   strict_pacifist_reading (categorical prohibition on any armed forces).
 *   Extraction here is measured against THIS reading's own operation — an
 *   elastic, executive-administered standard whose 'survival-threatening'
 *   trigger has no textual anchor and is set by the same body that benefits
 *   from expanding it. The reading's coordination function (alliance
 *   interoperability) is real; its extraction (interpretive authority moved
 *   from amendment process to executive fiat, mission scope subject to
 *   incremental administrative expansion) is what the tangled_rope
 *   classification tracks.
 *
 * KEY AGENTS:
 *   - executive_cabinet_security_apparatus: agenda_setter/beneficiary (institutional/arbitrage) — authored and administers the reinterpretation
 *   - united_states_alliance_planners: beneficiary (institutional/arbitrage) — gains interoperable partner, bears none of the domestic legitimacy cost
 *   - self_defense_forces_personnel: payer (moderate/constrained) — bears expanded deployment risk under a mandate shift they did not choose
 *   - constitutional_stability_reliant_citizens: payer (powerless/trapped) — bears cost of a settlement altered without amendment-track legitimation
 *   - pacifist_constituency: payer/excluded (organized/constrained) — objected through available channels, prevailed procedurally against them
 *   - constitutional_courts: observer (institutional/analytical) — abstains from adjudicating the reinterpretation's validity as a political question
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__collective_self_defense_reading, 0.58).
domain_priors:suppression_score(article_9_war_renunciation__collective_self_defense_reading, 0.47).
domain_priors:theater_ratio(article_9_war_renunciation__collective_self_defense_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__collective_self_defense_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__collective_self_defense_reading, "Article 9 — Collective Self-Defense (2014 Reinterpretation) Reading").
narrative_ontology:topic_domain(article_9_war_renunciation__collective_self_defense_reading, "constitutional_law/security_policy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__collective_self_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__collective_self_defense_reading, '9c24d46f-45c6-47e6-af87-228875ec86e6').
narrative_ontology:cs_kernel_codification('9c24d46f-45c6-47e6-af87-228875ec86e6', fixed_text).
narrative_ontology:cs_authority_grounding('9c24d46f-45c6-47e6-af87-228875ec86e6', extraction).
narrative_ontology:cs_interpretation_layer_present('9c24d46f-45c6-47e6-af87-228875ec86e6').
narrative_ontology:cs_reading_relation('9c24d46f-45c6-47e6-af87-228875ec86e6', article_9_war_renunciation__inherent_right_reading, influences).
narrative_ontology:cs_reading_relation('9c24d46f-45c6-47e6-af87-228875ec86e6', article_9_war_renunciation__strict_pacifist_reading, coexists_with).
narrative_ontology:cs_axiom('9c24d46f-45c6-47e6-af87-228875ec86e6', foundational, inherent_right_extends_to_allied_defense).
narrative_ontology:cs_axiom_status(inherent_right_extends_to_allied_defense, holdable).
narrative_ontology:cs_axiom_grounding('9c24d46f-45c6-47e6-af87-228875ec86e6', inherent_right_extends_to_allied_defense, instrumental).
narrative_ontology:cs_axiom('9c24d46f-45c6-47e6-af87-228875ec86e6', secondary, survival_threatening_situation_as_valid_trigger).
narrative_ontology:cs_axiom_status(survival_threatening_situation_as_valid_trigger, holdable).
narrative_ontology:cs_axiom_grounding('9c24d46f-45c6-47e6-af87-228875ec86e6', survival_threatening_situation_as_valid_trigger, conventional).
narrative_ontology:cs_reference_frame('9c24d46f-45c6-47e6-af87-228875ec86e6', id_1954_individual_self_defense_settlement).
narrative_ontology:cs_drift_state('9c24d46f-45c6-47e6-af87-228875ec86e6', post_2014_reinterpretation_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('9c24d46f-45c6-47e6-af87-228875ec86e6', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, executive_cabinet_security_apparatus).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, united_states_alliance_planners).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, defense_industrial_base).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, constitutional_stability_reliant_citizens).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, self_defense_forces_personnel).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, pacifist_constituency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Cabinet issued the 2014 reinterpretation resolution and shepherded the 2015 security legislation through the Diet without formal constitutional amendment. It sets the operative meaning of Article 9 through cabinet legal bureau opinion rather than the amendment process specified in Article 96, and gains expanded policy discretion, alliance leverage, and defense budget authority from the reading it authored.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, executive_cabinet_security_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__collective_self_defense_reading, executive_cabinet_security_apparatus, beneficiary).

% U.S. defense planners gain a partner able to conduct joint operations, shared logistics, and mutual defense contributions beyond narrow homeland defense. They lobbied for and benefit from the expanded reading without bearing any of Japan's domestic constitutional legitimacy costs; their exit from the arrangement is costless relative to Japan's own stakeholders.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, united_states_alliance_planners, beneficiary,
    institutional, generational, arbitrage, global).

% Domestic and allied defense contractors gain expanded procurement mandates tied to interoperability requirements for collective operations. They did not create the constitutional ambiguity but capture material benefit from its resolution toward expansion.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, defense_industrial_base, beneficiary,
    organized, biographical, mobile, national).

% Personnel who joined under a narrower homeland-defense mandate now face potential deployment to overseas joint operations under the survival-threatening trigger. Their employment contract, training, and professional identity were built around a different legal predicate than the one now governing their deployment orders; exit means abandoning a career path, not a policy preference.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, self_defense_forces_personnel, payer,
    moderate, biographical, constrained, national).

% Ordinary citizens who structured expectations of Japan's non-belligerent international posture around the postwar constitutional settlement bear the cost of a reinterpretation achieved without a popular referendum or Diet supermajority amendment. They cannot exit the jurisdiction's constitutional order and were not the audience the reinterpretation was argued to.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, constitutional_stability_reliant_citizens, payer,
    powerless, generational, trapped, national).

% Organized pacifist civil society groups and opposition parties argued the reinterpretation exceeded any defensible reading of the text and should have required Article 96 amendment. Their objections were litigated, protested, and voted on, but the Cabinet legal bureau's interpretive authority prevailed procedurally; they remain a live but structurally overridden voice.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, pacifist_constituency, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__collective_self_defense_reading, pacifist_constituency, excluded).

% Japanese courts have historically treated the scope of Article 9 as a political question largely unreviewable, declining to adjudicate the reinterpretation's constitutionality directly. Their abstention is itself a structural fact shaping which reading of the kernel prevails in practice.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_9_war_renunciation__collective_self_defense_reading, diffuse).
narrative_ontology:fixing_cost_class(article_9_war_renunciation__collective_self_defense_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables Japan to participate in mutual defense arrangements and coalition operations with allies, addressing a genuine gap where a purely homeland-defense-only posture could not respond to threats against allied forces whose defeat would itself threaten Japan (e.g., disruption of sea lanes, an ally's collapse in a regional conflict).
% TRANSFER_FUNCTION: Moves interpretive authority over the constitution's most contested clause from the amendment process (Diet supermajority plus national referendum, Article 96) to the executive's cabinet legal bureau, and moves deployment risk from a bounded homeland-defense mandate onto Self-Defense Forces personnel and from a narrow alliance-support role onto a mission-scope that can expand incrementally under an elastic 'survival-threatening situation' standard.
% ABSENT_VOICES: The 1946 constitutional framers and the generation that ratified the postwar settlement are not present to attest what 'inherent right' was originally intended to license; regional neighbors (South Korea, China) who have historical stakes in Japan's remilitarization trajectory are excluded from the domestic interpretive process entirely, despite bearing externality risk from mission-scope expansion.
% DISAPPEARANCE_RATIONALE: If the 2014 reinterpretation were reversed and Japan returned strictly to the narrower inherent-right (individual self-defense only) reading, alliance planners would need to renegotiate operational assumptions built since 2015, defense procurement tied to interoperability could stall, and SDF personnel would revert to a bounded mandate — the security apparatus and allied planners would say the world rearranges toward dangerous exposure; pacifist constituents would say the world merely returns to the settlement it never should have left.
% FOUNDING_PROBLEM: Cold War and post-Cold War Japan faced a structural mismatch: constitutional text renouncing war and war potential, alongside a functioning alliance system and regional security environment that assumed active military contribution. The reinterpretation was built to close that gap without the political cost and uncertainty of formal amendment.
% FOUNDING_PROBLEM_CORROBORATION: The Cabinet Legal Bureau and ruling-coalition legal scholars attest the reinterpretation is a valid, necessary closing of a real security gap. Independent constitutional scholars outside the government (including several who resigned advisory positions in protest in 2014-2015) and opposition-aligned legal academics attest the gap was real but the fix bypassed the constitutionally specified amendment mechanism, making the 'founding problem' framing a post-hoc justification for what is structurally an executive-branch reinterpretation achieved without the supermajority and referendum Article 96 requires.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__collective_self_defense_reading, contested).
narrative_ontology:founding_problem_status(article_9_war_renunciation__collective_self_defense_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__collective_self_defense_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_9_war_renunciation__collective_self_defense_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__collective_self_defense_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__collective_self_defense_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_9_war_renunciation__collective_self_defense_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_9_war_renunciation__collective_self_defense_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) reflects that the reinterpretation moved a fundamental question of constitutional scope out of the Article 96 amendment track and into cabinet legal opinion, while claiming continuity with 'inherent right' language it substantially expanded. Suppression (0.47) is moderate rather than severe: courts have not affirmatively blocked challenge but have declined jurisdiction, and dissent is politically live (mass protests in 2015, ongoing opposition party positions) even though it has not reversed the policy. Theater ratio (0.42) is elevated because much of the legal argument is framed as mere clarification of a pre-existing 'inherent right' rather than acknowledged expansion — a performative continuity claim over a substantive shift. Accessibility collapse (0.4) is moderate: the strict pacifist and narrower inherent-right readings remain articulable and held by real political factions, so alternatives have not collapsed, only lost the executive's endorsement. Resistance (0.68) is high, reflecting sustained legal academic, civil society, and opposition party contestation that has not disappeared even after a decade.
 *
 * DIRECTIONALITY LOGIC:
 *   The Cabinet and U.S. alliance planners sit at the beneficiary end: they set or benefit from the reading's operative scope and hold arbitrage-level exit (they are not bound by the domestic legitimacy costs their choice generates). SDF personnel and ordinary citizens sit toward the target end: their operational exposure or their expectation of constitutional stability is what the reinterpretation spends, and their exit options (career-constrained, jurisdictionally trapped) are narrow. The pacifist constituency is a payer with real but insufficient organized power — a coalition-power case, not a powerless-individual case, yet still unable to reverse the reading through available channels.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (alliance interoperability gap under a text renouncing war potential) is contested as either still live (security apparatus) or as having been used to justify a scope expansion beyond what the founding gap required (independent scholars). This is precisely the mandatrophy pattern R5 is built to surface: an arrangement whose original problem may be real but whose administered scope has outgrown the original justification without returning to the amendment process that would re-test its legitimacy. Classifying this as tangled_rope rather than snare acknowledges the genuine coordination function (allies can plan jointly) while refusing to let that function launder the executive capture of interpretive authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    survival_threatening_standard_elasticity,
    'Is the ''survival-threatening situation'' trigger a bounded, judicially cognizable standard, or is it elastic enough to be set by the same executive body that benefits from expanding it, with no meaningful external check?',
    'Track whether any invocation of the standard has been challenged and adjudicated on the merits by a court (rather than dismissed as a political question), and whether the Diet has meaningfully narrowed or specified the standard through subsequent legislation.',
    'If the standard proves genuinely bounded and subject to real external check, this reading is closer to a legitimate coordination mechanism (rope-leaning); if it proves purely executive-discretionary with no binding check, the tangled_rope classification understates the extraction and a snare reading becomes defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survival_threatening_standard_elasticity, empirical, 'Whether the survival-threatening trigger is a real constraint or an empty vessel for executive discretion.').

omega_variable(
    amendment_track_bypass_legitimacy,
    'Does achieving a constitutional-scope change through cabinet reinterpretation rather than the Article 96 amendment process constitute a legitimate exercise of interpretive authority, or a structural evasion of the amendment supermajority and referendum requirements?',
    'Comparative analysis of how other constitutional systems treat executive reinterpretation of foundational rights-and-powers clauses versus formal amendment, and whether Japan''s own constitutional theory (as opposed to political convenience) supports cabinet legal bureau authority over questions of this magnitude.',
    'If reinterpretation-without-amendment is accepted as legitimate constitutional method, the extraction reading softens toward ordinary institutional evolution; if it is illegitimate bypass, the extraction reading hardens and the case for reclassification toward snare strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_track_bypass_legitimacy, conceptual, 'Whether executive constitutional reinterpretation without formal amendment is a legitimate method or a bypass of specified legitimation procedure.').

omega_variable(
    kernel_reading_incommensurability,
    'Are the three readings of the Article 9 kernel (strict pacifist, narrower inherent-right, collective self-defense) genuinely incommensurable positions that cannot be adjudicated by textual analysis alone, or does one reading have decisively superior textual/originalist support that the others merely resist for political reasons?',
    'This is inherently a conceptual/interpretive question rather than an empirical one; it would require a settled theory of Japanese constitutional interpretation (textualism vs. living-document vs. originalism) that does not currently command consensus among Japanese constitutional scholars.',
    'If the readings are genuinely incommensurable, all three remain permanently coexisting constraints with different victim sets, and no reading can claim to have ''resolved'' the kernel. If one reading has decisively superior support, the others should be understood as contested but ultimately weaker claims, which would affect how much weight the corroboration in founding_problem_corroboration should carry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the three kernel readings are permanently coexisting or whether one has superior textual support.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__collective_self_defense_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1954, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 1954, 0.15).
narrative_ontology:measurement(arti_tr_t1991, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 1991, 0.22).
narrative_ontology:measurement(arti_tr_t2003, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2003, 0.28).
narrative_ontology:measurement(arti_tr_t2014, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2014, 0.4).
narrative_ontology:measurement(arti_tr_t2015, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2015, 0.44).
narrative_ontology:measurement(arti_tr_t2022, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2022, 0.46).
narrative_ontology:measurement(arti_tr_t2024, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(arti_be_t1954, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 1954, 0.18).
narrative_ontology:measurement(arti_be_t1991, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 1991, 0.28).
narrative_ontology:measurement(arti_be_t2003, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2003, 0.34).
narrative_ontology:measurement(arti_be_t2014, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2014, 0.52).
narrative_ontology:measurement(arti_be_t2015, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement(arti_be_t2022, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2022, 0.64).
narrative_ontology:measurement(arti_be_t2024, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1954, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 1954, 0.2).
narrative_ontology:measurement(arti_su_t1991, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 1991, 0.25).
narrative_ontology:measurement(arti_su_t2003, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2003, 0.32).
narrative_ontology:measurement(arti_su_t2014, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2014, 0.48).
narrative_ontology:measurement(arti_su_t2015, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2015, 0.53).
narrative_ontology:measurement(arti_su_t2022, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2022, 0.5).
narrative_ontology:measurement(arti_su_t2024, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2024, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation__inherent_right_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation__strict_pacifist_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, us_japan_security_treaty_burden_sharing).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language label 'Article 9' into structurally distinct constraints per the ε-invariance principle: strict_pacifist_reading (categorical prohibition, lowest ε), inherent_right_reading (individual self-defense only, moderate ε), and this collective_self_defense_reading (elastic survival-threatening trigger, highest ε among the three, tangled_rope). Each carries its own beneficiary/victim structure and its own ε assessed by that reading's own lights against the standing arrangement it describes. The three are linked via affects_constraints rather than merged, because averaging or parameterizing across them would violate the requirement that a single constraint have one stable ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
