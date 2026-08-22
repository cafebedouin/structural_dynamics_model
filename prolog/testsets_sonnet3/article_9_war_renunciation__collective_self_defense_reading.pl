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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: article_9_war_renunciation__collective_self_defense_reading
 *   human_readable: Article 9 — Collective Self-Defense (2014 Reinterpretation) Reading
 *   domain: Constitutional Law / Security Policy / Institutional Legitimacy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the Article 9 kernel: the
 *   collective self-defense reading crystallized by the 2014 cabinet
 *   resolution and 2015 Peace and Security Legislation, which holds that
 *   Japan's constitutionally 'inherent right of self-defense' extends to
 *   collective self-defense of allies when a 'survival-threatening situation'
 *   is certified, even absent a direct armed attack on Japanese territory.
 *   This is a distinct constraint from the inherent_right_reading (which
 *   bounds the inherent right to Japan's own territorial defense) and from
 *   the strict_pacifist_reading (which treats any standing force as
 *   categorically prohibited by the 'never be maintained' clause). Each
 *   reading has its own ε, its own beneficiary/victim structure, and its own
 *   classification; this file does not average across them.
 *
 * KEY AGENTS:
 *   - executive_cabinet: agenda_setter (institutional/arbitrage) — administers the reinterpretation and certifies triggering conditions
 *   - us_alliance_planners: beneficiary (institutional/mobile) — gains a more capable, legally flexible ally
 *   - defense_industrial_base: beneficiary (organized/mobile) — gains from expanded mission-driven procurement
 *   - self_defense_force_personnel_deployed_overseas: payer (moderate/constrained) — bears the concrete combat-risk expansion
 *   - narrow_inherent_right_constituency: payer (moderate/trapped) — loses the stability of the narrower doctrine it relied upon
 *   - pacifist_civil_society: excluded (organized/trapped) — objected loudly but was not a party to the reinterpretation
 *   - constitutional_courts: observer (institutional/analytical) — has structurally declined to adjudicate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__collective_self_defense_reading, 0.58).
domain_priors:suppression_score(article_9_war_renunciation__collective_self_defense_reading, 0.47).
domain_priors:theater_ratio(article_9_war_renunciation__collective_self_defense_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__collective_self_defense_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__collective_self_defense_reading, "Article 9 — Collective Self-Defense (2014 Reinterpretation) Reading").
narrative_ontology:topic_domain(article_9_war_renunciation__collective_self_defense_reading, "Constitutional Law / Security Policy / Institutional Legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__collective_self_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__collective_self_defense_reading, '46aaac26-871a-4a99-8278-476cf133b295').
narrative_ontology:cs_kernel_codification('46aaac26-871a-4a99-8278-476cf133b295', fixed_text).
narrative_ontology:cs_authority_grounding('46aaac26-871a-4a99-8278-476cf133b295', extraction).
narrative_ontology:cs_interpretation_layer_present('46aaac26-871a-4a99-8278-476cf133b295').
narrative_ontology:cs_reading_relation('46aaac26-871a-4a99-8278-476cf133b295', article_9_war_renunciation__inherent_right_reading, influences).
narrative_ontology:cs_reading_relation('46aaac26-871a-4a99-8278-476cf133b295', article_9_war_renunciation__strict_pacifist_reading, forecloses).
narrative_ontology:cs_axiom('46aaac26-871a-4a99-8278-476cf133b295', foundational, inherent_right_encompasses_collective_defense).
narrative_ontology:cs_axiom_status(inherent_right_encompasses_collective_defense, holdable).
narrative_ontology:cs_axiom_grounding('46aaac26-871a-4a99-8278-476cf133b295', inherent_right_encompasses_collective_defense, conventional).
narrative_ontology:cs_axiom('46aaac26-871a-4a99-8278-476cf133b295', secondary, survival_threatening_situation_justifies_extraterritorial_action).
narrative_ontology:cs_axiom_status(survival_threatening_situation_justifies_extraterritorial_action, holdable).
narrative_ontology:cs_axiom_grounding('46aaac26-871a-4a99-8278-476cf133b295', survival_threatening_situation_justifies_extraterritorial_action, instrumental).
narrative_ontology:cs_reference_frame('46aaac26-871a-4a99-8278-476cf133b295', postwar_territorial_pacifism_settlement).
narrative_ontology:cs_drift_state('46aaac26-871a-4a99-8278-476cf133b295', post_2015_security_legislation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('46aaac26-871a-4a99-8278-476cf133b295', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, executive_cabinet).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, us_alliance_planners).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, defense_industrial_base).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, narrow_inherent_right_constituency).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, pacifist_civil_society).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, self_defense_force_personnel_deployed_overseas).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__collective_self_defense_reading, cabinet_reinterpretation_authority_doctrine).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__collective_self_defense_reading, alliance_burden_sharing_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issued the 2014 cabinet resolution and steered the 2015 Peace and Security Legislation through the Diet, reinterpreting the 'inherent right' language to cover collective self-defense without formal constitutional amendment. Sets the operative doctrine, controls the legal opinion apparatus (Cabinet Legislation Bureau), and administers when the 'survival-threatening situation' trigger is invoked. Bears little personal cost from expansion and gains policy flexibility and alliance leverage.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, executive_cabinet, agenda_setter,
    institutional, generational, arbitrage, national).

% Gain a materially more capable and legally flexible partner able to conduct joint operations, escort US vessels, and participate in collective missions. They lobbied for this reading for over a decade and bear none of the domestic legal risk; their exit option (alliance restructuring, basing elsewhere) remains open regardless of how Japan's courts or public eventually rule.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, us_alliance_planners, beneficiary,
    institutional, generational, mobile, global).

% Domestic and allied defense contractors benefit from expanded procurement, joint development programs, and export-control loosening justified by the broadened mission scope. Their fortunes track directly with how expansively 'survival-threatening situation' is read.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, defense_industrial_base, beneficiary,
    organized, biographical, mobile, national).

% Personnel who joined under decades of exclusively territorial, non-combat doctrine now face deployment into collective-defense missions with live combat risk that did not exist in the institutional bargain they entered under. They cannot litigate their way out of orders once the cabinet has certified a triggering situation; exit means resignation from career and pension, not exemption from the specific mission.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, self_defense_force_personnel_deployed_overseas, payer,
    moderate, biographical, constrained, global).

% Constitutional scholars, opposition parties, and citizens who accepted the postwar settlement on the understanding that 'inherent right' meant strictly territorial self-defense now find the same textual anchor stretched to license collective operations abroad. They have no exit from the reinterpretation short of a constitutional amendment fight or Supreme Court reversal, both structurally difficult; the elastic reading erodes the predictability they relied on.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, narrow_inherent_right_constituency, payer,
    moderate, generational, trapped, national).

% Mass protest movements, war-generation survivors' groups, and peace organizations that hold the strict pacifist or narrow inherent-right reading were the loudest objectors to the 2014-2015 changes but were not parties to the cabinet's reinterpretation process; their objections were registered in polling and demonstrations but did not alter the legal instrument. They remain structurally outside the interpretive authority that decides trigger conditions.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, pacifist_civil_society, excluded,
    organized, generational, trapped, national).

% States with historical memory of Japanese militarism (South Korea, China) view the elastic reading with concern but have no standing within Japan's domestic constitutional process; their objections operate only through diplomacy and regional security dynamics, not through the interpretive mechanism itself.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, regional_neighbors, excluded,
    institutional, generational, constrained, continental).

% The Japanese judiciary has historically declined to rule on the constitutionality of security legislation under the political question doctrine, leaving the cabinet's reinterpretation largely unreviewed. Their continued abstention is itself a structural precondition for this reading's persistence.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_9_war_renunciation__collective_self_defense_reading, executive_cabinet).
narrative_ontology:fixing_cost_class(article_9_war_renunciation__collective_self_defense_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables Japan to participate in collective security arrangements — joint missile defense, escort operations, alliance interoperability — that a strictly territorial reading of Article 9 would foreclose, addressing a genuine gap where Japan's alliance partners and regional security environment had changed substantially since 1947 without constitutional amendment keeping pace.
% TRANSFER_FUNCTION: Moves interpretive authority over the scope of permissible military action from the amendment process (requiring two-thirds Diet approval and popular referendum) to the executive cabinet's reinterpretation power; moves deployment risk from a bounded, self-defense-only exposure onto SDF personnel and moves predictability away from the narrow-reading constituency who structured their political and legal expectations around a stable text.
% ABSENT_VOICES: Pacifist civil society groups and war-generation constituencies who hold the strict pacifist or narrow inherent-right readings were vocal in protest but were not parties to the Cabinet Legislation Bureau's internal reinterpretation process; regional neighbors with direct historical stake in Japanese remilitarization have no standing in the domestic interpretive mechanism at all.
% DISAPPEARANCE_RATIONALE: If the collective self-defense reading were reversed (by cabinet withdrawal, Diet repeal, or judicial invalidation of the 2015 legislation), Japan's alliance commitments to joint operations, escort missions, and integrated missile defense would require renegotiation, US-Japan alliance planning would need to revert to older bilateral-only assumptions, and SDF doctrine and procurement tied to expanded mission sets would need to be unwound — a substantial rearrangement, not a return to an unnoticed status quo.
% FOUNDING_PROBLEM: The postwar founding problem Article 9 addressed was preventing a remilitarized Japan from posing renewed aggressive threat to the region while allowing minimal territorial self-defense. The collective self-defense reading was built to solve a different, later problem: how to make Japan a credible, capable alliance partner as its neighborhood grew more contested (North Korean missiles, Chinese naval expansion) without the political cost of formal amendment.
% FOUNDING_PROBLEM_CORROBORATION: The executive cabinet and US alliance planners attest the alliance-credibility problem is live and growing given regional threat trajectories. Independent constitutional scholars, the Japan Federation of Bar Associations, and opposition Diet members — outside the beneficiary set — attest that the reinterpretation exceeds what any textual or historical reading of 'inherent right' can bear, and that the underlying founding problem (aggressive Japanese remilitarization) was never live in a way that required abandoning the narrower doctrine; they characterize the 2014-2015 process as executive doctrine-shopping rather than a genuine constitutional evolution.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__collective_self_defense_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__collective_self_defense_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__collective_self_defense_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate-high (0.58) and rising over the interval: the doctrine has not merely stabilized at its 2015 boundaries but has been used incrementally to authorize wider joint-operation and procurement decisions each year, consistent with the expected 'elastic, absorbing incremental expansion' structural delta for this reading. Suppression is moderate (0.47) — there is no criminal penalty for opposing the reading, but the executive's control of legal-interpretation machinery and the judiciary's political-question abstention functionally foreclose the ordinary means (litigation, formal amendment) by which the narrower readings could reassert themselves. Theater ratio starts high (0.55) reflecting the initial reliance on elaborate 'three new conditions' legal formalism to dress the reinterpretation in continuity with prior doctrine, then falls slightly as the doctrine normalizes and the formalism becomes less load-bearing. Accessibility collapse (0.4) is moderate rather than severe because the amendment process and electoral politics remain nominally open, even if practically difficult.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive cabinet sits at the beneficiary end: it authored the reinterpretation, controls certification of triggering conditions, and bears essentially no personal cost from mission-scope expansion. US alliance planners and the defense industrial base are structural beneficiaries who did not even need to participate in Japan's domestic legal process to gain from it. SDF personnel and the narrow-reading constituency sit toward the target end: personnel bear literal combat-risk expansion they did not sign up for under the prior doctrine, and the narrow-reading constituency loses the predictability and textual stability it had organized around. Pacifist civil society and regional neighbors are excluded rather than coordinated — their preferences were not incorporated into the reinterpretation mechanism at all, which is different from being a victim of an active transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem this reading answers (alliance credibility amid a changed regional threat environment) is genuinely live by the cabinet's and alliance planners' own account, which is why this is authored as tangled_rope rather than pure snare: there is a real coordination function (interoperability, deterrence) riding alongside the extraction (executive capture of interpretive authority, erosion of the narrower constituency's settled expectations). Classifying this as pure extraction would erase the genuine alliance-security coordination it performs; classifying it as pure coordination would erase the transfer of interpretive power away from the amendment process and onto SDF personnel and the narrow-reading constituency. The tangled_rope label preserves both halves.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cabinet_reinterpretation_legitimacy,
    'Can the constitutional meaning of ''inherent right of self-defense'' be validly expanded by executive cabinet resolution and ordinary Diet legislation, or does such expansion require the Article 96 amendment process (two-thirds Diet approval plus popular referendum)?',
    'A Supreme Court ruling squarely addressing the constitutionality of the 2015 Peace and Security Legislation would resolve this; to date the Court has avoided the question under the political question doctrine, leaving it structurally undecided.',
    'If reinterpretation without amendment is illegitimate, this reading is a snare wearing coordination cover (elastic authority seized rather than granted); if legitimate, the tangled_rope classification''s coordination component is more solidly grounded and the extraction is more clearly the ordinary cost of adaptive governance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cabinet_reinterpretation_legitimacy, conceptual, 'Whether executive reinterpretation of ''inherent right'' without formal amendment is constitutionally valid.').

omega_variable(
    survival_threatening_trigger_elasticity,
    'How elastic is the ''survival-threatening situation'' trigger in practice — will it remain bounded to the narrow scenarios described in 2015 Diet debate, or will it expand to cover an increasing range of contingencies over time?',
    'Track actual invocations and near-invocations of the trigger over the coming decade against the scenarios the government described during the 2015 legislative debate; compare stated scope to used scope.',
    'A tightly bounded trigger supports the tangled_rope reading holding steady; an expanding trigger supports the T17-style extraction-accumulation hypothesis and would justify reclassification toward snare as the coordination cover thins.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survival_threatening_trigger_elasticity, empirical, 'Whether the survival-threatening trigger stays bounded or expands over time.').

omega_variable(
    kernel_framing_alternative,
    'Is the appropriate unit of analysis ''the collective self-defense reading of Article 9'' as a single reinterpretation event (2014-2015), or should it be decomposed further into the cabinet resolution reading versus the subsequent legislative-implementation reading, since the Diet''s 2015 statutory conditions arguably narrowed the cabinet''s initial 2014 formulation?',
    'Compare the 2014 cabinet resolution text against the enacted 2015 statutory ''three new conditions'' for material divergence; if the statute meaningfully narrows the cabinet''s initial claim, they are two distinct readings with two ε values.',
    'If treated as one reading (as authored here), the ε reflects the composite doctrine as currently operative; if decomposed, the cabinet-resolution-only reading would likely carry a higher ε (broader, less constrained) than the statute-implemented reading (some legislative narrowing occurred).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_alternative, conceptual, 'Whether the 2014 cabinet resolution and 2015 statutory implementation should be treated as one reading or two.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__collective_self_defense_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(arti_tr_t4, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 4, 0.52).
narrative_ontology:measurement(arti_tr_t8, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 8, 0.48).
narrative_ontology:measurement(arti_tr_t12, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 12, 0.46).
narrative_ontology:measurement(arti_tr_t16, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 16, 0.45).
narrative_ontology:measurement(arti_tr_t20, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 20, 0.44).
narrative_ontology:measurement(arti_tr_t24, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 24, 0.44).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(arti_be_t4, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(arti_be_t8, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(arti_be_t12, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(arti_be_t16, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 16, 0.53).
narrative_ontology:measurement(arti_be_t20, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(arti_be_t24, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(arti_su_t4, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 4, 0.34).
narrative_ontology:measurement(arti_su_t8, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(arti_su_t12, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 12, 0.41).
narrative_ontology:measurement(arti_su_t16, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 16, 0.43).
narrative_ontology:measurement(arti_su_t20, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(arti_su_t24, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 24, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__collective_self_defense_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_9_war_renunciation__collective_self_defense_reading, 0.12).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation__inherent_right_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation__strict_pacifist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the article_9_war_renunciation kernel. The strict_pacifist_reading treats any standing force as categorically prohibited (near-mountain within its own framework, extremely low tolerance for any military capacity). The inherent_right_reading permits minimum defensive capacity but confines it to direct attack on Japanese territory (a narrower tangled_rope or rope-leaning reading with lower ε than this one). This collective_self_defense_reading extends the inherent right to allied defense under a survival-threatening trigger, producing the widest mission scope and highest ε of the three. Each carries its own extractiveness, its own beneficiary/victim structure, and its own classification; they are linked here rather than merged into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
