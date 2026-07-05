% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__collective_self_defense_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Article 9 — Collective Self-Defense (Survival-Threatening Situations) Reading
 *   domain: constitutional_law/security_policy/institutional_legitimacy
 *
 * SUMMARY:
 *   This story instantiates the collective-self-defense reading of the
 *   Article 9 kernel: the 2014-2015 cabinet reinterpretation holding that
 *   Japan's inherent right to self-defense extends to collective self-defense
 *   when an attack on a close ally creates a 'survival-threatening situation'
 *   for Japan, even absent direct attack on Japanese territory. This is a
 *   distinct constraint from the inherent_right_reading (individual
 *   self-defense only) and the strict_pacifist_reading (no armed forces at
 *   all) — the three readings have different beneficiary/victim structures,
 *   different ε trajectories, and different institutional mechanisms of
 *   persistence, and are authored as separate linked stories per the
 *   ε-invariance principle rather than as one story with a measurement
 *   parameter.
 *
 * KEY AGENTS:
 *   - cabinet_legislation_bureau_and_executive: agenda_setter (institutional/arbitrage) — controls the interpretive trigger and can widen it further
 *   - united_states_alliance_planners: beneficiary (institutional/arbitrage) — gains interoperable ally without bearing domestic constitutional cost
 *   - self_defense_force_expansion_advocates: beneficiary/agenda_setter (organized/mobile) — gains mission scope and budget
 *   - constitutional_pacifism_constituency: payer (moderate/constrained) — bears loss of amendment-process protection
 *   - narrower_inherent_right_reading_adherents: payer (organized/trapped) — displaced interpretive position without formal defeat
 *   - sdf_personnel_deployed_overseas: payer (powerless/constrained) — bears physical deployment risk
 *   - supreme_court_of_japan: observer (institutional/analytical) — declines to adjudicate, passively ratifying executive authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__collective_self_defense_reading, 0.58).
domain_priors:suppression_score(article_9_war_renunciation__collective_self_defense_reading, 0.52).
domain_priors:theater_ratio(article_9_war_renunciation__collective_self_defense_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__collective_self_defense_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__collective_self_defense_reading, "Article 9 — Collective Self-Defense (Survival-Threatening Situations) Reading").
narrative_ontology:topic_domain(article_9_war_renunciation__collective_self_defense_reading, "constitutional_law/security_policy/institutional_legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__collective_self_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__collective_self_defense_reading, 'ff48bca6-e065-4221-bded-36efcab47079').
narrative_ontology:cs_kernel_codification('ff48bca6-e065-4221-bded-36efcab47079', fixed_text).
narrative_ontology:cs_authority_grounding('ff48bca6-e065-4221-bded-36efcab47079', extraction).
narrative_ontology:cs_interpretation_layer_present('ff48bca6-e065-4221-bded-36efcab47079').
narrative_ontology:cs_reading_relation('ff48bca6-e065-4221-bded-36efcab47079', article_9_war_renunciation__inherent_right_reading, influences).
narrative_ontology:cs_reading_relation('ff48bca6-e065-4221-bded-36efcab47079', article_9_war_renunciation__strict_pacifist_reading, forecloses).
narrative_ontology:cs_axiom('ff48bca6-e065-4221-bded-36efcab47079', foundational, inherent_right_extends_to_collective_defense).
narrative_ontology:cs_axiom_status(inherent_right_extends_to_collective_defense, holdable).
narrative_ontology:cs_axiom_grounding('ff48bca6-e065-4221-bded-36efcab47079', inherent_right_extends_to_collective_defense, conventional).
narrative_ontology:cs_axiom('ff48bca6-e065-4221-bded-36efcab47079', foundational, survival_threatening_situation_is_valid_trigger_absent_direct_attack).
narrative_ontology:cs_axiom_status(survival_threatening_situation_is_valid_trigger_absent_direct_attack, holdable).
narrative_ontology:cs_axiom_grounding('ff48bca6-e065-4221-bded-36efcab47079', survival_threatening_situation_is_valid_trigger_absent_direct_attack, instrumental).
narrative_ontology:cs_axiom('ff48bca6-e065-4221-bded-36efcab47079', secondary, some_armed_capacity_is_constitutionally_permissible).
narrative_ontology:cs_axiom_status(some_armed_capacity_is_constitutionally_permissible, holdable).
narrative_ontology:cs_axiom_grounding('ff48bca6-e065-4221-bded-36efcab47079', some_armed_capacity_is_constitutionally_permissible, conventional).
narrative_ontology:cs_reference_frame('ff48bca6-e065-4221-bded-36efcab47079', narrow_individual_self_defense_settlement).
narrative_ontology:cs_drift_state('ff48bca6-e065-4221-bded-36efcab47079', post_2015_security_legislation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ff48bca6-e065-4221-bded-36efcab47079', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, cabinet_legislation_bureau_and_executive).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, united_states_alliance_planners).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, self_defense_force_expansion_advocates).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, constitutional_pacifism_constituency).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, narrower_inherent_right_reading_adherents).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, sdf_personnel_deployed_overseas).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__collective_self_defense_reading, japan_us_alliance_interoperability_doctrine).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__collective_self_defense_reading, proactive_contribution_to_peace_policy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reinterpreted the CLB's own long-held position in 2014-2015 to certify that collective self-defense falls within the 'minimum necessary' inherent right, then drafted the 2015 Peace and Security Legislation operationalizing a 'survival-threatening situation' trigger. Controls the interpretive apparatus itself and can adjust the trigger's threshold through cabinet resolution rather than constitutional amendment, giving it durable control over the constraint's elasticity.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, cabinet_legislation_bureau_and_executive, agenda_setter,
    institutional, generational, arbitrage, national).

% Gains a Japan that can now legally participate in joint operations, escort US vessels, and provide mutual defense contributions previously foreclosed by the narrower reading. Bears none of the domestic constitutional cost and can request expanded burden-sharing without needing to argue Japan's direct attack.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, united_states_alliance_planners, beneficiary,
    institutional, generational, arbitrage, global).

% Defense industry, SDF institutional leadership, and pro-normalization political factions who gain expanded mission scope, procurement budgets, and international standing from the reinterpretation. Actively lobby for further elasticity in what counts as 'survival-threatening.'
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, self_defense_force_expansion_advocates, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__collective_self_defense_reading, self_defense_force_expansion_advocates, agenda_setter).

% Citizens, legal scholars, and opposition parties who hold that the postwar settlement's stability depended on a narrow, non-elastic reading of Article 9. They experience the 2015 reinterpretation as a de facto constitutional amendment achieved without the Article 96 amendment process, and their only recourse is electoral or litigation channels that have so far failed to reverse the reinterpretation.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, constitutional_pacifism_constituency, payer,
    moderate, generational, constrained, national).

% Legal scholars and politicians who accept individual self-defense as inherent but explicitly rejected collective self-defense as beyond Article 9's minimum-necessary threshold. Their interpretive position, which had governed policy for decades, was displaced by cabinet reinterpretation rather than defeated through amendment or judicial ruling; they now operate inside a legal order that has foreclosed the position they still hold as correct.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, narrower_inherent_right_reading_adherents, payer,
    organized, generational, trapped, national).

% Individual service members now face deployment risk in joint overseas operations that were not part of the SDF's mission profile when they enlisted under the narrower reading. They bear the direct physical and legal risk of the mission-scope expansion without having consented to the reinterpretation that created it.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, sdf_personnel_deployed_overseas, payer,
    powerless, biographical, constrained, global).

% Has consistently declined to rule on the constitutionality of SDF existence or the 2015 legislation on political-question grounds, leaving the reinterpretation's legal status formally unadjudicated. This non-ruling itself functions as a passive endorsement of executive interpretive authority over the kernel.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, supreme_court_of_japan, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_9_war_renunciation__collective_self_defense_reading, cabinet_legislation_bureau_and_executive).
narrative_ontology:fixing_cost_class(article_9_war_renunciation__collective_self_defense_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows Japan to participate in collective security arrangements and respond to regional threats (e.g., to sea lanes, allied forces, or missile trajectories) that fall short of direct attack on Japanese territory, coordinating defense posture with allies without requiring Japan to wait for a first strike.
% TRANSFER_FUNCTION: Moves interpretive authority over the constitutional pacifism settlement from the amendment process (Article 96, requiring supermajority and referendum) to the executive's cabinet resolution power; moves military risk exposure from a purely territorial-defense SDF to personnel engaged in overseas joint operations; moves alliance burden-sharing costs from the US to Japan incrementally.
% ABSENT_VOICES: The public was not offered a referendum on this specific constitutional question despite Article 96's amendment procedure existing precisely for such changes; large protest movements in 2015 registered objection but had no formal veto point. Neighboring states with historical grievances about Japanese militarism were not party to the domestic reinterpretation process at all.
% DISAPPEARANCE_RATIONALE: If the collective self-defense reading were reversed and Japan reverted to the narrower inherent-right posture, the 2015 Peace and Security Legislation would need to be repealed or radically narrowed, joint operational planning with the US and Australia would contract, and SDF mission scope would shrink back to territorial and individual self-defense — a substantial rearrangement of alliance commitments and defense procurement already underway.
% FOUNDING_PROBLEM: The postwar Article 9 settlement was built to permanently foreclose Japanese remilitarization after imperial aggression; the narrower inherent-right reading later addressed the practical need for territorial self-defense capacity, but by the 2010s policymakers argued that a rising security environment (North Korean missiles, Chinese naval expansion) created gaps that the narrow individual-self-defense-only reading could not address.
% FOUNDING_PROBLEM_CORROBORATION: The Cabinet Legislation Bureau and ruling-coalition security planners attest the security-gap problem is live and the reinterpretation is a necessary functional response. Independent constitutional scholars, the Japan Federation of Bar Associations, and opposition-aligned legal historians attest that the reinterpretation manufactured a new problem (interpretive instability) rather than solving the original one, and that the amendment process — not cabinet resolution — was constitutionally required; this corroboration comes from outside the beneficiary set.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__collective_self_defense_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__collective_self_defense_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__collective_self_defense_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises from 0.28 (1990, when the narrow reading still dominated policy debate) to 0.58 (2024) tracking the incremental widening of what counts as a 'survival-threatening situation' — each subsequent security white paper and legislative amendment has slightly expanded the trigger's scope without further public deliberation. Theater ratio rises in parallel (0.2 to 0.44): a substantial portion of Diet debate and public justification now performs a continuity narrative ('this was always the inherent right') that papers over the discontinuity from the narrower reading actually held for six decades. Suppression is moderate-high (0.52) — not physical coercion, but the structural suppression of the amendment pathway: reinterpretation bypasses the supermajority-and-referendum process the Constitution actually specifies for this kind of change, and the Supreme Court's political-question abstention removes judicial review as a check.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive/CLB sits at the beneficiary end — it authored and controls the interpretive elasticity and can adjust the trigger without further constitutional process. US alliance planners and SDF-expansion advocates are structural beneficiaries with high exit/mobility (they are not bound by the reading's internal logic; they benefit from whatever reading permits greater capability). The pacifist constituency and narrower-reading adherents are targets: their prior settled expectation (either full renunciation or narrow individual self-defense) was displaced by executive fiat, and their exit options are constrained (electoral) or trapped (the interpretive position itself has no forum for vindication now that the CLB does not hold it). SDF personnel are powerless payers — individuals bearing the compounding real-world risk of policy decisions made by seats they do not occupy.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (not snare) recognizes a genuine coordination function: Japan's alliance posture does address real regional security dynamics (missile threats, naval expansion) that the 1947 settlement did not anticipate. But the coordination gain is captured through a specific mechanism — cabinet reinterpretation rather than Article 96 amendment — that shifts costs onto the pacifist constituency and the narrower-reading adherents without their consent through the process the Constitution specifies for exactly this kind of change. Calling it a pure snare would deny the real security coordination it accomplishes; calling it a pure rope would launder the process bypass as if it carried no victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reinterpretation_vs_amendment_legitimacy,
    'Can a cabinet reinterpretation of ''inherent right to self-defense'' legitimately accomplish what the Constitution''s Article 96 amendment process was designed to gate, or does this reading constitute a de facto amendment achieved without the required supermajority and referendum?',
    'A Supreme Court ruling on the merits (rather than political-question abstention) addressing whether the 2015 legislation exceeds the CLB''s prior settled interpretation; alternatively, a future Diet vote to formally amend Article 9 would either ratify or bypass the question.',
    'If reinterpretation without amendment is illegitimate, this reading''s persistence depends entirely on continued judicial abstention and executive control of the interpretive apparatus — closer to snare. If reinterpretation within ''inherent right'' is a legitimate constitutional method, the coordination function stands on firmer ground — closer to rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reinterpretation_vs_amendment_legitimacy, conceptual, 'Whether cabinet reinterpretation is a legitimate amendment-equivalent process or a bypass of the constitutionally specified amendment path.').

omega_variable(
    trigger_elasticity_ceiling,
    'Is there a principled ceiling to what counts as a ''survival-threatening situation,'' or does the trigger''s definition remain entirely within executive discretion with no external constraint?',
    'Track whether future security legislation cites objective, externally verifiable criteria for the trigger versus purely executive judgment; observe whether any invocation of the trigger is successfully challenged in any forum.',
    'An unbounded trigger supports classification drift toward snare over time (extraction accumulating via interpretive creep); a bounded, judicially or legislatively constrained trigger supports continued tangled_rope classification with stable, non-accumulating extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trigger_elasticity_ceiling, empirical, 'Whether the survival-threatening-situation trigger has any external limiting principle or is purely executive-discretionary.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Given the same constitutional text (''the right of belligerency of the state will not be recognized'' / ''never be maintained''), which of the three readings (strict pacifist, narrow inherent-right, collective self-defense) represents the text''s actual constraint, and is the selection among them itself a political rather than legal question?',
    'This is inherently a framing question the courts have declined to resolve; a definitive answer would require either a Supreme Court merits ruling breaking from six decades of political-question abstention, or a formal constitutional amendment settling the text directly.',
    'The choice of reading determines which population is classified as victim versus beneficiary of the same underlying text — the collective self-defense reading and the inherent right reading, while sharing the same textual kernel, produce materially different extraction and victim profiles.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Committer-frame ambiguity: which reading of the Article 9 kernel is authoritative is itself contested and unresolved by any adjudicating body.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__collective_self_defense_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1990, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(arti_tr_t1999, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 1999, 0.25).
narrative_ontology:measurement(arti_tr_t2008, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2008, 0.3).
narrative_ontology:measurement(arti_tr_t2014, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2014, 0.38).
narrative_ontology:measurement(arti_tr_t2015, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2015, 0.42).
narrative_ontology:measurement(arti_tr_t2020, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2020, 0.43).
narrative_ontology:measurement(arti_tr_t2024, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2024, 0.44).

% Extraction over time
narrative_ontology:measurement(arti_be_t1990, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 1990, 0.28).
narrative_ontology:measurement(arti_be_t1999, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 1999, 0.34).
narrative_ontology:measurement(arti_be_t2008, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2008, 0.4).
narrative_ontology:measurement(arti_be_t2014, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2014, 0.5).
narrative_ontology:measurement(arti_be_t2015, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement(arti_be_t2020, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2020, 0.56).
narrative_ontology:measurement(arti_be_t2024, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1990, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 1990, 0.3).
narrative_ontology:measurement(arti_su_t1999, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 1999, 0.35).
narrative_ontology:measurement(arti_su_t2008, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2008, 0.38).
narrative_ontology:measurement(arti_su_t2014, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2014, 0.46).
narrative_ontology:measurement(arti_su_t2015, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2015, 0.52).
narrative_ontology:measurement(arti_su_t2020, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2020, 0.51).
narrative_ontology:measurement(arti_su_t2024, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2024, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__collective_self_defense_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_9_war_renunciation__collective_self_defense_reading, 0.12).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, inherent_right_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, strict_pacifist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the article_9_war_renunciation kernel. inherent_right_reading (narrower, individual self-defense only) is the historically dominant prior reading this constraint displaced via 2014-2015 cabinet reinterpretation — the two share victim/beneficiary overlap but diverge sharply in ε trajectory and trigger scope. strict_pacifist_reading is the textually strictest reading, foreclosed by both other readings' premise that some armed capacity is constitutionally permissible. Each carries its own ε, stakeholders, and classification per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
