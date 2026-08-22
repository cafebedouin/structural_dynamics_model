% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__collective_self_defense_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Article 9 Collective Self-Defense Reading (2014 Cabinet Reinterpretation Regime)
 *   domain: constitutional/security/institutional
 *
 * SUMMARY:
 *   In July 2014 the Japanese Cabinet reinterpreted Article 9 to permit
 *   collective self-defense when Japan's survival is threatened, and the 2015
 *   Peace and Security Legislation operationalized the reading. This story
 *   treats the reading-as-operative-arrangement — the cabinet-held trigger,
 *   the enabling statutes, and the mission approvals that flow through them —
 *   as the constraint under classification. Its interval maps time_point 0 to
 *   the 2014 Cabinet decision and time_point 12 to 2026, spanning the 2015
 *   legislation, the sustained protest cycle against it, and the 2022
 *   strategic-document expansion (counterstrike acquisition, two-percent
 *   budget path). The claim/metric gap is deliberate: the arrangement is
 *   CLAIMED here as tangled_rope on structural grounds (a genuine
 *   alliance-coordination function fused with asymmetric extraction through
 *   trigger elasticity and the bypassed amendment threshold), while the
 *   metrics are authored independently from its observed operation. The
 *   engine computes per-seat classifications; where a computed seat diverges
 *   from this claim, that divergence is the datum. KEY AGENTS (by structural
 *   relationship): - japanese_cabinet_executive: Agenda-setter and primary
 *   beneficiary (institutional/arbitrage) — holds the interpretive pen over
 *   the survival trigger and collects discretionary war powers without
 *   amendment - pacifist_constituency: Primary payer
 *   (moderate/identity_locked) — bears the loss of constitutional stability
 *   relied on across generations, no referendum ever held -
 *   constitutional_scholarship: Payer and analytical observer
 *   (moderate/identity_locked) — doctrinal consensus overridden by cabinet
 *   fiat - sdf_personnel: Payer with secondary beneficiary position
 *   (organized/constrained) — bear deployment risk and contested legality;
 *   collect professional legitimacy and widened missions -
 *   united_states_alliance_command: Secondary beneficiary
 *   (institutional/mobile) — decades of pressure rewarded with an
 *   interoperable partner - supreme_court_of_japan: Observer
 *   (institutional/identity_locked) — six decades of avoidance is the
 *   load-bearing silence - opposition_legislators: Excluded
 *   (organized/constrained) — absent from the interpretive act that defines
 *   the arrangement - defense_industry_contractors: Beneficiary
 *   (organized/mobile) — collect the procurement attached to each expansion -
 *   regional_neighboring_states: Excluded external parties
 *   (institutional/mobile) — bear the security consequences with no seat in
 *   the process
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__collective_self_defense_reading, 0.6).
domain_priors:suppression_score(article_9_war_renunciation__collective_self_defense_reading, 0.58).
domain_priors:theater_ratio(article_9_war_renunciation__collective_self_defense_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__collective_self_defense_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__collective_self_defense_reading, "Article 9 Collective Self-Defense Reading (2014 Cabinet Reinterpretation Regime)").
narrative_ontology:topic_domain(article_9_war_renunciation__collective_self_defense_reading, "constitutional/security/institutional").

domain_priors:requires_active_enforcement(article_9_war_renunciation__collective_self_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__collective_self_defense_reading, 'a4f4d788-7feb-4967-8a01-0a6586dd54d3').
narrative_ontology:cs_kernel_codification('a4f4d788-7feb-4967-8a01-0a6586dd54d3', fixed_text).
narrative_ontology:cs_authority_grounding('a4f4d788-7feb-4967-8a01-0a6586dd54d3', extraction).
narrative_ontology:cs_interpretation_layer_present('a4f4d788-7feb-4967-8a01-0a6586dd54d3').
narrative_ontology:cs_reading_relation('a4f4d788-7feb-4967-8a01-0a6586dd54d3', article_9_war_renunciation__strict_pacifist_reading, forecloses).
narrative_ontology:cs_reading_relation('a4f4d788-7feb-4967-8a01-0a6586dd54d3', article_9_war_renunciation__inherent_right_reading, influences).
narrative_ontology:cs_axiom('a4f4d788-7feb-4967-8a01-0a6586dd54d3', foundational, inherent_right_extends_to_collective_defense).
narrative_ontology:cs_axiom_status(inherent_right_extends_to_collective_defense, holdable).
narrative_ontology:cs_axiom_grounding('a4f4d788-7feb-4967-8a01-0a6586dd54d3', inherent_right_extends_to_collective_defense, deontological).
narrative_ontology:cs_axiom('a4f4d788-7feb-4967-8a01-0a6586dd54d3', secondary, survival_threat_trigger_suffices).
narrative_ontology:cs_axiom_status(survival_threat_trigger_suffices, holdable).
narrative_ontology:cs_axiom_grounding('a4f4d788-7feb-4967-8a01-0a6586dd54d3', survival_threat_trigger_suffices, instrumental).
narrative_ontology:cs_reference_frame('a4f4d788-7feb-4967-8a01-0a6586dd54d3', inherent_collective_defense_baseline).
narrative_ontology:cs_drift_state('a4f4d788-7feb-4967-8a01-0a6586dd54d3', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a4f4d788-7feb-4967-8a01-0a6586dd54d3', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, japanese_cabinet_executive).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, united_states_alliance_command).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, defense_industry_contractors).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, pacifist_constituency).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, constitutional_scholarship).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, sdf_personnel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, sdf_personnel).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the interpretive pen over Article 9: it issued the July 2014 Cabinet decision, defines what counts as a survival-threatening situation, approves SDF mission scopes, and maintains the reading through successive cabinets and enabling legislation. It collects discretionary war powers without ever facing the Article 96 amendment process, and can shift between narrower and broader readings as the threat environment and coalition politics demand. Its costs are electoral exposure, alliance-entanglement risk, and the burden of defending each increment against scholarly and public objection.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, japanese_cabinet_executive, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__collective_self_defense_reading, japanese_cabinet_executive, beneficiary).

% Individual members can resign, but careers, pensions, and professional training are invested in the force. They bear deployment risk, expanded rules of engagement, logistics missions near conflict zones, and the burden that the legality of their every mission is publicly disputed by a large share of the constitutional law community. They simultaneously collect professional legitimacy, budget growth, and a widened mission set that individual self-defense alone would never sustain.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, sdf_personnel, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__collective_self_defense_reading, sdf_personnel, beneficiary).

% Pressed Tokyo for collective self-defense capability for decades and now gains a partner legally able to defend US forces, integrate into joint operations, and share deterrence burdens across the Indo-Pacific. Its alliance network is global, so it can adjust posture or deepen other partnerships if Japan's legal framework shifts, but it has consistently collected value from this reading's operation.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, united_states_alliance_command, beneficiary,
    institutional, generational, mobile, global).

% Citizens, civic organizations, and religious and labor movements whose security expectations, civic identity, and intergenerational commitments were built around the peace clause over seven decades. They bore the costs of the reinterpretation without the referendum the amendment process would have required, and they now bear each absorbed expansion. Leaving the position would mean abandoning a civic identity constituted by constitutional pacifism, so their participation takes the form of protest, litigation, and local referendum campaigns rather than exit.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, pacifist_constituency, payer,
    moderate, generational, identity_locked, national).

% The academic discipline whose interpretive consensus was overridden by cabinet fiat: surveys at the time of the 2014 decision found the overwhelming majority of public-law scholars judged the reinterpretation incompatible with the text. Their doctrinal authority over constitutional meaning is the thing set aside, yet they continue to analyze, testify, and litigate. Their professional identity is bound to textual fidelity, so they cannot simply adopt the cabinet's frame; they carry both the analytical seat and the cost of being overridden.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, constitutional_scholarship, payer,
    moderate, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__collective_self_defense_reading, constitutional_scholarship, observer).

% Has avoided adjudicating the Self-Defense Forces' constitutionality since the 1959 Sunakawa decision, treating the question as a political matter for the elected branches. Its silence is load-bearing for the reading's persistence: a ruling either way would restructure the entire arrangement. The court has institutionally become its avoidance doctrine, and each passing year of non-decision deepens that lock.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, supreme_court_of_japan, observer,
    institutional, generational, identity_locked, national).

% Parties that opposed the reinterpretation and contested the 2015 legislation through every procedural channel available. The interpretive core was taken by Cabinet decision in which they had no vote; they legislated enabling statutes under imposed time limits but do not sit in the interpretive channel that maintains and expands the reading. Their constitutional role as co-architects of the text's meaning was set aside, and they bear the loss without a procedural exit from the arrangement.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, opposition_legislators, excluded,
    organized, biographical, constrained, national).

% Collect expanded procurement tied to the widened mission set: counterstrike missiles, standoff munitions, and a defense budget path toward two percent of GDP. These are diversified conglomerates with global portfolios, so they can shift between civil and defense lines if the legal framework contracts, but the reading's operation has steadily enlarged their addressable market.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, defense_industry_contractors, beneficiary,
    organized, generational, mobile, national).

% China, both Koreas, and Russia bear the security consequences of Japan's expanded military role — forward-deployable logistics, counterstrike capability, alliance integration — but have no seat in Japan's constitutional interpretation and no standing in its domestic contest. They adjust by arming, aligning, and protesting diplomatically; their responses in turn feed the threat assessments that justify the next increment.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, regional_neighboring_states, excluded,
    institutional, generational, mobile, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_9_war_renunciation__collective_self_defense_reading, japanese_cabinet_executive).
narrative_ontology:fixing_cost_class(article_9_war_renunciation__collective_self_defense_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reconciles Japan's alliance obligations under the US-Japan Security Treaty with a constitutional text renouncing war: it supplies a legal framework under which the Self-Defense Forces can defend allied forces and join overseas operations without formal constitutional amendment, and it coordinates Japan's contribution to allied deterrence in a region where no such framework previously existed.
% TRANSFER_FUNCTION: Moves interpretive authority over the war-renunciation clause from the constitutional text and the Article 96 amendment process to the cabinet of the day; moves deployment risk and expanded operational obligations onto Self-Defense Forces personnel; and moves binding security commitments onto the electorate without a referendum.
% ABSENT_VOICES: The strict-pacifist constituency whose constitutional order the reading displaces had no vote in the 2014 Cabinet decision; neighboring states who bear the security consequences are structurally outside the interpretive process; the Supreme Court has never appeared to adjudicate; and the supermajority of voters whose Article 96 referendum right the reinterpretation bypasses were never consulted on the content. Opposition legislators were present for the enabling statutes but absent from the interpretive act itself.
% DISAPPEARANCE_RATIONALE: If the reading were withdrawn overnight, the 2015 enabling legislation would lose its constitutional foundation, overseas deployments and alliance-integration missions would halt or require formal amendment, US-Japan burden-sharing arrangements would need renegotiation, and the procurement programs built on the widened mission set would be stranded. The regional security architecture organized around Japan's expanded role would rearrange.
% FOUNDING_PROBLEM: Under the narrower readings, Japan could not defend allied forces under attack or participate in collective operations, which the government argued created an alliance-interoperability gap and weakened deterrence against a rising China and nuclear-armed North Korea; formal amendment of Article 9 was politically unreachable, so the gap was addressed by reinterpretation.
% FOUNDING_PROBLEM_CORROBORATION: The existence of the deterrence and interoperability gap is corroborated from outside the benefiting parties: opposition parties' own defense platforms acknowledge the threat environment, and allied and neutral states' security assessments independently document the regional deterioration. But the claim that cabinet reinterpretation was the necessary remedy is corroborated by no one outside the benefiting parties — constitutional scholars outside the government overwhelmingly denied that necessity at the time, and they have not changed position.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__collective_self_defense_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__collective_self_defense_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__collective_self_defense_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_9_war_renunciation__collective_self_defense_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__collective_self_defense_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness (0.60) is moderate-high: the arrangement's operation transfers interpretive authority from the text and the amendment process to the cabinet, and each increment (2015 statutes, 2022 counterstrike doctrine, budget expansion) is absorbed without the supermajority-plus-referendum check the constitution prescribes for change of this magnitude. Even assessed by this reading's own lights — which hold the survival trigger to be a real limit and Diet approval requirements to bind — the trigger has never been adjudicated against the cabinet's designation of a contingency, so the limit's force depends entirely on the cabinet's own restraint. Suppression (0.58) is a raw structural property, unscaled by power or scope in the engine's arithmetic: it reflects the procedural force-feeding of the 2015 legislation against scholarly consensus and mass protest, the Cabinet Legislation Bureau's conversion from guardian of the strict readings to instrument of the new one, and the courts' avoidance — not physical coercion. Theater ratio (0.40): the Diet approval mechanisms and the 'survival-threatening' formulation are partly functional legal limits and partly performative framing that lets each expansion be described as unchanged policy; the share of performative maintenance grows as the trigger is never tested. Accessibility collapse (0.52): once the reading is entrenched in legislation and ongoing operations, the formal alternatives — amendment, court ruling, re-reinterpretation — remain open but each is politically prohibitive, so alternatives are substantially but not completely closed. Resistance (0.64): the 2015 protest cycle (SEALDs and allied movements), the scholarly near-consensus of unconstitutionality, citizen litigation, and Okinawan referendum campaigns are sustained and ongoing. The measurement series run on one shared time grid (points 0, 2, 4, 6, 8, 10, 12) with every tracked metric authored at every point; points through 10 are observed history, the terminal points are authored projections for the in-progress year. The rising suppression_requirement series is authored because the story genuinely tracks enforcement-capacity change: the machinery matured from a contested 2014 fiat to entrenched, normalized doctrine.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute differently, and the divergence is structural rather than evaluative. From the cabinet's position the arrangement is a legitimate adaptation of a mid-century clause to a transformed threat environment, executed through the government's established interpretive authority; its arbitrage exit across readings makes the constraint cheap for it to hold. From the pacifist constituency's and the scholarship's positions, the same arrangement operates as the dismantling of a constitutional order without the consent procedure that order prescribes — and their identity-locked exit amplifies what they bear. SDF personnel straddle the gap: mission legitimacy flows to them while legal contestation follows every deployment. The excluded seats sharpen the divergence: opposition legislators and neighboring states experience the arrangement's defining feature — that it was made and is maintained without them — as its most salient fact, while the cabinet experiences that same feature as ordinary executive competence.
 *
 * DIRECTIONALITY LOGIC:
 *   The cabinet sits near the beneficiary end: it collects the discretionary power the arrangement generates, and its arbitrage exit means it can move between narrower and broader readings at will. The US alliance command and defense contractors collect without administering; both hold mobile exit, damping their effective position further toward subsidy. The pacifist constituency and the scholarship sit near the full-target end: they bear the transfer of interpretive authority and the erosion of the stability they organized around, and identity lock amplifies their exposure because exit would cost them the civic and professional selves the constraint is defined against. SDF personnel are mixed — declared victims (deployment risk, contested legality) with a genuine secondary beneficiary position (legitimacy, budget, mission scope) — so their effective position sits between the pure payers and symmetry. The court holds an observer seat whose silence is itself a structural input: no directionality of gain or loss, but its avoidance is what keeps the arrangement's enforcement cost manageable. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms already differentiate these seats, and a per-power-atom override would be too coarse to improve on the derivation here.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the alliance-interoperability and deterrence gap — remains live, so this is not a resolved-mandatrophy case, and mandatrophy_resolved is deliberately not declared. But the arrangement carries a latent mandatrophy signature worth recording: part of its justification was transitional (amendment was politically unreachable, so reinterpretation served as the available bridge), and if formal amendment ever passes, the entire interpretive apparatus becomes vestigial overnight — the trigger, the Cabinet Legislation Bureau's converted role, and the annual approval machinery would persist as performance around a text that no longer needs them. The tangled_rope classification is what prevents mislabeling in both directions: reading the arrangement as a pure coordination mechanism would erase the real extraction (the bypassed amendment threshold, the cabinet's interpretive monopoly, the absorbed expansions); reading it as pure extraction would erase the genuine coordination function (no framework existed before 2014 under which Japan could legally defend allied forces, and allied deterrence integration is real). Both halves are structurally present, actively enforced, and inseparable in the current arrangement — which is the tangled-rope definition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_ambiguity,
    'This constraint is one reading of the kernel article_9_war_renunciation (reading: collective_self_defense_reading). Which reading the authoritative framework ultimately adopts changes the constraint''s entire structure — what would the siblings'' adoption do to this story''s classification?',
    'A Supreme Court ruling on the SDF''s constitutionality, or a formal Article 96 amendment adopting one reading explicitly, would collapse the contest into a single authoritative framework.',
    'Under strict_pacifist_reading the entire deployment enterprise becomes unconstitutional per se and this story''s arrangement dissolves into outright contestation; under inherent_right_reading the survival trigger becomes unnecessary, the elasticity that generates most of this story''s measured extraction disappears, and the classification shifts toward a tighter, lower-extraction profile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_ambiguity, conceptual, 'Committer-frame omega: which reading of the Article 9 kernel is authoritative is unresolved, and each sibling restructures this constraint''s victim set, epsilon, and type.').

omega_variable(
    survival_trigger_elasticity,
    'Is the survival-threatening trigger a genuine limit that some cabinet-designated contingency would actually fail, or an elastic instrument that can absorb any increment the government of the day proposes?',
    'Longitudinal observation of trigger adjudication: does any proposed mission or capability ever fail the cabinet''s own test, or does every proposal pass? Adversarial litigation that forces a court to draw the trigger''s boundary would resolve it directly.',
    'If the trigger genuinely binds, a large share of the measured extraction is coordination cost and the tangled_rope reading is confirmed. If it is fully elastic, the arrangement is cabinet discretion wearing constitutional dress and the classification shifts toward the snare end of the spectrum, with the amendment-bypass as the primary extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survival_trigger_elasticity, empirical, 'Whether the reading''s central limit constrains or merely decorates the cabinet''s discretion.').

omega_variable(
    amendment_bypass_legitimacy,
    'Is cabinet reinterpretation a legitimate mode of constitutional change for a text of this gravity, or does it extract the Article 96 amendment threshold — supermajority plus referendum — from the populace whose protection it is?',
    'Referendum evidence on the content (would the 2014/2015 package have passed a vote?), comparative constitutional theory on executive interpretation versus formal amendment, and the government''s own behavior when amendment became politically feasible.',
    'If the bypass is illegitimate, the reading''s authority structure is extraction-grounded at its core and effective extraction rises across every seat; if legitimate, part of the measured extraction is the price of executive flexibility in a rigid amendment regime, and the payer seats'' claims weaken accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_bypass_legitimacy, conceptual, 'Whether the arrangement''s defining mechanism — change without the prescribed consent procedure — is a legitimate interpretive practice or the extraction itself.').

omega_variable(
    court_avoidance_persistence,
    'Will the Supreme Court continue its six-decade avoidance of the SDF''s constitutionality, or does the accumulating deployment record and citizen litigation eventually force adjudication?',
    'Track the litigation pipeline (deployment challenges, local referendum follow-ons) and the court''s docket behavior as the operational record grows.',
    'A ruling either way restructures the entire enforcement surface overnight — validating the reading collapses the payer seats'' strongest claim, invalidating it dissolves the arrangement. Continued avoidance keeps the suppression requirement elevated and the arrangement''s legitimacy dependent on the cabinet''s interpretive monopoly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(court_avoidance_persistence, empirical, 'Whether the load-bearing judicial silence persists or breaks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__collective_self_defense_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 0, 0.26).
narrative_ontology:measurement(arti_tr_t2, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2, 0.31).
narrative_ontology:measurement(arti_tr_t4, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 4, 0.33).
narrative_ontology:measurement(arti_tr_t6, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 6, 0.34).
narrative_ontology:measurement(arti_tr_t8, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 8, 0.37).
narrative_ontology:measurement(arti_tr_t10, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 10, 0.39).
narrative_ontology:measurement(arti_tr_t12, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 12, 0.4).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(arti_be_t2, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2, 0.5).
narrative_ontology:measurement(arti_be_t4, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(arti_be_t6, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 6, 0.53).
narrative_ontology:measurement(arti_be_t8, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 8, 0.56).
narrative_ontology:measurement(arti_be_t10, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(arti_be_t12, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 12, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(arti_su_t2, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2, 0.53).
narrative_ontology:measurement(arti_su_t4, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 4, 0.55).
narrative_ontology:measurement(arti_su_t6, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 6, 0.55).
narrative_ontology:measurement(arti_su_t8, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 8, 0.56).
narrative_ontology:measurement(arti_su_t10, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 10, 0.57).
narrative_ontology:measurement(arti_su_t12, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 12, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__collective_self_defense_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, strict_pacifist_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, inherent_right_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, article_96_amendment_threshold).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, us_japan_security_treaty).

% DUAL FORMULATION NOTE:
% Article 9 is a single fixed text, but the colloquial label 'the Article 9 constraint' covers three structurally distinct claims — categorical pacifism, minimum individual defense, and collective self-defense under a survival trigger — with different epsilon values, victim sets, and enforcement structures. Per the epsilon-invariance principle they are authored as three stories in one constraint family (the article_9_war_renunciation kernel), linked through network.affects_constraints. The upstream sibling (inherent_right_reading, higher empirical and doctrinal confidence) is the baseline this reading extends; each expansion of this reading erodes the upstream sibling's claim to mark the outer limit of the permissible, which is why the family's victim set includes the narrower readings' dependents.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
