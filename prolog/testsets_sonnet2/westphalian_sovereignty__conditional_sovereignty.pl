% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__conditional_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__conditional_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__conditional_sovereignty
 *   human_readable: Responsibility to Protect / Conditional Sovereignty Doctrine
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the 'conditional sovereignty' reading of the
 *   Westphalian sovereignty kernel: the claim that sovereignty is not an
 *   unconditional grant but a responsibility, and that systematic human
 *   rights violations — genocide, ethnic cleansing, crimes against humanity,
 *   war crimes — trigger legitimate external intervention (formalized in the
 *   Responsibility to Protect doctrine, adopted at the 2005 UN World Summit).
 *   The doctrine emerged from the perceived failures of strict
 *   non-intervention in Rwanda (1994) and Srebrenica (1995). Its actual
 *   operation since 2005 shows a moderate, selectively-applied extraction:
 *   the doctrine constrains targeted states' autonomy and is invoked
 *   asymmetrically, correlating more with the invoking coalition's strategic
 *   interests and the target's lack of a P5 protector than with the severity
 *   of violations alone (Libya 2011 vs. Syria's chemical-weapons use vs.
 *   Myanmar's Rohingya campaign).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__conditional_sovereignty, 0.38).
domain_priors:suppression_score(westphalian_sovereignty__conditional_sovereignty, 0.42).
domain_priors:theater_ratio(westphalian_sovereignty__conditional_sovereignty, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, extractiveness, 0.38).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__conditional_sovereignty, snare).
narrative_ontology:human_readable(westphalian_sovereignty__conditional_sovereignty, "Responsibility to Protect / Conditional Sovereignty Doctrine").
narrative_ontology:topic_domain(westphalian_sovereignty__conditional_sovereignty, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(westphalian_sovereignty__conditional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__conditional_sovereignty, '1d786c38-faef-460b-9258-33ab3d3403ed').
narrative_ontology:cs_kernel_codification('1d786c38-faef-460b-9258-33ab3d3403ed', distributed).
narrative_ontology:cs_authority_grounding('1d786c38-faef-460b-9258-33ab3d3403ed', distributed).
narrative_ontology:cs_reading_relation('1d786c38-faef-460b-9258-33ab3d3403ed', westphalian_sovereignty__absolute_sovereignty, forecloses).
narrative_ontology:cs_reading_relation('1d786c38-faef-460b-9258-33ab3d3403ed', westphalian_sovereignty__graduated_sovereignty, influences).
narrative_ontology:cs_axiom('1d786c38-faef-460b-9258-33ab3d3403ed', foundational, sovereignty_conditioned_on_responsibility_to_protect).
narrative_ontology:cs_axiom_status(sovereignty_conditioned_on_responsibility_to_protect, holdable).
narrative_ontology:cs_axiom_grounding('1d786c38-faef-460b-9258-33ab3d3403ed', sovereignty_conditioned_on_responsibility_to_protect, conventional).
narrative_ontology:cs_axiom('1d786c38-faef-460b-9258-33ab3d3403ed', secondary, systematic_atrocity_threshold_triggers_external_authority).
narrative_ontology:cs_axiom_status(systematic_atrocity_threshold_triggers_external_authority, holdable).
narrative_ontology:cs_axiom_grounding('1d786c38-faef-460b-9258-33ab3d3403ed', systematic_atrocity_threshold_triggers_external_authority, instrumental).
narrative_ontology:cs_reference_frame('1d786c38-faef-460b-9258-33ab3d3403ed', post_1945_charter_non_intervention_norm).
narrative_ontology:cs_drift_state('1d786c38-faef-460b-9258-33ab3d3403ed', post_r2p_2005_world_summit, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('1d786c38-faef-460b-9258-33ab3d3403ed', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, intervention_advocacy_coalitions).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, permanent_security_council_members).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, regional_hegemons_with_intervention_capacity).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, targeted_state_governments).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, civilian_populations_in_contested_interventions).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, weak_states_without_veto_protection).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__conditional_sovereignty, responsibility_to_protect_doctrine).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__conditional_sovereignty, human_rights_as_universal_constraint_on_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% NGOs, human-rights bodies, and diplomatic coalitions that build the evidentiary and normative case for intervention, frame violations as triggering the doctrine, and lobby the Security Council or coalitions of the willing to act. They set the interpretive terms of when the threshold is crossed but bear none of the military, economic, or civilian costs of an intervention gone wrong.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, intervention_advocacy_coalitions, agenda_setter,
    organized, generational, analytical, global).

% The five veto powers control whether the doctrine is invoked at all: they can authorize intervention against a target state or shield an ally by veto, regardless of the underlying facts on the ground. They apply the standard selectively — this doctrine has never overridden a P5 member's own sovereignty or that of its close clients.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, permanent_security_council_members, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__conditional_sovereignty, permanent_security_council_members, agenda_setter).

% States with the military and economic capacity to actually mount an intervention gain a legitimizing vocabulary for actions that also serve strategic, resource, or regional-influence interests. The doctrine gives cover for interventions they might undertake anyway.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, regional_hegemons_with_intervention_capacity, beneficiary,
    powerful, generational, mobile, continental).

% Governments accused of systematic violations face suspended sovereignty claims, sanctions, arms embargoes, or armed intervention. They cannot exit the international system that adjudicates the claim against them, and the accusation itself — contested or not — degrades their diplomatic standing and internal legitimacy.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, targeted_state_governments, payer,
    moderate, biographical, trapped, national).

% Populations in the targeted state bear the direct costs of both the original violations and any intervention response — including strikes, occupation, destabilization, or prolonged civil conflict that the intervention sometimes causes or worsens. They have no voice in whether intervention is invoked or how it is conducted.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, civilian_populations_in_contested_interventions, payer,
    powerless, immediate, trapped, local).

% Small or non-aligned states without a great-power patron are structurally more exposed to having the doctrine invoked against them than states with a protector on the Security Council, even for comparable levels of internal violation. Their sovereignty is conditional in practice in a way that P5 clients' sovereignty is not.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, weak_states_without_veto_protection, payer,
    powerless, generational, constrained, national).

% Study the doctrine's invocation pattern, compare declared thresholds against actual triggering events, and document the asymmetry between formal universality and selective application.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, international_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalian_sovereignty__conditional_sovereignty, diffuse).
narrative_ontology:fixing_cost_class(westphalian_sovereignty__conditional_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared normative vocabulary for the international community to respond collectively to mass atrocities — genocide, ethnic cleansing, crimes against humanity — where inaction would otherwise be the default given the non-intervention norm.
% TRANSFER_FUNCTION: Moves the presumption of non-interference away from accused states and toward the intervening coalition; in practice moves military, economic, and reputational costs onto targeted governments and their civilian populations, while moving legitimacy and geopolitical leverage toward states capable of intervening.
% ABSENT_VOICES: The targeted state's own population rarely has a direct voice in whether intervention is invoked on their behalf; the invocation decision runs through the Security Council and advocacy networks, not through mechanisms accountable to the people the doctrine claims to protect.
% DISAPPEARANCE_RATIONALE: Advocacy coalitions and human-rights bodies would say the world rearranges catastrophically — mass atrocities would proceed with even less external check. Targeted states and skeptical scholars would say the world barely changes in practice, because selective P5-gated invocation already makes the doctrine's actual deterrent and interventionist force far weaker than its stated universality implies.
% FOUNDING_PROBLEM: The international system's post-1945 non-intervention norm left it structurally unable to respond to genocide and mass atrocity inside sovereign borders — Rwanda and Srebrenica are the canonical failures the doctrine was built to answer.
% FOUNDING_PROBLEM_CORROBORATION: UN-affiliated review panels and independent legal scholars outside the advocacy coalitions attest the doctrine has been invoked inconsistently — Libya 2011 as an invocation, Syria and Myanmar as non-invocations despite comparable severity — and that its selective application is documented by scholars with no stake in either advocacy or targeted-state defense.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__conditional_sovereignty, contested).
narrative_ontology:founding_problem_status(westphalian_sovereignty__conditional_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__conditional_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(westphalian_sovereignty__conditional_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__conditional_sovereignty, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__conditional_sovereignty_tests).
:- end_tests(westphalian_sovereignty__conditional_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.18 pre-doctrine to a peak of 0.40 around the 2011 Libya intervention — the doctrine's most consequential invocation — then settles near 0.38 as invocation becomes more contested and less frequently acted upon (Syria, Myanmar non-invocations despite comparable severity). Theater ratio climbs steadily: the doctrine is invoked rhetorically far more often than it is acted upon, and its declaratory function increasingly exceeds its operational function post-2011 as major powers grow wary of the precedent Libya set. Suppression tracks the doctrine's coercive apparatus — sanctions regimes, arms embargoes, ICC referrals — which intensified through the 2000s and plateaued as enforcement capacity met political limits.
 *
 * DIRECTIONALITY LOGIC:
 *   Advocacy coalitions and P5 members sit near the beneficiary end: they set or gate the interpretive terms without bearing intervention costs, and P5 members retain veto-based immunity for themselves and their clients. Targeted governments and, more severely, civilian populations in contested interventions sit near the full-target end: trapped, bearing the direct costs of both the underlying violations and any intervention response, with no structural voice in the invocation decision. Weak states without a P5 patron experience materially higher effective exposure to the doctrine than similarly-situated states with a protector, despite the doctrine's formally universal language — this is the asymmetry the doctrine's own defenders and critics both document.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — international paralysis in the face of genocide — remains genuinely live (Rwanda, Srebrenica, and more recent atrocities were not hypothetical), so this is not simple mandatrophy where the function has vanished. But the doctrine's persistence as a UNIVERSAL claim, applied in practice only where P5 interests align, is exactly the divergence the framework should flag: the claimed function (universal atrocity-response) and the measured operation (selective, veto-gated, strategically-inflected extraction of legitimacy and leverage) diverge substantially. Classifying this as snare rather than rope prevents mislabeling a selectively-enforced power instrument as pure coordination; classifying it as anything other than mountain or pure extraction (it retains genuine coordination value where invoked in good faith) prevents the opposite error of dismissing the doctrine's real atrocity-prevention function entirely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_norm_vs_power_instrument,
    'Is R2P/conditional sovereignty a genuine emerging international norm constraining state conduct, or is it primarily a discretionary legitimation vocabulary that powerful states invoke when convenient and ignore when inconvenient?',
    'Systematic comparison of invocation rate against severity-matched non-invocation cases (Syria, Myanmar, Xinjiang) controlling for the target state''s P5 patron status; if invocation correlates more strongly with patron absence than with violation severity, the power-instrument reading is supported.',
    'If genuine norm, the snare classification should weight coordination function more heavily and ε should trend toward the rope/tangled_rope boundary; if power instrument, ε and suppression are understated here and a snare classification with even higher extraction is warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_norm_vs_power_instrument, empirical, 'Whether conditional sovereignty is an emerging binding norm or a selectively-invoked legitimation tool.').

omega_variable(
    kernel_reading_choice_ambiguity,
    'Is the conditional-sovereignty reading the correct lens for cases like Kosovo (1999, no Security Council authorization) where intervention proceeded outside the very institutional gate this reading assumes?',
    'Track whether ''illegal but legitimate'' interventions (Kosovo-style) are better modeled as instances of this reading operating informally, or as evidence for the graduated_sovereignty sibling reading, which does not require formal institutional triggering.',
    'If Kosovo-style cases belong structurally to the graduated reading rather than this one, this story''s stakeholder set and invocation-pattern measurements should exclude them, tightening this reading''s ε toward the P5-gated cases only.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_choice_ambiguity, conceptual, 'Whether informally-authorized interventions belong to this reading or to the graduated_sovereignty sibling.').

omega_variable(
    civilian_cost_attribution,
    'When intervention under this doctrine causes civilian harm (e.g., post-2011 Libyan instability), should that harm be attributed to the doctrine''s operation or to the separate failure of post-intervention state-building, which the doctrine does not itself govern?',
    'Comparative case analysis of interventions with strong post-conflict stabilization plans versus those without, isolating whether harm correlates with the intervention decision itself or with downstream execution choices outside the doctrine''s scope.',
    'If harm is primarily attributable to execution failures rather than the triggering doctrine, the victim-side extractiveness attributed to this constraint specifically (as opposed to intervention-execution constraints) should be reduced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_cost_attribution, conceptual, 'Whether post-intervention harm should be scored against this doctrine or against separate execution/state-building constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__conditional_sovereignty, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1990, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(west_tr_t1999, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 1999, 0.3).
narrative_ontology:measurement(west_tr_t2005, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2005, 0.33).
narrative_ontology:measurement(west_tr_t2011, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2011, 0.35).
narrative_ontology:measurement(west_tr_t2016, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2016, 0.42).
narrative_ontology:measurement(west_tr_t2024, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(west_be_t1990, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 1990, 0.18).
narrative_ontology:measurement(west_be_t1999, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 1999, 0.28).
narrative_ontology:measurement(west_be_t2005, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2005, 0.32).
narrative_ontology:measurement(west_be_t2011, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2011, 0.4).
narrative_ontology:measurement(west_be_t2016, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2016, 0.36).
narrative_ontology:measurement(west_be_t2024, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2024, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1990, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 1990, 0.2).
narrative_ontology:measurement(west_su_t1999, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 1999, 0.3).
narrative_ontology:measurement(west_su_t2005, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2005, 0.38).
narrative_ontology:measurement(west_su_t2011, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2011, 0.45).
narrative_ontology:measurement(west_su_t2016, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2016, 0.4).
narrative_ontology:measurement(west_su_t2024, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__conditional_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty__absolute_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty__graduated_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, un_security_council_veto_power).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, international_criminal_court_jurisdiction).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the westphalian_sovereignty kernel, decomposed per the ε-invariance principle: absolute_sovereignty (categorical non-intervention, low ε as a coordination norm from the sovereign-state seat), conditional_sovereignty (this story — moderate ε, snare, threshold-triggered intervention), and graduated_sovereignty (capacity/legitimacy-indexed spectrum, structurally distinct triggering logic). Each reading has its own beneficiary/victim structure and its own ε; they are linked here rather than merged because measuring 'sovereignty' by different observables (formal charter text vs. R2P practice vs. capacity-based legitimacy assessments) yields materially different extraction profiles — exactly the case the ε-invariance test flags for decomposition rather than a single averaged story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
