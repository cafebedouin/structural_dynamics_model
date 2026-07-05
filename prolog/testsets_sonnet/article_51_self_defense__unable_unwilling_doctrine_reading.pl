% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__unable_unwilling_doctrine_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__unable_unwilling_doctrine_reading, []).

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
 *   constraint_id: article_51_self_defense__unable_unwilling_doctrine_reading
 *   human_readable: Article 51 Self-Defense: Unwilling-or-Unable Host-State Doctrine
 *   domain: international_law/security_studies/constitutional_interpretation
 *
 * SUMMARY:
 *   This story instantiates the unable-or-unwilling reading of the Article 51
 *   self-defense kernel: force is lawful against non-state actors operating
 *   from a host state's territory once that state is judged unable or
 *   unwilling to suppress the threat, without requiring the attack be
 *   attributable to the host state itself. This sits structurally between the
 *   narrow armed-attack reading (which requires state attribution) and the
 *   expansive preventive reading (which does not require an attack to have
 *   occurred at all). Since 2001 the doctrine has moved from a marginal
 *   justification invoked in a handful of cases to a standard component of
 *   counterterrorism legal argument by major military powers, with the
 *   operative 'unwilling or unable' determination made unilaterally and
 *   without independent verification.
 *
 * KEY AGENTS:
 *   - intervening_states_with_counterterrorism_mandates: agenda_setter/beneficiary — makes the unilateral determination and conducts the resulting strikes
 *   - host_states_with_weak_governance_capacity: payer — sovereignty bypassed once labeled unwilling or unable
 *   - civilian_populations_in_host_state_border_regions: payer — bears direct physical and displacement costs
 *   - non_state_armed_groups: excluded — the nominal trigger, not a party to any legal process
 *   - un_security_council: observer — structurally sidelined by unilateral pre-emption of collective authorization
 *   - international_law_scholars: analytical observer — documents the doctrine's contested customary status
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__unable_unwilling_doctrine_reading, 0.58).
domain_priors:suppression_score(article_51_self_defense__unable_unwilling_doctrine_reading, 0.62).
domain_priors:theater_ratio(article_51_self_defense__unable_unwilling_doctrine_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__unable_unwilling_doctrine_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__unable_unwilling_doctrine_reading, "Article 51 Self-Defense: Unwilling-or-Unable Host-State Doctrine").
narrative_ontology:topic_domain(article_51_self_defense__unable_unwilling_doctrine_reading, "international_law/security_studies/constitutional_interpretation").

domain_priors:requires_active_enforcement(article_51_self_defense__unable_unwilling_doctrine_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__unable_unwilling_doctrine_reading, 'ca40a7c1-9ad8-4028-8f1c-df4f507b4443').
narrative_ontology:cs_kernel_codification('ca40a7c1-9ad8-4028-8f1c-df4f507b4443', fixed_text).
narrative_ontology:cs_authority_grounding('ca40a7c1-9ad8-4028-8f1c-df4f507b4443', distributed).
narrative_ontology:cs_reading_relation('ca40a7c1-9ad8-4028-8f1c-df4f507b4443', article_51_self_defense__narrow_armed_attack_reading, influences).
narrative_ontology:cs_reading_relation('ca40a7c1-9ad8-4028-8f1c-df4f507b4443', article_51_self_defense__expansive_preventive_reading, influences).
narrative_ontology:cs_axiom('ca40a7c1-9ad8-4028-8f1c-df4f507b4443', foundational, host_state_incapacity_substitutes_for_attribution).
narrative_ontology:cs_axiom_status(host_state_incapacity_substitutes_for_attribution, holdable).
narrative_ontology:cs_axiom_grounding('ca40a7c1-9ad8-4028-8f1c-df4f507b4443', host_state_incapacity_substitutes_for_attribution, conventional).
narrative_ontology:cs_axiom('ca40a7c1-9ad8-4028-8f1c-df4f507b4443', foundational, actual_or_imminent_attack_still_required).
narrative_ontology:cs_axiom_status(actual_or_imminent_attack_still_required, holdable).
narrative_ontology:cs_axiom_grounding('ca40a7c1-9ad8-4028-8f1c-df4f507b4443', actual_or_imminent_attack_still_required, conventional).
narrative_ontology:cs_reference_frame('ca40a7c1-9ad8-4028-8f1c-df4f507b4443', un_charter_state_centric_attack_attribution).
narrative_ontology:cs_drift_state('ca40a7c1-9ad8-4028-8f1c-df4f507b4443', post_9_11_counterterrorism_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ca40a7c1-9ad8-4028-8f1c-df4f507b4443', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_counterterrorism_mandates).
narrative_ontology:constraint_beneficiary(article_51_self_defense__unable_unwilling_doctrine_reading, military_and_intelligence_apparatuses_of_intervening_states).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, host_states_with_weak_governance_capacity).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, civilian_populations_in_host_state_border_regions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invoke the unwilling-or-unable standard to justify cross-border strikes against non-state armed groups without host-state consent, framing the host state's governance failure as the trigger for lawful self-defense. Sets the operative content of the doctrine through state practice, legal memoranda, and after-the-fact justification, and bears essentially no binding external check on its own determination of 'unwilling or unable.'
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_counterterrorism_mandates, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_counterterrorism_mandates, beneficiary).

% Have their territorial sovereignty bypassed once an outside power unilaterally judges them unwilling or unable to suppress a threat operating from their soil. Often lack the military or administrative capacity to contest the determination and cannot practically prevent strikes once the label is applied; their consent becomes legally irrelevant to the intervening state's justification.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, host_states_with_weak_governance_capacity, payer,
    moderate, biographical, trapped, national).

% Live in the areas where strikes against non-state actors actually occur. Bear the direct physical risk, displacement, and collateral harm from an intervention triggered by a legal determination made in a foreign capital about their own government's capacity, in which they have no voice and no remedy.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, civilian_populations_in_host_state_border_regions, payer,
    powerless, immediate, trapped, regional).

% Are the proximate cause invoked to trigger the doctrine but are not parties to any legal process; they often relocate operations across borders precisely because dispersed, mobile organization is what makes the unwilling-or-unable determination easy to assert against a given host state.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, non_state_armed_groups, excluded,
    organized, biographical, mobile, regional).

% Retains formal authority over collective security determinations but has been structurally sidelined by states acting unilaterally under Article 51 self-defense claims before or without Council authorization; can condemn or tacitly acquiesce after the fact but rarely reverses a completed strike.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, un_security_council, observer,
    institutional, generational, constrained, global).

% Debate whether the unwilling-or-unable standard reflects emerging customary international law or is state practice dressed as law by the powers most able to project force. Their disagreement is itself evidence of the doctrine's contested legal status.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_counterterrorism_mandates).
narrative_ontology:fixing_cost_class(article_51_self_defense__unable_unwilling_doctrine_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal vocabulary allowing states to respond to genuine cross-border non-state-actor attacks (terrorism, insurgent cross-border raids) in situations where the territorial host government cannot or will not act, avoiding a gap in which victim states have no lawful recourse against ongoing armed attacks merely because the attacker is not a state.
% TRANSFER_FUNCTION: Moves the practical authority to determine when force is lawful in a third country's territory from that territory's government (and from the UN Security Council's collective security mechanism) to the unilateral judgment of the intervening state; moves physical risk and sovereignty costs onto the host state and its border populations.
% ABSENT_VOICES: Non-state armed groups have no forum. More importantly, host-state populations affected by strikes are never party to the bilateral or unilateral legal determination of 'unwillingness or inability' — that determination is made entirely by the intervening state's own legal and intelligence apparatus, evaluated against no external evidentiary standard.
% DISAPPEARANCE_RATIONALE: Intervening states argue that without this doctrine, safe havens for non-state armed groups would multiply and no lawful remedy would exist against undergoverned or complicit host states — the world would rearrange toward more unchecked non-state violence. Host states and much of the non-aligned bloc argue the doctrine primarily supplies legal cover for interventions that would occur anyway under weaker justifications, and that its disappearance would simply force intervening states back to Security Council authorization or narrower armed-attack attribution, restoring sovereignty as the default rule.
% FOUNDING_PROBLEM: Non-state actors (transnational terrorist networks, insurgent groups) can mount attacks producing armed-attack-level harm from territory whose government is either complicit, captured, or simply incapable of controlling its own territory, creating an apparent gap in the classical state-to-state Article 51 framework where no attributable state action exists to respond to.
% FOUNDING_PROBLEM_CORROBORATION: Intervening states and allied legal scholars (some serving as government counsel) attest the gap remains live, citing ongoing transnational terrorist operations from ungoverned spaces. Independent scholarship — including scholars from non-intervening states and some UN Special Rapporteurs on counter-terrorism and human rights — corroborates that a genuine gap existed historically but argues the doctrine's actual operation has expanded well past closing that gap, now covering strikes against groups whose threat to the intervening state is speculative or historical rather than ongoing; this corroboration comes from outside the set of states that benefit from invoking the doctrine.
narrative_ontology:disappearance_verdict(article_51_self_defense__unable_unwilling_doctrine_reading, contested).
narrative_ontology:founding_problem_status(article_51_self_defense__unable_unwilling_doctrine_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__unable_unwilling_doctrine_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_51_self_defense__unable_unwilling_doctrine_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__unable_unwilling_doctrine_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__unable_unwilling_doctrine_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__unable_unwilling_doctrine_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__unable_unwilling_doctrine_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) and suppression (0.62) sit at moderate-high levels: real coordination function exists (a genuine legal gap regarding non-state actor attacks from ungoverned territory), but the determination mechanism has no independent check, letting the intervening state set both the trigger and its own compliance. Theater ratio (0.34) reflects that legal justification documents increasingly perform compliance with a standard (imminence, necessity, proportionality) whose actual application has drifted toward post hoc rationalization of decisions made on operational grounds. Accessibility collapse (0.5) and resistance (0.6) are moderate — host states and scholars do actively contest the doctrine's application, and alternative frameworks (Security Council authorization, narrow armed-attack) remain live and invoked.
 *
 * PERSPECTIVAL GAP:
 *   From the intervening state's seat, unable-or-unwilling functions as a modest, necessity-constrained extension of settled self-defense law closing an obvious gap. From the host state's seat, the same standard functions as a unilateral override of sovereignty dressed in the language of self-defense, since the intervening state is simultaneously accuser, judge, and enforcer of the 'unwilling or unable' finding.
 *
 * DIRECTIONALITY LOGIC:
 *   Intervening states with counterterrorism mandates sit near the beneficiary end of directionality: they both set the standard and collect its benefit (legal cover for unilateral force) with essentially arbitrage-grade exit from any binding external review. Host states with weak governance capacity sit near the target end: trapped exit, sovereignty bypassed by a determination they cannot contest ex ante. Civilian populations in border regions sit at the extreme target end — powerless, immediate time horizon, trapped, bearing the doctrine's sharpest physical costs with zero voice in its invocation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a genuine post-9/11 gap where non-state actors exploited weak or complicit host states) was real and is corroborated even by non-benefiting scholars as having existed. But the founding_problem_status is contested precisely because the doctrine's scope of application has not remained tethered to that narrow gap — it now reaches groups posing speculative rather than ongoing threats. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (a real answer to a real gap) while still registering the asymmetric extraction (host-state sovereignty and civilian safety bypassed by an unreviewable unilateral determination) that the pure coordination story alone would suppress.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unilateral_determination_review_gap,
    'Should the ''unwilling or unable'' determination be subject to any independent verification (UN fact-finding, third-party tribunal) before or after force is used, or does requiring prior review defeat the doctrine''s operational purpose?',
    'State practice and opinio juris accumulation: track whether states increasingly submit unable-or-unwilling determinations to any multilateral forum for post hoc review, and whether such review has ever altered a determination''s legal standing.',
    'If no independent review mechanism ever develops or is ever honored, the doctrine functions structurally as self-certified extraction regardless of its coordination rationale; if review mechanisms mature, the doctrine moves closer to genuine rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unilateral_determination_review_gap, empirical, 'Whether any external check constrains the self-certifying determination at the doctrine''s core.').

omega_variable(
    customary_law_crystallization_status,
    'Has the unwilling-or-unable standard actually crystallized into customary international law through sufficiently widespread and consistent state practice plus opinio juris, or does it remain a contested claim asserted primarily by states with the military capacity to act on it?',
    'Systematic survey of state practice across non-aligned, developing, and major-power states, weighted by whether states are consistently practicing the doctrine as law (versus merely tolerating powerful states'' invocations of it without endorsing it as binding for themselves).',
    'If crystallized, the constraint is better read as settled law with genuine (if contested) coordination function; if not crystallized, the doctrine is more accurately characterized as powerful-state practice cloaked in legal vocabulary — pushing the classification toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(customary_law_crystallization_status, conceptual, 'Whether the reading reflects settled customary law or is asserted law by the states best positioned to act on it.').

omega_variable(
    scope_creep_from_gap_closing_to_general_license,
    'Has the doctrine''s actual invocation pattern remained tethered to closing the narrow post-9/11 gap (imminent, ongoing non-state attacks from ungoverned territory) or has it drifted into a general license for cross-border force against any group a state disfavors, provided the host state can be characterized as weak?',
    'Case-by-case review of invoked strikes: assess whether the underlying threat was ongoing/imminent versus historical/speculative at time of strike, and whether host-state incapacity was demonstrated versus asserted.',
    'Scope creep toward general license would corroborate the rising extractiveness trend in the temporal measurements and support reclassification pressure toward snare; continued tethering to the narrow gap would support the tangled_rope reading remaining stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_creep_from_gap_closing_to_general_license, empirical, 'Whether the doctrine''s application has expanded beyond its founding justification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__unable_unwilling_doctrine_reading, 2001, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t2001, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2001, 0.18).
narrative_ontology:measurement(arti_tr_t2005, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2005, 0.22).
narrative_ontology:measurement(arti_tr_t2009, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2009, 0.26).
narrative_ontology:measurement(arti_tr_t2014, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2014, 0.29).
narrative_ontology:measurement(arti_tr_t2019, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2019, 0.32).
narrative_ontology:measurement(arti_tr_t2024, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2024, 0.34).

% Extraction over time
narrative_ontology:measurement(arti_be_t2001, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2001, 0.32).
narrative_ontology:measurement(arti_be_t2005, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2005, 0.4).
narrative_ontology:measurement(arti_be_t2009, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2009, 0.46).
narrative_ontology:measurement(arti_be_t2014, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2014, 0.52).
narrative_ontology:measurement(arti_be_t2019, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2019, 0.56).
narrative_ontology:measurement(arti_be_t2024, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t2001, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2001, 0.4).
narrative_ontology:measurement(arti_su_t2005, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2005, 0.48).
narrative_ontology:measurement(arti_su_t2009, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2009, 0.53).
narrative_ontology:measurement(arti_su_t2014, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2014, 0.58).
narrative_ontology:measurement(arti_su_t2019, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2019, 0.6).
narrative_ontology:measurement(arti_su_t2024, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__unable_unwilling_doctrine_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, narrow_armed_attack_reading).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, expansive_preventive_reading).

% DUAL FORMULATION NOTE:
% This story is the middle reading of the article_51_self_defense kernel, sitting between narrow_armed_attack_reading (requires state attribution) and expansive_preventive_reading (does not require an attack to have occurred). Structural pressure runs in both directions: successful invocations of the unwilling-or-unable standard lower the practical threshold for expansive_preventive_reading claims by normalizing unilateral threat assessment, while persistent doctrinal resistance to unable-or-unwilling from host states and scholars sustains narrow_armed_attack_reading as the fallback position major powers must still occasionally invoke.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
