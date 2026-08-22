% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__narrow_armed_attack_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__narrow_armed_attack_reading, []).

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
 *   constraint_id: article_51_self_defense__narrow_armed_attack_reading
 *   human_readable: Article 51 Narrow Armed-Attack Reading of Self-Defense
 *   domain: international_law/security_studies
 *
 * SUMMARY:
 *   This story instantiates the NARROW ARMED-ATTACK reading of the Article 51
 *   self-defense kernel: self-defense is lawful only in response to an actual
 *   or imminent armed attack, and where the attack is by non-state actors,
 *   only if the attack is attributable to a state under the ordinary rules of
 *   state responsibility (effective control, not mere harboring or inability
 *   to suppress). This is the reading most associated with the ICJ's
 *   Nicaragua and Wall jurisprudence and with non-aligned and weaker-state
 *   diplomatic positions. It is a genuine coordination mechanism — a
 *   Schelling point that prevents the Charter's use-of-force prohibition from
 *   unraveling into generalized preventive war — but it also functions as an
 *   asymmetric constraint: it binds powerful states with the military
 *   capacity and non-state-actor threat exposure to act, while its
 *   coordination benefits (sovereignty protection, Council primacy) accrue
 *   disproportionately to weaker states and multilateral institutions. That
 *   dual character — real coordination function plus asymmetric binding — is
 *   why this reading is authored as tangled_rope rather than pure rope or
 *   pure snare.
 *
 * KEY AGENTS:
 *   - weaker_un_member_states: primary beneficiary (powerless/trapped) — sovereignty shielded by the high attribution bar
 *   - un_security_council: institutional beneficiary and partial agenda-setter — retains gatekeeping monopoly over authorized force
 *   - international_court_of_justice: agenda-setter (institutional/analytical) — the doctrinal engine that has entrenched this reading through case law
 *   - powerful_states_facing_nonstate_threats: primary target (powerful/constrained) — legally barred from lawful unilateral strikes absent state attribution
 *   - counterterrorism_coalition_states: secondary target — bears reputational and legal costs for operations against non-attributable actors
 *   - host_states_with_ungoverned_territory: excluded voice — genuinely incapable but legally irrelevant to the attribution test
 *   - international_law_scholars: analytical observer — documents the doctrine/practice gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__narrow_armed_attack_reading, 0.42).
domain_priors:suppression_score(article_51_self_defense__narrow_armed_attack_reading, 0.55).
domain_priors:theater_ratio(article_51_self_defense__narrow_armed_attack_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__narrow_armed_attack_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__narrow_armed_attack_reading, "Article 51 Narrow Armed-Attack Reading of Self-Defense").
narrative_ontology:topic_domain(article_51_self_defense__narrow_armed_attack_reading, "international_law/security_studies").

domain_priors:requires_active_enforcement(article_51_self_defense__narrow_armed_attack_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__narrow_armed_attack_reading, 'f45cfc89-3f05-441c-a64a-47f69c1019d4').
narrative_ontology:cs_kernel_codification('f45cfc89-3f05-441c-a64a-47f69c1019d4', fixed_text).
narrative_ontology:cs_authority_grounding('f45cfc89-3f05-441c-a64a-47f69c1019d4', practice).
narrative_ontology:cs_interpretation_layer_present('f45cfc89-3f05-441c-a64a-47f69c1019d4').
narrative_ontology:cs_reading_relation('f45cfc89-3f05-441c-a64a-47f69c1019d4', article_51_self_defense__expansive_preventive_reading, coexists_with).
narrative_ontology:cs_reading_relation('f45cfc89-3f05-441c-a64a-47f69c1019d4', article_51_self_defense__unable_unwilling_doctrine_reading, influences).
narrative_ontology:cs_axiom('f45cfc89-3f05-441c-a64a-47f69c1019d4', foundational, state_attribution_is_necessary_condition).
narrative_ontology:cs_axiom_status(state_attribution_is_necessary_condition, holdable).
narrative_ontology:cs_axiom_grounding('f45cfc89-3f05-441c-a64a-47f69c1019d4', state_attribution_is_necessary_condition, conventional).
narrative_ontology:cs_axiom('f45cfc89-3f05-441c-a64a-47f69c1019d4', foundational, collective_security_primacy_over_unilateral_force).
narrative_ontology:cs_axiom_status(collective_security_primacy_over_unilateral_force, holdable).
narrative_ontology:cs_axiom_grounding('f45cfc89-3f05-441c-a64a-47f69c1019d4', collective_security_primacy_over_unilateral_force, instrumental).
narrative_ontology:cs_reference_frame('f45cfc89-3f05-441c-a64a-47f69c1019d4', charter_era_interstate_aggression_prevention).
narrative_ontology:cs_drift_state('f45cfc89-3f05-441c-a64a-47f69c1019d4', post_2001_counterterrorism_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f45cfc89-3f05-441c-a64a-47f69c1019d4', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__narrow_armed_attack_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, weaker_un_member_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, un_security_council).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, international_court_of_justice).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, non_aligned_bloc_states).
narrative_ontology:constraint_victim(article_51_self_defense__narrow_armed_attack_reading, powerful_states_facing_nonstate_threats).
narrative_ontology:constraint_victim(article_51_self_defense__narrow_armed_attack_reading, counterterrorism_coalition_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Depend on the narrow reading to prevent stronger states from justifying incursions onto their territory as 'self-defense' against diffuse or attributed non-state threats. Their sovereignty is protected precisely because the bar for lawful unilateral force against them is kept high. They have no capacity to resist a determined great power militarily and rely entirely on the legal constraint holding.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, weaker_un_member_states, beneficiary,
    powerless, generational, trapped, global).

% Its collective-security monopoly on authorizing force is preserved by keeping Article 51 narrow; every expansion of unilateral self-defense is a corresponding contraction of the Council's gatekeeping role. Permanent members with veto power benefit from the Council remaining the chokepoint, even though they sometimes chafe at it when they themselves want to act unilaterally.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, un_security_council, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__narrow_armed_attack_reading, un_security_council, agenda_setter).

% Has repeatedly affirmed the narrow reading (Nicaragua, Wall Advisory Opinion, DRC v. Uganda), attributing armed attacks to states and rejecting attribution based on mere harboring or inability to control non-state actors. Its jurisprudence is the primary doctrinal engine enforcing this reading, though it has no independent enforcement power and depends on state compliance.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, international_court_of_justice, agenda_setter,
    institutional, civilizational, analytical, global).

% Historically championed the strict attribution requirement at the UN General Assembly and in the Friendly Relations Declaration precisely to foreclose great-power intervention dressed as self-defense. They gain diplomatic leverage and legal cover by keeping the threshold high, even when it means they cannot themselves easily justify cross-border strikes against threats originating in neighboring failed states.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, non_aligned_bloc_states, beneficiary,
    moderate, generational, constrained, global).

% Face armed groups operating from or through the territory of states that cannot or will not suppress them, but under this reading cannot lawfully strike without proving state attribution — a demanding evidentiary bar rarely met. Must either forgo action, act and absorb international condemnation and reduced legal standing, or seek Security Council authorization that a veto can block. Their military capability vastly exceeds their legal license to use it against these threats.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, powerful_states_facing_nonstate_threats, payer,
    powerful, immediate, constrained, global).

% Coordinate cross-border operations against non-state armed groups and are repeatedly told by the narrow reading's proponents that such strikes lack clear Article 51 cover absent proof of host-state direction or effective control. They bear reputational and legal-liability costs for actions their own populations and legislatures regard as necessary security responses.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, counterterrorism_coalition_states, payer,
    powerful, biographical, constrained, continental).

% Often genuinely unable to control armed groups within their borders, but the narrow reading does not ask what they are ABLE to do — only whether the attack is attributable to them as a state act. Their perspective (that they are victims of the same non-state actors, not accomplices) rarely enters the doctrinal debate about whether force against them is lawful.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, host_states_with_ungoverned_territory, excluded,
    moderate, biographical, trapped, national).

% Debate whether the narrow reading reflects genuine customary international law or is increasingly divorced from state practice since 2001. They document the widening gap between the doctrinal rule and what powerful states actually do, without power to resolve which controls.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_51_self_defense__narrow_armed_attack_reading, diffuse).
narrative_ontology:fixing_cost_class(article_51_self_defense__narrow_armed_attack_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, bright-line rule for when unilateral cross-border force is lawful, preventing an unraveling of the UN Charter's prohibition on the use of force into a general license for preventive war whenever a state perceives a threat.
% TRANSFER_FUNCTION: Moves strategic freedom of action away from powerful states with the military capacity to strike non-state threats abroad, and toward weaker states (protected sovereignty) and the Security Council (retained gatekeeping authority over authorized force).
% ABSENT_VOICES: Populations living under threat from non-state armed groups based in host states are rarely heard directly — neither the host state's incapacity nor the victim population's exposure enters the attribution analysis, which asks only about the host state's legal responsibility for the act.
% DISAPPEARANCE_RATIONALE: If the narrow attribution requirement disappeared, powerful states would face far fewer legal barriers to cross-border strikes against non-state actors; the Security Council's gatekeeping function would erode further; weaker and host states would lose a significant legal shield against unilateral intervention; and the customary law baseline governing when force is lawful would shift substantially toward the discretion of militarily capable states.
% FOUNDING_PROBLEM: The UN Charter framers sought to outlaw unilateral war except in narrowly defined circumstances, having just witnessed how expansive self-defense claims (including preventive-war rhetoric) were used to justify aggression in the lead-up to World War II.
% FOUNDING_PROBLEM_CORROBORATION: The ICJ and non-aligned states attest the founding problem remains live — that expansive self-defense claims still function as pretexts for intervention, citing recent invocations. Independent international law scholars outside both the beneficiary and payer camps document that state practice since 2001 (drone strikes, cross-border counterterrorism operations against non-attributable non-state actors) has diverged substantially from the narrow rule, suggesting the rule's function has partly shifted from constraining aggression to constraining counterterrorism operations the original framers did not anticipate.
narrative_ontology:disappearance_verdict(article_51_self_defense__narrow_armed_attack_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__narrow_armed_attack_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__narrow_armed_attack_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_51_self_defense__narrow_armed_attack_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__narrow_armed_attack_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__narrow_armed_attack_reading_tests).
:- end_tests(article_51_self_defense__narrow_armed_attack_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at 2025) rather than high because the constraint genuinely does prevent aggression dressed as self-defense — it is not pure extraction. But it has risen steadily since 1945 as non-state armed threats (terrorism, insurgency, transnational militancy) have become the dominant security concern, while the doctrinal rule has not adapted; the widening gap between the rule's design assumptions (interstate war) and contemporary threat patterns (non-state violence from ungoverned or unwilling territories) is what drives the extraction trend upward. Suppression (0.55) reflects the active doctrinal and diplomatic pressure — through ICJ rulings, UN resolutions, and non-aligned bloc advocacy — required to keep powerful states from simply asserting a broader customary rule through repeated unilateral practice. Theater ratio (0.28) captures that a portion of continued invocation of the narrow rule in diplomatic fora is now performative: states publicly reaffirm the rule while privately or operationally acting outside it (targeted strikes, drone campaigns), producing a doctrine increasingly maintained by rhetorical adherence rather than universal compliance.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of a weaker UN member state or the ICJ, this reading is coordination-preserving: a Rope holding back a return to unrestricted interstate violence. From the seat of a powerful state facing a sustained non-state threat from an incapable host state, the same rule computes as a Snare-like constraint — coercive, unresponsive to the actual security problem, and enforced through diplomatic and legal condemnation rather than genuine consent. The tangled_rope classification is meant to hold both computations as structurally true from their respective seats rather than resolving the gap in either direction.
 *
 * DIRECTIONALITY LOGIC:
 *   Weaker states, the Security Council, and the ICJ are structural beneficiaries: the narrow reading is precisely what preserves their protected status or institutional authority, so their directionality sits near the beneficiary end. Powerful states with non-state-actor threat exposure are structural targets: the rule directly constrains the strategic option they would otherwise exercise, and their exit options are constrained (they can violate the rule and pay reputational/legal costs, or comply and accept reduced security options) rather than genuinely mobile. Host states with ungoverned territory are excluded rather than positioned on the beneficiary/victim axis at all — the doctrine does not ask about their capacity, only their attributability, so their actual situation is invisible to the rule's own terms.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing pretextual wars of aggression) remains partially live — expansive self-defense claims are still used to justify interventions — but the doctrine's central contemporary friction is with counterterrorism and non-state threats, a problem category the 1945 framers did not anticipate. This is not mandatrophy in the classic sense (the mandate has not become fully obsolete), but there is a genuine mismatch between the rule's original target (interstate aggression) and its current primary application (constraining responses to non-state violence), which is why founding_problem_status is authored as 'contested' rather than 'live' or 'dead.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    customary_law_erosion_vs_persistence,
    'Has repeated state practice departing from the narrow attribution requirement (post-2001 counterterrorism strikes) generated a new rule of customary international law, or does the narrow reading remain the controlling legal standard despite widespread violation?',
    'Systematic survey of state practice and opinio juris: do states justifying cross-border strikes claim they are creating new law, or do they claim their specific case satisfies the existing narrow test (special pleading)? The latter suggests persistence of the rule; the former suggests erosion.',
    'If customary law has shifted, this reading is a description of formal doctrine that no longer reflects the operative legal rule — closer to a Piton (theatrical adherence, atrophied function) than a Tangled Rope. If the rule persists despite violations, the Tangled Rope classification holds: genuine coordination function under active strain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_erosion_vs_persistence, empirical, 'Whether the narrow reading still describes binding customary law or has become a legal fiction maintained rhetorically.').

omega_variable(
    which_reading_is_the_real_kernel,
    'Is the narrow armed-attack reading the correct interpretation of the Charter''s original design, or is it itself a restrictive construction favored by states that benefit from constraining others'' military options?',
    'This is a contested kernel with three declared readings (narrow_armed_attack, expansive_preventive, unable_unwilling_doctrine); no single resolution mechanism can adjudicate between them because each reading is held by a different coalition of states and scholars for reasons that track their own strategic position as much as legal reasoning.',
    'The choice of reading determines which states are classified as beneficiaries versus victims of the Article 51 kernel; this story deliberately takes the narrow reading as its own clean, ε-invariant constraint per Rule 1, without averaging across or adjudicating the sibling readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_reading_is_the_real_kernel, conceptual, 'Committer-frame ambiguity: which reading of the Article 51 kernel is authoritative is itself contested and not resolved by this story.').

omega_variable(
    attribution_standard_beneficiary_structure,
    'Does the strict state-attribution requirement genuinely serve the coordination function of preventing pretextual aggression, or does it also function to preserve the strategic position of states and blocs that lack the military capacity to project force against non-state threats and thus have little to lose from the restriction?',
    'Compare the voting and advocacy patterns of states on attribution-standard questions against their own military capability and non-state-threat exposure; if support for the narrow standard correlates strongly with lacking capacity to act unilaterally, this supports a partially self-interested beneficiary structure rather than pure principle.',
    'If beneficiary support tracks capability rather than principle, the coordination function claimed for this reading is partly cover for a capability-preserving asymmetry, reinforcing the tangled_rope classification over a pure rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attribution_standard_beneficiary_structure, conceptual, 'Whether beneficiary support for the narrow reading is principled or capability-correlated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__narrow_armed_attack_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(arti_tr_t1970, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1970, 0.14).
narrative_ontology:measurement(arti_tr_t2001, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2001, 0.22).
narrative_ontology:measurement(arti_tr_t2010, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2010, 0.26).
narrative_ontology:measurement(arti_tr_t2018, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2018, 0.27).
narrative_ontology:measurement(arti_tr_t2025, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1945, 0.2).
narrative_ontology:measurement(arti_be_t1970, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement(arti_be_t2001, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2001, 0.32).
narrative_ontology:measurement(arti_be_t2010, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2010, 0.38).
narrative_ontology:measurement(arti_be_t2018, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2018, 0.4).
narrative_ontology:measurement(arti_be_t2025, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1945, 0.35).
narrative_ontology:measurement(arti_su_t1970, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1970, 0.42).
narrative_ontology:measurement(arti_su_t2001, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2001, 0.48).
narrative_ontology:measurement(arti_su_t2010, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2010, 0.52).
narrative_ontology:measurement(arti_su_t2018, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2018, 0.54).
narrative_ontology:measurement(arti_su_t2025, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__narrow_armed_attack_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, article_51_self_defense__expansive_preventive_reading).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, article_51_self_defense__unable_unwilling_doctrine_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the article_51_self_defense kernel. The narrow_armed_attack_reading is the most restrictive; expansive_preventive_reading is the least restrictive (permits preemptive/preventive force against emerging non-state threats given demonstrated necessity); unable_unwilling_doctrine_reading occupies a hybrid position (triggers self-defense against non-state actors when the host state is unable/unwilling to suppress the threat, without requiring full state attribution). Each reading is authored with its own independent epsilon and beneficiary/victim structure per the epsilon-invariance principle; they are not measurements of the same constraint under different observables but structurally distinct constraints sharing a contested textual kernel (UN Charter Article 51).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
