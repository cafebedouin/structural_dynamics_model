% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__sovereignty_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: article_27_veto_power__sovereignty_reading
 *   human_readable: P5 Veto as Westphalian Sovereignty Applied to Enforcement-Capable Great Powers
 *   domain: international_relations/institutional_design/constitutional_law
 *
 * SUMMARY:
 *   Article 27(3) of the UN Charter requires the concurring vote of all five
 *   permanent Security Council members for non-procedural resolutions — the
 *   veto. Under the sovereignty reading, this clause is not a policy choice
 *   made by drafters in 1945 that could have gone another way; it is the
 *   formal acknowledgment that no international institution, however
 *   constituted, could compel a nuclear-armed state with independent global
 *   enforcement capacity to submit to a binding decision it had not consented
 *   to, without that attempted compulsion collapsing into the very
 *   great-power war the UN exists to prevent. The claim under this reading is
 *   that the constraint is closer to a structural fact about the distribution
 *   of coercive capacity in the international system than to a negotiated
 *   privilege.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__sovereignty_reading, 0.06).
domain_priors:suppression_score(article_27_veto_power__sovereignty_reading, 0.08).
domain_priors:theater_ratio(article_27_veto_power__sovereignty_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, extractiveness, 0.06).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__sovereignty_reading, mountain).
narrative_ontology:human_readable(article_27_veto_power__sovereignty_reading, "P5 Veto as Westphalian Sovereignty Applied to Enforcement-Capable Great Powers").
narrative_ontology:topic_domain(article_27_veto_power__sovereignty_reading, "international_relations/institutional_design/constitutional_law").

domain_priors:emerges_naturally(article_27_veto_power__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__sovereignty_reading, '68469e57-7efc-4287-8e1c-5c35bcbf4052').
narrative_ontology:cs_kernel_codification('68469e57-7efc-4287-8e1c-5c35bcbf4052', fixed_text).
narrative_ontology:cs_authority_grounding('68469e57-7efc-4287-8e1c-5c35bcbf4052', lineage).
narrative_ontology:cs_interpretation_layer_present('68469e57-7efc-4287-8e1c-5c35bcbf4052').
narrative_ontology:cs_reading_relation('68469e57-7efc-4287-8e1c-5c35bcbf4052', article_27_veto_power__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('68469e57-7efc-4287-8e1c-5c35bcbf4052', article_27_veto_power__oligopoly_reading, influences).
narrative_ontology:cs_axiom('68469e57-7efc-4287-8e1c-5c35bcbf4052', foundational, consent_precedes_enforceable_obligation).
narrative_ontology:cs_axiom_status(consent_precedes_enforceable_obligation, holdable).
narrative_ontology:cs_axiom_grounding('68469e57-7efc-4287-8e1c-5c35bcbf4052', consent_precedes_enforceable_obligation, deontological).
narrative_ontology:cs_axiom('68469e57-7efc-4287-8e1c-5c35bcbf4052', foundational, enforcement_capacity_asymmetry_is_prior_to_charter_design).
narrative_ontology:cs_axiom_status(enforcement_capacity_asymmetry_is_prior_to_charter_design, holdable).
narrative_ontology:cs_axiom_grounding('68469e57-7efc-4287-8e1c-5c35bcbf4052', enforcement_capacity_asymmetry_is_prior_to_charter_design, empirically_contingent).
narrative_ontology:cs_reference_frame('68469e57-7efc-4287-8e1c-5c35bcbf4052', westphalian_state_consent_baseline).
narrative_ontology:cs_drift_state('68469e57-7efc-4287-8e1c-5c35bcbf4052', post_cold_war_multipolar_transition, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('68469e57-7efc-4287-8e1c-5c35bcbf4052', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__sovereignty_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__sovereignty_reading, permanent_five_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(article_27_veto_power__sovereignty_reading, non_permanent_member_states).
narrative_ontology:constraint_vindicates(article_27_veto_power__sovereignty_reading, state_consent_doctrine).
narrative_ontology:constraint_vindicates(article_27_veto_power__sovereignty_reading, sovereign_equality_of_enforcement_capacity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Each holds a veto that cannot be overridden by any voting bloc or procedural maneuver. From this reading's vantage, the veto is not a privilege granted by the Charter but a formal recognition of a fact already true outside it: no combination of other states can compel a nuclear-armed, globally-deployable military power to act against its will without war. The veto simply lets that reality register in the institution's voting rule instead of being discovered later through non-compliance or armed confrontation.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, permanent_five_states, beneficiary,
    institutional, civilizational, arbitrage, global).

% Serve rotating two-year terms with full votes on Council business but no veto. Under this reading their absence of veto power is not a grievance to be litigated against the Charter's fairness — it reflects that they lack the independent global-enforcement capacity whose absence-of-consent the veto is registering. They can vote, deliberate, and shape outcomes short of a P5 veto being invoked, but cannot compel a P5 state to act.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, non_permanent_member_states, payer,
    moderate, biographical, constrained, national).

% Represents near-universal state membership and passes resolutions by majority, but has no binding enforcement authority over Council matters. Under the sovereignty reading, this is not an injustice correctable by charter reform — the Assembly's numerical majority does not correspond to enforcement capacity, and binding a P5 state through Assembly majority alone would not change the underlying fact that only the P5 states can compel compliance through force at global reach.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, un_general_assembly, excluded,
    organized, generational, constrained, global).

% Analyze whether Article 27(3) codifies a pre-existing structural fact of the international system (no enforceable law without the consent of the entity capable of resisting enforcement) or merely one historically contingent settlement among several. This reading holds the former; sibling readings (coordination, oligopoly) hold variants of the latter.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents the Security Council from issuing formally binding resolutions that a nuclear-armed, globally-enforcement-capable state has not consented to — because no such resolution could be enforced against that state's will without triggering the confrontation the Council exists to avoid.
% TRANSFER_FUNCTION: Under this reading nothing is transferred: the veto does not move authority from the general membership to the P5, it registers a distribution of enforcement capacity that already exists independent of the Charter. What looks like a transfer under other readings is, in this reading, a recognition.
% ABSENT_VOICES: States without global enforcement capacity — the vast majority of UN membership — would object that formal sovereign equality (one state, one vote in principle) is undermined by informal enforcement asymmetry. This reading does not dispute the asymmetry; it holds the asymmetry is prior to and independent of the Charter, so the veto tracks rather than creates it.
% DISAPPEARANCE_RATIONALE: If Article 27(3) were struck from the Charter tomorrow, this reading holds the underlying structural fact — that no institution can compel a nuclear power with global reach to act against its will without risking war — would persist unchanged; only the formal registration of that fact would vanish, and the Council would likely re-derive an equivalent informal veto through non-compliance and withdrawal threats. Sibling readings dispute this: the oligopoly reading holds removal would materially open institutional evolution.
% FOUNDING_PROBLEM: How to create a collective security body with universal membership while avoiding formal obligations that no enforcement mechanism could actually compel a nuclear-armed great power to honor.
% FOUNDING_PROBLEM_CORROBORATION: Military historians and international-relations realists outside the P5 states (e.g., analyses of Cold War-era Council paralysis and post-1991 unipolar/multipolar transitions) attest that no Security Council resolution has ever been enforced against a P5 state's core security interest without either its acquiescence or a war the Charter was designed to prevent — corroboration external to the P5's own diplomatic justifications for retaining the veto.
narrative_ontology:disappearance_verdict(article_27_veto_power__sovereignty_reading, contested).
narrative_ontology:founding_problem_status(article_27_veto_power__sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__sovereignty_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_27_veto_power__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__sovereignty_reading, 0.06, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__sovereignty_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, ExtMetricName, E),
    domain_priors:suppression_score(article_27_veto_power__sovereignty_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(article_27_veto_power__sovereignty_reading),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(article_27_veto_power__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near-zero (0.06) because under this reading, removing the veto would not redistribute anything real — any successor institution empowered to bind a nuclear great power against its will would face the identical enforcement problem, meaning the veto extracts nothing that would otherwise flow elsewhere; it merely formalizes a non-negotiable fact. Suppression is authored low (0.08): the constraint does not need active coercive maintenance because it is not being defended against an alternative that could actually replace it — no alternative enforcement mechanism exists that could bind a P5 state, so there is nothing to suppress. Accessibility collapse is authored high (0.88): once the underlying enforcement-capacity asymmetry is understood, alternative institutional designs (majority-binding votes over P5 objection) are recognized as unworkable rather than merely undesirable. Resistance is authored low (0.15): most resistance to the veto (from non-P5 states, from reform advocates) targets the Charter provision, not the underlying enforcement-capacity fact this reading holds the provision to be tracking — under this reading that resistance is aimed at the wrong target.
 *
 * DIRECTIONALITY LOGIC:
 *   The P5 states are declared beneficiaries here for FSM-triggering purposes, not because this reading holds they extract from anyone — the reading explicitly denies a transfer function. The beneficiary declaration exists so the false-summit-mountain check can run and register: does this mountain claim hold up when a concentrated beneficiary group is named? The reading's own position is that the P5's benefit (freedom from being bound without consent) is symmetric with what any similarly-capable state would hold — it is a structural position, not a captured rent. Non-permanent members and the General Assembly are payers/excluded only in the sense of lacking the veto, not in the sense of being extracted from by it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — avoiding a Charter that promises binding authority over great powers no institution can actually compel — is authored as still live: the enforcement-capacity gap between P5 and non-P5 states has not closed since 1945 (if anything nuclear proliferation among the P5 plus their conventional global-reach capacity has kept it durable). This blocks a mandatrophy read: a live founding problem plus persistence is coordination continuing to function, not a scaffold whose sunset has been missed. The disappearance_verdict is authored 'contested' rather than 'world_unchanged' precisely because the sibling readings dispute this: the oligopoly reading holds the world would rearrange favorably if the veto vanished (institutional evolution would follow); this reading holds it would functionally re-derive itself. That contest is the kernel's live fault line, not resolved here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_as_natural_fact_or_constructed_privilege,
    'Is the P5 veto a formal registration of a structural fact about global enforcement-capacity distribution (this reading), or a constructed privilege that a differently-designed institution could have avoided granting (the oligopoly reading''s premise)?',
    'Comparative institutional analysis: examine whether any historical or contemporary international enforcement body has successfully bound a militarily undefeated nuclear-armed great power against its explicit will, without triggering war or that state''s exit. Absence of any such case across 80 years supports the natural-fact reading; a single clear counterexample would substantially weaken it.',
    'If resolved toward ''constructed privilege,'' this story''s mountain classification collapses and the constraint should be re-read through the oligopoly framing (tangled_rope or snare) rather than mountain. If resolved toward ''structural fact,'' the mountain claim is corroborated independent of Charter design choices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_as_natural_fact_or_constructed_privilege, conceptual, 'Whether the veto tracks a pre-existing structural fact of power distribution or is itself a negotiated, potentially-otherwise institutional design choice.').

omega_variable(
    beneficiary_declaration_and_fsm_trigger,
    'Does naming permanent_five_states as a beneficiary correctly trigger false-summit-mountain scrutiny, or does it mischaracterize a symmetric structural position as a captured benefit?',
    'Compare P5 states'' position to a counterfactual: would a currently non-P5 state that acquired equivalent nuclear and global-enforcement capacity be granted equivalent veto standing under this reading''s logic? If yes (the position is capacity-contingent, not identity-contingent), the beneficiary framing is a labeling artifact of naming actual current P5 states rather than evidence of extraction. If no (the position is locked to the five historical victors regardless of capacity shifts), the beneficiary framing understates genuine capture.',
    'If capacity-contingent, this supports the mountain reading surviving FSM scrutiny (no identifiable actor benefits from the RULE per se, only from possessing the capacity the rule tracks). If identity-locked regardless of capacity, this reading''s mountain claim is substantially weakened and shifts toward the oligopoly reading''s entrenchment critique.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_declaration_and_fsm_trigger, empirical, 'Whether P5 beneficiary status is contingent on enforcement capacity (supporting mountain) or locked to 1945 victor identity regardless of capacity shifts (supporting oligopoly).').

omega_variable(
    kernel_reading_selection_evidence,
    'What in the Charter''s drafting history and subsequent practice favors reading Article 27(3) as sovereignty-tracking rather than as coordination-mechanism or oligopoly-entrenchment?',
    'Examine the 1945 San Francisco Conference negotiating record: were smaller states'' objections to the veto framed by P5 negotiators in enforcement-capacity terms (this reading) or in war-prevention terms (coordination reading)? Historical record shows both framings were used simultaneously by different P5 delegations for different audiences.',
    'If drafting history overwhelmingly favors one framing, that reading gains stronger evidentiary claim to represent the ''real'' constraint; if the framings were genuinely mixed and simultaneous from the outset, this supports treating all three readings as genuinely coexisting rather than one being more authentic than the others.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, conceptual, 'Whether historical evidence privileges the sovereignty framing over its sibling readings, or whether the ambiguity was present from the Charter''s founding.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__sovereignty_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_27_veto_power__sovereignty_reading, theater_ratio, 1945, 0.08).
narrative_ontology:measurement(arti_tr_t1960, article_27_veto_power__sovereignty_reading, theater_ratio, 1960, 0.09).
narrative_ontology:measurement(arti_tr_t1975, article_27_veto_power__sovereignty_reading, theater_ratio, 1975, 0.09).
narrative_ontology:measurement(arti_tr_t1990, article_27_veto_power__sovereignty_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(arti_tr_t2005, article_27_veto_power__sovereignty_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(arti_tr_t2025, article_27_veto_power__sovereignty_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_27_veto_power__sovereignty_reading, base_extractiveness, 1945, 0.04).
narrative_ontology:measurement(arti_be_t1960, article_27_veto_power__sovereignty_reading, base_extractiveness, 1960, 0.05).
narrative_ontology:measurement(arti_be_t1975, article_27_veto_power__sovereignty_reading, base_extractiveness, 1975, 0.05).
narrative_ontology:measurement(arti_be_t1990, article_27_veto_power__sovereignty_reading, base_extractiveness, 1990, 0.05).
narrative_ontology:measurement(arti_be_t2005, article_27_veto_power__sovereignty_reading, base_extractiveness, 2005, 0.06).
narrative_ontology:measurement(arti_be_t2025, article_27_veto_power__sovereignty_reading, base_extractiveness, 2025, 0.06).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(article_27_veto_power__sovereignty_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(article_27_veto_power__sovereignty_reading, article_27_veto_power__coordination_reading).
narrative_ontology:affects_constraint(article_27_veto_power__sovereignty_reading, article_27_veto_power__oligopoly_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the article_27_veto_power kernel, all keyed to the identical Charter text (Article 27(3)). sovereignty_reading (this story) claims mountain with near-zero ε on the premise that the veto tracks a pre-existing structural fact about enforcement-capacity distribution rather than creating or entrenching anything. coordination_reading claims the veto is a functional war-prevention mechanism (likely rope or tangled_rope, moderate ε, coordination framing dominant). oligopoly_reading claims the veto is entrenched rent extraction by an incumbent power cartel using Charter immutability to block redistribution (likely snare or tangled_rope, high ε, explicit beneficiary/victim framing). The three stories share the same kernel_id and reading structure but diverge sharply on claimed_type and ε — this divergence is the object of study, not an inconsistency to reconcile. Each story stands as its own ε-invariant constraint per the decomposition principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
