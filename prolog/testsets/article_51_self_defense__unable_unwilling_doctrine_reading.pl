% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__unable_unwilling_doctrine_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: article_51_self_defense__unable_unwilling_doctrine_reading
 *   human_readable: Article 51 Self-Defense: Unable/Unwilling Host State Doctrine
 *   domain: international_law/security_studies/constitutional_interpretation
 *
 * SUMMARY:
 *   The 'unable or unwilling' doctrine is a reading of UN Charter Article 51
 *   that permits states to use force in another state's territory against
 *   non-state actors when that host state is unable or unwilling to suppress
 *   the threat itself. This reading emerged post-2001 as states grappled with
 *   transnational terrorist networks operating in ungoverned spaces. The
 *   doctrine sits between two extreme interpretations: the narrow reading
 *   (self-defense only against state-attributable armed attacks) and the
 *   expansive reading (preventive force against emerging threats). The
 *   unable/unwilling doctrine is a middle ground—it requires an actual
 *   non-state attack to have occurred but permits unilateral response when
 *   the host state fails to act. This constraint story instantiates ONLY the
 *   unable/unwilling reading as a clean, ε-invariant construction. The
 *   sibling readings (narrow and expansive) are separate constraint stories;
 *   they are linked via network.affects_constraints but not described here.
 *
 * KEY AGENTS:
 *   - Intervening States (US, Israel, Turkey, UAE, etc.): Powerful institutional actors with counterterrorism mandates; benefit from the doctrine because it provides legal justification for operations they view as militarily necessary; high arbitrage exit (can reframe as preventive, use covert ops, or negotiate with host states).
 *   - Host States with Weak Capacity (Somalia, Syria, Yemen, Pakistan border regions): Moderate power, constrained exit; bear the cost of sovereignty breach and loss of territorial control; labeled 'unable' or 'unwilling' in ways they cannot easily contest without admitting weakness.
 *   - Host State Governments: Identity-locked position; benefit tacitly from burden-shifting but cannot admit it politically; caught between avoiding intervention (by appearing capable) and enabling intervention (by admitting incapacity).
 *   - International Court of Justice: Observer seat; interprets Article 51 and adjudicates claimed self-defense; shapes the doctrine's legitimacy and scope through case law.
 *   - Sovereignty Doctrine Advocates (legal scholars, UNGA, human rights bodies): Analytical observers who contest the doctrine's expansion and propose narrower scope; argue for explicit burden of proof, diplomatic exhaustion, or UN involvement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__unable_unwilling_doctrine_reading, 0.68).
domain_priors:suppression_score(article_51_self_defense__unable_unwilling_doctrine_reading, 0.72).
domain_priors:theater_ratio(article_51_self_defense__unable_unwilling_doctrine_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__unable_unwilling_doctrine_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__unable_unwilling_doctrine_reading, "Article 51 Self-Defense: Unable/Unwilling Host State Doctrine").
narrative_ontology:topic_domain(article_51_self_defense__unable_unwilling_doctrine_reading, "international_law/security_studies/constitutional_interpretation").

domain_priors:requires_active_enforcement(article_51_self_defense__unable_unwilling_doctrine_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__unable_unwilling_doctrine_reading, '2f06272c-369b-4615-9a0a-60fd8a9e5b7e').
narrative_ontology:cs_kernel_codification('2f06272c-369b-4615-9a0a-60fd8a9e5b7e', fixed_text).
narrative_ontology:cs_authority_grounding('2f06272c-369b-4615-9a0a-60fd8a9e5b7e', lineage).
narrative_ontology:cs_interpretation_layer_present('2f06272c-369b-4615-9a0a-60fd8a9e5b7e').
narrative_ontology:cs_reading_relation('2f06272c-369b-4615-9a0a-60fd8a9e5b7e', article_51_self_defense__narrow_armed_attack_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f06272c-369b-4615-9a0a-60fd8a9e5b7e', article_51_self_defense__expansive_preventive_reading, influences).
narrative_ontology:cs_axiom('2f06272c-369b-4615-9a0a-60fd8a9e5b7e', foundational, non_state_actor_triggered_self_defense).
narrative_ontology:cs_axiom_status(non_state_actor_triggered_self_defense, holdable).
narrative_ontology:cs_axiom_grounding('2f06272c-369b-4615-9a0a-60fd8a9e5b7e', non_state_actor_triggered_self_defense, empirically_contingent).
narrative_ontology:cs_axiom('2f06272c-369b-4615-9a0a-60fd8a9e5b7e', foundational, host_state_incapacity_justifies_intervention).
narrative_ontology:cs_axiom_status(host_state_incapacity_justifies_intervention, holdable).
narrative_ontology:cs_axiom_grounding('2f06272c-369b-4615-9a0a-60fd8a9e5b7e', host_state_incapacity_justifies_intervention, instrumental).
narrative_ontology:cs_reference_frame('2f06272c-369b-4615-9a0a-60fd8a9e5b7e', westphalian_sovereignty_with_self_defense_exception).
narrative_ontology:cs_drift_state('2f06272c-369b-4615-9a0a-60fd8a9e5b7e', contemporary_post_2020s, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2f06272c-369b-4615-9a0a-60fd8a9e5b7e', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_counterterrorism_mandates).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, host_states_with_weak_capacity).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__unable_unwilling_doctrine_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(article_51_self_defense__unable_unwilling_doctrine_reading, 'none', 1).

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
 *   The extractiveness (0.68 at interval end) reflects that the doctrine asymmetrically benefits intervening powerful states while imposing sovereignty costs on weak-capacity host states. The ratio is not at snare-level extremes (0.85+) because the doctrine does require an actual non-state attack to have occurred (not purely preventive), and because some host states tacitly benefit from burden-shifting. Suppression is substantial (0.72) because the doctrine's persistence depends on actively defending against narrow readings that would constrain it and against sovereignty doctrine advocates who call for stricter limits. Theater ratio is moderate-high (0.48) because increasing shares of doctrine deployment cite counterterrorism necessity while expanding the scope of targets and lowering the proof standard for 'unable' and 'unwilling'—the performative defense of the doctrine is rising relative to its core coordinating function. Accessibility collapse is moderate (0.62): host states see alternatives (diplomatic negotiation, UN involvement, covert cooperation) but find them blocked or less appealing; intervening states face moderate friction from international legal norms but can navigate them. Resistance is high (0.71) because sovereignty doctrine advocates, international courts, and some host state coalitions actively contest the doctrine and propose narrower readings.
 *
 * PERSPECTIVAL GAP:
 *   The intervening-state seat should compute the constraint as beneficial coordination (they see the unable/unwilling problem as real, its solution as necessary). The host-state seat should compute it as extractive authority (they see sovereignty bypass as coercive and the 'unable/unwilling' label as imposed rather than consensual). The ICJ seat computes it as a complex legal rule with genuine tension between self-defense rights and sovereignty norms. The sovereignty advocates compute it as a cover story for power asymmetry. The engine derives these divergences from the structural data: beneficiary seat gets low d (subsidized), victim seat gets high d (extracted from), observer seats get intermediate d. The doctrine itself claims coordination; the metrics describe asymmetric extraction that requires active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Intervening states with powerful institutional capacity and arbitrage options (alternative legal framings, covert ops, diplomatic negotiation) sit at low d—they benefit from the doctrine and face low structural extraction. Host states with moderate power and constrained options (cannot afford civil war with intervening state, cannot easily contest 'unable/unwilling' labeling without admitting incapacity) sit at high d—they bear the costs. Host state governments occupy an unusual high-d middle position: they benefit tacitly from burden-shifting but are identity-locked by sovereignty claims, so they cannot admit the benefit. The derivation should reflect this: high-d for the sovereignty cost, modulated downward only if data shows explicit host-state consent or reciprocal benefit-sharing arrangements (rarely present). ICJ and advocacy seats are analytical (d near 0.5 or independent of the constraint).
 *
 * MANDATROPHY ANALYSIS:
 *   The unable/unwilling doctrine is NOT a mandatrophy case yet. The founding problem (transnational non-state actor networks in ungoverned spaces) is contested but still arguably live: terrorist organizations like ISIS-K (post-Afghanistan), Al-Qaeda franchises (Sahel, Somalia), and Houthi networks (Yemen) continue to mount attacks from weak-state territory. However, the doctrine shows signs of mandate-slippage: it increasingly justifies counterterrorism operations broader than response to imminent threats (drone strikes on individuals based on pattern-of-life analysis rather than specific attack planning), and the 'unable' and 'unwilling' bar has dropped over time (a state is now deemed 'unwilling' if it does not cooperate with the intervening state, even if it has suppressed the specific threat). The measurement series shows rising theater_ratio (performative defense of the doctrine rising) and rising extractiveness relative to the founding problem's severity, suggesting the constraint is shifting from coordination to pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unable_vs_unwilling_boundary,
    'What is the structural difference between a host state that is ''unable'' to suppress non-state threats (lacks capacity) versus ''unwilling'' (has capacity but chooses not to act or benefits from the threat)? How should intervening states prove which condition holds?',
    'International court judgments establishing evidentiary standards for ''unable'' vs. ''unwilling''; diplomatic correspondence and UN proceedings documenting host state capacity claims; independent capacity assessments by UN bodies or regional organizations.',
    'If ''unable'' and ''unwilling'' require different proof standards, the doctrine''s scope narrows (unable requires aid and capacity-building; unwilling requires formal diplomatic notice and host-state consent). If they are treated as equivalent, the doctrine permits intervention on the basis of host-state non-cooperation alone, expanding the doctrine into a pure sovereignty-bypass mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unable_vs_unwilling_boundary, conceptual, 'Boundary ambiguity between incapacity and non-cooperation grounds for intervention.').

omega_variable(
    coordination_vs_power_extraction,
    'Does the unable/unwilling doctrine solve a genuine coordination problem (host state capacity crisis makes self-help necessary), or does it provide legal cover for power-based territorial control that would occur regardless of legal framing?',
    'Counterfactual analysis: if the doctrine were abolished tomorrow, would intervening states cease counterterrorism operations in weak-state territory, or would they reframe them (preventive doctrine, covert ops, or bilateral agreements) and continue? Scholarly consensus on whether alternative coordination mechanisms (UN peacekeeping, capacity-building, host-state consent regimes) could solve the founding problem.',
    'If the doctrine provides cover for operations that would occur anyway, it is pure extraction dressed as coordination—reclassify to snare. If operations genuinely depend on legal justification (intervening states avoid weak-state territory without legal cover), the doctrine solves a real coordination problem and remains tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_power_extraction, conceptual, 'Whether the constraint genuinely solves a coordination problem or merely legitimizes power asymmetry.').

omega_variable(
    host_state_identity_lock_mechanism,
    'To what extent is host-state acceptance of the unable/unwilling doctrine driven by genuine incapacity versus by the identity-lock dynamic (admitting incapacity undermines state legitimacy, so states tacitly tolerate intervention to avoid admitting weakness)?',
    'Post-intervention analysis of host-state public statements, internal deliberations (leaked documents, interviews), and willingness to contest ''unable/unwilling'' characterizations in international forums. Comparison across host states with similar capacity but different political regimes (do authoritarian states resist the doctrine less than democracies facing domestic legitimacy costs?).',
    'If identity-lock is the primary suppression mechanism, the constraint''s effective suppression is higher than the structural measure suggests, and the doctrine is more extractive than the base metrics indicate. It would also suggest that the doctrine is structurally unstable—as states'' identities shift or legitimacy recalculates, resistance will rise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(host_state_identity_lock_mechanism, empirical, 'Role of identity-lock in host-state suppression and constraint stability.').

omega_variable(
    kernel_reading_contest_underdetermination,
    'The three readings of Article 51 (narrow, unable/unwilling, expansive) arise from contested interpretations of the same constitutional text. Is the text genuinely ambiguous, or does it more clearly favor one reading, with other readings imposing doctrinal overlay?',
    'Textual originalism: what did the Charter''s drafters intend by ''armed attack'' and ''self-defence''? Historical evidence from San Francisco drafting sessions; comparisons with parallel texts in regional human rights instruments; evolution of state practice since 1945.',
    'If the text is ambiguous, all three readings remain live and no single reading can claim unique legitimacy (coexists_with relation holds across all three). If the text favors the narrow reading, the unable/unwilling and expansive readings are doctrinal additions that can be challenged; if it favors the expansive reading, the narrow reading is an artificial constraint (forecloses relation). Different conclusions shift the stability and contestation level of this constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_underdetermination, conceptual, 'Textual determinacy of Article 51 and the legitimacy status of alternative readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__unable_unwilling_doctrine_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(arti_tr_t5, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 5, 0.37).
narrative_ontology:measurement(arti_tr_t10, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement(arti_tr_t15, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 15, 0.46).
narrative_ontology:measurement(arti_tr_t20, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 20, 0.47).
narrative_ontology:measurement(arti_tr_t25, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 25, 0.48).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(arti_be_t5, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(arti_be_t10, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(arti_be_t15, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(arti_be_t20, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(arti_be_t25, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(arti_su_t5, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(arti_su_t10, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(arti_su_t15, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(arti_su_t20, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(arti_su_t25, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__unable_unwilling_doctrine_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_51_self_defense__unable_unwilling_doctrine_reading, 0.12).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_self_defense__narrow_armed_attack_reading).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_self_defense__expansive_preventive_reading).

% DUAL FORMULATION NOTE:
% The Article 51 self-defense kernel has three distinct readings that instantiate different constraints with different ε values and structural beneficiary/victim sets. The unable/unwilling doctrine reading (this story) sits between the narrow reading (lowest ε, minimal extraction, respects state sovereignty strictly) and the expansive reading (highest ε, permits preventive force). The three stories form a kernel family linked via network.affects_constraints. The unable/unwilling reading influences both siblings: it provides middle-ground legitimacy that can swing courts and state consensus toward either pole, depending on how proof standards for 'unable' and 'unwilling' are set. The narrow reading coexists with this one (different legal communities hold both simultaneously) but constrains its scope through judicial interpretation. The expansive reading lies downstream: it takes the unable/unwilling rationale and extends it to purely preventive contexts, removing the requirement for an actual non-state attack.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_51_self_defense__unable_unwilling_doctrine_reading, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
