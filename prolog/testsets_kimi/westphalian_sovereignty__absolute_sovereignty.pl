% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__absolute_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__absolute_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__absolute_sovereignty
 *   human_readable: Westphalian Absolute Sovereignty
 *   domain: international_law/political_philosophy/global_governance
 *
 * SUMMARY:
 *   This constraint instantiates the absolute_sovereignty reading of the
 *   westphalian_sovereignty kernel: the claim that sovereignty grants states
 *   unconditional authority over domestic affairs and that external
 *   interference is categorically illegitimate. This reading is distinguished
 *   from conditional_sovereignty (responsibility triggers intervention) and
 *   graduated_sovereignty (capacity and legitimacy determine sovereign
 *   standing). Under the absolute reading, state executivesâparticularly
 *   authoritarian regimesâare the primary beneficiaries of a
 *   non-interference shield, while domestic populations under repressive
 *   governance bear the costs of blocked external protection. The constraint
 *   is authored as tangled_rope because it retains a genuine coordination
 *   function (mutual non-interference prevents interstate chaos) while
 *   asymmetrically extracting from trapped domestic populations who lose
 *   humanitarian recourse.
 *
 * KEY AGENTS:
 *   - Authoritarian regimes: Primary beneficiary (institutional/arbitrage) â collect impunity through the sovereignty shield.
 *   - Liberal democratic states: Dual-position beneficiary-payer (institutional/arbitrage) â benefit from the shield but are constrained when seeking to intervene.
 *   - Domestic populations under repression: Primary target (powerless/trapped) â bear the extraction through loss of external protection.
 *   - Human rights advocacy networks: Excluded voice (organized/constrained) â documented atrocities are filtered out by the consent requirement.
 *   - International Court of Justice: Analytical observer (institutional/analytical) â adjudicates within the sovereignty frame it cannot escape.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, 0.55).
domain_priors:suppression_score(westphalian_sovereignty__absolute_sovereignty, 0.7).
domain_priors:theater_ratio(westphalian_sovereignty__absolute_sovereignty, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, extractiveness, 0.55).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__absolute_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalian_sovereignty__absolute_sovereignty, "Westphalian Absolute Sovereignty").
narrative_ontology:topic_domain(westphalian_sovereignty__absolute_sovereignty, "international_law/political_philosophy/global_governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__absolute_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__absolute_sovereignty, 'c5775d58-c24e-45f8-babe-9001e68ee1f2').
narrative_ontology:cs_kernel_codification('c5775d58-c24e-45f8-babe-9001e68ee1f2', formalized).
narrative_ontology:cs_authority_grounding('c5775d58-c24e-45f8-babe-9001e68ee1f2', lineage).
narrative_ontology:cs_interpretation_layer_present('c5775d58-c24e-45f8-babe-9001e68ee1f2').
narrative_ontology:cs_reading_relation('c5775d58-c24e-45f8-babe-9001e68ee1f2', westphalian_sovereignty__conditional_sovereignty, forecloses).
narrative_ontology:cs_reading_relation('c5775d58-c24e-45f8-babe-9001e68ee1f2', westphalian_sovereignty__graduated_sovereignty, forecloses).
narrative_ontology:cs_axiom('c5775d58-c24e-45f8-babe-9001e68ee1f2', foundational, domestic_jurisdiction_is_absolute).
narrative_ontology:cs_axiom_status(domestic_jurisdiction_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('c5775d58-c24e-45f8-babe-9001e68ee1f2', domestic_jurisdiction_is_absolute, conventional).
narrative_ontology:cs_axiom('c5775d58-c24e-45f8-babe-9001e68ee1f2', foundational, external_intervention_categorically_illegitimate).
narrative_ontology:cs_axiom_status(external_intervention_categorically_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('c5775d58-c24e-45f8-babe-9001e68ee1f2', external_intervention_categorically_illegitimate, conventional).
narrative_ontology:cs_reference_frame('c5775d58-c24e-45f8-babe-9001e68ee1f2', classical_territorial_supremacy).
narrative_ontology:cs_drift_state('c5775d58-c24e-45f8-babe-9001e68ee1f2', post_r2p_consensus_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c5775d58-c24e-45f8-babe-9001e68ee1f2', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, authoritarian_regimes).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, liberal_democratic_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, domestic_populations_under_repression).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, liberal_democratic_states).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__absolute_sovereignty, non_intervention_doctrine).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__absolute_sovereignty, state_equality_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invoke the absolute sovereignty norm to deflect international pressure, sanctions, and humanitarian intervention. Claim territorial integrity and non-interference to maintain domestic control without external accountability. Collect the primary extraction: impunity for internal repression.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, authoritarian_regimes, beneficiary,
    institutional, generational, arbitrage, global).

% Benefit from the general non-interference shield against unwanted external meddling in their own affairs. Simultaneously constrained when they seek to intervene in humanitarian crises or promote democratic transitions, which creates friction between their values and the legal norm they uphold.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, liberal_democratic_states, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__absolute_sovereignty, liberal_democratic_states, payer).

% Bear the costs of unchecked state violence and systematic repression because the absolute sovereignty norm blocks external protective intervention. Cannot easily exit their state's jurisdiction and lack standing in the international legal structure that recognizes their government's authority over them.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, domestic_populations_under_repression, payer,
    powerless, biographical, trapped, national).

% Document atrocities and lobby for intervention but are structurally excluded from the sovereignty-recognition framework. Their claims are filtered through state consent and UN Security Council politics, which treat external intervention as categorically illegitimate under the absolute reading.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, human_rights_advocacy_networks, excluded,
    organized, biographical, constrained, global).

% Adjudicates interstate disputes but cannot hear cases brought by individuals against their own states without consent. Its jurisdiction reinforces the sovereignty shield, and its advisory role is circumscribed by the same non-interference principles it is asked to interpret.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, international_court_of_justice, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalian_sovereignty__absolute_sovereignty, authoritarian_regimes).
narrative_ontology:fixing_cost_class(westphalian_sovereignty__absolute_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents constant interstate war and intervention by establishing mutual non-interference; allows states to coexist without each one asserting the right to override another's internal order.
% TRANSFER_FUNCTION: Transfers authority and impunity from the international community to state executives, moving the cost of repression from the international enforcement system to domestic populations who lose external recourse.
% ABSENT_VOICES: Domestic populations under repressive rule and humanitarian intervention advocates are formally excluded from the sovereignty-recognition framework; their testimony enters only if filtered through state consent or Security Council authorization.
% DISAPPEARANCE_RATIONALE: If absolute sovereignty vanished overnight, the normative barrier to humanitarian intervention, cross-border human rights enforcement, and international criminal jurisdiction would collapse. States would face immediate pressure to justify domestic conduct by international standards, and authoritarian regimes would lose their primary legal shield.
% FOUNDING_PROBLEM: Religious wars and dynastic conflicts in early modern Europe demonstrated the chaos of unlimited external intervention in domestic affairs; the Peace of Westphalia sought to stabilize interstate order by fixing territorial authority.
% FOUNDING_PROBLEM_CORROBORATION: International relations historians and critical legal scholars outside the state-beneficiary set attest that the original problem of dynastic and religious warfare has been replaced by problems of intra-state atrocity and global governance gaps; these corroborating sources note the arrangement now protects repressive stability rather than resolving interstate anarchy.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__absolute_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__absolute_sovereignty, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__absolute_sovereignty, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(westphalian_sovereignty__absolute_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__absolute_sovereignty, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__absolute_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__absolute_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) is moderate-high because the shield is not pure extractionâit genuinely coordinates interstate behaviorâbut it extracts heavily from repressed populations by blocking their external recourse. Suppression (0.70) is high because the norm actively suppresses alternative frameworks such as Responsibility to Protect and unilateral humanitarian intervention. Theater ratio (0.40) reflects significant performative maintenance at the UN General Assembly and in diplomatic rhetoric, alongside real structural enforcement through the Security Council veto and non-interference taboo. Accessibility collapse (0.60) captures that alternatives exist in theory but are structurally collapsed by the consent requirement and veto architecture. Resistance (0.55) registers sustained opposition from human rights networks and some liberal states that seek to soften the absolute shield.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of an authoritarian regime, the constraint is a protective rope guaranteeing survival and interstate order; from the seat of a domestic population facing systematic repression, the same constraint is a snare that blocks external rescue. The engine computes this divergence from the structural dataâabsolute sovereignty is coordination for the state and extraction for the individual under that state's power.
 *
 * DIRECTIONALITY LOGIC:
 *   State executives (both authoritarian and democratic) sit near the beneficiary end: they collect the non-interference shield and can arbitrage sovereignty claims strategically depending on geopolitical convenience. Domestic populations sit at the full-target end: they pay with loss of external protection and are trapped by citizenship and territorial jurisdiction. Human rights advocates are excluded rather than coordinatedâtheir inclusion would collapse the absolute reading by introducing conditionalities.
 *
 * MANDATROPHY ANALYSIS:
 *   The absolute reading prevents mislabeling by requiring both coordination (interstate stability) and extraction (domestic impunity) to be present. If it were pure coordination, victims would be absent; if it were pure extraction, the coordination function (preventing interstate war) would not be historically grounded. The tangled_rope classification captures this hybridity and prevents either the apologist framing (pure rope) or the cynical framing (pure snare) from dominating.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    r2p_penetration_depth,
    'Has the Responsibility to Protect doctrine sufficiently penetrated the absolute sovereignty norm to constitute a structural shift, or does it remain an exception that proves the rule?',
    'Comparative case analysis of intervention frequency and Security Council authorization rates pre- and post-2005 World Summit Outcome Document.',
    'If R2P is structural, the absolute reading is drifting toward conditional sovereignty and extractiveness should be measured as declining; if cosmetic, absolute sovereignty remains intact and the extraction is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(r2p_penetration_depth, empirical, 'Whether R2P constitutes structural erosion or diplomatic theater.').

omega_variable(
    regime_type_asymmetry,
    'Does the absolute sovereignty shield benefit all regimes equally, or does it asymmetrically advantage authoritarian states by blocking democratic conditionalities?',
    'Statistical analysis of sovereignty invocations by regime type and intervention targets across the post-1945 corpus.',
    'If asymmetric, the beneficiary structure is tilted and extraction concentrates on populations under authoritarian rule, reinforcing the tangled_rope classification; if symmetric, the cost is distributed more evenly across all domestic populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_type_asymmetry, empirical, 'Whether the sovereignty shield is equally valuable to all regime types.').

omega_variable(
    domestic_consent_fiction,
    'To what extent does the absolute sovereignty reading depend on the fiction that state executives represent domestic populations?',
    'Comparative legitimacy metrics, election monitoring data, and popular attitude surveys in repressive states.',
    'If representation is widely fictional, the directionality derivation treating states as beneficiaries systematically masks the true victimhood of domestic populations, and effective extraction is higher than the structural measure suggests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(domestic_consent_fiction, conceptual, 'Whether the state-beneficiary directionality rests on a representational fiction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__absolute_sovereignty, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 0, 0.2).
narrative_ontology:measurement(west_tr_t15, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 15, 0.28).
narrative_ontology:measurement(west_tr_t30, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 30, 0.35).
narrative_ontology:measurement(west_tr_t45, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 45, 0.4).
narrative_ontology:measurement(west_tr_t60, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 60, 0.42).
narrative_ontology:measurement(west_tr_t70, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 70, 0.4).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(west_be_t15, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(west_be_t30, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(west_be_t45, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 45, 0.5).
narrative_ontology:measurement(west_be_t60, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 60, 0.54).
narrative_ontology:measurement(west_be_t70, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 70, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(west_su_t15, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(west_su_t30, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(west_su_t45, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 45, 0.72).
narrative_ontology:measurement(west_su_t60, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 60, 0.71).
narrative_ontology:measurement(west_su_t70, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 70, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
