% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__sovereignty_maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__sovereignty_maximalist_reading, []).

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
 *   constraint_id: rbio_practice_norm_complex__sovereignty_maximalist_reading
 *   human_readable: RBIO Sovereignty-Maximalist Reading: State Sovereignty as Absolute Barrier to Intervention
 *   domain: international_relations/international_law/political_economy
 *
 * SUMMARY:
 *   The RBIO (Rules-Based International Order) practice-norm complex is a
 *   contested kernel with three live readings. This constraint story
 *   instantiates the sovereignty-maximalist reading: state sovereignty is
 *   absolute; RBIO norms are legitimate only when they protect sovereignty
 *   against external interference; humanitarian exceptions are pretexts for
 *   regime change. The reading functions as a tangled rope: it provides
 *   genuine coordination (preventing great-power conflict via clear
 *   non-intervention rules) while simultaneously extracting
 *   protection-from-accountability for authoritarian regimes at the expense
 *   of populations under repression. The constraint is actively enforced
 *   through Security Council vetoes, diplomatic coalitions, treaty
 *   interpretations, and domestic 'foreign agent' laws. The theater ratio
 *   reflects the gap between the reading's proclaimed coordination function
 *   (systemic stability) and its operational extraction function (impunity
 *   for repression).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.78).
domain_priors:suppression_score(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.85).
domain_priors:theater_ratio(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__sovereignty_maximalist_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__sovereignty_maximalist_reading, "RBIO Sovereignty-Maximalist Reading: State Sovereignty as Absolute Barrier to Intervention").
narrative_ontology:topic_domain(rbio_practice_norm_complex__sovereignty_maximalist_reading, "international_relations/international_law/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__sovereignty_maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__sovereignty_maximalist_reading, '195ceaab-56cb-4037-8cbc-6748391cc4a1').
narrative_ontology:cs_kernel_codification('195ceaab-56cb-4037-8cbc-6748391cc4a1', formalized).
narrative_ontology:cs_authority_grounding('195ceaab-56cb-4037-8cbc-6748391cc4a1', lineage).
narrative_ontology:cs_interpretation_layer_present('195ceaab-56cb-4037-8cbc-6748391cc4a1').
narrative_ontology:cs_reading_relation('195ceaab-56cb-4037-8cbc-6748391cc4a1', rbio_practice_norm_complex__liberal_institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('195ceaab-56cb-4037-8cbc-6748391cc4a1', rbio_practice_norm_complex__hegemonic_extraction_reading, forecloses).
narrative_ontology:cs_axiom('195ceaab-56cb-4037-8cbc-6748391cc4a1', foundational, sovereignty_absolute_nonintervention).
narrative_ontology:cs_axiom_status(sovereignty_absolute_nonintervention, holdable).
narrative_ontology:cs_axiom_grounding('195ceaab-56cb-4037-8cbc-6748391cc4a1', sovereignty_absolute_nonintervention, conventional).
narrative_ontology:cs_axiom('195ceaab-56cb-4037-8cbc-6748391cc4a1', foundational, humanitarian_intervention_illegitimate_pretext).
narrative_ontology:cs_axiom_status(humanitarian_intervention_illegitimate_pretext, holdable).
narrative_ontology:cs_axiom_grounding('195ceaab-56cb-4037-8cbc-6748391cc4a1', humanitarian_intervention_illegitimate_pretext, conventional).
narrative_ontology:cs_reference_frame('195ceaab-56cb-4037-8cbc-6748391cc4a1', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('195ceaab-56cb-4037-8cbc-6748391cc4a1', contemporary_r2p_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('195ceaab-56cb-4037-8cbc-6748391cc4a1', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, sovereignty_maximalist_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, populations_under_repression).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, minority_groups_targeted_by_state).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, civil_society_actors_in_closed_states).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__sovereignty_maximalist_reading, westphalian_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__sovereignty_maximalist_reading, non_intervention_principle).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__sovereignty_maximalist_reading, humanitarian_intervention_as_regime_change_pretext).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Use absolute sovereignty norms to shield domestic repression from external scrutiny or action. The reading provides legal and rhetorical cover: any external pressure is framed as illegitimate intervention, and the regime can leverage veto-wielding allies in the Security Council to block enforcement. Gains legitimacy domestically by portraying sovereignty defense as anti-imperialism.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes, beneficiary,
    powerful, biographical, arbitrage, national).

% States that champion the sovereignty-maximalist reading in multilateral forums (e.g., China, Russia, and their coalition partners). They shape UNGA resolutions, Security Council vetoes, and treaty interpretations to entrench the reading. They benefit by constraining the liberal interventionist agenda and by normalizing a world order where internal governance is unreviewable. They administer the constraint through diplomatic coalitions and institutional blocking.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, sovereignty_maximalist_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Civilians living under repressive governments who bear the full cost of the sovereignty shield. They cannot exit the state's territory easily (closed borders, exit visas, asset freezes), and they have no effective voice in the international forums where the reading is authored and enforced. Their suffering is the externality the constraint externalizes.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, populations_under_repression, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__sovereignty_maximalist_reading, populations_under_repression, excluded).

% Ethnic, religious, or political minorities specifically targeted by state repression. The sovereignty-maximalist reading removes the only external mechanism (R2P, humanitarian intervention, targeted sanctions) that could alter their situation. They are structurally excluded from both domestic politics and international protection.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, minority_groups_targeted_by_state, payer,
    powerless, biographical, trapped, national).

% Local NGOs, journalists, lawyers, and activists who operate under constant threat. The reading legitimizes state laws that criminalize foreign funding, foreign contacts, and 'foreign agent' designations — cutting off their resources and legal protection. Some can flee (constrained exit), but the constraint raises the cost of dissent and international solidarity.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, civil_society_actors_in_closed_states, payer,
    moderate, biographical, constrained, national).

% States that formally endorse R2P and human rights enforcement but find their operational capacity neutralized by the sovereignty-maximalist reading's institutional vetos and diplomatic coalitions. They are not direct payers of the extraction but are structurally constrained from acting on their declared commitments.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, liberal_institutional_states, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__sovereignty_maximalist_reading, liberal_institutional_states, excluded).

% International lawyers, IR scholars, and norm researchers who track the contestation over RBIO meaning. They see the full structure: the reading is one of three coherent framings of the same kernel, each with different beneficiary/victim distributions and different implications for global order. Their seat experiences the constraint as an analytical object, not as a lived condition.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, analytical_observers_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes).
narrative_ontology:fixing_cost_class(rbio_practice_norm_complex__sovereignty_maximalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a stable international order by establishing a clear, bright-line rule: no external military intervention in domestic affairs. This solves the coordination problem of preventing great-power conflict over internal governance by removing the legal basis for intervention entirely. The coordination benefit is systemic stability and predictability for states.
% TRANSFER_FUNCTION: Transfers the cost of domestic repression from the repressing state to the repressed population, and transfers the burden of proof for intervention from the state to the would-be intervener (who must overcome the sovereignty presumption). The extraction is protection-from-accountability for authoritarian regimes, paid in human rights violations by their populations.
% ABSENT_VOICES: The populations under repression, minority groups targeted by the state, and civil society actors in closed states are structurally excluded from the forums where the sovereignty-maximalist reading is authored (UNGA, Security Council, treaty bodies). They would object to the reading's legitimacy but have no seat at the table. Their absence is not accidental — the reading's enforcement mechanism (veto, non-intervention norm) actively keeps them out.
% DISAPPEARANCE_RATIONALE: If the sovereignty-maximalist reading vanished overnight, the legal presumption against intervention would collapse. R2P and humanitarian intervention norms would become operative without veto obstruction. Authoritarian regimes would lose their primary shield. Populations under repression would gain potential external recourse. The international order would shift from Westphalian non-intervention to a contested but enforceable responsibility-to-protect framework. Great-power conflict risk would rise as intervention justifications proliferate.
% FOUNDING_PROBLEM: The post-WWII order needed a stable foundation to prevent great-power war. The Westphalian sovereignty principle — non-intervention in domestic affairs — was the coordination solution: it gave every state a recognized sphere of exclusive authority, making the system legible and conflict-limiting. The sovereignty-maximalist reading is the hard-line institutionalization of that solution.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (preventing great-power war through sovereignty rules) is attested by the UN Charter drafting history and by realist IR scholarship (e.g., Morgenthau, Waltz) — sources outside the authoritarian beneficiary set. However, liberal institutionalists and R2P advocates contest whether the founding problem remains the *only* live problem, arguing that mass atrocity prevention is a co-equal founding commitment (Universal Declaration, Genocide Convention). The status is contested because the kernel itself contains both commitments in tension.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__sovereignty_maximalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__sovereignty_maximalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__sovereignty_maximalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(rbio_practice_norm_complex__sovereignty_maximalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the reading transfers the full cost of repression from perpetrator states to victim populations while providing a legal-rhetorical shield that is actively enforced. Suppression (0.85) is very high because the reading's persistence depends on veto power, coalition discipline, and domestic legal machinery that criminalizes external engagement — alternatives (R2P, humanitarian corridors, targeted sanctions) are structurally suppressed. Theater ratio (0.42) is moderate and rising: the coordination function (conflict prevention) is real but increasingly performative as the reading is weaponized to block even consensual assistance. Accessibility collapse (0.68) reflects that once a state adopts the sovereignty-maximalist frame, alternative framings (R2P, conditional sovereignty) become diplomatically and legally inaccessible. Resistance (0.35) is low from the victim populations (trapped, powerless) but significant from liberal institutional states and civil society — the engine computes per-seat resistance from structural data.
 *
 * PERSPECTIVAL GAP:
 *   The sovereignty-maximalist states experience the constraint as genuine coordination (rope-like): it prevents the chaos of competing intervention claims and protects their own domestic autonomy. The repressed populations experience it as pure extraction (snare-like): a legal shield for their persecutors with no exit. Liberal institutional states experience it as a tangled rope: they value the coordination function but recognize the extraction. The engine computes these divergent per-seat classifications from the single structural dataset — this divergence is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Authoritarian regimes and sovereignty-maximalist states are structural beneficiaries (d near 0.0): they collect protection-from-accountability and institutional control. The agenda-setter states (China, Russia, coalition) administer the constraint and gain geopolitical stability. Populations under repression, targeted minorities, and civil society actors are structural targets (d near 1.0): they bear the extraction with trapped or constrained exit. Liberal institutional states sit near symmetric (d ~0.5): they have declared commitments to protection but are constrained by the reading's institutional architecture. Analytical observers are at the analytical seat (d = 0.5 by definition). The derivation chain produces these directionalities from the declared beneficiaries/victims + power + exit; no overrides are needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (great-power war prevention via sovereignty rules) is contested as still live. Realist scholarship and UN Charter history corroborate it. But the Genocide Convention, Universal Declaration, and R2P evolution show a co-equal founding commitment to atrocity prevention. The reading resolves the mandatrophy by freezing the 1945 coordination solution and declaring the 1948+ human rights layer illegitimate — it treats mandate drift as illegitimate expansion rather than co-equal evolution. This is a classic mandatrophy pattern: the constraint's mandate (sovereignty as conflict-prevention) has been extended to cover a domain (internal repression) where it functions as extraction, and the reading denies the drift by declaring the new domain outside the kernel's scope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_maximalist_reading_of_kernel,
    'Is this constraint a genuine coordination mechanism for international stability, or a constructed shield for authoritarian impunity?',
    'Counterfactual analysis: if the sovereignty-maximalist reading were removed from Security Council practice and treaty interpretation, would great-power conflict increase (coordination loss) or would atrocity prevention improve (extraction removal)? Historical cases: Kosovo 1999 (intervention without authorization), Libya 2011 (authorized intervention), Syria 2011+ (veto-blocked intervention).',
    'If coordination loss dominates, the reading is a genuine rope/tangled_rope. If extraction removal dominates with minimal conflict increase, it is a snare with a coordination cover story. The current tangled_rope classification reflects genuine ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_maximalist_reading_of_kernel, empirical, 'Whether the sovereignty-maximalist reading''s coordination function is genuine or pretextual.').

omega_variable(
    humanitarian_exception_pretext_claim,
    'Are humanitarian exceptions to sovereignty *structurally* pretexts for regime change, or is that a rhetorical claim that masks a genuine disagreement about intervention legitimacy?',
    'Case coding of all humanitarian interventions since 1990: code each for (a) UN authorization, (b) regime change outcome, (c) stated vs. actual objectives. If regime change consistently follows intervention regardless of mandate, the pretext claim gains empirical support.',
    'If pretext claim is empirically true, the reading''s extraction function is stronger (the shield blocks genuine protection). If false, the reading''s coordination function is stronger (it blocks a genuinely dangerous tool).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(humanitarian_exception_pretext_claim, empirical, 'Empirical status of the claim that humanitarian intervention is a regime-change pretext.').

omega_variable(
    conditionality_exit_without_cost,
    'Is ''conditionality acceptable only when target state can exit without cost'' a coherent structural position, or does it render all conditionality impossible (since any pressure imposes cost)?',
    'Analyze the reading''s own diplomatic practice: when has it accepted conditionality? What exit mechanisms were offered? If never, the condition is a theoretical escape hatch that functionally forecloses all conditionality.',
    'If the exit-without-cost condition is a null set, the reading is a snare (no coordination function survives). If it has been operationalized in practice, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditionality_exit_without_cost, conceptual, 'Whether the reading''s stated condition for legitimate conditionality is structurally real or a performative escape hatch.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__sovereignty_maximalist_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t1945, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1945, 0.15).
narrative_ontology:measurement(rbio_tr_t1960, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1960, 0.18).
narrative_ontology:measurement(rbio_tr_t1975, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1975, 0.22).
narrative_ontology:measurement(rbio_tr_t1990, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1990, 0.28).
narrative_ontology:measurement(rbio_tr_t2005, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2005, 0.35).
narrative_ontology:measurement(rbio_tr_t2015, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2015, 0.39).
narrative_ontology:measurement(rbio_tr_t2025, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(rbio_be_t1945, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement(rbio_be_t1960, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1960, 0.42).
narrative_ontology:measurement(rbio_be_t1975, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1975, 0.48).
narrative_ontology:measurement(rbio_be_t1990, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(rbio_be_t2005, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2005, 0.68).
narrative_ontology:measurement(rbio_be_t2015, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2015, 0.73).
narrative_ontology:measurement(rbio_be_t2025, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t1945, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1945, 0.45).
narrative_ontology:measurement(rbio_su_t1960, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1960, 0.52).
narrative_ontology:measurement(rbio_su_t1975, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1975, 0.6).
narrative_ontology:measurement(rbio_su_t1990, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(rbio_su_t2005, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2005, 0.78).
narrative_ontology:measurement(rbio_su_t2015, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2015, 0.82).
narrative_ontology:measurement(rbio_su_t2025, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2025, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__sovereignty_maximalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.1).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex__liberal_institutional_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex__hegemonic_extraction_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, r2p_operationalization).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, security_council_veto_power).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, icc_jurisdiction_complementarity).

% DUAL FORMULATION NOTE:
% This is one of three constraint stories decomposing the RBIO practice-norm complex kernel. The sovereignty_maximalist_reading treats the kernel's coordination function (conflict prevention via sovereignty) as its sole legitimate core and its extraction function (impunity for repression) as a necessary cost. The liberal_institutional_reading treats both coordination and human rights protection as co-equal kernel commitments. The hegemonic_extraction_reading treats the kernel as a frozen hegemonic project where extraction is structural. All three stories share the same kernel_id but instantiate different constraints with different ε, different beneficiary/victim structures, and different claimed types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
