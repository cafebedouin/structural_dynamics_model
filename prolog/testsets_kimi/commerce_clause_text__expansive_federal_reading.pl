% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__expansive_federal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__expansive_federal_reading, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: commerce_clause_text__expansive_federal_reading
 *   human_readable: Expansive Federal Commerce Power Reading
 *   domain: constitutional/law/federalism
 *
 * SUMMARY:
 *   The Commerce Clause of the U.S. Constitution has been interpreted through
 *   multiple readings. This constraint story instantiates the expansive
 *   federal reading: the claim that interstate commerce encompasses all
 *   economic activity with substantial aggregate effects on national markets.
 *   This reading emerged from the New Deal era and authorizes broad federal
 *   regulatory power over intrastate economic conduct. It is one reading of
 *   the commerce_clause_text kernel, alongside the originalist narrow reading
 *   and the substantial effects limited reading. The constraint coordinates a
 *   national market but asymmetrically extracts regulatory autonomy from
 *   state governments.
 *
 * KEY AGENTS:
 *   - federal_judiciary: agenda_setter (institutional/analytical) â maintains the doctrinal tests (substantial effects, rational basis) that validate federal statutes
 *   - federal_administrative_state: primary beneficiary (institutional/constrained) â captures jurisdiction to regulate local economic activity
 *   - state_governments: primary payer/victim (institutional/trapped) â loses autonomy to set local policy and faces federal preemption
 *   - national_regulatory_advocates: secondary beneficiary (organized/mobile) â favors uniform national standards over state variation
 *   - interstate_commerce_participants: payer (powerful/constrained) â bears the compliance costs of federal displacement of state regulatory frameworks
 *   - constitutional_originalists: excluded voice (organized/mobile) â argues for categorical limits on federal power; structurally marginalized in dominant framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__expansive_federal_reading, 0.72).
domain_priors:suppression_score(commerce_clause_text__expansive_federal_reading, 0.75).
domain_priors:theater_ratio(commerce_clause_text__expansive_federal_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__expansive_federal_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__expansive_federal_reading, "Expansive Federal Commerce Power Reading").
narrative_ontology:topic_domain(commerce_clause_text__expansive_federal_reading, "constitutional/law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_text__expansive_federal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__expansive_federal_reading, 'b0c979b9-8360-46ee-934b-697113b60bc6').
narrative_ontology:cs_kernel_codification('b0c979b9-8360-46ee-934b-697113b60bc6', fixed_text).
narrative_ontology:cs_authority_grounding('b0c979b9-8360-46ee-934b-697113b60bc6', lineage).
narrative_ontology:cs_interpretation_layer_present('b0c979b9-8360-46ee-934b-697113b60bc6').
narrative_ontology:cs_reading_relation('b0c979b9-8360-46ee-934b-697113b60bc6', commerce_clause_text__originalist_narrow_reading, coexists_with).
narrative_ontology:cs_reading_relation('b0c979b9-8360-46ee-934b-697113b60bc6', commerce_clause_text__substantial_effects_limited_reading, influences).
narrative_ontology:cs_axiom('b0c979b9-8360-46ee-934b-697113b60bc6', foundational, aggregate_effects_suffice_for_commerce_jurisdiction).
narrative_ontology:cs_axiom_status(aggregate_effects_suffice_for_commerce_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('b0c979b9-8360-46ee-934b-697113b60bc6', aggregate_effects_suffice_for_commerce_jurisdiction, conventional).
narrative_ontology:cs_axiom('b0c979b9-8360-46ee-934b-697113b60bc6', secondary, federal_authority_over_local_economic_activity).
narrative_ontology:cs_axiom_status(federal_authority_over_local_economic_activity, holdable).
narrative_ontology:cs_axiom_grounding('b0c979b9-8360-46ee-934b-697113b60bc6', federal_authority_over_local_economic_activity, conventional).
narrative_ontology:cs_reference_frame('b0c979b9-8360-46ee-934b-697113b60bc6', new_deal_regulatory_state).
narrative_ontology:cs_drift_state('b0c979b9-8360-46ee-934b-697113b60bc6', post_lopez_rehnquist_federalism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b0c979b9-8360-46ee-934b-697113b60bc6', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__expansive_federal_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, federal_administrative_state).
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, national_regulatory_advocates).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, state_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, interstate_commerce_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Commerce Clause to authorize federal regulation of intrastate economic activity; maintains and updates doctrinal tests such as substantial effects and rational basis review that determine whether federal statutes survive constitutional challenge.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Federal agencies rely on expansive commerce power to regulate local economic conduct in labor, environment, health care, and consumer protection; their regulatory jurisdiction and budget scale with the breadth of the reading.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, federal_administrative_state, beneficiary,
    institutional, generational, constrained, national).

% Lose autonomy to regulate local economic and social matters; face federal preemption and must either comply with federal mandates or litigate against them at severe fiscal and political cost, with no exit from the constitutional framework.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, state_governments, payer,
    institutional, generational, trapped, national).

% Civil society and policy advocates who favor uniform national standards; benefit from federal regulatory capacity to override state resistance on labor, environmental, and civil rights policy.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, national_regulatory_advocates, beneficiary,
    organized, biographical, mobile, national).

% Businesses operating across state lines bear compliance costs of federal regulation that displaces state regulatory frameworks; they cannot opt out of the national market without existential commercial harm.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, interstate_commerce_participants, payer,
    powerful, biographical, constrained, national).

% Legal scholars and jurists who argue for categorical limits on federal commerce power; their interpretive framework is structurally marginalized in the dominant post-New Deal doctrinal architecture but remains active in legal academia and some judicial dissents.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, constitutional_originalists, excluded,
    organized, civilizational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_text__expansive_federal_reading, federal_administrative_state).
narrative_ontology:fixing_cost_class(commerce_clause_text__expansive_federal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables a single national market by preventing state-level regulatory fragmentation; allows federal coordination of labor, environmental, and economic standards across jurisdictions.
% TRANSFER_FUNCTION: Transfers regulatory authority from state governments to federal agencies and courts; shifts compliance obligations from state-regulated to federally-regulated frameworks.
% ABSENT_VOICES: Constitutional originalists and state sovereignty advocates who argue for categorical limits on federal commerce power are present in legal academia and some statehouses but structurally excluded from the dominant post-New Deal interpretive framework.
% DISAPPEARANCE_RATIONALE: If this reading vanished, major federal regulatory statutes would face immediate constitutional challenge, states would regain broad autonomy over local economic activity, and the national administrative state's jurisdiction would contract dramatically.
% FOUNDING_PROBLEM: State-level protectionism and incompatible regulatory regimes threatened to fragment the national economy during industrialization and the early twentieth century.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians corroborate pre-New Deal state trade barriers. Constitutional originalists and state attorneys general outside the beneficiary set attest that the founding problem did not justify the expansive remedy and that federal overreach now exceeds the original problem.
narrative_ontology:disappearance_verdict(commerce_clause_text__expansive_federal_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__expansive_federal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__expansive_federal_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(commerce_clause_text__expansive_federal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__expansive_federal_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__expansive_federal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_text__expansive_federal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_text__expansive_federal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the reading transfers vast regulatory authority from states to federal actors, extracting state autonomy as a structural cost. Suppression (0.75) is high because the constraint persists through federal preemption and judicial review that actively suppresses state-level alternatives; states cannot opt out of the constitutional framework. Theater ratio (0.40) is moderate: the doctrine is functionally powerful but carries performative elements where courts ritually invoke 'substantial effects' while rarely striking federal statutes. Accessibility collapse (0.48) is moderate because the originalist alternative remains intellectually accessible, though institutionally disfavored. Resistance (0.55) reflects ongoing state litigation and federalism revival movements. The temporal series show extraction rising from the New Deal through the 1990s, with a slight moderation post-Lopez and a plateau thereafter.
 *
 * PERSPECTIVAL GAP:
 *   The federal administrative state and regulatory advocates experience this constraint as necessary coordination preventing a race to the bottom; state governments experience it as extraction of their constitutional autonomy. The engine will compute divergent per-seat classifications from this structural asymmetry: the beneficiary seats derive low directionality and damped effective extraction, while the trapped state payer seats derive high directionality and amplified extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal agencies are structural beneficiaries: the constraint subsidizes their jurisdiction (low d). State governments are structural targets: the constraint extracts their regulatory autonomy, and they are trapped within the federal system (high d). The federal judiciary sits as agenda-setter with analytical exit, deriving near-neutral d. Interstate businesses are constrained payers: they bear compliance costs but lack the political power to reshape the constitutional framework. Constitutional originalists are excluded from the conversation, their exit options mobile in the sense of academic discourse but ineffective in the doctrinal apparatus.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling by acknowledging the genuine coordination function: a national market genuinely requires some mechanism to prevent balkanization. However, the expansive reading goes beyond minimal coordination by swallowing purely local economic activity. That asymmetry â coordinating the national market while extracting state autonomy â makes it tangled rope rather than pure rope. It is not a snare because the coordination story is not mere cover: the constraint does solve a real collective-action problem in market unification, even as it overreaches. It is not a piton because the beneficiaries actively maintain and benefit from it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    federalism_zero_sum_assumption,
    'Is the expansion of federal commerce authority strictly zero-sum against state regulatory autonomy, or do states gain capacity through federal coordination?',
    'Comparative analysis of state regulatory budgets and policy outputs under expansive versus narrow commerce readings.',
    'If non-zero-sum, the authored extractiveness overstates the victim structure; if zero-sum, state governments are pure targets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federalism_zero_sum_assumption, empirical, 'Whether federal expansion purely extracts state autonomy.').

omega_variable(
    originalist_viability,
    'Can the originalist narrow reading still function as a live constitutional framework for a modern national economy, or has it been rendered structurally nonviable by economic transformation?',
    'Jurisdictional modeling and comparative constitutional analysis of federal systems with narrower commerce analogues.',
    'If nonviable, the expansive reading''s persistence reflects functional necessity; if viable, its dominance is institutionally chosen extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(originalist_viability, conceptual, 'Structural viability of the originalist reading in a modern economy.').

omega_variable(
    kernel_reading_contest,
    'This constraint is the expansive federal reading of the commerce clause kernel; how would classification change if the originalist narrow reading were adopted instead?',
    'Comparison with the sibling constraint story for originalist_narrow_reading.',
    'Would likely shift classification toward rope or mountain depending on empirical premises about cross-border trade.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer-frame uncertainty from kernel decomposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__expansive_federal_reading, 0, 87).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, commerce_clause_text__expansive_federal_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(comm_tr_t15, commerce_clause_text__expansive_federal_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(comm_tr_t30, commerce_clause_text__expansive_federal_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(comm_tr_t45, commerce_clause_text__expansive_federal_reading, theater_ratio, 45, 0.32).
narrative_ontology:measurement(comm_tr_t60, commerce_clause_text__expansive_federal_reading, theater_ratio, 60, 0.36).
narrative_ontology:measurement(comm_tr_t87, commerce_clause_text__expansive_federal_reading, theater_ratio, 87, 0.4).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, commerce_clause_text__expansive_federal_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(comm_be_t15, commerce_clause_text__expansive_federal_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(comm_be_t30, commerce_clause_text__expansive_federal_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(comm_be_t45, commerce_clause_text__expansive_federal_reading, base_extractiveness, 45, 0.72).
narrative_ontology:measurement(comm_be_t60, commerce_clause_text__expansive_federal_reading, base_extractiveness, 60, 0.74).
narrative_ontology:measurement(comm_be_t87, commerce_clause_text__expansive_federal_reading, base_extractiveness, 87, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, commerce_clause_text__expansive_federal_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(comm_su_t15, commerce_clause_text__expansive_federal_reading, suppression_requirement, 15, 0.64).
narrative_ontology:measurement(comm_su_t30, commerce_clause_text__expansive_federal_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(comm_su_t45, commerce_clause_text__expansive_federal_reading, suppression_requirement, 45, 0.72).
narrative_ontology:measurement(comm_su_t60, commerce_clause_text__expansive_federal_reading, suppression_requirement, 60, 0.74).
narrative_ontology:measurement(comm_su_t87, commerce_clause_text__expansive_federal_reading, suppression_requirement, 87, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__expansive_federal_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
