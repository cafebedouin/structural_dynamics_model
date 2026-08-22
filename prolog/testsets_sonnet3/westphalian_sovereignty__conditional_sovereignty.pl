% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__conditional_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   This story instantiates the conditional sovereignty reading of the
 *   Westphalian sovereignty kernel: the claim that sovereignty is not
 *   absolute but entails a responsibility to protect populations from mass
 *   atrocity, and that systematic human rights violations trigger legitimate
 *   external intervention. This is a distinct constraint from the absolute
 *   sovereignty reading (which forecloses external interference
 *   categorically) and the graduated sovereignty reading (which ties
 *   sovereign authority to a capacity/legitimacy spectrum rather than a
 *   violation threshold). The ε authored here (0.38) is for THIS reading's
 *   own account of the standing arrangement — the Responsibility to Protect
 *   doctrine and its selective invocation record — not for an idealized
 *   universally-applied version of the doctrine.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__conditional_sovereignty, 0.38).
domain_priors:suppression_score(westphalian_sovereignty__conditional_sovereignty, 0.42).
domain_priors:theater_ratio(westphalian_sovereignty__conditional_sovereignty, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, extractiveness, 0.38).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__conditional_sovereignty, snare).
narrative_ontology:human_readable(westphalian_sovereignty__conditional_sovereignty, "Responsibility to Protect / Conditional Sovereignty Doctrine").
narrative_ontology:topic_domain(westphalian_sovereignty__conditional_sovereignty, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(westphalian_sovereignty__conditional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__conditional_sovereignty, '462bc23f-bb9e-487a-ae16-759914326a94').
narrative_ontology:cs_kernel_codification('462bc23f-bb9e-487a-ae16-759914326a94', distributed).
narrative_ontology:cs_authority_grounding('462bc23f-bb9e-487a-ae16-759914326a94', distributed).
narrative_ontology:cs_reading_relation('462bc23f-bb9e-487a-ae16-759914326a94', westphalian_sovereignty__absolute_sovereignty, forecloses).
narrative_ontology:cs_reading_relation('462bc23f-bb9e-487a-ae16-759914326a94', westphalian_sovereignty__graduated_sovereignty, coexists_with).
narrative_ontology:cs_axiom('462bc23f-bb9e-487a-ae16-759914326a94', foundational, sovereignty_is_conditioned_on_protective_responsibility).
narrative_ontology:cs_axiom_status(sovereignty_is_conditioned_on_protective_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('462bc23f-bb9e-487a-ae16-759914326a94', sovereignty_is_conditioned_on_protective_responsibility, deontological).
narrative_ontology:cs_axiom('462bc23f-bb9e-487a-ae16-759914326a94', secondary, systematic_violation_of_threshold_severity_dissolves_non_interference_claim).
narrative_ontology:cs_axiom_status(systematic_violation_of_threshold_severity_dissolves_non_interference_claim, holdable).
narrative_ontology:cs_axiom_grounding('462bc23f-bb9e-487a-ae16-759914326a94', systematic_violation_of_threshold_severity_dissolves_non_interference_claim, conventional).
narrative_ontology:cs_reference_frame('462bc23f-bb9e-487a-ae16-759914326a94', post_1990_atrocity_prevention_consensus).
narrative_ontology:cs_drift_state('462bc23f-bb9e-487a-ae16-759914326a94', post_libya_intervention_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('462bc23f-bb9e-487a-ae16-759914326a94', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, intervention_advocacy_states).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, un_security_council_permanent_members).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, human_rights_ngo_coalitions).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, at_risk_civilian_populations).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, targeted_sovereign_governments).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, postcolonial_states_wary_of_precedent).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, non_aligned_bloc_states).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__conditional_sovereignty, responsibility_to_protect_doctrine).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__conditional_sovereignty, individual_sovereign_immunity_is_not_absolute).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A government accused of systematic human rights violations faces the prospect of external military, diplomatic, or economic intervention justified by the doctrine. It cannot appeal to sovereign non-interference once the threshold is invoked by powerful states or coalitions; its only options are compliance under external pressure, negotiated concession, or armed resistance to the intervention itself.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, targeted_sovereign_governments, payer,
    moderate, immediate, trapped, national).

% Powerful states and coalitions invoke the doctrine to authorize sanctions, military action, or diplomatic isolation against target states. They control the interpretive machinery — what counts as 'systematic violation' and when the threshold is crossed — and are rarely themselves subject to the same standard being applied against them by comparably powerful actors.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, intervention_advocacy_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Hold veto power over which invocations of the doctrine proceed to formal international sanction. They can selectively authorize intervention against rivals and block it against allies or themselves, converting a universal principle into a discretionary instrument they administer.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, un_security_council_permanent_members, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__conditional_sovereignty, un_security_council_permanent_members, agenda_setter).

% Document violations, lobby for invocation of the doctrine, and gain institutional standing and resources when intervention frameworks are activated. Their advocacy work benefits from the doctrine's existence regardless of whether any given intervention succeeds.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, human_rights_ngo_coalitions, beneficiary,
    organized, generational, mobile, global).

% Populations suffering the violations that trigger the doctrine stand to benefit from protection if intervention occurs and succeeds — but they bear the risks of intervention itself (military action on their territory, destabilization, retaliation by their own government) and have no control over whether, how, or when the doctrine is invoked on their behalf.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, at_risk_civilian_populations, beneficiary,
    powerless, immediate, trapped, local).

% States with histories of colonial intervention view the doctrine as reviving external license to interfere in domestic governance under humanitarian pretext. They cannot easily reject the doctrine outright without appearing to endorse rights violations, but adopting it fully exposes them to selective future targeting.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, postcolonial_states_wary_of_precedent, payer,
    moderate, generational, constrained, national).

% Smaller and non-aligned states have limited voice in defining the threshold criteria or in Security Council deliberations where invocation is authorized. They would prefer clearer, symmetric rules applied to all states equally but lack the institutional power to secure that outcome.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, non_aligned_bloc_states, excluded,
    moderate, generational, constrained, global).

% Study the doctrine's application record, comparing invoked cases to non-invoked cases of comparable severity, and assess whether the pattern reflects principled application or power-selective enforcement.
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
% COORDINATION_FUNCTION: Provides a shared international norm for responding to mass atrocities that would otherwise be addressed only by unilateral action or left entirely unaddressed — coordinating collective response to genocide, ethnic cleansing, war crimes, and crimes against humanity.
% TRANSFER_FUNCTION: Moves authority to define and act on 'systematic violation' from the targeted state to external actors (primarily powerful states and Security Council members), and moves risk of intervention consequences onto the civilian populations the doctrine claims to protect.
% ABSENT_VOICES: Non-aligned and smaller states have limited say in setting invocation criteria; the populations the doctrine is invoked to protect are rarely consulted on whether intervention is wanted, on what terms, or at what risk to themselves.
% DISAPPEARANCE_RATIONALE: Advocates argue the doctrine's disappearance would remove the only formal international check on sovereign impunity for atrocities, leaving affected populations with no normative claim to external help. Critics argue its disappearance would simply return international relations to the pre-existing pattern of selective, power-justified intervention conducted under different pretexts — the underlying power asymmetry, not the doctrine, drives outcomes.
% FOUNDING_PROBLEM: The international system had no principled basis for external response to mass atrocities committed within a state's own borders; Rwanda and Srebrenica exposed the cost of treating sovereignty as an absolute shield against intervention even during genocide.
% FOUNDING_PROBLEM_CORROBORATION: UN-affiliated bodies and human rights scholars attest the founding problem (unchecked mass atrocity under sovereign shield) remains live and cite continuing cases where intervention was not authorized despite documented violations. Independent international relations scholars outside the advocacy coalitions note the doctrine's actual invocation pattern correlates more strongly with the political alignment of the target state than with severity of violations, suggesting the stated founding problem is partially superseded by a selective-enforcement function.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__conditional_sovereignty, contested).
narrative_ontology:founding_problem_status(westphalian_sovereignty__conditional_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__conditional_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extraction (0.38) reflects the doctrine's function as a constraint on targeted states' autonomy once invoked, moderated by the fact that invocation itself remains discretionary and inconsistently applied — the extraction is real but bounded to specific triggering episodes rather than continuous. Suppression (0.42) reflects the coercive machinery (sanctions regimes, Security Council authorization, potential military intervention) available once the threshold is asserted, but tempered by the frequent absence of enforcement even where violations are severe. Theater ratio (0.30) captures the gap between doctrinal invocation as rhetoric (used to justify or condemn without triggering actual intervention) and cases where the doctrine translates into concrete action. Accessibility collapse is moderate (0.35): non-invocation remains a live alternative in practice, which is precisely what critics point to as evidence of selective application rather than principled universal enforcement. Resistance is high (0.60) because targeted and postcolonial states actively contest the doctrine's legitimacy and invocation pattern.
 *
 * DIRECTIONALITY LOGIC:
 *   Powerful intervention-advocacy states and Security Council permanent members are beneficiaries: they administer the interpretive threshold, selectively invoke it against rivals, and are structurally insulated from having it invoked against themselves (near-arbitrage exit). Targeted sovereign governments are full targets: once the doctrine is invoked against them, they have no comparable counter-invocation available and are structurally trapped. At-risk civilian populations occupy an ambiguous position — nominal beneficiaries of the doctrine's protective intent, but bear intervention risk without control over its exercise, which is why their exit option is authored as trapped despite their beneficiary role. This divergence between nominal beneficiary status and actual exposure is central to the story.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Rwanda, Srebrenica — no principled basis for intervention against mass atrocity) remains genuinely live in the sense that atrocities continue to occur without intervention. But the doctrine's actual operation has drifted toward a selectively-invoked instrument correlated with target-state political alignment rather than violation severity — this is the substituted function the temporal measurements trace (rising theater_ratio and suppression_requirement without a corresponding rise in consistent application). Classifying this as snare rather than rope prevents mislabeling a selectively-applied coercive instrument as pure humanitarian coordination; classifying it as anything other than pure extraction is also wrong, because a genuine (if imperfect) coordination function — collective response capacity against atrocity — does exist and some populations have benefited from actual interventions taken under the doctrine's authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    invocation_selectivity_ambiguity,
    'Is the conditional sovereignty doctrine applied according to a principled, violation-severity-based threshold, or is invocation actually a function of the target state''s political alignment with Security Council permanent members?',
    'Comparative case analysis across all documented instances of systematic human rights violations meeting the R2P threshold criteria, cross-referenced against whether intervention was authorized, blocked, or never formally considered, controlling for the target state''s alignment with P5 members.',
    'If invocation correlates strongly with alignment rather than severity, the doctrine functions substantially as a snare disguised as universal humanitarian coordination; if invocation correlates with severity independent of alignment, the coordination function is closer to genuine and ε should be revised downward toward the rope/scaffold boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(invocation_selectivity_ambiguity, empirical, 'Whether invocation is severity-driven or alignment-driven.').

omega_variable(
    reading_boundary_conditional_vs_graduated,
    'Where exactly does the conditional sovereignty reading''s threshold (''systematic violations trigger intervention'') diverge structurally from the graduated sovereignty reading''s spectrum (''sovereignty scales with governance capacity/legitimacy'')? In practice, weak-capacity states are targeted more often, which could mean the two readings converge in application even though they diverge in stated justification.',
    'Track whether actual intervention decisions are better predicted by a discrete violation-threshold model (conditional_sovereignty) or a continuous capacity/legitimacy-scoring model (graduated_sovereignty) using historical intervention/non-intervention case sets.',
    'If the graduated model predicts outcomes better, this reading''s own doctrinal self-description (threshold-triggered) may be a legitimating narrative for what is functionally a graduated, capacity-based practice — meaning conditional_sovereignty and graduated_sovereignty are less distinct in operation than in stated axioms, though they remain separately authored constraints per the ε-invariance principle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_conditional_vs_graduated, conceptual, 'Whether the conditional and graduated readings are structurally distinct in application or only in stated justification.').

omega_variable(
    protective_benefit_vs_intervention_risk_tradeoff,
    'For at-risk civilian populations, does the doctrine''s existence net-benefit them (increased probability of eventual protective intervention) or net-harm them (increased risk of destabilizing intervention without their consent, plus retaliation risk from their own government once the doctrine is invoked)?',
    'Population-level outcome studies comparing civilian welfare trajectories in R2P-invoked interventions versus comparable non-invoked atrocity cases versus comparable interventions conducted under different legal justifications.',
    'Resolves whether at_risk_civilian_populations'' beneficiary role is doing real work or is a nominal classification masking a structurally uncertain or negative directionality — bears directly on whether this reading''s stated coordination function is real for its intended beneficiaries or primarily serves the intervention-advocacy states and NGO coalitions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(protective_benefit_vs_intervention_risk_tradeoff, empirical, 'Whether the doctrine nets a benefit or a harm for the populations it is invoked to protect.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__conditional_sovereignty, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 0, 0.15).
narrative_ontology:measurement(west_tr_t5, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 5, 0.18).
narrative_ontology:measurement(west_tr_t10, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 10, 0.22).
narrative_ontology:measurement(west_tr_t15, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 15, 0.25).
narrative_ontology:measurement(west_tr_t20, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 20, 0.27).
narrative_ontology:measurement(west_tr_t25, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 25, 0.29).
narrative_ontology:measurement(west_tr_t30, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(west_be_t5, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 5, 0.27).
narrative_ontology:measurement(west_be_t10, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 10, 0.31).
narrative_ontology:measurement(west_be_t15, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 15, 0.34).
narrative_ontology:measurement(west_be_t20, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(west_be_t25, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 25, 0.37).
narrative_ontology:measurement(west_be_t30, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 30, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(west_su_t5, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 5, 0.33).
narrative_ontology:measurement(west_su_t10, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 10, 0.36).
narrative_ontology:measurement(west_su_t15, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 15, 0.38).
narrative_ontology:measurement(west_su_t20, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(west_su_t25, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 25, 0.41).
narrative_ontology:measurement(west_su_t30, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 30, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__conditional_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty__absolute_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty__graduated_sovereignty).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the westphalian_sovereignty kernel. absolute_sovereignty holds unconditional non-interference (this reading's core premise directly forecloses that position within a single legal framework — a state cannot simultaneously be an unconditional and a conditional duty-bearer). graduated_sovereignty ties sovereign authority to a capacity/legitimacy spectrum rather than a discrete violation threshold; the two readings coexist as live positions in contemporary international law scholarship and practice, and an omega above documents where their predictions may converge despite differing stated justifications. Each reading carries its own independently authored ε, beneficiary/victim structure, and stakeholder surface per the ε-invariance principle — they are not measurements of one constraint from different angles but three structurally distinct constraints sharing a kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
