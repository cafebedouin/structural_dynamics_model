% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__performative_tool_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doomsday_clock_metric__performative_tool_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: doomsday_clock_metric__performative_tool_reading
 *   human_readable: Doomsday Clock as Performative Policy Mobilization Tool
 *   domain: science_communication/normative_epistemology/risk_governance
 *
 * SUMMARY:
 *   The Doomsday Clock, maintained by the Bulletin of the Atomic Scientists
 *   since 1947, is publicly presented as an expert synthesis of existential
 *   risk levels — a 'report card' on humanity's proximity to catastrophe.
 *   This reading (performative_tool_reading) asserts that the clock's actual
 *   structural function has shifted: the annual setting is now strategically
 *   chosen to maximize policy impact, media attention, and mobilization of
 *   collective action, with methodological rigor subordinated to symbolic
 *   utility. The constraint is the practice of strategic clock-setting itself
 *   — an arrangement that coordinates policy activism (beneficiary) by
 *   extracting epistemic credibility from science communication institutions
 *   and public trust (victims). The claimed_type is tangled_rope because the
 *   constraint retains a genuine coordination function (synchronizing urgency
 *   across fragmented actors) while simultaneously operating as an extraction
 *   mechanism (credibility transferred to advocacy). The engine will compute
 *   per-seat classifications from the structural data; the divergence between
 *   the agenda-setter's experience (coordination they control) and the payer
 *   seats' experience (credibility extracted without consent) is the
 *   measurement this story contributes.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__performative_tool_reading, 0.72).
domain_priors:suppression_score(doomsday_clock_metric__performative_tool_reading, 0.58).
domain_priors:theater_ratio(doomsday_clock_metric__performative_tool_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__performative_tool_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__performative_tool_reading, "Doomsday Clock as Performative Policy Mobilization Tool").
narrative_ontology:topic_domain(doomsday_clock_metric__performative_tool_reading, "science_communication/normative_epistemology/risk_governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__performative_tool_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__performative_tool_reading, '51f7ecaf-2915-4e0e-9228-e2314048f12a').
narrative_ontology:cs_kernel_codification('51f7ecaf-2915-4e0e-9228-e2314048f12a', formalized).
narrative_ontology:cs_authority_grounding('51f7ecaf-2915-4e0e-9228-e2314048f12a', lineage).
narrative_ontology:cs_interpretation_layer_present('51f7ecaf-2915-4e0e-9228-e2314048f12a').
narrative_ontology:cs_reading_relation('51f7ecaf-2915-4e0e-9228-e2314048f12a', doomsday_clock_metric__objective_index_reading, forecloses).
narrative_ontology:cs_reading_relation('51f7ecaf-2915-4e0e-9228-e2314048f12a', doomsday_clock_metric__hybrid_legitimacy_reading, coexists_with).
narrative_ontology:cs_axiom('51f7ecaf-2915-4e0e-9228-e2314048f12a', foundational, policy_impact_justifies_epistemic_compromise).
narrative_ontology:cs_axiom_status(policy_impact_justifies_epistemic_compromise, holdable).
narrative_ontology:cs_axiom_grounding('51f7ecaf-2915-4e0e-9228-e2314048f12a', policy_impact_justifies_epistemic_compromise, instrumental).
narrative_ontology:cs_axiom('51f7ecaf-2915-4e0e-9228-e2314048f12a', foundational, symbolic_mobilization_trumps_measurement_fidelity_in_existential_risk).
narrative_ontology:cs_axiom_status(symbolic_mobilization_trumps_measurement_fidelity_in_existential_risk, holdable).
narrative_ontology:cs_axiom_grounding('51f7ecaf-2915-4e0e-9228-e2314048f12a', symbolic_mobilization_trumps_measurement_fidelity_in_existential_risk, instrumental).
narrative_ontology:cs_reference_frame('51f7ecaf-2915-4e0e-9228-e2314048f12a', founding_physicist_consensus_1947).
narrative_ontology:cs_drift_state('51f7ecaf-2915-4e0e-9228-e2314048f12a', contemporary_multi_threat_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('51f7ecaf-2915-4e0e-9228-e2314048f12a', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, policy_activists).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, bulletin_of_atomic_scientists_leadership).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, existential_risk_advocacy_organizations).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, epistemic_credibility_of_science_communication).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, public_trust_in_expert_institutions).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, participating_scientists_reputational_capital).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__performative_tool_reading, policy_mobilization_justifies_epistemic_compromise).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__performative_tool_reading, symbolic_action_trumps_measurement_fidelity_in_existential_risk).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the annual clock-setting ceremony and narrative framing. Their institutional identity and funding depend on the clock's policy relevance. They justify strategic settings as necessary to break political inertia on existential risks. Exit would mean abandoning the Bulletin's signature franchise and its convening power.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, bulletin_of_atomic_scientists_leadership, agenda_setter,
    institutional, generational, identity_locked, global).

% Use the clock setting as a lever in legislative testimony, media campaigns, and diplomatic pressure. The clock's apparent scientific authority gives their demands a veneer of objectivity. They can switch to other mobilization tools if the clock loses credibility, but currently it is a high-value asset.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, policy_activists, beneficiary,
    organized, biographical, mobile, global).

% Amplify the clock announcement in fundraising, recruitment, and policy briefs. The clock provides a media hook and a shared reference point across fragmented risk communities. They benefit from the urgency narrative regardless of the clock's methodological rigor.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, existential_risk_advocacy_organizations, beneficiary,
    organized, biographical, mobile, global).

% Lend their expert credibility to the clock-setting process, believing they are contributing to an objective assessment. Their participation is used to legitimize settings that may reflect political judgment more than their technical input. Dissent risks marginalization within the expert community; silence erodes their standing when the clock's credibility is later questioned.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, participating_scientists_reputational_capital, payer,
    moderate, biographical, constrained, global).

% Each strategic clock move that is later revealed as politically calibrated — or that fails to track measurable risk indicators — increments a hidden tax on all expert pronouncements. The public has no exit from relying on expert institutions for risk guidance; the clock's performative use degrades the very infrastructure they depend on.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, public_trust_in_expert_institutions, payer,
    powerless, generational, trapped, global).

% The clock is cited as a paradigm of science-informed policy. When its methodology is shown to be elastic to advocacy goals, the template spreads: other science communication efforts adopt strategic framing over transparency. The cost is diffuse but structural — a general erosion of the norm that scientific communication should be insulated from instrumental use.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, epistemic_credibility_of_science_communication, payer,
    moderate, civilizational, constrained, global).

% Develop alternative risk indices (e.g., Global Catastrophic Risk Index, Nuclear Threat Initiative scores) that prioritize methodological transparency over symbolic punch. They are excluded from the clock's media platform and policy access; their objections to the clock's opacity are framed as 'missing the point' of mobilization.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, rival_science_communicators, excluded,
    moderate, biographical, constrained, global).

% Study the clock as a case study in science communication ethics, science-policy interface, and performative metrics. They document the gap between the clock's claimed objectivity and its observed strategic calibration. Their analysis feeds back into the field but carries no direct leverage over the clock-setting process.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, meta_science_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, media-legible focal point that synchronizes disparate policy actors, NGOs, and publics around existential risk urgency, enabling collective action that would otherwise fragment across issue silos.
% TRANSFER_FUNCTION: Transfers epistemic credibility from participating scientists and the Bulletin's institutional reputation to policy mobilization campaigns; the clock's scientific veneer is the currency that purchases political attention and legislative windows.
% ABSENT_VOICES: Scientists who have withdrawn from participation citing politicization; historians of science who document the clock's methodological drift; journalists who treat the clock as objective news rather than advocacy theater; publics in Global South nations whose existential risks (climate, famine) are backgrounded by the clock's great-power nuclear focus.
% DISAPPEARANCE_RATIONALE: If the clock vanished overnight, the policy activism ecosystem would lose its most recognizable urgency symbol and shared deadline. Advocacy coalitions would fragment across issue-specific metrics; media coverage of existential risk would lose its annual peg; the Bulletin would lose its primary convening asset and revenue driver. A replacement symbol would eventually emerge but the transition would disrupt coordinated pressure campaigns.
% FOUNDING_PROBLEM: In 1947, nuclear physicists needed a visceral, publicly intelligible symbol to convey that nuclear weapons had created a novel, irreversible existential threshold — not just another policy problem — and that the distance to catastrophe was a function of human choices, not fate.
% FOUNDING_PROBLEM_CORROBORATION: The Bulletin's founding documents and Einstein-Szilard correspondence corroborate the original intent. However, current Bulletin leadership asserts the founding problem persists unchanged (nuclear threat plus climate plus disruptive tech). Participating scientists from the 1990s-2000s cohorts (e.g., former Science and Security Board members) attest that the clock's methodology has drifted from risk assessment to advocacy signaling, and that the founding problem of 'conveying expert consensus on measurable risk' is no longer the clock's actual function.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__performative_tool_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__performative_tool_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__performative_tool_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(doomsday_clock_metric__performative_tool_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__performative_tool_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_metric__performative_tool_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(doomsday_clock_metric__performative_tool_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(doomsday_clock_metric__performative_tool_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the clock's scientific authority — its core asset — is systematically deployed for advocacy ends that exceed the evidentiary warrant. The 2020 and 2023 settings (100 and 90 seconds to midnight) coincided with policy campaigns (New START renewal, climate legislation) rather than step-changes in measurable risk indicators. Theater ratio (0.68) is high because the elaborate expert consultation process, published methodology statements, and academic framing serve increasingly as ritual validation for predetermined settings. Suppression (0.58) is moderate: the constraint does not coerce participation, but it structurally suppresses alternative risk indices by monopolizing the 'single authoritative metric' media niche and by framing methodological critique as 'undermining urgency.' Accessibility collapse (0.45) is moderate: alternative framings (probabilistic risk assessment, scenario planning) exist but lack the clock's symbolic compression. Resistance (0.35) is low-moderate: scientific pushback occurs (e.g., 2020 open letter from Nobel laureates questioning methodology) but is absorbed into the clock's narrative as 'evidence of the threat's seriousness.'
 *
 * PERSPECTIVAL GAP:
 *   From the Bulletin leadership seat, the clock is a necessary coordination tool in a world where expert consensus fails to translate to policy — the strategic elasticity is a feature, not a bug. From the participating scientist seat, the same elasticity is a betrayal of the expert-consensus premise they signed onto. From the public trust seat, the clock's advocacy use is a hidden tax on all future expert pronouncements. The engine computes these divergent effective extractions from the single base ε and the structural directionality data; this story authors the structural data, not the reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   The Bulletin leadership (agenda_setter) sits at d ≈ 0.15: they control the constraint, capture its reputational and funding benefits, and face identity-locked exit (the clock IS the Bulletin's brand). Policy activists and advocacy orgs (beneficiaries) sit at d ≈ 0.25: they gain mobilization leverage with mobile exit (they can adopt other symbols). Participating scientists (payers) sit at d ≈ 0.75: they contribute expert credibility under the premise of objective assessment but the output serves advocacy; their exit is constrained by professional norms and the cost of public dissent. Public trust and epistemic credibility (payers) sit at d ≈ 0.85: they bear the diffuse, long-horizon cost of credibility erosion with no exit from dependence on expert institutions. Rival communicators (excluded) are structurally blocked from the clock's media platform; their exclusion maintains the clock's monopoly on the 'single number' niche. Meta-observers (analytical) sit at d = 0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (visceral symbol for novel nuclear threshold) was live in 1947. By 1991 the Cold War nuclear template had shifted; the clock added climate (2007) and disruptive tech (2020) to maintain relevance. The founding problem_status is 'contested' because the original problem (conveying expert consensus on measurable risk) is arguably dead — the clock no longer operates as a consensus metric — but the Bulletin asserts it persists in expanded form. The mandatrophy analysis shows the constraint has not resolved its mandatrophy: it has expanded its mandate (nuclear → climate → AI → bio) to justify continued operation, while its core methodology has atrophied into advocacy signaling. This is not a scaffold (no sunset clause, no transition plan) and not a piton (the agenda_setter actively maintains and benefits from it). It is a tangled_rope: coordination persists (mobilization works) but extraction has grown to dominate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Where exactly does the performative_tool_reading end and the hybrid_legitimacy_reading begin? Both acknowledge normative stakes; the difference is whether strategic manipulation is the PRIMARY driver (this reading) or an INEVITABLE entanglement (hybrid).',
    'Counterfactual test: if the Bulletin leadership were presented with a methodologically rigorous but policy-inconvenient risk assessment (e.g., nuclear risk decreasing while climate risk increases), which reading predicts their clock-setting behavior? Empirical analysis of past setting deliberations (board minutes, leaked drafts) could resolve.',
    'If the boundary collapses, this reading''s claimed_type (tangled_rope) may merge with hybrid_legitimacy_reading''s likely classification (also tangled_rope but with different extraction profile). The decomposition into separate constraint stories would be unjustified; a single story with a CS framing omega would suffice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether performative_tool and hybrid_legitimacy are structurally distinct readings or a single reading with internal tension.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.58) structural (media platform monopoly, institutional gatekeeping) or internalized (scientists self-censor because they believe ''the cause justifies the method'')?',
    'Post-exit trajectory analysis: track scientists who left the Science and Security Board. If their public criticism of the clock''s methodology persists after exit, suppression was structural. If they revert to endorsing the clock''s authority, suppression was partially internalized (identity fusion with the ''urgency'' narrative).',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint travels with the agent after exit. This would increase χ for the participating_scientists seat and could shift its computed type toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for participating scientists.').

omega_variable(
    coordination_extraction_separability,
    'Can the clock''s coordination function (mobilization focal point) be preserved while eliminating the extraction (strategic manipulation of the metric)? Or are they structurally inseparable — does the mobilization power REQUIRE the scientific veneer?',
    'Natural experiment: jurisdictions or issue-areas where alternative risk indices (e.g., NTI Nuclear Security Index, Climate Action Tracker) have gained policy traction without a performative single metric. If mobilization persists without symbolic compression, the functions are separable.',
    'If inseparable, the extraction is the price of coordination — the constraint is a genuine tangled_rope where the coordination function cannot exist without the epistemic compromise. If separable, the strategic manipulation is gratuitous extraction riding on a real coordination function, strengthening the snare characterization for the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the clock''s coordination and extraction components are structurally separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__performative_tool_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dcp_tr_t1947, doomsday_clock_metric__performative_tool_reading, theater_ratio, 1947, 0.1).
narrative_ontology:measurement(dcp_tr_t1960, doomsday_clock_metric__performative_tool_reading, theater_ratio, 1960, 0.18).
narrative_ontology:measurement(dcp_tr_t1984, doomsday_clock_metric__performative_tool_reading, theater_ratio, 1984, 0.35).
narrative_ontology:measurement(dcp_tr_t1991, doomsday_clock_metric__performative_tool_reading, theater_ratio, 1991, 0.25).
narrative_ontology:measurement(dcp_tr_t2007, doomsday_clock_metric__performative_tool_reading, theater_ratio, 2007, 0.48).
narrative_ontology:measurement(dcp_tr_t2015, doomsday_clock_metric__performative_tool_reading, theater_ratio, 2015, 0.58).
narrative_ontology:measurement(dcp_tr_t2020, doomsday_clock_metric__performative_tool_reading, theater_ratio, 2020, 0.64).
narrative_ontology:measurement(dcp_tr_t2024, doomsday_clock_metric__performative_tool_reading, theater_ratio, 2024, 0.68).

% Extraction over time
narrative_ontology:measurement(dcp_be_t1947, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 1947, 0.15).
narrative_ontology:measurement(dcp_be_t1960, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 1960, 0.22).
narrative_ontology:measurement(dcp_be_t1984, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 1984, 0.35).
narrative_ontology:measurement(dcp_be_t1991, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 1991, 0.28).
narrative_ontology:measurement(dcp_be_t2007, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 2007, 0.52).
narrative_ontology:measurement(dcp_be_t2015, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 2015, 0.61).
narrative_ontology:measurement(dcp_be_t2020, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement(dcp_be_t2024, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(dcp_su_t1947, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 1947, 0.05).
narrative_ontology:measurement(dcp_su_t1960, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 1960, 0.15).
narrative_ontology:measurement(dcp_su_t1984, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 1984, 0.35).
narrative_ontology:measurement(dcp_su_t1991, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 1991, 0.25).
narrative_ontology:measurement(dcp_su_t2007, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 2007, 0.45).
narrative_ontology:measurement(dcp_su_t2015, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 2015, 0.52).
narrative_ontology:measurement(dcp_su_t2020, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 2020, 0.55).
narrative_ontology:measurement(dcp_su_t2024, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__performative_tool_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(doomsday_clock_metric__performative_tool_reading, 0.1).
narrative_ontology:affects_constraint(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric__objective_index_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric__hybrid_legitimacy_reading).

% DUAL FORMULATION NOTE:
% This story decomposes the 'Doomsday Clock' label into three structurally distinct constraints per the ε-invariance principle. The performative_tool_reading (this story) has ε ≈ 0.72 and claimed_type tangled_rope. The objective_index_reading claims ε ≈ 0.15 (mountain/rope boundary). The hybrid_legitimacy_reading claims ε ≈ 0.45 (tangled_rope with different beneficiary/victim structure). All three share the kernel 'doomsday_clock_metric' and are linked via affects_constraints. The upstream reading (objective_index) influences the downstream readings by providing the scientific authority that the performative reading exploits.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(doomsday_clock_metric__performative_tool_reading, moderate, 0.75).
constraint_indexing:directionality_override(doomsday_clock_metric__performative_tool_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
