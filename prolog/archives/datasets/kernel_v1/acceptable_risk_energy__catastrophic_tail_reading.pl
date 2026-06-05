% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__catastrophic_tail_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__catastrophic_tail_reading, []).

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
 *   constraint_id: acceptable_risk_energy__catastrophic_tail_reading
 *   human_readable: Catastrophic Tail Risk Dominance in Energy Policy
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   This constraint instantiates the catastrophic-tail reading of the
 *   contested acceptable_risk_energy kernel. The reading holds that when
 *   potential outcomes include true catastrophes (reactor core failures, dam
 *   collapses, waste-storage failures with multi-generational consequences),
 *   the mathematical and ethical structure of risk assessment must
 *   categorically weight tail scenarios above expected-value aggregation.
 *   This reading forecloses the possibility that expected-value frameworks
 *   adequately capture the decision problem — once catastrophic endpoints are
 *   possible, expected-value logic becomes not just insufficient but
 *   potentially incoherent as a decision rule. The measurement trajectory
 *   shows rising extractiveness (0.45→0.68), rising theater (0.42→0.58), and
 *   rising suppression (0.60→0.72), indicating that the institutional
 *   machinery for maintaining expected-value dominance has intensified over
 *   the interval despite mounting evidence of its inadequacy. Post-Fukushima,
 *   post-Texas freeze, and post-Oroville Dam reviews have all documented that
 *   standard PRA systematically underweights tail scenarios, yet regulatory
 *   practice has doubled down on PRA-based frameworks rather than adopting
 *   alternatives.
 *
 * KEY AGENTS:
 *   - Hypothetical Catastrophe Populations: Powerless/trapped — bear tail risk with no input, no exit, no negotiation capacity; maximal snare experience
 *   - Energy Industry & Regulatory Apparatus: Institutional/arbitrage — beneficiaries of expected-value suppression; coordinating factor that sustains infrastructure investment and capital allocation
 *   - Local Risk-Aware Communities: Moderate/constrained — both coordinated by energy infrastructure and extracted from through concentrated tail risk; mixed tangled_rope experience
 *   - Renewable Energy Coalition: Organized/mobile — sees catastrophic-tail-weighting as justifying infrastructure transition; scaffold agents with clear exit pathway
 *   - PRA Orthodoxy Institution: Institutional/arbitrage — degraded methodology maintained through professional consensus and regulatory entrenchment; piton structure
 *   - Regulatory Safety Engineers: Moderate/identity_locked — structurally capable of adopting alternative frameworks but professionally bound to PRA career path; identity prevents exit even as framework fails
 *   - Analytical Observer: Analytical/analytical — risks naturalizing contingent methodology as mathematical law of decision theory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__catastrophic_tail_reading, 0.68).
domain_priors:suppression_score(acceptable_risk_energy__catastrophic_tail_reading, 0.72).
domain_priors:theater_ratio(acceptable_risk_energy__catastrophic_tail_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__catastrophic_tail_reading, snare).
narrative_ontology:human_readable(acceptable_risk_energy__catastrophic_tail_reading, "Catastrophic Tail Risk Dominance in Energy Policy").
narrative_ontology:topic_domain(acceptable_risk_energy__catastrophic_tail_reading, "risk_assessment/energy_policy/public_safety_governance").

domain_priors:requires_active_enforcement(acceptable_risk_energy__catastrophic_tail_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__catastrophic_tail_reading, '3067a1be-b32b-4c1b-8642-bc8f1ff06fc3').
narrative_ontology:cs_kernel_codification('3067a1be-b32b-4c1b-8642-bc8f1ff06fc3', implicit).
narrative_ontology:cs_authority_grounding('3067a1be-b32b-4c1b-8642-bc8f1ff06fc3', distributed).
narrative_ontology:cs_reading_relation('3067a1be-b32b-4c1b-8642-bc8f1ff06fc3', acceptable_risk_energy__expected_value_reading, forecloses).
narrative_ontology:cs_reading_relation('3067a1be-b32b-4c1b-8642-bc8f1ff06fc3', acceptable_risk_energy__comparative_harm_reading, influences).
narrative_ontology:cs_axiom('3067a1be-b32b-4c1b-8642-bc8f1ff06fc3', foundational, irreversible_outcomes_non_negotiable_weight).
narrative_ontology:cs_axiom_status(irreversible_outcomes_non_negotiable_weight, holdable).
narrative_ontology:cs_axiom_grounding('3067a1be-b32b-4c1b-8642-bc8f1ff06fc3', irreversible_outcomes_non_negotiable_weight, deontological).
narrative_ontology:cs_axiom('3067a1be-b32b-4c1b-8642-bc8f1ff06fc3', foundational, decision_incoherence_under_unbounded_catastrophe).
narrative_ontology:cs_axiom_status(decision_incoherence_under_unbounded_catastrophe, holdable).
narrative_ontology:cs_axiom_grounding('3067a1be-b32b-4c1b-8642-bc8f1ff06fc3', decision_incoherence_under_unbounded_catastrophe, empirically_contingent).
narrative_ontology:cs_reference_frame('3067a1be-b32b-4c1b-8642-bc8f1ff06fc3', precautionary_energy_allocation).
narrative_ontology:cs_drift_state('3067a1be-b32b-4c1b-8642-bc8f1ff06fc3', contemporary_energy_transition_stall, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3067a1be-b32b-4c1b-8642-bc8f1ff06fc3', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__catastrophic_tail_reading, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_reading, incumbent_energy_industry).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_reading, political_decision_makers_seeking_continuity).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_reading, hypothetical_catastrophe_populations).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_reading, non_local_risk_bearing_communities).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_reading, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HYPOTHETICAL CATASTROPHE POPULATIONS (SNARE) — Populations in potential disaster zones (seismic regions near reactors, downstream of dams, near waste repositories) bear tail-outcome risk with no meaningful exit option and zero input into decision-making. The constraint traps them in accepting infinitesimal but catastrophic downside solely through geographic proximity. Cannot organize, exit, or negotiate. Maximum extraction because the suppression is total and the risk asymmetry is absolute.
constraint_indexing:constraint_classification(acceptable_risk_energy__catastrophic_tail_reading, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ENERGY INDUSTRY & REGULATORY APPARATUS (ROPE) — Benefits from the catastrophic-tail-suppression mechanism that sustains energy infrastructure continuity. The constraint coordinates energy supply and capital investment by suppressing tail-risk discourse: expected-value framing keeps attention on aggregate benefits, not on low-probability catastrophic endpoints. Regulatory apparatus captures legitimacy from technocratic risk assessment. Net beneficiary with arbitrage options — can shift to alternatives if political pressure rises, but structure incentivizes maintaining status quo.
constraint_indexing:constraint_classification(acceptable_risk_energy__catastrophic_tail_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: LOCAL RISK-AWARE COMMUNITIES (TANGLED ROPE) — Communities adjacent to energy infrastructure experience both genuine coordination benefit (electricity, economic activity) and asymmetric extraction (concentrated tail risk). Constrained exit — relocation is costly; local economy is integrated with energy facility. Both coordinated and constrained by the same infrastructure. Theater_ratio moderate because community perception may diverge from official risk assessments, but enforcement (regulatory blessing, cost-benefit analyses excluding tail scenarios) suppresses alternative valuation.
constraint_indexing:constraint_classification(acceptable_risk_energy__catastrophic_tail_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: RENEWABLE ENERGY COALITION (SCAFFOLD) — Organized agents (solar/wind developers, climate movements, some institutional investors) see catastrophic-tail dominance as justifying transitional policy: rapid phase-out of tail-heavy energy sources, accelerated deployment of lower-tail alternatives. Sunset logic: as renewable capacity matures and storage improves, the coordination function provided by high-risk energy disappears, making the constraint obsolete. Low effective extraction because coalition has mobility and clear exit pathway (infrastructure replacement).
constraint_indexing:constraint_classification(acceptable_risk_energy__catastrophic_tail_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PROBABILISTIC RISK ASSESSMENT ORTHODOXY (PITON) — The institutionalized expected-value framework (used in regulatory safety analysis) persists through professional consensus despite structural failure: PRA methodology systematically underweights tail scenarios and cannot assign meaningful probabilities to black-swan events. The methodology is recognized as degraded (every post-Fukushima review acknowledges this) but remains the administrative standard because alternatives lack institutional legitimacy. Theater_ratio high: performative quantification (false precision) substitutes for genuine catastrophe preparation. Enforcement decays as practitioners increasingly distrust their own outputs.
constraint_indexing:constraint_classification(acceptable_risk_energy__catastrophic_tail_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: MATHEMATICAL NATURAL LAW VIEW (MOUNTAIN) — From an analytical/civilizational perspective, tail-outcome weighting emerges as an immutable feature of rational decision-making under deep uncertainty: when downside is unbounded or catastrophic, expected-value aggregation mathematically produces incoherent policy regardless of probability discount. The constraint appears as a law of decision theory, not a contingent institutional arrangement. Engine will flag this as false summit: the 'naturalness' of tail weighting masks the fact that regulatory frameworks actively suppress tail weighting through their institutional design (cost-benefit analysis, discounting, probability thresholds).
constraint_indexing:constraint_classification(acceptable_risk_energy__catastrophic_tail_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: REGULATORY ENGINEERS (IDENTITY-LOCKED, SNARE) — Professional safety engineers and regulators whose identities are constituted through decades of PRA training and career path dependence on probabilistic frameworks. Structurally mobile (could use alternative frameworks) but cognitively trapped: their professional identity, career legitimacy, and epistemic authority rest on the adequacy of PRA. Acknowledging tail-weighting primacy would dissolve their expert status. Experiences as snare because identity lock prevents exit even as the framework's inadequacy becomes undeniable. This perspective demonstrates the oracle gap: the analyst sees both the mathematical truth (tail dominance) and the identity barrier preventing professional acknowledgment.
constraint_indexing:constraint_classification(acceptable_risk_energy__catastrophic_tail_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__catastrophic_tail_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(acceptable_risk_energy__catastrophic_tail_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(acceptable_risk_energy__catastrophic_tail_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_energy__catastrophic_tail_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(acceptable_risk_energy__catastrophic_tail_reading, TR),
    TR >= 0.70.

:- end_tests(acceptable_risk_energy__catastrophic_tail_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint systematically directs resources, attention, and legitimacy toward incumbent energy sources by suppressing tail-risk discourse. Expected-value framing produces cost-benefit analyses that conclude high-tail-risk sources remain justified. The extraction is not coercive but structurally embedded: those who bear tail risk have no standing in official risk assessment, while those who benefit from continuity (capital owners, regulatory agencies) control methodology. Suppression (0.72): Very high. Multiple mechanisms suppress tail-risk weighting: (1) Probabilistic Risk Assessment is the institutionalized methodology — alternative frameworks lack regulatory legitimacy. (2) Black-swan events are defined as outside the domain of quantifiable risk — catastrophes are dismissed as 'residual unknowns' rather than decision-relevant. (3) Professional identity of safety engineers is tied to PRA adequacy — acknowledging tail-dominance requires career-path admission of decades of false confidence. (4) Cost-benefit analysis discounting systematically devalues future catastrophes (high discount rates reduce 200-year tail-event impacts to negligible present value). Theater ratio (0.58): Moderate-high. PRA produces precise numerical outputs (e.g., core-damage frequency of 10^-6 per reactor-year) that appear scientifically rigorous but encode massive subjective assumptions about human behavior, equipment failure, and unforeseeable interactions. The precision is performative — false specificity obscures true uncertainties. Post-disaster reviews consistently find that actual failure modes fell outside PRA's enumerated scenarios, yet the methodology's apparent quantitative legitimacy prevents institutional adoption of alternatives.
 *
 * PERSPECTIVAL GAP:
 *   The catastrophic-tail reading produces maximal perspectival divergence. Trapped populations see unbounded downside with no mitigation input (snare). Industry and regulators see coordination function and investment incentive (rope). Local communities experience both benefit and risk concentration (tangled rope). Renewable advocates see a solvable constraint with clear sunset (scaffold). PRA institutions see their own methodology as degraded but unreplaceable (piton). Engineers see a framework they no longer believe in but cannot professionally exit (identity_locked snare). The analytical observer risks treating tail-weighting as a law of decision theory rather than a structural feature of policy choices. This reading's strength is its explicit refusal of expected-value logic — it claims the gap between tail outcomes and aggregate expected value is not a measurement problem but a fundamental incommensurability. Expected-value frameworks cannot rationally weight possibilities that include permanent civilizational disruption.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from each agent's structural relationship to tail-risk suppression. Hypothetical catastrophe populations: full victims (d→1.0), trapped with no exit, no beneficiary status → maximum f(d). Industry and regulators: net beneficiaries (d→0.1), arbitrage-mobile, gain from status quo → low/negative χ. Local communities: mixed (d→0.55), constrained exit, share some benefits → moderate χ. Renewable coalition: constrained exit but mobile framework, low extraction → lower χ. PRA institutions: beneficiaries in immediate term but increasingly delegitimized (d→0.35) → moderate χ with rising theater compensation. Identity-locked engineers: structurally mobile but cognitively trapped (d→0.85) → high experienced χ because they cannot exercise mobility.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint at ε=0.68 exceeds the mandatrophy threshold (0.70) and requires resolution. The mandatrophy for catastrophic-tail reading is resolved by acknowledging that the constraint IS a snare from the perspective of tail-bearing populations, but the snare persists because expected-value frameworks institutionally deny that tail outcomes are decision-dominant. The resolution path is not to choose between snare and rope — both are structurally accurate from their respective positions — but to recognize that the choice between tail-weighting and expected-value aggregation IS the distributional choice that determines who bears catastrophic outcomes. Mandatrophy is 'resolved' when the analysis explicitly grounds the classification in the reading's normative axiom: catastrophic outcomes have non-negotiable moral weight regardless of probability assignment. The constraint is a snare not because tail weighting is mathematically optimal, but because those who would bear catastrophe are systematically excluded from the decision framework that permits expected-value logic to suppress tail considerations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bounded_vs_unbounded_catastrophe,
    'Can catastrophic outcomes (extinction, civilizational collapse, irreversible biosphere damage) be assigned numerical probabilities meaningful for expected-value calculation, or are they fundamentally non-quantifiable black swans?',
    'Attempt to construct coherent Bayesian posterior for tail scenarios; identify where probability assignment becomes epistemic theater rather than genuine quantification.',
    'If truly non-quantifiable: tail-weighting is not a preference but a rationality requirement (snare classification holds). If assignable: expected-value frameworks can include them, and the constraint becomes more of a coordination problem (rope) than extraction mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bounded_vs_unbounded_catastrophe, conceptual, 'Whether catastrophic outcomes can be meaningfully quantified for expected-value analysis').

omega_variable(
    regulatory_capture_via_methodology,
    'Is the suppression of tail-risk discourse a deliberate industry capture mechanism or an inevitable consequence of applying expected-value mathematics to safety?',
    'Historical analysis of regulatory capture: Did industry actors explicitly lobby for adoption of PRA-based frameworks, or did frameworks emerge from independent technical development? Cross-jurisdictional comparison: Do regulatory bodies without industry pressure also suppress tail weighting?',
    'If deliberate capture: snare classification affirmed (engineered extraction). If mathematical inevitability: constraint may be mountain or rope rather than snare (structural feature rather than extraction mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_via_methodology, empirical, 'Whether tail-risk suppression is intentional regulatory capture or mathematical inevitability').

omega_variable(
    alternative_decision_frameworks_viability,
    'Can viable energy policy be constructed using tail-weighting decision rules (precautionary principle, maximin, robust satisficing) that don''t rely on expected-value aggregation?',
    'Policy simulation: compare energy system stability, investment incentives, and consumer welfare under tail-weighting vs expected-value frameworks. Do tail-focused frameworks produce inferior outcomes on dimensions other than catastrophe avoidance?',
    'If viable: scaffold perspective confirmed; tail-weighting is achievable without fundamental system collapse. If fraught: tail-weighting reveals actual incompleteness in decision theory (the reading''s foundational axiom is true, but its implementation creates new extraction mechanisms).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_decision_frameworks_viability, empirical, 'Whether tail-weighting decision rules produce viable energy policy').

omega_variable(
    kernel_reading_contest_locus,
    'Which sibling reading (expected_value_reading, comparative_harm_reading) is the catastrophic_tail_reading in direct logical conflict with? Where exactly does the mathematical or normative disagreement sit?',
    'Formal analysis of the three readings'' foundational axioms. Identify which axioms are contradictory (forecloses relationship), which are orthogonal (coexist_with), and which create downstream pressure (influences).',
    'This is meta-analysis of the kernel itself. Resolution reveals whether the contest is mathematical (one framework logically rules out another), empirical (different probability assignments), or normative (different value orderings on uncertain outcomes). Affects interpretation of reading_relations in cs_structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_locus, conceptual, 'Logical structure of disagreement between catastrophic_tail_reading and sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__catastrophic_tail_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catail_theater_t0, acceptable_risk_energy__catastrophic_tail_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(catail_theater_t5, acceptable_risk_energy__catastrophic_tail_reading, theater_ratio, 5, 0.5).
narrative_ontology:measurement(catail_theater_t10, acceptable_risk_energy__catastrophic_tail_reading, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(catail_extractiveness_t0, acceptable_risk_energy__catastrophic_tail_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(catail_extractiveness_t5, acceptable_risk_energy__catastrophic_tail_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(catail_extractiveness_t10, acceptable_risk_energy__catastrophic_tail_reading, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(catail_suppression_t0, acceptable_risk_energy__catastrophic_tail_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(catail_suppression_t5, acceptable_risk_energy__catastrophic_tail_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(catail_suppression_t10, acceptable_risk_energy__catastrophic_tail_reading, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__catastrophic_tail_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(acceptable_risk_energy__catastrophic_tail_reading, 0.25).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_reading, acceptable_risk_energy__expected_value_reading).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_reading, acceptable_risk_energy__comparative_harm_reading).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_reading, nuclear_waste_perpetual_containment).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_reading, systemic_risk_interconnection).

% DUAL FORMULATION NOTE:
% The 'acceptable risk in energy' problem is not one constraint but three: expected_value_reading (ε≈0.35, Rope from beneficiary perspective), comparative_harm_reading (ε≈0.52, Tangled Rope), and catastrophic_tail_reading (ε≈0.68, Snare). Each reading treats the same kernel (energy-risk-justification) through incompatible decision frameworks. This story instantiates only the tail-reading; the other two are separate constraint stories. They are linked through network.affects_constraints to show that the choice of which reading governs policy determines the extractiveness value and victim distribution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_energy__catastrophic_tail_reading, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
