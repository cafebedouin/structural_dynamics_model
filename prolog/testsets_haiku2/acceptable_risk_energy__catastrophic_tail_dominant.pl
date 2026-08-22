% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__catastrophic_tail_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__catastrophic_tail_dominant, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: acceptable_risk_energy__catastrophic_tail_dominant
 *   human_readable: Catastrophic Tail-Risk Dominance in Energy Policy
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   Energy policy faces a contested choice: should acceptable risk be defined
 *   by minimizing aggregate expected harm (empirical, weighted by probability
 *   and magnitude) or by preventing catastrophic tail outcomes regardless of
 *   their probability? The catastrophic-tail-dominant reading answers: tail
 *   outcomes are intolerable and warrant excluding entire pathways (nuclear)
 *   even if their expected mortality per unit energy is lower than
 *   alternatives (fossil fuels). This reading benefits renewables by treating
 *   them as inherently safer and locks fossil fuel communities into extended
 *   reliance on coal because nuclear is forbidden and renewables remain
 *   unreliable. The constraint suppresses the expected-value reading through
 *   narrative control (framing tail-outcome aversion as moral clarity rather
 *   than value choice) and regulatory exclusion (nuclear prohibition
 *   regardless of empirical comparison). This is ONE reading of the contested
 *   kernel 'acceptable risk in energy'; the other readings
 *   (expected_value_dominant, option_value_preserving) instantiate different
 *   constraints with different victim sets and beneficiaries. This story
 *   describes the catastrophic-tail reading as the structure that currently
 *   dominates policy, with all its extractive consequences for
 *   present-generation air quality and fossil fuel workers.
 *
 * KEY AGENTS:
 *   - nuclear_abandonment_movements: agenda-setter enforcing tail-dominance reading (institutional power, global scope)
 *   - renewable_energy_advocates: beneficiary, benefits from constraint's elevation of renewables as default safe path (organized power, global scope)
 *   - coal_miners_and_workers: victim, identity-locked into coal because nuclear is forbidden and renewables are intermittent (powerless, local scope, identity_locked exit)
 *   - present_generation_air_quality_victims: victim, suffer diffuse, distributed fossil fuel harms that the constraint discounts as acceptable and reversible (powerless, immediate horizon, trapped exit)
 *   - expected_value_analysts: observer, compute per-TWh mortality and show nuclear is empirically lower-risk, challenging the reading's legitimacy but lacking enforcement power (analytical power, global scope)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__catastrophic_tail_dominant, 0.68).
domain_priors:suppression_score(acceptable_risk_energy__catastrophic_tail_dominant, 0.79).
domain_priors:theater_ratio(acceptable_risk_energy__catastrophic_tail_dominant, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, extractiveness, 0.68).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__catastrophic_tail_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__catastrophic_tail_dominant, "Catastrophic Tail-Risk Dominance in Energy Policy").
narrative_ontology:topic_domain(acceptable_risk_energy__catastrophic_tail_dominant, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__catastrophic_tail_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__catastrophic_tail_dominant, 'e09a4d92-9a18-48bf-9a8d-7023209b2501').
narrative_ontology:cs_kernel_codification('e09a4d92-9a18-48bf-9a8d-7023209b2501', distributed).
narrative_ontology:cs_authority_grounding('e09a4d92-9a18-48bf-9a8d-7023209b2501', extraction).
narrative_ontology:cs_interpretation_layer_present('e09a4d92-9a18-48bf-9a8d-7023209b2501').
narrative_ontology:cs_reading_relation('e09a4d92-9a18-48bf-9a8d-7023209b2501', acceptable_risk_energy__expected_value_dominant, forecloses).
narrative_ontology:cs_reading_relation('e09a4d92-9a18-48bf-9a8d-7023209b2501', acceptable_risk_energy__option_value_preserving, coexists_with).
narrative_ontology:cs_axiom('e09a4d92-9a18-48bf-9a8d-7023209b2501', foundational, catastrophic_outcomes_intolerable).
narrative_ontology:cs_axiom_status(catastrophic_outcomes_intolerable, holdable).
narrative_ontology:cs_axiom_grounding('e09a4d92-9a18-48bf-9a8d-7023209b2501', catastrophic_outcomes_intolerable, deontological).
narrative_ontology:cs_axiom('e09a4d92-9a18-48bf-9a8d-7023209b2501', secondary, tail_risk_veto_unbounded).
narrative_ontology:cs_axiom_status(tail_risk_veto_unbounded, holdable).
narrative_ontology:cs_axiom_grounding('e09a4d92-9a18-48bf-9a8d-7023209b2501', tail_risk_veto_unbounded, deontological).
narrative_ontology:cs_reference_frame('e09a4d92-9a18-48bf-9a8d-7023209b2501', tail_outcome_aversion_foundational).
narrative_ontology:cs_drift_state('e09a4d92-9a18-48bf-9a8d-7023209b2501', contemporary_renewable_maturation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e09a4d92-9a18-48bf-9a8d-7023209b2501', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, renewable_energy_advocates).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_abandonment_movements).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, risk_aversion_constituencies).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_dependent_communities).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, coal_miners_and_workers).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, present_generation_air_quality_victims).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Frame the catastrophic-tail reading as the correct moral stance: nuclear risk, however small in probability, is unacceptable because the downside is unbounded. This reading elevates renewables by default (they lack tail catastrophe) and justifies accelerated funding, preferential grid access, and regulatory favor. They benefit from the constraint's operation because it legitimizes their policy preferences without requiring them to defend renewable reliability or cost-competitiveness against fossil fuels directly — the risk calculus preempts the comparison.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, renewable_energy_advocates, beneficiary,
    organized, generational, mobile, global).

% Set and enforce the acceptable-risk framework that prohibits nuclear expansion and hastens retirement. Operate through regulatory bodies, legislative coalitions, and public narrative control. Define what counts as 'acceptable' by centering catastrophic tail outcomes and using them as veto on nuclear pathways. Actively suppress the expected-value reading by framing it as morally bankrupt risk-shifting onto future generations.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_abandonment_movements, agenda_setter,
    institutional, generational, mobile, global).

% Genuinely prefer certainty over gambles with tail outcomes: citizens in seismic zones, near legacy reactor sites, or historically affected by industrial disasters. They benefit from a framework that treats their fears as morally central rather than as costs to be weighed and traded off. The constraint validates their risk preferences as ethical imperatives rather than as one input among many.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, risk_aversion_constituencies, beneficiary,
    moderate, biographical, constrained, national).

% Bear the extraction through prolonged coal and natural gas reliance because the acceptable-risk framework suppresses alternatives (nuclear is excluded by catastrophic-tail concern; renewables are still intermittent and underfunded). They face present air quality, water contamination, and mining-injury harms that the constraint systematically discounts as distributed and reversible — acceptable losses under the tail-dominant reading. Their exit is geographically and economically trapped.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_dependent_communities, payer,
    powerless, biographical, trapped, local).

% Sustain the coal pathway that the acceptable-risk framework treats as the default safety measure (avoid catastrophic-tail outcomes by staying with familiar, distributed harm). Their occupational identity is fused with coal extraction; exit requires abandoning not just a job but a community, professional identity, and regional economic structure. The constraint locks them in place by making coal the only socially acceptable alternative to the forbidden nuclear pathway.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, coal_miners_and_workers, payer,
    powerless, biographical, identity_locked, local).

% Suffer respiratory disease, particulate-matter mortality, water contamination, and agricultural losses from fossil fuel combustion. These harms are diffuse (not attributed to one facility or policy choice), delayed (accumulated over years), and normalized (treated as an unavoidable cost of electricity). The catastrophic-tail reading discounts them as reversible and distributed — acceptable under a framework that weights unbounded low-probability events above bounded high-probability ones.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, present_generation_air_quality_victims, payer,
    powerless, immediate, trapped, regional).

% Compute mortality risk per unit energy across pathways using best available epidemiology and engineering data. They find nuclear mortality risk substantially lower than coal on a per-TWh basis, making the catastrophic-tail reading indefensible on empirical grounds if the goal is to minimize expected harm. Their analysis challenges the acceptable-risk reading's legitimacy, but they lack enforcement power to override the political coalition supporting tail dominance.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, expected_value_analysts, observer,
    analytical, civilizational, analytical, global).

% Would optimize the energy mix to minimize aggregate mortality while maintaining reliability and cost-effectiveness. The catastrophic-tail reading excludes them from the relevant policy table by pre-deciding that certain pathways (nuclear) are forbidden regardless of empirical or economic comparison. Their exclusion is structural: they would argue the expected-value reading, which contradicts the tail-dominance frame.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, energy_infrastructure_planners, excluded,
    institutional, generational, constrained, global).

% Note the constraint embeds a specific value framework (catastrophic-tail aversion) as if it were a technical fact about risk. They observe that the reading suppresses the deeper question: what weight should unbounded tail outcomes carry relative to bounded distributed harms? The constraint presents one answer to a value question as if it were neutral policy.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, decision_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_energy__catastrophic_tail_dominant, renewable_energy_advocates).
narrative_ontology:fixing_cost_class(acceptable_risk_energy__catastrophic_tail_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared framework for deeming energy pathways acceptable or unacceptable based on tail-risk aversion, coordinating regulatory, legislative, and investment decisions around catastrophic-outcome prevention rather than aggregate harm minimization.
% TRANSFER_FUNCTION: Transfers occupational viability, air quality, and present-generation health from fossil fuel workers and communities to renewable energy constituencies and risk-averse populations. Moves decision-making authority from technocratic energy planners (who would optimize on mortality per TWh) to political coalitions organized around catastrophic tail outcomes.
% ABSENT_VOICES: Fossil fuel workers and present-generation air-quality victims are systematically excluded from the policy conversation because the constraint pre-decides their harms count as acceptable, distributed, and reversible. Energy infrastructure planners and expected-value analysts are excluded because their methodology contradicts the frame. Option-value-preserving advocates (those arguing for portfolio diversity under deep uncertainty about climate sensitivity and renewable scalability) are marginalized as climate denialists even when their argument is purely epistemic.
% DISAPPEARANCE_RATIONALE: If the catastrophic-tail-dominant reading disappeared overnight and expected-value analysis became the governing framework, nuclear would be permitted and likely expanded where it met cost and reliability targets; coal retirement would accelerate (not because of tail risk, but because per-TWh mortality is highest); renewable investment would be justified on a different basis (empirical success, not moral exclusion of alternatives); fossil fuel communities would face a different negotiation (transition support tied to expected-value harm reduction, not moral urgency of tail-outcome prevention). The entire apparatus of regulatory exclusion, narrative framing, and resource allocation would reorganize.
% FOUNDING_PROBLEM: How should societies weigh low-probability catastrophic outcomes (nuclear reactor failure, meltdown, long-term waste hazard) against high-probability distributed harms (coal air pollution, mining deaths, climate forcing) when choosing energy pathways?
% FOUNDING_PROBLEM_CORROBORATION: The constraint's beneficiaries (renewable advocates, anti-nuclear movements) frame the founding problem as still live: tail outcomes remain intolerable and any risk of them is unacceptable, making their approach timeless. Expected-value analysts and epidemiologists attest that the founding problem is CONTESTED: the empirical facts about risk per unit energy are now well-understood, and the dispute is entirely about value weightings (should distributed present-generation harms be discounted relative to hypothetical future tail risks?), not about uncertainty in the founding problem itself. This split corroboration indicates the constraint persists by enforcing a value preference, not by solving a lingering factual puzzle.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__catastrophic_tail_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__catastrophic_tail_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__catastrophic_tail_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_energy__catastrophic_tail_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__catastrophic_tail_dominant, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__catastrophic_tail_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_energy__catastrophic_tail_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_energy__catastrophic_tail_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.68 at interval end) because the constraint benefits one coalition (renewable advocates, risk-averse constituencies) through suppression of alternatives and systematic discounting of present-generation distributed harms. The extraction accelerates slightly (0.52 → 0.68) as coal is increasingly recognized as a bridging fuel while nuclear remains excluded, locking in fossil reliance. Suppression is high (0.79) because the constraint's persistence depends on actively excluding the expected-value reading from policy deliberation and on marginalizing analysts who compute per-TWh mortality. The constraint must suppress the empirical counter-argument (nuclear is lower-risk than coal) to maintain its legitimacy. Theater ratio is moderate (0.41) and rising: the constraint invokes safety and moral clarity, but an increasing share of its enforcement activity is suppressing the expected-value comparison rather than conducting genuine risk analysis. The measurement grid captures this: suppression requirement rises as the constraint matures, suggesting institutional energy is increasingly devoted to narrative control and exclusion rather than substantive risk analysis.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (renewable advocates, nuclear abandonment movements) and the payer seats (coal miners, air-quality victims) should compute very differently from this constraint. From the beneficiary position, this is a genuine coordination function that aligns moral clarity with energy policy: catastrophic tail outcomes are intolerable and should veto nuclear regardless of trade-offs. From the payer position, this is enforced extraction: a value preference (tail-outcome aversion over expected-value minimization) is presented as technical rationality, suppressing the comparison that would justify coal retirement or renewable acceleration on other grounds. The engine computes this divergence from the structural data (beneficiary/victim declarations + directionality + exit options): payers have trapped or identity-locked exits, beneficiaries have mobile exit and control the agenda-setting apparatus.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear_abandonment_movements and renewable_energy_advocates are structural beneficiaries (d near 0.0): they set the framework, benefit from narrative control, and face no material cost from the constraint's operation — their exit is mobile and they never face suppression. Coal miners and air-quality victims are structural targets (d near 1.0): they bear extraction through prolonged coal reliance (no exit because nuclear is forbidden, renewables are insufficient), and they face high suppression (their harms are discounted as distributed/reversible; their policy proposals are excluded). Risk-aversion_constituencies are near-symmetric but slightly toward beneficiary (d near 0.4): they genuinely benefit from a framework that validates their risk preferences, but they also face diffuse harm if coal extends because renewables remain underfunded. The constraint systematically understates the directionality of coal-dependent communities by treating their harms as acceptable and reversible — a suppression mechanism embedded in the constraint's logic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to weight tail outcomes against distributed present-generation harm — is live but the constraint presents one answer (tail dominance) as if it were the answer. The mandatrophy question: does the constraint still solve the founding problem, or does it now primarily extract from coal communities on behalf of risk-averse constituencies? Evidence: (1) the empirical facts about per-TWh mortality are now well-established; the dispute is entirely about value weightings, not about unknown risks. (2) The constraint increasingly suppresses the expected-value reading rather than conducting risk analysis, suggesting its function has shifted from solving a factual puzzle to enforcing a value preference. (3) Coal-dependent communities lack alternative exits and face identifiable harms that are now better-quantified, yet are still discounted by the constraint as acceptable. Verdict: the constraint has crossed from solving a genuine coordination problem into extractive enforcement of a particular risk-aversion stance. The mandatrophy is partial but significant — the founding problem is not entirely dead (tail outcomes are legitimately concerning) but the constraint now extracts substantially more than it coordinates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tail_outcome_weight_arbitrariness,
    'What is the principled basis for weighting catastrophic tail outcomes infinitely (or near-infinitely) when computing acceptable risk, rather than weighting them by probability and magnitude like other outcomes?',
    'Formal decision-theoretic analysis showing why expected-utility maximization should not apply to energy risk; or empirical discovery that tail outcomes are not actually unbounded (e.g., new containment technologies, geological stability data) such that they reduce to bounded outcomes and rejoin the expected-value calculation.',
    'If no principled basis exists, the constraint''s legitimacy rests on value preference enforcement rather than on technical risk analysis, and the beneficiary/victim asymmetry becomes the primary classification driver. If tail outcomes are demonstrably unbounded and incommensurate with other risks, the constraint''s logic is vindicated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tail_outcome_weight_arbitrariness, conceptual, 'Whether tail-outcome aversion is a defensible decision principle or an arbitrary value weighting presented as technical fact.').

omega_variable(
    distributed_harm_discounting_mechanism,
    'Why are distributed, high-probability harms (coal air pollution, mining deaths) counted as acceptable and reversible while concentrated, low-probability harms (nuclear accident) are counted as intolerable and catastrophic?',
    'Epidemiological reassessment of fossil fuel health costs with attribution methods that assign causality more directly (moving them from distributed to concentrated in accounting); or legal/regulatory change that treats chronic distributed harm the same as acute tail harm in acceptable-risk frameworks.',
    'If distributed harms are merely harder to see but equally real, the constraint''s asymmetry becomes indefensible and the expected-value reading becomes more plausible. If distributed harms are genuinely less morally salient than catastrophic ones, the constraint''s logic is vindicated but must be stated explicitly as a value claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_harm_discounting_mechanism, empirical, 'Whether the constraint''s asymmetry between distributed and catastrophic harms rests on genuine moral/epistemic distinction or on visibility bias.').

omega_variable(
    coal_community_exit_impossibility,
    'To what extent is the coal miner''s identity-locked exit a product of the catastrophic-tail-dominant constraint (nuclear forbidden, renewables underfunded), and to what extent is it a pre-existing structural feature of coal-dependent regions?',
    'Natural experiment: jurisdiction that switches to expected-value analysis and permits nuclear expansion while accelerating coal retirement with worker transition support. Observe whether coal miners'' exit options change (become less identity-locked) in the new framework.',
    'If the tail-dominance constraint is a primary lock (without it, coal communities would have access to nuclear jobs or accelerated renewable transition), the constraint''s extraction is substantial and culpable. If coal communities are locked-in for deeper structural reasons, the constraint''s role in extraction diminishes but does not disappear.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coal_community_exit_impossibility, empirical, 'The degree to which the catastrophic-tail reading creates or merely perpetuates coal-community lock-in.').

omega_variable(
    kernel_reading_under_determination,
    'Are the three readings (catastrophic-tail-dominant, expected-value-dominant, option-value-preserving) truly distinct constraints with different ε values, or are they interpretations of a single underlying constraint whose type is observer-relative?',
    'Structural analysis: if the three readings produce identical victim and beneficiary sets but merely disagree about how to weight them, they may be three perspectives on one constraint; if they produce different victim/beneficiary sets (as hypothesized: tail-dominant has coal miners as victims, expected-value has renewable-dependent investors as victims), they are distinct constraints by the ε-invariance principle.',
    'If genuinely distinct, each should be a separate story with its own constraint_id, linked via network.affects_constraints. If observer-relative, the constraint is being misdecomposed and a single flexible framework is being artificially split.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_under_determination, conceptual, 'Whether the three energy-risk readings are separate constraints or interpretations of one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__catastrophic_tail_dominant, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(acce_tr_t0, observed).
narrative_ontology:measurement(acce_tr_t5, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(acce_tr_t5, observed).
narrative_ontology:measurement(acce_tr_t10, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(acce_tr_t10, observed).
narrative_ontology:measurement(acce_tr_t15, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(acce_tr_t15, observed).
narrative_ontology:measurement(acce_tr_t25, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(acce_tr_t25, observed).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(acce_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(acce_be_t0, observed).
narrative_ontology:measurement(acce_be_t5, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 5, 0.58).
narrative_ontology:measurement_basis(acce_be_t5, observed).
narrative_ontology:measurement(acce_be_t10, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(acce_be_t10, observed).
narrative_ontology:measurement(acce_be_t15, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(acce_be_t15, observed).
narrative_ontology:measurement(acce_be_t25, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(acce_be_t25, observed).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(acce_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 0, 0.71).
narrative_ontology:measurement_basis(acce_su_t0, observed).
narrative_ontology:measurement(acce_su_t5, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 5, 0.74).
narrative_ontology:measurement_basis(acce_su_t5, observed).
narrative_ontology:measurement(acce_su_t10, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 10, 0.75).
narrative_ontology:measurement_basis(acce_su_t10, observed).
narrative_ontology:measurement(acce_su_t15, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 15, 0.77).
narrative_ontology:measurement_basis(acce_su_t15, observed).
narrative_ontology:measurement(acce_su_t25, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 25, 0.78).
narrative_ontology:measurement_basis(acce_su_t25, observed).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 40, 0.79).
narrative_ontology:measurement_basis(acce_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__catastrophic_tail_dominant, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(acceptable_risk_energy__catastrophic_tail_dominant, 0.12).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy__expected_value_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy__option_value_preserving).

% DUAL FORMULATION NOTE:
% This constraint is part of the acceptable_risk_energy constraint family (kernel acceptable_risk_energy with three readings). The catastrophic_tail_dominant reading (this story) forecloses expected_value_dominant within a single framework but coexists_with option_value_preserving as live alternative positions held by different parties. All three readings share the same founding problem (how to weight tail outcomes against distributed harms) but structure victims and beneficiaries differently and achieve different types under engine computation. They are linked because the tail-dominance reading actively suppresses the expected-value reading through narrative and regulatory control, making the causal relationship directional.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_energy__catastrophic_tail_dominant, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
