% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__option_value_preserving
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__option_value_preserving, []).

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
 *   constraint_id: acceptable_risk_energy__option_value_preserving
 *   human_readable: Option-Value-Preserving Energy Risk Framework
 *   domain: energy_policy/risk_assessment/decision_theory
 *
 * SUMMARY:
 *   An energy risk framework gates policy decisions on the principle that
 *   deep uncertainty justifies maintaining multiple energy pathways (nuclear,
 *   fossil with carbon capture, renewables, etc.) rather than closing any
 *   pathway prematurely. The logic is: under unknowable futures, flexibility
 *   preserves option value and defers regret. This reading operationalizes
 *   that principle as a constraint. It coexists with two sibling readings:
 *   one that prioritizes catastrophic climate-tail outcomes
 *   (catastrophic_tail_dominant, foreclosing the premise that fossil pathways
 *   should remain open), and one that uses mortality-per-TWh expected-value
 *   metrics to declare all pathways directly (expected_value_dominant,
 *   sidelining uncertainty hedging). This reading instantiates the
 *   option-value version: it maintains the victim set (deployment-delay
 *   constituencies, asset-closure deferrers) and the moderate suppression
 *   (both catastrophe-tail advocates and rapid-decarbonization pushes are
 *   modulated by the 'wait and preserve options' logic).
 *
 * KEY AGENTS:
 *   - energy_portfolio_flexibility_advocates: institutional agenda-setters (policymakers, energy economists) who design and defend multiple-pathway regulatory frameworks.
 *   - premature_closure_losers: powerful constituencies (coal regions, nuclear suppliers, fossil operators) bearing the opportunity costs of pathway redundancy and regulatory uncertainty.
 *   - deployment_delay_constituencies: organized victims (climate advocates, renewable manufacturers, pollution-affected communities) paying the costs of continued fossil viability and delayed renewable acceleration.
 *   - energy_system_operators: institutional beneficiaries and secondary agenda-setters deferring asset retirement and transition costs under flexibility framework.
 *   - catastrophe_tail_prioritizers: excluded (structurally incommensurable with the framework's core axiom) — their premise that low-probability high-severity outcomes override expected-value logic is de-privileged by the framework's operationalization.
 *   - regulatory_bodies: observers translating option-value logic into permit, subsidy, and procurement rules.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__option_value_preserving, 0.58).
domain_priors:suppression_score(acceptable_risk_energy__option_value_preserving, 0.52).
domain_priors:theater_ratio(acceptable_risk_energy__option_value_preserving, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, extractiveness, 0.58).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__option_value_preserving, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__option_value_preserving, "Option-Value-Preserving Energy Risk Framework").
narrative_ontology:topic_domain(acceptable_risk_energy__option_value_preserving, "energy_policy/risk_assessment/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__option_value_preserving).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__option_value_preserving, '75a18dba-7616-47a7-b88a-f3f24434295a').
narrative_ontology:cs_kernel_codification('75a18dba-7616-47a7-b88a-f3f24434295a', distributed).
narrative_ontology:cs_authority_grounding('75a18dba-7616-47a7-b88a-f3f24434295a', expertise).
narrative_ontology:cs_interpretation_layer_present('75a18dba-7616-47a7-b88a-f3f24434295a').
narrative_ontology:cs_reading_relation('75a18dba-7616-47a7-b88a-f3f24434295a', acceptable_risk_energy__acceptable_risk_energy_catastrophic_tail_dominant, forecloses).
narrative_ontology:cs_reading_relation('75a18dba-7616-47a7-b88a-f3f24434295a', acceptable_risk_energy__acceptable_risk_energy_expected_value_dominant, coexists_with).
narrative_ontology:cs_axiom('75a18dba-7616-47a7-b88a-f3f24434295a', foundational, irreducible_deep_uncertainty_requires_option_preservation).
narrative_ontology:cs_axiom_status(irreducible_deep_uncertainty_requires_option_preservation, holdable).
narrative_ontology:cs_axiom_grounding('75a18dba-7616-47a7-b88a-f3f24434295a', irreducible_deep_uncertainty_requires_option_preservation, empirically_contingent).
narrative_ontology:cs_axiom('75a18dba-7616-47a7-b88a-f3f24434295a', foundational, regret_minimization_across_unknowable_futures).
narrative_ontology:cs_axiom_status(regret_minimization_across_unknowable_futures, holdable).
narrative_ontology:cs_axiom_grounding('75a18dba-7616-47a7-b88a-f3f24434295a', regret_minimization_across_unknowable_futures, deontological).
narrative_ontology:cs_reference_frame('75a18dba-7616-47a7-b88a-f3f24434295a', uncertainty_hedging_decision_framework).
narrative_ontology:cs_drift_state('75a18dba-7616-47a7-b88a-f3f24434295a', climate_science_convergence_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('75a18dba-7616-47a7-b88a-f3f24434295a', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, energy_portfolio_flexibility_advocates).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, premature_closure_losers).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, deployment_delay_constituencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, energy_system_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, deep_uncertainty_epistemicists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Policymakers, energy economists, and decision scientists who design regulatory frameworks operationalizing the option-value principle. They argue that deep uncertainty about future climate, technology costs, and demand justifies keeping multiple energy pathways open rather than closing any prematurely. They set the agenda through design of technology-neutral permitting, portfolio-wide subsidy allocation, and regulatory cost-benefit frameworks that weight irreducible uncertainty. They benefit from institutional influence, policy legitimacy, and from avoiding the visible costs of early pathway closure.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, energy_portfolio_flexibility_advocates, agenda_setter,
    institutional, generational, arbitrage, global).

% Coal mining communities, nuclear fuel suppliers, fossil-generation operators, and infrastructure investors whose viability depends on the pathways the framework keeps half-open. They bear the opportunity costs of regulatory uncertainty — operators defer retirement and expansion decisions, workers face wage volatility, supply chains remain in limbo, and capital is diverted to maintain redundant infrastructure. Their exit from the constraint would require early pathway closure and stranded-asset write-downs they cannot control.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, premature_closure_losers, payer,
    powerful, biographical, constrained, global).

% Climate advocates, renewable-energy-dependent regions, communities bearing air and water pollution from continued fossil operation, and renewable-technology manufacturers. They experience the framework as permission to defer harm reduction: continued emissions, pollution deaths, ecosystem damage, and renewable market-share erosion while fossil pathways remain viable. The option-value logic translates, from their position, to deferral of transition costs onto future generations and vulnerable populations. They cannot exit the constraint without fundamental policy shift.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, deployment_delay_constituencies, payer,
    organized, biographical, constrained, global).

% Utilities, grid operators, and energy companies managing generation, transmission, and distribution. The framework buys them time to defer expensive transition investments, maintain redundant generation assets, and preserve capital flexibility across multiple fuel sources. They benefit from continued regulatory protection for diverse pathways, reduced pressure to retire infrastructure, and from the institutional entrenchment of multi-pathway planning that aligns with their operational interests. They have the mobility to pivot between fuel types and the institutional power to shape permitting timelines.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, energy_system_operators, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__option_value_preserving, energy_system_operators, agenda_setter).

% Risk theorists, decision scientists, and academic researchers whose epistemic framework — deep uncertainty as an irreducible feature of energy policy requiring option preservation — becomes institutionalized in regulatory design. They benefit from policy influence, research funding tied to uncertainty-hedging frameworks, and from the credibility the framework grants to their analytical categories. They are insulated from the harms of continued fossil operation and deployment deferral because their professional vantage is analytical rather than operational.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, deep_uncertainty_epistemicists, beneficiary,
    moderate, generational, mobile, global).

% Climate scientists, catastrophe researchers, and advocates prioritizing low-probability high-severity climate outcomes. They argue that rapid decarbonization is required regardless of near-term expected-value mortality costs, and that maintaining fossil pathways as viable choices embeds catastrophic climate lock-in. They are structurally excluded from the framework's authority structure: regulatory cost-benefit processes systematize around expected-value and option-preservation logic, and processes de-privilege catastrophe-modeling and tail-risk testimony. Their core premise (catastrophic outcomes override expected-value calculation) is incommensurable with the framework's foundational axiom.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, catastrophe_tail_prioritizers, excluded,
    organized, generational, constrained, global).

% Government energy agencies, environmental regulators, climate commissions, and permitting authorities that must operationalize the option-value framework. They translate the principle into concrete rules: technology-neutral procurement, portfolio-wide subsidy allocation, renewable-integration timelines, and fossil-pathway permitting decisions. They absorb pressure from beneficiary and payer constituencies, manage the framework's contradictions as evidence emerges, and face pressure to reclassify when policy failures surface. They are positioned as neutral administrators but are de facto enforcers of the framework's constraints on catastrophe-tail and rapid-decarbonization advocacy.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, regulatory_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_energy__option_value_preserving, energy_system_operators).
narrative_ontology:fixing_cost_class(acceptable_risk_energy__option_value_preserving, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the allocation-of-decision-flexibility problem under deep uncertainty: when future technological and climate trajectories are radically unknowable, maintaining multiple energy pathways distributes the risk of premature closure across all options rather than concentrating regret on a single chosen path. This is a genuine coordination function if deep uncertainty is real and irreducible.
% TRANSFER_FUNCTION: Moves the opportunity costs of pathway redundancy and deployment delay from option-preserving beneficiaries to the constituencies bearing the costs of continued fossil operation and renewable acceleration deferral — coal regions deferring closure, climate-vulnerable communities bearing continued pollution, renewable manufacturers facing market-share erosion from continued fossil viability.
% ABSENT_VOICES: Catastrophic-tail-risk prioritizers are structurally excluded: their core premise (low-probability, high-severity outcomes justify rapid decarbonization) is incommensurable with the framework's foundational axiom. They would object that option preservation is a cover story for path-dependent inertia and that deferring closure on fossil pathways embeds the regret of catastrophic climate change into the present. Regulatory processes built around expected-value and option-preservation logics systematically de-privilege catastrophe-modeling and tail-risk testimony.
% DISAPPEARANCE_RATIONALE: If the option-value framework vanished and the constraint ceased to operate, energy policy would snap to either rapid decarbonization (tail-dominant reading) or pure expected-value mortality minimization (expected-value reading). Coal plants would close on accelerated timelines; nuclear subsidy would consolidate or terminate based on mortality-per-TWh calculation; renewable deployment would accelerate; stranded assets would materialize rapidly. The energy system would reorganize around a single criterion rather than maintaining portfolios.
% FOUNDING_PROBLEM: Irreducible deep uncertainty about future climate sensitivity, technology costs, demand trajectories, and societal preferences makes it impossible to know ex ante which energy pathway will minimize total harm. Locking in a single path commits future generations to regretting the choice if circumstances diverge from current expectations. Maintaining optionality defers that regret and preserves flexibility.
% FOUNDING_PROBLEM_CORROBORATION: Deep-uncertainty scholars (Lempert, Popper, Bankes on ROBUST decision-making) attest the founding problem is live and motivate option-preservation logic. Expected-value modelers (mortality-per-TWh researchers, IPCC integrated assessment modelers) attest the problem is substantially over-stated and that expected-value calculation under acknowledged uncertainty already hedges tail risks appropriately. Climate catastrophe researchers attest the founding problem is a rationalization for path-dependent inertia: uncertainty is not symmetric across pathways — fossil carbon lock-in creates irreversible climate trajectories while renewable delay is reversible, so 'option preservation' privileges the irreversible wrong choice. Outside the benefiting institutional set, the corroboration for the founding problem comes exclusively from academics with influence on the framework's legitimacy; testimony from constituencies bearing the deployment-delay costs does not corroborate the founding problem — it contests it.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__option_value_preserving, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__option_value_preserving, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__option_value_preserving, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_energy__option_value_preserving, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__option_value_preserving, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__option_value_preserving_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_energy__option_value_preserving, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_energy__option_value_preserving_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness plateaus around 0.58–0.60 and then slightly declines (plateau effect rather than accumulation) because the framework reaches saturation: continued maintenance of multiple pathways becomes routine policy rather than contested novelty, and emergence of clearer climate/technology signals begins to push back against pure uncertainty hedging. Theater ratio peaks around 15–20 years as the 'flexibility' narrative becomes ritualized in regulatory language divorced from real deployment decisions, then stabilizes as both the beneficiaries and victims grow accustomed to the framework's operations. Suppression requirement follows a similar trajectory: maximum pressure on both catastrophe advocates (constrain their decarbonization push) and option-closure advocates (prevent early nuclear/fossil retirement) occurs mid-interval, then eases as the framework normalizes. The trajectory is not monotonic extraction-accumulation because the constraint reaches a stable operating point where all parties have incorporated the framework into their strategies and expectations.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary/agenda-setter seat (flexibility advocates, energy operators), the framework is a genuine solution to an irreducible coordination problem — multiple parties must make decisions under shared uncertainty, and preserving options defers commitment regret. From the payer seats (coal regions, climate advocates), the framework operates as enforced extraction: the option-value logic is a cover story for path-dependent inertia that defers harms (continued emissions, asset stranding, pollution costs) onto those constituencies. The engine computes this divergence from the structural data: different power levels, different exit options (operators have arbitrage; constituencies are constrained), different time horizons (flexibility advocates think generational; deployment-delay constituencies think biographical). The authorized framework (how regulatory bodies operationalize option preservation) modulates both sides: it suppresses the catastrophe-tail advocates' testimony (de-privileges tail-risk modeling in cost-benefit analysis) AND suppresses rapid-decarbonization pressure (preserves permitting for fossil generation under 'technology-neutral' rules). Both suppressions are necessary for the framework's persistence.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/agenda-setter (flexibility advocates, operators) sits near d=0.1–0.2 (net beneficiary, arbitrage exit). The victim constituencies sit near d=0.8–0.9 (constrained, bearing deferred costs). This is not a single-seat story: it is a two-seat bifurcation driven by structural relationship to the pathway-redundancy decision. Regulatory bodies and catastrophe-tail advocates occupy intermediate positions: regulators are nominally neutral (observer seat, d=0.5) but carry the institutional burden of managing the framework's contradictions; catastrophe advocates are identity-locked into the tail-risk epistemic frame (d≈0.85–0.95, trapped by professional commitment to modeling catastrophe) but excluded from the framework's authority structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (irreducible deep uncertainty) is CONTESTED. Tail-risk researchers and expected-value modelers both argue the uncertainty is not as deep or symmetric as flexibility advocates claim, and that rational decision-making under uncertainty does not require maintaining two incompatible pathways simultaneously — expected-value hedging or tail-risk prioritization are sufficient. The framework's persistence does NOT depend on a live founding problem; it depends on continued institutional buy-in from energy operators and regulatory agencies that benefit from deferral. This is a tangled-rope signature: the coordination (multiple-pathway flexibility under uncertainty) rides on asymmetric extraction (deferred costs landing on pollution-affected constituencies and renewable-acceleration advocates). The constraint requires active enforcement (regulatory suppression of catastrophe-tail testimony, technology-neutral permitting that keeps fossil pathways open) to persist. Without that enforcement, energy policy would snap to a single criterion (expected-value or tail-dominant). Classification as tangled_rope rather than snare depends on whether the flexibility coordination function is genuine or merely ornamental. The measurement series show theater_ratio peaking at 0.41–0.42 (moderately theatrical: enforcement activity increasingly divorced from flexibility-generation — permitting is routine, funding is locked in, the 'decision flexibility' is abstract), which is consistent with tangled_rope mid-range operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deep_uncertainty_reducibility,
    'Is the deep uncertainty about future energy technology, climate sensitivity, and demand genuinely irreducible, or can it be substantially resolved through climate science advances and technology-pathway research?',
    'Measurable convergence in climate modeling outputs (IPCC AR5→AR6 range compression), technology cost-curve predictability (learning rates empirically consistent), and demand-scenario variance reduction; or continued high structural variance despite decades of research effort.',
    'If uncertainty is demonstrably reducible on the relevant timescale (10–20 years), the founding problem declines and option-preservation transitions from justified hedging to inertia-protection. The constraint''s classification would shift toward snare as the coordination rationale weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deep_uncertainty_reducibility, empirical, 'Whether deep energy-policy uncertainty is truly irreducible or progressively resolvable.').

omega_variable(
    pathway_symmetry_assumption,
    'Are all maintained energy pathways structurally equivalent in their option-value — i.e., is keeping fossil open as a future choice equivalent in regret-minimization value to keeping nuclear or renewable open?',
    'Analysis of path-dependency and irreversibility: fossil carbon lock-in creates centuries-scale climate commitment; renewable delay is reversible on decadal timescales. If asymmetric, the framework''s symmetry assumption fails.',
    'If pathways are asymmetrically reversible, maintaining fossil pathways is not option preservation — it is locking in the irreversible choice while keeping reversible options open. This would reclassify the constraint as snare (the ''flexibility'' is cover for fossil-pathway lock-in).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pathway_symmetry_assumption, conceptual, 'Whether all energy pathways have equivalent option value under deep uncertainty.').

omega_variable(
    regulatory_suppression_mechanism,
    'How much of the framework''s persistence depends on active institutional suppression of catastrophe-tail and rapid-decarbonization testimony, versus voluntary buy-in from all constituencies?',
    'Regulatory process audit: measure the proportion of submitted testimony, modeling, and cost-benefit analyses from catastrophe-risk and tail-heavy traditions that appear in formal decision records vs. in public comment; track exclusion patterns.',
    'High suppression indicates the framework''s coercive character; low suppression indicates genuine pluralistic accommodation. High suppression supports the tangled_rope classification (coordination + extraction); absence of suppression would support rope classification (genuine multi-sided benefit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_suppression_mechanism, empirical, 'Degree of institutional suppression vs. voluntary pluralism in the framework''s operation.').

omega_variable(
    kernel_reading_incommensurability,
    'Are the catastrophic-tail and option-value readings genuinely incommensurable (foreclosure relation), or can they coexist within a single decision framework?',
    'Formal decision analysis: can a single utility function, cost-benefit structure, or policy objective weight both expected-value and tail-risk hedging? If yes, coexistence; if no, foreclosure.',
    'If foreclosing: the readings are contending ontologies, and the framework''s persistence depends on institutional victory for option-value over tail-dominant; if coexisting: both are live epistemic positions and the framework accommodates both. The relation characterization affects how external evidence (emergent climate signals, technology surprise) would trigger reclassification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether option-value and catastrophic-tail readings are logically incommensurable or coexistable within one framework.').

omega_variable(
    victim_set_boundary_ambiguity,
    'Do deployment-delay constituencies (climate advocates, renewable manufacturers) constitute victims of the framework, or beneficiaries (they get portfolio insurance against renewable-only lock-in)?',
    'Post-transition analysis: if the framework is abandoned and rapid decarbonization occurs, do these constituencies report that deferral harmed their interests (victimhood) or protected them (beneficiary status from option value)? Or do they bifurcate into constituencies with opposed interests?',
    'High beneficiary component would downgrade the extraction reading; sole victimhood would strengthen it. Bifurcation would indicate the constraint sorts actors differently than authored (e.g., some renewable manufacturers benefit from deployment delay if it lets them optimize cost curves).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_boundary_ambiguity, empirical, 'Whether deployment-delay constituencies are net victims or net beneficiaries of the framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__option_value_preserving, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_energy__option_value_preserving, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(acce_tr_t0, observed).
narrative_ontology:measurement(acce_tr_t5, acceptable_risk_energy__option_value_preserving, theater_ratio, 5, 0.33).
narrative_ontology:measurement_basis(acce_tr_t5, observed).
narrative_ontology:measurement(acce_tr_t10, acceptable_risk_energy__option_value_preserving, theater_ratio, 10, 0.38).
narrative_ontology:measurement_basis(acce_tr_t10, observed).
narrative_ontology:measurement(acce_tr_t15, acceptable_risk_energy__option_value_preserving, theater_ratio, 15, 0.41).
narrative_ontology:measurement_basis(acce_tr_t15, observed).
narrative_ontology:measurement(acce_tr_t20, acceptable_risk_energy__option_value_preserving, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(acce_tr_t20, observed).
narrative_ontology:measurement(acce_tr_t25, acceptable_risk_energy__option_value_preserving, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(acce_tr_t25, observed).
narrative_ontology:measurement(acce_tr_t30, acceptable_risk_energy__option_value_preserving, theater_ratio, 30, 0.4).
narrative_ontology:measurement_basis(acce_tr_t30, observed).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_energy__option_value_preserving, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(acce_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_energy__option_value_preserving, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(acce_be_t0, observed).
narrative_ontology:measurement(acce_be_t5, acceptable_risk_energy__option_value_preserving, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(acce_be_t5, observed).
narrative_ontology:measurement(acce_be_t10, acceptable_risk_energy__option_value_preserving, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(acce_be_t10, observed).
narrative_ontology:measurement(acce_be_t15, acceptable_risk_energy__option_value_preserving, base_extractiveness, 15, 0.57).
narrative_ontology:measurement_basis(acce_be_t15, observed).
narrative_ontology:measurement(acce_be_t20, acceptable_risk_energy__option_value_preserving, base_extractiveness, 20, 0.59).
narrative_ontology:measurement_basis(acce_be_t20, observed).
narrative_ontology:measurement(acce_be_t25, acceptable_risk_energy__option_value_preserving, base_extractiveness, 25, 0.6).
narrative_ontology:measurement_basis(acce_be_t25, observed).
narrative_ontology:measurement(acce_be_t30, acceptable_risk_energy__option_value_preserving, base_extractiveness, 30, 0.59).
narrative_ontology:measurement_basis(acce_be_t30, observed).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_energy__option_value_preserving, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(acce_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_energy__option_value_preserving, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(acce_su_t0, observed).
narrative_ontology:measurement(acce_su_t5, acceptable_risk_energy__option_value_preserving, suppression_requirement, 5, 0.42).
narrative_ontology:measurement_basis(acce_su_t5, observed).
narrative_ontology:measurement(acce_su_t10, acceptable_risk_energy__option_value_preserving, suppression_requirement, 10, 0.47).
narrative_ontology:measurement_basis(acce_su_t10, observed).
narrative_ontology:measurement(acce_su_t15, acceptable_risk_energy__option_value_preserving, suppression_requirement, 15, 0.51).
narrative_ontology:measurement_basis(acce_su_t15, observed).
narrative_ontology:measurement(acce_su_t20, acceptable_risk_energy__option_value_preserving, suppression_requirement, 20, 0.54).
narrative_ontology:measurement_basis(acce_su_t20, observed).
narrative_ontology:measurement(acce_su_t25, acceptable_risk_energy__option_value_preserving, suppression_requirement, 25, 0.56).
narrative_ontology:measurement_basis(acce_su_t25, observed).
narrative_ontology:measurement(acce_su_t30, acceptable_risk_energy__option_value_preserving, suppression_requirement, 30, 0.54).
narrative_ontology:measurement_basis(acce_su_t30, observed).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_energy__option_value_preserving, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(acce_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__option_value_preserving, resource_allocation).
narrative_ontology:boltzmann_floor_override(acceptable_risk_energy__option_value_preserving, 0.18).
narrative_ontology:affects_constraint(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy_catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy_expected_value_dominant).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel 'acceptable_risk_energy'. All three share a referent (energy-pathway decision-making under uncertainty) but instantiate different decision criteria and victim sets. The option-value reading maintains both nuclear and fossil pathways as viable (moderate suppression of both extremes). The catastrophic-tail reading prioritizes climate-tail outcomes and would foreclose fossil pathways. The expected-value reading uses mortality-per-TWh to declare all pathways on a single metric. The three readings are linked via network.affects_constraints as a constraint family, documenting the kernel decomposition (ε-invariance principle: each reading has its own ε because each reading makes a different empirical/normative claim about which outcomes should count in 'acceptable risk').

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_energy__option_value_preserving, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
