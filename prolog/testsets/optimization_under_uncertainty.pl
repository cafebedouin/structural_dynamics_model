% ============================================================================
% CONSTRAINT STORY: optimization_under_uncertainty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_optimization_under_uncertainty, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: optimization_under_uncertainty
 *   human_readable: Optimization Under Uncertainty: Extraction Through Deferral
 *   domain: decision_theory/epistemology/institutional_governance
 *
 * SUMMARY:
 *   Optimization under uncertainty represents a fundamental structural
 *   tension between the desire for rational decision-making and the
 *   irreducible epistemic limits on knowledge about future states.
 *   Institutional actors invoke uncertainty as justification for centralized
 *   decision-making authority—framing optimization-under-uncertainty as a
 *   technical problem requiring expert resolution. However, this framing
 *   masks an extraction mechanism: the authority to decide persists through
 *   deferral ("we cannot decide yet because uncertainty is too high"), while
 *   consequence-bearers have no voice and no exit. The constraint exhibits
 *   all six classifications across different perspectives, revealing how
 *   optimization rhetoric naturalizes institutional gatekeeping. The theater
 *   ratio has increased over the measurement interval (0.35 → 0.58) as the
 *   formal sophistication of uncertainty quantification (Bayesian networks,
 *   Monte Carlo methods, sensitivity analysis) has grown without proportional
 *   increase in actual decision reversibility or stakeholder influence. The
 *   extractiveness trajectory reflects historical layering: early
 *   uncertainty-based decisions were coordinated (communities believed in
 *   expert optimization); over time, repeated failures and revealed conflicts
 *   of interest transformed the coordination mechanism into extraction.
 *
 * KEY AGENTS:
 *   - Decision-Making Authority: Primary beneficiary (institutional/arbitrage) — maintains decision-making power while deferring accountability through uncertainty rhetoric
 *   - Consequence-Bearing Stakeholders: Primary victim (powerless/trapped) — affected by optimization outcomes without voice, exit, or ability to challenge decisions
 *   - Information Gatekeepers: Secondary beneficiary (institutional/arbitrage) — control modeling authority and uncertainty characterization; maintain knowledge asymmetries
 *   - Affected Communities: Secondary victim (moderate/constrained) — can develop countervailing expertise but face resource barriers and institutional resistance
 *   - Adaptive Governance Practitioners: Organized agents (organized/mobile) — building decentralized optimization alternatives with genuine stakeholder participation
 *   - Epistemic Commons: Abstract victim (powerless/trapped) — accumulating suboptimal decisions and knowledge contamination from deferred accountability
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as inevitable features of decision-making
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(optimization_under_uncertainty, 0.52).
domain_priors:suppression_score(optimization_under_uncertainty, 0.48).
domain_priors:theater_ratio(optimization_under_uncertainty, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(optimization_under_uncertainty, extractiveness, 0.52).
narrative_ontology:constraint_metric(optimization_under_uncertainty, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(optimization_under_uncertainty, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(optimization_under_uncertainty, tangled_rope).
narrative_ontology:human_readable(optimization_under_uncertainty, "Optimization Under Uncertainty: Extraction Through Deferral").
narrative_ontology:topic_domain(optimization_under_uncertainty, "decision_theory/epistemology/institutional_governance").

domain_priors:requires_active_enforcement(optimization_under_uncertainty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(optimization_under_uncertainty, decision_making_authority).
narrative_ontology:constraint_beneficiary(optimization_under_uncertainty, information_gatekeepers).
narrative_ontology:constraint_victim(optimization_under_uncertainty, stakeholders_bearing_consequences).
narrative_ontology:constraint_victim(optimization_under_uncertainty, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSEQUENCE-BEARER (SNARE) — Stakeholders affected by optimization decisions have no exit from the system and cannot influence the decision-making process. They bear full costs of suboptimal choices made under uncertainty without voice or veto. Maximum experienced extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(optimization_under_uncertainty, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: AFFECTED COMMUNITY (TANGLED ROPE) — Communities experiencing optimization outcomes can organize and develop countervailing knowledge (community science, local expertise) but face resource barriers and institutional gatekeeping. Real coordination function (communities developing adaptive capacity) combined with asymmetric extraction (burden of proof on community to challenge expert optimization).
constraint_indexing:constraint_classification(optimization_under_uncertainty, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: OPTIMIZATION AUTHORITY (ROPE) — Decision-making institutions experience the constraint as pure coordination: optimizing under uncertainty requires aggregating dispersed information and making collective commitments. The authority benefits from the legitimacy provided by optimization rhetoric while maintaining decision-making authority. Low experienced extraction relative to power.
constraint_indexing:constraint_classification(optimization_under_uncertainty, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: RISK MANAGEMENT INFRASTRUCTURE (PITON) — Institutional mechanisms for uncertainty quantification and risk communication have become largely performative. Cost-benefit analysis, risk assessments, and sensitivity analyses produce elaborate theater while actual decisions follow pre-determined political and economic interests. Theater ratio reflects the gap between analytical sophistication and actual decision influence.
constraint_indexing:constraint_classification(optimization_under_uncertainty, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ADAPTIVE GOVERNANCE MOVEMENT (SCAFFOLD) — Organized agents (adaptive management practitioners, resilience researchers, decentralized decision-makers) are building alternative pathways that reduce reliance on centralized optimization under uncertainty. Strategies include polycentrism, participatory modeling, and iterative learning. These create a genuine sunset clause: as adaptive governance maturity increases, the extraction mechanism of centralized optimization-by-deferral loses force.
constraint_indexing:constraint_classification(optimization_under_uncertainty, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FUNDAMENTAL IMPOSSIBILITY VIEW (MOUNTAIN) — From a civilizational perspective, optimization under uncertainty appears as an immutable constraint: decision-making under incomplete information is a fundamental structural feature of all complex systems. No agent can escape the basic epistemic limitation that the future is unknowable. This perspective risks naturalizing what may be a contingent institutional arrangement (centralized optimization) as an inevitable feature of decision-making itself.
constraint_indexing:constraint_classification(optimization_under_uncertainty, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(optimization_under_uncertainty_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(optimization_under_uncertainty, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(optimization_under_uncertainty, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(optimization_under_uncertainty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(optimization_under_uncertainty, TR),
    TR >= 0.70.

:- end_tests(optimization_under_uncertainty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Institutional actors benefit from authority to optimize in the face of uncertainty while deferring accountability for outcomes. The extraction is not maximal because some genuine coordination function remains (uncertainty genuinely exists, optimization frameworks do aggregate information). But extractiveness has increased over time (0.28 → 0.52) as the gap between optimization rhetoric and actual reversibility of decisions has widened. Suppression (0.48): Moderate. Barriers include cognitive complexity (uncertainty quantification requires specialist knowledge), information asymmetries (gatekeepers control model design), resource requirements (participatory optimization requires investment communities rarely receive), and institutional gatekeeping (alternative models face dismissal as unrigorous). Suppression is not total because communities can develop countervailing expertise and adapt locally. Theater ratio (0.58): Moderate-high. Risk assessments, cost-benefit analyses, and sensitivity analyses have become increasingly elaborate without proportional increase in decision-reversal or stakeholder influence. The theater reflects the gap between analytical sophistication and actual governance change. As institutional decisions face challenge, institutions respond by elaborating their analytical frameworks rather than reconsidering decision authority itself.
 *
 * PERSPECTIVAL GAP:
 *   Maximum perspectival divergence across six classification types from identical structural data reveals how optimization-under-uncertainty functions as a constraint. The beneficiary institution experiences coordination (Rope), the powerless stakeholder experiences extraction (Snare), organized agents see a solvable problem (Scaffold), institutional infrastructure sees its own degradation (Piton), affected communities see asymmetric burden (Tangled Rope), and the analytical observer risks naturalizing a contingent arrangement (Mountain). The gap is not noise—it is the constraint's signature. Institutional actors benefit from the classification divergence: they can simultaneously claim coordination (to beneficiaries and to themselves), assert necessity (to analytical observers), and defer to uncertainty (to challenging stakeholders). The constraint's extractiveness depends on preventing different perspectives from recognizing each other's structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position within the extraction flow. Institutional authorities with arbitrage options experience low effective extraction (they benefit from the constraint). Powerless consequence-bearers with no exit experience maximum extraction (they bear costs without voice). Moderate agents with constrained options but organizational capacity experience mixed extraction. Organized agents with mobile options and developing alternatives experience lower extraction despite moderate suppression because they can see and access exit paths. The analytical observer risks identity-locking into the 'optimization is necessary' frame, making their mountain classification itself an instance of the constraint's operation: the observer's analytical authority over the concept of necessity mirrors the optimization authority's institutional authority over the concept of uncertainty.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by revealing that optimization-under-uncertainty can simultaneously be: (1) a genuine coordination problem (managing collective action under incomplete information), (2) an institutional extraction mechanism (maintaining authority while deferring accountability), and (3) a natural law constraint (the future is unknowable). All three are structurally true. The mandatrophy resolution requires distinguishing the genuine coordination function (which would justify Rope classification) from the extraction overlay (which requires Snare or Tangled Rope). The key differentiator is decision reversibility: if optimization decisions can be revisited and reversed when new information emerges, the constraint is primarily Rope (coordination with genuine learning). If decisions are locked in by institutional inertia and can only be revisited after massive stakeholder mobilization, the constraint is primarily Snare (extraction masked by uncertainty rhetoric). The measurement interval (0-10) shows extractiveness increasing while theater ratio increases, suggesting that the coordination function is degrading and the extraction function is strengthening. This is the signature of a constraint transitioning from Rope toward Snare, with Tangled Rope as the current hybrid state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    uncertainty_vs_institutional_gatekeeping,
    'How much of the constraint''s extractiveness derives from genuine epistemological uncertainty versus deliberate institutional gatekeeping of information and modeling authority?',
    'Comparison of extractiveness in high-transparency optimization regimes versus low-transparency regimes; analysis of information access restrictions; examination of countervailing expertise suppression patterns',
    'If primarily epistemic: constraint is closer to Mountain (irreducible). If primarily institutional: constraint is closer to Snare (contingent extraction). This distinction determines whether optimization-deferral is a natural law or a governance choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(uncertainty_vs_institutional_gatekeeping, empirical, 'Genuine uncertainty versus institutional gatekeeping as drivers of extraction').

omega_variable(
    adaptive_capacity_development,
    'Can consequence-bearing communities develop sufficient adaptive capacity and countervailing expertise to functionally exit centralized optimization authority?',
    'Longitudinal tracking of community science capabilities, resource allocation to local knowledge systems, measurement of decision-authority deference to community models',
    'If yes: adaptive governance scaffold is structural and sunset is real; constraint moves toward generational transformation. If no: constraint persists across generations and communities remain trapped; consequence-bearers cannot mobilize power even through organization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptive_capacity_development, empirical, 'Viability of adaptive community alternatives to centralized optimization').

omega_variable(
    optimization_legitimacy_mechanism,
    'Does the constraint''s extraction mechanism depend on the decision authority''s continued ability to claim optimization legitimacy through uncertainty rhetoric?',
    'Historical analysis of public trust in optimization-based decisions; correlation between optimization rhetoric use and actual decision reversals; measurement of legitimacy erosion following failed optimization outcomes',
    'If legitimacy is load-bearing: constraint''s extraction relies on performative theater and is vulnerable to explicit exposure. If legitimacy is decoupled from constraint mechanics: extraction persists regardless of whether optimization claims are believed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(optimization_legitimacy_mechanism, conceptual, 'Whether optimization legitimacy rhetoric is necessary to maintain extraction').

omega_variable(
    suppression_mechanism_specificity,
    'Is suppression achieved through active enforcement (institutional gatekeeping, knowledge prohibition) or through structural barriers (cognitive complexity, resource requirements) or internalization (stakeholders accepting their own powerlessness)?',
    'Measurement of suppression decline when access barriers are removed; analysis of stakeholder mobilization when suppression mechanisms fail; comparison of suppression across high-enforcement versus low-enforcement institutional contexts',
    'If primarily active enforcement: suppression can be rapidly reduced through policy change. If primarily structural: barriers persist even when gatekeeping is removed. If internalized: stakeholders carry suppression with them even when institutional barriers fall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_specificity, empirical, 'Nature of suppression mechanism: active, structural, or internalized').

omega_variable(
    model_uncertainty_real_versus_formal,
    'Is the uncertainty driving optimization deferral a genuine property of the systems being optimized or a formal property of the models used to represent them?',
    'Comparison of empirical prediction accuracy across different model structures; analysis of whether uncertainty bounds reflect observational limits or representational choices; examination of uncertainty reduction as models improve',
    'If real: uncertainty is irreducible and optimization under it is a natural constraint. If formal: switching models can reduce apparent uncertainty, revealing the constraint as partly dependent on institutional modeling choices rather than natural limits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(model_uncertainty_real_versus_formal, empirical, 'Whether optimization uncertainty is intrinsic or model-dependent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(optimization_under_uncertainty, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(optunc_tr_t0, optimization_under_uncertainty, theater_ratio, 0, 0.35).
narrative_ontology:measurement(optunc_tr_t5, optimization_under_uncertainty, theater_ratio, 5, 0.48).
narrative_ontology:measurement(optunc_tr_t10, optimization_under_uncertainty, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(optunc_be_t0, optimization_under_uncertainty, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(optunc_be_t5, optimization_under_uncertainty, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(optunc_be_t10, optimization_under_uncertainty, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(optimization_under_uncertainty, resource_allocation).
narrative_ontology:affects_constraint(optimization_under_uncertainty, institutional_decision_deferral).
narrative_ontology:affects_constraint(optimization_under_uncertainty, knowledge_gatekeeping).
narrative_ontology:affects_constraint(optimization_under_uncertainty, accountability_asymmetry).

% DUAL FORMULATION NOTE:
% Optimization under uncertainty is upstream of multiple institutional constraints that depend on it for legitimacy. The constraint family includes separate stories for: (1) formal optimization methods (lower ε, primarily coordination), (2) institutional gatekeeping of optimization authority (higher ε, primarily extraction), (3) deferral of accountability through uncertainty (highest ε, pure extraction). This story covers the hybrid Tangled Rope form. Decomposition reflects ε-invariance: different observables (mathematical optimization vs institutional authority vs accountability deferral) produce different ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(optimization_under_uncertainty, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
