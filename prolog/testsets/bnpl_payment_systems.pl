% ============================================================================
% CONSTRAINT STORY: bnpl_payment_systems
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_bnpl_payment_systems, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bnpl_payment_systems
 *   human_readable: "Buy Now, Pay Later" (BNPL) Payment Systems
 *   domain: economic
 *
 * SUMMARY:
 *   "Buy Now, Pay Later" (BNPL) services like Klarna and Clearpay are integrated
 *   into online retail checkouts, offering consumers the ability to split
 *   payments for purchases, often interest-free. While this provides a genuine
 *   coordination function (smoothing consumption), it also creates a powerful
 *   extractive mechanism through late fees, encouragement of over-spending, and
 *   the accumulation of unregulated debt, disproportionately affecting
 *   younger, financially vulnerable consumers.
 *
 * KEY AGENTS (by structural relationship):
 *   - Young Consumers: Primary target (powerless/trapped) — bear extraction via late fees and unmanageable debt.
 *   - BNPL Firms: Primary beneficiary (institutional/arbitrage) — benefit from merchant fees and consumer late fees.
 *   - Online Retailers: Secondary beneficiary (organized/mobile) — benefit from increased sales conversions.
 *   - Financial Regulators: Inter-institutional actor (institutional/constrained) — tasked with managing the system's negative externalities, but facing legislative delays.
 *   - Analytical Observer: Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bnpl_payment_systems, 0.48).
domain_priors:suppression_score(bnpl_payment_systems, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(bnpl_payment_systems, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bnpl_payment_systems, extractiveness, 0.48).
narrative_ontology:constraint_metric(bnpl_payment_systems, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(bnpl_payment_systems, theater_ratio, 0.15).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(bnpl_payment_systems, tangled_rope).
narrative_ontology:topic_domain(bnpl_payment_systems, "economic").
narrative_ontology:human_readable(bnpl_payment_systems, "\"Buy Now, Pay Later\" (BNPL) Payment Systems").

% --- Binary flags ---
domain_priors:requires_active_enforcement(bnpl_payment_systems). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(bnpl_payment_systems, bnpl_firms).
narrative_ontology:constraint_beneficiary(bnpl_payment_systems, online_retailers).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(bnpl_payment_systems, young_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function.
   The engine derives d from beneficiary/victim membership + exit_options.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Young consumers who are drawn into debt cycles. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% Calculation: χ = 0.48 * f(0.95) * σ(national) ≈ 0.48 * 1.42 * 1.0 ≈ 0.68
% This meets the Snare criteria (χ ≥ 0.66, ε ≥ 0.46, suppression ≥ 0.60).
constraint_indexing:constraint_classification(bnpl_payment_systems, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% BNPL firms who profit from the system. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
% Calculation: χ = 0.48 * f(0.05) * σ(global) ≈ 0.48 * -0.12 * 1.2 ≈ -0.07
% From their perspective, it's a highly efficient coordination mechanism.
constraint_indexing:constraint_classification(bnpl_payment_systems, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context. Recognizes both the coordination and extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15.
% Calculation: χ = 0.48 * f(0.72) * σ(global) ≈ 0.48 * 1.15 * 1.2 ≈ 0.66
% Given ε=0.48, sup=0.65, and the presence of both beneficiaries and victims,
% this classifies as a Tangled Rope.
constraint_indexing:constraint_classification(bnpl_payment_systems, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---

% Perspective 4A: Financial Regulator (e.g., FCA)
% Views the system as a coordination mechanism needing management, but is
% constrained by political and legislative processes.
% Exit is constrained, d is higher than a pure beneficiary but not a victim.
% Derivation: (institutional, constrained, not beneficiary/victim) -> fallback
% to canonical d for institutional (0.0). But their 'constrained' exit
% suggests a higher d. Let's model their goal as pure coordination.
constraint_indexing:constraint_classification(bnpl_payment_systems, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 4B: Online Retailer
% A direct beneficiary, but with more exit options than the BNPL firms (can
% switch providers). Mobile exit + beneficiary status → d ≈ 0.15 → f(d) ≈ -0.01.
% Calculation: χ = 0.48 * f(0.15) * σ(national) ≈ 0.48 * -0.01 * 1.0 ≈ -0.005.
% A clear Rope.
constraint_indexing:constraint_classification(bnpl_payment_systems, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bnpl_payment_systems_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify the core perspectival gap between the consumer and the BNPL firm.
    constraint_indexing:constraint_classification(bnpl_payment_systems, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(bnpl_payment_systems, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    true.

test(tangled_rope_conditions_met) :-
    % Verify that the structural conditions for a Tangled Rope are present.
    narrative_ontology:constraint_claim(bnpl_payment_systems, tangled_rope),
    narrative_ontology:constraint_beneficiary(bnpl_payment_systems, _),
    narrative_ontology:constraint_victim(bnpl_payment_systems, _),
    domain_priors:requires_active_enforcement(bnpl_payment_systems),
    domain_priors:base_extractiveness(bnpl_payment_systems, E), E >= 0.30,
    domain_priors:suppression_score(bnpl_payment_systems, S), S >= 0.40.

:- end_tests(bnpl_payment_systems_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): Set high to reflect the significant financial harm caused by late fees and encouraged over-spending, which goes beyond simple transaction costs. This is not a low-friction system.
 *   - Suppression (0.65): High due to the aggressive integration at online checkouts and marketing that presents BNPL as a default, "smarter" payment method, suppressing alternatives like saving or using regulated credit.
 *   - Enforcement: Required because the business model depends on collecting payments, charging late fees, and reporting to credit agencies.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For a young consumer trapped by debt (powerless/trapped), the system is a Snare. The initial convenience gives way to coercive extraction with high exit costs (damaged credit, persistent debt). For the BNPL firm (institutional/arbitrage), it is a Rope. They see a valuable coordination service (connecting merchants and consumers) that generates revenue. The negative externalities are, from their perspective, a manageable cost of doing business, not a core feature.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `bnpl_firms` and `online_retailers` directly profit from the system's existence and adoption. This declaration drives their derived directionality `d` towards 0, resulting in a low or negative effective extraction (χ) and a Rope classification.
 *   - Victims: `young_consumers` bear the costs of the system's failure modes (late fees, debt). This declaration drives their derived directionality `d` towards 1.0, resulting in a high χ and a Snare classification.
 *   This mapping of structural roles to directionality is the core of the model.
 *
 * MANDATROPHY ANALYSIS:
 *   This model avoids mislabeling BNPL as either pure coordination (a simple Rope) or pure predation (a simple Snare). The Tangled Rope classification from the analytical perspective correctly identifies its dual nature: it solves a real coordination problem (payment smoothing) but does so via a mechanism with significant, asymmetric extractive properties. A purely economic analysis might miss the coercive aspect, while a purely consumer-advocacy view might miss the genuine coordination function that drives its adoption. The indexical classification captures both realities simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_bnpl_payment_systems,
    'Is the profitability of BNPL services primarily driven by merchant-side coordination fees or by consumer-side extraction (late fees, encouraged over-consumption)?',
    'Mandatory public disclosure and regulatory analysis of BNPL firms'' detailed revenue breakdowns.',
    'If primarily merchant-driven, the constraint is a stable Tangled Rope. If primarily consumer-extraction-driven, it is functionally a Snare masquerading as a Tangled Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(bnpl_payment_systems, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This models the evolution of BNPL from a niche convenience tool to a
% systemically important, highly extractive financial product. This is a
% classic case of extraction_accumulation drift.

% Theater ratio over time (stable and low):
narrative_ontology:measurement(bnpl_tr_t0, bnpl_payment_systems, theater_ratio, 0, 0.10).
narrative_ontology:measurement(bnpl_tr_t5, bnpl_payment_systems, theater_ratio, 5, 0.12).
narrative_ontology:measurement(bnpl_tr_t10, bnpl_payment_systems, theater_ratio, 10, 0.15).

% Extraction over time (shows significant accumulation):
narrative_ontology:measurement(bnpl_ex_t0, bnpl_payment_systems, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(bnpl_ex_t5, bnpl_payment_systems, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(bnpl_ex_t10, bnpl_payment_systems, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: It is fundamentally a system for allocating credit and
% managing payment flows.
narrative_ontology:coordination_type(bnpl_payment_systems, resource_allocation).

% Network relationships: The rise of BNPL directly impacts the established
% consumer credit regulatory framework by operating outside of it.
narrative_ontology:affects_constraint(bnpl_payment_systems, consumer_credit_regulation).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation from
% beneficiary/victim declarations and exit options accurately models the
% directionality for the key agents involved.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */