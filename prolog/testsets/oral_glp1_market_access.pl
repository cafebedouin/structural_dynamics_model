% ============================================================================
% CONSTRAINT STORY: oral_glp1_market_access
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-22
% ============================================================================

:- module(constraint_oral_glp1_market_access, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: oral_glp1_market_access
 *   human_readable: Patent-Protected Market for Oral GLP-1 Agonists
 *   domain: economic/technological/healthcare
 *
 * SUMMARY:
 *   This constraint describes the economic and regulatory structure surrounding
 *   the market introduction of a highly effective, patent-protected oral
 *   weight-loss drug (e.g., oral semaglutide). The structure provides a genuine
 *   coordination function by incentivizing R&D and delivering a valuable
 *   therapeutic. Simultaneously, it leverages patent protection to create
 *   high prices and limited access, resulting in significant asymmetric
 *   extraction from patients and healthcare systems.
 *
 * KEY AGENTS (by structural relationship):
 *   - patients_with_obesity: Primary target (powerless/trapped) — bear the direct health and financial costs of restricted access.
 *   - pharmaceutical_manufacturer: Primary beneficiary (institutional/arbitrage) — reaps super-normal profits from patent-protected sales.
 *   - health_insurers: Secondary institutional actor (institutional/constrained) — forced to negotiate coverage and pricing, bearing significant costs.
 *   - regulatory_agencies: Secondary institutional actor (institutional/constrained) — enable the market by approving the drug based on safety and efficacy, largely firewalled from price considerations.
 *   - analytical_observer: Analytical observer — sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(oral_glp1_market_access, 0.55).
domain_priors:suppression_score(oral_glp1_market_access, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(oral_glp1_market_access, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(oral_glp1_market_access, extractiveness, 0.55).
narrative_ontology:constraint_metric(oral_glp1_market_access, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(oral_glp1_market_access, theater_ratio, 0.10).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(oral_glp1_market_access, tangled_rope).
narrative_ontology:human_readable(oral_glp1_market_access, "Patent-Protected Market for Oral GLP-1 Agonists").

% --- Binary flags ---
domain_priors:requires_active_enforcement(oral_glp1_market_access). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(oral_glp1_market_access, pharmaceutical_manufacturer).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(oral_glp1_market_access, patients_with_obesity).
narrative_ontology:constraint_victim(oral_glp1_market_access, health_insurers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (INDIVIDUAL PATIENT)
% A powerless individual faces a life-changing but unaffordable treatment.
% victim + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ.
% χ ≈ 0.55 * 1.42 * 1.0 (national scope) ≈ 0.78. This exceeds the Snare
% threshold (χ ≥ 0.66), so it classifies as a Snare.
constraint_indexing:constraint_classification(oral_glp1_market_access, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (MANUFACTURER)
% The patent holder sees a successful R&D and market coordination mechanism.
% beneficiary + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ.
% χ ≈ 0.55 * -0.12 * 1.2 (global scope) ≈ -0.08. Negative χ classifies as a Rope.
constraint_indexing:constraint_classification(oral_glp1_market_access, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Sees both the coordination and extraction functions clearly.
% Analytical perspective d ≈ 0.72 → f(d) ≈ 1.15.
% χ ≈ 0.55 * 1.15 * 1.2 (global scope) ≈ 0.76. This falls in the Tangled Rope
% range (0.40 ≤ χ ≤ 0.90) and meets the structural requirements.
constraint_indexing:constraint_classification(oral_glp1_market_access, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---

% Perspective 4A: Health Insurers
% An institutional actor forced to participate and bear costs.
% victim + constrained exit → d is moderately high. The system sees them as
% less trapped than an individual, but still a victim.
constraint_indexing:constraint_classification(oral_glp1_market_access, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 4B: Regulatory Agencies (e.g., FDA)
% An institutional actor whose mandate focuses on safety/efficacy, not price.
% From their perspective, the system is coordinating successfully to bring a
% useful product to market. They are neither a direct beneficiary nor victim
% of the pricing structure. This appears as a Rope.
constraint_indexing:constraint_classification(oral_glp1_market_access, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 5: Organized Patient Advocacy Groups
% victim + organized power + constrained exit → d is high but less than trapped.
% χ ≈ 0.55 * f(d=0.75) * 1.0 ≈ 0.66. Right at the Snare/Tangled Rope boundary.
% They see the extraction acutely but have some agency to fight it.
constraint_indexing:constraint_classification(oral_glp1_market_access, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(oral_glp1_market_access_tests).

test(perspectival_gap_target_beneficiary) :-
    constraint_indexing:constraint_classification(oral_glp1_market_access, snare, context(agent_power(powerless), _, trapped, _)),
    constraint_indexing:constraint_classification(oral_glp1_market_access, rope, context(agent_power(institutional), _, arbitrage, _)),
    format('Passed: Snare (powerless) vs Rope (institutional) gap confirmed.~n').

test(inter_institutional_gap) :-
    constraint_indexing:constraint_classification(oral_glp1_market_access, tangled_rope, context(agent_power(institutional), _, constrained, _)),
    constraint_indexing:constraint_classification(oral_glp1_market_access, rope, context(agent_power(institutional), _, arbitrage, _)),
    format('Passed: Inter-institutional gap (constrained vs arbitrage exit) confirmed.~n').

test(analytical_claim_matches) :-
    narrative_ontology:constraint_claim(oral_glp1_market_access, Claim),
    constraint_indexing:constraint_classification(oral_glp1_market_access, Claim, context(agent_power(analytical), _, _, _)),
    format('Passed: Analytical classification matches constraint_claim.~n').

test(tangled_rope_structural_gates) :-
    narrative_ontology:constraint_beneficiary(oral_glp1_market_access, _),
    narrative_ontology:constraint_victim(oral_glp1_market_access, _),
    domain_priors:requires_active_enforcement(oral_glp1_market_access),
    format('Passed: Tangled Rope structural requirements met.~n').


:- end_tests(oral_glp1_market_access_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): High. Represents the significant markup over production cost, enabled by patents. It is not higher because the drug provides substantial, non-theatrical clinical value, which constitutes a genuine coordination function (solving the health problem of obesity).
 *   - Suppression (s=0.65): High. Patent law actively suppresses generic alternatives for a long period, creating a monopoly on this specific, highly convenient (oral) formulation.
 *   - Theater (τ=0.10): Low. The drug's efficacy is well-documented; its function is real, not performative.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For a patient without affordable access (powerless/trapped), the drug is a Snare: a solution dangled before them that is practically unattainable, trapping them in their health condition. For the manufacturer (institutional/arbitrage), it's a perfect Rope: a system that coordinates R&D investment with market rewards, solving the problem of financing innovation.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `pharmaceutical_manufacturer`. The patent system directs immense profits to them, which is the explicit design of the incentive structure.
 *   - Victim: `patients_with_obesity`. They bear the direct cost, either through out-of-pocket expenses or through the health consequences of being unable to afford treatment. `health_insurers` are also victims, as they are forced to pay exorbitant prices, costs which are eventually passed on to all premium-holders.
 *   The system derives directionality (d) from these explicit declarations combined with exit options, correctly calculating high effective extraction (χ) for victims and low/negative χ for the beneficiary.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This case highlights a key inter-institutional tension. The FDA (regulatory_agencies) sees a Rope because its mandate is limited to safety and efficacy. The pricing mechanism is external to its core function. Health insurers, however, are directly exposed to the financial extraction and have constrained exit (they cannot simply stop covering all major drug classes). Their institutional perspective is thus a Tangled Rope, as they must manage both the clinical benefit and the financial cost.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a canonical Tangled Rope, and failing to classify it as such would be a critical error. A simplistic analysis might label it a pure Snare (focusing only on patient cost) or a pure Rope (focusing only on innovation). The Deferential Realism framework, by indexing to different perspectives, reveals the dual nature: it is a system that *simultaneously* coordinates and extracts. This prevents the mislabeling that often occurs in political discourse where one function is used to obscure the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_oral_glp1_market_access,
    'Does the high profit from patent-protected drugs like this spur sufficient future innovation to offset the immediate public health cost of restricted access?',
    'Longitudinal (multi-decade) comparative analysis of health outcomes and R&D pipelines under different intellectual property regimes.',
    'If YES, the system is a justifiable, if harsh, Tangled Rope. If NO, it is a highly inefficient Snare masquerading as a coordination mechanism.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(oral_glp1_market_access, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint (ε=0.55 > 0.46), so temporal data is required.
% This models the extraction increasing dramatically as the drug moves from
% development (T=0) to market launch (T=5) and blockbuster status (T=10).

% Theater ratio over time (remains low because the drug is effective):
narrative_ontology:measurement(oral_glp1_tr_t0, oral_glp1_market_access, theater_ratio, 0, 0.05).
narrative_ontology:measurement(oral_glp1_tr_t5, oral_glp1_market_access, theater_ratio, 5, 0.10).
narrative_ontology:measurement(oral_glp1_tr_t10, oral_glp1_market_access, theater_ratio, 10, 0.10).

% Extraction over time (spikes upon market approval and monopoly pricing):
narrative_ontology:measurement(oral_glp1_ex_t0, oral_glp1_market_access, base_extractiveness, 0, 0.10).
narrative_ontology:measurement(oral_glp1_ex_t5, oral_glp1_market_access, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(oral_glp1_ex_t10, oral_glp1_market_access, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The patent system is a mechanism for allocating societal
% resources (via high prices) towards a specific goal (pharmaceutical R&D).
narrative_ontology:coordination_type(oral_glp1_market_access, resource_allocation).

% Network relationships: This specific constraint is an instance of, and
% interacts with, broader systemic constraints.
narrative_ontology:affects_constraint(us_patent_system, oral_glp1_market_access).
narrative_ontology:affects_constraint(oral_glp1_market_access, public_health_obesity_crisis).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The structural declarations of
% beneficiary/victim, combined with the different exit options for each agent,
% are sufficient for the engine to derive accurate and differentiated
% directionality (d) values for each perspective.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */