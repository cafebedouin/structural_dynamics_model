% ============================================================================
% CONSTRAINT STORY: armra_colostrum_regulation
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-27
% ============================================================================

:- module(constraint_armra_colostrum_regulation, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: armra_colostrum_regulation
 *   human_readable: Regulatory Oversight of ARMRA Colostrum Supplement Claims
 *   domain: economic
 *
 * SUMMARY:
 *   The constraint concerns the regulatory landscape surrounding ARMRA, a colostrum supplement company, and the veracity of its health claims. The constraint focuses on the balance between allowing innovation in the supplement market and ensuring consumer protection against misleading or unsubstantiated claims, potentially requiring regulatory intervention to ensure claims are properly validated and do not exploit consumer vulnerabilities. This is further complicated by ARMRA directly contacting their customers to lobby against potential FDA regulation.
 *
 * KEY AGENTS (by structural relationship):
 *   - Consumers: Primary target (powerless/trapped) — bears extraction (financial and health risks) from misleading claims.
 *   - ARMRA: Primary beneficiary (powerful/arbitrage) — benefits from sales and brand recognition.
 *   - Regulatory Agencies (e.g., FDA): Secondary actor (institutional/constrained) — faces resource constraints and political pressures in regulating the supplement market.
 *   - Analytical Observer: Analytical observer (analytical/analytical) — assesses the overall impact of regulatory policies.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(armra_colostrum_regulation, 0.48).  % Moderate extraction due to potential health and financial risks from misleading claims.
domain_priors:suppression_score(armra_colostrum_regulation, 0.55).    % Moderate suppression, as consumers may have limited alternatives and access to reliable information.
domain_priors:theater_ratio(armra_colostrum_regulation, 0.30).        % Low theater ratio, as the primary function involves genuine health claims and real product sales, with minimal performative maintenance so far.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(armra_colostrum_regulation, extractiveness, 0.48).
narrative_ontology:constraint_metric(armra_colostrum_regulation, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(armra_colostrum_regulation, theater_ratio, 0.30).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(armra_colostrum_regulation, tangled_rope).
narrative_ontology:human_readable(armra_colostrum_regulation, "Regulatory Oversight of ARMRA Colostrum Supplement Claims").
narrative_ontology:topic_domain(armra_colostrum_regulation, "economic").

% --- Binary flags ---
domain_priors:requires_active_enforcement(armra_colostrum_regulation). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(armra_colostrum_regulation, armra).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(armra_colostrum_regulation, consumers_of_supplements).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three)
%   Scaffold:     beneficiary + (has_sunset_clause OR no enforcement)
%   Snare:        victim required; beneficiary optional

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
%
% NOTE: Per "Dynamic Coalition" extension, this agent's power may be
% upgraded to 'organized' if the constraint is a snare with a critical
% mass of victims, potentially changing the classification.
constraint_indexing:constraint_classification(armra_colostrum_regulation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(armra_colostrum_regulation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(armra_colostrum_regulation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES (declare when applicable) ---
% When a constraint operates between institutional actors with different
% structural relationships, declare separate perspectives for each.
% The engine differentiates via directionality: different exit_options
% produce different d values even for the same power atom.

% Perspective 4A: Regulatory Agencies (institutional, constrained exit)
constraint_indexing:constraint_classification(armra_colostrum_regulation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(armra_colostrum_regulation_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(armra_colostrum_regulation, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(armra_colostrum_regulation, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(armra_colostrum_regulation, ExtMetricName, E),
    (E =< 0.25 -> true ; E >= 0.46). % Mountain or high-extraction Snare/Tangled.

:- end_tests(armra_colostrum_regulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is set at 0.48, reflecting the potential financial and health risks consumers face when purchasing ARMRA products based on unsubstantiated claims. Suppression is at 0.55, as consumers have limited access to reliable information and alternative products, making them vulnerable. The theater ratio is low at 0.30, as the primary function of ARMRA involves real product sales and health claims, with minimal performative maintenance.
 *
 * PERSPECTIVAL GAP:
 *   Consumers, who are the primary target, perceive the situation as a Snare because they face potential financial losses and health risks due to misleading claims. ARMRA, on the other hand, perceives the situation as a Rope, as they benefit from sales and brand recognition with minimal perceived downsides.
 *
 * DIRECTIONALITY LOGIC:
 *   ARMRA benefits by selling its product based on claims that may not be fully substantiated, leading to increased sales and market presence. Consumers bear the costs in the form of financial risks from purchasing overpriced supplements and potential health risks from relying on unverified claims. This dynamic necessitates regulatory enforcement to balance market innovation with consumer protection.
 *
 * INTER-INSTITUTIONAL DYNAMICS (if applicable):
 *   Regulatory agencies like the FDA operate under resource constraints and political pressures, influencing their ability to effectively regulate the supplement market. This dynamic creates a tension between allowing companies to innovate and ensuring consumer protection against misleading claims. The FDA perspective is classified as a rope due to the inherent coordination required to regulate industry while still allowing for innovation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling the situation as pure extraction (Snare) by acknowledging the genuine coordination function of companies like ARMRA to innovate and provide health products. However, it also recognizes the asymmetric extraction involved in the lack of strict regulatory oversight.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_armra_regulation,
    'What is the true efficacy of ARMRA\'s colostrum supplement?',
    'Independent scientific studies with rigorous methodology.',
    'If True: Reduced need for strict regulation; If False: Increased need for strict regulation.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
% The reporting engine reads narrative_ontology:omega_variable/3 with structure
% (ID, TypeClass, Description) where TypeClass is one of:
%   empirical   — resolvable by gathering more data
%   conceptual  — depends on definitional or theoretical framing
%   preference  — depends on value judgments or policy choices
% The /3 form is what the engine reads; /5 provides narrative context.
narrative_ontology:omega_variable(omega_armra_regulation, empirical, 'Uncertainty over the true efficacy of the colostrum supplement, resolvable by independent studies.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(armra_colostrum_regulation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
%
% Required for high-extraction constraints (base_extractiveness > 0.46).
% Use at least 3 time points (T=0, midpoint, T=end) for each tracked metric.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(armra_colostrum_regulation_tr_t0, armra_colostrum_regulation, theater_ratio, 0, 0.20).
narrative_ontology:measurement(armra_colostrum_regulation_tr_t5, armra_colostrum_regulation, theater_ratio, 5, 0.25).
narrative_ontology:measurement(armra_colostrum_regulation_tr_t10, armra_colostrum_regulation, theater_ratio, 10, 0.30).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(armra_colostrum_regulation_ex_t0, armra_colostrum_regulation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(armra_colostrum_regulation_ex_t5, armra_colostrum_regulation, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(armra_colostrum_regulation_ex_t10, armra_colostrum_regulation, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(armra_colostrum_regulation, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% This constraint does not require directionality overrides as the structural
% derivation from beneficiary/victim groups and exit options accurately
% models the relationships between ARMRA, consumers, and regulators.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */