% ============================================================================
% CONSTRAINT STORY: fda_component_efficacy_standard
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-22
% ============================================================================

:- module(constraint_fda_component_efficacy_standard, []).

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
 *   constraint_id: fda_component_efficacy_standard
 *   human_readable: FDA's Component-Level Efficacy Standard for Combination Vaccines
 *   domain: technological/political
 *
 * SUMMARY:
 *   The U.S. Food and Drug Administration (FDA) requires that for a combination
 *   vaccine (e.g., a single shot for COVID, flu, and RSV), manufacturers must
 *   demonstrate the efficacy of each individual component. This is a higher
 *   evidentiary bar than simply showing the combination shot prevents a general
 *   "respiratory illness" clinical endpoint. This constraint imposes significant
 *   additional trial complexity, cost, and time on developers, but ensures that
 *   no ineffective component is "carried" by the others.
 *
 * KEY AGENTS (by structural relationship):
 *   - Combination Vaccine Developers (e.g., Moderna): Primary target (institutional/constrained) — bears the high cost of complex trials.
 *   - US Public Health System (represented by FDA): Primary beneficiary (institutional/arbitrage) — benefits from higher, more reliable evidence standards.
 *   - Small Biotech Startups: Secondary target (powerless/trapped) — may be completely blocked from the market by the high R&D costs.
 *   - Analytical Observer: Sees the trade-off between innovation speed and evidentiary rigor.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fda_component_efficacy_standard, 0.55). % High cost in time, money, and trial complexity.
domain_priors:suppression_score(fda_component_efficacy_standard, 0.75).   % Strongly suppresses simpler/faster trial designs. FDA approval is a hard gate.
domain_priors:theater_ratio(fda_component_efficacy_standard, 0.10).       % A genuine scientific/evidentiary requirement, not performative.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fda_component_efficacy_standard, extractiveness, 0.55).
narrative_ontology:constraint_metric(fda_component_efficacy_standard, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(fda_component_efficacy_standard, theater_ratio, 0.10).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A. This is a constructed, not natural, constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(fda_component_efficacy_standard, tangled_rope).
narrative_ontology:human_readable(fda_component_efficacy_standard, "FDA's Component-Level Efficacy Standard for Combination Vaccines").

% --- Binary flags ---
domain_priors:requires_active_enforcement(fda_component_efficacy_standard). % Required for Tangled Rope. Enforced by FDA review process.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(fda_component_efficacy_standard, us_public_health_system).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(fda_component_efficacy_standard, combination_vaccine_developers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SMALL STARTUP)
% A small, powerless developer with one product is trapped. The high cost of
% trials is an existential threat. The engine derives a high d (~0.95), leading
% to very high effective extraction (χ), classifying this as a Snare.
constraint_indexing:constraint_classification(fda_component_efficacy_standard, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (FDA / PUBLIC HEALTH)
% The FDA acts as an institutional beneficiary with policy arbitrage. The engine
% derives a very low d (~0.05), leading to negative effective extraction (χ).
% From this view, the standard is a pure coordination mechanism (Rope) that
% generates public good.
constraint_indexing:constraint_classification(fda_component_efficacy_standard, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Sees both the valid coordination function (ensuring vaccine quality) and the
% heavy asymmetric extraction (high costs on developers). The high ε and
% suppression, combined with the presence of both beneficiaries and victims,
% lead to a Tangled Rope classification.
constraint_indexing:constraint_classification(fda_component_efficacy_standard, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---
% This captures the dynamic between the regulator and a large, regulated firm
% like Moderna. Both are institutional, but have different structural relationships.

% PERSPECTIVE 4: THE REGULATED INSTITUTION (MODERNA)
% As a victim with institutional power but constrained exit options (they cannot
% sell in the US without approval), the engine derives a moderately high d.
% The resulting χ is high enough to be extractive, but not a pure snare,
% recognizing both the coordination and extraction elements. A classic Tangled Rope.
constraint_indexing:constraint_classification(fda_component_efficacy_standard, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fda_component_efficacy_standard_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify perspectival gap between a trapped target and the beneficiary.
    constraint_indexing:constraint_classification(fda_component_efficacy_standard, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(fda_component_efficacy_standard, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    true.

test(analytical_view_is_tangled_rope) :-
    % The analytical view must identify the hybrid nature of the constraint.
    constraint_indexing:constraint_classification(fda_component_efficacy_standard, tangled_rope, context(agent_power(analytical), _, _, _)).

test(inter_institutional_gap) :-
    % Verify the gap between two institutional actors with different exit options.
    constraint_indexing:constraint_classification(fda_component_efficacy_standard, TypeRegulator, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(fda_component_efficacy_standard, TypeRegulated, context(agent_power(institutional), _, exit_options(constrained), _)),
    TypeRegulator = rope,
    TypeRegulated = tangled_rope.

:- end_tests(fda_component_efficacy_standard_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): Set high to reflect the immense cost, time,
 *     and complexity of running separate efficacy analyses for each component
 *     of a combination vaccine. This isn't a paperwork hurdle; it's a
 *     fundamental driver of R&D strategy and cost.
 *   - Suppression (0.75): The FDA's authority is nearly absolute for market
 *     access in the US. This standard effectively suppresses alternative,
 *     faster trial methodologies based on broader clinical endpoints.
 *   - The combination of a clear beneficiary (public health), a clear victim
 *     (developers), and active enforcement makes this a textbook Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the public health system (via the FDA), this is
 *   a Rope—a pure coordination rule that creates certainty and trust. For a
 *   small startup, it's a Snare—an impossibly high barrier to entry that can
 *   kill the company. This huge gap stems from who bears the cost of generating
 *   certainty. The constraint socializes the benefit (safer, more reliable
 *   vaccines) while privatizing the cost (on the developer).
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `us_public_health_system`. The rule coordinates the market
 *     towards a higher standard of evidence, benefiting public trust and safety.
 *   - Victim: `combination_vaccine_developers`. They directly bear the financial
 *     and temporal costs of the higher evidentiary bar.
 *   The directionality derivation engine correctly assigns low `d` to the
 *   beneficiary and high `d` to the victims, driving the perspectival split.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The FDA-Moderna dynamic is key. Both are `institutional` actors, but their
 *   `exit_options` differ critically. The FDA has `arbitrage` (it sets the
 *   rules), while Moderna is `constrained` (it must follow the rules to
 *   access the market). This structural difference is why the FDA sees a Rope,
 *   while even a powerful company like Moderna experiences it as a Tangled
 *   Rope, acknowledging the coordination function but feeling the direct sting
 *   of the extraction. No directionality override is needed because the
 *   structural data (victim + constrained exit) is sufficient for the engine.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the dual nature of the regulation.
 *   A simplistic analysis might call it a Snare (focusing only on Moderna's
 *   costs) or a Rope (focusing only on the public good). Deferential Realism,
 *   by indexing to perspective and identifying the structure as a Tangled Rope,
 *   shows that it is simultaneously a valid coordination mechanism AND a highly
 *   extractive one. The core policy debate is not *if* it extracts, but whether
 *   the *amount* of extraction is justified by the coordination benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_fda_component_efficacy_standard,
    'Is the component-level efficacy standard the minimum necessary for safety, or an overly burdensome requirement that stifles innovation?',
    'Comparative analysis of clinical outcomes and innovation rates in jurisdictions with different standards (e.g., US FDA vs. Europe EMA) over a 10-year period.',
    'If necessary -> Tangled Rope (as classified). If overly burdensome -> A more extractive Tangled Rope, bordering on a Snare from an analytical perspective.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fda_component_efficacy_standard, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This models the standard evolving from a guideline to a strictly enforced
% rule, especially with the advent of complex mRNA combination vaccines.
% Base extraction increases as the rule becomes more rigid and costly to meet.
% Theater remains low throughout, as it's always been a technical requirement.

% Theater ratio over time (stable):
narrative_ontology:measurement(fces_tr_t0, fda_component_efficacy_standard, theater_ratio, 0, 0.10).
narrative_ontology:measurement(fces_tr_t5, fda_component_efficacy_standard, theater_ratio, 5, 0.10).
narrative_ontology:measurement(fces_tr_t10, fda_component_efficacy_standard, theater_ratio, 10, 0.10).

% Extraction over time (increasing):
narrative_ontology:measurement(fces_ex_t0, fda_component_efficacy_standard, base_extractiveness, 0, 0.35). % Initial state as a guideline.
narrative_ontology:measurement(fces_ex_t5, fda_component_efficacy_standard, base_extractiveness, 5, 0.45). % Codified as a formal policy.
narrative_ontology:measurement(fces_ex_t10, fda_component_efficacy_standard, base_extractiveness, 10, 0.55).% Strictly applied to new platforms.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The constraint's primary function is to create a reliable
% standard for scientific information.
narrative_ontology:coordination_type(fda_component_efficacy_standard, information_standard).

% Network relationships: This standard directly impacts the timeline and cost
% of vaccine development, which in turn affects other areas of the healthcare system.
narrative_ontology:affects_constraint(fda_component_efficacy_standard, vaccine_development_timeline).
narrative_ontology:affects_constraint(fda_component_efficacy_standard, pharmaceutical_rd_investment_models).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation chain
% (using beneficiary/victim declarations and exit options) accurately models
% the different directionalities experienced by the FDA (beneficiary, arbitrage)
% and Moderna (victim, constrained), producing the correct perspectival gap
% between the two institutional actors.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */