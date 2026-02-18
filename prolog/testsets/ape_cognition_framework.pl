% ============================================================================
% CONSTRAINT STORY: ape_cognition_framework
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-22
% ============================================================================

:- module(constraint_ape_cognition_framework, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ape_cognition_framework
 *   human_readable: The scientific and ethical framework defining the limits of ape cognition.
 *   domain: social/scientific
 *
 * SUMMARY:
 *   This constraint is not the biological limit on ape intelligence itself (which
 *   would be a Mountain), but the human-constructed scientific and legal
 *   framework used to define, measure, and ultimately limit it. This framework
 *   has historically characterized non-human great apes as lacking complex
 *   inner lives, imagination, or pretense, thereby justifying their use in
 *   extractive research and entertainment. As suggested by the source article,
 *   emerging research challenges this paradigm, revealing it as a Tangled Rope
 *   that both coordinates scientific activity and enables severe extraction.
 *
 * KEY AGENTS (by structural relationship):
 *   - Non-human great apes: Primary target (powerless/trapped) — bear the full cost of the framework through confinement and experimentation.
 *   - Research & entertainment industries: Primary beneficiary (institutional/arbitrage) — use the framework as legal and ethical cover for their operations.
 *   - Established scientific community: Secondary beneficiary (organized/constrained) — uses the framework as a stable paradigm (Kuhnian "normal science") for research coordination.
 *   - Analytical observer: Sees the dual function of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ape_cognition_framework, 0.55).
domain_priors:suppression_score(ape_cognition_framework, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(ape_cognition_framework, 0.40).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ape_cognition_framework, extractiveness, 0.55).
narrative_ontology:constraint_metric(ape_cognition_framework, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ape_cognition_framework, theater_ratio, 0.40).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(ape_cognition_framework, tangled_rope).
narrative_ontology:human_readable(ape_cognition_framework, "The scientific and ethical framework defining the limits of ape cognition.").
narrative_ontology:topic_domain(ape_cognition_framework, "social/scientific").

% --- Binary flags ---
domain_priors:requires_active_enforcement(ape_cognition_framework). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(ape_cognition_framework, research_and_entertainment_industries).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(ape_cognition_framework, non_human_great_apes).

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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership (non_human_great_apes) + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(ape_cognition_framework, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership (research_and_entertainment_industries) + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
constraint_indexing:constraint_classification(ape_cognition_framework, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context. Sees both coordination and extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for this perspective.
constraint_indexing:constraint_classification(ape_cognition_framework, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ape_cognition_framework_tests).

test(perspectival_gap_is_snare_vs_rope) :-
    % Verify the core perspectival gap between the powerless target and institutional beneficiary.
    constraint_indexing:constraint_classification(ape_cognition_framework, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(ape_cognition_framework, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_tangled_rope) :-
    % Verify the analytical observer correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(ape_cognition_framework, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    % Verify that all three structural requirements for a Tangled Rope are declared.
    narrative_ontology:constraint_beneficiary(ape_cognition_framework, _),
    narrative_ontology:constraint_victim(ape_cognition_framework, _),
    domain_priors:requires_active_enforcement(ape_cognition_framework).

:- end_tests(ape_cognition_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): High. The framework enables the extraction of immense value (scientific data, entertainment profit, pharmaceutical testing results) from the lives and bodies of apes.
 *   - Suppression (s=0.75): High. The dominant paradigm is maintained through institutional inertia, peer review, and funding structures that marginalize research into animal personhood or more complex cognitive models. Alternative frameworks are suppressed. Suppression is a raw structural score, not scaled by perspective.
 *   - Theater (t=0.40): Moderate. The existence of animal welfare committees and ethical review boards creates a performance of care, but these often operate within the extractive framework rather than challenging its premises.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the research institutions (beneficiaries), the framework is a pure coordination device (Rope) that standardizes methods and provides legal clarity, making their work possible. For the apes (victims), the framework is a pure instrument of coercion and extraction (Snare) that completely defines and constrains their existence with no benefit. They cannot form coalitions to resist, making them perpetually powerless. The analytical observer sees both functions simultaneously, classifying it correctly as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived directly from the structural relationships. The 'non_human_great_apes' are declared victims with trapped exit, leading to a high directionality (d≈0.95) and thus high effective extraction (χ). The 'research_and_entertainment_industries' are declared beneficiaries with arbitrage exit (e.g., moving research to countries with looser regulations), leading to a very low, even negative, directionality (d≈0.05) and negative χ. This accurately models how the same constraint functions as a subsidy for one group and a tax for another.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two errors. It doesn't mistake the framework for a pure coordination tool (Rope), which would ignore the immense extraction from the apes. It also doesn't mistake it for a pure coercive tool (Snare), which would ignore its genuine function in coordinating scientific research. The Tangled Rope classification captures this duality, which is essential for understanding why the framework is so resilient: it serves a real purpose for a powerful group, which defends it despite the harm it causes to a powerless one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_ape_cognition_framework,
    'Is the observed cognitive limit in apes an inherent biological reality (a Mountain), or is it an artifact of the impoverished, stressful environments humans impose (making this framework a Snare)?',
    'Long-term, multi-generational studies of great apes in enriched, semi-wild sanctuaries with access to advanced, non-coercive communication interfaces.',
    'If the limit is biological, the framework is a flawed map of a real territory (Tangled Rope). If the limit is environmental, the framework is a cage that creates the reality it purports to describe (pure Snare).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(ape_cognition_framework, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint has evolved over time. In the mid-20th century, extraction was
% higher and theater was lower. Public pressure and internal ethical shifts
% have led to a decrease in raw extraction and an increase in performative
% welfare measures (theater). Base extractiveness > 0.46, so this is required.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(ape_cognition_framework_tr_t0, ape_cognition_framework, theater_ratio, 0, 0.10).
narrative_ontology:measurement(ape_cognition_framework_tr_t5, ape_cognition_framework, theater_ratio, 5, 0.30).
narrative_ontology:measurement(ape_cognition_framework_tr_t10, ape_cognition_framework, theater_ratio, 10, 0.40).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(ape_cognition_framework_ex_t0, ape_cognition_framework, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(ape_cognition_framework_ex_t5, ape_cognition_framework, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(ape_cognition_framework_ex_t10, ape_cognition_framework, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The framework acts as a standard for what constitutes valid knowledge and
% ethical practice in the field.
narrative_ontology:coordination_type(ape_cognition_framework, information_standard).

% Network relationships (structural influence edges)
% This framework has direct structural implications for legal and regulatory constraints.
narrative_ontology:affects_constraint(ape_cognition_framework, animal_personhood_legal_status).
narrative_ontology:affects_constraint(ape_cognition_framework, pharmaceutical_testing_protocols).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The automatic derivation chain,
% based on the declared beneficiary/victim groups and their respective exit
% options, accurately models the directionality for all key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */