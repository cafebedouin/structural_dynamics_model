% ============================================================================
% CONSTRAINT STORY: constitutional_supremacy
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_constitutional_supremacy, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constitutional_supremacy
 *   human_readable: The Supremacy of Written Constitutions and Judicial Review
 *   domain: legal/political
 *
 * SUMMARY:
 *   Established in Marbury v. Madison (1803), this constraint posits that a
 *   written constitution is the "paramount law of the nation," and any legislative
 *   act repugnant to it is void. This principle establishes judicial review,
 *   creating a legal hierarchy where the judiciary must prioritize the
 *   Constitution over legislative statutes, effectively extracting final
 *   sovereignty from the legislative branch and vesting it in the judicial.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Legislature (powerful/constrained): Primary target — its legislative sovereignty is extracted.
 *   - The Citizen (powerless/trapped): Secondary target — bound by the law, but also a beneficiary of constitutional rights.
 *   - The Judiciary (institutional/arbitrage): Primary beneficiary — its authority is defined and elevated.
 *   - The Analytical Observer (analytical/analytical): Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_supremacy, 0.53).
domain_priors:suppression_score(constitutional_supremacy, 0.95).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(constitutional_supremacy, 0.1).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_supremacy, extractiveness, 0.53).
narrative_ontology:constraint_metric(constitutional_supremacy, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(constitutional_supremacy, theater_ratio, 0.1).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(constitutional_supremacy, tangled_rope).
narrative_ontology:human_readable(constitutional_supremacy, "The Supremacy of Written Constitutions and Judicial Review").

% --- Binary flags ---
domain_priors:requires_active_enforcement(constitutional_supremacy). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(constitutional_supremacy, the_judiciary).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(constitutional_supremacy, the_legislature).
narrative_ontology:constraint_victim(constitutional_supremacy, the_citizen).

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

% PERSPECTIVE 1: THE CITIZEN (SNARE)
% Agent who is subject to the law. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% The citizen is trapped within a system that both coordinates their rights
% and extracts from them via the state's authority, which this system legitimizes.
constraint_indexing:constraint_classification(constitutional_supremacy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE JUDICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
% For the judiciary, this is a pure coordination tool that defines its role and
% power to interpret law and maintain governmental structure.
constraint_indexing:constraint_classification(constitutional_supremacy, rope,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE LEGISLATURE (SNARE)
% The primary target of extraction. Engine derives d from:
%   victim membership + powerful/constrained status -> high d -> high χ
% From the perspective of a legislature whose statute is struck down, judicial
% review is a snare that coercively extracts its legislative sovereignty.
constraint_indexing:constraint_classification(constitutional_supremacy, snare,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context. Sees both the coordination function (stabilizing
% the legal system) and the asymmetric extraction (of sovereignty from the
% legislature).
constraint_indexing:constraint_classification(constitutional_supremacy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_supremacy_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(constitutional_supremacy, TypeTarget, context(agent_power(powerful), _, _, _)),
    constraint_indexing:constraint_classification(constitutional_supremacy, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary,
    TypeTarget == snare,
    TypeBeneficiary == rope.

test(analytical_classification_matches_claim) :-
    narrative_ontology:constraint_claim(constitutional_supremacy, ClaimedType),
    constraint_indexing:constraint_classification(constitutional_supremacy, AnalyticalType, context(agent_power(analytical), _, _, _)),
    ClaimedType == AnalyticalType.

:- end_tests(constitutional_supremacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.53) is set high to reflect the significant
 *   extraction of legislative sovereignty, which is the core function of
 *   judicial review. This is not a financial extraction, but an extraction of
 *   power and finality. The suppression score (0.95) reflects Marshall's
 *   argument that there is no middle ground: the Constitution is either
 *   paramount or it is meaningless. The theater ratio is low (0.1) as this
 *   is a highly functional, non-performative constraint.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the Judiciary (beneficiary), it's a Rope—a tool for
 *   coordination and fulfilling their duty. For the Legislature (victim), it's
 *   a Snare that invalidates their will and extracts their power. For the
 *   Citizen, it is also a Snare, as they are trapped within this power structure,
 *   even while benefiting from the rights it coordinates. The analytical view
 *   sees both functions, classifying it as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: The Judiciary's power is created and defined by this constraint.
 *   - Victims: The Legislature loses its claim to ultimate sovereignty. The Citizen
 *     is also a victim in the structural sense of being subject to this power
 *     hierarchy, unable to exit it.
 *
 * MANDATROPHY ANALYSIS:
 *   The high base extraction and suppression scores correctly identify the
 *   coercive nature of this constraint. By classifying it as a Tangled Rope
 *   analytically, the framework acknowledges its genuine coordination function
 *   (stabilizing the rule of law) while refusing to ignore the asymmetric
 *   extraction of power that underpins it. This prevents mislabeling a
 *   foundational power seizure as pure coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
omega_variable(
    omega_judicial_neutrality,
    "Is the Court a neutral arbiter of the Constitution, or does judicial review inherently transform into a tool of judicial policy-making, becoming a snare for democratic self-governance?",
    "Requires longitudinal analysis of case law to determine if review is used primarily for constitutional coordination versus partisan extraction (legislating from the bench).",
    "If neutral, the constraint functions as a stabilizing Tangled Rope. If partisan, it degrades into a pure Snare against political opponents.",
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(omega_judicial_neutrality, empirical, "Whether judicial review is applied neutrally or as a partisan tool.").

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(constitutional_supremacy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Required for high-extraction constraints (base_extractiveness > 0.46).
% This constraint has been remarkably stable since its inception.
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(cs_tr_t0, constitutional_supremacy, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cs_tr_t5, constitutional_supremacy, theater_ratio, 5, 0.1).
narrative_ontology:measurement(cs_tr_t10, constitutional_supremacy, theater_ratio, 10, 0.1).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(cs_ex_t0, constitutional_supremacy, base_extractiveness, 0, 0.53).
narrative_ontology:measurement(cs_ex_t5, constitutional_supremacy, base_extractiveness, 5, 0.53).
narrative_ontology:measurement(cs_ex_t10, constitutional_supremacy, base_extractiveness, 10, 0.53).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This constraint is a fundamental mechanism for enforcing the legal hierarchy.
narrative_ontology:coordination_type(constitutional_supremacy, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% status and exit options accurately models the power dynamics.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */