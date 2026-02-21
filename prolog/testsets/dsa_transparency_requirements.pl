% ============================================================================
% CONSTRAINT STORY: dsa_transparency_requirements
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-24
% ============================================================================

:- module(constraint_dsa_transparency_requirements, []).

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
    domain_priors:emerges_naturally/1.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dsa_transparency_requirements
 *   human_readable: EU Digital Services Act (DSA) Transparency Requirements
 *   domain: political/technological
 *
 * SUMMARY:
 *   The European Union's Digital Services Act (DSA) imposes strict
 *   transparency obligations on Very Large Online Platforms (VLOPs). This
 *   includes disclosing how their recommendation algorithms and content
 *   moderation systems work. This constraint models the coercive and
 *   coordinative aspects of these rules, exemplified by the EU fining platform
 *   X for non-compliance.
 *
 * KEY AGENTS (by structural relationship):
 *   - Very Large Online Platforms (e.g., X): Primary target (institutional/constrained) — bears the costs of compliance and penalties for failure.
 *   - EU Citizens and Researchers: Primary beneficiary (represented by institutional/arbitrage) — benefits from increased platform accountability and data access.
 *   - European Commission: Enforcing institution (institutional/arbitrage) — applies the constraint to achieve policy goals.
 *   - Analytical Observer: Systems analyst (analytical/analytical) — sees both the coordination and extraction functions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsa_transparency_requirements, 0.55).
domain_priors:suppression_score(dsa_transparency_requirements, 0.80).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(dsa_transparency_requirements, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsa_transparency_requirements, extractiveness, 0.55).
narrative_ontology:constraint_metric(dsa_transparency_requirements, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(dsa_transparency_requirements, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(dsa_transparency_requirements, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(dsa_transparency_requirements). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(dsa_transparency_requirements, eu_citizens_and_researchers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(dsa_transparency_requirements, vloops_like_x).

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

% PERSPECTIVE 1: THE REGULATED PLATFORM (SNARE)
% As a victim with constrained exit, the derived d is high, pushing χ above
% the snare threshold. The coordination benefit is invisible from this
% perspective, which sees only coercive extraction of resources and data.
constraint_indexing:constraint_classification(dsa_transparency_requirements, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 2: THE REGULATOR (ROPE)
% As a beneficiary with arbitrage exit, the derived d is very low, making χ
% negative. From the EU Commission's perspective, this is a pure coordination
% mechanism to create a safer, more accountable digital public sphere.
constraint_indexing:constraint_classification(dsa_transparency_requirements, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE END USER (UNTARGETED)
% A typical EU citizen is a beneficiary but has little power or direct agency.
% They experience the law's effects indirectly. Their power is low, and they
% are essentially trapped within the digital ecosystem governed by these rules.
% They perceive it as a protective rope.
constraint_indexing:constraint_classification(dsa_transparency_requirements, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).


% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the genuine coordination function (benefiting users)
% and the high, asymmetric extraction imposed on platforms. This dual nature
% is the definition of a Tangled Rope. This is the canonical classification.
constraint_indexing:constraint_classification(dsa_transparency_requirements, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dsa_transparency_requirements_tests).

test(perspectival_gap_inter_institutional, [nondet]) :-
    % Verify the core perspectival gap between the two institutional actors.
    constraint_indexing:constraint_classification(dsa_transparency_requirements, TypeTarget, context(agent_power(institutional), _, exit_options(constrained), _)),
    constraint_indexing:constraint_classification(dsa_transparency_requirements, TypeBeneficiary, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    assertion(TypeTarget == snare),
    assertion(TypeBeneficiary == rope),
    TypeTarget \= TypeBeneficiary.

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(dsa_transparency_requirements, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    % A constraint is only a Tangled Rope if it has all three structural markers.
    narrative_ontology:constraint_beneficiary(dsa_transparency_requirements, _), % Has coordination function
    narrative_ontology:constraint_victim(dsa_transparency_requirements, _),     % Has asymmetric extraction
    domain_priors:requires_active_enforcement(dsa_transparency_requirements).   % Requires enforcement

:- end_tests(dsa_transparency_requirements_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): Represents the significant cost to VLOPs for
 *     compliance, including engineering resources, legal teams, potential
 *     disclosure of proprietary methods, and fines for non-compliance.
 *   - Suppression (s=0.80): The DSA is a legal mandate within the EU; platforms
 *     have no alternative but to comply or exit a major global market. This
 *     suppresses alternative governance models.
 *   - The combination of a clear public good (coordination function) and
 *     significant, coercive costs (asymmetric extraction) makes this a
 *     canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark and occurs between two institutional actors.
 *   - For the regulated platform (X), the DSA is a Snare. It's a coercive,
 *     extractive mandate imposed externally with high costs and penalties. The
 *     coordination benefits for society are externalized and not felt by the
 *     platform itself.
 *   - For the regulator (EU Commission), it is a Rope. They designed it as a
 *     coordination tool to align platform behavior with public interest.
 *     From their position of power (arbitrage exit), the coercive element is
 *     simply a necessary tool for alignment, not extraction.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This story is a prime example of inter-institutional dynamics. Both the
 *   EU Commission and platform X are `institutional` actors, but their
 *   structural relationship to the constraint is inverted. The key
 *   differentiator that the model uses is `exit_options`. The Commission has
 *   `arbitrage` (they write the rules), while X has `constrained` exit (leaving
 *   the EU is a massive, costly decision). This difference in exit options,
 *   combined with their declared beneficiary/victim status, correctly drives
 *   the directionality `d` to opposite ends of the spectrum, producing the
 *   Rope/Snare classification gap.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification is crucial here. Labeling the DSA as a
 *   pure Snare (as the platforms might) would ignore its genuine and intended
 *   coordination function of protecting citizens. Labeling it as a pure Rope
 *   (as the EU might) would ignore the immense coercive power and extractive
 *   cost imposed on an entire industry. The Tangled Rope classification
 *   correctly identifies and holds both truths in tension, providing a more
 *   complete structural picture than either of the polar-opposite labels.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_dsa_effectiveness,
    'Will the DSA primarily function as a coordination mechanism for public good, or will its compliance costs become a barrier to entry that entrenches incumbent VLOPs, effectively becoming a snare for market competition?',
    'Empirical analysis of market dynamics, new platform entry rates, and innovation in the EU digital sector over the next 5-10 years.',
    'If it fosters a healthier ecosystem -> validates the Rope aspect. If it stifles competition and entrenches incumbents -> validates the Snare aspect and indicates potential degradation into a Piton maintained by large players.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(dsa_transparency_requirements, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This models the DSA's evolution from a proposal to an actively enforced law,
% demonstrating extraction_accumulation as it solidified.
%
% Theater ratio over time (declines as enforcement becomes real):
narrative_ontology:measurement(dsa_tr_t0, dsa_transparency_requirements, theater_ratio, 0, 0.30).
narrative_ontology:measurement(dsa_tr_t5, dsa_transparency_requirements, theater_ratio, 5, 0.25).
narrative_ontology:measurement(dsa_tr_t10, dsa_transparency_requirements, theater_ratio, 10, 0.20).

% Extraction over time (increases as compliance becomes mandatory and fines are levied):
narrative_ontology:measurement(dsa_ex_t0, dsa_transparency_requirements, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dsa_ex_t5, dsa_transparency_requirements, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(dsa_ex_t10, dsa_transparency_requirements, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The DSA sets a common standard for platform operation.
narrative_ontology:coordination_type(dsa_transparency_requirements, information_standard).

% Network relationship: The DSA is part of a broader global move towards
% regulating platform power.
narrative_ontology:affects_constraint(platform_accountability_regulations, dsa_transparency_requirements).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation
% chain (beneficiary/victim declarations + exit_options) accurately models
% the directionality for all key agents, particularly the asymmetric
% relationship between the regulator and the regulated platform.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */