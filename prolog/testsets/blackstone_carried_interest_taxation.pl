% ============================================================================
% CONSTRAINT STORY: blackstone_carried_interest_taxation
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-20
% ============================================================================

:- module(constraint_blackstone_carried_interest_taxation, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: blackstone_carried_interest_taxation
 *   human_readable: Carried Interest Partnership Taxation
 *   domain: economic/political
 *
 * SUMMARY:
 *   The regulatory and tax framework in the United States that treats "carried
 *   interest" (performance fees) for partners in private equity and hedge funds
 *   as long-term capital gains rather than ordinary income. This results in a
 *   significantly lower tax rate. The constraint's existence relies on active
 *   lobbying and complex legal structuring to defend its status against
 *   legislative challenges that seek to reclassify it as income.
 *
 * KEY AGENTS (by structural relationship):
 *   - us_taxpayers: Primary target (powerless/trapped) — bear the cost via reduced public tax revenue, with no direct recourse.
 *   - private_equity_partners: Primary beneficiary (institutional/arbitrage) — benefit from lower tax rates on their primary form of compensation.
 *   - reformist_legislators: Secondary institutional actor (institutional/constrained) — attempt to change the rule but face high political and structural barriers.
 *   - analytical_observer: Analytical observer — sees the dual function of coordination (for partners) and extraction (from the tax base).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(blackstone_carried_interest_taxation, 0.30).
domain_priors:suppression_score(blackstone_carried_interest_taxation, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(blackstone_carried_interest_taxation, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(blackstone_carried_interest_taxation, extractiveness, 0.30).
narrative_ontology:constraint_metric(blackstone_carried_interest_taxation, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(blackstone_carried_interest_taxation, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(blackstone_carried_interest_taxation, tangled_rope).
narrative_ontology:human_readable(blackstone_carried_interest_taxation, "Carried Interest Partnership Taxation").

% --- Binary flags ---
domain_priors:requires_active_enforcement(blackstone_carried_interest_taxation). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(blackstone_carried_interest_taxation, private_equity_partners).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(blackstone_carried_interest_taxation, us_taxpayers).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three)

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
% U.S. taxpayers who indirectly bear the cost through reduced public revenue.
% They are trapped within the national tax system with no direct ability to
% change this specific rule.
constraint_indexing:constraint_classification(blackstone_carried_interest_taxation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Private equity partners (e.g., Blackstone's Senior Managing Directors).
% For them, this is a pure coordination mechanism to structure their
% compensation in the most tax-efficient way possible.
constraint_indexing:constraint_classification(blackstone_carried_interest_taxation, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% An economic analyst sees both the coordination function for partners and the
% asymmetric extraction from the public tax base, supported by high suppression
% (lobbying, legal complexity). This dual nature is the hallmark of a Tangled Rope.
constraint_indexing:constraint_classification(blackstone_carried_interest_taxation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---
% A reformist legislator attempting to change the law. They are an institutional
% actor but face immense structural resistance, making their exit options
% 'constrained'. From this viewpoint, the rule is a Snare designed to extract
% wealth and resist modification.
constraint_indexing:constraint_classification(blackstone_carried_interest_taxation, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(blackstone_carried_interest_taxation_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(blackstone_carried_interest_taxation, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(blackstone_carried_interest_taxation, TypeBeneficiary, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    TypeTarget \= TypeBeneficiary,
    TypeTarget = snare,
    TypeBeneficiary = rope.

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(blackstone_carried_interest_taxation, tangled_rope, context(agent_power(analytical), _, _, _)).

test(inter_institutional_gap) :-
    % Verify that two institutional actors with different exit options see different classifications.
    constraint_indexing:constraint_classification(blackstone_carried_interest_taxation, TypeBeneficiary, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(blackstone_carried_interest_taxation, TypeReformer, context(agent_power(institutional), _, exit_options(constrained), _)),
    TypeBeneficiary = rope,
    TypeReformer = snare.

:- end_tests(blackstone_carried_interest_taxation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.30): The extraction is indirect. The constraint doesn't take money from taxpayers directly, but prevents revenue from being collected, representing a moderate opportunity cost to the public treasury.
 *   - Suppression (S=0.70): High. The status of carried interest is maintained through intense lobbying, complex legal arguments, and the political difficulty of tax reform. Alternatives (treating it as ordinary income) are actively suppressed.
 *   - Theater Ratio (TR=0.10): Low. The constraint is highly functional for its beneficiaries; there is very little performative activity.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the private equity partners (beneficiaries), it's a 'Rope'—a brilliant tool for wealth coordination. For taxpayers (victims) and legislative reformers, it's a 'Snare'—an unfair loophole that extracts value from the public commons and is fiercely defended against change.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `private_equity_partners`. They directly and substantially benefit from lower tax rates. Their `arbitrage` exit option and beneficiary status derive a low `d` value, resulting in a negative effective extraction (χ) and a 'Rope' classification.
 *   - Victim: `us_taxpayers`. They bear the collective cost of lost revenue. Their `trapped` status and victim membership derive a high `d` value, resulting in a high χ and a 'Snare' classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The analytical classification as 'Tangled Rope' is crucial. A naive analysis might label this a pure 'Snare' (focusing only on the extraction) or a pure 'Rope' (focusing only on the partners' coordination). The Tangled Rope classification correctly identifies that it has BOTH a genuine coordination function for a specific group AND an asymmetric extractive effect on another, and requires active enforcement (lobbying) to maintain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_blackstone_carried_interest,
    'Is the "risk-taking" argument for carried interest a functional justification (reducing ε) or a theatrical narrative (increasing TR)?',
    'An empirical study comparing the risk profiles of compensation via carried interest versus executive stock options.',
    'If functional, it is a true Tangled Rope. If theatrical, it is a Snare masquerading as a Tangled Rope.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_blackstone_carried_interest, conceptual, 'Whether the "at-risk capital" argument for carried interest is a functional justification or a theatrical narrative.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(blackstone_carried_interest_taxation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% No temporal data required as base_extractiveness (0.30) is below the 0.46 threshold.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This constraint functions as a mechanism for allocating resources (tax revenue)
% away from the public and towards a specific group.
narrative_ontology:coordination_type(blackstone_carried_interest_taxation, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed. The structural derivation from beneficiary/victim
% declarations and exit options accurately models the directionality for all
% key agents in this scenario.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */