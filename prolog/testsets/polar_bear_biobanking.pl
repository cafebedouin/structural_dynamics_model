% ============================================================================
% CONSTRAINT STORY: polar_bear_biobanking
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_polar_bear_biobanking, []).

:- use_module(library(plunit)).
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
    constraint_indexing:directionality_override/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: polar_bear_biobanking
 *   human_readable: Polar Bear Genetic Biobanking as a Climate Change Hedge
 *   domain: technological/political
 *
 * SUMMARY:
 *   A policy to create a "biobank" of polar bear DNA to enable potential
 *   "de-extinction" should the species be wiped out by climate change. This
 *   creates a technological coordination effort while also diverting
 *   resources and political will from immediate, systemic climate action,
 *   posing a significant moral hazard.
 *
 * KEY AGENTS (by structural relationship):
 *   - Polar bear species: Primary target (powerless/trapped) — their genetic material is extracted, while their immediate existential threat (habitat loss) is not addressed by this policy.
 *   - Bio-engineering institutes & companies: Primary beneficiary (institutional/arbitrage) — receive funding, prestige, and valuable genetic data/IP.
 *   - Government policymakers: Secondary beneficiary (institutional/constrained) — gain a politically expedient, theatrical "solution" to demonstrate action on conservation.
 *   - The general public / taxpayers: Secondary victim (moderate/mobile) — their funds are used for a speculative project, and they bear the risk of the moral hazard it creates.
 *   - Analytical observer: Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(polar_bear_biobanking, 0.48).
domain_priors:suppression_score(polar_bear_biobanking, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(polar_bear_biobanking, 0.55).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(polar_bear_biobanking, extractiveness, 0.48).
narrative_ontology:constraint_metric(polar_bear_biobanking, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(polar_bear_biobanking, theater_ratio, 0.55).

% --- NL Profile Metrics ---
% Not a mountain constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(polar_bear_biobanking, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(polar_bear_biobanking). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(polar_bear_biobanking, bioengineering_institutes).
narrative_ontology:constraint_beneficiary(polar_bear_biobanking, government_policymakers).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(polar_bear_biobanking, polar_bear_species).
narrative_ontology:constraint_victim(polar_bear_biobanking, public_taxpayers).
narrative_ontology:constraint_victim(polar_bear_biobanking, traditional_conservation_efforts).

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

% PERSPECTIVE 1: THE PRIMARY TARGET (POLAR BEAR SPECIES)
% For the species itself, this program offers no immediate survival benefit.
% It extracts their genetic identity while the cause of their decline continues.
% This is pure, coercive extraction. Engine derives d from victim + trapped.
% d ≈ 0.95 -> f(d) ≈ 1.42. χ ≈ 0.48 * 1.42 * 1.2 ≈ 0.82. -> Snare.
constraint_indexing:constraint_classification(polar_bear_biobanking, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (BIO-ENGINEERING INSTITUTES)
% This is a well-funded coordination mechanism that provides resources and
% prestige. Engine derives d from beneficiary + arbitrage.
% d ≈ 0.05 -> f(d) ≈ -0.12. χ ≈ 0.48 * -0.12 * 1.0 ≈ -0.06 -> Rope.
constraint_indexing:constraint_classification(polar_bear_biobanking, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Sees both the genuine coordination function and the severe extractive
% side-effects (moral hazard, resource diversion). This dual nature is the
% definition of a Tangled Rope.
% Engine derives d ≈ 0.72 -> f(d) ≈ 1.15. χ ≈ 0.48 * 1.15 * 1.2 ≈ 0.66. -> Tangled Rope
constraint_indexing:constraint_classification(polar_bear_biobanking, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---

% PERSPECTIVE 4A: GOVERNMENT POLICYMAKERS (Beneficiary)
% Views the program as a Rope that solves a political problem: appearing to act
% on a popular environmental issue without imposing politically costly economic
% changes. Their exit is constrained by political cycles and public opinion.
% d (beneficiary + constrained exit) is slightly higher than for arbitrage.
constraint_indexing:constraint_classification(polar_bear_biobanking, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4B: TRADITIONAL CONSERVATION NGOS (Victim)
% Experiences the program as a competitor for funding and political attention,
% suppressing their preferred (and more direct) conservation strategies. They
% see the coordination but primarily feel the extraction and suppression.
% d (victim + mobile) -> d ≈ 0.85 -> f(d) ≈ 1.15. χ ≈ 0.48 * 1.15 * 1.0 ≈ 0.55. -> Tangled Rope.
constraint_indexing:constraint_classification(polar_bear_biobanking, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(polar_bear_biobanking_tests).

test(perspectival_gap_target_beneficiary) :-
    constraint_indexing:constraint_classification(polar_bear_biobanking, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(polar_bear_biobanking, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    format('... Perspectival gap validated (Snare vs. Rope)\n').

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(polar_bear_biobanking, tangled_rope, context(agent_power(analytical), _, _, _)),
    format('... Analytical classification as Tangled Rope validated\n').

test(tangled_rope_gate_requirements_met) :-
    narrative_ontology:constraint_beneficiary(polar_bear_biobanking, _),
    narrative_ontology:constraint_victim(polar_bear_biobanking, _),
    domain_priors:requires_active_enforcement(polar_bear_biobanking),
    format('... Tangled Rope structural requirements (beneficiary, victim, enforcement) validated\n').

:- end_tests(polar_bear_biobanking_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): This value reflects not just the direct financial cost,
 *     but the significant opportunity cost and moral hazard. The policy extracts political
 *     will and public attention from more effective but difficult solutions like decarbonization.
 *   - Suppression (s=0.65): High. The existence of a high-tech "backup plan" actively
 *     suppresses the urgency of systemic climate action, making it harder for alternative
 *     policies to gain traction.
 *   - Theater (t=0.55): Significant. While there is a real scientific function, a large
 *     part of the program's value to policymakers is performative—demonstrating action.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the bio-engineering institutes (beneficiaries), this is a
 *   model of coordination (Rope), funding their research. For the polar bears (targets),
 *   it is a cruel irony (Snare), archiving their genetic code while their world melts.
 *   This highlights the core tension of a Tangled Rope: the coordination function that
 *   benefits one group is funded by an extraction that harms another.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from clear structural roles. 'bioengineering_institutes'
 *   are beneficiaries as they receive funding and IP. 'polar_bear_species' and
 *   'public_taxpayers' are victims, as one provides the raw material without consent or
 *   benefit, and the other pays for a program that may undermine their long-term security
 *   by enabling climate inaction. These declarations drive the d-value calculation, leading
 *   to negative effective extraction (χ) for beneficiaries and high positive χ for victims.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The model distinguishes between policymakers and conservation NGOs. Both are powerful,
 *   organized actors. However, policymakers benefit from the political theater and see a
 *   Rope. Conservation NGOs see their primary mission (habitat preservation) undermined
 *   and resources diverted, thus experiencing it as a Tangled Rope. Their different
 *   structural relationships to the constraint's primary function and side-effects create
 *   this gap, even at similar power levels.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a prime example of where a simple analysis might fail. A naive view could
 *   label it a pure coordination effort ("saving the polar bears" - Rope) or pure waste
 *   ("pointless science project" - Piton). The Tangled Rope classification correctly
 *   identifies that it is BOTH: a genuine coordination effort AND a mechanism for
 *   extractive moral hazard. It avoids mandatrophy by acknowledging the real coordination
 *   function while quantifying the severe negative externality imposed on the victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_polar_bear_biobanking,
    'Is the program a genuine last-resort safety net or a deliberate political distraction to enable climate inaction?',
    'Analysis of internal government/funder communications to determine primary intent vs. stated intent.',
    'If genuine safety net -> Re-classify towards Scaffold (lower ε). If deliberate distraction -> Re-classify towards Snare (higher ε, higher suppression).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(polar_bear_biobanking, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint likely began as a more idealistic research proposal (Scaffold/Rope)
% and drifted towards a Tangled Rope as it became institutionalized and
% co-opted for political purposes. This drift is modeled below.
% Required since base_extractiveness (0.48) > 0.46.

% Theater ratio over time (drifting from function to performance):
narrative_ontology:measurement(pbb_tr_t0, polar_bear_biobanking, theater_ratio, 0, 0.20).
narrative_ontology:measurement(pbb_tr_t5, polar_bear_biobanking, theater_ratio, 5, 0.40).
narrative_ontology:measurement(pbb_tr_t10, polar_bear_biobanking, theater_ratio, 10, 0.55).

% Extraction over time (drifting from research project to moral hazard):
narrative_ontology:measurement(pbb_ex_t0, polar_bear_biobanking, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(pbb_ex_t5, polar_bear_biobanking, base_extractiveness, 5, 0.40).
narrative_ontology:measurement(pbb_ex_t10, polar_bear_biobanking, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The primary function is allocating funds and scientific
% resources towards a specific goal.
narrative_ontology:coordination_type(polar_bear_biobanking, resource_allocation).

% Network relationships: This policy exists as a downstream effect of the
% broader failure to address climate change, which is driven by fossil fuel
% dependency.
narrative_ontology:affects_constraint(fossil_fuel_dependency, polar_bear_biobanking).
narrative_ontology:affects_constraint(systemic_climate_inaction, polar_bear_biobanking).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation
% based on the declared beneficiary/victim groups and their exit options
% accurately models the directionality of the constraint for each key agent.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */