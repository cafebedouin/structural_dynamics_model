% ============================================================================
% CONSTRAINT STORY: fptp_electoral_system
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_fptp_electoral_system, []).

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
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fptp_electoral_system
 *   human_readable: The "First-Past-the-Post" (Plurality) Electoral System
 *   domain: political
 *
 * SUMMARY:
 *   The First-Past-the-Post (FPTP) voting system is a winner-take-all
 *   electoral mechanism where the candidate with the most votes in a
 *   district wins, regardless of whether they achieve a majority. This system
 *   structurally encourages a two-party duopoly by creating a "spoiler effect"
 *   that renders third-party votes ineffective, forcing strategic voting
 *   and producing disproportional legislative outcomes.
 *
 * KEY AGENTS (by structural relationship):
 *   - established_major_parties: Primary beneficiary (institutional/arbitrage) — benefits from reduced competition and entrenched power.
 *   - third_party_voters_and_candidates: Primary target (powerless/trapped) — bears extraction of political representation; their votes are suppressed or "wasted".
 *   - electoral_reform_advocates: Secondary actor (organized/constrained) - attempts to change the system but faces high structural barriers.
 *   - political_scientists: Analytical observer — sees the full game-theoretic structure, including both the coordination and extraction functions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fptp_electoral_system, 0.75).
domain_priors:suppression_score(fptp_electoral_system, 0.80).   % Structural property (raw, unscaled). High due to suppression of alternatives (e.g. Proportional Representation, RCV).
domain_priors:theater_ratio(fptp_electoral_system, 0.30).       % Piton detection (< 0.70). The system functions to produce a government, so it's not pure theater.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fptp_electoral_system, extractiveness, 0.75).
narrative_ontology:constraint_metric(fptp_electoral_system, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(fptp_electoral_system, theater_ratio, 0.30).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(fptp_electoral_system, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(fptp_electoral_system). % Required for Tangled Rope. The system is codified in law and actively maintained.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(fptp_electoral_system, established_major_parties).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(fptp_electoral_system, third_party_voters_and_candidates).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three are met)
%   Snare:        victim required (met)

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
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
%   χ ≈ 0.75 * 1.42 * 1.0 (national scope) = 1.065. This clearly classifies as a Snare.
constraint_indexing:constraint_classification(fptp_electoral_system, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
%   χ ≈ 0.75 * -0.12 * 1.0 (national scope) = -0.09. A negative χ indicates a subsidy; this is a pure Rope.
constraint_indexing:constraint_classification(fptp_electoral_system, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context (civilizational/analytical/global).
% Sees both the coordination function and the severe extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15.
% χ ≈ 0.75 * 1.15 * 1.2 (global scope) = 1.035.
% While χ is high, the analytical perspective recognizes the coordination function
% (beneficiary exists) and asymmetric extraction (victim exists), leading to Tangled Rope.
constraint_indexing:constraint_classification(fptp_electoral_system, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE REFORM ADVOCATE (TANGLED ROPE)
% This agent is organized but constrained by the system they are trying to change.
% They see both the system's function and its deep flaws.
constraint_indexing:constraint_classification(fptp_electoral_system, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fptp_electoral_system_tests).

test(perspectival_gap_is_snare_vs_rope, [nondet]) :-
    % Verify the core perspectival gap between target (Snare) and beneficiary (Rope).
    constraint_indexing:constraint_classification(fptp_electoral_system, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(fptp_electoral_system, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_tangled_rope, [nondet]) :-
    % The system's final claim must be Tangled Rope.
    constraint_indexing:constraint_classification(fptp_electoral_system, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_gates_pass) :-
    % Verify that all three structural conditions for a Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(fptp_electoral_system, _),
    narrative_ontology:constraint_victim(fptp_electoral_system, _),
    domain_priors:requires_active_enforcement(fptp_electoral_system).

:- end_tests(fptp_electoral_system_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.75): High. This value represents the significant loss of representative fidelity. It's measured by the number of "wasted votes" (votes not cast for the winner) and the structural incentive for strategic voting, where voters misrepresent their true preferences. A system where ~50% of votes don't elect anyone and many others are cast strategically is highly extractive of political will.
 *   - Suppression (S=0.80): High. The existence of well-understood and functional alternatives (like Proportional Representation or RCV) which are actively resisted by the system's beneficiaries (the major parties) demonstrates high suppression. The system's core mechanic (Duverger's Law) is to suppress third parties.
 *   - The combination of a genuine coordination function (reliably producing a government) and severe asymmetric extraction makes this a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. For `third_party_voters_and_candidates` (powerless, trapped), the system is a Snare. Their political participation is captured and nullified to maintain the status quo. For the `established_major_parties` (institutional, arbitrage), it is a perfect Rope. It provides stability, predictability, and a near-insurmountable barrier to entry for competitors, all at no cost to them. This gap between Snare and Rope is characteristic of deeply entrenched, extractive political institutions.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `established_major_parties`. The FPTP system is the primary structural reason for the durable two-party system in countries like the US and UK. It creates a duopoly that benefits from reduced competition. Their directionality `d` is very low.
 *   - Victim: `third_party_voters_and_candidates`. They bear the direct cost. Their preferences are systematically under-represented, their candidates cannot win, and they are forced to choose between voting for their preference (and likely "wasting" the vote) or voting strategically for the "lesser of two evils." Their directionality `d` is very high.
 *   The automatic derivation from these structural roles and exit options correctly models the dynamics of the system.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 *   This classification correctly avoids two potential errors. A naive analysis might see only the extraction and label FPTP a pure Snare, ignoring that it does solve the complex coordination problem of forming a stable government. Conversely, a defender of the system might see only the coordination and label it a Rope, ignoring the massive representational cost imposed on the electorate. The Tangled Rope classification captures this essential duality: it is a system that performs a function, but does so via a highly extractive and suppressive mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_fptp_stability,
    'Is the governing stability provided by the two-party system a necessary coordination good that outweighs its representational costs, or is it merely an artifact of incumbent preservation?',
    'Long-term comparative analysis of governance stability, policy responsiveness, and public trust in FPTP countries vs. multi-party systems with proportional representation.',
    'If stability is a critical, irreplaceable benefit, the "Rope" aspect is stronger. If it is an over-stated benefit used to justify incumbent power, the "Snare" aspect is almost total.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_fptp_stability, empirical, 'Is the stability from FPTP a necessary good or an artifact of incumbent preservation?').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(fptp_electoral_system, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. FPTP has become more extractive
% over time as partisan polarization has increased and gerrymandering has
% become more precise, amplifying its worst effects. This demonstrates
% 'extraction_accumulation' drift.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time:
narrative_ontology:measurement(fptp_tr_t0, fptp_electoral_system, theater_ratio, 0, 0.20).
narrative_ontology:measurement(fptp_tr_t5, fptp_electoral_system, theater_ratio, 5, 0.25).
narrative_ontology:measurement(fptp_tr_t10, fptp_electoral_system, theater_ratio, 10, 0.30).

% Extraction over time:
narrative_ontology:measurement(fptp_ex_t0, fptp_electoral_system, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(fptp_ex_t5, fptp_electoral_system, base_extractiveness, 5, 0.70).
narrative_ontology:measurement(fptp_ex_t10, fptp_electoral_system, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% FPTP allocates a scarce resource: political representation/power.
narrative_ontology:coordination_type(fptp_electoral_system, resource_allocation).

% Network relationships (structural influence edges)
% FPTP is a foundational constraint that enables or intensifies others.
% Gerrymandering is only a high-stakes game because of FPTP's winner-take-all nature.
narrative_ontology:affects_constraint(fptp_electoral_system, gerrymandering).
narrative_ontology:affects_constraint(fptp_electoral_system, campaign_finance_duopoly).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The structural derivation based
% on the declared beneficiary/victim groups and their canonical exit options
% accurately models the power dynamics of the FPTP system.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */