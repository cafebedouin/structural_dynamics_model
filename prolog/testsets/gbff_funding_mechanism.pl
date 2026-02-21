% ============================================================================
% CONSTRAINT STORY: gbff_funding_mechanism
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-12-07
% ============================================================================

:- module(constraint_gbff_funding_mechanism, []).

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
    narrative_ontology:coordination_type/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gbff_funding_mechanism
 *   human_readable: Global Biodiversity Framework Fund (GBFF) Funding Mechanism
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   The Global Biodiversity Framework Fund (GBFF) is a multilateral fund designed
 *   to coordinate financing from wealthy nations to developing nations to meet
 *   global biodiversity targets. While it serves a genuine coordination
 *   function, it is criticized for insufficient funding levels ("a drop in the
 *   ocean") and complex, bureaucratic application processes that impose high
 *   transaction costs on recipients. This creates a structural tension between
 *   its stated goal and its functional reality.
 *
 * KEY AGENTS (by structural relationship):
 *   - Indigenous Peoples & Local Communities (IPLCs): Primary target (powerless/trapped) — bear costs of inaccessible, nationally-intermediated funds.
 *   - Developing Nations: Secondary target (organized/constrained) — recipients of aid, but burdened by high transaction costs and insufficient funding.
 *   - Donor Nations (Global North): Primary beneficiary (institutional/arbitrage) — fulfill international commitments and gain diplomatic goodwill at relatively low cost.
 *   - Global Environment Facility (GEF): Secondary beneficiary (institutional/constrained) — the bureaucracy that administers the fund, justifying its existence and budget.
 *   - Analytical Observer: Policy analyst or journalist assessing the fund's effectiveness.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gbff_funding_mechanism, 0.48). % High due to transaction costs, opportunity costs of insufficient funding, and power asymmetry.
domain_priors:suppression_score(gbff_funding_mechanism, 0.65).   % Structural property (raw, unscaled). High because few alternative global funding mechanisms exist.
domain_priors:theater_ratio(gbff_funding_mechanism, 0.40).       % Piton detection (< 0.70). Significant performative aspect in announcements vs. actual impact.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gbff_funding_mechanism, extractiveness, 0.48).
narrative_ontology:constraint_metric(gbff_funding_mechanism, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(gbff_funding_mechanism, theater_ratio, 0.40).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(gbff_funding_mechanism, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(gbff_funding_mechanism). % The GEF actively manages applications and disburses funds.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(gbff_funding_mechanism, donor_nations).
narrative_ontology:constraint_beneficiary(gbff_funding_mechanism, gef_bureaucracy).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(gbff_funding_mechanism, developing_nations).
narrative_ontology:constraint_victim(gbff_funding_mechanism, indigenous_peoples_and_local_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function. The engine derives d
   from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, ..., global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (INDIGENOUS PEOPLES & LOCAL COMMUNITIES)
% For IPLCs, the fund is a remote, bureaucratic process with high barriers to
% entry. They are trapped within national systems that may not represent them.
% Engine derives: victim + trapped -> d ≈ 0.95 -> f(d) ≈ 1.42 -> high χ
% χ ≈ 0.48 * 1.42 * 1.2 (global scope) ≈ 0.82 -> Snare
constraint_indexing:constraint_classification(gbff_funding_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (DONOR NATIONS)
% For donor nations, this is an efficient coordination mechanism to meet treaty
% obligations and exert soft power.
% Engine derives: beneficiary + arbitrage -> d ≈ 0.05 -> f(d) ≈ -0.12 -> negative χ
% χ ≈ 0.48 * -0.12 * 1.2 ≈ -0.07 -> Rope
constraint_indexing:constraint_classification(gbff_funding_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE INTER-INSTITUTIONAL TARGET (DEVELOPING NATIONS)
% Recipient nations experience both the coordination (receiving funds) and the
% extraction (high transaction costs, insufficient amounts).
% Engine derives: victim + constrained exit + organized power -> mid-to-high d
% d ≈ 0.60 -> f(d) ≈ 0.85. χ ≈ 0.48 * 0.85 * 1.2 ≈ 0.49 -> Tangled Rope
constraint_indexing:constraint_classification(gbff_funding_mechanism, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER
% The analyst sees the complete structure: a necessary coordination function
% burdened by significant extractive dynamics.
% Engine derives: analytical power -> d ≈ 0.72 -> f(d) ≈ 1.15
% χ ≈ 0.48 * 1.15 * 1.2 ≈ 0.66 -> Tangled Rope (at the boundary of Snare)
constraint_indexing:constraint_classification(gbff_funding_mechanism, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gbff_funding_mechanism_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify the core perspectival gap between the ultimate target and primary beneficiary.
    constraint_indexing:constraint_classification(gbff_funding_mechanism, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(gbff_funding_mechanism, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    !. % Succeeds if both classifications are found.

test(tangled_rope_conditions_met) :-
    % The analytical and organized perspectives should resolve to Tangled Rope.
    constraint_indexing:constraint_classification(gbff_funding_mechanism, tangled_rope, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(gbff_funding_mechanism, tangled_rope, context(agent_power(organized), _, _, _)),
    !.

test(structural_data_present) :-
    % A Tangled Rope requires beneficiary, victim, and enforcement data.
    narrative_ontology:constraint_beneficiary(gbff_funding_mechanism, _),
    narrative_ontology:constraint_victim(gbff_funding_mechanism, _),
    domain_priors:requires_active_enforcement(gbff_funding_mechanism).

:- end_tests(gbff_funding_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it possesses both a
 *   genuine, necessary coordination function (distributing funds for biodiversity)
 *   and significant, asymmetric extraction. The base extractiveness (ε=0.48)
 *   captures the high transaction costs, bureaucratic hurdles, and the
 *   opportunity cost imposed on developing nations by the gap between promised
 *   and delivered funding. The high suppression score (0.65) reflects the lack
 *   of viable, large-scale alternatives, making engagement with the GBFF
 *   quasi-mandatory for nations seeking such aid.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. Donor nations (institutional/arbitrage) perceive a Rope: an
 *   efficient tool for foreign policy and environmental diplomacy with negative
 *   effective extraction (χ < 0). Conversely, Indigenous Peoples (powerless/trapped)
 *   experience a Snare (χ > 0.66): the promised benefits are largely inaccessible
 *   due to bureaucratic barriers at the national level, leaving them with the
 *   unmitigated costs of biodiversity loss. Developing nations sit in the middle,
 *   experiencing the Tangled Rope directly as they navigate the trade-off
 *   between receiving some aid and bearing the high costs of the process.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `donor_nations` benefit from fulfilling treaty obligations
 *     cheaply and projecting soft power. `gef_bureaucracy` benefits from the
 *     operational budget and institutional mandate. Their `arbitrage` or
 *     `constrained` exit options and beneficiary status lead to low `d` values.
 *   - Victims: `developing_nations` bear the cost of navigating the system and
 *     the shortfall in funding. `indigenous_peoples_and_local_communities`
 *     are victims of both biodiversity loss and a funding system that is
 *     difficult for them to access directly. Their `constrained` or `trapped`
 *     exit options and victim status lead to high `d` values.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two potential errors. It is not a pure
 *   Rope, because that would ignore the significant, well-documented costs and
 *   power imbalances borne by recipients. It is not a pure Snare, because that
 *   would deny its real, albeit flawed, coordination function—money is, in fact,
 *   being distributed. The Tangled Rope classification captures this essential
 *   duality, identifying it as a system where a coordination mandate has become
 *   entangled with extractive dynamics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_gbff_funding_mechanism,
    'Will the GBFF successfully scale to meet its funding targets (e.g., $20B/year), or will it remain a largely symbolic, underfunded instrument?',
    'Tracking total donor contributions vs. pledges and disbursed funds over the next 5-10 years.',
    'If it scales successfully, its extractive nature may decrease relative to its coordination function (drifting towards Rope). If it stagnates, its theater ratio will rise as it fails to achieve its goals, risking degradation into a Piton.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(gbff_funding_mechanism, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This models the potential for the fund to accumulate bureaucracy and fall
% short of its funding goals over time, increasing its extractive and
% theatrical nature. The final T=10 values match the current assessment.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(gbff_tr_t0, gbff_funding_mechanism, theater_ratio, 0, 0.10).
narrative_ontology:measurement(gbff_tr_t5, gbff_funding_mechanism, theater_ratio, 5, 0.25).
narrative_ontology:measurement(gbff_tr_t10, gbff_funding_mechanism, theater_ratio, 10, 0.40).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(gbff_ex_t0, gbff_funding_mechanism, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(gbff_ex_t5, gbff_funding_mechanism, base_extractiveness, 5, 0.40).
narrative_ontology:measurement(gbff_ex_t10, gbff_funding_mechanism, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(gbff_funding_mechanism, resource_allocation).

% Network relationships (structural influence edges)
% The GBFF exists to service the Kunming-Montreal Global Biodiversity Framework treaty.
narrative_ontology:affects_constraint(kunming_montreal_gbf, gbff_funding_mechanism).


% DUAL FORMULATION NOTE:
% This constraint story focuses on the *funding mechanism* (ε=0.48, Tangled Rope).
% A separate constraint, `kunming_montreal_gbf`, would model the biodiversity
% *targets* themselves. The targets might be a Rope (a set of shared goals), but
% the mechanism to achieve them is a Tangled Rope. This decomposition adheres
% to the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The automatic derivation from
% beneficiary/victim status and exit options accurately reflects the structural
% relationships of the agents involved.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */