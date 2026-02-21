% ============================================================================
% CONSTRAINT STORY: indian_ai_licensing_regime
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_indian_ai_licensing_regime, []).

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
 *   constraint_id: indian_ai_licensing_regime
 *   human_readable: India's Sovereign AI Licensing and Data Localization Mandate
 *   domain: technological/political
 *
 * SUMMARY:
 *   Following the 2026 Delhi AI Expo, the Indian government enacted a new
 *   regulatory framework under its "AI for All" initiative. This framework
 *   mandates that all large AI models operating in India must obtain a license,
 *   pay significant fees, and process/store sensitive Indian user data on
 *   local servers. The stated goal is to foster a "sovereign AI" ecosystem
 *   and protect citizens, but it simultaneously erects substantial barriers to
 *   entry for foreign technology companies.
 *
 * KEY AGENTS (by structural relationship):
 *   - Indian Citizens: Primary target (powerless/trapped) — bear costs of reduced competition and potential surveillance.
 *   - Foreign Tech Companies (OpenAI, Google, etc.): Primary target (powerful/constrained) — face extraction via licensing fees and operational hurdles.
 *   - Indian State & Domestic AI Firms: Primary beneficiary (institutional/arbitrage) — benefit from protected market and captured value.
 *   - Global Policy Analyst: Analytical observer — sees the dual function of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indian_ai_licensing_regime, 0.48).
domain_priors:suppression_score(indian_ai_licensing_regime, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(indian_ai_licensing_regime, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indian_ai_licensing_regime, extractiveness, 0.48).
narrative_ontology:constraint_metric(indian_ai_licensing_regime, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(indian_ai_licensing_regime, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(indian_ai_licensing_regime, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(indian_ai_licensing_regime). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(indian_ai_licensing_regime, indian_state_and_domestic_ai_firms).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(indian_ai_licensing_regime, foreign_tech_companies).
narrative_ontology:constraint_victim(indian_ai_licensing_regime, indian_citizens).

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

% PERSPECTIVE 1: THE INDIAN CITIZEN (SNARE)
% As a member of the 'victim' group with 'trapped' exit, the engine
% derives a high d (≈0.95), leading to a high f(d) (≈1.42).
% χ ≈ 0.48 * 1.42 * 1.0 (national) ≈ 0.68. This exceeds the Snare threshold (0.66).
constraint_indexing:constraint_classification(indian_ai_licensing_regime, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE INDIAN STATE & DOMESTIC AI FIRMS (ROPE)
% As a member of the 'beneficiary' group with 'arbitrage' exit, the engine
% derives a low d (≈0.05), leading to a negative f(d) (≈-0.12).
% χ ≈ 0.48 * -0.12 * 1.0 (national) ≈ -0.06. This is a clear Rope.
constraint_indexing:constraint_classification(indian_ai_licensing_regime, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: FOREIGN TECH COMPANIES (TANGLED ROPE)
% As a 'victim' with 'constrained' exit (the Indian market is too large to
% abandon easily), the engine derives a high d, but lower than 'trapped'.
% This leads to a high χ that reveals the extraction but may fall short
% of the pure Snare threshold, exposing the constraint's dual nature.
constraint_indexing:constraint_classification(indian_ai_licensing_regime, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analytical observer sees both the coordination function and the
% asymmetric extraction. The default analytical 'd' (≈0.72) and global scope
% modifier (1.2) produce a high χ.
% χ ≈ 0.48 * 1.15 * 1.2 (global) ≈ 0.66. This sits on the Snare/Tangled Rope
% boundary, but Tangled Rope is the correct analytical claim as it
% acknowledges the (claimed) coordination function.
constraint_indexing:constraint_classification(indian_ai_licensing_regime, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indian_ai_licensing_regime_tests).

test(perspectival_gap_citizen_vs_state, [nondet]) :-
    % Verify the core perspectival gap.
    constraint_indexing:constraint_classification(indian_ai_licensing_regime, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(indian_ai_licensing_regime, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_claim_matches_type, [nondet]) :-
    narrative_ontology:constraint_claim(indian_ai_licensing_regime, Claim),
    constraint_indexing:constraint_classification(indian_ai_licensing_regime, Claim, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements_met) :-
    % A Tangled Rope requires a beneficiary, a victim, and active enforcement.
    narrative_ontology:constraint_beneficiary(indian_ai_licensing_regime, _),
    narrative_ontology:constraint_victim(indian_ai_licensing_regime, _),
    domain_priors:requires_active_enforcement(indian_ai_licensing_regime).

:- end_tests(indian_ai_licensing_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): The policy is explicitly designed to capture value
 *     from foreign entities (via fees) and transfer market opportunity to domestic
 *     players. This is a significant level of structural extraction.
 *   - Suppression (0.65): The high cost of compliance and the threat of market
 *     exclusion effectively suppress the alternative of an open, unregulated market.
 *     It coerces participation in the new regime.
 *   - The combination of ε >= 0.30, suppression >= 0.40, and the presence of both
 *     beneficiaries and victims makes this a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. The Indian state and its favored domestic firms see a
 *   Rope: a pure coordination mechanism for building a national tech ecosystem.
 *   Foreign firms see a Tangled Rope, acknowledging the coordination goal but
 *   feeling the sharp sting of extraction. Indian citizens, who are trapped within
 *   the system and may face higher costs, less choice, and increased state
 *   surveillance as a result, perceive a pure Snare.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `indian_state_and_domestic_ai_firms`. They directly benefit from
 *     the protectionist measures and licensing revenue/control. Their 'arbitrage'
 *     exit option reflects their ability to shape the rules of the system they inhabit.
 *   - Victims: `foreign_tech_companies` and `indian_citizens`. The former are the
 *     explicit targets of extraction. The latter are indirect victims, bearing the
 *     second-order costs. Their different exit options (`constrained` vs. `trapped`)
 *     correctly model their differing levels of agency and result in different
 *     classifications (Tangled Rope vs. Snare).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two common errors. It does not accept
 *   the state's "coordination-only" narrative (which would misclassify it as a
 *   Rope for all). It also does not dismiss the coordination function entirely
 *   (which would misclassify it as a Snare from an analytical view). The Tangled
 *   Rope classification correctly identifies that the constraint has BOTH a
 *   genuine (if debatable) coordination function AND a significant, asymmetric
 *   extractive function. The model also notes that for the most trapped agents,
 *   the coordination function becomes irrelevant, and the constraint resolves to a pure Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_indian_ai_licensing_regime,
    'Will this "sovereign AI" policy lead to a vibrant, competitive domestic ecosystem or an inefficient, state-controlled digital fortress?',
    'Long-term observation of market concentration, AI service quality/pricing in India, and global competitiveness of Indian AI firms.',
    'If it fosters competition (Scaffold-like outcome), the long-term extraction might decrease. If it creates monopolies (Snare entrenchment), extraction will rise.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indian_ai_licensing_regime, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint (ε=0.48 > 0.46), so temporal data is required.
% We model the policy's implementation, starting from a less extractive proposal
% phase (T=0) and ramping up to its full extractive potential (T=10).

% Theater ratio over time (the "AI for All" rhetoric remains constant):
narrative_ontology:measurement(indian_ai_tr_t0, indian_ai_licensing_regime, theater_ratio, 0, 0.15).
narrative_ontology:measurement(indian_ai_tr_t5, indian_ai_licensing_regime, theater_ratio, 5, 0.20).
narrative_ontology:measurement(indian_ai_tr_t10, indian_ai_licensing_regime, theater_ratio, 10, 0.20).

% Extraction over time (licensing fees and compliance costs increase as the regime is fully enforced):
narrative_ontology:measurement(indian_ai_ex_t0, indian_ai_licensing_regime, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(indian_ai_ex_t5, indian_ai_licensing_regime, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(indian_ai_ex_t10, indian_ai_licensing_regime, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% The policy's primary function is to allocate market access and control data flows.
narrative_ontology:coordination_type(indian_ai_licensing_regime, resource_allocation).

% This national policy has clear structural impacts on global technology standards and supply chains.
narrative_ontology:affects_constraint(indian_ai_licensing_regime, global_tech_supply_chain).
narrative_ontology:affects_constraint(indian_ai_licensing_regime, digital_sovereignty_frameworks).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The standard derivation chain,
% using the declared beneficiary/victim groups and their respective exit options,
% accurately models the directionality for each key agent. The distinction
% between `constrained` (for foreign tech) and `trapped` (for citizens) is
% sufficient to generate the correct perspectival classifications.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */