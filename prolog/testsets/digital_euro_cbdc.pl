% ============================================================================
% CONSTRAINT STORY: digital_euro_cbdc
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_digital_euro_cbdc, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: digital_euro_cbdc
 *   human_readable: The European Union's Central Bank Digital Currency (CBDC)
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The proposed Digital Euro is a central bank-issued digital currency intended to
 *   complement cash and commercial bank deposits. While officially framed as a tool
 *   for monetary sovereignty and payment system modernization, it structurally introduces
 *   the capacity for universal transaction surveillance and programmable money,
 *   fundamentally altering the relationship between the citizen and the state.
 *
 * KEY AGENTS (by structural relationship):
 *   - European Citizens: Primary target (powerless/trapped) — bear the extraction of privacy and financial autonomy.
 *   - Central Banks & Governments: Primary beneficiary (institutional/arbitrage) — gain granular control over the monetary system.
 *   - Commercial Banking Sector: Secondary actor (institutional/constrained) — threatened by disintermediation but also integrated into the new system.
 *   - Analytical Observer: Analytical observer — sees both the coordination function and the extractive potential.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_euro_cbdc, 0.55). % Potential for surveillance, negative rates, spending controls.
domain_priors:suppression_score(digital_euro_cbdc, 0.75).   % Structurally suppresses anonymous cash as the primary alternative.
domain_priors:theater_ratio(digital_euro_cbdc, 0.25).       % Presented as functional modernization, but downplays control aspects.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_euro_cbdc, extractiveness, 0.55).
narrative_ontology:constraint_metric(digital_euro_cbdc, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(digital_euro_cbdc, theater_ratio, 0.25).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(digital_euro_cbdc, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(digital_euro_cbdc). % Programmability and transaction limits are not emergent.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(digital_euro_cbdc, central_banks_and_governments).
narrative_ontology:constraint_beneficiary(digital_euro_cbdc, commercial_banking_sector). % Also a beneficiary of a stable digital system

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(digital_euro_cbdc, privacy_conscious_citizens).
narrative_ontology:constraint_victim(digital_euro_cbdc, commercial_banking_sector). % Also a victim of disintermediation risk

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

% PERSPECTIVE 1: THE PRIVACY-CONSCIOUS CITIZEN (SNARE)
% Engine derives d from: victim membership + trapped exit (if cash is phased out) → d ≈ 0.95 → f(d) ≈ 1.42.
% χ = 0.55 * 1.42 * 1.1 (continental) ≈ 0.86. This is well into Snare territory (χ >= 0.66).
constraint_indexing:constraint_classification(digital_euro_cbdc, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ECB/GOVERNMENTS) (ROPE)
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12.
% χ becomes negative, indicating subsidy. The system is a pure coordination tool.
constraint_indexing:constraint_classification(digital_euro_cbdc, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Sees both coordination and extraction. Engine derives canonical d ≈ 0.72 → f(d) ≈ 1.15.
% χ = 0.55 * 1.15 * 1.2 (global) ≈ 0.76. Meets Tangled Rope χ criteria.
% The logic gates for Tangled Rope also pass: has_coordination_function (from beneficiary)
% + has_asymmetric_extraction (from victim) + requires_active_enforcement.
constraint_indexing:constraint_classification(digital_euro_cbdc, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---

% PERSPECTIVE 4: THE COMMERCIAL BANKING SECTOR (TANGLED ROPE)
% A complex position. They are listed as both beneficiary (new platform) and victim (disintermediation risk).
% Their exit is constrained; they cannot easily opt out of the Eurozone financial system.
% Engine derives d from: 'both' + 'constrained' exit -> d ≈ 0.50 -> f(d) ≈ 0.65.
% χ = 0.55 * 0.65 * 1.1 (continental) ≈ 0.39. This is below the Snare threshold but reflects a
% system with both benefits and significant costs/risks. Classified as Tangled Rope.
constraint_indexing:constraint_classification(digital_euro_cbdc, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_euro_cbdc_tests).

test(perspectival_gap_citizen_vs_state, [nondet]) :-
    % Verify the core perspectival gap between the citizen (target) and state (beneficiary).
    constraint_indexing:constraint_classification(digital_euro_cbdc, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(digital_euro_cbdc, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_classification_is_tangled_rope, [nondet]) :-
    % The analytical observer must correctly identify the hybrid nature.
    constraint_indexing:constraint_classification(digital_euro_cbdc, tangled_rope, context(agent_power(analytical), _, _, _)).

test(inter_institutional_perspective_is_distinct, [nondet]) :-
    % Verify the commercial bank perspective is different from the central bank perspective.
    constraint_indexing:constraint_classification(digital_euro_cbdc, TypeCB, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(digital_euro_cbdc, TypeCommB, context(agent_power(institutional), _, exit_options(constrained), _)),
    TypeCB \= TypeCommB. % Rope vs Tangled Rope

:- end_tests(digital_euro_cbdc_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): This high value reflects the structural capacity for complete transaction
 *     surveillance, the ability to implement negative interest rates directly on holdings, and the potential
 *     for programmable money (e.g., spending limits, expiry dates, restricted use). This represents a
 *     fundamental extraction of financial autonomy and privacy from the end-user.
 *   - Suppression Score (s=0.75): The CBDC, if widely adopted, directly competes with and threatens the
 *     viability of physical cash, the last bastion of anonymous peer-to-peer transactions. Its existence
 *     and promotion by the state actively suppresses this crucial alternative.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the central bank (institutional/arbitrage), the Digital Euro is a Rope: a tool
 *   for coordinating monetary policy, ensuring financial stability, and maintaining sovereignty against
 *   private stablecoins. The extractive features are seen as policy levers. For the citizen (powerless/trapped),
 *   these same features constitute a Snare: an inescapable grid of financial control that eliminates privacy
 *   and exposes them to arbitrary state power (e.g., asset freezes, mandated spending).
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `central_banks_and_governments` gain unprecedented insight and control over the economy.
 *   - Victims: `privacy_conscious_citizens` lose the fundamental right to private transactions. The `commercial_banking_sector`
 *     is also a victim of potential disintermediation, as deposits may flow from them to the central bank. This dual role
 *     for commercial banks is why they are listed in both groups.
 *   The engine uses these declarations to derive directionality (d), correctly identifying the state as a beneficiary (low d)
 *   and the citizen as a victim (high d).
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The ECB and commercial banks are both institutional actors, but experience the constraint differently. The ECB has
 *   `arbitrage` exit; it designed the system and can change the rules. Commercial banks have `constrained` exit; they
 *   must operate within the system the ECB creates. This difference in exit options, combined with their dual beneficiary/victim
 *   status, results in a different derived directionality and a different classification (Rope vs. Tangled Rope),
 *   capturing the nuance of their precarious position.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a canonical example of preventing Mandatrophy. The public narrative is purely about coordination (a Rope).
 *   Without a rigorous framework, the extractive capacity could be ignored. By requiring the declaration of both
 *   `constraint_beneficiary` and `constraint_victim`, the system forces an accounting of the asymmetric costs. The
 *   `requires_active_enforcement` flag further signals that this is not a spontaneously ordered system. The result is
 *   the correct classification of Tangled Rope, acknowledging both the stated coordination goal and the embedded extractive structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_digital_euro_cbdc,
    'Will the Digital Euro be implemented with robust, legally-binding privacy protections (making it a true cash-like alternative), or will these be superficial backdoors for state surveillance?',
    'Analysis of the final legislative text and technical architecture, specifically regarding anonymity thresholds and access rights for state agencies.',
    'If privacy is robust, ε drops significantly (to ~0.20) and the system becomes a Rope. If it is a backdoor, ε remains high (~0.55+) and it is a Tangled Rope/Snare.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(digital_euro_cbdc, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Models the potential "extraction accumulation" as the project moves from concept
% to reality, with initial promises of privacy giving way to expanded control features.
% This is a high-extraction constraint (ε > 0.46), so temporal data is required.

% Theater ratio over time:
narrative_ontology:measurement(digital_euro_cbdc_tr_t0, digital_euro_cbdc, theater_ratio, 0, 0.10).
narrative_ontology:measurement(digital_euro_cbdc_tr_t5, digital_euro_cbdc, theater_ratio, 5, 0.20).
narrative_ontology:measurement(digital_euro_cbdc_tr_t10, digital_euro_cbdc, theater_ratio, 10, 0.25).

% Extraction over time:
narrative_ontology:measurement(digital_euro_cbdc_ex_t0, digital_euro_cbdc, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(digital_euro_cbdc_ex_t5, digital_euro_cbdc, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(digital_euro_cbdc_ex_t10, digital_euro_cbdc, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(digital_euro_cbdc, global_infrastructure).

% Network relationships (structural influence edges)
% The CBDC directly impacts the viability and function of physical cash.
narrative_ontology:affects_constraint(digital_euro_cbdc, physical_cash_anonymity).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */