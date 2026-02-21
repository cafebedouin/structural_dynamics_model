% ============================================================================
% CONSTRAINT STORY: india_france_horizon_2047
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_india_france_horizon_2047, []).

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
 *   constraint_id: india_france_horizon_2047
 *   human_readable: India-France "Horizon 2047" Strategic Partnership
 *   domain: geopolitical
 *
 * SUMMARY:
 *   The "Horizon 2047" agreement is a comprehensive strategic partnership
 *   between India and France, aiming to coordinate action on defense, space,
 *   nuclear energy, AI, and other critical technologies. While presented as a
 *   partnership of equals, it formalizes dependencies and resource flows that
 *   have asymmetric consequences, blending genuine coordination with subtle
 *   extraction.
 *
 * KEY AGENTS (by structural relationship):
 *   - Indian Taxpayers: Primary target (powerless/trapped) — bears the financial cost and potential liabilities (e.g., nuclear) of the large-scale projects.
 *   - French Defense-Aerospace Sector: Primary beneficiary (institutional/arbitrage) — secures a major, long-term market for high-value exports and co-development projects.
 *   - Indian Strategic Establishment: Secondary beneficiary (institutional/constrained) — gains access to advanced technology and geopolitical leverage, but with fewer alternative partners than France.
 *   - Competing Arms Exporters: Secondary target (institutional/mobile) - (e.g., from Russia, US) whose market share is suppressed by this long-term alignment.
 *   - Analytical Observer: Sees the full structure of coordination and asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(india_france_horizon_2047, 0.48).
domain_priors:suppression_score(india_france_horizon_2047, 0.55).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(india_france_horizon_2047, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(india_france_horizon_2047, extractiveness, 0.48).
narrative_ontology:constraint_metric(india_france_horizon_2047, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(india_france_horizon_2047, theater_ratio, 0.30).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(india_france_horizon_2047, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(india_france_horizon_2047). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(india_france_horizon_2047, french_defense_aerospace_sector).
narrative_ontology:constraint_beneficiary(india_france_horizon_2047, indian_strategic_establishment).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(india_france_horizon_2047, indian_taxpayers).
narrative_ontology:constraint_victim(india_france_horizon_2047, competing_arms_exporters).

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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Agent who bears the most extraction (Indian Taxpayers). Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
%   χ = 0.48 * 1.42 * 1.0 (national scope) = 0.68. This meets the snare threshold (>= 0.66).
constraint_indexing:constraint_classification(india_france_horizon_2047, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most (French Defense-Aerospace Sector). Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
%   χ = 0.48 * -0.12 * 1.1 (continental scope) = -0.06. This is a clear Rope.
constraint_indexing:constraint_classification(india_france_horizon_2047, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context. Sees both coordination and extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
%   χ = 0.48 * 1.15 * 1.2 (global scope) = 0.66. At the threshold, but the recognition
%   of a coordination function (via beneficiary declarations) classifies it as Tangled Rope.
constraint_indexing:constraint_classification(india_france_horizon_2047, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES (demonstrating v6.0 logic) ---
% The two state actors have different structural relationships to the constraint,
% primarily due to differing exit options, which the engine captures.

% Perspective 4A: French Strategic Establishment (institutional, arbitrage exit)
% As a major global power and tech exporter, France has many potential partners.
% The engine derives a low `d` (beneficiary + arbitrage), resulting in a Rope classification.
constraint_indexing:constraint_classification(india_france_horizon_2047, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% Perspective 4B: Indian Strategic Establishment (institutional, constrained exit)
% As a technology importer seeking diversification, India's options for partners
% willing to engage in deep tech transfer are more limited.
% The engine derives a higher `d` (beneficiary + constrained), resulting in a
% higher χ value, though still within the Rope classification. The gap is measurable.
constraint_indexing:constraint_classification(india_france_horizon_2047, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(india_france_horizon_2047_tests).

test(perspectival_gap) :-
    % Verify the core perspectival gap between the target and a primary beneficiary.
    constraint_indexing:constraint_classification(india_france_horizon_2047, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(india_france_horizon_2047, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_classification_is_tangled_rope) :-
    % The analytical observer must correctly identify the hybrid nature.
    constraint_indexing:constraint_classification(india_france_horizon_2047, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_gates_pass) :-
    % A Tangled Rope must have beneficiaries, victims, and require enforcement.
    narrative_ontology:constraint_beneficiary(india_france_horizon_2047, _),
    narrative_ontology:constraint_victim(india_france_horizon_2047, _),
    domain_priors:requires_active_enforcement(india_france_horizon_2047).

:- end_tests(india_france_horizon_2047_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): This value was chosen to reflect significant, but not total, asymmetry. While the partnership involves co-development, the history of such deals suggests that terms around intellectual property, liability (especially for nuclear plants), and financial flows often favor the more established technology exporter. It's high enough to tip the powerless perspective into a Snare, reflecting the potential for severe financial burden.
 *   - Suppression (0.55): A long-term, deep strategic alignment like this naturally suppresses alternatives. By committing to French platforms and ecosystems, India's ability to integrate competing systems from the US or Russia is reduced, creating a dependency.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the French defense sector (institutional/arbitrage), this is a pure Rope: a coordination mechanism that opens up a vast, stable market. For the Indian taxpayer (powerless/trapped), it is a Snare: a non-negotiable commitment of public funds to projects with opaque outcomes and potentially immense liabilities, from which they cannot exit. The benefits (national security) are abstract, while the costs are concrete.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the model.
 *   - Beneficiaries: The `french_defense_aerospace_sector` and `indian_strategic_establishment` are declared beneficiaries as they gain direct market access and technological/geopolitical capabilities, respectively.
 *   - Victims: `indian_taxpayers` are victims due to the direct financial burden and risk exposure. `competing_arms_exporters` are victims of the market suppression created by the deal.
 *   This structure ensures that the engine calculates low directionality (d) for beneficiaries and high (d) for victims, producing the observed perspectival gap in effective extraction (χ).
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This is a key feature. Both the Indian and French state actors are `institutional` beneficiaries. However, their structural positions differ. France, as a P5 nation and established arms exporter, has more alternatives (`arbitrage` exit). India, while a major power, has more `constrained` exit options when seeking high-end technology transfer without significant geopolitical strings attached. The Deferential Realism engine captures this nuance by deriving a slightly higher directionality `d` for India, meaning it experiences the partnership as slightly more extractive (or less beneficial) than France does, even though both classify it as a Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This framework prevents mislabeling. A simplistic view might call this a pure Rope (coordination) or a pure Snare (neo-colonial extraction). The Tangled Rope classification from the analytical perspective correctly identifies its dual nature: it possesses a genuine coordination function essential for both nations' strategic goals, BUT it is built upon a structure of asymmetric extraction. This avoids both naive celebration and cynical dismissal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_ifh_2047_ip,
    'Are the technology transfer and IP-sharing clauses genuinely equitable, or do they create long-term technological dependency for India?',
    'Declassification and independent audit of the specific co-development contracts for projects like the AMCA engine or nuclear reactor components.',
    'If equitable, ε is lower (~0.30) and the constraint is closer to a pure Rope. If dependency-creating, ε is higher (~0.55) and the Snare classification becomes more dominant.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(india_france_horizon_2047, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This models a gradual "extraction accumulation" as the initial goodwill
% of the partnership is replaced by the hard realities of contract negotiation
% and implementation over a ten-year conceptual period.

% Theater ratio over time (stable):
narrative_ontology:measurement(ifh_2047_tr_t0, india_france_horizon_2047, theater_ratio, 0, 0.20).
narrative_ontology:measurement(ifh_2047_tr_t5, india_france_horizon_2047, theater_ratio, 5, 0.25).
narrative_ontology:measurement(ifh_2047_tr_t10, india_france_horizon_2047, theater_ratio, 10, 0.30).

% Extraction over time (extraction_accumulation drift):
narrative_ontology:measurement(ifh_2047_ex_t0, india_france_horizon_2047, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ifh_2047_ex_t5, india_france_horizon_2047, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(ifh_2047_ex_t10, india_france_horizon_2047, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This partnership's core function is aligning industrial and R&D resources.
narrative_ontology:coordination_type(india_france_horizon_2047, resource_allocation).

% Network relationships (structural influence edges)
% This agreement structurally affects other potential defense partnerships.
narrative_ontology:affects_constraint(india_france_horizon_2047, india_us_jet_engine_deal).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The standard derivation chain,
% using beneficiary/victim declarations and the different exit_options for
% the institutional actors, correctly models the directionality dynamics.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */