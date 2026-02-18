% ============================================================================
% CONSTRAINT STORY: tiktok_us_divestiture_mandate
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_tiktok_us_divestiture_mandate, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: tiktok_us_divestiture_mandate
 *   human_readable: "TikTok Trust & Safety" Divestiture Mandate
 *   domain: geopolitical/technological/economic
 *
 * SUMMARY:
 *   Following persistent national security concerns regarding data privacy and
 *   potential foreign influence, the US Government mandates that TikTok's parent
 *   company, ByteDance, must sell its US operations to a US-controlled entity.
 *   This constraint is the regulatory framework forcing the sale and governing
 *   the new entity's operations to ensure compliance with US law, effectively
 *   severing operational control from its Chinese parent.
 *
 * KEY AGENTS (by structural relationship):
 *   - US TikTok Users: Primary target (powerless/trapped) — their data & attention continue to be monetized, now by a US entity.
 *   - ByteDance (China): Primary target (institutional/constrained) — forced to divest a highly valuable asset under coercive pressure.
 *   - US Government (CFIUS): Primary beneficiary (institutional/arbitrage) — achieves national security objectives and asserts regulatory sovereignty.
 *   - US Tech Acquirer: Primary beneficiary (institutional/arbitrage) — acquires a massive social media asset with a captive user base.
 *   - Analytical Observer: Analytical observer — sees the dual function of national security coordination and coercive economic extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tiktok_us_divestiture_mandate, 0.48).
domain_priors:suppression_score(tiktok_us_divestiture_mandate, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(tiktok_us_divestiture_mandate, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tiktok_us_divestiture_mandate, extractiveness, 0.48).
narrative_ontology:constraint_metric(tiktok_us_divestiture_mandate, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(tiktok_us_divestiture_mandate, theater_ratio, 0.30).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this human-constructed constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(tiktok_us_divestiture_mandate, tangled_rope).
narrative_ontology:topic_domain(tiktok_us_divestiture_mandate, "geopolitical/technological/economic").
narrative_ontology:human_readable(tiktok_us_divestiture_mandate, "\"TikTok Trust & Safety\" Divestiture Mandate").

% --- Binary flags ---
domain_priors:requires_active_enforcement(tiktok_us_divestiture_mandate). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(tiktok_us_divestiture_mandate, us_government).
narrative_ontology:constraint_beneficiary(tiktok_us_divestiture_mandate, us_tech_acquirer).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(tiktok_us_divestiture_mandate, bytedance_china).
narrative_ontology:constraint_victim(tiktok_us_divestiture_mandate, us_tiktok_users).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three are present)

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

% PERSPECTIVE 1: THE US TIKTOK USER (SNARE)
% Victim group, trapped by network effects. Engine derives d ≈ 0.95 → f(d) ≈ 1.42.
% χ = 0.48 * 1.42 * 1.0 (national) ≈ 0.68. This exceeds the Snare threshold (χ ≥ 0.66).
% The coordination benefit (national security) is abstract, while the extraction
% of data/attention is immediate and tangible.
constraint_indexing:constraint_classification(tiktok_us_divestiture_mandate, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE US GOVERNMENT (ROPE)
% Beneficiary group, has arbitrage over regulatory tools. Engine derives d ≈ 0.05 → f(d) ≈ -0.12.
% χ = 0.48 * -0.12 * 1.0 ≈ -0.06. Negative extraction signifies a subsidy.
% For the government, this is a pure coordination mechanism to align corporate
% behavior with national security policy.
constraint_indexing:constraint_classification(tiktok_us_divestiture_mandate, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Sees both the coordination function and the asymmetric extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15. Global scope amplifies χ.
% χ = 0.48 * 1.15 * 1.2 (global) ≈ 0.66. This fits Tangled Rope (0.40 ≤ χ ≤ 0.90).
% Base metrics (ε=0.48, suppression=0.75) and structural data are present,
% correctly identifying the hybrid nature of the constraint.
constraint_indexing:constraint_classification(tiktok_us_divestiture_mandate, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---

% PERSPECTIVE 4A: BYTEDANCE (TANGLED ROPE)
% An institutional victim with constrained exit. Engine derives d ≈ 0.80 → f(d) ≈ 1.15.
% χ = 0.48 * 1.15 * 1.0 (national) ≈ 0.55. This does not meet the Snare threshold (χ ≥ 0.66).
% It is not a pure snare for ByteDance because they receive compensation and
% avoid a total loss, but it is a highly extractive, coercive arrangement. This
% classifies correctly as a Tangled Rope.
constraint_indexing:constraint_classification(tiktok_us_divestiture_mandate, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4B: US TECH ACQUIRER (ROPE)
% An institutional beneficiary with arbitrage exit. Same as US Govt perspective.
constraint_indexing:constraint_classification(tiktok_us_divestiture_mandate, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tiktok_us_divestiture_mandate_tests).

test(perspectival_gap_user_vs_govt, [nondet]) :-
    constraint_indexing:constraint_classification(tiktok_us_divestiture_mandate, TypeUser, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(tiktok_us_divestiture_mandate, TypeGovt, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    assertion(TypeUser == snare),
    assertion(TypeGovt == rope),
    assertion(TypeUser \= TypeGovt).

test(analytical_is_tangled_rope, [nondet]) :-
    constraint_indexing:constraint_classification(tiktok_us_divestiture_mandate, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    assertion(TypeAnalytical == tangled_rope).

test(inter_institutional_gap_bytedance_vs_acquirer, [nondet]) :-
    constraint_indexing:constraint_classification(tiktok_us_divestiture_mandate, TypeByteDance, context(agent_power(institutional), _, exit_options(constrained), _)),
    constraint_indexing:constraint_classification(tiktok_us_divestiture_mandate, TypeAcquirer, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    assertion(TypeByteDance == tangled_rope),
    assertion(TypeAcquirer == rope),
    assertion(TypeByteDance \= TypeAcquirer).

test(tangled_rope_gate_check) :-
    narrative_ontology:constraint_beneficiary(tiktok_us_divestiture_mandate, _),
    narrative_ontology:constraint_victim(tiktok_us_divestiture_mandate, _),
    domain_priors:requires_active_enforcement(tiktok_us_divestiture_mandate).

:- end_tests(tiktok_us_divestiture_mandate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): This high value reflects the coercive nature of the asset transfer from ByteDance and the ongoing monetization of the US user base. It's not maximal extraction (ε=1.0) because ByteDance receives compensation, but it's far from a voluntary market transaction.
 *   - Suppression (0.75): The US government's threat of a total market ban is a highly effective tool for suppressing ByteDance's alternatives. Users are suppressed by high network-effect switching costs.
 *   - Theater (0.30): While there is political signaling, the constraint has concrete, structural effects (asset sale, data governance changes), making its functional component dominant.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the US government and the acquirer, this is a Rope—a clean solution to a complex national security and economic problem. For the US user, who experiences no tangible benefit but whose data is still harvested, it's a Snare. For ByteDance, it's a coercive but not completely extractive deal—a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is driven by the flow of power and value. The beneficiaries (US Govt, US acquirer) designed the constraint to their advantage, giving them arbitrage exit options and resulting in a low `d` value (directionality). The victims (ByteDance, US users) are structurally targeted, facing constrained or trapped exit options, which results in a high `d` value and thus high effective extraction (χ).
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This is a classic inter-institutional case. Both ByteDance and the US Government are 'institutional' actors, but their relationship to the constraint is opposite. The model captures this not by changing the power atom, but by recognizing their different exit options (`constrained` vs. `arbitrage`) and beneficiary/victim status. This correctly generates different classifications (Tangled Rope vs. Rope) from two institutional perspectives, quantifying the power imbalance in the arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint could be easily misidentified. A purely political analysis might call it a Snare (economic warfare), while a purely governmental one might call it a Rope (good governance). The Deferential Realism framework, by requiring an analytical perspective that accounts for both the genuine coordination function (national security) and the asymmetric extraction (forced sale), correctly identifies it as a Tangled Rope. This avoids both cynical reductionism and naive acceptance of the official narrative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_tiktok_us_divestiture_mandate,
    'Is the national security threat a genuine, severe risk, or primarily a pretext for economic protectionism?',
    'Declassification of intelligence reports detailing specific data exfiltration vectors and their demonstrated impact on national security.',
    'If threat is genuine, the coordination aspect is primary (Rope-like Tangled Rope). If pretextual, extraction is primary (Snare-like Tangled Rope).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(tiktok_us_divestiture_mandate, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Required for high-extraction constraints (base_extractiveness > 0.46).
% This models a scenario where the new US entity, after an initial period of
% careful compliance, gradually optimizes for monetization, increasing
% extraction. Theater also rises slightly as compliance becomes more performative.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(tt_divest_tr_t0, tiktok_us_divestiture_mandate, theater_ratio, 0, 0.15).
narrative_ontology:measurement(tt_divest_tr_t5, tiktok_us_divestiture_mandate, theater_ratio, 5, 0.25).
narrative_ontology:measurement(tt_divest_tr_t10, tiktok_us_divestiture_mandate, theater_ratio, 10, 0.30).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(tt_divest_ex_t0, tiktok_us_divestiture_mandate, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(tt_divest_ex_t5, tiktok_us_divestiture_mandate, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(tt_divest_ex_t10, tiktok_us_divestiture_mandate, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This mandate is a mechanism for enforcing a geopolitical policy.
narrative_ontology:coordination_type(tiktok_us_divestiture_mandate, enforcement_mechanism).

% Network relationships (structural influence edges)
% This constraint is a key component of a broader geopolitical strategy.
narrative_ontology:affects_constraint(tiktok_us_divestiture_mandate, us_china_tech_decoupling).
narrative_ontology:affects_constraint(tiktok_us_divestiture_mandate, social_media_platform_governance).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this story. The structural derivation chain
% (beneficiary/victim declarations + exit_options) accurately computes the
% directionality for all key agents, correctly distinguishing between the
% different institutional actors.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */