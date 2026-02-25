% ============================================================================
% CONSTRAINT STORY: bonbon_drop_sticker_craze
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_bonbon_drop_sticker_craze, []).

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
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bonbon_drop_sticker_craze
 *   human_readable: UHA Mikakuto's "Bonbon Drop" Collectible Sticker Promotion
 *   domain: economic
 *
 * SUMMARY:
 *   The Japanese candy company UHA Mikakuto packages its "Bonbon Drop" candy
 *   with collectible stickers featuring popular characters. The stickers are
 *   distributed randomly, with certain designs being extremely rare. This has
 *   created a speculative frenzy, with consumers buying large quantities of
 *   the candy solely to acquire the stickers, driving a secondary market
 *   where rare stickers sell for hundreds of times the candy's price.
 *
 * KEY AGENTS (by structural relationship):
 *   - Sticker Collectors: Primary target (powerless/trapped) — bear the cost of the chase, buying unwanted products for a chance at a rare item.
 *   - UHA Mikakuto (Manufacturer): Primary beneficiary (institutional/arbitrage) — benefits from massively inflated sales and brand visibility.
 *   - Retailers (e.g., Don Quijote): Secondary beneficiary (organized/constrained) — benefit from increased sales and foot traffic, but have less control than the manufacturer.
 *   - Analytical Observer: Analytical observer — sees the full structure of the engineered scarcity and value extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bonbon_drop_sticker_craze, 0.75).
domain_priors:suppression_score(bonbon_drop_sticker_craze, 0.80).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(bonbon_drop_sticker_craze, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bonbon_drop_sticker_craze, extractiveness, 0.75).
narrative_ontology:constraint_metric(bonbon_drop_sticker_craze, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(bonbon_drop_sticker_craze, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(bonbon_drop_sticker_craze, snare).

% --- Binary flags ---
domain_priors:requires_active_enforcement(bonbon_drop_sticker_craze). % The company must actively manage rarity ratios.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(bonbon_drop_sticker_craze, uha_mikakuto).
narrative_ontology:constraint_beneficiary(bonbon_drop_sticker_craze, retailers).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(bonbon_drop_sticker_craze, sticker_collectors).

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
%   χ = 0.75 * f(0.95) * σ(national) ≈ 0.75 * 1.42 * 1.0 ≈ 1.065
constraint_indexing:constraint_classification(bonbon_drop_sticker_craze, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
%   χ = 0.75 * f(0.05) * σ(national) ≈ 0.75 * -0.12 * 1.0 ≈ -0.09
constraint_indexing:constraint_classification(bonbon_drop_sticker_craze, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% Default analytical context. Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
%   χ = 0.75 * f(0.72) * σ(global) ≈ 0.75 * 1.15 * 1.2 ≈ 1.035
constraint_indexing:constraint_classification(bonbon_drop_sticker_craze, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---

% PERSPECTIVE 4: THE RETAILERS (ROPE)
% Beneficiaries, but with less power and control than the manufacturer. Their exit is
% constrained by competitive pressure. The engine derives a higher d than the
% primary beneficiary, but still well below the symmetric point.
% A beneficiary with 'constrained' exit implies d ≈ 0.30 -> f(d) ≈ 0.20
%   χ = 0.75 * f(0.30) * σ(national) ≈ 0.75 * 0.20 * 1.0 ≈ 0.15
constraint_indexing:constraint_classification(bonbon_drop_sticker_craze, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bonbon_drop_sticker_craze_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target (collector) and beneficiary (manufacturer).
    constraint_indexing:constraint_classification(bonbon_drop_sticker_craze, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bonbon_drop_sticker_craze, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(snare_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(bonbon_drop_sticker_craze, ExtMetricName, E),
    E >= 0.46,
    config:param(suppression_metric_name, SupMetricName),
    narrative_ontology:constraint_metric(bonbon_drop_sticker_craze, SupMetricName, S),
    S >= 0.60.

:- end_tests(bonbon_drop_sticker_craze_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.75): Extremely high. The value is not in the
 *     primary product (candy) but in the secondary, randomized item (sticker).
 *     The vast difference between the product cost (~¥250) and the secondary
 *     market value of rare items (up to ¥100,000) demonstrates massive value
 *     extraction from consumer desire, channeled to the manufacturer through
 *     repeat purchases. This is a classic "gacha" or "loot box" mechanic.
 *   - Suppression Score (0.80): High. The manufacturer is the sole official
 *     source of the stickers. There are no alternative legitimate channels;
 *     participation requires buying the bundled product, effectively
 *     suppressing all other means of acquisition and forcing engagement with
 *     the extractive lottery.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. From the perspective of UHA Mikakuto (institutional),
 *   this is a Rope: a brilliant coordination mechanism to align consumer
 *   desire with corporate profit, resulting in a net subsidy (negative χ).
 *   For the dedicated sticker collector (powerless, trapped), it is a Snare: a
 *   coercive and expensive trap that leverages completionist psychology to
 *   extract significant financial resources for minimal tangible return.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint is highly directional. Value flows from the `sticker_collectors`
 *   (the victim group) to `uha_mikakuto` (the primary beneficiary). The
 *   mechanism is designed to obscure this flow by bundling the extraction
 *   vehicle (the sticker lottery) with a low-cost consumable good. Retailers
 *   are secondary beneficiaries, profiting from the increased sales volume but
 *   having less control over the system's core parameters (rarity, distribution).
 *   The model considers whether the `powerless` collectors could achieve
 *   `organized` status via coalition. While possible (e.g., through boycotts
 *   or collective trading pacts), the current narrative shows them acting as
 *   competing individuals, which maintains their `powerless` status.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY] This case is a powerful example of how a commercial
 *   activity, framed as "fun" or "collecting," can be structurally identical
 *   to a Snare. An analysis that ignores directionality might misclassify this
 *   as a Rope (coordinating a hobby) or a Tangled Rope. However, the lack of
 *   a genuine, primary coordination function (the community is a side effect,
 *   not the goal) and the sheer scale of the asymmetric extraction (high ε)
 *   firmly place it in the Snare category from any non-beneficiary perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_bonbon_drop_sticker_craze,
    'Is the collector behavior primarily driven by voluntary fun or by coercive, addiction-like psychological mechanisms inherent in gacha systems?',
    'Longitudinal psychological studies of collector cohorts, measuring self-reported enjoyment versus stress, financial strain, and compulsion.',
    'If voluntary fun, the constraint might be better modeled as a Tangled Rope (a coordination game with high extraction). If coercive, the Snare classification is correct.',
    confidence_without_resolution(high) % Confidence in the Snare classification is high given the structural parallels to gambling.
).

% /3 form: typed classification for reporting engine (REQUIRED)
% The reporting engine reads narrative_ontology:omega_variable/3 with structure
% (ID, TypeClass, Description) where TypeClass is one of:
%   empirical   — resolvable by gathering more data
%   conceptual  — depends on definitional or theoretical framing
%   preference  — depends on value judgments or policy choices
% The /3 form is what the engine reads; /5 provides narrative context.
narrative_ontology:omega_variable(omega_bonbon_drop_sticker_craze, conceptual, 'Is collector behavior voluntary fun or driven by coercive gacha mechanics?').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(bonbon_drop_sticker_craze, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. This craze intensified over time.
% Required because base_extractiveness (0.75) > 0.46.

% Theater ratio over time (stable and low):
narrative_ontology:measurement(bdsc_tr_t0, bonbon_drop_sticker_craze, theater_ratio, 0, 0.10).
narrative_ontology:measurement(bdsc_tr_t5, bonbon_drop_sticker_craze, theater_ratio, 5, 0.15).
narrative_ontology:measurement(bdsc_tr_t10, bonbon_drop_sticker_craze, theater_ratio, 10, 0.20).

% Extraction over time (intensified as the craze grew):
narrative_ontology:measurement(bdsc_ex_t0, bonbon_drop_sticker_craze, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(bdsc_ex_t5, bonbon_drop_sticker_craze, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(bdsc_ex_t10, bonbon_drop_sticker_craze, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% This constraint is primarily extractive, not coordinative. Therefore,
% coordination_type and boltzmann_floor_override are not applicable.
% No clear network relationships to other constraints in the corpus are
% immediately apparent, though it belongs to a class of "gacha mechanics"
% which could be linked in a broader analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The automatic directionality
% derivation from beneficiary/victim declarations and exit options accurately
% models the structural relationships between the manufacturer, retailers,
% and consumers.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */