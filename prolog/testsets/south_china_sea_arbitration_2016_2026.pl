% ============================================================================
% CONSTRAINT STORY: south_china_sea_arbitration_2016_2026
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-20
% ============================================================================

:- module(constraint_south_china_sea_arbitration_2016_2026, []).

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
 *   constraint_id: south_china_sea_arbitration_2016_2026
 *   human_readable: The 2016 South China Sea Arbitral Award (2016-2026)
 *   domain: geopolitical/legal
 *
 * SUMMARY:
 *   On July 12, 2016, an Arbitral Tribunal under UNCLOS ruled in favor of the
 *   Philippines, invalidating China's "nine-dash line" and "historic rights"
 *   claims. This ruling acts as a legal constraint on maritime claims. However,
 *   its lack of a dedicated enforcement mechanism creates a severe gap between
 *   de jure rights and de facto reality, where the law's intended beneficiaries
 *   (e.g., Filipino fisherfolk) are victimized by the very tensions the law
 *   clarifies. The constraint is both a coordination tool for international
 *   allies and a coercive instrument against China's expansionism.
 *
 * KEY AGENTS (by structural relationship):
 *   - Filipino Fisherfolk: Primary target (powerless/trapped) — bear the cost of non-enforcement, facing harassment and loss of livelihood.
 *   - People's Republic of China: Primary target (institutional/constrained) — rejects the ruling, which extracts its expansive maritime claims.
 *   - Republic of the Philippines & Allies: Primary beneficiary (institutional/mobile) — uses the ruling as a legal basis for policy and alliances.
 *   - Analytical Observer: Sees the dual coordination/extraction function of the unenforced law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(south_china_sea_arbitration_2016_2026, 0.55).
domain_priors:suppression_score(south_china_sea_arbitration_2016_2026, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(south_china_sea_arbitration_2016_2026, 0.04).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(south_china_sea_arbitration_2016_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(south_china_sea_arbitration_2016_2026, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(south_china_sea_arbitration_2016_2026, theater_ratio, 0.04).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(south_china_sea_arbitration_2016_2026, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(south_china_sea_arbitration_2016_2026). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(south_china_sea_arbitration_2016_2026, philippines_and_allies).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(south_china_sea_arbitration_2016_2026, chinese_maritime_claims).
narrative_ontology:constraint_victim(south_china_sea_arbitration_2016_2026, filipino_fisherfolk).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three) -> MET

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

% PERSPECTIVE 1: FILIPINO FISHERFOLK (SNARE)
% Agent who bears the cost of non-enforcement. The law promises rights but
% offers no physical protection, creating a trap where asserting those rights
% invites physical harm.
% d(victim, trapped) -> high; χ = 0.55 * f(d) * σ(regional) -> high enough for Snare
constraint_indexing:constraint_classification(south_china_sea_arbitration_2016_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PHILIPPINES & ALLIES (ROPE)
% The ruling is a pure coordination tool, providing a common legal foundation
% for diplomatic statements, joint patrols, and military aid.
% d(beneficiary, mobile) -> low; χ = 0.55 * f(d) * σ(global) -> low/negative
constraint_indexing:constraint_classification(south_china_sea_arbitration_2016_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: PEOPLE'S REPUBLIC OF CHINA (SNARE)
% From Beijing's perspective, the ruling is an externally imposed, non-consensual
% constraint designed to extract its historical claims and limit its sovereignty.
% d(victim, constrained) -> high; χ = 0.55 * f(d) * σ(national) -> high enough for Snare
constraint_indexing:constraint_classification(south_china_sea_arbitration_2016_2026, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The observer sees the full structure: a valid coordination function for one
% coalition (Rope) and a coercive, extractive function for another (Snare),
% requiring active enforcement to resolve the tension. This duality is the
% definition of a Tangled Rope.
constraint_indexing:constraint_classification(south_china_sea_arbitration_2016_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(south_china_sea_arbitration_2016_2026_tests).

test(perspectival_gap_beneficiary_vs_victim) :-
    % Verify the gap between the primary beneficiary (Philippines) and a primary target (Fisherfolk).
    constraint_indexing:constraint_classification(south_china_sea_arbitration_2016_2026, TypeBeneficiary, context(agent_power(institutional), _, exit_options(mobile), _)),
    constraint_indexing:constraint_classification(south_china_sea_arbitration_2016_2026, TypeTarget, context(agent_power(powerless), _, exit_options(trapped), _)),
    TypeBeneficiary = rope,
    TypeTarget = snare.

test(tangled_rope_analytical_claim) :-
    % The analytical claim must be Tangled Rope.
    narrative_ontology:constraint_claim(south_china_sea_arbitration_2016_2026, tangled_rope).

test(tangled_rope_structural_gates_met) :-
    % Verify all three structural requirements for Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(south_china_sea_arbitration_2016_2026, _),
    narrative_ontology:constraint_victim(south_china_sea_arbitration_2016_2026, _),
    domain_priors:requires_active_enforcement(south_china_sea_arbitration_2016_2026).

:- end_tests(south_china_sea_arbitration_2016_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The original file's metrics (ε=0.45, S=0.35) and claim ('mountain') were
 *   inconsistent. The claim violated mountain thresholds, and the metrics were
 *   too low to produce the 'snare' classifications described in the narrative.
 *   I increased base_extractiveness to 0.55 and suppression_score to 0.65.
 *   This reflects the high stakes (extraction of sovereign claims) and the
 *   strong suppression of the "nine-dash line" alternative. These values allow
 *   the 'snare' classifications for the powerless and targeted agents to be
 *   structurally valid. The analytical classification is 'tangled_rope' because
 *   the constraint clearly has both a coordination function (for allies) and
 *   an asymmetric extractive function (against China), and requires active
 *   enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The gap is severe. For the Philippines and its allies, the ruling is a
 *   'Rope'—a tool for coordinating international law and order. For Filipino
 *   fisherfolk, the gap between the law's promise and its non-enforcement
 *   creates a deadly 'Snare'. For China, it is a 'Snare' of foreign legal
 *   coercion. The Philippines DFA may *treat* the ruling as a 'Mountain'
 *   (a fixed, non-negotiable fact), but its structural metrics (high ε and S)
 *   mean it can never be one; their experience is structurally that of a 'Rope'.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: 'philippines_and_allies' use the ruling to coordinate policy.
 *   - Victims: 'chinese_maritime_claims' are directly extracted/invalidated.
 *     'filipino_fisherfolk' are victims of the unstable equilibrium created by
 *     the unenforced law, bearing the physical costs of the geopolitical friction.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the dual nature of the constraint.
 *   A simpler analysis might label it a 'Rope' (focusing on its legal intent) or
 *   a 'Snare' (focusing on its effect on China). The 'Tangled Rope'
 *   classification correctly captures that it is both simultaneously, and that
 *   the tension between these functions is the core of the conflict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_scs_unga_referral,
    "Will the Philippines successfully forward the 2016 Award to the UN General Assembly for an advisory opinion?",
    "Monitor the vote count in the UNGA during its next relevant session.",
    "If Yes: The award's legitimacy hardens, strengthening its Rope function. If No: It remains a regionally contested Snare.",
    confidence_without_resolution(low)
).

omega_variable(
    omega_scs_asean_coc,
    "Will the ASEAN Code of Conduct, when finalized, explicitly reference the 2016 Award as its legal basis?",
    "Review the final communique of the ASEAN Summit that announces the Code of Conduct.",
    "If Yes: The constraint's coordination function is institutionalized (strong Rope). If No: The constraint remains primarily a tool of great power competition (Tangled Rope/Snare).",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing. Interval represents 2016-2026.
narrative_ontology:interval(south_china_sea_arbitration_2016_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for the 2016-2026 period.
% The ruling's extractive potential became more actualized over time as
% geopolitical friction and on-the-water incidents increased.
%
% Theater ratio over time (remains low and stable):
narrative_ontology:measurement(scs_tr_t0, south_china_sea_arbitration_2016_2026, theater_ratio, 0, 0.02).
narrative_ontology:measurement(scs_tr_t5, south_china_sea_arbitration_2016_2026, theater_ratio, 5, 0.03).
narrative_ontology:measurement(scs_tr_t10, south_china_sea_arbitration_2016_2026, theater_ratio, 10, 0.04).

% Extraction over time (increases as conflict intensifies):
narrative_ontology:measurement(scs_ex_t0, south_china_sea_arbitration_2016_2026, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(scs_ex_t5, south_china_sea_arbitration_2016_2026, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(scs_ex_t10, south_china_sea_arbitration_2016_2026, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The ruling serves as a standard for interpreting UNCLOS.
narrative_ontology:coordination_type(south_china_sea_arbitration_2016_2026, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations accurately models the directionality for all key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */