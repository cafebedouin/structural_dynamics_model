% ============================================================================
% CONSTRAINT STORY: jp_eez_enforcement
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_jp_eez_enforcement, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jp_eez_enforcement
 *   human_readable: Enforcement of Japan's Claimed Exclusive Economic Zone (EEZ)
 *   domain: geopolitical
 *
 * SUMMARY:
 *   This constraint represents the active enforcement of Japan's claimed
 *   Exclusive Economic Zone (EEZ) around the Senkaku/Diaoyu Islands. The
 *   Japanese Coast Guard patrols these waters and seizes foreign fishing
 *   vessels, primarily Chinese, that it deems to be operating illegally.
 *   This enforcement is a mechanism for resource control and a physical
 *   assertion of a disputed sovereignty claim.
 *
 * KEY AGENTS (by structural relationship):
 *   - Chinese Fishing Crews: Primary target (powerless/trapped) — bear the direct cost of seizure (loss of catch, boat, freedom).
 *   - Japanese State & Domestic Fishing Industry: Primary beneficiary (institutional/arbitrage) — gain exclusive access to marine resources.
 *   - Chinese State: Contesting institutional actor (institutional/constrained) — views the enforcement as illegitimate and a violation of its own claims.
 *   - Analytical Observer: Analytical observer — sees the dual function of resource coordination and coercive geopolitical signaling.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jp_eez_enforcement, 0.55). % Value of denied fishing access.
domain_priors:suppression_score(jp_eez_enforcement, 0.80).   % Structural property (raw, unscaled). Backed by armed coast guard vessels.
domain_priors:theater_ratio(jp_eez_enforcement, 0.10).       % Piton detection (>= 0.70). Seizures are functional, not performative.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jp_eez_enforcement, extractiveness, 0.55).
narrative_ontology:constraint_metric(jp_eez_enforcement, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(jp_eez_enforcement, theater_ratio, 0.10).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(jp_eez_enforcement, tangled_rope).
narrative_ontology:human_readable(jp_eez_enforcement, "Enforcement of Japan's Claimed Exclusive Economic Zone (EEZ)").

% --- Binary flags ---
domain_priors:requires_active_enforcement(jp_eez_enforcement). % Required for Tangled Rope. The constraint is meaningless without JCG patrols.

% --- Emergence flag (required for mountain constraints) ---
% N/A for this constraint.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(jp_eez_enforcement, japanese_state_and_fishing_industry).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(jp_eez_enforcement, chinese_fishing_crews).
narrative_ontology:constraint_victim(jp_eez_enforcement, chinese_state).

% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three are met)

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

% PERSPECTIVE 1: THE PRIMARY TARGET (CHINESE FISHING CREW)
% Experiences this as pure, coercive extraction.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ.
constraint_indexing:constraint_classification(jp_eez_enforcement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (JAPANESE STATE)
% Views this as a legitimate resource management and sovereignty protection mechanism.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ.
constraint_indexing:constraint_classification(jp_eez_enforcement, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Recognizes the dual nature: a genuine coordination function (for Japan)
% combined with high coercive extraction (against others).
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15.
constraint_indexing:constraint_classification(jp_eez_enforcement, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---
% The Chinese state, as a contesting institution, sees this differently from
% both the powerless fishermen and the benefiting Japanese state.

% PERSPECTIVE 4: THE CONTESTING INSTITUTION (CHINESE STATE)
% Views the constraint as an illegitimate Snare. It's a victim, but with far
% more power and different exit options than the fishing crews.
% Engine derives d from: victim membership + constrained exit → d ≈ 0.70.
constraint_indexing:constraint_classification(jp_eez_enforcement, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jp_eez_enforcement_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify perspectival gap between the powerless target and institutional beneficiary.
    constraint_indexing:constraint_classification(jp_eez_enforcement, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(jp_eez_enforcement, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(inter_institutional_gap) :-
    % Verify that the two institutional actors have different classifications/experiences.
    constraint_indexing:constraint_classification(jp_eez_enforcement, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(jp_eez_enforcement, snare, context(agent_power(institutional), _, exit_options(constrained), _)).

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(jp_eez_enforcement, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_compliance) :-
    narrative_ontology:constraint_beneficiary(jp_eez_enforcement, _),
    narrative_ontology:constraint_victim(jp_eez_enforcement, _),
    domain_priors:requires_active_enforcement(jp_eez_enforcement).

:- end_tests(jp_eez_enforcement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): Represents the significant economic value of the fishing grounds that are denied to Chinese fishermen and reserved for Japanese use.
 *   - Suppression (0.80): Very high, as the constraint is enforced by quasi-military assets (coast guard cutters) capable of intercepting, boarding, and seizing vessels. Alternatives are physically suppressed.
 *   - Theater (0.10): Low. The enforcement actions are functional and consequential, not merely symbolic posturing.
 *   The combination of a coordination function (resource management for Japan) and high, coercive extraction (from the perspective of China) makes this a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound and rooted in the unresolved sovereignty dispute.
 *   - For the Chinese fishing crew (powerless/trapped), it is a pure Snare. They see only the coercive force that seizes their livelihood, with no corresponding benefit.
 *   - For the Japanese state (institutional/arbitrage), it is a pure Rope. It's a legitimate tool for managing national resources and defending sovereign territory, seen as a necessary coordination function.
 *   - The analytical view acknowledges both realities, hence Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from clear structural relationships:
 *   - Beneficiary: `japanese_state_and_fishing_industry`. They directly benefit from the exclusion of competitors. This declaration, combined with their `arbitrage` exit (they set the rules), drives their `d` value close to 0, yielding a low/negative effective extraction (χ) and a Rope classification.
 *   - Victim: `chinese_fishing_crews` and `chinese_state`. They bear the costs. For the crew, `trapped` exit drives their `d` value near 1.0, yielding high χ and a Snare classification. For the state, `constrained` exit yields a moderately high `d`, also resulting in a Snare classification from its institutional viewpoint.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This story highlights a key inter-institutional conflict. Both Japan and China are `institutional` actors, but their relationship to the constraint is asymmetric.
 *   - Japan has `arbitrage` exit: it defines and enforces the rules in this domain.
 *   - China has `constrained` exit: it cannot simply ignore the enforcement without risking escalation, nor can it easily change the rule. Its options are diplomatic protest, counter-patrols (which are costly and risky), or tacit acceptance.
 *   The framework captures this asymmetry through the different `exit_options`, leading to different derived `d` values and, consequently, different classifications (Rope vs. Snare) even for actors with the same `institutional` power level.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two potential errors. It does not label the constraint a pure Snare, which would ignore the genuine (from Japan's perspective) coordination function of resource management. It also does not label it a pure Rope, which would ignore the high degree of coercive, asymmetric extraction imposed on non-consenting parties. The Tangled Rope classification captures this essential duality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_jp_eez_enforcement,
    'Is the enforcement of the EEZ primarily a resource-management action (coordination) or a geopolitical assertion of sovereignty (extraction)?',
    'Declassification of internal policy documents from both governments detailing the strategic intent behind the patrols.',
    'If primarily resource management, the coordination function is stronger, leaning more towards Rope. If primarily geopolitical, the extractive/coercive function is dominant, leaning more towards Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(jp_eez_enforcement, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint has intensified over the modeled 10-year interval as
% geopolitical tensions have risen, leading to more systematic enforcement.
% This is modeled as a slight increase in base extractiveness.
% Theater ratio remains consistently low as enforcement is functional.
% Required because base_extractiveness (0.55) > 0.46.

% Theater ratio over time (stable and low):
narrative_ontology:measurement(jp_eez_enforcement_tr_t0, jp_eez_enforcement, theater_ratio, 0, 0.10).
narrative_ontology:measurement(jp_eez_enforcement_tr_t5, jp_eez_enforcement, theater_ratio, 5, 0.10).
narrative_ontology:measurement(jp_eez_enforcement_tr_t10, jp_eez_enforcement, theater_ratio, 10, 0.10).

% Extraction over time (slight intensification):
narrative_ontology:measurement(jp_eez_enforcement_ex_t0, jp_eez_enforcement, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(jp_eez_enforcement_ex_t5, jp_eez_enforcement, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(jp_eez_enforcement_ex_t10, jp_eez_enforcement, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This constraint's coordination function is managing a natural resource.
narrative_ontology:coordination_type(jp_eez_enforcement, resource_allocation).

% Network relationships (structural influence edges)
% This specific enforcement action is a downstream consequence of the larger,
% unresolved sovereignty dispute over the islands themselves.
narrative_ontology:affects_constraint(senkaku_diaoyu_sovereignty_dispute, jp_eez_enforcement).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation
% chain (beneficiary/victim declarations + exit_options) accurately models
% the asymmetric relationships between the Chinese fishing crews, the Chinese
% state, and the Japanese state, producing the correct directionality values.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */