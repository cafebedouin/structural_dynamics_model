% ============================================================================
% CONSTRAINT STORY: roc_african_exarchate
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_roc_african_exarchate, []).

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
 *   constraint_id: roc_african_exarchate
 *   human_readable: "Russian Orthodox Church's African Exarchate as a Geopolitical Tool"
 *   domain: geopolitical/religious
 *
 * SUMMARY:
 *   This constraint describes the use of the Russian Orthodox Church (ROC),
 *   specifically its African Exarchate, as an instrument of Russian state
 *   soft power. By creating a parallel religious structure and offering
 *   financial incentives to African clergy, the Kremlin co-opts local
 *   religious communities to generate geopolitical influence, undermine
 *   rival patriarchal authority (specifically Alexandria), and legitimize
 *   its presence on the continent. The system extracts political loyalty
 *   and alignment under the guise of religious solidarity and support.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Kremlin / Russian State: Primary beneficiary (institutional/arbitrage) — gains soft power and geopolitical leverage.
 *   - African Orthodox Clergy & Communities: Primary target (powerless/trapped) — face a coerced choice between loyalty to their traditional Patriarchate and accepting resources from the ROC, leading to a schism.
 *   - Patriarchate of Alexandria: Secondary target (institutional/constrained) — its religious authority and flock are directly challenged and eroded by a state-backed competitor.
 *   - Russian Orthodox Church (ROC): Secondary beneficiary / enforcer (institutional/arbitrage) — gains global influence and state relevance by executing the geopolitical strategy.
 *   - Analytical Observer: Sees the full structure of religious instrumentalization for statecraft.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(roc_african_exarchate, 0.68). % High extraction of loyalty/alignment for low material cost.
domain_priors:suppression_score(roc_african_exarchate, 0.75).   % Actively suppresses the alternative (loyalty to Alexandria). Structural property (raw, unscaled).
domain_priors:theater_ratio(roc_african_exarchate, 0.55).       % Significant religious theater, but a very real functional/extractive purpose. Not a Piton.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(roc_african_exarchate, extractiveness, 0.68).
narrative_ontology:constraint_metric(roc_african_exarchate, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(roc_african_exarchate, theater_ratio, 0.55).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(roc_african_exarchate, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(roc_african_exarchate). % Required for Tangled Rope. Needs active funding and political support.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(roc_african_exarchate, kremlin_geopolitical_actors).
narrative_ontology:constraint_beneficiary(roc_african_exarchate, roc_hierarchy).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(roc_african_exarchate, african_orthodox_communities).
narrative_ontology:constraint_victim(roc_african_exarchate, patriarchate_of_alexandria).

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
% African Orthodox Communities facing the schism. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(roc_african_exarchate, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The Kremlin. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
constraint_indexing:constraint_classification(roc_african_exarchate, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Sees both the coordination function and the asymmetric extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15.
constraint_indexing:constraint_classification(roc_african_exarchate, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---
% The conflict between the two religious institutions.

% Perspective 4A: The Patriarchate of Alexandria (Targeted Institution)
% Experiences the constraint as a coercive, extractive attack.
% Engine derives d from victim status + constrained exit → high d → high χ.
constraint_indexing:constraint_classification(roc_african_exarchate, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained), % Cannot easily abandon their historical flock.
            spatial_scope(continental))).

% Perspective 4B: The Russian Orthodox Church (Enforcing Institution)
% Experiences the constraint as an effective tool for coordination and expansion.
% Engine derives d from beneficiary status + arbitrage exit → low d → low/negative χ.
constraint_indexing:constraint_classification(roc_african_exarchate, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage), % Can scale operation based on state needs.
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(roc_african_exarchate_tests).

test(perspectival_gap) :-
    % Verify gap between target (powerless), beneficiary (institutional), and analytical.
    constraint_indexing:constraint_classification(roc_african_exarchate, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(roc_african_exarchate, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(roc_african_exarchate, tangled_rope, context(agent_power(analytical), _, _, _)).

test(inter_institutional_gap) :-
    % Verify gap between the two institutional actors.
    constraint_indexing:constraint_classification(roc_african_exarchate, snare, context(agent_power(institutional), _, exit_options(constrained), _)),
    constraint_indexing:constraint_classification(roc_african_exarchate, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(tangled_rope_gate_compliance) :-
    % Verify all conditions for a Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(roc_african_exarchate, _),
    narrative_ontology:constraint_victim(roc_african_exarchate, _),
    domain_priors:requires_active_enforcement(roc_african_exarchate).

:- end_tests(roc_african_exarchate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.68): High. The value of geopolitical influence,
 *     soft power, and strategic partnerships gained by the Russian state is
 *     far greater than the cost of the stipends and infrastructure provided.
 *     It is a highly efficient conversion of religious affinity into state power.
 *   - Suppression (0.75): High. The exarchate's very existence is a direct
 *     challenge to the authority of the Patriarchate of Alexandria. Its goal is
 *     to suppress the alternative by poaching clergy and creating a schism,
 *     making it the only viable patron for its members. Suppression is a raw
 *     structural score, not scaled by perspective.
 *   - Classification: The analytical view is Tangled Rope because the constraint
 *     has both a genuine coordination function (organizing and funding a network
 *     of pro-Russian clergy) and a clear, asymmetrically extractive purpose
 *     (leveraging that network for statecraft).
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the Kremlin (beneficiary), this is a "Rope" — an
 *   efficient tool to coordinate assets and achieve strategic goals with low
 *   overhead. For the African communities and clergy (target), it is a "Snare"
 *   — a coercive offer that leverages their economic precarity to trap them
 *   in a geopolitical conflict, forcing them to break with their historical
 *   religious authorities.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `kremlin_geopolitical_actors` and the `roc_hierarchy`. The
 *     engine computes a low directionality (d) for them, leading to a low or
 *     negative effective extraction (χ), classifying the constraint as a Rope.
 *   - Victims: `african_orthodox_communities` and the `patriarchate_of_alexandria`.
 *     The engine computes a high d for them, leading to a high χ, classifying it
 *     as a Snare. This reflects the reality that the costs (schism, loss of
-    *     autonomy, instrumentalization) are borne by the targets.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The model captures the asymmetric conflict between the two religious bodies.
 *   Both are `institutional` actors, but their relationship to the constraint
 *   is opposite. The ROC has `arbitrage` exit; it's an optional, state-backed
 *   expansion project. The Patriarchate of Alexandria has `constrained` exit;
 *   it cannot simply walk away from its historical flock. This difference in
 *   exit options is key to the engine deriving different directionalities and
 *   thus different classifications (Rope vs. Snare), even for two institutions.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification avoids two errors. It doesn't mislabel the system as a
 *   pure Snare, because that would ignore the genuine coordination and resource
 *   provision that makes it attractive to some clergy. It also doesn't mislabel
 *   it as a benign Rope, which would ignore the immense, asymmetric geopolitical
 *   extraction and the coercive pressure it applies. The Tangled Rope
 *   classification correctly identifies it as a hybrid system where a coordination
 *   mechanism has been weaponized for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_roc_african_exarchate,
    'Is the African Exarchate a top-down Kremlin-directed initiative from inception, or a more organic ROC expansion that the state opportunistically co-opted and funded?',
    'Leaked internal Kremlin or ROC Synod communications detailing the strategic planning and budget allocation for the exarchate.',
    'If Kremlin-directed, it solidifies the high extraction score. If opportunistic, it suggests the extraction accumulated over time, making the initial state more Rope-like.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(roc_african_exarchate, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for this high-extraction (ε > 0.46) constraint models its
% intensification over time, likely starting as a reaction to the 2018
% recognition of the Orthodox Church of Ukraine's independence.

% Theater ratio over time:
narrative_ontology:measurement(roc_african_exarchate_tr_t0, roc_african_exarchate, theater_ratio, 0, 0.65).
narrative_ontology:measurement(roc_african_exarchate_tr_t5, roc_african_exarchate, theater_ratio, 5, 0.60).
narrative_ontology:measurement(roc_african_exarchate_tr_t10, roc_african_exarchate, theater_ratio, 10, 0.55).

% Extraction over time (extraction_accumulation):
narrative_ontology:measurement(roc_african_exarchate_ex_t0, roc_african_exarchate, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(roc_african_exarchate_ex_t5, roc_african_exarchate, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(roc_african_exarchate_ex_t10, roc_african_exarchate, base_extractiveness, 10, 0.68).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(roc_african_exarchate, resource_allocation).

% Network relationships (structural influence edges)
% The ROC's activities are structurally linked to, and often complementary with,
% other Russian state influence operations in Africa, such as those involving
% private military contractors (e.g., Wagner Group).
narrative_ontology:affects_constraint(roc_african_exarchate, wagner_group_influence).
narrative_ontology:affects_constraint(wagner_group_influence, roc_african_exarchate).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The combination of
% beneficiary/victim declarations and the distinct exit_options for each
% agent (trapped, constrained, arbitrage) is sufficient for the engine's
% structural derivation chain to compute accurate and differentiated
% directionality (d) values for all perspectives.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */