% ============================================================================
% CONSTRAINT STORY: kardashev_scale_progress_narrative
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_kardashev_scale_progress_narrative, []).

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
 *   constraint_id: kardashev_scale_progress_narrative
 *   human_readable: "The Kardashev Scale as a Narrative of Civilizational Progress"
 *   domain: social/technological
 *
 * SUMMARY:
 *   The Kardashev scale, originally a SETI heuristic, was popularized as a
 *   teleological framework for civilizational progress, where advancement is
 *   equated with increasing energy consumption. This narrative constrains
 *   discourse about humanity's long-term future, privileging expansionist,
 *   energy-intensive goals (e.g., Dyson spheres) while suppressing alternative
 *   models of development centered on sustainability, information density,
 *   or de-growth (the "transcension hypothesis").
 *
 * KEY AGENTS (by structural relationship):
 *   - Proponents of megastructure engineering: Primary beneficiary (institutional/arbitrage) — their projects are legitimized and centered by this narrative.
 *   - Advocates for alternative civilizational models: Primary target (organized/constrained) — their paradigms (sustainability, transcension) are marginalized and suppressed.
 *   - Non-specialist public and planners: Secondary target (powerless/trapped) — their imagination and long-term planning are channeled by the scale's apparent objectivity.
 *   - Analytical observer: Sees the dual function as both a simple coordination metric and an extractive ideological filter.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kardashev_scale_progress_narrative, 0.48).
domain_priors:suppression_score(kardashev_scale_progress_narrative, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(kardashev_scale_progress_narrative, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kardashev_scale_progress_narrative, extractiveness, 0.48).
narrative_ontology:constraint_metric(kardashev_scale_progress_narrative, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(kardashev_scale_progress_narrative, theater_ratio, 0.20).

% --- NL Profile Metrics (required for mountain constraints) ---
% This is a human-constructed narrative, not a natural law.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(kardashev_scale_progress_narrative, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(kardashev_scale_progress_narrative). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(kardashev_scale_progress_narrative, proponents_of_megastructure_engineering).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(kardashev_scale_progress_narrative, advocates_for_alternative_civilizational_models).
narrative_ontology:constraint_victim(kardashev_scale_progress_narrative, non_specialist_public_and_planners).

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
% Advocates for alternative models see the scale as a Snare. It traps discourse,
% starves their paradigms of legitimacy and resources, and presents a single,
% narrow path to the future as inevitable. They are organized, but constrained
% by the dominant narrative.
constraint_indexing:constraint_classification(kardashev_scale_progress_narrative, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Proponents of large-scale space engineering see the scale as a pure Rope.
% It provides a simple, powerful Schelling point to coordinate research,
% justify funding for ambitious projects, and build public consensus.
% For them, it is a tool of pure coordination with no downside.
constraint_indexing:constraint_classification(kardashev_scale_progress_narrative, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analytical view recognizes both functions. The scale genuinely solves a
% coordination problem (how to talk about vast differences in civilizational
% capacity), but this function has been coupled to an extractive one (suppressing
% alternative measures of progress). This dual-nature is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(kardashev_scale_progress_narrative, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE GENERAL PUBLIC (SNARE)
% For the non-specialist public and planners, the narrative acts as a Snare.
% It traps their long-term vision within an energy-centric paradigm, making it
% difficult to even consider or value alternative futures. They are effectively
% powerless and trapped within the dominant discourse.
constraint_indexing:constraint_classification(kardashev_scale_progress_narrative, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kardashev_scale_progress_narrative_tests).

test(perspectival_gap_is_snare_vs_rope, [nondet]) :-
    constraint_indexing:constraint_classification(kardashev_scale_progress_narrative, snare, context(agent_power(organized), _, _, _)),
    constraint_indexing:constraint_classification(kardashev_scale_progress_narrative, rope, context(agent_power(institutional), _, _, _)).

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(kardashev_scale_progress_narrative, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_is_satisfied) :-
    % Verify that all three conditions for a Tangled Rope classification are met.
    narrative_ontology:constraint_beneficiary(kardashev_scale_progress_narrative, _), % Has coordination function
    narrative_ontology:constraint_victim(kardashev_scale_progress_narrative, _),       % Has asymmetric extraction
    domain_priors:requires_active_enforcement(kardashev_scale_progress_narrative).    % Requires enforcement

:- end_tests(kardashev_scale_progress_narrative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): The extraction is not monetary but conceptual.
 *     The narrative extracts legitimacy and potential from a wide range of
 *     possible futures and channels it into a single, energy-centric vision.
 *     This is a significant structural cost for marginalized paradigms.
 *   - Suppression Score (0.65): The scale is deeply embedded in science fiction
 *     and popular science, making it a powerful cultural filter. Alternative
 *     ideas face a high barrier to entry in public discourse.
 *   - This constraint is a Tangled Rope because it possesses both a genuine
 *     coordination function (providing a simple, universally understood metric)
 *     and a significant, asymmetric extractive function (suppressing other
 *     metrics of progress like sustainability or informational complexity).
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For beneficiaries (proponents of megastructures), the scale
 *   is a useful Rope that organizes thought and action toward goals they value.
 *   For victims (advocates for sustainability/transcension), it's a Snare that
 *   forecloses debate and makes their own visions seem naive or un-ambitious.
 *   The beneficiary does not perceive the cost imposed on the victim, seeing it
 *   merely as the natural outcome of a superior idea.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `proponents_of_megastructure_engineering`. They benefit as the
 *     scale provides a ready-made justification for their research and projects.
 *     The engine derives a low `d` value for them, leading to a negative `chi` (Rope).
 *   - Victims: Two groups bear costs. `advocates_for_alternative_civilizational_models`
 *     are an organized group whose paradigms are actively suppressed (Snare from their
 *     perspective). `non_specialist_public_and_planners` are a diffuse group whose
 *     imaginative landscape is constrained, trapping them in a single vision of progress
 *     (Snare from their powerless perspective). The engine derives high `d` values for both.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification avoids two common errors. A naive analysis might call the
 *   scale a pure Rope ("it's just a helpful thought experiment"), ignoring the
 *   real-world suppression of alternative ideas. A cynical analysis might call it
 *   a pure Snare ("it's just capitalist ideology"), ignoring that it does solve a
 *   real coordination problem of communicating vast scales. The Tangled Rope
 *   classification correctly identifies it as a hybrid structure where a useful
 *   coordination tool has been repurposed for extractive ends.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_kardashev_progress_narrative,
    'Is the Kardashev scale a causal driver of expansionist ideology, or merely a convenient label for a pre-existing technological trajectory?',
    'Analysis of long-term policy and R&D funding decisions, correlating them with explicit mentions of the scale as a motivating framework.',
    'If causal, it is a strong Tangled Rope. If merely a label, its structural power is lower, and it may be better modeled as a Piton or a weaker Rope.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_kardashev_progress_narrative, empirical, 'Is the scale a causal driver of expansionist ideology or just a convenient label?').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kardashev_scale_progress_narrative, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data modeling the narrative's shift from a technical heuristic to
% an ideological constraint. T=0 is 1964 (Kardashev's paper), T=5 is ~1980s
% (Sagan's popularization), T=10 is the present day.

% Theater ratio over time:
narrative_ontology:measurement(kspn_tr_t0, kardashev_scale_progress_narrative, theater_ratio, 0, 0.05).
narrative_ontology:measurement(kspn_tr_t5, kardashev_scale_progress_narrative, theater_ratio, 5, 0.15).
narrative_ontology:measurement(kspn_tr_t10, kardashev_scale_progress_narrative, theater_ratio, 10, 0.20).

% Extraction over time (extraction_accumulation):
narrative_ontology:measurement(kspn_ex_t0, kardashev_scale_progress_narrative, base_extractiveness, 0, 0.10).
narrative_ontology:measurement(kspn_ex_t5, kardashev_scale_progress_narrative, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(kspn_ex_t10, kardashev_scale_progress_narrative, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(kardashev_scale_progress_narrative, information_standard).

% --- Network Decomposition (Constraint Families) ---
% The natural-language term "Kardashev Scale" decomposes into two distinct
% constraints with different ε values, per the ε-invariance principle.

% DUAL FORMULATION NOTE:
% This constraint is one of 2 stories decomposed from "The Kardashev Scale".
% Decomposed because ε differs across observables (ε-invariance principle).
% The upstream heuristic (low-ε) enabled the downstream narrative (high-ε).
% Related stories:
%   - kardashev_scale_seti_heuristic (ε=0.05, Rope)
%   - kardashev_scale_progress_narrative (ε=0.48, Tangled Rope)

narrative_ontology:affects_constraint(kardashev_scale_seti_heuristic, kardashev_scale_progress_narrative).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% The standard derivation from beneficiary/victim + exit options appears
% to accurately model the structural relationships in this case. No overrides
% are necessary.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */