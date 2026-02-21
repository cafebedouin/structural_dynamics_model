% ============================================================================
% CONSTRAINT STORY: antikythera_knowledge_loss
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_antikythera_knowledge_loss, []).

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
 *   constraint_id: antikythera_knowledge_loss
 *   human_readable: Loss of Hellenistic Precision Gearing Knowledge
 *   domain: technological
 *
 * SUMMARY:
 *   The Antikythera Mechanism, a 2nd-century BC analog computer of astonishing
 *   complexity, demonstrates a level of mechanical and astronomical knowledge
 *   that was subsequently lost to the world for over 1,500 years. This
 *   constraint is not the device itself, but the historical epistemic barrier
 *   created by the loss of the knowledge required for its construction. This
 *   discontinuity represents a fixed, unchangeable feature of the
 *   technological landscape for subsequent civilizations until the knowledge
 *   was independently re-derived during the European Renaissance.
 *
 * KEY AGENTS (by structural relationship):
 *   - Medieval Artisan/Astronomer: Subject to the constraint (powerless/trapped) — unable to access or even conceive of this technological path.
 *   - Modern Historian/Archaeologist: Observer with perspective (institutional/arbitrage) — can understand the scope of the loss but cannot change the historical fact.
 *   - Analytical Observer: Sees the full structure across civilizational time.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(antikythera_knowledge_loss, 0.05).
domain_priors:suppression_score(antikythera_knowledge_loss, 0.04).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(antikythera_knowledge_loss, 0.0).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(antikythera_knowledge_loss, extractiveness, 0.05).
narrative_ontology:constraint_metric(antikythera_knowledge_loss, suppression_requirement, 0.04).
narrative_ontology:constraint_metric(antikythera_knowledge_loss, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Without these, the NL signature defaults
% to 0.5 and fails certification.
narrative_ontology:constraint_metric(antikythera_knowledge_loss, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(antikythera_knowledge_loss, resistance, 0.02).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(antikythera_knowledge_loss, mountain).

% --- Emergence flag (required for mountain constraints) ---
% The loss of knowledge was a result of historical contingency, not design.
% For subsequent generations, the resulting technological barrier was an
% un-designed, emergent feature of their reality. Required for the
% mountain metric gate to fire.
domain_priors:emerges_naturally(antikythera_knowledge_loss).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% As a Mountain constraint representing a passive state (a lack of knowledge),
% there are no clearly defined beneficiaries or victims in the active sense.
% The absence of these declarations is intentional and characteristic of
% Mountain constraints.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% UNIFORM-TYPE CONSTRAINT: This is a Mountain-only constraint. The classification
% is invariant across all perspectives because the low base extractiveness (ε)
% and suppression scores are below the Mountain thresholds, regardless of
% scaling from power, scope, or directionality. The following perspectives
% demonstrate this invariance.

% PERSPECTIVE 1: THE MEDIEVAL ARTISAN
% For an artisan in 1000 AD, the inability to build such a device is an
% absolute, unchangeable technological limitation. It is a 'law of nature'
% for their world. They are trapped in their technological paradigm.
constraint_indexing:constraint_classification(antikythera_knowledge_loss, mountain,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: THE MODERN HISTORIAN
% An institutional actor who understands the full context. The historical fact
% of the knowledge loss is fixed and unalterable. They have 'arbitrage' over
% historical information but cannot change the past.
constraint_indexing:constraint_classification(antikythera_knowledge_loss, mountain,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context. From a civilizational perspective, the 1500-year
% technological gap is a fixed feature of the historical record, qualifying as
% a Mountain.
constraint_indexing:constraint_classification(antikythera_knowledge_loss, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(antikythera_knowledge_loss_tests).

test(perspectival_invariance, [nondet]) :-
    % For a Mountain, all perspectives should yield the same classification.
    findall(Type,
            constraint_indexing:constraint_classification(antikythera_knowledge_loss, Type, _),
            Types),
    list_to_set(Types, [mountain]).

test(mountain_threshold_validation) :-
    % Verify the constraint meets the strict numerical criteria for a Mountain.
    narrative_ontology:constraint_metric(antikythera_knowledge_loss, extractiveness, E),
    narrative_ontology:constraint_metric(antikythera_knowledge_loss, suppression_requirement, S),
    E =< 0.25,
    S =< 0.05.

test(natural_law_profile_validation) :-
    % Verify the constraint has the necessary metrics and flags for NL certification.
    domain_priors:emerges_naturally(antikythera_knowledge_loss),
    narrative_ontology:constraint_metric(antikythera_knowledge_loss, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(antikythera_knowledge_loss, resistance, R),
    config:param(natural_law_collapse_min, ACMIN),
    config:param(natural_law_resistance_max, RMAX),
    AC >= ACMIN,
    R =< RMAX.

:- end_tests(antikythera_knowledge_loss_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Mountain because it represents a fixed,
 *   passive, and unchangeable barrier for a long historical period. The base
 *   extractiveness (ε=0.05) is extremely low, reflecting that the loss of
 *   knowledge imposed an immense opportunity cost rather than an active
 *   extraction. The suppression score (0.04) is also very low, as no active
 *   agency was suppressing this technology; its absence was a product of
 *   historical contingency (e.g., societal collapse, loss of texts).
 *   The Natural Law profile metrics confirm this: `accessibility_collapse` is
 *   near total (0.98), as the technological path was completely foreclosed,
 *   and `resistance` was nonexistent (0.02) because one cannot resist knowledge
 *   that is unknown.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap; this is a uniform-type constraint. From the
 *   trapped medieval artisan to the modern historian with full context, the
 *   historical fact of this 1500-year technological stasis is an unchangeable
 *   reality. This invariance is the hallmark of a true Mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not a significant factor here, as the base extractiveness
 *   is too low for the f(d) multiplier to push χ out of the Mountain classification
 *   range. No beneficiary/victim declarations are made because it is not a
 *   constraint with active stakeholders. One could argue all subsequent
 *   humanity was the 'victim' of the opportunity cost, but this is too broad
 *   to be structurally useful.
 *
 * MANDATROPHY ANALYSIS:
 *   The Mountain classification correctly identifies this as a passive,
 *   structural feature of history, not an active system of coordination or
 *   extraction. It avoids mislabeling a historical accident as a Snare (no
 *   perpetrator) or a Piton (no performative maintenance). It was simply a
 *   hole in the collective knowledge of civilization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_antikythera,
    'Was the knowledge behind the mechanism truly lost, or did it merely fail to find fertile ground for propagation due to economic/social conditions, surviving in isolated pockets (e.g., Islamic-era automata)?',
    'Discovery of intermediate devices or detailed schematics from the 1st to 14th centuries that show a continuous, albeit niche, tradition of complex gearing.',
    'If knowledge survived but was not adopted, the constraint shifts from a Mountain (hard epistemic barrier) to a Tangled Rope (social/economic factors suppressing a known technology for the benefit of existing craft guilds or intellectual traditions).',
    confidence_without_resolution(high) % High confidence in the "lost knowledge" model given current evidence.
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(antikythera_knowledge_loss, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required as base_extractiveness (0.05) is below the 0.46 threshold.
% The metrics for this constraint are static over its entire historical duration.
%
% narrative_ontology:measurement(antikythera_knowledge_loss_tr_t0, antikythera_knowledge_loss, theater_ratio, 0, 0.0).
% narrative_ontology:measurement(antikythera_knowledge_loss_ex_t0, antikythera_knowledge_loss, base_extractiveness, 0, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% This constraint is causally linked to broader historical events that led to
% knowledge loss in the classical world.
narrative_ontology:affects_constraint(alexandrian_library_destruction, antikythera_knowledge_loss).
narrative_ontology:affects_constraint(collapse_of_western_roman_empire, antikythera_knowledge_loss).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary. As a Mountain constraint with no declared
% beneficiary/victim groups, the engine's canonical fallbacks for power atoms
% are sufficient, and the classification is insensitive to directionality due
% to the extremely low base extractiveness.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */