% ============================================================================
% CONSTRAINT STORY: great_awakening_rekindling
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_great_awakening_rekindling, []).

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
 *   constraint_id: great_awakening_rekindling
 *   human_readable: The Great Awakening's Reframing of Biblical Authority
 *   domain: religious/social
 *
 * SUMMARY:
 *   The First Great Awakening (c. 1730-1755) was a religious revival that
 *   shifted interpretive authority of the King James Bible from the established
 *   clergy to the individual. This constraint models the revivalist movement's
 *   use of the text, which simultaneously functioned as a coordination tool for
 *   populist empowerment and an extractive mechanism that dismantled the
 *   authority of the established "Old Light" church hierarchy.
 *
 * KEY AGENTS (by structural relationship):
 *   - "New Light" Converts: Primary beneficiary (powerless/mobile) — gain direct interpretive agency.
 *   - "Old Light" Clergy: Primary target (institutional/constrained) — lose interpretive monopoly and social control.
 *   - Analytical Historian: Analytical observer — sees the dual function of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% While decentralized, the movement's primary effect was the extraction of
% authority, social control, and tithes from the established church. This
% transfer of power is modeled as high base extraction.
domain_priors:base_extractiveness(great_awakening_rekindling, 0.50).

% The "Old Light" establishment actively tried to suppress the movement by
% banning itinerant preachers and denouncing the revivals as emotional excess.
domain_priors:suppression_score(great_awakening_rekindling, 0.60).
domain_priors:theater_ratio(great_awakening_rekindling, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(great_awakening_rekindling, extractiveness, 0.50).
narrative_ontology:constraint_metric(great_awakening_rekindling, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(great_awakening_rekindling, theater_ratio, 0.15).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(great_awakening_rekindling, tangled_rope).

% --- Binary flags ---
% The movement was sustained by itinerant preachers and revival meetings, which
% constituted a form of active, decentralized enforcement of the new interpretation.
domain_priors:requires_active_enforcement(great_awakening_rekindling).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(great_awakening_rekindling, new_light_converts).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(great_awakening_rekindling, old_light_clergy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function.
   The engine derives d from beneficiary/victim membership + exit_options.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE "NEW LIGHT" CONVERT (ROPE)
% For the socially powerless, the revivalist interpretation is a pure Rope. It
% provides a direct connection to spiritual authority, bypassing the rigid
% hierarchy of the established church. It coordinates a new, empowered
% community of believers.
% Engine derives d from: beneficiary + mobile exit -> d ≈ 0.15 -> f(d) ≈ -0.01 -> low χ
constraint_indexing:constraint_classification(great_awakening_rekindling, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE "OLD LIGHT" CLERGY (SNARE)
% For the established institutional clergy, the movement is a Snare. It
% asymmetrically extracts their interpretive authority, social standing, and
% economic support. They are trapped within their parishes, unable to easily
% escape the challenge to their power.
% Engine derives d from: victim + constrained exit -> d ≈ 0.90 -> f(d) ≈ 1.35 -> high χ
constraint_indexing:constraint_classification(great_awakening_rekindling, snare,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The historian sees both functions simultaneously. The constraint is a
% Tangled Rope: it has a genuine coordination function (uniting the New Lights)
% but is inextricably linked to an asymmetric extraction of power from the
% Old Light establishment, requiring active enforcement (preaching) to sustain.
constraint_indexing:constraint_classification(great_awakening_rekindling, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(great_awakening_rekindling_tests).

test(perspectival_gap) :-
    % Verify the gap between the beneficiary (New Light) and target (Old Light).
    constraint_indexing:constraint_classification(great_awakening_rekindling, TypeTarget,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(great_awakening_rekindling, TypeBeneficiary,
        context(agent_power(powerless), _, _, _)),
    assertion(TypeTarget == snare),
    assertion(TypeBeneficiary == rope),
    TypeTarget \= TypeBeneficiary.

test(analytical_type_is_tangled_rope) :-
    % The analytical view must resolve the conflict as a Tangled Rope.
    constraint_indexing:constraint_classification(great_awakening_rekindling, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_check) :-
    % Verify that the structural conditions for a Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(great_awakening_rekindling, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(great_awakening_rekindling, _),     % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(great_awakening_rekindling).

:- end_tests(great_awakening_rekindling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The key decision was to model the extraction of authority as a high base
 *   extractiveness (ε=0.50). While the movement was decentralized, its primary
 *   structural impact was a non-consensual transfer of power and resources
 *   away from the established church. The suppression score (0.60) reflects
 *   the active, though ultimately unsuccessful, attempts by the Old Light
 *   clergy to ban itinerant preachers and quell the revivals.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the New Light convert, the constraint is a tool
 *   of liberation (Rope). For the Old Light minister, it is an existential
 *   threat that dismantles their authority (Snare). The system correctly
 *   classifies these opposing views based on their structural relationship
 *   (beneficiary vs. victim) and exit options (mobile vs. constrained).
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `new_light_converts`. They gain spiritual and social agency.
 *     Their mobile exit (they can follow preachers or form new congregations)
 *     gives them power, leading to a low derived `d` and a Rope classification.
 *   - Victims: `old_light_clergy`. They lose their monopoly on interpretation,
 *     social status, and income. Their constrained exit (tied to a physical
 *     parish) makes them vulnerable, leading to a high derived `d` and a Snare
 *     classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The analytical classification as a Tangled Rope is crucial. It prevents
 *   mislabeling the movement as either pure populist coordination (Rope) or
 *   pure top-down extraction (Snare). The framework correctly identifies that
 *   it possesses both functions simultaneously, which is the hallmark of a
 *   Tangled Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_great_awakening_rekindling,
    'Was the extraction of authority from the Old Lights a primary goal of the movement, or an unintended consequence of its coordination function?',
    'Analysis of sermons and writings of key revivalist figures (e.g., Jonathan Edwards, George Whitefield) for explicit intent to dismantle the existing hierarchy versus focus on individual salvation.',
    'If primary goal: Confirms the Snare/Tangled Rope structure. If consequence: Suggests the constraint is closer to a Rope whose externalities were weaponized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(omega_great_awakening_rekindling, conceptual, 'Distinguishing between intent and consequence in the extraction of institutional authority.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(great_awakening_rekindling, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extraction > 0.46 requires temporal data. The values model a stable
% period of high conflict and extraction during the peak of the Awakening.
% Theater ratio over time:
narrative_ontology:measurement(gar_tr_t0, great_awakening_rekindling, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gar_tr_t5, great_awakening_rekindling, theater_ratio, 5, 0.15).
narrative_ontology:measurement(gar_tr_t10, great_awakening_rekindling, theater_ratio, 10, 0.15).

% Extraction over time:
narrative_ontology:measurement(gar_ex_t0, great_awakening_rekindling, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(gar_ex_t5, great_awakening_rekindling, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(gar_ex_t10, great_awakening_rekindling, base_extractiveness, 10, 0.50).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% The constraint's coordination function revolves around creating a shared
% interpretive framework for a text.
narrative_ontology:coordination_type(great_awakening_rekindling, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary. The structural derivation from beneficiary/victim
% declarations and exit options accurately models the power dynamics.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */