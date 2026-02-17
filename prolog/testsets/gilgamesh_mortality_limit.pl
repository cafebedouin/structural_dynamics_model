% ============================================================================
% CONSTRAINT STORY: gilgamesh_mortality_limit
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-21
% ============================================================================

:- module(constraint_gilgamesh_mortality_limit, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: gilgamesh_mortality_limit
 * human_readable: The Allotment of Mortality
 * domain: philosophical/religious
 * * SUMMARY:
 * This constraint models mortality as the ultimate physical limit defined in the 
 * Epic of Gilgamesh. Gilgamesh—a king of immense power—attempts to treat his 
 * mortality as a "Snare" he can untie. Ultimately, he discovers that death is a 
 * Mountain (a physical law set by the gods) that extracts the "margin" of human 
 * life regardless of rank or strength. From an analytical view, it is a Tangled
 * Rope: a coordination mechanism (population control, divine/mortal distinction)
 * that relies on the total, asymmetric extraction of life from humanity.
 * * KEY AGENTS:
 * - Gilgamesh (The Subject): Powerless in the face of the limit.
 * - The Gods (Enlil/Anu): Beneficiary (Institutional), architects of the constraint.
 * - Systems Analyst (Auditor): Analytical observer of the structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(gilgamesh_mortality_limit, 1.0). % Death is total extraction.
domain_priors:suppression_score(gilgamesh_mortality_limit, 0.9).   % Alternatives (immortality) are strictly suppressed by divine decree.
domain_priors:theater_ratio(gilgamesh_mortality_limit, 0.1).       % The function is direct and non-performative.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(gilgamesh_mortality_limit, extractiveness, 1.0).
narrative_ontology:constraint_metric(gilgamesh_mortality_limit, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(gilgamesh_mortality_limit, theater_ratio, 0.1).

% Constraint self-claim (what does the constraint claim to be?)
% It is presented as an immutable law of the cosmos for humankind.
narrative_ontology:constraint_claim(gilgamesh_mortality_limit, tangled_rope).
narrative_ontology:human_readable(gilgamesh_mortality_limit, "The Allotment of Mortality").

% Binary flags
% The divine decree requires enforcement to maintain the distinction.
domain_priors:requires_active_enforcement(gilgamesh_mortality_limit).

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(gilgamesh_mortality_limit, divine_council).
narrative_ontology:constraint_victim(gilgamesh_mortality_limit, humanity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (GILGAMESH, POST-QUEST) - MOUNTAIN
% After failing all tests, he accepts mortality as an unchangeable fact.
constraint_indexing:constraint_classification(gilgamesh_mortality_limit, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (THE GODS) - ROPE
% From their perspective, it's a necessary coordination tool to manage humanity.
constraint_indexing:constraint_classification(gilgamesh_mortality_limit, rope,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER - TANGLED ROPE
% Recognizes the coordination function but also the total, asymmetric extraction
% and the requirement for active enforcement (divine decree).
constraint_indexing:constraint_classification(gilgamesh_mortality_limit, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE REBEL (GILGAMESH, MID-QUEST) - SNARE
% While actively fighting it, he perceives mortality as a predatory trap.
constraint_indexing:constraint_classification(gilgamesh_mortality_limit, snare,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gilgamesh_mortality_limit_tests).

test(perspectival_gap) :-
    % Verify the gap between the powerless subject (Mountain) and institutional beneficiary (Rope).
    constraint_indexing:constraint_classification(gilgamesh_mortality_limit, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gilgamesh_mortality_limit, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    assertion(TypePowerless == mountain),
    assertion(TypeInstitutional == rope),
    TypePowerless \= TypeInstitutional.

test(analytical_classification_is_tangled_rope) :-
    % The analytical view must resolve the conflict into a Tangled Rope.
    constraint_indexing:constraint_classification(gilgamesh_mortality_limit, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    assertion(TypeAnalytical == tangled_rope).

:- end_tests(gilgamesh_mortality_limit_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness is set to 1.0 as mortality represents the total
 * extraction of an individual's biological existence. Suppression is 0.9 because
 * the alternative (immortality) is shown to exist (Utnapishtim) but is actively
 * denied to humanity by divine will.
 *
 * The Perspectival Gap is profound:
 * - To the powerless subject (late-stage Gilgamesh), it's an immutable Mountain.
 * - To the institutional beneficiary (the Gods), it's a Rope for world management.
 * - To the powerful but trapped rebel (mid-quest Gilgamesh), it's a predatory Snare.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The analytical classification as a Tangled Rope correctly identifies that this
 * is not a pure Mountain (natural law) but a constructed system with both a
 * coordination function (for the gods) and an asymmetric extraction function
 * (on humanity). This prevents misclassifying a divine decree as a simple fact
 * of physics, which is the core of Gilgamesh's tragic discovery.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_gilgamesh_mortality_limit,
    "Is mortality a true Mountain (a physical necessity) that the gods merely claim credit for, or is it a constructed Snare (a contingent rule) they actively enforce?",
    "Discovery of extraterrestrial biological systems with different aging or life-cycle properties.",
    "If true Mountain, the gods' claim is theatrical. If constructed Snare, the constraint is fundamentally political, not physical.",
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(gilgamesh_mortality_limit, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Mortality as a constraint is constant over the interval of human history.
% The values are flat, indicating no drift. This is required due to high extraction.

% Theater ratio over time:
narrative_ontology:measurement(gml_tr_t0, gilgamesh_mortality_limit, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gml_tr_t5, gilgamesh_mortality_limit, theater_ratio, 5, 0.1).
narrative_ontology:measurement(gml_tr_t10, gilgamesh_mortality_limit, theater_ratio, 10, 0.1).

% Extraction over time:
narrative_ontology:measurement(gml_ex_t0, gilgamesh_mortality_limit, base_extractiveness, 0, 1.0).
narrative_ontology:measurement(gml_ex_t5, gilgamesh_mortality_limit, base_extractiveness, 5, 1.0).
narrative_ontology:measurement(gml_ex_t10, gilgamesh_mortality_limit, base_extractiveness, 10, 1.0).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The constraint enforces the divine/mortal distinction and manages population.
narrative_ontology:coordination_type(gilgamesh_mortality_limit, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */