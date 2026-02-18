% ============================================================================
% CONSTRAINT STORY: comitatus_bond
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_comitatus_bond, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: comitatus_bond
 *   human_readable: The Germanic Comitatus Code
 *   domain: social/political
 *
 * SUMMARY:
 *   The comitatus is the foundational socio-political constraint governing the
 *   relationship between a lord (the "ring-giver") and his thanes in early
 *   Germanic society. It mandates absolute loyalty in exchange for protection,
 *   sustenance, and a share of spoils. Breaking the bond results in exile
 *   (becoming "wrackful"), a fate often considered worse than death due to the
 *   complete lack of social or economic alternatives.
 *
 * KEY AGENTS (by structural relationship):
 *   - Thanes (e.g., Wiglaf, the cowardly thanes): Primary targets (powerless/trapped) — bear the extraction of life-risking service and absolute loyalty.
 *   - Lords (e.g., Hrothgar, Beowulf as king): Primary beneficiaries (institutional/arbitrage) — benefit from a stable warrior class to defend and expand their domain.
 *   - Analytical Observer: Sees the full structure as a hybrid coordination/extraction mechanism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: Moderate. While the lord extracts life-risking labor, he provides
% protection, land, and gold. It is a functional coordination with high cost.
domain_priors:base_extractiveness(comitatus_bond, 0.40).
% Rationale: High. Alternatives to the bond are practically invisible. To be
% without a lord is to be a "wanderer," a social outcast with no rights.
domain_priors:suppression_score(comitatus_bond, 0.80).
% Rationale: Low. The system is highly functional, not performative.
domain_priors:theater_ratio(comitatus_bond, 0.10).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(comitatus_bond, extractiveness, 0.40).
narrative_ontology:constraint_metric(comitatus_bond, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(comitatus_bond, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(comitatus_bond, tangled_rope).
narrative_ontology:human_readable(comitatus_bond, "The Germanic Comitatus Code").
narrative_ontology:topic_domain(comitatus_bond, "social/political").

% --- Binary flags ---
% Requires active enforcement (gold-giving, feasting) and cultural transmission.
domain_priors:requires_active_enforcement(comitatus_bond).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(comitatus_bond, lords_and_tribal_leadership).
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(comitatus_bond, thanes_and_warrior_class).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function.
   The engine derives d from beneficiary/victim membership + exit_options.
   ========================================================================== */

% PERSPECTIVE 1: THE LOYAL OR EXILED THANE (SNARE)
% Agent who bears the most extraction. For the thane, the bond is coercive.
% Loyalty is compelled by the threat of social death (exile), which is the
% ultimate sanction in a world with no alternative social structures.
% Engine derives d from: victim membership + trapped exit -> d ≈ 0.95 -> high χ.
constraint_indexing:constraint_classification(comitatus_bond, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE LORD / RING-GIVER (ROPE)
% Agent who benefits most. For the lord, the bond is a pure coordination
% mechanism to organize military power and ensure social stability.
% Engine derives d from: beneficiary membership + arbitrage exit -> d ≈ 0.05 -> low/negative χ.
constraint_indexing:constraint_classification(comitatus_bond, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analytical view recognizes both the genuine coordination function (Rope)
% and the highly coercive, asymmetric extraction (Snare). The system cannot
% be reduced to either pure function; it is a hybrid.
% Engine derives d ≈ 0.72 for analytical perspective.
constraint_indexing:constraint_classification(comitatus_bond, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(comitatus_bond_tests).

test(perspectival_gap_is_snare_vs_rope) :-
    % Verify the core perspectival gap between the target (thane) and beneficiary (lord).
    constraint_indexing:constraint_classification(comitatus_bond, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(comitatus_bond, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_tangled_rope) :-
    % The synthesis of the two perspectives is a Tangled Rope.
    constraint_indexing:constraint_classification(comitatus_bond, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties_present) :-
    % Verify the three structural requirements for a Tangled Rope are declared.
    narrative_ontology:constraint_beneficiary(comitatus_bond, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(comitatus_bond, _),     % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(comitatus_bond).

:- end_tests(comitatus_bond_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.40) reflects the high cost (risk of death)
 *   demanded of thanes, balanced by material rewards and social status. The
 *   suppression score (0.80) is critical: the non-existence of viable
 *   alternatives is what makes the system so coercive. A thane cannot simply
 *   "opt out" and become a farmer; to be lordless is to be an outcast.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the lord (Hrothgar), the comitatus is a tool for
 *   social order and defense—a classic Rope. For the thane (Wiglaf), it is a
 *   coercive system where the only alternative to compliance is social death.
 *   This high suppression and cost makes it a Snare from his perspective, even
 *   if he embraces it willingly. The willingness is itself a product of the
 *   constraint's totalizing nature.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `lords_and_tribal_leadership`. They gain a loyal fighting
 *     force, enabling stability and expansion.
 *   - Victims: `thanes_and_warrior_class`. They bear the physical risks and are
 *     trapped by the social code, exchanging autonomy for security.
 *   This clear division drives the directionality calculation and the resulting
 *   perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a canonical Tangled Rope. To call it a pure Rope ignores the
 *   immense coercion and suppression of alternatives. To call it a pure Snare
 *   ignores its genuine, and essential, coordination function for tribal
 *   survival in a violent era. The Tangled Rope classification correctly
 *   captures this dual nature, preventing mislabeling.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    wyrd_nature,
    'Is \'Wyrd\' (Fate) a literal Mountain constraint of physics, or a conceptual Rope used to rationalize outcomes within the comitatus system?',
    'Textual analysis of whether characters attribute success/failure to Fate consistently, or instrumentally to uphold the social code.',
    'If Mountain: The comitatus is secondary to an external, unchangeable reality. If Rope: The comitatus is the primary driver of action, and "Fate" is a post-hoc justification.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(wyrd_nature, conceptual, 'Distinguishing the structural role of Fate (Wyrd) from its narrative function as justification for the comitatus code.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(comitatus_bond, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required, as base_extractiveness (0.40) is below the 0.46 threshold.

/*
narrative_ontology:measurement(comitatus_bond_tr_t0, comitatus_bond, theater_ratio, 0, 0.1).
narrative_ontology:measurement(comitatus_bond_tr_t5, comitatus_bond, theater_ratio, 5, 0.1).
narrative_ontology:measurement(comitatus_bond_tr_t10, comitatus_bond, theater_ratio, 10, 0.1).

narrative_ontology:measurement(comitatus_bond_ex_t0, comitatus_bond, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(comitatus_bond_ex_t5, comitatus_bond, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(comitatus_bond_ex_t10, comitatus_bond, base_extractiveness, 10, 0.4).
*/

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The comitatus is a mechanism for enforcing a social contract.
narrative_ontology:coordination_type(comitatus_bond, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations accurately models the power dynamics.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */