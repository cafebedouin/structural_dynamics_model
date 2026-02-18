% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-31
% ============================================================================

:- module(constraint_gita_kurukshetra, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:interval/3,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gita_kurukshetra
 *   human_readable: The Duty of the Kshatriya (Warrior Caste)
 *   domain: religious/philosophical/social
 *
 * SUMMARY:
 *   The "dharma" or sacred duty of the warrior caste (Kshatriya) to fight in a
 *   lawful war, even against kin. It presents a powerful moral obligation
 *   rooted in a specific metaphysical worldview and social order, as articulated by
 *   Krishna to Arjuna on the battlefield of Kurukshetra. The constraint is the
 *   social and ideological pressure to conform to one's caste duty.
 *
 * KEY AGENTS (by structural relationship):
 *   - Arjuna: Primary target (powerless/trapped) — bears the moral and psychological cost of the duty.
 *   - The Cosmic Order (as personified by Krishna): Primary beneficiary (institutional/arbitrage) — the social and metaphysical system is upheld by adherence to dharma.
 *   - The Enlightened Sage: Analytical observer — perceives the system's logic from a detached viewpoint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% While a war extracts lives, the Gita argues that "Right Action" yields
% no separate profit for the actor; it is for the "upholding of thy kind".
% The extraction is the psychological/moral cost imposed on the individual.
domain_priors:base_extractiveness(gita_kurukshetra, 0.20).

% Alternatives (like fleeing to be a beggar) are explicitly framed as
% "shameful," "infamous," and "sinful" for a noble-born warrior. This is
% extreme social and ideological suppression.
domain_priors:suppression_score(gita_kurukshetra, 0.90).

% The constraint's function is presented through a grand metaphysical narrative,
% but its core function is social role enforcement.
domain_priors:theater_ratio(gita_kurukshetra, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra, extractiveness, 0.20).
narrative_ontology:constraint_metric(gita_kurukshetra, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(gita_kurukshetra, theater_ratio, 0.34).

% --- Constraint claim (must match analytical perspective type) ---
% The system is a complex mix of coordination (upholding social order) and
% asymmetric extraction (imposing duty on individuals), enforced by ideology.
% This is the definition of a Tangled Rope.
narrative_ontology:constraint_claim(gita_kurukshetra, tangled_rope).
narrative_ontology:human_readable(gita_kurukshetra, "The Duty of the Kshatriya (Warrior Caste)").
narrative_ontology:topic_domain(gita_kurukshetra, "religious/philosophical/social").

% --- Binary flags ---
% The high suppression score reflects active social/ideological enforcement.
% This is required for the Tangled Rope classification.
domain_priors:requires_active_enforcement(gita_kurukshetra).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(gita_kurukshetra, cosmic_order).
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(gita_kurukshetra, individual_ego).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ARJUNA (THE PRIMARY TARGET)
% Arjuna sees the constraint as extractive and destructive. It "perisheth"
% households and leads to "Hell." It is a moral weight that paralyzes him,
% strangling his will to fight due to overwhelming grief and confusion.
% The high suppression and perceived cost make it a Snare.
constraint_indexing:constraint_classification(gita_kurukshetra, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: KRISHNA (THE PRIMARY BENEFICIARY / ENFORCER)
% Krishna frames Dharma as a functional 'Rope'—a coordination mechanism
% for the cosmic order. "Work is more excellent than idleness." It is a path
% to "highest bliss" and "deliverance" when done without attachment, providing
% a means to fulfill one's role in the universe. From this view, the extraction is negligible.
constraint_indexing:constraint_classification(gita_kurukshetra, rope,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(arbitrage), % Krishna operates outside the system he enforces
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ENLIGHTENED SAGE (ANALYTICAL OBSERVER)
% To the sage, the workings of Dharma appear as an unchangeable 'Mountain'—a
% natural law of the Spirit. "Life cannot slay. Life is not slain!" Actions are
% just the "play of visible things" and "Nature's way," an immutable truth of
% existence. This is a classic "false natural law" perception, where a highly
% coercive social system is perceived as a natural, unchangeable reality.
constraint_indexing:constraint_classification(gita_kurukshetra, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_tests).

test(perspectival_gap_is_maximal) :-
    % Verify that the three key perspectives see three different types.
    constraint_indexing:constraint_classification(gita_kurukshetra, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gita_kurukshetra, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(gita_kurukshetra, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    TypeTarget == snare,
    TypeBeneficiary == rope,
    TypeAnalytical == mountain,
    TypeTarget \= TypeBeneficiary,
    TypeBeneficiary \= TypeAnalytical.

:- end_tests(gita_kurukshetra_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics reflect a system of ideological control. Base extractiveness (0.20)
 *   is low because the stated goal is not material gain but cosmic balance.
 *   The suppression score (0.90) is extremely high, reflecting the powerful
 *   social and moral sanctions against defying one's caste duty (dharma).
 *   The constraint is classified as a Tangled Rope because it has a genuine
 *   coordination function (maintaining social order) but achieves it via
 *   asymmetric extraction (imposing immense psychological burden) and requires
 *   active ideological enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The gap is maximal: Snare -> Rope -> Mountain.
 *   - Arjuna (Target) feels the immediate, coercive force of the duty as a Snare.
 *   - Krishna (Beneficiary/Enforcer) presents its function as a pure coordination Rope for the cosmos.
 *   - The Sage (Analytical) perceives the system as so totalizing and inescapable that it appears to be a Mountain, a feature of reality itself. This is a "false natural law" signature, where a human-constructed system is naturalized.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary is the 'cosmic_order', the abstract system of social and
 *   metaphysical roles that is maintained by the constraint. The victim is the
 *   'individual_ego', which must be subordinated to this order, bearing the
 *   cost of actions that violate personal conscience.
 *
 * MANDATROPHY ANALYSIS:
 *   This story is a canonical example of a Tangled Rope. A purely extractive
 *   analysis (Arjuna's view) would miss the coordination function. A purely
 *   functionalist analysis (Krishna's view) would ignore the coercive extraction.
 *   The analytical perspective reveals the most subtle failure mode: the
 *   system is so effective that it becomes perceived as a natural law (Mountain),
 *   masking its constructed and enforced nature. The removal of `emerges_naturally`
 *   is critical; this is a social, not a physical, constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    karma_yoga_efficacy,
    "Is 'Karma Yoga' (action without attachment to results) a universally effective coordination mechanism ('Rope') for navigating moral dilemmas, or is its perceived effectiveness dependent on accepting a specific metaphysical worldview that naturalizes a social order ('Mountain')?",
    "Comparative analysis of ethical frameworks; psychological studies on detachment and moral injury.",
    "If universal, it's a pure Rope. If worldview-dependent, it's a feature of this specific Tangled Rope, serving to mask the extraction.",
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(karma_yoga_efficacy, conceptual, "Is selfless action universally applicable or dependent on a specific metaphysical frame?").

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(gita_kurukshetra, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required as base_extractiveness (0.20) is below the 0.46 threshold.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% The constraint functions as an enforcement mechanism for a social hierarchy.
narrative_ontology:coordination_type(gita_kurukshetra, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */