% ============================================================================
% CONSTRAINT STORY: dionysiac_frenzy
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_dionysiac_frenzy, []).

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
 * * constraint_id: dionysiac_frenzy
 * human_readable: Dionysiac Religious Possession
 * domain: religious/social
 * * SUMMARY:
 * A state of divinely induced madness (ecstasy) that compels subjects to
 * abandon social roles for mountain rites. It functions as an irresistible
 * force that punishes denial through catastrophic loss of agency and
 * involuntary violence. The god Dionysus brings the frenzy to Thebes as
 * punishment for the city's denial of his godhead.
 * * KEY AGENTS:
 * - Agave (and the Maenads): Subject (Powerless)
 * - Dionysus: Beneficiary (Institutional)
 * - Analytical Historian: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(dionysiac_frenzy, 0.80). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(dionysiac_frenzy, 0.90).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(dionysiac_frenzy, 0.10).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(dionysiac_frenzy, extractiveness, 0.80).
narrative_ontology:constraint_metric(dionysiac_frenzy, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(dionysiac_frenzy, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(dionysiac_frenzy, tangled_rope).
narrative_ontology:human_readable(dionysiac_frenzy, "Dionysiac Religious Possession").

% Binary flags
domain_priors:requires_active_enforcement(dionysiac_frenzy). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(dionysiac_frenzy, dionysus).
narrative_ontology:constraint_victim(dionysiac_frenzy, agave).
narrative_ontology:constraint_victim(dionysiac_frenzy, pentheus).
narrative_ontology:constraint_victim(dionysiac_frenzy, theban_women).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For Agave and the Maenads, the frenzy is an inescapable, extractive trap.
% The high base extraction (0.8) is amplified by their powerlessness (π=1.5),
% resulting in a massive effective extraction (χ = 0.8 * 1.5 * 0.9 = 1.08).
% It feels like a Mountain, but its high extraction makes it a Snare.
constraint_indexing:constraint_classification(dionysiac_frenzy, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For Dionysus, the frenzy is a tool for coordination and enforcement of his
% divine status. His institutional power (π=-0.2) inverts the extraction,
% making it feel beneficial (χ = 0.8 * -0.2 * 1.2 = -0.192).
constraint_indexing:constraint_classification(dionysiac_frenzy, rope,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the coordination function (establishing a religion)
% and the severe asymmetric extraction (mortals suffer for the god's glory).
% It requires active enforcement by Dionysus. This combination of coordination,
% extraction, and enforcement is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(dionysiac_frenzy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dionysiac_frenzy_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(dionysiac_frenzy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dionysiac_frenzy, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(dionysiac_frenzy, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    narrative_ontology:constraint_metric(dionysiac_frenzy, extractiveness, E),
    E >= 0.46.

:- end_tests(dionysiac_frenzy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The core of this constraint is the massive perspectival gap between the god
 * Dionysus and his mortal followers.
 * - EXTRACTION (0.80): The flow of value is almost entirely one-way. Mortals
 *   lose their agency, social roles, and even their lives (Pentheus). The god
 *   receives worship, recognition, and "due meed of majesty."
 * - SUPPRESSION (0.90): Alternatives to joining the rites are actively and
 *   violently suppressed. Pentheus's attempt to enforce civic law is met with
 *   divine power that destroys his palace and his mind.
 * - PERSPECTIVAL GAP:
 *   - For the powerless Maenads, the experience is totalizing and inescapable.
 *     While it feels like a law of nature (Mountain), the system classifies it
 *     as a Snare due to the extremely high effective extraction.
 *   - For the institutional Dionysus, the frenzy is a mere "harness," a tool
 *     for coordinating his followers and establishing his religion (Rope).
 *   - The analytical observer sees both sides. The system has a coordination
 *     function (beneficiary exists) but also brutal asymmetric extraction
 *     (victims exist) and requires active enforcement. This is a canonical
 *     Tangled Rope.
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * An early analysis might classify this as a Mountain from the victim's view
 * and a Snare from an observer's. The Tangled Rope classification is crucial
 * because it prevents this mislabeling. It acknowledges the genuine (if
 * coercive) coordination function of establishing a religious order, while
 * simultaneously accounting for the violent extraction imposed on its victims.
 * It correctly identifies the structure as a hybrid, not pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_dionysiac_frenzy,
    'Is Dionysus a literal metaphysical entity or a metaphor for socio-psychological mass hysteria?',
    'Empirical verification of divine intervention vs. historical/anthropological evidence of ecstatic cult phenomena.',
    'If literal entity, the constraint is a true natural_law (Mountain). If metaphor, it is a constructed social phenomenon (Snare/Tangled Rope).',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(dionysiac_frenzy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. This models the frenzy's arrival and
% intensification in Thebes over the course of the play.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time (remains low, the frenzy is brutally functional):
narrative_ontology:measurement(dionysiac_frenzy_tr_t0, dionysiac_frenzy, theater_ratio, 0, 0.05).
narrative_ontology:measurement(dionysiac_frenzy_tr_t5, dionysiac_frenzy, theater_ratio, 5, 0.08).
narrative_ontology:measurement(dionysiac_frenzy_tr_t10, dionysiac_frenzy, theater_ratio, 10, 0.10).

% Extraction over time (intensifies as Dionysus's punishment escalates):
narrative_ontology:measurement(dionysiac_frenzy_ex_t0, dionysiac_frenzy, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(dionysiac_frenzy_ex_t5, dionysiac_frenzy, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(dionysiac_frenzy_ex_t10, dionysiac_frenzy, base_extractiveness, 10, 0.80).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The frenzy is a mechanism to enforce the god's authority and punish dissent.
narrative_ontology:coordination_type(dionysiac_frenzy, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */