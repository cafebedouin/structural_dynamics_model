% ============================================================================
% CONSTRAINT STORY: theory_of_visitors
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_theory_of_visitors, []).

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
 * * constraint_id: theory_of_visitors
 * human_readable: The Theory of Visitors (Relationship Transience)
 * domain: social/psychological
 * * SUMMARY:
 * This constraint defines all human relationships as inherently transient "visitors" who arrive for a limited time and inevitably depart. It posits a fundamental trade-off: visitors take something irreplaceable from the host upon departure (extraction), but leave "souvenirs" (memories/lessons) that can be kept forever (coordination benefit). The theory is enforced through cultural narratives that frame acceptance of this transience as maturity.
 * * KEY AGENTS:
 * - The Host (Powerless): The agent who experiences the loss of a departing "visitor".
 * - Social Media Platforms (Institutional): Entities that monetize the high churn rate of transient connections.
 * - The Analytical Observer (Analytical): An auditor evaluating the theory's net effect on well-being.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(theory_of_visitors, 0.65). % High extraction: visitors "take something from you... that you can’t ever get back."
domain_priors:suppression_score(theory_of_visitors, 0.50).   % Suppresses the alternative of seeking permanent relationships.
domain_priors:theater_ratio(theory_of_visitors, 0.10).       % The theory is functional, not primarily performative.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(theory_of_visitors, extractiveness, 0.65).
narrative_ontology:constraint_metric(theory_of_visitors, suppression_requirement, 0.50).
narrative_ontology:constraint_metric(theory_of_visitors, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(theory_of_visitors, tangled_rope).
narrative_ontology:human_readable(theory_of_visitors, "The Theory of Visitors (Relationship Transience)").

% Binary flags
domain_priors:requires_active_enforcement(theory_of_visitors). % Required for Tangled Rope; enforced by social norms and platforms.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(theory_of_visitors, stoics_and_navigators). % Those who use the theory for emotional resilience.
narrative_ontology:constraint_victim(theory_of_visitors, hosts_seeking_permanence). % Those who suffer the irreplaceable loss.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   ========================================================================== */

% PERSPECTIVE 1: THE HOST / MOURNER (SNARE)
% For the individual experiencing loss, transience is a Snare. The visitor
% extracts an irreplaceable part of the self and leaves, strangling the
% host's sense of continuity and meaning.
constraint_indexing:constraint_classification(theory_of_visitors, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE SOCIAL MEDIA PLATFORM (ROPE)
% For an institution that monetizes high-churn interactions (e.g., dating apps),
% the theory is a perfect Rope. It normalizes the rapid formation and dissolution
% of ties, which is the core of their business model.
constraint_indexing:constraint_classification(theory_of_visitors, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The theory is a Tangled Rope. It provides a genuine coordination function
% (helping people manage the pain of loss) while also enabling and justifying
% asymmetric extraction (the irreplaceable loss suffered by the host, which
% benefits platforms that thrive on churn).
constraint_indexing:constraint_classification(theory_of_visitors, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(theory_of_visitors_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(theory_of_visitors, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(theory_of_visitors, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation_high_extraction) :-
    % Verify the constraint is correctly identified as high-extraction.
    narrative_ontology:constraint_metric(theory_of_visitors, extractiveness, E),
    E >= 0.46.

test(tangled_rope_conditions_met) :-
    % Verify the analytical observer correctly classifies as tangled_rope.
    constraint_indexing:constraint_classification(theory_of_visitors, tangled_rope, context(agent_power(analytical), _, _, _)).

:- end_tests(theory_of_visitors_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness of 0.65 reflects the narrative's core claim of an "irreplaceable" loss. The suppression score of 0.5 reflects its function in discouraging the pursuit of "forever" relationships.
 * The perspectival gap is stark: for the powerless individual experiencing loss, it is a brutal Snare (χ = 0.65 * 1.5 * 0.8 = 0.78). For the institutional beneficiary (a dating app), the extraction is negligible and the coordination function is paramount, making it a Rope (χ = 0.65 * -0.2 * 1.2 = -0.156, effectively zero).
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * This constraint is a classic case of potential Mandatrophy. A purely extractive view would label it a Snare for everyone. A purely coordination view would label it a Rope. The Tangled Rope classification, from the analytical perspective, correctly resolves this by acknowledging both functions simultaneously: it is a coping mechanism (coordination) that also normalizes and facilitates a system of emotional extraction (asymmetric extraction), especially when amplified by digital platforms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% The core uncertainty is whether the "souvenirs" (lessons, memories) truly
% compensate for the irrecoverable loss, or if the theory is merely a coping
% mechanism for a terminal Snare.
omega_variable(
    omega_theory_of_visitors,
    'Is the value of the "souvenir" sufficient to offset the irrecoverable loss, or is the theory a narrative coping mechanism for a terminal Snare?',
    'Longitudinal tracking of well-being and psychological resilience in individuals who adopt the theory versus those who reject it.',
    'If souvenirs offset loss, the theory is a functional Rope for growth. If loss is terminal, the theory is a Snare masquerading as a Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(theory_of_visitors, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This models the theory's intensification as it is co-opted by digital
% platforms that monetize relationship churn. What began as a philosophical
% coping mechanism becomes an economic engine.
%
% Theater ratio over time (stable and low):
narrative_ontology:measurement(theory_of_visitors_tr_t0, theory_of_visitors, theater_ratio, 0, 0.10).
narrative_ontology:measurement(theory_of_visitors_tr_t5, theory_of_visitors, theater_ratio, 5, 0.10).
narrative_ontology:measurement(theory_of_visitors_tr_t10, theory_of_visitors, theater_ratio, 10, 0.10).

% Extraction over time (extraction_accumulation drift):
narrative_ontology:measurement(theory_of_visitors_ex_t0, theory_of_visitors, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(theory_of_visitors_ex_t5, theory_of_visitors, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(theory_of_visitors_ex_t10, theory_of_visitors, base_extractiveness, 10, 0.65).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The theory acts as a standard for interpreting social interactions and
% managing expectations.
narrative_ontology:coordination_type(theory_of_visitors, information_standard).

% This theory structurally enables and reinforces business models based on
% high user churn and transient engagement.
narrative_ontology:affects_constraint(theory_of_visitors, digital_dating_monetization).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */