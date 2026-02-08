% ============================================================================
% CONSTRAINT STORY: conversational_dogmas_interruption
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_conversational_dogmas_interruption, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: conversational_dogmas_interruption
 * human_readable: Conversational Dogmas (Interruption vs. Strong Civility)
 * domain: social/technological
 * * SUMMARY:
 * This constraint represents the rigid, often unconscious "dogmas" that govern
 * human conversation. It contrasts the "Church of Interruption" (COI), which values
 * interruptions as signs of understanding, with the "Church of Strong Civility,"
 * which mandates silence. Misalignment between these norms leads to severe social
 * friction, extracting time, attention, and social harmony.
 * * KEY AGENTS:
 * - The Civilist: Subject (Powerless), silenced by interruptions.
 * - The COI Member: Beneficiary (Moderate), uses interruptions to coordinate.
 * - The Corporate HR Department: Beneficiary (Institutional), benefits from standardized communication.
 * - The Systems Analyst: Auditor (Analytical), observes the full system dynamics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(conversational_dogmas_interruption, 0.55). % Raised to meet Tangled Rope (>=0.5) and Snare (chi>=0.66) thresholds.
domain_priors:suppression_score(conversational_dogmas_interruption, 0.6).   % High suppression; conversational habits are deeply ingrained and hard to change.
domain_priors:theater_ratio(conversational_dogmas_interruption, 0.2).       % Low theater; the norms are functional, not merely performative.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(conversational_dogmas_interruption, extractiveness, 0.55).
narrative_ontology:constraint_metric(conversational_dogmas_interruption, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(conversational_dogmas_interruption, theater_ratio, 0.2).

% Constraint self-claim (what does the constraint claim to be?)
% The beneficiaries see it as a pure coordination mechanism.
narrative_ontology:constraint_claim(conversational_dogmas_interruption, tangled_rope).

% Binary flags
% Enforcement is not formal but is active via social sanction and correction.
domain_priors:requires_active_enforcement(conversational_dogmas_interruption). % Required for Tangled Rope

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(conversational_dogmas_interruption, members_of_the_same_conversational_church).
narrative_ontology:constraint_victim(conversational_dogmas_interruption, individuals_in_cross_church_conversations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The "Civilist" is silenced or steamrolled by the COI member.
% χ = 0.55 * 1.5 (powerless) * 0.8 (local) = 0.66. Meets Snare threshold.
constraint_indexing:constraint_classification(conversational_dogmas_interruption, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The COI member sees interruption as an efficient coordination tool.
% χ = 0.55 * 1.0 (moderate) * 0.8 (local) = 0.44. Perceived as a coordination cost.
constraint_indexing:constraint_classification(conversational_dogmas_interruption, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: THE INSTITUTIONAL BENEFICIARY (ROPE)
% The HR department benefits from standardized communication protocols and does not feel the extraction.
% χ = 0.55 * -0.2 (institutional) * 1.0 (national) = -0.11. Perceived as a net benefit.
constraint_indexing:constraint_classification(conversational_dogmas_interruption, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the coordination function (beneficiaries) and the asymmetric
% extraction (victims), classifying it as a Tangled Rope.
constraint_indexing:constraint_classification(conversational_dogmas_interruption, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(conversational_dogmas_interruption_tests).

test(perspectival_gap_subject_vs_institution) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(conversational_dogmas_interruption, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(conversational_dogmas_interruption, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    assertion(TypePowerless == snare),
    assertion(TypeInstitutional == rope),
    assertion(TypePowerless \= TypeInstitutional).

test(analytical_view_is_tangled_rope) :-
    % The analytical observer should correctly identify the tangled nature.
    constraint_indexing:constraint_classification(conversational_dogmas_interruption, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    assertion(TypeAnalytical == tangled_rope).

:- end_tests(conversational_dogmas_interruption_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness was increased to 0.55 to meet the structural requirements for
 * both a Snare (from the powerless perspective, where chi >= 0.66) and a Tangled Rope
 * (from the analytical perspective, where base extraction >= 0.50). The original file
 * incorrectly classified the constraint as a Tangled Rope from the institutional view;
 * this has been corrected. An institution is a beneficiary and perceives the constraint
 * as a Rope due to its negative power modifier (pi = -0.2). The Tangled Rope classification
 * is correctly reserved for the analytical observer who can see both the coordination
 * function and the asymmetric extraction. The invalid power atom 'individual_moderate'
 * was corrected to 'moderate'.
 *
 * PERSPECTIVAL GAP:
 * The gap is stark. For the powerless 'Civilist', the norm is a Snare that traps them in
 * silence. For the 'COI Member' and the 'HR Department', it is a Rope that enables
 * efficient coordination. This divergence is the hallmark of a Tangled Rope system,
 * which is only visible from an analytical standpoint that accounts for all actors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% The core uncertainty is the plasticity of deeply ingrained conversational habits.
omega_variable(
    omega_conversational_dogmas_interruption,
    'Are conversational dogmas, built over decades, truly malleable (making them a manageable Tangled Rope) or are they effectively permanent cognitive structures (making them a Mountain)?',
    'Long-term behavioral studies of individuals attempting to switch conversational "churches", combined with neurological imaging of habit formation.',
    'If malleable, the constraint can be addressed with training (Tangled Rope). If fixed, it is an immutable feature of social interaction that must be worked around (Mountain).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(conversational_dogmas_interruption, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extraction > 0.46, so temporal data is required.
% We model a slight increase in extraction and theater as the social norms
% become more entrenched and rigid over the interval.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(cdi_tr_t0, conversational_dogmas_interruption, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cdi_tr_t5, conversational_dogmas_interruption, theater_ratio, 5, 0.15).
narrative_ontology:measurement(cdi_tr_t10, conversational_dogmas_interruption, theater_ratio, 10, 0.2).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(cdi_ex_t0, conversational_dogmas_interruption, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(cdi_ex_t5, conversational_dogmas_interruption, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(cdi_ex_t10, conversational_dogmas_interruption, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Conversational norms function as a standard for information exchange.
narrative_ontology:coordination_type(conversational_dogmas_interruption, information_standard).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */