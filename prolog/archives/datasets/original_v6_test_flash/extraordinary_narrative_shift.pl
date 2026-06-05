% ============================================================================
% CONSTRAINT STORY: extraordinary_narrative_shift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-03-07
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_extraordinary_narrative_shift, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: extraordinary_narrative_shift
 *   human_readable: The Narrative Framing of "Extraordinary" Experience
 *   domain: social/psychological
 *
 * SUMMARY:
 *   This constraint describes the social and psychological mechanism by which
 *   experiences are framed as "extraordinary" or "ordinary". This framing
 *   process can be used to control the interpretation of experiences and
 *   extract value from them. The narrative framing process exhibits mixed
 *   coordination and extraction. Narrative entrepreneurs shape the dominant
 *   narrative, influencing public perception and extracting social and
 *   economic value. However, the process also involves a degree of
 *   coordination, as shared narratives can foster social cohesion.
 *
 * KEY AGENTS:
 *   - Experiencers: Primary target (powerless/trapped) - Subject to narrative framing.
 *   - Narrative Entrepreneurs: Primary beneficiary (institutional/arbitrage) - Control narrative framing.
 *   - Local Community: Secondary actor (moderate/constrained) - Influenced by narrative framing, also shapes it.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(extraordinary_narrative_shift, 0.5).
domain_priors:suppression_score(extraordinary_narrative_shift, 0.6).
domain_priors:theater_ratio(extraordinary_narrative_shift, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(extraordinary_narrative_shift, extractiveness, 0.5).
narrative_ontology:constraint_metric(extraordinary_narrative_shift, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(extraordinary_narrative_shift, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(extraordinary_narrative_shift, tangled_rope).
narrative_ontology:human_readable(extraordinary_narrative_shift, "The Narrative Framing of \"Extraordinary\" Experience").
narrative_ontology:topic_domain(extraordinary_narrative_shift, "social/psychological").

domain_priors:requires_active_enforcement(extraordinary_narrative_shift).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(extraordinary_narrative_shift, narrative_entrepreneurs).
narrative_ontology:constraint_victim(extraordinary_narrative_shift, experiencers).
narrative_ontology:constraint_victim(extraordinary_narrative_shift, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The individual who had the experience may find themselves trapped by the dominant narrative, unable to express their experience authentically.
constraint_indexing:constraint_classification(extraordinary_narrative_shift, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% The local community might benefit from the unique identity the "extraordinary" experience brings but are also constrained by the narrative, potentially suppressing alternative interpretations.
constraint_indexing:constraint_classification(extraordinary_narrative_shift, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Those who actively shape the narrative (e.g., media, religious institutions) benefit from the control and influence it provides.
constraint_indexing:constraint_classification(extraordinary_narrative_shift, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% From an analytical perspective, the narrative framing process involves a complex interplay of coordination (shared meaning) and extraction (control of interpretation).
constraint_indexing:constraint_classification(extraordinary_narrative_shift, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(extraordinary_narrative_shift_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(extraordinary_narrative_shift, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(extraordinary_narrative_shift, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(extraordinary_narrative_shift, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(extraordinary_narrative_shift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.50): Moderate. Narrative entrepreneurs extract value (influence, prestige, economic gain) from shaping the narrative, but the process also provides a coordinating function for society. Suppression (0.60): Moderate-high. Dominant narratives suppress alternative interpretations of experiences, creating pressure to conform. Theater ratio (0.30): Low. While some performative aspects exist (e.g., sensationalism), the narrative framing process also serves functional purposes.
 *
 * PERSPECTIVAL GAP:
 *   The experiencer feels trapped, the narrative shapers benefit, the community both benefits and is constrained, the analyst sees both coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (narrative entrepreneurs) have lower directionality, victims (experiencers) have higher directionality, based on their relative power and exit options.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authenticity_threshold,
    'What threshold of deviation from dominant narratives renders an experience ''extraordinary''?',
    'Sociological study of narrative thresholds, statistical analysis of experience reports',
    'High threshold -> more experiences pathologized. Low threshold -> narrative dilution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authenticity_threshold, empirical, 'Deviation threshold that defines ''extraordinary''').

omega_variable(
    narrative_control_mechanism,
    'To what extent is narrative control intentional vs emergent?',
    'Historical and ethnographic studies of narrative construction.',
    'High intentional control -> more snare-like. High emergent control -> more rope-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrative_control_mechanism, conceptual, 'Intentionality of narrative construction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(extraordinary_narrative_shift, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(extr_tr_t0, extraordinary_narrative_shift, theater_ratio, 0, 0.1).
narrative_ontology:measurement(extr_tr_t5, extraordinary_narrative_shift, theater_ratio, 5, 0.2).
narrative_ontology:measurement(extr_tr_t10, extraordinary_narrative_shift, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(extr_be_t0, extraordinary_narrative_shift, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(extr_be_t5, extraordinary_narrative_shift, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(extr_be_t10, extraordinary_narrative_shift, base_extractiveness, 10, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(extraordinary_narrative_shift, information_standard).
narrative_ontology:affects_constraint(extraordinary_narrative_shift, groupthink).
narrative_ontology:affects_constraint(extraordinary_narrative_shift, confirmation_bias).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
