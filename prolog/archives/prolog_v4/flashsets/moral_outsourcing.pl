% ============================================================================
% CONSTRAINT STORY: moral_outsourcing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_moral_outsourcing, []).

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
 *   constraint_id: moral_outsourcing
 *   human_readable: The Ethical Externalization Loop
 *   domain: social/economic/technological
 *
 * SUMMARY:
 *   The Ethical Externalization Loop describes a scenario where a system or
 *   institution delegates ethical agency by automating decision-making or
 *   deferring responsibility to algorithmic or bureaucratic frameworks. This
 *   process often leads to a reduction in individual ethical reflection and
 *   responsibility, as individuals come to rely on external systems for
 *   guidance. The loop is perpetuated as institutions benefit from the
 *   efficiency and reduced liability, further incentivizing the outsourcing
 *   of ethical considerations.
 *
 * KEY AGENTS:
 *   - Institutions: Primary beneficiary (institutional/arbitrage) – benefit from reduced liability and increased efficiency.
 *   - Algorithmic Systems: Secondary beneficiary (powerful/constrained) - gain increased influence and adoption.
 *   - Individual Ethical Agency: Primary victim (powerless/trapped) – loses capacity for ethical decision-making.
 *   - Societal Moral Fabric: Secondary victim (powerless/trapped) - erosion of collective ethical standards.
 *   - Ethical Professionals: Constrained actors (moderate/constrained) - grapple with the conflict between personal ethics and system demands.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(moral_outsourcing, 0.55).
domain_priors:suppression_score(moral_outsourcing, 0.6).
domain_priors:theater_ratio(moral_outsourcing, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(moral_outsourcing, extractiveness, 0.55).
narrative_ontology:constraint_metric(moral_outsourcing, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(moral_outsourcing, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(moral_outsourcing, tangled_rope).
narrative_ontology:human_readable(moral_outsourcing, "The Ethical Externalization Loop").
narrative_ontology:topic_domain(moral_outsourcing, "social/economic/technological").

domain_priors:requires_active_enforcement(moral_outsourcing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(moral_outsourcing, institutions).
narrative_ontology:constraint_beneficiary(moral_outsourcing, algorithmic_systems).
narrative_ontology:constraint_victim(moral_outsourcing, individual_ethical_agency).
narrative_ontology:constraint_victim(moral_outsourcing, societal_moral_fabric).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Individuals find their ethical decision-making capacity eroded by the reliance on external systems, leading to a feeling of being trapped in a morally compromised landscape.
constraint_indexing:constraint_classification(moral_outsourcing, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Professionals (e.g., lawyers, doctors) who are meant to uphold ethical standards find themselves constrained by the systems they operate within but also derive benefits from them.
constraint_indexing:constraint_classification(moral_outsourcing, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Institutions benefit from the efficiency and reduced liability that comes with outsourcing ethical decisions, seeing it as a coordination mechanism.
constraint_indexing:constraint_classification(moral_outsourcing, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Oversight boards, while intended to provide ethical guidance, often become performative, with real ethical considerations being sidelined due to bureaucratic processes and institutional pressures. They are constrained in their ability to affect real change.
constraint_indexing:constraint_classification(moral_outsourcing, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% An analytical observer recognizes the complex interplay of coordination and extraction, identifying the systemic incentives and disincentives that perpetuate the ethical externalization loop.
constraint_indexing:constraint_classification(moral_outsourcing, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(moral_outsourcing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(moral_outsourcing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(moral_outsourcing, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(moral_outsourcing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(moral_outsourcing, TR),
    TR >= 0.70.

:- end_tests(moral_outsourcing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The system extracts ethical responsibility from individuals and diffuses it across the institution/algorithm. Suppression (0.60): High. Individuals feel pressured to conform to the externally imposed ethical frameworks, suppressing their own moral reasoning. Theater Ratio (0.40): Moderate. There is a performative aspect to ethical compliance, with institutions emphasizing adherence to external codes while potentially overlooking deeper ethical considerations.
 *
 * PERSPECTIVAL GAP:
 *   Individuals see the system as a snare, trapping them in a landscape where their own ethical agency is undermined. Institutions view it as a rope, facilitating coordination and efficient decision-making. Ethical Professionals see a tangled rope, balancing their personal ethics with the constraints of the system.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutions, with arbitrage options, benefit from the reduced liability and increased efficiency that come with outsourcing ethical decisions, experiencing the constraint as a coordination mechanism. Individuals, with few exit options, find their ethical decision-making capacity eroded, feeling trapped in a morally compromised landscape. Ethical Professionals, constrained but not entirely powerless, experience a mix of coordination and extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithm_bias_detection,
    'How effectively can algorithmic biases be detected and mitigated, and what recourse is available when they cause ethical harms?',
    'Developing robust testing methodologies and establishing clear lines of responsibility for algorithmic outcomes.',
    'If biases are easily mitigated, the constraint shifts toward a rope. If not, the constraint becomes a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithm_bias_detection, empirical, 'Effectiveness of algorithmic bias detection and mitigation').

omega_variable(
    institutional_accountability,
    'To what extent are institutions willing to be held accountable for ethical harms resulting from outsourced decision-making?',
    'Legislative action, public pressure, and changes to institutional governance structures.',
    'Increased accountability shifts the balance of power and reduces the overall extractiveness. Lack of accountability reinforces the snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_accountability, preference, 'Degree of institutional accountability for ethical harms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(moral_outsourcing, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mora_tr_t0, moral_outsourcing, theater_ratio, 0, 0.25).
narrative_ontology:measurement(mora_tr_t5, moral_outsourcing, theater_ratio, 5, 0.35).
narrative_ontology:measurement(mora_tr_t10, moral_outsourcing, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(mora_be_t0, moral_outsourcing, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(mora_be_t5, moral_outsourcing, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(mora_be_t10, moral_outsourcing, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(moral_outsourcing, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
