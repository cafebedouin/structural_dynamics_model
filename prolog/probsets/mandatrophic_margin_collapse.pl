% ============================================================================
% CONSTRAINT STORY: mandatrophic_margin_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandatrophic_margin_collapse, []).

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
 *   constraint_id: mandatrophic_margin_collapse
 *   human_readable: Mandatrophy (The Extraction of Margin for Mandate)
 *   domain: institutional/technological
 *
 * SUMMARY:
 *   Mandatrophy is the systemic wasting away of resilience caused by the
 *   rigid prioritization of a top-down administrative mandate over the
 *   organic margins (buffers, redundancies, or 'slack') required for
 *   survival. This often results in an unsustainable system where the central
 *   mandate triumphs at the expense of the underlying components and their
 *   ability to function effectively over time. The core dynamic involves a
 *   transfer of resources or 'slack' from the operational level to a
 *   centralized authority, often with the intention of improving efficiency
 *   or control, but in practice this extraction erodes the system's capacity
 *   to adapt to unexpected challenges or innovate locally.
 *
 * KEY AGENTS:
 *   - Central Authorities: Primary beneficiaries (institutional/arbitrage) – They receive extracted margin, enhancing their perceived efficiency and control.
 *   - Decentralized Innovators: Primary victims (powerless/trapped) – They lose their margin, reducing their resilience and adaptability.
 *   - Implementers of the Central Mandate: Secondary actors (institutional/constrained) – They are constrained in their implementation, experiencing the trade-offs between mandate adherence and organic resilience.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandatrophic_margin_collapse, 0.75).
domain_priors:suppression_score(mandatrophic_margin_collapse, 0.8).
domain_priors:theater_ratio(mandatrophic_margin_collapse, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandatrophic_margin_collapse, extractiveness, 0.75).
narrative_ontology:constraint_metric(mandatrophic_margin_collapse, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(mandatrophic_margin_collapse, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandatrophic_margin_collapse, snare).
narrative_ontology:human_readable(mandatrophic_margin_collapse, "Mandatrophy (The Extraction of Margin for Mandate)").
narrative_ontology:topic_domain(mandatrophic_margin_collapse, "institutional/technological").

domain_priors:requires_active_enforcement(mandatrophic_margin_collapse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandatrophic_margin_collapse, central_authorities).
narrative_ontology:constraint_victim(mandatrophic_margin_collapse, decentralized_innovators).
narrative_ontology:constraint_victim(mandatrophic_margin_collapse, organic_resilience).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of those whose organic resilience is suppressed. They are trapped and bear the full brunt of the mandate.
constraint_indexing:constraint_classification(mandatrophic_margin_collapse, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Perspective of those implementing the mandate. They are constrained but benefit as institutions so it classifies as a piton - formally rope but functionally extracting.
constraint_indexing:constraint_classification(mandatrophic_margin_collapse, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective of those who institute the mandate. It classifies as a rope from their perspective because they are extracting from another source and benefit.
constraint_indexing:constraint_classification(mandatrophic_margin_collapse, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical observer sees both extraction and coordination resulting in a tangled rope.
constraint_indexing:constraint_classification(mandatrophic_margin_collapse, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandatrophic_margin_collapse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mandatrophic_margin_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mandatrophic_margin_collapse, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandatrophic_margin_collapse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(mandatrophic_margin_collapse, TR),
    TR >= 0.70.

:- end_tests(mandatrophic_margin_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75): High. A large portion of operational margin is extracted by the central mandate, leaving little room for local adaptation or innovation. Suppression (0.80): High. The mandate actively suppresses alternative approaches or decentralized initiatives, requiring strict adherence to centralized directives. Theater ratio (0.75): High. Some performative adherence to the mandate is observed, where symbolic actions are taken to demonstrate compliance, even if they do not contribute to the mandate's actual goals. Theater has increased over time due to the loss of organic innovation.
 *
 * PERSPECTIVAL GAP:
 *   The central authorities classify this as a rope, a coordination mechanism to improve efficiency and control. Decentralized innovators experience it as a snare, where their resilience and adaptability are systemically undermined. Implementers see it as a degraded institution (piton). The analytical observer understands the mixed motives/outcomes leading to a tangled rope classification.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries are the central authorities. The victims are the decentralized innovators. The magnitude of extraction is significant. The victims have limited exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is a Snare, where what appears as necessary coordination (a rope) at the central authority level is actively extracting and suppressing organic innovation and resilience at the decentralized level. Resolution occurs when local innovation and local slack are valued against the value extracted by the central authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    central_authority_legitimacy,
    'Is the central authority considered legitimate by the decentralized innovators?',
    'Assess levels of trust and compliance within the system.',
    'If legitimate: extraction is accepted, and the system functions with some efficiency. If illegitimate: extraction leads to revolt and system failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(central_authority_legitimacy, conceptual, 'Whether the central authority is seen as legitimate.').

omega_variable(
    margin_visibility,
    'Is the organic margin visible and measurable?',
    'Develop metrics and auditing processes to track the margin.',
    'If visible: margin can be managed and protected. If invisible: margin is easily extracted without notice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(margin_visibility, empirical, 'The visibility of the margin').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandatrophic_margin_collapse, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandatrophic_margin_collapse, theater_ratio, 0, 0.2).
narrative_ontology:measurement(mand_tr_t2, mandatrophic_margin_collapse, theater_ratio, 2, 0.5).
narrative_ontology:measurement(mand_tr_t4, mandatrophic_margin_collapse, theater_ratio, 4, 0.75).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandatrophic_margin_collapse, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(mand_be_t2, mandatrophic_margin_collapse, base_extractiveness, 2, 0.5).
narrative_ontology:measurement(mand_be_t4, mandatrophic_margin_collapse, base_extractiveness, 4, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandatrophic_margin_collapse, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
