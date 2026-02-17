% ============================================================================
% CONSTRAINT STORY: litany_of_the_real
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_litany_of_the_real, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:omega_variable/3,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: litany_of_the_real
 *   human_readable: The Litany of the Real (Deferential Realism)
 *   domain: philosophical/cognitive
 *
 * SUMMARY:
 *   A formal set of cognitive constraints and linguistic protocols designed
 *   to align an agent's internal model with external reality. It functions
 *   as a meta-constraint: a rule for how to handle other rules, describing
 *   the fundamental, unchangeable nature of substrate.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Escapist: Primary target (powerless/trapped) — resents the unyielding nature of reality, experiencing it as a coercive limit.
 *   - The Practitioner: Primary beneficiary (powerless/mobile) — uses the Litany as a coordination tool to navigate reality effectively.
 *   - The Architect: Analytical observer (analytical/analytical) — views the Litany as a description of natural law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Extractiveness is very low; the Litany describes reality, it does not
% extract from the agent. The perceived extraction is of false hope.
domain_priors:base_extractiveness(litany_of_the_real, 0.08).
% Suppression is at the maximum for a Mountain. The Litany does not suppress
% alternatives; reality itself does. Alternatives are "pretending."
domain_priors:suppression_score(litany_of_the_real, 0.05).
% As a philosophical tool, its function is its substance. Low theater.
domain_priors:theater_ratio(litany_of_the_real, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(litany_of_the_real, extractiveness, 0.08).
narrative_ontology:constraint_metric(litany_of_the_real, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(litany_of_the_real, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl.
% Alternatives are conceivable ("pretending") but structurally inaccessible.
narrative_ontology:constraint_metric(litany_of_the_real, accessibility_collapse, 0.95).
% The "escapist" represents resistance, but it is framed as futile.
narrative_ontology:constraint_metric(litany_of_the_real, resistance, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(litany_of_the_real, mountain).
narrative_ontology:human_readable(litany_of_the_real, "The Litany of the Real (Deferential Realism)").

% --- Binary flags ---
% The law emerges naturally, but alignment with it requires active practice.
domain_priors:requires_active_enforcement(litany_of_the_real).

% --- Emergence flag (required for mountain constraints) ---
% It describes the "lattice of life" which emerges without human design.
domain_priors:emerges_naturally(litany_of_the_real).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(litany_of_the_real, practitioner).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(litany_of_the_real, escapist).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE ESCAPIST (MOUNTAIN)
% Agent who resists the constraint. The feeling of being trapped comes from
% pushing against an unchangeable reality, not from extraction.
constraint_indexing:constraint_classification(litany_of_the_real, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRACTITIONER (ROPE)
% Agent who uses the Litany for navigation. For this agent, it is a pure
% coordination tool for aligning action with reality.
% Engine derives low d from beneficiary + mobile exit -> low χ -> Rope.
constraint_indexing:constraint_classification(litany_of_the_real, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: THE ARCHITECT (MOUNTAIN)
% The institutional voice defining the framework. From this perspective, the
% Litany is not a choice but a description of natural law.
constraint_indexing:constraint_classification(litany_of_the_real, mountain,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (MOUNTAIN)
% Default analytical context. The metrics (ε=0.08, supp=0.05) and natural
% law profile firmly place the constraint in the Mountain category.
constraint_indexing:constraint_classification(litany_of_the_real, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(litany_of_the_real_tests).

test(perspectival_gap_practitioner_vs_architect) :-
    % Verify the gap between the practitioner (who uses it as a tool) and
    % the architect (who sees its fundamental nature).
    constraint_indexing:constraint_classification(litany_of_the_real, TypePractitioner,
        context(agent_power(powerless), time_horizon(biographical), exit_options(mobile), _)),
    constraint_indexing:constraint_classification(litany_of_the_real, TypeArchitect,
        context(agent_power(institutional), _, _, _)),
    assertion(TypePractitioner == rope),
    assertion(TypeArchitect == mountain),
    TypePractitioner \= TypeArchitect.

test(analytical_claim_is_mountain) :-
    narrative_ontology:constraint_claim(litany_of_the_real, mountain).

:- end_tests(litany_of_the_real_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The core challenge was reconciling the narrative's different emotional
 *   responses with a single set of metrics. The original file had metrics
 *   inconsistent with its classifications. I resolved this by setting metrics
 *   that firmly establish the constraint as a Mountain (ε=0.08, supp=0.05)
 *   and adding the required Natural Law profile (accessibility_collapse, resistance).
 *
 * PERSPECTIVAL GAP:
 *   The primary gap is between the Practitioner (Rope) and all other observers
 *   (Mountain). The Practitioner, by accepting the constraint, transforms it
 *   into a coordination tool for navigating life. The Escapist and Architect
 *   both see its unchangeable nature (Mountain), but respond differently: the
 *   Escapist with resentment, the Architect with analytical acceptance. The
 *   negative feeling of the Escapist is re-interpreted not as evidence of a
 *   Snare (which the low metrics preclude), but as the friction of resisting
 *   a Mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: The 'practitioner' benefits by gaining a reliable map of reality,
 *     enabling effective action. This relationship, combined with mobile exit,
 *     derives a low directionality (d), leading to a Rope classification.
 *   - Victim: The 'escapist' bears the cost of confronting an unyielding reality
 *     that invalidates their desire for absolute freedom. This derives a high d,
 *     but because base extraction (ε) is so low, it does not result in a Snare.
 *
 * MANDATROPHY ANALYSIS:
 *   By classifying the constraint as a Mountain based on its objective metrics,
 *   the framework correctly identifies that the Escapist's negative experience
 *   is not due to asymmetric extraction. It prevents mislabeling a fundamental
 *   limit of reality as a predatory Snare, distinguishing between the pain of
 *   extraction and the pain of confronting necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_litany_of_the_real_1,
    'Is the Litany a true description of an ontological Mountain, or a highly effective philosophical Piton that has become reified as natural law?',
    'Identification of a civilization or agent that successfully operates on a fundamentally different set of physical/logical laws.',
    'If true Mountain, its principles are universal. If a Piton, it is a culturally contingent tool that could be replaced.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_litany_of_the_real_1, conceptual, 'Distinguishing a true ontological Mountain from a reified philosophical Piton.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(litany_of_the_real, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data is not required as base_extractiveness (0.08) is below the
% 0.46 threshold. This section is included as a placeholder.
%
% narrative_ontology:measurement(litany_of_the_real_tr_t0, litany_of_the_real, theater_ratio, 0, 0.08).
% narrative_ontology:measurement(litany_of_the_real_tr_t5, litany_of_the_real, theater_ratio, 5, 0.08).
% narrative_ontology:measurement(litany_of_the_real_tr_t10, litany_of_the_real, theater_ratio, 10, 0.08).
%
% narrative_ontology:measurement(litany_of_the_real_ex_t0, litany_of_the_real, base_extractiveness, 0, 0.08).
% narrative_ontology:measurement(litany_of_the_real_ex_t5, litany_of_the_real, base_extractiveness, 5, 0.08).
% narrative_ontology:measurement(litany_of_the_real_ex_t10, litany_of_the_real, base_extractiveness, 10, 0.08).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% No specific coordination type or network relationships are defined for this
% philosophical meta-constraint.
%
% narrative_ontology:coordination_type(litany_of_the_real, information_standard).
% narrative_ontology:affects_constraint(litany_of_the_real, other_constraint_id).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% The structural derivation from beneficiary/victim declarations is accurate
% for this constraint, so no overrides are needed.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */