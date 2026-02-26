% ============================================================================
% CONSTRAINT STORY: cinderella_midnight_deadline
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cinderella_midnight_deadline, []).

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
 *   constraint_id: cinderella_midnight_deadline
 *   human_readable: The Fairy Godmother's Midnight Deadline
 *   domain: magical/social
 *
 * SUMMARY:
 *   A Fairy Godmother grants Cinderella a temporary, high-status
 *   transformation (coach, gown, glass slippers) to attend a royal ball,
 *   enabling a brief circumvention of her oppressive social reality. The
 *   transformation is bound by a single, absolute constraint: it will expire
 *   at the stroke of midnight, at which point all magical items will revert
 *   to their mundane original forms. This constraint serves as the central
 *   tension of the narrative's climax.
 *
 * KEY AGENTS:
 *   - Cinderella: Primary beneficiary and victim (moderate/constrained) — receives the benefit of the transformation but bears the risk of the deadline.
 *   - Fairy Godmother: The constraint's author and enforcer (institutional/arbitrage) — provides the magical 'scaffolding' and sets its terms.
 *   - The Prince: Unaware observer (powerful/trapped) — interacts with the *effect* of the constraint (Cinderella's presence and sudden departure) but is ignorant of its cause, perceiving it as an absolute, inexplicable event.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cinderella_midnight_deadline, 0.28).
domain_priors:suppression_score(cinderella_midnight_deadline, 0.9).
domain_priors:theater_ratio(cinderella_midnight_deadline, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cinderella_midnight_deadline, extractiveness, 0.28).
narrative_ontology:constraint_metric(cinderella_midnight_deadline, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(cinderella_midnight_deadline, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cinderella_midnight_deadline, scaffold).
narrative_ontology:human_readable(cinderella_midnight_deadline, "The Fairy Godmother's Midnight Deadline").
narrative_ontology:topic_domain(cinderella_midnight_deadline, "magical/social").

domain_priors:requires_active_enforcement(cinderella_midnight_deadline).
narrative_ontology:has_sunset_clause(cinderella_midnight_deadline).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cinderella_midnight_deadline, cinderella).
narrative_ontology:constraint_beneficiary(cinderella_midnight_deadline, fairy_godmother).
narrative_ontology:constraint_victim(cinderella_midnight_deadline, cinderella).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CINDERELLA (SCAFFOLD) — For Cinderella, the magic is a temporary support structure enabling her to achieve a specific goal (attend the ball, meet the Prince). The deadline is the explicit sunset clause. She is both beneficiary (gains access) and victim (bears the risk of humiliating reversion). Her exit is constrained; she cannot alter the terms. The low effective extraction (χ ≈ 0.19) reflects the net positive utility despite the risk.
constraint_indexing:constraint_classification(cinderella_midnight_deadline, scaffold,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: FAIRY GODMOTHER (ROPE) — As the provider of the magic, the Fairy Godmother perceives the constraint as a pure coordination mechanism. She sets the terms (arbitrage) to solve a problem for her beneficiary. The deadline is simply a parameter of the solution. Her directionality is that of a pure beneficiary (d≈0.05), resulting in negative effective extraction (χ≈-0.03), the signature of a subsidy or gift.
constraint_indexing:constraint_classification(cinderella_midnight_deadline, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: THE PRINCE (MOUNTAIN) — From the Prince's perspective, Cinderella's sudden departure is an inexplicable, absolute event. He is powerful within his domain but trapped by his ignorance of the constraint's nature. He cannot negotiate with it, prevent it, or understand it. It appears as an immutable law of this specific situation. The engine will flag this as a false summit, as the constraint's base properties (ε=0.28, suppression=0.90, emerges_naturally=false) are inconsistent with a true Mountain.
constraint_indexing:constraint_classification(cinderella_midnight_deadline, mountain,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (SCAFFOLD) — The analytical view confirms the constraint's structural identity as a Scaffold. It is a temporary, enabling structure with a defined, non-negotiable end point, designed to facilitate a change in state for a specific agent. The combination of a clear coordination function, an explicit sunset clause, and low effective extraction confirms this classification.
constraint_indexing:constraint_classification(cinderella_midnight_deadline, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cinderella_midnight_deadline_tests).
:- end_tests(cinderella_midnight_deadline_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.28): Low. The constraint is fundamentally a gift. The 'extraction' represents the potential social cost and humiliation Cinderella faces if she fails to adhere to the deadline. It's a risk, not a direct transfer of value. Suppression (0.90): High. The deadline is absolute and non-negotiable. There are no alternative magical providers or ways to extend the time. Cinderella's only option is compliance. Theater Ratio (0.15): Low. While the transformation itself is high-theater, the *deadline* is a brutally functional, non-performative rule. Its enforcement is automatic and devoid of ritual.
 *
 * PERSPECTIVAL GAP:
 *   The gap is driven by information asymmetry. Cinderella, knowing the terms, sees a temporary support structure (Scaffold). The Fairy Godmother, setting the terms, sees a coordination tool (Rope). The Prince, knowing nothing of the terms, experiences the consequence as an unchangeable fact of his reality (Mountain). This demonstrates how an artificial, contingent rule can appear as a natural law to those outside its operational context.
 *
 * DIRECTIONALITY LOGIC:
 *   Cinderella is declared as both beneficiary and victim. She gains the opportunity but also carries the entire risk of the spell's collapse. This dual role is characteristic of agents using temporary, high-stakes support systems. The Fairy Godmother is a beneficiary, as her goal (Cinderella's happiness) is advanced. This structural relationship correctly derives a low-to-moderate directionality (d) for Cinderella and a very low d for the Godmother, producing the Scaffold and Rope classifications respectively.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a canonical example of a Scaffold. It avoids mandatrophy by correctly identifying the structure as temporary and enabling, rather than purely extractive (Snare) or permanent (Mountain). The Prince's 'Mountain' perspective highlights a common failure mode: mistaking the local, observable effects of a contingent, man-made rule for an immutable law of nature. The DR system correctly identifies this as a 'false summit' by checking the constraint's underlying structural properties, which lack the signature of a natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    magical_law_vs_arbitrary_rule,
    'Is the midnight deadline an inherent, immutable limitation of the Fairy Godmother''s magic, or is it an arbitrary rule she imposed for pedagogical or narrative reasons?',
    'Comparative analysis of magical systems in the story''s universe; direct testimony from the Fairy Godmother.',
    'If it''s an inherent limitation, the constraint has Mountain-like properties, and the Fairy Godmother''s power is less absolute. If it''s an arbitrary rule, it is purely a Scaffold, and the suppression is a feature of her choice, not a necessity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(magical_law_vs_arbitrary_rule, conceptual, 'Whether the deadline is a fundamental limit of magic or an imposed rule.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cinderella_midnight_deadline, 2000, 2400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cinderella_midnight_deadline, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
