% ============================================================================
% CONSTRAINT STORY: nl_gay_prime_minister_norm
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nl_gay_prime_minister_norm, []).

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
 *   constraint_id: nl_gay_prime_minister_norm
 *   human_readable: Societal Norm of Heteronormativity in National Leadership
 *   domain: political/social
 *
 * SUMMARY:
 *   The appointment of the first openly gay Prime Minister in the Netherlands
 *   is framed by LGBTQ+ organizations as a 'historic milestone' and an
 *   important signal of normalization. This event challenges the implicit
 *   societal constraint that leaders should conform to a heteronormative
 *   profile. However, this progress occurs against a backdrop of 'hardening'
 *   opposition and increasing anti-LGBTQ+ sentiment, creating a structural
 *   tension. The Prime Minister himself seeks to downplay his orientation as
 *   a private matter, resisting his symbolic role.
 *
 * KEY AGENTS:
 *   - LGBTQ+ Community: Primary beneficiary (of normalization) and primary victim (of backlash).
 *   - LGBTQ+ Advocacy Organizations: Organized beneficiary (leverages the event for coordination).
 *   - The Prime Minister: Powerful but constrained agent, experiencing both costs and benefits.
 *   - Socially Conservative Groups: Victims who perceive a loss of cultural dominance.
 *   - The Broader Public: The arena where the norm is contested and renegotiated.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nl_gay_prime_minister_norm, 0.45).
domain_priors:suppression_score(nl_gay_prime_minister_norm, 0.55).
domain_priors:theater_ratio(nl_gay_prime_minister_norm, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nl_gay_prime_minister_norm, extractiveness, 0.45).
narrative_ontology:constraint_metric(nl_gay_prime_minister_norm, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(nl_gay_prime_minister_norm, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nl_gay_prime_minister_norm, tangled_rope).
narrative_ontology:human_readable(nl_gay_prime_minister_norm, "Societal Norm of Heteronormativity in National Leadership").
narrative_ontology:topic_domain(nl_gay_prime_minister_norm, "political/social").

domain_priors:requires_active_enforcement(nl_gay_prime_minister_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nl_gay_prime_minister_norm, lgbtq_community).
narrative_ontology:constraint_beneficiary(nl_gay_prime_minister_norm, proponents_of_social_liberalism).
narrative_ontology:constraint_victim(nl_gay_prime_minister_norm, lgbtq_community_facing_backlash).
narrative_ontology:constraint_victim(nl_gay_prime_minister_norm, socially_conservative_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL (SNARE) — An LGBTQ+ person who experiences the 'hardening' of societal opposition mentioned in the article. For them, the high-profile appointment provokes a backlash that increases their personal risk. The benefit of 'normalization' is abstract, while the threat of violence is concrete. The constraint extracts safety from them. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.64.
constraint_indexing:constraint_classification(nl_gay_prime_minister_norm, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ADVOCACY ORG (ROPE) — For an organization like COC Nederland, the appointment is a powerful coordination signal. It establishes a new focal point for what is acceptable at the highest level of power, which they can leverage for further advocacy. They see it as a historic milestone that solves a coordination problem of visibility. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.34.
constraint_indexing:constraint_classification(nl_gay_prime_minister_norm, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE PRIME MINISTER (TANGLED ROPE) — The PM wishes for his orientation to be a non-issue. He experiences extraction via unwanted scrutiny and being made a symbol, a cost to his privacy and professional focus. He also benefits from the broad acceptance that allows his appointment. He is constrained by the public nature of his role. The situation has both costs and benefits he must navigate.
constraint_indexing:constraint_classification(nl_gay_prime_minister_norm, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SOCIAL CONSERVATIVE (SNARE) - From the perspective of groups invested in a traditional, heteronormative social order, this event represents an extractive loss of cultural power and the imposition of a liberal norm they oppose. They perceive the shift as a coercive constraint on their values and public expression.
constraint_indexing:constraint_classification(nl_gay_prime_minister_norm, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL (TANGLED ROPE) — The observer sees both the genuine coordination function (normalization of LGBTQ+ identity in power) and the asymmetric extraction (the costs of backlash are borne by the LGBTQ+ community, while cultural ground is extracted from conservatives). The coexistence of these two functions is the definition of a Tangled Rope. χ≈0.47
constraint_indexing:constraint_classification(nl_gay_prime_minister_norm, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nl_gay_prime_minister_norm_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nl_gay_prime_minister_norm, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nl_gay_prime_minister_norm, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(nl_gay_prime_minister_norm_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.45): High. This represents the significant costs imposed by the system. Historically, it was the cost of exclusion for LGBTQ+ individuals. In the current moment, it represents the cost of backlash and targeted hostility that is provoked by the progressive step. The measurement data shows this extractiveness declining over time, but it remains substantial. Suppression (0.55): Represents the ongoing contestation of LGBTQ+ rights and visibility. While the appointment signals a weakening of the old norm, the existence of a 'hardening' opposition shows that alternatives (full, uncontested acceptance) are still actively suppressed by certain social factions. Requires Active Enforcement (true): The new, more inclusive norm is not self-sustaining and requires active defense from advocacy groups and legal frameworks against social and political opposition.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is stark. Advocacy organizations see a Rope, a pure coordination victory that sets a new standard. Individuals on the receiving end of the backlash experience a Snare, where their identity makes them a target. Social conservatives also perceive a Snare, but one aimed at their values. The PM himself navigates a Tangled Rope, balancing the professional duty with the personal cost of becoming a symbol. The analytical view must synthesize these realities, leading to the Tangled Rope classification.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality is complex because one group is simultaneously a beneficiary and a victim. The 'lgbtq_community' benefits from the representational milestone (lowering their 'd' value). However, the 'lgbtq_community_facing_backlash' are victims of the resulting hostility (raising their 'd' value). This dual role is a hallmark of a Tangled Rope, where the benefits of a coordination good are not distributed evenly, and the costs of maintaining it fall asymmetrically on the very group it's meant to benefit. Socially conservative groups are purely victims, experiencing an extraction of cultural status.
 *
 * MANDATROPHY ANALYSIS:
 *   This analysis avoids two common errors. It does not misclassify the event as a pure Rope, which would ignore the real, documented costs of the backlash and paint a naively optimistic picture. It also avoids classifying it as a pure Snare, which would deny the genuine, important coordination function of normalization and representation. By identifying it as a Tangled Rope, the framework correctly models the phenomenon as a contested advance where progress and peril coexist and are asymmetrically distributed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    normalization_vs_tokenism,
    'Does this appointment represent a fundamental, durable normalization of LGBTQ+ identity in leadership, or is it a symbolic token that masks persistent underlying intolerance?',
    'Longitudinal analysis of public attitude surveys, hate crime statistics, and the political success of other openly LGBTQ+ candidates in the years following the term.',
    'If it proves to be durable normalization, the constraint evolves toward a Rope. If it''s tokenism and attitudes stagnate or worsen, it remains a Tangled Rope or degrades into a theatrical Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normalization_vs_tokenism, empirical, 'Whether the event leads to true normalization or is merely symbolic.').

omega_variable(
    backlash_dynamics,
    'Is the ''hardening'' of opposition a temporary, reactive spike, or the beginning of a sustained and organized counter-movement?',
    'Tracking membership and funding of anti-LGBTQ+ groups, and analyzing the rhetoric of opposing political parties over a multi-year period.',
    'A temporary spike means the Tangled Rope is successfully transitioning into a Rope. A sustained counter-movement means the extractive element is becoming more severe, potentially turning the constraint into a full Snare for the targeted community.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(backlash_dynamics, empirical, 'Whether the opposition backlash is temporary or sustained.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nl_gay_prime_minister_norm, 2014, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nl_g_tr_t2014, nl_gay_prime_minister_norm, theater_ratio, 2014, 0.1).
narrative_ontology:measurement(nl_g_tr_t2019, nl_gay_prime_minister_norm, theater_ratio, 2019, 0.15).
narrative_ontology:measurement(nl_g_tr_t2024, nl_gay_prime_minister_norm, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(nl_g_be_t2014, nl_gay_prime_minister_norm, base_extractiveness, 2014, 0.55).
narrative_ontology:measurement(nl_g_be_t2019, nl_gay_prime_minister_norm, base_extractiveness, 2019, 0.5).
narrative_ontology:measurement(nl_g_be_t2024, nl_gay_prime_minister_norm, base_extractiveness, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nl_gay_prime_minister_norm, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
