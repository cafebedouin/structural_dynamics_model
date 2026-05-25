% ============================================================================
% CONSTRAINT STORY: the_calm_protocol_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_the_calm_protocol_suppression, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: the_calm_protocol_suppression
 *   human_readable: The "Calm" of Antarctic Protocol
 *   domain: social/psychological
 *
 * SUMMARY:
 *   The 'Calm' of Antarctic Protocol describes the institutional suppression
 *   of emotional expression, spontaneous communication, and individual
 *   psychological need-states within isolated research stations. Residents
 *   are required to maintain composure and suppress emotional display —
 *   grief, fear, anger, or joy are all treated as operational failures. The
 *   protocol originates in Cold War military doctrine (maintaining
 *   operational security and unit cohesion in isolated, dangerous conditions)
 *   but persists in civilian scientific contexts where the original rationale
 *   has largely atrophied. The psychological weight described by Soh and
 *   Mbatha is the cumulative cost of this enforced suppression: inability to
 *   process emotional experience in real-time, redirected emotional energy
 *   into institutional compliance performance, and psychological burden
 *   concentrated on individuals with lower suppression capacity. The
 *   constraint exhibits the full range of indexical classification: it
 *   appears as pure coordination (protocol authority's perspective),
 *   temporary institutional failure (psychological support advocates),
 *   degraded ritual (institutional history's perspective), mixed
 *   coordination-extraction (research teams), pure extraction (individuals
 *   bearing suppression cost), and apparent natural law (abstract framing
 *   that risks naturalizing contingent institutional choice). The theater
 *   ratio has risen from 0.45 to 0.65 over the interval, indicating
 *   increasing performative content — the suppression ritual is increasingly
 *   maintained for institutional legitimacy rather than actual safety
 *   requirement, as alternative psychological support mechanisms emerge.
 *
 * KEY AGENTS:
 *   - Individual Residents: Primary victims (powerless/trapped) — bear full psychological extraction cost of suppression requirement; no exit mechanism except emergency evacuation or protocol violation
 *   - Research Team Collective: Secondary victims (moderate/constrained) — benefit from panic prevention and operational cohesion but face asymmetric suppression burden; more emotionally expressive members bear disproportionate cost
 *   - Protocol Authority: Primary beneficiary (institutional/arbitrage) — maintains operational control and institutional legitimacy through 'calm' enforcement; can abstract away from individual suppression cost through institutional role distance
 *   - Psychological Support Advocates: Organized agents (organized/constrained) — build alternative pathways (peer support, teletherapy, improved screening) that enable emotional processing without protocol violation; see sunset clause
 *   - Cold War Military Doctrine Legacy: Institutional actor (institutional/arbitrage) — original rationale for suppression protocol; persists through organizational tradition despite reduced relevance in civilian contexts
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangement as inherent feature of isolated team operations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(the_calm_protocol_suppression, 0.58).
domain_priors:suppression_score(the_calm_protocol_suppression, 0.72).
domain_priors:theater_ratio(the_calm_protocol_suppression, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(the_calm_protocol_suppression, extractiveness, 0.58).
narrative_ontology:constraint_metric(the_calm_protocol_suppression, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(the_calm_protocol_suppression, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(the_calm_protocol_suppression, snare).
narrative_ontology:human_readable(the_calm_protocol_suppression, "The \"Calm\" of Antarctic Protocol").
narrative_ontology:topic_domain(the_calm_protocol_suppression, "social/psychological").

domain_priors:requires_active_enforcement(the_calm_protocol_suppression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(the_calm_protocol_suppression, institutional_authority).
narrative_ontology:constraint_beneficiary(the_calm_protocol_suppression, protocol_maintainers).
narrative_ontology:constraint_victim(the_calm_protocol_suppression, individual_personnel).
narrative_ontology:constraint_victim(the_calm_protocol_suppression, psychological_autonomy).
narrative_ontology:constraint_victim(the_calm_protocol_suppression, emergent_communication).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL RESIDENT (SNARE) — Trapped within the station perimeter and operational mandate. Faces psychological extraction: required to suppress emotional expression, spontaneous communication, and individual need-states. The 'calm' is enforced through institutional inertia and isolation. No exit mechanism short of emergency or protocol violation. Maximum experienced suppression and extraction.
constraint_indexing:constraint_classification(the_calm_protocol_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: RESEARCH TEAM (TANGLED ROPE) — Constrained by safety protocols and station interdependence. The 'calm' serves a genuine coordination function: suppressing panic prevents cascading failures in isolated, dangerous conditions. But the mechanism extracts individual psychological autonomy and creates asymmetric burden distribution. Team members with lower tolerance for suppression bear disproportionate cost. Experiences both coordination benefit and extraction cost.
constraint_indexing:constraint_classification(the_calm_protocol_suppression, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PROTOCOL AUTHORITY (ROPE) — Institutional beneficiary. The 'calm' protocol enables coordination of complex, dangerous operations across multiple nations and disciplines. Maintains operational efficiency and institutional legitimacy. Experiences the constraint as pure coordination: preventing panic is essential for station safety. Authority can arbitrage away from individual suppression cost through institutional abstraction.
constraint_indexing:constraint_classification(the_calm_protocol_suppression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PSYCHOLOGICAL SUPPORT MOVEMENT (SCAFFOLD) — Organized advocates and emerging psychological support protocols. See the 'calm' suppression as a temporary institutional failure with a sunset clause: improved mental health screening, peer support training, and teletherapy create alternative pathways for psychological stability that don't require emotion suppression. Exit path visible but requires generational norm shift.
constraint_indexing:constraint_classification(the_calm_protocol_suppression, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: COLD WAR INSTITUTIONAL INERTIA (PITON) — The 'calm' protocol originates in Cold War-era operational security and military psychology doctrine. The underlying rationale (suppressing emotional expression prevents security breaches and maintains combat readiness) has largely atrophied in civilian scientific contexts, yet the protocol persists through institutional inertia. Theater ratio high because the suppression ritual is maintained for legitimacy, not because it reliably produces the safety outcomes it claims.
constraint_indexing:constraint_classification(the_calm_protocol_suppression, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN RISK) — From an abstract framing, some suppression of affective expression might appear inherent to high-stakes, isolated team operations: emotional contagion in confined spaces creates objective danger; therefore suppression is a natural law. This perspective risks naturalizing what is actually a contingent institutional choice. The false summit test: alternative protocols (transparent emotional expression with bounded reaction frameworks) are succeeding in analogous contexts (submarine crews, long-duration space missions), revealing the 'natural law' framing as institutional naturalization.
constraint_indexing:constraint_classification(the_calm_protocol_suppression, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(the_calm_protocol_suppression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(the_calm_protocol_suppression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(the_calm_protocol_suppression, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(the_calm_protocol_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(the_calm_protocol_suppression, TR),
    TR >= 0.70.

:- end_tests(the_calm_protocol_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The protocol extracts psychological autonomy and emotional expression from residents. However, the extraction is not total suppression — residents can and do experience emotions, and some emotional communication does occur (though constrained). The value reflects that while the requirement is enforced and the cost is real, alternative channels and gradual norm relaxation allow partial emotional expression. Suppression (0.72): High. Significant barriers to emotional expression include: explicit protocol enforcement, career/reputation consequences for emotional display, institutional culture of emotional containment, and geographic isolation preventing external emotional support. These barriers are substantial and actively maintained. Theater ratio (0.65): Moderate-high. The 'calm' ritual has increasing performative content. The suppression is presented as safety requirement, but the actual safety necessity is declining as alternative psychological support mechanisms emerge. Rising from 0.45 to 0.65 indicates that the ratio of performative maintenance to actual functional requirement is increasing.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival variation. Protocol authority sees coordination (Rope) — suppression prevents panic cascades in dangerous, isolated conditions. Residents experience extraction (Snare) — suppression requirement with no exit and no benefit. Research teams experience mixed effects (Tangled Rope) — suppression both enables team coordination and extracts individual autonomy. Psychological support advocates see a temporary institutional failure (Scaffold) — emerging alternatives suggest a sunset clause. Cold War institutional legacy sees a degraded ritual (Piton) — the original military rationale has atrophied but suppression persists through inertia. The analytical observer risks seeing natural law (Mountain) — emotional expression might appear incompatible with high-stakes isolated operations — but the empirical success of transparent emotion protocols in analogous contexts (submarine crews, long-duration space missions) reveals this as institutional naturalization rather than inherent necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position and power asymmetry. Residents (powerless/trapped) experience d → 0.95, producing high f(d) ≈ 1.42, amplifying experienced extractiveness. Protocol authority (institutional/arbitrage) experiences d → 0.05, producing negative f(d) ≈ -0.12, experiencing suppression as cost-free coordination. Research teams (moderate/constrained) experience d → 0.65, producing f(d) ≈ 1.00, experiencing both sides of the extraction-coordination balance. Psychological advocates (organized/constrained) experience d → 0.55, producing f(d) ≈ 0.75, because they have exit paths (alternative protocols) and institutional voice. The piton classification derives from rising theater ratio (now 0.65) indicating increasing performative content relative to actual safety function. The mountain perspective's false summit classification reveals that the constraint's apparent naturalness is contingent on institutional choice, not inherent to team operations.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by revealing that the 'Calm' protocol conflates two distinct structural claims: (1) Panic prevention is essential in isolated, dangerous operations (genuine coordination requirement), and (2) Suppression of emotional expression is necessary to prevent panic (contingent institutional mechanism). Claim 1 is true and justifies some suppression (Tangled Rope/legitimate mixed coordination-extraction). Claim 2 is increasingly false — alternative protocols (transparent expression with bounded reaction frameworks) prevent panic equivalently. The protocol persists because institutional authority benefits from suppression (easier to manage compliant residents than to process emotional expression), not because suppression is necessary. This is textbook mandatrophy resolution: the justification (safety/coordination) remains valid, but the mechanism (emotional suppression) has become extractive relative to alternatives. The protocol should be reclassified as the psychology support movement succeeds — from Snare (under suppression-only framing) toward Scaffold (under alternative protocols framing) with eventual sunset to Rope (once emotional expression becomes standard coordination mechanism).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_efficacy_threshold,
    'At what suppression level does the coordination benefit (panic prevention) reverse into psychological damage that undermines actual safety?',
    'Longitudinal psychological assessment data; correlation between suppression intensity and subsequent mental health outcomes; comparison with alternative protocols (e.g., transparent emotion expression with bounded response frameworks)',
    'If threshold < 0.60: current protocol operates beyond optimal suppression; should reduce enforcement. If threshold > 0.80: suppression validates as necessary coordination. If no threshold exists: suppression is pure extraction masked as safety.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_efficacy_threshold, empirical, 'The suppression level at which safety benefit reverses into harm').

omega_variable(
    alternative_coordination_equivalence,
    'Can transparent emotional expression (with structured response protocols) achieve equivalent safety outcomes to suppression-based calm?',
    'Controlled comparison between protocol variants: suppression-strict stations vs. transparency-with-structure stations; incident rates, team cohesion measures, and psychological outcome metrics',
    'If true equivalence: suppression is extractive theater (Snare/Piton confirmed). If suppression outperforms: coordination benefit is real (Tangled Rope confirmed). If mixed outcomes: depends on personnel type and environmental stress level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_coordination_equivalence, empirical, 'Whether alternative emotional protocols achieve equivalent safety').

omega_variable(
    institutional_inertia_persistence,
    'What sustains the ''calm'' protocol despite evidence of alternatives? Institutional legitimacy, career incentives for authority, risk aversion, or actual safety requirement?',
    'Institutional analysis of protocol adoption history; interviews with decision-makers; tracking of protocol changes correlating with new leadership or external pressure',
    'If legitimacy/inertia dominant: Piton classification confirmed. If actual safety requires it: Tangled Rope/Mountain confirmed. If risk aversion on authority: extractive institutional rationale (Snare confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_inertia_persistence, conceptual, 'What institutional factors sustain the suppression protocol').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(the_calm_protocol_suppression, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(calm_tr_t0, the_calm_protocol_suppression, theater_ratio, 0, 0.45).
narrative_ontology:measurement(calm_tr_t25, the_calm_protocol_suppression, theater_ratio, 25, 0.62).
narrative_ontology:measurement(calm_tr_t50, the_calm_protocol_suppression, theater_ratio, 50, 0.65).

% Extraction over time
narrative_ontology:measurement(calm_be_t0, the_calm_protocol_suppression, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(calm_be_t25, the_calm_protocol_suppression, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(calm_be_t50, the_calm_protocol_suppression, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(the_calm_protocol_suppression, enforcement_mechanism).
narrative_ontology:affects_constraint(the_calm_protocol_suppression, isolated_team_psychological_cohesion).
narrative_ontology:affects_constraint(the_calm_protocol_suppression, institutional_legitimacy_through_compliance).

% DUAL FORMULATION NOTE:
% The 'Calm' protocol decomposes into two distinct constraints: (1) panic_prevention_in_isolation (ε ≈ 0.15, Mountain/Rope — genuine coordination requirement), and (2) emotional_suppression_enforcement (ε ≈ 0.58, Snare — institutional extraction mechanism). The current story models their conflation. As alternative protocols emerge, the decomposition becomes visible: panic prevention succeeds without emotional suppression, revealing suppression as extractive overlay. The upstream constraint (panic_prevention_necessity) affects the downstream constraint (suppression_as_mechanism); as alternatives to suppression prove equivalent for panic prevention, suppression loses its coordinating function and becomes pure extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(the_calm_protocol_suppression, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
