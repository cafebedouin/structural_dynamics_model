% ============================================================================
% CONSTRAINT STORY: boundary_dissolution_risk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_boundary_dissolution_risk, []).

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
 *   constraint_id: boundary_dissolution_risk
 *   human_readable: The Infinite Porosity Trap
 *   domain: technological/labor
 *
 * SUMMARY:
 *   The 'Infinite Porosity Trap' describes the erosion of boundaries between
 *   professional and private life, driven by pervasive digital connectivity
 *   and workplace monitoring. While enabling flexible and remote work, this
 *   dissolution creates a structure that extracts uncompensated time,
 *   attention, and data from workers, while suppressing their ability to
 *   disconnect. The constraint is not a simple top-down imposition but a
 *   complex system with genuine coordination functions intertwined with
 *   extractive mechanisms.
 *
 * KEY AGENTS:
 *   - Digital Workers: Primary victims (powerless/trapped) — bear the costs of eroded boundaries, including burnout and loss of autonomy.
 *   - Employers: Primary beneficiaries (institutional/arbitrage) — gain perceived productivity, operational control, and flexibility from a distributed workforce.
 *   - Monitoring Technology Providers: Secondary beneficiaries (institutional/arbitrage) — profit from the sale of surveillance and productivity management tools.
 *   - Legacy HR Departments: Institutional actors (institutional/constrained) — implement monitoring policies, often resulting in performative compliance rather than functional productivity gains.
 *   - Gig Economy Freelancers: Secondary victims (moderate/mobile) — experience an extreme version of the trap, trading precarity for flexibility under constant platform surveillance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(boundary_dissolution_risk, 0.55).
domain_priors:suppression_score(boundary_dissolution_risk, 0.65).
domain_priors:theater_ratio(boundary_dissolution_risk, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(boundary_dissolution_risk, extractiveness, 0.55).
narrative_ontology:constraint_metric(boundary_dissolution_risk, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(boundary_dissolution_risk, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(boundary_dissolution_risk, tangled_rope).
narrative_ontology:human_readable(boundary_dissolution_risk, "The Infinite Porosity Trap").
narrative_ontology:topic_domain(boundary_dissolution_risk, "technological/labor").

domain_priors:requires_active_enforcement(boundary_dissolution_risk).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(boundary_dissolution_risk, employers).
narrative_ontology:constraint_beneficiary(boundary_dissolution_risk, monitoring_technology_providers).
narrative_ontology:constraint_victim(boundary_dissolution_risk, digital_workers).
narrative_ontology:constraint_victim(boundary_dissolution_risk, gig_economy_freelancers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE DIGITAL WORKER (SNARE) — Individually unable to opt-out of 'always-on' culture and monitoring without risking employment. Experiences the constraint as pure extraction of personal time and autonomy. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.78. This high effective extraction, combined with high suppression, meets the Snare classification gates.
constraint_indexing:constraint_classification(boundary_dissolution_risk, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE EMPLOYER (ROPE) — Experiences the constraint as a pure coordination mechanism for managing a distributed workforce and maximizing productivity. As a primary beneficiary with arbitrage options (switching tech, accessing global labor), the effective extraction is negative. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.08.
constraint_indexing:constraint_classification(boundary_dissolution_risk, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE GIG WORKER (TANGLED ROPE) — Experiences both the coordination benefits (flexibility) and the extractive costs (platform surveillance, precariousness). Has mobility to switch platforms, but all platforms impose similar constraints. d≈0.85, f(d)≈1.15, σ=1.2 → χ≈0.76. This falls within the Tangled Rope range, reflecting the hybrid nature of their experience.
constraint_indexing:constraint_classification(boundary_dissolution_risk, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: LEGACY HR (PITON) — Tasked with implementing monitoring policies. The original function (productivity assurance) has degraded into a theatrical performance of measuring 'online presence' (keystrokes, mouse movement). The high theater_ratio (0.75) satisfies the Piton gate (≥0.70), reflecting a system maintained by institutional inertia despite its declining functional relevance.
constraint_indexing:constraint_classification(boundary_dissolution_risk, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees the full structure: a genuine coordination function (enabling remote work) is inextricably linked to an asymmetric extraction mechanism (eroding worker autonomy and personal time). The classification reflects the hybrid nature of the constraint. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(boundary_dissolution_risk, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(boundary_dissolution_risk_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(boundary_dissolution_risk, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(boundary_dissolution_risk, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(boundary_dissolution_risk, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(boundary_dissolution_risk, TR),
    TR >= 0.70.

:- end_tests(boundary_dissolution_risk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): High. Represents the value of uncompensated 'on-call' time, mental availability outside of work hours, and the productivity data extracted from workers. Suppression (0.65): High. Opting out of the 'always-on' culture or refusing monitoring is often career-limiting or impossible, effectively removing alternatives. Theater Ratio (0.75): Very High. Monitoring often incentivizes performative 'busyness' (e.g., mouse jiggling) over actual productive work, meaning a large portion of the activity is for show, satisfying the Piton classification for the HR perspective.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. Employers view the system as a Rope, a tool for coordinating work. For the individual worker, who cannot escape the expectation of constant availability, it is a Snare, extracting their personal life. HR departments, caught in the middle, see a Piton—a degraded ritual of productivity theater. The analytical view recognizes both the coordination and extraction elements, classifying it as a Tangled Rope. This diversity of classification from a single set of metrics is a hallmark of a complex, socially embedded constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position. Employers and tech providers are declared beneficiaries with arbitrage exit, yielding a low 'd' value and a negative effective extraction (χ), hence the Rope classification. Workers are victims with trapped or mobile exit options, yielding a high 'd' and a high positive χ, leading to Snare or Tangled Rope classifications. This demonstrates how the χ formula correctly maps structural relationships to perspectival classifications.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by correctly identifying the dual nature of the system. A naive analysis might label it a pure Snare (focusing only on the worker's experience) or a pure Rope (focusing only on the employer's claims). The Deferential Realism framework, by using indexical classification, shows that both are valid perspectives. The analytical classification of Tangled Rope correctly captures the core truth: a genuine coordination function has been coupled with a powerful, asymmetric extraction mechanism. It is the entanglement of the two that makes the constraint so stable and difficult to reform.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    productivity_causality,
    'Does pervasive monitoring causally increase genuine productivity, or does it primarily incentivize performative work (theatrical ''busyness'')?',
    'Controlled studies comparing output quality and innovation rates between monitored and non-monitored teams in creative/knowledge work domains.',
    'If monitoring boosts only performative work, the constraint is closer to a pure Snare. If it genuinely boosts productivity, the Rope/Tangled Rope classifications are more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(productivity_causality, empirical, 'Causal link between monitoring and genuine vs. performative productivity.').

omega_variable(
    autonomy_vs_flexibility_tradeoff,
    'Is the loss of boundary-defined autonomy an acceptable and fair trade for the gain in location and time flexibility?',
    'Revealed preference surveys and labor market analysis on wage premiums/discounts for jobs with different mixes of flexibility and surveillance.',
    'Defines whether the extraction is perceived as a cost or a price. If workers value flexibility more, the constraint functions more like a Rope. If they value autonomy more, it functions as a Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(autonomy_vs_flexibility_tradeoff, preference, 'Worker valuation of the tradeoff between flexibility and autonomy.').

omega_variable(
    normalization_threshold,
    'At what point does this technologically-mediated practice become an irreversible social norm, effectively a cultural ''Mountain''?',
    'Longitudinal analysis of ''right to disconnect'' legislation success/failure and shifts in management theory and labor contracts over a generational timescale.',
    'If the practice becomes fully normalized, exit options collapse and suppression approaches 1.0, potentially transforming the constraint into a Mountain from most non-analytical perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normalization_threshold, conceptual, 'Threshold at which boundary dissolution becomes a fixed social norm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(boundary_dissolution_risk, 2008, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(boun_tr_t2008, boundary_dissolution_risk, theater_ratio, 2008, 0.25).
narrative_ontology:measurement(boun_tr_t2015, boundary_dissolution_risk, theater_ratio, 2015, 0.5).
narrative_ontology:measurement(boun_tr_t2023, boundary_dissolution_risk, theater_ratio, 2023, 0.75).

% Extraction over time
narrative_ontology:measurement(boun_be_t2008, boundary_dissolution_risk, base_extractiveness, 2008, 0.2).
narrative_ontology:measurement(boun_be_t2015, boundary_dissolution_risk, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement(boun_be_t2023, boundary_dissolution_risk, base_extractiveness, 2023, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(boundary_dissolution_risk, resource_allocation).
narrative_ontology:affects_constraint(boundary_dissolution_risk, mental_health_outcomes_knowledge_work).
narrative_ontology:affects_constraint(boundary_dissolution_risk, future_of_labor_organizing).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
