% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__technological_inevitability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__technological_inevitability, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: press_reformation_causality__technological_inevitability
 *   human_readable: Printing Press Technology as Inevitable Constraint on Scripture Propagation
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint instantiates the technological_inevitability reading of
 *   the press_reformation_causality kernel. The reading claims that printing
 *   press technology — specifically the physics of movable type, the cost
 *   structure of mechanical reproduction, and the mathematical reduction in
 *   per-copy costs — constitutes an immutable constraint on scripture
 *   propagation. Once the press existed as a technical possibility, the
 *   Reformation's spread of vernacular scripture became inevitable, not
 *   contingent on reformist strategy, beneficiary intent, or institutional
 *   decisions. The constraint operates as a natural law: the cost
 *   differential between scribal and mechanical reproduction makes wide
 *   propagation of texts mathematically cheaper and thus inevitable.
 *   Reformers are passive beneficiaries of technology they did not create;
 *   the Church is bound by a physical limit on their monopoly they cannot
 *   overcome through enforcement. This reading positions technology as the
 *   primary causal agent, with human choice and organization as secondary
 *   effects of technological determinism.
 *
 * KEY AGENTS:
 *   - Movable Type Physics: The immutable constraint itself (mountain) — the mechanical property that enables mass reproduction at lower cost than scribal copying
 *   - Gutenberg and Press Inventors: Technological agents (institutional/arbitrage) — discover the capability but are presented as non-agents in this reading; they simply uncover pre-existing physical laws
 *   - Scribal Economy: Primary victim (powerless/trapped) — bound by the immutable cost disadvantage; cannot compete once cheaper reproduction technology exists
 *   - Reformation Communities: Primary beneficiaries (moderate/mobile) — benefit from technology they did not control or deploy strategically; passive recipients of technological inevitability
 *   - Church Authority: Secondary victim (institutional/constrained) — experiences the press as an uncontrollable constraint on their information monopoly; suppression is mathematically impossible
 *   - Technological Determinist Analyst: Observational perspective (analytical/analytical) — sees causality flowing from technology → social outcomes, interpreting Reformation as engineering consequence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__technological_inevitability, 0.08).
domain_priors:suppression_score(press_reformation_causality__technological_inevitability, 0.02).
domain_priors:theater_ratio(press_reformation_causality__technological_inevitability, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_inevitability, extractiveness, 0.08).
narrative_ontology:constraint_metric(press_reformation_causality__technological_inevitability, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(press_reformation_causality__technological_inevitability, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_inevitability, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(press_reformation_causality__technological_inevitability, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__technological_inevitability, mountain).
narrative_ontology:human_readable(press_reformation_causality__technological_inevitability, "Printing Press Technology as Inevitable Constraint on Scripture Propagation").
narrative_ontology:topic_domain(press_reformation_causality__technological_inevitability, "history_of_technology/religious_history/media_studies").

domain_priors:emerges_naturally(press_reformation_causality__technological_inevitability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__technological_inevitability, '1cac216c-d22e-4752-b1bb-2b2ecdfb809e').
narrative_ontology:cs_kernel_codification('1cac216c-d22e-4752-b1bb-2b2ecdfb809e', formalized).
narrative_ontology:cs_authority_grounding('1cac216c-d22e-4752-b1bb-2b2ecdfb809e', lineage).
narrative_ontology:cs_interpretation_layer_present('1cac216c-d22e-4752-b1bb-2b2ecdfb809e').
narrative_ontology:cs_reading_relation('1cac216c-d22e-4752-b1bb-2b2ecdfb809e', press_reformation_causality__beneficiary_deployment, forecloses).
narrative_ontology:cs_reading_relation('1cac216c-d22e-4752-b1bb-2b2ecdfb809e', press_reformation_causality__precondition_convergence, coexists_with).
narrative_ontology:cs_axiom('1cac216c-d22e-4752-b1bb-2b2ecdfb809e', foundational, technology_determines_social_outcomes).
narrative_ontology:cs_axiom_status(technology_determines_social_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('1cac216c-d22e-4752-b1bb-2b2ecdfb809e', technology_determines_social_outcomes, instrumental).
narrative_ontology:cs_axiom('1cac216c-d22e-4752-b1bb-2b2ecdfb809e', foundational, reformist_agency_is_response_not_cause).
narrative_ontology:cs_axiom_status(reformist_agency_is_response_not_cause, holdable).
narrative_ontology:cs_axiom_grounding('1cac216c-d22e-4752-b1bb-2b2ecdfb809e', reformist_agency_is_response_not_cause, deontological).
narrative_ontology:cs_reference_frame('1cac216c-d22e-4752-b1bb-2b2ecdfb809e', press_as_immutable_technological_fact).
narrative_ontology:cs_drift_state('1cac216c-d22e-4752-b1bb-2b2ecdfb809e', contemporary_historical_discourse, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('1cac216c-d22e-4752-b1bb-2b2ecdfb809e', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__technological_inevitability, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_inevitability, technological_inevitability_interpretation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SCRIBE ECONOMY (MOUNTAIN) — Bound by the physics of manual reproduction. The press constraint is absolute from this perspective: once movable type physics was discovered, the cost differential became immutable. No amount of effort or organization could preserve the scribe monopoly once the press existed. The constraint is experienced as an unchangeable law of physics and economics.
constraint_indexing:constraint_classification(press_reformation_causality__technological_inevitability, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EARLY REFORMATION COMMUNITIES (MOUNTAIN) — The press constraint operates as an enabling natural law: the physics of movable type creates an irreversible cost differential that guarantees propagation of reformist scripture once the technical infrastructure exists. From this perspective, the constraint is immutable and works in their favor — the technology is the constraint, not the reformers' agency. The Reformation becomes inevitable once the press exists, independent of human intention or organization.
constraint_indexing:constraint_classification(press_reformation_causality__technological_inevitability, mountain,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 3: CHURCH INSTITUTIONAL AUTHORITY (MOUNTAIN) — Observes the press as an immutable constraint on their information monopoly. The technology's physics (movable type cost structure) makes control impossible at civilizational scale. The Church experiences this as a law of nature: they can suppress specific books or printers, but they cannot suppress the underlying cost advantage of the press. This perspective sees the constraint as genuinely natural — a physical limit on their power, not a contingent arrangement.
constraint_indexing:constraint_classification(press_reformation_causality__technological_inevitability, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TECHNOLOGICAL DETERMINIST ANALYST (MOUNTAIN) — Views the press as a pure technological constraint: the physics of reproduction and the economics of scale are irreducible features of material reality. Reformation propagation becomes a deterministic consequence of press physics, not contingent on reformist strategy, beneficiary choice, or institutional deployment decisions. The analyst sees causality running from technology → social outcomes. From this framework, the constraint is universal and unchangeable.
constraint_indexing:constraint_classification(press_reformation_causality__technological_inevitability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__technological_inevitability_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(press_reformation_causality__technological_inevitability, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(press_reformation_causality__technological_inevitability, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_inevitability, ExtMetricName, E),
    domain_priors:suppression_score(press_reformation_causality__technological_inevitability, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(press_reformation_causality__technological_inevitability),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_inevitability, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_inevitability, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(press_reformation_causality__technological_inevitability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Extremely low. Under technological_inevitability framing, there is no extraction — the constraint is a natural law of physics and economics, not a mechanism by which one party extracts from another. The cost differential is an objective feature of reproduction technology, not an asymmetric relationship created for benefit. Suppression (0.02): Negligible. The constraint cannot be suppressed because it is not a human arrangement susceptible to enforcement; it is a law of physics. The scribe economy cannot be defended through suppression because the technological fact exists independent of will. Theater ratio (0.15): Very low. The constraint operates with high functional clarity from the technological determinist perspective — the mechanism (movable type cost advantage) is transparent and requires no performative framing. The constraint 'works' as stated; there is minimal gap between claimed function (technology determines propagation) and actual operation (physics does determine cost structures). The low theater reflects that this reading presents itself as pure mechanism without need for narrative or legitimacy claim.
 *
 * PERSPECTIVAL GAP:
 *   The technological_inevitability reading produces a counterintuitive perspectival profile: all four perspectives classify as MOUNTAIN. This uniformity is the reading's diagnostic signature. The typical perspectival gap (powerless sees snare, institutional sees rope, analytical sees tangled_rope) is absent. Instead, all observers — regardless of power level or exit options — perceive the constraint as an immutable law of physics. The scribe, the reformer, the Church, and the analyst all agree: the press is a constraint that cannot be changed, only adapted to. This uniformity is suspicious. It suggests either a genuinely natural law (rare) or a false summit where technological framing naturalizes a contingent institutional arrangement. The kernel reading contestation is explicit: if the beneficiary_deployment or precondition_convergence readings are also defensible, then technological_inevitability is not the only coherent interpretation. The perspectival uniformity masks a deeper disagreement about whether technology determines causality or whether reformist agency, beneficiary strategy, and institutional preconditions are equally necessary.
 *
 * DIRECTIONALITY LOGIC:
 *   Under the technological_inevitability reading, directionality is not derived from beneficiary/victim relationships but from structural position relative to the technology. The scribe economy is trapped by physics (d → 1.0, high f(d)). Reformation communities benefit from technology they did not create (d → 0.0, negative f(d)). Church authority experiences a mathematical constraint they cannot overcome (d → 0.9, high f(d)). The analytical observer sees technology as the causal agent, making all human actors either passive beneficiaries or victims of physical law (d → 0.72, analytical canonical). The derivation chain here is not beneficiary/victim → d; rather, it is position relative to physical law → experienced constraint character → d value. This reading sidesteps human agency, making directionality a consequence of where the actor sits relative to a technological fact, not relative to institutional or strategic positioning.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_contingent_framework,
    'Is the press''s cost advantage over scribal reproduction a law of physics (mountain) or a contingent outcome of 15th-century resource scarcity and labor market conditions?',
    'Counterfactual analysis: Would the cost differential persist under alternative labor scarcity regimes (e.g., high-skill copyist abundance) or alternative metallurgy (if movable type required rare materials)? Examine whether the constraint is mathematically necessary or historically contingent.',
    'If mathematical/physical: mountain classification is correct; technology is determinative. If contingent: the constraint is tangled_rope or snare depending on who captured the beneficiary gains; reformist readings become agency-dependent rather than technology-determined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_contingent_framework, conceptual, 'Whether press cost advantage is immutable law or contingent historical condition').

omega_variable(
    technology_determinism_assumption,
    'Does this reading naturalistically assume that technological capabilities automatically translate into social outcomes, or does deployment require intentional beneficiary strategy?',
    'Historical comparison: identify printing technologies that existed but saw no social adoption (failed innovations, abandoned techniques); compare adoption timelines against reformist organizational readiness. If deployment requires strategic choice and resources, the constraint is not mountain but tangled_rope with technological affordance as one structural component.',
    'If technology determines: mountain stands; reformers are passive. If deployment requires choice: false_summit fires; the constraint is actually tangled_rope (technology coordinates with reformist strategy and beneficiary resource mobilization).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technology_determinism_assumption, empirical, 'Whether technological existence guarantees social propagation').

omega_variable(
    beneficiary_causality_reversal,
    'Did the press technology drive Reformation propagation, or did reformist organizational capacity and demand for vernacular scripture drive adoption and optimization of press technology?',
    'Timeline analysis: which came first, press development or reformist organizational readiness? Did printers develop presses to fill an existing market demand (reformists chose the technology) or did printers invent presses and reformists opportunistically deployed them? Examine pre-Reformation printing uses (secular texts, technical manuals, religious texts from traditional authorities) vs Reformation-period pivots.',
    'If technology-first: mountain; causality runs technology→reformation. If demand-first: false_summit fires; the beneficiary_deployment reading better explains the constraint; causality runs reformist demand→technology optimization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_causality_reversal, empirical, 'Causal direction: technology determines outcomes vs reformists drive technology adoption').

omega_variable(
    scribal_suppression_capacity,
    'Could the Church or secular authorities have suppressed press-based scripture propagation through enforcement (burning books, imprisoning printers, banning texts) with sufficient resource commitment, or was suppression mathematically impossible?',
    'Historical analysis: examine actual suppression efforts (Inquisitorial bans, printer persecution, Index Librorum Prohibitorum effectiveness); assess whether suppression failure was due to insufficient enforcement effort or mathematical impossibility of stopping distributed reproduction. Compare against other technologies (printing of scientific works, heretical texts, political pamphlets) to isolate the constraint on scripture specifically.',
    'If suppression was possible but insufficient: the constraint is tangled_rope (coordination of reproduction + limits on suppression) or snare (extraction of control by authorities who tried and failed). If suppression was mathematically impossible once press existed: mountain stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scribal_suppression_capacity, empirical, 'Whether suppression of press-based texts was technologically possible').

omega_variable(
    kernel_reading_contestation,
    'This constraint instantiates the technological_inevitability reading of the press_reformation_causality kernel. Are sibling readings (beneficiary_deployment, precondition_convergence) logically foreclosed by this reading''s premise, or do they coexist as competing interpretations?',
    'Logical analysis: If technological_inevitability is true (press physics determines outcomes), does that rule out beneficiary_deployment (reformists intentionally chose to use the press strategically) or precondition_convergence (press existed but required reformist organizational readiness)? Assess whether the readings are in logical contradiction or represent different causal levels that could both be true simultaneously.',
    'If technological_inevitability forecloses the others: they cannot both be maintained in the same analytical framework. If coexist: all three readings are live positions representing different aspects of a complex historical causality (technology + strategy + preconditions all matter).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Logical relationship of technological_inevitability reading to sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__technological_inevitability, 1450, 1517).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(press_theater_1450_gutenberg, press_reformation_causality__technological_inevitability, theater_ratio, 1450, 0.08).
narrative_ontology:measurement(press_theater_1480_diffusion, press_reformation_causality__technological_inevitability, theater_ratio, 1480, 0.12).
narrative_ontology:measurement(press_theater_1517_reformation, press_reformation_causality__technological_inevitability, theater_ratio, 1517, 0.15).

% Extraction over time
narrative_ontology:measurement(press_extract_1450_gutenberg, press_reformation_causality__technological_inevitability, base_extractiveness, 1450, 0.05).
narrative_ontology:measurement(press_extract_1480_diffusion, press_reformation_causality__technological_inevitability, base_extractiveness, 1480, 0.07).
narrative_ontology:measurement(press_extract_1517_reformation, press_reformation_causality__technological_inevitability, base_extractiveness, 1517, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__technological_inevitability, information_standard).
narrative_ontology:affects_constraint(press_reformation_causality__technological_inevitability, press_reformation_causality__beneficiary_deployment).
narrative_ontology:affects_constraint(press_reformation_causality__technological_inevitability, press_reformation_causality__precondition_convergence).
narrative_ontology:affects_constraint(press_reformation_causality__technological_inevitability, information_reproduction_cost_asymmetry).
narrative_ontology:affects_constraint(press_reformation_causality__technological_inevitability, scribal_labor_market_suppression).

% DUAL FORMULATION NOTE:
% The press_reformation_causality kernel decomposes into three reading-specific constraints: technological_inevitability (physics determines outcomes), beneficiary_deployment (reformist choice determines outcomes), and precondition_convergence (both technology and reformist readiness are necessary conditions). Each reading has a different ε (technology-determined = 0.08 mountain; strategy-dependent = 0.45+ snare/tangled_rope; convergence = 0.30 rope/tangled_rope). They are linked by kernel contestation, not by empirical decomposition. All three readings use the same observable (press adoption during Reformation), but they interpret causality differently. The engine's false_summit signature may fire if beneficiary declarations reveal that the 'natural law' framing conceals strategic choices.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
