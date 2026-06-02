% ============================================================================
% CONSTRAINT STORY: organizational_hierarchy_filtering
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_organizational_hierarchy_filtering, []).

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
 *   constraint_id: organizational_hierarchy_filtering
 *   human_readable: Organizational Hierarchy Filtering: Information Control and Asymmetric Power
 *   domain: organizational_behavior/institutional_governance
 *
 * SUMMARY:
 *   Organizational hierarchy filtering is the structural mechanism by which
 *   information flows upward through layers of gatekeeping, each filtering
 *   layer controlling what reaches the next level. This constraint exhibits
 *   the core tension of all institutional hierarchies: it performs a genuine
 *   coordination function (preventing leadership from drowning in noise)
 *   while simultaneously extracting asymmetric power (those who filter
 *   control those who are filtered). The constraint is neither purely
 *   coordination nor purely extraction, but an inseparable hybrid. The
 *   theater ratio (0.68) reflects that organizational reporting structures
 *   maintain performative authority while actual information flows through
 *   informal channels, dashboards, and direct communication that bypass
 *   formal hierarchy. The extractiveness trajectory (0.35 → 0.58 over the
 *   interval) shows accumulating extraction as hierarchies age: initial
 *   filtering serves coordination, but over time, gatekeepers entrench
 *   filtering to protect their own authority and shield leadership from
 *   uncomfortable truths.
 *
 * KEY AGENTS:
 *   - Senior Management: Primary beneficiary (institutional/arbitrage) — enjoys information compression and insulation from operational chaos; mobility to change systems
 *   - Middle Management: Mixed beneficiary-victim (moderate/constrained) — benefits from control over information flow; constrained by accountability for emergent problems
 *   - Frontline Workers: Primary victim (powerless/trapped) — no voice in hierarchy; information about their conditions filtered through gatekeepers; trapped by economic dependency
 *   - Organizational Learning System: Victim (moderate/constrained) — the collective capacity to learn from ground truth is systematically degraded by filtering
 *   - Adaptive Responsiveness: Victim (moderate/constrained) — organizations become slow to respond to emerging problems because information is delayed by filtering layers
 *   - Analytical Observer: Neutral (analytical/analytical) — sees the inevitable trade-off between coordination necessity and extraction risk
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(organizational_hierarchy_filtering, 0.58).
domain_priors:suppression_score(organizational_hierarchy_filtering, 0.65).
domain_priors:theater_ratio(organizational_hierarchy_filtering, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(organizational_hierarchy_filtering, extractiveness, 0.58).
narrative_ontology:constraint_metric(organizational_hierarchy_filtering, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(organizational_hierarchy_filtering, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(organizational_hierarchy_filtering, tangled_rope).
narrative_ontology:human_readable(organizational_hierarchy_filtering, "Organizational Hierarchy Filtering: Information Control and Asymmetric Power").
narrative_ontology:topic_domain(organizational_hierarchy_filtering, "organizational_behavior/institutional_governance").

domain_priors:requires_active_enforcement(organizational_hierarchy_filtering).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(organizational_hierarchy_filtering, senior_management).
narrative_ontology:constraint_beneficiary(organizational_hierarchy_filtering, gatekeeping_middle_management).
narrative_ontology:constraint_victim(organizational_hierarchy_filtering, frontline_workers).
narrative_ontology:constraint_victim(organizational_hierarchy_filtering, organizational_learning_capacity).
narrative_ontology:constraint_victim(organizational_hierarchy_filtering, adaptive_responsiveness).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTLINE WORKER (SNARE) — Trapped by economic dependency and lack of alternative employment. Information filtered through layers of hierarchy prevents direct voice. Career advancement requires conformity to gatekeepers' interpretations. Cannot exit the constraint without abandoning livelihood. Experiences maximum extraction with minimal benefit.
constraint_indexing:constraint_classification(organizational_hierarchy_filtering, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: AMBITIOUS MIDDLE MANAGER (TANGLED ROPE) — Constrained by career path dependency and performance metrics tied to span of control. Benefits from filtering: controlling information flow expands their perceived indispensability and authority. Also bears costs: held accountable for problems they filtered upward that later emerge. Mixed coordination (they genuinely aggregate and synthesize information) with embedded extraction (they control what senior leadership sees).
constraint_indexing:constraint_classification(organizational_hierarchy_filtering, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EXECUTIVE LEADERSHIP (ROPE) — Experiences hierarchy filtering as coordination mechanism: delegation requires filtering to prevent information overload; compression of data into actionable summaries is a genuine coordination function. Mobility to exit (can restructure, flatten, or change information systems) makes this institutional actor perceive the constraint as solving a real problem with manageable side effects.
constraint_indexing:constraint_classification(organizational_hierarchy_filtering, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ORGANIZATIONAL LEARNING SYSTEM (TANGLED ROPE) — The organization's collective capacity to learn from frontline observations is both coordinated and constrained. Hierarchy enables structure for knowledge synthesis, but filtering creates systematic blindness. Organizations learn slowly because ground truth is distorted by each filtering layer. Genuine coordination function (structure) coupled with extraction (loss of data fidelity).
constraint_indexing:constraint_classification(organizational_hierarchy_filtering, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: COMMAND-AND-CONTROL PARADIGM (PITON) — Hierarchical filtering persists through institutional inertia despite emergence of flat organizations, networked teams, and real-time information systems that bypass traditional filtering. The theater of reporting structures and formal chains of command remains functionally degraded — information flows through Slack, email, and informal networks while the org chart maintains theatrical authority. High theater ratio reflects performative org charts.
constraint_indexing:constraint_classification(organizational_hierarchy_filtering, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — The hierarchy filtering constraint is genuinely hybrid. It performs the coordination function of information compression and decision clarity. It simultaneously extracts through asymmetric power: those who filter control those who are filtered. The extraction is not eliminable without losing the coordination function entirely. Organizations cannot scale without some filtering; they cannot be just without reducing filtering. This is the core structural tension that makes Tangled Rope the canonical classification.
constraint_indexing:constraint_classification(organizational_hierarchy_filtering, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(organizational_hierarchy_filtering_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(organizational_hierarchy_filtering, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(organizational_hierarchy_filtering, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(organizational_hierarchy_filtering, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(organizational_hierarchy_filtering, TR),
    TR >= 0.70.

:- end_tests(organizational_hierarchy_filtering_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The original research baseline treated hierarchy filtering as primarily extractive, but the analytical frame recognizes genuine coordination necessity. Real organizations above ~100 people cannot scale without information compression. However, extractiveness remains elevated (not pure rope at 0.35) because middle managers reliably filter information to protect their own authority and because leadership relies on filtered data that prevents them from seeing problems. The trajectory shows this dynamic: initial filtering (0.35) is justifiable compression; later filtering (0.58) is accumulating gatekeeping rent-seeking. Suppression (0.65): High. Frontline workers face structural barriers to voice (formal hierarchy, power differential, career risk) and internalized barriers (belief that their observations don't matter, that hierarchy is natural). Suppression includes both mechanisms. Theater ratio (0.68): Moderate-high. Org charts remain legally significant and symbolically important, but actual information flows through informal networks, email, and digital tools. The reporting structure is a theatrical performance of authority that persists because it legitimates hierarchy, even as information bypasses it.
 *
 * PERSPECTIVAL GAP:
 *   The executive leadership sees Rope (pure coordination) because they experience the constraint as solving the information overload problem. Frontline workers see Snare (pure extraction) because they experience the constraint as silencing and disempowering them. Middle managers see Tangled Rope (hybrid) because they genuinely perform information synthesis (coordination) while also protecting their own authority (extraction). The organizational learning system sees Tangled Rope (hybrid) because the hierarchy enables structure but degrades information fidelity. The traditional command-and-control paradigm is Piton (degraded ritual) because the formal reporting structure persists despite the rise of flat teams and real-time information systems. The analytical observer sees Tangled Rope as the stable classification because the constraint cannot be eliminated without losing coordination capacity at scale. This is not a perspectival disagreement about the same constraint, but a genuine structural difference in how different agents experience an asymmetric system.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from their structural position: whether they benefit or bear costs, and whether they can exit. Senior management has low d (they are beneficiaries with high mobility, so f(d) is negative or near-zero — they experience coordination benefit). Frontline workers have very high d (they are trapped victims, so f(d) is high — they experience maximum extraction). Middle managers have intermediate d around 0.55 (they benefit from control but are constrained by accountability, creating mixed directionality). The organizational learning system has d around 0.70 (it is a victim — ground truth is lost — with no exit option). The institutional power atoms modulate how much each agent experiences the constraint: powerless agents with trapped exit experience raw f(d); institutional agents with arbitrage options experience dampened or negative f(d). The perspectival gap reveals that the constraint's structural nature is identical for all observers, but experienced extractiveness differs by structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification resolves the mandatrophy by rejecting the false choice between 'hierarchy is just coordination' (Rope) and 'hierarchy is pure extraction' (Snare). The constraint is genuinely both. Information compression at organizational scale requires filtering — this is the coordination function. But filtering creates power asymmetry — those who filter control what gets communicated upward and how it is framed — this is the extraction. Neither function is eliminable without destroying the constraint's viability. An organization cannot scale to hundreds or thousands of people without some hierarchical filtering. It also cannot do so without creating gatekeeping positions where people accumulate power through controlling information. The Tangled Rope classification insists that both the coordination and extraction components are structural, not artifacts of poor management or lack of transparency. Better management and more transparency reduce extractiveness (the trajectory could have been flatter), but they do not eliminate the fundamental hybrid nature. This is what separates Tangled Rope from Rope (which lacks the victim/extraction component) and from Scaffold (which has a sunset clause — hierarchical filtering does not have a built-in sunset, only external threats to it).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    filtering_necessity_threshold,
    'What organizational scale makes information filtering structurally necessary versus merely convenient for gatekeepers?',
    'Comparative analysis across organization sizes: flat organizations (< 50 people) vs hierarchical (> 500 people). Measurement of decision latency and information fidelity at different scales.',
    'If filtering is necessary above 100 people: Tangled Rope holds at scale. If hierarchy is optional above 500 people: more extraction is attributable to gatekeeping than to coordination necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(filtering_necessity_threshold, empirical, 'Organizational scale at which filtering becomes structurally necessary').

omega_variable(
    gatekeeper_incentive_alignment,
    'Are middle managers filtering information to protect organizational learning (coordination) or to protect their own power (extraction)?',
    'Analysis of what information is filtered: data that reduces senior leadership''s comfort level (extraction signal) vs data that increases decision quality (coordination signal). Behavioral tracking across reorganizations or after transparency initiatives.',
    'If filtering prioritizes leader comfort: extraction component is dominant, pushing toward Snare. If filtering reflects genuine complexity management: coordination component is dominant, solidifying Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gatekeeper_incentive_alignment, empirical, 'Whether filtering serves learning or power protection').

omega_variable(
    alternative_information_architecture_feasibility,
    'Can flat organizations, real-time dashboards, or radical transparency actually replace hierarchical filtering without creating information overload at leadership level?',
    'Post-implementation studies of organizations that removed filtering layers (holocracy, flat startups, transparency initiatives). Measurement of decision quality, leadership stress, and organizational response time.',
    'If alternatives work: filtering is extractive convention, not coordination necessity. Constraint shifts toward Snare. If alternatives fail: filtering is genuine necessity, Tangled Rope confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_information_architecture_feasibility, empirical, 'Whether flat information architectures can replace hierarchical filtering').

omega_variable(
    suppression_measurement_ambiguity,
    'Is the measured suppression (0.65) structural (people genuinely cannot access information) or internalized (people believe information is not theirs to access)?',
    'Exit trajectory analysis: if suppression persists after formal information barriers are removed (e.g., town halls, open dashboards), it is internalized rather than structural.',
    'If internalized: constraint''s effective suppression is higher than measured — the target carries the suppression with them. If structural: suppression is accurately measured; removing barriers directly reduces it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_measurement_ambiguity, empirical, 'Whether suppression is structural or internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(organizational_hierarchy_filtering, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ohf_tr_t0, organizational_hierarchy_filtering, theater_ratio, 0, 0.5).
narrative_ontology:measurement(ohf_tr_t5, organizational_hierarchy_filtering, theater_ratio, 5, 0.62).
narrative_ontology:measurement(ohf_tr_t10, organizational_hierarchy_filtering, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(ohf_be_t0, organizational_hierarchy_filtering, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ohf_be_t5, organizational_hierarchy_filtering, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(ohf_be_t10, organizational_hierarchy_filtering, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(organizational_hierarchy_filtering, information_standard).
narrative_ontology:affects_constraint(organizational_hierarchy_filtering, gatekeeping_bias_accumulation).
narrative_ontology:affects_constraint(organizational_hierarchy_filtering, organizational_inertia_through_hierarchy).
narrative_ontology:affects_constraint(organizational_hierarchy_filtering, information_asymmetry_in_power_delegation).

% DUAL FORMULATION NOTE:
% Organizational hierarchy filtering is downstream of institutional governance structures but represents a distinct constraint. The upstream constraint is the coordination necessity at organizational scale; the filtering mechanism is the specific structural implementation. Separate constraint stories may analyze hierarchy's role in specific domains (e.g., healthcare hierarchy filtering in medical decision-making, military hierarchy filtering in tactical information) — each would have its own ε value reflecting domain-specific extractiveness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(organizational_hierarchy_filtering, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
