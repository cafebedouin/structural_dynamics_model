% ============================================================================
% CONSTRAINT STORY: organizational_capability_lock
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_organizational_capability_lock, []).

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
 *   constraint_id: organizational_capability_lock
 *   human_readable: Organizational Capability Lock
 *   domain: organizational_dynamics/institutional_inertia
 *
 * SUMMARY:
 *   Organizational capability lock occurs when an organization's power
 *   structure concentrates knowledge and skill gatekeeping in incumbent
 *   leadership, preventing emerging agents from acquiring capabilities needed
 *   for advancement or organizational adaptation. This creates a hybrid
 *   constraint: genuine coordination function (stable role definitions,
 *   predictable expertise distribution) coexists with asymmetric extraction
 *   (blocked advancement, training resource denial, knowledge monopoly). The
 *   constraint is maintained through both structural barriers (formal
 *   gatekeeping mechanisms, certification requirements, access control) and
 *   internalized mechanisms (identity fusion with narrow roles, learned
 *   helplessness, internalized status hierarchies). The theater component is
 *   high: formal rank structures and promotion rituals persist despite
 *   reduced verification necessity, performing organizational legitimacy
 *   while functioning primarily as barrier maintenance. The extractiveness
 *   trajectory shows increasing severity: as organizational complexity grows,
 *   the lock becomes more extractive because emerging agents face steeper
 *   capability requirements while gatekeeping tightens. The constraint
 *   exhibits all six DR types across different organizational positions,
 *   making it a diagnostic exemplar for how institutional power consolidates
 *   through capability asymmetry.
 *
 * KEY AGENTS:
 *   - Incumbent Leadership Coalition: Primary beneficiary (institutional/arbitrage) — maintains power through information and skill monopoly; experiences constraint as coordination
 *   - Emerging Capability Seekers: Primary victim (powerless/trapped) — blocked from advancement, denied training, locked into obsolete roles; bears full extraction cost
 *   - Specialized Skill Gatekeepers: Secondary beneficiary (moderate/constrained) — protected from competition through knowledge asymmetry; benefit from asymmetry while also constrained by gatekeeping logic
 *   - Mid-Level Functional Managers: Secondary victim (moderate/constrained) — experience mixed coordination and extraction; dependent on system that constrains broader capability development
 *   - Organizational Transformation Initiative: Organized agents (organized/constrained) — implementing knowledge transfer and capability democratization programs; see lock as remediable through institutional redesign
 *   - Hierarchical Status Ritual: Institutional mechanism (institutional/arbitrage) — promotion gates, certification processes, rank structures performing legitimacy; maintaining gatekeeping through ritual
 *   - Organizational Adaptive Capacity: Abstract collective victim (powerless/trapped) — organizational ability to evolve and respond to external change is constrained by capability lock
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(organizational_capability_lock, 0.58).
domain_priors:suppression_score(organizational_capability_lock, 0.62).
domain_priors:theater_ratio(organizational_capability_lock, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(organizational_capability_lock, extractiveness, 0.58).
narrative_ontology:constraint_metric(organizational_capability_lock, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(organizational_capability_lock, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(organizational_capability_lock, tangled_rope).
narrative_ontology:human_readable(organizational_capability_lock, "Organizational Capability Lock").
narrative_ontology:topic_domain(organizational_capability_lock, "organizational_dynamics/institutional_inertia").

domain_priors:requires_active_enforcement(organizational_capability_lock).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(organizational_capability_lock, incumbent_leadership).
narrative_ontology:constraint_beneficiary(organizational_capability_lock, specialized_skill_gatekeepers).
narrative_ontology:constraint_victim(organizational_capability_lock, emerging_capability_seekers).
narrative_ontology:constraint_victim(organizational_capability_lock, organizational_adaptive_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING CAPABILITY SEEKER (SNARE) — Trapped within organizational hierarchy. Cannot acquire new skills without permission from gatekeepers; cannot exit organization without sacrificing tenure, pension, and accumulated social capital. Bears full extraction cost: blocked from advancement, denied training resources, locked into obsolete role. Maximum experienced extraction.
constraint_indexing:constraint_classification(organizational_capability_lock, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MID-LEVEL FUNCTIONAL MANAGER (TANGLED ROPE) — Constrained by both institutional dependency (career path, benefits, organizational identity) and gatekeeping logic (protecting domain expertise creates asymmetric extraction). Also genuinely benefits from coordination function: stable functional domains reduce uncertainty, specialized knowledge creates value. Experiences mixed coordination and extraction — some agency, some genuine benefit, but also extraction through knowledge asymmetry.
constraint_indexing:constraint_classification(organizational_capability_lock, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT LEADERSHIP COALITION (ROPE) — Net beneficiary. Maintains power through information asymmetry and skill gatekeeping. Experiences constraint as coordination mechanism: monopoly on critical knowledge enables command-and-control structure. Can exit or redistribute power but chooses not to — arbitrage remains available. Perceived constraint is minimal; extraction flows toward these agents.
constraint_indexing:constraint_classification(organizational_capability_lock, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ORGANIZATIONAL TRANSFORMATION INITIATIVE (SCAFFOLD) — Organized agents (HR departments, learning & development, new leadership cohorts) recognize the lock as temporary coordination failure with defined exit pathway. Structured training programs, competency frameworks, and knowledge-transfer initiatives create sunset logic: capability democratization reduces the lock's force. High suppression initially, but declining over implementation timeline. Sees extraction as remediable through institutional redesign.
constraint_indexing:constraint_classification(organizational_capability_lock, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: HIERARCHICAL STATUS RITUAL (PITON) — Formal rank structures and certification processes (degrees, certifications, tenure ladders) persist through inertia despite reduced functional necessity. These rituals perform organizational legitimacy but their verification function has atrophied — expertise can be assessed through performance, not credentials alone. Theater ratio is high: promotions gates are ritualized barrier maintenance rather than capability verification. Piton classification reflects vestigial function maintained by institutional momentum.
constraint_indexing:constraint_classification(organizational_capability_lock, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scope, some asymmetry between experienced and inexperienced agents appears inevitable: organizations require coordination, coordination requires stable role definitions, stable roles require differentiation of access to training and advancement pathways. This perspective risks naturalizing what is actually a contingent institutional choice — the perception that capability lock is 'how human hierarchies must work.' The engine's false summit detector will flag this perspective as naturalization of a remediable institutional arrangement.
constraint_indexing:constraint_classification(organizational_capability_lock, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(organizational_capability_lock_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(organizational_capability_lock, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(organizational_capability_lock, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(organizational_capability_lock, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(organizational_capability_lock, TR),
    TR >= 0.70.

:- end_tests(organizational_capability_lock_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The lock initially extracts at 0.38 (early gatekeeping is loosely enforced) and accelerates to 0.58 (gatekeeping tightens as organizational complexity increases and knowledge becomes more specialized). The trajectory reflects that as organizations grow, the incumbent leadership's ability and motivation to maintain gatekeeping increases. Suppression (0.62): Moderate-high. Significant barriers exist: formal organizational hierarchy, gatekeeping mechanisms, certification requirements, career path dependency (sunk tenure, pension obligations, social network embeddedness). However, suppression is not total — some agents do acquire new capabilities through external training or organizational transitions. Theater ratio (0.65): Moderate-high and rising. Formal promotion rituals, performance evaluation theaters, and certification processes perform organizational legitimacy and status differentiation but increasingly fail to verify actual capability. As expertise becomes more specialized and external, internal rank structures become less predictive of actual capacity. The theater ratio increase reflects growing mismatch between credential verification (what formal processes claim to do) and capability verification (what actually happens in practice).
 *
 * PERSPECTIVAL GAP:
 *   This constraint reveals fundamental disagreement about whether capability asymmetry is coordination or extraction. The incumbent leadership genuinely experiences the lock as coordination mechanism — stable functional domains, predictable expertise distribution, clear chains of command. The emerging capability seekers experience pure extraction — they bear costs of learning-opportunity denial while benefiting not at all from the coordination function (they experience only the barriers). The transformation initiative sees a remediable temporary problem with a sunset — capability democratization will reduce the lock's force. The analytical observer risks seeing the lock as inevitable human hierarchy (mountain) when it is actually a contingent institutional choice. The perspectival gap reveals that 'coordination' and 'extraction' are not objective properties but are indexed to agent positions: what coordinates for the powerful extracts from the powerless.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position relative to the capability flow. Incumbent leadership has d ≈ 0.05 (full beneficiary with arbitrage exit) — extraction runs toward them, so they experience negative effective extraction. Emerging capability seekers have d ≈ 0.95 (full target, trapped exit) — they bear the full cost, so f(d) ≈ 1.42, multiplying their experienced extraction. Mid-level managers have d ≈ 0.55 (intermediate — they benefit from some aspects of the structure while constrained by others) — producing f(d) ≈ 0.75, moderate experienced extraction. The transformation initiative has d ≈ 0.60 (targeted by gatekeeping but with organizing capacity and exit pathways) — producing f(d) ≈ 0.85, decreasing over the implementation timeline as their organizational power grows. Analytical observers have d ≈ 0.73 (canonical for analytical power) — they experience the structure from outside all positions.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The capability lock is a genuine Tangled Rope — it satisfies all three gates: (1) Beneficiaries exist (incumbent leadership, skill gatekeepers) and experience low extraction through arbitrage, confirming coordination function. (2) Victims exist (emerging capability seekers, organizational adaptive capacity) and bear high extraction through trapped/constrained exit, confirming asymmetric extraction. (3) Active enforcement is required — gatekeeping must be continuously maintained through policy, cultural reinforcement, and structural barriers; the lock does not self-sustain. The false summit (mountain) perspective from the analytical observer misses that the constraint is remediable through institutional design. The snare perspective from powerless agents reflects their experienced reality (pure extraction) without recognizing the genuine coordination function that also exists. The classification resolves the mandatrophy by accepting multiple legitimate truths indexed to agent position: the lock IS coordination for the powerful; the lock IS extraction for the powerless; the lock IS temporary and remediable for organized agents with resources to implement alternatives. No single type is 'correct' — the constraint's true structure is the presheaf of all six perspectives, with the Tangled Rope classification at the analytical context representing the synthetic view.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gatekeeping_necessity_threshold,
    'What proportion of specialized knowledge actually requires gatekeeping for organizational stability, versus what proportion is maintained as control mechanism?',
    'Comparative analysis: organizations with strong gatekeeping vs distributed knowledge models; measurement of coordination failure rates and innovation output across different knowledge-access regimes',
    'If gatekeeping necessity is high (>60%): the lock reflects genuine coordination need, moving classification toward Rope. If necessity is low (<30%): extraction is primary mechanism, confirming Snare and Tangled Rope classifications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_necessity_threshold, empirical, 'Necessity of gatekeeping for organizational coordination').

omega_variable(
    knowledge_transfer_impedance,
    'Is suppression of emerging capability seekers structural (impossible to acquire skills) or internalized (agents believe they cannot acquire skills)?',
    'Post-exit analysis: do individuals who leave capability-lock organizations rapidly acquire blocked skills? Measurement of skill acquisition rates pre-exit vs post-exit.',
    'If structural: suppression metric is accurate as measured. If internalized: effective suppression is higher than structural barriers suggest — agents carry the lock with them.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(knowledge_transfer_impedance, empirical, 'Whether suppression is structural or internalized').

omega_variable(
    identity_fusion_binding_mechanism,
    'To what extent is the organizational capability lock maintained by identity fusion (agents identifying with their narrow role) versus material dependency (career path, pension, benefits)?',
    'Analysis of exit behavior post-lock release: do agents rapidly acquire new capabilities and identities, or do they maintain role-locked identities despite barrier removal? Measurement of cognitive reopening after organizational redesign.',
    'If identity fusion is dominant: some perspectives should use identity_locked exit option, changing classification and revealing cognitive rather than structural binding. If material dependency dominates: trapped/constrained exit options are accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_binding_mechanism, empirical, 'Relative contribution of identity fusion versus material dependency').

omega_variable(
    adaptive_capacity_externality,
    'What is the magnitude of organizational adaptive capacity loss caused by the capability lock, and does it exceed the coordination stability gains?',
    'Comparative innovation rates, time-to-market for capability evolution, organizational responsiveness to market shifts: capability-locked organizations vs distributed-knowledge models in same industry',
    'If externality is large: classification should shift toward Snare (high hidden cost). If externality is small: Tangled Rope classification is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptive_capacity_externality, empirical, 'Magnitude of organizational adaptive capacity loss').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(organizational_capability_lock, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orgcap_tr_t0, organizational_capability_lock, theater_ratio, 0, 0.52).
narrative_ontology:measurement(orgcap_tr_t5, organizational_capability_lock, theater_ratio, 5, 0.59).
narrative_ontology:measurement(orgcap_tr_t10, organizational_capability_lock, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(orgcap_be_t0, organizational_capability_lock, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(orgcap_be_t5, organizational_capability_lock, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(orgcap_be_t10, organizational_capability_lock, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(organizational_capability_lock, resource_allocation).
narrative_ontology:affects_constraint(organizational_capability_lock, organizational_learning_cascade).
narrative_ontology:affects_constraint(organizational_capability_lock, institutional_knowledge_decay).
narrative_ontology:affects_constraint(organizational_capability_lock, succession_planning_failure).

% DUAL FORMULATION NOTE:
% Organizational capability lock decomposes into three structurally distinct constraints: (1) the gatekeeping mechanism itself (high extraction, enables coordination), (2) the institutional knowledge decay when gatekeepers retire without transfer (institutional failure), and (3) the succession planning failure when emerging agents cannot acquire capabilities needed to replace departing expertise (cascading extraction). These stories are linked: capability lock enables knowledge decay (gatekeepers don't transfer because they're protecting their position), and knowledge decay triggers succession failures. Each has different ε and different measurement trajectories. This story (organizational_capability_lock) focuses on the active gatekeeping mechanism; downstream stories address decay and succession as institutional failures enabled by the lock.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
