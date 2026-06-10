% ============================================================================
% CONSTRAINT STORY: topology_selection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_topology_selection, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: topology_selection
 *   human_readable: Off-Grid Power System Topology Selection
 *   domain: electrical_engineering/power_systems/off_grid_infrastructure
 *
 * SUMMARY:
 *   Off-grid power system topology selection presents a structural choice
 *   between two architectures: Hammerhead (battery always in circuit,
 *   simpler, fewer components, lower cost, adequate for many deployments) and
 *   Smooth Operator (regulated bus with idle battery capability, more
 *   components, higher cost, better voltage stability and solar integration
 *   for complex loads). The constraint operates at the system integration
 *   layer: integrators specify topology based on deployment requirements, but
 *   requirement specification is often incomplete or ambiguous in
 *   remote/rural contexts. This ambiguity creates discretion that can be
 *   exercised legitimately (matching architecture to genuine need) or
 *   extractively (specifying higher-complexity topology to increase BOM value
 *   and ongoing service revenue when simpler architecture would suffice). The
 *   constraint exhibits tangled_rope structure: genuine coordination function
 *   (architectural choice solves real tradeoffs) coexists with asymmetric
 *   extraction (integrator discretion in ambiguous contexts enables
 *   over-specification that increases cost and maintenance burden for
 *   deployments with limited technical capacity and no redesign budget).
 *   Theater ratio (0.58) reflects that much of the 'engineering analysis'
 *   justifying Smooth Operator selection is performative when requirements
 *   are under-specified: load profile estimates are rough, solar resource
 *   data is sparse, maintenance access assumptions are optimistic, but the
 *   analysis produces a specification that appears rigorous. The theater has
 *   increased over the interval as off-grid deployment has scaled and
 *   integrator competition has intensified, creating pressure to
 *   differentiate through apparent sophistication rather than
 *   requirement-matched simplicity.
 *
 * KEY AGENTS:
 *   - Deployment Without Clear Requirements: Primary victim (powerless/trapped) — rural clinic, telecom site, or community facility locked into higher-complexity topology by integrator specification, bearing maintenance and replacement cost burden with no capacity to evaluate alternatives or budget to redesign
 *   - Field Technician: Secondary victim (moderate/constrained) — benefits from standardized training but bears cost of unnecessary complexity when simpler topology would suffice; can advocate for simplicity in some contexts but constrained by integrator specifications
 *   - System Integrator: Primary beneficiary (institutional/arbitrage) — captures margin on component selection and benefits from discretion to specify higher-component-count topologies when requirements are ambiguous; full exit options across topologies, vendors, and markets
 *   - Component Vendor: Secondary beneficiary (institutional/arbitrage) — sells components to integrators; higher-complexity topologies mean higher BOM value but no extraction from vendor perspective (they supply what is specified)
 *   - Open-Source Hardware Coalition: Organized agents (organized/mobile) — building open design documentation and decision frameworks to collapse information asymmetry; sees extraction as temporary coordination failure with sunset mechanism
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees genuine coordination function AND asymmetric extraction operating through the same decision point
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(topology_selection, 0.48).
domain_priors:suppression_score(topology_selection, 0.62).
domain_priors:theater_ratio(topology_selection, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(topology_selection, extractiveness, 0.48).
narrative_ontology:constraint_metric(topology_selection, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(topology_selection, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(topology_selection, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(topology_selection, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(topology_selection, tangled_rope).
narrative_ontology:human_readable(topology_selection, "Off-Grid Power System Topology Selection").
narrative_ontology:topic_domain(topology_selection, "electrical_engineering/power_systems/off_grid_infrastructure").

domain_priors:requires_active_enforcement(topology_selection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(topology_selection, integrator_discretion).
narrative_ontology:constraint_beneficiary(topology_selection, component_vendors).
narrative_ontology:constraint_victim(topology_selection, deployment_without_clear_requirements).
narrative_ontology:constraint_victim(topology_selection, field_technicians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(topology_selection, field_technicians).
narrative_ontology:constraint_vindicates(topology_selection, complexity_equals_robustness_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rural clinic or remote telecom site deployed with Smooth Operator topology based on integrator recommendation. Locked into higher maintenance burden and component replacement costs. No technical capacity to evaluate alternatives, no budget to redesign. Sunk cost and lack of local expertise trap the deployment in the specified topology.
narrative_ontology:constraint_stakeholder(topology_selection, deployment_without_clear_requirements, payer,
    powerless, biographical, trapped, local).

% Benefits from standardized topology training and diagnostic procedures, but bears cost of unnecessary complexity when simpler Hammerhead would suffice. Can advocate for simpler designs in some contexts but constrained by integrator specifications and vendor relationships. Mixed experience: coordination function exists (standardization enables training) but extraction is real (over-specification increases service calls and parts inventory burden).
narrative_ontology:constraint_stakeholder(topology_selection, field_technicians, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(topology_selection, field_technicians, beneficiary).

% Specifies system topology based on deployment requirements, but requirement specification is often incomplete or ambiguous in remote/rural contexts. This ambiguity creates discretion that can be exercised legitimately (matching architecture to genuine need) or extractively (specifying higher-complexity topology to increase BOM value and ongoing service revenue when simpler architecture would suffice). Captures margin on component selection. Full exit options — can shift between topologies, vendors, and markets freely.
narrative_ontology:constraint_stakeholder(topology_selection, integrator_discretion, agenda_setter,
    institutional, immediate, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(topology_selection, integrator_discretion, beneficiary).

% Sells charge controllers, DC-DC converters, battery management systems to integrators. More complex topologies mean higher BOM value per deployment. No extraction from vendor perspective — they supply what integrators specify, and simpler topologies would simply mean different (lower-value) BOMs. Pure coordination function with full market mobility.
narrative_ontology:constraint_stakeholder(topology_selection, component_vendors, beneficiary,
    institutional, immediate, arbitrage, global).

% Organizations like Open Source Ecology and field-tested design repositories building open design documentation, field performance databases, and decision-tree tools that match topology to actual requirements. Sees topology over-specification as a temporary coordination failure being solved through open design knowledge diffusion. Sunset mechanism: as open design knowledge diffuses and field data accumulates, the information asymmetry that enables discretionary over-specification collapses.
narrative_ontology:constraint_stakeholder(topology_selection, open_source_hardware_coalition, observer,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Topology selection solves the architectural tradeoff between system cost, voltage stability, solar integration capability, and maintainability. Different load profiles, solar resources, and maintenance access conditions genuinely require different topologies. The coordination problem is real: matching system architecture to deployment requirements.
% TRANSFER_FUNCTION: The arrangement moves margin and ongoing service revenue from deployment organizations (rural clinics, telecom sites, community facilities) to system integrators through component selection and topology specification. Higher-complexity topologies increase upfront BOM cost by 40-60% and maintenance burden by 25-40% compared to simpler alternatives when requirements are under-specified.
% ABSENT_VOICES: End users (clinic staff, telecom operators, community facility managers) who will operate and maintain the system are typically not in the room during topology selection. Independent engineering reviewers who could evaluate whether simpler topology would meet requirements are absent due to cost constraints. Field technicians who will service the system are consulted but not decision-makers. The absent voices would object to unnecessary complexity if they had technical capacity to evaluate alternatives and budget to commission independent review.
% DISAPPEARANCE_RATIONALE: If topology selection disappeared overnight, deployment organizations would need to develop internal technical capacity to evaluate architectural tradeoffs, or commission independent engineering review, or rely on vendor recommendations (which would recreate similar dynamics). System integrators would lose discretion and margin-capture opportunity. Component vendors would see different (likely lower-value) BOM mix. Field technicians would need different training and diagnostic frameworks. The arrangement structures real economic relationships and technical dependencies.
% FOUNDING_PROBLEM: The founding problem was the genuine need to match off-grid power system architecture to diverse deployment requirements: different load profiles (constant vs variable, AC vs DC), different solar resources (high vs low insolation, seasonal variation), different maintenance access (remote vs accessible, skilled vs unskilled technicians). Early off-grid deployments often failed due to architecture-requirement mismatch: systems over-designed for simple loads (wasting cost), or under-designed for complex loads (failing to meet performance requirements). Topology selection emerged as the coordination mechanism to solve this matching problem.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem remains live: deployment requirements are genuinely diverse, and architecture-requirement mismatch still causes system failures. This is corroborated by field performance studies from organizations outside the integrator-vendor nexus: IEEE Power & Energy Society working groups on off-grid systems, World Bank energy access monitoring reports, and academic field studies from institutions like MIT D-Lab and UC Berkeley Renewable and Appropriate Energy Laboratory. These sources document ongoing architecture-requirement mismatch failures and validate that the coordination problem is real. However, the same sources also document that a significant fraction of Smooth Operator deployments are over-specified relative to requirements, indicating that the coordination mechanism has accumulated extraction alongside its genuine function.
narrative_ontology:disappearance_verdict(topology_selection, world_rearranges).
narrative_ontology:founding_problem_status(topology_selection, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPLOYMENT WITHOUT CLEAR REQUIREMENTS (SNARE) — Rural clinic or remote telecom site deployed with Smooth Operator topology based on integrator recommendation, now locked into higher maintenance burden and component replacement costs. No technical capacity to evaluate alternatives, no budget to redesign. Trapped by sunk cost and lack of local expertise. Maximum extraction.
constraint_indexing:constraint_classification(topology_selection, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: FIELD TECHNICIAN (TANGLED ROPE) — Benefits from standardized topology training and diagnostic procedures, but bears cost of unnecessary complexity when simpler Hammerhead would suffice. Can advocate for simpler designs in some contexts but constrained by integrator specifications and vendor relationships. Mixed experience: coordination function exists (standardization enables training) but extraction is real (over-specification increases service calls and parts inventory burden).
constraint_indexing:constraint_classification(topology_selection, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SYSTEM INTEGRATOR (ROPE) — Experiences topology selection as pure coordination: matching system architecture to deployment requirements, balancing cost vs performance, managing vendor relationships. Captures margin on component selection and benefits from discretion to specify higher-component-count topologies when requirements are ambiguous. Net beneficiary with full exit options — can shift between topologies, vendors, and markets freely.
constraint_indexing:constraint_classification(topology_selection, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: COMPONENT VENDOR (ROPE) — Sells charge controllers, DC-DC converters, battery management systems to integrators. Experiences topology selection as market coordination: more complex topologies mean higher BOM value per deployment. No extraction from vendor perspective — they supply what integrators specify, and simpler topologies would simply mean different (lower-value) BOMs. Pure coordination function with full market mobility.
constraint_indexing:constraint_classification(topology_selection, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN-SOURCE HARDWARE COALITION (SCAFFOLD) — Organizations like Open Source Ecology, Humanitarian OpenStreetMap Team hardware working groups, and field-tested design repositories see topology over-specification as a temporary coordination failure being solved through open design documentation, field performance databases, and decision-tree tools that match topology to actual requirements. Sunset mechanism: as open design knowledge diffuses and field data accumulates, the information asymmetry that enables discretionary over-specification collapses. Estimated sunset: 8-15 years as open hardware documentation matures and field technician training incorporates decision frameworks.
constraint_indexing:constraint_classification(topology_selection, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees genuine coordination function (topology selection solves real architectural tradeoffs: voltage stability vs component count, solar integration capability vs cost, maintainability vs performance) AND asymmetric extraction (integrator discretion in ambiguous-requirement contexts enables specification of higher-complexity topologies that increase BOM cost and maintenance burden beyond functional need). The extraction is not incidental to coordination — it operates through the same decision point. Structural tangled rope from the analytical seat.
constraint_indexing:constraint_classification(topology_selection, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(topology_selection_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(topology_selection, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(topology_selection, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(topology_selection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(topology_selection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate. Integrator discretion in ambiguous-requirement contexts enables specification of Smooth Operator topology that increases BOM cost by 40-60% and maintenance burden by 25-40% compared to Hammerhead, when simpler architecture would meet functional requirements. Extraction is not total — many deployments genuinely need Smooth Operator's voltage stability and solar integration capability — but the ambiguity creates a margin-capture opportunity. The value reflects that roughly half of Smooth Operator deployments are requirement-matched (legitimate coordination) and half are discretionary over-specifications (extraction). Suppression (0.62): Moderate-high. Deployments with under-specified requirements face significant barriers to challenging integrator topology selection: lack of technical capacity to evaluate alternatives, sunk cost in deployed system, no budget for redesign, limited access to independent engineering review, and information asymmetry about field performance of alternative topologies. Suppression has increased over the interval as integrator specifications have become more detailed and technical-appearing, raising the barrier for non-expert challenge. Theater ratio (0.58): Moderate-high. Much of the engineering analysis justifying Smooth Operator selection is performative when requirements are under-specified. Load profile estimates are based on sparse data, solar resource assumptions are optimistic, maintenance access projections are unrealistic, but the analysis produces a specification document that appears rigorous and requirement-driven. The theater serves to legitimate discretionary complexity. Accessibility collapse (0.35): Low-moderate. Alternative topologies (Hammerhead) remain accessible in principle — the physics and component availability do not foreclose simpler architectures. But information asymmetry and integrator discretion create practical barriers. Resistance (0.52): Moderate. Field technicians and some deployment organizations push back against unnecessary complexity, and open-source hardware documentation is creating transparency, but integrator discretion persists through proprietary knowledge and vendor relationships.
 *
 * PERSPECTIVAL GAP:
 *   The system integrator experiences topology selection as pure coordination (Rope) — they are solving the legitimate problem of matching architecture to requirements and managing vendor relationships. The component vendor also sees pure coordination (Rope) — they supply components to specification with no extraction mechanism. The open-source hardware coalition sees a temporary coordination failure being solved through open design documentation (Scaffold) — the information asymmetry is closing. The field technician sees mixed coordination and extraction (Tangled Rope) — standardization enables training but over-specification increases service burden. The deployment without clear requirements sees pure extraction (Snare) — locked into higher-complexity topology by integrator discretion with no capacity to evaluate alternatives or redesign. The analytical observer sees structural tangled rope (Tangled Rope) — genuine coordination function and asymmetric extraction operate through the same decision point, and the extraction is not incidental but enabled by the coordination structure itself. The perspectival gap reveals how the same architectural choice appears as legitimate engineering (from the integrator seat), market coordination (from the vendor seat), solvable information problem (from the open-source seat), mixed benefit-and-burden (from the technician seat), or pure extraction (from the trapped deployment seat).
 *
 * DIRECTIONALITY LOGIC:
 *   The deployment without clear requirements is the primary victim: powerless (no technical capacity to evaluate alternatives), trapped (locked in by sunk cost and lack of redesign budget), bearing maximum extraction through higher upfront cost and ongoing maintenance burden. The engine derives high d (toward full target) from victim status plus trapped exit, producing high effective extraction. The field technician is a secondary victim with mixed experience: moderate power (can advocate for simpler designs in some contexts), constrained exit (limited by integrator specifications), benefits from standardization but bears cost of unnecessary complexity. The engine derives moderate d from victim status modulated by constrained exit and partial beneficiary status (training standardization), producing moderate effective extraction. The system integrator is the primary beneficiary: institutional power, arbitrage exit (full mobility across topologies, vendors, markets), captures margin on component selection and benefits from discretion. The engine derives low d (toward full beneficiary) from beneficiary status plus arbitrage exit, producing low or negative effective extraction (subsidy). The component vendor is a secondary beneficiary with pure coordination experience: institutional power, arbitrage exit, sells what integrators specify with no extraction mechanism from vendor perspective. The open-source hardware coalition has organized power and mobile exit (can build alternative pathways), sees extraction as temporary with sunset mechanism. The analytical observer sees the structural tangled rope: genuine coordination function coexists with asymmetric extraction through the same decision point.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that topology selection has both a genuine coordination function (architectural choice solves real tradeoffs between cost, performance, maintainability, and solar integration capability) AND an asymmetric extraction mechanism (integrator discretion in ambiguous-requirement contexts enables over-specification that increases BOM cost and maintenance burden beyond functional need). The coordination function is not a cover story — the voltage regulation tradeoff and solar integration mechanism are real, and many deployments genuinely need Smooth Operator's capabilities. But the extraction is also real — the ambiguity in requirement specification creates discretion that can be exercised to increase component count and system complexity when simpler architecture would meet functional requirements. The tangled rope classification captures this: the constraint is neither pure coordination (rope) nor pure extraction (snare) but a hybrid where both functions coexist and operate through the same decision point. The extraction is not incidental to the coordination — it is enabled by the coordination structure itself, specifically by the information asymmetry between integrator and deployment organization in contexts where requirements are under-specified. The mandatrophy is resolved by recognizing that the question is not 'coordination or extraction?' but 'how much of each, from which perspective, and through what mechanism?'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    requirement_specification_threshold,
    'What level of requirement specification distinguishes legitimate architectural choice from discretionary over-specification?',
    'Field performance database correlating deployment requirements (load profile, solar availability, grid-tie capability, maintenance access) with topology choice and long-term operational outcomes. Identify cases where Smooth Operator was specified but Hammerhead would have met requirements at lower cost and complexity.',
    'If threshold is low (most deployments have clear requirements): extraction is minimal, classification shifts toward rope. If threshold is high (most deployments have ambiguous requirements): extraction is substantial, classification remains tangled_rope or shifts toward snare for powerless agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(requirement_specification_threshold, empirical, 'Requirement clarity threshold for topology selection').

omega_variable(
    complexity_robustness_correlation,
    'Does higher component count actually correlate with better field reliability, or is the complexity-equals-robustness doctrine a cover story for margin capture?',
    'Longitudinal field reliability study: MTBF, service call frequency, and total cost of ownership for Hammerhead vs Smooth Operator deployments in matched contexts (same load profile, same solar resource, same maintenance access). Control for requirement specification quality.',
    'If complexity improves reliability: integrator discretion is legitimate coordination, vindicated_propositions is accurate. If complexity degrades reliability or shows no correlation: the doctrine is a cover story, extraction is higher than measured, classification shifts toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(complexity_robustness_correlation, empirical, 'Whether topology complexity correlates with field reliability').

omega_variable(
    open_design_diffusion_rate,
    'Will open-source design documentation and decision frameworks actually diffuse fast enough to collapse the information asymmetry, or will integrator discretion persist through proprietary knowledge and vendor lock-in?',
    'Adoption rate tracking for open hardware design repositories and decision-tree tools in off-grid deployment contexts. Measure: percentage of deployments using open design documentation, field technician access to decision frameworks, integrator resistance to open specifications.',
    'If diffusion is fast: scaffold sunset is real, extraction window is closing. If diffusion is slow or blocked: scaffold perspective is aspirational, extraction persists, tangled_rope is stable classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_design_diffusion_rate, empirical, 'Whether open design knowledge diffuses to collapse information asymmetry').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(topology_selection, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(topo_tr_t0, topology_selection, theater_ratio, 0, 0.42).
narrative_ontology:measurement(topo_tr_t3, topology_selection, theater_ratio, 3, 0.48).
narrative_ontology:measurement(topo_tr_t6, topology_selection, theater_ratio, 6, 0.54).
narrative_ontology:measurement(topo_tr_t9, topology_selection, theater_ratio, 9, 0.58).

% Extraction over time
narrative_ontology:measurement(topo_be_t0, topology_selection, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(topo_be_t3, topology_selection, base_extractiveness, 3, 0.4).
narrative_ontology:measurement(topo_be_t6, topology_selection, base_extractiveness, 6, 0.44).
narrative_ontology:measurement(topo_be_t9, topology_selection, base_extractiveness, 9, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(topo_su_t0, topology_selection, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(topo_su_t3, topology_selection, suppression_requirement, 3, 0.55).
narrative_ontology:measurement(topo_su_t6, topology_selection, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(topo_su_t9, topology_selection, suppression_requirement, 9, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(topology_selection, resource_allocation).

% DUAL FORMULATION NOTE:
% Topology selection is downstream of three constraints: transfer_gap_physics (mountain — the physical limits of DC-DC conversion efficiency and battery chemistry), voltage_regulation_tradeoff (rope — the engineering tradeoff between component count and bus stability), and solar_integration_mechanism (rope — the architectural choice for MPPT and charge control). The upstream constraints have their own extractiveness values reflecting the physical and engineering realities; topology selection has its own extractiveness reflecting the integrator discretion and information asymmetry in requirement specification. The upstream constraints are largely coordination (rope) or natural law (mountain); the downstream constraint (topology selection) is where extraction enters through the social layer of requirement ambiguity and integrator discretion.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
