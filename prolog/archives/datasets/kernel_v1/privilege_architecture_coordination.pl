% ============================================================================
% CONSTRAINT STORY: privilege_architecture_coordination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_privilege_architecture_coordination, []).

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
 *   constraint_id: privilege_architecture_coordination
 *   human_readable: Attorney-Client Privilege Architecture Coordination in AI Legal Technology
 *   domain: legal_technology/data_sovereignty/professional_services
 *
 * SUMMARY:
 *   The attorney-client privilege architecture constraint emerges from the
 *   intersection of the Heppner doctrine (third-party access to privileged
 *   communications waives privilege) and AI legal technology deployment
 *   models. Law firms adopting AI tools for document review, legal research,
 *   or contract analysis face a structural choice: deploy on-premise systems
 *   where the firm maintains physical custody and no vendor has operational
 *   access (privilege preserved), or use SaaS platforms where vendor
 *   operational access (for maintenance, support, model training, or system
 *   monitoring) may trigger Heppner waiver. This constraint is fundamentally
 *   a coordination mechanism — it solves the collective action problem of
 *   'how do we adopt AI without waiving privilege?' — but the coordination
 *   function has asymmetric effects because infrastructure requirements
 *   create barriers that benefit firms with existing technical capacity. The
 *   constraint does not require active enforcement (no regulatory body
 *   mandates on-premise deployment), but the legal risk of waiver creates
 *   strong incentives for privilege-preserving architectures. Theater ratio
 *   is moderate and rising (0.35) as some firms adopt 'privilege-washing'
 *   practices (contractual terms claiming privilege protection without actual
 *   architectural guarantees), but the core coordination function remains
 *   genuine.
 *
 * KEY AGENTS:
 *   - Large Law Firms with Technical Infrastructure: Primary beneficiary (institutional/arbitrage) — can deploy on-premise systems, experience pure coordination
 *   - Mid-Size Firms Without Infrastructure: Mixed position (moderate/constrained) — need AI tools but lack capital/staff for on-premise deployment; experience coordination + extraction
 *   - Enterprise Legal Departments: Beneficiary (institutional/mobile) — existing IT infrastructure makes on-premise deployment feasible; experience coordination
 *   - On-Premise Vendors: Beneficiary (powerful/mobile) — privilege architecture requirement creates demand for their product category
 *   - SaaS Vendors with Operational Access: Mixed position (moderate/constrained) — operational model conflicts with privilege architecture; can exit by redesigning but at cost
 *   - Analytical Observer: Sees coordination mechanism (analytical/analytical) — genuine collective action problem with asymmetric infrastructure barrier
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(privilege_architecture_coordination, 0.38).
domain_priors:suppression_score(privilege_architecture_coordination, 0.42).
domain_priors:theater_ratio(privilege_architecture_coordination, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(privilege_architecture_coordination, extractiveness, 0.38).
narrative_ontology:constraint_metric(privilege_architecture_coordination, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(privilege_architecture_coordination, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(privilege_architecture_coordination, rope).
narrative_ontology:human_readable(privilege_architecture_coordination, "Attorney-Client Privilege Architecture Coordination in AI Legal Technology").
narrative_ontology:topic_domain(privilege_architecture_coordination, "legal_technology/data_sovereignty/professional_services").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(privilege_architecture_coordination, law_firms_with_technical_capacity).
narrative_ontology:constraint_beneficiary(privilege_architecture_coordination, enterprise_legal_departments).
narrative_ontology:constraint_beneficiary(privilege_architecture_coordination, on_premise_vendors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LARGE LAW FIRM (ROPE) — Firm with capital and technical capacity to deploy on-premise AI systems experiences this as pure coordination: the privilege architecture requirement (physical custody + zero vendor access) is a legitimate technical specification that solves the Heppner waiver problem. The firm benefits from the coordination function (clear privilege boundary) and can arbitrage between deployment models based on case sensitivity.
constraint_indexing:constraint_classification(privilege_architecture_coordination, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-SIZE FIRM (TANGLED ROPE) — Firm without capital or technical staff to deploy on-premise systems faces genuine coordination need (privilege protection) but also experiences extraction: the architecture requirement creates a barrier to AI adoption that benefits firms with existing infrastructure. Can exit to SaaS with waiver risk or forego AI tools, but both options impose costs. Coordination function is real (privilege is a legitimate concern) but asymmetric extraction is also present (infrastructure barrier).
constraint_indexing:constraint_classification(privilege_architecture_coordination, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ENTERPRISE LEGAL DEPARTMENT (ROPE) — In-house counsel with enterprise IT infrastructure experiences this as coordination: the privilege architecture is a technical specification that integrates with existing data governance. Mobile exit options (can deploy on-premise or negotiate vendor contracts with attestation architecture) and benefits from clear privilege boundaries. Low effective extraction.
constraint_indexing:constraint_classification(privilege_architecture_coordination, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: ON-PREMISE VENDOR (ROPE) — Vendors offering on-premise deployment models benefit from the coordination function: the privilege architecture requirement creates demand for their product category. Experience this as legitimate coordination (solving a real technical problem) with mobile exit (can also offer SaaS with attestation architecture if market demands it).
constraint_indexing:constraint_classification(privilege_architecture_coordination, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: SAAS VENDOR (TANGLED ROPE) — Vendors whose operational model requires access to customer data (for model training, system maintenance, or support) face genuine coordination need (customers need AI tools) but also experience extraction: the privilege architecture requirement creates a structural disadvantage relative to on-premise competitors. Can exit by redesigning architecture (zero-knowledge systems, client-side processing) but this imposes significant engineering costs. Coordination and extraction both present.
constraint_indexing:constraint_classification(privilege_architecture_coordination, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From the analytical perspective, the privilege architecture is a coordination mechanism that solves a genuine collective action problem: how to adopt AI tools in legal practice without waiving attorney-client privilege. The Heppner doctrine (third-party access waives privilege) is the upstream constraint; the architecture requirement is a technical response that preserves privilege while enabling technology adoption. Base extraction is moderate (0.38) because infrastructure requirements create barriers, but the coordination function is genuine and the constraint does not require active enforcement to suppress alternatives.
constraint_indexing:constraint_classification(privilege_architecture_coordination, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(privilege_architecture_coordination_tests).
:- end_tests(privilege_architecture_coordination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The privilege architecture requirement creates a genuine barrier to AI adoption for firms without technical infrastructure, but the extraction is not severe because: (1) the coordination function is real (privilege protection is a legitimate concern, not a pretext), (2) alternative architectures are emerging (attestation systems, zero-knowledge proofs, client-side processing), and (3) cloud infrastructure commoditization is reducing deployment costs. The extraction is rising slowly (0.30 → 0.38 over 6 years) as AI adoption accelerates and the infrastructure gap becomes more salient. Suppression (0.42): Moderate. Barriers include capital requirements for on-premise deployment, technical staff expertise, ongoing maintenance costs, and legal risk of waiver if architecture is misconfigured. But suppression is not high because: (1) SaaS options exist (with waiver risk trade-off), (2) managed service providers can reduce technical burden, and (3) no regulatory mandate prevents alternative approaches. Theater ratio (0.35): Moderate and rising. Some theater is present in contractual 'privilege protection' claims that lack architectural substance (vendor contracts claiming privilege preservation without actual zero-access guarantees), and in performative security audits that don't verify operational access controls. But the core function (privilege preservation via architecture) is not theatrical — the technical distinction between physical custody and vendor access is real and measurable.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how a genuine coordination mechanism (privilege preservation) can produce asymmetric effects based on infrastructure capacity. Large firms and enterprise legal departments experience pure coordination (Rope) — the architecture requirement solves a real problem they can afford to solve. Mid-size firms and SaaS vendors experience mixed coordination and extraction (Tangled Rope) — the problem is real but the solution imposes costs that benefit competitors with existing infrastructure. The analytical observer confirms the coordination function is genuine (Rope from civilizational perspective) — this is not a false summit naturalizing extraction as law. The perspectival gap is structural: agents with infrastructure see coordination; agents without infrastructure see coordination + barrier. The gap is measurable via exit options (arbitrage vs constrained) and beneficiary/victim status.
 *
 * DIRECTIONALITY LOGIC:
 *   Large law firms with existing infrastructure are beneficiaries — they experience the privilege architecture as pure coordination (solving a real problem) and have arbitrage exit options (can choose deployment model based on case sensitivity). Their low d value reflects that extraction flows away from them. Mid-size firms without infrastructure are partial victims — they need AI tools (coordination function is real) but the architecture requirement imposes costs they cannot easily absorb (extraction). Their moderate d value reflects mixed experience. Enterprise legal departments are beneficiaries with mobile exit — existing IT infrastructure makes compliance feasible. On-premise vendors are beneficiaries — the constraint creates demand for their product. SaaS vendors with operational access are partial victims — their business model conflicts with the architecture requirement, but they can exit by redesigning (constrained, not trapped). The analytical observer sees coordination — the Heppner doctrine is the upstream constraint; the architecture requirement is a technical response that preserves privilege while enabling AI adoption.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that moderate extraction (0.38) can coexist with genuine coordination function, producing Tangled Rope from some perspectives and Rope from others. The coordination function is not pretextual — attorney-client privilege is a real legal doctrine, Heppner waiver is a real risk, and the architecture requirement is a legitimate technical response. But the coordination mechanism has asymmetric effects because infrastructure requirements create barriers. The constraint is not a Snare (extraction is moderate, not severe; suppression is moderate, not high; no victims are trapped) and not a Mountain (it does not emerge naturally; it is a constructed response to the Heppner doctrine). The Rope classification from the analytical perspective reflects that the coordination function is genuine and the constraint does not require active enforcement. The Tangled Rope classification from mid-size firms reflects that coordination and extraction are both structurally present for agents without infrastructure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    attestation_architecture_sufficiency,
    'Do cryptographic attestation architectures (zero-knowledge proofs, client-side processing, encrypted enclaves) provide privilege protection equivalent to physical custody?',
    'Case law development on whether technical controls preventing vendor access satisfy Heppner''s ''reasonable expectation of confidentiality'' standard; expert testimony on cryptographic guarantees vs physical custody',
    'If equivalent: SaaS vendors can compete on equal footing by redesigning architecture, reducing extraction. If not equivalent: physical custody remains privileged, maintaining infrastructure barrier.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attestation_architecture_sufficiency, empirical, 'Whether attestation architecture provides privilege protection equivalent to physical custody').

omega_variable(
    waiver_risk_threshold,
    'What level of vendor operational access triggers Heppner waiver? Is any vendor access fatal, or only access to privileged content?',
    'Judicial interpretation of Heppner in AI/SaaS context; distinction between access to system (metadata, logs, performance metrics) vs access to privileged communications',
    'If any access is fatal: on-premise deployment is the only safe harbor, maximizing extraction. If only content access matters: SaaS vendors can offer privilege-preserving services with operational access to non-privileged system data.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(waiver_risk_threshold, empirical, 'Threshold of vendor access that triggers privilege waiver').

omega_variable(
    infrastructure_cost_trajectory,
    'Are on-premise deployment costs rising or falling relative to SaaS? Does cloud infrastructure commoditization reduce the barrier?',
    'Market analysis of total cost of ownership for on-premise AI systems vs SaaS; tracking of cloud infrastructure pricing and managed service offerings',
    'If costs converge: infrastructure barrier diminishes, reducing extraction. If costs diverge: barrier increases, amplifying extraction toward firms with existing infrastructure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(infrastructure_cost_trajectory, empirical, 'Trajectory of infrastructure cost differential between on-premise and SaaS').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(privilege_architecture_coordination, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(priv_arch_tr_t0, privilege_architecture_coordination, theater_ratio, 0, 0.25).
narrative_ontology:measurement(priv_arch_tr_t3, privilege_architecture_coordination, theater_ratio, 3, 0.3).
narrative_ontology:measurement(priv_arch_tr_t6, privilege_architecture_coordination, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(priv_arch_be_t0, privilege_architecture_coordination, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(priv_arch_be_t3, privilege_architecture_coordination, base_extractiveness, 3, 0.34).
narrative_ontology:measurement(priv_arch_be_t6, privilege_architecture_coordination, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(privilege_architecture_coordination, information_standard).

% DUAL FORMULATION NOTE:
% The privilege architecture constraint is downstream of the Heppner doctrine (third-party access waives privilege). Heppner is the upstream legal constraint; the architecture requirement is a technical coordination response. If Heppner were relaxed or clarified (e.g., courts rule that cryptographic attestation satisfies 'reasonable expectation of confidentiality'), the architecture constraint's extraction would decrease as SaaS vendors could compete on equal footing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
