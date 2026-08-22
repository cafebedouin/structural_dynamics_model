% ============================================================================
% CONSTRAINT STORY: software_source_status__utilitarian_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__utilitarian_hybrid_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: software_source_status__utilitarian_hybrid_reading
 *   human_readable: Software Licensing as Welfare-Maximizing Hybrid Model Selection
 *   domain: economic/technological/philosophical
 *
 * SUMMARY:
 *   The utilitarian-hybrid reading frames software licensing as a
 *   context-dependent welfare optimization problem. Under this reading,
 *   open-source and proprietary licensing are neither categorically good nor
 *   bad; each serves different stakeholder contexts better. Infrastructure,
 *   research, and educational contexts benefit maximally from open-source
 *   (transparency, collaboration, accessibility); specialized domains
 *   (medical imaging, aviation, critical finance) may benefit from
 *   proprietary quality assurance and liability frameworks; enterprise
 *   deployment often benefits from hybrid licensing (open-source core with
 *   proprietary support). The reading does not mandate universality; it
 *   empowers context-aware selection. This is ONE reading of the contested
 *   kernel 'software source status' — the other readings (freedom-imperative,
 *   property-rights, pragmatic-development) instantiate different constraints
 *   with different ε values and beneficiary structures. Do not interpret this
 *   reading as covering or addressing those alternatives; they are separate
 *   constraint stories linked via network relationships.
 *
 * KEY AGENTS:
 *   - End users in diverse contexts (research, infrastructure, specialized domains)
 *   - Open-source developers and communities
 *   - Proprietary software vendors and enterprises
 *   - Academic and research institutions
 *   - Enterprise infrastructure teams
 *   - Ecosystem coordinators (foundations, platforms)
 *   - Developers in resource-constrained economies
 *   - Freedom advocates and property-rights advocates (excluded from this reading's conversation)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__utilitarian_hybrid_reading, 0.38).
domain_priors:suppression_score(software_source_status__utilitarian_hybrid_reading, 0.22).
domain_priors:theater_ratio(software_source_status__utilitarian_hybrid_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__utilitarian_hybrid_reading, rope).
narrative_ontology:human_readable(software_source_status__utilitarian_hybrid_reading, "Software Licensing as Welfare-Maximizing Hybrid Model Selection").
narrative_ontology:topic_domain(software_source_status__utilitarian_hybrid_reading, "economic/technological/philosophical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__utilitarian_hybrid_reading, '05be1dda-5601-4b12-b1df-bdbf5b684902').
narrative_ontology:cs_kernel_codification('05be1dda-5601-4b12-b1df-bdbf5b684902', distributed).
narrative_ontology:cs_authority_grounding('05be1dda-5601-4b12-b1df-bdbf5b684902', diffuse_epistemic).
narrative_ontology:cs_reading_relation('05be1dda-5601-4b12-b1df-bdbf5b684902', software_source_status__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('05be1dda-5601-4b12-b1df-bdbf5b684902', software_source_status__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('05be1dda-5601-4b12-b1df-bdbf5b684902', software_source_status__pragmatic_development_reading, influences).
narrative_ontology:cs_axiom('05be1dda-5601-4b12-b1df-bdbf5b684902', foundational, welfare_maximization_as_primary_optimization_criterion).
narrative_ontology:cs_axiom_status(welfare_maximization_as_primary_optimization_criterion, holdable).
narrative_ontology:cs_axiom_grounding('05be1dda-5601-4b12-b1df-bdbf5b684902', welfare_maximization_as_primary_optimization_criterion, instrumental).
narrative_ontology:cs_axiom('05be1dda-5601-4b12-b1df-bdbf5b684902', foundational, context_dependent_licensing_legitimacy).
narrative_ontology:cs_axiom_status(context_dependent_licensing_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('05be1dda-5601-4b12-b1df-bdbf5b684902', context_dependent_licensing_legitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('05be1dda-5601-4b12-b1df-bdbf5b684902', pluralistic_licensing_ecosystem).
narrative_ontology:cs_drift_state('05be1dda-5601-4b12-b1df-bdbf5b684902', contemporary_open_source_dominance_with_proprietary_persistence, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('05be1dda-5601-4b12-b1df-bdbf5b684902', '').
narrative_ontology:cs_kernel_id(software_source_status__utilitarian_hybrid_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, end_users_across_contexts).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, specialized_domain_practitioners).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, infrastructure_maintainers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, open_source_developers).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, academic_researchers).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, enterprise_infrastructure_teams).
narrative_ontology:constraint_victim(software_source_status__utilitarian_hybrid_reading, open_source_developers).
narrative_ontology:constraint_victim(software_source_status__utilitarian_hybrid_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(software_source_status__utilitarian_hybrid_reading, developers_in_developing_economies).
narrative_ontology:constraint_vindicates(software_source_status__utilitarian_hybrid_reading, context_dependent_optimization).
narrative_ontology:constraint_vindicates(software_source_status__utilitarian_hybrid_reading, mixed_ecosystem_viability).
narrative_ontology:constraint_vindicates(software_source_status__utilitarian_hybrid_reading, welfare_pluralism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from access to software optimized for their specific context. Some contexts (scientific research, infrastructure) benefit maximally from open-source transparency and collaborative development; others (specialized domain software like medical imaging) may benefit from proprietary quality assurance and dedicated support. Under this reading, the goal is matching license model to context, not imposing a single model universally.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, end_users_across_contexts, beneficiary,
    organized, biographical, mobile, global).

% Voluntarily contribute to open-source projects, gaining reputation, skill development, and collaborative benefit. They accept unpaid labor as part of their professional identity and community participation. Some pay a cost in forgone proprietary income; the reading treats this as a rational choice within their optimization context.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, open_source_developers, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__utilitarian_hybrid_reading, open_source_developers, payer).

% Invest in proprietary software development, recoup costs through licensing, and provide intensive support and quality assurance for specialized domains. The reading does not frame this as inherently extractive; it recognizes proprietary licensing as a welfare-maximizing choice in contexts where quality, liability, or support are critical.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, proprietary_software_vendors, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__utilitarian_hybrid_reading, proprietary_software_vendors, payer).

% Benefit maximally from open-source software for reproducibility, transparency, and collaborative research. They can access, modify, and share tools freely. This reading treats open source as the welfare-maximizing choice for their specific context, not a universal imperative.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, academic_researchers, beneficiary,
    moderate, generational, arbitrage, global).

% Operate critical systems (cloud, networking, databases) where open-source tooling combined with proprietary support contracts (hybrid licensing) often maximizes welfare: they get source access for debugging, control for security, and paid support.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, enterprise_infrastructure_teams, beneficiary,
    powerful, generational, constrained, global).

% Hold that software freedom is a fundamental ethical requirement regardless of context; proprietary software is categorically unjust. They object that the utilitarian-hybrid reading instrumentalizes freedom and permits harm. They are not in the conversation that this reading conducts.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, freedom_imperative_advocates, excluded,
    moderate, generational, mobile, global).

% Hold that creators have fundamental intellectual-property rights over their software; restricting access and modification is legitimate. They object that the utilitarian reading subordinates ownership rights to welfare outcomes. They are not centered in this reading's framework.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, property_rights_advocates, excluded,
    moderate, generational, mobile, global).

% Face high costs for proprietary software licensing due to currency and purchasing-power differences. Open-source availability is often their only practical access to modern tooling. The reading recognizes this context-dependent asymmetry: welfare maximization in their context strongly favors open-source accessibility, yet the constraint itself does not mandate it — outcomes depend on ecosystem choices made by upstream developers and vendors.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, developers_in_developing_economies, payer,
    powerless, biographical, trapped, global).

% Institutions and communities that choose licensing models for their projects (Linux Foundation, Apache Software Foundation, GitHub community norms, corporate open-source offices). This reading empowers them to make context-aware decisions: open source for infrastructure and research, proprietary or dual-licensed for specialized domains.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, ecosystem_coordinators, agenda_setter,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates software artifacts across contexts via licensing-model selection, matching transparency/collaboration/freedom (open source) with quality assurance/liability/support (proprietary) to maximize welfare in each case. Coordinates diverse stakeholder expectations into a pluralistic ecosystem rather than enforcing a single normative model.
% TRANSFER_FUNCTION: Moves decision-making authority over software governance to context-aware agents: open-source projects gain visibility and collaboration; proprietary vendors gain revenue protection; users in each context receive tools optimized for their welfare. The constraint transfers legitimacy for licensing choice from universal imperative to contextual reasoning.
% ABSENT_VOICES: Stakeholders who reject the utilitarian-hybrid framing entirely — freedom-imperative advocates (who see proprietary software as categorically unjust regardless of consequences) and strong property-rights advocates (who see licensing choice as an owner's absolute right, not a welfare calculation). These voices would argue that the reading instrumentalizes fundamental values; they are structurally excluded by the reading's consequentialist framing.
% DISAPPEARANCE_RATIONALE: If the utilitarian-hybrid reading vanished (replaced by a mandate for universal open source or universal property rights), software licensing practice would reorganize: either all software would be open-source-only (eliminating proprietary quality assurance in specialized domains and professional support revenue), or all would be proprietary (blocking access for developers in poor economies and eliminating the collaborative infrastructure base that modern development depends on). The constraint's existence as a legitimacy frame enables the mixed ecosystem; its absence would force a categorical choice.
% FOUNDING_PROBLEM: Early software licensing was binary: either proprietary (full control, high cost, limited transparency) or informal (uncontrolled copying, no sustainability). Neither maximized aggregate welfare across diverse use cases. The welfare-maximizing frame recognizes that different contexts have different optima: research needs transparency, infrastructure needs reliability, specialized domains need liability protection.
% FOUNDING_PROBLEM_CORROBORATION: Empirical ecosystem data from outside the reading's own advocacy: the success and growth of mixed licensing (GPL, Apache, MIT, dual-licensed hybrids) shows welfare gains across contexts. Infrastructure communities (Cloud Native Computing Foundation, Kubernetes) and academic institutions document higher productivity and collaboration in open-source contexts. Simultaneously, proprietary software in medical imaging, aviation, and financial systems documents welfare improvements from liability, quality assurance, and dedicated support that the market structure enables. Neither sector's welfare metrics align with the categories alone; the correlation is context-dependent.
narrative_ontology:disappearance_verdict(software_source_status__utilitarian_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__utilitarian_hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__utilitarian_hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_source_status__utilitarian_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__utilitarian_hybrid_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__utilitarian_hybrid_reading_tests).
:- end_tests(software_source_status__utilitarian_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.38) is moderate because the reading permits proprietary licensing in some contexts, acknowledging that vendors recoup investment through fees; however, it is not high because the reading does NOT categorize proprietary licensing as extractive per se — extraction only emerges when a model optimized for one context is imposed on a context where a different model would maximize welfare. Suppression is low (0.22) because the reading does not enforce a single model; context-aware agents retain choice. Theater is minimal (0.18) because the reasoning is explicitly consequentialist and empirical: match the model to the context. The measurement series show stability over the interval because the core reading (welfare-maximization in context) remains consistent; the modest rise and fall in theater reflect increasing articulation of the contextual reasoning (more explicit framing, less performance) mid-interval, then normalization. Resistance is high (0.71) because freedom advocates and property-rights advocates both contest the reading's core premise that welfare outcomes should drive licensing choice rather than fundamental principles.
 *
 * PERSPECTIVAL GAP:
 *   The utilitarian frame creates a structural asymmetry: freedom advocates see the reading as subordinating ethical imperatives to consequences; property-rights advocates see it as subordinating legitimate ownership to external welfare criteria. From the utilitarian seat, both framings are valid within their own value systems, but the reading explicitly chooses to optimize consequences over principles. This is a genuine value-commitment conflict, not a factual disagreement. The engine's per-seat computation will show divergent types from these different seats.
 *
 * DIRECTIONALITY LOGIC:
 *   This reading declares no categorical victims because extraction is context-dependent. In contexts where open-source licensing maximizes welfare (research, infrastructure), proprietary licensing would be extractive; in contexts where proprietary licensing maximizes welfare (specialized domains with high liability), open-source-only would be extractive. The beneficiaries are those who gain decision-making authority aligned with their context (researchers get open source, specialized practitioners get proprietary options with support). Developers in resource-constrained economies benefit from open-source availability but are not framed as victims under this reading — rather, their welfare is optimized by ensuring open-source remains available as a choice, not by mandating it universally. The reading's directional claim is that welfare maximization as the decision criterion benefits all seats, even if specific licensing choices vary by context.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (matching licensing models to context optima) is live and contested. Freedom advocates deny the problem exists — they hold that freedom is categorically required regardless of consequences. Property-rights advocates deny it as stated — they hold that creators' rights, not aggregate welfare, should drive the choice. The reading sustains its mandate only by accepting this contestation explicitly. The disappearance verdict (world_rearranges) reflects the genuine coordinating role: if the welfare-maximizing frame disappeared, licensing choice would revert to either universal ideological mandates (all open, all proprietary) or raw market power, both of which would reduce welfare in some contexts. This is what keeps the reading live despite ideological opposition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_measurement_context_dependency,
    'How should ''aggregate welfare'' be measured and aggregated across contexts with incommensurable values (research freedom vs. patient safety vs. user accessibility)?',
    'Empirical welfare assessment in each sector (publication output and replicability in research, patient outcome metrics in medical software, accessibility metrics in consumer software) reveals whether welfare-maximizing licensing models correlate with identifiable welfare improvements or whether the correlation is confounded by other factors.',
    'If welfare is measurable and licensing choice correlates with improvements, the reading''s consequentialist frame is empirically defensible. If welfare cannot be aggregated across contexts or if licensing choice is a weak signal, the reading''s theoretical foundation weakens and the framework shifts toward either deontological (freedom-based or rights-based) reasoning.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_measurement_context_dependency, empirical, 'Whether aggregate welfare is computable across diverse software contexts or remains fundamentally context-local.').

omega_variable(
    reading_commitment_system_conflict,
    'Does the utilitarian-hybrid reading''s core commitment to welfare-maximization foreclose or coexist with the freedom-imperative reading''s core commitment to ethical imperatives?',
    'Logical analysis of the two axioms: if a framework permits BOTH welfare-maximization-as-primary and freedom-as-non-negotiable, the readings coexist (different parties emphasize different optima within the same framework). If neither can be subordinated to the other, they foreclose each other.',
    'If they foreclose, the kernel admits no single unified interpretation; licensing philosophy must operate as genuine pluralism (multiple incompatible framings, no synthesis). If they coexist, a meta-framework could treat them as two optimization dimensions and explore trade-offs. The reading''s classification depends partly on this answer.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_commitment_system_conflict, conceptual, 'Whether the utilitarian and freedom-imperative readings are logically compatible or mutually excluding in their core axioms.').

omega_variable(
    proprietary_quality_assurance_causality,
    'In specialized domains (medical imaging, aviation), does proprietary licensing causally drive higher quality/safety, or does it correlate with quality because specialized high-stakes domains attract better-funded development regardless of licensing?',
    'Comparative analysis of quality metrics (safety records, failure rates, security vulnerabilities) across proprietary and open-source software in the same domain. Natural experiments from domains that shifted licensing models.',
    'If proprietary licensing causally drives quality in specialized domains, the utilitarian reading''s claim that proprietary maximizes welfare in those contexts is supported. If quality correlates primarily with funding and expertise regardless of licensing, the reading''s distinction collapses and welfare-maximization may favor open-source universally.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proprietary_quality_assurance_causality, empirical, 'Whether proprietary licensing itself drives superior quality outcomes or whether correlation reflects other causal factors (funding, liability, specialization).').

omega_variable(
    ecosystem_coordination_neutrality,
    'Can ecosystem coordinators (foundations, platforms, developers) actually remain neutral on licensing choice, or does the infrastructure itself embed biases toward one model?',
    'Ethnographic and structural analysis of how Git platforms, package managers, and development communities signal and enforce licensing preferences. Measurement of switching costs and social pressure for different licensing models.',
    'If coordinators cannot be neutral, the reading''s claim that context-aware choice is possible is undermined — institutional bias would enforce a de-facto standard regardless of declared pluralism. If neutrality is achievable, the reading''s empowerment claim is viable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecosystem_coordination_neutrality, empirical, 'Whether software infrastructure can genuinely enable neutral choice among licensing models or inherently favors one.').

omega_variable(
    kernel_reading_relationship,
    'Is the utilitarian-hybrid reading a live interpretation of the software-source-status kernel, or does it represent a meta-frame that tries to transcend the contested kernel rather than inhabit it?',
    'Examination of whether practicing developers, legal frameworks, and institutional actors actually invoke utilitarian-welfare reasoning when making licensing decisions, or whether they primarily appeal to freedom, rights, or methodology.',
    'If utilitarian reasoning is a live frame within the kernel''s community, the reading is valid and will coexist with others. If it is an external meta-frame attempting to adjudicate the contest, it does not instantiate a reading of the kernel — it attempts to dissolve it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relationship, conceptual, 'Whether the utilitarian-hybrid frame is a genuine reading within the kernel''s own interpretive tradition or an external attempt to transcend the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__utilitarian_hybrid_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__utilitarian_hybrid_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(soft_tr_t0, observed).
narrative_ontology:measurement(soft_tr_t5, software_source_status__utilitarian_hybrid_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement_basis(soft_tr_t5, observed).
narrative_ontology:measurement(soft_tr_t10, software_source_status__utilitarian_hybrid_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement_basis(soft_tr_t10, observed).
narrative_ontology:measurement(soft_tr_t15, software_source_status__utilitarian_hybrid_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement_basis(soft_tr_t15, observed).
narrative_ontology:measurement(soft_tr_t20, software_source_status__utilitarian_hybrid_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement_basis(soft_tr_t20, observed).
narrative_ontology:measurement(soft_tr_t25, software_source_status__utilitarian_hybrid_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement_basis(soft_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(soft_be_t0, observed).
narrative_ontology:measurement(soft_be_t5, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 5, 0.36).
narrative_ontology:measurement_basis(soft_be_t5, observed).
narrative_ontology:measurement(soft_be_t10, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement_basis(soft_be_t10, observed).
narrative_ontology:measurement(soft_be_t15, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 15, 0.39).
narrative_ontology:measurement_basis(soft_be_t15, observed).
narrative_ontology:measurement(soft_be_t20, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement_basis(soft_be_t20, observed).
narrative_ontology:measurement(soft_be_t25, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement_basis(soft_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(soft_su_t0, observed).
narrative_ontology:measurement(soft_su_t5, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 5, 0.19).
narrative_ontology:measurement_basis(soft_su_t5, observed).
narrative_ontology:measurement(soft_su_t10, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 10, 0.21).
narrative_ontology:measurement_basis(soft_su_t10, observed).
narrative_ontology:measurement(soft_su_t15, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 15, 0.23).
narrative_ontology:measurement_basis(soft_su_t15, observed).
narrative_ontology:measurement(soft_su_t20, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement_basis(soft_su_t20, observed).
narrative_ontology:measurement(soft_su_t25, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 25, 0.22).
narrative_ontology:measurement_basis(soft_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__utilitarian_hybrid_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_source_status__utilitarian_hybrid_reading, 0.12).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__property_rights_reading).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__pragmatic_development_reading).

% DUAL FORMULATION NOTE:
% The constraint 'software_source_status' is a contested kernel admitting multiple readings with structurally distinct constraints. This story instantiates the utilitarian-hybrid reading: software licensing should maximize aggregate welfare; both open-source and proprietary models serve different contexts. Sibling readings — freedom-imperative, property-rights, pragmatic-development — instantiate different constraints from the same kernel (different ε values, different beneficiary/victim structures, different types). Each reading is one epistemic interpretation of software licensing norms; the network relationships show how readings influence each other's legitimacy conditions without foreclosing them (except in rare cases of direct axiom contradiction). The ε-invariance principle applies: each reading has one stable referent (the standing arrangement under contest — software licensing norms — assessed by the reading's own epistemic frame) and one stable ε (how much welfare the reading sees as gained or lost under the utilitarian vs. alternative framings).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
