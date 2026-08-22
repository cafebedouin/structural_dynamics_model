% ============================================================================
% CONSTRAINT STORY: software_source_status__utilitarian_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   domain: intellectual_property/software_engineering/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates a utilitarian-hybrid reading of the
 *   contested kernel 'software_source_status' — the fundamental question of
 *   whether software should be open or proprietary. The utilitarian reading
 *   rejects both categorical imperatives ("all software must be free" and
 *   "creators own absolute property rights") and instead frames licensing as
 *   an optimization problem: choose the model that maximizes aggregate
 *   welfare in the specific domain context. Infrastructure and foundational
 *   libraries favor open licensing; specialized tools with small addressable
 *   markets may justify proprietary models; hybrid ecosystems mixing both
 *   often perform best. The constraint itself does not mandate outcomes—it
 *   authorizes context-dependent analysis and permits licensing diversity.
 *   This reading coexists with three siblings: freedom-imperative (ethical
 *   requirement for openness), pragmatic-development (methodological
 *   superiority of open source), and property-rights (absolute creator
 *   control). Each reading produces a different constraint with different
 *   beneficiary structures and extraction profiles. This story generates only
 *   the utilitarian-hybrid reading as a clean, ε-invariant constraint.
 *
 * KEY AGENTS:
 *   - Open-source ecosystem: maintains Linux, Apache, OpenSSL; benefits from permissive licensing and volunteer contribution
 *   - End users in critical infrastructure: depend on open-source auditability and fork-ability for security and vendor-independence
 *   - Specialized-tool developers: small-market creators (scientific, professional domain tools) who justify proprietary licensing by welfare recovery
 *   - Commercial vendors: pay proprietary licenses; also benefit from open-source components; navigate mixed ecosystems
 *   - Individual developers: enjoy free tools; pay when productivity gains justify cost; can arbitrage between open and proprietary
 *   - Freedom advocates (excluded): would reject proprietary licensing as unethical; not in the conversation under this reading
 *   - Property-rights advocates (excluded): would reject welfare-based constraints on creator rights; not in the conversation under this reading
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
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__utilitarian_hybrid_reading, rope).
narrative_ontology:human_readable(software_source_status__utilitarian_hybrid_reading, "Software Licensing as Welfare-Maximizing Hybrid Model Selection").
narrative_ontology:topic_domain(software_source_status__utilitarian_hybrid_reading, "intellectual_property/software_engineering/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__utilitarian_hybrid_reading, 'ec80ed01-3db4-43a6-bff6-e6c42c938265').
narrative_ontology:cs_kernel_codification('ec80ed01-3db4-43a6-bff6-e6c42c938265', distributed).
narrative_ontology:cs_authority_grounding('ec80ed01-3db4-43a6-bff6-e6c42c938265', distributed).
narrative_ontology:cs_reading_relation('ec80ed01-3db4-43a6-bff6-e6c42c938265', software_source_status__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('ec80ed01-3db4-43a6-bff6-e6c42c938265', software_source_status__pragmatic_development_reading, influences).
narrative_ontology:cs_reading_relation('ec80ed01-3db4-43a6-bff6-e6c42c938265', software_source_status__property_rights_reading, coexists_with).
narrative_ontology:cs_axiom('ec80ed01-3db4-43a6-bff6-e6c42c938265', foundational, context_dependent_welfare_optimization).
narrative_ontology:cs_axiom_status(context_dependent_welfare_optimization, holdable).
narrative_ontology:cs_axiom_grounding('ec80ed01-3db4-43a6-bff6-e6c42c938265', context_dependent_welfare_optimization, instrumental).
narrative_ontology:cs_axiom('ec80ed01-3db4-43a6-bff6-e6c42c938265', secondary, mixed_ecosystem_permissible).
narrative_ontology:cs_axiom_status(mixed_ecosystem_permissible, holdable).
narrative_ontology:cs_axiom_grounding('ec80ed01-3db4-43a6-bff6-e6c42c938265', mixed_ecosystem_permissible, instrumental).
narrative_ontology:cs_reference_frame('ec80ed01-3db4-43a6-bff6-e6c42c938265', welfare_maximizing_equilibrium).
narrative_ontology:cs_drift_state('ec80ed01-3db4-43a6-bff6-e6c42c938265', contemporary_platform_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ec80ed01-3db4-43a6-bff6-e6c42c938265', '').
narrative_ontology:cs_kernel_id(software_source_status__utilitarian_hybrid_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, end_users_critical_infrastructure).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, specialized_tool_developers).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, open_source_ecosystem_maintainers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, open_source_ecosystem).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, commercial_vendors).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, individual_developers).
narrative_ontology:constraint_victim(software_source_status__utilitarian_hybrid_reading, commercial_vendors).
narrative_ontology:constraint_victim(software_source_status__utilitarian_hybrid_reading, individual_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains foundational open-source projects (Linux, Apache, OpenSSL, Git, Kubernetes) under permissive (MIT, Apache 2.0) and copyleft (GPL) licenses. Attracts volunteer contributors through source transparency, collaborative governance, and reputational incentives. Benefits from reduced barriers to contribution, rapid feature iteration, and community-driven security review. Can fork projects if maintainers become untrustworthy, migrate to alternative communities, or establish new license regimes in response to corporate capture.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, open_source_ecosystem, beneficiary,
    organized, generational, mobile, global).

% Depend on open-source software for critical infrastructure: Linux powers cloud providers, data centers, and embedded systems; OpenSSL secures HTTPS and cryptographic protocols; Apache and Nginx run the majority of web servers; DNS, routing, and telecommunications infrastructure rely on open-source foundations. Access to source code enables independent security auditing, vulnerability detection, and fork-ability if the primary maintainer becomes unreliable or abandoned. Benefits from reduced vendor lock-in and the ability to maintain systems independently if upstream abandons them.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, end_users_critical_infrastructure, beneficiary,
    powerful, generational, mobile, global).

% Develop domain-specific, low-scale software: scientific instruments (FEA solvers, bioinformatics pipelines), professional tools (CAD plugins, medical imaging software), research code (machine-learning frameworks with specialized hardware support). Small addressable markets (hundreds to thousands of potential customers) make open-source sustainability difficult because volunteer contributions scale poorly to domain-specific problems. Proprietary licensing allows cost recovery on small markets, protects against low-cost competitive clones by competitors who repackage open code, and permits sustainable pricing strategies. Can offer tiered licensing (free academic, commercial fees), professional support (consulting, training), or customization services while retaining proprietary source.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, specialized_tool_developers, beneficiary,
    moderate, biographical, constrained, global).

% Navigate mixed ecosystems: pay for proprietary licenses (IDEs, specialized libraries, cloud platforms) when tools deliver strategic advantage or when open-source equivalents require extensive customization. Also benefit from open-source libraries and components that reduce development burden and time-to-market (web frameworks, database drivers, testing tools). Often build proprietary user-facing products on open-source foundations (SaaS stacks, mobile apps, embedded systems). Can negotiate volume discounts, support contracts, contribute code back to open projects to maintain leverage and reduce future licensing risk.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, commercial_vendors, payer,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__utilitarian_hybrid_reading, commercial_vendors, beneficiary).

% Enjoy free or low-cost open-source tools for learning, prototyping, hobby projects, and skill development (compilers, text editors, databases, web frameworks). Pay for proprietary tools when productivity gains justify cost (IDE licenses like JetBrains, specialized libraries, professional plugins, cloud platform features). Can arbitrage between tools: use open-source bases for core work and proprietary extensions for specialized tasks. Exit options include learning alternative tools, contributing to open projects to gain feature parity, or waiting for open-source equivalents to mature.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, individual_developers, beneficiary,
    moderate, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__utilitarian_hybrid_reading, individual_developers, payer).

% Argue that software freedom is a fundamental ethical requirement and that proprietary software violates user autonomy and digital rights. See the right to inspect, modify, and redistribute code as non-negotiable. Would contest the utilitarian reading's acceptance of specialized-tool proprietary licensing as a legitimate context—from their perspective, welfare optimization cannot justify freedom violation. Excluded from this reading's framing because the reading treats freedom as instrumental to welfare outcomes, not as a foundational right, and permits context-dependent licensing choices that freedom imperatives would forbid entirely.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, freedom_imperative_advocates, excluded,
    organized, generational, identity_locked, global).

% Argue that software creators have natural or earned intellectual property rights and should be able to license restrictively without external welfare justification. See licensing as a creator prerogative, not a social optimization problem. Would contest the utilitarian reading's requirement that licensing choices be justified by aggregate welfare outcomes—from their perspective, creator choice is foundational and welfare claims cannot override property rights. Excluded from this reading's framing because the reading subordinates property claims to welfare optimization and requires justification for all licensing choices against measured welfare outcomes.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, property_rights_advocates, excluded,
    organized, generational, identity_locked, global).

% Empirically investigate which licensing model (open or proprietary) produces superior software quality, faster development cycles, better security outcomes, more sustainable maintenance, and reduced technical debt. Provide evidence-based findings about methodology effectiveness, contributor recruitment, code-review discipline, and long-term project health. Their research informs context-dependent licensing choices and welfare calculations but does not adjudicate the utilitarian reading's normative framework—they measure how, not whether welfare should be the warrant.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, pragmatic_development_advocates, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__utilitarian_hybrid_reading, diffuse).
narrative_ontology:fixing_cost_class(software_source_status__utilitarian_hybrid_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes licensing regimes (open-source under copyleft or permissive terms; proprietary under restricted-access licenses; hybrid models combining both) that allocate source-code access, modification rights, and redistribution permissions based on aggregate welfare predictions in context-specific software domains. Solves the coordination problem: software production requires incentives for contribution and sustainability; licensing is the mechanism that balances contributor rewards, user access, and long-term maintenance.
% TRANSFER_FUNCTION: Moves developer time, creative effort, licensing revenue, and intellectual control across parties according to which licensing model the reading predicts will maximize aggregate welfare in a given software domain. Under open-source: volunteer contributors donate time; commercial vendors pay support and infrastructure costs; users gain free or low-cost access and modification rights. Under proprietary: specialized developers capture cost recovery and pricing power; commercial licensees pay; users bear licensing costs or accept access restrictions. Under hybrid: components optimize under different models simultaneously.
% ABSENT_VOICES: Two excluded communities: (1) freedom-imperative advocates would argue that any proprietary licensing is unjust and that welfare optimization cannot justify freedom violation—they are not in the conversation because the reading treats freedom as instrumental, not foundational; (2) property-rights advocates would argue that creator rights should not be subordinated to welfare claims—they are not in the conversation because the reading makes welfare the paramount warrant. Developing-world technologists dependent on cost-free software are included structurally (end users of critical infrastructure) but may have limited voice in licensing governance institutions dominated by wealthy-world actors with proprietary interests.
% DISAPPEARANCE_RATIONALE: If the utilitarian-hybrid-reading constraint disappeared, software ecosystems would face a coherence crisis: some domains would shift toward pure open-source mandate driven by freedom advocates (infrastructure, core libraries); others would shift toward pure proprietary control driven by commercial incentives (specialized tools, cloud platforms); still others would oscillate between readings based on political pressure and market power rather than systematic welfare analysis. The constraint itself does not control outcomes—developers and organizations will license software regardless—but its disappearance removes the warrant for context-dependent optimization and forces governance toward categorical rules. This is contested because: freedom advocates would celebrate the elimination of proprietary licensing justifications; property-rights advocates would celebrate the removal of welfare-based constraints on creator choice; pragmatists would lament the loss of evidence-based reasoning and return to ideological struggle.
% FOUNDING_PROBLEM: Early software production faced a deep coordination failure: closed-source models created vendor lock-in, reduced auditability, and prevented community improvement, but open-source models created sustainability crises where developers could not capture the value they created and specialized tools suffered chronic under-maintenance. Neither categorical imperative ("all open" or "all proprietary") solved both problems. The utilitarian-hybrid reading was built to reject the binary and optimize licensing per domain context, recognizing that infrastructure favors open licensing (security benefits outweigh proprietary revenue); specialized tools favor proprietary licensing (cost recovery justifies access restriction); and hybrid models often optimize better than pure categories.
% FOUNDING_PROBLEM_CORROBORATION: Academic research in software economics (Lerner & Tirole 2005, Shah 2006, Muniz & Ferreira 2010) confirms that open-source production is cost-effective for infrastructure and large-market projects but struggles with small-market specialization; proprietary licensing sustains niche tools that volunteers cannot support. Infrastructure communities (Linux Foundation, Apache Foundation, Cloud Native Computing Foundation) attest that open licensing sustained by corporate sponsorship and user dependencies outperforms proprietary models for core infrastructure. Empirical data on GitHub shows that open-source projects in infrastructure domains (databases, web servers, container orchestration) have longer lifespans and more active contributors than proprietary equivalents; specialized scientific tools show the reverse (closed-source research instruments with commercial support have more stable development than open-source academic code). Commercial software companies document that proprietary licensing (combined with open-source dependencies) sustains their business models without requiring closed-source core infrastructure. No single corroborating source bridges all domains; the reading's warrant rests on distributed empirical evidence across sectors and a growing consensus that licensing should be selected by outcome, not ideology.
narrative_ontology:disappearance_verdict(software_source_status__utilitarian_hybrid_reading, contested).
narrative_ontology:founding_problem_status(software_source_status__utilitarian_hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__utilitarian_hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.38 at interval end) because the reading permits legitimate proprietary licensing in specialized contexts, so some developer time and pricing power transfer to closed vendors—but this is welfare-justified, not pure extraction, and the open-source gains offset it in aggregate. Suppression is low (0.22) because enforcement comes from weak incentive alignment and norms, not coercion: there is no mechanism forcing anyone to open-source or proprietary; licensing choice is negotiated and alternatives exist in most domains. Theater is very low (0.18) because the reading makes no performative claim beyond what it does functionally—it analyzes welfare and permits mixed models, without theatrical maintenance or cover-story operations. Accessibility collapse is low (0.35) because developers retain real alternatives: for infrastructure, switching to rival open-source projects is feasible; for specialized tools, building proprietary competitors is possible (smaller market, higher entry cost, but not impossible); for hybrids, dual-licensing and open-core models are live options. Resistance is high (0.71) because substantial, organized opposition exists: absolutist freedom advocates resist any proprietary licensing; absolutist property-rights advocates resist open-licensing mandates; both read the utilitarian framing as unacceptably subordinating their foundational claims. Measurement series show modest trajectory: extractiveness and suppression rise slightly as proprietary markets mature and consolidation pressures increase (SaaS licensing, cloud vendor lock-in), but stabilize because open-source counterpressure and hybrid alternatives prevent runaway. Theater ratio stays low because the reading remains committed to welfare analysis rather than shifting to cover-story operation (unlike a Piton, where performance would dominate function).
 *
 * PERSPECTIVAL GAP:
 *   From the open-source ecosystem seat, the constraint is experienced as a rope: coordination function (permits volunteer contribution and interoperation) with minimal extraction (no forced licensing revenue transfer, though some proprietary vendors extract rents from adjacent markets). From the specialized-tool developer seat, the constraint is a rope: coordination function (permits cost recovery for small-market tools) with equitable participation (developers capture legitimate pricing power). From the freedom-advocate excluded seat, it would read as a snare: extraction (permits proprietary licensing) sustained by subordinating freedom to welfare optimization. From the property-rights-advocate excluded seat, it would read as a snare: extraction (open-licensing incentives) sustained by welfare claims subordinating property rights. The engine computes these divergences from power levels and exit options: open-source communities (organized power, mobile exit) experience low directionality; specialized developers (moderate power, constrained exit but multiple licensing paths) experience symmetric; freedom advocates (organized power, identity-locked opposition to proprietary) would compute high target directionality if included, but are excluded from this reading's framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Open-source ecosystem: beneficiary role, organized power, mobile exit → low d (benefits from licensing choice, can fork or establish new regimes). End-user infrastructure: beneficiary role, powerful institutional actors, mobile exit through architectural switching → low d (security gains and vendor-independence outweigh any licensing cost). Specialized developers: beneficiary role, moderate power, constrained exit (small markets limit alternatives) → near-symmetric d (welfare-justified pricing partially offsets constraint on exit). Commercial vendors: dual role (pay proprietary licenses, benefit from open components), powerful, mobile (can choose stacks, negotiate licensing, contribute back) → near-symmetric d (mixed costs and benefits balance). Individual developers: dual role (free tools, pay for premium), moderate power, arbitrage exit (can switch tools, learn alternatives) → low-to-moderate d (access gains outweigh licensing costs). Freedom advocates: excluded, organized power, identity-locked opposition → would compute high target d if included (constraint violates foundational commitment), but not in this reading. Property-rights advocates: excluded, organized power, identity-locked opposition → would compute high target d if included (constraint violates property principle), but not in this reading. No directionality overrides are needed—the structural data (roles, power, exit) generate the appropriate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy by maintaining genuine coordination function: it solves a real problem (how to allocate licensing under uncertainty about welfare outcomes) without pretending the problem is solved. The founding problem (binary choice between vendor lock-in and sustainability crisis) remains live and contested, which the reading acknowledges; the constraint does not assert the problem is solved, only that context-dependent analysis improves on categorical rules. No sunset clause is needed because the constraint is structurally permanent—software licensing decisions will always need to be made, and welfare-based reasoning remains valid even if specific instantiations change. The reading does not degrade to performance: it continues to perform analysis and permit choice, not maintain theatrical compliance with obsolete rules.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_measurement_problem,
    'How should aggregate welfare be measured and compared across licensing regimes? What counts as benefit (source-code auditability, security, innovation speed, developer compensation, user cost, vendor independence) and how are tradeoffs weighed?',
    'Establish a shared empirical methodology for welfare accounting across domains: systematic comparison of outcomes (security metrics, development pace, cost, sustainability) between open and proprietary codebases in same domain; economic analysis of externalities (security benefits, monopoly rents, innovation spillovers); stakeholder welfare surveys.',
    'Different welfare metrics produce different licensing recommendations: security-focused metrics favor open infrastructure; economic-efficiency metrics may favor proprietary specialization; freedom-inclusive metrics shift the tradeoff. The reading''s permissiveness across domains depends on accepting diverse welfare measures per context.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(welfare_measurement_problem, empirical, 'Welfare measurement under-determination across software domains.').

omega_variable(
    context_boundary_ambiguity,
    'What constitutes a ''context'' for licensing-choice purposes? Are domains defined by technical function (infrastructure vs. specialized tools), market structure (small vs. large market), developer capacity (volunteer vs. commercial), or user characteristics (technical vs. end-user)?',
    'Establish boundary criteria from empirical analysis: create a typology of software categories by welfare outcomes and identify the features that predict which licensing model optimizes in each category; test whether boundaries shift as markets mature or technology evolves.',
    'Boundary shifts change licensing recommendations: if a domain treated as ''specialized proprietary'' becomes critical infrastructure (e.g., email protocols, web servers), the reading would recommend re-licensing to open-source. Fuzzy boundaries permit advocacy: actors claiming a domain is ''specialized'' when it becomes ''infrastructure'' to resist licensing change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(context_boundary_ambiguity, empirical, 'Definition of software domain contexts and their welfare implications.').

omega_variable(
    reading_kernel_coexistence,
    'Can the utilitarian reading and the freedom-imperative reading genuinely coexist as stable equilibria, or does the utilitarian reading inevitably erode the freedom reading by permitting proprietary cases that freedom advocates see as victories for exploitation?',
    'Historical case study: track how freedom-advocate communities respond to utilitarian licensing choices over 20+ years; measure whether acceptable proprietary cases (specialized, low-revenue tools) become templates for unacceptable proprietary extraction (platform lock-in, SaaS licensing) as markets mature.',
    'If utilitarian reasoning consistently serves as a ratchet permitting proprietarization that freedom advocates later contest, the readings do not coexist stably—the utilitarian reading would be shown to undermine the freedom reading in practice. If proprietary cases remain bounded to genuinely small-market contexts, coexistence holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_coexistence, empirical, 'Stable coexistence of utilitarian and freedom readings under market pressure.').

omega_variable(
    development_model_causality,
    'Does open-source licensing CAUSE better software quality and faster development (pragmatic reading''s warrant), or does it correlate with domains where volunteers self-select and commercial incentives align with quality?',
    'Natural experiment: measure development outcomes when licensing changes (fork studies, license-conversion case studies) controlling for domain selection; isolate causal contribution of licensing from domain effects.',
    'If licensing is causally efficacious, the pragmatic reading holds independent weight and welfare analysis must account for methodology effects. If licensing is correlational, the pragmatic reading is descriptive of domain selection, not a foundational claim, and welfare analysis incorporates domain differences directly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(development_model_causality, empirical, 'Causal vs. correlational relationship between open licensing and development quality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__utilitarian_hybrid_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__utilitarian_hybrid_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(soft_tr_t5, software_source_status__utilitarian_hybrid_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(soft_tr_t10, software_source_status__utilitarian_hybrid_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(soft_tr_t15, software_source_status__utilitarian_hybrid_reading, theater_ratio, 15, 0.17).
narrative_ontology:measurement(soft_tr_t20, software_source_status__utilitarian_hybrid_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(soft_tr_t25, software_source_status__utilitarian_hybrid_reading, theater_ratio, 25, 0.18).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(soft_be_t5, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 5, 0.31).
narrative_ontology:measurement(soft_be_t10, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(soft_be_t15, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 15, 0.37).
narrative_ontology:measurement(soft_be_t20, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(soft_be_t25, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 25, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(soft_su_t5, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 5, 0.17).
narrative_ontology:measurement(soft_su_t10, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 10, 0.19).
narrative_ontology:measurement(soft_su_t15, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 15, 0.21).
narrative_ontology:measurement(soft_su_t20, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement(soft_su_t25, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 25, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__utilitarian_hybrid_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_source_status__utilitarian_hybrid_reading, 0.18).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__property_rights_reading).

% DUAL FORMULATION NOTE:
% The software_source_status kernel has been decomposed into four constraint stories, each instantiating a different reading with different structural beneficiary/victim sets and extraction profiles. The utilitarian-hybrid reading permits context-dependent licensing (open for infrastructure, proprietary for specialized tools, hybrid for complex systems) and treats welfare optimization as the warrant for licensing choice. It coexists with three sibling readings: freedom-imperative (which would read proprietary licensing as injustice regardless of context), pragmatic-development (which treats open-source as methodologically superior), and property-rights (which treats creator choice as foundational). Each reading is a distinct constraint story with its own ε, beneficiary structure, and six-questions interview. Links via affects_constraints denote the family relationship; the constraint divergence in classification and metrics is the measurement the corpus takes—whether different readings of the same kernel produce measurably different constraint structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
