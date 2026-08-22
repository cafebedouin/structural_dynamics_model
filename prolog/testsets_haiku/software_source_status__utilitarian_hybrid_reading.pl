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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Software Licensing as Welfare-Optimizing Hybrid Regime
 *   domain: intellectual_property/software_engineering/political_economy
 *
 * SUMMARY:
 *   The utilitarian hybrid reading treats software licensing as a
 *   welfare-optimization problem where different models (open-source and
 *   proprietary) serve different contexts optimally. Infrastructure and
 *   public-good software is welfare-maximizing under open-source licensing
 *   because distributed maintenance, community auditing, and transparency
 *   produce security and quality benefits. Specialized domain tools justify
 *   proprietary licensing because the investment costs and appropriability
 *   incentives necessary for development outweigh the closure costs. The
 *   reading rejects both the freedom-imperative claim (that freedom is a
 *   foundational right) and the property-rights claim (that proprietary
 *   licensing is universally legitimate), instead locating legitimacy in
 *   context-specific welfare analysis. This reading is one of four
 *   interpretations of the contested kernel 'software_source_status'; sibling
 *   readings emphasize ethical freedom requirements, pragmatic development
 *   methodology, or natural property rights. The constraint operates as a
 *   tangled rope: it coordinates the coexistence of both licensing models
 *   while extracting from developers locked into proprietary ecosystems.
 *
 * KEY AGENTS:
 *   - Infrastructure maintainers (open-source, globally distributed, high mobility) — benefit from permission to choose open licensing
 *   - Specialized domain developers (proprietary, moderate power, constrained exit) — benefit from permission to choose closed licensing for high-investment tools
 *   - Developers locked to proprietary ecosystems (identity-locked, moderate power) — bear the extraction cost of path dependency
 *   - Proprietary software companies (institutional, agenda-setting) — enforce the default assumption that software is proprietary
 *   - Free software advocates (excluded, organized) — dispute the welfare-optimization framing itself
 *   - Regulatory bodies (observer, institutional) — can mandate licensing terms in specific contexts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__utilitarian_hybrid_reading, 0.42).
domain_priors:suppression_score(software_source_status__utilitarian_hybrid_reading, 0.31).
domain_priors:theater_ratio(software_source_status__utilitarian_hybrid_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__utilitarian_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(software_source_status__utilitarian_hybrid_reading, "Software Licensing as Welfare-Optimizing Hybrid Regime").
narrative_ontology:topic_domain(software_source_status__utilitarian_hybrid_reading, "intellectual_property/software_engineering/political_economy").

domain_priors:requires_active_enforcement(software_source_status__utilitarian_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__utilitarian_hybrid_reading, '25e4b0e7-b61a-4ca4-8f36-c736e8015cef').
narrative_ontology:cs_kernel_codification('25e4b0e7-b61a-4ca4-8f36-c736e8015cef', distributed).
narrative_ontology:cs_authority_grounding('25e4b0e7-b61a-4ca4-8f36-c736e8015cef', distributed).
narrative_ontology:cs_reading_relation('25e4b0e7-b61a-4ca4-8f36-c736e8015cef', software_source_status__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('25e4b0e7-b61a-4ca4-8f36-c736e8015cef', software_source_status__pragmatic_development_reading, influences).
narrative_ontology:cs_reading_relation('25e4b0e7-b61a-4ca4-8f36-c736e8015cef', software_source_status__property_rights_reading, coexists_with).
narrative_ontology:cs_axiom('25e4b0e7-b61a-4ca4-8f36-c736e8015cef', foundational, context_dependent_welfare_maximization_is_legitimate_ground).
narrative_ontology:cs_axiom_status(context_dependent_welfare_maximization_is_legitimate_ground, holdable).
narrative_ontology:cs_axiom_grounding('25e4b0e7-b61a-4ca4-8f36-c736e8015cef', context_dependent_welfare_maximization_is_legitimate_ground, instrumental).
narrative_ontology:cs_axiom('25e4b0e7-b61a-4ca4-8f36-c736e8015cef', foundational, both_open_and_proprietary_licensing_serve_distinct_welfare_contexts).
narrative_ontology:cs_axiom_status(both_open_and_proprietary_licensing_serve_distinct_welfare_contexts, holdable).
narrative_ontology:cs_axiom_grounding('25e4b0e7-b61a-4ca4-8f36-c736e8015cef', both_open_and_proprietary_licensing_serve_distinct_welfare_contexts, empirically_contingent).
narrative_ontology:cs_reference_frame('25e4b0e7-b61a-4ca4-8f36-c736e8015cef', welfare_optimizing_hybrid_licensing_regime).
narrative_ontology:cs_drift_state('25e4b0e7-b61a-4ca4-8f36-c736e8015cef', contemporary_regulatory_pressure_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('25e4b0e7-b61a-4ca4-8f36-c736e8015cef', '').
narrative_ontology:cs_kernel_id(software_source_status__utilitarian_hybrid_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, infrastructure_maintainers).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, specialized_domain_developers).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, users_of_stable_proprietary_tools).
narrative_ontology:constraint_victim(software_source_status__utilitarian_hybrid_reading, developers_locked_to_proprietary_ecosystems).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, open_source_pragmatists).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, proprietary_software_companies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizations and individuals maintaining foundational software systems (compilers, operating systems, package managers, cryptographic libraries, networking stacks) that serve as public goods. Under the hybrid reading, they benefit from permission to choose open-source licensing for infrastructure because it enables community contribution, security auditing, and collective maintenance. The reading accommodates their choice and treats open licensing as welfare-maximizing for infrastructure.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, infrastructure_maintainers, beneficiary,
    organized, generational, mobile, global).

% Teams building proprietary tools for specialized domains (medical imaging, industrial control, financial trading, aerospace simulation) where the investment in domain-specific knowledge, regulatory compliance, and safety certification is high and appropriability (the ability to capture returns) is the incentive structure that justifies the engineering effort. The hybrid reading permits proprietary licensing for these tools on the grounds that the welfare gain from having such specialized tools available exceeds the welfare loss from closure.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, specialized_domain_developers, beneficiary,
    moderate, biographical, constrained, global).

% Organizations and individuals who depend on proprietary software tools for stable, predictable, long-term operation: enterprise database systems, CAD software, specialized analytics platforms. They benefit from the constraint because proprietary licensing creates the incentive structures that fund the development and maintenance of these tools, and the closed nature of the vendor relationship provides contractual stability and liability clarity.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, users_of_stable_proprietary_tools, beneficiary,
    moderate, biographical, constrained, global).

% Software engineers whose careers have been built on proprietary platform ecosystems (enterprise database systems, game engines, development platforms) where the tools are closed, the learning investments are specific to the platform, and the career paths depend on vendor certification and ecosystem-specific expertise. They pay through reduced exit options and vendor dependency: their skills do not transfer as readily to open alternatives, retraining costs are high, and the proprietary vendor controls the evolution of the platform they depend on. The hybrid reading treats this as a regrettable but justified cost of the overall welfare optimization.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, developers_locked_to_proprietary_ecosystems, payer,
    moderate, biographical, identity_locked, global).

% Communities and organizations holding that software freedom is a fundamental right or ethical requirement (GNU/FSF, Free Software Foundation, ethical FOSS movements) who are structurally excluded from this reading's deliberations. They would object that the hybrid reading instrumentalizes freedom and treats ethical constraints as trade-offs subject to welfare calculation, rather than as foundational moral claims. Their voices are absent from the welfare-maximizing framework.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, free_software_advocates, excluded,
    organized, generational, analytical, global).

% Developers and organizations who view open-source licensing as a superior development methodology (Apache, Mozilla, Linux communities) and who recognize that code review transparency, distributed contribution, and community auditing produce higher-quality, more secure software. The hybrid reading accommodates their position by identifying contexts (infrastructure, security-critical systems, widely-used libraries) where open-source licensing is welfare-maximizing, and by treating their methodological advantages as legitimate reasons to choose open licensing without requiring ethical justification.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, open_source_pragmatists, beneficiary,
    organized, generational, mobile, global).

% Organizations whose business model depends on proprietary software licensing and who enforce the constraint through licensing terms, technical protection measures, and legal frameworks protecting intellectual property. They set the default assumption that software is proprietary unless explicitly licensed otherwise. The hybrid reading accommodates their position by treating proprietary licensing as legitimate in certain contexts (specialized tools, high-investment domains) while rejecting their claim that proprietary licensing is universally welfare-maximizing.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, proprietary_software_companies, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__utilitarian_hybrid_reading, proprietary_software_companies, beneficiary).

% Government agencies, competition authorities, and standard-setting organizations that evaluate whether software licensing restrictions create anti-competitive effects, undue barriers to entry, or public harms. They observe the constraint and can impose regulations mandating interoperability, source disclosure, or licensing terms in specific contexts (e.g., software used in critical infrastructure, medical devices, or government procurement).
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, regulatory_bodies_and_competition_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__utilitarian_hybrid_reading, proprietary_software_companies).
narrative_ontology:fixing_cost_class(software_source_status__utilitarian_hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The hybrid licensing regime solves the software knowledge production problem: it creates differential incentive structures matching the type of software being produced. Infrastructure and public-good software benefits from open transparency and distributed maintenance (open-source licensing); specialized, high-investment, domain-specific tools benefit from appropriability and vendor continuity (proprietary licensing). The constraint enables coordination by permitting context-dependent licensing choices rather than mandating a universal model.
% TRANSFER_FUNCTION: The arrangement transfers control over source code, modification rights, and distribution authority from developers to end-users (in open-source contexts) or from developers and end-users to vendors (in proprietary contexts). It also transfers returns on investment: proprietary licensing transfers market rents to the original developer; open-source licensing distributes maintenance burden and innovation benefits across the community.
% ABSENT_VOICES: Free software advocates and ethical FOSS movements are structurally excluded: they hold that software freedom is a foundational moral requirement, not a context-dependent welfare trade-off. They would argue that framing freedom as an optimization variable subject to welfare calculation instrumentalizes and compromises the ethical claim. Regulatory bodies and competition authorities are partially absent: their voices on anti-competitive effects of software licensing are present in some jurisdictions but not systematized into the welfare-maximizing framework itself.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, the software ecosystem would not disappear, but its structure would shift substantially. Open-source licensing would likely dominate infrastructure (because the coordination benefits are greatest there and enforcement would be unnecessary). Proprietary licensing would persist in specialized domains but would face pressure from regulatory mandates to open source or provide source escrow in critical infrastructure and public-sector contracts. The overall ecosystem would become more explicitly hybrid but with different jurisdictional boundaries.
% FOUNDING_PROBLEM: Early software development faced a knowledge production problem: should code be treated as a proprietary artifact (protected intellectual property) or as a public good subject to shared maintenance and collaborative improvement? Different contexts had different efficiency answers. The hybrid constraint was built to acknowledge that both licensing models have legitimate roles depending on the type of software, the incentive structure required for development, the safety and security implications of closure, and the infrastructure requirements for public goods.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by open-source pragmatists (Linux, Apache, Mozilla communities) who argue that code review transparency produces higher quality and security; by specialized domain developers and proprietary vendors who argue that high-investment, domain-specific tools require appropriability; and by infrastructure maintainers who demonstrate that distributed community maintenance is effective for foundational software. Competition authorities and regulatory bodies attest that both models create distinct trade-offs between innovation incentives, security, interoperability, and access. Free software advocates and ethical FOSS movements dispute that welfare calculation should govern the choice at all, treating the founding problem as misframed — that the problem was never efficiency optimization but rights recognition.
narrative_ontology:disappearance_verdict(software_source_status__utilitarian_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__utilitarian_hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__utilitarian_hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_source_status__utilitarian_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__utilitarian_hybrid_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.42) because the constraint is not purely extractive: it permits both licensing models and recognizes legitimate roles for each. However, it extracts from developers locked into proprietary ecosystems because their skills are ecosystem-specific and retraining costs are high — they bear the cost of path dependency. Suppression is lower than in purely extractive constraints (0.31) because the regime does not universally foreclose alternatives; developers can choose projects in open-source communities if they wish, though the opportunity cost is high if their prior training is proprietary-specific. Theater ratio is low-moderate (0.22) because the welfare-optimization justification is substantively applied: vendors do make genuine investments in specialized tools, and open-source projects do demonstrate quality advantages. The measurement series are relatively flat around the baseline values, indicating a stable constraint structure over the interval — the hybrid regime has reached equilibrium without substantial drift in either direction. Accessibility collapse (0.48) reflects the reality that alternatives do exist (open-source infrastructure is available, proprietary tools are available) but exit is costly depending on the developer's prior investments. Resistance (0.58) is moderate-to-high because the constraint meets real opposition from free software advocates, competition authorities investigating anti-competitive effects, and developers frustrated with lock-in.
 *
 * PERSPECTIVAL GAP:
 *   The proprietary software companies (agenda-setter) and users of stable proprietary tools perceive the constraint as legitimate coordination that produces necessary incentives. Specialized domain developers perceive it as legitimate for their work but resent that the same justification is applied universally. Developers locked to proprietary ecosystems perceive the constraint as exploitation masked by welfare rhetoric — their exit costs are real but treated as acceptable trade-offs in the reading's calculation. Free software advocates perceive the constraint as fundamentally misframed: they reject the idea that freedom should be instrumental to welfare optimization rather than foundational. Infrastructure maintainers perceive the constraint as accommodating their choice to go open-source, so they experience it as enabling rather than extractive. The engine should compute substantially different effective extraction values across these seats: from the infrastructure maintainers' perspective, the constraint enables their chosen licensing model (low d, low χ); from the developers locked to proprietary ecosystems' perspective, the same constraint suppresses their exit options (high d, high χ).
 *
 * DIRECTIONALITY LOGIC:
 *   Infrastructure maintainers are near-beneficiary (d ≈ 0.2) because the constraint permits and legitimizes their choice of open-source licensing. Specialized domain developers are near-neutral (d ≈ 0.45) because they benefit from the permission to charge appropriably but face pressure from regulatory mandates and competitive open-source alternatives. Users of stable proprietary tools are beneficiaries (d ≈ 0.3) because the constraint protects the vendor incentives that fund their tools. Developers locked to proprietary ecosystems are near-target (d ≈ 0.75) because the constraint structures their career path toward the proprietary platform, making exit costly. Proprietary software companies are full beneficiary-agenda-setters (d ≈ 0.05) because they set the default rule that software is proprietary unless explicitly licensed otherwise. Free software advocates have high d (≈ 0.8) because the constraint explicitly rejects their foundational claim and instrumentalizes freedom as a trade-off variable. These directionality assignments follow from the stakeholders' declared beneficiary/victim status and exit options: beneficiaries have low d; victims/payers have high d; those with mobile exit options have lower d (less trapped); those with identity-locked exit have higher d (more trapped).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as tangled_rope because it combines genuine coordination (permitting both licensing models, avoiding universal mandates) with asymmetric extraction (developers locked to proprietary ecosystems bear costs their beneficiaries do not). The founding problem was knowledge production under different incentive structures; the founding problem status is contested — infrastructure maintainers and open-source pragmatists argue the problem is substantially solved (open-source produces quality), while proprietary vendors argue specialized domains still require proprietary incentives. The disappearance verdict is world_rearranges: the ecosystem would reorganize into different jurisdictional boundaries. The mismatch between 'contested' status and 'world_rearranges' verdict suggests the constraint has accomplished something real (coordinating coexistence) but at a cost (locking developers into proprietary paths). The theater_ratio is low (0.22) because the welfare-optimization justification is substantively applied — vendors do invest in specialized tools — rather than performed. However, the constraint's ability to persist despite organized opposition (free software advocates, some regulatory jurisdictions) without overwhelming enforcement machinery (suppression 0.31) indicates that the utilitarian frame itself is carrying legitimacy work; the constraint persists because many parties find the welfare-optimization argument persuasive, not because enforcement is overwhelming. This is the opposite of piton dynamics: it is tangled rope held together partly by the plausibility of its coordination narrative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    context_dependent_welfare_measurement,
    'How should context-dependent welfare be measured and compared across different types of software? What metrics determine whether open-source or proprietary licensing is welfare-maximizing in a specific domain?',
    'Empirical research comparing development costs, security incident rates, maintenance burden, innovation rates, and user access across open-source and proprietary software in matched domains (e.g., web servers, databases, development tools). Surveys of developers on actual exit costs and retraining investment requirements.',
    'If welfare measurement can be systematized, the hybrid reading becomes a falsifiable constraint: domains where empirical evidence favors one model can be identified and licensing recommendations can be justified. If welfare is fundamentally incommensurable across domains or contestable among stakeholders, the reading remains under-determined and the constraint persists without clear decision rules.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(context_dependent_welfare_measurement, empirical, 'Whether welfare-maximizing licensing choices can be empirically determined or remain contestable.').

omega_variable(
    identity_lock_internalization_vs_structural,
    'For developers locked to proprietary ecosystems, is the suppression and high exit cost structural (external economic barriers) or internalized (the developer''s identity and self-concept are fused with the proprietary platform)?',
    'Post-exit trajectory studies: if developers leaving proprietary ecosystems quickly adapt to open-source contexts and report lower suppression post-exit, suppression is primarily structural and temporary. If developers carry high suppression with them (identity-locked status persists after leaving the ecosystem), suppression is partially internalized.',
    'If internalized, the constraint''s effective suppression of developers is higher than the structural measure (0.31) suggests — the cost persists even after exit. If structural, the suppression is high while in the ecosystem but diminishes with exit. This affects whether developers can realistically be said to have ''exit options'' or remain identity-locked even when structural barriers are removed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_internalization_vs_structural, empirical, 'Whether developer lock-in is primarily structural or internalized.').

omega_variable(
    free_software_axiom_status,
    'Is the free-software axiom (software freedom as a foundational ethical requirement) actually ''overridden'' within its own tradition, or does the utilitarian-hybrid reading represent a different tradition making different commitments?',
    'Close reading of free software foundation texts, GPL evolution, and community debates: is the freedom ethic treated as subordinate to welfare optimization, or is the disagreement between two incommensurable value frameworks that do not share an authority structure?',
    'If the freedom axiom is overridden, the utilitarian reading has legitimately superseded it within the broader software licensing discourse. If it is incommensurable rather than overridden, the two readings coexist but do not converge — the constraint cannot adjudicate between them, and both persist as live positions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(free_software_axiom_status, conceptual, 'Whether the free-software ethical commitment and utilitarian welfare optimization are measured against the same authority or remain incommensurable frameworks.').

omega_variable(
    regulatory_mandate_effects,
    'What is the actual effect of regulatory mandates for open-source or source disclosure (e.g., in critical infrastructure, medical devices, government procurement) on the welfare-optimization calculation? Do regulatory mandates improve welfare or create unintended costs?',
    'Case studies of jurisdictions implementing open-source mandates or source disclosure requirements: measure innovation rates, security incident rates, maintenance burden, developer participation, and ecosystem health pre- and post-mandate.',
    'If mandates improve welfare outcomes (more security, more innovation, healthier ecosystems), they validate the hybrid reading''s openness to regulatory intervention in specific contexts. If mandates create costs exceeding benefits, they suggest the utilitarian case for proprietary licensing is understated and the hybrid reading underestimates domain-specific factors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_mandate_effects, empirical, 'Whether regulatory open-source mandates improve welfare or create unintended costs.').

omega_variable(
    kernel_reading_incommensurability,
    'Is the disagreement among the four readings (freedom-imperative, pragmatic-development, property-rights, and utilitarian-hybrid) a dispute that can be resolved by empirical evidence and shared values, or is it a fundamental incommensurability among different ethical and epistemological frameworks?',
    'Philosophy of technology analysis: do the readings share a common authority structure and measure themselves against the same metrics (in which case the disagreement is empirically tractable), or do they appeal to different foundational commitments (ethics, methodology, property rights, welfare) that are not mutually translatable?',
    'If the disagreement is empirically tractable, future evidence about licensing effects can shift the dominant reading. If the disagreement is incommensurable, all four readings persist as live positions held by different epistemic communities, and the constraint is inherently contested without prospect of resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the kernel dispute is empirically resolvable or conceptually incommensurable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__utilitarian_hybrid_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__utilitarian_hybrid_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(soft_tr_t5, software_source_status__utilitarian_hybrid_reading, theater_ratio, 5, 0.19).
narrative_ontology:measurement(soft_tr_t10, software_source_status__utilitarian_hybrid_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(soft_tr_t15, software_source_status__utilitarian_hybrid_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(soft_tr_t20, software_source_status__utilitarian_hybrid_reading, theater_ratio, 20, 0.23).
narrative_ontology:measurement(soft_tr_t25, software_source_status__utilitarian_hybrid_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement(soft_tr_t30, software_source_status__utilitarian_hybrid_reading, theater_ratio, 30, 0.21).
narrative_ontology:measurement(soft_tr_t35, software_source_status__utilitarian_hybrid_reading, theater_ratio, 35, 0.22).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(soft_be_t5, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(soft_be_t10, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(soft_be_t15, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(soft_be_t20, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 20, 0.43).
narrative_ontology:measurement(soft_be_t25, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 25, 0.42).
narrative_ontology:measurement(soft_be_t30, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 30, 0.41).
narrative_ontology:measurement(soft_be_t35, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 35, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(soft_su_t5, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 5, 0.29).
narrative_ontology:measurement(soft_su_t10, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(soft_su_t15, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 15, 0.32).
narrative_ontology:measurement(soft_su_t20, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 20, 0.31).
narrative_ontology:measurement(soft_su_t25, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 25, 0.3).
narrative_ontology:measurement(soft_su_t30, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 30, 0.31).
narrative_ontology:measurement(soft_su_t35, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 35, 0.31).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__utilitarian_hybrid_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_source_status__utilitarian_hybrid_reading, 0.18).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__property_rights_reading).

% DUAL FORMULATION NOTE:
% The software_source_status kernel is instantiated by four distinct constraint stories, one per reading. This story (utilitarian-hybrid) locates legitimacy in context-dependent welfare optimization. Sibling readings (freedom-imperative, pragmatic-development, property-rights) locate legitimacy in ethical freedom, methodological superiority, or natural property rights respectively. The four stories share the same referent (the question of software licensing) but author different ε values because the readings assess the standing arrangement through different value frameworks. Decomposition is required by ε-invariance: changing how the constraint is justified (ethical vs. pragmatic vs. utilitarian) changes what the same physical arrangement IS, structurally — it is not the same constraint viewed from different angles but genuinely different constraints reading the same kernel. The network relationships model upstream/downstream influence: the utilitarian reading influences (but does not foreclose) the pragmatic reading because welfare optimization can incorporate methodological arguments; the freedom-imperative reading influences the utilitarian reading by forcing welfare calculations to account for freedom losses; the property-rights reading coexists with but does not foreclose the utilitarian reading because they appeal to different authority sources (natural rights vs. aggregate welfare).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(software_source_status__utilitarian_hybrid_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
