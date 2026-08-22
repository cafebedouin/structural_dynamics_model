% ============================================================================
% CONSTRAINT STORY: software_source_status__property_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__property_rights_reading, []).

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
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: software_source_status__property_rights_reading
 *   human_readable: Software Source Code as Proprietary Asset (Property Rights Reading)
 *   domain: intellectual_property/software_engineering/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the property-rights reading of the
 *   contested kernel 'software source status.' The reading holds that source
 *   code is a legitimate form of intellectual property; that creators and
 *   publishers have the right to restrict access, study, modification, and
 *   redistribution; and that licensing restrictions are proper exercises of
 *   ownership rather than coercive control. This reading is one of four live,
 *   structurally distinct positions in contemporary software-governance
 *   discourse. The claim (tangled_rope) and the metrics (moderate-to-high
 *   extractiveness, suppression, and theater) are authored independently: the
 *   constraint genuinely solves a coordination problem (incentivizing
 *   development via property rights) and simultaneously extracts from users
 *   and derivative developers (through access restrictions and licensing
 *   fees). The measurement series document extractiveness accumulating from
 *   0.48 to 0.68 over the interval as licensing restrictions intensify and
 *   expand to new software domains (cloud-based services, embedded systems);
 *   suppression and theater ratio rise as the enforcement machinery tightens
 *   and defensive rhetoric increases.
 *
 * KEY AGENTS:
 *   - Software publishers: institutional agenda-setters who define and enforce the licensing constraints; defend property-rights framing; benefit from pricing power.
 *   - Proprietary vendors: powerful beneficiaries who profit from source-code scarcity and switching costs; have exit options but rarely take them.
 *   - Downstream users: organized payers with constrained exit; depend on software but cannot inspect or modify it; accept lock-in.
 *   - Derivative developers: moderate-power payers with mixed interests; want to build on existing code but cannot without vendor permission.
 *   - Security researchers: organized actors excluded from code inspection; cannot audit for vulnerabilities without vendor permission.
 *   - Open-source communities: organized actors excluded by incompatible licensing frameworks; operate in separate ecosystem.
 *   - Intellectual property authorities: institutional observers with enforcement power; adjudicate rights and licensing disputes.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__property_rights_reading, 0.68).
domain_priors:suppression_score(software_source_status__property_rights_reading, 0.62).
domain_priors:theater_ratio(software_source_status__property_rights_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__property_rights_reading, tangled_rope).
narrative_ontology:human_readable(software_source_status__property_rights_reading, "Software Source Code as Proprietary Asset (Property Rights Reading)").
narrative_ontology:topic_domain(software_source_status__property_rights_reading, "intellectual_property/software_engineering/political_economy").

domain_priors:requires_active_enforcement(software_source_status__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__property_rights_reading, '35d30a6f-c78e-4bd6-8f09-ccfafad96083').
narrative_ontology:cs_kernel_codification('35d30a6f-c78e-4bd6-8f09-ccfafad96083', fixed_text).
narrative_ontology:cs_authority_grounding('35d30a6f-c78e-4bd6-8f09-ccfafad96083', extraction).
narrative_ontology:cs_interpretation_layer_present('35d30a6f-c78e-4bd6-8f09-ccfafad96083').
narrative_ontology:cs_reading_relation('35d30a6f-c78e-4bd6-8f09-ccfafad96083', software_source_status__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('35d30a6f-c78e-4bd6-8f09-ccfafad96083', software_source_status__pragmatic_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('35d30a6f-c78e-4bd6-8f09-ccfafad96083', software_source_status__utilitarian_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('35d30a6f-c78e-4bd6-8f09-ccfafad96083', foundational, source_code_is_intellectual_property).
narrative_ontology:cs_axiom_status(source_code_is_intellectual_property, holdable).
narrative_ontology:cs_axiom_grounding('35d30a6f-c78e-4bd6-8f09-ccfafad96083', source_code_is_intellectual_property, conventional).
narrative_ontology:cs_axiom('35d30a6f-c78e-4bd6-8f09-ccfafad96083', foundational, licensing_restrictions_legitimate_ownership_expression).
narrative_ontology:cs_axiom_status(licensing_restrictions_legitimate_ownership_expression, holdable).
narrative_ontology:cs_axiom_grounding('35d30a6f-c78e-4bd6-8f09-ccfafad96083', licensing_restrictions_legitimate_ownership_expression, deontological).
narrative_ontology:cs_reference_frame('35d30a6f-c78e-4bd6-8f09-ccfafad96083', source_code_copyright_property_framework).
narrative_ontology:cs_drift_state('35d30a6f-c78e-4bd6-8f09-ccfafad96083', contemporary_open_source_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('35d30a6f-c78e-4bd6-8f09-ccfafad96083', '').
narrative_ontology:cs_kernel_id(software_source_status__property_rights_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, software_publishers).
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, proprietary_vendors).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, downstream_users).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, derivative_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, derivative_developers).
narrative_ontology:constraint_vindicates(software_source_status__property_rights_reading, copyright_authority_doctrine).
narrative_ontology:constraint_vindicates(software_source_status__property_rights_reading, market_driven_incentive_model).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces licensing restrictions on source code access and modification. Controls the terms under which users may run, study, modify, or redistribute software. Justifies restrictions as protecting intellectual property rights, ensuring quality control, and maintaining business models. Collects licensing revenue and maintains competitive advantage through source secrecy.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, software_publishers, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefit from licensing restrictions that protect market differentiation, enable price discrimination, and create switching costs for users. Can exit by relicensing under permissive terms, though rarely do so. Their profit models depend on access scarcity and licensing control.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, proprietary_vendors, beneficiary,
    powerful, biographical, mobile, global).

% Cannot inspect, modify, or redistribute software they depend on without violating licensing terms. Pay licensing fees, subscription costs, or accept behavioral lock-in. Their exit options are constrained by network effects, compatibility requirements, and high switching costs. Must trust vendors' claims about security, functionality, and fairness.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, downstream_users, payer,
    organized, biographical, constrained, global).

% Want to build on existing software but face licensing restrictions. Cannot study code to understand how it works or fork/modify for their own use cases. Must either license at vendor terms, pay for proprietary APIs, or rebuild from scratch. Benefit from proprietary vendor innovation but pay through restriction on their own innovation.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, derivative_developers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__property_rights_reading, derivative_developers, beneficiary).

% Are contractually barred from studying source code to audit security or discover vulnerabilities. Must work within vendor disclosure programs with nondisclosure agreements. Cannot publish general principles or fixes without permission. Their exclusion from code inspection is what licensing enforcement maintains.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, security_researchers, excluded,
    organized, biographical, constrained, global).

% Are excluded from using proprietary software under terms compatible with open-source development models (copyleft, permissive licensing). They operate in a separate ecosystem with its own licensing frameworks. Their exclusion is structural: proprietary licensing and open-source licensing are incompatible frameworks.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, open_source_communities, excluded,
    organized, generational, mobile, global).

% Adjudicate copyright and licensing disputes; enforce the legal framework that makes source-code property restrictions enforceable. They take positions in court cases, set interpretation standards, and can alter the constraint through doctrine or statute. They operate from the analytical seat but wield enforcement power.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, intellectual_property_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__property_rights_reading, software_publishers).
narrative_ontology:fixing_cost_class(software_source_status__property_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a market-based incentive mechanism for software development: by granting publishers exclusive rights to distribute and modify code, the constraint creates property-like assets that publishers can sell, license, and profit from. This solves the problem of how to fund software development when copying is cheap and copying is hard to prevent without legal enforcement.
% TRANSFER_FUNCTION: Moves economic rent from users and derivative developers to publishers and proprietary vendors. Users pay licensing fees, subscription costs, or acceptance of lock-in. Derivative developers pay by being unable to modify, extend, or study code. Publishers receive monopoly-like pricing power over their software.
% ABSENT_VOICES: Open-source advocates, security researchers, and derivative developers are structurally excluded from the conversation where the property-rights framing is decided. They would argue that source-code restrictions harm security, innovation, and autonomy, and that alternative incentive models (reputation, sustainability funding, community contribution) could fund development without restriction. They are kept out by the same legal and licensing frameworks the constraint rides on.
% DISAPPEARANCE_RATIONALE: If proprietary software licensing restrictions and their legal enforcement disappeared overnight, the software industry would reorganize around open-source development models, subscription services for hosted code, and patronage/crowdfunding mechanisms. Publishers would lose exclusive control over source code and the pricing power it grants. Development incentives would shift to reputation, community status, and alternate business models (services, customization, support).
% FOUNDING_PROBLEM: Early personal-computing software was difficult and expensive to develop. Without legal protections, software could be copied freely, making it hard to recoup development costs or fund continued improvements. Publishers needed a way to extract economic value from software so that development remained incentivized.
% FOUNDING_PROBLEM_CORROBORATION: Publishers and property-rights advocates attest the founding problem remains live, citing continued high development costs and piracy threats. Open-source advocates and empirical software-development studies from outside the publishing industry attest the problem is substantially solved: open-source projects fund themselves through alternate mechanisms (corporate sponsorship, patronage, community contribution, hosted services) and produce high-quality software without proprietary restrictions. Legislative testimony and independent economic analysis support the shifted-function reading.
narrative_ontology:disappearance_verdict(software_source_status__property_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__property_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__property_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_source_status__property_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__property_rights_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__property_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_source_status__property_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_source_status__property_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.68 at interval end) because licensing restrictions create monopoly-like pricing power and users bear switching costs and lock-in. Suppression is substantial (0.62) because the constraint depends on active enforcement: legal frameworks criminalizing circumvention, technical mechanisms (DRM), licensing terms, and institutional pressure against competitors. Theater is moderate (0.41) and rising: the constraint is justified with property-rights and innovation-incentive narratives, but an increasing share of the constraint's operation is defensive (expanding licensing scope, tightening DRM, pursuing copyright extensions) rather than solving the original coordination problem. The measurement series run on one shared time grid (t0 through t50, every 8 units) to enable temporal analysis. Extractiveness plateaus at 0.68 because market saturation and open-source competition limit further rent expansion; suppression and theater reach plateaus around t40 as the constraint matures and its core function stabilizes.
 *
 * PERSPECTIVAL GAP:
 *   This is a marked seat divergence. From the publisher/vendor seat, the constraint is a legitimate property mechanism solving a real coordination problem: without exclusive rights, development funding dries up. From the user and derivative-developer seats, the same structure operates as coercive access restriction, enabled by legal enforcement and technical lock-in. The constraint genuinely provides coordination (development incentives) and simultaneously extracts (through access denial and pricing power). The engine computes this divergence from the structural data: beneficiary seats compute high coordination gain and negative/low effective extraction; payer seats compute high effective extraction and constrained alternatives. The authored claim (tangled_rope) reflects this mixed structure: coordination + asymmetric extraction + active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Software publishers are structural beneficiaries (d near 0.0): they set the rules, control source code, collect licensing revenue, and have exit options (relicensing). Proprietary vendors benefit from the constraint's operation (d ~0.2) but also compete in a market constrained by open-source alternatives. Downstream users are structural targets (d ~0.85): they pay fees, accept lock-in, and cannot exit without abandoning network effects and compatibility. Derivative developers are mixed-positioned (d ~0.65): they benefit from existing software innovation but pay by being unable to modify or extend code. Security researchers are excluded rather than coordinated (d >0.9): their exclusion from code inspection is what enforcement maintains. Open-source communities have high directionality (d >0.8) as they are structurally incompatible with the proprietary licensing framework. Intellectual property authorities sit at the observer seat (d = analytical).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—funding software development in the face of easy copying—was real. But its status is now contested. Publishers attest it remains live; open-source communities and empirical evidence attest it is substantially solved through alternate mechanisms (corporate sponsorship, hosted services, community contribution, patronage). The constraint persists despite the founding problem's ambiguity because publishers capture the rent; no party is hurt enough to fix it while publishers have every incentive to maintain it. This is not pure preservation of dead function (piton); the coordination function is still valuable. But the extraction component has decoupled from the coordination component and persists through institutional lock-in and legal enforcement rather than participant preference. The theater ratio's rise (from 0.22 to 0.41) suggests increasing performative activity: copyright extensions, DRM expansion, aggressive licensing enforcement, and rhetorical defense of 'intellectual property rights' increasingly dominate over actual development-incentive provision.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_rights_justification,
    'Are intellectual property rights in software justified on grounds of natural rights (creators own their creation), incentive theory (property rights incentivize creation), or convention (property is what law says it is)?',
    'Philosophical and legal analysis of property-rights foundations; empirical comparison of development productivity and innovation rates across jurisdictions with different intellectual property regimes (strong vs. weak IP protection).',
    'If justified on natural rights, the constraint is foundational and removal would be unjust. If justified on incentive theory, the constraint''s legitimacy depends on empirical evidence that development funding actually improves under property-rights protection—if open-source models fund equivalent innovation, the justification collapses. If justified on convention, the constraint is mutable and no stronger than social consensus.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(property_rights_justification, preference, 'What philosophical foundation, if any, justifies property rights in source code.').

omega_variable(
    coordination_extraction_decoupling,
    'Could the coordination function (incentivizing development) be preserved while removing the extraction function (access restrictions and pricing power)?',
    'Natural experiment from open-source-dominated domains (Linux kernel, Apache, Kubernetes) where equivalent software is funded and maintained without property-rights restrictions. Cross-domain analysis: software with strong IP protection vs. software with weak or no IP protection, controlling for market size and development resources.',
    'If the functions are decoupled, the extraction component is not necessary to the coordination function and persists through institutional lock-in and enforcement rather than necessity. This would support reclassification toward snare or piton (extraction without coordination justification). If the functions are inseparable, the constraint is justified as the price of development incentives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_decoupling, empirical, 'Whether coordination could persist without extraction—evidence from open-source software development at scale.').

omega_variable(
    alternative_incentive_models,
    'What are the viable alternative funding and incentive mechanisms for software development, and do they scale to complex, long-lived, mission-critical systems?',
    'Empirical analysis of open-source funding models (corporate sponsorship, crowdfunding, patronage, public funding, services revenue) and their application to different software domains: developer tools, infrastructure, enterprise systems, consumer applications.',
    'If scalable alternatives exist and fund high-quality software, the justification for proprietary restriction weakens and the constraint shifts toward pure extraction. If proprietary funding remains materially superior for certain classes of software, the constraint remains partially coordinating.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_incentive_models, empirical, 'Viability and scalability of non-proprietary funding models for software development.').

omega_variable(
    reading_boundary_axiom_one,
    'Is source code a form of intellectual property that should be protected by copyright and licensing law?',
    'This is the foundational axiom of the property-rights reading: source code is property. Sibling readings deny this or make it conditional. The freedom_imperative_reading denies property status entirely (source code should be free). The pragmatic_development_reading is conditional (property is instrumental, not essential). The utilitarian_hybrid_reading is context-dependent (property in some cases, free in others). This axiom is holdable but contested—it is the primary structural difference between this reading and its siblings.',
    'If this axiom is denied or overridden within the property-rights reading''s own tradition (e.g., major corporations shift to open-source models wholesale, treating their code as public goods), the reading''s foundational claim is weakened and may foreclose its own coherence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_boundary_axiom_one, preference, 'Source code is intellectual property deserving legal protection (foundational axiom of property-rights reading).').

omega_variable(
    reading_boundary_axiom_two,
    'Are licensing restrictions legitimate exercises of property ownership, or are they coercive constraints on user autonomy?',
    'This is the secondary axiom: given that property rights are recognized, do licensing terms properly express the owner''s authority, or do they exceed legitimate ownership by restricting use of something already sold/licensed? This maps to the ''first sale'' doctrine, right-to-repair movements, and the boundary between property and contract. The freedom_imperative_reading holds that even property ownership should not include the right to restrict source-code modification and redistribution. The pragmatic and utilitarian readings are conditional on context.',
    'If licensing restrictions are reclassified as beyond the scope of legitimate ownership (right-to-repair doctrine expands; courts restrict anti-circumvention statutes), the property-rights reading''s secondary axiom is overridden and the constraint reclassifies toward snare or scaffold (coercive rather than proprietary).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_boundary_axiom_two, preference, 'Licensing restrictions are legitimate exercises of source-code ownership, not coercive control (secondary axiom of property-rights reading).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__property_rights_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__property_rights_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(soft_tr_t0, observed).
narrative_ontology:measurement(soft_tr_t8, software_source_status__property_rights_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement_basis(soft_tr_t8, observed).
narrative_ontology:measurement(soft_tr_t16, software_source_status__property_rights_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement_basis(soft_tr_t16, observed).
narrative_ontology:measurement(soft_tr_t24, software_source_status__property_rights_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement_basis(soft_tr_t24, observed).
narrative_ontology:measurement(soft_tr_t32, software_source_status__property_rights_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement_basis(soft_tr_t32, observed).
narrative_ontology:measurement(soft_tr_t40, software_source_status__property_rights_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(soft_tr_t40, observed).
narrative_ontology:measurement(soft_tr_t50, software_source_status__property_rights_reading, theater_ratio, 50, 0.41).
narrative_ontology:measurement_basis(soft_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__property_rights_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(soft_be_t0, observed).
narrative_ontology:measurement(soft_be_t8, software_source_status__property_rights_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement_basis(soft_be_t8, observed).
narrative_ontology:measurement(soft_be_t16, software_source_status__property_rights_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement_basis(soft_be_t16, observed).
narrative_ontology:measurement(soft_be_t24, software_source_status__property_rights_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement_basis(soft_be_t24, observed).
narrative_ontology:measurement(soft_be_t32, software_source_status__property_rights_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement_basis(soft_be_t32, observed).
narrative_ontology:measurement(soft_be_t40, software_source_status__property_rights_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(soft_be_t40, observed).
narrative_ontology:measurement(soft_be_t50, software_source_status__property_rights_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(soft_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_source_status__property_rights_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(soft_su_t0, observed).
narrative_ontology:measurement(soft_su_t8, software_source_status__property_rights_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement_basis(soft_su_t8, observed).
narrative_ontology:measurement(soft_su_t16, software_source_status__property_rights_reading, suppression_requirement, 16, 0.57).
narrative_ontology:measurement_basis(soft_su_t16, observed).
narrative_ontology:measurement(soft_su_t24, software_source_status__property_rights_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement_basis(soft_su_t24, observed).
narrative_ontology:measurement(soft_su_t32, software_source_status__property_rights_reading, suppression_requirement, 32, 0.62).
narrative_ontology:measurement_basis(soft_su_t32, observed).
narrative_ontology:measurement(soft_su_t40, software_source_status__property_rights_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement_basis(soft_su_t40, observed).
narrative_ontology:measurement(soft_su_t50, software_source_status__property_rights_reading, suppression_requirement, 50, 0.62).
narrative_ontology:measurement_basis(soft_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__property_rights_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_source_status__property_rights_reading, 0.22).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_source_status__utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'software_source_status' — the standing question of source-code legitimacy in software governance. Four structurally distinct readings emit four different constraints from the same kernel: property_rights_reading (this file), freedom_imperative_reading, pragmatic_development_reading, and utilitarian_hybrid_reading. They share the same referent (source-code access restrictions) but differ fundamentally in whether those restrictions are legitimate property rights (property reading), unjust (freedom reading), instrumentally justified (pragmatic reading), or context-dependent (utilitarian reading). Each reading has its own ε, beneficiary/victim structure, and classification. The readings coexist as live positions held by different parties in software-governance discourse; none logically forecloses the others within a single party's framework, though they compete for institutional authority. Network edges link all four files; each reading affects the others' legitimacy and operational space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(software_source_status__property_rights_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
