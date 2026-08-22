% ============================================================================
% CONSTRAINT STORY: software_source_status__pragmatic_development_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__pragmatic_development_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: software_source_status__pragmatic_development_reading
 *   human_readable: Open Source as Superior Development Methodology (Pragmatic Reading)
 *   domain: software_engineering/political_economy_of_technology
 *
 * SUMMARY:
 *   This constraint story instantiates the pragmatic development reading of
 *   the contested kernel 'software_source_status'. The reading holds that
 *   open source development is superior because it enables peer review, rapid
 *   bug detection, distributed innovation, and knowledge transfer — not
 *   because proprietary software is ethically illegitimate. The constraint
 *   operates as a coordination mechanism: developers and companies adopt open
 *   practices because they produce better software faster. Permissive
 *   licensing (MIT, BSD, Apache) is acceptable and often preferred over
 *   copyleft because it maximizes adoption and reuse. The reading gained
 *   dominance in industry from roughly 1998 (Eric Raymond's 'The Cathedral
 *   and the Bazaar', Netscape's Mozilla release) through the present,
 *   displacing the earlier freedom_imperative framing in commercial contexts.
 *   Extraction is low and stable because the arrangement is genuinely
 *   coordination-oriented — participants join voluntarily and can exit to
 *   proprietary models. Theater has risen as corporate actors perform 'open
 *   source' signaling (vanity repos, open-core theater, contributor license
 *   agreements that concentrate control) without proportionate community
 *   benefit. Suppression is low but nonzero: platform network effects
 *   (GitHub, npm, PyPI) create soft lock-in, and corporate governance of
 *   major projects (Kubernetes, React, VS Code) creates asymmetric influence.
 *
 * KEY AGENTS:
 *   - open_source_maintainers: Primary coordinators (organized/mobile) — contribute labor, govern project direction, can fork or leave
 *   - software_users_general: Primary beneficiaries (organized/constrained) — gain free, inspectable, improvable software; exit constrained by ecosystem integration
 *   - companies_using_permissive_oss: Beneficiaries with agenda-setter influence (powerful/arbitrage) — extract value from commons, fund maintenance selectively, can internalize forks
 *   - developer_communities: Coordinated participants (organized/constrained) — contribute patches, report bugs, build reputation; exit constrained by skill specificity and network effects
 *   - proprietary_software_vendors: Excluded but adjacent (powerful/mobile) — compete with open alternatives, adopt open practices selectively, lobby for IP enforcement
 *   - platform_infrastructure_providers: Agenda-setters with extraction potential (institutional/arbitrage) — GitHub, GitLab, npm, PyPI; control distribution infrastructure, can shape governance norms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__pragmatic_development_reading, 0.18).
domain_priors:suppression_score(software_source_status__pragmatic_development_reading, 0.22).
domain_priors:theater_ratio(software_source_status__pragmatic_development_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__pragmatic_development_reading, rope).
narrative_ontology:human_readable(software_source_status__pragmatic_development_reading, "Open Source as Superior Development Methodology (Pragmatic Reading)").
narrative_ontology:topic_domain(software_source_status__pragmatic_development_reading, "software_engineering/political_economy_of_technology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__pragmatic_development_reading, 'a2791b0a-bd97-4aae-b87e-a3244c0a7ea5').
narrative_ontology:cs_kernel_codification('a2791b0a-bd97-4aae-b87e-a3244c0a7ea5', distributed).
narrative_ontology:cs_authority_grounding('a2791b0a-bd97-4aae-b87e-a3244c0a7ea5', practice).
narrative_ontology:cs_interpretation_layer_present('a2791b0a-bd97-4aae-b87e-a3244c0a7ea5').
narrative_ontology:cs_reading_relation('a2791b0a-bd97-4aae-b87e-a3244c0a7ea5', software_source_status__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('a2791b0a-bd97-4aae-b87e-a3244c0a7ea5', software_source_status__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('a2791b0a-bd97-4aae-b87e-a3244c0a7ea5', software_source_status__utilitarian_hybrid_reading, influences).
narrative_ontology:cs_axiom('a2791b0a-bd97-4aae-b87e-a3244c0a7ea5', foundational, open_development_superior_for_quality_velocity).
narrative_ontology:cs_axiom_status(open_development_superior_for_quality_velocity, holdable).
narrative_ontology:cs_axiom_grounding('a2791b0a-bd97-4aae-b87e-a3244c0a7ea5', open_development_superior_for_quality_velocity, empirically_contingent).
narrative_ontology:cs_axiom('a2791b0a-bd97-4aae-b87e-a3244c0a7ea5', foundational, permissive_licensing_maximizes_adoption_and_reuse).
narrative_ontology:cs_axiom_status(permissive_licensing_maximizes_adoption_and_reuse, holdable).
narrative_ontology:cs_axiom_grounding('a2791b0a-bd97-4aae-b87e-a3244c0a7ea5', permissive_licensing_maximizes_adoption_and_reuse, empirically_contingent).
narrative_ontology:cs_axiom('a2791b0a-bd97-4aae-b87e-a3244c0a7ea5', secondary, freedom_instrumental_not_intrinsic).
narrative_ontology:cs_axiom_status(freedom_instrumental_not_intrinsic, holdable).
narrative_ontology:cs_axiom_grounding('a2791b0a-bd97-4aae-b87e-a3244c0a7ea5', freedom_instrumental_not_intrinsic, instrumental).
narrative_ontology:cs_reference_frame('a2791b0a-bd97-4aae-b87e-a3244c0a7ea5', bazaar_development_model).
narrative_ontology:cs_drift_state('a2791b0a-bd97-4aae-b87e-a3244c0a7ea5', corporate_capture_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a2791b0a-bd97-4aae-b87e-a3244c0a7ea5', '').
narrative_ontology:cs_kernel_id(software_source_status__pragmatic_development_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, open_source_maintainers).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, software_users_general).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, companies_using_permissive_oss).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, developer_communities).
narrative_ontology:constraint_vindicates(software_source_status__pragmatic_development_reading, peer_review_improves_software_quality).
narrative_ontology:constraint_vindicates(software_source_status__pragmatic_development_reading, distributed_development_accelerates_innovation).
narrative_ontology:constraint_vindicates(software_source_status__pragmatic_development_reading, freedom_instrumental_to_technical_excellence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Govern project direction, review contributions, set release cadence. Gain reputation, skill, community standing, and sometimes employment offers. Can fork or leave if governance becomes extractive. Their authority derives from demonstrated contribution, not formal appointment.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, open_source_maintainers, agenda_setter,
    organized, biographical, mobile, global).

% Use open source software for free with rights to inspect, modify, redistribute. Benefit from security audits, bug fixes, and feature improvements driven by the community. Exit constrained by ecosystem integration (dependencies, workflows, data formats) — switching costs are real but not prohibitive.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, software_users_general, beneficiary,
    organized, biographical, constrained, global).

% Incorporate permissively-licensed open source into commercial products, cloud services, and internal infrastructure. Capture disproportionate value relative to contribution (often zero direct contribution). Fund maintenance selectively via sponsorships or employed maintainers. Can internalize forks if upstream becomes problematic. Lobby for standards and policies that favor permissive licensing.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, companies_using_permissive_oss, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__pragmatic_development_reading, companies_using_permissive_oss, agenda_setter).

% Contribute patches, report bugs, write documentation, answer questions. Gain skills, reputation, portfolio, and professional networks. Exit constrained by skill specificity (deep project knowledge not transferable) and social capital invested in the community. Can fork but forking fragments the community they value.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, developer_communities, beneficiary,
    organized, biographical, constrained, global).

% Develop and sell closed-source software competing with open alternatives. Adopt open source selectively (components, tools, infrastructure) while keeping core products proprietary. Lobby for strong IP enforcement, software patents, and against copyleft mandates. Not coerced by open source constraint — they compete with it.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, proprietary_software_vendors, excluded,
    powerful, biographical, mobile, global).

% Operate GitHub, GitLab, npm, PyPI, Docker Hub, and similar infrastructure. Control discovery, distribution, identity, and CI/CD for the majority of open source projects. Extract platform rents (compute minutes, org seats, advanced features). Shape governance norms via terms of service, default branch names, codes of conduct, and feature priorities. Can deplatform projects. Their extraction is parasitic on the coordination layer — an omega addresses whether this is structural or separate.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, platform_infrastructure_providers, agenda_setter,
    institutional, generational, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of distributed, trust-minimized software development by providing a shared methodology: public source code enables peer review, distributed debugging, parallel feature development, and knowledge transfer across organizational boundaries. Replaces centralized cathedral development with bazaar coordination.
% TRANSFER_FUNCTION: Moves software artifacts (code, fixes, features, knowledge) from contributors to the commons, and from the commons to users and companies. No monetary transfer required; the currency is contribution and reputation. Companies capture commercial value downstream without direct payment upstream.
% ABSENT_VOICES: End users who lack technical literacy to exercise software freedom (inspect, modify) — they receive the software but cannot meaningfully participate in the coordination. Would-be contributors excluded by hostile community norms, language barriers, or time poverty. Future generations who inherit a software commons potentially degraded by corporate capture — they are not present to object.
% DISAPPEARANCE_RATIONALE: If the pragmatic open source constraint vanished overnight (e.g., all permissive licenses became proprietary), the software industry would reorganize around closed-source components, paid libraries, and vendor lock-in. Development velocity would drop as knowledge transfer barriers rise. Security would degrade as auditability disappears. Companies that built on permissive OSS would face massive rewrites or licensing costs. The world would rearrange significantly.
% FOUNDING_PROBLEM: Pre-1998 software development suffered from: fragmented codebases with no shared inspection, slow bug discovery in closed code, vendor lock-in preventing user agency, duplicated effort across isolated teams, and high barriers to entry for new developers. The 'cathedral' model (centralized, opaque, slow) could not scale to internet-era complexity.
% FOUNDING_PROBLEM_CORROBORATION: The pragmatic reading's own proponents (Raymond, O'Reilly, early Linux distributors) attest the founding problem is substantially solved by open collaboration. Freedom_imperative advocates (FSF, Stallman) attest the problem was misdiagnosed — the issue was not development efficiency but user freedom — and the pragmatic solution betrayed the principle. Property_rights advocates (Microsoft circa 2001, proprietary vendors) attested the problem never justified infringing creator rights. Utilitarian_hybrid analysts (e.g., Lerner & Tirole, von Hippel) corroborate that open collaboration solves a genuine coordination problem but dispute whether permissive licensing is optimal for all contexts. No single corroborating source outside the beneficiary set is universally accepted.
narrative_ontology:disappearance_verdict(software_source_status__pragmatic_development_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__pragmatic_development_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__pragmatic_development_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(software_source_status__pragmatic_development_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__pragmatic_development_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__pragmatic_development_reading_tests).
:- end_tests(software_source_status__pragmatic_development_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18) reflects that the constraint extracts little from participants — contributions are voluntary, licensing is permissive, forking is always available. The modest extraction that exists comes from platform infrastructure fees (GitHub Actions minutes, npm org seats), asymmetric governance in corporate-backed projects, and the opportunity cost of participating in open rather than proprietary development. Suppression (0.22) captures soft lock-in from ecosystem integration and network effects, not hard coercion. Theater (0.31, rising) captures the growing gap between 'open source' signaling and substantive community governance — corporate projects with CLAs, open-core models, foundation capture. Accessibility collapse (0.35) is moderate: alternatives (proprietary development, closed-source) remain fully viable and widely used. Resistance (0.28) reflects ongoing contestation from freedom_imperative advocates (who view permissive licensing as surrender) and property_rights advocates (who view copyleft as viral overreach).
 *
 * PERSPECTIVAL GAP:
 *   Maintainer seat: experiences the constraint as genuine coordination — they gain reputation, skill, community, and influence. User seat: experiences as net beneficiary — free software with inspection/modification rights. Corporate user seat: experiences as asymmetric value extraction from commons — they capture disproportionate value relative to contribution. Platform provider seat: experiences as infrastructure control with rent-seeking potential — they tax the coordination layer. The engine will compute per-seat effective extraction from these structural positions; the claimed rope type reflects the dominant coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: open_source_maintainers (coordinators who benefit from network effects), software_users_general (receive software freedom without obligation), companies_using_permissive_oss (capture value with minimal reciprocity obligation), developer_communities (gain skills, reputation, public goods). No victims declared — the reading explicitly rejects the premise that proprietary software is inherently harmful or that non-participants are injured. Platform_infrastructure_providers are not listed as beneficiaries despite extracting platform rents because their extraction is parasitic on the coordination function, not intrinsic to it — an omega addresses this ambiguity. Proprietary_software_vendors are excluded, not victims: they compete with open alternatives but are not coerced by the open source constraint itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fragmented, unreliable software development before open collaboration) is contested: open advocates say it's solved; freedom advocates say the solution betrayed the principle; property advocates say the problem never justified infringing IP. The constraint has not undergone mandatrophy — its coordination function remains live and expanding. Theater rise suggests drift toward piton dynamics in corporate-backed sub-projects, but the core constraint (permissive open collaboration) remains functional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    platform_extraction_parasitism,
    'Does the rent extraction by platform infrastructure providers (GitHub, npm, PyPI) constitute a structural feature of the open source coordination constraint, or a separate parasitic constraint layered on top?',
    'Compare extraction rates and governance control in platform-mediated vs. self-hosted/federated open source ecosystems (e.g., GitHub vs. SourceHut vs. email-driven kernel development). If platform extraction correlates with governance capture, it is structural; if self-hosted projects avoid it without coordination loss, it is parasitic.',
    'If structural, the constraint''s effective extractiveness is higher than the 0.18 core value and platform providers should be listed as beneficiaries. If parasitic, the core constraint remains low-extraction and platform rents are a separate snare/tangled_rope constraint linked via network.affects_constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_extraction_parasitism, empirical, 'Whether platform rent extraction is intrinsic to or layered on the open source coordination constraint.').

omega_variable(
    permissive_vs_copyleft_coordination_efficiency,
    'Does permissive licensing actually maximize coordination efficiency (adoption, contribution, innovation velocity) compared to copyleft, or does it enable free-riding that degrades the commons over time?',
    'Longitudinal comparison of project health metrics (contributor retention, bug fix latency, fork activity, commercial adoption) between permissive and copyleft projects of similar scale and domain, controlling for corporate backing.',
    'If permissive licensing degrades coordination long-term, the constraint''s claimed_type may drift toward tangled_rope (coordination function undermined by asymmetric extraction via free-riding). If copyleft creates higher suppression (license compliance burden) without proportional coordination gain, permissive remains the coordination optimum.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(permissive_vs_copyleft_coordination_efficiency, empirical, 'Whether the permissive licensing preference in this reading is empirically justified or ideologically driven.').

omega_variable(
    kernel_reading_framing_underdetermination,
    'Is the ''pragmatic development'' framing the only coherent reading of the kernel that treats freedom as instrumental, or does it collapse into utilitarian_hybrid under scrutiny?',
    'Test whether there exist policy positions (e.g., on software patents, DRM, interoperability mandates) where pragmatic_development and utilitarian_hybrid diverge in their prescriptions. If no divergence exists across a broad policy space, the readings are empirically indistinguishable and the kernel framing is underdetermined.',
    'If indistinguishable, the two readings should be merged into one constraint story. If distinct, the structural delta (pragmatic focuses on development methodology; utilitarian includes distributive welfare) must be sharpened in the axioms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing_underdetermination, conceptual, 'Whether the pragmatic development reading is structurally distinct from the utilitarian hybrid reading or a subtype of it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__pragmatic_development_reading, 1998, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1998, software_source_status__pragmatic_development_reading, theater_ratio, 1998, 0.08).
narrative_ontology:measurement(soft_tr_t2005, software_source_status__pragmatic_development_reading, theater_ratio, 2005, 0.15).
narrative_ontology:measurement(soft_tr_t2012, software_source_status__pragmatic_development_reading, theater_ratio, 2012, 0.22).
narrative_ontology:measurement(soft_tr_t2019, software_source_status__pragmatic_development_reading, theater_ratio, 2019, 0.28).
narrative_ontology:measurement(soft_tr_t2025, software_source_status__pragmatic_development_reading, theater_ratio, 2025, 0.31).

% Extraction over time
narrative_ontology:measurement(soft_be_t1998, software_source_status__pragmatic_development_reading, base_extractiveness, 1998, 0.12).
narrative_ontology:measurement(soft_be_t2005, software_source_status__pragmatic_development_reading, base_extractiveness, 2005, 0.14).
narrative_ontology:measurement(soft_be_t2012, software_source_status__pragmatic_development_reading, base_extractiveness, 2012, 0.16).
narrative_ontology:measurement(soft_be_t2019, software_source_status__pragmatic_development_reading, base_extractiveness, 2019, 0.18).
narrative_ontology:measurement(soft_be_t2025, software_source_status__pragmatic_development_reading, base_extractiveness, 2025, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(software_source_status__pragmatic_development_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__pragmatic_development_reading, information_standard).
narrative_ontology:boltzmann_floor_override(software_source_status__pragmatic_development_reading, 0.02).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__property_rights_reading).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__utilitarian_hybrid_reading).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, platform_infrastructure_rent_extraction).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, open_core_business_model_extraction).

% DUAL FORMULATION NOTE:
% This constraint is one member of the software_source_status constraint family (kernel_id: software_source_status). The four readings instantiate different constraints with different ε values, beneficiary/victim structures, and claimed types. This reading (pragmatic_development) has ε≈0.18, claimed rope, beneficiaries but no victims. The freedom_imperative reading has higher ε (copyleft enforcement as suppression), claimed tangled_rope/snare, victims including proprietary software users. The property_rights reading has ε near zero for proprietary developers but high for would-be modifiers, claimed rope/tangled_rope. The utilitarian_hybrid reading has context-dependent ε. All four are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
