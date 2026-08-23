% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__property_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__property_rights_reading, []).

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
 *   constraint_id: software_control_legitimacy__property_rights_reading
 *   human_readable: Software Control as Property Right — Proprietary Licensing Regime
 *   domain: economic/technological/legal
 *
 * SUMMARY:
 *   The property rights reading of software control legitimacy asserts that
 *   creators hold a legitimate property right in their code, entitling them
 *   to restrict use, modification, and distribution. This reading underpins
 *   the proprietary software industry: copyright and contract law enforce
 *   license terms, vendors monetize through per-copy or per-seat fees, and
 *   investors fund development expecting IP-protected returns. The constraint
 *   is CLAIMED as a coordination mechanism (enabling commercial
 *   sustainability) but operates with substantial extraction (restricting
 *   user freedoms, suppressing commons alternatives, enabling rent extraction
 *   via lock-in). The engine will compute per-seat types from the structural
 *   data; the authored claim (tangled_rope) and metrics are independent.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__property_rights_reading, 0.45).
domain_priors:suppression_score(software_control_legitimacy__property_rights_reading, 0.65).
domain_priors:theater_ratio(software_control_legitimacy__property_rights_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__property_rights_reading, tangled_rope).
narrative_ontology:human_readable(software_control_legitimacy__property_rights_reading, "Software Control as Property Right — Proprietary Licensing Regime").
narrative_ontology:topic_domain(software_control_legitimacy__property_rights_reading, "economic/technological/legal").

domain_priors:requires_active_enforcement(software_control_legitimacy__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__property_rights_reading, '63fca4cc-8724-47b3-97ad-d9a5b259257b').
narrative_ontology:cs_kernel_codification('63fca4cc-8724-47b3-97ad-d9a5b259257b', formalized).
narrative_ontology:cs_authority_grounding('63fca4cc-8724-47b3-97ad-d9a5b259257b', lineage).
narrative_ontology:cs_interpretation_layer_present('63fca4cc-8724-47b3-97ad-d9a5b259257b').
narrative_ontology:cs_reading_relation('63fca4cc-8724-47b3-97ad-d9a5b259257b', software_control_legitimacy__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('63fca4cc-8724-47b3-97ad-d9a5b259257b', software_control_legitimacy__pragmatic_openness_reading, coexists_with).
narrative_ontology:cs_reading_relation('63fca4cc-8724-47b3-97ad-d9a5b259257b', software_control_legitimacy__commons_reading, influences).
narrative_ontology:cs_axiom('63fca4cc-8724-47b3-97ad-d9a5b259257b', foundational, creator_has_exclusive_control_right).
narrative_ontology:cs_axiom_status(creator_has_exclusive_control_right, holdable).
narrative_ontology:cs_axiom_grounding('63fca4cc-8724-47b3-97ad-d9a5b259257b', creator_has_exclusive_control_right, deontological).
narrative_ontology:cs_axiom('63fca4cc-8724-47b3-97ad-d9a5b259257b', secondary, commercial_sustainability_requires_exclusion).
narrative_ontology:cs_axiom_status(commercial_sustainability_requires_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('63fca4cc-8724-47b3-97ad-d9a5b259257b', commercial_sustainability_requires_exclusion, instrumental).
narrative_ontology:cs_reference_frame('63fca4cc-8724-47b3-97ad-d9a5b259257b', classical_ip_framework).
narrative_ontology:cs_drift_state('63fca4cc-8724-47b3-97ad-d9a5b259257b', digital_era_commons_expansion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('63fca4cc-8724-47b3-97ad-d9a5b259257b', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__property_rights_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, software_vendors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, investors).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, foss_advocates).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, software_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, software_users).
narrative_ontology:constraint_vindicates(software_control_legitimacy__property_rights_reading, property_rights_doctrine).
narrative_ontology:constraint_vindicates(software_control_legitimacy__property_rights_reading, commercial_sustainability_thesis).
narrative_ontology:constraint_vindicates(software_control_legitimacy__property_rights_reading, labor_desert_justification).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and enforce license terms that restrict use, modification, and distribution. Collect license fees and control product roadmaps. Justify restrictions as necessary to recoup R&D investment and fund ongoing development. Can pivot to open-core or SaaS models if enforcement weakens.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).

% Provide capital expecting returns protected by IP exclusivity. Benefit from the ability to restrict and monetize software artifacts. Diversify across portfolios; exit via acquisition or public markets. Their returns depend on the enforceability of vendor control.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, investors, beneficiary,
    powerful, biographical, mobile, global).

% Build and maintain free/open alternatives that compete with proprietary software. Denied return on investment when proprietary network effects, IP thickets, and vendor lock-in suppress adoption. Exit means abandoning the field or accepting proprietary dependencies; constrained by ecosystem momentum and compatibility requirements.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, foss_advocates, payer,
    organized, generational, constrained, global).

% Pay license fees and accept restrictions on modification, redistribution, and repair. Receive polished, supported, integrated products in return. Exit is constrained by switching costs, data lock-in, institutional mandates, and lack of viable alternatives for specialized domains.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, software_users, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__property_rights_reading, software_users, beneficiary).

% Investigate whether IP enforcement crosses into anti-competitive conduct (refusal to license, tying, predatory pricing). Can impose remedies that alter the constraint's enforcement but do not set the underlying property regime.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, competition_authorities, observer,
    institutional, generational, analytical, national).

% Contribute labor to commons-based production under the assumption that code remains free. Their work is sometimes incorporated into proprietary products without reciprocity (permissive licenses) or they face legal risk from patent claims. Would object to expansion of proprietary control but are structurally excluded from legislative and judicial IP-setting processes.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, open_source_maintainers, excluded,
    organized, biographical, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates capital allocation to software development by guaranteeing creators exclusive control over copies and derivatives, enabling them to capture returns on investment and sustain commercial R&D.
% TRANSFER_FUNCTION: Moves control over use, modification, and distribution from users to vendors; moves monetary payment from users (and downstream businesses) to vendors and investors; moves developmental agency from the commons to controlled roadmaps.
% ABSENT_VOICES: Users in developing economies priced out of proprietary stacks; developers in jurisdictions with weak IP enforcement who cannot participate in the proprietary economy; future innovators blocked by patent thickets and API copyright claims; repair technicians and right-to-repair advocates denied access to diagnostic software.
% DISAPPEARANCE_RATIONALE: If exclusive control vanished overnight, the commercial proprietary software model would collapse: venture capital would flee, vendors would shift to SaaS/hosted models or open-core, FOSS would become the default for infrastructure, and the entire economy of software licensing would reorganize around services and support rather than copy restriction.
% FOUNDING_PROBLEM: Early commercial software (1970s–80s) had no effective protection against copying; companies could not recoup development costs, leading to underinvestment and market failure for complex applications.
% FOUNDING_PROBLEM_CORROBORATION: Historical accounts from early software industry participants (e.g., Gates' 1976 'Open Letter to Hobbyists', contemporaneous trade press) attest to the copying problem. Economic historians (e.g., Bessen & Meurer on software patents, Lampe & Moser on copyright) document the investment response. FOSS advocates and development economists (e.g., Lerner & Tirole, von Hippel) attest that alternative funding models (corporate sponsorship, services, commons-based peer production) now solve the founding problem without exclusive control.
narrative_ontology:disappearance_verdict(software_control_legitimacy__property_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__property_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__property_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_control_legitimacy__property_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__property_rights_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__property_rights_reading_tests).
:- end_tests(software_control_legitimacy__property_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) reflects moderate but structural extraction: users pay above marginal cost, FOSS contributors lose potential returns, lock-in enables pricing power. Suppression (0.65) is higher because persistence depends on active legal/technical enforcement (copyright, DRM, contract, patent) not voluntary participation. Theater ratio (0.3) is low-moderate: enforcement is genuinely functional for the coordination claim, but a growing share (audit trails, license compliance tooling, anti-circumvention) serves extraction maintenance. Accessibility collapse (0.5) is partial: FOSS alternatives exist and grow, but network effects and compatibility constraints limit exit. Resistance (0.55) is significant: FOSS movement, right-to-repair, regulatory scrutiny (DMA, essential facilities), and user pushback are active.
 *
 * PERSPECTIVAL GAP:
 *   From the vendor seat, the constraint is genuine coordination: they built it, they maintain it, it funds the product. From the FOSS advocate seat, the same structure is extraction: their labor is enclosed, their alternatives suppressed by network effects the constraint reinforces. From the user seat, it is a mixed deal: real product value received, but at monopoly-inflated cost with freedom loss. The engine computes this divergence; the claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Vendors and investors are structural beneficiaries (d near 0.0): they collect rents, set terms, have arbitrage-grade exit (pivot models, diversify). FOSS advocates and users are structural targets (d near 1.0): they bear restrictions, pay transfers, have constrained exit (ecosystem lock-in, identity_locked for maintainers). Competition authorities are analytical observers. The derivation chain from beneficiary/victim declarations + exit options produces this gradient; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (inability to monetize software without copy control) is contested as live. Vendors and investors attest it remains live (piracy, free-rider problem). FOSS advocates and development economists attest it is substantially solved by alternative models (corporate-sponsored FOSS, SaaS, services). The arrangement persists partly because the original mandate (enable commercial software) has been joined by a secondary mandate (protect incumbent rent streams) — a classic mandatrophy signature. The classification (tangled_rope) captures this: the coordination function is real but extraction has layered onto it and now sustains the enforcement machinery.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the property_rights_reading a distinct constraint from its sibling readings, or a framing of the same underlying arrangement?',
    'Apply the ε-invariance test: if measuring extraction under the property rights framing yields a different ε than under the freedom imperative framing, they are distinct constraints. The structural delta (different victim sets, different beneficiary sets, different coordination claims) indicates distinct constraints.',
    'If distinct, each reading gets its own constraint story with its own ε, stakeholders, and classification. If same constraint, the framework must model observer-relative classification — which the ε-invariance principle forbids.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s readings map to distinct constraints per ε-invariance.').

omega_variable(
    coordination_vs_extraction_boundary,
    'Is the coordination function (enabling commercial R&D) genuine and separable from the extraction (restricting user freedom, suppressing commons), or is the coordination story cover for extraction?',
    'Counterfactual: if enforcement were limited to commercial redistribution (not personal use, modification, repair, interoperability), would commercial R&D still be funded? Evidence from FOSS commercial models (Red Hat, SUSE, cloud providers) suggests yes — coordination survives without full extraction.',
    'If separable, the constraint is a tangled_rope where extraction is layered onto coordination. If inseparable, the property rights claim itself is the extraction mechanism and the constraint may be a snare from the user/FOSS seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Whether the property right''s coordination and extraction components are structurally separable.').

omega_variable(
    suppression_mechanism_mix,
    'Is the measured suppression primarily structural (legal/technical barriers) or does it include internalized suppression (users/developers believing proprietary control is natural/legitimate)?',
    'Post-reform suppression trajectory: if jurisdictions that legalize personal-use copying, repair, or interoperability see persistent behavioral avoidance of those freedoms, internalized suppression is present.',
    'If internalized, effective suppression is higher than structural measure suggests; the constraint persists partly through ideological capture, not just enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_mix, empirical, 'Structural vs internalized suppression in proprietary software regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__property_rights_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(swctl_pr_tr_t0, software_control_legitimacy__property_rights_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(swctl_pr_tr_t6, software_control_legitimacy__property_rights_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(swctl_pr_tr_t12, software_control_legitimacy__property_rights_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement(swctl_pr_tr_t18, software_control_legitimacy__property_rights_reading, theater_ratio, 18, 0.25).
narrative_ontology:measurement(swctl_pr_tr_t24, software_control_legitimacy__property_rights_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement(swctl_pr_tr_t30, software_control_legitimacy__property_rights_reading, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(swctl_pr_be_t0, software_control_legitimacy__property_rights_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(swctl_pr_be_t6, software_control_legitimacy__property_rights_reading, base_extractiveness, 6, 0.32).
narrative_ontology:measurement(swctl_pr_be_t12, software_control_legitimacy__property_rights_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(swctl_pr_be_t18, software_control_legitimacy__property_rights_reading, base_extractiveness, 18, 0.42).
narrative_ontology:measurement(swctl_pr_be_t24, software_control_legitimacy__property_rights_reading, base_extractiveness, 24, 0.44).
narrative_ontology:measurement(swctl_pr_be_t30, software_control_legitimacy__property_rights_reading, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(swctl_pr_su_t0, software_control_legitimacy__property_rights_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(swctl_pr_su_t6, software_control_legitimacy__property_rights_reading, suppression_requirement, 6, 0.5).
narrative_ontology:measurement(swctl_pr_su_t12, software_control_legitimacy__property_rights_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement(swctl_pr_su_t18, software_control_legitimacy__property_rights_reading, suppression_requirement, 18, 0.62).
narrative_ontology:measurement(swctl_pr_su_t24, software_control_legitimacy__property_rights_reading, suppression_requirement, 24, 0.64).
narrative_ontology:measurement(swctl_pr_su_t30, software_control_legitimacy__property_rights_reading, suppression_requirement, 30, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__property_rights_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_control_legitimacy__property_rights_reading, 0.12).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_patent_enforcement).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, api_copyright_regime).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, drm_anti_circumvention).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, saas_terms_of_service).

% DUAL FORMULATION NOTE:
% This constraint (property_rights_reading) and its siblings (freedom_imperative_reading, pragmatic_openness_reading, commons_reading) form a constraint family decomposing the colloquial label 'software IP legitimacy.' Each has distinct ε, victim/beneficiary structure, and claimed type. The property rights reading has the highest ε (moderate extraction) and claims tangled_rope; freedom imperative claims snare (ε higher from user seat); pragmatic openness claims rope (low ε); commons claims scaffold (transitional governance).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
