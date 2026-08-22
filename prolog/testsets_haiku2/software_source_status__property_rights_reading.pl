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
 *   human_readable: Software Source Code Property Rights and Licensing Restriction
 *   domain: intellectual_property/economic/technological
 *
 * SUMMARY:
 *   The property-rights reading of software source code asserts that creators
 *   have legitimate ownership claims to their source code comparable to
 *   physical property ownership. Under this reading, licensing restrictions
 *   that prohibit reverse engineering, modification, and redistribution are
 *   legitimate exercises of ownership rights. This is one of four competing
 *   readings of the kernel 'software source code status': the
 *   freedom-imperative reading frames source access as a fundamental human
 *   right; the pragmatic-development reading treats open source as
 *   methodologically superior; the utilitarian-hybrid reading seeks to
 *   maximize aggregate welfare across contexts. This constraint story
 *   instantiates ONLY the property-rights reading, assessing the standing
 *   arrangement (proprietary licensing with restriction enforcement) as the
 *   property-rights reading sees it — high extractiveness, real coordination
 *   function, active suppression required to maintain.
 *
 * KEY AGENTS:
 *   - proprietary_software_vendors: institutional power, arbitrage exit; set and enforce the licensing terms
 *   - dependent_developers: moderate power, constrained exit; must pay licensing fees and accept vendor control
 *   - end_users seeking modification: powerless, trapped exit; face legal barriers to customization
 *   - security researchers: moderate power, constrained exit; restricted by DMCA and vendor-controlled disclosure
 *   - open-source developers: moderate power, mobile exit; structurally excluded by licensing incompatibility
 *   - intellectual property attorneys: institutional power; benefit from system complexity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__property_rights_reading, 0.68).
domain_priors:suppression_score(software_source_status__property_rights_reading, 0.52).
domain_priors:theater_ratio(software_source_status__property_rights_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__property_rights_reading, tangled_rope).
narrative_ontology:human_readable(software_source_status__property_rights_reading, "Software Source Code Property Rights and Licensing Restriction").
narrative_ontology:topic_domain(software_source_status__property_rights_reading, "intellectual_property/economic/technological").

domain_priors:requires_active_enforcement(software_source_status__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__property_rights_reading, '1dfc7f88-4719-4d8b-97e9-e6fda3fbc38d').
narrative_ontology:cs_kernel_codification('1dfc7f88-4719-4d8b-97e9-e6fda3fbc38d', formalized).
narrative_ontology:cs_authority_grounding('1dfc7f88-4719-4d8b-97e9-e6fda3fbc38d', lineage).
narrative_ontology:cs_interpretation_layer_present('1dfc7f88-4719-4d8b-97e9-e6fda3fbc38d').
narrative_ontology:cs_reading_relation('1dfc7f88-4719-4d8b-97e9-e6fda3fbc38d', software_source_status__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('1dfc7f88-4719-4d8b-97e9-e6fda3fbc38d', software_source_status__pragmatic_development_reading, influences).
narrative_ontology:cs_reading_relation('1dfc7f88-4719-4d8b-97e9-e6fda3fbc38d', software_source_status__utilitarian_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('1dfc7f88-4719-4d8b-97e9-e6fda3fbc38d', foundational, source_code_is_property).
narrative_ontology:cs_axiom_status(source_code_is_property, holdable).
narrative_ontology:cs_axiom_grounding('1dfc7f88-4719-4d8b-97e9-e6fda3fbc38d', source_code_is_property, deontological).
narrative_ontology:cs_axiom('1dfc7f88-4719-4d8b-97e9-e6fda3fbc38d', foundational, creator_ownership_rights_legitimate).
narrative_ontology:cs_axiom_status(creator_ownership_rights_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('1dfc7f88-4719-4d8b-97e9-e6fda3fbc38d', creator_ownership_rights_legitimate, conventional).
narrative_ontology:cs_axiom('1dfc7f88-4719-4d8b-97e9-e6fda3fbc38d', secondary, licensing_restriction_valid_ownership_exercise).
narrative_ontology:cs_axiom_status(licensing_restriction_valid_ownership_exercise, holdable).
narrative_ontology:cs_axiom_grounding('1dfc7f88-4719-4d8b-97e9-e6fda3fbc38d', licensing_restriction_valid_ownership_exercise, conventional).
narrative_ontology:cs_reference_frame('1dfc7f88-4719-4d8b-97e9-e6fda3fbc38d', intellectual_property_doctrine_framework).
narrative_ontology:cs_drift_state('1dfc7f88-4719-4d8b-97e9-e6fda3fbc38d', contemporary_open_source_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1dfc7f88-4719-4d8b-97e9-e6fda3fbc38d', '').
narrative_ontology:cs_kernel_id(software_source_status__property_rights_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, copyright_holders).
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, intellectual_property_attorneys).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, dependent_developers).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, end_users_seeking_modification).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, security_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, dependent_developers).
narrative_ontology:constraint_vindicates(software_source_status__property_rights_reading, intellectual_property_protection_doctrine).
narrative_ontology:constraint_vindicates(software_source_status__property_rights_reading, market_based_software_distribution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control source code access, set licensing terms, enforce restrictions through legal and technical mechanisms (DRM, code signing, license verification). Derive revenue and strategic advantage from exclusive control of implementation details. Set and enforce the terms under which others may use, modify, or distribute software. Justify restrictions as protecting investment, maintaining quality control, and preventing unauthorized redistribution.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, proprietary_software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).

% Own legal title to source code and compiled artifacts. Collect licensing fees and maintain exclusive modification rights. Their interests are protected by international copyright treaties and patent frameworks. They benefit from the restriction mechanism without needing to enforce it directly in most cases.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, copyright_holders, beneficiary,
    powerful, generational, arbitrage, global).

% Build applications using proprietary libraries, frameworks, or SDKs. Must pay licensing fees, respect usage restrictions, and accept the vendor's upgrade schedule and breaking changes. Cannot inspect or modify source to fix bugs or add features critical to their own work. Face lock-in: switching costs are high because their entire architecture depends on the licensed software.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, dependent_developers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__property_rights_reading, dependent_developers, beneficiary).

% Use proprietary software under restrictive end-user license agreements (EULAs) that prohibit reverse engineering, modification, or redistribution. Cannot customize the software to their needs, extend it for their use case, or repair it independently. Their only recourse is to request features from the vendor, creating a dependency relationship.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, end_users_seeking_modification, payer,
    powerless, immediate, trapped, global).

% Face legal restrictions on reverse engineering and vulnerability disclosure. Cannot publish detailed exploit analysis, create tools to audit security, or fix vulnerabilities independently. Must work through vendor-controlled disclosure processes. Restriction creates asymmetric information: vendors know their own vulnerabilities; researchers cannot independently verify claimed fixes.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, security_researchers, payer,
    moderate, biographical, constrained, global).

% Operate under alternative licensing models that permit source inspection, modification, and redistribution. They are excluded from proprietary-enforced spaces by design: proprietary vendors explicitly prohibit mixing their code with open-source licensed code (GPL incompatibility clauses). This exclusion is maintained by licensing restrictions and legal enforcement.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, open_source_developers, excluded,
    moderate, biographical, mobile, global).

% Benefit from the complexity and necessity of licensing frameworks. Provide legal services for contract negotiation, compliance auditing, and enforcement litigation. Their professional authority and fees increase with the system's complexity.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, intellectual_property_attorneys, beneficiary,
    institutional, generational, arbitrage, global).

% Administer and enforce copyright and patent law. Can modify scope of protection (patent duration, DMCA exemptions, fair use doctrine). Their decisions shape the effective strength of the constraint.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, patent_offices_and_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__property_rights_reading, proprietary_software_vendors).
narrative_ontology:fixing_cost_class(software_source_status__property_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns incentives for software development by guaranteeing creators exclusive rights to their work, enabling them to recover investment costs, fund continued development, and maintain strategic advantage through exclusive access to implementation details. Theoretically solves the underproduction problem: without exclusive rights, creators have reduced incentive to invest in software creation.
% TRANSFER_FUNCTION: Moves licensing fees, per-seat charges, and subscription revenue from dependent developers and end users to proprietary vendors and copyright holders. Moves time and effort from security researchers and end users (who cannot verify or fix code) to vendors (who control all modifications). Moves strategic advantage in competing products to vendors with closed implementations.
% ABSENT_VOICES: Open-source developers and free-software advocates are structurally excluded by the licensing restrictions themselves and would argue that property-based software distribution is inefficient and unjust. Security researchers who face legal restrictions on disclosure would argue that source access is prerequisite to genuine security. Users in jurisdictions with weak enforcement (Global South) would argue the system preserves inequality of access.
% DISAPPEARANCE_RATIONALE: If proprietary licensing enforcement disappeared, software development would not cease but would rapidly shift toward open-source and collaborative models (as witnessed historically: Linux, Apache, Wikipedia). Vendor revenues would collapse; dependent developers and users would gain modification rights; security would improve through transparency. The constraint's persistence depends on active enforcement of copyright law and DRM technology.
% FOUNDING_PROBLEM: Early software was often copied freely; creators had no mechanism to fund ongoing development or recover their investment. The copyright system was extended to software to establish property rights and enable creators to charge for their work, theoretically funding innovation.
% FOUNDING_PROBLEM_CORROBORATION: Vendors argue the problem is still live: software development requires massive capital investment and proprietary protection is necessary to fund R&D. Open-source communities and academic researchers cite decades of evidence that collaborative, source-transparent development produces high-quality, secure software with lower total cost-of-ownership. Economic analysis from sources outside the vendor ecosystem shows mixed results: some software markets (enterprise databases, business suites) show vendor-funded innovation, while others (operating systems, web servers, cloud infrastructure) show open-source-led innovation with comparable or superior outcomes.
narrative_ontology:disappearance_verdict(software_source_status__property_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__property_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__property_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.68) because licensing fees, lock-in, and forced dependency on vendor upgrade schedules extract significant value from dependent users without tight coupling to marginal cost of serving them. Suppression is moderate (0.52) because the mechanism relies heavily on legal enforcement (copyright law, DRM prohibitions, DMCA) rather than purely technical barriers; exit is constrained but not completely trapped (open-source alternatives exist, though adoption costs are high). Theater is low-moderate (0.28): the 'investment protection' narrative is real (software development does require funding) but increasingly decoupled from actual enforcement (many core technologies are open-source; vendors increasingly adopt open-source foundations; licensing is often about market control rather than cost recovery). Temporal measurements show extraction rising steeply early (0-16 point interval) as digital markets grew and lock-in deepened, then plateauing (24-40 point interval) as open-source alternatives gained viability and regulatory pressure increased. Suppression requirement also plateaus: once legal frameworks are established, the active enforcement cost stabilizes.
 *
 * PERSPECTIVAL GAP:
 *   The property-rights reading asserts this constraint should compute as rope (genuine coordination for sustainable funding) from the vendor's seat. From the dependent-developer and security-researcher seats, the same structure computes as tangled rope or snare (enforced extraction riding on real coordination, but with asymmetric cost distribution and constrained alternatives). The engine computes seat divergence from the structural data: vendors are agenda-setters with arbitrage exit (beneficiary end of directionality), dependent developers and end users are targeted with constrained/trapped exit (target end). The difference in computed type reflects this structural asymmetry, which is analytically valid — the reading does not adjudicate which seat's type is 'correct', only that they differ.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary vendors occupy the beneficiary seat: they collect licensing fees, control modification rights, set upgrade paths, and benefit from lock-in without bearing the costs of customization or security vulnerability fixing by users. Their directionality d is near 0.0 (beneficiary end). Dependent developers and end users occupy the target seat: they pay licensing fees, face constraints on modification, accept vendor-imposed changes, and bear the costs of working around vendor limitations. Their directionality d is near 1.0 (target end). Security researchers are a mixed case: they benefit from the existence of software systems but are constrained by restriction enforcement. Intellectual property attorneys are beneficiaries: their professional value and fees increase with licensing system complexity. The property-rights reading itself assumes this distribution is legitimate (creators have property rights, users have contractual rights only); alternative readings would contest the beneficiary/victim labeling. This constraint story honors the property-rights framing without collapsing the structural asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (enabling cost recovery for software development) is contested. Vendors claim it is still live: without intellectual property protection, software companies cannot fund R&D. Outside observers (academic computer science, open-source projects, developing economies) increasingly argue the founding problem is dead: major software infrastructure (Linux, Apache, Kubernetes, TensorFlow) has demonstrated that high-quality, secure, economically-sustainable software can be developed collaboratively without proprietary protection. The question is whether the constraint persists because it still solves the founding problem or because the beneficiaries have captured the regulatory and legal frameworks. The 'diffuse accountability' omega below addresses this: if the founding problem is actually dead but vendors have successfully lobbied to maintain the constraint anyway, this would be mandatrophy (a constraint persisting without its justification). The theater_ratio measurements suggest increasing mandatrophy: the security/quality narrative (which was genuine in early eras) is increasingly performative as vendors adopt open-source internally and the constraint's main function becomes market gatekeeping rather than funding innovation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_analogy_boundary,
    'Are software source code and intellectual property sufficiently analogous to tangible property to justify the same restriction mechanisms, or is source code fundamentally different in ways that invalidate the analogy?',
    'Structural comparison: tangible property is rivalrous (use by one party excludes others) and exhaustible (consumption reduces availability); source code is non-rivalrous (reading/using does not prevent others from reading) and non-exhaustible (copying creates no scarcity). Empirical test: do the restriction mechanisms designed for tangible property (copyright, licensing, price-based allocation) actually solve the coordination problems they were designed for (underproduction, cost recovery) when applied to non-rivalrous goods?',
    'If source code is fundamentally non-rivalrous, property-based restriction mechanisms may be structurally inefficient — solving a rivalrous-goods problem via a non-rivalrous medium. The constraint might then compute as pure extraction (snare) rather than legitimate property protection (tangled rope). Alternatively, the non-rivalrous character might justify even stronger restrictions (to prevent perfect price competition), amplifying extraction upward.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(property_analogy_boundary, conceptual, 'Whether the tangible-property analogy holds for non-rivalrous goods.').

omega_variable(
    funding_mechanism_necessity,
    'Is proprietary licensing necessary to fund software development and innovation, or can high-quality software be sustainably developed under alternative funding models (open source, public funding, volunteer communities)?',
    'Empirical: compare software quality, security, innovation velocity, and cost-of-ownership across proprietary and open-source software in the same domain (operating systems, web servers, databases, cloud infrastructure, mobile development frameworks). Inspect funding sources for open-source projects (tech company R&D allocation, cloud service revenue, volunteer labor, public funding).',
    'If open-source software demonstrates equal or superior outcomes with alternative funding mechanisms, the property-rights founding problem is partially or wholly solved by non-proprietary means. This would reclassify the constraint from tangled rope (real coordination need + asymmetric extraction) toward snare (extraction without genuine coordination necessity) or scaffold (coordination was necessary in the 1980s-2000s but is now unnecessary). The theater_ratio would rise as the ''investment protection'' narrative becomes increasingly performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(funding_mechanism_necessity, empirical, 'Whether proprietary licensing is necessary for sustainable software development.').

omega_variable(
    restriction_enforceability_drift,
    'As technological barriers to enforcement decrease (cryptographic access control, DRM, code signing) while social barriers increase (global open-source adoption, security-through-transparency norms, antitrust enforcement), what is the effective enforcement cost and viability of licensing restrictions?',
    'Time-series analysis of enforcement attempts: measure frequency and cost of legal actions (DMCA takedowns, GPL enforcement, patent litigation); measure adoption of enforcement technologies (code signing, DRM, license verification); measure successful circumvention (reverse engineering, open-source reimplementations); survey developer perception of restriction legitimacy across cohorts (older vendors vs. startups vs. research communities).',
    'If enforcement cost rises while social legitimacy declines, the constraint may become increasingly piton-like (persists through inertia and legal frameworks rather than stakeholder acceptance). The suppression metric would show rising disconnection between legal enforcement and practical compliance, indicating growing theater. This would trigger mandatrophy analysis: a constraint whose enforcement burden exceeds its coordination benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restriction_enforceability_drift, empirical, 'Whether restriction enforcement remains viable under changing technological and social conditions.').

omega_variable(
    kernel_reading_contention,
    'Which kernel reading — property-rights, freedom-imperative, pragmatic-development, or utilitarian-hybrid — best describes actual regulatory and industry practice?',
    'Analysis of regulatory decisions (European software interoperability directives, DMCA exemptions, patent office guidance), vendor behavior (adoption of open-source components, open-source foundations, dual licensing), and legislative intent (antitrust enforcement, copyright reform debates). Check whether one reading''s axioms have been formally adopted or rejected in law/policy.',
    'The property-rights reading assumes source code IS property with legitimate restriction rights. If regulatory and industry practice increasingly adopt the utilitarian-hybrid or pragmatic-development readings (treating licensing as context-dependent, favoring open-source in some domains), the property-rights reading becomes a ''holdable but contested'' axiom rather than the default framework. This would shift the reading''s axiom.status from holdable to merely defended, and inform the drift_state analysis of the CS structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contention, conceptual, 'Which kernel reading is instantiated by actual regulatory and industry practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__property_rights_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__property_rights_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(soft_tr_t0, projected).
narrative_ontology:measurement(soft_tr_t8, software_source_status__property_rights_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement_basis(soft_tr_t8, observed).
narrative_ontology:measurement(soft_tr_t16, software_source_status__property_rights_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement_basis(soft_tr_t16, observed).
narrative_ontology:measurement(soft_tr_t24, software_source_status__property_rights_reading, theater_ratio, 24, 0.25).
narrative_ontology:measurement_basis(soft_tr_t24, observed).
narrative_ontology:measurement(soft_tr_t32, software_source_status__property_rights_reading, theater_ratio, 32, 0.27).
narrative_ontology:measurement_basis(soft_tr_t32, observed).
narrative_ontology:measurement(soft_tr_t40, software_source_status__property_rights_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(soft_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__property_rights_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(soft_be_t0, projected).
narrative_ontology:measurement(soft_be_t8, software_source_status__property_rights_reading, base_extractiveness, 8, 0.51).
narrative_ontology:measurement_basis(soft_be_t8, observed).
narrative_ontology:measurement(soft_be_t16, software_source_status__property_rights_reading, base_extractiveness, 16, 0.59).
narrative_ontology:measurement_basis(soft_be_t16, observed).
narrative_ontology:measurement(soft_be_t24, software_source_status__property_rights_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement_basis(soft_be_t24, observed).
narrative_ontology:measurement(soft_be_t32, software_source_status__property_rights_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement_basis(soft_be_t32, observed).
narrative_ontology:measurement(soft_be_t40, software_source_status__property_rights_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(soft_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_source_status__property_rights_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(soft_su_t0, projected).
narrative_ontology:measurement(soft_su_t8, software_source_status__property_rights_reading, suppression_requirement, 8, 0.41).
narrative_ontology:measurement_basis(soft_su_t8, observed).
narrative_ontology:measurement(soft_su_t16, software_source_status__property_rights_reading, suppression_requirement, 16, 0.46).
narrative_ontology:measurement_basis(soft_su_t16, observed).
narrative_ontology:measurement(soft_su_t24, software_source_status__property_rights_reading, suppression_requirement, 24, 0.5).
narrative_ontology:measurement_basis(soft_su_t24, observed).
narrative_ontology:measurement(soft_su_t32, software_source_status__property_rights_reading, suppression_requirement, 32, 0.51).
narrative_ontology:measurement_basis(soft_su_t32, observed).
narrative_ontology:measurement(soft_su_t40, software_source_status__property_rights_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(soft_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__property_rights_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_source_status__property_rights_reading, 0.18).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_source_status__utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% The software_source_status kernel decomposes into four constraint stories, each instantiating a different reading with distinct ε values and beneficiary/victim structures. The property_rights_reading (this story) frames source code as property and licensing restrictions as legitimate; extractiveness is high (0.68) but justified by coordination function (funding development). The freedom_imperative_reading frames property restrictions as unjust suppression; extractiveness is even higher (~0.80+) with no coordination defense. The pragmatic_development_reading focuses on development outcomes; extractiveness is moderate-to-high (~0.55-0.65) but with open-source as viable alternative. The utilitarian_hybrid_reading permits both models; extractiveness varies by context. All four stories reference the same standing arrangement (proprietary licensing with restriction enforcement) but assess it through different normative frameworks. The kernel question is: what is the legitimate status of source code? The four readings give incompatible answers, none of which is derivable from the facts alone — the disagreement is located in the foundational axioms (property rights vs. human rights vs. development methodology vs. aggregate welfare). This family decomposition honors the ε-invariance principle: each reading gets its own stable ε (not a parameter of observer choice, but grounded in the reading's own axioms about what counts as extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
