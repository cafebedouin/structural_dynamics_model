% ============================================================================
% CONSTRAINT STORY: software_source_status__property_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   human_readable: Proprietary Software Licensing — Property Rights Reading
 *   domain: economic/technological/legal
 *
 * SUMMARY:
 *   The property_rights_reading of the software_source_status kernel asserts
 *   that source code is a proprietary asset and that licensing restrictions
 *   are legitimate exercises of ownership. Users are consumers with
 *   contractual rights only. This reading instantiates a constraint where
 *   software creators and companies (beneficiaries) control distribution,
 *   modification, and use through copyright law and license enforcement,
 *   while users, independent developers, educators, and researchers (victims)
 *   bear the costs of restricted access. The constraint has a genuine
 *   coordination function — IP protection funds commercial software
 *   development — but also asymmetric extraction: restrictions enable
 *   rent-seeking, vendor lock-in, and suppression of competitive/educational
 *   uses. Active enforcement (copyright, DRM, EULAs, trade secrecy) is
 *   required to maintain the restriction regime. The measurement series (T=0
 *   to T=50, roughly 1970s–2020s) shows rising extractiveness as software
 *   became pervasive, theater ratio increasing as 'IP protection' rhetoric
 *   expands beyond its coordination function, and suppression requirement
 *   growing with DRM and legal enforcement machinery.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__property_rights_reading, 0.65).
domain_priors:suppression_score(software_source_status__property_rights_reading, 0.55).
domain_priors:theater_ratio(software_source_status__property_rights_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__property_rights_reading, tangled_rope).
narrative_ontology:human_readable(software_source_status__property_rights_reading, "Proprietary Software Licensing — Property Rights Reading").
narrative_ontology:topic_domain(software_source_status__property_rights_reading, "economic/technological/legal").

domain_priors:requires_active_enforcement(software_source_status__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__property_rights_reading, '1fcb3fa8-77ba-4356-b14d-6cca0f2c6df7').
narrative_ontology:cs_kernel_codification('1fcb3fa8-77ba-4356-b14d-6cca0f2c6df7', formalized).
narrative_ontology:cs_authority_grounding('1fcb3fa8-77ba-4356-b14d-6cca0f2c6df7', lineage).
narrative_ontology:cs_interpretation_layer_present('1fcb3fa8-77ba-4356-b14d-6cca0f2c6df7').
narrative_ontology:cs_reading_relation('1fcb3fa8-77ba-4356-b14d-6cca0f2c6df7', software_source_status__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('1fcb3fa8-77ba-4356-b14d-6cca0f2c6df7', software_source_status__pragmatic_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('1fcb3fa8-77ba-4356-b14d-6cca0f2c6df7', software_source_status__utilitarian_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('1fcb3fa8-77ba-4356-b14d-6cca0f2c6df7', foundational, creator_owns_source_code).
narrative_ontology:cs_axiom_status(creator_owns_source_code, holdable).
narrative_ontology:cs_axiom_grounding('1fcb3fa8-77ba-4356-b14d-6cca0f2c6df7', creator_owns_source_code, deontological).
narrative_ontology:cs_axiom('1fcb3fa8-77ba-4356-b14d-6cca0f2c6df7', foundational, licensing_restrictions_are_legitimate_exercise).
narrative_ontology:cs_axiom_status(licensing_restrictions_are_legitimate_exercise, holdable).
narrative_ontology:cs_axiom_grounding('1fcb3fa8-77ba-4356-b14d-6cca0f2c6df7', licensing_restrictions_are_legitimate_exercise, conventional).
narrative_ontology:cs_axiom('1fcb3fa8-77ba-4356-b14d-6cca0f2c6df7', secondary, intellectual_property_incentivizes_innovation).
narrative_ontology:cs_axiom_status(intellectual_property_incentivizes_innovation, holdable).
narrative_ontology:cs_axiom_grounding('1fcb3fa8-77ba-4356-b14d-6cca0f2c6df7', intellectual_property_incentivizes_innovation, instrumental).
narrative_ontology:cs_reference_frame('1fcb3fa8-77ba-4356-b14d-6cca0f2c6df7', proprietary_software_legitimacy).
narrative_ontology:cs_drift_state('1fcb3fa8-77ba-4356-b14d-6cca0f2c6df7', contemporary_open_source_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1fcb3fa8-77ba-4356-b14d-6cca0f2c6df7', '').
narrative_ontology:cs_kernel_id(software_source_status__property_rights_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, software_creators).
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, proprietary_software_companies).
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, ip_holding_entities).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, software_users).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, independent_developers).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, educational_institutions).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, security_researchers).
narrative_ontology:constraint_vindicates(software_source_status__property_rights_reading, creator_owns_source_code).
narrative_ontology:constraint_vindicates(software_source_status__property_rights_reading, licensing_restrictions_are_legitimate_exercise).
narrative_ontology:constraint_vindicates(software_source_status__property_rights_reading, intellectual_property_incentivizes_innovation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individual developers and small teams who create software and choose proprietary licensing. They set license terms, control distribution, and collect revenue directly. They can exit to open source or employment, but proprietary licensing is their chosen business model.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, software_creators, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__property_rights_reading, software_creators, beneficiary).

% Large vendors (Microsoft, Oracle, Adobe, etc.) whose business models depend on proprietary licensing. They lobby for stronger IP law, build enforcement machinery (DRM, audit programs), and shape the legal framework. They collect the vast majority of license revenue and control ecosystem standards.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, proprietary_software_companies, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__property_rights_reading, proprietary_software_companies, beneficiary).

% Patent assertion entities, copyright trolls, and IP licensing firms that extract value without producing software. They benefit from the property-rights regime by monetizing litigation threat and portfolio licensing. They are mobile — they move across jurisdictions and portfolios.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, ip_holding_entities, beneficiary,
    organized, biographical, mobile, global).

% Enterprises and individuals who pay license fees, accept EULAs, submit to audits, and cannot modify or repair software. Switching costs (data lock-in, training, integration) make exit costly. They bear the extraction directly through recurring payments and indirectly through constrained workflows.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, software_users, payer,
    organized, biographical, constrained, global).

% Developers who want to build on, extend, or interoperate with proprietary platforms but face API restrictions, license fees, and legal threats. They pay through platform commissions, restricted APIs, and reimplementing functionality. Exit means abandoning the platform's user base.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, independent_developers, payer,
    moderate, biographical, constrained, global).

% Universities and schools that need source access for teaching and research but face restrictive academic licenses, audit risk, and vendor lock-in. Their mission (education, knowledge dissemination) is identity-fused with software freedom; they cannot fully exit without compromising their educational role.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, educational_institutions, payer,
    organized, generational, identity_locked, national).

% Researchers who need to audit proprietary code for vulnerabilities but face DMCA anti-circumvention liability, EULA bans on reverse engineering, and vendor retaliation. They are trapped: their work requires accessing the restricted artifact, but the constraint criminalizes that access.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, security_researchers, payer,
    moderate, biographical, trapped, global).

% Organizations and individuals (FSF, EFF, copyleft proponents) who argue software freedom is an ethical imperative. They are structurally excluded from the proprietary licensing conversation — their objections are treated as ideological, not commercial. They build alternative ecosystems (GNU/Linux, copyleft licenses) but cannot participate in setting proprietary terms.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, free_software_advocates, excluded,
    organized, generational, arbitrage, global).

% Courts and legislatures that interpret and enforce copyright, patent, and contract law as applied to software. They adjudicate disputes between beneficiaries and payers, set precedents on fair use/reverse engineering, and can modify the constraint's enforcement boundary. They see the full structure but operate within the property-rights framework.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, legal_system_courts, observer,
    institutional, generational, analytical, national).

% Companies offering competing proprietary or open-source alternatives. They are excluded from interoperating with dominant proprietary platforms (API access, file formats, protocols). They would compete on features/price if admitted; their exclusion is maintained by the same IP enforcement that protects the incumbents.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, competitors_alternative_vendors, excluded,
    powerful, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables commercial software development by giving creators exclusive control over copying and distribution, allowing them to charge for licenses and recoup investment. Solves the public-goods problem of software: without exclusion, free-riding would underfund development.
% TRANSFER_FUNCTION: Moves license fees, subscription revenue, and control over software evolution from users and dependent developers to proprietary software companies and creators. Also moves legal risk (audit liability, infringement exposure) onto users.
% ABSENT_VOICES: Free software advocates, security researchers, and users in Global South jurisdictions with weaker IP enforcement are structurally excluded. They would argue for user freedom, right to repair, and access to knowledge, but the proprietary licensing framework treats them as non-participants (pirates, ideologues, or irrelevant markets).
% DISAPPEARANCE_RATIONALE: If proprietary licensing and its enforcement vanished overnight, the software economy would reorganize around service models, open source, and public funding within years. Companies would shift to support/hosting revenue; users would gain modification rights; independent developers would build on formerly closed platforms. The world rearranges because the constraint actively shapes the industrial structure.
% FOUNDING_PROBLEM: Early commercial software (1970s–1980s) faced rampant copying with no legal framework to protect investment. Companies like Microsoft argued that without copyright protection for software, no one would fund professional development. The constraint was built to solve the public-goods problem of software by creating artificial scarcity through IP law.
% FOUNDING_PROBLEM_CORROBORATION: Proprietary vendors (BSA, Microsoft, Oracle) attest the problem is live, citing ongoing piracy and need for R&D funding. Open source advocates (FSF, Linux Foundation), economic researchers (e.g., Lerner & Tirole on open source motivation), and EU competition authorities attest the problem is substantially solved by alternative models (open source, SaaS, public funding) and the constraint persists as rent extraction. Independent academic studies corroborate the shifted-function reading.
narrative_ontology:disappearance_verdict(software_source_status__property_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__property_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__property_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_source_status__property_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__property_rights_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.65) reflects that users surrender modification rights, pay recurring license fees, and accept audit/compliance burdens — costs that exceed the marginal cost of software distribution. Suppression (0.55) is structural: copyright law, DMCA anti-circumvention, contract law, and technical measures (DRM, license keys) actively prevent alternatives. Theater ratio (0.25) is moderate: the coordination function (funding development) is real but a growing share of enforcement protects business models (SaaS lock-in, planned obsolescence) rather than funding innovation. Accessibility collapse (0.45) is partial: open source alternatives exist but are excluded from many commercial/enterprise contexts. Resistance (0.48) is significant: open source movement, right-to-repair, security research exemptions, and regulatory scrutiny (EU Software Resilience Act) all push back.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (software companies), the constraint is a rope — it coordinates investment, quality assurance, and sustainable development. From the payer seats (users, independent developers, educators), it computes as snare/tangled_rope — the coordination story covers extraction (vendor lock-in, rent-seeking, suppression of repair/research). The engine computes this divergence from the structural data; the claimed_type (tangled_rope) acknowledges both functions exist simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Software creators and proprietary companies are structural beneficiaries (d near 0.0–0.2): they collect license revenue, control roadmap, and enforce terms. Users are primary targets (d near 0.8–0.9): they pay, cannot modify, face audit risk, and exit is constrained by switching costs and ecosystem lock-in. Independent developers are payers (d ~0.7): they cannot build on proprietary foundations, must license or reimplement. Educational institutions and security researchers are payers with identity_locked exit (d ~0.75): their mission requires source access but the constraint denies it. The legal system sits at analytical (d=0.5) but its enforcement choices tilt toward beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (funding software development via IP monopoly) is contested: beneficiaries claim it's still live; critics argue open source and service models have solved it. The constraint persists despite alternative funding models (open source, SaaS, public funding) because the beneficiary coalition (large proprietary vendors) controls the legal framework. This is not pure mandatrophy — the coordination function still operates for some beneficiaries — but extraction has accumulated beyond the coordination floor.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine natural-law property right or a constructed legal arrangement that benefits identifiable agents?',
    'Compare historical contingency of software IP law across jurisdictions; examine whether the ''property'' framing predates or follows commercial software industry formation.',
    'If constructed, the constraint is a false-summit candidate (tangled_rope masquerading as mountain via property rhetoric); if natural-law, the beneficiary structure is incidental to a genuine mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Natural-law vs. constructed status of software property rights').

omega_variable(
    coordination_extraction_boundary,
    'Is the coordination function (funding software development) structurally inseparable from the extraction function (restricting user freedom, enabling rent-seeking), or can they be separated?',
    'Natural experiment: jurisdictions with compulsory licensing or strong fair-use exceptions for software — if development funding persists while restrictions relax, functions are separable.',
    'If separable, the extraction is avoidable overhead on a real coordination function; if inseparable, the property-rights reading''s claim that restriction is necessary for coordination is structurally validated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether IP restriction is necessary for software development funding').

omega_variable(
    sibling_reading_delta,
    'How does the property_rights_reading''s structural claim (source code as proprietary asset) differ from the freedom_imperative_reading (software freedom as ethical requirement) and pragmatic_development_reading (open source as superior methodology) in beneficiary/victim structure?',
    'Map beneficiary/victim sets across all four readings; identify which agents change role (beneficiary↔payer) and which exit_options shift when the kernel is read differently.',
    'If the same agents flip roles across readings, the kernel is a genuine contested commitment; if different agents are implicated, the readings may describe different constraints entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_delta, conceptual, 'Structural delta between this reading and its sibling readings of the software_source_status kernel').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (copyright law, DRM, contract enforcement) or internalized (users accept restrictions as normal, developers self-censor)?',
    'Post-reform suppression trajectory: if restrictions are legally relaxed but user/developer behavior doesn''t change, internalized component is significant.',
    'If internalized, effective suppression exceeds structural measure; the constraint persists partly through cognitive capture, not just law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in proprietary software licensing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__property_rights_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(software_source_status__property_rights_reading_tr_t0, software_source_status__property_rights_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(software_source_status__property_rights_reading_tr_t0, observed).
narrative_ontology:measurement(software_source_status__property_rights_reading_tr_t10, software_source_status__property_rights_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(software_source_status__property_rights_reading_tr_t10, observed).
narrative_ontology:measurement(software_source_status__property_rights_reading_tr_t20, software_source_status__property_rights_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(software_source_status__property_rights_reading_tr_t20, observed).
narrative_ontology:measurement(software_source_status__property_rights_reading_tr_t30, software_source_status__property_rights_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement_basis(software_source_status__property_rights_reading_tr_t30, observed).
narrative_ontology:measurement(software_source_status__property_rights_reading_tr_t40, software_source_status__property_rights_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement_basis(software_source_status__property_rights_reading_tr_t40, observed).
narrative_ontology:measurement(software_source_status__property_rights_reading_tr_t50, software_source_status__property_rights_reading, theater_ratio, 50, 0.25).
narrative_ontology:measurement_basis(software_source_status__property_rights_reading_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(software_source_status__property_rights_reading_be_t0, software_source_status__property_rights_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(software_source_status__property_rights_reading_be_t0, observed).
narrative_ontology:measurement(software_source_status__property_rights_reading_be_t10, software_source_status__property_rights_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(software_source_status__property_rights_reading_be_t10, observed).
narrative_ontology:measurement(software_source_status__property_rights_reading_be_t20, software_source_status__property_rights_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(software_source_status__property_rights_reading_be_t20, observed).
narrative_ontology:measurement(software_source_status__property_rights_reading_be_t30, software_source_status__property_rights_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(software_source_status__property_rights_reading_be_t30, observed).
narrative_ontology:measurement(software_source_status__property_rights_reading_be_t40, software_source_status__property_rights_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement_basis(software_source_status__property_rights_reading_be_t40, observed).
narrative_ontology:measurement(software_source_status__property_rights_reading_be_t50, software_source_status__property_rights_reading, base_extractiveness, 50, 0.65).
narrative_ontology:measurement_basis(software_source_status__property_rights_reading_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(software_source_status__property_rights_reading_su_t0, software_source_status__property_rights_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(software_source_status__property_rights_reading_su_t0, observed).
narrative_ontology:measurement(software_source_status__property_rights_reading_su_t10, software_source_status__property_rights_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement_basis(software_source_status__property_rights_reading_su_t10, observed).
narrative_ontology:measurement(software_source_status__property_rights_reading_su_t20, software_source_status__property_rights_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement_basis(software_source_status__property_rights_reading_su_t20, observed).
narrative_ontology:measurement(software_source_status__property_rights_reading_su_t30, software_source_status__property_rights_reading, suppression_requirement, 30, 0.53).
narrative_ontology:measurement_basis(software_source_status__property_rights_reading_su_t30, observed).
narrative_ontology:measurement(software_source_status__property_rights_reading_su_t40, software_source_status__property_rights_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement_basis(software_source_status__property_rights_reading_su_t40, observed).
narrative_ontology:measurement(software_source_status__property_rights_reading_su_t50, software_source_status__property_rights_reading, suppression_requirement, 50, 0.55).
narrative_ontology:measurement_basis(software_source_status__property_rights_reading_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__property_rights_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_source_status__property_rights_reading, 0.15).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_source_status__utilitarian_hybrid_reading).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, dmca_anticircumvention).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_patent_regime).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, saas_vendor_lockin).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the software_source_status kernel. The property_rights_reading and freedom_imperative_reading have opposed beneficiary/victim structures; pragmatic_development_reading and utilitarian_hybrid_reading occupy intermediate positions. All four are linked via affects_constraints. The ε values differ substantially: property_rights_reading ε≈0.65 (extraction visible), freedom_imperative_reading ε≈0.15 (coordination visible), pragmatic_development_reading ε≈0.25, utilitarian_hybrid_reading ε≈0.35.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(software_source_status__property_rights_reading, institutional, 0.15).
constraint_indexing:directionality_override(software_source_status__property_rights_reading, organized, 0.75).
constraint_indexing:directionality_override(software_source_status__property_rights_reading, moderate, 0.7).
constraint_indexing:directionality_override(software_source_status__property_rights_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
