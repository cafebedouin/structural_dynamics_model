% ============================================================================
% CONSTRAINT STORY: software_source_status__pragmatic_development_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Open Source Development Methodology Norm (Pragmatic Reading)
 *   domain: software_engineering/political_economy_of_technology
 *
 * SUMMARY:
 *   This constraint is ONE READING of the contested kernel
 *   'software_source_status.' The pragmatic-development reading frames open
 *   source as methodologically superior because peer review, transparency,
 *   and collaborative problem-solving produce higher quality, faster security
 *   response, and accelerated innovation velocity. It is distinct from the
 *   freedom-imperative reading (which locates value in ethics) and the
 *   property-rights reading (which defends creators' entitlement to
 *   restrict). This reading accepts permissive licensing and acknowledges
 *   that proprietary software is legitimate in contexts where closed
 *   development serves stakeholder interests well. The measurement series
 *   reflects the constraint's actual operation: relatively low extraction
 *   (0.38 at interval end) because the norm produces genuine coordination
 *   benefits and carries low active suppression (0.22), though proprietary
 *   vendors do bear a structural cost (legitimacy pressure). The theater
 *   ratio (0.18) reflects that implementation includes some performative
 *   elements (companies marketing open source credentials while maintaining
 *   proprietary core products) but remains grounded in real technical
 *   practices.
 *
 * KEY AGENTS:
 *   - open_source_developers: Participate in visible, modifiable codebases; benefit from peer reputation and low gatekeeping barriers
 *   - software_users: Depend on the peer-review function and transparency for security assurance and rapid bug fixes
 *   - downstream_integrators: Benefit from being able to inspect, modify, and integrate source code without vendor gatekeeping
 *   - proprietary_software_vendors: Bear structural cost of legitimacy pressure and reduced market segments where closure is defensible
 *   - corporate_open_source_programs: Dual-positioned beneficiaries and payers — capture innovation velocity and peer review while maintaining proprietary portfolios under legitimacy pressure
 *   - software_freedom_activists: Excluded from the core claim; would object to reduction of freedom to instrumentality
 *   - IP scholars and independent security researchers: Analytical observers who can measure whether quality and velocity claims hold empirically
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__pragmatic_development_reading, 0.38).
domain_priors:suppression_score(software_source_status__pragmatic_development_reading, 0.22).
domain_priors:theater_ratio(software_source_status__pragmatic_development_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__pragmatic_development_reading, rope).
narrative_ontology:human_readable(software_source_status__pragmatic_development_reading, "Open Source Development Methodology Norm (Pragmatic Reading)").
narrative_ontology:topic_domain(software_source_status__pragmatic_development_reading, "software_engineering/political_economy_of_technology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__pragmatic_development_reading, 'd64b1397-c2b5-42f8-97b8-d2d8c89a1fca').
narrative_ontology:cs_kernel_codification('d64b1397-c2b5-42f8-97b8-d2d8c89a1fca', distributed).
narrative_ontology:cs_authority_grounding('d64b1397-c2b5-42f8-97b8-d2d8c89a1fca', distributed).
narrative_ontology:cs_reading_relation('d64b1397-c2b5-42f8-97b8-d2d8c89a1fca', software_source_status__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('d64b1397-c2b5-42f8-97b8-d2d8c89a1fca', software_source_status__property_rights_reading, influences).
narrative_ontology:cs_reading_relation('d64b1397-c2b5-42f8-97b8-d2d8c89a1fca', software_source_status__utilitarian_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('d64b1397-c2b5-42f8-97b8-d2d8c89a1fca', foundational, transparency_and_collaboration_methodology_superior).
narrative_ontology:cs_axiom_status(transparency_and_collaboration_methodology_superior, holdable).
narrative_ontology:cs_axiom_grounding('d64b1397-c2b5-42f8-97b8-d2d8c89a1fca', transparency_and_collaboration_methodology_superior, empirically_contingent).
narrative_ontology:cs_axiom('d64b1397-c2b5-42f8-97b8-d2d8c89a1fca', foundational, freedom_instrumental_to_quality_not_foundational_right).
narrative_ontology:cs_axiom_status(freedom_instrumental_to_quality_not_foundational_right, holdable).
narrative_ontology:cs_axiom_grounding('d64b1397-c2b5-42f8-97b8-d2d8c89a1fca', freedom_instrumental_to_quality_not_foundational_right, instrumental).
narrative_ontology:cs_reference_frame('d64b1397-c2b5-42f8-97b8-d2d8c89a1fca', development_methodology_as_legitimacy_ground).
narrative_ontology:cs_drift_state('d64b1397-c2b5-42f8-97b8-d2d8c89a1fca', contemporary_corporate_capture_phase, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d64b1397-c2b5-42f8-97b8-d2d8c89a1fca', '').
narrative_ontology:cs_kernel_id(software_source_status__pragmatic_development_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, open_source_developers).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, software_users).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, downstream_integrators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, corporate_open_source_programs).
narrative_ontology:constraint_victim(software_source_status__pragmatic_development_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(software_source_status__pragmatic_development_reading, corporate_open_source_programs).
narrative_ontology:constraint_vindicates(software_source_status__pragmatic_development_reading, peer_review_improves_quality).
narrative_ontology:constraint_vindicates(software_source_status__pragmatic_development_reading, transparency_reduces_bugs).
narrative_ontology:constraint_vindicates(software_source_status__pragmatic_development_reading, collaborative_velocity_exceeds_closed_iteration).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in projects where source code is visible and modifiable by peers. They benefit from collective problem-solving, reputation accumulation within technical communities, and the ability to fork or contribute to projects without gatekeeping. Entry barriers are low; they can build career capital and influence within open ecosystems.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, open_source_developers, beneficiary,
    moderate, generational, mobile, global).

% Depend on software tools and libraries for productivity and infrastructure. Under this reading they benefit from the transparency of open source: they can audit code for security, suggest fixes, and rely on distributed communities to catch problems faster than closed vendor support cycles. Exit is partly constrained by ecosystem lock-in, but choice between open and proprietary tools remains available.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, software_users, beneficiary,
    organized, biographical, constrained, global).

% Build products and services that depend on open source foundations. They benefit from being able to inspect, modify, and integrate source code without license restrictions; they also depend on the peer-review function to ensure quality and security of upstream projects. Their ability to capture value is not constrained by source access.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, downstream_integrators, beneficiary,
    moderate, generational, mobile, global).

% Defend closed development models and proprietary licensing as legitimate. Under this reading they bear a structural cost: the norm that open source is methodologically superior creates institutional pressure to justify closed development, exposes proprietary software to legitimacy challenges, and narrows the market segments where closed source dominates. Their exit is constrained because abandoning proprietary models means abandoning their business model, yet the norm continuously requires them to defend the legitimacy of closure.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, proprietary_software_vendors, payer,
    institutional, generational, constrained, global).

% Large corporations that maintain both proprietary and open source portfolios. They benefit from the peer review and innovation velocity gains from open projects while maintaining proprietary offerings for strategic assets. They also incur costs: funding and maintaining open source codebases, managing dual licensing complexity, and navigating the norm that openness is superior (which can constrain their ability to monetize proprietary components).
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, corporate_open_source_programs, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__pragmatic_development_reading, corporate_open_source_programs, payer).

% Advocate for software freedom as a fundamental ethical right, not merely an instrumental development advantage. This reading (pragmatic/developmental) does not extend to the freedom-imperative framing; it accepts permissive licensing and proprietary software as legitimate, locating the value of openness in quality and velocity rather than ethics. Activists in the freedom tradition are excluded from the core claim; they would object to the reduction of freedom to instrumentality.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, software_freedom_activists, excluded,
    moderate, generational, trapped, global).

% Analyze the kernel from multiple readings: IP rights doctrine, welfare economics, pragmatic software engineering. They observe the contested kernel and the different readings claiming different legitimacy grounds. From this seat they can measure whether claims about peer review quality and innovation velocity hold empirically, and whether the constraint's persistence depends on evidence or on institutional capture.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, intellectual_property_scholars, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__pragmatic_development_reading, open_source_developers).
narrative_ontology:fixing_cost_class(software_source_status__pragmatic_development_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of distributed quality assurance and rapid iteration in software development: peer visibility and contribution channels reduce latency between bug detection and fix, enable diverse security audits, and accelerate innovation by allowing downstream reuse and modification without vendor gatekeeping.
% TRANSFER_FUNCTION: Transfers time and attention from proprietary software vendors to open source ecosystems, and from proprietary licensing regimes to commons-based models. Proprietary vendors lose the ability to gatekeep modifications and capture all derivative value; open ecosystems gain the pool of contributions, audit capacity, and downstream innovation velocity.
% ABSENT_VOICES: Software freedom activists arguing that the constraint instrumentalizes freedom rather than recognizing it as a fundamental right; property-rights advocates arguing that creators have legitimate moral entitlement to restrict access and modification; and utilitarian framings that would locate legitimacy in context-specific welfare maximization rather than methodological superiority.
% DISAPPEARANCE_RATIONALE: If this norm disappeared overnight (i.e., if open source development were no longer regarded as methodologically superior), institutional support for open projects would shift: corporate sponsorship would decline absent legitimacy pressure, peer-review volunteer communities would shrink, and the relative investment in proprietary development would rise. Market structure would reorganize around licensing models chosen purely on business utility rather than normative commitment. Software quality outcomes would measurably change (or not), enabling empirical testing of the coordination claim.
% FOUNDING_PROBLEM: Late 1980s-1990s: software development was siloed in vendor proprietary shops; bug reports traveled slowly through support channels; security vulnerabilities were discovered by vendors, competitors, or attackers with no distributed peer auditing; innovation velocity was constrained by single vendor's development cycle; downstream integrators could not modify components to suit their needs.
% FOUNDING_PROBLEM_CORROBORATION: Security researchers, open source maintainers, and technology economists outside the benefiting parties attest that peer review catches vulnerabilities faster than closed vendor support and that transparent codebases enable faster innovation cycles. Comparative empirical studies (e.g., Linux security incident response vs. proprietary OS vendors, Apache vs. commercial web server velocity) from independent sources support the founding problem's continued salience. No dissent from these parties on the empirical claim — disagreement exists on whether the coordination benefit justifies the claimed reading's instrumental reduction of freedom, and on whether proprietary models might serve different contexts better.
narrative_ontology:disappearance_verdict(software_source_status__pragmatic_development_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__pragmatic_development_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__pragmatic_development_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_source_status__pragmatic_development_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__pragmatic_development_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   The constraint sits between rope (genuine coordination function that benefits users and developers) and tangled rope (proprietary vendors face structural cost and suppression of their preferred business model). Extractiveness is moderate (0.38) because the coordination genuinely produces value — the benefits are not fictive. The beneficiary/victim split is asymmetric: open source developers, users, and downstream integrators benefit from the norm without bearing comparable costs; proprietary vendors pay (legitimacy pressure, market shrinkage, pressure to justify closure). Suppression is low (0.22) because the norm operates primarily through institutional legitimacy and technical choice rather than legal coercion or active interdiction — vendors can and do maintain proprietary software, though under pressure. Theater is modest (0.18): some corporate open source programs are partly performative (marketing open credentials while core revenue remains proprietary), but the underlying technical claim (peer review improves outcomes) remains substantive and tested. The measurement trajectory shows gentle increase in extraction and theater through the interval with stabilization after t=20, reflecting the norm maturing and vendor defensive positioning settling into sustainable equilibrium.
 *
 * PERSPECTIVAL GAP:
 *   From the open source developer and user seats, this constraint is enabling coordination with minimal downside — it expands their agency and technical choices. From the proprietary vendor seat, it is delegitimizing and extractive, narrowing the contexts where their business model is defensible. The agenda-setter seat is distributed across open source communities, corporate open source programs, and academic computer science — no single powerful agent controls the norm's evolution. The engine should compute the vendor seat as experiencing higher effective extraction (d closer to 1.0) because they are the structural target whose choices are being constrained, while developer and user seats should compute lower d (closer to 0.0-0.3) because they are being enabled rather than restricted.
 *
 * DIRECTIONALITY LOGIC:
 *   Open source developers: beneficiary, moderate power, mobile exit (can fork projects, start new ones, work across organizations) — derives d ≈ 0.2-0.3 (low target pressure, high benefit). Software users: organized power, constrained exit (lock-in to ecosystems) but choice between open/proprietary tools available — derives d ≈ 0.4-0.5 (mild asymmetry; users benefit from norm but are not the norm's primary focus). Downstream integrators: moderate power, mobile exit — derives d ≈ 0.2-0.3 (beneficiaries of code reuse freedom). Proprietary vendors: institutional power, but constrained exit (cannot abandon business model without existential change) and identity-locked (corporate identity constituted around proprietary software) — derives d ≈ 0.6-0.7 (structural targets of the norm, even though no legal coercion exists). Corporate open source programs: powerful institutional actors, mobile exit in principle (can shift portfolios) but identity-locked in practice to both models — derives d ≈ 0.45-0.55 (genuinely dual-positioned; the secondary payer role reflects the legitimacy burden they carry for maintaining proprietary components).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (slow vendor support, siloed innovation, downstream gatekeeping) was live and pressing in the 1980s-1990s. By 2025, the problem's status is contested: open source communities and security researchers attest it remains live (vulnerabilities still discovered slowly in proprietary systems, innovation velocity still constrained by single vendors); proprietary vendors and some enterprise software houses attest it is substantially resolved through professional support infrastructure and security practices. The disappearance verdict (world_rearranges) indicates the constraint is not a natural law but a constructed norm. However, the founding problem lives on in actual software development practice — the norm persists because the underlying coordination problem persists, not because of institutional inertia or theatrical maintenance. The constraint is NOT mandatrophied. If the founding problem became truly dead (if proprietary vendors matched open source quality and velocity) the constraint would lose justification, but empirical evidence does not yet support that outcome; the contest remains open.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_vs_freedom_imperative,
    'Is the pragmatic-development reading a genuine alternative framing that accepts proprietary software as legitimate, or is it a rhetorical move that instrumentalizes freedom to avoid the freedom_imperative reading''s ethical demands?',
    'Test consistency: Does this reading''s proponents defend high-quality proprietary software on its merits? Do they apply the methodology-superiority claim neutrally, or do they selectively invoke it against proprietary software while ignoring open source quality failures? Do the same actors maintain this framing across different institutional contexts (professional, academic, advocacy)?',
    'If the reading is genuinely pragmatic, it should be coherent when applied neutrally to counterexamples. If it proves to be instrumental cover for freedom-imperative commitments, the reading collapses into its sibling and the constraint''s classification shifts. If it is consistently applied but only in contexts where openness dominates, it remains a live reading but requires a modifier (pragmatism-in-practice vs. pragmatism-as-principle).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_freedom_imperative, conceptual, 'Whether this reading is genuinely pragmatic or rhetorically dependent on the freedom_imperative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__pragmatic_development_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__pragmatic_development_reading, theater_ratio, 0, 0.09).
narrative_ontology:measurement_basis(soft_tr_t0, observed).
narrative_ontology:measurement(soft_tr_t5, software_source_status__pragmatic_development_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement_basis(soft_tr_t5, observed).
narrative_ontology:measurement(soft_tr_t10, software_source_status__pragmatic_development_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement_basis(soft_tr_t10, observed).
narrative_ontology:measurement(soft_tr_t15, software_source_status__pragmatic_development_reading, theater_ratio, 15, 0.16).
narrative_ontology:measurement_basis(soft_tr_t15, observed).
narrative_ontology:measurement(soft_tr_t20, software_source_status__pragmatic_development_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(soft_tr_t20, observed).
narrative_ontology:measurement(soft_tr_t25, software_source_status__pragmatic_development_reading, theater_ratio, 25, 0.19).
narrative_ontology:measurement_basis(soft_tr_t25, observed).
narrative_ontology:measurement(soft_tr_t30, software_source_status__pragmatic_development_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement_basis(soft_tr_t30, observed).
narrative_ontology:measurement(soft_tr_t35, software_source_status__pragmatic_development_reading, theater_ratio, 35, 0.18).
narrative_ontology:measurement_basis(soft_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__pragmatic_development_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(soft_be_t0, observed).
narrative_ontology:measurement(soft_be_t5, software_source_status__pragmatic_development_reading, base_extractiveness, 5, 0.31).
narrative_ontology:measurement_basis(soft_be_t5, observed).
narrative_ontology:measurement(soft_be_t10, software_source_status__pragmatic_development_reading, base_extractiveness, 10, 0.34).
narrative_ontology:measurement_basis(soft_be_t10, observed).
narrative_ontology:measurement(soft_be_t15, software_source_status__pragmatic_development_reading, base_extractiveness, 15, 0.37).
narrative_ontology:measurement_basis(soft_be_t15, observed).
narrative_ontology:measurement(soft_be_t20, software_source_status__pragmatic_development_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement_basis(soft_be_t20, observed).
narrative_ontology:measurement(soft_be_t25, software_source_status__pragmatic_development_reading, base_extractiveness, 25, 0.37).
narrative_ontology:measurement_basis(soft_be_t25, observed).
narrative_ontology:measurement(soft_be_t30, software_source_status__pragmatic_development_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(soft_be_t30, observed).
narrative_ontology:measurement(soft_be_t35, software_source_status__pragmatic_development_reading, base_extractiveness, 35, 0.38).
narrative_ontology:measurement_basis(soft_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_source_status__pragmatic_development_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(soft_su_t0, observed).
narrative_ontology:measurement(soft_su_t5, software_source_status__pragmatic_development_reading, suppression_requirement, 5, 0.17).
narrative_ontology:measurement_basis(soft_su_t5, observed).
narrative_ontology:measurement(soft_su_t10, software_source_status__pragmatic_development_reading, suppression_requirement, 10, 0.19).
narrative_ontology:measurement_basis(soft_su_t10, observed).
narrative_ontology:measurement(soft_su_t15, software_source_status__pragmatic_development_reading, suppression_requirement, 15, 0.21).
narrative_ontology:measurement_basis(soft_su_t15, observed).
narrative_ontology:measurement(soft_su_t20, software_source_status__pragmatic_development_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement_basis(soft_su_t20, observed).
narrative_ontology:measurement(soft_su_t25, software_source_status__pragmatic_development_reading, suppression_requirement, 25, 0.22).
narrative_ontology:measurement_basis(soft_su_t25, observed).
narrative_ontology:measurement(soft_su_t30, software_source_status__pragmatic_development_reading, suppression_requirement, 30, 0.22).
narrative_ontology:measurement_basis(soft_su_t30, observed).
narrative_ontology:measurement(soft_su_t35, software_source_status__pragmatic_development_reading, suppression_requirement, 35, 0.22).
narrative_ontology:measurement_basis(soft_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__pragmatic_development_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(software_source_status__pragmatic_development_reading, 0.18).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__property_rights_reading).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the software_source_status kernel. The kernel-reading decomposition follows OQ-26 (ε-invariance principle): each reading instantiates a different constraint with distinct ε, beneficiary/victim structure, and narrative justification. All four readings (pragmatic_development, freedom_imperative, property_rights, utilitarian_hybrid) are linked via network.affects_constraints as a constraint family — each reading influences the others by creating competing institutional legitimacy grounds, resource allocation pressures, and policy arguments. The pragmatic_development reading (this file) influences downstream readings by establishing quality/velocity as a salient legitimacy basis; it coexists_with freedom_imperative (neither rules the other out; different parties hold both) and influences property_rights (by raising the bar for proprietary legitimacy justification).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
