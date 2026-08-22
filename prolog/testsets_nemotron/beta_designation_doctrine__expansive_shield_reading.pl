% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__expansive_shield_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-07
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_beta_designation_doctrine__expansive_shield_reading, []).

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
 *   constraint_id: beta_designation_doctrine__expansive_shield_reading
 *   human_readable: Expansive Beta Designation Liability Shield
 *   domain: technology_law/software_liability/consumer_protection
 *
 * SUMMARY:
 *   The expansive shield reading treats 'beta' as a magic word that
 *   eliminates all product liability indefinitely, across all software
 *   contexts — from consumer apps to medical devices, financial
 *   infrastructure, and critical control systems. Vendors unilaterally apply
 *   the label; users are bound by adhesion contracts; courts enforce
 *   clickwrap assent. The reading emerged from the shift from boxed software
 *   (where beta was a distinct pre-release phase) to SaaS/continuous
 *   deployment (where every release can be labeled beta). The constraint is
 *   not a coordination mechanism — it is a liability externalization tool
 *   enforced by contract law formalism and platform power.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__expansive_shield_reading, 0.88).
domain_priors:suppression_score(beta_designation_doctrine__expansive_shield_reading, 0.72).
domain_priors:theater_ratio(beta_designation_doctrine__expansive_shield_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__expansive_shield_reading, snare).
narrative_ontology:human_readable(beta_designation_doctrine__expansive_shield_reading, "Expansive Beta Designation Liability Shield").
narrative_ontology:topic_domain(beta_designation_doctrine__expansive_shield_reading, "technology_law/software_liability/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__expansive_shield_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__expansive_shield_reading, 'ba4ad0ce-ff82-4465-b46a-cc38ac32292a').
narrative_ontology:cs_kernel_codification('ba4ad0ce-ff82-4465-b46a-cc38ac32292a', distributed).
narrative_ontology:cs_authority_grounding('ba4ad0ce-ff82-4465-b46a-cc38ac32292a', practice).
narrative_ontology:cs_interpretation_layer_present('ba4ad0ce-ff82-4465-b46a-cc38ac32292a').
narrative_ontology:cs_reading_relation('ba4ad0ce-ff82-4465-b46a-cc38ac32292a', beta_designation_doctrine__narrow_warning_reading, forecloses).
narrative_ontology:cs_reading_relation('ba4ad0ce-ff82-4465-b46a-cc38ac32292a', beta_designation_doctrine__severity_carve_out_reading, influences).
narrative_ontology:cs_axiom('ba4ad0ce-ff82-4465-b46a-cc38ac32292a', foundational, beta_designation_eliminates_all_liability_indefinitely).
narrative_ontology:cs_axiom_status(beta_designation_eliminates_all_liability_indefinitely, holdable).
narrative_ontology:cs_axiom_grounding('ba4ad0ce-ff82-4465-b46a-cc38ac32292a', beta_designation_eliminates_all_liability_indefinitely, conventional).
narrative_ontology:cs_axiom('ba4ad0ce-ff82-4465-b46a-cc38ac32292a', foundational, user_assent_to_terms_constitutes_complete_risk_allocation).
narrative_ontology:cs_axiom_status(user_assent_to_terms_constitutes_complete_risk_allocation, holdable).
narrative_ontology:cs_axiom_grounding('ba4ad0ce-ff82-4465-b46a-cc38ac32292a', user_assent_to_terms_constitutes_complete_risk_allocation, conventional).
narrative_ontology:cs_reference_frame('ba4ad0ce-ff82-4465-b46a-cc38ac32292a', genuine_beta_testing_reciprocity).
narrative_ontology:cs_drift_state('ba4ad0ce-ff82-4465-b46a-cc38ac32292a', continuous_deployment_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('ba4ad0ce-ff82-4465-b46a-cc38ac32292a', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, software_vendors).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, platform_operators).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, vc_backed_startups).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, software_users).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, enterprise_customers).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, consumer_advocacy_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Release software under perpetual beta labels to avoid all defect liability while collecting revenue. Control the designation unilaterally; users must accept terms of service that incorporate the beta shield. Can shift products in and out of beta at will without user consent.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, software_vendors, beneficiary,
    institutional, biographical, arbitrage, global).

% Operate app stores and distribution platforms that host beta-labeled software. Benefit from increased developer activity and transaction volume while platform terms of service disclaim liability for hosted applications. Set platform policies that treat beta designation as a valid liability disclaimer.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, platform_operators, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__expansive_shield_reading, platform_operators, agenda_setter).

% Use perpetual beta as a go-to-market strategy: ship fast, iterate in production, externalize QA costs to early users. The beta shield enables fundraising narratives about 'rapid iteration' while limiting downside exposure. Can exit via acquisition before liability materializes.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, vc_backed_startups, beneficiary,
    powerful, biographical, mobile, global).

% Bear all costs of defects, data loss, security breaches, and operational failures in beta-designated software. Cannot negotiate terms; must accept clickwrap agreements. Switching costs (data lock-in, workflow integration, organizational dependency) make exit expensive. No recourse for damages even when defects are known and unaddressed for years.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, software_users, payer,
    moderate, biographical, constrained, global).

% Procure beta-designated software for critical operations under enterprise agreements that incorporate vendor beta disclaimers. Bear costs of downtime, compliance failures, and security incidents. Have some negotiating leverage but face vendor lock-in and ecosystem dependency. Often cannot obtain meaningful indemnification for beta-designated products.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, enterprise_customers, payer,
    powerful, biographical, constrained, global).

% Attempt to challenge beta shields through litigation, regulatory complaints, and legislative advocacy. Systematically excluded from terms-of-service formation; courts defer to clickwrap assent. Lack standing to sue on behalf of diffuse user harms. Regulatory agencies (FTC, state AGs) have brought limited actions but no comprehensive rulemaking.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, consumer_advocacy_groups, excluded,
    organized, generational, trapped, national).

% Analyze the doctrine's evolution from genuine testing disclosure to liability elimination tool. Track circuit splits on enforceability of perpetual beta disclaimers. Observe the gap between contract law formalism (assent = binding) and the structural reality of adhesion contracts in software markets.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, legal_scholars_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally: a time-bounded disclosure mechanism allowing developers to recruit test users for genuine pre-release validation while signaling reduced reliability. Users voluntarily assume risk in exchange for early access and influence over product direction.
% TRANSFER_FUNCTION: Moves all defect costs (data loss, downtime, security breaches, compliance failures, reputational harm) from software vendors to users — both individual and enterprise — without temporal limit, severity cap, or reciprocity. Vendors retain all revenue and control; users bear all downside.
% ABSENT_VOICES: End users (especially non-technical consumers and small businesses) who cannot evaluate beta risk meaningfully; regulatory bodies that have not updated product liability frameworks for software; insurance markets that cannot price beta-designated software risk; open-source maintainers pressured to adopt beta shields by platform policies.
% DISAPPEARANCE_RATIONALE: If the expansive beta shield vanished overnight, vendors would face standard product liability for defects in released software. Release practices would shift toward more rigorous pre-release testing, staged rollouts, and meaningful SLAs. Insurance markets would develop software liability products. Users would gain legal recourse for known, unaddressed defects. The 'ship broken, fix later' development model would become economically untenable.
% FOUNDING_PROBLEM: Early personal computing and nascent internet software distribution needed a low-friction way for developers to distribute pre-release software to willing testers without facing ruinous liability for inevitable bugs in genuinely experimental code.
% FOUNDING_PROBLEM_CORROBORATION: Software historians and early industry participants (e.g., Joel Spolsky, Eric Raymond, Microsoft pre-2000 developer relations archives) attest that beta programs were explicitly time-bounded, opt-in, and reciprocal — testers got early access and direct developer channels in exchange for structured feedback. No contemporary practitioner defends perpetual, universal, non-reciprocal beta as a testing mechanism; the status is dead per the original function. The only defenders are current beneficiaries arguing the arrangement enables innovation (a shifted justification, not the founding problem).
narrative_ontology:disappearance_verdict(beta_designation_doctrine__expansive_shield_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__expansive_shield_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__expansive_shield_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(beta_designation_doctrine__expansive_shield_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__expansive_shield_reading, 0.88, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beta_designation_doctrine__expansive_shield_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(beta_designation_doctrine__expansive_shield_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(beta_designation_doctrine__expansive_shield_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.88) reflects near-total cost externalization: vendors capture 100% of revenue while bearing 0% of defect liability. Suppression (0.72) comes from adhesion contracts, platform gatekeeping, and judicial deference to terms of service — users have no meaningful exit. Theater (0.25) is low but rising: vendors maintain minimal 'feedback' channels (bug trackers, forums) that perform the testing ritual without changing the liability structure. Accessibility collapse (0.42) is moderate because alternatives exist (open source, regulated vendors, self-hosted) but are structurally disadvantaged. Resistance (0.38) reflects scattered litigation and regulatory gestures that have not coalesced into a countervailing framework.
 *
 * PERSPECTIVAL GAP:
 *   From the vendor seat, the arrangement looks like efficient risk allocation: users get free/cheap software, vendors innovate faster. From the user seat, it is unbounded liability absorption with no reciprocity. From the platform seat, it is ecosystem growth at zero marginal cost. From the court seat, it is contract enforcement. The engine computes per-seat classifications from these structural positions — the claimed type (snare) reflects the dominant structural reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Software vendors, platform operators, and VC-backed startups are structural beneficiaries (d ≈ 0.1–0.2): they collect revenue, control the designation, and face no downside. Software users and enterprise customers are structural targets (d ≈ 0.8–0.9): they bear all defect costs with constrained exit. Consumer advocacy groups are excluded (d ≈ 0.95): they would object but are structurally locked out of contract formation and standing doctrines. Legal scholars/courts are observers (d = 0.5): they analyze but do not bear costs or collect gains.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (genuine pre-release testing with reciprocal risk-sharing) is dead. The arrangement persists as a liability elimination tool — a classic mandatrophy case where the original coordination function has been entirely displaced by extraction. The beta label is now a legal technology for converting defect costs into user-borne externalities. No stakeholder defends the arrangement as a testing mechanism; defenders argue it 'enables innovation' (a post-hoc justification). The constraint should be classified as snare, not rope or tangled rope, because the coordination story is cover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    beta_as_liability_elimination_vs_testing_signal,
    'Is the beta designation in contemporary SaaS/continuous-deployment practice a genuine testing signal or a liability elimination technology?',
    'Empirical study of beta duration distributions, defect remediation rates, and user feedback incorporation in beta-labeled vs. stable-labeled products from the same vendors. If beta products show no faster remediation or higher feedback incorporation than stable products, the testing signal is fictitious.',
    'If beta is a liability elimination technology, the constraint is pure extraction (snare). If it retains a genuine testing function for a subset of users/products, it may be tangled_rope with a coordination core.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beta_as_liability_elimination_vs_testing_signal, empirical, 'Whether the beta label performs any genuine coordination function in current practice').

omega_variable(
    clickwrap_assent_as_structural_coercion,
    'Does clickwrap assent to beta terms constitute meaningful consent or structural coercion given switching costs and market concentration?',
    'Behavioral economics study of user comprehension of beta disclaimers, combined with market concentration metrics (HHI of major software categories) and switching cost quantification.',
    'If assent is structural coercion, suppression is higher than the formal contract measure suggests — the constraint operates through market structure, not voluntary agreement. This affects the snare vs. tangled_rope boundary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(clickwrap_assent_as_structural_coercion, conceptual, 'Whether the consent mechanism is genuine or a structural fiction').

omega_variable(
    kernel_reading_foreclosure_structure,
    'Does the expansive shield reading logically foreclose the narrow warning reading within a single legal framework, or do they coexist as competing doctrines across jurisdictions?',
    'Comparative analysis of court opinions: do courts applying the expansive reading explicitly reject the temporal/reciprocity requirements of the narrow reading as legally incoherent, or do they treat them as policy choices?',
    'If forecloses, the kernel has a structural fracture — one reading''s victory eliminates the other. If coexists_with, the kernel remains a live contest with jurisdictional variation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_structure, conceptual, 'Structural relationship between expansive and narrow readings of the beta doctrine').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__expansive_shield_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t0, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(beta_tr_t5, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(beta_tr_t10, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(beta_tr_t15, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(beta_tr_t20, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(beta_tr_t25, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 25, 0.25).

% Extraction over time
narrative_ontology:measurement(beta_be_t0, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(beta_be_t5, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(beta_be_t10, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(beta_be_t15, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 15, 0.8).
narrative_ontology:measurement(beta_be_t20, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 20, 0.85).
narrative_ontology:measurement(beta_be_t25, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 25, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t0, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(beta_su_t5, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(beta_su_t10, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(beta_su_t15, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(beta_su_t20, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(beta_su_t25, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
