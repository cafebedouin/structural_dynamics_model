% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__enforcement_vacuum_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__enforcement_vacuum_reading, []).

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
 *   constraint_id: gpl_copyleft_scope__enforcement_vacuum_reading
 *   human_readable: GPL Copyleft Scope Enforcement Vacuum
 *   domain: intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   The GPL's Section 2(b) requires that derivative works be licensed under
 *   GPL. But the term 'derivative work' is ambiguous under modern software
 *   architecture: does it include dynamically linked plugins? Statically
 *   linked libraries? Service calls? Two interpretive communities coexist
 *   with incompatible readings. The FSF-aligned reading is expansive (all
 *   functional coupling triggers GPL); the industry-dominated reading is
 *   narrow (traditional copyright derivative doctrine only). Absence of
 *   definitive judicial precedent allows both to claim legitimacy. This is
 *   NOT a mountain or a genuine rope—the coordination function exists (GPL
 *   does coordinate free software development) but the constraint's actual
 *   operation depends on which interpretive community can enforce its reading
 *   in specific contexts. The separation itself is the extraction mechanism:
 *   clarity-seeking adopters face elevated transaction costs; pragmatic
 *   adopters benefit from ambiguity that lets them navigate between
 *   interpretations.
 *
 * KEY AGENTS:
 *   - FSF-aligned projects: interpret GPL expansively, enforce through social/community mechanisms
 *   - Industry-dominated ecosystems: interpret GPL narrowly, enforce through legal review and CLAs
 *   - Clarity-seeking adopters: face dual-compliance costs, constrained exit
 *   - Pragmatic adopters: exploit ambiguity strategically, arbitrage between readings
 *   - Independent developers: lack negotiation power, identity-locked to GPL commitment
 *   - Courts: institutional silence that sustains the vacuum
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__enforcement_vacuum_reading, 0.42).
domain_priors:suppression_score(gpl_copyleft_scope__enforcement_vacuum_reading, 0.38).
domain_priors:theater_ratio(gpl_copyleft_scope__enforcement_vacuum_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__enforcement_vacuum_reading, tangled_rope).
narrative_ontology:human_readable(gpl_copyleft_scope__enforcement_vacuum_reading, "GPL Copyleft Scope Enforcement Vacuum").
narrative_ontology:topic_domain(gpl_copyleft_scope__enforcement_vacuum_reading, "intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__enforcement_vacuum_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__enforcement_vacuum_reading, 'c7202882-49e4-4fae-9c49-b377efe1d868').
narrative_ontology:cs_kernel_codification('c7202882-49e4-4fae-9c49-b377efe1d868', fixed_text).
narrative_ontology:cs_authority_grounding('c7202882-49e4-4fae-9c49-b377efe1d868', distributed).
narrative_ontology:cs_reading_relation('c7202882-49e4-4fae-9c49-b377efe1d868', gpl_copyleft_scope__strong_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('c7202882-49e4-4fae-9c49-b377efe1d868', gpl_copyleft_scope__narrow_scope_reading, coexists_with).
narrative_ontology:cs_axiom('c7202882-49e4-4fae-9c49-b377efe1d868', foundational, derivative_work_scope_fundamentally_unsettled).
narrative_ontology:cs_axiom_status(derivative_work_scope_fundamentally_unsettled, holdable).
narrative_ontology:cs_axiom_grounding('c7202882-49e4-4fae-9c49-b377efe1d868', derivative_work_scope_fundamentally_unsettled, empirically_contingent).
narrative_ontology:cs_axiom('c7202882-49e4-4fae-9c49-b377efe1d868', foundational, institutional_silence_constitutes_legitimate_ambiguity).
narrative_ontology:cs_axiom_status(institutional_silence_constitutes_legitimate_ambiguity, holdable).
narrative_ontology:cs_axiom_grounding('c7202882-49e4-4fae-9c49-b377efe1d868', institutional_silence_constitutes_legitimate_ambiguity, conventional).
narrative_ontology:cs_reference_frame('c7202882-49e4-4fae-9c49-b377efe1d868', gpl_v2_expanded_adoption_era).
narrative_ontology:cs_drift_state('c7202882-49e4-4fae-9c49-b377efe1d868', contemporary_software_architecture, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c7202882-49e4-4fae-9c49-b377efe1d868', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_adopters).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, ecosystem_gatekeepers).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, clarity_seeking_adopters).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, independent_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, license_authors_fsf).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_adopters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets GPL Section 2(b) expansively: derivative work boundary extends to dynamic linking, plugin architectures, and functional coupling. Enforces this reading through license review, community governance, and selective grant/withdrawal of trademark rights. Operates primarily through social enforcement (disapproval, forking) rather than litigation. Maintains the interpretation as a normative commitment to software freedom, not as a legal claim they have tested in court.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, fsf_aligned_projects, agenda_setter,
    organized, generational, mobile, global).

% Interprets GPL Section 2(b) narrowly: derivative work boundary follows traditional copyright doctrine, constraining only direct source modifications and static linking. Enforces this reading through internal legal review, contributor license agreements (CLAs), and selective binary distribution. Operates through license compliance audits and potential litigation threat. Adopted GPL for community goodwill while maintaining proprietary business models around platform, services, or distribution.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, industry_dominated_ecosystems, agenda_setter,
    institutional, biographical, constrained, global).

% Wish to use GPL-licensed components while maintaining proprietary code. Face elevated transaction costs from the interpretive vacuum: must conduct legal analysis under multiple readings, negotiate with both interpretive communities, potentially conform to both standards simultaneously, or accept prolonged licensing uncertainty. Their exit cost is high because GPL ecosystem value is significant but clarity is unavailable at any price point.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, clarity_seeking_adopters, payer,
    moderate, biographical, constrained, global).

% Navigate the vacuum strategically: adopt GPL components where the narrow reading supports their architecture, maintain private interpretation documentation to support narrow-reading positions if challenged, contribute selectively to FSF-aligned projects to maintain legitimacy, and scale contributions to industry-dominated ecosystems. The ambiguity is an asset—it allows design flexibility without forcing a single binding commitment. They can shift interpretive alignment based on project context.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_adopters, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_adopters, payer).

% Lack the legal resources to navigate dual interpretations or the market power to negotiate special treatment from either interpretive community. Face the full cost of the vacuum: must either commit to a single reading and risk later challenge, seek explicit permission from both communities (which may be withheld), or avoid GPL-licensed dependencies entirely. Their professional identity in open source often carries ideological commitment to GPL principles, making exit psychologically costly.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, independent_developers, payer,
    powerless, biographical, identity_locked, global).

% Have not produced binding precedent that resolves the GPL derivative-work boundary question. This absence is structural to the constraint: the vacuum persists because no court has definitively ruled on whether dynamic linking, plugin architectures, or functional coupling trigger GPL's section 2(b) requirements. The refusal or delay of courts to adjudicate gives both interpretive communities standing to claim their reading reflects the law.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, courts_and_legal_precedent, agenda_setter,
    institutional, generational, analytical, national).

% The FSF authored GPL and publishes interpretation guidance, but that guidance is not legally binding and has never been tested comprehensively in court. They benefit from the broad reading because it maximizes the scope of their copyleft commitment, aligns with their normative mission to spread software freedom, and increases the dependency on their interpretive authority. Yet they also incur costs: uncertainty undermines license adoption, and the absence of court backing means their interpretation can be challenged without legal consequences.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, license_authors_fsf, beneficiary,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_copyleft_scope__enforcement_vacuum_reading, diffuse).
narrative_ontology:fixing_cost_class(gpl_copyleft_scope__enforcement_vacuum_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The GPL license itself—a coordination mechanism that resolves the commons problem of free software development by requiring any derivative to remain free. The enforcement vacuum reflects the coordination cost: without settled legal meaning of 'derivative work,' participants cannot coordinate unambiguously on what code triggers the reciprocal obligation.
% TRANSFER_FUNCTION: The constraint moves interpretive authority: from formal legal institutions (courts) to informal communities (FSF-aligned projects, industry ecosystems) based on their enforcement capacity in specific contexts. Clarity-seeking adopters transfer negotiation costs and legal risk to whichever community has stronger enforcement capacity in their context. Pragmatic adopters transfer risk management costs to themselves via strategic community navigation.
% ABSENT_VOICES: Independent developers and smaller open-source foundations lack the legal resources or market power to participate in shaping interpretations. They are de facto excluded because they cannot credibly threaten either interpretation community with litigation or market retaliation. Absent too are the courts themselves—their silence is structural to the vacuum, but judicial voices are not 'excluded' so much as 'not-yet-heard'; treating their absence as exclusion would be conceptually confused.
% DISAPPEARANCE_RATIONALE: If the enforcement vacuum disappeared overnight (via definitive court ruling or authoritative GPL amendment), the constraint would vanish and be replaced by a new constraint: the settled GPL derivative-work boundary. Software architectures currently designed around ambiguity would need to be re-evaluated and potentially restructured. The dual interpretive communities would lose their role as de facto authorities, and licensing costs would shift (upward for those whose preferred reading lost, downward for those whose reading prevailed). Business models built on exploiting the ambiguity would face either renegotiation or obsolescence.
% FOUNDING_PROBLEM: The GPL was written in 1989 with static linking and source modification as the primary coupling mechanisms. By the 1990s–2000s, plugin architectures, dynamic linking, and service-oriented coupling emerged as new forms of code integration that GPL's text did not explicitly address. The founding problem became: how should GPL's reciprocal obligation apply to these new architectural forms? The text ('derivative work') was ambiguous under 21st-century practice.
% FOUNDING_PROBLEM_CORROBORATION: The FSF, major open-source distributors (Debian, Red Hat), and independent researchers all attest that the derivative-work boundary question remains live and unresolved. GPL v3 (2007) was partly a response to this problem, but did not resolve the core ambiguity—it added clarifications on tivoization and DRM, but did not settle the plugin/dynamic-linking boundary. Legal scholars, software architects, and industry compliance officers all independently attest that uncertainty persists. No external court has ruled to settle the question.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__enforcement_vacuum_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__enforcement_vacuum_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__enforcement_vacuum_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_copyleft_scope__enforcement_vacuum_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__enforcement_vacuum_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__enforcement_vacuum_reading_tests).
:- end_tests(gpl_copyleft_scope__enforcement_vacuum_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the constraint's operation produces real costs (legal risk, dual compliance, negotiation overhead) but also enables beneficial coordination (GPL ecosystem remains vibrant, free software develops). The vacuum itself is the coordinating mechanism—it allows flexibility that a settled reading would foreclose. Suppression is moderate (0.38) because neither interpretation community can fully suppress the other without court backing; FSF-aligned projects can deny trademark/social legitimacy, but cannot legally prevent narrow-reading implementations; industry ecosystems can implement their reading, but cannot prevent FSF projects from treating them as GPL violators. Theater ratio is moderate (0.28) because both communities perform their interpretation as commitments to principle, but significant interpretive activity is driven by risk management and business model fit. The measurement series show rising extractiveness and theater from 1989 to 2007 as plugin architectures and dynamic linking became central (the problem emerged), then stabilized from 2007-2026 as both communities settled into stable interpretive strategies despite the absence of resolution. Theater stabilizes because the performative work of defending the interpretation reached an equilibrium.
 *
 * PERSPECTIVAL GAP:
 *   FSF-aligned projects and industry-dominated ecosystems experience this constraint structurally differently. For FSF projects, the ambiguity is ENABLING: they can interpret expansively, enforce through community pressure, and claim the high ground of GPL purity without facing legal contradiction. For industry ecosystems, the ambiguity is LIMITING: they must implement a narrow reading defensively, conduct compliance audits, manage legal risk, and accept that their implementation could be challenged. Clarity-seeking adopters and pragmatic adopters sit on opposite ends of the risk tolerance axis: clarity-seekers want the vacuum resolved (would benefit from clear rules even if unfavorable); pragmatists want it to persist (profit from ambiguity). Courts (institutional observer) experience the constraint as a choice to remain silent—their power to resolve the vacuum is high, but their decisions not to adjudicate GPL derivative-work cases is the structural fact that sustains the constraint. The engine computes per-seat classifications from this asymmetric structural data: FSF-aligned projects compute closer to beneficiary d; clarity-seeking adopters compute closer to target d; pragmatists compute closer to beneficiary d despite nominally paying the extraction cost; industry ecosystems compute closer to balanced d (they enforce but don't collect; they suppress but don't fully control).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: Pragmatic adopters (exploit ambiguity for architectural flexibility; no need to commit to one reading); FSF-aligned projects (their interpretation remains uncontested in their own domain; they benefit from normative standing). Victims: Clarity-seeking adopters (face dual-compliance costs and legal risk); independent developers (lack power to negotiate, bear full uncertainty cost without compensation). The asymmetry is structural: the vacuum is resolved at the level of CONTEXT (FSF ecosystem vs industry ecosystem) rather than at the level of the license itself. An adopter's exit options depend on their power and their community alignment. Industry ecosystems have arbitrage-grade exit: they can shift interpretation, contribute selectively, maintain dual-face presentation. Clarity-seeking adopters have constrained exit: they can use GPL components or avoid them, but cannot change the license's meaning. Independent developers have identity-locked exit: GPL embodies their normative commitment to software freedom; exiting the GPL ecosystem means abandoning core professional identity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how should GPL apply to new architectural coupling forms) is LIVE but UNRESOLVED. The constraint was not built to solve this problem—it emerged as GPL aged and software architecture evolved. The constraint that emerged is NOT a deliberate construction but an artifact of institutional silence (courts) combined with interpretive pluralism (two communities). Is this still a valid coordination mechanism? The FSF reads it as clarifying GPL's scope; industry reads it as clarifying the narrow interpretation that permits commercial use. Both readings claim they preserve GPL's essential function: reciprocal freedom. Mandatrophy is ABSENT in the sense that the constraint is not yet functionally obsolete—software development proceeds, licensing happens, ecosystems thrive. But there is a CRISIS OF LEGITIMACY: neither interpretation community can justify claiming their reading is THE READING without court backing or explicit GPL v4 amendment. The vacuum itself is treated as a feature by pragmatists and as a defect by clarity-seekers. This is a tangled_rope, not a snare or rope, precisely because: (1) genuine coordination function exists (GPL does coordinate), (2) asymmetric extraction exists (clarity-seekers pay, pragmatists benefit), (3) active enforcement exists (both communities enforce their reading), (4) no universal consensus on the boundary between coordination and extraction (the crisis of legitimacy).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_silence_mechanism,
    'Why have courts consistently declined to adjudicate GPL derivative-work boundary cases? Is this silence deliberate (avoiding precedent that would disrupt either community), accidental (GPL disputes settle before trial), or structural (cases are rare because early settlements and community pressure handle most disputes)?',
    'Analysis of GPL litigation history: which cases were filed, which settled early, why, and on what terms. Interviews with GPL licensing experts on whether judicial silence is stable or contingent.',
    'If silence is deliberate/structural, the vacuum is a stable equilibrium and may be constitutive to GPL''s function as a flexible coordination mechanism. If silence is accidental, a single test case could shatter the vacuum and reclassify all stakeholders'' directionality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_silence_mechanism, empirical, 'Why courts have not produced binding GPL derivative-work precedent despite decades of software architecture evolution').

omega_variable(
    narrow_reading_enforceability,
    'Can an industry-dominated ecosystem credibly enforce the narrow GPL reading without court backing, given that the FSF-aligned reading claims superior normative standing within the open-source community?',
    'Longitudinal tracking of GPL compliance disputes: do narrow-reading implementations face successful community challenges and social sanctions? Do industry ecosystems maintain market position despite FSF-aligned criticism?',
    'If the narrow reading is unenforceable without legal backing, it is a performative cover story and should reclassify the industry-dominated ecosystem''s directionality upward (toward target, not agenda-setter). If enforceable through market dominance and contributor relations, the reading is genuinely coexistent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrow_reading_enforceability, empirical, 'Whether the narrow GPL reading remains viable without judicial precedent backing it').

omega_variable(
    pragmatist_extraction_asymmetry,
    'Do pragmatic adopters who exploit ambiguity actually BENEFIT from the vacuum, or do they incur hidden transaction costs (legal review overhead, maintaining dual-face interpretation, constant monitoring of case law developments) that offset the architectural flexibility gains?',
    'Detailed cost accounting from companies managing GPL-licensed dependencies: what is the true cost of navigating dual interpretations vs. the cost of committing to a single reading and defending it?',
    'If hidden costs are substantial, pragmatists are not beneficiaries but targeted victims with high sophistication for risk management. The extraction would be higher than 0.42 measured globally, concentrated on powerful actors who pay it via lawyers and compliance infrastructure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pragmatist_extraction_asymmetry, empirical, 'Whether pragmatic adopters'' strategic flexibility genuinely offsets their transaction costs').

omega_variable(
    fsf_normative_standing_erosion,
    'Is the FSF''s claim to interpretive authority durable, or is it eroding as commercial adoption of GPL increases and industry ecosystems accumulate enforceability through market dominance?',
    'Longitudinal tracking of FSF interpretive authority: do new GPL projects adopt FSF guidance? Do industry ecosystems cite FSF interpretation or assert their own? Has FSF''s explicit positioning on controversial architectural forms (tivoization, GPL v3 adoption rates) changed in response to lack of judicial backing?',
    'If FSF authority erodes, the asymmetry between interpretive communities shifts. The strong_copyleft_reading becomes less enforceable; the enforcement_vacuum_reading would stabilize as a permanent state rather than a transitional one. Directionality would shift toward benefiting industry ecosystems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fsf_normative_standing_erosion, empirical, 'Whether the FSF maintains cultural/normative authority to enforce expansive GPL interpretation without court backing').

omega_variable(
    kernel_reading_under_determination,
    'Are these three readings (strong_copyleft, narrow_scope, enforcement_vacuum) genuinely structurally distinct interpretations of GPL Section 2(b), or does the enforcement_vacuum reading conflate two separable claims: (a) the legal question of derivative-work scope, and (b) the institutional question of who gets to adjudicate the scope?',
    'Formal analysis of the three readings'' axioms: do they differ on the LEGAL MEANING of derivative work, or on WHO DECIDES the meaning? If the latter, the enforcement_vacuum reading is not a reading of the kernel (the license text) but a reading of the META-KERNEL (the institutional settlement problem).',
    'If the enforcement_vacuum reading is actually a meta-reading, it should be decomposed into two separate constraint stories: (1) the strong_copyleft vs. narrow_scope readings (competing interpretations of the license text), and (2) a constraint about institutional settlement (who gets to decide when there is ambiguity). The current story would be reclassified as a snare or piton focusing on institutional deadlock rather than interpretive pluralism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_under_determination, conceptual, 'Whether enforcement_vacuum is a reading of the GPL text or a reading of the institutional settlement mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__enforcement_vacuum_reading, 1989, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t1989, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 1989, 0.08).
narrative_ontology:measurement(gpl__tr_t1999, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 1999, 0.14).
narrative_ontology:measurement(gpl__tr_t2007, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2007, 0.19).
narrative_ontology:measurement(gpl__tr_t2015, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2015, 0.24).
narrative_ontology:measurement(gpl__tr_t2020, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2020, 0.27).
narrative_ontology:measurement(gpl__tr_t2026, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(gpl__be_t1989, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 1989, 0.15).
narrative_ontology:measurement(gpl__be_t1999, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 1999, 0.28).
narrative_ontology:measurement(gpl__be_t2007, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2007, 0.35).
narrative_ontology:measurement(gpl__be_t2015, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2015, 0.39).
narrative_ontology:measurement(gpl__be_t2020, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2020, 0.41).
narrative_ontology:measurement(gpl__be_t2026, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2026, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1989, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 1989, 0.18).
narrative_ontology:measurement(gpl__su_t1999, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 1999, 0.27).
narrative_ontology:measurement(gpl__su_t2007, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2007, 0.32).
narrative_ontology:measurement(gpl__su_t2015, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2015, 0.37).
narrative_ontology:measurement(gpl__su_t2020, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2020, 0.38).
narrative_ontology:measurement(gpl__su_t2026, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2026, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__enforcement_vacuum_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gpl_copyleft_scope__enforcement_vacuum_reading, 0.12).
narrative_ontology:affects_constraint(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope__strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope__narrow_scope_reading).

% DUAL FORMULATION NOTE:
% The GPL copyleft scope kernel admits three structurally distinct constraint readings. The strong_copyleft_reading interprets GPL's derivative-work boundary expansively; the narrow_scope_reading interprets it narrowly; this enforcement_vacuum_reading names the institutional fact that both readings coexist in the absence of binding judicial precedent. All three readings share the same referent (GPL Section 2(b)) but differ on whether the scope is settled by legal doctrine, by community interpretation, or by institutional silence. Each reading has distinct beneficiaries, victims, and enforcement mechanisms. The three readings are linked via affects_constraints to enable comparative analysis of how the same legal text instantiates different constraints depending on epistemic closure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_copyleft_scope__enforcement_vacuum_reading, powerful, 0.38).
constraint_indexing:directionality_override(gpl_copyleft_scope__enforcement_vacuum_reading, organized, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
