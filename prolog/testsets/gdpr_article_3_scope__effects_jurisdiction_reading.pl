% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__effects_jurisdiction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gdpr_article_3_scope__effects_jurisdiction_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gdpr_article_3_scope__effects_jurisdiction_reading
 *   human_readable: GDPR Article 3(2) Effects-Based Jurisdiction (EU Data Subject Protection Reading)
 *   domain: technology/regulatory/international
 *
 * SUMMARY:
 *   GDPR Article 3(2) extends EU data protection law to non-EU controllers
 *   who target or monitor EU residents. This constraint embodies one reading
 *   of a contested kernel: the Article 3(2) scope clause. The
 *   EFFECTS-JURISDICTION READING interprets Article 3(2) as grounding
 *   jurisdiction in the effects on EU residents—targeting, monitoring,
 *   profiling behavior creates sufficient connection to assert enforcement
 *   authority over non-EU controllers without claiming territorial
 *   sovereignty. Under this reading, the extraterritorial reach is legitimate
 *   because it protects those whose data is being processed, not because the
 *   EU claims territorial authority over the controller's home state. The
 *   reading generates high compliance costs for non-EU controllers,
 *   enforcement through fines up to 4% of global revenue, and a beneficiary
 *   structure anchored in EU data subject protection. The claim/metric gap is
 *   intentional: this reading is CLAIMED as tangled_rope (coordination of
 *   privacy standards + asymmetric extraction from non-EU controllers) while
 *   the authored metrics describe substantially extractive, actively enforced
 *   operation. The engine measures how the computed per-seat types diverge
 *   from this claim; the gap is the diagnostic signal this reading generates.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__effects_jurisdiction_reading, 0.68).
domain_priors:suppression_score(gdpr_article_3_scope__effects_jurisdiction_reading, 0.71).
domain_priors:theater_ratio(gdpr_article_3_scope__effects_jurisdiction_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__effects_jurisdiction_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__effects_jurisdiction_reading, "GDPR Article 3(2) Effects-Based Jurisdiction (EU Data Subject Protection Reading)").
narrative_ontology:topic_domain(gdpr_article_3_scope__effects_jurisdiction_reading, "technology/regulatory/international").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__effects_jurisdiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__effects_jurisdiction_reading, '75be9f8b-65ed-46a0-b61a-f6a9a8833aed').
narrative_ontology:cs_kernel_codification('75be9f8b-65ed-46a0-b61a-f6a9a8833aed', fixed_text).
narrative_ontology:cs_authority_grounding('75be9f8b-65ed-46a0-b61a-f6a9a8833aed', extraction).
narrative_ontology:cs_interpretation_layer_present('75be9f8b-65ed-46a0-b61a-f6a9a8833aed').
narrative_ontology:cs_reading_relation('75be9f8b-65ed-46a0-b61a-f6a9a8833aed', gdpr_article_3_scope__territorial_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('75be9f8b-65ed-46a0-b61a-f6a9a8833aed', gdpr_article_3_scope__market_access_reading, influences).
narrative_ontology:cs_axiom('75be9f8b-65ed-46a0-b61a-f6a9a8833aed', foundational, effects_create_jurisdictional_nexus).
narrative_ontology:cs_axiom_status(effects_create_jurisdictional_nexus, holdable).
narrative_ontology:cs_axiom_grounding('75be9f8b-65ed-46a0-b61a-f6a9a8833aed', effects_create_jurisdictional_nexus, deontological).
narrative_ontology:cs_axiom('75be9f8b-65ed-46a0-b61a-f6a9a8833aed', foundational, extraterritorial_enforcement_legitimate_to_protect_affected_residents).
narrative_ontology:cs_axiom_status(extraterritorial_enforcement_legitimate_to_protect_affected_residents, holdable).
narrative_ontology:cs_axiom_grounding('75be9f8b-65ed-46a0-b61a-f6a9a8833aed', extraterritorial_enforcement_legitimate_to_protect_affected_residents, instrumental).
narrative_ontology:cs_reference_frame('75be9f8b-65ed-46a0-b61a-f6a9a8833aed', effects_based_jurisdictional_reach).
narrative_ontology:cs_drift_state('75be9f8b-65ed-46a0-b61a-f6a9a8833aed', contemporary_enforcement_maturity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('75be9f8b-65ed-46a0-b61a-f6a9a8833aed', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_privacy_enforcement_regime).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_data_controllers).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, small_non_eu_platforms).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__effects_jurisdiction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(gdpr_article_3_scope__effects_jurisdiction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gdpr_article_3_scope__effects_jurisdiction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gdpr_article_3_scope__effects_jurisdiction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gdpr_article_3_scope__effects_jurisdiction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68 at interval end) because non-EU controllers bear substantial compliance costs (data processing redesign, legal, DPAs, breach notifications) decoupled from marginal service provision. The costs accumulate over time (0.45 → 0.68 from 2018 to 2026) as enforcement matures: early GDPR enforcement (2018–2020) focused on EU-based entities; by 2022–2026, non-EU controllers faced increasing DPA actions, fine issuance, and adequacy disputes. Suppression is high (0.71) because the constraint's persistence depends on active enforcement (DPA investigations, fines, adequacy mechanism pressure) and exclusion of rival jurisdictional readings (territorial and market-access). Non-EU controllers cannot escape GDPR's reach by claiming jurisdiction is illegitimate; DPAs continue enforcement regardless. Theater is low-moderate (0.28): the coordination function (privacy protection for EU residents) is genuinely served, but enforcement increasingly focuses on suppressing alternatives (blocking market-access framing, excluding territorial defenses) rather than improving privacy outcomes. The targeting/monitoring test is subject to continued reinterpretation; DPA guidance documents and enforcement decisions show theater increasing over time (0.12 → 0.28) as the rule's operational boundary becomes more performative than substantive. All measurements share one time grid (2018, 2020, 2022, 2024, 2026) so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   From the EU enforcement perspective: the constraint is genuine coordination (privacy protection for EU residents wherever they are) with necessary enforcement overhead. From the non-EU controller perspective: the constraint is enforced extraction (compliance costs decoupled from benefit) masked as protection, with no meaningful exit (geoblocking loses revenue; compliance is mandatory; resistance to DPA action is futile without EU presence). From a territorial-sovereignty advocate's perspective: the constraint is illegitimate overreach that violates international law (no territorial basis for jurisdiction). From a market-access advocate's perspective: the constraint functions as conditional market access (de facto standard-setting), not jurisdiction—a different frame that would shift the beneficiary (market-access setter, not data protector) and the victim structure (competitors excluded by standard, not controllers burdened by law). The engine computes per-seat types from power + exit + beneficiary/victim; these divergent readings produce divergent computed types for the same structural constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   EU data subjects and the EU enforcement regime are beneficiaries (protected, empowered): d near 0.0 for data subjects (constraint benefits them without cost), near 0.1–0.2 for DPAs (they collect authority but bear administrative cost). Non-EU controllers are victims (bear compliance costs, enforcement exposure, constrained alternatives): d near 0.8–0.9 for large institutional controllers who can absorb costs but have no exit, higher (near 0.95) for small platforms trapped between business continuity and compliance. Market-access advocates and territorial sovereignty advocates are excluded, not coordinated: their exclusion is the enforcement object (suppressing alternative readings). The engine derives d from beneficiary/victim data + exit options; the structural asymmetry here drives seat divergence: a non-EU controller computes high extractiveness (d near 0.85, trapped exit); an EU data subject computes low/zero extractiveness (d near 0.0, beneficiary). The agenda-setter (EU enforcement) computes moderate extractiveness (d near 0.2, administrative burden despite authority gain).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (non-EU controllers processing EU data under weaker standards) is LIVE: platforms continue to locate processing outside EU, attempt to minimize GDPR overhead, and contest the enforcement mechanism. The constraint's function (protection for EU residents) persists as the stated mandate. Mandatrophy is NOT triggered because the constraint is still actively defending its founding problem against alternatives (territorial readings, market-access framings). However, theater_ratio rising (0.12 → 0.28) suggests performative enforcement increasing: DPA guidance documents and fine issuance show increasing focus on boundary policing (defining targeting/monitoring broadly to catch edge cases, issuing fines for marginal violations) rather than substantive privacy improvement. This is consistent with a constraint beginning to drift toward piton status (the mandate persists, enforcement persists, but increasing share of enforcement protects the rule itself rather than the underlying problem). The omega variable `kernel_reading_lifecycle` documents this: is this reading of Article 3(2) beginning to atrophy as a substantive privacy tool and become a jurisdictional assertion for its own sake?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_legitimacy,
    'Is extraterritorial jurisdiction grounded in effects on EU residents (this reading''s claim) or in EU market access power (market-access reading alternative)?',
    'Analyze DPA decision language, European Court of Justice rulings, and non-EU government responses to distinguish whether enforcement is framed as jurisdiction (authority over the person/entity) or market conditioning (access requirement). Historical trace: GDPR preamble and impact assessments stated effects-jurisdiction; enforcement practice shows increasing market-access framing (adequacy decisions, Schrems decisions, Code of Conduct adoption).',
    'If enforcement is actually market-access (not jurisdiction), the reading mischaracterizes the mechanism. The constraint would be tangled_rope with a different beneficiary structure (market-setter, not data protector) and enforcement mechanism (conditional market access, not enforcement authority). The claim/metric gap would invert: claimed rope (this reading), computed snare (market-access alternative).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_legitimacy, conceptual, 'Whether Article 3(2) enforcement is grounded in jurisdiction or market-access power.').

omega_variable(
    targeting_monitoring_test_coherence,
    'Does the targeting/monitoring test have a stable, coherent definition, or is it an open-ended standard subject to continuous reinterpretation?',
    'Audit DPA guidance, European Data Protection Board decisions (Guidelines 3/2022, etc.), and national court rulings (2018–2026) to document: (a) whether the test''s scope converges or diverges over time, (b) whether enforcement applies consistently to controllers with similar practices, (c) whether DPA decisions predict subsequent enforcement or diverge.',
    'If the test is incoherent/constantly reinterpreted, suppression is higher than authored (the constraint gains power through ambiguity, not clarity). Theater_ratio would be revised upward (more of enforcement is boundary-policing than substantive privacy). The constraint would compute closer to snare (extraction sustained by opaque rules, not transparent coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(targeting_monitoring_test_coherence, empirical, 'Whether the targeting/monitoring test''s definition is stable or subject to continuous reinterpretation.').

omega_variable(
    legitimacy_of_unilateral_extraterritorial_reach,
    'Is a single jurisdiction''s unilateral extraterritorial reach (EU asserting authority over non-EU controllers without those controllers'' consent or their government''s participation) consistent with international law and legitimate regulatory authority?',
    'International law analysis and diplomatic record: does the UN, WTO, UNCLOS, or customary international law recognize effects-based jurisdiction? Do non-EU governments formally contest GDPR extraterritoriality or accept it de facto? Have adequacy mechanisms or trade negotiations resulted in explicit consent or resistance?',
    'If unilateral extraterritorial reach violates international law, the constraint''s legitimacy is undermined (foundational omega for the effects-jurisdiction reading). Territorial-sovereignty reading would gain force. The constraint''s mandate may become contested (mandatrophy) if non-EU governments formally object. Beneficiary structure would need revision: is EU enforcement regime a legitimate beneficiary if it exceeds international authority?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_unilateral_extraterritorial_reach, conceptual, 'Whether unilateral extraterritorial jurisdiction is consistent with international law and legitimate regulatory authority.').

omega_variable(
    kernel_reading_lifecycle,
    'Is the effects-jurisdiction reading in a lifecycle transition from substantive privacy protection (the founding mandate) toward jurisdictional assertion for its own sake (piton atrophy)?',
    'Longitudinal analysis: measure privacy-benefit-per-enforcement-case (2018–2026). Track whether DPA enforcement focuses on privacy-substantive violations (consent, data minimization, security) or jurisdictional-boundary cases (targeting/monitoring threshold disputes). If boundary cases dominate by 2024–2026, the reading is atrophying.',
    'If the reading is atrophying, mandate becomes contested and theater_ratio rises further (>0.4). The constraint should be reclassified to piton (inertial enforcement, no concentrated beneficiary, atrophied function) or toward mandatrophy-resolved status. The founding_problem_status shifts from ''live'' to ''dead'' or ''contested''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_lifecycle, empirical, 'Whether the effects-jurisdiction reading is transitioning from substantive privacy protection toward jurisdictional assertion for its own sake.').

omega_variable(
    competing_jurisdictional_readings,
    'This constraint is ONE reading of a contested kernel (Article 3(2)). Do the TERRITORIAL-SOVEREIGNTY and MARKET-ACCESS readings genuinely foreclose this effects-jurisdiction reading, or do they coexist as live but incompatible interpretive positions?',
    'Institutional and epistemic audit: what would it take for each reading''s core axiom to be adopted? Effects-jurisdiction reading requires: ''effects create sufficient jurisdictional nexus.'' Territorial-sovereignty reading requires: ''jurisdiction only where controller has territorial presence.'' Market-access reading requires: ''GDPR is not jurisdiction but conditional market access.'' These are incommensurate in one framework. Coexistence requires different parties holding each (EU institutions holding effects-jurisdiction, some non-EU governments holding territorial, some trade advocates holding market-access). Foreclosure would require one framework capable of holding all three.',
    'If the readings merely coexist (not foreclose), the constraint is part of a constraint family of three stories, each with different ε, beneficiary, and type. If one forecloses the others, the kernel is not actually contested—one reading is true and others are errors. The cs_structure.reading_relations assignment depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competing_jurisdictional_readings, conceptual, 'Whether competing jurisdictional readings of Article 3(2) coexist or foreclose one another.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__effects_jurisdiction_reading, 2018, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t2018, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2018, 0.12).
narrative_ontology:measurement(gdpr_tr_t2020, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2020, 0.18).
narrative_ontology:measurement(gdpr_tr_t2022, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2022, 0.24).
narrative_ontology:measurement(gdpr_tr_t2024, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2024, 0.26).
narrative_ontology:measurement(gdpr_tr_t2026, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t2018, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2018, 0.45).
narrative_ontology:measurement(gdpr_be_t2020, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2020, 0.54).
narrative_ontology:measurement(gdpr_be_t2022, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2022, 0.62).
narrative_ontology:measurement(gdpr_be_t2024, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2024, 0.66).
narrative_ontology:measurement(gdpr_be_t2026, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_su_t2018, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2018, 0.55).
narrative_ontology:measurement(gdpr_su_t2020, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2020, 0.62).
narrative_ontology:measurement(gdpr_su_t2022, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2022, 0.68).
narrative_ontology:measurement(gdpr_su_t2024, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2024, 0.7).
narrative_ontology:measurement(gdpr_su_t2026, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2026, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__effects_jurisdiction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gdpr_article_3_scope__effects_jurisdiction_reading, 0.12).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope__territorial_sovereignty_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope__market_access_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, adequacy_mechanism_determination).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, data_transfer_mechanisms_schrems).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the GDPR Article 3(2) scope kernel. Three constraint stories decompose the kernel: (1) EFFECTS-JURISDICTION READING (this story): jurisdiction follows effects on EU residents; enforced via targeting/monitoring test; high compliance costs for non-EU controllers; beneficiary is EU data subject protection regime. (2) TERRITORIAL-SOVEREIGNTY READING: jurisdiction bounded by territorial sovereignty; extraterritorial application exceeds legitimate authority; lower compliance costs but disputes enforcement legitimacy. (3) MARKET-ACCESS READING: GDPR functions as conditional market access, not jurisdiction; Brussels Effect standard-setting rather than jurisdictional assertion; different beneficiary structure (market-access setter, not data protector). The three readings have different ε values: effects-jurisdiction reading shows high extractiveness (0.68) because non-EU controllers bear asymmetric costs; territorial-sovereignty reading would show lower extractiveness (compliance costs distributed); market-access reading would show lower extractiveness (cost is entry price, not extraction). Each is linked via network.affects_constraints so the constraint family can be analyzed together. The upstream story (effects-jurisdiction, closest to formal GDPR language and EU institutional position) influences the downstream stories (territorial and market-access readings offer alternative framings). Cross-story comparison reveals how the same kernel (Article 3(2) text) generates different constraint structures depending on interpretive reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gdpr_article_3_scope__effects_jurisdiction_reading, moderate, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
