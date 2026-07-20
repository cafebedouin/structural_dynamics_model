% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__effects_jurisdiction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: gdpr_article_3_scope__effects_jurisdiction_reading
 *   human_readable: GDPR Article 3(2) Effects Jurisdiction Reading
 *   domain: technology_governance/international_law/privacy_regulation
 *
 * SUMMARY:
 *   This constraint instantiates the effects jurisdiction reading of the
 *   contested GDPR Article 3 scope kernel. Under this reading, EU data
 *   protection law follows the data subject extraterritorially whenever a
 *   non-EU controller targets or monitors EU residents. The reading is
 *   contested by a territorial sovereignty reading (jurisdiction bounded by
 *   Member State territory) and a market access reading (extraterritoriality
 *   as Brussels Effect standard-setting rather than jurisdictional
 *   assertion). The constraint combines genuine protective coordination for
 *   EU residents with asymmetric compliance cost extraction from non-EU
 *   economic actors.
 *
 * KEY AGENTS:
 *   - eu_data_subjects: Primary beneficiary (organized/continental) â receive extraterritorial protective rights
 *   - eu_supervisory_authorities: Agenda setter (institutional/continental) â enforce and interpret Article 3(2)
 *   - non_eu_controllers: Primary payer (powerful/global) â bear compliance costs and legal uncertainty
 *   - non_eu_processors: Secondary payer (moderate/global) â face direct liability under controller instructions
 *   - third_country_governments: Excluded voice (institutional/global) â contest sovereignty but lack rulemaking seat
 *   - global_digital_rights_ngos: Analytical observer (organized/global) â corroborate protection rationale from outside beneficiary set
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__effects_jurisdiction_reading, 0.68).
domain_priors:suppression_score(gdpr_article_3_scope__effects_jurisdiction_reading, 0.72).
domain_priors:theater_ratio(gdpr_article_3_scope__effects_jurisdiction_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__effects_jurisdiction_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__effects_jurisdiction_reading, "GDPR Article 3(2) Effects Jurisdiction Reading").
narrative_ontology:topic_domain(gdpr_article_3_scope__effects_jurisdiction_reading, "technology_governance/international_law/privacy_regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__effects_jurisdiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__effects_jurisdiction_reading, 'a80469d8-022c-4f09-893b-c1edc2b11cac').
narrative_ontology:cs_kernel_codification('a80469d8-022c-4f09-893b-c1edc2b11cac', formalized).
narrative_ontology:cs_authority_grounding('a80469d8-022c-4f09-893b-c1edc2b11cac', lineage).
narrative_ontology:cs_interpretation_layer_present('a80469d8-022c-4f09-893b-c1edc2b11cac').
narrative_ontology:cs_reading_relation('a80469d8-022c-4f09-893b-c1edc2b11cac', gdpr_article_3_scope__territorial_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('a80469d8-022c-4f09-893b-c1edc2b11cac', gdpr_article_3_scope__market_access_reading, coexists_with).
narrative_ontology:cs_axiom('a80469d8-022c-4f09-893b-c1edc2b11cac', foundational, fundamental_rights_follow_the_person).
narrative_ontology:cs_axiom_status(fundamental_rights_follow_the_person, holdable).
narrative_ontology:cs_axiom_grounding('a80469d8-022c-4f09-893b-c1edc2b11cac', fundamental_rights_follow_the_person, deontological).
narrative_ontology:cs_axiom('a80469d8-022c-4f09-893b-c1edc2b11cac', foundational, effects_test_jurisdiction_valid).
narrative_ontology:cs_axiom_status(effects_test_jurisdiction_valid, holdable).
narrative_ontology:cs_axiom_grounding('a80469d8-022c-4f09-893b-c1edc2b11cac', effects_test_jurisdiction_valid, conventional).
narrative_ontology:cs_reference_frame('a80469d8-022c-4f09-893b-c1edc2b11cac', eu_fundamental_rights_framework).
narrative_ontology:cs_drift_state('a80469d8-022c-4f09-893b-c1edc2b11cac', contemporary_geopolitical_resistance, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('a80469d8-022c-4f09-893b-c1edc2b11cac', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_supervisory_authorities).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_controllers).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_processors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% EU residents whose personal data is processed by non-EU controllers; they receive protective rights including access, erasure, and portability, and may lodge complaints with supervisory authorities, but do not directly control enforcement against foreign entities.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects, beneficiary,
    organized, biographical, constrained, continental).

% Independent data protection authorities established under GDPR; they investigate non-EU controllers, impose administrative fines, and assess third-country adequacy, thereby extending EU regulatory reach extraterritorially while operating within the EU legal framework.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_supervisory_authorities, agenda_setter,
    institutional, generational, constrained, continental).

% Non-EU entities offering goods or services to EU residents or monitoring their behavior; they must appoint EU representatives, maintain legal basis documentation, and respond to subject requests, bearing significant compliance costs and exposure to fines.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_controllers, payer,
    powerful, biographical, constrained, global).

% Process personal data on behalf of non-EU controllers; face direct GDPR liability and contractual obligations to assist controllers with compliance, with limited leverage to renegotiate terms.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_processors, payer,
    moderate, biographical, constrained, global).

% Sovereign states whose legal regimes are assessed for adequacy by the EU; they contest extraterritorial application as an infringement of sovereignty and a trade barrier, but are structurally excluded from the GDPR rulemaking process.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, third_country_governments, excluded,
    institutional, generational, constrained, global).

% Monitor enforcement disparities and advocate for broad application of data subject rights across borders; provide external corroboration for the protective rationale without directly bearing costs or capturing gains.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, global_digital_rights_ngos, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gdpr_article_3_scope__effects_jurisdiction_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protecting the fundamental rights of EU data subjects by closing enforcement gaps when their personal data is processed outside the Union; establishing a common extraterritorial standard through the targeting and monitoring tests of Article 3(2).
% TRANSFER_FUNCTION: Moves compliance burdens, legal representation requirements, and financial liability from EU territory to non-EU controllers and processors; moves protective coverage and enforcement authority to EU supervisory authorities and data subjects.
% ABSENT_VOICES: Non-EU data subjects who lack equivalent protection; small non-EU controllers without legal resources to interpret the targeting and monitoring test; third-country governments whose sovereignty concerns are marginalized in the EU legislative process.
% DISAPPEARANCE_RATIONALE: If the effects jurisdiction reading vanished overnight, non-EU controllers would dismantle EU representative structures and local compliance infrastructure, enforcement actions against foreign entities would lose legal basis, and EU data subjects would lack recourse against extraterritorial processing; the global data protection landscape would revert to territorial fragmentation.
% FOUNDING_PROBLEM: EU data subjects' personal data was routinely processed by non-EU entities with no enforceable rights, creating an enforcement gap that undermined the level of protection guaranteed within the Union.
% FOUNDING_PROBLEM_CORROBORATION: EU supervisory authorities and digital rights NGOs attest the problem remains live, citing continued transfers to jurisdictions without adequate protection. Third-country governments and some international trade scholars contest that the problem justifies extraterritorial reach; no neutral international body has corroborated the specific jurisdictional solution.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__effects_jurisdiction_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__effects_jurisdiction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__effects_jurisdiction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gdpr_article_3_scope__effects_jurisdiction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__effects_jurisdiction_reading, 0.68, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.68) because non-EU controllers face substantial compliance costs that are partially decoupled from the marginal protective benefit they produce. Suppression (0.72) reflects active enforcement through adequacy mechanisms, administrative fines, and the obligation to maintain EU representatives. Theater ratio (0.25) is moderate: much compliance is substantive, but a growing fraction involves box-checking and cosmetic legal structuring. Accessibility collapse (0.45) is moderate because alternatives such as geoblocking exist but are costly. Resistance (0.55) captures sustained third-state objections and industry lobbying.
 *
 * PERSPECTIVAL GAP:
 *   From the EU supervisory authority seat, the constraint is a necessary coordination mechanism closing an enforcement gap in fundamental rights protection. From the non-EU controller seat, it is an asymmetric extraction of compliance surplus backed by the threat of substantial fines. The engine computes this divergence from the structural asymmetry in power and exit: the authority has a generational time horizon and institutional power, while the controller faces a biographical horizon and constrained exit despite being globally powerful.
 *
 * DIRECTIONALITY LOGIC:
 *   EU data subjects and supervisory authorities are structural beneficiaries (low d): the constraint subsidizes their protective position and regulatory authority. Non-EU controllers and processors are structural targets (high d): the constraint extracts compliance costs and legal risk from them, amplified by their global scope and constrained exit options. Third-country governments are excluded from the framework but bear diplomatic costs; their directionality is intermediate but skewed toward target due to sovereignty costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy mislabeling by requiring both a genuine coordination function (protecting data subjects) and identifiable victims (non-EU controllers bearing costs). Without the coordination function, the extraterritorial reach would be a pure snare; without the victim identification, it would be misread as a rope. The Tangled Rope classification captures the hybrid: the protection is real, but so is the asymmetric cost transfer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraterritorial_legitimacy_under_international_law,
    'Does the effects-based extraterritorial jurisdiction of GDPR Article 3(2) comport with legitimate international law principles of jurisdiction, or does it constitute regulatory overreach?',
    'ICJ advisory opinion or widespread treaty codification explicitly accepting or rejecting effects-based data protection jurisdiction.',
    'If deemed illegitimate under international law, the constraint''s persistence relies on economic coercion rather than legal authority, shifting classification toward snare. If legitimate, the coordination function is legally grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraterritorial_legitimacy_under_international_law, conceptual, 'Whether effects-based data protection jurisdiction is legally legitimate or overreach').

omega_variable(
    compliance_cost_protection_benefit_balance,
    'Do the compliance costs imposed on non-EU controllers produce a commensurate protective benefit for EU data subjects, or do costs exceed benefits?',
    'Empirical study comparing data breach rates, subject complaint resolution, and controller compliance expenditures before and after the activation of extraterritorial enforcement.',
    'If costs exceed benefits, extraction dominates the coordination function; if balanced, tangled_rope remains appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_cost_protection_benefit_balance, empirical, 'Balance between compliance costs on non-EU controllers and protective benefits').

omega_variable(
    enforcement_disparity_across_sectors,
    'Does enforcement concentrate on high-profile tech giants while small non-EU controllers evade compliance, creating selective extraction?',
    'Cross-sectional analysis of enforcement actions by controller size, sector, and geographic origin.',
    'Selective enforcement would indicate the constraint functions as a targeted snare against specific actors rather than uniform coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_disparity_across_sectors, empirical, 'Whether enforcement is uniform or selectively targets specific controller classes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__effects_jurisdiction_reading, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t0, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(gdpr_tr_t2, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2, 0.16).
narrative_ontology:measurement(gdpr_tr_t4, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(gdpr_tr_t6, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 6, 0.23).
narrative_ontology:measurement(gdpr_tr_t8, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 8, 0.25).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t0, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(gdpr_be_t2, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2, 0.55).
narrative_ontology:measurement(gdpr_be_t4, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 4, 0.61).
narrative_ontology:measurement(gdpr_be_t6, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 6, 0.65).
narrative_ontology:measurement(gdpr_be_t8, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 8, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_su_t0, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(gdpr_su_t2, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2, 0.58).
narrative_ontology:measurement(gdpr_su_t4, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 4, 0.65).
narrative_ontology:measurement(gdpr_su_t6, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 6, 0.69).
narrative_ontology:measurement(gdpr_su_t8, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 8, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__effects_jurisdiction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope__territorial_sovereignty_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope__market_access_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the gdpr_article_3_scope kernel. The effects jurisdiction reading asserts that personal data protection follows the data subject via targeting/monitoring tests; the territorial sovereignty reading bounds jurisdiction by Member State territory; the market access reading frames extraterritorial application as Brussels Effect standard-setting rather than jurisdictional assertion. Each reading instantiates a structurally distinct constraint with its own epsilon and stakeholder configuration.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
