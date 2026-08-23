% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__effects_jurisdiction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-26
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
 *   human_readable: GDPR Article 3(2) Effects Jurisdiction Extraterritorial Reach
 *   domain: technology_governance/international_law/privacy_regulation
 *
 * SUMMARY:
 *   This constraint instantiates the effects-jurisdiction reading of GDPR
 *   Article 3(2), under which EU data protection law follows the effects of
 *   processing on EU residents regardless of where the data controller is
 *   established. The kernel gdpr_article_3_scope is contested: the
 *   territorial-sovereignty reading holds that extraterritorial application
 *   exceeds legitimate regulatory authority, while the market-access reading
 *   reframes the mechanism as Brussels Effect standard-setting rather than
 *   jurisdictional assertion. This story isolates the effects reading as a
 *   structurally independent constraint with its own epsilon, beneficiaries,
 *   and victim set.
 *
 * KEY AGENTS:
 *   - EU data protection authorities (agenda_setter/institutional): enforce the extraterritorial reach, interpret targeting and monitoring tests, and levy fines.
 *   - EU data subjects (beneficiary/powerless): receive statutory privacy protections against non-EU processing.
 *   - Global tech platforms (payer/powerful): bear high compliance costs and fine exposure but cannot abandon the EU market.
 *   - Small non-EU controllers (payer/moderate): lack resources to comply and often geoblock EU users.
 *   - Non-EU governments (excluded/institutional): contest extraterritoriality but are structurally excluded from the GDPR enforcement conversation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__effects_jurisdiction_reading, 0.58).
domain_priors:suppression_score(gdpr_article_3_scope__effects_jurisdiction_reading, 0.62).
domain_priors:theater_ratio(gdpr_article_3_scope__effects_jurisdiction_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__effects_jurisdiction_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__effects_jurisdiction_reading, "GDPR Article 3(2) Effects Jurisdiction Extraterritorial Reach").
narrative_ontology:topic_domain(gdpr_article_3_scope__effects_jurisdiction_reading, "technology_governance/international_law/privacy_regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__effects_jurisdiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__effects_jurisdiction_reading, '2f710c5f-ba79-481f-889a-7826d9c49afe').
narrative_ontology:cs_kernel_codification('2f710c5f-ba79-481f-889a-7826d9c49afe', formalized).
narrative_ontology:cs_authority_grounding('2f710c5f-ba79-481f-889a-7826d9c49afe', lineage).
narrative_ontology:cs_interpretation_layer_present('2f710c5f-ba79-481f-889a-7826d9c49afe').
narrative_ontology:cs_reading_relation('2f710c5f-ba79-481f-889a-7826d9c49afe', gdpr_article_3_scope__territorial_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('2f710c5f-ba79-481f-889a-7826d9c49afe', gdpr_article_3_scope__market_access_reading, influences).
narrative_ontology:cs_axiom('2f710c5f-ba79-481f-889a-7826d9c49afe', foundational, effective_protection_requires_effects_jurisdiction).
narrative_ontology:cs_axiom_status(effective_protection_requires_effects_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('2f710c5f-ba79-481f-889a-7826d9c49afe', effective_protection_requires_effects_jurisdiction, empirically_contingent).
narrative_ontology:cs_axiom('2f710c5f-ba79-481f-889a-7826d9c49afe', secondary, eu_data_subjects_retain_rights_against_foreign_processors).
narrative_ontology:cs_axiom_status(eu_data_subjects_retain_rights_against_foreign_processors, holdable).
narrative_ontology:cs_axiom_grounding('2f710c5f-ba79-481f-889a-7826d9c49afe', eu_data_subjects_retain_rights_against_foreign_processors, deontological).
narrative_ontology:cs_reference_frame('2f710c5f-ba79-481f-889a-7826d9c49afe', eu_data_protection_effects_principle).
narrative_ontology:cs_drift_state('2f710c5f-ba79-481f-889a-7826d9c49afe', contemporary_extraterritorial_enforcement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2f710c5f-ba79-481f-889a-7826d9c49afe', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, global_tech_platforms).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, small_non_eu_controllers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer and enforce GDPR across Member States, issuing guidelines on extraterritorial application, levying fines against non-EU controllers, and negotiating adequacy decisions. Their authority derives from the regulation's text and CJEU interpretation.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_protection_authorities, agenda_setter,
    institutional, generational, analytical, continental).

% Individuals physically in the EU whose personal data is processed by non-EU websites, apps, and services. They gain statutory data protection rights, access mechanisms, and the ability to lodge complaints regardless of where the data controller is established.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects, beneficiary,
    powerless, biographical, constrained, continental).

% Large non-EU technology firms that target or monitor EU residents. They maintain expensive EU-specific legal and technical compliance infrastructures, face billion-euro fine exposure, and cannot easily abandon the EU market without significant revenue loss.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, global_tech_platforms, payer,
    powerful, biographical, constrained, global).

% Small and medium non-EU websites, publishers, and SaaS providers that inadvertently monitor EU residents through analytics or cookies. Many lack legal resources to assess compliance, face existential fine risk, and often respond by geoblocking EU users rather than adapting.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, small_non_eu_controllers, payer,
    moderate, biographical, trapped, global).

% Foreign trade ministries and diplomatic corps that contest the extraterritorial application as an overreach of regulatory sovereignty. They are largely excluded from the EU's interpretive and enforcement process, lodging objections through international forums rather than the GDPR's administrative framework.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_governments, excluded,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gdpr_article_3_scope__effects_jurisdiction_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protects the fundamental right to data protection for individuals physically in the EU by ensuring that processing of their personal data by non-EU actors is governed by consistent legal standards, regardless of where the processing entity is established.
% TRANSFER_FUNCTION: Moves compliance costs and legal risk from EU residents to non-EU controllers, who must implement EU-standard data protection infrastructure, face DPA oversight, and risk fines, enforced through the threat of market exclusion and monetary penalties.
% ABSENT_VOICES: Non-EU governments and foreign trade ministries contest the extraterritorial reach as regulatory overreach; small non-EU publishers silently geoblock EU users rather than entering the compliance conversation.
% DISAPPEARANCE_RATIONALE: If Article 3(2) effects jurisdiction vanished overnight, non-EU controllers would shed their EU compliance infrastructure for non-EU operations, EU residents would lose statutory data protection rights against foreign processors, and the global data governance landscape would fragment into competing territorial regimes.
% FOUNDING_PROBLEM: The inability of purely territorial privacy law to protect EU residents whose data is processed by global digital services physically located outside the EU, creating an enforcement gap that rendered substantive rights illusory in a borderless internet.
% FOUNDING_PROBLEM_CORROBORATION: The European Commission and EU DPAs attest the problem is live, citing continued non-EU processing of EU resident data. Non-EU governments and trade bodies contest the framing, arguing mutual legal assistance and bilateral frameworks already address the gap; academic comparative law scholarship from outside the benefiting parties documents both the enforcement gap and the sovereignty tension.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__effects_jurisdiction_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__effects_jurisdiction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__effects_jurisdiction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gdpr_article_3_scope__effects_jurisdiction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__effects_jurisdiction_reading, 0.58, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.58) is substantial because non-EU controllers face significant compliance burdens, legal representation costs, and fine exposure that is decoupled from their domestic regulatory frameworks. Suppression (0.62) is slightly higher than extractiveness because the constraint's persistence depends on active enforcementâadequacy decisions, billion-euro fines, and the procedural machinery of cross-border cooperationârather than voluntary adoption. Theater ratio (0.30) reflects moderate performative maintenance: high-profile fines against major platforms generate visibility that sustains the regime's credibility, but the underlying coordination function (rights protection) remains operative. Accessibility collapse (0.45) is incomplete because geoblocking and market withdrawal remain viable alternatives for some non-EU controllers. Resistance (0.55) captures active diplomatic pushback and strategic non-compliance by third states.
 *
 * PERSPECTIVAL GAP:
 *   The EU data subject seat experiences this constraint as protective coordinationâenforceable rights regardless of where their data travels. The non-EU controller seats experience the identical legal text as asymmetric extractionâcostly extraterritorial obligations imposed by a foreign regulator. The engine computes this divergence from the structural data: identical scope, opposite beneficiary/victim declarations, and divergent exit options (constrained market withdrawal vs. constrained but subsidized rights access).
 *
 * DIRECTIONALITY LOGIC:
 *   EU data subjects are declared beneficiaries (low d, extraction damped into subsidy). Non-EU controllers are declared victims/payers (high d, extraction amplified). EU DPAs sit near the beneficiary end as administrators of the regime, though they do not personally collect the transfer. Global tech platforms have powerful resources but constrained exit because EU market revenue is irreplaceable, placing their d above the moderate midpoint. Small non-EU controllers with trapped exit (geoblock or risk ruin) sit nearest the full-target end.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâa borderless internet rendering territorial privacy law ineffectiveâis genuinely live, which prevents misclassifying this constraint as a pure snare. However, the arrangement does not collapse to a pure rope because the compliance costs are borne by non-EU parties who receive no corresponding benefit from the coordination. The asymmetric distribution of costs and benefits, combined with active enforcement against non-consenting extraterritorial parties, satisfies the tangled rope gate rather than the rope or snare extremes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    effects_vs_territorial_fracture,
    'Does the effects-jurisdiction reading of Article 3(2) foreclose the territorial-sovereignty reading within a single interpretive framework, or can both readings coexist as live legal positions?',
    'ICJ advisory proceeding or WTO dispute settlement ruling on the legitimacy of effects-based data jurisdiction under customary international law.',
    'If foreclosed toward territoriality, the constraint''s enforcement scope narrows and extractiveness falls; if coexistence is stable, the ambiguity itself becomes a source of compliance pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effects_vs_territorial_fracture, conceptual, 'Whether effects jurisdiction and territorial sovereignty can coexist as interpretations of the same kernel.').

omega_variable(
    compliance_cost_benefit_asymmetry,
    'Does the compliance cost borne by non-EU controllers produce a proportionate privacy benefit for EU data subjects, or does the cost structure exceed the protective value?',
    'Empirical measurement of privacy outcomes for EU residents against compliance spend by non-EU controllers, including deadweight loss from geoblocking.',
    'If costs exceed benefits by a wide margin, the coordination function is weaker than claimed and the constraint shifts toward snare classification; if proportionate, the tangled rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_cost_benefit_asymmetry, empirical, 'Whether extraction is proportionate to the coordination benefit.').

omega_variable(
    enforcement_selectivity,
    'Is enforcement of Article 3(2) against non-EU controllers structurally limited to high-profile targets, creating a de facto selective suppression?',
    'Comprehensive analysis of DPA enforcement actions by target location, size, and sector, comparing complaint-driven vs ex officio cases.',
    'If enforcement is selective, smaller non-EU controllers face nominal but not actual extraction (lowering effective suppression), while large platforms bear the full brunt; this bifurcation would require split stakeholder treatment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_selectivity, empirical, 'Whether suppression is evenly applied or concentrated on visible targets.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__effects_jurisdiction_reading, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t0, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(gdpr_tr_t2, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2, 0.22).
narrative_ontology:measurement(gdpr_tr_t4, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(gdpr_tr_t6, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 6, 0.29).
narrative_ontology:measurement(gdpr_tr_t8, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 8, 0.3).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t0, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(gdpr_be_t2, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(gdpr_be_t4, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(gdpr_be_t6, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 6, 0.56).
narrative_ontology:measurement(gdpr_be_t8, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 8, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_su_t0, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(gdpr_su_t2, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2, 0.48).
narrative_ontology:measurement(gdpr_su_t4, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 4, 0.54).
narrative_ontology:measurement(gdpr_su_t6, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 6, 0.59).
narrative_ontology:measurement(gdpr_su_t8, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 8, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__effects_jurisdiction_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
