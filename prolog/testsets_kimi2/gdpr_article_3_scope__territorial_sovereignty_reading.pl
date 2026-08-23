% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__territorial_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gdpr_article_3_scope__territorial_sovereignty_reading, []).

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
 *   constraint_id: gdpr_article_3_scope__territorial_sovereignty_reading
 *   human_readable: GDPR Article 3(2) Extraterritorial Reach â Territorial Sovereignty Reading
 *   domain: technology_governance/international_law/privacy_regulation
 *
 * SUMMARY:
 *   This constraint story instantiates the territorial_sovereignty_reading of
 *   the gdpr_article_3_scope kernel. The standing arrangement under contest
 *   is the extraterritorial application of GDPR via Article 3(2) (the
 *   targeting/monitoring test). Assessed by this reading's own lights, the
 *   arrangement extracts compliance and sovereign autonomy from non-EU actors
 *   while coordinating data protection for EU residents. The reading holds
 *   that the kernel's legitimate scope is bounded by territorial sovereignty
 *   and that Article 3(2) exceeds that boundary, constituting an asymmetric
 *   extraction mechanism backed by active enforcement.
 *
 * KEY AGENTS:
 *   - EU data protection authorities: agenda_setter (institutional/analytical) â set and enforce the jurisdictional boundary.
 *   - EU data subjects: beneficiary (organized/constrained) â receive protection; their residence triggers the hook.
 *   - Non-EU data controllers: payer (powerful/constrained) â bear compliance costs and legal risk; market access dependency limits exit.
 *   - Third-country governments: payer (institutional/constrained) â bear sovereignty erosion costs and mount resistance via localization statutes.
 *   - International law scholars: observer (analytical/analytical) â evaluate compliance with public international law jurisdictional limits.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__territorial_sovereignty_reading, 0.78).
domain_priors:suppression_score(gdpr_article_3_scope__territorial_sovereignty_reading, 0.72).
domain_priors:theater_ratio(gdpr_article_3_scope__territorial_sovereignty_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__territorial_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__territorial_sovereignty_reading, "GDPR Article 3(2) Extraterritorial Reach â Territorial Sovereignty Reading").
narrative_ontology:topic_domain(gdpr_article_3_scope__territorial_sovereignty_reading, "technology_governance/international_law/privacy_regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__territorial_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__territorial_sovereignty_reading, '5814c9b9-f305-4067-9fd5-140893c8080c').
narrative_ontology:cs_kernel_codification('5814c9b9-f305-4067-9fd5-140893c8080c', formalized).
narrative_ontology:cs_authority_grounding('5814c9b9-f305-4067-9fd5-140893c8080c', lineage).
narrative_ontology:cs_interpretation_layer_present('5814c9b9-f305-4067-9fd5-140893c8080c').
narrative_ontology:cs_reading_relation('5814c9b9-f305-4067-9fd5-140893c8080c', gdpr_article_3_scope__effects_jurisdiction_reading, influences).
narrative_ontology:cs_reading_relation('5814c9b9-f305-4067-9fd5-140893c8080c', gdpr_article_3_scope__market_access_reading, coexists_with).
narrative_ontology:cs_axiom('5814c9b9-f305-4067-9fd5-140893c8080c', foundational, jurisdiction_bounded_by_territory).
narrative_ontology:cs_axiom_status(jurisdiction_bounded_by_territory, holdable).
narrative_ontology:cs_axiom_grounding('5814c9b9-f305-4067-9fd5-140893c8080c', jurisdiction_bounded_by_territory, conventional).
narrative_ontology:cs_axiom('5814c9b9-f305-4067-9fd5-140893c8080c', foundational, extraterritorial_presumption_illegitimate).
narrative_ontology:cs_axiom_status(extraterritorial_presumption_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('5814c9b9-f305-4067-9fd5-140893c8080c', extraterritorial_presumption_illegitimate, conventional).
narrative_ontology:cs_reference_frame('5814c9b9-f305-4067-9fd5-140893c8080c', strict_territorial_jurisdiction).
narrative_ontology:cs_drift_state('5814c9b9-f305-4067-9fd5-140893c8080c', gdpr_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5814c9b9-f305-4067-9fd5-140893c8080c', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, eu_data_subjects).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, eu_data_protection_authorities).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_data_controllers).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, third_country_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce Article 3(2) against non-EU controllers through investigations, fines, and adequacy determinations. They set the jurisdictional perimeter by asserting that targeting or monitoring EU residents triggers GDPR obligations regardless of the controller's location. Their institutional authority expands with each extraterritorial enforcement action.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, eu_data_protection_authorities, agenda_setter,
    institutional, generational, analytical, continental).

% Receive data protection coverage from controllers worldwide by virtue of their EU residence or location. They cannot individually opt out of the jurisdictional hook; their presence in the EU is what triggers the extraterritorial obligation.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, eu_data_subjects, beneficiary,
    organized, biographical, constrained, continental).

% Multinational and mid-sized controllers outside the EU must maintain GDPR compliance programs, legal representation in the EU, and face fines up to four percent of global turnover if they offer goods or services to, or monitor, EU residents. Market access dependency makes exit from the EU channel costly, while full compliance imposes dual regulatory burdens.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_data_controllers, payer,
    powerful, biographical, constrained, global).

% Experience erosion of regulatory autonomy over data governance within their own territory as EU law reaches their domestic entities. Respond with data localization statutes, blocking regulations, and diplomatic objections to reassert sovereign control over data processing governed by their own legal frameworks.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, third_country_governments, payer,
    institutional, generational, constrained, national).

% Analyze the compatibility of Article 3(2) with customary international law limits on prescriptive jurisdiction. They document the split between effects-based and territorial theories of jurisdiction and assess whether the EU assertion constitutes a lawful extension or an overreach.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents controllers from evading EU data protection standards by relocating processing infrastructure or corporate seats outside the Union, thereby closing a territorial loophole in the protection of EU residents.
% TRANSFER_FUNCTION: Moves compliance burdens, legal risk, and regulatory submission from non-EU controllers and third-country governments to the EU legal order, extracting adherence to EU privacy standards as a condition of engaging with EU residents.
% ABSENT_VOICES: Third-country governments and non-EU SMEs are excluded from the EU legislative process that adopted Article 3(2); their opposition is expressed through diplomatic channels, WTO dispute rhetoric, and unilateral blocking statutes rather than inside the EU regulatory conversation. Non-EU data subjects whose own domestic regimes may offer different privacy balances are not heard in the EU's unilateral jurisdictional assertion.
% DISAPPEARANCE_RATIONALE: If the extraterritorial application vanished overnight, non-EU controllers would shed parallel GDPR compliance layers for non-EU operations, third-country governments would ease data localization and blocking statutes, EU DPAs would lose their global enforcement reach, and cross-border data governance would reorganize around territorial boundaries.
% FOUNDING_PROBLEM: Data controllers could evade EU data protection law by shifting processing outside the Union, creating a loophole that undermined the protection of EU residents.
% FOUNDING_PROBLEM_CORROBORATION: The EU Commission and DPAs attest the problem remains live. Third-country governments and international law scholars from outside the EU attest that the loophole is closed by Article 3(1) establishment jurisdiction, and that Article 3(2) goes beyond the founding problem into illegitimate extraterritoriality.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__territorial_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__territorial_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__territorial_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gdpr_article_3_scope__territorial_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__territorial_sovereignty_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gdpr_article_3_scope__territorial_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gdpr_article_3_scope__territorial_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gdpr_article_3_scope__territorial_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because non-EU controllers must maintain dual compliance regimes and face existential fines; suppression (0.72) is high because the EU actively blocks alternative jurisdictional arrangements through adequacy requirements and extraterritorial fines. Theater ratio (0.45) is moderate: the data protection function is genuine, but an increasing share of enforcement performs EU digital sovereignty rather than protecting residents. Accessibility collapse (0.60) reflects that market access dependency makes exit difficult, though data localization offers a partial alternative. Resistance (0.68) captures active third-country blocking statutes and diplomatic opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the EU DPA seat, the arrangement is necessary coordination to close a territorial loophole. From the non-EU controller and third-country government seats, the same structure is unilateral regulatory extraction that disrespects sovereign boundaries. The engine computes this divergence from the structural data; the reading does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   EU data subjects and DPAs sit near the beneficiary end: the constraint subsidizes their protection and expands their authority. Non-EU data controllers and third-country governments sit near the target end: they bear compliance costs and sovereignty erosion. The structural asymmetry is geographic and jurisdictional, not merely economic.
 *
 * MANDATROPHY ANALYSIS:
 *   The territorial sovereignty reading prevents mislabeling the constraint as pure coordination (rope) by foregrounding the asymmetric extraction on non-EU parties. It does not claim the founding mandate (protecting EU residents) has atrophied; rather, it claims the means (extraterritorial jurisdiction) exceed the mandate's legitimate scope, creating a hybrid coordination-extraction structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    territorial_sovereignty_vs_effects,
    'Does customary international law permit effects-based prescriptive jurisdiction for data protection, or does it require a strict territorial nexus?',
    'ICJ advisory opinion or comprehensive survey of state practice and opinio juris on data-protection jurisdiction.',
    'If territorial sovereignty is the governing rule, GDPR Article 3(2) is an illegitimate extraterritorial snare; if effects-based jurisdiction is permitted, the extraction is at least partially legitimate under international law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_sovereignty_vs_effects, conceptual, 'Whether the extraterritorial reach violates international law.').

omega_variable(
    data_localization_resistance_efficacy,
    'Does data localization by third countries successfully restore regulatory independence against GDPR extraterritoriality, or does it impose added costs without escaping the long-arm?',
    'Comparative empirical study of localization statutes and their interaction with GDPR enforcement and adequacy politics.',
    'If localization is ineffective, the constraint''s effective suppression and extraction are higher than structural measures suggest because exit is illusory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(data_localization_resistance_efficacy, empirical, 'Whether data localization is a real exit or a costly gesture.').

omega_variable(
    coordination_extraction_separability,
    'Is the protection of EU residents via extraterritorial jurisdiction structurally separable from the sovereignty costs imposed on third countries?',
    'Natural experiment or counterfactual analysis: whether equivalent resident protection could be achieved through territorial or market-access mechanisms without prescriptive jurisdiction over foreign soil.',
    'If separable, the sovereignty cost is pure extraction riding on a real coordination function; if inseparable, the extraction is the necessary price of the coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether coordination and extraction components are structurally separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__territorial_sovereignty_reading, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_territorial_tr_t0, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(gdpr_territorial_tr_t2, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2, 0.28).
narrative_ontology:measurement(gdpr_territorial_tr_t4, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 4, 0.35).
narrative_ontology:measurement(gdpr_territorial_tr_t6, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 6, 0.4).
narrative_ontology:measurement(gdpr_territorial_tr_t8, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 8, 0.45).

% Extraction over time
narrative_ontology:measurement(gdpr_territorial_be_t0, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gdpr_territorial_be_t2, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(gdpr_territorial_be_t4, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 4, 0.6).
narrative_ontology:measurement(gdpr_territorial_be_t6, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 6, 0.7).
narrative_ontology:measurement(gdpr_territorial_be_t8, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 8, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_territorial_su_t0, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(gdpr_territorial_su_t2, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2, 0.58).
narrative_ontology:measurement(gdpr_territorial_su_t4, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 4, 0.66).
narrative_ontology:measurement(gdpr_territorial_su_t6, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 6, 0.7).
narrative_ontology:measurement(gdpr_territorial_su_t8, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 8, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__territorial_sovereignty_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
