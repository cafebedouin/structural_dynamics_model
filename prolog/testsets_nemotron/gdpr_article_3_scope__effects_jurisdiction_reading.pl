% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__effects_jurisdiction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: gdpr_article_3_scope__effects_jurisdiction_reading
 *   human_readable: GDPR Article 3(2) Effects Jurisdiction Reading — Extraterritorial Protection via Targeting/Monitoring Test
 *   domain: technology_governance/international_law/privacy_regulation
 *
 * SUMMARY:
 *   The GDPR Article 3(2) 'effects jurisdiction' reading asserts that any
 *   controller or processor outside the EU that targets EU residents
 *   (offering goods/services) or monitors their behaviour falls under GDPR
 *   jurisdiction. This reading — instantiated by the EDPB Guidelines 3/2018
 *   and CJEU jurisprudence (Weltimmo, Google Spain, Schrems II) — creates a
 *   global compliance obligation anchored in effects on EU data subjects
 *   rather than territorial presence. The constraint is structurally a
 *   tangled rope: it solves a genuine coordination problem (regulatory
 *   arbitrage / race to the bottom) while extracting asymmetric compliance
 *   costs from non-EU actors who lack democratic representation in the EU
 *   legislative process. The beneficiary is the EU data subject protection
 *   regime (fundamental rights framework); the payers are non-EU
 *   controllers/processors, with disproportionate burden on small services.
 *   Enforcement is active (fines, adequacy mechanisms, representative
 *   requirements) and the reading has expanded through CJEU interpretation
 *   beyond the original legislative compromise.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__effects_jurisdiction_reading, 0.68).
domain_priors:suppression_score(gdpr_article_3_scope__effects_jurisdiction_reading, 0.55).
domain_priors:theater_ratio(gdpr_article_3_scope__effects_jurisdiction_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__effects_jurisdiction_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__effects_jurisdiction_reading, "GDPR Article 3(2) Effects Jurisdiction Reading — Extraterritorial Protection via Targeting/Monitoring Test").
narrative_ontology:topic_domain(gdpr_article_3_scope__effects_jurisdiction_reading, "technology_governance/international_law/privacy_regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__effects_jurisdiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__effects_jurisdiction_reading, '305934da-6c7c-44ea-aaf0-6bd8455546eb').
narrative_ontology:cs_kernel_codification('305934da-6c7c-44ea-aaf0-6bd8455546eb', formalized).
narrative_ontology:cs_authority_grounding('305934da-6c7c-44ea-aaf0-6bd8455546eb', lineage).
narrative_ontology:cs_interpretation_layer_present('305934da-6c7c-44ea-aaf0-6bd8455546eb').
narrative_ontology:cs_reading_relation('305934da-6c7c-44ea-aaf0-6bd8455546eb', gdpr_article_3_scope__territorial_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('305934da-6c7c-44ea-aaf0-6bd8455546eb', gdpr_article_3_scope__market_access_reading, coexists_with).
narrative_ontology:cs_axiom('305934da-6c7c-44ea-aaf0-6bd8455546eb', foundational, data_protection_rights_follow_the_data_subject).
narrative_ontology:cs_axiom_status(data_protection_rights_follow_the_data_subject, holdable).
narrative_ontology:cs_axiom_grounding('305934da-6c7c-44ea-aaf0-6bd8455546eb', data_protection_rights_follow_the_data_subject, deontological).
narrative_ontology:cs_axiom('305934da-6c7c-44ea-aaf0-6bd8455546eb', foundational, extraterritorial_effectiveness_required_for_rights_realization).
narrative_ontology:cs_axiom_status(extraterritorial_effectiveness_required_for_rights_realization, holdable).
narrative_ontology:cs_axiom_grounding('305934da-6c7c-44ea-aaf0-6bd8455546eb', extraterritorial_effectiveness_required_for_rights_realization, instrumental).
narrative_ontology:cs_reference_frame('305934da-6c7c-44ea-aaf0-6bd8455546eb', eu_fundamental_rights_jurisdiction).
narrative_ontology:cs_drift_state('305934da-6c7c-44ea-aaf0-6bd8455546eb', post_schrems_ii_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('305934da-6c7c-44ea-aaf0-6bd8455546eb', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_protection_authorities).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_controllers).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_processors).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, small_non_eu_digital_services).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__effects_jurisdiction_reading, data_protection_as_fundamental_right).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__effects_jurisdiction_reading, extraterritorial_effectiveness_of_rights).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__effects_jurisdiction_reading, digital_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% EU residents whose personal data is processed by controllers worldwide. The reading extends GDPR protection to them regardless of the controller's location. Their data protection rights are constitutive of their legal identity within the EU legal order; exiting this protection would mean relinquishing fundamental rights guarantees. They do not choose this constraint — it is the framework that constitutes their digital personhood.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects, beneficiary,
    powerless, biographical, identity_locked, regional).

% National supervisory authorities (DPAs) and the EDPB that interpret, enforce, and coordinate the effects jurisdiction reading. They issue guidelines on the targeting/monitoring test, coordinate cross-border enforcement, and levy fines. Their institutional mandate and resource base expand with the reading's scope. They are not neutral arbiters — their authority grows when the reading expands.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_protection_authorities, agenda_setter,
    institutional, generational, analytical, regional).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_protection_authorities, beneficiary).

% Large non-EU companies (US tech platforms, global SaaS providers, multinational corporations) that target or monitor EU residents. They bear substantial compliance costs: DPO appointments, representative establishment, DPIAs, adequacy mechanisms, breach notification, and fine exposure up to 4% global turnover. Their exit option is withdrawing from the EU market — costly but structurally possible. They lobby for narrower readings (market_access_reading) and challenge enforcement in courts.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_controllers, payer,
    powerful, biographical, constrained, global).

% Cloud providers, analytics services, advertising networks, and other processors outside the EU that process data on behalf of controllers subject to the effects jurisdiction. They inherit compliance obligations through Article 28 contracts and face direct liability. Their exit is constrained by the global nature of digital infrastructure — they cannot easily segregate EU data flows without architectural changes.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_processors, payer,
    organized, biographical, constrained, global).

% Startups, SMEs, and individual developers outside the EU who incidentally reach EU users. They face disproportionate compliance burdens relative to revenue: no legal teams, no EU representative budget, no DPIA capacity. Their exit option is geo-blocking EU users — technically feasible but commercially damaging. They are the most extracted-from seat per unit of power.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, small_non_eu_digital_services, payer,
    moderate, immediate, constrained, global).

% The Court of Justice of the EU that authoritatively interprets Article 3(2) in cases like Weltimmo, Google Spain, Schrems II, and Meta v. Bundeskartellamt. Their jurisprudence has consistently expanded the effects reading. They do not collect rents but their interpretive authority is the engine of the reading's expansion.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_courts_cjeu, agenda_setter,
    institutional, generational, analytical, regional).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__effects_jurisdiction_reading, eu_courts_cjeu, observer).

% Data protection authorities and legislatures outside the EU (UK ICO, US FTC, Canadian OPC, Brazilian ANPD, etc.) who must navigate conflicting jurisdictional claims. They would argue for territorial sovereignty or comity-based approaches but are structurally excluded from the EU's interpretive process. Their regulatory autonomy is constrained by the Brussels Effect.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_regulators, excluded,
    institutional, generational, trapped, global).

% Academics, advocates, and practitioners who analyze the jurisdictional reach, compliance mechanics, and fundamental rights implications. They produce the interpretive literature that feeds back into DPA guidelines and CJEU reasoning. Their seat is analytical — they observe the structure without bearing its costs or collecting its rents.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, legal_scholars_practitioners, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the regulatory arbitrage problem: without extraterritorial reach, controllers could evade EU data protection law by locating processing outside the EU while targeting EU residents. The targeting/monitoring test creates a coherent jurisdictional hook that prevents a race to the bottom in data protection standards.
% TRANSFER_FUNCTION: Moves compliance costs (legal, technical, organizational) from EU data subjects and DPAs to non-EU controllers and processors. The transfer is asymmetrical: large controllers absorb costs as cost of market access; small services face existential burdens. Fines (up to 4% global turnover) transfer value from non-EU entities to EU member state treasuries.
% ABSENT_VOICES: Non-EU regulators and legislatures who would assert territorial sovereignty or comity-based frameworks are excluded from the EU's interpretive process. Small non-EU digital services lack representation in Brussels lobbying. Individual non-EU developers who geo-block EU users rather than comply are invisible in the policy record. The 'Brussels Effect' literature documents this exclusion as structural, not incidental.
% DISAPPEARANCE_RATIONALE: If the effects jurisdiction reading vanished overnight, non-EU controllers would immediately cease GDPR compliance for EU-targeted processing unless independently motivated. EU data subjects would lose enforceable rights against foreign processors. The global data protection standard-setting dynamic (Brussels Effect) would collapse. Adequacy decisions would lose their anchor. The transatlantic data flow framework would require renegotiation from scratch.
% FOUNDING_PROBLEM: The 1995 Data Protection Directive's territorial scope (establishment-based) allowed controllers to evade EU law by processing data outside the EU while targeting EU residents. The GDPR negotiators (2012-2016) designed Article 3(2) to close this arbitrage, grounding jurisdiction in the effects on EU residents rather than the controller's location.
% FOUNDING_PROBLEM_CORROBORATION: The European Commission's 2012 impact assessment and the LIBE Committee reports corroborate the regulatory arbitrage problem as the founding motivation. However, the Article 29 Working Party's 2014 guidance and subsequent EDPB guidelines expanded the targeting test beyond the negotiators' intent (e.g., mere website accessibility ≠ targeting). The CJEU's Weltimmo (2015) and Google Spain (2014) rulings are cited by the Commission as validation but criticized by non-EU scholars (Kuner, Svantesson, Swire) as judicial expansion beyond the founding problem. No non-beneficiary source corroborates that the current scope matches the founding problem.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__effects_jurisdiction_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__effects_jurisdiction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__effects_jurisdiction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(gdpr_article_3_scope__effects_jurisdiction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__effects_jurisdiction_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) reflects the substantial compliance cost transfer to non-EU actors who cannot vote in EU elections — the 'taxation without representation' structural dynamic. Suppression (0.55) is moderate: the constraint does not physically prevent non-compliance but makes it commercially suicidal for market-seeking entities; geo-blocking is the functional exit, which is itself a cost. Theater ratio (0.18) is low: the coordination function (preventing regulatory arbitrage) is real and the enforcement machinery (DPAs, EDPB, CJEU) is functional, not performative. Accessibility collapse (0.42) is moderate: alternatives (standard contractual clauses, binding corporate rules, adequacy decisions) exist but are themselves shaped by the same reading. Resistance (0.58) is significant: Schrems litigation, US-EU diplomatic friction, 'Brussels Effect' pushback, and the rise of data localization laws globally all constitute resistance.
 *
 * PERSPECTIVAL GAP:
 *   The EU data subject seat experiences this as a mountain (rights follow the person — fundamental, non-negotiable). The DPA/CJEU seats experience it as a rope (coordination mechanism they administer). The non-EU controller seats experience it as a snare (extraction without voice). The small non-EU service seat experiences it as a piton (theatrical compliance impossible; exit via geo-blocking). The engine computes this divergence from the structural data: same constraint, different effective extraction per seat.
 *
 * DIRECTIONALITY LOGIC:
 *   EU data subjects: full beneficiaries (d ≈ 0.05) — constraint subsidizes their rights. DPAs/CJEU: agenda-setters with analytical exit (d ≈ 0.15) — institutional authority expands with constraint scope. Non-EU large controllers: powerful but constrained exit (d ≈ 0.75) — pay extraction, can exit market at high cost. Non-EU processors: organized, constrained exit (d ≈ 0.7) — architectural lock-in. Small non-EU services: moderate power, constrained exit (d ≈ 0.85) — disproportionate burden, geo-blocking only exit. Non-EU regulators: excluded, trapped (d ≈ 0.9) — sovereignty constrained, no voice. Analytical observers: d = 0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (regulatory arbitrage under the 1995 Directive) is contested as live vs. solved. The reading has expanded beyond the founding problem via CJEU interpretation (Weltimmo: establishment test broadened; Google Spain: right to be forgotten extraterritorialized; Schrems II: adequacy mechanism weaponized). This expansion is not mandatrophy — the coordination function remains live (arbitrage would return without it) — but the extraction asymmetry has grown. The mandate has not atrophied; it has been instrumentally expanded. The reading persists because the coordination problem is genuine AND the extraction is structurally locked in by the Brussels Effect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    targeting_test_boundary,
    'Where does the ''targeting'' test end and mere accessibility begin? The EDPB Guidelines 3/2018 distinguish them, but the boundary is contested in practice (e.g., language, currency, TLD, marketing).',
    'CJEU preliminary rulings on specific fact patterns; EDPB guideline revisions; empirical study of DPA enforcement decisions.',
    'If targeting is read broadly, extractiveness rises (more controllers captured). If narrowly, the coordination function degrades (arbitrage returns). The reading''s ε is sensitive to this boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(targeting_test_boundary, conceptual, 'The conceptual boundary of the targeting test determines the constraint''s scope and extractiveness.').

omega_variable(
    monitoring_test_scope,
    'Does ''monitoring behaviour'' (Article 3(2)(b)) extend to all behavioral analytics, ad tech, and algorithmic profiling, or only intentional tracking of EU residents?',
    'CJEU interpretation of ''monitoring'' in pending cases; EDPB guidance on ad tech; regulatory action against behavioral advertising.',
    'Broad monitoring reading captures the entire programmatic advertising ecosystem — massive extraction expansion. Narrow reading limits to intentional surveillance. This is the single largest extractiveness delta in the reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(monitoring_test_scope, empirical, 'Scope of the monitoring limb determines whether ad tech is structurally captured.').

omega_variable(
    enforcement_asymmetry,
    'Can EU DPAs effectively enforce against non-EU controllers without assets or presence in the EU? The ''enforcement gap'' between formal jurisdiction and practical enforceability.',
    'Track record of cross-border enforcement actions; adequacy decisions as enforcement leverage; international cooperation agreements.',
    'If enforcement is ineffective, the constraint degrades toward piton (theatrical). If effective via adequacy/fines on EU-revenue, extraction is real. The theater_ratio trajectory depends on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_asymmetry, empirical, 'Whether formal extraterritorial jurisdiction translates into effective extraction.').

omega_variable(
    kernel_framing_ambiguity,
    'Is Article 3(2) a jurisdictional rule (effects_jurisdiction_reading), a market access condition (market_access_reading), or an ultra vires extraterritorial assertion (territorial_sovereignty_reading)? The kernel''s framing determines which structural analysis applies.',
    'This is a conceptual framing question — it resolves through institutional contest (CJEU, legislatures, diplomatic negotiation), not empirical discovery. The omega documents the irreducible underdetermination.',
    'If market_access_reading prevails, the constraint reclassifies toward rope (conditional access is coordination). If territorial_sovereignty_reading prevails, it reclassifies toward snare (extraction without legitimacy). The effects_jurisdiction_reading sits as tangled_rope between them.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'The kernel framing contest is the structural ambiguity that generates the constraint family.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__effects_jurisdiction_reading, 2018, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_a3_scope_ej_tr_t2018, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2018, 0.08).
narrative_ontology:measurement(gdpr_a3_scope_ej_tr_t2020, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2020, 0.12).
narrative_ontology:measurement(gdpr_a3_scope_ej_tr_t2022, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2022, 0.15).
narrative_ontology:measurement(gdpr_a3_scope_ej_tr_t2024, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2024, 0.17).
narrative_ontology:measurement(gdpr_a3_scope_ej_tr_t2026, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2026, 0.18).
narrative_ontology:measurement(gdpr_a3_scope_ej_tr_t2028, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2028, 0.18).
narrative_ontology:measurement(gdpr_a3_scope_ej_tr_t2030, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2030, 0.18).

% Extraction over time
narrative_ontology:measurement(gdpr_a3_scope_ej_be_t2018, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2018, 0.45).
narrative_ontology:measurement(gdpr_a3_scope_ej_be_t2020, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2020, 0.52).
narrative_ontology:measurement(gdpr_a3_scope_ej_be_t2022, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2022, 0.59).
narrative_ontology:measurement(gdpr_a3_scope_ej_be_t2024, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2024, 0.64).
narrative_ontology:measurement(gdpr_a3_scope_ej_be_t2026, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2026, 0.66).
narrative_ontology:measurement(gdpr_a3_scope_ej_be_t2028, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2028, 0.67).
narrative_ontology:measurement(gdpr_a3_scope_ej_be_t2030, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2030, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_a3_scope_ej_su_t2018, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2018, 0.35).
narrative_ontology:measurement(gdpr_a3_scope_ej_su_t2020, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2020, 0.42).
narrative_ontology:measurement(gdpr_a3_scope_ej_su_t2022, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2022, 0.48).
narrative_ontology:measurement(gdpr_a3_scope_ej_su_t2024, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2024, 0.52).
narrative_ontology:measurement(gdpr_a3_scope_ej_su_t2026, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2026, 0.54).
narrative_ontology:measurement(gdpr_a3_scope_ej_su_t2028, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2028, 0.55).
narrative_ontology:measurement(gdpr_a3_scope_ej_su_t2030, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2030, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__effects_jurisdiction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gdpr_article_3_scope__effects_jurisdiction_reading, 0.12).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_adequacy_decisions).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, eu_us_data_privacy_framework).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, global_data_localization_laws).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, eu_ai_act_extraterritorial_scope).

% DUAL FORMULATION NOTE:
% The gdpr_article_3_scope kernel decomposes into three readings: effects_jurisdiction_reading (this story, tangled_rope, ε=0.68), market_access_reading (rope, lower ε, coordination-as-market-condition), territorial_sovereignty_reading (snare from non-EU perspective, higher ε). This story's ε is higher than market_access because the jurisdictional claim creates stronger enforcement machinery; lower than territorial_sovereignty because the coordination function is genuine. The family is linked by shared referent (Article 3(2) text) and causal dependency (effects reading drives Brussels Effect that market_access reading describes).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gdpr_article_3_scope__effects_jurisdiction_reading, institutional, 0.15).
constraint_indexing:directionality_override(gdpr_article_3_scope__effects_jurisdiction_reading, powerful, 0.75).
constraint_indexing:directionality_override(gdpr_article_3_scope__effects_jurisdiction_reading, organized, 0.7).
constraint_indexing:directionality_override(gdpr_article_3_scope__effects_jurisdiction_reading, moderate, 0.85).
constraint_indexing:directionality_override(gdpr_article_3_scope__effects_jurisdiction_reading, powerless, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
