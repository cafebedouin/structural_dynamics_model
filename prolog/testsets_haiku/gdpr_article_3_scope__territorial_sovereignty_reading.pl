% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__territorial_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: gdpr_article_3_scope__territorial_sovereignty_reading
 *   human_readable: GDPR Article 3 Territorial Sovereignty Reading: Extraterritorial Application as Regulatory Overreach
 *   domain: legal/technological/political
 *
 * SUMMARY:
 *   GDPR Article 3 establishes extraterritorial reach: EU data protection law
 *   applies to any processing of EU residents' data, regardless of where the
 *   processor is located. This constraint instantiates the TERRITORIAL
 *   SOVEREIGNTY READING of Article 3's scope: the claim that extraterritorial
 *   application exceeds the EU's legitimate regulatory authority under
 *   international law principles of territorial jurisdiction. This reading
 *   contests the effects-jurisdiction and market-access readings by asserting
 *   that data law must respect Westphalian sovereignty—jurisdiction follows
 *   territory, not data flows or global market effects. The
 *   territorial-sovereignty reading is held by non-EU states (as leverage in
 *   data localization arguments), some multinational non-EU technology
 *   companies (as a defense against compliance scope), and international law
 *   scholars who defend classical jurisdictional boundaries. It is opposed by
 *   EU regulators, data-protection authorities, and scholars who argue human
 *   rights protection justifies extraterritorial reach.
 *
 * KEY AGENTS:
 *   - EU regulators and data-protection authorities: enforce GDPR Article 3 extraterritorial scope; treat it as legitimate protection mechanism
 *   - Non-EU state regulators: benefit from territorial-sovereignty reading as a justification for data localization and regulatory independence; resist EU extraterritorial claims
 *   - Non-EU multinational technology companies: pay compliance costs but benefit rhetorically from territorial-sovereignty framing; lobby to contain GDPR's scope
 *   - EU residents (data subjects): nominally benefit from extraterritorial protection but lack agency; protection is byproduct of institutional assertion
 *   - Non-EU companies (esp. US tech): bear architectural compliance costs; support territorial-sovereignty reading to reduce enforcement threat
 *   - International law scholars: contest whether extraterritorial data regulation violates Westphalian sovereignty principles
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__territorial_sovereignty_reading, 0.68).
domain_priors:suppression_score(gdpr_article_3_scope__territorial_sovereignty_reading, 0.72).
domain_priors:theater_ratio(gdpr_article_3_scope__territorial_sovereignty_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__territorial_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__territorial_sovereignty_reading, "GDPR Article 3 Territorial Sovereignty Reading: Extraterritorial Application as Regulatory Overreach").
narrative_ontology:topic_domain(gdpr_article_3_scope__territorial_sovereignty_reading, "legal/technological/political").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__territorial_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__territorial_sovereignty_reading, 'db66f5c5-33de-4839-b779-98e6bcec2964').
narrative_ontology:cs_kernel_codification('db66f5c5-33de-4839-b779-98e6bcec2964', fixed_text).
narrative_ontology:cs_authority_grounding('db66f5c5-33de-4839-b779-98e6bcec2964', extraction).
narrative_ontology:cs_interpretation_layer_present('db66f5c5-33de-4839-b779-98e6bcec2964').
narrative_ontology:cs_reading_relation('db66f5c5-33de-4839-b779-98e6bcec2964', gdpr_article_3_scope__effects_jurisdiction_reading, forecloses).
narrative_ontology:cs_reading_relation('db66f5c5-33de-4839-b779-98e6bcec2964', gdpr_article_3_scope__market_access_reading, coexists_with).
narrative_ontology:cs_axiom('db66f5c5-33de-4839-b779-98e6bcec2964', foundational, jurisdiction_bounded_by_territory).
narrative_ontology:cs_axiom_status(jurisdiction_bounded_by_territory, holdable).
narrative_ontology:cs_axiom_grounding('db66f5c5-33de-4839-b779-98e6bcec2964', jurisdiction_bounded_by_territory, deontological).
narrative_ontology:cs_axiom('db66f5c5-33de-4839-b779-98e6bcec2964', foundational, extraterritorial_authority_exceeds_legitimacy).
narrative_ontology:cs_axiom_status(extraterritorial_authority_exceeds_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('db66f5c5-33de-4839-b779-98e6bcec2964', extraterritorial_authority_exceeds_legitimacy, conventional).
narrative_ontology:cs_reference_frame('db66f5c5-33de-4839-b779-98e6bcec2964', westphalian_territorial_sovereignty).
narrative_ontology:cs_drift_state('db66f5c5-33de-4839-b779-98e6bcec2964', contemporary_transnational_data_flows, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('db66f5c5-33de-4839-b779-98e6bcec2964', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_state_regulators).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_multinational_technology_companies).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, eu_residents_data_subjects).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_companies_unable_to_comply).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__territorial_sovereignty_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(gdpr_article_3_scope__territorial_sovereignty_reading, 'none', 1).

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
 *   Extractiveness (0.68 at interval end) is high because the constraint imposes compliance costs (data architecture, consent infrastructure, breach notification) on non-EU actors without their consent or participation in rule-setting. Suppression (0.72) is high because non-EU states and companies have limited exit: they cannot opt out of GDPR compliance while serving EU residents without losing market access. Theater (0.41) rises over the interval as non-EU states adopt countervailing data localization rules (presentational response to EU assertion) and companies increasingly frame compliance as bureaucratic theater rather than meaningful protection (extractive overhead). Accessibility collapse (0.64) is moderate-high: alternatives to GDPR compliance exist (market abandonment, data localization, processing minimization) but are costly; the collapse is economic, not absolute. Resistance (0.78) is high: non-EU states actively resist the reading via data sovereignty arguments and countervailing legislation; companies lobby against enforcement expansion; scholars contest the principle. The measurement series show extractiveness plateauing at t=20, suppression and theater stabilizing—the constraint has reached an enforced equilibrium rather than escalating further, suggesting institutional adaptation rather than breakdown.
 *
 * PERSPECTIVAL GAP:
 *   The constraint computes as Tangled Rope when measured from the EU's seat (genuine coordination: rule-of-law protection, clear standards; asymmetric extraction resisted actively by non-EU actors). From the non-EU state regulator's seat, it computes as Snare (pure extraction of regulatory authority, with the territorial-sovereignty reading serving as the resistance mechanism). From non-EU company seats, it computes as intermediate: rope-like for those with institutional capacity to engage EU regulators (Apple, Google negotiating compliance standards) and snare-like for smaller firms (trapped by compliance costs, no negotiating power). The engine will compute these divergences from the structural data; the territorial-sovereignty reading authors that divergence explicitly.
 *
 * DIRECTIONALITY LOGIC:
 *   The narrative above covers the directionality logic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting EU resident data in global flows) is CONTESTED: EU regulators say it remains live; non-EU governments say it was solved by market mechanisms and GDPR compliance is now defensive rather than protective (extractive overhead, theater). The reading's core assertion—that extraterritorial jurisdiction exceeds legitimacy—does not depend on mandatrophy: even if the protection is still needed, a territorial-sovereignty reading holds that the EU is not the rightful authority to provide it. Mandatrophy would arise if extraction persisted without the founding problem (data protection fails despite GDPR enforcement), not from the jurisdictional contest itself. The constraint avoids mandatrophy classification by remaining contested—the founding problem status is live enough for some parties (EU) and dead enough for others (non-EU) that no consensus exists on whether the regulation serves its original purpose or has become institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_legitimacy_grounding,
    'On what grounds can the EU legitimately claim regulatory authority over data processing outside its territory?',
    'International law scholarship and diplomatic resolution: Does the EU''s authority rest on effects doctrine (targeting EU residents), human rights extraterritorial obligations, de facto market power (Brussels Effect), or exceed all legitimate grounds? Different resolutions produce different readings.',
    'If effects doctrine is accepted as legitimate, the territorial-sovereignty reading loses force and the effects-jurisdiction reading prevails. If human rights extraterritoriality is rejected, the reading gains force. If Brussels Effect is recognized, market-access reading dominates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_legitimacy_grounding, conceptual, 'The foundational legitimacy claim distinguishing this reading: territorial jurisdiction is the boundary of authority.').

omega_variable(
    westphalian_sovereignty_applicability,
    'Does Westphalian sovereignty doctrine apply to data regulation, or is data flows a domain where classical territorial jurisdiction is obsolete?',
    'International law evolution and consensus: adoption of new multilateral frameworks (e.g., UN data governance principles) that either reinforce territorial sovereignty or establish alternative legitimacy grounds for extraterritorial reach.',
    'If Westphalian principles are deemed obsolete for data, the reading''s core premise (territorial bounded jurisdiction) is rejected and the effects-jurisdiction reading is vindicated. If territorial sovereignty is reaffirmed, the reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(westphalian_sovereignty_applicability, conceptual, 'Whether territorial sovereignty doctrine applies to transnational data flows or is historically contingent.').

omega_variable(
    compliance_cost_vs_protection_trade_off,
    'To what extent do non-EU companies'' compliance costs reflect genuine protection of EU residents versus regulatory overreach beyond market-access requirements?',
    'Cost-benefit analysis and comparative legal study: detailed assessment of compliance costs (consent infrastructure, data localization, breach notification) versus actual privacy harms prevented. If costs are decoupled from harm-reduction, they indicate extractive overhead.',
    'If compliance costs are decoupled from protection outcomes, the constraint is more extractive and the territorial-sovereignty reading gains force (regulation without legitimacy). If costs track protection outcomes, the constraint is justified and the reading weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_cost_vs_protection_trade_off, empirical, 'Whether compliance costs are proportionate to protection benefits or reflect extractive regulatory reach.').

omega_variable(
    data_localization_as_countermeasure,
    'Are non-EU data localization mandates (China, India, Russia) legitimate exercises of territorial sovereignty or extractive counter-regulations that harm data subjects?',
    'Assessment of outcomes and intent: Do localization mandates improve data protection for residents of those countries, or serve primarily to extract regulatory authority and restrict flows for geopolitical reasons?',
    'If localization mandates are extractive, the territorial-sovereignty reading is delegitimized by its own logic—non-EU states are making the same extraterritorial claims in reverse. If localization mandates improve protection, they validate the reading''s principle. The reading does not require non-EU localization to be legitimate; it requires that the EU''s extraterritorial claim be illegitimate on principle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(data_localization_as_countermeasure, empirical, 'Whether countervailing data localization by non-EU states reflects genuine sovereignty or extractive regulation masked as protection.').

omega_variable(
    reading_contest_institutional_drivers,
    'Is the territorial-sovereignty reading driven by genuine principle (respect for international law boundaries) or by institutional interest (non-EU states asserting sovereignty, companies reducing compliance scope)?',
    'Analysis of who holds the reading, what constraints they face, and what changes when institutional incentives shift. Do scholars and government officials who benefit from rejecting the reading''s principle nonetheless hold it? Do countries that benefit from EU''s extraterritorial reach nevertheless support territorial-sovereignty doctrine?',
    'If the reading is driven primarily by institutional interest (non-EU states against GDPR, companies against regulation), it is less grounded in principle and more grounded in extraction resistance. If principled scholars hold it independent of institutional interest, it has stronger conceptual grounds. The reading''s epistemic status is ambiguous.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_institutional_drivers, empirical, 'Whether the territorial-sovereignty reading reflects genuine international-law principle or institutional position-taking.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__territorial_sovereignty_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t0, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(gdpr_tr_t5, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 5, 0.29).
narrative_ontology:measurement(gdpr_tr_t10, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(gdpr_tr_t15, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(gdpr_tr_t20, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(gdpr_tr_t25, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t0, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(gdpr_be_t5, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 5, 0.51).
narrative_ontology:measurement(gdpr_be_t10, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement(gdpr_be_t15, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(gdpr_be_t20, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(gdpr_be_t25, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_su_t0, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(gdpr_su_t5, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(gdpr_su_t10, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(gdpr_su_t15, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement(gdpr_su_t20, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(gdpr_su_t25, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__territorial_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gdpr_article_3_scope__territorial_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope__effects_jurisdiction_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope__market_access_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel gdpr_article_3_scope. All three readings accept GDPR's substantive rules and mechanism but dispute the legitimacy and scope of EU regulatory authority. Sibling stories: effects_jurisdiction_reading (authority follows effects on EU residents; Article 3(2) targeting/monitoring test); market_access_reading (GDPR as Brussels Effect conditional market access, not jurisdictional assertion). The constraint family exhibits ε-invariance: each reading instantiates different structural beneficiaries and authority legitimacy claims, producing different compliance structures and resistance mechanisms despite identical regulatory text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gdpr_article_3_scope__territorial_sovereignty_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
