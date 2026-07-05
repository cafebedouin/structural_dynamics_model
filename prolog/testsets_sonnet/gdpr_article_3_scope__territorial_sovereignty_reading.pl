% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__territorial_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: gdpr_article_3_scope__territorial_sovereignty_reading
 *   human_readable: GDPR Article 3 Extraterritorial Scope — Territorial Sovereignty Reading
 *   domain: technology_governance/international_law/privacy_regulation
 *
 * SUMMARY:
 *   This story instantiates the territorial-sovereignty reading of the GDPR
 *   Article 3 scope kernel: jurisdiction is bounded by territorial
 *   sovereignty as a matter of customary international law, and Article
 *   3(2)'s targeting/monitoring test — which extends GDPR obligations to
 *   entities with no EU establishment whenever they offer goods/services to
 *   or monitor EU residents — exceeds the EU's legitimate prescriptive
 *   authority. This is a distinct constraint from the
 *   effects_jurisdiction_reading (which treats the targeting test as a
 *   legitimate extension of protective jurisdiction following harm to EU
 *   residents) and the market_access_reading (which treats
 *   extraterritoriality as conditional market access / Brussels Effect
 *   standard-setting rather than a jurisdictional assertion at all). The
 *   three readings share a text (Article 3) but diverge sharply on ε and on
 *   who counts as victim: under this reading, non-EU firms and third-country
 *   regulators are genuine victims of an ultra vires assertion; under the
 *   market-access reading, there are no victims because no one is compelled
 *   to sell into the EU market; under the effects-jurisdiction reading, EU
 *   residents are the beneficiaries of legitimately extended protection and
 *   there is no illegitimate extraction at all. Per the ε-invariance
 *   principle, these are three separate stories linked by network edges, not
 *   one story with a variable interpretation.
 *
 * KEY AGENTS:
 *   - eu_data_protection_authorities: agenda_setter (institutional/arbitrage) — asserts and enforces extraterritorial jurisdiction
 *   - non_eu_based_smes: primary payer (moderate/constrained) — bears compliance cost and enforcement exposure with no domestic nexus
 *   - third_country_sovereign_regulatory_authority: institutional payer/excluded — sees its exclusive territorial jurisdiction functionally displaced without consent or treaty basis
 *   - non_eu_state_regulators: beneficiary (institutional/constrained) — uses the conflict to build reciprocal regulatory and localization capacity
 *   - international_legal_scholars: analytical observer — evaluates the doctrinal legitimacy question directly
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__territorial_sovereignty_reading, 0.58).
domain_priors:suppression_score(gdpr_article_3_scope__territorial_sovereignty_reading, 0.52).
domain_priors:theater_ratio(gdpr_article_3_scope__territorial_sovereignty_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__territorial_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__territorial_sovereignty_reading, "GDPR Article 3 Extraterritorial Scope — Territorial Sovereignty Reading").
narrative_ontology:topic_domain(gdpr_article_3_scope__territorial_sovereignty_reading, "technology_governance/international_law/privacy_regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__territorial_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__territorial_sovereignty_reading, 'ce9325b2-89e7-4f33-9df0-572f1f95b33e').
narrative_ontology:cs_kernel_codification('ce9325b2-89e7-4f33-9df0-572f1f95b33e', fixed_text).
narrative_ontology:cs_authority_grounding('ce9325b2-89e7-4f33-9df0-572f1f95b33e', extraction).
narrative_ontology:cs_interpretation_layer_present('ce9325b2-89e7-4f33-9df0-572f1f95b33e').
narrative_ontology:cs_reading_relation('ce9325b2-89e7-4f33-9df0-572f1f95b33e', gdpr_article_3_scope__effects_jurisdiction_reading, forecloses).
narrative_ontology:cs_reading_relation('ce9325b2-89e7-4f33-9df0-572f1f95b33e', gdpr_article_3_scope__market_access_reading, coexists_with).
narrative_ontology:cs_axiom('ce9325b2-89e7-4f33-9df0-572f1f95b33e', foundational, territorial_exclusivity_of_prescriptive_jurisdiction).
narrative_ontology:cs_axiom_status(territorial_exclusivity_of_prescriptive_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('ce9325b2-89e7-4f33-9df0-572f1f95b33e', territorial_exclusivity_of_prescriptive_jurisdiction, conventional).
narrative_ontology:cs_axiom('ce9325b2-89e7-4f33-9df0-572f1f95b33e', secondary, consent_or_treaty_required_for_extraterritorial_obligation).
narrative_ontology:cs_axiom_status(consent_or_treaty_required_for_extraterritorial_obligation, holdable).
narrative_ontology:cs_axiom_grounding('ce9325b2-89e7-4f33-9df0-572f1f95b33e', consent_or_treaty_required_for_extraterritorial_obligation, conventional).
narrative_ontology:cs_reference_frame('ce9325b2-89e7-4f33-9df0-572f1f95b33e', westphalian_territorial_exclusivity).
narrative_ontology:cs_drift_state('ce9325b2-89e7-4f33-9df0-572f1f95b33e', post_gdpr_enforcement_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('ce9325b2-89e7-4f33-9df0-572f1f95b33e', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_state_regulators).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, eu_data_protection_authorities).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, domestic_data_localization_vendors).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_based_smes).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, third_country_sovereign_regulatory_authority).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, cross_border_data_flow_dependent_firms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, eu_domiciled_multinational_competitors).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__territorial_sovereignty_reading, westphalian_territorial_jurisdiction_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce Article 3(2)'s targeting/monitoring test against entities with no EU establishment, issuing fines and orders that reach firms whose only nexus to the EU is processing data about EU residents. From the territorial-sovereignty reading, this enforcement asserts prescriptive jurisdiction beyond the EU's territorial boundary, resting the claim on the nationality/location of the data subject rather than the location of the regulated conduct.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, eu_data_protection_authorities, agenda_setter,
    institutional, generational, arbitrage, continental).

% Watch a foreign regulator assert authority over firms incorporated, operating, and processing data entirely within their own territory. Some respond by building or strengthening data localization regimes and competing regulatory frameworks that reassert domestic control over data processed within their borders, converting the sovereignty dispute into an occasion to expand their own regulatory reach and reduce dependence on EU-compliant infrastructure.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_state_regulators, beneficiary,
    institutional, generational, constrained, national).

% Sell in-country hosting, processing, and compliance services to firms seeking to avoid extraterritorial GDPR exposure by keeping data (and therefore the regulatory nexus) fully within domestic borders. Their business model depends on the sovereignty conflict persisting and on firms believing that localization is the reliable way to exit EU jurisdictional reach.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, domestic_data_localization_vendors, beneficiary,
    organized, biographical, mobile, national).

% Operate wholly within their own country, incorporated and taxed there, with no EU offices or assets, yet find themselves subject to EU compliance obligations, audit demands, and potential fines merely because they offer goods or services to, or monitor the behavior of, people located in the EU. Their government cannot shield them from enforcement actions that reach through payment processors, EU-based representatives, or seized assets in third countries; genuine exit would mean refusing all EU-connected customers, an option many cannot afford.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_based_smes, payer,
    moderate, biographical, constrained, national).

% As a matter of formal doctrine, holds exclusive prescriptive and enforcement jurisdiction over conduct occurring within its own territory. That exclusivity is functionally displaced whenever the EU imposes compliance obligations on domestic firms without the third country's consent, treaty, or mutual legal assistance framework. The authority has no seat in the EU's own rulemaking process and no formal veto over Article 3(2)'s scope, despite the doctrine's premise that its territorial authority should be dispositive.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, third_country_sovereign_regulatory_authority, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__territorial_sovereignty_reading, third_country_sovereign_regulatory_authority, excluded).

% Depend on frictionless cross-border data transfer for business models (ad-tech, SaaS, cloud analytics) that the sovereignty conflict pressures toward fragmentation. As more states respond to perceived extraterritorial overreach with reciprocal localization mandates, these firms face duplicated infrastructure costs and conflicting compliance regimes they did not choose and cannot arbitrage away, since operating multiple parallel data residencies is not a viable option for smaller players.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, cross_border_data_flow_dependent_firms, payer,
    moderate, biographical, trapped, global).

% Already maintain EU compliance infrastructure as a cost of doing business in the single market, so the extraterritorial reading imposes comparatively little marginal burden while raising costs for foreign competitors who previously operated without EU-grade compliance overhead. They have the scale to absorb both jurisdictions' demands and treat compliance capacity itself as a competitive moat.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, eu_domiciled_multinational_competitors, beneficiary,
    powerful, generational, arbitrage, global).

% Debate whether Article 3(2)'s targeting test is consistent with customary international law's territoriality and nationality principles, or whether it represents an unprecedented and destabilizing expansion of prescriptive jurisdiction that other states will increasingly emulate or resist through counter-legislation.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, there is no genuine coordination function performed by extraterritorial reach itself — territorial regulators coordinating with entities physically and legally within their own borders is the legitimate coordination baseline. What the constraint actually coordinates is a defensive counter-regime: third-country regulators and localization vendors coordinating around resistance to a jurisdictional claim they regard as illegitimate.
% TRANSFER_FUNCTION: Moves compliance costs, legal exposure, and enforcement risk from EU residents (who receive privacy protection they did not have to negotiate for) to non-EU firms and third-country regulatory authorities, while transferring competitive advantage to EU-domiciled multinationals already carrying compliance overhead and to domestic localization vendors selling exit-from-jurisdiction as a service.
% ABSENT_VOICES: Third-country legislatures and regulators had no seat at the table when Article 3(2) was drafted or is subsequently interpreted by EU courts and data protection authorities; non-EU SMEs subject to enforcement have no diplomatic channel to contest jurisdiction and must litigate within a framework built entirely on the other side's premises.
% DISAPPEARANCE_RATIONALE: If Article 3(2)'s extraterritorial reach were narrowed to true territorial nexus (EU establishment or EU-located processing), non-EU firms would lose their GDPR exposure overnight, data localization vendors would lose a substantial share of their addressable market, third-country regulators would reassert unchallenged domestic authority, and EU residents' data would be governed only by whatever protections the foreign jurisdiction independently chose to offer — a materially different global data governance landscape.
% FOUNDING_PROBLEM: The EU sought to prevent circumvention of its data protection regime by firms that process EU residents' data from outside the Union while targeting or monitoring those residents — closing what regulators saw as an obvious loophole for foreign-incorporated surveillance and ad-tech firms.
% FOUNDING_PROBLEM_CORROBORATION: EU data protection authorities and privacy advocates attest the anti-circumvention problem remains live and cite continued foreign ad-tech targeting of EU residents as evidence. Independent international law scholars and non-EU regulators — parties outside the EU's own beneficiary structure — attest the problem has been substantially resolved through alternative mechanisms (adequacy decisions, standard contractual clauses, market-access conditioning) and that continued reliance on unilateral extraterritorial assertion beyond those channels represents jurisdictional overreach rather than a live enforcement necessity.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__territorial_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__territorial_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__territorial_sovereignty_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gdpr_article_3_scope__territorial_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__territorial_sovereignty_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at a moderate-high 0.58 reflecting real compliance and litigation costs imposed on entities outside EU territory, rising over the interval as enforcement actions and fines against non-EU firms accumulate (Meta, Clearview AI, and similar precedents established that the targeting test would actually be litigated, not merely threatened). Suppression sits at 0.52 — meaningful but not maximal, because affected firms retain a genuine (if costly) exit: refusing EU-connected business or relocating data flows via localization. Resistance is authored high (0.72) because this reading's central prediction is exactly what the expected structural delta calls for: third-country regulatory pushback, competing localization mandates, and diplomatic friction, rather than quiet acceptance. Accessibility collapse is moderate (0.40) — alternatives to submission exist (localization, market withdrawal, counter-legislation) but are costly, which is why resistance is high without full suppression.
 *
 * PERSPECTIVAL GAP:
 *   From the EU agenda_setter seat, this looks like legitimate protective regulation properly enforced against circumvention. From the third-country regulator and non-EU SME seats, the identical enforcement action looks like an ultra vires jurisdictional assertion imposed without consent, treaty, or representation. The engine should compute these seats to opposite type-experiences from the same structural facts — that divergence is the substance of the sovereignty dispute, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   EU data protection authorities sit as agenda_setter/beneficiary-adjacent: institutional power, arbitrage-grade exit (they choose whether and how aggressively to enforce). Non-EU SMEs and cross-border-dependent firms are targets: moderate power, constrained-to-trapped exit, high d. Non-EU state regulators and domestic localization vendors are beneficiaries of the conflict itself — even though they are nominally 'targets' of the sovereignty dispute, the dispute is the occasion for their own regulatory or commercial expansion, so their directionality sits closer to the beneficiary end despite institutional friction. Third-country sovereign authority is the paradigmatic payer under this reading: it holds formal institutional power but that power is precisely what is being structurally overridden, so its effective directionality is high despite its nominal status.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding anti-circumvention problem (foreign ad-tech surveilling EU residents while evading EU law) was real at GDPR's drafting. Under this reading, the question is whether the targeting test as currently applied has drifted from remedying that specific circumvention harm into a general assertion of jurisdiction over any entity anywhere that processes EU-resident data, regardless of whether genuine circumvention is occurring. The founding_problem_status is authored as contested precisely because EU authorities and independent scholars disagree on whether the original narrow anti-circumvention rationale still bounds current enforcement practice, or whether enforcement has expanded past what the founding problem justifies — a classic mandatrophy signature where the original mandate persists in name while its application scope has grown.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_reading_versus_sibling_framings,
    'Is GDPR Article 3(2)''s extraterritorial reach best understood as (a) an illegitimate assertion of prescriptive jurisdiction beyond EU territory (this reading), (b) a legitimate extension of protective jurisdiction following harm to EU residents (effects_jurisdiction_reading), or (c) a conditional market-access requirement that is not a jurisdictional assertion at all (market_access_reading)?',
    'This is fundamentally a question of customary international law doctrine and is unlikely to be resolved by empirical data alone; movement would come from an authoritative international tribunal ruling (e.g., ICJ, WTO panel, or binding multilateral treaty) that either endorses or rejects the targeting test as consistent with territorial jurisdiction principles, or from widespread state practice (opinio juris) converging on one framing.',
    'If the effects_jurisdiction or market_access framing prevails as the dominant international legal consensus, this reading''s classification of non-EU firms and third-country regulators as ''victims'' of illegitimate extraction becomes contested at the doctrinal level even though the structural facts (who pays compliance costs, who enforces) remain identical. The three readings would then represent a live, unresolved doctrinal fork rather than one correct account with two mistaken alternatives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_reading_versus_sibling_framings, conceptual, 'Which of the three sibling readings of Article 3 scope reflects the operative international law consensus, if any consensus exists.').

omega_variable(
    localization_resistance_efficacy,
    'Does data localization actually succeed in insulating non-EU firms from GDPR extraterritorial enforcement, or does the targeting/monitoring test reach firms regardless of where data is physically stored?',
    'Track enforcement outcomes against firms that have adopted localization strategies specifically to avoid EU nexus; if EU authorities successfully assert jurisdiction over such firms anyway (because the test looks to targeting/monitoring of residents, not data location), localization is revealed as an ineffective resistance mechanism and the vendor beneficiary relationship weakens.',
    'If localization is enforcement-proof, third-country regulators and localization vendors gain a durable coordination win and the beneficiary classification holds robustly. If localization fails to prevent enforcement because the targeting test is behaviorally rather than geographically defined, the entire resistance strategy this reading predicts collapses, and the sovereignty conflict shifts from a jurisdiction-versus-jurisdiction contest to a de facto EU standard-setting outcome closer to the market_access_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(localization_resistance_efficacy, empirical, 'Whether data localization provides genuine jurisdictional insulation or merely delays enforcement.').

omega_variable(
    reciprocal_extraterritoriality_escalation,
    'Will third-country regulators respond to perceived EU overreach by asserting their own extraterritorial data protection or localization requirements against EU firms, escalating into a general norm of reciprocal extraterritorial assertion?',
    'Observe whether non-EU jurisdictions (e.g., under national data protection or localization statutes) begin applying similar targeting-based extraterritorial tests against EU-based firms processing their residents'' data, and whether this becomes a stable multilateral pattern or remains asymmetric.',
    'Reciprocal escalation would validate this reading''s core prediction (jurisdictional conflict escalation) and strengthen the case that Article 3(2) set a destabilizing precedent rather than a uniquely justified anti-circumvention measure; a failure to escalate would suggest EU extraterritoriality is being tolerated as a special case rather than treated as a violable sovereignty norm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocal_extraterritoriality_escalation, empirical, 'Whether GDPR''s extraterritorial model becomes a normalized international practice of reciprocal jurisdictional assertion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__territorial_sovereignty_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t0, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(gdpr_tr_t4, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(gdpr_tr_t8, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(gdpr_tr_t12, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement(gdpr_tr_t16, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(gdpr_tr_t20, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(gdpr_tr_t24, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t0, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(gdpr_be_t4, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(gdpr_be_t8, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(gdpr_be_t12, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(gdpr_be_t16, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(gdpr_be_t20, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(gdpr_be_t24, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_su_t0, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(gdpr_su_t4, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 4, 0.36).
narrative_ontology:measurement(gdpr_su_t8, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 8, 0.41).
narrative_ontology:measurement(gdpr_su_t12, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 12, 0.45).
narrative_ontology:measurement(gdpr_su_t16, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(gdpr_su_t20, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(gdpr_su_t24, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__territorial_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope__effects_jurisdiction_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope__market_access_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling stories decomposing the natural-language concept 'GDPR Article 3 extraterritorial scope' per the ε-invariance principle. All three share the same statutory text but assign different ε values, different beneficiary/victim structures, and different claimed types: territorial_sovereignty_reading (this story, tangled_rope, ε=0.58, victims = non-EU firms and third-country regulators), effects_jurisdiction_reading (expected rope or tangled_rope with lower ε, beneficiaries = EU residents receiving legitimately extended protection, no genuine victims under that framing), and market_access_reading (expected rope, near-mountain-like voluntariness framing, ε reflecting compliance cost of voluntary market participation rather than coercive jurisdictional extraction). Each story must be read independently; do not average or blend their ε values, as that would violate the constraint-identity principle that ε is intrinsic to a single well-specified claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
