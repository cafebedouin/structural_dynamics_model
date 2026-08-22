% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__territorial_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: gdpr_article_3_scope__territorial_sovereignty_reading
 *   human_readable: GDPR Article 3(2) Extraterritorial Scope — Territorial Sovereignty Reading
 *   domain: technology_governance/international_law/privacy_regulation
 *
 * SUMMARY:
 *   This story instantiates the territorial-sovereignty reading of the GDPR
 *   Article 3(2) jurisdictional kernel: the claim that a state's regulatory
 *   authority is bounded by its physical territory and that GDPR's extension
 *   to any entity anywhere that targets or monitors an EU resident exceeds
 *   legitimate jurisdiction. On this reading the extraterritorial claim
 *   functions as a coordination device wrapped around an asymmetric
 *   extraction: it genuinely closes an evasion loophole for EU-established
 *   controllers routing data offshore, but it also reaches entities with zero
 *   territorial connection to the EU beyond serving an EU customer,
 *   extracting compliance cost and legal exposure from them without their
 *   having consented to EU authority through presence or conduct on EU soil.
 *   Non-EU state regulators and domestic localization vendors gain from the
 *   resulting jurisdictional friction — the former as precedent for their own
 *   extraterritorial and localization claims, the latter as a market
 *   opportunity. This is a distinct constraint from the
 *   effects_jurisdiction_reading (which treats the targeting/monitoring test
 *   as the legitimate basis of jurisdiction, not an overreach of it) and from
 *   the market_access_reading (which treats extraterritoriality as
 *   conditional market access rather than jurisdictional assertion at all) —
 *   each reading has its own ε, its own beneficiary/victim structure, and its
 *   own classification, linked here only by shared kernel identity.
 *
 * KEY AGENTS:
 *   - eu_data_protection_authorities: agenda_setter asserting extraterritorial jurisdiction
 *   - non_eu_domiciled_online_businesses: primary payer bearing compliance cost without territorial nexus
 *   - non_eu_small_and_medium_enterprises: powerless payer, trapped exit
 *   - non_eu_state_regulators: beneficiary using the precedent for own sovereignty claims
 *   - domestic_data_localization_vendors: beneficiary of the resulting compliance market
 *   - eu_resident_data_subjects: excluded from the jurisdictional dispute despite being its stated justification
 *   - international_courts_and_trade_bodies: analytical observer of the unresolved conflict
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__territorial_sovereignty_reading, 0.62).
domain_priors:suppression_score(gdpr_article_3_scope__territorial_sovereignty_reading, 0.58).
domain_priors:theater_ratio(gdpr_article_3_scope__territorial_sovereignty_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__territorial_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__territorial_sovereignty_reading, "GDPR Article 3(2) Extraterritorial Scope — Territorial Sovereignty Reading").
narrative_ontology:topic_domain(gdpr_article_3_scope__territorial_sovereignty_reading, "technology_governance/international_law/privacy_regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__territorial_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__territorial_sovereignty_reading, '1e71421d-9560-4f8b-9ed7-b05d7995d83c').
narrative_ontology:cs_kernel_codification('1e71421d-9560-4f8b-9ed7-b05d7995d83c', formalized).
narrative_ontology:cs_authority_grounding('1e71421d-9560-4f8b-9ed7-b05d7995d83c', distributed).
narrative_ontology:cs_reading_relation('1e71421d-9560-4f8b-9ed7-b05d7995d83c', gdpr_article_3_scope__effects_jurisdiction_reading, forecloses).
narrative_ontology:cs_reading_relation('1e71421d-9560-4f8b-9ed7-b05d7995d83c', gdpr_article_3_scope__market_access_reading, influences).
narrative_ontology:cs_axiom('1e71421d-9560-4f8b-9ed7-b05d7995d83c', foundational, jurisdiction_bounded_by_territorial_presence).
narrative_ontology:cs_axiom_status(jurisdiction_bounded_by_territorial_presence, holdable).
narrative_ontology:cs_axiom_grounding('1e71421d-9560-4f8b-9ed7-b05d7995d83c', jurisdiction_bounded_by_territorial_presence, conventional).
narrative_ontology:cs_axiom('1e71421d-9560-4f8b-9ed7-b05d7995d83c', secondary, data_subject_location_insufficient_for_regulatory_nexus).
narrative_ontology:cs_axiom_status(data_subject_location_insufficient_for_regulatory_nexus, holdable).
narrative_ontology:cs_axiom_grounding('1e71421d-9560-4f8b-9ed7-b05d7995d83c', data_subject_location_insufficient_for_regulatory_nexus, conventional).
narrative_ontology:cs_reference_frame('1e71421d-9560-4f8b-9ed7-b05d7995d83c', westphalian_territorial_jurisdiction).
narrative_ontology:cs_drift_state('1e71421d-9560-4f8b-9ed7-b05d7995d83c', post_gdpr_extraterritorial_enforcement_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('1e71421d-9560-4f8b-9ed7-b05d7995d83c', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_state_regulators).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, domestic_data_localization_vendors).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_domiciled_online_businesses).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_small_and_medium_enterprises).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assert enforcement authority over any entity that offers goods/services to or monitors EU residents, regardless of where the entity is incorporated or where its servers sit. Issue fines, demand representatives be appointed on EU soil, and treat non-compliance abroad as within their remit. From the territorial-sovereignty reading, this is an assertion of authority beyond the EU's actual territory, resting on the location of the data subject rather than the location of the regulated conduct.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, eu_data_protection_authorities, agenda_setter,
    institutional, generational, analytical, global).

% Operate entirely outside EU territory — incorporated, taxed, and physically present elsewhere — but must comply with EU rules whenever an EU resident is a customer or is monitored, on pain of fines levied against assets or revenue the EU can reach through banking and payment-processor leverage. Their only clean exit is geoblocking EU traffic, which costs revenue and relationships that predate the rule's extraterritorial claim.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_domiciled_online_businesses, payer,
    moderate, biographical, constrained, global).

% Lack the legal budget to litigate jurisdictional questions or the market power to simply exit the EU customer base. They absorb compliance costs (EU representatives, DPO retainers, contract rewrites) calibrated for firms many times their size, imposed by a regulator whose writ, on this reading, does not properly extend to their home territory at all.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_small_and_medium_enterprises, payer,
    powerless, biographical, trapped, regional).

% Point to the EU's extraterritorial overreach as precedent and cover for asserting their own competing sovereignty claims — data localization mandates, blocking statutes, and counter-extraterritorial rules of their own. The contested scope of Article 3(2) becomes leverage for domestic regulators to insist that data about their citizens likewise stays under their exclusive writ, strengthening their own jurisdictional and institutional position regardless of privacy outcomes.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_state_regulators, beneficiary,
    institutional, generational, arbitrage, national).

% Sell in-country hosting, local-entity, and data-residency compliance products that firms adopt specifically to avoid falling within reach of jurisdictional assertions like Article 3(2) — the contested extraterritorial claim, on the sovereignty reading, is precisely what makes localization a defensible commercial offering rather than a redundant cost.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, domestic_data_localization_vendors, beneficiary,
    organized, biographical, arbitrage, national).

% Their data is processed by firms outside EU territory who, on the sovereignty reading, are not properly EU jurisdiction's to bind at all. They are not consulted in the jurisdictional dispute between the EU and the states hosting the businesses that hold their data — their protection interest is invoked by the EU's claim of authority but the legitimacy of that claim, on this reading, does not run through their residency alone.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, eu_resident_data_subjects, excluded,
    powerless, biographical, trapped, global).

% Adjudicate conflicts between competing jurisdictional claims — enforcement of EU fines against foreign assets, mutual legal assistance disputes, trade-law challenges to data rules framed as non-tariff barriers. They observe the territorial-sovereignty argument being raised as a defense and as a diplomatic protest, without a settled international law consensus resolving which reading controls.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, international_courts_and_trade_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gdpr_article_3_scope__territorial_sovereignty_reading, diffuse).
narrative_ontology:fixing_cost_class(gdpr_article_3_scope__territorial_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, the only genuine coordination function GDPR's territorial scope could legitimately serve is protecting persons and conduct actually within EU territory — where a regulated act occurs, or where an establishment is physically present. Extending scope to any entity anywhere that merely serves or monitors an EU resident is not coordination but a unilateral extension of regulatory reach past the boundary that gives coordination its legitimacy in the first place.
% TRANSFER_FUNCTION: Moves compliance costs, legal risk, and enforcement exposure from EU territory onto entities domiciled and operating entirely under other states' law, without those entities having consented to EU authority through presence, incorporation, or conduct within EU territory — the transfer runs on the location of the data subject rather than the location of the regulated actor.
% ABSENT_VOICES: Non-EU legislatures and foreign ministries whose own citizens' commercial conduct is now bound by rules they had no part in drafting are structurally absent from GDPR's own lawmaking process; they object through diplomatic channels, trade complaints, and blocking statutes rather than as parties inside the regulation's own deliberative process.
% DISAPPEARANCE_RATIONALE: If EU authorities abandoned extraterritorial enforcement and confined Article 3 to entities established within EU territory, foreign SMEs would drop EU-specific compliance programs, some geoblocking would reverse, non-EU regulators would lose a ready precedent for their own extraterritorial claims, and the data-localization vendor market serving foreign firms specifically to escape EU reach would contract sharply — real money and real compliance architecture are riding on the extraterritorial claim standing.
% FOUNDING_PROBLEM: GDPR's territorial provisions were built to stop EU residents' data protections from being trivially defeated by routing data or incorporating a controller offshore — closing an obvious evasion loophole in a regime meant to bind conduct affecting EU persons.
% FOUNDING_PROBLEM_CORROBORATION: EU institutions and privacy advocates attest the evasion-closing problem remains live and justifies Article 3(2)'s reach. Independent evidence from outside the EU's own institutions is thinner on this specific reading: foreign trade ministries, WTO dispute submissions, and comparative-law scholars outside EU academic circles have repeatedly characterized the same provision as an overreach rather than a closed loophole — that disagreement from outside the benefiting institution is itself the corroboration record, not a resolution of it.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__territorial_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__territorial_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__territorial_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gdpr_article_3_scope__territorial_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__territorial_sovereignty_reading, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (0.62) reflects the compliance and legal-risk burden non-EU-domiciled firms carry under a jurisdictional claim they did not consent to through territorial presence; it is substantial but not maximal because large multinational targets often have genuine EU establishments or revenue interdependence that blur the sovereignty question. Suppression (0.58) reflects real but partial coercive force — EU-reachable assets, payment processors, and market access give enforcement teeth, but firms with no EU-reachable assets at all retain meaningful non-compliance as an option, which is why suppression is not near-maximal. Theater ratio (0.30) is moderate: enforcement activity against genuinely offshore, no-EU-nexus entities is comparatively rare relative to the compliance apparatus built around the threat of it. Accessibility collapse (0.42) is middling — geoblocking, jurisdictional arbitrage, and non-EU market pivots remain real alternatives, distinguishing this from a fully collapsed mountain-like constraint. Resistance (0.72) is high: this reading is precisely the one most actively contested by foreign governments, trade bodies, and businesses, which is the observable signature of a disputed jurisdictional claim rather than a settled one.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-EU domiciled businesses and especially non-EU SMEs sit near the full-target end: they bear the transfer, have constrained-to-trapped exit, and did not choose EU-territorial presence as the basis for their obligations. EU data protection authorities sit at the agenda-setting position with analytical exit — they administer the claim and bear none of its costs. Non-EU state regulators and data-localization vendors are beneficiaries despite having no formal role in GDPR: the contested jurisdictional claim is a resource they convert into their own institutional and commercial gain, which is why they are declared beneficiaries here rather than mere bystanders.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — closing the offshore-routing loophole for genuinely EU-connected controllers — retains live force for controllers with real EU establishment. But on the territorial-sovereignty reading, once the rule is extended to entities with no territorial connection to the EU whatsoever, the arrangement has drifted from evasion-closing coordination into an assertion of authority the reading holds illegitimate on sovereignty grounds alone — classifying this as tangled_rope rather than pure snare or pure mountain preserves the fact that a real, narrower coordination function exists inside a claim that has been extended well past it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    territorial_nexus_threshold_ambiguity,
    'Is there a principled territorial-nexus threshold (e.g., number of EU customers, volume of monitoring, local infrastructure use) below which Article 3(2) enforcement is illegitimate under a sovereignty-bounded reading, or does the reading imply no extraterritorial application is legitimate regardless of scale?',
    'Comparative international law analysis of how sovereignty-bounded jurisdictional doctrines (e.g., traditional conflict-of-laws minimum-contacts tests) would translate to data protection, and whether any enforcement actions to date have been confined to a de facto nexus threshold.',
    'A principled threshold reading would soften this constraint toward tangled_rope-with-narrower-scope; a zero-tolerance sovereignty reading would push toward classifying nearly all extraterritorial enforcement as illegitimate extraction, sharpening the snare characterization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_nexus_threshold_ambiguity, conceptual, 'Whether territorial sovereignty implies a graduated nexus test or a bright-line territorial boundary.').

omega_variable(
    kernel_reading_indeterminacy,
    'Which of the three readings of Article 3(2) scope (territorial_sovereignty, effects_jurisdiction, market_access) actually governs enforcement practice, and is the indeterminacy itself a resource that different institutional actors exploit strategically?',
    'Track which reading EU courts, foreign courts, and trade tribunals actually adopt in resolved disputes over time; track whether non-EU regulators cite the sovereignty reading opportunistically only when it serves their own counter-jurisdictional claims.',
    'If institutional actors invoke whichever reading serves their immediate interest without genuine commitment to a consistent jurisdictional theory, that supports treating the disagreement itself as extraction cover (tangled_rope) rather than a good-faith unresolved legal question (which would push toward a more mountain-like unresolved-natural-law framing).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the multiplicity of Article 3(2) readings reflects genuine jurisprudential uncertainty or strategic selection by interested parties.').

omega_variable(
    reciprocal_extraterritoriality_precedent_causality,
    'Does the EU''s extraterritorial claim under GDPR actually cause non-EU states to adopt their own extraterritorial or data-localization rules, or would those rules have emerged regardless as a general trend in digital sovereignty policy?',
    'Comparative timeline analysis of data localization and extraterritorial privacy legislation adoption dates relative to GDPR enforcement actions, controlling for other digital-sovereignty drivers (e.g., surveillance concerns, industrial policy).',
    'If causally linked, the non_eu_state_regulators beneficiary declaration is strongly supported; if the trend is independent, the beneficiary relationship is weaker and the constraint''s extraction profile should be reassessed downward.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reciprocal_extraterritoriality_precedent_causality, empirical, 'Whether GDPR extraterritoriality causes or merely coincides with rising global data localization policy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__territorial_sovereignty_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t0, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(gdpr_tr_t4, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(gdpr_tr_t8, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 8, 0.23).
narrative_ontology:measurement(gdpr_tr_t12, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement(gdpr_tr_t16, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement(gdpr_tr_t20, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement(gdpr_tr_t24, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 24, 0.3).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t0, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(gdpr_be_t4, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(gdpr_be_t8, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(gdpr_be_t12, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(gdpr_be_t16, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(gdpr_be_t20, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(gdpr_be_t24, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_su_t0, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(gdpr_su_t4, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 4, 0.42).
narrative_ontology:measurement(gdpr_su_t8, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(gdpr_su_t12, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 12, 0.51).
narrative_ontology:measurement(gdpr_su_t16, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 16, 0.54).
narrative_ontology:measurement(gdpr_su_t20, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(gdpr_su_t24, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope__effects_jurisdiction_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope__market_access_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language concept 'GDPR's extraterritorial scope' per the ε-invariance principle: measuring the provision through a territorial-sovereignty lens versus an effects-jurisdiction lens versus a market-access lens yields three structurally distinct claims with different beneficiary/victim structures and different ε. All three share the gdpr_article_3_scope kernel and are linked via affects_constraints; the sovereignty reading forecloses the effects reading within any single legal framework (a jurisdiction cannot simultaneously hold that presence-based territory is the sole legitimate basis AND that resident-location-based targeting is sufficient) while merely influencing the market-access reading (a market-access framing can absorb sovereignty objections as one more cost of doing business without being logically refuted by them).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
