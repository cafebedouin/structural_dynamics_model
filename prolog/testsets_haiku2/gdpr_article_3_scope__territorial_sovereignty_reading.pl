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
 *   human_readable: GDPR Article 3 Extraterritorial Scope: Territorial Sovereignty Reading
 *   domain: regulatory/international_law/privacy
 *
 * SUMMARY:
 *   The European Union's General Data Protection Regulation, Article 3(2),
 *   extends GDPR's scope to any organization processing the personal data of
 *   EU residents, regardless of where the organization is located or where
 *   the processing occurs. This constraint story instantiates the territorial
 *   sovereignty reading: that extraterritorial application of GDPR to non-EU
 *   firms handling non-EU residents' data in non-EU jurisdictions exceeds the
 *   legitimate bounds of regulatory authority under customary international
 *   law. Under this reading, the EU asserts jurisdiction based on the
 *   incidental fact that a firm's global processing pipeline touches EU
 *   residents, transforming the firm's home-jurisdiction data practices into
 *   EU-regulated activity without the home jurisdiction's consent. The claim
 *   is TANGLED ROPE (genuine coordination function—privacy protection
 *   baseline—combined with asymmetric extraction—EU regulatory reach
 *   displacing non-EU sovereignty). The authored metrics describe an
 *   extractive arrangement with rising enforcement intensity and moderate
 *   performative activity.
 *
 * KEY AGENTS:
 *   - European Commission: administers GDPR Article 3(2) through national DPAs; agenda-setter; controls interpretation and enforcement
 *   - Non-EU technology firms (US, Chinese, Indian): subject to GDPR despite no home-jurisdiction consent; constrained exit; pay compliance costs
 *   - Non-EU state regulators: structurally lose authority over their own residents' data flows; trapped; nominally benefit from privacy floor but lose autonomy
 *   - Non-EU data processors: gain competitive advantage from compliance-cost barrier to foreign competitors; benefit from Brussels Effect
 *   - EU residents: receive automatic privacy protection; beneficiary by identity-lock (no exit from EU residence)
 *   - Data localization advocates: excluded from policy conversation; would counter-assert national data sovereignty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__territorial_sovereignty_reading, 0.68).
domain_priors:suppression_score(gdpr_article_3_scope__territorial_sovereignty_reading, 0.71).
domain_priors:theater_ratio(gdpr_article_3_scope__territorial_sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__territorial_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__territorial_sovereignty_reading, "GDPR Article 3 Extraterritorial Scope: Territorial Sovereignty Reading").
narrative_ontology:topic_domain(gdpr_article_3_scope__territorial_sovereignty_reading, "regulatory/international_law/privacy").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__territorial_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__territorial_sovereignty_reading, '4d04e244-c4d7-41d8-a90a-785f6a835e4f').
narrative_ontology:cs_kernel_codification('4d04e244-c4d7-41d8-a90a-785f6a835e4f', fixed_text).
narrative_ontology:cs_authority_grounding('4d04e244-c4d7-41d8-a90a-785f6a835e4f', extraction).
narrative_ontology:cs_interpretation_layer_present('4d04e244-c4d7-41d8-a90a-785f6a835e4f').
narrative_ontology:cs_reading_relation('4d04e244-c4d7-41d8-a90a-785f6a835e4f', gdpr_article_3_scope__effects_jurisdiction_reading, coexists_with).
narrative_ontology:cs_reading_relation('4d04e244-c4d7-41d8-a90a-785f6a835e4f', gdpr_article_3_scope__market_access_reading, influences).
narrative_ontology:cs_axiom('4d04e244-c4d7-41d8-a90a-785f6a835e4f', foundational, jurisdiction_bounded_by_territorial_presence).
narrative_ontology:cs_axiom_status(jurisdiction_bounded_by_territorial_presence, holdable).
narrative_ontology:cs_axiom_grounding('4d04e244-c4d7-41d8-a90a-785f6a835e4f', jurisdiction_bounded_by_territorial_presence, deontological).
narrative_ontology:cs_axiom('4d04e244-c4d7-41d8-a90a-785f6a835e4f', foundational, regulatory_reach_requires_home_jurisdiction_consent).
narrative_ontology:cs_axiom_status(regulatory_reach_requires_home_jurisdiction_consent, holdable).
narrative_ontology:cs_axiom_grounding('4d04e244-c4d7-41d8-a90a-785f6a835e4f', regulatory_reach_requires_home_jurisdiction_consent, conventional).
narrative_ontology:cs_reference_frame('4d04e244-c4d7-41d8-a90a-785f6a835e4f', westphalian_territorial_sovereignty).
narrative_ontology:cs_drift_state('4d04e244-c4d7-41d8-a90a-785f6a835e4f', contemporary_gdpr_enforcement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4d04e244-c4d7-41d8-a90a-785f6a835e4f', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_state_regulators).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_data_processors).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_technology_firms).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_jurisdictions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, eu_residents).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_state_regulators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers GDPR Article 3(2) enforcement through national data protection authorities. Interprets 'targeting' and 'monitoring' of EU residents as sufficient basis for regulating non-EU firms' global data practices, including those in their home jurisdictions. Collects compliance leverage over non-EU firms as the price of market access to EU markets.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, european_commission, agenda_setter,
    institutional, generational, arbitrage, global).

% Subject to GDPR enforcement despite having no physical presence in EU and serving non-EU customers in non-EU jurisdictions. Must rewrite global data architectures, compliance programs, and customer contractual terms to conform to EU law whenever they incidentally handle data of EU residents. Exit options: exit EU markets, apply EU standards globally (absorbing cost), or litigate across member states.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_technology_firms, payer,
    powerful, biographical, constrained, global).

% Face data localization pressures and regulatory conflict when their domestic data-handling norms diverge from GDPR. Nominally benefit from GDPR's privacy floor (indirectly raising baseline protections for their own citizens), but lose regulatory autonomy over their data subjects' information. Must either harmonize with GDPR or experience enforcement friction in bilateral trade and technology partnerships.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_state_regulators, beneficiary,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_state_regulators, payer).

% Data processors and cloud infrastructure providers in non-EU jurisdictions gain competitive advantage when EU regulation raises compliance costs for foreign competitors, enabling them to market 'GDPR-compliant' processing to multinational firms. Also face selective enforcement when they operate in EU markets or process EU residents' data incidentally.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_data_processors, beneficiary,
    moderate, biographical, mobile, regional).

% Experience erosion of jurisdictional authority over their own residents' and firms' data practices. Firms domiciled in their territory comply with EU law even when handling purely domestic data flows if those flows touch EU residents incidentally. Cannot enforce counter-regulatory policies (e.g., data sovereignty, localization mandates, surveillance capitalism acceptance) without creating non-compliance in global firms.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_jurisdictions, payer,
    organized, generational, trapped, global).

% Receive privacy protection by virtue of regulatory reach: non-EU firms operating on EU territory or incidentally handling their data must comply with GDPR even when serving non-EU users. Protection is automatic and jurisdiction-independent—a non-EU firm processing EU residents' data from anywhere on Earth triggers GDPR scope. Cost is borne by non-EU jurisdictions and firms that must absorb or restructure around compliance.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, eu_residents, beneficiary,
    powerless, biographical, identity_locked, local).

% Monitor the precedent of unilateral extraterritorial regulatory reach and its reciprocal tension with principles of sovereignty and comity in international law. The territorial sovereignty reading flags this as a violation of customary international law boundaries; the effects reading defends it as legitimate protective jurisdiction; market access reading sidesteps the question by framing it as conditional market entry, not jurisdictional assertion.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, international_law_bodies, observer,
    institutional, generational, analytical, global).

% Governments and firms in non-EU jurisdictions that mandate data localization as a counter-regulatory strategy. Excluded from the EU's policy conversation over Article 3(2) scope; would argue the extraterritorial reach delegitimizes EU claims to privacy leadership and justifies national data sovereignty enforcement. Their exclusion from the interpretation process is enforced by GDPR's unilateral design—they have no seat at the table determining the meaning of Article 3.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, data_localization_advocates, excluded,
    powerful, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gdpr_article_3_scope__territorial_sovereignty_reading, european_commission).
narrative_ontology:fixing_cost_class(gdpr_article_3_scope__territorial_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform baseline for personal data protection across all actors handling EU residents' data, anywhere on Earth. Solves the coordination problem of determining whose data protection standards apply when a non-EU firm serves both EU and non-EU customers: the answer is GDPR applies to the EU-resident subset of data, enforced from Brussels.
% TRANSFER_FUNCTION: Transfers regulatory authority from non-EU states and firms to EU institutions: when a non-EU firm handles EU residents' data in any context, GDPR's compliance requirements displace the firm's home jurisdiction's data rules. The firm must adopt EU standards globally to operate, creating a de facto imposition of EU regulatory preferences on non-EU jurisdictions without their consent or participation.
% ABSENT_VOICES: Non-EU state regulators are excluded from the determination of Article 3(2) scope. Data localization advocates (governments mandating domestic data storage) are not participants in GDPR interpretation. Firms in non-EU jurisdictions have no standing in the process that extends EU law's reach to their home territory. The reading is authored by EU institutions from within EU jurisdictions.
% DISAPPEARANCE_RATIONALE: If GDPR Article 3(2) extraterritorial scope disappeared overnight, non-EU firms would route EU-resident data through separate processing pipelines compliant with their home jurisdiction's laws; non-EU states could enforce data localization and sovereignty mandates without triggering global compliance cascades; bilateral data-sharing treaties would replace unilateral EU reach; the regulatory landscape would fragment along territorial lines rather than converge on Brussels standards.
% FOUNDING_PROBLEM: Early internet scale revealed that personal data flows transcend borders: firms outside EU could collect, process, and profit from EU residents' data without EU-resident consent or knowledge. Pre-GDPR, a non-EU firm had no legal obligation to the EU for data handling unless operating on EU soil. The founding problem was the regulatory vacuum: how to protect EU residents from overseas data processing.
% FOUNDING_PROBLEM_CORROBORATION: EU institutions and privacy advocates attest the founding problem is live: data flows remain borderless and firms remain motivated to extract value from EU residents' information. Non-EU state regulators and international law scholars attest the founding problem is real BUT the solution (extraterritorial enforcement) exceeds legitimate authority—they argue jurisdictional reach should remain bounded by territorial presence or explicit trade agreement, not unilateral assertion of effects. Economic analysis and non-EU government testimony from trade negotiations support the 'solution exceeds problem' reading.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__territorial_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__territorial_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__territorial_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gdpr_article_3_scope__territorial_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__territorial_sovereignty_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.68 at interval end) because GDPR Article 3(2) enforces a unilateral regulatory displacement: non-EU firms must absorb compliance cost without having a vote in the rule-making process, and non-EU jurisdictions lose authority over their own data subjects' information flows. The constraint benefits EU institutions (regulatory reach) and nominally EU residents (privacy protection), but extracts from non-EU firms and non-EU jurisdictions. Suppression is high (0.71) because non-EU states cannot enforce counter-regulatory policies (data localization, national data sovereignty mandates) without triggering GDPR non-compliance among global firms. The suppression mechanism is structural: firms operating globally cannot segment EU and non-EU data handling without massive architecture costs, so they apply EU standards everywhere, collapsing non-EU jurisdictions' regulatory alternatives. Theater ratio is moderate (0.42): the privacy protection function is real, but a rising share of enforcement energy goes to defending jurisdictional reach against non-EU challenges and defending the 'targeting/monitoring' interpretations that expand scope—performative expansion of the legitimacy claim. Measurements show extractiveness rising as enforcement machinery matures (t=0 to t=12), then stabilizing as bottleneck acceptance sets in; suppression requirement rises alongside, indicating non-EU state resistance hardens proportionally to the constraint's entrenchment.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (non-EU firms and jurisdictions) and the agenda-setter seat (EU Commission) compute drastically different types from identical structural data because the positions reverse the beneficiary/victim axis. From the EU institutional perspective, GDPR Article 3(2) is legitimate protective jurisdiction defending EU residents from borderless data exploitation (a rope or even a mountain—naturally necessary privacy protection). From the non-EU firm seat, the same rule is coercive imposition of foreign law without consent (a snare—pure extraction). From the non-EU state regulator seat, it is an erosion of jurisdictional autonomy bundled with a privacy benefit (tangled rope, where the target is the state's regulatory freedom and the coordinator is the privacy floor). The engine's per-seat computation reveals this divergence; the authored metrics (high extractiveness, high suppression) reflect the territorial-sovereignty reading's assessment that the legitimacy claim is overstated relative to the jurisdictional reach.
 *
 * DIRECTIONALITY LOGIC:
 *   The EU institutional seat experiences this constraint as legitimate protective jurisdiction with real coordination benefits (d near 0.3—beneficiary, but subject to contestation). Non-EU technology firms experience it as unilateral regulatory imposition with constrained exit—they cannot exit the EU market without abandoning billions in revenue, and applying EU standards globally is the least-costly workaround (d near 0.75—target). Non-EU state regulators sit near d=0.6: they experience loss of regulatory autonomy (target-adjacent) but also diffuse benefit from privacy baseline (beneficiary-adjacent). EU residents sit at d near 0.0 (full beneficiary—protection without cost, because cost is externaliz to non-EU jurisdictions). International law bodies sit at d=0.5 (symmetric: genuine coordination problem, genuine rule-of-law concern). The engine derives these from the declared beneficiary/victim structure and exit options; the reading's core claim (that extraterritoriality exceeds legitimate authority) drives the beneficiary/victim declaration itself—non-EU states and firms are victims under this reading, whereas the effects reading and market-access reading would list them differently.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—regulatory vacuum for borderless data processing—is structurally live (firms still extract value from EU data without EU consent). However, the territorial-sovereignty reading's answer (extraterritorial enforcement) creates a new problem: jurisdictional conflict and the loss of non-EU regulatory autonomy. This is a case where the solution generates a secondary mandate: if extraterritorial reach is illegitimate, then legitimate solutions must remain within territorial bounds (bilateral treaties, market-access conditions, EU-only enforcement). The constraint is not purely a mandatrophy (the founding problem is real and persistent), but it carries a contestable mandate: is unilateral extraterritorial enforcement the legitimate response, or a power grab? The three readings parse this precisely: effects jurisdiction says the mandate is live; market access sidesteps the mandate question; territorial sovereignty says the mandate has exceeded its bounds. No override to mandatrophy_resolved is needed here; the constraint honestly carries the contest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraterritorial_legitimacy_boundary,
    'What is the legitimate boundary of jurisdictional reach under customary international law? Does protecting one''s own residents justify regulating non-residents'' behavior in their home jurisdiction?',
    'International law tribunal or consensus-building around principles of comity and territorial sovereignty; bilateral treaty frameworks establishing mutual enforcement limits; reciprocal regulatory assertions by other jurisdictions that invoke the same ''effects'' logic on EU firms.',
    'If territorial sovereignty is the legitimate limit, GDPR Article 3(2) is an overreach and should be narrowed to firms with EU presence or explicit EU market participation. If effects-based jurisdiction is legitimate, the constraint''s scope is justified. If neither—if legitimacy requires explicit treaty—then unilateral GDPR enforcement is inherently illegitimate regardless of its merits.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraterritorial_legitimacy_boundary, conceptual, 'What constitutes legitimate extraterritorial regulatory reach under international law principles.').

omega_variable(
    data_localization_as_suppression,
    'Is non-EU state resistance to GDPR extraterritoriality expressed through data localization mandates a legitimate assertion of regulatory autonomy, or an extractive counter-measure that suppresses privacy rights?',
    'Comparative analysis of localization mandates in GDPR-constrained jurisdictions (India, Russia, China) to distinguish sovereignty assertion from surveillance-capacity protection; empirical study of whether data localization mandates correlate with improved privacy outcomes or with government surveillance expansion.',
    'If localization is primarily autonomy-assertion, GDPR''s suppression of localization (by making global standard-setting impossible) is extractive. If localization is primarily a cover for surveillance, then GDPR''s suppression of localization is protective—flipping the victim/beneficiary axis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_localization_as_suppression, empirical, 'Whether data localization in non-EU jurisdictions serves privacy or state control.').

omega_variable(
    structural_vs_ideological_suppression,
    'How much of the suppression of non-EU regulatory alternatives is structural (global compliance architecture prevents segmentation) versus ideological (GDPR''s privacy legitimacy claim suppresses non-EU states'' ability to articulate counter-regulatory positions without appearing anti-privacy)?',
    'Post-exit analysis: if a non-EU jurisdiction formally rejects GDPR and builds alternative privacy regimes, would firms still apply GDPR-equivalent standards globally? Do non-EU policymakers report self-censoring on data regulation out of fear of appearing privacy-hostile?',
    'If suppression is primarily structural, technical solutions (data pipeline segmentation, jurisdiction-aware processing) could reduce extraction. If suppression is primarily ideological, the constraint''s legitimacy cover prevents its contestation even when the underlying extraction is unjustified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_ideological_suppression, empirical, 'Mechanism of suppression in non-EU state regulatory autonomy: technical or narrative-based.').

omega_variable(
    kernel_codification_and_interpretation,
    'Is Article 3(2) a fixed, bounded rule susceptible to narrow interpretation (territorial + effects only), or an open-textured authority that legitimizes expansive interpretation by EU institutions?',
    'Historical analysis of Article 3(2) drafting intent; comparison with parallel provisions in other international regulatory instruments; empirical tracking of how EU DPAs interpret the scope over time (if interpretation expands, codification was open; if stable, codification was fixed).',
    'If Article 3(2) is fixed-text with limited interpretation, the territorial sovereignty reading is defensible as a narrowing interpretation. If it is open-textured delegation to EU institutions, any interpretation they offer is within legitimate authority—shifting the legitimacy verdict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_codification_and_interpretation, empirical, 'The epistemic status of Article 3(2): fixed rule or open authority for interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__territorial_sovereignty_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t0, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(gdpr_tr_t3, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 3, 0.32).
narrative_ontology:measurement(gdpr_tr_t6, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 6, 0.36).
narrative_ontology:measurement(gdpr_tr_t12, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement(gdpr_tr_t18, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 18, 0.42).
narrative_ontology:measurement(gdpr_tr_t24, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t0, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 0, 0.54).
narrative_ontology:measurement(gdpr_be_t3, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 3, 0.58).
narrative_ontology:measurement(gdpr_be_t6, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 6, 0.61).
narrative_ontology:measurement(gdpr_be_t12, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 12, 0.65).
narrative_ontology:measurement(gdpr_be_t18, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 18, 0.68).
narrative_ontology:measurement(gdpr_be_t24, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_su_t0, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(gdpr_su_t3, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 3, 0.62).
narrative_ontology:measurement(gdpr_su_t6, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 6, 0.66).
narrative_ontology:measurement(gdpr_su_t12, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(gdpr_su_t18, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 18, 0.71).
narrative_ontology:measurement(gdpr_su_t24, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 24, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__territorial_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gdpr_article_3_scope__territorial_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope__effects_jurisdiction_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope__market_access_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a three-reading kernel: gdpr_article_3_scope. The territorial_sovereignty_reading declares GDPR Article 3(2) as exceeding legitimate jurisdictional authority (high extraction, suppression of non-EU state autonomy, asymmetric beneficiary/victim structure). The effects_jurisdiction_reading instantiates the same Article 3(2) but with opposing beneficiary/victim structure—it treats extraterritorial reach as legitimate protective jurisdiction following effects on EU residents. The market_access_reading sidesteps the sovereignty question entirely, framing GDPR as a conditional market-access requirement. All three readings share the same kernel (Article 3(2) text) but generate different ε values, different victim sets, and different types because they assess the same rule through different legitimacy frameworks. They are linked via network.affects_constraints to enable contention and legitimacy-dispute analysis. The territorial_sovereignty_reading is the most skeptical of EU authority; the effects_jurisdiction_reading is the most supportive; market_access_reading is institutionally neutral. Per the ε-invariance principle, each is a complete constraint story; they are not perspectives on one constraint but distinct constraints grounded in the same text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
