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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: gdpr_article_3_scope__effects_jurisdiction_reading
 *   human_readable: GDPR Article 3(2) Effects-Based Jurisdiction Over Non-EU Controllers
 *   domain: technology_governance/international_law/privacy_regulation
 *
 * SUMMARY:
 *   Article 3(2) of the GDPR extends the regulation's territorial scope
 *   beyond the EU's borders by applying GDPR standards to any controller that
 *   'targets' or 'monitors' EU residents or offers goods/services to them.
 *   This constraint represents the 'effects-jurisdiction reading'—a committer
 *   position that treats GDPR jurisdiction as legitimate when the effects of
 *   processing fall on EU residents, regardless of where the controller is
 *   incorporated or where processing occurs. The reading is contested:
 *   territorial-sovereignty readings argue the effects-based test exceeds
 *   legitimate regulatory authority; market-access readings reframe
 *   extraterritoriality as conditional access to EU markets rather than
 *   jurisdiction. This story instantiates only the effects-jurisdiction
 *   reading, with its own ε, beneficiaries, and type classification. The
 *   competing readings are separate constraint stories, linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - eu_data_subjects: powerless targets of data processing, beneficiaries of the protection rule
 *   - eu_regulators: institutional agenda-setters that interpret and enforce Article 3(2) scope
 *   - non_eu_technology_companies: institutional payers bearing compliance costs and fines
 *   - us_government: excluded actor objecting to the jurisdiction theory
 *   - privacy_advocates: organized beneficiaries supporting extraterritorial reach
 *   - business_associations: organized payers opposing the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__effects_jurisdiction_reading, 0.68).
domain_priors:suppression_score(gdpr_article_3_scope__effects_jurisdiction_reading, 0.72).
domain_priors:theater_ratio(gdpr_article_3_scope__effects_jurisdiction_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__effects_jurisdiction_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__effects_jurisdiction_reading, "GDPR Article 3(2) Effects-Based Jurisdiction Over Non-EU Controllers").
narrative_ontology:topic_domain(gdpr_article_3_scope__effects_jurisdiction_reading, "technology_governance/international_law/privacy_regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__effects_jurisdiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__effects_jurisdiction_reading, '8724ef67-7820-4108-82e1-8f85a9097edf').
narrative_ontology:cs_kernel_codification('8724ef67-7820-4108-82e1-8f85a9097edf', fixed_text).
narrative_ontology:cs_authority_grounding('8724ef67-7820-4108-82e1-8f85a9097edf', extraction).
narrative_ontology:cs_interpretation_layer_present('8724ef67-7820-4108-82e1-8f85a9097edf').
narrative_ontology:cs_reading_relation('8724ef67-7820-4108-82e1-8f85a9097edf', gdpr_article_3_scope__territorial_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('8724ef67-7820-4108-82e1-8f85a9097edf', gdpr_article_3_scope__market_access_reading, influences).
narrative_ontology:cs_axiom('8724ef67-7820-4108-82e1-8f85a9097edf', foundational, extraterritorial_jurisdiction_legitimated_by_subject_effects).
narrative_ontology:cs_axiom_status(extraterritorial_jurisdiction_legitimated_by_subject_effects, holdable).
narrative_ontology:cs_axiom_grounding('8724ef67-7820-4108-82e1-8f85a9097edf', extraterritorial_jurisdiction_legitimated_by_subject_effects, deontological).
narrative_ontology:cs_axiom('8724ef67-7820-4108-82e1-8f85a9097edf', foundational, regulatory_authority_follows_processing_effects_not_territorial_presence).
narrative_ontology:cs_axiom_status(regulatory_authority_follows_processing_effects_not_territorial_presence, holdable).
narrative_ontology:cs_axiom_grounding('8724ef67-7820-4108-82e1-8f85a9097edf', regulatory_authority_follows_processing_effects_not_territorial_presence, conventional).
narrative_ontology:cs_reference_frame('8724ef67-7820-4108-82e1-8f85a9097edf', protection_based_extraterritorial_jurisdiction).
narrative_ontology:cs_drift_state('8724ef67-7820-4108-82e1-8f85a9097edf', contemporary_enforcement_phase, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8724ef67-7820-4108-82e1-8f85a9097edf', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_regulators).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_technology_companies).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_controllers_operating_in_eu).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, privacy_advocates).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_market_access_gatekeepers).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, business_associations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Residents of EU member states whose personal data is processed by controllers anywhere in the world, provided the controller targets or monitors EU residents or offers goods/services to them. Under Article 3(2), they gain statutory protection rights (access, rectification, erasure, portability) enforceable against foreign companies. Exit from the data economy is impractical; the constraint codifies their access to remedies rather than offering them a choice.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects, beneficiary,
    powerless, biographical, constrained, continental).

% National data protection authorities (DPAs) and the European Data Protection Board interpret Article 3(2), issue guidance on targeting/monitoring tests, investigate complaints from EU data subjects, and impose fines on non-compliant non-EU controllers. Enforce the boundary of the regulation's extraterritorial reach via administrative proceedings. Justify the reach as protecting EU fundamental rights; critics argue it asserts authority beyond legitimate territorial scope.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_regulators, agenda_setter,
    institutional, generational, analytical, continental).

% Large technology companies with global operations (e.g., social media platforms, search engines, ad networks, cloud services). Must comply with GDPR Article 3(2) even though they have no physical presence in the EU and may not have incorporated the regulation into their home jurisdiction's laws. Face compliance costs (data subject request processing, privacy impact assessments, consent mechanisms), audit burdens, and fines up to 4% of global revenue for violations. Their exit option is to cease offering services or data processing that targets EU residents—economically costly given EU market size.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_technology_companies, payer,
    institutional, generational, constrained, global).

% Mid-size and smaller non-EU technology companies, data brokers, and analytics firms that process EU resident data. Bear compliance costs and fines but lack the legal/compliance infrastructure of megacorporations. Their constraint is binding: they cannot ignore EU residents' data without abandoning revenue from EU operations; they cannot cheaply comply without substantial investment in privacy infrastructure.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_controllers_operating_in_eu, payer,
    powerful, generational, constrained, global).

% Does not recognize Article 3(2) as binding on US companies under US law. From the US vantage, the constraint is an extraterritorial overreach that violates principles of territorial jurisdiction and conflicts with US First Amendment doctrine on data flows. US companies lobby their government to challenge GDPR scope; US government has negotiated adequacy frameworks (Privacy Shield, later invalidated; Standard Contractual Clauses) but does not concede legitimacy to the effects-based jurisdiction theory itself.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, us_government, excluded,
    institutional, generational, trapped, global).

% Regulators in jurisdictions outside the EU (UK post-Brexit, China, India, etc.) view Article 3(2) as a model they can adopt or as a precedent they oppose. They would object to EU unilateral jurisdiction assertions, yet some have adopted similar reach themselves. Their objection is to the principle of extraterritorial jurisdiction, not to the data subject protection goal.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, third_country_regulators, excluded,
    institutional, generational, trapped, global).

% NGOs and civil society organizations that support GDPR enforcement endorse Article 3(2)'s extraterritorial reach as necessary to protect EU residents from processing by companies that would otherwise escape accountability. They argue that a territorial limitation would create a loophole: non-EU companies could process EU resident data with impunity by claiming no local jurisdiction. They advocate for strict interpretation and vigorous enforcement.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, privacy_advocates, beneficiary,
    organized, biographical, mobile, continental).

% Trade associations representing non-EU technology companies and digital businesses oppose Article 3(2) as imposing unilateral regulatory authority on companies that have no physical nexus to the EU. They argue for market-based approaches (consumer choice, contractual allocation of responsibility) rather than regulatory reach. Their exit is to lobby for carve-outs or to press home governments to challenge GDPR scope through trade negotiations.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, business_associations, payer,
    organized, generational, constrained, global).

% EU companies that benefit from GDPR's extraterritorial reach because it raises the compliance floor globally. Non-EU competitors face EU compliance costs whether or not they serve EU residents; if they try to compete on EU soil, they must meet GDPR standards. This indirectly raises non-EU competitors' costs relative to EU native companies, creating a competitive advantage for EU firms. The constraint benefits EU firms through a compliance-cost barrier that protects their market share.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_market_access_gatekeepers, beneficiary,
    institutional, generational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gdpr_article_3_scope__effects_jurisdiction_reading, eu_regulators).
narrative_ontology:fixing_cost_class(gdpr_article_3_scope__effects_jurisdiction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a baseline data protection standard that applies globally to any controller processing EU resident data, eliminating a regulatory arbitrage where non-EU companies could evade accountability by claiming territorial jurisdiction does not bind them. Creates a single, enforceable rule: 'if you process EU resident data, you must comply with GDPR' regardless of where your company is incorporated or where the processing occurs.
% TRANSFER_FUNCTION: Moves compliance costs and fines from EU residents (who would otherwise bear the risk of unaccountable data processing) to non-EU controllers (who now must invest in privacy infrastructure, consent mechanisms, and risk management). Also transfers authority to adjudicate the scope of protection from home-country regulators of non-EU companies to EU data protection authorities and the CJEU.
% ABSENT_VOICES: US government, non-EU regulators, and third-country governments that view the constraint as an assertion of unilateral jurisdiction incompatible with territorial sovereignty. They are structurally excluded from the lawmaking process—the GDPR was adopted by the EU and EU institutions unilaterally. They may challenge it through trade dispute or refuse to enforce compatible standards in their own jurisdictions, but they have no seat at the GDPR rule-making table. Their objection, if present, would be that Article 3(2) exceeds legitimate regulatory authority.
% DISAPPEARANCE_RATIONALE: If Article 3(2) vanished overnight—if EU jurisdiction were limited to processing that occurs within EU territory—non-EU companies could resume processing EU resident data without GDPR compliance (by locating processing servers outside the EU, routing data extraterritorially, or using third-country service providers). EU regulators would lose enforcement authority. EU residents would face a regulatory gap: companies could target them, monitor them, and extract value from their data while claiming no jurisdictional obligation. The entire data protection baseline for EU residents would collapse unless EU companies unilaterally chose to comply out of market preference.
% FOUNDING_PROBLEM: The territorial model of jurisdiction proved inadequate to protect EU residents from data processing by companies with no local presence. Before Article 3(2), non-EU companies could argue they were not 'established' in the EU and thus fell outside GDPR scope entirely, even if they processed EU resident data. This created a regulatory loophole: the more sophisticated the company's jurisdictional engineering (by outsourcing processing, using non-EU subsidiaries, or routing data globally), the more effectively it could evade accountability to EU residents. Article 3(2) was adopted to close this loophole by moving from a territorial test ('is the controller in the EU?') to an effects test ('does the controller target or monitor EU residents?').
% FOUNDING_PROBLEM_CORROBORATION: Privacy advocates, EU regulators, and the European Commission attest the founding problem is live and the effects-based jurisdiction was necessary. Non-EU governments and business associations dispute the diagnosis: they argue the founding problem is not unaccountable data processing, but rather regulatory overreach and extraterritorial assertion of authority. CJEU case law (Schrems I, Schrems II) has upheld Article 3(2) scope and extended it via case interpretation; this corroborates the EU reading from the judicial authority. No corroboration from outside the EU regulatory ecosystem supports the founding problem diagnosis; the US and third countries have not adopted similar jurisdiction tests and actively resist the principle.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__effects_jurisdiction_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__effects_jurisdiction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__effects_jurisdiction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gdpr_article_3_scope__effects_jurisdiction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__effects_jurisdiction_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness (0.68 at interval end) reflects that non-EU controllers bear substantial compliance burdens—data infrastructure investment, consent and request processing, risk of large fines up to 4% of global revenue—while the primary benefit accrues to EU regulators' authority and EU residents' protection. The constraint rises from 0.58 to 0.68 over the interval because Case Schrems II (2020) and subsequent DPA guidance broadened the targeting/monitoring test, raising the cost for non-EU companies. Suppression is high (0.72) because the constraint's persistence depends on EU regulators actively enforcing the targeting/monitoring test and rejecting non-EU companies' jurisdictional objections. Theater starts low (0.12) because the constraint's protective function is genuine—EU residents do gain access to remedies against non-EU controllers—but rises over time (to 0.28) as secondary effects become more prominent: non-EU companies increasingly report compliance theater (privacy policies, consent boxes) that signal compliance without substantive privacy change, especially among smaller non-EU actors who invest in compliance framing rather than systemic privacy redesign. Accessibility collapse is high (0.81) because the targeting/monitoring test is broad enough that non-EU companies cannot realistically avoid it if they operate on global platforms or ad networks; the only true exit is geographic isolation or data localization, both costly. Resistance is substantial (0.64) because the US government, other third countries, and business associations actively dispute the constraint's legitimacy through trade negotiations, adequacy challenges, and Standard Contractual Clause litigation (Schrems II); this resistance persists because the foundational premise—that effects-based jurisdiction is legitimate—is deeply contested.
 *
 * PERSPECTIVAL GAP:
 *   From the EU regulatory and data-subject seat, Article 3(2) is a necessary protection rule that closes a loophole; from the non-EU controller and home-country regulator seat, it is extraterritorial authority assertion that violates principles of territorial sovereignty. The engine computes this divergence from the structural data: non-EU controllers experience high directionality (d near 1.0 = target) because they bear concentrated compliance costs and have constrained exit; EU residents experience low directionality (d near 0.0 = beneficiary) because the constraint subsidizes their protection and they have no exit from the data economy anyway. EU regulators sit in the agenda-setter seat with analytical directionality. The Schrems II litigation shows the structural asymmetry: when third-country governments challenge the constraint's scope, they have no seat in the CJEU proceedings that uphold it.
 *
 * DIRECTIONALITY LOGIC:
 *   EU data subjects are beneficiaries (d ≈ 0.1): they gain access to statutory remedies against non-EU controllers without bearing compliance costs directly. Non-EU technology companies are targets (d ≈ 0.85): they face binding compliance obligations, audit burden, and fine risk without a parallel right to challenge the jurisdiction theory in their home courts. EU regulators are the agenda-setters (d ≈ 0.5): they wield interpretive authority over the targeting/monitoring test and enforce it, but must also justify the reach to international constituencies and manage pushback from other regulators. Privacy advocates are beneficiaries through the regulatory mechanism (d ≈ 0.15). Business associations are secondary targets (d ≈ 0.8): they bear advocacy costs and market pressure from member companies' compliance burdens. The constraint's effectiveness depends on this asymmetry: non-EU controllers cannot easily exit without abandoning EU-derived revenue; EU regulators have institutional authority to enforce; EU subjects have no bargaining power but also no cost-bearing responsibility.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as tangled_rope because it possesses both a genuine coordination function (eliminating regulatory arbitrage, establishing a single enforceable data protection baseline) AND asymmetric extraction (non-EU controllers bear concentrated costs; EU regulators and residents benefit; the cost-to-benefit ratio is structurally asymmetric). The coordination story alone would suggest rope-type classification: 'all data controllers should comply with consistent standards.' But the constraint is not voluntary or low-cost—it is enforced through fines and legal proceedings, and it is imposed unilaterally by EU institutions without reciprocal obligation from non-EU jurisdictions. The extraction story alone would suggest snare-type classification: 'non-EU companies are coerced into a cost-bearing regime they did not consent to.' But the coordination function is real—the constraint solves a genuine problem (regulatory loophole, unaccountable processing) that EU residents and regulators face. Tangled_rope captures both: the coordination function (protection for EU residents) rides on top of an asymmetric extraction mechanism (compliance costs for non-EU controllers), and persistence depends on active enforcement (DPA proceedings, fines, CJEU backing). The mandate of 'protecting EU resident data' is live and still actively served, which distinguishes this from a piton; the constraint is not performative theater maintained by institutional inertia. The theater_ratio rise (0.12 to 0.28) tracks the growth of compliance signaling relative to privacy redesign, but does not reverse the functional core.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    jurisdiction_vs_market_access_premise,
    'Is Article 3(2) an assertion of extraterritorial JURISDICTION (as the effects-jurisdiction reading holds), or merely a conditional requirement for MARKET ACCESS to the EU (as the market-access reading claims)?',
    'If EU courts and regulators explicitly frame Article 3(2) as a market-access condition (not jurisdiction), and non-EU companies can exit the EU market without GDPR fines if they cease serving EU residents entirely, then the market-access reading becomes dominant. If EU courts assert that Article 3(2) binds non-EU companies regardless of market intention (even passive data collection of EU residents), then the jurisdiction reading holds.',
    'If market-access framing prevails, ε drops to ~0.3 (voluntary coordination, low extraction); if jurisdiction framing persists, ε stays high (extraction + enforcement). This is a reading-selection ambiguity, not a metrics divergence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(jurisdiction_vs_market_access_premise, conceptual, 'Whether Article 3(2) instantiates jurisdictional authority or market-access requirement.').

omega_variable(
    targeting_monitoring_test_clarity,
    'Does the ''targeting'' and ''monitoring'' test in Article 3(2) produce a clear, administrable boundary between covered and uncovered controllers, or does it generate perpetual interpretive drift?',
    'DPA guidance, CJEU case law, and compliance audit data over the next 5–10 years will show whether non-EU companies can achieve legal certainty about scope. If guidance converges and litigation declines, the boundary is clear; if guidance diverges (different DPAs issue conflicting targeting tests) and litigation expands, drift persists.',
    'High clarity strengthens the constraint''s legitimacy (beneficiaries can rely on stable rules; payers can plan compliance). Perpetual drift converts the constraint toward piton-type (performative adherence to an unclear rule, theater replacing function). If drift is high, theater_ratio eventually rises above 0.5.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(targeting_monitoring_test_clarity, empirical, 'Whether the effects-based test produces administrable scope or interpretive cascades.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the suppression of non-EU companies'' objections to Article 3(2) primarily structural (legal-system design prevents non-EU companies from challenging scope) or internalized (non-EU companies accept the constraints as legitimate even if they disagree)?',
    'Monitor non-EU company compliance trajectories after GDPR fines are imposed. If companies challenge fines through every available legal remedy (CJEU review, WTO dispute, trade negotiation pressure), suppression is structural. If companies pay fines and comply without mounting legal objections, suppression is partly internalized (companies have accepted the authority even if unhappy).',
    'If suppression is structural, the constraint persists by coercive force and is high-risk for reversal if EU enforcement capacity declines. If internalized, suppression is more stable but represents a shift in non-EU company legitimacy acceptance (a form of authority consolidation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether non-EU company suppression is structural (forced) or internalized (accepted).').

omega_variable(
    competing_readings_foreclosure_risk,
    'Could the territorial-sovereignty reading logically foreclose the effects-jurisdiction reading—i.e., could a CJEU decision affirm that GDPR Article 3(2) exceeds legitimate jurisdiction and void it?',
    'This is a committer-axis question about whether the kernel permits both readings to coexist in a single legal system. If the CJEU rules that effects-based jurisdiction is illegitimate under international law or EU constitutional law, the effects-jurisdiction reading is foreclosed within the EU legal order.',
    'Foreclosure would mean the effects-jurisdiction constraint collapses entirely (ε becomes irrelevant). Non-foreclosure means the readings remain in tension but coexist in different legal orders (EU affirms effects, US/third countries deny legitimacy).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competing_readings_foreclosure_risk, conceptual, 'Whether territorial-sovereignty and effects-jurisdiction readings logically foreclose each other.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__effects_jurisdiction_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t0, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(gdpr_tr_t0, observed).
narrative_ontology:measurement(gdpr_tr_t4, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 4, 0.16).
narrative_ontology:measurement_basis(gdpr_tr_t4, observed).
narrative_ontology:measurement(gdpr_tr_t8, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement_basis(gdpr_tr_t8, observed).
narrative_ontology:measurement(gdpr_tr_t12, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement_basis(gdpr_tr_t12, observed).
narrative_ontology:measurement(gdpr_tr_t16, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement_basis(gdpr_tr_t16, observed).
narrative_ontology:measurement(gdpr_tr_t20, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(gdpr_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t0, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(gdpr_be_t0, observed).
narrative_ontology:measurement(gdpr_be_t4, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 4, 0.62).
narrative_ontology:measurement_basis(gdpr_be_t4, observed).
narrative_ontology:measurement(gdpr_be_t8, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 8, 0.65).
narrative_ontology:measurement_basis(gdpr_be_t8, observed).
narrative_ontology:measurement(gdpr_be_t12, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 12, 0.67).
narrative_ontology:measurement_basis(gdpr_be_t12, observed).
narrative_ontology:measurement(gdpr_be_t16, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 16, 0.68).
narrative_ontology:measurement_basis(gdpr_be_t16, observed).
narrative_ontology:measurement(gdpr_be_t20, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(gdpr_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_su_t0, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 0, 0.64).
narrative_ontology:measurement_basis(gdpr_su_t0, observed).
narrative_ontology:measurement(gdpr_su_t4, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 4, 0.68).
narrative_ontology:measurement_basis(gdpr_su_t4, observed).
narrative_ontology:measurement(gdpr_su_t8, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 8, 0.7).
narrative_ontology:measurement_basis(gdpr_su_t8, observed).
narrative_ontology:measurement(gdpr_su_t12, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 12, 0.71).
narrative_ontology:measurement_basis(gdpr_su_t12, observed).
narrative_ontology:measurement(gdpr_su_t16, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 16, 0.72).
narrative_ontology:measurement_basis(gdpr_su_t16, observed).
narrative_ontology:measurement(gdpr_su_t20, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(gdpr_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__effects_jurisdiction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gdpr_article_3_scope__effects_jurisdiction_reading, 0.12).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope__territorial_sovereignty_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope__market_access_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, adequacy_framework_data_transfer).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, standard_contractual_clauses_enforcement).

% DUAL FORMULATION NOTE:
% Article 3(2) scope is a contested kernel with three structural readings. The effects_jurisdiction_reading (this constraint) treats extraterritorial reach as legitimate when processing effects fall on EU residents. The territorial_sovereignty_reading rejects this as exceeding jurisdictional authority. The market_access_reading reframes the constraint as a market-entry requirement, not jurisdiction. Each reading produces different ε values and type classifications. They are linked via network.affects_constraints because each reading's credibility and enforcement affects the others' operational environment: if effects-jurisdiction reading strengthens (via case law), it raises costs for territorial-sovereignty and market-access readings; if US challenges the jurisdiction theory, enforcement uncertainty rises for effects-reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gdpr_article_3_scope__effects_jurisdiction_reading, institutional, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
