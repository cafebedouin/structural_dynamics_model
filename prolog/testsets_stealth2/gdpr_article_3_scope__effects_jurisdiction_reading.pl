% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__effects_jurisdiction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: gdpr_article_3_scope__effects_jurisdiction_reading
 *   human_readable: GDPR Article 3(2) Extraterritorial Application (Effects-Jurisdiction Reading)
 *   domain: technology_governance/international_law/privacy_regulation
 *
 * SUMMARY:
 *   Article 3(2) of the GDPR extends the Regulation to controllers and
 *   processors outside the EU that offer goods or services to EU data
 *   subjects or monitor their behavior, regardless of where processing
 *   occurs. This story instantiates the effects-jurisdiction reading of the
 *   contested gdpr_article_3_scope kernel: jurisdiction follows effects on EU
 *   residents, operationalized through the targeting and monitoring tests.
 *   Under this reading the arrangement is a binding jurisdictional assertion
 *   enforced by supervisory authorities through fines up to four percent of
 *   worldwide turnover, adequacy decisions, and standard contractual clauses.
 *   The epsilon referent is the standing extraterritorial-application
 *   arrangement as this reading sees it — not the territorial-limit norm the
 *   sovereignty reading would install, nor the consensual access-pricing
 *   scheme the market-access reading describes. Sibling readings are separate
 *   constraint stories with their own epsilon, beneficiaries, and victims:
 *   the territorial-sovereignty reading evaluates the same provision as
 *   exceeding legitimate authority (its epsilon attaches to a different
 *   arrangement), and the market-access reading recasts the same compliance
 *   flows as voluntary pricing of market access (lower epsilon, consensual
 *   structure). This file links both siblings through
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - - eu_supervisory_authorities: Agenda setter (institutional/constrained) — administers and enforces the arrangement; collects fine revenue
 *   - - eu_data_subjects: Primary beneficiary (moderate/constrained) — holds enforceable rights; bears consent friction and service withdrawal
 *   - - non_eu_multinational_controllers: Primary payer (powerful/constrained) — bears compliance costs and fine exposure
 *   - - small_non_eu_exporters: Payer (powerless/trapped) — bears regressive fixed compliance costs
 *   - - foreign_data_protection_regulators: Excluded party (institutional/constrained) — regulatory space displaced; raises sovereignty objections
 *   - - eu_based_businesses: Secondary beneficiary (organized/constrained) — gains level playing field and transfer clarity
 *   - - dp_compliance_services_industry: Secondary beneficiary (organized/mobile) — sells compliance capacity created by the scope rule
 *   - - comparative_privacy_scholars: Analytical observer (analytical/analytical) — sees the full structure and the competing readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__effects_jurisdiction_reading, 0.66).
domain_priors:suppression_score(gdpr_article_3_scope__effects_jurisdiction_reading, 0.58).
domain_priors:theater_ratio(gdpr_article_3_scope__effects_jurisdiction_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__effects_jurisdiction_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__effects_jurisdiction_reading, "GDPR Article 3(2) Extraterritorial Application (Effects-Jurisdiction Reading)").
narrative_ontology:topic_domain(gdpr_article_3_scope__effects_jurisdiction_reading, "technology_governance/international_law/privacy_regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__effects_jurisdiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__effects_jurisdiction_reading, '38f19f60-8f5d-4f64-93ba-e5ece53abcef').
narrative_ontology:cs_kernel_codification('38f19f60-8f5d-4f64-93ba-e5ece53abcef', fixed_text).
narrative_ontology:cs_authority_grounding('38f19f60-8f5d-4f64-93ba-e5ece53abcef', lineage).
narrative_ontology:cs_interpretation_layer_present('38f19f60-8f5d-4f64-93ba-e5ece53abcef').
narrative_ontology:cs_reading_relation('38f19f60-8f5d-4f64-93ba-e5ece53abcef', gdpr_article_3_scope__territorial_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('38f19f60-8f5d-4f64-93ba-e5ece53abcef', gdpr_article_3_scope__market_access_reading, influences).
narrative_ontology:cs_axiom('38f19f60-8f5d-4f64-93ba-e5ece53abcef', foundational, protective_effects_ground_jurisdiction).
narrative_ontology:cs_axiom_status(protective_effects_ground_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('38f19f60-8f5d-4f64-93ba-e5ece53abcef', protective_effects_ground_jurisdiction, deontological).
narrative_ontology:cs_axiom('38f19f60-8f5d-4f64-93ba-e5ece53abcef', foundational, targeting_monitoring_suffices_for_application).
narrative_ontology:cs_axiom_status(targeting_monitoring_suffices_for_application, holdable).
narrative_ontology:cs_axiom_grounding('38f19f60-8f5d-4f64-93ba-e5ece53abcef', targeting_monitoring_suffices_for_application, conventional).
narrative_ontology:cs_reference_frame('38f19f60-8f5d-4f64-93ba-e5ece53abcef', effects_based_protective_jurisdiction).
narrative_ontology:cs_drift_state('38f19f60-8f5d-4f64-93ba-e5ece53abcef', contemporary_post_schrems_ii_enforcement_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('38f19f60-8f5d-4f64-93ba-e5ece53abcef', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_based_businesses).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, dp_compliance_services_industry).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_multinational_controllers).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, small_non_eu_exporters).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, foreign_data_protection_regulators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_supervisory_authorities).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__effects_jurisdiction_reading, effects_jurisdiction_doctrine).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__effects_jurisdiction_reading, charter_articles_7_8_data_protection_rights).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% National data protection authorities coordinated through the EDPB. They issue guidelines interpreting when a foreign company targets or monitors EU residents, open cross-border proceedings, and impose administrative fines calculated as a percentage of worldwide turnover. The scope of Article 3(2) defines their caseload, budget requests, and institutional standing, and fine revenue flows to member-state budgets. Their mandate is fixed by the same regulation whose reach they administer, so they operate inside the arrangement they police.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_supervisory_authorities, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__effects_jurisdiction_reading, eu_supervisory_authorities, beneficiary).

% Residents of EU member states who hold enforceable rights (access, rectification, erasure, portability, objection) against any company worldwide that offers them services or tracks their behavior. They gain a single enforceable standard regardless of where processing occurs. They also bear costs: consent interfaces on nearly every website, occasional withdrawal of services from the EU market, and price pass-through of compliance spending. They cannot exit EU residency, and their individual voice is mediated by NGOs and collective redress mechanisms.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects, beneficiary,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects, payer).

% Large non-EU platform and enterprise groups that offer services to EU users or monitor their behavior from offices and data centers outside the Union. They appoint EU representatives and data protection officers, maintain processing records, run impact assessments, notify breaches, and face administrative fines up to four percent of global turnover. Leaving the EU market or geofencing EU users would forfeit hundreds of millions of customers, so their realistic options are compliance, litigation, and lobbying rather than exit.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_multinational_controllers, payer,
    powerful, generational, constrained, global).

% Small and medium-sized companies outside the EU that sell into the EU or whose web analytics touch EU visitors. Fixed compliance steps such as EU representative appointment, documentation, and vendor contracts weigh heavily relative to modest EU revenue. Many respond by blocking EU IP addresses outright, shrinking their own market; those who continue absorb costs that larger competitors spread over bigger customer bases.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, small_non_eu_exporters, payer,
    powerless, immediate, trapped, regional).

% Data protection and consumer protection authorities outside the EU, for example the US FTC, state attorneys general, and counterparts in Asia and Latin America. Where the GDPR reaches conduct they would otherwise govern domestically, their regulatory space is displaced. They are consulted as adequacy partners and negotiation counterparts but did not author the rules that bind firms in their jurisdictions, and they periodically raise sovereignty and reciprocity objections.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, foreign_data_protection_regulators, excluded,
    institutional, generational, constrained, national).

% Companies established inside the EU that were already subject to European data protection law. Extraterritorial extension places the same duties on their foreign competitors, removing a cost asymmetry, and gives them clearer rules for international transfers through adequacy decisions and standard contractual clauses.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_based_businesses, beneficiary,
    organized, biographical, constrained, continental).

% Law firms, consultancies, outsourced data protection officers, audit providers, and consent-management vendors whose client demand is created by the obligation to comply wherever a firm touches EU residents. They serve clients globally and can redirect effort to other regimes, and increasingly do as other jurisdictions copy the model.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, dp_compliance_services_industry, beneficiary,
    organized, biographical, mobile, global).

% Academic and think-tank analysts of comparative privacy law and jurisdictional doctrine. They produce the accounts that describe Article 3(2) alternately as protective jurisdiction, as market-access conditionality, and as sovereignty overreach, and both defenders and critics of the regime cite their work. They bear none of the costs and collect none of the fines.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, comparative_privacy_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gdpr_article_3_scope__effects_jurisdiction_reading, diffuse).
narrative_ontology:fixing_cost_class(gdpr_article_3_scope__effects_jurisdiction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of protecting EU residents' personal data when processing is global: one enforceable standard binds any controller worldwide that targets or monitors EU residents, preventing controllers from escaping obligations by relocating processing beyond EU borders and giving firms a single predictable rule for the EU market.
% TRANSFER_FUNCTION: Moves compliance expenditure (legal, technical, organizational) and penalty exposure from non-EU controllers toward member-state public budgets (fines), the data protection compliance services industry (fees), and the protected position of EU residents' data rights; moves regulatory authority over certain conduct away from non-EU regulators toward EU authorities.
% ABSENT_VOICES: Non-EU data protection regulators would object that rules displacing their domestic authority were written without them; they appear only as adequacy counterparts. Non-EU individuals whose data about EU persons is swept into compliance scopes (business contacts, collaborators) were never consulted. Small non-EU exporters had no seat in the legislative process and discovered their obligations only at application.
% DISAPPEARANCE_RATIONALE: If Article 3(2) ceased to bind foreign controllers overnight, large platforms would restructure processing and corporate boundaries to place EU-facing operations beyond EU reach, EU residents' enforceable rights against foreign processors would collapse to whatever territorial remedies their own states provide, the adequacy and standard-contractual-clause ecosystem would unwind, and the compliance services market built on the scope rule would contract sharply.
% FOUNDING_PROBLEM: Data processing became borderless while enforcement jurisdiction remained territorial: under the 1995 Directive, controllers could strip EU residents of legal protection by serving or tracking them from servers and entities located outside the Union, and the CJEU's Google Spain line revealed effects-based gaps that territorial establishment tests could not close.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: CJEU judgments (Google Spain, Fashion ID, Schrems II) and EDPB guidelines trace the borderless-enforcement gap independently of EU institutional preference; non-EU legislatures (California, Brazil, India, among others) enacted effects-style scope rules of their own, an admission by non-beneficiaries that the gap is real; and US executive-branch and FTC statements acknowledge harms to Americans from foreign processing, confirming the problem class crosses jurisdictions. No corroborating source attests that the problem is solved.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__effects_jurisdiction_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__effects_jurisdiction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__effects_jurisdiction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gdpr_article_3_scope__effects_jurisdiction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__effects_jurisdiction_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

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
 *   The arrangement carries a genuine coordination function — a single enforceable data protection standard for a borderless processing environment — and simultaneously imposes asymmetric, actively enforced costs on non-EU actors, hence the tangled_rope claim. Extractiveness is 0.66: compliance burdens and fine exposure are substantial and decoupled from marginal harm in individual cases, yet the arrangement delivers real, valued protection rather than pure rent. Suppression is 0.58: enforcement machinery (proceedings, fines, adequacy leverage) is real, but exits remain — geofencing, market withdrawal, corporate restructuring — so alternatives are degraded, not eliminated (accessibility_collapse 0.35). Resistance 0.52 reflects sustained industry lobbying, sovereignty objections from third states, and litigation turbulence around transfer tools. Theater ratio 0.35: core enforcement is functional, but a growing share of observable compliance is performative (consent-interface ritual, boilerplate assessments), rising steadily since 2018. All three temporal series share one seven-point grid (2014-2026). The suppression_requirement series is authored because enforcement capacity is the traced dynamic: it was thin before 2018, built rapidly at application, and matured to a stable plateau by 2022-2026. The trajectories are monotonic ratchets, not cycles; no intermittent-reinforcement mechanism is alleged. Small exporters could in principle pool compliance resources to soften the regressive fixed-cost burden; the absence of such coalitions so far is itself evidence of how thinly their power spreads.
 *
 * PERSPECTIVAL GAP:
 *   From the supervisory-authority seat the arrangement computes as legitimate protective coordination it administers; from the multinational payer seat it computes as unilateral cost imposition with exit priced at forfeiture of a roughly 450-million-customer market; from the small-exporter seat it computes as a trap whose cheapest exit is blocking EU traffic; from the scholar seat it is a descriptive contest between three readings of one text. The engine computes these divergent per-seat classifications from power, exit, and directionality data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place EU data subjects, EU-established businesses, and the compliance services industry near the beneficiary end; victim declarations place non-EU multinationals, small exporters, and foreign regulators near the target end. One override is authored: the derivation would seat moderate-power agents (here, EU data subjects) near full beneficiary, but data subjects bear measurable cost incidence — consent friction, geo-blocked services, price pass-through — so d is corrected to 0.18. Supervisory authorities hold a dual position: they administer the arrangement (agenda_setter) and collect fine revenue (secondary beneficiary), keeping their derived d low while their enforcement interest keeps the machinery active. Foreign regulators are declared victims because the scope rule displaces their domestic regulatory space even though they pay no money — displacement of authority is the cost they bear.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — processing went borderless while enforcement stayed territorial — remains live: cross-border advertising technology, remote monitoring, and AI training runs on EU-resident data all proceed from outside the Union. No sunset exists and none is appropriate while the problem persists, so mandatrophy is not resolved and the constraint is not drifting toward piton through mandate death. The tangled_rope classification guards against two mislabels: a pure-rope label would erase the asymmetric, non-consensual cost incidence on non-EU payers; a pure-snare label would erase the genuine protective coordination EU residents would lose if the arrangement vanished. The receipt surface sharpens the distinction: gains are diffuse (internalized compliance overhead, fines scattered across twenty-seven public budgets, fees spread across a fragmented services industry), so no capturer seat converts this into a snare, while the live founding problem and concentrated coordination benefits block the piton reading despite diffuse gains and prohibitive fixing costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_3_kernel_reading_indexicality,
    'This constraint is one reading (effects_jurisdiction_reading) of the gdpr_article_3_scope kernel; would instantiating a sibling reading change the victim set and the classification?',
    'Compile the sibling stories (territorial_sovereignty_reading, market_access_reading) and compare computed per-seat classifications, victim sets, and epsilon across the family.',
    'The territorial reading removes non-EU controllers as victims and evaluates the arrangement as void overreach; the market-access reading recasts payers as consenting participants pricing access, lowering measured extraction toward rope-like coordination. The disagreement is located in the legal character of the same compliance flows.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_3_kernel_reading_indexicality, conceptual, 'Reading-indexicality of the Article 3 kernel: victim sets and classification are properties of the reading, not of the topic.').

omega_variable(
    reciprocity_legitimacy_pressure,
    'Will effects-based jurisdiction survive reciprocal assertion — if major non-EU states claim effects jurisdiction over EU firms'' processing abroad, does the EU position erode?',
    'Track reciprocal extraterritorial statutes, diplomatic protests, and mutual-adequacy negotiations over the next decade.',
    'Sustained reciprocity would raise resistance and threaten enforcement cooperation, pushing the arrangement toward contested legitimacy; managed reciprocity through mutual recognition would stabilize it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_legitimacy_pressure, empirical, 'Whether the reading''s legitimacy depends on asymmetric power that reciprocity would cancel.').

omega_variable(
    enforcement_capacity_gap,
    'Does doctrinal scope match practical enforcement, or do one-stop-shop bottlenecks and supervisory budget limits leave much of the claimed jurisdiction nominally asserted but practically unenforced?',
    'Fine collection rates, cross-border case durations, and lead-supervisory-authority throughput statistics compared against complaint volumes.',
    'A wide gap lowers effective extraction below the nominal measure and raises theater_ratio; convergence would validate the 0.66 extractiveness estimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_gap, empirical, 'Nominal versus operational reach of the scope rule.').

omega_variable(
    regressive_fixed_cost_incidence,
    'Do compliance costs scale regressively, concentrating burden on small non-EU exporters relative to multinationals?',
    'Firm-level compliance cost surveys disaggregated by size and EU revenue share; observed rates of EU traffic blocking by small non-EU sites.',
    'Confirmed regression would sharpen the victim structure into two distinct payer seats with different exit profiles and support proportionality-based reform pressure; absence would simplify the payer seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regressive_fixed_cost_incidence, empirical, 'Distribution of the compliance burden across payer scales.').

omega_variable(
    adequacy_transfer_stability,
    'Are adequacy decisions and standard contractual clauses stable enough to keep the arrangement functioning as coordination, or will successive invalidations along the Schrems litigation line convert transfer compliance into recurring crisis?',
    'Outcomes of pending challenge litigation and successor adequacy reviews; rate of transfer-mechanism replacement over time.',
    'Recurring invalidation raises suppression and resistance, degrades the coordination function, and pushes the arrangement toward a more purely extractive profile; durable mechanisms keep it tangled-rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adequacy_transfer_stability, empirical, 'Stability of the transfer-tool ecosystem the scope rule depends on.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__effects_jurisdiction_reading, 2014, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t2014, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2014, 0.1).
narrative_ontology:measurement(gdpr_tr_t2016, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2016, 0.12).
narrative_ontology:measurement(gdpr_tr_t2018, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2018, 0.2).
narrative_ontology:measurement(gdpr_tr_t2020, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2020, 0.26).
narrative_ontology:measurement(gdpr_tr_t2022, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2022, 0.3).
narrative_ontology:measurement(gdpr_tr_t2024, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2024, 0.33).
narrative_ontology:measurement(gdpr_tr_t2026, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2026, 0.35).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t2014, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2014, 0.35).
narrative_ontology:measurement(gdpr_be_t2016, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2016, 0.45).
narrative_ontology:measurement(gdpr_be_t2018, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2018, 0.55).
narrative_ontology:measurement(gdpr_be_t2020, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2020, 0.6).
narrative_ontology:measurement(gdpr_be_t2022, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2022, 0.63).
narrative_ontology:measurement(gdpr_be_t2024, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2024, 0.65).
narrative_ontology:measurement(gdpr_be_t2026, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2026, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_su_t2014, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2014, 0.22).
narrative_ontology:measurement(gdpr_su_t2016, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2016, 0.38).
narrative_ontology:measurement(gdpr_su_t2018, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2018, 0.5).
narrative_ontology:measurement(gdpr_su_t2020, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2020, 0.55).
narrative_ontology:measurement(gdpr_su_t2022, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2022, 0.57).
narrative_ontology:measurement(gdpr_su_t2024, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2024, 0.58).
narrative_ontology:measurement(gdpr_su_t2026, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2026, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__effects_jurisdiction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope__territorial_sovereignty_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope__market_access_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'GDPR extraterritoriality' decomposes into three structurally distinct readings of the Article 3 kernel: effects-jurisdiction (this file, epsilon 0.66 — enforced protective jurisdiction with asymmetric costs on non-EU actors), territorial-sovereignty (the same provision evaluated as exceeding legitimate authority), and market-access (the same compliance flows described as consensual conditional standard-setting, lower epsilon). Each is a separate constraint story with its own epsilon, beneficiaries, and victims, linked per the epsilon-invariance principle. The upstream doctrinal success of this reading — the accumulating enforcement record — supplies the phenomenon the other two readings describe or contest, which is why the influence edges run outward from this file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gdpr_article_3_scope__effects_jurisdiction_reading, moderate, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
