% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__effects_jurisdiction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: gdpr_article_3_scope__effects_jurisdiction_reading
 *   human_readable: GDPR Article 3(2) Extraterritorial Reach — Effects-Jurisdiction Reading
 *   domain: technological/legal/political
 *
 * SUMMARY:
 *   This story authors ONE reading of the GDPR Article 3(2) kernel: the
 *   effects-jurisdiction reading, under which the regulation legitimately
 *   reaches any controller worldwide that offers goods or services to people
 *   present in the EU or monitors their behavior, operationalized through the
 *   targeting/monitoring test. The standing arrangement under contest — and
 *   therefore the referent of epsilon — is the extraterritorial application
 *   machinery itself: representative appointment, records of processing,
 *   approved transfer mechanisms, fine exposure up to 4 percent of worldwide
 *   turnover, and the adequacy regime governing third countries. KEY AGENTS
 *   (by structural relationship): - eu_supervisory_authorities:
 *   Agenda-setting administrator (institutional/identity_locked) — enforces
 *   the arrangement and collects fines, budgets, and competence -
 *   eu_data_subjects: Primary intended beneficiary (organized/constrained) —
 *   receives enforceable rights against offshore processing -
 *   eu_compliant_incumbents: Secondary beneficiary (powerful/arbitrage) —
 *   collects relative competitive advantage from sunk compliance costs -
 *   non_eu_controllers_targeting_eu: Primary payer (powerful/constrained) —
 *   bears direct compliance and fine exposure - small_foreign_data_exporters:
 *   Disproportionate payer (powerless/trapped) — bears regressive fixed costs
 *   with no process voice - us_trade_negotiators: Excluded challenger
 *   (institutional/mobile) — contests the arrangement only outside the EU
 *   process - international_legal_academy: Analytical observer
 *   (analytical/analytical) — sees the full doctrinal structure. Sibling
 *   readings of the same kernel (territorial-sovereignty, market-access) are
 *   separate constraint stories with their own epsilon and stakeholder
 *   structure; they are linked through the network block, not folded into
 *   this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__effects_jurisdiction_reading, 0.66).
domain_priors:suppression_score(gdpr_article_3_scope__effects_jurisdiction_reading, 0.65).
domain_priors:theater_ratio(gdpr_article_3_scope__effects_jurisdiction_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__effects_jurisdiction_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__effects_jurisdiction_reading, "GDPR Article 3(2) Extraterritorial Reach — Effects-Jurisdiction Reading").
narrative_ontology:topic_domain(gdpr_article_3_scope__effects_jurisdiction_reading, "technological/legal/political").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__effects_jurisdiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__effects_jurisdiction_reading, '8f28cd06-ba19-4ae8-86ee-b269cf0c42e3').
narrative_ontology:cs_kernel_codification('8f28cd06-ba19-4ae8-86ee-b269cf0c42e3', formalized).
narrative_ontology:cs_authority_grounding('8f28cd06-ba19-4ae8-86ee-b269cf0c42e3', lineage).
narrative_ontology:cs_interpretation_layer_present('8f28cd06-ba19-4ae8-86ee-b269cf0c42e3').
narrative_ontology:cs_reading_relation('8f28cd06-ba19-4ae8-86ee-b269cf0c42e3', gdpr_article_3_scope__territorial_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('8f28cd06-ba19-4ae8-86ee-b269cf0c42e3', gdpr_article_3_scope__market_access_reading, coexists_with).
narrative_ontology:cs_axiom('8f28cd06-ba19-4ae8-86ee-b269cf0c42e3', foundational, protection_duty_follows_residents_not_controller_location).
narrative_ontology:cs_axiom_status(protection_duty_follows_residents_not_controller_location, holdable).
narrative_ontology:cs_axiom_grounding('8f28cd06-ba19-4ae8-86ee-b269cf0c42e3', protection_duty_follows_residents_not_controller_location, deontological).
narrative_ontology:cs_axiom('8f28cd06-ba19-4ae8-86ee-b269cf0c42e3', secondary, targeting_monitoring_test_sufficiently_administrable).
narrative_ontology:cs_axiom_status(targeting_monitoring_test_sufficiently_administrable, holdable).
narrative_ontology:cs_axiom_grounding('8f28cd06-ba19-4ae8-86ee-b269cf0c42e3', targeting_monitoring_test_sufficiently_administrable, instrumental).
narrative_ontology:cs_reference_frame('8f28cd06-ba19-4ae8-86ee-b269cf0c42e3', protective_effects_follow_residents_frame).
narrative_ontology:cs_drift_state('8f28cd06-ba19-4ae8-86ee-b269cf0c42e3', post_schrems_ii_enforcement_maturation, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('8f28cd06-ba19-4ae8-86ee-b269cf0c42e3', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_supervisory_authorities).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_compliant_incumbents).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_controllers_targeting_eu).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, small_foreign_data_exporters).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__effects_jurisdiction_reading, effects_based_jurisdiction_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% National data protection authorities and the European Data Protection Board administer and enforce the regulation: they investigate complaints, issue corrective orders and fines, adopt binding guidelines, and run the adequacy process for third countries. Their budgets and staffing expanded materially with the mandate. Their institutional self-conception is bound up with stewardship of the regime; stepping back from enforcement would mean abandoning the mission they were built for.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_supervisory_authorities, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__effects_jurisdiction_reading, eu_supervisory_authorities, beneficiary).

% People present in the EU gain enforceable rights (access, erasure, portability, objection) against any controller worldwide that handles their data, backed by complaint channels and representative litigation by NGOs. Their protection depends on supervisory capacity they do not control; individually they cannot opt out of being targeted by foreign services, and their practical remedy runs through authorities and civil-society litigants rather than direct action.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects, beneficiary,
    organized, biographical, constrained, continental).

% Established EU-headquartered platforms and firms that absorbed the fixed compliance build-out early. Ongoing obligations are largely sunk cost for them, while each new foreign entrant faces the same fixed costs fresh, so the rule prices market entry in their favor. They participate in shaping implementing guidance through industry associations and hold compliance capability most challengers lack.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_compliant_incumbents, beneficiary,
    powerful, biographical, arbitrage, global).

% Foreign-headquartered companies that offer services to people in the EU or monitor their behavior. They must appoint EU representatives, maintain processing records, honor data-subject requests, and structure transfers under approved mechanisms, with fine exposure up to 4 percent of worldwide turnover. Leaving the EU market is technically available but commercially severe at scale; day-to-day exit is limited to product-line withdrawals and geo-blocking, which some services have done.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_controllers_targeting_eu, payer,
    powerful, biographical, constrained, global).

% Small overseas businesses and publishers whose websites or services are reachable from the EU. Fixed compliance costs weigh disproportionately on them; common responses are blocking EU traffic, deploying minimally functional consent widgets, or absorbing costs that consume margins. They have no channel in the EU legislative process and little individual voice in enforcement consultations.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, small_foreign_data_exporters, payer,
    powerless, immediate, trapped, global).

% Trade officials of major non-EU economies raise the arrangement in bilateral and multilateral forums as an extraterritorial burden on their industries, seeking carve-outs, mutual-recognition deals, and adequacy-style accommodations. They operate entirely outside the EU legislative process; their leverage runs through market size and retaliatory framing rather than any seat in the rule's administration.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, us_trade_negotiators, excluded,
    institutional, generational, mobile, national).

% Scholars and practitioners of international law analyze whether the arrangement coheres with established jurisdictional doctrine, track CJEU and EDPB output, and publish assessments cited by courts and negotiators on all sides. They hold no stake in outcomes beyond professional standing.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, international_legal_academy, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gdpr_article_3_scope__effects_jurisdiction_reading, eu_supervisory_authorities).
narrative_ontology:fixing_cost_class(gdpr_article_3_scope__effects_jurisdiction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single uniform rule set so that personal data of people present in the EU receives enforceable protection regardless of where the processing controller is located. It solves a collective-action problem no individual member state could solve alone: foreign controllers could otherwise play divergent national regimes against each other, and individuals had no practical remedy against offshore processing.
% TRANSFER_FUNCTION: Moves compliance obligations, audit exposure, and fine risk onto any foreign controller that targets or monitors people in the EU; moves bargaining power over data-handling terms from controllers to individuals and their representatives; moves fine revenue to member-state budgets; and incidentally moves relative competitive position toward firms that have already absorbed the fixed costs.
% ABSENT_VOICES: Non-EU governments and the foreign firms bearing the costs had no seat in the ordinary legislative procedure; their interests entered only afterward through trade diplomacy and lobbying. Foreign data subjects whose home jurisdictions absorb EU-style rules through imitation were likewise absent. Dissent survives in trade negotiations and third-country blocking statutes, not in the EU process itself.
% DISAPPEARANCE_RATIONALE: If Article 3(2) ceased to reach foreign controllers overnight, non-EU firms would re-segment or drop EU-facing services rather than maintain compliance, member-state regimes would fragment back toward the pre-2018 patchwork, adequacy decisions and standard contractual clauses would lose their anchor, and EU residents would lose actionable rights against offshore processors — the cross-border data economy would reorganize around territorial enforcement limits.
% FOUNDING_PROBLEM: Directive 95/46 left enforcement to twenty-seven divergent national transpositions; foreign websites and platforms escaped effective oversight by sitting outside any single member state's reach, and high-profile cross-border data misuse showed individuals had no practical remedy against processing conducted from abroad.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Council of Europe Convention 108+ drafting records treat cross-border protection gaps as unresolved; the US FTC's own enforcement record documents the same categories of harm against consumers independent of EU law; and independent academic audits confirm continued large-scale violations, corroborating the supervisory authorities' own reports. No source outside the beneficiary set attests that the founding problem is solved.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__effects_jurisdiction_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__effects_jurisdiction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__effects_jurisdiction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored at 0.66: compliance burdens on foreign controllers are substantially decoupled from marginal harm prevention (fixed costs dominate, and they fall on any controller that touches the EU, however slight the processing), yet the arrangement delivers real, verifiable protection, which bounds epsilon below snare territory. Suppression is 0.65: persistence rests on catastrophic fine exposure and on the practical impossibility of abandoning the EU market for firms of scale, tempered by a genuine exit (geo-blocking, market withdrawal) that some actors have actually taken. Theater ratio is 0.30 with a documented hump: the consent-banner era drove performative compliance to roughly 0.38 around year 6 before supervisory focus on dark patterns and landmark penalties pulled activity back toward substance. Accessibility_collapse is 0.45 — alternatives (infrastructure segmentation, SCCs, adequacy reliance, withdrawal) survive but each carries material cost. Resistance is 0.5 — sustained litigation, trade-diplomatic pressure, and third-country blocking statutes, short of open defiance. The suppression_requirement series is authored deliberately: this interval saw visible enforcement-capacity buildup (DPA budget growth, EDPB binding decisions, the first nine-figure fines), so the enforcement trajectory is part of the story, not a static backdrop. All three metric series share one time grid (years since application, 2018 = t0); points after t8 are projections from the enforcement-maturation trend.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently, and the structural data explains why. From the agenda-setter seat (supervisory authorities), the arrangement is the protective duty it administers — costs are inputs to a rights mission. From the powerful payer seat (large foreign controllers), the same text is unrepresented rule-taking: binding obligations issued by a legislature in which they hold no vote, enforced by fines scaled to global turnover. From the incumbent beneficiary seat, the arrangement is an amortized moat. From the SME payer seat, it is an existential fixed cost with no process channel. Same instrument, four operative realities; the engine derives per-seat classifications from power, exit, and role data, and the divergence between the payer seats and the agenda-setter seat is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place eu_data_subjects, eu_supervisory_authorities, and eu_compliant_incumbents near the subsidized end of directionality: subjects receive protection without bearing the compliance burden; authorities collect fines, budgets, and institutional competence; incumbents collect entry-deterring advantage from costs they have already sunk. Victim declarations place both payer groups near the full-target end: non_eu_controllers_targeting_eu bear direct, enforced costs with constrained exit; small_foreign_data_exporters are trapped, bearing regressive fixed costs with neither market power nor process voice. The supervisory authorities' dual position (administration and receipt) is carried by their secondary_role rather than a directionality override, because the structural derivation from their beneficiary declaration plus identity-locked exit already captures it. Excluded and observer seats sit outside the extraction circuit: us_trade_negotiators contest the arrangement from outside it, and the academy collects nothing from its operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem remains live (cross-border data misuse persists, corroborated by non-beneficiary sources), so no mandatrophy declaration is authored and no zombie flag is expected from the status-by-verdict mismatch (live x world_rearranges). The tangled_rope claim does real work in both directions: naming the beneficiaries keeps the genuine coordination function visible — without it, the arrangement collapses into the territorial reading's picture of pure extraterritorial rent-seeking; naming the victims and active enforcement keeps the asymmetric extraction visible — without them, the arrangement collapses into the official framing of pure consumer protection. The classification prevents either mislabel by requiring both halves to be authored as structural data rather than argued as narrative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Is the operative structure of Article 3(2) best captured by this effects-jurisdiction reading, by the market-access reading (conditional market access, standard-setting rather than jurisdiction), or by the territorial-sovereignty reading (ultra vires assertion)?',
    'Crystallization of CJEU doctrine on the targeting/monitoring test and its limits, plus treaty-level dispute settlement over extraterritorial regulatory assertions; comparative treatment of parallel effects doctrines (antitrust, securities) as precedent.',
    'Under the territorial reading the same conduct migrates to the illegitimate-imposition side of the ledger and the payer seats compute as wronged parties rather than regulated subjects; under the market-access reading the payer seats re-characterize as tariff-bearers and the beneficiary set narrows to EU negotiators. Each resolution yields a different constraint, not a different measurement of this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which sibling reading of the Article 3(2) kernel captures the arrangement''s actual structure.').

omega_variable(
    unrepresented_payer_legitimacy,
    'Is binding substantial costs on actors who had no vote in the adopting legislature a legitimate exercise of protective jurisdiction, or a democratic-legitimacy defect that colors the extraction component?',
    'Comparative normative assessment against accepted effects doctrines in international practice and evolving comity norms; trace whether third-country accommodation over time constitutes tacit legitimation or coerced acquiescence.',
    'Resolution toward legitimacy defect pushes payer-seat classifications toward the pure-extraction end and strengthens the territorial sibling''s standing; resolution toward legitimate protection prices the extraction as the cost of rights enforcement and supports the coordination half of the claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unrepresented_payer_legitimacy, preference, 'Whether no-representation extraterritoriality is legitimate protection or structural overreach.').

omega_variable(
    compliance_cost_regressivity,
    'What share of total compliance cost falls on small foreign exporters versus large controllers, given that the obligations are largely fixed costs?',
    'Firm-level compliance-cost surveys stratified by revenue, and observed rates of EU-market withdrawal among small foreign services versus absorption.',
    'High regressivity entrenches the small_foreign_data_exporters seat''s trapped status and amplifies its effective extraction; a flat cost profile would weaken the victim declaration for that seat and pull the overall classification toward the coordination-dominated reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_cost_regressivity, empirical, 'Whether the fixed-cost structure makes the burden regressive across firm sizes.').

omega_variable(
    transfer_mechanism_stability,
    'Will the cross-border transfer machinery (standard contractual clauses, adequacy decisions, the EU-US Data Privacy Framework) survive ongoing legal challenge?',
    'Pending CJEU challenges to adequacy decisions and the new framework; EDPB guidance adoption rates; observed contract-volume shifts toward regional data localization.',
    'Collapse of the transfer mechanisms would leave foreign payers bearing full compliance cost without lawful transfer routes — spiking effective extraction on payer seats without reciprocal protection, and potentially flipping their computed classifications toward the pure-target end.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transfer_mechanism_stability, empirical, 'Stability of the enforcement chain''s transfer layer after Schrems II.').

omega_variable(
    brussels_effect_spillover_scope,
    'Has the arrangement''s operative function expanded to include de facto global standard-setting beyond the protection of people present in the EU, and if so, does that spillover count as a benefit the arrangement delivers?',
    'Comparative statutory analysis of third-country adoptions tracing causal lineage to the EU text, distinguishing imitation driven by firms'' single-standard economics from independent domestic processes.',
    'If spillover is load-bearing, the effective beneficiary set widens to include EU regulatory influence as such, lowering derived directionality for the institutional seats further and strengthening the coordination-function gate; if spillover is incidental, the arrangement''s justification stays confined to resident protection.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(brussels_effect_spillover_scope, conceptual, 'Whether global standard-setting spillover is part of the arrangement''s real function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__effects_jurisdiction_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_a3_effects_tr_t0, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(gdpr_a3_effects_tr_t0, observed).
narrative_ontology:measurement(gdpr_a3_effects_tr_t2, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2, 0.24).
narrative_ontology:measurement_basis(gdpr_a3_effects_tr_t2, observed).
narrative_ontology:measurement(gdpr_a3_effects_tr_t4, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 4, 0.34).
narrative_ontology:measurement_basis(gdpr_a3_effects_tr_t4, observed).
narrative_ontology:measurement(gdpr_a3_effects_tr_t6, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 6, 0.38).
narrative_ontology:measurement_basis(gdpr_a3_effects_tr_t6, observed).
narrative_ontology:measurement(gdpr_a3_effects_tr_t8, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 8, 0.33).
narrative_ontology:measurement_basis(gdpr_a3_effects_tr_t8, observed).
narrative_ontology:measurement(gdpr_a3_effects_tr_t10, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(gdpr_a3_effects_tr_t10, projected).
narrative_ontology:measurement(gdpr_a3_effects_tr_t12, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement_basis(gdpr_a3_effects_tr_t12, projected).

% Extraction over time
narrative_ontology:measurement(gdpr_a3_effects_be_t0, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(gdpr_a3_effects_be_t0, observed).
narrative_ontology:measurement(gdpr_a3_effects_be_t2, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2, 0.46).
narrative_ontology:measurement_basis(gdpr_a3_effects_be_t2, observed).
narrative_ontology:measurement(gdpr_a3_effects_be_t4, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement_basis(gdpr_a3_effects_be_t4, observed).
narrative_ontology:measurement(gdpr_a3_effects_be_t6, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 6, 0.57).
narrative_ontology:measurement_basis(gdpr_a3_effects_be_t6, observed).
narrative_ontology:measurement(gdpr_a3_effects_be_t8, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 8, 0.61).
narrative_ontology:measurement_basis(gdpr_a3_effects_be_t8, observed).
narrative_ontology:measurement(gdpr_a3_effects_be_t10, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 10, 0.64).
narrative_ontology:measurement_basis(gdpr_a3_effects_be_t10, projected).
narrative_ontology:measurement(gdpr_a3_effects_be_t12, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 12, 0.66).
narrative_ontology:measurement_basis(gdpr_a3_effects_be_t12, projected).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_a3_effects_su_t0, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(gdpr_a3_effects_su_t0, observed).
narrative_ontology:measurement(gdpr_a3_effects_su_t2, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2, 0.44).
narrative_ontology:measurement_basis(gdpr_a3_effects_su_t2, observed).
narrative_ontology:measurement(gdpr_a3_effects_su_t4, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 4, 0.53).
narrative_ontology:measurement_basis(gdpr_a3_effects_su_t4, observed).
narrative_ontology:measurement(gdpr_a3_effects_su_t6, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 6, 0.59).
narrative_ontology:measurement_basis(gdpr_a3_effects_su_t6, observed).
narrative_ontology:measurement(gdpr_a3_effects_su_t8, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement_basis(gdpr_a3_effects_su_t8, observed).
narrative_ontology:measurement(gdpr_a3_effects_su_t10, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(gdpr_a3_effects_su_t10, projected).
narrative_ontology:measurement(gdpr_a3_effects_su_t12, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement_basis(gdpr_a3_effects_su_t12, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__effects_jurisdiction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope__territorial_sovereignty_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope__market_access_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'GDPR extraterritoriality' decomposes into three structurally distinct claims about one text: effects-based jurisdiction (this story), conditional market access, and territorial overreach. Each carries its own epsilon, victim set, and legitimacy basis, so they are modeled as a constraint family linked through affects_constraints rather than merged into one observable-dependent story. The upstream/downstream structure runs from this reading (the CJEU-endorsed operative doctrine) outward: the market-access reading borrows this reading's enforcement facts while disputing their characterization, and the territorial reading defines itself against this reading's core premise.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
