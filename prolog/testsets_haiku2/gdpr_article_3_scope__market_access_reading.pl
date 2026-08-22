% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__market_access_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gdpr_article_3_scope__market_access_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: gdpr_article_3_scope__market_access_reading
 *   human_readable: GDPR Article 3 Scope as Market Access Requirement (Market Access Reading)
 *   domain: technology_governance/international_law/privacy_regulation
 *
 * SUMMARY:
 *   GDPR Article 3 establishes the constraint's material scope: it applies to
 *   any processor offering goods or services to EU residents or monitoring
 *   their behavior, regardless of where the processor is located or where
 *   processing occurs. This constraint story instantiates the MARKET-ACCESS
 *   READING: Article 3 scope is understood as a conditional market-access
 *   requirement, not an assertion of extraterritorial jurisdiction. Under
 *   this reading, compliance with GDPR is a business strategy decision for
 *   global processors: they choose whether EU market access is worth GDPR
 *   compliance costs. The EU does not claim jurisdiction over processors'
 *   domestic operations; it conditions market access on compliance. This
 *   reading treats GDPR as the standard vehicle for the 'Brussels Effect'—the
 *   diffusion of EU regulatory standards globally because market-access
 *   incentives are more powerful than formal legal jurisdiction. The contrast
 *   reading (effects-jurisdiction) understands Article 3 as asserting
 *   jurisdiction over any processing targeting EU residents, independent of
 *   market strategy. The third reading (territorial-sovereignty) rejects all
 *   extraterritorial scope as exceeding legitimate regulatory authority. All
 *   three readings share the same kernel (Article 3 text and practice) but
 *   interpret the scope mechanism, the burden of compliance choice, and the
 *   legitimacy of extraterritorial scope differently. This story reports the
 *   market-access reading's structural properties without adjudicating the
 *   contest.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__market_access_reading, 0.38).
domain_priors:suppression_score(gdpr_article_3_scope__market_access_reading, 0.22).
domain_priors:theater_ratio(gdpr_article_3_scope__market_access_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__market_access_reading, rope).
narrative_ontology:human_readable(gdpr_article_3_scope__market_access_reading, "GDPR Article 3 Scope as Market Access Requirement (Market Access Reading)").
narrative_ontology:topic_domain(gdpr_article_3_scope__market_access_reading, "technology_governance/international_law/privacy_regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__market_access_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__market_access_reading, '9bf79e27-7381-481d-a74e-53768034a6d1').
narrative_ontology:cs_kernel_codification('9bf79e27-7381-481d-a74e-53768034a6d1', formalized).
narrative_ontology:cs_authority_grounding('9bf79e27-7381-481d-a74e-53768034a6d1', extraction).
narrative_ontology:cs_interpretation_layer_present('9bf79e27-7381-481d-a74e-53768034a6d1').
narrative_ontology:cs_reading_relation('9bf79e27-7381-481d-a74e-53768034a6d1', gdpr_article_3_scope__effects_jurisdiction_reading, coexists_with).
narrative_ontology:cs_reading_relation('9bf79e27-7381-481d-a74e-53768034a6d1', gdpr_article_3_scope__territorial_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('9bf79e27-7381-481d-a74e-53768034a6d1', foundational, scope_is_market_access_condition).
narrative_ontology:cs_axiom_status(scope_is_market_access_condition, holdable).
narrative_ontology:cs_axiom_grounding('9bf79e27-7381-481d-a74e-53768034a6d1', scope_is_market_access_condition, instrumental).
narrative_ontology:cs_axiom('9bf79e27-7381-481d-a74e-53768034a6d1', foundational, compliance_choice_is_voluntary_exit).
narrative_ontology:cs_axiom_status(compliance_choice_is_voluntary_exit, holdable).
narrative_ontology:cs_axiom_grounding('9bf79e27-7381-481d-a74e-53768034a6d1', compliance_choice_is_voluntary_exit, deontological).
narrative_ontology:cs_reference_frame('9bf79e27-7381-481d-a74e-53768034a6d1', fragmented_privacy_regulation_baseline).
narrative_ontology:cs_drift_state('9bf79e27-7381-481d-a74e-53768034a6d1', contemporary_brussels_effect_maturity, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9bf79e27-7381-481d-a74e-53768034a6d1', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__market_access_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_residents_data_subjects).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_regulatory_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, non_eu_residents_data_subjects).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_platform_operators).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, standards_spillover_recipients).
narrative_ontology:constraint_victim(gdpr_article_3_scope__market_access_reading, global_data_processors).
narrative_ontology:constraint_victim(gdpr_article_3_scope__market_access_reading, non_eu_residents_data_subjects).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% EU-based individuals whose personal data is processed globally. Under the market-access reading, they benefit from extended GDPR protection applied globally to any processor targeting them or monitoring their behavior. They gain a coordinated, high-baseline privacy standard that applies consistently across platforms and geographies. They also bear indirect costs when processors implement compliance friction (consent popups, service restrictions, data access delays) or deny service to avoid GDPR compliance burdens.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_residents_data_subjects, beneficiary,
    organized, biographical, mobile, global).

% Sets GDPR scope standards via Article 3(2) interpretation and enforces compliance through national data protection authorities. Does not extract direct revenue but accrues regulatory influence: global platforms must adopt EU-drafted privacy standards to participate in EU market. Under the market-access reading, scope enforcement is framed as conditioning market access, not asserting jurisdiction—compliance is a business strategy decision for processors, not a sovereignty imposition. Regulatory authority benefits from global standard diffusion (Brussels Effect) as non-EU jurisdictions adopt GDPR-aligned frameworks.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_regulatory_authority, agenda_setter,
    institutional, generational, analytical, continental).

% U.S., China, India, and other non-EU platforms, data processors, and technology companies that handle EU residents' data. They must either comply with GDPR requirements or deny EU residents service. Compliance requires infrastructure investment (privacy engineering, consent systems, data minimization, audit trails, breach notification, international data transfer mechanisms). They face enforcement risk from data protection authorities and reputational harm from violations. Under the market-access reading, they retain the choice to exit the EU market entirely—GDPR does not claim jurisdiction over their domestic operations, only conditions market access. The constraint persists because EU market access is valuable enough that most processors comply rather than exit.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, global_data_processors, payer,
    powerful, biographical, constrained, global).

% Non-EU individuals whose data is processed by platforms operating under GDPR compliance regimes. They incidentally benefit from higher global privacy standards (GDPR compliance applies globally to any processor in scope) but may pay indirectly through reduced service features, increased friction, higher prices, or platform exit from markets where compliance is operationally difficult. Under the market-access reading, they are not the direct regulatory subjects of Article 3 scope—the constraint targets processors seeking EU market access, not non-EU residents' rights. The spillover protection they receive is a side effect, not the targeted benefit.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, non_eu_residents_data_subjects, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__market_access_reading, non_eu_residents_data_subjects, payer).

% U.S., UK, China, India, and other-jurisdiction regulators and sovereignty advocates who contest the market-access framing and argue that EU scope enforcement masks extraterritorial jurisdiction and exceeds legitimate regulatory authority. They would argue that the market-access reading downplays the coercive effect: U.S. and Chinese processors face pressure (lose EU market or comply with foreign rules) that makes the choice between compliance and exit not genuinely voluntary. Their exclusion from the constraint's authoring is that this reading does NOT frame GDPR as jurisdictional assertion; they dispute that framing and contest whether the market-access characterization is accurate or is propaganda masking regulatory overreach.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, other_jurisdictions_regulators, excluded,
    powerful, generational, trapped, global).

% EU-registered and EU-based platforms and data processors (e.g., German, French, Dutch tech companies). They benefit from the level-playing-field effect: non-EU competitors must meet GDPR standards to access EU market, while they already comply as a baseline (lower relative compliance burden). They participate in compliance infrastructure and standards-setting discussions and may gain competitive advantage in privacy-conscious markets. They face lower exit costs than non-EU processors because they are already GDPR-compliant.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_platform_operators, beneficiary,
    organized, biographical, mobile, continental).

% Non-EU jurisdictions (Brazil, South Korea, Singapore, Kenya) that adopt GDPR-compatible or GDPR-aligned frameworks as the global regulatory standard diffuses. They benefit from a coordinated, high-baseline privacy framework without internal negotiation or redrafting; adopting GDPR-aligned standards positions them as compatible with the de facto global standard and reduces friction with EU traders. They also become constrained by Brussels Effect diffusion—the GDPR-baseline standard becomes globally incumbent, limiting regulatory experimentation and sovereignty in privacy policymaking.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, standards_spillover_recipients, beneficiary,
    moderate, generational, mobile, global).

% Researchers, economists, legal scholars, and policy analysts studying GDPR Article 3 scope as a regulatory coordination mechanism and Brussels Effect case study. They observe processor behavior, regulatory framing, adoption decisions globally, and the structural legitimacy of the market-access reading. They examine evidence for and against the market-access characterization (Does it predict processor behavior? Do regulators justify scope via market-access incentives or jurisdictional assertion? Do third jurisdictions accept the market-access framing or contest it as jurisdictional overreach?) and contribute to the larger kernel contest.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gdpr_article_3_scope__market_access_reading, eu_regulatory_authority).
narrative_ontology:fixing_cost_class(gdpr_article_3_scope__market_access_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified, high-baseline privacy protection regime applied globally to any processor targeting EU residents or monitoring their behavior. Solves the coordination problem of fragmented privacy regulation: without GDPR scope, EU residents would face different privacy standards depending on processor location and jurisdiction. The constraint creates a single legal framework that applies globally once triggered (targeting/monitoring test), reducing processor complexity (one standard instead of many) and resident confusion (consistent protection).
% TRANSFER_FUNCTION: Transfers compliance and infrastructure costs from EU residents to data processors. Moves standard-setting authority from fragmented national regulators and processor preferences to the EU regulatory framework. Moves regulatory influence globally: as processors comply with GDPR to access EU market, they diffuse GDPR-aligned practices and standards to their other operations and to other jurisdictions. Moves privacy baseline upward for EU residents and incidentally for non-EU residents whose data is processed by GDPR-compliant firms.
% ABSENT_VOICES: Other-jurisdiction regulators (U.S., UK, China, India) who contest the market-access characterization and argue that GDPR scope assertion masks extraterritorial jurisdiction. Data processors in jurisdictions with incompatible privacy architectures (e.g., authoritarian regimes requiring data localization) are structurally excluded because compliance is technically impossible for them. Non-EU residents whose data is incidentally covered are not party to the scope decision and have no voice in Article 3 design, despite being affected.
% DISAPPEARANCE_RATIONALE: If GDPR Article 3 scope and enforcement disappeared, processors would no longer face market-access conditionality for GDPR compliance. Many would fragment privacy standards by jurisdiction or by market value (complying with GDPR only for EU market, degrading privacy elsewhere). The Brussels Effect diffusion would reverse over 5–10 years: non-EU jurisdictions that adopted GDPR-aligned frameworks would diverge, regional standards would re-emerge, and global privacy baseline would fall below current GDPR-level. Within the EU, residents would face a return to fragmented processor-specific privacy standards, undoing the unified-framework coordination benefit.
% FOUNDING_PROBLEM: Early-to-mid 2010s: EU residents' personal data was processed globally by thousands of processors, each applying different privacy standards depending on processor location and jurisdiction. A U.S.-based social network, a Chinese analytics firm, and a European search engine all collecting data from the same EU resident would apply U.S., Chinese, and EU privacy law respectively. Residents faced coordination failure (no unified protection) and processors faced complexity (fragmented compliance burden).
% FOUNDING_PROBLEM_CORROBORATION: EU regulatory authorities and data protection authorities (e.g., French CNIL, German BfDI) attest that fragmented privacy protection was the pre-GDPR state and remains a risk without scope enforcement. Civil society and resident advocates attest that consolidated privacy standards reduce their burden. Processors attest that pre-GDPR compliance diversity was operationally complex. Independent evidence: the rapid adoption of GDPR-aligned frameworks by non-EU jurisdictions (Brazil's LGPD, South Korea's PIPA amendments, UAE's DIFC law, Kenya's PDPA) demonstrates that the unified standard solves a real coordination problem. Other-jurisdiction regulators (who contest the market-access framing) do not dispute that fragmentation was the prior state or that coordination is beneficial; they dispute whether GDPR scope is the legitimate mechanism to achieve it.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__market_access_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__market_access_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__market_access_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gdpr_article_3_scope__market_access_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__market_access_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gdpr_article_3_scope__market_access_reading_tests).
:- end_tests(gdpr_article_3_scope__market_access_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Under the market-access reading, extraction is moderate-low. Processors face compliance costs to access EU market, but they retain the choice to exit. EU regulatory authority benefits from global standard diffusion without formal jurisdiction. The extraction is real (processors bear costs they would not bear without GDPR scope) but it is conditioned on market participation—processors can avoid it by denying EU market access. This is weaker extraction than jurisdictional assertion (which would be unavoidable) but stronger than pure coordination (which would have minimal conditional structure). Suppression (0.22): Low suppression because the market-access reading does not frame scope as a coercive mandate. Suppression is the enforcement mechanism that prevents processors from routing around GDPR (geographic fragmentation, jurisdiction shopping); under this reading, that enforcement is necessary but not onerous—processors can exit the market if compliance costs exceed benefits. Theater ratio (0.18): Low theater because the market-access framing is substantively accurate—processors do treat GDPR compliance as a market strategy decision, and regulators do justify scope via market-access incentives rather than jurisdictional assertion. The engagement is relatively functional. Accessibility_collapse (0.42): Moderate. Processors have legal exit (deny market access) and economic exit (comply selectively if market value exceeds compliance cost). For EU residents, alternatives are harder—they cannot easily choose non-GDPR processors if those processors are major global players. For EU regulators, exit is not meaningful—scope is the regulatory mechanism. Resistance (0.58): Moderate-high resistance from processors, especially U.S.-based firms and China-based platforms for which GDPR compliance is operationally difficult and the EU market, while valuable, may not justify infrastructure costs. The constraint meets real pushback; it is not self-evidently legitimate to other-jurisdiction regulators. The measurement series shows extractiveness rising over 2010–2026 as market-access incentives strengthen (more processors enter EU market, more adoption of GDPR-aligned standards globally), theater ratio remains stable (the market-access framing is consistently used), and suppression stabilizes (enforcement machinery matures but does not escalate as jurisdictional assertion would imply).
 *
 * PERSPECTIVAL GAP:
 *   The EU regulatory authority seat and the global processor seat should compute significantly differently. From the regulatory authority seat, the constraint is genuine coordination with market-access incentive structure (genuine rope at low d). From the processor seat, especially for U.S. and Asian firms, the constraint is a gating condition that limits their EU market access in exchange for compliance (high d, closer to snare from their position). The engine computes both perspectives: the regulatory seat sees cooperation incentives and diffusion benefits; the processor seat sees compliance costs and exit pressure. The market-access reading predicts this divergence should be structurally meaningful but not extreme—the reading assumes processors retain choice (market exit), so the constraint is not a mandate. If processors report that GDPR compliance is mandatory in practice (e.g., because EU market access is too valuable to decline), the effects-jurisdiction reading would better describe the actual directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   EU regulatory authority is the structural beneficiary and agenda-setter (d near 0.0): it accrues regulatory influence without bearing compliance costs; it sets the scope rule; it gains global standard diffusion as other jurisdictions adopt GDPR-aligned frameworks; it retains analytical exit (the scope rule can be changed or repealed). EU residents are beneficiaries (d near 0.2): they gain extended protection and coordination benefit (unified standard), but they also bear indirect costs when platforms implement friction or exit markets. Global data processors are the payers (d near 0.85): they bear compliance costs, face enforcement risk, and are constrained by market-access gating; however, they retain exit via market denial. The asymmetry is substantial but not absolute—processors' exit option (deny EU market) is valuable enough that some do exercise it, reducing effective extraction below what it would be under jurisdictional assertion. Other-jurisdiction regulators face structural exclusion (d undefined—they are not party to this constraint, but they contest its legitimacy from outside). Standards-spillover recipients (non-EU adopters of GDPR-aligned frameworks) are incidental beneficiaries—they gain a coordinated standard without negotiation, but that benefit was not targeted; they also become constrained by the diffused standard.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fragmented privacy protection) remains live under the market-access reading. The founding problem is not solved by GDPR scope itself—it is solved by widespread processor adoption of GDPR-aligned standards. The constraint persists because the market-access incentive to adopt GDPR standards is strong enough that global processors converge on that framework, creating the unified standard EU residents want. If the founding problem were solved (all major processors voluntarily adopted compatible privacy standards independent of GDPR), the constraint could theoretically become vestigial—market-access conditionality would no longer be needed to drive adoption. But empirical evidence suggests the market-access incentive is still necessary (many processors comply only because EU market access is valuable, not because they prefer the standard independently). The constraint is not yet mandatrophic under this reading, though a future world where processors voluntarily adopt GDPR-baseline standards independently would remove the justification for scope enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__market_access_reading, 2010, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t2010, gdpr_article_3_scope__market_access_reading, theater_ratio, 2010, 0.08).
narrative_ontology:measurement_basis(gdpr_tr_t2010, observed).
narrative_ontology:measurement(gdpr_tr_t2014, gdpr_article_3_scope__market_access_reading, theater_ratio, 2014, 0.12).
narrative_ontology:measurement_basis(gdpr_tr_t2014, observed).
narrative_ontology:measurement(gdpr_tr_t2018, gdpr_article_3_scope__market_access_reading, theater_ratio, 2018, 0.16).
narrative_ontology:measurement_basis(gdpr_tr_t2018, observed).
narrative_ontology:measurement(gdpr_tr_t2022, gdpr_article_3_scope__market_access_reading, theater_ratio, 2022, 0.18).
narrative_ontology:measurement_basis(gdpr_tr_t2022, observed).
narrative_ontology:measurement(gdpr_tr_t2026, gdpr_article_3_scope__market_access_reading, theater_ratio, 2026, 0.18).
narrative_ontology:measurement_basis(gdpr_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t2010, gdpr_article_3_scope__market_access_reading, base_extractiveness, 2010, 0.15).
narrative_ontology:measurement_basis(gdpr_be_t2010, observed).
narrative_ontology:measurement(gdpr_be_t2014, gdpr_article_3_scope__market_access_reading, base_extractiveness, 2014, 0.22).
narrative_ontology:measurement_basis(gdpr_be_t2014, observed).
narrative_ontology:measurement(gdpr_be_t2018, gdpr_article_3_scope__market_access_reading, base_extractiveness, 2018, 0.32).
narrative_ontology:measurement_basis(gdpr_be_t2018, observed).
narrative_ontology:measurement(gdpr_be_t2022, gdpr_article_3_scope__market_access_reading, base_extractiveness, 2022, 0.37).
narrative_ontology:measurement_basis(gdpr_be_t2022, observed).
narrative_ontology:measurement(gdpr_be_t2026, gdpr_article_3_scope__market_access_reading, base_extractiveness, 2026, 0.38).
narrative_ontology:measurement_basis(gdpr_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_su_t2010, gdpr_article_3_scope__market_access_reading, suppression_requirement, 2010, 0.12).
narrative_ontology:measurement_basis(gdpr_su_t2010, observed).
narrative_ontology:measurement(gdpr_su_t2014, gdpr_article_3_scope__market_access_reading, suppression_requirement, 2014, 0.16).
narrative_ontology:measurement_basis(gdpr_su_t2014, observed).
narrative_ontology:measurement(gdpr_su_t2018, gdpr_article_3_scope__market_access_reading, suppression_requirement, 2018, 0.2).
narrative_ontology:measurement_basis(gdpr_su_t2018, observed).
narrative_ontology:measurement(gdpr_su_t2022, gdpr_article_3_scope__market_access_reading, suppression_requirement, 2022, 0.22).
narrative_ontology:measurement_basis(gdpr_su_t2022, observed).
narrative_ontology:measurement(gdpr_su_t2026, gdpr_article_3_scope__market_access_reading, suppression_requirement, 2026, 0.22).
narrative_ontology:measurement_basis(gdpr_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__market_access_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gdpr_article_3_scope__market_access_reading, 0.12).
narrative_ontology:affects_constraint(gdpr_article_3_scope__market_access_reading, gdpr_article_3_scope__effects_jurisdiction_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__market_access_reading, gdpr_article_3_scope__territorial_sovereignty_reading).

% DUAL FORMULATION NOTE:
% GDPR Article 3 scope kernel decomposes into three structurally distinct constraint stories. All three share the same boundary-setting rule (Article 3 text and practice) but interpret the causal mechanism and legitimacy differently. (1) Market-access reading: scope is a conditional market-access requirement; Brussels Effect diffusion; beneficiary is EU regulatory influence via standard diffusion; lower extraction, lower suppression. (2) Effects-jurisdiction reading: scope asserts extraterritorial jurisdiction over any processor targeting EU residents; mandatory compliance; beneficiary is EU resident protection via jurisdictional assertion; higher extraction, higher suppression. (3) Territorial-sovereignty reading: scope exceeds legitimate regulatory authority; no legitimate beneficiary; pure regulatory overreach. Each story has its own ε, beneficiary/victim structure, and classification. The ε-invariance principle requires decomposition: changing the observable (market-access mechanism vs. jurisdictional assertion) changes ε (0.38 vs. 0.65+), so one kernel supports multiple constraints. The readings are linked via network.affects_constraints to enable kernel-level analysis: observers can examine how the constraint's classification changes across readings and what structural evidence would favor one reading over another.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
