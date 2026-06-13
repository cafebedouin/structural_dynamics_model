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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gdpr_article_3_scope__market_access_reading
 *   human_readable: GDPR Article 3 Scope: Market Access Reading
 *   domain: legal/regulatory/technology
 *
 * SUMMARY:
 *   Article 3(2) of the GDPR extends the regulation's scope to processing
 *   activities of non-EU controllers/processors if they offer goods or
 *   services to EU residents or monitor their behavior. This reading frames
 *   Article 3 scope as a market-access requirement: firms can operate in any
 *   jurisdiction and under any local data law EXCEPT when seeking to serve EU
 *   residents, at which point they must meet GDPR standards as a condition of
 *   market access. The EU is not asserting extraterritorial jurisdiction in
 *   the traditional sense (claiming authority to regulate behavior in foreign
 *   sovereign territory); instead, it is conditioning access to the EU market
 *   on compliance. This is economically coercive but structurally different
 *   from territorial jurisdiction. The constraint benefits EU residents by
 *   standardizing privacy protections and benefits the EU regulatory
 *   apparatus by diffusing EU norms globally without direct enforcement; it
 *   imposes compliance costs on multinational platforms. The reading is
 *   CONTESTED — the effects_jurisdiction_reading reinterprets Article 3 as
 *   genuine extraterritorial jurisdiction, and the
 *   territorial_sovereignty_reading denies legitimacy to any form of
 *   extraterritorial application.
 *
 * KEY AGENTS:
 *   - EU regulatory apparatus (agenda-setter, institutional power): establishes and monitors Article 3 scope as market-access conditioning; does not claim territorial jurisdiction but exercises market-control authority.
 *   - Multinational tech platforms (payer, powerful): absorb compliance costs to access EU market; experience the constraint as a voluntary market-entry condition, not coercion, but would exit EU operations if market value did not justify compliance cost.
 *   - EU resident data subjects (beneficiary, organized): receive standardized privacy rights as a market-standard condition; their exit is constrained by network effects but the constraint is experienced as protective rather than extractive.
 *   - Data protection authorities (agenda-setter + beneficiary, institutional): administers compliance monitoring and gains organizational legitimacy and soft power from GDPR enforcement.
 *   - Third-country regulators (excluded, institutional): affected by market-access conditioning but not party to the arrangement; would argue the frame obscures coercive effects.
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
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__market_access_reading, rope).
narrative_ontology:human_readable(gdpr_article_3_scope__market_access_reading, "GDPR Article 3 Scope: Market Access Reading").
narrative_ontology:topic_domain(gdpr_article_3_scope__market_access_reading, "legal/regulatory/technology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__market_access_reading, '877ac83c-074f-4625-a7d5-a2d8e288be26').
narrative_ontology:cs_kernel_codification('877ac83c-074f-4625-a7d5-a2d8e288be26', fixed_text).
narrative_ontology:cs_authority_grounding('877ac83c-074f-4625-a7d5-a2d8e288be26', lineage).
narrative_ontology:cs_interpretation_layer_present('877ac83c-074f-4625-a7d5-a2d8e288be26').
narrative_ontology:cs_reading_relation('877ac83c-074f-4625-a7d5-a2d8e288be26', gdpr_article_3_scope__effects_jurisdiction_reading, coexists_with).
narrative_ontology:cs_reading_relation('877ac83c-074f-4625-a7d5-a2d8e288be26', gdpr_article_3_scope__territorial_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('877ac83c-074f-4625-a7d5-a2d8e288be26', foundational, market_access_as_legitimate_regulatory_mechanism).
narrative_ontology:cs_axiom_status(market_access_as_legitimate_regulatory_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('877ac83c-074f-4625-a7d5-a2d8e288be26', market_access_as_legitimate_regulatory_mechanism, conventional).
narrative_ontology:cs_axiom('877ac83c-074f-4625-a7d5-a2d8e288be26', foundational, brussels_effect_soft_power_not_coercion).
narrative_ontology:cs_axiom_status(brussels_effect_soft_power_not_coercion, holdable).
narrative_ontology:cs_axiom_grounding('877ac83c-074f-4625-a7d5-a2d8e288be26', brussels_effect_soft_power_not_coercion, conventional).
narrative_ontology:cs_reference_frame('877ac83c-074f-4625-a7d5-a2d8e288be26', market_conditional_authority).
narrative_ontology:cs_drift_state('877ac83c-074f-4625-a7d5-a2d8e288be26', post_schrems_ii_enforcement_escalation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('877ac83c-074f-4625-a7d5-a2d8e288be26', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__market_access_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_regulatory_influence).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_resident_data_subjects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, multinational_tech_platforms).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, data_protection_authorities).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, privacy_advocates).
narrative_ontology:constraint_victim(gdpr_article_3_scope__market_access_reading, multinational_tech_platforms).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__market_access_reading, brussels_effect_standard_diffusion).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__market_access_reading, privacy_as_market_condition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Established Article 3 scope as conditioning market access to EU residents on compliance with EU data protection standards. Does not assert extraterritorial jurisdiction over foreign sovereigns; instead anchors compliance obligation to the bilateral economic relationship (access to EU market as consideration). Administers compliance monitoring and can withdraw market access for violations. Views GDPR extraterritoriality as standard diffusion, not coercion — a coordination mechanism rather than jurisdictional assertion.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_regulatory_apparatus, agenda_setter,
    institutional, generational, analytical, continental).

% Pay compliance costs (data governance infrastructure, legal review, consent management, breach notification procedures) to access EU market. Their exit options are constrained: they can exit EU operations entirely, but doing so means abandoning a primary revenue pool (450M residents, high digital spending). Compliance is modeled as a market-access transaction cost, not a coercive imposition on their sovereign operations. Many platforms have adopted GDPR-standard processing worldwide, treating compliance as a standardized offering cost rather than a jurisdiction-specific burden.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, multinational_tech_platforms, payer,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__market_access_reading, multinational_tech_platforms, beneficiary).

% Receive substantive rights (access, portability, deletion, objection) and transparency requirements as a standard condition of digital platform use. Their exit is constrained by network effects and digital necessity; most major platforms operate in EU, so they have some choice between platforms but cannot avoid the platforms themselves without economic and social isolation. GDPR conditions market access for service providers, which benefits subjects as a standardized regulatory baseline rather than a jurisdiction-specific protection.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_resident_data_subjects, beneficiary,
    organized, biographical, constrained, regional).

% Are not parties to the Article 3 arrangement but are affected by it: their firms' access to EU market is conditioned on EU compliance, which can displace local data governance frameworks if local norms are weaker. They would argue for sovereignty recognition and local-law primacy but have no seat in the EU legislative process and can only negotiate bilateral adequacy or binding corporate rules arrangements. Their regulatory authority is indirectly constrained by the market-access conditioning, but they characterize this as a Brussels Effect side effect, not jurisdictional overreach by the EU.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, third_country_regulators, excluded,
    institutional, generational, trapped, national).

% Collectively administer Article 3 compliance monitoring (investigation, enforcement, coordination through EDPB). Receive organizational authority and legitimacy from GDPR's existence; their institutional interest is in sustaining the framework's application. View Article 3 as a mechanism for extending EU privacy norms through market mechanisms rather than force — firms comply because market access is valuable, not because EU coercion reaches their territory.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, data_protection_authorities, agenda_setter,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__market_access_reading, data_protection_authorities, beneficiary).

% Support GDPR scope extension as a practical enforcement mechanism for privacy rights globally. Under this reading, they see the Brussels Effect as a positive diffusion of privacy protection through market incentives, not as jurisdictional overreach. They advocate for similar privacy frameworks in other jurisdictions and cite GDPR compliance as a market standard that non-EU firms have voluntarily adopted.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, privacy_advocates, beneficiary,
    moderate, biographical, mobile, global).

% Contest the market-access reading by arguing that Article 3(2) constitutes genuine effects-based jurisdiction, not merely market conditioning. They would insist this reading obscures the jurisdictional assertion underneath the market language. Their objection is structural: the EU is claiming authority to regulate behavior affecting EU residents globally, which is extraterritorial jurisdiction regardless of the economic mechanism.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, legal_scholars_effects_jurisdiction, excluded,
    analytical, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a collective-action problem for EU residents: platform operators need a unified, standardized data governance framework instead of negotiating piecemeal privacy protections with every jurisdiction; residents gain transparency and substantive rights as a standard market condition instead of depending on voluntary platform policies. The coordination is EU-residents + platform-operators on data governance; the vehicle is conditional market access, not coerced extraterritorial authority.
% TRANSFER_FUNCTION: Moves compliance cost from platform operators to their data governance infrastructure budgets, funded by revenue from EU market access. The transfer is not from operators to authorities; authorities do not directly collect from operators. Instead, operators pay compliance infrastructure cost, data subjects receive standardized privacy rights as a market-standard condition, and EU regulatory apparatus gains influence over global data governance standards via market diffusion (the Brussels Effect).
% ABSENT_VOICES: Third-country regulators and firms operating under non-GDPR frameworks are excluded from the European legislative process but affected by market-access conditioning. They would argue for sovereignty recognition and the right to establish competing data governance frameworks; their objection is that conditioning market access on compliance creates indirect extraterritorial coercion. They are kept out by the EU's ability to set the terms of market access to its territory.
% DISAPPEARANCE_RATIONALE: If Article 3 scope and market-access conditioning disappeared, multinational platforms would diverge their data governance by region, EU residents would lose the standardized privacy baseline currently embedded in market access, and the Brussels Effect diffusion of privacy norms would reverse or weaken significantly. Compliance costs would be distributed differently (platforms might offer lower-privacy tiers in low-regulation jurisdictions), and the EU's soft-power influence over global data governance would diminish.
% FOUNDING_PROBLEM: Digital platform operators' data governance practices before GDPR were not uniform: firms offered different privacy standards in different markets based on local regulation and competitive pressure. EU residents had no standardized, enforceable baseline; firms collected data with minimal transparency and limited subject rights. The founding problem: ensuring that firms serving EU markets maintain substantive privacy protections as a condition of market access, rather than treating privacy as negotiable based on local competitive dynamics.
% FOUNDING_PROBLEM_CORROBORATION: The EU regulatory apparatus attests the founding problem persists (firms would revert to lower privacy standards absent GDPR enforcement). Multinational platforms operating under GDPR frame compliance as a standardized cost of doing business, implicitly confirming that market access conditioning sustains privacy norms. Privacy advocates and data protection authorities corroborate that the founding problem (uneven privacy standards in digital markets) remains active. Third-country regulators dispute whether GDPR scope extension is the legitimate solution, but do not dispute that privacy governance fragmentation is a real problem.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__market_access_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__market_access_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__market_access_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(gdpr_article_3_scope__market_access_reading, 'none', 1).

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
 *   Extractiveness is moderate (0.38 at interval end, rising from 0.28) because compliance costs are real and platforms cannot serve EU residents without absorbing them. However, under the market-access reading, this is framed as a voluntary market-entry cost, not extraction. The constraint does not require suppression: firms comply because EU market access is valuable, not because the EU enforces punishment across borders. Suppression remains low (0.22) throughout the interval because the mechanism is market conditional, not coercive. Theater is very low (0.18): the privacy protection function is genuine; enforcement is administrative review and market-access denial, not theatrical performance. Accessibility collapse is moderate (0.65): alternatives exist (serve only non-EU markets, or accept lower revenue from EU restrictions), but network effects and EU market size make exit costly. Resistance is moderate (0.42): platforms protest GDPR scope extension, but many have integrated GDPR standards globally, suggesting they do not experience it as purely coercive. The measurement series shows modest upward drift in extractiveness (from 0.28 to 0.38 over 25 units) as compliance infrastructure matured and firms internalized compliance as a business cost; theater stays low, suppression stays low. This pattern is consistent with a coordination arrangement that has settled into stable market practice.
 *
 * PERSPECTIVAL GAP:
 *   The EU regulatory apparatus experiences Article 3 as legitimate standard-setting and soft-power diffusion of privacy norms (d near beneficiary end for this reading). Multinational platforms experience it as a market-access cost they accept because EU market value justifies it (d near symmetric/slightly payer-end). EU residents experience it as protective standardization (d beneficiary end). Third-country regulators experience it as indirect extraterritorial coercion disguised as market conditioning (d target/payer end). The effects_jurisdiction_reading would compute higher extractiveness and higher suppression because it treats compliance obligation as enforced by regulatory reach, not market choice. The territorial_sovereignty_reading would deny any legitimate basis for Article 3 scope. The engine computes per-seat types from power/exit/beneficiary data; this reading specifies the stakeholder structure that produces rope-type results.
 *
 * DIRECTIONALITY LOGIC:
 *   EU regulatory apparatus: d near 0.0 (full beneficiary) — sets the rules, gains soft power, does not comply with anyone else's standards. Multinational platforms: d near 0.35–0.40 (slightly payer-end to symmetric) — absorb compliance costs but benefit from unified market rules that eliminate per-jurisdiction variance. EU data subjects: d near 0.15–0.20 (slight beneficiary) — receive standardized rights but also face global-platform standardization of their data governance. Data protection authorities: d near 0.05 (beneficiary) — gain institutional authority and relevance from GDPR enforcement. Third-country regulators: trapped, but d not authored because they are excluded. This directionality profile reflects the market-access reading: beneficiaries are those who benefit from unified standards (EU residents, privacy advocates, EU apparatus), payers are those who absorb compliance cost (platforms), but the payer cost is framed as voluntary market-entry cost, not extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (ensuring privacy baselines in digital markets) remains live (t=25); firms are actively complying and the EU is actively enforcing. The constraint has NOT resolved the problem but has sustained it at a new equilibrium: instead of privacy baselines degrading per market, they are now unified at the EU level and diffused globally via market incentives. This is not mandatrophy — the founding purpose is still being served. However, there is a secondary trajectory visible in the measurements: extractiveness and theater both drift upward modestly, suggesting the constraint may be accumulating secondary extraction as compliance infrastructure becomes more elaborate and enforcement activity includes boundary-testing cases (e.g., Schrems II, TikTok investigations) that extend GDPR reach. The market-access reading prevents mandatrophy classification because the constraint is still solving its founding problem; the effects_jurisdiction_reading might highlight mandatrophy risk if extraterritorial application extends beyond market conditioning into regulatory reach claims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_access_vs_jurisdiction_framing,
    'Is Article 3 scope legitimately framed as conditional market access (a bilateral economic arrangement) or does the practical effect constitute extraterritorial jurisdiction (unilateral regulatory authority over non-EU conduct)?',
    'Examine whether firms can sustain materially different data practices in non-EU operations serving only non-EU residents without facing market-access consequences. Test whether the EU enforcement apparatus treats Article 3 as a jurisdiction claim or a market condition. Analyze whether third-country governments recognize the arrangement as market conditioning or jurisdictional overreach.',
    'If the frame is genuinely market conditioning, the constraint operates as rope (mutual coordination benefit via standardized platform governance). If the practical effect is jurisdiction, the constraint operates as tangled_rope or snare (the EU unilaterally extends regulatory reach via market power, and firms comply not from voluntary market participation but from inability to exit EU markets). This omega determines whether the reading''s core claim holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_access_vs_jurisdiction_framing, conceptual, 'Whether Article 3 is market conditioning or disguised jurisdiction.').

omega_variable(
    brussels_effect_voluntariness,
    'Do multinational platforms adopt GDPR-standard processing globally because it is genuinely efficient and cost-effective (true coordination benefit), or because EU market access is so valuable that the cost is passed to their global operations (extracted from global users via EU-resident subsidy)?',
    'Analyze platform cost structures: do firms offer lower-privacy tiers in non-GDPR jurisdictions where they operate? If so, the global adoption of GDPR standards suggests efficiency, not forced diffusion. If firms apply GDPR standards globally even where not required, examine whether the global standards are profit-maximizing or a side effect of compliance infrastructure already built for the EU market. Compare transaction costs for platforms with and without EU operations.',
    'If global GDPR adoption is cost-efficient coordination, the rope claim is stronger (mutual benefit, low extractiveness). If firms are essentially subsidizing EU privacy standards by applying them globally and unable to offer competitive lower-privacy alternatives, the constraint''s extractiveness is higher than authored (0.38) and the rope claim weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brussels_effect_voluntariness, empirical, 'Whether Brussels Effect diffusion reflects genuine efficiency or market-power-driven cost distribution.').

omega_variable(
    kernel_reading_frame_dependence,
    'Does the market-access reading of Article 3 depend on accepting the EU''s own characterization of its authority as regulatory standard-setting rather than jurisdictional assertion?',
    'Examine how the reading changes if we adopt the perspective of third-country regulators or firms: do they experience Article 3 as market conditioning or as the EU asserting regulatory authority over them? Test the stability of the reading across different institutional perspectives. Analyze whether the frame is observer-independent (a property of the constraint itself) or frame-dependent (true only from the EU''s perspective or the perspective of willing market participants).',
    'If the market-access reading is frame-dependent, the constraint is not a rope but a contested institutional arrangement where different parties experience it as different types. The engine computes per-seat types; this omega documents the reading-level contest. The sibling reading (effects_jurisdiction_reading) may be equally structurally valid from a different perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_frame_dependence, conceptual, 'Whether the market-access frame is universal or reader-dependent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__market_access_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_a3_ma_tr_t0, gdpr_article_3_scope__market_access_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(gdpr_a3_ma_tr_t0, observed).
narrative_ontology:measurement(gdpr_a3_ma_tr_t5, gdpr_article_3_scope__market_access_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement_basis(gdpr_a3_ma_tr_t5, observed).
narrative_ontology:measurement(gdpr_a3_ma_tr_t10, gdpr_article_3_scope__market_access_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement_basis(gdpr_a3_ma_tr_t10, observed).
narrative_ontology:measurement(gdpr_a3_ma_tr_t15, gdpr_article_3_scope__market_access_reading, theater_ratio, 15, 0.17).
narrative_ontology:measurement_basis(gdpr_a3_ma_tr_t15, observed).
narrative_ontology:measurement(gdpr_a3_ma_tr_t20, gdpr_article_3_scope__market_access_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(gdpr_a3_ma_tr_t20, observed).
narrative_ontology:measurement(gdpr_a3_ma_tr_t25, gdpr_article_3_scope__market_access_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement_basis(gdpr_a3_ma_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(gdpr_a3_ma_be_t0, gdpr_article_3_scope__market_access_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(gdpr_a3_ma_be_t0, observed).
narrative_ontology:measurement(gdpr_a3_ma_be_t5, gdpr_article_3_scope__market_access_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement_basis(gdpr_a3_ma_be_t5, observed).
narrative_ontology:measurement(gdpr_a3_ma_be_t10, gdpr_article_3_scope__market_access_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement_basis(gdpr_a3_ma_be_t10, observed).
narrative_ontology:measurement(gdpr_a3_ma_be_t15, gdpr_article_3_scope__market_access_reading, base_extractiveness, 15, 0.37).
narrative_ontology:measurement_basis(gdpr_a3_ma_be_t15, observed).
narrative_ontology:measurement(gdpr_a3_ma_be_t20, gdpr_article_3_scope__market_access_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement_basis(gdpr_a3_ma_be_t20, observed).
narrative_ontology:measurement(gdpr_a3_ma_be_t25, gdpr_article_3_scope__market_access_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement_basis(gdpr_a3_ma_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_a3_ma_su_t0, gdpr_article_3_scope__market_access_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(gdpr_a3_ma_su_t0, observed).
narrative_ontology:measurement(gdpr_a3_ma_su_t5, gdpr_article_3_scope__market_access_reading, suppression_requirement, 5, 0.19).
narrative_ontology:measurement_basis(gdpr_a3_ma_su_t5, observed).
narrative_ontology:measurement(gdpr_a3_ma_su_t10, gdpr_article_3_scope__market_access_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement_basis(gdpr_a3_ma_su_t10, observed).
narrative_ontology:measurement(gdpr_a3_ma_su_t15, gdpr_article_3_scope__market_access_reading, suppression_requirement, 15, 0.21).
narrative_ontology:measurement_basis(gdpr_a3_ma_su_t15, observed).
narrative_ontology:measurement(gdpr_a3_ma_su_t20, gdpr_article_3_scope__market_access_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement_basis(gdpr_a3_ma_su_t20, observed).
narrative_ontology:measurement(gdpr_a3_ma_su_t25, gdpr_article_3_scope__market_access_reading, suppression_requirement, 25, 0.22).
narrative_ontology:measurement_basis(gdpr_a3_ma_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__market_access_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gdpr_article_3_scope__market_access_reading, 0.12).
narrative_ontology:affects_constraint(gdpr_article_3_scope__market_access_reading, gdpr_article_3_scope__effects_jurisdiction_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__market_access_reading, gdpr_article_3_scope__territorial_sovereignty_reading).

% DUAL FORMULATION NOTE:
% The GDPR Article 3 scope kernel decomposes into three structurally distinct readings: market_access_reading (this file) frames compliance as conditional market access with low extraction; effects_jurisdiction_reading interprets Article 3 as extraterritorial jurisdiction with moderate-to-high extraction; territorial_sovereignty_reading denies legitimate basis for extraterritorial application. Each reading has different ε, beneficiary/victim structure, and computed type. They are not three observations of the same constraint; they are three competing claims about what Article 3 means and how authority flows from it. The network edges link them as a constraint family: any reading's validity depends on the kernel's interpretation, and shifts in how the kernel is read propagate across all three readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
