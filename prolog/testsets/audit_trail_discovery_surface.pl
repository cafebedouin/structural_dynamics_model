% ============================================================================
% CONSTRAINT STORY: audit_trail_discovery_surface
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_audit_trail_discovery_surface, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: audit_trail_discovery_surface
 *   human_readable: Audit Trail Discovery Surface in Legal Technology
 *   domain: legal_technology/data_sovereignty/professional_services
 *
 * SUMMARY:
 *   Legal technology platforms marketed as privilege protection and
 *   compliance tools create immutable, attributed audit trails of attorney
 *   work product. Every research query, document review action, partner
 *   consultation, and draft iteration generates permanent timestamped records
 *   tied to individual attorneys. Vendors market this immutability as
 *   safeguarding attorney-client privilege and demonstrating diligence, but
 *   the same immutability creates a comprehensive discoverable record in
 *   malpractice litigation and fee disputes. The constraint exhibits genuine
 *   coordination functions—quality control, billing verification, regulatory
 *   compliance—layered with asymmetric extraction as plaintiff malpractice
 *   attorneys exploit granular attribution to reveal associate knowledge
 *   gaps, partner supervision failures, and research pattern deficiencies.
 *   The structural tension arises because the features that enable
 *   coordination (immutability, attribution, comprehensive logging) are
 *   identical to the features that expand liability surface. Associate
 *   attorneys are trapped by employment requirements and professional norms
 *   requiring system use. Law firm risk management is constrained by
 *   malpractice insurance requirements and client expectations but also
 *   benefits from demonstrable diligence. Plaintiff malpractice bar and legal
 *   tech vendors are primary beneficiaries with arbitrage exit options. The
 *   constraint's extractiveness has increased over the measurement interval
 *   as discovery practices have adapted to exploit the granular data, while
 *   theater ratio has risen as firms perform compliance rituals (partner
 *   review checkboxes, research documentation requirements) that generate
 *   discoverable records without necessarily improving work quality.
 *
 * KEY AGENTS:
 *   - Associate Attorney: Primary victim (powerless/trapped) — required to use firm systems, every action creates attributed permanent record, bears maximum career risk from discoverable research patterns
 *   - Law Firm Risk Management: Secondary victim and beneficiary (moderate/constrained) — constrained by insurance and client requirements, benefits from quality control and compliance demonstration, bears discovery surface expansion in disputes
 *   - Plaintiff Malpractice Bar: Primary beneficiary (institutional/arbitrage) — audit trails provide discovery evidence of knowledge gaps and supervision failures, can arbitrage across cases and firms
 *   - Legal Tech Vendor: Primary beneficiary (institutional/arbitrage) — captures revenue from both coordination narrative and liability surface creation, markets immutability as protection while it expands risk
 *   - Professional Liability Insurers: Mixed position (organized/mobile) — benefit from risk assessment transparency, bear increased claim costs, can influence policy terms
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees genuine coordination function with substantial and growing asymmetric extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(audit_trail_discovery_surface, 0.48).
domain_priors:suppression_score(audit_trail_discovery_surface, 0.52).
domain_priors:theater_ratio(audit_trail_discovery_surface, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(audit_trail_discovery_surface, extractiveness, 0.48).
narrative_ontology:constraint_metric(audit_trail_discovery_surface, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(audit_trail_discovery_surface, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(audit_trail_discovery_surface, tangled_rope).
narrative_ontology:human_readable(audit_trail_discovery_surface, "Audit Trail Discovery Surface in Legal Technology").
narrative_ontology:topic_domain(audit_trail_discovery_surface, "legal_technology/data_sovereignty/professional_services").

domain_priors:requires_active_enforcement(audit_trail_discovery_surface).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(audit_trail_discovery_surface, plaintiff_malpractice_bar).
narrative_ontology:constraint_beneficiary(audit_trail_discovery_surface, legal_tech_vendors).
narrative_ontology:constraint_victim(audit_trail_discovery_surface, law_firm_risk_management).
narrative_ontology:constraint_victim(audit_trail_discovery_surface, associate_attorneys).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ASSOCIATE ATTORNEY (SNARE) — Trapped by employment structure and professional norms requiring use of firm systems. Every research query, every draft iteration, every partner consultation request creates permanent attributed record. Cannot opt out without appearing uncooperative or technologically incompetent. Bears maximum career risk from discoverable research patterns showing gaps or inefficiencies.
constraint_indexing:constraint_classification(audit_trail_discovery_surface, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LAW FIRM RISK MANAGEMENT (TANGLED ROPE) — Constrained by malpractice insurance requirements and client expectations for audit trails, but also benefits from the coordination function: audit trails enable quality control, billing verification, and compliance demonstration. Significant extraction through expanded discovery surface in fee disputes and malpractice claims, but genuine coordination value in demonstrating diligence to clients and insurers.
constraint_indexing:constraint_classification(audit_trail_discovery_surface, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLAINTIFF MALPRACTICE BAR (ROPE) — Primary beneficiary. Immutable audit trails provide discovery gold mine: associate query patterns reveal knowledge gaps, partner review timestamps show supervision failures, research trails demonstrate missed issues. Experiences constraint as pure coordination: the legal tech infrastructure solves their evidence-gathering problem. Can arbitrage across multiple cases and firms using standardized discovery requests.
constraint_indexing:constraint_classification(audit_trail_discovery_surface, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LEGAL TECH VENDOR (ROPE) — Benefits from selling audit trail immutability as privilege protection feature while the same immutability creates malpractice liability surface. Market position strengthened by regulatory compliance requirements and client demand for audit capabilities. Can arbitrage between security/compliance narrative and actual discovery risk. Low extraction experienced because vendor captures revenue from both the coordination function and the liability surface it creates.
constraint_indexing:constraint_classification(audit_trail_discovery_surface, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PROFESSIONAL LIABILITY INSURERS (TANGLED ROPE) — Organized institutional actors who both benefit from audit trail transparency (enables risk assessment and premium pricing) and bear costs (increased claim frequency and settlement values when audit trails reveal gaps). Can influence policy terms and technology requirements but cannot exit the market. Mixed coordination-extraction: genuine underwriting value from transparency, but also subsidizes plaintiff bar's discovery advantage.
constraint_indexing:constraint_classification(audit_trail_discovery_surface, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational perspective, the constraint exhibits genuine coordination function (quality control, compliance, billing transparency) layered with asymmetric extraction (discovery surface expansion, associate surveillance, fee dispute weaponization). The immutability marketed as privilege protection creates permanent liability record. Coordination floor is real but extraction is substantial and growing as discovery practices adapt to exploit granular attribution.
constraint_indexing:constraint_classification(audit_trail_discovery_surface, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(audit_trail_discovery_surface_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(audit_trail_discovery_surface, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(audit_trail_discovery_surface, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(audit_trail_discovery_surface, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(audit_trail_discovery_surface_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The constraint exhibits genuine coordination value (quality control, billing verification, compliance demonstration) but substantial extraction through expanded discovery surface. The base extractiveness has risen from 0.32 to 0.48 over six years as plaintiff malpractice attorneys have developed sophisticated discovery strategies exploiting granular attribution. The coordination floor is real—firms do need audit capabilities for client service and regulatory compliance—but the extraction layer is significant and growing. The immutability marketed as privilege protection creates permanent liability records that would otherwise be ephemeral (draft research notes, preliminary analyses, internal consultations). Suppression (0.52): Moderate-high and rising. Associates cannot opt out without career penalty. Firms are constrained by malpractice insurance requirements mandating audit trail capabilities and client expectations for transparency. The suppression has increased from 0.42 to 0.52 as insurance underwriting has tightened requirements and as major clients have standardized audit trail expectations in outside counsel guidelines. Alternative systems without immutable attribution exist but face market barriers from insurance and client requirements. Theater ratio (0.38): Moderate and rising. Some performative compliance activity has emerged: partner review checkboxes that generate discoverable timestamps without substantive review, research documentation requirements that create records for compliance rather than quality improvement, mandatory consultation logs that formalize what were previously informal hallway conversations. The theater has increased from 0.25 to 0.38 as firms have layered compliance rituals onto the underlying coordination function. However, theater is not dominant—much of the audit trail activity serves genuine coordination purposes.
 *
 * PERSPECTIVAL GAP:
 *   The associate attorney experiences pure extraction (snare)—trapped by employment requirements, every action creates permanent attributed record usable against them in career evaluations or malpractice claims. Law firm risk management experiences mixed coordination and extraction (tangled rope)—constrained by insurance and client requirements, benefits from quality control capabilities, but bears expanded discovery surface in fee disputes and malpractice litigation. Plaintiff malpractice bar experiences pure coordination (rope)—audit trails solve their evidence-gathering problem, providing timestamped records of knowledge gaps and supervision failures. Legal tech vendors experience pure coordination (rope)—they benefit from selling the constraint as both privilege protection and compliance tool. Professional liability insurers experience mixed coordination and extraction (tangled rope)—transparency enables better risk assessment and pricing, but also increases claim frequency and settlement values. The analytical observer sees tangled rope at civilizational scope—genuine coordination function (quality control, billing verification, regulatory compliance) layered with substantial and growing asymmetric extraction (discovery surface expansion, associate surveillance, fee dispute weaponization). The perspectival gap reveals that the same immutability feature is simultaneously a coordination mechanism (for quality control and compliance), an extraction mechanism (for malpractice discovery), and a career surveillance system (for associates).
 *
 * DIRECTIONALITY LOGIC:
 *   Associate attorneys are victims with trapped exit options, yielding high d and high experienced extraction—they bear maximum career risk from permanent attributed records and cannot opt out. Law firm risk management is both victim and beneficiary with constrained exit, yielding moderate d—they face expanded discovery surface in disputes but also benefit from quality control and compliance demonstration capabilities. Plaintiff malpractice bar is primary beneficiary with arbitrage exit, yielding very low d and negative experienced extraction—audit trails solve their evidence-gathering problem and they can arbitrage discovery strategies across multiple cases. Legal tech vendors are beneficiaries with arbitrage exit, yielding very low d—they capture revenue from both the coordination narrative and the liability surface they create. Professional liability insurers are mixed beneficiaries and victims with mobile exit options, yielding moderate d—they benefit from underwriting transparency but bear increased claim costs. The analytical observer uses canonical analytical d, recognizing both genuine coordination function and substantial asymmetric extraction. No directionality overrides are needed—the structural derivation from beneficiary/victim declarations and exit options accurately captures each agent's relationship to the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that the same structural features (immutability, attribution, comprehensive logging) serve both genuine coordination functions and asymmetric extraction simultaneously. This is not a case of mislabeling coordination as extraction or vice versa—both are present and structurally inseparable given current technology design. The coordination function is real: firms need audit capabilities for quality control, billing verification, and regulatory compliance. The extraction is also real: immutable attributed records create permanent discoverable evidence of associate knowledge gaps, partner supervision failures, and research pattern deficiencies that would otherwise be ephemeral. The tangled rope classification captures this structural duality. The constraint could migrate toward rope if deletion capabilities were introduced that preserved coordination benefits while reducing discovery surface, or toward snare if coordination functions atrophied while extraction persisted. The omega variables identify the empirical questions that would determine whether current extraction levels are inherent to the coordination function or gratuitous additions that could be engineered away.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    privilege_protection_vs_discovery_surface,
    'Does the audit trail''s immutability actually protect attorney-client privilege, or does it primarily create discoverable work product that would otherwise be ephemeral?',
    'Empirical analysis of privilege assertion success rates in cases with vs without granular audit trails; comparison of discovery motion outcomes across technology platforms with different retention policies',
    'If privilege protection is genuine: coordination function dominates and lower extractiveness justified. If privilege protection is marketing narrative: extraction dominates and higher extractiveness warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(privilege_protection_vs_discovery_surface, empirical, 'Whether audit trail immutability protects privilege or expands discovery surface').

omega_variable(
    deletion_capability_threshold,
    'What level of selective deletion capability would preserve coordination benefits (quality control, billing verification) while reducing extraction (malpractice discovery surface)?',
    'Comparative analysis of legal tech platforms with different retention policies; correlation between deletion granularity and both quality control effectiveness and malpractice claim outcomes',
    'If narrow deletion windows sufficient: constraint could migrate toward rope with lower extraction. If broad deletion required: coordination function collapses and extraction persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deletion_capability_threshold, empirical, 'Deletion capability threshold balancing coordination and extraction').

omega_variable(
    attribution_granularity_necessity,
    'Is individual attorney attribution necessary for the coordination functions (quality control, billing), or would team-level or matter-level attribution suffice?',
    'Analysis of quality control and billing dispute resolution effectiveness at different attribution granularities; assessment of whether individual-level attribution provides marginal coordination value beyond team-level',
    'If individual attribution unnecessary: extraction mechanism (associate surveillance, career risk) is gratuitous and extractiveness should be higher. If individual attribution necessary: some extraction is inherent to coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attribution_granularity_necessity, empirical, 'Whether individual attribution is necessary for coordination functions').

omega_variable(
    malpractice_claim_frequency_causation,
    'Does granular audit trail availability increase malpractice claim frequency by revealing actionable gaps, or does it primarily shift claim outcomes without changing filing rates?',
    'Longitudinal analysis of malpractice claim frequency and outcomes before and after adoption of immutable audit trail systems; comparison across jurisdictions with different discovery rules for work product',
    'If claim frequency increases: audit trails are generating new liability rather than just evidencing existing negligence, suggesting higher extraction. If only outcomes shift: audit trails are coordination mechanism for legitimate claims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(malpractice_claim_frequency_causation, empirical, 'Whether audit trails increase claim frequency or only shift outcomes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(audit_trail_discovery_surface, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(audit_trail_tr_t0, audit_trail_discovery_surface, theater_ratio, 0, 0.25).
narrative_ontology:measurement(audit_trail_tr_t3, audit_trail_discovery_surface, theater_ratio, 3, 0.32).
narrative_ontology:measurement(audit_trail_tr_t6, audit_trail_discovery_surface, theater_ratio, 6, 0.38).

% Extraction over time
narrative_ontology:measurement(audit_trail_be_t0, audit_trail_discovery_surface, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(audit_trail_be_t3, audit_trail_discovery_surface, base_extractiveness, 3, 0.4).
narrative_ontology:measurement(audit_trail_be_t6, audit_trail_discovery_surface, base_extractiveness, 6, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(audit_trail_su_t0, audit_trail_discovery_surface, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(audit_trail_su_t3, audit_trail_discovery_surface, suppression_requirement, 3, 0.48).
narrative_ontology:measurement(audit_trail_su_t6, audit_trail_discovery_surface, suppression_requirement, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(audit_trail_discovery_surface, enforcement_mechanism).
narrative_ontology:affects_constraint(audit_trail_discovery_surface, privilege_architecture_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of privilege_architecture_coordination. The upstream constraint (rope classification) describes the coordination function of attorney-client privilege and work product doctrine. This constraint describes how legal technology implementation of privilege protection creates a distinct structural problem: the audit trail marketed as privilege safeguard becomes a malpractice discovery surface. The two constraints have different extractiveness values reflecting different structural dynamics—the upstream privilege coordination is low-extraction rope; the downstream audit trail implementation is moderate-extraction tangled rope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
