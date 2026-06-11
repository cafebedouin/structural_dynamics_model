% ============================================================================
% CONSTRAINT STORY: medical_debt_collection_escalation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_medical_debt_collection_escalation, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: medical_debt_collection_escalation
 *   human_readable: Medical Debt Collection Escalation: Pipeline Hardening into Wage Garnishment and Liens
 *   domain: healthcare_finance/debt_collection
 *
 * SUMMARY:
 *   A hospital system's debt-collection pipeline has progressively hardened
 *   from billing-with-negotiation into automated wage garnishment and
 *   property liens. The escalation is not uniform across organizational
 *   levels or time. Individual patient stakes inflate fastest — within 5
 *   years of billing system implementation, wage garnishment becomes standard
 *   for all unpaid balances over $5,000. Hospital organizational machinery
 *   (collection divisions, debt sales, legal case processing) hardens more
 *   slowly — standardized lien procedures take 7-10 years to become policy.
 *   Structural machinery (state garnishment law changes, court system
 *   adaptation, debt purchasing markets) lags furthest behind but eventually
 *   normalizes the practice as routine. This level-differential is the
 *   story's key signal: individual agents experience the snare's tightening
 *   immediately (wages garnished within months of missed payment), while the
 *   institutional machinery that makes garnishment routine takes years to
 *   solidify. The grid captures this asynchrony: individual stakes inflation
 *   (0.52 → 0.89 over 10 years) outpaces organizational stakes inflation
 *   (0.48 → 0.64) and structural stakes inflation (0.38 → 0.52). Similarly,
 *   individual-level suppression intensifies fastest (0.62 → 0.88), driven by
 *   wage garnishment and legal machinery aimed at individuals. Organizational
 *   suppression (hospital policy, collection procedures) rises slower (0.44 →
 *   0.61). Resistance decays at all levels as the machinery hardens:
 *   individual resistance (0.28 → 0.15) collapses as patients lose wage
 *   negotiation capacity; organizational resistance (small hospitals
 *   objecting to collection escalation) weakens (0.32 → 0.22) as debt sales
 *   become standard practice; class resistance (patient coalitions) remains
 *   weak (0.48 → 0.38) but shows more stability than individual or
 *   organizational resistance because collective organizing preserves
 *   potential agency. Structural resistance (state attorneys general,
 *   legislative scrutiny) is highest (0.62 → 0.54 — slight decline) because
 *   system-level actors can mount credible challenges. The architecture of
 *   coercion is hierarchical: the constraint squeezes hardest where agents
 *   are least organized (individuals) and most gently where agents have
 *   institutional resources (states, legislatures). The hospital system that
 *   initiates the escalation may believe it is coordinating solvency; the
 *   collection agencies pursuing the escalation experience it as pure
 *   efficiency; the patients experiencing wage seizure experience it as
 *   snare. All are correct from their perspective. The engine's
 *   classification reveals that the snare-level extraction overwhelms any
 *   coordination narrative.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(medical_debt_collection_escalation, 0.78).
domain_priors:suppression_score(medical_debt_collection_escalation, 0.82).
domain_priors:theater_ratio(medical_debt_collection_escalation, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(medical_debt_collection_escalation, extractiveness, 0.78).
narrative_ontology:constraint_metric(medical_debt_collection_escalation, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(medical_debt_collection_escalation, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(medical_debt_collection_escalation, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(medical_debt_collection_escalation, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(medical_debt_collection_escalation, snare).
narrative_ontology:human_readable(medical_debt_collection_escalation, "Medical Debt Collection Escalation: Pipeline Hardening into Wage Garnishment and Liens").
narrative_ontology:topic_domain(medical_debt_collection_escalation, "healthcare_finance/debt_collection").

domain_priors:requires_active_enforcement(medical_debt_collection_escalation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(medical_debt_collection_escalation, hospital_debt_collection_division).
narrative_ontology:constraint_beneficiary(medical_debt_collection_escalation, debt_collection_agencies).
narrative_ontology:constraint_beneficiary(medical_debt_collection_escalation, debt_purchasers).
narrative_ontology:constraint_victim(medical_debt_collection_escalation, medically_bankrupted_patients).
narrative_ontology:constraint_victim(medical_debt_collection_escalation, low_income_households).
narrative_ontology:constraint_victim(medical_debt_collection_escalation, working_uninsured).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(medical_debt_collection_escalation, small_community_hospitals).
narrative_ontology:constraint_beneficiary(medical_debt_collection_escalation, hospital_finance_leadership).
narrative_ontology:constraint_beneficiary(medical_debt_collection_escalation, insurance_companies).
narrative_ontology:constraint_vindicates(medical_debt_collection_escalation, medical_necessity_justifies_cost_recovery).
narrative_ontology:constraint_vindicates(medical_debt_collection_escalation, patient_responsibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Received necessary medical treatment during acute health crisis (surgery, emergency care, chronic illness management). Unable to pay full bills. Face wage garnishment (up to 25% of wages in many states), property liens (hospitals file liens on homes), and bankruptcy if total debt exceeds income recovery potential. No negotiation mechanism available; collection is automated. Medical debt follows patient across job changes and state lines. No alternative to the hospital system for emergency care. The constraint captures the patient's future earnings through wage garnishment, creating permanent partial insolvency.
narrative_ontology:constraint_stakeholder(medical_debt_collection_escalation, medically_bankrupted_patients, payer,
    powerless, biographical, trapped, national).

% Employed but uninsured due to employer benefit gaps or inability to afford premiums. Seek care only when acutely ill, incurring large bills. Internally committed to 'responsible person' identity — believe they should pay what they owe, experience shame about debt, do not discuss financial situation with peers. Structurally could change jobs, relocate, or seek legal debt relief, but identity-lock prevents recognizing these options. The debt is experienced as legitimate personal failure rather than structural entrapment. The constraint's suppression operates partially through the agent's own identity frame.
narrative_ontology:constraint_stakeholder(medical_debt_collection_escalation, working_uninsured, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(medical_debt_collection_escalation, working_uninsured, excluded).

% Operate critical regional healthcare infrastructure; must sustain operations through patient revenue collection. Face declining insurance reimbursement (Medicare/Medicaid rates below cost, commercial insurance shifting costs to patients), rising operational costs (labor, supplies, equipment). Outsource collection to specialized debt collection agencies to avoid direct moral responsibility while maintaining revenue. The hospital's finance leadership experiences collection escalation as operational necessity, not coercion. The constraint enables the hospital to defer difficult financial decisions (accepting losses on uncompensable care, renegotiating cost structure) by extracting payment from vulnerable patients instead.
narrative_ontology:constraint_stakeholder(medical_debt_collection_escalation, small_community_hospitals, agenda_setter,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(medical_debt_collection_escalation, small_community_hospitals, beneficiary).

% Centralized department or contracted agency that processes patient billing, pursues collection through all available legal mechanisms (liens, garnishment, lawsuits), and sells unrecoverable debt to debt purchasers. Operates with high efficiency through automation: bills are generated, sent, escalated through legal procedures with minimal individual review. The collection division's revenue or cost-recovery targets are independent of patient outcomes; success is measured in dollars collected, not patients served. Experiences constraint as pure coordination: connecting patient debt to legal enforcement machinery efficiently.
narrative_ontology:constraint_stakeholder(medical_debt_collection_escalation, hospital_debt_collection_division, agenda_setter,
    institutional, immediate, arbitrage, global).

% Acquire medical debt portfolios from hospitals at 10-30% face value, pursue collection through aggressive legal and communication strategies (wage garnishment, property liens, repeated contact, credit report damage). Profit from price gap between acquisition cost and recovery. Operate with high leverage: small enforcement cost per case (form generation, court filing fees) relative to large recovery if patient has any collectable assets or wages. Completely decoupled from hospital operations or patient wellbeing; incentive structure rewards maximum extraction regardless of patient hardship.
narrative_ontology:constraint_stakeholder(medical_debt_collection_escalation, debt_collection_agencies, beneficiary,
    institutional, immediate, arbitrage, global).

% Financial firms that purchase debt portfolios from collection agencies at further discounts, hold or resell. Purely financial players with no connection to healthcare delivery or patient care. Profit from secondary market inefficiency (debt purchased at 5-10% face value, pursued aggressively or resold at higher price). The constraint enables financial extraction completely divorced from any coordination function.
narrative_ontology:constraint_stakeholder(medical_debt_collection_escalation, debt_purchasers, beneficiary,
    institutional, immediate, arbitrage, global).

% Senior leadership responsible for hospital financial solvency. Face pressure from declining insurance reimbursement, rising debt service costs, board expectations for operational margin maintenance. Implement collection escalation as operational strategy: shift from billing department (patient-facing, negotiation-capable) to debt collection agency outsourcing (coercive, automated, scaled). Experience the constraint as legitimate response to structural financial crisis in healthcare, not as extraction. The constraint enables financial decisions that defer difficult system-level reforms (cost reduction, business model change, advocacy for public insurance expansion) by temporarily solving the budget problem through patient extraction.
narrative_ontology:constraint_stakeholder(medical_debt_collection_escalation, hospital_finance_leadership, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(medical_debt_collection_escalation, hospital_finance_leadership, beneficiary).

% Provide legal machinery (court filings, judgment issuance, lien recording, garnishment orders) for medical debt collection. In many jurisdictions, medical debt collection cases are the highest-volume civil docket; judges routinely approve collection motions with minimal adversarial review (default judgment when patient does not appear). Court system capacity is stretched; routine approval of collection cases is treated as administrative processing rather than disputed claims. The constraint operates through institutional routine rather than contested legal principle.
narrative_ontology:constraint_stakeholder(medical_debt_collection_escalation, state_courts_and_magistrates, agenda_setter,
    organized, generational, constrained, regional).

% Hospital employees who generate bills, process payments, attempt initial patient communication about debt. Often experience moral friction between job requirements (pursue collection) and patient harm visibility (direct contact with patients in financial distress). Constrained by employment and automation systems that escalate cases automatically after payment deadline passes. May wish to negotiate or forgive debt but lack authority; cases escalate beyond their decision-making scope within 30-60 days.
narrative_ontology:constraint_stakeholder(medical_debt_collection_escalation, hospital_billing_staff, agenda_setter,
    moderate, biographical, constrained, local).

% Benefit from cost-shifting to patients through high deductibles, narrow networks, preauthorization barriers. Do not directly engage with collection but design policies that leave patients uninsured or underinsured, triggering medical debt. Completely decoupled from patient outcomes or debt consequences; financial incentive is to minimize claims paid. The constraint enables insurance business model optimization at patient expense.
narrative_ontology:constraint_stakeholder(medical_debt_collection_escalation, insurance_companies, beneficiary,
    institutional, immediate, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(medical_debt_collection_escalation, insurance_companies, excluded).

% Non-agent entity: the aggregate class of individuals without health insurance. This class has no collective voice, organization, or exit options. Systematically targeted by medical debt collection because individual members cannot negotiate insurance coverage. Listed here for narrative completeness rather than as an agent that collects rents or experiences constraint directly.
narrative_ontology:constraint_stakeholder(medical_debt_collection_escalation, uninsured_population_class, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_non_agent(medical_debt_collection_escalation, uninsured_population_class).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(medical_debt_collection_escalation, hospital_debt_collection_division).
narrative_ontology:fixing_cost_class(medical_debt_collection_escalation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Hospitals need to collect revenue to sustain operations and provide care to future patients (genuine coordination problem: unpaid medical debt threatens operational solvency). The original coordination mechanism was billing-as-service-recovery with built-in negotiation and financial assistance pathways. The actual operation has become automated wage garnishment and property liens with minimal negotiation.
% TRANSFER_FUNCTION: The constraint moves wealth (wages through garnishment, property through liens, future earnings through debt claims) from medically bankrupted and uninsured patients to hospitals, debt collection agencies, and debt purchasers. The transferred value originates in patient wages and property, flows through legal machinery (court systems, garnishment rules, lien statutes), and accumulates in beneficiary organizations.
% ABSENT_VOICES: Patients who died or suffered permanent disability from deferring care due to medical debt fear (not present at any discussion table). Uninsured patients who avoid hospitals entirely due to collection fear (not present). Healthcare workers (nurses, doctors, social workers) who experience moral injury from being part of extraction machinery (largely absent from institutional decision-making). Community health advocates and public health authorities (excluded from hospital finance decisions).
% DISAPPEARANCE_RATIONALE: If medical debt collection escalation disappeared overnight, the entire financial model of hospital operations would need restructuring. Hospitals currently depend on collection revenue to meet debt service and operational budgets. Disappearance would force either (a) acceptance of uncompensable care losses (shifting burden to charitable funding, tax funding, or private donation), (b) dramatic cost reduction, or (c) business model change toward community health focus rather than revenue maximization. Healthcare financing would fundamentally reorganize. The constraint is not a natural law but an institutional choice embedded in financial obligation and reimbursement structure.
% FOUNDING_PROBLEM: How do hospitals sustain operations when insurance reimbursement rates are below cost and patients cannot afford care? The original answer was cost-plus billing with negotiated payment and financial assistance. As reimbursement pressure intensified (Medicare/Medicaid rates declining, commercial insurance shifting costs) and hospital debt service increased, the 'solution' shifted from negotiation to coercion.
% FOUNDING_PROBLEM_CORROBORATION: Hospital finance leadership: operational necessity framing (pressure from reimbursement decline, debt service, budget targets). Debt collection agencies: market efficiency framing (high recovery rates justify automation). Patient advocates: structural entrapment framing (patients face medical necessity, no insurance, no wage mobility). State attorneys general: predatory practice framing (collection escalation targets vulnerable populations, violates consumer protection principles). Economic researchers: contradictory evidence on whether collection escalation materially improves hospital financial health or merely transfers cost to patients. NO CORROBORATION from outside beneficiary set that collection escalation is truly necessary (hospital finance officers state necessity, but independent financial analysis is sparse).
narrative_ontology:disappearance_verdict(medical_debt_collection_escalation, world_rearranges).
narrative_ontology:founding_problem_status(medical_debt_collection_escalation, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MEDICALLY BANKRUPTED PATIENT (SNARE) — Trapped by combined medical necessity (cannot refuse treatment), wage dependency (income required for survival), and institutional coercion (garnishment, liens). No exit options. Full extraction realized. The debt follows the patient into bankruptcy, wage seizure, property lien — the constraint's coercive machinery operates at maximum intensity against this agent.
constraint_indexing:constraint_classification(medical_debt_collection_escalation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WORKING UNINSURED / IDENTITY-LOCKED (SNARE) — Structurally mobile (could theoretically change jobs, move, seek debt relief) but identity-locked into the role of 'responsible provider' — cannot exit the employment market or the identity commitment to paying debts. The bind is cognitive (internalized obligation, shame about medical debt, belief that the debt is legitimately owed) layered over material barriers (no savings, no legal paths to discharge medical debt except bankruptcy). The classification is snare: identity lock does not change the predatory structure, it only prevents the agent from recognizing their own mobility.
constraint_indexing:constraint_classification(medical_debt_collection_escalation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: SMALL COMMUNITY HOSPITAL (TANGLED ROPE) — Faces genuine coordination problem: must collect payment to sustain operations and repay debt service. But has shifted from billing cooperation (negotiate, forgive where appropriate) to aggressive collection (automated liens, wage garnishment). The small hospital benefits from centralized debt collection services (outsourced collection reduces administrative overhead) while its vulnerable patients bear the full cost. The constraint coordinates hospital solvency while extracting from those least able to pay. Moderate agent power constrained by dependence on collection machinery.
constraint_indexing:constraint_classification(medical_debt_collection_escalation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: DEBT COLLECTION INDUSTRY (ROPE) — Experiences the constraint as pure coordination: acquiring medical debt portfolios, automating collection via liens and garnishment, scaling enforcement. No experienced extraction — instead, net benefit from the pipeline's increasing efficiency. High agency and arbitrage (can shift between hospital clients, markets, regulatory jurisdictions). The constraint is coordination from their seat: how to efficiently collect diffuse patient debt at scale. Theater ratio for this agent is low (direct coercive mechanisms require minimal pretense).
constraint_indexing:constraint_classification(medical_debt_collection_escalation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: HOSPITAL FINANCE LEADERSHIP (TANGLED ROPE) — Coordinating financial solvency (genuine problem: hospitals must sustain operations in a fee-for-service system) while implementing escalating extraction against vulnerable patients. The leadership experiences the constraint as a necessary tightening — patient revenue is declining, insurance reimbursement is shrinking, operational costs are rising. The 'solution' (aggressive debt collection) is coordination for one goal (institutional survival) that extracts from patients (asymmetric burden). Active enforcement required to maintain the pipeline; stakes inflation appears as operational necessity to hospital finance.
constraint_indexing:constraint_classification(medical_debt_collection_escalation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a broad analytical position, the medical debt collection escalation is a snare: patients face medical necessity (cannot refuse treatment), no alternatives to the hospital system, escalating coercive machinery (liens, garnishment) with minimal resistance capacity. The coordination story (hospitals need to collect to sustain operations) is genuine but overwhelmed by the extraction intensity. The constraint shows snare characteristics at maximum scale: necessity entrapment + institutional coercion + suppression of alternatives + high theater ratio (billing as 'medical necessity recovery' rather than debt slavery).
constraint_indexing:constraint_classification(medical_debt_collection_escalation, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(medical_debt_collection_escalation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(medical_debt_collection_escalation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(medical_debt_collection_escalation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(medical_debt_collection_escalation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(medical_debt_collection_escalation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medical_necessity_entrapment,
    'Is the patient''s inability to refuse treatment a legitimate basis for collection escalation, or a coercive mechanism that exploits the absence of choice?',
    'Comparative analysis: do hospitals escalate collection against patients with effective alternatives (wealthy patients with lawyers, insured patients with strong coverage) at the same rate as uninsured/underinsured patients? If escalation correlates with power asymmetry, the trap is coercive exploitation rather than coordination.',
    'If coercive exploitation: reclassify definitively as snare at all perspectives. If legitimate coordination challenge: some perspectives (hospital finance) may have genuine rope characteristics. The data will distinguish.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(medical_necessity_entrapment, empirical, 'Whether medical necessity is leveraged as coercive mechanism').

omega_variable(
    identity_lock_mechanism_scope,
    'How many patients experience the debt through identity_locked framing (internalized obligation to pay, shame, belief in legitimacy) versus trapped framing (pure material coercion)?',
    'Post-debt survey or interview data from bankrupted patients: ask whether exit was possible (structurally mobile) but unthinkable (identity fused), or genuinely impossible (trapped). Behavioral data: do patients with identity lock exit the constraint after identity disruption (bankruptcy filing, counseling, peer support groups) or remain locked?',
    'If identity lock predominates: the constraint''s suppression is partly internalized, reducing measured institutional force but increasing post-exit persistence. If trapped predominates: structural barriers alone drive the snare. The mechanism changes clinical/policy intervention design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_scope, empirical, 'Proportion of patients experiencing identity-locked versus trapped binding').

omega_variable(
    collection_escalation_causality,
    'Does hospital financial pressure drive collection escalation, or does collection escalation become institutionalized independently of genuine financial necessity?',
    'Historical analysis comparing hospitals with different financial pressures (well-funded academic centers vs financially stressed community hospitals). Do all escalate collection at the same rate, suggesting institutional adoption rather than necessity? Or do financially pressured hospitals escalate faster?',
    'If institutional adoption dominates: the constraint becomes a piton (performative escalation maintained by inertia). If financial necessity dominates: tangled_rope classification confirmed for hospital finance leadership. The data will show whether ''operational necessity'' is genuine or cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collection_escalation_causality, empirical, 'Whether collection escalation is driven by financial necessity or institutional inertia').

omega_variable(
    wage_garnishment_enforcement_capacity,
    'How much enforcement capacity (court infrastructure, collection agency coordination, payroll integration) is required to maintain the wage garnishment pipeline, and where is the scaling limit?',
    'Cost accounting: enforcement machinery cost per dollar collected; comparison across states with different garnishment rules (some states cap garnishment, others allow deeper collection). Identify the break-even point where enforcement cost exceeds recovered debt.',
    'If enforcement reaches cost break-even before full extraction: constraint is unsustainable and will degrade (piton trajectory). If extraction exceeds enforcement cost indefinitely: institutional machinery will harden further. Understanding the economics reveals the true terminal state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_garnishment_enforcement_capacity, empirical, 'Cost economics of wage garnishment enforcement pipeline').

omega_variable(
    patient_coalition_possibility,
    'Under what conditions could medically bankrupted patients achieve organized power sufficient to resist collection escalation?',
    'Case studies of patient collective action: medical debt strikes, bankruptcy courts filing patterns, state attorney general actions, legislative outcomes. Identify what scale of organization generates pressure on hospital systems and collection agencies.',
    'If coalition organizing is possible at scale: powerless agents could move toward organized status, reducing experienced snare intensity. If coalition organizing is structurally impossible (patients too dispersed, energy depleted by medical crises, shame isolation): snare persists. The data identifies the structural barriers to collective action.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(patient_coalition_possibility, empirical, 'Feasibility of patient collective action against medical debt collection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(medical_debt_collection_escalation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(meddebt_tr_t0, medical_debt_collection_escalation, theater_ratio, 0, 0.68).
narrative_ontology:measurement_basis(meddebt_tr_t0, observed).
narrative_ontology:measurement(meddebt_tr_t5, medical_debt_collection_escalation, theater_ratio, 5, 0.52).
narrative_ontology:measurement_basis(meddebt_tr_t5, observed).
narrative_ontology:measurement(meddebt_tr_t10, medical_debt_collection_escalation, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(meddebt_tr_t10, observed).

% Extraction over time
narrative_ontology:measurement(meddebt_be_t0, medical_debt_collection_escalation, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(meddebt_be_t0, observed).
narrative_ontology:measurement(meddebt_be_t5, medical_debt_collection_escalation, base_extractiveness, 5, 0.61).
narrative_ontology:measurement_basis(meddebt_be_t5, observed).
narrative_ontology:measurement(meddebt_be_t10, medical_debt_collection_escalation, base_extractiveness, 10, 0.78).
narrative_ontology:measurement_basis(meddebt_be_t10, observed).

% Suppression requirement over time
narrative_ontology:measurement(meddebt_su_t0, medical_debt_collection_escalation, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(meddebt_su_t0, observed).
narrative_ontology:measurement(meddebt_su_t5, medical_debt_collection_escalation, suppression_requirement, 5, 0.71).
narrative_ontology:measurement_basis(meddebt_su_t5, observed).
narrative_ontology:measurement(meddebt_su_t10, medical_debt_collection_escalation, suppression_requirement, 10, 0.82).
narrative_ontology:measurement_basis(meddebt_su_t10, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=10
narrative_ontology:measurement(meddebt_grid_01, medical_debt_collection_escalation, accessibility_collapse(class), 0, 0.48).
narrative_ontology:measurement(meddebt_grid_02, medical_debt_collection_escalation, accessibility_collapse(class), 10, 0.71).
narrative_ontology:measurement(meddebt_grid_03, medical_debt_collection_escalation, accessibility_collapse(individual), 0, 0.58).
narrative_ontology:measurement(meddebt_grid_04, medical_debt_collection_escalation, accessibility_collapse(individual), 10, 0.84).
narrative_ontology:measurement(meddebt_grid_05, medical_debt_collection_escalation, accessibility_collapse(organizational), 0, 0.42).
narrative_ontology:measurement(meddebt_grid_06, medical_debt_collection_escalation, accessibility_collapse(organizational), 10, 0.68).
narrative_ontology:measurement(meddebt_grid_07, medical_debt_collection_escalation, accessibility_collapse(structural), 0, 0.35).
narrative_ontology:measurement(meddebt_grid_08, medical_debt_collection_escalation, accessibility_collapse(structural), 10, 0.61).
narrative_ontology:measurement(meddebt_grid_09, medical_debt_collection_escalation, resistance(class), 0, 0.48).
narrative_ontology:measurement(meddebt_grid_10, medical_debt_collection_escalation, resistance(class), 10, 0.38).
narrative_ontology:measurement(meddebt_grid_11, medical_debt_collection_escalation, resistance(individual), 0, 0.28).
narrative_ontology:measurement(meddebt_grid_12, medical_debt_collection_escalation, resistance(individual), 10, 0.15).
narrative_ontology:measurement(meddebt_grid_13, medical_debt_collection_escalation, resistance(organizational), 0, 0.32).
narrative_ontology:measurement(meddebt_grid_14, medical_debt_collection_escalation, resistance(organizational), 10, 0.22).
narrative_ontology:measurement(meddebt_grid_15, medical_debt_collection_escalation, resistance(structural), 0, 0.62).
narrative_ontology:measurement(meddebt_grid_16, medical_debt_collection_escalation, resistance(structural), 10, 0.54).
narrative_ontology:measurement(meddebt_grid_17, medical_debt_collection_escalation, stakes_inflation(class), 0, 0.44).
narrative_ontology:measurement(meddebt_grid_18, medical_debt_collection_escalation, stakes_inflation(class), 10, 0.58).
narrative_ontology:measurement(meddebt_grid_19, medical_debt_collection_escalation, stakes_inflation(individual), 0, 0.52).
narrative_ontology:measurement(meddebt_grid_20, medical_debt_collection_escalation, stakes_inflation(individual), 10, 0.89).
narrative_ontology:measurement(meddebt_grid_21, medical_debt_collection_escalation, stakes_inflation(organizational), 0, 0.48).
narrative_ontology:measurement(meddebt_grid_22, medical_debt_collection_escalation, stakes_inflation(organizational), 10, 0.64).
narrative_ontology:measurement(meddebt_grid_23, medical_debt_collection_escalation, stakes_inflation(structural), 0, 0.38).
narrative_ontology:measurement(meddebt_grid_24, medical_debt_collection_escalation, stakes_inflation(structural), 10, 0.52).
narrative_ontology:measurement(meddebt_grid_25, medical_debt_collection_escalation, suppression(class), 0, 0.52).
narrative_ontology:measurement(meddebt_grid_26, medical_debt_collection_escalation, suppression(class), 10, 0.68).
narrative_ontology:measurement(meddebt_grid_27, medical_debt_collection_escalation, suppression(individual), 0, 0.62).
narrative_ontology:measurement(meddebt_grid_28, medical_debt_collection_escalation, suppression(individual), 10, 0.88).
narrative_ontology:measurement(meddebt_grid_29, medical_debt_collection_escalation, suppression(organizational), 0, 0.44).
narrative_ontology:measurement(meddebt_grid_30, medical_debt_collection_escalation, suppression(organizational), 10, 0.61).
narrative_ontology:measurement(meddebt_grid_31, medical_debt_collection_escalation, suppression(structural), 0, 0.48).
narrative_ontology:measurement(meddebt_grid_32, medical_debt_collection_escalation, suppression(structural), 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(medical_debt_collection_escalation, resource_allocation).
narrative_ontology:boltzmann_floor_override(medical_debt_collection_escalation, 0.22).
narrative_ontology:affects_constraint(medical_debt_collection_escalation, healthcare_insurance_coverage_gap).
narrative_ontology:affects_constraint(medical_debt_collection_escalation, hospital_debt_restructuring).
narrative_ontology:affects_constraint(medical_debt_collection_escalation, bankruptcy_discharge_doctrine_medical_debt).

% DUAL FORMULATION NOTE:
% Medical debt collection escalation decomposes into three distinct constraints with different ε values: (1) Healthcare access (whether patients can obtain treatment without debt threat) — ε low (coordination-heavy, genuine medical necessity driving access). (2) Collection pipeline (whether payment is negotiated vs coercive) — ε high (extraction mechanism, the focus of this story). (3) Debt persistence (whether medical debt survives bankruptcy discharge) — ε high (legal constraint enabling the snare). This story is constraint family member 2 (collection mechanism). Members 1 and 3 are upstream and downstream.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(medical_debt_collection_escalation, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
