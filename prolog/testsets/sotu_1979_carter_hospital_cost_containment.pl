% ============================================================================
% CONSTRAINT STORY: sotu_1979_carter_hospital_cost_containment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1979_carter_hospital_cost_containment, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sotu_1979_carter_hospital_cost_containment
 *   human_readable: Federal Hospital Cost Containment Regulation (1979 Carter Proposal)
 *   domain: healthcare/economic_policy
 *
 * SUMMARY:
 *   Carter's 1979 hospital cost containment proposal frames spiraling
 *   healthcare costs as an inflationary threat requiring federal price
 *   regulation. The constraint operates through mandatory spending caps on
 *   hospital revenues, coupled with utilization review to reduce unnecessary
 *   procedures. The mechanism creates an asymmetric distribution: federal
 *   government, taxpayers, and insured patients benefit from $60 billion in
 *   projected savings; hospital providers and healthcare workers bear the
 *   costs through constrained revenues, reduced staffing, and wage
 *   suppression. The constraint exhibits simultaneous coordination (federal
 *   price control reduces uncoordinated cost escalation) and extraction
 *   (concentrated on healthcare workers and smaller hospitals with limited
 *   market power). The regulation centralizes pricing authority in federal
 *   hands rather than allowing market negotiation or local adaptation,
 *   creating a network of compliance infrastructure (utilization review
 *   committees, cost accounting requirements) whose theater ratio increases
 *   over the implementation period as compliance becomes an end in itself
 *   rather than a means to cost control. This represents a textbook Tangled
 *   Rope: genuine coordination function (containing aggregate healthcare
 *   costs) layered with asymmetric extraction (burden falls on powerless
 *   frontline workers while benefits concentrate on taxpayers and federal
 *   budget).
 *
 * KEY AGENTS:
 *   - Federal Government / Medicare/Medicaid: Primary beneficiary (institutional/arbitrage) — captures $25 billion in federal budget savings and regulatory control authority; can exit through legislative repeal
 *   - Taxpayers and Insured Patients: Secondary beneficiary (moderate/constrained) — benefit from controlled premium growth and $35 billion in reduced out-of-pocket costs; constrained by potential service reductions
 *   - Hospital Providers (Large Networks): Moderate power (powerful/constrained) — face strict cost caps but retain some capacity to shift costs to outpatient care and adjust operations; can negotiate with federal regulators
 *   - Frontline Healthcare Workers: Primary victim (powerless/trapped) — face wage suppression and staffing reduction with no exit capacity; cannot renegotiate compensation under federal controls; subject to maximum extraction
 *   - State and Local Planning Agencies: Institutional actors (organized/constrained) — nominally coordinating actors whose actual discretion declines as federal compliance requirements expand
 *   - Hospital Industry Coalition: Organized negotiators (organized/constrained) — propose voluntary 9% growth cap as alternative to federal mandate; conditional commitment with sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing political economy of hospital pricing as immutable market failure rather than institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1979_carter_hospital_cost_containment, 0.58).
domain_priors:suppression_score(sotu_1979_carter_hospital_cost_containment, 0.65).
domain_priors:theater_ratio(sotu_1979_carter_hospital_cost_containment, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1979_carter_hospital_cost_containment, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_1979_carter_hospital_cost_containment, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sotu_1979_carter_hospital_cost_containment, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1979_carter_hospital_cost_containment, tangled_rope).
narrative_ontology:human_readable(sotu_1979_carter_hospital_cost_containment, "Federal Hospital Cost Containment Regulation (1979 Carter Proposal)").
narrative_ontology:topic_domain(sotu_1979_carter_hospital_cost_containment, "healthcare/economic_policy").

domain_priors:requires_active_enforcement(sotu_1979_carter_hospital_cost_containment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1979_carter_hospital_cost_containment, federal_government).
narrative_ontology:constraint_beneficiary(sotu_1979_carter_hospital_cost_containment, taxpayers).
narrative_ontology:constraint_beneficiary(sotu_1979_carter_hospital_cost_containment, insured_patients).
narrative_ontology:constraint_victim(sotu_1979_carter_hospital_cost_containment, hospital_providers).
narrative_ontology:constraint_victim(sotu_1979_carter_hospital_cost_containment, healthcare_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTLINE HEALTHCARE WORKER (SNARE) — Nurses, technicians, and support staff face wage suppression and staffing reduction directly caused by hospital cost caps. Cannot exit the healthcare system without retraining; cannot negotiate wages effectively under federal caps. Experiences maximum extraction with minimal coordination benefit. Federal price control removes their leverage in local labor markets.
constraint_indexing:constraint_classification(sotu_1979_carter_hospital_cost_containment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HOSPITAL NETWORK ADMINISTRATOR (TANGLED ROPE) — Experiences genuine coordination benefit (federal oversight reduces local rent-seeking, enables planning), but also faces strict cost constraints that force difficult allocation choices. Can adjust operations and potentially shift costs to outpatient care, but cannot exit federal regulation. Moderate power but high constraint. Benefits from regulatory predictability alongside costs of enforcement compliance.
constraint_indexing:constraint_classification(sotu_1979_carter_hospital_cost_containment, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL GOVERNMENT AND TAXPAYERS (ROPE) — Clear beneficiaries with arbitrage options: can adjust policy, redirect savings to other programs, exit commitment through legislative repeal. Experiences the constraint as pure coordination: federal spending control aligns incentives across the healthcare market. $60 billion in projected savings flows primarily to this group. Net positive directionality.
constraint_indexing:constraint_classification(sotu_1979_carter_hospital_cost_containment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE AND LOCAL HEALTH PLANNING AGENCIES (PITON) — Nominally coordinating actors (certificate-of-need programs, regional planning), but their function degrades under federal caps. They lose local discretion as federal rules become compliance theater. The constraint maintains their institutional presence but hollows their actual decision-making authority. Theater ratio rises as planning becomes regulatory compliance rather than adaptive resource allocation.
constraint_indexing:constraint_classification(sotu_1979_carter_hospital_cost_containment, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INSURED PATIENTS AND EMPLOYER GROUPS (TANGLED ROPE) — Benefit from controlled premium growth and federal cost containment, but also constrained by reduced service availability and longer wait times as hospitals economize. Genuine coordination of catastrophic risk pooling exists alongside extraction through access restrictions. Lower income patients within this group bear disproportionate access costs.
constraint_indexing:constraint_classification(sotu_1979_carter_hospital_cost_containment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: HOSPITAL INDUSTRY COALITION (SCAFFOLD) — Organized hospitals accept temporary cost controls as preferable to market competition pressure, but with explicit sunset logic: the industry proposes voluntary compliance with 9% annual cost growth caps as an alternative to federal mandatory controls. If industry meets targets, legislation is suspended. Classification as Scaffold derives from the coalition's framing of this as temporary, conditional commitment with negotiated exit terms.
constraint_indexing:constraint_classification(sotu_1979_carter_hospital_cost_containment, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a macroeconomic perspective, hospital cost inflation is treated as an immutable feature of healthcare markets: demand is inelastic (life-or-death decisions), supply is monopolistic (location-based hospital markets), and information asymmetries prevent price competition. Price controls appear as necessary corrections to market failure. This perspective risks treating the political economy of hospital pricing (technology adoption, capital intensity, labor practices) as natural constraints rather than institutional choices.
constraint_indexing:constraint_classification(sotu_1979_carter_hospital_cost_containment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1979_carter_hospital_cost_containment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1979_carter_hospital_cost_containment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1979_carter_hospital_cost_containment, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1979_carter_hospital_cost_containment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1979_carter_hospital_cost_containment, TR),
    TR >= 0.70.

:- end_tests(sotu_1979_carter_hospital_cost_containment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from healthcare providers and workers primarily through revenue caps and staffing restrictions. The $60 billion in savings (over 5 years, ~$12 billion annually) represents extraction from providers at 1979 baseline hospital spending of ~$100 billion — approximately 12% annual extraction. However, extraction is not total because hospitals retain capacity to cost-shift to outpatient care, adjust service mix, and negotiate pricing for new technologies. Measurement at t=4 (end of proposal interval) reflects increasing extractiveness as compliance mechanisms tighten and evasion options narrow. Suppression (0.65): Moderate-high. Hospitals and healthcare workers cannot easily exit federal price controls (national scope, universal application to Medicare/Medicaid providers). However, suppression is not total because providers can petition for exceptions, shift costs across service lines, and lobby for policy changes. Healthcare workers cannot negotiate wages, but some can transition to less-regulated outpatient settings. Theater ratio (0.48): Moderate. Utilization review committees and cost accounting requirements do perform genuine coordination functions (identifying unnecessary procedures, preventing local cost escalation), but some compliance activity becomes theater as regulators and providers game the system. Theater increases slightly over the interval (t=0 to t=4) as compliance infrastructure matures and documentation requirements expand beyond the functional minimum.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a profound perspectival gap across power and exit dimensions. Federal government sees pure coordination (Rope) — price controls solve the inflation problem by centralizing market authority. Large hospital networks see mixed coordination and constraint (Tangled Rope) — federal oversight prevents uncoordinated cost escalation, but also restricts their pricing flexibility. Frontline healthcare workers see pure extraction (Snare) — federal caps suppress their wages with no offsetting benefit. The hospital industry coalition sees temporary conditional constraint (Scaffold) — proposing voluntary compliance as an alternative exit path. State and local planning agencies see degraded authority (Piton) — maintaining institutional presence but losing real decision-making power as federal rules replace local adaptive capacity. The analytical observer risks seeing natural economic law (Mountain) — treating healthcare market failure as immutable rather than as a consequence of specific institutional arrangements (insurance pooling, provider licensing, capital intensity, information asymmetry). The perspectival gap reveals that the constraint's classification depends entirely on the observer's position in the extraction flow: beneficiaries with exit options see coordination; trapped agents with no voice see extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary status flows to federal government ($25B budget savings), taxpayers (~$35B in premium/out-of-pocket reductions), and employed insured patients. Victim status flows to healthcare providers (revenue constraints) and especially frontline healthcare workers (wage suppression). Derived directionality computations: federal actors (institutional power, arbitrage options) derive d ≈ 0.05-0.10 (strong beneficiary position); large hospital networks (powerful, constrained) derive d ≈ 0.55 (near-center, moderate extraction); smaller hospitals (powerful but location-trapped) derive d ≈ 0.75 (high extraction); healthcare workers (powerless, trapped) derive d ≈ 0.95 (maximum extraction target). The sig() function f(d) applies the standard sigmoid mapping, producing the perspectival gradation from negative χ (beneficiary) through moderate χ (tangled) to high χ (snare).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy through explicit structural decomposition. The federal government and taxpayers experience pure coordination (Rope perspective) — the constraint solves a genuine collective action problem (hospital cost escalation without private price competition). Simultaneously, frontline healthcare workers experience pure extraction (Snare perspective) — the constraint's costs are imposed without their consent and with no offsetting benefit. Both readings are structurally accurate; neither is a perceptual error. The Tangled Rope classification at the institutional and moderate power levels captures the hybrid: genuine coordination function (federal cost control addresses market failure) coexists with asymmetric extraction (burden concentrated on powerless agents). Mandatrophy is resolved by recognizing that the constraint solves a real problem while creating new extraction mechanisms — it is coordination that extracts from the weakest participants. The false-summit risk arises when the constraint is framed as natural economic law (Mountain) rather than as a political economy choice: healthcare cost inflation is treated as immutable market failure, naturalizing the need for federal intervention while hiding the choice of WHO bears the costs of adjustment. The analytical perspective approaches this false summit and must be explicitly flagged: the constraint appears unchangeable only if you accept the framing that hospital cost growth is inevitable. If you recognize it as institutional (choice of payment mechanisms, provider incentives, insurance pooling), the constraint becomes a contestable political economy question rather than natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_vs_mandatory_equivalence,
    'Does the hospital industry''s voluntary 9% growth cap commitment represent genuine coordination, or is it a negotiating position designed to forestall mandatory federal controls while preserving pricing power?',
    'Comparative historical analysis: track actual cost growth under voluntary compliance (1979-1982) versus mandatory federal cap scenarios in states that later implemented tighter controls. Measure compliance rates and industry evasion tactics (cost-shifting to outpatient care, diagnostic reclassification).',
    'If voluntary compliance succeeds: constraint classifies as Scaffold with genuine sunset. If industry systematically evades: constraint is Tangled Rope or Snare, and voluntary commitment masks extraction. Classification pivots on empirical compliance data.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_vs_mandatory_equivalence, empirical, 'Whether voluntary industry compliance represents genuine coordination or evasion').

omega_variable(
    cost_shifting_mechanism,
    'Do federal hospital cost caps simply transfer extraction to outpatient facilities, nursing homes, and insurance deductibles rather than reducing overall healthcare extraction?',
    'Longitudinal cost tracking across hospital, ambulatory, long-term care, and out-of-pocket spending post-implementation. Measure whether savings claimed for federal budget appear as cost increases elsewhere in healthcare system.',
    'If costs shift but total extraction unchanged: the constraint is Tangled Rope or Snare for the healthcare system overall, despite appearing as Rope for federal actors. If overall extraction genuinely decreases: the constraint is real coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cost_shifting_mechanism, empirical, 'Whether cost caps shift extraction rather than reduce it').

omega_variable(
    frontline_wage_suppression_causality,
    'Are healthcare worker wage freezes causally driven by federal price controls, or are they independent responses to inflation and labor supply conditions?',
    'Comparative wage growth analysis: hospital workers subject to federal caps versus workers in non-hospital healthcare (outpatient, insurance) and other regulated industries (utilities, telecommunications) during the same period. Controls for labor market tightness and inflation.',
    'If causally driven: suppression component of the constraint is structural and targets frontline workers specifically. If independent: the constraint''s suppression is lower than appears, and wage stagnation reflects broader macroeconomic factors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(frontline_wage_suppression_causality, empirical, 'Whether federal caps directly cause healthcare worker wage suppression').

omega_variable(
    federal_extraction_intent,
    'Does the $25 billion federal budget savings component represent legitimate cost containment, or is it implicit extraction from healthcare sector to federal coffers?',
    'Fiscal analysis of budget allocation: where do federal savings flow? Enhanced Medicare benefit generosity, investment in preventive care, or general appropriations? If savings are diverted to unrelated spending, the constraint has a federal extraction component.',
    'If savings invested in healthcare infrastructure: constraint is coordination with unequal benefit distribution (Tangled Rope). If diverted to general spending: constraint has explicit extraction mechanism (Snare component).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_extraction_intent, preference, 'Whether federal budget savings represent legitimate cost control or federal extraction').

omega_variable(
    market_discipline_counterfactual,
    'Would unregulated market competition (consumer price sensitivity, provider competition) achieve equivalent or superior cost control compared to federal price regulation?',
    'Counterfactual analysis: compare hospital cost growth in regulated versus less-regulated healthcare systems (e.g., pre-regulation baseline, state variations in certificate-of-need stringency). International comparison with market-based systems (Switzerland, Netherlands).',
    'If market discipline would succeed: federal regulation is extractive overhead (Snare component). If market discipline fails (information asymmetry, monopolistic markets): federal regulation is necessary coordination (Rope component).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(market_discipline_counterfactual, conceptual, 'Counterfactual: whether market discipline would match federal regulation effectiveness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1979_carter_hospital_cost_containment, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hcc_tr_t0, sotu_1979_carter_hospital_cost_containment, theater_ratio, 0, 0.35).
narrative_ontology:measurement(hcc_tr_t2, sotu_1979_carter_hospital_cost_containment, theater_ratio, 2, 0.42).
narrative_ontology:measurement(hcc_tr_t4, sotu_1979_carter_hospital_cost_containment, theater_ratio, 4, 0.48).

% Extraction over time
narrative_ontology:measurement(hcc_be_t0, sotu_1979_carter_hospital_cost_containment, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(hcc_be_t2, sotu_1979_carter_hospital_cost_containment, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(hcc_be_t4, sotu_1979_carter_hospital_cost_containment, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1979_carter_hospital_cost_containment, resource_allocation).
narrative_ontology:affects_constraint(sotu_1979_carter_hospital_cost_containment, healthcare_provider_bargaining_power).
narrative_ontology:affects_constraint(sotu_1979_carter_hospital_cost_containment, medicaid_fee_schedule_negotiation).
narrative_ontology:affects_constraint(sotu_1979_carter_hospital_cost_containment, certificate_of_need_hospital_expansion).

% DUAL FORMULATION NOTE:
% This constraint is part of a regulatory ecosystem for healthcare cost control spanning federal price regulation, state certificate-of-need programs, and Medicaid fee negotiation. Each component has its own extractiveness value: federal rate-setting (ε=0.58), state CON boards (ε=0.52), and Medicaid fee schedules (ε=0.48). The three constraints are linked through regulatory coordination — federal rules shape state implementation, which shapes Medicaid outcomes. Decomposition reflects that each regulatory mechanism has distinct beneficiaries (federal budget vs. state administrative control vs. federal program solvency) and victims (hospital revenues, healthcare workers, patient access).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1979_carter_hospital_cost_containment, powerful, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
