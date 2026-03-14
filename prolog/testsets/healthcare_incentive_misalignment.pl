% ============================================================================
% CONSTRAINT STORY: healthcare_incentive_misalignment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_healthcare_incentive_misalignment, []).

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
 *   constraint_id: healthcare_incentive_misalignment
 *   human_readable: Healthcare Incentive Misalignment Between Providers and Patients
 *   domain: healthcare_economics/institutional_incentives
 *
 * SUMMARY:
 *   Healthcare incentive misalignment is a constraint operating across
 *   multiple institutional levels: between insurance companies and patients,
 *   between hospitals and providers, between manufacturers and prescribers.
 *   The fundamental structural problem is that financial incentives reward
 *   volume and intensity of care rather than health outcomes. Fee-for-service
 *   payment creates a principal-agent problem where providers capture surplus
 *   by delivering more care, while patients bear both direct costs and the
 *   burden of unnecessary treatment. The constraint exhibits five distinct
 *   classification types across different observer positions, revealing how
 *   the same structural arrangement appears as pure extraction (snare) to
 *   trapped patients, as coordination with embedded extraction (tangled rope)
 *   to mid-level actors, as pure coordination (rope) to institutional
 *   beneficiaries, as a temporary problem being solved (scaffold) to reform
 *   coalitions, and as an immutable feature of healthcare (mountain) to
 *   observers who naturalize economic constraints. The theater ratio has
 *   risen from 0.55 to 0.68 over 20 years, reflecting the massive growth of
 *   administrative overhead (billing codes, prior authorization, compliance)
 *   without corresponding improvement in health outcomes. The extractiveness
 *   has increased from 0.42 to 0.58, driven by rising pharmaceutical costs,
 *   insurance copayments, and out-of-pocket spending despite increased
 *   aggregate healthcare spending.
 *
 * KEY AGENTS:
 *   - Chronically Ill Patients: Primary victims (powerless/trapped) — bear full extraction cost; no exit option due to medical necessity
 *   - Low-Income Population: Structural victims (powerless/trapped) — face intergenerational health debt and forced choice between treatment and financial ruin
 *   - Primary Care Physicians: Mid-level constrained actors (moderate/constrained) — benefit from system stability while constrained by productivity metrics; experience both coordination and extraction
 *   - Hospital Administrators: Mid-level mobile actors (powerful/mobile) — structurally mobile but career-constrained by revenue metrics; occupy extraction position despite higher power level
 *   - Insurance Companies: Primary beneficiaries (institutional/arbitrage) — designed and benefit from coordination; high exit options through diversification
 *   - Pharmaceutical Manufacturers: Secondary beneficiaries (institutional/arbitrage) — benefit from patent protection and pricing power; experience constraint as pure coordination
 *   - Healthcare Reform Coalition: Organized disruptors (organized/constrained) — building alternative pathways (value-based care, capitation); see extractive mechanism as solvable
 *   - Billing and Coding Infrastructure: Institutional persistence mechanism (institutional/arbitrage) — maintains performative administrative theater; acknowledged as dysfunctional but entrenched
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing contingent institutional arrangements (information asymmetry, moral hazard) as inherent economic laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(healthcare_incentive_misalignment, 0.58).
domain_priors:suppression_score(healthcare_incentive_misalignment, 0.65).
domain_priors:theater_ratio(healthcare_incentive_misalignment, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(healthcare_incentive_misalignment, extractiveness, 0.58).
narrative_ontology:constraint_metric(healthcare_incentive_misalignment, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(healthcare_incentive_misalignment, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(healthcare_incentive_misalignment, tangled_rope).
narrative_ontology:human_readable(healthcare_incentive_misalignment, "Healthcare Incentive Misalignment Between Providers and Patients").
narrative_ontology:topic_domain(healthcare_incentive_misalignment, "healthcare_economics/institutional_incentives").

domain_priors:requires_active_enforcement(healthcare_incentive_misalignment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(healthcare_incentive_misalignment, insurance_companies).
narrative_ontology:constraint_beneficiary(healthcare_incentive_misalignment, hospital_systems).
narrative_ontology:constraint_beneficiary(healthcare_incentive_misalignment, pharmaceutical_manufacturers).
narrative_ontology:constraint_victim(healthcare_incentive_misalignment, patients_with_chronic_conditions).
narrative_ontology:constraint_victim(healthcare_incentive_misalignment, low_income_patients).
narrative_ontology:constraint_victim(healthcare_incentive_misalignment, treatment_effectiveness).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CHRONICALLY ILL PATIENT (SNARE) — Trapped by medical necessity and lack of alternatives. Cannot exit the healthcare system without risking death or disability. Faces maximum extraction: pricing power, unnecessary procedures, pharmaceutical costs, and behavioral manipulation (adherence theater). No viable exit options.
constraint_indexing:constraint_classification(healthcare_incentive_misalignment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOW-INCOME POPULATION (SNARE) — Structurally trapped by economic dependency and health vulnerability. Bears disproportionate extraction through medical debt, inability to access preventive care, and forced choice between treatment and financial ruin. Generational time horizon reveals intergenerational debt transmission and health disparities.
constraint_indexing:constraint_classification(healthcare_incentive_misalignment, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: PRIMARY CARE PHYSICIAN (TANGLED ROPE) — Constrained by insurance reimbursement rates, patient volume requirements, and liability exposure. Experiences genuine coordination function (managing patient health requires physician-patient alignment) alongside extraction mechanism (productivity metrics incentivize volume over quality, time pressure reduces patient engagement). Mixed position: benefits from system stability but constrained by throughput demands.
constraint_indexing:constraint_classification(healthcare_incentive_misalignment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: HOSPITAL SYSTEM ADMINISTRATOR (TANGLED ROPE) — Mobile exit option (can move between systems or sectors) but constrained by career incentives tied to revenue maximization and patient volume metrics. Genuine coordination function (managing operational efficiency, staff coordination, quality metrics) coexists with extraction incentive (fee-for-service model rewards volume regardless of outcome). Can see both the coordination necessity and the extraction mechanism.
constraint_indexing:constraint_classification(healthcare_incentive_misalignment, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: INSURANCE COMPANY (ROPE) — Benefits from coordination of risk pooling and claims processing. Experiences constraint as pure coordination (managing network, reducing adverse selection) with minimal perceived extraction. High exit options through diversification and arbitrage across markets. Low effective extraction from their perspective because they designed the system.
constraint_indexing:constraint_classification(healthcare_incentive_misalignment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PHARMACEUTICAL MANUFACTURER (ROPE) — Benefits from patent protection and market access. Experiences constraint as coordination of product distribution and reimbursement pathways. Views extraction as fair compensation for R&D risk. High arbitrage options through pricing power across markets and product line diversification.
constraint_indexing:constraint_classification(healthcare_incentive_misalignment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: HEALTHCARE REFORM COALITION (SCAFFOLD) — Organized advocacy groups, policymakers, and health systems experimenting with value-based care. See the incentive misalignment as a temporary structural problem with a sunset: integrated payment models, capitation, and outcome-based reimbursement are building alternative coordination pathways. Constrained by political economy and incumbent resistance, but with visible exit trajectory.
constraint_indexing:constraint_classification(healthcare_incentive_misalignment, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: FEE-FOR-SERVICE BILLING SYSTEM (PITON) — The billing and coding system (CPT codes, DRG classifications, prior authorization workflows) persists through institutional inertia. Theater ratio is high: enormous administrative overhead (theater=0.68) with minimal functional improvement in health outcomes. The system is widely acknowledged as dysfunctional (by patients, physicians, administrators) but remains because replacing it is politically and technically intractable. Piton classification derives from theater dominance, not from high experienced extraction.
constraint_indexing:constraint_classification(healthcare_incentive_misalignment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Naturalizes incentive misalignment as an inherent feature of healthcare markets: information asymmetry (patients cannot assess quality), moral hazard (insurance creates demand inflation), adverse selection (sickest patients drive costs). From civilizational scope, frames these as immutable economic laws. However, structural data reveals this as false naturalization: countries with different institutional arrangements (single-payer, global budgets, capitation) show that misalignment is contingent, not inherent.
constraint_indexing:constraint_classification(healthcare_incentive_misalignment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(healthcare_incentive_misalignment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(healthcare_incentive_misalignment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(healthcare_incentive_misalignment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(healthcare_incentive_misalignment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(healthcare_incentive_misalignment, TR),
    TR >= 0.70.

:- end_tests(healthcare_incentive_misalignment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The constraint extracts value from patients through pricing power, unnecessary procedures, pharmaceutical costs, and productivity-driven behavioral manipulation. However, extraction is not maximal because genuine care coordination occurs (some revenue flows to actual medical services, not pure rent capture), and some patients have insurance buffering direct costs. The value increased from 0.42 to 0.58 over the interval due to rising out-of-pocket costs, specialty drug prices, and administrative overhead. Suppression (0.65): High. Multiple barriers prevent exit: medical necessity (cannot choose not to get sick), economic dependency (no alternative system in US context), information asymmetry (cannot assess care quality), and behavioral lock-in (patients internalize deference to medical authority). However, suppression is not total because some patients can access concierge care, travel for treatment, or self-manage minor conditions. Theater ratio (0.68): High and rising. Massive administrative overhead (billing codes, prior authorization, credentialing, compliance) persists despite evidence that it does not improve outcomes. The rise from 0.55 to 0.68 reflects the explosion of regulatory and insurance-mandated documentation with minimal functional improvement. Many participants (patients, physicians, administrators) explicitly acknowledge the theater but feel unable to exit.
 *
 * PERSPECTIVAL GAP:
 *   The snare-to-rope reversal between patients and institutional actors is a clear diagnostic signal. Patients see the constraint as unchangeable and purely extractive because they are trapped by medical necessity and economic dependency. Institutional actors see the constraint as coordination because they occupy the beneficiary position. The tangled rope perspectives (physicians, administrators) are structurally accurate: they experience both genuine coordination function (managing health, operations) and extraction incentive (productivity metrics). The scaffold perspective is empirically grounded: value-based care models are actively being deployed and show measurable results in reducing unnecessary utilization. The piton perspective is real: billing and coding infrastructure is widely acknowledged as dysfunctional but persists through regulatory inertia. The mountain perspective is a false naturalization: information asymmetry and moral hazard are real economic phenomena, but they do not explain why the US system extracts 50% more per capita than other high-income countries with similar asymmetries but different institutions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are determined by structural position. Victims of the constraint (patients, treatment effectiveness) have high d because extraction flows away from them: d(trapped patient) ≈ 0.95 → f(d) ≈ 1.42 → high experienced extractiveness. Beneficiaries (insurance, pharma) have low d because extraction flows toward them: d(institutional beneficiary) ≈ 0.10 → f(d) ≈ -0.08 → negative/neutral experienced extractiveness (they see coordination). Mid-level actors have intermediate d: physicians constrained by metrics but able to practice legitimate medicine, administrators mobile but career-tied to revenue targets. The directionality computation from beneficiary/victim declarations correctly maps to structural reality: the constraint was built to extract from patients toward institutional actors, and their experienced positions reflect this.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint exhibits tangled_rope structure with genuine coordination and asymmetric extraction. The coordination function is real: healthcare requires insurance pooling (moral hazard problem), provider networks (access problem), quality monitoring (information asymmetry problem). But the constraint also extracts asymmetrically: fee-for-service payment incentivizes volume over outcome, pharmaceutical pricing captures consumer surplus, billing infrastructure creates private rents. The mandatrophy is resolved by showing that both functions are necessary to understand the constraint. Removal of the extraction mechanism (move to pure coordination/rope) would require institutional reform (capitation, value-based care, price controls), which is empirically feasible (observed in other countries) but politically difficult (concentrated benefits to incumbents, diffuse benefits to consumers). The constraint is NOT a coordination problem masquerading as extraction (which would be false classification), nor is it pure extraction with coincidental coordination (which would underestimate legitimacy of actual care). It is genuinely hybrid: the coordination necessity is real, and the extraction asymmetry is real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_hazard_vs_system_design,
    'How much of measured overutilization is genuine moral hazard (patients demand more care because they don''t pay directly) versus system design (providers have financial incentive to deliver unnecessary care)?',
    'Comparative analysis of utilization rates across different payment models (fee-for-service vs capitation vs global budget) controlling for disease prevalence, demographics, and access barriers. International comparison of overtreatment rates by payment system architecture.',
    'If dominated by moral hazard: constraint is coordination problem requiring deductibles/cost-sharing. If dominated by provider incentives: constraint is extraction mechanism requiring payment model reform. Current evidence suggests 60-70% system design, 30-40% moral hazard, but confidence is medium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_hazard_vs_system_design, empirical, 'Attribution of overutilization to moral hazard versus provider incentive misalignment').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is patient suppression (inability to exit) primarily structural (no alternatives, medical necessity, cost barriers) or internalized (belief that current system is necessary, deference to medical authority, acceptance of extraction as legitimate)?',
    'Post-exit survey of patients who changed insurance or systems; analysis of medical debt impact on future healthcare-seeking behavior; comparison of patient agency and advocacy between systems with different institutional arrangements.',
    'If primarily structural: suppression will persist until alternatives are built (scaffold sunset logic applies). If partially internalized: patients may not utilize alternatives even when available; internalized suppression reduces actual exit rates below structural barriers would predict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether patient suppression is structural or internalized').

omega_variable(
    value_based_care_effectiveness,
    'Does value-based care (outcome-based reimbursement, capitation, integrated payment) actually reduce extraction and align incentives, or does it merely shift the extraction mechanism without reducing total overhead?',
    'Longitudinal comparison of outcomes, costs, and patient satisfaction between value-based and fee-for-service cohorts controlling for selection bias. Analysis of whether total healthcare spending decreases or merely shifts between categories (e.g., prevention spending increases while acute care decreases).',
    'If effective: scaffold perspective is structural — value-based models represent genuine exit from the extraction mechanism. If ineffective: scaffold is aspirational; value-based care merely redistributes extraction, making the constraint appear to sunset when it actually persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(value_based_care_effectiveness, empirical, 'Whether value-based care models reduce extraction or redistribute it').

omega_variable(
    information_asymmetry_reducibility,
    'Can information asymmetry between patients and providers be meaningfully reduced through transparency mandates, quality reporting, and price disclosure, or is medical complexity sufficiently irreducible that asymmetry persists despite institutional reform?',
    'Analysis of patient decision-making behavior pre- and post-price transparency policies; effectiveness of quality metrics in changing patient provider selection; comparison of patient comprehension and confidence in medical decision-making across transparency intervention types.',
    'If reducible: information asymmetry is contingent and policy-addressable. If irreducible: mountain view contains truth — some extraction flows from structural patient powerlessness independent of institutional design. This affects whether full alignment is achievable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(information_asymmetry_reducibility, empirical, 'Reducibility of information asymmetry through institutional reform').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(healthcare_incentive_misalignment, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hcim_tr_t0, healthcare_incentive_misalignment, theater_ratio, 0, 0.55).
narrative_ontology:measurement(hcim_tr_t10, healthcare_incentive_misalignment, theater_ratio, 10, 0.62).
narrative_ontology:measurement(hcim_tr_t20, healthcare_incentive_misalignment, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(hcim_be_t0, healthcare_incentive_misalignment, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(hcim_be_t10, healthcare_incentive_misalignment, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(hcim_be_t20, healthcare_incentive_misalignment, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(healthcare_incentive_misalignment, resource_allocation).
narrative_ontology:affects_constraint(healthcare_incentive_misalignment, pharmaceutical_price_escalation).
narrative_ontology:affects_constraint(healthcare_incentive_misalignment, insurance_moral_hazard).
narrative_ontology:affects_constraint(healthcare_incentive_misalignment, administrative_billing_overhead).

% DUAL FORMULATION NOTE:
% Healthcare incentive misalignment is a constraint family with at least three structurally distinct components: (1) pharmaceutical pricing (ε ≈ 0.65, Snare — pure extraction with supply/demand manipulation), (2) insurance demand inflation via cost-sharing reduction (ε ≈ 0.40, Tangled Rope — genuine risk pooling plus moral hazard), (3) administrative overhead (ε ≈ 0.52, Piton — performative billing theater). This story aggregates across all three. Decomposition into separate stories is possible but would require different base_extractiveness values for each component.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(healthcare_incentive_misalignment, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
