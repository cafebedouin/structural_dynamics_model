% ============================================================================
% CONSTRAINT STORY: employment_boundary__hybrid_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employment_boundary__hybrid_security_reading, []).

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
 *   constraint_id: employment_boundary__hybrid_security_reading
 *   human_readable: Hybrid Third-Category Regime for Platform Work
 *   domain: labor economics/platform economy/social policy
 *
 * SUMMARY:
 *   A growing set of jurisdictions has answered the platform-work
 *   classification dispute by legislating a third category: platform workers
 *   are neither employees nor ordinary independent contractors but occupants
 *   of a bespoke status carrying tailored protections — mandatory
 *   occupational-accident insurance and enrollment in basic health schemes
 *   (observed enrollment around 91.5% and 86.2% respectively in mature
 *   schemes) — while owing nothing like the full employment-cost schedule (no
 *   pension accrual, paid leave, minimum hours, or severance). Platforms fund
 *   the partial schedule, insurers administer it, and legislatures present it
 *   as the modern compromise. This file generates ONE reading of the
 *   employment_boundary kernel — the hybrid_security_reading — as a clean,
 *   epsilon-invariant constraint. The epsilon referent is the standing hybrid
 *   arrangement itself, assessed by this reading's own lights: a reading that
 *   endorses tailored protection nonetheless authors moderate extraction
 *   because the arrangement institutionalizes precarity (retirement and
 *   career-development gaps) while claiming protection. The claim
 *   (tangled_rope) and the metrics are authored independently; the engine
 *   computes per-seat classifications from the structural data.
 *
 * KEY AGENTS:
 *   - - platform_operators: Primary beneficiary and administrator (institutional/arbitrage) — pays the partial contribution schedule, owes none of the employment-baseline obligations, captures the margin and the legitimacy
 *   - - platform_workers: Primary target with partial coverage (moderate/constrained) — enrolled in accident and health schemes at high rates, bears the retirement, sick-pay, and career-development gaps
 *   - - occupational_insurance_providers: Secondary beneficiary (organized/mobile) — holds mandated premium streams and expands into portable-benefits administration
 *   - - traditional_sector_employers: Collateral payer (powerful/mobile) — carries full employment costs while competing against platforms whose per-worker obligations are lighter
 *   - - legislature_labor_committees: Agenda setter (institutional/generational) — drafted the category after years of reclassification litigation, runs the consultations, holds amendment power
 *   - - platform_worker_collectives: Excluded voice (organized/constrained) — unions and rider associations pressing for full employee status from outside the consultation room
 *   - - labor_market_regulators: Analytical observer (institutional/analytical) — audits contributions, publishes the coverage statistics, refers violations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__hybrid_security_reading, 0.58).
domain_priors:suppression_score(employment_boundary__hybrid_security_reading, 0.42).
domain_priors:theater_ratio(employment_boundary__hybrid_security_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__hybrid_security_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__hybrid_security_reading, "Hybrid Third-Category Regime for Platform Work").
narrative_ontology:topic_domain(employment_boundary__hybrid_security_reading, "labor economics/platform economy/social policy").

domain_priors:requires_active_enforcement(employment_boundary__hybrid_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__hybrid_security_reading, '7772c226-4d3b-4666-8e3b-27e13e9b1a1d').
narrative_ontology:cs_kernel_codification('7772c226-4d3b-4666-8e3b-27e13e9b1a1d', formalized).
narrative_ontology:cs_authority_grounding('7772c226-4d3b-4666-8e3b-27e13e9b1a1d', lineage).
narrative_ontology:cs_interpretation_layer_present('7772c226-4d3b-4666-8e3b-27e13e9b1a1d').
narrative_ontology:cs_reading_relation('7772c226-4d3b-4666-8e3b-27e13e9b1a1d', employment_boundary__formalist_employment_reading, coexists_with).
narrative_ontology:cs_reading_relation('7772c226-4d3b-4666-8e3b-27e13e9b1a1d', employment_boundary__substantive_employment_reading, influences).
narrative_ontology:cs_axiom('7772c226-4d3b-4666-8e3b-27e13e9b1a1d', foundational, protection_tracks_arrangement_not_contract_form).
narrative_ontology:cs_axiom_status(protection_tracks_arrangement_not_contract_form, holdable).
narrative_ontology:cs_axiom_grounding('7772c226-4d3b-4666-8e3b-27e13e9b1a1d', protection_tracks_arrangement_not_contract_form, instrumental).
narrative_ontology:cs_axiom('7772c226-4d3b-4666-8e3b-27e13e9b1a1d', foundational, platform_flexibility_worth_preserving).
narrative_ontology:cs_axiom_status(platform_flexibility_worth_preserving, holdable).
narrative_ontology:cs_axiom_grounding('7772c226-4d3b-4666-8e3b-27e13e9b1a1d', platform_flexibility_worth_preserving, empirically_contingent).
narrative_ontology:cs_reference_frame('7772c226-4d3b-4666-8e3b-27e13e9b1a1d', plural_category_protection_framework).
narrative_ontology:cs_drift_state('7772c226-4d3b-4666-8e3b-27e13e9b1a1d', post_eu_platform_work_directive, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('7772c226-4d3b-4666-8e3b-27e13e9b1a1d', '').
narrative_ontology:cs_kernel_id(employment_boundary__hybrid_security_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, platform_operators).
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, occupational_insurance_providers).
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, platform_workers).
narrative_ontology:constraint_victim(employment_boundary__hybrid_security_reading, platform_workers).
narrative_ontology:constraint_victim(employment_boundary__hybrid_security_reading, traditional_sector_employers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate ride-hail, delivery, and microtask applications engaging millions of third-category workers. Pay accident-insurance premiums and basic health contributions for enrolled workers; owe no pension accrual, paid leave, minimum-hours guarantees, or severance. Administer day-to-day compliance, registration, and reporting, and shaped the category's design through consultation submissions and lobbying. Can adjust corporate structures, insert subcontracting layers, or shift market footprint across jurisdictions if obligations tighten.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Drive, deliver, and complete tasks across one or more apps. Are enrolled at high rates in occupational-accident insurance and basic health schemes funded by platform contributions and small deductions. Carry the costs the schedule does not transfer: no retirement accrual, limited sick pay, no guaranteed minimum hours, no career ladder. Income depends on staying available to the apps; switching between platforms is possible, but leaving app-based work altogether means forfeiting the income stream, and many stabilize earnings only by running several apps at once.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_workers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(employment_boundary__hybrid_security_reading, platform_workers, beneficiary).

% Underwrite the mandated accident and basic health schemes, receiving premium streams the statute creates. Have expanded product lines into portable-benefits administration and scheme management contracts. Can price policies, compete for administration tenders, and withdraw from unprofitable markets.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, occupational_insurance_providers, beneficiary,
    organized, biographical, mobile, continental).

% Drafted and enacted the third-category statutes after years of inconsistent reclassification rulings, and run the tripartite consultations that maintain the scheme. Face platform pressure to keep obligations light and union pressure to widen them. Amendment requires coalition-building across committees and chambers, and repeal would reopen the classification question the category was built to close.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, legislature_labor_committees, agenda_setter,
    institutional, generational, constrained, national).

% Unions and rider associations campaigning for full employee status or stronger floors. Were not seated in the consultation bodies that designed the category and are not part of scheme governance. Pursue reclassification lawsuits, strike actions, and ballot initiatives from outside the room where the category's terms are set.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_worker_collectives, excluded,
    organized, biographical, constrained, continental).

% Restaurants, logistics firms, and retailers who pay the full employment schedule — payroll taxes, leave entitlements, pension contributions — while competing against platforms whose per-worker obligations are lighter under the third category. Can relocate, automate, reclassify their own workforces toward similar arrangements, or lobby for symmetric obligations.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, traditional_sector_employers, payer,
    powerful, biographical, mobile, national).

% Ministries and inspectorates that monitor contribution compliance, maintain the enrollment statistics, and audit platforms for misclassification within the category. Publish the coverage figures cited in the policy debate and refer violations, but cannot unilaterally redefine the category's boundaries.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, labor_market_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(employment_boundary__hybrid_security_reading, platform_operators).
narrative_ontology:fixing_cost_class(employment_boundary__hybrid_security_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Extends basic social protection — occupational-accident insurance and enrollment in health schemes — to a workforce that falls outside both employment statutes and contractor norms, giving platforms, insurers, and the state a common administrative channel where the binary categories left workers uninsured.
% TRANSFER_FUNCTION: Moves scheduled contributions from platforms (plus small worker deductions) into accident and health schemes; leaves retirement accrual, paid leave, minimum-hours guarantees, and career-development costs untransferred, resting on workers; moves reputational legitimacy to platforms and the drafting legislature.
% ABSENT_VOICES: Worker collectives pressing for full employee status were not seated in the tripartite consultations that designed the category; undocumented workers and workers below registration thresholds are unrepresented in scheme governance. Both would argue the category freezes a second-tier status and forecloses the stronger remedy.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would strip enrolled workers' accident and health coverage pending replacement, reopen classification litigation in every jurisdiction that adopted the category, and force platforms to choose between absorbing full employment costs and shedding the protective framing. Courts, legislatures, insurers, and platforms would all immediately rearrange around the reopened boundary; millions of workers' current coverage depends on the scheme's continued operation.
% FOUNDING_PROBLEM: As platform work scaled faster than either legal category could reach, injured couriers and drivers had no occupational-accident cover, no health enrollment, and no recourse, while platforms carried none of the employer obligations that fund such protections in standard employment.
% FOUNDING_PROBLEM_CORROBORATION: Social-insurance administrators and public-health agencies — outside the benefiting parties — corroborate that the original gap was real and that enrollment now covers the large majority of registered platform workers. Union surveys and independent longitudinal studies of platform-worker finances corroborate that retirement accrual, sick pay, and career progression remain uncovered. No party disputes that the original gap existed; the parties dispute whether its closure is complete.
narrative_ontology:disappearance_verdict(employment_boundary__hybrid_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__hybrid_security_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__hybrid_security_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(employment_boundary__hybrid_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__hybrid_security_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_boundary__hybrid_security_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(employment_boundary__hybrid_security_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(employment_boundary__hybrid_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.58 at interval end) because the arrangement makes real transfers — accident-insurance premiums and health contributions flow, and enrollment is high — while the obligation schedule sits far below the employment baseline, leaving the difference (retirement, leave, career development) resting on workers; the gap has widened slowly as more workforces were absorbed into the category. Suppression (0.42) is authored as a raw structural property, unscaled by power or scope: the coercive element is not force against workers but foreclosure of alternatives — the category's existence deflects employee-status claims and its designers kept reclassification advocates out of the consultation room — with enforcement machinery (registration, contribution collection, audit) built up steadily over the interval. Theater ratio (0.40) reflects the growing share of arrangement activity that consists of citing coverage percentages as proof of protection while the uncovered security components persist; the coverage itself is real function, not performance. Accessibility collapse is low-moderate (0.35): the formalist and substantive alternatives remain live in other jurisdictions and before courts, and platforms retain restructuring exits. Resistance (0.55) is substantial and sustained: strikes by courier fleets, reclassification litigation, and ballot-measure campaigns run continuously against the category's terms. All three tracked series share one time grid (points 0-12 at stride 2) so no metric row is sampled against another metric's end-state.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the platform_operator seat the arrangement is a hard-won, functional compromise it helps fund and administer — protection delivered, litigation settled, business model intact. From the platform_worker seat the same structure is partial coverage layered over persistent precarity: insured against crashes at work, uninsured against old age and income collapse. The occupational_insurance_provider seat sees mandated markets; the traditional_sector_employer seat sees a cost asymmetry it must lobby or automate against; the legislature seat sees an administrable middle way that ended years of inconsistent rulings; the excluded worker collectives see a second-class status frozen into statute. Same statute, structurally different arrangements depending on which seat computes.
 *
 * DIRECTIONALITY LOGIC:
 *   platform_operators are declared beneficiaries and sit near the beneficiary end: they pay a partial schedule, gain the protective framing, and hold arbitrage-grade exit (corporate restructuring, jurisdiction shopping). occupational_insurance_providers are pure beneficiaries of mandated premium streams with mobile exit. platform_workers are deliberately declared in BOTH the beneficiary and victim arrays — they receive real coverage (which damps directionality away from full target) and bear the uncovered security costs (which raises it); the derivation should land them mid-to-high rather than at the full-target pole, which is exactly the hybrid's signature. traditional_sector_employers are victims of the competitive distortion with no offsetting receipt, sitting high. legislature_labor_committees administer the arrangement and collect legitimacy — low-to-mid. The excluded collectives and the analytical regulator carry no extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — acute, total unprotectedness of platform workers — is substantially closed for enrolled workers, and the arrangement persists beyond that closure as the settled answer to the classification dispute. Authoring tangled_rope keeps both facts simultaneously visible: the coordination function (coverage delivery through a workable administrative channel) and the asymmetric extraction (obligation shortfall plus foreclosure of the reclassification route). Calling it a rope would hide the shortfall behind the delivered coverage; calling it a snare would erase coverage that genuinely flows and that workers would lose in a formalist world. The R5 interview records the founding problem as contested — platforms and the legislature cite the enrollment statistics as closure, unions and longitudinal studies attest the retirement and career gaps — so the mismatch consumer sees status=contested crossed with verdict=world_rearranges: no zombie flag fires, but the live dispute is on the record rather than smoothed over.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates only the hybrid_security_reading of the employment_boundary kernel. What would change structurally if a sibling reading governed instead, and where exactly is the disagreement located?',
    'Compare jurisdictional adoption patterns and worker outcomes across regimes: formalist_employment_reading empties the victim set of any platform obligation (workers fully exposed to accident, illness, and income risk; epsilon collapses toward the contractor baseline), while substantive_employment_reading collapses platform workers into full employment (victim set shrinks to residual gaps; platforms carry the entire employment-cost schedule). The disagreement is located in the classification criterion itself: contract form versus economic dependence versus a bespoke third category.',
    'Reclassification under either sibling changes the victim set, the beneficiary set, and epsilon discontinuously; the hybrid arrangement''s moderate extraction profile exists only under this reading''s category boundaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one reading of the employment_boundary kernel among three live siblings.').

omega_variable(
    coverage_security_gap,
    'Do high enrollment rates (medical coverage ~91.5%, injury coverage ~86.2%) constitute genuine security when retirement accrual, paid sick leave, minimum-hours guarantees, and career development remain uncovered?',
    'Longitudinal worker-outcome panels comparing hybrid-enrolled, employed, and contractor cohorts on retirement savings accumulation, injury-compensation adequacy, and income stability over a decade.',
    'If hybrid-cohort outcomes converge with employment cohorts, the authored extractiveness is overstated; if they track contractor precarity, the coverage statistics function as legitimation display and extractiveness is understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coverage_security_gap, empirical, 'Whether measured coverage depth matches lived security.').

omega_variable(
    counterfactual_baseline,
    'Is the third category net-harmful (it institutionalizes a second-tier status and gives platforms a settlement that blunts reclassification claims) or net-beneficial (it delivers accident and health coverage that would not otherwise exist)?',
    'Natural experiments where courts imposed employee status on platform workforces: track whether coverage, hours stability, and earnings improved, or whether platforms cut headcount, converted to franchise/subcontract layers, or exited.',
    'If the substantive counterfactual delivered better worker outcomes without mass exclusion, the hybrid''s extraction component reads as rent protected by political feasibility; if reclassification triggered exclusion, part of the measured extraction is the price of deliverable protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_baseline, conceptual, 'Counterfactual ambiguity in evaluating the hybrid bargain.').

omega_variable(
    compliance_erosion_drift,
    'Do platform contributions to the hybrid schemes erode over time through misclassification drift, subcontracting layers, and multi-apping that shifts premium burdens?',
    'Audit series on contribution arrears, effective coverage per active worker, and the gap between registered and actually-working populations.',
    'Sustained erosion pushes the arrangement from shared-cost compromise toward pure extraction riding on a protective label; stable compliance supports a durable hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_erosion_drift, empirical, 'Whether the partial-obligation schedule holds or decays.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__hybrid_security_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t0, employment_boundary__hybrid_security_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(empl_tr_t2, employment_boundary__hybrid_security_reading, theater_ratio, 2, 0.25).
narrative_ontology:measurement(empl_tr_t4, employment_boundary__hybrid_security_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement(empl_tr_t6, employment_boundary__hybrid_security_reading, theater_ratio, 6, 0.31).
narrative_ontology:measurement(empl_tr_t8, employment_boundary__hybrid_security_reading, theater_ratio, 8, 0.34).
narrative_ontology:measurement(empl_tr_t10, employment_boundary__hybrid_security_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement(empl_tr_t12, employment_boundary__hybrid_security_reading, theater_ratio, 12, 0.4).

% Extraction over time
narrative_ontology:measurement(empl_be_t0, employment_boundary__hybrid_security_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(empl_be_t2, employment_boundary__hybrid_security_reading, base_extractiveness, 2, 0.47).
narrative_ontology:measurement(empl_be_t4, employment_boundary__hybrid_security_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(empl_be_t6, employment_boundary__hybrid_security_reading, base_extractiveness, 6, 0.53).
narrative_ontology:measurement(empl_be_t8, employment_boundary__hybrid_security_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(empl_be_t10, employment_boundary__hybrid_security_reading, base_extractiveness, 10, 0.57).
narrative_ontology:measurement(empl_be_t12, employment_boundary__hybrid_security_reading, base_extractiveness, 12, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t0, employment_boundary__hybrid_security_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(empl_su_t2, employment_boundary__hybrid_security_reading, suppression_requirement, 2, 0.32).
narrative_ontology:measurement(empl_su_t4, employment_boundary__hybrid_security_reading, suppression_requirement, 4, 0.34).
narrative_ontology:measurement(empl_su_t6, employment_boundary__hybrid_security_reading, suppression_requirement, 6, 0.36).
narrative_ontology:measurement(empl_su_t8, employment_boundary__hybrid_security_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(empl_su_t10, employment_boundary__hybrid_security_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(empl_su_t12, employment_boundary__hybrid_security_reading, suppression_requirement, 12, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__hybrid_security_reading, resource_allocation).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, formalist_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, substantive_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, portable_benefits_schemes).

% DUAL FORMULATION NOTE:
% The colloquial label 'employment status of platform workers' decomposes into three structurally distinct constraints sharing one kernel (employment_boundary): the formalist reading (historical baseline; no platform obligations; workers fully exposed), the substantive reading (full employment obligations; victim set shrinks to residual gaps), and this hybrid reading (partial obligation schedule; moderate epsilon). The formalist reading is upstream as the inherited default; substantive-reading court rulings created the pressure that produced hybrid statutes; hybrid enactment in turn siphons reform energy from substantive campaigns and shapes portable-benefits design downstream. Each member authors its own epsilon over the standing arrangement it contests; none hedges across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
