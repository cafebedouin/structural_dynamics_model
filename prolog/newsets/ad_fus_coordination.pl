% ============================================================================
% CONSTRAINT STORY: ad_fus_coordination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-16
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ad_fus_coordination, []).

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
 *   constraint_id: ad_fus_coordination
 *   human_readable: The Focused Ultrasound Alzheimer's Intervention
 *   domain: medical/neurological
 *
 * SUMMARY:
 *   High-intensity Focused Ultrasound (FUS) for Alzheimer's Disease
 *   represents a novel therapeutic pathway that coordinates ultrasonic energy
 *   to temporarily open the blood-brain barrier (BBB), enabling enhanced drug
 *   delivery and potential plaque clearance. While promising, this
 *   intervention creates a new structural constraint system. The system has a
 *   genuine coordination function (solving the BBB delivery problem) but is
 *   coupled with significant extraction (high costs, unknown long-term risks
 *   for patients, and large profits for patent holders). The classification
 *   of this constraint is therefore highly dependent on the observer's
 *   structural position.
 *
 * KEY AGENTS:
 *   - Alzheimer's Patients: Primary victims (powerless/trapped) — bear the physical risks and financial costs with few alternative treatments.
 *   - Pharmaceutical/Device Companies: Primary beneficiaries (institutional/arbitrage) — gain new markets for drugs and proprietary FUS systems.
 *   - Clinical Researchers: Secondary beneficiaries/victims (organized/constrained) — advance science and careers but are dependent on proprietary technology and funding cycles.
 *   - Healthcare Payers (Insurers/Governments): Secondary victims (institutional/constrained) — must bear the high cost of a new standard of care.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ad_fus_coordination, 0.55).
domain_priors:suppression_score(ad_fus_coordination, 0.65).
domain_priors:theater_ratio(ad_fus_coordination, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ad_fus_coordination, extractiveness, 0.55).
narrative_ontology:constraint_metric(ad_fus_coordination, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ad_fus_coordination, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ad_fus_coordination, tangled_rope).
narrative_ontology:human_readable(ad_fus_coordination, "The Focused Ultrasound Alzheimer's Intervention").
narrative_ontology:topic_domain(ad_fus_coordination, "medical/neurological").

domain_priors:requires_active_enforcement(ad_fus_coordination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ad_fus_coordination, pharmaceutical_companies).
narrative_ontology:constraint_beneficiary(ad_fus_coordination, medical_device_manufacturers).
narrative_ontology:constraint_beneficiary(ad_fus_coordination, research_institutions).
narrative_ontology:constraint_victim(ad_fus_coordination, alzheimers_patients).
narrative_ontology:constraint_victim(ad_fus_coordination, healthcare_payers).
narrative_ontology:constraint_victim(ad_fus_coordination, competing_research_programs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PATIENT (SNARE) — Facing a terminal diagnosis with few effective alternatives, the patient is structurally trapped. The high cost, unknown long-term risks of BBB opening, and reliance on a proprietary system constitute severe extraction, despite the therapeutic promise. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.78.
constraint_indexing:constraint_classification(ad_fus_coordination, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PHARMA/DEVICE CO. (ROPE) — For the developer, FUS is a pure coordination solution. It solves the problem of delivering drugs across the blood-brain barrier, creating new markets for previously failed therapeutics. From this position of arbitrage, the system is a net benefit. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.07.
constraint_indexing:constraint_classification(ad_fus_coordination, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: CLINICAL RESEARCHER (TANGLED ROPE) — Researchers are constrained by funding and career incentives tied to the technology. They see both the coordination function (a new tool to fight AD) and the extractive pressures (reliance on proprietary tech, pressure for positive results, ethical burden). d≈0.60, f(d)≈0.87, σ=1.0 → χ≈0.48.
constraint_indexing:constraint_classification(ad_fus_coordination, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (TANGLED ROPE) — The analytical view recognizes the dual nature of the constraint: a genuine coordination mechanism for therapy delivery that is structurally coupled with a high-extraction system benefiting developers at the expense of patients and payers. This is the canonical Tangled Rope. d≈0.73, f(d)≈1.15, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(ad_fus_coordination, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ad_fus_coordination_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ad_fus_coordination, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ad_fus_coordination, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ad_fus_coordination, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ad_fus_coordination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.55): High. This reflects the significant financial cost of a cutting-edge procedure, the value captured by patent-holding technology developers, and the physical risks (pathogen entry, long-term effects) borne by the patient. Suppression (0.65): High. For patients with progressing Alzheimer's, effective therapeutic alternatives are scarce, creating a situation of low effective choice. Access to the technology is also suppressed by its proprietary nature and high cost, limiting it to specialized centers. Theater Ratio (0.20): Low. The mechanism is largely functional, with measurable BBB opening and plaque reduction. The theater component is minor, related to marketing and hype cycles rather than core function.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is stark. For the technology developer with arbitrage options, FUS is a Rope that solves a technical problem and creates value. For the patient trapped by their diagnosis, the same system is a Snare, offering hope at a potentially devastating cost and unknown risk. The analytical observer and clinical researcher, who can see both the coordination benefits and the extractive costs, classify it as a Tangled Rope. This divergence is not a contradiction but the core analytical output of the DR system, revealing how the same constraint is experienced differently from different structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Pharma/Device Co.) with arbitrage exit options have a very low directionality (d≈0.05), resulting in negative effective extraction (χ<0) and a Rope classification. Victims (Patients) who are trapped have the highest possible directionality (d≈0.95), leading to amplified effective extraction (χ>0.66) and a Snare classification. Agents in the middle, like researchers, have moderate directionality, leading to a Tangled Rope classification. The final classification depends entirely on which agent's perspective is adopted.
 *
 * MANDATROPHY ANALYSIS:
 *   This case resolves a potential mandatrophy by refusing to label the FUS intervention as simply 'good' (Rope) or 'bad' (Snare). The Tangled Rope classification from the analytical perspective correctly identifies the dual nature of the system. It acknowledges the genuine coordination function (it helps patients) while simultaneously accounting for the asymmetric distribution of risk and reward (it helps corporations more). This prevents the mislabeling of a high-extraction medical system as a pure coordination good, while also avoiding the cynical dismissal of its real therapeutic potential.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    long_term_safety,
    'What are the neurological consequences of repeated, targeted blood-brain barrier openings over a patient''s remaining lifespan?',
    'Longitudinal studies (10+ years) tracking cognitive decline, brain atrophy, and incidence of opportunistic infections or other pathologies in treated vs. control cohorts.',
    'If long-term effects are negligible, the constraint''s extractiveness (ε) decreases, shifting classifications toward Rope. If significant negative effects emerge, ε increases, reinforcing the Snare perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(long_term_safety, empirical, 'Uncertainty over long-term safety of repeated BBB opening').

omega_variable(
    efficacy_generalizability,
    'Is the observed plaque reduction and cognitive improvement a robust, generalizable effect, or an artifact of specific patient subgroups and trial conditions?',
    'Large-scale, multi-center, double-blind, placebo-controlled trials with diverse patient populations.',
    'High generalizability confirms a strong coordination function (Rope/Tangled Rope). Low generalizability suggests the system is more extractive, functioning as a high-cost intervention with limited benefit (Snare/Piton).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(efficacy_generalizability, empirical, 'Whether therapeutic effects are robust and generalizable').

omega_variable(
    cost_benefit_threshold,
    'At what price point does the intervention shift from a justifiable therapeutic cost to an extractive burden on public and private healthcare systems?',
    'Health economic analysis (QALY calculations) combined with policy debate on acceptable cost ceilings for end-of-life care.',
    'This defines the boundary between Tangled Rope and Snare for the ''healthcare_payers'' victim group. There is no single empirical answer; it depends on societal preference.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_benefit_threshold, preference, 'Price point at which the intervention becomes unacceptably extractive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ad_fus_coordination, 2015, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ad_f_tr_t2015, ad_fus_coordination, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(ad_f_tr_t2022, ad_fus_coordination, theater_ratio, 2022, 0.15).
narrative_ontology:measurement(ad_f_tr_t2030, ad_fus_coordination, theater_ratio, 2030, 0.2).

% Extraction over time
narrative_ontology:measurement(ad_f_be_t2015, ad_fus_coordination, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement(ad_f_be_t2022, ad_fus_coordination, base_extractiveness, 2022, 0.5).
narrative_ontology:measurement(ad_f_be_t2030, ad_fus_coordination, base_extractiveness, 2030, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ad_fus_coordination, resource_allocation).
narrative_ontology:affects_constraint(ad_fus_coordination, alzheimers_drug_approval_pipeline).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
