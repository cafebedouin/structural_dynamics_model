% ============================================================================
% CONSTRAINT STORY: cancer_chronotherapy_timing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cancer_chronotherapy_timing, []).

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
 *   constraint_id: cancer_chronotherapy_timing
 *   human_readable: The Circadian Lifeline: Cancer Chronotherapy Timing Constraint
 *   domain: health/medical_technology
 *
 * SUMMARY:
 *   Cancer chronotherapy timing creates a structural constraint where the
 *   biological optimum (T-cell congregation circadian timing) intersects with
 *   institutional standardization (universal 3pm protocol) and population
 *   heterogeneity (chronotype diversity). The constraint exhibits mixed
 *   characteristics: genuine coordination value (discovering optimal timing
 *   windows), legitimate extraction (late chronotypes and shift workers
 *   cannot access peak efficacy times), institutional inertia (protocol
 *   enforcement persists partly through regulatory momentum), and real
 *   potential for sunset (personalized circadian measurement is becoming
 *   feasible). This is not a natural law — circadian biology is natural, but
 *   the universal 3pm protocol is institutional. Night-shift workers, late
 *   chronotypes (~30% of population), and populations with subsistence labor
 *   schedules face hard extraction: their peak immune response occurs outside
 *   the protocol window with no feasible exit. Early chronotypes and
 *   standard-schedule workers benefit from alignment. The research program
 *   and pharmaceutical manufacturer benefit through protocol ownership and
 *   market segmentation. An organized equity coalition sees this as a
 *   temporary problem solvable through personalized dosing windows within
 *   8-15 years as wearable circadian measurement becomes routine.
 *
 * KEY AGENTS:
 *   - Night-shift workers with cancer (powerless/trapped) — circadian desynchrony from both labor and treatment protocol; no exit; maximum extraction
 *   - Late chronotype patients (powerless/trapped) — ~30% of population; peak immune response 3-5 hours after protocol window; physiologically trapped
 *   - Global South populations (moderate/constrained) — benefit from treatment access but face schedule/infrastructure constraints; informal labor incompatible with fixed dosing windows
 *   - Oncology research programs (institutional/arbitrage) — discovered protocol; benefit from priority and therapeutic ownership; high agency
 *   - Pharmaceutical manufacturers (institutional/arbitrage) — benefit from protocol-driven market; can stratify patients and optimize supply chains
 *   - Chronotherapy equity coalition (organized/constrained) — organized advocates and researchers developing chronotype-stratified protocols and personalized approaches; see sunset mechanism
 *   - Clinical trial standardization system (institutional/arbitrage) — maintains fixed-time protocol for trial comparability; benefits from standardization; low functional verification
 *   - Analytical observer (analytical/analytical) — risks naturalizing institutional protocol choice as biological universality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cancer_chronotherapy_timing, 0.32).
domain_priors:suppression_score(cancer_chronotherapy_timing, 0.48).
domain_priors:theater_ratio(cancer_chronotherapy_timing, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cancer_chronotherapy_timing, extractiveness, 0.32).
narrative_ontology:constraint_metric(cancer_chronotherapy_timing, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(cancer_chronotherapy_timing, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cancer_chronotherapy_timing, tangled_rope).
narrative_ontology:human_readable(cancer_chronotherapy_timing, "The Circadian Lifeline: Cancer Chronotherapy Timing Constraint").
narrative_ontology:topic_domain(cancer_chronotherapy_timing, "health/medical_technology").

domain_priors:requires_active_enforcement(cancer_chronotherapy_timing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cancer_chronotherapy_timing, early_chronotype_patients).
narrative_ontology:constraint_beneficiary(cancer_chronotherapy_timing, oncology_research_programs).
narrative_ontology:constraint_beneficiary(cancer_chronotherapy_timing, pharmaceutical_manufacturers).
narrative_ontology:constraint_victim(cancer_chronotherapy_timing, late_chronotype_patients).
narrative_ontology:constraint_victim(cancer_chronotherapy_timing, shift_workers).
narrative_ontology:constraint_victim(cancer_chronotherapy_timing, global_south_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NIGHT-SHIFT WORKER WITH CANCER (SNARE) — Circadian timing of immunotherapy is structurally incompatible with work schedule. No exit: cannot abandon treatment window or employment. Bears full cost of pharmacokinetic mismatch. T-cell congregation peaks at times when patient is asleep or working; optimal treatment window is unavailable. Maximum experienced extraction — trapped between biological constraint and labor necessity.
constraint_indexing:constraint_classification(cancer_chronotherapy_timing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LATE CHRONOTYPE PATIENT (SNARE) — Circadian immune response is genetically distributed; ~30% of population are constitutive late chronotypes. These patients have peak T-cell congregation 3-5 hours after the protocol window (3pm). Trapped: cannot shift chronotype without severe health and social costs. Protocol suppresses awareness of chronotype variation. Maximum extraction — optimal treatment time is physiologically inaccessible.
constraint_indexing:constraint_classification(cancer_chronotherapy_timing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: GLOBAL SOUTH POPULATION (TANGLED ROPE) — Benefit: access to immunotherapy previously unavailable. Constraint: treatment timing protocol assumes urban electricity access, predictable work schedules, and healthcare infrastructure synchronized to Western time zones. Many patients operate on subsistence agriculture or informal labor with highly variable daily schedules. Constrained exit: treatment is life-saving but requires behavioral reorganization. Mixed extraction — significant benefit but also significant access asymmetry and implicit labor/schedule coercion.
constraint_indexing:constraint_classification(cancer_chronotherapy_timing, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: ONCOLOGY RESEARCH PROGRAM (ROPE) — Sees chronotherapy as a pure coordination achievement: discovering and disseminating the T-cell congregation timing window solves a critical problem (tumor heterogeneity, variable treatment response). Benefits from research priority, publication advantage, and therapeutic protocol ownership. Can exit or modify protocol based on new data. Low extraction — the constraint is experienced as scientific progress and institutional reputation.
constraint_indexing:constraint_classification(cancer_chronotherapy_timing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PHARMACEUTICAL MANUFACTURER (ROPE) — Chronotherapy timing creates a measurable, protocol-driven market: dosing windows, timing devices, patient monitoring systems. Can arbitrage regulatory approval (timing specificity increases novelty claims), market segmentation (early chronotypes become the primary addressable market), and supply chain optimization (dosing window creates predictable demand cycles). High agency — can modify protocol, adjust market strategy, or pivot to alternative interventions. Low extraction — the constraint creates commercial value.
constraint_indexing:constraint_classification(cancer_chronotherapy_timing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: CHRONOTHERAPY EQUITY COALITION (SCAFFOLD) — Organized effort (patient advocates, sleep researchers, global health organizations) to develop chronotype-stratified protocols and dosing windows tuned to individual circadian phases. See the current 3pm-only protocol as a temporary, improvable constraint with an 8-15 year sunset: personalized circadian measurement (wearables, core temperature monitoring, actigraphy) is becoming accessible and could enable individualized dosing windows. Constraints are real but temporary. Theater ratio declining as precision medicine tooling matures.
constraint_indexing:constraint_classification(cancer_chronotherapy_timing, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: CLINICAL TRIAL STANDARDIZATION SYSTEM (PITON) — The 3pm dosing window originated from a small cohort study with early-chronotype predominance (morning people, standard working hours). Institutional inertia: regulatory pathways require fixed-time protocols for comparability across trials. The 'universal 3pm rule' persists largely through procedural momentum, not through repeated verification of its optimality across chronotype distributions. Theater: trial protocol adherence is meticulously tracked and reported, masking the underlying chronotype sampling bias. Low actual verification of optimality — maintained through regulatory and institutional routine rather than functional necessity.
constraint_indexing:constraint_classification(cancer_chronotherapy_timing, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / CIRCADIAN BIOLOGY VIEW (MOUNTAIN) — From a civilizational view, circadian-dependent immune timing is a natural constraint of mammalian biology: T-cell congregation around tumors IS time-dependent due to circadian lymphocyte trafficking. This appears as an immutable natural law. However, the structural data reveals this as a false summit: the 'universal 3pm window' naturalizes a protocol choice derived from a biased cohort, not from biological universality. The underlying biology is natural; the protocol enforcement is institutional. Engine false summit detection flags this.
constraint_indexing:constraint_classification(cancer_chronotherapy_timing, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cancer_chronotherapy_timing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cancer_chronotherapy_timing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cancer_chronotherapy_timing, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(cancer_chronotherapy_timing, TR),
    TR >= 0.70.

:- end_tests(cancer_chronotherapy_timing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate. The constraint extracts value from patients whose circadian timing is misaligned with the 3pm window, but it also provides genuine therapeutic benefit (T-cell targeting works). The extraction is real but not maximal — late chronotypes still receive the immunotherapy, just with suboptimal pharmacokinetics. The value is lower than snare-level because the underlying intervention is beneficial and because organized efforts to personalize dosing are advancing. Suppression (0.48): Moderate-high. Barriers include: (1) protocol rigidity enforced through regulatory standardization, (2) low awareness of chronotype variation in cancer populations, (3) infeasibility of schedule modification for shift workers and subsistence laborers, (4) sleep inertia making early treatment difficult for late chronotypes. But suppression is not total — some patients can and do modify schedules, and wearable technology is reducing measurement barriers. Theater ratio (0.58): Moderate-high and rising. Initial protocol was presented as derived from biological universality ('optimal circadian window'), but it originated from a convenience sample with early chronotype bias. Trial reports emphasize protocol adherence tracking and statistical rigor, masking the chronotype sampling bias. Theater increased as the protocol became standardized — the procedural and reporting layer grew thicker relative to functional verification. Declining theater is possible as personalized approaches mature (scaffold perspective).
 *
 * PERSPECTIVAL GAP:
 *   Night-shift workers and late chronotypes see pure extraction (Snare) — the constraint extracts maximal cost with no exit. Global South populations see mixed effects (Tangled Rope) — significant benefit but also schedule coercion. Research programs and manufacturers see coordination (Rope) — they solved an important problem. The equity coalition sees a temporary barrier (Scaffold) — real but solvable with 8-15 year timeline. The trial standardization system sees procedural necessity (Piton) — maintains the rule through institutional routine, not functional verification. The civilizational analytical view risks naturalizing it as biology (Mountain) — but the structural data reveals the protocol as an institutional choice layered on top of real biological variation. The perspectival gap is largest between victims (trapped agents experiencing snare-level extraction) and beneficiaries (research/industry experiencing rope-level coordination value).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position. Late chronotype patients: beneficiary status (they receive treatment) but trapped exit (cannot modify schedule without severe costs) and victim status (excluded from peak efficacy window). The engine derives high d → high f(d) → high experienced extractiveness. Night-shift workers: similar derivation. Early chronotype patients: beneficiary status + mobile exit (can arrange morning schedule) + beneficiary alignment → low d → low or negative experienced extractiveness (constraint subsidizes them). Research programs: institutional beneficiary (priority, publications, therapeutic ownership) + arbitrage exit (can modify protocol or pivot) → very low d → negative χ (institutional subsidy effect). Global South populations: mixed beneficiary (access to therapy) and victim (schedule constraints) status with constrained exit → moderate d → moderate χ. Equity coalition: organized power + constrained exit (working to change protocol) → moderate d, but the organized status moderates the experienced extraction below the victim level.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint satisfies the tangled rope definition: (1) genuine coordination function — discovering circadian-optimized timing solves tumor heterogeneity problem; (2) asymmetric extraction — late chronotypes and shift workers are targets; (3) active enforcement — regulatory protocols mandate 3pm timing; (4) base extraction (0.32) and suppression (0.48) both meet thresholds (0.30 ≤ ε, suppression ≥ 0.40). The mandatrophy is resolved by showing that no single type adequately captures the constraint from all perspectives. Night-shift workers and late chronotypes experience Snare (high extraction, no exit). Early chronotypes and research programs experience Rope (low extraction, coordination value). The equity coalition experiences Scaffold (temporary, solvable). The system-level analyst sees institutional inertia (Piton). The false summit risk is the 'natural circadian law' framing — circadian biology is natural; the 3pm protocol is not. The constraint's true structure is: real biological variation (natural) + standardized protocol (institutional) + population heterogeneity in chronotype (natural, not captured by standard) = systematic extraction of late chronotypes + asymmetric benefit to early chronotypes + real research/industry coordination value. This is institutional extraction layered on biological reality, not pure biology.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    chronotype_heterogeneity_distribution,
    'What is the true population distribution of circadian peak immune timing, and what proportion of cancer patients cannot access the standard 3pm protocol window?',
    'Large-scale chronotype surveys in oncology populations; measurement of actual peak T-cell congregation times across diverse populations; correlation of chronotype distribution with treatment access and outcomes',
    'If >40% of patients are late chronotypes or shift workers: protocol should stratify by chronotype. If <20%: current protocol adequately covers the population. Different outcomes suggest different constraint types for different patient subgroups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chronotype_heterogeneity_distribution, empirical, 'Population distribution of circadian immune peak timing relative to protocol window').

omega_variable(
    personalized_dosing_window_feasibility,
    'Can wearable circadian monitoring (actigraphy, core temperature, melatonin salivary sampling) deliver personalized dosing windows at scale without prohibitive cost or patient burden?',
    'Cost analysis of personalized circadian measurement; clinical trials with individualized vs standard dosing; patient adherence rates; healthcare system adoption barriers',
    'If feasible: scaffold perspective is valid, sunset timeline is real, constraint is transitional. If not feasible: personalization remains aspirational, scaffold is premature, constraint persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(personalized_dosing_window_feasibility, empirical, 'Feasibility of scaled personalized circadian monitoring for dosing').

omega_variable(
    protocol_origin_bias_documentation,
    'What was the actual chronotype composition of the original cohort that established the 3pm window? Was it deliberately validated across diverse chronotypes, or was it derived from a convenience sample?',
    'Historical analysis of original trial protocols and participant demographics; reconstruction of circadian measurement methodology; comparison with modern chronotype measurement standards',
    'If deliberate cross-chronotype validation: protocol is evidence-based universality. If convenience sample bias: current protocol is institutional inertia masquerading as biology. Changes classification from natural constraint to institutional extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(protocol_origin_bias_documentation, empirical, 'Chronotype composition and validation scope of original protocol-establishing trial').

omega_variable(
    treatment_outcome_variance_by_chronotype,
    'Do late chronotype patients treated outside their peak T-cell window experience measurably worse outcomes (response rate, survival, adverse events) compared to early chronotypes, or is outcome variance driven by other factors (tumor heterogeneity, comorbidity)?',
    'Retrospective outcome analysis stratified by chronotype; prospective trials with chronotype-matched and mismatched cohorts; multivariate analysis controlling for tumor biology and patient factors',
    'If outcome variance is large by chronotype: extraction magnitude is high (snare classification confirmed). If variance is minimal: timing constraint may be theoretical rather than clinically enforcing, reclassifying as false extraction or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treatment_outcome_variance_by_chronotype, empirical, 'Treatment outcome variance attributable to chronotype-protocol timing mismatch').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cancer_chronotherapy_timing, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chrono_tr_t0, cancer_chronotherapy_timing, theater_ratio, 0, 0.38).
narrative_ontology:measurement(chrono_tr_t5, cancer_chronotherapy_timing, theater_ratio, 5, 0.52).
narrative_ontology:measurement(chrono_tr_t10, cancer_chronotherapy_timing, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(chrono_be_t0, cancer_chronotherapy_timing, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(chrono_be_t5, cancer_chronotherapy_timing, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(chrono_be_t10, cancer_chronotherapy_timing, base_extractiveness, 10, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cancer_chronotherapy_timing, resource_allocation).
narrative_ontology:affects_constraint(cancer_chronotherapy_timing, circadian_drug_metabolism).
narrative_ontology:affects_constraint(cancer_chronotherapy_timing, sleep_debt_immune_suppression).
narrative_ontology:affects_constraint(cancer_chronotherapy_timing, shift_work_cancer_risk).

% DUAL FORMULATION NOTE:
% The circadian chronotherapy timing constraint decomposes into three related but structurally distinct claims: (1) circadian_drug_metabolism (ε ≈ 0.12, Mountain) — pharmacokinetics of immunotherapy exhibit circadian variation; (2) sleep_debt_immune_suppression (ε ≈ 0.24, Mountain-to-Rope) — sleep disruption reduces immune function generally; (3) cancer_chronotherapy_timing (this story, ε = 0.32, Tangled Rope) — the institutional protocol choice that enforces a universal timing window despite population heterogeneity. This story is downstream of the first two (depends on their evidence) but adds institutional and social constraints not present in the underlying biology.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cancer_chronotherapy_timing, powerless, 0.92).
constraint_indexing:directionality_override(cancer_chronotherapy_timing, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
