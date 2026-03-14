% ============================================================================
% CONSTRAINT STORY: organ_scarcity_allocation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_organ_scarcity_allocation, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: organ_scarcity_allocation
 *   human_readable: Organ Scarcity Allocation System
 *   domain: healthcare/institutional_allocation
 *
 * SUMMARY:
 *   The organ scarcity allocation system presents a foundational constraint
 *   in modern medicine: how to distribute organs that cannot be created on
 *   demand to patients who will die without them. The constraint exhibits all
 *   six DR types from different structural positions, revealing how the same
 *   institutional system appears as natural law (to those who naturalize
 *   scarcity), as pure extraction (to those dying on waiting lists), as
 *   coordination (to those managing the system), and as an architectural
 *   problem with solutions (to those studying alternatives like opt-out
 *   procurement and paired exchange). The system's extractiveness has
 *   increased over the measurement interval as medical knowledge has improved
 *   organ preservation and matching, expanding what is technically possible —
 *   yet allocation mechanisms have not kept pace, creating artificial
 *   scarcity. The theater ratio reflects that family organ donation consent
 *   conversations often embed institutional frames and time pressure that
 *   constrain genuine autonomy, and that allocation criteria include elements
 *   justified by historical precedent rather than current medical evidence.
 *
 * KEY AGENTS:
 *   - Patients on waiting lists (especially low-SES, geographically isolated): Primary victims (powerless/trapped) — mortality is the ultimate extraction cost; no exit options
 *   - Transplant centers: Primary beneficiaries (institutional/arbitrage) — receive organ allocation, surgical revenue, prestige; full agency in negotiating allocation protocols
 *   - Living donors and altruistic participants: Secondary victims (moderate/constrained) — face medical risk, altruistic motivation exploitation, limited information about outcomes
 *   - Opt-out donation countries and paired exchange coalitions: Organized reformers (organized/constrained) — see architectural solutions; face resistance from entrenched procurement systems
 *   - Government health authorities: Dual role (institutional/constrained) — benefits from system stability and ethical legitimacy; constrained by international norms and domestic mortality pressure
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional scarcity as biological inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(organ_scarcity_allocation, 0.58).
domain_priors:suppression_score(organ_scarcity_allocation, 0.72).
domain_priors:theater_ratio(organ_scarcity_allocation, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(organ_scarcity_allocation, extractiveness, 0.58).
narrative_ontology:constraint_metric(organ_scarcity_allocation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(organ_scarcity_allocation, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(organ_scarcity_allocation, tangled_rope).
narrative_ontology:human_readable(organ_scarcity_allocation, "Organ Scarcity Allocation System").
narrative_ontology:topic_domain(organ_scarcity_allocation, "healthcare/institutional_allocation").

domain_priors:requires_active_enforcement(organ_scarcity_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(organ_scarcity_allocation, transplant_centers_with_waiting_power).
narrative_ontology:constraint_beneficiary(organ_scarcity_allocation, high_socioeconomic_status_patients).
narrative_ontology:constraint_victim(organ_scarcity_allocation, patients_on_waiting_lists).
narrative_ontology:constraint_victim(organ_scarcity_allocation, low_socioeconomic_status_patients).
narrative_ontology:constraint_victim(organ_scarcity_allocation, geographically_isolated_patients).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DYING PATIENT (SNARE) — Trapped on waiting lists with no exit option. Bears full cost of organ scarcity through mortality. Cannot negotiate, cannot arbitrage, cannot exit the system without death. The constraint extracts the ultimate cost: life itself. No coordination benefit perceived — only the extraction of time, hope, and eventually life.
constraint_indexing:constraint_classification(organ_scarcity_allocation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RESOURCED PATIENT (TANGLED ROPE) — Constrained by medical eligibility and geography, but with financial means to relocate or access multiple waiting lists. Experiences both coordination (access to organs through systematic matching) and extraction (waiting time, medical risk during wait, unequal access based on wealth). Can theoretically exit through private markets or relocation but faces high costs.
constraint_indexing:constraint_classification(organ_scarcity_allocation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: TRANSPLANT CENTER (ROPE) — Institutional actor with arbitrage capacity. Benefits from organ allocation system through patient volume, surgical revenue, transplant center prestige, and research opportunities. Experiences the constraint as coordination: the allocation system directs organs to their facility. Net beneficiary with full agency — can negotiate allocation protocols, accept or reject organs, influence waiting list prioritization.
constraint_indexing:constraint_classification(organ_scarcity_allocation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: OPT-OUT/LIVING DONOR COALITION (SCAFFOLD) — Organized agents (living donor programs, opt-out donation countries, paired kidney exchanges) see organ scarcity as a temporary coordination problem with architectural solutions. Opt-out systems (Spain, France, Belgium) achieve donation rates 5-10x higher than opt-in systems, suggesting the bottleneck is institutional design, not biological inevitability. Living donation and paired exchanges increase supply without coercion. Sunset logic: as opt-out norms spread and paired exchanges scale, the scarcity bottleneck weakens. Theater moderate but declining as evidence accumulates.
constraint_indexing:constraint_classification(organ_scarcity_allocation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL PROCUREMENT MODEL (PITON) — Deceased-donor organ procurement through hospital death certification and family consent is substantially performative. Medical determination of death is ritualized; family conversations are often theater-heavy with decision frames predetermined by local transplant center capacity and urgency. The procurement system persists through institutional inertia despite evidence that opt-out systems and living donation yield higher supply. Theater ratio reflects that much family consultation involves social influence rather than genuine choice — family autonomy is framed as a ritual gift rather than a structural decision point.
constraint_indexing:constraint_classification(organ_scarcity_allocation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: GOVERNMENT/HEALTH AUTHORITY (TANGLED ROPE) — Institutional actor facing constraints on its own power. Genuinely benefits from current allocation system (organs distributed, transplant outcomes reported, political stability maintained, no coercive procurement costs). But also constrained by international organ trafficking norms, ethical frameworks limiting commodification, and domestic political pressure from waiting list mortality. Cannot freely extract through live coercive procurement or corpse commodification without violating international treaties. Mixed coordination-extraction: the system coordinates organ supply and demand, but also extracts political legitimacy from maintaining ethical theater while tolerating high death rates.
constraint_indexing:constraint_classification(organ_scarcity_allocation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / BIOLOGICAL SCARCITY VIEW (MOUNTAIN) — From a civilizational/universal perspective, organ scarcity appears to be a natural limit: biological bodies have finite organs, transplant compatibility is restricted, mortality produces unpredictable supply. This perspective naturalizes the constraint as immutable. However, structural evidence contradicts the mountain classification: opt-out donation rates show scarcity is institutional design-dependent, not absolute. The 'natural scarcity' framing naturalizes extractive allocation mechanisms as inevitable, when the evidence suggests redesign could dramatically increase supply without coercion.
constraint_indexing:constraint_classification(organ_scarcity_allocation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(organ_scarcity_allocation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(organ_scarcity_allocation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(organ_scarcity_allocation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(organ_scarcity_allocation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(organ_scarcity_allocation, TR),
    TR >= 0.70.

:- end_tests(organ_scarcity_allocation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The system extracts through artificial scarcity maintenance: 17 people die daily in the US waiting for organs while viable organs are not procured. Opt-out countries achieve donation rates 5-10x higher, indicating scarcity is partly institutional. Extractiveness is not maximal (would be >0.70) because genuine medical coordination needs exist — organ matching, preservation, surgical timing — and because some allocation rules reflect medical necessity rather than pure rent-seeking. Suppression (0.72): High. Waiting list patients face insurmountable barriers: medical eligibility, geographic location, wealth for relocation, and time availability (must be reachable within hours for transplant). Barriers combine external (system design, equipment scarcity) and structural (biological facts). But suppression is not absolute — some patients do receive organs and some geographic/wealth barriers can theoretically be overcome. Theater ratio (0.65): Moderate-high. Family organ donation consent conversations embed institutional framing and time pressure. Allocation criteria include historical precedent and center preferences alongside medical factors. But transplantation itself has genuine technical requirements that justify some procedural theater.
 *
 * PERSPECTIVAL GAP:
 *   Powerless/trapped patients perceive a snare (extraction without coordination benefit, no exit). Resourced patients perceive tangled rope (mixed coordination and extraction, constrained exit). Transplant centers perceive rope (coordination with beneficiary status, full arbitrage). Opt-out coalitions perceive scaffold (solvable architectural problem with sunset). Traditional procurement perceives itself as piton (degraded ritual maintained by inertia). Government perceives tangled rope (mixed coordination and constraint). Analytical observer risks mountain (biological inevitability). The gap reveals that the constraint is experienced entirely differently depending on structural position. No single type describes all experiences because the structural asymmetry is the constraint itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) flow from structural positions in the extraction pipeline. Patients on waiting lists are powerless and trapped: high d toward 1.0 (they are pure targets). Transplant centers are institutional with arbitrage options: low d toward 0.0 (they are beneficiaries). Governments face constraints (international norms, ethical frameworks) preventing free commodification: moderate d around 0.40-0.50 (they benefit but face external limits on extractive power). Living donors have constrained options and face altruistic motivation framing: moderate-high d around 0.60-0.70 (they are partly victims, partly willing). The derived d values feed into chi computation: powerless patients experience high chi from the constraint; institutional centers experience low chi; governments experience moderate chi modified by their constrained exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that classification type varies legitimately with observer position. The same system is a snare for the powerless (high extraction, no coordination benefit, no exit), a rope for the centers (coordination with beneficiary status), and a scaffold for reformers (temporary problem with architectural solution). The false summit (mountain) naturalizes contingent institutional choices as biological inevitability. The piton (performative procurement rituals maintained by inertia) reveals that theater has increased as alternatives became known but not adopted. The mandatrophy is resolved not by finding the 'true' type but by recognizing that the presheaf over observation positions IS the constraint model. Each position sees a legitimate but partial view. The analytical meta-view (that scarcity is institutional-design-dependent, not absolute) integrates without flattening the perspectival diversity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_versus_institutional_scarcity,
    'How much of measured organ scarcity is biological inevitability versus institutional design choices (procurement, allocation rules, living donation barriers)?',
    'Comparative analysis of donation rates across procurement systems (opt-out vs opt-in countries); paired kidney exchange scaling data; xenotransplantation and artificial organ pipeline progress',
    'If institutional factors dominate: scarcity is contingent and the allocation system extracts through artificially maintained shortage. If biological factors dominate: scarcity is inevitable and the system optimizes distribution of unavoidable shortage.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biological_versus_institutional_scarcity, empirical, 'Decomposition of biological versus institutional scarcity factors').

omega_variable(
    allocation_criteria_capture,
    'Do allocation criteria (waitlist time, medical urgency, HLA match, geography) reflect genuine medical optimization or capture by transplant centers and wealthy regions?',
    'Outcome analysis: do center-favorable allocation rules correlate with transplant success rates or with center volume/revenue? Geographic equity audit; comparison of outcomes under different allocation rule sets',
    'If criteria are medically justified: tangled rope classification stands. If criteria are captured: classification should shift toward snare for powerless agents and beneficiary groups should be narrowed to capturing centers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allocation_criteria_capture, empirical, 'Whether allocation criteria reflect medical optimization or institutional capture').

omega_variable(
    ethical_theater_in_family_consent,
    'How much of family organ donation consent genuinely reflects family autonomy versus institutional framing and time pressure?',
    'Studies of family decision-making in donation contexts; comparison of framing effects on consent rates; analysis of refusal follow-up and reframing attempts',
    'High theater suggests procurement system uses social engineering to maintain donation rates despite scarcity and suppression. Changes theater ratio and piton classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ethical_theater_in_family_consent, empirical, 'Degree of family autonomy in organ donation consent').

omega_variable(
    paired_exchange_scaling_limits,
    'What are the true scaling limits for paired kidney exchange networks and living donation programs? Are they biological or organizational?',
    'Network matching algorithms and capacity analysis; international paired exchange data; organizational barriers to living donation scaling',
    'If limits are organizational: scaffold sunset is realistic and theatrical. If limits are biological: scaffold is aspirational and scarcity remains fundamental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paired_exchange_scaling_limits, empirical, 'Biological versus organizational limits to paired exchange scaling').

omega_variable(
    commodification_boundary,
    'Where is the boundary between ethical organ exchange (living donation, altruistic paired exchange) and commodification (direct sale, broking)?',
    'Ethical frameworks and international law analysis; empirical outcomes from countries permitting different levels of compensation; behavioral economics of donation motivation',
    'If ethical-commodification boundary is arbitrary: government suppression is political theater masking artificial scarcity. If boundary reflects real risk: constraints on commodification are justified harm-reduction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commodification_boundary, preference, 'Ethical and practical boundary of acceptable organ exchange mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(organ_scarcity_allocation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(organ_tr_t0, organ_scarcity_allocation, theater_ratio, 0, 0.55).
narrative_ontology:measurement(organ_tr_t10, organ_scarcity_allocation, theater_ratio, 10, 0.62).
narrative_ontology:measurement(organ_tr_t20, organ_scarcity_allocation, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(organ_be_t0, organ_scarcity_allocation, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(organ_be_t10, organ_scarcity_allocation, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(organ_be_t20, organ_scarcity_allocation, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(organ_scarcity_allocation, resource_allocation).
narrative_ontology:boltzmann_floor_override(organ_scarcity_allocation, 0.18).
narrative_ontology:affects_constraint(organ_scarcity_allocation, living_organ_donation_incentives).
narrative_ontology:affects_constraint(organ_scarcity_allocation, xenotransplantation_accessibility).
narrative_ontology:affects_constraint(organ_scarcity_allocation, artificial_organ_development_funding).

% DUAL FORMULATION NOTE:
% The organ scarcity allocation system is upstream of specific clinical outcomes (waiting list mortality, transplant success, access equity) and downstream of procurement policy design (opt-out versus opt-in systems, living donation barriers, paired exchange regulation). Separate constraint stories should model procurement design as its own constraint, with different ε reflecting institutional design-dependency rather than biological scarcity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(organ_scarcity_allocation, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
