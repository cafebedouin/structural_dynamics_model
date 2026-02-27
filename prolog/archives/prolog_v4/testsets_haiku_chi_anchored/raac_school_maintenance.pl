% ============================================================================
% CONSTRAINT STORY: raac_school_maintenance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_raac_school_maintenance, []).

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
 *   constraint_id: raac_school_maintenance
 *   human_readable: Systemic Response to RAAC Concrete Failures in UK Schools
 *   domain: economic/political
 *
 * SUMMARY:
 *   Reinforced Autoclaved Aerated Concrete (RAAC) was used in hundreds of UK
 *   schools from the 1950s through 1990s as a cost-effective building
 *   material. By 2022, structural engineers confirmed widespread failure:
 *   concrete degrades, loses strength, and loses structural integrity. The
 *   constraint emerges from the institutional apparatus designed to manage
 *   this failure—a system that coordinates emergency response while
 *   extracting costs from students, families, and local authorities. Central
 *   government controls capital budgets and remediation timelines; local
 *   authorities and schools bear operational costs and educational
 *   disruption; students and families have no exit option. The system
 *   exhibits all characteristics of tangled rope: genuine coordination
 *   function (emergency triage, temporary relocations, remediation grants)
 *   overlaid with asymmetric extraction (selective funding, delayed spending,
 *   politically-motivated prioritization). Theater is high: extensive
 *   condition surveys, risk assessments, and public commitments to
 *   remediation exist alongside minimal actual spending. The constraint's
 *   temporal trajectory shows increasing extractiveness (ε rising from 0.32
 *   to 0.58 over 20 years) and theater ratio (from 0.45 to 0.68), consistent
 *   with institutional degradation—bureaucratic oversight persists while
 *   functional remediation stalls. Suppression is severe: students have no
 *   alternative schools; families cannot relocate; building closures cascade
 *   across regions simultaneously, preventing market-based solutions.
 *
 * KEY AGENTS:
 *   - Central Government Treasury: Institutional beneficiary (institutional/arbitrage) — controls capital allocation and can delay spending; benefits from deferred maintenance as fiscal policy tool
 *   - Department for Education: Institutional coordinator (institutional/constrained) — manages emergency response and statutory oversight but lacks independent capital authority
 *   - Students and Families: Primary victims (powerless/trapped) — no exit option; bear full risk of school closures, temporary relocations, and educational disruption
 *   - Local Education Authorities and School Leaders: Secondary victims (moderate/constrained) — statutorily obliged to provide education; bear operational costs of managing failing buildings without capital control
 *   - Building Safety Reform Coalition: Organized reformers (organized/constrained) — pushing for mandatory national standards and long-term funding commitments; see scaffold path through building information systems and predictive maintenance
 *   - School Maintenance Audit System: Institutional apparatus (institutional/arbitrage) — continues to generate formal risk assessments and condition reports while remediation decisions remain outside the audit function (piton classification from civilizational view)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(raac_school_maintenance, 0.58).
domain_priors:suppression_score(raac_school_maintenance, 0.72).
domain_priors:theater_ratio(raac_school_maintenance, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(raac_school_maintenance, extractiveness, 0.58).
narrative_ontology:constraint_metric(raac_school_maintenance, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(raac_school_maintenance, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(raac_school_maintenance, tangled_rope).
narrative_ontology:human_readable(raac_school_maintenance, "Systemic Response to RAAC Concrete Failures in UK Schools").
narrative_ontology:topic_domain(raac_school_maintenance, "economic/political").

domain_priors:requires_active_enforcement(raac_school_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(raac_school_maintenance, central_government_budgets).
narrative_ontology:constraint_beneficiary(raac_school_maintenance, original_contractors_and_insurers).
narrative_ontology:constraint_beneficiary(raac_school_maintenance, delayed_remediation_agents).
narrative_ontology:constraint_victim(raac_school_maintenance, affected_schools).
narrative_ontology:constraint_victim(raac_school_maintenance, students_and_families).
narrative_ontology:constraint_victim(raac_school_maintenance, local_education_authorities).
narrative_ontology:constraint_victim(raac_school_maintenance, building_safety_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STUDENTS/FAMILIES (SNARE) — No choice but to attend schools with failing RAAC; cannot relocate; bear full risk of structural failure, temporary closures, and disrupted education. d≈0.93, f(d)≈1.40, σ=1.0 → χ≈0.81. High effective extraction via forced exposure to deteriorating infrastructure without alternative access.
constraint_indexing:constraint_classification(raac_school_maintenance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOCAL EDUCATION AUTHORITIES & SCHOOL LEADERS (SNARE) — Constrained by statute to provide education while lacking capital budget control. Bear operational costs of managing failing buildings (emergency repairs, temporary moves, staff retention). Cannot exit their duties. d≈0.88, f(d)≈1.30, σ=1.0 → χ≈0.75. Effective extraction: must compensate for systemic underfunding.
constraint_indexing:constraint_classification(raac_school_maintenance, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: DfE / LOCAL GOVERNMENT (TANGLED ROPE) — Coordination function: central government allocates emergency remediation grants; local authorities coordinate building surveys and temporary relocations. Extraction: DfE maintains tight control over capital spending, forcing local authorities to prioritize politically visible projects over maintenance backlog. Required enforcement of spending conditionalities. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.44. Mixed coordination (real emergency response) and extraction (budget constraint weaponized).
constraint_indexing:constraint_classification(raac_school_maintenance, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CENTRAL GOVERNMENT TREASURY (ROPE) — Coordinates emergency response; benefits from controlling remediation timeline (can delay spending to smooth fiscal forecasts). Has arbitrage: can reallocate from other departments, issue bonds, or delay. d≈0.15, f(d)≈-0.01, σ=1.0 → χ≈-0.01. Negative extraction — this actor is net beneficiary of the coordination mechanism itself.
constraint_indexing:constraint_classification(raac_school_maintenance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: BUILDING SAFETY REFORM COALITION (SCAFFOLD) — Organized actors (safety campaigners, engineering associations, some civil servants) pushing for: (a) mandatory national building condition databases, (b) long-term capital funding commitments, (c) liability clarification. Has sunset: as building standards improve and predictive maintenance frameworks mature, the temporary emergency response should transition to routine condition monitoring. d≈0.45, f(d)≈0.45, σ=1.0 → χ≈0.26. Theater at transition points (public inquiries, emergency closures) but with genuine structural function (knowledge generation).
constraint_indexing:constraint_classification(raac_school_maintenance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: SCHOOL MAINTENANCE AUDIT SYSTEM (PITON) — Formal inspections and condition reporting have persisted for 30+ years despite consistent findings of massive underfunding. The audit theater continues (condition surveys, risk ratings, etc.) but generates no meaningful action because remediation decisions are made outside the audit system. theater_ratio=0.68 satisfies piton gate. System maintains institutional legitimacy without functional remediation.
constraint_indexing:constraint_classification(raac_school_maintenance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the constraint exhibits genuine coordination (managing collective infrastructure risk) AND genuine extraction (political budget cycles prioritizing short-term spending over long-term maintenance). The constraint did not emerge naturally; it was engineered by post-WWII austerity logic that assumed maintenance costs would decline. Instead, deferred maintenance compounds. ε=0.58, suppression=0.72 indicate this is not a natural law but a contested policy regime. The false summit test catches attempts to naturalize underfunding as inevitable.
constraint_indexing:constraint_classification(raac_school_maintenance, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(raac_school_maintenance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(raac_school_maintenance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(raac_school_maintenance, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(raac_school_maintenance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(raac_school_maintenance, TR),
    TR >= 0.70.

:- end_tests(raac_school_maintenance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Central government captures fiscal benefits from deferring remediation (lower annual spending, better short-term budget metrics), while the costs are distributed across students (educational disruption), families (anxiety, temporary relocations), and local authorities (emergency repair spending). The extraction is not total because emergency grants do flow and some remediation occurs—but the allocation is politically constrained and slower than structural urgency would warrant. Suppression (0.72): High. Students and families cannot exit their schools; local authorities cannot refuse to provide education; builders cannot retroactively improve concrete. Emergency measures (temporary relocations, enhanced monitoring) exist but are costly and disruptive, leaving suppression high. Theater ratio (0.68): Moderate-high. Formal condition surveys, risk assessments, and public commitments to remediation (especially post-2022 media coverage) are extensive. The actual remediation spending (as proportion of total school maintenance budget) is smaller, reflecting that the theater has increased over the interval—inspection capacity grew without remediation capacity growing in parallel. The school maintenance audit system exemplifies piton: it generates formal outputs (condition ratings, asset registers) that maintain institutional legitimacy while decisions about actual spending remain outside the audit logic.
 *
 * PERSPECTIVAL GAP:
 *   Students and families (powerless/trapped) see pure extraction (Snare): their structural position offers no exit, and they bear full educational risk. Local authorities (moderate/constrained) also see extraction (Snare): they must provide education but cannot control capital budgets. Central government (institutional/arbitrage) sees coordination (Rope): it controls the remediation timeline and can arbitrage between spending now vs. later. The building safety coalition (organized/constrained) sees a temporary crisis with a structural solution path (Scaffold): as national building standards improve and predictive maintenance systems mature, the emergency response should mature into routine condition monitoring—sunset logic. The maintenance audit system (institutional/arbitrage, civilizational view) sees its own degraded function (Piton): audits continue to generate formal outputs, but the real remediation decisions happen through political/fiscal channels outside the audit system. The analytical observer (civilizational) sees that this is not an immutable infrastructure constraint but a chosen policy regime—the extraction is contingent on capital budget architecture, not on the physics of concrete failure.
 *
 * DIRECTIONALITY LOGIC:
 *   Central government Treasury: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.02. Controls spending timeline; can reallocate from other budgets. Net beneficiary of deferral. Students/families: Victim + trapped → d≈0.93, f(d)≈1.40. No alternative schools; full exposure to educational disruption. Maximum extraction from this group. Local education authorities: Victim + constrained → d≈0.88, f(d)≈1.30. Must provide education but cannot control capital allocation. High extraction but not maximal (some emergency grants flow). Department for Education: Intermediate role (institutional/constrained). Must manage emergency response while executing central government's fiscal strategy. d≈0.55, f(d)≈0.75. Mixed coordination (real emergency triage) and extraction (fiscal discipline imposed from above). Building safety coalition: Organized + constrained → d≈0.45, f(d)≈0.45. Has agency and visibility but constrained by parliamentary budget process. Low effective extraction because coalition can advocate for structural reform.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION (Extracted Coordination vs. Pure Extraction): The constraint exhibits genuine coordination function (emergency triage of 223+ schools, temporary relocations, prioritization of highest-risk sites) AND genuine extraction (political budget cycles prioritizing short-term spending over long-term maintenance commitments, selective funding tied to electoral cycles). The tangled rope classification captures this hybrid: central government's immediate need to respond to the crisis (coordination logic) coexists with its fiscal incentive to stretch remediation over electoral cycles (extraction logic). The false summit test applies: attempts to naturalize the underfunding as 'inevitable infrastructure aging' or 'immutable budget constraints' would obscure the policy choice. The extraction is not technologically determined—higher capital budgets, earlier concrete inspection, or mandatory building condition standards could have changed the trajectory. The theater ratio (0.68) reflects that formal institutional processes (condition surveys, risk assessments, emergency protocols) have proliferated while actual remediation spending has not kept pace—Goodhart drift. The constraint will remain tangled rope unless one of two things happens: (1) building safety reform succeeds in shifting to predictive maintenance frameworks and sustained capital commitment (moving toward scaffold/rope equilibrium), or (2) political pressure forces immediate full remediation, collapsing the extraction mechanism (shifting toward rope). Current trajectory suggests neither—the system is drifting toward piton as the emergency becomes chronic and audit theater substitutes for remediation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    actual_remediation_cost_estimate,
    'What is the true total cost to remediate all RAAC-affected UK schools, and over what timeline is it achievable without disrupting education?',
    'Comprehensive structural survey of all 223+ affected schools; engineering assessments of repair vs replacement costs; budget modeling for competing capital demands',
    'If cost < £2B: underfunding is choice, not constraint (strengthens snare/extraction classification). If cost > £10B: remediation timeline forces extended school closures (shifts to catastrophic risk scenario).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(actual_remediation_cost_estimate, empirical, 'True remediation cost and feasible timeline for affected schools').

omega_variable(
    liability_and_insurance_resolution,
    'Can insurance, contractor liability, or defects liability trusts cover part of remediation costs, or does responsibility default entirely to public sector?',
    'Legal review of original build contracts; insurance policy discovery; assessment of statute of limitations on latent defects',
    'If private sector bears 20%+ of costs: constraint shifts toward snare (public absorbs residual). If public sector bears >95%: constraint is pure extraction from current students for past cost evasion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(liability_and_insurance_resolution, empirical, 'Allocation of liability and remediation costs between public and private actors').

omega_variable(
    maintenance_culture_shift,
    'Will the RAAC crisis trigger sustained political commitment to predictive maintenance and long-term capital planning, or revert to deferred-maintenance cycles once emergency is visibly ''resolved''?',
    'Historical comparison with previous building crises (asbestos removal, contaminated land remediation); tracking of capital spending commitments 5+ years post-crisis',
    'If shift is sustained: scaffold perspective is accurate, sunset is real. If reversion occurs: constraint returns to piton (performative audits without remediation); no learning accumulated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(maintenance_culture_shift, preference, 'Whether RAAC crisis produces lasting maintenance culture change or reverts to deferred maintenance cycles').

omega_variable(
    student_educational_harm_quantification,
    'What is the measurable impact on student attainment, attendance, and wellbeing from attending schools with failing RAAC vs. facilities-adequate comparators?',
    'Matched cohort analysis; longitudinal tracking of exam results, absence rates, and mental health indicators; school-level facility condition mapping',
    'If harm is quantifiable and large: victims'' true extraction cost becomes visible (shifts classification toward snare from powerless perspective). If harm is small/unmeasurable: extraction is obscured by dispersed risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(student_educational_harm_quantification, empirical, 'Measurable educational harm from attending schools with failing RAAC').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(raac_school_maintenance, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(raac_tr_t0, raac_school_maintenance, theater_ratio, 0, 0.45).
narrative_ontology:measurement(raac_tr_t10, raac_school_maintenance, theater_ratio, 10, 0.62).
narrative_ontology:measurement(raac_tr_t20, raac_school_maintenance, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(raac_be_t0, raac_school_maintenance, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(raac_be_t10, raac_school_maintenance, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(raac_be_t20, raac_school_maintenance, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(raac_school_maintenance, resource_allocation).
narrative_ontology:affects_constraint(raac_school_maintenance, school_capital_budget_cycles).
narrative_ontology:affects_constraint(raac_school_maintenance, local_government_fiscal_squeeze).
narrative_ontology:affects_constraint(raac_school_maintenance, uk_building_safety_standards).

% DUAL FORMULATION NOTE:
% RAAC concrete failure is a downstream constraint arising from mid-20th century cost-optimization decisions (upstream constraint: post-WWII austerity logic embedded in building codes). The structural failure is technologically inevitable; the institutional response is not. This story focuses on the systemic response to failure, not the material failure itself. The network links identify related policy constraints where capital budget cycles drive deferred maintenance patterns across multiple sectors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(raac_school_maintenance, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
