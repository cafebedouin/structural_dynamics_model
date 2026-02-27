% ============================================================================
% CONSTRAINT STORY: delta_force_selection_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_delta_force_selection_2026, []).

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
 *   constraint_id: delta_force_selection_2026
 *   human_readable: Delta Force (1st SFOD-D) Selection & Assessment
 *   domain: military/special_operations
 *
 * SUMMARY:
 *   The 1st Special Forces Operational Detachment-Delta selection process
 *   represents a high-extractiveness constraint that operates through extreme
 *   suppression of alternatives and candidate agency. The constraint exhibits
 *   the structural signature of a snare: candidates face exceptional
 *   physical, psychological, and temporal costs with no meaningful exit path;
 *   originating units bear organizational depletion; yet SOCOM and the
 *   selection institution itself benefit from the process. The 30-year
 *   interval tracks a subtle but significant trend: as the operational
 *   environment has become more complex and non-kinetic, the selection
 *   process has become more theatrical (theater_ratio rising from 0.38 to
 *   0.52) while extractiveness has increased from 0.55 to 0.68. This pattern
 *   suggests institutional drift toward rent-seeking: the selection process
 *   has become increasingly detached from functional filtering and more
 *   oriented toward legitimacy maintenance and institutional perpetuation.
 *   The false summit detected from the analytical perspective reveals that
 *   defenders of current selection intensity often invoke natural law
 *   arguments ('this is inherent to elite warfare') that are actually
 *   contingent institutional choices.
 *
 * KEY AGENTS:
 *   - Candidate Soldiers: Primary victims (powerless/trapped) — bear 2-3 years of unpaid degradation, career opportunity cost, psychological trauma with no exit mechanism
 *   - Originating Unit Commanders: Secondary victims (moderate/constrained) — lose trained personnel to selection, face operational capacity extraction
 *   - Special Operations Command: Primary beneficiary (institutional/arbitrage) — acquires filtered operators, maintains institutional prestige, controls elite force narrative
 *   - Military Personnel System: Systemic victim (organized/constrained) — loses personnel, experiences morale degradation, bears long-term PTSD/retention costs
 *   - Selection Institution: Institutional actor (institutional/arbitrage) — maintains legitimacy through ritual persistence despite degraded functional necessity
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional practices as inherent requirements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(delta_force_selection_2026, 0.68).
domain_priors:suppression_score(delta_force_selection_2026, 0.78).
domain_priors:theater_ratio(delta_force_selection_2026, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(delta_force_selection_2026, extractiveness, 0.68).
narrative_ontology:constraint_metric(delta_force_selection_2026, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(delta_force_selection_2026, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(delta_force_selection_2026, snare).
narrative_ontology:human_readable(delta_force_selection_2026, "Delta Force (1st SFOD-D) Selection & Assessment").
narrative_ontology:topic_domain(delta_force_selection_2026, "military/special_operations").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(delta_force_selection_2026, special_operations_command).
narrative_ontology:constraint_victim(delta_force_selection_2026, candidate_soldiers).
narrative_ontology:constraint_victim(delta_force_selection_2026, military_unit_cohesion).
narrative_ontology:constraint_victim(delta_force_selection_2026, standard_army_morale).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CANDIDATE SOLDIER (SNARE) — Trapped in selection process with no meaningful exit. Extraction: 2-3 years of unpaid physical/psychological degradation, career opportunity cost. Cannot quit without stigma or retaliation. d≈0.93, f(d)≈1.40, σ=1.0 → χ≈0.95.
constraint_indexing:constraint_classification(delta_force_selection_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORIGINATING UNIT COMMANDER (SNARE) — Loses personnel to selection process; faces extraction of operational capacity and institutional continuity. Exit is constrained by military command structure — cannot refuse to send candidates. d≈0.82, f(d)≈1.25, σ=1.0 → χ≈0.85.
constraint_indexing:constraint_classification(delta_force_selection_2026, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SPECIAL OPERATIONS COMMAND (ROPE) — Net beneficiary. Acquires proven operators through extreme filtration. Selection process solves coordination problem: identifying who can perform in high-risk environments. SOCOM sees the constraint as legitimate vetting mechanism. d≈0.10, f(d)≈-0.10, σ=1.0 → χ≈-0.07.
constraint_indexing:constraint_classification(delta_force_selection_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MILITARY PERSONNEL SYSTEM (SNARE) — Organized institutional perspective. Selection creates systemic extraction: loss of trained personnel, psychological trauma affecting retention, institutional erosion of morale across conventional units. The system is constrained by statutory force structure requirements — cannot exit without Congressional authorization. d≈0.70, f(d)≈1.05, σ=1.0 → χ≈0.71.
constraint_indexing:constraint_classification(delta_force_selection_2026, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: SELECTION INSTITUTION (PITON) — Selection protocols persist through institutional inertia despite degraded functionality: theater_ratio=0.52. Instructors acknowledge that physical punishment (sleep deprivation, cold water, land navigation) is largely theatrical — the real selection happens in earlier, less performative stages. Yet the brutal ritual persists as a legitimation mechanism: 'we survived the gauntlet' becomes proof of worthiness. d≈0.15, f(d)≈0.05, σ=1.0 → χ≈0.03.
constraint_indexing:constraint_classification(delta_force_selection_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW — From a civilizational perspective, extreme filtering is an irreducible feature of any profession requiring consistent performance under conditions where failure means death. Some minimum threshold of selection intensity is inherent to the task structure. However, the base properties (ε=0.68, suppression=0.78, theater=0.52) contradict the mountain classification — the engine will detect a false summit, revealing that specific institutional practices (the 'hell week' ritual, psychological domination, deliberate humiliation) are contingent design choices, not laws of nature.
constraint_indexing:constraint_classification(delta_force_selection_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(delta_force_selection_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(delta_force_selection_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(delta_force_selection_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(delta_force_selection_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(delta_force_selection_2026, TR),
    TR >= 0.70.

:- end_tests(delta_force_selection_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Candidates invest 2-3 years of unpaid labor, psychological trauma, and foregone career advancement opportunities. Success rate of 1-3% means 97-99% of candidates experience extraction with no benefit. SOCOM captures operational advantage without proportional cost. The trajectory (0.55→0.68 over 30 years) indicates increasing extraction intensity. Suppression (0.78): Very high. Candidates cannot exit without severe stigma, retaliation, or career termination. The originating unit system prevents horizontal mobility — soldiers cannot easily transfer to other units to avoid the selection deployment. Psychological pressure is structured to maximize compliance and suppress alternatives (self-care, unit cohesion, rational cost-benefit analysis). Theater ratio (0.52): Moderate-high. Significant portion of selection is performative: sleep deprivation and cold water immersion are documented as poor predictors of actual operator performance; the real selection is cognitive and judgment-based and happens earlier; yet the brutal rituals persist as legitimation theater. The upward trend (0.38→0.52) suggests degradation toward more theatrical maintenance as functional necessity declines.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces a radical perspectival divergence. SOCOM sees legitimate coordination (Rope) — a mechanism for identifying who can perform in life-or-death contexts. Candidates see pure extraction (Snare) — a mechanism for extracting 2-3 years of unpaid labor and psychological trauma from individuals with no exit option. Originating units see organizational predation (Snare) — systematic depletion of trained personnel. The selection institution sees its own ritual as degraded but legitimate (Piton) — protocols persist through inertia despite weak functional grounding. The analytical observer risks seeing immutable necessity (Mountain) but the data reveals contingency: theater_ratio of 0.52 shows that 52% of selection protocols have no direct bearing on operator capability prediction. If the theatrical component were removed, selection would become less exclusive but not less effective.
 *
 * DIRECTIONALITY LOGIC:
 *   Candidate soldiers: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction — no exit, full bearing of costs. Originating unit commanders: Victim + constrained → d≈0.82, f(d)≈1.25. High extraction — constrained by command structure, cannot refuse to deploy personnel. SOCOM: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.10. Net beneficiary — can deploy candidates to multiple conflicts, maintains institutional autonomy, can set selection standards unilaterally. Military personnel system: Victim + constrained → d≈0.70, f(d)≈1.05. Systemic extraction — constrained by force structure requirements, bears long-term PTSD/retention costs without compensation mechanism. Selection institution: Institutional + arbitrage → d≈0.15, f(d)≈0.05. Piton classification reflects that institution maintains ritual through inertia; directionality is low because institution has agency to reform protocols but chooses not to (arbitrage exit).
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CONFIRMATION: The mandatrophy is resolved by recognizing that Delta selection presents itself as pure coordination (identifying best operators) but functions as pure extraction (capturing 2-3 years of unpaid labor from 97-99% of candidates). The snare classification is confirmed across all victim-perspective readings: candidates, originating units, and the military personnel system all perceive the constraint as extraction-dominant. The only perspective that sees coordination (SOCOM) is the beneficiary perspective. The rising extractiveness (0.55→0.68) and rising theater (0.38→0.52) over 30 years indicate a drift away from functional filtering and toward rent-seeking legitimacy maintenance. The analytical observer's mountain is a false summit: while extreme filtering may be inherent to elite military selection in principle, the specific practices (deliberate humiliation, sleep deprivation rituals, psychological domination) are not inherent — they are institutional choices that persist because they serve legitimacy functions rather than selection functions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    selection_intensity_threshold,
    'What level of selection intensity is functionally necessary versus theatrically excessive for identifying operators capable of high-stakes performance?',
    'Longitudinal performance correlation: compare operators selected through harsh vs moderate selection protocols; track operational success, team integration, psychological health, retention rates',
    'If harsh selection correlates with superior performance: justifies current intensity as functional filtering (Rope). If correlation weak or negative: selection is extraction mechanism (Snare) masquerading as necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selection_intensity_threshold, empirical, 'Threshold between functional selection intensity and theatrical excess').

omega_variable(
    psychological_trauma_institutional_cost,
    'What is the true institutional cost of psychological trauma and PTSD incidence in originating units, relative to the operational benefit of successful candidates?',
    'Medical records analysis; unit morale metrics before/after selection deployments; retention data for traumatized non-selected candidates; operational casualty rates attributable to team degradation',
    'If trauma cost exceeds operational benefit: selection is extractive net-negative (Snare confirmed). If benefit exceeds cost: may justify classification shift toward Tangled Rope (mixed coordination/extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(psychological_trauma_institutional_cost, empirical, 'Institutional cost-benefit of psychological trauma from selection').

omega_variable(
    alternative_selection_sufficiency,
    'Could equally effective operators be identified through non-degrading selection protocols (cognitive assessment, situational judgment, peer evaluation, simulation)?',
    'Comparative selection study: implement alternative protocols on trial cohort; measure identification accuracy and downstream operator performance; compare attrition and psychological health outcomes',
    'If alternatives are sufficient: current harsh protocols are purely extractive (Snare confirmed, suppression justified as unnecessary). If alternatives fail: selection maintains functional necessity (shifts toward Tangled Rope or even Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_selection_sufficiency, empirical, 'Whether alternative selection methods can achieve equivalent outcomes').

omega_variable(
    institutional_legitimacy_dependence,
    'How much does Delta Force institutional legitimacy and recruitment depend on the reputation for extreme selection versus actual operational effectiveness?',
    'Narrative analysis of recruitment messaging; surveys of candidates on motivation factors; comparison of elite operators from less brutal selection processes (Australian SAS, British SAS); institutional prestige decoupling from selection brutality',
    'If legitimacy heavily depends on brutality reputation: selection is performative (theater ratio higher than 0.52). If legitimacy independent: selection can be reformed without institutional collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_legitimacy_dependence, conceptual, 'How institutional legitimacy depends on harsh selection reputation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(delta_force_selection_2026, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(delta_tr_t0, delta_force_selection_2026, theater_ratio, 0, 0.38).
narrative_ontology:measurement(delta_tr_t15, delta_force_selection_2026, theater_ratio, 15, 0.45).
narrative_ontology:measurement(delta_tr_t30, delta_force_selection_2026, theater_ratio, 30, 0.52).

% Extraction over time
narrative_ontology:measurement(delta_be_t0, delta_force_selection_2026, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(delta_be_t15, delta_force_selection_2026, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(delta_be_t30, delta_force_selection_2026, base_extractiveness, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(delta_force_selection_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(delta_force_selection_2026, military_recruitment_quality).
narrative_ontology:affects_constraint(delta_force_selection_2026, special_operations_institutional_autonomy).
narrative_ontology:affects_constraint(delta_force_selection_2026, unit_cohesion_retention).

% DUAL FORMULATION NOTE:
% Delta selection is downstream of the broader military institutional structure (recruitment, force structure, command authority) but represents a distinct extractive mechanism. The upstream constraints establish the context (how many soldiers are available, what command authority exists); Delta selection is the extractive lever that operates within that context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
