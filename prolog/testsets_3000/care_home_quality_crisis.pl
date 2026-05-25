% ============================================================================
% CONSTRAINT STORY: care_home_quality_crisis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_care_home_quality_crisis, []).

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
 *   constraint_id: care_home_quality_crisis
 *   human_readable: Care Home Quality Crisis and Resident Extraction
 *   domain: healthcare/elder_care/social_welfare
 *
 * SUMMARY:
 *   The care home quality crisis represents a structural tension between the
 *   coordination function of elder care provisioning and the extraction
 *   incentives created by financialization and regulatory capture. Elderly
 *   residents require 24/7 care that families cannot provide independently;
 *   care homes coordinate this essential service. However, the constraint
 *   exhibits strong snare dynamics: residents are trapped by physical and
 *   legal dependency; families are trapped by lack of alternatives; workers
 *   are trapped by economic dependency and wage stagnation; operators extract
 *   through understaffing, fee escalation, and cost-cutting. The constraint's
 *   extractiveness (0.68) and suppression (0.72) have increased over the
 *   measurement interval as private equity consolidation intensifies
 *   cost-cutting and regulatory inspection becomes increasingly performative
 *   (theater_ratio rising from 0.48 to 0.65). This constraint demonstrates
 *   the transformation of a coordination mechanism into an extraction regime
 *   through profit-maximization incentives and regulatory capture.
 *
 * KEY AGENTS:
 *   - Elderly Residents: Primary victims (powerless/trapped) — physically and legally dependent; no exit options; bear full extraction through understaffing, neglect, dignity loss
 *   - Care Workers: Secondary victims (moderate/constrained) — economically dependent; constrained by wage stagnation and lack of alternatives; bear extraction through labor exploitation
 *   - Families: Tertiary victims (moderate/constrained) — constrained by lack of alternatives; experience mixed coordination (care is provided) and extraction (fees, information asymmetry, emotional manipulation)
 *   - Care Home Operators: Primary beneficiaries (institutional/arbitrage) — multiple exit options enable cost externalization; perceive constraint as coordination; arbitrage options minimize experienced extraction
 *   - Private Equity Owners: Secondary beneficiaries (institutional/arbitrage) — debt-leverage model extracts value; multiple portfolio exits available; maximize extraction through understaffing and fee increases
 *   - Regulatory Inspectors: Institutional actor (powerful/mobile) — inspection system is performative; see their own role as degraded (piton perspective)
 *   - Advocacy Coalition: Organized agents (organized/constrained) — politically constrained by regulatory capture; see problem as solvable but face structural barriers to policy change
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(care_home_quality_crisis, 0.68).
domain_priors:suppression_score(care_home_quality_crisis, 0.72).
domain_priors:theater_ratio(care_home_quality_crisis, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(care_home_quality_crisis, extractiveness, 0.68).
narrative_ontology:constraint_metric(care_home_quality_crisis, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(care_home_quality_crisis, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(care_home_quality_crisis, snare).
narrative_ontology:human_readable(care_home_quality_crisis, "Care Home Quality Crisis and Resident Extraction").
narrative_ontology:topic_domain(care_home_quality_crisis, "healthcare/elder_care/social_welfare").

domain_priors:requires_active_enforcement(care_home_quality_crisis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(care_home_quality_crisis, care_home_operators).
narrative_ontology:constraint_beneficiary(care_home_quality_crisis, private_equity_owners).
narrative_ontology:constraint_beneficiary(care_home_quality_crisis, management_consultants).
narrative_ontology:constraint_victim(care_home_quality_crisis, elderly_residents).
narrative_ontology:constraint_victim(care_home_quality_crisis, frontline_care_workers).
narrative_ontology:constraint_victim(care_home_quality_crisis, families_of_residents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ELDERLY RESIDENT (SNARE) — Physically, cognitively, and legally trapped in the facility. No viable exit: alternatives cost more or don't exist in their region. Bears full extraction: understaffing means poor nutrition, missed medications, neglect of dignity. Maximum suppression enforced by physical dependency and cognitive decline. The constraint extracts time, health, and dignity.
constraint_indexing:constraint_classification(care_home_quality_crisis, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CARE WORKER (SNARE) — Constrained by economic dependency and lack of alternative employment. Works understaffed shifts under time pressure, bearing emotional labor and stress. Suppression high: wage stagnation, no union protection in many regions, threat of termination. Benefits minimally from coordination function; experiences extraction through exploitation of their labor scarcity.
constraint_indexing:constraint_classification(care_home_quality_crisis, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FAMILY MEMBER (TANGLED ROPE) — Constrained by lack of alternatives (no parent in the home, long-term care facility unavoidable). Experiences genuine coordination: the facility coordinates care tasks that the family cannot provide. But also experiences extraction: fees rise faster than quality, information asymmetry prevents monitoring, emotional manipulation (facility controls resident contact) creates dependency.
constraint_indexing:constraint_classification(care_home_quality_crisis, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CARE HOME OPERATOR (ROPE) — Institutional actor with multiple exit options: relocate to higher-reimbursement regions, convert to private-pay model, exit to real estate speculation, sell to private equity. Perceives the constraint as coordination: standardizing care protocols, managing regulatory compliance, filling beds efficiently. Arbitrage options mean effective extraction is scaled downward relative to structural position.
constraint_indexing:constraint_classification(care_home_quality_crisis, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY INSPECTOR (PITON) — Inspections are largely performative: facilities improve temporarily before inspection, revert afterward. Theater ratio high (0.65): compliance checklists are satisfied without improvement in actual resident outcomes. The regulatory system persists through institutional inertia (licensure is required) but has lost functional connection to quality assurance. Inspectors see their own role as degraded.
constraint_indexing:constraint_classification(care_home_quality_crisis, piton,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ADVOCACY COALITION (TANGLED ROPE) — Family councils, worker unions, elder rights organizations see the crisis as solvable through policy reform: mandatory staffing ratios, wage floors, independent ombudsmen. Organized agents with real political voice but constrained by regulatory capture (operators lobby aggressively) and political economy (elder care is low-priority spending). Experience mixed: genuine coordination function (advocacies create accountability pressure) combined with asymmetric extraction (their labor is volunteer-subsidized).
constraint_indexing:constraint_classification(care_home_quality_crisis, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN) — From a civilizational view, aging populations require care infrastructure, and infrastructure has costs; some tension between quality and cost is inherent. This framing naturalizes profit motive as inevitable. However, the structural data contradicts the mountain classification — the crisis is not inherent to care provisioning but to financialization and regulatory capture. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(care_home_quality_crisis, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(care_home_quality_crisis_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(care_home_quality_crisis, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(care_home_quality_crisis, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(care_home_quality_crisis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(care_home_quality_crisis, TR),
    TR >= 0.70.

:- end_tests(care_home_quality_crisis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. The trend from 0.42 to 0.68 over the measurement interval reflects intensifying cost-externalization as private equity consolidation increases leverage and cash flow extraction. Suppressions (0.72): Correspondingly high. Trapped elderly have absolute dependency; workers face wage stagnation and threat of termination; families face information asymmetry and regulatory capture preventing exit to quality alternatives. Theater ratio (0.65): Moderate-high and increasing. Regulatory inspections are largely performative — facilities can temporarily meet compliance standards without improving actual resident care (nutrition, medication accuracy, dignity preservation). The ratio has increased as the gap between compliance theater and actual resident outcomes has widened. The measurement trajectory shows systematic degradation: extractiveness and theater both rising, indicating the constraint is becoming more extractive and more performative simultaneously.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence: residents see snare (pure extraction with no coordination benefit); operators see rope (pure coordination with incidental profit); families see tangled_rope (mixed coordination and extraction); workers see snare (extraction through labor scarcity); advocacy sees tangled_rope (coordination plus solvable asymmetry); regulator sees piton (performative ritual divorced from function); analyst sees false mountain (naturalizing contingent financial arrangements). This divergence reflects the extreme asymmetry in exit options and the use of dependent relationships to extract.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from structural position: residents and workers are victims with trapped/constrained exit → high d → high f(d) → experience maximum extraction; operators are beneficiaries with arbitrage exit → low d → low f(d) → experience minimal effective extraction despite extracting much. The asymmetry is structural: the same institutional arrangement extracts heavily from trapped agents and extracts lightly from mobile agents. For the advocacy coalition: organized agents with constrained exit (regulatory capture limits their political options even though they have organizational capacity) → moderate d → moderate f(d) → experience real but not maximal extraction. For the regulator: institutional power but mobile exit (can shift roles) combined with analytical distance (sees process as performative) → low-moderate d → sees constraints as solvable rather than binding.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by disaggregating the constraint family: the care home system contains two structurally distinct constraints. First: essential care coordination (ε ≈ 0.10, pure rope) — coordinating 24/7 care across multiple residents genuinely solves a collective action problem with minimal coercion. Second: profit extraction through understaffing and regulatory capture (ε ≈ 0.68, pure snare) — the extraction mechanism depends entirely on suppression of alternatives and exit options. These should be decomposed into separate stories. The combined view (current snare classification) is correct for the empirical state but obscures the structural decomposition. A post-reform scenario would see the coordination function persist at ε ≈ 0.10 (rope, solved through public/nonprofit provision) while the extraction regime would be eliminated. The snare classification correctly identifies that currently extraction dominates, but should be paired with network links to the underlying coordination constraint and the profit extraction constraint separately.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inherent_versus_structural_quality_cost,
    'Is care home quality degradation inherent to the economics of elder care, or is it a contingent result of profit-maximization incentives and regulatory capture?',
    'Cross-national comparison: jurisdictions with public or not-for-profit care sector dominance; correlation between operator profit margins and measured quality metrics (staffing, wound care, medication accuracy)',
    'If inherent: quality crisis is mountain (immutable constraint). If contingent: crisis is tangled_rope (solvable through policy reform).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inherent_versus_structural_quality_cost, empirical, 'Whether quality degradation is inherent to care economics or contingent on profit incentives').

omega_variable(
    regulatory_capture_mechanism,
    'Do care home operators successfully lobby for regulatory leniency, or does regulation reflect genuine cost-benefit tradeoffs?',
    'Lobbying expenditure tracking; correlation between operator contributions to politicians and subsequent regulatory changes; analysis of inspection failure rates vs documented violations in subsequent audits',
    'If capture is strong: regulatory system is piton (degraded and inert). If capture is weak: regulation could improve quality via enforcement tightening.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_mechanism, empirical, 'Strength of regulatory capture by care home operators').

omega_variable(
    worker_exit_threshold,
    'At what wage and staffing level would care worker exit rates drop below turnover that destabilizes resident care?',
    'Comparative analysis of staffing ratios and worker tenure across high-wage and low-wage jurisdictions; correlation between wage increases and stability metrics',
    'If threshold is achievable: worker constraint is constrained→mobile (policy intervention could unlock mobility). If threshold is economically unviable: worker constraint is trapped (structural immobility).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(worker_exit_threshold, empirical, 'Wage level required to stabilize care worker retention').

omega_variable(
    private_equity_extraction_mechanism,
    'Are private equity acquisitions in care increasing extraction rates through debt-leveraging and cost-cutting, or do they improve efficiency and redirect savings to residents?',
    'Longitudinal tracking of quality metrics pre and post PE acquisition; debt load analysis; comparison of fee structures and staffing levels before and after takeover',
    'If extraction increases: PE model is pure snare for residents. If efficiency gains materialize: model could be rope (coordination with incidental profits).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(private_equity_extraction_mechanism, empirical, 'Whether private equity acquisitions increase extraction or improve efficiency').

omega_variable(
    alternative_care_model_viability,
    'Do public, cooperative, or not-for-profit care models produce measurably higher quality at similar cost, or do they face different trade-offs?',
    'Matched comparison of quality metrics, cost per resident, worker wages, and sustainability across public/nonprofit vs private operators in same region',
    'If alternatives work: current snare classification reflects policy failure, not necessity. If alternatives have different but not better trade-offs: constraint is genuine tangled_rope with no escape route.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_care_model_viability, empirical, 'Whether alternative ownership models achieve better quality-cost trade-offs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(care_home_quality_crisis, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(careh_tr_t0, care_home_quality_crisis, theater_ratio, 0, 0.48).
narrative_ontology:measurement(careh_tr_t5, care_home_quality_crisis, theater_ratio, 5, 0.58).
narrative_ontology:measurement(careh_tr_t10, care_home_quality_crisis, theater_ratio, 10, 0.65).
narrative_ontology:measurement(careh_tr_t3, care_home_quality_crisis, theater_ratio, 3, 0.52).
narrative_ontology:measurement(careh_tr_t8, care_home_quality_crisis, theater_ratio, 8, 0.62).

% Extraction over time
narrative_ontology:measurement(careh_be_t0, care_home_quality_crisis, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(careh_be_t5, care_home_quality_crisis, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(careh_be_t10, care_home_quality_crisis, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(careh_be_t2, care_home_quality_crisis, base_extractiveness, 2, 0.5).
narrative_ontology:measurement(careh_be_t7, care_home_quality_crisis, base_extractiveness, 7, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(care_home_quality_crisis, resource_allocation).
narrative_ontology:boltzmann_floor_override(care_home_quality_crisis, 0.18).
narrative_ontology:affects_constraint(care_home_quality_crisis, elder_care_resource_scarcity).
narrative_ontology:affects_constraint(care_home_quality_crisis, wage_suppression_in_care_sectors).
narrative_ontology:affects_constraint(care_home_quality_crisis, regulatory_capture_in_healthcare).

% DUAL FORMULATION NOTE:
% The care home quality crisis should be decomposed into: (1) care_coordination_function (ε≈0.10, rope) — essential scheduling and logistics coordination. (2) profit_extraction_through_understaffing (ε≈0.68, snare) — financial mechanism. The current story represents the empirical combined state but obscures the structural decomposition. The network links to elder_care_resource_scarcity (upstream, affects availability of alternative placements), wage_suppression_in_care_sectors (coupled mechanism, both cause and effect), and regulatory_capture_in_healthcare (enables suppression of exit options).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(care_home_quality_crisis, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
