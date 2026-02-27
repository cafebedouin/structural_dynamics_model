% ============================================================================
% CONSTRAINT STORY: norm_erosion_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_norm_erosion_threshold, []).

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
 *   constraint_id: norm_erosion_threshold
 *   human_readable: The Social Fabric Breach
 *   domain: social/political
 *
 * SUMMARY:
 *   The social fabric breach models the non-linear collapse of cooperation
 *   when perceived defection crosses a critical threshold. This constraint
 *   exhibits the full spectrum of DR types across observer positions. At the
 *   individual compliance level, the erosion functions as a Snare:
 *   norm-compliant agents are trapped in deteriorating social environments
 *   with mounting enforcement costs and reputational damage. At the
 *   institutional enforcement level, the same phenomenon functions as a
 *   hybrid Rope-Snare dynamic (Tangled Rope): enforcement authorities benefit
 *   from selective enforcement and crisis management authority, while
 *   coordinating on maintaining shared expectations. Strategic defectors
 *   experience a Tangled Rope: they benefit from the cooperation
 *   infrastructure they selectively defect from, but face suppression through
 *   reputational and social mechanisms. Organized renewal movements see a
 *   Scaffold: the breach is a temporary coordination failure with recovery
 *   pathways through relationship repair and norm re-establishment.
 *   Ceremonial institutions see a Piton: formal rituals claiming to restore
 *   community bonds have degraded into performative theater. The analytical
 *   observer risks a false Mountain: framing the threshold as an immutable
 *   phase transition in social dynamics, when the breach is actually driven
 *   by contingent choices in enforcement selectivity, norm visibility, and
 *   institutional incentive structures.
 *
 * KEY AGENTS:
 *   - Norm-Compliant Majority: Primary victim (powerless/trapped) — bears escalating social friction, enforcement costs, and reputation damage as norms erode
 *   - Enforcement Authority: Primary beneficiary (institutional/arbitrage) — gains crisis authority, budget expansion, and legitimacy renewal from managing breach
 *   - Strategic Defector Subgroup: Secondary beneficiary (moderate/constrained) — extracts value from selective enforcement and reputation asymmetry while maintaining plausible deniability
 *   - Social Renewal Movement: Organized secondary actor (organized/constrained) — civic organizations and community leaders with agency to address root causes and restore cooperation
 *   - Ceremonial Community: Institutional secondary actor (institutional/arbitrage) — formal institutions whose norm-repair function has degraded into theater; maintain legitimacy through ritual
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional choices as inevitable system dynamics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(norm_erosion_threshold, 0.58).
domain_priors:suppression_score(norm_erosion_threshold, 0.68).
domain_priors:theater_ratio(norm_erosion_threshold, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(norm_erosion_threshold, extractiveness, 0.58).
narrative_ontology:constraint_metric(norm_erosion_threshold, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(norm_erosion_threshold, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(norm_erosion_threshold, tangled_rope).
narrative_ontology:human_readable(norm_erosion_threshold, "The Social Fabric Breach").
narrative_ontology:topic_domain(norm_erosion_threshold, "social/political").

domain_priors:requires_active_enforcement(norm_erosion_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(norm_erosion_threshold, defector_subgroup).
narrative_ontology:constraint_beneficiary(norm_erosion_threshold, institutional_enforcement_apparatus).
narrative_ontology:constraint_victim(norm_erosion_threshold, norm_compliant_majority).
narrative_ontology:constraint_victim(norm_erosion_threshold, collective_cooperation_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NORM-COMPLIANT MAJORITY (SNARE) — Trapped in deteriorating social environment. Cannot exit local community without catastrophic cost. Experiences high suppression as defectors face minimal enforcement while compliers bear reputation costs and social friction. d≈0.92, f(d)≈1.38, σ=0.8 → χ≈0.64.
constraint_indexing:constraint_classification(norm_erosion_threshold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ENFORCEMENT AUTHORITY (ROPE) — Institutional position with arbitrage exit (jurisdiction shopping, legitimacy trade-offs). Sees norm enforcement as coordination problem: maintaining shared expectations reduces enforcement burden. However, selective enforcement creates extraction opportunity. d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.06. Net beneficiary through institutional legitimacy preservation.
constraint_indexing:constraint_classification(norm_erosion_threshold, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: STRATEGIC DEFECTOR (TANGLED ROPE) — Constrained by social visibility and retaliation risk, but also benefits from cooperation infrastructure. Extraction derives from reputation asymmetry: can defect on low-visibility norms while maintaining compliance on high-surveillance norms. d≈0.65, f(d)≈0.95, σ=0.8 → χ≈0.44.
constraint_indexing:constraint_classification(norm_erosion_threshold, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: SOCIAL RENEWAL MOVEMENT (SCAFFOLD) — Organized agents (community leaders, civic organizations) see the norm erosion as a temporary coordination failure with a recovery pathway. Low effective extraction because they have agency and see a sunset: relationship repair, norm re-establishment, and trust-rebuilding programs. d≈0.35, f(d)≈0.30, σ=0.9 → χ≈0.16. Theater ratio low (0.45) because renewal is functional, not performative.
constraint_indexing:constraint_classification(norm_erosion_threshold, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: CEREMONIAL COMMUNITY (PITON) — Formal institutions (civic ceremonies, rituals, governance theatrics) that claim to reestablish community bonds but have degraded into performative expressions. Town halls, community meetings, and unity events persist through inertia despite low genuine norm-repair function. Theater_ratio in this subsystem approaches 0.70 as theatrical compliance replaces actual norm restoration.
constraint_indexing:constraint_classification(norm_erosion_threshold, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / CRITICAL THRESHOLD (MOUNTAIN ATTEMPT) — From a civilizational view, this perspective risks framing the norm erosion as an inevitable phase transition in social systems — a natural law of collective dynamics. 'Once defection crosses X%, cooperation collapses' appears as a system invariant. However, structural data (ε=0.58, suppression=0.68, theater=0.45) contradicts mountain requirements (ε≤0.25, suppression≤0.05). This is a false summit: the threshold is institutional and contingent, not natural.
constraint_indexing:constraint_classification(norm_erosion_threshold, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(norm_erosion_threshold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(norm_erosion_threshold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(norm_erosion_threshold, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(norm_erosion_threshold, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(norm_erosion_threshold, TR),
    TR >= 0.70.

:- end_tests(norm_erosion_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, showing escalation over interval (0.22 → 0.58). The constraint exhibits classic extraction signatures: selective enforcement targeting low-visibility defectors, enforcement apparatus growing faster than genuine norm repair, and institutional incentives favoring crisis management over resolution. The escalation reflects institutional capture — as the breach widens, enforcement authorities gain authority and resources, creating perverse incentives to maintain the crisis. Suppression (0.68): High. Multiple barriers prevent norm restoration: visibility of defection creates conformity pressure (not informational but coercive), retaliation risk silences coordination discussion, institutional inflexibility prevents norm adaptation, and enforcement selectivity prevents transparent resolution. The suppression is structural, not emerging from the defection directly. Theater ratio (0.45): Moderate-low. The system still has functional elements (some genuine enforcement, some authentic relationship repair), but performance increasingly substitutes for function as ceremonial institutions expand (town halls, unity events, consensus-building) without addressing underlying incentive misalignment. The trajectory shows escalation (0.25 → 0.45), indicating growing performative content as functional solutions fail.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The norm-compliant majority sees pure extraction (Snare) — they cannot exit and cannot coordinate effective response. The enforcement authority sees coordination (Rope) and benefit (arbitrage exit) — they are 'solving' the breach by managing it. The strategic defector sees mixed benefit (Tangled Rope) — the breach creates opportunity (lower enforcement of low-visibility norms) while depending on remaining cooperation infrastructure. The renewal movement sees a solvable temporary problem (Scaffold) — they perceive agency and pathways to restoration. The ceremonial community sees their own degraded function (Piton) — they recognize that rituals have lost substance. The analytical observer risks seeing inevitability (false Mountain) — framing the breach as an immutable threshold effect when it is actually driven by institutional choices. These gaps indicate the constraint is authentically Tangled Rope: it has genuine coordination function (maintaining some shared norms) and genuine extraction mechanism (selective enforcement, crisis management authority). The mandatrophy is resolved by showing that all perspectives are structurally valid — the perspectival gap is the data.
 *
 * DIRECTIONALITY LOGIC:
 *   Norm-compliant majority: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. Cannot exit local social context without catastrophic cost. Enforcement authority: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary through institutional legitimacy and crisis authority. Strategic defector: Victim + constrained → d≈0.65, f(d)≈0.95 (constrained by visibility and retaliation but able to defect on low-surveillance norms). Moderate extraction. Social renewal movement: Organized + constrained → d≈0.35, f(d)≈0.30. Low effective extraction because they have agency and can initiate relationship repair. Ceremonial community: Institutional + arbitrage → d≈0.08, f(d)≈-0.11. Appears as net beneficiary but is actually victim of its own degradation (Piton classification comes from theater gate at 0.70 threshold, not from high chi). Analytical observer: analytical → d≈0.72, f(d)≈1.15. The high d comes from observer position viewing the system from outside; the mountain classification is perspectival and reveals a false summit when checked against base metrics.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint resolves the mandatrophy by showing that the same structural phenomenon (norm erosion) genuinely exhibits both coordination function AND asymmetric extraction. Coordination function: maintaining shared norms is a public good that reduces enforcement burden and enables collective cooperation. Extraction mechanism: selective enforcement of defectors and institutional expansion during crisis create benefits for authorities and strategic defectors that depend on continued breach. Both are real. The perspectival gap is not an observational artifact — it is the actual structure. Norm-compliant majority experiences pure extraction (Snare) because they have no exit and no enforcement discretion. Enforcement authority experiences coordination (Rope) because they have arbirtrage-level exit and enforcement authority. Neither is 'correct' — both are true from their positions. The constraint is Tangled Rope because it cannot be classified as either pure coordination or pure extraction without losing structural accuracy. The mandatrophy is resolved by the 6-perspective presheaf: showing that all types are legitimate readings prevents conflation of perspectives while maintaining classification precision.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    critical_defection_threshold,
    'What is the precise defection rate at which cooperation cascades to collapse versus stable mixed equilibrium?',
    'Longitudinal measurement of defection prevalence and cooperation indicators (trust surveys, public goods contributions, enforcement incidence); identification of inflection point in cooperation trajectory',
    'If threshold ≤10%: small defector subgroup triggers collapse (Mountain-adjacent: systemic fragility is inherent). If threshold ≥30%: substantial defection absorbed without cascade (Snare escalation mechanism is institutional choice, not structural inevitability).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(critical_defection_threshold, empirical, 'The critical defection rate at which cooperation cascades').

omega_variable(
    enforcement_selectivity,
    'Is selective enforcement of defectors a necessary feature of the institutional structure, or a contingent choice by enforcement authorities?',
    'Comparative analysis of communities with uniform enforcement vs selective enforcement; measurement of defection rates and norm stability under each regime',
    'If necessary: norm erosion is inherent cost of stratified enforcement (Snare becomes Rope if enforcement becomes uniform). If contingent: selective enforcement is extraction mechanism driving breach (classification stays Tangled Rope but with higher chi).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_selectivity, empirical, 'Whether enforcement selectivity is structural or contingent').

omega_variable(
    visibility_feedback_loop,
    'Does perceived defection rate drive actual defection rate (positive feedback), or do they remain decoupled?',
    'Experimental manipulation of visibility signals (publicizing defection rates vs hiding them); measurement of actual defection response',
    'If coupled: the threshold is a self-fulfilling prophecy (coordination failure, addressable by Scaffold-type interventions). If decoupled: the threshold is an attractor in social dynamics (closer to Mountain, immutable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(visibility_feedback_loop, empirical, 'Whether perceived defection rate drives actual defection through feedback').

omega_variable(
    institutional_capture_of_enforcement,
    'To what extent does the enforcement apparatus extract value from the norm erosion crisis rather than resolve it?',
    'Measurement of enforcement apparatus growth/budget during crisis periods; analysis of enforcement outcomes (actual norm repair vs symbolic enforcement); comparison with communities where enforcement is decentralized or informal',
    'If high capture: the constraint is primarily Snare (enforcement profits from crisis). If low capture: the constraint is primarily Tangled Rope (coordination with unavoidable asymmetry).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_of_enforcement, empirical, 'Degree of institutional capture of enforcement apparatus').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(norm_erosion_threshold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(norm_tr_t0, norm_erosion_threshold, theater_ratio, 0, 0.25).
narrative_ontology:measurement(norm_tr_t5, norm_erosion_threshold, theater_ratio, 5, 0.38).
narrative_ontology:measurement(norm_tr_t10, norm_erosion_threshold, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(norm_be_t0, norm_erosion_threshold, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(norm_be_t5, norm_erosion_threshold, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(norm_be_t10, norm_erosion_threshold, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(norm_erosion_threshold, enforcement_mechanism).
narrative_ontology:affects_constraint(norm_erosion_threshold, institutional_capture).
narrative_ontology:affects_constraint(norm_erosion_threshold, trust_collapse_cascade).
narrative_ontology:affects_constraint(norm_erosion_threshold, selective_enforcement_incentive).

% DUAL FORMULATION NOTE:
% The norm erosion threshold decomposes into two structurally distinct constraints: (1) the empirical tipping point in cooperation dynamics (ε≈0.15, closer to Mountain if threshold is truly phase-transitive) and (2) the institutional extraction mechanism that converts the empirical tipping point into asymmetric enforcement authority (ε≈0.58, Tangled Rope). This story focuses on the second. The first is downstream in the network.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(norm_erosion_threshold, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
