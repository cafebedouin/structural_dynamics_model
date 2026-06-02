% ============================================================================
% CONSTRAINT STORY: irgc_proxy_funding_constraints
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_irgc_proxy_funding_constraints, []).

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
 *   constraint_id: irgc_proxy_funding_constraints
 *   human_readable: IRGC Proxy Funding Constraints in Middle Eastern Geopolitics
 *   domain: geopolitical/military/financial
 *
 * SUMMARY:
 *   The IRGC proxy funding constraint is a structural mechanism enabling
 *   Iran's regional military projection while generating severe costs for
 *   foot soldiers and civilian populations. The constraint functions as a
 *   snare: it extracts labor, loyalty, and life-risk from trapped agents
 *   (proxy foot soldiers) while concentrating benefits to IRGC leadership.
 *   The extraction is sustained through high suppression (coercion, identity
 *   fusion, economic desperation) and accompanied by significant theater —
 *   international sanctions compliance rituals that impose costs but fail to
 *   materially disrupt funding flows. The constraint's extractiveness has
 *   increased over the 20-year observation interval (0.42 → 0.68) as IRGC has
 *   systematized proxy networks and expanded operational scope. Theater has
 *   also increased (0.35 → 0.58) as sanctions enforcement infrastructure has
 *   grown without corresponding effectiveness in blocking actual funding
 *   transfers. This is the signature of a maturing snare: extraction becomes
 *   more efficient while theater masks the constraint's true mechanism.
 *
 * KEY AGENTS:
 *   - IRGC Leadership: Primary beneficiary (institutional/arbitrage) — captures regional military reach, strategic flexibility, and resource flows; experiences constraint as coordination mechanism for force projection
 *   - Proxy Group Field Commanders: Secondary beneficiary (moderate/constrained) — receive command authority and resource allocation; constrained by IRGC oversight and retaliation risk
 *   - Proxy Foot Soldiers: Primary victim (powerless/trapped) — bear labor extraction, loyalty demands, and life risk; exit blocked by coercion, economic desperation, and identity fusion
 *   - Civilian Populations in Conflict Zones: Secondary victim (powerless/trapped) — experience displacement, casualties, and economic disruption with no participation in decisions driving conflict
 *   - International Sanctions Regime: Institutional actor attempting enforcement (institutional/constrained) — maintains theater compliance but limited material impact; degraded mechanism persists through inertia
 *   - Analytical Observer: Sees structural snare mechanism (analytical/analytical) — recognizes extraction asymmetry despite sanctions theater
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irgc_proxy_funding_constraints, 0.68).
domain_priors:suppression_score(irgc_proxy_funding_constraints, 0.72).
domain_priors:theater_ratio(irgc_proxy_funding_constraints, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irgc_proxy_funding_constraints, extractiveness, 0.68).
narrative_ontology:constraint_metric(irgc_proxy_funding_constraints, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(irgc_proxy_funding_constraints, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irgc_proxy_funding_constraints, snare).
narrative_ontology:human_readable(irgc_proxy_funding_constraints, "IRGC Proxy Funding Constraints in Middle Eastern Geopolitics").
narrative_ontology:topic_domain(irgc_proxy_funding_constraints, "geopolitical/military/financial").

domain_priors:requires_active_enforcement(irgc_proxy_funding_constraints).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irgc_proxy_funding_constraints, irgc_leadership).
narrative_ontology:constraint_beneficiary(irgc_proxy_funding_constraints, proxy_group_commanders).
narrative_ontology:constraint_victim(irgc_proxy_funding_constraints, proxy_foot_soldiers).
narrative_ontology:constraint_victim(irgc_proxy_funding_constraints, civilian_populations_in_conflict_zones).
narrative_ontology:constraint_victim(irgc_proxy_funding_constraints, regional_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROXY FOOT SOLDIER (SNARE) — Enlisted into proxy militias through economic desperation, family pressure, or coercion. Exit carries death penalty in many contexts or permanent family ostracization. No meaningful alternatives. Experiences maximum extraction: labor, loyalty, life risk, and ideological indoctrination. Trapped both materially and identity-locked through militant socialization.
constraint_indexing:constraint_classification(irgc_proxy_funding_constraints, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: CIVILIAN POPULATIONS IN CONFLICT ZONES (SNARE) — Bear costs of proxy warfare without participating in decision-making. Face displacement, property destruction, casualties, and psychological trauma. No exit option except migration, which is itself costly and dangerous. Suppression through inability to organize or influence proxy group behavior. Zero direct benefit from the funding constraint.
constraint_indexing:constraint_classification(irgc_proxy_funding_constraints, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: PROXY GROUP FIELD COMMANDERS (TANGLED ROPE) — Benefit from funding and command authority but constrained by IRGC oversight and the requirement to execute IRGC strategic directives. Have some negotiating power over resource allocation and tactical autonomy, but cannot exit IRGC control without losing funding and facing retaliation. Mixed extraction: some authority and resource capture, but asymmetric extraction by IRGC overseers.
constraint_indexing:constraint_classification(irgc_proxy_funding_constraints, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: IRGC STRATEGIC LEADERSHIP (ROPE) — Primary beneficiary. Funding channel enables regional military projection, denial of adversary advantage, and plausible deniability for state action. Experiences the constraint as a coordination mechanism: distributing funds to proxy groups solves the problem of extending military reach beyond Iran's borders while maintaining strategic flexibility. Net beneficiary with low experienced extraction.
constraint_indexing:constraint_classification(irgc_proxy_funding_constraints, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: INTERNATIONAL SANCTIONS ENFORCEMENT REGIME (PITON) — Created to prevent financial flows to IRGC and proxy groups, but the constraint (IRGC proxy funding) persists despite decades of sanctions. Theater ratio is high: significant sanctions compliance theater (reporting requirements, financial monitoring, asset freezes) yields limited material impact on actual funding flows. The enforcement mechanism is degraded — maintained through institutional inertia and political signaling rather than effective operational disruption.
constraint_indexing:constraint_classification(irgc_proxy_funding_constraints, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a civilizational perspective, the funding constraint exhibits all the structural properties of a snare: high extractiveness from trapped agents, high suppression preventing alternatives, high benefit concentration to IRGC leadership, and effective extraction running from foot soldiers and civilians toward military leadership. The theater component (sanctions compliance theater) does not obscure the core extraction mechanism.
constraint_indexing:constraint_classification(irgc_proxy_funding_constraints, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irgc_proxy_funding_constraints_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(irgc_proxy_funding_constraints, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(irgc_proxy_funding_constraints, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(irgc_proxy_funding_constraints, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(irgc_proxy_funding_constraints, TR),
    TR >= 0.70.

:- end_tests(irgc_proxy_funding_constraints_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. The IRGC captures sustained resource flows and strategic benefit from proxy networks while distributing costs to foot soldiers and civilians. The value reflects both direct extraction (resource concentration) and indirect extraction (opportunity cost to regional stability and civilian welfare). The trajectory shows extraction intensification over 20 years as proxy networks have become more systematized and operationally effective. Suppression (0.72): Very high. Multiple suppression mechanisms operate: (1) Material — economic desperation in source regions leaves foot soldiers no viable alternatives; (2) Coercive — proxy groups enforce exit penalties through violence; (3) Cognitive — identity socialization in militias makes exit psychologically unthinkable; (4) Political — civilian populations cannot organize or exit conflict zones. Theater ratio (0.58): Moderate-high. Sanctions compliance theater is substantial but not total. Significant real costs are imposed (asset freezes, financial monitoring, sanctions-related black market pricing premiums) but actual funding flows continue through alternative channels. The theater has increased as sanctions enforcement infrastructure has grown, suggesting that theater itself is becoming part of the extraction mechanism — sanctions justify continued IRGC militarization and hardening against external pressure.
 *
 * PERSPECTIVAL GAP:
 *   IRGC leadership sees rope: the funding mechanism solves their strategic problem (regional projection without direct state involvement). Field commanders see tangled rope: genuine command authority and resource benefits combined with IRGC control and retaliation risk. Foot soldiers see snare: coercion, identity lock, and no exit. Civilians see snare: costs without agency. Sanctions regime sees piton: degraded enforcement mechanism maintained through institutional inertia. The analytical observer sees snare: high extraction, high suppression, high beneficiary concentration. The largest gap is between IRGC leadership (rope) and foot soldiers (snare) — same constraint, opposite experiences, driven by directionality differences (leadership as beneficiary with arbitrage options → low d → low χ; foot soldiers as trapped victims → high d → high χ).
 *
 * DIRECTIONALITY LOGIC:
 *   IRGC leadership as beneficiary with institutional power and arbitrage options: derives low directionality (d ≈ 0.10), resulting in negative effective extraction — they perceive the constraint as beneficial coordination. Field commanders as moderate-power agents with constrained exit: derive moderate directionality (d ≈ 0.55), resulting in moderate effective extraction χ ≈ 0.40. Foot soldiers as powerless agents with trapped exit: derive high directionality (d ≈ 0.95), resulting in high effective extraction χ ≈ 0.96 (capped at 1.0 by sigmoid). Civilians as external victims with trapped exit and no organizational capacity: derive maximum directionality (d ≈ 1.0), resulting in maximum experienced extraction. The constraint's core mechanism is directionality-driven: the same funding flow that benefits IRGC leadership (negative extraction) extracts heavily from foot soldiers (high extraction). The asymmetry is not in the constraint's structure but in who occupies which structural position relative to the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that IRGC proxy funding is fundamentally a snare, not a tangled rope. The presence of coordination benefits (IRGC and commanders both benefit from the arrangement) does not make it a tangled rope if those benefits flow to the already-powerful and costs fall on the powerless. Tangled rope requires (1) coordination function serving some genuine problem, (2) asymmetric extraction, AND (3) both coordination and extraction visible from at least one perspective. IRGC sees pure coordination (rope). But this is a beneficiary's perspective. From the foot soldier perspective, the constraint is pure extraction with no coordination benefit — they do not benefit from IRGC regional projection; they bear costs. The definition of tangled rope requires BOTH conditions visible from the SAME perspective (the moderate agent's view). Field commanders see both: genuine coordination (IRGC strategy enables their command role) AND extraction (IRGC control, retaliation risk). This justifies tangled rope for the field commander perspective. But for IRGC leadership and foot soldiers, the constraint classifies as rope (beneficiary) and snare (victim) respectively. The snare classification is correct when the constraint is judged from its aggregate structural impact: the primary mechanism is extraction from the powerless to the powerful, with coordination benefits secondary and distributed only to already-powerful actors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_militant_recruitment_viability,
    'What proportion of proxy foot soldiers are recruited through economic necessity versus ideological commitment versus coercion?',
    'Defector interviews, recruitment network analysis, economic opportunity studies in source regions',
    'If majority economic necessity: constraint is pure snare, exit barrier is material. If majority ideological: some agents are identity_locked, classification shifts toward complex identity-based suppression. If majority coercive: suppression is higher than current assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_militant_recruitment_viability, empirical, 'Recruitment mechanism decomposition: economic vs ideological vs coercive').

omega_variable(
    sanctions_effectiveness_actual_vs_theater,
    'What percentage of actual IRGC funding flows are blocked by sanctions vs. successfully routed through alternative channels (shell companies, cash couriers, cryptocurrency, hawala)?',
    'Financial forensics, intelligence agency estimates, blockchain analysis, courier network mapping',
    'If >70% successfully routed: piton classification confirmed — theater is very high. If <30% successfully routed: sanctions have material bite, classification shifts toward higher effectiveness of enforcement regime.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sanctions_effectiveness_actual_vs_theater, empirical, 'Sanctions implementation gap: intended vs actual blockage').

omega_variable(
    proxy_group_exit_cost_structure,
    'What are the material and social costs for a proxy group commander to defect or cease operations without IRGC permission?',
    'Historical analysis of defections, retaliation patterns, intelligence reports on post-defection outcomes',
    'If costs are extreme (death, family retaliation, property seizure): exit_options must be trapped, not constrained. If costs are moderate (loss of funding, social ostracization): constrained is correct. Determines if field commanders experience tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_group_exit_cost_structure, empirical, 'Exit cost structure for proxy group commanders').

omega_variable(
    identity_lock_depth_in_militias,
    'For foot soldiers socialized into proxy militias from adolescence, is the binding mechanism primarily external (cannot leave due to material/coercive barriers) or cognitive (identity fused with militant role, cannot imagine exit)?',
    'Psychosocial assessment of defectors, longitudinal identity studies, exit motivation analysis',
    'If primarily external: trapped is correct. If primarily cognitive: identity_locked better captures the suppression mechanism — agent carries suppression with them even after external barriers are removed. Affects post-conflict reintegration strategy assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_depth_in_militias, conceptual, 'Identity fusion vs external barrier mechanisms in foot soldier suppression').

omega_variable(
    regional_stability_aggregate_cost,
    'What is the total regional cost (displaced persons, economic disruption, security fragmentation, institutional erosion) attributable to IRGC proxy funding constraints, and what proportion falls on uncompensated civilians?',
    'Conflict cost accounting, humanitarian impact studies, economic impact assessments, institutional capacity analysis',
    'Determines whether the victim designation ''civilian_populations_in_conflict_zones'' is accurate and whether extractiveness value of 0.68 captures the true asymmetry of costs borne by the powerless.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_stability_aggregate_cost, empirical, 'Aggregate civilian cost and distribution analysis').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irgc_proxy_funding_constraints, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irgc_tr_t0, irgc_proxy_funding_constraints, theater_ratio, 0, 0.35).
narrative_ontology:measurement(irgc_tr_t10, irgc_proxy_funding_constraints, theater_ratio, 10, 0.48).
narrative_ontology:measurement(irgc_tr_t20, irgc_proxy_funding_constraints, theater_ratio, 20, 0.58).
narrative_ontology:measurement(irgc_tr_t5, irgc_proxy_funding_constraints, theater_ratio, 5, 0.42).

% Extraction over time
narrative_ontology:measurement(irgc_be_t0, irgc_proxy_funding_constraints, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(irgc_be_t10, irgc_proxy_funding_constraints, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(irgc_be_t20, irgc_proxy_funding_constraints, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(irgc_be_t5, irgc_proxy_funding_constraints, base_extractiveness, 5, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irgc_proxy_funding_constraints, enforcement_mechanism).
narrative_ontology:affects_constraint(irgc_proxy_funding_constraints, sanctions_regime_architecture).
narrative_ontology:affects_constraint(irgc_proxy_funding_constraints, regional_proxy_rivalry_dynamics).
narrative_ontology:affects_constraint(irgc_proxy_funding_constraints, iranian_military_innovation_capacity).

% DUAL FORMULATION NOTE:
% IRGC proxy funding is upstream of specific regional conflicts (Syria, Iraq, Yemen, Lebanon) and downstream of Iranian strategic doctrine. This story models the funding constraint itself; specific proxy group operations and regional conflicts constitute separate stories with their own extractiveness values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
