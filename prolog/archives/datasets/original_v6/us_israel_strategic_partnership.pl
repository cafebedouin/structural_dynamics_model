% ============================================================================
% CONSTRAINT STORY: us_israel_strategic_partnership
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_israel_strategic_partnership, []).

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
 *   constraint_id: us_israel_strategic_partnership
 *   human_readable: US-Israel Strategic Partnership Framework
 *   domain: geopolitics/security/alliance
 *
 * SUMMARY:
 *   The US-Israel strategic partnership represents a hybrid
 *   coordination-extraction constraint spanning military cooperation,
 *   intelligence sharing, technology development, and regional security
 *   architecture. Formalized progressively from the 1960s onward and
 *   intensifying after 1973, the partnership exhibits genuine coordination
 *   functions (mutual security interests, shared intelligence on regional
 *   threats, technology development, forward military positioning) alongside
 *   systematic extraction (Palestinian populations trapped in asymmetric
 *   security dependency, regional polarization that suppresses diplomatic
 *   alternatives, extraction of US diplomatic flexibility in Middle East
 *   policy, institutional capture of US decision-making through lobbying and
 *   alliance identification). The partnership's theater ratio (0.55) reflects
 *   that justificatory narratives (shared democratic values, strategic
 *   alignment, regional stability) require continuous maintenance despite the
 *   structural reality being primarily about military-strategic positioning
 *   and defense contractor integration. Extractiveness has increased from
 *   0.35 to 0.58 over the 50-year measurement interval, indicating
 *   progressive layering of extraction onto the original coordination
 *   function. Theater ratio has similarly increased, suggesting that
 *   rhetorical justification burden has grown as the extractive mechanisms
 *   have become more visible.
 *
 * KEY AGENTS:
 *   - US Defense and Strategic Interest: Primary beneficiary (institutional/arbitrage) — benefits from regional military positioning, intelligence cooperation, technology development partnership
 *   - Israeli Defense and Security Establishment: Primary beneficiary (institutional/arbitrage, but increasingly identity_locked) — benefits from military aid, technology transfer, strategic cover; exit increasingly constrained by decades of dependency integration
 *   - Palestinian Populations: Primary victim (powerless/trapped) — structurally confined by military asymmetry and territorial control embedded in the partnership; no meaningful exit option
 *   - Regional Arab States: Secondary victim (moderate/constrained) — forced into alliance choices and regional polarization; experience both coordination and extraction through alliance dynamics
 *   - International Humanitarian and Legal Order: Victim (organized/constrained) — experiences extraction through partnership's opacity and asymmetric accountability
 *   - US Diplomatic and Political Actors: Complex position (powerful/mobile but politically constrained) — experience coordination benefit alongside extraction of diplomatic flexibility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_israel_strategic_partnership, 0.58).
domain_priors:suppression_score(us_israel_strategic_partnership, 0.65).
domain_priors:theater_ratio(us_israel_strategic_partnership, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_israel_strategic_partnership, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_israel_strategic_partnership, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(us_israel_strategic_partnership, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_israel_strategic_partnership, tangled_rope).
narrative_ontology:human_readable(us_israel_strategic_partnership, "US-Israel Strategic Partnership Framework").
narrative_ontology:topic_domain(us_israel_strategic_partnership, "geopolitics/security/alliance").

domain_priors:requires_active_enforcement(us_israel_strategic_partnership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_israel_strategic_partnership, us_military_industrial_complex).
narrative_ontology:constraint_beneficiary(us_israel_strategic_partnership, israeli_defense_sector).
narrative_ontology:constraint_beneficiary(us_israel_strategic_partnership, regional_us_strategic_position).
narrative_ontology:constraint_victim(us_israel_strategic_partnership, palestinian_populations).
narrative_ontology:constraint_victim(us_israel_strategic_partnership, regional_stability_equilibrium).
narrative_ontology:constraint_victim(us_israel_strategic_partnership, us_middle_east_diplomatic_flexibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALESTINIAN POPULATIONS (SNARE) — Trapped by the asymmetric military dependency and territorial control embedded in the partnership. No meaningful exit option; bears full cost of security asymmetry. Zero degrees of freedom to alter the structural arrangement. Maximum experienced extraction without coordination benefit.
constraint_indexing:constraint_classification(us_israel_strategic_partnership, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ARAB REGIONAL STATES (TANGLED ROPE) — Constrained by US security umbrella dynamics and Israeli military asymmetry. Experience genuine coordination through regional security architecture but also extraction through alliance polarization and forced alignment choices. Can shift positions (Abraham Accords show this) but at significant diplomatic cost.
constraint_indexing:constraint_classification(us_israel_strategic_partnership, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: US DEFENSE AND STRATEGIC INTEREST (ROPE) — Primary beneficiary experiencing the partnership as pure coordination. Solves multiple US interests: regional counterweight to Iran, platform for forward-deployed capabilities, intelligence cooperation, technology development partnership. Net benefit flows toward US institutional actors with low suppression cost.
constraint_indexing:constraint_classification(us_israel_strategic_partnership, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ISRAELI DEFENSE AND SECURITY ESTABLISHMENT (ROPE) — Primary beneficiary experiencing the partnership as coordination of security interests. Receives military aid, technology transfer, and strategic cover. Exit is theoretically possible (arbitrage option) but practically locked by decades of dependency integration and strategic identity fusion with the US relationship.
constraint_indexing:constraint_classification(us_israel_strategic_partnership, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL HUMANITARIAN AND LEGAL ORDER (TANGLED ROPE) — Organized actors (UN bodies, ICC, human rights frameworks) experience the partnership as both coordination (shared security governance norms) and extraction. The partnership's opacity and asymmetric accountability enforcement extract legitimacy from universal legal principles while being coordinated through security architecture logic. Constrained exit: powerful actors benefit from the partnership framework.
constraint_indexing:constraint_classification(us_israel_strategic_partnership, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: US DIPLOMATIC AND POLITICAL ACTORS (TANGLED ROPE) — Powerful but constrained. Experience genuine coordination benefit (Israel as strategic ally, intelligence sharing, technology development) alongside extraction (constrained diplomatic flexibility in Middle East, political costs of supporting contested policies, bifurcated messaging requirements). Mobile in principle but politically constrained in practice — exit carries significant domestic political cost.
constraint_indexing:constraint_classification(us_israel_strategic_partnership, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: POST-COLD WAR ALLIANCE RHETORIC (PITON) — The partnership's framing as 'shared democratic values' and 'strategic alignment' carries high theater ratio. The actual structural function (military asset positioning, regional alignment, defense contractor coordination) persists through institutional inertia and rhetorical maintenance long after the Cold War strategic logic has degraded. Theater ratio indicates that the justificatory narrative requires active maintenance.
constraint_indexing:constraint_classification(us_israel_strategic_partnership, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, the partnership exhibits genuine coordination (shared intelligence, technology, regional security architecture) alongside systematic extraction (Palestinian confinement, regional polarization, suppression of diplomatic alternatives, institutional capture of US flexibility). Neither pure coordination nor pure extraction — hybrid with active suppression of alternatives to the partnership framework.
constraint_indexing:constraint_classification(us_israel_strategic_partnership, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_israel_strategic_partnership_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_israel_strategic_partnership, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_israel_strategic_partnership, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_israel_strategic_partnership, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_israel_strategic_partnership, TR),
    TR >= 0.70.

:- end_tests(us_israel_strategic_partnership_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The partnership extracts at multiple levels: (1) Palestinian confinement and asymmetric military control, (2) suppression of diplomatic alternatives and settlement solutions, (3) constraint of US diplomatic flexibility toward Iran and broader Middle East, (4) institutional capture of US decision-making. However, extractiveness is not maximal (0.72+) because genuine coordination functions exist — the partnership solves real security interests for both parties, intelligence cooperation benefits both, and technology development is reciprocal. The rising trajectory (0.35 → 0.58) indicates progressive layering of extraction onto original coordination, particularly through defense contractor expansion and political institutionalization of the alliance. Suppression (0.65): Moderate-high. Multiple suppression mechanisms: (1) military asymmetry that confines Palestinian populations, (2) institutional barriers to diplomatic alternatives (both countries' decision-making captured by defense/security apparatus), (3) political cost of questioning the partnership in both US and Israel, (4) media and lobbying infrastructure that maintains favorable framing. Theater ratio (0.55): Moderate. The partnership's justificatory narratives (shared democratic values, regional stability, strategic necessity) require active maintenance. The actual structural function (military asset positioning, defense contractor integration, regional alignment) operates beneath these narratives. Theater has increased as extractive mechanisms have become more visible, requiring stronger rhetorical maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The partnership generates maximum perspectival divergence. US and Israeli institutional actors see pure coordination (Rope) — they are solving genuine shared security interests. Arab regional states and international legal order see tangled rope — experiencing both coordination through regional security architecture and extraction through alliance polarization and asymmetric accountability. Palestinian populations see pure extraction (Snare) — they experience only the military confinement and asymmetric control with no coordination benefit. US diplomatic actors experience tangled rope at higher power level — they see genuine strategic benefit alongside constrained diplomatic flexibility. The post-Cold War alliance rhetoric sees itself as piton — the justificatory narrative persists through institutional inertia rather than current strategic logic. The analytical observer classifies the entire structure as tangled rope — genuine coordination overlaid with systematic extraction and suppression of alternatives. The gap between beneficiary perception (rope) and victim perception (snare) with analytical perspective (tangled rope) reveals that the partnership functions through active suppression of the victim perspective's salience in institutional decision-making.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) flow from each agent's structural position. US and Israeli institutional beneficiaries with arbitrage exit options derive low d (0.05-0.20), producing negative or minimal f(d) and low experienced extraction chi. Palestinian populations with trapped exit derive high d (0.95), producing high f(d) and high experienced extraction chi. Arab regional states with constrained exit and split beneficiary/victim status derive moderate-high d (0.55-0.65), producing moderate f(d) and moderate chi. US diplomatic actors (powerful but politically constrained) derive moderate d (0.50-0.60). The Israeli defense establishment derives high d in alternative analysis where identity-lock is considered (0.40-0.50) rather than pure arbitrage (0.15). The analytical observer derives d from the network position (0.72), factoring in the global scope and the systemic nature of the asymmetry. The rising extractiveness trajectory is partially explained by decreasing exit options for locked agents and increasing lock-in effects for beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The partnership is a hybrid tangled rope, not pure coordination disguised as extraction. The mandatrophy resolves by acknowledging that both coordination and extraction are structurally real. The coordination function (shared security interests, intelligence cooperation, technology development) is genuine and benefits both institutional actors. The extraction function (Palestinian confinement, suppression of diplomatic alternatives, constraint of US flexibility) is also genuine and benefits institutional actors at cost to other populations. The constraint cannot be reduced to coordination alone (that would be false natural law masking as rope) nor to extraction alone (that would miss the real coordination benefits). The tangled rope classification holds because: (1) genuine coordination mechanisms exist and create real benefits, (2) active enforcement is required (alliance maintenance, military coordination, lobbying infrastructure), (3) asymmetric extraction is embedded within the coordination structure, (4) suppression of alternatives is built into the alliance's institutional logic. The rising theater ratio indicates that the justificatory narrative burden has increased as extractive mechanisms have become more visible — the partnership requires stronger rhetorical maintenance to sustain institutional buy-in as costs accumulate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    us_strategic_flexibility_measurement,
    'How much has the US-Israel partnership constrained US diplomatic flexibility toward Iran, Palestine, and broader Middle East settlement compared to a counterfactual without the partnership?',
    'Comparative analysis of US diplomatic positioning with allied states (Saudi Arabia, UAE) vs constrained positioning (Palestine, Iran); counterfactual modeling of US Middle East policy without Israel partnership constraint',
    'If flexibility loss > 30% of optimal: extraction from US diplomatic corpus is substantial, raising χ. If < 10%: extraction is minimal, lowering χ toward rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_strategic_flexibility_measurement, empirical, 'Degree to which partnership constrains US diplomatic flexibility').

omega_variable(
    israeli_defense_sector_dependency_depth,
    'Is Israeli defense capability and economic viability structurally dependent on the US partnership, or could Israel maintain security independently with alternative alliances?',
    'Analysis of Israeli defense budget composition, technology transfer dependency, military equipment sourcing, alternative alliance options (Russia, China, EU); counterfactual capacity assessment without US aid',
    'If fully dependent (>70%): Israeli exit is trapped rather than arbitrage, raising d and χ. If partially dependent (30-50%): arbitrage classification holds but with reduced exit freedom.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(israeli_defense_sector_dependency_depth, empirical, 'Depth of Israeli structural dependency on US partnership').

omega_variable(
    regional_stability_vs_partnership_benefit_tradeoff,
    'Does the partnership''s contribution to regional stability (deterring aggression, enabling security coordination) outweigh its costs to regional stability (polarization, settlement entrenchment, suppression of diplomatic solutions)?',
    'Longitudinal regional conflict analysis pre/post partnership formalization; comparative stability metrics with/without partnership; assessment of alternative regional security architectures',
    'If partnership improves net stability: coordination function is genuine, supporting rope/tangled_rope. If destabilizing: extraction dominates, supporting snare/tangled_rope with higher χ.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regional_stability_vs_partnership_benefit_tradeoff, conceptual, 'Net regional stability impact of the partnership').

omega_variable(
    identity_lock_vs_strategic_choice,
    'Does Israeli or US commitment to the partnership reflect strategic analysis or identity fusion (alliance as core national identity rather than contingent strategic choice)?',
    'Political discourse analysis; counterfactual assessment of how each nation would respond to significant partnership cost shocks; elite opinion survey on partnership essentiality vs instrumentality',
    'If identity-locked: classification upgrades from rope/arbitrage to constrained or identity_locked exit, raising χ and revealing suppressed alternatives. If strategic choice: arbitrage exit classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_strategic_choice, conceptual, 'Whether partnership commitment is identity-based or strategic').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_israel_strategic_partnership, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usisrael_tr_t0, us_israel_strategic_partnership, theater_ratio, 0, 0.38).
narrative_ontology:measurement(usisrael_tr_t20, us_israel_strategic_partnership, theater_ratio, 20, 0.48).
narrative_ontology:measurement(usisrael_tr_t40, us_israel_strategic_partnership, theater_ratio, 40, 0.55).
narrative_ontology:measurement(usisrael_tr_t50, us_israel_strategic_partnership, theater_ratio, 50, 0.62).

% Extraction over time
narrative_ontology:measurement(usisrael_be_t0, us_israel_strategic_partnership, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(usisrael_be_t20, us_israel_strategic_partnership, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(usisrael_be_t40, us_israel_strategic_partnership, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(usisrael_be_t50, us_israel_strategic_partnership, base_extractiveness, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_israel_strategic_partnership, enforcement_mechanism).
narrative_ontology:affects_constraint(us_israel_strategic_partnership, palestinian_security_dependency).
narrative_ontology:affects_constraint(us_israel_strategic_partnership, middle_east_diplomatic_architecture).
narrative_ontology:affects_constraint(us_israel_strategic_partnership, us_iran_strategic_constraint).
narrative_ontology:affects_constraint(us_israel_strategic_partnership, israeli_defense_sector_rent_extraction).

% DUAL FORMULATION NOTE:
% The US-Israel partnership is upstream of several downstream constraints including Palestinian security confinement, regional diplomatic alternatives suppression, and institutional capture of US Middle East policy flexibility. This story focuses on the partnership's hybrid coordination-extraction structure; downstream stories analyze the specific extractive mechanisms in each domain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_israel_strategic_partnership, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
