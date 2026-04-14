% ============================================================================
% CONSTRAINT STORY: eu_migration_burden_sharing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_migration_burden_sharing, []).

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
 *   constraint_id: eu_migration_burden_sharing
 *   human_readable: EU Migration Burden Sharing Constraint
 *   domain: political_economy/migration_policy
 *
 * SUMMARY:
 *   The EU migration burden-sharing constraint represents a structural
 *   asymmetry in which the costs of asylum processing, integration, and
 *   border management are concentrated on geographically peripheral member
 *   states (Greece, Italy, Hungary, Poland) while wealthier, more central
 *   states (Germany, France, Netherlands, Scandinavia) exercise
 *   disproportionate control over policy while accepting fewer asylum
 *   seekers. The constraint emerged formally with the 2015-2016 migration
 *   crisis and the EU's failed relocation mechanism, but its roots extend
 *   deeper into the Schengen Area structure and asylum harmonization efforts.
 *   The mechanism combines genuine coordination (harmonized asylum standards,
 *   free movement for EU citizens, shared external border) with systematic
 *   extraction (border states trapped by geography and treaty; wealth
 *   concentration enables opt-out options; migrants caught in legal limbo
 *   with no representation). The theater ratio reflects the gap between EU
 *   policy proposals (quota systems, relocation mechanisms, burden-sharing
 *   directives) and actual implementation — most ambitious sharing mechanisms
 *   fail due to member state non-compliance or legal avoidance. The
 *   extractiveness has increased over the measurement interval (0.35→0.58) as
 *   initial hope for solidarity mechanisms gave way to hardline policies
 *   (pushbacks, externalization agreements, criminalization of migration)
 *   while formal rhetoric of burden-sharing persists.
 *
 * KEY AGENTS:
 *   - Asylum Seekers and Migrants: Primary victims (powerless/trapped, regional) — bear legal restrictions, economic vulnerability, geographic confinement; no representation in EU policy; maximum suppression with zero coordination benefit
 *   - Southern/Eastern Border States: Primary victims (powerless/trapped, regional) — Greece, Italy, Spain, Hungary, Poland trapped by geography and treaty; forced to receive and process migrants; face resource depletion and political destabilization; minimal EU compensation
 *   - Northern/Western Wealthy States: Primary beneficiaries (institutional/arbitrage, global) — Germany, France, Netherlands, Scandinavian countries control policy, externalize border costs, absorb selective migrants based on labor needs; net beneficiaries experiencing low extraction
 *   - EU Institutions: Secondary beneficiary (institutional/arbitrage, global) — Commission and Council maintain formal burden-sharing frameworks that generate legitimacy while achieving minimal actual redistribution; institutional survival depends on performative solidarity
 *   - Organized Member State Coalitions: Mixed (organized/constrained, regional) — Visegrád group, Southern EU bloc, various coalitions experience both coordination benefits and extraction asymmetries; trapped by treaty obligations but capable of collective action
 *   - Civil Society and NGOs: Organized advocates (organized/constrained, regional) — Human rights organizations, migration networks maintain pressure for reform; have exit pathways through advocacy but face suppression through funding constraints and political marginalization
 *   - Analytical Observer: Global/civilizational position (analytical/analytical, global) — Sees the constraint as downstream of global inequality and upstream of European political disintegration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_migration_burden_sharing, 0.58).
domain_priors:suppression_score(eu_migration_burden_sharing, 0.68).
domain_priors:theater_ratio(eu_migration_burden_sharing, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_migration_burden_sharing, extractiveness, 0.58).
narrative_ontology:constraint_metric(eu_migration_burden_sharing, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(eu_migration_burden_sharing, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_migration_burden_sharing, tangled_rope).
narrative_ontology:human_readable(eu_migration_burden_sharing, "EU Migration Burden Sharing Constraint").
narrative_ontology:topic_domain(eu_migration_burden_sharing, "political_economy/migration_policy").

domain_priors:requires_active_enforcement(eu_migration_burden_sharing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_migration_burden_sharing, northern_western_member_states).
narrative_ontology:constraint_beneficiary(eu_migration_burden_sharing, eu_institutions).
narrative_ontology:constraint_victim(eu_migration_burden_sharing, southern_eastern_border_states).
narrative_ontology:constraint_victim(eu_migration_burden_sharing, asylum_seekers_migrants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ASYLUM SEEKERS AND MIGRANTS (SNARE) — Face maximum suppression through legal barriers, economic dependency, geographic containment in border camps, and lack of political representation. No exit options; cannot organize effectively; bear full cost of the burden-sharing asymmetry. No coordination benefit — the constraint exists purely to extract labor, control, and vulnerability for receiving states' benefit.
constraint_indexing:constraint_classification(eu_migration_burden_sharing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: SOUTHERN/EASTERN BORDER STATES (SNARE) — Trapped by geographic position and EU law to receive and process migrants while wealthier northern/western states refuse responsibility or impose minimal burden. Cannot exit EU without catastrophic cost; cannot refuse migrants under Schengen/asylum law. Face resource depletion, political instability, and demographic burden with minimal EU funding. Maximum suppression through treaty lock-in.
constraint_indexing:constraint_classification(eu_migration_burden_sharing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: EU COALITIONS (TANGLED ROPE) — Organized actors (Visegrád group, Southern EU coalition, etc.) experience both coordination and extraction. Coordination function: EU labor mobility and asylum standardization do solve genuine cross-border problems. Extraction function: the system asymmetrically concentrates costs on border states while allowing wealthy states to externalize responsibility. High suppression (treaty enforcement, Schengen lock-in) combined with genuine coordination benefits creates the hybrid structure.
constraint_indexing:constraint_classification(eu_migration_burden_sharing, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: NORTHERN/WESTERN STATES (ROPE) — Institutional beneficiaries (arbitrage exit: can enforce external border control, fund border agencies in poorer states, or absorb minimal migrants). Experience the constraint as pure coordination: asylum harmonization and free movement facilitate labor supply and economic integration without internal border friction. Net beneficiaries experiencing low extraction because they shape the rules.
constraint_indexing:constraint_classification(eu_migration_burden_sharing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: EU INSTITUTIONS (PITON) — The Commission, Parliament, and Council maintain burden-sharing frameworks (relocation mechanisms, asylum directives) that are largely performative. Proposals like mandatory refugee quotas fail repeatedly; actual burden-sharing rates remain far below targets. The bureaucratic apparatus persists through institutional inertia and foundational legitimacy despite low functional burden-sharing. Theater ratio high because the rituals (directives, negotiations, symbolic agreements) produce minimal actual redistribution.
constraint_indexing:constraint_classification(eu_migration_burden_sharing, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CIVIL SOCIETY AND ADVOCATES (SCAFFOLD) — Organized agents (NGOs, human rights groups, migration networks) see the burden-sharing asymmetry as a temporary institutional failure solvable by changing member state priorities and treaty interpretation. They experience suppression (political marginalization, funding constraints) but maintain exit pathways (advocacy campaigns, legal challenges, norm pressure). Scaffold classification: they perceive the constraint as changeable within a generational timeframe through political mobilization, with a credible sunset if political will shifts.
constraint_indexing:constraint_classification(eu_migration_burden_sharing, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a global/civilizational view, the EU constraint is downstream of global migration asymmetries (wealth inequality, climate/conflict displacement, labor market mismatch). The EU mechanism both coordinates asylum policy (genuine function) and extracts from border states via rule lock-in (asymmetric cost distribution). The analytical perspective sees both coordination necessity and extractive rent-seeking in the same institutional structure.
constraint_indexing:constraint_classification(eu_migration_burden_sharing, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_migration_burden_sharing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_migration_burden_sharing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_migration_burden_sharing, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_migration_burden_sharing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(eu_migration_burden_sharing, TR),
    TR >= 0.70.

:- end_tests(eu_migration_burden_sharing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting the systematic asymmetry in burden concentration. This is not maximal extraction (0.75+) because wealthy states do absorb some migrants and do fund some EU mechanisms, creating a veneer of coordination. However, the asymmetry in control versus burden is severe — border states contribute far more per capita in costs and migrants than their population or wealth would suggest in an equitable system. The value has increased over time as relocation mechanisms failed and externalization strategies (pushing responsibility to Libya, Turkey, etc.) concentrated burden on entry states. Suppression (0.68): High. Barriers include legal restrictions on migrant movement, visa regimes, border controls, detention, processing delays, and the treaty lock-in that prevents border states from unilaterally reforming. For migrants: near-total legal suppression. For border states: treaty-based inability to refuse migrants or renegotiate costs without existential EU risk. Theater ratio (0.62): Moderate-high. The EU's burden-sharing machinery (relocation directives, asylum harmonization, solidarity mechanisms) is substantially performative. Member states repeatedly negotiate, agree, then fail to implement or legally circumvent quotas. The 2015 relocation mechanism achieved <50% of proposed relocations. Ongoing directives generate political theater (summit meetings, unanimous agreement theater) while actual redistribution remains minimal. The increase from 0.42 to 0.68 reflects growing gap between policy proposals and implementation as resistance hardened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits sharp perspectival divergence. Asylum seekers and migrants experience pure snare (trapped, maximum suppression, no coordination benefit). Border states experience snare (trapped by treaty and geography with high costs). Wealthy states experience rope (coordination benefit, minimal extraction, arbitrage exit). EU institutions experience piton (performative framework sustained by institutional inertia). Civil society experiences scaffold (sees temporary failure addressable by political change). The analytical observer sees tangled_rope (genuine coordination needs combined with systematic extraction). The perspectival gap reveals that the same constraint is simultaneously: (1) an unavoidable consequence of coordinating open borders (beneficiary perspective), (2) an unjust system of cost concentration (victim perspective), (3) a degraded ritual of failed solidarity (institutional perspective), and (4) a solvable political problem (advocacy perspective). The gap is not noise — it reflects the constraint's hybrid structure: the coordination function is real (asylum harmonization does prevent race-to-the-bottom), but so is the extraction (the distribution of costs is not justified by capacity or responsibility).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the fundamental structural asymmetry: who controls the constraint mechanism versus who bears its costs. Wealthy northern states have arbitrage exit (can enforce borders, externalize via funding, or absorb selective migrants) combined with beneficiary status (labor supply, control over redistribution). This yields low d and negative χ — experienced extraction runs toward them. Border states have trapped exit (cannot refuse migrants without treaty violation; cannot control costs without EU sanction) combined with victim status (forced to process; bear integration burden). This yields high d and high f(d) — experienced extraction runs against them. Migrants have trapped exit (legal barriers) and victim status; maximum d, maximum f(d). Organized coalitions have constrained exit (can negotiate, can form blocs, but cannot unilaterally exit) and mixed beneficiary/victim status depending on coalition membership; moderate d. The asymmetry in control (wealthy states shape policy) versus burden (border states and migrants absorb costs) is the core directionality driver.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The tangled_rope classification is correct because the constraint possesses genuine coordination function (asylum harmonization, Schengen free movement, shared external border) alongside asymmetric extraction (costs concentrated on border states, migrants stripped of agency). Both features are structural, not observational artifacts. The threat of false natural law (mountain classification: 'migration burden is necessarily asymmetric because of geography') is resolved by noting that geography determines vulnerability but not the distribution choice. The EU could implement equitable burden-sharing (per-capita quotas, genuine relocation, proportional funding); it chooses not to. This is extraction, not immutable law. The threat of false coordination (rope classification) is resolved by identifying the systematic extraction: northern states benefit disproportionately, have exit options that southern states lack, and use institutional power to avoid responsibility. The constraint is not pure coordination — it is coordination layered with extractive asymmetry. The theater increases (0.42→0.68) as the gap between stated solidarity and actual redistribution widens, indicating that performative activity substitutes for functional burden-sharing. This is a signature of mandate drift: the constraint's public purpose (equitable burden-sharing) has been progressively displaced by actual function (cost concentration on weakest states).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    burden_measurement_ambiguity,
    'How should burden be measured: by per-capita asylum arrivals, by costs incurred, by integration success, or by political/social disruption tolerance?',
    'Comparative analysis across member states using standardized metrics; correlation between different burden measures and actual state capacity; longitudinal tracking of migration outcomes by destination',
    'If per-capita: Denmark and Malta become high-burden states despite finite resources. If by cost: wealthy states can reduce burden through spending. If by integration success: burden shifts to long-term integration capacity rather than arrival management. Different measures justify different redistributions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(burden_measurement_ambiguity, conceptual, 'Metric ambiguity in defining what constitutes fair burden distribution').

omega_variable(
    solidarity_versus_control_tradeoff,
    'Is the EU''s emphasis on external border control (Frontex, externalization) coordination to manage shared entry points or extraction from border states through coerced containment?',
    'Comparison of border state agency in policy design vs imposed directives; analysis of resource flows (does control investment benefit border states or primarily benefit northern enforcement?); examination of policy alternatives proposed vs rejected by wealthy states',
    'If control is coordination: border enforcement benefits all states and is a shared function. If control is extraction: border states bear coercive costs to benefit wealthy states'' sovereignty preferences. Classification shifts from tangled_rope toward snare if control is extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(solidarity_versus_control_tradeoff, empirical, 'Whether external border control serves coordination or extraction').

omega_variable(
    relocation_mechanism_efficacy,
    'Do EU relocation mechanisms (mandatory quotas, solidarity transfers) actually redistribute burden, or do they generate theater while allowing states to legally opt out?',
    'Historical data on relocation rates vs agreed targets; analysis of opt-out rates and penalties; tracking of actual vs promised financial contributions; examination of mechanism design flaws enabling avoidance',
    'If mechanisms work: burden-sharing is genuinely attempted and failure is due to political will, not structural design. If mechanisms fail systematically: the constraint is piton-like (performative) rather than tangled_rope (genuine hybrid). If opt-out rates exceed 50%: classification shifts toward snare (suppression without coordination benefit).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(relocation_mechanism_efficacy, empirical, 'Efficacy of EU burden-sharing relocation mechanisms').

omega_variable(
    migrant_agency_versus_coercion_ratio,
    'To what extent is migrant suppression due to actual unavoidable barriers (border capacity, legal status) versus deliberate policy choice to create scarcity and control?',
    'Comparative analysis of border management costs vs allocated resources; examination of policy alternatives (e.g., streamlined legal processing) that have been rejected; interviews with border officials regarding constraints vs choices; historical data on processing capacity and wait times',
    'If barriers are unavoidable: suppression represents genuine resource scarcity and classification centers on coordination cost. If barriers are deliberate: suppression is extractive rent-seeking (preventing migrants from accessing resources/stability) and classification shifts toward pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(migrant_agency_versus_coercion_ratio, empirical, 'Ratio of unavoidable barriers versus deliberate suppression in migrant control').

omega_variable(
    civilizational_equity_versus_pragmatic_feasibility,
    'Is equitable burden-sharing (based on capacity or per-capita responsibility) the appropriate ethical frame, or is pragmatic feasibility (accepting unequal distribution as stable equilibrium) more structurally sound?',
    'Long-term stability analysis: do unequal burden distributions eventually generate political backlash (border state exit, far-right politics, European disintegration)? Do equity-based frameworks prove implementable? Case comparison with other international burden-sharing mechanisms (trade, defense, climate).',
    'If equity is feasible: scaffold/tangled_rope analysis holds and reorganization is possible. If equity is structurally infeasible: the constraint is a mountain (unavoidable asymmetry of state capacity and demographic distribution). If unequal distributions destabilize: burden-sharing becomes a snare for the weaker states (trapped by instability).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civilizational_equity_versus_pragmatic_feasibility, preference, 'Whether equitable burden-sharing is feasible or whether asymmetry is structural').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_migration_burden_sharing, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_mig_tr_t0, eu_migration_burden_sharing, theater_ratio, 0, 0.42).
narrative_ontology:measurement(eu_mig_tr_t5, eu_migration_burden_sharing, theater_ratio, 5, 0.55).
narrative_ontology:measurement(eu_mig_tr_t10, eu_migration_burden_sharing, theater_ratio, 10, 0.62).
narrative_ontology:measurement(eu_mig_tr_t15, eu_migration_burden_sharing, theater_ratio, 15, 0.68).

% Extraction over time
narrative_ontology:measurement(eu_mig_be_t0, eu_migration_burden_sharing, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(eu_mig_be_t5, eu_migration_burden_sharing, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(eu_mig_be_t10, eu_migration_burden_sharing, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(eu_mig_be_t15, eu_migration_burden_sharing, base_extractiveness, 15, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_migration_burden_sharing, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_migration_burden_sharing, eu_schengen_border_externalization).
narrative_ontology:affects_constraint(eu_migration_burden_sharing, dublin_asylum_responsibility).
narrative_ontology:affects_constraint(eu_migration_burden_sharing, migration_driven_populism).

% DUAL FORMULATION NOTE:
% This constraint is downstream of the Schengen Area structure (which creates coordination needs) and upstream of national populist reactions (which destabilize the constraint). The burden-sharing mechanism itself is distinct from its upstream causes (why free movement requires coordination) and downstream effects (how extraction creates political backlash). Each story in this family has its own ε value reflecting different empirical questions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_migration_burden_sharing, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
