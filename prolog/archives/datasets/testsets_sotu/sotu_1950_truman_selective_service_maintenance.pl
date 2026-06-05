% ============================================================================
% CONSTRAINT STORY: sotu_1950_truman_selective_service_maintenance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1950_truman_selective_service_maintenance, []).

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
 *   constraint_id: sotu_1950_truman_selective_service_maintenance
 *   human_readable: Selective Service System (Peacetime Conscription)
 *   domain: military/political_economy
 *
 * SUMMARY:
 *   In 1950, President Truman justified the continuation of the Selective
 *   Service System as essential to Cold War national defense and UN
 *   collective security commitments following World War II. The constraint
 *   operates by distributing military service obligations compulsorily across
 *   the male population via legal conscription, concentrating defense
 *   capacity and decision-making power in state military apparatus while
 *   imposing direct service costs (opportunity costs, life risk, time burden)
 *   on conscripted individuals. This tension — between the state's genuine
 *   need for sustained military readiness and the conscript population's
 *   forced labor transfer — generates the DR classification puzzle: from the
 *   state's institutional perspective, conscription solves a critical
 *   collective action problem (assembling force without volatile labor
 *   markets). From the conscript's perspective, it is coercive extraction
 *   with legal force backing exit suppression. The constraint exhibits all
 *   six DR types across its observational positions, making it a diagnostic
 *   exemplar for how structural arrangements that appear as natural necessity
 *   to beneficiaries appear as pure extraction to victims.
 *
 * KEY AGENTS:
 *   - State Military Apparatus: Primary beneficiary (institutional/arbitrage) — captures compulsory labor supply, sustains force readiness, controls service duration and assignment; minimal exit costs from state perspective
 *   - Conscripted Male Population: Primary victim (powerless/trapped) — faces legal obligation with severe penalties, zero exit options, absorbed opportunity costs and life risk; experiences extraction with no apparent coordination benefit
 *   - Defense Industrial Base: Secondary beneficiary (organized/constrained) — depends on sustained military spending enabled by conscription-powered force structure and modernization cycles
 *   - Professional Military Officer Corps: Mixed agent (moderate/constrained) — benefits from conscript supply and career advancement paths but faces integration challenges and extended active duty
 *   - Anti-Conscription Coalition: Emerging organized agent (organized/constrained) — pacifist churches, civil rights organizations perceiving conscription as temporary violation with moral sunset
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing geopolitical necessity framing that masks contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1950_truman_selective_service_maintenance, 0.58).
domain_priors:suppression_score(sotu_1950_truman_selective_service_maintenance, 0.72).
domain_priors:theater_ratio(sotu_1950_truman_selective_service_maintenance, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1950_truman_selective_service_maintenance, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_1950_truman_selective_service_maintenance, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sotu_1950_truman_selective_service_maintenance, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sotu_1950_truman_selective_service_maintenance, accessibility_collapse, 0.0).
narrative_ontology:constraint_metric(sotu_1950_truman_selective_service_maintenance, resistance, 0.0).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1950_truman_selective_service_maintenance, tangled_rope).
narrative_ontology:human_readable(sotu_1950_truman_selective_service_maintenance, "Selective Service System (Peacetime Conscription)").
narrative_ontology:topic_domain(sotu_1950_truman_selective_service_maintenance, "military/political_economy").

domain_priors:requires_active_enforcement(sotu_1950_truman_selective_service_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1950_truman_selective_service_maintenance, state_military_apparatus).
narrative_ontology:constraint_beneficiary(sotu_1950_truman_selective_service_maintenance, defense_industrial_base).
narrative_ontology:constraint_victim(sotu_1950_truman_selective_service_maintenance, conscripted_male_population).
narrative_ontology:constraint_victim(sotu_1950_truman_selective_service_maintenance, voluntary_military_labor_market).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSCRIPTED MALE POPULATION (SNARE) — Legal obligation with severe penalties for refusal (imprisonment, social stigma). Draft-age males face zero meaningful exit options: conscription is universal and compulsory, with no conscientious objector pathways in the 1950 Truman framing. Trapped agents experience maximum extraction — their labor is coerced, their opportunity costs are absorbed by the state, and their exit is blocked by law and social enforcement. No coordination benefit is apparent from this position; the constraint is pure forced labor transfer.
constraint_indexing:constraint_classification(sotu_1950_truman_selective_service_maintenance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROFESSIONAL MILITARY OFFICER CORPS (TANGLED ROPE) — Constrained exit: officers benefit from conscription's supply of personnel and the stable force structure it enables, but face career costs (longer active duty, integration challenges with conscripted troops). Genuine coordination function: conscription creates predictable personnel flows enabling long-range force planning. Asymmetric extraction: officer careers advance through commanding conscript masses, concentrating advancement in professional hands while conscripts absorb service costs. Mixed experience — significant benefit and significant constraint.
constraint_indexing:constraint_classification(sotu_1950_truman_selective_service_maintenance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE MILITARY APPARATUS (ROPE) — Primary beneficiary with arbitrage options (can adjust conscription levels, manage policy, capture surplus via administrative control). Experiences the constraint as pure coordination: conscription solves the collective action problem of force assembly without relying on volatile labor markets. Extracts immediate value (conscript labor, sustained readiness) with minimal direct cost to state apparatus. The constraint is functional cooperation — the state coordinates the distribution of service obligations that sustain military capacity.
constraint_indexing:constraint_classification(sotu_1950_truman_selective_service_maintenance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DEFENSE INDUSTRIAL BASE (TANGLED ROPE) — Organized beneficiary constrained by dependence on state procurement and conscription-sustained military expansion. Genuine coordination: conscription enables sustained defense spending, predictable force modernization cycles, and technology procurement pipelines. Asymmetric extraction: defense contractors capture surplus via cost-plus contracts while conscripts absorb wage and opportunity costs. Organizational power enables partial exit (diversification, international sales) but major dependence remains. Strong coordination function with embedded extraction.
constraint_indexing:constraint_classification(sotu_1950_truman_selective_service_maintenance, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANTI-CONSCRIPTION COALITION / ORGANIZED RESISTANCE (SCAFFOLD) — Emerging organized opposition (pacifist churches, civil rights organizations) sees conscription as temporary violation with moral sunset. Low effective extraction from this perspective because coalition perceives alternative pathways: voluntary military, universal civilian service, or demilitarization. Coalition constraints are real (legal penalties, social cost) but exit path is visible (political change, cultural shift). Theater is moderate — conscription performs legitimacy via universal male citizenship, but the performance is increasingly contested.
constraint_indexing:constraint_classification(sotu_1950_truman_selective_service_maintenance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / COLD WAR NECESSITY (MOUNTAIN) — From the civilizational/global perspective of 1950, conscription appears as an immutable requirement of geopolitical structure: the USSR maintains conscription, collective security requires sustained NATO force projection, and no democratic state can match professional military recruitment during peacetime military buildup. The constraint appears as natural law of geopolitical necessity. However, structural beneficiaries exist (state apparatus, defense industry) and victims are identifiable (conscripted males), triggering false summit evaluation. The 'immutable geopolitical necessity' naturalizes a contingent institutional choice to distribute military burdens via conscription rather than alternative mechanisms.
constraint_indexing:constraint_classification(sotu_1950_truman_selective_service_maintenance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1950_truman_selective_service_maintenance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1950_truman_selective_service_maintenance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1950_truman_selective_service_maintenance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1950_truman_selective_service_maintenance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sotu_1950_truman_selective_service_maintenance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The state extracts substantial labor value from conscripts (two years of service, minimal compensation, directed labor allocation) but the extraction is bounded by duration (peacetime conscription is framed as temporary) and partial legitimacy (framed as national defense burden-sharing, not pure coercion). Suppression (0.72): High. Multiple suppression mechanisms operate: legal penalties for non-compliance (federal prosecution, imprisonment), social enforcement (patriotic duty, masculinity norms tying military service to citizenship), and administrative barriers (no meaningful conscientious objector pathways in 1950 framing, limited deferment windows). However, suppression is not total (some deferments exist for critical occupations, educational paths) — hence 0.72 rather than 0.85+. Theater ratio (0.48): Moderate-low. Conscription performs legitimacy via universal male citizenship framing and rhetorical tie to UN collective security. But the mechanism itself is largely functional rather than performative — conscription actually does deliver manpower for force readiness, not merely the appearance of it. The theater is present but not dominant; contrast this with purely performative ritual systems (piton) that maintain activity without functional output.
 *
 * PERSPECTIVAL GAP:
 *   Maximum divergence between victim and beneficiary perspectives. Conscripted males (d=0.95) experience χ ≈ 0.82 and classify as Snare: high extraction, high suppression, minimal coordination benefit perceived. State apparatus (d=0.05) experiences χ ≈ -0.07 and classifies as Rope: extraction flows toward them, coordination function is central to their functional view. This gap reflects that the same institutional mechanism is extraction mechanism from victim position and coordination mechanism from beneficiary position. The gap is not measurement error — it is a genuine structural feature of asymmetric power. The scaffold perspective (anti-conscription coalition, d ≈ 0.68) experiences moderate χ ≈ 0.39 and sees temporary problem with visible sunset, lowering experienced severity relative to the trapped perspective. The false summit mountain perspective naturalizes the state's coordination framing as inevitable geopolitical law, missing the contingency that voluntary professional military or alternative conscription designs could have solved the same coordination problem.
 *
 * DIRECTIONALITY LOGIC:
 *   The chi formula χ = ε × f(d) × σ(S) applies scope modifier σ(national) = 1.0 (national scope does not amplify or dampen relative to standard). Extracted values: conscript perspective: χ ≈ 0.58 × 1.42 × 1.0 ≈ 0.82 (high experienced extraction); state apparatus: χ ≈ 0.58 × (-0.12) × 1.0 ≈ -0.07 (extraction captured as surplus); moderate professional officers: χ ≈ 0.58 × 0.75 × 1.0 ≈ 0.44 (moderate extraction). These computed χ values anchor the classification types per the canonical rules: χ > 0.66 → Snare, 0.40 ≤ χ ≤ 0.90 → Tangled Rope, χ < 0.35 + coordination → Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that Snare and Rope classifications are perspectival, not contradictory. The same structural mechanism is extractive from the victim's position and coordinative from the beneficiary's position. The mandatrophy question — 'Is conscription extraction or coordination?' — has no single answer; it has six answers depending on structural position. The false summit mountain classification reveals that the 'inevitable geopolitical necessity' framing naturalizes a contingent institutional choice. Alternative designs existed (all-volunteer professional military, higher-wage recruitment, rotating short-service conscription with lower suppression, international burden-sharing mechanisms). The Truman administration's choice to frame conscription as inherent to Cold War survival rather than as one option among several is the naturalizing move the framework detects. The mandatrophy is resolved when we recognize that the constraint's extractiveness (0.58) and classification type (tangled_rope from state perspective, snare from conscript perspective) are structural facts about the system, not logical contradictions. Both types are correct; they describe different agents' structural experiences.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_recruitment_feasibility,
    'Could sustained military force projection have been achieved via higher professional military wages and benefits rather than conscription?',
    'Historical comparative analysis: contrast actual conscription-based force with counterfactual all-volunteer professional military; economic modeling of recruitment elasticity to compensation; international comparison of all-volunteer military effectiveness (UK, other NATO countries with higher professional pay)',
    'If feasible: conscription is pure extraction mechanism (Snare from field perspective) hiding behind manufactured necessity. If infeasible: conscription is justified coordination mechanism (Rope from state perspective). Most likely: mixed — partial feasibility up to force size ceiling, creating genuine coordination problem above that threshold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voluntary_recruitment_feasibility, empirical, 'Whether voluntary recruitment could have replaced conscription').

omega_variable(
    suppression_mechanism_internalization,
    'To what degree is suppression of exit in the Selective Service System structural (legal penalties, enforcement apparatus) versus internalized (patriotic duty, masculinity norms)?',
    'Comparative analysis of post-Vietnam conscription: exit behavior after conscription ended (did previously internalized obligation disappear?); survey data on conscripts'' perceived coercion vs duty; analysis of conscientious objector pathways and their use rates as proxy for internalization levels',
    'If primarily structural: suppression metric is accurate; post-conscription voluntary recruitment would succeed. If primarily internalized: suppression metric underestimates true constraint power; internalized obligation persists after legal mechanism removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Degree of structural vs internalized suppression in conscription').

omega_variable(
    state_apparatus_extractive_intent,
    'Is the Selective Service System intentionally designed to extract labor for state military purposes, or is conscription a regrettable necessity perceived as burden-sharing?',
    'Analysis of Truman administration documents, Defense Department policy memoranda, and Congressional testimony; examination of conscription policy evolution and alternative proposals entertained; comparative study of conscription framing in Cold War rhetoric vs actual policy mechanics',
    'If intentional extraction: Snare classification from state perspective is incorrect; state should show constrained rather than arbitrage exit. If burden-sharing framing: state genuinely views conscription as temporary necessity, changing time_horizon (biographical → generational suggests sunset, supporting Scaffold classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_apparatus_extractive_intent, conceptual, 'Whether Selective Service represents extraction or burden-sharing').

omega_variable(
    coalition_power_threshold,
    'At what scale of organized anti-conscription coalition does the constraint shift from Snare (for organized agents) to Tangled Rope or Scaffold?',
    'Historical track: organize coalition membership (churches, civil rights, student organizations) and cross-reference against shifts in draft policy flexibility (conscientious objector pathways, educational deferments, medical exemptions); measure coalition growth over interval and correlate with policy loosening',
    'If threshold < 10% organized opposition: coalition power is real and constraint tightens (coalitions extract concessions, lowering effeciveness χ). If threshold > 30%: coalition effects emerge only in near-term, supporting Scaffold classification''s sunset claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_power_threshold, empirical, 'Threshold for organized opposition to shift constraint classification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1950_truman_selective_service_maintenance, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu_ss_tr_t0, sotu_1950_truman_selective_service_maintenance, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sotu_ss_tr_t3, sotu_1950_truman_selective_service_maintenance, theater_ratio, 3, 0.45).
narrative_ontology:measurement(sotu_ss_tr_t6, sotu_1950_truman_selective_service_maintenance, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(sotu_ss_be_t0, sotu_1950_truman_selective_service_maintenance, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(sotu_ss_be_t3, sotu_1950_truman_selective_service_maintenance, base_extractiveness, 3, 0.56).
narrative_ontology:measurement(sotu_ss_be_t6, sotu_1950_truman_selective_service_maintenance, base_extractiveness, 6, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1950_truman_selective_service_maintenance, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(sotu_1950_truman_selective_service_maintenance, 0.12).
narrative_ontology:affects_constraint(sotu_1950_truman_selective_service_maintenance, military_industrial_complex_expansion).
narrative_ontology:affects_constraint(sotu_1950_truman_selective_service_maintenance, cold_war_security_state_formation).

% DUAL FORMULATION NOTE:
% The Selective Service System is a structural component of the emerging Cold War military-industrial complex. The conscription mechanism enables sustained defense spending and force projection that benefits defense contractors and military planning apparatus. Upstream constraints (Soviet military posture, geopolitical bipolarity) create the necessity framing for conscription; downstream constraints (defense industrial procurement, military-university research coupling) depend on conscription-enabled force readiness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1950_truman_selective_service_maintenance, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
