% ============================================================================
% CONSTRAINT STORY: sotu_1953_eisenhower_bipartisan_foreign_policy_coordination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, []).

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
 *   constraint_id: sotu_1953_eisenhower_bipartisan_foreign_policy_coordination
 *   human_readable: Executive-Legislative Bipartisan Coordination for Foreign Policy (1953 Eisenhower Framework)
 *   domain: governance/foreign_policy
 *
 * SUMMARY:
 *   In his 1953 State of the Union address, President Eisenhower articulated
 *   a vision of foreign policy development rooted in genuine bipartisan
 *   cooperation between executive and legislative branches. This
 *   institutional constraint distributes authority over foreign policy
 *   decisions between branches that possess asymmetric capabilities — the
 *   executive has intelligence access, diplomatic expertise, and operational
 *   capacity; Congress has electoral legitimacy, treaty ratification
 *   authority, and appropriations power. The constraint imposes friction on
 *   unilateral executive action while binding Congress into co-responsibility
 *   for foreign commitments, creating mutual legitimacy but also
 *   decision-making slowness. The mechanism exhibits all eight DR types from
 *   different observational positions, revealing how indexical classification
 *   exposes the gap between stated coordination function and actual
 *   extraction patterns.
 *
 * KEY AGENTS:
 *   - Executive Branch: Primary beneficiary (institutional/arbitrage) — gains congressional legitimacy for foreign commitments, strengthening domestic and international negotiating position
 *   - Congressional Leadership (Majority Party): Secondary beneficiary (moderate/constrained) — gains veto power and policy influence, but constrained by legislative process and consensus requirements
 *   - Electoral Legitimacy System: Tertiary beneficiary (institutional/arbitrage) — coordinates two sources of democratic legitimacy (executive competence, legislative representation) into unified foreign policy authority
 *   - Foreign Policy Agility: Primary victim (powerless/trapped) — structural inability to respond rapidly to emerging crises; systematic extraction of response time
 *   - Classified Intelligence Operations: Primary victim (powerless/trapped) — mandatory congressional notification creates compartmentalization failures and operational security dilemmas
 *   - Cold War Consensus Coalition: Organized actors (organized/constrained) — perceive the mechanism as temporary scaffolding designed to hold during existential threat, with implicit sunset upon threat resolution
 *   - Constitutional Separation of Powers Doctrine: Institutional actor (institutional/arbitrage) — persists through constitutional reverence and institutional inertia despite variable functional enforcement (piton perspective)
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing a contingent Cold War institutional arrangement as a universal principle of democratic governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, 0.38).
domain_priors:suppression_score(sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, 0.42).
domain_priors:theater_ratio(sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, extractiveness, 0.38).
narrative_ontology:constraint_metric(sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, tangled_rope).
narrative_ontology:human_readable(sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, "Executive-Legislative Bipartisan Coordination for Foreign Policy (1953 Eisenhower Framework)").
narrative_ontology:topic_domain(sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, "governance/foreign_policy").

domain_priors:requires_active_enforcement(sotu_1953_eisenhower_bipartisan_foreign_policy_coordination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, executive_branch).
narrative_ontology:constraint_beneficiary(sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, legislative_branch).
narrative_ontology:constraint_beneficiary(sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, electoral_legitimacy).
narrative_ontology:constraint_victim(sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, foreign_policy_agility).
narrative_ontology:constraint_victim(sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, classified_decision_making).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FOREIGN POLICY AGILITY (SNARE) — The requirement for genuine bipartisan consensus before major foreign commitments systematically slows response to international crises, intelligence shifts, and emerging threats. The constraint traps agility — it cannot exit, organize, or negotiate. Structural extraction is maximum: coordination friction is the entire purpose of the mechanism.
constraint_indexing:constraint_classification(sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CLASSIFIED INTELLIGENCE AND COVERT OPERATIONS (SNARE) — Congressional oversight requirements force disclosure of classified intelligence to legislative committees, creating security dilemmas. The intelligence community cannot exit this constraint — congressional notification is legally mandated. Extraction runs toward compartmentalization failure and operational risk.
constraint_indexing:constraint_classification(sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MAJORITY PARTY IN CONGRESS (TANGLED ROPE) — Benefits from institutional legitimacy and veto power over major foreign commitments. Also constrained by legislative process friction — cannot unilaterally block, must build consensus. Mixed coordination and extraction: genuine coordination function (builds electoral legitimacy for commitments) combined with forced participation costs (time investment, compromise requirements).
constraint_indexing:constraint_classification(sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: EXECUTIVE BRANCH (ROPE) — Benefits from legislative legitimacy for foreign commitments, increasing domestic support and international credibility. Experiences the bipartisan requirement as coordination mechanism: consensus-building with Congress strengthens executive negotiating position abroad. Can arbitrage executive expertise against congressional oversight. Net beneficiary.
constraint_indexing:constraint_classification(sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ELECTORAL LEGITIMACY (ROPE) — The bipartisan coordination mechanism serves a genuine coordination function: it distributes foreign policy authority between elected branches, preventing unilateral executive war-making and maintaining electoral accountability. Congress represents constituent interests; executive represents executive competence. The mechanism coordinates these two legitimacy sources. Low suppression, clear coordination benefit.
constraint_indexing:constraint_classification(sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: COLD WAR CONSENSUS COALITION (SCAFFOLD) — Organized actors (both parties, executive, congressional leadership) see bipartisan coordination as a temporary framework designed to hold during the Cold War when existential threat makes unity valuable. The sunset clause is implicit: if Cold War ends or if bipartisanship proves unachievable, the mechanism dissolves. Theater ratio moderate-high because much bipartisan activity is performative (public statements of unity for international audience).
constraint_indexing:constraint_classification(sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: SEPARATION OF POWERS DOCTRINE (PITON) — The constitutional principle of shared war powers has persisted for 175+ years with minimal functional enforcement. The bipartisan mechanism treats separation of powers as a living constraint, but in practice it often becomes performative: ex-post-facto congressional authorizations, emergency declarations that bypass consultation, and secret appropriations render the consultation requirement theatrical. Maintained through institutional inertia and constitutional reverence rather than actual function.
constraint_indexing:constraint_classification(sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, bipartisan coordination of foreign policy might appear as an immutable principle: democracies cannot wage war without domestic consensus; executive-legislative coordination is inherent to representative government. However, the base properties reveal this as a false summit. The constraint has clear beneficiaries (both branches gain legitimacy), clear victims (policy agility is extracted), and temporal bounds (the Cold War consensus is specific to 1950s era, not universal).
constraint_indexing:constraint_classification(sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1953_eisenhower_bipartisan_foreign_policy_coordination_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, TR),
    TR >= 0.70.

:- end_tests(sotu_1953_eisenhower_bipartisan_foreign_policy_coordination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts policy agility and compartmentalization from the system in exchange for democratic legitimacy and electoral accountability. This is not maximal extraction — the mechanism does serve genuine coordination functions (legitimizing major commitments, distributing authority between branches). But the extraction is real: the requirement for bipartisan consensus systematically slows response to international events, and the mandatory congressional notification requirement compromises intelligence operations. The extractiveness trajectory increases from 0.18 to 0.38 over the interval because, as Cold War pressure intensifies, the bipartisan mechanism shifts from genuine consensus-building to performative unanimity — the theater_ratio increases from 0.35 to 0.58. Suppression (0.42): Moderate. Neither the executive nor Congress can exit the constraint — it is constitutionally mandated and politically required. But suppression is not maximal because both branches benefit from the arrangement in some contexts (legitimacy), creating bargaining incentives. Theater ratio (0.58): Moderate-high and rising. The initial theater (0.35) reflects genuine debate and negotiation in the 1950s consensus period. As Cold War intensifies and partisan pressures mount (by 1960s), the bipartisan mechanism becomes increasingly performative — public displays of unity mask private disagreements, and Congress increasingly offers post-hoc legitimation rather than pre-decision consultation. By the end of the interval, theater is substantial but not dominant.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates profound perspectival disagreement. The executive sees a coordination mechanism (Rope) that legitimizes its commitments and strengthens its bargaining position — net beneficiary. Congress sees mixed coordination and extraction (Tangled Rope) — veto power benefits them, but legislative friction constrains them. The foreign policy agility sees pure extraction (Snare) — the requirement for bipartisan consensus is an obstacle with no benefit to agility itself. The classified intelligence community sees extraction (Snare) — compartmentalization failure without compensation. The Cold War consensus actors see a temporary framework with a sunset (Scaffold) — bipartisanship is a tool to hold during existential threat. The constitutional doctrine sees itself as eternal (Piton), persisting through reverence rather than function. The civilizational analytical observer risks seeing an immutable principle of democracy (Mountain) but this is a false summit — the constraint is specific to the Cold War era, has clear beneficiaries, and exhibits rising theater_ratio suggesting performative rather than functional operation.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by its structural position relative to the bipartisan constraint. The executive branch benefits from legitimacy (low d ≈ 0.20, negative extraction); Congress benefits from veto power (low d ≈ 0.25, moderate extraction); electoral legitimacy benefits from distributed authority (low d ≈ 0.15, coordination without extraction). Foreign policy agility is systematically constrained (high d ≈ 0.85, maximum extraction); classified intelligence operations face compartmentalization risk (high d ≈ 0.80, high extraction). The Cold War consensus actors are organized but temporarily committed (moderate d ≈ 0.55, moderate extraction + sunsetable); the constitutional doctrine persists through inertia regardless of functional benefit (low d ≈ 0.10, theater-driven rather than extraction-driven). The analytical observer at civilizational scope (d ≈ 0.72, per canonical analytical) risks conflating the specific Cold War institutional arrangement with a universal democratic principle — the engine's false summit detector identifies this as naturalization.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy through temporal decomposition. The bipartisan mechanism is genuinely Rope-like in its early form (1953, genuine consensus-building, low theater) — true coordination between branches. It becomes increasingly Tangled Rope (1960s, partisan pressure, rising theater) as the Cold War deepens and electoral incentives diverge. It approaches Piton-like status (1970s+, performative bipartisanship masking unilateral executive action through post-hoc authorization) as the mechanism's functional capacity declines. The mandatrophy is not 'which type is correct?' but rather 'how does the constraint's class membership change as the coordination function degrades and the theater increases?' The rising theater_ratio (0.35 → 0.58) and rising extractiveness (0.18 → 0.38) are diagnostic signals: the mechanism is transitioning from Rope toward Tangled Rope and eventually Piton, with the false summit (constitutional doctrine eternalism) masking this degradation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bipartisan_consensus_authenticity,
    'Is the bipartisan coordination mechanism producing genuine consensus or theatrical agreement that masks executive unilateralism?',
    'Comparative analysis of cases with early congressional consultation vs. cases with post-hoc authorization. Examination of whether Congress meaningfully shapes foreign commitments or merely legitimizes executive decisions. Historical tracing of major foreign policy reversals initiated by Congress.',
    'If consensus is genuine: rope/tangled_rope classification confirmed, low extraction. If theatrical: snare classification confirmed, high extraction masked by procedural legitimacy. This determines whether the mechanism coordinates or merely performs coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bipartisan_consensus_authenticity, empirical, 'Whether bipartisan coordination is genuine consensus or theatrical legitimacy').

omega_variable(
    cold_war_temporal_specificity,
    'Is the bipartisan mechanism a universal principle of democratic foreign policy or a specific adaptation to Cold War existential threat?',
    'Historical analysis before 1947 (pre-Cold War bipartisanship levels), during Cold War (1947-1991, baseline), and post-1991 (whether bipartisanship persists after existential threat declines). Measurement of congressional assertiveness and executive deference across these periods.',
    'If universal: mountain/rope classification. If Cold War specific: scaffold classification with real sunset logic. If Cold War-dependent then re-emerging: the mechanism oscillates between sunset and active, suggesting deeper extraction cycle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cold_war_temporal_specificity, empirical, 'Whether bipartisanship is universal or Cold War-contingent').

omega_variable(
    classified_information_compromise,
    'How much does mandatory congressional notification compromise operationally sensitive intelligence collection, covert operations, and negotiating positions?',
    'Classified government records (if available through declassification); intelligence community risk assessments of compartmentalization failure; correlation between congressional notification and operational compromise incidents.',
    'If compromise is significant: Congress becomes a victim rather than beneficiary (extraction of intelligence security for democratic legitimacy). If negligible: coordination function is clean. This determines whether the tangled_rope classification is accurate or misses a second extraction layer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(classified_information_compromise, empirical, 'Operational security cost of mandatory congressional notification').

omega_variable(
    majority_party_coalition_stability,
    'Can genuine bipartisan consensus be sustained when one party holds strong partisan interest in foreign policy outcomes?',
    'Measurement of partisan voting patterns on foreign policy authorizations over time. Analysis of instances where one party used foreign policy strategically for electoral gain (e.g., 1968 China card, 1980 hostage crisis). Identification of conditions under which bipartisanship breaks.',
    'If bipartisanship is unstable under partisan pressure: the mechanism is scaffolding-adjacent (temporary during consensual periods, disappears during partisan conflict). If stable: genuine coordination function. If unstable but theater persists: piton classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(majority_party_coalition_stability, empirical, 'Stability of bipartisan consensus under partisan pressure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eisen_tr_t0, sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, theater_ratio, 0, 0.35).
narrative_ontology:measurement(eisen_tr_t5, sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, theater_ratio, 5, 0.48).
narrative_ontology:measurement(eisen_tr_t10, sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(eisen_be_t0, sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(eisen_be_t5, sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(eisen_be_t10, sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, senate_treaty_ratification_authority).
narrative_ontology:affects_constraint(sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, war_powers_resolution_1973).
narrative_ontology:affects_constraint(sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, intelligence_authorization_oversight).

% DUAL FORMULATION NOTE:
% The Eisenhower bipartisan mechanism is upstream to three downstream institutional constraints: Senate treaty ratification (which it must coordinate with), the 1973 War Powers Resolution (which formalized and tightened the consultation requirement), and intelligence authorization oversight (which added mandatory notification requirements). Each downstream constraint has its own extractiveness based on how effectively it enforces the bipartisan principle. See those stories for ε comparison.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, institutional, 0.18).
constraint_indexing:directionality_override(sotu_1953_eisenhower_bipartisan_foreign_policy_coordination, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
