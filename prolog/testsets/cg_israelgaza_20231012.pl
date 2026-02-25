% ============================================================================
% CONSTRAINT STORY: cg_israelgaza_20231012
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-10-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cg_israelgaza_20231012, []).

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
 *   constraint_id: cg_israelgaza_20231012
 *   human_readable: Israeli Blockade of Gaza Strip
 *   domain: political/humanitarian
 *
 * SUMMARY:
 *   The Israeli blockade of the Gaza Strip, imposed in 2007 and severely
 *   intensified in October 2023, is a system of control restricting the
 *   movement of people and goods. Israel cites security concerns, primarily
 *   preventing weapons from reaching Hamas, as its rationale. However, human
 *   rights organizations and the UN have described it as a form of collective
 *   punishment, leading to a severe, long-term humanitarian crisis and the
 *   collapse of the Gazan economy. The constraint is actively maintained
 *   through military control of land, air, and sea access, with Egypt
 *   controlling the only other border crossing at Rafah.
 *
 * KEY AGENTS:
 *   - Gaza Civilian Population: Primary victim (powerless/trapped) — bears the full humanitarian and economic cost with no exit.
 *   - Israeli Security Establishment: Primary beneficiary (institutional/arbitrage) — enforces the blockade to achieve security and political objectives, controlling its terms.
 *   - Hamas Governance: Secondary victim/beneficiary (organized/constrained) — the stated target of the blockade, yet the siege also consolidates its internal power.
 *   - International Aid Organizations: Mitigating actor (organized/constrained) — operates a temporary support structure made necessary by the crisis.
 *   - Analytical Observers: Includes international legal bodies and 'realpolitik' analysts who frame the constraint in starkly different terms (Snare vs. Mountain).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cg_israelgaza_20231012, 0.85).
domain_priors:suppression_score(cg_israelgaza_20231012, 0.95).
domain_priors:theater_ratio(cg_israelgaza_20231012, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cg_israelgaza_20231012, extractiveness, 0.85).
narrative_ontology:constraint_metric(cg_israelgaza_20231012, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(cg_israelgaza_20231012, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cg_israelgaza_20231012, snare).
narrative_ontology:human_readable(cg_israelgaza_20231012, "Israeli Blockade of Gaza Strip").
narrative_ontology:topic_domain(cg_israelgaza_20231012, "political/humanitarian").

domain_priors:requires_active_enforcement(cg_israelgaza_20231012).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cg_israelgaza_20231012, israeli_security_establishment).
narrative_ontology:constraint_victim(cg_israelgaza_20231012, gaza_civilian_population).
narrative_ontology:constraint_victim(cg_israelgaza_20231012, gaza_economy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE TRAPPED POPULATION (SNARE) — Experiences the blockade as totalizing coercion. With no exit, bearing the full humanitarian and economic cost, the structure is pure extraction. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈1.14. This is the canonical victim experience.
constraint_indexing:constraint_classification(cg_israelgaza_20231012, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE ISRAELI STATE (ROPE) — As the enforcer and primary beneficiary, the state experiences the blockade as a security coordination mechanism. It controls the terms (arbitrage exit) and externalizes all costs. d≈0.05, f(d)≈-0.12, σ=0.9 → χ≈-0.09. The negative effective extraction signifies a net subsidy from the constraint.
constraint_indexing:constraint_classification(cg_israelgaza_20231012, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: HAMAS GOVERNANCE (TANGLED ROPE) — As the stated target, Hamas is a victim of the blockade's extraction. However, the siege also serves a coordination function for its internal political control, creating a rally effect and dependency. It is both extracted from and benefits from the structure. d≈0.75, f(d)≈1.10, σ=0.8 → χ≈0.75.
constraint_indexing:constraint_classification(cg_israelgaza_20231012, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: INTERNATIONAL AID ORGANIZATIONS (SCAFFOLD) — These organizations operate a massive support structure made necessary by the blockade. Their entire operation is a temporary scaffold to sustain life, predicated on the aspirational sunset clause of a future political resolution that ends the blockade. Their work is a coordination function (distributing aid) under coercive conditions.
constraint_indexing:constraint_classification(cg_israelgaza_20231012, scaffold,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: THE ANALYTICAL OBSERVER (SNARE) — The default analytical view, which aligns with the claimed_type. The extreme metrics for base extractiveness (0.85) and suppression (0.95), combined with clearly identified victims, make the Snare classification structurally unavoidable. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈1.17.
constraint_indexing:constraint_classification(cg_israelgaza_20231012, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: THE 'REALPOLITIK' ANALYST (MOUNTAIN) — This perspective naturalizes the blockade as an immutable feature of an intractable conflict, a geopolitical 'law of nature'. The engine will identify this as a false summit, as the constraint requires continuous, active military enforcement and has none of the structural properties of a Mountain (e.g., emerges_naturally=false, high ε and suppression).
constraint_indexing:constraint_classification(cg_israelgaza_20231012, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cg_israelgaza_20231012_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cg_israelgaza_20231012, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cg_israelgaza_20231012, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cg_israelgaza_20231012, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cg_israelgaza_20231012_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.85) is extremely high, reflecting the documented economic devastation ($16.7B cost, 50% GDP reduction), mass unemployment (>40%), and near-total aid dependency (80%). The constraint extracts life chances, economic potential, and well-being. Suppression (0.95) is near-total; with military control of air, sea, and two of three land crossings, there are virtually no alternatives for the trapped population. Theater Ratio (0.40) reflects the gap between the stated security rationale and the observed reality of restrictions on food, medicine, and basic goods, which suggests a broader purpose of political and economic pressure beyond pure security screening.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the Israeli state, it is a Rope—a necessary security tool whose costs are externalized. For the Gaza civilian, it is a Snare—a trap that extracts their future. For Hamas, it is a Tangled Rope—a constraint that also serves as a political coordination device. For aid organizations, it is a Scaffold—a temporary structure pending a political solution. For some analysts, it is a Mountain—an intractable feature of the landscape. This diversity demonstrates that the 'type' of a constraint is not absolute but is indexed to the observer's structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   The starkly different classifications emerge directly from the agents' structural positions. The Israeli state, as a beneficiary with arbitrage exit, has a derived directionality `d` near 0, producing a negative chi (Rope). The Gaza civilian, as a victim with trapped exit, has a `d` near 1.0, producing a maximal chi (Snare). Hamas and aid organizations, as organized but constrained actors, occupy intermediate positions, leading to Tangled Rope and Scaffold classifications. The analytical observer's default `d` of ~0.72 is high enough to classify the structure as a Snare from a neutral standpoint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy of whether the blockade is a 'security measure' or 'collective punishment'. The DR framework shows it is structurally both, depending on the observer. The Israeli state experiences the security coordination function (Rope). The civilian population experiences the punishment and extraction function (Snare). The analytical perspective, which must account for the immense, undeniable extraction (ε=0.85), correctly identifies the overall structure as a Snare, while acknowledging the beneficiary's perspectival reality. The framework does not have to choose one label; it maps the entire perspectival manifold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_vs_punishment,
    'Is the blockade a narrowly tailored security measure (Rope-like function) or a form of collective punishment (pure Snare function)?',
    'Declassified intelligence on prevented attacks correlated with analysis of goods permitted/denied to determine if the logic is consistent with security needs or broader economic/political pressure.',
    'If proven to be primarily collective punishment, the Snare classification is solidified. If a strong, direct link to preventing specific, credible threats is shown, the Rope/Tangled Rope perspectives gain more weight, though the high ε remains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_vs_punishment, empirical, 'Distinguishing the security rationale from collective punishment.').

omega_variable(
    hamas_culpability_feedback_loop,
    'To what extent do Hamas''s own governance, resource allocation (e.g., tunnels vs. civilian infrastructure), and military actions perpetuate the blockade, creating a feedback loop?',
    'Counterfactual modeling of Israeli policy under a different Gazan authority; analysis of Hamas''s internal budgets and strategic decisions.',
    'High Hamas culpability would strengthen the Tangled Rope classification, framing the constraint as a destructive equilibrium between two actors. Low culpability reinforces the primary Snare classification against the civilian population.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hamas_culpability_feedback_loop, conceptual, 'Quantifying the feedback loop between Hamas''s actions and the blockade''s continuation.').

omega_variable(
    egyptian_strategic_interest,
    'Is Egypt''s role in enforcing the blockade at Rafah primarily driven by coordination with Israel or by its own independent security interests regarding Sinai and the Muslim Brotherhood?',
    'Analysis of Egyptian border policy during periods of varying relations with Israel and Hamas; intelligence on Egyptian security assessments.',
    'If Egypt''s role is independent, the system is a network of two overlapping constraints rather than a single monolithic Snare, complicating exit pathways and diplomatic solutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(egyptian_strategic_interest, empirical, 'Determining if Egypt''s role is independent or coordinated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cg_israelgaza_20231012, 2007, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cg_i_tr_t0, cg_israelgaza_20231012, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cg_i_tr_t8, cg_israelgaza_20231012, theater_ratio, 8, 0.3).
narrative_ontology:measurement(cg_i_tr_t17, cg_israelgaza_20231012, theater_ratio, 17, 0.4).

% Extraction over time
narrative_ontology:measurement(cg_i_be_t0, cg_israelgaza_20231012, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(cg_i_be_t8, cg_israelgaza_20231012, base_extractiveness, 8, 0.75).
narrative_ontology:measurement(cg_i_be_t17, cg_israelgaza_20231012, base_extractiveness, 17, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cg_israelgaza_20231012, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
