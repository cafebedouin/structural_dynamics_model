% ============================================================================
% CONSTRAINT STORY: magistracies_and_collegiality__collegial_veto_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magistracies_and_collegiality__collegial_veto_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: magistracies_and_collegiality__collegial_veto_reading
 *   human_readable: Magistracies and Collegiality: The Collegial Veto Reading
 *   domain: legal/doctrinal/constitutional
 *
 * SUMMARY:
 *   The magistracies of the Roman Republic embodied a fundamental
 *   constitutional doctrine: that power itself must be checked at the moment
 *   of its exercise, not after. The collegial veto reading instantiates one
 *   specific solution to this problem — the mechanism by which every power is
 *   held jointly and every colleague holds a veto over the other's actions.
 *   In this reading, the magistracy system is understood primarily as a
 *   coordination apparatus: collegiality prevents unilateral command, forces
 *   negotiation, and distributes decision-making authority across multiple
 *   agents so that no single magistrate can act as an autocrat. This reading
 *   coexists with two other structural readings of the same magistracy
 *   kernel: the cursus honorum reading (which emphasizes the ladder structure
 *   of offices and career socialization of ambition) and the term limit
 *   reading (which emphasizes that power expires by calendar, not by action).
 *   This constraint story generates the collegial veto reading as a pure Rope
 *   constraint from the perspective of the collective body it protects, while
 *   acknowledging that the veto appears as obstruction from the perspective
 *   of decisive executive action, and as a natural law from the perspective
 *   of the civilizational analyst.
 *
 * KEY AGENTS:
 *   - The Senatorial Oligarchy: Collective beneficiary (organized/constrained) — benefits from veto structure that prevents unilateral command and enables consensus-based governance
 *   - The Junior Magistrate: Veto-holder (moderate/constrained) — has genuine veto power enabling coordination, but faces career constraints and social pressure on its use
 *   - The Executive Impulse: Suppressed alternative (powerless/trapped) — any attempt at unilateral command is structurally blocked; no exit from the constraint
 *   - The Aristocratic Order: Systemic beneficiary (institutional/arbitrage) — the order as a whole benefits from the veto preventing any single member from consolidating power threatening the whole
 *   - The Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the collegiate veto as an immutable law of governance rather than recognizing it as a contingent institutional choice among alternatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magistracies_and_collegiality__collegial_veto_reading, 0.28).
domain_priors:suppression_score(magistracies_and_collegiality__collegial_veto_reading, 0.32).
domain_priors:theater_ratio(magistracies_and_collegiality__collegial_veto_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magistracies_and_collegiality__collegial_veto_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(magistracies_and_collegiality__collegial_veto_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(magistracies_and_collegiality__collegial_veto_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magistracies_and_collegiality__collegial_veto_reading, rope).
narrative_ontology:human_readable(magistracies_and_collegiality__collegial_veto_reading, "Magistracies and Collegiality: The Collegial Veto Reading").
narrative_ontology:topic_domain(magistracies_and_collegiality__collegial_veto_reading, "legal/doctrinal/constitutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magistracies_and_collegiality__collegial_veto_reading, 'a2c6546d-bbdd-49fd-89db-3203537783f2').
narrative_ontology:cs_kernel_codification('a2c6546d-bbdd-49fd-89db-3203537783f2', formalized).
narrative_ontology:cs_authority_grounding('a2c6546d-bbdd-49fd-89db-3203537783f2', lineage).
narrative_ontology:cs_interpretation_layer_present('a2c6546d-bbdd-49fd-89db-3203537783f2').
narrative_ontology:cs_reading_relation('a2c6546d-bbdd-49fd-89db-3203537783f2', magistracies_and_collegiality__cursus_honorum_reading, coexists_with).
narrative_ontology:cs_reading_relation('a2c6546d-bbdd-49fd-89db-3203537783f2', magistracies_and_collegiality__term_limit_reading, coexists_with).
narrative_ontology:cs_axiom('a2c6546d-bbdd-49fd-89db-3203537783f2', foundational, power_held_jointly_prevents_tyranny).
narrative_ontology:cs_axiom_status(power_held_jointly_prevents_tyranny, holdable).
narrative_ontology:cs_axiom_grounding('a2c6546d-bbdd-49fd-89db-3203537783f2', power_held_jointly_prevents_tyranny, deontological).
narrative_ontology:cs_axiom('a2c6546d-bbdd-49fd-89db-3203537783f2', foundational, colleague_veto_is_legitimate_coordination).
narrative_ontology:cs_axiom_status(colleague_veto_is_legitimate_coordination, holdable).
narrative_ontology:cs_axiom_grounding('a2c6546d-bbdd-49fd-89db-3203537783f2', colleague_veto_is_legitimate_coordination, conventional).
narrative_ontology:cs_reference_frame('a2c6546d-bbdd-49fd-89db-3203537783f2', collegial_magistracy_order).
narrative_ontology:cs_drift_state('a2c6546d-bbdd-49fd-89db-3203537783f2', late_republic_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a2c6546d-bbdd-49fd-89db-3203537783f2', '').
narrative_ontology:cs_kernel_id(magistracies_and_collegiality__collegial_veto_reading, magistracies_and_collegiality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magistracies_and_collegiality__collegial_veto_reading, political_body_protected_from_unilateral_command).
narrative_ontology:constraint_beneficiary(magistracies_and_collegiality__collegial_veto_reading, junior_magistrate_with_veto_power).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SENATORIAL OLIGARCHY (ROPE) — The Senate as collective body benefits from the veto structure that prevents any single consul from commanding unilaterally. Collegiality serves coordination: it enables the Senate to govern through consensus-building rather than submission to imperium. The constraint solves the collective action problem of how to exercise power without ceding it to any single agent. Suppression is moderate — the veto binds all magistrates equally, but senior magistrates perceive this as legitimate coordination cost rather than extraction.
constraint_indexing:constraint_classification(magistracies_and_collegiality__collegial_veto_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: JUNIOR MAGISTRATE (TANGLED ROPE) — Holds genuine veto power by collegiate right, enabling real coordination and constraint on senior colleague's command. But also locked into a power structure where seniority, age, and prior office determine access to real authority over time. The junior magistrate has agency (the veto) but faces career risk and social pressure if exercising it appears factious. Mixed experience: genuine coordination benefit (the veto protects the Republic) plus asymmetric constraint (career mobility depends on not using it provocatively).
constraint_indexing:constraint_classification(magistracies_and_collegiality__collegial_veto_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXECUTIVE IMPULSE / UNILATERAL COMMAND (SNARE) — Any attempt at decisive single command is structurally barred. The constraint extracts compliance cost from whoever seeks to act alone: they must negotiate, persuade, or defer to their colleague. This perspective treats collegiality as a structural trap on executive action itself. No exit — the magistrate cannot shed their colleague or the veto. Maximum suppression of the alternatives (unilateral decision, military command without confirmation).
constraint_indexing:constraint_classification(magistracies_and_collegiality__collegial_veto_reading, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: ARISTOCRATIC ORDER (ROPE) — The collegiality constraint serves the institutional interest of preserving the aristocracy's collective power against the rise of individual strongmen or charismatic commanders. It is a coordination mechanism protecting a class rather than extracting from it. The order benefits from the veto structure because it prevents any single member from consolidating power that could threaten the whole. This is Rope from the perspective of the institution that designed and maintains the constraint.
constraint_indexing:constraint_classification(magistracies_and_collegiality__collegial_veto_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal civilizational view, collegiality appears as an immutable structural feature: shared command is the only way to prevent tyranny; any system that concentrates power in one agent will collapse into despotism. This reading treats the veto and joint office-holding as natural laws of good governance. However, the structural data reveals this as a false-summit candidate: the collegiate veto is a contingent institutional choice, not a necessity of nature.
constraint_indexing:constraint_classification(magistracies_and_collegiality__collegial_veto_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magistracies_and_collegiality__collegial_veto_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(magistracies_and_collegiality__collegial_veto_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(magistracies_and_collegiality__collegial_veto_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(magistracies_and_collegiality__collegial_veto_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The collegiate veto does extract from unilateral executive action — it forces negotiation and compromise, delaying and constraining single-agent decision-making. But the extraction is low because the constraint primarily serves a coordination function: the veto enables the collective body to govern and makes decisions more robust through consensus. The extractiveness is not the rent-seeking of a constraint designed to privilege one faction, but the necessary friction of coordination. Suppression (0.32): Moderate. The veto structure suppresses the alternatives (unilateral command, decisive single action) effectively, but not absolutely. Magistrates retain options to persuade, negotiate, appeal to shared norms, or escalate to the Senate. There are costs to working around the veto, but they are not insurmountable. Theater ratio (0.55): Moderate. The collegial veto involves performative elements (formal consultation, ceremonial acknowledgment of each colleague's authority), but also genuine functional coordination. The constraint is neither purely theatrical nor purely functional — both dimensions are real.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates three distinct perspectives on the same structural mechanism. The senatorial oligarchy and aristocratic order see the veto as Rope (pure coordination enabling collective power). The junior magistrate sees Tangled Rope (genuine veto power mixed with career constraints that limit its use). The executive impulse sees Snare (the veto is a structural trap on decisive action). The analytical observer risks seeing Mountain (collegiality as a natural law of governance), but the structural data (identifiable beneficiaries, contingent institutional design) reveals this as a false summit. The perspectival gap reveals that the constraint's function depends entirely on context: it is coordination for the collective body, obstruction for individual initiative, and political safety for the order. The false summit indicates that the 'natural law' reading serves the institutional interests of those who benefit from preventing change.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from their structural relationship to the constraint. The senatorial oligarchy and aristocratic order are beneficiaries (low d, negative effective extraction) — the constraint serves their interest in collective governance without ceding power to any single member. The junior magistrate is a moderate agent with constrained exit and mixed structural role (holder of veto power but trapped in a hierarchy) — moderate d producing moderate effective extraction from their perspective. The executive impulse (unilateral command) faces maximal suppression — high d approaching 1.0, experiencing the veto as a trap. The analytical observer risks naturalizing the constraint, which the false-summit detector reveals: the constraint has identifiable beneficiaries and is a contingent institutional choice, not a natural law.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that the classification type (Rope) is consistent with the structural data (low extractiveness, moderate suppression, moderate theater) and stable across the perspectives that genuinely experience the constraint as coordination (senatorial oligarchy, aristocratic order). The tangled rope perspective (junior magistrate) shows mixed coordination and constraint, which is coherent. The snare perspective (executive impulse) shows obstruction, which is consistent with the extraction mechanism. The false summit (mountain/analytical observer) is properly detected: the constraint has identifiable beneficiaries (the collective body) and is a contingent institutional choice (not a natural law). The theater ratio's increase over time (0.48 → 0.62) indicates that the performative elements grow as the constraint matures, suggesting the classical trajectory from functional coordination to ritualized maintenance. This is consistent with eventual Piton degradation if the constraint persists beyond its functional necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_as_coordination_vs_obstruction,
    'Does the veto primarily function as coordination (enabling collective decision-making) or as obstruction (preventing efficient executive action)?',
    'Historical case analysis: compare outcomes when veto is exercised cooperatively (both magistrates aligned) vs. factiously (colleagues blocking each other''s initiatives). Measure speed of decision-making and stability of outcomes across periods of strong vs. weak collegiality.',
    'If veto is primarily coordinating: constraint is Rope from most perspectives, extractiveness < 0.35. If veto is primarily obstructive: constraint is Tangled Rope or Snare, extractiveness > 0.40.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_as_coordination_vs_obstruction, empirical, 'Whether the veto functions as coordination or obstruction').

omega_variable(
    kernel_contest_natural_law_status,
    'Is collegiality a natural law of governance (mountain), or a contingent institutional reading of magistracies that coexists with other legitimate readings (cursus honorum, term limits)?',
    'Comparative institutional analysis: examine magistracies systems with weak or absent veto mechanisms (military command structures, provincial governors with sole authority) and assess their stability and failure modes. Determine whether veto is necessary or merely sufficient for preventing tyranny.',
    'If collegiality is natural law: classification is Mountain from all perspectives (high accessibility_collapse, low resistance). If collegiality is contingent reading: classification is Rope/Tangled Rope, and the mountain perspective in this reading becomes a false summit (beneficiaries disguise institutional choice as natural necessity).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_natural_law_status, conceptual, 'Whether collegiality is a natural law or a contingent institutional reading').

omega_variable(
    sibling_reading_compatibility,
    'Can the collegial veto reading coexist coherently with the cursus honorum reading and the term limit reading within a single magistracy framework, or does the veto reading foreclose one or both of them?',
    'Logical consistency analysis: model each reading as a set of axioms and constraints; check whether all three can be simultaneously true in the same institutional framework. Historical test: verify whether Rome''s actual magistracy system combined all three readings or if institutional evolution forced choices.',
    'If all three coexist: each reading is independent (coexists_with relation). If veto forecloses cursus honorum or term limits: establish the logical contradiction. If veto is downstream of one sibling: establish influences relation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_compatibility, conceptual, 'Logical compatibility of the three magistracy readings').

omega_variable(
    veto_exercise_frequency_dynamics,
    'Does the actual exercise of veto power decline over time as colleagues develop trust and shared norms, or does veto exercise remain constant, indicating that coordination benefit is independent of frequency?',
    'Historical record analysis: count veto blocks, collegial disputes, and cases where magistrates refused to cooperate across time periods. Separate incidents into categories: legitimate coordination disputes (both magistrates acting in good faith) vs. factious obstruction (one magistrate blocking for personal gain). Track whether periods of high veto exercise correlate with instability or improved checks on arbitrary power.',
    'If veto exercise declines (trust builds, norm compliance grows): constraint''s extractiveness on the executive impulse diminishes over time, suggesting it is primarily coordinating. If veto exercise remains constant or becomes performative: extractiveness may increase (theater rises), suggesting constraint transitions to Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_exercise_frequency_dynamics, empirical, 'Temporal trajectory of actual veto exercise').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magistracies_and_collegiality__collegial_veto_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(collegveto_tr_t0, magistracies_and_collegiality__collegial_veto_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(collegveto_tr_t100, magistracies_and_collegiality__collegial_veto_reading, theater_ratio, 100, 0.55).
narrative_ontology:measurement(collegveto_tr_t200, magistracies_and_collegiality__collegial_veto_reading, theater_ratio, 200, 0.62).

% Extraction over time
narrative_ontology:measurement(collegveto_be_t0, magistracies_and_collegiality__collegial_veto_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(collegveto_be_t100, magistracies_and_collegiality__collegial_veto_reading, base_extractiveness, 100, 0.28).
narrative_ontology:measurement(collegveto_be_t200, magistracies_and_collegiality__collegial_veto_reading, base_extractiveness, 200, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magistracies_and_collegiality__collegial_veto_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magistracies_and_collegiality__collegial_veto_reading, magistracies_and_collegiality__cursus_honorum_reading).
narrative_ontology:affects_constraint(magistracies_and_collegiality__collegial_veto_reading, magistracies_and_collegiality__term_limit_reading).

% DUAL FORMULATION NOTE:
% The magistracies_and_collegiality kernel decomposes into three structurally distinct readings: collegial_veto_reading (this story), cursus_honorum_reading, and term_limit_reading. Each reading instantiates a different foundational principle (collegiality, career ladder, calendar) and produces a different constraint with different extractiveness and suppression values. All three readings coexist in the historical magistracy system, but each reading emphasizes a different mechanism. The three stories are linked through the kernel_id and should be read as a family. The collegial_veto_reading affects the other readings because the veto mechanism creates structural pressure on how the career ladder and calendar function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
