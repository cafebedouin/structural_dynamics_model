% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__inherent_executive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__inherent_executive_reading, []).

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
 *   constraint_id: war_powers_allocation__inherent_executive_reading
 *   human_readable: Commander-in-Chief Power: Inherent Executive Authority Reading
 *   domain: constitutional_law/separation_of_powers/war_powers
 *
 * SUMMARY:
 *   The inherent executive reading of commander-in-chief power holds that the
 *   Constitution grants the president structural authority to deploy military
 *   force in defense of national interests without prior congressional
 *   authorization. This reading interprets Article II's vesting of
 *   commander-in-chief authority as conferring independent war-making power,
 *   constrained only by subsequent legislative action (appropriations, formal
 *   declaration) rather than requiring legislative approval as a condition of
 *   deployment. The constraint exhibits a characteristic tangled-rope
 *   structure: genuine coordination function (unified command, operational
 *   flexibility, rapid response to security threats) coexists with asymmetric
 *   extraction (executive unilateral action reduces Congress to a post-hoc
 *   ratifying body). The measurement trajectory shows extraction accumulating
 *   over the interval: base extractiveness rising from 0.35 (post-1973 War
 *   Powers Resolution) through 0.48 (post-9/11 broad AUMFs) to 0.58
 *   (contemporary). Suppression has correspondingly intensified (0.48 → 0.68)
 *   as executive precedent has normalized unilateral deployment. Theater
 *   ratio remains moderate (0.42 → 0.55), reflecting that while AUMF
 *   authorization provides performative legislative cover, the underlying
 *   mechanism involves genuine operational decisions, not pure performative
 *   ritual.
 *
 * KEY AGENTS:
 *   - Executive Branch: Primary beneficiary (institutional/arbitrage) — gains unilateral action authority and operational initiative; can deploy force first and seek authorization after
 *   - Congress: Primary victim (organized/constrained) — structurally weakened from authorization authority to post-hoc ratification body; high political friction prevents meaningful override
 *   - Legislative Constraint Authority (War Powers Resolution): Secondary victim (powerless/trapped) — statutory framework undermined by constitutional interpretation; cannot defend itself without Congress repudiating its own statute
 *   - Judiciary: Powerful actor (powerful/mobile) — maintains interpretive authority but constrained by political question doctrine; can exit through jurisdictional doctrine but institutional norms discourage it
 *   - Military-operational system: Beneficiary-adjacent (institutional/arbitrage) — benefits from unified command and executive flexibility; operationally coordinated but not primary extraction target
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing a contested reading as constitutive necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__inherent_executive_reading, 0.58).
domain_priors:suppression_score(war_powers_allocation__inherent_executive_reading, 0.68).
domain_priors:theater_ratio(war_powers_allocation__inherent_executive_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__inherent_executive_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__inherent_executive_reading, "Commander-in-Chief Power: Inherent Executive Authority Reading").
narrative_ontology:topic_domain(war_powers_allocation__inherent_executive_reading, "constitutional_law/separation_of_powers/war_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__inherent_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__inherent_executive_reading, '67682a78-6f8e-43b1-aa7d-f6b5560c96f5').
narrative_ontology:cs_kernel_codification('67682a78-6f8e-43b1-aa7d-f6b5560c96f5', fixed_text).
narrative_ontology:cs_authority_grounding('67682a78-6f8e-43b1-aa7d-f6b5560c96f5', lineage).
narrative_ontology:cs_interpretation_layer_present('67682a78-6f8e-43b1-aa7d-f6b5560c96f5').
narrative_ontology:cs_reading_relation('67682a78-6f8e-43b1-aa7d-f6b5560c96f5', war_powers_allocation__congressional_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('67682a78-6f8e-43b1-aa7d-f6b5560c96f5', war_powers_allocation__functional_accommodation_reading, influences).
narrative_ontology:cs_axiom('67682a78-6f8e-43b1-aa7d-f6b5560c96f5', foundational, executive_inherent_commander_in_chief_authority).
narrative_ontology:cs_axiom_status(executive_inherent_commander_in_chief_authority, holdable).
narrative_ontology:cs_axiom_grounding('67682a78-6f8e-43b1-aa7d-f6b5560c96f5', executive_inherent_commander_in_chief_authority, deontological).
narrative_ontology:cs_axiom('67682a78-6f8e-43b1-aa7d-f6b5560c96f5', foundational, unilateral_deployment_authorized_by_text).
narrative_ontology:cs_axiom_status(unilateral_deployment_authorized_by_text, holdable).
narrative_ontology:cs_axiom_grounding('67682a78-6f8e-43b1-aa7d-f6b5560c96f5', unilateral_deployment_authorized_by_text, deontological).
narrative_ontology:cs_reference_frame('67682a78-6f8e-43b1-aa7d-f6b5560c96f5', framers_unitary_executive_design).
narrative_ontology:cs_drift_state('67682a78-6f8e-43b1-aa7d-f6b5560c96f5', contemporary_post_911_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('67682a78-6f8e-43b1-aa7d-f6b5560c96f5', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__inherent_executive_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, executive_branch).
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, presidential_military_prerogative).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, legislative_constraint_authority).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, statutory_war_powers_framework).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STATUTORY WAR POWERS CONSTRAINT (SNARE) — The War Powers Resolution (1973) is structurally trapped: Congress voted to constrain itself, but the inherent executive reading treats the statute as unconstitutional limitation on inherent authority. The statute cannot exit this constraint without repudiating itself. Trapped in pure extraction: executive authority uses commander-in-chief clause to override statutory limits, bearing maximum suppression (congressional avenue blocked).
constraint_indexing:constraint_classification(war_powers_allocation__inherent_executive_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONGRESS AS ORGANIZED AGENT (TANGLED ROPE) — Congress retains coordination function (appropriations, treaty ratification, formal declarations) but faces extraction: presidential unilateral action reduces legislative constraint to ex-post ratification via appropriations. Congress can organize and delay, but cannot prevent. Constrained exit: formal veto power exists legally but politically costly to exercise. Mixed: genuine coordination function (war powers do require funding, which Congress provides) plus asymmetric extraction (presidential initiative + congressional reaction).
constraint_indexing:constraint_classification(war_powers_allocation__inherent_executive_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXECUTIVE BRANCH (ROPE) — Experiences the constraint as coordination: military readiness requires unified command, rapid decision-making, and executive operational flexibility. The commander-in-chief clause solves the genuine coordination problem of deploying force without legislative paralysis. Net beneficiary with arbitrage options: can act first and seek authorization later (appropriations), retaining initiative advantage.
constraint_indexing:constraint_classification(war_powers_allocation__inherent_executive_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: JUDICIARY (TANGLED ROPE) — Powerful actors with mobility (can decline jurisdiction or interpret narrowly) but still trapped by the political question doctrine. Courts see genuine coordination value (unified command structure) and genuine extraction (executive unilateralism reduces judicial review access). Mobile exit available through jurisdictional doctrine, but institutional norms constrain exit. Low suppression of judicial alternatives (courts have interpretive authority) but high political friction.
constraint_indexing:constraint_classification(war_powers_allocation__inherent_executive_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / CONSTITUTIVE POWER (MOUNTAIN) — From a universal/civilizational frame, executive commander-in-chief authority is a foundational constitutional structure: the clause is textually explicit, historically rooted in separation of powers theory, and structurally necessary for operational coherence. This perspective risks naturalizing what is actually a contested reading. The engine will identify this as a false summit: the 'necessity' is interpretive, not natural law.
constraint_indexing:constraint_classification(war_powers_allocation__inherent_executive_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: THE AUMF FRAMEWORK (PITON) — Post-9/11 AUMFs (2001 Afghanistan, 2002 Iraq) functionally ratify executive war powers through broad language ('associated forces,' 'continuing threat'). The authorization mechanism is substantially performative: it provides legislative cover for executive action already underway or operationally necessary. Theater ratio high (AUMF language is theater for pre-authorized action); functional extraction low (the military action would proceed regardless). Piton because the mechanism persists through institutional inertia despite degraded original function.
constraint_indexing:constraint_classification(war_powers_allocation__inherent_executive_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__inherent_executive_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(war_powers_allocation__inherent_executive_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(war_powers_allocation__inherent_executive_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__inherent_executive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(war_powers_allocation__inherent_executive_reading, TR),
    TR >= 0.70.

:- end_tests(war_powers_allocation__inherent_executive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The executive gains unilateral action authority, but the extraction is not maximal because Congress retains formal power: appropriations authority, treaty ratification, formal declaration rights. The accumulation over time (0.35 → 0.58) reflects post-9/11 normalization of broad executive deployment authority through AUMF framework. Suppression (0.68): High. Congress faces severe structural barriers to exercising formal war powers: first-mover advantage to executive (forces already deployed), political cost of appearing to restrict military action, public opinion rallying effects, media control by executive branch during crises, and repeated AUMF framework ratifying executive deployment as fait accompli. However, suppression is not total because Congress can theoretically defund, override with statute, or impeach. Theater ratio (0.55): Moderate. AUMFs provide legislative theater (appearance of authorization) for executive action operationally necessary or already underway, but the mechanism involves genuine military decisions, not pure performative ritual like some regulatory theater.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a diagnostic perspectival gap between institutional perspectives. Congress (organized/constrained) sees tangled rope: mixed coordination (military readiness requires unified command) and extraction (reduced to ratification body). Executive (institutional/arbitrage) sees rope: coordination value dominates experienced extraction because they initiate action and select timing. The judiciary (powerful/mobile) sees tangled rope because they experience coordination value (clarity of command) but face structural extraction (political question doctrine prevents judicial review). The statutory framework (powerless/trapped) sees pure snare because it cannot exit or defend itself. The mountain perspective risks false-summit classification: the 'necessity' of inherent executive authority naturalizes what is actually a contested constitutional reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Congress's structural relationship (organized power, constrained exit, victim status) derives d ≈ 0.60-0.65, producing high experienced extractiveness. Executive branch (institutional power, arbitrage exit, beneficiary status) derives d ≈ 0.15-0.25, producing low or negative experienced extractiveness. Congress experiences high chi despite organized power because constrained exit (high political cost to override) amplifies d; executive experiences low chi because arbitrage exit (can act first, seek ratification after) dampens d. The suppression coefficient (0.68) acts uniformly — not scaled by power or scope per the ε-invariance principle — constraining congressional override capacity across all contexts.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The inherent executive reading resolves the war powers mandatrophy by clearly declaring which agent (executive) benefits from the authority allocation and showing mixed function (genuine coordination value + asymmetric extraction). The extraction is NOT disguised as pure coordination — the oxygen variable on appropriations-as-ratification makes explicit that Congress's post-hoc approval is a structural weakening, not genuine co-authorization. The reading commits to the axiom 'executive inherent authority from text' which is distinct from the congressional_primacy_reading axiom 'congressional primacy from text + historical practice.' Both axioms are internally coherent but cannot coexist in a single framework (forecloses relation). The functional_accommodation_reading axiom 'war powers allocation by evolutionary practice' influences but does not foreclose either reading — practice could ratify either textual commitment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_permissiveness_boundary,
    'Does the Constitution''s text (''commander in chief of the Army and Navy'') grant inherent war-making authority or merely designate operational command within wars Congress authorizes?',
    'Originalist exegesis of Founding-era understanding; comparative analysis with analogous constitutional democracies; historical practice analysis (Washington, Lincoln, FDR precedents)',
    'If inherent: snare classification for Congress is correct. If operational-only: tangled_rope or rope classification from executive perspective becomes inaccurate; constraint becomes congressional primacy structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textual_permissiveness_boundary, conceptual, 'Whether commander-in-chief clause grants inherent war-making power or operational command only').

omega_variable(
    appropriations_ratification_mechanism,
    'Does congressional appropriation of military funds constitute post-hoc authorization of executive force deployments, or is it a separate structural constraint on executive action?',
    'Case law analysis of congressional appropriation as explicit vs implicit authorization; historical instances of appropriations withheld from ongoing military actions; constitutional scholar consensus on appropriations doctrine',
    'If constitutes authorization: executive action is formally validated after-the-fact, reducing snare classification severity. If separate: appropriations are a tardy structural check, not authorization, and snare classification is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appropriations_ratification_mechanism, empirical, 'Whether appropriations function as post-hoc authorization or separate constraint').

omega_variable(
    reading_relationship_to_functional_accommodation,
    'Does the inherent executive reading foreclose, coexist with, or influence the functional_accommodation_reading (which treats war powers as evolutionarily determined by practice rather than textual grant)?',
    'Logical analysis of reading commitments: if both readings claim the same allocation mechanism (inherent grant vs evolved practice), they may foreclose; if they arrive at similar outcomes via different justifications, they coexist; if one creates conditions that pressure the other, they influence.',
    'If foreclose: one reading must be abandoned. If coexist: both remain live options. If influence: one reading creates resource or legitimacy pressure on the other.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relationship_to_functional_accommodation, conceptual, 'Logical relationship between inherent executive and functional accommodation readings').

omega_variable(
    congressional_primacy_suppression_paradox,
    'Why does Congress (organized, powerful) experience high suppression (0.68) under this reading, when Congress retains formal power to declare war and control appropriations?',
    'Political economy analysis: congressional collective action costs, executive first-mover advantage, public opinion rallying effects, media framing dominance. Quantify: how often has Congress withheld appropriations or overridden executive deployment? Frequency of veto threats vs actual vetoes.',
    'If suppression is political rather than structural: the constraint is weaker than coded. Congress''s formal power is suppressed by political friction, not by constitutional architecture. If suppression is structural: constitutional design actively prevents congressional override.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congressional_primacy_suppression_paradox, empirical, 'Source of congressional suppression: constitutional architecture vs political economy').

omega_variable(
    false_summit_natural_law_charge,
    'Is the inherent executive authority reading grounded in discovered constitutional principle (mountain), or is it a contested interpretive reading that benefits the executive and therefore risks FNL classification?',
    'Identify beneficiary (executive branch gains unilateral action authority). Identify alternative readings (congressional_primacy_reading, functional_accommodation_reading). If alternative readings exist with internal logical coherence, this is NOT a natural law but a contingent interpretation that benefits a named agent.',
    'If natural law: accessibility_collapse and resistance metrics should show immutability. If contingent reading: beneficiary presence triggers false_summit_mountain evaluation in the engine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_charge, conceptual, 'Whether inherent executive reading is discovered constitutional principle or contingent interpretation benefiting executive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__inherent_executive_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(warpow_theater_t0_post_wpa, war_powers_allocation__inherent_executive_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(warpow_theater_t10_post_911, war_powers_allocation__inherent_executive_reading, theater_ratio, 10, 0.52).
narrative_ontology:measurement(warpow_theater_t20_contemporary, war_powers_allocation__inherent_executive_reading, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(warpow_extract_t0_post_wpa, war_powers_allocation__inherent_executive_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(warpow_extract_t10_post_911, war_powers_allocation__inherent_executive_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(warpow_extract_t20_contemporary, war_powers_allocation__inherent_executive_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(warpow_suppress_t0_post_wpa, war_powers_allocation__inherent_executive_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(warpow_suppress_t10_post_911, war_powers_allocation__inherent_executive_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(warpow_suppress_t20_contemporary, war_powers_allocation__inherent_executive_reading, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__inherent_executive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, war_powers_allocation__congressional_primacy_reading).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, war_powers_allocation__functional_accommodation_reading).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, aumf_ratification_mechanism).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, presidential_emergency_powers).

% DUAL FORMULATION NOTE:
% The war_powers_allocation kernel decomposes into three distinct readings with different ε values and beneficiary/victim allocations. inherent_executive_reading (ε≈0.58) treats executive authority as textually foundational. congressional_primacy_reading (ε≈0.22, expected) treats Congress as primary authority with executive narrow exception. functional_accommodation_reading (ε≈0.45, expected) treats allocation as evolved through practice rather than textually determined. All three stories should link via network.affects_constraints to model the kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_powers_allocation__inherent_executive_reading, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
