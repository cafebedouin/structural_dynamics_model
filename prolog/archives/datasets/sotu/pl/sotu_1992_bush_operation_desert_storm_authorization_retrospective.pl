% ============================================================================
% CONSTRAINT STORY: sotu_1992_bush_operation_desert_storm_authorization_retrospective
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1992_bush_operation_desert_storm_authorization_retrospective, []).

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
 *   constraint_id: sotu_1992_bush_operation_desert_storm_authorization_retrospective
 *   human_readable: Congressional Endorsement of Operation Desert Storm and Presidential War Powers Precedent (1991-1992)
 *   domain: governance/constitutional_separation_of_powers
 *
 * SUMMARY:
 *   Operation Desert Storm (1990-1991) and President Bush's framing of
 *   congressional authorization in his 1992 State of the Union address
 *   established a structural precedent for presidential war powers
 *   constrained by legislative approval and retrospective outcome
 *   vindication. This constraint operates at the intersection of
 *   constitutional separation of powers, institutional norms, and military
 *   authority. Bush's decision to seek authorization from Congress despite
 *   having broad executive discretion under the Commander-in-Chief clause
 *   created a political precedent that subsequent presidents have felt
 *   obligated to follow. The constraint is not purely coordinative (pure
 *   Rope) because it redistributes power asymmetrically away from the
 *   executive, nor is it purely extractive (pure Snare) because it also
 *   creates legitimacy benefits and shared accountability. Instead, it is a
 *   hybrid mechanism that embodies both coordination (shared responsibility
 *   for war) and extraction (executive loss of unilateral initiative), making
 *   it a paradigmatic Tangled Rope at the institutional level. The
 *   constraint's durability depends on whether the bipartisan consensus norm
 *   can survive in polarized political environments and whether the
 *   authorization requirement can be satisfied within operationally
 *   acceptable timescales.
 *
 * KEY AGENTS:
 *   - President Bush (Executive Branch): Primary agent establishing the precedent (institutional/arbitrage) — benefits from congressional legitimacy but constrained by authorization requirement
 *   - Congress (Legislative Body): Primary beneficiary (institutional/arbitrage) — restores war powers authority that had drifted to executive; gains bipartisan consensus mechanism
 *   - Future Military Commanders: Secondary victim (powerless/trapped) — bound by precedent that authorization is structurally required before unilateral action
 *   - Wartime Public and Military Personnel: Mixed stakeholder (moderate/constrained) — benefits from legitimacy and shared responsibility; constrained by requirement for measurable outcome vindication
 *   - Constitutional Reform Advocates: Tertiary beneficiary (powerful/mobile) — view constraint as temporary scaffolding for restored separation of powers with sunset logic
 *   - War Powers Resolution Enforcement System: Institutional mechanism (institutional/arbitrage) — persists through performative authorization ritual rather than genuine enforcement power
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent precedent as immutable constitutional law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1992_bush_operation_desert_storm_authorization_retrospective, 0.38).
domain_priors:suppression_score(sotu_1992_bush_operation_desert_storm_authorization_retrospective, 0.42).
domain_priors:theater_ratio(sotu_1992_bush_operation_desert_storm_authorization_retrospective, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1992_bush_operation_desert_storm_authorization_retrospective, extractiveness, 0.38).
narrative_ontology:constraint_metric(sotu_1992_bush_operation_desert_storm_authorization_retrospective, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(sotu_1992_bush_operation_desert_storm_authorization_retrospective, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1992_bush_operation_desert_storm_authorization_retrospective, tangled_rope).
narrative_ontology:human_readable(sotu_1992_bush_operation_desert_storm_authorization_retrospective, "Congressional Endorsement of Operation Desert Storm and Presidential War Powers Precedent (1991-1992)").
narrative_ontology:topic_domain(sotu_1992_bush_operation_desert_storm_authorization_retrospective, "governance/constitutional_separation_of_powers").

domain_priors:requires_active_enforcement(sotu_1992_bush_operation_desert_storm_authorization_retrospective).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1992_bush_operation_desert_storm_authorization_retrospective, constitutional_separation_of_powers).
narrative_ontology:constraint_beneficiary(sotu_1992_bush_operation_desert_storm_authorization_retrospective, legislative_oversight_norm).
narrative_ontology:constraint_beneficiary(sotu_1992_bush_operation_desert_storm_authorization_retrospective, bipartisan_war_consensus).
narrative_ontology:constraint_victim(sotu_1992_bush_operation_desert_storm_authorization_retrospective, executive_speed_and_flexibility).
narrative_ontology:constraint_victim(sotu_1992_bush_operation_desert_storm_authorization_retrospective, future_unilateral_military_initiative).
narrative_ontology:constraint_victim(sotu_1992_bush_operation_desert_storm_authorization_retrospective, rapid_response_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTRAINED MILITARY COMMANDER (SNARE) — Future military leadership faces binding precedent that congressional authorization is structurally required before unilateral action. No exit from the template: success requires legislative approval and measurable outcome validation. Commander bears full extraction cost: delayed deployment windows, operational constraints from political deliberation, public commitment to specific objectives that may shift. Maximum experience of constraint as coercive apparatus.
constraint_indexing:constraint_classification(sotu_1992_bush_operation_desert_storm_authorization_retrospective, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EXECUTIVE BRANCH (TANGLED ROPE) — Gains coordination benefit from bipartisan validation and public legitimacy for military action, but also loses unilateral speed and flexibility. Must invest political capital in securing congressional endorsement before acting. Extraction runs both directions: constrained by requirement for legislative sign-off, but also benefiting from distributed accountability and shared risk. Constraint enforces consultation; this is both burden and protection.
constraint_indexing:constraint_classification(sotu_1992_bush_operation_desert_storm_authorization_retrospective, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LEGISLATIVE BODY (ROPE) — Net beneficiary of the constraint. Gains restored power of the purse and authorization function that had drifted toward executive dominance in Cold War era. Congress experiences the constraint as coordination mechanism: it solves the collective action problem of war accountability by mandating deliberation and shared responsibility. Bipartisan endorsement becomes the legitimacy standard. Exit options include refusing authorization, but refusal creates its own political costs. Arbitrage: Congress can condition authorization on policy outcomes, force public debate, and claim shared credit for success.
constraint_indexing:constraint_classification(sotu_1992_bush_operation_desert_storm_authorization_retrospective, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: WARTIME PUBLIC AND MILITARY PERSONNEL (TANGLED ROPE) — Mixed experience. Benefits from legitimacy and public consensus that authorization provides — this reduces domestic opposition and creates moral justification for sacrifice. But also constrained by requirement that military objectives be measurable and vindicated by outcomes. If the conflict drags or objectives shift, the retrospective vindication mechanism becomes a trap: 'policies were vindicated' requires demonstrated success. Personnel and public bear both the coordination benefit (shared responsibility) and the extraction cost (public commitment to specific outcomes).
constraint_indexing:constraint_classification(sotu_1992_bush_operation_desert_storm_authorization_retrospective, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL REFORM ADVOCATES (SCAFFOLD) — See the Desert Storm precedent as temporary scaffolding for restored separation of powers. This perspective views the constraint as a sunset mechanism: the precedent establishes that congressional authorization is now structurally expected, and as this norm matures, the need for explicit validation of this precedent diminishes. The constraint has a built-in sunset: once the norm is internalized (next 10-20 years), explicit legislative authorization becomes default expectation rather than extraordinary requirement. Theater is moderate because the constraint serves real institutional function (redistribution of war powers) with temporary enforcement mechanism (the precedent itself).
constraint_indexing:constraint_classification(sotu_1992_bush_operation_desert_storm_authorization_retrospective, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: WAR POWERS RESOLUTION ENFORCEMENT SYSTEM (PITON) — The formal legal mechanism (War Powers Resolution of 1973) was already in place but largely unenforced and honored in the breach. Desert Storm established the precedent through Bush's strategic choice to seek authorization (not legally required, but politically astute). The enforcement system now persists through institutional inertia: presidents now must seek authorization, but the underlying mechanism (Congressional votes) is performative in many respects — authorization becomes a ritual that rarely fails once a president commits to military action. Theater ratio elevated: the authorization vote becomes symbolic of legitimacy rather than substantive gate. The constraint persists because it now shapes expectations, not because it has independent enforcement mechanism.
constraint_indexing:constraint_classification(sotu_1992_bush_operation_desert_storm_authorization_retrospective, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / CONSTITUTIONAL NATURAL LAW VIEW (MOUNTAIN) — From a civilizational horizon, the separation of powers is presented as a constitutional immutability: Congress has war powers, the President is commander-in-chief, and this structural tension is inherent to the Constitution's design. The Desert Storm precedent appears as revelation of an unchangeable principle rather than contingent institutional choice. However, this perspective risks naturalizing what is actually a precedent: earlier presidents had far greater unilateral war authority (Vietnam escalation without formal authorization), so the constraint is not immutable but rather a restored norm anchored to this specific moment of choice.
constraint_indexing:constraint_classification(sotu_1992_bush_operation_desert_storm_authorization_retrospective, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1992_bush_operation_desert_storm_authorization_retrospective_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1992_bush_operation_desert_storm_authorization_retrospective, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1992_bush_operation_desert_storm_authorization_retrospective, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1992_bush_operation_desert_storm_authorization_retrospective, TR),
    TR >= 0.70.

:- end_tests(sotu_1992_bush_operation_desert_storm_authorization_retrospective_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint imposes real costs on executive unilateral authority and speed of military response, but these are not maximal costs. Presidents retain substantial discretion in framing authorization requests and can interpret outcome vindication retrospectively. The extractiveness value reflects the genuine redistribution of war powers authority to Congress while acknowledging that executives retain substantial agency in working within the constraint. The measurement trajectory shows extractiveness increasing over the 5-year interval as the precedent becomes internalized and expected, with initial measurement (0.22) reflecting the immediate post-authorization context and final measurement (0.38) reflecting the mature precedent stage. Suppression (0.42): Moderate. There are real barriers to executive unilateral action — the need to secure congressional consensus, manage public debate, and commit to measurable objectives. But suppression is not absolute; presidents retain options including emergency executive authority, treaty-based action, and doctrinal reframing. The suppression value reflects that the constraint is enforceable through political cost (failed authorization votes create legitimacy crises) rather than through direct legal prohibition. Theater ratio (0.58): Moderate-high. Congressional authorization votes do serve symbolic legitimacy function, but they also retain substantive policy content — authorization can be conditioned on specific objectives, sunset clauses, or resource limits. The theater ratio reflects that the authorization process mixes performative affirmation of shared commitment with actual policy deliberation. The trajectory shows theater increasing over time as the authorization process becomes routine ritual rather than exceptional deliberation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural mechanism (congressional authorization requirement) produces radically different classifications depending on the observer's position. The executive branch experiences the constraint as Tangled Rope: they gain legitimacy and shared accountability but lose unilateral speed and flexibility. Congress experiences it as pure Rope: they solve the collective action problem of war accountability and restore their constitutional authority. Future military commanders experience it as Snare: they face binding precedent with no exit, delayed authorization windows, and operational constraints. The wartime public experiences Tangled Rope: they gain legitimacy from bipartisan consensus and moral justification, but also face the extraction cost of public commitment to specific outcomes that may shift. Constitutional reform advocates experience it as Scaffold: a temporary mechanism that sunset as the norm matures. The War Powers Resolution enforcement system experiences it as Piton: the mechanism persists through theatrical authorization rituals rather than genuine institutional change. The analytical observer at civilizational scale risks seeing Mountain: treating the separation of powers as immutable constitutional law rather than contingent institutional precedent. The perspectival gap reveals that 'policies were vindicated' means entirely different things from these different positions: success to the executive and Congress, constraint to future commanders, legitimacy to the public, temporary scaffolding to reformers, performative theater to the enforcement system.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from the agent's structural relationship to the authorization requirement. Congress as a primary beneficiary with institutional power and arbitrage options experiences low d (benefits from the constraint) — approximately 0.15. This produces low or negative effective extraction chi: Congress's power is amplified rather than constrained. The executive branch as a constrained agent with institutional power and reduced unilateral authority experiences moderate d (mixed extraction and benefit) — approximately 0.55. This produces moderate chi consistent with the Tangled Rope classification. Future military commanders as powerless agents with trapped exit experience maximum d (approximately 0.95), producing high chi consistent with the Snare classification. The wartime public with moderate power and constrained exit (cannot easily oppose authorized war) experiences high d (approximately 0.70), producing chi consistent with Tangled Rope. The directionality derivation reflects that beneficiary status (legislative oversight, bipartisan consensus, shared accountability) produces lower d, while victim status (executive speed loss, future commander constraint, public commitment to outcomes) produces higher d. The constraint redistributes directionality: it moves d upward for powerless agents (commanders, public) and downward for institutional agents (Congress, executive).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint exemplifies how apparent classification ambiguity dissolves when perspectival indexing is applied systematically. The question 'Is this constraint Rope or Snare or Tangled Rope?' appears unanswerable from a naive standpoint, but becomes determinate when asked 'From whose perspective, at what time horizon, with what exit options, at what spatial scope?' Congress sees Rope (pure coordination: shared responsibility for war). Future commanders see Snare (pure extraction: binding precedent with no exit). The executive sees Tangled Rope (mixed: gain legitimacy but lose speed). The public sees Tangled Rope (mixed: gain moral justification but face outcome commitment). The analytical observer risks seeing Mountain (natural constitutional law) but the structural data (executive authorization is historically contingent, not immutable) indicates this is a false summit. The mandatrophy resolves by accepting that all six types are valid perspectival readings of the same structural mechanism. There is no single 'true' classification — the presheaf of perspectives IS the complete analysis. Bush's 1992 framing ('policies were vindicated') is itself perspectival: from Congress and the wartime public, this is true (bipartisan consensus, successful military operation). From future commanders' perspective, this establishes a binding precedent that limits their authority. Both readings are structurally correct from their respective positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    outcome_vindication_credibility,
    'What constitutes ''policies were vindicated'' in retrospective evaluation of military operations with extended timelines and mission creep?',
    'Historical analysis of post-Desert Storm strategic outcomes: Did Kuwait remain independent? Were regional threats contained? Did humanitarian costs align with stated objectives? Comparison with conflicts where retrospective vindication claims proved contested (Vietnam, Iraq 2003).',
    'If vindication standards are clear and objective: constraint functions as true accountability mechanism (Rope from legislative perspective). If standards are moveable and retrospective: constraint devolves into performative authorization ritual (Piton), undermining the separation of powers benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(outcome_vindication_credibility, preference, 'Definition of retrospective outcome vindication for military operations').

omega_variable(
    precedent_binding_force,
    'How much binding force does the Desert Storm precedent carry for future presidents, and can it be circumvented through doctrinal reframing?',
    'Tracking of subsequent military authorizations and presidential claims of authority: Do presidents uniformly seek authorization post-1992? Are there doctrinal workarounds (humanitarian intervention, counterterrorism exemptions, treaty-based authority)? Compare with pre-Desert Storm baseline.',
    'If precedent is consistently binding: constraint is structural and durable (Tangled Rope for executive, Rope for legislature). If precedent is frequently circumvented: constraint is weakly enforced (Piton), relying on theatrical adherence rather than real power redistribution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(precedent_binding_force, empirical, 'Binding force of Desert Storm authorization precedent on subsequent military operations').

omega_variable(
    bipartisan_consensus_fragility,
    'Does the bipartisan consensus mechanism depend on shared strategic interest (Cold War threat perception) or can it survive in polarized contexts?',
    'Analysis of subsequent military authorization votes in periods of increasing partisan polarization: Does authorization still require broad consensus? Does partisan opposition constrain executive action or merely register dissent? Test case: compare Gulf War authorization (both chambers overwhelming support) with Iraq 2003 authorization (significant Democratic opposition but authorization passed).',
    'If consensus is essential: constraint may degrade in polarized environments (Piton). If constraint functions despite polarization: it is more robust than appearances suggest (true Tangled Rope). The bipartisan benefit may be illusory if consensus is already present for military action independent of authorization requirement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bipartisan_consensus_fragility, empirical, 'Stability of bipartisan consensus mechanism across political polarization').

omega_variable(
    speed_versus_legitimacy_tradeoff,
    'How much operational delay does the authorization requirement impose, and does this delay accumulate strategic disadvantage?',
    'Timeline analysis: Days between presidential decision to seek authorization and authorization vote, vs. operational readiness. Desert Storm case: authorization vote February 1991, operations began January 1991 (timing driven by UN deadline and coalition coordination, not authorization process). Compare with hypothetical scenario requiring pre-authorization.',
    'If authorization requirement measurably delays critical operations: extraction cost to executive is real and significant (Snare from commander perspective). If authorization can be sought and obtained within operationally acceptable timescale: extraction cost is primarily political rather than strategic, changing the Snare classification to Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speed_versus_legitimacy_tradeoff, empirical, 'Operational impact of congressional authorization timeline requirements').

omega_variable(
    false_summit_constitutional_naturalization,
    'Is the separation of powers constraint presented as immutable constitutional law actually a contingent institutional precedent?',
    'Historical analysis: Were earlier presidents (Eisenhower, Kennedy, Johnson) constitutionally bound to seek authorization? How much unilateral military authority did they exercise? Vietnam escalation occurred without formal authorization until 1964 Gulf of Tonkin resolution. Does the Constitution itself mandate authorization, or is this a norm restored post-Vietnam?',
    'If the constraint is truly constitutional (immutable): Mountain classification is correct. If constraint is a precedent (changeable): the analytical mountain perspective is a false summit naturalizing contingent choice. The engine''s false summit detector may flag this.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_constitutional_naturalization, conceptual, 'Constitutional vs. precedential nature of war powers authorization requirement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1992_bush_operation_desert_storm_authorization_retrospective, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(desert_storm_tr_t0, sotu_1992_bush_operation_desert_storm_authorization_retrospective, theater_ratio, 0, 0.38).
narrative_ontology:measurement(desert_storm_tr_t2, sotu_1992_bush_operation_desert_storm_authorization_retrospective, theater_ratio, 2, 0.48).
narrative_ontology:measurement(desert_storm_tr_t5, sotu_1992_bush_operation_desert_storm_authorization_retrospective, theater_ratio, 5, 0.58).

% Extraction over time
narrative_ontology:measurement(desert_storm_be_t0, sotu_1992_bush_operation_desert_storm_authorization_retrospective, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(desert_storm_be_t2, sotu_1992_bush_operation_desert_storm_authorization_retrospective, base_extractiveness, 2, 0.32).
narrative_ontology:measurement(desert_storm_be_t5, sotu_1992_bush_operation_desert_storm_authorization_retrospective, base_extractiveness, 5, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1992_bush_operation_desert_storm_authorization_retrospective, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1992_bush_operation_desert_storm_authorization_retrospective, war_powers_resolution_post_vietnam_norm).
narrative_ontology:affects_constraint(sotu_1992_bush_operation_desert_storm_authorization_retrospective, presidential_war_authority_boundary).

% DUAL FORMULATION NOTE:
% The Desert Storm authorization represents a specific instantiation of the broader war powers coordination problem (how to distribute authority between executive and legislative branches). The constraint is downstream of the constitutional separation of powers principle and upstream of specific military operation authorizations (Iraq 2003, Afghanistan 2001, Libya 2011). Each operation has its own authorization story; this constraint story models the institutional precedent mechanism itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1992_bush_operation_desert_storm_authorization_retrospective, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
