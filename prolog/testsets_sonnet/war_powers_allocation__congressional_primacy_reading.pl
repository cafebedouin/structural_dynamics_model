% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__congressional_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__congressional_primacy_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: war_powers_allocation__congressional_primacy_reading
 *   human_readable: Congressional Primacy Reading of War Powers Allocation
 *   domain: constitutional_law/separation_of_powers
 *
 * SUMMARY:
 *   This story instantiates the congressional-primacy reading of the war
 *   powers allocation kernel: the claim that the Constitution's text and
 *   structure make explicit congressional authorization a necessary predicate
 *   for military force beyond immediate self-defense, and that executive
 *   practice deviating from this is not a legitimate alternative reading but
 *   an extraction of a power the Constitution assigns elsewhere. Since the
 *   Korean War, and accelerating through Vietnam, the post-9/11 AUMFs, Libya
 *   (2011), and Syria operations, presidents have increasingly initiated and
 *   sustained force without contemporaneous authorization, treating the War
 *   Powers Resolution's reporting requirements as the operative standard
 *   rather than prior authorization. Under this reading, that pattern
 *   constitutes a sustained transfer of constitutional war power from
 *   Congress to the executive, dressed in the language of operational
 *   necessity. The sibling readings — inherent_executive and
 *   functional_accommodation — are NOT part of this constraint; they are
 *   separate constraints with their own ε values, linked here only by network
 *   reference.
 *
 * KEY AGENTS:
 *   - executive_branch_war_planners: Primary agenda_setter and beneficiary (institutional/arbitrage) — initiates force and controls sequencing
 *   - congress_war_declaration_power: Primary payer/victim (institutional/constrained) — holds formal power bypassed in practice
 *   - deployed_service_members: Powerless payer (trapped) — bears physical risk without voice in authorization question
 *   - affected_foreign_civilian_populations: Powerless payer (trapped) — bears costs with no standing at all
 *   - federal_judiciary: Analytical observer — declines to adjudicate via political question doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__congressional_primacy_reading, 0.62).
domain_priors:suppression_score(war_powers_allocation__congressional_primacy_reading, 0.71).
domain_priors:theater_ratio(war_powers_allocation__congressional_primacy_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__congressional_primacy_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__congressional_primacy_reading, "Congressional Primacy Reading of War Powers Allocation").
narrative_ontology:topic_domain(war_powers_allocation__congressional_primacy_reading, "constitutional_law/separation_of_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__congressional_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__congressional_primacy_reading, '55a3697f-b7ed-4c3c-b8cf-76a2891d8444').
narrative_ontology:cs_kernel_codification('55a3697f-b7ed-4c3c-b8cf-76a2891d8444', fixed_text).
narrative_ontology:cs_authority_grounding('55a3697f-b7ed-4c3c-b8cf-76a2891d8444', lineage).
narrative_ontology:cs_interpretation_layer_present('55a3697f-b7ed-4c3c-b8cf-76a2891d8444').
narrative_ontology:cs_reading_relation('55a3697f-b7ed-4c3c-b8cf-76a2891d8444', war_powers_allocation__inherent_executive_reading, forecloses).
narrative_ontology:cs_reading_relation('55a3697f-b7ed-4c3c-b8cf-76a2891d8444', war_powers_allocation__functional_accommodation_reading, influences).
narrative_ontology:cs_axiom('55a3697f-b7ed-4c3c-b8cf-76a2891d8444', foundational, declare_war_clause_is_exclusive_prior_grant).
narrative_ontology:cs_axiom_status(declare_war_clause_is_exclusive_prior_grant, holdable).
narrative_ontology:cs_axiom_grounding('55a3697f-b7ed-4c3c-b8cf-76a2891d8444', declare_war_clause_is_exclusive_prior_grant, conventional).
narrative_ontology:cs_axiom('55a3697f-b7ed-4c3c-b8cf-76a2891d8444', secondary, sustained_force_without_authorization_is_ultra_vires).
narrative_ontology:cs_axiom_status(sustained_force_without_authorization_is_ultra_vires, holdable).
narrative_ontology:cs_axiom_grounding('55a3697f-b7ed-4c3c-b8cf-76a2891d8444', sustained_force_without_authorization_is_ultra_vires, deontological).
narrative_ontology:cs_reference_frame('55a3697f-b7ed-4c3c-b8cf-76a2891d8444', declare_war_clause_exclusive_grant).
narrative_ontology:cs_drift_state('55a3697f-b7ed-4c3c-b8cf-76a2891d8444', post_9_11_authorization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('55a3697f-b7ed-4c3c-b8cf-76a2891d8444', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__congressional_primacy_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, executive_branch_war_planners).
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, standing_military_command_structure).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, congress_war_declaration_power).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, deployed_service_members).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, affected_foreign_civilian_populations).
narrative_ontology:constraint_vindicates(war_powers_allocation__congressional_primacy_reading, constitutional_separation_of_powers_doctrine).
narrative_ontology:constraint_vindicates(war_powers_allocation__congressional_primacy_reading, declare_war_clause_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiates and sustains military operations under claimed Article II authority, treating the War Powers Resolution's 60/90-day clock and prior-authorization expectation as advisory rather than binding. Controls operational tempo, classification of engagements as 'hostilities' or not, and the timing of any request for authorization, effectively setting the terms under which Congress is asked to ratify decisions already made.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, executive_branch_war_planners, agenda_setter,
    institutional, immediate, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__congressional_primacy_reading, executive_branch_war_planners, beneficiary).

% Holds the constitutionally textual Declare War and Letters of Marque powers under Article I, but faces a structural asymmetry: it can only react to force already deployed, cannot easily claw back appropriated funds mid-campaign without appearing to abandon troops, and lacks a reliable enforcement mechanism against a president who acts first and briefs later. Its formal power is real on paper but is bypassed in practice through sequencing.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, congress_war_declaration_power, payer,
    institutional, generational, constrained, national).

% Bear the direct physical risk of engagements initiated without the constitutional predicate of congressional authorization. Have no institutional voice in whether the deployment underlying their orders satisfies Article I requirements; their obligation to follow lawful orders is decoupled from the question of whether the initiating authority was itself lawful.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, deployed_service_members, payer,
    powerless, immediate, trapped, global).

% Bear the physical and social costs of military operations undertaken without the deliberative check that congressional authorization is meant to provide. Have no standing in the US constitutional process at all; their exposure to risk increases in proportion to how easily the executive can act without the friction of legislative debate.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, affected_foreign_civilian_populations, payer,
    powerless, immediate, trapped, regional).

% Benefits from operational flexibility and reduced political friction when force can be committed on executive timelines rather than legislative ones. Budget requests, force posture, and doctrine are built around expectation of executive-initiated action, reinforcing the pattern this reading identifies as extraction from congressional prerogative.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, standing_military_command_structure, beneficiary,
    institutional, generational, arbitrage, global).

% Has repeatedly declined to resolve war powers disputes between the political branches on political question grounds, leaving the constitutional allocation contested in practice rather than settled by adjudication. Its abstention is itself a structural feature that lets the extraction persist unchecked by a third branch.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, federal_judiciary, observer,
    institutional, civilizational, analytical, national).

% Argue at length, in law reviews and congressional testimony, that the Declare War Clause and the structure of Article I Section 8 make congressional authorization a precondition rather than a formality for sustained force. Their analysis informs public debate but has no binding mechanism; they are heard but not empowered.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, constitutional_law_scholars, excluded,
    moderate, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__congressional_primacy_reading, executive_branch_war_planners).
narrative_ontology:fixing_cost_class(war_powers_allocation__congressional_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its ideal form, requiring explicit congressional authorization for sustained force coordinates deliberative legitimacy (many elected voices debate before committing the nation to prolonged conflict) with military effectiveness (once authorized, the executive commands with a clear, publicly-debated mandate), preventing unilateral commitments that later fracture public and political support.
% TRANSFER_FUNCTION: Under this reading, congressional war power that should govern the initiation of sustained force is instead exercised post-hoc or not at all, transferring the practical authority to decide, fund by inertia, and prosecute prolonged military campaigns from the legislature to the executive and the standing command structure it directs.
% ABSENT_VOICES: Deployed service members and foreign civilian populations bear the direct costs of the bypassed authorization but have no seat in the process; constitutional law scholars articulate the objection but hold no enforcement power; the judiciary that could adjudicate the dispute has largely declined to do so via the political question doctrine.
% DISAPPEARANCE_RATIONALE: If the congressional-primacy norm were dropped entirely (rather than merely bypassed as it now often is), executive branch planners and the standing command structure would experience little practical change since the norm is already frequently circumvented in practice. Congress would lose its remaining rhetorical and appropriations leverage. Whether the world 'rearranges' or stays the same is itself the contested question this reading exists to answer differently than its siblings.
% FOUNDING_PROBLEM: The founders divided the power to declare/fund war (Congress) from the power to command forces in the field (President) specifically to prevent a single actor from unilaterally committing the nation to war, based on historical experience with monarchical war-making that bypassed popular and legislative consent.
% FOUNDING_PROBLEM_CORROBORATION: Congressional Research Service reports, War Powers Resolution legislative history, and testimony from constitutional scholars outside the executive branch attest the founding problem remains live and unresolved; executive branch legal counsel offices across administrations of both parties attest the problem has been functionally superseded by evolving practice and technological/operational necessity — corroboration is genuinely split across institutional lines rather than unanimous.
narrative_ontology:disappearance_verdict(war_powers_allocation__congressional_primacy_reading, contested).
narrative_ontology:founding_problem_status(war_powers_allocation__congressional_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__congressional_primacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(war_powers_allocation__congressional_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__congressional_primacy_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__congressional_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__congressional_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__congressional_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.62 and rising over the interval, reflecting a pattern (not a single event) of executive-initiated sustained operations proceeding without the prior authorization this reading holds constitutionally required. Suppression is authored higher (0.71) because the mechanism that would check this pattern — congressional appropriations leverage, judicial review, the War Powers Resolution's clock — has been progressively defanged in practice; suppression here specifically captures the active foreclosure of the inherent-authority counter-claim within THIS reading's own framework, not scaled by power or scope. Theater ratio (0.40) captures the significant gap between formal compliance rituals (WPR notifications, informal 'consultations') and substantive prior authorization; the rising trajectory models notification-as-theater increasingly substituting for genuine authorization-seeking.
 *
 * PERSPECTIVAL GAP:
 *   From the executive branch planner's seat, the pattern is functional necessity — modern threats move faster than legislative deliberation allows, and the practice has become an accepted operational reality. From Congress's seat, and from the seat of this reading specifically, the same pattern is a structural bypass of an explicit constitutional textual assignment (Article I Section 8: 'To declare War'). The engine should compute these as structurally different experiences of the identical historical pattern — that divergence is exactly what the tangled_rope classification is measuring, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Executive branch war planners and the standing command structure sit near the beneficiary end: they gain operational flexibility, reduced political friction, and effective control over the initiation question. Congress sits near the target end despite its formal institutional power, because its exit options are constrained (clawing back mid-campaign funding is politically costly) and its enforcement mechanism is weak. Deployed service members and foreign civilians sit at the extreme target end: trapped exit options, immediate time horizon, and zero voice in the authorization question whose absence directly produces their exposure to risk.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing unilateral war-initiation by a single actor — remains structurally live in the sense that the underlying danger (concentrated war-making power) has not disappeared; if anything, the speed and lethality of modern force projection make the danger more acute, not less. This is why founding_problem_status is authored as contested rather than dead: this reading holds the problem live, while executive branch corroborators effectively treat it as superseded by operational necessity. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (national security decision-making does need SOME mechanism, and existing statutory frameworks are not wholly theater) while still naming the asymmetric extraction this reading identifies — a pure snare framing would erase the legitimate coordination problem the constitutional design was solving.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    congressional_primacy_vs_inherent_executive_foreclosure,
    'Does the congressional-primacy reading logically foreclose the inherent-executive reading within a single constitutional framework, or can both be held as live competing interpretations by different political actors simultaneously?',
    'This is fundamentally a question about constitutional interpretive method — whether the Declare War Clause is read as an exclusive grant (foreclosing inherent unilateral authority beyond genuine self-defense) or as one power among several concurrent war-related powers. No empirical fact resolves this; it depends on priors about original meaning, structural inference, and post-ratification practice''s interpretive weight.',
    'If congressional_primacy genuinely forecloses inherent_executive, then executive unilateral action beyond self-defense is definitionally unconstitutional extraction, sharpening this constraint toward snare. If the readings coexist as live alternatives across the political system, the tangled_rope framing better captures an unresolved contest rather than settled extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congressional_primacy_vs_inherent_executive_foreclosure, conceptual, 'Whether this reading''s core premise is logically incompatible with the inherent-executive reading''s core premise.').

omega_variable(
    practice_drift_evidentiary_weight,
    'Does a long, bipartisan pattern of executive-initiated force (Korea through present) constitute legitimate constitutional gloss establishing a functional amendment to the original allocation, or does it constitute an accumulated pattern of extraction that never achieved constitutional legitimacy regardless of its duration?',
    'Comparative analysis of how the judiciary treats ''historical gloss'' arguments in other separation-of-powers contexts (e.g., appointments, removal power) versus its consistent refusal to resolve war powers disputes directly; also resolvable in part by whether Congress ever affirmatively ratifies the pattern through formal legislative action versus mere acquiescence by inaction.',
    'If practice constitutes legitimate gloss, the congressional_primacy reading''s extraction framing weakens over time as a matter of evolving constitutional meaning. If practice is extraction regardless of duration, the accumulating pattern strengthens the case for treating this as approaching snare rather than tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(practice_drift_evidentiary_weight, conceptual, 'Whether sustained executive practice legitimizes the deviation or merely documents its persistence.').

omega_variable(
    judicial_abstention_as_structural_enabler,
    'Is the federal judiciary''s consistent refusal to adjudicate war powers disputes (via the political question doctrine) a neutral institutional restraint, or does it function as a structural enabler that allows the extraction this reading identifies to proceed unchecked by any independent arbiter?',
    'Track whether any future war powers case overcomes the political question bar, and analyze whether the doctrine is applied consistently across analogous separation-of-powers questions or specifically hardened in this domain.',
    'If judicial abstention is a neutral separation-of-powers principle, the absence of judicial check is not itself part of the extraction mechanism. If abstention specifically shields this pattern from review, it should be counted as part of the suppression apparatus this reading measures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_abstention_as_structural_enabler, empirical, 'Whether judicial non-involvement is a neutral restraint or an active component of the suppression this reading identifies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__congressional_primacy_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_powers_allocation__congressional_primacy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(war__tr_t10, war_powers_allocation__congressional_primacy_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(war__tr_t20, war_powers_allocation__congressional_primacy_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(war__tr_t30, war_powers_allocation__congressional_primacy_reading, theater_ratio, 30, 0.33).
narrative_ontology:measurement(war__tr_t40, war_powers_allocation__congressional_primacy_reading, theater_ratio, 40, 0.36).
narrative_ontology:measurement(war__tr_t50, war_powers_allocation__congressional_primacy_reading, theater_ratio, 50, 0.38).
narrative_ontology:measurement(war__tr_t60, war_powers_allocation__congressional_primacy_reading, theater_ratio, 60, 0.4).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(war__be_t10, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement(war__be_t20, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(war__be_t30, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(war__be_t40, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(war__be_t50, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 50, 0.6).
narrative_ontology:measurement(war__be_t60, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 60, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(war__su_t10, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(war__su_t20, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(war__su_t30, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 30, 0.63).
narrative_ontology:measurement(war__su_t40, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 40, 0.66).
narrative_ontology:measurement(war__su_t50, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 50, 0.69).
narrative_ontology:measurement(war__su_t60, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 60, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__congressional_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, inherent_executive_reading).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, functional_accommodation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the war_powers_allocation kernel. congressional_primacy_reading (this file) treats explicit prior authorization as constitutionally necessary and classifies unilateral executive action as extraction. inherent_executive_reading treats commander-in-chief power as an independent, sufficient constitutional grant and would classify the same executive actions as legitimate exercise of enumerated authority rather than extraction — under that reading Congress is not a victim but a co-equal branch declining to exercise a power it retains. functional_accommodation_reading occupies a middle position, authorizing unilateral action only for imminent threats and requiring authorization for prolonged campaigns — its ε would be lower than this story's for short engagements and would converge toward this story's ε for sustained campaigns. Each reading has its own beneficiary/victim structure and its own ε; they are not the same constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
