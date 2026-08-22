% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__congressional_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   powers kernel: the position that the Declare War Clause and the
 *   structural logic of Article I vest in Congress the exclusive authority to
 *   authorize military force beyond immediate self-defense, and that
 *   sustained executive military action without specific authorization is a
 *   constitutional violation rather than a permissible exercise of inherent
 *   power. From this reading's own lights, the standing arrangement under
 *   contest is the accreted practice since the Gulf of Tonkin era and the War
 *   Powers Resolution of open-ended AUMFs, unilateral troop extensions, and
 *   executive non-compliance with the Resolution's reporting and withdrawal
 *   clock — an arrangement this reading regards as substantially extractive
 *   of Congress's constitutional war power, not the rights-respecting
 *   alternative (strict prior authorization) it would install. Other readings
 *   of the same kernel — that context-dependent operational accommodation is
 *   proper (functional_accommodation_reading), or that the commander-in-chief
 *   clause grants standing inherent authority (inherent_executive_reading) —
 *   are separate constraint stories with their own ε and stakeholder
 *   structures, not alternative measurements of this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__congressional_primacy_reading, 0.68).
domain_priors:suppression_score(war_powers_allocation__congressional_primacy_reading, 0.72).
domain_priors:theater_ratio(war_powers_allocation__congressional_primacy_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__congressional_primacy_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__congressional_primacy_reading, "Congressional Primacy Reading of War Powers Allocation").
narrative_ontology:topic_domain(war_powers_allocation__congressional_primacy_reading, "constitutional_law/separation_of_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__congressional_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__congressional_primacy_reading, '6aadc387-ef4c-4a2a-b3d8-ae5aaa2e373f').
narrative_ontology:cs_kernel_codification('6aadc387-ef4c-4a2a-b3d8-ae5aaa2e373f', fixed_text).
narrative_ontology:cs_authority_grounding('6aadc387-ef4c-4a2a-b3d8-ae5aaa2e373f', lineage).
narrative_ontology:cs_interpretation_layer_present('6aadc387-ef4c-4a2a-b3d8-ae5aaa2e373f').
narrative_ontology:cs_reading_relation('6aadc387-ef4c-4a2a-b3d8-ae5aaa2e373f', war_powers_allocation__inherent_executive_reading, forecloses).
narrative_ontology:cs_reading_relation('6aadc387-ef4c-4a2a-b3d8-ae5aaa2e373f', war_powers_allocation__functional_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('6aadc387-ef4c-4a2a-b3d8-ae5aaa2e373f', foundational, declare_war_clause_is_exclusive_and_specific).
narrative_ontology:cs_axiom_status(declare_war_clause_is_exclusive_and_specific, holdable).
narrative_ontology:cs_axiom_grounding('6aadc387-ef4c-4a2a-b3d8-ae5aaa2e373f', declare_war_clause_is_exclusive_and_specific, conventional).
narrative_ontology:cs_axiom('6aadc387-ef4c-4a2a-b3d8-ae5aaa2e373f', secondary, commander_in_chief_power_limited_to_repelling_sudden_attack).
narrative_ontology:cs_axiom_status(commander_in_chief_power_limited_to_repelling_sudden_attack, holdable).
narrative_ontology:cs_axiom_grounding('6aadc387-ef4c-4a2a-b3d8-ae5aaa2e373f', commander_in_chief_power_limited_to_repelling_sudden_attack, conventional).
narrative_ontology:cs_reference_frame('6aadc387-ef4c-4a2a-b3d8-ae5aaa2e373f', founding_era_declare_war_clause_primacy).
narrative_ontology:cs_drift_state('6aadc387-ef4c-4a2a-b3d8-ae5aaa2e373f', post_9_11_indefinite_aumf_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6aadc387-ef4c-4a2a-b3d8-ae5aaa2e373f', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__congressional_primacy_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, executive_branch_national_security_apparatus).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, congress_war_power).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, public_democratic_accountability).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, deployed_service_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiates and sustains military operations under commander-in-chief authority, citing tempo, secrecy, and operational necessity to bypass or narrow congressional authorization requests. Controls classification, timing of disclosure, and the operational tempo that makes prior authorization politically costly to withhold once forces are committed. Accrues expanded unilateral latitude each time an extended deployment proceeds without a formal declaration or specific authorization.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, executive_branch_national_security_apparatus, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__congressional_primacy_reading, executive_branch_national_security_apparatus, agenda_setter).

% Holds the constitutional declare-war and appropriations powers but faces committed troops, sunk political costs, and the political risk of appearing to abandon deployed forces if it withholds funding or authorization after the fact. Passes broad or ambiguous authorizations (AUMFs) that are later stretched across decades and theaters far beyond their original scope, effectively ratifying unilateral action it did not specifically approve. Its practical leverage erodes with each cycle of after-the-fact acquiescence.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, congress_war_power, payer,
    institutional, generational, constrained, national).

% Bear the direct physical risk of military action initiated or extended without the specific congressional authorization this reading holds is constitutionally required. Have no voice in the authorization decision and cannot withdraw from deployment once committed; their exposure to sustained conflict tracks the executive's willingness to act without congressional sign-off.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, deployed_service_members, payer,
    powerless, immediate, trapped, global).

% The public's constitutional entitlement to have extended war-making decided through its elected representatives' collective deliberation, rather than by a single office. Diminished each time force is sustained on unilateral executive judgment; the electorate cannot exit this arrangement short of long-run electoral or constitutional realignment.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, public_democratic_accountability, payer,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(war_powers_allocation__congressional_primacy_reading, public_democratic_accountability).

% The 1973 statutory framework nominally requiring executive notification and a 60/90-day clock on unauthorized hostilities. In this reading it is the enforcement machinery that should compel congressional primacy but is treated by successive executives as advisory, and Congress has not forced a cutoff-of-funds confrontation to test it; its persistence without teeth is itself part of what this reading identifies as suppressed.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, war_powers_resolution_enforcement_mechanism, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__congressional_primacy_reading, war_powers_resolution_enforcement_mechanism, observer).

% Would be positioned to adjudicate the constitutional boundary but routinely decline war-powers disputes as political questions or dismiss for lack of standing, leaving the inter-branch conflict to resolve itself through political practice rather than binding legal ruling.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, federal_courts, excluded,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__congressional_primacy_reading, executive_branch_national_security_apparatus).
narrative_ontology:fixing_cost_class(war_powers_allocation__congressional_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Requiring collective congressional authorization for extended force is meant to ensure that decisions committing the nation to sustained war are made through deliberative, accountable, multi-member judgment rather than single-officeholder discretion — coordinating the distributed democratic legitimacy needed to sustain a prolonged war effort.
% TRANSFER_FUNCTION: Under this reading, the constitutional war-declaring authority is transferred away from Congress and toward the executive branch each time force is initiated or extended without specific authorization: decision-making power, political accountability, and practical control over escalation move from the legislature to the presidency.
% ABSENT_VOICES: Deployed service members bear the immediate physical cost of unauthorized or under-authorized extended operations but have no institutional voice in the authorization decision. Federal courts, positioned to adjudicate the boundary, are structurally absent because standing doctrine and the political-question doctrine keep the dispute out of the courts.
% DISAPPEARANCE_RATIONALE: If the congressional-primacy norm and its (weak) enforcement apparatus vanished entirely, executive practice would simply formalize what this reading already treats as creeping fact: standing presidential authority to sustain military campaigns indefinitely without a legislative vote, collapsing the distinction between an emergency response and a war. Congress's appropriations leverage would remain the only check, and its practical bite would diminish further absent even the rhetorical claim of a constitutional requirement.
% FOUNDING_PROBLEM: The Constitution's framers, having just escaped a monarchy that could commit the nation to war unilaterally, deliberately vested the power to declare war in the legislature — the branch structurally suited to slow, collective deliberation — while leaving the executive only the power to repel sudden attacks.
% FOUNDING_PROBLEM_CORROBORATION: Congressional Research Service reports and a substantial line of constitutional scholars (attesting from outside both the executive and legislative institutions that benefit or lose materially from the current arrangement) hold the founding problem is live and the constitutional design has been substantially circumvented rather than resolved. Executive branch legal counsel offices dispute this characterization, but that dispute comes from the benefiting party itself and is not treated here as independent corroboration.
narrative_ontology:disappearance_verdict(war_powers_allocation__congressional_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__congressional_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__congressional_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(war_powers_allocation__congressional_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__congressional_primacy_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.68) reflects this reading's assessment that the accretion of unilateral executive war-making — from Vietnam-era escalation through post-9/11 indefinite AUMF invocation — has substantially transferred the war-declaring function away from Congress in practice, even though the constitutional text was never formally amended. Suppression (0.72) is high because inherent-authority claims are treated, from this reading's seat, as actively suppressing the countervailing constitutional argument: executive branch legal opinions (OLC memos) function to foreclose congressional primacy claims before they reach a forum capable of adjudicating them, and the political-question doctrine insulates the practice from judicial testing. Theater ratio (0.40) captures that War Powers Resolution notifications and 60-day clock invocations are frequently issued as formal compliance gestures while operations continue substantively unaltered — real reporting infrastructure exists but functions increasingly as performance once the clock is not enforced by a funding cutoff.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive branch is the structural beneficiary: it exercises the powers being contested, times disclosure, and benefits from the political cost asymmetry that makes after-the-fact congressional ratification more likely than a fresh prior vote. Congress and the deployed service members and public who bear the practical costs of decisions made without their branch's constitutionally-required collective judgment sit at the target end — congress_war_power because its constitutional prerogative is what is being displaced, service members because they bear direct physical risk from force committed without the authorization this reading holds is required, and the public because its accountability channel (a recorded congressional vote) is bypassed.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification (rather than snare) is deliberate: this reading does not claim the coordination story is pure cover. A genuine coordination problem exists — collective deliberation before committing the nation to extended war is a real function that Article I's design was built to solve, and specific-authorization AUMFs (e.g., against al-Qaeda in 2001) do function as intended when Congress votes them promptly and narrowly. The extraction is asymmetric and layered on top of that genuine function: broad or stale authorizations get stretched by the executive far past their original scope, and the enforcement mechanism (War Powers Resolution) is honored procedurally while its substantive teeth (the funding cutoff) go untested. Classifying this as a pure snare would erase the real coordination function the framers built; classifying it as a clean rope would erase the asymmetric extraction this reading holds is actually occurring.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Which of the three war-powers-allocation readings (congressional_primacy, inherent_executive, functional_accommodation) correctly describes the constitutional allocation, and is the disagreement resolvable by textual/historical analysis or is it an irreducible interpretive commitment?',
    'A definitive Supreme Court ruling squarely addressing extended unilateral military action (which the Court has consistently avoided via justiciability doctrines) would resolve this at the level of binding law; absent that, resolution remains a matter of constitutional theory and historical practice, not empirical fact.',
    'If the inherent_executive_reading were judicially vindicated, the extraction and suppression values authored here would be recharacterized as normal operation of a constitutionally sound power, not extraction. If functional_accommodation prevailed, the victim/beneficiary structure would need to be reworked around an operational-context threshold rather than a bright-line authorization requirement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Which sibling reading of the war-powers kernel is constitutionally correct is not settled by this story and is routed here rather than hedged into epsilon.').

omega_variable(
    aumf_ratification_vs_bypass,
    'Does Congress''s repeated funding of operations under broad, stale AUMFs constitute genuine, if belated, congressional authorization (satisfying even this reading''s own requirement) or does it represent captured/coerced ratification that does not cure the original bypass?',
    'Analysis of the specificity and contemporaneity of each AUMF invocation relative to the operations it is cited to authorize, and whether Congress had a realistic non-funding alternative at each juncture (troops already deployed, political cost of defunding active operations).',
    'If funding constitutes genuine authorization, the effective extraction is lower than authored (Congress is not bypassed, merely exercising its power through appropriations rather than fresh declarations). If it is coerced ratification, the extraction is at least as high as authored, since Congress''s ''consent'' is manufactured by the sequencing of executive action.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aumf_ratification_vs_bypass, empirical, 'Whether after-the-fact funding cures or merely launders the authorization bypass this reading identifies.').

omega_variable(
    political_question_doctrine_as_suppression,
    'Is the federal judiciary''s consistent refusal to adjudicate war-powers disputes (via standing and political-question doctrines) itself a form of structural suppression of the congressional-primacy claim, or a legitimate application of separation-of-powers restraint that this reading should not count against the executive?',
    'Comparative analysis of the political-question doctrine''s application in war-powers cases versus its application in other separation-of-powers disputes the courts do resolve, to test whether war-powers cases are singled out for avoidance.',
    'If courts are applying a neutral, generally-applicable restraint doctrine, the suppression metric may overstate what is attributable to the executive specifically. If war-powers cases are anomalously avoided relative to comparable disputes, the suppression figure is well-grounded as a structural feature benefiting the executive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_question_doctrine_as_suppression, conceptual, 'Whether judicial non-justiciability should be scored as suppression benefiting the executive seat or as neutral institutional restraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__congressional_primacy_reading, 1973, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1973, war_powers_allocation__congressional_primacy_reading, theater_ratio, 1973, 0.25).
narrative_ontology:measurement(war__tr_t1983, war_powers_allocation__congressional_primacy_reading, theater_ratio, 1983, 0.3).
narrative_ontology:measurement(war__tr_t1993, war_powers_allocation__congressional_primacy_reading, theater_ratio, 1993, 0.33).
narrative_ontology:measurement(war__tr_t2003, war_powers_allocation__congressional_primacy_reading, theater_ratio, 2003, 0.36).
narrative_ontology:measurement(war__tr_t2013, war_powers_allocation__congressional_primacy_reading, theater_ratio, 2013, 0.38).
narrative_ontology:measurement(war__tr_t2024, war_powers_allocation__congressional_primacy_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(war__be_t1973, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 1973, 0.42).
narrative_ontology:measurement(war__be_t1983, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 1983, 0.48).
narrative_ontology:measurement(war__be_t1993, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 1993, 0.52).
narrative_ontology:measurement(war__be_t2003, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 2003, 0.6).
narrative_ontology:measurement(war__be_t2013, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 2013, 0.65).
narrative_ontology:measurement(war__be_t2024, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1973, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 1973, 0.45).
narrative_ontology:measurement(war__su_t1983, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 1983, 0.52).
narrative_ontology:measurement(war__su_t1993, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 1993, 0.58).
narrative_ontology:measurement(war__su_t2003, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 2003, 0.65).
narrative_ontology:measurement(war__su_t2013, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 2013, 0.7).
narrative_ontology:measurement(war__su_t2024, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__congressional_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_allocation__inherent_executive_reading).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_allocation__functional_accommodation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the war_powers_allocation kernel. congressional_primacy_reading treats the Declare War Clause as exclusive and specific, placing Congress and deployed service members in the victim set when the executive acts without authorization. inherent_executive_reading (sibling, separately authored) treats commander-in-chief power as an independent, self-sufficient source of authority and would author near-zero extraction for the same underlying operational history. functional_accommodation_reading (sibling, separately authored) ties the authorization requirement to operational context (imminent threat vs. prolonged campaign) and would author intermediate extraction, contingent on classifying each operation's duration and threat-imminence. The three share no common epsilon because each reading disagrees about what the constitutional baseline is, not merely about how extractive a given fact pattern is against an agreed baseline.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
