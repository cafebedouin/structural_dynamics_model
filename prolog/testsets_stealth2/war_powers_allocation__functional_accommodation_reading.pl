% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__functional_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__functional_accommodation_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: war_powers_allocation__functional_accommodation_reading
 *   human_readable: Functional Accommodation Reading of War Powers Allocation
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The functional accommodation reading instantiates the following
 *   constraint: the allocation of war-initiation authority between the
 *   political branches varies with operational context — imminent threats
 *   license unilateral presidential action, while prolonged campaigns require
 *   congressional authorization. This is one of three competing readings of
 *   the war_powers_allocation kernel (the constitutional text dividing the
 *   declare-war power from the commander-in-chief power); the
 *   congressional-primacy and inherent-executive readings are separate
 *   constraint stories linked through the network. The epsilon referent here
 *   is the standing functional-accommodation arrangement itself — the
 *   operating allocation as this reading understands it — assessed by this
 *   reading's own lights: a real coordination achievement (decision speed
 *   matched to decision legitimacy) carrying a systematically exploitable
 *   ambiguity zone whose rents accrue to the executive. The claim/metric gap
 *   is deliberate: claimed_type records this reading's structural
 *   self-understanding (a hybrid that coordinates and transfers through the
 *   same structure), while the metrics describe observed operation; the
 *   engine computes per-seat classifications from the structural data and the
 *   divergence is the datum. KEY AGENTS (by structural relationship): -
 *   sitting_presidents: Primary beneficiary and agenda setter
 *   (institutional/arbitrage) — controls operational characterization,
 *   captures the ambiguity zone, faces no exit pressure because any action
 *   can be reframed to fit - national_security_bureaucracy: Secondary
 *   beneficiary (institutional/mobile) — converts each unchallenged operation
 *   into precedent and budget - united_states_congress: Primary payer
 *   (institutional/constrained) — holds nominal authorization power it cannot
 *   enforce; bears erosion of its Article I role - deployed_service_members:
 *   Payer (powerless/trapped) — bear operational risk of undertested
 *   authorizations - foreign_populations_in_strike_zones: Payer
 *   (powerless/trapped) — absorb unilateral force decisions with no seat in
 *   the allocation - american_electorate: Excluded voice
 *   (organized/constrained) — the founding-era consenter, structurally absent
 *   from the initiation window - federal_courts: Observer with a maintenance
 *   function (institutional/analytical) — sustains the arrangement through
 *   justiciability abstention - constitutional_law_scholars: Analytical
 *   observers (analytical/analytical) — map the doctrine-practice gap without
 *   enforcement consequence
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, 0.66).
domain_priors:suppression_score(war_powers_allocation__functional_accommodation_reading, 0.67).
domain_priors:theater_ratio(war_powers_allocation__functional_accommodation_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, suppression_requirement, 0.67).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__functional_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__functional_accommodation_reading, "Functional Accommodation Reading of War Powers Allocation").
narrative_ontology:topic_domain(war_powers_allocation__functional_accommodation_reading, "constitutional/political").

domain_priors:requires_active_enforcement(war_powers_allocation__functional_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__functional_accommodation_reading, 'e96bfb4b-76f9-4d0e-998e-d79f073e24be').
narrative_ontology:cs_kernel_codification('e96bfb4b-76f9-4d0e-998e-d79f073e24be', fixed_text).
narrative_ontology:cs_authority_grounding('e96bfb4b-76f9-4d0e-998e-d79f073e24be', practice).
narrative_ontology:cs_interpretation_layer_present('e96bfb4b-76f9-4d0e-998e-d79f073e24be').
narrative_ontology:cs_reading_relation('e96bfb4b-76f9-4d0e-998e-d79f073e24be', war_powers_allocation__congressional_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('e96bfb4b-76f9-4d0e-998e-d79f073e24be', war_powers_allocation__inherent_executive_reading, coexists_with).
narrative_ontology:cs_axiom('e96bfb4b-76f9-4d0e-998e-d79f073e24be', foundational, allocation_tracks_operational_context).
narrative_ontology:cs_axiom_status(allocation_tracks_operational_context, holdable).
narrative_ontology:cs_axiom_grounding('e96bfb4b-76f9-4d0e-998e-d79f073e24be', allocation_tracks_operational_context, instrumental).
narrative_ontology:cs_axiom('e96bfb4b-76f9-4d0e-998e-d79f073e24be', foundational, no_fixed_branch_assignment_obtains).
narrative_ontology:cs_axiom_status(no_fixed_branch_assignment_obtains, holdable).
narrative_ontology:cs_axiom_grounding('e96bfb4b-76f9-4d0e-998e-d79f073e24be', no_fixed_branch_assignment_obtains, conventional).
narrative_ontology:cs_reference_frame('e96bfb4b-76f9-4d0e-998e-d79f073e24be', contextual_allocation_equilibrium).
narrative_ontology:cs_drift_state('e96bfb4b-76f9-4d0e-998e-d79f073e24be', contemporary_gray_zone_operations, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e96bfb4b-76f9-4d0e-998e-d79f073e24be', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__functional_accommodation_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, sitting_presidents).
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, national_security_bureaucracy).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, united_states_congress).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, deployed_service_members).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, foreign_populations_in_strike_zones).
narrative_ontology:constraint_vindicates(war_powers_allocation__functional_accommodation_reading, youngstown_tripartite_framework).
narrative_ontology:constraint_vindicates(war_powers_allocation__functional_accommodation_reading, case_by_case_accommodation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiates military operations and frames each action's operational character — as imminent self-defense, as limited reprisal, as continuation of an existing authorization. Files War Powers Resolution reports after strikes begin, describes consultations that follow rather than precede action, and treats the gap between the two branches of the allocation as discretionary space. Exit from the arrangement is unnecessary: any future operation can be characterized to fit whichever branch is convenient at the time.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, sitting_presidents, agenda_setter,
    institutional, biographical, arbitrage, global).

% Defense Department, intelligence agencies, and combatant commands plan and execute operations under whatever legal characterization the White House supplies. Each unchallenged operation adds precedent and budget justification; executive legal offices produce opinions that widen the range of characterizable actions. Career continuity depends on the arrangement persisting, and personnel rotate through administrations that all inherit and extend the same operational latitude.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, national_security_bureaucracy, beneficiary,
    institutional, generational, mobile, global).

% Holds the formal authorization power and passes resolutions asserting it, but cannot compel compliance: courts dismiss member suits for lack of standing, funding cutoffs carry unacceptable political risk, and authorization votes arrive after operations are underway or not at all. Periodic reassertion efforts — War Powers Resolution votes on Yemen and Iran, repeal proposals for old authorizations — fail against vetoes or procedural evasion. Members bear the institutional cost of a shrinking role while retaining nominal authority.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, united_states_congress, payer,
    institutional, generational, constrained, national).

% Carry out missions whose legal basis may rest on decades-old authorizations stretched to new purposes, or on imminence claims never tested in court. They cannot decline deployment, cannot obtain judicial review of their orders' authorization, and bear the physical risk of operations initiated without deliberative sign-off. Their exposure grows with each expansion of the gray zone.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, deployed_service_members, payer,
    powerless, biographical, trapped, global).

% Experience force initiated by a single branch's judgment, under standards of imminence and necessity defined by the striking government alone. They have no seat in the allocation, receive no notice before operations, and have no forum in which their governments' objections alter the legal characterization of strikes on their territory.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, foreign_populations_in_strike_zones, payer,
    powerless, immediate, trapped, global).

% Would insist on deliberative consent before entering sustained conflict — the founding-era expectation — but learns of operations after commencement, votes on war policy only indirectly and infrequently, and finds neither party offers an alternative to the arrangement. Its absence from the initiation window is structural: the speed rationale that justifies unilateral action is the same reason the public cannot be consulted in time.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, american_electorate, excluded,
    organized, generational, constrained, national).

% Decline to adjudicate interbranch war powers disputes under political-question and standing doctrines, treating the allocation as committed to the political branches. Each dismissal preserves the Court's institutional position while leaving the operative boundary wherever the last operation left it. Individual justices signal discomfort in concurrences — the leading framework itself emerged from a concurrence — without constructing enforcement machinery.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, federal_courts, observer,
    institutional, generational, analytical, national).

% Analyze the allocation from outside the operating arrangement, documenting the widening gap between doctrinal statement and practice. They supply the frameworks both branches invoke opportunistically, and their critiques circulate without enforcement consequence. Their position is fully external: they can describe the system accurately precisely because they depend on nothing from it.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, constitutional_law_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__functional_accommodation_reading, sitting_presidents).
narrative_ontology:fixing_cost_class(war_powers_allocation__functional_accommodation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the speed-versus-deliberation dilemma in force decisions: enables immediate response when delay would be catastrophic, while reserving democratic authorization for commitments that accumulate into sustained campaigns. The two branches of the allocation divide the decision space by operational context rather than by fixed branch assignment.
% TRANSFER_FUNCTION: Moves war-initiation authority from the legislature to the executive during the ambiguous opening phase of any conflict, and moves the resulting risks — casualties, escalation, precedent — onto service members, targeted populations, and the institutional balance itself, while returning a diminished authorization role to Congress once facts on the ground make refusal costly.
% ABSENT_VOICES: The American electorate — the founding-era consenter — is absent from the initiation window: the speed rationale that licenses unilateral action is the same reason public deliberation cannot occur in time. Populations in strike zones have no seat at all. Rank-and-file legislators outside leadership often learn of operations from press reports. All three would object to the current placement of the context boundary; their objections surface only retrospectively, in elections and hearings that cannot reach the completed act.
% DISAPPEARANCE_RATIONALE: If the functional allocation vanished overnight, every force decision would require a prior categorical settlement: either the congressional-primacy or the inherent-executive rule would have to win, ongoing operations would face immediate legal challenge, and the standing machinery of reports, consultations, and abstention doctrines would lose its object. Branch prerogatives, deployment pipelines, and enforcement expectations are all built on the arrangement's continuation.
% FOUNDING_PROBLEM: The 1787 text split war powers between a legislature given the declare-war power and an executive made commander-in-chief, leaving undecided who may initiate force when delay is dangerous and who decides when a defensive action has become a sustained war. The functional accommodation was built to answer both questions operationally without amending the text.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Supreme Court opinions themselves (the Youngstown concurrence's framework, the Hamdi plurality's concession that war takes no fixed form across contexts), bipartisan congressional war-powers activity (the War Powers Resolution passed over a presidential veto; limitation and repeal proposals recur from both parties), and a scholarly literature that uniformly treats the allocation as unsettled across ideological lines. No seat inside the arrangement claims the problem is solved — the executive asserts flexibility is needed, Congress asserts erosion, courts assert non-justiciability — and the persistence of the dispute is itself the corroboration.
narrative_ontology:disappearance_verdict(war_powers_allocation__functional_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__functional_accommodation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__functional_accommodation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(war_powers_allocation__functional_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__functional_accommodation_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__functional_accommodation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__functional_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness ends the interval at 0.66 and suppression at 0.67: the arrangement's persistence depends on active maintenance (judicial abstention, executive reframing, congressional acquiescence under procedural pressure), and its costs concentrate on seats with no enforcement recourse. Theater ratio reaches 0.46 — nearly half of the arrangement's observable activity (after-the-fact reports, ritual consultations, symbolic War Powers votes) is performative rather than functional, though genuine functions remain: rapid response occurs, and major commitments still sometimes draw real authorization votes. Accessibility collapse sits at 0.52: the categorical alternatives (fixed congressional primacy, fixed executive plenary power) survive as live doctrinal positions — they are the sibling readings — but cannot be operationalized inside the running arrangement, which absorbs every proposed bright line into discretionary characterization. Resistance at 0.60 reflects recurring congressional assertion, scholarly critique, and occasional judicial signals.
 *   
 *   The measurement series run on one shared time grid (t = years since 1952; points at 0, 8, 13, 21, 29, 38, 47, 49, 57, 67, 72) so every tracked metric is authored at every examined point. The trajectories are cyclical, not monotonic: crisis-driven executive accumulation, then congressional reassertion (the War Powers Resolution at t=21; the Gulf War authorization at t=38; the 2001 authorization at t=49), then relaxation, then renewed accumulation. Two full cycles are visible. The oscillation is itself partly an extraction mechanism operating as intermittent reinforcement: each episode teaches both branches that assertion is transient and acquiescence is cheap, so the post-cycle trough of extractiveness ratchets upward (0.44 to 0.47 to 0.51 to 0.57 across successive cycle floors).
 *   
 *   Suppression_requirement is authored because the story specifically traces enforcement-capacity change: justiciability doctrines hardened, classification expanded, and prior consultation degraded into after-the-fact notification — the machinery keeping resistance down matured over the interval. Suppression in base_properties is the raw structural scalar, unscaled by power or scope; only extractiveness is scaled downstream by directionality and spatial scope. Coordination type is declared as resource_allocation because the arrangement's primary function is allocating a shared governance resource — force-initiation authority — between two institutional claimants under varying demand conditions.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the presidency, the arrangement is constitutional statesmanship: flexibility no fixed rule could provide, exercised under ultimate accountability. From Congress, the same structure is a ratchet: each operation initiated without authorization lowers the cost of the next, and the nominal authorization power decays toward ceremony. Deployed service members and struck populations experience the arrangement as unaccountable risk — decisions shaping their exposure were made inside a legal argument they never saw, between institutions they cannot reach. Federal courts experience abstention as institutional prudence; scholars see the accumulating gap between statement and practice. The engine derives these divergent per-seat classifications from the declared directionalities and exit options; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Sitting presidents sit nearest the beneficiary pole: the arrangement subsidizes them with discretionary space, and their arbitrage-grade exit (reframing any operation to fit the convenient branch) insulates them from its costs. The national security bureaucracy shares that pole with slightly weaker capture — it collects precedent and budget but inherits the commitments. Congress is a target despite holding formal power: its constrained exit (member suits dismissed for standing, funding leverage politically unusable) places it near the full-target end — an unusual configuration of institutional power with structural weakness that the derivation captures through exit options rather than the power atom. Service members and struck populations are full targets: trapped exit, no enforcement forum. The electorate's exclusion keeps it outside the beneficiary side — it bears diffuse costs with constrained voice. Receipt concentrates rather than diffuses: the ambiguity zone's rents demonstrably accrue to the presidency (initiative, precedent, reframing discretion), so gain_flow names sitting_presidents rather than diffuse; the bureaucracy benefits incidentally but the gains land on the executive seat. No directionality overrides were needed: the beneficiary/victim declarations plus exit options reproduce the true relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification prevents two opposite mislabels. Reading the arrangement as pure coordination would erase the payers: Congress's decayed authority, exposed service members, struck populations — the coordination story is real, but the same structure carries asymmetric transfer to the executive. Reading it as pure extraction would erase the function: imminent-defense unilateralism solves a genuine collective-action problem no categorical rule handles as well, and even the arrangement's critics concede the speed rationale. The hybrid classification holds both truths and locates the pathology precisely: not in either branch of the allocation, but in the unpoliced boundary between them. Mandatrophy is not resolved — the founding problem (who decides when speed matters, and who decides when commitment becomes sustained) is live and recurs with each new threat technology. Fixing is prohibitive for the only seat positioned to fix it: Congress cannot obtain judicial enforcement of its own assertions, a constitutional amendment is unreachable, and the political cost of forcing the boundary exceeds any single session's benefit. The forward danger is drift: if the prolonged-campaign branch becomes fully dead letter, the arrangement converges with the inherent-executive reading and the coordination half atrophies — at that point the classification should slide toward snare, and the temporal series (rising extractiveness troughs, rising theater ratio) is the early-warning instrument for that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading of the war_powers_allocation kernel — what would change structurally if either sibling reading displaced it?',
    'Comparative classification of the sibling stories (congressional_primacy_reading, inherent_executive_reading): track the shift in beneficiary/victim sets, epsilon, and enforcement profile across the three files.',
    'Under congressional primacy the executive''s gray-zone rents vanish and the victim set contracts to wartime minorities; under inherent executive authority Congress''s payer seat dissolves and the authorization branch disappears entirely. This reading''s hybrid classification is stable only while both siblings remain live rivals.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: sibling readings would relocate or abolish the ambiguity zone this constraint runs on.').

omega_variable(
    imminence_boundary_indeterminacy,
    'Where does the imminence/prolongation boundary sit, and can an operation initiated as imminent avoid ever crossing it?',
    'Longitudinal tracking of operations initiated under imminence or self-defense framing: measure elapsed time and scope expansion before any authorization is sought.',
    'If imminence framing persists indefinitely — as with multi-decade authorizations stretched to new theaters — the prolonged-campaign branch is dead letter and the constraint converges in practice toward the inherent-executive reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(imminence_boundary_indeterminacy, empirical, 'Operative content of the context boundary between the two branches of the allocation.').

omega_variable(
    authorization_branch_enforceability,
    'Is the prolonged-campaign authorization requirement enforceable by Congress at all, given standing doctrine and political costs?',
    'Outcome audit of congressional enforcement attempts: member-suit dismissals (Crockett v. Reagan, Campbell v. Clinton, Conyers v. Bush), War Powers clock litigation, and funding-leverage episodes — dismissal rates and remedial success.',
    'If systematically unenforceable, the effective allocation is executive-dominant regardless of doctrinal statement, and effective extraction exceeds the authored base value.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authorization_branch_enforceability, empirical, 'Enforcement asymmetry between the two branches of the allocation.').

omega_variable(
    categorical_rule_suppression_necessity,
    'Is the suppression of categorical allocations a structural necessity (irreducible contextual variation in war) or a constructed doctrine serving incumbent discretion?',
    'Counterfactual institutional-design analysis: whether a bright-line trigger (automatic funding cutoff at fixed duration) performs acceptably across plausible threat scenarios, and comparison with constitutional systems holding firmer allocations.',
    'If a workable categorical rule exists, the ambiguity zone is rent-bearing construction and the constraint shifts toward snare; if genuinely unworkable, part of the measured transfer is the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_rule_suppression_necessity, conceptual, 'Whether context-variance is an irreducible feature of war or a constructed preserve of executive discretion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__functional_accommodation_reading, 0, 72).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_powers_allocation__functional_accommodation_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(war__tr_t0, observed).
narrative_ontology:measurement(war__tr_t8, war_powers_allocation__functional_accommodation_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement_basis(war__tr_t8, observed).
narrative_ontology:measurement(war__tr_t13, war_powers_allocation__functional_accommodation_reading, theater_ratio, 13, 0.28).
narrative_ontology:measurement_basis(war__tr_t13, observed).
narrative_ontology:measurement(war__tr_t21, war_powers_allocation__functional_accommodation_reading, theater_ratio, 21, 0.24).
narrative_ontology:measurement_basis(war__tr_t21, observed).
narrative_ontology:measurement(war__tr_t29, war_powers_allocation__functional_accommodation_reading, theater_ratio, 29, 0.34).
narrative_ontology:measurement_basis(war__tr_t29, observed).
narrative_ontology:measurement(war__tr_t38, war_powers_allocation__functional_accommodation_reading, theater_ratio, 38, 0.27).
narrative_ontology:measurement_basis(war__tr_t38, observed).
narrative_ontology:measurement(war__tr_t47, war_powers_allocation__functional_accommodation_reading, theater_ratio, 47, 0.41).
narrative_ontology:measurement_basis(war__tr_t47, observed).
narrative_ontology:measurement(war__tr_t49, war_powers_allocation__functional_accommodation_reading, theater_ratio, 49, 0.33).
narrative_ontology:measurement_basis(war__tr_t49, observed).
narrative_ontology:measurement(war__tr_t57, war_powers_allocation__functional_accommodation_reading, theater_ratio, 57, 0.43).
narrative_ontology:measurement_basis(war__tr_t57, observed).
narrative_ontology:measurement(war__tr_t67, war_powers_allocation__functional_accommodation_reading, theater_ratio, 67, 0.48).
narrative_ontology:measurement_basis(war__tr_t67, observed).
narrative_ontology:measurement(war__tr_t72, war_powers_allocation__functional_accommodation_reading, theater_ratio, 72, 0.46).
narrative_ontology:measurement_basis(war__tr_t72, observed).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(war__be_t0, observed).
narrative_ontology:measurement(war__be_t8, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement_basis(war__be_t8, observed).
narrative_ontology:measurement(war__be_t13, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 13, 0.55).
narrative_ontology:measurement_basis(war__be_t13, observed).
narrative_ontology:measurement(war__be_t21, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 21, 0.47).
narrative_ontology:measurement_basis(war__be_t21, observed).
narrative_ontology:measurement(war__be_t29, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 29, 0.56).
narrative_ontology:measurement_basis(war__be_t29, observed).
narrative_ontology:measurement(war__be_t38, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 38, 0.51).
narrative_ontology:measurement_basis(war__be_t38, observed).
narrative_ontology:measurement(war__be_t47, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 47, 0.6).
narrative_ontology:measurement_basis(war__be_t47, observed).
narrative_ontology:measurement(war__be_t49, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 49, 0.57).
narrative_ontology:measurement_basis(war__be_t49, observed).
narrative_ontology:measurement(war__be_t57, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 57, 0.65).
narrative_ontology:measurement_basis(war__be_t57, observed).
narrative_ontology:measurement(war__be_t67, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 67, 0.68).
narrative_ontology:measurement_basis(war__be_t67, observed).
narrative_ontology:measurement(war__be_t72, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 72, 0.66).
narrative_ontology:measurement_basis(war__be_t72, observed).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(war__su_t0, observed).
narrative_ontology:measurement(war__su_t8, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement_basis(war__su_t8, observed).
narrative_ontology:measurement(war__su_t13, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 13, 0.5).
narrative_ontology:measurement_basis(war__su_t13, observed).
narrative_ontology:measurement(war__su_t21, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 21, 0.47).
narrative_ontology:measurement_basis(war__su_t21, observed).
narrative_ontology:measurement(war__su_t29, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 29, 0.53).
narrative_ontology:measurement_basis(war__su_t29, observed).
narrative_ontology:measurement(war__su_t38, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 38, 0.49).
narrative_ontology:measurement_basis(war__su_t38, observed).
narrative_ontology:measurement(war__su_t47, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 47, 0.57).
narrative_ontology:measurement_basis(war__su_t47, observed).
narrative_ontology:measurement(war__su_t49, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 49, 0.61).
narrative_ontology:measurement_basis(war__su_t49, observed).
narrative_ontology:measurement(war__su_t57, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 57, 0.66).
narrative_ontology:measurement_basis(war__su_t57, observed).
narrative_ontology:measurement(war__su_t67, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 67, 0.69).
narrative_ontology:measurement_basis(war__su_t67, observed).
narrative_ontology:measurement(war__su_t72, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 72, 0.67).
narrative_ontology:measurement_basis(war__su_t72, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__functional_accommodation_reading, resource_allocation).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, congressional_primacy_reading).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, inherent_executive_reading).

% DUAL FORMULATION NOTE:
% Decomposition of the war_powers_allocation kernel into three epsilon-invariant constraint stories. The colloquial label 'war powers' conflates three structurally distinct claims: a fixed congressional-primacy allocation, a fixed executive-plenary allocation, and a context-sensitive allocation. Each has its own epsilon, beneficiary/victim structure, and failure modes; this file authors the third. The family pattern differs from the BGS upstream-evidence chain: here the functional reading is the operative arrangement whose practice drift feeds both siblings — each unauthorized operation strengthens the inherent-executive reading's factual premises and weakens the congressional-primacy reading's — so this story links to both as mutual structural influence rather than strict evidentiary upstream.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
