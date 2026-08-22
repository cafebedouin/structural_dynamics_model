% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__minoritarian_veto_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supermajority_threshold__minoritarian_veto_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: supermajority_threshold__minoritarian_veto_reading
 *   human_readable: Supermajority Amendment Threshold as Minoritarian Veto Lock
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The supermajority amendment threshold in federal constitutions is
 *   presented (in the consensus_safeguard reading) as a protection against
 *   impulsive majoritarian revision of fundamental law. This reading—the
 *   minoritarian_veto_reading—interprets the same threshold as a lock-in
 *   mechanism that permanently insulates entrenched interests from
 *   majoritarian pressure for constitutional reform. Historical minorities
 *   (regionally dominant, institutionally privileged) use the supermajority
 *   requirement not to advance their own reform agenda but to block reforms
 *   majorities seek. The threshold transforms structural advantage into
 *   permanent veto: what once required constant political defense now
 *   requires majorities to achieve the near-impossible. The threshold's
 *   persistence is itself extractive—it denies democratic expression to
 *   reform majorities while protecting the beneficiaries of the status quo.
 *   This reading does not claim the threshold was designed for minoritarian
 *   lock-in (that is not necessarily the case); it claims the threshold's
 *   actual operation, under contemporary demographic and political
 *   conditions, functions as such a lock-in.
 *
 * KEY AGENTS:
 *   - Status quo beneficiaries (hereditary wealth holders, regionally dominant interests): benefit from barrier to redistributive reform; entrenched in power via supermajority requirement
 *   - Entrenched elites (institutional actors, constitutional offices): benefit from frozen authority against majoritarian reconfiguration; control interpretation of unamendable provisions
 *   - Blocking minorities (regional, ideological, sectional): use supermajority veto to prevent unwanted reforms; their cost is political mobilization to maintain blocking coalition
 *   - Contemporary majoritarian coalitions (labor, voting rights, equality movements): pay the cost of blocked constitutional change; face near-impossible threshold
 *   - Reform coalitions facing threshold (specific amendment seekers): extract-bearing group facing extraordinary coordination requirements
 *   - Regional minorities (indigenous nations, territorial populations): trapped exit; permanent structural subordination protected by amendment barrier
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, 0.82).
domain_priors:suppression_score(supermajority_threshold__minoritarian_veto_reading, 0.78).
domain_priors:theater_ratio(supermajority_threshold__minoritarian_veto_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__minoritarian_veto_reading, snare).
narrative_ontology:human_readable(supermajority_threshold__minoritarian_veto_reading, "Supermajority Amendment Threshold as Minoritarian Veto Lock").
narrative_ontology:topic_domain(supermajority_threshold__minoritarian_veto_reading, "constitutional/political").

domain_priors:requires_active_enforcement(supermajority_threshold__minoritarian_veto_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__minoritarian_veto_reading, '277191b8-f6a6-4353-bcb5-d5e4a5180752').
narrative_ontology:cs_kernel_codification('277191b8-f6a6-4353-bcb5-d5e4a5180752', fixed_text).
narrative_ontology:cs_authority_grounding('277191b8-f6a6-4353-bcb5-d5e4a5180752', extraction).
narrative_ontology:cs_interpretation_layer_present('277191b8-f6a6-4353-bcb5-d5e4a5180752').
narrative_ontology:cs_reading_relation('277191b8-f6a6-4353-bcb5-d5e4a5180752', supermajority_threshold__consensus_safeguard_reading, coexists_with).
narrative_ontology:cs_reading_relation('277191b8-f6a6-4353-bcb5-d5e4a5180752', supermajority_threshold__adaptive_gradient_reading, influences).
narrative_ontology:cs_axiom('277191b8-f6a6-4353-bcb5-d5e4a5180752', foundational, supermajority_threshold_enables_minoritarian_lock_in).
narrative_ontology:cs_axiom_status(supermajority_threshold_enables_minoritarian_lock_in, holdable).
narrative_ontology:cs_axiom_grounding('277191b8-f6a6-4353-bcb5-d5e4a5180752', supermajority_threshold_enables_minoritarian_lock_in, empirically_contingent).
narrative_ontology:cs_axiom('277191b8-f6a6-4353-bcb5-d5e4a5180752', foundational, lock_in_of_status_quo_delegitimates_amendment_barrier).
narrative_ontology:cs_axiom_status(lock_in_of_status_quo_delegitimates_amendment_barrier, holdable).
narrative_ontology:cs_axiom_grounding('277191b8-f6a6-4353-bcb5-d5e4a5180752', lock_in_of_status_quo_delegitimates_amendment_barrier, deontological).
narrative_ontology:cs_reference_frame('277191b8-f6a6-4353-bcb5-d5e4a5180752', democratic_majoritarian_amendment_expectation).
narrative_ontology:cs_drift_state('277191b8-f6a6-4353-bcb5-d5e4a5180752', contemporary_accumulated_reform_failures, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('277191b8-f6a6-4353-bcb5-d5e4a5180752', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, status_quo_beneficiaries).
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, entrenched_elites).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, contemporary_majoritarian_coalitions).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, reform_coalitions_facing_threshold).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, blocking_minority).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, regional_minorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups whose interests are locked in by the current constitutional arrangement—hereditary wealth holders, regionally dominant interests, institutional insiders. The supermajority threshold insulates them from majoritarian pressure to redistribute resources, reform institutions, or alter foundational rules. Their exit option is to lobby for threshold-lowering when threatened; their structural position is that the threshold does the work they would otherwise need to perform through constant political defense.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, status_quo_beneficiaries, beneficiary,
    powerful, generational, arbitrage, national).

% Institutional actors—legislatures, courts, executive offices—whose formal authority and interpretive power are frozen by the difficulty of constitutional amendment. They benefit from the threshold's stabilization of their own authority against majoritarian reconfiguration and from the rent-seeking space it creates around unamendable provisions. Institutional change faces the same barrier ordinary citizens face; elites control the interpretation of what the frozen text means.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, entrenched_elites, beneficiary,
    institutional, generational, arbitrage, national).

% A regional, ideological, or sectional minority whose interests are opposed by the majority but whose structural position in the federal/legislative system grants them supermajority blocking power—through Senate representation, state legislature control, or constitutional convention rules. They use the threshold not to advance their own reform agenda but to prevent reforms the majority seeks. Their 'cost' is the constant political mobilization required to maintain the blocking coalition; their 'benefit' is veto power over unwanted change.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, blocking_minority, payer,
    powerful, biographical, constrained, national).

% Contemporary majorities seeking constitutional reform—labor rights expansions, suffrage extensions, institutional restructuring, rights protection—who cannot achieve their policy goals through ordinary legislative process because they face a supermajority amendment barrier they cannot clear. Their exit is localized policy adoption (state-level, subnational), non-constitutional workaround (legislation, interpretation, civil disobedience), or long-term coalition-building until demography shifts the supermajority calculus. They bear the cost of democratic will thwarted by structural design.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, contemporary_majoritarian_coalitions, payer,
    organized, biographical, constrained, national).

% Groups seeking specific amendments—voting rights, labor protections, gender equality, land redistribution, structural decolonization. They face an amendment process designed to be nearly impossible: supermajority of legislatures or conventions, state ratification requirements, temporal restrictions. The constraint extracts from them by requiring extraordinary political coordination to achieve what simple democracy would deliver. Their alternative is to accept non-constitutional approximation or to wait for demographic/political shifts that might eventually move the supermajority dial.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, reform_coalitions_facing_threshold, payer,
    moderate, biographical, constrained, national).

% Geographically concentrated minorities (e.g., indigenous nations, regional populations under colonial or minority-rule institutional structures) whose exit from the supermajority threshold system is prevented by territorial entrapment. They cannot leave the federal system; they cannot achieve constitutional reform through it; they bear permanent structural subordination protected by the amendment barrier. Their situation combines victim-of-extraction with blocked-exit.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, regional_minorities, payer,
    organized, generational, trapped, national).

% The historical consensus of the founding generation about the supermajority threshold's purpose (most reconstructions place it as a safeguard against tyranny, not as a lock-in mechanism for status quo privilege). This consensus is invoked by both this reading and the consensus_safeguard_reading; the minoritarian_veto_reading interprets the threshold's ACTUAL operation as divergent from the framers' narrative intent.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, framers_historical_consensus, excluded,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(supermajority_threshold__minoritarian_veto_reading, framers_historical_consensus).

% Legislators, state convention delegates, and constitutional scholars who participate in amendment processes and can observe the threshold's actual blocking effects. They witness whether the threshold is functioning as consensus-protection (the framers' narrative) or as minoritarian veto (this reading's claim). Their observations feed the empirical base for the omega around actual consensus-formation vs. blocking.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, constitutional_amendment_participants, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(supermajority_threshold__minoritarian_veto_reading, status_quo_beneficiaries).
narrative_ontology:fixing_cost_class(supermajority_threshold__minoritarian_veto_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The supermajority requirement coordinates on a single stable constitutional text across time: without a high barrier to amendment, frequent majoritarian changes would destabilize institutional expectations and long-term constitutional meaning. The stated coordination problem is 'protect against impulsive majoritarian revision.'
% TRANSFER_FUNCTION: Transfers the ability to block constitutional change from democratic majorities (one-person-one-vote) to structural supermajorities (states, regions, institutional actors whose overrepresentation gives them veto). The transfer is from 'majoritarian will' to 'supermajority check.' In practice, it moves reform power away from contemporary coalitions toward entrenched interests whose current position would be threatened by amendment.
% ABSENT_VOICES: Groups systematically locked out of the ratification process—disenfranchised historical populations, colonial subjects, those deemed insufficiently numerous or regionally dispersed to achieve supermajority status. They would testify that the threshold's effect is to permanently entrench their exclusion; they are prevented from doing so by the same threshold.
% DISAPPEARANCE_RATIONALE: If the supermajority threshold disappeared (replaced by simple majority or lower barrier), constitutional amendments addressing rights expansion, institutional restructuring, and resource redistribution would become achievable where currently blocked. Entrenched minorities would lose veto power; status quo beneficiaries would lose structural insulation. Multiple reform efforts currently stalled would advance. The constitutional landscape would reorganize around majoritarian preference rather than minority veto.
% FOUNDING_PROBLEM: The founding problem, as reconstructed by the consensus_safeguard_reading: tyranny of transient majorities making impulsive constitutional changes that destabilize governance and oppress minority rights. The framers established a high amendment bar to force deliberative consensus-formation.
% FOUNDING_PROBLEM_CORROBORATION: The minoritarian_veto_reading asserts the founding problem (protection against impulsive majoritarian tyranny) has been resolved by other institutional features—executive veto, judicial review, federalism, supermajority voting in legislatures—such that the amendment supermajority now operates as a redundant lock-in protecting entrenched interests rather than preventing tyranny. Corroboration from outside the status-quo-beneficiary set: contemporary reform movements (labor, voting rights, indigenous rights organizations), constitutional scholars analyzing historical amendment success rates, and comparative constitutional analysis showing democracies with lower amendment thresholds that maintain stability. Counter-corroboration from consensus-safeguard advocates emphasizes continued risks of majoritarian excess; the disagreement is genuine, not resolvable by appeal to simple fact.
narrative_ontology:disappearance_verdict(supermajority_threshold__minoritarian_veto_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__minoritarian_veto_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__minoritarian_veto_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(supermajority_threshold__minoritarian_veto_reading, 'none', 1).
narrative_ontology:epsilon_provenance(supermajority_threshold__minoritarian_veto_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supermajority_threshold__minoritarian_veto_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(supermajority_threshold__minoritarian_veto_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.65 to 0.82 over the interval because the supermajority threshold's lock-in function becomes more apparent as failed reform attempts accumulate and demographic shifts fail to move the supermajority calculus. Early in the interval, the threshold can be narrated as consensus protection; as decades pass with blocked reforms that majorities repeatedly seek, the extraction component becomes visible. Suppression is high (0.78) and rising because the constraint's persistence depends on active defense of the amendment barrier itself—constitutional scholars must argue for its necessity, status quo beneficiaries must prevent threshold-lowering even when majorities demand it, blocking minorities must maintain disciplined coalitions. Theater rises (0.25 to 0.41) as the consensus-protection narrative becomes increasingly theatrical: more effort goes into defending why the threshold is necessary than into demonstrating that it is actually protecting against majoritarian tyranny. Accessibility collapse is high (0.71) because once the threshold is understood as a veto mechanism, alternatives are severely constrained: majorities cannot amend the text; their only options are subnational policy, legal workaround, prolonged coalition-building, or accepting constitutional subordination. Resistance remains substantial (0.64) because contemporary reform movements actively contest the threshold's legitimacy through amendment campaigns, constitutional scholarship, and civil disobedience. The leveled coercion grid shows asymmetric effects: structural suppression rises more steeply than individual suppression (the institutional machinery for blocking becomes more formalized), while class-level resistance remains strongest (organized reform coalitions persist despite the barrier).
 *
 * PERSPECTIVAL GAP:
 *   From the status_quo_beneficiaries and entrenched_elites seats, the supermajority threshold appears as functional coordination—it protects against destabilizing change and keeps constitutional meaning stable. From the contemporary_majoritarian_coalitions and reform_coalitions_facing_threshold seats, the same threshold appears as extractive lock-in—it prevents democratic expression and protects minority privilege. The agenda_setter position is diffuse (no single agent sets the threshold; it is constitutionally embedded); this diffuseness is itself a suppression mechanism—no negotiable counterparty can be identified to demand threshold-lowering. The payer seats cannot exit by negotiating with a single threshold-holder; they must amend the constitution itself, which requires surmounting the threshold they seek to lower.
 *
 * DIRECTIONALITY LOGIC:
 *   Status quo beneficiaries and entrenched elites are near d=0.0 (full beneficiaries): the threshold subsidizes their position by preventing the constitutional changes that would threaten them. Contemporary majorities and reform coalitions are near d=1.0 (full targets): the threshold extracts from them by requiring supermajority (not majority) consensus. Blocking minorities are positioned asymmetrically: they benefit from veto power (low d on that dimension) but pay the political cost of maintaining discipline (higher d on that dimension). For blocking_minority I author d≈0.4 (slightly below symmetric) because they benefit from veto without bearing the full cost of the lock-in. Regional minorities sit nearest d=1.0 because they lack arbitrage exit and face permanent structural subordination. The directionality override for blocking_minority accounts for the fact that structural position (powerful state/regional actor) normally implies arbitrage exit, but the amendment barrier is itself what prevents that exit from being viable—they cannot escape the supermajority requirement by negotiating with the national majority. Their power is structural; their exit is blocked by the very mechanism they use.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading asserts that the supermajority threshold's founding mandate—to protect against tyrannical majoritarian change—has outlived its function because that protection is now provided by other institutional mechanisms (executive veto, judicial review, federalism). The amendment threshold persists not because it still solves the founding problem but because entrenched interests benefit from its lock-in effect and no agenda-setter has the incentive to lower it. The constraint exhibits mandatrophy: the founding problem is dead (protection against majoritarian tyranny is adequately supplied by other means); the founding problem status is marked as dead; and the disappearance verdict is world_rearranges (constitutional reform would proceed if the threshold vanished). The three-way mismatch (dead founding problem + world_rearranges verdict + theater_ratio rising) is the diagnostic signature of a constraint whose original justification has eroded but whose lock-in function sustains it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consensus_formation_vs_blocking,
    'Does the supermajority requirement function to protect against majoritarian excess (requiring deep consensus before change), or does it function to enable minoritarian blocking of changes majorities genuinely support?',
    'Historical analysis of amendment success rates and amendment failure reasons: do failed amendments fail because the supermajority barrier prevented passage of proposals lacking consensus, or because powerful blocking minorities vetoed proposals that majorities clearly supported? Comparative analysis of democracies with lower amendment thresholds: do they exhibit instability and majoritarian tyranny, or stable governance with responsiveness to majoritarian preferences?',
    'If consensus formation is the operative mechanism, the consensus_safeguard_reading''s legitimacy claim holds and the threshold operates closer to a rope. If blocking-of-supported-reforms is the operative mechanism, the minoritarian_veto_reading''s claim holds and the threshold is an extractive lock-in.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consensus_formation_vs_blocking, empirical, 'Whether the supermajority threshold functions as consensus-protection or minoritarian-veto.').

omega_variable(
    redundancy_of_other_institutional_checks,
    'To what extent do other institutional mechanisms (executive veto, judicial review, federalism, supermajority voting in legislatures, divided government) already protect against majoritarian tyranny, making the amendment supermajority threshold redundant?',
    'Comparative constitutional analysis of how democracies with lower amendment thresholds manage majoritarian risk through alternative checks. Historical investigation of periods when the amendment threshold was the binding constraint vs. periods when other mechanisms were binding. Institutional modeling of what majoritarian tyranny risk would remain if the amendment threshold were lowered while other checks remained.',
    'If redundancy is substantial, the threshold''s justification as tyranny-protection evaporates and the lock-in function becomes the primary explanation for its persistence. If alternative checks are insufficient and the threshold remains the primary guard against majoritarian excess, the consensus_safeguard reading retains legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(redundancy_of_other_institutional_checks, empirical, 'Whether the amendment threshold''s protective function is redundant with other institutional mechanisms.').

omega_variable(
    foundational_problem_historical_reconstruction,
    'What was the actual historical consensus among the founding framers about the supermajority amendment threshold''s purpose—protection against tyranny, or entrenchment of compromises reached at the founding, or something else?',
    'Close reading of founding-era constitutional debate, ratification documents, and the Federalist Papers. Historical context about whether the founders expected the amendment process to be frequently used or exceptionally rare. Comparison of how different founding cohorts (framers, ratifiers, early interpreters) understood and justified the threshold.',
    'If the historical consensus was tyranny-protection, both consensus_safeguard_reading and minoritarian_veto_reading can claim fidelity to founding intent while disputing whether that intent is still valid. If the historical consensus was entrenchment of founding compromises, the threshold''s lock-in function is historically continuous and the minoritarian_veto_reading must rest on normative critique of founding-era entrenchment, not on historical displacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foundational_problem_historical_reconstruction, empirical, 'Historical founding consensus about the amendment threshold''s purpose.').

omega_variable(
    demographic_change_and_supermajority_shift,
    'As demographic composition changes (regional population shifts, birth rates, immigration), does the supermajority requirement eventually respond to reflect new consensus, or does the blocking minority''s structural position (e.g., Senate malapportionment, state veto power) prevent demographic shifts from translating to supermajority shifts?',
    'Time-series analysis of whether historical demographic shifts have resulted in supermajority shifts toward reform (indicating the threshold can eventually respond to new majorities) or whether supermajority position has remained stable despite major demographic change (indicating structural lock-in independent of opinion).',
    'If demographic shifts eventually move supermajorities, the threshold operates as an extended-consensus mechanism and the adaptive_gradient reading''s claim that it responds to actual consensus formation has merit. If demographic shifts fail to move supermajorities, the lock-in function is structural and independent of majoritarian preference—the minoritarian_veto_reading''s claim about permanent entrenchment is supported.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demographic_change_and_supermajority_shift, empirical, 'Whether demographic change eventually shifts supermajorities or is locked out by constitutional structure.').

omega_variable(
    reading_kernel_contest_location,
    'Is the contestation between these readings located at the level of empirical fact (what does the threshold actually do), or at the level of normative evaluation (is lock-in of status quo legitimate), or at the level of reading the historical founding claim (what did the framers intend)?',
    'The three readings coexist across different interpretive traditions and political positions; the contest is not resolvable by empirical fact alone because readings agree on the threshold''s actual operation (it blocks reform) and disagree on whether that operation is legitimate or historically continuous with founding intent.',
    'Locating the contest determines what kind of resolution is possible: if empirical, factual discovery can resolve it; if normative, democratic choice must resolve it; if interpretive, constitutional scholarship and tradition determine the outcome. This omega routes the framing ambiguity through the existing apparatus for conceptual-type omegas rather than leaving it implicit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_contest_location, conceptual, 'What kind of disagreement sustains the kernel contest between readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__minoritarian_veto_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supe_tr_t0, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(supe_tr_t10, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 10, 0.29).
narrative_ontology:measurement(supe_tr_t20, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement(supe_tr_t30, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 30, 0.37).
narrative_ontology:measurement(supe_tr_t40, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 40, 0.39).
narrative_ontology:measurement(supe_tr_t50, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 50, 0.41).

% Extraction over time
narrative_ontology:measurement(supe_be_t0, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(supe_be_t10, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 10, 0.71).
narrative_ontology:measurement(supe_be_t20, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(supe_be_t30, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 30, 0.79).
narrative_ontology:measurement(supe_be_t40, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 40, 0.81).
narrative_ontology:measurement(supe_be_t50, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 50, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(supe_su_t0, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(supe_su_t10, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(supe_su_t20, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(supe_su_t30, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(supe_su_t40, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 40, 0.77).
narrative_ontology:measurement(supe_su_t50, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 50, 0.78).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=50
narrative_ontology:measurement(supe_grid_01, supermajority_threshold__minoritarian_veto_reading, accessibility_collapse(class), 0, 0.72).
narrative_ontology:measurement(supe_grid_02, supermajority_threshold__minoritarian_veto_reading, accessibility_collapse(class), 50, 0.8).
narrative_ontology:measurement(supe_grid_03, supermajority_threshold__minoritarian_veto_reading, accessibility_collapse(individual), 0, 0.65).
narrative_ontology:measurement(supe_grid_04, supermajority_threshold__minoritarian_veto_reading, accessibility_collapse(individual), 50, 0.71).
narrative_ontology:measurement(supe_grid_05, supermajority_threshold__minoritarian_veto_reading, accessibility_collapse(organizational), 0, 0.6).
narrative_ontology:measurement(supe_grid_06, supermajority_threshold__minoritarian_veto_reading, accessibility_collapse(organizational), 50, 0.68).
narrative_ontology:measurement(supe_grid_07, supermajority_threshold__minoritarian_veto_reading, accessibility_collapse(structural), 0, 0.68).
narrative_ontology:measurement(supe_grid_08, supermajority_threshold__minoritarian_veto_reading, accessibility_collapse(structural), 50, 0.75).
narrative_ontology:measurement(supe_grid_09, supermajority_threshold__minoritarian_veto_reading, resistance(class), 0, 0.71).
narrative_ontology:measurement(supe_grid_10, supermajority_threshold__minoritarian_veto_reading, resistance(class), 50, 0.74).
narrative_ontology:measurement(supe_grid_11, supermajority_threshold__minoritarian_veto_reading, resistance(individual), 0, 0.58).
narrative_ontology:measurement(supe_grid_12, supermajority_threshold__minoritarian_veto_reading, resistance(individual), 50, 0.61).
narrative_ontology:measurement(supe_grid_13, supermajority_threshold__minoritarian_veto_reading, resistance(organizational), 0, 0.62).
narrative_ontology:measurement(supe_grid_14, supermajority_threshold__minoritarian_veto_reading, resistance(organizational), 50, 0.68).
narrative_ontology:measurement(supe_grid_15, supermajority_threshold__minoritarian_veto_reading, resistance(structural), 0, 0.48).
narrative_ontology:measurement(supe_grid_16, supermajority_threshold__minoritarian_veto_reading, resistance(structural), 50, 0.52).
narrative_ontology:measurement(supe_grid_17, supermajority_threshold__minoritarian_veto_reading, stakes_inflation(class), 0, 0.71).
narrative_ontology:measurement(supe_grid_18, supermajority_threshold__minoritarian_veto_reading, stakes_inflation(class), 50, 0.79).
narrative_ontology:measurement(supe_grid_19, supermajority_threshold__minoritarian_veto_reading, stakes_inflation(individual), 0, 0.64).
narrative_ontology:measurement(supe_grid_20, supermajority_threshold__minoritarian_veto_reading, stakes_inflation(individual), 50, 0.72).
narrative_ontology:measurement(supe_grid_21, supermajority_threshold__minoritarian_veto_reading, stakes_inflation(organizational), 0, 0.58).
narrative_ontology:measurement(supe_grid_22, supermajority_threshold__minoritarian_veto_reading, stakes_inflation(organizational), 50, 0.67).
narrative_ontology:measurement(supe_grid_23, supermajority_threshold__minoritarian_veto_reading, stakes_inflation(structural), 0, 0.55).
narrative_ontology:measurement(supe_grid_24, supermajority_threshold__minoritarian_veto_reading, stakes_inflation(structural), 50, 0.62).
narrative_ontology:measurement(supe_grid_25, supermajority_threshold__minoritarian_veto_reading, suppression(class), 0, 0.68).
narrative_ontology:measurement(supe_grid_26, supermajority_threshold__minoritarian_veto_reading, suppression(class), 50, 0.79).
narrative_ontology:measurement(supe_grid_27, supermajority_threshold__minoritarian_veto_reading, suppression(individual), 0, 0.72).
narrative_ontology:measurement(supe_grid_28, supermajority_threshold__minoritarian_veto_reading, suppression(individual), 50, 0.82).
narrative_ontology:measurement(supe_grid_29, supermajority_threshold__minoritarian_veto_reading, suppression(organizational), 0, 0.58).
narrative_ontology:measurement(supe_grid_30, supermajority_threshold__minoritarian_veto_reading, suppression(organizational), 50, 0.71).
narrative_ontology:measurement(supe_grid_31, supermajority_threshold__minoritarian_veto_reading, suppression(structural), 0, 0.61).
narrative_ontology:measurement(supe_grid_32, supermajority_threshold__minoritarian_veto_reading, suppression(structural), 50, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__minoritarian_veto_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(supermajority_threshold__minoritarian_veto_reading, 0.18).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold__consensus_safeguard_reading).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold__adaptive_gradient_reading).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, senate_malapportionment__structural_veto).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, constitutional_amendment_process__procedural_lock_in).

% DUAL FORMULATION NOTE:
% The supermajority_threshold kernel decomposes into three structurally distinct constraint stories, each with its own ε and legitimacy claim. This story (minoritarian_veto_reading) instantiates the reading that interprets the threshold as a lock-in mechanism for entrenched interests, claiming high extractiveness (0.82) and identifying beneficiaries and victims. The consensus_safeguard_reading and adaptive_gradient_reading offer competing interpretations of the same constitutional rule with different ε values and different beneficiary/victim structures. All three stories link to one another via network.affects_constraints because the same constitutional provision grounds all three claims; each reading's legitimacy depends partly on whether the sibling readings' premises hold. This is a genuine kernel contest, not a measurement ambiguity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(supermajority_threshold__minoritarian_veto_reading, powerful, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
