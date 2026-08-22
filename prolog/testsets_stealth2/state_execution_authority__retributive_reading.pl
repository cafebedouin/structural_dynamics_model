% ============================================================================
% CONSTRAINT STORY: state_execution_authority__retributive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_execution_authority__retributive_reading, []).

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
 *   constraint_id: state_execution_authority__retributive_reading
 *   human_readable: State Execution Authority — Retributive Reading (Moral Balance Through Proportionate Death)
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates the retributive reading of the
 *   state_execution_authority kernel: the standing arrangement under contest
 *   is the set of statutes, courts, clemency mechanisms, and execution
 *   protocols by which retentionist polities put convicted offenders to death
 *   as the proportionate answer to heinous crime. Assessed by this reading's
 *   own lights, the arrangement's warrant is desert, not prevention:
 *   execution is owed, and its moral-restoration function is what
 *   imprisonment allegedly cannot supply. The epsilon authored here refers to
 *   that standing execution arrangement — never to the abolitionist
 *   alternative this reading rejects or the deterrence rationale it does not
 *   invoke. Sibling readings (deterrence_reading, abolition_reading) are
 *   separate constraints with their own epsilon, beneficiaries, and
 *   classifications; they appear here only as network edges and omega
 *   content. KEY AGENTS (by structural relationship): -
 *   condemned_capital_offenders: primary target (powerless/trapped) — bears
 *   the terminal cost - wrongfully_convicted_executed: secondary target
 *   (powerless/trapped) — bears the framework's accepted error cost -
 *   victims_families_of_heinous_crimes: declared beneficiary
 *   (moderate/constrained) — the constituency the restoration promise
 *   addresses - law_abiding_community_members: diffuse beneficiary
 *   (moderate/constrained) - state_execution_authority: agenda setter
 *   (institutional/arbitrage) — administers the machinery and could repeal it
 *   - abolition_advocacy_movement: excluded challenger (organized/mobile) -
 *   constitutional_criminal_law_scholars: analytical observer — sees the full
 *   structure
 *
 * KEY AGENTS:
 *   - condemned_capital_offenders: primary target (powerless/trapped) — bears the arrangement's terminal cost
 *   - wrongfully_convicted_executed: secondary target (powerless/trapped) — bears the accepted error cost
 *   - victims_families_of_heinous_crimes: declared beneficiary (moderate/constrained) — restoration-promise constituency
 *   - law_abiding_community_members: diffuse beneficiary (moderate/constrained)
 *   - state_execution_authority: agenda setter (institutional/arbitrage) — administers and could repeal
 *   - abolition_advocacy_movement: excluded challenger (organized/mobile)
 *   - constitutional_criminal_law_scholars: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__retributive_reading, 0.71).
domain_priors:suppression_score(state_execution_authority__retributive_reading, 0.58).
domain_priors:theater_ratio(state_execution_authority__retributive_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__retributive_reading, tangled_rope).
narrative_ontology:human_readable(state_execution_authority__retributive_reading, "State Execution Authority — Retributive Reading (Moral Balance Through Proportionate Death)").
narrative_ontology:topic_domain(state_execution_authority__retributive_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__retributive_reading, 'd0fc1b10-6e8c-456d-95b5-d21bd146b168').
narrative_ontology:cs_kernel_codification('d0fc1b10-6e8c-456d-95b5-d21bd146b168', formalized).
narrative_ontology:cs_authority_grounding('d0fc1b10-6e8c-456d-95b5-d21bd146b168', lineage).
narrative_ontology:cs_interpretation_layer_present('d0fc1b10-6e8c-456d-95b5-d21bd146b168').
narrative_ontology:cs_reading_relation('d0fc1b10-6e8c-456d-95b5-d21bd146b168', state_execution_authority__deterrence_reading, coexists_with).
narrative_ontology:cs_reading_relation('d0fc1b10-6e8c-456d-95b5-d21bd146b168', state_execution_authority__abolition_reading, forecloses).
narrative_ontology:cs_axiom('d0fc1b10-6e8c-456d-95b5-d21bd146b168', foundational, proportional_death_required_for_moral_balance).
narrative_ontology:cs_axiom_status(proportional_death_required_for_moral_balance, holdable).
narrative_ontology:cs_axiom_grounding('d0fc1b10-6e8c-456d-95b5-d21bd146b168', proportional_death_required_for_moral_balance, deontological).
narrative_ontology:cs_axiom('d0fc1b10-6e8c-456d-95b5-d21bd146b168', secondary, wrongful_execution_error_not_disqualifier).
narrative_ontology:cs_axiom_status(wrongful_execution_error_not_disqualifier, holdable).
narrative_ontology:cs_axiom_grounding('d0fc1b10-6e8c-456d-95b5-d21bd146b168', wrongful_execution_error_not_disqualifier, deontological).
narrative_ontology:cs_reference_frame('d0fc1b10-6e8c-456d-95b5-d21bd146b168', lex_talionis_proportional_desert_order).
narrative_ontology:cs_drift_state('d0fc1b10-6e8c-456d-95b5-d21bd146b168', contemporary_retentionist_jurisdictions, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d0fc1b10-6e8c-456d-95b5-d21bd146b168', '').
narrative_ontology:cs_kernel_id(state_execution_authority__retributive_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, victims_families_of_heinous_crimes).
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, law_abiding_community_members).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, condemned_capital_offenders).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, wrongfully_convicted_executed).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, state_execution_authority).
narrative_ontology:constraint_vindicates(state_execution_authority__retributive_reading, retributive_desert_doctrine).
narrative_ontology:constraint_vindicates(state_execution_authority__retributive_reading, lex_talionis_principle).
narrative_ontology:constraint_vindicates(state_execution_authority__retributive_reading, moral_balance_hypothesis).
narrative_ontology:constraint_vindicates(state_execution_authority__retributive_reading, state_monopoly_on_legitimate_lethal_force).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convicted under statutes enumerating death-eligible offenses; pass through sentencing and the appellate gauntlet toward an execution date. Clemency exists but is exercised rarely. They bear the arrangement's terminal cost — the framework counts this cost as payment of a moral debt rather than as a loss imposed, which is precisely what this reading asserts and what the metrics measure independently.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, condemned_capital_offenders, payer,
    powerless, immediate, trapped, national).

% Convicted and sentenced for crimes they did not commit, identified mostly through post-conviction investigation and sometimes only after execution. The framework classifies their deaths as tragic error to be minimized procedurally rather than as grounds for abandoning the practice.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, wrongfully_convicted_executed, payer,
    powerless, biographical, trapped, national).

% Lost relatives to murder. The framework names them as the constituency whose moral injury the perpetrator's death addresses. Outcomes diverge: some report the long-awaited relief arriving with the execution, others report emptiness afterward, and an organized minority publicly opposes executions carried out in their relatives' names.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, victims_families_of_heinous_crimes, beneficiary,
    moderate, biographical, constrained, local).

% The diffuse public whose sense of moral order the framework claims to restore when the severest crimes meet the severest sanctioned response. They receive the symbolic good when executions proceed as promised and bear diffuse costs: the fiscal weight of capital litigation, exposure to error risk as potential future defendants, and the standing precedent of state killing.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, law_abiding_community_members, beneficiary,
    moderate, generational, constrained, national).

% Legislatures enumerate capital crimes and fund prosecution; courts administer the appellate gauntlet; governors hold clemency; corrections agencies conduct executions. The apparatus wrote the rules it administers and can revise or repeal them by ordinary law. Each completed execution re-demonstrates the polity's supreme authority over life; each revealed error lands as political risk the apparatus absorbs.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, state_execution_authority, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(state_execution_authority__retributive_reading, state_execution_authority, beneficiary).

% Campaigns for categorical repeal through legislation, litigation, religious witness, and international treaty pressure. Structurally absent from clemency decisions, execution-protocol drafting, and capital-statute committees in retentionist jurisdictions; its absence from the administrative seats is maintained by the same enforcement machinery that runs the executions.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, abolition_advocacy_movement, excluded,
    organized, generational, mobile, continental).

% Document error rates, compare retentionist and abolitionist jurisdictions, and test the coherence of proportionality claims. Their findings inform neither clemency decisions nor statutory drafting as binding input; they see the full structure without holding any operational seat.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, constitutional_criminal_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_execution_authority__retributive_reading, state_execution_authority).
narrative_ontology:fixing_cost_class(state_execution_authority__retributive_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels the community's response to heinous killing through institutional procedure instead of private vengeance: the state monopoly on lethal punishment displaces feud and vigilantism, and supplies a determinate terminal point — the debt is paid, the book closed — that indeterminate incarceration does not.
% TRANSFER_FUNCTION: Moves the condemned offender's remaining life, and the preceding years of death-row existence, from the offender to the community's moral ledger as payment of an asserted debt; moves a recurring demonstration of supreme sovereign authority to the state; moves symbolic closure to victims' families, with delivery empirically contested.
% ABSENT_VOICES: The condemned themselves — their testimony enters the record but carries no vote at any decision seat. The not-yet-discovered wrongfully convicted have no seat until posthumous exoneration. Abolition advocates are structurally absent from clemency boards, protocol drafting, and statute committees in retentionist jurisdictions. Survivors who oppose execution are spoken for by the framework's beneficiary framing rather than seated as dissenters.
% DISAPPEARANCE_RATIONALE: Capital statutes, death rows, appellate gauntlets, clemency machinery, execution protocols, and the punitive electoral economy built around them would all reorganize overnight. Custodial function would migrate to life-without-parole regimes while the moral-restoration claim lost its instrument; victims'-family expectations and prosecutorial charging practice would reset within a decade.
% FOUNDING_PROBLEM: Unregulated private vengeance after heinous killing — blood feud, lynching, vendetta — combined with the felt inadequacy of ordinary penalties for acts that rupture the moral order. The arrangement was built to monopolize the answer to such acts in state hands and to make the answer proportionate to the act.
% FOUNDING_PROBLEM_CORROBORATION: Historians of penal transition corroborate the founding problem from outside the beneficiary set: the documented displacement of feud and private vengeance by a state monopoly on punishment is standard historiography, not retributivist self-attestation. Criminological data corroborate that heinous killings persist and continue to provoke severe-punishment demand. Religious bodies and human-rights monitors — adversaries of the arrangement — corroborate the problem's liveness while rejecting the execution solution. Corroboration for the problem is broad; corroboration for this reading's solution is confined to the beneficiary coalition.
narrative_ontology:disappearance_verdict(state_execution_authority__retributive_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__retributive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__retributive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_execution_authority__retributive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__retributive_reading, 0.71, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__retributive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_execution_authority__retributive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_execution_authority__retributive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.71) because the arrangement's cost is the offender's life itself, delivered irreversibly, and the reading's own moral-restoration requirement forecloses the cheaper substitute of imprisonment — the framework cannot discount its central cost without ceasing to be the retributive framework. Suppression (0.58) is authored as a raw structural property, unscaled: the condemned have no exit short of clemency and retentionist politics marginalize repeal, but abolition remains democratically reachable and has been reached in multiple jurisdictions, so suppression is substantial rather than total. Theater (0.34) reflects real machinery — executions occur, appeals decide — shadowed by performative layers: superdue-process appellate ritual that performs scrupulousness while error persists, clemency ceremonies that perform mercy without granting it, and post-botched-execution protocol revisions that perform control. Accessibility collapse (0.55) and resistance (0.58) sit in the tangled-rope band: alternatives remain partly open and meet persistent organized resistance. All temporal series share one grid. Extractiveness climbs through the execution boom then plateaus as exonerations accumulate. Suppression_requirement traces the enforcement arc the narrative actually tracks — build-up through the expanded-statute era (funded prosecutions, streamlined appeals), peak, then decay as moratoria, lethal-drug shortages, and error revelations eroded enforcement capacity — which is why it is tracked temporally while base_properties.suppression records the end-state. Claimed type is tangled_rope on my structural read: genuine coordination (vengeance channeled into procedure, displacing feud and vigilantism; a determinate terminal point for moral debts) fused with asymmetric, irreversible extraction (offenders' lives, wrongful deaths accepted as tragic error), held together by active enforcement. Metrics were authored independently of that claim; where the engine's per-seat computation diverges, that divergence is the datum.
 *
 * PERSPECTIVAL GAP:
 *   From the condemned and wrongfully convicted seats the arrangement computes as enforced life-taking with zero exit — the experience of a snare regardless of its warrant. From the state seat it computes as lawful desert-administration that the state itself authored and can revise. From the victims'-families seat it computes as a restoration promise whose delivery is contested (omega moral_restoration_delivery). From the scholarly seat it computes as a contested framework whose error-tolerance axiom is load-bearing. The engine derives these per-seat classifications from power, exit, and declared position; the divergence between payer-seat and agenda-setter-seat computation is the perspectival fact this story exists to register.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (victims' families, law-abiding community) derive low d — the arrangement subsidizes their moral-order interest. Declared payers (condemned offenders, wrongfully convicted) derive high d, pushed toward the full-target end by trapped exit: no arbitrage, no mobility, the sentence itself is the trap. The state apparatus sits near the beneficiary end with arbitrage-grade exit — it wrote the rules and faces no constraint it did not author, which is why it appears as agenda_setter with a beneficiary secondary role and as the receipt seat in gain_flow. National spatial scope modestly amplifies effective extraction by complicating verification of error rates and of the restoration good's delivery. Suppression is deliberately discussed unscaled: only extractiveness rides directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how a polity answers acts that rupture its moral order without collapsing into private vengeance — remains live: heinous killings persist and provoke the same demand the arrangement was built to channel. Status live crossed with disappearance verdict world_rearranges yields no zombie flag; mandate and operation still correspond. The tangled_rope classification guards both mislabels: reading the arrangement as pure snare erases the real coordination function (feud-displacement, determinate debt-payment) that even critics concede historically; reading it as pure rope erases the asymmetric, irreversible extraction and the accepted wrongful-death toll that no coordination benefit fully prices. The live drift risk is pitonward: if omega moral_restoration_delivery resolves negatively — execution systematically failing to restore while the machinery persists on inertia and ritual — theater_ratio climbs and the arrangement decays into performance. The measurement series' steadily rising theater line is the early indicator of exactly that trajectory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the state_execution_authority kernel — which reading governs a given polity''s arrangement, and how would the sibling readings restructure it?',
    'Jurisdictional adoption patterns and the operative warrant cited in capital statutes, clemency rulings, and repeal debates: deterrence-warranted statutes shift beneficiaries toward the potential-victim public and make epsilon contingent on measured deterrent effect; abolition-warranted repeal prohibits the arrangement outright.',
    'If the deterrence reading governs, the executed offender is instrumental cost rather than deserved payment and epsilon floats with the empirical deterrence literature; if the abolition reading governs, the arrangement is impermissible rather than extractive and this story''s beneficiary set dissolves.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: this story is the retributive reading of a three-reading kernel; siblings are separate constraints.').

omega_variable(
    moral_restoration_delivery,
    'Does execution actually deliver the moral restoration the reading promises — closure to victims'' families, restored communal balance — or is the good asserted rather than received?',
    'Longitudinal studies of survivors before and after the perpetrator''s execution against matched controls; systematic review of the closure literature across retentionist jurisdictions.',
    'If delivery fails systematically, the framework''s coordination function hollows while the machinery persists — driving pitonward drift with rising theater_ratio; if delivery holds for a substantial fraction, the beneficiary declarations stand as written.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_restoration_delivery, empirical, 'Whether the restoration good the reading trades on is actually delivered to its declared beneficiaries.').

omega_variable(
    wrongful_execution_threshold,
    'The framework accepts wrongful execution as tragic error without invalidating itself — is that tolerance stable, and at what error rate does the retributive framework''s own desert logic self-undermine?',
    'Comparative analysis of exonerations-per-execution across retentionist jurisdictions, paired with doctrinal analysis of the points at which retributivist theorists themselves suspend or retract support.',
    'Above the threshold, the arrangement''s claimed desert-tracking fails and extraction continues without its warrant — pushing classification from tangled_rope toward snare; below it, the error-tolerance axiom remains load-bearing but defensible within the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_execution_threshold, empirical, 'Whether the accepted wrongful-death toll stays inside the bound the reading''s own proportionality logic can absorb.').

omega_variable(
    proportionality_uniqueness,
    'Is death uniquely proportionate to heinous crime such that imprisonment cannot satisfy the moral-restoration requirement, or is that uniqueness a doctrinal assertion?',
    'Conceptual analysis within moral philosophy of proportionality and desert; no empirical resolution is available — the hinge separates this reading from the abolition reading and cannot be settled by outcome data.',
    'If life imprisonment can satisfy proportionality, the moral-restoration requirement collapses, the substitution ban fails, and this reading loses its distinguishing axiom — converging structurally toward the abolition reading''s permissibility space.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proportionality_uniqueness, conceptual, 'The metaphysical hinge on which the reading''s impossibility-of-substitution claim rests.').

omega_variable(
    error_risk_valuation,
    'How much wrongful-execution risk may a polity accept in exchange for desert satisfaction — and who is entitled to make that trade on behalf of those who bear the risk?',
    'Political resolution only: referendum, legislative repeal, or constitutional adjudication; the trade is a values question about whose risk may be priced by whom, not an empirical one.',
    'A polity that refuses to price others'' lives for its moral order converges on the abolition reading; one that accepts the trade ratifies this reading''s error-tolerance axiom and its beneficiary structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(error_risk_valuation, preference, 'The preference-level question underlying the framework''s acceptance of irreversible error.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__retributive_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__retributive_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(stat_tr_t0, observed).
narrative_ontology:measurement(stat_tr_t8, state_execution_authority__retributive_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement_basis(stat_tr_t8, observed).
narrative_ontology:measurement(stat_tr_t16, state_execution_authority__retributive_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement_basis(stat_tr_t16, observed).
narrative_ontology:measurement(stat_tr_t24, state_execution_authority__retributive_reading, theater_ratio, 24, 0.29).
narrative_ontology:measurement_basis(stat_tr_t24, observed).
narrative_ontology:measurement(stat_tr_t32, state_execution_authority__retributive_reading, theater_ratio, 32, 0.31).
narrative_ontology:measurement_basis(stat_tr_t32, observed).
narrative_ontology:measurement(stat_tr_t40, state_execution_authority__retributive_reading, theater_ratio, 40, 0.33).
narrative_ontology:measurement_basis(stat_tr_t40, observed).
narrative_ontology:measurement(stat_tr_t48, state_execution_authority__retributive_reading, theater_ratio, 48, 0.34).
narrative_ontology:measurement_basis(stat_tr_t48, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__retributive_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement_basis(stat_be_t0, observed).
narrative_ontology:measurement(stat_be_t8, state_execution_authority__retributive_reading, base_extractiveness, 8, 0.64).
narrative_ontology:measurement_basis(stat_be_t8, observed).
narrative_ontology:measurement(stat_be_t16, state_execution_authority__retributive_reading, base_extractiveness, 16, 0.68).
narrative_ontology:measurement_basis(stat_be_t16, observed).
narrative_ontology:measurement(stat_be_t24, state_execution_authority__retributive_reading, base_extractiveness, 24, 0.7).
narrative_ontology:measurement_basis(stat_be_t24, observed).
narrative_ontology:measurement(stat_be_t32, state_execution_authority__retributive_reading, base_extractiveness, 32, 0.71).
narrative_ontology:measurement_basis(stat_be_t32, observed).
narrative_ontology:measurement(stat_be_t40, state_execution_authority__retributive_reading, base_extractiveness, 40, 0.72).
narrative_ontology:measurement_basis(stat_be_t40, observed).
narrative_ontology:measurement(stat_be_t48, state_execution_authority__retributive_reading, base_extractiveness, 48, 0.71).
narrative_ontology:measurement_basis(stat_be_t48, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__retributive_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(stat_su_t0, observed).
narrative_ontology:measurement(stat_su_t8, state_execution_authority__retributive_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement_basis(stat_su_t8, observed).
narrative_ontology:measurement(stat_su_t16, state_execution_authority__retributive_reading, suppression_requirement, 16, 0.72).
narrative_ontology:measurement_basis(stat_su_t16, observed).
narrative_ontology:measurement(stat_su_t24, state_execution_authority__retributive_reading, suppression_requirement, 24, 0.76).
narrative_ontology:measurement_basis(stat_su_t24, observed).
narrative_ontology:measurement(stat_su_t32, state_execution_authority__retributive_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement_basis(stat_su_t32, observed).
narrative_ontology:measurement(stat_su_t40, state_execution_authority__retributive_reading, suppression_requirement, 40, 0.63).
narrative_ontology:measurement_basis(stat_su_t40, observed).
narrative_ontology:measurement(stat_su_t48, state_execution_authority__retributive_reading, suppression_requirement, 48, 0.58).
narrative_ontology:measurement_basis(stat_su_t48, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__retributive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_execution_authority__retributive_reading, deterrence_reading).
narrative_ontology:affects_constraint(state_execution_authority__retributive_reading, abolition_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'capital punishment' conflates three structurally distinct claims, decomposed per the epsilon-invariance principle. This story holds the retributive warrant fixed so epsilon reflects the moral-restoration arrangement alone; deterrence_reading carries the empirical-prevention warrant (epsilon contingent on deterrent-effect evidence) and abolition_reading carries the categorical-prohibition warrant (the arrangement as rights violation). Edges run from this story to both siblings: wrongful-execution evidence contaminates the retributive and deterrence claims differently, and repeal in one jurisdiction shifts legitimacy conditions for the others. Upstream/downstream is warrant-dependent rather than fixed — the deterrence reading is upstream where legislatures cite prevention, downstream where courts cite desert.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
