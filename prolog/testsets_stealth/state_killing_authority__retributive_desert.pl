% ============================================================================
% CONSTRAINT STORY: state_killing_authority__retributive_desert
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_authority__retributive_desert, []).

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
 *   constraint_id: state_killing_authority__retributive_desert
 *   human_readable: State Killing Authority — Retributive Desert Reading (Lex Talionis Proportionality)
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   The standing arrangement under contest is the capital-punishment regime
 *   as grounded in retributive desert: the state kills convicted murderers
 *   because murder forfeits the killer's right to life and proportional
 *   justice (lex talionis) requires a life for a life. This file instantiates
 *   the retributive_desert reading of the state_killing_authority kernel as
 *   one clean, epsilon-invariant constraint; the deterrence and abolition
 *   readings are separate stories with their own epsilon and victim sets,
 *   linked through the network. Epsilon's referent is the existing
 *   arrangement assessed by this reading's own lights: the reading concedes
 *   no extraction from the guilty condemned (execution delivers what desert
 *   owes, as collecting a debt is not extraction) but must concede the
 *   wrongly convicted (its own proportionality premise counts their execution
 *   as the gravest injustice), the apparatus's over-punishment (multi-decade
 *   death-row delay), and collateral costs to the condemned's innocent kin.
 *   The claimed type and the metrics are authored independently: the claim
 *   states tangled_rope — a genuine coordination function (feud-suppression
 *   and a proportional answer to murder) carrying real conceded extraction —
 *   while the metrics describe what the reading itself must concede of the
 *   arrangement's operation.
 *
 * KEY AGENTS:
 *   - state_execution_authority: agenda-setter and beneficiary (institutional/arbitrage) — administers the capital statutes and collects the vindicated punitive authority each execution demonstrates
 *   - murder_victims_families: primary beneficiary (moderate/constrained) — receives the arrangement's vindication; cannot exit the demand for an answer
 *   - murdered_victims_posthumous: nominal beneficiary, non-agent (powerless/trapped) — admitted to the beneficiary set by the reading's own vindication claim; collects nothing
 *   - condemned_convicted_murderers: primary target (powerless/trapped) — bears the maximal cost; by this reading's own lights the guilty among them receive desert, not extraction
 *   - wrongly_convicted_defendants: pure target (powerless/trapped) — the conceded extraction component the reading's own proportionality premise condemns
 *   - execution_personnel: cost-bearing instrument (moderate/constrained) — administers the acts and bears their psychological and moral costs
 *   - abolitionist_advocates: excluded voice (organized/mobile) — contests the forfeiture premise from outside the sentencing conversation
 *   - international_human_rights_bodies: analytical observer (institutional/analytical) — documents error, disparity, and method-of-execution practice against evolving standards
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__retributive_desert, 0.45).
domain_priors:suppression_score(state_killing_authority__retributive_desert, 0.85).
domain_priors:theater_ratio(state_killing_authority__retributive_desert, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, extractiveness, 0.45).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__retributive_desert, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__retributive_desert, "State Killing Authority — Retributive Desert Reading (Lex Talionis Proportionality)").
narrative_ontology:topic_domain(state_killing_authority__retributive_desert, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_killing_authority__retributive_desert).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__retributive_desert, '22b5ff3d-3961-4478-9aca-c9be7ed6524f').
narrative_ontology:cs_kernel_codification('22b5ff3d-3961-4478-9aca-c9be7ed6524f', formalized).
narrative_ontology:cs_authority_grounding('22b5ff3d-3961-4478-9aca-c9be7ed6524f', lineage).
narrative_ontology:cs_interpretation_layer_present('22b5ff3d-3961-4478-9aca-c9be7ed6524f').
narrative_ontology:cs_reading_relation('22b5ff3d-3961-4478-9aca-c9be7ed6524f', state_killing_authority__deterrence_instrument, coexists_with).
narrative_ontology:cs_reading_relation('22b5ff3d-3961-4478-9aca-c9be7ed6524f', state_killing_authority__categorical_abolition, forecloses).
narrative_ontology:cs_axiom('22b5ff3d-3961-4478-9aca-c9be7ed6524f', foundational, murder_forfeits_right_to_life).
narrative_ontology:cs_axiom_status(murder_forfeits_right_to_life, holdable).
narrative_ontology:cs_axiom_grounding('22b5ff3d-3961-4478-9aca-c9be7ed6524f', murder_forfeits_right_to_life, deontological).
narrative_ontology:cs_axiom('22b5ff3d-3961-4478-9aca-c9be7ed6524f', foundational, death_required_for_proportional_justice).
narrative_ontology:cs_axiom_status(death_required_for_proportional_justice, holdable).
narrative_ontology:cs_axiom_grounding('22b5ff3d-3961-4478-9aca-c9be7ed6524f', death_required_for_proportional_justice, deontological).
narrative_ontology:cs_reference_frame('22b5ff3d-3961-4478-9aca-c9be7ed6524f', lex_talionis_proportional_authority).
narrative_ontology:cs_drift_state('22b5ff3d-3961-4478-9aca-c9be7ed6524f', contemporary_post_innocence_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('22b5ff3d-3961-4478-9aca-c9be7ed6524f', '').
narrative_ontology:cs_kernel_id(state_killing_authority__retributive_desert, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, state_execution_authority).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, murder_victims_families).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, murdered_victims_posthumous).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, condemned_convicted_murderers).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, wrongly_convicted_defendants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, execution_personnel).
narrative_ontology:constraint_vindicates(state_killing_authority__retributive_desert, lex_talionis_proportionality_doctrine).
narrative_ontology:constraint_vindicates(state_killing_authority__retributive_desert, murderer_rights_forfeiture_doctrine).
narrative_ontology:constraint_vindicates(state_killing_authority__retributive_desert, state_punitive_authority_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and maintains capital statutes, charges capital cases, and carries out sentences through its courts and corrections apparatus. Grounds its punitive authority in the proportionality norm — the claim that answering death with death is what justice requires — and collects the legitimacy that flows from visibly delivering it. It can amend, commute, or abolish the arrangement by ordinary legislative or executive act; numerous jurisdictions have done exactly that.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, state_execution_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Lost a family member to homicide. The arrangement delivers its answer to them: an execution framed as the proportionate response to their loss and as vindication of the person killed. They cannot recover what was taken and cannot exit the demand for an answer; the state's channel is the only one on offer, though a documented minority of such families publicly reject execution and advocate life imprisonment instead.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, murder_victims_families, beneficiary,
    moderate, biographical, constrained, local).

% The person killed. The reading's structure admits them to the beneficiary set posthumously: the execution is said to vindicate their moral standing by answering their killing in kind. They cannot collect anything; the vindication accrues to their memory and to the living. Listed for structural completeness of the reading's own beneficiary claim; excluded from derivation as a non-collecting entity.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, murdered_victims_posthumous, beneficiary,
    powerless, civilizational, trapped, local).
narrative_ontology:stakeholder_non_agent(state_killing_authority__retributive_desert, murdered_victims_posthumous).

% Convicted of capital murder and sentenced to death. Bears the arrangement's ultimate cost: the loss of life, preceded by years or decades in death-row conditions. By this reading's own lights the guilty among them are receiving what they owe rather than being extracted from; structurally they bear the maximal cost and have no exit — commutation or exoneration runs entirely through the state's own machinery.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, condemned_convicted_murderers, payer,
    powerless, biographical, trapped, local).

% Convicted and sentenced for murders they did not commit. The reading's own proportionality premise counts their execution as the gravest possible injustice — a life taken with no desert to answer. Exonerations from death row outnumber those from any other sentence category; some exit exists through appeals and DNA testing, but it is slow, procedurally barred in places, and for the executed it arrives only posthumously.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, wrongly_convicted_defendants, payer,
    powerless, biographical, trapped, national).

% Corrections officers, execution teams, and chaplains who administer the arrangement's acts. They bear its psychological and moral costs — documented stress, turnover, and conscientious refusal — without setting policy or collecting the vindication. Exit means resigning from an apparatus that replaces them.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, execution_personnel, payer,
    moderate, biographical, constrained, local).

% Organizations and movements that reject the arrangement outright — innocence networks, religious bodies, international human rights campaigns. Within retentionist jurisdictions they are heard in litigation and at referendum but excluded from the desert framing itself: the proportionality question is decided by legislatures, prosecutors, and juries operating inside the forfeiture premise they deny. Their venue is the long campaign, not the sentencing hearing.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, abolitionist_advocates, excluded,
    organized, generational, mobile, national).

% Treaty bodies and courts that assess the arrangement against evolving human-rights standards and document its application — error rates, method-of-execution litigation, racial and geographic disparity. They impose no direct domestic enforcement in retentionist states but supply the external record the domestic debate cites.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_authority__retributive_desert, state_execution_authority).
narrative_ontology:fixing_cost_class(state_killing_authority__retributive_desert, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts the private demand for vengeance into a single state-administered, rule-bound act: it solves the feud-escalation problem by monopolizing the answer to murder and calibrating it to the talionic standard (death for death), and it supplies the vindication channel through which the community answers the killing of one of its members.
% TRANSFER_FUNCTION: Moves the condemned person's remaining life from the convicted to the state's punitive account; moves vindication and closure to the murder victim's survivors; moves demonstrated sovereign authority to the state; moves the moral burden of the killing itself to execution personnel.
% ABSENT_VOICES: The executed innocent — permanently silent, their exonerations arriving only as case files; the condemned's own families, whose loss the proportionality ledger does not count; abolitionist advocates, who are heard in court and at referendum but excluded from the desert framing inside which sentencing decisions are made; and the murdered victims' family members who oppose execution, whose objection the vindication frame does not accommodate.
% DISAPPEARANCE_RATIONALE: Capital statutes would fall to life-without-parole regimes within a legislative cycle in retentionist jurisdictions; death rows would be commuted en masse; victims' families would lose the specific vindication channel and a portion would redirect into advocacy; the state would need a different ground for its punitive-authority claim; the wrongly convicted would face grave but reversible rather than irreversible error. The world rearranges because every named seat's arrangements depend on the constraint.
% FOUNDING_PROBLEM: Before the state monopolized punishment, murder was answered by kin vendetta: blood feud with escalation cycles that consumed whole communities. Lex talionis — an eye for an eye, a life for a life — was built as an escalation cap: a proportionality ceiling administered by authority, replacing open-ended retaliation with a bounded, state-executed answer. The founding problem: how to answer murder proportionately without feud.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: criminal-law historians document the talionic rule's origin as an escalation cap on kin vendetta in ancient Near Eastern, biblical, and Roman law; abolitionist philosophers concede the standing demand for proportionate response to grave crime while disputing that death is its required form; innocence-project litigators corroborate that the problem murder poses remains live. No serious party attests the founding problem itself is dead — the dispute is over the answer, not the problem.
narrative_ontology:disappearance_verdict(state_killing_authority__retributive_desert, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__retributive_desert, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__retributive_desert, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_killing_authority__retributive_desert, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__retributive_desert, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__retributive_desert_tests).
:- end_tests(state_killing_authority__retributive_desert_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.45 is the reading-indexed concession: near-zero for the guilty-desert component, weighted by the conceded error class (survival-analysis estimates put roughly 4% of the death-sentenced at actually innocent), the apparatus over-punishment (average death-row stay has roughly tripled since the interval's start, exceeding what retributivist scholarship itself argues desert requires), and collateral costs to the condemned's innocent kin. Suppression 0.85 is structural and reading-invariant: the condemned has no exit, and the arrangement runs on the full coercive apparatus — death-qualified juries, prosecutorial charging discretion, restricted post-conviction review. Theater 0.32: the killing itself is functional; the ceremonial layer (final statements, execution protocols) and the comparative-proportionality review that almost never grants relief are the performative share. Accessibility collapse 0.40: the alternative — life without parole — is fully workable, widely used, and expanding; the constraint does not close alternatives, it is losing to them. Resistance 0.62: organized abolition movements, innocence litigation, religious bodies, and international pressure meet majority-but-declining public support in retentionist jurisdictions. The measurement series run on one shared grid (decade steps across the 60-year interval) with all three metrics authored at every point. The suppression_requirement series is authored because this story genuinely tracks enforcement-capacity change: the mid-interval dip marks the Furman-era lapse, the subsequent rise marks the post-Gregg rebuild and the 1990s hardening (restriction of federal post-conviction review), and the plateau reflects a suppressive legal structure that persists even as execution counts decline.
 *
 * PERSPECTIVAL GAP:
 *   From the state's seat the arrangement is authority vindicating itself: each execution demonstrates that the proportionality norm is real, and the seat computes coordination-with-benefit. From the condemned's seat the same structure is maximal extraction with zero exit. From the victims'-families seat it is a delivered good — vindication — with the cost borne elsewhere. From the wrongly-convicted seat it is pure injustice that the reading's own premise condemns. The engine computes these divergences from the structural data (power, exit, role); the authored claim does not adjudicate them. Note the coarsest seam: execution_personnel and murder_victims_families share the moderate power atom but sit on opposite sides of the transfer, so per-seat directionalities inherit that coarseness.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d: the state (near the beneficiary end — it collects authority and legitimacy), victims' families (low d — they receive vindication and closure), and the posthumous victim (excluded from derivation entirely via agent:false — the dead collect nothing, and the reading's own beneficiary claim for them is symbolic). Victim declarations drive high d: the condemned (near full target — maximal cost, zero exit) and the wrongly convicted (full target — cost with no desert to answer, the component the reading itself counts as extraction). Execution personnel derive as payers with constrained exit — a moderate overestimate of their true position, accepted because directionality overrides key on power atoms and would also displace the victims'-families seat. Abolitionist advocates and international bodies carry no beneficiary/victim declaration and fall to canonical fallback — they contest or observe rather than collect or pay. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled downstream.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both directions. Against mislabeling as pure snare: the coordination function is genuine and ancient — the talionic rule originated as an escalation cap on blood feud, and the vindication demand it answers is sincerely held by the surviving victims it serves; a snare reading would erase the real collective-action problem the arrangement solves. Against mislabeling as rope: the extraction is real and the reading itself concedes it — the wrongly convicted pay with lives they do not owe, the apparatus over-punishes by the reading's own proportionality standard, and the state captures durable authority from each execution. The founding problem is live (murder persists; the demand for a proportional answer persists), so no mandatrophy is declared. The drift risk is toward piton rather than zombie-mandate: as executions become rarer and more ritualized while proportionality review almost never grants relief, the theater_ratio series tracks whether the arrangement is becoming performance around a function it no longer performs at talionic scale — the reference frame says death for death; practice delivers death for a discretion-shaped few.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexing,
    'This constraint is the retributive_desert reading of the state_killing_authority kernel; which structural facts of the standing arrangement would the sibling readings (deterrence_instrument, categorical_abolition) re-author, and how would classification shift under them?',
    'The sibling stories themselves: each sibling authors its own epsilon, beneficiary set, and victim set over the same referent arrangement; cross-reading comparison of the three files locates the disagreement structurally rather than inside this story.',
    'The categorical_abolition reading would move every condemned person into the rights-holder set regardless of guilt (epsilon near maximal); the deterrence_instrument reading would ground beneficiaries in the protected population and author epsilon from failed-deterrence cost. This story''s values are valid only for the retributive seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexing, conceptual, 'Reading-indexing: epsilon, victim sets, and classification are properties of this reading, not of the kernel.').

omega_variable(
    capital_error_rate,
    'What is the true rate of wrongful capital convictions — the extraction component this reading''s own proportionality premise must concede?',
    'Survival-analysis estimates on exoneration data, expanded post-conviction DNA and non-DNA testing, and systematic review of executed-case doubt files.',
    'Reading-indexed epsilon scales with the conceded error component: a rate near zero collapses epsilon toward the apparatus-disproportionality floor; a substantially higher rate pushes the arrangement toward snare (coordination story intact, extraction dominant).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_error_rate, empirical, 'Error rate of capital convictions as the dominant conceded-extraction component.').

omega_variable(
    posthumous_vindication_status,
    'Is the murdered person''s posthumous vindication a benefit anyone actually holds, or a framing that routes the arrangement''s real yield to the living (survivors'' closure, the state''s authority display)?',
    'Moral philosophy on posthumous interests combined with survivor-outcome studies comparing families who witness executions with those who do not.',
    'If vindication is wholly a living-person good, the posthumous beneficiary entry drops out of the structural set and the coordination function re-weights toward survivor and state interests — tightening the measured extraction asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(posthumous_vindication_status, conceptual, 'Whether the posthumous beneficiary seat collects anything real.').

omega_variable(
    selective_application_vs_talionic_frame,
    'The reference frame requires death for death; practice applies death to a small, discretion- and geography-shaped subset of murders. Does selective application violate the reading''s own proportionality premise (self-undermining), or count as permissible administration?',
    'Jurisprudential analysis of the arbitrariness line from Furman through contemporary disparity studies, assessed against the talionic standard the reading itself endorses.',
    'If selection violates the premise, the arrangement fails by its own lights — the proportionality function becomes partly theatrical and classification drifts toward piton or snare; if permissible, practice drift is an administration cost rather than a premise failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_application_vs_talionic_frame, conceptual, 'Whether selective application breaks the reading''s own proportionality premise.').

omega_variable(
    apparatus_disproportionality,
    'Do death-row conditions and multi-decade delay over-punish relative to desert by the reading''s own proportionality standard — a conceded extraction from even the guilty?',
    'Comparative review of death-row duration trends, retributivist scholarship on delay, and execution-method litigation outcomes.',
    'Sets the apparatus component of reading-indexed epsilon independently of the error component; a large apparatus component raises epsilon even under a low error rate and fuels expedited-execution reform pressure from inside the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(apparatus_disproportionality, empirical, 'Conceded over-punishment embedded in the modern death-row apparatus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__retributive_desert, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_authority__retributive_desert, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(stat_tr_t0, observed).
narrative_ontology:measurement(stat_tr_t10, state_killing_authority__retributive_desert, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(stat_tr_t10, observed).
narrative_ontology:measurement(stat_tr_t20, state_killing_authority__retributive_desert, theater_ratio, 20, 0.25).
narrative_ontology:measurement_basis(stat_tr_t20, observed).
narrative_ontology:measurement(stat_tr_t30, state_killing_authority__retributive_desert, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(stat_tr_t30, observed).
narrative_ontology:measurement(stat_tr_t40, state_killing_authority__retributive_desert, theater_ratio, 40, 0.3).
narrative_ontology:measurement_basis(stat_tr_t40, observed).
narrative_ontology:measurement(stat_tr_t50, state_killing_authority__retributive_desert, theater_ratio, 50, 0.31).
narrative_ontology:measurement_basis(stat_tr_t50, observed).
narrative_ontology:measurement(stat_tr_t60, state_killing_authority__retributive_desert, theater_ratio, 60, 0.32).
narrative_ontology:measurement_basis(stat_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_authority__retributive_desert, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(stat_be_t0, observed).
narrative_ontology:measurement(stat_be_t10, state_killing_authority__retributive_desert, base_extractiveness, 10, 0.28).
narrative_ontology:measurement_basis(stat_be_t10, observed).
narrative_ontology:measurement(stat_be_t20, state_killing_authority__retributive_desert, base_extractiveness, 20, 0.32).
narrative_ontology:measurement_basis(stat_be_t20, observed).
narrative_ontology:measurement(stat_be_t30, state_killing_authority__retributive_desert, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(stat_be_t30, observed).
narrative_ontology:measurement(stat_be_t40, state_killing_authority__retributive_desert, base_extractiveness, 40, 0.42).
narrative_ontology:measurement_basis(stat_be_t40, observed).
narrative_ontology:measurement(stat_be_t50, state_killing_authority__retributive_desert, base_extractiveness, 50, 0.44).
narrative_ontology:measurement_basis(stat_be_t50, observed).
narrative_ontology:measurement(stat_be_t60, state_killing_authority__retributive_desert, base_extractiveness, 60, 0.45).
narrative_ontology:measurement_basis(stat_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_authority__retributive_desert, suppression_requirement, 0, 0.6).
narrative_ontology:measurement_basis(stat_su_t0, observed).
narrative_ontology:measurement(stat_su_t10, state_killing_authority__retributive_desert, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(stat_su_t10, observed).
narrative_ontology:measurement(stat_su_t20, state_killing_authority__retributive_desert, suppression_requirement, 20, 0.68).
narrative_ontology:measurement_basis(stat_su_t20, observed).
narrative_ontology:measurement(stat_su_t30, state_killing_authority__retributive_desert, suppression_requirement, 30, 0.78).
narrative_ontology:measurement_basis(stat_su_t30, observed).
narrative_ontology:measurement(stat_su_t40, state_killing_authority__retributive_desert, suppression_requirement, 40, 0.82).
narrative_ontology:measurement_basis(stat_su_t40, observed).
narrative_ontology:measurement(stat_su_t50, state_killing_authority__retributive_desert, suppression_requirement, 50, 0.85).
narrative_ontology:measurement_basis(stat_su_t50, observed).
narrative_ontology:measurement(stat_su_t60, state_killing_authority__retributive_desert, suppression_requirement, 60, 0.85).
narrative_ontology:measurement_basis(stat_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__retributive_desert, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, state_killing_authority__deterrence_instrument).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, state_killing_authority__categorical_abolition).

% DUAL FORMULATION NOTE:
% The colloquial label 'capital punishment' decomposes, per epsilon-invariance, into three readings of one kernel: this file (retributive_desert — authority grounded in forfeiture and proportionality), state_killing_authority__deterrence_instrument (authority grounded in outcome), and state_killing_authority__categorical_abolition (denial of the authority). Same referent arrangement; different epsilon, different victim sets, different classification. The retributive reading is upstream of the deterrence reading historically (desert claims secured retention while deterrence evidence was being assembled) and is the direct logical opposite of the abolition reading. Edges declared accordingly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
