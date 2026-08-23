% ============================================================================
% CONSTRAINT STORY: state_execution_authority__retributive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: state_execution_authority__retributive_reading
 *   human_readable: State Execution Authority — Retributive Reading: Execution as Restoration of Moral Balance Through Proportionate Punishment
 *   domain: criminal justice/political philosophy/constitutional law
 *
 * SUMMARY:
 *   Retentionist jurisdictions kill convicted murderers as an act of moral
 *   bookkeeping: the retributive claim that proportionate punishment restores
 *   a balance broken by the crime. This file instantiates ONE reading of the
 *   state_execution_authority kernel and hedges nothing across readings.
 *   Family decomposition (epsilon-invariance): the colloquial label 'the
 *   death penalty debate' splits into three structurally distinct constraints
 *   sharing statutes and condemned bodies but differing in epsilon and
 *   ledgers — abolition_reading (rights-violation referent; the executed
 *   offender anchors ITS victim ledger), deterrence_reading (consequence
 *   referent; epsilon tracks killings prevented), and this
 *   retributive_reading (desert-ledger referent). Epsilon here is authored
 *   for the STANDING execution arrangement as practiced in retaining
 *   jurisdictions, assessed by the reading's own proportionate-desert
 *   standard — never for the abolition alternative. By that own-lights
 *   standard the arrangement scores high: identifying desert is fallible
 *   while the sanction is irreversible; application is arbitrary across
 *   counties, victim race, and counsel quality; only a sliver of statutorily
 *   eligible cases reaches the chamber, making the 'restored balance' a
 *   lottery; and the punishment arrives wrapped in decades of death-row limbo
 *   that no proportionality account priced. The claim/metric split is
 *   deliberate and unreconciled: claimed_type is authored from structure
 *   (genuine vengeance-channeling function + asymmetric cost-bearing +
 *   continuously administered coercion), the metrics from the operating
 *   record; per-seat engine computations may diverge from the claim, and that
 *   divergence is the datum. KEY AGENTS (by structural relationship): -
 *   condemned_capital_offenders: primary cost-bearing seat
 *   (powerless/trapped) — bears the arrangement's terminal outcome; this
 *   reading books that bearing as desert collected rather than as harm
 *   inflicted, and therefore does NOT place it in the victim ledger -
 *   wrongfully_executed_capital_defendants: error-bearing seat
 *   (powerless/trapped) — bears the ultimate cost without desert; the
 *   reading's tolerated tragic residue, and the only condemned class in this
 *   reading's victim ledger - executed_inmates_family_members: collateral
 *   cost-bearing seat (powerless/constrained) — pure loss with no desert
 *   question attaching to them; acknowledged as regrettable by every reading
 *   - murder_victim_survivor_families: primary beneficiary seat
 *   (organized/constrained) — receive the arrangement's core product,
 *   restored moral balance - capital_case_prosecutors: operational
 *   agenda-setter with secondary collection (institutional/mobile) -
 *   retentionist_legislatures: statutory agenda-setter with secondary
 *   collection (institutional/constrained) - execution_administration_staff:
 *   enforcing administrators (institutional/identity_locked) with documented
 *   secondary cost-bearing - taxpayers_of_retaining_jurisdictions: diffuse
 *   premium-funders (organized/constrained) - abolitionist_advocacy_networks:
 *   excluded voice (organized/mobile/global) -
 *   international_human_rights_bodies: analytical observer
 *   (institutional/analytical/global)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__retributive_reading, 0.76).
domain_priors:suppression_score(state_execution_authority__retributive_reading, 0.71).
domain_priors:theater_ratio(state_execution_authority__retributive_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, accessibility_collapse, 0.56).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, resistance, 0.66).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__retributive_reading, tangled_rope).
narrative_ontology:human_readable(state_execution_authority__retributive_reading, "State Execution Authority — Retributive Reading: Execution as Restoration of Moral Balance Through Proportionate Punishment").
narrative_ontology:topic_domain(state_execution_authority__retributive_reading, "criminal justice/political philosophy/constitutional law").

domain_priors:requires_active_enforcement(state_execution_authority__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__retributive_reading, 'e178f50e-ac92-41e0-880e-dce164ff8bda').
narrative_ontology:cs_kernel_codification('e178f50e-ac92-41e0-880e-dce164ff8bda', formalized).
narrative_ontology:cs_authority_grounding('e178f50e-ac92-41e0-880e-dce164ff8bda', lineage).
narrative_ontology:cs_interpretation_layer_present('e178f50e-ac92-41e0-880e-dce164ff8bda').
narrative_ontology:cs_reading_relation('e178f50e-ac92-41e0-880e-dce164ff8bda', state_execution_authority__deterrence_reading, coexists_with).
narrative_ontology:cs_reading_relation('e178f50e-ac92-41e0-880e-dce164ff8bda', state_execution_authority__abolition_reading, forecloses).
narrative_ontology:cs_axiom('e178f50e-ac92-41e0-880e-dce164ff8bda', foundational, proportionate_execution_restores_moral_balance).
narrative_ontology:cs_axiom_status(proportionate_execution_restores_moral_balance, holdable).
narrative_ontology:cs_axiom_grounding('e178f50e-ac92-41e0-880e-dce164ff8bda', proportionate_execution_restores_moral_balance, deontological).
narrative_ontology:cs_axiom('e178f50e-ac92-41e0-880e-dce164ff8bda', foundational, desert_not_utility_governs_capital_sentencing).
narrative_ontology:cs_axiom_status(desert_not_utility_governs_capital_sentencing, holdable).
narrative_ontology:cs_axiom_grounding('e178f50e-ac92-41e0-880e-dce164ff8bda', desert_not_utility_governs_capital_sentencing, deontological).
narrative_ontology:cs_axiom('e178f50e-ac92-41e0-880e-dce164ff8bda', secondary, wrongful_execution_tragedy_without_refutation).
narrative_ontology:cs_axiom_status(wrongful_execution_tragedy_without_refutation, holdable).
narrative_ontology:cs_axiom_grounding('e178f50e-ac92-41e0-880e-dce164ff8bda', wrongful_execution_tragedy_without_refutation, deontological).
narrative_ontology:cs_reference_frame('e178f50e-ac92-41e0-880e-dce164ff8bda', talionic_desert_proportionality).
narrative_ontology:cs_drift_state('e178f50e-ac92-41e0-880e-dce164ff8bda', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e178f50e-ac92-41e0-880e-dce164ff8bda', '').
narrative_ontology:cs_kernel_id(state_execution_authority__retributive_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, murder_victim_survivor_families).
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, capital_case_prosecutors).
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, retentionist_legislatures).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, wrongfully_executed_capital_defendants).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, executed_inmates_family_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, condemned_capital_offenders).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, execution_administration_staff).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, taxpayers_of_retaining_jurisdictions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convicted of aggravated homicide and sentenced to death under a capital statute. Spends years to decades on death row through mandatory appeals; where executive clemency is denied, is put to death by the state on a scheduled night. Cannot buy, waive, transfer, or resign the sentence; the only exits are reversal on appeal, commutation, or dying of another cause first. Bears the arrangement's terminal outcome as its designed product.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, condemned_capital_offenders, payer,
    powerless, immediate, trapped, national).

% Convicted and death-sentenced on flawed evidence, ineffective defense, prosecutorial misconduct, or mistaken identification later exposed; some are exonerated only after the execution, others after decades of appeals. Bears the same terminal cost as the guilty while owing none of the desert the sentencing rationale presupposes. Posthumous pardons and DNA exonerations, when they arrive, arrive too late to matter to the person.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, wrongfully_executed_capital_defendants, payer,
    powerless, biographical, trapped, national).

% Lose a parent, child, or sibling to the execution chamber; carry grief and the stigma of association in communities that read the execution as communal justice. Hold no formal standing in clemency decisions, witness protocols, or disposition of the body beyond minimal notification rules; in some jurisdictions are billed for portions of incarceration or process costs.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, executed_inmates_family_members, payer,
    powerless, biographical, constrained, national).

% Lose kin to homicide and are offered the killer's execution as vindication and completion of the moral account. Testimony from this seat drives charging decisions and sways clemency hearings; advocacy organizations on opposing sides both organize from it. Many report the anticipated relief arriving weaker than promised or not at all; others report durable peace after the execution. Disengaging means abandoning their own case unresolved.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, murder_victim_survivor_families, beneficiary,
    organized, biographical, constrained, national).

% Decide which homicides to charge capitally, negotiate plea agreements priced against the possibility of a death sentence, and litigate the appeals that follow. Collect plea leverage, conviction records, and career advancement from running the process; a capital case is a career-making assignment in many offices. Can exit into defense work, politics, or academia with no personal exposure to the sanction itself.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, capital_case_prosecutors, agenda_setter,
    institutional, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(state_execution_authority__retributive_reading, capital_case_prosecutors, beneficiary).

% Enact and maintain the statutes designating capital crimes, fund the appellate and execution apparatus, and respond to wrongful-execution scandals with procedural patching more often than repeal. Harvest tough-on-crime positioning and displays of sovereign resolve from keeping the statutes; repeal exposes members to primary challengers running soft-on-crime campaigns. Electoral accountability holds the stance even where personal conviction wavers.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, retentionist_legislatures, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_execution_authority__retributive_reading, retentionist_legislatures, beneficiary).

% Wardens, death-row officers, chaplains, and execution teams who house the condemned for years and carry out the killing on schedule. Professional identity fuses with faithful execution of court orders; declining an assignment carries career and colleague-sanction costs, and several staff histories record conversion to outspoken opposition after participating. Reported operational trauma runs well above comparison units in the same departments.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, execution_administration_staff, agenda_setter,
    institutional, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(state_execution_authority__retributive_reading, execution_administration_staff, payer).

% Fund the premium: a capital case from charging through appeals to execution costs substantially more than lifetime imprisonment across repeated studies. Pay through general revenue with no direct control over case selection; the available lever is voting, exercised on schedules and agendas set by others.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, taxpayers_of_retaining_jurisdictions, payer,
    organized, biographical, constrained, national).

% Campaign for repeal through litigation, referendum, legislation, and international pressure; maintain exoneree registries and wrongful-execution archives that supply the evidentiary record cited against the statutes. Speak publicly at every hearing yet hold no seat in the rooms where individual charges, clemency petitions, and warrants are decided. Donation-funded; able to redirect effort across jurisdictions at will.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, abolitionist_advocacy_networks, excluded,
    organized, generational, mobile, global).

% Monitor, report, and pass resolutions against the practice; compile comparative statistics and review treaty compliance. Hold analytic distance — operating and financing no part of the machinery — while their findings feed domestic litigation and diplomatic pressure.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_execution_authority__retributive_reading, murder_victim_survivor_families).
narrative_ontology:fixing_cost_class(state_execution_authority__retributive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels the community's demand for ultimate sanction against heinous killers into a single public procedure administered by the state, replacing private vengeance and lynch law with controlled retribution; marks and enforces the boundary of conduct the political community refuses to tolerate.
% TRANSFER_FUNCTION: Moves the ultimate cost — life itself — from the convicted offender (and, through error, from the wrongfully convicted) to satisfy the community's desert ledger; delivers vindication and promised closure to surviving families, plea leverage and career capital to prosecutors, durable issue ownership to legislators, budgets and staffing to the penal apparatus; financed by taxpayers at a large premium over life imprisonment.
% ABSENT_VOICES: The condemned speak only through counsel until appeals end, then not at all; families of the executed hold no standing in clemency or protocol decisions; abolitionist advocates and international monitors testify publicly but are absent from the operative seats — charging conferences, jury rooms, clemency boards, warrant signings; future generations who inherit whatever precedent the practice sets have no representative at all.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, pending death sentences convert to life terms, death-row facilities empty, prosecutors lose their strongest plea-pricing instrument, legislative coalitions lose a signature crime issue, and survivor families who anchored recovery to a coming execution lose the promised event and must rebuild meaning around permanent imprisonment. Jurisdictions that abolished demonstrate the substitute equilibrium is stable — the rearrangement is real and extensive but survivable, which is precisely what distinguishes this constructed arrangement from a natural limit.
% FOUNDING_PROBLEM: Private vengeance and retaliatory feuding after homicide threatened escalating cycles of killing; early states centralized the blood-price into public execution both to stop feud spirals and to demonstrate that the sovereign, not the aggrieved kin, held the sword.
% FOUNDING_PROBLEM_CORROBORATION: Criminal-law historians corroborate the feud-channeling genealogy from outside the benefiting parties; comparative criminology and official abolition commissions cite the absence of feud resurgence in abolitionist jurisdictions to argue the channeling function survives without execution; survivor-family organizations on both sides dispute the closure premise from within their own constituency. No source outside the beneficiary set attests that restored moral balance specifically requires death rather than permanent incapacitation.
narrative_ontology:disappearance_verdict(state_execution_authority__retributive_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__retributive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__retributive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_execution_authority__retributive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__retributive_reading, 0.76, 'stealth/ox-alpha', 'none', direct).

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
 *   Why tangled_rope as the structural claim: all three defining features hold simultaneously. A genuine coordination function exists (the vengeance-channeling settlement recorded in the founding problem — it predates and would outlast any particular moral theory); the cost-bearing is sharply asymmetric (two declared victim classes against concentrated beneficiary seats); and the arrangement requires continuous active enforcement (charging decisions, appellate litigation, clemency administration, the chamber itself). Pure coordination framing would erase the wrongfully executed; pure cover-story framing would erase a channeling function that imprisonment also performs — which is exactly the open dispute, recorded rather than resolved. Metrics, authored independently: extractiveness 0.76 is derived from the reading's OWN proportionality standard being violated four ways — (i) fallible desert identification meets irreversible sanction, with the framework absorbing documented wrongful executions as tolerable error instead of correcting the requirement; (ii) systematic arbitrariness across geography, victim race, and defense quality breaks the proportionality the justification promises; (iii) extreme selectivity turns the moral ledger into a lottery; (iv) delivery corruption — decade-plus death-row limbo — wraps the sanction in torment the desert account never authorized. Suppression 0.71 is raw and intentionally unscaled (only extractiveness is scaled by directionality and scope): it combines statutory compulsion with the doctrinal disqualification of the imprisonment substitute; roughly sixty percent structural, forty percent internalized public belief that a life term betrays the victim. Theater_ratio 0.42: the killing is really performed, but a growing share of activity stages diligence — solemnity scripts, secrecy statutes framed as dignity, last-meal protocols — over an allocation the record shows to be arbitrary. Accessibility_collapse 0.56: life-without-parole demonstrably sustains order in abolitionist jurisdictions, so alternatives persist in fact while being ruled inadequate in doctrine — partial collapse only. Resistance 0.66: sustained abolition campaigning, exoneree testimony, moratoria, and international pressure, met chiefly with procedural patching. Identity-lock note: execution_administration_staff bind through institutional identity fusion — the sentence-carried-out IS the professional self; if that frame breaks (staff refusal movements, as in historical volunteer-shortage episodes), enforcement capacity decays faster than statute can repair. Temporal series run on ONE shared grid (points 0,10,20,30,40,50) with every tracked metric authored at every point; suppression_requirement is tracked because the story specifically traces enforcement-capacity change: post-reinstatement build-up, appellate-review hardening mid-interval, then drug-shortage disruption and spreading moratoria producing mild late decay.
 *
 * PERSPECTIVAL GAP:
 *   Per-seat divergence is structural, not rhetorical. From the condemned seat the arrangement presents as total and compulsory cost-bearing with no exit — maximal severity by any computation. From the prosecutor seat it presents as administered justice with personal upside: leverage, record, advancement, and a mobile exit the condemned lacks. From the survivor seat it presents as restoration owed and (sometimes) received. The wrongfully executed seat exposes the framework's internal contradiction: its outputs breach its own proportionality standard, yet the framework self-certifies by absorbing the breaches as 'tragic error' — the seat where the reading's justification strains hardest. Inter-institutional dynamics at equal nominal standing: prosecutors and legislatures hold the same institutional power atom but different exits — prosecutorial careers are portable across roles (mobile), while legislators are electorally locked into the stance they took (constrained) — a constraint-specific factor, not a general power difference, that separates their directionalities. Same-level lateral divergence: two capital-eligible defendants in neighboring counties face different charging customs, counsel markets, and appellate cultures — the arbitrariness is experienced precisely BETWEEN equals. Coalition note: the powerless seats have historically combined — opposed-survivor organizations, exoneree networks, and former staff — and that coalition, not any single seat, drove the moratoria visible in the late-interval suppression decay.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries derive low d: murder_victim_survivor_families receive the arrangement's product directly; prosecutors and legislatures collect instrumental returns as agenda-setters, with the prosecutors' mobile exit pulling them further toward the beneficiary pole than the electorally locked legislatures. Declared victims derive near-full-target d, pinned by trapped or constrained exits: wrongfully_executed_capital_defendants (trapped — the sanction follows regardless of jurisdiction) and executed_inmates_family_members (constrained — no standing, no compensation). The deliberate ledger choice: condemned_capital_offenders appear as stakeholders bearing the terminal cost (role payer) but are NOT placed in base_properties.victims[], because within this reading's lights their bearing of cost is adjudicated desert — the arrangement functioning as designed, not extracting from the undeserving. This is the expected structural delta against the sibling files: the abolition_reading places these same bodies at the head of ITS victim ledger. Nothing about the atoms hides the seat — the condemned carry powerless/trapped, so the engine still computes their per-seat verdict near the full-target end from power and exit alone; the displacement of the ledger ENTRY between sibling files is the family's sharpest contrast, and it belongs in the files, not averaged away. Taxpayers sit moderately targeted (diffuse premium, no standing). Excluded and observer seats sit outside the ledger by construction. No directionality_overrides are used: the derivation chain already separates every seat, and the one ambiguity (an undeclared cost-bearing class) resolves through exit modulation rather than override.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification work here is boundary-keeping in both directions. Reading the arrangement as pure coordination would erase the wrongfully executed and the families of the executed — classes the record forces into view. Reading it as pure extraction-with-cover would erase a real channeling function that predates every modern moral theory attached to it; the honest location of that function's continuing necessity is the contested founding-problem status, corroborated externally, not a verdict smuggled into the type. No sunset clause is declared and none could honestly be: the moral-restoration justification is steady-state by design (desert claims do not expire), so the scaffold shape — a transitional arrangement that retires when its work is done — is structurally unavailable. The consequence cuts against the reading: if the desert-identifiability premise fails (see omega desert_identifiability_under_fallibility), the correct structural motion is not graceful retirement but convergence toward the abolition sibling file, where the executed offender enters the victim ledger and epsilon re-references onto the rights violation. The R5 interview records the obsolescence question without forcing it: founding problem partially superseded (imprisonment also channels vengeance), status contested, disappearance verdict world_rearranges — the mismatch consumer sees contested-x-rearranges, correctly firing no zombie flag, because the machinery demonstrably still reorganizes the world around it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This file instantiates only the retributive reading of the state_execution_authority kernel; deterrence_reading and abolition_reading instantiate rival constraints over the same statutes, chambers, and condemned bodies. Which structural elements carry the disagreement, and what would collapse if this reading''s premises fell?',
    'Family-level comparison across the three linked files: align victim ledgers, beneficiary ledgers, epsilon values, and computed per-seat types; the disagreement localizes to (a) the executed offender''s ledger placement, (b) survivor families'' beneficiary status, (c) the assessment standard applied to the shared referent.',
    'If the proportionate-desert premise falls, this reading''s structure converges toward the abolition file — the offender enters the victim ledger, epsilon re-references onto rights violation, and the family reduces toward two live readings. If the premise holds, the family remains tripartite with permanently divergent ledgers over identical bodies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer-frame uncertainty: which sibling reading, if any, displaces this one, and where the contest structurally lives.').

omega_variable(
    desert_identifiability_under_fallibility,
    'Can any real-world charging, trial, and appellate procedure identify desert accurately enough that irreversible proportionate collection is possible — given documented posthumous exonerations, DNA reversals of death sentences, and systematic race-of-victim and county-level disparity findings?',
    'Cumulative exoneration rates among the death-sentenced population, matched-panel disparity studies, and audits of charging discretion across jurisdictions; a bounded error rate inside the framework''s own tolerance would stabilize this reading, an unbounded one dissolves the proportionality premise it runs on.',
    'If identifiability fails beyond the tolerance the reading itself asserts (''tragic error''), the moral-restoration requirement operates as an error-amplifier — extraction toward the wrongfully-executed seat compounds, and this reading''s computed classification drifts from a hybrid coordination shape toward pure extraction for that seat, strengthening the abolition sibling''s hand in the family contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(desert_identifiability_under_fallibility, empirical, 'Whether the proportionality premise survives contact with the procedure''s demonstrated error rate.').

omega_variable(
    survivor_closure_authenticity,
    'Does execution actually deliver the restored moral balance that surviving families are promised, or does the promise routinely defer, disappoint, or complicate grief?',
    'Longitudinal studies of survivors before and after executions, independent of advocacy sponsorship on either side; compare against matched survivors in life-sentence cases.',
    'If closure mostly fails to materialize, the primary beneficiary seat''s benefit collapses, the coordination-function claim thins toward cover story, and this file''s classification shifts toward the abolition sibling''s structure; if it holds for a substantial fraction, the beneficiary ledger stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survivor_closure_authenticity, empirical, 'Authenticity of the moral-balance product delivered to the beneficiary seat.').

omega_variable(
    substitution_block_mechanism,
    'Is the impossibility of substituting life imprisonment for execution structural (statutes, sentencing law, clemency architecture) or internalized (public belief that a life term betrays the victim and that only death completes the account)?',
    'Statutory and clemency-rule analysis crossed with opinion surveys that isolate the belief that execution specifically — not permanent incapacitation — is what justice requires.',
    'If the block is predominantly internalized, repeal becomes procedurally cheap and the prohibitive fixing-cost judgment revises downward; if predominantly structural, only statute-by-statute dismantling moves the arrangement, and the measured suppression is mostly external barrier rather than belief.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_block_mechanism, empirical, 'Structural versus internalized composition of the substitute-blocking force behind the measured suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__retributive_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__retributive_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(stat_tr_t10, state_execution_authority__retributive_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(stat_tr_t20, state_execution_authority__retributive_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(stat_tr_t30, state_execution_authority__retributive_reading, theater_ratio, 30, 0.36).
narrative_ontology:measurement(stat_tr_t40, state_execution_authority__retributive_reading, theater_ratio, 40, 0.39).
narrative_ontology:measurement(stat_tr_t50, state_execution_authority__retributive_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__retributive_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(stat_be_t10, state_execution_authority__retributive_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(stat_be_t20, state_execution_authority__retributive_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(stat_be_t30, state_execution_authority__retributive_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement(stat_be_t40, state_execution_authority__retributive_reading, base_extractiveness, 40, 0.74).
narrative_ontology:measurement(stat_be_t50, state_execution_authority__retributive_reading, base_extractiveness, 50, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__retributive_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(stat_su_t10, state_execution_authority__retributive_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(stat_su_t20, state_execution_authority__retributive_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(stat_su_t30, state_execution_authority__retributive_reading, suppression_requirement, 30, 0.74).
narrative_ontology:measurement(stat_su_t40, state_execution_authority__retributive_reading, suppression_requirement, 40, 0.73).
narrative_ontology:measurement(stat_su_t50, state_execution_authority__retributive_reading, suppression_requirement, 50, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__retributive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_execution_authority__retributive_reading, deterrence_reading).
narrative_ontology:affects_constraint(state_execution_authority__retributive_reading, abolition_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the colloquial label 'capital punishment' (kernel: state_execution_authority). The label conflates three epsilon-invariant constraints: abolition_reading (rights referent; executed offender heads the victim ledger; categorical-impermissibility axiom), deterrence_reading (consequence referent; epsilon tracks killings prevented; utility-governs axiom), and this retributive_reading (desert referent; survivor families enter the beneficiary ledger, the guilty executed are booked as adjudicated desert rather than victims, epsilon is high from the moral-restoration requirement, and wrongful execution is absorbed as tolerable error). Upstream/downstream within the family: this reading's desert-governance axiom is frequently cited alongside deterrence claims in retentionist defense, so this file influences the deterrence sibling's operating environment; the abolition sibling stands in logical contradiction to this file's foundational axiom. Each member links the others via affects_constraints per the family rule; epsilon values are authored independently per file and never reconciled.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
