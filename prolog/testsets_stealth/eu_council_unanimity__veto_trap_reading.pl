% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__veto_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__veto_trap_reading, []).

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
 *   constraint_id: eu_council_unanimity__veto_trap_reading
 *   human_readable: EU Council Unanimity — Veto-Trap Reading (Minoritarian Extraction via Credible Blocking)
 *   domain: political/institutional/international-relations
 *
 * SUMMARY:
 *   Within the Council of the European Union, designated policy domains —
 *   foreign policy and sanctions, taxation, treaty revision, own resources,
 *   enlargement steps — require unanimous agreement before collective action
 *   proceeds. This story instantiates the veto-trap reading of that rule:
 *   unanimity operates as a standing vulnerability that any single government
 *   can convert into bargaining leverage. Because a blocking threat is cheap
 *   to issue and costly to test, governments with narrow or domestically
 *   profitable objections obtain concessions, opt-outs, funding carve-outs,
 *   and timing control from otherwise-formed majorities. The transfer is
 *   systematic rather than incidental: as the Union enlarged from twelve to
 *   twenty-seven, heterogeneity grew, potential blockers multiplied, and the
 *   price of unblocking rose. KEY AGENTS (by structural relationship):
 *   blocking_member_states — primary beneficiary (institutional/constrained),
 *   converts withheld consent into opt-outs, carve-outs, and timing control;
 *   coalition_majority_states — primary target (institutional/constrained),
 *   bears the concessions, delays, and dilution; european_commission —
 *   agenda-setter and secondary beneficiary (institutional/identity_locked),
 *   brokers the unlocks and collects brokerage rewards;
 *   aid_and_sanction_target_populations — excluded third party
 *   (powerless/trapped), bears delay costs with no seat;
 *   integration_federalist_movements — excluded reform constituency
 *   (moderate/constrained), locked out of the amendment path;
 *   comparative_political_scientists — analytical observer
 *   (analytical/analytical), sees the full structure. Sibling readings of the
 *   same treaty rule are authored as separate constraint files linked through
 *   the network section; this file carries only the veto-trap instantiation
 *   with its own stable epsilon.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__veto_trap_reading, 0.72).
domain_priors:suppression_score(eu_council_unanimity__veto_trap_reading, 0.62).
domain_priors:theater_ratio(eu_council_unanimity__veto_trap_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__veto_trap_reading, tangled_rope).
narrative_ontology:human_readable(eu_council_unanimity__veto_trap_reading, "EU Council Unanimity — Veto-Trap Reading (Minoritarian Extraction via Credible Blocking)").
narrative_ontology:topic_domain(eu_council_unanimity__veto_trap_reading, "political/institutional/international-relations").

domain_priors:requires_active_enforcement(eu_council_unanimity__veto_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__veto_trap_reading, '05c87021-0540-4e82-baa6-bc2ecaa2061b').
narrative_ontology:cs_kernel_codification('05c87021-0540-4e82-baa6-bc2ecaa2061b', formalized).
narrative_ontology:cs_authority_grounding('05c87021-0540-4e82-baa6-bc2ecaa2061b', extraction).
narrative_ontology:cs_interpretation_layer_present('05c87021-0540-4e82-baa6-bc2ecaa2061b').
narrative_ontology:cs_reading_relation('05c87021-0540-4e82-baa6-bc2ecaa2061b', eu_council_unanimity__sovereignty_guarantor_reading, coexists_with).
narrative_ontology:cs_reading_relation('05c87021-0540-4e82-baa6-bc2ecaa2061b', eu_council_unanimity__diplomatic_capital_reading, influences).
narrative_ontology:cs_axiom('05c87021-0540-4e82-baa6-bc2ecaa2061b', foundational, minority_block_leverage_is_illegitimate_when_self_regarding).
narrative_ontology:cs_axiom_status(minority_block_leverage_is_illegitimate_when_self_regarding, holdable).
narrative_ontology:cs_axiom_grounding('05c87021-0540-4e82-baa6-bc2ecaa2061b', minority_block_leverage_is_illegitimate_when_self_regarding, deontological).
narrative_ontology:cs_axiom('05c87021-0540-4e82-baa6-bc2ecaa2061b', secondary, removing_priced_consent_improves_collective_outcomes).
narrative_ontology:cs_axiom_status(removing_priced_consent_improves_collective_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('05c87021-0540-4e82-baa6-bc2ecaa2061b', removing_priced_consent_improves_collective_outcomes, instrumental).
narrative_ontology:cs_reference_frame('05c87021-0540-4e82-baa6-bc2ecaa2061b', narrow_vital_interest_safeguard_baseline).
narrative_ontology:cs_drift_state('05c87021-0540-4e82-baa6-bc2ecaa2061b', contemporary_enlarged_union_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('05c87021-0540-4e82-baa6-bc2ecaa2061b', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__veto_trap_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__veto_trap_reading, blocking_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__veto_trap_reading, european_commission).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, coalition_majority_states).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, aid_and_sanction_target_populations).
narrative_ontology:constraint_vindicates(eu_council_unanimity__veto_trap_reading, credible_threat_bargaining_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% National governments that withhold consent, or credibly threaten to, in unanimous-domain files. Withholding costs them little: no vote is lost, no office forfeited, and domestic audiences often reward visible defiance of Brussels. What returns to them is concrete: opt-outs written into legal texts, funding carve-outs, delayed or diluted measures they opposed, and elevated standing at home. Leaving the Union altogether is not a realistic option — the withdrawal precedent demonstrated the cost — so their leverage lives entirely inside the room.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, blocking_member_states, beneficiary,
    institutional, biographical, constrained, national).

% Governments holding supermajority preferences for collective action — sanctions packages, support mechanisms, enlargement steps, tax coordination. They cannot outvote a single holdout in these domains. Their realistic choices are delay, dilution, or payment: assembling package deals that route concessions to the holdout, or threatening workarounds such as enhanced cooperation that fragment the Union and carry their own participation thresholds. Their preferred policies arrive late, smaller, and more expensive than the majorities behind them.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, coalition_majority_states, payer,
    institutional, generational, constrained, continental).

% Proposes legislation, chairs much of the negotiation, and brokers the package deals that unlock blocked files. Each crisis it resolves enlarges its brokerage relevance and often its portfolio and budget reach, so it gains tangibly from the dealmaking the deadlock demands. Its self-understanding is bound up with being the indispensable honest broker between capitals, and it rarely champions dismantling the very deadlock machinery that generates demand for its services.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, european_commission, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__veto_trap_reading, european_commission, beneficiary).

% Populations outside the Council — residents of countries awaiting support packages, sanctions-relief sequencing, or accession steps — who bear the costs of months-long delays produced by holdouts negotiating price. They have no seat, no vote, and no channel into the negotiation beyond public appeal; their timelines are settled in bargains struck over their heads.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, aid_and_sanction_target_populations, excluded,
    powerless, immediate, trapped, regional).

% Cross-border movements and political factions pressing for treaty reform toward majority voting in the blocked domains. Their remedy runs through the rule they want to change: treaty revision itself requires unanimous agreement, so every government content with the current leverage holds a permanent gate on the reform path. They organize, publish, and lobby from outside the decision rooms.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, integration_federalist_movements, excluded,
    moderate, civilizational, constrained, continental).

% Researchers counting veto players, coding concession flows, and modeling blocking power across federations and international organizations. They sit outside the bargain, observe the full structure across many cases, and publish the analyses that governments cite selectively.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, comparative_political_scientists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eu_council_unanimity__veto_trap_reading, blocking_member_states).
narrative_ontology:fixing_cost_class(eu_council_unanimity__veto_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of binding sovereign states to collective action without coercion: in designated domains, no member is bound to a course it has not consented to, which keeps all members inside the voluntary association and guarantees each capital a final check on commitments touching its core interests.
% TRANSFER_FUNCTION: Moves concessions — legal opt-outs, budget carve-outs, dilution, and control of timing — from the majority coalition to whichever single government withholds consent; moves delay onto everyone downstream, including populations outside the Council; and moves brokerage rewards, in the form of expanded mediating roles, toward the institutions brokering the unlocks.
% ABSENT_VOICES: Aid and sanction target populations have no seat and would object to their timelines being priced into intergovernmental bargains; pro-reform federalist movements stand outside the treaty-amendment process that could admit them; future majorities are bound by blocks cast under current electoral cycles. None of these seats participates in the unanimity conversation.
% DISAPPEARANCE_RATIONALE: If unanimity in these domains vanished overnight and qualified majority voting applied instead, blocking leverage would evaporate within a legislative cycle: holdouts could no longer price their consent, pending files would pass on majority preference, opt-out and carve-out flows would cease, and demand for brokerage would shrink. Governments currently profiting from withheld consent would lose their principal source of cross-dossier income, and integration in foreign policy and taxation would accelerate.
% FOUNDING_PROBLEM: After the 1965 empty-chair crisis, the Community settled whether majorities could push a member into commitments it deemed vital: the Luxembourg Compromise entrenched a de facto veto wherever a government invoked important national interests. The arrangement was built to make continued membership compatible with sovereignty — to ensure no state could be outvoted into obligations touching its existence.
% FOUNDING_PROBLEM_CORROBORATION: Historians of European integration, working outside any current beneficiary set, document the founding problem precisely: protection against coerced vital commitments in a small community facing existential questions. Whether the problem remains live is disputed along beneficiary lines — member governments casting blocks attest its liveness, while integration scholarship outside the blocking governments' camp largely treats the original problem (coerced war-making, existential imposition) as transformed and the present arrangement as leverage machinery. No disinterested source attests that the founding problem persists in its original form.
narrative_ontology:disappearance_verdict(eu_council_unanimity__veto_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__veto_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__veto_trap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(eu_council_unanimity__veto_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__veto_trap_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__veto_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_council_unanimity__veto_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eu_council_unanimity__veto_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end) because the transfer is decoupled from contribution: the blocker supplies nothing but withheld consent and receives real value — legal opt-outs, money, delay priced to its advantage. Suppression (0.62) is a raw structural property, unscaled by power or scope: the majority has no vote-based remedy in these domains, and the alternatives that exist (enhanced cooperation, constructive abstention, treaty revision) are each gated behind thresholds or unanimities of their own. Theater ratio (0.35) reflects the growing share of activity that is performative — unity communiques drafted while the real deal is cut bilaterally, 'constructive abstention' rituals that record dissent without stopping anything — alongside genuinely functional negotiation. Accessibility collapse is moderate (0.45): alternatives do not vanish once the structure is understood, but every exit is expensive and partly blocked. Resistance (0.62) is real and rising: majorities freeze funds through other legal instruments, litigate conditionality mechanisms, and threaten enhanced-cooperation workarounds. The temporal series run on one shared grid (points 0, 5, 10, 15, 20, 25, 30, mapping approximately to 1993–2023, Maastricht's entry into force to the peak Ukraine-aid veto contestation); every tracked metric is authored at every point, so no end-state value leaks backward into earlier times. Extractiveness climbs as enlargement multiplies potential blockers and high-stakes unanimous dossiers accumulate; suppression_requirement rises as the machinery defending the rule hardens (Lisbon preserving unanimity for taxes and CFSP under pressure, Article 7 processes failing against protected members, conditionality instruments surviving court challenge); theater rises as public consensus performance expands to cover private concession trading.
 *
 * PERSPECTIVAL GAP:
 *   Every member government sits at the same formal power level (institutional) — the divergence is computed from role and exit, not rank. From the blocking seat, the rule is an asset: a machine that converts obstinacy into income, experienced as mild coordination overhead plus windfall gains. From the majority seat, the identical rule is a standing tax: every preferred policy carries a holdup premium, experienced as heavy extraction with no in-room remedy. From the Commission's seat it is both stage and salary: the deadlock manufactures the brokerage demand that constitutes its relevance, so the intermediary experiences the structure as functional even while paying enforcement costs. Same-level actors diverge because the constraint differentiates them by what their consent is worth, not by what their office is.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries drive the derivation: blocking_member_states sit near the full-beneficiary end (the rule subsidizes them directly; their exit is constrained, which pins them inside the arrangement they profit from), and european_commission sits partway down (declared secondary beneficiary — it collects brokerage rewards — but it also bears real administrative costs, damping the subsidy). Declared victims drive the opposite pole: coalition_majority_states sit near the full-target end (they pay the concessions, their exit is constrained, and the continental scope of what they seek amplifies what each block costs them). aid_and_sanction_target_populations are excluded rather than coordinated — they bear costs without entering the derivation as a seated party, which is itself diagnostic of the structure. No directionality overrides are needed: the beneficiary/victim declarations plus exit options already place each seat correctly, and a power-atom-keyed override would wrongly move all three institutional seats together.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim guards against two symmetrical errors. Reading unanimity as pure coordination (rope) would erase the documented concession flows — the opt-outs, carve-outs, and priced delays that recur across dossiers and decades. Reading it as pure extraction (snare) would erase the genuine consent function that keeps twenty-seven sovereigns voluntarily bound and that no majority has dared abolish. The hybrid claim holds both halves in one structure: real coordination, real transfer, active enforcement needed to keep the majority from routing around the blocker. On genealogy: the founding problem (protecting members against coerced vital commitments, fixed after the empty-chair crisis) is contested rather than dead — defenders attest its liveness from inside the beneficiary set, while scholarship outside that set treats it as transformed. The contested status keeps mandatrophy unresolved rather than declaring the arrangement a zombie; the mismatch consumer reads the contested-status-plus-world_rearranges combination against the computed extraction profile.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading of kernel eu_council_unanimity; what structural facts would the sibling readings change?',
    'Cross-file comparison of the sibling stories'' beneficiary/victim declarations and epsilon values against this file''s, over the shared referent of the standing unanimity arrangement.',
    'Under the sovereignty_guarantor_reading the victim set empties and epsilon collapses toward coordination cost (rope-like); under the diplomatic_capital_reading concessions re-read as legitimate deliberation prices and epsilon drops moderately. This file''s high epsilon holds only under the veto-trap instantiation; the readings are different constraints, not different measurements of one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Reading-indexed epsilon over a fixed referent; sibling readings instantiate structurally different constraints from the same treaty rule.').

omega_variable(
    anticipatory_concession_measurement,
    'Is the measured transfer driven by cast vetoes or by anticipatory concessions made under the shadow of a credible threat?',
    'Dossier-level coding comparing formal veto events with pre-vote concession packages (opt-outs, carve-outs, sequencing changes) across a matched sample of unanimous-domain files.',
    'If most transfer is anticipatory, formal-vote statistics understate the flow and the correct observable is negotiated-text deltas; thresholds calibrated on veto counts would read the arrangement as milder than it operates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anticipatory_concession_measurement, empirical, 'Whether extraction occurs at the vote or in the shadow preceding it.').

omega_variable(
    vital_interest_or_leverage,
    'When a government blocks citing national interest, is the block protecting a genuine vital interest or manufacturing bargaining leverage?',
    'Revealed-preference analysis across dossiers and years: blocks abandoned upon side-payment indicate leverage; blocks held irrespective of offers indicate conviction.',
    'If most blocks are leverage-priced, the transfer is systematic and this reading''s high epsilon is confirmed; if most are conviction-held, part of the measured cost belongs on the sovereignty reading''s ledger as legitimate interest protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vital_interest_or_leverage, empirical, 'Authenticity of the invoked national interest behind blocking episodes.').

omega_variable(
    qmv_counterfactual_valuation,
    'Would majority-preferred outcomes under qualified majority voting be better, and better by whose valuation?',
    'Not resolvable by data alone: requires weighting minority-insulation losses against majority-efficiency gains — a values question about what decision rules owe to outvoted minorities.',
    'Valuing minority insulation keeps part of the arrangement''s cost on the coordination side of the ledger; valuing majority efficacy shifts it to the extraction side. The boundary between rope and tangled_rope for this kernel depends on this weighting.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(qmv_counterfactual_valuation, preference, 'Counterfactual valuation of majority rule versus unanimity in the blocked domains.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__veto_trap_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t0, eu_council_unanimity__veto_trap_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(eu_c_tr_t0, observed).
narrative_ontology:measurement(eu_c_tr_t5, eu_council_unanimity__veto_trap_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement_basis(eu_c_tr_t5, observed).
narrative_ontology:measurement(eu_c_tr_t10, eu_council_unanimity__veto_trap_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement_basis(eu_c_tr_t10, observed).
narrative_ontology:measurement(eu_c_tr_t15, eu_council_unanimity__veto_trap_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement_basis(eu_c_tr_t15, observed).
narrative_ontology:measurement(eu_c_tr_t20, eu_council_unanimity__veto_trap_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement_basis(eu_c_tr_t20, observed).
narrative_ontology:measurement(eu_c_tr_t25, eu_council_unanimity__veto_trap_reading, theater_ratio, 25, 0.33).
narrative_ontology:measurement_basis(eu_c_tr_t25, observed).
narrative_ontology:measurement(eu_c_tr_t30, eu_council_unanimity__veto_trap_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement_basis(eu_c_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t0, eu_council_unanimity__veto_trap_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(eu_c_be_t0, observed).
narrative_ontology:measurement(eu_c_be_t5, eu_council_unanimity__veto_trap_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement_basis(eu_c_be_t5, observed).
narrative_ontology:measurement(eu_c_be_t10, eu_council_unanimity__veto_trap_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(eu_c_be_t10, observed).
narrative_ontology:measurement(eu_c_be_t15, eu_council_unanimity__veto_trap_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement_basis(eu_c_be_t15, observed).
narrative_ontology:measurement(eu_c_be_t20, eu_council_unanimity__veto_trap_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement_basis(eu_c_be_t20, observed).
narrative_ontology:measurement(eu_c_be_t25, eu_council_unanimity__veto_trap_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(eu_c_be_t25, observed).
narrative_ontology:measurement(eu_c_be_t30, eu_council_unanimity__veto_trap_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement_basis(eu_c_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(eu_c_su_t0, eu_council_unanimity__veto_trap_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(eu_c_su_t0, observed).
narrative_ontology:measurement(eu_c_su_t5, eu_council_unanimity__veto_trap_reading, suppression_requirement, 5, 0.44).
narrative_ontology:measurement_basis(eu_c_su_t5, observed).
narrative_ontology:measurement(eu_c_su_t10, eu_council_unanimity__veto_trap_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(eu_c_su_t10, observed).
narrative_ontology:measurement(eu_c_su_t15, eu_council_unanimity__veto_trap_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement_basis(eu_c_su_t15, observed).
narrative_ontology:measurement(eu_c_su_t20, eu_council_unanimity__veto_trap_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement_basis(eu_c_su_t20, observed).
narrative_ontology:measurement(eu_c_su_t25, eu_council_unanimity__veto_trap_reading, suppression_requirement, 25, 0.59).
narrative_ontology:measurement_basis(eu_c_su_t25, observed).
narrative_ontology:measurement(eu_c_su_t30, eu_council_unanimity__veto_trap_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(eu_c_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__veto_trap_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_council_unanimity__sovereignty_guarantor_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_council_unanimity__diplomatic_capital_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'EU Council unanimity' covers multiple structurally distinct claims about one treaty rule. Per the epsilon-invariance principle the label decomposes into a constraint family: this file authors the veto-trap reading (high epsilon, named victims, blocking as transfer mechanism); the sovereignty-guarantor file authors the same referent as consent protection (negligible extraction, no victims); the diplomatic-capital file authors it as a deliberation discipline (moderate epsilon, diffuse costs). Citation runs upstream from the sovereignty defense (invoked to justify the rule) into this diagnosis (mounted against it), so this file links to both siblings. Each file carries its own epsilon, stakeholders, and classification; none averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
