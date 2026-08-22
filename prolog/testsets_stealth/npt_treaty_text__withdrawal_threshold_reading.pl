% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__withdrawal_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_text__withdrawal_threshold_reading, []).

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
 *   constraint_id: npt_treaty_text__withdrawal_threshold_reading
 *   human_readable: NPT Article X Withdrawal Threshold — Ambiguous Self-Judged Exit Pathway
 *   domain: international_law/arms_control/treaty_interpretation
 *
 * SUMMARY:
 *   The Non-Proliferation Treaty's Article X grants every party the right to
 *   withdraw on three months' notice if 'extraordinary events... have
 *   jeopardized the supreme interests of its country' — a threshold the text
 *   makes self-judging. This story instantiates ONE reading of that kernel:
 *   the withdrawal-threshold reading, under which the operative arrangement
 *   is the ambiguous pathway itself — neither a codified high threshold
 *   (regime stability priority) nor an unrestricted sovereign right
 *   (sovereignty preservation priority), but a politically managed
 *   indeterminacy. The North Korean precedent (1993 announcement, 2003
 *   effective exit, Security Council response that censured without ever
 *   ruling on validity) demonstrated that the pathway exists, is usable, and
 *   carries no authoritative legal determination — which is precisely what
 *   gives threshold states (Iran explicitly, Japan and South Korea
 *   implicitly, Saudi Arabia and Egypt in review-conference signaling) a
 *   credible exit option to hold as leverage. Per Rule 1, the sibling
 *   readings (nws_reading: the text as binding constraint on NNWS;
 *   nnws_reading: disarmament as binding Article VI obligation) are separate
 *   constraints in separate files; this file does not average over them or
 *   hedge epsilon across them. Assumptions: agents are authored as classes
 *   (threshold_states spans Iran, Japan, South Korea, Saudi Arabia, Egypt);
 *   the interval maps t0=1970 (NPT entry into force), t23=1993 (DPRK
 *   withdrawal announcement), t33=2003 (effective DPRK exit), t55=2025. The
 *   claim/metric relationship is deliberate: the arrangement is CLAIMED as
 *   tangled_rope and the authored metrics describe a genuine coordination
 *   function (the accession bargain) carrying asymmetric position (free
 *   option value collected by threshold states, maintenance and deterrent
 *   costs paid by stayers and exercisers) — the engine computes per-seat
 *   divergence from the structural data; the claim is not tuned to any
 *   predicted output.
 *
 * KEY AGENTS:
 *   - threshold_states: primary beneficiary (moderate/arbitrage) — holds the exit option as a priced hedge; collects option value without paying for the pathway's maintenance
 *   - nuclear_weapon_states_p5: agenda_setter and secondary beneficiary (institutional/arbitrage) — administers the regime and manages withdrawal episodes through Security Council practice without codifying either reading
 *   - compliant_nnws: primary payer (organized/constrained) — pays verification, latency, and diplomatic-maintenance costs while holding an exit option they cannot afford to exercise
 *   - withdrawing_states: payer (powerless/trapped) — exercised the self-judged right (DPRK paradigm) and absorbed the deterrent costs that keep exercise from becoming cheap
 *   - iaea_safeguards_system: payer (moderate/constrained) — absorbs verification-continuity losses and enforcement planning with no seat in the threshold debate
 *   - nonparty_nuclear_states: excluded (powerful/arbitrage) — India, Pakistan, Israel bear cascade exposure to withdrawal precedent with no seat in the process
 *   - international_law_community: observer (analytical/analytical) — sees the full text-precedent-practice structure; produces the doctrinal analyses both camps cite
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__withdrawal_threshold_reading, 0.58).
domain_priors:suppression_score(npt_treaty_text__withdrawal_threshold_reading, 0.6).
domain_priors:theater_ratio(npt_treaty_text__withdrawal_threshold_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__withdrawal_threshold_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__withdrawal_threshold_reading, "NPT Article X Withdrawal Threshold — Ambiguous Self-Judged Exit Pathway").
narrative_ontology:topic_domain(npt_treaty_text__withdrawal_threshold_reading, "international_law/arms_control/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_treaty_text__withdrawal_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__withdrawal_threshold_reading, '08a7ef21-16fc-444d-892a-e374bbcef555').
narrative_ontology:cs_kernel_codification('08a7ef21-16fc-444d-892a-e374bbcef555', fixed_text).
narrative_ontology:cs_authority_grounding('08a7ef21-16fc-444d-892a-e374bbcef555', distributed).
narrative_ontology:cs_reading_relation('08a7ef21-16fc-444d-892a-e374bbcef555', npt_treaty_text__nws_reading, coexists_with).
narrative_ontology:cs_reading_relation('08a7ef21-16fc-444d-892a-e374bbcef555', npt_treaty_text__nnws_reading, influences).
narrative_ontology:cs_axiom('08a7ef21-16fc-444d-892a-e374bbcef555', foundational, withdrawal_right_is_self_judged).
narrative_ontology:cs_axiom_status(withdrawal_right_is_self_judged, holdable).
narrative_ontology:cs_axiom_grounding('08a7ef21-16fc-444d-892a-e374bbcef555', withdrawal_right_is_self_judged, conventional).
narrative_ontology:cs_axiom('08a7ef21-16fc-444d-892a-e374bbcef555', foundational, exit_option_constitutes_accession_bargain).
narrative_ontology:cs_axiom_status(exit_option_constitutes_accession_bargain, holdable).
narrative_ontology:cs_axiom_grounding('08a7ef21-16fc-444d-892a-e374bbcef555', exit_option_constitutes_accession_bargain, instrumental).
narrative_ontology:cs_reference_frame('08a7ef21-16fc-444d-892a-e374bbcef555', self_judged_withdrawal_bargain).
narrative_ontology:cs_drift_state('08a7ef21-16fc-444d-892a-e374bbcef555', post_dprk_precedent_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('08a7ef21-16fc-444d-892a-e374bbcef555', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, threshold_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, nuclear_weapon_states_p5).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, compliant_nnws).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, withdrawing_states).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, iaea_safeguards_system).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, compliant_nnws).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Iran, Japan, South Korea, Saudi Arabia, Egypt and comparable parties hold the Article X exit right as a live hedge. Iran has invoked withdrawal explicitly under external pressure; Japan and South Korea maintain the option implicitly as a hedge against regional proliferation; Saudi Arabia and Egypt signal it in review-conference statements. What flows to them is the option's credibility — that exit remains available on three months' notice on self-judged grounds — which is what makes their treaty compliance a bargaining position rather than a concession. Leaving the regime outright would cost them security cooperation and trade benefits; the option is worth more held than exercised. They pay ordinary membership costs but nothing for keeping the pathway itself available.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, threshold_states, beneficiary,
    moderate, generational, arbitrage, regional).

% The depositaries and permanent Security Council members administer the regime: they manage each withdrawal episode through Council statements, sanctions, and coordinated diplomacy, while declining to secure any definitive ruling on whether a given withdrawal was valid. They set the review-conference agenda and draft the compromise language both camps can cite. What flows to them is regime stability maintained without the cost of codifying either reading — a hard threshold would require concessions and amendment politics they do not want; an unrestricted right would invite cascade. They also carry exposure: every successful exit weakens the regime their security planning assumes.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, nuclear_weapon_states_p5, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__withdrawal_threshold_reading, nuclear_weapon_states_p5, beneficiary).

% States that stay and pay: IAEA assessments, safeguards infrastructure, foregone enrichment latency, and the diplomatic capital spent holding the review process together. They benefit from neighbors' restraint and from the regime's existence, but they carry the maintenance burden while other parties hold the exit option free. Their own formally held withdrawal right is unusable — exercising it would trigger the regional cascade they most fear — so their compliance is unconditional in practice even though the text makes it conditional.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, compliant_nnws, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__withdrawal_threshold_reading, compliant_nnws, beneficiary).

% States that have exercised or attempted the exit right — North Korea paradigmatically, announcing in 1993 and exiting effectively in 2003. They act on the self-judged reading of the text and then absorb the political price the arrangement imposes to keep exercise from becoming cheap: Council censure that stops short of any validity determination, sanctions, isolation. After exit they stand outside the regime's benefits with their economy and security worse off, which is precisely the demonstration effect that deters the next exerciser.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, withdrawing_states, payer,
    powerless, immediate, trapped, regional).

% The verification institution absorbs the arrangement's operational costs: when a state withdraws, its safeguards agreement terminates and continuity of knowledge is lost, as happened with North Korea; every withdrawal threat forces re-verification planning and reporting redesign. It has no seat in the threshold debate, cannot adjudicate whether any withdrawal is valid, and implements consequences designed entirely by others.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, iaea_safeguards_system, payer,
    moderate, generational, constrained, global).

% India, Pakistan, and Israel are nuclear-armed states outside the treaty. Withdrawal precedent shapes their environment directly — a validated exit pathway is a template their neighbors could follow; a suppressed one is a caution — but they hold no seat in review conferences, depositary consultations, or the Council deliberations that manage each episode. Their recognition and arming decisions feed back into the threshold question from outside the room where it is discussed.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, nonparty_nuclear_states, excluded,
    powerful, generational, arbitrage, regional).

% Treaty lawyers and scholars who have debated Article X's threshold since the 1993 North Korean announcement. They see the full structure — text, negotiating history, the precedent, Security Council practice — and produce the doctrinal analyses that both the sovereignty-first and regime-stability camps cite. They hold no enforcement seat and collect nothing from the arrangement's operation.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, international_law_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_text__withdrawal_threshold_reading, threshold_states).
narrative_ontology:fixing_cost_class(npt_treaty_text__withdrawal_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solved the 1960s accession problem: states would not accept an irrevocable renunciation of the weapons option under a US-Soviet duopoly, so Article X's self-judged exit right was written to make signature possible. The clause converts perpetual lock-in into conditional commitment, and its threshold ambiguity lets every party sign while reading the exit condition its own way.
% TRANSFER_FUNCTION: Moves regime-maintenance burden — verification funding, safeguards infrastructure, foregone nuclear latency, diplomatic capital — from all parties onto those who stay, while the option value of exit accrues at zero cost to any party that might want it, currently most valuable to threshold states holding it as leverage.
% ABSENT_VOICES: Nonparty nuclear-armed states (India, Pakistan, Israel) bear the cascade consequences of withdrawal precedent but hold no seat in the review process; within the treaty, states that would bear the security costs of a neighbor's exit are represented only through general membership; and the verification institution has no voice in the threshold debate whose consequences it must implement.
% DISAPPEARANCE_RATIONALE: If the Article X arrangement — self-judged exit at three months' notice, politically managed ambiguity — vanished overnight and were replaced by either codified reading, the accession bargain rearranges: a codified high threshold would reopen ratification politics and push sovereignty-first parties toward exit before the harder rule binds them, while a codified unrestricted right would immediately reprice every threshold state's hedging and invite sequential exits. The near-universal membership the regime rests on was purchased with this clause, and the membership cannot be held without either restoring it or renegotiating the regime's terms.
% FOUNDING_PROBLEM: The 1960s commitment problem: how to obtain near-universal adherence to a permanent renunciation of the weapons option from states that feared permanent second-class status under the US-Soviet duopoly and would not sign an irrevocable treaty.
% FOUNDING_PROBLEM_CORROBORATION: The 1967-68 negotiating record of the Eighteen-Nation Disarmament Committee and contemporaneous ratification debates (including Japan's, where accession was conditioned on good-faith disarmament progress and the retained exit right) corroborate the accession-price rationale — sources outside the current beneficiary set. But no party outside the current beneficiary set attests that the founding problem remains live today: review-conference testimony splits along the same beneficiary/payer lines, with the P5 citing continuing accession dynamics and critics calling the clause a solved problem now serving option-value functions.
narrative_ontology:disappearance_verdict(npt_treaty_text__withdrawal_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__withdrawal_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__withdrawal_threshold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_treaty_text__withdrawal_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__withdrawal_threshold_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__withdrawal_threshold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_text__withdrawal_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_text__withdrawal_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at interval end): the coordination function is real — Article X was the price of near-universal accession and remains the load-bearing wall of the membership bargain — but the option value it produces is collected free by threshold states while maintenance costs are paid by compliant stayers and deterrent costs by the states that actually exercise the right. Suppression (0.60) reflects the post-2003 enforcement machinery: Security Council statements, sanctions, and coordinated responses that raise the political price of exercising a text-granted right without ever formally restricting it — suppression of the exercise, not of the right, which is why it cannot be codified and must be re-imposed episode by episode. Theater (0.42) is moderate and rising: review conferences produce withdrawal language both camps can cite, and the recurring 'threshold debate' is partly a performance that maintains the ambiguity each side needs — though the sanctions and verification responses are functionally real, so the ratio stays below the substitution threshold. Accessibility collapse (0.50): the alternatives — treaty amendment, a definitive Security Council or ICJ ruling, formal acceptance of the unrestricted reading — are visible and repeatedly proposed, but each is understood to be self-undermining (codifying either reading destroys the bargain), so they collapse only partially. Resistance (0.60): the sovereignty-first position is actively maintained through Iran's explicit invocations, the DPRK precedent, and a live doctrinal literature. All three series run on one shared grid (t=0,10,20,23,33,40,48,55) so no metric is sampled against another metric's end-state; the trajectories are monotonic rather than cyclical, driven by the ratchet of precedent rather than oscillating enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical text. From the P5 seat the arrangement is managed stability: an exit clause that keeps members in and lets the Council handle defectors without setting precedent. From the threshold-state seat it is a priced option: credibility that exit remains available is what makes their compliance a bargaining chip rather than a concession. From the compliant-NNWS seat it is free-riding machinery: they fund the regime while others hold an option they themselves cannot afford to touch. From the withdrawing-state seat it is a trap with a text-granted door: the right is real, but exercising it costs everything the regime provided. The engine computes these per-seat types from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Threshold states are declared beneficiaries with arbitrage-grade exit — they can play the ambiguity (threaten, delay, stay) — placing them near the beneficiary end of directionality. The P5 are agenda-setters and secondary beneficiaries with arbitrage exit: they administer the ambiguity and collect stability, though cascade exposure keeps them short of the full-beneficiary pole. Compliant NNWS are payers with secondary beneficiary status and constrained exit — mid-to-high directionality, since the option they formally hold is unusable in practice. Withdrawing states are victims with trapped exit — near full-target directionality: the arrangement's deterrent costs land on them. The IAEA is a payer with no adjudicating power — high directionality despite institutional standing, because its institutional power does not reach the question it must implement. No directionality overrides were needed: the beneficiary/victim declarations plus exit options already differentiate the seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — making accession possible for states that would not sign an irrevocable treaty — is plausibly solved (membership is near-universal), which is why a pure coordination reading would mislabel the present arrangement: the clause no longer primarily recruits members; it produces option value for states already inside. But a pure extraction reading would equally mislabel: the coordination function is not cover — the accession bargain was real, the membership it purchased persists, and every party still prices the option. Tangled rope holds both truths: genuine coordination (conditional commitment) and asymmetric position (free option premium, socialized maintenance). The mandate question is genuinely contested rather than resolved — the R5 interview records status 'contested' because whether the commitment-credibility problem is permanent (states always need exit assurance) or solved (the clause now serves only hedging) determines whether this is a living bargain or a mandate outliving its function. The classification prevents mislabeling in both directions: it refuses to let the accession story launder the option-premium asymmetry, and refuses to let the asymmetry story erase the bargain that holds the regime's membership together.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    npt_kernel_reading_provenance,
    'This constraint is one reading of the npt_treaty_text kernel — the withdrawal-threshold reading. Where exactly do the sibling readings diverge structurally (nws_reading on who is bound; nnws_reading on what the restraint purchases; this reading on whether exit is available), and is the family decomposition correct, or should the readings be merged or split further?',
    'Comparative authoring of the sibling files with epsilon computed over each reading''s own referent; if sibling epsilon values converge with this reading''s and victim sets coincide, the decomposition is over-split; if they diverge as authored, the family stands.',
    'Merging the readings into one constraint would average a self-judged-exit structure (moderate epsilon, threshold-state beneficiaries) with a binding-obligation structure (high epsilon on NNWS under the nws_reading), destroying the epsilon-invariance of each; splitting further (e.g., a depositary-practice reading isolating the UNSC management machinery) would give the enforcement layer its own epsilon and beneficiary set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(npt_kernel_reading_provenance, conceptual, 'Kernel decomposition boundary for the NPT text constraint family.').

omega_variable(
    withdrawal_codification_counterfactual,
    'If a future withdrawal episode forced a definitive determination (Security Council validity ruling or ICJ advisory opinion), would the pathway collapse to an adjudicated high threshold that suppresses exit, or trigger a sovereignty-first cascade of sequential withdrawals?',
    'The next withdrawal crisis that reaches formal determination; also state behavior if Article X revision ever reaches a treaty-amendment conference agenda.',
    'High-threshold codification would push the arrangement toward enforced exit-suppression with the text-granted right formally overridden; low-threshold codification would strip the option premium, reprice every threshold state''s compliance, and likely dissolve the tangled structure toward either a bare coordination remainder or regime collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(withdrawal_codification_counterfactual, empirical, 'Whether the ambiguous pathway survives its next stress test or codifies in one direction.').

omega_variable(
    unsc_practice_interpreter_status,
    'Is Security Council withdrawal-management practice (post-2003 statements and sanctions without validity determinations) a de facto authoritative interpreter of Article X, or merely one distributed voice among competing readings?',
    'Doctrinal analysis of whether states acquiesce in Council practice as interpretation (opinio juris) versus treat it as episodic politics; depositary behavior in the next episode.',
    'If a de facto interpreter exists, the commitment-system classification shifts toward extraction-grounded authority with a functioning interpretation layer; if not, the kernel remains distributed and the ambiguity is structurally unowned — maintained by no one and available to anyone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unsc_practice_interpreter_status, conceptual, 'Whether Council practice constitutes the kernel''s interpretive authority or is one voice in a distributed interpretive field.').

omega_variable(
    threshold_state_option_valence,
    'Do Japan and South Korea genuinely collect value from exit-option credibility, or are they net payers — states whose security depends on the regime holding and for whom the option''s existence is primarily the threat they fear from others exercising it?',
    'Revealed-preference analysis: whether these states'' diplomacy works to preserve the option (hedging behavior, fuel-cycle latency investment, refusal of withdrawal-restriction language) or to harden the threshold (supporting exit-restriction proposals at review conferences).',
    'If the alliance-dependent states are net payers, the beneficiary set contracts to coercive hedgers and the P5, the option''s value concentrates in fewer hands, and directionality for the moderate-power seats shifts toward the target end — changing the computed per-seat classifications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_state_option_valence, empirical, 'Whether the exit option is a subsidy or a poison pill for the alliance-dependent threshold states.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__withdrawal_threshold_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_withdrawal_threshold_tr_t0, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(npt_withdrawal_threshold_tr_t10, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(npt_withdrawal_threshold_tr_t20, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(npt_withdrawal_threshold_tr_t23, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 23, 0.28).
narrative_ontology:measurement(npt_withdrawal_threshold_tr_t33, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 33, 0.36).
narrative_ontology:measurement(npt_withdrawal_threshold_tr_t40, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 40, 0.39).
narrative_ontology:measurement(npt_withdrawal_threshold_tr_t48, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 48, 0.41).
narrative_ontology:measurement(npt_withdrawal_threshold_tr_t55, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 55, 0.42).

% Extraction over time
narrative_ontology:measurement(npt_withdrawal_threshold_be_t0, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(npt_withdrawal_threshold_be_t10, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 10, 0.33).
narrative_ontology:measurement(npt_withdrawal_threshold_be_t20, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(npt_withdrawal_threshold_be_t23, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 23, 0.45).
narrative_ontology:measurement(npt_withdrawal_threshold_be_t33, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 33, 0.52).
narrative_ontology:measurement(npt_withdrawal_threshold_be_t40, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(npt_withdrawal_threshold_be_t48, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 48, 0.57).
narrative_ontology:measurement(npt_withdrawal_threshold_be_t55, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 55, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(npt_withdrawal_threshold_su_t0, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(npt_withdrawal_threshold_su_t10, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 10, 0.13).
narrative_ontology:measurement(npt_withdrawal_threshold_su_t20, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 20, 0.16).
narrative_ontology:measurement(npt_withdrawal_threshold_su_t23, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 23, 0.34).
narrative_ontology:measurement(npt_withdrawal_threshold_su_t33, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 33, 0.52).
narrative_ontology:measurement(npt_withdrawal_threshold_su_t40, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 40, 0.56).
narrative_ontology:measurement(npt_withdrawal_threshold_su_t48, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 48, 0.58).
narrative_ontology:measurement(npt_withdrawal_threshold_su_t55, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 55, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__withdrawal_threshold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text__nws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text__nnws_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the NPT' conflates at least three structurally distinct claims read from one fixed text. This story authors only the withdrawal-threshold reading (Article X as ambiguous self-judged pathway; moderate epsilon; threshold-state option value). The nws_reading authors the text as binding constraint on NNWS (high epsilon on non-weapon states, P5-collected stability), and the nnws_reading authors it as conditional restraint purchasing NWS Article VI compliance (epsilon indexed to NWS disarmament performance). The upstream claim (near-universal accession achieved via the exit clause) is cited as evidence within the sibling readings, hence the family links; each file keeps its own epsilon, beneficiaries, and victims per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
