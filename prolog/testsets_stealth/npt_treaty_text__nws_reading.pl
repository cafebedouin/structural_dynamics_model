% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__nws_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-18
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_text__nws_reading, []).

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
 *   constraint_id: npt_treaty_text__nws_reading
 *   human_readable: NPT Regime, NWS Reading: Binding Restraint on Non-Armed States, Aspirational Disarmament
 *   domain: international_law/arms_control/treaty_interpretation
 *
 * SUMMARY:
 *   The Nuclear Non-Proliferation Treaty regime, as it actually operates
 *   under the reading the five arsenal holders maintain, binds roughly 180
 *   non-weapon states to comprehensive, externally verified, actively
 *   enforced restraint while leaving the Article VI disarmament commitment as
 *   an open-ended undertaking of good faith with no deadline, no verification
 *   standard, and no enforcement path. This file instantiates ONE reading of
 *   the contested kernel npt_treaty_text — the nws_reading — as a clean,
 *   epsilon-invariant constraint: the contest itself is recorded in omega
 *   variables and in the sibling files (npt_treaty_text__nnws_reading,
 *   npt_treaty_text__withdrawal_threshold_reading), never averaged into this
 *   one. The epsilon referent is the standing arrangement under contest — the
 *   regime as it operates — assessed by this reading's own lights: this
 *   reading accepts the asymmetry as the price of the bargain and authors the
 *   metrics that describe its actual magnitude. The claimed type and the
 *   metric values are independent authored facts: the claim states the
 *   structure believed true (a genuine coordination function wrapped around
 *   an enforced asymmetry); the metrics describe observed operation; the
 *   engine computes per-seat classifications from the structural data and
 *   owns any divergence.
 *
 * KEY AGENTS:
 *   - - nuclear_weapon_states: agenda-setter and principal beneficiary (institutional/arbitrage) — retains arsenals under the legitimating frame and controls the operative reading of the disarmament clause
 *   - - non_nuclear_weapon_states: primary payer (organized/constrained) — bears permanent verified restraint and the inspection burden
 *   - - extended_deterrence_allies: secondary beneficiary (powerful/constrained) — collects protection without ownership costs
 *   - - iaea_verification_system: administering enforcer (institutional/identity_locked) — budget and mandate concentrate on horizontal verification
 *   - - tpnw_coalition_states: excluded challenger (organized/constrained) — built a parallel instrument after concluding the clause would never be honored
 *   - - non_signatory_nuclear_states: excluded outsiders (powerful/arbitrage) — acquired arms outside the treaty and gained de facto accommodation
 *   - - disarmament_civil_society: excluded advocate (moderate/mobile) — campaigns for timed verifiable elimination from outside the decision rooms
 *   - - arms_control_legal_scholarship: analytical observer (analytical/analytical) — maps the interpretive contest, collects nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nws_reading, 0.74).
domain_priors:suppression_score(npt_treaty_text__nws_reading, 0.73).
domain_priors:theater_ratio(npt_treaty_text__nws_reading, 0.53).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, suppression_requirement, 0.73).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, theater_ratio, 0.53).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nws_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__nws_reading, "NPT Regime, NWS Reading: Binding Restraint on Non-Armed States, Aspirational Disarmament").
narrative_ontology:topic_domain(npt_treaty_text__nws_reading, "international_law/arms_control/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_treaty_text__nws_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nws_reading, 'e9839f40-216f-4720-98b7-b06f7d4f3a63').
narrative_ontology:cs_kernel_codification('e9839f40-216f-4720-98b7-b06f7d4f3a63', fixed_text).
narrative_ontology:cs_authority_grounding('e9839f40-216f-4720-98b7-b06f7d4f3a63', extraction).
narrative_ontology:cs_interpretation_layer_present('e9839f40-216f-4720-98b7-b06f7d4f3a63').
narrative_ontology:cs_reading_relation('e9839f40-216f-4720-98b7-b06f7d4f3a63', npt_treaty_text__nnws_reading, coexists_with).
narrative_ontology:cs_reading_relation('e9839f40-216f-4720-98b7-b06f7d4f3a63', npt_treaty_text__withdrawal_threshold_reading, influences).
narrative_ontology:cs_axiom('e9839f40-216f-4720-98b7-b06f7d4f3a63', foundational, article_vi_good_faith_without_deadline).
narrative_ontology:cs_axiom_status(article_vi_good_faith_without_deadline, holdable).
narrative_ontology:cs_axiom_grounding('e9839f40-216f-4720-98b7-b06f7d4f3a63', article_vi_good_faith_without_deadline, conventional).
narrative_ontology:cs_axiom('e9839f40-216f-4720-98b7-b06f7d4f3a63', foundational, nonproliferation_duties_unconditional_on_disarmament_performance).
narrative_ontology:cs_axiom_status(nonproliferation_duties_unconditional_on_disarmament_performance, holdable).
narrative_ontology:cs_axiom_grounding('e9839f40-216f-4720-98b7-b06f7d4f3a63', nonproliferation_duties_unconditional_on_disarmament_performance, conventional).
narrative_ontology:cs_axiom('e9839f40-216f-4720-98b7-b06f7d4f3a63', secondary, regime_longevity_vindicates_gradualism).
narrative_ontology:cs_axiom_status(regime_longevity_vindicates_gradualism, holdable).
narrative_ontology:cs_axiom_grounding('e9839f40-216f-4720-98b7-b06f7d4f3a63', regime_longevity_vindicates_gradualism, empirically_contingent).
narrative_ontology:cs_reference_frame('e9839f40-216f-4720-98b7-b06f7d4f3a63', nonproliferation_primacy_frame).
narrative_ontology:cs_drift_state('e9839f40-216f-4720-98b7-b06f7d4f3a63', post_tpnw_entry_into_force, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e9839f40-216f-4720-98b7-b06f7d4f3a63', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__nws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, extended_deterrence_allies).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, non_nuclear_weapon_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, iaea_verification_system).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, tpnw_coalition_states).
narrative_ontology:constraint_vindicates(npt_treaty_text__nws_reading, deterrence_stability_doctrine).
narrative_ontology:constraint_vindicates(npt_treaty_text__nws_reading, step_by_step_disarmament_principle).
narrative_ontology:constraint_vindicates(npt_treaty_text__nws_reading, horizontal_proliferation_priority_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Five states holding arsenals at the treaty's signing. They administer the regime through Security Council vetoes, control the pace and interpretation of the disarmament language, submit only voluntary-offer safeguards covering small fractions of their own facilities, and decide among themselves what continued good faith requires. Leaving the arrangement would cost them the legitimating frame for arsenal retention and invite counter-proliferation coalitions; staying asks nothing enforceable of them.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nws_reading, nuclear_weapon_states, beneficiary).

% States — NATO members, Japan, the Republic of Korea, Australia — that forgo weapons programs in exchange for security guarantees from an arsenal holder. They receive protection without bearing the political costs of weapons ownership, vote to preserve the regime, and lobby against prohibition instruments that would delegitimize the umbrella. Exiting would mean either indigenous weapons programs or exposed dependence, both worse than their current position.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, extended_deterrence_allies, beneficiary,
    powerful, biographical, constrained, global).

% The Agency and its member-state board run inspection regimes, verify declarations, and report noncompliance to the Security Council. Its safeguards budget and staffing grew around verifying non-weapon states; arsenal-holder facilities enter only through voluntary offers covering a small fraction of sites. The institution's mandate, expertise, and funding are bound to the horizontal-verification mission; reorienting toward arsenal-holder stockpile verification would require rewriting its statutory basis and funding structure.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, iaea_verification_system, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nws_reading, iaea_verification_system, beneficiary).

% The roughly 180 states that joined without weapons. They accept comprehensive safeguards, export-control limits on their nuclear industries, and permanent foregone weapons options, while the disarmament clause they cite carries no deadline and no enforcement. Their collective leverage shows in General Assembly majorities and the humanitarian-initiative treaty process, but inside the review conference they need consensus, which the arsenal holders can block. Individual exit through the withdrawal clause invites sanctions and isolation, as the one withdrawing state's experience shows.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, non_nuclear_weapon_states, payer,
    organized, generational, constrained, global).

% A coalition of non-weapon states plus allied campaigns that negotiated a prohibition instrument outside the review process after concluding the disarmament clause would never be honored on its own terms. The arsenal holders and their allies boycotted the negotiation and reject the instrument's premises; inside the review conference this coalition's timeline proposals are consistently struck from consensus texts.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, tpnw_coalition_states, excluded,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nws_reading, tpnw_coalition_states, payer).

% Three states that acquired weapons without joining. They sit outside the treaty's obligations and its formal benefits, yet have gained de facto accommodation — one received an exemption from the export-control cartel, others maintain informal deterrence relationships. Their existence demonstrates that the boundary the regime polices is drawn by membership rather than capability, and they would insist any legitimate order accommodate armed status outside the founding club.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, non_signatory_nuclear_states, excluded,
    powerful, generational, arbitrage, regional).

% Campaign networks, survivor organizations, and legal advocates pressing for timed, verifiable elimination. They hold no decision seats in review conferences, funded the advocacy that produced the prohibition instrument and a Nobel recognition, and document the gap between declared commitments and arsenal-modernization budgets. Their influence runs through public opinion and coalition politics rather than through the treaty's own rooms.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, disarmament_civil_society, excluded,
    moderate, biographical, mobile, global).

% Academic interpreters and practitioners who map the treaty's interpretive contest, publish the doctrinal analyses both camps cite, and supply the advisory-opinion record showing the disarmament clause imposes a good-faith duty of unspecified content. They collect no rents and bear no obligations under the arrangement.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, arms_control_legal_scholarship, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_text__nws_reading, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_treaty_text__nws_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the cascade problem: each state's restraint is worth little unless others' restraint is assured, so the treaty supplies mutually verified restraint plus a predictable licensing framework for peaceful nuclear commerce, administered centrally through the Agency.
% TRANSFER_FUNCTION: Moves the weapons option and fuel-cycle autonomy from non-weapon states to the collective regime — permanently, under external verification; moves compliance risk and inspection burden onto non-weapon states; moves status rents and time to the five arsenal holders, whose possession the treaty text legally accommodates.
% ABSENT_VOICES: The three armed states outside the treaty would object to a legitimate order that freezes them out of the legal arsenal-holder category; survivor organizations and campaign networks hold no decision seats; inside the treaty, the non-weapon majority is present but without agenda power — consensus procedure lets the five strike timeline language from operative texts.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would reopen the weapons question in a dozen capable capitals simultaneously, dissolve the licensing framework governing the largest civilian plutonium and enrichment commerce, and force the alliance systems built on extended deterrence to renegotiate their foundations — the arrangements of every named seat depend on the regime persisting in something like its current form.
% FOUNDING_PROBLEM: Two linked problems circa 1968: a forecast cascade — senior officials publicly estimated ten to twenty-five new arsenal states within two decades — that mutual suspicion made unilateral restraint irrational; and the demand of non-weapon states that freezing the club be paired with a commitment to end the division, written into Article VI as the price of their accession.
% FOUNDING_PROBLEM_CORROBORATION: Outside the beneficiary set: the ICJ's 1996 advisory opinion (unanimously finding a good-faith duty exists while declining to fix its content), the humanitarian-initiative conference record and prohibition-treaty negotiating history assembled by non-weapon-state sponsors, Non-Aligned Movement summit documents dating to 1975, and published treaty histories by scholars who take no side in the contest. No source outside the arsenal holders and their allies attests that the disarmament half remains on track; conversely, no serious party denies the cascade-prevention half was achieved.
narrative_ontology:disappearance_verdict(npt_treaty_text__nws_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__nws_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__nws_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_treaty_text__nws_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__nws_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__nws_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_text__nws_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_text__nws_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.74 reflects the shape of the obligations: non-weapon states accept permanent, verified, externally audited restraint whose breach triggers Security Council referral and sanctions, while the arsenal holders' reciprocal clause carries no date, no standard, and no consequence for non-performance — and the holders control the operative interpretation of 'at an early date'. Suppression 0.73 reflects real machinery (full-scope safeguards, the Additional Protocol, the export-control cartel, interdiction partnerships, Council referral) layered onto consensual accession; the withdrawal door formally exists but its one user paid in sanctions and isolation. Theater 0.53: the verification function is genuinely functional, but the review-conference cycle, action-plan bookkeeping, and step-by-step communiques increasingly perform commitment without consequence — timeline language is struck from every consensus text, and the 2015 and 2022 conferences closed without consensus outputs at all. Accessibility collapse 0.40: exits and alternatives demonstrably exist (withdrawal, outside-the-treaty acquisition, the prohibition instrument) but each carries severe cost or exclusion. Resistance 0.55: a sustained non-weapon-state coalition built an entire parallel treaty, the movement bloc presses annually, and one state exited with its arsenal program intact. All three series share one time grid (1970-2026, eight points); the trajectories show extraction accumulation, an enforcement ratchet concentrated after the 1991 Iraq discoveries and the 2003 withdrawal, and monotonic theater drift after indefinite extension removed the renewal lever.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from one structure. From the agenda-setting seat the arrangement is a regime it built, funds, and interprets — coordination it legitimately administers, with the asymmetry read as the stable price of a working bargain. From the payer seat the same clauses operate as permanent verified restraint traded against an unenforceable promise, with the interpreter of the promise sitting on the other side of the table. Extended-deterrence allies sit near the middle: they collect protection and pay mainly reputational costs. The excluded seats see the boundary-drawing itself — outsiders who acquired arms outside the treaty demonstrate the line is drawn by membership rather than capability, and the prohibition coalition reads the whole structure as a failed bargain maintained by procedural control. The engine derives these divergences from the declared roles, power atoms, and exit options; nothing here adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states anchor the beneficiary end: they collect the status rents, the time, and the interpretive control, and their exit is arbitrage-grade because they wrote the operative reading. Extended-deterrence allies derive low directionality as declared beneficiaries — protection flows in, only reputational costs flow out. The verification system sits mid-low: it collects budget and mandate growth from the horizontal focus while performing a real coordination service, and its institutional identity is fused with the mission it administers. Non-weapon states anchor the target end: binding duties, inspection burden, foregone options, constrained exit. The excluded seats (outsider armed states, prohibition coalition, civil society) contribute to the suppression and absent-voice picture rather than to the beneficiary/target axis — their exclusion is part of what the enforcement maintains.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem had two halves, and the classification turns on keeping them apart. The cascade-prevention half is live and was achieved — which is exactly why the arrangement cannot be read as pure extraction: a real collective-action problem is solved, continuously, for nearly all parties. The disarmament half is dead as operated — no deadline, no standard, no consequence — yet the parties dispute its status, so the genealogy is authored contested rather than dead, and the zombie mismatch flag (dead status plus world-rearranging persistence) deliberately does not fire. What prevents mislabeling in both directions: reading the regime as pure coordination ignores that the asymmetry is enforced and that its terms are set by its beneficiaries; reading it as pure extraction ignores that the coordination function is genuine and that most participants renew voluntarily. The 1995 indefinite extension is the identifiable moment the hybrid hardened — the renewal lever was surrendered without benchmarks in return — and the rising theater series tracks the substitution of review ritual for the surrendered leverage.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_legal_force,
    'Does Article VI impose a binding obligation of determinate content (good-faith negotiation toward a specified result within a reasonable time), or an aspirational undertaking whose satisfaction is measured only by continued process?',
    'Contentious proceedings before the International Court of Justice brought by a non-weapon-state coalition, or a Review Conference adopting benchmark language over arsenal-holder objection — either would force the interpretive choice into an authoritative forum.',
    'A binding-with-content finding converts the unenforced duty into the dominant extraction channel and pushes the arrangement toward pure extraction; confirmation of the aspirational reading stabilizes the current hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_legal_force, conceptual, 'Whether the disarmament clause binds with content or merely exhorts.').

omega_variable(
    kernel_reading_contest_structure,
    'This story instantiates the nws_reading of kernel npt_treaty_text; what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Comparative authoring of the sibling stories (npt_treaty_text__nnws_reading, npt_treaty_text__withdrawal_threshold_reading) against the same referent; the disagreement is located in the legal force of Article VI and in the permissible threshold of the Article X withdrawal clause.',
    'The nnws_reading would raise the authored epsilon over the same referent and re-weight the harm toward the disarmament-clause breach; the withdrawal_threshold_reading leaves epsilon largely fixed but reshapes exit structure and therefore effective extraction for marginally mobile non-weapon states.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, conceptual, 'Committer-frame record of the kernel''s reading contest and its structural stakes.').

omega_variable(
    safeguards_budget_allocation_motive,
    'Is the concentration of verification resources on non-weapon-state facilities a technical necessity (fissile-material accounting is where diversion risk lies) or an artifact of arsenal-holder influence over the Agency''s budget and board?',
    'Resource audit comparing verification intensity per kilogram of weapons-usable material at voluntary-offer versus full-scope facilities, plus board voting records on verification-expansion proposals.',
    'If influence-driven, the verification apparatus functions partly as the asymmetry''s administrative arm and effective extraction on payer seats rises; if necessity-driven, the concentration is coordination cost and the hybrid reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safeguards_budget_allocation_motive, empirical, 'Whether horizontal verification concentration reflects risk profile or institutional capture.').

omega_variable(
    indefinite_extension_leverage_loss,
    'Did the 1995 indefinite extension permanently remove the periodic-review leverage non-weapon states held under 25-year renewal cycles, or did the strengthened-review-process promises create substitute accountability?',
    'Before/after comparison of Review Conference outcome documents and implementation rates of agreed actions across the 1995 boundary.',
    'Permanent leverage loss dates the ratchet to 1995 and marks the review process as the performative channel; substitute accountability would support a recoverable-hybrid reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indefinite_extension_leverage_loss, empirical, 'Whether the 1995 extension converted review leverage into a one-time concession.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nws_reading, 1970, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_text__nws_reading, theater_ratio, 1970, 0.25).
narrative_ontology:measurement(npt__tr_t1978, npt_treaty_text__nws_reading, theater_ratio, 1978, 0.28).
narrative_ontology:measurement(npt__tr_t1986, npt_treaty_text__nws_reading, theater_ratio, 1986, 0.3).
narrative_ontology:measurement(npt__tr_t1995, npt_treaty_text__nws_reading, theater_ratio, 1995, 0.38).
narrative_ontology:measurement(npt__tr_t2003, npt_treaty_text__nws_reading, theater_ratio, 2003, 0.42).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_text__nws_reading, theater_ratio, 2010, 0.44).
narrative_ontology:measurement(npt__tr_t2017, npt_treaty_text__nws_reading, theater_ratio, 2017, 0.5).
narrative_ontology:measurement(npt__tr_t2026, npt_treaty_text__nws_reading, theater_ratio, 2026, 0.53).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_text__nws_reading, base_extractiveness, 1970, 0.52).
narrative_ontology:measurement(npt__be_t1978, npt_treaty_text__nws_reading, base_extractiveness, 1978, 0.55).
narrative_ontology:measurement(npt__be_t1986, npt_treaty_text__nws_reading, base_extractiveness, 1986, 0.58).
narrative_ontology:measurement(npt__be_t1995, npt_treaty_text__nws_reading, base_extractiveness, 1995, 0.63).
narrative_ontology:measurement(npt__be_t2003, npt_treaty_text__nws_reading, base_extractiveness, 2003, 0.67).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_text__nws_reading, base_extractiveness, 2010, 0.69).
narrative_ontology:measurement(npt__be_t2017, npt_treaty_text__nws_reading, base_extractiveness, 2017, 0.72).
narrative_ontology:measurement(npt__be_t2026, npt_treaty_text__nws_reading, base_extractiveness, 2026, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_text__nws_reading, suppression_requirement, 1970, 0.35).
narrative_ontology:measurement(npt__su_t1978, npt_treaty_text__nws_reading, suppression_requirement, 1978, 0.4).
narrative_ontology:measurement(npt__su_t1986, npt_treaty_text__nws_reading, suppression_requirement, 1986, 0.44).
narrative_ontology:measurement(npt__su_t1995, npt_treaty_text__nws_reading, suppression_requirement, 1995, 0.55).
narrative_ontology:measurement(npt__su_t2003, npt_treaty_text__nws_reading, suppression_requirement, 2003, 0.62).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_text__nws_reading, suppression_requirement, 2010, 0.66).
narrative_ontology:measurement(npt__su_t2017, npt_treaty_text__nws_reading, suppression_requirement, 2017, 0.7).
narrative_ontology:measurement(npt__su_t2026, npt_treaty_text__nws_reading, suppression_requirement, 2026, 0.73).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__nws_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, npt_treaty_text__nnws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, npt_treaty_text__withdrawal_threshold_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the kernel npt_treaty_text per the epsilon-invariance principle: the colloquial label 'the NPT bargain' covers structurally distinct claims that cannot share one epsilon. This file instantiates the nws_reading (operative reading: binding non-proliferation, aspirational disarmament). The sibling npt_treaty_text__nnws_reading authors the same referent from the non-weapon-state seat (higher epsilon, conditional-restraint structure); npt_treaty_text__withdrawal_threshold_reading decomposes the exit clause into a separate threshold contest. The nws_reading is upstream: it controls operative interpretation and thereby shapes the legitimacy conditions and resource availability under which both siblings operate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
