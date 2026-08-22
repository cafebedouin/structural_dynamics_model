% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__withdrawal_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_1970__withdrawal_sovereignty_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: npt_treaty_1970__withdrawal_sovereignty_reading
 *   human_readable: NPT Article X Withdrawal Right — Sovereignty Reading (Obligations as Revocable Consent)
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   Article X of the Nuclear Nonproliferation Treaty (in force 1970) permits
 *   any party to withdraw on three months' notice, citing extraordinary
 *   events it regards as having jeopardized its supreme interests. This story
 *   instantiates one reading of the NPT kernel: the withdrawal-sovereignty
 *   reading, under which the clause is a legitimate sovereignty reservation
 *   and treaty obligations are contingent on the security environment that
 *   induced them — revocable consent, not perpetual allegiance. Per the
 *   epsilon-invariance discipline, the reading is generated as its own
 *   constraint: the epsilon referent is the standing revocable-obligations
 *   arrangement itself, assessed by this reading's own lights (which credit
 *   the consent function the clause performs and discount what the
 *   enforcement reading would count as defection), which is why the authored
 *   epsilon (0.52) sits well below what the oligopoly-enforcement sibling
 *   would author over the same arrangement. The structural delta against the
 *   siblings: the regime-stability norm enters the victim set (each
 *   legitimized exit undermines the compliance incentive), threshold states
 *   gain option value from the withdrawal threat, and obligations become
 *   revocable rather than binding. The claim (tangled_rope) and the metrics
 *   are independent authored facts: the clause genuinely solved a
 *   ratification problem no other instrument solved, and the same structure
 *   concentrates bargaining leverage in exit-capable states at the expense of
 *   complying parties and the regime's bindingness. The engine computes
 *   per-seat classifications from the structural data. KEY AGENTS (by
 *   structural relationship): - threshold_states: primary beneficiary
 *   (organized/arbitrage) — holds the codified exit card; option value prices
 *   into every negotiation it runs - withdrawal_precedent_states: realized
 *   beneficiary (moderate/arbitrage) — exercised the clause in 2003 and
 *   monetized the exit cycle - nonaligned_sovereignty_bloc: beneficiary and
 *   reading-maintenance crew (organized/mobile) — defends exit legitimacy in
 *   review conferences - nuclear_weapon_states: agenda_setter and payer
 *   (institutional/arbitrage) — authored the clause as the ratification
 *   price, now runs the criminalization counter-doctrine and pays in
 *   exit-threat leverage - good_faith_complying_nnws: primary payer
 *   (moderate/constrained) — complies fully while bearing the security
 *   externality of neighbors' live exit cards - iaea_safeguards_system: payer
 *   (institutional/trapped) — absorbs continuity-of-safeguards and
 *   verification-gap costs of each exit - regime_stability_norm: victim at
 *   the norm level (non-agent) — bindingness depreciates with each
 *   legitimized withdrawal; costs land on complying parties -
 *   disarmament_civil_society: excluded (moderate/constrained) — would
 *   contest the revocability reading; consultative status only -
 *   treaty_law_scholars: analytical observer (analytical/analytical) — tracks
 *   the doctrinal fight over accrued obligations
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__withdrawal_sovereignty_reading, 0.52).
domain_priors:suppression_score(npt_treaty_1970__withdrawal_sovereignty_reading, 0.58).
domain_priors:theater_ratio(npt_treaty_1970__withdrawal_sovereignty_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__withdrawal_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__withdrawal_sovereignty_reading, "NPT Article X Withdrawal Right — Sovereignty Reading (Obligations as Revocable Consent)").
narrative_ontology:topic_domain(npt_treaty_1970__withdrawal_sovereignty_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__withdrawal_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__withdrawal_sovereignty_reading, 'f5952d86-f17e-43f0-a954-d1956098c4e6').
narrative_ontology:cs_kernel_codification('f5952d86-f17e-43f0-a954-d1956098c4e6', fixed_text).
narrative_ontology:cs_authority_grounding('f5952d86-f17e-43f0-a954-d1956098c4e6', distributed).
narrative_ontology:cs_reading_relation('f5952d86-f17e-43f0-a954-d1956098c4e6', npt_treaty_1970__oligopoly_enforcement_reading, influences).
narrative_ontology:cs_reading_relation('f5952d86-f17e-43f0-a954-d1956098c4e6', npt_treaty_1970__reciprocal_disarmament_reading, influences).
narrative_ontology:cs_axiom('f5952d86-f17e-43f0-a954-d1956098c4e6', foundational, sovereign_exit_right_inalienable).
narrative_ontology:cs_axiom_status(sovereign_exit_right_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('f5952d86-f17e-43f0-a954-d1956098c4e6', sovereign_exit_right_inalienable, deontological).
narrative_ontology:cs_axiom('f5952d86-f17e-43f0-a954-d1956098c4e6', foundational, obligations_contingent_on_security_environment).
narrative_ontology:cs_axiom_status(obligations_contingent_on_security_environment, holdable).
narrative_ontology:cs_axiom_grounding('f5952d86-f17e-43f0-a954-d1956098c4e6', obligations_contingent_on_security_environment, instrumental).
narrative_ontology:cs_reference_frame('f5952d86-f17e-43f0-a954-d1956098c4e6', sovereign_consent_conditionality_framework).
narrative_ontology:cs_drift_state('f5952d86-f17e-43f0-a954-d1956098c4e6', post_dprk_withdrawal_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('f5952d86-f17e-43f0-a954-d1956098c4e6', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, nonaligned_sovereignty_bloc).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, withdrawal_precedent_states).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, good_faith_complying_nnws).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, regime_stability_norm).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, iaea_safeguards_system).
narrative_ontology:constraint_vindicates(npt_treaty_1970__withdrawal_sovereignty_reading, state_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(npt_treaty_1970__withdrawal_sovereignty_reading, supreme_interest_self_judgment_doctrine).
narrative_ontology:constraint_vindicates(npt_treaty_1970__withdrawal_sovereignty_reading, clausula_rebus_sic_stantibus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States with latent weapons capability that stay inside the regime while keeping the withdrawal card warm. The codified exit — three months' notice citing extraordinary events jeopardizing supreme interests — prices into every negotiation they run with the weapons states and their neighbors: security assurances, technology access, and sanctions posture all move with the credibility of the threat. They collect the option value without exercising it; exercising it is the last move, not the profitable one.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states, beneficiary,
    organized, generational, arbitrage, global).

% A coalition of mostly non-weapons states, coordinated through movement caucusing and review-conference bloc discipline, that defends withdrawal as a sovereignty reservation and resists automatic Security Council consequences attaching to exit. What flows to them is a general precedent: if exit can be criminalized in this treaty, the consent basis of treaties generally weakens. Their defense work is the active maintenance the reading requires.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, nonaligned_sovereignty_bloc, beneficiary,
    organized, generational, mobile, global).

% The state that has actually exercised the clause: gave notice in 2003 citing security threats, then ran a cycle of escalation, negotiated freezes, sanctions, and aid-for-agreement deals on the strength of the demonstrated exit. What flowed to it was concessions and strategic space; what it exported was the precedent that the exit works. Its security environment is regional; the effect of its precedent is global.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, withdrawal_precedent_states, beneficiary,
    moderate, biographical, arbitrage, regional).

% The five recognized weapons states that drafted the treaty and administer the regime: they accepted the withdrawal clause in 1968 as the price of near-universal ratification, and they now run the counter-doctrine — Security Council language asserting that withdrawal does not extinguish accrued obligations, safeguards-succession arguments, coordinated pressure on withdrawers. What flows from them is enforcement and interpretation; what flows to them is the leverage bill — every threshold state's live exit threat extracts concessions they pay, and the doctrine the clause grounds is the one they now contest.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__withdrawal_sovereignty_reading, nuclear_weapon_states, payer).

% The majority bloc of non-weapons states that comply fully: they forgo weapons, accept comprehensive safeguards, fund verification, and vote the regime's budgets, while threshold neighbors hold live exit cards that keep their security environment permanently contingent. Exit is legally open to them too, but exercising it would collapse the compliance standing they trade on and trigger the regional cascade they most fear — so they remain, and pay, in place.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, good_faith_complying_nnws, payer,
    moderate, generational, constrained, global).

% The treaty's verification organ. Each withdrawal hands it a continuity-of-safeguards dispute (whether safeguards agreements survive exit), a verification gap where inspections used to run, and a budget stretched across the difference. It cannot decline the function; verification is its mandate, and it has no position from which to exit the arrangement it verifies.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, iaea_safeguards_system, payer,
    institutional, generational, trapped, global).

% The norm-level entity the reading's structural accounting places among those who bear costs: the working expectation that treaty obligations bind. Each legitimized withdrawal depreciates it — obligations become revocable rather than binding — and the depreciation is carried by every party whose compliance posture rests on others' compliance not being optional. It is a norm rather than an actor, listed for completeness; its losses land on the complying parties.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, regime_stability_norm, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(npt_treaty_1970__withdrawal_sovereignty_reading, regime_stability_norm).

% NGO coalitions, hibakusha organizations, and disarmament advocacy networks holding consultative status without a vote. They would argue that legitimizing revocable obligations hollows the disarmament side of the bargain the treaty was traded for, and that exit legitimacy is being priced onto the wrong side of the exchange. They are present at the margins of review conferences and absent from the state-to-state bargaining where the reading is actually defended.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, disarmament_civil_society, excluded,
    moderate, generational, constrained, global).

% International-law scholars and practitioners tracking the doctrinal fight: whether withdrawal extinguishes accrued obligations, whether the fundamental-change-of-circumstances doctrine bounds self-judgment, what the 1968 drafting history settles and what it leaves open. They produce the analyses the diplomatic process cites and sets aside in equal measure.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, treaty_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states).
narrative_ontology:fixing_cost_class(npt_treaty_1970__withdrawal_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the sovereignty-commitment problem that blocked near-universal adherence: no state in 1968 would accept perpetual security obligations, so Article X converts permanent allegiance into renewable consent — obligations bind only while the security environment that induced them persists, with exit on three months' notice. The exit valve also keeps marginal states inside the regime who would otherwise never join, and gives every party a standing insurance policy that makes the safeguards burden acceptable.
% TRANSFER_FUNCTION: Moves strategic option value and bargaining leverage from the regime's compliance structure to exit-capable states: threshold states convert the credible withdrawal threat into security assurances, sanctions relief, and concessions (the 2003 exit cycle is the realized case), while each legitimized withdrawal transfers a depreciation charge to every complying party — bindingness is the asset being drawn down.
% ABSENT_VOICES: Disarmament civil society and hibakusha organizations are outside the state-to-state bargaining where the reading is defended; populations of regional cascade zones — states whose neighbors hold live exit cards — have no seat at review conferences; and future parties to a post-exit proliferation cascade are unrepresented by construction. Within the talks the reading is negotiated almost exclusively among states: the people whose security is made contingent are not parties to it.
% DISAPPEARANCE_RATIONALE: If the withdrawal right and the sovereignty reading of it vanished overnight — obligations became binding and exit criminalized — the treaty's membership logic would rearrange: states that joined because consent stayed renewable would reassess adherence, the bloc that defends exit legitimacy would contest the criminalization openly, safeguards politics would reorganize around enforcement against parties with no exit, and the bargaining leverage currently priced into every threshold state's posture would evaporate. The regime would survive in name but reconstitute around a different consent structure.
% FOUNDING_PROBLEM: In 1965–1968 the drafters needed near-universal adherence to permanent security commitments from sovereign states that would not accept perpetual obligation: the US Joint Committee on Atomic Energy made a withdrawal clause a condition of Senate ratification, and the clause was accepted as the sovereignty price of a universal treaty — obligations contingent on the security environment that induced consent, exit on notice.
% FOUNDING_PROBLEM_CORROBORATION: The 1968 US ratification record (Senate testimony of ACDA director William Foster) and the Soviet delegation's acceptance of the clause attest, from outside the current beneficiary set, that the withdrawal right was the deliberate sovereignty condition of adherence — the weapons states who now resist the conditionality doctrine attested at founding that no universal treaty was ratifiable without it. Contemporary treaty-law scholarship outside the regime, on fundamental-change-of-circumstances doctrine and state consent, corroborates that the sovereignty problem the clause solved is structural and ongoing rather than historical.
narrative_ontology:disappearance_verdict(npt_treaty_1970__withdrawal_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__withdrawal_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__withdrawal_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_treaty_1970__withdrawal_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__withdrawal_sovereignty_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__withdrawal_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_1970__withdrawal_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_1970__withdrawal_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.52: under this reading's own lights the clause performs a real consent function, but the structural record shows option value accumulating in exit-capable states (the series rises 0.20 to 0.55 across the 2003 demonstration, then plateaus near 0.52 as the Security Council counter-doctrine blunts marginal gains). Suppression (0.58) is the reading's maintenance burden, not the regime's coercion of members: since 2003, holding withdrawal legitimacy in place requires active bloc discipline, review-conference defense, and resistance to nullification doctrine — the rising suppression_requirement series traces that enforcement build-up, which is why the series is authored at all rather than left to the scalar. Theater (0.35) is the ritual layer: condemnation-paired-with-engagement cycles around the withdrawer, extension-bargain reaffirmations never operationalized. Accessibility_collapse (0.45) is honest to an open contest: the sibling readings keep the binding-obligations alternative alive in the same texts and rooms. Resistance (0.6) is the counter-doctrine itself — Security Council resolution language, safeguards-succession argument, the scholarship. All three series run on one shared grid (years since entry-into-force: 1970 = 0 through 2025 = 55) with every metric authored at every point; the extraction series is deliberately non-monotonic at the end because the counter-doctrine measurably blunted, without reversing, the option-value gains.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute different constraints from one text. From the threshold-state seat the arrangement is insurance and bargaining infrastructure: obligations that bind only while conditions hold are the only obligations a sovereign can rationally accept, and the exit card is the premium refunded. From the complying-state seat the same structure is a standing security externality: neighbors hold revocable commitments, so no compliance posture is ever finally safe, and the verification burden falls on those who were staying anyway. The weapons-state seat is split against itself: it authored the clause as the price of universality and now spends institutional capital contesting the doctrine that clause legitimates — the agenda-setter pays the leverage its own instrument created. The engine computes these per-seat divergences from power, exit, and role; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d: threshold_states and withdrawal_precedent_states sit near the full-beneficiary end (the arrangement subsidizes their option value directly, and their exit is arbitrage-grade — the clause IS their exit). nonaligned_sovereignty_bloc collects the general precedent and sits slightly higher. The payer declarations drive high d: good_faith_complying_nnws bear the externality with practically constrained exit (legal exit exists, but exercising it destroys the compliance standing they trade on and triggers the cascade they fear); iaea_safeguards_system absorbs each exit's operational cost with no exit from its mandate at all. regime_stability_norm is authored as a non-agent and is excluded from directional arithmetic; its erosion is carried by the complying parties. nuclear_weapon_states are deliberately left out of the beneficiaries and victims arrays: their position is genuinely mixed (authors of the clause, payers of the leverage it created, opponents of the doctrine it now grounds), and no directionality override is authored because overrides key on power atoms and would misfire across the institutional seats this story contains — the weapons states and the safeguards system are both institutional but sit at opposite structural ends, so a per-atom correction would fix one and break the other.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim is what prevents both mislabels. Reading the clause as the enforcement sibling does — a loophole to be closed — would push the arrangement toward snare treatment (suppress the exit, trap the members), which mislabels genuine coordination: without Article X there is no near-universal treaty, and the states that joined because consent stays renewable are held inside by the very exit the closure would remove. Reading it as this reading's own pure-legitimacy framing would push toward rope and hide the extraction: option value concentrates in exit-capable states, compliers and the verification system pay, and bindingness — the regime's core asset — is drawn down with each legitimized exit. The founding problem (sovereign states will not accept perpetual security obligations) is live: the clause still performs the function it was built for, so no mandatrophy is declared, and the founding-problem-status x disappearance-verdict pair (live x world_rearranges) is consistent — no capture or zombie flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the withdrawal_sovereignty_reading of the npt_treaty_1970 kernel — how would instantiating either sibling reading change the structure?',
    'Compile the sibling stories (npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970__reciprocal_disarmament_reading) and compare victim/beneficiary sets and epsilon: the oligopoly reading moves Articles I-II into primary binding status and recasts exit threats as regime defects to be closed (criminalization pressure; threshold option value removed from the beneficiary side); the reciprocal reading makes Article VI a binding obligation with temporal urgency, converting exit-threat leverage into bargain-enforcement leverage.',
    'Under either sibling the regime-stability norm leaves the victim set and threshold option value is reclassified from insurance to defect; the disagreement is located in Article X''s status — sovereignty reservation (this reading), enforcement loophole (oligopoly), or bargain collateral (reciprocal) — and in whether obligations are revocable or binding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one of three readings of the NPT kernel; sibling readings restructure the victim and beneficiary sets.').

omega_variable(
    withdrawal_contagion_vs_sui_generis,
    'Does each legitimized withdrawal actually erode compliance incentives across threshold states (the regime-stability cost this reading''s victim set encodes), or is the 2003 exit sui generis — pariah isolation pricing exit so high that no follower follows?',
    'Comparative analysis of threshold-state postures after 2003 (hedging rhetoric, latent-capability investment, withdrawal-threat deployment in Iran, South Korea, and Japan debates): if exit-threat behavior rose measurably across threshold states, contagion is real; if post-2003 threshold states converged on deeper institutional binding instead, the cost is contained.',
    'If contagion is real, the victim-set costs exceed what the moderate epsilon encodes and the arrangement drifts toward pure extraction; if the precedent is contained, the reading''s cost accounting shrinks toward the coordination end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(withdrawal_contagion_vs_sui_generis, empirical, 'Whether withdrawal legitimacy contagiously erodes compliance or the precedent stays isolated.').

omega_variable(
    supreme_interest_objectivity,
    'Can ''extraordinary events jeopardizing supreme interests'' be adjudicated by any body other than the withdrawing state, or is the self-judgment clause absolute?',
    'State practice and Security Council response to withdrawal notices: the 2003 notice was deplored but not nullified; if a future notice triggers binding nullification or accrued-obligation enforcement, self-judgment is bounded; if notices remain unreviewable, it is absolute.',
    'If adjudicable, the sovereignty reading''s conditionality is bounded and threshold option value is discounted; if absolute, option value is unbounded and the extraction series understates it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supreme_interest_objectivity, empirical, 'Whether the self-judgment clause admits external adjudication.').

omega_variable(
    option_value_distribution,
    'Does the withdrawal option''s value concentrate in threshold states (as this reading''s beneficiary set encodes) or diffuse across the whole membership as general sovereignty insurance?',
    'Negotiating-record analysis across review conferences: whose positions move when withdrawal legitimacy is contested — if only threshold-state demands shift, value is concentrated; if general bloc bargaining posture moves uniformly, value is diffuse.',
    'If diffuse, the victim set narrows (compliers hold the same insurance they pay for) and the asymmetry claim weakens toward pure coordination; if concentrated, the hybrid structure is confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(option_value_distribution, empirical, 'Whether exit-option value is concentrated in threshold states or diffuse across the membership.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__withdrawal_sovereignty_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(npt__tr_t5, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(npt__tr_t15, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement(npt__tr_t25, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement(npt__tr_t33, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 33, 0.3).
narrative_ontology:measurement(npt__tr_t45, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 45, 0.34).
narrative_ontology:measurement(npt__tr_t55, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 55, 0.35).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(npt__be_t5, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 5, 0.24).
narrative_ontology:measurement(npt__be_t15, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 15, 0.31).
narrative_ontology:measurement(npt__be_t25, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 25, 0.42).
narrative_ontology:measurement(npt__be_t33, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 33, 0.55).
narrative_ontology:measurement(npt__be_t45, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 45, 0.54).
narrative_ontology:measurement(npt__be_t55, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 55, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(npt__su_t5, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 5, 0.15).
narrative_ontology:measurement(npt__su_t15, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 15, 0.2).
narrative_ontology:measurement(npt__su_t25, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 25, 0.34).
narrative_ontology:measurement(npt__su_t33, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 33, 0.5).
narrative_ontology:measurement(npt__su_t45, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 45, 0.56).
narrative_ontology:measurement(npt__su_t55, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 55, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__withdrawal_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970__oligopoly_enforcement_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970__reciprocal_disarmament_reading).

% DUAL FORMULATION NOTE:
% Constraint family from the epsilon-invariance decomposition of the 'NPT' label: the colloquial treaty covers at least three structurally distinct claims — enforcement of horizontal nonproliferation (oligopoly reading), the bindingness and urgency of the disarmament obligation (reciprocal reading), and the revocability structure of obligations (this story). Each carries its own epsilon, beneficiary set, and victim set; the upstream enforcement reading is typically cited as authority in the criminalization push this story's resistance metric records. This story links both siblings; the family is complete only when all three files exist.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
