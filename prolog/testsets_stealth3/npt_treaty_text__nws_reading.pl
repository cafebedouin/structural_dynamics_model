% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__nws_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   human_readable: NPT Regime Under the Possessor-State Reading: Binding Restraint Below, Aspirational Disarmament Above
 *   domain: international_law/arms_control/treaty_interpretation
 *
 * SUMMARY:
 *   The Nuclear Non-Proliferation Treaty binds non-possessor states to
 *   verified restraint while committing possessor states to disarmament 'at
 *   an early date', a phrase with no deadline, no benchmark, and no
 *   enforcement mechanism. Under the reading instantiated here, the operative
 *   constraint is the binding half: comprehensive safeguards, supply-side
 *   discipline, and sanctions exposure fall on the non-possessors, while the
 *   disarmament half functions as diplomatic atmosphere. The verification
 *   economy confirms the asymmetry: possessor states submit to no
 *   comprehensive safeguards at all, so the entire IAEA verification
 *   apparatus, funded substantially by the very states it scrutinizes,
 *   monitors horizontal proliferation only. KEY AGENTS (by structural
 *   relationship): nuclear_weapon_states: primary beneficiary and de facto
 *   agenda-setter (institutional/arbitrage) — control interpretation, bear no
 *   symmetric verification; non_nuclear_weapon_states: primary target
 *   (organized/trapped) — bear binding audited obligations; iaea_secretariat:
 *   institutional collector (institutional/identity_locked) — budget and
 *   mandate grow with horizontal verification; extended_deterrence_allies:
 *   secondary beneficiaries (powerful/constrained);
 *   humanitarian_disarmament_coalition: excluded challenger
 *   (organized/mobile); nonparty_threshold_states: excluded outsiders
 *   (powerful/arbitrage); arms_control_verification_community: analytical
 *   observer (analytical/analytical). This file is one reading of the
 *   npt_treaty_text kernel; the claim/metrics pair is authored independently
 *   per corpus rules.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nws_reading, 0.74).
domain_priors:suppression_score(npt_treaty_text__nws_reading, 0.68).
domain_priors:theater_ratio(npt_treaty_text__nws_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nws_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__nws_reading, "NPT Regime Under the Possessor-State Reading: Binding Restraint Below, Aspirational Disarmament Above").
narrative_ontology:topic_domain(npt_treaty_text__nws_reading, "international_law/arms_control/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_treaty_text__nws_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nws_reading, '790c0ada-bcc4-4452-8747-5ec3b34154b9').
narrative_ontology:cs_kernel_codification('790c0ada-bcc4-4452-8747-5ec3b34154b9', fixed_text).
narrative_ontology:cs_authority_grounding('790c0ada-bcc4-4452-8747-5ec3b34154b9', extraction).
narrative_ontology:cs_interpretation_layer_present('790c0ada-bcc4-4452-8747-5ec3b34154b9').
narrative_ontology:cs_reading_relation('790c0ada-bcc4-4452-8747-5ec3b34154b9', npt_treaty_text__nnws_reading, coexists_with).
narrative_ontology:cs_reading_relation('790c0ada-bcc4-4452-8747-5ec3b34154b9', npt_treaty_text__withdrawal_threshold_reading, influences).
narrative_ontology:cs_axiom('790c0ada-bcc4-4452-8747-5ec3b34154b9', foundational, article_vi_political_not_justiciable_obligation).
narrative_ontology:cs_axiom_status(article_vi_political_not_justiciable_obligation, holdable).
narrative_ontology:cs_axiom_grounding('790c0ada-bcc4-4452-8747-5ec3b34154b9', article_vi_political_not_justiciable_obligation, conventional).
narrative_ontology:cs_axiom('790c0ada-bcc4-4452-8747-5ec3b34154b9', foundational, nuclear_deterrence_preserves_strategic_stability).
narrative_ontology:cs_axiom_status(nuclear_deterrence_preserves_strategic_stability, holdable).
narrative_ontology:cs_axiom_grounding('790c0ada-bcc4-4452-8747-5ec3b34154b9', nuclear_deterrence_preserves_strategic_stability, empirically_contingent).
narrative_ontology:cs_reference_frame('790c0ada-bcc4-4452-8747-5ec3b34154b9', horizontal_nonproliferation_compact).
narrative_ontology:cs_drift_state('790c0ada-bcc4-4452-8747-5ec3b34154b9', contemporary_post_tpnw_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('790c0ada-bcc4-4452-8747-5ec3b34154b9', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__nws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, extended_deterrence_allies).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, iaea_secretariat).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_vindicates(npt_treaty_text__nws_reading, nuclear_deterrence_stability_doctrine).
narrative_ontology:constraint_vindicates(npt_treaty_text__nws_reading, horizontal_proliferation_priority_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Five states that possessed nuclear weapons when the treaty opened for signature. They accept no comprehensive verification of their own arsenals or fissile stocks, publish voluntary transparency figures, and decide through shared practice what phrases like 'at an early date' require of them. They extend security assurances to allies, maintain and modernize their arsenals, and hold veto power over Security Council enforcement directed at themselves.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nws_reading, nuclear_weapon_states, beneficiary).

% States that forswore acquisition and accept comprehensive safeguards: facility design information, inspector access, materials accounting, and for many the Additional Protocol. They fund a large share of the IAEA budget through assessed contributions, face supply-side restrictions coordinated by supplier states, and if they withdraw they inherit immediate sanctions exposure. In exchange they hold an unenforceable pledge that the possessor states will pursue disarmament.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, non_nuclear_weapon_states, payer,
    organized, generational, trapped, global).

% Implements the verification mandate: inspects facilities, accounts for materials, reports noncompliance to the Board of Governors. Its safeguards budget grows with every new verification task, nearly all of it directed at non-possessor states' programs. Its organizational self-conception is fused with the safeguards mission, and its directorate answers to a board where possessor states hold permanent weight.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, iaea_secretariat, beneficiary,
    institutional, generational, identity_locked, global).

% Industrial democracies under United States-led nuclear umbrellas. They forswear indigenous arsenals, host or support forward-deployed capabilities, and receive protection they could not cheaply replicate. Leaving the umbrella would mean either acquiring weapons, breaking the taboo and inviting regional cascade, or accepting strategic exposure.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, extended_deterrence_allies, beneficiary,
    powerful, biographical, constrained, regional).

% A cross-regional bloc of mostly small and middle powers that negotiated a prohibition treaty outside this regime after three humanitarian-consequences conferences. Possessor states boycott its meetings and press allies to stay away. Their preferred framework treats possession itself as the danger; they are not seated in the Review Conference consensus process they seek to reform.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, humanitarian_disarmament_coalition, excluded,
    organized, biographical, mobile, global).

% Three states that never joined: two tested arsenals outside the treaty, one undeclared. They live under supplier-country exceptions or penalties shaped by the regime's rules but had no seat writing them. Each argues the bargain codifies a permanent caste distinction between lawful and unlawful possession.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, nonparty_threshold_states, excluded,
    powerful, generational, arbitrage, regional).

% Technical specialists, former inspectors, and academic analysts who track compliance data, budget lines, and negotiating records. They observe the full structure, including who is verified, who verifies, and what the phrase 'at an early date' has been made to mean in practice, and publish outside any party's delegation.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, arms_control_verification_community, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_text__nws_reading, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_treaty_text__nws_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a real collective-action problem: without a common restraint-and-verification standard, neighboring states' weapons programs trigger security-dilemma cascades. The treaty supplies a shared rulebook, forswear acquisition, accept inspection, gain lawful access to civil nuclear trade, that lowers mutual suspicion among dozens of technically capable states at once.
% TRANSFER_FUNCTION: Moves verification burden, intrusive inspection rights, and a large share of the IAEA's funding from non-possessor states to the regime; moves discretionary security assurances and controlled technology access from possessor states to non-possessors. The obligations running toward non-possessors carry enforcement; the corresponding disarmament pledge running toward possessors carries none.
% ABSENT_VOICES: Nonparty threshold states never consented to the rules that now govern their trade and status; the humanitarian-disarmament coalition is locked out of the consensus process and answered with boycott rather than argument. Both would insist the bargain's terms were set exclusively by the parties who benefit from them.
% DISAPPEARANCE_RATIONALE: Civil-nuclear commerce would lose its governance layer overnight; several technically capable states would face immediate domestic pressure to hedge or weaponize; alliance architectures built on extended deterrence would renegotiate; the supplier cartel would fracture. Rearrangement, not continuity.
% FOUNDING_PROBLEM: Early-1960s intelligence projected fifteen to twenty-five nuclear states within two decades; the possessors sought to freeze the club at five while offering non-possessors peaceful technology and a disarmament promise sufficient to make restraint look temporary.
% FOUNDING_PROBLEM_CORROBORATION: Outside the possessor set: IAEA reporting and independent analyses (SIPRI, Federation of American Scientists) corroborate that runaway spread was a real projected problem and that diffusion ran far slower than forecast after entry into force. The humanitarian-disarmament coalition and much of the Non-Aligned Movement attest from outside that the founding problem as originally framed is superseded, since in their account the operative danger is possession itself rather than further possession. No external source attests the possessor framing intact.
narrative_ontology:disappearance_verdict(npt_treaty_text__nws_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__nws_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__nws_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
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
 *   Extraction is high (0.74 at interval end) because the arrangement's burdens and exemptions are decoupled from any performance standard: one side's compliance is audited continuously, the other side's pledge has never once been benchmarked. Suppression (0.68) is authored as raw structural property, unscaled by scope or power per corpus rules: it reflects supply-cartel discipline, Additional Protocol reach, and the sanctions price of Article X exit, tempered by the formal sovereignty the treaty leaves intact. Theater ratio (0.45) splits the difference between a substantively functional verification machine and an increasingly performative Review Conference cycle whose consensus documents have lost operational content. Accessibility collapse (0.55): alternatives persist but at heavy cost — withdrawal is survivable (one state proved it) yet ruinous, and latency hedging is viable for advanced industrial states. Resistance (0.62): sustained non-possessor coalition pressure culminating in a rival prohibition treaty adopted by a large minority of states. The temporal series run on one shared eight-point grid; the dip at t=16 (mid-1980s superpower summits and the INF negotiation) is a partial-cycle oscillation driven by external detente factors, not intermittent reinforcement — the underlying ratchet resumes at t=24 and steepens after indefinite extension in 1995 removed the last scheduled occasion on which the possessors' half of the bargain could be repriced.
 *
 * PERSPECTIVAL GAP:
 *   From the possessor seat the arrangement computes as a functioning compact honored in letter: signatures given, diplomacy funded, bilateral reduction treaties negotiated and (until recently) observed. From the non-possessor seat the same structure computes as enforced asymmetry: audited duties here, unaudited promises there, and the auditor's budget paid by the audited. The engine derives both per-seat classifications from the same structural data; the authored claim referees nothing. Identity-lock note: the IAEA secretariat's seat is stabilized by institutional identity fusion — the organization has become its verification function, so its exit option is nominal even though its leadership periodically documents the budget asymmetry itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Possessor states sit at the beneficiary pole: they collect interpretive control over the disarmament clause, continued legitimacy for retained arsenals, and allies' restraint, while contributing no symmetric verification surface (derived d near 0.0). Non-possessor states sit at the target pole: binding duties, intrusive inspection, funding share, and sanctions exposure on exit (derived d near 1.0). Extended-deterrence allies collect protection below replacement cost and pay a restraint they would likely choose anyway — moderately beneficiary-side. The IAEA secretariat collects budget and mandate growth concentrated on horizontal verification — beneficiary-side with identity lock. The two excluded seats fall outside the beneficiary/victim arrays; their exclusion is itself part of what the enforcement machinery maintains, which is why they register as absent voices rather than derived directionalities.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification keeps two symmetrical errors apart. Reading the arrangement as pure extraction erases the genuine coordination achievement: measured against 1960s forecasts, diffusion slowed dramatically, and no non-possessor government proposes abolishing the verification standard. Reading it as pure coordination erases the asymmetry: one side's obligations are audited annually, the other side's are rhetorical. On the genealogy interview, the founding problem (runaway spread) is contested rather than dead — the arrangement persists on terms whose center of gravity has migrated from preventing new possessors to legitimating old ones. The mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges: the world does depend on the arrangement, but the justification offered for it has drifted from the founding bargain, and the fix (amendment under Article VIII, which requires assent including every possessor) is priced beyond any actor's willingness — hence fixing_cost=prohibitive with capture located at the possessor seat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_justiciability_status,
    'Is Article VI''s disarmament pledge a binding legal obligation whose breach is cognizable (the nnws_reading of the npt_treaty_text kernel), or a political aspiration without enforcement (this reading)?',
    'Authoritative adjudication or a Vienna Convention object-and-purpose review joined by state-practice records: whether any possessor state has ever accepted justiciability of the pledge, and whether non-possessor protests have ever taken breach-form rather than grievance-form.',
    'If binding, the arrangement converts toward pure extraction — an enforceable-duty/never-enforced-duty pair with identifiable victims; if aspirational, the tangled-rope reading stands and remedies are political rather than legal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_justiciability_status, conceptual, 'Kernel contest: legal status of the Article VI pledge across readings.').

omega_variable(
    interpretive_control_concentration,
    'Does interpretation of ''at an early date'' rest with the possessor states as a practical matter, or is it genuinely distributed through Review Conference consensus?',
    'Code two decades of outcome documents and national statements: trace which proposed benchmarks survived, which deletions possessor delegations demanded, and whether consensus has ever imposed a dated milestone.',
    'Concentrated control confirms the extraction channel this story models; distributed control shifts weight toward the coordination reading and lowers effective extraction at the possessor seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretive_control_concentration, empirical, 'Whether interpretive authority over the disarmament clause is captured.').

omega_variable(
    safeguards_budget_horizontal_concentration,
    'What fraction of IAEA verification resources monitors non-possessor programs versus possessor fissile-material and warhead activity?',
    'Line-item analysis of IAEA regular-budget and extra-budgetary verification spending; possessor states submit no comprehensive safeguards agreements, so the denominator for vertical monitoring is near zero by design.',
    'Near-total horizontal concentration supports the high-extraction profile; any material vertical component would soften the asymmetry claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(safeguards_budget_horizontal_concentration, empirical, 'Budget-level confirmation that verification burden falls on the non-possessors.').

omega_variable(
    restraint_attribution_confound,
    'How much observed non-spread is attributable to this arrangement rather than to confounders — technological barriers, bilateral security guarantees, alliance discipline?',
    'Compare states outside the treaty with comparable capability and no security umbrella against inside-the-treaty trajectories; use withdrawal events as natural experiments.',
    'If confounders explain most restraint, the coordination function is weaker than claimed and the arrangement''s persistence rests more heavily on enforcement and inertia than on solved collective action.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restraint_attribution_confound, empirical, 'Counterfactual attribution of non-proliferation outcomes.').

omega_variable(
    cs_framing_text_vs_practice,
    'Should the kernel be framed as the fixed treaty text (declared here) or as the possessor states'' informal interpretive practice layered above the text — and does the choice change the commitment-system classification?',
    'Test both framings against the drift record: if the text''s own amendment machinery (Article VIII) has never once operated while practice has repeatedly redefined the operative clauses, the practice-layer framing is the better descriptor.',
    'Under the practice-layer framing, kernel_codification moves toward implicit while authority_grounding remains extraction; the reading-relations structure is unchanged but drift magnitude reads higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_text_vs_practice, conceptual, 'Framing under-determination: text-kernel versus practice-kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nws_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_treaty_text__nws_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(npt__tr_t8, npt_treaty_text__nws_reading, theater_ratio, 8, 0.23).
narrative_ontology:measurement(npt__tr_t16, npt_treaty_text__nws_reading, theater_ratio, 16, 0.17).
narrative_ontology:measurement(npt__tr_t24, npt_treaty_text__nws_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(npt__tr_t32, npt_treaty_text__nws_reading, theater_ratio, 32, 0.31).
narrative_ontology:measurement(npt__tr_t40, npt_treaty_text__nws_reading, theater_ratio, 40, 0.36).
narrative_ontology:measurement(npt__tr_t48, npt_treaty_text__nws_reading, theater_ratio, 48, 0.42).
narrative_ontology:measurement(npt__tr_t55, npt_treaty_text__nws_reading, theater_ratio, 55, 0.45).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_treaty_text__nws_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(npt__be_t8, npt_treaty_text__nws_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(npt__be_t16, npt_treaty_text__nws_reading, base_extractiveness, 16, 0.44).
narrative_ontology:measurement(npt__be_t24, npt_treaty_text__nws_reading, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(npt__be_t32, npt_treaty_text__nws_reading, base_extractiveness, 32, 0.61).
narrative_ontology:measurement(npt__be_t40, npt_treaty_text__nws_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(npt__be_t48, npt_treaty_text__nws_reading, base_extractiveness, 48, 0.71).
narrative_ontology:measurement(npt__be_t55, npt_treaty_text__nws_reading, base_extractiveness, 55, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_treaty_text__nws_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(npt__su_t8, npt_treaty_text__nws_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(npt__su_t16, npt_treaty_text__nws_reading, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(npt__su_t24, npt_treaty_text__nws_reading, suppression_requirement, 24, 0.56).
narrative_ontology:measurement(npt__su_t32, npt_treaty_text__nws_reading, suppression_requirement, 32, 0.59).
narrative_ontology:measurement(npt__su_t40, npt_treaty_text__nws_reading, suppression_requirement, 40, 0.63).
narrative_ontology:measurement(npt__su_t48, npt_treaty_text__nws_reading, suppression_requirement, 48, 0.66).
narrative_ontology:measurement(npt__su_t55, npt_treaty_text__nws_reading, suppression_requirement, 55, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__nws_reading, resource_allocation).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, npt_treaty_text__nnws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, npt_treaty_text__withdrawal_threshold_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the NPT bargain' conflates at least three structurally distinct claims: the legal status of Article VI (this file versus nnws_reading), the exit architecture (withdrawal_threshold_reading), and the verification-burden allocation (authored here as the extraction channel). Each claim gets its own epsilon, beneficiaries, and classification; the files link through network edges. The possessor reading is upstream: its entrenchment sets the legitimacy conditions and resource flows within which the other two readings contend.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
