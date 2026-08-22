% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__nonproliferation_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_article_iv_vi_pairing__nonproliferation_primary, []).

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
 *   constraint_id: npt_article_iv_vi_pairing__nonproliferation_primary
 *   human_readable: NPT Article IV/VI Pairing — Nonproliferation-Primary Reading (Two-Tier Stabilized Order)
 *   domain: international_law/nuclear_governance/treaty_interpretation
 *
 * SUMMARY:
 *   This story instantiates the nonproliferation_primary reading of the NPT
 *   Article IV/VI pairing kernel: Article IV's peaceful-use entitlement is
 *   read as conditional on Article III safeguards verification, Article VI is
 *   read as an aspirational commitment lacking justiciable content, and the
 *   regime's authority is grounded in weapon states' security interest in
 *   preventing horizontal proliferation. The standing arrangement under
 *   contest — the referent for every metric here — is the operating two-tier
 *   order: five recognized arsenals outside enforcement scope, roughly 190
 *   non-weapon states bearing perpetual verified restraint, and a review
 *   process that absorbs disarmament dissent without converting it into
 *   obligation. Sibling readings (grand_bargain, abolitionist) are separate
 *   constraint files linked through the network section; their epsilon values
 *   differ because they assess the same referent through different premises,
 *   not because this constraint is measured inconsistently. KEY AGENTS (by
 *   structural relationship): - recognized_nuclear_weapon_states: Primary
 *   beneficiary and agenda-setter (institutional/arbitrage) — arsenals exempt
 *   from enforcement, control of interpretation - non_weapon_state_parties:
 *   Primary target (organized/constrained) — perpetual restraint plus
 *   verification costs - aspiring_fuel_cycle_states: Secondary target
 *   (moderate/trapped) — fuel-cycle autonomy policed - iaea_secretariat:
 *   Enforcement administrator (institutional/constrained) -
 *   umbrella_state_allies: Dual-positioned dependents
 *   (organized/identity_locked) - tpnw_coalition_states: Organized
 *   challenger, structurally discounted (organized/constrained) -
 *   de_facto_weapon_states: Outside-the-treaty comparators (powerful/mobile)
 *   - arms_control_legal_analysts: Analytical observer
 *   (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__nonproliferation_primary, 0.6).
domain_priors:suppression_score(npt_article_iv_vi_pairing__nonproliferation_primary, 0.64).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__nonproliferation_primary, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, extractiveness, 0.6).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__nonproliferation_primary, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__nonproliferation_primary, "NPT Article IV/VI Pairing — Nonproliferation-Primary Reading (Two-Tier Stabilized Order)").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__nonproliferation_primary, "international_law/nuclear_governance/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__nonproliferation_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__nonproliferation_primary, '3f876342-f784-4cf8-960f-3380c262d0b5').
narrative_ontology:cs_kernel_codification('3f876342-f784-4cf8-960f-3380c262d0b5', fixed_text).
narrative_ontology:cs_authority_grounding('3f876342-f784-4cf8-960f-3380c262d0b5', extraction).
narrative_ontology:cs_interpretation_layer_present('3f876342-f784-4cf8-960f-3380c262d0b5').
narrative_ontology:cs_reading_relation('3f876342-f784-4cf8-960f-3380c262d0b5', npt_article_iv_vi_pairing__grand_bargain, coexists_with).
narrative_ontology:cs_reading_relation('3f876342-f784-4cf8-960f-3380c262d0b5', npt_article_iv_vi_pairing__abolitionist, forecloses).
narrative_ontology:cs_axiom('3f876342-f784-4cf8-960f-3380c262d0b5', foundational, article_vi_aspirational_not_justiciable).
narrative_ontology:cs_axiom_status(article_vi_aspirational_not_justiciable, holdable).
narrative_ontology:cs_axiom_grounding('3f876342-f784-4cf8-960f-3380c262d0b5', article_vi_aspirational_not_justiciable, conventional).
narrative_ontology:cs_axiom('3f876342-f784-4cf8-960f-3380c262d0b5', foundational, peaceful_use_conditional_on_verified_safeguards).
narrative_ontology:cs_axiom_status(peaceful_use_conditional_on_verified_safeguards, holdable).
narrative_ontology:cs_axiom_grounding('3f876342-f784-4cf8-960f-3380c262d0b5', peaceful_use_conditional_on_verified_safeguards, instrumental).
narrative_ontology:cs_axiom('3f876342-f784-4cf8-960f-3380c262d0b5', secondary, weapon_state_arsenals_outside_enforcement_scope).
narrative_ontology:cs_axiom_status(weapon_state_arsenals_outside_enforcement_scope, holdable).
narrative_ontology:cs_axiom_grounding('3f876342-f784-4cf8-960f-3380c262d0b5', weapon_state_arsenals_outside_enforcement_scope, conventional).
narrative_ontology:cs_reference_frame('3f876342-f784-4cf8-960f-3380c262d0b5', stabilized_two_tier_nonproliferation_order).
narrative_ontology:cs_drift_state('3f876342-f784-4cf8-960f-3380c262d0b5', post_parallel_ban_treaty_entry_into_force, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('3f876342-f784-4cf8-960f-3380c262d0b5', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, recognized_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, non_weapon_state_parties).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, aspiring_fuel_cycle_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, non_weapon_state_parties).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, umbrella_state_allies).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, umbrella_state_allies).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__nonproliferation_primary, horizontal_proliferation_prevention_doctrine).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__nonproliferation_primary, verification_conditionality_principle).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__nonproliferation_primary, step_by_step_disarmament_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Five states whose arsenals predate the treaty and are expressly accommodated by it. They set the enforcement agenda through the Security Council and supplier cartels, fund and staff much of the verification system, and anchor interpretive outcomes in review conferences. Their own programs fall outside every verification obligation and their modernization continues throughout. Departure would cost them the legal frame that renders others' programs illegitimate while theirs go unexamined, so they remain indefinitely while controlling what the treaty means.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, recognized_nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__nonproliferation_primary, recognized_nuclear_weapon_states, beneficiary).

% Administers Article III verification: designs safeguards, conducts inspections, and reports non-compliance upward to bodies that decide consequences. Its mandate and budget grow with each enforcement episode, and its findings carry the weight that triggers sanctions. It cannot decline missions or redefine its mandate independently of member-state politics.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, iaea_secretariat, agenda_setter,
    institutional, generational, constrained, global).

% Roughly 190 states that forswore weapons and submit facilities to inspection. They pay verification costs, accept facility intrusion, and forgo the weapons option permanently; in return they receive the verification commons and peaceful-commerce access, both experienced as shrinking whenever supplier rules tighten. Formal exit exists under Article X, but its single user became a pariah under universal sanctions, pricing departure prohibitively.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, non_weapon_state_parties, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__nonproliferation_primary, non_weapon_state_parties, beneficiary).

% States pursuing enrichment or reprocessing capability under safeguards. They bear the tightest conditionality: supplier denials, case-by-case fuel guarantees, and, where defiance occurs, layered sanctions. Their programs serve as the regime's principal exhibits, and each enforcement precedent worsens their bargaining position. Withdrawal or defiance carries costs their economies cannot absorb.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, aspiring_fuel_cycle_states, payer,
    moderate, biographical, trapped, regional).

% Allies sheltered by weapon-state security guarantees. They gain protection without ownership and pay by hosting arrangements, endorsing the two-tier logic in diplomatic forums, and forgoing indigenous options their industrial base would permit. Their security planning is fused with the guarantee; dissolving that fusion would mean rebuilding national defense identity from its foundations.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, umbrella_state_allies, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__nonproliferation_primary, umbrella_state_allies, payer).

% States that built a parallel prohibition treaty on humanitarian-consequence grounds. Within this reading's framework their instrument is discounted as non-binding and its sponsors treated as outside the operative architecture; they continue attending review conferences where their position is recorded and outvoted. Their alternative exists but commands no weapon-state participation, so it gives voice without leverage.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, tpnw_coalition_states, excluded,
    organized, generational, constrained, global).

% Three armed states outside the treaty entirely. Supplier rules and diplomacy touch them, but its obligations and inspections do not; one received a supplier-cartel waiver opening civilian trade despite its arsenal. They demonstrate that the restraint path is not the only path, which the regime manages through isolation rather than incorporation.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, de_facto_weapon_states, excluded,
    powerful, generational, mobile, global).

% Scholars, former negotiators, and practitioners who map the treaty's interpretive contests. They publish the negotiating histories and doctrinal analyses that every seat cites; they hold no enforcement power and collect nothing from the arrangement.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, arms_control_legal_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_article_iv_vi_pairing__nonproliferation_primary, recognized_nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_article_iv_vi_pairing__nonproliferation_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the proliferation-cascade collective-action problem: centralized verification makes restraint observable, lowering the insecurity that drives each state to arm preemptively, while supplier governance channels peaceful nuclear commerce so that cooperation does not double as weapons acquisition.
% TRANSFER_FUNCTION: Moves restraint costs — foregone weapons options, intrusive inspection, restricted fuel-cycle autonomy, verification funding — from non-weapon states into a common security pool administered under weapon-state-weighted governance; moves security dividends — verified neighbor compliance, arsenal legitimacy, immunity from disarmament enforcement — to the recognized weapon states.
% ABSENT_VOICES: De facto weapon states are absent from the table their behavior polices; test-affected communities and humanitarian-consequence witnesses enter only through the discounted parallel-treaty channel; non-weapon-state fuel-cycle-equity dissent is diluted inside bloc-consensus formats. Within this reading's framework each of these objections is classified as outside the binding architecture rather than answered on the merits.
% DISAPPEARANCE_RATIONALE: Overnight disappearance removes the verification commons and the supplier regime: latent-capability states across the Middle East, Northeast Asia, and elsewhere face immediate neighborhood insecurity, several would hedge or sprint within years, peaceful nuclear commerce would revert to ad hoc bilateral arrangements, and weapon states would lose the legal instrument that frames others' programs as illegitimate while their own remain unexamined.
% FOUNDING_PROBLEM: At drafting (1965-68), the prevailing forecast was dozens of nuclear-armed states within two decades; the problem was freezing the weapons club before the cascade made verification and crisis stability impossible, using guaranteed peaceful-use access as the inducement for restraint.
% FOUNDING_PROBLEM_CORROBORATION: Weapon states and the verification agency attest that horizontal risk remains live (the DPRK program, Iranian enrichment, the A.Q. Khan network's history) — but they are the benefiting parties. Outside corroboration: independent arsenal audits and safeguards-implementation reporting document persistent horizontal risk, while the humanitarian-initiative evidence base and the parallel ban treaty's negotiating records attest, from outside the beneficiary set, that the operative problem has shifted to the consequences of existing arsenals — supporting 'contested' rather than 'live'.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__nonproliferation_primary, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__nonproliferation_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__nonproliferation_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__nonproliferation_primary, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__nonproliferation_primary, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__nonproliferation_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__nonproliferation_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_article_iv_vi_pairing__nonproliferation_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.60: the flows are real and conceded even within this reading — non-weapon states pay restraint and verification in perpetuity while weapon-state arsenals sit outside every obligation — but the reading holds much of the burden to be justified security coordination, so epsilon sits at hybrid rather than confiscatory levels. Suppression 0.64: persistence depends on actively deployed machinery (supplier-cartel denial, sanctions webs, interdiction practice, Security-Council referral) aimed at defectors and challengers; the broad membership complies largely willingly, so suppression is targeted rather than pervasive. Theater 0.48: the safeguards function is genuinely functional, but a growing share of activity is review-cycle performance — consensus documents, pillar language, action plans — that simulates balance while changing nothing binding. Accessibility_collapse 0.50: alternatives persist (parallel ban treaty, latency hedging, Article X withdrawal) but each is costly or discounted by the enforcement seats. Resistance 0.62: organized bloc resistance, a parallel treaty with 70+ adherents, and open defiance by exit and threshold states. All three tracked metrics run on ONE shared eight-point grid (1968-2025), each metric authored at every point. The series oscillates around a rising trend: periodic procedural concessions (1995 Principles and Objectives, 2000 Thirteen Steps, 2010 Action Plan) briefly lower measured extraction and raise theater, then stall — the concession-and-stall cycle is itself part of the maintenance mechanism, absorbing non-weapon-state discontent without structural change, an intermittent-reinforcement pattern rather than noise. Base properties are authored at the interval-end phase of the latest cycle.
 *
 * PERSPECTIVAL GAP:
 *   From the weapon-state seat the arrangement is successful security architecture that its owners fund, staff, and interpret; from the non-weapon-state seat it is perpetual restraint with an unenforceable quid; from the aspiring-fuel-cycle seat it is discriminatory gating — the sharpest single exhibit being the supplier-cartel waiver opened for one non-party arsenal while a compliant state's program drew layered sanctions. Same-nominal-class divergence is equally sharp: umbrella allies and parallel-treaty coalition states hold identical treaty status yet opposite relationships to the enforcement machinery, differentiated entirely by exit structure (identity fusion with the guarantee versus a costly but real alternative channel). The engine computes these per-seat divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   recognized_nuclear_weapon_states derive near the beneficiary pole: they collect the security dividend, control interpretation, and bear no enforcement exposure (d near 0.0). non_weapon_state_parties derive near the target pole: they pay the transfer with constrained exit (d well above 0.5). aspiring_fuel_cycle_states sit nearest the full-target end: trapped, sanctions-exposed, and the regime's principal objects. iaea_secretariat sits mildly beneficiary-side as administrator collecting mandate and funding, damped by its constrained position. umbrella_state_allies are pulled back toward the target side by identity lock despite formal protection. tpnw_coalition_states sit moderately target-side with a partial alternative reducing trap depth. de_facto_weapon_states sit outside the constraint's pull entirely — mobile, never bound. No directionality_overrides were needed: the beneficiary/victim declarations plus exit atoms reproduce these positions without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a hybrid rather than pure extraction preserves the genuine coordination achievement — cascade prevention and the verification commons — that a pure-extraction reading would erase; refusing the pure-coordination label preserves the asymmetric flows the reading itself concedes in its own structural delta ('non-weapon states as perpetual restraint-bearers'). The reading's distinctive move — declaring Article VI non-justiciable — is precisely what prevents the hybrid from resolving in either direction: it blocks the reciprocity mechanism (the grand_bargain sibling) that would convert restraint into a bargained, callable debt, and it blocks the prohibition mechanism (the abolitionist sibling) that would dissolve the tiers altogether. Mandatrophy is not resolved: the founding problem remains contested-live, so the arrangement retains function. But the 1995 indefinite extension removed the arrangement's built-in reassessment point — the place a sunset clause would have lived — which is why the hybrid has hardened rather than transitioned, and why the theater ratio climbs while the extraction trend rises.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_governance_question,
    'Which reading of the npt_article_iv_vi_pairing kernel governs the treaty''s operative meaning — this nonproliferation_primary instantiation, the grand_bargain reciprocity reading, or the abolitionist mandate reading?',
    'Track Review Conference outcome documents, any judicial or arbitral treatment of Article VI, and enforcement practice: whichever reading''s premises operationalize in binding practice becomes governing.',
    'Wholesale reclassification: under grand_bargain the arrangement computes as a breached reciprocal exchange with weapon states as defaulting debtors; under abolitionist it computes as a rights-violating regime maintaining prohibited arsenals; this file''s two-tier-stabilization profile holds only under the present reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_governance_question, conceptual, 'Committer structure: this story is one reading of a contested kernel; sibling readings are separate constraints, not hedges inside this one.').

omega_variable(
    article_vi_justiciability_status,
    'Is Article VI genuinely non-justiciable (negotiating-record ''ultimate goal'' language, no timeline or standard), or has subsequent law — the 1996 ICJ unanimous paragraph, humanitarian-consequence jurisprudence, accumulating ban-norm practice — converted it into an enforceable obligation?',
    'Doctrinal analysis of any forum willing to hear an Article VI claim, plus state-practice and opinio juris surveys distinguishing acceptance of obligation from political endorsement.',
    'If justiciable, this reading''s foundational axiom fails, the perpetual-restraint structure loses its legal keystone, and the constraint migrates toward the grand_bargain profile with enforceable reciprocity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_justiciability_status, conceptual, 'Legal status of Article VI: aspiration versus enforceable obligation.').

omega_variable(
    two_tier_permanence_vs_transition,
    'Is the two-tier order this reading stabilizes a permanent equilibrium (as the reading holds) or a transitional arrangement whose justification decays for as long as disarmament stalls?',
    'Longitudinal comparison of Article VI implementation across review cycles against the stated rationale of the 1995 indefinite extension; successive cycles without enforceable progress decay the transition justification measurably.',
    'A permanent-order finding supports the present hybrid classification; a transitional finding pushes toward expired-transition dynamics and strengthens capture and zombie-function flags.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(two_tier_permanence_vs_transition, empirical, 'Whether the stabilized hierarchy is terminal or transitional.').

omega_variable(
    conditionality_efficacy_vs_underground_substitution,
    'Does conditioning Article IV access on Article III verification actually reduce diversion risk, or does it push sensitive programs outside inspection (the DPRK trajectory) while denying compliant states the bargain''s promised benefit?',
    'Comparative case analysis of safeguarded versus unsafeguarded program trajectories and of compliant states'' fuel-cycle outcomes under supplier-denial episodes.',
    'If conditionality drives defection, part of the measured suppression is counterproductive enforcement that manufactures the violations it punishes, raising effective extraction and pushing the profile toward pure extraction; if it works, more of the burden is genuine coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditionality_efficacy_vs_underground_substitution, empirical, 'Efficacy of verification conditionality versus substitution into covert programs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__nonproliferation_primary, 1968, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_np_primary_tr_t1968, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 1968, 0.18).
narrative_ontology:measurement_basis(npt_np_primary_tr_t1968, observed).
narrative_ontology:measurement(npt_np_primary_tr_t1978, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 1978, 0.24).
narrative_ontology:measurement_basis(npt_np_primary_tr_t1978, observed).
narrative_ontology:measurement(npt_np_primary_tr_t1988, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 1988, 0.27).
narrative_ontology:measurement_basis(npt_np_primary_tr_t1988, observed).
narrative_ontology:measurement(npt_np_primary_tr_t1995, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 1995, 0.35).
narrative_ontology:measurement_basis(npt_np_primary_tr_t1995, observed).
narrative_ontology:measurement(npt_np_primary_tr_t2003, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2003, 0.32).
narrative_ontology:measurement_basis(npt_np_primary_tr_t2003, observed).
narrative_ontology:measurement(npt_np_primary_tr_t2010, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2010, 0.37).
narrative_ontology:measurement_basis(npt_np_primary_tr_t2010, observed).
narrative_ontology:measurement(npt_np_primary_tr_t2017, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2017, 0.43).
narrative_ontology:measurement_basis(npt_np_primary_tr_t2017, observed).
narrative_ontology:measurement(npt_np_primary_tr_t2025, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2025, 0.48).
narrative_ontology:measurement_basis(npt_np_primary_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(npt_np_primary_be_t1968, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 1968, 0.36).
narrative_ontology:measurement_basis(npt_np_primary_be_t1968, observed).
narrative_ontology:measurement(npt_np_primary_be_t1978, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 1978, 0.4).
narrative_ontology:measurement_basis(npt_np_primary_be_t1978, observed).
narrative_ontology:measurement(npt_np_primary_be_t1988, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 1988, 0.44).
narrative_ontology:measurement_basis(npt_np_primary_be_t1988, observed).
narrative_ontology:measurement(npt_np_primary_be_t1995, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 1995, 0.5).
narrative_ontology:measurement_basis(npt_np_primary_be_t1995, observed).
narrative_ontology:measurement(npt_np_primary_be_t2003, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2003, 0.56).
narrative_ontology:measurement_basis(npt_np_primary_be_t2003, observed).
narrative_ontology:measurement(npt_np_primary_be_t2010, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2010, 0.53).
narrative_ontology:measurement_basis(npt_np_primary_be_t2010, observed).
narrative_ontology:measurement(npt_np_primary_be_t2017, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2017, 0.58).
narrative_ontology:measurement_basis(npt_np_primary_be_t2017, observed).
narrative_ontology:measurement(npt_np_primary_be_t2025, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2025, 0.6).
narrative_ontology:measurement_basis(npt_np_primary_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(npt_np_primary_su_t1968, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 1968, 0.3).
narrative_ontology:measurement_basis(npt_np_primary_su_t1968, observed).
narrative_ontology:measurement(npt_np_primary_su_t1978, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 1978, 0.38).
narrative_ontology:measurement_basis(npt_np_primary_su_t1978, observed).
narrative_ontology:measurement(npt_np_primary_su_t1988, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 1988, 0.34).
narrative_ontology:measurement_basis(npt_np_primary_su_t1988, observed).
narrative_ontology:measurement(npt_np_primary_su_t1995, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 1995, 0.46).
narrative_ontology:measurement_basis(npt_np_primary_su_t1995, observed).
narrative_ontology:measurement(npt_np_primary_su_t2003, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2003, 0.62).
narrative_ontology:measurement_basis(npt_np_primary_su_t2003, observed).
narrative_ontology:measurement(npt_np_primary_su_t2010, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2010, 0.56).
narrative_ontology:measurement_basis(npt_np_primary_su_t2010, observed).
narrative_ontology:measurement(npt_np_primary_su_t2017, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2017, 0.59).
narrative_ontology:measurement_basis(npt_np_primary_su_t2017, observed).
narrative_ontology:measurement(npt_np_primary_su_t2025, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2025, 0.64).
narrative_ontology:measurement_basis(npt_np_primary_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__nonproliferation_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing__grand_bargain).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing__abolitionist).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, tpnw_humanitarian_ban_regime).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, iaea_safeguards_verification_system).

% DUAL FORMULATION NOTE:
% Constraint family: the single treaty text pairing peaceful-use access (Article IV) with a disarmament commitment (Article VI) decomposes into three structurally distinct constraints — one per reading of the kernel. This file instantiates the nonproliferation_primary reading; the grand_bargain and abolitionist readings are separate stories with their own epsilon values, beneficiary structures, and classifications. The decomposition follows the epsilon-invariance principle: measuring the arrangement through this reading's observables (enforcement practice, justiciability doctrine) yields a different and more stable epsilon than measuring through the reciprocity or humanitarian-law observables, so they are not one constraint viewed from angles but different constraints. Upstream/downstream structure: this reading is the currently operative one and exerts structural pressure on the grand_bargain sibling (every round of hardened horizontal enforcement without vertical reciprocation raises the salience and the strain of the bargain frame), while logically excluding the abolitionist sibling within any single framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
