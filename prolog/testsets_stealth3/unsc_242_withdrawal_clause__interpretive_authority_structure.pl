% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__interpretive_authority_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__interpretive_authority_structure, []).

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
 *   constraint_id: unsc_242_withdrawal_clause__interpretive_authority_structure
 *   human_readable: UNSC Resolution 242 Withdrawal Clause — Contested Interpretive Authority Structure
 *   domain: international law/diplomatic history/treaty interpretation
 *
 * SUMMARY:
 *   Security Council Resolution 242's withdrawal paragraph — 'withdrawal of
 *   Israeli armed forces from territories occupied in the recent conflict' —
 *   exists in two divergent authoritative language versions whose difference
 *   (French definite article versus English phrasing) determines whether full
 *   or partial withdrawal is required. Fifty-eight years of diplomacy have
 *   produced no definitive determination of the text's meaning. This story
 *   models the reason: the authority to decide is itself contested. The
 *   International Court of Justice claims judicial interpretation; the
 *   drafting governments claim authorial intent; the occupying state claims
 *   customary state practice and refuses every path that would submit its
 *   reading to an outside decision; the veto-holding patron ensures no
 *   binding Council determination occurs. Per the epsilon-invariance
 *   principle, the colloquial question 'what does the withdrawal clause
 *   require?' decomposes into three structurally distinct constraints: the
 *   maximal reading, the partial reading, and this one — the
 *   interpretive-authority structure. The substantive readings concern what
 *   is owed; this one concerns who may say. Their epsilon values differ
 *   accordingly, and the family is linked through
 *   network.affects_constraints. The claim and metrics are independent
 *   authored facts: the snare claim rests on structure (a coordination cover
 *   story, enforcement-dependent persistence, identifiable victims), while
 *   the metric values describe the arrangement's observed operation.
 *
 * KEY AGENTS:
 *   - occupying_state: primary beneficiary (powerful/constrained) — maintains its preferred reading through non-cooperation; collects continued control of the disputed territory
 *   - veto_holding_patron: secondary beneficiary (institutional/arbitrage) — veto blocks any binding determination; broker monopoly over every negotiation round
 *   - text_drafting_states: tertiary beneficiaries (institutional/constrained) — authorial-intent claim grants permanent standing as arbiters of the texts they wrote
 *   - international_court_of_justice: blocked resolver (institutional/constrained) — claims judicial authority, never granted a binding referral on this clause
 *   - occupied_population: primary target (powerless/trapped) — bears continued occupation for as long as the meaning stays open
 *   - arab_demand_states: secondary targets (organized/constrained) — seek definitive legal closure; pay in stalled relations and periodic war
 *   - general_assembly_majority: closure-seeking coalition (organized/constrained) — annual non-binding reaffirmation cycle
 *   - international_law_community: analytical observer (moderate/analytical) — sees the full structure from outside the bargaining
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.78).
domain_priors:suppression_score(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.66).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, extractiveness, 0.78).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__interpretive_authority_structure, snare).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__interpretive_authority_structure, "UNSC Resolution 242 Withdrawal Clause — Contested Interpretive Authority Structure").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__interpretive_authority_structure, "international law/diplomatic history/treaty interpretation").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__interpretive_authority_structure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__interpretive_authority_structure, 'd68375dc-b6b8-490a-b4ee-d000a774ac43').
narrative_ontology:cs_kernel_codification('d68375dc-b6b8-490a-b4ee-d000a774ac43', fixed_text).
narrative_ontology:cs_authority_grounding('d68375dc-b6b8-490a-b4ee-d000a774ac43', distributed).
narrative_ontology:cs_reading_relation('d68375dc-b6b8-490a-b4ee-d000a774ac43', unsc_242_withdrawal_clause__maximal_withdrawal_reading, influences).
narrative_ontology:cs_reading_relation('d68375dc-b6b8-490a-b4ee-d000a774ac43', unsc_242_withdrawal_clause__partial_withdrawal_reading, influences).
narrative_ontology:cs_axiom('d68375dc-b6b8-490a-b4ee-d000a774ac43', foundational, no_recognized_single_arbiter_exists_for_the_clause).
narrative_ontology:cs_axiom_status(no_recognized_single_arbiter_exists_for_the_clause, holdable).
narrative_ontology:cs_axiom_grounding('d68375dc-b6b8-490a-b4ee-d000a774ac43', no_recognized_single_arbiter_exists_for_the_clause, conventional).
narrative_ontology:cs_axiom('d68375dc-b6b8-490a-b4ee-d000a774ac43', secondary, clause_meaning_stays_indeterminate_absent_authoritative_determination).
narrative_ontology:cs_axiom_status(clause_meaning_stays_indeterminate_absent_authoritative_determination, holdable).
narrative_ontology:cs_axiom_grounding('d68375dc-b6b8-490a-b4ee-d000a774ac43', clause_meaning_stays_indeterminate_absent_authoritative_determination, conventional).
narrative_ontology:cs_reference_frame('d68375dc-b6b8-490a-b4ee-d000a774ac43', plural_authority_equilibrium).
narrative_ontology:cs_drift_state('d68375dc-b6b8-490a-b4ee-d000a774ac43', post_2024_icj_advisory_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('d68375dc-b6b8-490a-b4ee-d000a774ac43', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, veto_holding_patron).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, text_drafting_states).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, occupied_population).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, arab_demand_states).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, general_assembly_majority).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__interpretive_authority_structure, consent_based_adjudication_doctrine).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__interpretive_authority_structure, p5_veto_privilege_norm).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__interpretive_authority_structure, sovereign_interpretive_autonomy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds military control of territory taken in the June 1967 war. Invokes its own reading of the withdrawal paragraph — grounded in claimed customary state practice and security doctrine — and declines every path that would submit that reading to an outside decision: it has not accepted compulsory court jurisdiction over the question, rejects referral attempts, and negotiates only bilaterally from strength. What flows to it is continued physical control of the disputed land for as long as no authority exists able to overrule its interpretation; what flows from it is the diplomatic isolation and periodic censure that accompany that stance. Leaving the dispute entirely would mean either withdrawing unilaterally or annexing outright — both carry costs it has declined to pay for five decades.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state, beneficiary,
    powerful, generational, constrained, regional).

% A permanent member of the Security Council aligned with the occupying state. Its veto over binding Council action means no definitive determination can be imposed without its acquiescence, and its role as broker of every negotiation round gives it a gatekeeping position no other outside power holds. It periodically shields the occupying state from Council action while sponsoring the frameworks that keep the settlement formula in circulation, playing Council procedure, bilateral channels, and Assembly politics against one another. Walking away from the broker role would cost it influence across the region, so it stays engaged on its own terms.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, veto_holding_patron, beneficiary,
    institutional, generational, arbitrage, global).

% The governments whose diplomats wrote the two language versions of the resolution in 1967. Their authorship gives them a standing claim that the original intent — including any deliberate ambiguity — is theirs to expound, a claim they have invoked at intervals for nearly sixty years. Two of the three hold Security Council vetoes; all three retain diplomatic weight from being the texts' parents. They cannot disown authorship without surrendering that standing, so they remain parties to the argument whether or not they wish to be.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, text_drafting_states, beneficiary,
    institutional, generational, constrained, global).

% The principal judicial organ of the United Nations. It claims that treaty meaning is properly fixed by judicial interpretation and has issued advisory opinions touching the occupation's legality and consequences, but the specific withdrawal paragraph has never been referred to it for a binding determination — states choose when to ask it questions, and the parties with the most at stake decline to ask this one. Its authority over the clause therefore remains asserted rather than exercised.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, international_court_of_justice, excluded,
    institutional, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__interpretive_authority_structure, international_court_of_justice, agenda_setter).

% The people living under the occupation whose territory the disputed paragraph governs. They had no representative at the 1967 drafting table and hold no vote in any body that could fix the text's meaning. Every year the interpretive question stays open is a year the occupation continues on the ground. Their practical exits — emigration, accommodation, or uprising — have all been tried at enormous cost without altering who decides the text's meaning.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, occupied_population, payer,
    powerless, biographical, trapped, regional).

% The neighboring states from whose territory withdrawal is demanded. They read the paragraph as requiring full withdrawal and have sought a definitive legal ruling to that effect for decades. Two of them eventually recovered their territory through direct bilateral treaties that bypassed the interpretive argument altogether; the remainder still hold out for closure through the multilateral channel, paying in stalled relations and periodic wars for the absence of a decision.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, arab_demand_states, payer,
    organized, generational, constrained, regional).

% The broad coalition of states that votes annually to reaffirm the maximal reading of the paragraph. Its resolutions express the numerical weight of opinion but carry no enforcement, and its members have no lever that compels anyone to accept their interpretation. Participation consumes diplomatic capital and returns reaffirmation rather than resolution.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, general_assembly_majority, payer,
    organized, generational, constrained, global).

% Scholars, bar associations, and jurists who track the dispute and publish on it. Many serve as advisers to one side or another; others document the interpretive history. Their stake is doctrinal: the episode is a leading test of whether treaty text can mean anything determinate when the parties refuse a common arbiter. They see the full structure from outside the bargaining.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, international_law_community, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state).
narrative_ontology:fixing_cost_class(unsc_242_withdrawal_clause__interpretive_authority_structure, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a single negotiated formula — territory for recognition and secure boundaries — available as common diplomatic currency among parties that agree on nothing else, so that ceasefire lines, disengagement agreements, and successive negotiation rounds all have a shared reference text.
% TRANSFER_FUNCTION: Transfers interpretive-authority claims among contending institutions and states, and — through the ambiguity those claims preserve — transfers open-ended control of occupied territory to the occupying state and settlement gatekeeping to the veto-holding patron and drafting states, away from the parties seeking definitive legal closure.
% ABSENT_VOICES: The occupied population had no seat at the 1967 table and none in any body empowered to fix the meaning; the text speaks of 'territories' and 'refugees' without representing the people concerned. The principal judicial organ is acknowledged in principle but never referred the binding question. Both would object to the current allocation of interpretive authority; both are structurally outside it.
% DISAPPEARANCE_RATIONALE: If a single recognized interpretive authority appeared overnight — a binding Court determination or an uncontested Council ruling — the substantive question would be forced to resolution within months: one reading would become obligatory law and the other heresy, occupation terms would face legal deadlines, the patron's broker monopoly would collapse, and five decades of positional diplomacy would lose its organizing ambiguity. If instead the interpretive structure vanished without replacement — all parties simply stopped citing the clause — the land-for-peace architecture underpinning every peace plan since 1967 would lose its anchor. Either way the world rearranges; nothing about the current arrangement is self-maintaining.
% FOUNDING_PROBLEM: Consolidate the June 1967 ceasefire into a negotiable settlement: a formula all belligerents could accept linking withdrawal from captured territory to recognition, secure boundaries, and a just settlement for the displaced.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: successive Secretaries-General and the Court's advisory jurisprudence treat the settlement formula as still operative and still unresolved; the 2002 Arab Peace Initiative attests demand-side acceptance of the formula's terms; the academic treaty-law literature documents the interpretive impasse as persistent. No source outside the arrangement's beneficiaries attests that the contested authority structure itself is necessary to solving the problem — the corroboration covers the founding problem's liveness, not the arrangement's indispensability.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__interpretive_authority_structure, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__interpretive_authority_structure, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__interpretive_authority_structure, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__interpretive_authority_structure, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__interpretive_authority_structure_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__interpretive_authority_structure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__interpretive_authority_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) because the arrangement's principal yield — open-ended territorial control and settlement gatekeeping — flows continuously to the strong while closure is denied to the weak; the ambiguity is not a cost anyone is trying to eliminate but the asset each beneficiary defends. Suppression (0.66) is structural, not internalized: it consists of the veto over binding Council action, the consent-based jurisdiction regime that lets the parties with the most at stake decline adjudication, and the power-gating of the bilateral exit route. Suppression is authored as a raw structural property and is not scaled by power or scope — only extractiveness is scaled, by the engine, through directionality and scope. Theater (0.55) reflects the annual reaffirmation cycle, anniversary diplomacy, and process rounds that restate positions rather than resolve them; the function is not dead (Egypt and Jordan treaties were real outputs) but a growing share of activity is performative maintenance of the dispute itself. Accessibility collapse (0.55) is partial: the multilateral routes to determination (binding Court referral, enforceable Council action) are effectively closed, but bilateral negotiation remains a real alternative for sufficiently strong actors. Resistance (0.62) is continuous and organized: Assembly majorities, demand states, and the legal community contest the arrangement constantly without displacing it. Enforcement dependence is genuine and evolving — hence the suppression_requirement series, which tracks the enforcement machinery's build-up (rising veto reliance through 2004, the cheaper abstention tactic after 2016) rather than merely restating the scalar. The trajectory is drift-with-perturbation, not oscillation: the 1978 dip reflects the temporary credibility of the bilateral exit, the 2000 rise the collapse of the negotiated track; there is no intermittent-reinforcement cycle to model. All three series share one eight-point grid so the engine samples every metric at every examined time.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the occupying state's position the arrangement is legitimate interpretive pluralism — sovereign judgment over its own security, protected by the same consent-based rules every state enjoys. From the occupied population's position the identical structure is the indefinite extension of occupation by procedural means: every year without a decision is a year of the status quo on the ground. From the Court's position it is usurpation — a judicial function acknowledged in principle and starved in practice. From the patron's position it is indispensable mediation: the ambiguity is what makes its brokerage necessary. Coalition potential among the powerless exists on paper (Assembly majority plus demand states plus the population's advocates command overwhelming numbers), but it is blunted structurally: the veto and the consent-jurisdiction gate sit upstream of anything a numeric coalition can reach, so coalitions generate pressure and reaffirmation, never determination. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations map to real structural relationships: the occupying state collects continued territorial control (low derived directionality, amplified little by its constrained exit since it is a net gainer regardless); the veto-holding patron collects broker monopoly and alliance shielding, with arbitrage-grade mobility across venues (near-beneficiary end); the drafting states collect standing from authorship, bound to defend the claim they live on. The victim declarations map symmetrically: the occupied population sits nearest the full-target end (powerless, trapped, bearing the arrangement's entire physical cost); the demand states and Assembly majority bear diffuse diplomatic and security costs with constrained exits. The Court has no beneficiary or victim declaration — it is neither collecting nor paying — so its seat falls to the canonical fallback for its power atom. No directionality override was authored: the schema keys overrides on power atoms, and an institutional-wide override calibrated for the Court would simultaneously misplace the patron and the drafting states, whose derived directionalities from their declared beneficiary positions are already correct. The Court's blocked-resolver position is carried instead by its excluded/agenda-setter dual role and constrained exit, which the derivation reads directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy lens guards against two opposite errors here. Read from the beneficiary seats, the arrangement presents as a rope — a neutral interpretive framework all parties can live with, the price of doing diplomacy among sovereignties. Read from the target seats, it presents as pure theft of time and territory. The snare classification holds because the coordination story and the extraction ride the same structure: the formula that enables diplomacy is exactly the ambiguity that defers closure, and the arrangement persists only through active enforcement (veto deployments, jurisdiction refusals) rather than participant preference. On obsolescence: the founding problem — a mutually accepted territorial settlement — is demonstrably still unsolved, so the arrangement cannot be dismissed as a zombie administering a dead mandate; but the corroboration record shows the problem's liveness is attested from outside while the arrangement's necessity is attested only by its beneficiaries. The status is therefore authored contested rather than dead, which keeps the mismatch consumer (dead-status plus world-rearranges verdict) from firing a premature capture flag while leaving the door open: if bilateral settlements eventually cover every territorial claim and the multilateral structure persists unused, the status flips to dead and the flag fires on schedule.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_of_unsc242_kernel,
    'This constraint is one reading of the unsc_242_withdrawal_clause kernel (the interpretive-authority reading). What would change structurally if a sibling reading were instantiated instead?',
    'Compare compiled classifications across the three family stories: if the maximal reading computes as a Charter-grounded obligation with minimal tolerance for territorial retention, and the partial reading computes as a licensed-discretion regime, then the persistence value of this meta-structure lies precisely in keeping both substantive readings from consolidating.',
    'Sibling instantiation would relocate the victim set: the maximal reading names the occupied population and demand states as entitled parties; the partial reading licenses the occupying state''s retention; this reading names closure-seekers as the parties harmed by the authority vacuum itself. The disagreement is located in who holds authority to fix the text''s meaning, not in what the text means.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_of_unsc242_kernel, conceptual, 'Committer-frame routing: this story is the authority-contest reading of a three-reading kernel; siblings are substantive-outcome readings.').

omega_variable(
    anarchy_vs_maintained_vacuum,
    'Is the absence of a binding interpretive arbiter an irreducible feature of a decentralized system of sovereign states, or a maintained artifact of specific veto deployments and jurisdiction-refusal choices?',
    'Counterfactual analysis of near-miss moments (the 1973 Geneva Conference, the 1978 Camp David framework, the 2000 Taba talks, Resolution 2334 in 2016): did definitive determination fail because no authority was possible, or because specific actors declined each available path?',
    'If irreducible, part of the measured suppression is the structural price of sovereignty itself — a natural-law floor beneath the arrangement that no reform removes. If maintained, the arrangement is enforced extraction through institutional design and the classification firms accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anarchy_vs_maintained_vacuum, empirical, 'Whether the authority vacuum is natural to the international system or manufactured and defended.').

omega_variable(
    bilateral_exit_asymmetry,
    'Is the bilateral-treaty exit from the interpretive dispute (Egypt 1979, Jordan 1994) generally available to closure-seekers, or only to actors strong enough to negotiate from positions the occupying state accepts?',
    'Compare exit outcomes across power levels: Egypt recovered the entire Sinai; Jordan recovered most of its claimed land; Syria recovered nothing; the occupied population recovered nothing through negotiation across five decades of process participation.',
    'If exit scales with power, the arrangement''s defining feature is asymmetric exit availability — strengthening the extraction reading. If exits generalize to weaker seekers, the structure drifts toward a coordination bottleneck with overhead rather than a trap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bilateral_exit_asymmetry, empirical, 'Whether the bypass route around the interpretive dispute is democratically available or power-gated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__interpretive_authority_structure, 1967, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t1967, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 1967, 0.2).
narrative_ontology:measurement_basis(unsc_tr_t1967, observed).
narrative_ontology:measurement(unsc_tr_t1973, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 1973, 0.25).
narrative_ontology:measurement_basis(unsc_tr_t1973, observed).
narrative_ontology:measurement(unsc_tr_t1978, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 1978, 0.3).
narrative_ontology:measurement_basis(unsc_tr_t1978, observed).
narrative_ontology:measurement(unsc_tr_t1993, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 1993, 0.4).
narrative_ontology:measurement_basis(unsc_tr_t1993, observed).
narrative_ontology:measurement(unsc_tr_t2000, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 2000, 0.35).
narrative_ontology:measurement_basis(unsc_tr_t2000, observed).
narrative_ontology:measurement(unsc_tr_t2004, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 2004, 0.5).
narrative_ontology:measurement_basis(unsc_tr_t2004, observed).
narrative_ontology:measurement(unsc_tr_t2016, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 2016, 0.52).
narrative_ontology:measurement_basis(unsc_tr_t2016, observed).
narrative_ontology:measurement(unsc_tr_t2025, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 2025, 0.55).
narrative_ontology:measurement_basis(unsc_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(unsc_be_t1967, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 1967, 0.55).
narrative_ontology:measurement_basis(unsc_be_t1967, observed).
narrative_ontology:measurement(unsc_be_t1973, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 1973, 0.6).
narrative_ontology:measurement_basis(unsc_be_t1973, observed).
narrative_ontology:measurement(unsc_be_t1978, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 1978, 0.58).
narrative_ontology:measurement_basis(unsc_be_t1978, observed).
narrative_ontology:measurement(unsc_be_t1993, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 1993, 0.62).
narrative_ontology:measurement_basis(unsc_be_t1993, observed).
narrative_ontology:measurement(unsc_be_t2000, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 2000, 0.7).
narrative_ontology:measurement_basis(unsc_be_t2000, observed).
narrative_ontology:measurement(unsc_be_t2004, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 2004, 0.74).
narrative_ontology:measurement_basis(unsc_be_t2004, observed).
narrative_ontology:measurement(unsc_be_t2016, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 2016, 0.76).
narrative_ontology:measurement_basis(unsc_be_t2016, observed).
narrative_ontology:measurement(unsc_be_t2025, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 2025, 0.78).
narrative_ontology:measurement_basis(unsc_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t1967, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 1967, 0.4).
narrative_ontology:measurement_basis(unsc_su_t1967, observed).
narrative_ontology:measurement(unsc_su_t1973, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 1973, 0.5).
narrative_ontology:measurement_basis(unsc_su_t1973, observed).
narrative_ontology:measurement(unsc_su_t1978, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 1978, 0.48).
narrative_ontology:measurement_basis(unsc_su_t1978, observed).
narrative_ontology:measurement(unsc_su_t1993, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 1993, 0.55).
narrative_ontology:measurement_basis(unsc_su_t1993, observed).
narrative_ontology:measurement(unsc_su_t2000, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement_basis(unsc_su_t2000, observed).
narrative_ontology:measurement(unsc_su_t2004, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 2004, 0.68).
narrative_ontology:measurement_basis(unsc_su_t2004, observed).
narrative_ontology:measurement(unsc_su_t2016, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 2016, 0.64).
narrative_ontology:measurement_basis(unsc_su_t2016, observed).
narrative_ontology:measurement(unsc_su_t2025, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 2025, 0.66).
narrative_ontology:measurement_basis(unsc_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__interpretive_authority_structure, enforcement_mechanism).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__interpretive_authority_structure, maximal_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__interpretive_authority_structure, partial_withdrawal_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'what does Resolution 242's withdrawal paragraph require?' decomposes into three structurally distinct constraints per the epsilon-invariance principle: the maximal reading (a Charter-grounded withdrawal obligation), the partial reading (a drafter-intent discretion license), and this story — the interpretive-authority structure that determines whether either can consolidate. Each member carries its own epsilon, beneficiaries, and victims; the substantive readings concern what is owed, this one concerns who may say. Direction of influence runs from this story outward: the two substantive readings draw their continuing force from the authority vacuum modeled here — neither could consolidate while the meta-dispute holds — so this reading influences both siblings rather than depending on them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
