% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__binding_referee_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__binding_referee_reading, []).

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
 *   constraint_id: wto_dsb_authority__binding_referee_reading
 *   human_readable: WTO Dispute Settlement as Binding Referee (Binding-Referee Reading)
 *   domain: international law / trade governance / institutional legitimacy
 *
 * SUMMARY:
 *   The WTO Dispute Settlement Body sits at the center of the most
 *   judicialized enforcement machinery in international law: panels hear
 *   complaints, reports are adopted by reverse consensus so no member —
 *   including the losing member — can block them, and members that fail to
 *   conform face authorized suspension of trade concessions. This story
 *   instantiates the binding-referee reading of the contested DSB-authority
 *   kernel: rulings are binding treaty obligations, non-compliance is a
 *   treaty breach rather than a policy choice, and member states surrendered
 *   policy discretion within WTO-covered domains as the explicit price of
 *   secure market access. Per the ε-referent rule for kernel readings,
 *   extractiveness is authored for the standing binding-adjudication
 *   arrangement as this reading assesses it — never for the advisory
 *   arrangement a sibling reading would prefer. The claimed type is authored
 *   independently of the metrics: the arrangement is claimed as a tangled
 *   rope because it pairs a genuine, consented, reciprocal coordination
 *   function (dispute governance, commitment stabilization, a forum that
 *   neutralizes raw power asymmetry) with real asymmetric extraction
 *   (surrendered discretion, compliance costs borne by losing respondents and
 *   domestic regulators), and its persistence depends on active enforcement
 *   machinery. The engine computes each seat's classification from the
 *   structural data; where a computed seat-type diverges from this claim,
 *   that divergence is the measurement. KEY AGENTS (by structural
 *   relationship): - wto_dispute_settlement_body: agenda-setter
 *   (institutional/constrained) — adopts rulings by reverse consensus and
 *   authorizes retaliation - losing_respondent_states: primary target seat
 *   (powerful/constrained) — bears compliance obligations -
 *   complainant_states: primary beneficiary seat (powerful/mobile) — receives
 *   case-level remedies - small_trading_states: beneficiary seat
 *   (organized/constrained) — procures procedural neutralization of power
 *   asymmetry - domestic_regulatory_agencies: secondary target
 *   (moderate/trapped) — surrenders regulatory discretion within covered
 *   domains - large_trading_powers: dual-positioned payer/beneficiary
 *   (powerful/arbitrage) — heaviest user, most capable resister -
 *   affected_non_trade_interests: excluded constituency (organized/trapped) —
 *   bears regulatory losses without standing - wto_secretariat: analytical
 *   observer (moderate/mobile) — administers the case-law apparatus
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__binding_referee_reading, 0.66).
domain_priors:suppression_score(wto_dsb_authority__binding_referee_reading, 0.53).
domain_priors:theater_ratio(wto_dsb_authority__binding_referee_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, suppression_requirement, 0.53).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__binding_referee_reading, tangled_rope).
narrative_ontology:human_readable(wto_dsb_authority__binding_referee_reading, "WTO Dispute Settlement as Binding Referee (Binding-Referee Reading)").
narrative_ontology:topic_domain(wto_dsb_authority__binding_referee_reading, "international law / trade governance / institutional legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__binding_referee_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__binding_referee_reading, '22b22edf-3cc7-4387-9f9e-14c98d657f1a').
narrative_ontology:cs_kernel_codification('22b22edf-3cc7-4387-9f9e-14c98d657f1a', fixed_text).
narrative_ontology:cs_authority_grounding('22b22edf-3cc7-4387-9f9e-14c98d657f1a', lineage).
narrative_ontology:cs_interpretation_layer_present('22b22edf-3cc7-4387-9f9e-14c98d657f1a').
narrative_ontology:cs_reading_relation('22b22edf-3cc7-4387-9f9e-14c98d657f1a', wto_dsb_authority__advisory_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('22b22edf-3cc7-4387-9f9e-14c98d657f1a', wto_dsb_authority__judicial_activism_reading, influences).
narrative_ontology:cs_axiom('22b22edf-3cc7-4387-9f9e-14c98d657f1a', foundational, adopted_rulings_bind_as_treaty_obligations).
narrative_ontology:cs_axiom_status(adopted_rulings_bind_as_treaty_obligations, holdable).
narrative_ontology:cs_axiom_grounding('22b22edf-3cc7-4387-9f9e-14c98d657f1a', adopted_rulings_bind_as_treaty_obligations, conventional).
narrative_ontology:cs_axiom('22b22edf-3cc7-4387-9f9e-14c98d657f1a', foundational, covered_domain_discretion_surrendered).
narrative_ontology:cs_axiom_status(covered_domain_discretion_surrendered, holdable).
narrative_ontology:cs_axiom_grounding('22b22edf-3cc7-4387-9f9e-14c98d657f1a', covered_domain_discretion_surrendered, conventional).
narrative_ontology:cs_reference_frame('22b22edf-3cc7-4387-9f9e-14c98d657f1a', consented_treaty_binding_adjudication).
narrative_ontology:cs_drift_state('22b22edf-3cc7-4387-9f9e-14c98d657f1a', post_appellate_body_crisis, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('22b22edf-3cc7-4387-9f9e-14c98d657f1a', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__binding_referee_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, complainant_states).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, small_trading_states).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, losing_respondent_states).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, large_trading_powers).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, domestic_regulatory_agencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, large_trading_powers).
narrative_ontology:constraint_vindicates(wto_dsb_authority__binding_referee_reading, rule_based_multilateral_trade).
narrative_ontology:constraint_vindicates(wto_dsb_authority__binding_referee_reading, treaty_supremacy_in_covered_domains).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The member states sitting collectively adopt panel and appellate reports by reverse consensus, so a losing member cannot block the ruling against it, and authorize suspension of trade concessions when a member does not bring its measures into conformity. It runs compliance proceedings and monitors implementation. Its authority exists only inside the mandate the treaty gives it, and members can — and since 2017 one has — disable parts of it by blocking appointments.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, wto_dispute_settlement_body, agenda_setter,
    institutional, generational, constrained, global).

% Members whose measures are found inconsistent with their obligations. They must bring laws and regulations into conformity or face authorized suspension of concessions by the winning member; non-compliance is recorded as a treaty breach rather than a discretionary policy position. They can defy a ruling and absorb retaliation, negotiate a settlement, or withdraw from the treaty system entirely — each at substantial cost.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, losing_respondent_states, payer,
    powerful, generational, constrained, global).

% Members that bring challenges to regain market access or remove discriminatory treatment against their exporters. They obtain a lawful, rule-governed route to enforce commitments that would otherwise depend on their own retaliatory muscle, and they choose freely between litigating and negotiating. Large complainants retain the outside option of unilateral pressure, which strengthens their hand inside the process.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, complainant_states, beneficiary,
    powerful, generational, mobile, global).

% Members whose market size is too small for retaliation to be a credible threat on its own. The forum lets them win on legal argument against far larger economies — a substitution of procedure for power that unilateral bargaining never offered them. Enforcement remains their weak point: a ruling they cannot back with market leverage may be honored slowly or not at all, and they have no realistic alternative venue.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, small_trading_states, beneficiary,
    organized, generational, constrained, global).

% Food-safety, environmental, health, and consumer agencies whose protective measures can be challenged as trade-restrictive by trading partners. They must design and defend regulation to survive treaty scrutiny, and when a measure is struck down the agency rewrites it. The obligation binds the state, and the agency has no separate exit from it.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, domestic_regulatory_agencies, payer,
    moderate, generational, trapped, national).

% The largest economies are simultaneously the heaviest users of the system and its most frequent targets, with landmark wins and losses on both sides of the docket. Their size lets them absorb authorized retaliation that would ruin a smaller economy, negotiate bilateral settlements outside the process, and — as one member demonstrated from 2017 by blocking appellate appointments — disable parts of the machinery they dislike while remaining inside the treaty.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, large_trading_powers, payer,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__binding_referee_reading, large_trading_powers, beneficiary).

% Environmental, public-health, consumer, and labor constituencies whose protective measures are the subject matter of many disputes. Only member states may bring claims, so these interests have no standing of their own; their views enter only through amicus submissions that panels may accept or disregard. When a protection is struck down, they bear the loss with no procedural recourse.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, affected_non_trade_interests, excluded,
    organized, generational, trapped, global).

% Provides legal and administrative support to panels and maintains the case-law apparatus. Its professional staff are trade lawyers whose careers and institutional continuity are bound up with the system's functioning; they advise but do not decide, and individual staff can and do leave for other institutions.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, wto_secretariat, observer,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_dsb_authority__binding_referee_reading, complainant_states).
narrative_ontology:fixing_cost_class(wto_dsb_authority__binding_referee_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts trade disputes from self-judged retaliation contests into rule-governed proceedings: a neutral forum, agreed procedures, and adopted findings all members can predict. This stabilizes reciprocal market-access commitments — each member's concession is worth more because breach carries an agreed consequence — and gives small members a forum in which legal argument substitutes for market power.
% TRANSFER_FUNCTION: Moves policy discretion from member governments to treaty discipline within covered domains: losing respondents must conform domestic law or face authorized suspension of concessions. It also moves enforcement leverage from unilateral state power into the multilateral process, and moves compliance costs onto losing respondents and the domestic agencies whose measures are struck down.
% ABSENT_VOICES: Affected non-trade interests — environmental, public-health, consumer, and labor constituencies — have no standing; only member states litigate. Non-member states facing accession conditions also have no seat. Both would argue for preserved regulatory autonomy and direct participation; they are outside the room because the treaty channels all voice through member governments.
% DISAPPEARANCE_RATIONALE: Without binding adopted rulings and authorized retaliation, disputes revert to power-based bargaining and self-judged retaliation; market-access commitments degrade to what each member can enforce alone; the small-member forum disappears; and the treaty's tariff and rules bindings lose their agreed consequence — the escalation dynamic the system was built to suppress returns.
% FOUNDING_PROBLEM: Under GATT 1947, panel reports could be adopted only by consensus, so the losing party could block its own defeat; commitments were enforceable only by unilateral retaliation, and the 1930s-style escalation spiral was the standing alternative. The Uruguay Round negotiators created reverse-consensus adoption, a standing appellate body, and authorized retaliation precisely so rulings could not be blocked and breach would carry an agreed cost.
% FOUNDING_PROBLEM_CORROBORATION: The GATT-era blocking record and the Uruguay Round negotiating history are documented in official records and international-law scholarship independent of any current beneficiary's advocacy; and the system's continued use by members on all sides — including the member currently blocking appellate appointments, which still files new complaints — attests from outside any single beneficiary set that the underlying problem of enforcing trade commitments without escalation persists.
narrative_ontology:disappearance_verdict(wto_dsb_authority__binding_referee_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__binding_referee_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__binding_referee_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(wto_dsb_authority__binding_referee_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__binding_referee_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__binding_referee_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_dsb_authority__binding_referee_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_dsb_authority__binding_referee_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.66 at interval end) is substantial but bounded: covered-domain discretion really is surrendered and adopted reports really do bind under reverse consensus, but the trade is reciprocal and consented — every member is periodically on both sides of the docket. Suppression (0.53) is authored as a raw structural property and is NOT scaled by power or scope in authoring; only extractiveness is scaled by the engine. The arrangement cannot compel conformity directly, and real alternatives persist (defy and absorb authorized retaliation, settle, withdraw), but deviation carries treaty-breach status rather than discretionary-choice status — the binding reading's own structural delta. Theater (0.42) reflects real adjudication and mostly real compliance shadowed by a widening performative layer: unenforceable small-state wins, post-2019 'appeal into the void', and formal-conformity metrics substituting for enforced market-access outcomes. Accessibility collapse (0.5): alternatives persist by design — the DSU prefers negotiated settlement and bilateral leverage remains available — so the arrangement prices alternatives rather than eliminating them. Resistance (0.6): the appellate-appointment blockade, the interim-appeal workaround, persistent sovereignty critique, and standing reform demands are organized, ongoing resistance from inside the membership. The measurement series run on ONE shared time grid (t=0 is 1995, DSU entry into force; t=30 is 2025) so every tracked metric is authored at every examined point: extractiveness rises with the case law's reach into domestic regulatory space and plateaus after the enforcement tier is paralyzed; the suppression_requirement series rises as compliance machinery matures (Art. 21.5 proceedings, sequencing agreements, retaliation authorizations) and falls after 2019 as the active enforceable force decays while the binding claim persists — an enforcement-intensification-then-decay arc, not a ratchet; theater climbs monotonically across the interval.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the respondent and regulatory-agency seats, the same adopted report that the complainant seat experiences as the guarantee of its market access is the surrender of domestic policy discretion — high structural exposure, and for the trapped agency seat no procedural recourse at all. The agenda-setter seat (the DSB as members collective) experiences the arrangement as its own constitutive function rather than as something imposed on it. The large-power seat shows the sharpest divergence: with arbitrage-grade exit it uses the machinery when favorable (the most frequent complainant of the system's first two decades) and disables parts of it when not (the post-2017 appellate blockade) — the same structure reads as guarantee and as overreach from that seat depending on the docket. The engine computes per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: complainant_states and small_trading_states sit at the beneficiary end — the arrangement subsidizes their enforcement problem (they gain a lawful remedy route they could not self-supply), with small states' constrained exit keeping them inside the system they benefit from. Victim declarations: losing_respondent_states and large_trading_powers sit at the target end — compliance obligations and authorized retaliation land on them — with the large powers' arbitrage exit (absorbable retaliation, blockade leverage, bilateral workarounds) damping their effective extraction well below their raw exposure; domestic_regulatory_agencies sit nearest the full-target end: trapped, moderate power, and the obligation binds the state they cannot leave. The DSB seat is the administrator: near-symmetric, since its institutional continuity depends on the arrangement it runs but it collects no case-level gains. The excluded constituency bears losses with no standing — maximal structural exposure with zero procedural voice. Global spatial scope is authored on all member seats: verification of conformity across 160+ members is hard, and the engine's scope amplification lands on the target-side seats. Receipt note: the arrangement's case-level gains land on prevailing complainants, but the seat rotates (every member is periodically a respondent), so no seat persistently captures — the receipt surface is authored accordingly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — enforcement of trade commitments without escalation to power contests — is live, so the arrangement has not outlived its function and no mandatrophy is declared. The tangled-rope claim does the anti-mislabeling work: a pure-coordination reading would erase the surrendered-discretion victims (domestic regulators, losing respondents) whose costs are this reading's own subject; a pure-extraction reading would erase the reciprocal, consented structure — every member is both potential complainant and potential respondent, and the system's loudest critic remains one of its heaviest users. The theater series (0.15 to 0.42) tracks Goodhart-style drift — as compliance and procedural throughput became the visible metrics, formal conformity and docket activity partially substituted for enforced market-access outcomes — but the ratio stays below the substitution threshold, consistent with a coordination function that still dominates its performative shell. The R5 mismatch check (founding_problem_status live × disappearance_verdict world_rearranges) raises no zombie flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation_ambiguity,
    'This story instantiates one reading (binding referee) of the wto_dsb_authority kernel; would the advisory-coordination or judicial-activism reading better capture the arrangement members actually operate, and how would the structural delta change if so?',
    'Observe member conduct under adverse rulings: if members treat adopted reports as obligations (Art. 23 conformity legislation, interim-appeal accession, compliance implementation), the binding reading is instantiated; if they treat reports as negotiating inputs or as overreach to be resisted wholesale, the sibling readings gain.',
    'The advisory reading would carry a much lower ε and no surrendered-discretion victims (approaching pure coordination); the activism reading would carry a higher ε with an illegitimacy-based victim structure and would push the arrangement toward the pure-extraction end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_instantiation_ambiguity, conceptual, 'Which reading of the DSB-authority kernel the operating arrangement actually instantiates.').

omega_variable(
    reciprocity_vs_regulatory_extraction,
    'Is the surrendered policy discretion a genuinely reciprocal exchange (each member both grants and receives binding discipline) or an asymmetric transfer in which trade values systematically override domestic regulatory values?',
    'Longitudinal analysis of complainant/respondent distribution by development status and of regulatory outcomes across SPS and TBT case law; check whether regulatory-autonomy losses concentrate on particular classes of members.',
    'If asymmetric, the authored ε understates extraction for the regulatory-autonomy seats and the arrangement drifts toward the extractive end for those seats; if reciprocal, the coordination framing holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_vs_regulatory_extraction, empirical, 'Whether the discretion trade is reciprocal or systematically asymmetric.').

omega_variable(
    appellate_paralysis_decay_vs_transition,
    'Does the post-2019 appellate paralysis represent permanent decay of the binding-adjudication arrangement (the binding claim persisting while the enforcing tier is disabled) or a transitional gap that interim arbitration and reform will close?',
    'Track interim-appeal arrangement membership growth, any restoration of appellate appointments, and compliance rates for panel reports that would otherwise have been appealed.',
    'A decay reading would date a drift from enforced binding adjudication toward theatrical maintenance of the binding claim (rising theater, falling suppression, eventual inertial persistence); a transition reading would re-anchor the enforcement tier and validate the current suppression trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appellate_paralysis_decay_vs_transition, empirical, 'Whether the enforcement-tier paralysis is decay or transition.').

omega_variable(
    small_state_enforcement_gap,
    'For small trading states, is the protection the forum provides real or nominal — do their wins produce compliance when they lack the market power to retaliate?',
    'Compliance rates disaggregated by respondent market size and complainant retaliation capacity; compare implementation timelines for rulings won by large versus small complainants.',
    'If small-state wins are largely unenforced, the theater ratio understates the performative component for that seat and the coordination benefit claimed for small members is partly nominal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(small_state_enforcement_gap, empirical, 'Whether the small-member forum benefit is enforced or nominal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__binding_referee_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto_dsb_binding_referee_tr_t0, wto_dsb_authority__binding_referee_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_tr_t0, observed).
narrative_ontology:measurement(wto_dsb_binding_referee_tr_t5, wto_dsb_authority__binding_referee_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_tr_t5, observed).
narrative_ontology:measurement(wto_dsb_binding_referee_tr_t10, wto_dsb_authority__binding_referee_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_tr_t10, observed).
narrative_ontology:measurement(wto_dsb_binding_referee_tr_t15, wto_dsb_authority__binding_referee_reading, theater_ratio, 15, 0.27).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_tr_t15, observed).
narrative_ontology:measurement(wto_dsb_binding_referee_tr_t20, wto_dsb_authority__binding_referee_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_tr_t20, observed).
narrative_ontology:measurement(wto_dsb_binding_referee_tr_t25, wto_dsb_authority__binding_referee_reading, theater_ratio, 25, 0.38).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_tr_t25, observed).
narrative_ontology:measurement(wto_dsb_binding_referee_tr_t30, wto_dsb_authority__binding_referee_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(wto_dsb_binding_referee_be_t0, wto_dsb_authority__binding_referee_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_be_t0, observed).
narrative_ontology:measurement(wto_dsb_binding_referee_be_t5, wto_dsb_authority__binding_referee_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_be_t5, observed).
narrative_ontology:measurement(wto_dsb_binding_referee_be_t10, wto_dsb_authority__binding_referee_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_be_t10, observed).
narrative_ontology:measurement(wto_dsb_binding_referee_be_t15, wto_dsb_authority__binding_referee_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_be_t15, observed).
narrative_ontology:measurement(wto_dsb_binding_referee_be_t20, wto_dsb_authority__binding_referee_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_be_t20, observed).
narrative_ontology:measurement(wto_dsb_binding_referee_be_t25, wto_dsb_authority__binding_referee_reading, base_extractiveness, 25, 0.65).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_be_t25, observed).
narrative_ontology:measurement(wto_dsb_binding_referee_be_t30, wto_dsb_authority__binding_referee_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(wto_dsb_binding_referee_su_t0, wto_dsb_authority__binding_referee_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_su_t0, observed).
narrative_ontology:measurement(wto_dsb_binding_referee_su_t5, wto_dsb_authority__binding_referee_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_su_t5, observed).
narrative_ontology:measurement(wto_dsb_binding_referee_su_t10, wto_dsb_authority__binding_referee_reading, suppression_requirement, 10, 0.57).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_su_t10, observed).
narrative_ontology:measurement(wto_dsb_binding_referee_su_t15, wto_dsb_authority__binding_referee_reading, suppression_requirement, 15, 0.61).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_su_t15, observed).
narrative_ontology:measurement(wto_dsb_binding_referee_su_t20, wto_dsb_authority__binding_referee_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_su_t20, observed).
narrative_ontology:measurement(wto_dsb_binding_referee_su_t25, wto_dsb_authority__binding_referee_reading, suppression_requirement, 25, 0.56).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_su_t25, observed).
narrative_ontology:measurement(wto_dsb_binding_referee_su_t30, wto_dsb_authority__binding_referee_reading, suppression_requirement, 30, 0.53).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__binding_referee_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, mpia_interim_appeal_arbitration).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, regional_trade_agreement_dispute_mechanisms).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, wto_accession_protocol_obligations).

% DUAL FORMULATION NOTE:
% One colloquial concept — 'WTO dispute settlement authority' — decomposes into three structurally distinct readings of a single kernel, per the ε-invariance principle. This file instantiates the binding-referee reading: rulings bind as treaty obligations and covered-domain discretion is surrendered, giving a moderate-high ε with named beneficiary and victim seats. The advisory-coordination reading (reports as negotiating inputs, discretion retained) would carry a much lower ε and no surrendered-discretion victims; the judicial-activism reading (illegitimate interpretive legislation) would carry a higher ε and an illegitimacy-based victim structure. Each is authored as its own constraint file; they are linked through cs_structure.reading_relations rather than averaged into one story, and this reading structurally influences the downstream mechanisms (interim appeal arbitration, RTA dispute clauses, accession conditionality) that exist to preserve or extend binding adjudication.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
