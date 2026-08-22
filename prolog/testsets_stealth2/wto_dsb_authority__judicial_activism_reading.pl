% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__judicial_activism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__judicial_activism_reading, []).

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
 *   constraint_id: wto_dsb_authority__judicial_activism_reading
 *   human_readable: WTO DSB Binding Authority — Judicial Activism Reading (Interpretive Drift as Unconsented Law-Creation)
 *   domain: international_law/trade_governance/institutional_legitimacy
 *
 * SUMMARY:
 *   This story instantiates the judicial-activism reading of the WTO
 *   dispute-settlement authority kernel: the claim that DSB panels and the
 *   Appellate Body exceeded the mandate members negotiated at Marrakesh,
 *   converting a consent-bound adjudicative procedure into a source of new
 *   obligations through interpretive drift — 'clarification' no member voted
 *   on, enforced by authorized retaliation. The constraint under measurement
 *   is the standing binding arrangement itself: compulsory jurisdiction,
 *   reverse-consensus report adoption, precedent accumulation, and
 *   retaliation authorization. Per the ε-referent rule for kernel readings,
 *   extractiveness is authored for that standing arrangement as this reading
 *   sees it (substantial: policy discretion transferred from member capitals
 *   into adjudicative case law), never for the mandate-bounded
 *   negotiated-settlement system this reading endorses. The reading's
 *   expected structural delta is visible in the record: active resistance to
 *   compliance (appointment blockage from 2017), treaty interpretation itself
 *   contested, retaliation authorization delegitimized in the eyes of its
 *   chief targets, and members building enforcement mechanisms outside the
 *   blocked tier (MPIA). The claimed type is authored from this reading's
 *   structural assessment — a real coordination function fused with
 *   asymmetric extraction, held together by active enforcement — while the
 *   metrics are authored as descriptive facts about the arrangement's
 *   operation; where the engine's per-seat computations diverge from the
 *   claim, that divergence is the measurement.
 *
 * KEY AGENTS:
 *   - dsb_adjudicative_body: agenda-setting seat (institutional/constrained) — panels and the suspended Appellate Body administer the arrangement and author the interpretive case law this reading identifies as law-creation; collects the extracted good, which is binding authority itself
 *   - prevailing_litigant_exporters: primary beneficiary (powerful/mobile) — collect rulings opening other members' markets beyond what negotiation yielded
 *   - trade_remedy_dependent_members: primary target (powerful/constrained) — trade-remedy regimes repeatedly rewritten by appellate interpretation; the seat that resisted by blocking appointments
 *   - small_developing_members: secondary target (moderate/constrained) — bound by case law they did not shape, without capacity to litigate or credibly retaliate
 *   - domestic_import_competing_industries: excluded cost-bearer (organized/trapped) — lose protection when rulings strike down domestic measures; no seat in the conversation
 *   - trade_law_specialist_community and wto_secretariat_legal_affairs: secondary beneficiaries (organized/identity-locked) — careers and institutional standing bound to the interpretive corpus's growth
 *   - mpia_participants: sustaining beneficiaries (organized/mobile) — keep binding arbitration alive among themselves after the appellate tier failed
 *   - international_economic_law_scholars: analytical observer — documents the interpretive trajectory without collecting from it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__judicial_activism_reading, 0.7).
domain_priors:suppression_score(wto_dsb_authority__judicial_activism_reading, 0.45).
domain_priors:theater_ratio(wto_dsb_authority__judicial_activism_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__judicial_activism_reading, tangled_rope).
narrative_ontology:human_readable(wto_dsb_authority__judicial_activism_reading, "WTO DSB Binding Authority — Judicial Activism Reading (Interpretive Drift as Unconsented Law-Creation)").
narrative_ontology:topic_domain(wto_dsb_authority__judicial_activism_reading, "international_law/trade_governance/institutional_legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__judicial_activism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__judicial_activism_reading, 'd14862ba-ea64-4672-a3a4-5fe54e51cdfe').
narrative_ontology:cs_kernel_codification('d14862ba-ea64-4672-a3a4-5fe54e51cdfe', fixed_text).
narrative_ontology:cs_authority_grounding('d14862ba-ea64-4672-a3a4-5fe54e51cdfe', extraction).
narrative_ontology:cs_interpretation_layer_present('d14862ba-ea64-4672-a3a4-5fe54e51cdfe').
narrative_ontology:cs_reading_relation('d14862ba-ea64-4672-a3a4-5fe54e51cdfe', wto_dsb_authority__binding_referee_reading, influences).
narrative_ontology:cs_reading_relation('d14862ba-ea64-4672-a3a4-5fe54e51cdfe', wto_dsb_authority__advisory_coordination_reading, influences).
narrative_ontology:cs_axiom('d14862ba-ea64-4672-a3a4-5fe54e51cdfe', foundational, obligations_require_explicit_member_consent).
narrative_ontology:cs_axiom_status(obligations_require_explicit_member_consent, holdable).
narrative_ontology:cs_axiom_grounding('d14862ba-ea64-4672-a3a4-5fe54e51cdfe', obligations_require_explicit_member_consent, conventional).
narrative_ontology:cs_axiom('d14862ba-ea64-4672-a3a4-5fe54e51cdfe', foundational, retaliation_for_unconsented_obligations_is_illegitimate).
narrative_ontology:cs_axiom_status(retaliation_for_unconsented_obligations_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('d14862ba-ea64-4672-a3a4-5fe54e51cdfe', retaliation_for_unconsented_obligations_is_illegitimate, deontological).
narrative_ontology:cs_reference_frame('d14862ba-ea64-4672-a3a4-5fe54e51cdfe', negotiated_text_supremacy).
narrative_ontology:cs_drift_state('d14862ba-ea64-4672-a3a4-5fe54e51cdfe', appellate_body_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d14862ba-ea64-4672-a3a4-5fe54e51cdfe', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, dsb_adjudicative_body).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, prevailing_litigant_exporters).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, trade_law_specialist_community).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, wto_secretariat_legal_affairs).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, mpia_participants).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, trade_remedy_dependent_members).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, small_developing_members).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, domestic_import_competing_industries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, trade_remedy_dependent_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Panels, and until December 2019 the Appellate Body, hear compulsory disputes between members, interpret the covered agreements, and issue reports the membership adopts unless every member objects. Their interpretations accumulate into a case-law corpus that later panels and arbitral bodies treat as settled. Their mandate text says recommendations 'cannot add to or diminish' the covered agreements, and they describe their interpretive work as clarification. Any single member can block their appointments, which is how the appellate tier stopped functioning from December 2019. The docket, precedent archive, and institutional standing of the body grow with each interpretive extension.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, dsb_adjudicative_body, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__judicial_activism_reading, dsb_adjudicative_body, beneficiary).

% Members that bring cases and collect rulings opening other members' markets — concentrated among large economies with deep litigation capacity, though mid-size members win as well. A ruling converts a negotiating position they could not obtain at the table into an enforceable entitlement backed by authorized retaliation. They also defend cases and sometimes lose, but their defining position in the arrangement is collecting enforceable wins.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, prevailing_litigant_exporters, beneficiary,
    powerful, biographical, mobile, global).

% Members whose trade-remedy instruments — antidumping duties, countervailing measures, safeguards — have been repeatedly narrowed by appellate interpretation, forcing statutory rewrites and withdrawal of protective orders. They bear the compliance costs when their own measures fall. They also win cases opening others' markets, and one of them answered the accumulated interpretive losses by blocking appellate appointments from 2017 onward rather than by leaving the organization.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, trade_remedy_dependent_members, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__judicial_activism_reading, trade_remedy_dependent_members, beneficiary).

% Members with minimal litigation capacity: obligations accumulate in a case-law corpus they had no hand in shaping, they must litigate or accept rulings against their measures, and authorized retaliation means little when they cannot credibly suspend concessions. Procedural rules treat all members identically while the practical capacity gradient runs steep. The arrangement is also the only forum where a small member can formally win against a large one, which is why most of them defend its continuation.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, small_developing_members, payer,
    moderate, biographical, constrained, global).

% The government counsel, private litigators, and arbitrators whose practice the dispute arrangement sustains. Interpretive complexity raises demand for their services; their authority, client bases, and career paths are bound to the case-law corpus. Exit means retooling into adjacent fields and abandoning accumulated standing.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, trade_law_specialist_community, beneficiary,
    organized, biographical, identity_locked, global).

% Secretariat legal staff who service panels, maintain the precedent archive, and draft the background analyses panels rely on. The institution's standing rests on the arrangement operating and the interpretive corpus growing. Staff describe themselves as servants of the multilateral trading system; leaving means abandoning that identity, not just an employer.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, wto_secretariat_legal_affairs, beneficiary,
    organized, generational, identity_locked, global).

% Industries behind members' protective measures — antidumping orders, subsidies, safeguard tariffs — that lose protection when rulings strike their governments' instruments down. They have no seat in the dispute arrangement: their governments represent them and may trade their interests away in settlement or litigation strategy. Their only exits are relocation or absorbing the import competition.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, domestic_import_competing_industries, excluded,
    organized, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__judicial_activism_reading, domestic_import_competing_industries, payer).

% Members that built the Multi-Party Interim Appeal Arbitration arrangement after the appellate tier stopped functioning, preserving binding appeal arbitration among themselves under an arbitration clause in the underlying agreement. They sustain the binding apparatus in reduced form and collect its enforcement benefits within their group while the blocked tier sits vacant.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, mpia_participants, beneficiary,
    organized, generational, mobile, global).

% Academic and think-tank analysts who document the interpretive trajectory of the case law, the legitimacy contest over the arrangement's authority, and the reform proposals. They collect no rents from the arrangement's operation and can adopt any of the competing accounts of what its mandate permits.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, international_economic_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_dsb_authority__judicial_activism_reading, dsb_adjudicative_body).
narrative_ontology:fixing_cost_class(wto_dsb_authority__judicial_activism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves trade disputes between sovereign members through agreed procedures instead of unilateral retaliation: any member can challenge another's measure before a neutral panel, rulings give exporters predictable enforcement of negotiated market-access commitments, and even small members obtain a venue where formal rules substitute for power.
% TRANSFER_FUNCTION: Moves binding policy commitments from member governments into adjudicated obligations: domestic regulatory autonomy and trade-remedy instruments are transferred to the constraint of panel and appellate rulings; market access and enforceable wins flow to prevailing litigants; dispute-resolution authority flows from diplomatic negotiation to the adjudicative apparatus and its case law.
% ABSENT_VOICES: Domestic import-competing industries bear the costs of rulings that strike down protective measures but have no seat — only member governments speak for them, and may trade their interests away. The Uruguay Round negotiators whose consent this reading invokes are mostly absent from the interpretive conversation; non-member trading partners affected by rulings have no standing at all.
% DISAPPEARANCE_RATIONALE: Dispute settlement would revert to power-based settlement: large members would retaliate unilaterally, small members would lose the only venue where they can formally win against large ones, negotiated market-access commitments would lose their enforcement backstop, and the interim appeal arbitration among MPIA participants would either expand to fill the gap or collapse.
% FOUNDING_PROBLEM: Under GATT 1947, panel reports could be blocked by the defendant because adoption required consensus, so rulings against powerful members were routinely strangled and dispute settlement was power-based. The Uruguay Round negotiators built the DSU — compulsory jurisdiction, reverse-consensus adoption, authorized retaliation — to make negotiated commitments enforceable against the strong.
% FOUNDING_PROBLEM_CORROBORATION: GATT-era diplomatic records and contemporaneous scholarship document the blockage problem, including repeated blocking of adverse panel reports in the 1980s. Members on every side of the current legitimacy contest — including this reading's own proponents — concede the pre-1995 system failed. Reform submissions in the 2022-2024 DSU talks from both defenders and critics of the adjudicative apparatus reaffirm that enforceable dispute settlement remains necessary. No party to the contest disputes that the founding problem was real.
narrative_ontology:disappearance_verdict(wto_dsb_authority__judicial_activism_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__judicial_activism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__judicial_activism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(wto_dsb_authority__judicial_activism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__judicial_activism_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__judicial_activism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_dsb_authority__judicial_activism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_dsb_authority__judicial_activism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.70) because this reading's core observation is a transfer of policy discretion no member voted on: appellate doctrines on zeroing, public bodies, and subsidy external limits rewrote domestic statutes under the banner of clarification. Suppression (0.45) is moderate rather than high because the enforcement machinery that peaked around 2019 has since been degraded — the appellate tier is paralyzed — though compulsory jurisdiction, reverse-consensus adoption, and authorized retaliation persist at panel level. Theater (0.55) is now the majority mode: panel reports are appealed into a void that cannot adopt them, and the arrangement maintains the forms of adjudication without the appellate function — performative maintenance of a blocked machine. Accessibility collapse (0.5) is moderate: negotiated settlement, the MPIA, and DSU reform keep alternatives partly live, which is precisely this reading's delta. Resistance (0.7) is high and effective: a single powerful member starved the appellate tier of appointments for years. The measurement series run on one shared time grid (T0 = 1995 DSU entry into force, T30 = 2025; seven points at five-year steps) so every tracked metric is authored at every point. Suppression_requirement is tracked because this story's dynamic IS enforcement-capacity change: a ratchet upward as the machinery hardened from 1995 to 2019, then decay as members withdrew from the blocked tier.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the adjudicative agenda-setting seat, the arrangement is a neutral procedure applying agreed text — each interpretation a clarification, the accumulation ordinary development of jurisprudence. From the trade-remedy payer seat, the same accumulation is legislation without consent: statutes rewritten by tribunals the member can no longer seat. From the small-developing-member seat, the arrangement is simultaneously its only venue for winning against large members and a source of obligations it had no hand in writing — a genuine ambivalence the engine should register as intermediate directionality. The prevailing-litigant seat experiences the arrangement as an enforcement asset. The engine derives these divergences from the structural declarations; this reading's authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries cluster where rulings and the apparatus's growth land: the adjudicative body (collects binding authority — near the beneficiary end), prevailing litigants (collect market access beyond negotiation), the specialist community and secretariat (collect careers and institutional standing), and MPIA participants (collect continued binding arbitration in reduced form). Targets cluster where unconsented obligations and retaliation risk land: trade-remedy-dependent members (their statutes are the recurring object of narrow interpretive readings), small developing members (bound by case law they did not shape), and import-competing industries (lose protection with no seat). The trade-remedy seat is genuinely dual — it wins cases too — which its secondary beneficiary role records; its derived directionality should sit high but short of the full-target end. Suppression is authored as a raw structural property of the arrangement (compulsory jurisdiction, reverse consensus, authorized retaliation) and is not scaled; only extractiveness carries the directionality and scope amplification.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — defendants strangling adverse panel reports under GATT consensus rules — is live, not dead: remove the binding apparatus and power-based settlement returns, so the dead-problem-plus-world-rearranges mismatch flag does not fire. The mandatrophy question here is not whether the arrangement outlived its function but whether its function expanded past its mandate. The tangled-rope claim keeps both facts in view: the coordination function is real (small members do win against large ones; negotiated commitments are enforced) and the extraction is real (obligations created by interpretation, enforced by authorized retaliation). A pure-extraction label would erase the coordination function that most developing members defend; a pure-coordination label would erase the drift this reading documents. The classification holds the hybrid open until the consent-boundary omegas resolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_delta,
    'Is the DSB''s binding interpretive practice grounded in member consent (the binding-referee sibling''s claim) or in excess of it (this reading''s claim)? This story is one reading of kernel wto_dsb_authority; the disagreement is located in the consent boundary — which obligations the covered agreements'' text actually authorizes.',
    'Doctrinal adjudication of the consent boundary against the Uruguay Round negotiating history and DSU Article 3.2''s no-addition-no-diminution clause; adoption of a sibling reading reclassifies this constraint''s ε and victim set.',
    'If the binding-referee reading is adopted, ε falls toward coordination cost and the payer seats shrink to ordinary defendants; if the advisory reading is adopted, the binding apparatus itself is the deviation and the constraint decomposes into negotiation support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_delta, conceptual, 'Which reading of the DSB-authority kernel the structural record supports; fixes the consent boundary.').

omega_variable(
    doctrine_consent_boundary,
    'Which specific appellate doctrines (zeroing, public body, external limits in subsidies, standards of review) constitute obligation-creation versus legitimate clarification of genuinely ambiguous text?',
    'Systematic comparison of appellate holdings against negotiating history and textual plausibility, plus a count of holdings members formally disavowed in DSB statements.',
    'Determines how much of the measured extraction is genuine overreach versus the ordinary price of adjudicating ambiguous text; a low overreach count would pull ε down and weaken this reading''s claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_consent_boundary, empirical, 'Doctrinal decomposition of the interpretive corpus into consented clarification and unconsented creation.').

omega_variable(
    paralysis_trajectory,
    'Is the post-2019 appellate paralysis a correction toward negotiated-text supremacy (this reading''s reference frame) or destruction of the arrangement''s coordination function?',
    'Track post-2019 dispute-settlement behavior: MPIA uptake, negotiated-settlement rates, panel reports appealed into the void, and the outcome of DSU reform talks.',
    'If the arrangement degrades into theatrical maintenance without function, it trends toward inertial persistence with high theater; if reform restores a mandate-bounded appellate tier, it trends back toward bounded coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paralysis_trajectory, empirical, 'Whether the enforcement decay is restoration or collapse.').

omega_variable(
    developing_member_net_position,
    'Are small developing members net payers (obligations from case law they did not shape, capacity asymmetry, unusable retaliation) or net beneficiaries (the only venue where they can formally win against large members)?',
    'Litigation-outcome data disaggregated by member capacity, plus compliance costs of rulings against developing-country defendants and the counterfactual of power-based settlement.',
    'If net beneficiaries, the victim declaration overstates their extraction and their derived directionality falls toward symmetry; if net payers, the reading''s equity critique strengthens and their seat hardens as target.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(developing_member_net_position, empirical, 'Net structural position of capacity-poor members inside the arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__judicial_activism_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t0, wto_dsb_authority__judicial_activism_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(wto__tr_t0, observed).
narrative_ontology:measurement(wto__tr_t5, wto_dsb_authority__judicial_activism_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement_basis(wto__tr_t5, observed).
narrative_ontology:measurement(wto__tr_t10, wto_dsb_authority__judicial_activism_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement_basis(wto__tr_t10, observed).
narrative_ontology:measurement(wto__tr_t15, wto_dsb_authority__judicial_activism_reading, theater_ratio, 15, 0.33).
narrative_ontology:measurement_basis(wto__tr_t15, observed).
narrative_ontology:measurement(wto__tr_t20, wto_dsb_authority__judicial_activism_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(wto__tr_t20, observed).
narrative_ontology:measurement(wto__tr_t25, wto_dsb_authority__judicial_activism_reading, theater_ratio, 25, 0.5).
narrative_ontology:measurement_basis(wto__tr_t25, observed).
narrative_ontology:measurement(wto__tr_t30, wto_dsb_authority__judicial_activism_reading, theater_ratio, 30, 0.55).
narrative_ontology:measurement_basis(wto__tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(wto__be_t0, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(wto__be_t0, observed).
narrative_ontology:measurement(wto__be_t5, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement_basis(wto__be_t5, observed).
narrative_ontology:measurement(wto__be_t10, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(wto__be_t10, observed).
narrative_ontology:measurement(wto__be_t15, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(wto__be_t15, observed).
narrative_ontology:measurement(wto__be_t20, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(wto__be_t20, observed).
narrative_ontology:measurement(wto__be_t25, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 25, 0.69).
narrative_ontology:measurement_basis(wto__be_t25, observed).
narrative_ontology:measurement(wto__be_t30, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 30, 0.7).
narrative_ontology:measurement_basis(wto__be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t0, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(wto__su_t0, observed).
narrative_ontology:measurement(wto__su_t5, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement_basis(wto__su_t5, observed).
narrative_ontology:measurement(wto__su_t10, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement_basis(wto__su_t10, observed).
narrative_ontology:measurement(wto__su_t15, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement_basis(wto__su_t15, observed).
narrative_ontology:measurement(wto__su_t20, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement_basis(wto__su_t20, observed).
narrative_ontology:measurement(wto__su_t25, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement_basis(wto__su_t25, observed).
narrative_ontology:measurement(wto__su_t30, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement_basis(wto__su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__judicial_activism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority__binding_referee_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority__advisory_coordination_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'WTO dispute settlement authority' decomposes into three structurally distinct constraints per the ε-invariance principle: the binding-referee claim (rulings are consent-grounded and legitimate), the advisory-coordination claim (panels should advise and members decide), and this reading (binding practice exceeds the negotiated mandate through interpretive drift). This story instantiates the third. Its ε (0.70) is authored for the standing binding arrangement as this reading sees it — not for the negotiated-settlement alternative the reading endorses. The upstream binding-referee story typically supplies the institutional facts this reading contests; the advisory story shares this reading's endorsed endpoint but locates the deviation in the binding design itself rather than in interpretive drift. All three stories carry their own ε, victim sets, and classifications, linked through network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
