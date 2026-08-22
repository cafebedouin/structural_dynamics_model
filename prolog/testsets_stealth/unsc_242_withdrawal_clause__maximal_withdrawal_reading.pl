% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__maximal_withdrawal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__maximal_withdrawal_reading, []).

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
 *   constraint_id: unsc_242_withdrawal_clause__maximal_withdrawal_reading
 *   human_readable: UNSC 242 Withdrawal Clause - Maximal (Full Retrocession) Reading
 *   domain: international_law/diplomatic_history/treaty_interpretation
 *
 * SUMMARY:
 *   The Security Council's Resolution 242 (November 1967) demands withdrawal
 *   of Israeli armed forces from territories occupied in the recent conflict.
 *   Its two authentic texts diverge: the English uses an indefinite article
 *   ('from territories'), the French a definite one ('des territoires' - from
 *   THE territories). This story instantiates the maximal reading of that
 *   clause: under Vienna Convention article 33 the authentic texts carry
 *   equal weight and divergences resolve toward the Charter's object and
 *   purpose, and the Charter's Article 2(4) supplies a territorial-integrity
 *   default under which conquest conveys no title - so the withdrawal demand
 *   is mandatory and comprehensive, reaching every territory taken in 1967.
 *   The standing arrangement this reading assesses is the occupation itself:
 *   fifty-eight years of administered territory, settlement construction,
 *   land requisition, and displacement, punctuated by one completed
 *   retrocession (Sinai, 1979-1982) executed under the same formula. Claimed
 *   type and metrics are authored independently: the claim is rope - a
 *   system-wide coordination norm whose cost falls on a norm-breaking
 *   occupier as restitution - while the metrics describe the contested
 *   standing arrangement as this reading honestly assesses it, with epsilon
 *   high (0.78) because the arrangement's taking is comprehensive and
 *   compounding. The engine computes per-seat classifications from the
 *   structural data below; where a computed seat-type diverges from the rope
 *   claim, that divergence is the measurement. KEY AGENTS (by structural
 *   relationship): - occupying_state: Primary target (powerful/constrained) -
 *   bears the full retrocession burden; receives the standing arrangement's
 *   gains - dispossessed_1967_territorial_claimants: Primary beneficiary
 *   (organized/trapped) - holds the enforceable legal position -
 *   occupied_territory_residents: Beneficiary (powerless/trapped) - bear the
 *   standing arrangement's daily costs; absent from the drafting table -
 *   unsc_permanent_members: Agenda setter (institutional/arbitrage) - hold
 *   the enforcement pen and exercise it selectively -
 *   occupying_state_great_power_patron: Dual-positioned
 *   beneficiary/agenda_setter (institutional/arbitrage) - endorses the
 *   formula, blocks its compulsion - land_for_peace_mediators: Secondary
 *   beneficiary (institutional/mobile) - broker the exchanges the formula
 *   frames - state_system_parties_to_charter: Systemic beneficiary
 *   (institutional/civilizational horizon) - every frontier protected while
 *   the rule holds - icj_and_treaty_jurists: Analytical observer
 *   (institutional/analytical) - fix the meaning the system cites -
 *   treaty_interpretation_historians: Analytical observer
 *   (analytical/analytical) - hold the drafting record
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.78).
domain_priors:suppression_score(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.55).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "UNSC 242 Withdrawal Clause - Maximal (Full Retrocession) Reading").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "international_law/diplomatic_history/treaty_interpretation").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__maximal_withdrawal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__maximal_withdrawal_reading, '1b918ef9-b7a3-4ee6-8c1a-2e6bae211d85').
narrative_ontology:cs_kernel_codification('1b918ef9-b7a3-4ee6-8c1a-2e6bae211d85', fixed_text).
narrative_ontology:cs_authority_grounding('1b918ef9-b7a3-4ee6-8c1a-2e6bae211d85', lineage).
narrative_ontology:cs_interpretation_layer_present('1b918ef9-b7a3-4ee6-8c1a-2e6bae211d85').
narrative_ontology:cs_reading_relation('1b918ef9-b7a3-4ee6-8c1a-2e6bae211d85', unsc_242_withdrawal_clause__partial_withdrawal_reading, forecloses).
narrative_ontology:cs_reading_relation('1b918ef9-b7a3-4ee6-8c1a-2e6bae211d85', unsc_242_withdrawal_clause__interpretive_authority_structure, influences).
narrative_ontology:cs_axiom('1b918ef9-b7a3-4ee6-8c1a-2e6bae211d85', foundational, french_definite_article_controls_scope).
narrative_ontology:cs_axiom_status(french_definite_article_controls_scope, holdable).
narrative_ontology:cs_axiom_grounding('1b918ef9-b7a3-4ee6-8c1a-2e6bae211d85', french_definite_article_controls_scope, conventional).
narrative_ontology:cs_axiom('1b918ef9-b7a3-4ee6-8c1a-2e6bae211d85', foundational, conquest_confers_no_sovereignty).
narrative_ontology:cs_axiom_status(conquest_confers_no_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('1b918ef9-b7a3-4ee6-8c1a-2e6bae211d85', conquest_confers_no_sovereignty, deontological).
narrative_ontology:cs_reference_frame('1b918ef9-b7a3-4ee6-8c1a-2e6bae211d85', charter_territorial_integrity_baseline).
narrative_ontology:cs_drift_state('1b918ef9-b7a3-4ee6-8c1a-2e6bae211d85', post_icj_2024_advisory, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1b918ef9-b7a3-4ee6-8c1a-2e6bae211d85', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, dispossessed_1967_territorial_claimants).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupied_territory_residents).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, state_system_parties_to_charter).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, land_for_peace_mediators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupying_state_great_power_patron).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupying_state).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds territory taken in the 1967 war - the West Bank, East Jerusalem, and the Golan Heights; the Sinai Peninsula was returned under the 1979 treaty. Read on the French text, the clause requires it to give up all of it. It has spent five decades building civilian presence and administrative structures in the held areas, which raises the price of retrocession every year. It cannot renounce the obligation without losing standing in the treaty system the clause belongs to, and its security establishment argues retention is defensive. Its practical routes are delay, settlement, and invoking its patron's protection.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupying_state, payer,
    powerful, generational, constrained, regional).

% The states and peoples from whom territory was taken in June 1967. They hold the clause as their legal title to recovery: a Security Council demand for withdrawal, backed by the Charter's bar on acquiring territory by war. They have no military path to recovery that the system would countenance, so the resolution is their only instrument; they press it through General Assembly majorities, bloc diplomacy, and litigation before the World Court. Waiting is their permanent condition.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, dispossessed_1967_territorial_claimants, beneficiary,
    organized, generational, trapped, regional).

% The people living under the occupying administration in the held territories - subject to military orders, land requisition, movement permits, and a parallel civilian settlement project on the land they live on. The clause promises the end of this administration; they were not represented when it was drafted and appear in its text only as refugees. Their daily circumstances worsen the longer withdrawal is deferred, and they cannot vote the administration out or leave en masse.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupied_territory_residents, beneficiary,
    powerless, generational, trapped, regional).

% Hold the enforcement pen for the clause: only they can adopt binding measures, and any one of them can block them. They have reaffirmed the withdrawal formula in summit communiques while vetoing or diluting the measures that would carry it out, and each weighs the clause against bilateral relationships, arms sales, and alliance management. Their selective use of the pen is the main reason the clause's demand remains unmet.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_permanent_members, agenda_setter,
    institutional, generational, arbitrage, global).

% The occupier's principal ally and arms supplier. It joined the withdrawal formula at the drafting stage and repeats it as policy, while exercising its veto to prevent compulsory measures and supplying the occupied administration's defense needs. It collects the global benefit of a world where borders are not changed by war, and simultaneously manages when and whether that benefit is applied to this particular border.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupying_state_great_power_patron, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupying_state_great_power_patron, agenda_setter).

% Third-party governments and envoys who broker exchanges framed by the clause: territory for recognition, security arrangements, and normalized relations. The 1979 Egyptian treaty and the Oslo architecture were built on the formula. Their diplomatic relevance depends on the clause remaining the agreed frame; when talks collapse they lose their portfolio, and when the frame is bypassed by unilateral moves they are sidelined.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, land_for_peace_mediators, beneficiary,
    institutional, biographical, mobile, regional).

% Every state party to the Charter holds an interest in the rule the clause applies: that war does not move borders. Each one's own frontier is safer while the rule holds, and each is a potential future claimant if it is ever invaded. They sustain the rule through recognition practice, voting, and treaty-making, and almost none of them bears any cost under this particular application of it.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, state_system_parties_to_charter, beneficiary,
    institutional, civilizational, mobile, global).

% The World Court and the treaty-interpretation profession. The Court's 2004 wall opinion and its 2024 advisory proceedings treated the withdrawal demand as legally operative and the occupation as unlawful; jurists maintain the authentic-texts doctrine under which the French and English texts carry equal weight and divergences resolve toward the object and purpose of the Charter. They decide nothing enforceable by themselves but fix the meaning the system cites.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, icj_and_treaty_jurists, observer,
    institutional, generational, analytical, global).

% Scholars of the drafting episode who assembled the record: the British and American drafts, the Soviet objections, the choice of an English indefinite article alongside a French definite one, and the statements of several drafters that the ambiguity was meant to buy unanimity. Their archives are the evidentiary base on which the authorial-intent side builds and against which the textualists argue.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, treaty_interpretation_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupying_state).
narrative_ontology:fixing_cost_class(unsc_242_withdrawal_clause__maximal_withdrawal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the state system on the rule that military conquest confers no sovereignty: an occupation creates a withdrawal obligation that restores the pre-war territorial baseline, so wars produce lines to negotiate from rather than new titles. In the specific case, it gives the vanquished a lawful, nonviolent path to recovery of territory and gives the region a common formula - territory for peace - on which treaties can be built.
% TRANSFER_FUNCTION: Moves territorial control - and everything riding on it: land, water, airspace, settlement rights, administrative authority - from the occupying state back to the pre-war sovereign claimants; and moves diplomatic legitimacy toward the dispossessed and away from retention.
% ABSENT_VOICES: The residents of the occupied territories had no seat at the drafting table: Resolution 242 was negotiated among states, and the territories' population enters its text only inside the refugee clause. Their successors would object that the resolution prices land and defers people - a state-to-state territorial ledger that left self-determination outside the frame. Also absent were the smaller states whose frontiers the precedent would later govern; the text was settled in consultations among the permanent members and the belligerents' sponsors.
% DISAPPEARANCE_RATIONALE: If the withdrawal demand vanished overnight, the 1967 conquests would ripen into recognized titles, the returned-Sinai precedent would invert, and every state watching would conclude that borders are held by force and renegotiable by force. Neighboring states would rebuild for revanchist war, the Charter's territorial-integrity guarantee would lose credibility system-wide, and the Court's occupation jurisprudence would lose its anchor.
% FOUNDING_PROBLEM: In November 1967 the Council faced a finished conquest: one belligerent held the Sinai Peninsula, Gaza, the West Bank, East Jerusalem, and the Golan Heights, and the prior armistice regime was dead. The founding problem was how to require restoration of the taken territories - without rewarding conquest, without demanding withdrawal into indefensible lines, and with language both superpowers and all the belligerents could accept. The drafters answered with paired authentic texts whose articles diverge: an English indefinite article and a French definite one.
% FOUNDING_PROBLEM_CORROBORATION: The International Court of Justice attests the problem's persistence from outside the beneficiary bloc: its 2004 wall opinion and its 2024 advisory proceedings treat the withdrawal demand as outstanding and the occupation as unlawful. The occupier's own 1979 treaty returning the Sinai attests the demand's operative force from the payer's seat. Successive Secretary-General reports and repeated European Union position statements corroborate non-fulfillment. No party to the dispute claims the founding problem is resolved except by pointing to outcomes the clause itself defines as incomplete.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__maximal_withdrawal_reading, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__maximal_withdrawal_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__maximal_withdrawal_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__maximal_withdrawal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__maximal_withdrawal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__maximal_withdrawal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.78: the referent is the standing arrangement under contest - the occupation as this reading assesses it - and its taking is comprehensive (land, water, airspace, administrative control) and compounding (each year of settlement deepens the stock). Suppression 0.55 is authored as the constraint's own coercive reach and is NOT scaled by power or scope: the clause forecloses retention legally and diplomatically but cannot compel - Council enforcement is veto-shielded - so its suppressive force is real but bounded. Theater 0.46: roughly half the clause's observable activity is ritual (annual reaffirmation resolutions, anniversary statements, communiques that change nothing) while half remains functional (the Sinai treaty executed the formula; the Court's 2004 and 2024 opinions cite it as operative law; mediation portfolios depend on it). Accessibility_collapse 0.45: workable alternatives to compliance persist in practice - facts-on-the-ground accumulation, patron protection, normalization agreements that bypass the frame - so understanding the clause does not close the occupier's routes. Resistance 0.72: five decades of active resistance - settlement expansion, the secure-boundaries doctrine, veto diplomacy, and open contestation of the clause's meaning. All three tracked series run on one shared grid (t = 0, 12, 24, 36, 48, 58) so no metric borrows another's end-state; the trajectories are monotonic rather than cyclical - extraction accumulates, theater grows as enforcement stalls, and the enforcement requirement hardens as the compliance debt compounds.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the beneficiary seats should compute opposite types from the same text. From the occupier's position the clause is a confiscatory demand - trapped, powerful, and targeted, it experiences maximal effective extraction even though this reading counts its loss as restitution. From the claimants' and residents' positions the same clause is the only lawful road home - subsidized, near-zero effective extraction, and worth defending at the Assembly and the Court. The permanent members and the patron sit between: arbitrage-grade exit lets them endorse the formula while rationing its enforcement, so they experience neither the burden nor the remedy. The Court and the historians hold analytical seats that see the whole structure. The engine computes this divergence from power, exit, and role; the authored rope claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: dispossessed claimants, occupied residents, the Charter state system, and the mediators all receive something from the clause's operation and bear none of its burden, placing them near the beneficiary end of d. The occupier is declared payer and is trapped - it cannot renounce the obligation without exiting the treaty system itself - so its d sits near the full-target end and its effective extraction is amplified. The permanent members and the patron hold the enforcement pen with arbitrage exit; their d lands mid-range, damped by mobility. No directionality overrides are used: role, power, and exit differentiate every seat, and the one genuinely dual-positioned agent (the patron - beneficiary by interest, agenda-setter by veto) carries a secondary_role rather than an override. Scope amplification is modest here: the clause's verification problems localize to one territory, unlike system-wide norms where universal scope inflates effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetrical mislabels. First, the rising theater ratio invites a piton reading - a dead letter kept alive by ritual reaffirmation. The founding-problem interview defeats it: the problem the clause was built to solve (territory held by force pending withdrawal) is live, the Sinai retrocession proves the formula executable, and the Court's 2024 opinion re-anchored it - the mandate has outlived neither its function nor its occasion, so mandatrophy is not resolved. Second, the occupier's heavy burden invites a snare reading - identifiable payer, high extraction. But the clause creates no victim class: its cost falls on a norm-breaker as restitution, its beneficiaries hold an enforceable legal position, and its coordination function (war moves no borders) is among the postwar system's most successful. The genuine pathology - selective enforcement serving permanent-member interests - lives in the enforcement layer, which this clean reading excludes and the sibling authority-structure reading carries. Keeping the layers separate is what prevents the rope claim from laundering the enforcement politics, and the enforcement politics from condemning the norm.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint instantiates the maximal_withdrawal_reading of kernel unsc_242_withdrawal_clause. Would instantiating the partial_withdrawal_reading instead restructure the constraint''s obligor burden and beneficiary set?',
    'Side-by-side compilation of the sibling reading: compare obligor burden (total versus discretionary retrocession), the claimants'' enforceable position, and epsilon under each reading against the same historical record.',
    'Under the partial reading the occupier''s burden shrinks to negotiable scope, the claimants'' position weakens from mandatory right to bargaining chip, and epsilon drops accordingly; the two readings cannot share one classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story is one reading of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    authentic_text_precedence,
    'Does the Vienna Convention''s authentic-texts rule (equal weight, divergence resolved by object and purpose) decisively favor the French definite article, or does the drafting history - an ambiguity several drafters later described as deliberate - defeat textual primacy?',
    'Doctrinal analysis of VCLT articles 31-33 applied to the paired texts, tested against the published travaux preparatoires and the drafters'' contemporaneous and retrospective statements.',
    'If textual primacy holds, the maximal reading stands on settled interpretive law; if negotiating history defeats it, the maximal reading rests on policy preference and its accessibility_collapse should fall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authentic_text_precedence, conceptual, 'Whether authentic-text doctrine or authorial intent governs the scope of the withdrawal demand.').

omega_variable(
    charter_default_status,
    'Does Charter Article 2(4) supply a default baseline under which withdrawal is presumptively total and reducible only by consent, or a floor that security considerations can lower?',
    'Comparative analysis of the Charter norm''s application across post-1945 territorial cases (Kuwait, East Timor, Crimea) and the Court''s characterization of the norm''s peremptory status.',
    'Default status completes the maximal reading from the Charter alone; floor status admits the secure-boundaries exception and moves this constraint toward the partial reading''s structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charter_default_status, conceptual, 'Whether the territorial-integrity norm operates as presumption or as floor.').

omega_variable(
    enforcement_selectivity_ambiguity,
    'Is the clause''s non-implementation a failure of the norm''s strength, or the product of selective enforcement that serves the permanent members'' bilateral interests?',
    'Cross-case comparison of Council enforcement against comparable occupations and annexations where the veto configuration differed.',
    'If selectivity is structural, the operative arrangement includes an enforcement-layer extraction component this clean reading excludes, and the family needs a further story for the enforcement layer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_selectivity_ambiguity, empirical, 'Whether non-implementation reflects norm weakness or interest-driven selectivity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0, 58).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t0, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(unsc_tr_t0, observed).
narrative_ontology:measurement(unsc_tr_t12, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement_basis(unsc_tr_t12, observed).
narrative_ontology:measurement(unsc_tr_t24, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement_basis(unsc_tr_t24, observed).
narrative_ontology:measurement(unsc_tr_t36, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 36, 0.38).
narrative_ontology:measurement_basis(unsc_tr_t36, observed).
narrative_ontology:measurement(unsc_tr_t48, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 48, 0.44).
narrative_ontology:measurement_basis(unsc_tr_t48, observed).
narrative_ontology:measurement(unsc_tr_t58, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 58, 0.46).
narrative_ontology:measurement_basis(unsc_tr_t58, observed).

% Extraction over time
narrative_ontology:measurement(unsc_be_t0, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement_basis(unsc_be_t0, observed).
narrative_ontology:measurement(unsc_be_t12, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 12, 0.64).
narrative_ontology:measurement_basis(unsc_be_t12, observed).
narrative_ontology:measurement(unsc_be_t24, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 24, 0.69).
narrative_ontology:measurement_basis(unsc_be_t24, observed).
narrative_ontology:measurement(unsc_be_t36, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 36, 0.72).
narrative_ontology:measurement_basis(unsc_be_t36, observed).
narrative_ontology:measurement(unsc_be_t48, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 48, 0.76).
narrative_ontology:measurement_basis(unsc_be_t48, observed).
narrative_ontology:measurement(unsc_be_t58, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 58, 0.78).
narrative_ontology:measurement_basis(unsc_be_t58, observed).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t0, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(unsc_su_t0, observed).
narrative_ontology:measurement(unsc_su_t12, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 12, 0.36).
narrative_ontology:measurement_basis(unsc_su_t12, observed).
narrative_ontology:measurement(unsc_su_t24, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 24, 0.42).
narrative_ontology:measurement_basis(unsc_su_t24, observed).
narrative_ontology:measurement(unsc_su_t36, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 36, 0.47).
narrative_ontology:measurement_basis(unsc_su_t36, observed).
narrative_ontology:measurement(unsc_su_t48, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 48, 0.52).
narrative_ontology:measurement_basis(unsc_su_t48, observed).
narrative_ontology:measurement(unsc_su_t58, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 58, 0.55).
narrative_ontology:measurement_basis(unsc_su_t58, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__maximal_withdrawal_reading, resource_allocation).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, partial_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, interpretive_authority_structure).

% DUAL FORMULATION NOTE:
% The colloquial label 'the 242 withdrawal clause' decomposes into a substantive-scope pair - this maximal reading versus partial_withdrawal_reading, with different obligor burdens, different beneficiary enforceable positions, and different epsilon - and an authority question carried by interpretive_authority_structure (who may settle the textual divergence: the Court, the drafting states, or the occupier's practice). Epsilon differs across the family because each reading fixes a different standing arrangement: this reading assesses the occupation as comprehensive unlawful retention (epsilon 0.78); the partial reading would assess a narrower, negotiable obligor burden. Family members link via affects_constraints; upstream textual doctrine feeds downstream enforcement politics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
