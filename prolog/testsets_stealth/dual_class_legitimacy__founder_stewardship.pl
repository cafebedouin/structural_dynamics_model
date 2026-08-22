% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__founder_stewardship
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dual_class_legitimacy__founder_stewardship, []).

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
 *   constraint_id: dual_class_legitimacy__founder_stewardship
 *   human_readable: Dual-Class Founder Control Defended as Fiduciary Stewardship (founder_stewardship reading)
 *   domain: corporate governance/securities law/organizational economics
 *
 * SUMMARY:
 *   A dual-class charter gives the founder's share class ten votes per share
 *   against one for the public class, decoupling governance control from
 *   capital at risk. This story instantiates the founder_stewardship reading
 *   of the dual_class_legitimacy kernel: the arrangement is defended as
 *   coordination — a fiduciary steward using insulated control to execute a
 *   long-horizon mission that fast-turnover dispersed capital would otherwise
 *   discipline away, with Class A holders served indirectly through mission
 *   success. The epsilon referent is the standing dual-class arrangement
 *   itself, assessed by this reading's own lights: the reading concedes that
 *   Class A bears a real voting-power cost, prices it as the premium for
 *   mission protection, and holds the bargain net-fair. Claim and metrics are
 *   independent authored facts: claimed_type tangled_rope is this author's
 *   structural assessment of the arrangement (genuine coordination function
 *   plus asymmetric extraction plus active enforcement), while the reading's
 *   own claim of pure coordination lives in the cs_structure axioms and is
 *   tested — not reconciled — by the engine's per-seat computation. The
 *   sibling readings over the same arrangement are separate constraint
 *   stories, linked in the network and documented in kernel_context.
 *
 * KEY AGENTS:
 *   - founder_control_holders: primary beneficiary and agenda setter (institutional / identity_locked) — holds supervoting control, collects the control premium, decides whether any sunset ever reaches a vote
 *   - class_a_public_shareholders: primary cost-bearer (moderate / mobile) — one vote per share at the same price; no voting path to charter change; exit only by sale at a price that already embeds the discount
 *   - index_fund_managers: trapped institutional cost-bearer (institutional / trapped) — must hold in index funds, votes the Class A bloc, bears the stewardship cost of a structure its own guidelines criticize
 *   - founder_vision_investors: secondary beneficiary (powerful / mobile) — buys the mission thesis and accepts the vote differential as its admission price
 *   - company_employees_with_equity: indirect beneficiary and secondary cost-bearer (moderate / constrained) — equity value rides on insulated multi-year projects
 *   - minority_governance_advocates: excluded (organized / constrained) — would impose sunsets but lacks the votes the structure withholds
 *   - proxy_advisory_firms: analytical observer (institutional / analytical) — coordinates institutional Class A voting through recommendations
 *   - securities_regulators: institutional observer (institutional / analytical) — polices disclosure at the IPO margin; has not prohibited the structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__founder_stewardship, 0.32).
domain_priors:suppression_score(dual_class_legitimacy__founder_stewardship, 0.48).
domain_priors:theater_ratio(dual_class_legitimacy__founder_stewardship, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, extractiveness, 0.32).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__founder_stewardship, tangled_rope).
narrative_ontology:human_readable(dual_class_legitimacy__founder_stewardship, "Dual-Class Founder Control Defended as Fiduciary Stewardship (founder_stewardship reading)").
narrative_ontology:topic_domain(dual_class_legitimacy__founder_stewardship, "corporate governance/securities law/organizational economics").

domain_priors:requires_active_enforcement(dual_class_legitimacy__founder_stewardship).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__founder_stewardship, '7d595758-6c84-4009-a71f-c353da1f9671').
narrative_ontology:cs_kernel_codification('7d595758-6c84-4009-a71f-c353da1f9671', distributed).
narrative_ontology:cs_authority_grounding('7d595758-6c84-4009-a71f-c353da1f9671', practice).
narrative_ontology:cs_interpretation_layer_present('7d595758-6c84-4009-a71f-c353da1f9671').
narrative_ontology:cs_reading_relation('7d595758-6c84-4009-a71f-c353da1f9671', dual_class_legitimacy__minority_extraction, coexists_with).
narrative_ontology:cs_reading_relation('7d595758-6c84-4009-a71f-c353da1f9671', dual_class_legitimacy__disclosure_consent, coexists_with).
narrative_ontology:cs_axiom('7d595758-6c84-4009-a71f-c353da1f9671', foundational, decoupled_control_legitimately_serves_all_shareholders).
narrative_ontology:cs_axiom_status(decoupled_control_legitimately_serves_all_shareholders, holdable).
narrative_ontology:cs_axiom_grounding('7d595758-6c84-4009-a71f-c353da1f9671', decoupled_control_legitimately_serves_all_shareholders, instrumental).
narrative_ontology:cs_axiom('7d595758-6c84-4009-a71f-c353da1f9671', foundational, fiduciary_duty_substitutes_for_voting_protection).
narrative_ontology:cs_axiom_status(fiduciary_duty_substitutes_for_voting_protection, holdable).
narrative_ontology:cs_axiom_grounding('7d595758-6c84-4009-a71f-c353da1f9671', fiduciary_duty_substitutes_for_voting_protection, deontological).
narrative_ontology:cs_reference_frame('7d595758-6c84-4009-a71f-c353da1f9671', founder_as_fiduciary_steward).
narrative_ontology:cs_drift_state('7d595758-6c84-4009-a71f-c353da1f9671', contemporary_index_investor_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('7d595758-6c84-4009-a71f-c353da1f9671', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, founder_control_holders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, founder_vision_investors).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, company_employees_with_equity).
narrative_ontology:constraint_victim(dual_class_legitimacy__founder_stewardship, class_a_public_shareholders).
narrative_ontology:constraint_victim(dual_class_legitimacy__founder_stewardship, index_fund_managers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dual_class_legitimacy__founder_stewardship, company_employees_with_equity).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__founder_stewardship, fiduciary_stewardship_doctrine).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__founder_stewardship, patient_capital_thesis).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__founder_stewardship, mission_insulation_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the supervoting share class carrying ten votes per share against one for the public class. Sets board composition, approves or blocks mergers and capital allocation, and decides whether any sunset provision ever reaches a shareholder vote. Receives the control premium embedded in the share price and practical immunity from proxy challenges that the vote differential provides. Selling control would mean surrendering the mission the company exists to execute — an exit that is personally, not just financially, costly.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, founder_control_holders, agenda_setter,
    institutional, generational, identity_locked, global).

% Buys shares at the same market price as supervoting holders but receives one vote per share. Cannot assemble a voting coalition capable of changing the charter, replacing the board, or accepting a takeover premium over founder objection, because the vote math makes every such coalition arithmetically futile. Can sell at any time, but the price already reflects the voting discount, and selling forfeits any voice rather than exercising it.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, class_a_public_shareholders, payer,
    moderate, biographical, mobile, global).

% Must hold the stock in funds tracking major indices, so exit means abandoning the index-tracking product their clients bought. Votes the Class A bloc, runs or joins sunset campaigns, and bears the stewardship workload and reputational exposure of holding a structure its own proxy guidelines criticize. Fee income continues regardless of how the governance question resolves.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, index_fund_managers, payer,
    institutional, generational, trapped, global).

% Deliberately buys the dual-class stock to back the founder's long-horizon plan, treating the vote differential as the admission price for mission exposure. Gains when the insulated strategy pays and takes board-adjacent access without voting parity; can sell if the thesis breaks, and does so more readily than any other holder class.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, founder_vision_investors, beneficiary,
    powerful, biographical, mobile, global).

% Holds options and restricted stock whose value rides on the multi-year projects the insulated strategy funds. Gains continuity of mission and protection from activist cost-cutting; bears the risk that a leadership team no one can vote out misallocates capital for years before the error becomes visible. Leaving the company means forfeiting unvested equity.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, company_employees_with_equity, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__founder_stewardship, company_employees_with_equity, payer).

% Runs sunset-clause campaigns, proxy-access proposals, and listing-standard petitions on behalf of dispersed holders. Would impose vote sunsets or proportional governance but stands outside the charter conversation: the votes needed to put a sunset to a shareholder vote are held by the class the sunset would unseat. Works instead through exchanges, regulators, and public pressure.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, minority_governance_advocates, excluded,
    organized, generational, constrained, national).

% Publishes voting recommendations that shape how institutional Class A holders vote. Recommends against new dual-class structures and for sunset provisions, and its guidelines are the main coordination point for dispersed institutional opposition. Holds no position in the charter itself and bears no direct cost from the arrangement.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, proxy_advisory_firms, observer,
    institutional, biographical, analytical, global).

% Sets the disclosure requirements for the offering that creates the structure and polices the accuracy of the offering documents. Has declined to prohibit dual-class listings, leaving permissibility to exchanges and state corporate law; engages the arrangement only at the margins, such as disclosure adequacy and related-party transactions.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, securities_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dual_class_legitimacy__founder_stewardship, founder_control_holders).
narrative_ontology:fixing_cost_class(dual_class_legitimacy__founder_stewardship, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the control-allocation problem for firms whose value depends on investments that mature over decades: it gives a committed founding team voting control durable enough to carry long-gestation projects — R&D programs, infrastructure, mission bets — through activist campaigns, takeover approaches, and shareholder bases that turn over faster than the investment cycle completes.
% TRANSFER_FUNCTION: Moves voting control — and with it agenda-setting power over capital allocation, board composition, and takeover response — from dispersed Class A capital to the founder's supervoting block, at a price embedded in the share price rather than negotiated per decision; and moves the downside risk of governance error onto Class A holders, who hold no vote by which to remove the error-maker.
% ABSENT_VOICES: Class A holders who would demand sunset clauses or proportional governance are present in the market but absent from the charter conversation — the votes that would give them a voice are the very thing the structure withholds. Future Class A holders (index-mandated funds, investors who arrive after the IPO) cannot participate in the consent the legitimacy claim relies on. Proxy advisors and governance advocates speak for them partially, after the fact, and through venues outside the charter.
% DISAPPEARANCE_RATIONALE: If dual-class structures vanished overnight, control premiums would reprice immediately, founder-led firms would face takeover and proxy pressure their charters currently absorb, and several long-horizon strategies now shielded from quarterly discipline would be re-cut or abandoned. IPO structuring, index inclusion rules, and proxy-voting guidelines would all reorganize around one-share-one-vote; the corporate ecosystem built on insulated founder control would visibly rearrange.
% FOUNDING_PROBLEM: Dispersed public ownership exposes mission-critical firms to short-horizon discipline: activist campaigns and hostile bids priced off quarterly earnings, and shareholder bases that churn faster than decade-scale investments complete. Dual-class structures were built and defended to solve that — to give the founding team control durable enough to execute a multi-decade mission that dispersed capital would otherwise interrupt.
% FOUNDING_PROBLEM_CORROBORATION: The short-termism problem itself is corroborated from outside the benefiting parties: survey and archival evidence in the finance literature on managerial myopia under earnings pressure, and the revealed preference of independent investors who deliberately buy dual-class stock for mission exposure. But the claim that the problem remains live for firms decades past their IPO is attested mainly by founders and their counsel; the Investor Stewardship Group, the Council of Institutional Investors, and the major proxy advisors — all outside the benefiting set — attest the opposite. Partial corroboration: the founding problem's historical reality is corroborated; its continuing liveness for mature firms is not.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__founder_stewardship, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__founder_stewardship, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__founder_stewardship, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dual_class_legitimacy__founder_stewardship, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__founder_stewardship, 0.32, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dual_class_legitimacy__founder_stewardship_tests).
:- end_tests(dual_class_legitimacy__founder_stewardship_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.32 is reading-indexed: even by the stewardship reading's own lights the arrangement transfers a real voting-power cost to Class A, which the reading prices as a fair premium rather than denies; a strict 'serves all, costs none' reading would be boilerplate, not a held position. Suppression 0.48 is a raw structural property, unscaled by power or scope: the voting math structurally forecloses Class A voice and rival bids, while open sale exit and the disclosure regime keep it well below coercive maxima. Theater_ratio 0.28: mission language increasingly doubles as entrenchment cover in mature firms, but the insulation function is real in the R&D-intensive cohort, so performance is a minority share of activity. Accessibility_collapse 0.40: within a dual-class firm, governance alternatives collapse completely once the vote math is understood, but market-level alternatives persist (single-class firms, index exclusions), so collapse is partial. Resistance 0.60: the arrangement meets organized, sustained opposition — proxy-advisor recommend-against policies, index-inclusion exclusions, sunset campaigns. All three tracked series run on one shared time grid (points 0–36 at step 6). The rising trajectories are the story's core temporal finding: even by the reading's own lights, the bargain has degraded — supervoting became perpetual rather than transitional, sunset clauses disappeared from the modern cohort, non-voting share classes were layered on, and enforcement hardened (pills, listing fights) — so extraction accumulation and enforcement intensification proceed together while the mission-necessity case weakens with firm maturity.
 *
 * PERSPECTIVAL GAP:
 *   The reading predicts uniform experience: every shareholder seat is served by stewardship. The structural data predict sharp seat divergence, which the engine computes. From the founder seat, the arrangement is a coordination structure it built and personally embodies (identity_locked: exit would dissolve the mission-self). From the Class A seat, the same structure is a priced-but-involuntary subordination: no coalition of Class A holders can outvote the supervoting block, so the usual remedy of dispersed-holder coalition power is arithmetically unavailable — their leverage runs entirely through exit and through venues outside the charter. The index-manager seat experiences it as a trapped mandate: compelled holding, stewardship burden, fee income regardless of outcome. Same nominal status ('shareholder'), three different constraints experienced — that divergence is the measurement this corpus exists to take, and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries map to low directionality: founder_control_holders (beneficiary, identity_locked, agenda_setter) sit nearest the beneficiary end — the structure subsidizes them with the control premium and agenda power, and their exit lock removes even arbitrage-grade departure. founder_vision_investors (beneficiary, mobile) sit low but slightly above the founder: they pay the same dilution they endorse. company_employees_with_equity (beneficiary/payer, constrained) sit mid-low. Declared victims map to high directionality: class_a_public_shareholders (victim, mobile) are damped from full-target by open sale exit but bear the full premium while held; index_fund_managers (victim, trapped) are pushed toward full-target by their exit lock and damped only by fee compensation. minority_governance_advocates are excluded rather than coordinated — their exclusion is the enforcement object. Suppression (0.48) stays unscaled; effective extraction is scaled by directionality and scope, and the global scope of these firms makes stewardship claims hard to verify, amplifying extraction at the target seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The stewardship reading would classify this arrangement as pure coordination; this story's tangled_rope claim keeps both halves visible — the genuine coordination function (mission insulation, real in the founding cohort) and the asymmetric extraction (a concentrated control premium enforced against Class A). Mandatrophy is not declared resolved: the founding problem's liveness is contested. If omega sunset_omission_vs_founding_problem resolves toward a dead founding problem for mature firms, the arrangement drifts toward the atrophied end of the lifecycle — but the piton reading is barred by the receipt surface: gain_flow names a concentrated capturer (the founder seat), and a piton characteristically has none; a dead-mandate outcome with a concentrated capturer sharpens toward capture, not inertia. The classification therefore keeps the stewardship claim empirically testable instead of letting the mission narrative absorb all disconfirming evidence — which is exactly what the reading's own axioms, being instrumental, invite.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stewardship_entrenchment_ambiguity,
    'When founder control is actually exercised, is it stewardship (long-horizon mission execution that dispersed capital could not otherwise buy) or entrenchment (private benefits of control, dynastic succession, capital misallocation shielded from discipline)?',
    'Matched-firm outcome studies separating mission-investment intensity and long-horizon returns under founder control versus one-share-one-vote controls, with succession events and supervoting-block sales as natural experiments.',
    'If entrenchment dominates, this reading''s foundational axiom fails on its own instrumental ground and the same arrangement reclassifies toward pure extraction wearing a coordination cover story; if stewardship dominates, the coordination reading strengthens and most measured extraction is the priced premium the reading already concedes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stewardship_entrenchment_ambiguity, empirical, 'Whether observed founder control functions as stewardship or entrenchment.').

omega_variable(
    kernel_reading_position,
    'This constraint is the founder_stewardship reading of the dual_class_legitimacy kernel — how would the sibling readings (minority_extraction, disclosure_consent) restructure the classification of the same arrangement?',
    'No in-story resolution: the siblings are separate constraint stories over the same arrangement, linked via the network. The disagreement is located in the legitimacy ground — outcomes (this reading), capital-proportional entitlement (minority_extraction), and IPO-time informed consent under the Securities Act disclosure regime (disclosure_consent).',
    'Under minority_extraction, the same structure''s epsilon rises sharply and Class A holders re-index as rights-bearing victims rather than priced-in counterparties; under disclosure_consent, legitimacy attaches to the disclosure event and epsilon falls toward the consent price for consenting IPO-era holders while later, consentless acquirers carry the residual.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one reading of a contested kernel; siblings instantiate different constraints from the same arrangement.').

omega_variable(
    consent_transmission_to_later_holders,
    'Does the consent obtained through IPO-time disclosure reach holders who acquire Class A shares later — index-mandated funds and investors who arrive after the offering — or is their position consentless?',
    'Test whether later-acquirer behavior fully prices the voting discount (no systematic post-IPO re-rating of the discount as the holder base turns over) and whether index-mandated holdings show governance outcomes distinct from consenting IPO-era holders.',
    'If consent does not transmit across the holder turnover, the reading''s legitimacy base covers only the IPO-era holder set, and the extraction the reading concedes re-indexes onto the later, consentless majority of the Class A seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_transmission_to_later_holders, conceptual, 'Whether IPO-time consent extends to subsequent Class A acquirers.').

omega_variable(
    sunset_omission_vs_founding_problem,
    'Does the continuing absence of sunset clauses in modern dual-class charters track a still-live short-termism problem, or the preservation of control rents in firms decades past their founding?',
    'Compare charter sunset terms against founding-problem indicators (firm maturity, R&D intensity, takeover vulnerability) across the IPO cohort; if sunset absence tracks maturity rather than mission intensity, the founding problem has gone dead for the persistent cases.',
    'If the founding problem is dead for mature firms, the arrangement persists on inertia and the classification drifts toward the atrophied end of the lifecycle; if it is live, the coordination function remains genuine and the hybrid reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_omission_vs_founding_problem, empirical, 'Whether sunset omission reflects a live founding problem or rent preservation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__founder_stewardship, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_class_legitimacy__founder_stewardship, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(dual_tr_t0, observed).
narrative_ontology:measurement(dual_tr_t6, dual_class_legitimacy__founder_stewardship, theater_ratio, 6, 0.15).
narrative_ontology:measurement_basis(dual_tr_t6, observed).
narrative_ontology:measurement(dual_tr_t12, dual_class_legitimacy__founder_stewardship, theater_ratio, 12, 0.17).
narrative_ontology:measurement_basis(dual_tr_t12, observed).
narrative_ontology:measurement(dual_tr_t18, dual_class_legitimacy__founder_stewardship, theater_ratio, 18, 0.2).
narrative_ontology:measurement_basis(dual_tr_t18, observed).
narrative_ontology:measurement(dual_tr_t24, dual_class_legitimacy__founder_stewardship, theater_ratio, 24, 0.22).
narrative_ontology:measurement_basis(dual_tr_t24, observed).
narrative_ontology:measurement(dual_tr_t30, dual_class_legitimacy__founder_stewardship, theater_ratio, 30, 0.25).
narrative_ontology:measurement_basis(dual_tr_t30, observed).
narrative_ontology:measurement(dual_tr_t36, dual_class_legitimacy__founder_stewardship, theater_ratio, 36, 0.28).
narrative_ontology:measurement_basis(dual_tr_t36, observed).

% Extraction over time
narrative_ontology:measurement(dual_be_t0, dual_class_legitimacy__founder_stewardship, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(dual_be_t0, observed).
narrative_ontology:measurement(dual_be_t6, dual_class_legitimacy__founder_stewardship, base_extractiveness, 6, 0.21).
narrative_ontology:measurement_basis(dual_be_t6, observed).
narrative_ontology:measurement(dual_be_t12, dual_class_legitimacy__founder_stewardship, base_extractiveness, 12, 0.24).
narrative_ontology:measurement_basis(dual_be_t12, observed).
narrative_ontology:measurement(dual_be_t18, dual_class_legitimacy__founder_stewardship, base_extractiveness, 18, 0.26).
narrative_ontology:measurement_basis(dual_be_t18, observed).
narrative_ontology:measurement(dual_be_t24, dual_class_legitimacy__founder_stewardship, base_extractiveness, 24, 0.28).
narrative_ontology:measurement_basis(dual_be_t24, observed).
narrative_ontology:measurement(dual_be_t30, dual_class_legitimacy__founder_stewardship, base_extractiveness, 30, 0.3).
narrative_ontology:measurement_basis(dual_be_t30, observed).
narrative_ontology:measurement(dual_be_t36, dual_class_legitimacy__founder_stewardship, base_extractiveness, 36, 0.32).
narrative_ontology:measurement_basis(dual_be_t36, observed).

% Suppression requirement over time
narrative_ontology:measurement(dual_su_t0, dual_class_legitimacy__founder_stewardship, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(dual_su_t0, observed).
narrative_ontology:measurement(dual_su_t6, dual_class_legitimacy__founder_stewardship, suppression_requirement, 6, 0.38).
narrative_ontology:measurement_basis(dual_su_t6, observed).
narrative_ontology:measurement(dual_su_t12, dual_class_legitimacy__founder_stewardship, suppression_requirement, 12, 0.41).
narrative_ontology:measurement_basis(dual_su_t12, observed).
narrative_ontology:measurement(dual_su_t18, dual_class_legitimacy__founder_stewardship, suppression_requirement, 18, 0.44).
narrative_ontology:measurement_basis(dual_su_t18, observed).
narrative_ontology:measurement(dual_su_t24, dual_class_legitimacy__founder_stewardship, suppression_requirement, 24, 0.46).
narrative_ontology:measurement_basis(dual_su_t24, observed).
narrative_ontology:measurement(dual_su_t30, dual_class_legitimacy__founder_stewardship, suppression_requirement, 30, 0.47).
narrative_ontology:measurement_basis(dual_su_t30, observed).
narrative_ontology:measurement(dual_su_t36, dual_class_legitimacy__founder_stewardship, suppression_requirement, 36, 0.48).
narrative_ontology:measurement_basis(dual_su_t36, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__founder_stewardship, enforcement_mechanism).
narrative_ontology:affects_constraint(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy__minority_extraction).
narrative_ontology:affects_constraint(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy__disclosure_consent).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question 'is dual-class founder control legitimate?' decomposes into three structurally distinct readings of one kernel, each with its own epsilon, beneficiary/victim structure, and type — per the epsilon-invariance principle, one reading per story. This story (founder_stewardship) assesses the standing dual-class arrangement by the stewardship reading's lights (epsilon = 0.32: the voting-dilution cost the reading concedes and prices as a fair premium for mission protection). The minority_extraction sibling assesses the same arrangement as a violation of capital-proportional entitlement (substantially higher epsilon; Class A re-indexed as rights-bearing victims). The disclosure_consent sibling locates legitimacy in the IPO disclosure event (epsilon indexed to consent quality; settled disclosure law makes it the empirically upstream story). Each story links the other two via affects_constraints; the disagreement is over the legitimacy ground, not over the arrangement's observable facts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
