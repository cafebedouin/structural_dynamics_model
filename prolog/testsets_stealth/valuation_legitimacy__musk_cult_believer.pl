% ============================================================================
% CONSTRAINT STORY: valuation_legitimacy__musk_cult_believer
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_valuation_legitimacy__musk_cult_believer, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: valuation_legitimacy__musk_cult_believer
 *   human_readable: Founder Track-Record Standard for Valuation Legitimacy (Believer Reading)
 *   domain: corporate finance / technology governance / space economics
 *
 * SUMMARY:
 *   This story instantiates ONE reading — musk_cult_believer — of the
 *   contested kernel valuation_legitimacy. The referent is fixed across the
 *   family: the standing arrangement by which Musk-controlled enterprises
 *   raise capital and defend governance at premium valuations. This reading
 *   authors that arrangement's epsilon from its own lights: transfers are
 *   real but read as earned pricing, so epsilon is low-moderate (0.30) and
 *   rising slowly as package sizes grow. The sibling readings author
 *   different epsilon over the SAME referent — governance_skeptic would
 *   author high epsilon (minority-holder extraction with no admitted
 *   coordination), dcf_fundamentalist would author epsilon concentrated on
 *   the epistemic commons (degraded price discovery),
 *   real_options_technologist would author lower epsilon still (option-space
 *   value is real). Per the epsilon-invariance principle these are separate
 *   files linked by network.affects_constraints; nothing about the contest is
 *   averaged into this one. The claim/metric gap is deliberate and
 *   load-bearing: the reading sincerely claims rope (a truth-tracking
 *   coordination standard open to anyone willing to look at the record),
 *   while the authored metrics describe an actively enforced arrangement with
 *   identifiable losers — the engine measures that divergence per seat.
 *
 * KEY AGENTS:
 *   - musk_voting_control_block: agenda-setter (institutional/arbitrage) — sets the standard, collects its capital-access and control rents
 *   - buy_and_hold_believer_coalition: primary beneficiary (organized/identity_locked) — narrative goods and paper gains; dilution drag as secondary payer
 *   - tesla_short_sellers: primary target (powerful/constrained) — publishes the counter-case, absorbs squeeze losses
 *   - dispersed_minority_shareholders: diffuse target (powerless/mobile) — absorbs dilution, arithmetically outvoted
 *   - tesla_employee_equity_holders: dual-positioned (moderate/constrained) — correlated job-and-savings exposure with upside participation
 *   - sell_side_skeptical_analysts: excluded voice (organized/mobile) — free to speak, no hearing inside the frame
 *   - delaware_chancery_governance_reviewers: analytical observer (institutional/analytical) — binds process, not belief
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__musk_cult_believer, 0.3).
domain_priors:suppression_score(valuation_legitimacy__musk_cult_believer, 0.5).
domain_priors:theater_ratio(valuation_legitimacy__musk_cult_believer, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__musk_cult_believer, extractiveness, 0.3).
narrative_ontology:constraint_metric(valuation_legitimacy__musk_cult_believer, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(valuation_legitimacy__musk_cult_believer, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__musk_cult_believer, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(valuation_legitimacy__musk_cult_believer, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__musk_cult_believer, rope).
narrative_ontology:human_readable(valuation_legitimacy__musk_cult_believer, "Founder Track-Record Standard for Valuation Legitimacy (Believer Reading)").
narrative_ontology:topic_domain(valuation_legitimacy__musk_cult_believer, "corporate finance / technology governance / space economics").

domain_priors:requires_active_enforcement(valuation_legitimacy__musk_cult_believer).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__musk_cult_believer, '76a49ad6-f5af-44bd-b2da-7e0da4fb40ac').
narrative_ontology:cs_kernel_codification('76a49ad6-f5af-44bd-b2da-7e0da4fb40ac', distributed).
narrative_ontology:cs_authority_grounding('76a49ad6-f5af-44bd-b2da-7e0da4fb40ac', practice).
narrative_ontology:cs_interpretation_layer_present('76a49ad6-f5af-44bd-b2da-7e0da4fb40ac').
narrative_ontology:cs_reading_relation('76a49ad6-f5af-44bd-b2da-7e0da4fb40ac', valuation_legitimacy__dcf_fundamentalist, coexists_with).
narrative_ontology:cs_reading_relation('76a49ad6-f5af-44bd-b2da-7e0da4fb40ac', valuation_legitimacy__real_options_technologist, coexists_with).
narrative_ontology:cs_reading_relation('76a49ad6-f5af-44bd-b2da-7e0da4fb40ac', valuation_legitimacy__governance_skeptic, forecloses).
narrative_ontology:cs_axiom('76a49ad6-f5af-44bd-b2da-7e0da4fb40ac', foundational, founder_track_record_grounds_valuation_legitimacy).
narrative_ontology:cs_axiom_status(founder_track_record_grounds_valuation_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('76a49ad6-f5af-44bd-b2da-7e0da4fb40ac', founder_track_record_grounds_valuation_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('76a49ad6-f5af-44bd-b2da-7e0da4fb40ac', secondary, governance_friction_subordinate_to_unique_founder_capability).
narrative_ontology:cs_axiom_status(governance_friction_subordinate_to_unique_founder_capability, holdable).
narrative_ontology:cs_axiom_grounding('76a49ad6-f5af-44bd-b2da-7e0da4fb40ac', governance_friction_subordinate_to_unique_founder_capability, instrumental).
narrative_ontology:cs_reference_frame('76a49ad6-f5af-44bd-b2da-7e0da4fb40ac', founder_track_record_supremacy).
narrative_ontology:cs_drift_state('76a49ad6-f5af-44bd-b2da-7e0da4fb40ac', post_2025_pay_package_ratification, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('76a49ad6-f5af-44bd-b2da-7e0da4fb40ac', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__musk_cult_believer, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__musk_cult_believer, buy_and_hold_believer_coalition).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__musk_cult_believer, musk_voting_control_block).
narrative_ontology:constraint_victim(valuation_legitimacy__musk_cult_believer, tesla_short_sellers).
narrative_ontology:constraint_victim(valuation_legitimacy__musk_cult_believer, dispersed_minority_shareholders).
narrative_ontology:constraint_victim(valuation_legitimacy__musk_cult_believer, sell_side_skeptical_analysts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__musk_cult_believer, tesla_employee_equity_holders).
narrative_ontology:constraint_victim(valuation_legitimacy__musk_cult_believer, buy_and_hold_believer_coalition).
narrative_ontology:constraint_victim(valuation_legitimacy__musk_cult_believer, tesla_employee_equity_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds roughly 42% of equity carrying on the order of 82% of voting power across the enterprise complex. Sets the narrative through owned broadcast channels, shapes board composition, proposes compensation structures including awards vesting on multiplanetary milestones, and decides when to warn of bankruptcy and when to declare delivery. Premium-valuation capital access flows to the ventures under this standard; governance challenges are answered by counting votes rather than negotiating. Capital and attention rotate freely among Tesla, SpaceX, xAI, and other ventures; no single venture's distress forces liquidation of the position.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, musk_voting_control_block, agenda_setter,
    institutional, generational, arbitrage, global).

% Retail and semi-professional holders who treat ownership as membership in the mission. They receive narrative coherence, community belonging, and large unrealized gains from prior cycles; they also absorb dilution when each successive award package passes and carry concentrated risk if delivery slips. Selling is socially coded as betrayal inside their forums; repeated 'paper hands' ridicule across cycles has fused the position with personal identity. Their votes reliably ratify control-block proposals, and they supply the volunteer defense of the standard on every platform where it is challenged.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, buy_and_hold_believer_coalition, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__musk_cult_believer, buy_and_hold_believer_coalition, payer).

% Institutional and retail traders positioned against the valuation. They finance research, publish short theses, and provide the counter-narrative the believer community defines itself against. Position mechanics expose them to unbounded upside risk in the stock; several cycles ended in forced covering at large losses during violent rallies. Exiting means covering at a realized loss and abandoning publicly staked theses; staying in after a squeeze invites repeat. Borrow costs and crowd-out dynamics tighten precisely when the standard is working.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, tesla_short_sellers, payer,
    powerful, immediate, constrained, global).

% Atomized retail holders and passive index funds holding the non-control float. They receive whatever the narrative delivers and absorb dilution from repeated mega-award packages; their governance objections are arithmetically irrelevant against an 82% voting bloc, and broker non-votes widen the gap further. Any individual holder can sell at any time; collective action has never formed. Proxy advisors aggregate part of their voice, but recommendations against packages have been overridden in every recent vote.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, dispersed_minority_shareholders, payer,
    powerless, biographical, mobile, global).

% Employees compensated substantially in restricted stock and options. Household wealth concentrates in the employer whose valuation the standard sustains; they gain when the multiple expands and bear correlated job-plus-savings risk if it compresses. Departure forfeits unvested tranches, so tenure deepens exposure. Public dissent from the legitimacy standard inside the company or on its platforms is career-limiting, so internal skepticism rarely becomes audible.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, tesla_employee_equity_holders, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__musk_cult_believer, tesla_employee_equity_holders, beneficiary).

% Analysts and research shops whose models price on cash flows, governance risk, and comparable multiples. Inside the believer frame their output is pre-dismissed as lagging-indicator noise or motivated bias; prominent bears have faced client attrition, platform pile-ons, and firm-level pressure after high-profile misses. They remain free to publish and to change employers; what they lack is a hearing inside the legitimacy conversation their subject dominates.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, sell_side_skeptical_analysts, excluded,
    organized, biographical, mobile, continental).

% The court and adjacent governance institutions — proxy advisors, institutional stewardship teams — that adjudicate compensation and control disputes. They rescinded the 2018 CEO award on process grounds, watched it be re-ratified by the same conflicted voter base, and continue to review successor packages of escalating size. Their rulings bind process but not belief; the believer community records adverse opinions as noise to be outvoted at the next meeting.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, delaware_chancery_governance_reviewers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(valuation_legitimacy__musk_cult_believer, musk_voting_control_block).
narrative_ontology:fixing_cost_class(valuation_legitimacy__musk_cult_believer, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a shared valuation standard for assets whose value lies in capabilities that do not yet appear in financial statements — reusable launch, satellite internet scale, autonomous driving, multiplanetary logistics — letting a dispersed capital coalition coordinate on holding through volatility that metric-only frameworks would interpret as distress.
% TRANSFER_FUNCTION: Moves valuation legitimacy (and the capital access it confers) on the basis of founder execution history rather than audited financials; moves governance concessions from minority holders to the control block through serially larger award packages; and moves capital from contrarian positions to the holder coalition through squeeze cycles that convert published skepticism into realized losses.
% ABSENT_VOICES: Short sellers and skeptical analysts are structurally outside the legitimacy conversation — their objections are pre-classified as lagging-indicator noise or motivated bias before evaluation begins. Dispersed minority shareholders object through proxies but are outvoted 82-to-18 before deliberation starts. Former employees with unvested equity have strong views and no safe channel.
% DISAPPEARANCE_RATIONALE: If the track-record standard stopped binding overnight, the ventures' cost of capital would reprice toward cash-flow fundamentals, long-horizon programs would need restructuring around conventional financing or state support, the believer coalition would fragment into ordinary diversified holders, and the control block would face governance negotiation instead of vote counting.
% FOUNDING_PROBLEM: Keep capital flowing to ventures whose value proposition outran their balance sheets — 2008-era SpaceX had reached orbit on nearly empty accounts and Tesla was weeks from insolvency during Model 3 ramp; conventional metrics read both as dead, and the track-record standard was built to overrule that reading.
% FOUNDING_PROBLEM_CORROBORATION: No corroborating source outside the benefiting parties attests the founding problem remains live: the control block and believer communities attest it, citing each new venture as a fresh instance. Against that, Delaware Chancery's Tornetta opinion, repeated ISS and Glass Lewis recommendations, and published short research all attest the standard now functions primarily to protect control and suppress contrary capital. The corroboration asymmetry is itself signal.
narrative_ontology:disappearance_verdict(valuation_legitimacy__musk_cult_believer, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__musk_cult_believer, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__musk_cult_believer, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(valuation_legitimacy__musk_cult_believer, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__musk_cult_believer, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(valuation_legitimacy__musk_cult_believer_tests).
:- end_tests(valuation_legitimacy__musk_cult_believer_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.30 because this reading assesses the standing arrangement's transfers — award dilution, squeeze losses, waived governance process — as legitimate prices of participation rather than takings; the slow rise across the interval tracks the reading's own grudging concessions as packages escalated from millions to a trillion-dollar award. Suppression (0.50) is authored as a raw structural property and is NOT scaled by power or scope in this field: community enforcement, platform amplification, and margin mechanics are real coercive surfaces, but exit (sell, leave the forum, cover) exists, so it sits mid-scale. Theater ratio (0.35) splits the difference between genuine epistemic content — reusable rockets did happen, and the heuristic had real predictive power in 2008-2015 — and the growing ritual recitation of past wins deployed to dismiss present evidence. Accessibility collapse is low (0.25): DCF models, real-options frameworks, and governance analysis remain fully available to anyone outside the community; the standard collapses alternatives only for its adherents. Resistance is high (0.70): a financed short complex, Delaware litigation, proxy-advisor opposition, and skeptical press constitute sustained organized pushback. The measurement series run on one shared six-point grid (years-since-2008: 0, 5, 10, 12, 15, 17) with every tracked metric authored at every point; suppression_requirement is tracked because enforcement capacity is a central dynamic here — the 2018 short war and the 2022 platform acquisition are visible step-changes in the enforcement machinery. Dynamics are ratchet-shaped, not cyclical: each victory (squeeze, re-ratification, package approval) permanently raises the enforcement baseline, so no oscillation cycle is modeled.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute radically different constraints from identical market data. From the control-block seat the arrangement is a meritocracy it built and repeatedly earned; from the short-seller seat it is a machine that converts published research into forced losses; from the minority-shareholder seat it is arithmetic — 82% outvotes 18% before any argument starts; from the employee seat it is a correlated bet they cannot hedge without quitting. Same ticker, same filings, four different constraints. Inter-institutionally, Delaware Chancery binds process but cannot touch belief: its rescission of the 2018 award was followed by re-ratification by the same conflicted electorate, which is why the observer seat's power is real yet peripheral to the constraint's persistence. Among same-level actors, believers and skeptics hold nominally identical retail positions with opposite exits: the believer's sell button is fused to identity, the skeptic's is fused to a published thesis — equal standing, asymmetric locks.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. The control block sits nearest the beneficiary pole (declared beneficiary, agenda-setting power, arbitrage-grade rotation across ventures). The believer coalition derives low d from its beneficiary declaration, damped slightly by its secondary payer position (dilution). Short sellers derive near-full-target d: declared victims, immediate horizons, and exit that exists mechanically but at realized-loss cost. Dispersed minority shareholders derive high d tempered by mobile exit — they can leave, they just never do collectively. Skeptical analysts derive moderately high d through exclusion: they pay in reputation and hearing, not principal. One directionality override is authored: the moderate power atom (occupied in this story only by tesla_employee_equity_holders) is corrected from a derived ~0.80 down to 0.55, because the derivation reads the payer role alone and misses the equity-upside participation that gives employees a genuine beneficiary flank — the correction is effectively per-agent here since only one agent holds that atom.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — keeping capital alive for ventures whose balance sheets undersold their trajectories — was real and is plausibly still live in mutated form (each new venture restarts the pattern). Status is authored contested rather than dead because the problem's liveness is exactly what the four readings dispute. The mismatch consumer reads status x verdict: contested-status plus world_rearranges yields no zombie flag, but the receipt surface independently flags functional drift — gains land on the control block, and fixing is prohibitive for every seat that could attempt it, which is the signature of a coordination standard whose center of gravity has moved from financing radical uncertainty to protecting control. Classification prevents both failure modes: reading the arrangement as pure snare erases the genuine epistemic service it performed when metrics genuinely could not see the option space; reading it as pure rope launders the enforcement shell and the squeeze machinery. The tangled middle is the honest description, and this reading's rope claim against extractive-flavored metrics is precisely the divergence the corpus exists to measure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates only the musk_cult_believer reading of the valuation_legitimacy kernel; what structurally changes under the dcf_fundamentalist, real_options_technologist, and governance_skeptic readings?',
    'Author each sibling as its own constraint story over the fixed referent; compare victim sets, epsilon, and per-seat classifications across the family with the referent held constant.',
    'Under governance_skeptic the same arrangement computes with a high-epsilon victim set centered on minority holders and no admitted coordination function; under real_options_technologist epsilon falls further and the coordination component strengthens; under dcf_fundamentalist the harm relocates to the epistemic commons. Cross-reading comparisons are valid only referent-fixed, since epsilon is reading-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one of four readings of the valuation_legitimacy kernel; sibling readings redistribute victims and epsilon over the same referent.').

omega_variable(
    unfalsifiable_track_record_standard,
    'Is the track-record standard falsifiable at all, given that delivery confirms it and distress is reframed as negotiating tactic (''genuine risk of bankruptcy'' warnings as theater)?',
    'Pre-register ex ante the specific ''impossible'' goals and dates the standard predicts, then score outcomes against independent engineering and financial records rather than believer interpretation.',
    'If the standard is unfalsifiable in operation, its epistemic content collapses toward pure identity maintenance; theater_ratio should be revised sharply upward and the arrangement''s persistence mechanism shifts from prediction to belonging.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unfalsifiable_track_record_standard, empirical, 'Whether the constraint''s core heuristic can lose, or only win and reframe.').

omega_variable(
    survivorship_vs_skill_attribution,
    'How much of the legitimizing track record is durable founder skill versus survivorship selection and risk shifted onto late-arriving holders?',
    'Compare base rates of founder-promised ''impossible'' goals across the full population of comparable ventures including failures; decompose holder returns into skill-attributable alpha versus beta borne by entrants after each validation.',
    'If attribution is largely survivorship, the standard fails to generalize beyond its single exemplar, the coordination function weakens, and the believer coalition''s identity lock becomes the primary persistence mechanism rather than the record itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survivorship_vs_skill_attribution, empirical, 'Skill-versus-survivorship decomposition of the legitimizing record.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of dissent structural (platform amplification, margin mechanics, career consequences) or internalized (identity fusion that makes selling unthinkable regardless of barriers)?',
    'Post-exit trajectory study of former believers who sold: if dissent-aversion and mission-frame loyalty persist after the position is closed, the internalized component is substantial.',
    'If largely internalized, effective suppression exceeds the structural measure, the believer coalition''s exit_options are mis-modeled wherever identity_lock is not declared, and enforcement decay would not relax the constraint as fast as structural models predict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized split of the constraint''s suppressive force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__musk_cult_believer, 0, 17).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vl_musk_believer_tr_t0, valuation_legitimacy__musk_cult_believer, theater_ratio, 0, 0.18).
narrative_ontology:measurement(vl_musk_believer_tr_t5, valuation_legitimacy__musk_cult_believer, theater_ratio, 5, 0.21).
narrative_ontology:measurement(vl_musk_believer_tr_t10, valuation_legitimacy__musk_cult_believer, theater_ratio, 10, 0.26).
narrative_ontology:measurement(vl_musk_believer_tr_t12, valuation_legitimacy__musk_cult_believer, theater_ratio, 12, 0.3).
narrative_ontology:measurement(vl_musk_believer_tr_t15, valuation_legitimacy__musk_cult_believer, theater_ratio, 15, 0.33).
narrative_ontology:measurement(vl_musk_believer_tr_t17, valuation_legitimacy__musk_cult_believer, theater_ratio, 17, 0.35).

% Extraction over time
narrative_ontology:measurement(vl_musk_believer_be_t0, valuation_legitimacy__musk_cult_believer, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(vl_musk_believer_be_t5, valuation_legitimacy__musk_cult_believer, base_extractiveness, 5, 0.18).
narrative_ontology:measurement(vl_musk_believer_be_t10, valuation_legitimacy__musk_cult_believer, base_extractiveness, 10, 0.24).
narrative_ontology:measurement(vl_musk_believer_be_t12, valuation_legitimacy__musk_cult_believer, base_extractiveness, 12, 0.27).
narrative_ontology:measurement(vl_musk_believer_be_t15, valuation_legitimacy__musk_cult_believer, base_extractiveness, 15, 0.29).
narrative_ontology:measurement(vl_musk_believer_be_t17, valuation_legitimacy__musk_cult_believer, base_extractiveness, 17, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(vl_musk_believer_su_t0, valuation_legitimacy__musk_cult_believer, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(vl_musk_believer_su_t5, valuation_legitimacy__musk_cult_believer, suppression_requirement, 5, 0.28).
narrative_ontology:measurement(vl_musk_believer_su_t10, valuation_legitimacy__musk_cult_believer, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(vl_musk_believer_su_t12, valuation_legitimacy__musk_cult_believer, suppression_requirement, 12, 0.48).
narrative_ontology:measurement(vl_musk_believer_su_t15, valuation_legitimacy__musk_cult_believer, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(vl_musk_believer_su_t17, valuation_legitimacy__musk_cult_believer, suppression_requirement, 17, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__musk_cult_believer, identity_coordination).
narrative_ontology:affects_constraint(valuation_legitimacy__musk_cult_believer, dcf_fundamentalist).
narrative_ontology:affects_constraint(valuation_legitimacy__musk_cult_believer, real_options_technologist).
narrative_ontology:affects_constraint(valuation_legitimacy__musk_cult_believer, governance_skeptic).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'how should Musk enterprises be valued?' decomposes into four structurally distinct constraints — one per reading of the valuation_legitimacy kernel — because the label conflates rival grounding sources with different victim sets and different epsilon over a fixed referent. This file is the musk_cult_believer member. Family edges run sibling-to-sibling via affects_constraints; the governance_skeptic member is the sharpest contrast (same referent, near-opposite victim set), and the real_options_technologist member is the closest cousin (both reject cash-flow grounding). No member's epsilon may be averaged with another's; comparison is referent-fixed only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(valuation_legitimacy__musk_cult_believer, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
