% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__extraction_snare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__extraction_snare_reading, []).

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
 *   constraint_id: statutory_debt_ceiling__extraction_snare_reading
 *   human_readable: Statutory Debt Ceiling as Minority Extraction Weapon (Extraction-Snare Reading)
 *   domain: constitutional/political economy/fiscal governance
 *
 * SUMMARY:
 *   This story instantiates the extraction-snare reading of the statutory
 *   debt ceiling: a statutory aggregate limit on Treasury borrowing that,
 *   once the federal government runs persistent deficits, no longer
 *   authorizes anything new (spending is already obligated by prior
 *   appropriations) and instead operates as a recurring hostage boundary. A
 *   legislative minority withholds the increase, threatens default on
 *   obligations Congress already incurred, and releases its votes only in
 *   exchange for policy concessions unavailable through ordinary majoritarian
 *   channels — the Budget Control Act caps and sequestration extracted in
 *   2011, the Fiscal Responsibility Act caps extracted in 2023, with the 2011
 *   S&P downgrade and the 2023 Fitch downgrade marking the market cost of
 *   each firing. The wielders can end any standoff at will and reload the
 *   lever next cycle; the costs land on bondholders, payment-dependent
 *   households, and the diffuse public. Sibling stories
 *   (statutory_debt_ceiling__coordination_scaffold_reading,
 *   statutory_debt_ceiling__constitutional_nullity_reading) instantiate the
 *   other readings of the same kernel with their own epsilon values and
 *   stakeholder sets; this file authors only the snare reading and links the
 *   family via network edges. KEY AGENTS (by structural relationship): -
 *   legislative_minority_factions: Primary wielder/agenda-setter
 *   (organized/mobile) — withholds ceiling increases to obtain concessions;
 *   can reload the lever each cycle - presidents_and_governing_majorities:
 *   Primary negotiating target (institutional/constrained) — must settle to
 *   avoid default - us_treasury: Administrative executor
 *   (institutional/constrained) — manages the exhaustion date, deploys
 *   extraordinary measures, bears operational and legal risk -
 *   federal_bondholders: Financial target (powerful/constrained) — bear
 *   repricing, downgrade losses, safe-asset disruption -
 *   benefit_recipients_and_contractors: Human-cost payers (powerless/trapped)
 *   — face payment-priority risk on fixed dates - general_taxpaying_public:
 *   Diffuse payer (moderate/trapped) — bears austerity concessions, risk
 *   premia, growth drag - primary_election_challengers: Secondary beneficiary
 *   (moderate/mobile) — converts crisis hardball into fundraising and
 *   positioning - alternative_authority_advocates: Excluded seat
 *   (moderate/mobile) — Fourteenth Amendment, coin, and premium-bond routes
 *   kept out of settlements - credit_rating_agencies: Analytical observer
 *   (institutional/arbitrage) — price the credibility damage of each episode
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__extraction_snare_reading, 0.76).
domain_priors:suppression_score(statutory_debt_ceiling__extraction_snare_reading, 0.72).
domain_priors:theater_ratio(statutory_debt_ceiling__extraction_snare_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__extraction_snare_reading, snare).
narrative_ontology:human_readable(statutory_debt_ceiling__extraction_snare_reading, "Statutory Debt Ceiling as Minority Extraction Weapon (Extraction-Snare Reading)").
narrative_ontology:topic_domain(statutory_debt_ceiling__extraction_snare_reading, "constitutional/political economy/fiscal governance").

domain_priors:requires_active_enforcement(statutory_debt_ceiling__extraction_snare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__extraction_snare_reading, '573971c0-d338-4a93-add8-93fe964d3fb6').
narrative_ontology:cs_kernel_codification('573971c0-d338-4a93-add8-93fe964d3fb6', formalized).
narrative_ontology:cs_authority_grounding('573971c0-d338-4a93-add8-93fe964d3fb6', extraction).
narrative_ontology:cs_interpretation_layer_present('573971c0-d338-4a93-add8-93fe964d3fb6').
narrative_ontology:cs_reading_relation('573971c0-d338-4a93-add8-93fe964d3fb6', statutory_debt_ceiling__constitutional_nullity_reading, influences).
narrative_ontology:cs_reading_relation('573971c0-d338-4a93-add8-93fe964d3fb6', statutory_debt_ceiling__coordination_scaffold_reading, forecloses).
narrative_ontology:cs_axiom('573971c0-d338-4a93-add8-93fe964d3fb6', foundational, authorized_debt_payment_not_leverage).
narrative_ontology:cs_axiom_status(authorized_debt_payment_not_leverage, holdable).
narrative_ontology:cs_axiom_grounding('573971c0-d338-4a93-add8-93fe964d3fb6', authorized_debt_payment_not_leverage, deontological).
narrative_ontology:cs_axiom('573971c0-d338-4a93-add8-93fe964d3fb6', secondary, minority_default_threat_is_coercion).
narrative_ontology:cs_axiom_status(minority_default_threat_is_coercion, holdable).
narrative_ontology:cs_axiom_grounding('573971c0-d338-4a93-add8-93fe964d3fb6', minority_default_threat_is_coercion, conventional).
narrative_ontology:cs_reference_frame('573971c0-d338-4a93-add8-93fe964d3fb6', standing_default_leverage_regime).
narrative_ontology:cs_drift_state('573971c0-d338-4a93-add8-93fe964d3fb6', contemporary_post_2011_downgrade_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('573971c0-d338-4a93-add8-93fe964d3fb6', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__extraction_snare_reading, legislative_minority_factions).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__extraction_snare_reading, primary_election_challengers).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, federal_bondholders).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, benefit_recipients_and_contractors).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, general_taxpaying_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, us_treasury).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, presidents_and_governing_majorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A caucus of legislators, typically in the chamber minority, that withholds the votes needed to raise or suspend the statutory limit on Treasury borrowing. By refusing consent they push the government toward missed payments, then release their votes in exchange for policy concessions — spending caps, program cuts, procedural commitments — that they could not obtain through ordinary bill-passing. They can end each standoff at will by voting yes, and they return to the same lever in the next cycle; their own constituents rarely bear the first-order costs of a default scare.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, legislative_minority_factions, agenda_setter,
    organized, biographical, mobile, national).

% Outsider candidates and insurgent challengers who campaign on refusing to raise the borrowing limit. Each standoff supplies them with fundraising appeals, media attention, and proof-of-defiance credentials against incumbents; they collect reputational returns from every episode without bearing responsibility for the settlement terms.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, primary_election_challengers, beneficiary,
    moderate, biographical, mobile, national).

% Holders of Treasury securities — domestic pension funds, foreign central banks, money-market funds — whose assets are the direct object of the default threat. Every standoff forces repricing of the world's benchmark safe asset; the 2011 episode produced the first-ever US credit downgrade and measurable yield penalties. They cannot exit the position meaningfully: Treasuries are the collateral and liquidity backbone of the global financial system, so selling during a scare realizes exactly the losses the threat implies.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, federal_bondholders, payer,
    powerful, biographical, constrained, global).

% Social Security recipients, veterans, federal employees, Medicare providers, and government contractors who depend on scheduled federal payments. In each standoff they face the possibility that Treasury must choose which obligations to pay after the deadline; their income arrives on fixed dates and they have no way to hedge or relocate away from the payment calendar.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, benefit_recipients_and_contractors, payer,
    powerless, immediate, trapped, national).

% Households and firms outside the payment calendar who bear the diffuse costs: higher borrowing costs passed through mortgages and credit, austerity concessions written into settlement bills, growth drag from each uncertainty episode, and the accumulated fiscal commitments traded away in closed-door deals. Their recourse is electoral, delayed, and diluted across millions of households.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, general_taxpaying_public, payer,
    moderate, generational, trapped, national).

% Administers the borrowing limit day to day: issues debt up to the cap, forecasts the exhaustion date, and deploys 'extraordinary measures' — accounting maneuvers around G Fund and exchange-stabilization rules — to postpone it. During standoffs it drafts contingency plans for prioritizing payments, briefs markets, and absorbs the operational and legal risk of whatever happens after the deadline. It cannot refuse the statute and cannot resolve the standoff; it can only manage the countdown.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, us_treasury, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__extraction_snare_reading, us_treasury, agenda_setter).

% The president and congressional majority in office when the limit binds. They must assemble the votes to raise or suspend it, absorbing blame for any market turmoil and paying the policy price demanded by the withholding faction. Their alternatives — invoking the Fourteenth Amendment, minting a large-denomination coin, ignoring the limit — carry constitutional and legal jeopardy, so their practical option set collapses to negotiation.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, presidents_and_governing_majorities, payer,
    institutional, biographical, constrained, national).

% Legal scholars, former officials, and advocates who argue the executive can honor debt obligations regardless of the limit — through Section 4 of the Fourteenth Amendment, a platinum coin, or premium bonds — and that the limit itself is unnecessary or invalid. They are kept outside the negotiated settlements: deals are struck between the withholding faction and the leadership facing the deadline, and the alternative-authority routes are dismissed as too risky to attempt.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, alternative_authority_advocates, excluded,
    moderate, biographical, mobile, national).

% Rate sovereign credit and publish outlooks tied to the standoffs. S&P's 2011 downgrade and Fitch's 2023 action translated each episode into measurable repricing across global portfolios. They score the arrangement from outside; their ratings shape the perceived cost of every standoff without their holding any seat in the negotiation.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, credit_rating_agencies, observer,
    institutional, biographical, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statutory_debt_ceiling__extraction_snare_reading, legislative_minority_factions).
narrative_ontology:fixing_cost_class(statutory_debt_ceiling__extraction_snare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Consolidates what would otherwise be a separate authorization vote for every Treasury debt auction into a single periodic decision on the aggregate stock of federal debt, giving Congress a recurring checkpoint on cumulative indebtedness without micromanaging each issuance.
% TRANSFER_FUNCTION: Moves policy concessions — discretionary spending caps, program cuts, procedural commitments — from the governing majority to the withholding minority faction, with the price of each transfer paid in default risk absorbed by bondholders, payment-schedule risk absorbed by benefit recipients and contractors, and economic drag absorbed by the broader public.
% ABSENT_VOICES: Bondholders, benefit recipients, and future taxpayers have no seat at the settlement table; advocates of alternative authorities (Fourteenth Amendment invocation, the platinum coin, premium bonds) are structurally outside the room; rank-and-file members of both parties learn deal terms after they are struck. Unanimity around each settlement reflects who was in the room — the faction holding the limit and the leadership facing the deadline — not consent among those who bear the costs.
% DISAPPEARANCE_RATIONALE: If the limit vanished overnight, Treasury would resume issuing debt against already-appropriated obligations as a matter of course; the withholding faction would lose its recurring lever and migrate to appropriations and reconciliation fights where it holds weaker cards; the periodic market scares, downgrade risk premia, and last-minute austerity settlements would disappear; and fiscal argument would return to the ordinary budget process, where majorities can actually pass what they negotiate.
% FOUNDING_PROBLEM: In 1917, Congress faced rapidly expanding World War I borrowing and delegated to Treasury the ability to issue bonds without a separate statute for each offering, retaining instead a single aggregate ceiling on the outstanding stock — trading issuance-by-issuance control for speed while keeping a cumulative check.
% FOUNDING_PROBLEM_CORROBORATION: Fiscal hawks in Congress attest that an aggregate check on debt remains necessary. Outside the benefiting parties: the Congressional Research Service traces the ceiling's divergence from its 1917 delegation purpose; GAO reports document that it does not constrain spending decisions already made; former Treasury secretaries and Federal Reserve chairs (Geithner, Lew, Bernanke, Powell) have testified it serves no useful fiscal-discipline function; and the comparative record — most advanced economies (Denmark, Sweden, New Zealand, the United Kingdom) operate without binding debt ceilings while maintaining fiscal control — corroborates that the founding problem no longer requires this instrument.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__extraction_snare_reading, world_rearranges).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__extraction_snare_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__extraction_snare_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(statutory_debt_ceiling__extraction_snare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__extraction_snare_reading, 0.76, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__extraction_snare_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(statutory_debt_ceiling__extraction_snare_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(statutory_debt_ceiling__extraction_snare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.76 at interval end) because the arrangement transfers real policy concessions to a faction that obtains them by threatening an outcome nobody wants, and imposes economy-wide risk costs on top; the transfer is decoupled from any service rendered. Suppression (0.72) is structural: the weapon works by making missed payments the default outcome unless the target concedes, and neither Treasury nor the governing majority has a lawful exit — the alternative authorities all carry constitutional jeopardy. Theater (0.44) is moderate: the symbolic voting is heavy (poison-pill bills, defiance messaging, blame choreography), but the exhaustion date is mechanically real, so performance rides on a live trigger rather than replacing one. Accessibility collapse is moderate (0.45): the alternatives are well understood — coin, Fourteenth Amendment, premium bonds, simple abolition, as practiced by most peer nations — but each collapses institutionally rather than conceptually. Resistance (0.62) is substantial: presidential refusal-to-negotiate stands, market discipline, rating actions, and recurring abolition proposals. The measurement series run on one shared grid (T=0..30 mapping 1995..2025, sampled at five-year points) with every tracked metric authored at every point. The series oscillate rather than drift monotonically: buildup, brinkmanship, settlement, calm, accumulation — with dips in the surplus era (~T=5) and the 2019-2021 suspension window (~T=25) when the lever was legally unavailable. The oscillation is itself part of the mechanism: each resolved crisis rewards the wielders (intermittent reinforcement), teaching that brinkmanship pays and ratcheting the envelope upward from 0.34 to 0.76 across the interval. Suppression_requirement is tracked because enforcement intensity is the traced dynamic: the brinkmanship machinery matured over the period — exhaustion-date forecasting, extraordinary-measures depletion schedules, payment-prioritization playbooks, calibrated market messaging — so the active force needed to keep the lever loaded rose from 0.38 to 0.72. Claimed type (snare) is authored from the structural reading; the metrics are authored independently from the descriptive record; the engine computes per-seat classifications from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same statute. From the wielder seat, the arrangement is legitimate accountability — a check on debt the majority refuses to impose on itself — and each standoff is successful strategy. From the negotiating-target seat (president, governing majority), the same structure is extortion: pay concessions or preside over default. From the Treasury seat it is an administrative siege — managing a countdown under legal jeopardy for a decision it does not control. From the bondholder seat it is recurring tail risk on the world's safe asset. The engine derives these divergent per-seat classifications from the structural data (role, power, exit options); the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The declared beneficiaries sit near the subsidy end: legislative_minority_factions collect the concessions and hold mobile exit (they can end any crisis by voting yes, and they choose not to), and primary_election_challengers collect reputational rents incidentally. The declared victims sit near the full-target end: federal_bondholders are powerful but portfolio-constrained (selling realizes the threatened loss), benefit_recipients_and_contractors are powerless and trapped on a fixed payment calendar, and the general_taxpaying_public bears diffuse costs with only delayed electoral recourse. Us_treasury is dual-positioned — it administers the mechanics (agenda-setting secondary role) while absorbing the operational and legal costs — placing it mid-to-high. Presidents_and_governing_majorities are full targets at the table. National scope modestly amplifies effective extraction for the target seats by making coordinated verification and response harder; the wielders' organizational power and mobile exit keep their effective burden near zero.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — delegating rapid wartime bond issuance while retaining a cumulative check — died with the institutional arrangements it served; the modern budget process handles aggregate fiscal control through appropriations and reconciliation. The arrangement persists not from inertia alone but because the lever retains concentrated value to a repeat player, which is what separates this from a piton: a piton has no seat that meaningfully profits, whereas here gain_flow names the withholding faction, and fixing is prohibitive precisely because disarmament requires the benefiting actors (and a blame-averse majority) to surrender the weapon voluntarily. Reading the ceiling as a coordination scaffold would mask the transfer entirely; reading it as mere vestigial dysfunction would miss the concentrated capturer; the snare classification keeps both the victims and the capturing seat visible. The R5 interview records the founding problem as contested — wielders attest the aggregate check is live, while CRS, GAO, former Treasury secretaries, Fed chairs, and the comparative international record corroborate obsolescence — paired with a world_rearranges disappearance verdict, since the negotiating calendar, the market-risk cycle, and the concession stream all depend on the arrangement continuing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates one reading of the statutory_debt_ceiling kernel — the extraction-snare reading. How would the coordination-scaffold or constitutional-nullity readings change the constraint''s structure?',
    'Author the sibling stories: the scaffold reading would produce low extraction, no victim set, and a sunset-compatible profile; the nullity reading would relocate the arrangement from statute-boundary to executive-legislative constitutional conflict with the ceiling''s validity itself as the contested object.',
    'Under the scaffold reading the same statute computes as low-extraction coordination; under the nullity reading the operative constraint becomes the constitutional dispute rather than the borrowing limit. The classification in this file applies only to the snare reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this is one of three readings of the debt-ceiling kernel; siblings are separate constraints linked by network edges.').

omega_variable(
    counterfactual_fiscal_discipline,
    'Would federal debt trajectories differ materially if the ceiling were abolished, or does the appropriations process alone determine borrowing?',
    'Comparative panel studies of advanced economies with and without binding ceilings; natural experiment from the 2019-2021 suspension window.',
    'If a real discipline effect exists, part of the measured transfer prices a genuine check (shading the arrangement toward hybrid coordination); if none, the arrangement is pure transfer with no coordination residue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_fiscal_discipline, empirical, 'Whether the ceiling produces any fiscal-discipline effect that would offset its transfer function.').

omega_variable(
    concession_attribution_ambiguity,
    'Are the policy concessions obtained at limit standoffs attributable to the default threat, or would equivalent concessions have emerged through ordinary appropriations bargaining?',
    'Compare concession outcomes across suspended-limit periods, clean-raise periods, and brinkmanship periods matched on divided government.',
    'Sets the magnitude of the transfer component of extractiveness; misattribution would inflate or deflate the measured extraction attributable to the threat mechanism specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(concession_attribution_ambiguity, empirical, 'Attribution of extracted concessions to the default-threat mechanism versus background bargaining.').

omega_variable(
    diffuse_victim_coalition_potential,
    'Can the diffuse victims — bondholders, payment-dependent households, the broad public — coordinate to impose costs on the withholding faction sufficient to disarm the lever (market discipline, electoral punishment), or does their dispersion leave the arrangement stable?',
    'Track yield penalties and electoral outcomes following successive standoffs; measure whether incumbents who led brinkmanship suffer measurable punishment in subsequent elections.',
    'If coalition costs bind, the arrangement destabilizes toward reform or abolition; if not, it stabilizes as a repeat-play equilibrium with the current victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diffuse_victim_coalition_potential, empirical, 'Whether dispersed victims can form an effective counter-coalition against the wielding faction.').

omega_variable(
    x_date_priority_legality,
    'Can Treasury legally prioritize principal payments over other obligations after the exhaustion date, and would doing so avert technical default?',
    'Litigation or OLC/GAO determination on payment-prioritization authority; market reaction to actual prioritization in a live standoff.',
    'Determines the credibility of the threat that powers the transfer: a credible prioritization path weakens the lever; legal impossibility of prioritization sharpens it and raises the effective stakes of every standoff.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(x_date_priority_legality, empirical, 'Legality and feasibility of post-deadline payment prioritization, which calibrates the threat''s potency.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__extraction_snare_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(stat_tr_t5, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement(stat_tr_t10, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(stat_tr_t15, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement(stat_tr_t20, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 20, 0.56).
narrative_ontology:measurement(stat_tr_t25, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 25, 0.36).
narrative_ontology:measurement(stat_tr_t30, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 30, 0.44).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(stat_be_t5, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 5, 0.18).
narrative_ontology:measurement(stat_be_t10, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 10, 0.26).
narrative_ontology:measurement(stat_be_t15, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(stat_be_t20, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(stat_be_t25, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 25, 0.45).
narrative_ontology:measurement(stat_be_t30, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 30, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(stat_su_t5, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 5, 0.3).
narrative_ontology:measurement(stat_su_t10, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 10, 0.36).
narrative_ontology:measurement(stat_su_t15, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(stat_su_t20, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 20, 0.64).
narrative_ontology:measurement(stat_su_t25, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement(stat_su_t30, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__extraction_snare_reading, resource_allocation).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling__coordination_scaffold_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling__constitutional_nullity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the debt ceiling' decomposes into three structurally distinct claims per the epsilon-invariance principle. The coordination-scaffold reading (upstream, design-era claim) carries near-zero extraction and no victim set; the extraction-snare reading (this file) carries high extraction with a named capturing seat; the constitutional-nullity reading relocates the constraint entirely, making validity rather than borrowing the contested object. The scaffold claim is cited as cover by wielders during standoffs, and each snare episode feeds momentum to the nullity argument — hence the family edges run scaffold -> snare -> nullity in influence terms. Each member has its own epsilon, beneficiaries, victims, and classification; no member averages across the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
