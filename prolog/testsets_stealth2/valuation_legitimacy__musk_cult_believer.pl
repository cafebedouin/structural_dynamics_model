% ============================================================================
% CONSTRAINT STORY: valuation_legitimacy__musk_cult_believer
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Founder Track-Record Valuation Legitimacy Norm (Believer Reading)
 *   domain: corporate finance/technology governance/space economics
 *
 * SUMMARY:
 *   This story instantiates ONE reading — musk_cult_believer — of the
 *   contested valuation_legitimacy kernel. The constraint is the operative
 *   norm that valuations of the founder's enterprises are legitimate because
 *   they derive from a demonstrated record of achieving goals industries had
 *   declared impossible, with financial metrics demoted to lagging
 *   indicators. Under this reading the standing arrangement (the current
 *   valuation regime for these enterprises) is assessed by the reading's own
 *   lights: the believer sees a coordination achievement around a uniquely
 *   capable executor, concedes limited extraction (short-covering losses
 *   treated as market discipline working correctly, skeptic credibility costs
 *   treated as epistemic failure rather than suppression), and reads the
 *   $1.75T-class valuation as conservative. The sibling readings —
 *   dcf_fundamentalist, real_options_technologist, governance_skeptic — are
 *   other constraints in other files with their own epsilon values and victim
 *   sets; per the epsilon-invariance principle nothing about them is averaged
 *   into this story. KEY AGENTS (by structural relationship):
 *   founder_ceo_musk (agenda setter, institutional/arbitrage) sets the
 *   narrative and administers milestone-linked compensation;
 *   musk_controlled_enterprises (primary beneficiary,
 *   institutional/constrained) receive below-fundamental cost of capital;
 *   believer_shareholders (primary beneficiary, organized/identity_locked)
 *   hold the coordinated book; financial_media_amplifiers (secondary
 *   beneficiary, organized/mobile) monetize the narrative;
 *   index_fund_managers (passive beneficiary/payer,
 *   institutional/constrained) hold by mandate; short_sellers (primary
 *   target, powerful/mobile) bear forced-covering losses; valuation_skeptics
 *   (primary target, moderate/constrained) bear repeated credibility costs;
 *   minority_governance_advocates (excluded, moderate/constrained) are voiced
 *   but not counted; market_regulators_sec (analytical observer,
 *   institutional/analytical) polices process without adjudicating
 *   legitimacy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__musk_cult_believer, 0.27).
domain_priors:suppression_score(valuation_legitimacy__musk_cult_believer, 0.42).
domain_priors:theater_ratio(valuation_legitimacy__musk_cult_believer, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__musk_cult_believer, extractiveness, 0.27).
narrative_ontology:constraint_metric(valuation_legitimacy__musk_cult_believer, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(valuation_legitimacy__musk_cult_believer, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__musk_cult_believer, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(valuation_legitimacy__musk_cult_believer, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__musk_cult_believer, tangled_rope).
narrative_ontology:human_readable(valuation_legitimacy__musk_cult_believer, "Founder Track-Record Valuation Legitimacy Norm (Believer Reading)").
narrative_ontology:topic_domain(valuation_legitimacy__musk_cult_believer, "corporate finance/technology governance/space economics").

domain_priors:requires_active_enforcement(valuation_legitimacy__musk_cult_believer).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__musk_cult_believer, '4be638e7-685d-428d-ad9e-a4f6647fa116').
narrative_ontology:cs_kernel_codification('4be638e7-685d-428d-ad9e-a4f6647fa116', distributed).
narrative_ontology:cs_authority_grounding('4be638e7-685d-428d-ad9e-a4f6647fa116', practice).
narrative_ontology:cs_interpretation_layer_present('4be638e7-685d-428d-ad9e-a4f6647fa116').
narrative_ontology:cs_reading_relation('4be638e7-685d-428d-ad9e-a4f6647fa116', valuation_legitimacy__dcf_fundamentalist, forecloses).
narrative_ontology:cs_reading_relation('4be638e7-685d-428d-ad9e-a4f6647fa116', valuation_legitimacy__real_options_technologist, influences).
narrative_ontology:cs_reading_relation('4be638e7-685d-428d-ad9e-a4f6647fa116', valuation_legitimacy__governance_skeptic, forecloses).
narrative_ontology:cs_axiom('4be638e7-685d-428d-ad9e-a4f6647fa116', foundational, track_record_outweighs_financial_metrics).
narrative_ontology:cs_axiom_status(track_record_outweighs_financial_metrics, holdable).
narrative_ontology:cs_axiom_grounding('4be638e7-685d-428d-ad9e-a4f6647fa116', track_record_outweighs_financial_metrics, empirically_contingent).
narrative_ontology:cs_axiom('4be638e7-685d-428d-ad9e-a4f6647fa116', foundational, founder_capability_nullifies_governance_constraints).
narrative_ontology:cs_axiom_status(founder_capability_nullifies_governance_constraints, holdable).
narrative_ontology:cs_axiom_grounding('4be638e7-685d-428d-ad9e-a4f6647fa116', founder_capability_nullifies_governance_constraints, instrumental).
narrative_ontology:cs_reference_frame('4be638e7-685d-428d-ad9e-a4f6647fa116', founder_execution_primacy).
narrative_ontology:cs_drift_state('4be638e7-685d-428d-ad9e-a4f6647fa116', contemporary_evidence_accumulation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4be638e7-685d-428d-ad9e-a4f6647fa116', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__musk_cult_believer, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__musk_cult_believer, believer_shareholders).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__musk_cult_believer, musk_controlled_enterprises).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__musk_cult_believer, financial_media_amplifiers).
narrative_ontology:constraint_victim(valuation_legitimacy__musk_cult_believer, short_sellers).
narrative_ontology:constraint_victim(valuation_legitimacy__musk_cult_believer, valuation_skeptics).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__musk_cult_believer, founder_ceo_musk).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__musk_cult_believer, index_fund_managers).
narrative_ontology:constraint_victim(valuation_legitimacy__musk_cult_believer, index_fund_managers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the narrative through public statements, product announcements, and timeline commitments, and administers compensation structures tied to long-horizon milestones. His delivered results — landed and reflown boosters, satellite internet reaching profitability, mass-market electric vehicles — constitute the evidentiary base the legitimacy claim rests on. Collects capital access at favorable terms, milestone-triggered award eligibility, and operating freedom from the board discipline conventional governance would impose. Exit is unusually open: attention and capital follow him across ventures, so no single enterprise's repricing traps him.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, founder_ceo_musk, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__musk_cult_believer, founder_ceo_musk, beneficiary).

% Raise equity and debt at costs far below what cash-flow-based pricing alone would support, recruit talent with equity narratives anchored to mission milestones, and operate with reduced investor scrutiny of margins and schedule slippage. They cannot exit the valuation regime their funding depends on — a repricing toward fundamentals would impair capital raises, equity-denominated compensation, and supplier terms simultaneously.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, musk_controlled_enterprises, beneficiary,
    institutional, generational, constrained, global).

% Hold concentrated positions justified by the founder's record of delivering what industries had declared impossible. Community membership supplies identity, information flow, and emotional payoff; selling is experienced as betrayal of the community and admission of having been wrong. Gains arrive as multiple expansion funded by new entrants and by short sellers forced to cover. The horizon extends to decade-scale milestones (Mars-linked vesting), which makes interim financial reporting feel beside the point.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, believer_shareholders, beneficiary,
    organized, generational, identity_locked, global).

% Monetize attention around founder statements, launch events, delivery numbers, and valuation milestones. Engagement follows the narrative in both directions — celebration and outrage both convert — so the amplifier seat collects regardless of which way the story moves. Coverage can rotate to other subjects if attention shifts, making this the most mobile beneficiary seat.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, financial_media_amplifiers, beneficiary,
    organized, immediate, mobile, global).

% Hold because index construction requires it, collecting the valuation premium passively while bearing concentration risk inside client portfolios without sharing the underlying conviction. Mandate rules bind them to hold whatever the index contains at whatever weight, so neither the gains nor the risk exposure are chosen.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, index_fund_managers, beneficiary,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__musk_cult_believer, index_fund_managers, payer).

% Publish research arguing the price is decoupled from fundamentals, borrow and sell shares, and wait for convergence that arrives, when it does, later and higher than modeled. Losses arrive as forced covering during momentum rallies amplified by the believer bid. Capital is mobile — positions can be closed and redeployed — but timing a short against an identity-driven buyer base has repeatedly been expensive, and the seat bears both financial loss and public ridicule.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, short_sellers, payer,
    powerful, biographical, mobile, global).

% Sell-side and independent analysts applying conventional frameworks — discounted cash flows, comparable multiples, governance screens. Their ratings and price targets are repeatedly overrun by momentum, costing credibility with clients and standing with employers. Switching to neutral coverage, rotating sectors, or moving buy-side preserves a career but abandons the analytical franchise and sector expertise they spent years building.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, valuation_skeptics, payer,
    moderate, biographical, constrained, continental).

% Proxy advisors, pension funds, and governance-focused investors raising conflicts-of-interest and control-concentration objections to related-party transactions and outsized voting power. Their arguments are voiced publicly but dismissed within the believer framework as category error — governance quality is held to be irrelevant when the founder is uniquely capable — so the objections never enter the legitimacy determination despite being on the record.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, minority_governance_advocates, excluded,
    moderate, biographical, constrained, national).

% Litigates over market-moving statements, reviews executive compensation packages, and monitors disclosure compliance. Takes testimony and documents from the other seats and can impose process remedies, but does not adjudicate the valuation question itself — its remit stops at statement accuracy and procedure, leaving the legitimacy contest to the market.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, market_regulators_sec, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(valuation_legitimacy__musk_cult_believer, believer_shareholders).
narrative_ontology:fixing_cost_class(valuation_legitimacy__musk_cult_believer, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates believer capital allocation and community identity: provides a shared standard that lets members hold concentrated positions through drawdowns without capitulating, and maintains a membership boundary (believer versus heretic) against continuously arriving contrary evidence. Solves, for this population, the collective-action problem of individual panic during volatility.
% TRANSFER_FUNCTION: Moves capital at below-fundamental cost into the founder's enterprises (from believer savings and index mandates); moves realized losses from short sellers forced to cover and from skeptics whose calls are overrun, to long holders riding multiple expansion; moves reputational standing toward believers and away from dissenters; moves attention to founder-controlled channels.
% ABSENT_VOICES: Minority-governance advocates and disciplined-value investors are present in the market but excluded from the legitimacy-determining conversation: their framework is ruled a category error before argument begins, so unanimity inside the believer community reflects the boundary, not persuasion. They publish proxy advice, short reports, and academic critiques — heard, but not counted.
% DISAPPEARANCE_RATIONALE: If the track-record legitimacy norm vanished overnight, believer-held positions would reprice toward cash-flow anchors, the enterprises' cost of capital would rise sharply enough to force financing restructuring, the interpretive apparatus (media amplification, community moderation, milestone celebration) would lose its coordinating function, and shorts and skeptics would regain standing as ordinary participants rather than heretics.
% FOUNDING_PROBLEM: Through roughly 2015, the founder's ventures faced expert consensus that their goals were uninvestable: three consecutive Falcon 1 failures preceded the first successful orbit, Tesla approached bankruptcy in 2008 with conventional lenders unwilling to extend, and mainstream auto and aerospace finance treated electric vehicles and reusable rockets as unsound allocations of capital. The track-record legitimacy norm solved the problem of raising capital and retaining talent against that consensus.
% FOUNDING_PROBLEM_CORROBORATION: Business historians and finance academics outside the beneficiary set corroborate the founding problem itself — the 2008 funding crisis and the Falcon 1 failure record are documented independently of believer testimony. Those same sources dispute the status: they attest the original problem was substantially solved by the mid-2010s, when the ventures began raising capital at scale through conventional channels, while the believer community attests it remains live because the ventures still pursue goals the broader industry calls impossible. No source outside the benefiting parties attests that the problem remains live in its original form.
narrative_ontology:disappearance_verdict(valuation_legitimacy__musk_cult_believer, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__musk_cult_believer, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__musk_cult_believer, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(valuation_legitimacy__musk_cult_believer, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__musk_cult_believer, 0.27, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.27 — reading-indexed: the believer seat concedes that late entrants bear downside the narrative prices near zero and that short losses are real transfers, but assesses the bulk of the arrangement as value creation rather than extraction. Suppression at 0.42 reflects the heretic-boundary machinery (community moderation, FUD-policing, framework dismissal) that the reading experiences internally as epistemic hygiene but which structurally imposes costs on dissent. Theater at 0.18: most community activity is functional milestone-tracking; a growing devotional-ritual share (anniversary celebrations, quote canonization) is visible but minor. Accessibility_collapse at 0.45: within the community, accepting the frame collapses alternative frameworks completely (they become category errors); market-wide, the alternatives remain live — hence mid-range. Resistance at 0.72: short campaigns, governance litigation, package revotes, and regulatory actions constitute sustained organized opposition unusual for a valuation norm. The measurement series run on ONE shared grid (t=0,3,6,9,12,15,18 mapping 2008-2026) with all three metrics authored at every point; the drift is monotone, not cyclical — no intermittent-reinforcement mechanism is claimed. suppression_requirement is tracked because enforcement capacity visibly changed: the interpretive and boundary apparatus scaled sharply through the 2018-2021 retail-participation wave and hardened further as external resistance grew, so the enforcement picture is dynamic, not static.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the believer seat the arrangement is coordination it voluntarily joins: the same evidence stream that believers read as vindication, payer seats read as the mechanism of their losses. Short sellers and skeptics experience enforced extraction (forced covering at momentum peaks; careers damaged by correct-but-early calls), while the excluded governance seat experiences the suppression dimension most acutely — its framework is dismissed without engagement. The founder seat sits near the beneficiary pole while administering the machinery. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for believer_shareholders, musk_controlled_enterprises, and financial_media_amplifiers; victim declarations drive high directionality for short_sellers and valuation_skeptics. Exit modulation matters within the victim set: short sellers are mobile (capital redeploys after covering), so their effective extraction is dampened relative to valuation_skeptics, whose constrained exit (sector expertise is career-path-specific) traps the cost with them. The founder combines agenda-setting with benefit collection, placing him near the beneficiary pole despite administering the constraint. Index fund managers are declared beneficiaries but hold ambivalently — mandated holding without conviction places them nearer symmetric than the derivation from the beneficiary label alone would suggest; no override was authored because the secondary payer role carries that ambivalence structurally. Identity_lock concentrates the believer seat's beneficiary position: exit is not merely costly but self-negating, which stabilizes the coordinated book that the extraction rides on.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — raising capital against expert consensus that the ventures were uninvestable — was plausibly solved by the mid-2010s, yet the norm persisted and scaled past its original function, which is the classic mandatrophy signature. Classifying this as tangled_rope rather than snare prevents erasing the genuine coordination function (the believer community really does solve a holding-through-volatility collective-action problem for its members, and the enterprises really did deliver vindicating milestones); classifying it as rope rather than snare's neighbor would erase the real bearing of costs by shorts and skeptics. The status is authored contested rather than dead because the ventures still pursue goals the industry calls impossible, so the problem's liveness is genuinely disputed. If the track-record predictive-validity omega resolves negatively — if the record stops predicting delivery at the required scale — the norm's justification detaches from its operation and the constraint trends toward theatrical maintenance administered by an agenda setter who could change it but for whom the cost of fixing (repricing the capital stack) exceeds what he bears: the prohibitive-fixing-cost cell. The classification keeps that trajectory visible instead of letting either the coordination story or the extraction story swallow the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading (musk_cult_believer) of the valuation_legitimacy kernel — would instantiating a sibling reading (dcf_fundamentalist, real_options_technologist, governance_skeptic) produce a different victim/beneficiary structure and classification?',
    'Author the three sibling stories against the same standing arrangement and compare computed per-seat classifications across the kernel family.',
    'Under governance_skeptic, minority shareholders enter the victim set and founder control re-reads as extraction rather than capability; under dcf_fundamentalist the legitimacy basis inverts and believer gains re-read as bubble artifact. The victim set, epsilon, and type are all reading-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: which reading of the valuation-legitimacy kernel is instantiated changes the structural data.').

omega_variable(
    track_record_predictive_validity,
    'Does past achievement of industry-declared-impossible goals predict future achievement at the required scale (Mars colony, unsupervised autonomy), or did the track record accumulate in regimes (launch economics, EV adoption S-curve) that do not extrapolate?',
    'Ex-post delivery audit: score stated timelines and milestone claims from 2016-2026 against realized outcomes, weighted by capital committed to each claim.',
    'If predictive validity is decaying, the legitimacy foundation erodes while the norm persists on inertia — the constraint drifts from coordination-plus-extraction toward inertial performance, and the authored extractiveness understates forward-looking extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(track_record_predictive_validity, empirical, 'Whether the track record is a valid predictor or a survivorship-selected sample.').

omega_variable(
    bankruptcy_warning_interpretation,
    'Are public distress warnings (''genuine risk of bankruptcy'') genuine risk assessments or bargaining tactics directed at suppliers, regulators, and labor?',
    'Compare warned risks against realized outcomes and subsequent capital-raise timing and terms; litigation-discovered internal communications where available.',
    'If tactical, the reading''s central interpretive move is confirmed and skeptic losses reflect misreading signaling; if genuine, the reading systematically understates risk and the authored extractiveness is understated — late entrants absorb risks the narrative priced as zero.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bankruptcy_warning_interpretation, empirical, 'Status of the distress-warning reinterpretation move on which the reading partly rests.').

omega_variable(
    believer_identity_lock_reversibility,
    'Is believer exit genuinely identity_locked (self-concept fused with the thesis such that selling equals self-betrayal), and what event class would break the fusion?',
    'Post-drawdown holder-behavior studies: compare realized selling among self-identified community members versus economically identical non-member holders after equivalent losses.',
    'If the lock breaks under a identifiable trigger, coordinated holding collapses discontinuously rather than decaying — changing the persistence profile and the enforcement burden the norm requires.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(believer_identity_lock_reversibility, empirical, 'Reversibility conditions of the identity lock binding the beneficiary seat.').

omega_variable(
    identity_frame_extraction_cover,
    'Does the identity-coordination framing cover a capital-transfer mechanism — dispersed late entrants and forced short covering funding insider liquidity events and early-holder exits?',
    'Flow-of-funds analysis: aggregate insider sales and early-holder realizations against believer-cohort net returns, controlling for index-driven flows.',
    'If the identity frame is cover, effective extraction is far above the authored 0.27 and the constraint drifts toward the pure-extraction end of the spectrum; if not, the coordination reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_frame_extraction_cover, conceptual, 'Identity-narrative gaming risk: whether relational framing conceals asymmetric transfer.').

omega_variable(
    interpretive_apparatus_falsifiability,
    'Is the reading''s evidence metabolism falsifiable — successes vindicate the thesis while failures are reframed as sandbagging, negotiation tactics, or enemy action — or does every outcome class confirm it?',
    'Pre-register which outcome classes would count against the thesis, then check whether the believer community has ever updated on any member of those classes.',
    'If the structure is unfalsifiable, the constraint''s persistence is decoupled from its justification — persistence then rests entirely on enforcement and identity maintenance rather than evidential support, which changes the classification inputs materially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_apparatus_falsifiability, conceptual, 'Falsifiability of the track-record legitimacy claim as operated by its interpretive layer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__musk_cult_believer, 0, 18).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(valu_tr_t0, valuation_legitimacy__musk_cult_believer, theater_ratio, 0, 0.05).
narrative_ontology:measurement(valu_tr_t3, valuation_legitimacy__musk_cult_believer, theater_ratio, 3, 0.07).
narrative_ontology:measurement(valu_tr_t6, valuation_legitimacy__musk_cult_believer, theater_ratio, 6, 0.09).
narrative_ontology:measurement(valu_tr_t9, valuation_legitimacy__musk_cult_believer, theater_ratio, 9, 0.11).
narrative_ontology:measurement(valu_tr_t12, valuation_legitimacy__musk_cult_believer, theater_ratio, 12, 0.14).
narrative_ontology:measurement(valu_tr_t15, valuation_legitimacy__musk_cult_believer, theater_ratio, 15, 0.17).
narrative_ontology:measurement(valu_tr_t18, valuation_legitimacy__musk_cult_believer, theater_ratio, 18, 0.18).

% Extraction over time
narrative_ontology:measurement(valu_be_t0, valuation_legitimacy__musk_cult_believer, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(valu_be_t3, valuation_legitimacy__musk_cult_believer, base_extractiveness, 3, 0.13).
narrative_ontology:measurement(valu_be_t6, valuation_legitimacy__musk_cult_believer, base_extractiveness, 6, 0.16).
narrative_ontology:measurement(valu_be_t9, valuation_legitimacy__musk_cult_believer, base_extractiveness, 9, 0.19).
narrative_ontology:measurement(valu_be_t12, valuation_legitimacy__musk_cult_believer, base_extractiveness, 12, 0.24).
narrative_ontology:measurement(valu_be_t15, valuation_legitimacy__musk_cult_believer, base_extractiveness, 15, 0.26).
narrative_ontology:measurement(valu_be_t18, valuation_legitimacy__musk_cult_believer, base_extractiveness, 18, 0.27).

% Suppression requirement over time
narrative_ontology:measurement(valu_su_t0, valuation_legitimacy__musk_cult_believer, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(valu_su_t3, valuation_legitimacy__musk_cult_believer, suppression_requirement, 3, 0.24).
narrative_ontology:measurement(valu_su_t6, valuation_legitimacy__musk_cult_believer, suppression_requirement, 6, 0.28).
narrative_ontology:measurement(valu_su_t9, valuation_legitimacy__musk_cult_believer, suppression_requirement, 9, 0.33).
narrative_ontology:measurement(valu_su_t12, valuation_legitimacy__musk_cult_believer, suppression_requirement, 12, 0.38).
narrative_ontology:measurement(valu_su_t15, valuation_legitimacy__musk_cult_believer, suppression_requirement, 15, 0.41).
narrative_ontology:measurement(valu_su_t18, valuation_legitimacy__musk_cult_believer, suppression_requirement, 18, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__musk_cult_believer, identity_coordination).
narrative_ontology:affects_constraint(valuation_legitimacy__musk_cult_believer, valuation_legitimacy__dcf_fundamentalist).
narrative_ontology:affects_constraint(valuation_legitimacy__musk_cult_believer, valuation_legitimacy__real_options_technologist).
narrative_ontology:affects_constraint(valuation_legitimacy__musk_cult_believer, valuation_legitimacy__governance_skeptic).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'is the valuation legitimate?' decomposes into four structurally distinct readings of the valuation_legitimacy kernel, each with its own epsilon, victim set, and type. This story instantiates the musk_cult_believer reading only. The dcf_fundamentalist reading is upstream (the established convention this reading defines itself against); the real_options_technologist reading is adjacent (this reading's dominance increases that framework's adoption by supplying demand for high-multiple rationalization); the governance_skeptic reading contests from the control-structure axis and assigns an almost disjoint victim set. Linking all four via affects_constraints lets contamination propagate: erosion of the track-record premise devalues the option-space framing that borrows its optimism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
