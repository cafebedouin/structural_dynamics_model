% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__strong_exclusivity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_agreement_interpretive_kernel__strong_exclusivity_reading, []).

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
 *   constraint_id: trips_agreement_interpretive_kernel__strong_exclusivity_reading
 *   human_readable: TRIPS Strong-Exclusivity Reading: Uniform Pharmaceutical Patent Protections with Narrow Flexibilities
 *   domain: international_trade_law/public_health_policy/intellectual_property
 *
 * SUMMARY:
 *   The TRIPS Agreement is read here as mandating high, uniform
 *   pharmaceutical patent protections with narrow public-health flexibilities
 *   — the strong-exclusivity reading of the
 *   trips_agreement_interpretive_kernel. This file instantiates that one
 *   reading as a clean, epsilon-invariant constraint: the referent for
 *   extractiveness is the standing arrangement under contest (TRIPS as
 *   administered under strong construction, including the TRIPS-plus
 *   accretions layered through bilateral agreements), assessed by this
 *   reading's own lights; the sibling public-health-flexibility reading is a
 *   different constraint in a different file, not a hedge folded into this
 *   one. The claimed type is tangled_rope: a genuine coordination function
 *   (common enforceable minimums solving the free-rider problem in innovation
 *   incentives) operates through the same structure that transfers
 *   monopoly-period margins from patients and importing states to originator
 *   firms, and the structure holds only under active enforcement (dispute
 *   settlement, retaliation exposure, bilateral pressure). Claim and metrics
 *   are authored independently: the metrics below describe substantially
 *   extractive, actively enforced operation with a rising rent-defense share;
 *   the engine computes per-seat classifications from the structural data.
 *
 * KEY AGENTS:
 *   - originator_pharmaceutical_firms: Primary beneficiary (institutional/arbitrage) — collects monopoly-period margins; shaped the regime's design
 *   - high_income_exporting_states: Agenda-setter and indirect beneficiary (institutional/mobile) — administers enforcement, hosts the firms
 *   - low_income_importing_states: Payer (moderate/constrained) — implements minimums without manufacturing capacity; formally equal, leverage-poor
 *   - patients_without_generic_access: Payer (powerless/trapped) — bears monopoly prices and treatment gaps directly
 *   - developing_country_generic_manufacturers: Payer with residual benefit (organized/constrained) — excluded from on-patent markets, dominant off-patent
 *   - manufacturing_capable_middle_income_states: Payer with flexibility capacity (organized/constrained) — occasional compulsory-license users under pressure
 *   - civil_society_health_advocates: Analytical-political observer (organized/analytical) — documents, litigates, mobilized the Doha correction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.72).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.66).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "TRIPS Strong-Exclusivity Reading: Uniform Pharmaceutical Patent Protections with Narrow Flexibilities").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "international_trade_law/public_health_policy/intellectual_property").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__strong_exclusivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__strong_exclusivity_reading, '00d80f52-631e-4b75-824f-9a29887e9a72').
narrative_ontology:cs_kernel_codification('00d80f52-631e-4b75-824f-9a29887e9a72', fixed_text).
narrative_ontology:cs_authority_grounding('00d80f52-631e-4b75-824f-9a29887e9a72', lineage).
narrative_ontology:cs_interpretation_layer_present('00d80f52-631e-4b75-824f-9a29887e9a72').
narrative_ontology:cs_reading_relation('00d80f52-631e-4b75-824f-9a29887e9a72', trips_agreement_interpretive_kernel__public_health_flexibility_reading, forecloses).
narrative_ontology:cs_reading_relation('00d80f52-631e-4b75-824f-9a29887e9a72', trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, influences).
narrative_ontology:cs_axiom('00d80f52-631e-4b75-824f-9a29887e9a72', foundational, uniform_exclusivity_preconditions_pharma_innovation).
narrative_ontology:cs_axiom_status(uniform_exclusivity_preconditions_pharma_innovation, holdable).
narrative_ontology:cs_axiom_grounding('00d80f52-631e-4b75-824f-9a29887e9a72', uniform_exclusivity_preconditions_pharma_innovation, empirically_contingent).
narrative_ontology:cs_axiom('00d80f52-631e-4b75-824f-9a29887e9a72', foundational, health_flexibilities_are_conditioned_exceptions).
narrative_ontology:cs_axiom_status(health_flexibilities_are_conditioned_exceptions, holdable).
narrative_ontology:cs_axiom_grounding('00d80f52-631e-4b75-824f-9a29887e9a72', health_flexibilities_are_conditioned_exceptions, conventional).
narrative_ontology:cs_reference_frame('00d80f52-631e-4b75-824f-9a29887e9a72', strict_textual_uniform_exclusivity).
narrative_ontology:cs_drift_state('00d80f52-631e-4b75-824f-9a29887e9a72', post_doha_post_covid_waiver, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('00d80f52-631e-4b75-824f-9a29887e9a72', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__strong_exclusivity_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, originator_pharmaceutical_firms).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, high_income_exporting_states).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_importing_states).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patients_without_generic_access).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, developing_country_generic_manufacturers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, developing_country_generic_manufacturers).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, manufacturing_capable_middle_income_states).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, manufacturing_capable_middle_income_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold patent portfolios covering patented medicines worldwide and price well above competitive levels during the protection term. Financed and shaped the treaty's negotiation through industry associations, and continue to press for longer effective protection through bilateral trade agreements, data-exclusivity demands, and patent-term extensions. Can shift research investment, launch sequencing, and pricing across jurisdictions, and can grant voluntary licenses on chosen terms.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, originator_pharmaceutical_firms, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__strong_exclusivity_reading, originator_pharmaceutical_firms, agenda_setter).

% Negotiated and administer the regime through WTO councils and the dispute settlement system, and back it with unilateral trade-pressure tools and bilateral agreement templates that extend protection beyond treaty minimums. Host the major research-based firms, so export revenue and industry employment concentrate domestically. Can pursue plurilateral alternatives if the multilateral forum turns hostile.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, high_income_exporting_states, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__strong_exclusivity_reading, high_income_exporting_states, beneficiary).

% Must implement treaty-minimum patent standards without domestic pharmaceutical manufacturing, so protected prices flow outward for imported medicines. Hold formally equal votes in the WTO but face retaliation threats, aid conditionality, and legal-technical capacity limits when considering the treaty's public-health exceptions. Leaving the trading system is not a realistic option.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_importing_states, payer,
    moderate, generational, constrained, national).

% Need medicines on the timeline of illness, not of patent expiry. Face monopoly-period prices for patented drugs, have no substitute product to switch to, and depend on government flexibilities or donor programs they do not control. Costs land on household budgets, treatment interruption, and mortality.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patients_without_generic_access, payer,
    powerless, immediate, trapped, global).

% Built businesses producing newer medicines without permission under prior national legal regimes; product-patent obligations ended that model for on-patent molecules. Now operate in off-patent segments and voluntary-license territories, retaining scale and export reach concentrated in a few middle-income countries. Entry into protected markets waits on expiry or license-holder consent.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, developing_country_generic_manufacturers, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__strong_exclusivity_reading, developing_country_generic_manufacturers, beneficiary).

% Host significant generic industries and carry treaty obligations while retaining the technical capacity to invoke public-health exceptions. Have issued compulsory licenses during health crises and absorbed diplomatic and trade pressure for doing so. Leverage sits between the exporting and importing poles.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, manufacturing_capable_middle_income_states, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__strong_exclusivity_reading, manufacturing_capable_middle_income_states, beneficiary).

% Document access gaps, support litigation and license applications, and mobilized the diplomatic pressure that produced the 2001 ministerial declaration on public health. Collect no payments under the regime and bear no treatment costs directly; their seat is evidentiary and political.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, civil_society_health_advocates, observer,
    organized, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(trips_agreement_interpretive_kernel__strong_exclusivity_reading, originator_pharmaceutical_firms).
narrative_ontology:fixing_cost_class(trips_agreement_interpretive_kernel__strong_exclusivity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sets common, enforceable minimum exclusive-rights standards across trading partners, solving the free-rider problem in which any single state permitting copying undermines the incentive structure others maintain, and giving research investors a predictable multi-jurisdiction rights baseline.
% TRANSFER_FUNCTION: Moves monopoly-period pricing margins on patented medicines from patients, health systems, and generic producers — concentrated in low- and middle-income importing countries — to research-based pharmaceutical firms headquartered in high-income exporting states, along with policy discretion moved from importing states to the enforcing trade framework.
% ABSENT_VOICES: Patients in least-developed countries beyond NGO reach have no seat; sufferers of neglected tropical diseases stand outside both the patent bargain and the flexibility contest because their markets are too small to attract either protection-driven investment or access campaigning; future patients facing antimicrobial resistance are represented by no negotiating constituency. Unanimity behind 'balance' rhetoric arises partly because the worst-affected never entered the room.
% DISAPPEARANCE_RATIONALE: Patent standards would fragment within years as states reverted to heterogeneous national regimes; generic producers in capable middle-income states would expand immediately into newly unprotected markets; originator firms would re-price and re-site research around bilateral deals and market size; the trade system would lose a settled chapter and reopen negotiation. Pricing, procurement, and industrial-policy arrangements across dozens of states currently depend on the settlement holding.
% FOUNDING_PROBLEM: Before 1995, intellectual property standards varied widely and enforcement was weak; exporters of knowledge-intensive goods faced free-riding and unpredictable protection, and the negotiating premise held that uniform enforceable minimums would channel investment into innovation, including pharmaceutical research.
% FOUNDING_PROBLEM_CORROBORATION: Exporting-state trade ministries and industry associations attest the problem remains live, citing counterfeiting and enforcement gaps. Independent corroboration of the shifted picture comes entirely from outside the benefiting parties: innovation economists finding market size dominates patent strength for most drug classes, the WHO Commission on Intellectual Property Rights (2006), and UNDP/UNAIDS reporting — all attesting that uniform strong protection exceeds what pharmaceutical innovation requires and that access costs are real. No corroborating source outside the beneficiary set attests that the founding problem, as originally framed, justifies the current arrangement unchanged.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__strong_exclusivity_reading, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__strong_exclusivity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__strong_exclusivity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__strong_exclusivity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trips_agreement_interpretive_kernel__strong_exclusivity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness ends at 0.72: the transfer is large and asymmetric, but part of the margin funds genuine research and the Doha-era flexibilities keep the arrangement short of pure appropriation. Suppression (0.66) is authored as a raw structural property — it is not scaled by power or scope; the engine scales only extractiveness, by directionality and spatial scope. Theater (0.42) reflects a growing share of enforcement effort defending evergreening, data exclusivity, and launch delays rather than first-generation innovation. Accessibility_collapse (0.52): alternatives — compulsory licensing, parallel importation, LDC waivers — remain legally open but procedurally burdened, so alternatives persist rather than collapsing as they would under a natural law. Resistance (0.62): the Doha Declaration, the South African and Brazilian litigation-era mobilization, and the COVID waiver demand joined by over a hundred members constitute sustained, partially successful resistance. The measurement series run on one shared seven-point grid (every tracked metric authored at every point; 1995–2025 mapped to 0–30). Trajectories are monotonic-rising with two documented inflections — the post-Doha softening (t≈10–20) and the COVID-waiver confrontation spike (t≈25) followed by partial relaxation — external-shock responses rather than oscillatory cycles, so no intermittent-reinforcement mechanism is claimed.
 *
 * PERSPECTIVAL GAP:
 *   Payer seats compute a snare-flavored world: patients face trapped, near-full-target extraction; low-income importing states bear obligations whose escape routes require capacity they lack; generic producers lost their founding business model. Agenda-setter and beneficiary seats compute a rope-flavored world: exporting states and firms experience the arrangement as a bargained, maintained incentive system they would rebuild if dissolved. Same-nominal-level divergence is sharpest between high-income and low-income WTO members — formally equal votes, radically different enforcement leverage, manufacturing capacity, and retaliation exposure — which is what differentiates their exit options despite equal formal standing. The engine computes this per-seat divergence from power, exit, and role data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place originator firms near the subsidy end (d≈0.1): they collect the margins and hold arbitrage-grade exit, which damps effective extraction onto them further. Exporting states sit similarly low (d≈0.15) as indirect collectors and administrators. Victim declarations place patients near the full-target end (d≈0.95) — trapped exit amplifies — and low-income importing states high (d≈0.75) under constrained exit. Generic manufacturers and middle-income states sit mid-high (d≈0.6–0.7): net payers whose secondary positions (off-patent markets; usable flexibilities) damp slightly. Civil-society advocates take the analytical seat and feed no chi. Coalition note: patients are individually powerless, but the Doha outcome demonstrates coalition power when patient interests ally with middle-income states and NGOs — a meaningful share of the enforcement budget is spent preempting exactly that coalition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — fragmented, unenforceable IP standards deterring innovation investment — is contested rather than dead: enforcement-gap concerns persist in other sectors, but for pharmaceuticals the independent literature finds market size, not patent strength, dominates investment decisions, and the rent-defense share of enforcement activity keeps rising (theater 0.20→0.45 across the interval). The classification prevents two opposite mislabels: reading the arrangement as pure snare would erase the real coordination function (common enforceable minimums do solve a free-rider problem); reading it as pure rope would erase the asymmetric transfer and the coercion needed to hold it. Tangled_rope is the honest label; rising theater and accumulating extraction mark it as a tangled rope drifting toward snare, not a piton — the administrator coalition still profits enough to maintain it actively. Watch items: if flexibility pathways become genuinely usable and margins compress, the constraint migrates back toward rope; if TRIPS-plus accretion continues, toward snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the TRIPS text hardens into operative law — the strong-exclusivity construction instantiated here, the public-health-flexibility construction, or a dispute-settlement-centered allocation of interpretive authority?',
    'Track Appellate Body and panel jurisprudence, ministerial declarations, waiver decisions, and membership practice for which construction stabilizes.',
    'If the flexibility reading prevails, patients and importing states gain usable pathways, epsilon falls toward coordination-cost levels, and this constraint converges toward rope; if the strong reading hardens, suppression deepens and the constraint trends snare. This story is one reading of kernel trips_agreement_interpretive_kernel; sibling readings restructure the beneficiary and victim sets rather than adjusting a shared metric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer-frame uncertainty: which sibling reading of the TRIPS kernel governs, and what structural delta each would produce.').

omega_variable(
    patent_incentive_empirical_basis,
    'Does uniform strong patent protection causally deliver proportionate pharmaceutical innovation, as this reading''s foundational axiom requires?',
    'Natural experiments: patent-term extensions, market-size shocks, and prize-fund or push-funding pilots compared against research output in affected therapeutic classes.',
    'If the causal link is weak, the coordination justification collapses and the arrangement trends snare; if strong, part of the measured transfer is the price of the incentive function itself rather than excess extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patent_incentive_empirical_basis, empirical, 'Empirical contingency of the foundational innovation-incentive axiom.').

omega_variable(
    flexibility_usability_gap,
    'Are the post-Doha compulsory-licensing and import pathways practically usable by low-capacity states, or procedurally choked into paper rights?',
    'Audit actual license issuances, application timelines, and outcomes after 2001 against legal-technical capacity measures of the issuing states.',
    'Separates structural suppression from capacity deficit; if the rights are largely paper, accessibility_collapse is understated and part of the suppression is internalized as learned futility in importing states, persisting even where formal barriers are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(flexibility_usability_gap, empirical, 'De jure versus de facto usability of the flexibilities this reading construes narrowly.').

omega_variable(
    trips_plus_attribution,
    'How much of the measured extraction originates in the treaty text itself versus TRIPS-plus provisions layered through bilateral agreements?',
    'Matched comparison of free-trade-agreement parties and non-parties controlling for income and disease burden.',
    'Decomposes epsilon across the constraint family; a large TRIPS-plus share may warrant splitting this story into bare-treaty and TRIPS-plus constraints per the epsilon-invariance principle, linked by network edges.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(trips_plus_attribution, empirical, 'Attribution ambiguity between the kernel-level arrangement and its bilateral accretions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trips_se_reading_tr_t0, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(trips_se_reading_tr_t5, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(trips_se_reading_tr_t10, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(trips_se_reading_tr_t15, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(trips_se_reading_tr_t20, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(trips_se_reading_tr_t25, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 25, 0.45).
narrative_ontology:measurement(trips_se_reading_tr_t30, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(trips_se_reading_be_t0, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(trips_se_reading_be_t5, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(trips_se_reading_be_t10, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(trips_se_reading_be_t15, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 15, 0.73).
narrative_ontology:measurement(trips_se_reading_be_t20, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 20, 0.74).
narrative_ontology:measurement(trips_se_reading_be_t25, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 25, 0.76).
narrative_ontology:measurement(trips_se_reading_be_t30, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 30, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(trips_se_reading_su_t0, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(trips_se_reading_su_t5, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(trips_se_reading_su_t10, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(trips_se_reading_su_t15, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 15, 0.64).
narrative_ontology:measurement(trips_se_reading_su_t20, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement(trips_se_reading_su_t25, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(trips_se_reading_su_t30, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 30, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__strong_exclusivity_reading, resource_allocation).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, public_health_flexibility_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, dispute_settlement_interpretive_authority).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'TRIPS': one treaty text yields three structurally distinct claims — who interprets it (dispute_settlement_interpretive_authority), what the flexibilities clauses embed (public_health_flexibility_reading), and what protections the text mandates (this story, strong_exclusivity_reading). Each carries its own epsilon, beneficiary/victim structure, and classification; they are linked here rather than collapsed into one observable-dependent story. The upstream dispute-authority reading shapes the operating environment of both substantive readings; the two substantive readings contradict on the same clauses' meaning.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
