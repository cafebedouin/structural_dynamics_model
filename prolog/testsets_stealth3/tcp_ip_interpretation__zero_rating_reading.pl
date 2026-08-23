% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__zero_rating_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tcp_ip_interpretation__zero_rating_reading, []).

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
 *   constraint_id: tcp_ip_interpretation__zero_rating_reading
 *   human_readable: Sponsored-Content Zero-Rating Exemption Regime
 *   domain: technological/regulatory
 *
 * SUMMARY:
 *   This story instantiates the zero_rating_reading of the
 *   tcp_ip_interpretation kernel: the claim that TCP/IP, as a layered
 *   transport specification, carries no normative commitment about
 *   application-layer economics, and therefore permits network operators to
 *   sell selective data-cap exemptions to sponsoring content providers. The
 *   standing arrangement under contest — and the sole epsilon referent here —
 *   is the operating regime of paid zero-rating: carriers whitelist
 *   sponsor-funded services whose traffic bypasses subscriber data caps. Per
 *   the epsilon-invariance principle the colloquial label 'what TCP/IP
 *   allows' decomposes into three structurally distinct stories (see
 *   network.dual_formulation_note); this file carries only the
 *   sponsored-exemption reading, with its own epsilon, beneficiary structure,
 *   and classification, linked to its siblings through network edges. The
 *   structural delta the kernel context hands this reading — incumbent
 *   platforms advantaged, competitive entry barriers raised — is recorded
 *   honestly in the beneficiary/victim declarations. The claim and the
 *   metrics are authored independently: the claimed type states what I
 *   believe structurally true, the metrics what I believe descriptively true,
 *   and any divergence from computed per-seat classifications is the datum
 *   the corpus exists to take.
 *
 * KEY AGENTS:
 *   - - mobile_carriers: agenda-setter and receipt-holder (institutional/arbitrage) — designs the exemption programs, runs whitelist administration and differential cap accounting, collects sponsorship revenue
 *   - - incumbent_platforms: principal sponsor-beneficiary (powerful/mobile) — pays exemption fees that purchase metric-free distribution and raise rivals' relative costs
 *   - - indie_content_providers: primary target (powerless/constrained) — identical bytes count fully against caps while sponsored rivals stream free; sponsorship priced beyond reach
 *   - - capped_mobile_users: mixed paying-beneficiary seat (organized/constrained) — receives whitelisted free access, carries the narrowed and implicitly reallocated allowance environment
 *   - - subsidized_access_users: incidental beneficiary (powerless/trapped) — sponsored free tiers are the only affordable on-ramp to selected services
 *   - - net_neutrality_regulators: analytical observer (institutional/analytical) — adjudicate permissibility across divergent national regimes
 *   - - would_be_market_entrants: excluded voice (powerless/constrained) — absent from the bilateral negotiations that define exemption terms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__zero_rating_reading, 0.66).
domain_priors:suppression_score(tcp_ip_interpretation__zero_rating_reading, 0.65).
domain_priors:theater_ratio(tcp_ip_interpretation__zero_rating_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__zero_rating_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__zero_rating_reading, "Sponsored-Content Zero-Rating Exemption Regime").
narrative_ontology:topic_domain(tcp_ip_interpretation__zero_rating_reading, "technological/regulatory").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__zero_rating_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__zero_rating_reading, '692c89e1-4a8b-4a5d-bce3-93947d2b8d10').
narrative_ontology:cs_kernel_codification('692c89e1-4a8b-4a5d-bce3-93947d2b8d10', formalized).
narrative_ontology:cs_authority_grounding('692c89e1-4a8b-4a5d-bce3-93947d2b8d10', extraction).
narrative_ontology:cs_interpretation_layer_present('692c89e1-4a8b-4a5d-bce3-93947d2b8d10').
narrative_ontology:cs_reading_relation('692c89e1-4a8b-4a5d-bce3-93947d2b8d10', tcp_ip_interpretation__neutrality_reading, forecloses).
narrative_ontology:cs_reading_relation('692c89e1-4a8b-4a5d-bce3-93947d2b8d10', tcp_ip_interpretation__prioritization_reading, coexists_with).
narrative_ontology:cs_axiom('692c89e1-4a8b-4a5d-bce3-93947d2b8d10', foundational, edge_economics_outside_protocol_scope).
narrative_ontology:cs_axiom_status(edge_economics_outside_protocol_scope, holdable).
narrative_ontology:cs_axiom_grounding('692c89e1-4a8b-4a5d-bce3-93947d2b8d10', edge_economics_outside_protocol_scope, conventional).
narrative_ontology:cs_axiom('692c89e1-4a8b-4a5d-bce3-93947d2b8d10', secondary, sponsored_exemption_expands_access).
narrative_ontology:cs_axiom_status(sponsored_exemption_expands_access, holdable).
narrative_ontology:cs_axiom_grounding('692c89e1-4a8b-4a5d-bce3-93947d2b8d10', sponsored_exemption_expands_access, instrumental).
narrative_ontology:cs_reference_frame('692c89e1-4a8b-4a5d-bce3-93947d2b8d10', transport_indifference_to_edge_economics).
narrative_ontology:cs_drift_state('692c89e1-4a8b-4a5d-bce3-93947d2b8d10', contemporary_post_repeal_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('692c89e1-4a8b-4a5d-bce3-93947d2b8d10', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, mobile_carriers).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, incumbent_platforms).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, subsidized_access_users).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, indie_content_providers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, capped_mobile_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, capped_mobile_users).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, incumbent_platforms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate metered mobile data plans and administer the exemption programs: negotiate sponsorship agreements, maintain the whitelist of metric-free services, bill sponsors, and market free-data offers. Collect the sponsorship revenue and enjoy measurable churn reduction from the programs. Wrote the cap-accounting rules that decide whose traffic counts, and can reprice, expand, or retire programs at will.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, mobile_carriers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__zero_rating_reading, mobile_carriers, beneficiary).

% Large streaming, social, and messaging services that sign sponsorship deals so their traffic does not count against subscribers' data allowances. Pay the exemption fees, but the purchased metric-free treatment converts directly into usage share and retention; for them the arrangement is a marketing channel with measurable returns. Their scale lets them treat sponsorship as routine acquisition spending.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, incumbent_platforms, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__zero_rating_reading, incumbent_platforms, payer).

% Small developers, independent news sites, podcasters, and niche services with no sponsorship deal. Their traffic counts fully against every subscriber's data cap while sponsored competitors stream free, so a user watching an independent video spends allowance that an identical sponsored video would not. Sponsorship minimums and per-user fees sit far above their budgets; their practical options are slower growth, pushing audiences to wi-fi contexts, or exiting.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, indie_content_providers, payer,
    powerless, biographical, constrained, global).

% Subscribers on metered plans. Receive genuinely free access to whatever services their carrier has whitelisted, but every non-sponsored byte draws down a finite allowance, making off-whitelist content effectively more expensive than before the programs existed. Switching carriers means comparing overlapping but different whitelists, contract terms, and coverage; VPN workarounds are discouraged and sometimes degraded.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, capped_mobile_users, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__zero_rating_reading, capped_mobile_users, beneficiary).

% Price-sensitive subscribers, disproportionately in developing markets and low-income segments, for whom sponsored free tiers are the only affordable window onto selected online services. Their connectivity depends on staying inside sponsor-curated catalogs; full-price open-internet access remains out of reach.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, subsidized_access_users, beneficiary,
    powerless, immediate, trapped, regional).

% National communications authorities evaluating whether paid data-cap exemptions constitute discriminatory traffic treatment. Some have banned differential pricing outright (India, Chile), some restrict it within broader open-internet frameworks (EU), and some tolerate it case-by-case (US after 2017). They weigh carrier submissions, platform filings, startup complaints, and academic evidence.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, net_neutrality_regulators, observer,
    institutional, generational, analytical, national).

% Founders considering launching video, social, messaging, or media products in markets where incumbents ride metric-free. They are not parties to the bilateral carrier-platform negotiations that define exemption terms; their category's customer-acquisition math changes before they ever enter. Pivoting to other markets or non-mobile-first products is possible but costly.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, would_be_market_entrants, excluded,
    powerless, immediate, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tcp_ip_interpretation__zero_rating_reading, mobile_carriers).
narrative_ontology:fixing_cost_class(tcp_ip_interpretation__zero_rating_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Matches content sponsors' payment to carriers' cap economics: sponsorship revenue funds network capacity while price-sensitive users receive free access to selected services — solving, for participants, the problem of monetizing demand for data among users unwilling or unable to pay metered rates.
% TRANSFER_FUNCTION: Moves sponsorship fees from large content platforms to network operators; moves metric-free treatment toward sponsored incumbents and away from unsponsored providers; implicitly reallocates users' finite data allowances toward sponsor ecosystems.
% ABSENT_VOICES: Would-be entrants and unsponsored indie providers are absent from the bilateral negotiations where exemption terms are defined; their interests reach the table only secondhand through advocacy submissions to regulators, and in jurisdictions without active proceedings nobody present represents the unsponsored middle of the content economy.
% DISAPPEARANCE_RATIONALE: If paid selective exemption vanished overnight, carrier revenue lines built on sponsorship billing would collapse, incumbent platforms would lose a purchased distribution advantage and reopen customer-acquisition competition on price and product, indie providers' effective costs would drop back to parity, and subsidized-access programs would need replacement through universal-service mechanisms — the mobile content economy would reorganize around uniform metering.
% FOUNDING_PROBLEM: Flat-rate plans priced beyond many users' reach while marginal sponsored traffic cost carriers almost nothing — zero-rating emerged to bridge users' affordability gap and carriers' network-utilization gap simultaneously.
% FOUNDING_PROBLEM_CORROBORATION: Sources outside the benefiting parties corroborate the shifted-function reading: India's TRAI found the affordability rationale insufficient against discrimination harms and prohibited differential pricing in 2016; BEREC's guidelines treat zero-rating as presumptively problematic; peer-reviewed studies of curated free-tier programs found limited conversion from sponsored access to open internet use. Carrier associations and sponsoring platforms, predictably, attest the founding problem remains live. No disinterested body attests it unchanged.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__zero_rating_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__zero_rating_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__zero_rating_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tcp_ip_interpretation__zero_rating_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__zero_rating_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tcp_ip_interpretation__zero_rating_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tcp_ip_interpretation__zero_rating_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tcp_ip_interpretation__zero_rating_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.66: metric-free treatment is sold rather than allocated by cost or public-interest criteria; the fee schedule prices smaller providers out of parity, and the entry-barrier effect is a designed output — tempered below pure-extraction levels by the genuine subsidized-access function some programs deliver. Suppression 0.65 is the raw structural figure (unscaled; the engine owns directionality and scope arithmetic): persistence requires active machinery — whitelist administration, differential cap accounting, terms discouraging VPN circumvention — but exits persist (wi-fi offload, rival carriers, whole-jurisdiction bans), so this is enforced asymmetry rather than sealed closure. Theater_ratio 0.45 and rising across the grid: 'digital inclusion' and 'network management' framings increasingly dress the advantage marketplace while delivery and billing functions remain real. accessibility_collapse 0.45 and resistance 0.6 record that alternatives and opposition survive — India's 2016 differential-pricing ban, Chile's earlier ban, BEREC's restrictive guidelines, the US 2015-order/2017-repeal whiplash, ongoing startup and academic challenge. All three series share one eight-point annual grid (2010-2024); suppression_requirement is authored because the story tracks enforcement build-out (program maturation, repeal-era hardening) and partial decay (European enforcement chilling programs), not merely extraction drift; the small 2022 dip reflects that chill, not a cycle. Receipt: sponsorship revenue demonstrably accrues to mobile_carriers. Fixing is prohibitive for the actors positioned to fix it: every serious remedial attempt has drawn litigation, lobbying, and reversal (US) or required extraordinary popular mobilization (India) — costs exceeding what any single office bears from the status quo. The main latent counterweight is coalition formation among indie providers (joint regulatory filings, app-fairness coalitions), which remained unrealized at interval end.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently by construction. From the carrier seat the arrangement is a product line it designed: cap accounting, whitelisting, and sponsorship billing are operations it runs, placing it near the beneficiary pole. From the indie-provider seat the same whitelist is a metering regime it cannot join: identical bytes, opposite charge incidence. The sharpest divergence is lateral — incumbent platforms and indie providers occupy nominally the same level (content providers facing the same caps), yet the constraint-specific factor of sponsorship affordability splits them into beneficiary and payer; global standing alone does not explain the split, the fee schedule does. Inter-institutionally, regulators experience the arrangement as an adjudication object whose evidence base is largely produced by the very parties it evaluates. Subsidized-access users experience something close to pure provision; capped general users occupy the mixed position their dual role records.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared structure maps to directionality as follows: mobile_carriers hold the agenda_setter role, collect the receipts (named in gain_flow), and control the accounting rules — nearest the beneficiary pole. incumbent_platforms are declared beneficiaries despite paying fees because the purchased metric-free treatment is worth more than the fee at their scale; the secondary payer role moderates their position upward but leaves them net-subsidized. subsidized_access_users receive the arrangement's genuine provision and sit low-d. capped_mobile_users carry the mixed position: free whitelisted access against a narrowed, implicitly reallocated allowance environment — mid-to-high d. indie_content_providers bear the asymmetric cost with no compensating flow — nearest the full-target pole, amplified by constrained rather than arbitrage-grade exit. Regulators are analytical; would-be entrants are excluded and commentary-grade only. No directionality overrides were needed: beneficiary/victim declarations plus exit options reproduce these positions without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the regime as pure coordination would erase the transfer: the fee schedule, not any capacity constraint, decides whose traffic rides free, and raised entry barriers are a designed output. Reading it as pure extraction would erase the subsidized-access half that some users genuinely depend on and that motivated the earliest programs. The tangled_rope claim holds both halves apart instead of collapsing either. On the genealogy: the founding affordability problem is authored as contested rather than dead, because the parties dispute whether plan competition and falling handset prices dissolved it — carriers and sponsors attest it live; regulators' findings (India's prohibition, BEREC's treatment) and adoption studies of curated free tiers corroborate the shifted-function reading from outside the benefiting parties. The status-times-verdict mismatch consumer is the guard here: if the founding problem is dead while the world still rearranges around the arrangement, capture is flagged and cross-checked against the theater trajectory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tcp_ip_kernel_reading_location,
    'This story instantiates the zero_rating_reading of the tcp_ip_interpretation kernel. Which of the three declared readings governs a given jurisdiction''s treatment of sponsored data exemptions, and where exactly is the disagreement located?',
    'Adjudication in legislative, regulatory, and judicial fora over the normative content of the protocol suite: whether the end-to-end design carries a nondiscrimination requirement (neutrality_reading), a managed-differentiation permission limited to service quality (prioritization_reading), or no application-economics commitment at all (this reading).',
    'Under the neutrality_reading this arrangement is a discriminatory practice to prohibit; under the prioritization_reading metering distinctions remain out of bounds even where quality tiers are lawful; under this reading the arrangement is presumptively lawful subject to ordinary competition law. Victim sets, remedies, and per-seat classifications all shift with the adopted reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tcp_ip_kernel_reading_location, conceptual, 'Committer-frame omega: which reading of the TCP/IP interpretive kernel governs, and where the readings disagree (whether the protocol embeds any normative commitment about edge economics).').

omega_variable(
    subsidy_advantage_separability,
    'Is the subsidized-access function of sponsored exemptions structurally separable from the incumbent-advantage function, or does purchasing metric-free treatment necessarily concentrate distribution advantage?',
    'Cross-jurisdiction comparison of programs limited to nonprofit or public-interest exemptions versus paid commercial sponsorship, tracking usage conversion and new-provider entry rates in each.',
    'If separable, a remedy preserving subsidized access while capping sponsorship fees is available and the coordination half survives reform; if inseparable, part of the measured extraction is the price of the subsidy itself and bans trade access expansion against competition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidy_advantage_separability, empirical, 'Whether the arrangement''s provision half and advantage half can be separated by design.').

omega_variable(
    entry_barrier_magnitude,
    'How large is the entry-barrier effect of sponsored-exemption regimes on new content and application providers?',
    'Difference-in-differences analysis of startup formation and survival in sponsorship-eligible categories across jurisdictions that banned versus tolerated paid zero-rating.',
    'Large measured barriers push the arrangement toward the extractive pole and support structural remedies; negligible effects support a coordination-dominant reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entry_barrier_magnitude, empirical, 'Magnitude of the competitive harm the kernel context attributes to this reading.').

omega_variable(
    user_welfare_net_direction,
    'Do capped subscribers gain or lose net welfare under sponsored-exemption regimes once narrowed choice environments and implicit reallocation of their allowances are priced in?',
    'Revealed-preference studies of program uptake, substitution behavior, and willingness-to-pay for off-whitelist usage.',
    'Net losses push the user seat toward full-target directionality; net gains support treating users as genuine secondary beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_welfare_net_direction, empirical, 'Net welfare direction for the mixed-position user seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__zero_rating_reading, 2010, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp__tr_t2010, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement_basis(tcp__tr_t2010, observed).
narrative_ontology:measurement(tcp__tr_t2012, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 2012, 0.22).
narrative_ontology:measurement_basis(tcp__tr_t2012, observed).
narrative_ontology:measurement(tcp__tr_t2014, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 2014, 0.28).
narrative_ontology:measurement_basis(tcp__tr_t2014, observed).
narrative_ontology:measurement(tcp__tr_t2016, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 2016, 0.34).
narrative_ontology:measurement_basis(tcp__tr_t2016, observed).
narrative_ontology:measurement(tcp__tr_t2018, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 2018, 0.4).
narrative_ontology:measurement_basis(tcp__tr_t2018, observed).
narrative_ontology:measurement(tcp__tr_t2020, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 2020, 0.42).
narrative_ontology:measurement_basis(tcp__tr_t2020, observed).
narrative_ontology:measurement(tcp__tr_t2022, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 2022, 0.44).
narrative_ontology:measurement_basis(tcp__tr_t2022, observed).
narrative_ontology:measurement(tcp__tr_t2024, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 2024, 0.45).
narrative_ontology:measurement_basis(tcp__tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(tcp__be_t2010, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 2010, 0.28).
narrative_ontology:measurement_basis(tcp__be_t2010, observed).
narrative_ontology:measurement(tcp__be_t2012, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 2012, 0.36).
narrative_ontology:measurement_basis(tcp__be_t2012, observed).
narrative_ontology:measurement(tcp__be_t2014, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 2014, 0.48).
narrative_ontology:measurement_basis(tcp__be_t2014, observed).
narrative_ontology:measurement(tcp__be_t2016, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 2016, 0.57).
narrative_ontology:measurement_basis(tcp__be_t2016, observed).
narrative_ontology:measurement(tcp__be_t2018, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 2018, 0.61).
narrative_ontology:measurement_basis(tcp__be_t2018, observed).
narrative_ontology:measurement(tcp__be_t2020, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement_basis(tcp__be_t2020, observed).
narrative_ontology:measurement(tcp__be_t2022, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 2022, 0.64).
narrative_ontology:measurement_basis(tcp__be_t2022, observed).
narrative_ontology:measurement(tcp__be_t2024, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 2024, 0.66).
narrative_ontology:measurement_basis(tcp__be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(tcp__su_t2010, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 2010, 0.25).
narrative_ontology:measurement_basis(tcp__su_t2010, observed).
narrative_ontology:measurement(tcp__su_t2012, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 2012, 0.33).
narrative_ontology:measurement_basis(tcp__su_t2012, observed).
narrative_ontology:measurement(tcp__su_t2014, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 2014, 0.48).
narrative_ontology:measurement_basis(tcp__su_t2014, observed).
narrative_ontology:measurement(tcp__su_t2016, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 2016, 0.55).
narrative_ontology:measurement_basis(tcp__su_t2016, observed).
narrative_ontology:measurement(tcp__su_t2018, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 2018, 0.62).
narrative_ontology:measurement_basis(tcp__su_t2018, observed).
narrative_ontology:measurement(tcp__su_t2020, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 2020, 0.64).
narrative_ontology:measurement_basis(tcp__su_t2020, observed).
narrative_ontology:measurement(tcp__su_t2022, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 2022, 0.63).
narrative_ontology:measurement_basis(tcp__su_t2022, observed).
narrative_ontology:measurement(tcp__su_t2024, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 2024, 0.65).
narrative_ontology:measurement_basis(tcp__su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__zero_rating_reading, resource_allocation).
narrative_ontology:affects_constraint(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation__neutrality_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation__prioritization_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'what TCP/IP allows' decomposes into three structurally distinct constraint stories per the epsilon-invariance principle: the neutrality_reading (a nondiscrimination requirement — negligible extraction, mountain-like from the engineering seat), the prioritization_reading (permission for managed service-quality tiers — contested at the margins), and this file's zero_rating_reading (permission for paid selective exemption — the family member with the widest beneficiary/victim spread and the highest epsilon). The neutrality doctrine functions as the normative upstream this reading explicitly departs from; this reading's commercial deployment exerts downstream pressure on prioritization debates by normalizing paid differentiation. Each file carries its own epsilon, stakeholders, and classification; the affects_constraints edges link the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
