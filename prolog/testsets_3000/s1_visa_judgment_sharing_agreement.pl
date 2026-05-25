% ============================================================================
% CONSTRAINT STORY: s1_visa_judgment_sharing_agreement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_s1_visa_judgment_sharing_agreement, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: s1_visa_judgment_sharing_agreement
 *   human_readable: Visa Judgment Sharing Agreement (AMEX Antitrust Case)
 *   domain: legal/economic
 *
 * SUMMARY:
 *   The Visa Judgment Sharing Agreement (AMEX antitrust case) represents a
 *   structural hybrid between legal remedy coordination and economic
 *   extraction lock-in. In 2012-2015, American Express litigated antitrust
 *   claims against Visa and Mastercard for anticompetitive network rules and
 *   fee structures. The judgment ruled in AMEX's favor on key monopolization
 *   claims. However, rather than enforcing direct price correction, the
 *   settlement created a judgment sharing agreement: monetary compensation to
 *   prior claimants and current merchants, with settlement administrators
 *   distributing funds while the underlying pricing power of the
 *   Visa/Mastercard duopoly remained intact. The constraint exhibits all
 *   characteristics of tangled rope: genuine coordination function
 *   (distributing $100M+ to millions of claimants requires administrative
 *   infrastructure), asymmetric extraction (benefiting the duopoly by
 *   preventing subsequent litigation and rate adjustment), and active
 *   enforcement (court-supervised settlement administration). The theater
 *   ratio reflects that the judgment distribution emphasizes procedural
 *   fairness (settlement administration transparency, claim validation) while
 *   leaving the structural antitrust harm (duopoly pricing on merchants)
 *   unresolved. The extractiveness trajectory shows initial high values
 *   (immediate post-judgment period when duopoly faced existential pricing
 *   pressure) declining as the settlement locked the fee structure in place
 *   and made subsequent challenges more difficult.
 *
 * KEY AGENTS:
 *   - Visa and Mastercard Duopoly: Primary beneficiary (institutional/arbitrage) — converts antitrust judgment into settlement framework that preserves pricing power and prevents future rate-correction litigation
 *   - American Express: Victim despite judgment victory (moderate/constrained) — wins antitrust case but receives restricted compensation; cannot capture full remedy; bears litigation costs; constrained by network infrastructure dependencies
 *   - Merchant Ecosystems: Primary victim (powerless/trapped) — continue paying extracted interchange fees; settlement distribution does not reach current merchants; cannot exit network without losing customer reach
 *   - Prior Litigation Claimants: Mixed beneficiary (moderate/constrained) — receive settlement compensation but may be disadvantaged relative to new claimants; locked into historical distribution formulas
 *   - Settlement Administrators: Organized agents (organized/constrained) — operate under court mandate; solve real coordination problem (claims distribution) but cannot exit administrative role; constrained by judicial supervision
 *   - Consumer Payment Choice: Structural victim (powerless/trapped) — duopoly continues to control network access and pricing; settlement does not enable competitive alternatives; trapped in duopoly infrastructure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(s1_visa_judgment_sharing_agreement, 0.58).
domain_priors:suppression_score(s1_visa_judgment_sharing_agreement, 0.62).
domain_priors:theater_ratio(s1_visa_judgment_sharing_agreement, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(s1_visa_judgment_sharing_agreement, extractiveness, 0.58).
narrative_ontology:constraint_metric(s1_visa_judgment_sharing_agreement, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(s1_visa_judgment_sharing_agreement, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(s1_visa_judgment_sharing_agreement, tangled_rope).
narrative_ontology:human_readable(s1_visa_judgment_sharing_agreement, "Visa Judgment Sharing Agreement (AMEX Antitrust Case)").
narrative_ontology:topic_domain(s1_visa_judgment_sharing_agreement, "legal/economic").

domain_priors:requires_active_enforcement(s1_visa_judgment_sharing_agreement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(s1_visa_judgment_sharing_agreement, visa_mastercard_duopoly).
narrative_ontology:constraint_beneficiary(s1_visa_judgment_sharing_agreement, settlement_administrators).
narrative_ontology:constraint_victim(s1_visa_judgment_sharing_agreement, american_express).
narrative_ontology:constraint_victim(s1_visa_judgment_sharing_agreement, merchant_ecosystems).
narrative_ontology:constraint_victim(s1_visa_judgment_sharing_agreement, consumer_payment_choice).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MERCHANT ECOSYSTEM (SNARE) — Small and mid-sized merchants cannot exit Visa/Mastercard network participation without losing customer reach. The judgment sharing agreement locks merchants into continued extraction via interchange fees while settlement proceeds are distributed to prior claimants, not current fee-bearers. Trapped exit; maximum experienced extraction.
constraint_indexing:constraint_classification(s1_visa_judgment_sharing_agreement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: AMERICAN EXPRESS (SNARE) — Despite the judgment in AMEX's favor, the judgment sharing agreement prevents AMEX from capturing its full remedy. Visa and Mastercard internalize settlement costs while AMEX bears litigation burden and receives restricted compensation. Exit options constrained by network effects and processing infrastructure dependencies. Significant extraction despite litigation victory.
constraint_indexing:constraint_classification(s1_visa_judgment_sharing_agreement, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: VISA/MASTERCARD DUOPOLY (TANGLED ROPE) — Primary beneficiary. The judgment sharing agreement coordinates settlement distribution while preserving pricing power and network control. Duopoly experiences this as coordination (avoiding repeated antitrust exposure) combined with extraction (maintaining fee structure despite judgment). Arbitrage exit options via rate adjustments and settlement administration control.
constraint_indexing:constraint_classification(s1_visa_judgment_sharing_agreement, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SETTLEMENT ADMINISTRATORS (ROPE) — Third-party settlement administrators operate under court mandate with constrained exit (cannot refuse administration). But the mechanism solves a real coordination problem: distributing $100M+ to millions of class members and prior claimants. Low extractiveness because administration operates under judicial supervision and transparency requirements. Pure coordination from this institutional perspective.
constraint_indexing:constraint_classification(s1_visa_judgment_sharing_agreement, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: JUDICIAL REMEDIES THEATER (PITON) — The judgment sharing agreement maintains the appearance of legal remedy and consumer protection while preserving the underlying antitrust harm (duopoly pricing). Courts enforce the settlement framework, but the settlement's functional capacity to remedy the constraint is degraded — merchants still pay interchange, AMEX still bears costs, duopoly still controls networks. Theater ratio high because settlement structure emphasizes distribution procedure over price correction. Institutional inertia: the remedial mechanism persists because no alternative enforcement infrastructure has matured.
constraint_indexing:constraint_classification(s1_visa_judgment_sharing_agreement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/analytical perspective, the judgment sharing agreement is a hybrid coordination-extraction mechanism that solves the settlement distribution problem (genuine coordination function) while locking the duopoly's market power into place (structural extraction). Bifurcation: remedy for past violations is coordinated; prevention of future violations is partially extracted. The agreement bridges past (settlement) and future (continued fees) with asymmetric burden.
constraint_indexing:constraint_classification(s1_visa_judgment_sharing_agreement, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(s1_visa_judgment_sharing_agreement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(s1_visa_judgment_sharing_agreement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(s1_visa_judgment_sharing_agreement, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(s1_visa_judgment_sharing_agreement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(s1_visa_judgment_sharing_agreement, TR),
    TR >= 0.70.

:- end_tests(s1_visa_judgment_sharing_agreement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The settlement initially appeared to be a clear victory for AMEX and merchants, triggering potential duopoly price correction. However, the judgment sharing agreement preserved the duopoly's pricing power while satisfying legal remedy requirements through monetary compensation. Current merchants bear continued interchange extraction but receive no direct compensation — only prior claimants do. The extractiveness trajectory declines from 0.72 (pre-settlement when duopoly faced real pricing pressure) to 0.58 (post-settlement when fees are locked in by agreement) because the duopoly successfully converted the existential threat into a manageable monetary obligation. Suppression (0.62): Moderate-high. Merchants cannot exit the Visa/Mastercard network without losing customer reach (network effects). AMEX cannot fully exploit its judgment victory because settlement structures prevent unilateral rate increases that would trigger relitigation and further settlements. The agreement suppresses alternative remedies: direct price regulation, break-up, or conduct injunctions are all off the table once settlement binds. Theater ratio (0.48): Moderate-low. The settlement administration is substantive — distribution actually occurs, claims are validated, funds flow. But the theater has increased over time because the functional remedy (price correction) has not materialized. The agreement emphasizes settlement procedure over antitrust outcome, creating theater drift as the gap widens between 'remedy was distributed' and 'duopoly pricing persists.'
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between merchant experience (Snare) and duopoly experience (Tangled Rope) is the diagnostic heart of this constraint. Merchants see pure extraction because settlement compensation flows to prior claimants, not current fee-payers, while extraction (interchange fees) continues indefinitely. The duopoly sees coordination because the settlement solves a critical problem: it prevents unlimited relitigation and establishes a predictable cost structure. American Express occupies an intermediate position: it sees the agreement as extraction (restricts its remedial options) but also coordinates (prevents further legal uncertainty). The analytical observer sees Tangled Rope because both functions are real — the agreement does coordinate settlement distribution AND does preserve extraction. The piton perspective identifies that judicial oversight creates theater: the remedy appears legitimate because courts supervise it, but the underlying antitrust harm is unresolved. This is judicial inertia — the settlement persists because it satisfies legal form requirements while courts retain authority to enforce, even though the settlement's substantive remedial capacity is degraded.
 *
 * DIRECTIONALITY LOGIC:
 *   The Visa/Mastercard duopoly derives low directionality (d ≈ 0.20-0.30) from beneficiary status (arbitrage exit) — the agreement benefits them by converting an existential threat into a managed obligation. American Express derives high directionality (d ≈ 0.70) from victim status (constrained exit) — it won the judgment but cannot fully capitalize on it; the settlement constrains its remedial options. Merchants derive maximum directionality (d ≈ 0.95) from powerless/trapped positioning — no exit options and continued extraction despite judgment. Settlement administrators derive moderate directionality (d ≈ 0.50) from organized/constrained positioning — they benefit from the settlement's existence (administration mandate, operational budget) but are constrained by judicial oversight and cannot modify terms. The analytical observer derives moderate-high directionality (d ≈ 0.65) because structural analysis reveals the coordination function is real but partially instrumentalized toward extraction preservation.
 *
 * MANDATROPHY ANALYSIS:
 *   The judgment sharing agreement resolves mandatrophy by decomposing remedy into two dimensions: past (settlement distribution) and future (pricing structure). PAST REMEDY: The agreement is genuine coordination — distributing $100M+ to claimants across millions of transactions requires administrative infrastructure that is not extraction. Settlement administration is Rope from the administrative perspective. FUTURE REMEDY: The agreement preserves the extraction structure — merchants continue to bear interchange, the duopoly maintains pricing power, competitive alternatives remain suppressed. Future antitrust harm is Snare from merchant and consumer perspectives. The constraint is Tangled Rope precisely because it accomplishes both simultaneously: it coordinates the settlement while extracting through fee structure preservation. The mandatrophy is resolved by observing that the classification changes based on time horizon — immediate/biographical perspectives see Snare (current extraction); institutional/generational perspectives see Tangled Rope (mixed coordination and structure locking). The agreement prevents misclassification as pure Rope (which would hide the fee preservation) or pure Snare (which would hide the settlement's genuine coordination function). The theater ratio evolution (0.35 → 0.48) tracks the growing gap between settlement procedure (increasingly transparent and coordinated) and remedy outcome (increasingly theatrical as fees persist). This is the classic piton signature: the mechanism becomes more theatrical as its function degrades.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    settlement_remedy_adequacy,
    'Does the judgment sharing agreement adequately remedy competitive harm, or does it merely distribute past damages while leaving the antitrust violation''s economic structure intact?',
    'Comparative analysis: post-settlement merchant fee levels vs. counterfactual competitive pricing; AMEX competitive position before/after settlement; network adoption patterns for alternative payment systems',
    'If adequate: settlement is primarily coordination (Rope). If inadequate: settlement is primarily extraction (Snare from merchant perspective). Classification depends on remedial sufficiency.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(settlement_remedy_adequacy, empirical, 'Whether settlement adequately remedies antitrust violation or preserves pricing structure').

omega_variable(
    prior_claimant_distribution_fairness,
    'Is the distribution of settlement proceeds to prior litigation participants fair relative to current merchant victims, or does it create path-dependent lock-in favoring earlier claimants?',
    'Historical analysis of claimant composition across litigation waves; comparison of per-merchant compensation across claim cohorts; market impact analysis on merchant retention post-settlement',
    'If fair: settlement is legitimate coordination. If unfair: settlement perpetuates historical extraction patterns under judicial cover. Theater ratio interpretation shifts accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prior_claimant_distribution_fairness, empirical, 'Fairness of settlement distribution across claim cohorts').

omega_variable(
    duopoly_pricing_power_post_settlement,
    'Does the judgment sharing agreement preserve Visa/Mastercard pricing power post-settlement, or do competitive dynamics from the judgment enable price correction?',
    'Longitudinal tracking of interchange fee levels, network pricing, and merchant switching costs pre/post settlement; analysis of new market entrant activity (digital wallets, fintech payment systems) in settlement period',
    'If pricing power preserved: duopoly has successfully extracted while appearing to submit to remedy (Snare with legal theater). If pricing power reduced: settlement represents genuine coordination outcome (Rope or Scaffold). Extractiveness rating depends on outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(duopoly_pricing_power_post_settlement, empirical, 'Whether settlement preserves or reduces duopoly pricing power').

omega_variable(
    judicial_enforcement_credibility,
    'Is the judgment sharing agreement enforced by courts with sufficient rigor to prevent defection and collusion reformation, or is judicial enforcement itself becoming a piton (performative ritual)?',
    'Longitudinal analysis of judicial enforcement records; rate of settlement modification requests; evidence of Visa/Mastercard collusive behavior post-judgment; merchant complaint rates and judicial response patterns',
    'If credible: enforcement is real coordination. If rituals: agreement becomes piton (false remedy theater). Theater ratio and classification shift accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_enforcement_credibility, empirical, 'Credibility of judicial enforcement of settlement terms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(s1_visa_judgment_sharing_agreement, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(visa_jsa_tr_t0, s1_visa_judgment_sharing_agreement, theater_ratio, 0, 0.35).
narrative_ontology:measurement(visa_jsa_tr_t5, s1_visa_judgment_sharing_agreement, theater_ratio, 5, 0.42).
narrative_ontology:measurement(visa_jsa_tr_t10, s1_visa_judgment_sharing_agreement, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(visa_jsa_be_t0, s1_visa_judgment_sharing_agreement, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(visa_jsa_be_t5, s1_visa_judgment_sharing_agreement, base_extractiveness, 5, 0.64).
narrative_ontology:measurement(visa_jsa_be_t10, s1_visa_judgment_sharing_agreement, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(s1_visa_judgment_sharing_agreement, enforcement_mechanism).
narrative_ontology:affects_constraint(s1_visa_judgment_sharing_agreement, visa_mastercard_interchange_extraction).
narrative_ontology:affects_constraint(s1_visa_judgment_sharing_agreement, payment_network_market_concentration).

% DUAL FORMULATION NOTE:
% The judgment sharing agreement decomposes into two structurally distinct constraints: (1) settlement distribution coordination (low ε, Rope) for administering prior claims; (2) duopoly pricing structure preservation (high ε, Snare) for merchant fee extraction. The tangled rope classification arises from their simultaneous operation within a single legal mechanism. The network links indicate that the judgment sharing agreement structurally preserves the upstream extraction constraint (visa_mastercard_interchange_extraction) by converting an existential threat into a bounded obligation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(s1_visa_judgment_sharing_agreement, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
