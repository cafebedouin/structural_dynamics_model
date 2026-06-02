% ============================================================================
% CONSTRAINT STORY: meta_pay_or_okay_model
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_meta_pay_or_okay_model, []).

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
 *   constraint_id: meta_pay_or_okay_model
 *   human_readable: Meta's "Pay or Okay" Data Monetization Model in the EU
 *   domain: technological/platform_governance
 *
 * SUMMARY:
 *   Meta's 'Pay or Okay' policy, introduced in response to EU GDPR and DMA
 *   enforcement, creates a binary choice for EU users: consent to personal
 *   data processing for targeted advertising, or pay a monthly subscription
 *   (€12-14 depending on platform) for ad-free access. This constraint
 *   exemplifies the mandatrophy problem: is it a fair market segmentation
 *   mechanism (Rope) solving the collective action problem of how to monetize
 *   platforms under GDPR restrictions, or is it coercive extraction (Snare)
 *   exploiting network effects to force users into an unequal bargain? The
 *   structural answer is Tangled Rope: it contains both genuine coordination
 *   (clarifying the data-for-access contract, solving Meta's revenue problem,
 *   segmenting high-intent users) and significant extraction (using network
 *   effects and switching costs to suppress the genuine alternatives). The
 *   constraint exhibits eight distinct classifications from different
 *   observer positions, revealing how the same institutional arrangement
 *   appears as natural law to some, fair coordination to others, and coercive
 *   extraction to those with the fewest options.
 *
 * KEY AGENTS:
 *   - EU Users: Primary victims (powerless/trapped) — face binary choice between privacy degradation and financial extraction; high switching costs due to network effects
 *   - Meta Shareholders: Primary beneficiaries (institutional/arbitrage) — capture revenue from either ad targeting or subscriptions; extensive exit options and strategic flexibility
 *   - Advertisers: Secondary beneficiaries (institutional/arbitrage) — access either high-fidelity targeting or high-intent user cohort; can allocate spend between strategies
 *   - Privacy Advocates / Civil Society: Secondary victim (moderate/constrained) — mobilize legal pressure but at resource cost; constrained by regulatory uncertainty about GDPR Article 7
 *   - EU Regulators (EDPB/DMA): Organized institutional actors (organized/constrained) — created the regulatory environment forcing this choice architecture; now must police whether it complies with the rules they made
 *   - Alternative Platforms (Mastodon, Bluesky, Matrix): Organized technical communities (organized/mobile) — building interoperability and federation to erode Meta's network effects over generational timescale
 *   - Consent Theater: Institutional performance (institutional/arbitrage) — the legal form of consent masks underlying coercion; maintained through regulatory ritual
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(meta_pay_or_okay_model, 0.58).
domain_priors:suppression_score(meta_pay_or_okay_model, 0.68).
domain_priors:theater_ratio(meta_pay_or_okay_model, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(meta_pay_or_okay_model, extractiveness, 0.58).
narrative_ontology:constraint_metric(meta_pay_or_okay_model, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(meta_pay_or_okay_model, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(meta_pay_or_okay_model, tangled_rope).
narrative_ontology:human_readable(meta_pay_or_okay_model, "Meta's \"Pay or Okay\" Data Monetization Model in the EU").
narrative_ontology:topic_domain(meta_pay_or_okay_model, "technological/platform_governance").

domain_priors:requires_active_enforcement(meta_pay_or_okay_model).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(meta_pay_or_okay_model, meta_shareholders).
narrative_ontology:constraint_beneficiary(meta_pay_or_okay_model, advertisers).
narrative_ontology:constraint_victim(meta_pay_or_okay_model, eu_users).
narrative_ontology:constraint_victim(meta_pay_or_okay_model, data_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EU USER (SNARE) — Structurally trapped. Meaningful social participation now requires either surrendering data autonomy or paying a premium (€14/month for ad-free). Exit is high-cost: alternatives (Mastodon, BlueSky, Signal) lack network effects; abandoning social connection imposes biographical costs. Experiences maximum extraction: forced choice between privacy degradation and financial extraction, with no genuine third option.
constraint_indexing:constraint_classification(meta_pay_or_okay_model, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PRIVACY ADVOCATE / CIVIL SOCIETY (TANGLED ROPE) — Constrained exit: advocacy networks can mobilize legal pressure (EDPB complaints, court challenges) but at resource cost. Experience both coordination benefit (the model creates clarity around data value) and extraction (the model's structure forces users into an unequal bargain). High suppression: regulatory uncertainty about whether GDPR prohibits this tradeoff prevents robust opposition.
constraint_indexing:constraint_classification(meta_pay_or_okay_model, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: META SHAREHOLDERS (ROPE) — Primary beneficiaries. Experiences the constraint as pure coordination: monetizing user attention through either ad targeting or subscription creates clear revenue stream and market segmentation. Arbitrage options abound: can shift geographic strategy, adjust pricing, or adjust the data consent granularity. The constraint solves the collective action problem of 'how do we extract value from social graph while navigating GDPR?'. Net benefit flows to this agent.
constraint_indexing:constraint_classification(meta_pay_or_okay_model, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ADVERTISERS (ROPE) — Secondary beneficiaries. Constraint solves their coordination problem: they can access either high-fidelity targeting (from consented users) or a population of paying users (proxy for higher intent/purchasing power). Arbitrage options: can allocate budget between targeted and non-targeted campaigns. Experience net benefit.
constraint_indexing:constraint_classification(meta_pay_or_okay_model, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: EU REGULATORS (TANGLED ROPE) — Organized institutional actors facing a structural constraint of their own making. The DMA and GDPR create the regulatory environment that forced Meta's choice architecture. Regulators now experience both coordination and extraction: coordination benefit (the model clarifies the data-for-access contract), extraction burden (they must now police whether the choice is genuinely free or coercive under GDPR Article 7). Constrained exit: they cannot easily unwind the regulatory structure without signaling inconsistency. Active enforcement burden is high.
constraint_indexing:constraint_classification(meta_pay_or_okay_model, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ALTERNATIVE PLATFORMS (SCAFFOLD) — Organized technical communities (Mastodon, Matrix, Bluesky) are building opt-out pathways through federation and protocol standardization. Sunset clause logic applies: if interoperability standards mature and user experience on federated platforms reaches parity, Meta's extraction mechanism loses value (network effects become less binding). Currently constrained by user coordination costs (switching friction) but mobile over generational timescale. Scaffold perspective reveals real technical substitutes in formation.
constraint_indexing:constraint_classification(meta_pay_or_okay_model, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 7: CONSENT THEATER (PITON) — The legal form of 'consent' in the opt-in/pay-or-okay choice is substantially performative. The choice architecture itself violates Article 7 GDPR's prohibition on conditional access (bundling service with unwanted data processing). The 'consent' is merely the surface ritual; the underlying extraction mechanism (forced choice) is what is operationally functional. Theater ratio high (0.45 for this constraint reflects that the regulatory ritual of 'consent' masks coercion). This perspective shows that the formal legal compliance framework is degraded institutional theater — maintained because alternatives haven't fully replaced it, but operationally dysfunctional.
constraint_indexing:constraint_classification(meta_pay_or_okay_model, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL / NATURAL LAW VIEW (MOUNTAIN) — Risk of naturalization: from a civilizational view, network effects create an apparent immutable constraint: 'Whoever controls the largest social graph can extract value via data or subscription.' This perspective treats network concentration as a law of nature rather than a contingent institutional arrangement. However, the structural data contradicts this: network effects are policy-contingent (regulatory choices determine whether network switching costs are high or low), not natural laws. The mountain classification is a false summit revealing naturalization bias.
constraint_indexing:constraint_classification(meta_pay_or_okay_model, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(meta_pay_or_okay_model_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(meta_pay_or_okay_model, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(meta_pay_or_okay_model, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(meta_pay_or_okay_model, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(meta_pay_or_okay_model, TR),
    TR >= 0.70.

:- end_tests(meta_pay_or_okay_model_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, justified by the structural asymmetry. The constraint forces users into an unequal bargain where refusal of data processing requires paying a premium (€14/month ≈ 168/year), while the monetization of consent-derived data generates substantially higher value to Meta per user. However, 0.58 rather than 0.80+ because some users have genuine exit options (switching to alternative platforms, accepting targeted ads as cost of service), and Meta has legitimately solved a real coordination problem (how to monetize under GDPR). The value increased from 0.42 to 0.58 over the interval as EDPB regulatory pressure mounted and the pay option became the only viable path to maintain consent-based targeting — i.e., the structural coercion increased as regulatory constraints tightened. Suppression (0.68): High. Switching costs are severe: Mastodon lacks network effects, Signal is messaging-only, alternative platforms have poor discoverability. The €14/month price creates financial friction. GDPR Article 7 legal uncertainty suppresses robust regulatory challenge. Theater ratio (0.45): Moderate. The choice architecture is relatively transparent (users can see both options clearly), but the underlying consent ritual is performative — 'consent' obtained under coercive choice architecture is theater masking forced selection.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full mandatrophy problem. Meta shareholders and advertisers see Rope: a clean coordination mechanism that clarifies the data-for-access contract and enables market segmentation. EU users see Snare: forced choice between privacy loss and financial extraction, with no genuine third option. Regulators see Tangled Rope: they created this dynamic through DMA/GDPR rules, and now must enforce whether it violates the very rules they wrote. Privacy advocates see mixed signals: there is real coordination benefit (clarity about data use) but overwhelming extraction. Alternative platforms see Scaffold: the constraint's extraction mechanism will decay if interoperability standards mature and switching costs fall. The analytical observer risks seeing Mountain (network effects as inevitable law) when the structural analysis reveals a contingent institutional arrangement (network concentration is policy-enforced, not natural).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from each agent's structural relationship. Meta shareholders: beneficiaries + arbitrage exit → low d (0.15) → negative f(d) → they experience the constraint as enabling, not extractive. EU users: victims + trapped exit → high d (0.95) → high f(d) ≈ 1.42 → they experience maximum extraction. Privacy advocates: victims + constrained exit (they can mobilize pressure but at cost) → moderate d (0.60) → moderate f(d) → mixed experience. Regulators: complex — they are institutional beneficiaries (control over platform behavior) but also constrained victims (they cannot easily unwind what they created) → d ≈ 0.50-0.55 → moderate extraction. This explains why the EU regulatory perspective is Tangled Rope rather than Rope or Snare: regulators created the constraint, benefit from its compliance function, but experience significant burden in enforcing it. Alternative platforms have mobile exit options (users can switch if platforms reach feature parity) → d ≈ 0.45 → Scaffold classification.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by recognizing that all eight classifications are simultaneously valid from their respective structural positions. The critical insight is that Meta's choice architecture is not 'is it extraction or coordination?' but 'to whom does it transfer value?' To Meta shareholders and advertisers: it is Rope (coordination). To EU users: it is Snare (extraction). To regulators: it is Tangled Rope (both enforcement and burden). To alternative platforms: it is Scaffold (temporary, with a sunset as interoperability matures). The mandatrophy is resolved by refusing to collapse the presheaf into a single type and instead recognizing that the classification presheaf accurately captures a structurally heterogeneous constraint. The analytical observer's temptation to see Mountain (network effects as natural law) is revealed as naturalization bias — network concentration is policy-contingent, not inevitable. The constraint's claim to being 'fair' (Rope-like) rests on the false premise that users have genuine alternatives; in fact, network effects and switching costs (which are policy-reinforced through platform aggregation and data portability limits) suppress alternatives and make the choice coercive (Snare-like). Tangled Rope is the honest classification: real coordination function (solving Meta's GDPR compliance problem) + significant asymmetric extraction (using market power to shift value toward the platform).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gdpr_article_7_violation_scope,
    'Does bundling service access with unwanted data processing constitute a per se GDPR Article 7 violation, or does the subscription option create sufficient ''genuineness of consent'' under the ICO/EDPB interpretation?',
    'EDPB preliminary ruling on whether pay-or-okay satisfies Article 7 conditions; court interpretation of ''freely given'' consent when refusal is economically coercive',
    'If per se violation: constraint is regulatory violation requiring dismantling (Snare reclassifies to illegal extraction). If permitted: constraint becomes legitimate business model with regulatory sanction (Tangled Rope confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gdpr_article_7_violation_scope, conceptual, 'Whether pay-or-okay violates GDPR Article 7 freely-given consent requirement').

omega_variable(
    dma_interoperability_effectiveness,
    'Will DMA-mandated interoperability (APIs for rival social networks to access Meta''s social graph) effectively create exit options for users, or will data portability remain insufficient to overcome network effects?',
    'Empirical tracking of migration patterns post-DMA compliance; measurement of whether rival platforms reaching feature parity with Meta see sustained user switching',
    'If effective interop: scaffold sunset becomes real — users gain mobile exit options, suppression drops, extraction mechanism loses force. If insufficient: network effects remain binding, constrained/trapped classifications persist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dma_interoperability_effectiveness, empirical, 'Whether DMA interoperability mandates create effective user exit options').

omega_variable(
    subscription_price_adequacy,
    'Does Meta''s €14/month pricing reflect the true economic value of personal data being relinquished, or is it artificially depressed to reduce uptake and maintain consent-based monetization?',
    'Comparison of €14/month fee to: (1) advertising revenue per user in ad-targeted cohort, (2) shadow pricing of data via identity theft insurance, (3) willingness-to-pay studies. Analysis of Meta''s pricing strategy and user migration patterns.',
    'If underpriced: constraint is extractive (victims are EU users subsidizing ad-targeted cohort). If fair-priced: constraint is legitimate market segmentation (Rope reclassifies). If overpriced: suppression metric understated — few users can afford exit via subscription.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subscription_price_adequacy, empirical, 'Whether subscription pricing reflects true data value or artificial suppression of exit').

omega_variable(
    consent_quality_measurement,
    'What fraction of users selecting ''Okay'' to data processing actually understood the implications, versus clicking through choice architecture under time pressure or satisficing bias?',
    'Behavioral experiments on choice architecture friction (time to click through, number of screens, clarity of data use statements); post-hoc surveys of user understanding; analysis of Meta''s AB testing data on consent rates at different friction levels',
    'If < 40% genuine understanding: suppression is underestimated (consent is theater, not genuine choice). If > 70% understanding: consent is more genuine (though still coercive). Affects theater_ratio calibration.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_quality_measurement, empirical, 'Actual user comprehension of data processing implications').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(meta_pay_or_okay_model, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(metapay_tr_t0, meta_pay_or_okay_model, theater_ratio, 0, 0.38).
narrative_ontology:measurement(metapay_tr_t1, meta_pay_or_okay_model, theater_ratio, 1, 0.41).
narrative_ontology:measurement(metapay_tr_t2, meta_pay_or_okay_model, theater_ratio, 2, 0.45).

% Extraction over time
narrative_ontology:measurement(metapay_be_t0, meta_pay_or_okay_model, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(metapay_be_t1, meta_pay_or_okay_model, base_extractiveness, 1, 0.5).
narrative_ontology:measurement(metapay_be_t2, meta_pay_or_okay_model, base_extractiveness, 2, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(meta_pay_or_okay_model, resource_allocation).
narrative_ontology:affects_constraint(meta_pay_or_okay_model, gdpr_data_protection_as_competitive_moat).
narrative_ontology:affects_constraint(meta_pay_or_okay_model, network_effects_market_concentration).
narrative_ontology:affects_constraint(meta_pay_or_okay_model, dma_platform_interoperability_mandate).

% DUAL FORMULATION NOTE:
% The pay-or-okay model can be decomposed into two structurally distinct constraints: (1) regulatory compliance coordination (extractiveness ≈ 0.25, Mountain: GDPR's data minimization requirement creates an immutable coordination problem for platforms), and (2) market extraction via choice architecture (extractiveness ≈ 0.58, Tangled Rope: Meta's bundling of service with data processing exploits network effects to extract value). These are linked: the regulatory constraint creates the context that makes the extraction mechanism possible. The network edges capture these dependencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(meta_pay_or_okay_model, institutional, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
