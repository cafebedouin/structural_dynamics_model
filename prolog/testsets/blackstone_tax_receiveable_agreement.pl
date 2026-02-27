% ============================================================================
% CONSTRAINT STORY: blackstone_tax_receiveable_agreement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_blackstone_tax_receiveable_agreement, []).

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
 *   constraint_id: blackstone_tax_receiveable_agreement
 *   human_readable: Blackstone Tax Receivable Agreement (TRA)
 *   domain: economic/financial_engineering
 *
 * SUMMARY:
 *   Blackstone's Tax Receivable Agreement (TRA) represents a hybrid
 *   coordination-extraction mechanism embedded in the firm's 2007 IPO
 *   structure. When Blackstone acquired interests from existing partnership
 *   investors to create a publicly tradable equity structure, it committed to
 *   paying 85% of the resulting tax benefits directly to those selling
 *   partners. This constraint exhibits the full diagnostic range of indexical
 *   classification: from the selling partners' perspective, it is a fair
 *   coordination mechanism pricing their equity exit; from public
 *   shareholders' perspective, it is a permanent extraction mechanism that
 *   depresses equity value and locks capital allocation decisions for
 *   decades. The constraint's theater ratio has increased over time—the
 *   original 'fair compensation for founders' rationale has been
 *   progressively replaced by secondary market trading, catch-up provisions,
 *   and valuation disputes that serve primarily to maintain the extraction
 *   flow after the original coordination problem (founder exit) is exhausted.
 *   The suppression is structural: public shareholders cannot easily
 *   understand the TRA's cash flow impact (it is buried in tax footnotes and
 *   actuarial calculations), cannot exit without realizing the depressed
 *   price, and cannot renegotiate the terms. The constraint is active,
 *   enforceable, and has generated billions in payments to pre-IPO partners.
 *
 * KEY AGENTS:
 *   - Selling Partners (Pre-IPO Investors): Primary beneficiaries (organized/arbitrage) — receive 85% of tax savings from their interest sales; negotiated exit compensation
 *   - Blackstone Management Company: Secondary beneficiary and coordinator (institutional/arbitrage) — solves the equity consolidation problem without requiring partner liquidations; captures residual 15% of tax benefit
 *   - Public Shareholders: Primary victims (powerless/trapped) — bear the TRA liability in perpetuity; cannot exit without realizing depressed price; no visibility into true cost
 *   - Future Equity Holders: Secondary victims (powerless/trapped) — purchase equity with embedded TRA liability; face permanent cash flow claims from pre-IPO beneficiaries
 *   - Tax Equity Investors: Tertiary beneficiaries (powerful/mobile) — arbitrage the secondary TRA market and capture pricing gaps between actuarial value and market prices
 *   - Tax Regulatory Authorities: Oversight actor (organized/constrained) — perceive TRA as a temporary coordination mechanism subject to legislative reform; hold implicit sunset power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(blackstone_tax_receiveable_agreement, 0.38).
domain_priors:suppression_score(blackstone_tax_receiveable_agreement, 0.65).
domain_priors:theater_ratio(blackstone_tax_receiveable_agreement, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(blackstone_tax_receiveable_agreement, extractiveness, 0.38).
narrative_ontology:constraint_metric(blackstone_tax_receiveable_agreement, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(blackstone_tax_receiveable_agreement, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(blackstone_tax_receiveable_agreement, tangled_rope).
narrative_ontology:human_readable(blackstone_tax_receiveable_agreement, "Blackstone Tax Receivable Agreement (TRA)").
narrative_ontology:topic_domain(blackstone_tax_receiveable_agreement, "economic/financial_engineering").

domain_priors:requires_active_enforcement(blackstone_tax_receiveable_agreement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(blackstone_tax_receiveable_agreement, selling_partners).
narrative_ontology:constraint_beneficiary(blackstone_tax_receiveable_agreement, blackstone_management).
narrative_ontology:constraint_victim(blackstone_tax_receiveable_agreement, blackstone_public_shareholders).
narrative_ontology:constraint_victim(blackstone_tax_receiveable_agreement, future_equity_holders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PUBLIC SHAREHOLDER (SNARE) — Powerless and trapped. The TRA obligation extracts from Blackstone's future cash flows without shareholder consent or visibility into the true cost. Shareholders cannot exit: selling their stake realizes the depressed price resulting from the TRA liability. They bear the extraction in perpetuity or until the agreement expires. Maximum experienced extraction with no exit option.
constraint_indexing:constraint_classification(blackstone_tax_receiveable_agreement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SELLING PARTNERS (TANGLED ROPE) — Organized agents with significant negotiating power. They benefit from the 85% tax receivable pass-through (coordination function: they are compensated for their pre-IPO ownership). But they also constrain the firm's capital allocation for decades (extraction mechanism: future shareholders subsidize their exit). The constraint has both a genuine coordination component (pricing their equity stake) and an asymmetric extraction component (locking in permanent tax obligations). Active enforcement required: the TRA is contractually binding and indexed to tax code sections.
constraint_indexing:constraint_classification(blackstone_tax_receiveable_agreement, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BLACKSTONE MANAGEMENT COMPANY (ROPE) — Institutional actor with arbitrage exit (can liquidate or restructure). Management benefits from TRA as a coordination mechanism: it enables acquisition of partner interests without requiring them to sell at discount or exit immediately. The TRA pricing is negotiated (not coercive). From management's perspective during the IPO, the TRA is a pure coordination solution: we want to buy partners out, they want fair compensation, TRA bridges that gap. Management experiences this as rope: solving the collective action problem of equity consolidation with minimal coercion.
constraint_indexing:constraint_classification(blackstone_tax_receiveable_agreement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TAX EQUITY MARKETS (TANGLED ROPE) — Powerful institutional investors and financial engineers who understand TRA mechanics and can arbitrage tax benefit asymmetries. They benefit from TRA as a coordination instrument (it creates a reliable tax asset class they can value and trade). But they also extract from future shareholders through secondary market pricing that capitalizes the tax obligation without fully passing the cost back. The TRA enables their arbitrage (mobile exit) while constraining the underlying asset. Moderate extraction because the ecosystem has agency and visibility; coordination because TRA creates a new asset class.
constraint_indexing:constraint_classification(blackstone_tax_receiveable_agreement, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: TAX REGULATORY OVERSIGHT (SCAFFOLD) — Organized government actors (Treasury, IRS, Congress) who see TRA as a temporary coordination mechanism with an implicit sunset: tax code changes, carried interest reform, or partnership liquidation will eventually unwind the 85/15 split. Theater ratio is low here because regulators perceive TRA as a structural arbitrage they can address through legislative action. The constraint has a sunset horizon (not indefinite). Suppression is moderate because regulators have enforcement mechanisms (tax code redefinition) but face coordination costs (legislative gridlock). The scaffold perspective suggests that as tax reform advances, the TRA obligation will diminish in force.
constraint_indexing:constraint_classification(blackstone_tax_receiveable_agreement, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: FINANCIAL ANALYST / THEORETICAL VIEW (PITON) — From a deep analytical perspective, TRA classifies as degraded coordination. It was created to solve a legitimate problem (how to price founder equity in a transformation from partnership to public company) but has become a theater: the original coordination rationale (founders exiting fairly) is exhausted within years, yet the TRA obligation persists for decades with updated valuations, catch-up provisions, and secondary exchanges. The mechanism has lost its primary function but remains enforced through institutional inertia. Analysts see the TRA as increasingly performative — the 'fair compensation' narrative persists while the actual redistribution effect dominates. Theater ratio 0.42 reflects this partial degradation: TRA still has real coordination function at inception, but becomes increasingly theatrical after the first generation of beneficiaries exits.
constraint_indexing:constraint_classification(blackstone_tax_receiveable_agreement, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(blackstone_tax_receiveable_agreement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(blackstone_tax_receiveable_agreement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(blackstone_tax_receiveable_agreement, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(blackstone_tax_receiveable_agreement, TR),
    TR >= 0.70.

:- end_tests(blackstone_tax_receiveable_agreement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The TRA creates a genuine value transfer from public shareholders to selling partners, but it is not as severe as pure extraction (snare ε ≥ 0.46) because: (1) the original coordination rationale is legitimate (founders needed an exit mechanism), (2) the tax benefit is real (not phantom), and (3) the amount (85%) is contractually specified (not arbitrary coercion). However, extractiveness has increased from 0.22 at inception to 0.38 currently as the original coordination rationale has been exhausted and the mechanism has become primarily extractive in character. Suppression (0.65): Moderate-high. Multiple barriers exist: (a) informational suppression — most investors do not understand TRA mechanics or cannot quantify the cash flow impact from SEC disclosures; (b) exit suppression — selling shares realizes the depressed price created by the TRA liability; (c) renegotiation suppression — the agreement is contractual and not subject to shareholder votes; (d) distributed suppression — the impact is spread across millions of shareholders making coordination costly. Theater ratio (0.42): Moderate-low. At inception, the TRA had high functional content (it solved a real problem). Currently, it still has functional content (partners continue exiting, tax benefits continue accruing) but increasingly includes theatrical elements: secondary market valuations are disputed, catch-up provisions require actuarial revaluation, and the 'fair compensation' narrative persists despite the mechanism's primary function being exhausted. The theater ratio has grown from 0.15 to 0.42 over the measurement interval.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a stark perspectival divergence between the beneficiary and victim perspectives. The selling partners (organized/arbitrage) experience the TRA as rope—a coordination mechanism that fairly prices their exit and compensates them for their pre-IPO risk. They negotiated the terms, they understand the mechanics, and they have agency (they can refuse the deal or demand different terms). Blackstone management experiences it as rope: the TRA enables equity consolidation without forcing partners to liquidate at discounts. Public shareholders (powerless/trapped) experience the TRA as snare: they inherit an obligation they did not create, cannot understand fully, cannot exit, and cannot renegotiate. The gap reflects the fundamental asymmetry: founders experience the constraint as fair compensation; later shareholders experience it as permanent extraction. The analytical perspective from a financial engineering ecosystem sees it as tangled rope—a coordination tool that has become an arbitrage opportunity, benefiting those with information advantage and mobile capital. The regulatory perspective sees it as scaffold—a temporary arrangement subject to legislative and regulatory change. The gap between 'fair founder compensation' (selling partners' view) and 'permanent shareholder extraction' (public shareholder view) is the core diagnostic feature.
 *
 * DIRECTIONALITY LOGIC:
 *   The TRA's directionality differs dramatically by agent. Selling partners have d ≈ 0.20 (beneficiary with exit options—they negotiated the deal and can walk away if unsatisfied; they have arbitrage: they can restructure or avoid the TRA if terms are unfavorable). Their f(d) ≈ 0.15, producing low χ—they experience the constraint as beneficial. Public shareholders have d ≈ 0.85 (victims with minimal exit—they inherit the liability, cannot negotiate, and realizing their shares at a discount is their only exit). Their f(d) ≈ 1.15, producing high χ—they experience maximum extraction. The regulatory perspective has d ≈ 0.55 (symmetric, with constrained exit—they can change the tax code but face political and coordination costs). Their f(d) ≈ 0.75, producing moderate χ. The derivation chain runs from the structural positions (who negotiated, who inherited, who regulates) through the exit options (arbitrage for negotiators, trapped for inheritors, constrained for regulators) to d values that reflect true structural relationships, not nominal power.
 *
 * MANDATROPHY ANALYSIS:
 *   The TRA resolves the mandatrophy by disambiguating between legitimate coordination and rent extraction. The temptation is to classify the TRA as pure rope (founders fair compensation, problem solved) or pure snare (shareholder extraction, rent-seeking). The mandatrophy test asks: does the constraint have BOTH a genuine coordination function AND asymmetric extraction? YES: (1) Coordination function: the TRA enables founders to exit without forcing them to liquidate at discounts, solving a real collective action problem. Blackstone management also coordinates founder interests with public capital raising. (2) Asymmetric extraction: the TRA extracts from public shareholders who inherit the obligation without choice or consent. The benefit flows to a specific class (selling partners) in perpetuity, not to the system as a whole. The asymmetry is baked in—founders captured upside during negotiation, later shareholders inherit downside in perpetuity. The classification is tangled rope because BOTH elements are present: genuine coordination at inception (founders exit fairly) AND asymmetric extraction at steady state (public shareholders permanently subsidize founder returns). Piton analysis: The theater ratio (0.42) reflects the constraint's degradation. At inception, theater was low (0.15) because the mechanism had clear functional content. At time 10, theater has increased (0.42) because the original function (founder exit) is exhausted, yet the mechanism persists through secondary markets, catch-up provisions, and valuation disputes. This creeping theater is the signature of mandatrophy in progress—a coordination mechanism converting to extraction theater as its primary function atrophies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tax_code_permanence,
    'Will carried interest preferential tax treatment survive legislative reform, or will TRA obligations become claims on ordinary income?',
    'Tracking of legislative proposals for carried interest taxation; monitoring of Treasury guidance on partnership-to-public entity transitions; analysis of comparable TRA provisions under alternative tax regimes',
    'If carried interest preserved: TRA remains high-extraction (ε stays 0.38+). If carried interest eliminated: TRA obligations collapse to ordinary income rates, shareholder extraction decreases significantly (ε drops to 0.15-0.20), constraint reclassifies as scaffold with hard sunset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tax_code_permanence, empirical, 'Permanence of carried interest tax treatment and impact on TRA enforceability').

omega_variable(
    future_partner_acquisitions,
    'Will Blackstone continue acquiring partner interests at rates that sustain the TRA tax benefit generation, or will acquisition rates decline?',
    'Historical analysis of partner buyback rates relative to headcount growth; projection models of when most profitable partners will have fully exited; analysis of whether new partners avoid equity consolidation to preserve their own eventual TRA benefits',
    'If acquisitions continue: TRA obligations persist as permanent extraction mechanism. If acquisitions decline: TRA becomes a wasting asset (beneficiaries get paid but new generations don''t), shifting it toward scaffold dynamics (sunset from exhaustion rather than legislation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(future_partner_acquisitions, empirical, 'Sustainability of partner interest acquisition rates needed to sustain TRA tax benefits').

omega_variable(
    secondary_market_pricing_opacity,
    'Do secondary market prices for TRA interests fully reflect the underlying tax obligation to public shareholders, or do they systematically underprice the future liability?',
    'Comparison of secondary market TRA valuations with actuarial models of future tax liability; analysis of whether secondary purchasers experience value capture in excess of their acquisition cost; tracking of realized vs expected tax savings after secondary transfers',
    'If fully priced: market discovery functions correctly, public shareholders adjust valuations rationally (no hidden extraction). If underpriced: secondary market participants extract from public shareholders through information asymmetry (extraction ε increases to 0.50+, snare classification for analysts).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secondary_market_pricing_opacity, empirical, 'Whether secondary TRA market prices fully reflect future tax liabilities').

omega_variable(
    regulatory_classification_drift,
    'Will tax authorities ever challenge the TRA as a disguised partnership distribution rather than a legitimate tax provision realization?',
    'Monitoring of IRS guidance, Tax Court cases, and Treasury regulations on TRA treatment; analysis of whether comparable provisions in other structured acquisitions receive similar treatment; tracking of audit activity on TRA filers',
    'If challenged successfully: TRA obligations become unenforceable (constraint collapses to rope or dissolves entirely). If upheld: TRA remains enforceable as a snare for public shareholders (extraction ε stabilizes at 0.38+).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_classification_drift, empirical, 'Whether tax authorities will challenge TRA as mislabeled distribution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(blackstone_tax_receiveable_agreement, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bstra_tr_t0, blackstone_tax_receiveable_agreement, theater_ratio, 0, 0.15).
narrative_ontology:measurement(bstra_tr_t5, blackstone_tax_receiveable_agreement, theater_ratio, 5, 0.28).
narrative_ontology:measurement(bstra_tr_t10, blackstone_tax_receiveable_agreement, theater_ratio, 10, 0.42).

% Extraction over time
narrative_ontology:measurement(bstra_be_t0, blackstone_tax_receiveable_agreement, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(bstra_be_t5, blackstone_tax_receiveable_agreement, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(bstra_be_t10, blackstone_tax_receiveable_agreement, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(blackstone_tax_receiveable_agreement, resource_allocation).
narrative_ontology:affects_constraint(blackstone_tax_receiveable_agreement, carried_interest_tax_preference).
narrative_ontology:affects_constraint(blackstone_tax_receiveable_agreement, partnership_to_corporation_arbitrage).
narrative_ontology:affects_constraint(blackstone_tax_receiveable_agreement, private_equity_fee_extraction).

% DUAL FORMULATION NOTE:
% The TRA is downstream of two structural constraints: (1) carried interest tax preference (ε ≈ 0.15, rope—tax code structure enabling partnership income to qualify for capital gains treatment), and (2) partnership-to-corporation arbitrage (ε ≈ 0.35, tangled rope—asymmetric benefit capture during IPO structure change). The TRA operationalizes the exploitation of both upstream constraints. As a family: carried_interest_tax_preference → partnership_to_corporation_arbitrage → blackstone_tra. Each constraint has a distinct ε and structural data; together they form a constraint family showing how tax code features enable corporate structure arbitrage which enables individual TRA extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(blackstone_tax_receiveable_agreement, analytical, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
