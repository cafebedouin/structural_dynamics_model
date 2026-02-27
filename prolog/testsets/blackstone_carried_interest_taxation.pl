% ============================================================================
% CONSTRAINT STORY: blackstone_carried_interest_taxation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_blackstone_carried_interest_taxation, []).

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
 *   constraint_id: blackstone_carried_interest_taxation
 *   human_readable: Carried Interest Partnership Taxation
 *   domain: economic/political
 *
 * SUMMARY:
 *   Carried interest taxation in the United States exemplifies a hybrid
 *   constraint combining genuine coordination function with asymmetric
 *   extraction. The preferential treatment of carried interest — structuring
 *   performance fees for private equity and hedge fund managers as long-term
 *   capital gains rather than ordinary income — creates alignment between
 *   fund managers and investors (genuine coordination) while simultaneously
 *   extracting substantial tax value from the ordinary income taxpayer base
 *   (asymmetric extraction). The constraint's extractiveness has increased
 *   over the measurement interval as PE/HF assets under management have grown
 *   and regulatory scrutiny has heightened, driving both increased lobbying
 *   effort (theater) and accumulating pressure for reform. The constraint
 *   exhibits all major perspectival patterns: powerless wage earners see pure
 *   extraction (Snare), institutional beneficiaries see coordination (Rope),
 *   organized reform coalitions see a temporary arrangement with sunset logic
 *   (Scaffold), and the tax distinction itself shows signs of institutional
 *   degradation (Piton), with the original rationale (encouraging capital
 *   investment by those who own capital) no longer fitting the empirical
 *   reality (performance fees by managers with minimal principal at risk).
 *
 * KEY AGENTS:
 *   - Private Equity Partners: Institutional beneficiaries (institutional/arbitrage) — receive preferential capital gains treatment on carried interest, capturing significant tax value relative to equivalent wage compensation
 *   - Hedge Fund Managers: Institutional beneficiaries (institutional/arbitrage) — similar preferential treatment, with slightly lower coordination justification than PE given less explicit performance-fee structure
 *   - Ordinary Income Taxpayers: Powerless victims (powerless/trapped) — bear the relative cost of preferential treatment through higher effective tax burden on wage income; cannot restructure compensation to benefit from capital gains rates
 *   - Federal Treasury / Tax Base: Organized victim (organized/constrained) — experiences direct revenue loss from preferential treatment; constrained exit (cannot eliminate provision without political cost and capital flight risk)
 *   - Congressional Reform Coalition: Organized challengers (organized/constrained) — multiple reform efforts (2010-2015, 2017-2024) seek to close loophole; constrained by lobbying pressure and divided political alignment
 *   - Limited Partners / Fund Investors: Secondary beneficiaries (institutional/arbitrage) — benefit from GP incentive alignment; prefer carried interest structure as motivational tool, though indifferent to tax treatment per se
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees both the genuine coordination function (incentive alignment) and the contingent extraction mechanism (statutory tax classification)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(blackstone_carried_interest_taxation, 0.58).
domain_priors:suppression_score(blackstone_carried_interest_taxation, 0.68).
domain_priors:theater_ratio(blackstone_carried_interest_taxation, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(blackstone_carried_interest_taxation, extractiveness, 0.58).
narrative_ontology:constraint_metric(blackstone_carried_interest_taxation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(blackstone_carried_interest_taxation, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(blackstone_carried_interest_taxation, tangled_rope).
narrative_ontology:human_readable(blackstone_carried_interest_taxation, "Carried Interest Partnership Taxation").
narrative_ontology:topic_domain(blackstone_carried_interest_taxation, "economic/political").

domain_priors:requires_active_enforcement(blackstone_carried_interest_taxation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(blackstone_carried_interest_taxation, private_equity_partners).
narrative_ontology:constraint_beneficiary(blackstone_carried_interest_taxation, hedge_fund_managers).
narrative_ontology:constraint_beneficiary(blackstone_carried_interest_taxation, asset_management_firms).
narrative_ontology:constraint_victim(blackstone_carried_interest_taxation, wage_earners).
narrative_ontology:constraint_victim(blackstone_carried_interest_taxation, ordinary_income_taxpayers).
narrative_ontology:constraint_victim(blackstone_carried_interest_taxation, public_treasury).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORDINARY INCOME TAXPAYER (SNARE) — Wage earners and middle-class professionals pay ordinary income tax on employment compensation while capital gains from carried interest are taxed at lower rates. These taxpayers cannot exit the wage-based income system and bear the full extraction asymmetry. Maximum suppression: no alternative tax treatment available, and the regulatory structure actively maintains this disparity through statutory language and IRS guidance.
constraint_indexing:constraint_classification(blackstone_carried_interest_taxation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRIVATE EQUITY PARTNERSHIP (ROPE) — General partners structure carried interest as a coordination mechanism: the performance fee aligns fund managers with investor returns and incentivizes capital deployment. From the partner perspective, the tax treatment enables this coordination by making the incentive structure competitive with wage-based compensation. Exit option: arbitrage — the GP can structure deals differently or relocate capital if tax treatment changes. Experiences the constraint as pure coordination with net benefit.
constraint_indexing:constraint_classification(blackstone_carried_interest_taxation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: FUND MANAGER (TANGLED ROPE) — Individual managers benefit from the capital gains treatment (coordination function) but also experience extraction pressure from limited partners and investors who demand higher performance fees, and from regulatory scrutiny that threatens the favorable treatment. Mobile exit options: managers can switch funds, relocate internationally, or negotiate different compensation structures. Experiences both coordination and asymmetric extraction depending on regulatory stability and investor demands.
constraint_indexing:constraint_classification(blackstone_carried_interest_taxation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: PUBLIC TREASURY (TANGLED ROPE) — The tax base experiences extraction: federal revenues are reduced by the preferential treatment of carried interest relative to ordinary income taxation. However, the treasury also has a coordination function — the capital gains rate structure exists to encourage capital formation and investment. Treasury is organized (Congress, IRS, Treasury Department) but constrained in exit options: cannot simply eliminate the preferential rate without political cost and capital flight. Effective extraction moderated by the coordination rationale but real in magnitude.
constraint_indexing:constraint_classification(blackstone_carried_interest_taxation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORM COALITION (SCAFFOLD) — Multiple reform efforts (Obama administration 2010-2015, Democratic tax proposals 2017-2024) sought to close or restrict the carried interest loophole. Each reform proposal has sunset language or phased implementation. These organized actors (Congressional Democrats, progressive tax organizations) see the preferential treatment as a temporary tax benefit whose justification has eroded as PE/HF assets have grown. The constraint is temporary: political momentum for reform persists, suggesting the extraction mechanism will eventually be modified. Theater relatively low for reform proposals — direct legislative language rather than performative review.
constraint_indexing:constraint_classification(blackstone_carried_interest_taxation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CAPITAL GAINS DISTINCTION (PITON) — The underlying tax principle — that long-term capital gains deserve preferential treatment to encourage investment — is increasingly performative. Carried interest is not capital at risk in the traditional sense; managers have cashed-out profits with no principal investment. The tax distinction persists through institutional inertia and lobbying, not because the original rationale (encouraging capital formation by those who own assets) applies to performance fees. Theater high (0.65): significant effort spent defending the distinction through technical legal language rather than defending the underlying economic logic.
constraint_indexing:constraint_classification(blackstone_carried_interest_taxation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / ATTEMPTED NATURAL LAW (MOUNTAIN) — One might frame the preferential capital gains treatment as a natural law of efficient tax policy — that capital income must be taxed differently from labor income to avoid double taxation and encourage capital formation. However, the structural data contradicts this: carried interest is neither owned capital nor systematically at risk; the extraction is contingent on statutory language (Section 1231 partnership treatment) and regulatory interpretation, not on immutable economic principles. The engine will detect this as a false summit — naturalizing a contingent institutional arrangement as invariant.
constraint_indexing:constraint_classification(blackstone_carried_interest_taxation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(blackstone_carried_interest_taxation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(blackstone_carried_interest_taxation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(blackstone_carried_interest_taxation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(blackstone_carried_interest_taxation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(blackstone_carried_interest_taxation, TR),
    TR >= 0.70.

:- end_tests(blackstone_carried_interest_taxation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, growing over interval. The preferential treatment transfers measurable tax value from ordinary taxpayers to fund managers. Treasury revenue loss estimates range from $10-50B annually depending on methodology. The extractiveness has increased as PE/HF sectors have grown — the same rate structure applied to a much larger asset base generates proportionally larger extraction. Not maximal (0.85+) because the coordination function (incentive alignment) is genuine: absent some performance fee mechanism, fund managers and LPs would face principal-agent misalignment. The constraint achieves real coordination value alongside extraction. Suppression (0.68): Moderate-high. The suppression mechanisms include: (1) technical legal language making the distinction opaque to ordinary taxpayers, (2) regulatory interpretation (IRS Section 1231 partnership rules) rather than explicit statutory carve-out, (3) effective lobbying preventing reform, (4) political fragmentation (some Democrats support carry-friendly policies; some Republicans oppose 'loopholes'). Suppression is not total — reform movements periodically surface, media attention spikes, and the distinction is publicly debatable. Theater ratio (0.65): Moderate-high and growing. Significant effort spent defending the distinction through technical arguments about capital formation incentives and double taxation, when the primary effect is straightforward tax rate arbitrage. As PE/HF sectors have grown, defensive rhetoric has intensified without corresponding economic rationale strengthening. Theater has increased from 0.40 to 0.65 as the original justification (encouraging capital formation) has become increasingly tenuous relative to actual behavior (massive PE/HF asset accumulation regardless of tax treatment).
 *
 * PERSPECTIVAL GAP:
 *   The gap between beneficiary (Rope) and victim (Snare) perspectives is fundamental: the same constraint solves an alignment problem for one agent class while creating arbitrary tax disparities for another. No single agent experiences the full constraint symmetrically. This asymmetry is the signature of Tangled Rope — genuine coordination function alongside genuine extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality depends critically on structural position. Beneficiaries (private equity partners, hedge fund managers) have high exit mobility: they can restructure compensation, relocate to low-tax jurisdictions, or negotiate different deal terms. This high-mobility exit (arbitrage) produces low directionality (d ≈ 0.10). Victims (wage earners, treasury) have constrained or trapped exit: wage earners cannot restructure employment into capital income; the treasury cannot simply eliminate the provision without political and economic consequences. This produces high directionality (d ≈ 0.85 for wage earners, d ≈ 0.60 for treasury). The power atoms differ sharply: beneficiaries are institutional; victims are powerless (wage earners) or organized but constrained (treasury). The effective extraction χ experienced differs by orders of magnitude across perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY STATUS: UNRESOLVED — The constraint contains genuine ambiguity about whether it is best classified as Tangled Rope (hybrid coordination/extraction) or Snare (pure extraction). The mandatrophy resolution requires determining whether the coordination function (GP-LP incentive alignment) is integral to the constraint or merely a post-hoc justification for an extraction mechanism that would persist regardless. Evidence supporting Tangled Rope: (1) carried interest does align manager and investor interests materially; (2) without some performance fee structure, PE/HF fund management would face principal-agent problems; (3) the coordination function existed before the tax preference and would exist after. Evidence supporting Snare: (1) the tax preference adds substantial value to carried interest beyond what performance fees alone would achieve; (2) reform efforts consistently target the tax treatment, not the carried interest structure itself, suggesting the tax preference is the extraction mechanism; (3) the coordination rationale (encouraging capital formation) does not hold empirically — PE/HF sectors accumulate capital regardless of tax treatment. RESOLUTION APPROACH: The constraint is legitimately Tangled Rope if the coordination function cannot be disentangled from the extraction mechanism. The critical test (omega_1: principal_at_risk) addresses this: if carried interest truly represents at-risk capital, the coordination is genuine and Tangled Rope holds; if it is disguised compensation with no real risk, the coordination is merely nominal and Snare becomes primary. The current classification (Tangled Rope with mandatrophy_resolved: false) reflects the empirical ambiguity: the constraint exhibits both functions, neither can be clearly isolated as dominant, and resolution requires deeper structural analysis of GP capital commitments and loss-sharing provisions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    carried_interest_principal_at_risk,
    'Does the general partner''s carried interest represent genuinely at-risk capital or is it a disguised performance fee?',
    'Analysis of historical profit-sharing provisions: Do GPs lose their carried interest share if the fund underperforms? Do they have personal capital invested? Do deal structures require co-investment at comparable risk levels to LP capital?',
    'If genuinely at-risk capital: preferential treatment is justified (constraint reclassifies toward pure Rope). If disguised performance fee: treatment is purely extractive (Snare from treasury perspective becomes dominant interpretation, mandatrophy shifts toward snare classification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(carried_interest_principal_at_risk, empirical, 'Whether carried interest represents truly at-risk capital or disguised compensation').

omega_variable(
    capital_formation_efficacy,
    'Does the preferential capital gains treatment actually increase capital formation in PE/HF sectors, or would similar capital flow regardless of tax treatment?',
    'Econometric analysis comparing PE/HF asset flows before and after tax changes; cross-country comparison with jurisdictions using different tax treatment; counterfactual analysis using instrumental variables for tax rate changes.',
    'If treatment significantly increases capital: coordination rationale holds (constraint leans Rope). If no elasticity or very low elasticity: coordination rationale collapses and extraction becomes primary function (constraint becomes Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_formation_efficacy, empirical, 'Whether preferential tax treatment materially increases capital formation').

omega_variable(
    legislative_reform_feasibility,
    'Is carried interest taxation politically sustainable, or will legislative reform inevitably narrow or eliminate preferential treatment?',
    'Analysis of congressional voting patterns, constituency pressure, lobbying expenditure trends, and public opinion on tax fairness; comparison with similar provisions that were successfully repealed (e.g., corporate tax shelters post-2008).',
    'If reform is inevitable: scaffold perspective (sunset logic) is correct and constraint will degrade to Rope or Snare as treatment is eliminated. If treatment survives indefinitely: piton classification (inertial degradation) becomes primary, constraint remains Tangled Rope or Piton long-term.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legislative_reform_feasibility, preference, 'Political sustainability of preferential carried interest treatment').

omega_variable(
    extraction_magnitude_verification,
    'What is the total annual revenue loss from preferential carried interest treatment, and how does this compare to other major tax provisions?',
    'Treasury Joint Committee on Taxation revenue estimates; longitudinal analysis of carried interest volumes and effective tax rates; comparison with revenue impact of other preferential provisions.',
    'If loss is > $50B annually: extraction is economically massive and public salience increases, likelihood of reform rises (scaffold becomes more probable). If loss is < $10B: can be portrayed as minor technical provision (piton classification reinforced).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_magnitude_verification, empirical, 'Quantified annual revenue loss from carried interest preferential treatment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(blackstone_carried_interest_taxation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bsci_tr_t0, blackstone_carried_interest_taxation, theater_ratio, 0, 0.4).
narrative_ontology:measurement(bsci_tr_t10, blackstone_carried_interest_taxation, theater_ratio, 10, 0.55).
narrative_ontology:measurement(bsci_tr_t20, blackstone_carried_interest_taxation, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(bsci_be_t0, blackstone_carried_interest_taxation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bsci_be_t10, blackstone_carried_interest_taxation, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(bsci_be_t20, blackstone_carried_interest_taxation, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(blackstone_carried_interest_taxation, resource_allocation).
narrative_ontology:affects_constraint(blackstone_carried_interest_taxation, private_equity_fee_structure).
narrative_ontology:affects_constraint(blackstone_carried_interest_taxation, wealth_inequality_accumulation).
narrative_ontology:affects_constraint(blackstone_carried_interest_taxation, executive_compensation_incentives).

% DUAL FORMULATION NOTE:
% Carried interest taxation decomposes into two structurally distinct claims: (1) the genuine coordination mechanism (performance-fee alignment between GPs and LPs), which would exist as Rope under any tax treatment, and (2) the preferential tax treatment (statutory Section 1231 partnership classification), which creates the extraction asymmetry. These could be modeled as separate constraints: carried_interest_as_coordination (ε ≈ 0.10, Rope) and carried_interest_tax_preference (ε ≈ 0.72, Snare). The current story captures both as Tangled Rope because they are empirically entangled: the tax preference fundamentally shapes the compensation structure, and the coordination function would be materially weaker without preferential treatment. Decomposition would require counterfactual analysis of PE/HF compensation under ordinary income taxation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(blackstone_carried_interest_taxation, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
