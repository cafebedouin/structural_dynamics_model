% ============================================================================
% CONSTRAINT STORY: collateral_sufficiency_floor
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_collateral_sufficiency_floor, []).

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
 *   constraint_id: collateral_sufficiency_floor
 *   human_readable: Collateral Sufficiency Floor in Credit Markets
 *   domain: economic/financial/credit
 *
 * SUMMARY:
 *   The collateral sufficiency floor in credit markets represents a
 *   structural tension between legitimate lender risk management and
 *   systematic extraction of surplus from borrowers with real productive
 *   assets that do not fit institutional valuation standards. This constraint
 *   is a diagnostic exemplar for how a single structural arrangement produces
 *   different DR classifications depending on the observer's position: the
 *   institutional lender sees coordination (Rope), the asset-rich, cash-poor
 *   borrower sees extraction (Snare), the small business owner sees mixed
 *   coordination-extraction (Tangled Rope), the informal economy participant
 *   sees complete exclusion (Snare), the appraisal system sees its own
 *   degraded ritual (Piton), large corporations see workaround pathways
 *   (Tangled Rope), the analytical observer risks naturalizing it as inherent
 *   to credit (Mountain, but false summit), and the alternative credit system
 *   sees a temporary institutional problem being solved (Scaffold). The
 *   constraint's extractiveness has risen over the interval (0.38 → 0.52) as
 *   institutional valuation standards have tightened and alternative credit
 *   pathways have remained marginalized. Suppression has increased (0.58 →
 *   0.68) as regulatory requirements for collateral documentation have
 *   strengthened. Theater has risen (0.35 → 0.55) as appraisal methodology
 *   has become increasingly detached from actual asset-based lending practice
 *   — the appraisal ritual persists as regulatory compliance theater even as
 *   credit underwriting moves toward behavioral and cash-flow assessment.
 *
 * KEY AGENTS:
 *   - Institutional Lenders: Primary beneficiary (institutional/arbitrage) — capture coordination surplus through standardized terms, lower screening costs, and secondary market liquidity
 *   - Asset-Rich, Cash-Poor Borrowers: Primary victim (powerless/trapped) — hold real productive assets but cannot access credit due to valuation standards; trapped between financial stagnation and predatory informal lending
 *   - Small Business Owners: Secondary victim (moderate/constrained) — benefit from access to institutional credit once granted, but face extraction through collateral haircuts, excess capital reserves, and valuation discounts
 *   - Informal Economy Participants: Excluded victims (organized/trapped) — entirely outside institutional credit gate; trapped in informal lending with 50-200% extraction rates
 *   - Collateral Appraisers: Secondary beneficiary (institutional/arbitrage) — benefit from regulation-mandated appraisal requirements; control valuation gate
 *   - Large Corporations: Powerful agents (powerful/mobile) — nominally subject to collateral floor but can bypass via equity markets, ratings, or guarantees; extract surplus from constraint enforcement against weaker agents
 *   - Alternative Credit Systems: Organized agents (organized/constrained) — CDFIs, peer lending, fintech; building alternative pathways with sunset potential
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(collateral_sufficiency_floor, 0.52).
domain_priors:suppression_score(collateral_sufficiency_floor, 0.68).
domain_priors:theater_ratio(collateral_sufficiency_floor, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(collateral_sufficiency_floor, extractiveness, 0.52).
narrative_ontology:constraint_metric(collateral_sufficiency_floor, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(collateral_sufficiency_floor, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(collateral_sufficiency_floor, tangled_rope).
narrative_ontology:human_readable(collateral_sufficiency_floor, "Collateral Sufficiency Floor in Credit Markets").
narrative_ontology:topic_domain(collateral_sufficiency_floor, "economic/financial/credit").

domain_priors:requires_active_enforcement(collateral_sufficiency_floor).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(collateral_sufficiency_floor, institutional_lenders).
narrative_ontology:constraint_beneficiary(collateral_sufficiency_floor, collateral_appraisers).
narrative_ontology:constraint_victim(collateral_sufficiency_floor, asset_rich_cash_poor_borrowers).
narrative_ontology:constraint_victim(collateral_sufficiency_floor, informal_economy_participants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ASSET-RICH, CASH-POOR BORROWER (SNARE) — Holds real productive assets (land, equipment, inventory) with measurable market value, but cannot access credit because institutional valuation standards discount or reject non-standardized collateral. Trapped: cannot liquidate assets without severe loss (fire sale), cannot move to alternative credit systems (informal lenders require payment at extraction rates 50-200%), cannot improve valuation (institutional appraisers control the gate). Maximum experienced extraction — the constraint forces a choice between financial stagnation and predatory borrowing.
constraint_indexing:constraint_classification(collateral_sufficiency_floor, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL BUSINESS OWNER (TANGLED ROPE) — Benefits from the coordination mechanism: institutional credit terms are genuinely better than informal lending once access is achieved. But also bears extraction: the collateral floor forces them to hold excess capital reserves, post assets valued below market rates, or accept higher interest as risk premium. Exit is costly but possible — can shift to trade credit, customer financing, or owner financing. Significant extraction but not maximal; genuine coordination function exists (standardized lending terms, lower rates than alternatives).
constraint_indexing:constraint_classification(collateral_sufficiency_floor, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL LENDER (ROPE) — Experiences the collateral floor as a pure coordination mechanism: standardized valuations reduce screening costs, enable capital redeployment across portfolios, facilitate secondary market transactions, and support regulatory compliance. The lender benefits from the constraint's existence. Lower-cost borrowers with liquid collateral subsidize the system. Net beneficiary — extraction runs toward this agent.
constraint_indexing:constraint_classification(collateral_sufficiency_floor, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INFORMAL ECONOMY PARTICIPANTS (SNARE) — Excluded entirely from institutional credit due to lack of documented collateral, tax records, or formal business registration. Trapped in informal lending networks with extraction rates of 50-200% annual. The collateral floor operates as a gate that prevents entry entirely. Maximum extraction — not merely from credit terms, but from exclusion from the formal system. Organized power (peer lending groups, informal guilds) cannot overcome the institutional gate.
constraint_indexing:constraint_classification(collateral_sufficiency_floor, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 5: COLLATERAL APPRAISAL SYSTEM (PITON) — The institutional machinery for valuation has become substantially performative. Appraisals rely on comparable sales in standardized markets (real estate, vehicles), creating false precision for unique or illiquid assets. The theater has increased as financial complexity outpaced appraisal methodology. The system persists through regulatory mandate (lenders must document valuation) and institutional inertia, despite recognized limitations. Theater ratio here reflects that much appraisal ritual is compliance theater rather than genuine risk assessment.
constraint_indexing:constraint_classification(collateral_sufficiency_floor, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: LARGE CORPORATIONS WITH DIVERSIFIED ASSETS (TANGLED ROPE) — Nominally subject to the same collateral floor but experience it differently: can leverage equity markets, credit ratings, or related-party guarantees to bypass traditional collateral requirements. The constraint coordinates among them (standardized terms enable massive capital flows) while simultaneously extracting from smaller firms that lack these workarounds. Powerful agents extract surplus from the constraint's enforcement against weaker agents. Mixed coordination and asymmetric extraction.
constraint_indexing:constraint_classification(collateral_sufficiency_floor, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some collateral requirement is inherent to credit allocation: asymmetric information means lenders cannot verify borrower repayment capacity without security. A collateral floor is natural law — an irreducible feature of credit markets themselves. However, structural data reveals this as a false summit: institutional standards are not inherent to credit; they are contingent choices (what counts as acceptable collateral is socially determined, not physically given). The mountain framing naturalizes extractive institutional arrangements.
constraint_indexing:constraint_classification(collateral_sufficiency_floor, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: ALTERNATIVE CREDIT SYSTEM (SCAFFOLD) — Peer lending platforms, community development financial institutions (CDFIs), and microfinance institutions use alternative collateral standards or behavioral underwriting. They see the traditional sufficiency floor as a temporary problem being solved by technology (credit scoring, online verification) and institutional innovation (relationship banking, portfolio-based assessment). Low effective extraction for participants because the system has agency and an explicit exit path from traditional banking. Sunset clause logic: as alternative systems mature and achieve scale, the institutional collateral floor's gatekeeping power erodes.
constraint_indexing:constraint_classification(collateral_sufficiency_floor, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(collateral_sufficiency_floor_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(collateral_sufficiency_floor, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(collateral_sufficiency_floor, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(collateral_sufficiency_floor, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(collateral_sufficiency_floor, TR),
    TR >= 0.70.

:- end_tests(collateral_sufficiency_floor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The collateral floor creates measurable borrower welfare loss through multiple channels: (1) interest rate premiums (2-4% higher than equivalent risk would justify) for collateral deficiency, (2) collateral haircuts (15-40% valuation discounts), (3) appraisal and documentation fees, (4) opportunity cost from holding excess liquid reserves to compensate for low collateral valuation, (5) complete exclusion of informal economy participants. The 0.52 value reflects that institutional credit remains superior to informal lending alternatives (which would be 0.80+), creating a coordination function that partially offsets extraction. Suppression (0.68): High. Barriers to exit are substantial: cannot liquidate assets (fire sale loss), cannot access alternative credit easily (informal lenders are geographically limited and extractive), cannot improve collateral valuation (institutional appraisers control the gate), cannot change institutional standards (regulatory mandates apply uniformly). The rising trajectory reflects post-2008 regulatory tightening (Basel III, Dodd-Frank) that increased collateral documentation requirements. Theater (0.55): Moderate. Appraisal methodology is substantially performative: relies on comparable sales for unique or illiquid assets, uses standardized models that poorly fit asset-specific risk, serves primarily as regulatory compliance artifact. Yet appraisal is not purely theater — it provides baseline risk assessment. The rising trajectory reflects that financial complexity (structured products, complex collateral arrangements) has outpaced appraisal capacity, increasing reliance on regulatory-mandated ritual over actual risk assessment.
 *
 * PERSPECTIVAL GAP:
 *   The largest gap is between the institutional lender (Rope) and the trapped asset-rich borrower (Snare). The lender experiences the constraint as solving a coordination problem (how to allocate capital safely and efficiently). The borrower experiences the constraint as a pure extraction mechanism (access to credit requires collateral discount + interest premium + appraisal fee + reserve requirements). The gap is not perspectival ambiguity — it reflects real structural difference in who controls the valuation gate and who bears the cost of excluding collateral forms outside standardized markets. The small business (Tangled Rope) splits the difference: they experience coordination benefits once approved but extraction costs during the approval process and ongoing collateral management.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional lender's beneficiary status derives from control of the collateral valuation gate: the floor enables them to set credit terms, migrate capital efficiently, and participate in secondary markets. The asset-rich, cash-poor borrower's victim status derives from asset illiquidity: real productive assets cannot be quickly liquidated without loss, and institutional standards discount or reject non-standardized collateral. The directionality chain computes d from this: beneficiary + arbitrage exit → low d → low chi; victim + trapped exit → high d → high chi. Organized borrowers (small businesses) have constrained exit (can shift to trade credit, slower growth) yielding intermediate d. Large corporations have mobile exit (equity markets, ratings, guarantees) despite beneficiary/victim status ambiguity, yielding lower d. Alternative systems have constrained exit (still marginal, still emerging) yielding intermediate d despite organizing against the institutional floor.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by recognizing that all six types are legitimate readings of different structural positions, not competing claims about the constraint's 'true' nature. The institutional lender's Rope is their genuine experience: the collateral floor genuinely reduces their screening costs and enables liquidity. The trapped borrower's Snare is their genuine experience: the floor genuinely extracts surplus from them via collateral discounts and premium pricing. The Tangled Rope is the small business's genuine experience: mixed coordination (access to credit) and extraction (collateral requirements). The Piton is the appraisal system's genuine status: performative regulatory machinery. The Scaffold is the alternative system's genuine pathway: real exit option emerging. The Mountain is the analytical observer's risk: false summit from naturalizing institutional choices. The constraint is not 'really' one type — it is the presheaf over multiple structural positions, all of which are correct from their respective positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collateral_sufficiency_definition,
    'Is the collateral sufficiency floor a lender''s legitimate risk-management requirement or an arbitrary institutional standard that excludes genuine credit-worthy borrowers?',
    'Comparative study of actual default rates: borrowers rejected by institutional standards but approved by CDFIs or peer lending; comparison of default rates for approved vs rejected cohorts; longitudinal tracking of asset value recovery rates',
    'If default rates for rejected borrowers are higher: mountain classification strengthened, standard is risk-based. If default rates are similar to accepted borrowers: snare classification strengthened, standard is extractive gatekeeping.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(collateral_sufficiency_definition, empirical, 'Whether collateral standards reflect actual credit risk or extractive gatekeeping').

omega_variable(
    alternative_collateral_viability,
    'Can alternative collateral forms (cash flow-based lending, inventory collateral, equipment leases, community guarantees) achieve cost-equivalent default rates without institutional standardization?',
    'Performance data from CDFIs, peer lending, and fintech platforms using non-traditional collateral; comparison of portfolio loss rates between alternative and institutional systems at equivalent risk tiers',
    'If alternative systems achieve comparable performance: scaffold sunset is real and the collateral floor is contingent institutional choice. If alternative systems have higher loss rates: floor represents genuine risk necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_collateral_viability, empirical, 'Whether alternative collateral forms can achieve equivalent risk management').

omega_variable(
    appraisal_methodology_accuracy,
    'How much of the variance in collateral valuation across institutional lenders is due to methodological differences vs genuine asset risk differences?',
    'Meta-analysis of appraisals for identical assets from multiple institutions; variance decomposition (methodology variance vs risk variance); bias analysis of appraiser consistency for asset classes outside standardized real estate',
    'If methodology variance is high: piton classification confirmed, appraisal system is substantially theater. If methodology variance is low: valuation floor reflects genuine assessment practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appraisal_methodology_accuracy, empirical, 'What portion of valuation variance is methodological vs risk-based').

omega_variable(
    extraction_rate_measurement,
    'What is the borrower welfare loss from the collateral floor compared to a counterfactual credit system without it (e.g., pure relationship banking, cash flow lending)?',
    'Borrower expense comparison: actual credit costs (interest, collateral haircuts, appraisal fees) vs estimated costs under alternative systems; analysis of borrowers who access credit through workarounds; historical comparison with pre-standardization credit markets',
    'If welfare loss is substantial: tangled rope extractiveness confirmed at 0.50+. If welfare loss is minimal: rope classification becomes more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_rate_measurement, empirical, 'Borrower welfare loss from collateral floor requirement').

omega_variable(
    systemic_risk_prevention,
    'Does the collateral sufficiency floor prevent systemic credit risk (bank runs, cascading defaults) or does it merely concentrate extraction on excluded borrowers?',
    'Historical analysis of credit crises: were crises caused by insufficient collateral standards (over-lending to weak collateral) or by other factors? Cross-national study of collateral standards vs financial stability outcomes',
    'If systemic risk prevention is demonstrated: mountain classification strengthened, floor is natural law protection. If floor does not prevent crises: false summit diagnosis confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(systemic_risk_prevention, empirical, 'Whether collateral standards prevent systemic financial risk').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(collateral_sufficiency_floor, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(csf_tr_t0, collateral_sufficiency_floor, theater_ratio, 0, 0.35).
narrative_ontology:measurement(csf_tr_t10, collateral_sufficiency_floor, theater_ratio, 10, 0.45).
narrative_ontology:measurement(csf_tr_t20, collateral_sufficiency_floor, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(csf_be_t0, collateral_sufficiency_floor, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(csf_be_t10, collateral_sufficiency_floor, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(csf_be_t20, collateral_sufficiency_floor, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(csf_su_t0, collateral_sufficiency_floor, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(csf_su_t10, collateral_sufficiency_floor, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(csf_su_t20, collateral_sufficiency_floor, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(collateral_sufficiency_floor, resource_allocation).
narrative_ontology:affects_constraint(collateral_sufficiency_floor, informal_lending_extraction).
narrative_ontology:affects_constraint(collateral_sufficiency_floor, small_business_capital_formation).
narrative_ontology:affects_constraint(collateral_sufficiency_floor, land_value_suppression).

% DUAL FORMULATION NOTE:
% The collateral sufficiency floor is upstream of informal lending (forces asset-rich borrowers into high-extraction informal systems) and small business capital formation (constrains growth pathways). It is also upstream of land value suppression (institutional collateral standards suppress land values by limiting demand from borrowers excluded by valuation standards). Each downstream constraint has its own ε reflecting their specific structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(collateral_sufficiency_floor, organized, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
