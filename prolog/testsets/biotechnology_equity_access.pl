% ============================================================================
% CONSTRAINT STORY: biotechnology_equity_access
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biotechnology_equity_access, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: biotechnology_equity_access
 *   human_readable: Biotechnology Equity Access: Therapeutic Innovation vs. Economic Exclusion
 *   domain: healthcare/economic/biotech
 *
 * SUMMARY:
 *   Biotechnology equity access represents a structural constraint governing
 *   who can benefit from cutting-edge therapeutics. The constraint operates
 *   at the intersection of intellectual property law, market economics,
 *   regulatory frameworks, and global health governance. On one side:
 *   pharmaceutical manufacturers and patent holders in wealthy markets
 *   benefit from monopoly pricing and exclusive access. On the other side:
 *   low-income populations, global south nations, and rare disease
 *   communities face insurmountable barriers to accessing life-saving or
 *   life-extending therapeutics. The constraint exhibits all six DR types
 *   from different perspectives, making it a diagnostic exemplar for how
 *   institutional arrangements that claim to serve universal innovation goals
 *   actually distribute extractive costs asymmetrically. The underlying
 *   tension is genuine coordination (how to allocate scarce biotechnology
 *   therapeutics globally, how to balance innovation incentives against
 *   equity) overlaid with extractive mechanisms (patent monopolies, price
 *   discrimination, regulatory capture). Extractiveness has been increasing
 *   over the measurement interval (0.48 → 0.58) as the cost of cutting-edge
 *   therapies has risen faster than global income growth. Theater ratio has
 *   also increased (0.42 → 0.55), reflecting growing performative compliance
 *   with 'access commitments' that lack enforcement mechanisms.
 *
 * KEY AGENTS:
 *   - Low-Income Populations: Primary victim (powerless/trapped) — lack purchasing power and insurance coverage; face complete exclusion from most cutting-edge biotechnology therapeutics
 *   - Global South Nations: Secondary victim (moderate/constrained) — healthcare systems have limited budgets for biotech; constrained by debt, institutional capacity, and international trade agreements
 *   - Rare Disease Communities: Tertiary victim (moderate/constrained) — small patient populations mean low revenue potential, creating markets too small to sustain innovation but too large to ignore ethically; caught between exclusion and unaffordable access
 *   - Pharmaceutical Manufacturers: Primary beneficiary (institutional/arbitrage) — capture monopoly rents through patent protection and price discrimination; can redirect investment, license selectively, or relocate operations
 *   - Patent System: Institutional actor (institutional/arbitrage) — maintains exclusivity mechanisms; benefits from enforcement through legal apparatus and regulatory alignment
 *   - Global Health Coalitions: Organized actors (organized/constrained) — WHO, MSF, GAVI, patent pools, health foundations; building alternative pathways (tiered pricing, compulsory licensing, technology transfer); see sunset as feasible within generational timeline
 *   - Middle-Income Healthcare Systems: Institutional actor (moderate/constrained) — navigate between coordination problems (how to integrate biotech into care) and extraction (pricing, licensing restrictions)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent policy choices as inherent to biotechnology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biotechnology_equity_access, 0.58).
domain_priors:suppression_score(biotechnology_equity_access, 0.68).
domain_priors:theater_ratio(biotechnology_equity_access, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biotechnology_equity_access, extractiveness, 0.58).
narrative_ontology:constraint_metric(biotechnology_equity_access, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(biotechnology_equity_access, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biotechnology_equity_access, tangled_rope).
narrative_ontology:human_readable(biotechnology_equity_access, "Biotechnology Equity Access: Therapeutic Innovation vs. Economic Exclusion").
narrative_ontology:topic_domain(biotechnology_equity_access, "healthcare/economic/biotech").

domain_priors:requires_active_enforcement(biotechnology_equity_access).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biotechnology_equity_access, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(biotechnology_equity_access, wealthy_market_economies).
narrative_ontology:constraint_beneficiary(biotechnology_equity_access, patent_holders).
narrative_ontology:constraint_victim(biotechnology_equity_access, low_income_populations).
narrative_ontology:constraint_victim(biotechnology_equity_access, global_south_nations).
narrative_ontology:constraint_victim(biotechnology_equity_access, rare_disease_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED PATIENT (SNARE) — Low-income individuals and populations in resource-limited regions face insurmountable barriers to accessing biotechnology-derived therapeutics. Patent monopolies, pricing mechanisms, and regulatory frameworks lock them out with no viable exit. High extraction, maximum suppression, minimal coordination benefit.
constraint_indexing:constraint_classification(biotechnology_equity_access, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MIDDLE-INCOME NATION (TANGLED ROPE) — Healthcare systems in developing economies face genuine coordination problems (how to allocate scarce biotech therapeutics, how to integrate into existing care pathways) alongside asymmetric extraction through pricing and licensing restrictions. Constrained exit: cannot simply opt out of biotech access without abandoning treatment options for local patients. Mixed coordination and extraction.
constraint_indexing:constraint_classification(biotechnology_equity_access, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PHARMACEUTICAL CORPORATION (ROPE) — Experiences the biotech constraint as coordination: patent protection enables R&D investment recoupment, pricing mechanisms coordinate allocation across markets with different willingness-to-pay. Full beneficiary with arbitrage options (can redirect investment, license alternatives, relocate operations). Net positive extraction flow toward this agent.
constraint_indexing:constraint_classification(biotechnology_equity_access, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GLOBAL HEALTH COALITION (SCAFFOLD) — Organized actors (WHO, Médecins Sans Frontières, global health foundations, TRIPS waiver movements) see the access gap as a temporary coordination failure with sunset potential. Tiered pricing, compulsory licensing, technology transfer agreements, and generic production capacity in LMIC represent alternative pathways. Sunset clause: as generic manufacturing scales in South Asia and Africa, and as patent pools and prize mechanisms mature, traditional monopoly extraction loses force. Estimated generational timeline for norms shift.
constraint_indexing:constraint_classification(biotechnology_equity_access, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PATENT PROTECTION SYSTEM (PITON) — Institutional framework maintains enforcement of IP rights primarily through inertia and theater. Original function (incentivizing innovation) is substantially degraded — much innovation is driven by public research funding, philanthropy, and academic competition rather than patent monopolies alone. System persists through performative compliance (patent filings, litigation threats) rather than actual functional necessity. Theater-ratio driven classification: sophisticated legal apparatus maintaining exclusion whose functional innovation incentive is outsized relative to actual mechanism.
constraint_indexing:constraint_classification(biotechnology_equity_access, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN) — Risk of misclassifying the access constraint as an inherent feature of biotech economics: 'innovation requires patent protection, high prices fund R&D, poor countries cannot afford cutting-edge therapeutics — this is natural to the system.' This perspective naturalizes contingent institutional arrangements (patent law, market pricing, regulatory frameworks) as immutable laws of biotechnology. The engine's false summit detector will identify this naturalization as a misclassification: the constraint is contingent, not inherent.
constraint_indexing:constraint_classification(biotechnology_equity_access, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biotechnology_equity_access_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(biotechnology_equity_access, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(biotechnology_equity_access, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(biotechnology_equity_access, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(biotechnology_equity_access, TR),
    TR >= 0.70.

:- end_tests(biotechnology_equity_access_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Pharmaceutical manufacturers extract significant rents through patent monopolies, price discrimination, and regulatory barriers to generic competition. However, the extraction is not total (0.70+): substantial public research funding, patent pools, generic production in some jurisdictions, and tiered pricing arrangements create countervailing forces. The value reflects that genuine coordination functions coexist with extractive mechanisms. Suppression (0.68): Moderate-high. Barriers include patent law enforcement (international), regulatory approval requirements, manufacturing complexity (especially for biologics and mRNA therapies), lack of generic manufacturing capacity in LMIC, currency/payment barriers, and information asymmetries. Notably, suppression is not structural unavoidability — it is enforced through law and institutional design. Theater ratio (0.55): Moderate. Patent system maintains significant performative elements: patent litigation threats, access commitment announcements without enforcement, WHO-brokered tiered pricing frameworks that operate at token levels. But actual biotechnology development and production create genuine coordination challenges (supply chain logistics, regulatory approval complexity, manufacturing quality). The theater has increased as the gap between 'access commitments' and actual pricing has widened.
 *
 * PERSPECTIVAL GAP:
 *   The pharmaceutical corporation sees rope: the patent system coordinates their R&D investment, price discrimination allocates therapeutics across markets with different demand elasticities, and regulatory exclusivity enables market planning. The global health coalition sees scaffold: tiered pricing, compulsory licensing, patent pools, and manufacturing capacity in LMIC represent genuine exit mechanisms with sunset potential. The excluded patient sees snare: patents and prices create insurmountable barriers with no way out except through illness, death, or waiting for generic alternatives. The patent system itself sees piton: its enforcement apparatus is increasingly performative (patent threats without litigation, 'access commitments' without binding enforcement), and much actual innovation is driven by public funding and academic competition rather than patent incentives. The false summit at the analytical level risks saying 'this is how biotechnology must work' when it is actually how policy has chosen to structure it. These are not failures of perspective — they are real structural differences driven by power asymmetries and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The excluded patient (powerless/trapped) has no exit options and bears full extraction cost — d ≈ 0.95, f(d) ≈ 1.42 — maximum experienced extractiveness. The global south nation (moderate/constrained) has limited but real options (compulsory licensing, generic substitution, therapy refusal) — d ≈ 0.65, f(d) ≈ 1.00 — moderate extraction. The pharmaceutical corporation (institutional/arbitrage) can reallocate investment and licensing to maximize revenue — d ≈ 0.05, f(d) ≈ -0.12 — negative extraction (beneficiary position). The global health coalition (organized/constrained) has coalition power and policy levers — d ≈ 0.35, f(d) ≈ 0.25 — low extraction due to organized position. Directionality overrides are not needed: the structural data correctly drives differentiation through exit options and beneficiary/victim declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates mandatrophy resolution through perspectival multiplicity. The mandatrophy question: 'Is biotech access coordination or extraction?' Answer: both, depending on structural position. For pharmaceutical manufacturers, it is coordination (patent system solves legitimate innovation incentive problems). For excluded patients, it is extraction (barriers with no exit). For global south nations, it is tangled rope (genuine coordination challenges with overlaid extraction). The piton classification captures institutional degradation: patent system persists through theater, not through genuine innovation incentive function (much innovation is publicly funded). The false summit (mountain) captures the risk of naturalizing policy: framing access barriers as inherent to biotechnology rather than contingent on institutional design. No single type resolves the mandatrophy — the presheaf over all perspectives reveals that the constraint is a policy choice, not a natural law, and that different agents experience radically different structural realities from the same institutional arrangements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    innovation_incentive_attribution,
    'How much does patent-based extraction actually drive biotechnology innovation versus public funding, academic competition, and philanthropic incentives?',
    'Comparative analysis of innovation output per dollar: publicly-funded vs private patented research in therapeutics; correlation between patent strength and R&D investment; tracking of innovation drivers in high-burden diseases (malaria, tuberculosis) with significant public funding',
    'If public/philanthropy funding dominates innovation: patent monopolies are unnecessary extraction (snare classification strengthens for all victim perspectives). If patents critical: coordination function is genuine (tangled rope classification holds). Determines whether scaffold sunset is structural or aspirational.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(innovation_incentive_attribution, empirical, 'Attribution of biotechnology innovation drivers').

omega_variable(
    tiered_pricing_effectiveness,
    'Can tiered pricing and differential licensing actually achieve broad equity access without collapsing high-income market incentives or creating gray-market arbitrage?',
    'Real-world case studies of tiered pricing (antiretrovirals, COVID vaccines, oncology drugs); measurement of access expansion vs price collapse in high-income markets; tracking of generic competition and regulatory arbitrage',
    'If tiered pricing works: scaffold has real implementation pathway (sunset is feasible). If pricing collapses or arbitrage undermines differentiation: extraction mechanism is structural (snare strengthens). If partial success: tangled rope with sunset incomplete.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tiered_pricing_effectiveness, empirical, 'Effectiveness of tiered pricing for equity access').

omega_variable(
    global_manufacturing_capacity_trajectory,
    'Will generic biotechnology manufacturing capacity in LMIC (particularly mRNA and cell therapy production) grow fast enough to establish independent supply chains, or will supply chain capture persist?',
    'Tracking of manufacturing facility growth, regulatory approvals, and technology transfer agreements in South Asia, Africa, and Latin America; measurement of cost reduction in generic biotech production; analysis of supply chain concentration in specific therapeutic classes',
    'If capacity scales significantly: scaffold sunset is structural (powerless agents gain arbitrage options within 10-15 years). If capacity remains concentrated: extraction mechanisms remain stable (snare and mountain perspectives more durable). Determines whether the constraint is transient or structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_manufacturing_capacity_trajectory, empirical, 'Trajectory of generic biotechnology manufacturing capacity in LMIC').

omega_variable(
    identity_lock_in_pharmaceutical_policy,
    'Are policy elites and regulators in high-income countries identity-locked into IP-maximalist framing, or are they structurally constrained by institutional incentives?',
    'Qualitative analysis of policy narratives and reform resistance; comparison with jurisdictions that have adopted alternative frameworks (Brazil TRIPS flexibility, India generic production, EU tiered pricing); tracking of elite mobility between pharmaceutical industry and regulatory bodies',
    'If identity-locked: regulatory change requires identity frame shift (longer timeline). If constrained by incentives: policy reform is possible with different institutional design. Affects whether piton classification is accurate (theatrical maintenance) or whether the system is more actively defended.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_pharmaceutical_policy, conceptual, 'Identity lock in pharmaceutical policy elites').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biotechnology_equity_access, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(biotech_equity_tr_t0, biotechnology_equity_access, theater_ratio, 0, 0.42).
narrative_ontology:measurement(biotech_equity_tr_t5, biotechnology_equity_access, theater_ratio, 5, 0.5).
narrative_ontology:measurement(biotech_equity_tr_t10, biotechnology_equity_access, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(biotech_equity_be_t0, biotechnology_equity_access, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(biotech_equity_be_t5, biotechnology_equity_access, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(biotech_equity_be_t10, biotechnology_equity_access, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biotechnology_equity_access, resource_allocation).
narrative_ontology:affects_constraint(biotechnology_equity_access, pharmaceutical_patent_monopoly).
narrative_ontology:affects_constraint(biotechnology_equity_access, global_health_governance).
narrative_ontology:affects_constraint(biotechnology_equity_access, generic_drug_supply_chain).

% DUAL FORMULATION NOTE:
% Biotechnology equity access is downstream of pharmaceutical patent policy and global trade agreements (TRIPS, trade bilaterals) but represents a distinct structural constraint operating at the healthcare delivery level. The upstream constraints (patent law) have their own extractiveness values; this constraint captures the systemic effects on access across all biotechnology therapeutics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
