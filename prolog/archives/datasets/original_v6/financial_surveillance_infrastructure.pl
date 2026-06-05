% ============================================================================
% CONSTRAINT STORY: financial_surveillance_infrastructure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_financial_surveillance_infrastructure, []).

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
 *   constraint_id: financial_surveillance_infrastructure
 *   human_readable: Financial Surveillance Infrastructure
 *   domain: financial/regulatory/governance
 *
 * SUMMARY:
 *   Financial surveillance infrastructure has evolved over the past two
 *   decades from targeted anti-money-laundering mechanisms into comprehensive
 *   behavioral tracking systems embedded in banking, payment processing, and
 *   digital finance. The constraint exhibits fundamental tension between
 *   genuine coordination benefits (fraud prevention, operational reliability,
 *   regulatory capability) and asymmetric extraction of behavioral and
 *   locational data from ordinary financial participants. This data is
 *   retained indefinitely, accessible to state and institutional actors, and
 *   increasingly used for purposes beyond the stated regulatory mission. The
 *   constraint shows measurable extraction accumulation: extractiveness
 *   increased from 0.22 (primarily transaction-level reporting) to 0.58
 *   (real-time behavioral surveillance, cross-border data sharing, predictive
 *   analytics). Theater ratio has increased from 0.35 to 0.55, reflecting
 *   that much compliance activity is performative ritual (periodic reporting,
 *   documentation verification) rather than functional security control. The
 *   suppression mechanism combines structural barriers (exit from payment
 *   systems carries severe cost) and internalized acceptance (pervasive
 *   framing that 'nothing to hide' justifies unlimited surveillance). The
 *   constraint's identity as coordination vs. extraction depends critically
 *   on the observer's position.
 *
 * KEY AGENTS:
 *   - Ordinary Transaction Participants: Primary victims (powerless/trapped) — bear behavioral extraction without meaningful exit or choice; data retained indefinitely
 *   - Financial Regulators: Primary beneficiary (institutional/arbitrage) — core surveillance infrastructure operators; use data for stated regulatory mission
 *   - Institutional Banks: Secondary beneficiary (institutional/arbitrage) — benefit from compliance cost barriers creating competitive moats; access surveillance data for fraud detection
 *   - Privacy-Conscious Citizens: Secondary victims (moderate/constrained) — experience both coordination benefits (fraud detection, account security) and extraction burden; can partially exit through cash but face friction
 *   - Alternative Finance Coalition: Organized challengers (organized/constrained) — cryptocurrency, DeFi, informal value transfer represent alternative coordination pathways; face regulatory suppression
 *   - Unbanked Populations: Tertiary victims (powerless/identity_locked) — financial exclusion driven partly by surveillance infrastructure documentation requirements; identity fused with informal economy outside formal surveillance
 *   - Law Enforcement Agencies: Secondary beneficiaries (institutional/arbitrage) — access financial surveillance data for investigations beyond regulatory mission; secondary use enables scope creep
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(financial_surveillance_infrastructure, 0.58).
domain_priors:suppression_score(financial_surveillance_infrastructure, 0.68).
domain_priors:theater_ratio(financial_surveillance_infrastructure, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(financial_surveillance_infrastructure, extractiveness, 0.58).
narrative_ontology:constraint_metric(financial_surveillance_infrastructure, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(financial_surveillance_infrastructure, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(financial_surveillance_infrastructure, tangled_rope).
narrative_ontology:human_readable(financial_surveillance_infrastructure, "Financial Surveillance Infrastructure").
narrative_ontology:topic_domain(financial_surveillance_infrastructure, "financial/regulatory/governance").

domain_priors:requires_active_enforcement(financial_surveillance_infrastructure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(financial_surveillance_infrastructure, financial_regulators).
narrative_ontology:constraint_beneficiary(financial_surveillance_infrastructure, law_enforcement_agencies).
narrative_ontology:constraint_beneficiary(financial_surveillance_infrastructure, institutional_banks).
narrative_ontology:constraint_victim(financial_surveillance_infrastructure, ordinary_transaction_participants).
narrative_ontology:constraint_victim(financial_surveillance_infrastructure, financial_privacy_commons).
narrative_ontology:constraint_victim(financial_surveillance_infrastructure, unbanked_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORDINARY FINANCIAL PARTICIPANT (SNARE) — Cannot opt out of payment systems without accepting severe material disadvantage. Every financial transaction is recorded, analyzed, and retained in perpetuity. No genuine alternative exists for participating in the modern economy. Full extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(financial_surveillance_infrastructure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PRIVACY-CONSCIOUS CITIZEN (TANGLED ROPE) — Constrained by real coordination benefits (fraud detection, funds transfer reliability, account security) that surveillance infrastructure genuinely provides, alongside asymmetric extraction of behavioral and locational data. Can partially exit through cash-based transactions but faces time cost and merchant friction. Experiences both benefit and burden.
constraint_indexing:constraint_classification(financial_surveillance_infrastructure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL BANK (ROPE) — Benefits from surveillance infrastructure for operational reliability, regulatory compliance, and competitive intelligence on customer behavior. Can navigate reporting requirements through compliance infrastructure. Experiences constraint primarily as coordination mechanism enabling efficient large-scale payment systems.
constraint_indexing:constraint_classification(financial_surveillance_infrastructure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FINANCIAL REGULATOR (ROPE) — Primary coordinator and beneficiary. Surveillance infrastructure is their core tool for accomplishing stated mission (detecting money laundering, terrorism financing, financial crime). Experiences it as legitimate coordination mechanism. Extraction runs in beneficiary's direction — they extract behavioral data to accomplish regulatory goals.
constraint_indexing:constraint_classification(financial_surveillance_infrastructure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ALTERNATIVE FINANCE COALITION (TANGLED ROPE) — Organized response from cryptocurrency networks, decentralized finance (DeFi), and informal value transfer systems. These represent genuine coordination alternatives but operate under increasing regulatory suppression. Experience mixed extraction (regulatory targeting) and coordination benefit (building alternatives). Extraction asymmetry comes from regulatory enforcement advantage.
constraint_indexing:constraint_classification(financial_surveillance_infrastructure, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY FINANCIAL COMPLIANCE THEATER (PITON) — Certain surveillance mechanisms (transaction reporting thresholds, Suspicious Activity Reports, Know Your Customer processes) persist through institutional inertia despite limited effectiveness. Theater_ratio reflects that many compliance procedures are performative ritual rather than functional security. Banks perform compliance to satisfy regulators; regulators process reports to justify funding. The underlying coordination need (preventing financial crime) could be served more efficiently through direct technical controls, but the ritual persists.
constraint_indexing:constraint_classification(financial_surveillance_infrastructure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From maximum civilizational distance, some transaction verification is inherent to modern finance: any system that transfers value at scale must have some mechanism to prevent double-spending, fraud, and theft. This perspective naturalizes surveillance as an immutable feature of complex financial coordination. However, the distinction between necessary verification and extractive surveillance is observational, not structural. This perspective risks false summit — conflating contingent institutional choices (centralized ledgers with unlimited retention) with inherent limits of coordinated finance.
constraint_indexing:constraint_classification(financial_surveillance_infrastructure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(financial_surveillance_infrastructure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(financial_surveillance_infrastructure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(financial_surveillance_infrastructure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(financial_surveillance_infrastructure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(financial_surveillance_infrastructure, TR),
    TR >= 0.70.

:- end_tests(financial_surveillance_infrastructure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. The constraint extracts comprehensive behavioral and transactional data from financial participants, retained indefinitely, accessible to state and institutional actors. Current value reflects that extraction has accumulated over time (0.22 → 0.58) as infrastructure capabilities expanded from transaction reporting to real-time monitoring. Extractiveness continues rising due to regulatory scope creep (terrorism financing detection broadened to include drug trafficking, tax evasion, market manipulation) and institutional expansion (surveillance data now feeds AI/ML systems for predictive behavior analysis). Suppression (0.68): High. Participants cannot meaningfully exit payment systems without severe material disadvantage (job loss, housing barriers, business failure). Documentation requirements (KYC/AML) create barriers to financial inclusion for vulnerable populations. Regulatory enforcement against alternative systems (cryptocurrency restrictions, DeFi regulation) actively suppresses coordination alternatives. Theater ratio (0.55): Moderate-high. Significant proportion of compliance activity is performative: Suspicious Activity Report filing occurs at institutional level with minimal genuine investigation; Know Your Customer procedures collect documentation with limited verification effectiveness; transaction threshold reporting (structuring laws) creates compliance burden with marginal fraud-detection benefit. However, theater is not total — fraud detection mechanisms do prevent measurable losses. The rise from 0.35 to 0.55 reflects increasing documentation burden and cross-border reporting requirements that are primarily procedural rather than functionally essential.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence across power positions. The regulator and institutional bank see coordination (Rope) — surveillance infrastructure genuinely solves their operational and compliance problems. The ordinary participant sees extraction (Snare) — they have no meaningful choice and bear full data burden. The privacy-conscious citizen sees mixed coordination and extraction (Tangled Rope) — fraud prevention benefits are real, but behavioral surveillance extraction is asymmetric. The alternative finance coalition sees organized resistance to extractive constraint (Tangled Rope from organized perspective) — DeFi and cryptocurrency represent legitimate coordination alternatives being suppressed. The unbanked see financial exclusion (Snare with identity lock) — surveillance infrastructure documentation requirements create identity-based exclusion that goes beyond material barriers. The civilizational analytical observer risks naturalizing surveillance as immutable law of finance (Mountain), but the measurement trajectory and alternative systems evidence reveal this as false summit — the current architecture is contingent institutional choice, not inherent to financial coordination. The perspectival gaps reveal that 'financial surveillance' is not a single constraint but overlapping extraction mechanisms targeting different victim populations with different suppression mechanisms.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values vary dramatically by agent position. Regulators and institutional banks occupy beneficiary positions with arbitrage exit options (they can navigate compliance requirements, influence regulatory design, and exit through institutional scale) — their d values approach 0.15-0.25 (beneficiary with exit), yielding low/negative χ and rope classification. Ordinary participants occupy victim positions with trapped exit options (cannot exit payment systems without material catastrophe) — their d values approach 0.95 (full target), yielding high χ ≈ 1.4-1.5 and snare classification. Privacy-conscious citizens with constrained exit (can use cash with friction but not fully exit) occupy intermediate positions — d values around 0.65-0.75, yielding moderate χ around 1.0-1.1 and tangled rope classification. The alternative finance coalition with organized status but constrained exit (building alternatives that face suppression) occupies d around 0.50-0.60, yielding tangled rope classification with genuine coordination function alongside organized response to extraction. Unbanked populations with identity lock (excluded not just by material barriers but by documentation requirements fused with formal economy participation) occupy d around 0.85-0.90, similar to trapped victims but with cognitive dimension of exclusion from formal identity.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy tension manifests in the gap between regulatory justification and institutional function. Stated mission: prevent money laundering and terrorism financing. Actual function: comprehensive behavioral surveillance generating permanent records of ordinary financial activity. The tangled rope classification resolves this by acknowledging both dimensions: genuine coordination benefit (fraud prevention, operational reliability) exists alongside asymmetric extraction (behavioral data retention, secondary use, financial exclusion of non-compliant populations). The constraint cannot be purely extractive (snare) because the coordination benefits are real — banks do prevent fraud, regulators do detect financial crime. The constraint cannot be purely coordination (rope) because extraction is asymmetric and suppresses alternatives. The theater ratio rise (0.35 → 0.55) indicates increasing performative overhead relative to functional benefit — the constraint is accumulating theater, suggesting potential piton degradation over 30-50 year timeframe as technical alternatives (blockchain verification, zero-knowledge proofs, distributed fraud detection) enable equivalent coordination with lower surveillance extraction. The mandatrophy resolves by distinguishing the constraint's stated coordination goal (real, necessary) from the institutional implementation (extractive, escalating). Alternative implementations of equivalent coordination (e.g., privacy-preserving verification) may be technically feasible but politically infeasible because financial institutions and regulators benefit from current architecture's data extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_threshold_ambiguity,
    'What level of transaction data collection and retention is necessary for fraud prevention vs. what is extractive surveillance?',
    'Comparative analysis of fraud detection rates in high-retention (current) systems vs. minimal-retention jurisdictions; assessment of whether data aggregation improves fraud detection beyond transaction-level controls.',
    'If minimal retention sufficient: extractiveness should be higher (0.68+), suppression reflects unnecessary coercion. If aggregation essential: extractiveness may be lower (0.40-0.50), reflecting genuine coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_threshold_ambiguity, empirical, 'Necessity threshold for transaction data retention in fraud prevention').

omega_variable(
    regulatory_capture_mechanism,
    'To what extent do financial institutions benefit from surveillance infrastructure through competitive moats (smaller competitors cannot afford compliance infrastructure) vs. regulatory coordination?',
    'Analysis of compliance cost burden by institution size; measurement of regulatory enforcement asymmetry; assessment of market concentration trends post-surveillance infrastructure expansion.',
    'If capture mechanism dominant: beneficiary classification shifts from regulator to institutional banks; directionality inverts for banks from beneficiary to mixed; tangled rope may become snare from smaller competitor perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_mechanism, empirical, 'Regulatory capture through compliance infrastructure').

omega_variable(
    alternative_coordination_feasibility,
    'Could distributed ledger or direct-measurement approaches achieve equivalent fraud prevention with lower extraction than centralized surveillance?',
    'Technical assessment of DeFi fraud prevention mechanisms; measurement of theft/fraud rates in cryptocurrency systems; analysis of whether privacy-preserving verification methods exist.',
    'If feasible: constraint is contingent institutional choice, not natural law; tangled_rope classification is stronger. If not feasible: mountain perspective gains credibility; current architecture reflects genuine necessary coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_coordination_feasibility, empirical, 'Alternative coordination feasibility for financial verification').

omega_variable(
    data_retention_purpose_creep,
    'Is surveillance infrastructure data used primarily for stated regulatory mission (money laundering/terrorism detection) or for secondary purposes (tax enforcement, law enforcement investigations, market intelligence)?',
    'Freedom of information requests for data access logs; measurement of data requests by purpose; analysis of regulatory mission scope creep over time.',
    'If primary mission only: extractiveness lower (0.45-0.50), theater ratio lower (0.35-0.45). If significant secondary use: extractiveness higher (0.60+), theater ratio higher (0.65+), manifests as hidden extraction layer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_retention_purpose_creep, empirical, 'Data use scope creep beyond regulatory mission').

omega_variable(
    unbanked_population_barrier,
    'Does financial surveillance infrastructure contribute to financial exclusion of vulnerable populations (undocumented immigrants, poor communities, informal economy workers)?',
    'Demographic analysis of unbanked populations; measurement of access barriers; assessment of correspondence between surveillance infrastructure expansion and financial exclusion trends.',
    'If barrier effect significant: victims classification expands; suppression score should increase (0.75+); victimhood becomes distributed across powerless populations unable to comply with documentation requirements.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unbanked_population_barrier, empirical, 'Surveillance infrastructure as financial exclusion mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(financial_surveillance_infrastructure, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(finsurv_tr_t0, financial_surveillance_infrastructure, theater_ratio, 0, 0.35).
narrative_ontology:measurement(finsurv_tr_t10, financial_surveillance_infrastructure, theater_ratio, 10, 0.45).
narrative_ontology:measurement(finsurv_tr_t20, financial_surveillance_infrastructure, theater_ratio, 20, 0.55).
narrative_ontology:measurement(finsurv_tr_t5, financial_surveillance_infrastructure, theater_ratio, 5, 0.38).

% Extraction over time
narrative_ontology:measurement(finsurv_be_t0, financial_surveillance_infrastructure, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(finsurv_be_t10, financial_surveillance_infrastructure, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(finsurv_be_t20, financial_surveillance_infrastructure, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(finsurv_be_t5, financial_surveillance_infrastructure, base_extractiveness, 5, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(financial_surveillance_infrastructure, enforcement_mechanism).
narrative_ontology:affects_constraint(financial_surveillance_infrastructure, financial_inclusion_exclusion).
narrative_ontology:affects_constraint(financial_surveillance_infrastructure, cryptocurrency_regulatory_suppression).
narrative_ontology:affects_constraint(financial_surveillance_infrastructure, tax_enforcement_scope_creep).

% DUAL FORMULATION NOTE:
% Financial surveillance infrastructure operates at the intersection of three structurally distinct constraints: (1) Anti-money-laundering coordination (genuine regulatory need), (2) Behavioral extraction from financial participants (asymmetric surveillance), (3) Financial exclusion of populations unable or unwilling to comply with documentation. These three overlap within the single infrastructure but have distinct ε values. The anti-money-laundering coordination alone would be Rope (ε≈0.20). Behavioral extraction alone would be Snare (ε≈0.65). Financial exclusion adds suppression dimension. The current story models the hybrid tangled rope, but decomposition into three separate stories would enable more precise classification of each mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(financial_surveillance_infrastructure, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
