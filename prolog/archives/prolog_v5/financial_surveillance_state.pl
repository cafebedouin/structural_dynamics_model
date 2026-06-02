% ============================================================================
% CONSTRAINT STORY: financial_surveillance_state
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_financial_surveillance_state, []).

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
 *   constraint_id: financial_surveillance_state
 *   human_readable: Financial Surveillance State
 *   domain: political_economy/governance
 *
 * SUMMARY:
 *   The financial surveillance state represents a constraint that has evolved
 *   from genuine post-9/11 security coordination (AML/KYC frameworks,
 *   counterterrorism financing detection) into a hybrid extraction mechanism
 *   that preserves security theater while expanding state and corporate
 *   control over financial flows. The constraint exhibits contradictory
 *   structural properties: legitimate coordination benefits (fraud reduction,
 *   money laundering detection) coexist with asymmetric extraction (privacy
 *   elimination, informal economy criminalization, financial autonomy
 *   suppression). Different institutional actors experience radically
 *   different classifications: state tax agencies and intelligence services
 *   see pure coordination (Rope); individual account holders see pure
 *   extraction (Snare); banks experience mixed coordination and regulatory
 *   burden (Tangled Rope). The theater ratio (0.55) reflects that
 *   surveillance intensity has outpaced genuine threat reduction — much
 *   monitoring now performs geopolitical signaling, tax enforcement, and
 *   political control rather than security functions. Exit pathways exist
 *   (cryptocurrency, informal economy, cash) but face accelerating regulatory
 *   suppression, suggesting the system is shifting from
 *   coordination-extraction hybrid toward pure extraction.
 *
 * KEY AGENTS:
 *   - Individual Account Holders: Primary victim (powerless/trapped) — trapped within banking infrastructure; every transaction recorded and subject to analysis or seizure
 *   - Informal Economy Workers: Primary victim (powerless/trapped) — forced into false choice between illegibility and criminal liability; maximum suppression and extraction
 *   - State Tax Enforcement: Primary beneficiary (institutional/arbitrage) — captures revenue and compliance benefits; experiences constraint as pure coordination
 *   - Financial Intelligence Agencies: Primary beneficiary (institutional/arbitrage) — accesses surveillance infrastructure for threat detection; arbitrage between monitoring intensity and target selection
 *   - Commercial Banks: Secondary actor (institutional/constrained) — benefits from fraud reduction and regulatory coordination; bears compliance burden and liability
 *   - Large Corporations: Secondary beneficiary (powerful/arbitrage) — possess compliance infrastructure and regulatory arbitrage capacity; extract advantage from competitors lacking resources
 *   - Small Business Owners: Secondary victim (moderate/constrained) — constrained by reporting requirements and audit risk; competitive disadvantage versus large firms
 *   - Cryptocurrency Adopters: Exit-seeking organized agent (organized/mobile) — partially exit surveillance through alternative systems; face increasing regulatory suppression
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing contingent surveillance architecture as immutable requirement of financial systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(financial_surveillance_state, 0.58).
domain_priors:suppression_score(financial_surveillance_state, 0.68).
domain_priors:theater_ratio(financial_surveillance_state, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(financial_surveillance_state, extractiveness, 0.58).
narrative_ontology:constraint_metric(financial_surveillance_state, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(financial_surveillance_state, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(financial_surveillance_state, tangled_rope).
narrative_ontology:human_readable(financial_surveillance_state, "Financial Surveillance State").
narrative_ontology:topic_domain(financial_surveillance_state, "political_economy/governance").

domain_priors:requires_active_enforcement(financial_surveillance_state).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(financial_surveillance_state, state_tax_enforcement).
narrative_ontology:constraint_beneficiary(financial_surveillance_state, financial_intelligence_agencies).
narrative_ontology:constraint_beneficiary(financial_surveillance_state, large_institutional_banks).
narrative_ontology:constraint_victim(financial_surveillance_state, individual_financial_privacy).
narrative_ontology:constraint_victim(financial_surveillance_state, informal_economy_workers).
narrative_ontology:constraint_victim(financial_surveillance_state, financial_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL ACCOUNT HOLDER (SNARE) — Trapped within banking infrastructure with no viable exit. Every transaction is recorded, analyzed, and potentially subject to seizure or investigation. Cannot conduct routine financial activity outside surveillance network. Bears full extraction cost with no compensating benefit or agency.
constraint_indexing:constraint_classification(financial_surveillance_state, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INFORMAL ECONOMY WORKERS (SNARE) — Trapped between illegibility (cannot prove income for credit/housing) and exposure (formal banking reveals tax liability). Suppression is asymmetric: forced toward informal channels that carry criminal risk, or into formal system that creates liability. Maximum extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(financial_surveillance_state, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: COMMERCIAL BANKS (TANGLED ROPE) — Coordinated through compliance infrastructure (AML/KYC requirements reduce fraud costs), but also subject to regulatory capture and compliance burden. Benefits from financial system coordination and reduced fraud risk; bears operational costs of surveillance maintenance. Mixed extraction: regulatory burden is genuine but arbitrage exists within compliance frameworks.
constraint_indexing:constraint_classification(financial_surveillance_state, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE TAX ENFORCEMENT (ROPE) — Net beneficiary. Surveillance infrastructure enables collection and reduces evasion. Experiences constraint as pure coordination: surveillance mechanisms coordinate tax extraction with minimal operational friction. Can arbitrage between formal and informal economy targeting.
constraint_indexing:constraint_classification(financial_surveillance_state, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: FINANCIAL INTELLIGENCE AGENCIES (ROPE) — Pure coordination benefit. Surveillance infrastructure enables threat detection, money laundering prevention, and security monitoring. Institutional actor with arbitrage capacity (can choose monitoring targets and intensity). Experiences constraint as coordinated information flow with distributed benefit.
constraint_indexing:constraint_classification(financial_surveillance_state, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: SMALL BUSINESS OWNERS (SNARE) — Constrained by surveillance and reporting requirements that create operational burden and competitive disadvantage versus large firms with compliance infrastructure. High suppression through regulatory complexity and audit risk. Limited genuine benefit from coordination; mostly bears extraction of time, money, and operational friction.
constraint_indexing:constraint_classification(financial_surveillance_state, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: CRYPTOCURRENCY ADOPTERS (TANGLED ROPE) — Partially exit through alternative currency systems; possess organizational capacity and technical mobility. Benefit from financial innovation coordination; face increasing regulatory suppression. Mixed classification: genuine coordination benefit of decentralized finance exists alongside extractive regulatory response and network effects capture by early adopters.
constraint_indexing:constraint_classification(financial_surveillance_state, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: POST-COLD WAR REGIME (PITON) — Surveillance infrastructure inherited from Cold War counterintelligence and drug enforcement now maintained through institutional inertia. Theater ratio rising (0.55): much surveillance performs geopolitical signaling rather than actual threat detection. Regime persists because alternatives haven't fully emerged; primary function (actual security gain) may be degraded relative to secondary function (state control maintenance).
constraint_indexing:constraint_classification(financial_surveillance_state, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From civilizational/universal perspective, some financial monitoring is structurally necessary: coordinating resource flows across complex economies creates inherent need for transaction transparency. This perspective risks naturalizing contingent institutional design choices (centralized surveillance, state monopoly on monitoring) as immutable requirements of any financial system. Engine's false summit detector will reveal whether the mountain classification is justified or represents naturalization of a contingent arrangement.
constraint_indexing:constraint_classification(financial_surveillance_state, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(financial_surveillance_state_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(financial_surveillance_state, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(financial_surveillance_state, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(financial_surveillance_state, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(financial_surveillance_state, TR),
    TR >= 0.70.

:- end_tests(financial_surveillance_state_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The trajectory shows clear accumulation from 0.32 (2004 post-9/11 genuine coordination) to 0.58 (2024 hybrid extraction-coordination). Initial surveillance served genuine coordination functions (reducing terrorism financing, money laundering). Current surveillance persists in coordination function but has layered extraction: tax enforcement, political targeting, informal economy suppression, financial autonomy elimination. The 0.58 value reflects that the primary function (security) has been substantially overtaken by secondary functions (control, extraction) but genuine coordination benefit remains sufficient to sustain hybrid rather than pure snare classification. Suppression (0.68): High. Multiple barriers to exit: mandatory banking system participation, regulatory criminalization of alternatives (informal economy, cryptocurrency), transaction recording eliminating financial privacy, account seizure risk. Suppression is structural and escalating. Theater ratio (0.55): Moderate-rising. Much surveillance activity performs intelligence signaling and political control rather than genuine threat detection. Increasing focus on routine financial activity (structuring, cash deposits, cross-border transfers) that poses minimal security threat but maximum political utility. Theater has risen from 0.42 to 0.55 over interval as genuine security threats have declined while surveillance scope expanded.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. State tax enforcement sees pure coordination (Rope) — surveillance solves genuine collection problems with minimal friction. Individual account holders see pure extraction (Snare) — trapped, surveilled, no benefit. Banks see mixed (Tangled Rope) — coordination benefit of fraud reduction balanced against regulatory burden and liability risk. Cryptocurrency community sees partial exit pathway with rising suppression (Tangled Rope) — genuine coordination of alternative finance exists alongside regulatory extraction increasing over time. Analytical observer at universal scope risks mountain classification — naturalizing centralized surveillance as inevitable requirement of financial systems — but structural data suggests this is naturalization rather than necessity: viable privacy-preserving alternatives exist technically; suppression is political choice, not structural requirement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply by institutional position. State tax agencies (beneficiary + arbitrage) experience low d → negative χ: they benefit from coordination with minimal cost. Financial intelligence agencies (beneficiary + arbitrage) similarly low d. Individual account holders (victim + trapped) experience maximum d ≈ 0.95 → high f(d) ≈ 1.42, producing χ values amplified by scope modifier σ(national) = 1.0. Banks occupy intermediate position: beneficiary from fraud reduction and coordination, but victim of regulatory burden and liability — moderate d ≈ 0.50 → constrained exit option raises d further. Small business owners (victim + constrained) experience higher d than banks due to reduced exit capacity and compliance resources. Cryptocurrency adopters (victim but mobile exit option) experience lower d than fully trapped agents despite victim status — their exit capacity moderates experienced extractiveness. The piton perspective has institutional power and arbitrage exit (lowest d values), explaining why post-Cold War regime perceives minimal extraction despite significant global suppression.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits strong mandatrophy indicators that complicate classification. Initial genuine coordination benefit (AML/KYC reducing fraud and terrorism financing) has been overlaid with extraction mechanisms (tax enforcement, financial autonomy suppression, informal economy criminalization) that were not the stated justification. The theater ratio (0.55) and rising trajectory (0.42 → 0.55) indicate secondary functions increasingly dominating primary function. Resolution requires distinguishing: (1) What proportion of surveillance activity actually detects genuine threats versus performs political/economic control? (2) Could equivalent threat detection be achieved with lower suppression through privacy-preserving architectures? (3) Is suppression justified by security benefit or explained primarily by extraction logic? Current classification as Tangled Rope reflects ambiguity: genuine coordination function persists (fraud detection, AML compliance) but is increasingly hollowed out by extractive overlay. If theater ratio crosses 0.65 or extractiveness reaches 0.70, constraint should reclassify toward Snare. Mandatrophy is unresolved pending empirical resolution of genuine-versus-theatrical security benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_vs_theatrical_security,
    'What proportion of financial surveillance activity produces genuine security benefit versus serving state control and revenue extraction?',
    'Correlation analysis between surveillance intensity and actual threat detection rates; comparison of false positive/negative ratios over time; analysis of surveillance targeting patterns correlated with tax compliance versus genuine financial crimes',
    'If genuine benefit > 70%: constraint is primarily coordination (Rope from analyst perspective). If genuine benefit < 40%: constraint is primarily extractive (Snare from analyst perspective, replicating affected populations). Theater ratio trajectory critical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_vs_theatrical_security, empirical, 'Proportion of surveillance activity producing security benefit versus state control').

omega_variable(
    exit_capacity_informal_economy,
    'Can informal economy workers meaningfully exit through cryptocurrency, cash, or alternative systems, or does regulatory suppression close these exits faster than they open?',
    'Historical analysis of exit pathway emergence and regulatory closure; cross-national comparison of informal economy size in high-surveillance versus low-surveillance regimes; measurement of adoption rates versus regulatory crackdown timelines',
    'If exits remain open: constraint is Tangled Rope for informal workers (coordination with asymmetric extraction). If exits close faster than emergence: constraint is Snare with degraded alternatives (maximum extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_capacity_informal_economy, empirical, 'Whether informal economy workers can sustain exit pathways').

omega_variable(
    essential_vs_contingent_surveillance,
    'Is centralized transaction surveillance structurally required for financial coordination, or could distributed/privacy-preserving alternatives (encrypted ledgers, local clearing houses) achieve equivalent coordination with lower suppression?',
    'Technical analysis of alternative architectures; empirical comparison of coordination efficiency in privacy-preserving versus centralized systems; pilot studies of alternative financial monitoring mechanisms',
    'If alternatives are viable: surveillance state is not a mountain but a contingent institutional choice (Tangled Rope or Snare depending on extraction level). If true necessity: mountain classification may be justified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(essential_vs_contingent_surveillance, conceptual, 'Whether centralized surveillance is structurally necessary for financial coordination').

omega_variable(
    suppression_internalization,
    'To what degree have citizens internalized surveillance as inevitable, reducing perceived suppression while actual structural barriers remain constant?',
    'Comparison of stated suppression perception versus measured behavioral restrictions; longitudinal analysis of resistance/exit attempts; pre- and post-disclosure studies of surveillance scope versus behavior change',
    'If high internalization: measured suppression (0.68) understates actual constraint impact. Victims experience lower suppression than structural analysis suggests. Classification remains Snare but with underestimated coercive power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Degree of internalization of financial surveillance as inevitable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(financial_surveillance_state, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(finsurv_tr_t0, financial_surveillance_state, theater_ratio, 0, 0.42).
narrative_ontology:measurement(finsurv_tr_t10, financial_surveillance_state, theater_ratio, 10, 0.48).
narrative_ontology:measurement(finsurv_tr_t20, financial_surveillance_state, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(finsurv_be_t0, financial_surveillance_state, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(finsurv_be_t10, financial_surveillance_state, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(finsurv_be_t20, financial_surveillance_state, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(financial_surveillance_state, resource_allocation).
narrative_ontology:affects_constraint(financial_surveillance_state, informal_economy_criminalization).
narrative_ontology:affects_constraint(financial_surveillance_state, cryptocurrency_regulatory_capture).
narrative_ontology:affects_constraint(financial_surveillance_state, privacy_rights_erosion).

% DUAL FORMULATION NOTE:
% Financial surveillance state operates at multiple scales with different extractiveness: transaction-level monitoring (ε ≈ 0.45), systemic account-holder suppression (ε ≈ 0.68), geopolitical sanctions coordination (ε ≈ 0.35). This story aggregates at national scope; international coordination and informal economy suppression are structurally distinct constraints with different beneficiary/victim configurations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(financial_surveillance_state, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
