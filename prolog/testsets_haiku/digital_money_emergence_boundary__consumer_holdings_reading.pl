% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__consumer_holdings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_emergence_boundary__consumer_holdings_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: digital_money_emergence_boundary__consumer_holdings_reading
 *   human_readable: Digital Money Consumer Holdings Boundary (1990s–2000s EMD Reading)
 *   domain: monetary_economics/financial_history/regulatory_classification
 *
 * SUMMARY:
 *   The consumer-holdings reading of the digital money emergence boundary
 *   asserts that digital money came into existence when ordinary individuals
 *   could directly hold and transact with digital instruments outside
 *   traditional bank deposit accounts. This reading crystallized in the 1990s
 *   e-purse experiments and formally codified in the EU's 2000 Electronic
 *   Money Directive (EMD). The constraint is the regulatory classification
 *   itself: the definition that money is a digital instrument held by
 *   consumers, separable from bank deposits. This reading produces a
 *   tangled-rope structure: genuine coordination function (enabling fintech,
 *   clarifying monetary aggregates, standardizing licensing) paired with
 *   asymmetric extraction (traditional banks lose deposit monopoly; unbanked
 *   populations face two-tier access; regulators and EMI issuers benefit from
 *   the definitional authority and new market categories). The constraint
 *   activates enforcement through licensing requirements, capital rules, and
 *   the regulatory insistence that EMD be treated distinctly from deposits in
 *   monetary policy and consumer protection frameworks.
 *
 * KEY AGENTS:
 *   - Central banks (ECB, Federal Reserve) — agenda-setters who define and enforce the boundary; benefit from clear monetary aggregates and regulatory authority
 *   - Electronic Money Issuers (fintech, payment operators) — beneficiaries who gain licensing and legitimacy as money issuers under the new category
 *   - Traditional deposit banks — payers who lose the monopoly on consumer money-holding
 *   - Consumers with access — receive payment innovation but bear fragmentation costs
 *   - Unbanked and underbanked populations — bear the cost of two-tier access and exclusion
 *   - Policy economists — analytical observers who measure and critique the boundary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__consumer_holdings_reading, 0.62).
domain_priors:suppression_score(digital_money_emergence_boundary__consumer_holdings_reading, 0.41).
domain_priors:theater_ratio(digital_money_emergence_boundary__consumer_holdings_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__consumer_holdings_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__consumer_holdings_reading, "Digital Money Consumer Holdings Boundary (1990s–2000s EMD Reading)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__consumer_holdings_reading, "monetary_economics/financial_history/regulatory_classification").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__consumer_holdings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__consumer_holdings_reading, '1ca02bbc-acf3-4477-a100-b025dfbd5622').
narrative_ontology:cs_kernel_codification('1ca02bbc-acf3-4477-a100-b025dfbd5622', fixed_text).
narrative_ontology:cs_authority_grounding('1ca02bbc-acf3-4477-a100-b025dfbd5622', extraction).
narrative_ontology:cs_interpretation_layer_present('1ca02bbc-acf3-4477-a100-b025dfbd5622').
narrative_ontology:cs_reading_relation('1ca02bbc-acf3-4477-a100-b025dfbd5622', digital_money_emergence_boundary__infrastructure_reading, influences).
narrative_ontology:cs_reading_relation('1ca02bbc-acf3-4477-a100-b025dfbd5622', digital_money_emergence_boundary__conceptualization_reading, coexists_with).
narrative_ontology:cs_axiom('1ca02bbc-acf3-4477-a100-b025dfbd5622', foundational, money_exists_when_consumers_hold_it).
narrative_ontology:cs_axiom_status(money_exists_when_consumers_hold_it, holdable).
narrative_ontology:cs_axiom_grounding('1ca02bbc-acf3-4477-a100-b025dfbd5622', money_exists_when_consumers_hold_it, conventional).
narrative_ontology:cs_axiom('1ca02bbc-acf3-4477-a100-b025dfbd5622', secondary, regulatory_classification_enables_monetary_policy).
narrative_ontology:cs_axiom_status(regulatory_classification_enables_monetary_policy, holdable).
narrative_ontology:cs_axiom_grounding('1ca02bbc-acf3-4477-a100-b025dfbd5622', regulatory_classification_enables_monetary_policy, instrumental).
narrative_ontology:cs_reference_frame('1ca02bbc-acf3-4477-a100-b025dfbd5622', deposit_monopoly_baseline).
narrative_ontology:cs_drift_state('1ca02bbc-acf3-4477-a100-b025dfbd5622', emd_directive_and_beyond, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('1ca02bbc-acf3-4477-a100-b025dfbd5622', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, central_banks_and_regulators).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, electronic_money_issuers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, traditional_deposit_banks).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, unbanked_and_underbanked_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, consumers_with_access).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, payments_infrastructure_operators).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, consumers_with_access).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__consumer_holdings_reading, monetary_aggregates_require_redefinition).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__consumer_holdings_reading, custody_separation_enables_financial_innovation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define what counts as digital money, codify the boundary at consumer direct-holding, classify EMD as distinct from bank deposits (M4 vs M5), and write regulatory frameworks (EU EMD Directives, 2000 onward) that enforce this classification. Benefit from regulatory clarity and the ability to monitor money supply across new instruments. Control the definitional apparatus that determines which instruments are 'money' and which are mere payment mechanisms.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, central_banks_and_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Licensed by regulators to issue stored-value products and e-purses that consumers can hold directly, outside bank accounts. Gain a new channel for financial products and customer relationships. The boundary classification legitimates their existence as a separate category of financial institution and opens regulatory access (licensing, capital rules, etc.). Benefit from the clarity that they are issuing 'money' rather than mere payment vouchers.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, electronic_money_issuers, beneficiary,
    organized, biographical, mobile, national).

% Lose the monopoly on consumer money-holding. Deposit accounts were the primary retail financial product; the boundary recognition that consumers can hold money outside banks in EMD instruments creates competition for stored value. Must adapt business models or face disintermediation. Pay through erosion of deposit market share and the need to develop EMD products of their own or partner with issuers.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, traditional_deposit_banks, payer,
    institutional, generational, constrained, national).

% Gain the ability to hold money in digital form outside bank accounts, enabling faster payments, portability, and access to fintech services. Also carry the cost of managing multiple payment instruments, learning new platforms, and bearing the risk of non-bank issuers. Benefit from coordination (efficient payments) and choice, but pay through fragmentation and platform switching costs.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, consumers_with_access, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__consumer_holdings_reading, consumers_with_access, payer).

% The boundary definition does not extend traditional deposit protection guarantees to EMD in the same way; regulatory frameworks protect EMD holders (usually middle-income account holders) but leave unbanked populations dependent on informal systems or excluded from the formal EMD ecosystem. The classification of EMD as distinct from deposits can create a two-tier system where access to one form of money versus another is structured by wealth and documentation status. Bear the cost of exclusion or fragmented access.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, unbanked_and_underbanked_populations, payer,
    powerless, biographical, identity_locked, national).

% The boundary definition creates demand for backend infrastructure (card networks, clearing, settlement, e-purse protocols). Processors and network operators benefit from the growth in digital payment volume and the regulatory standardization that comes with EMD classification. They provide the settlement and switching infrastructure that makes the holding and transfer of digital money possible.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, payments_infrastructure_operators, beneficiary,
    powerful, generational, arbitrage, global).

% Monitor the monetary aggregates, study the boundary classification, and publish analyses of whether the distinction holds up empirically. Academic consensus and debate shape how regulators refine the boundary. They see the full structure and can critique whether the consumer-holdings definition captures a real phenomenon or serves interests.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, policy_economists_and_academics, observer,
    analytical, generational, analytical, global).

% Non-bank payment systems, cryptocurrency advocates, and alternative monetary frameworks (local currencies, mutual credit, blockchain systems) that could fulfill money functions but are structurally excluded from the official 'digital money' boundary. Their voices argue for broader definitions or rejection of the regulatory boundary altogether, but are not part of the definitional conversation at central banks and regulatory bodies.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, alternative_payment_systems, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_emergence_boundary__consumer_holdings_reading, central_banks_and_regulators).
narrative_ontology:fixing_cost_class(digital_money_emergence_boundary__consumer_holdings_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared, regulatory definition of what digital money IS (consumer-held, non-deposit instruments) versus what it is NOT (traditional bank deposits, payment vouchers). This enables consistent treatment across jurisdictions, allows money supply measurement and monetary policy to account for new instruments, and lets fintech and traditional finance coordinate on licensing and consumer protection frameworks.
% TRANSFER_FUNCTION: Moves regulatory authority, competitive advantage, and deposit market share from traditional banks to new classes of institutions (EMI, e-money issuers). The definition legitimates fintech entry into money issuance and transfers some of consumers' stored value from bank deposits into EMD products. Also transfers the burden of regulatory compliance and capital holding from banks to new issuers.
% ABSENT_VOICES: Alternative monetary systems and unbanked populations whose exclusion from the formal EMD ecosystem is not audible in central bank policy forums; cryptocurrency proponents who contest the regulatory boundary itself; informal payment systems in developing economies that fulfill money functions outside any formal definition.
% DISAPPEARANCE_RATIONALE: If this boundary definition vanished, monetary aggregates would be ambiguous, regulators would lose the ability to distinguish deposit money from e-money, fintech licensing would collapse (no category to regulate), and markets would reorganize around competing definitions of what counts as money. The 1990s–2000s financial system explicitly adopted this boundary to enable EMD innovation; removing it would require rewriting monetary policy frameworks.
% FOUNDING_PROBLEM: The 1990s e-purse and 2000 EMD technologies created instruments that consumers could hold that looked and functioned like money but did not fit the traditional definition (bank deposits). Regulators and central banks needed a boundary to separate money from payment vouchers, to monitor the money supply, and to enable fintech licensing without treating all digital payment innovations as 'money.'
% FOUNDING_PROBLEM_CORROBORATION: Central banks (ECB, Federal Reserve) attest the founding problem was live and the boundary necessary for monetary policy. Fintech advocates and EMI issuers attest the problem was solved by the 2000 EMD Directive and subsequent regulatory frameworks. Academic economists debate whether the boundary accurately captures the relevant phenomenon or serves regulatory and commercial interests; external sources (IMF, BIS, academic papers on monetary aggregates) document the problem and the contested nature of the solution.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__consumer_holdings_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__consumer_holdings_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__consumer_holdings_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__consumer_holdings_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__consumer_holdings_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_emergence_boundary__consumer_holdings_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_emergence_boundary__consumer_holdings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62 at interval end) because the boundary definition grants regulatory authority and market legitimacy to specific actors (central banks, EMI issuers) while disadvantaging others (traditional banks, unbanked populations). The definition is not a natural fact but a regulatory choice; it could have been drawn differently (e.g., at infrastructure enablement, or at theoretical conceivability). Suppression is moderate (0.41) because the boundary must be actively maintained through licensing requirements and regulatory surveillance — EMD is not self-evidently money; the regulatory system continuously reasserts the distinction. Theater is low-moderate (0.28) because the coordination function (clear monetary definition, standardized licensing) is genuine but grows less visible over time as the regulatory framework is normalized; by 2005, theater stabilizes as the boundary becomes institutional routine. Accessibility collapse is high (0.72) because once the boundary is adopted by central banks, alternative definitions become practically unavailable — the regulatory definition is the only one that matters for licenses, capital rules, and consumer protection. The measured extraction and suppression support a tangled-rope classification: real coordination benefit paired with asymmetric cost distribution and active maintenance.
 *
 * PERSPECTIVAL GAP:
 *   From a central banker's seat, the boundary is genuine coordination — a necessary classification to manage monetary policy in an era of financial innovation. From a traditional bank's seat, it is enforced competition — a regulatory choice that erodes their market position. From an unbanked person's seat, it is irrelevant abstraction — formal money definitions do not apply to their lives because the licensed infrastructure is out of reach. The widest gap is between the analytical seat (policy economists, who can see the boundary as contingent and contestable) and the institutional seat (central banks, who treat the boundary as necessary and permanent).
 *
 * DIRECTIONALITY LOGIC:
 *   Central banks and regulators sit at the beneficiary end (d near 0.0) — they gain definitional authority, the ability to monitor money supply, and clarity for policy. EMI issuers sit at beneficiary (d near 0.15) — they gain market legitimacy and licensing. Traditional banks sit at the target end (d near 0.85) — they lose deposit market share and must adapt to fintech competition. Consumers with access sit at symmetric (d near 0.5) — they gain payment choice and speed, but pay through fragmentation and platform learning. Unbanked populations sit at high-target (d near 0.90) — the boundary definition does not extend protections to them equally and locks them into exclusion through identity-lock mechanisms (documentation requirements, platform bias toward middle-income users, regulatory focus on licensed issuers in formal systems). The engine should compute these differently per seat; the constraint appears as coordination to beneficiaries and as enforced extraction to targets.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the need to classify 1990s e-money technologies so regulators could measure the money supply and license issuers — was live in 1990–2000. By 2003–2005, the problem is contested but arguably partly addressed: the 2000 EMD Directive and subsequent national implementations settled the regulatory boundary. However, the constraint persists beyond that settlement because the boundary now serves secondary interests: it grants regulatory authority, legitimates EMI business models, and structures access to money-holding by income level. The theater ratio remains stable at 0.28 from 2000 onward, suggesting the enforcement machinery has shifted from solving the original problem to maintaining the regulatory category itself. Mandatrophy is not fully resolved (the founding problem is contested, not dead), but the constraint shows signs of inertial persistence — it is maintained partly because the regulatory apparatus that defines it benefits from its existence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_contingency_vs_necessity,
    'Is the consumer-holdings boundary a discovered fact about what digital money IS, or a regulatory choice among multiple defensible definitions?',
    'Comparative analysis of how different jurisdictions and alternative frameworks define digital money; historical analysis of why 1990s regulators chose the consumer-holdings boundary versus alternatives (infrastructure, conceptual, use-case based).',
    'If contingent, the boundary is a constructed constraint benefiting regulators and EMI issuers; if necessary, it is a natural classification. This affects whether the constraint should be classified as tangled_rope (contingent, extractive) or rope (necessary coordination). The measurement of extraction depends on this.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(boundary_contingency_vs_necessity, conceptual, 'Whether the consumer-holdings boundary is a discovered fact or a regulatory choice.').

omega_variable(
    two_tier_access_mechanism,
    'Is the two-tier access system (middle-income consumers in formal EMD, unbanked populations in informal or excluded systems) a structural consequence of the boundary definition, or an incidental feature of implementation?',
    'Study of regulatory frameworks in high-access-equity jurisdictions and low-access-equity jurisdictions; analysis of EMD directive language regarding protection guarantees and documentation requirements; cross-country comparison of unbanked population trends post-EMD.',
    'If structural, the boundary is inherently extractive at the unbanked end and justifies snare classification for that seat; if incidental, the constraint is tangled_rope but policy remedies could extend access without changing the definition. Changes how to model the victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(two_tier_access_mechanism, empirical, 'Whether two-tier access is structural or incidental to the boundary definition.').

omega_variable(
    money_supply_measurement_validity,
    'Does the consumer-holdings boundary produce a valid, measurable distinction between deposit money and e-money, or do consumers and issuers treat them as functionally identical?',
    'Empirical study of substitution patterns between deposits and EMD holdings; central bank data on velocity and behavior divergence; transaction-level studies of how consumers allocate stored value.',
    'If the distinction is functionally irrelevant (consumers treat deposits and EMD identically), the entire coordination rationale for the boundary collapses, and the constraint is pure extraction: regulatory authority and market position creation with no genuine coordination payoff. Reclassifies to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(money_supply_measurement_validity, empirical, 'Whether the consumer-holdings boundary produces functionally meaningful monetary separation.').

omega_variable(
    sibling_reading_contingency,
    'Could this reading coexist with the infrastructure reading or the conceptualization reading within a single regulatory framework, or does each reading''s core premise foreclose the others?',
    'Formal analysis of the definitions: if ''digital money = consumer-held instruments'' and ''digital money = electronically transferred funds'' are both true, the set of digital money would be their intersection (consumer-held AND electronically transferred). If the intersection is empty or the definitions are mutually exclusive, foreclosure applies.',
    'Determines whether the three readings are genuinely coexisting alternatives (coexists_with relation) or whether this reading forecloses the others (forecloses relation). Affects how the constraint family is modeled in the network.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_contingency, conceptual, 'Logical relationship between sibling readings of the digital money emergence boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__consumer_holdings_reading, 1990, 2005).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__consumer_holdings_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(digital_money_emergence_boundary__consumer_holdings_reading, 0.12).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary__infrastructure_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary__conceptualization_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a kernel family: digital_money_emergence_boundary. Three readings coexist in the literature and in regulatory practice: (1) infrastructure_reading — digital money emerged when technology enabled electronic transfer (1967–1977, ATMs/ACH/SWIFT); (2) conceptualization_reading — digital money emerged when theory formalized it (1960s–1985, Chaum, telecommunications); (3) this constraint, consumer_holdings_reading — digital money emerged when individuals could hold it directly (1990s–2000 EMD). Each reading has distinct ε, beneficiary/victim structure, and temporal anchor. The ε-invariance principle requires separate stories: the boundary is drawn in three different places depending on the reading adopted. All three are linked via network.affects_constraints to track the kernel decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_money_emergence_boundary__consumer_holdings_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
