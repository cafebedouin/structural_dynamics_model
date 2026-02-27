% ============================================================================
% CONSTRAINT STORY: horizon_liability_contract
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_horizon_liability_contract, []).

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
 *   constraint_id: horizon_liability_contract
 *   human_readable: Post Office Horizon Contractual Liability
 *   domain: economic/technological/legal
 *
 * SUMMARY:
 *   The Post Office Horizon contractual liability system represents a
 *   structural extraction mechanism disguised as a coordination contract.
 *   Beginning in 1999, the Post Office mandated that 11,500+ sub-postmasters
 *   use the faulty Horizon IT system while accepting personal liability for
 *   all reported financial discrepancies. Over more than a decade, the
 *   Horizon system generated false accounting shortfalls (due to software
 *   bugs, system crashes, and data corruption). Sub-postmasters were held
 *   personally and legally responsible for amounts they did not actually
 *   lose, forcing them to pay shortfalls from personal savings or face
 *   criminal prosecution. The constraint combines high base extractiveness
 *   (0.68) with extreme suppression (0.82): sub-postmasters were trapped by
 *   employment contracts, possessed no alternative system, could not opt out
 *   without losing their livelihoods, and lacked technical knowledge or
 *   resources to audit or challenge system-reported discrepancies. The
 *   theater ratio (0.55) reflects that Post Office enforcement proceedings
 *   maintained legal theater (formal charges, court trials) while the
 *   fundamental cause (system defect) remained concealed and unaddressed.
 *   This constraint shifted from low-extraction coordination (early years,
 *   when system appeared functional) to high-extraction snare (later years,
 *   as defects accumulated and Post Office knowledge grew while
 *   accountability transfers remained intact).
 *
 * KEY AGENTS:
 *   - Sub-Postmasters: Primary victims (powerless/trapped) — forced to bear personal and criminal liability for system failures; experienced extractiveness includes financial ruin, home repossession, incarceration, and reputational destruction
 *   - Sub-Postmaster Families: Secondary victims (powerless/trapped) — generational extraction through household financial ruin and social capital destruction
 *   - Post Office Corporation: Primary beneficiary (institutional/arbitrage) — transferred all customer-facing risk to sub-postmasters while maintaining network revenue; could renegotiate or abandon system at any time
 *   - Fujitsu Technology Provider: Secondary beneficiary (institutional/arbitrage) — shielded from liability for defective product; captured revenue while Post Office bore reputational and financial cost
 *   - UK Regulatory and Legal System: Institutional enforcer (institutional/constrained) — maintained performative compliance and legal proceedings against sub-postmasters while institutional path dependency prevented exit from established enforcement regime
 *   - Sub-Postmaster Coalitions: Organized resistance (organized/mobile) — built collective legal action and public narrative pressure; experienced extraction through coordination costs but gained exit options through collective mobilization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(horizon_liability_contract, 0.68).
domain_priors:suppression_score(horizon_liability_contract, 0.82).
domain_priors:theater_ratio(horizon_liability_contract, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(horizon_liability_contract, extractiveness, 0.68).
narrative_ontology:constraint_metric(horizon_liability_contract, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(horizon_liability_contract, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(horizon_liability_contract, snare).
narrative_ontology:human_readable(horizon_liability_contract, "Post Office Horizon Contractual Liability").
narrative_ontology:topic_domain(horizon_liability_contract, "economic/technological/legal").

domain_priors:requires_active_enforcement(horizon_liability_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(horizon_liability_contract, post_office_corporation).
narrative_ontology:constraint_beneficiary(horizon_liability_contract, fujitsu_technology_provider).
narrative_ontology:constraint_victim(horizon_liability_contract, sub_postmasters).
narrative_ontology:constraint_victim(horizon_liability_contract, sub_postmaster_families).
narrative_ontology:constraint_victim(horizon_liability_contract, postal_network_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUB-POSTMASTER (SNARE) — Trapped by contractual obligation to use faulty Horizon system. Contractual liability clause forces personal liability for system-generated discrepancies. No exit without financial ruin or criminal prosecution. Maximum extraction: bears full cost of system failures, reduced income, legal defense costs, and reputational destruction. Zero degrees of freedom.
constraint_indexing:constraint_classification(horizon_liability_contract, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SUB-POSTMASTER FAMILIES (SNARE) — Financial ruin cascades across household: loss of primary income, savings liquidated for legal defense, home repossession. Children's education disrupted. No exit from systemic poverty once accused. Generational extraction: family social capital destroyed.
constraint_indexing:constraint_classification(horizon_liability_contract, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: POST OFFICE CORPORATION (ROPE) — Benefits from contractual liability transfer to sub-postmasters. Experiences constraint as coordination mechanism: standardized IT system enables network efficiency and accountability transfer. Can exit or renegotiate at will (institutional power/arbitrage). Net extraction flows toward this agent. Effective suppression is institutional — backed by legal enforcement and reputational authority.
constraint_indexing:constraint_classification(horizon_liability_contract, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FUJITSU TECHNOLOGY PROVIDER (ROPE) — Benefits from contractual structure that shields them from liability for Horizon defects. Post Office bears all customer-facing risk; Fujitsu captures revenue and maintains distance from failures. Arbitrage exit: can renegotiate terms, abandon product line, or rebrand. Effective extraction flows toward vendor.
constraint_indexing:constraint_classification(horizon_liability_contract, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY AND LEGAL SYSTEM (PITON) — Enforcement mechanism for contractual liability persists despite known system failures. Theater_ratio high: legal proceedings continue against accused sub-postmasters while system defects are documented. Regulatory bodies (Post Office, Companies House) maintain performative compliance checks. The constraint survives through institutional inertia and legal theater — formal rules remain in place despite loss of legitimacy. Regulatory exit is constrained by path dependency and institutional authority.
constraint_indexing:constraint_classification(horizon_liability_contract, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: SUB-POSTMASTER COALITIONS (TANGLED ROPE) — Organized groups (Justice for Sub-Postmasters, media campaigns, legal collectives) experience a mixed constraint. Coordination benefit: collective legal action and public narrative shape pressure. Active enforcement requirement: must organize, litigate, and maintain media visibility to counter Post Office authority. Significant extraction: legal costs, time burden, emotional labor. Exit options are mobile for organized actors — can shift strategy, escalate to public inquiry, or appeal to political actors. Classification reflects real coordination function (coalition building) alongside genuine extraction (institutional power asymmetry).
constraint_indexing:constraint_classification(horizon_liability_contract, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From civilizational/universal scope, this constraint represents a structural failure of technological accountability: the contractual liability shift from technology provider to individual sub-postmasters inverts normal product liability. The system exhibits maximum suppression (legal enforcement, institutional authority, asymmetric information about Horizon defects) combined with high extraction (personal financial and legal ruin). Theater_ratio moderate (some performative legal proceedings; some genuine technical failures documented). The analytical view confirms snare: systematic extraction from trapped powerless agents backed by institutional suppression.
constraint_indexing:constraint_classification(horizon_liability_contract, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(horizon_liability_contract_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(horizon_liability_contract, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(horizon_liability_contract, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(horizon_liability_contract, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(horizon_liability_contract, TR),
    TR >= 0.70.

:- end_tests(horizon_liability_contract_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68): High. The sub-postmaster bears contractual liability for system-generated discrepancies with no compensation mechanism or dispute resolution process. Career income was contingent on bearing these costs. The extraction flow is unidirectional from sub-postmaster to Post Office (via reduced competition, controlled labor, risk transfer). The value reflects that this is not maximum extraction (0.80+) because some sub-postmasters did eventually organize legal challenges and regulatory pressure, creating some contestation. Suppression (0.82): Very high. Sub-postmasters faced multiple suppression mechanisms: contractual non-negotiability, technical unauditability of Horizon system, criminal prosecution for alleged shortfalls, institutional authority of Post Office narrative, and resource asymmetry (Post Office legal team vs. individual sub-postmaster). Alternative exit paths were systematically blocked: quitting meant losing franchise, challenging the system meant facing criminal charges, seeking external audit meant violating confidentiality clauses. Theater ratio (0.55): Moderate. Enforcement proceedings maintained legal theater (formal trials, criminal charges) while the actual cause (Horizon defects) remained officially unaddressed and concealed. Some theater (the performative legal machinery) but not maximum theater, because the extraction mechanism was partially functional in its core purpose (transferring risk to sub-postmasters). As defects accumulated and became harder to conceal, theater ratio remained stable because enforcement only intensified.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how indexical context determines classification without changing the underlying structural facts. The same contractual liability mechanism appears as: rope (Post Office view: coordination mechanism for network efficiency); snare (sub-postmaster view: systematic extraction with zero exit options); piton (regulatory view: performative legal theater masking institutional failure); and tangled rope (organized coalitions: mixed coordination and extraction, with emerging exit options through collective action). None of these classifications is 'more correct' — they are all observations from different structural positions. The mandatrophy resolution is structural: the constraint persists because it benefits institutional actors (Post Office, Fujitsu) and is enforced by institutional actors (regulators, legal system) against powerless individual targets. The perspectival diversity is not ambiguity — it is the complete picture of how the constraint functions across the social hierarchy.
 *
 * DIRECTIONALITY LOGIC:
 *   Sub-postmasters derive d = 0.95 (full target) from their trapped exit + victim status + powerless position. Post Office derives d = 0.05 (full beneficiary) from their arbitrage exit + beneficiary status + institutional power. The sigmoid f(d) maps these to f(0.95) ≈ 1.42 and f(0.05) ≈ -0.12, producing high and negative effective extractiveness respectively. The sub-postmaster experiences maximum chi; the Post Office experiences negative chi (the constraint subsidizes their operations). The directionality derivation confirms the snare classification: one agent at maximum extraction, the other capturing full benefit, with zero degrees of freedom for the extracted agent to renegotiate or exit. No overrides are needed; the structural data automatically generates the correct directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL MANDATROPHY CASE: This constraint illustrates why indexical classification prevents mislabeling extraction as coordination. Early framings of Horizon described it as a 'coordination mechanism' for network IT standardization — a rope. This framing was institutionally convenient (minimized Post Office accountability) and technically appealing (standardization does enable coordination). The snare classification emerges only when you shift to the powerless victim perspective and ask: 'What are this agent's exit options?' The answer is trapped + liable = snare. The mandatrophy resolution: the constraint is BOTH a coordination mechanism (from Post Office perspective) AND an extraction mechanism (from sub-postmaster perspective) simultaneously. The key to breaking the extraction is to recognize that what appears as 'coordination' from the beneficiary's view is actually 'transfer of all risk to powerless agents' — a classic hybrid that leans heavily toward snare because exit is trapped. The engine's requirement to declare beneficiary AND victim, combined with the exit options indexical, prevents the false natural law framing ('IT standardization requires someone to bear liability') that would elevate snare to mountain. The facts are clear: the liability could have been distributed differently, shared with Fujitsu, or pooled with Post Office; that it concentrated entirely on sub-postmasters is a policy choice, not a natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    system_defect_knowledge_timeline,
    'When did Post Office and Fujitsu leadership know that Horizon system generated false discrepancies, and did they deliberately conceal this from sub-postmasters and regulators?',
    'Email forensics, internal Post Office investigation, witness testimony, document discovery from litigation, Fujitsu internal communications',
    'If deliberate concealment confirmed: snare classification strengthened (suppression was active deceit, not just contractual asymmetry). If genuine mutual ignorance: classification shifts toward tangled_rope (coordination failure, not pure extraction). If leadership knew but Sub-Post Office internal audit teams were deceived: intermediate extraction model.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(system_defect_knowledge_timeline, empirical, 'Timeline of knowledge about Horizon defects within Post Office and Fujitsu').

omega_variable(
    contractual_liability_negotiation_asymmetry,
    'Could sub-postmasters have individually negotiated liability terms with Post Office, or was the contract genuinely non-negotiable for all sub-postmasters?',
    'Analysis of contract offer process, historical records of sub-postmaster disputes over terms, comparison with other national postal services'' liability structures, interviews with network managers about negotiation flexibility',
    'If terms were non-negotiable: confirms trapped exit and maximum suppression. If some sub-postmasters negotiated alternatives: exit was constrained rather than trapped, potentially reducing snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contractual_liability_negotiation_asymmetry, empirical, 'Whether sub-postmasters had any meaningful negotiation over liability terms').

omega_variable(
    technical_auditability_of_horizon,
    'Was the Horizon system technically auditable by sub-postmasters or their accountants, or did the contractual and technical structure systematically prevent independent verification of discrepancies?',
    'Technical audit of Horizon code, review of sub-postmaster audit rights in contracts, analysis of whether independent auditors could identify system-generated vs. user-generated discrepancies',
    'If systematically unauditable: suppression includes technical architecture designed to prevent verification (maximum extraction). If theoretically auditable but sub-postmasters lacked resources: suppression is resource-based, not structural (slightly lower extraction rating).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technical_auditability_of_horizon, empirical, 'Whether Horizon was technically auditable by sub-postmasters or their representatives').

omega_variable(
    regulatory_capture_of_post_office,
    'Did Post Office''s regulatory oversight (Companies House, Department for Business) function as genuine external constraint, or was regulatory authority captured by Post Office''s institutional position and narrative control?',
    'Analysis of regulatory inspections prior to scandal emergence, communication patterns between regulators and Post Office, timing of regulatory concerns relative to internal Post Office documentation of Horizon issues',
    'If captured: regulatory theater component increases, piton classification strengthened. If genuinely independent: piton shifts toward tangled_rope (enforcement with genuine external check).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_of_post_office, conceptual, 'Whether regulatory oversight functioned as independent constraint or was captured').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(horizon_liability_contract, 1999, 2010).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(horiz_tr_t1999, horizon_liability_contract, theater_ratio, 1999, 0.3).
narrative_ontology:measurement(horiz_tr_t2005, horizon_liability_contract, theater_ratio, 2005, 0.45).
narrative_ontology:measurement(horiz_tr_t2010, horizon_liability_contract, theater_ratio, 2010, 0.55).

% Extraction over time
narrative_ontology:measurement(horiz_be_t1999, horizon_liability_contract, base_extractiveness, 1999, 0.35).
narrative_ontology:measurement(horiz_be_t2005, horizon_liability_contract, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(horiz_be_t2010, horizon_liability_contract, base_extractiveness, 2010, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(horizon_liability_contract, resource_allocation).
narrative_ontology:affects_constraint(horizon_liability_contract, postal_network_franchising_model).
narrative_ontology:affects_constraint(horizon_liability_contract, uk_it_procurement_liability_transfer).

% DUAL FORMULATION NOTE:
% The Horizon liability contract is downstream of the Post Office franchising model (which created powerless individual sub-postmasters) and the UK IT procurement norm of vendor liability transfer to customers. These upstream constraints establish the structural conditions that made the Horizon liability snare possible. The dual formulation distinguishes the specific Horizon contractual mechanism (this story) from the broader franchising exploitation model (upstream) and IT liability norms (upstream).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
