% ============================================================================
% CONSTRAINT STORY: cryptographic_custody_risk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cryptographic_custody_risk, []).

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
 *   constraint_id: cryptographic_custody_risk
 *   human_readable: Cryptographic Custody Risk and Key Management Asymmetry
 *   domain: digital_security/cryptography/institutional_dependency
 *
 * SUMMARY:
 *   Cryptographic custody risk creates a structural asymmetry between asset
 *   holders who require key management and custodial institutions that
 *   control access to those keys. The constraint exhibits all six DR types
 *   from different perspectives, revealing how the same technical risk is
 *   experienced as irreducible natural law by some observers, pure
 *   coordination by others, and extractive lock-in by still others. The
 *   technical fact — private key loss results in permanent asset loss —
 *   creates genuine operational risk. But the institutional arrangement —
 *   mandatory delegation to custodians, regulatory lock-in, and information
 *   asymmetry about key handling practices — layers extraction on top of this
 *   technical necessity. The constraint's theater_ratio (0.48) reflects that
 *   custody regulations serve partly as genuine risk management and partly as
 *   competitive moat that excludes non-custodial alternatives and legitimates
 *   institutional dependency.
 *
 * KEY AGENTS:
 *   - Individual Asset Holders: Primary victims (powerless/trapped) — structurally dependent on custodians due to technical complexity and regulatory requirements; bear full loss if custodian fails
 *   - Technically Sophisticated Users: Secondary victims (moderate/constrained) — can self-custody but face operational burden; benefit from custody infrastructure but constrained by fees and lock-in
 *   - Custodial Institutions: Primary beneficiaries (institutional/arbitrage) — capture fees and network effects; can arbitrage between regulatory jurisdictions and exit if necessary
 *   - Institutional Investors: Powerful constrained (powerful/constrained) — derive genuine coordination benefit from custody aggregation but experience asymmetric extraction via fees and regulatory lock-in
 *   - Regulatory Framework: Theater maintainers (institutional/constrained) — regulations persist through inertia; serve dual function as risk management and competitive moat
 *   - Non-Custodial Alternative Coalition: Organized agents (organized/mobile) — building technical pathways that reduce institutional dependency; represent sunset mechanism for the constraint
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent cryptographic limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cryptographic_custody_risk, 0.58).
domain_priors:suppression_score(cryptographic_custody_risk, 0.72).
domain_priors:theater_ratio(cryptographic_custody_risk, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cryptographic_custody_risk, extractiveness, 0.58).
narrative_ontology:constraint_metric(cryptographic_custody_risk, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(cryptographic_custody_risk, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cryptographic_custody_risk, tangled_rope).
narrative_ontology:human_readable(cryptographic_custody_risk, "Cryptographic Custody Risk and Key Management Asymmetry").
narrative_ontology:topic_domain(cryptographic_custody_risk, "digital_security/cryptography/institutional_dependency").

domain_priors:requires_active_enforcement(cryptographic_custody_risk).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cryptographic_custody_risk, custodial_institutions).
narrative_ontology:constraint_beneficiary(cryptographic_custody_risk, key_management_vendors).
narrative_ontology:constraint_victim(cryptographic_custody_risk, asset_holders).
narrative_ontology:constraint_victim(cryptographic_custody_risk, cryptographic_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL ASSET HOLDER (SNARE) — Structurally trapped by the requirement to delegate custody of cryptographic keys to institutional intermediaries due to technical complexity, operational risk, and social pressure. Cannot exit without abandoning asset access entirely. Bears full suppression cost: key loss, institutional default, or regulatory seizure — all create total asset loss with no recourse. Maximum experienced extraction.
constraint_indexing:constraint_classification(cryptographic_custody_risk, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TECHNICALLY SOPHISTICATED USER (TANGLED ROPE) — Constrained by operational burden of self-custody (hardware wallet management, backup procedures, social engineering risk). Genuine coordination benefit exists: custody infrastructure reduces individual operational failure. But asymmetric extraction persists: institutional fee capture, regulatory lock-in (forced to use licensed custodians for accounts), and information asymmetry (users cannot audit key handling practices). Moderate exit cost — can self-custody but faces ongoing vigilance burden.
constraint_indexing:constraint_classification(cryptographic_custody_risk, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CUSTODIAL INSTITUTION (ROPE) — Experiences the constraint as pure coordination: managing custody infrastructure for heterogeneous clients solves a genuine problem (operational risk aggregation, insurance pooling, regulatory compliance). Benefits from fee capture and network effects. Has arbitrage options: can exit by shutting down, relocating, or adopting new protocols. Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(cryptographic_custody_risk, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY FRAMEWORK (PITON) — Custody regulations (AML, KYC, segregated asset requirements) were designed to prevent fraud and money laundering but now function primarily as theater that legitimates institutional custody and creates barrier to entry for non-custodial alternatives. The theater has increased as regulatory burden has escalated without demonstrable improvement in security outcomes. Regulations persist through inertia even as their original function has atrophied — alternatives (decentralized custody, threshold cryptography, on-chain governance) operate outside the regulatory frame entirely.
constraint_indexing:constraint_classification(cryptographic_custody_risk, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL INVESTOR (TANGLED ROPE) — Large allocators are constrained by fiduciary duty, audit requirements, and institutional risk frameworks that mandate third-party custody for regulatory acceptance. Genuine coordination benefit: custody aggregation enables risk pooling, insurance, and operational efficiency. But asymmetric extraction persists: fee capture, lock-in through regulatory requirements, and information asymmetry about key handling. Powerful agent with constrained exit options — can negotiate terms but cannot fully exit without abandoning asset class.
constraint_indexing:constraint_classification(cryptographic_custody_risk, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: NON-CUSTODIAL ALTERNATIVE COALITION (SCAFFOLD) — Organized developers (threshold signature protocols, multi-sig wallets, decentralized custody networks, hardware wallet manufacturers) are building technical pathways that reduce dependency on institutional custodians. These alternatives have genuine sunset logic: as threshold cryptography becomes more accessible, as multi-sig standards mature, and as non-custodial infrastructure improves, the structural dependency on institutional custody diminishes. Mobile exit options become available as technical barriers lower. The constraint's extraction mechanism loses force as alternatives proliferate.
constraint_indexing:constraint_classification(cryptographic_custody_risk, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some custody risk is inherent to cryptographic asset management: key loss results in permanent asset loss by mathematical necessity, and the complexity of key management creates irreducible operational risk. This perspective sees custody dependency as a natural law of cryptographic systems — inescapable without violating the mathematical properties that make the system secure. However, the structural data reveals false summit: the constraint includes significant institutional, regulatory, and fee-capture components that are contingent, not inherent.
constraint_indexing:constraint_classification(cryptographic_custody_risk, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cryptographic_custody_risk_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cryptographic_custody_risk, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cryptographic_custody_risk, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cryptographic_custody_risk, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cryptographic_custody_risk, TR),
    TR >= 0.70.

:- end_tests(cryptographic_custody_risk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Institutional custodians capture significant fees (0.5%-2% annually) and benefit from regulatory lock-in that prevents non-custodial alternatives from reaching parity. However, extraction is not maximal because genuine coordination benefits exist: custody aggregation enables insurance pooling, operational risk reduction, and regulatory compliance that individual asset holders cannot achieve alone. The extractiveness has increased over the interval (0.35 → 0.58) as regulatory requirements have tightened, forcing more agents into custodial dependency and raising barriers to non-custodial alternatives. Suppression (0.72): High. Structural barriers to exit include: (1) technical complexity of key management, (2) regulatory requirements that mandate custodial institutions for fiduciary accounts and regulated entities, (3) social pressure that positions self-custody as irresponsible, (4) operational friction (insurance, audit trails) that custodial institutions provide. Suppression is high but not total — some agents can and do self-custody, and regulatory requirements vary by jurisdiction. Theater ratio (0.48): Moderate. Custody regulations (AML/KYC, segregated assets, capital adequacy) serve dual functions: genuine risk management and competitive moat. The theater has increased as regulatory burden has escalated without proportional improvement in security outcomes — regulations now primarily serve to legitimize institutional custody and exclude non-custodial alternatives, rather than to prevent actual fraud.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates significant perspectival divergence. The custodial institution sees pure coordination (Rope) — managing custody solves a genuine problem of operational risk aggregation. The non-custodial coalition sees temporary extraction with a sunset (Scaffold) — technical alternatives are maturing and will reduce institutional dependency. The regulatory framework sees its own degraded function (Piton) — custody rules persist through inertia and competitive lock-in rather than proven security benefit. Technically sophisticated users see mixed coordination and extraction (Tangled Rope) — custody infrastructure provides genuine benefit but with asymmetric fee capture. Individual asset holders see extractive trap (Snare) — they have no real exit option and bear full loss if custodian fails. The analytical observer risks seeing a natural law (Mountain) — key loss results in permanent asset loss by mathematics — but the structural data reveals false summit: the constraint includes significant contingent institutional and regulatory components.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) reflects the agent's structural position relative to extraction flow. Individual asset holders are trapped victims with no exit options and no ability to organize collectively (powerless/trapped) — d approaches 1.0, producing maximum experienced extraction. Technically sophisticated users are constrained victims who can exit but at high operational cost (moderate/constrained) — d ≈ 0.70. Custodial institutions are beneficiaries with arbitrage options who can exit the market if necessary (institutional/arbitrage) — d approaches 0.0, experiencing negative effective extraction (the constraint subsidizes them). Institutional investors are powerful but constrained by fiduciary duty and regulatory requirements (powerful/constrained) — d ≈ 0.55, experiencing moderate extraction. The regulatory framework appears as institutional actor with constrained options (institutional/constrained) — d ≈ 0.50, but derives classification from theater ratio rather than from directionality. Non-custodial coalition are organized agents with mobile exit options who are building alternatives (organized/mobile) — d approaches 0.30, experiencing low extraction because they have real agency to bypass the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves by distinguishing technical necessity (inescapable custody risk inherent to cryptography) from institutional arrangement (mandatory delegation to custodians with asymmetric information and fee capture). The technical layer is rope-adjacent (genuine coordination to manage operational risk). The institutional layer is snare-adjacent (regulatory lock-in, fee extraction, no real exit options for powerless agents). The constraint's tangled rope classification reflects the hybrid: genuine coordination function (custody aggregation reduces operational risk) coupled with asymmetric extraction (institutional capture of fees and regulatory advantages). The false mountain perspective reveals that the 'inherent limitation' framing naturalizes what is actually a choice about institutional design. Threshold cryptography, multi-sig, and decentralized custody protocols demonstrate that the technical risk can be managed without institutional intermediaries — the extraction component depends on maintaining regulatory barriers and operational complexity that make non-custodial alternatives appear riskier than they are.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technical_competence_distribution,
    'What proportion of asset holders could realistically self-custody with acceptable operational risk given sufficient tooling and education?',
    'Empirical studies of self-custody failure rates (key loss, accidental exposure, social engineering) across different user populations; comparison to institutional custody failure rates',
    'If >50% could self-custody: trapped exit option is misclassified, constraint is weaker than measured. If <10% could self-custody: technical barriers are the binding constraint, not institutional lock-in. If 20-40%: institutional dependency is genuine for most agents but not universal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_competence_distribution, empirical, 'Distribution of technical competence required for self-custody').

omega_variable(
    regulatory_mandate_necessity,
    'Do institutional custody requirements (AML/KYC, segregated assets, capital adequacy) actually reduce systemic risk or primarily serve as regulatory theater that legitimates institutional intermediation?',
    'Comparative analysis of institutional custody failure rates vs non-custodial protocols; examination of whether regulatory requirements correlate with improved security outcomes or primarily with reduced competition',
    'If regulations improve outcomes: they represent genuine coordination, constraint is rope from regulatory perspective. If regulations are primarily theater: they represent extractive lock-in, constraint is snare from regulatory perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_mandate_necessity, empirical, 'Whether custody regulations improve security outcomes or serve as competitive moat').

omega_variable(
    technical_maturity_of_alternatives,
    'Are threshold cryptography, multi-sig standards, and decentralized custody protocols technically mature enough to replace institutional custody at scale, or are they still in development with unresolved failure modes?',
    'Security audits of non-custodial protocols; adoption rates among institutional investors; incident analysis comparing self-custody losses to institutional custody losses at comparable scales',
    'If mature: scaffold sunset is real and the constraint will degrade as alternatives proliferate. If immature: alternatives pose new risks without eliminating institutional dependency, and the constraint''s structure persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_maturity_of_alternatives, empirical, 'Technical maturity of non-custodial custody alternatives').

omega_variable(
    fee_extraction_vs_coordination_cost,
    'What proportion of institutional custody fees represents genuine operational cost (insurance, compliance, infrastructure) vs rent extraction (lock-in, information asymmetry, network effects)?',
    'Cost accounting analysis of custody operations; comparison of fee levels across competitive markets vs regulated monopolies; analysis of fee sustainability if non-custodial alternatives reach parity in convenience',
    'If >70% operational: extraction component is smaller than measured, constraint is more rope-like. If <30% operational: extraction component is larger, constraint is more snare-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fee_extraction_vs_coordination_cost, empirical, 'Decomposition of custody fees into operational cost vs rent extraction').

omega_variable(
    asset_holder_preference_vs_forced_custody,
    'How much of the institutional custody dependency reflects genuine user preference for delegation vs regulatory coercion?',
    'Surveys of custody users in regulated vs unregulated environments; comparison of self-custody adoption rates before and after regulatory restrictions; analysis of users who maintain both custodial and self-custody positions',
    'If preference-driven: suppression is lower than measured, agents have more agency than trapped classification suggests. If coercion-driven: suppression is correctly measured, trapped is appropriate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(asset_holder_preference_vs_forced_custody, preference, 'Whether custody dependency reflects genuine preference or regulatory coercion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cryptographic_custody_risk, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(crypt_cust_tr_t0, cryptographic_custody_risk, theater_ratio, 0, 0.3).
narrative_ontology:measurement(crypt_cust_tr_t3, cryptographic_custody_risk, theater_ratio, 3, 0.38).
narrative_ontology:measurement(crypt_cust_tr_t6, cryptographic_custody_risk, theater_ratio, 6, 0.45).
narrative_ontology:measurement(crypt_cust_tr_t10, cryptographic_custody_risk, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(crypt_cust_be_t0, cryptographic_custody_risk, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(crypt_cust_be_t3, cryptographic_custody_risk, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(crypt_cust_be_t6, cryptographic_custody_risk, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(crypt_cust_be_t10, cryptographic_custody_risk, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cryptographic_custody_risk, resource_allocation).
narrative_ontology:affects_constraint(cryptographic_custody_risk, regulatory_lock_in_fintech).
narrative_ontology:affects_constraint(cryptographic_custody_risk, institutional_information_asymmetry).
narrative_ontology:affects_constraint(cryptographic_custody_risk, decentralized_custody_infrastructure).

% DUAL FORMULATION NOTE:
% Cryptographic custody risk decomposes into three structurally distinct constraints: (1) technical custody risk (asset loss from key management failure), (2) institutional custody dependency (regulatory and operational lock-in to custodial intermediaries), and (3) information asymmetry (asset holders cannot audit custodian key handling practices). This story focuses on the constraint that combines all three — the institutional arrangement that makes custody dependency structurally necessary. Alternative stories would isolate the technical layer (lower ε, more rope-like) and the information asymmetry layer (higher ε, more snare-like).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cryptographic_custody_risk, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
