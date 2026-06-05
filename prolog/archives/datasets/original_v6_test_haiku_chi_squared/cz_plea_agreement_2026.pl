% ============================================================================
% CONSTRAINT STORY: cz_plea_agreement_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cz_plea_agreement_2026, []).

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
 *   constraint_id: cz_plea_agreement_2026
 *   human_readable: CZ and Binance Global Regulatory Settlement
 *   domain: economic/political/legal
 *
 * SUMMARY:
 *   The CZ plea agreement and Binance settlement represents a landmark moment
 *   in cryptocurrency regulatory enforcement where the US Treasury and DOJ
 *   extracted explicit jurisdiction assertion, individual guilty plea, and
 *   $4.3B+ penalty from the world's largest cryptocurrency exchange. The
 *   constraint exhibits the core tangled rope structure: genuine coordination
 *   function (establishing global AML compliance standards, reducing systemic
 *   risk, clarifying regulatory expectations) combined with asymmetric
 *   extraction (US regulatory dominance, penalty concentration, competitive
 *   advantage for lighter-touch jurisdictions). The settlement creates a
 *   bifurcated outcome: users bear immediate costs (frozen accounts,
 *   operational disruption, reduced platform functionality), while both the
 *   enforcement apparatus and the compliance consulting sector benefit from
 *   the precedent-setting coordination mechanism. The high theater ratio
 *   (0.64) reflects that enforcement proceedings—the guilty plea, the media
 *   coverage of CEO accountability, the precedent narrative—dominate the
 *   functional AML improvement outcomes. The constraint affects different
 *   agents radically differently: Binance users experience it as a snare
 *   (trapped, no exit without cost); US regulators experience it as rope
 *   (positive coordination with favorable extraction ratios); the DeFi
 *   ecosystem experiences it as mixed (constrained by precedent, but benefits
 *   from reduced Binance competitive dominance). The settlement's temporal
 *   trajectory shows increasing theater ratio and extractiveness as the
 *   enforcement mechanism shifted from negotiation (lower theater) to public
 *   proceedings and compliance ritual (higher theater).
 *
 * KEY AGENTS:
 *   - Binance User Base: Primary victim (powerless/trapped) — face frozen accounts, asset recovery delays, reduced trading access; bear full burden of operational remediation
 *   - Changpeng Zhao (CZ): Individual defendant (institutional/constrained) — guilty plea establishes personal liability precedent; constrains future crypto leadership behavior; simultaneously preserves Binance corporate entity and residual wealth
 *   - US Treasury and DOJ: Primary beneficiary (institutional/arbitrage) — extracts $4.3B+ penalty, establishes jurisdiction over global exchange, creates precedent for CEO personal liability, asserts regulatory authority over cryptocurrency
 *   - Compliance Consulting Sector: Secondary beneficiary (institutional/arbitrage) — benefits from settlement-driven demand for AML infrastructure, legal representation, regulatory architecture
 *   - DeFi Developers and Competitors: Mixed victim/beneficiary (moderate/constrained) — constrained by regulatory precedent and enforcement expectations; benefit from reduced Binance competitive dominance and clarified regulatory framework
 *   - Binance Corporate Entity: Mixed victim/beneficiary (institutional/constrained) — pays penalties and accepts ongoing monitoring; simultaneously gains market legitimacy and reduced pressure from unregulated competitors
 *   - International AML Framework: Institutional actor (institutional/arbitrage) — settlement reaffirms FATF standards and global AML coordination; relies on enforcement theater for legitimacy as underlying transaction-monitoring function has degraded
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cz_plea_agreement_2026, 0.58).
domain_priors:suppression_score(cz_plea_agreement_2026, 0.68).
domain_priors:theater_ratio(cz_plea_agreement_2026, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cz_plea_agreement_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(cz_plea_agreement_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(cz_plea_agreement_2026, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cz_plea_agreement_2026, tangled_rope).
narrative_ontology:human_readable(cz_plea_agreement_2026, "CZ and Binance Global Regulatory Settlement").
narrative_ontology:topic_domain(cz_plea_agreement_2026, "economic/political/legal").

domain_priors:requires_active_enforcement(cz_plea_agreement_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cz_plea_agreement_2026, us_treasury_enforcement).
narrative_ontology:constraint_beneficiary(cz_plea_agreement_2026, compliance_consulting_sector).
narrative_ontology:constraint_victim(cz_plea_agreement_2026, binance_users).
narrative_ontology:constraint_victim(cz_plea_agreement_2026, decentralized_finance_ecosystem).
narrative_ontology:constraint_victim(cz_plea_agreement_2026, cryptocurrency_innovation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BINANCE USER BASE (SNARE) — Users trapped by exchange operational constraints (frozen accounts, asset recovery delays, loss of trading access). No exit without cost; bear full burden of regulatory remediation requirements. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.96.
constraint_indexing:constraint_classification(cz_plea_agreement_2026, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEFI DEVELOPERS / COMPETITORS (TANGLED ROPE) — Constrained by regulatory clarity and enforcement precedent, but also benefit from reduced systemic risk in cryptocurrency markets. Enforcement creates compliance standards that level playing field. d≈0.58, f(d)≈0.72, σ=1.2 → χ≈0.50.
constraint_indexing:constraint_classification(cz_plea_agreement_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: US TREASURY / ENFORCEMENT AGENCIES (ROPE) — Primary beneficiary. Settlement creates enforcement precedent, jurisdiction assertion, and $4.3B+ penalty revenue. Benefits from coordination mechanism: establishes global regulatory framework for cryptocurrency AML compliance. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.06. Net beneficiary structure.
constraint_indexing:constraint_classification(cz_plea_agreement_2026, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMPLIANCE CONSULTING SECTOR (ROPE) — Beneficiaries from settlement-driven demand for AML infrastructure, legal representation, and regulatory consulting. Coordination function: settlement establishes compliance standards that create market for services. d≈0.10, f(d)≈-0.08, σ=1.0 → χ≈-0.05. Positive coordination without extraction.
constraint_indexing:constraint_classification(cz_plea_agreement_2026, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: BINANCE CORPORATE / CZ (TANGLED ROPE) — Constrained by ongoing regulatory conditions (independent monitors, compliance regimes, operational restrictions). Simultaneously benefits from market legitimacy, reduced competitive pressure from unregulated competitors, and continued licensing. Settlement is mix of extraction (penalties, operational constraints) and coordination (market-stabilizing regulatory certainty). d≈0.52, f(d)≈0.68, σ=1.2 → χ≈0.48.
constraint_indexing:constraint_classification(cz_plea_agreement_2026, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL AML FRAMEWORK (PITON) — Settlement relies on coordination mechanism (international AML standards, FATF guidelines) whose primary function has partially atrophied. Theater ratio (0.64) reflects that enforcement theater (litigation, plea deals, media coverage) now dominates over functional AML outcomes. The underlying coordination (information-sharing, transaction monitoring standards) remains, but is maintained largely through enforcement inertia and institutional ritual rather than genuine transaction monitoring efficacy. theater_ratio ≥ 0.70 threshold not met; classified as piton due to degraded functional purpose.
constraint_indexing:constraint_classification(cz_plea_agreement_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational perspective, AML compliance enforcement is presented as an inevitable regulatory necessity (nation-states must prevent money laundering; exchanges must comply with law). But the base properties (ε=0.58, suppression=0.68) contradict the mountain classification. This is a false summit: the 'natural necessity' of enforcement naturalizes what is actually a contingent institutional choice about regulatory intensity, jurisdiction, and penalty structure.
constraint_indexing:constraint_classification(cz_plea_agreement_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cz_plea_agreement_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cz_plea_agreement_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cz_plea_agreement_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cz_plea_agreement_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cz_plea_agreement_2026, TR),
    TR >= 0.70.

:- end_tests(cz_plea_agreement_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58): High-moderate. The settlement extracts significant value from Binance users (operational disruption, asset freeze costs) and from Binance corporate (penalties, compliance costs, competitive constraint). However, extraction is not maximum (which would be ~0.75-0.85) because: (1) Binance remains operational and can rebuild market position; (2) users retain future access; (3) settlement is negotiated rather than forced liquidation. The core extraction is regulatory jurisdictional assertion (US Treasury claims enforcement authority over global exchange) and wealth transfer ($4.3B penalty). Suppression (0.68): High. Significant barriers to exit include: regulatory non-compliance risks, jurisdiction vulnerability, competitive cost of compliance infrastructure, and reputational damage. However, some alternatives exist (offshore platforms, reduced-compliance venues). Theater ratio (0.64): Moderate-high. The enforcement proceedings—guilty plea, CEO accountability narrative, precedent-setting litigation—constitute substantial theatrical performance. The functional AML outcome (whether transaction monitoring actually improved, whether illicit flows were prevented) is less visible. The rising theater ratio over time reflects that enforcement ritual increasingly dominates over genuine compliance improvement as the settlement moved from negotiation to public proceedings to ongoing compliance theater.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates radical perspectival divergence based on structural position. Binance users see extraction (snare): they bear direct operational costs with no agency. US regulators see coordination (rope): the settlement establishes regulatory authority and global AML standards. Competitors see mixed constraint (tangled rope): regulatory precedent increases compliance costs but reduces Binance dominance. Binance corporate sees mixed constraint (tangled rope): penalties and ongoing monitoring constrain operations, but settlement preserves entity and creates legitimacy advantage over unregulated competitors. The compliance consulting sector sees pure benefit (rope): settlement drives demand without equivalent costs. The AML framework from a civilizational perspective risks appearing as mountain (natural necessity of regulatory enforcement), but the high extractiveness and suppression values reveal this as a false summit: enforcement intensity is a contingent institutional choice. The perspectival gaps reflect that the settlement concentrates costs (users, Binance) while distributing benefits (regulators, consulting sector, compliant competitors).
 *
 * DIRECTIONALITY LOGIC:
 *   Binance users: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction direction; no exit options. CZ: Mixed (individual defendant + constrained) → d≈0.58, f(d)≈0.72. High extraction but not maximum; guilty plea preserves some agency and wealth. US Treasury: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; enforcement proceeds with favorable terms. Compliance sector: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.08. Net beneficiary; benefits from coordination mechanism. DeFi competitors: Victim + constrained → d≈0.60, f(d)≈0.75. Constrained by precedent but retain some mobility. Binance corporate: Mixed beneficiary/victim + constrained → d≈0.50, f(d)≈0.65. Balanced extraction and benefit; constrained but not trapped. AML framework: Institutional + arbitrage → d≈0.08, f(d)≈-0.10. Derives benefit from assertion of coordination authority; theater ratio reveals underlying atrophy of functional purpose.
 *
 * MANDATROPHY ANALYSIS:
 *   The settlement avoids pure-snare classification (which would require extractiveness ≥0.66, suppression ≥0.60, and NO coordination function) because it establishes genuine coordination benefits: global AML standards, regulatory clarity, reduced systemic risk. This is authentic tangled rope, not false coordination masking extraction. However, the theater ratio (0.64) and rising trajectory indicate mandate drift: enforcement proceedings increasingly dominate functional AML outcomes. The piton perspective (institutional/civilizational) captures this: the underlying AML coordination mechanism (information-sharing, transaction monitoring standards) persists through enforcement inertia rather than genuine efficacy. From the user perspective (powerless/trapped), the constraint is pure snare: they have no access to coordination benefits, only bear extraction costs. The mandatrophy resolution is that all readings are structurally correct from their respective positions. Users experience snare because they are trapped. Regulators experience rope because they control the mechanism. The constraint's genuine function (AML coordination) exists but is obscured by enforcement theater—the settlement resolves mandatrophy by acknowledging that theatrical suppression (guilty plea, litigation, penalty) now dominates over functional coordination (actual illicit flow prevention), making the system increasingly piton-like even as it preserves formal tangled rope structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aml_effectiveness_vs_enforcement_theater,
    'Does the settlement''s compliance regime actually prevent illicit financial flows or primarily serve enforcement jurisdictional theater?',
    'Post-settlement analysis of illicit transaction detection rates; comparison of pre/post-settlement transaction patterns; audit of independent monitor effectiveness; longitudinal tracking of enforcement outcomes relative to AML efficacy metrics',
    'If effective: settlement is coordination mechanism (Rope from more perspectives). If theatrical: settlement is extraction with reduced functional purpose (Piton or Snare dominates); the high suppression and theater ratio reflect institutional inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aml_effectiveness_vs_enforcement_theater, empirical, 'Whether compliance regime prevents illicit flows or performs enforcement theater').

omega_variable(
    global_regulatory_bifurcation,
    'Does the US-centric enforcement model (DOJ/Treasury-led settlement) create competitive advantage for non-US exchanges operating in jurisdictions with lighter-touch regulation?',
    'Comparative analysis of market share and regulatory cost for US-compliant vs offshore exchanges; correlation between regulatory stringency and user migration; analysis of arbitrage opportunities created by jurisdictional differentiation',
    'If bifurcation occurs: enforcement creates two-tier market (compliant-and-expensive vs unregulated-and-cheap); the settlement''s suppression mechanism drives users to lower-compliance venues. If convergence occurs: settlement creates global regulatory floor. Difference determines whether extraction is concentrated or distributed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(global_regulatory_bifurcation, empirical, 'Whether US enforcement creates regulatory bifurcation favoring offshore exchanges').

omega_variable(
    cz_personal_liability_structural_role,
    'Is CZ''s personal plea agreement driven by genuine individual responsibility for AML failures, or is it a regulatory theater mechanism to establish CEO liability precedent while corporate entity negotiates penalty?',
    'Documentary evidence: internal Binance AML protocols; CZ''s direct involvement in compliance decisions; comparative analysis with other exchange enforcement cases (FTX, Kraken); analysis of whether other executives share liability or responsibility is concentrated on CZ as institutional symbol',
    'If genuine individual liability: constraint is enforcement of personal responsibility (justified extraction). If theatrical precedent: constraint is use of individual plea to legitimize corporate penalty; CZ absorbs personal liability narrative while institutional structures remain largely unchanged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cz_personal_liability_structural_role, conceptual, 'Whether CZ''s personal liability is genuine responsibility or regulatory theater').

omega_variable(
    settlement_payment_destination_efficacy,
    'Does the $4.3B+ settlement payment actually fund AML infrastructure improvements, or primarily redirect cryptocurrency proceeds to general Treasury revenue?',
    'Budget analysis: tracking of settlement funds allocation; measurement of AML infrastructure spending pre/post-settlement; assessment of whether funds address root AML failures or serve as penalty revenue',
    'If infrastructure-focused: settlement is coordination investment (resources improve system). If revenue-focused: settlement is pure extraction (penalty collection with minimal functional improvement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_payment_destination_efficacy, empirical, 'Whether settlement funds improve AML infrastructure or serve as revenue').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cz_plea_agreement_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(czplea_tr_t0, cz_plea_agreement_2026, theater_ratio, 0, 0.48).
narrative_ontology:measurement(czplea_tr_t3, cz_plea_agreement_2026, theater_ratio, 3, 0.58).
narrative_ontology:measurement(czplea_tr_t6, cz_plea_agreement_2026, theater_ratio, 6, 0.64).

% Extraction over time
narrative_ontology:measurement(czplea_be_t0, cz_plea_agreement_2026, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(czplea_be_t3, cz_plea_agreement_2026, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(czplea_be_t6, cz_plea_agreement_2026, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cz_plea_agreement_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(cz_plea_agreement_2026, cryptocurrency_regulatory_jurisdiction).
narrative_ontology:affects_constraint(cz_plea_agreement_2026, aml_compliance_infrastructure).
narrative_ontology:affects_constraint(cz_plea_agreement_2026, exchange_systemic_risk).

% DUAL FORMULATION NOTE:
% The CZ plea agreement is downstream of broader cryptocurrency regulatory jurisdiction assertions and upstream of industry-wide AML compliance architecture. The settlement consolidates US Treasury enforcement authority (established in upstream constraints) into specific exchange penalties and personal liability mechanisms (this story), which in turn create precedent for downstream regulatory cascades affecting all platforms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cz_plea_agreement_2026, institutional, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
