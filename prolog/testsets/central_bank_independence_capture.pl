% ============================================================================
% CONSTRAINT STORY: central_bank_independence_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_central_bank_independence_capture, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: central_bank_independence_capture
 *   human_readable: Central Bank Independence Regulatory Capture
 *   domain: monetary_policy/institutional_capture
 *
 * SUMMARY:
 *   Central bank independence represents a constraint that combines genuine
 *   coordination (insulating monetary policy from short-term political cycles
 *   enables credibility-building) with extractive capture (subordinating full
 *   employment and labor market welfare to price stability targets that
 *   benefit creditors and asset holders). The constraint operates through
 *   active institutional enforcement (central bank charters,
 *   inflation-targeting frameworks, regulatory dominance by independent
 *   central banks) while maintaining a theater of technocratic neutrality.
 *   The measurement data shows extractiveness increasing from 0.35 to 0.58
 *   over a 30-year interval, paralleled by rising theater_ratio (0.52 to
 *   0.68), indicating that the coordination function is degrading while
 *   performative aspects intensify — a signature of Goodhart drift and
 *   institutional Pitonization. The constraint exhibits six distinct
 *   classifications depending on observational position: labor market
 *   constituencies experience it as a snare (trapped, no exit, bearing full
 *   cost); financial sector incumbents experience it as coordination
 *   mechanism (rope); central bank institutions experience their own
 *   independence as coordination (rope); regulatory agencies are
 *   identity-locked into the independence frame (tangled rope via cognitive
 *   capture); democratic accountability is substantially degraded (piton);
 *   and the analytical observer risks a false mountain (naturalizing
 *   contingent institutional arrangements as laws of economics). The
 *   mandatrophy is resolved by recognizing that all six readings are valid
 *   from their respective structural positions; the constraint's true nature
 *   is revealed through the perspectival gap and the increasing theater_ratio
 *   — institutional capture maintained through doctrinal dominance rather
 *   than functional necessity.
 *
 * KEY AGENTS:
 *   - Labor Market Constituency: Primary victim (powerless/trapped) — workers and savers bear the welfare cost of restrictive monetary policy; no exit mechanism at national level
 *   - Financial Sector Incumbents: Primary beneficiary (institutional/arbitrage) — large banks and asset managers capture value through inflation suppression and central bank backstops; high exit mobility
 *   - Central Bank Institution: Institutional beneficiary (institutional/arbitrage) — captures prestige of technocratic autonomy; coordinates with peer central banks globally to maintain independence norms
 *   - Regulatory Agencies & Supervisors: Institutional actor (institutional/identity_locked) — identity fused with independence doctrine; structurally mobile but cognitively captured; cannot exit without abandoning professional authority
 *   - Non-Financial Producers: Secondary victim (moderate/constrained) — benefit from credit coordination but bear suppressed demand; partially mobile through foreign capital access
 *   - Democratic Accountability Mechanism: Institutional actor (powerful/constrained) — oversight capability substantially degraded through institutional inertia; maintained as theater rather than functional constraint
 *   - Analytical Observer: Perspective holder (analytical/analytical) — risks naturalizing contingent institutional arrangements as economic laws of nature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(central_bank_independence_capture, 0.58).
domain_priors:suppression_score(central_bank_independence_capture, 0.65).
domain_priors:theater_ratio(central_bank_independence_capture, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(central_bank_independence_capture, extractiveness, 0.58).
narrative_ontology:constraint_metric(central_bank_independence_capture, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(central_bank_independence_capture, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(central_bank_independence_capture, tangled_rope).
narrative_ontology:human_readable(central_bank_independence_capture, "Central Bank Independence Regulatory Capture").
narrative_ontology:topic_domain(central_bank_independence_capture, "monetary_policy/institutional_capture").

domain_priors:requires_active_enforcement(central_bank_independence_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(central_bank_independence_capture, financial_sector_incumbents).
narrative_ontology:constraint_beneficiary(central_bank_independence_capture, institutional_central_bank).
narrative_ontology:constraint_victim(central_bank_independence_capture, public_monetary_sovereignty).
narrative_ontology:constraint_victim(central_bank_independence_capture, price_stability_mandate).
narrative_ontology:constraint_victim(central_bank_independence_capture, labor_market_welfare).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LABOR MARKET CONSTITUENCY (SNARE) — Workers and savers trapped in the monetary regime designed to prioritize price stability over employment. Inflation targeting at 2% creates persistent demand shortfall that keeps unemployment and wage growth suppressed relative to potential. No exit mechanism exists at the national level; currency substitution impossible for wage earners. Bears full extraction cost with zero agency.
constraint_indexing:constraint_classification(central_bank_independence_capture, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FINANCIAL SECTOR INCUMBENTS (ROPE) — Large banks and asset managers benefit from the low-rate environment and central bank backstop without bearing inflation cost. Experience the constraint as pure coordination: 'independence' means insulation from political pressure to loosen policy, protecting investment returns. High exit arbitrage (capital mobility, regulatory shopping). Net beneficiary.
constraint_indexing:constraint_classification(central_bank_independence_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: NON-FINANCIAL PRODUCERS (TANGLED ROPE) — Small and medium enterprises see mixed effects: access to credit coordination (genuine benefit) alongside suppressed demand from deflationary bias and weak employment. Constrained by inability to relocate banking relationships; can exit partially through foreign capital markets. Moderate extraction that is asymmetric but contains coordination function.
constraint_indexing:constraint_classification(central_bank_independence_capture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE CENTRAL BANK INSTITUTION (ROPE) — The central bank sees independence as coordination: freedom from short-term political pressure enables long-term credibility and inflation stability. Captures the prestige of technocratic autonomy and insulation from electoral cycles. Experiences constraint as coordination mechanism that protects institutional prerogatives. Has arbitrage exit (can coordinate with other central banks to reinforce independence norms globally).
constraint_indexing:constraint_classification(central_bank_independence_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DEMOCRATIC ACCOUNTABILITY MECHANISM (PITON) — Parliamentary or legislative oversight of monetary policy is substantially degraded. Central bank independence is maintained through institutional inertia and technical mystification (central banking as 'science' requiring insulation from political interference) rather than through functional necessity. Theater ratio high: independence is performed as technocratic neutrality while actual policy choices (inflation target level, asset purchase scope, whose assets to purchase) are intensely political. Mechanism persists despite low functionality.
constraint_indexing:constraint_classification(central_bank_independence_capture, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY FRAMEWORK INSTITUTION (TANGLED ROPE with IDENTITY_LOCKED) — Regulatory agencies and banking supervisors are identity-locked into the independence paradigm: their professional identity, expertise claims, and institutional autonomy are constituted through the 'independent central bank' frame. They cannot exit without abandoning their own institutional prestige and the epistemological foundation of their authority. Structurally mobile (could coordinate with elected officials) but identity-fused with the independence doctrine. This demonstrates capture through cognitive/institutional lock, not material coercion. Regulatory supervision coordinates credit allocation while simultaneously extracting regulatory rent (Basel compliance theater, stress test ritualism that shields incumbent banks from genuine discipline).
constraint_indexing:constraint_classification(central_bank_independence_capture, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE MOUNTAIN) — From a technocratic vantage, central bank independence appears as a natural law: inflation targeting requires insulation from political pressure; price stability requires credibility; credibility requires autonomy from elected officials. This perspective naturalizes a contingent institutional choice as immutable. However, the structural data reveals the false summit: the independence constraint is maintained through active enforcement (suppression of alternative monetary frameworks, dominance of inflation-targeting orthodoxy), requires beneficiary support (financial sector has strong interest in independence norms), and shows degraded democratic accountability (piton signature). The mountain is actually a snare dressed as law.
constraint_indexing:constraint_classification(central_bank_independence_capture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(central_bank_independence_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(central_bank_independence_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(central_bank_independence_capture, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(central_bank_independence_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(central_bank_independence_capture, TR),
    TR >= 0.70.

:- end_tests(central_bank_independence_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The financial sector and central bank institution capture substantial asymmetric benefits during deflationary regime, but the extraction is not maximal (Snare threshold 0.66) because: (1) coordination function is genuine — insulation from electoral cycles does provide credibility value; (2) labor market still functions with employment existing, wages paid, credit allocated; (3) escape mechanisms exist partially (foreign investment, currency shifting). The 0.58 reflects that this is hybrid extraction with real coordination blended in. Suppression (0.65): High. Multiple barriers prevent exit or policy change: (1) institutional dominance of inflation-targeting orthodoxy suppresses alternative frameworks; (2) regulatory capture insulates central bank from political pressure; (3) financial sector has structural incentive and capacity to maintain independence norms; (4) technical mystification ('central banking as science requiring insulation'); (5) lock-in through international coordination (Basel, EU treaties). Theater ratio (0.68): High and rising. Independence is increasingly performed: (1) stress tests show high theatrical content (designed to appear rigorous while preserving incumbent banks); (2) forward guidance is communication theater without material policy constraint; (3) financial stability mandates expand without changing actual decision process; (4) independence is invoked to explain policy choices that are actually political (inflation target, duration of low rates, asset purchase composition). The rising theater_ratio (0.52→0.68) indicates institutional degradation — performing independence while actual policy becomes more responsive to financial sector interests.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces starkly divergent classifications from different positions. Financial sector and central bank see rope: genuine coordination mechanism that solves a real problem (electoral political pressure causing inflation instability). Labor market sees snare: trapped in deflationary regime designed to suppress wage growth and employment. Regulatory agencies see tangled rope with identity lock: benefits from autonomy while trapped in independence frame that prevents reconfiguration. Democratic institutions see piton: machinery of accountability degraded to theater while independence persists through inertia. The perspectival gap reveals that what appears as natural law (mountain) from an analytical distance is actually contingent institutional arrangement (tangled rope) upon inspection. The false summit diagnosis is crucial: the analytical observer risks concluding that 'monetary policy independence is necessary for price stability' and 'independence requires insulation from elected officials' — both claims are contested and contingent, not universal laws.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position in the extraction flow. Financial sector incumbents (beneficiaries with high exit capacity) have d ≈ 0.10-0.15: they benefit, they can move capital elsewhere if policy shifts, they experience low effective extraction. Central bank institutions (beneficiaries with global arbitrage through coordination) have d ≈ 0.15-0.20: they benefit from independence norms, they coordinate with peer institutions to maintain norms globally, low experienced extraction. Labor market constituencies (victims with no exit) have d ≈ 0.95: trapped in deflationary regime by design, cannot substitute currency, bear suppressed employment and wages, experience maximum extraction. Regulatory agencies are identity_locked: structurally, they could exit (coordinate directly with elected officials, adopt alternative frameworks) but cognitively cannot because their professional identity is fused with 'independent central banking' as a concept. This is capture through identity, not through material barriers. The identity lock means d is derived not from exit options alone but from the internalization of the constraint's framing as natural/necessary — they see independence as obligatory, not chosen. Override: No directionality overrides needed; the structural derivation captures the identity-lock mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION via perspectival plurality. The question is not 'which classification is correct?' but 'which structural position are we analyzing from?'. Labor market perspective (snare) reveals extraction mechanism. Financial sector perspective (rope) reveals coordination benefit. Central bank perspective (rope) reveals institutional self-interest masked as coordination. Regulatory perspective (identity_locked tangled rope) reveals cognitive capture. Democratic perspective (piton) reveals theatrical maintenance. Analytical perspective (false mountain) reveals the risk of naturalizing contingent arrangements. The system resolves mandatrophy by acknowledging all six as valid readings. The constraint is NOT purely extractive (labor constituency view) — coordination function is real. The constraint is NOT purely coordinative (financial sector view) — asymmetric extraction is real. The constraint is HYBRID with degrading coordination function (rising theater_ratio) and intensifying capture (extractiveness rising from 0.35 to 0.58). The mandate is resolved: frame the constraint as Tangled Rope (per base_properties.claimed_type) with explicit notation that the rope component (genuine insulation from electoral pressure) is eroding and the snare component (asymmetric extraction benefiting financial sector through suppressed labor demand) is intensifying.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    independence_definition_ambiguity,
    'Is ''independence'' a constraint on monetary policy autonomy or a benefit granted to financial sector from political constraint?',
    'Comparative analysis of policy choices under ''independent'' vs elected central banks; empirical track record of price stability and unemployment outcomes across regimes; historical cases where governments overrode independence and actual inflation/welfare consequences',
    'If independence enables better outcomes: classify as Mountain or Rope. If outcomes are equivalent or worse: classify as Snare or Tangled Rope (independence is extraction mechanism, not optimization constraint).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(independence_definition_ambiguity, empirical, 'Whether independence defines a functional constraint or extractive mechanism').

omega_variable(
    inflation_target_selection_process,
    'Is the 2% inflation target a technically determined optimal choice or a politically contingent selection that favors asset holders and creditors over wage earners?',
    'Analysis of alternative inflation targets (1%, 1.5%, 3%, 4%) and their welfare impacts across income quintiles; historical evidence on why 2% was chosen; sensitivity analysis of employment-inflation tradeoff at different targets',
    'If 2% is technically optimal across all groups: extraction is lower-than-measured. If 2% systematically favors creditors/asset holders: extraction is higher and more asymmetric than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inflation_target_selection_process, empirical, 'Whether inflation target reflects technical optimality or political asymmetry').

omega_variable(
    regulatory_capture_feedback_loop,
    'Does central bank independence create regulatory capture through identity fusion (supervisory agencies internalize independence doctrine) or through material incentive alignment (central banks and financial sector share interests in inflation stability)?',
    'Process tracing of regulatory decisions; analysis of career paths between central banking and financial sector; study of stress test outcomes and enforcement action patterns; comparison of regulatory stringency under different political oversight regimes',
    'If primarily material incentives: constraint is structural extraction (Snare). If primarily identity fusion: constraint is cognitive capture (identity_locked Tangled Rope). If mixed: both mechanisms operate simultaneously, requiring multi-omega resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_feedback_loop, empirical, 'Whether capture operates through material incentives or identity lock').

omega_variable(
    alternative_framework_suppression,
    'Are alternative monetary frameworks (job guarantees, nominal GDP targeting, helicopter money) technically inferior or suppressed through doctrinal dominance of inflation-targeting orthodoxy?',
    'Comparative modeling of employment and stability outcomes; historical analysis of why inflation targeting became hegemonic; documentation of research funding and journal space allocation across frameworks; case studies of countries that tried alternatives',
    'If technically inferior: current framework is justified, extraction is lower. If suppressed by orthodoxy: current framework is maintained through active enforcement, extraction is higher and constraint is Snare not Mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_framework_suppression, empirical, 'Whether alternative monetary frameworks are technically inferior or doctrinally suppressed').

omega_variable(
    identity_lock_institutional_escape,
    'Can regulatory and central bank institutions exit the independence frame without losing professional identity and prestige, or is the frame constitutive of institutional authority?',
    'Qualitative research on institutional self-conception; analysis of cases where institutions tried to reframe authority (e.g., ECB ''whatever it takes''); documentation of professional blowback or support; theoretical reconstruction of what ''regulatory authority without independence'' would mean',
    'If exit is institutionally possible: identity_locked classification is incorrect, should downgrade to constrained. If exit would dissolve institutional identity: identity_locked is correct and represents genuine cognitive capture at the institutional level.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_institutional_escape, conceptual, 'Whether institutional identity is constituted through independence frame').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(central_bank_independence_capture, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cbic_tr_t0, central_bank_independence_capture, theater_ratio, 0, 0.52).
narrative_ontology:measurement(cbic_tr_t10, central_bank_independence_capture, theater_ratio, 10, 0.62).
narrative_ontology:measurement(cbic_tr_t20, central_bank_independence_capture, theater_ratio, 20, 0.68).
narrative_ontology:measurement(cbic_tr_t30, central_bank_independence_capture, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(cbic_be_t0, central_bank_independence_capture, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cbic_be_t10, central_bank_independence_capture, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(cbic_be_t20, central_bank_independence_capture, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(cbic_be_t30, central_bank_independence_capture, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(central_bank_independence_capture, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(central_bank_independence_capture, 0.12).
narrative_ontology:affects_constraint(central_bank_independence_capture, inflation_targeting_asymmetry).
narrative_ontology:affects_constraint(central_bank_independence_capture, financial_sector_regulatory_capture).
narrative_ontology:affects_constraint(central_bank_independence_capture, labor_market_employment_suppression).

% DUAL FORMULATION NOTE:
% Central bank independence capture is downstream of inflation-targeting framework selection and upstream of labor market employment suppression. The independence constraint coordinates insulation from electoral cycles (genuine function) while simultaneously extracting through suppression of full employment mandate (asymmetric function). Decomposition: separable constraint on 'freedom from electoral pressure' (lower ε, genuine coordination) vs 'subordination of employment to price stability' (higher ε, extraction mechanism). Both operate through the same institutional structure but have distinct ε values (~0.15 and ~0.68). Linked via network.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(central_bank_independence_capture, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
