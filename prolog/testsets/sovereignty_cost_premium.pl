% ============================================================================
% CONSTRAINT STORY: sovereignty_cost_premium
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereignty_cost_premium, []).

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
 *   constraint_id: sovereignty_cost_premium
 *   human_readable: Data Sovereignty Cost Premium in Legal Technology
 *   domain: legal_technology/data_sovereignty/professional_services
 *
 * SUMMARY:
 *   The data sovereignty cost premium in legal technology represents a
 *   transitional coordination mechanism bridging a closing window of legal
 *   and technical uncertainty. Regulated law firms pay 20-40% premiums for
 *   on-premise sovereign hosting solutions to satisfy client confidentiality
 *   requirements and regulatory compliance obligations during a period when
 *   legal frameworks have not yet recognized cryptographic attestation
 *   (confidential computing / TEE) as equivalent to physical jurisdictional
 *   control. This constraint is structurally a rope — genuine coordination
 *   solving a real collective action problem — but contains the seeds of its
 *   own obsolescence. As TEE deployment matures and regulatory acceptance
 *   follows, the technical necessity for physical sovereignty erodes. The
 *   constraint may then bifurcate: collapsing to near-zero for clients who
 *   accept cryptographic attestation, or persisting as a legibility moat for
 *   clients who prefer the audit simplicity and jurisdictional clarity of
 *   physical control. The current extractiveness (0.18) reflects fair
 *   compensation for specialized infrastructure and compliance overhead, not
 *   rent-seeking. The rising theater ratio (0.25 → 0.35) captures increasing
 *   performative emphasis on sovereignty certifications and audit rituals as
 *   the market matures and providers differentiate on compliance theater
 *   rather than pure technical capability.
 *
 * KEY AGENTS:
 *   - Regulated Law Firms: Primary beneficiary (institutional/constrained) — pay premium to achieve credible compliance posture and satisfy client confidentiality requirements during legal/technical uncertainty window
 *   - Sovereign Hosting Providers: Primary beneficiary (institutional/arbitrage) — capture premium revenue for jurisdictionally-bounded infrastructure; can pivot to TEE-based services as technology matures
 *   - Compliance Officers: Beneficiary (moderate/constrained) — individual professionals who benefit from defensible audit trail and jurisdictional certainty the premium buys
 *   - Confidential Computing Coalition: Organized agents (organized/mobile) — TEE vendors, cloud providers, standards bodies building cryptographic attestation infrastructure that will eliminate technical necessity for physical sovereignty (scaffold perspective with sunset logic)
 *   - Legacy Sovereign Providers (Post-TEE): Potential future extractors (institutional/constrained) — providers who maintain premium pricing after TEE matures by emphasizing legibility advantages and exploiting regulatory ambiguity (tangled rope perspective in future state)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereignty_cost_premium, 0.18).
domain_priors:suppression_score(sovereignty_cost_premium, 0.22).
domain_priors:theater_ratio(sovereignty_cost_premium, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereignty_cost_premium, extractiveness, 0.18).
narrative_ontology:constraint_metric(sovereignty_cost_premium, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(sovereignty_cost_premium, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereignty_cost_premium, rope).
narrative_ontology:human_readable(sovereignty_cost_premium, "Data Sovereignty Cost Premium in Legal Technology").
narrative_ontology:topic_domain(sovereignty_cost_premium, "legal_technology/data_sovereignty/professional_services").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereignty_cost_premium, sovereign_hosting_providers).
narrative_ontology:constraint_beneficiary(sovereignty_cost_premium, regulated_law_firms).
narrative_ontology:constraint_beneficiary(sovereignty_cost_premium, compliance_officers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REGULATED LAW FIRM (ROPE) — Pays premium for on-premise sovereign hosting to satisfy client confidentiality requirements and regulatory compliance. Experiences this as legitimate coordination cost: the premium buys verifiable jurisdictional control during a period of legal/technical uncertainty. Constrained exit (cannot simply abandon sovereignty claims without client consent) but benefits from the coordination function (credible compliance posture). Low extraction because the cost maps to genuine risk mitigation.
constraint_indexing:constraint_classification(sovereignty_cost_premium, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: SOVEREIGN HOSTING PROVIDER (ROPE) — Captures premium revenue by providing jurisdictionally-bounded infrastructure. Arbitrage exit (can pivot to attested-cloud services as TEE matures). Experiences constraint as coordination: clients pay for verifiable sovereignty, provider delivers it. The premium is not extractive from this position — it reflects genuine infrastructure cost (physical data centers, jurisdictional compliance overhead, audit requirements) plus fair margin for specialized service.
constraint_indexing:constraint_classification(sovereignty_cost_premium, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: CONFIDENTIAL COMPUTING COALITION (SCAFFOLD) — Organized actors (TEE vendors, cloud providers, standards bodies) building cryptographic attestation infrastructure that will eliminate technical necessity for physical sovereignty. Sees the cost premium as temporary coordination mechanism bridging the gap until attested-cloud alternatives achieve regulatory acceptance. Sunset clause: as TEE deployment matures and legal frameworks recognize cryptographic attestation as equivalent to physical control, the premium collapses to near-zero. Estimated timeline: 5-10 years for regulatory harmonization.
constraint_indexing:constraint_classification(sovereignty_cost_premium, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: COMPLIANCE OFFICER (ROPE) — Individual professional responsible for demonstrating data sovereignty to regulators and clients. Constrained exit (cannot unilaterally change firm's infrastructure choices) but benefits from the coordination function: the premium buys audit trail, jurisdictional certainty, and defensible compliance posture. Experiences low extraction because the cost is proportional to the risk being mitigated.
constraint_indexing:constraint_classification(sovereignty_cost_premium, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGACY SOVEREIGN PROVIDER POST-TEE (TANGLED ROPE) — After confidential computing matures, some sovereign hosting providers maintain premium pricing by emphasizing legibility advantages (physical audits, jurisdictional simplicity) even when technical necessity has eroded. This perspective sees mixed coordination and extraction: genuine residual value (some clients prefer physical sovereignty for legibility reasons) alongside rent-seeking (premium no longer justified by technical differentiation alone). Requires active enforcement through client lock-in, regulatory ambiguity exploitation, and FUD about TEE trustworthiness.
constraint_indexing:constraint_classification(sovereignty_cost_premium, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From a global/civilizational view, the sovereignty cost premium is a transitional coordination mechanism solving a real collective action problem: how to credibly demonstrate data control during a period when legal frameworks lag technical capabilities. The premium is not extractive at the system level — it reflects genuine infrastructure cost plus coordination overhead. As TEE matures, the premium should collapse to near-zero, with only a legibility residual for clients who prefer physical sovereignty's audit simplicity.
constraint_indexing:constraint_classification(sovereignty_cost_premium, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereignty_cost_premium_tests).
:- end_tests(sovereignty_cost_premium_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The premium reflects genuine infrastructure cost (physical data centers, jurisdictional compliance overhead, specialized audit requirements) plus fair margin for risk mitigation service. Not extractive because clients receive proportional value: verifiable sovereignty, defensible compliance posture, and reduced regulatory/client risk. The slight upward trajectory (0.12 → 0.18) reflects market maturation and increasing provider sophistication in pricing risk mitigation, not rent accumulation. Suppression (0.22): Low. Exit barriers are real but not severe: firms can migrate to alternative sovereign providers, negotiate pricing, or (in future) adopt TEE-based alternatives. Regulatory requirements create lock-in but not total captivity. Theater ratio (0.35 at T=6): Moderate and rising. Increasing performative emphasis on sovereignty certifications, audit rituals, and compliance documentation as the market matures. Providers differentiate through certification depth and audit frequency rather than pure technical capability. The theater is not yet dominant (< 0.50) because genuine technical differentiation still exists, but the trajectory suggests theater will increase as the market commoditizes.
 *
 * PERSPECTIVAL GAP:
 *   The constraint appears as rope from all current perspectives (regulated firms, sovereign providers, compliance officers, analytical observer) because all agents experience genuine coordination value. The scaffold perspective (confidential computing coalition) sees the same rope but with a sunset clause: the coordination function is temporary, bridging the gap until cryptographic attestation achieves regulatory acceptance. The tangled rope perspective (legacy sovereign providers post-TEE) is a potential future state, not the current reality — it represents the risk that some providers will maintain premium pricing after technical necessity erodes by exploiting legibility advantages and regulatory lag. The perspectival gap is temporal rather than positional: current agents see rope, future agents may see tangled rope if the transition is mismanaged.
 *
 * DIRECTIONALITY LOGIC:
 *   All primary agents are beneficiaries in the current state (T=0 to T=6): regulated law firms benefit from credible compliance, sovereign providers benefit from premium revenue, compliance officers benefit from defensible audit trails. No victims are declared because the premium is proportional to value delivered — this is genuine coordination, not extraction. The scaffold perspective (confidential computing coalition) sees a sunset: as TEE matures, the technical necessity for physical sovereignty erodes, and the premium should collapse. The tangled rope perspective (legacy sovereign providers post-TEE) is a future state where some providers maintain premium pricing after technical necessity has eroded by emphasizing legibility advantages — this would introduce victims (clients paying for unnecessary physical sovereignty) and require active enforcement (lock-in, FUD, regulatory ambiguity exploitation). The current constraint has no victims because it is solving a real problem during a genuine uncertainty window.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that rope can contain its own sunset logic without becoming scaffold in the present. The current classification is rope (genuine coordination, low extraction, proportional value) while simultaneously being scaffold from the organized coalition's perspective (temporary mechanism with identifiable sunset). The distinction: rope describes the current structural relationship (all agents benefit, no victims, fair pricing), scaffold describes the temporal trajectory (the coordination function will become unnecessary as TEE matures). Both are true. The constraint is not currently extractive, but it is structurally temporary. If providers attempt to sustain the premium after TEE eliminates technical necessity, the constraint transitions to tangled rope (mixed coordination and extraction, requiring active enforcement). The mandatrophy resolution: a constraint can be simultaneously rope (current state), scaffold (temporal trajectory), and tangled rope (future risk) without contradiction — these are different observational contexts, not competing claims about a single fixed type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tee_regulatory_acceptance_timeline,
    'How quickly will legal frameworks recognize cryptographic attestation (TEE) as equivalent to physical data sovereignty for attorney-client privilege and regulatory compliance?',
    'Tracking regulatory guidance updates, case law development, and professional standards body pronouncements across major jurisdictions (EU, US, UK, Australia). Monitor adoption rates of TEE-based solutions by regulated law firms and acceptance by courts/regulators.',
    'If acceptance is rapid (< 5 years): scaffold sunset confirmed, premium collapses quickly. If slow (> 10 years): premium persists longer, increasing risk of rent-seeking behavior by legacy providers (tangled rope from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tee_regulatory_acceptance_timeline, empirical, 'Timeline for regulatory acceptance of TEE as sovereignty equivalent').

omega_variable(
    legibility_moat_durability,
    'After TEE eliminates technical necessity for physical sovereignty, how much residual premium can providers sustain based purely on legibility advantages (simpler audits, jurisdictional clarity, client preference for physical control)?',
    'Price trajectory analysis post-TEE maturity. Compare premium retention rates for providers emphasizing legibility vs those competing on TEE cost-efficiency. Client survey data on willingness-to-pay for physical sovereignty after cryptographic alternatives are available.',
    'If legibility moat is durable (premium > 20% post-TEE): some providers successfully transition to selling audit simplicity rather than technical necessity. If moat is weak (premium < 5% post-TEE): market commoditizes rapidly, confirming pure coordination hypothesis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legibility_moat_durability, empirical, 'Residual premium sustainability based on legibility advantages alone').

omega_variable(
    tee_trustworthiness_perception,
    'Will legal professionals and clients trust cryptographic attestation (TEE) as much as physical data sovereignty, or will residual skepticism about ''black box'' cryptography sustain demand for physical control?',
    'Client preference surveys, adoption rate analysis segmented by firm size and risk profile, qualitative interviews with general counsel and compliance officers. Track correlation between TEE education/transparency efforts and adoption rates.',
    'If trust gap persists: physical sovereignty retains premium even after TEE is technically mature and legally accepted (legibility moat confirmed). If trust converges: premium collapses to infrastructure cost differential only.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tee_trustworthiness_perception, preference, 'Client trust in cryptographic attestation vs physical sovereignty').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereignty_cost_premium, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sov_cost_theater_t0, sovereignty_cost_premium, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sov_cost_theater_t3, sovereignty_cost_premium, theater_ratio, 3, 0.3).
narrative_ontology:measurement(sov_cost_theater_t6, sovereignty_cost_premium, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(sov_cost_extract_t0, sovereignty_cost_premium, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(sov_cost_extract_t3, sovereignty_cost_premium, base_extractiveness, 3, 0.15).
narrative_ontology:measurement(sov_cost_extract_t6, sovereignty_cost_premium, base_extractiveness, 6, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereignty_cost_premium, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is downstream of privilege_architecture_coordination. The upstream constraint establishes the legal/technical framework requiring data sovereignty; this constraint models the cost premium paid to satisfy that requirement during the transitional period before cryptographic attestation achieves regulatory acceptance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
