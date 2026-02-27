% ============================================================================
% CONSTRAINT STORY: eu_digital_services_act
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_digital_services_act, []).

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
 *   constraint_id: eu_digital_services_act
 *   human_readable: EU Digital Services Act (DSA)
 *   domain: technological/political
 *
 * SUMMARY:
 *   The EU Digital Services Act represents a major attempt to regulate large
 *   online platforms through a coordinated continental policy framework.
 *   Enacted in 2022 and entering enforcement phase in 2024-2025, the DSA
 *   imposes transparency, content moderation, and algorithmic accountability
 *   requirements on 'very large online platforms' (VLOPs: platforms with >45M
 *   monthly active users in EU). The constraint exhibits structural tension
 *   between its legitimate coordination function (harmonizing fragmented
 *   national regulations, establishing baseline user protections, creating
 *   enforceable standards) and its extraction mechanisms (compliance cost
 *   barriers protecting incumbents, suppression of platform innovation,
 *   potential rent creation). The constraint's extractiveness has increased
 *   over its first two years of implementation as platforms discovered the
 *   true compliance burden; theater ratio has increased as regulatory
 *   announcements emphasize enforcement drama while actual outcome
 *   verification remains limited. The DSA simultaneously solves real problems
 *   (platforms' previous self-regulation failure, race-to-the-bottom
 *   dynamics, fragmented national rules) and creates new ones (compliance
 *   overhead entrenchment, reduced platform competition, innovation
 *   suppression, regulatory capture vulnerability).
 *
 * KEY AGENTS:
 *   - Very Large Online Platforms (Meta, Google, Amazon, TikTok): Primary victims (powerful/constrained) — face compliance costs, algorithmic audits, liability exposure; cannot exit EU market; experience extraction.
 *   - EU Regulatory Authority (European Commission, national regulators): Primary beneficiary (institutional/arbitrage) — gains enforcement power, standardized compliance expectations, jurisdictional clarity; can modify rules.
 *   - European Users and Civil Society: Mixed victim/beneficiary (moderate/constrained) — gain content moderation protections and algorithmic transparency; lose platform service quality and innovation; trapped in EU digital ecosystem.
 *   - Smaller Platforms and European Startups: Secondary beneficiary (organized/mobile) — protected from US mega-platform competition; lighter regulatory burden; can relocate if needed.
 *   - Platform Innovation Capacity: Victim (powerless/trapped) — reduced R&D investment in experimental features due to compliance burden and liability exposure; cannot organize or exit.
 *   - Global Digital Regulation Discourse: Institutional actor (institutional/arbitrage) — DSA serves as model for other jurisdictions (UK, Australia, others), creating parallel regulatory regimes; maintains performative role.
 *   - Analytical Observer: Generational/global view (analytical/analytical) — sees genuine coordination benefit AND genuine extraction simultaneously; constraint is not purely regulatory or purely extractive.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_digital_services_act, 0.52).
domain_priors:suppression_score(eu_digital_services_act, 0.58).
domain_priors:theater_ratio(eu_digital_services_act, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_digital_services_act, extractiveness, 0.52).
narrative_ontology:constraint_metric(eu_digital_services_act, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(eu_digital_services_act, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_digital_services_act, tangled_rope).
narrative_ontology:human_readable(eu_digital_services_act, "EU Digital Services Act (DSA)").
narrative_ontology:topic_domain(eu_digital_services_act, "technological/political").

domain_priors:requires_active_enforcement(eu_digital_services_act).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_digital_services_act, eu_regulatory_authority).
narrative_ontology:constraint_beneficiary(eu_digital_services_act, european_users).
narrative_ontology:constraint_beneficiary(eu_digital_services_act, smaller_platforms).
narrative_ontology:constraint_victim(eu_digital_services_act, very_large_platforms).
narrative_ontology:constraint_victim(eu_digital_services_act, platform_innovation_capacity).
narrative_ontology:constraint_victim(eu_digital_services_act, global_competitiveness).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VERY LARGE PLATFORM (SNARE) — Constrained by DSA compliance requirements (content moderation, transparency reports, algorithmic audits). Cannot exit EU market without massive revenue loss. High suppression: must implement compliance infrastructure, accept regulatory audits, expose proprietary algorithms. Extraction mechanism: compliance costs (~€100M+/year for Meta/Google scale) create barrier to entry for competitors while incumbent platforms absorb costs. d≈0.80, f(d)≈1.20, σ=1.1 → χ≈0.62.
constraint_indexing:constraint_classification(eu_digital_services_act, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 2: EU REGULATORY AUTHORITY (ROPE) — Experiences DSA as a coordination mechanism solving collective action problem (regulation of cross-border platforms). Benefits from jurisdiction clarity, standardized compliance expectations, and enhanced regulatory capacity. Exit via arbitrage: can modify rules, grant exemptions, create safe harbors. Suppression is low from this perspective (rules are self-imposed). d≈0.10, f(d)≈-0.08, σ=1.1 → χ≈-0.04. Net coordination benefit.
constraint_indexing:constraint_classification(eu_digital_services_act, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: EUROPEAN USERS AND CIVIL SOCIETY (TANGLED ROPE) — Experience both coordination benefit (enhanced content removal, reduced illegal material, transparency on algorithmic ranking) AND extraction cost (reduced platform service quality, slower innovation, potential censorship creep if moderation is over-enforced). Exit constrained: cannot migrate to non-EU platforms without losing local services. Benefits from coordination (illegal content control, right to explanation); bears costs of reduced platform competition. d≈0.60, f(d)≈0.75, σ=1.1 → χ≈0.43.
constraint_indexing:constraint_classification(eu_digital_services_act, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: SMALLER PLATFORMS AND STARTUPS (ROPE) — Experience DSA as pure coordination benefit: compliance requirements create barrier-to-entry for US mega-platforms, protecting smaller European competitors. Have mobile exit options (can relocate HQ, focus on non-EU markets). Suppression low: regulatory burden is lighter for non-VLOPs. Benefit from level playing field without bearing full extraction cost. d≈0.25, f(d)≈0.10, σ=1.1 → χ≈0.06. Net beneficiary.
constraint_indexing:constraint_classification(eu_digital_services_act, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: PLATFORM INNOVATION CAPACITY (SNARE) — Abstract collective of potential innovations that cannot be realized due to compliance overhead and liability exposure. Trapped: regulatory burden diverts R&D spending from new features to compliance infrastructure. Cannot organize or exit. Extraction mechanism: DSA compliance costs create rent extraction from platform innovation. Suppression: algorithmic experimentation faces regulatory liability; content recommendation systems must be defensible to auditors (not optimal). d≈0.92, f(d)≈1.38, σ=1.1 → χ≈0.81.
constraint_indexing:constraint_classification(eu_digital_services_act, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 6: GLOBAL DIGITAL REGULATION DISCOURSE (PITON) — DSA is often performed as a civilizational stand against Big Tech ('Europe standing up to Silicon Valley') but the actual regulatory mechanism is substantially theatrical. Compliance audits focus on documentation and process rather than outcome verification. Content moderation claims are difficult to verify; platforms can game metrics. Theater ratio 0.68 reflects significant performative element: regulatory announcements, fines (calibrated for media impact rather than deterrence), public commitments vs. private implementation gaps. Mechanism is inertially maintained (all other jurisdictions now expect regulatory intervention) but primary function (actually improving user safety/platform accountability) is contested.
constraint_indexing:constraint_classification(eu_digital_services_act, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a generational/global view, DSA exhibits genuine coordination function (harmonized regulation reduces fragmentation) AND genuine extraction (compliance costs create rents, innovation suppression, margin protection for incumbents). Both are structural. The constraint is neither a natural law nor a pure extraction trap, but an institutional artifact that solves a real problem (coordinating platform governance) while generating real costs (suppressed competition, innovation burden). d≈0.65, f(d)≈0.95, σ=1.1 → χ≈0.54.
constraint_indexing:constraint_classification(eu_digital_services_act, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_digital_services_act_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_digital_services_act, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_digital_services_act, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_digital_services_act, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(eu_digital_services_act, TR),
    TR >= 0.70.

:- end_tests(eu_digital_services_act_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and rising. Initial DSA extractiveness was lower (0.35) when estimates focused on abstract regulatory burden. As platforms implemented compliance infrastructure, true costs emerged: Meta reported €100M+ annual DSA compliance spending; algorithmic recommendation system audits require dedicated teams; content moderation burden created supplier dependencies. The rising trajectory reflects cost accumulation (compliance is additive, not one-time). Moderate-high rather than severe (0.70+) because (1) some compliance costs are genuine safety improvements with social benefit, not pure rent extraction, and (2) smaller platforms face lighter burden (DSA scales by user count). Suppression (0.58): Moderate-high. Significant barriers: platforms cannot experiment with recommendation algorithms without audit risk; content moderation cannot use aggressive automation without liability exposure; innovation in user engagement features faces regulatory scrutiny. But suppression is not absolute (some experimentation continues, some platforms achieve compliance efficiently). Theater ratio (0.68): High and rising. Regulatory announcements emphasize enforcement drama; fines are calibrated for media impact (€5.5B Google fine announced with ceremony) rather than economic deterrence; platforms comply with documentation and reporting rather than outcome verification; content moderation claims are difficult to falsify. Theater has increased as regulators discovered that actual outcome measurement is harder than process verification.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival inversion: the powerless agent in many regulatory scenarios (users) experiences mixed coordination/extraction here, while the powerful agent (platforms) experiences pure extraction. The beneficiary (regulators) experiences low-cost coordination while paying no extraction cost. Smaller platforms experience pure upside (protection without burden). The piton perspective reveals the constraint's theatrical dimension: regulatory enforcement is observed with greater skepticism from civilizational distance, showing how DSA relies on performative compliance (documentation, reported metrics) rather than actual outcome verification. The analytical observer sees both dimensions simultaneously: DSA is NOT a false summit (unlike naturalizing narratives that treat platform regulation as impossible), but it IS a hybrid with real coordination benefit AND real extraction cost, not decomposable into 'regulation is good' or 'regulation is bad'.
 *
 * DIRECTIONALITY LOGIC:
 *   Very large platforms: Victim + constrained → d≈0.80. High extraction. Cannot exit EU market; compliance costs are mandatory; liability exposure is broad. EU regulatory authority: Beneficiary + arbitrage → d≈0.10. Low/negative effective extraction. Rules are self-imposed; can modify or grant exemptions. European users: Mixed victim/beneficiary + constrained → d≈0.60. Moderate extraction despite coordination benefit. Gain content moderation and transparency but lose service quality and innovation; trapped in EU digital ecosystem. Smaller platforms: Beneficiary + mobile → d≈0.25. Low/negative extraction. Protected by compliance barriers; can exit if needed. Platform innovation capacity: Victim + trapped → d≈0.92. Severe extraction. Abstract collective cannot defend itself; regulatory liability disproportionately affects experimental features. Regulators view innovation as externalizable cost. Global regulation discourse: Institutional + arbitrage → d≈0.10. Piton perspective: theatrical maintenance of regulatory role despite uncertain effectiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL MANDATROPHY CASE: DSA initially appears to be 'regulatory coordination, obviously good for users' (mandate: protect Europeans from platform harms). But the constraint structure reveals simultaneous extraction from multiple vectors: (1) compliance cost concentration protecting incumbents (snare structure for platforms); (2) innovation suppression (snare structure for innovation capacity); (3) theater ratio increase (piton structure for regulatory discourse); (4) mixed outcome for users who gain moderation but lose service quality (tangled_rope). The mandatrophy is resolved by recognizing that DSA is NOT 'regulation vs. non-regulation' but 'what kind of regulation and who bears costs.' The constraint classifies as tangled_rope at the analytical level because (a) it has genuine coordination function (harmonizes rules, addresses market failure), (b) it has genuine extraction (compliance costs, innovation suppression, competitive entrenchment), (c) both are structural and persistent. The temptation to classify as pure rope ('regulation is coordination') or pure snare ('regulation is extraction') both fail to capture the real hybrid structure. Mandatrophy is resolved by accepting that regulations can be simultaneously pro-coordination AND pro-extraction, depending on implementation and distributional outcomes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compliance_cost_concentration,
    'Do DSA compliance costs concentrate extraction in ways that entrench incumbent platforms, or do they create market space for specialized compliance infrastructure (making costs distributed)?',
    'Analysis of actual compliance spending vs. platform size; emergence and viability of third-party compliance vendors; cost-per-user comparison across platform sizes over 5-year horizon',
    'If concentrated in incumbents: snare classification strengthens; extraction persists. If distributed via vendor ecosystem: tangled_rope confirmed; coordination benefit outweighs extraction cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_cost_concentration, empirical, 'Whether DSA compliance costs concentrate in incumbents or distribute via vendor ecosystem').

omega_variable(
    content_moderation_effectiveness,
    'Does DSA-mandated content moderation actually reduce illegal material prevalence, or merely shift moderation bottleneck without improving outcomes?',
    'Longitudinal tracking of illegal content prevalence on EU platforms pre/post DSA; comparison with non-EU regulatory regimes; measurement of false positive/negative rates in automated vs. human moderation',
    'If effective: coordination benefit is real, tangled_rope justified. If ineffective: DSA is theater masking extraction mechanism; classification shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(content_moderation_effectiveness, empirical, 'Whether DSA content moderation reduces illegal material or creates illusion of effectiveness').

omega_variable(
    algorithmic_transparency_feasibility,
    'Can algorithmic ranking systems be meaningfully audited/verified under DSA requirements, or is ''algorithmic transparency'' inherently impossible for proprietary systems?',
    'Assessment of audit frameworks; comparison of platform-reported rankings vs. external measurement; technical feasibility studies on reverse-engineering recommendation systems',
    'If feasible: DSA transparency mechanism is functional, coordination component is real. If infeasible: audits are theater; classification shifts toward piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_transparency_feasibility, conceptual, 'Whether algorithmic transparency under DSA is technically achievable or inherently theatrical').

omega_variable(
    regulatory_capture_risk,
    'Does DSA implementation risk regulatory capture whereby platform lobbying shapes enforcement, turning regulation into protection mechanism for incumbents?',
    'Analysis of enforcement patterns (do fines scale with platform size/market dominance?); tracking of exemption requests and approval rates; measurement of regulatory agency capture indicators',
    'If captured: DSA becomes pure snare for both platforms (forced compliance enforces entrenchment) and users (regulation loses legitimacy). If genuinely independent: tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_risk, empirical, 'Risk that DSA enforcement becomes regulatory capture protecting incumbents').

omega_variable(
    non_eu_platform_relocation,
    'Do US-based platforms respond to DSA by relocating EU operations (creating separate EU entities) or genuinely restructuring architecture, revealing whether extraction cost exceeds coordination benefit for incumbents?',
    'Mapping of platform entity structures post-DSA; analysis of service quality changes in EU vs. non-EU versions; measurement of actual vs. nominal platform consolidation',
    'If relocation occurs: extraction mechanism is so severe that even monopoly incumbents find compliance burden unsustainable; classification shifts toward snare. If restructuring occurs: platforms absorb cost; tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_eu_platform_relocation, empirical, 'Whether platforms relocate operations or absorb DSA compliance costs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_digital_services_act, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsa_tr_t0, eu_digital_services_act, theater_ratio, 0, 0.55).
narrative_ontology:measurement(dsa_tr_t2, eu_digital_services_act, theater_ratio, 2, 0.62).
narrative_ontology:measurement(dsa_tr_t5, eu_digital_services_act, theater_ratio, 5, 0.68).

% Extraction over time
narrative_ontology:measurement(dsa_be_t0, eu_digital_services_act, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dsa_be_t2, eu_digital_services_act, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(dsa_be_t5, eu_digital_services_act, base_extractiveness, 5, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_digital_services_act, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(eu_digital_services_act, 0.45).
narrative_ontology:affects_constraint(eu_digital_services_act, platform_market_concentration).
narrative_ontology:affects_constraint(eu_digital_services_act, content_moderation_labor_bottleneck).
narrative_ontology:affects_constraint(eu_digital_services_act, eu_regulatory_fragmentation).

% DUAL FORMULATION NOTE:
% DSA should be decomposed into three distinct constraints: (1) content_moderation_requirement (ε=0.38, primarily rope), addressing illegal material coordination; (2) algorithmic_accountability (ε=0.55, primarily tangled_rope), mixing transparency coordination with proprietary algorithm protection extraction; (3) regulatory_harmonization_framework (ε=0.42, primarily scaffold with sunset clause as regulatory capacity matures). The present story treats DSA as monolithic; separation enables more precise analysis of which components are functional coordination and which are extractive or theatrical. Each sub-constraint affects the others through implementation coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_digital_services_act, powerful, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
