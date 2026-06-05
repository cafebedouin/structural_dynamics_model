% ============================================================================
% CONSTRAINT STORY: viral_transmission_rates
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_viral_transmission_rates, []).

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
 *   constraint_id: viral_transmission_rates
 *   human_readable: Socio-Political Response to Viral Transmission
 *   domain: political/technological/public_health
 *
 * SUMMARY:
 *   The socio-political response to viral transmission rate constraints
 *   models a hybrid coordination-extraction mechanism where legitimate
 *   disease suppression objectives are inseparable from institutional
 *   expansion, behavioral control infrastructure, and authority
 *   consolidation. The constraint exhibits structural tension between genuine
 *   public health coordination needs (disease prevention, vaccination,
 *   isolation of contagious individuals) and extractive overlays (mandatory
 *   lockdowns, surveillance integration, mobility restrictions that exceed
 *   epidemiological necessity, selective enforcement favoring institutional
 *   actors). The theater ratio trajectory (0.25 → 0.58) reflects a
 *   progressive shift from functional disease management to performative
 *   compliance ritual: early interventions target actual transmission
 *   pathways; later interventions persist despite epidemiological rationale
 *   weakening, suggesting substitution of proxy compliance for outcome
 *   optimization. The extractiveness trajectory (0.35 → 0.52) documents
 *   rent-seeking accumulation: initial policies address transmission;
 *   subsequent policies layer enforcement overhead, surveillance
 *   infrastructure, and capacity restrictions that increase institutional
 *   control independent of disease risk. This constraint demonstrates how
 *   coordination problems can become extraction mechanisms when the solving
 *   agent captures asymmetric benefits, suppresses alternatives, and exploits
 *   urgency to bypass democratic deliberation.
 *
 * KEY AGENTS:
 *   - Immobilized Worker: Primary victim (powerless/trapped) — bears full cost of lockdowns, job loss, freedom of movement restriction
 *   - Small Business Operator: Secondary victim (moderate/constrained) — faces capacity restrictions, sector-specific closures, regulatory compliance burden
 *   - Economic Activity: Victim (powerless/trapped) — abstract aggregate of interrupted production, investment freeze, supply chain collapse
 *   - Bodily Autonomy: Victim (powerless/trapped) — individual liberty constrained by medical mandate enforcement
 *   - State Authority: Primary beneficiary (institutional/arbitrage) — consolidates emergency powers, expands surveillance capacity, centralizes resource allocation
 *   - Medical Establishment: Primary beneficiary (institutional/arbitrage) — gains resource allocation, expanded authority, data access, legitimacy boost
 *   - Surveillance Infrastructure: Beneficiary (institutional/arbitrage) — contact tracing, vaccine passports, mobility monitoring become normalized and persistent
 *   - Labor Collective: Secondary actor (organized/constrained) — organized enough to coordinate but too dependent on employment to exit restrictions
 *   - Public Health Apparatus: Institutional actor (institutional/arbitrage) — maintains performative apparatus; sees own function as degraded (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent policy choices as epidemic necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(viral_transmission_rates, 0.52).
domain_priors:suppression_score(viral_transmission_rates, 0.65).
domain_priors:theater_ratio(viral_transmission_rates, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(viral_transmission_rates, extractiveness, 0.52).
narrative_ontology:constraint_metric(viral_transmission_rates, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(viral_transmission_rates, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(viral_transmission_rates, tangled_rope).
narrative_ontology:human_readable(viral_transmission_rates, "Socio-Political Response to Viral Transmission").
narrative_ontology:topic_domain(viral_transmission_rates, "political/technological/public_health").

domain_priors:requires_active_enforcement(viral_transmission_rates).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(viral_transmission_rates, state_authority).
narrative_ontology:constraint_beneficiary(viral_transmission_rates, medical_establishment).
narrative_ontology:constraint_beneficiary(viral_transmission_rates, surveillance_infrastructure).
narrative_ontology:constraint_victim(viral_transmission_rates, economic_activity).
narrative_ontology:constraint_victim(viral_transmission_rates, bodily_autonomy).
narrative_ontology:constraint_victim(viral_transmission_rates, labor_mobility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IMMOBILIZED WORKER (SNARE) — No exit from geographic lockdown; livelihood depends on compliance with movement restrictions. Cannot arbitrage or relocate. Bears full cost of economic contraction and loss of mobility. d≈0.92, f(d)≈1.39, σ=1.0 → χ≈0.65.
constraint_indexing:constraint_classification(viral_transmission_rates, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL BUSINESS OPERATOR (TANGLED ROPE) — Constrained by sector-specific closure mandates and capacity restrictions. Benefits from public health coordination (disease avoidance) but bears extraction through revenue collapse and regulatory overhead. d≈0.70, f(d)≈1.08, σ=0.9 → χ≈0.49.
constraint_indexing:constraint_classification(viral_transmission_rates, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MEDICAL ESTABLISHMENT (ROPE) — Benefits from state resource allocation, expanded authority, and data collection. Experiences constraint as coordination: communicating transmission risk justifies centralized response. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.05.
constraint_indexing:constraint_classification(viral_transmission_rates, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LABOR COLLECTIVE (TANGLED ROPE) — Organized labor sees both coordination (workplace safety norms) and extraction (wage theft via unpaid leave, skill atrophy). Exit constrained by collective action requirements. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(viral_transmission_rates, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PUBLIC HEALTH APPARATUS (PITON) — Original function (disease containment coordination) has degraded into performative compliance theater. Mask mandates, capacity restrictions, and closure orders persist long after epidemiological rationale weakens. theater_ratio=0.58 indicates shift from functional coordination to ritualized enforcement. d≈0.10, f(d)≈-0.08, σ=1.0 → χ≈-0.04.
constraint_indexing:constraint_classification(viral_transmission_rates, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, disease transmission is an immutable property of human contact and viral biology. Some form of coordination response (quarantine, vaccination, isolation) is inherent to managing pandemic risk. This perspective risks naturalizing what are contingent policy choices (mandatory lockdowns, enforcement mechanisms, surveillance integration) as inevitable laws of epidemiology. However, the structural data (ε=0.52, suppression=0.65, theater=0.58) contradicts the mountain classification — revealing a false summit where policy contingency is disguised as natural necessity.
constraint_indexing:constraint_classification(viral_transmission_rates, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(viral_transmission_rates_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(viral_transmission_rates, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(viral_transmission_rates, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(viral_transmission_rates, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(viral_transmission_rates, TR),
    TR >= 0.70.

:- end_tests(viral_transmission_rates_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint imposes significant costs on targeted populations (immobilized workers, small businesses, labor) that exceed the disease suppression benefits accruing to those populations. A lower-extraction alternative would be voluntary coordination with transparent risk communication — that would achieve similar transmission reduction at much lower imposition cost. The measured extractiveness reflects the gap between mandatory enforcement and minimally necessary coordination. Suppression (0.65): High. Significant barriers to exit include legal penalties for movement restrictions, employment dependency for vaccine mandate compliance, financial ruin for businesses violating closure orders, and social stigma for non-compliance. Exit is not impossible (migration, underground economy, civil disobedience) but carries severe costs. Theater ratio (0.58): Moderate. Early pandemic response was functionally focused (testing, isolation, vaccination). By months 12-24, significant performative content emerges: mandatory mask rules persist despite declining transmission; capacity restrictions continue without epidemiological basis; vaccine passports remain in place despite variant-driven transmission changes. Approximately 58% of enforcement activity by month 24 consists of compliance theater rather than outcome optimization.
 *
 * PERSPECTIVAL GAP:
 *   The immobilized worker sees pure extraction (Snare) — they bear the cost of restrictions without receiving disease suppression benefits (they were already isolated by economic precarity). The small business operator sees mixed coordination-extraction (Tangled Rope) — public health coordination is real, but its implementation extracts via arbitrary closures and capacity constraints. The medical establishment sees pure coordination (Rope) — they are solving the legitimate problem of disease suppression and benefit from the authority to do so. The labor collective sees mixed coordination-extraction (Tangled Rope) — workplace safety norms are genuinely valuable, but enforcement mechanism extracts via employment dependency and wage suppression. The public health apparatus sees itself as degraded (Piton) — the apparatus knows its early function (actual disease management) has been partially replaced by compliance theater, yet continues because the surveillance and control infrastructure is institutionally entrenched. The analytical observer risks seeing an immutable law (Mountain) — that pandemic response requires centralized mandatory enforcement — but the structural data reveals this as a false summit: the contingent policy choices (mandatory vs voluntary, centralized vs decentralized, time-limited vs persistent) are not laws of epidemiology.
 *
 * DIRECTIONALITY LOGIC:
 *   Immobilized worker: Victim + trapped → d≈0.92, f(d)≈1.39. Maximum extraction. State authority: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.10. Net beneficiary (negative χ). Medical establishment: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Small business operator: Victim + constrained → d≈0.70, f(d)≈1.08. High extraction but partially mitigated by safety coordination benefits. Labor collective: Victim + constrained → d≈0.55, f(d)≈0.75. Moderate-high extraction; organized enough to negotiate but dependent on employment compliance. Public health apparatus: Institutional + arbitrage → d≈0.10, f(d)≈-0.08. Piton classification comes from theater gate (0.58 ≥ 0.70 threshold not met for pure piton, but trajectory suggests degradation). Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival; engine's false summit detector catches naturalization of policy contingency.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint avoids mandatrophy by clearly identifying the coordination function (disease suppression) as distinct from the extraction mechanism (centralized control, surveillance integration, mobility suppression). The mandatrophy resolution chain: (1) Legitimate coordination problem exists: viral transmission creates collective action problem requiring information sharing and isolation of infectious individuals. (2) Multiple solutions available: mandatory enforcement is ONE solution; voluntary coordination with transparent risk data is another. (3) Extraction is in the enforcement choice: mandatory implementation extracts from powerless agents who would voluntarily comply if they had accurate risk information and economic support. (4) Theater substitution detectable: theater ratio trajectory (0.25 → 0.58) shows shift from outcome optimization to proxy compliance, indicating that extraction mechanism is replacing coordination function. (5) Classification is Tangled Rope, not Snare: the coordination function is real and valuable; the extraction is a parasitic layer on top of coordination, not the constraint's sole function. If the constraint had zero coordination function (pure surveillance expansion with no disease suppression rationale), it would be Snare. If it had genuine sunset mechanisms for surveillance infrastructure and explicit sunset clauses for enforcement, it would be Scaffold. Current reality is Tangled Rope: real coordination + real extraction + active enforcement with no formal sunset.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transmission_severity_threshold,
    'What level of transmission severity justifies the shift from voluntary coordination to mandatory suppression?',
    'Comparative analysis of mortality rates, hospitalization capacity, and enforcement intensity across pandemic phases and jurisdictions',
    'If threshold is epidemiologically rigorous: enforcement is proportionate coordination (Rope from more perspectives). If threshold is politically determined: enforcement is extraction mechanism (Snare/Tangled Rope from most perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_severity_threshold, empirical, 'Epidemiological threshold for justifying mandatory enforcement').

omega_variable(
    voluntary_compliance_capacity,
    'Could voluntary public health coordination achieve comparable disease suppression without mandatory lockdowns and mobility restrictions?',
    'Longitudinal analysis of infection rates across jurisdictions with different enforcement intensities; behavioral surveys on compliance motivation (disease avoidance vs mandate compliance)',
    'If yes: mandatory enforcement is extractive overlay on viable coordination mechanism (Snare from victim perspectives). If no: mandatory enforcement is necessary coordination (Rope from all perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_compliance_capacity, empirical, 'Whether voluntary coordination can achieve comparable disease suppression').

omega_variable(
    surveillance_integration_permanence,
    'Are contact tracing, vaccine passport, and mobility monitoring systems designed to sunset when transmission threat declines, or do they persist as permanent surveillance infrastructure?',
    'Legal analysis of emergency clause duration; empirical tracking of surveillance infrastructure persistence beyond declared emergency period; policy review of archival vs deletion protocols',
    'If sunset mechanisms exist and operate: constraint is temporary (Scaffold). If persistence is structural: surveillance becomes permanent extraction mechanism (Snare), and the public health framing was Trojan horse for institutional expansion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(surveillance_integration_permanence, empirical, 'Whether surveillance infrastructure has genuine sunset mechanisms').

omega_variable(
    alternative_coordination_pathways,
    'Do decentralized, voluntary public health information systems (community risk assessment, mutual aid networks, transparent data sharing) provide equivalent or superior coordination outcomes compared to centralized mandates?',
    'Comparative effectiveness studies; analysis of adaptation speed and local responsiveness in centralized vs decentralized health response models',
    'If equivalent: the constraint is pure extraction masquerading as coordination (Snare). If decentralized fails: mandatory coordination is necessary (Rope).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_coordination_pathways, conceptual, 'Effectiveness of decentralized vs centralized coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(viral_transmission_rates, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(viral_tr_t0, viral_transmission_rates, theater_ratio, 0, 0.25).
narrative_ontology:measurement(viral_tr_t12, viral_transmission_rates, theater_ratio, 12, 0.42).
narrative_ontology:measurement(viral_tr_t24, viral_transmission_rates, theater_ratio, 24, 0.58).

% Extraction over time
narrative_ontology:measurement(viral_be_t0, viral_transmission_rates, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(viral_be_t12, viral_transmission_rates, base_extractiveness, 12, 0.44).
narrative_ontology:measurement(viral_be_t24, viral_transmission_rates, base_extractiveness, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(viral_transmission_rates, enforcement_mechanism).
narrative_ontology:affects_constraint(viral_transmission_rates, centralized_surveillance_normalization).
narrative_ontology:affects_constraint(viral_transmission_rates, emergency_authority_expansion).
narrative_ontology:affects_constraint(viral_transmission_rates, supply_chain_vulnerability).

% DUAL FORMULATION NOTE:
% The viral transmission constraint has two structurally distinct components: (1) biological_transmission_dynamics (ε≈0.08, Mountain) — the epidemiological reality of virus spreading through populations is an immutable physical constraint. (2) socio_political_response (ε≈0.52, Tangled Rope) — the policy choices made to manage transmission are contingent institutional arrangements. The JSON story addresses only the socio-political response. The biological constraint is a separate mountain story (not included here). The socio-political response is downstream of but structurally independent from the biological reality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(viral_transmission_rates, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
