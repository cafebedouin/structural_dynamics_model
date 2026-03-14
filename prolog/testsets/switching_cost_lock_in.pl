% ============================================================================
% CONSTRAINT STORY: switching_cost_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_switching_cost_lock_in, []).

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
 *   constraint_id: switching_cost_lock_in
 *   human_readable: Switching Cost Lock-In in Platform and Service Markets
 *   domain: economic/institutional
 *
 * SUMMARY:
 *   Switching cost lock-in operates when a provider designs or exploits
 *   system features that make migration to alternatives expensive for
 *   customers, extracting economic rent beyond what competitive pricing would
 *   allow. The constraint combines genuine coordination (system integration,
 *   data persistence, learning curves) with intentional or
 *   structurally-enabled extraction (proprietary formats, incompatible APIs,
 *   contract lock-ins, high migration friction). This makes it a canonical
 *   Tangled Rope: the coordinate function is real (systems do require
 *   continuity and integration), but the extraction mechanism is asymmetric
 *   (the incumbent captures the benefit of lock-in while the customer bears
 *   the cost). The constraint varies dramatically across perspectives: the
 *   locked-in customer experiences it as a Snare with no exit; the incumbent
 *   provider experiences it as pure Rope coordination; regulators see a
 *   temporary market failure with a policy sunset; and the analytical
 *   observer risks naturalizing it as an inevitable feature of any durable
 *   system.
 *
 * KEY AGENTS:
 *   - Locked-In Customer: Primary victim (powerless/trapped) — faces prohibitive switching costs with no realistic alternatives; bears full extraction cost
 *   - Incumbent Provider: Primary beneficiary (institutional/arbitrage) — captures switching cost rents; experiences constraint as coordination benefit
 *   - Competing Alternative Provider: Secondary actor (moderate/constrained) — benefits from lower overall market prices and innovating but constrained from accessing locked-in customer base
 *   - Regulatory Coalition: Organized agent (organized/constrained) — implements data portability and interoperability mandates to reduce switching costs; sees constraint as temporary market failure with sunset
 *   - Legacy Infrastructure Standard: Institutional actor (institutional/arbitrage) — maintains outdated technical standards that justify continued switching costs; benefits from inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent design choices as inevitable features of durable systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(switching_cost_lock_in, 0.58).
domain_priors:suppression_score(switching_cost_lock_in, 0.65).
domain_priors:theater_ratio(switching_cost_lock_in, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(switching_cost_lock_in, extractiveness, 0.58).
narrative_ontology:constraint_metric(switching_cost_lock_in, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(switching_cost_lock_in, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(switching_cost_lock_in, tangled_rope).
narrative_ontology:human_readable(switching_cost_lock_in, "Switching Cost Lock-In in Platform and Service Markets").
narrative_ontology:topic_domain(switching_cost_lock_in, "economic/institutional").

domain_priors:requires_active_enforcement(switching_cost_lock_in).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(switching_cost_lock_in, incumbent_provider).
narrative_ontology:constraint_beneficiary(switching_cost_lock_in, switching_cost_creator).
narrative_ontology:constraint_victim(switching_cost_lock_in, locked_in_customer).
narrative_ontology:constraint_victim(switching_cost_lock_in, market_efficiency).
narrative_ontology:constraint_victim(switching_cost_lock_in, consumer_choice).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCKED-IN CUSTOMER (SNARE) — Customer faces prohibitive switching costs (data migration, retraining, contract penalties, compatibility barriers) with no realistic exit. Trapped by accumulated sunk costs and incompatibility with alternatives. Maximum extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(switching_cost_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INCUMBENT PROVIDER (ROPE) — Benefits from coordination function (integrating services, providing continuity) while capturing switching cost rents. Experiences constraint as pure coordination benefit — the switching cost mechanism both enables service delivery and extracts customer surplus. Net beneficiary with full exit optionality.
constraint_indexing:constraint_classification(switching_cost_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: COMPETING ALTERNATIVE PROVIDER (TANGLED ROPE) — Benefits from coordination in the broader market (interoperability standards, data portability norms) but constrained by incumbent's switching cost barriers. Faces extraction through customer unavailability despite competitive quality. Some agency (can innovate, can lower prices) but significant constraint from incumbent's lock-in mechanism.
constraint_indexing:constraint_classification(switching_cost_lock_in, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY COALITION (SCAFFOLD) — Organized regulators and consumer advocates see switching costs as a temporary market failure with a policy sunset: data portability requirements, interoperability mandates, and open standards create a time-bounded exit pathway. The constraint has active enforcement requirements (incumbent compliance) that decline over the policy horizon as switching costs artificially engineered through incompatibility become illegal. Theater ratio is moderate because regulation creates genuine coordination (not purely performative).
constraint_indexing:constraint_classification(switching_cost_lock_in, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGACY INFRASTRUCTURE STANDARD (PITON) — Decades-old technical standards (file formats, APIs, protocols) that created switching costs through inertia rather than active enforcement. The standards persist long after cheaper alternatives exist; vendors maintain backward compatibility theatrically to justify continued lock-in. Theater ratio high (theatrical legacy support) but extractiveness low (costs are sunk). The constraint is maintained through institutional habit, not because it solves a current coordination problem.
constraint_indexing:constraint_classification(switching_cost_lock_in, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, switching costs are presented as an inevitable feature of any durable system: any service with user-specific configuration, data, or learning curve naturally creates path-dependent lock-in. This perspective naturalizes what are contingent design choices (proprietary data formats, incompatible APIs, intentional interoperability barriers). The engine's false summit detector will identify this as naturalization of institutional arrangement.
constraint_indexing:constraint_classification(switching_cost_lock_in, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(switching_cost_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(switching_cost_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(switching_cost_lock_in, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(switching_cost_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(switching_cost_lock_in, TR),
    TR >= 0.70.

:- end_tests(switching_cost_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. The incumbent provider captures significant economic rent through switching cost mechanisms above what competitive markets would permit. Customers cannot freely exit despite potentially better alternatives because the migration cost (data conversion, system retraining, operational disruption, contractual penalties) is prohibitive. The extractiveness has risen from 0.35 to 0.58 over the interval as platforms have deepened integration and tightened proprietary lock-in, making migration technically more complex and financially more expensive. This increase reflects intentional design choices to raise switching barriers, not legitimate coordination complexity growth. Suppression (0.65): High. Multiple barriers prevent exit: technical incompatibility (proprietary data formats, API lock-in), contractual constraints (minimum terms, termination penalties), sunk investment (training, customization, data entry), and information asymmetry (switching costs not fully disclosed at point-of-purchase). The suppression is partially structural (genuine technical complexity) and partially extractive (intentional incompatibility). Theater ratio (0.48): Moderate. The incumbent may perform or exaggerate the necessity of their proprietary system to justify continued switching costs, but there is genuine coordination function — platforms do provide real integration and continuity benefits. Theater is lower than in pure Piton cases because the coordination has authentic value; it's not purely theatrical maintenance of a degraded system.
 *
 * PERSPECTIVAL GAP:
 *   The locked-in customer (Snare perspective) sees only extraction: they cannot leave and bear the full cost of the lock-in mechanism. The incumbent provider (Rope perspective) sees pure coordination: their system solves a real integration problem, and switching costs are a natural feature of any durable platform. The competing alternative (Tangled Rope perspective) sees mixed extraction and coordination: they can build better services but cannot access customers because switching costs trap them in the incumbent's system. Regulators (Scaffold perspective) see a temporary market failure with a policy-driven sunset: interoperability mandates and data portability requirements (GDPR, DMA, open banking standards) are constructing alternative exit pathways that will eventually dissolve the lock-in. The legacy infrastructure observer (Piton perspective) sees a degraded system maintained through inertia: the technical standards that justified switching costs are decades old, and continued incompatibility is theatrical justification for continued rent extraction. The analytical observer (Mountain perspective) risks naturalizing the constraint: 'any system creates switching costs, this is inherent to how durable platforms work,' which obscures that the magnitude and character of switching costs are design choices, not laws of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   The incumbent provider's directionality (d) is low — they are a beneficiary with arbitrage exit options (can raise prices, can maintain monopoly without fear of losing customers due to high switching costs). Derived d ≈ 0.15 (beneficiary + institutional power + arbitrage exit) → f(d) ≈ -0.01 (near-zero or negative effective extraction experienced by them). The locked-in customer's directionality is high — they are a victim with trapped exit options (cannot leave without severe cost, no realistic alternatives). Derived d ≈ 0.95 (victim + powerless + trapped exit) → f(d) ≈ 1.42 (maximum extraction experienced). The competing provider's directionality is intermediate — they can exit the customer market by focusing on untapped segments, but are constrained in accessing the locked-in base. Derived d ≈ 0.60 (constrained exit, competitor position) → f(d) ≈ 0.85 (significant but not maximal extraction relative to their position).
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLVED: The constraint satisfies all three gates for Tangled Rope classification. (1) Genuine coordination function: Switching costs arise partly from legitimate system integration, data persistence, and service continuity — customers do benefit from platform coherence. (2) Asymmetric extraction: The incumbent provider captures disproportionate rent through switching cost mechanisms; customers bear costs they did not anticipate. (3) Active enforcement required: The incumbent must actively maintain incompatibility (resist standard formats, block data export, impose contractual barriers) to sustain switching costs — without active enforcement, customers would defect to cheaper alternatives. The mandatrophy is resolved by observing that this is NOT a pure Snare disguised as coordination, nor a pure Rope with accidental barriers. It is structurally both: the constraint genuinely coordinates platform services AND asymmetrically extracts customer surplus. The Snare perspective (locked-in customer) is correct from their vantage point; the Rope perspective (incumbent) is correct from theirs. The classification unifies both through Tangled Rope, which admits both coordination and extraction as structural features.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentional_vs_emergent_barriers,
    'What proportion of switching costs arise from intentional design choices vs emergent from legitimate coordination complexity?',
    'Comparative analysis of switching cost structure across providers with different interoperability philosophies; measurement of switching friction in open-standard vs proprietary ecosystems',
    'If mostly intentional: classification shifts toward Snare across more perspectives. If mostly emergent: classification shifts toward Rope (legitimate coordination cost) and the extraction component shrinks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentional_vs_emergent_barriers, empirical, 'Intentional vs emergent switching cost mechanisms').

omega_variable(
    interoperability_feasibility,
    'Is true platform-agnostic interoperability technically achievable at acceptable cost, or does the switching cost structure reflect irreducible technical constraints?',
    'Case studies of successful data migration and format conversion; analysis of failed interoperability initiatives; technical feasibility assessments from independent experts',
    'If feasible: switching costs are extractive choices, not natural constraints. If infeasible: switching costs are partially justified and classification shifts toward legitimate coordination (Rope gains ground). If partially feasible: Tangled Rope classification is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interoperability_feasibility, empirical, 'Technical feasibility of true interoperability').

omega_variable(
    customer_awareness_and_decision_quality,
    'Do customers make fully-informed switching cost calculations when selecting providers, or does switching cost lock-in exploit bounded rationality and attention constraints?',
    'Customer surveys measuring switching cost awareness at point-of-purchase; comparison of stated switching costs vs actual experienced friction; analysis of contract clarity and disclosure practices',
    'If aware and rational: extraction is compensated by service value (Rope classification more justified). If exploited by bounded rationality: suppression mechanism is stronger and classification shifts toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customer_awareness_and_decision_quality, empirical, 'Customer awareness and decision quality in switching cost calculation').

omega_variable(
    regulatory_sunset_realism,
    'Can interoperability and data portability regulations (GDPR, DMA, interoperability mandates) actually achieve the policy goal of reducing switching costs, or do they face technical implementation barriers?',
    'Longitudinal tracking of switching cost decline post-regulation; measurement of data portability utilization rates; analysis of compliance implementation vs regulatory intent',
    'If effective: Scaffold classification is justified, sunset is real, and the constraint has a policy-driven endpoint. If ineffective: Scaffold perspective is aspirational and the constraint persists despite regulation (Piton gains ground).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_sunset_realism, empirical, 'Effectiveness of regulatory switching cost reduction measures').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(switching_cost_lock_in, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(switch_tr_t0, switching_cost_lock_in, theater_ratio, 0, 0.42).
narrative_ontology:measurement(switch_tr_t10, switching_cost_lock_in, theater_ratio, 10, 0.45).
narrative_ontology:measurement(switch_tr_t20, switching_cost_lock_in, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(switch_be_t0, switching_cost_lock_in, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(switch_be_t10, switching_cost_lock_in, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(switch_be_t20, switching_cost_lock_in, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(switching_cost_lock_in, resource_allocation).
narrative_ontology:affects_constraint(switching_cost_lock_in, vendor_lock_in_it_services).
narrative_ontology:affects_constraint(switching_cost_lock_in, standards_incompatibility_extraction).
narrative_ontology:affects_constraint(switching_cost_lock_in, data_portability_bottleneck).

% DUAL FORMULATION NOTE:
% Switching cost lock-in is an upstream constraint affecting multiple downstream market-specific instantiations: vendor lock-in in IT services (SaaS platforms, cloud services), standards incompatibility in software ecosystems, and data portability barriers in consumer platforms. Each downstream constraint has domain-specific extractiveness values reflecting sector-specific technical and regulatory realities, but all depend on the underlying switching cost mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
