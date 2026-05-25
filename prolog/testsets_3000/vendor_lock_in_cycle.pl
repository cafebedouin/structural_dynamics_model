% ============================================================================
% CONSTRAINT STORY: vendor_lock_in_cycle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vendor_lock_in_cycle, []).

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
 *   constraint_id: vendor_lock_in_cycle
 *   human_readable: Vendor Lock-In Cycle
 *   domain: technology/economics/business
 *
 * SUMMARY:
 *   Vendor lock-in represents a structural extraction cycle where customers
 *   accumulate switching costs (data portability barriers, custom
 *   integrations, organizational process dependency, retraining burden,
 *   contract penalties) that create asymmetric exit costs favoring the
 *   incumbent vendor. The constraint exhibits coordination elements (vendor
 *   provides integration services, ecosystem support, feature development)
 *   alongside extraction elements (price increases leveraging switching-cost
 *   barriers, feature stagnation, forced upgrades, proprietary data formats).
 *   The lock-in cycle is not static — extractiveness has increased from 0.35
 *   to 0.58 over the measurement interval as vendors progressively build
 *   architectural barriers (APIs locked to proprietary ecosystems, data
 *   formats resistant to migration, licensing terms penalizing departure).
 *   Theater ratio increased from 0.38 to 0.52, reflecting growing
 *   performative elements: vendor marketing emphasizing ecosystem lock-in as
 *   'strategic value', bogus 'open' APIs with hidden proprietary extensions,
 *   theatrical interoperability commitments without actual implementation.
 *   The constraint demonstrates all six DR types: pure extraction for
 *   powerless trapped customers, mixed coordination-extraction for
 *   constrained enterprises, pure coordination for the beneficiary vendor,
 *   temporary problems with architectural solutions for organized standards
 *   coalitions, degraded ecosystems for legacy platforms, and risks of
 *   naturalization for analysts viewing lock-in as inherent to complex
 *   software.
 *
 * KEY AGENTS:
 *   - Incumbent Vendor: Primary beneficiary (institutional/arbitrage) — captures lock-in rents through pricing power and feature prioritization; can exit to alternative strategies without penalty
 *   - Locked-In Customer Base: Primary victim (powerless/trapped) — faces prohibitive switching costs; cannot exit without severe operational disruption; pays lock-in premium indefinitely
 *   - Enterprise Clients: Secondary victim (moderate/constrained) — experience genuine coordination services alongside extraction; face high but theoretically surmountable switching costs; some organizational agency to negotiate terms
 *   - Open Standards Coalition: Organized actors — open-source foundations, interoperability consortia, data portability advocates building architectural alternatives; create sunset pathways through standards maturation and regulatory mandates
 *   - Legacy Vendor Ecosystem: Institutional actor — older platforms persisting through inertia; customers locked in by sunk costs not by current feature superiority; extraction maintained through switching-cost barriers alone (piton mechanism)
 *   - Regulatory Bodies: Emerging institutional actors — GDPR Article 20 (data portability), Digital Markets Act (interoperability mandates), equivalent regulations creating compliance requirements that enable exit pathways
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vendor_lock_in_cycle, 0.58).
domain_priors:suppression_score(vendor_lock_in_cycle, 0.65).
domain_priors:theater_ratio(vendor_lock_in_cycle, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vendor_lock_in_cycle, extractiveness, 0.58).
narrative_ontology:constraint_metric(vendor_lock_in_cycle, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(vendor_lock_in_cycle, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vendor_lock_in_cycle, tangled_rope).
narrative_ontology:human_readable(vendor_lock_in_cycle, "Vendor Lock-In Cycle").
narrative_ontology:topic_domain(vendor_lock_in_cycle, "technology/economics/business").

domain_priors:requires_active_enforcement(vendor_lock_in_cycle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vendor_lock_in_cycle, incumbent_vendor).
narrative_ontology:constraint_victim(vendor_lock_in_cycle, customer_base).
narrative_ontology:constraint_victim(vendor_lock_in_cycle, market_competition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCKED-IN CUSTOMER (SNARE) — Faces prohibitive switching costs (data migration, retraining, contract penalties, compatibility disruption). Cannot exit without severe operational disruption. Experiences pure extraction with minimal coordination benefit. Bears full cost of vendor price increases and feature stagnation.
constraint_indexing:constraint_classification(vendor_lock_in_cycle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ENTERPRISE CLIENT (TANGLED ROPE) — Experiences genuine coordination: vendor provides ongoing support, integration services, and ecosystem access. Also experiences asymmetric extraction: switching costs create price leverage for the vendor. Mixed costs and benefits — can theoretically exit but faces significant friction.
constraint_indexing:constraint_classification(vendor_lock_in_cycle, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT VENDOR (ROPE) — Experiences the lock-in as coordination mechanism: customer switching costs create reliable revenue stream and long-term partnership stability. Can exit (to alternative vendor strategies) without penalty. Net beneficiary — extraction flows toward the vendor.
constraint_indexing:constraint_classification(vendor_lock_in_cycle, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN STANDARDS COALITION (SCAFFOLD) — Organized actors (open-source foundations, interoperability consortia, data portability advocates) see lock-in as a temporary coordination failure with architectural solutions: API standardization, data portability regulations (GDPR, DMA), and open-source alternatives create exit pathways. Sunset mechanism: as standards mature and regulatory mandates take effect, proprietary lock-in loses force.
constraint_indexing:constraint_classification(vendor_lock_in_cycle, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY VENDOR ECOSYSTEM (PITON) — Older enterprise platforms (mainframe vendors, legacy ERP systems) continue extracting through lock-in despite degraded functionality. Customers remain locked-in by massive switching costs and organizational inertia, not by superior product features. The constraint persists through institutional momentum rather than current functional coordination. Theater ratio high because the extraction mechanism is maintenance of switching-cost barriers, not delivery of exceptional value.
constraint_indexing:constraint_classification(vendor_lock_in_cycle, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational scale, some vendor-customer coordination cost and switching inertia may appear inherent to complex software ecosystems: integration, customization, and operational dependency are genuine coordination mechanisms. This perspective risks naturalizing contingent institutional arrangements (proprietary APIs, closed data formats, legal contract lock-in) as laws of technology. The engine's false summit detector will flag this as naturalization of extractive design choices.
constraint_indexing:constraint_classification(vendor_lock_in_cycle, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vendor_lock_in_cycle_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(vendor_lock_in_cycle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(vendor_lock_in_cycle, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(vendor_lock_in_cycle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(vendor_lock_in_cycle, TR),
    TR >= 0.70.

:- end_tests(vendor_lock_in_cycle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Lock-in creates genuine price leverage for vendors — customers accept above-market pricing because switching costs exceed the price premium. But extraction is not total (0.66+) because some coordination value persists: vendors do provide integration, feature development, and ecosystem maintenance. The constraint is hybrid. Suppression (0.65): High. Multiple suppression mechanisms: technical (proprietary APIs, closed data formats), legal (contract lock-in, license restrictions), organizational (switching requires enterprise-wide retraining, disruption to operations), and informational (vendors obscure true switching costs, market conditions make alternatives appear risky). Customers face multiple barriers to exit. Theater ratio (0.52): Moderate-high. Vendor marketing emphasizes lock-in as 'partnership value' and 'strategic alignment' rather than extraction. APIs are labeled 'open' while remaining proprietary. Interoperability commitments are announced without implementation. The theater has increased over time as sophisticated vendors learned to frame extraction as coordination. Claimed type (Tangled Rope): Justified by both genuine coordination function (vendor provides real services) AND asymmetric extraction (lock-in creates pricing leverage and feature stagnation). Active enforcement is required — vendors must continuously maintain switching-cost barriers (proprietary APIs, incompatible data formats, licensing restrictions) for extraction to persist.
 *
 * PERSPECTIVAL GAP:
 *   The locked-in customer sees pure extraction (Snare) — they perceive no coordination benefit, only switching-cost penalty. The enterprise client sees mixed coordination-extraction (Tangled Rope) — they recognize genuine vendor services while also experiencing lock-in leverage. The incumbent vendor sees pure coordination (Rope) — they frame the lock-in as customer investment in ecosystem and long-term partnership, not extraction. The standards coalition sees a temporary problem with architectural solutions (Scaffold) — they are actively building exit pathways through open standards. The legacy vendor ecosystem sees its own degradation (Piton) — older platforms persisting through inertia, no longer delivering exceptional value but too costly to leave. The analytical observer risks naturalizing lock-in as inherent to complex software (Mountain) — this is the false summit. The structural data contradicts naturalization: lock-in is largely architectural and legal design choice, not inherent constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies by agent position. The incumbent vendor is a beneficiary with arbitrage options (low d, negative chi) — they can exit to alternative vendor strategies without cost. The locked-in customer is a victim with trapped exit options (high d, high chi) — they perceive maximum extraction because they have no credible exit threat. The enterprise client is a victim with constrained exit options (d ≈ 0.65, moderate chi) — they have some theoretical mobility but face significant friction. The standards coalition has organizational power and some exit optionality (d ≈ 0.45, moderate chi) — their barrier is architectural immaturity and vendor resistance, not fundamental incapacity. Directionality overrides are not needed; the standard derivation chain produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   Vendor lock-in resolves mandatrophy by demonstrating that the same structural phenomenon (customer switching costs, vendor switching incentives, ecosystem coupling) produces different classifications depending on observer position. The powerless trapped customer sees Snare (pure extraction with no exit). The moderate constrained enterprise sees Tangled Rope (coordination with extraction). The beneficiary vendor sees Rope (coordination with beneficial outcomes). The organized standards coalition sees Scaffold (temporary problem with sunset). The legacy platform sees Piton (degraded extraction persisting through inertia). The analytical observer risks seeing Mountain (inherent to software) but this is false — switching costs are largely architectural choices, not laws of nature. The mandatrophy is not 'which type is correct' but 'which perspective captures the structural reality from which position?' All six are legitimate readings of different positions in the constraint network.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    switching_cost_composition,
    'What proportion of switching costs are inherent coordination costs versus deliberate architectural extraction?',
    'Comparative analysis: switching costs for proprietary platforms vs open-standard platforms; architectural audit of data portability barriers; customer interviews on perceived vs actual migration friction',
    'If mostly inherent: lock-in is Rope (legitimate coordination). If mostly deliberate: lock-in is Snare (pure extraction). Current assessment (Tangled Rope) assumes mixed composition; resolution refines classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_composition, empirical, 'Proportion of switching costs that are inherent vs architectural extraction').

omega_variable(
    interoperability_standard_viability,
    'Can open interoperability standards (APIs, data formats, cross-platform protocols) actually achieve functional parity with proprietary lock-in ecosystems?',
    'Performance benchmarks comparing standard-based platforms to proprietary equivalents; adoption rates for open-standard alternatives; customer migration success rates and post-migration satisfaction',
    'If yes: scaffold sunset is real and achievable. If no: open standards remain aspirational and lock-in persists indefinitely as effective snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interoperability_standard_viability, empirical, 'Whether open standards can achieve functional parity with proprietary platforms').

omega_variable(
    regulatory_compliance_enforcement,
    'Will data portability and interoperability regulations (DMA, GDPR Article 20, successor mandates) actually reduce lock-in, or will vendors find compliance-minimal workarounds?',
    'Post-regulation audit: measurement of actual data portability rates, customer switching rates, and vendor API compliance depth; analysis of regulatory enforcement mechanisms and penalty structures',
    'If enforced effectively: regulatory sunset enables scaffold transition. If compliance-minimal: regulations become theater and lock-in persists (piton perspective confirmed).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_compliance_enforcement, empirical, 'Whether regulatory compliance will reduce lock-in or trigger workarounds').

omega_variable(
    cloud_migration_lock_in_tradeoff,
    'Does cloud migration (moving to cloud-native platforms) reduce lock-in to traditional vendors or create new lock-in to cloud platform providers?',
    'Comparative lock-in analysis: traditional enterprise software vs cloud infrastructure providers; measurement of switching costs between cloud providers; customer lock-in perception before/after migration',
    'If lock-in merely transfers: the constraint family expands (vendor_lock_in_cycle is upstream of cloud_provider_lock_in). If lock-in reduces: cloud enables scaffold transition (positive). If lock-in increases: cloud architecture is net-negative for customer freedom.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cloud_migration_lock_in_tradeoff, empirical, 'Whether cloud migration reduces or transfers lock-in').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vendor_lock_in_cycle, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vlock_tr_t0, vendor_lock_in_cycle, theater_ratio, 0, 0.38).
narrative_ontology:measurement(vlock_tr_t3, vendor_lock_in_cycle, theater_ratio, 3, 0.45).
narrative_ontology:measurement(vlock_tr_t6, vendor_lock_in_cycle, theater_ratio, 6, 0.52).

% Extraction over time
narrative_ontology:measurement(vlock_be_t0, vendor_lock_in_cycle, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vlock_be_t3, vendor_lock_in_cycle, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(vlock_be_t6, vendor_lock_in_cycle, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vendor_lock_in_cycle, resource_allocation).
narrative_ontology:affects_constraint(vendor_lock_in_cycle, cloud_provider_lock_in).
narrative_ontology:affects_constraint(vendor_lock_in_cycle, proprietary_api_ecosystem).
narrative_ontology:affects_constraint(vendor_lock_in_cycle, data_portability_barrier).

% DUAL FORMULATION NOTE:
% Vendor lock-in is downstream of specific architectural choices (proprietary APIs, closed data formats, licensing lock-in) but represents a distinct structural constraint. The upstream constraints have their own extractiveness values reflecting technical implementation details; vendor_lock_in_cycle aggregates these into a customer-experienced economic constraint with its own dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
