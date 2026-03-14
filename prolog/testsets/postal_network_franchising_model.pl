% ============================================================================
% CONSTRAINT STORY: postal_network_franchising_model
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_postal_network_franchising_model, []).

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
 *   constraint_id: postal_network_franchising_model
 *   human_readable: Postal Network Franchising Model
 *   domain: infrastructure/logistics/public_goods
 *
 * SUMMARY:
 *   The postal network franchising model represents a hybrid institutional
 *   arrangement where a central postal authority delegates last-mile delivery
 *   to distributed franchise operators, typically within geographically
 *   defined territories. The model emerged in countries seeking to maintain
 *   universal service obligations (rural coverage) while reducing direct
 *   state operational burden. It coordinates the logistical challenge of
 *   reaching low-density areas through entrepreneurial incentive structures,
 *   but this coordination function is layered with extraction mechanisms:
 *   franchise fees, regulatory control, and asymmetric risk distribution. The
 *   constraint's theater ratio (0.58) reflects the performative aspects of
 *   universal service regulation — mandatory obligations that franchisees
 *   navigate through service quality degradation rather than outright
 *   refusal. Rural and small-town communities experience severe suppression
 *   (0.65): they are geographically trapped and regulated to depend on
 *   franchised service, with minimal alternatives and no negotiating power.
 *   Franchise operators experience genuine coordination benefits (lower d,
 *   negative chi from institutional perspective) alongside extraction of
 *   rural routes. The postal authority occupies the hybrid position: it must
 *   coordinate universal service (genuine function) while capturing franchise
 *   revenue and regulatory rents (extraction).
 *
 * KEY AGENTS:
 *   - Franchise Operators: Primary beneficiary (institutional/arbitrage) — capture urban route profitability; navigate regulatory obligations through discretionary service management
 *   - Central Postal Authority: Coordinator and secondary beneficiary (organized/constrained) — collects franchise fees and maintains regulatory control; depends on franchise model for budget stability
 *   - Rural Communities and Small Towns: Primary victims (powerless/trapped and moderate/constrained) — geographically dependent on franchised service; suppressed by lack of alternatives and regulatory obligations that bind operators but lack enforcement teeth
 *   - Large Commercial Shippers: Secondary beneficiary (powerful/mobile) — arbitrage between postal and private logistics; experience piton-level constraints from performative universal service obligations
 *   - Mail Customers in Unprofitable Areas: Dispersed victims (moderate/constrained) — experience service quality degradation as franchisees minimize costs; benefit from receiving any service but constrained by lack of choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(postal_network_franchising_model, 0.52).
domain_priors:suppression_score(postal_network_franchising_model, 0.65).
domain_priors:theater_ratio(postal_network_franchising_model, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(postal_network_franchising_model, extractiveness, 0.52).
narrative_ontology:constraint_metric(postal_network_franchising_model, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(postal_network_franchising_model, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(postal_network_franchising_model, tangled_rope).
narrative_ontology:human_readable(postal_network_franchising_model, "Postal Network Franchising Model").
narrative_ontology:topic_domain(postal_network_franchising_model, "infrastructure/logistics/public_goods").

domain_priors:requires_active_enforcement(postal_network_franchising_model).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(postal_network_franchising_model, franchise_operators).
narrative_ontology:constraint_beneficiary(postal_network_franchising_model, central_postal_authority).
narrative_ontology:constraint_victim(postal_network_franchising_model, rural_service_areas).
narrative_ontology:constraint_victim(postal_network_franchising_model, universal_access_recipients).
narrative_ontology:constraint_victim(postal_network_franchising_model, small_towns).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Rural and underserved communities perceive the franchising model as pure extraction. They are trapped by geography and regulatory obligation — postal operators must service unprofitable routes, but franchising creates incentive structures where operators minimize service quality to unprofitable areas. Exit is impossible: communities cannot obtain service elsewhere, and operators bear no cost for degradation.
constraint_indexing:constraint_classification(postal_network_franchising_model, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% Franchise operators experience the model as coordination with benefit. They solve the logistics problem of distributed mail delivery while capturing revenue from profitable urban routes. Exit options include expanding service territory, consolidating operations, or pivoting to parcel logistics. The constraint functions as genuine coordination: operators must maintain network coverage to retain franchise; the maintenance discipline enables the postal authority's universal service mandate.
constraint_indexing:constraint_classification(postal_network_franchising_model, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% The postal authority occupies a hybrid position. It genuinely coordinates universal service through franchising — the mechanism enables coverage that a monolithic state operation might not achieve. But it also extracts: franchise fees, performance requirements, and regulatory control create asymmetric returns. The authority faces constraints: loss of franchise fees disrupts its budget model, but maintaining too much extraction pressure on operators risks service collapse.
constraint_indexing:constraint_classification(postal_network_franchising_model, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Small towns experience both coordination and extraction. The franchising model coordinates mail delivery to areas that pure market pricing would abandon. But customers are constrained: they depend on franchised operator quality, face limited choice, and bear costs if the operator decides to reduce hours or service scope. Some benefit from the system (receiving any service at all); others bear extraction if service degrades.
constraint_indexing:constraint_classification(postal_network_franchising_model, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Large shippers see the franchising model as largely performative theater. They have alternative logistics options (private couriers, integrated logistics) and use postal franchises opportunistically. The postal obligation to maintain rural service appears to them as a regulatory constraint, but they are mobile — they can arbitrage between postal and private logistics. The franchising system's ritual commitment to universal service carries minimal operational cost for large shippers, who effectively bypass it.
constraint_indexing:constraint_classification(postal_network_franchising_model, piton,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% The analytical observer at civilizational scope risks identifying the franchising model as a natural law of postal logistics: that geographically distributed last-mile delivery in low-density areas is inherently uneconomical and therefore requires subsidy or forced cross-subsidization. This perspective naturalizes the tension between universal service and cost recovery. The structural data contradicts this naturalization — the tension is institutional (franchise incentive structures, regulatory mandate design), not physical or mathematical.
constraint_indexing:constraint_classification(postal_network_franchising_model, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(postal_network_franchising_model_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(postal_network_franchising_model, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(postal_network_franchising_model, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(postal_network_franchising_model, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(postal_network_franchising_model, TR),
    TR >= 0.70.

:- end_tests(postal_network_franchising_model_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The model shows increasing extractiveness over the 15-year interval (0.35 → 0.52), reflecting accumulating franchise fees, regulatory rents, and franchisee incentive drift. The underlying structure is mixed: genuine coordination (franchising solves the last-mile problem) combined with extraction (franchisees profit from mandatory rural routes via cross-subsidy, postal authority captures ongoing fees). The trajectory shows classic rent-seeking accumulation: initial efficiency gains (franchising is cheaper than state operation) followed by fee and regulatory pressure increases. Suppression (0.65): High. Rural communities face genuine barriers: geographic isolation, regulatory obligation to depend on single franchisee, no exit options, and weak enforcement of service quality. Urban competitors use private alternatives; rural communities do not have this option. The suppression mechanism is structural (geography + regulation + market absence), not primarily coercive (no threats or violence), which places it in the tangled_rope/snare range. Theater ratio (0.58, increasing to 0.65): Moderate-high. The performative aspect is in universal service regulation: postal authorities mandate service to unprofitable routes, but enforcement is weak (creating piton-like dynamics), and franchisees manage obligations through service degradation rather than exit. The trend toward higher theater reflects increasing reliance on performative compliance rather than genuine service — posted hours that are not staffed, service points that are understaffed, processing delays that are within regulation but exceed customer expectations.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single institutional arrangement can be beneficial coordination for one agent (franchisee) and destructive extraction for another (rural community). The franchisee's rope classification reflects genuine solving of the collective action problem: without franchising incentives, who would serve unprofitable routes? The rural community's snare classification reflects genuine extraction: they are trapped and bear costs of degradation with no exit. The postal authority's tangled_rope classification reflects the hybrid function: it genuinely solves the universal service coordination problem (without it, rural service might collapse entirely), but it also extracts franchise rents and regulatory control. The theater ratio increase (0.42 → 0.65) suggests the coordination function is degrading relative to the performative function — the model is increasingly a mechanism for extracting fees while maintaining the appearance of universal service, rather than genuinely solving coordination. The piton perspective on large shippers suggests that the universal service obligation is becoming increasingly theatrical relative to actual impact on commercial logistics.
 *
 * DIRECTIONALITY LOGIC:
 *   Franchisees derive directionality from beneficiary status (profit from urban routes) combined with arbitrage exit options (can expand territory, pivot to parcels, exit franchising entirely). This yields low d, negative chi — they experience the constraint as enabling rather than extractive. The postal authority's directionality reflects organized power and constrained exit (budget model depends on franchise revenue), yielding moderate d. Rural communities' directionality reflects powerless status, trapped exit (no alternatives), and victim status (bear costs of service quality degradation), yielding high d and high chi. Large shippers' directionality combines powerful status and mobile exit (can use alternative logistics), yielding moderate-to-low d despite appearing as beneficiaries of postal infrastructure — they can arbitrage away. Small towns occupy intermediate positions depending on their specific economic profile: towns with some commercial activity experience constrained exit (could theoretically shift to private logistics at cost); very small towns experience trapped exit. The divergence reflects real structural differences in how the constraint operates across spatial scales and economic profiles.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that franchising can genuinely serve both coordination and extraction functions simultaneously. The coordination function is real: franchising enables rural service that a purely state operation might not provide and that pure market pricing would certainly abandon. The extraction function is also real: franchisees profit from cross-subsidization, postal authorities capture rents, and rural communities bear the cost of degraded service quality. The classification as Tangled Rope captures both simultaneously: the model is not pure coordination (would be Rope) nor pure extraction (would be Snare), but a hybrid where enforcement of the coordination mandate is weak (theater_ratio 0.58) and beneficiaries capture asymmetric returns (extractiveness 0.52). The increasing theater_ratio and extractiveness over time suggest mandatrophy drift: the model may be devolving from tangled_rope toward piton (performative compliance without functional coordination) or snare (pure extraction with coordination function abandoned). Monitoring the omega variables — whether alternatives emerge, whether enforcement strengthens, whether institutional lock-in persists — will determine if the constraint is stable tangled_rope or degrading toward extraction-dominated classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    profitable_route_extraction_magnitude,
    'What proportion of franchisee profit derives from cross-subsidization (urban-to-rural) vs. legitimate operational efficiency?',
    'Comparative analysis of franchisee margins on urban routes vs. mandatory rural routes; decomposition of profit drivers into scale economics, operational efficiency, and cross-subsidy capture',
    'If cross-subsidy exceeds 40% of urban profit: extraction is higher than base metrics suggest (reclassify tangled_rope toward snare). If below 20%: model functions more as coordination than extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(profitable_route_extraction_magnitude, empirical, 'Proportion of franchisee profit from cross-subsidization vs. efficiency').

omega_variable(
    rural_service_alternatives_emergence,
    'Are technological alternatives (drone delivery, local hub-and-spoke models, consolidated rural co-ops) becoming viable options that would deconstrain rural communities?',
    'Market analysis of emerging logistics models in low-density areas; pilot project outcomes; cost trajectories of alternative delivery technologies',
    'If alternatives become viable and cost-competitive: rural communities'' exit options upgrade from ''trapped'' to ''constrained'' or ''mobile'', changing classification from snare toward tangled_rope or rope. Franchising model transitions from extraction mechanism to voluntary coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rural_service_alternatives_emergence, empirical, 'Viability of technological alternatives for rural delivery').

omega_variable(
    regulatory_mandate_enforceability,
    'Can postal authorities actually enforce universal service obligations, or is enforcement itself the theater that operators navigate?',
    'Audit of service compliance in unprofitable regions; documentation of enforcement actions vs. service violations; franchisee response patterns to regulatory pressure',
    'If enforcement is weak: operators de facto choose service quality independent of mandate, eliminating the coordination function and pushing classification toward snare. If strong: franchising genuinely constrains operator behavior toward collective good, supporting tangled_rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_mandate_enforceability, empirical, 'Enforceability of universal service mandate').

omega_variable(
    institutional_dependency_lock,
    'Does the postal authority''s dependence on franchise fee revenue create path dependency that prevents regulatory reform even if reform would improve social welfare?',
    'Budget analysis showing franchise revenue as percentage of postal authority budget; modeling of authority financial stability under alternative funding mechanisms; historical cases of regulatory reform attempts',
    'If dependency is high and lock-in is institutional: the postal authority becomes an identity_locked agent at the institutional level, unable to perceive or implement less extractive alternatives. This changes directionality and may push postal_authority perspective from tangled_rope toward identity_locked snare (from analytical view).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_dependency_lock, conceptual, 'Institutional dependency lock on franchise revenue model').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(postal_network_franchising_model, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(postal_tr_t0, postal_network_franchising_model, theater_ratio, 0, 0.42).
narrative_ontology:measurement(postal_tr_t5, postal_network_franchising_model, theater_ratio, 5, 0.5).
narrative_ontology:measurement(postal_tr_t10, postal_network_franchising_model, theater_ratio, 10, 0.58).
narrative_ontology:measurement(postal_tr_t15, postal_network_franchising_model, theater_ratio, 15, 0.65).

% Extraction over time
narrative_ontology:measurement(postal_be_t0, postal_network_franchising_model, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(postal_be_t5, postal_network_franchising_model, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(postal_be_t10, postal_network_franchising_model, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(postal_be_t15, postal_network_franchising_model, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(postal_network_franchising_model, resource_allocation).
narrative_ontology:affects_constraint(postal_network_franchising_model, universal_service_obligation_logistics).
narrative_ontology:affects_constraint(postal_network_franchising_model, rural_infrastructure_access).
narrative_ontology:affects_constraint(postal_network_franchising_model, last_mile_delivery_economics).

% DUAL FORMULATION NOTE:
% The postal franchising model decomposes into three structurally related constraints: (1) universal_service_obligation_logistics (ε~0.15, coordination challenge at core), (2) postal_network_franchising_model (ε~0.52, hybrid institutional arrangement), and (3) rural_infrastructure_access (ε~0.68, impact on rural communities). The franchising model is downstream of the universal service obligation (which creates the coordination problem) and upstream of rural access (which experiences the impact). Each constraint has different extractiveness because each measures a different observable: the obligation itself is low-extraction (defining a coordination goal), the institutional mechanism is moderate-extraction (adding fees and regulatory rents), and the impact on rural actors is high-extraction (showing the burden distribution).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(postal_network_franchising_model, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
