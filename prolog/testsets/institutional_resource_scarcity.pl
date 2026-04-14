% ============================================================================
% CONSTRAINT STORY: institutional_resource_scarcity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_resource_scarcity, []).

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
 *   constraint_id: institutional_resource_scarcity
 *   human_readable: Institutional Resource Scarcity as Coordination Constraint
 *   domain: organizational_governance/institutional_economics
 *
 * SUMMARY:
 *   Institutional resource scarcity creates a structural coordination
 *   problem: finite resources require prioritization, but the prioritization
 *   mechanism becomes an extraction device when control over allocation
 *   criteria is asymmetrically distributed. This constraint exhibits the full
 *   range of DR classification depending on the observer's structural
 *   position within the institutional hierarchy. The same scarcity that
 *   enables coordination (rope perspective: priorities create focus and
 *   efficient resource use) simultaneously enables extraction (snare
 *   perspective: powerless agents are systematically excluded). The
 *   theater_ratio (0.64) reflects that formal allocation procedures often
 *   perform legitimation functions alongside their functional coordination
 *   role — committees review requests, criteria are applied, and decisions
 *   are made through documented processes, but these rituals often obscure
 *   the underlying power asymmetries that determine who gets prioritized. The
 *   extractiveness has increased over the measurement interval (0.38 → 0.52)
 *   as allocation pressure intensifies and informal workarounds become
 *   formalized into policy barriers.
 *
 * KEY AGENTS:
 *   - Excluded Claimants: Primary victims (powerless/trapped) — systematically denied access to resources through allocation criteria; bear extraction cost without institutional voice
 *   - Resource Allocating Authority: Primary beneficiary (institutional/arbitrage) — controls priority-setting and can shift allocation criteria; captures efficiency gains and political credit
 *   - Priority-Setting Coalition: Secondary beneficiary (organized/arbitrage) — insider groups whose preferences are built into allocation criteria; benefit from alignment with authority
 *   - Secondary Stakeholders: Mixed agent (moderate/constrained) — participate in allocation negotiations with limited power; experience both coordination benefits and extraction
 *   - Formal Allocation Procedure: Institutional actor (institutional/constrained) — maintains legitimacy through ritual adherence even as functional coordination capacity declines
 *   - Reform Coalition: Organized agents (organized/constrained) — building alternative mechanisms (participatory budgeting, mutual aid, transparent resource-sharing) with sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing policy-constructed scarcity as inherent resource limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_resource_scarcity, 0.52).
domain_priors:suppression_score(institutional_resource_scarcity, 0.58).
domain_priors:theater_ratio(institutional_resource_scarcity, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_resource_scarcity, extractiveness, 0.52).
narrative_ontology:constraint_metric(institutional_resource_scarcity, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(institutional_resource_scarcity, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_resource_scarcity, tangled_rope).
narrative_ontology:human_readable(institutional_resource_scarcity, "Institutional Resource Scarcity as Coordination Constraint").
narrative_ontology:topic_domain(institutional_resource_scarcity, "organizational_governance/institutional_economics").

domain_priors:requires_active_enforcement(institutional_resource_scarcity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_resource_scarcity, resource_allocating_authority).
narrative_ontology:constraint_beneficiary(institutional_resource_scarcity, priority_setting_coalition).
narrative_ontology:constraint_victim(institutional_resource_scarcity, excluded_claimants).
narrative_ontology:constraint_victim(institutional_resource_scarcity, service_access_inequality).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED CLAIMANT (SNARE) — Faces material barriers to accessing institutional resources. Cannot exit the institutional framework (legal dependency, geographic isolation, lack of alternatives). Bears full extraction cost without benefit. Suppression is structural — the claimant has no negotiating power and no path to voice preferences within allocation mechanisms.
constraint_indexing:constraint_classification(institutional_resource_scarcity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SECONDARY STAKEHOLDER (TANGLED ROPE) — Participates in allocation negotiations but with limited power. Experiences both coordination benefits (shared institutional infrastructure enables collective action) and extraction (priority system systematically favors other claimants). Can exit at significant cost (relocation, alternative institutions) but not easily. Mixed experience of the constraint.
constraint_indexing:constraint_classification(institutional_resource_scarcity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RESOURCE ALLOCATING AUTHORITY (ROPE) — Experiences the constraint primarily as coordination mechanism: scarcity enables prioritization, which solves the collective action problem of deciding which needs are most urgent. Benefits from first-mover advantage in setting criteria. Can arbitrage resources across multiple institutional contexts. Sees scarcity as enabling rather than constraining.
constraint_indexing:constraint_classification(institutional_resource_scarcity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FORMAL ALLOCATION PROCEDURE (PITON) — The documented process for resource allocation persists through institutional inertia even when its actual function has degraded. Theater_ratio is high (0.64) because much of the allocation process is performative: requests are submitted through formal channels, reviewed in committees, and approved/denied according to criteria that may not reflect actual scarcity or actual need. The procedure maintains legitimacy through ritual rather than effectiveness.
constraint_indexing:constraint_classification(institutional_resource_scarcity, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REFORM COALITION (SCAFFOLD) — Organized agents (advocacy groups, policy reformers, resource-sharing initiatives) perceive the scarcity constraint as temporary and solvable. They are building alternative allocation mechanisms (participatory budgeting, transparent resource pools, mutual aid networks) with sunset logic: as these alternatives mature and norms shift toward distributed allocation, the hierarchical scarcity-based model loses force. Low effective extraction because the coalition has agency and sees an exit path.
constraint_indexing:constraint_classification(institutional_resource_scarcity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some scarcity is inherent to finite resources and complex institutions: bounded rationality means no allocation system can perfectly distribute resources, and coordination always carries overhead costs. This perspective sees institutional scarcity as a natural law — inevitable given resource finitude and decision costs. However, the structural data reveals this as a false summit: the 'inherent' scarcity often reflects policy choices (funding levels, priority criteria, access rules) rather than physical limits.
constraint_indexing:constraint_classification(institutional_resource_scarcity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_resource_scarcity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_resource_scarcity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_resource_scarcity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_resource_scarcity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_resource_scarcity, TR),
    TR >= 0.70.

:- end_tests(institutional_resource_scarcity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts from excluded claimants (denied access) while benefiting the allocating authority (control over criteria, political credit). The value reflects that some extraction is genuine coordination cost (decision-making infrastructure has real overhead), but a significant portion flows asymmetrically to the authority and its coalition. The measurement trajectory (0.38 → 0.52) reflects intensifying pressure as institutional demands exceed explicit resource levels, forcing harsher criteria. Suppression (0.58): Moderate-high. Excluded claimants face multiple barriers: they lack voting power in allocation decisions, often lack information about criteria, face costs of appealing rejections, and have few exit options (institutional dependency is structural). Secondary stakeholders face lower suppression (can negotiate, can appeal, can relocate at cost). Theater ratio (0.64): Moderate-high. Allocation procedures perform legitimate-appearance functions alongside decision-making — formal criteria are applied, committees review requests, decisions are documented — but these rituals often obscure the power asymmetries that determine real priorities. The ratio has increased over the interval as procedures have become more formalized and elaborate, possibly to manage legitimacy as pressure intensifies.
 *
 * PERSPECTIVAL GAP:
 *   The excluded claimant and resource authority experience the same constraint with inverted extractiveness. The claimant sees pure extraction (snare) — denied access, no voice, no exit — because d ≈ 0.95 produces high f(d) ≈ 1.42. The authority sees coordination (rope) — priorities enable focus, shared infrastructure, efficient decision-making — because d ≈ 0.10 produces low f(d) ≈ -0.01. Neither perspective is wrong; they are measuring from structurally different positions. The piton classification reveals that the formal allocation procedure persists through legitimation ritual even as its functional coordination capacity degrades — committees continue to meet, criteria continue to be applied, decisions continue to be documented, but the actual decisions are increasingly determined by power asymmetries rather than the stated criteria. The scaffold perspective shows that organized reform efforts (participatory budgeting, transparent resource-sharing, mutual aid networks) are building alternative exit paths with sunset logic — as these alternatives mature and norms shift, the hierarchical scarcity-based model loses force. The mountain perspective risks naturalizing policy-constructed scarcity as inherent resource limits, concealing that the 'inevitable' nature of the constraint reflects institutional choices (funding levels, priority criteria, access rules) rather than physical finitude.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) flows from each agent's structural position relative to the resource flow. Excluded claimants (powerless/trapped) derive d ≈ 0.95: they bear extraction, have no exit, and cannot negotiate. The allocating authority (institutional/arbitrage) derives d ≈ 0.10: they benefit from the constraint, have high exit mobility (can shift to other institutions), and control criteria. Secondary stakeholders (moderate/constrained) derive d ≈ 0.60: mixed position — they participate and benefit from coordination but face constraints. The reform coalition (organized/constrained) derives d ≈ 0.45: they see the constraint as temporary and solvable, reducing experienced extraction. These directionality values feed the sigmoid f(d) to produce effective extractiveness chi for each perspective, explaining the perspectival gap: the excluded claimant experiences high chi (snare), the authority experiences low chi (rope), and the analytical observer risks naturalizing the constraint as invariant (false summit).
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: This constraint resolves the mandatrophy by demonstrating how the same structural phenomenon (resource scarcity) appears as pure coordination (rope), mixed coordination-extraction (tangled_rope), pure extraction (snare), degraded ritual (piton), temporary problem with exit path (scaffold), or natural law (mountain), depending on the observer's position. The mandatrophy is not 'which type is correct?' but 'which structural relationship are you embedded in?' The allocating authority's rope is their genuine experience (scarcity enables coordination and gives them decision power). The excluded claimant's snare is their structural reality (systematic exclusion, no voice, no exit). The piton is a real observation (formal procedures persist through ritual even as functional capacity declines). The scaffold is real structure (alternative mechanisms are emerging with genuine viability). The mountain is a false summit (constructed scarcity masquerading as natural limit). All six readings are locally correct — the challenge is determining which structural features are primary and which are secondary, and whether the constraint is degrading (piton trajectory), being replaced (scaffold trajectory), or persisting (snare trajectory).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scarcity_constructed_vs_inherent,
    'Is the measured resource scarcity inherent to physical limits or constructed by institutional allocation choices?',
    'Historical comparison of resource availability vs. institutional access; analysis of allocation criteria changes; counterfactual scenarios with different priority systems',
    'If constructed: scarcity is a policy choice amenable to reform (scaffold perspective validated, mountain is false summit). If inherent: some hierarchy is unavoidable (mountain partially validated, extraction flows from resource limits not institutional design).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scarcity_constructed_vs_inherent, empirical, 'Whether resource scarcity is structural or policy-constructed').

omega_variable(
    allocation_transparency_mechanism,
    'Do transparent allocation criteria actually reduce extraction or merely relocate resistance to the criteria-setting process itself?',
    'Comparison of satisfaction and compliance rates pre/post transparency implementation; tracking of lobbying intensity; analysis of appeal frequency',
    'If transparency reduces extraction: theater_ratio should decline (piton not diagnosed). If it relocates resistance: theater_ratio may persist (piton diagnosis confirmed, showing how ritual formality masks underlying power asymmetry).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allocation_transparency_mechanism, empirical, 'Whether transparency reduces extraction or relocates it').

omega_variable(
    coalition_exit_path_viability,
    'Are alternative allocation mechanisms (participatory budgeting, mutual aid networks, resource-sharing pools) actually reaching viability as replacements for hierarchical scarcity-based systems, or are they aspirational supplements?',
    'Longitudinal tracking of adoption rates, resource flows through alternative mechanisms, sustainability metrics; comparison of outcomes (fairness, efficiency, resilience) between systems',
    'If viable: scaffold sunset logic is real, constraint will degrade as alternatives mature. If aspirational: scaffold is optimistic classification, constraint may persist indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_exit_path_viability, empirical, 'Whether alternative allocation mechanisms are becoming viable replacements').

omega_variable(
    powerless_agent_coalition_potential,
    'Can excluded claimants organize into a coalition sufficient to shift the constraint from powerless/trapped to organized/constrained?',
    'Analysis of existing mutual aid networks, collective action successes, coalition threshold estimates, barriers to organizing',
    'If yes: powerless agent classification is temporary; collective action could re-index the constraint and shift it toward tangled_rope or scaffold from snare. If no: powerless/trapped classification is stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(powerless_agent_coalition_potential, empirical, 'Whether excluded claimants can achieve organized coalition power').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_resource_scarcity, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irs_tr_t0, institutional_resource_scarcity, theater_ratio, 0, 0.45).
narrative_ontology:measurement(irs_tr_t3, institutional_resource_scarcity, theater_ratio, 3, 0.58).
narrative_ontology:measurement(irs_tr_t6, institutional_resource_scarcity, theater_ratio, 6, 0.64).

% Extraction over time
narrative_ontology:measurement(irs_be_t0, institutional_resource_scarcity, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(irs_be_t3, institutional_resource_scarcity, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(irs_be_t6, institutional_resource_scarcity, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_resource_scarcity, resource_allocation).
narrative_ontology:affects_constraint(institutional_resource_scarcity, bureaucratic_allocation_capture).
narrative_ontology:affects_constraint(institutional_resource_scarcity, access_inequality_reproduction).

% DUAL FORMULATION NOTE:
% Institutional resource scarcity is upstream of specific sectoral constraints (education funding scarcity, healthcare resource rationing, housing allocation). The upstream constraint structures how scarcity manifests in downstream domains. Separate stories should be written for domain-specific scarcity constraints (e.g., educational_resource_scarcity, healthcare_resource_rationing) with their own ε values reflecting how scarcity plays out in specialized contexts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_resource_scarcity, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
