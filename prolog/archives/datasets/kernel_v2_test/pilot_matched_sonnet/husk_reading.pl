% ============================================================================
% CONSTRAINT STORY: husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_husk_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: husk_reading
 *   human_readable: Preparedness as Memorial Performance (Husk Reading)
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   The husk reading of preparedness persistence interprets disaster
 *   preparedness regimes as degraded institutional forms where operational
 *   capacity has atrophied while memorial performance persists. Drills occur
 *   on schedule, inspections are documented, compliance metrics are met — but
 *   the actual capacity to respond effectively to disasters has eroded
 *   through resource starvation, knowledge loss, and institutional drift. The
 *   preparedness apparatus functions primarily as legitimacy theater: it
 *   reassures the public and satisfies bureaucratic accountability
 *   requirements while providing minimal operational value. This reading sees
 *   the constraint as a piton — a former coordination mechanism (preparedness
 *   drills once built real capacity) that has degraded into performance
 *   maintained through institutional inertia. The theater_ratio (0.85)
 *   reflects that most preparedness activity is now performative rather than
 *   functional. The extraction (0.35) is moderate rather than severe because
 *   the primary flow is not rent extraction but legitimacy maintenance —
 *   resources go to sustaining the performance rather than to concentrated
 *   beneficiaries. The suppression (0.40) reflects that populations in flood
 *   zones cannot exit and operational responders are institutionally required
 *   to participate in the theater.
 *
 * KEY AGENTS:
 *   - Population at Flood Risk: Primary victim (powerless/trapped) — bears the cost of degraded operational capacity while the preparedness theater provides false reassurance
 *   - Operational Responders: Secondary victim (moderate/constrained) — required to perform drills that no longer build real capacity; see the degradation directly but cannot exit
 *   - Compliance Bureaucracy: Primary beneficiary (institutional/arbitrage) — benefits from legible preparedness metrics regardless of operational effectiveness
 *   - Institutional Legitimacy Apparatus: Mixed beneficiary (institutional/constrained) — benefits from public reassurance but also constrained by need to sustain the performance
 *   - Analytical Observer: Generational view (analytical/analytical) — sees the preparedness regime as a degraded institutional form, a piton maintained through inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(husk_reading, 0.35).
domain_priors:suppression_score(husk_reading, 0.4).
domain_priors:theater_ratio(husk_reading, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(husk_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(husk_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(husk_reading, theater_ratio, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(husk_reading, piton).
narrative_ontology:human_readable(husk_reading, "Preparedness as Memorial Performance (Husk Reading)").
narrative_ontology:topic_domain(husk_reading, "disaster_preparedness/institutional_memory/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(husk_reading, '6c9d8eaa-fa63-46a5-9051-0fa172d754cb').
narrative_ontology:cs_kernel_codification('6c9d8eaa-fa63-46a5-9051-0fa172d754cb', formalized).
narrative_ontology:cs_authority_grounding('6c9d8eaa-fa63-46a5-9051-0fa172d754cb', extraction).
narrative_ontology:cs_interpretation_layer_present('6c9d8eaa-fa63-46a5-9051-0fa172d754cb').
narrative_ontology:cs_reading_relation('6c9d8eaa-fa63-46a5-9051-0fa172d754cb', husk_reading__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('6c9d8eaa-fa63-46a5-9051-0fa172d754cb', husk_reading__hybrid_reading, influences).
narrative_ontology:cs_axiom('6c9d8eaa-fa63-46a5-9051-0fa172d754cb', foundational, theater_sufficiency_for_legitimacy).
narrative_ontology:cs_axiom_status(theater_sufficiency_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('6c9d8eaa-fa63-46a5-9051-0fa172d754cb', theater_sufficiency_for_legitimacy, conventional).
narrative_ontology:cs_axiom('6c9d8eaa-fa63-46a5-9051-0fa172d754cb', secondary, capacity_atrophy_inevitable).
narrative_ontology:cs_axiom_status(capacity_atrophy_inevitable, holdable).
narrative_ontology:cs_axiom_grounding('6c9d8eaa-fa63-46a5-9051-0fa172d754cb', capacity_atrophy_inevitable, empirically_contingent).
narrative_ontology:cs_reference_frame('6c9d8eaa-fa63-46a5-9051-0fa172d754cb', operational_capacity_maintenance).
narrative_ontology:cs_drift_state('6c9d8eaa-fa63-46a5-9051-0fa172d754cb', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6c9d8eaa-fa63-46a5-9051-0fa172d754cb', '').
narrative_ontology:cs_kernel_id(husk_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(husk_reading, institutional_legitimacy_apparatus).
narrative_ontology:constraint_beneficiary(husk_reading, compliance_bureaucracy).
narrative_ontology:constraint_victim(husk_reading, population_at_flood_risk).
narrative_ontology:constraint_victim(husk_reading, operational_responders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(husk_reading, institutional_legitimacy_apparatus).
narrative_ontology:constraint_vindicates(husk_reading, preparedness_as_ritual_sufficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Residents in flood-prone zones who cannot relocate. They bear the cost of degraded operational capacity while preparedness theater provides false reassurance. When disasters strike, the drills that were performed do not translate to effective response. They are trapped geographically and informationally — the theater prevents them from seeing the true state of preparedness.
narrative_ontology:constraint_stakeholder(husk_reading, population_at_flood_risk, payer,
    powerless, immediate, trapped, local).

% Emergency response personnel required to perform drills that no longer build real capacity. They see the degradation directly: equipment is outdated, protocols are rote, institutional knowledge has been lost. They bear the cost of participating in theater while their actual operational capacity atrophies. Exit is possible (leave the profession) but costly (career change, loss of specialized training investment).
narrative_ontology:constraint_stakeholder(husk_reading, operational_responders, payer,
    moderate, biographical, constrained, regional).

% Administrative apparatus that oversees preparedness compliance. Benefits from legible metrics (drills conducted, inspections completed, documentation filed) regardless of whether those metrics correspond to operational effectiveness. The theater serves their coordination needs: it provides quantifiable accountability measures and satisfies institutional reporting requirements. They can exit easily (the metrics are portable across jurisdictions).
narrative_ontology:constraint_stakeholder(husk_reading, compliance_bureaucracy, beneficiary,
    institutional, immediate, arbitrage, national).

% The broader institutional structure that depends on preparedness theater for public legitimacy. Benefits from public reassurance (the drills signal that authorities are prepared) but also bears the cost of sustaining the performance (resources flow to theater rather than capacity). Dual-positioned: collects legitimacy benefits while paying maintenance costs. Exit is constrained (abandoning preparedness theater would undermine institutional authority).
narrative_ontology:constraint_stakeholder(husk_reading, institutional_legitimacy_apparatus, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(husk_reading, institutional_legitimacy_apparatus, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The preparedness regime originally coordinated disaster response capacity: drills built operational competence, inspections maintained equipment, protocols transmitted institutional knowledge. In the husk reading, this coordination function has atrophied.
% TRANSFER_FUNCTION: Resources (budget, personnel time, institutional attention) flow from operational capacity building to memorial performance. Money goes to conducting drills that satisfy compliance requirements rather than to maintaining equipment or training personnel in adaptive response. Attention flows to documentation and metrics rather than to actual disaster scenarios.
% ABSENT_VOICES: Future disaster victims are absent from the preparedness regime's design. The population at risk participates in drills but has no voice in whether those drills build real capacity or merely satisfy bureaucratic requirements. Operational responders see the degradation but lack institutional power to redirect resources from theater to capacity. The absent voices would object that preparedness theater provides false reassurance while operational capacity erodes.
% DISAPPEARANCE_RATIONALE: If the preparedness regime disappeared overnight, institutional legitimacy structures would need to find alternative reassurance mechanisms, compliance bureaucracies would lose their metrics, and the population at risk would face disaster without even the theater of preparedness. The world rearranges because the regime serves real institutional functions (legitimacy maintenance, bureaucratic coordination) even though its operational function has atrophied. The theater is load-bearing for institutional stability.
% FOUNDING_PROBLEM: The preparedness regime was built to solve the operational problem of disaster response capacity: how to maintain trained personnel, functional equipment, and effective protocols across long periods without disasters. The founding problem was real — disasters are infrequent but catastrophic, and capacity atrophies without maintenance.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (maintaining disaster response capacity across time) remains live, corroborated by actual disaster outcomes when preparedness regimes fail. Hurricane Katrina (2005), Fukushima (2011), and COVID-19 (2020) all revealed degraded operational capacity despite extensive preparedness theater. The problem is live; what has changed is that the preparedness regime no longer solves it effectively. Corroboration comes from disaster researchers, operational responders, and post-disaster investigations — sources outside the institutional legitimacy apparatus that benefits from the theater.
narrative_ontology:disappearance_verdict(husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(husk_reading, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POPULATION AT FLOOD RISK (PITON) — Trapped in flood zones with no exit options, experiences the preparedness apparatus as pure theater. Drills occur but operational capacity has atrophied. The performance persists while the function has degraded. High theater ratio dominates the classification despite moderate extraction.
constraint_indexing:constraint_classification(husk_reading, piton,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: OPERATIONAL RESPONDER (PITON) — Constrained by institutional requirements to perform drills that no longer build real capacity. Sees the degradation directly: equipment is outdated, protocols are rote, institutional knowledge has been lost. The ritual persists but the function is gone. Theater ratio drives classification.
constraint_indexing:constraint_classification(husk_reading, piton,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: COMPLIANCE BUREAUCRACY (ROPE) — Benefits from the preparedness apparatus as a coordination mechanism. Drills and inspections provide legible metrics for institutional accountability. Experiences low extraction because the apparatus serves their coordination needs regardless of operational effectiveness. The theater is functional for this agent.
constraint_indexing:constraint_classification(husk_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL LEGITIMACY APPARATUS (TANGLED ROPE) — Benefits from preparedness theater as legitimacy maintenance but also constrained by the need to sustain the performance. Coordination function exists (public reassurance, institutional continuity) alongside extraction (resources flow to performance rather than capacity). Mixed experience.
constraint_indexing:constraint_classification(husk_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (PITON) — From a generational view, the preparedness apparatus is a degraded institutional form. The original function (operational disaster response capacity) has atrophied, but the constraint persists through institutional inertia and legitimacy requirements. Theater ratio is the dominant structural feature. This is the husk reading's analytical claim.
constraint_indexing:constraint_classification(husk_reading, piton,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(husk_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(husk_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(husk_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(husk_reading, TR),
    TR >= 0.70.

:- end_tests(husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. Resources flow to sustaining preparedness theater rather than building operational capacity, but the extraction is not concentrated in identifiable rent-seeking beneficiaries. The primary beneficiary is institutional legitimacy itself — an abstract good rather than a specific actor. The extraction is real (resources are diverted from operational capacity to performance) but diffuse. Suppression (0.40): Moderate. Populations in flood zones cannot exit their geographic risk, and operational responders are institutionally required to participate in drills. But suppression is not total — responders can leave the profession, and some jurisdictions maintain better capacity than others. Theater ratio (0.85): Very high. This is the dominant structural feature. Most preparedness activity is performative: drills follow rote scripts, inspections check compliance boxes, documentation satisfies bureaucratic requirements — but operational capacity has atrophied. Equipment is outdated, institutional knowledge has been lost, protocols are no longer adapted to actual disaster scenarios. The performance persists because it serves legitimacy functions, not because it builds capacity.
 *
 * PERSPECTIVAL GAP:
 *   The husk reading produces a characteristic perspectival gap: trapped and constrained agents (population at risk, operational responders) see piton — degraded theater with minimal function. The compliance bureaucracy sees rope — the preparedness apparatus coordinates their accountability needs effectively. The institutional legitimacy apparatus sees tangled_rope — mixed coordination (public reassurance) and extraction (resources to performance rather than capacity). The analytical observer sees piton from a generational view — the constraint is a degraded institutional form. The gap reveals that preparedness theater serves different functions for different agents: legitimacy maintenance for institutions, false reassurance for populations, bureaucratic coordination for compliance apparatus, and degraded ritual for operational responders.
 *
 * DIRECTIONALITY LOGIC:
 *   The population at flood risk is the primary victim — they bear the cost of degraded operational capacity while receiving false reassurance from preparedness theater. High d (victim status + trapped exit) produces high experienced extraction despite moderate base extractiveness. Operational responders are secondary victims — constrained by institutional requirements to perform degraded drills. Moderate d (victim status + constrained exit). The compliance bureaucracy is a primary beneficiary — preparedness metrics serve their coordination needs regardless of operational effectiveness. Low d (beneficiary status + arbitrage exit) produces low or negative experienced extraction. The institutional legitimacy apparatus is a mixed case — benefits from public reassurance but also bears costs of sustaining the performance. Moderate d reflecting mixed position.
 *
 * MANDATROPHY ANALYSIS:
 *   The husk reading resolves mandatrophy by identifying preparedness persistence as a piton — a constraint whose original function (building operational disaster response capacity) has atrophied while the form persists through institutional inertia and legitimacy requirements. The mandate (disaster preparedness) has not been fulfilled; the constraint has degraded into memorial performance. This is not a false mountain (the constraint is not naturalized as inevitable) and not a snare (the extraction is diffuse rather than concentrated). It is a degraded coordination mechanism maintained as theater. The mandatrophy is resolved by recognizing that the constraint's current function (legitimacy maintenance) is different from its original mandate (operational capacity), and the theater_ratio measures this gap.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is preparedness persistence a degraded ritual (husk reading), a maintained competence (competence reading), or a mixed state with pockets of both (hybrid reading)?',
    'Longitudinal operational capacity measurement: response time degradation, equipment maintenance records, personnel training retention, actual disaster response outcomes compared to drill performance. Cross-jurisdictional comparison of preparedness regimes with different institutional structures.',
    'If husk: preparedness is piton from most perspectives, extraction flows to legitimacy theater. If competence: preparedness is rope or scaffold, coordination function is real. If hybrid: classification varies by jurisdiction and disaster type, requiring decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, empirical, 'Which reading of the preparedness persistence kernel is structurally accurate').

omega_variable(
    atrophy_mechanism,
    'What drives operational capacity atrophy while memorial performance persists? Is it resource starvation, knowledge loss, institutional capture, or normalization of deviance?',
    'Historical analysis of preparedness regime degradation: budget allocation over time, personnel turnover and training gaps, equipment replacement cycles, institutional priority shifts. Identification of critical junctures where performance decoupled from capacity.',
    'Mechanism determines intervention points. If resource starvation: funding restores capacity. If knowledge loss: training and documentation interventions work. If institutional capture: structural reform required. If normalization: the theater itself is the problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrophy_mechanism, empirical, 'Mechanism by which operational capacity atrophies while performance persists').

omega_variable(
    legitimacy_dependency,
    'Does the institutional legitimacy apparatus depend on preparedness theater, or could it survive transparent acknowledgment of degraded capacity?',
    'Political economy analysis: what happens to institutional authority when preparedness failures are exposed? Historical cases of disaster response failure and institutional consequences. Public trust dynamics around acknowledged vs concealed incapacity.',
    'If dependent: the theater is load-bearing for institutional stability, making reform structurally difficult. If independent: transparent capacity assessment becomes possible without institutional collapse.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_dependency, preference, 'Whether institutional legitimacy structurally depends on preparedness theater').

omega_variable(
    cs_framing_underdetermination,
    'Is the kernel the preparedness regime itself (institutional rules and drills), or the legitimacy claim layered above it (the assertion that drills equal readiness)?',
    'Structural analysis: does changing the drill protocol change the classification, or does changing the legitimacy narrative change it? If the former, kernel is the regime. If the latter, kernel is the claim.',
    'If kernel is regime: cs_structure.kernel_codification should be ''formalized'' (the drill protocols are the kernel). If kernel is claim: should be ''implicit'' (the kernel is whatever the legitimacy apparatus asserts). Current framing treats regime as kernel; alternative framing would treat legitimacy claim as kernel and produce different cs_pattern classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Whether the kernel is the preparedness regime or the legitimacy claim above it').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(husk_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(husk_tr_t0, husk_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(husk_tr_t3, husk_reading, theater_ratio, 3, 0.65).
narrative_ontology:measurement(husk_tr_t6, husk_reading, theater_ratio, 6, 0.75).
narrative_ontology:measurement(husk_tr_t9, husk_reading, theater_ratio, 9, 0.82).
narrative_ontology:measurement(husk_tr_t12, husk_reading, theater_ratio, 12, 0.85).

% Extraction over time
narrative_ontology:measurement(husk_be_t0, husk_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(husk_be_t3, husk_reading, base_extractiveness, 3, 0.25).
narrative_ontology:measurement(husk_be_t6, husk_reading, base_extractiveness, 6, 0.3).
narrative_ontology:measurement(husk_be_t9, husk_reading, base_extractiveness, 9, 0.33).
narrative_ontology:measurement(husk_be_t12, husk_reading, base_extractiveness, 12, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(husk_su_t0, husk_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(husk_su_t6, husk_reading, suppression_requirement, 6, 0.35).
narrative_ontology:measurement(husk_su_t12, husk_reading, suppression_requirement, 12, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(husk_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% The husk reading is one of three readings of the preparedness_persistence kernel. It should be linked to competence_reading and hybrid_reading when those constraints are authored. The three readings share the same observable (preparedness regime persistence) but interpret it with different theater_ratio and extractiveness values, reflecting different structural hypotheses about whether operational capacity has been maintained or has atrophied.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
