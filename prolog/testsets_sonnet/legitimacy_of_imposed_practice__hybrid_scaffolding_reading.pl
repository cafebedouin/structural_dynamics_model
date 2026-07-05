% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__hybrid_scaffolding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__hybrid_scaffolding_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legitimacy_of_imposed_practice__hybrid_scaffolding_reading
 *   human_readable: Scaffolded Dress-Code Westernization via Elite Modeling and Ideological Messaging
 *   domain: political/cultural/state_formation
 *
 * SUMMARY:
 *   This story instantiates the hybrid-scaffolding reading of the kernel
 *   'legitimacy_of_imposed_practice,' applied to a dress reform accompanying
 *   a broader modernization program. Unlike the paired calendar reform (pure
 *   decree, unscaffolded, largely failed to displace prior practice — see the
 *   exogenous_override_reading sibling) and unlike a purely organic,
 *   bottom-up cultural shift (the endogenous_climb_reading sibling), the
 *   dress reform combined legal mandate with active scaffolding: elite
 *   modeling, state ceremony, media messaging, and career incentives that
 *   gave the new practice a quasi-endogenous pull among urban populations
 *   while leaving rural populations exposed to the coercive half of the same
 *   policy without its self-reinforcing half. The result was durable but
 *   partial and unevenly distributed displacement — hybrid dress practices
 *   persisting alongside adopted Western markers, split sharply along
 *   urban/rural and elite/non-elite lines.
 *
 * KEY AGENTS:
 *   - state_modernization_bureaucracy: designs and funds the scaffolding, issues the decree
 *   - urban_westernizing_elites: primary beneficiaries, become the modeled exemplars
 *   - rural_populations_excluded_from_scaffolding: bear enforcement cost without incentive infrastructure
 *   - traditional_dress_artisans: lose livelihood as craft demand collapses under both legal and prestige pressure
 *   - provincial_administrators: enforce the decree locally with only coercive tools available
 *   - comparative_policy_historians: analytical seat comparing this outcome to the failed calendar case
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.58).
domain_priors:suppression_score(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.62).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, "Scaffolded Dress-Code Westernization via Elite Modeling and Ideological Messaging").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, "political/cultural/state_formation").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, '32efd507-d587-47a5-8001-fc3c2769d928').
narrative_ontology:cs_kernel_codification('32efd507-d587-47a5-8001-fc3c2769d928', distributed).
narrative_ontology:cs_authority_grounding('32efd507-d587-47a5-8001-fc3c2769d928', extraction).
narrative_ontology:cs_interpretation_layer_present('32efd507-d587-47a5-8001-fc3c2769d928').
narrative_ontology:cs_reading_relation('32efd507-d587-47a5-8001-fc3c2769d928', legitimacy_of_imposed_practice__exogenous_override_reading, influences).
narrative_ontology:cs_reading_relation('32efd507-d587-47a5-8001-fc3c2769d928', legitimacy_of_imposed_practice__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_axiom('32efd507-d587-47a5-8001-fc3c2769d928', foundational, ideological_scaffolding_necessary_for_durable_displacement).
narrative_ontology:cs_axiom_status(ideological_scaffolding_necessary_for_durable_displacement, holdable).
narrative_ontology:cs_axiom_grounding('32efd507-d587-47a5-8001-fc3c2769d928', ideological_scaffolding_necessary_for_durable_displacement, empirically_contingent).
narrative_ontology:cs_axiom('32efd507-d587-47a5-8001-fc3c2769d928', secondary, partial_uneven_displacement_is_the_expected_success_condition).
narrative_ontology:cs_axiom_status(partial_uneven_displacement_is_the_expected_success_condition, holdable).
narrative_ontology:cs_axiom_grounding('32efd507-d587-47a5-8001-fc3c2769d928', partial_uneven_displacement_is_the_expected_success_condition, instrumental).
narrative_ontology:cs_reference_frame('32efd507-d587-47a5-8001-fc3c2769d928', modernization_through_visible_identity_reform).
narrative_ontology:cs_drift_state('32efd507-d587-47a5-8001-fc3c2769d928', post_reform_consolidation_generation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('32efd507-d587-47a5-8001-fc3c2769d928', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_westernizing_elites).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, state_modernization_bureaucracy).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, rural_populations_excluded_from_scaffolding).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, traditional_dress_artisans).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, provincial_administrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and administers the dress reform: issues decrees, but also funds schools, ceremonies, media campaigns, and civil-service dress codes that model the new practice and frame it as a marker of national modernity and progress. Unlike the pure-decree calendar reform, this apparatus builds scaffolding infrastructure — visible elite exemplars, ideological narrative linking dress to civilizational advancement — that generates uptake beyond what decree alone produces.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, state_modernization_bureaucracy, agenda_setter,
    institutional, generational, analytical, national).

% Adopt Western dress markers early and visibly, converting compliance into social and career capital: access to state employment, foreign contact, and status signaling. They receive the scaffolding infrastructure (schools, media, exemplar roles) built by the state and become the models the messaging apparatus points to. Their adoption is genuinely partly internalized — not pure compliance — because the ideological framing gives them a felt stake in the new identity.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_westernizing_elites, beneficiary,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_westernizing_elites, agenda_setter).

% Live outside the reach of the schools, media, and ceremonial infrastructure that make the dress reform partly self-sustaining in cities. They face the same legal exposure to enforcement (fines, exclusion from state services, public shaming) without receiving any of the status incentives or ideological narrative that makes urban elites want to comply. For them the constraint operates closer to pure decree — imposed cost without the quasi-endogenous pull — while still being nominally the same national policy.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, rural_populations_excluded_from_scaffolding, payer,
    powerless, generational, trapped, regional).

% Lose livelihood and craft status as demand for traditional garments collapses under both legal pressure and the elite-modeled prestige economy favoring Western dress. They cannot access the scaffolding narrative that would let them reposition their trade (no state messaging valorizes their goods) and are treated as remnants of a superseded order rather than as a constituency to be transitioned.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, traditional_dress_artisans, payer,
    moderate, biographical, constrained, local).

% Tasked with enforcing the same dress code in areas lacking the ideological infrastructure that makes compliance self-reinforcing in the capital. They must rely on coercive tools (fines, harassment, denial of services) precisely because the softer mechanisms available to central elites are absent locally, making their local enforcement look more like the failed pure-decree pattern even though it is nominally the same national policy.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, provincial_administrators, agenda_setter,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, provincial_administrators, payer).

% Study why the calendar reform (pure decree, unscaffolded) failed to displace prior practice while the dress reform (decree plus elite modeling and ideological messaging) achieved durable partial displacement. They read across cases to isolate scaffolding infrastructure as the causal variable distinguishing failed imposition from successful hybrid imposition.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, comparative_policy_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_westernizing_elites).
narrative_ontology:fixing_cost_class(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, legible marker of national modernization that domestic elites and foreign observers can read at a glance, and creates a pathway (schools, ceremony, media, career incentives) by which a genuinely new practice can become partly self-sustaining rather than requiring permanent coercion.
% TRANSFER_FUNCTION: Moves status, state employment access, and reputational capital toward those who adopt the new dress code early and visibly (urban elites), while moving economic loss (collapsed craft demand) and enforcement burden (fines, exclusion, local coercion without local incentive infrastructure) onto rural populations and traditional artisans who are not reached by the same scaffolding.
% ABSENT_VOICES: Rural populations and traditional artisans have no representation in the design of the ideological messaging or the elite-modeling apparatus; their objections would be that the same law reaches them without any of the incentive structure that makes it tolerable or self-reinforcing for urban elites, but they are structurally outside the rooms (ministries, elite social circles, media production) where the scaffolding is designed.
% DISAPPEARANCE_RATIONALE: Urban elites and the state bureaucracy would experience disappearance of the mandate as a genuine loss — the dress practice has partly internalized into felt identity and career signaling, so its removal would cause real disruption to an existing social order. Rural populations and artisans would experience disappearance as simple relief from an externally imposed cost with no compensating internal stake — for them the world barely rearranges because the practice never took root beyond compliance under threat.
% FOUNDING_PROBLEM: The state sought a legible, internationally recognizable marker of civilizational modernization to accompany broader institutional reforms, believing that visible identity markers would accelerate elite buy-in to a wider modernization project and improve standing with foreign powers.
% FOUNDING_PROBLEM_CORROBORATION: State ministries and urban elite memoirs attest the reform succeeded in cultivating a genuine modern identity and remains functionally live as a marker of national progress. Independent historians and testimony collected from rural communities and artisan guilds attest the founding problem was substantially about elite status signaling and foreign legitimacy rather than a felt civilizational need, and that the persistence of hybrid dress practices reflects unequal scaffolding access rather than uniform internalization.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, contested).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__hybrid_scaffolding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__hybrid_scaffolding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) and suppression (0.62) sit meaningfully lower than a pure-decree snare would show, because the scaffolding genuinely reduces the coercive load needed to sustain compliance among the population it reaches — this is the structural signature distinguishing hybrid scaffolding from pure override. But both metrics remain substantial because a large population (rural, artisan) is governed by the coercive half of the policy without access to the incentive half, producing real, asymmetric extraction. Theater ratio (0.4) reflects that a meaningful share of the ideological messaging is genuine identity-formation activity (not pure performance) rather than assigning it fully to either coordination or theater.
 *
 * DIRECTIONALITY LOGIC:
 *   Urban elites derive low d (beneficiary end): they receive scaffolding infrastructure, convert compliance into capital, and partly internalize the new identity, which the derivation chain reads as subsidized rather than extracted from. Rural populations and artisans derive high d (target end): they face the same legal exposure with none of the compensating incentive structure, arbitrage-free, trapped or constrained exit. Provincial administrators sit ambiguously — nominally agenda-setters locally, but functionally payers of the enforcement burden the central scaffolding does not extend to cover, which is why they are given a secondary payer role rather than pure agenda_setter.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a legible marker of civilizational modernization to accompany institutional reform) is contested rather than cleanly dead or live: for urban elites who internalized the identity, the problem the mandate solved persists as felt identity, not merely as inertia. For rural populations, the arrangement persists as compliance burden long after any credible claim that they, too, needed this identity marker — which is closer to zombie/mandatrophy dynamics for that population specifically, even though the constraint as a whole is not simply captured or simply obsolete. This is exactly the mislabeling risk a hybrid-scaffolding reading is built to catch: neither pure snare (there is real, partly internalized coordination for elites) nor pure rope (there is asymmetric, coercively backed extraction for rural populations) captures the full structure alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internalization_depth_ambiguity,
    'How much of the urban elite adoption of Western dress reflects genuine internalized identity versus sustained performance for career and status access that would collapse without continued incentive infrastructure?',
    'Longitudinal tracking of dress practice after removal of state incentives (career penalties, ceremonial requirements) in comparable post-reform states; oral history distinguishing self-reported identity change from reported instrumental compliance.',
    'If adoption is mostly performative even among elites, the constraint''s coordination function is much thinner than the hybrid-scaffolding reading claims, moving it toward tangled_rope-with-thin-coordination or even snare; if genuinely internalized, the reading''s claim of quasi-endogenous pull is validated for that population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_depth_ambiguity, empirical, 'Whether urban elite dress adoption is internalized identity or sustained-incentive performance.').

omega_variable(
    scaffolding_infrastructure_intentional_exclusion,
    'Was the failure to extend scaffolding infrastructure to rural populations a resource constraint, a deliberate strategy to concentrate modernization benefits among urban elites, or an unexamined byproduct of where state capacity happened to be strongest?',
    'Archival review of budget allocation decisions and internal state correspondence discussing rural extension of the reform''s ideological and ceremonial apparatus.',
    'Deliberate exclusion would sharpen the tangled_rope classification toward a more clearly extractive reading with rural populations as a targeted victim class; resource constraint would support a more genuinely mixed coordination/extraction reading where the asymmetry is emergent rather than designed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scaffolding_infrastructure_intentional_exclusion, empirical, 'Whether rural exclusion from scaffolding was strategic, resource-driven, or incidental.').

omega_variable(
    reading_framing_under_determination,
    'Is the calendar/dress distinction genuinely structural (scaffolding present vs. absent) or partly an artifact of which reform historians chose to study in depth, given that dress reform left more visible material and photographic record than the calendar reform?',
    'Comparative archival depth check: assess whether documentation asymmetry between the two reforms could itself explain the perceived success/failure gap, independent of actual scaffolding differences.',
    'If the calendar reform actually had comparable scaffolding that is simply less documented, the sharp contrast motivating this kernel decomposition weakens and the two readings converge; if the asymmetry is genuinely structural, the decomposition and this reading''s classification stand.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_framing_under_determination, conceptual, 'Whether the calendar/dress contrast reflects structural difference or documentation bias.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(legi_tr_t8, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(legi_tr_t16, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement(legi_tr_t24, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(legi_tr_t32, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(legi_be_t8, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(legi_be_t16, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(legi_be_t24, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 24, 0.54).
narrative_ontology:measurement(legi_be_t32, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 32, 0.57).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(legi_su_t8, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 8, 0.7).
narrative_ontology:measurement(legi_su_t16, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 16, 0.64).
narrative_ontology:measurement(legi_su_t24, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(legi_su_t32, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 32, 0.62).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 40, 0.62).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(legi_grid_01, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, accessibility_collapse(class), 0, 0.4).
narrative_ontology:measurement(legi_grid_02, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, accessibility_collapse(class), 40, 0.6).
narrative_ontology:measurement(legi_grid_03, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, accessibility_collapse(individual), 0, 0.2).
narrative_ontology:measurement(legi_grid_04, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, accessibility_collapse(individual), 40, 0.35).
narrative_ontology:measurement(legi_grid_05, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, accessibility_collapse(organizational), 0, 0.25).
narrative_ontology:measurement(legi_grid_06, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, accessibility_collapse(organizational), 40, 0.5).
narrative_ontology:measurement(legi_grid_07, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, accessibility_collapse(structural), 0, 0.3).
narrative_ontology:measurement(legi_grid_08, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, accessibility_collapse(structural), 40, 0.55).
narrative_ontology:measurement(legi_grid_09, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, resistance(class), 0, 0.6).
narrative_ontology:measurement(legi_grid_10, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, resistance(class), 40, 0.55).
narrative_ontology:measurement(legi_grid_11, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, resistance(individual), 0, 0.15).
narrative_ontology:measurement(legi_grid_12, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, resistance(individual), 40, 0.1).
narrative_ontology:measurement(legi_grid_13, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, resistance(organizational), 0, 0.25).
narrative_ontology:measurement(legi_grid_14, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, resistance(organizational), 40, 0.2).
narrative_ontology:measurement(legi_grid_15, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, resistance(structural), 0, 0.3).
narrative_ontology:measurement(legi_grid_16, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, resistance(structural), 40, 0.2).
narrative_ontology:measurement(legi_grid_17, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, stakes_inflation(class), 0, 0.5).
narrative_ontology:measurement(legi_grid_18, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, stakes_inflation(class), 40, 0.65).
narrative_ontology:measurement(legi_grid_19, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, stakes_inflation(individual), 0, 0.2).
narrative_ontology:measurement(legi_grid_20, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, stakes_inflation(individual), 40, 0.3).
narrative_ontology:measurement(legi_grid_21, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, stakes_inflation(organizational), 0, 0.35).
narrative_ontology:measurement(legi_grid_22, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, stakes_inflation(organizational), 40, 0.45).
narrative_ontology:measurement(legi_grid_23, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, stakes_inflation(structural), 0, 0.3).
narrative_ontology:measurement(legi_grid_24, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, stakes_inflation(structural), 40, 0.5).
narrative_ontology:measurement(legi_grid_25, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression(class), 0, 0.8).
narrative_ontology:measurement(legi_grid_26, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression(class), 40, 0.7).
narrative_ontology:measurement(legi_grid_27, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression(individual), 0, 0.55).
narrative_ontology:measurement(legi_grid_28, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression(individual), 40, 0.35).
narrative_ontology:measurement(legi_grid_29, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression(organizational), 0, 0.65).
narrative_ontology:measurement(legi_grid_30, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression(organizational), 40, 0.45).
narrative_ontology:measurement(legi_grid_31, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression(structural), 0, 0.7).
narrative_ontology:measurement(legi_grid_32, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression(structural), 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.1).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice__endogenous_climb_reading).

% DUAL FORMULATION NOTE:
% Part of a three-member kernel family under legitimacy_of_imposed_practice. exogenous_override_reading covers the calendar reform (pure decree, no scaffolding, largely failed displacement) as a separate constraint with lower coordination function and a cleaner snare/failed-imposition profile. endogenous_climb_reading names the pure bottom-up counterfactual pathway. This story (hybrid_scaffolding_reading) claims that adding ideological scaffolding to decree produces a qualitatively distinct, partially successful, but structurally uneven displacement dynamic — modeled here as tangled_rope rather than snare or rope, reflecting genuine partial coordination for the scaffolded population alongside asymmetric extraction from the unscaffolded population.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
