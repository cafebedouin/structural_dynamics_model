% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__degrowth_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__degrowth_transformation, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: climate_response_legitimacy__degrowth_transformation
 *   human_readable: Degrowth Structural Transformation as Legitimate Climate Response
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint story instantiates the degrowth_transformation reading of
 *   the climate_response_legitimacy kernel. The reading asserts that
 *   legitimate climate action requires wealthy nations to dismantle the
 *   growth imperative through universal basic services, working time
 *   reduction, and democratic firm ownership. The structural delta: current
 *   generations in developed economies bear costs (income reduction,
 *   structural disruption); future generations globally benefit (avoided
 *   warming without technological dependency); political feasibility barriers
 *   create implementation risk. The constraint operates as a tangled rope: it
 *   coordinates a genuine collective-action problem (intergenerational
 *   climate justice within planetary boundaries) while extracting
 *   asymmetrically from the current wealthy-nation generation. Enforcement is
 *   active — the constraint requires policy implementation, institutional
 *   redesign, and sustained political will against entrenched
 *   growth-dependent interests.
 *
 * KEY AGENTS:
 *   - current_generation_wealthy_nations: Primary target (powerful/constrained) — bears extraction via income reduction and structural change
 *   - future_generations_global: Primary beneficiary (powerless/analytical) — benefits from reduced warming without technological dependency
 *   - fossil_intensive_workers: Secondary target (moderate/trapped) — bears concentrated transition costs
 *   - capital_owners_growth_dependent: Secondary target (institutional/constrained) — bears asset stranding and profit reduction
 *   - global_south_populations: Beneficiary (moderate/identity_locked) — benefits from avoided warming and reduced resource extraction
 *   - climate_policy_elites: Agenda setter (institutional/arbitrage) — administers the constraint, sets implementation terms
 *   - degrowth_advocates: Observer (organized/analytical) — analytical seat, sees full structure
 *   - green_growth_technocrats: Observer (institutional/analytical) — analytical seat, sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__degrowth_transformation, 0.62).
domain_priors:suppression_score(climate_response_legitimacy__degrowth_transformation, 0.38).
domain_priors:theater_ratio(climate_response_legitimacy__degrowth_transformation, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, extractiveness, 0.62).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__degrowth_transformation, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__degrowth_transformation, "Degrowth Structural Transformation as Legitimate Climate Response").
narrative_ontology:topic_domain(climate_response_legitimacy__degrowth_transformation, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__degrowth_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__degrowth_transformation, '9c8584b3-cad3-445c-bf6c-6f8e988501fd').
narrative_ontology:cs_kernel_codification('9c8584b3-cad3-445c-bf6c-6f8e988501fd', distributed).
narrative_ontology:cs_authority_grounding('9c8584b3-cad3-445c-bf6c-6f8e988501fd', distributed).
narrative_ontology:cs_reading_relation('9c8584b3-cad3-445c-bf6c-6f8e988501fd', climate_response_legitimacy__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('9c8584b3-cad3-445c-bf6c-6f8e988501fd', climate_response_legitimacy__adaptation_priority, coexists_with).
narrative_ontology:cs_axiom('9c8584b3-cad3-445c-bf6c-6f8e988501fd', foundational, growth_imperative_incompatible_with_planetary_boundaries).
narrative_ontology:cs_axiom_status(growth_imperative_incompatible_with_planetary_boundaries, holdable).
narrative_ontology:cs_axiom_grounding('9c8584b3-cad3-445c-bf6c-6f8e988501fd', growth_imperative_incompatible_with_planetary_boundaries, empirically_contingent).
narrative_ontology:cs_axiom('9c8584b3-cad3-445c-bf6c-6f8e988501fd', foundational, intergenerational_justice_requires_non_discounting).
narrative_ontology:cs_axiom_status(intergenerational_justice_requires_non_discounting, holdable).
narrative_ontology:cs_axiom_grounding('9c8584b3-cad3-445c-bf6c-6f8e988501fd', intergenerational_justice_requires_non_discounting, deontological).
narrative_ontology:cs_reference_frame('9c8584b3-cad3-445c-bf6c-6f8e988501fd', planetary_boundaries_primacy_framework).
narrative_ontology:cs_drift_state('9c8584b3-cad3-445c-bf6c-6f8e988501fd', post_ar6_empirical_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9c8584b3-cad3-445c-bf6c-6f8e988501fd', '2026-08-05T12:00:00Z').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, future_generations_global).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, global_south_populations).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, biodiversity_systems).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, current_generation_wealthy_nations).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, fossil_intensive_workers).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, capital_owners_growth_dependent).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__degrowth_transformation, planetary_boundaries_primacy).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__degrowth_transformation, intergenerational_justice_non_discounting).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__degrowth_transformation, growth_decoupling_impossibility_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Citizens and residents of high-income countries who would experience reduced material throughput, shorter working hours with proportional income reduction, and transition costs as growth-dependent systems restructure. Exit is constrained by national citizenship, sunk investments in growth-dependent skills and assets, and the global reach of the transformation. They bear the extraction directly but retain democratic voice in wealthy nations.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, current_generation_wealthy_nations, payer,
    powerful, biographical, constrained, global).

% Workers in fossil extraction, combustion, and adjacent industries (steel, cement, aviation, internal combustion supply chains) who face job loss, skill obsolescence, and community devastation. Exit is trapped by geographic immobility, sector-specific human capital, age, and the concentrated geographic clustering of these industries. They bear the most concentrated costs with the least exit.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, fossil_intensive_workers, payer,
    moderate, biographical, trapped, regional).

% Owners of capital whose returns depend on growth (equity in growth-dependent firms, financial assets tied to GDP growth, real estate in growth corridors). They bear asset stranding and reduced returns but retain structural power to shape transition terms, capture policy design, and deploy capital mobility as partial exit. Their agenda_setter role reflects disproportionate influence on climate policy design.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, capital_owners_growth_dependent, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__degrowth_transformation, capital_owners_growth_dependent, agenda_setter).

% All future human generations who benefit from avoided catastrophic warming, preserved planetary boundaries, and a post-growth economic system that does not require technological miracles. They have no voice, no exit, and no agency in the current constraint — they are the pure beneficiaries of the coordination function. Their situation is defined by intergenerational asymmetry: they receive the benefits without bearing the transition costs.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, future_generations_global, beneficiary,
    powerless, civilizational, analytical, universal).

% Populations in low- and middle-income countries who benefit from wealthy-nation degrowth through reduced atmospheric colonization, avoided climate damages, and reduced resource extraction pressure. However, they are identity-locked into development-as-growth paradigms by international institutions, debt structures, and elite developmentalist ideologies that frame growth as the only path to dignity. Their benefit is real but their agency to claim it is constrained by the very growth paradigm the constraint challenges.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, global_south_populations, beneficiary,
    moderate, generational, identity_locked, global).

% Non-human life systems that benefit from reduced habitat destruction, pollution, and climate disruption. Included as a vindicated proposition bearer rather than an agent — they collect no rents and have no voice, but the constraint's operation vindicates the proposition that planetary boundaries have primacy over economic growth.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, biodiversity_systems, beneficiary,
    powerless, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(climate_response_legitimacy__degrowth_transformation, biodiversity_systems).

% Intergovernmental negotiators, central bankers, finance ministers, and transnational policy networks who design and implement climate policy. They administer the constraint, set the terms of 'just transition,' and arbitrage between growth-preserving and growth-transforming pathways. They benefit from agenda-setting position (institutional relevance, career capital) while bearing some implementation risk. Their arbitrage-grade exit reflects mobility across institutions and epistemic communities.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, climate_policy_elites, agenda_setter,
    institutional, generational, arbitrage, global).

% Academic, activist, and policy networks advocating for degrowth transformation. They occupy an analytical seat — they see the full structure, articulate the coordination function, and push for implementation. They bear personal/professional costs (marginalization, funding precarity) but do not structurally pay or collect from the constraint's operation.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, degrowth_advocates, observer,
    organized, biographical, analytical, global).

% Mainstream climate policy establishments (IEA, OECD, UNFCCC secretariat, major NGOs) advocating for mitigation_priority reading. They occupy an analytical seat with institutional power — they see the structure but interpret it through the green growth frame. They benefit from the status quo policy architecture and resist the degrowth reading's challenge to their epistemic authority.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, green_growth_technocrats, observer,
    institutional, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the intergenerational collective-action problem: how to allocate the rapidly shrinking carbon budget and planetary boundary space between current wealthy-nation consumption and future global survival. Coordinates a just transition that prevents both climate catastrophe and the unjust burdening of the Global South and future generations.
% TRANSFER_FUNCTION: Moves material throughput, carbon budget, and political-economic power from current wealthy-nation generation (via reduced consumption, working time, asset values, and growth-dependent profits) to future generations and Global South populations (via preserved planetary boundaries, avoided damages, and development space). Also transfers agenda-setting power from growth-dependent capital to democratic institutions managing universal basic services and firm ownership.
% ABSENT_VOICES: Future generations (by definition absent). Indigenous and frontline communities in the Global South whose cosmovisions and territorial struggles prefigure post-growth relations but are excluded from UNFCCC and IPCC framing. Informal economy workers in wealthy nations who already live post-growth realities but are invisible in 'just transition' discourse. Non-human life — the most affected, the least represented.
% DISAPPEARANCE_RATIONALE: If the degrowth transformation constraint vanished overnight, wealthy nations would continue growth-dependent decarbonization (mitigation_priority) or shift to adaptation-only (adaptation_priority). Either path rearranges the world: mitigation_priority locks in continued Global North resource extraction and technological gambling; adaptation_priority locks in sacrifice zones and climate apartheid. The constraint's absence does not return the world to a pre-constraint state — it selects a different contested future.
% FOUNDING_PROBLEM: The founding problem is the triple crisis of the 2010s-2020s: (1) empirical failure of green growth decoupling at required speed/scale, (2) intergenerational injustice of carbon budget allocation, (3) democratic deficit of climate policy captured by growth-dependent capital. The degrowth reading was built to solve all three simultaneously by making the growth imperative itself the leverage point.
% FOUNDING_PROBLEM_CORROBORATION: IPCC AR6 WGIII (2022) Chapter 5 documents the empirical failure of absolute decoupling at required rates. UN Committee on Economic, Social and Cultural Rights (2023) General Comment 26 affirms intergenerational justice as legal obligation. International Energy Agency (2023) Net Zero Roadmap shows continued fossil investment incompatible with 1.5°C. These sources — none of which are degrowth advocacy organizations — corroborate that the founding problem is live and intensifying.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__degrowth_transformation, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__degrowth_transformation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__degrowth_transformation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(climate_response_legitimacy__degrowth_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__degrowth_transformation, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__degrowth_transformation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__degrowth_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__degrowth_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.62) reflects substantial but not total transfer: the current wealthy generation bears significant costs (reduced consumption, working time, asset values) but retains basic services and democratic ownership stakes. Suppression (0.38) is moderate — the constraint requires active policy enforcement (carbon budgets, wealth redistribution, labor reform) but alternatives (green growth, techno-optimism) remain discursively available though structurally contested. Theater ratio (0.28) captures performative 'just transition' rhetoric that masks the depth of required structural change. Accessibility collapse (0.55) is mid-range: alternatives (continued growth with CCS, geoengineering) are not fully collapsed but face mounting empirical disconfirmation. Resistance (0.68) is high — growth-dependent capital, fossil labor, and political establishments actively resist. The measurement series shows extractiveness rising as climate impacts accelerate and the window for gradual transition closes; theater rising as 'green growth' rhetoric increasingly decouples from material decarbonization; suppression rising as policy enforcement must overcome stronger opposition.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (current wealthy generation, fossil workers, capital owners) experience this as enforced extraction with constrained exit — a snare-like pressure. The beneficiary seats (future generations, Global South) experience it as necessary coordination — a rope-like structure. The agenda-setter seat (climate policy elites) experiences it as a contested coordination problem requiring active enforcement — the tangled rope core. The observer seats see the full structural asymmetry. The engine computes per-seat classification from this data; the divergence between payer seats (snare/tangled_rope) and beneficiary seats (rope/mountain) IS the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Current generation in wealthy nations (powerful/constrained) sits near d=0.85 — they bear the extraction directly. Fossil-intensive workers (moderate/trapped) sit near d=0.95 — concentrated costs, minimal exit. Capital owners (institutional/constrained) sit near d=0.75 — they bear costs but retain arbitrage via capital mobility. Future generations (powerless/analytical) sit near d=0.05 — they are the ultimate beneficiaries but have no voice. Global South populations (moderate/identity_locked) sit near d=0.15 — they benefit from avoided warming but are locked into development pathways shaped by wealthy-nation policy. Climate policy elites (institutional/arbitrage) sit near d=0.10 — they administer and benefit from agenda-setting position. Degrowth advocates and green growth technocrats (analytical) sit at d=0.50 — symmetric observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (intergenerational climate justice within planetary boundaries) remains live and arguably intensifying. The constraint has not atrophied into a piton — its coordination function is strengthening as climate impacts materialize. However, mandatrophy risk exists if the constraint is implemented as 'green growth with redistribution' (preserving growth imperative while adopting degrowth aesthetics) — that would be a piton: performative maintenance of a degraded coordination function. The theater ratio rise in measurements tracks this risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_foreclosure_boundary,
    'Does the degrowth_transformation reading''s core premise (growth imperative must be dismantled) logically foreclose the mitigation_priority reading''s core premise (growth can be preserved through decoupling) within a single policy framework, or do they coexist as competing but non-contradictory positions?',
    'Analyze whether any existing or proposed climate policy framework formally incorporates both structural degrowth and green growth decoupling as simultaneous strategies without internal contradiction. If no such framework exists, foreclosure is likely.',
    'If forecloses, the readings cannot be held by the same authority structure; if coexists_with, they represent a genuine policy contest within a shared framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_boundary, conceptual, 'Logical relationship between degrowth_transformation and mitigation_priority readings').

omega_variable(
    political_feasibility_as_extraction_amplifier,
    'Does the high political feasibility barrier (implementation risk) function as an extraction amplifier — increasing effective extraction on the current generation by requiring disproportionate sacrifice for uncertain uptake — or as a coordination barrier that merely delays the constraint''s operation?',
    'Compare historical structural transformations (post-war reconstruction, Marshall Plan, wartime mobilization) where feasibility barriers were overcome: measure the ratio of transitional cost borne by initial cost-bearers to long-term benefit distribution.',
    'If amplifier, effective extraction on current generation is higher than base ε suggests; if barrier, the constraint''s temporal profile shows high initial suppression that decays as coordination succeeds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_feasibility_as_extraction_amplifier, empirical, 'Whether political feasibility barrier amplifies extraction or delays coordination').

omega_variable(
    kernel_naturalness_vs_constructed_legitimacy,
    'Is the ''legitimate climate response'' kernel a genuine natural-law constraint (planetary boundaries impose objective legitimacy conditions) or a constructed normative claim that benefits specific readings by framing their preferred policies as ''legitimate''?',
    'Test whether any reading of the kernel can satisfy all three legitimacy conditions (effectiveness, justice, feasibility) simultaneously. If no reading can, the kernel itself may be an over-constrained construct.',
    'If constructed, the kernel''s authority_grounding shifts toward extraction (authority extracts benefit from preventing revision); if natural-law, authority_grounding shifts toward expertise or lineage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_naturalness_vs_constructed_legitimacy, conceptual, 'Natural-law vs. constructed status of the climate_response_legitimacy kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__degrowth_transformation, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(climate_response_legitimacy__degrowth_transformation_tr_t0, climate_response_legitimacy__degrowth_transformation, theater_ratio, 0, 0.15).
narrative_ontology:measurement(climate_response_legitimacy__degrowth_transformation_tr_t5, climate_response_legitimacy__degrowth_transformation, theater_ratio, 5, 0.18).
narrative_ontology:measurement(climate_response_legitimacy__degrowth_transformation_tr_t10, climate_response_legitimacy__degrowth_transformation, theater_ratio, 10, 0.22).
narrative_ontology:measurement(climate_response_legitimacy__degrowth_transformation_tr_t15, climate_response_legitimacy__degrowth_transformation, theater_ratio, 15, 0.25).
narrative_ontology:measurement(climate_response_legitimacy__degrowth_transformation_tr_t20, climate_response_legitimacy__degrowth_transformation, theater_ratio, 20, 0.27).
narrative_ontology:measurement(climate_response_legitimacy__degrowth_transformation_tr_t25, climate_response_legitimacy__degrowth_transformation, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(climate_response_legitimacy__degrowth_transformation_be_t0, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(climate_response_legitimacy__degrowth_transformation_be_t5, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(climate_response_legitimacy__degrowth_transformation_be_t10, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(climate_response_legitimacy__degrowth_transformation_be_t15, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(climate_response_legitimacy__degrowth_transformation_be_t20, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(climate_response_legitimacy__degrowth_transformation_be_t25, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(climate_response_legitimacy__degrowth_transformation_su_t0, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(climate_response_legitimacy__degrowth_transformation_su_t5, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 5, 0.28).
narrative_ontology:measurement(climate_response_legitimacy__degrowth_transformation_su_t10, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 10, 0.32).
narrative_ontology:measurement(climate_response_legitimacy__degrowth_transformation_su_t15, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 15, 0.35).
narrative_ontology:measurement(climate_response_legitimacy__degrowth_transformation_su_t20, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 20, 0.37).
narrative_ontology:measurement(climate_response_legitimacy__degrowth_transformation_su_t25, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 25, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__degrowth_transformation, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_legitimacy__degrowth_transformation, 0.18).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, fossil_fuel_subsidy_regime).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, global_financial_architecture).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, labor_market_flexibilization).

% DUAL FORMULATION NOTE:
% The climate_response_legitimacy kernel decomposes into three constraint stories (degrowth_transformation, mitigation_priority, adaptation_priority) with distinct ε values and beneficiary/victim structures. They share the referent 'legitimate climate response' but instantiate different constraints. This story links to its siblings via affects_constraints. The degrowth reading influences the mitigation reading by raising the empirical bar for decoupling claims; it coexists with the adaptation reading as both address unavoidable warming but differ on primary response.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_legitimacy__degrowth_transformation, institutional, 0.1).
constraint_indexing:directionality_override(climate_response_legitimacy__degrowth_transformation, powerful, 0.85).
constraint_indexing:directionality_override(climate_response_legitimacy__degrowth_transformation, moderate, 0.65).
constraint_indexing:directionality_override(climate_response_legitimacy__degrowth_transformation, powerless, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
