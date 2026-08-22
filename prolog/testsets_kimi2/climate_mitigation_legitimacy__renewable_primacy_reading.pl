% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__renewable_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__renewable_primacy_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_mitigation_legitimacy__renewable_primacy_reading
 *   human_readable: Renewable Primacy Decarbonization Constraint
 *   domain: energy_policy/climate_mitigation
 *
 * SUMMARY:
 *   This constraint story models the renewable_primacy_reading of the
 *   climate_mitigation_legitimacy kernel: the institutionalized claim that
 *   renewable energy combined with storage can fully decarbonize electricity
 *   faster and cheaper than nuclear power. As enacted in green taxonomies,
 *   subsidy regimes, and integrated assessment models, this reading
 *   coordinates massive capital flows toward variable renewables and battery
 *   storage while structurally excluding nuclear baseload from the
 *   decarbonization portfolio. The constraint exhibits genuine coordination
 *   function (solving the collective-action problem of which zero-carbon
 *   technology to scale) alongside asymmetric extraction (diverting policy
 *   support and capital from nuclear operators, who become stranded in a
 *   framework that treats their output as unnecessary). Nuclear industry
 *   stakeholders enter the victim set not because renewables are extractive
 *   by nature, but because the specific institutionalization of this reading
 *   suppresses nuclear as a capital sink delaying the transition.
 *
 * KEY AGENTS:
 *   - renewable_developers: Primary beneficiary (organized/mobile) â receive policy preference and subsidy flows
 *   - storage_manufacturers: Primary beneficiary (organized/mobile) â positioned as essential infrastructure for renewable integration
 *   - nuclear_industry: Primary target (powerful/constrained) â loses capital access and policy standing
 *   - baseload_operators: Secondary target (institutional/constrained) â grid operators with stranded operational expertise
 *   - climate_policy_institutions: Agenda setter (institutional/analytical) â produces modeling that legitimizes the pathway
 *   - grid_reliability_engineers: Excluded voice (moderate/constrained) â operational warnings marginalized by capacity-planning models
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__renewable_primacy_reading, 0.65).
domain_priors:suppression_score(climate_mitigation_legitimacy__renewable_primacy_reading, 0.6).
domain_priors:theater_ratio(climate_mitigation_legitimacy__renewable_primacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__renewable_primacy_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__renewable_primacy_reading, "Renewable Primacy Decarbonization Constraint").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__renewable_primacy_reading, "energy_policy/climate_mitigation").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__renewable_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__renewable_primacy_reading, '5c310890-60ea-4021-ab1d-321ad17d070f').
narrative_ontology:cs_kernel_codification('5c310890-60ea-4021-ab1d-321ad17d070f', formalized).
narrative_ontology:cs_authority_grounding('5c310890-60ea-4021-ab1d-321ad17d070f', expertise).
narrative_ontology:cs_interpretation_layer_present('5c310890-60ea-4021-ab1d-321ad17d070f').
narrative_ontology:cs_reading_relation('5c310890-60ea-4021-ab1d-321ad17d070f', climate_mitigation_legitimacy__baseload_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('5c310890-60ea-4021-ab1d-321ad17d070f', climate_mitigation_legitimacy__portfolio_pragmatism_reading, influences).
narrative_ontology:cs_reading_relation('5c310890-60ea-4021-ab1d-321ad17d070f', climate_mitigation_legitimacy__degrowth_sufficiency_reading, coexists_with).
narrative_ontology:cs_axiom('5c310890-60ea-4021-ab1d-321ad17d070f', foundational, renewable_sufficiency_for_decarbonization).
narrative_ontology:cs_axiom_status(renewable_sufficiency_for_decarbonization, holdable).
narrative_ontology:cs_axiom_grounding('5c310890-60ea-4021-ab1d-321ad17d070f', renewable_sufficiency_for_decarbonization, empirically_contingent).
narrative_ontology:cs_axiom('5c310890-60ea-4021-ab1d-321ad17d070f', foundational, capital_cycle_speed_priority).
narrative_ontology:cs_axiom_status(capital_cycle_speed_priority, holdable).
narrative_ontology:cs_axiom_grounding('5c310890-60ea-4021-ab1d-321ad17d070f', capital_cycle_speed_priority, instrumental).
narrative_ontology:cs_reference_frame('5c310890-60ea-4021-ab1d-321ad17d070f', rapid_renewable_transition).
narrative_ontology:cs_drift_state('5c310890-60ea-4021-ab1d-321ad17d070f', contemporary_grid_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5c310890-60ea-4021-ab1d-321ad17d070f', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, storage_manufacturers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, distributed_energy_advocates).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_industry).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, baseload_operators).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__renewable_primacy_reading, grid_modernization_imperative).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__renewable_primacy_reading, decentralized_energy_transition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive production tax credits, priority grid access, and favorable permitting regimes. Their business model depends on sustained policy preference for variable renewable energy over dispatchable alternatives.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_developers, beneficiary,
    organized, biographical, mobile, global).

% Supply lithium-ion batteries and grid-scale storage systems. Their growth is predicated on the promise that storage resolves renewable intermittency, making them necessary infrastructure rather than optional add-ons.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, storage_manufacturers, beneficiary,
    organized, biographical, mobile, global).

% Advocate for rooftop solar, community microgrids, and prosumer models. Benefit from policy frameworks that privilege distributed generation over centralized baseload plants.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, distributed_energy_advocates, beneficiary,
    moderate, generational, constrained, national).

% Operates existing reactors and seeks to build new ones. Faces exclusion from green taxonomies, loss of subsidy eligibility, and regulatory environments that extend permitting timelines while fast-tracking renewables. Capital flees to shorter-cycle projects.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_industry, payer,
    powerful, generational, constrained, national).

% Manage grids with dispatchable thermal and nuclear plants. Must integrate rising shares of variable renewables while retiring baseload, increasing balancing costs and reliability risks. Their operational expertise is devalued in policy frameworks that treat all electrons as interchangeable.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, baseload_operators, payer,
    institutional, generational, constrained, national).

% Produce integrated assessment models and scenario analyses that treat high renewable penetration as the default optimal pathway. Their authority derives from techno-economic modeling; they set the terms of feasibility for national climate plans.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, climate_policy_institutions, agenda_setter,
    institutional, civilizational, analytical, global).

% Provide flexible gas generation that often fills gaps left by variable renewables. Are not centered in the renewable-vs-nuclear debate despite being the practical backup for intermittency; their role is obscured by the storage-will-solve-it narrative.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, fossil_gas_operators, excluded,
    powerful, biographical, mobile, global).

% Engineers with operational expertise in frequency regulation and grid stability. Their warnings about inertia and synchronous generation loss are marginalized in policy processes dominated by capacity-planning models that assume perfect storage substitution.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, grid_reliability_engineers, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns global climate investment toward scalable, mass-manufacturable energy technologies by solving the collective-action problem of which zero-carbon pathway to scale first, avoiding fragmentation across incompatible infrastructure.
% TRANSFER_FUNCTION: Moves capital, subsidies, and regulatory favor from centralized baseload generation toward variable renewable and battery storage supply chains, and from incumbent utilities to new renewable developers.
% ABSENT_VOICES: Grid reliability engineers concerned about synchronous inertia loss, nuclear engineers with operational baseload expertise, and fossil gas operators who fill intermittency gaps are structurally marginalized in integrated assessment modeling processes that treat all generation as interchangeable capacity.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, capital would reallocate toward nuclear and dispatchable thermal projects, subsidy structures would technology-neutrally favor carbon intensity rather than renewable identity, and grid planning would re-center reliability engineering over capacity-factor optimization.
% FOUNDING_PROBLEM: Climate change requires rapid decarbonization of electricity; early renewable technologies were marginal and needed policy support to achieve economies of scale against entrenched fossil and nuclear incumbents.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists corroborate the decarbonization imperative from outside the renewable industry. The specific claim that renewable primacy, as opposed to technology-neutral decarbonization, was necessary to solve the founding problem is contested by nuclear engineers and grid reliability experts from outside the benefiting parties; no undisputed corroboration exists for the exclusionary pathway framing.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__renewable_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__renewable_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__renewable_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__renewable_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__renewable_primacy_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__renewable_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__renewable_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__renewable_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the substantial diversion of capital and policy support from nuclear to renewables, measured by subsidy asymmetry and taxonomy exclusion. Suppression (0.60) is structural: nuclear projects face extended permitting, exclusion from green bond eligibility, and social licensure deficits manufactured by the primacy narrative. Theater ratio (0.42) captures the growing performative gap between storage-will-solve-intermittency claims and actual grid reliance on gas peaking or demand curtailment. Accessibility collapse (0.55) measures how alternative technology pathways become unspeakable in climate policy forums dominated by the renewable primacy frame. Resistance (0.60) reflects organized pushback from nuclear utilities and technology-neutral climate advocates. The measurement series tracks rising extraction and theater from 2000â2024 as the reading matured from genuine niche support to entrenched industrial policy.
 *
 * PERSPECTIVAL GAP:
 *   The renewable developer seat experiences the constraint as enabling coordination that unlocked economies of scale; the nuclear industry seat experiences the same structure as enforced exclusion from the climate solution set. The agenda-setter seat experiences it as legitimate optimization, while the baseload operator seat experiences it as reliability risk imposed by modelers who do not operate grids. These divergences are structurally derived from beneficiary/victim roles and exit asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewable developers and storage manufacturers are declared beneficiaries â they collect rents from the policy preference and have mobile exit. Distributed energy advocates benefit from cultural and policy alignment with their long-held goals. Nuclear industry and baseload operators are declared victims â they bear the cost of capital diversion and operational devaluation, with constrained exit due to asset specificity and regulatory lock-in. Climate policy institutions sit near symmetric: they do not collect financial rents but accrue epistemic authority from the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â rapid decarbonization â remains live, preventing automatic mandatrophy classification. However, the technology-specific solution is contested, with evidence that the exclusionary framing may outlive its original rationale as grid reliability challenges emerge. The theater_ratio trajectory (0.08 to 0.42) suggests partial mandatrophy drift: an increasing share of maintenance activity is performative defense of the primacy narrative against empirical complexity. The constraint is not yet a piton because beneficiaries remain concentrated and actively defend it, but the rising theater indicates incipient degradation of the coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    renewable_primacy_kernel_reading,
    'This constraint is the renewable_primacy_reading of kernel climate_mitigation_legitimacy. Would a baseload_necessity_reading or portfolio_pragmatism_reading change the beneficiary/victim structure?',
    'Cross-reading comparison of directionality derivation: in portfolio_pragmatism_reading, nuclear_industry would shift from payer to beneficiary, eliminating asymmetric extraction.',
    'Sibling readings dissolve the victim set identified here; if the kernel genuinely admits multiple readings, the extraction may be reading-dependent rather than structurally intrinsic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(renewable_primacy_kernel_reading, conceptual, 'Committer frame: this constraint as one reading of a contested kernel.').

omega_variable(
    storage_sufficiency_uncertainty,
    'Can current and projected battery storage technologies technically and economically provide seasonal storage and grid inertia at the scale required for 100% renewable systems?',
    'Empirical deployment at high-renewable-penetration grids without nuclear or fossil baseload, measured by cost and reliability outcomes over multi-year periods.',
    'If storage fails at scale, the coordination function of this constraint collapses and the extraction from nuclear becomes pure opportunity cost; if storage succeeds, the coordination function is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(storage_sufficiency_uncertainty, empirical, 'Whether storage can deliver the grid services promised by the renewable primacy reading.').

omega_variable(
    framing_under_determination,
    'Is the authoritative kernel the decarbonization imperative itself, or the specific techno-economic modeling frameworks that render renewable primacy optimal?',
    'Comparison of model outputs with alternative objective functions (reliability-constrained, security-constrained, capital-risk-adjusted) to see if the primacy conclusion is robust to framing.',
    'If the kernel is the decarbonization imperative, alternative readings are co-valid; if the kernel is the specific modeling framework, the reading is a commitment system with extraction authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_under_determination, conceptual, 'Alternative framings of the commitment system kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__renewable_primacy_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(climate_mitigation_legitimacy_renewable_primacy_tr_t0, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(climate_mitigation_legitimacy_renewable_primacy_tr_t4, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 4, 0.12).
narrative_ontology:measurement(climate_mitigation_legitimacy_renewable_primacy_tr_t8, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(climate_mitigation_legitimacy_renewable_primacy_tr_t12, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement(climate_mitigation_legitimacy_renewable_primacy_tr_t16, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(climate_mitigation_legitimacy_renewable_primacy_tr_t20, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(climate_mitigation_legitimacy_renewable_primacy_tr_t24, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(climate_mitigation_legitimacy_renewable_primacy_be_t0, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(climate_mitigation_legitimacy_renewable_primacy_be_t4, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 4, 0.28).
narrative_ontology:measurement(climate_mitigation_legitimacy_renewable_primacy_be_t8, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(climate_mitigation_legitimacy_renewable_primacy_be_t12, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 12, 0.48).
narrative_ontology:measurement(climate_mitigation_legitimacy_renewable_primacy_be_t16, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(climate_mitigation_legitimacy_renewable_primacy_be_t20, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(climate_mitigation_legitimacy_renewable_primacy_be_t24, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 24, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(climate_mitigation_legitimacy_renewable_primacy_su_t0, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(climate_mitigation_legitimacy_renewable_primacy_su_t4, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 4, 0.32).
narrative_ontology:measurement(climate_mitigation_legitimacy_renewable_primacy_su_t8, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(climate_mitigation_legitimacy_renewable_primacy_su_t12, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(climate_mitigation_legitimacy_renewable_primacy_su_t16, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(climate_mitigation_legitimacy_renewable_primacy_su_t20, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 20, 0.64).
narrative_ontology:measurement(climate_mitigation_legitimacy_renewable_primacy_su_t24, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 24, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__renewable_primacy_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
