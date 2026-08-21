% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_obligation__degrowth_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: climate_response_obligation__degrowth_reading
 *   human_readable: Degrowth Reading of Climate Response Obligation: Sufficiency over Efficiency
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint represents the 'degrowth' reading of the climate response
 *   obligation, emphasizing the necessity of reducing material throughput and
 *   prioritizing sufficiency over efficiency to remain within planetary
 *   boundaries. It posits that current global consumption patterns,
 *   particularly in the Global North, are extractive and must be curtailed,
 *   even if it means constraining traditional economic growth. Planetary
 *   systems and future generations are the primary beneficiaries, while
 *   high-consuming populations and fossil capital industries are the primary
 *   targets/victims. The constraint is claimed as a 'snare' because its
 *   implementation would require significant coercion and suppression of
 *   existing economic and social structures, with identifiable victims in the
 *   form of reduced consumption and economic activity for some, while
 *   benefiting others.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__degrowth_reading, 0.85).
domain_priors:suppression_score(climate_response_obligation__degrowth_reading, 0.7).
domain_priors:theater_ratio(climate_response_obligation__degrowth_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__degrowth_reading, snare).
narrative_ontology:human_readable(climate_response_obligation__degrowth_reading, "Degrowth Reading of Climate Response Obligation: Sufficiency over Efficiency").
narrative_ontology:topic_domain(climate_response_obligation__degrowth_reading, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__degrowth_reading, '70761310-e271-4abe-a800-099261681611').
narrative_ontology:cs_kernel_codification('70761310-e271-4abe-a800-099261681611', distributed).
narrative_ontology:cs_authority_grounding('70761310-e271-4abe-a800-099261681611', diffuse_epistemic).
narrative_ontology:cs_reading_relation('70761310-e271-4abe-a800-099261681611', climate_response_obligation__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('70761310-e271-4abe-a800-099261681611', climate_response_obligation__adaptation_priority, influences).
narrative_ontology:cs_axiom('70761310-e271-4abe-a800-099261681611', foundational, planetary_boundaries_are_absolute_limits).
narrative_ontology:cs_axiom_status(planetary_boundaries_are_absolute_limits, holdable).
narrative_ontology:cs_axiom_grounding('70761310-e271-4abe-a800-099261681611', planetary_boundaries_are_absolute_limits, empirically_contingent).
narrative_ontology:cs_axiom('70761310-e271-4abe-a800-099261681611', foundational, sufficiency_over_efficiency_is_ethical_imperative).
narrative_ontology:cs_axiom_status(sufficiency_over_efficiency_is_ethical_imperative, holdable).
narrative_ontology:cs_axiom_grounding('70761310-e271-4abe-a800-099261681611', sufficiency_over_efficiency_is_ethical_imperative, deontological).
narrative_ontology:cs_reference_frame('70761310-e271-4abe-a800-099261681611', pre_industrial_ecological_balance).
narrative_ontology:cs_drift_state('70761310-e271-4abe-a800-099261681611', contemporary_overshoot_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('70761310-e271-4abe-a800-099261681611', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(climate_response_obligation__degrowth_reading, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, planetary_systems).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, future_generations).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, global_north_consumers).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, fossil_capital_industry).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, growth_oriented_economies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from reduced material throughput and extraction, leading to stabilization within ecological limits. Currently bears the brunt of overshoot.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, planetary_systems, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(climate_response_obligation__degrowth_reading, planetary_systems).

% Inherits a stable climate and ecological systems, free from the burdens of ecological collapse. Currently has no voice in policy decisions.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, future_generations, beneficiary,
    powerless, generational, trapped, global).

% Required to significantly reduce consumption and material throughput, leading to lifestyle changes and potential economic contraction. Faces resistance to these changes due to ingrained habits and economic structures.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_north_consumers, payer,
    moderate, biographical, constrained, global).

% Faces existential threat due to the imperative to cease extraction and production of fossil fuels. Its business model is directly targeted by the degrowth imperative, leading to strong resistance.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, fossil_capital_industry, payer,
    institutional, immediate, constrained, global).

% Must fundamentally restructure their economic models away from continuous growth, impacting GDP, employment, and investment. This represents a radical departure from current economic paradigms.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, growth_oriented_economies, payer,
    institutional, generational, constrained, global).

% While benefiting from a stable climate, their aspirations for development and poverty eradication are constrained by the degrowth imperative, especially if the Global North does not reduce first. They demand climate justice and historical responsibility.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_south_nations, excluded,
    organized, generational, constrained, global).

% Actively promote the degrowth agenda, advocating for policies that prioritize ecological sustainability and social equity over economic growth. They face significant political and economic opposition.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, degrowth_advocates, agenda_setter,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_obligation__degrowth_reading, planetary_systems).
narrative_ontology:fixing_cost_class(climate_response_obligation__degrowth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates human economic activity to operate within the biophysical limits of the planet, ensuring long-term ecological stability and equitable resource distribution.
% TRANSFER_FUNCTION: Transfers ecological space and resources from current high-consuming economies (Global North) to planetary systems and future generations, by reducing material and energy throughput.
% ABSENT_VOICES: Future generations are structurally absent, unable to advocate for their interests directly. Global South nations, while present in climate discourse, are often excluded from setting the fundamental terms of the debate, which this reading seeks to rectify by prioritizing their development needs within a degrowth framework.
% DISAPPEARANCE_RATIONALE: If the imperative to reduce material throughput vanished, current economic systems would continue their growth trajectory, leading to accelerated ecological collapse, resource depletion, and increased climate instability. The global economy and planetary systems would reorganize towards a less sustainable, more extractive path.
% FOUNDING_PROBLEM: Human economic activity, driven by continuous growth, is exceeding planetary boundaries, leading to climate change, biodiversity loss, and resource depletion, threatening the long-term habitability of Earth.
% FOUNDING_PROBLEM_CORROBORATION: The scientific consensus on planetary boundaries and ecological overshoot, as documented by IPCC reports and ecological footprint analyses, corroborates the live status of the founding problem. This is attested by independent scientific bodies and intergovernmental organizations, not just the degrowth advocates.
narrative_ontology:disappearance_verdict(climate_response_obligation__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__degrowth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__degrowth_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_response_obligation__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__degrowth_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_obligation__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the significant changes required from current economic systems and lifestyles, particularly in the Global North. Suppression (0.70) is substantial because the degrowth agenda challenges deeply entrenched growth paradigms and powerful economic interests, requiring active enforcement to overcome resistance. Theater ratio is low (0.10) because the degrowth reading is a direct, functional challenge to existing structures, with little room for performative maintenance without genuine change. Resistance is high (0.90) due to the radical nature of the proposed changes and the direct impact on powerful stakeholders. Accessibility collapse (0.60) is moderate, as alternative economic models exist but are not widely adopted or easily accessible within current systems.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of planetary systems and future generations, this constraint is a necessary 'rope' or even a 'mountain' (natural law) for survival. However, from the perspective of Global North consumers and fossil capital industries, it is a 'snare' that extracts their current privileges and economic models. Degrowth advocates view it as a 'scaffold' for a just and sustainable transition. The engine's classification will highlight these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Planetary systems and future generations are full beneficiaries (d=0.0) as the constraint directly addresses their long-term well-being. Global North consumers, fossil capital industry, and growth-oriented economies are full targets (d=1.0) as the constraint demands significant reductions and restructuring from them. Global South nations are 'excluded' in the sense that their development aspirations are constrained, even if the overall goal benefits them, leading to a complex directionality that might require an override if not captured by the 'payer' role. Degrowth advocates are agenda-setters, pushing for the constraint's implementation.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the degrowth imperative as mere coordination. While it aims for planetary coordination, its implementation requires significant extraction from existing power structures and consumption patterns. Recognizing it as a 'snare' from certain seats highlights the coercive aspects necessary to achieve its goals, rather than assuming voluntary compliance. The high resistance and suppression metrics indicate that the mandate is far from atrophied; it is actively contested and requires substantial force to implement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    degrowth_feasibility_empirical,
    'Is a global degrowth transition empirically feasible without causing widespread social and economic collapse, particularly in the Global South?',
    'Long-term empirical studies of degrowth policies implemented at national or regional scales, assessing their social, economic, and ecological outcomes.',
    'If empirically infeasible, the constraint''s ''snare'' classification would be amplified due to the high costs with no viable path to benefits. If feasible, it would strengthen the ''scaffold'' or ''rope'' aspects for beneficiaries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(degrowth_feasibility_empirical, empirical, 'Uncertainty regarding the practical implementability of degrowth policies.').

omega_variable(
    global_north_responsibility_conceptual,
    'To what extent does the Global North bear historical responsibility for ecological overshoot, and how should this translate into differentiated degrowth obligations?',
    'International negotiations and ethical frameworks establishing principles of climate justice and common but differentiated responsibilities.',
    'A strong affirmation of historical responsibility would shift more of the ''extraction'' burden onto the Global North, potentially reclassifying Global South nations from ''constrained'' to ''beneficiary'' if their development space is protected.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_north_responsibility_conceptual, conceptual, 'Ambiguity in the ethical distribution of degrowth burdens based on historical emissions.').

omega_variable(
    capital_accumulation_extractive_mechanism,
    'Is capital accumulation itself an inherently extractive mechanism that must be dismantled for degrowth, or can it be reoriented towards sustainable ends?',
    'Theoretical and empirical analysis of alternative economic models (e.g., post-growth economics, circular economy models) and their relationship to capital dynamics.',
    'If capital accumulation is inherently extractive, the constraint''s ''snare'' nature is fundamental and deep-seated. If reorientable, the constraint might evolve towards a ''scaffold'' for systemic transformation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_accumulation_extractive_mechanism, conceptual, 'The role of capital accumulation as an extractive force within the degrowth framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__degrowth_reading, 2020, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2020, climate_response_obligation__degrowth_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(clim_tr_t2025, climate_response_obligation__degrowth_reading, theater_ratio, 2025, 0.12).
narrative_ontology:measurement(clim_tr_t2030, climate_response_obligation__degrowth_reading, theater_ratio, 2030, 0.1).
narrative_ontology:measurement(clim_tr_t2035, climate_response_obligation__degrowth_reading, theater_ratio, 2035, 0.08).
narrative_ontology:measurement(clim_tr_t2040, climate_response_obligation__degrowth_reading, theater_ratio, 2040, 0.09).
narrative_ontology:measurement(clim_tr_t2045, climate_response_obligation__degrowth_reading, theater_ratio, 2045, 0.1).
narrative_ontology:measurement(clim_tr_t2050, climate_response_obligation__degrowth_reading, theater_ratio, 2050, 0.1).

% Extraction over time
narrative_ontology:measurement(clim_be_t2020, climate_response_obligation__degrowth_reading, base_extractiveness, 2020, 0.8).
narrative_ontology:measurement(clim_be_t2025, climate_response_obligation__degrowth_reading, base_extractiveness, 2025, 0.82).
narrative_ontology:measurement(clim_be_t2030, climate_response_obligation__degrowth_reading, base_extractiveness, 2030, 0.85).
narrative_ontology:measurement(clim_be_t2035, climate_response_obligation__degrowth_reading, base_extractiveness, 2035, 0.87).
narrative_ontology:measurement(clim_be_t2040, climate_response_obligation__degrowth_reading, base_extractiveness, 2040, 0.88).
narrative_ontology:measurement(clim_be_t2045, climate_response_obligation__degrowth_reading, base_extractiveness, 2045, 0.89).
narrative_ontology:measurement(clim_be_t2050, climate_response_obligation__degrowth_reading, base_extractiveness, 2050, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2020, climate_response_obligation__degrowth_reading, suppression_requirement, 2020, 0.65).
narrative_ontology:measurement(clim_su_t2025, climate_response_obligation__degrowth_reading, suppression_requirement, 2025, 0.68).
narrative_ontology:measurement(clim_su_t2030, climate_response_obligation__degrowth_reading, suppression_requirement, 2030, 0.7).
narrative_ontology:measurement(clim_su_t2035, climate_response_obligation__degrowth_reading, suppression_requirement, 2035, 0.72).
narrative_ontology:measurement(clim_su_t2040, climate_response_obligation__degrowth_reading, suppression_requirement, 2040, 0.7).
narrative_ontology:measurement(clim_su_t2045, climate_response_obligation__degrowth_reading, suppression_requirement, 2045, 0.68).
narrative_ontology:measurement(clim_su_t2050, climate_response_obligation__degrowth_reading, suppression_requirement, 2050, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__degrowth_reading, global_infrastructure).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, climate_response_obligation__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, climate_response_obligation__adaptation_priority).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'climate_response_obligation' kernel. It emphasizes reducing material throughput, contrasting with mitigation_priority (rapid decarbonization) and adaptation_priority (resilience building). Each reading represents a distinct structural approach to the same overarching problem.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
