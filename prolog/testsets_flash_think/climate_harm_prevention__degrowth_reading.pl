% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__degrowth_reading, []).

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
 *   constraint_id: climate_harm_prevention__degrowth_reading
 *   human_readable: Planned Economic Contraction for Climate Harm Prevention (Degrowth Reading)
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint represents the 'degrowth' reading of the broader 'climate
 *   harm prevention' kernel. It asserts that a legitimate climate response
 *   *requires* planned economic contraction in the Global North, as
 *   mitigation efforts within a growth framework are deemed physically and
 *   politically impossible. This reading frames degrowth not as an option,
 *   but as a necessary, albeit highly extractive, transition (a scaffold) to
 *   prevent catastrophic climate and ecological breakdown. The high
 *   extractiveness reflects the profound changes demanded from
 *   growth-dependent economies and consumers in the Global North.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__degrowth_reading, 0.85).
domain_priors:suppression_score(climate_harm_prevention__degrowth_reading, 0.78).
domain_priors:theater_ratio(climate_harm_prevention__degrowth_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__degrowth_reading, scaffold).
narrative_ontology:human_readable(climate_harm_prevention__degrowth_reading, "Planned Economic Contraction for Climate Harm Prevention (Degrowth Reading)").
narrative_ontology:topic_domain(climate_harm_prevention__degrowth_reading, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__degrowth_reading).
narrative_ontology:has_sunset_clause(climate_harm_prevention__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__degrowth_reading, '18a7ad5c-da9f-45bb-bbd6-2364ca097866').
narrative_ontology:cs_kernel_codification('18a7ad5c-da9f-45bb-bbd6-2364ca097866', formalized).
narrative_ontology:cs_authority_grounding('18a7ad5c-da9f-45bb-bbd6-2364ca097866', expertise).
narrative_ontology:cs_interpretation_layer_present('18a7ad5c-da9f-45bb-bbd6-2364ca097866').
narrative_ontology:cs_reading_relation('18a7ad5c-da9f-45bb-bbd6-2364ca097866', climate_harm_prevention__mitigation_priority, forecloses).
narrative_ontology:cs_reading_relation('18a7ad5c-da9f-45bb-bbd6-2364ca097866', climate_harm_prevention__adaptation_priority, forecloses).
narrative_ontology:cs_axiom('18a7ad5c-da9f-45bb-bbd6-2364ca097866', foundational, planetary_boundaries_are_absolute).
narrative_ontology:cs_axiom_status(planetary_boundaries_are_absolute, holdable).
narrative_ontology:cs_axiom_grounding('18a7ad5c-da9f-45bb-bbd6-2364ca097866', planetary_boundaries_are_absolute, empirically_contingent).
narrative_ontology:cs_axiom('18a7ad5c-da9f-45bb-bbd6-2364ca097866', foundational, infinite_growth_on_finite_planet_impossible).
narrative_ontology:cs_axiom_status(infinite_growth_on_finite_planet_impossible, holdable).
narrative_ontology:cs_axiom_grounding('18a7ad5c-da9f-45bb-bbd6-2364ca097866', infinite_growth_on_finite_planet_impossible, empirically_contingent).
narrative_ontology:cs_reference_frame('18a7ad5c-da9f-45bb-bbd6-2364ca097866', ecological_limits_framework).
narrative_ontology:cs_drift_state('18a7ad5c-da9f-45bb-bbd6-2364ca097866', contemporary_growth_paradigm, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('18a7ad5c-da9f-45bb-bbd6-2364ca097866', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__degrowth_reading, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, global_south_nations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, planetary_ecosystems).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, global_north_consumers).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, extractive_industries).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, growth_dependent_economies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Propose and articulate the necessity of planned economic contraction in the Global North to address climate change and ecological overshoot. They seek to implement policies that would enforce this contraction.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, degrowth_advocates, agenda_setter,
    organized, civilizational, analytical, global).

% Would benefit from the Global North's reduced resource consumption and emissions, gaining ecological space and a fairer share of planetary resources, reducing their vulnerability to climate impacts.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_south_nations, beneficiary,
    organized, generational, constrained, global).

% Are the primary beneficiaries of a stable climate and healthy ecosystems, which degrowth aims to secure. They have no voice in current policy decisions but bear the full consequences of inaction.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% Would benefit from reduced human pressure, allowing for ecological regeneration and biodiversity recovery. They are a non-agent entity whose health is directly tied to the constraint's implementation.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, planetary_ecosystems, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(climate_harm_prevention__degrowth_reading, planetary_ecosystems).

% Would experience a planned reduction in material consumption and economic activity, requiring significant lifestyle changes and a re-evaluation of societal priorities away from growth.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_north_consumers, payer,
    moderate, biographical, constrained, national).

% Would face severe restrictions and eventual phase-out of their operations, as degrowth directly targets the material throughput of the economy. They represent significant economic and political power resisting this transition.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, extractive_industries, payer,
    institutional, biographical, constrained, global).

% National and international economic systems built on the imperative of continuous growth would need fundamental restructuring, challenging their foundational assumptions and operational models.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, growth_dependent_economies, payer,
    institutional, biographical, constrained, global).

% Advocate for technological solutions and efficiency gains within a growth paradigm. From the degrowth perspective, their proposals are insufficient and politically/physically impossible to achieve the necessary climate goals, thus they are excluded from the core premise of this reading.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, mitigation_advocates, excluded,
    organized, biographical, mobile, global).

% Prioritize building resilience to climate impacts, often accepting higher warming trajectories. From the degrowth perspective, adaptation without addressing the root cause of growth is a palliative measure that forecloses genuine climate response.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, adaptation_advocates, excluded,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_harm_prevention__degrowth_reading, global_south_nations).
narrative_ontology:fixing_cost_class(climate_harm_prevention__degrowth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate global economic activity and resource use within planetary ecological limits, ensuring a just and sustainable transition away from growth-dependent systems.
% TRANSFER_FUNCTION: Transfers ecological space, material resources, and a stable climate from the Global North's present consumption to the Global South and future generations. It also transfers economic activity and investment away from extractive sectors towards regenerative ones.
% ABSENT_VOICES: Those who benefit from the current growth paradigm, including powerful corporations, financial institutions, and political elites in the Global North, as well as those who believe in the possibility of 'green growth' or purely technological solutions. They would argue against the necessity or feasibility of degrowth.
% DISAPPEARANCE_RATIONALE: If the imperative for planned economic contraction vanished, the current growth-oriented global economy would continue its trajectory, leading to severe and irreversible climate change, ecological collapse, and increased social inequality, fundamentally reorganizing human societies and the planet's habitability in a catastrophic manner.
% FOUNDING_PROBLEM: Unchecked, exponential economic growth, particularly in the Global North, exceeding planetary boundaries, causing irreversible climate change, biodiversity loss, and ecological degradation, disproportionately impacting the Global South and future generations.
% FOUNDING_PROBLEM_CORROBORATION: Scientific consensus on planetary boundaries (e.g., Stockholm Resilience Centre), IPCC reports detailing climate impacts, ecological economics research, and the lived experiences and advocacy of indigenous communities and environmental justice movements globally.
narrative_ontology:disappearance_verdict(climate_harm_prevention__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__degrowth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__degrowth_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(climate_harm_prevention__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__degrowth_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_harm_prevention__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very high (0.85) because it demands a fundamental restructuring of economic systems and a reduction in material consumption, particularly in the Global North. Suppression is also high (0.78) because implementing such a policy would require overcoming immense resistance from entrenched economic interests and prevailing growth ideologies, necessitating strong enforcement. The theater ratio is low (0.1) as this reading is a direct, unvarnished call for systemic change, with little performative cover. Resistance is extremely high (0.9) due to the direct challenge to the status quo. The claimed type is 'scaffold' because degrowth is presented as a transitional necessity to achieve a steady-state, ecologically balanced economy, not the end-state itself.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of degrowth advocates and the Global South, this constraint is a necessary, albeit difficult, path to survival and justice. From the perspective of Global North consumers and industries, it is an unacceptable imposition that threatens prosperity and freedom. The engine's classification will highlight this divergence, showing a 'scaffold' for beneficiaries and a 'snare' or 'tangled_rope' for payers, reflecting the deep structural conflict.
 *
 * DIRECTIONALITY LOGIC:
 *   The Global South, future generations, and planetary ecosystems are clear beneficiaries, gaining ecological space and a more stable climate. Global North consumers, extractive industries, and growth-dependent economies are the targets, bearing the costs of planned contraction. Degrowth advocates act as agenda-setters, pushing for the implementation of this constraint. Other climate advocacy groups (mitigation, adaptation) are excluded from this reading's core premise, as their approaches are seen as insufficient or misguided.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    degrowth_necessity_empirical_basis,
    'Is planned economic contraction truly the only physically and politically possible path to prevent catastrophic climate harm, or are other pathways (e.g., radical technological decarbonization within growth) still viable?',
    'Further empirical evidence on the decoupling of economic growth from resource use and emissions, and political feasibility studies of rapid, large-scale technological transitions versus planned contraction.',
    'If alternative pathways are proven viable, the ''scaffold'' claim for degrowth weakens, potentially reclassifying it as a ''snare'' for those it extracts from, as its necessity would be undermined. If degrowth is confirmed as the only path, its ''scaffold'' nature is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(degrowth_necessity_empirical_basis, empirical, 'Whether degrowth is an unavoidable necessity or one of several possible climate responses.').

omega_variable(
    political_will_and_suppression_requirement,
    'What level of political will and societal suppression would actually be required to implement planned economic contraction in the Global North, and is this level achievable without authoritarian measures?',
    'Comparative analysis of historical instances of rapid economic restructuring (e.g., wartime mobilization) and studies on democratic pathways for degrowth transitions.',
    'If the required suppression is deemed incompatible with democratic governance, the constraint''s ''scaffold'' claim could be challenged, potentially reclassifying it as a ''snare'' due to the coercive nature of its enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_will_and_suppression_requirement, empirical, 'Feasibility and democratic compatibility of the required enforcement for degrowth.').

omega_variable(
    kernel_reading_divergence,
    'Given that this constraint is a ''degrowth_reading'' of the ''climate_harm_prevention'' kernel, how do its structural properties and classification diverge from sibling readings like ''mitigation_priority'' and ''adaptation_priority''?',
    'Comparative analysis of the full constraint stories for each reading, focusing on differences in beneficiaries, victims, extractiveness, and claimed type.',
    'The divergence highlights the deep structural disagreements within the climate response domain, revealing how different framings of the core problem lead to fundamentally different proposed solutions and distributions of costs/benefits.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Documents the structural differences between this degrowth reading and other climate response readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__degrowth_reading, 2024, 2074).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2024, climate_harm_prevention__degrowth_reading, theater_ratio, 2024, 0.1).
narrative_ontology:measurement(clim_tr_t2034, climate_harm_prevention__degrowth_reading, theater_ratio, 2034, 0.09).
narrative_ontology:measurement(clim_tr_t2044, climate_harm_prevention__degrowth_reading, theater_ratio, 2044, 0.08).
narrative_ontology:measurement(clim_tr_t2054, climate_harm_prevention__degrowth_reading, theater_ratio, 2054, 0.07).
narrative_ontology:measurement(clim_tr_t2064, climate_harm_prevention__degrowth_reading, theater_ratio, 2064, 0.06).
narrative_ontology:measurement(clim_tr_t2074, climate_harm_prevention__degrowth_reading, theater_ratio, 2074, 0.05).

% Extraction over time
narrative_ontology:measurement(clim_be_t2024, climate_harm_prevention__degrowth_reading, base_extractiveness, 2024, 0.85).
narrative_ontology:measurement(clim_be_t2034, climate_harm_prevention__degrowth_reading, base_extractiveness, 2034, 0.87).
narrative_ontology:measurement(clim_be_t2044, climate_harm_prevention__degrowth_reading, base_extractiveness, 2044, 0.88).
narrative_ontology:measurement(clim_be_t2054, climate_harm_prevention__degrowth_reading, base_extractiveness, 2054, 0.89).
narrative_ontology:measurement(clim_be_t2064, climate_harm_prevention__degrowth_reading, base_extractiveness, 2064, 0.9).
narrative_ontology:measurement(clim_be_t2074, climate_harm_prevention__degrowth_reading, base_extractiveness, 2074, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2024, climate_harm_prevention__degrowth_reading, suppression_requirement, 2024, 0.78).
narrative_ontology:measurement(clim_su_t2034, climate_harm_prevention__degrowth_reading, suppression_requirement, 2034, 0.8).
narrative_ontology:measurement(clim_su_t2044, climate_harm_prevention__degrowth_reading, suppression_requirement, 2044, 0.82).
narrative_ontology:measurement(clim_su_t2054, climate_harm_prevention__degrowth_reading, suppression_requirement, 2054, 0.83).
narrative_ontology:measurement(clim_su_t2064, climate_harm_prevention__degrowth_reading, suppression_requirement, 2064, 0.84).
narrative_ontology:measurement(clim_su_t2074, climate_harm_prevention__degrowth_reading, suppression_requirement, 2074, 0.84).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__degrowth_reading, global_infrastructure).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, climate_harm_prevention__mitigation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, climate_harm_prevention__adaptation_priority).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'climate_harm_prevention' kernel, each with different structural properties, beneficiaries, and victims. They are linked to highlight the contested nature of legitimate climate response.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
