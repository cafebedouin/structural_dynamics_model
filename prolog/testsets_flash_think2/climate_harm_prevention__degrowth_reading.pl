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
 *   constraint_id: climate_harm_prevention__degrowth_reading
 *   human_readable: Planned Economic Contraction for Climate Harm Prevention (Degrowth Reading)
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint represents the 'degrowth_reading' of the broader
 *   'climate_harm_prevention' kernel. It asserts that a legitimate and
 *   effective climate response necessitates planned economic contraction in
 *   the Global North, arguing that mitigation efforts within a conventional
 *   growth framework are physically and politically impossible. This reading
 *   frames continuous growth as the root cause of ecological overshoot and
 *   social injustice, demanding a fundamental shift in economic paradigms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__degrowth_reading, 0.85).
domain_priors:suppression_score(climate_harm_prevention__degrowth_reading, 0.75).
domain_priors:theater_ratio(climate_harm_prevention__degrowth_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__degrowth_reading, "Planned Economic Contraction for Climate Harm Prevention (Degrowth Reading)").
narrative_ontology:topic_domain(climate_harm_prevention__degrowth_reading, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__degrowth_reading, 'a0eeb62a-07ed-4098-ae36-51a34dd79b07').
narrative_ontology:cs_kernel_codification('a0eeb62a-07ed-4098-ae36-51a34dd79b07', implicit).
narrative_ontology:cs_authority_grounding('a0eeb62a-07ed-4098-ae36-51a34dd79b07', distributed).
narrative_ontology:cs_reading_relation('a0eeb62a-07ed-4098-ae36-51a34dd79b07', climate_harm_prevention__mitigation_priority, forecloses).
narrative_ontology:cs_reading_relation('a0eeb62a-07ed-4098-ae36-51a34dd79b07', climate_harm_prevention__adaptation_priority, coexists_with).
narrative_ontology:cs_axiom('a0eeb62a-07ed-4098-ae36-51a34dd79b07', foundational, infinite_growth_impossible_on_finite_planet).
narrative_ontology:cs_axiom_status(infinite_growth_impossible_on_finite_planet, holdable).
narrative_ontology:cs_axiom_grounding('a0eeb62a-07ed-4098-ae36-51a34dd79b07', infinite_growth_impossible_on_finite_planet, empirically_contingent).
narrative_ontology:cs_axiom('a0eeb62a-07ed-4098-ae36-51a34dd79b07', foundational, intergenerational_equity_requires_contraction).
narrative_ontology:cs_axiom_status(intergenerational_equity_requires_contraction, holdable).
narrative_ontology:cs_axiom_grounding('a0eeb62a-07ed-4098-ae36-51a34dd79b07', intergenerational_equity_requires_contraction, deontological).
narrative_ontology:cs_reference_frame('a0eeb62a-07ed-4098-ae36-51a34dd79b07', planetary_boundaries_framework).
narrative_ontology:cs_drift_state('a0eeb62a-07ed-4098-ae36-51a34dd79b07', contemporary_growth_paradigm, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('a0eeb62a-07ed-4098-ae36-51a34dd79b07', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__degrowth_reading, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, global_south_nations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, vulnerable_ecosystems).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, global_north_consumers).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, growth_oriented_industries).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, fossil_fuel_lobby).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Propose and articulate the degrowth framework, advocating for systemic change in economic and social structures to achieve ecological sustainability and social justice. They seek to shift the global policy agenda.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, degrowth_advocates, agenda_setter,
    analytical, generational, analytical, global).

% Bear the direct costs of reduced consumption, lifestyle changes, and potentially lower material living standards as a result of planned economic contraction. Their choices are constrained by policy and evolving social norms.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_north_consumers, payer,
    powerful, biographical, constrained, global).

% Benefit from reduced climate impacts, increased ecological space, and potential for equitable resource redistribution and reparations. Their agency in global policy is often constrained by historical power imbalances.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_south_nations, beneficiary,
    organized, generational, constrained, global).

% Primary beneficiaries of a stable climate and preserved ecological systems, which degrowth aims to secure. They have no direct voice or agency in current policy decisions.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% Face existential threats to their business models, which are predicated on continuous expansion and resource extraction. They would require fundamental restructuring or contraction under a degrowth paradigm.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, growth_oriented_industries, payer,
    institutional, biographical, constrained, global).

% Directly targeted by policies that would dismantle their industry. They face complete loss of influence and profit, with their business model rendered obsolete by the degrowth imperative. Their exit is effectively trapped by the constraint's logic.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, fossil_fuel_lobby, payer,
    institutional, biographical, trapped, global).

% Benefit from reduced human impact, leading to recovery and stability of biodiversity and ecological functions. They have no agency in human decision-making.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, vulnerable_ecosystems, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(climate_harm_prevention__degrowth_reading, vulnerable_ecosystems).

% Their growth-centric models and policy prescriptions are fundamentally rejected by the degrowth reading, effectively excluding their traditional approaches from the core debate on climate response.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, mainstream_economists, excluded,
    institutional, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_harm_prevention__degrowth_reading, diffuse).
narrative_ontology:fixing_cost_class(climate_harm_prevention__degrowth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate a global, equitable response to climate change by reallocating planetary resources and reducing aggregate consumption, ensuring ecological stability and intergenerational justice.
% TRANSFER_FUNCTION: Transfers ecological space, resource consumption capacity, and economic growth potential from the Global North (present) to the Global South and future generations.
% ABSENT_VOICES: Mainstream economists who insist on green growth, political parties committed to GDP growth, and industries whose business models depend on continuous expansion are structurally excluded from the degrowth discourse.
% DISAPPEARANCE_RATIONALE: If the imperative for degrowth vanished, the current growth-oriented trajectory would continue, leading to accelerated climate breakdown and exacerbating existing inequalities, fundamentally altering the future world in a negative direction.
% FOUNDING_PROBLEM: The recognition that infinite economic growth on a finite planet is ecologically unsustainable and socially unjust, leading to climate catastrophe and resource depletion, disproportionately harming the Global South and future generations.
% FOUNDING_PROBLEM_CORROBORATION: Ecological scientists, climate justice activists, and some intergovernmental reports (e.g., IPCC scenarios that include demand-side reductions) corroborate the problem's urgency and the inadequacy of growth-centric solutions. This corroboration comes from outside the immediate beneficiaries of degrowth.
narrative_ontology:disappearance_verdict(climate_harm_prevention__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__degrowth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__degrowth_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   The high extractiveness (0.85) reflects the significant economic and lifestyle changes demanded from Global North consumers and industries. Suppression (0.75) is high due to the need to actively counter and suppress dominant growth-oriented narratives and policies. The low theater ratio (0.10) indicates that this reading is direct and critical, offering little room for performative or symbolic actions without substantive change. Resistance is very high (0.90) as it challenges deeply entrenched economic and political interests.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of degrowth advocates, this constraint is a necessary, albeit difficult, coordination mechanism for planetary survival. From the perspective of Global North consumers and growth-oriented industries, it is a highly extractive and suppressive demand that threatens their established way of life and economic models. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Degrowth advocates (agenda_setter) articulate the constraint. Global South nations, future generations, and vulnerable ecosystems are the primary beneficiaries, gaining ecological space and justice. Global North consumers, growth-oriented industries, and the fossil fuel lobby are the primary targets/payers, bearing the costs of contraction and systemic change. Mainstream economists are excluded as their foundational assumptions are rejected.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    green_growth_viability,
    'Is mitigation within a growth framework truly physically/politically impossible, or are ''green growth'' alternatives viable for climate harm prevention?',
    'Empirical observation of decoupling rates of GDP from resource use and emissions in developed economies over a sustained period (e.g., 20-30 years), combined with political feasibility analysis of large-scale green transitions.',
    'If green growth proves viable, the degrowth reading''s core premise is weakened, potentially reclassifying its extractiveness as unnecessary. If green growth fails, the degrowth reading''s necessity is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(green_growth_viability, empirical, 'The feasibility of decoupling economic growth from environmental impact.').

omega_variable(
    global_north_acceptance_of_contraction,
    'To what extent would Global North populations and political systems genuinely accept and implement planned economic contraction?',
    'Sociological studies of public opinion, political science analysis of policy adoption in democratic systems, and observation of pilot programs or regional initiatives for degrowth.',
    'Low acceptance implies high suppression would be required, increasing the constraint''s coercive character. High acceptance would shift the constraint closer to a genuine Rope, indicating a collective-action problem solved with consent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_north_acceptance_of_contraction, empirical, 'Political and social feasibility of degrowth policies in the Global North.').

omega_variable(
    equitable_redistribution_mechanisms,
    'What are the precise and implementable mechanisms for equitable resource reallocation and wealth transfer from the Global North to the Global South under a degrowth paradigm?',
    'Development of detailed policy proposals, international agreements, and pilot projects demonstrating effective and just redistribution without creating new forms of extraction or dependency.',
    'Lack of clear, equitable mechanisms would undermine the justice claims of the degrowth reading, potentially exposing it to critiques of being another form of top-down control. Clear mechanisms would strengthen its legitimacy as a coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equitable_redistribution_mechanisms, conceptual, 'Feasibility and justice of degrowth''s redistribution mechanisms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__degrowth_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_harm_prevention__degrowth_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(clim_tr_t10, climate_harm_prevention__degrowth_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(clim_tr_t20, climate_harm_prevention__degrowth_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(clim_tr_t30, climate_harm_prevention__degrowth_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(clim_tr_t40, climate_harm_prevention__degrowth_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(clim_tr_t50, climate_harm_prevention__degrowth_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_harm_prevention__degrowth_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(clim_be_t10, climate_harm_prevention__degrowth_reading, base_extractiveness, 10, 0.77).
narrative_ontology:measurement(clim_be_t20, climate_harm_prevention__degrowth_reading, base_extractiveness, 20, 0.79).
narrative_ontology:measurement(clim_be_t30, climate_harm_prevention__degrowth_reading, base_extractiveness, 30, 0.81).
narrative_ontology:measurement(clim_be_t40, climate_harm_prevention__degrowth_reading, base_extractiveness, 40, 0.83).
narrative_ontology:measurement(clim_be_t50, climate_harm_prevention__degrowth_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_harm_prevention__degrowth_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(clim_su_t10, climate_harm_prevention__degrowth_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement(clim_su_t20, climate_harm_prevention__degrowth_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(clim_su_t30, climate_harm_prevention__degrowth_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(clim_su_t40, climate_harm_prevention__degrowth_reading, suppression_requirement, 40, 0.73).
narrative_ontology:measurement(clim_su_t50, climate_harm_prevention__degrowth_reading, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__degrowth_reading, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
