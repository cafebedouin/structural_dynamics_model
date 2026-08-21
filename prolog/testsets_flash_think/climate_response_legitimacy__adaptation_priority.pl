% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__adaptation_priority, []).

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
 *   constraint_id: climate_response_legitimacy__adaptation_priority
 *   human_readable: Legitimate Climate Response: Adaptation Priority
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint represents the 'adaptation priority' reading of
 *   legitimate climate response, which accepts a certain warming trajectory
 *   and focuses on building resilience for vulnerable populations, often at
 *   the expense of aggressive mitigation. This approach allows wealthy
 *   nations to preserve their development models, deferring significant
 *   intergenerational costs. The classification as a Tangled Rope reflects
 *   the dual function: a claimed coordination (protecting vulnerable)
 *   alongside substantial, asymmetric extraction (from vulnerable populations
 *   and future generations) maintained by active suppression of alternative
 *   responses.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__adaptation_priority, 0.82).
domain_priors:suppression_score(climate_response_legitimacy__adaptation_priority, 0.78).
domain_priors:theater_ratio(climate_response_legitimacy__adaptation_priority, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, extractiveness, 0.82).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__adaptation_priority, "Legitimate Climate Response: Adaptation Priority").
narrative_ontology:topic_domain(climate_response_legitimacy__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__adaptation_priority, '0b0cc880-fb3f-4b83-9f20-c9e0a42e04c2').
narrative_ontology:cs_kernel_codification('0b0cc880-fb3f-4b83-9f20-c9e0a42e04c2', formalized).
narrative_ontology:cs_authority_grounding('0b0cc880-fb3f-4b83-9f20-c9e0a42e04c2', extraction).
narrative_ontology:cs_interpretation_layer_present('0b0cc880-fb3f-4b83-9f20-c9e0a42e04c2').
narrative_ontology:cs_reading_relation('0b0cc880-fb3f-4b83-9f20-c9e0a42e04c2', climate_response_legitimacy__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('0b0cc880-fb3f-4b83-9f20-c9e0a42e04c2', climate_response_legitimacy__degrowth_transformation, coexists_with).
narrative_ontology:cs_axiom('0b0cc880-fb3f-4b83-9f20-c9e0a42e04c2', foundational, economic_growth_is_non_negotiable).
narrative_ontology:cs_axiom_status(economic_growth_is_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('0b0cc880-fb3f-4b83-9f20-c9e0a42e04c2', economic_growth_is_non_negotiable, conventional).
narrative_ontology:cs_axiom('0b0cc880-fb3f-4b83-9f20-c9e0a42e04c2', foundational, adaptation_is_primary_response_to_unavoidable_warming).
narrative_ontology:cs_axiom_status(adaptation_is_primary_response_to_unavoidable_warming, holdable).
narrative_ontology:cs_axiom_grounding('0b0cc880-fb3f-4b83-9f20-c9e0a42e04c2', adaptation_is_primary_response_to_unavoidable_warming, instrumental).
narrative_ontology:cs_reference_frame('0b0cc880-fb3f-4b83-9f20-c9e0a42e04c2', sustainable_development_paradigm).
narrative_ontology:cs_drift_state('0b0cc880-fb3f-4b83-9f20-c9e0a42e04c2', contemporary_impact_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0b0cc880-fb3f-4b83-9f20-c9e0a42e04c2', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, wealthy_nations_industries).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, adaptation_infrastructure_industry).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, vulnerable_populations).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, low_income_regions).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, future_generations).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__adaptation_priority, economic_growth_imperative).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__adaptation_priority, technological_solutionism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These actors define the 'legitimate' climate response, prioritizing adaptation to preserve their existing economic models and defer costly mitigation. They benefit from continued growth and delayed structural change, while often providing insufficient adaptation funding.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, wealthy_nations_industries, agenda_setter,
    institutional, generational, arbitrage, global).

% These communities bear the immediate and severe impacts of unmitigated warming, relying on (often insufficient) adaptation measures. Their livelihoods, homes, and lives are directly threatened, with limited capacity to influence policy.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, vulnerable_populations, payer,
    powerless, immediate, trapped, local).

% These regions face a significant adaptation deficit, receiving inadequate funding and resources to build resilience against climate impacts. They are forced to prioritize immediate survival over long-term development, perpetuating cycles of poverty and vulnerability.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, low_income_regions, payer,
    organized, biographical, constrained, regional).

% These unrepresented actors inherit compounded climate impacts, ecological degradation, and massive adaptation debt due to the deferred mitigation costs of the present. Their well-being is directly extracted from by current policy choices.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, future_generations, payer,
    powerless, civilizational, identity_locked, universal).

% This industry profits significantly from the construction and deployment of resilience infrastructure (e.g., sea walls, early warning systems, climate-resilient agriculture). Their business model aligns with the adaptation-first approach.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, adaptation_infrastructure_industry, beneficiary,
    organized, biographical, mobile, global).

% These groups argue for aggressive emissions reductions and systemic decarbonization. Their proposals are often sidelined or framed as economically unfeasible by the dominant adaptation-priority narrative, despite scientific consensus on the urgency of mitigation.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, mitigation_advocates, excluded,
    organized, generational, constrained, global).

% These groups advocate for dismantling the growth imperative in wealthy nations through structural economic transformation. Their radical proposals are largely dismissed from mainstream climate policy discussions, further entrenching the adaptation-priority frame.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, degrowth_advocates, excluded,
    moderate, generational, constrained, global).

% These experts provide the foundational data on warming trajectories, impacts, and the efficacy of various responses. While their data informs policy, their recommendations for rapid mitigation are often selectively interpreted or ignored in favor of adaptation-focused strategies.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, climate_scientists, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate global efforts to protect vulnerable populations from climate impacts by building resilience infrastructure and adaptive capacity, while allowing wealthy nations to maintain their development models.
% TRANSFER_FUNCTION: Transfers the primary burden of climate change impacts and deferred mitigation costs from wealthy nations and current generations to vulnerable populations, low-income regions, and future generations. It also transfers (often insufficient) adaptation funding to vulnerable regions.
% ABSENT_VOICES: The voices of future generations are entirely absent. Degrowth and radical mitigation advocates are systematically excluded from the core policy-making tables, as are many frontline communities whose lived experience of climate impacts is often tokenized rather than centered.
% DISAPPEARANCE_RATIONALE: If this framework vanished overnight, the global climate response would undergo a profound shift. The premise of 'acceptable warming' would be challenged, leading to increased pressure for aggressive mitigation and potentially degrowth, as the current deferral of costs would become untenable. Resource allocation for adaptation would likely be re-evaluated, potentially leading to more equitable distribution or a stronger focus on loss and damage.
% FOUNDING_PROBLEM: How to address the growing threat of climate change without disrupting existing economic growth models, particularly in wealthy nations, and without requiring politically difficult structural transformations.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (wealthy nations, industries) argue the problem is live, emphasizing the practical difficulties of rapid decarbonization. However, climate scientists and advocates for vulnerable communities argue that the 'problem' has shifted from 'how to respond' to 'how to survive unmitigated impacts' due to insufficient mitigation, and that the current approach is a political choice, not an inevitability. Independent economic analyses also question the long-term viability of deferring mitigation costs.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__adaptation_priority, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(climate_response_legitimacy__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__adaptation_priority, 0.82, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising because the costs of unmitigated warming are borne disproportionately by vulnerable populations and future generations, while wealthy nations continue to benefit from economic growth. Suppression is high because alternative, more transformative climate responses (like rapid mitigation or degrowth) are actively marginalized or dismissed as unfeasible. Theater ratio is moderate and rising, as the rhetoric of 'protecting the vulnerable' increasingly masks the inadequacy of adaptation efforts relative to escalating impacts, and the continued deferral of necessary mitigation. Accessibility collapse is high as the dominant narrative frames aggressive mitigation or degrowth as economically or politically impossible.
 *
 * PERSPECTIVAL GAP:
 *   Wealthy nations and industries experience this as a pragmatic, coordinated response that balances economic stability with climate action. Vulnerable populations and future generations, however, experience it as a highly extractive and suppressive constraint that forces them to bear the brunt of a crisis they did not create, with insufficient support. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Wealthy nations and the adaptation industry are clear beneficiaries, preserving their economic models and profiting from infrastructure projects. Vulnerable populations, low-income regions, and future generations are the primary targets, bearing the direct impacts and deferred costs. Mitigation and degrowth advocates are excluded, their policy alternatives suppressed by the dominant narrative.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this as a pure Rope (genuine coordination) by highlighting the asymmetric extraction and active suppression of alternatives. It also prevents mislabeling as a pure Snare by acknowledging the genuine, albeit often insufficient, efforts towards adaptation and resilience building for vulnerable populations. The 'coordination' function serves as a cover for the underlying extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptation_deficit_causality,
    'Is the persistent adaptation funding gap (estimated at $350B annually for low-income regions) a structural inevitability of climate finance, or a policy choice reflecting a lack of political will from wealthy nations?',
    'Analysis of climate finance mechanisms and political economy of aid, comparing pledged vs. delivered funds, and assessing the impact of conditionalities on recipient countries'' adaptive capacity.',
    'If a policy choice, the extractiveness from low-income regions is higher and more directly attributable to the beneficiaries of this constraint. If structural, it points to deeper systemic issues in global finance that this constraint merely reflects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_deficit_causality, empirical, 'Whether adaptation funding shortfalls are structural or volitional.').

omega_variable(
    intergenerational_equity_framing,
    'Is the deferral of significant mitigation costs and compounded climate impacts to future generations an ethically defensible ''balancing'' of present and future needs, or an extractive mechanism that exploits the political powerlessness of the unborn?',
    'Ethical and legal analysis of intergenerational justice, potentially through the establishment of ''future generations'' advocacy bodies with legal standing in policy debates.',
    'If an extractive mechanism, the extractiveness from future generations is a core, intentional feature of this constraint, not an unfortunate side effect. This would strengthen the Snare-like aspects of the Tangled Rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_equity_framing, conceptual, 'Ethical status of intergenerational cost deferral.').

omega_variable(
    suppression_mechanism_of_alternatives,
    'Is the suppression of aggressive mitigation and degrowth alternatives primarily structural (e.g., economic path dependence, technological lock-in) or ideological (e.g., framing as ''radical'' or ''unrealistic'' to protect vested interests)?',
    'Comparative policy analysis across jurisdictions with different political economies, and discourse analysis of climate policy debates to identify dominant framing strategies and their material effects.',
    'If primarily ideological, the suppression metric is amplified by the active discursive enforcement, making the constraint more Snare-like. If primarily structural, the suppression is a more ''natural'' consequence of existing systems, though still actively maintained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_of_alternatives, empirical, 'Nature of suppression for alternative climate responses.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__adaptation_priority, 2000, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2000, climate_response_legitimacy__adaptation_priority, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(clim_tr_t2010, climate_response_legitimacy__adaptation_priority, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(clim_tr_t2020, climate_response_legitimacy__adaptation_priority, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(clim_tr_t2030, climate_response_legitimacy__adaptation_priority, theater_ratio, 2030, 0.45).
narrative_ontology:measurement(clim_tr_t2040, climate_response_legitimacy__adaptation_priority, theater_ratio, 2040, 0.5).
narrative_ontology:measurement(clim_tr_t2050, climate_response_legitimacy__adaptation_priority, theater_ratio, 2050, 0.55).

% Extraction over time
narrative_ontology:measurement(clim_be_t2000, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(clim_be_t2010, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2010, 0.72).
narrative_ontology:measurement(clim_be_t2020, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2020, 0.78).
narrative_ontology:measurement(clim_be_t2030, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2030, 0.82).
narrative_ontology:measurement(clim_be_t2040, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2040, 0.85).
narrative_ontology:measurement(clim_be_t2050, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2050, 0.87).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2000, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(clim_su_t2010, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(clim_su_t2020, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2020, 0.75).
narrative_ontology:measurement(clim_su_t2030, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2030, 0.78).
narrative_ontology:measurement(clim_su_t2040, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2040, 0.8).
narrative_ontology:measurement(clim_su_t2050, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2050, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__adaptation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__degrowth_transformation).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('adaptation_priority') of the 'climate_response_legitimacy' kernel. Its structural properties and metrics differ significantly from sibling readings like 'mitigation_priority' and 'degrowth_transformation', which are modeled as separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
