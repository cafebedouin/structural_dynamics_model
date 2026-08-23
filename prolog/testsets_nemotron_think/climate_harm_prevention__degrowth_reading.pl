% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Planned Economic Contraction in Global North for Climate Harm Prevention
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The degrowth reading of climate harm prevention asserts that legitimate
 *   response requires planned economic contraction in the Global North
 *   because mitigation within a growth framework is physically and
 *   politically impossible. This reading identifies Global South populations
 *   and future generations as primary beneficiaries of contraction, while
 *   Global North present consumers and high-emission industries bear the
 *   costs. The constraint rejects growth as a boundary condition for climate
 *   policy. It operates as a tangled rope: a genuine coordination function
 *   (preventing catastrophic harm) combined with asymmetric extraction
 *   (Global North pays, Global South and future generations benefit)
 *   requiring active enforcement (policy implementation, institutional
 *   restructuring). The claimed type is tangled_rope; the metrics reflect
 *   high and rising extraction, substantial suppression, and moderate theater
 *   as performative green-growth rhetoric persists while contraction remains
 *   unimplemented.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__degrowth_reading, 0.72).
domain_priors:suppression_score(climate_harm_prevention__degrowth_reading, 0.78).
domain_priors:theater_ratio(climate_harm_prevention__degrowth_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__degrowth_reading, "Planned Economic Contraction in Global North for Climate Harm Prevention").
narrative_ontology:topic_domain(climate_harm_prevention__degrowth_reading, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__degrowth_reading, '18d78351-6621-4605-8b70-f2a46f6a4588').
narrative_ontology:cs_kernel_codification('18d78351-6621-4605-8b70-f2a46f6a4588', distributed).
narrative_ontology:cs_authority_grounding('18d78351-6621-4605-8b70-f2a46f6a4588', distributed).
narrative_ontology:cs_reading_relation('18d78351-6621-4605-8b70-f2a46f6a4588', climate_harm_prevention__mitigation_priority, forecloses).
narrative_ontology:cs_reading_relation('18d78351-6621-4605-8b70-f2a46f6a4588', climate_harm_prevention__adaptation_priority, coexists_with).
narrative_ontology:cs_axiom('18d78351-6621-4605-8b70-f2a46f6a4588', foundational, growth_incompatible_with_climate_stability).
narrative_ontology:cs_axiom_status(growth_incompatible_with_climate_stability, holdable).
narrative_ontology:cs_axiom_grounding('18d78351-6621-4605-8b70-f2a46f6a4588', growth_incompatible_with_climate_stability, empirically_contingent).
narrative_ontology:cs_axiom('18d78351-6621-4605-8b70-f2a46f6a4588', foundational, global_north_contraction_required_for_justice).
narrative_ontology:cs_axiom_status(global_north_contraction_required_for_justice, holdable).
narrative_ontology:cs_axiom_grounding('18d78351-6621-4605-8b70-f2a46f6a4588', global_north_contraction_required_for_justice, deontological).
narrative_ontology:cs_reference_frame('18d78351-6621-4605-8b70-f2a46f6a4588', growth_based_mitigation_framework).
narrative_ontology:cs_drift_state('18d78351-6621-4605-8b70-f2a46f6a4588', contemporary_climate_policy_debate, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('18d78351-6621-4605-8b70-f2a46f6a4588', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__degrowth_reading, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, global_south_populations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, future_generations).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, global_north_present_consumers).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, global_north_high_emission_industries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, global_north_governments).
narrative_ontology:constraint_vindicates(climate_harm_prevention__degrowth_reading, growth_incompatible_with_planetary_boundaries).
narrative_ontology:constraint_vindicates(climate_harm_prevention__degrowth_reading, intergenerational_justice_requires_contraction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive avoided climate harm (reduced extreme weather, sea-level rise, agricultural disruption) without bearing contraction costs. Their voice in global climate governance is structurally limited; they cannot exit the climate system but can advocate for contraction policies internationally.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_south_populations, beneficiary,
    moderate, generational, constrained, global).

% Inherit a habitable planet if contraction succeeds; inherit catastrophic harm if it fails. No exit from the climate system or from their temporal position. No political voice in present decisions. Their interests are represented only by present-day advocates.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% Bear material costs of contraction: reduced consumption, potential job losses in high-emission sectors, lifestyle changes. Exit options constrained by national borders, economic dependence, and lack of viable low-carbon alternatives at scale. Political resistance organized through consumer advocacy, labor unions, and cultural narratives of prosperity.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_north_present_consumers, payer,
    organized, biographical, constrained, global).

% Bear concentrated costs: asset stranding, regulatory compliance, profit reduction. Exit options include capital flight, regulatory capture, technological greenwashing, and political lobbying. Their institutional power allows them to shape policy design to minimize extraction on their seat.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_north_high_emission_industries, payer,
    institutional, biographical, mobile, global).

% Administer contraction policies (carbon pricing, regulation, public investment). Bear political costs of implementing contraction (electoral backlash, fiscal pressure). Constrained by growth-dependent tax bases, international competitiveness, and domestic political economy. Some governments may benefit from early-mover advantages in low-carbon tech.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_north_governments, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__degrowth_reading, global_north_governments, payer).

% Produce the physical evidence base (IPCC reports, carbon budgets, mitigation pathways) that defines the contraction requirement. Do not bear costs or collect benefits directly. Their authority is epistemic, not political. They frame the coordination problem but do not enforce the solution.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, climate_science_assessment_bodies, observer,
    analytical, generational, analytical, universal).

% Advocate for contraction in Global North (climate justice, common but differentiated responsibilities). Bear costs of adaptation and residual damages. Constrained by dependence on Global North finance, technology, and markets. Their agenda-setting power is collective (G77, AOSIS) but limited by structural power asymmetries.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_south_governments, agenda_setter,
    organized, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_harm_prevention__degrowth_reading, diffuse).
narrative_ontology:fixing_cost_class(climate_harm_prevention__degrowth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents catastrophic climate harm by aligning Global North material throughput with planetary boundaries, solving the collective-action problem of free-riding on the global carbon budget.
% TRANSFER_FUNCTION: Moves material throughput, consumption capacity, and emission rights from Global North present consumers and high-emission industries to Global South populations (avoided harm) and future generations (preserved habitability). The transfer is enforced through planned contraction policies.
% ABSENT_VOICES: Future generations are structurally excluded (cannot speak). Low-income Global North households are underrepresented in policy design (would bear regressive costs without redistribution). Global South marginalized communities (indigenous, rural poor) lack direct representation in UNFCCC negotiations.
% DISAPPEARANCE_RATIONALE: If the contraction constraint vanished, Global North emissions would continue unchecked, Global South would face escalating climate damages, future generations would inherit a degraded planet, and the growth-based mitigation framework would persist — the world would rearrange around uncontrolled warming.
% FOUNDING_PROBLEM: The founding problem is the physical impossibility of reconciling exponential GDP growth in the Global North with the carbon budget for 1.5°C/2°C, given historical emissions, technological deployment lags, and the injustice of denying Global South development space.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by IPCC Working Group III (mitigation pathways requiring demand-side reduction), ecological economics literature (limits to growth), and Global South climate justice movements (climate debt, CBDR). The growth-compatible mitigation pathway is contested by the mitigation_priority reading but lacks empirical validation at required speed/scale.
narrative_ontology:disappearance_verdict(climate_harm_prevention__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__degrowth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__degrowth_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_harm_prevention__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__degrowth_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extraction (0.72) is high because the constraint demands surrender of material throughput and consumption privileges from Global North populations. Suppression (0.78) is higher because the constraint's persistence depends on overcoming entrenched political-economic resistance (fossil capital, growth-dependent institutions, consumer expectations). Theater ratio (0.38) reflects the gap between degrowth discourse and actual policy: green-growth narratives perform coordination while extraction mechanisms (carbon pricing, regulation) remain weak. Accessibility collapse (0.65) is moderate: alternatives (mitigation within growth) are claimed impossible but remain politically live. Resistance (0.71) is high from Global North elites and growth-dependent institutions. The measurement series shows rising extraction and suppression over 30 years as climate urgency increases and growth-compatible mitigation fails to deliver.
 *
 * PERSPECTIVAL GAP:
 *   From the Global North consumer seat, the constraint appears as extreme extraction (snare-like) with high suppression of lifestyle options. From the Global South seat, it appears as necessary coordination (rope-like) with low suppression because the alternative (unmitigated climate harm) is worse. From the future generations seat (analytical), it appears as the only viable coordination. The engine computes these seat divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Global South populations and future generations are structural beneficiaries (d near 0.0): they receive avoided harm without bearing contraction costs. Global North present consumers and high-emission industries are structural targets (d near 1.0): they bear the material costs of contraction. The directionality derivation from beneficiary/victim declarations captures this asymmetry. No directionality overrides are needed; the structural derivation is accurate.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (preventing climate harm through contraction) remains live and contested. Mandatrophy is not resolved: the arrangement (growth-based climate policy) has outlived its function but persists. The degrowth reading identifies this as a false summit: the growth framework is presented as natural/inevitable but serves extractive interests. The mandatrophy analysis prevents mislabeling the growth framework's persistence as coordination when it functions as extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading,
    'This constraint is one reading (degrowth_reading) of the contested kernel climate_harm_prevention. What structural elements do sibling readings (mitigation_priority, adaptation_priority) change?',
    'Compare the three readings'' beneficiary/victim structures, extraction profiles, and coordination claims. The kernel is the shared commitment to preventing climate harm; the readings instantiate different constraints with different ε and seat structures.',
    'If the kernel is treated as a single constraint, ε becomes observer-relative and classification becomes unstable. Decomposition into three linked stories preserves ε-invariance per reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading, conceptual, 'Commitment-system framing: this story is a reading of a kernel, not a standalone constraint.').

omega_variable(
    extraction_measurement_boundary,
    'Where does the extraction of planned contraction fall — on current Global North consumers, on high-emission industries, or on both? How is the incidence distributed across income strata within the Global North?',
    'Empirical incidence analysis of contraction policies (carbon taxes with redistribution, output caps, working-time reductions) across income deciles and sectors.',
    'If extraction falls regressively, the constraint operates as a snare for low-income Global North households. If progressive with redistribution, the coordination function strengthens and extraction profile shifts toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_measurement_boundary, empirical, 'Incidence distribution of contraction costs within Global North.').

omega_variable(
    coordination_necessity_of_contraction,
    'Is planned contraction structurally necessary for climate harm prevention, or are there feasible mitigation pathways within a growth framework that the reading excludes?',
    'Integrated assessment model comparison: scenarios achieving 1.5°C/2°C with vs. without Global North GDP contraction, accounting for technological deployment rates, behavioral change, and political feasibility.',
    'If growth-compatible pathways are physically feasible but politically blocked, the constraint''s coordination claim is contested and extraction may be avoidable. If physically impossible, the coordination function is genuine and the reading''s claim is structurally true.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_necessity_of_contraction, empirical, 'Physical vs. political feasibility of growth-compatible mitigation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__degrowth_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chp_degrowth_tr_t0, climate_harm_prevention__degrowth_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(chp_degrowth_tr_t6, climate_harm_prevention__degrowth_reading, theater_ratio, 6, 0.26).
narrative_ontology:measurement(chp_degrowth_tr_t12, climate_harm_prevention__degrowth_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(chp_degrowth_tr_t18, climate_harm_prevention__degrowth_reading, theater_ratio, 18, 0.33).
narrative_ontology:measurement(chp_degrowth_tr_t24, climate_harm_prevention__degrowth_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement(chp_degrowth_tr_t30, climate_harm_prevention__degrowth_reading, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(chp_degrowth_be_t0, climate_harm_prevention__degrowth_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(chp_degrowth_be_t6, climate_harm_prevention__degrowth_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(chp_degrowth_be_t12, climate_harm_prevention__degrowth_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(chp_degrowth_be_t18, climate_harm_prevention__degrowth_reading, base_extractiveness, 18, 0.64).
narrative_ontology:measurement(chp_degrowth_be_t24, climate_harm_prevention__degrowth_reading, base_extractiveness, 24, 0.69).
narrative_ontology:measurement(chp_degrowth_be_t30, climate_harm_prevention__degrowth_reading, base_extractiveness, 30, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(chp_degrowth_su_t0, climate_harm_prevention__degrowth_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(chp_degrowth_su_t6, climate_harm_prevention__degrowth_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(chp_degrowth_su_t12, climate_harm_prevention__degrowth_reading, suppression_requirement, 12, 0.66).
narrative_ontology:measurement(chp_degrowth_su_t18, climate_harm_prevention__degrowth_reading, suppression_requirement, 18, 0.71).
narrative_ontology:measurement(chp_degrowth_su_t24, climate_harm_prevention__degrowth_reading, suppression_requirement, 24, 0.75).
narrative_ontology:measurement(chp_degrowth_su_t30, climate_harm_prevention__degrowth_reading, suppression_requirement, 30, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__degrowth_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_harm_prevention__degrowth_reading, 0.12).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, climate_harm_prevention__mitigation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, climate_harm_prevention__adaptation_priority).

% DUAL FORMULATION NOTE:
% The climate_harm_prevention kernel decomposes into three readings with distinct ε and seat structures. This reading (degrowth) has high ε (0.72) because it assigns contraction costs to Global North. Mitigation_priority has lower ε (growth-compatible tech transition). Adaptation_priority has different victim/beneficiary structure (near-term vulnerable populations vs. future generations). All three linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
