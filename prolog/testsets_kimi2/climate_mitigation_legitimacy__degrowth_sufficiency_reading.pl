% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__degrowth_sufficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__degrowth_sufficiency_reading, []).

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
 *   constraint_id: climate_mitigation_legitimacy__degrowth_sufficiency_reading
 *   human_readable: Degrowth/Sufficiency Climate Mitigation Legitimacy
 *   domain: energy policy / climate mitigation / technology governance
 *
 * SUMMARY:
 *   This constraint instantiates the degrowth/sufficiency reading of the
 *   contested climate_mitigation_legitimacy kernel. Under this reading,
 *   legitimate decarbonization is restructured around demand reduction and
 *   energy sufficiency, which renders large-scale generation expansion
 *   unnecessary. Nuclear and renewable energy sectors, alongside
 *   energy-intensive industries, bear the costs of this arrangement through
 *   constrained capital access and delegitimized growth. The sufficiency
 *   movement gains policy standing and institutional recognition. This is one
 *   of four structurally distinct readings of the kernel; siblings include
 *   baseload necessity, renewable primacy, and portfolio pragmatism. The
 *   constraint is claimed as tangled rope because it coordinates genuine
 *   climate mitigation action (avoiding overbuild and material extraction)
 *   while asymmetrically extracting from growth-dependent supply sectors.
 *
 * KEY AGENTS:
 *   - sufficiency_movement: Primary beneficiary (organized/mobile) â gains legitimacy and policy access
 *   - climate_policy_institutions: Agenda-setter (institutional/constrained) â administers planning frameworks
 *   - nuclear_industry: Primary payer (powerful/constrained) â bears exclusion from capital and policy
 *   - renewable_developers: Primary payer (powerful/constrained) â faces delegitimization despite low-carbon credentials
 *   - energy_intensive_industries: Secondary payer (powerful/constrained) â faces demand constraints
 *   - global_south_advocates: Excluded voice (organized/trapped) â marginalized in demand-reduction frameworks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.68).
domain_priors:suppression_score(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.62).
domain_priors:theater_ratio(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "Degrowth/Sufficiency Climate Mitigation Legitimacy").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "energy policy / climate mitigation / technology governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'c75f34bc-8edf-499c-ae04-370f90958971').
narrative_ontology:cs_kernel_codification('c75f34bc-8edf-499c-ae04-370f90958971', distributed).
narrative_ontology:cs_authority_grounding('c75f34bc-8edf-499c-ae04-370f90958971', distributed).
narrative_ontology:cs_reading_relation('c75f34bc-8edf-499c-ae04-370f90958971', climate_mitigation_legitimacy__baseload_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('c75f34bc-8edf-499c-ae04-370f90958971', climate_mitigation_legitimacy__renewable_primacy_reading, influences).
narrative_ontology:cs_reading_relation('c75f34bc-8edf-499c-ae04-370f90958971', climate_mitigation_legitimacy__portfolio_pragmatism_reading, influences).
narrative_ontology:cs_axiom('c75f34bc-8edf-499c-ae04-370f90958971', foundational, sufficiency_as_precondition).
narrative_ontology:cs_axiom_status(sufficiency_as_precondition, holdable).
narrative_ontology:cs_axiom_grounding('c75f34bc-8edf-499c-ae04-370f90958971', sufficiency_as_precondition, deontological).
narrative_ontology:cs_axiom('c75f34bc-8edf-499c-ae04-370f90958971', foundational, growth_incompatible_with_mitigation).
narrative_ontology:cs_axiom_status(growth_incompatible_with_mitigation, holdable).
narrative_ontology:cs_axiom_grounding('c75f34bc-8edf-499c-ae04-370f90958971', growth_incompatible_with_mitigation, empirically_contingent).
narrative_ontology:cs_reference_frame('c75f34bc-8edf-499c-ae04-370f90958971', planetary_boundaries_sufficiency).
narrative_ontology:cs_drift_state('c75f34bc-8edf-499c-ae04-370f90958971', contemporary_climate_policy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c75f34bc-8edf-499c-ae04-370f90958971', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, sufficiency_movement).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_industry).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, renewable_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_intensive_industries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for demand reduction and energy sufficiency as the core of climate strategy. Receives institutional recognition, research funding, and policy access when climate frameworks adopt sufficiency language. Their research agendas and campaign goals are validated by the constraint's dominance.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, sufficiency_movement, beneficiary,
    organized, biographical, mobile, global).

% Develop and administer climate mitigation scenarios and energy plans that prioritize demand reduction, efficiency, and sufficiency over supply expansion. They set planning assumptions that determine which energy projects receive permits and financing.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_policy_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Develops and deploys large-scale nuclear generation projects. Faces planning restrictions, financing exclusion, and declining policy support under frameworks that treat additional generation capacity as unnecessary. Their capital is locked in long-term projects with shrinking markets.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_industry, payer,
    powerful, generational, constrained, global).

% Develops wind, solar, and storage projects at scale. Despite providing low-carbon energy, they face reduced policy support and capital access when demand reduction is treated as sufficient for decarbonization. Their growth-dependent business model is delegitimized.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, renewable_developers, payer,
    powerful, generational, constrained, global).

% Manufactures materials, chemicals, and goods requiring high energy inputs. Faces demand constraints, efficiency mandates, and potential production limits under sufficiency-oriented policy. Their expansion plans are constrained by planning frameworks that treat energy demand growth as illegitimate.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_intensive_industries, payer,
    powerful, biographical, constrained, national).

% Represents populations in developing nations seeking expanded energy access for development. They are underrepresented in climate scenario planning that assumes industrialized-country demand patterns can be universalized. They would argue that demand-reduction frameworks lock in energy poverty.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, global_south_advocates, excluded,
    organized, generational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates climate mitigation action around demand reduction and sufficiency, avoiding the collective-action problems of material extraction, land-use conflict, and capital coordination that accompany large-scale supply-side buildout.
% TRANSFER_FUNCTION: Moves policy legitimacy and capital access away from growth-dependent energy supply sectors toward demand-side management, efficiency, and sufficiency frameworks.
% ABSENT_VOICES: Global South development advocates and energy-access NGOs are structurally underrepresented in sufficiency-framed scenario planning; they would argue that demand-reduction frameworks impose asymmetric burdens on poorer populations and constrain legitimate development needs.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, energy policy would shift back toward supply-side competition between nuclear, renewables, and portfolio mixes; capital would reallocate to generation infrastructure; demand-side frameworks would lose their privileged institutional position and planning authority.
% FOUNDING_PROBLEM: Industrialized energy systems generated runaway emissions through perpetual growth in demand and supply; climate mitigation was defaulting to techno-supply fixes that replicated growth logic without addressing underlying energy demand.
% FOUNDING_PROBLEM_CORROBORATION: Ecological economists and sufficiency researchers attest the problem remains live. Energy systems analysts and development economists outside the sufficiency coalition argue the problem has shifted: low-carbon supply options now exist, and the arrangement suppresses viable mitigation pathways rather than solving the original growth problem.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__degrowth_sufficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__degrowth_sufficiency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__degrowth_sufficiency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__degrowth_sufficiency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__degrowth_sufficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint systematically redirects capital and legitimacy away from supply-side buildout toward demand reduction, suppressing viable low-carbon generation pathways. Suppression (0.62) reflects the active enforcement through planning restrictions, scenario frameworks, and financing exclusions that prevent nuclear and renewable deployment. Theater ratio (0.32) is moderate: the demand-reduction coordination is functionally real (efficiency measures, retrofit programs) but an increasing share of the activity performs ideological boundary-policing against supply options. Accessibility collapse (0.58) captures the narrowing of legitimate climate discourse to sufficiency frames. Resistance (0.75) is high because the suppressed industries and development advocates actively contest the constraint.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats experience this constraint as necessary coordination to prevent ecologically destructive overbuild and to align climate action with planetary boundaries. The payer seats experience it as an artificial suppression of viable, low-carbon supply options that denies them capital and futures. The excluded Global South seat experiences it as a North-centric frame that constrains legitimate development. The engine computes these divergent classifications from the structural asymmetry in exit options (mobile versus constrained/trapped) and the beneficiary/victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   The sufficiency_movement is declared beneficiary: it collects policy legitimacy and institutional access, producing a low directionality toward subsidy. Nuclear and renewable developers are declared victims: they bear the denied capital and deployment opportunity, producing high directionality toward extraction. Energy-intensive industries are similarly victims of demand constraints. Climate policy institutions sit near symmetric: they administer the constraint without clearly collecting or paying. Global South advocates are excluded rather than coordinated.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mislabeling because its coordination function is genuine: demand reduction does solve collective-action problems of material extraction, land conflict, and capital coordination that accompany supply-side buildout. However, it is not a pure rope because the same structure that coordinates mitigation also asymmetrically suppresses legitimate supply alternatives (nuclear and renewables) and constrains industrial activity. The victim set is not incidental: the constraint's operation requires treating growth-dependent energy as illegitimate. Active enforcement (planning and financing exclusion) is required to maintain this suppression, distinguishing it from scaffold or rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sufficiency_without_supply_feasibility,
    'Can deep decarbonization be achieved purely through demand reduction and sufficiency without any large-scale low-carbon generation expansion?',
    'Integrated assessment model runs comparing high-demand-reduction-no-supply-build scenarios against scenarios with permitted supply expansion; empirical tracking of nations pursuing sufficiency-primary strategies.',
    'If sufficiency alone is insufficient, the constraint''s coordination function is compromised and extraction (suppression of viable supply) dominates; if sufficient, the tangled rope classification tilts toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sufficiency_without_supply_feasibility, empirical, 'Whether demand reduction alone can achieve decarbonization without supply expansion.').

omega_variable(
    growth_paradigm_homogenization,
    'Does the ''growth-dependent'' victim label correctly homogenize nuclear and renewable deployment, or does it obscure material differences in resource intensity, land use, and scalability?',
    'Comparative lifecycle analysis and material flow accounting for nuclear versus renewable buildout at equivalent energy service levels.',
    'If the categories are materially distinct, the constraint''s victim structure is overbroad and the extraction is more arbitrary than structurally necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_paradigm_homogenization, conceptual, 'Whether nuclear and renewables are correctly grouped as growth-dependent victims.').

omega_variable(
    global_south_asymmetric_burden,
    'Does the demand-reduction framing impose structurally asymmetric burdens on Global South development compared to industrialized nations?',
    'Comparative energy demand projections and climate policy burden-sharing analyses across income levels.',
    'If asymmetric, the constraint''s coordination function serves industrialized-country interests while extracting from Global South energy access; directionality shifts for global_south_advocates from excluded to payer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_south_asymmetric_burden, empirical, 'Whether demand-reduction frameworks asymmetrically burden Global South development.').

omega_variable(
    kernel_reading_contest,
    'This constraint is the degrowth_sufficiency_reading of the climate_mitigation_legitimacy kernel; sibling readings instantiate baseload necessity, renewable primacy, and portfolio pragmatism. Does the kernel itself admit a technology-neutral reconciliation, or is it structurally committed to partisan readings?',
    'Analysis of whether the kernel''s natural language (''what decarbonization requires'') can be disambiguated into non-competing sub-claims or whether the readings are irreducibly adversarial.',
    'If irreducibly adversarial, the kernel should remain decomposed; if reconcilable, a synthetic constraint could replace the family.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the climate_mitigation_legitimacy kernel admits reconciliation or requires decomposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(climate_degrowth_tr_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(climate_degrowth_tr_t5, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(climate_degrowth_tr_t10, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(climate_degrowth_tr_t15, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(climate_degrowth_tr_t20, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(climate_degrowth_tr_t25, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 25, 0.3).
narrative_ontology:measurement(climate_degrowth_tr_t30, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 30, 0.32).

% Extraction over time
narrative_ontology:measurement(climate_degrowth_be_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(climate_degrowth_be_t5, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(climate_degrowth_be_t10, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(climate_degrowth_be_t15, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(climate_degrowth_be_t20, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(climate_degrowth_be_t25, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 25, 0.64).
narrative_ontology:measurement(climate_degrowth_be_t30, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(climate_degrowth_su_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(climate_degrowth_su_t5, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 5, 0.32).
narrative_ontology:measurement(climate_degrowth_su_t10, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(climate_degrowth_su_t15, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement(climate_degrowth_su_t20, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(climate_degrowth_su_t25, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 25, 0.6).
narrative_ontology:measurement(climate_degrowth_su_t30, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, portfolio_pragmatism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the climate_mitigation_legitimacy kernel, decomposed per the epsilon-invariance principle because the kernel's natural-language label conflates four structurally distinct claims about what decarbonization requires. This reading instantiates the degrowth/sufficiency position; siblings instantiate baseload necessity, renewable primacy, and portfolio pragmatism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
