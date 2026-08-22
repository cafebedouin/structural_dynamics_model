% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__baseload_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__baseload_necessity_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: climate_mitigation_legitimacy__baseload_necessity_reading
 *   human_readable: Baseload Necessity Reading of Climate Mitigation Legitimacy
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint embodies one reading of a contested kernel about how to
 *   legitimately achieve climate mitigation: the reading asserts that
 *   reliable decarbonization structurally requires dispatchable baseload
 *   power (historically hydro/fossil, now nuclear) and that variable
 *   renewables cannot substitute at scale. The reading operationalizes this
 *   claim through technical authority: modeling, engineering practice, policy
 *   frameworks, and capital flows all treat baseload necessity as settled
 *   fact, not as a contestable empirical claim. Nuclear utilities, equipment
 *   manufacturers, and institutional investors benefit from the capital
 *   concentration and regulatory preference the reading justifies; renewable
 *   developers, distributed-generation advocates, and electricity consumers
 *   bear the costs. The constraint is CLAIMED as tangled_rope because it
 *   coordinates a real problem (grid stability) while extracting concentrated
 *   benefit from that coordination function; the reading's enforcement rests
 *   on suppressing alternative technical framings (storage adequacy, demand
 *   flexibility, distributed architectures) rather than on voluntary
 *   coordination.
 *
 * KEY AGENTS:
 *   - nuclear_utilities: Institutional beneficiary and agenda-setter; controls the technical framing and policy implementation; high power, arbitrage-level exit.
 *   - renewable_energy_developers: Organized payer; constrained by the reading's classification of their product as intermittent and inadequate.
 *   - electricity_consumers: Powerless victims; bear long-lived asset cost recovery through rates; trapped exit.
 *   - distributed_generation_advocates: Identity-locked payers; their professional identity is defined against the baseload model; costly exit from the constraint frame.
 *   - engineering_consultants: Institutional beneficiary; provide technical authority that operationalizes the reading.
 *   - grid_operators: Excluded; have operational experience managing mixed renewable systems but are not in the legitimacy-framing conversation.
 *   - climate_justice_advocates: Excluded; raise concerns about uranium mining, waste, and unequal distribution of risk but are outside the technical authority circle.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__baseload_necessity_reading, 0.68).
domain_priors:suppression_score(climate_mitigation_legitimacy__baseload_necessity_reading, 0.52).
domain_priors:theater_ratio(climate_mitigation_legitimacy__baseload_necessity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__baseload_necessity_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__baseload_necessity_reading, "Baseload Necessity Reading of Climate Mitigation Legitimacy").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__baseload_necessity_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__baseload_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__baseload_necessity_reading, '0a35e64b-71cb-49f2-ba60-1a94548c1cdb').
narrative_ontology:cs_kernel_codification('0a35e64b-71cb-49f2-ba60-1a94548c1cdb', fixed_text).
narrative_ontology:cs_authority_grounding('0a35e64b-71cb-49f2-ba60-1a94548c1cdb', extraction).
narrative_ontology:cs_interpretation_layer_present('0a35e64b-71cb-49f2-ba60-1a94548c1cdb').
narrative_ontology:cs_reading_relation('0a35e64b-71cb-49f2-ba60-1a94548c1cdb', climate_mitigation_legitimacy__renewable_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('0a35e64b-71cb-49f2-ba60-1a94548c1cdb', climate_mitigation_legitimacy__portfolio_pragmatism_reading, influences).
narrative_ontology:cs_reading_relation('0a35e64b-71cb-49f2-ba60-1a94548c1cdb', climate_mitigation_legitimacy__degrowth_sufficiency_reading, coexists_with).
narrative_ontology:cs_axiom('0a35e64b-71cb-49f2-ba60-1a94548c1cdb', foundational, baseload_dispatchability_necessary_for_grid_reliability).
narrative_ontology:cs_axiom_status(baseload_dispatchability_necessary_for_grid_reliability, holdable).
narrative_ontology:cs_axiom_grounding('0a35e64b-71cb-49f2-ba60-1a94548c1cdb', baseload_dispatchability_necessary_for_grid_reliability, empirically_contingent).
narrative_ontology:cs_axiom('0a35e64b-71cb-49f2-ba60-1a94548c1cdb', foundational, variable_renewables_inadequate_at_scale_without_baseload).
narrative_ontology:cs_axiom_status(variable_renewables_inadequate_at_scale_without_baseload, holdable).
narrative_ontology:cs_axiom_grounding('0a35e64b-71cb-49f2-ba60-1a94548c1cdb', variable_renewables_inadequate_at_scale_without_baseload, empirically_contingent).
narrative_ontology:cs_reference_frame('0a35e64b-71cb-49f2-ba60-1a94548c1cdb', centralized_dispatchable_generation_architecture).
narrative_ontology:cs_drift_state('0a35e64b-71cb-49f2-ba60-1a94548c1cdb', contemporary_storage_advancement_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0a35e64b-71cb-49f2-ba60-1a94548c1cdb', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_utilities).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, heavy_equipment_manufacturers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, engineering_consultants).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, institutional_investors).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_energy_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, distributed_generation_advocates).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, electricity_consumers_bearing_capital_cost).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__baseload_necessity_reading, thermodynamic_necessity_of_dispatchability).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__baseload_necessity_reading, storage_cost_scaling_limits).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__baseload_necessity_reading, grid_stability_constraints).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate existing nuclear plants and champion new reactor builds as essential to decarbonization targets. Directly benefit from capital cost recovery mechanisms, government-backed financing, and regulatory preferences for dispatchable generation. Set the technical framing that baseload is non-negotiable and design policy to favor long-lived assets.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_utilities, beneficiary,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_utilities, agenda_setter).

% Compete for capital and policy support in a framework that classifies renewable generation as intermittent and inadequate without baseload. They argue storage technology and demand flexibility can solve the dispatchability challenge, but the burden of proof under this reading falls entirely on them — baseload necessity is treated as settled, not contested.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_energy_developers, payer,
    organized, biographical, constrained, national).

% Push for community solar, microgrids, and local storage solutions. Structurally subordinated by this reading, which treats distributed approaches as auxiliary to central baseload. Their professional identity and advocacy narrative is built on challenging the centralized baseload model; identity lock makes exit from this constraint frame costly.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, distributed_generation_advocates, payer,
    moderate, biographical, identity_locked, local).

% Pay for long-lived nuclear infrastructure through rate recovery mechanisms and subsidies, often before plants begin operation. Bear construction cost overruns and extended outages. Lack organized power to shape the baseload decision; their only exit is off-grid (costly, identity-constrained).
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, electricity_consumers_bearing_capital_cost, payer,
    powerless, biographical, trapped, national).

% Supply reactor vessels, turbines, and heavy machinery for nuclear construction. Benefit from long project cycles and capital-intensive plant builds. Capture extraction margins as suppliers to utilities operating under the baseload-necessity framing.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, heavy_equipment_manufacturers, beneficiary,
    institutional, generational, arbitrage, global).

% Provide technical analysis, modeling, and design services that validate baseload necessity and justify large capital projects. Beneficiary of the technical authority role; their models and expert framings operationalize the constraint.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, engineering_consultants, beneficiary,
    powerful, biographical, arbitrage, global).

% Finance nuclear projects under regulatory frameworks that guarantee cost recovery and provide risk-shifting mechanisms. Benefit from stable, long-term assets backed by government support. Capital concentration in baseload plays.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, institutional_investors, beneficiary,
    institutional, generational, arbitrage, global).

% Provide empirical data on carbon budgets, temperature targets, and mitigation pathways. Their role is analytical, but this reading instrumentalizes their findings to justify a particular technical pathway, not simply to report the physical constraints.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, climate_scientists, observer,
    analytical, civilizational, analytical, global).

% Manage real-time electricity flows and are increasingly excluded from design discussions despite operating experience with mixed renewable + storage systems. Their lived operational knowledge about dispatchability is subordinated to theoretical models built into the baseload-necessity framing.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, grid_operators, excluded,
    organized, biographical, constrained, regional).

% Raise concerns about radioactive waste, uranium mining impacts on indigenous lands, and unequal distribution of nuclear risk and benefit. Their voice is structurally excluded from the technical legitimacy conversation by this reading, which frames the issue as thermodynamic necessity, not political choice.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, climate_justice_advocates, excluded,
    moderate, biographical, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_utilities).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__baseload_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the real coordination problem of grid stability: the electric system must match generation to demand in real time across multiple timescales (seconds to seasons). Baseload plants provide firm capacity that does not depend on weather or time-of-day variability, which this reading treats as indispensable to reliable electricity service.
% TRANSFER_FUNCTION: Moves capital investment and cost recovery obligations from investors and equipment suppliers to electricity consumers via rate mechanisms; shifts long-term financial risk from private capital to regulated utilities; concentrates technical decision-making authority in large utilities and their engineering consultants; redirects decarbonization policy funding toward nuclear projects instead of distributed renewables or demand reduction.
% ABSENT_VOICES: Grid operators with experience running high-renewable systems; distributed generation advocates; climate justice advocates concerned with uranium mining and waste impacts; communities hosting nuclear plants; renewable energy developers offering alternative technical solutions; voices from jurisdictions that have achieved high decarbonization without baseload expansion (Denmark, Costa Rica).
% DISAPPEARANCE_RATIONALE: If this constraint and its enforcement disappeared, decarbonization policy would immediately shift toward technology-neutral evaluation: alternatives like renewables plus storage, demand flexibility, and distributed systems would be funded and deployed at scale without the baseload-necessity justification. Capital flows would redirect, utilities would face genuine competition from alternative architectures, and grid design would adapt to empirical performance rather than theoretical necessity.
% FOUNDING_PROBLEM: Early-2000s concerns that renewable variable output would make grid frequency unstable and that no existing storage technology could economically buffer large-scale wind/solar integration at continental scale. Baseload generation (hydro, nuclear, fossil fuels) provided firm capacity; grid operators relied on it to maintain stability.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear industry and utility operators attest the founding problem is still live and worsening with higher renewable penetration. Grid operators and storage technology companies attest the problem is substantially solved by advances in battery storage, demand-side management, and forecasting; academic energy modeling shows multiple pathways to full decarbonization without baseload expansion (MIT, Stanford, NREL reports from outside the nuclear beneficiary set). The founding problem's persistence is now contested.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__baseload_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__baseload_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__baseload_necessity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__baseload_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__baseload_necessity_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__baseload_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__baseload_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__baseload_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68) because the reading channels capital investment and policy authority toward nuclear players, but does not extract through pure coercion—it maintains coordination function (grid stability) as cover. Suppression is moderate (0.52) because the reading's main mechanism is not violent exclusion but epistemic suppression: alternative technical framings (storage, demand flexibility, distributed systems) are not banned, they are systematically classified as inadequate, unproven, or auxiliary. Theater is moderate (0.41) because the technical legitimacy function is real—grid stability is a genuine problem and baseload plants do provide firm capacity—but an increasing share of the constraint's enforcement is now theatrical: maintaining the narrative that baseload is necessary despite mounting empirical evidence that renewables plus storage can achieve high reliability. The measurement series shows extraction accumulating (0.54 to 0.68) and theater ratio rising (0.28 to 0.41) over 30 years, consistent with a constraint whose primary function has partially atrophied but whose enforcement infrastructure has hardened. Suppression requirement rises gradually (0.38 to 0.52) reflecting the mounting pressure from alternative technical evidence; the reading must increasingly suppress competing framings to maintain its legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (nuclear utilities) and beneficiaries (equipment manufacturers, investors, consultants) experience this constraint as genuine coordination and necessary technical governance—they credibly believe baseload is required and see their beneficiary position as earning differential return from solving a hard problem. The payer seats (renewable developers, consumers, distributed advocates) experience it as enforced extraction disguised as technical necessity—they observe that policy and capital flow are controlled by the baseload faction, that alternative technical solutions are systematically underfunded despite evidence of adequacy, and that their exclusion from technical conversations is not based on physics but on institutional power. Grid operators, if included in the technical conversation, would report from lived experience that their operational challenges have shifted from managing baseload inflexibility to integrating variable renewables—their perspective threatens the reading's core claim and so they are institutionally excluded. The engine computes these divergences from the declared power atoms, exit options, and beneficiary/victim structure; they are seats experiencing the same constraint differently, not competing constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear utilities sit at d ≈ 0.1 (full beneficiary): institutional power, arbitrage-level exit options (can exit by stopping nuclear projects but substitute with other profitable infrastructure), and direct capital flow. Equipment manufacturers and consultants sit at d ≈ 0.15 (beneficiary): they benefit from capital concentration but depend on the utilities' continued investment decisions. Institutional investors sit at d ≈ 0.2 (beneficiary): capital concentration serves them, but their exit options are high (they can invest elsewhere). Renewable developers sit at d ≈ 0.7 (payer): organized power but constrained exit (their sector exists only within the energy system that this reading shapes), and subordinated technical status. Distributed advocates sit at d ≈ 0.8 (target): moderate power but identity-locked exit (they cannot pivot to a different advocacy model without dissolving their professional identity), making them structurally dependent on changing the reading itself. Electricity consumers sit at d ≈ 0.9 (full target): powerless, trapped exit (off-grid is identity-constrained or economically impossible), and bear cost recovery. The derivation chains are: beneficiaries get low d from power + exit options; victims get high d from constrained/trapped exit + subordinated status in the reading's framing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (baseload necessity for grid stability) is legitimately live—grid operators still report challenges managing variable generation at scale. However, the founding problem's SOLUTION HAS CHANGED: storage technology, demand-side management, and forecasting have matured such that alternatives to baseload expansion now demonstrably solve grid stability problems. The reading persists not because the founding problem is still alive, but because the reading has become an extraction mechanism. The mandatrophy test is: if the founding problem were solved via alternative means (renewables plus storage achieving reliable operation), would the reading still persist as policy and capital direction? The answer is yes—the reading has become decoupled from the problem it was built to solve. This is the classic mandatrophy signature: the constraint persists because powerful actors benefit from it, not because it solves its stated problem anymore. Classifying it as tangled_rope rather than piton reflects that the coordination function (grid stability) is real and still matters; but the theater_ratio's rise (0.28 to 0.41) and suppression_requirement's climb (0.38 to 0.52) are mandatrophy symptoms: more enforcement machinery must be deployed to defend a claim that would otherwise collapse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dispatchability_requirement_empirical,
    'What is the actual grid-reliability requirement for firm dispatchable generation at a given renewable penetration, and can it be met by the combination of renewables + storage + demand flexibility at economic cost?',
    'Real-world grid operations data from jurisdictions with >80% renewable penetration (Denmark, Costa Rica, South Australia, parts of California); operational simulation studies with real load and weather data; cost comparison of storage/demand-flexibility solutions vs. new nuclear builds.',
    'If firm capacity requirements can be reliably met without baseload expansion, the reading''s core justification collapses and the constraint reclassifies from tangled_rope (coordination + extraction) to snare (pure extraction). If firm requirements exceed what storage/flexibility can provide, the reading''s technical claim is validated and the constraint''s coordination function strengthens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dispatchability_requirement_empirical, empirical, 'Whether the grid''s dispatchability requirement necessitates baseload generation or can be solved by alternative means.').

omega_variable(
    capital_cost_trajectory_ambiguity,
    'Will nuclear capital costs decline (via learning-by-doing and manufacturing scale) or continue rising, relative to battery storage and renewable cost trajectories?',
    'Historical cost data from completed nuclear projects (2010–2030); battery manufacturing cost curves from multiple suppliers; modeling studies that account for continued technology improvement in both sectors.',
    'If nuclear costs decline relative to alternatives, the reading''s economic defense strengthens (baseload is not just necessary but economically sensible). If nuclear costs continue rising while storage/renewable costs fall, the reading becomes economically indefensible despite its technical claims, and policy will face pressure to reclassify the constraint as extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_cost_trajectory_ambiguity, empirical, 'Whether nuclear remains economically competitive for providing dispatchable capacity.').

omega_variable(
    reading_foreclosure_ambiguity,
    'Does baseload_necessity_reading logically foreclose renewable_primacy_reading within any single technical framework, or do the two readings differ on contingent empirical claims that could both be true?',
    'Formal analysis of the core premises: if baseload_necessity rests on ''renewables cannot achieve adequate dispatchability'' and renewable_primacy rests on ''renewables + storage can achieve adequate dispatchability,'' the disagreement is on an empirical question (how much storage is needed, can it be afforded) not a logical contradiction. Two readings coexist_with each other. If baseload_necessity rests on a deeper claim (e.g., ''thermodynamic law prevents variable generation from being reliable'') that would logically contradict renewable_primacy''s premise, foreclosure applies.',
    'If the readings coexist_with, the kernel admits multiple live interpretations and policy can shift between them as empirical evidence accumulates. If foreclosure applies, one reading will eliminate the other and the kernel will stabilize on a single legitimate interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_ambiguity, conceptual, 'Whether baseload_necessity and renewable_primacy readings can coexist or whether one logically forecloses the other.').

omega_variable(
    suppression_internalization_ambiguity,
    'Is the suppression of alternative technical framings (grid_operators'' operational experience, renewable developers'' proposed solutions) structural (policy rules, funding flows) or internalized (the alternative framings are intellectually discredited, not just officially excluded)?',
    'Post-policy-shift observation: if decarbonization policy shifts to include alternative technical solutions and the suppressed actors immediately propose implemented systems without having to rebuild their technical legitimacy from scratch, suppression was primarily structural. If the actors require years to recover intellectual credibility despite new policy opening, suppression was substantially internalized.',
    'If suppression is primarily structural, the constraint can be reclassified relatively quickly through policy change. If suppression is substantially internalized, actors must overcome cognitive lock-in and the constraint''s persistence will outlast policy changes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_ambiguity, empirical, 'Whether the suppression of alternative technical framings is structural or internalized in professional communities.').

omega_variable(
    kernel_reading_contest_stability,
    'Will the baseload_necessity_reading remain one of the kernel''s live readings, or will it be foreclosed by empirical evidence or political displacement?',
    'Longitudinal tracking of policy discourse, funding allocation, technical publications, and grid operations over 10–30 years. Observe whether alternatives prove adequate, whether nuclear deployment accelerates or stalls, and whether the reading is invoked as legitimacy for new projects or becomes a historical artifact.',
    'If the reading remains live, the kernel climate_mitigation_legitimacy continues to admit multiple interpretations and policy will oscillate between readings. If the reading is foreclosed, the kernel stabilizes on alternatives (renewable_primacy or portfolio_pragmatism) and this constraint story becomes a historical record of a defeated interpretation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_stability, conceptual, 'Whether baseload_necessity_reading will persist as a live reading of the climate mitigation kernel or be displaced.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__baseload_necessity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(clim_tr_t5, observed).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(clim_tr_t10, observed).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(clim_tr_t15, observed).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(clim_tr_t20, observed).
narrative_ontology:measurement(clim_tr_t25, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(clim_tr_t25, observed).
narrative_ontology:measurement(clim_tr_t30, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(clim_tr_t30, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 0, 0.54).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 5, 0.59).
narrative_ontology:measurement_basis(clim_be_t5, observed).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(clim_be_t10, observed).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(clim_be_t15, observed).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(clim_be_t20, observed).
narrative_ontology:measurement(clim_be_t25, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(clim_be_t25, observed).
narrative_ontology:measurement(clim_be_t30, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(clim_be_t30, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement_basis(clim_su_t5, observed).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement_basis(clim_su_t10, observed).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 15, 0.49).
narrative_ontology:measurement_basis(clim_su_t15, observed).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement_basis(clim_su_t20, observed).
narrative_ontology:measurement(clim_su_t25, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 25, 0.52).
narrative_ontology:measurement_basis(clim_su_t25, observed).
narrative_ontology:measurement(clim_su_t30, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(clim_su_t30, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__baseload_necessity_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_legitimacy__baseload_necessity_reading, 0.18).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy__renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel climate_mitigation_legitimacy. All four readings (baseload_necessity, renewable_primacy, portfolio_pragmatism, degrowth_sufficiency) share the same referent (how to achieve net-zero greenhouse gas emissions legitimately) but differ in which energy technologies are treated as necessary, which as auxiliary, and which costs are externalized. The baseload_necessity reading treats nuclear as necessary infrastructure and renewable-only pathways as inadequate; this structural difference in the beneficiary/victim set produces different ε values and different per-seat classifications from the same high-level decarbonization commitment. Network edges flow from more empirically settled upstream readings to more contested downstream readings: baseload_necessity influences all siblings because policy treating baseload as necessary starves capital from alternatives; renewable_primacy forecloses nothing but coexists_with baseload_necessity (both remain live political positions); portfolio_pragmatism influences baseload_necessity by creating space for integration (if pragmatism succeeds, baseload necessity is superseded); degrowth_sufficiency forecloses baseload_necessity if demand reduction proves adequate (no large-scale generation needed).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_legitimacy__baseload_necessity_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
