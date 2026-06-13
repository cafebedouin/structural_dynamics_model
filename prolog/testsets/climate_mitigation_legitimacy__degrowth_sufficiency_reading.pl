% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__degrowth_sufficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_mitigation_legitimacy__degrowth_sufficiency_reading
 *   human_readable: Decarbonization via Demand Reduction (Degrowth Sufficiency Reading)
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint instantiates the DEGROWTH SUFFICIENCY READING of
 *   contested climate mitigation legitimacy. The kernel is the question: what
 *   does decarbonization actually require? This reading answers: fundamental
 *   demand reduction, making energy expansion (nuclear, renewable,
 *   growth-dependent) structurally illegitimate. It contrasts with sibling
 *   readings that treat renewables-plus-storage or nuclear-heavy portfolios
 *   as viable decarbonization paths without demand contraction. The
 *   extractiveness trajectory shows the reading gaining enforcement salience
 *   (0.42→0.68 over the interval); suppression intensifies as enforcement
 *   machinery targets energy-intensive sectors; theater_ratio rises in early
 *   period (new framing) then plateaus (established as legitimate baseline).
 *   The constraint is CLAIMED as tangled rope (genuine coordination problem +
 *   asymmetric extraction) while metrics reveal rising extractiveness and
 *   suppression—a reading that offers real legitimacy gains to some
 *   (conservation advocates, low-income households) while imposing
 *   concentrated costs on others (nuclear/renewable industries,
 *   growth-dependent economies).
 *
 * KEY AGENTS:
 *   - energy_conservation_advocates: Agenda-setter and beneficiary. Set the degrowth legitimacy frame; benefit from policy mandates; drive enforcement of demand-reduction targets.
 *   - nuclear_power_industry: Victim seat. Capital-dependent on demand growth; new deployment becomes illegitimate under this reading; faces asset stranding and contraction.
 *   - renewable_energy_developers: Victim seat. Built expansion narratives on electrification growth; degrowth reading makes growth expansion itself the problem; face investor pressure and policy de-prioritization.
 *   - energy_intensive_manufacturing: Victim seat. Steel, cement, chemicals, data centers require abundant energy; degrowth targeting directly constrains their operations; can arbitrage but face rising regulatory costs.
 *   - growth_dependent_economies: Victim seat. Identity-locked to GDP growth; degrowth violates foundational institutional legitimacy; face fiscal and employment crisis if demand reduction enforced.
 *   - low_income_household_constituency: Beneficiary (lower energy bills, energy poverty protection) + payer (job losses in energy sectors, constrained access in developing regions); powerless exit.
 *   - developing_economy_governments: Excluded from legitimacy-setting conversation; would argue degrowth denies them historical growth rights already exercised by wealthy economies.
 *   - climate_science_communities: Observer seat; report carbon budgets and emission pathways; used as evidence by all readings but do not set decarbonization legitimacy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.68).
domain_priors:suppression_score(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.71).
domain_priors:theater_ratio(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resistance, 0.76).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "Decarbonization via Demand Reduction (Degrowth Sufficiency Reading)").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'a32daf2f-a153-42ae-bf61-f5d8290c6cbf').
narrative_ontology:cs_kernel_codification('a32daf2f-a153-42ae-bf61-f5d8290c6cbf', distributed).
narrative_ontology:cs_authority_grounding('a32daf2f-a153-42ae-bf61-f5d8290c6cbf', distributed).
narrative_ontology:cs_reading_relation('a32daf2f-a153-42ae-bf61-f5d8290c6cbf', climate_mitigation_legitimacy__baseload_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('a32daf2f-a153-42ae-bf61-f5d8290c6cbf', climate_mitigation_legitimacy__renewable_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('a32daf2f-a153-42ae-bf61-f5d8290c6cbf', climate_mitigation_legitimacy__portfolio_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('a32daf2f-a153-42ae-bf61-f5d8290c6cbf', foundational, demand_reduction_necessary_for_safe_decarbonization).
narrative_ontology:cs_axiom_status(demand_reduction_necessary_for_safe_decarbonization, holdable).
narrative_ontology:cs_axiom_grounding('a32daf2f-a153-42ae-bf61-f5d8290c6cbf', demand_reduction_necessary_for_safe_decarbonization, empirically_contingent).
narrative_ontology:cs_axiom('a32daf2f-a153-42ae-bf61-f5d8290c6cbf', foundational, energy_system_contraction_privileged_over_expansion).
narrative_ontology:cs_axiom_status(energy_system_contraction_privileged_over_expansion, holdable).
narrative_ontology:cs_axiom_grounding('a32daf2f-a153-42ae-bf61-f5d8290c6cbf', energy_system_contraction_privileged_over_expansion, deontological).
narrative_ontology:cs_reference_frame('a32daf2f-a153-42ae-bf61-f5d8290c6cbf', energy_system_expansion_normalized_as_decarbonization).
narrative_ontology:cs_drift_state('a32daf2f-a153-42ae-bf61-f5d8290c6cbf', contemporary_climate_urgency_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('a32daf2f-a153-42ae-bf61-f5d8290c6cbf', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_conservation_advocates).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, low_income_household_constituency).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_power_industry).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, renewable_energy_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_intensive_manufacturing).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, growth_dependent_economies).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'none', 1).

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
 *   Extractiveness is high and rising (0.42→0.68) because the reading requires transferring energy legitimacy from growth-dependent sectors to conservation, a transfer that imposes concentrated costs on the victim seats (nuclear, renewables, manufacturing, growth-dependent economies) while diffusing benefits (low-income households save on bills; conservation advocates gain legitimacy and policy influence). The reading is CLAIMED as tangled rope because it solves a real coordination problem (collective-action failure in demand reduction) while simultaneously extracting from industries whose growth has been normalized. Suppression is high and rising because the constraint's persistence requires active enforcement against the payer seats' resistance—regulatory bans on energy expansion, consumption taxes, efficiency mandates, and de-legitimization of growth narratives all fall outside what market mechanisms alone would produce. Theater ratio is moderate (0.42 at interval end): the constraint includes real demand-reduction activity and genuine conservation benefits, but a growing share of enforcement energy goes to delegitimizing energy-expansion alternatives (nuclear, renewables marketed as growth-compatible) rather than directly implementing conservation. The temporal series models increasing constraint salience from t=0 (emerging framing) through t=20 (policy adoption begins), then plateauing as the reading becomes institutionalized baseline (t=25-40). Resistance is high (0.76 base) because the payer seats have substantial power and resources: nuclear has decades of technological legitimacy, renewables have green-growth coalition support, manufacturing has employment leverage, and growth-dependent economies control monetary and fiscal policy.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (nuclear, renewables, manufacturing, growth-dependent economies) and the agenda-setter seat (conservation advocates) compute fundamentally different constraint types from structurally identical data. From the conservation advocate seat, the constraint is rope-equivalent coordination that solves demand-reduction collective action; from the nuclear/renewable seats, it is snare-pure—a legitimacy attack on their expansion using climate urgency as cover. From manufacturing and growth-economy seats, it is snare with identity-lock: the constraint doesn't just extract resources, it violates the foundational identity narratives (industrial productivity, economic growth) that make those actors intelligible to themselves. The engine computes each seat's classification from the power/exit/beneficiary-victim structure; the authored metrics (high extraction, high suppression, moderate theater) reflect the constraint's actual operation—it genuinely coordinates demand reduction AND genuinely extracts from growth-dependent industries. The divergence between claim (tangled rope) and metrics (high extraction/suppression profile) is exactly where this reading's structural truth is located.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality: energy_conservation_advocates (d ≈ 0.1-0.2, near full beneficiary—they set the rules and collect legitimacy/policy influence; high power, mobile exit means they bear minimal cost from the constraint they author). Low_income_household_constituency sits at d ≈ 0.4-0.5 (symmetric costline: genuine benefit from lower bills, but trapped exit and job losses in energy sectors impose real costs). Victim directionality: nuclear_power_industry (d ≈ 0.85, near full target—constrained exit via asset lifecycles and regulatory approval, high power but deployed against the constraint, growth narratives that once legitimized them now delegitimize them); renewable_energy_developers (d ≈ 0.80—similar structure, expansion licenses become radioactive, powerful but can't exit the legitimacy system they invested in); energy_intensive_manufacturing (d ≈ 0.75—high power but arbitrage limited, constrained exit via capital sunk in energy-dependent processes); growth_dependent_economies (d ≈ 0.78, identity-locked, institutional power that cannot override identity commitment without remaking itself). Climate science communities (d ≈ 0.5—analytical seat, symmetric, neither collecting nor paying, providing data both used and contested by all other seats). Developing economy governments (d ≈ 0.72—moderate power but constrained by sovereignty limits, trapped exit from the decarbonization legitimacy system set by wealthy-world actors).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is CONTESTED: does decarbonization require demand reduction, or can technology substitution alone achieve safe carbon outcomes? Climate scientists report carbon budgets and timelines but do NOT adjudicate whether demand reduction is necessary (climate data is used by all four sibling readings). Energy modelers disagree structurally: degrowth advocates cite integrated assessment models showing demand reduction necessary; nuclear and renewable advocates cite alternative IAMs showing technology expansion sufficient. Developing economies attest the founding problem has a LEGITIMACY component invisible to climate science: demand reduction is being imposed differentially (wealthy economies had growth, poor economies are denied it), which is a political problem disguised as a climate problem. The constraint avoids mandatrophy (abandonment of founding function) through the classification itself: degrowth sufficiency is CLAIMED as tangled rope precisely because it maintains that it solves a real coordination problem (demand reduction collective action) while extracting from others. If the constraint were CLASSIFIED as snare by the engine (pure extraction with no coordination function), it would enter mandatrophy review—the reading would fail at its own legitimacy claim. The high theater_ratio rise (0.18→0.32 in early period) flags that the constraint's enforcement is increasingly about delegitimizing alternatives (renewables, nuclear) rather than directly implementing conservation—a shift from functional demand reduction toward theatrical exclusion of competing readings. This rise is itself a mandatrophy signal: the reading risks becoming pure attack on competitor readings rather than positive implementation of demand reduction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demand_reduction_feasibility,
    'Can demand reduction of the scale and speed required by degrowth sufficiency reading (30-50% reduction over 20-30 years) be achieved without political economy collapse in growth-dependent economies?',
    'Historical precedent (post-WWII demobilization, post-Soviet contraction, pandemic-driven demand drops); pilot programs in specific regions; political-economy analysis of fiscal/employment restructuring feasibility.',
    'If feasible without collapse, the reading''s founding problem remains live and the constraint''s extractiveness can be justified as necessary; if infeasible, the reading enters mandatrophy territory—its enforcement structure persists but its legitimizing function disappears, transitioning toward snare classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(demand_reduction_feasibility, empirical, 'Whether the constraint''s core demand is politically and economically sustainable.').

omega_variable(
    technology_sufficiency_alternative,
    'Could renewable plus storage deployment at current technological trajectory achieve climate targets without demand reduction (the sibling readings'' claim)?',
    'Comparative integrated assessment models; energy modeling with explicit storage cost curves and grid flexibility assumptions; empirical deployment rates of renewables in high-growth economies.',
    'If technology alone can meet targets, the degrowth reading''s founding problem is false—demand reduction becomes optional policy choice, not climate necessity, and the constraint reclassifies toward snare (pure extraction disguised as climate response). If technology alone is insufficient, the founding problem is vindicated and the constraint''s tangled-rope claim is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_sufficiency_alternative, empirical, 'Whether sibling readings'' technological optimism is empirically grounded or cover story for continued growth.').

omega_variable(
    equity_externality_boundary,
    'Is demand reduction imposed equally across wealthy and developing economies (legitimate decarbonization logic) or differentially, with wealthy economies exempt (extractive legitimacy system)?',
    'Analysis of de-facto carbon budgets allocated to different economic groups; historical comparison to pre-industrial energy consumption baselines; developing economy energy-access trajectories under degrowth constraint.',
    'If equal imposition, the constraint''s extraction from growth-dependent economies is justified cost of climate stabilization; if differential (wealthy get growth, poor get degrowth), the constraint''s legitimacy is fraudulent and the reading enters false-summit territory (natural law framing concealing power asymmetry).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(equity_externality_boundary, empirical, 'Whether the constraint''s burden distribution tracks its own legitimacy criteria or violates them.').

omega_variable(
    sibling_reading_foreclosure_vs_coexistence,
    'Does this reading logically foreclose sibling readings (baseload_necessity, renewable_primacy, portfolio_pragmatism) or do all four readings remain coherent within different institutional and epistemic frameworks?',
    'Formal logical analysis of axiom compatibility; examination of whether high-level decision-makers (policy, investment, science communities) can hold multiple readings simultaneously or whether adoption of this reading creates logical impossibility for others.',
    'If forecloses: this reading is structurally exclusive and the network effects constrain sibling readings into Mandatrophy or false-summit territory. If coexists: multiple readings remain live, and the constraint''s extraction derives from power asymmetry rather than logical necessity, shifting classification pressure toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_vs_coexistence, conceptual, 'Whether this reading is logically exclusive or empirically competitive with siblings.').

omega_variable(
    conservation_movement_capture_risk,
    'Is the conservation movement that sets this constraint''s agenda authentically committed to demand reduction, or using climate urgency to advance unrelated anti-growth ideology?',
    'Analysis of conservation movement''s historical positions on growth; examination of policy proposals (do they maximize efficiency, or do they minimize growth specifically?); comparison of claimed and actual support for energy-intensive poverty alleviation in developing regions.',
    'If authentic climate concern, the constraint is tangled rope with justified extraction; if ideological cover, the extraction becomes snare-pure—the climate problem is real but being used to advance an alternative agenda. The agenda_setter seat''s role would shift from coordinating demand reduction to pure extraction via climate legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conservation_movement_capture_risk, preference, 'Whether the beneficiary''s stated function (climate mitigation) matches their actual goal (anti-growth ideology).').

omega_variable(
    nuclear_renewable_victim_asymmetry,
    'Should nuclear and renewable energy developers be classified as equivalent victims or is there a structural difference in how the constraint affects them?',
    'Comparative analysis of growth-dependence: do renewables markets require demand growth for deployment profitability, or can they operate profitably under demand stability/reduction with storage? Do nuclear dynamics differ?',
    'If equivalent: both enter victim set as authored. If renewable markets can absorb demand reduction better than nuclear, the constraint''s framing as anti-expansion-specific rather than anti-nuclear-specific becomes clear, and the reading''s relative positioning among siblings changes (renewable_primacy reading becomes more compatible).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_renewable_victim_asymmetry, empirical, 'Whether renewable and nuclear energy industries face symmetric extraction under this constraint.').

omega_variable(
    identity_lock_mechanisms_in_growth_economies,
    'What specific institutional features make growth-dependent economies identity-locked to growth commitment, and can those features be reformed without full economic restructuring?',
    'Analysis of fiscal structures (tax base expansion assumptions), employment guarantees, pension obligations, monetary policy frameworks; identification of specific policy levers that could decouple institutional identity from growth while preserving institutional function.',
    'If identity-lock is institutional (tax code, pension law, monetary mandate), it can be reformed independently of growth rates; if identity-lock is existential (capitalism requires growth), it cannot. Reform feasibility determines whether growth-dependent economies are truly trapped (exit_options=identity_locked) or are constrained but mobile.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanisms_in_growth_economies, empirical, 'Whether growth-economy identity-lock is structurally necessary or contingent on specific institutional choices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(clim_tr_t25, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(clim_tr_t30, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(clim_tr_t40, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(clim_be_t25, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(clim_be_t30, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 30, 0.67).
narrative_ontology:measurement(clim_be_t40, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 10, 0.53).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 15, 0.61).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 20, 0.67).
narrative_ontology:measurement(clim_su_t25, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(clim_su_t30, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(clim_su_t40, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.18).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four structurally distinct readings of the kernel 'climate_mitigation_legitimacy'. All four readings instantiate different ε-invariant constraints because they privilege different victim/beneficiary structures and have different enforcement architectures. Degrowth sufficiency reading treats energy expansion (nuclear, renewable) as structurally illegitimate and makes demand reduction binding; baseload_necessity reading treats demand as fixed and makes dispatchable power binding; renewable_primacy reading treats growth-compatible renewables as sufficient and makes nuclear margins unnecessary; portfolio_pragmatism reading treats all technologies as viable and makes cost-optimization binding. The four readings coexist as competing policy frameworks, each internally coherent but mutually exclusive at the level of legitimacy-setting for energy system governance. They influence each other's operating environment (adoption of one reading constrains others' institutional resources) but do not logically foreclose each other—different polities and epistemic communities hold different readings simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_legitimacy__degrowth_sufficiency_reading, institutional, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
