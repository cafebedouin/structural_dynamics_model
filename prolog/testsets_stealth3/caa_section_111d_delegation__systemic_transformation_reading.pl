% ============================================================================
% CONSTRAINT STORY: caa_section_111d_delegation__systemic_transformation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_caa_section_111d_delegation__systemic_transformation_reading, []).

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
 *   constraint_id: caa_section_111d_delegation__systemic_transformation_reading
 *   human_readable: Clean Air Act Section 111(d) Systemic Transformation Reading (Grid-Wide Best System)
 *   domain: administrative law/environmental regulation/constitutional interpretation
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel: the
 *   systemic_transformation_reading of Clean Air Act Section 111(d)'s 'best
 *   system of emission reduction,' under which EPA may set performance
 *   standards that reach beyond individual facility fences into grid-wide
 *   generation composition — renewable substitution and early coal retirement
 *   — implemented through state-administered pathway plans. Under this
 *   reading the arrangement coordinates a genuine collective-action problem
 *   (grid decarbonization) while imposing concentrated costs on coal assets,
 *   coal-dependent regions, and fossil-locked state budgets. KEY AGENTS (by
 *   structural relationship): epa_administrator — agenda-setting regulator
 *   (institutional/mobile) that defines the benchmarks and collects
 *   jurisdictional scope; coal_plant_operators — primary cost-bearing
 *   industry (powerful/trapped, immovable sunk assets);
 *   coal_mining_communities — concentrated regional cost-bearers
 *   (moderate/trapped); renewable_energy_producers — principal
 *   compliance-demand recipients (organized/mobile); natural_gas_generators —
 *   near-term beneficiary with next-cycle exposure (institutional/mobile);
 *   fossil_locked_state_governments — implementing payers drafting the
 *   pathways that shrink their own fiscal base (institutional/trapped);
 *   downwind_health_beneficiaries — diffuse health gainers with no procedural
 *   seat (powerless/trapped); environmental_advocacy_groups — mobilized
 *   supporters (organized/mobile); compliant_state_ratepayers — distributed
 *   cost carriers (moderate/trapped); future_generations — absent claimants
 *   (powerless/trapped); federal_judicial_reviewers — adjudicating observers
 *   (institutional/analytical). EPSILON REFERENT: following the
 *   kernel-reading rule, epsilon's referent is the standing arrangement under
 *   contest — the broad-delegation regime itself — assessed by this reading's
 *   own lights: the reading regards the arrangement as lawful and necessary,
 *   yet honestly registers the concentrated cost-bearing it imposes,
 *   partially offset by compliance-flexibility mechanisms. CLAIM/METRIC
 *   INDEPENDENCE: claimed_type (tangled_rope) is stated from what I believe
 *   structurally true — genuine coordination function plus asymmetric
 *   cost-bearing plus active enforcement — while the metrics are authored
 *   descriptively; the engine computes per-seat classifications and any
 *   divergence between claim and computed type is the datum, not an error.
 *
 * KEY AGENTS:
 *   - epa_administrator: agenda-setting regulator (institutional/mobile) — sets system-wide benchmarks, approves or displaces state plans, collects jurisdictional scope
 *   - coal_plant_operators: primary cost-bearing industry (powerful/trapped) — stranded assets, forced retirement pathway, immovable capital
 *   - coal_mining_communities: concentrated regional cost-bearers (moderate/trapped) — employment, tax-base, and pension losses with costly relocation
 *   - renewable_energy_producers: principal compliance-demand recipients (organized/mobile) — mandated procurement converts targets into contracts
 *   - natural_gas_generators: near-term beneficiary, next-cycle exposed (institutional/mobile) — bridge-fuel gains now, benchmarking precedent later
 *   - fossil_locked_state_governments: implementing payers (institutional/trapped) — administer plans while fiscally dependent on the shrinking industry
 *   - downwind_health_beneficiaries: diffuse health gainers (powerless/trapped) — mortality and morbidity improvements, no procedural seat
 *   - environmental_advocacy_groups: mobilized supporters (organized/mobile) — litigation and mobilization capacity tied to regulatory ambition
 *   - compliant_state_ratepayers: distributed cost carriers (moderate/trapped) — retail-rate compliance costs, shared benefits, intervenor-channel participation
 *   - future_generations: absent claimants (powerless/trapped) — bear addressed consequences with no seat in the process
 *   - federal_judicial_reviewers: adjudicating observers (institutional/analytical) — rulings redefine the operative boundary without bearing its costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__systemic_transformation_reading, 0.55).
domain_priors:suppression_score(caa_section_111d_delegation__systemic_transformation_reading, 0.7).
domain_priors:theater_ratio(caa_section_111d_delegation__systemic_transformation_reading, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__systemic_transformation_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__systemic_transformation_reading, "Clean Air Act Section 111(d) Systemic Transformation Reading (Grid-Wide Best System)").
narrative_ontology:topic_domain(caa_section_111d_delegation__systemic_transformation_reading, "administrative law/environmental regulation/constitutional interpretation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__systemic_transformation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__systemic_transformation_reading, 'e2126b16-3cde-497d-b4c1-f8522ef48255').
narrative_ontology:cs_kernel_codification('e2126b16-3cde-497d-b4c1-f8522ef48255', fixed_text).
narrative_ontology:cs_authority_grounding('e2126b16-3cde-497d-b4c1-f8522ef48255', lineage).
narrative_ontology:cs_interpretation_layer_present('e2126b16-3cde-497d-b4c1-f8522ef48255').
narrative_ontology:cs_reading_relation('e2126b16-3cde-497d-b4c1-f8522ef48255', caa_section_111d_delegation__facility_constraint_reading, forecloses).
narrative_ontology:cs_axiom('e2126b16-3cde-497d-b4c1-f8522ef48255', foundational, bser_scope_includes_grid_wide_generation_shifting).
narrative_ontology:cs_axiom_status(bser_scope_includes_grid_wide_generation_shifting, holdable).
narrative_ontology:cs_axiom_grounding('e2126b16-3cde-497d-b4c1-f8522ef48255', bser_scope_includes_grid_wide_generation_shifting, conventional).
narrative_ontology:cs_axiom('e2126b16-3cde-497d-b4c1-f8522ef48255', foundational, implicit_delegation_extends_to_transformative_regulation).
narrative_ontology:cs_axiom_status(implicit_delegation_extends_to_transformative_regulation, overridden).
narrative_ontology:cs_axiom_grounding('e2126b16-3cde-497d-b4c1-f8522ef48255', implicit_delegation_extends_to_transformative_regulation, conventional).
narrative_ontology:cs_reference_frame('e2126b16-3cde-497d-b4c1-f8522ef48255', chevron_era_expert_delegation_settlement).
narrative_ontology:cs_drift_state('e2126b16-3cde-497d-b4c1-f8522ef48255', post_west_virginia_v_epa, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('e2126b16-3cde-497d-b4c1-f8522ef48255', '').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__systemic_transformation_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_producers).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, natural_gas_generators).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, downwind_health_beneficiaries).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, environmental_advocacy_groups).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, coal_plant_operators).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, coal_mining_communities).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, fossil_locked_state_governments).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, compliant_state_ratepayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, compliant_state_ratepayers).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, natural_gas_generators).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__systemic_transformation_reading, agency_expertise_deference_tradition).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__systemic_transformation_reading, systemic_scope_bser_interpretation).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__systemic_transformation_reading, cooperative_federalism_implementation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets nationwide performance benchmarks for existing fossil generation and reviews state implementation plans, approving or displacing them with federal plans. Collects expanded regulatory jurisdiction and the technical-authority role of defining what counts as the best available reduction approach. Exit looks like revising or rescinding guidelines, constrained by statute, courts, and political turnover.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, epa_administrator, agenda_setter,
    institutional, generational, mobile, national).

% Own and operate the existing coal fleet that the performance benchmarks render progressively uneconomic; compliance pathways run through retrofit, refueling, or early retirement, and sunk site-specific capital cannot relocate. Litigation and lobbying capacity is substantial, but the asset base is immovable.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, coal_plant_operators, payer,
    powerful, biographical, trapped, national).

% Live in regions where production declines translate directly into employment, tax-base, and pension losses; severance-funded budgets and single-industry labor markets leave few local alternatives. Geographic roots, specialized skills, and depressed property values make relocation costly.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, coal_mining_communities, payer,
    moderate, generational, trapped, regional).

% Build wind, solar, and storage capacity whose demand is accelerated by the compliance targets; state plan trading mechanisms convert mandated emission cuts into procurement contracts. The industry thrives on policy momentum but is not dependent on this single instrument.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_producers, beneficiary,
    organized, biographical, mobile, national).

% Gain near-term dispatch share as coal units retire, positioning gas as the compliance bridge fuel; the same benchmarking logic that displaces coal sets a precedent that reaches gas in later regulatory cycles. Fleet capital is long-lived and increasingly exposed.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, natural_gas_generators, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(caa_section_111d_delegation__systemic_transformation_reading, natural_gas_generators, payer).

% Administer the state plans under the cooperative-federalism structure while their fiscal bases depend on severance revenues and coal employment; they draft the very pathways that shrink their dominant industry. Budget dependence and grid infrastructure lock them into compliance they did not choose.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, fossil_locked_state_governments, payer,
    institutional, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(caa_section_111d_delegation__systemic_transformation_reading, fossil_locked_state_governments, agenda_setter).

% Experience reduced particulate and ozone exposure as coal units retire, with mortality and morbidity improvements concentrated in communities downwind of the fleet. They cannot relocate away from regional airsheds and hold no direct procedural seat.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, downwind_health_beneficiaries, beneficiary,
    powerless, biographical, trapped, continental).

% Litigate, comment, and mobilize in support of the broad reading; membership and funding track regulatory ambition. Success depends on durable legal authorization rather than any single administration's tenure.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, environmental_advocacy_groups, beneficiary,
    organized, civilizational, mobile, national).

% Carry compliance costs through retail rates and system charges in states executing ambitious plans, while sharing in the air-quality and climate benefits. They cannot opt out of grid service and participate mainly through intervenor processes.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, compliant_state_ratepayers, payer,
    moderate, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(caa_section_111d_delegation__systemic_transformation_reading, compliant_state_ratepayers, beneficiary).

% Bear the climate consequences the reading claims to address and inherit whatever generation mix results; they hold no seat in rulemaking dockets, comment periods, or plan negotiations.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, future_generations, excluded,
    powerless, civilizational, trapped, global).

% Adjudicate the scope contest between competing interpretations of the statutory authorization, applying interpretive doctrine to determine what the delegation covers; their rulings redefine the operative boundary without themselves bearing its costs.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, federal_judicial_reviewers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_producers).
narrative_ontology:fixing_cost_class(caa_section_111d_delegation__systemic_transformation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action failure of grid decarbonization: individual generators and states cannot capture the full benefits of emission reduction, so federally set performance standards with state-implemented pathways coordinate generation-shifting (renewable substitution, early coal retirement) across interconnected regional grids.
% TRANSFER_FUNCTION: Moves compliance obligation and capital deployment away from coal and toward renewable generation: stranded-asset losses and premature-retirement costs fall on coal plant owners and coal-dependent regions; mandated demand and compliance-market revenue flow to renewable builders; health improvements accrue downwind; rate impacts land on compliant-state customers.
% ABSENT_VOICES: Coal-mining communities held only comment-period access during rulemaking — no formal seat in the federal-state plan negotiation. Future generations, on whose behalf the reading claims to act, hold no procedural position. Residential ratepayers enter only through intervenor funding in state proceedings. Their absence shapes whose costs count in the adequacy balancing.
% DISAPPEARANCE_RATIONALE: If the broad-delegation arrangement vanished overnight, state decarbonization plans would lapse, coal retirements would revert to market-paced attrition, mandated renewable procurement would unwind to subsidy-and-market rates, and the federal-state implementation architecture built around pathway plans would dissolve into facility-by-facility permitting.
% FOUNDING_PROBLEM: Existing stationary sources escaped the new-source performance standards that Section 111 imposed on newly built facilities; Section 111(d), as clarified by the 1990 amendments, was built to bring the installed fleet under performance standards through state-administered plans, extending technology-forcing control to sources already operating.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the legislative history of the 1970 Act and 1990 amendments, EPA's published regulatory-impact analyses, and — decisively — the litigation record, in which coal-state attorneys general and industry litigants acknowledge the reality of interstate pollution and the existing-source control problem while disputing only the breadth of the authorized instrument. Independent academic and policy analyses attest both the problem's persistence and the contest over instrument scope.
narrative_ontology:disappearance_verdict(caa_section_111d_delegation__systemic_transformation_reading, world_rearranges).
narrative_ontology:founding_problem_status(caa_section_111d_delegation__systemic_transformation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__systemic_transformation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(caa_section_111d_delegation__systemic_transformation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(caa_section_111d_delegation__systemic_transformation_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(caa_section_111d_delegation__systemic_transformation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(caa_section_111d_delegation__systemic_transformation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(caa_section_111d_delegation__systemic_transformation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.55 at interval end) reflects concentrated cost-bearing on coal assets and coal-dependent regions, moderated in this reading's own assessment by compliance flexibility (trading, averaging, banking) and by the health gains the arrangement delivers. Suppression (0.70) is structural compulsion, not violence: enforceable benchmarks, federal-plan backstops for non-compliant states, and litigation-defended authority leave trapped asset holders and locked states little lawful exit. NOTE: suppression is a raw structural property, unscaled by power or scope; only extractiveness is scaled by directionality and scope in the engine's computation. Theater ratio (0.24) is low-moderate: the arrangement performs real emission work, with the performative share concentrated in plan paperwork, accounting instruments, and the symbolic rulemaking of the repeal-revival ping-pong. Accessibility collapse (0.45): alternatives exist (plan design, timing, trading structures) but the compelled direction narrows the option space substantially without collapsing it. Resistance (0.78) is among the highest recorded for a regulatory construct: multi-state attorney-general coalitions, industry litigation, congressional disapproval attempts, and ultimately presidential-level judicial intervention — a demonstration that coalition power among nominally weaker-seated actors can reach the strongest enforcement seat. TEMPORAL DYNAMICS: the series runs on one shared seven-point grid (all three metrics authored at every point). Base extractiveness dips at t=4 (the repeal window, when the broad reading was formally withdrawn and coal cost-bearing eased) then recovers and climbs through the revival era — a single perturbation cycle driven by administration turnover, not intermittent reinforcement; the oscillation is a side effect of electoral cycling, not an extraction mechanism. Theater spikes at t=4 for the same reason: during the repeal-revival window both sides' rulemakings were largely placeholders pending adjudication. Suppression requirement rises monotonically across the interval: the story specifically tracks enforcement-capacity change — holding the broad reading against mounting resistance demanded escalating legal machinery (guideline revisions, federal-plan threats, litigation defense), which is why suppression_requirement is authored here while the enforcement picture is anything but static.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently from the same structure. From the EPA seat the arrangement is coordination it built and defends: benchmarks solve a collective problem states cannot solve alone. From the coal operator and coal community seats the same structure operates as compelled retirement of their livelihoods with immovable capital. The fossil-locked state governments occupy both positions at once — administering the plans (agenda-setter function) while bearing the fiscal consequences (payer function) — which is why they carry a secondary role. The judicial reviewer seat experiences neither cost nor gain, only the interpretive contest. Coalition dynamics matter here: the payer-side states converted individually moderate power into a durable multi-state coalition that reached the apex enforcement seat — the engine should see coalition capability latent in the payer cluster despite modest per-seat power.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: renewable_energy_producers (declared beneficiary, mobile exit) sit near the beneficiary end; downwind_health_beneficiaries (beneficiary, trapped, diffuse gains) sit near it as well; environmental_advocacy_groups collect influence rather than revenue. Coal_plant_operators (victim, trapped, immovable assets) sit near the full-target end; coal_mining_communities likewise, with geographic and skill lock-in deepening their position. Compliant_state_ratepayers straddle: declared victims carrying costs, with secondary beneficiary position from shared health gains. NO DIRECTIONALITY OVERRIDES AUTHORED, deliberately: the override mechanism keys on power atoms, and this story contains two institutional actors needing opposite corrections (EPA near the beneficiary end as agenda-setter; fossil_locked_state_governments near the target end as implementing payers) — a single atom-keyed override cannot separate them and would corrupt one seat to fix the other. The structural declarations (roles, secondary roles, exit options) already encode the differentiation, so the derivation chain is left to do the work.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetrical mislabelings. Reading the arrangement as pure extraction (snare) would erase the genuine coordination function — grid decarbonization is a real collective-action problem that no single state or generator can solve, and the health gains are real, delivered, and verifiable. Reading it as pure coordination (rope) would erase the concentrated, coerced cost-bearing on coal assets and coal-dependent regions whose exit options are structurally closed. Tangled rope preserves both halves: coordination function (beneficiaries declared, enforcement required) plus asymmetric extraction (victims declared, active enforcement holding the asymmetry in place). Mandatrophy is not resolved: the founding problem (existing-source emissions escaping new-source standards) remains live, corroborated from outside the benefiting parties, and the disappearance verdict (world_rearranges) confirms arrangements genuinely depend on the constraint — the founding-problem-status x disappearance-verdict pairing produces no zombie flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the caa_section_111d_delegation kernel — what would the facility_constraint_reading sibling change structurally if it became the operative reading?',
    'Adjudication by further Supreme Court review or explicit congressional amendment of Section 111(d); observe which reading governs subsequent EPA rulemakings and state plan cycles.',
    'Under the facility reading, EPA is confined to at-facility measures; coal-sector cost-bearing falls sharply, mandated renewable demand disappears, and this reading''s beneficiary/victim structure dissolves toward diffuse. The kernel''s operative constraint would classify far less extractively.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: this story instantiates the systemic_transformation_reading; the sibling reading is a different constraint with a different victim set.').

omega_variable(
    major_questions_revival_path,
    'Can the systemic reading regain operative status through explicit congressional authorization or state-level analogues, or is it permanently confined to scholarship and attenuated rulemaking forms?',
    'Track legislative proposals for a clean electricity standard, the scope of subsequent Section 111 rulemakings, and further applications of the major questions doctrine.',
    'Revival restores the full beneficiary/victim structure and enforcement demands modeled here; permanent confinement converts this story into a historical record whose residual form trends inertial within the constraint family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(major_questions_revival_path, empirical, 'Whether the reading''s foundational scope claim can be re-grounded after the major questions doctrine intervention.').

omega_variable(
    just_transition_offset_question,
    'Do just-transition provisions (worker assistance, community redevelopment funding) materially offset the costs borne by coal_mining_communities, or is the offset rhetorical?',
    'Compare appropriated transition funding against modeled community losses; audit delivery rates in coal-dependent regions against program announcements.',
    'Substantial delivered offsets lower effective cost-bearing on the community seat and soften the hybrid''s asymmetry; rhetorical-only offsets leave the asymmetry intact and strengthen the extraction half of the computation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(just_transition_offset_question, empirical, 'Whether compensation mechanisms actually reach the concentrated regional losers.').

omega_variable(
    state_discretion_depth,
    'How much genuine implementation discretion do state plans retain under the systemic reading — enough to constitute real coordination participation, or is state planning a compliance formality?',
    'Comparative analysis of approved state implementation plans: measure divergence between state-designed pathways and federal template defaults, and the durability of state design choices across administrations.',
    'Deep discretion strengthens the coordination-function half of the hybrid; shallow discretion shifts computational weight toward the extraction half at the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_discretion_depth, conceptual, 'Whether cooperative-federalism structure delivers real state agency or procedural theater.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__systemic_transformation_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa__tr_t0, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(caa__tr_t0, observed).
narrative_ontology:measurement(caa__tr_t2, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2, 0.17).
narrative_ontology:measurement_basis(caa__tr_t2, observed).
narrative_ontology:measurement(caa__tr_t4, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 4, 0.31).
narrative_ontology:measurement_basis(caa__tr_t4, observed).
narrative_ontology:measurement(caa__tr_t6, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 6, 0.27).
narrative_ontology:measurement_basis(caa__tr_t6, observed).
narrative_ontology:measurement(caa__tr_t8, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement_basis(caa__tr_t8, observed).
narrative_ontology:measurement(caa__tr_t10, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(caa__tr_t10, observed).
narrative_ontology:measurement(caa__tr_t12, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement_basis(caa__tr_t12, projected).

% Extraction over time
narrative_ontology:measurement(caa__be_t0, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(caa__be_t0, observed).
narrative_ontology:measurement(caa__be_t2, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2, 0.5).
narrative_ontology:measurement_basis(caa__be_t2, observed).
narrative_ontology:measurement(caa__be_t4, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 4, 0.45).
narrative_ontology:measurement_basis(caa__be_t4, observed).
narrative_ontology:measurement(caa__be_t6, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 6, 0.47).
narrative_ontology:measurement_basis(caa__be_t6, observed).
narrative_ontology:measurement(caa__be_t8, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 8, 0.51).
narrative_ontology:measurement_basis(caa__be_t8, observed).
narrative_ontology:measurement(caa__be_t10, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 10, 0.53).
narrative_ontology:measurement_basis(caa__be_t10, observed).
narrative_ontology:measurement(caa__be_t12, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement_basis(caa__be_t12, projected).

% Suppression requirement over time
narrative_ontology:measurement(caa__su_t0, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(caa__su_t0, observed).
narrative_ontology:measurement(caa__su_t2, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2, 0.52).
narrative_ontology:measurement_basis(caa__su_t2, observed).
narrative_ontology:measurement(caa__su_t4, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 4, 0.58).
narrative_ontology:measurement_basis(caa__su_t4, observed).
narrative_ontology:measurement(caa__su_t6, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement_basis(caa__su_t6, observed).
narrative_ontology:measurement(caa__su_t8, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 8, 0.66).
narrative_ontology:measurement_basis(caa__su_t8, observed).
narrative_ontology:measurement(caa__su_t10, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(caa__su_t10, observed).
narrative_ontology:measurement(caa__su_t12, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement_basis(caa__su_t12, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__systemic_transformation_reading, resource_allocation).
narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, caa_section_111d_delegation__facility_constraint_reading).
narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, caa_title_iv_acid_rain_trading_program).
narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, epa_section_111b_new_source_standards).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'Section 111(d) authority.' The single statutory term 'best system of emission reduction' covers two structurally distinct delegations: facility-scoped measures (sibling story caa_section_111d_delegation__facility_constraint_reading) and grid-wide generation-shifting strategies (this story). The readings assign different extensions to the same term, produce different victim sets, and carry different epsilon values; they are authored as separate epsilon-invariant stories linked through network.affects_constraints. Post-2022, the facility reading holds the doctrinally favored position and structurally constrains this reading's operating environment; this reading persists as a live rival claim pursued through explicit-authorization paths.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
