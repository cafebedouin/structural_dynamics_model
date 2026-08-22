% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__precautionary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_legitimacy_kernel__precautionary_reading, []).

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
 *   constraint_id: technology_legitimacy_kernel__precautionary_reading
 *   human_readable: Precautionary Legitimacy Test: Bounded/Reversible-Within-a-Generation Standard for Climate Technology
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint instantiates the precautionary reading of the
 *   technology_legitimacy_kernel: a technology counts as legitimate climate
 *   mitigation only if its worst-case failure modes and legacy costs are
 *   bounded and reversible within roughly one human generation. Applied
 *   consistently, this reading places renewables (and storage) inside the
 *   legitimate set — a decommissioned wind farm or solar array leaves a site
 *   restorable within years to decades — while excluding nuclear power, whose
 *   worst-case failure modes (core accidents producing long-lived
 *   contamination) and ordinary legacy costs (spent fuel requiring management
 *   on multi-millennial timescales) exceed any generational reversibility
 *   bound. The standard began as a genuine coordination device — a shared,
 *   technology-neutral-sounding test to avoid case-by-case relitigation of
 *   safety claims — but has become a lever actively used by advocacy
 *   organizations and aligned financiers to reallocate capital and legitimacy
 *   away from nuclear, generating real victims among nuclear workers,
 *   baseload-dependent industries, and populations in regions without strong
 *   renewable resource endowments. It is authored as tangled_rope: the
 *   coordination function (a shared prospective test) is genuine, but the
 *   standing arrangement now requires active enforcement (taxonomy rules,
 *   financing exclusion criteria, permitting litigation) and produces
 *   asymmetric extraction — someone is coordinated (financiers, developers
 *   who benefit from a clear rule) and someone pays (nuclear-dependent
 *   workers and regions, and possibly future generations if delayed
 *   decarbonization proves costlier than the legacy risks avoided).
 *
 * KEY AGENTS:
 *   - renewable_energy_developers: primary beneficiary (organized/arbitrage) — captures financing and legitimacy under the reversibility test
 *   - environmental_advocacy_organizations: agenda-setter (organized/mobile) — authors and campaigns for the standard's adoption in policy and finance
 *   - nuclear_industry_workers: primary target (moderate/constrained) — bears loss of financing and legitimacy regardless of individual plant safety record
 *   - future_generations_bearing_delayed_decarbonization: diffuse target (powerless/trapped) — bears the opportunity cost if excluding nuclear slows aggregate decarbonization
 *   - grid_stability_dependent_industries: secondary payer (powerful/constrained) — bears reliability risk from a slowed nuclear pipeline
 *   - nuclear_regulatory_bodies: analytical observer (institutional/analytical) — assesses technical claims feeding the standard's application
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__precautionary_reading, 0.58).
domain_priors:suppression_score(technology_legitimacy_kernel__precautionary_reading, 0.42).
domain_priors:theater_ratio(technology_legitimacy_kernel__precautionary_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__precautionary_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__precautionary_reading, "Precautionary Legitimacy Test: Bounded/Reversible-Within-a-Generation Standard for Climate Technology").
narrative_ontology:topic_domain(technology_legitimacy_kernel__precautionary_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__precautionary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__precautionary_reading, 'f389b8ce-a6ec-4189-8168-210ebf87d46a').
narrative_ontology:cs_kernel_codification('f389b8ce-a6ec-4189-8168-210ebf87d46a', distributed).
narrative_ontology:cs_authority_grounding('f389b8ce-a6ec-4189-8168-210ebf87d46a', distributed).
narrative_ontology:cs_reading_relation('f389b8ce-a6ec-4189-8168-210ebf87d46a', technology_legitimacy_kernel__reliability_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('f389b8ce-a6ec-4189-8168-210ebf87d46a', technology_legitimacy_kernel__velocity_primacy_reading, influences).
narrative_ontology:cs_axiom('f389b8ce-a6ec-4189-8168-210ebf87d46a', foundational, irreversibility_is_the_binding_constraint).
narrative_ontology:cs_axiom_status(irreversibility_is_the_binding_constraint, holdable).
narrative_ontology:cs_axiom_grounding('f389b8ce-a6ec-4189-8168-210ebf87d46a', irreversibility_is_the_binding_constraint, deontological).
narrative_ontology:cs_axiom('f389b8ce-a6ec-4189-8168-210ebf87d46a', secondary, generational_timescale_is_the_correct_moral_horizon).
narrative_ontology:cs_axiom_status(generational_timescale_is_the_correct_moral_horizon, holdable).
narrative_ontology:cs_axiom_grounding('f389b8ce-a6ec-4189-8168-210ebf87d46a', generational_timescale_is_the_correct_moral_horizon, conventional).
narrative_ontology:cs_reference_frame('f389b8ce-a6ec-4189-8168-210ebf87d46a', post_fukushima_precautionary_consensus).
narrative_ontology:cs_drift_state('f389b8ce-a6ec-4189-8168-210ebf87d46a', advanced_reactor_deployment_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f389b8ce-a6ec-4189-8168-210ebf87d46a', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__precautionary_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, renewable_energy_developers).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, environmental_advocacy_organizations).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, future_generations_advocates).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, decentralized_grid_communities).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, nuclear_industry_workers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, grid_stability_dependent_industries).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, future_generations_bearing_delayed_decarbonization).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, energy_poor_regions_without_baseload_alternatives).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__precautionary_reading, reversibility_as_moral_baseline).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__precautionary_reading, precautionary_principle_in_technology_assessment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Solar and wind developers gain a legitimacy framework that structurally favors their technology class — decommissioning a wind farm or solar array is understood as reversible within decades, so their projects clear the bounded-and-reversible test cleanly. They capture financing, permitting priority, and public legitimacy from the same standard that excludes their nuclear competitors.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, renewable_energy_developers, beneficiary,
    organized, generational, arbitrage, global).

% Author and campaign for the reversibility standard in policy documents, court filings, and financing exclusion criteria (e.g., taxonomy rules, divestment screens). They administer the test in practice by lobbying regulators and rating agencies to adopt it, and shift their target technologies as the standard's implications become clearer.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, environmental_advocacy_organizations, agenda_setter,
    organized, generational, mobile, global).

% Groups explicitly speaking for people not yet born argue the standard protects them from inheriting irreversible waste burdens or accident-contaminated land. They benefit from the framework's logic but have no direct voice or enforcement power of their own — they act through proxies (advocacy orgs, litigation, youth movements) and cannot verify whether the standard is actually applied consistently.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, future_generations_advocates, beneficiary,
    powerless, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__precautionary_reading, future_generations_advocates, excluded).

% Engineers, plant operators, and supply-chain workers in the nuclear sector see financing, permitting, and public legitimacy withdrawn from their industry because spent fuel and worst-case accident scenarios are read as exceeding the generational-reversibility bar. Their careers and communities depend on an industry the standard structurally disfavors regardless of the plants' actual safety record; exit means retraining into an adjacent but distinct energy sector.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, nuclear_industry_workers, payer,
    moderate, biographical, constrained, national).

% Heavy manufacturing, hospitals, and data centers that require dispatchable baseload power bear the cost when the reversibility standard excludes nuclear and slows its replacement pipeline without securing an equally reliable substitute. They can lobby for exemptions or build private backup capacity, but cannot exit the regional grid they are physically tied to.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, grid_stability_dependent_industries, payer,
    powerful, biographical, constrained, regional).

% If excluding nuclear slows aggregate decarbonization relative to a reliability-primacy or velocity-primacy pathway, the accumulated atmospheric carbon burden falls on people who have no standing in the current debate at all. They cannot object, cannot exit, and their exposure depends entirely on which reading of the kernel prevails in the next two decades of infrastructure investment.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, future_generations_bearing_delayed_decarbonization, payer,
    powerless, civilizational, trapped, global).

% Regions lacking strong renewable resource endowments (low wind/solar capacity factor, no storage buildout) and excluded from nuclear financing under the precautionary standard are left dependent on fossil generation for longer, or face energy poverty during the transition. They have no meaningful exit from their geography or resource base.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, energy_poor_regions_without_baseload_alternatives, payer,
    powerless, biographical, trapped, regional).

% Agencies that assess nuclear safety and waste management take testimony from all sides and issue technical findings on containment timelines and accident probability. Their findings feed into, but do not control, whether financing bodies and courts adopt the precautionary reading of the kernel.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, nuclear_regulatory_bodies, observer,
    institutional, generational, analytical, national).

% Communities pursuing microgrids and distributed renewable generation gain legitimacy and financing preference under the standard, since small-scale reversible installations clear the bar easily and give them local control over energy infrastructure and decommissioning.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, decentralized_grid_communities, beneficiary,
    moderate, generational, mobile, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_legitimacy_kernel__precautionary_reading, renewable_energy_developers).
narrative_ontology:fixing_cost_class(technology_legitimacy_kernel__precautionary_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, generation-scoped test that lets financiers, regulators, and courts agree on which climate technologies to fund and permit without re-litigating the safety and legacy-cost case for each project individually.
% TRANSFER_FUNCTION: Moves financing, permitting priority, and public legitimacy away from technologies with irreversible or long-tail failure modes (principally nuclear) and toward technologies whose worst case is understood as decommissionable within roughly one generation (principally wind, solar, and storage), while shifting decarbonization-pace risk onto populations dependent on baseload power or lacking strong renewable resources.
% ABSENT_VOICES: Future generations bearing either irreversible waste/accident legacies OR delayed decarbonization have no direct voice at all — both possible harms are borne by people not yet born, represented only by advocacy proxies who cannot be cross-examined by the people actually affected. Energy-poor regions without renewable resource endowments are also structurally underrepresented in standard-setting bodies dominated by developed-economy environmental and financial institutions.
% DISAPPEARANCE_RATIONALE: If the reversibility-within-a-generation test vanished as a legitimacy criterion, nuclear financing would likely reopen substantially (subject to the reliability-primacy and velocity-primacy readings' own tests), taxonomy classifications and green bond eligibility would be rewritten, and advocacy campaigns built around the reversibility framing would lose their primary lever — the allocation of capital across technology classes would shift measurably within a decade.
% FOUNDING_PROBLEM: Post-Chernobyl and post-Fukushima recognition that some technologies impose costs (radioactive waste, contaminated exclusion zones, accident tail risk) that persist far longer than any single human lifetime or political mandate, combined with a felt need for a principled, technology-neutral-sounding test that could be applied prospectively to new energy technologies rather than relitigating each one's specific history.
% FOUNDING_PROBLEM_CORROBORATION: Environmental advocacy organizations and future-generations advocates attest the problem (irreversible legacy risk) remains fully live and central. Nuclear industry engineers and independent nuclear safety regulators (e.g., IAEA technical assessments) attest that modern reactor designs and geological repository engineering have substantially narrowed the reversibility gap the standard was built against, making its current application partly anachronistic to the technology it was calibrated on. Grid engineers outside both camps note the standard was never calibrated against the systemic risk of a slower, fossil-dependent transition, which is a live but separately corroborated harm.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__precautionary_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__precautionary_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__precautionary_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(technology_legitimacy_kernel__precautionary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__precautionary_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_legitimacy_kernel__precautionary_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_legitimacy_kernel__precautionary_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_legitimacy_kernel__precautionary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is authored as substantial but not extreme: the standard genuinely coordinates capital allocation around a defensible principle (irreversibility matters morally), but its consistent application to disfavor nuclear specifically, while treating renewable legacy costs (rare-earth mining, panel disposal, offshore decommissioning of larger installations) as comparatively negligible, reflects an asymmetric application that extracts legitimacy from one technology class to the benefit of another. Suppression (0.42) is moderate — the standard operates mainly through financing exclusion and permitting friction rather than outright prohibition; nuclear projects can still proceed where states subsidize them directly, so alternatives are constrained rather than eliminated. Theater ratio (0.28) reflects that the technical reversibility assessments are mostly real analytical work, though a growing share of advocacy activity functions as legitimacy theater once the underlying financing decisions have already been made on other grounds. Accessibility collapse (0.4) is moderate: alternative development pathways for nuclear remain legally available in some jurisdictions, so the standard has not achieved decisive lock-out everywhere. Resistance (0.62) is high because nuclear industry actors, some grid engineers, and reliability-focused policymakers actively contest the standard's application, arguing it is calibrated to a legacy fear (Chernobyl/Fukushima-era reactor and repository designs) rather than to current engineering.
 *
 * PERSPECTIVAL GAP:
 *   From the environmental advocacy / renewable developer seats, this standard is straightforwardly protective coordination — a bright-line rule that prevents locking in irreversible harms. From the nuclear worker and grid-stability-industry seats, the same rule is enforced extraction: a fixed, generation-scoped bar applied unevenly to disfavor their sector while renewables' own legacy costs (grid-scale battery disposal, land use, mining externalities) are treated as comparatively bounded without equivalent scrutiny. The engine computes these as different seat-level types from the same structural data; the divergence is the point, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewable developers and environmental advocacy organizations sit near the beneficiary end: the standard was substantially shaped by and for their agenda, and they capture financing and legitimacy gains directly. Nuclear industry workers and grid-stability-dependent industries sit near the target end: they bear concentrated, identifiable costs (job loss, reliability risk) from a rule they had limited input into. Future generations split structurally across BOTH the beneficiary and victim sets depending on which risk dominates empirically — this is the deepest irreducible uncertainty in the story and is carried as an omega rather than resolved by authorial fiat. Energy-poor regions without renewable resource endowments are powerless and trapped: they cannot exit their geography, and the standard offers them no reversible-and-bounded pathway that is also deployable at their scale and cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (irreversible legacy costs from certain energy technologies) remains partly live — nuclear waste and accident tail risk are real and not resolved by rhetoric. But treating the precautionary reading as the SOLE legitimate test for climate technology, rather than one input weighed against reliability and deployment-velocity considerations, risks mandatrophy: a standard justified by genuine caution about irreversibility becoming a vehicle for excluding a technology class from the climate-mitigation portfolio altogether, regardless of updated engineering evidence (Generation III+/IV reactor designs, deep geological repository engineering) that narrows the original gap. Classifying this as tangled_rope rather than snare preserves the fact that the coordination function is real and was not invented as cover — it prevents both under-crediting the standard's legitimate precautionary content and over-crediting its current asymmetric application as neutral technology assessment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    future_generations_net_position,
    'Do future generations net-benefit or net-lose under the precautionary reading, once both averted nuclear-legacy risk AND foregone/delayed decarbonization from excluding nuclear are weighed against each other?',
    'Long-horizon integrated assessment modeling comparing decarbonization trajectories and cumulative radiative forcing under portfolios that include vs. exclude nuclear at scale, combined with updated probabilistic risk assessment of Gen III+/IV reactor accident rates and repository containment performance.',
    'If exclusion measurably slows decarbonization more than it averts expected legacy harm, the precautionary reading itself produces the irreversible cost it was designed to prevent, which would support reclassifying part of its extraction as self-defeating rather than protective; if legacy risk dominates, the current victim/beneficiary split for future generations is substantially correct as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_generations_net_position, empirical, 'Whether the precautionary reading protects or harms future generations on net, given the decarbonization-speed tradeoff.').

omega_variable(
    reversibility_bar_calibration,
    'Is ''reversible within a generation'' calibrated to the actual engineering state of modern nuclear technology (advanced reactor designs, deep geological repositories with demonstrated multi-century containment modeling), or is it calibrated to 1980s-2011-era reactor and waste-management technology and simply not updated?',
    'Independent technical review comparing the standard''s stated reversibility timeframe assumptions against current IAEA and national regulatory body technical assessments of next-generation reactor designs and repository engineering (e.g., Finland''s Onkalo facility).',
    'If the bar is stale, the standard''s exclusion of nuclear is a mismeasurement being treated as a settled moral fact, strengthening the case that this is drifting toward mandatrophy; if the bar is current and nuclear genuinely fails it even under modern designs, the exclusion is well-founded on its own terms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reversibility_bar_calibration, empirical, 'Whether the generational-reversibility bar reflects current or outdated nuclear engineering assumptions.').

omega_variable(
    renewable_legacy_cost_asymmetry,
    'Are renewable technologies'' own legacy costs (rare-earth and lithium mining externalities, large-scale battery and panel disposal, offshore wind decommissioning) being assessed against the SAME generational-reversibility bar applied to nuclear, or is the bar applied more leniently to renewables because they are the standard''s incumbent beneficiary?',
    'Comparative lifecycle legacy-cost audits applying identical reversibility criteria and time horizons across nuclear, solar, wind, and storage technologies, conducted by parties without a stake in either outcome.',
    'If renewables receive materially more lenient treatment under the same nominal test, the standard''s claimed technology-neutrality is false, supporting a tangled_rope (or stronger) classification; if the asymmetry is justified by genuinely different risk profiles, the current classification''s extraction estimate may be somewhat high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewable_legacy_cost_asymmetry, conceptual, 'Whether the reversibility bar is applied with genuine technology-neutrality or asymmetrically favors the incumbent beneficiary class.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__precautionary_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(tech_tr_t4, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(tech_tr_t8, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement(tech_tr_t12, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(tech_tr_t16, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(tech_tr_t20, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(tech_tr_t24, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(tech_be_t4, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 4, 0.41).
narrative_ontology:measurement(tech_be_t8, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(tech_be_t12, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(tech_be_t16, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(tech_be_t20, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(tech_be_t24, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(tech_su_t4, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 4, 0.27).
narrative_ontology:measurement(tech_su_t8, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 8, 0.31).
narrative_ontology:measurement(tech_su_t12, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 12, 0.35).
narrative_ontology:measurement(tech_su_t16, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 16, 0.38).
narrative_ontology:measurement(tech_su_t20, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(tech_su_t24, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__precautionary_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(technology_legitimacy_kernel__precautionary_reading, 0.12).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__precautionary_reading, technology_legitimacy_kernel__reliability_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__precautionary_reading, technology_legitimacy_kernel__velocity_primacy_reading).

% DUAL FORMULATION NOTE:
% These three constraints are sibling readings of technology_legitimacy_kernel, decomposed per the epsilon-invariance principle: each reading assigns a structurally different legitimacy test to the same natural-language claim ('legitimate climate technology'), producing different beneficiary/victim sets and different epsilon values. precautionary_reading favors renewables and excludes nuclear on irreversibility grounds; reliability_primacy_reading favors dispatchable/baseload technologies (potentially including nuclear) and may disfavor variable renewables without storage; velocity_primacy_reading favors whatever can be deployed fastest at scale, which could favor either camp depending on manufacturing and permitting timelines. The three readings are NOT averaged or reconciled into one constraint — each is authored independently with its own ID, its own epsilon, and its own stakeholder set, linked here for contamination-propagation analysis: a legitimacy crisis or resolution in one reading's standing (e.g., a major nuclear accident, or a major battery-recycling failure) will structurally pressure the other two readings' plausibility and adoption.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
