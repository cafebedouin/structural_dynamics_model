% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__velocity_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_legitimacy_kernel__velocity_primacy_reading, []).

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
 *   constraint_id: technology_legitimacy_kernel__velocity_primacy_reading
 *   human_readable: Deployment-Velocity Legitimacy Screen (Carbon-Budget Timeline Reading)
 *   domain: energy policy/climate mitigation/technology governance
 *
 * SUMMARY:
 *   Since the mid-2010s, climate policy discourse has operated a screening
 *   convention under which a mitigation technology counts as legitimate only
 *   if it can reach deployment scale within the remaining carbon budget —
 *   anchored to 2030 interim milestones and 2050 net-zero dates. The screen
 *   is administered not by any single statute but through deployability
 *   scorecards, green-taxonomy inclusion rules, subsidy design, lender
 *   due-diligence templates, and campaign rhetoric, and it is actively
 *   defended against counterargument. Its operation channels support toward
 *   solar, wind, and storage; sets aside new nuclear proposals on schedule
 *   grounds ahead of engineering review; and leaves the costs of integrating
 *   variable output with grid operators and ratepayers. This file
 *   instantiates one reading of the broader technology-legitimacy kernel;
 *   sibling files instantiate rival gating criteria, and the family
 *   relationship is recorded in network.affects_constraints and the
 *   kernel-context note. Per the epsilon-invariance principle, epsilon here
 *   is authored for THIS reading's arrangement only — the standing screen as
 *   operated — not for any rival arrangement.
 *
 * KEY AGENTS:
 *   - - solar_wind_manufacturers: Primary beneficiary (powerful/constrained) — collects procurement priority, credit eligibility, and permitting preference
 *   - - renewable_project_developers: Beneficiary (organized/mobile) — collects development margins on favored categories
 *   - - climate_advocacy_organizations: Agenda-setter (organized/identity_locked) — administers the screen's discourse and enforces its boundaries
 *   - - esg_green_finance_institutions: Beneficiary (institutional/arbitrage) — converts the screen into investable categories and fee income
 *   - - natural_gas_backup_producers: Incidental beneficiary (institutional/mobile) — keeps the firming role while rivals are schedule-disqualified
 *   - - nuclear_power_industry: Primary target (powerful/constrained) — bears schedule-based pre-disqualification
 *   - - advanced_nuclear_developers: Target (moderate/trapped) — bears capital starvation mid-certification
 *   - - grid_transmission_operators: Target (institutional/trapped) — absorbs unbilled integration costs
 *   - - electricity_ratepayers: Diffuse target (powerless/trapped) — carries socialized system costs through tariffs
 *   - - energy_system_modelers: Analytical observer (analytical/analytical) — sees full structure across candidate technologies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__velocity_primacy_reading, 0.63).
domain_priors:suppression_score(technology_legitimacy_kernel__velocity_primacy_reading, 0.55).
domain_priors:theater_ratio(technology_legitimacy_kernel__velocity_primacy_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, extractiveness, 0.63).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__velocity_primacy_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__velocity_primacy_reading, "Deployment-Velocity Legitimacy Screen (Carbon-Budget Timeline Reading)").
narrative_ontology:topic_domain(technology_legitimacy_kernel__velocity_primacy_reading, "energy policy/climate mitigation/technology governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__velocity_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__velocity_primacy_reading, 'adbd0009-b4b1-4604-8069-876cb3a215d6').
narrative_ontology:cs_kernel_codification('adbd0009-b4b1-4604-8069-876cb3a215d6', distributed).
narrative_ontology:cs_authority_grounding('adbd0009-b4b1-4604-8069-876cb3a215d6', distributed).
narrative_ontology:cs_reading_relation('adbd0009-b4b1-4604-8069-876cb3a215d6', technology_legitimacy_kernel__reliability_primacy_reading, influences).
narrative_ontology:cs_reading_relation('adbd0009-b4b1-4604-8069-876cb3a215d6', technology_legitimacy_kernel__precautionary_reading, coexists_with).
narrative_ontology:cs_axiom('adbd0009-b4b1-4604-8069-876cb3a215d6', foundational, deployment_velocity_decides_legitimacy).
narrative_ontology:cs_axiom_status(deployment_velocity_decides_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('adbd0009-b4b1-4604-8069-876cb3a215d6', deployment_velocity_decides_legitimacy, instrumental).
narrative_ontology:cs_axiom('adbd0009-b4b1-4604-8069-876cb3a215d6', secondary, carbon_budget_deadline_is_binding).
narrative_ontology:cs_axiom_status(carbon_budget_deadline_is_binding, holdable).
narrative_ontology:cs_axiom_grounding('adbd0009-b4b1-4604-8069-876cb3a215d6', carbon_budget_deadline_is_binding, empirically_contingent).
narrative_ontology:cs_reference_frame('adbd0009-b4b1-4604-8069-876cb3a215d6', carbon_budget_velocity_standard).
narrative_ontology:cs_drift_state('adbd0009-b4b1-4604-8069-876cb3a215d6', contemporary_mid2020s, gap(repudiation_pressure, minor, false)).
narrative_ontology:cs_created_at('adbd0009-b4b1-4604-8069-876cb3a215d6', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__velocity_primacy_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, solar_wind_manufacturers).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, renewable_project_developers).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, climate_advocacy_organizations).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, esg_green_finance_institutions).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, natural_gas_backup_producers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_power_industry).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, advanced_nuclear_developers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, grid_transmission_operators).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, electricity_ratepayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, electricity_ratepayers).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__velocity_primacy_reading, carbon_budget_depletion_urgency).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__velocity_primacy_reading, renewable_learning_curve_determinism).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__velocity_primacy_reading, levelized_cost_decision_heuristic).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Manufacture photovoltaic modules, wind turbines, and grid batteries. Policy frameworks that condition support on deployment speed channel procurement, tax credits, and permitting priority toward their product lines; sales forecasts and factory investment decisions assume that priority continues. Switching product lines toward disfavored categories would mean writing down specialized plant and re-entering markets against entrenched incumbents.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, solar_wind_manufacturers, beneficiary,
    powerful, biographical, constrained, global).

% Site, finance, and operate utility-scale solar, wind, and storage projects. Deployability-based support determines which projects clear financing hurdles and interconnection queues; development pipelines are built around the expectation that fast categories retain first call on subsidies and grid access.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, renewable_project_developers, beneficiary,
    organized, biographical, mobile, regional).

% Run campaigns, publish technology scorecards, and advise legislators on which mitigation options deserve support. Staff, donor bases, and theory of change are organized around the proposition that the remaining budget allows only fast-deploying options; advocating otherwise would undercut the urgency framing on which fundraising and coalition discipline rest.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, climate_advocacy_organizations, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__velocity_primacy_reading, climate_advocacy_organizations, beneficiary).

% Construct labeled funds and lending screens for climate-aligned assets. A deployability criterion gives them a simple, defensible screen for inclusion and exclusion; portfolio products, methodology teams, and fee income are built on the resulting categories. Mandates can be rewritten if the screen loses credibility, at the cost of relabeling and client communication.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, esg_green_finance_institutions, beneficiary,
    institutional, biographical, arbitrage, global).

% Produce and sell gas-fired generation capacity that firms variable output when wind and solar fall short. Because rival firm low-carbon entrants face schedule-based disqualification, the firming role remains open to gas through the transition period; industry planning documents treat extended bridge demand as a base case. The connection between the deployability screen and this preserved role is not part of the screen's stated rationale.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, natural_gas_backup_producers, beneficiary,
    institutional, biographical, mobile, continental).

% Operate existing reactor fleets and develop new units. Lifecycle carbon output is low, but new construction routinely exceeds the schedule thresholds the screen applies, so proposals are set aside before engineering review regardless of merit arguments; operating licenses, workforce, and supply chains are asset-specific, so the realistic alternative to pursuing new builds within this framework is managed decline.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_power_industry, payer,
    powerful, generational, constrained, global).

% Develop small modular reactors and other new fission designs. Capital raising depends on visible policy legitimacy; schedule-based disqualification dries up institutional investment mid-certification, and sunk licensing costs leave no comparable market to pivot into.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, advanced_nuclear_developers, payer,
    moderate, biographical, trapped, national).

% Plan and operate transmission networks and system balancing under whatever generation mix policy delivers. Rapid variable-renewable growth increases curtailment management, ancillary-service procurement, storage integration, and network reinforcement work; these costs are recovered through tariffs and system charges rather than attributed to the portfolio that created them. The obligation to maintain reliability cannot be exited.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, grid_transmission_operators, payer,
    institutional, generational, trapped, national).

% Pay electricity tariffs that bundle energy, network, and balancing costs. Cheap variable generation lowers wholesale energy prices at times of high output; system integration and network costs arrive later in the bill. Individual households have no practical exit from the grid tariff structure, though consumer advocacy groups aggregate their voice in regulatory proceedings.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, electricity_ratepayers, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__velocity_primacy_reading, electricity_ratepayers, beneficiary).

% Produce the integrated scenarios and pathway models that governments and campaigners cite. They observe the full structure — costs, build rates, firm-capacity needs, budget arithmetic — across all candidate technologies, and their publications are contested terrain among the other parties.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, energy_system_modelers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_legitimacy_kernel__velocity_primacy_reading, solar_wind_manufacturers).
narrative_ontology:fixing_cost_class(technology_legitimacy_kernel__velocity_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under a finite carbon budget with dated milestones, allocates scarce policy attention, public finance, permitting capacity, and private capital toward the technologies whose abatement arrives soonest, so that cumulative emissions avoided per year of remaining budget is maximized.
% TRANSFER_FUNCTION: Moves legitimacy, subsidy eligibility, green-finance access, and regulatory priority from slow-building low-carbon supply (new nuclear, CCS, deep geothermal) to fast-building supply (solar, wind, storage); moves the system costs of variable-output integration onto grid operators and, through tariffs, onto ratepayers.
% ABSENT_VOICES: Reliability planners and nuclear engineers testify in technical venues but sit outside the legitimacy conversation the screen governs; communities that would host long-build infrastructure have no seat; future generations who inherit whichever lock-in results are represented only by proxy. Within the screen's own forums, the objection 'slow but firm' is answered by definition rather than by analysis.
% DISAPPEARANCE_RATIONALE: If the screen vanished overnight, subsidy portfolios and finance screens would rebalance toward multi-criteria selection, several suspended nuclear programs would restart feasibility work, gas bridge-demand assumptions would tighten, and grid plans written around maximal variable penetration would be revisited — the low-carbon buildout would reorganize around a different selection rule.
% FOUNDING_PROBLEM: After Copenhagen stalled comprehensive climate policy, advocates needed a decision rule for choosing among low-carbon options under a hard deadline: which technologies deserve movement support when the budget allows no time for slow bets to mature?
% FOUNDING_PROBLEM_CORROBORATION: IPCC Working Group III carbon-budget assessments and IEA scenario work — bodies outside the screen's beneficiary set — corroborate that the budget deadline is real and binding. Whether the screen still serves that problem or now functions as coalition boundary maintenance is disputed: advocacy organizations attest it remains essential triage, while grid-operator reliability filings, nuclear-industry submissions, and a strand of independent energy-system literature attest the binding constraint has shifted toward firm capacity and integration cost, with the screen persisting past its original justification.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__velocity_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__velocity_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__velocity_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(technology_legitimacy_kernel__velocity_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__velocity_primacy_reading, 0.63, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_legitimacy_kernel__velocity_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_legitimacy_kernel__velocity_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_legitimacy_kernel__velocity_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.63 at interval end) reflects a real but bounded transfer: the screen redirects legitimacy, finance, and policy priority away from schedule-disqualified low-carbon supply and leaves integration costs unattributed, while its timing arithmetic retains genuine force under budget scarcity — hence below snare-range values. Suppression (0.55) is discursive-institutional rather than physical: counter-criteria are marginalized in mainstream fora, financing withdraws, and coalition boundaries are policed, but rival readings stay legally and scientifically publishable. Theater (0.38) rises as velocity rhetoric outpaces delivered buildout — pledge-performance gaps and scorecard rituals increasingly substitute for deployment accounting. Accessibility collapse (0.48) is moderate: inside the screen's frame, advocacy for slow technologies is pre-answered, yet the rival readings remain live outside it and several jurisdictions build disfavored technologies regardless. Resistance (0.58) is sustained: reliability testimony, nuclear restart programs, and technology-neutral legislation contest the screen continuously. All three tracked series run on one shared six-point grid (2015-2025); suppression_requirement is authored because the story specifically traces enforcement-machinery change — taxonomy codification and screen hardening through 2021, then partial relaxation as contestation grew.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical facts. From the advocacy seat the screen is triage under scarcity — refusing it wastes the budget; from the nuclear and developer seats it is pre-judgment that ends evaluation before evidence; from the grid-operator seat it is an unfunded mandate that assigns integration costs elsewhere; from the financier seat it is welcome simplicity. Same power atoms diverge by exit structure: ESG finance holds arbitrage-grade exit (rewrite the mandate), while grid operators hold none (the reliability obligation is inalienable), so identical institutional standing yields different experienced severity.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (manufacturers, developers, advocacy organizations, green finance, gas backup producers) derive low directionality — the screen subsidizes them. Declared victims (nuclear industry, advanced developers, grid operators, ratepayers) derive high directionality — the screen transfers from them, amplified for trapped exits (grid operators, ratepayers, mid-certification developers) relative to mobile ones. Gas producers warrant note: their benefit is real but second-order and unacknowledged, so the structural derivation correctly prices them as beneficiaries even though the screen's own framing never claims them. Ratepayers are dual-positioned (cheap energy at high-output hours, socialized integration costs in the bill); the victim declaration dominates their derivation, and the offset is carried qualitatively rather than by override, since overrides key on power atoms that would sweep unintended seats. Ratepayers' individual powerlessness is mitigated only through aggregated consumer-advocacy coalitions in tariff proceedings — a latent coalition channel the screen's beneficiaries do not face.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the screen as pure coordination would erase the asymmetry — the pre-disqualification of firm low-carbon rivals and the unattributed integration costs are real transfers, not overhead. Reading it as pure extraction would erase the genuine arithmetic — under a binding budget, deployability is a legitimate selection property, and the screen did solve a real post-Copenhagen allocation problem. The tangled-rope classification holds both: coordination function live, extraction layered on top. On genealogy: the founding problem (allocation under deadline) is materially live — budgets remain binding — but its application has drifted toward boundary maintenance, hence founding_problem_status 'contested' rather than 'dead'; the mismatch consumer therefore sees no dead-problem/world-rearranges capture flag, while the drift is documented in the measurement series and the selectivity omega.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_split,
    'Which gating property decides mitigation legitimacy — deployability within the budget timeline (this file), dispatchable baseload capability (reliability_primacy_reading), or bounded generation-reversible failure modes (precautionary_reading)?',
    'Not resolvable internally; resolvable only by observing which criterion actually governs capital-allocation and policy decisions jurisdiction by jurisdiction over time.',
    'Adopting the reliability reading moves nuclear into the beneficiary set and casts variable-heavy portfolios as the risk carrier; adopting the precautionary reading moves mining-scaleup and novel-chemistry deployment to the risk side; epsilon, victim sets, and classification change wholesale across the family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_split, conceptual, 'Committer structure: this constraint is one reading of technology_legitimacy_kernel; the disagreement is located in the gating criterion itself.').

omega_variable(
    criterion_selectivity_audit,
    'Is the deployability test applied uniformly to all slow components — including transmission buildout, workforce training, and permitting reform, themselves decade-scale undertakings — or selectively, to exclude rival supply technologies?',
    'Comparative audit of exemption patterns across deployability scorecards, taxonomy rules, and subsidy statutes: do slow enabling infrastructure and slow rival supply receive the same scrutiny?',
    'Uniform application supports the coordination-dominant (rope-leaning) reading; selective application establishes boundary maintenance as the operative function, raising effective extraction and pushing toward the snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(criterion_selectivity_audit, empirical, 'Whether the velocity screen is a neutral timing filter or a selectively applied coalition boundary.').

omega_variable(
    integration_cost_incidence,
    'Are intermittency integration costs attributed back to the generation portfolio that creates them, or socialized across all tariffs?',
    'Ancillary-service, curtailment, and network-reinforcement cost attribution studies across jurisdictions with high variable-renewable shares.',
    'Socialization means the burden on grid operators and ratepayers exceeds what base epsilon implies; full attribution would internalize the cost and likely soften the measured asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_cost_incidence, empirical, 'Whether the screen''s transfer onto grid seats is larger than the headline metric suggests.').

omega_variable(
    self_fulfilling_timeline_exclusion,
    'Is new nuclear''s slowness an intrinsic property, or partly produced by the legitimacy gate itself — capital starvation causing lost learning and longer builds — making the criterion circular?',
    'Compare construction timelines in jurisdictions inside versus outside the screen''s influence (Gulf and East Asian builds versus OECD attempts), controlling for design novelty and regulation.',
    'If the slowness is substantially produced, the exclusion''s justification weakens and the extraction attributable to the screen rises; if intrinsic, the timing logic stands and the screen''s coordination claim strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_fulfilling_timeline_exclusion, empirical, 'Whether the screen''s core factual premise about nuclear timelines is independent of the screen''s own operation.').

omega_variable(
    kernel_authority_framing_underdetermination,
    'Is the kernel best framed as distributed (no adjudicator; readings compete in discourse) or as formalized under expertise (scenario bodies such as the IEA and IPCC as de facto adjudicators)?',
    'Examine whether any body claims or exercises settlement authority over the kernel, or whether scenario outputs function as evidence cited by all sides rather than rulings obeyed by any.',
    'A formalized-expertise framing would license interpretation_layer_present=true, recast scenario bodies as interpretive authorities, and change contamination-path analysis across the family; the distributed framing used here leaves no designated interpreter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_authority_framing_underdetermination, conceptual, 'CS-framing under-determination: signals (no body claims settlement authority; scenarios cited as evidence by all factions) guided the distributed choice, but the alternative framing is coherent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__velocity_primacy_reading, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tlk_velocity_tr_t2015, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2015, 0.18).
narrative_ontology:measurement_basis(tlk_velocity_tr_t2015, observed).
narrative_ontology:measurement(tlk_velocity_tr_t2017, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2017, 0.22).
narrative_ontology:measurement_basis(tlk_velocity_tr_t2017, observed).
narrative_ontology:measurement(tlk_velocity_tr_t2019, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2019, 0.27).
narrative_ontology:measurement_basis(tlk_velocity_tr_t2019, observed).
narrative_ontology:measurement(tlk_velocity_tr_t2021, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2021, 0.32).
narrative_ontology:measurement_basis(tlk_velocity_tr_t2021, observed).
narrative_ontology:measurement(tlk_velocity_tr_t2023, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2023, 0.36).
narrative_ontology:measurement_basis(tlk_velocity_tr_t2023, observed).
narrative_ontology:measurement(tlk_velocity_tr_t2025, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2025, 0.38).
narrative_ontology:measurement_basis(tlk_velocity_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(tlk_velocity_be_t2015, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement_basis(tlk_velocity_be_t2015, observed).
narrative_ontology:measurement(tlk_velocity_be_t2017, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2017, 0.48).
narrative_ontology:measurement_basis(tlk_velocity_be_t2017, observed).
narrative_ontology:measurement(tlk_velocity_be_t2019, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2019, 0.56).
narrative_ontology:measurement_basis(tlk_velocity_be_t2019, observed).
narrative_ontology:measurement(tlk_velocity_be_t2021, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2021, 0.62).
narrative_ontology:measurement_basis(tlk_velocity_be_t2021, observed).
narrative_ontology:measurement(tlk_velocity_be_t2023, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2023, 0.64).
narrative_ontology:measurement_basis(tlk_velocity_be_t2023, observed).
narrative_ontology:measurement(tlk_velocity_be_t2025, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2025, 0.63).
narrative_ontology:measurement_basis(tlk_velocity_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(tlk_velocity_su_t2015, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2015, 0.4).
narrative_ontology:measurement_basis(tlk_velocity_su_t2015, observed).
narrative_ontology:measurement(tlk_velocity_su_t2017, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2017, 0.47).
narrative_ontology:measurement_basis(tlk_velocity_su_t2017, observed).
narrative_ontology:measurement(tlk_velocity_su_t2019, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2019, 0.54).
narrative_ontology:measurement_basis(tlk_velocity_su_t2019, observed).
narrative_ontology:measurement(tlk_velocity_su_t2021, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2021, 0.6).
narrative_ontology:measurement_basis(tlk_velocity_su_t2021, observed).
narrative_ontology:measurement(tlk_velocity_su_t2023, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2023, 0.58).
narrative_ontology:measurement_basis(tlk_velocity_su_t2023, observed).
narrative_ontology:measurement(tlk_velocity_su_t2025, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2025, 0.55).
narrative_ontology:measurement_basis(tlk_velocity_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__velocity_primacy_reading, resource_allocation).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, reliability_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, precautionary_reading).

% DUAL FORMULATION NOTE:
% Family decomposition of the colloquial label 'legitimate climate technology': the label conflates three structurally distinct gating criteria with different beneficiary/victim sets and different epsilon. This file carries the velocity criterion alone; the reliability and precautionary files carry theirs. Operational upstream/downstream structure: the velocity reading sits upstream of the reliability reading (its buildout outcomes become the reliability reading's problem load), which is why the influence edge runs velocity-to-reliability. Epsilon differs across the family because the arrangements differ — no observable-selection parameter mediates within any single file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
