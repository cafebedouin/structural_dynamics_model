% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__portfolio_optimization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_imperative__portfolio_optimization_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_mitigation_imperative__portfolio_optimization_reading
 *   human_readable: Climate Mitigation Imperative — Portfolio Optimization Reading (Maximize All Low-Carbon Sources; Nuclear Baseload Necessity)
 *   domain: energy policy/climate mitigation/technology governance
 *
 * SUMMARY:
 *   The portfolio_optimization_reading of the climate mitigation imperative
 *   holds that deep decarbonization requires maximizing every available
 *   low-carbon source simultaneously, with nuclear power specifically
 *   necessary to provide reliable firm capacity while variable renewables
 *   scale. As an operating arrangement it takes the form of clean electricity
 *   standards that count nuclear toward compliance, production and investment
 *   tax credits, zero-emission credit rescue programs for merchant reactors,
 *   capacity market rules that remunerate firm low-carbon capacity, and loan
 *   guarantees for new builds. The arrangement coordinates a genuine
 *   collective-action problem — decarbonizing grids that must stay reliable —
 *   while directing a protected revenue stream to specific industries and
 *   imposing exclusion costs on fossil generation and transition costs on its
 *   communities. This story instantiates ONE reading of a contested kernel:
 *   the opportunity_cost_reading (fastest abatement per dollar; nuclear
 *   net-harmful) and the systems_transition_reading (decentralization and
 *   democratic control; nuclear as extractive centralization) are separate
 *   constraints with their own ε, linked via network.affects_constraints.
 *   Claim and metrics are authored independently: the claimed type is
 *   tangled_rope from the structural reading (genuine coordination function
 *   plus asymmetric extraction plus active enforcement); the metrics describe
 *   the arrangement's actual operation as this reading assesses it.
 *
 * KEY AGENTS:
 *   - nuclear_industry: primary beneficiary (institutional/constrained) — collects tax credits, zero-emission credits, capacity payments, and loan guarantees; plants cannot relocate and closure means write-off, so its stake in the arrangement's maintenance is existential
 *   - renewable_energy_industry: secondary beneficiary (organized/mobile) — the technology-neutral mandate guarantees demand for wind, solar, and storage; capital is redeployable across jurisdictions
 *   - fossil_fuel_generators: primary target (powerful/constrained) — excluded from sanctioned portfolios, facing stranded assets and compliance costs; sunk capital cannot move
 *   - coal_and_gas_communities: secondary target (organized/trapped) — bear plant closures and transition costs concentrated in single-industry regions
 *   - electricity_ratepayers: diffuse payer with service benefit (moderate/constrained) — carry subsidy recovery through bills while receiving the decarbonizing grid
 *   - energy_poor_households: excluded voice (powerless/constrained) — bear regressive bill burdens but lack standing in the dockets where allocation is decided
 *   - energy_regulators_and_legislatures: agenda setter (institutional/arbitrage) — write portfolio standards, authorize subsidies, and set capacity market rules; can amend the arrangement at political cost
 *   - grid_reliability_engineers: analytical observer — run the adequacy studies that both the necessity claim and its challengers cite; collect no rents from either answer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__portfolio_optimization_reading, 0.48).
domain_priors:suppression_score(climate_mitigation_imperative__portfolio_optimization_reading, 0.5).
domain_priors:theater_ratio(climate_mitigation_imperative__portfolio_optimization_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__portfolio_optimization_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__portfolio_optimization_reading, "Climate Mitigation Imperative — Portfolio Optimization Reading (Maximize All Low-Carbon Sources; Nuclear Baseload Necessity)").
narrative_ontology:topic_domain(climate_mitigation_imperative__portfolio_optimization_reading, "energy policy/climate mitigation/technology governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__portfolio_optimization_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, renewable_energy_industry).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_generators).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, coal_and_gas_communities).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, energy_poor_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, electricity_ratepayers).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, electricity_ratepayers).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__portfolio_optimization_reading, portfolio_diversification_reduces_mitigation_cost).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__portfolio_optimization_reading, firm_low_carbon_capacity_required_for_reliability).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__portfolio_optimization_reading, technology_neutral_carbon_intensity_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the existing reactor fleet and develops new units. Collects production tax credits, state zero-emission credit payments, capacity market revenues, and federal loan guarantees; clean electricity standards count its output toward compliance. Several merchant plants became economically viable only through these supports. Plants are licensed for decades at fixed sites with dedicated workforces and cannot relocate; the alternative to the support architecture is case-by-case closure.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_industry, beneficiary,
    institutional, generational, constrained, national).

% Manufactures and deploys wind, solar, and storage. The all-sources mandate and associated tax credits guarantee demand and improve bankability; portfolio obligations create a compliance market for its output. Capital and crews redeploy across jurisdictions and policy regimes, so its attachment is to the mandate's existence rather than to any single statute.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, renewable_energy_industry, beneficiary,
    organized, biographical, mobile, global).

% Own coal and gas fleets that face exclusion from sanctioned portfolios, emissions compliance costs, and write-downs as portfolio standards tighten. Some units pivot to capacity-only operation or retrofit capture; others litigate and lobby against the mandates. Plant capital is sunk and cannot move; exit means booking losses.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_generators, payer,
    powerful, biographical, constrained, national).

% Live in single-industry regions where plant and mine closures remove the tax base and the dominant employer simultaneously. Transition funds arrive smaller and later than the losses. Housing and skills are locally immobile, so the closure decision is effectively made for them elsewhere.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, coal_and_gas_communities, payer,
    organized, biographical, trapped, regional).

% Pay subsidy recovery charges and system costs through bills and taxes while receiving a decarbonizing, reliability-managed grid. Their participation runs through rate cases and consumer advocates with limited technical standing; most cannot practically exit grid service.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, electricity_ratepayers, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__portfolio_optimization_reading, electricity_ratepayers, beneficiary).

% Carry the highest bill-burden share from recovery charges and system upgrades. They rarely intervene in the dockets where allocation and rate design are decided, lack technical representation, and experience the arrangement primarily as a bill line they did not vote on.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, energy_poor_households, excluded,
    powerless, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__portfolio_optimization_reading, energy_poor_households, payer).

% Write portfolio standards, authorize subsidy programs, and set capacity market rules in response to reliability concerns, industry testimony, and climate commitments. Amending or unwinding the arrangement is within their legal power but carries concentrated political cost with nuclear-employment constituencies and reliability-risk headlines.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, energy_regulators_and_legislatures, agenda_setter,
    institutional, biographical, arbitrage, national).

% Run the production-cost, adequacy, and stability studies that both the necessity claim and its challengers cite. Their findings shift with modeling assumptions and input costs; they collect no rents from either answer and publish under professional norms of disclosure.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, grid_reliability_engineers, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of deep, reliable decarbonization by committing jurisdictions to deploy the full set of low-carbon technologies at once — diversifying technology, supply-chain, and siting risk, and provisioning firm capacity while variable renewables scale — rather than betting the mitigation effort on any single pathway whose failure would be unrecoverable inside the climate timeline.
% TRANSFER_FUNCTION: Moves public revenue (tax credits, zero-emission credit payments, loan guarantees) and guaranteed compliance market share (portfolio standards, capacity remuneration) from taxpayers and ratepayers to low-carbon generators — disproportionately to nuclear through the firm-capacity clause — while excluding unabated fossil generation from sanctioned portfolios and stranding its capital.
% ABSENT_VOICES: Energy-poor households would object to rate designs that socialize nuclear support through regressive recovery charges but lack standing and technical representation in integrated resource planning and capacity market dockets. Communities hosting waste and new siting decisions enter late and weakly. Proponents of nuclear-free pathway designs are present in advocacy and literature but excluded from several official planning bodies where portfolio assumptions are set. Fossil-fuel labor appears mainly as a transition afterthought rather than a seat at allocation.
% DISAPPEARANCE_RATIONALE: Portfolio mandates would lapse and compliance accounting would lose nuclear as a counted source; merchant reactors currently surviving on zero-emission credits and capacity payments would face closure decisions within their fuel cycles; deployment would concentrate in the lowest-cost abatement options; capacity market prices and fossil retirement timing would shift; and the reliability insurance the constraint purchases would have to be replaced by storage, transmission, and demand flexibility — or the transition would carry more unabated fossil for longer. Jobs, plant towns, subsidy flows, and grid buildout plans all reorganize.
% FOUNDING_PROBLEM: Deep decarbonization at the required scale and speed, on grids that must remain reliable throughout: early in the mitigation era, analyses worried that variable renewables alone could not keep systems stable at high penetration, that excluding any available low-carbon option would raise total cost and slow deployment beyond climate tolerances, and that a single-technology bet was an unaffordable risk.
% FOUNDING_PROBLEM_CORROBORATION: IPCC assessment reports and independent system-operator adequacy studies (NERC reliability assessments, ENTSO-E adequacy analyses) attest from outside the benefiting parties that the decarbonization problem remains live and that the firm-capacity question is unresolved. No party outside the beneficiary set attests that nuclear specifically is necessary — that claim's corroboration is contested inside the engineering literature itself, which is the live edge of this reading.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__portfolio_optimization_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__portfolio_optimization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__portfolio_optimization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_imperative__portfolio_optimization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__portfolio_optimization_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_imperative__portfolio_optimization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_imperative__portfolio_optimization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_imperative__portfolio_optimization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.48: assessed by this reading's own lights, most of the arrangement is endorsed mitigation — portfolio diversification and firm low-carbon capacity have real system value — but the support architecture exceeds demonstrable mitigation value in identifiable places (zero-emission credit packages sized to plant rescue rather than avoided emissions; capacity premiums decoupled from reliability contribution), and the exclusion of fossil generation, while the reading's goal, is structurally a defined group bearing the arrangement's costs. Suppression 0.50 is authored as a raw structural property and is deliberately not scaled by power or scope: the arrangement excludes unabated fossil from sanctioned portfolios and forecloses nuclear-free pathway designs inside official planning, but it operates through ordinary legislation and rulemaking that remains democratically contestable. Theater 0.28 and rising: deployment is real, but the necessity rhetoric has outrun construction — the maximize-all-sources framing is invoked far more often than plants are built, and a growing share of the arrangement's public defense is performance of reliability anxiety rather than engineering. Accessibility collapse 0.30: alternatives are demonstrably not collapsed — the sibling readings are live, nuclear-free pathway studies are published and cited, and several jurisdictions have exited nuclear while decarbonizing. Resistance 0.65: fossil incumbents litigate and lobby, nuclear critics contest the necessity claim, and ratepayer advocates contest cost allocation. The measurement series run on one shared grid (points 0/5/10/15/20/25) with all three metrics authored at every point. The suppression_requirement series is authored because the story specifically traces enforcement-capacity build-up: compliance machinery hardened across the interval (state clean energy standard adoption, zero-emission credit statutes, capacity market redesign) — an enforcement story, not merely an extraction shift. Fixing cost is prohibitive for the agenda-setter: the arrangement can be amended legally, but repeal requires overcoming concentrated nuclear-employment constituencies and reliability-risk politics; the historical record shows unwinding has occurred only under scandal-level impetus, and the agenda-setter does not itself bear the subsidy costs that fixing would save.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different types from the same structure. From the nuclear industry's seat the arrangement is survival: without the firm-capacity clause and the credits, merchant reactors close, so it presents as indispensable coordination the industry did not build but depends on. From the fossil generator's seat the same structure is expropriation of sunk capital by rule. From the ratepayer's seat it is a bill carrying an unpriced reliability insurance policy — cost now, benefit diffuse and deferred. From the regulator's seat it is a defensible portfolio hedge against technology risk. The engine computes these divergences from the structural data (power, exit, role); the divergence between the beneficiary seats' rope-experience and the target seats' snare-experience is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. nuclear_industry sits near the beneficiary end: it collects the rents the baseload clause exists to secure, and its constrained exit (licensed, sited, unionized plants that cannot move) amplifies its stake in maintenance rather than any extraction it bears. renewable_energy_industry also sits near the beneficiary end with mobile exit — it captures the technology-neutral mandate without needing the necessity clause. fossil_fuel_generators sit near the target end: exclusion and stranded-asset costs concentrate on them, with only partial pivots (gas bridge, retrofit capture) available. coal_and_gas_communities sit near the target end with trapped exit — immobile housing and non-transferable skills. electricity_ratepayers sit near symmetric: they pay the subsidy recovery and receive the decarbonized, reliability-managed grid; the net position is genuinely ambiguous, which is why the seat carries payer with secondary beneficiary. energy_poor_households are declared victims on the cost side (regressive recovery charges) while their defining structural position is absence from the allocation conversation. energy_regulators_and_legislatures hold the agenda-setter seat and are declared neither beneficiary nor victim: they administer the extraction but do not collect it — gain_flow names nuclear_industry, not the regulator seat. Note on gain_flow: the arrangement's gains split across the low-carbon portfolio, but the seat this reading distinctively elevates — and whose marginal, contested rents (zero-emission credits, capacity premiums, life extensions) the necessity clause defends — is nuclear; renewables co-capture the technology-neutral mandate that any mitigation reading would provide.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim prevents mislabeling in both directions. A pure-rope reading would miss that the baseload clause directs protected revenue to a specific industry beyond demonstrable mitigation value — the asymmetry is real and the enforcement architecture (compliance mandates plus subsidy statutes) is what holds it. A pure-snare reading would miss the genuine collective-action core: decarbonization is a real coordination problem, portfolio diversification has real risk value, and the cost-bearing groups are not the arrangement's intended targets in the way a pure extraction scheme's are. On mandatrophy: the founding problem — reliable deep decarbonization — is live, corroborated from outside the beneficiary set by IPCC assessments and independent system-operator adequacy studies, so the R5 mismatch consumer sees founding_problem_status=live with disappearance_verdict=world_rearranges and no zombie flag fires. The trajectory to watch is the theater series: necessity rhetoric rising while nuclear buildout stagnates is the signature of a mandate drifting from deploy-all-sources toward preserve-existing-plants-and-the-claim-itself — if support persists while construction does not materialize, the arrangement completes a mandatrophy arc from coordination toward subsidy preservation, and the classification should be re-examined at that point.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_position_in_mitigation_kernel,
    'This constraint is the portfolio_optimization_reading of the climate_mitigation_imperative kernel; how would instantiating a sibling reading restructure the constraint''s beneficiary and victim sets?',
    'Author and compile the sibling stories (opportunity_cost_reading, systems_transition_reading) and compare computed per-seat classifications; the disagreement is located at the allocation rule governing which low-carbon sources are deployed, not at the mitigation imperative itself.',
    'Under opportunity_cost_reading, nuclear moves from beneficiary to net cost-bearer (capital intensity and timeline make support net-harmful per that reading) and ε for the nuclear-support architecture rises sharply; under systems_transition_reading, the beneficiary set contracts to decentralized actors, centralized generators broadly become extraction targets, and the victim set expands to include democratic-control deficits. Classification of this same policy surface would shift across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_position_in_mitigation_kernel, conceptual, 'Committer-frame routing: one reading of a three-reading kernel; sibling readings change the beneficiary/victim structure and ε over the same policy surface.').

omega_variable(
    firm_capacity_substitutability,
    'Is firm low-carbon capacity of the kind nuclear provides actually necessary for reliable deep decarbonization, or can storage, demand response, transmission expansion, and overbuild substitute at acceptable cost?',
    'Production-cost and adequacy modeling at high variable-renewable shares, plus observed operation of high-renewables systems; track whether reliability shortfalls in practice trace to missing firm capacity or to market and regulatory design choices.',
    'If substitutable, the baseload-necessity clause is preference rather than engineering necessity, the constraint''s extraction component rises, and this reading drifts toward the opportunity_cost sibling; if necessary, the clause is load-bearing coordination and the rope component dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(firm_capacity_substitutability, empirical, 'The empirical core of the necessity claim that distinguishes this reading from its siblings.').

omega_variable(
    subsidy_vs_incumbent_rescue,
    'Do nuclear support programs price mitigation value (avoided emissions valued at social cost of carbon) or rescue incumbent merchant plants facing gas-and-renewables competition?',
    'Audit zero-emission credit and production tax credit award justifications against modeled cost per tonne abated versus competing abatement spending; compare supported plants'' avoided-emissions value to their support levels.',
    'If rescue dominates, the constraint''s effective extraction is higher than the base measure suggests and its classification trends toward the snare end of the tangled-rope range; if mitigation-priced, the rope component dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidy_vs_incumbent_rescue, empirical, 'Whether the support architecture tracks climate value or incumbent protection.').

omega_variable(
    allocation_rule_boundary,
    'Where does this reading''s maximize-all-low-carbon-sources rule stop — does it bind at any cost, or only for sources clearing a cost-effectiveness threshold, and who draws that line?',
    'Statutory and regulatory text of portfolio standards and subsidy authorizations: whether they contain cost caps, alternative-compliance payments, or unbounded deployment mandates.',
    'An unbounded reading maximizes the beneficiary set and the extraction surface; a cost-capped reading converges toward the opportunity_cost sibling and shrinks nuclear''s protected position.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(allocation_rule_boundary, conceptual, 'Under-determination in the reading''s own allocation rule; the sibling readings occupy the boundary positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__portfolio_optimization_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmi_portfolio_opt_tr_t0, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(cmi_portfolio_opt_tr_t5, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(cmi_portfolio_opt_tr_t10, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(cmi_portfolio_opt_tr_t15, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(cmi_portfolio_opt_tr_t20, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(cmi_portfolio_opt_tr_t25, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(cmi_portfolio_opt_be_t0, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cmi_portfolio_opt_be_t5, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 5, 0.36).
narrative_ontology:measurement(cmi_portfolio_opt_be_t10, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(cmi_portfolio_opt_be_t15, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 15, 0.43).
narrative_ontology:measurement(cmi_portfolio_opt_be_t20, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 20, 0.46).
narrative_ontology:measurement(cmi_portfolio_opt_be_t25, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 25, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(cmi_portfolio_opt_su_t0, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement(cmi_portfolio_opt_su_t5, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(cmi_portfolio_opt_su_t10, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(cmi_portfolio_opt_su_t15, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 15, 0.46).
narrative_ontology:measurement(cmi_portfolio_opt_su_t20, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(cmi_portfolio_opt_su_t25, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 25, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__portfolio_optimization_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative__opportunity_cost_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative__systems_transition_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the climate_mitigation_imperative kernel decomposes into three readings with distinct ε over the same policy surface. This story (portfolio_optimization_reading) authors ε≈0.48 for the standing all-sources portfolio arrangement as this reading sees it: mostly-endorsed coordination with recognized subsidy rents. The opportunity_cost_reading authors high ε for nuclear's position within that surface (capital intensity and timeline make support net-harmful by its lights); the systems_transition_reading authors high ε for the centralization this reading subsidizes. The upstream claim shared by all three — the mitigation imperative itself — is cited as authority by each downstream reading; this story links both siblings via affects_constraints, and the sibling files should link back.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
