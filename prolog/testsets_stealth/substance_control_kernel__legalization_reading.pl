% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__legalization_reading, []).

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
 *   constraint_id: substance_control_kernel__legalization_reading
 *   human_readable: Legalization Regime: Liberty Baseline with Externality Capture and Excise Taxation
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This story instantiates the legalization reading of the
 *   substance_control_kernel as a clean, epsilon-invariant constraint:
 *   substance use is treated as individual liberty, the state's warrant is
 *   confined to preventing third-party harm and capturing externality costs,
 *   and the operative arrangement is a licensed market with excise taxation
 *   and externality-focused enforcement. The epsilon referent is the standing
 *   legalized arrangement itself — the licensing regime, the tax structure,
 *   the externality rules — assessed by this reading's own lights: users are
 *   liberty-exercisers, not victims; third parties bear real uncompensated
 *   costs; the state collects. The claim/metric gap is deliberate: the
 *   reading CLAIMS a liberty-preserving coordination regime while the
 *   authored metrics describe moderately extractive operation with rising
 *   drift — the engine measures that divergence rather than the author
 *   reconciling it.
 *
 * KEY AGENTS:
 *   - - adult_substance_users: Primary beneficiary (organized/mobile) — regained liberty and legal access; pays embedded excise but retains exit by abstention or substitution
 *   - - licensed_producers_and_retailers: Secondary beneficiary (powerful/constrained) — collects market margin; locked in by license-specific capital
 *   - - state_revenue_and_regulatory_agencies: Agenda setter and fiscal collector (institutional/constrained) — writes the rules, collects the taxes, increasingly bound by its own revenue dependence
 *   - - road_users_exposed_to_impaired_drivers: Primary target (powerless/trapped) — bears crash externalities they cannot decline
 *   - - secondhand_smoke_bystanders: Primary target (powerless/constrained) — absorbs involuntary exposure in shared air
 *   - - communities_disproportionately_enforced_against: Dual-positioned payer/beneficiary (moderate/constrained) — residual enforcement concentration and licensing exclusion, offset partly by statutory reinvestment
 *   - - legacy_illicit_market_operators: Excluded party (organized/trapped) — displaced from the legal market and barred from joining it
 *   - - public_health_surveillance_agencies: Analytical observer (institutional/analytical) — produces the externality evidence the regime's justification rests on
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__legalization_reading, 0.54).
domain_priors:suppression_score(substance_control_kernel__legalization_reading, 0.36).
domain_priors:theater_ratio(substance_control_kernel__legalization_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, extractiveness, 0.54).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, suppression_requirement, 0.36).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_kernel__legalization_reading, "Legalization Regime: Liberty Baseline with Externality Capture and Excise Taxation").
narrative_ontology:topic_domain(substance_control_kernel__legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__legalization_reading, '636d67b9-d130-4293-9678-1516f6a447b2').
narrative_ontology:cs_kernel_codification('636d67b9-d130-4293-9678-1516f6a447b2', formalized).
narrative_ontology:cs_authority_grounding('636d67b9-d130-4293-9678-1516f6a447b2', lineage).
narrative_ontology:cs_interpretation_layer_present('636d67b9-d130-4293-9678-1516f6a447b2').
narrative_ontology:cs_reading_relation('636d67b9-d130-4293-9678-1516f6a447b2', substance_control_kernel__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('636d67b9-d130-4293-9678-1516f6a447b2', substance_control_kernel__harm_reduction_reading, influences).
narrative_ontology:cs_axiom('636d67b9-d130-4293-9678-1516f6a447b2', foundational, self_regarding_use_beyond_state_purview).
narrative_ontology:cs_axiom_status(self_regarding_use_beyond_state_purview, holdable).
narrative_ontology:cs_axiom_grounding('636d67b9-d130-4293-9678-1516f6a447b2', self_regarding_use_beyond_state_purview, deontological).
narrative_ontology:cs_axiom('636d67b9-d130-4293-9678-1516f6a447b2', foundational, externality_capture_legitimates_intervention).
narrative_ontology:cs_axiom_status(externality_capture_legitimates_intervention, holdable).
narrative_ontology:cs_axiom_grounding('636d67b9-d130-4293-9678-1516f6a447b2', externality_capture_legitimates_intervention, instrumental).
narrative_ontology:cs_reference_frame('636d67b9-d130-4293-9678-1516f6a447b2', harm_principle_liberty_baseline).
narrative_ontology:cs_drift_state('636d67b9-d130-4293-9678-1516f6a447b2', contemporary_post_legalization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('636d67b9-d130-4293-9678-1516f6a447b2', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__legalization_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, adult_substance_users).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, licensed_producers_and_retailers).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, state_revenue_and_regulatory_agencies).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, road_users_exposed_to_impaired_drivers).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, secondhand_smoke_bystanders).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, communities_disproportionately_enforced_against).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, communities_disproportionately_enforced_against).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Purchase and consume regulated products through licensed channels without criminal exposure; choose among products, potencies, and consumption venues; carry the excise tax embedded in retail prices and the conduct rules (age gates, location restrictions, per-se driving limits) attached to legal purchase. Exit from the arrangement means abstaining or substituting — an option most retain, and voting blocs in legalization jurisdictions have repeatedly defended the arrangement at the ballot.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, adult_substance_users, beneficiary,
    organized, biographical, mobile, national).

% Hold licenses obtained through application, fees, security buildout, and compliance investment; sell into a market whose boundaries, potency limits, and advertising rules the regulator redraws; lobby continuously on tax rates and licensing expansion. License value and facility capital are recoverable only inside the regime, so exit means writing off sunk compliance costs.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, licensed_producers_and_retailers, beneficiary,
    powerful, generational, constrained, national).

% Write licensing rules, set and collect excise taxes, operate product-testing and age-verification regimes, and fund enforcement of the third-party-harm rules (impaired driving, public use, sales to minors). Schools, treatment programs, and community reinvestment grants have been built on the revenue stream, which narrows the realistic option of unwinding the arrangement or cutting rates sharply.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, state_revenue_and_regulatory_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Share roads with drivers impaired by now-legal substances; cannot opt out of road exposure and did not individually accept the crash risk the legal market generates; bear the residual risk left after per-se limits and enforcement, and receive compensation only through liability litigation after harm occurs.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, road_users_exposed_to_impaired_drivers, payer,
    powerless, immediate, trapped, national).

% Inhale smoked or vaporized product in multi-unit housing, sidewalks, patios, and shared courtyards where consumption is permitted; can move units or petition landlords and councils but cannot seal shared air; children and people with respiratory conditions absorb the highest exposures.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, secondhand_smoke_bystanders, payer,
    powerless, biographical, constrained, local).

% Live where residual enforcement (public-use citations, retail clustering, checkpoint placement) concentrates and where licensed outlets cluster densely; many residents carry prohibition-era records that disqualify them from the licenses the new economy sells; statutory reinvestment grants funded by the excise flow back into some of these neighborhoods.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, communities_disproportionately_enforced_against, payer,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__legalization_reading, communities_disproportionately_enforced_against, beneficiary).

% Built distribution networks, supplier relationships, and customer bases under prohibition; are barred from licensure by felony-history rules and capital requirements; are undercut on price, consistency, and legality in fully legal markets and retreat to gray niches — high-tax price gaps, exports to dry jurisdictions — where enforcement still finds them.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, legacy_illicit_market_operators, excluded,
    organized, immediate, trapped, regional).

% Track emergency-department presentations, impaired-driving fatality toxicology, youth-use survey series, and treatment admissions; publish the externality data on which the regime's own justification depends; advise legislatures on per-se thresholds and potency policy without setting them.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, public_health_surveillance_agencies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_kernel__legalization_reading, state_revenue_and_regulatory_agencies).
narrative_ontology:fixing_cost_class(substance_control_kernel__legalization_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaces fragmented illicit supply chains with licensed, inspected, quality-controlled production; channels demand into a taxable market; and concentrates externality management (impaired-driving enforcement, age verification, potency labeling, consumption-location rules) in one regulatory apparatus instead of dispersing it across criminal enforcement.
% TRANSFER_FUNCTION: Moves money from consumers to state treasuries via excise and sales taxes and licensing fees; moves margin from illicit distributors to licensed firms and the state; moves enforcement risk off users and onto third-party-harm violations and unlicensed operators.
% ABSENT_VOICES: People with compulsive use patterns are seated as ordinary consumers — their purchases taxed like any other and their care funded, if at all, from discretionary general revenue rather than guaranteed — and would object to that framing. Record-holders barred from licensure by the regime's own eligibility rules are absent from the licensing conversations that allocate the new economy. Residents of retail-clustered blocks rarely appear at zoning hearings dominated by industry and treasury interests.
% DISAPPEARANCE_RATIONALE: Overnight repeal would collapse licensed firms holding sunk compliance capital, sever earmarked school/treatment/reinvestment funding mid-stream, push supply back into illicit networks within weeks, and return hundreds of thousands of users to criminal-justice exposure — the entire legal-market economy, the fiscal programs built on it, and the enforcement architecture would reorganize around prohibition again.
% FOUNDING_PROBLEM: Prohibition had produced mass criminalization of users, violent and unaccountable supply chains, unregulated product, and racially concentrated enforcement, while genuine third-party harms (impaired driving, youth access, involuntary exposure) still required state management — the founding problem was securing users' liberty while preventing and pricing harms imposed on others.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: traffic-safety researchers' impaired-driving fatality series, public-health surveillance of emergency presentations and youth use, and criminal-justice reform organizations' decarceration and arrest-disparity data all attest both the founding failure of prohibition and the continuing reality of third-party harms. Industry associations and treasury offices attest the same genealogy, but the corroborating sources named above stand independent of them.
narrative_ontology:disappearance_verdict(substance_control_kernel__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__legalization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__legalization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_kernel__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__legalization_reading, 0.54, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__legalization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_kernel__legalization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_kernel__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.54) and rising: the core structure respects liberty, but third parties bear uncompensated externalities, excise rates in mature markets have climbed past credible externality-cost estimates, and tax incidence falls on consumption regardless of ability to pay. Suppression is moderate-low (0.36): enforcement machinery exists and is actively maintained (age gates, per-se enforcement, unlicensed-market operations) but nothing comparable to prohibition's coercive apparatus, and the suppression_requirement series is authored precisely because enforcement capacity changed over the interval — an early peak during gray-market suppression campaigns followed by decay as the licit market displaced illicit supply. Theater ratio (0.31) reflects accumulating performative layers (compliance signage regimes, public-health messaging campaigns, licensing ceremony) around a still-real regulatory core. Accessibility collapse is low (0.38): gray markets, home cultivation where permitted, cross-border arbitrage, and abstention all persist as alternatives, and the regime does not foreclose them. Resistance (0.42) is moderate: prohibitionist holdouts, public-health advocates pressing stricter caps, and displaced illicit operators all push back, while the broad electorate that enacted the arrangement defends it. All three temporal series run on one shared grid (points 0,2,4,6,8,10,12) so no metric row borrows another's end-state values. Receipt surface: gains demonstrably accrue to the state seat (excise yield, licensing fees), hence gain_flow names it; fixing the residual extraction (rate recalibration, reinvestment expansion, seating care guarantees) is achievable through ordinary legislation relative to the welfare at stake, hence fixing_cost is cheap.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the divergence is the finding. From the state seat the arrangement is coordination it administers and funds — a managed market doing what prohibition could not. From the user seat it is recovered liberty with a tolerable surcharge. From the trapped third-party seats the same structure is uncompensated exposure to risks they never accepted, enforced only after harm converts to liability. From the excluded illicit-operator seat it is a licensing cartel that confiscated their market and then disqualified them from re-entry. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: users hold mobile exit and a genuine liberty gain that outweighs the embedded tax; licensed firms collect margin directly; the state collects revenue and licensing fees and nets beneficiary even after enforcement expenditure. Victims derive high directionality, amplified by exit structure: road users are trapped (roads cannot be opted out of) and bystanders are constrained (shared air cannot be sealed), so neither can arbitrage away the externality they bear. The dual-positioned community seat nets toward the target end — reinvestment inflows are partial, statutory, and slower than the enforcement and exclusion costs they offset. Scope amplification applies modestly: the regime operates at national scale with local variation, making externality verification harder and effective extraction somewhat higher than raw epsilon suggests.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two opposite mislabelings. Reading the regime as pure rope would erase the third parties who pay through the very structure that coordinates users' liberty — the same licensing-and-tax apparatus that delivers safe legal supply also leaves crash victims and exposed bystanders uncompensated. Reading it as snare would erase what is genuinely coordinated and genuinely won: users exited the victim set, a violent supply chain shrank, and enforcement risk moved off possession onto harm. The founding problem remains live (status: live, verdict: world_rearranges), so no mandatrophy resolution is declared; the rising extractiveness series is monitored as rent-seeking layered onto coordination, not as a dead mandate kept alive theatrically — theater_ratio is a symptom here, not the test.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_locus,
    'This constraint instantiates the legalization reading of substance_control_kernel; which structural element do the sibling readings relocate, and what would their adoption change?',
    'Compare the compiled victim sets and state-warrant structures across the three sibling stories: the prohibition reading seats users as transgressors (warrant: moral order); the harm reduction reading seats heavy users as patients (warrant: clinical outcome); this reading seats only third parties as victims (warrant: externality prevention and cost capture).',
    'Resolution toward a sibling changes the constraint discontinuously rather than re-measuring this one: users enter the victim set under prohibition (epsilon rises sharply), or heavy users convert from consumers to care-recipients under harm reduction (the tax-transfer structure re-reads as health financing), altering beneficiaries, victims, and classification wholesale.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_locus, conceptual, 'Committer structure: one of three readings of a contested kernel; the disagreement lives in the user''s normative status and the resulting victim set.').

omega_variable(
    excise_rate_vs_externality_cost,
    'Do excise rates in mature legalization markets track measured externality costs, or have they drifted into revenue maximization?',
    'Fiscal incidence studies comparing tax yield per consumption unit against independently estimated externality costs (impaired-driving losses, healthcare burden, remediation), controlling for elasticity effects on illicit-market substitution.',
    'If rates substantially exceed externality cost, the regime''s own justification erodes for the taxpayer seat — the surplus is rent collected under a Pigouvian banner, pushing effective extraction upward and the classification toward snare-flavored for that seat; if rates track costs, the current moderate epsilon stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excise_rate_vs_externality_cost, empirical, 'Whether the tax is externality pricing or revenue extraction.').

omega_variable(
    externality_compensation_gap,
    'Are third-party harms under the legalized regime actually prevented or compensated, or merely priced and litigated after the fact?',
    'Longitudinal comparison of impaired-driving fatality rates, secondhand-exposure prevalence in multi-unit housing, and victim compensation recovery rates against pre-legalization baselines and against regimes with stricter consumption-space rules.',
    'If harms are merely priced rather than prevented, the victim seats'' effective extraction grows over time and the coordination function''s adequacy declines; if prevention is real, the moderate epsilon is stable and the regime''s tangled_rope balance holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_compensation_gap, empirical, 'Whether the externality-management function works or is nominal.').

omega_variable(
    gray_market_persistence,
    'Does the licit market fully displace illicit supply, or does high taxation sustain a permanent gray tier that keeps enforcement — and a residual victim class — alive?',
    'Market-share studies of seized unlicensed product, price-gap analysis between taxed and untaxed channels, and arrest-series decomposition showing who is still cited and for what.',
    'A persistent gray market keeps suppression elevated past the modeled decay curve and recreates an enforcement-exposed population the reading claims to have dissolved, raising both epsilon and suppression and straining the liberty-baseline claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gray_market_persistence, empirical, 'Whether black-market collapse is complete or partial.').

omega_variable(
    revenue_dependence_ratchet,
    'Has earmarking of excise revenue created a fiscal ratchet that locks rates above externality-cost levels regardless of the reading''s own principle?',
    'Legislative history of rate-setting votes and budget amendments: whether any jurisdiction has cut rates as externality estimates fell, or whether program constituencies block every reduction.',
    'A confirmed ratchet converts the state seat from neutral administrator into a captured beneficiary whose agenda-setting serves its own revenue stream — raising its derived directionality and supporting override-level correction of its computed d.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revenue_dependence_ratchet, conceptual, 'Whether the state''s exit from high-rate equilibrium is structurally blocked by its own spending commitments.').

omega_variable(
    authority_grounding_framing,
    'Is the legalization reading''s authority grounded in lineage (the Millian harm-principle tradition, interpreted case-by-case by courts) or better framed as distributed (legislatures, ballots, and courts producing competing settlements with no designated interpreter)?',
    'Examine whether a functioning interpretive layer consistently adjudicates the harm-principle boundary (per-se limits, consumption-space doctrine) or whether settlements vary irreducibly by jurisdiction with no interpretive continuity.',
    'Under the distributed framing, interpretation_layer_present becomes invalid and the commitment-system pattern reclassifies; the lineage framing adopted here rests on observed doctrinal continuity in harm-boundary adjudication.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_framing, conceptual, 'CS-framing under-determination: lineage versus distributed authority for the same kernel reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__legalization_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_kernel__legalization_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(subs_tr_t2, substance_control_kernel__legalization_reading, theater_ratio, 2, 0.2).
narrative_ontology:measurement(subs_tr_t4, substance_control_kernel__legalization_reading, theater_ratio, 4, 0.23).
narrative_ontology:measurement(subs_tr_t6, substance_control_kernel__legalization_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement(subs_tr_t8, substance_control_kernel__legalization_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(subs_tr_t10, substance_control_kernel__legalization_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(subs_tr_t12, substance_control_kernel__legalization_reading, theater_ratio, 12, 0.31).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_kernel__legalization_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(subs_be_t2, substance_control_kernel__legalization_reading, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(subs_be_t4, substance_control_kernel__legalization_reading, base_extractiveness, 4, 0.45).
narrative_ontology:measurement(subs_be_t6, substance_control_kernel__legalization_reading, base_extractiveness, 6, 0.47).
narrative_ontology:measurement(subs_be_t8, substance_control_kernel__legalization_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(subs_be_t10, substance_control_kernel__legalization_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(subs_be_t12, substance_control_kernel__legalization_reading, base_extractiveness, 12, 0.54).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_kernel__legalization_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(subs_su_t2, substance_control_kernel__legalization_reading, suppression_requirement, 2, 0.44).
narrative_ontology:measurement(subs_su_t4, substance_control_kernel__legalization_reading, suppression_requirement, 4, 0.43).
narrative_ontology:measurement(subs_su_t6, substance_control_kernel__legalization_reading, suppression_requirement, 6, 0.41).
narrative_ontology:measurement(subs_su_t8, substance_control_kernel__legalization_reading, suppression_requirement, 8, 0.39).
narrative_ontology:measurement(subs_su_t10, substance_control_kernel__legalization_reading, suppression_requirement, 10, 0.37).
narrative_ontology:measurement(subs_su_t12, substance_control_kernel__legalization_reading, suppression_requirement, 12, 0.36).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__legalization_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, substance_control_kernel__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, substance_control_kernel__harm_reduction_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'substance control policy' covers three structurally distinct constraints that decompose per the epsilon-invariance principle: the prohibition reading (users seated as moral transgressors; high epsilon with users in the victim set), the harm reduction reading (heavy users seated as patients; epsilon indexed to clinical outcomes), and this legalization reading (users outside the victim set entirely; third parties enter it via externalities; state enters as revenue collector). Each story carries its own epsilon, victim set, and classification; sibling IDs follow the kernel-prefix convention substance_control_kernel__{reading}. Prohibition is the historical baseline whose documented failures seeded both successor readings, so this story links to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
