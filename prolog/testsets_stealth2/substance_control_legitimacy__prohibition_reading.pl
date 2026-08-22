% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__prohibition_reading, []).

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
 *   constraint_id: substance_control_legitimacy__prohibition_reading
 *   human_readable: Criminalization-Based Substance Control Regime (Prohibition Reading)
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   The arrangement under contest is the criminalization regime for
 *   psychoactive substances: scheduled compounds whose possession, sale, and
 *   distribution are crimes, enforced through police interdiction,
 *   prosecutorial processing, and incarceration, and funded through
 *   appropriations and asset forfeiture. This story instantiates the
 *   prohibition reading of the substance-control-legitimacy kernel, which
 *   holds that substance use is inherently harmful and that state authority
 *   legitimately derives from a moral duty to prevent that harm through
 *   criminal law. The epsilon referent is the standing criminalization
 *   arrangement itself, assessed by this reading's own lights: the reading
 *   regards the arrangement as morally required while acknowledging its real
 *   operating costs — mass incarceration, forfeiture-driven policing, and a
 *   violent illicit market — as burdens the duty justifies. Claimed type and
 *   metrics are authored independently: the moral claim does not tune the
 *   descriptive scores, and the descriptive scores do not adjudicate the
 *   moral claim.
 *
 * KEY AGENTS:
 *   - - law_enforcement_apparatus: Agenda-setter and administrative collector (institutional/identity_locked) — sets enforcement intensity; budgets and forfeitures flow inward
 *   - - elected_prohibition_coalition: Agenda-setter (institutional/arbitrage) — authors schedules, sentences, and appropriations
 *   - - district_attorney_offices: Beneficiary (institutional/constrained) — processes cases; convictions and forfeitures resource the office
 *   - - correctional_facility_operators: Beneficiary (institutional/arbitrage) — holds capacity contracts scaled to drug-case admissions
 *   - - illicit_supply_networks: Structural beneficiary (organized/arbitrage) — earns prohibition price premiums; enforcement culls its rivals
 *   - - substance_users: Primary target (powerless/trapped) — bears criminalization, records, and market violence directly
 *   - - overpoliced_low_income_communities: Concentrated target (powerless/trapped) — absorbs patrol intensity and collateral records
 *   - - families_of_incarcerated_users: Diffuse target (powerless/trapped) — carries fines, fees, and household loss
 *   - - harm_reduction_practitioners: Excluded voice (moderate/constrained) — holds overdose-reducing tools the rules bar or marginalize
 *   - - drug_policy_researchers: Analytical observer (analytical/analytical) — measures outcomes across regimes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__prohibition_reading, 0.78).
domain_priors:suppression_score(substance_control_legitimacy__prohibition_reading, 0.85).
domain_priors:theater_ratio(substance_control_legitimacy__prohibition_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__prohibition_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__prohibition_reading, "Criminalization-Based Substance Control Regime (Prohibition Reading)").
narrative_ontology:topic_domain(substance_control_legitimacy__prohibition_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__prohibition_reading, '7061472f-a2d0-4b71-8389-56dc0d35c439').
narrative_ontology:cs_kernel_codification('7061472f-a2d0-4b71-8389-56dc0d35c439', formalized).
narrative_ontology:cs_authority_grounding('7061472f-a2d0-4b71-8389-56dc0d35c439', lineage).
narrative_ontology:cs_interpretation_layer_present('7061472f-a2d0-4b71-8389-56dc0d35c439').
narrative_ontology:cs_reading_relation('7061472f-a2d0-4b71-8389-56dc0d35c439', substance_control_legitimacy__harm_reduction_reading, coexists_with).
narrative_ontology:cs_reading_relation('7061472f-a2d0-4b71-8389-56dc0d35c439', substance_control_legitimacy__legalization_reading, forecloses).
narrative_ontology:cs_axiom('7061472f-a2d0-4b71-8389-56dc0d35c439', foundational, self_regarding_harm_prevention_is_state_duty).
narrative_ontology:cs_axiom_status(self_regarding_harm_prevention_is_state_duty, holdable).
narrative_ontology:cs_axiom_grounding('7061472f-a2d0-4b71-8389-56dc0d35c439', self_regarding_harm_prevention_is_state_duty, deontological).
narrative_ontology:cs_axiom('7061472f-a2d0-4b71-8389-56dc0d35c439', foundational, criminalization_is_required_instrument_of_duty).
narrative_ontology:cs_axiom_status(criminalization_is_required_instrument_of_duty, holdable).
narrative_ontology:cs_axiom_grounding('7061472f-a2d0-4b71-8389-56dc0d35c439', criminalization_is_required_instrument_of_duty, instrumental).
narrative_ontology:cs_reference_frame('7061472f-a2d0-4b71-8389-56dc0d35c439', moral_duty_paternalist_order).
narrative_ontology:cs_drift_state('7061472f-a2d0-4b71-8389-56dc0d35c439', contemporary_post_legalization_wave, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('7061472f-a2d0-4b71-8389-56dc0d35c439', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, law_enforcement_apparatus).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, correctional_facility_operators).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, illicit_supply_networks).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, district_attorney_offices).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, elected_prohibition_coalition).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, overpoliced_low_income_communities).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, families_of_incarcerated_users).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__prohibition_reading, criminal_deterrence_theory).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__prohibition_reading, state_paternalism_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Plans and carries out interdiction, street-level enforcement, and arrest operations; agency budgets, headcount, and equipment grants scale with enforcement volume, and seized assets flow back into operations. Five decades of mission concentration have fused the organization's professional identity around drug enforcement, so winding the mission down reads internally as institutional self-erasure.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, law_enforcement_apparatus, agenda_setter,
    institutional, generational, identity_locked, national).

% Public prison systems and private contractors hold facility contracts sized to incarcerated populations; drug offenses supply a large share of admissions and supervision caseloads. Operators advocate sentencing structures that sustain occupancy and can pursue contracts across states when one jurisdiction's population contracts.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, correctional_facility_operators, beneficiary,
    institutional, generational, arbitrage, national).

% Trafficking organizations sell into a market where lawful competition is barred, earning price premiums that exist only because supply is criminalized; each enforcement action that removes rivals consolidates their share. They reroute shipments and alter product compounds faster than scheduling can follow, operating across borders beyond any single jurisdiction's reach.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, illicit_supply_networks, beneficiary,
    organized, biographical, arbitrage, global).

% Prosecute possession and distribution cases; conviction counts and asset forfeitures resource the offices, and plea leverage in drug charges sets case-processing tempo. Career advancement tracks trial and conviction records, giving prosecutors a durable stake in case volume.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, district_attorney_offices, beneficiary,
    institutional, biographical, constrained, regional).

% Legislators and executives who author controlled-substance schedules, mandatory minimum sentences, and enforcement appropriations. Tough-on-drugs positioning has historically returned electoral benefit, and the position can be adjusted rhetorically when constituent preferences move, at low personal cost to the officeholder.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, elected_prohibition_coalition, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__prohibition_reading, elected_prohibition_coalition, beneficiary).

% Possession and use are criminal acts; a conviction record blocks employment, housing, licensure, and benefits long after sentence completion. Physical dependence keeps many in the market the rules target, while accumulated legal exposure deepens with each contact. Stepping outside the rules' reach would require both ending use and clearing a record, and neither is individually available to most.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, substance_users, payer,
    powerless, biographical, trapped, national).

% Patrol intensity, stop rates, and arrests concentrate in low-income neighborhoods, so residents absorb surveillance, the removal of working-age members, and the collateral record effects that follow. Moving away is bounded by income and housing discrimination; staying means living under the enforcement pattern.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, overpoliced_low_income_communities, payer,
    powerless, generational, trapped, regional).

% Households lose earners and caregivers to incarceration and then carry fines, court fees, and commissary and phone charges that price contact with imprisoned members. Children absorb the instability; the household has no procedural standing in the sentencing decisions that restructure it.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, families_of_incarcerated_users, payer,
    powerless, generational, trapped, national).

% Operate syringe service programs, naloxone distribution, and supervised-use pilots that reduce overdose deaths across study after study. The tools they rely on sit under legal ambiguity or explicit ban, and funding rules exclude them from formal policy deliberation, so their evidence reaches the conversation only secondhand.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, harm_reduction_practitioners, excluded,
    moderate, biographical, constrained, national).

% Measure prevalence, overdose mortality, enforcement disparities, and market substitution across jurisdictions and regimes; publish the comparisons that all sides of the policy argument cite. Hold no enforcement or legislative authority and no financial stake in either direction.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, drug_policy_researchers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_legitimacy__prohibition_reading, law_enforcement_apparatus).
narrative_ontology:fixing_cost_class(substance_control_legitimacy__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action against substance-attributable harm: deters use among the subset of people responsive to legal risk, restricts the open retail channels through which minors would otherwise obtain substances, and expresses and enforces shared moral norms about intoxication and self-destruction through a single uniform legal standard.
% TRANSFER_FUNCTION: Moves liberty (incarceration), income (fines, court fees, forfeited assets), and labor (incarcerated work assignments) from users and from the communities where enforcement concentrates, to the enforcement apparatus, correctional operators, and prosecuting offices — and, via black-market price premiums created by supply criminalization, to illicit supply networks.
% ABSENT_VOICES: Current and former users, harm-reduction practitioners, and residents of heavily patrolled communities have no formal seat in prohibition policymaking; the conversation is held among enforcement agencies, prosecutors, and legislators, with the people bearing the arrangement's direct costs represented only by proxy.
% DISAPPEARANCE_RATIONALE: If the criminalization regime vanished overnight, millions of pending cases would leave the dockets, the incarcerated drug-offense population would discharge into a contracting carceral system, black-market price premiums would collapse as lawful supply emerged, enforcement budgets and forfeiture streams would evaporate, and the heavily patrolled communities would reorganize around radically reduced police contact. The rearrangement would be large, fast, and unevenly distributed across the seats named above.
% FOUNDING_PROBLEM: Late nineteenth- and twentieth-century temperance and narcotics concerns: visible public intoxication, industrial and domestic harm attributed to alcohol, opium, and cocaine, and later urban disorder associated with narcotics trafficking — built to solve by eliminating substance use through moral suasion backed by criminal law.
% FOUNDING_PROBLEM_CORROBORATION: The underlying harm problem is corroborated from outside the benefiting parties: a century of public-health mortality and morbidity statistics — overdose deaths, substance-attributable disease burden — attests that substance harm persists, as do medical association statements independent of the enforcement apparatus. What no outside source attests is that criminalization is the operative remedy for it; remedy efficacy is precisely what the sibling readings dispute, and the enforcement apparatus's own attestations of efficacy are self-interested.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__prohibition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__prohibition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_legitimacy__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__prohibition_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_legitimacy__prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_legitimacy__prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the arrangement converts a public-health phenomenon into criminal process: liberty taken through incarceration, income taken through fines and forfeited assets, and years taken through records that outlast sentences. Suppression is higher (0.85) because persistence depends on actively barring alternatives — lawful markets are prohibited, harm-reduction tools are restricted, and the enforcement machinery itself is the barrier; suppression here is overwhelmingly structural (statute, policing, treaty commitment) with a minority internalized component (roughly fifteen percent: stigma that deters treatment-seeking even where services legally exist). Theater is moderate (0.30): arrests, prosecutions, and incarcerations are functionally real, but a growing share of activity is symbolic — press-conference seizures, awareness campaigns, rhetorical enforcement that maintains the appearance of progress while availability and overdose mortality rise. Accessibility collapse is mid-range (0.55): alternative regimes are visible in peer jurisdictions and domestic reform ballots, so the alternative set is not unimaginable, but statute, treaty obligations, and appropriated budgets keep those alternatives from becoming locally available. Resistance is high (0.70): decriminalization ordinances, legalization initiatives, sentencing-reform litigation, and harm-reduction advocacy constitute sustained organized opposition unusual for a settled policy. The three temporal series share one grid (T=0..50, roughly 1971-2021): extractiveness and enforcement capacity ratchet sharply through the mandatory-minimum and crack-era buildout (T=10-30), plateau at peak incarceration, dip slightly during the reform wave, then re-firm amid fentanyl-era escalation; theater climbs monotonically as symbolic activity substitutes for declining marginal effectiveness. Suppression_requirement is tracked because the story's central dynamic is enforcement-capacity construction — militarization, sentencing structure, appropriation growth — not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats the arrangement presents as duty fulfilled: coordination of collective moral effort against a real harm, with costs accepted as the price of prevention. From the payer seats the same structure presents as a machine that consumes households: contact begins with patrol discretion, proceeds through plea leverage, and ends in records that price life chances for decades. The excluded practitioners' seat adds a third view: the arrangement suppresses the specific instruments with the strongest overdose-mortality evidence, so what the agenda-setter calls duty appears to them as the blocking of known harm reduction. Coalition potential matters at the powerless end: users, families, and affected communities have repeatedly converted dispersed individual weakness into ballot-measure strength where initiative processes exist, which is why resistance scores high despite low individual power. The engine computes these divergent per-seat classifications from the structural data; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries map to low-directionality seats: the enforcement apparatus collects appropriations and forfeitures; correctional operators collect capacity payments; prosecutors collect case volume and forfeiture revenue; the elected coalition collects electoral returns; and — the structurally perverse case — illicit supply networks collect the price premium that criminalization alone creates, with enforcement intensity functioning as market discipline on their behalf. Declared victims map to high-directionality seats: users bear the criminal process directly with trapped exits (dependence plus record); enforcement-displaced communities bear concentrated exposure with geographically bounded exit; families bear the fiscal and household residue with no procedural standing. Trapped exit placement pushes the victim seats toward the full-target end of the directionality range, so effective extraction concentrates exactly where mobility is lowest — the engine owns that scaling; the declarations only fix who stands where.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — substance-attributable harm — is still live, so the mandate has not outlived its function and no mandatrophy resolution is declared. The tangled-rope classification is what prevents the two available mislabels: reading the arrangement as pure coordination (the prohibitionist claim) would erase the identifiable paying seats and the forfeiture-driven feedback loop; reading it as pure extraction (the abolitionist counter-claim) would erase the genuine coordination residue — youth-access channel restriction and deterrence for the subset of users responsive to legal risk — that keeps the arrangement from collapsing into open rent collection. The deterrence-efficacy omega marks the live seam: if criminalization's preventive yield resolves to negligible, the coordination half atrophies and the structure drifts toward snare; if it resolves to material, the coordination half holds and the asymmetry stays hybrid. Theater remains function-weighted at interval end, so no piton reading is warranted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the substance_control_legitimacy kernel; the disagreement with the sibling readings (harm_reduction_reading, legalization_reading) is located in the jurisdictional premise — whether state authority extends to self-regarding conduct at all. If a sibling reading acquires statutory force, does this reading''s victim set and enforcement structure dissolve?',
    'Comparative institutional analysis of jurisdictions where harm-reduction or autonomy framings acquired legal force: track whether user criminalization, carceral populations, and enforcement appropriations contract when the sibling premise governs.',
    'If a sibling reading displaces this one, users exit the victim set, effective extraction collapses toward the coordination floor, and the residual enforcement shell trends toward inertial maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether the prohibition reading survives displacement by sibling readings of the same kernel.').

omega_variable(
    inherent_vs_contingent_harm,
    'Is substance use inherently harmful (use itself produces the harm the duty responds to), or is harm contingent on dose, pattern, and context such that some use is benign?',
    'Epidemiological dose-response and pattern-of-use research distinguishing substance-class harm profiles from use-event harm.',
    'If harm is contingent, the moral-duty premise narrows to specific patterns, shrinking the criminalized class and weakening the constraint''s universal pretense; if inherent, the duty claim retains full scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inherent_vs_contingent_harm, empirical, 'The foundational factual premise beneath the reading''s moral claim.').

omega_variable(
    enforcement_self_preservation_loop,
    'Does enforcement intensity track measured public-safety need, or does it track institutional budget preservation and forfeiture dependency?',
    'Budget-cycle analysis correlating enforcement expansion with harm rates versus agency staffing and funding cycles; forfeiture-dependency audits of agency operating budgets.',
    'If self-preservation dominates, the coordination function is thinner than claimed and the arrangement sits nearer the snare boundary; if harm-tracking dominates, the coordination half is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_self_preservation_loop, empirical, 'Whether the beneficiary structure reflects public-safety service or institutional self-perpetuation.').

omega_variable(
    black_market_violence_attribution,
    'Is illicit-market violence an externality produced by prohibition''s market structure, or an externality of demand that any regulatory regime would face?',
    'Compare violence trajectories in substance markets before and after legalization or depenalization within comparable jurisdictions, controlling for market size.',
    'If prohibition-generated, the arrangement manufactures the harm it claims to prevent, strengthening the extraction reading; if demand-generated, part of the violence cost lies outside this constraint''s ledger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_violence_attribution, empirical, 'Attribution of the black-market violence externality.').

omega_variable(
    deterrence_efficacy_warrant,
    'Does criminalization reduce population-level use relative to public-health instruments by enough to justify its carceral costs?',
    'Cross-jurisdiction natural experiments comparing use onset and prevalence under prohibition versus regulated or harm-reduction regimes, controlling for income and enforcement history.',
    'If the deterrence advantage is negligible, the instrumental axiom loses its warrant and the reading collapses toward the harm-reduction sibling; if material, the coordination function stands and the asymmetry stays hybrid.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrence_efficacy_warrant, empirical, 'Whether the coordination half of the arrangement has real preventive yield.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__prohibition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__prohibition_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(subs_tr_t0, observed).
narrative_ontology:measurement(subs_tr_t10, substance_control_legitimacy__prohibition_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement_basis(subs_tr_t10, observed).
narrative_ontology:measurement(subs_tr_t20, substance_control_legitimacy__prohibition_reading, theater_ratio, 20, 0.23).
narrative_ontology:measurement_basis(subs_tr_t20, observed).
narrative_ontology:measurement(subs_tr_t30, substance_control_legitimacy__prohibition_reading, theater_ratio, 30, 0.27).
narrative_ontology:measurement_basis(subs_tr_t30, observed).
narrative_ontology:measurement(subs_tr_t40, substance_control_legitimacy__prohibition_reading, theater_ratio, 40, 0.29).
narrative_ontology:measurement_basis(subs_tr_t40, observed).
narrative_ontology:measurement(subs_tr_t50, substance_control_legitimacy__prohibition_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement_basis(subs_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__prohibition_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(subs_be_t0, observed).
narrative_ontology:measurement(subs_be_t10, substance_control_legitimacy__prohibition_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(subs_be_t10, observed).
narrative_ontology:measurement(subs_be_t20, substance_control_legitimacy__prohibition_reading, base_extractiveness, 20, 0.74).
narrative_ontology:measurement_basis(subs_be_t20, observed).
narrative_ontology:measurement(subs_be_t30, substance_control_legitimacy__prohibition_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement_basis(subs_be_t30, observed).
narrative_ontology:measurement(subs_be_t40, substance_control_legitimacy__prohibition_reading, base_extractiveness, 40, 0.76).
narrative_ontology:measurement_basis(subs_be_t40, observed).
narrative_ontology:measurement(subs_be_t50, substance_control_legitimacy__prohibition_reading, base_extractiveness, 50, 0.78).
narrative_ontology:measurement_basis(subs_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__prohibition_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(subs_su_t0, observed).
narrative_ontology:measurement(subs_su_t10, substance_control_legitimacy__prohibition_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(subs_su_t10, observed).
narrative_ontology:measurement(subs_su_t20, substance_control_legitimacy__prohibition_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement_basis(subs_su_t20, observed).
narrative_ontology:measurement(subs_su_t30, substance_control_legitimacy__prohibition_reading, suppression_requirement, 30, 0.86).
narrative_ontology:measurement_basis(subs_su_t30, observed).
narrative_ontology:measurement(subs_su_t40, substance_control_legitimacy__prohibition_reading, suppression_requirement, 40, 0.84).
narrative_ontology:measurement_basis(subs_su_t40, observed).
narrative_ontology:measurement(subs_su_t50, substance_control_legitimacy__prohibition_reading, suppression_requirement, 50, 0.85).
narrative_ontology:measurement_basis(subs_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy__legalization_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'drug prohibition' decomposes into three structurally distinct constraints — one per reading of the substance_control_legitimacy kernel. Each reading authors its own epsilon over the shared referent (the standing criminalization arrangement): this prohibition reading assesses the arrangement as a morally required instrument that carries real carceral and market-violence costs; the harm-reduction and legalization siblings assess the same arrangement from premises that shrink or dissolve its victim set. The files are linked via affects_constraints; downstream influence runs from whichever reading currently holds statutory force toward the others' operating environment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
