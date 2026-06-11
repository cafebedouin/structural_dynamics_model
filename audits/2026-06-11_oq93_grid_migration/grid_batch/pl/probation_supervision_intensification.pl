% ============================================================================
% CONSTRAINT STORY: probation_supervision_intensification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_probation_supervision_intensification, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: probation_supervision_intensification
 *   human_readable: Probation Supervision Intensification via Electronic Monitoring and Fee Stacking
 *   domain: criminal_justice/carceral_control
 *
 * SUMMARY:
 *   Electronic monitoring and fee stacking in probation supervision represent
 *   a system of intensified extraction masked as 'alternatives to
 *   incarceration.' A probationer on GPS monitoring with electronic
 *   supervision, day reporting requirements, and accumulated supervision fees
 *   faces removal of freedom (through constant monitoring and location
 *   restriction) and removal of resources (through stacked fees that can
 *   consume 20-40% of income). Compliance itself becomes unaffordable: the
 *   person must pay to be supervised, and failure to pay triggers
 *   reincarceration. The constraint intensifies across the interval
 *   (2010-2025) as vendors penetrate markets, states adopt fee-based
 *   probation funding, and supervision requirements expand. Individual-level
 *   coercion saturates by 2025 (accessibility_collapse=0.88,
 *   stakes_inflation=0.85): probationers face immobilization and unaffordable
 *   fees with zero legitimate exit. Simultaneously, structural visibility of
 *   the extraction falls (structural accessibility_collapse=0.48, structural
 *   suppression=0.35): the apparatus is publicly framed as
 *   'technology-enabled alternatives to prison,' obscuring its extractive
 *   logic. The grid captures this inversion: maximum coercion at the
 *   individual level, where pressure saturates and resistance is minimal;
 *   moderate-to-low pressure at the organizational and structural levels,
 *   where beneficiaries (vendors, departments, state budgets) experience
 *   coordination benefits and extract value with minimal accountability. The
 *   constraint is a snare because no exit legitimate under the law exists,
 *   and the coercion is actively managed to increase over time.
 *
 * KEY AGENTS:
 *   - Probationers: powerless/trapped — face intensifying surveillance, location restriction, and unaffordable fees with zero lawful exit
 *   - Probationer families: powerless/identity_locked — carry collateral extraction and stigma; identity fused with monitored person
 *   - Private monitoring vendors (e.g., Atticus, GEO Group): institutional/arbitrage — capture growing contracts as states expand monitoring; experience as pure market coordination
 *   - County probation departments: moderate/constrained — face genuine budget/supervision coordination problem but use fees to extract; exit is constrained (budget cuts if they refuse)
 *   - State corrections budgets: institutional/arbitrage — benefit from probation fee revenue and reduced incarceration costs; low extraction experience
 *   - Reentry services and nonprofits: organized/constrained — lose funding as probation dollars shift to monitoring; resistance is organized but blocked
 *   - Criminal justice reform advocates: organized/constrained — document the extraction but lack power to change system
 *   - Analytical observer: analytical/analytical — risks naturalizing technological control as inevitable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(probation_supervision_intensification, 0.72).
domain_priors:suppression_score(probation_supervision_intensification, 0.81).
domain_priors:theater_ratio(probation_supervision_intensification, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(probation_supervision_intensification, extractiveness, 0.72).
narrative_ontology:constraint_metric(probation_supervision_intensification, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(probation_supervision_intensification, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(probation_supervision_intensification, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(probation_supervision_intensification, resistance, 0.34).

% --- Constraint claim ---
narrative_ontology:constraint_claim(probation_supervision_intensification, snare).
narrative_ontology:human_readable(probation_supervision_intensification, "Probation Supervision Intensification via Electronic Monitoring and Fee Stacking").
narrative_ontology:topic_domain(probation_supervision_intensification, "criminal_justice/carceral_control").

domain_priors:requires_active_enforcement(probation_supervision_intensification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(probation_supervision_intensification, private_monitoring_vendors).
narrative_ontology:constraint_beneficiary(probation_supervision_intensification, county_probation_departments).
narrative_ontology:constraint_beneficiary(probation_supervision_intensification, state_corrections_budgets).
narrative_ontology:constraint_victim(probation_supervision_intensification, probationers).
narrative_ontology:constraint_victim(probation_supervision_intensification, probationer_families).
narrative_ontology:constraint_victim(probation_supervision_intensification, reentry_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(probation_supervision_intensification, reentry_services_nonprofits).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals under court-ordered probation are subject to electronic monitoring (GPS ankle monitor or RF-based tracking), day reporting requirements, and accumulating supervision fees. They must pay fees ranging from $50-150 monthly plus initial device fees ($300-500), often consuming 15-40% of income. They face curfew enforcement, location restrictions, and automated compliance monitoring. Noncompliance with any requirement (missing a payment, being outside designated zones, missing a day report) triggers arrest and reincarceration. Removal from probation requires perfect compliance for the full probation term (typically 3-5 years). Exit is legal only after the probation term ends; early termination is rare. They have no lawful way to exit the constraints while probation is active.
narrative_ontology:constraint_stakeholder(probation_supervision_intensification, probationers, payer,
    powerless, biographical, trapped, national).

% Families bear collateral costs: they may contribute to probation fees (from household income), experience surveillance of home visits, coordinate around curfew and monitoring restrictions, and carry social stigma. Minor children and dependent adults may experience restricted visitation or movement due to the probationer's conditions. Families have no standing to challenge conditions and cannot exit without severing family bonds. Their identity becomes fused with the monitored person—they are known as 'the family of'—making exit through disaffiliation psychologically costly.
narrative_ontology:constraint_stakeholder(probation_supervision_intensification, probationer_families, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(probation_supervision_intensification, probationer_families, excluded).

% Companies providing GPS monitoring devices, RF tracking systems, day reporting infrastructure, and compliance software (e.g., Atticus, GEO Group, Securus) receive government contracts for monitoring services. Revenue grows with jurisdiction adoption and expanded monitoring requirements. They experience the market as organic demand—jurisdictions request solutions to supervision challenges, and vendors supply them. They have exit capacity: contracts are renewed annually, and they operate in multiple jurisdictions. The apparatus itself (tracking technology) functions reliably. They benefit from fee-based revenue models where probationers or probation departments pay per device, per report, per facility visit.
narrative_ontology:constraint_stakeholder(probation_supervision_intensification, private_monitoring_vendors, beneficiary,
    institutional, immediate, arbitrage, global).

% Probation departments manage growing probation populations (often 300-500% increases over 20 years) with flat or declining general appropriations. Electronic monitoring allows them to supervise more people with fewer staff. They impose and collect supervision fees (authorized by state law) as cost-recovery, which becomes a revenue stream that departments depend on. Exit is constrained: if they reduce monitoring requirements or fees, they face pressure to reduce staff (political pressure for 'tough on crime' and budget cuts). If they expand monitoring, they capture more fee revenue but increase population immobilization. They set the agenda for individual probationers (conditions of probation) but operate within state policy and budget constraints set above them.
narrative_ontology:constraint_stakeholder(probation_supervision_intensification, county_probation_departments, agenda_setter,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(probation_supervision_intensification, county_probation_departments, beneficiary).

% State corrections systems benefit from probation-based supervision: it appears to reduce incarceration costs compared to building prisons, even though probation populations have grown 300%+. States collect portions of probation fee revenue and reduce costs for incarceration infrastructure. They frame the system as cost-effective and evidence-based. They have exit capacity: they could fund probation through general appropriations rather than fees, or reduce probation requirements, but they benefit from fee revenue and low incarceration budgets. The current arrangement subsidizes their budget with extracted probationer fees.
narrative_ontology:constraint_stakeholder(probation_supervision_intensification, state_corrections_budgets, beneficiary,
    institutional, immediate, arbitrage, national).

% Nonprofits providing job training, substance abuse treatment, mental health services, and housing support to formerly incarcerated people lose funding as probation dollars shift toward monitoring infrastructure and fee collection. They see probationers arriving with accumulated debt, health problems amplified by stress and sleep deprivation from monitoring, and reduced capacity to work or engage in services due to curfew and day reporting requirements. Their reentry effectiveness declines as probation constraints intensify. They resist through advocacy but have constrained exit: they depend on state funding and cannot defund themselves without closing programs.
narrative_ontology:constraint_stakeholder(probation_supervision_intensification, reentry_services_nonprofits, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(probation_supervision_intensification, reentry_services_nonprofits, excluded).

% Advocacy organizations document and challenge probation intensification through lawsuits, legislative testimony, and public reporting. They identify fee stacking, vendor capture, and recidivism increase driven by technical violations. They have constrained exit: they depend on foundation funding and public attention, and cannot easily shift focus to other issues without abandoning this constituency. They resist but remain organizationally outside the decision-making apparatus. Success requires changing state law or funding models—high-cost activities with uncertain outcomes.
narrative_ontology:constraint_stakeholder(probation_supervision_intensification, criminal_justice_reform_advocates, excluded,
    organized, generational, constrained, national).

% The public narrative framing around probation ('alternatives to incarceration,' 'community supervision,' 'technology-enabled management') is not an agent but a non-agent entity (a doctrine, a framing). It is listed because the constraint's persistence depends critically on the legitimacy narrative obscuring the extraction. The analytical seat recognizes this as a non-agent with analytic significance.
narrative_ontology:constraint_stakeholder(probation_supervision_intensification, public_legitimacy_apparatus, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(probation_supervision_intensification, public_legitimacy_apparatus).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(probation_supervision_intensification, private_monitoring_vendors).
narrative_ontology:fixing_cost_class(probation_supervision_intensification, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The genuine coordination problem: managing growing probation populations without mass incarceration. Supervision requires oversight, and electronic monitoring provides a technical mechanism (tracking presence via GPS/RF) that allows one officer to manage 100+ cases instead of 10. The coordination function solves a real resource constraint: states cannot afford to incarcerate or hire probation staff at the rate probation populations have grown.
% TRANSFER_FUNCTION: Probation systems transfer freedom (location restriction, surveillance, constant monitoring) and resources (fees, 15-40% of income) FROM probationers TO: (1) private vendors (contract revenue), (2) probation departments (fee revenue and operational budget), (3) state budgets (reduced incarceration costs, fee revenue). Reentry services and families experience net loss. The constraint moves money and mobility upward from powerless agents to institutional beneficiaries.
% ABSENT_VOICES: Voices absent from supervision policy design: currently or formerly incarcerated people (policy is made without their participation), economic researchers (cost-benefit analysis is rarely conducted), reentry services providers (their evidence about monitoring barriers to reentry is not weighted), and public defenders (who could speak to noncompliance patterns). Also absent: comparative jurisdiction data on effectiveness of different supervision models. The apparatus hears from vendors, police, and corrections administration; it does not hear from those bearing the costs.
% DISAPPEARANCE_RATIONALE: If probation supervision intensification disappeared tomorrow—if electronic monitoring were discontinued, supervision fees eliminated, and day reporting requirements removed—the arrangements would rearrange substantially. Probation departments would face the original coordination problem (how to supervise large populations with limited staff) and would need to solve it through: (1) hiring more staff (budget increase), (2) reducing probation populations through decriminalization (policy change), or (3) accepting higher incarceration rates (political risk). Vendors would lose contracts. States would lose fee revenue. The system depends on the intensification; removing it exposes the underlying resource constraint.
% FOUNDING_PROBLEM: The founding problem was the early-2000s crisis of 'mass incarceration': U.S. prison populations had tripled from 1970-2000. Policymakers sought 'alternatives to incarceration' to reduce prison populations without appearing soft on crime. Electronic monitoring and probation expansion were presented as the solution: supervise more people without building more prisons. The problem was live and real (incarceration costs were becoming unsustainable).
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (mass incarceration) is corroborated by every independent researcher and policy analyst working on criminal justice. However, whether probation intensification actually solved the problem is contested. Critics note: (1) incarceration rates did not decrease; they remained flat or increased in many jurisdictions, (2) probation populations grew 300%+ while incarceration remained high, (3) probation-based extraction may have replaced incarceration-based extraction but did not reduce total system coercion. Corroboration on the original problem comes from VERA Institute, Pew Research, scholarly consensus. Corroboration on whether intensified probation solved the problem comes only from correctional administrations claiming success without independent validation.
narrative_ontology:disappearance_verdict(probation_supervision_intensification, world_rearranges).
narrative_ontology:founding_problem_status(probation_supervision_intensification, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% A probationer on electronic monitoring with stacked supervision fees faces zero exit capacity. Removal from probation requires perfect compliance, but compliance itself is actively made unaffordable. Noncompliance triggers reincarceration. The constraint extracts both money (through fees) and freedom (through monitoring). This is the maximum-extraction, maximum-suppression perspective.
constraint_indexing:constraint_classification(probation_supervision_intensification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Family members experience collateral extraction: they bear costs of monitoring fees, surveillance, and restricted visitation, while carrying stigma from association with a monitored person. They have no standing to object and cannot exit through their own action. Their exit option is trapped via identity linkage rather than direct legal dependency.
constraint_indexing:constraint_classification(probation_supervision_intensification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% From the vendor's perspective, intensified monitoring requirements are coordination: they solve the genuine problem of oversight without incarceration. The vendor benefits from expanded contracts but experiences this as legitimate market demand response. Theater is moderate because the vendor's core function (tracking location via GPS/RF) genuinely works; the extraction is via rent-capture on a captured market. Effective extraction is negative from the vendor's seat—they are subsidized through government contracts.
constraint_indexing:constraint_classification(probation_supervision_intensification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% County probation departments face genuine coordination: they must supervise growing populations with flat or declining budgets. Electronic monitoring appears to solve this (more coverage with fewer staff). But the apparatus simultaneously extracts: departments capture supervision fees, staff employment is linked to supervision volume, and perverse incentives drive fee stacking. Exit is constrained—departments could reject fee-based funding but face budget cuts; they could reduce supervision intensity but face political pressure for 'tough on crime' optics. Mixed experience: genuine coordination problem plus embedded extraction.
constraint_indexing:constraint_classification(probation_supervision_intensification, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% Probation departments that drain resources through fee collection have fewer resources for reentry services (job training, substance abuse treatment, housing support). The reentry commons is a collective good that absorbs the extraction. As monitoring intensifies, reentry investment declines, recidivism rises, and the cycle deepens. Organized agents (nonprofits, advocacy groups) can resist but face state-level barriers. The commons sees the constraint as degradation of their mission (snare).
constraint_indexing:constraint_classification(probation_supervision_intensification, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Electronic monitoring is narratively framed as 'alternatives to incarceration'—a benign, even progressive technology. This narrative persists despite evidence that intensified monitoring produces functional reincarceration (immobilizing, extractive, without institutional confinement) with lower accountability. The ritual of claiming alternatives-to-incarceration persists through institutional theater, maintained because the core narrative (technology = rehabilitation) is no longer credible but abandoning it would require confronting the system's extractive logic. High theater ratio reflects this performative maintenance.
constraint_indexing:constraint_classification(probation_supervision_intensification, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% From a civilizational view, some level of supervision overhead is inherent to managing probation populations without incarceration: monitoring has real costs, and someone must pay. The mountain framing naturalizes the distribution of costs as inevitable rather than contingent. However, the structural data contradicts the mountain classification—the beneficiary structure is visible, and suppression/theater are high. The false summit detector will identify this as naturalization of a policy choice.
constraint_indexing:constraint_classification(probation_supervision_intensification, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(probation_supervision_intensification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(probation_supervision_intensification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(probation_supervision_intensification, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(probation_supervision_intensification, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(probation_supervision_intensification, TR),
    TR >= 0.70.

:- end_tests(probation_supervision_intensification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72): High and rising. The constraint extracts money (supervision fees stacking to prohibitive levels) and freedom (monitoring immobilization with no legitimate exit). The rise from 0.35 to 0.72 over 15 years tracks the intensification of fee stacking and expanded monitoring requirements. Suppression (0.81): High and rising. The suppression is structural (legal authority to reincarcerate for noncompliance), technological (GPS/RF monitoring with constant tracking), and financial (unaffordable fees block legal compliance). Theater ratio (0.58): Moderate. The constraint is theatrically framed as 'alternatives to incarceration' and 'community supervision'—benign language that obscures the immobilization and extraction. However, the core function (tracking location, enforcing curfew) is not purely theatrical—it genuinely restricts movement. The theater is in the legitimacy narrative, not in the mechanism itself. Accessibility collapse (0.88 individual, 0.48 structural): The critical leveled pattern. Individual probationers see nearly total collapse of alternatives (0.88 by year 15): they cannot afford to comply (fees too high), cannot fail to comply (reincarceration), and cannot legally exit (probation terms are fixed). Structural observers see the system as having alternatives (0.48): the apparatus is presented as a choice between incarceration and probation, and governments claim space to adjust. This inversion—maximum individual coercion, moderate structural visibility—is the core mechanism of snare dynamics. Stakeholders experience maximum pressure where they have zero power (individuals), and moderate/acceptable pressure where they have institutional power (departments, vendors).
 *
 * PERSPECTIVAL GAP:
 *   The probationer sees snare (trapped, extracted, unaffordable compliance). The probation department sees tangled rope (genuine coordination—budget constraints—mixed with embedded extraction through fees). The private vendor sees rope (solving a real market demand, tracking genuinely works). The county budget sees benefit (revenue, reduced incarceration costs). The analytical observer risks seeing mountain (unavoidable overhead of probation management) but the high beneficiary structure and rising extraction metrics reveal a false summit. The constraint is only 'natural' if one assumes probation fees and intensified monitoring are inevitable—they are not. The perspectival gap reveals whose interests the 'inevitability' serves.
 *
 * DIRECTIONALITY LOGIC:
 *   The engine derives d (directionality toward extraction) from agent power and exit options combined with beneficiary/victim status. Probationers (powerless/trapped) → d=1.0 (full targets). Probation departments (moderate/constrained) → d=0.65 (constrained exit, partial beneficiary status through fee revenue). Vendors (institutional/arbitrage) → d=0.15 (beneficiary, high exit capacity). The high d values for powerless agents combined with structural suppression and beneficiary presence produce the snare classification. The constraint's effective extraction chi is amplified because the primary targets (probationers) have zero mobility and maximum suppression applies to them specifically.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits resolved mandatrophy: the original mandate was 'supervise probation populations without mass incarceration.' That mandate was live and legitimate. However, intensified monitoring and fee stacking have converted the apparatus from a supervision mechanism into an extraction and immobilization mechanism. The original coordination function (managing probation populations) has been subordinated to the extraction function (capturing revenue, immobilizing bodies). The mandatrophy is resolved by recognizing that the constraint now serves the extraction, not the supervision. The narrative framing ('alternatives to incarceration') preserves the ghost of the original mandate while the apparatus operates as snare. Declaring mandatrophy_resolved=true captures this: the constraint's founding mandate has been outlived by its actual function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fee_proportionality_threshold,
    'At what point do supervision fees become prohibitive barriers to compliance, converting the system from accountability into extraction machinery?',
    'Empirical analysis of fee levels vs median probationer income; correlation between fee burden and technical violations (missing payments vs new crimes); post-supervision debt burden tracking.',
    'If fees are set below ~5% of median income: framing as legitimate cost-sharing is defensible. If fees exceed 15%: extracted amount is clearly coercive and the classification shifts firmly toward snare. Current ranges (15-40% of income) suggest intentional extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fee_proportionality_threshold, empirical, 'Threshold at which supervision fees become prohibitive barriers').

omega_variable(
    monitoring_effectiveness_paradox,
    'Do intensified electronic monitoring requirements actually reduce recidivism, or do they increase technical violations and reincarceration without changing underlying crime rates?',
    'Longitudinal recidivism tracking: comparison of new-crime rates vs technical-violation rates in high-monitoring vs low-monitoring jurisdictions; analysis of whether monitoring catches new offenses or primarily catches compliance failures unrelated to criminal behavior.',
    'If monitoring reduces new crimes: apparatus is coordinating (though still extractive through fees). If monitoring only increases technical violations: the constraint is pure supervision intensification for extraction purposes, snare classification is confirmed. Evidence to date suggests the latter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(monitoring_effectiveness_paradox, empirical, 'Whether monitoring reduces crime or only increases technical violations').

omega_variable(
    reentry_resource_displacement,
    'What fraction of probation departments'' fee revenue displaces or supplements general supervision budgets vs reentry services?',
    'Budget analysis comparing pre/post fee-stacking jurisdictions; audit of allocation patterns; survey of department heads on fee-revenue use.',
    'If fees primarily supplement existing budgets with no reentry displacement: extraction is lower than estimated. If fees substitute for general appropriations and reduce reentry investment: the constraint is definitely snare (extraction masked as cost-recovery).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reentry_resource_displacement, empirical, 'Whether supervision fees displace reentry services').

omega_variable(
    technological_determinism_lock,
    'Is the intensification of electronic monitoring driven by genuine public safety need or by vendor market capture and path dependence in adoption?',
    'Historical analysis of policy adoption: what evidence/threat justified each escalation? Comparison to jurisdictions that have reduced monitoring: what changed? Vendor lobbying records vs policy rationales.',
    'If adoption driven by evidence: constraint may be justified tangled rope. If adoption driven by vendor capture and institutional inertia: confirms snare classification and reveals false summit dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_determinism_lock, empirical, 'Whether monitoring intensification is driven by need or vendor capture').

omega_variable(
    identity_locked_reentry_identity,
    'Does the probation regime lock probationers into a ''monitored person'' identity that persists even after formal supervision ends, constraining reentry even absent structural barriers?',
    'Post-probation trajectory analysis: comparison of employment, housing access, and social integration between those who completed probation in high-monitoring vs low-monitoring regimes; qualitative interviews on identity dissolution post-supervision.',
    'If identity lock is substantial: adds internalized suppression layer on top of structural extraction. Even after removal from probation, individuals carry monitoring norms and assume continued surveillance. Classification remains snare but with amplified suppression mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_reentry_identity, conceptual, 'Whether monitoring creates persistent identity lock').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(probation_supervision_intensification, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prob_super_tr_t0, probation_supervision_intensification, theater_ratio, 0, 0.42).
narrative_ontology:measurement_basis(prob_super_tr_t0, observed).
narrative_ontology:measurement(prob_super_tr_t5, probation_supervision_intensification, theater_ratio, 5, 0.5).
narrative_ontology:measurement_basis(prob_super_tr_t5, observed).
narrative_ontology:measurement(prob_super_tr_t10, probation_supervision_intensification, theater_ratio, 10, 0.58).
narrative_ontology:measurement_basis(prob_super_tr_t10, observed).
narrative_ontology:measurement(prob_super_tr_t15, probation_supervision_intensification, theater_ratio, 15, 0.62).
narrative_ontology:measurement_basis(prob_super_tr_t15, projected).

% Extraction over time
narrative_ontology:measurement(prob_super_be_t0, probation_supervision_intensification, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(prob_super_be_t0, observed).
narrative_ontology:measurement(prob_super_be_t5, probation_supervision_intensification, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(prob_super_be_t5, observed).
narrative_ontology:measurement(prob_super_be_t10, probation_supervision_intensification, base_extractiveness, 10, 0.72).
narrative_ontology:measurement_basis(prob_super_be_t10, observed).
narrative_ontology:measurement(prob_super_be_t15, probation_supervision_intensification, base_extractiveness, 15, 0.68).
narrative_ontology:measurement_basis(prob_super_be_t15, projected).

% Suppression requirement over time
narrative_ontology:measurement(prob_super_su_t0, probation_supervision_intensification, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(prob_super_su_t0, observed).
narrative_ontology:measurement(prob_super_su_t5, probation_supervision_intensification, suppression_requirement, 5, 0.71).
narrative_ontology:measurement_basis(prob_super_su_t5, observed).
narrative_ontology:measurement(prob_super_su_t10, probation_supervision_intensification, suppression_requirement, 10, 0.81).
narrative_ontology:measurement_basis(prob_super_su_t10, observed).
narrative_ontology:measurement(prob_super_su_t15, probation_supervision_intensification, suppression_requirement, 15, 0.79).
narrative_ontology:measurement_basis(prob_super_su_t15, projected).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=15
narrative_ontology:measurement(prob_super_grid_01, probation_supervision_intensification, accessibility_collapse(class), 0, 0.58).
narrative_ontology:measurement(prob_super_grid_02, probation_supervision_intensification, accessibility_collapse(class), 15, 0.77).
narrative_ontology:measurement(prob_super_grid_03, probation_supervision_intensification, accessibility_collapse(individual), 0, 0.72).
narrative_ontology:measurement(prob_super_grid_04, probation_supervision_intensification, accessibility_collapse(individual), 15, 0.88).
narrative_ontology:measurement(prob_super_grid_05, probation_supervision_intensification, accessibility_collapse(organizational), 0, 0.45).
narrative_ontology:measurement(prob_super_grid_06, probation_supervision_intensification, accessibility_collapse(organizational), 15, 0.62).
narrative_ontology:measurement(prob_super_grid_07, probation_supervision_intensification, accessibility_collapse(structural), 0, 0.35).
narrative_ontology:measurement(prob_super_grid_08, probation_supervision_intensification, accessibility_collapse(structural), 15, 0.48).
narrative_ontology:measurement(prob_super_grid_09, probation_supervision_intensification, resistance(class), 0, 0.42).
narrative_ontology:measurement(prob_super_grid_10, probation_supervision_intensification, resistance(class), 15, 0.48).
narrative_ontology:measurement(prob_super_grid_11, probation_supervision_intensification, resistance(individual), 0, 0.18).
narrative_ontology:measurement(prob_super_grid_12, probation_supervision_intensification, resistance(individual), 15, 0.22).
narrative_ontology:measurement(prob_super_grid_13, probation_supervision_intensification, resistance(organizational), 0, 0.35).
narrative_ontology:measurement(prob_super_grid_14, probation_supervision_intensification, resistance(organizational), 15, 0.41).
narrative_ontology:measurement(prob_super_grid_15, probation_supervision_intensification, resistance(structural), 0, 0.28).
narrative_ontology:measurement(prob_super_grid_16, probation_supervision_intensification, resistance(structural), 15, 0.32).
narrative_ontology:measurement(prob_super_grid_17, probation_supervision_intensification, stakes_inflation(class), 0, 0.52).
narrative_ontology:measurement(prob_super_grid_18, probation_supervision_intensification, stakes_inflation(class), 15, 0.71).
narrative_ontology:measurement(prob_super_grid_19, probation_supervision_intensification, stakes_inflation(individual), 0, 0.64).
narrative_ontology:measurement(prob_super_grid_20, probation_supervision_intensification, stakes_inflation(individual), 15, 0.85).
narrative_ontology:measurement(prob_super_grid_21, probation_supervision_intensification, stakes_inflation(organizational), 0, 0.38).
narrative_ontology:measurement(prob_super_grid_22, probation_supervision_intensification, stakes_inflation(organizational), 15, 0.52).
narrative_ontology:measurement(prob_super_grid_23, probation_supervision_intensification, stakes_inflation(structural), 0, 0.28).
narrative_ontology:measurement(prob_super_grid_24, probation_supervision_intensification, stakes_inflation(structural), 15, 0.41).
narrative_ontology:measurement(prob_super_grid_25, probation_supervision_intensification, suppression(class), 0, 0.48).
narrative_ontology:measurement(prob_super_grid_26, probation_supervision_intensification, suppression(class), 15, 0.68).
narrative_ontology:measurement(prob_super_grid_27, probation_supervision_intensification, suppression(individual), 0, 0.68).
narrative_ontology:measurement(prob_super_grid_28, probation_supervision_intensification, suppression(individual), 15, 0.84).
narrative_ontology:measurement(prob_super_grid_29, probation_supervision_intensification, suppression(organizational), 0, 0.42).
narrative_ontology:measurement(prob_super_grid_30, probation_supervision_intensification, suppression(organizational), 15, 0.58).
narrative_ontology:measurement(prob_super_grid_31, probation_supervision_intensification, suppression(structural), 0, 0.22).
narrative_ontology:measurement(prob_super_grid_32, probation_supervision_intensification, suppression(structural), 15, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(probation_supervision_intensification, enforcement_mechanism).
narrative_ontology:affects_constraint(probation_supervision_intensification, carceral_labor_extraction).
narrative_ontology:affects_constraint(probation_supervision_intensification, recidivism_measurement_gaming).
narrative_ontology:affects_constraint(probation_supervision_intensification, probationer_debt_accumulation).

% DUAL FORMULATION NOTE:
% This constraint is upstream of carceral_labor_extraction (if probationers cannot afford fees, they may enter formal work programs or informal coerced labor). It affects recidivism_measurement_gaming (technical violations from fee noncompliance are counted as recidivism, inflating failure metrics). It drives probationer_debt_accumulation, which is a separate extractive constraint with different epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(probation_supervision_intensification, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
