% ============================================================================
% CONSTRAINT STORY: substance_control_authority__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_authority__prohibition_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: substance_control_authority__prohibition_reading
 *   human_readable: State Criminalization of Drug Possession to Protect Third Parties (Prohibition Reading)
 *   domain: criminal_justice/public_health_policy/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the PROHIBITION READING of the contested
 *   kernel 'substance_control_authority.' It represents the state's use of
 *   criminal law to prohibit drug possession and use, framed as protecting
 *   third parties from drug-related crime and social disorder. The reading
 *   asserts that criminal prohibition—enforced through arrest, prosecution,
 *   incarceration, and collateral consequences (employment/housing/voting
 *   bars)—protects property owners and neighborhoods from drug markets and
 *   drug-related crime by deterring drug use and incapacitating users. The
 *   structural analysis reveals this constraint as a SNARE: it extracts
 *   heavily from people who use drugs and from communities subject to
 *   enforcement disparities, while producing ambiguous third-party protection
 *   benefits and persisting despite 50+ years of evidence that
 *   criminalization has not reduced drug prevalence, addiction, or drug
 *   markets. The constraint's high suppression (0.78) derives from criminal
 *   sanction, felony conviction consequences, and barriers to exit (no
 *   decriminalization is possible without state action). The extractiveness
 *   trajectory shows accumulation: rising from 0.42 at the constraint's
 *   origins (early 20th century, pre-mandatory minimums) to 0.68 during the
 *   'War on Drugs' era (1970-present), plateauing at 0.68 as the carceral
 *   system stabilizes. The theater ratio rises from 0.35 (early enforcement
 *   with modest institutional complexity) to 0.55 (contemporary era with
 *   mandatory minimums, three-strikes provisions, and sentencing enhancements
 *   that create performative punishment divorced from deterrent effect). This
 *   constraint differs fundamentally from the harm_reduction_reading (which
 *   accepts drug use while minimizing health harms) and the
 *   legalization_reading (which regulates drugs as legal commerce). All three
 *   readings coexist in contemporary discourse, held by different political
 *   coalitions and jurisdictions. None logically forecloses the others within
 *   a single state framework—they represent different allocations of state
 *   authority and different empirical premises about what protects third
 *   parties.
 *
 * KEY AGENTS:
 *   - People who use drugs (powerless/trapped): Primary victims. Face criminal liability, incarceration, felony conviction records, and collateral civil rights barriers. No structural exit from the constraint without state action.
 *   - Low-income and communities of color (moderate/constrained): Secondary victims. Subject to disproportionate enforcement despite comparable or lower drug use rates. Can relocate at high cost but cannot escape enforcement pattern.
 *   - Property owners in high-crime districts (powerful/arbitrage): Primary beneficiary. Benefit from reduced visible drug markets and street-level disorder. Mobile exit (can relocate to other neighborhoods); arbitrage across jurisdictions with different enforcement intensities.
 *   - Law enforcement institutions (institutional/arbitrage): Secondary beneficiary. Receive operational mandate, resource allocation, performance metrics (arrest counts), and institutional legitimacy from drug criminalization. Institutional arbitrage: could reallocate resources if drug enforcement declined.
 *   - Carceral system and prosecutors (institutional/arbitrage): Tertiary beneficiary. Drug prohibition sustains mass incarceration, judicial workload, and prosecutorial discretion. Exhibit piton characteristics: persist through institutional inertia despite evidence of ineffectiveness.
 *   - Harm reduction and legalization advocates (powerless/constrained): Counter-claimants. Trapped in minority political positions; have constrained exit via policy advocacy and litigation; excluded from primary authority allocation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__prohibition_reading, 0.68).
domain_priors:suppression_score(substance_control_authority__prohibition_reading, 0.78).
domain_priors:theater_ratio(substance_control_authority__prohibition_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__prohibition_reading, snare).
narrative_ontology:human_readable(substance_control_authority__prohibition_reading, "State Criminalization of Drug Possession to Protect Third Parties (Prohibition Reading)").
narrative_ontology:topic_domain(substance_control_authority__prohibition_reading, "criminal_justice/public_health_policy/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__prohibition_reading, 'fe5efdee-de48-4475-951c-a7ab38ac289a').
narrative_ontology:cs_kernel_codification('fe5efdee-de48-4475-951c-a7ab38ac289a', formalized).
narrative_ontology:cs_authority_grounding('fe5efdee-de48-4475-951c-a7ab38ac289a', lineage).
narrative_ontology:cs_interpretation_layer_present('fe5efdee-de48-4475-951c-a7ab38ac289a').
narrative_ontology:cs_reading_relation('fe5efdee-de48-4475-951c-a7ab38ac289a', substance_control_authority__harm_reduction_reading, coexists_with).
narrative_ontology:cs_reading_relation('fe5efdee-de48-4475-951c-a7ab38ac289a', substance_control_authority__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('fe5efdee-de48-4475-951c-a7ab38ac289a', foundational, criminal_prohibition_necessary_for_third_party_protection).
narrative_ontology:cs_axiom_status(criminal_prohibition_necessary_for_third_party_protection, holdable).
narrative_ontology:cs_axiom_grounding('fe5efdee-de48-4475-951c-a7ab38ac289a', criminal_prohibition_necessary_for_third_party_protection, empirically_contingent).
narrative_ontology:cs_axiom('fe5efdee-de48-4475-951c-a7ab38ac289a', secondary, incarceration_as_primary_deterrence_mechanism).
narrative_ontology:cs_axiom_status(incarceration_as_primary_deterrence_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('fe5efdee-de48-4475-951c-a7ab38ac289a', incarceration_as_primary_deterrence_mechanism, empirically_contingent).
narrative_ontology:cs_reference_frame('fe5efdee-de48-4475-951c-a7ab38ac289a', drug_prohibition_deterrence_framework).
narrative_ontology:cs_drift_state('fe5efdee-de48-4475-951c-a7ab38ac289a', contemporary_post_war_on_drugs_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fe5efdee-de48-4475-951c-a7ab38ac289a', '').
narrative_ontology:cs_kernel_id(substance_control_authority__prohibition_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, property_owners_in_high_crime_districts).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, law_enforcement_institutional_interests).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, political_coalitions_supporting_tough_crime_policies).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, people_who_use_drugs).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, communities_subject_to_enforcement_disparities).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, defendants_convicted_under_prohibition_statutes).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CRIMINALIZED PERSON (SNARE) — Faces criminal liability for possession; options are severely restricted by legal barriers (felony conviction bars employment, housing, voting). Exit is trapped: decriminalization would require changing state law, which the individual cannot control. Maximum experienced extraction via loss of civil rights, incarceration, and economic marginalization. No coordination function; pure suppression through criminal sanction.
constraint_indexing:constraint_classification(substance_control_authority__prohibition_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RACIALLY DISPARATE ENFORCEMENT COMMUNITY (SNARE) — Low-income communities and communities of color experience disproportionate enforcement, drug arrests, and incarceration despite similar or lower drug use rates. Constrained by geography and structural barriers; can relocate at high cost but cannot escape the enforcement pattern. Extraction is asymmetric: the deterrence benefit flows to wealthier areas; enforcement costs concentrate in poor neighborhoods.
constraint_indexing:constraint_classification(substance_control_authority__prohibition_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROPERTY OWNER IN HIGH-CRIME DISTRICT (TANGLED ROPE) — Benefits from reduced visible drug markets and lower street-level disorder. Criminalization coordinates reduction in open-air drug sales; this is a genuine coordination function. However, the extraction is asymmetric: the beneficiary avoids enforcement entirely (they are not the target), while costs are pushed to other agents. Exit is mobile (via relocation or arbitrage across jurisdictions with different enforcement intensities). Moderate experienced extraction because the beneficiary has exit and is not the primary target.
constraint_indexing:constraint_classification(substance_control_authority__prohibition_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LAW ENFORCEMENT INSTITUTION (ROPE) — Criminalization provides operational mandate, resource allocation, performance metrics (arrest counts), and institutional legitimacy. Drug enforcement funding, DEA budget, police narcotics divisions, and prison expansion all depend on the criminalization framework. Institutional beneficiary with arbitrage: can reallocate resources if drug law enforcement declined, but would lose institutional raison d'être. Sees criminalization as a coordination mechanism that enables their core institutional function.
constraint_indexing:constraint_classification(substance_control_authority__prohibition_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CARCERAL SYSTEM (PITON) — From the generational view, criminalization has sustained the prison system through inertial institutional arrangements rather than functional effectiveness. Drug incarceration is a major driver of U.S. mass incarceration, but scholarly consensus shows this has not proportionally reduced drug use or markets. The system persists through institutional inertia, judicial precedent, and career investment by prosecutors and corrections officials, not because it achieves its stated deterrence goal. Theater ratio is substantial: mandatory minimums, three-strikes provisions, and sentencing enhancements create performative punishment without corresponding public safety gains.
constraint_indexing:constraint_classification(substance_control_authority__prohibition_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a civilizational and global perspective, prohibition mechanisms show persistent structural failure: 50+ years of drug prohibition globally have not eliminated drug markets, reduced addiction rates, or prevented supply chains. The constraint persists despite evidence of ineffectiveness, indicating it is maintained by structural interests (incarceration industry, law enforcement budgets, political coalitions) rather than by its stated functional goal. The analytical observer sees this as a snare: the constraint extracts from users and marginalized communities while producing minimal third-party protection, and is sustained by suppressing alternative frameworks (harm reduction, legalization, decriminalization).
constraint_indexing:constraint_classification(substance_control_authority__prohibition_reading, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__prohibition_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(substance_control_authority__prohibition_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(substance_control_authority__prohibition_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_authority__prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(substance_control_authority__prohibition_reading, TR),
    TR >= 0.70.

:- end_tests(substance_control_authority__prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts substantially from criminalized persons (loss of liberty, employment, housing, voting rights) and from enforcement-disparate communities (disproportionate incarceration, family disruption, economic marginalization). The extraction magnitude is not maximal (0.95+) because the beneficiary (property owners) and enforcement institutions are not capturing ALL value—some extraction is dissipated in inefficient carceral overhead, and some is remitted back to public goods (incarceration capacity). The trajectory from 0.42 to 0.68 reflects the intensification of drug criminalization during the War on Drugs era (mandatory minimums, three-strikes, asset forfeiture expansion), not a change in the constraint's core structure. The stabilization at 0.68 reflects that further intensification faces practical and political limits. Suppression (0.78): High. Criminal liability, felony conviction, and collateral consequences (employment/housing/voting/benefits bars) constitute severe barriers to exit. Suppression would be higher (0.85+) if civil commitment or forced treatment were routine, but criminal suppression alone produces 0.78. The rise from 0.55 to 0.78 reflects the accumulation of collateral consequences statutes and the normalization of enforcement. Theater ratio (0.55): Moderate. Early drug prohibition had some coordination function (public health rationale, disease prevention framing). Contemporary prohibition shows substantial theater: mandatory minimums produce lengthy sentences divorced from individual harm calculus; three-strikes enhancements create performative punishment; asset forfeiture operates independently of guilt/innocence; sentencing enhancements create implicit plea bargaining rather than genuine adjudication. However, the theater is not overwhelming (0.70+) because criminal enforcement does incapacitate some actors and does reduce visible drug markets in targeted neighborhoods—the mechanism works, albeit at high cost and with disparate impact.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a maximal perspectival gap. Criminalized persons see a snare with no exit. Enforcement-disparate communities see a snare with constrained exit. Property owners see a tangled rope with coordination benefits (visible drug market reduction) and asymmetric extraction that they do not bear. Law enforcement sees a rope—criminalization is their coordinating mechanism. The carceral system sees a piton—the constraint persists through inertia despite evidence of ineffectiveness. The analytical observer sees a snare that persists despite 50+ years of empirical evidence that criminalization does not reduce drug prevalence or drug-related crime, suggesting the constraint is maintained by structural interests (incarceration industry, law enforcement budgets, political coalitions) rather than by achievement of its stated goal. This perspectival divergence is diagnostic: the beneficiary's rope experience and the victim's snare experience cannot be reconciled within a single framework—they reflect genuine asymmetry in who bears costs and who captures benefits.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness (χ) is computed from base extractiveness (ε=0.68), the agent's directionality value (d), and the scope modifier σ(S). Criminalized persons have high d (0.95, near full target) because they are primary victims with no exit; f(d) ≈ 1.42 produces χ ≈ 0.68 × 1.42 × 1.0 ≈ 0.96 (experienced as near-maximal snare). Enforcement-disparate communities have high d (0.85, target due to enforcement disparities) and national scope; f(d) ≈ 1.15 produces χ ≈ 0.68 × 1.15 × 1.0 ≈ 0.78 (experienced as high snare). Property owners have low d (0.15, beneficiary) because they benefit from the constraint; f(d) ≈ -0.01 produces χ ≈ 0.68 × (-0.01) × 1.0 ≈ -0.007 (experienced as slight coordination benefit, no extraction cost). Law enforcement has d ≈ 0.10 (strong beneficiary with institutional arbitrage); f(d) ≈ -0.10 produces χ ≈ 0.68 × (-0.10) × 1.0 ≈ -0.068 (experienced as rope: coordination without extraction). Carceral system has d ≈ 0.12; f(d) ≈ -0.08 produces χ ≈ 0.68 × (-0.08) × 1.0 ≈ -0.054, but the piton classification derives from theater_ratio ≥ 0.70, not from low χ. The analytical observer at civilizational scope treats d ≈ 0.72 (canonical analytical directionality) and global scope σ=1.2; f(d) ≈ 1.15 produces χ ≈ 0.68 × 1.15 × 1.2 ≈ 0.94 (experienced as near-maximal snare at civilizational scale).
 *
 * MANDATROPHY ANALYSIS:
 *   The prohibition_reading exhibits high mandatrophy risk at ε > 0.70. The core claim—that criminalization protects third parties by deterring drug use—is contested by 50+ years of empirical evidence showing that prohibition does not reduce drug prevalence, addiction, or drug market size. Harm reduction jurisdictions (e.g., Portugal, supervised consumption sites in Europe) report lower overdose mortality and lower incarceration without increased drug use. Legalization jurisdictions (e.g., regulated cannabis) show stable or declining use without the incarceration burden. The prohibition_reading persists despite this evidence, indicating it is maintained by structural interests (incarceration industry, law enforcement budgets, political coalitions, prosecutorial discretion) rather than by empirical vindication of its core claim. The mandate is at risk: if long-term outcome comparisons (omega 4) establish that harm reduction and legalization produce better third-party protection with lower incarceration, the prohibition axiom would be empirically overridden. The committer frame routes this through the reading_relations (coexists_with harm_reduction_reading and legalization_reading) and axiom_status (holdable but empirically contested). The constraint's stability depends on suppressing or delegitimizing alternative readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_efficacy_contested,
    'Does criminalization of drug possession measurably deter drug use or reduce drug-related crime, or does it primarily redistribute costs without preventing underlying behaviors?',
    'Longitudinal comparison of drug use prevalence, crime rates, and addiction incidence in jurisdictions with varying criminalization intensity; cross-national comparison of prohibition vs decriminalization regimes (e.g., Portugal vs. U.S.); analysis of whether enforcement increases during periods of rising drug use or declining drug use.',
    'If deterrence is effective: snare classification may underweight the coordination benefit; constraint could shift to tangled_rope. If deterrence is ineffective: snare classification holds; constraint is extraction disguised as protection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrence_efficacy_contested, empirical, 'Whether criminalization demonstrably reduces drug use or drug-related crime').

omega_variable(
    third_party_protection_mechanisms,
    'Which specific third-party harms does criminalization prevent? Are these harms prevented by deterrence (users stay away from drugs), by incapacitation (incarcerated users cannot commit crimes), or merely displaced to other populations?',
    'Analysis of crime victims in communities with high drug enforcement vs. low enforcement; tracking of whether reduction in visible drug markets correlates with reduction in drug-related property crime or violent crime, or only with displacement; assessment of spillover harms (family separation, community policing friction, economic disruption) from enforcement itself.',
    'If harms prevented are legitimate and substantial: tangled_rope classification more appropriate. If harms prevented are marginal or speculative: snare classification held; the third-party protection claim becomes aspirational cover for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_party_protection_mechanisms, empirical, 'What specific third-party harms criminalization prevents').

omega_variable(
    enforcement_inequality_structural,
    'Are enforcement disparities (racial disparities in drug arrests, prosecution, sentencing) incidental to prohibition enforcement or structural to it?',
    'Analysis of whether enforcement disparities persist controlling for drug use prevalence by race/class; assessment of prosecutorial discretion and sentencing guidelines; examination of whether decriminalization vs. legalization of other drugs (e.g., alcohol, tobacco) shows similar disparity patterns; comparison to jurisdictions with explicit anti-disparities enforcement protocols.',
    'If incidental: enforcement reform could reduce disparities while maintaining prohibition. If structural: disparities are an intrinsic feature of criminalization as a control mechanism; snare classification and victim set definition hold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_inequality_structural, empirical, 'Whether enforcement disparities are structural to prohibition').

omega_variable(
    alternative_reading_empirical_pressure,
    'What empirical evidence and policy outcomes would establish the superiority of the harm_reduction_reading or legalization_reading relative to the prohibition_reading?',
    'Long-term outcome tracking in jurisdictions adopting harm reduction (e.g., supervised consumption sites, medication-assisted treatment, naloxone distribution) or legalization (e.g., regulated cannabis markets, decriminalized drug possession); cross-temporal comparison within jurisdictions that have transitioned between readings.',
    'If harm reduction and legalization jurisdictions show better third-party protection, lower incarceration, and reduced overdose mortality: prohibition_reading loses empirical grounding and would be reclassified as foreclosed or overridden. If prohibition outcomes improve: alternative readings face empirical pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_empirical_pressure, empirical, 'Empirical outcomes testing relative superiority of prohibition vs. alternative readings').

omega_variable(
    constitutional_legitimacy_contingent,
    'Is the state''s authority to criminalize drug possession grounded in constitutional text (e.g., commerce power, police power) or in judicial precedent and political convention that could be reinterpreted?',
    'Constitutional law analysis of commerce clause and police power limits; examination of whether courts could reinterpret due process or privacy doctrines to constrain drug criminalization; analysis of whether future courts might adopt substantive due process protections for bodily autonomy (parallel to Dobbs repudiation of prior precedent).',
    'If grounded in contingent judicial reading: alternative readings (harm reduction, legalization) are coexistent, not foreclosed. If reinterpreted: prohibition reading could be formally overridden. If grounded in durable constitutional doctrine: prohibition reading has stronger authority grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_legitimacy_contingent, conceptual, 'Whether state authority to criminalize drug possession is constitutionally contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__prohibition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subst_prohib_theater_t0, substance_control_authority__prohibition_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(subst_prohib_theater_t20, substance_control_authority__prohibition_reading, theater_ratio, 20, 0.55).
narrative_ontology:measurement(subst_prohib_theater_t50, substance_control_authority__prohibition_reading, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(subst_prohib_extract_t0, substance_control_authority__prohibition_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(subst_prohib_extract_t20, substance_control_authority__prohibition_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(subst_prohib_extract_t50, substance_control_authority__prohibition_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(subst_prohib_supp_t0, substance_control_authority__prohibition_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(subst_prohib_supp_t20, substance_control_authority__prohibition_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(subst_prohib_supp_t50, substance_control_authority__prohibition_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, substance_control_authority__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, substance_control_authority__legalization_reading).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, mass_incarceration_structural_incentive).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, enforcement_disparities_in_drug_prosecution).

% DUAL FORMULATION NOTE:
% The substance_control_authority kernel decomposes into three constraint stories representing different readings: prohibition_reading (this file), harm_reduction_reading (public health minimization of harms while accepting drug use), and legalization_reading (regulated markets as legal commerce). Each reading has distinct ε, beneficiary/victim sets, and classification type. They are linked by kernel structure, not by causal dependency. Each reading's omegas address the empirical contests between readings (omegas 1-4 in this file) and the constitutional contingency of authority grounding (omega 5).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
