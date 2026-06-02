% ============================================================================
% CONSTRAINT STORY: maternal_hbsag_screening
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maternal_hbsag_screening, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: maternal_hbsag_screening
 *   human_readable: Maternal HBsAg Screening and Perinatal Hepatitis B Prevention
 *   domain: public_health/maternal_medicine/infectious_disease
 *
 * SUMMARY:
 *   Maternal HBsAg screening during pregnancy is a public health intervention
 *   designed to prevent perinatal hepatitis B transmission through
 *   identification of infected mothers and implementation of birth-dose
 *   immunization and/or antiviral prophylaxis for exposed infants. The
 *   constraint exhibits tangled coordination: genuine coordination benefit
 *   (preventing transmission) coexists with asymmetric extraction through
 *   mandatory disclosure, variable treatment access, pharmaceutical benefit
 *   capture, and healthcare system burden. The classification varies
 *   dramatically by observer position: for powerless women in low-resource
 *   settings without treatment access, screening appears as pure extraction
 *   (Snare); for public health authorities solving a coordination problem, it
 *   is pure coordination (Rope); for organized international actors with
 *   pathway redundancy (vaccination scaling), it is temporary (Scaffold). The
 *   theater_ratio (0.55) reflects that screening protocols include elements
 *   with questionable clinical utility (repeated third-trimester screening in
 *   initially negative women) maintained through administrative inertia.
 *
 * KEY AGENTS:
 *   - Pregnant Women (Low-Resource Settings): Primary victim (powerless/trapped) — mandatory screening with no exit; bear suppression through limited access to results, treatment, and care coordination
 *   - Pregnant Women (With Treatment Access): Secondary victim-beneficiary (moderate/constrained) — genuine benefit from early intervention but face constraints from disclosure requirements, partner notification, and treatment adherence
 *   - Public Health Authorities: Primary beneficiary (institutional/arbitrage) — solve coordination problem of perinatal transmission prevention; have flexibility in implementation protocols
 *   - WHO and International Vaccine Coalition: Organized actor (organized/mobile) — see screening as transitional mechanism; have pathway diversity through vaccination scale-up and treatment expansion
 *   - Vaccine and Antiviral Manufacturers: Beneficiary (powerful/mobile) — profit from demand generation through screening; coordinate market access but maintain extraction through pricing
 *   - Healthcare Billing and Administrative Systems: Institutional inertia actor (institutional/arbitrage) — maintain screening protocols through administrative routine; perpetuate theater through compliance documentation
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing contingent institutional arrangements as epidemiological necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maternal_hbsag_screening, 0.38).
domain_priors:suppression_score(maternal_hbsag_screening, 0.48).
domain_priors:theater_ratio(maternal_hbsag_screening, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maternal_hbsag_screening, extractiveness, 0.38).
narrative_ontology:constraint_metric(maternal_hbsag_screening, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(maternal_hbsag_screening, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maternal_hbsag_screening, tangled_rope).
narrative_ontology:human_readable(maternal_hbsag_screening, "Maternal HBsAg Screening and Perinatal Hepatitis B Prevention").
narrative_ontology:topic_domain(maternal_hbsag_screening, "public_health/maternal_medicine/infectious_disease").

domain_priors:requires_active_enforcement(maternal_hbsag_screening).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maternal_hbsag_screening, newborn_infants).
narrative_ontology:constraint_beneficiary(maternal_hbsag_screening, public_health_systems).
narrative_ontology:constraint_beneficiary(maternal_hbsag_screening, vaccine_manufacturers).
narrative_ontology:constraint_victim(maternal_hbsag_screening, pregnant_women_with_limited_access).
narrative_ontology:constraint_victim(maternal_hbsag_screening, healthcare_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PREGNANT WOMEN (LOW-RESOURCE) (SNARE) — Mandatory screening with no genuine exit option. Women in under-resourced health systems face screening pressure without guaranteed access to timely results, antiviral therapy, or immunoprophylaxis for infants. Suppression is structural: no alternative pathway exists; non-compliance risks stigma and barriers to prenatal care. Maximum extraction from this agent's perspective.
constraint_indexing:constraint_classification(maternal_hbsag_screening, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PREGNANT WOMEN (WITH ACCESS) (TANGLED ROPE) — Screening provides genuine coordination benefit (early intervention prevents transmission) but also requires disclosure, potential partner notification, medication adherence, and repeated testing. Benefits and costs are asymmetric: newborns benefit most; mothers bear medical and psychological costs. Constrained exit — women can decline screening but face pressure from healthcare systems and family planning expectations.
constraint_indexing:constraint_classification(maternal_hbsag_screening, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PUBLIC HEALTH AUTHORITIES (ROPE) — Screening programs solve a genuine coordination problem: preventing perinatal transmission requires identifying HBsAg-positive mothers before delivery. From this perspective, the constraint is pure coordination with minimal coercive overhead. Health authorities have arbitrage options (can shift screening protocols, modify enforcement intensity, adjust resource allocation). Net benefit — no extraction flows toward them; they coordinate a genuine collective good.
constraint_indexing:constraint_classification(maternal_hbsag_screening, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL VACCINE COALITION (SCAFFOLD) — HBsAg screening exists within a larger infrastructure of hepatitis B vaccination, antiviral access, and birth-dose immunization. This coalition sees screening as a transitional mechanism: as universal infant vaccination expands and treatment accessibility improves, the structural need for intensive maternal screening becomes conditional. Sunset clause: once HBV vaccination coverage exceeds 95% globally and perinatal treatment is accessible in all settings, the screening mandate becomes redundant. Organized agents can navigate this transition; they experience the constraint as temporary.
constraint_indexing:constraint_classification(maternal_hbsag_screening, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: BILLING AND ADMINISTRATIVE SYSTEMS (PITON) — Screening generates billing codes, compliance reports, and documentation workflows that persist through institutional inertia. Many health systems continue three-trimester screening (first, second, third trimester) despite evidence that third-trimester screening has minimal clinical utility if first-trimester results are negative. The administrative requirement (theater_ratio = 0.55) reflects performative compliance with historical screening protocols. These systems have arbitrage options but maintain status quo because change requires coordination across multiple institutions.
constraint_indexing:constraint_classification(maternal_hbsag_screening, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: PHARMACEUTICAL MANUFACTURERS (TANGLED ROPE) — Screening programs generate market demand for HBsAg testing reagents, vaccines, and antivirals. Manufacturers benefit from expanded screening (coordinate access to market) but also extract through price-setting power for vaccines in low-resource markets and for antivirals where treatment alternatives are limited. Mobile exit option — can shift products or markets — but extraction persists through market positioning. Coordination function (ensuring consistent supply) is genuine; asymmetric benefit extraction also exists.
constraint_indexing:constraint_classification(maternal_hbsag_screening, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a pure epidemiological perspective, perinatal hepatitis B transmission prevention requires identifying infectious mothers. The constraint could appear as an immutable feature of infectious disease control — there is no escape from the biological fact that untreated HBsAg-positive mothers transmit to infants. However, the structural data contradicts the mountain classification: screening timing, intensity, and enforcement pathways are all contingent institutional choices, not biological necessities. The engine will detect this as a false summit.
constraint_indexing:constraint_classification(maternal_hbsag_screening, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maternal_hbsag_screening_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(maternal_hbsag_screening, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(maternal_hbsag_screening, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(maternal_hbsag_screening, TR),
    TR >= 0.70.

:- end_tests(maternal_hbsag_screening_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The screening program provides genuine coordination benefit (identifying mothers to prevent transmission) but extraction flows upward through several channels: (1) information asymmetry — women in low-resource settings may not receive timely results or understand implications; (2) pharmaceutical pricing — manufacturers extract through vaccine and antiviral costs; (3) administrative burden — healthcare systems extract labor from pregnant women through repeated screening and documentation. The trajectory shows increasing extractiveness (0.22 → 0.38) as healthcare systems have shifted from one-time screening to multi-trimester protocols and as administrative documentation requirements have expanded. Suppression (0.48): Moderate-high. Barriers to exit are substantial: mandatory screening in most formal healthcare settings, social/family pressure to undergo testing, cultural and religious factors affecting disclosure, and limited treatment alternatives in many regions. However, suppression is not total — home birth and informal healthcare pathways provide partial exit options in some contexts. Theater ratio (0.55): Moderate. Evidence suggests third-trimester screening has minimal clinical utility if first-trimester results are negative and no risk factors intervene (seroconversion rates < 1% in initially negative women). Yet many systems maintain three-trimester protocols due to administrative inertia, regulatory habit, and conservative liability management. This represents moderate theater — not purely performative (first-trimester screening has clear utility) but containing substantial ritualistic elements.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates fundamental perspectival divergence based on structural position and access. The powerless agent in low-resource settings (trapped, immediate horizon) experiences a Snare: mandatory screening with no guarantee of treatment, creating psychological burden and stigma without proportional benefit. The moderate agent with treatment access (constrained, biographical horizon) experiences a Tangled Rope: screening provides genuine prevention benefit but requires accepting disclosure, partner notification, and medication adherence — mixed costs and benefits. The public health authority (institutional, generational horizon) experiences a Rope: screening solves a legitimate coordination problem with minimal overhead from their perspective. The organized international actor (mobile, generational horizon) experiences a Scaffold: sees screening as temporary mechanism being replaced by vaccination and treatment scale-up. The administrative system (institutional, civilizational horizon) experiences a Piton: maintains screening protocols through inertia despite declining marginal utility. The analytical observer risks seeing a Mountain (immutable epidemiological law) but structural data reveals this as false naturalization of contingent institutional choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from agent power, exit options, and beneficiary/victim status. Powerless pregnant women in low-resource settings: trapped exit → high d (0.92) → high f(d) (1.38) → maximum experienced extraction. Moderate pregnant women with treatment access: constrained exit + victim status → moderate d (0.68) → moderate f(d) (1.02) → moderate extraction. Public health authorities: institutional power + arbitrage exit + beneficiary status → low d (0.15) → negative f(d) (-0.01) → minimal experienced extraction (coordination benefit dominates). Vaccine manufacturers: powerful + mobile + beneficiary status → low-moderate d (0.32) → low f(d) (0.18) → modest extraction but coordinated benefit visible. Healthcare systems: institutional + arbitrage + mixed relationship → low d (0.20) → low f(d) (0.02) → performs as coordination from their perspective. The perspectival gap reveals that structural extraction exists but is invisible to institutional beneficiaries and visible only to trapped agents. The scope modifier σ(S) = 1.0 (national) for most perspectives except global perspectives (manufacturers, WHO) where σ = 1.2.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy through decomposition by access level. The classification 'Tangled Rope' is appropriate for integrated systems where screening + treatment + support are coordinated (some high-income and upper-middle-income countries). For low-resource systems where screening exists without treatment access, the correct classification is Snare. The error would be treating 'maternal screening' as a single constraint with single classification across all contexts. Instead: maternal_hbsag_screening_high_access (ε=0.25, Rope) and maternal_hbsag_screening_low_access (ε=0.62, Snare) are separate constraints with different ε values and different beneficiary/victim configurations. The global coordination (Rope from WHO perspective) is real but does not invalidate the extraction (Snare from low-access pregnant women perspective). The mandate is resolved by recognizing that the same intervention label covers structurally distinct constraints in different contexts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    equity_access_threshold,
    'At what level of treatment access does screening transition from extractive (screening without treatment access) to coordinative (screening with guaranteed treatment pathway)?',
    'Analysis of screening impact in settings with varying treatment access; measurement of adverse outcomes (anxiety, stigma, non-engagement) in screened but untreated populations vs treated populations',
    'If access threshold is low (< 30% of screened women receive treatment): current global screening regime is primarily extractive for low-resource settings. If threshold is high (> 70%): current regime functions as intended coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equity_access_threshold, empirical, 'Access threshold at which screening functions as coordination vs extraction').

omega_variable(
    repetitive_screening_utility,
    'Does repeat HBsAg screening in second and third trimester (beyond initial screening) provide clinical utility proportional to cost and burden, or is it primarily theater?',
    'Longitudinal analysis of seroconversion rates in initially negative women; comparison of clinical outcomes between one-time vs repeated screening protocols; assessment of false-positive management burden',
    'If seroconversion < 0.5% in initially negative women: repeat screening is theater (theater_ratio should increase). If seroconversion > 3%: repeat screening has clinical justification (theater_ratio should decrease).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(repetitive_screening_utility, empirical, 'Clinical utility of repeat HBsAg screening').

omega_variable(
    vaccination_surge_coverage_timeline,
    'At what global HBV vaccination coverage rate does the structural utility of maternal screening become conditional rather than mandatory?',
    'Modeling of perinatal transmission risk across vaccination coverage scenarios; historical comparison with other vaccine-preventable disease screening programs; analysis of policy shifts in high-vaccination-coverage regions',
    'If coverage > 90% reduces transmission below detectable rates in screened populations: screening becomes safety-net rather than primary prevention. If coverage < 85% is insufficient: screening remains mandatory even with widespread vaccination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vaccination_surge_coverage_timeline, empirical, 'Vaccination coverage threshold for conditional screening utility').

omega_variable(
    disclosure_harm_asymmetry,
    'Does harm from HBsAg disclosure (stigma, partner notification, insurance/employment discrimination) in low-resource settings exceed benefit from treatment access that is unavailable or inaccessible?',
    'Qualitative analysis of disclosure outcomes in settings with treatment access vs without; measurement of mental health impact, healthcare engagement post-screening, and actual treatment uptake rates',
    'If harm > benefit in untreated populations: mandatory screening becomes net-harmful extraction (snare classification reinforced). If benefit > harm: coordination classification is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disclosure_harm_asymmetry, empirical, 'Balance of disclosure harm vs treatment benefit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maternal_hbsag_screening, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mhbs_tr_t0, maternal_hbsag_screening, theater_ratio, 0, 0.42).
narrative_ontology:measurement(mhbs_tr_t10, maternal_hbsag_screening, theater_ratio, 10, 0.5).
narrative_ontology:measurement(mhbs_tr_t20, maternal_hbsag_screening, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(mhbs_be_t0, maternal_hbsag_screening, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(mhbs_be_t10, maternal_hbsag_screening, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(mhbs_be_t20, maternal_hbsag_screening, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maternal_hbsag_screening, attachment_coordination).
narrative_ontology:affects_constraint(maternal_hbsag_screening, hepatitis_b_vaccination_universal_coverage).
narrative_ontology:affects_constraint(maternal_hbsag_screening, antiviral_access_low_income_countries).
narrative_ontology:affects_constraint(maternal_hbsag_screening, perinatal_transmission_prevention_infrastructure).

% DUAL FORMULATION NOTE:
% Maternal HBsAg screening decomposes into at least two distinct constraints by access level: high-access settings where screening coordinates with treatment and support systems (ε≈0.25, Rope); low-access settings where screening exists without treatment pathway (ε≈0.62, Snare). These are linked by affects_constraints because scaling treatment access shifts the low-access constraint toward the high-access classification. Additionally, universal HBV vaccination coverage would reduce the structural necessity for intensive maternal screening (Scaffold perspective validated).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(maternal_hbsag_screening, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
