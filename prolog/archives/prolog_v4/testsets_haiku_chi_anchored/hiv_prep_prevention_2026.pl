% ============================================================================
% CONSTRAINT STORY: hiv_prep_prevention_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hiv_prep_prevention_2026, []).

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
 *   constraint_id: hiv_prep_prevention_2026
 *   human_readable: PrEP-Mediated HIV Prevention as Sociotechnical Constraint
 *   domain: public_health/pharmaceutical/social
 *
 * SUMMARY:
 *   Pre-exposure prophylaxis (PrEP) represents a biomedical intervention that
 *   simultaneously solves a genuine coordination problem (enabling safer sex
 *   through pharmaceutical prevention) and instantiates an asymmetric
 *   extraction mechanism (global access inequality maintained through patent
 *   enforcement, pricing, and healthcare infrastructure gaps). The constraint
 *   exhibits the core Tangled Rope structure: a real coordination function
 *   (PrEP reduces transmission by >90% when accessed) coexists with severe
 *   asymmetric extraction (high-risk populations in low-income regions lack
 *   access while high-income regions concentrate prevention resources). The
 *   theater ratio (0.58) reflects the performative aspects of clinical
 *   risk-stratification protocols that nominally assess 'who needs PrEP' but
 *   actually preserve scarcity logic and clinical gatekeeping. The
 *   constraint's extractiveness (0.52) has increased over the decade
 *   2015-2025 as patent protection was maintained despite rising global HIV
 *   burden in low-income regions; the generic availability post-2025
 *   represents a potential sunset mechanism if manufacturing, regulatory
 *   approval, and supply-chain barriers are overcome. The suppression (0.68)
 *   combines legal barriers (criminalization of MSM and sex work in 60+
 *   countries), economic barriers ($1,500+ annual cost in-country markets),
 *   healthcare infrastructure gaps (lack of clinics, monitoring capacity),
 *   and social barriers (stigma, self-disclosure requirements). From the
 *   perspective of low-income MSM and sex workers, PrEP is a pure snare:
 *   structurally inaccessible prevention in regions with highest transmission
 *   risk. From the perspective of pharmaceutical manufacturers and
 *   high-income systems, it is a coordination success. From advocacy
 *   organizations, it is a mixed mechanism with sunset potential. The
 *   analytical observer risks naturalizing this inequality as inherent to
 *   medical innovation ('expensive new drugs take time to disseminate
 *   globally') when it is actually a structural arrangement (patent
 *   enforcement, trade agreements, healthcare financing) that is politically
 *   contingent.
 *
 * KEY AGENTS:
 *   - Pharmaceutical manufacturers (Gilead, ViiV, Shionogi): Institutional/arbitrage beneficiaries — monopoly pricing maintains margin, patent protection creates 15-year extraction window
 *   - Low-income MSM populations (sub-Saharan Africa, Asia): Powerless/trapped victims — highest infection risk, lowest access, criminalization prevents healthcare disclosure
 *   - Sex worker populations: Moderate/constrained victims — can access in high-income regions but face criminalization and labor instability that suppress uptake
 *   - Incarcerated populations: Powerless/trapped victims — confined, denied access, 5-15x prevalence, no exit options
 *   - High-income public health systems (CDC, NHS, Medicare): Institutional/arbitrage beneficiaries — coordinate domestic prevention through established infrastructure, drive demand for branded drugs
 *   - Generic manufacturers (Cipla, Mylan) and patent pool: Organized/constrained intermediaries — enabled by patent expiry and TRIPS flexibilities; scaffold mechanism with sunset clause (post-2025)
 *   - HIV prevention advocacy networks (UNAIDS, AMFAR, DWB, MSM coalitions): Organized/constrained — coordinate evidence and policy but structurally constrained by pharma pricing and government criminalization
 *   - High-income nation governments (US, EU, Australia): Powerful/arbitrage — maintain global IP regime that enforces pricing, benefit domestically from prevention, extract globally through trade leverage
 *   - Clinical gatekeeping protocols and risk-stratification: Institutional/constrained — maintenance of scarcity logic through performative assessment (theater_ratio=0.62)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hiv_prep_prevention_2026, 0.52).
domain_priors:suppression_score(hiv_prep_prevention_2026, 0.68).
domain_priors:theater_ratio(hiv_prep_prevention_2026, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hiv_prep_prevention_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(hiv_prep_prevention_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(hiv_prep_prevention_2026, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hiv_prep_prevention_2026, tangled_rope).
narrative_ontology:human_readable(hiv_prep_prevention_2026, "PrEP-Mediated HIV Prevention as Sociotechnical Constraint").
narrative_ontology:topic_domain(hiv_prep_prevention_2026, "public_health/pharmaceutical/social").

domain_priors:requires_active_enforcement(hiv_prep_prevention_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hiv_prep_prevention_2026, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(hiv_prep_prevention_2026, high_income_region_healthcare_systems).
narrative_ontology:constraint_beneficiary(hiv_prep_prevention_2026, prevention_advocates).
narrative_ontology:constraint_victim(hiv_prep_prevention_2026, low_income_msm_populations).
narrative_ontology:constraint_victim(hiv_prep_prevention_2026, sex_workers).
narrative_ontology:constraint_victim(hiv_prep_prevention_2026, incarcerated_populations).
narrative_ontology:constraint_victim(hiv_prep_prevention_2026, sub_saharan_african_nations).
narrative_ontology:constraint_victim(hiv_prep_prevention_2026, hiv_prevention_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME MSM IN SUB-SAHARAN AFRICA (SNARE) — Cannot afford PrEP ($1,500+ annual cost in-country), lacks healthcare infrastructure for monitoring, faces criminalization of homosexuality in 30+ nations, has no exit from infection risk without access. d≈0.96, f(d)≈1.42, σ=1.2 → χ≈0.88. Extraction is severe: PrEP availability in high-income regions is marketed as universal prevention while remaining structurally inaccessible to highest-risk populations.
constraint_indexing:constraint_classification(hiv_prep_prevention_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INCARCERATED POPULATIONS (SNARE) — HIV prevalence 5-15x general population; denied access to PrEP in most US prisons; coercive institutional confinement prevents exit; no autonomy over sexual safety. d≈0.94, f(d)≈1.40, σ=1.0 → χ≈0.73. Pure extraction: state maintains custody and denies prevention tools.
constraint_indexing:constraint_classification(hiv_prep_prevention_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: SEX WORKER COLLECTIVES (TANGLED ROPE) — Can access PrEP in high-income regions but face legal barriers (criminalization), labor instability (no health insurance), and social stigma (self-disclosure required for clinical access). Constrained exit. Also benefit from PrEP: enables safer working conditions, reduces transmission to partners and clients. d≈0.68, f(d)≈1.05, σ=0.9 → χ≈0.50. Mixed: genuine coordination function (enabling safer labor) but suppressed by legal/social hostility.
constraint_indexing:constraint_classification(hiv_prep_prevention_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: HIGH-INCOME PUBLIC HEALTH SYSTEMS (ROPE) — Can distribute PrEP through established healthcare infrastructure. Experiences the constraint as coordination: standardized protocols, insurance coverage, integration with testing. Arbitrage exit: can shift to other prevention modalities if needed. d≈0.12, f(d)≈0.05, σ=1.0 → χ≈0.02. Net beneficiary from the coordination function; extraction is minimal.
constraint_indexing:constraint_classification(hiv_prep_prevention_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PHARMACEUTICAL MANUFACTURERS (ROPE) — Primary beneficiaries. Gilead (tenofovir/emtricitabine) patent protection until 2025; global monopoly pricing ($1,500-2,000 USD annually). Experiences the constraint as coordination: regulatory approval, supply chain, market segmentation. Full arbitrage: can exit to other therapeutic markets. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Effective negative extraction (subsidy); institutional profit maximization is the constraint's primary beneficiary.
constraint_indexing:constraint_classification(hiv_prep_prevention_2026, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PREVENTION ADVOCACY ORGANIZATIONS (TANGLED ROPE) — Organized agents with generational horizon. See PrEP as partial solution embedded in asymmetric global health architecture. Benefit from PrEP as evidence of progress (fundraising, mandate expansion) but constrained by structural inequities they cannot unilaterally solve (drug pricing, criminalization, healthcare access). Enforcement: advocacy requires engaging with pharma pricing, government policy, and donor funding constraints. d≈0.52, f(d)≈0.68, σ=1.1 → χ≈0.39. Moderate extraction balanced by real prevention outcomes; sunset depends on generic availability and political will for equitable access.
constraint_indexing:constraint_classification(hiv_prep_prevention_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: GENERIC PRODUCERS AND PATENT POOL (SCAFFOLD) — Cipla, Mylan, others licensed to produce generic tenofovir at $50-100 annually after Gilead patent expiry (2025+). Represents temporary coordination scaffolding: patent pool allows licensed production at cost-compatible pricing while maintaining revenue for brand manufacturers. Active enforcement: patent pool agreements, WHO-prequalified suppliers, TRIPS flexibilities. Sunset clause: inherent to patent expiry and license terms. d≈0.35, f(d)≈0.35, σ=1.1 → χ≈0.22. Theater ≤0.70: coordination function is genuine (enabling access); enforcement is temporary and declining.
constraint_indexing:constraint_classification(hiv_prep_prevention_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: HIGH-INCOME NATION GOVERNMENTS (TANGLED ROPE) — Powerful institutional actors with generational view and arbitrage exit. See PrEP as prevention infrastructure investment (lower treatment costs, prevention messaging) but maintain global IP regime that enforces high pricing in low-income regions. Dual role: coordinate domestic prevention (benefit) while extracting rents through patent enforcement (cost to others). d≈0.28, f(d)≈0.20, σ=1.1 → χ≈0.11. Low direct extraction but structural leverage over patent/trade regime creates asymmetry.
constraint_indexing:constraint_classification(hiv_prep_prevention_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: CLINICAL GATEKEEPING (PITON) — Medical risk-stratification criteria (CD4 count, viral load, behavioral risk scores) persist as if they predict access barriers; in practice they mostly filter who gets preventive care. Theater_ratio=0.62 (assessment ritual maintains legitimacy of access denial). Institutional inertia: protocols designed for treatment (identifying who needs it most) repurposed for prevention (preventing preventive overuse) but actually preserving scarcity logic. d≈0.50, f(d)≈0.65, σ=0.9 → χ≈0.36. Performative gatekeeping with real extractive consequence.
constraint_indexing:constraint_classification(hiv_prep_prevention_2026, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 10: ANALYTICAL OBSERVER (MOUNTAIN) — From civilizational/universal view, PrEP instantiates an intrinsic structural tension: biomedical interventions require individual agency (adherence, access, decision-making) while HIV transmission is fundamentally a structural problem (inequality, criminalization, partner dynamics). This paradox is immutable: no pill resolves social determinants. ε should be ≤0.25 if this were truly a natural law. However, the structural data (ε=0.52, suppression=0.68, extractiveness at beneficiary arbitrage rates) reveals this is a false summit: the appearance of a natural limit is actually how institutional arrangements (pharma pricing, access inequality, criminalization) are naturalized as inevitable.
constraint_indexing:constraint_classification(hiv_prep_prevention_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hiv_prep_prevention_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hiv_prep_prevention_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hiv_prep_prevention_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hiv_prep_prevention_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hiv_prep_prevention_2026, TR),
    TR >= 0.70.

:- end_tests(hiv_prep_prevention_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts through multiple mechanisms: (1) Pharma monopoly pricing during patent protection (0.38→0.52 trajectory reflects expanding global demand meeting fixed high price); (2) Regulatory gatekeeping that controls access through clinical risk-scoring; (3) Healthcare infrastructure dependency that confines PrEP to high-income regions. The value reflects that this is not maximum extraction (ε=0.70+) because genuine prevention benefits accrue to those who access it, and fallback options exist (condoms, post-exposure prophylaxis, treatment-as-prevention). Suppression (0.68): Moderate-high and structural. Multiple reinforcing barriers: Legal (criminalization of MSM in 60+ countries prevents safe healthcare access); Economic (cost-of-living in most low-income regions makes $50-100 annual cost prohibitive; $1,500+ in-country cost is impossible); Infrastructural (lack of clinics, monitoring capacity, refrigeration); Social (stigma, identity concealment). These are not incidental barriers but structural design features of the global health architecture. Theater ratio (0.58): Moderate. Clinical risk-stratification protocols (CD4 count, viral load assessment, behavioral risk scoring) perform as if they optimize access but primarily function to limit prescribing and preserve scarcity. The ratio increases over the interval (0.45→0.58) as protocols become more elaborate while access remains stalled, indicating increasing performative content relative to functional prevention outcomes.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal across all perspectives. Low-income MSM see pure extraction (Snare): prevention is theoretically available but structurally inaccessible, and they bear full costs of non-access. Pharmaceutical manufacturers see coordination success (Rope): they solved a problem (HIV prevention), the market validates their solution, and they experience no extraction because they have exit options (other markets). Advocacy organizations see mixed with sunset potential (Tangled Rope): genuine coordination function (PrEP works) embedded in asymmetric extraction (pricing, inequality), with a real pathway to equity post-2025 if political will exists. Generic manufacturers and patent pool see temporary scaffolding (Scaffold): they are building alternative pathways with an inherent sunset (patent expiry, license terms). High-income systems see coordination (Rope): they can deliver PrEP through existing infrastructure, benefiting domestically without bearing costs of global inequality. Clinical gatekeeping sees itself as pure coordination (risk assessment) but functions as performative extraction (Piton): the ritual persists through institutional inertia, divorced from genuine optimization of access. The analytical observer risks the false summit (Mountain): naturalizing biomedical innovation's uneven distribution as inherent to how new drugs disseminate, when the inequality is actually a political choice (patent enforcement, trade rules, healthcare financing) that could be unmade.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharmaceutical manufacturers: Beneficiary + arbitrage exit → d≈0.08, f(d)≈-0.10. Negative effective extraction (institutional subsidy). They set the constraint's initial conditions and benefit from its existence. Low-income MSM: Victim + trapped exit → d≈0.96, f(d)≈1.42. Maximum extraction: no exit from infection risk without access, no ability to access, criminalization prevents seeking help. Incarcerated populations: Victim + trapped exit → d≈0.94, f(d)≈1.40. Near-maximum: confinement is total, state denies prevention tools, no autonomy. Sex workers (high-income regions): Victim + constrained exit → d≈0.68, f(d)≈1.05. Moderate extraction: can theoretically access but face criminalization and labor instability that suppress uptake; constrained but not trapped. Advocacy organizations: Mixed + constrained exit → d≈0.52, f(d)≈0.68. They benefit from evidence of progress but are constrained by structural barriers they cannot unilaterally solve; neither pure beneficiary nor pure victim. High-income systems: Beneficiary + arbitrage → d≈0.12, f(d)≈0.05. They coordinate prevention domestically and can exit to other modalities; net beneficiary. Generic manufacturers: Intermediate + constrained → d≈0.35, f(d)≈0.35. Constrained by patent pool license terms and generic market dynamics; neither fully beneficiary nor victim; scaffolding role.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED. The constraint is genuinely Tangled Rope, not misclassified as pure coordination or pure extraction. The mandatrophy test: Does the constraint possess BOTH a coordination function AND asymmetric extraction? YES. Coordination: PrEP is a technologically functional prevention mechanism that requires coordinated healthcare delivery, informed consent, adherence support, and population-level behavior change — these are genuine coordination goods. Asymmetric extraction: Pricing inequality, access barriers, and criminalization-driven suppression create a structure where prevention benefits concentrate in high-income regions while transmission burden remains in low-income regions. Neither function is subordinate to the other. The coordination function (prevention works) does not eliminate the extraction (inequality remains). The extraction mechanism (patent monopoly) does not eliminate the coordination (genuine prevention infrastructure). The constraint is active enforcement of both: pharma maintains pricing, governments maintain criminalization and healthcare gaps, yet simultaneously prevention advocates expand access through generic pathways. The Tangled Rope classification is not a compromise between Rope (if we ignored inequality) and Snare (if we ignored prevention efficacy). It is the accurate structural characterization: the constraint REQUIRES both coordination AND extraction as interlocked components. Without the coordination function, the snare would be obvious (a drug you cannot get). Without the extraction mechanism, it would be pure coordination (a prevention tool equitably distributed). The coexistence of both, reinforced by patent, pricing, and criminalization structures, is the Tangled Rope signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adherence_vs_access_causality,
    'How much of observed PrEP non-effectiveness in low-income populations is adherence failure versus structural access failure?',
    'Randomized controlled trials with direct provision (free PrEP, clinic accompaniment); comparison of adherence rates when access barriers removed; pharmacokinetic tracking of drug levels in populations with vs without cost barriers',
    'If primarily adherence: PrEP is coordination mechanism (Rope), problem is behavioral. If primarily access: PrEP is snare, the coordination is illusory, and suppression (0.68) understates the real barrier.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adherence_vs_access_causality, empirical, 'Root cause of PrEP effectiveness gap: adherence vs structural access').

omega_variable(
    criminalization_causal_role,
    'Does decriminalization of homosexuality and sex work directly increase PrEP uptake independent of other factors (income, healthcare access)?',
    'Natural experiment analysis: countries that decriminalized vs those that did not; correlation with PrEP uptake controlling for GDP and healthcare infrastructure; qualitative interviews with MSM in criminalized vs decriminalized contexts about disclosure decisions',
    'If causal: suppression (0.68) is structural-legal, not purely medical/economic. Snare classification becomes inevitable in criminalized contexts regardless of drug availability. If not causal: suppression is driven by other factors and policy lever is weaker.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(criminalization_causal_role, empirical, 'Causal link between criminalization and PrEP uptake barriers').

omega_variable(
    generic_access_ceiling,
    'Will generic PrEP availability post-2025 create universal access in low-income regions or will supply-chain and regulatory barriers maintain inequality?',
    'Post-2025 tracking: number of people on generic PrEP in sub-Saharan Africa by 2030; supply-chain audits; regulatory approval timelines in target countries; WHO-prequalification rates vs demand',
    'If universal access achieved: scaffold perspective confirmed, extraction mechanism ends ~2030. If barriers persist: structural inequality is maintained through regulatory/supply-chain mechanisms rather than patent monopoly, classification shifts from Snare-due-to-pricing to Snare-due-to-infrastructure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generic_access_ceiling, empirical, 'Whether patent expiry enables true access equity or maintains barriers through other mechanisms').

omega_variable(
    behavioral_risk_amplification,
    'Does PrEP availability cause measurable increase in condomless sex or sexual partner numbers (risk compensation) in ways that change net HIV transmission reduction?',
    'Meta-analysis of behavioral outcomes in PrEP cohorts vs controls; long-term follow-up of sexual practices and STI transmission; comparison of population-level HIV incidence with PrEP rollout vs baseline',
    'If significant risk compensation occurs: net prevention benefit is lower than per-adherence claims; extraction mechanism becomes more subtle (individual-level protection but population-level risk shift). If minimal: individual biomedical model holds, classification is more clearly Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(behavioral_risk_amplification, empirical, 'Risk compensation and net population-level HIV transmission reduction').

omega_variable(
    patent_pool_efficacy,
    'Do patent pool licensing arrangements actually reduce prices to cost-compatible levels or maintain near-monopoly pricing through licensing restrictions?',
    'Price tracking of generic tenofovir/emtricitabine 2025-2030 in patent-pool vs non-pool regions; audit of licensing terms for restrictive clauses; comparison with other generic HIV drugs (ART regimens) in same regions',
    'If pool maintains high prices: patent expiry is formal, not structural, and extraction mechanism continues. Scaffold perspective is false summit. If prices drop to $30-50: scaffold is real, extraction mechanism ends.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(patent_pool_efficacy, empirical, 'Actual pricing outcomes of patent pool post-2025 patent expiry').

omega_variable(
    structural_versus_behavioral_prevention,
    'How much of HIV transmission is attributable to individual behavioral choice versus structural factors (economic coercion of unsafe sex, partner violence, incarceration)?',
    'Population-level analysis of HIV transmission drivers; comparative effectiveness of structural interventions (economic support, legal reform, housing) vs behavioral interventions (PrEP, condoms, testing)',
    'If structural factors dominant: PrEP as individual-choice intervention is fundamentally insufficient; classification shifts toward Snare-of-structural-inequality. If behavioral dominant: PrEP-as-Rope is more appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_versus_behavioral_prevention, conceptual, 'Degree to which HIV transmission is behavior-vs-structure determined').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hiv_prep_prevention_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, hiv_prep_prevention_2026, theater_ratio, 0, 0.45).
narrative_ontology:measurement(prep_tr_t5, hiv_prep_prevention_2026, theater_ratio, 5, 0.52).
narrative_ontology:measurement(prep_tr_t10, hiv_prep_prevention_2026, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, hiv_prep_prevention_2026, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(prep_be_t5, hiv_prep_prevention_2026, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(prep_be_t10, hiv_prep_prevention_2026, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hiv_prep_prevention_2026, resource_allocation).
narrative_ontology:affects_constraint(hiv_prep_prevention_2026, hiv_treatment_cascade_global).
narrative_ontology:affects_constraint(hiv_prep_prevention_2026, patent_enforcement_pharmaceutical_access).
narrative_ontology:affects_constraint(hiv_prep_prevention_2026, criminalization_healthcare_access).

% DUAL FORMULATION NOTE:
% PrEP-as-prevention is part of a broader HIV constraint family: (1) treatment_cascade_global (ε=0.35, Tangled Rope) — access to antiretroviral treatment itself in low-income regions; (2) prep_prevention_2026 (ε=0.52, Tangled Rope) — access to prevention in advance of infection; (3) patent_enforcement_pharmaceutical (ε=0.58, Snare) — the structural IP regime that enforces pricing inequality across health domains. PrEP's extractiveness (0.52) is higher than treatment_cascade (0.35) because prevention targets healthy individuals facing behavioral barriers, while treatment targets symptomatic individuals with no choice; PrEP's suppression is higher because additional legal and social barriers (criminalization of sexuality) compound economic barriers. The family decomposes because the ε values differ significantly (0.35 vs 0.52 vs 0.58) — they have different failure modes (treatment access vs prevention access vs IP regime) and different advocacy communities, but are linked through the underlying patent/pricing constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hiv_prep_prevention_2026, organized, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
