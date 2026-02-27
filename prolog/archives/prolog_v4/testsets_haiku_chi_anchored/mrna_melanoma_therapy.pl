% ============================================================================
% CONSTRAINT STORY: mrna_melanoma_therapy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mrna_melanoma_therapy, []).

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
 *   constraint_id: mrna_melanoma_therapy
 *   human_readable: Personalized mRNA Cancer Vaccine Protocol (mRNA-4157/V940)
 *   domain: healthcare/biotechnology/pharmaceutical_economics
 *
 * SUMMARY:
 *   The mRNA-4157/V940 personalized melanoma vaccine represents a frontier
 *   biotechnology where genuine therapeutic coordination (patient data →
 *   personalized neoantigen prediction → clinical benefit) coexists with
 *   structural extraction mechanisms (monopolistic manufacturing, patent
 *   protection, cost-sharing concentrated on vulnerable patients, regulatory
 *   theater). The constraint combines real innovation coordination with
 *   strong suppression of alternatives. The therapy shows 44% recurrence-free
 *   survival improvement over standard of care, making it a life-extending
 *   innovation. Yet the personalization architecture creates dual
 *   gatekeeping: clinical (which patients qualify) and economic (who can
 *   afford access). From the perspective of an uninsured high-risk melanoma
 *   patient, this is pure extraction with no exit. From the pharmaceutical
 *   manufacturer's perspective, it is coordination-enabled innovation with
 *   legitimate first-mover advantage. From the healthcare regulator's
 *   perspective, it is a mixed constraint requiring both clinical innovation
 *   and affordability guardrails. The expanding theater ratio (0.35→0.58)
 *   reflects increasing performative content: initial regulatory scrutiny was
 *   justified (de novo pathway approval for novel mechanism); persistent
 *   regulatory complexity now protects market position. The extractiveness
 *   trajectory (0.38→0.52) reflects layered rent-seeking as manufacturing
 *   scales and market concentration solidifies.
 *
 * KEY AGENTS:
 *   - Pharmaceutical Manufacturer (Institutional/Arbitrage): Primary beneficiary. Controls patent, manufacturing, IP, and first-mover market position. Benefits from personalization architecture that enables price discrimination and access control.
 *   - High-Risk Melanoma Patients, Low-Income (Powerless/Trapped): Primary victims. Treatment is survival-critical, price is non-negotiable, no alternatives. Face maximum extraction with zero exit options.
 *   - Insured Patients with Cost-Sharing (Moderate/Constrained): Secondary victims. Insurance covers some cost, but high deductibles and formulary restrictions concentrate access barriers on sickest patients during treatment initiation.
 *   - National Health Systems / Insurance Regulators (Organized/Constrained): Organized actors with enforcement power but constrained by budget pressure and inability to negotiate price (patent monopoly). See both coordination benefit (outcome improvement) and extraction cost (budget pressure, equity gaps).
 *   - Competing Technology Developers (Organized/Constrained): Off-patent vaccine platforms, decentralized manufacturing pilots, open-source neoantigen prediction. See mRNA-4157 dominance as temporary; building alternative pathways with lower cost and decentralized delivery.
 *   - Regulatory Agencies (Institutional/Constrained): FDA/EMA maintain accelerated pathways and prioritized review status. Initial justification (de novo innovation) has degraded into theater protecting incumbent from competition-based re-review demands.
 *   - Analytical Observer (Analytical/Analytical): Risks naturalizing the personalization bottleneck as inherent to biology rather than as a contingent economic structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mrna_melanoma_therapy, 0.52).
domain_priors:suppression_score(mrna_melanoma_therapy, 0.68).
domain_priors:theater_ratio(mrna_melanoma_therapy, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mrna_melanoma_therapy, extractiveness, 0.52).
narrative_ontology:constraint_metric(mrna_melanoma_therapy, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(mrna_melanoma_therapy, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mrna_melanoma_therapy, tangled_rope).
narrative_ontology:human_readable(mrna_melanoma_therapy, "Personalized mRNA Cancer Vaccine Protocol (mRNA-4157/V940)").
narrative_ontology:topic_domain(mrna_melanoma_therapy, "healthcare/biotechnology/pharmaceutical_economics").

domain_priors:requires_active_enforcement(mrna_melanoma_therapy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mrna_melanoma_therapy, pharmaceutical_manufacturer).
narrative_ontology:constraint_beneficiary(mrna_melanoma_therapy, treatment_infrastructure).
narrative_ontology:constraint_victim(mrna_melanoma_therapy, high_risk_melanoma_patients_low_income).
narrative_ontology:constraint_victim(mrna_melanoma_therapy, global_healthcare_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNINSURED HIGH-RISK MELANOMA PATIENT (SNARE) — Cannot exit: treatment is survival-critical, manufacturing and delivery infrastructure is monopolistic, pricing power is absolute. Patient bears full extraction cost with no alternatives. d≈0.96, f(d)≈1.41, σ=1.2 → χ≈0.88.
constraint_indexing:constraint_classification(mrna_melanoma_therapy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INSURED PATIENT WITH COST-SHARING (TANGLED ROPE) — Partially mobile through insurance but trapped by deductible structure and formulary restrictions. Coordination function: personalization requires patient tumor sequencing and clinical data sharing (benefits from innovation). Extraction function: cost-sharing and access restrictions concentrated on sickest patients. d≈0.72, f(d)≈1.13, σ=1.0 → χ≈0.59.
constraint_indexing:constraint_classification(mrna_melanoma_therapy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PHARMACEUTICAL MANUFACTURER (ROPE) — Experiences constraint as coordination: manufacturing personalization requires patient-specific neoantigen prediction, supply chain integration, and regulatory compliance. Benefits from network effects as adoption scales. Patent protection provides arbitrage exit (can move to new markets/indications). d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary through first-mover advantage and ecosystem control.
constraint_indexing:constraint_classification(mrna_melanoma_therapy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: NATIONAL HEALTH SYSTEM / INSURANCE REGULATOR (TANGLED ROPE) — Organized actors see both coordination benefit (clinical outcomes improve, population health strengthens) and extraction cost (budget pressures, affordability constraints, equity gaps). Constrained exit: cannot refuse coverage (political/ethical mandate) but cannot negotiate price (patent monopoly). Requires active enforcement of cost-sharing rules and access restrictions. d≈0.65, f(d)≈0.98, σ=1.0 → χ≈0.51.
constraint_indexing:constraint_classification(mrna_melanoma_therapy, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: COMPETING TECHNOLOGY COALITION (SCAFFOLD) — Organized developers of alternative personalized cancer vaccines (off-patent, modular, open-access formats) see the mRNA-4157 monopoly as temporary. Personalized medicine will eventually decentralize: liquid biopsy costs falling, neoantigen prediction open-sourcing, manufacturing becoming modular. d≈0.50, f(d)≈0.65, σ=1.1 → χ≈0.38. Sunset estimated: 8-15 years as competing platforms mature and patent expiration approaches.
constraint_indexing:constraint_classification(mrna_melanoma_therapy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY APPROVAL THEATER (PITON) — FDA accelerated approval and breakthrough designation were justified at initial approval (2023). Five years later, the regulatory framework persists unchanged despite the technology being de-risked and in routine deployment. Continued prioritized review, predictive biomarker mandates, and personalization requirements now function as rent-protection theater rather than genuine innovation gatekeeping. theater_ratio=0.58 reflects partial degradation — some genuine function remains (safety monitoring) but much is inertial. d≈0.10, f(d)≈-0.08, σ=1.2 → χ≈-0.05.
constraint_indexing:constraint_classification(mrna_melanoma_therapy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, risk the false argument that: 'Personalized medicine inherently requires centralized manufacturing, sequencing infrastructure, and long approval timelines — these are natural laws of biology and regulation, not contingent economic arrangements.' However, the base properties (ε=0.52, suppression=0.68, theater=0.58) contradict mountain classification. The engine's false summit detector reveals that the extraction and suppression are institutional, not natural. This perspective naturalizes contingent barriers (regulatory complexity, manufacturing centralization, IP protection) as inherent to the technology itself.
constraint_indexing:constraint_classification(mrna_melanoma_therapy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mrna_melanoma_therapy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mrna_melanoma_therapy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mrna_melanoma_therapy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mrna_melanoma_therapy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(mrna_melanoma_therapy, TR),
    TR >= 0.70.

:- end_tests(mrna_melanoma_therapy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The pharmaceutical manufacturer captures significant economic value during patent term (approximately 2027-2037 under US patent law, with potential extension). The extraction is not maximal because: (1) genuine therapeutic benefit (44% RFS improvement) justifies some premium; (2) insurance coverage reduces out-of-pocket extraction for some patients; (3) competing platforms are already in development. However, the extraction is substantial due to monopolistic pricing power, cost-sharing mechanisms that concentrate barriers on vulnerable patients, and manufacturing control that enables price discrimination across geographic markets. Suppression (0.68): High. Multiple mechanisms suppress alternatives: (1) regulatory framework built around mRNA-4157 personalization as the gold standard creates approval burden for competitors; (2) manufacturing complexity (tumor sequencing → neoantigen design → personalized production) requires centralized infrastructure and creates capex barriers; (3) patent protection (composition of matter, method of manufacture, neoantigen prediction algorithms) blocks direct competition until 2037; (4) publication bias and clinical trial design favor incumbent platform; (5) healthcare systems have invested in infrastructure compatible with centralized manufacturing, increasing switching cost. Theater ratio (0.58): Moderate-high, increasing trend. The 0.35→0.58 trajectory reflects regulatory theater accumulation. Initial FDA approval (2023) required genuine evidence generation. By 2026, the regulatory framework persists but much of the review complexity serves rent-protection rather than genuine safety/efficacy gatekeeping. Persistent 'personalization confidence thresholds,' manufacturing audits, and neoantigen prediction accuracy mandates have become inertial — they no longer drive innovation but rather protect market position by raising competitor approval burden.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence across the rich-poor divide and between coordinating institutions. The uninsured patient sees Snare (pure extraction, no exit, maximum suppression). The insured patient sees Tangled Rope (partial mobility through insurance, genuine therapeutic coordination benefit, but still significant cost concentration). The manufacturer sees Rope (coordination-enabled innovation with legitimate first-mover advantage). The health regulator sees Tangled Rope (both innovation benefit and budget pressure, constrained ability to enforce affordability). Competing developers see Scaffold (monopoly is temporary, alternative pathways coming online with sunset 8-15 years). The regulatory system sees Piton (initial genuine function has degraded into theater). The analytical observer risks Mountain (naturalizes personalization complexity as inherent to biology). The perspectival gap widens as the constraint moves from innovation (time 0: more Rope) to exploitation (time 6: more Snare). This is a diagnostic exemplar of how a constraint can transition from coordination-heavy to extraction-heavy over a single observational interval.
 *
 * DIRECTIONALITY LOGIC:
 *   Uninsured melanoma patient: Victim + trapped → d≈0.96, f(d)≈1.41. Near-maximal extraction — no insurance, no arbitrage options, no mobility, treatment is survival-critical. Insured cost-sharing patient: Mixed (both beneficiary of innovation and victim of cost-sharing) + constrained → d≈0.72, f(d)≈1.13. Asymmetric extraction through cost concentration during vulnerable period (treatment initiation). Pharmaceutical manufacturer: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary through patent control and market position. Can exit via geographic arbitrage, product line extension, patent litigation. National health system: Mixed (both coordinator and victim) + constrained → d≈0.65, f(d)≈0.98. Constrained by ethical mandate to cover (cannot exit) but cannot negotiate price (patent monopoly prevents arbitrage). Competing developers: Ambivalent (benefit from technology diffusion, harmed by incumbent position) + constrained → d≈0.50, f(d)≈0.65. See sunset pathway, lower effective extraction because they have long-term agency. Regulatory system: Beneficiary (maintains authority, preserves innovation pipeline) + constrained → d≈0.10, f(d)≈-0.08. Piton classification comes from theater gate, not from high chi.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint avoids the trap of 'is this coordination or extraction?' by recognizing it is both. The genuine coordination function is real: personalized neoantigen prediction, patient-specific manufacturing, and clinical outcome improvement represent authentic innovation that solves a hard problem. The extraction is also real: monopolistic pricing, cost-sharing concentrated on vulnerable patients, manufacturing centralization, and regulatory theater create significant asymmetric burden. The tangled_rope classification resolves the mandatrophy by declaring both mechanisms simultaneously: active enforcement of manufacturing standards and regulatory pathways (coordination), asymmetric benefits to manufacturer and harms to low-income patients (extraction), and both beneficiaries (manufacturer, treatment infrastructure) and victims (uninsured patients, healthcare systems). The theater ratio of 0.58 reflects that roughly 40% of the regulatory complexity serves genuine coordination (safety/efficacy assurance) and 58% serves inertial rent-protection. The trajectory from 0.35 to 0.58 indicates growing theater: as the technology de-risks, the proportion of regulatory effort devoted to genuine safety questions declines, while the proportion devoted to market-protection grows. This divergence is a leading indicator that the constraint is transitioning from innovation-enabling (Rope) toward exploitation-enabling (Snare). Without mandatrophy resolution, the system would oscillate between calling it 'groundbreaking innovation' (Rope framing) and 'price gouging on desperate patients' (Snare framing). The tangled_rope classification with suppression=0.68 and extractiveness=0.52 captures both truths: the innovation is real AND the extraction is real, and they coexist in the same institutional structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    neoantigen_prediction_accuracy_threshold,
    'At what accuracy threshold does neoantigen prediction become a genuine coordination bottleneck vs. an artificial gating mechanism for access control?',
    'Comparative analysis: prediction accuracy rates in published cohorts vs. clinical outcome correlation; measurement of access denial rates due to ''insufficient confidence'' scores vs. actual clinical failures',
    'If threshold < 60% accuracy enforced: pure extraction mechanism disguised as safety. If threshold > 95% accuracy required: artificial gate blocking eligible patients. If 75-85% empirically optimal: genuine coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(neoantigen_prediction_accuracy_threshold, empirical, 'Whether neoantigen accuracy requirements serve safety or access control').

omega_variable(
    manufacturing_scalability_constraint,
    'Is the requirement for centralized personalized manufacturing (tumor sequencing → neoantigen design → personalized production) technologically necessary or economically enforced by the incumbent manufacturer?',
    'Decentralized pilot programs (regional labs, hospital-based production, contract manufacturing validation); comparison of quality metrics, turnaround time, and cost between centralized vs. decentralized production',
    'If decentralized production fails quality gates: centralization is technical necessity (coordination). If decentralized succeeds: centralization is economic rent-protection (extraction). This determines whether the supply-side constraint is Mountain or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manufacturing_scalability_constraint, empirical, 'Whether manufacturing must be centralized or is economically enforced').

omega_variable(
    generic_personalized_vaccine_viability,
    'Can off-patent, open-source personalized cancer vaccine platforms achieve clinical efficacy comparable to mRNA-4157 within 10-15 years, enabling the scaffold sunset?',
    'Tracking clinical trial data for competing platforms (BioNTech/Gritstone, Moderna alternatives, academic initiatives); measurement of cost trajectories and manufacturing complexity as IP barriers expire',
    'If competitors achieve >70% of mRNA-4157 efficacy and <50% of cost by 2033-2035: scaffold sunset is structural. If competitors cannot close the gap: monopoly extraction will persist beyond patent expiration due to network effects and regulatory lock-in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generic_personalized_vaccine_viability, empirical, 'Whether alternative personalized cancer vaccines can achieve clinical parity').

omega_variable(
    equity_mandate_enforcement_gap,
    'Will national healthcare systems enforce equity-based pricing or access mandates, or will political economy of pharmaceutical lobbying prevent regulatory action?',
    'Longitudinal tracking of coverage policies, pricing negotiations, and access equity metrics across OECD countries; measurement of correlation between pro-regulation policy announcements and actual price-setting outcomes',
    'If equity mandates are enforced: tangled_rope classification holds, suppression declines over time. If mandates remain rhetorical: snare classification dominates, suppression increases as cost-sharing concentrates on vulnerable patients.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(equity_mandate_enforcement_gap, preference, 'Whether political economy will enforce equity-based pricing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mrna_melanoma_therapy, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mrna_tr_t0, mrna_melanoma_therapy, theater_ratio, 0, 0.35).
narrative_ontology:measurement(mrna_tr_t3, mrna_melanoma_therapy, theater_ratio, 3, 0.48).
narrative_ontology:measurement(mrna_tr_t6, mrna_melanoma_therapy, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(mrna_be_t0, mrna_melanoma_therapy, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(mrna_be_t3, mrna_melanoma_therapy, base_extractiveness, 3, 0.46).
narrative_ontology:measurement(mrna_be_t6, mrna_melanoma_therapy, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mrna_melanoma_therapy, resource_allocation).
narrative_ontology:affects_constraint(mrna_melanoma_therapy, personalized_medicine_manufacturing_bottleneck).
narrative_ontology:affects_constraint(mrna_melanoma_therapy, neoantigen_prediction_gatekeeping).
narrative_ontology:affects_constraint(mrna_melanoma_therapy, pharmaceutical_patent_duration_dynamics).

% DUAL FORMULATION NOTE:
% The mRNA-4157 therapy represents three structurally distinct constraints: (1) manufacturing personalization as a resource allocation coordination problem (low ε, genuine bottleneck), (2) neoantigen prediction as a gatekeeping mechanism (ε~0.35, mixed coordination/extraction), and (3) pharmaceutical patent monopoly as rent extraction (ε~0.68, pure extraction). This story focuses on the integrated constraint as experienced by patients and regulators (ε=0.52, Tangled Rope). The decomposition into three stories would show the manufacturing bottleneck as Rope, the prediction gatekeeping as Tangled Rope, and the patent monopoly as Snare. The unified story (mrna_melanoma_therapy) links all three as a family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mrna_melanoma_therapy, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
