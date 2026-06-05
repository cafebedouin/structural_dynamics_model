% ============================================================================
% CONSTRAINT STORY: somatic_gene_therapy_approval
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_somatic_gene_therapy_approval, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: somatic_gene_therapy_approval
 *   human_readable: Somatic Gene Therapy Regulatory Approval Bottleneck
 *   domain: biomedical_regulation/drug_approval
 *
 * SUMMARY:
 *   Somatic gene therapy regulatory approval constitutes a structural
 *   bottleneck that exhibits genuine coordination benefits (safety
 *   verification) alongside asymmetric extraction (approval delay that
 *   extends patient suffering, concentrates market power, and suppresses
 *   startup innovation). The constraint's extractiveness has grown from 0.38
 *   to 0.58 over 15 years as approval standards have tightened, trial
 *   requirements have expanded, and manufacturing requirements have become
 *   more stringent. Theater ratio has risen from 0.52 to 0.71, indicating
 *   that traditional randomized controlled trial paradigms — designed for
 *   large-population studies — are increasingly performative when applied to
 *   rare diseases with N<1000 globally. This constraint demonstrates a core
 *   mandatrophy challenge: the same regulatory apparatus that prevents
 *   dangerous premature approvals also extracts years of life from patients
 *   with fatal diseases. The constraint is not a pure snare (extraction with
 *   no coordination benefit) — genuine safety verification occurs. Nor is it
 *   pure rope (coordination with no extraction) — approval timelines are
 *   substantially padded with theater and institutional delay beyond
 *   technical necessity. The constraint is tangled rope: authentic
 *   coordination objectives (safety) are entangled with institutional
 *   extraction (market protection, organizational legitimacy, career
 *   incentives around trial expertise). A patient advocacy coalition is
 *   building parallel pathways (Right-to-Try, expanded access, international
 *   harmonization) that offer a sunset clause: real-world evidence and
 *   adaptive licensing could replace traditional timelines within 10-15 years
 *   if policy reform succeeds. The traditional clinical trial paradigm (piton
 *   perspective) persists through inertia despite acknowledged inadequacy for
 *   rare diseases, maintained by regulatory agency staff incentives, pharma
 *   industry adaptation, and absent countervailing pressure until recently.
 *
 * KEY AGENTS:
 *   - Rare Disease Patient: Primary victim (powerless/trapped) — faces life-limiting condition with no exit from disease; regulatory bottleneck extends suffering by 5-15 years beyond technical readiness
 *   - Gene Therapy Startup: Secondary victim (moderate/constrained) — expends 10+ years and $500M+ runway while facing incumbent firm scale advantage; some exit optionality through acquisition or international markets
 *   - Incumbent Pharmaceutical Firm: Primary beneficiary (institutional/arbitrage) — approval delay protects existing therapeutic market; high regulatory barriers enable premium pricing; arbitrage options through portfolio diversification
 *   - Regulatory Agency (FDA/EMA): Secondary beneficiary with mixed motive (institutional/constrained) — coordinates genuine safety verification but also extracts legitimacy from visible gatekeeping; constrained by liability exposure
 *   - Patient Advocacy Coalition: Organized agent (organized/mobile) — building alternative approval pathways (Right-to-Try, expanded access, real-world evidence); sees regulatory bottleneck as temporary with clear sunset trajectory
 *   - Traditional Clinical Trial Paradigm: Institutional inertia (institutional/arbitrage) — randomized controlled trials maintained as gold standard despite empirical infeasibility for rare diseases; persists through career incentives and regulatory guidance alignment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(somatic_gene_therapy_approval, 0.58).
domain_priors:suppression_score(somatic_gene_therapy_approval, 0.65).
domain_priors:theater_ratio(somatic_gene_therapy_approval, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(somatic_gene_therapy_approval, extractiveness, 0.58).
narrative_ontology:constraint_metric(somatic_gene_therapy_approval, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(somatic_gene_therapy_approval, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(somatic_gene_therapy_approval, tangled_rope).
narrative_ontology:human_readable(somatic_gene_therapy_approval, "Somatic Gene Therapy Regulatory Approval Bottleneck").
narrative_ontology:topic_domain(somatic_gene_therapy_approval, "biomedical_regulation/drug_approval").

domain_priors:requires_active_enforcement(somatic_gene_therapy_approval).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(somatic_gene_therapy_approval, incumbent_pharma_firms).
narrative_ontology:constraint_beneficiary(somatic_gene_therapy_approval, regulatory_agencies).
narrative_ontology:constraint_victim(somatic_gene_therapy_approval, rare_disease_patients).
narrative_ontology:constraint_victim(somatic_gene_therapy_approval, innovation_capacity).
narrative_ontology:constraint_victim(somatic_gene_therapy_approval, gene_therapy_startups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RARE DISEASE PATIENT (SNARE) — Faces life-limiting condition with no approved therapeutic option. Cannot exit the disease. Regulatory bottleneck extends suffering by 5-15 years beyond technical readiness. Zero alternatives; zero negotiating power. Maximum extraction: the regulatory apparatus extracts years of life.
constraint_indexing:constraint_classification(somatic_gene_therapy_approval, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GENE THERAPY STARTUP (TANGLED ROPE) — Experiences both coordination and extraction. Regulatory process ensures safety verification (genuine coordination benefit). But approval timeline, capital requirements, and incumbent firm gatekeeping create asymmetric extraction: startups expend 10+ years and $500M+ in runway while incumbent firms face 5-7 year timelines with lower per-unit costs due to scale. Constrained by funding and survival pressure, but some exit optionality through acquisition or international markets.
constraint_indexing:constraint_classification(somatic_gene_therapy_approval, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT PHARMA FIRM (ROPE) — Net beneficiary of regulatory bottleneck. Approval delay protects existing therapeutic market. High regulatory barriers create coordination benefit: safety-verified pipeline commands premium pricing and market protection. Effective arbitrage options through portfolio diversification, international markets, and regulatory capture. Experiences the constraint as legitimate coordination of pharmaceutical risk management.
constraint_indexing:constraint_classification(somatic_gene_therapy_approval, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AGENCY (TANGLED ROPE) — Genuinely coordinates patient safety through rigorous preclinical and clinical review. But also extracts organizational legitimacy from visible gatekeeping: more stringent requirements signal competence and reduce accountability for post-approval harms (publication bias toward null safety findings). Constrained by political pressure and liability exposure. Experiences tension between safety mandate (coordination) and institutional self-preservation (extraction).
constraint_indexing:constraint_classification(somatic_gene_therapy_approval, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PATIENT ADVOCACY COALITION (SCAFFOLD) — Organized agents (patient groups, compassionate-use networks) see regulatory bottleneck as a temporary problem with a sunset: Right-to-Try laws, expanded access programs, and international regulatory harmonization are building parallel approval pathways. Coalition has agency to push for regulatory reform and can point to technical precedent (accelerated approval, breakthrough designation). Exit path is clear: distributed verification through real-world evidence and adaptive licensing. Sunset clause: 10-15 years as real-world data replaces randomized trials.
constraint_indexing:constraint_classification(somatic_gene_therapy_approval, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: TRADITIONAL CLINICAL TRIAL PARADIGM (PITON) — Randomized controlled trial as gold standard for drug approval is substantially performative in rare disease contexts. For diseases with N<1000 globally, powered RCTs are empirically infeasible, yet the paradigm persists. Theater ratio reflects: expensive trial infrastructure maintained despite known inadequacy for rare disease; regulatory guidance written to RCT assumptions even when alternatives exist; agency staff trained in RCT interpretation. The paradigm persists through institutional inertia (career incentives aligned with RCT expertise) not because it works for somatic gene therapy. Alternatives (adaptive trials, real-world evidence, n-of-1 designs) exist but face adoption barriers.
constraint_indexing:constraint_classification(somatic_gene_therapy_approval, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational perspective, some regulatory caution is inherent to novel therapies: irreversible gene modifications require verification for long-term safety, and the gap between proof-of-concept and population safety is a structural feature of biomedical governance. This view naturalizes the bottleneck as an immutable constraint. But the structural data contradicts this: approval timelines vary 3-5x across jurisdictions for identical therapies (FDA vs EMA vs Japan), regulatory standards have changed repeatedly, and real-world evidence is technically sufficient for safety monitoring. The mountain classification is a false summit — it naturalizes a contingent institutional choice as inherent scientific constraint.
constraint_indexing:constraint_classification(somatic_gene_therapy_approval, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(somatic_gene_therapy_approval_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(somatic_gene_therapy_approval, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(somatic_gene_therapy_approval, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(somatic_gene_therapy_approval, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(somatic_gene_therapy_approval, TR),
    TR >= 0.70.

:- end_tests(somatic_gene_therapy_approval_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The regulatory bottleneck creates substantial extraction through approval delay: rare disease patients wait 5-10 years beyond proof-of-concept; startups expend 10+ years and >$500M while incumbent firms move faster due to scale; market protection accrues to existing therapeutics. But extractiveness is not at snare level (>0.66) because genuine safety verification occurs, trials do detect safety signals, and post-market harm from premature approvals would be severe. The extraction is coordination-entangled. Suppression (0.65): High. Multiple barriers prevent exit: patients cannot exit disease; startups cannot exit regulatory process without approval; alternative approval pathways (Right-to-Try, expanded access) exist but are constrained by regulatory discretion. Theater ratio (0.68): High. Traditional RCT paradigm applied to rare diseases (N<1000 globally) is substantially performative: expensive trial infrastructure maintained despite known inadequacy; regulatory guidance written to RCT assumptions even when alternatives exist; agency staff trained in RCT interpretation create path dependency. Rising trend (0.52→0.71) reflects increasing theater as trial requirements expand beyond technical necessity for rare disease contexts. The measurement trajectory shows extraction growing over time as theater increases—this is Goodhart drift: regulatory standards tighten to signal competence, creating padding independent of safety gain.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival disagreement. The rare disease patient sees pure snare (extraction, no escape, no benefit). The incumbent pharma firm sees rope (coordination that ensures safety-verified premium pricing). The regulatory agency sees rope from institutional perspective but acknowledges tangled rope from patient welfare perspective. The startup sees tangled rope (mixed coordination/extraction with scale disadvantage). The patient advocacy coalition sees scaffold (temporary problem with clear sunset). The traditional RCT paradigm perspective (piton) sees its own obsolescence yet persists through inertia. The civilizational analytical view risks naturalizing the bottleneck as inherent scientific constraint, but the structural data reveals this as institutional choice: approval timelines vary 3-5x across jurisdictions for identical therapies (FDA vs EMA vs Japan), proving contingency not inevitability. The perspectival gap arises from power asymmetry: beneficiaries experience coordination; victims experience extraction; observers see institutional theater. The gap is the constraint's structure, not noise.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality (d) derives from power level, exit options, and structural relationship. Powerless/trapped patients experience maximum d (~0.95): they bear full extraction with zero exit options. Moderate/constrained startups experience high d (~0.75): they face substantial costs (time, capital) with some arbitrage optionality through acquisition. Institutional/arbitrage incumbent firms experience low d (~0.20): they benefit from the constraint and can exit by investing in gene therapy portfolios or licensing approved therapies. Regulatory agencies experience moderate-high d (~0.55): constrained by liability and political pressure, unable to fully exit despite seeing institutional costs. The sigma scope modifier (σ(S)) scales extractiveness: national scope (σ=1.0) for most actors, but global scope (σ=1.2) for incumbent pharma benefits and patient harm (global therapeutic markets, global disease burden). The f(d) sigmoid converts these d values to power modifiers ranging from -0.12 (institutional beneficiaries) to 1.42 (powerless victims), determining who experiences the constraint as coordination vs extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE CONFIRMATION: This constraint resolves the mandatrophy by showing why it cannot be classified as pure rope (coordination only) despite genuine safety verification function. The presence of systematic extraction is not eliminated by the presence of coordination. The beneficiary (incumbent pharma) captures market protection value from approval delay. The victim (patient) bears cost from approval delay. Both effects are real and structural. A pure rope classification (coordination with χ ≤ 0.35) would miss that the regulatory apparatus is actively extracting organizational legitimacy through theater (rising theater ratio) and suppressing startup competitors through approval timeline asymmetry. Conversely, snare classification (extraction with no coordination benefit, χ ≥ 0.66) would overstate the case: regulatory delay does prevent approval of genuinely unsafe therapies, does catch safety signals in trials, and does generate patient-protective information. The mandatrophy is resolved by recognizing that extraction and coordination can be structurally intertwined (tangled) rather than existing in separate constraints. The agency that coordinates safety also benefits from institutional gatekeeping. The delay that prevents harm also extracts years of life. Both mechanisms operate simultaneously, not as alternatives. Mandatrophy resolved: the classification is structurally precise because it identifies the hybrid mechanism, not because it chooses between impossible alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    safety_verification_plateau,
    'Does additional regulatory delay beyond Year 5 of Phase III trials actually reduce long-term adverse event risk, or does it plateau?',
    'Post-market surveillance data: comparison of adverse event profiles for early-approved therapies vs delayed-approval equivalents; meta-analysis of safety curves across Phase III extension, Phase IV, and 5-year post-market periods',
    'If plateau occurs before Year 7: regulatory extraction is substantially overhead (recommend 3-5 year timeline). If delay continues reducing risk: extraction is coordinating genuine safety discovery (recommend current 7-10 year timeline).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_verification_plateau, empirical, 'Whether additional delay beyond Year 5 Phase III reduces long-term safety risk').

omega_variable(
    rare_disease_patient_population_evolution,
    'What proportion of rare disease patients die or become terminally dependent during the approval window (5-10 years) due to disease progression?',
    'Natural history studies; retrospective cohort analysis of patients who died or reached end-stage disease while awaiting approval; comparison to comparable non-gene-therapy rare diseases with historical mortality curves',
    'If >30% die/reach irreversibility during window: extraction is catastrophic (snare classification confirmed, argue for compassionate use sunset). If <10%: extraction is real but not immediately life-threatening (tangled rope confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rare_disease_patient_population_evolution, empirical, 'Mortality/irreversibility during regulatory approval window').

omega_variable(
    incumbent_firm_approval_asymmetry,
    'Do incumbent firms face systematically faster approval timelines than startups for comparable somatic gene therapies, controlling for technical complexity?',
    'Comparative analysis of FDA approval timelines by applicant type (large pharma vs biotech startup) for 2015-2025 gene therapy and advanced therapy cohorts; regression analysis controlling for indication complexity, mechanism novelty, and trial design',
    'If timeline difference >2 years: confirms incumbent advantage and validates startup extraction perspective (justify higher tangled_rope classification for startups). If <6 months: suggests approval bottleneck is uniform and barriers are technical, not institutional.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incumbent_firm_approval_asymmetry, empirical, 'Approval timeline asymmetry by applicant firm size').

omega_variable(
    regulatory_agency_approval_standard_drift,
    'Have FDA and EMA somatic gene therapy approval standards drifted over time (2010-2026) in ways that extend approval timelines independent of scientific need?',
    'Historical analysis of regulatory guidance documents; content analysis of FDA advisory committee transcripts; comparison of approval standards applied to early approvals vs recent approvals for functionally equivalent mechanisms',
    'If standards have tightened beyond technical justification: supports piton and regulatory extraction perspective (recommend standardization/harmonization). If standards reflect accumulating safety knowledge: supports mountain and rope perspectives (recommend current timelines).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_agency_approval_standard_drift, empirical, 'Historical drift in somatic gene therapy approval standards').

omega_variable(
    real_world_evidence_adequacy,
    'Can long-term safety of somatic gene therapies be monitored with equal statistical power through real-world evidence (patient registries, electronic health records) vs traditional Phase IV post-market surveillance?',
    'Simulation study comparing statistical power of rare-disease registries with active follow-up vs Phase IV trials with 90% attrition; prospective validation using early-approved gene therapies with both traditional Phase IV and real-world evidence monitoring',
    'If real-world evidence is adequate: supports scaffold sunset clause (recommend adaptive licensing that gates approval on registry enrollment, not trial completion). If inadequate: supports piton perspective (traditional paradigm justified despite limitations).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(real_world_evidence_adequacy, empirical, 'Whether real-world evidence provides adequate safety monitoring').

omega_variable(
    irreversibility_justification_threshold,
    'What degree of mechanism irreversibility actually justifies the observed approval timeline extension, and do somatic gene therapies exceed that threshold?',
    'Comparative analysis across therapeutic irreversibility levels (surgical correction vs pharmaceuticals vs permanently integrated gene therapy); Bayesian decision analysis of approval timeline vs irreversibility; expert consensus on justified caution per mechanism type',
    'If somatic gene therapies fall within justified irreversibility threshold: current timelines are proportionate (mountain/rope perspective justified). If they exceed threshold by >2x: extraction is disproportionate to actual risk (snare/tangled rope confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irreversibility_justification_threshold, conceptual, 'Proportionality of approval timeline to therapy irreversibility').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(somatic_gene_therapy_approval, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sgt_tr_t0, somatic_gene_therapy_approval, theater_ratio, 0, 0.52).
narrative_ontology:measurement(sgt_tr_t5, somatic_gene_therapy_approval, theater_ratio, 5, 0.62).
narrative_ontology:measurement(sgt_tr_t10, somatic_gene_therapy_approval, theater_ratio, 10, 0.68).
narrative_ontology:measurement(sgt_tr_t15, somatic_gene_therapy_approval, theater_ratio, 15, 0.71).

% Extraction over time
narrative_ontology:measurement(sgt_be_t0, somatic_gene_therapy_approval, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(sgt_be_t5, somatic_gene_therapy_approval, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(sgt_be_t10, somatic_gene_therapy_approval, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(sgt_be_t15, somatic_gene_therapy_approval, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(somatic_gene_therapy_approval, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(somatic_gene_therapy_approval, 0.12).
narrative_ontology:affects_constraint(somatic_gene_therapy_approval, germline_gene_therapy_prohibition).
narrative_ontology:affects_constraint(somatic_gene_therapy_approval, rare_disease_pharmaceutical_pricing).
narrative_ontology:affects_constraint(somatic_gene_therapy_approval, clinical_trial_phase_duration).

% DUAL FORMULATION NOTE:
% Somatic gene therapy approval is upstream of germline therapy prohibition (regulatory precedent) and rare disease pricing constraints (market dynamics after approval). The approval bottleneck influences but does not cause rare disease pricing extraction—these are structurally distinct constraints linked by causal sequence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(somatic_gene_therapy_approval, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
