% ============================================================================
% CONSTRAINT STORY: genetic_determinism_in_criminal_justice
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genetic_determinism_in_criminal_justice, []).

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
 *   constraint_id: genetic_determinism_in_criminal_justice
 *   human_readable: Genetic Determinism Framing in Criminal Justice
 *   domain: criminal_justice/neuroscience/policy
 *
 * SUMMARY:
 *   Genetic determinism framing in criminal justice is a structural
 *   constraint that naturalizes incarceration as a biological inevitability
 *   rather than a policy outcome. The constraint operates by converting
 *   social and structural factors (poverty, systemic racism, policing bias,
 *   unequal opportunity) into genetic narratives that locate criminality in
 *   individual biology. This frame benefits prosecutorial institutions by
 *   reducing accountability pressure, while imposing maximum cost on
 *   defendants from marginalized populations — particularly Black defendants,
 *   who face disproportionate application of genetic determinism rhetoric.
 *   The constraint is neither pure coordination nor pure extraction from all
 *   perspectives; instead, it exhibits the full Deferential Realism spectrum:
 *   prosecutors see coordination, defendants see snare, reformers see a
 *   temporary problem with a sunset, the expert witness system maintains
 *   itself through performative theater, and the analytical observer sees a
 *   hybrid structure combining genuine explanatory function with asymmetric
 *   institutional extraction. The theater_ratio (0.64) reflects that much
 *   genetic expert testimony operates as ritual performance: the procedural
 *   form (expert testimony, scientific language, court authority) persists
 *   despite weak epistemic foundation. Extractiveness has risen over the
 *   measurement interval (0.42 to 0.58) as neuroscientific claims have
 *   proliferated in courtroom settings and gained unwarranted authority.
 *
 * KEY AGENTS:
 *   - Defendants from Marginalized Populations: Primary victims (powerless/trapped) — bear full cost of genetic determinism framing; no exit mechanism; subject to heightened incarceration through naturalized biological narrative
 *   - Prosecutorial Institutions: Primary beneficiaries (institutional/arbitrage) — reduce accountability pressure by attributing crime to biology rather than policy failures; can abandon frame if politically costly
 *   - Public Defenders: Secondary actor (moderate/constrained) — face resource barriers and professional norms that legitimize genetic testimony; experience moral cost while constrained by system
 *   - Criminal Justice Reform Coalition: Organized agents (organized/constrained) — building alternative narrative infrastructure and legislative reform pathways; see sunset potential for genetic determinism frame
 *   - Neuroscience Expert Witness System: Institutional actor (institutional/arbitrage) — maintains authority through performative testimony despite scientific limitations; aware of own degradation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes constraint as hybrid coordination-extraction mechanism with asymmetric distribution across racial groups
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genetic_determinism_in_criminal_justice, 0.58).
domain_priors:suppression_score(genetic_determinism_in_criminal_justice, 0.68).
domain_priors:theater_ratio(genetic_determinism_in_criminal_justice, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genetic_determinism_in_criminal_justice, extractiveness, 0.58).
narrative_ontology:constraint_metric(genetic_determinism_in_criminal_justice, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(genetic_determinism_in_criminal_justice, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genetic_determinism_in_criminal_justice, snare).
narrative_ontology:human_readable(genetic_determinism_in_criminal_justice, "Genetic Determinism Framing in Criminal Justice").
narrative_ontology:topic_domain(genetic_determinism_in_criminal_justice, "criminal_justice/neuroscience/policy").

domain_priors:requires_active_enforcement(genetic_determinism_in_criminal_justice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genetic_determinism_in_criminal_justice, prosecutorial_institutions).
narrative_ontology:constraint_beneficiary(genetic_determinism_in_criminal_justice, incarceration_infrastructure).
narrative_ontology:constraint_victim(genetic_determinism_in_criminal_justice, defendants_from_marginalized_populations).
narrative_ontology:constraint_victim(genetic_determinism_in_criminal_justice, equitable_sentencing_principles).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEFENDANT FROM MARGINALIZED POPULATIONS (SNARE) — Trapped by genetic determinism framing that naturalizes incarceration as inevitable biological outcome. No meaningful exit from this cognitive trap; genetic narrative overrides agency and choice. Suppression is total: alternative narratives (structural inequality, policing bias, institutional racism) are systematically excluded from court proceedings. Extraction is maximal — the defendant bears the entire cost of the naturalized frame while institutions benefit from reduced accountability.
constraint_indexing:constraint_classification(genetic_determinism_in_criminal_justice, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROSECUTORIAL INSTITUTION (ROPE) — Perceives genetic determinism framing as pure coordination mechanism: the narrative solves the institutional problem of high crime rates by attributing them to biology rather than systemic failure. Net beneficiary with arbitrage options — can abandon the frame if it becomes politically costly, yet reaps benefits (reduced accountability pressure, streamlined sentencing, public support for incarceration) while deployed. Experiences the constraint as low-extraction coordination.
constraint_indexing:constraint_classification(genetic_determinism_in_criminal_justice, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: PUBLIC DEFENDER (TANGLED ROPE) — Constrained by resource limitations and professional norms that accept genetic framing as legitimate courtroom argument. Also benefits from genetic frame in narrow way: it simplifies sentencing narrative and reduces need for complex structural analysis. But bears extraction through moral cost and reduced effectiveness — cannot mount robust alternative narrative despite knowing the frame is scientifically unsupported. Mixed experience: coordination of simplified legal procedure alongside extraction of integrity and case effectiveness.
constraint_indexing:constraint_classification(genetic_determinism_in_criminal_justice, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CRIMINAL JUSTICE REFORM COALITION (SCAFFOLD) — Organized agents (advocacy groups, neuroscience oversight bodies, sentencing reform advocates) view genetic determinism framing as a temporary institutional problem with sunset potential. Building alternative narrative infrastructure (genetic literacy programs, sentencing guidelines that exclude speculative biology, institutional review processes for expert testimony). Sees exit path through norm change and legislative reform. Constraint is temporary support structure that can be dismantled as alternative mechanisms mature.
constraint_indexing:constraint_classification(genetic_determinism_in_criminal_justice, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: NEUROSCIENCE EXPERT WITNESS SYSTEM (PITON) — The machinery of neuroscientific expert testimony persists through institutional inertia despite severe scientific limitations. Genetic determinism framing is substantially performative theater: expert witnesses present speculative findings as established science; judges treat neuroscientific claims as authoritative despite poor replicability and contested interpretation. The expert system sees its own authority as degraded (researchers openly acknowledge limitations that don't make it into testimony). Theater ratio is high because the procedural form (expert on stand, scientific testimony, court acceptance) persists despite low epistemic content. Maintained through institutional path dependence, not because it effectively determines guilt.
constraint_indexing:constraint_classification(genetic_determinism_in_criminal_justice, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational and global scope, genetic determinism in criminal justice exhibits both genuine coordination function and asymmetric extraction. Coordination function: provides shared framework for understanding crime etiology across jurisdictions and enables comparative sentencing policy. Asymmetric extraction: naturalizes racial disparities in incarceration (genetic framing allows institutions to blame biology rather than policy), concentrates power in prosecutorial hands (genetic experts become gatekeepers of narrative), and systematically suppresses alternative explanations (structural racism, poverty, policing bias). The constraint requires active institutional enforcement — judges, legislatures, expert witness rules all actively maintain the genetic frame against competing narratives.
constraint_indexing:constraint_classification(genetic_determinism_in_criminal_justice, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genetic_determinism_in_criminal_justice_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(genetic_determinism_in_criminal_justice, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(genetic_determinism_in_criminal_justice, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(genetic_determinism_in_criminal_justice, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(genetic_determinism_in_criminal_justice, TR),
    TR >= 0.70.

:- end_tests(genetic_determinism_in_criminal_justice_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The genetic determinism frame extracts significantly from defendants by naturalizing incarceration and removing accountability from institutions. However, it is not as severe as pure snares (≥0.66) because the frame retains some coordination function — it does provide a coherent explanatory framework, even if speculative and misapplied. The extraction is not total suppression of all alternatives. Suppression (0.68): High. Significant barriers to counter-narratives include: expert testimony rules that accept genetic claims without rigorous scrutiny; professional norms that legitimize neuroscience as authoritative; institutional pressure to process cases rapidly (preventing detailed structural analysis); and media narratives that amplify genetic determinism while suppressing systemic explanations. But suppression is not absolute — criminal justice reform advocates are successfully challenging genetic determinism in some jurisdictions. Theater ratio (0.64): Moderately high. Genetic expert testimony operates substantially as institutional ritual: the form (expert on stand, scientific language, technical jargon, court authority) persists and carries great weight in sentencing decisions, while the epistemic content remains contested and often speculative. Much testimony relies on preliminary findings, small sample sizes, or extrapolations from animal models that don't replicate well. The theater has increased as neuroscientific claims have become fashionable in courtroom settings.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence. Prosecutors experience the genetic determinism frame as efficient coordination — a shared explanatory paradigm that enables consistent sentencing policy. Defendants experience it as pure extraction — their agency is removed, their circumstances are naturalized as biological destiny, and they have no mechanism to contest the frame. Public defenders see a mixed constraint: it simplifies procedure (helpful) while undermining effective defense (harmful). Reformers see a temporary institutional arrangement with a sunset — as genetic literacy spreads and neuroscience standards tighten, the frame will lose authority. The expert witness system sees its own theatrical quality but can't exit without losing professional status. The analytical observer sees the constraint as requiring active institutional enforcement: judges, legislatures, and professional licensing bodies all work to maintain genetic determinism despite scientific limitations. The gap is not merely disagreement about facts — it's disagreement about whether the frame is natural, inevitable, efficient, or extractive.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values reflect each agent's structural relationship to the extraction flow. Defendants from marginalized populations are targets (d ≈ 0.95): trapped exit, victim status, no arbitrage options. Prosecutorial institutions are beneficiaries (d ≈ 0.10): arbitrage exit, institutional power, extraction flows toward them. Public defenders occupy intermediate position (d ≈ 0.55): constrained exit, mixed status (benefit from simplified procedure, bear cost of reduced case effectiveness and moral compromise). Reform coalition (d ≈ 0.60): constrained exit despite organizing, but see exit path through norm change. Expert witnesses derive from institutional perspective with arbitrage (d ≈ 0.15): can maintain or abandon genetic testimony depending on professional costs/benefits. The analytical observer (d ≈ 0.70): positioned to see the full extraction structure but analytically, not materially.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by revealing that genetic determinism is neither pure coordination nor pure extraction — it is a tangled rope at the analytical level. The frame coordinates criminal justice responses across jurisdictions (genuine function: coherent explanatory model). But it extracts asymmetrically: benefits prosecutorial institutions and incarceration infrastructure while imposing maximum cost on defendants from marginalized populations. The mandatrophy is resolved by documenting both functions simultaneously: the constraint must be classified at analytical level as tangled_rope (not snare), but the snare classification from the defendant's perspective is equally valid. The polymorph across perspectives is the answer — no single type is 'correct' because each perspective captures something structurally real about the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genetic_causality_threshold,
    'At what level of heritability does a trait become legally and ethically relevant to criminal sentencing — where does descriptive genetics become prescriptive criminal liability?',
    'Neuroscience consensus on gene-crime pathway specificity; examination of actual courtroom expert testimony to determine claimed heritability thresholds; comparison with standards in medical/insurance contexts where genetic information is restricted',
    'If threshold is high (>0.70 heritability): most genetic testimony is legally inapplicable, snare classification strengthens. If threshold is low (<0.30): genetic framing becomes institutionally legitimate, classification shifts toward rope for institutional perspectives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(genetic_causality_threshold, conceptual, 'Legal threshold for genetic relevance in criminal sentencing').

omega_variable(
    gene_environment_decomposition,
    'Can courtroom expert testimony reliably partition crime etiology into genetic vs environmental components, or is the decomposition inherently speculative?',
    'Analysis of twin studies and adoption study methodologies used to generate heritability estimates; examination of gene-environment interactions that violate simple additive models; review of actual expert testimony to determine claimed decomposability',
    'If decomposition is reliable: genetic framing is scientifically justified, extraction component decreases. If fundamentally speculative: the entire genetic determinism frame becomes pseudoscientific theater, snare classification strengthens dramatically.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gene_environment_decomposition, empirical, 'Whether gene-environment decomposition is scientifically tractable in courtroom').

omega_variable(
    racial_genetic_differential_appropriation,
    'Is genetic determinism framing applied equally across racial groups in sentencing, or does the narrative preferentially burden defendants from marginalized populations?',
    'Audit of expert testimony and sentencing outcomes stratified by defendant race; analysis of which populations receive genetic determinism framing vs structural explanations; comparison with medical genetics where genetic findings don''t trigger harsher treatment',
    'If applied equally: genetic frame is institutionally neutral (though still extraction). If differentially applied: the constraint is explicitly racial extraction mechanism, snare classification confirmed, suppression measures revealed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(racial_genetic_differential_appropriation, empirical, 'Racial differential application of genetic determinism in sentencing').

omega_variable(
    expert_testimony_replicability,
    'What percentage of neuroscientific claims about criminality made in expert testimony rest on replicated, peer-reviewed findings vs speculative, preliminary, or contested research?',
    'Systematic review of expert testimony transcripts; cross-reference with neuroscience literature; identify which claims have positive replication records and which remain isolated findings or failed replications',
    'If >70% replicated: genetic framing has epistemic legitimacy, snare classification weakens. If <40% replicated: genetic expert testimony is theater, piton and snare elements strengthen.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(expert_testimony_replicability, empirical, 'Replicability rate of neuroscientific claims in criminal expert testimony').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genetic_determinism_in_criminal_justice, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, genetic_determinism_in_criminal_justice, theater_ratio, 0, 0.55).
narrative_ontology:measurement(gene_tr_t10, genetic_determinism_in_criminal_justice, theater_ratio, 10, 0.61).
narrative_ontology:measurement(gene_tr_t20, genetic_determinism_in_criminal_justice, theater_ratio, 20, 0.64).
narrative_ontology:measurement(gene_tr_t30, genetic_determinism_in_criminal_justice, theater_ratio, 30, 0.64).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, genetic_determinism_in_criminal_justice, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(gene_be_t10, genetic_determinism_in_criminal_justice, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(gene_be_t20, genetic_determinism_in_criminal_justice, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(gene_be_t30, genetic_determinism_in_criminal_justice, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genetic_determinism_in_criminal_justice, enforcement_mechanism).
narrative_ontology:affects_constraint(genetic_determinism_in_criminal_justice, neuroscience_expert_testimony_standards).
narrative_ontology:affects_constraint(genetic_determinism_in_criminal_justice, sentencing_guideline_equity).

% DUAL FORMULATION NOTE:
% Genetic determinism in criminal justice is a specific instantiation of the broader constraint family around neuroscientific authority in legal proceedings. The upstream constraint is neuroscience expert testimony standards (ε ≈ 0.35, rope: reasonable epistemic gatekeeping function); the downstream constraint is genetic determinism application in sentencing (ε ≈ 0.58, snare from defendant perspective: naturalization mechanism). These are decomposed as separate stories because their ε values differ by factor of ~1.65, reflecting different measurement observables (testimony quality standards vs sentencing outcome equity). The network link shows how low standards in expert testimony enable high extraction in sentencing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(genetic_determinism_in_criminal_justice, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
