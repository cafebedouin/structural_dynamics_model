% ============================================================================
% CONSTRAINT STORY: replication_crisis_biomedicine
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_replication_crisis_biomedicine, []).

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
 *   constraint_id: replication_crisis_biomedicine
 *   human_readable: Replication Crisis in Biomedical Research
 *   domain: biomedical_research/institutional_science
 *
 * SUMMARY:
 *   The replication crisis in biomedical research represents a structural
 *   constraint where the incentive architecture of academic science —
 *   publish-or-perish competition, novelty premium, impact factor obsession,
 *   and career advancement tied to citations — creates systematic pressure
 *   toward false positive publication while suppressing negative results and
 *   replication studies. This constraint exhibits multiple structurally
 *   distinct mechanisms: (1) the rational response of early-career
 *   researchers to career incentives (coordination problem: how to allocate
 *   limited academic positions), (2) the extraction mechanism where novel but
 *   unreplicated findings generate career and financial benefits for
 *   claimants while imposing costs on clinical practice and the reliability
 *   commons, (3) the performative peer review apparatus that maintains
 *   theatrical legitimacy without detecting methodological failures, and (4)
 *   the false natural law framing that irreproducibility is inherent to
 *   exploratory biology rather than a contingent institutional arrangement.
 *   The theater ratio (0.81) reflects that peer review for complex biological
 *   claims is substantially ritualistic: reviewers assess novelty and
 *   plausibility but cannot verify experimental technique, detect p-hacking,
 *   or assess selective outcome reporting from manuscript alone. The
 *   extractiveness value (0.58) reflects moderate-to-high extraction with
 *   some genuine coordination functions (journal editors do allocate
 *   resources, peer review does screen some false positives) embedded
 *   alongside the extraction mechanism. The suppression value (0.68) reflects
 *   significant barriers to exit: publication bias against negative results,
 *   replication studies are unfunded and undervalued, career risk for
 *   researchers who publish replication failures, and structural inability of
 *   clinical practice to audit the literature it relies on.
 *
 * KEY AGENTS:
 *   - Patient Populations: Primary victim (powerless/trapped) — bear health costs of unreplicated clinical claims with no exit option
 *   - Clinical Practice Reliability: Primary victim (powerless/trapped) — abstract epistemic commons that cannot exit or organize; contaminated by systematic false positives
 *   - Independent Replication Groups: Secondary victim (moderate/constrained) — face funding barriers, publication bias, and career risk despite providing verification function
 *   - Early-Career Researchers: Primary beneficiary (institutional/arbitrage) — capture citations, grants, and career advancement through novel findings regardless of replicability
 *   - Pharmaceutical Marketing: Beneficiary (institutional/arbitrage) — leverages unreplicated biomedical claims for product positioning and market advantage
 *   - Funding Agencies and Journal Editors: Institutional actors (powerful/constrained) — benefit from novelty and citation metrics while facing pressure from infrastructure limitations; constrained by stakeholder expectations
 *   - Peer Review Apparatus: Performative institutional mechanism (institutional/arbitrage) — maintains theatrical legitimacy without functional verification capacity; persists through inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent biological limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(replication_crisis_biomedicine, 0.58).
domain_priors:suppression_score(replication_crisis_biomedicine, 0.68).
domain_priors:theater_ratio(replication_crisis_biomedicine, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(replication_crisis_biomedicine, extractiveness, 0.58).
narrative_ontology:constraint_metric(replication_crisis_biomedicine, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(replication_crisis_biomedicine, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(replication_crisis_biomedicine, tangled_rope).
narrative_ontology:human_readable(replication_crisis_biomedicine, "Replication Crisis in Biomedical Research").
narrative_ontology:topic_domain(replication_crisis_biomedicine, "biomedical_research/institutional_science").

domain_priors:requires_active_enforcement(replication_crisis_biomedicine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(replication_crisis_biomedicine, early_career_claim_makers).
narrative_ontology:constraint_beneficiary(replication_crisis_biomedicine, pharmaceutical_marketing).
narrative_ontology:constraint_beneficiary(replication_crisis_biomedicine, research_funding_institutions).
narrative_ontology:constraint_victim(replication_crisis_biomedicine, clinical_practice_reliability).
narrative_ontology:constraint_victim(replication_crisis_biomedicine, patient_populations).
narrative_ontology:constraint_victim(replication_crisis_biomedicine, independent_replication_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PATIENT POPULATIONS (SNARE) — Clinical practice relies on biomedical literature that is systematically inflated and unreplicated. Patients cannot exit this constraint; they bear the cost through ineffective treatments, adverse outcomes, and wasted health resources. No exit option and maximum experienced extraction from a trapped position.
constraint_indexing:constraint_classification(replication_crisis_biomedicine, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CLINICAL PRACTICE RELIABILITY (SNARE) — Abstract epistemic commons that cannot organize or exit. Systematic publication bias and p-hacking contaminate the clinical knowledge base. The field's reliability infrastructure bears cost without capacity to defend itself. Pure extraction with no coordination function from this perspective.
constraint_indexing:constraint_classification(replication_crisis_biomedicine, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: INDEPENDENT REPLICATION GROUPS (TANGLED ROPE) — Constrained by funding scarcity, publication bias against negative results, and career risk. Provides genuine coordination function through attempted verification. Benefits from legitimacy as rigorous researchers but bears extraction through replication burden and publication friction. Mixed experience with both coordination and extraction.
constraint_indexing:constraint_classification(replication_crisis_biomedicine, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: EARLY-CAREER RESEARCHERS AND PHARMACEUTICAL MARKETING (ROPE) — Net beneficiaries during claim-staking window. Early-career researchers capture citations, grants, and career advancement through novel findings regardless of replicability. Pharmaceutical marketing leverages unreplicated claims for product positioning. Experience the constraint as pure coordination: literature production is their coordination mechanism. Low chi from beneficiary + arbitrage exit position.
constraint_indexing:constraint_classification(replication_crisis_biomedicine, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: FUNDING AGENCIES AND JOURNAL EDITORS (TANGLED ROPE) — Powerful institutional actors with constrained exit options. Benefit from publishing novel findings and allocating research funds. Face pressure from peer review infrastructure limitations. Experience mixed signals: genuine coordination role (allocating resources) alongside extraction (gaming metrics, theater maintenance). Institutional power is constrained by stakeholder expectations.
constraint_indexing:constraint_classification(replication_crisis_biomedicine, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: PEER REVIEW AND STATISTICAL TESTING APPARATUS (PITON) — Performative verification infrastructure. Peer review cannot detect p-hacking, data dredging, or selective outcome reporting. Statistical significance testing (p < 0.05) is a ritualized gate that generates false positives when applied to exploratory analysis. The apparatus persists through institutional inertia despite known failure modes. Theater ratio reflects that the review ritual is largely performative — it does not verify the claims it gates.
constraint_indexing:constraint_classification(replication_crisis_biomedicine, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational perspective, some irreproducibility is inherent to exploratory science: multiple hypothesis testing, noise, and researcher degrees of freedom create false positives inherently. The mountain classification naturalizes what is actually a contingent institutional arrangement — the problem is not inherent to biology but to the publication and funding incentives that reward novel findings over replication.
constraint_indexing:constraint_classification(replication_crisis_biomedicine, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(replication_crisis_biomedicine_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(replication_crisis_biomedicine, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(replication_crisis_biomedicine, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(replication_crisis_biomedicine, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(replication_crisis_biomedicine, TR),
    TR >= 0.70.

:- end_tests(replication_crisis_biomedicine_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-to-high, reflecting that the constraint systematically favors novel claimants over replicators. Early-career researchers rationally respond to incentives; the extraction is not from fraud but from structural asymmetry in how the scientific system rewards and distributes reputation. The value increased from 0.38 to 0.58 over the measurement interval as awareness of the crisis heightened without corresponding structural reform. Suppression (0.68): Moderate-high, reflecting multiple barriers to exit: publication bias against negative results is structural (editors perceive negative studies as lower novelty), replication studies receive minimal funding, replication failure carries career risk (appears as cautious or unable to confirm others' work), and clinical practitioners cannot easily audit the literature they rely on. Theater ratio (0.81): High and increasing, indicating that peer review has become increasingly performative. Reviewers cannot detect selective outcome reporting, p-hacking, or insufficient statistical power from manuscript text alone. The ritual persists because alternatives (preprint scrutiny, preregistration) are still maturing. The slight decrease from 0.81 to 0.79 at endpoint reflects early adoption of open science practices (OSF preregistration, data sharing), which lower theater by moving verification mechanisms outside the peer review gate.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence: beneficiaries see coordination (Rope), victims see pure extraction (Snare), intermediate actors see mixed (Tangled Rope), the institutional gate-keeping apparatus sees itself as degraded theater (Piton), and the civilizational view risks naturalizing institutional arrangements as biological law (Mountain). This presheaf of perspectives is diagnostic: if all perspectives produced the same classification, the constraint would be simpler; the perspectival gap is the constraint's signature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from the agent's structural position: power level, exit options, and relationship to extraction flow. Early-career researchers and pharma firms occupy d ≈ 0.10 (beneficiaries with arbitrage exit) — they experience negative effective extraction (the system subsidizes their career advancement). Replication groups occupy d ≈ 0.60 (moderate power, constrained exit, victim-adjacent) — they experience moderate positive extraction despite providing coordination function. Patients and clinical practice occupy d ≈ 0.95 (powerless, trapped) — they experience maximum extraction with no exit capacity. Funding agencies occupy d ≈ 0.45 (powerful but constrained by stakeholder expectations; benefit from novelty but bear reputational cost of crisis). The analytical observer occupies d ≈ 0.72 (can see full structure but cannot unilaterally reform incentives). These derive from beneficiary/victim declarations and exit options without override.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through perspectival multiplicity: there is no single 'correct' type, only the set of legitimate readings from different structural positions. The beneficiary's Rope is their genuine experience (literature production does solve coordination). The victim's Snare is equally real (patients bear extraction with no exit). The false mountain (natural law view) is revealed as naturalization: replication rates vary by domain, showing the crisis is institutional not biological. The piton classification (degraded review apparatus) is corroborated by the high theater ratio and reviewers' own recognition that peer review cannot detect p-hacking. The tangled rope (intermediate actors) captures mixed experience accurately. The mandatrophy resolves not by finding the 'true' type but by understanding that the constraint is fundamentally a presheaf over the observer position landscape — each agent's experience is structurally determined by their role in the extraction flow.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    replicability_threshold_ambiguity,
    'What replication rate threshold distinguishes legitimate statistical noise from systemic extraction through irreproducible claims?',
    'Large-scale replication studies (e.g., OSF Reproducibility Project); meta-analyses comparing effect sizes across original and replication studies; Bayesian assessment of false discovery rate across biomedical domains',
    'If true replication rate is >70%: constraint is primarily coordination problem (Rope from more perspectives). If true rate is <40%: constraint is primarily extraction mechanism (Snare from field perspective). Current estimates (30-50%) suggest tangled extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(replicability_threshold_ambiguity, empirical, 'Threshold for distinguishing noise from systemic extraction').

omega_variable(
    incentive_structure_malignity,
    'How much of the replication crisis is caused by rational self-interested behavior within the current incentive structure versus fraud, incompetence, or negligence?',
    'Comparison of replication failure rates across domains with different incentive structures (high-impact journals vs preprint culture); analysis of researchers'' p-curve patterns and data availability practices; interviews with replication task force members',
    'If primarily rational incentive response: constraint classification remains tangled_rope (coordination + extraction hybrid). If primarily fraud/negligence: reclassify as snare (pure predatory extraction). Affects whether reform pathway is structural redesign or enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incentive_structure_malignity, empirical, 'Whether crisis reflects rational incentives or systemic misbehavior').

omega_variable(
    preregistration_effectiveness,
    'Do preregistration and open science reforms (OSF preregistration, registered reports, data sharing mandates) actually reduce false positive rates or merely displace gaming to new metrics?',
    'Longitudinal comparison of effect sizes and replication rates pre- and post-preregistration adoption; analysis of P-curve compliance among preregistered studies; monitoring for shifting to alternative metrics (confidence intervals, Bayes factors, prior specification gaming)',
    'If effective: scaffold sunset is real — open science reforms are structural exit pathway. If ineffective: extraction mechanism persists despite formal transparency, suggesting enforcement failure or deeper incentive capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preregistration_effectiveness, empirical, 'Whether preregistration reforms effectively reduce false positives').

omega_variable(
    publisher_capture_extent,
    'To what degree are high-impact journal editors and publishers captured by the extraction mechanism (benefiting from sensational unreplicated findings) versus acting as naive gatekeepers?',
    'Analysis of editorial decisions: rejection patterns for null results and replications; comparison of impact factors before/after editorial policy changes; interviews with editors about awareness of replication issues and incentive pressures',
    'If captured: journal system is structural beneficiary of extraction (snare from field perspective). If naive: classification may shift toward scaffold as editors can be educated to select preregistered and replicable studies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(publisher_capture_extent, empirical, 'Degree of publisher capture in replication crisis').

omega_variable(
    biological_complexity_vs_methodological_failure,
    'What proportion of replication failures reflects genuine biological complexity and noise versus methodological failures, poor experimental technique, and insufficient statistical power?',
    'Meta-analysis of replication studies identifying failure modes (underpowered design, poor technique, conditions not reproduced, effect modification); interviews with replicating researchers on source of divergence; comparison of failure rates across biological domains (molecular vs organismal)',
    'If primarily complexity: constraint is natural law with coordination gap (mountain with rope coordination function). If primarily method: constraint is institutional (tangled_rope or snare with extractive gate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biological_complexity_vs_methodological_failure, empirical, 'Whether failures reflect biological complexity or methodological issues').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(replication_crisis_biomedicine, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(repbio_tr_t0, replication_crisis_biomedicine, theater_ratio, 0, 0.62).
narrative_ontology:measurement(repbio_tr_t8, replication_crisis_biomedicine, theater_ratio, 8, 0.75).
narrative_ontology:measurement(repbio_tr_t15, replication_crisis_biomedicine, theater_ratio, 15, 0.81).
narrative_ontology:measurement(repbio_tr_t20, replication_crisis_biomedicine, theater_ratio, 20, 0.79).

% Extraction over time
narrative_ontology:measurement(repbio_be_t0, replication_crisis_biomedicine, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(repbio_be_t8, replication_crisis_biomedicine, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(repbio_be_t15, replication_crisis_biomedicine, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(repbio_be_t20, replication_crisis_biomedicine, base_extractiveness, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(replication_crisis_biomedicine, resource_allocation).
narrative_ontology:affects_constraint(replication_crisis_biomedicine, pharmaceutical_efficacy_claims).
narrative_ontology:affects_constraint(replication_crisis_biomedicine, clinical_guideline_reliability).
narrative_ontology:affects_constraint(replication_crisis_biomedicine, academic_career_incentive_structure).

% DUAL FORMULATION NOTE:
% The replication crisis decomposes into multiple structurally distinct constraints: (1) resource_allocation_coordination — how should scarce academic positions and funding be distributed among researchers (Rope); (2) false_positive_extraction — asymmetric incentive structure that rewards novel claims over replications (Snare); (3) peer_review_degradation — performative verification apparatus (Piton); (4) publication_bias_mechanism — structural suppression of negative results (Tangled Rope). This story treats the family as a single constraint with multiple perspectives rather than decomposing into separate JSON files, because the core structural property (extractiveness from misaligned incentives) is stable across the decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(replication_crisis_biomedicine, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
