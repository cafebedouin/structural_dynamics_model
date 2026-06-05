% ============================================================================
% CONSTRAINT STORY: causal_inference_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_causal_inference_gap, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: causal_inference_gap
 *   human_readable: Causal Inference Gap in Complex Biological Systems
 *   domain: systems_biology/epistemology/translational_medicine
 *
 * SUMMARY:
 *   The causal inference gap in complex biological systems represents a
 *   fundamental structural mismatch between the exponential growth in
 *   disease-associated genetic variants identified through GWAS (tens of
 *   thousands since 2007) and the near-static validation and therapeutic
 *   translation rates (<15% mechanistic validation, <5% clinical
 *   translation). This constraint operates as a tangled rope: the discovery
 *   ecosystem (GWAS teams, funding agencies, journals) benefits from high
 *   publication volume and citation impact while remaining epistemically and
 *   institutionally decoupled from the validation ecosystem (mechanistic
 *   researchers, pharmaceutical companies, disease communities). The gap is
 *   neither purely a natural epistemological limit (causation cannot be
 *   inferred from correlations) nor purely an institutional separation — it
 *   is a hybrid where institutional structures reinforce and exploit the
 *   epistemological gap. The measurement trajectory shows extraction
 *   accumulating over time: base extractiveness rises from 0.32 (early GWAS
 *   era, when discovery and validation were loosely coupled) to 0.58
 *   (contemporary), driven by career incentive misalignment and funding
 *   structure divergence. Theater ratio increases from 0.48 to 0.68,
 *   reflecting that reductionist validation 'successes' (published mechanism
 *   papers) fail to predict clinical translation, making the validation
 *   ritual increasingly performative. Suppression requirement rises from 0.50
 *   to 0.65, indicating that mechanistic researchers face growing barriers:
 *   complex traits require multi-level validation, reductionist dogma resists
 *   systems approaches, and funding for validation remains scarce relative to
 *   discovery.
 *
 * KEY AGENTS:
 *   - GWAS Research Institutions: Institutional beneficiary (institutional/arbitrage) — capture citations, funding, and scientific prestige from association discovery; no pressure to fund validation
 *   - Mechanistic Validation Researchers: Primary victim (powerless/trapped) — trapped by the requirement to validate individual associations with no funding, career reward, or exit pathway; cannot return to GWAS generation without losing standing; cannot fund validation work
 *   - Patient Communities (Untreated Disease): Primary victim (powerless/trapped) — bear the cost of non-translation; face treatment stagnation while association publications accumulate
 *   - Pharmaceutical Industry: Organized secondary victim (organized/constrained) — benefit from candidate filtering but must fund their own validation infrastructure; constrained by lack of public validation data and by reductionist validation paradigm that may not work for complex traits
 *   - NIH/Funding Agencies: Powerful hybrid (powerful/mobile) — benefit from publication metrics and political optics of 'discovering disease genes'; bear suppression costs of pressure to fund translation without institutional mechanisms; mobile exit available but politically costly
 *   - Systems Biology Mapping Initiatives: Emerging organized coalition (organized/constrained) — see the gap as remediable through infrastructure; constrained by need for sustained funding and institutional acceptance of non-reductionist approaches
 *   - Analytical Observer (Natural Law): Civilizational perspective (analytical/analytical) — risks naturalizing the gap as an immutable epistemological problem, obscuring the contingent institutional choices that created it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(causal_inference_gap, 0.58).
domain_priors:suppression_score(causal_inference_gap, 0.65).
domain_priors:theater_ratio(causal_inference_gap, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(causal_inference_gap, extractiveness, 0.58).
narrative_ontology:constraint_metric(causal_inference_gap, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(causal_inference_gap, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(causal_inference_gap, tangled_rope).
narrative_ontology:human_readable(causal_inference_gap, "Causal Inference Gap in Complex Biological Systems").
narrative_ontology:topic_domain(causal_inference_gap, "systems_biology/epistemology/translational_medicine").

domain_priors:requires_active_enforcement(causal_inference_gap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(causal_inference_gap, gwas_research_institutions).
narrative_ontology:constraint_beneficiary(causal_inference_gap, genetics_funding_bodies).
narrative_ontology:constraint_beneficiary(causal_inference_gap, pharmaceutical_gatekeepers).
narrative_ontology:constraint_victim(causal_inference_gap, therapeutic_translation_pipeline).
narrative_ontology:constraint_victim(causal_inference_gap, patient_access_to_validated_mechanisms).
narrative_ontology:constraint_victim(causal_inference_gap, mechanistic_validation_researchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MECHANISTIC VALIDATION RESEARCHER (SNARE) — Trapped by the statistical-mechanistic divide. Cannot exit: publishing non-validated associations consumes research currency; validating individual associations requires years of bench work with limited reward. No pathway exists for translational researchers to move from statistical discovery to mechanistic proof without leaving academia or accepting permanent subordination to GWAS-generating teams. Maximum extraction with suppression through career incentive misalignment.
constraint_indexing:constraint_classification(causal_inference_gap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PATIENT POPULATION — UNTREATED DISEASE (SNARE) — Bears the full cost of non-translation. Identified genetic associations are published and cited extensively but do not translate to new therapeutic mechanisms or clinical interventions. Trapped in disease state while association statistics accumulate. Suppression operates through institutional gatekeeping: drug development requires proof of mechanism (which doesn't exist) and investment (which doesn't flow to unvalidated associations). No mechanism for patients to directly benefit from association discovery.
constraint_indexing:constraint_classification(causal_inference_gap, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: GWAS RESEARCH INSTITUTIONS (ROPE) — Primary beneficiaries. Experience the constraint as pure coordination: identify associations rapidly, publish in high-impact venues, attract funding and citations. For this institutional perspective, the gap is not extraction — it is a solved problem (discovery scaling). No pressure to fund mechanistic validation; the coordination function (disseminating genetic associations) operates efficiently. Arbitrage available through alternative funding sources and career rewards. Extraction flows toward these institutions.
constraint_indexing:constraint_classification(causal_inference_gap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PHARMACEUTICAL INDUSTRY VALIDATION TEAMS (TANGLED ROPE) — Constrained by the requirement to validate mechanisms before investing in drug development. Benefits from GWAS associations (filtering candidates, reducing early-stage screening costs) but bears the suppressive cost of implementing validation protocols that are not standardized, not funded by discovery teams, and often unsuccessful. Active enforcement: companies must develop proprietary validation frameworks because public infrastructure does not exist. Extracted from through the unpaid labor of mechanism identification; benefits through candidate prioritization.
constraint_indexing:constraint_classification(causal_inference_gap, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SYSTEMS BIOLOGY MAPPING INITIATIVES (SCAFFOLD) — Emerging organized effort to bridge the gap through protein-interaction mapping, expression quantitative trait loci (eQTL), and causal inference algorithms (e.g., Mendelian randomization). See the gap as temporary and solvable through infrastructure investment. Constrained by need for sustained funding and by competition with GWAS for institutional attention. The sunset clause is real: as multi-omic integration and causal inference methods mature, the statistical-mechanistic divide becomes traversable. Estimated sunset: 10–15 years for standard causal frameworks to integrate across biological scales.
constraint_indexing:constraint_classification(causal_inference_gap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: REDUCTIONIST MOLECULAR VALIDATION DOGMA (PITON) — The belief that mechanism validation requires single-gene perturbation studies (CRISPR knockout/knockdown, transgenic models, protein biochemistry) has become institutionally inert. This standard persists despite evidence that complex trait mechanisms often involve networks, epistasis, and context-dependence where reductionist validation cannot succeed. The theater ratio is high: publications of 'validated' mechanisms that don't translate to clinical phenotypes; grant reviews that demand reductionist proof even when systems-level logic suggests network models. The functional requirement (understanding causal pathways) has atrophied; the ritual (publish single-gene validation) persists through inertia and funding structure. Theater maintained by prestigious journals and review committees; function degraded to <15% translation rate.
constraint_indexing:constraint_classification(causal_inference_gap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the gap between statistical association and mechanistic causation appears as an immutable property of inference itself: Hume's problem of causation, the impossibility of inferring mechanisms from correlations alone, the fundamental incompleteness of any finite sample size. This perspective naturalizes what is actually a contingent institutional arrangement (institutional separation of discovery and validation, career incentives that reward association generation, lack of funding for mechanistic work). The engine will identify this as a false summit, revealing that the 'fundamental gap' is not a natural law but a constructed constraint.
constraint_indexing:constraint_classification(causal_inference_gap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: NIH / MAJOR FUNDING AGENCIES (TANGLED ROPE) — Powerful actors with mobile exit options who benefit from the current structure (large volume of publications, high citation impact of GWAS) while bearing suppression costs (pressure to fund validation without institutional mechanisms to do so, disease communities demanding translation, reputational risk from non-translation). Active enforcement via grant review requirements for 'preliminary data' and 'mechanistic rationale,' yet mechanism validation funding remains scarce. Experienced extraction is moderate — agencies can redirect funding and create new programs (mobile exit), but doing so requires admitting the gap exists and shifting political capital.
constraint_indexing:constraint_classification(causal_inference_gap, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(causal_inference_gap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(causal_inference_gap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(causal_inference_gap, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(causal_inference_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(causal_inference_gap, TR),
    TR >= 0.70.

:- end_tests(causal_inference_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, reflecting significant asymmetry. GWAS institutions extract benefit (citations, funding, discovery credit) while bearing minimal cost of validation. Mechanistic researchers bear high cost (validation work, career risk, scarce funding) with minimal reward. The constraint extracts from the validation pipeline and patient populations toward the discovery pipeline. However, extractiveness is not as severe as a pure snare (0.66+) because partial coordination function exists: GWAS genuinely identifies disease-associated variants, which are useful information even without mechanisms. The extraction layer sits atop legitimate coordination. Suppression (0.65): High. Barriers to mechanistic validation include: resource scarcity (validation costs 10–100x more than association identification), reductionist validation dogma (single-gene perturbation models are institutionally enforced but epistemically insufficient), funding structure (discovery funding is abundant, validation funding is sparse), career incentive misalignment (validation publications have lower citation impact than discovery papers, and timeframes exceed typical grant cycles), and institutional gatekeeping (journals prefer novel associations over mechanistic validation, pharmaceutical companies bear validation costs). These barriers are partly material (funding scarcity) and partly epistemic/institutional (dogma, career incentives). Theater ratio (0.68): High. The validation ritual has become increasingly performative. Reductionist perturbation studies (CRISPR knockdowns, transgenic models) are published as 'mechanism validation' but frequently fail to predict clinical phenotypes or drug efficacy. The theater has increased as validation has been institutionalized without reform — validation papers are now expected and published despite low predictive power. The ritual persists because journals, funding bodies, and pharmaceutical companies all demand it, even though it often doesn't work.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits the sharpest perspectival gap between GWAS institutions (rope/arbitrage) and mechanistic researchers (snare/trapped). For GWAS teams, the constraint is solved: they identify associations rapidly and publish prolifically. Mechanistic researchers experience it as inescapable: they cannot exit discovery (no pathway, insufficient standing) and cannot adequately fund validation (scarce resources). The pharmaceutical industry experiences it as tangled rope: they benefit from association filtering but must absorb the cost of building validation infrastructure. The systems biology coalition sees a temporary gap (scaffold) with a sunset — emerging causal inference methods will bridge the divide. The piton perspective reveals that reductionist validation is theater: the ritual persists (mechanism papers are published, grants are reviewed for 'mechanistic rationale') despite low translation success, indicating institutional inertia rather than functional validation. The natural law (mountain) perspective risks misclassifying a contingent institutional gap as an immutable epistemological limit. The engine will identify this as a false summit: the gap is constructed, not natural.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) is determined by beneficiary/victim status and exit options. GWAS institutions have beneficiary status (extract value) and arbitrage exit (can shift research focus if incentives change) → low d → negative chi, experienced as rope. Mechanistic researchers have victim status (bear validation cost) and trapped exit (cannot exit the field without career loss) → high d → high f(d), experienced as snare. Pharmaceutical companies have mixed status (benefit from candidates, bear validation cost) and constrained exit (must validate to develop drugs, but validation is unpaid public service) → d ≈ 0.50-0.60 → moderate chi, experienced as tangled rope. Systems biology initiatives have victim status (must fund research without institutional support) but constrained exit (can pursue alternative funding sources, collaborations) → d ≈ 0.55-0.65 → moderate-high chi, experienced as scaffold because they perceive a sunset. NIH agencies have beneficiary status (funding ratios favor discovery) but face powerful victim constituencies (disease communities) and mobile exit (can reallocate funding) → d ≈ 0.50-0.55 → moderate chi.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by recognizing that the constraint is genuinely tangled rope at the structural level: it coordinates discovery (identifies disease associations, provides candidate filtering) while extracting from validation (asymmetric cost distribution, suppressed mechanistic pathways). The classification is NOT 'is this coordination or extraction?' but 'does this satisfy both gates for tangled rope?' (1) Does genuine coordination function exist? Yes — GWAS provides real informational value. (2) Does asymmetric extraction exist? Yes — GWAS institutions extract benefit while mechanistic researchers and patients bear costs. (3) Is active enforcement required? Yes — the institutional separation of discovery and validation is maintained through funding structure, career incentive alignment, and institutional review standards that privilege discovery over translation. All three tangled rope gates are satisfied. The high extractiveness (0.58) reflects that extraction dominates the functional value — for every genome-wide association identified, <5% translates to clinical intervention, making the extraction component substantial relative to coordination. The snare perspectives (mechanistic researchers, patients) are correct locally — they experience the constraint as pure extraction with no exit. The rope perspective (GWAS institutions) is correct locally — they experience it as coordination with high reward. The tangled rope classification is the structural description that encompasses both: the constraint's existence depends on both coordination and extraction; neither alone would sustain it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mechanism_definition_ambiguity,
    'What constitutes a valid ''mechanistic explanation'' for a genetic association? Single-gene perturbation? Pathway models? Systems-level causal graphs? Network effects?',
    'Empirical: correlate validation standard with clinical translation success; determine which mechanistic frameworks predict therapeutic efficacy best. Conceptual: clarify whether reductionist single-gene mechanisms are sufficient or whether network models are required for complex traits.',
    'If single-gene reductionism is sufficient: current validation approach is adequate, and low translation rates reflect other constraints (drug development economics, polygenic architecture). If networks required: entire validation infrastructure must shift, rendering current piton status permanent unless reformed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mechanism_definition_ambiguity, empirical, 'Definition of valid mechanistic explanation for genetic associations').

omega_variable(
    causal_inference_algorithm_sufficiency,
    'Do emerging causal inference algorithms (Mendelian randomization, instrumental variables, network analyses) actually bridge the statistical-mechanistic gap, or do they merely substitute one form of unvalidated inference for another?',
    'Empirical: longitudinal comparison of therapeutic translation rates for associations validated via classical reductionism vs algorithm-based causal inference. Track false-positive rates and reproducibility.',
    'If algorithms work: scaffold perspective is correct, and sunset is achievable. If algorithms fail: the gap is deeper than institutional infrastructure; fundamentally linked to the inferential limits of complex systems, and snare perspective dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_inference_algorithm_sufficiency, empirical, 'Whether causal inference algorithms effectively bridge statistical-mechanistic gap').

omega_variable(
    polygenicity_barrier,
    'Is the low translation rate fundamentally caused by polygenic architecture (most disease-associated variants have tiny individual effect sizes, making individual validation impractical), or by institutional separation of discovery and validation?',
    'Empirical: comparison of translation rates for oligo-allelic traits vs highly polygenic traits; analysis of whether increasing effect-size threshold improves validation success. Structural: examine whether polygenicity is inherent or partly caused by current GWAS design (discovery power) vs replication power.',
    'If polygenicity dominates: institutional reforms cannot solve the gap; it is rooted in the statistical nature of complex traits. If institutional separation dominates: the gap is remediable through funding and infrastructure changes. Likely: both factors interact; decomposition is essential for setting realistic expectations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(polygenicity_barrier, empirical, 'Relative contribution of polygenic architecture vs institutional separation to translation gap').

omega_variable(
    reductionist_validation_sufficiency,
    'Do single-gene perturbation studies (CRISPR, transgenics) actually validate complex trait mechanisms, or do they validate that the gene participates in biology but not that it causes disease through the proposed pathway?',
    'Epistemological: clarify the logical structure of reductionist validation. Does perturbing a gene in controlled settings prove it causes disease in humans? Empirical: correlate reductionist validation papers with clinical translation success; identify failures.',
    'If reductionism proves insufficient: piton classification is correct, and the validation dogma is theater. Mechanism validation requires network and system-level approaches, which current institutions do not fund. If sufficient: scale up reductionist validation capacity and expect translation rate improvements.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reductionist_validation_sufficiency, conceptual, 'Whether reductionist perturbation studies prove causal disease mechanisms').

omega_variable(
    funding_structure_constraint,
    'Is the low translation rate caused by GWAS generating more associations than mechanistic resources can validate (discovery-validation ratio imbalance), or by structural incentives that reward association generation but not validation?',
    'Empirical: model required validation capacity for current association volume; determine whether increasing validation funding would solve the bottleneck or whether constraints are knowledge-based (we don''t know how to validate these) vs resource-based (we can''t afford to). Historical: compare institutional funding ratios across genomics research, disease research, and translational research; identify structural misalignment.',
    'If imbalance correctable by funding: scaffold or organized actor (NIH/agencies) can implement targeted programs. If structural incentives dominate: cultural and institutional reform is required, not just budget reallocation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(funding_structure_constraint, empirical, 'Whether causal inference gap is caused by discovery-validation capacity imbalance or structural incentive misalignment').

omega_variable(
    false_summit_natural_law,
    'Is the causal inference gap a fundamental and immutable property of statistical epistemology (Hume''s problem, unprovable from finite data), or a contingent institutional arrangement that can be reformed?',
    'Philosophical: establish whether the gap is inherent to inference itself or to current institutional practices. Empirical: show whether historical periods with integrated discovery-validation systems (e.g., early pharmacology before GWAS) achieved better association-to-mechanism ratios. Structural: document whether the gap has widened as institutional separation has increased.',
    'If immutable: mountain classification is correct, and acceptance rather than reform is appropriate policy. If contingent: the gap is a snare/tangled rope, and reform is possible and ethically mandated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law, conceptual, 'Whether causal inference gap is fundamental or contingent institutional arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(causal_inference_gap, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(causal_inf_tr_t0, causal_inference_gap, theater_ratio, 0, 0.48).
narrative_ontology:measurement(causal_inf_tr_t6, causal_inference_gap, theater_ratio, 6, 0.58).
narrative_ontology:measurement(causal_inf_tr_t12, causal_inference_gap, theater_ratio, 12, 0.68).

% Extraction over time
narrative_ontology:measurement(causal_inf_be_t0, causal_inference_gap, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(causal_inf_be_t6, causal_inference_gap, base_extractiveness, 6, 0.45).
narrative_ontology:measurement(causal_inf_be_t12, causal_inference_gap, base_extractiveness, 12, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(causal_inf_su_t0, causal_inference_gap, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(causal_inf_su_t6, causal_inference_gap, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(causal_inf_su_t12, causal_inference_gap, suppression_requirement, 12, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(causal_inference_gap, information_standard).
narrative_ontology:affects_constraint(causal_inference_gap, pharmaceutical_translation_bottleneck).
narrative_ontology:affects_constraint(causal_inference_gap, reductionist_validation_dogma).
narrative_ontology:affects_constraint(causal_inference_gap, gwas_publication_bias).

% DUAL FORMULATION NOTE:
% The causal inference gap decomposes into three structural constraints: (1) causal_inference_gap itself (ε=0.58, tangled_rope) — the statistical-mechanistic divide; (2) pharmaceutical_translation_bottleneck (ε=0.72, snare) — the requirement for validated mechanisms before drug development; (3) reductionist_validation_dogma (ε=0.65, piton) — the performative theater of single-gene perturbation studies. The second and third stories are downstream of the first: the gap is created by institutional separation of discovery and validation; pharmaceutical development requirements impose a validation bottleneck; reductionist dogma provides a performative (non-functional) validation framework. These three constraints interact: the gap causes the bottleneck causes the dogma's persistence as theater.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
