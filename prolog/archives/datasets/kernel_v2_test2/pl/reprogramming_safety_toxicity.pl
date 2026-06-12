% ============================================================================
% CONSTRAINT STORY: reprogramming_safety_toxicity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-06-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reprogramming_safety_toxicity, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: reprogramming_safety_toxicity
 *   human_readable: Chemical Reprogramming Safety Toxicity Constraint
 *   domain: biomedical_research/regulatory_science/technology_governance
 *
 * SUMMARY:
 *   Chemical reprogramming — the use of small molecules to convert
 *   differentiated cells into pluripotent or alternative differentiated
 *   states — faces a fundamental dose-toxicity constraint: the concentrations
 *   required to drive epigenetic reprogramming overlap with concentrations
 *   that cause systemic toxicity in animal models. No published evidence
 *   demonstrates safe systemic dosing in mammals for any chemical
 *   reprogramming cocktail that achieves clinically relevant reprogramming
 *   efficiency. This constraint appears as a natural law across all
 *   perspectives: patients cannot exit the dose-response curve,
 *   pharmaceutical companies cannot arbitrage around it, academic researchers
 *   cannot fund their way past it, and regulatory agencies cannot approve
 *   what biochemistry forbids. However, the constraint has identifiable
 *   beneficiaries — pharmaceutical companies capture research funding and
 *   patent portfolios, academic groups capture grants and citations,
 *   regulatory agencies capture institutional authority — which triggers the
 *   false summit detector. The omega variables document the irreducible
 *   uncertainty: is this a genuine Mountain (an immutable limit of mammalian
 *   biochemistry) or a false summit (a contingent constraint naturalized by
 *   institutional arrangements that benefit from its persistence)?
 *
 * KEY AGENTS:
 *   - Patient Population: Powerless/trapped — cannot exit the dose-toxicity relationship; no alternative pathway exists
 *   - Pharmaceutical Companies: Institutional/arbitrage — benefit from research funding, patent portfolios, and regulatory expertise; can arbitrage across therapeutic modalities but not around the fundamental constraint
 *   - Academic Research Groups: Institutional/constrained — benefit from grants, citations, and career advancement; constrained by funding cycles but experience the constraint as natural
 *   - Regulatory Agencies: Institutional/constrained — benefit from institutional authority and toxicology expertise; constrained by statutory mandate but experience the constraint as a natural fact that regulations must accommodate
 *   - Analytical Observer: Analytical/analytical — sees the dose-toxicity relationship as a structural feature of mammalian biochemistry, but must consider whether the beneficiary structure indicates false summit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reprogramming_safety_toxicity, 0.15).
domain_priors:suppression_score(reprogramming_safety_toxicity, 0.2).
domain_priors:theater_ratio(reprogramming_safety_toxicity, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reprogramming_safety_toxicity, extractiveness, 0.15).
narrative_ontology:constraint_metric(reprogramming_safety_toxicity, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(reprogramming_safety_toxicity, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reprogramming_safety_toxicity, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(reprogramming_safety_toxicity, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reprogramming_safety_toxicity, mountain).
narrative_ontology:human_readable(reprogramming_safety_toxicity, "Chemical Reprogramming Safety Toxicity Constraint").
narrative_ontology:topic_domain(reprogramming_safety_toxicity, "biomedical_research/regulatory_science/technology_governance").

domain_priors:emerges_naturally(reprogramming_safety_toxicity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reprogramming_safety_toxicity, pharmaceutical_companies).
narrative_ontology:constraint_beneficiary(reprogramming_safety_toxicity, academic_research_groups).
narrative_ontology:constraint_beneficiary(reprogramming_safety_toxicity, regulatory_agencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reprogramming_safety_toxicity, patient_population).
narrative_ontology:constraint_vindicates(reprogramming_safety_toxicity, dose_response_pharmacology).
narrative_ontology:constraint_vindicates(reprogramming_safety_toxicity, therapeutic_window_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Patients with degenerative diseases or tissue damage who would benefit from cellular reprogramming therapies cannot access them because no safe systemic dosing protocol exists. They bear the cost of the constraint (foregone therapeutic benefit) with no exit option — no alternative pathway to cellular reprogramming is available at safe concentrations. They do not benefit from the research ecosystem and have no voice in resource allocation decisions.
narrative_ontology:constraint_stakeholder(reprogramming_safety_toxicity, patient_population, payer,
    powerless, immediate, trapped, universal).

% Pharmaceutical companies capture research funding, patent portfolios, and regulatory consulting revenue during the search for safe chemical reprogramming protocols. They can arbitrage across therapeutic modalities (gene therapy, cell therapy, small molecules) and across disease indications, but they cannot arbitrage around the fundamental dose-toxicity relationship. They benefit from the constraint through sustained research investment and intellectual property accumulation, even though no commercially viable product has emerged. Exit is easy — they can pivot to other therapeutic areas — but while engaged, they collect resources.
narrative_ontology:constraint_stakeholder(reprogramming_safety_toxicity, pharmaceutical_companies, beneficiary,
    institutional, biographical, arbitrage, global).

% Academic researchers capture grant funding, publications, and career advancement by working on the chemical reprogramming problem. They are constrained by funding cycles and publication pressure but benefit from the research ecosystem regardless of whether safe protocols are discovered. The constraint sustains a research community: incremental optimization studies, toxicology assessments, and delivery mechanism explorations generate publications even when the fundamental therapeutic window problem remains unsolved. Exit is costly (career pivot, lab retooling) but possible.
narrative_ontology:constraint_stakeholder(reprogramming_safety_toxicity, academic_research_groups, beneficiary,
    institutional, generational, constrained, national).

% Regulatory agencies (FDA, EMA, PMDA) benefit from the constraint through institutional authority and toxicology expertise. They do not enforce the dose-toxicity relationship — it enforces itself through adverse events — but they control the approval pathways and set the safety thresholds that any chemical reprogramming therapy must meet. The constraint justifies their role and resources: complex toxicology assessment, risk-benefit analysis, and post-market surveillance. They are constrained by statutory mandate and political oversight but benefit from the expertise gap between their toxicology infrastructure and the research community's capacity to generate safety data.
narrative_ontology:constraint_stakeholder(reprogramming_safety_toxicity, regulatory_agencies, beneficiary,
    institutional, civilizational, constrained, global).

% Researchers working on non-chemical reprogramming modalities (optogenetics, mechanotransduction, direct cell-cell fusion, viral transdifferentiation) are systematically under-resourced relative to chemical approaches. They would object that the dose-toxicity constraint is treated as immutable when alternative pathways exist, but they are not in the resource allocation conversation. Funding agencies, pharmaceutical companies, and regulatory frameworks are optimized for small-molecule drugs, creating structural barriers for alternative modalities. They are excluded not by explicit suppression but by institutional path dependence.
narrative_ontology:constraint_stakeholder(reprogramming_safety_toxicity, alternative_modality_researchers, excluded,
    moderate, generational, constrained, global).

% The analytical observer sees the dose-toxicity relationship as a structural feature of mammalian biochemistry: chemicals that drive epigenetic reprogramming at the required concentrations disrupt normal cellular function. This appears as a natural law — it would persist in any institutional arrangement. However, the observer must consider whether the beneficiary structure (pharmaceutical companies, academic groups, regulatory agencies all capturing resources) and the systematic under-resourcing of alternative modalities indicate that the 'natural law' framing naturalizes institutional choices. The observer's task is to distinguish genuine natural limits from false summits.
narrative_ontology:constraint_stakeholder(reprogramming_safety_toxicity, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reprogramming_safety_toxicity, diffuse).
narrative_ontology:fixing_cost_class(reprogramming_safety_toxicity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint coordinates research investment in cellular reprogramming by establishing a shared understanding of the dose-toxicity barrier. Researchers, funders, and regulators agree on the problem (toxic concentrations required for reprogramming) and the solution space (delivery mechanisms, prodrug strategies, tissue-specific targeting, or alternative modalities). This coordination is genuine — without shared recognition of the constraint, resources would be wasted on approaches that ignore the therapeutic window problem.
% TRANSFER_FUNCTION: The constraint transfers research funding, patent portfolios, and regulatory authority from the patient population (who bear the cost of foregone therapeutic benefit) to pharmaceutical companies, academic research groups, and regulatory agencies (who capture resources during the search for safe protocols). The transfer is not direct extraction — patients do not pay pharmaceutical companies for failed reprogramming therapies — but indirect: public and private research funding flows to actors with expertise in navigating the constraint, and patients remain without access to cellular reprogramming therapies.
% ABSENT_VOICES: Alternative modality researchers (optogenetics, mechanotransduction, viral transdifferentiation) are systematically excluded from resource allocation decisions. They would object that the chemical reprogramming approach is over-resourced relative to its prospects and that institutional path dependence (pharmaceutical industry expertise in small molecules, regulatory frameworks optimized for drugs, academic training pipelines focused on medicinal chemistry) naturalizes the dose-toxicity constraint when alternative pathways exist. They are not in the room when funding priorities are set or when regulatory frameworks are designed.
% DISAPPEARANCE_RATIONALE: If the dose-toxicity constraint disappeared overnight (i.e., if safe systemic dosing of chemical reprogramming agents became possible), the world would rearrange substantially: cellular reprogramming therapies would become viable, pharmaceutical companies would shift from research to commercialization, regulatory agencies would shift from toxicology assessment to post-market surveillance, and patients would gain access to regenerative medicine. However, if the constraint is a genuine natural law (an immutable feature of mammalian biochemistry), it cannot disappear — the question is incoherent. The verdict is contested because the constraint's status is contested: if it is a natural law, the world cannot rearrange because the constraint cannot disappear; if it is partly constructed (naturalized by institutional path dependence), the world would rearrange if the construction were dismantled.
% FOUNDING_PROBLEM: The founding problem is cellular reprogramming for therapeutic use: converting differentiated cells into pluripotent or alternative differentiated states to treat degenerative diseases, tissue damage, or genetic disorders. Chemical reprogramming was proposed as a non-genetic alternative to viral reprogramming (which carries oncogenic risk) and direct cell therapy (which requires invasive tissue harvest). The problem is live — patients still need regenerative therapies — but the chemical approach has not solved it due to the dose-toxicity constraint.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (need for safe cellular reprogramming therapies) is corroborated by patient advocacy groups, independent health economists, and WHO disease burden assessments — all outside the beneficiary set (pharmaceutical companies, academic researchers, regulatory agencies). The problem is real and the need is urgent. However, the choice to pursue chemical reprogramming as the primary solution pathway is corroborated primarily by actors within the beneficiary set, with limited input from alternative modality researchers or health technology assessment bodies that might prioritize non-chemical approaches.
narrative_ontology:disappearance_verdict(reprogramming_safety_toxicity, contested).
narrative_ontology:founding_problem_status(reprogramming_safety_toxicity, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PATIENT POPULATION (MOUNTAIN) — Cannot exit the dose-toxicity relationship; no alternative pathway to cellular reprogramming exists at safe systemic concentrations. The constraint appears as an immutable biological limit: chemicals that reprogram cells at the concentrations required are toxic to the organism. No amount of advocacy or resource mobilization changes the dose-response curve.
constraint_indexing:constraint_classification(reprogramming_safety_toxicity, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: PHARMACEUTICAL INDUSTRY (MOUNTAIN) — Despite beneficiary status (captures research funding, patent portfolios, and regulatory expertise during the search for safe reprogramming protocols), experiences the constraint as immutable. Can arbitrage across therapeutic modalities (gene therapy, cell therapy, small molecules) but cannot arbitrage around the fundamental dose-toxicity relationship. The constraint is a natural law that shapes the entire competitive landscape equally.
constraint_indexing:constraint_classification(reprogramming_safety_toxicity, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ACADEMIC RESEARCH GROUP (MOUNTAIN) — Constrained by funding cycles and publication pressure but experiences the dose-toxicity relationship as a natural limit, not an institutional barrier. Can explore alternative chemical scaffolds, delivery mechanisms, or target pathways, but the underlying pharmacological constraint persists across all approaches. Benefits from the research ecosystem (grants, citations, career advancement) but does not experience the constraint itself as constructed.
constraint_indexing:constraint_classification(reprogramming_safety_toxicity, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY AGENCY (MOUNTAIN) — Constrained by statutory mandate and political oversight but experiences the dose-toxicity relationship as a natural fact that regulatory frameworks must accommodate, not create. Benefits from the constraint through institutional authority (expertise in toxicology assessment, control over approval pathways) but does not enforce the constraint — the constraint enforces itself through adverse events. Regulatory thresholds are responses to the natural limit, not sources of it.
constraint_indexing:constraint_classification(reprogramming_safety_toxicity, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — The dose-toxicity relationship for chemical reprogramming agents is a structural feature of mammalian biochemistry: the concentrations required to drive epigenetic reprogramming in differentiated cells overlap with concentrations that disrupt normal cellular function, trigger apoptosis, or induce oncogenic transformation. This is not a regulatory artifact, a funding gap, or an institutional barrier — it is a constraint imposed by the chemistry of chromatin remodeling and the narrow therapeutic windows of small molecules that interfere with fundamental cellular processes. The constraint would persist in any institutional arrangement.
constraint_indexing:constraint_classification(reprogramming_safety_toxicity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reprogramming_safety_toxicity_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(reprogramming_safety_toxicity, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reprogramming_safety_toxicity, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(reprogramming_safety_toxicity, ExtMetricName, E),
    domain_priors:suppression_score(reprogramming_safety_toxicity, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(reprogramming_safety_toxicity),
    narrative_ontology:constraint_metric(reprogramming_safety_toxicity, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(reprogramming_safety_toxicity, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(reprogramming_safety_toxicity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): Low but non-zero. The constraint extracts modest rents through research funding concentration (pharmaceutical companies and academic groups capture resources during the search for safe protocols), patent accumulation (intellectual property on failed or marginally effective compounds), and regulatory consulting fees (toxicology assessment expertise). However, the extraction is substantially lower than typical Snare or Tangled Rope constraints because the constraint does not primarily function as a rent-extraction mechanism — it functions as a genuine biochemical limit that incidentally benefits actors with expertise in navigating it. The modest upward drift (0.10 → 0.15 over the interval) reflects increasing concentration of research funding in established pharmaceutical and academic players as the field matures. Suppression (0.20): Low. The constraint does not require active enforcement — it enforces itself through adverse events in animal models and would-be human trials. The suppression that exists is structural (high capital requirements for toxicology studies, regulatory barriers to human trials) rather than coercive. The modest upward drift (0.15 → 0.20) reflects increasing regulatory stringency as early reprogramming attempts revealed unexpected toxicities. Theater ratio (0.25): Low but rising. Most toxicology assessment is functional — dose-response curves, adverse event monitoring, and histopathology are genuine measurements of biological harm. However, some theater has accumulated: incremental optimization studies that test minor chemical modifications without addressing the fundamental therapeutic window problem, regulatory submissions that repackage known toxicities as 'acceptable risks' without new safety data, and academic publications that frame failed reprogramming attempts as 'proof of concept' studies. The upward drift (0.15 → 0.25) reflects increasing performative activity as the field matures without solving the core problem. Accessibility collapse (0.88): Very high. Once the dose-toxicity relationship is understood, alternative approaches (gene therapy, cell therapy, tissue engineering) become the only viable pathways to cellular reprogramming for therapeutic use. The chemical reprogramming approach does not collapse entirely (research continues on delivery mechanisms, prodrug strategies, and tissue-specific targeting), but the therapeutic window problem forecloses the most direct path (systemic administration of reprogramming cocktails). Resistance (0.12): Very low. The constraint meets almost no resistance because it is perceived as a natural law. Researchers do not protest the dose-toxicity relationship; they accept it and search for workarounds. The minimal resistance that exists comes from advocates for alternative modalities (optogenetics, mechanotransduction) who argue that the chemical approach is over-resourced relative to its prospects, but this is a resource allocation dispute, not resistance to the constraint itself.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits no perspectival gap in classification — all five perspectives classify as Mountain — but exhibits a critical gap in interpretation: is the Mountain classification correct (genuine natural law) or is it a false summit (naturalized institutional arrangement)? The patient population, pharmaceutical industry, academic researchers, and regulatory agencies all experience the constraint as immutable, but their structural positions differ dramatically. Patients are trapped with no exit and no benefit. Pharmaceutical companies and academic groups are beneficiaries who capture resources during the search for safe protocols. Regulatory agencies are constrained institutional actors who benefit from toxicology expertise. The analytical observer sees a structural feature of mammalian biochemistry but must consider whether the beneficiary structure and the systematic under-resourcing of alternative modalities indicate that the 'natural law' framing naturalizes institutional path dependence. The false summit detector fires because the constraint has declared beneficiaries despite claiming Mountain status — the omega variables document the unresolved question.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Patients are victims (no entry in beneficiaries array) with trapped exit → high d → high effective extraction, but the constraint is classified as Mountain from their perspective because accessibility collapse is very high and resistance is very low (they perceive no alternative). Pharmaceutical companies, academic groups, and regulatory agencies are beneficiaries with arbitrage or constrained exit → low d → low or negative effective extraction, and they classify as Mountain because they experience the constraint as a natural limit that shapes the competitive landscape equally. The analytical observer has analytical exit and sees the constraint as universal in scope, classifying as Mountain. The directionality derivation produces the expected pattern: beneficiaries experience low extraction, victims experience high extraction, but all perspectives agree on the classification because the constraint appears immutable regardless of structural position. The critical question is whether this unanimity reflects genuine natural law or successful naturalization of a contingent arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that a Mountain classification can coexist with identifiable beneficiaries without contradiction — IF the beneficiaries are incidental (they benefit from navigating an immutable limit, not from maintaining it). The mandate is 'solve the cellular reprogramming problem safely'; the constraint is 'chemical reprogramming requires toxic concentrations'. If the constraint is a genuine natural law, the beneficiaries are simply the actors best positioned to search for workarounds (pharmaceutical companies with medicinal chemistry expertise, academic groups with toxicology infrastructure, regulatory agencies with safety assessment frameworks). Their benefit is a side effect of their expertise, not the purpose of the constraint. However, if the constraint is partly constructed — if funding structures, regulatory pathways, and intellectual property regimes systematically favor chemical approaches over alternative modalities that might bypass the dose-toxicity relationship — then the beneficiaries are not incidental, and the Mountain classification is a false summit. The omega variables document this irreducible uncertainty. The mandatrophy is resolved not by choosing between 'natural law' and 'constructed constraint' but by making the choice explicit and measurable: the false summit detector fires, the omega variables specify what evidence would resolve the ambiguity, and the temporal measurements track whether extraction is accumulating (which would indicate construction) or remaining flat (which would indicate natural law).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    false_summit_beneficiary_structure,
    'Is the dose-toxicity constraint a genuine natural law, or does the presence of identifiable beneficiaries (pharmaceutical companies capturing research funding, academic groups capturing grants and citations, regulatory agencies capturing institutional authority) indicate that the constraint is partly constructed through institutional arrangements that benefit from its persistence?',
    'Historical analysis of research funding allocation: does funding disproportionately flow to incremental optimization of existing chemical scaffolds rather than to alternative reprogramming modalities (mechanical, electrical, or biological) that might bypass the dose-toxicity relationship? Comparative analysis of regulatory frameworks: do approval pathways favor pharmaceutical solutions over non-chemical interventions? If funding and regulatory structures systematically channel resources toward approaches that preserve the constraint, the ''natural law'' framing may naturalize institutional choices.',
    'If the constraint is genuinely natural: Mountain classification is correct across all perspectives, and the beneficiary structure is incidental (they benefit from navigating an immutable limit, not from maintaining it). If the constraint is partly constructed: the analytical perspective is a false summit, and the constraint should reclassify as Tangled Rope (genuine coordination problem in cellular reprogramming research, but with asymmetric extraction favoring actors whose expertise and business models depend on the constraint''s persistence).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_beneficiary_structure, empirical, 'Whether beneficiary structure indicates false summit or incidental benefit from natural law').

omega_variable(
    alternative_modality_suppression,
    'Are non-chemical reprogramming modalities (optogenetic, mechanotransduction, direct cell-cell fusion, or in vivo transdifferentiation via viral vectors) systematically under-resourced or under-explored relative to their technical promise, and if so, does this suppression arise from natural technical barriers or from institutional path dependence?',
    'Funding allocation analysis: compare investment in chemical vs non-chemical reprogramming approaches relative to their respective proof-of-concept success rates. Patent landscape analysis: identify whether intellectual property structures favor chemical approaches. Regulatory pathway analysis: assess whether approval frameworks are optimized for small-molecule drugs in ways that create barriers for alternative modalities.',
    'If alternative modalities are under-resourced due to natural technical barriers (e.g., optogenetic approaches require genetic modification, mechanotransduction lacks scalable delivery mechanisms): the Mountain classification holds. If under-resourcing reflects institutional path dependence (pharmaceutical industry expertise in small molecules, regulatory frameworks designed for drugs, academic training pipelines focused on medicinal chemistry): the constraint''s ''naturalness'' is overstated, and suppression is higher than the base metric (0.20) suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_modality_suppression, empirical, 'Whether alternative reprogramming modalities are suppressed by natural barriers or institutional path dependence').

omega_variable(
    therapeutic_window_discovery_timeline,
    'What is the expected timeline for discovering chemical reprogramming agents with acceptable therapeutic windows, and does this timeline reflect intrinsic chemical constraints or the current state of medicinal chemistry and delivery technology?',
    'Retrospective analysis of other ''impossible'' therapeutic windows that were eventually solved (e.g., kinase inhibitors, immunosuppressants, antiretrovirals): how long did it take, and what breakthroughs were required? Prospective modeling of reprogramming agent chemical space: are there unexplored scaffolds or delivery mechanisms (nanoparticle encapsulation, prodrug strategies, tissue-specific targeting) that could widen the therapeutic window?',
    'If the timeline is measured in decades and depends on breakthroughs not yet visible: Mountain classification is robust. If the timeline is measured in years and depends on applying existing medicinal chemistry techniques to reprogramming agents: the constraint is more contingent than the Mountain framing suggests, and the resistance metric (0.12) may be too low — the constraint is not meeting resistance because it is perceived as immutable, but that perception may be premature.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(therapeutic_window_discovery_timeline, empirical, 'Whether therapeutic window discovery timeline reflects intrinsic limits or current technology state').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reprogramming_safety_toxicity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reprogram_tox_theater_t0, reprogramming_safety_toxicity, theater_ratio, 0, 0.15).
narrative_ontology:measurement(reprogram_tox_theater_t3, reprogramming_safety_toxicity, theater_ratio, 3, 0.18).
narrative_ontology:measurement(reprogram_tox_theater_t6, reprogramming_safety_toxicity, theater_ratio, 6, 0.22).
narrative_ontology:measurement(reprogram_tox_theater_t10, reprogramming_safety_toxicity, theater_ratio, 10, 0.25).

% Extraction over time
narrative_ontology:measurement(reprogram_tox_extract_t0, reprogramming_safety_toxicity, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(reprogram_tox_extract_t3, reprogramming_safety_toxicity, base_extractiveness, 3, 0.12).
narrative_ontology:measurement(reprogram_tox_extract_t6, reprogramming_safety_toxicity, base_extractiveness, 6, 0.13).
narrative_ontology:measurement(reprogram_tox_extract_t10, reprogramming_safety_toxicity, base_extractiveness, 10, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(reprogram_tox_suppress_t0, reprogramming_safety_toxicity, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(reprogram_tox_suppress_t3, reprogramming_safety_toxicity, suppression_requirement, 3, 0.17).
narrative_ontology:measurement(reprogram_tox_suppress_t6, reprogramming_safety_toxicity, suppression_requirement, 6, 0.19).
narrative_ontology:measurement(reprogram_tox_suppress_t10, reprogramming_safety_toxicity, suppression_requirement, 10, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reprogramming_safety_toxicity, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is a candidate for decomposition if alternative reprogramming modalities (optogenetic, mechanotransduction, viral transdifferentiation) are found to have systematically different dose-toxicity profiles. Each modality would then be a separate constraint story with its own extractiveness value. Currently modeled as a single constraint because all chemical reprogramming approaches share the same fundamental dose-toxicity relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
