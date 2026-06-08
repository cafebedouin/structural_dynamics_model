% ============================================================================
% CONSTRAINT STORY: delivery_modality_toxicity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_delivery_modality_toxicity, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: delivery_modality_toxicity
 *   human_readable: Delivery Modality Toxicity in Adenine Base Editing
 *   domain: biomedical_research/genome_editing/reproductive_medicine
 *
 * SUMMARY:
 *   The delivery modality toxicity constraint in adenine base editing
 *   represents a sharp technical boundary in genome editing: the same editor
 *   protein (ABE) enables normal embryo development when delivered as
 *   ribonucleoprotein (RNP) but causes invariable early arrest when delivered
 *   as mRNA. This is not a gradual dose-response or a statistical tendency —
 *   it is a binary outcome (32-33% blastocyst rate for RNP vs 0/19 embryos
 *   for mRNA). The constraint solves a genuine coordination problem: research
 *   groups need to know which delivery modality works to conduct successful
 *   experiments and advance toward clinical applications. The structural
 *   delta is striking because it isolates delivery modality as the causal
 *   variable — the editor identity, the target sequence, and the experimental
 *   protocol are held constant. The only difference is the molecular form of
 *   the editor when it enters the cell. This constraint is classified as Rope
 *   from all non-analytical perspectives because it exhibits the canonical
 *   coordination structure: a genuine collective-action problem (how to edit
 *   embryos without toxicity) with a solution (RNP delivery) that benefits
 *   participants and does not suppress alternatives. The analytical
 *   perspective classifies it as Mountain because the embryo's differential
 *   response to mRNA vs protein appears to be a biological property of early
 *   development, not an institutional arrangement.
 *
 * KEY AGENTS:
 *   - RNP Delivery Research Groups: Primary beneficiaries (institutional/mobile) — discovered the constraint and benefit from priority in viable clinical pathways
 *   - Independent Replication Labs: Secondary beneficiaries (moderate/constrained) — benefit from knowing the constraint, preventing wasted effort on mRNA delivery
 *   - Clinical Translation Consortia: Organized beneficiaries (organized/mobile) — use the constraint as a design specification for safe therapeutic development
 *   - Pharmaceutical Development Programs: Powerful beneficiaries (powerful/arbitrage) — incorporate the constraint as a technical requirement in R&D investment decisions
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the constraint as a biological property of mammalian embryos
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(delivery_modality_toxicity, 0.12).
domain_priors:suppression_score(delivery_modality_toxicity, 0.18).
domain_priors:theater_ratio(delivery_modality_toxicity, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(delivery_modality_toxicity, extractiveness, 0.12).
narrative_ontology:constraint_metric(delivery_modality_toxicity, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(delivery_modality_toxicity, theater_ratio, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(delivery_modality_toxicity, rope).
narrative_ontology:human_readable(delivery_modality_toxicity, "Delivery Modality Toxicity in Adenine Base Editing").
narrative_ontology:topic_domain(delivery_modality_toxicity, "biomedical_research/genome_editing/reproductive_medicine").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(delivery_modality_toxicity, rnp_delivery_research_groups).
narrative_ontology:constraint_beneficiary(delivery_modality_toxicity, embryo_viability_optimization_programs).
narrative_ontology:constraint_beneficiary(delivery_modality_toxicity, clinical_translation_pathways).
narrative_ontology:constraint_vindicates(delivery_modality_toxicity, rna_sensing_pathway_hypothesis).
narrative_ontology:constraint_vindicates(delivery_modality_toxicity, delivery_modality_primacy_over_editor_identity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RNP DELIVERY RESEARCH GROUPS (ROPE) — Primary beneficiaries who discovered the delivery modality constraint. Experience this as pure coordination: the constraint solves the genuine problem of enabling base editing in embryos without toxicity. Mobile exit (can switch to other delivery methods or editor types) and institutional power mean low effective extraction. The constraint coordinates research effort toward viable clinical pathways.
constraint_indexing:constraint_classification(delivery_modality_toxicity, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: INDEPENDENT REPLICATION LABS (ROPE) — Moderate power, constrained exit (invested in base editing infrastructure but can pivot to other genome editing modalities). Experience the constraint as coordination: the delivery modality requirement is a genuine technical boundary that, once known, enables successful experiments. Constrained exit raises effective extraction slightly above institutional/mobile agents, but the coordination function dominates — knowing the constraint prevents wasted effort.
constraint_indexing:constraint_classification(delivery_modality_toxicity, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CLINICAL TRANSLATION CONSORTIA (ROPE) — Organized agents (multi-institution partnerships, regulatory working groups) with generational time horizon and mobile exit (can pursue alternative therapeutic modalities). See the constraint as coordination: the delivery modality requirement is a design constraint that, once satisfied, enables safe clinical translation. Low effective extraction because the constraint is not suppressing alternatives — it is revealing which technical path works.
constraint_indexing:constraint_classification(delivery_modality_toxicity, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: PHARMACEUTICAL DEVELOPMENT PROGRAMS (ROPE) — Powerful agents with arbitrage-level exit (can invest in multiple therapeutic platforms simultaneously) and immediate time horizon (quarterly decision cycles). Experience minimal effective extraction: the constraint is a technical specification that guides R&D investment toward viable products. The delivery modality requirement is not extractive — it is informative.
constraint_indexing:constraint_classification(delivery_modality_toxicity, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal perspective, the delivery modality toxicity appears as a biological constraint: mammalian embryos have an RNA-sensing pathway that responds to exogenous mRNA but not to protein. This is a property of the developmental biology, not an institutional arrangement. The constraint would persist regardless of who studies it or whether anyone enforces it. Zero degrees of freedom for all indices — the embryo's response to mRNA vs RNP is not negotiable.
constraint_indexing:constraint_classification(delivery_modality_toxicity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(delivery_modality_toxicity_tests).
:- end_tests(delivery_modality_toxicity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Low. The constraint has minimal extractive overhead. The original discovery group benefits from priority and citation advantage, but the benefit is proportional to the genuine technical contribution — they solved a real problem. Other groups can immediately adopt RNP delivery without licensing barriers, proprietary reagents, or institutional gatekeeping. The extractiveness reflects only the natural first-mover advantage in a competitive research environment, not structural rent-seeking. Suppression (0.18): Low. The constraint does not suppress alternatives — it reveals which alternative works. Groups are free to pursue mRNA delivery if they believe they can solve the toxicity problem, or to abandon base editing entirely for other genome editing modalities (CRISPR-Cas9, prime editing, homology-directed repair). The suppression value reflects only the resource investment required to switch modalities, not active enforcement. Theater ratio (0.08): Very low. The constraint is directly testable: inject embryos with mRNA or RNP, count blastocysts. There is minimal performative content — the developmental outcome is the verification mechanism. The small theater component reflects only the standard scientific communication overhead (manuscript preparation, peer review) that applies to all experimental findings. The theater ratio has decreased over the interval as replication data accumulated, reducing uncertainty about the constraint's reality.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is minimal for this constraint because all agents with structural relationships to it (research groups, clinical programs, pharmaceutical developers) experience it as coordination. The only divergence is the analytical perspective's mountain classification, which reflects a genuine structural difference: from a civilizational/universal view, the embryo's response to mRNA vs RNP is a biological property that would persist regardless of institutional arrangements. This is not a false summit — there are no identifiable beneficiaries who collect rents from the constraint's operation. The constraint is not enforced; it is discovered. The mountain classification is warranted because the constraint exhibits the natural law profile: it emerges from the biology, not from institutional design; it would persist even if all current research groups disbanded; and it meets negligible resistance because it is not contested (groups adopt RNP delivery once they learn it works). The gap between rope (from institutional perspectives) and mountain (from analytical perspective) reflects the difference between experiencing a constraint as a coordination solution and recognizing it as a biological limit.
 *
 * DIRECTIONALITY LOGIC:
 *   All non-analytical perspectives are beneficiaries with mobile or constrained exit options, producing low directionality values (d near 0.0-0.3) and correspondingly low or negative effective extraction (chi). The RNP delivery research groups are institutional beneficiaries with mobile exit — they can pivot to other editors or delivery methods if needed — producing d near 0.0 (full beneficiary). Independent replication labs are moderate-power beneficiaries with constrained exit (invested in base editing infrastructure) — producing d near 0.2 (mostly beneficiary, slight extraction from switching costs). Clinical translation consortia are organized beneficiaries with mobile exit and generational time horizon — producing d near 0.1 (strong beneficiary). Pharmaceutical programs are powerful beneficiaries with arbitrage exit — producing d near 0.0 (full beneficiary). The analytical perspective is not a beneficiary or victim in the structural sense — it observes the constraint as a biological property — producing d = 0.5 (symmetric) by default, but the mountain classification derives from the natural law profile (emerges_naturally would be true if this were claimed as mountain), not from extraction metrics.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that rope and mountain classifications can both be structurally valid for the same phenomenon, viewed from different indices. The research groups experience coordination (rope) because the constraint solves their problem — knowing which delivery modality works enables successful experiments. The analytical observer recognizes a biological limit (mountain) because the embryo's differential response to mRNA vs protein is not an institutional arrangement. Both classifications are correct from their respective perspectives. The constraint is not a false summit because no one collects rents from its operation — the original discovery group benefits from priority, but this is proportional to their genuine contribution, not structural extraction. The constraint is not a snare because it does not suppress alternatives — groups are free to pursue other genome editing modalities. The constraint is not a tangled rope because there are no identifiable victims — all agents with structural relationships to the constraint benefit from knowing it exists. The mandatrophy is resolved by recognizing that the indexical tuple determines classification: institutional agents with coordination problems see rope; analytical observers with civilizational time horizons see mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rna_sensing_mechanism_identity,
    'What is the molecular identity of the RNA-sensing pathway that causes mRNA-delivered ABE toxicity?',
    'Systematic knockout screens of candidate innate immune sensors (RIG-I, MDA5, TLR3, PKR, OAS); rescue experiments with pathway inhibitors; transcriptomic profiling of arrested embryos',
    'If pathway is identified and can be transiently inhibited: mRNA delivery becomes viable, expanding the coordination solution space. If pathway is essential for early development: RNP delivery remains the only viable modality, confirming the constraint as a hard biological limit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rna_sensing_mechanism_identity, empirical, 'Identity of the RNA-sensing pathway causing mRNA toxicity').

omega_variable(
    species_generalizability,
    'Does the delivery modality toxicity generalize across mammalian species, or is it specific to the tested model (likely mouse or human)?',
    'Cross-species replication: test mRNA vs RNP delivery of ABE in bovine, porcine, primate embryos; compare developmental arrest phenotypes and transcriptomic signatures',
    'If species-specific: the constraint is a contingent feature of one developmental program, not a universal mammalian property. If universal: the constraint is a conserved feature of early mammalian development, strengthening the mountain classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(species_generalizability, empirical, 'Whether delivery modality toxicity is species-specific or universal').

omega_variable(
    editor_generalizability,
    'Does the delivery modality toxicity apply to all base editors (CBE, ABE variants, prime editors) or only to the specific ABE variant tested?',
    'Systematic comparison: deliver multiple base editor architectures (TadA8e, TadA9, ABE8e, CBE4max, PE2, PE3) as both mRNA and RNP; measure blastocyst rates and arrest phenotypes',
    'If editor-specific: the constraint is a property of one protein''s mRNA sequence (codon usage, secondary structure, UTR elements), not a general delivery modality effect. If editor-general: the constraint is a property of the delivery modality itself, confirming the structural hypothesis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(editor_generalizability, empirical, 'Whether toxicity is specific to one ABE variant or general to all base editors').

omega_variable(
    dose_response_threshold,
    'Is there a dose threshold below which mRNA delivery is tolerated, or is the toxicity binary (any mRNA causes arrest)?',
    'Dose-response curve: inject embryos with serial dilutions of ABE mRNA (1 pg to 1000 pg); measure developmental outcomes and identify minimum toxic dose',
    'If threshold exists: low-dose mRNA delivery may be viable for some applications, expanding the solution space. If binary: the constraint is all-or-nothing, confirming that RNP is the only viable modality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dose_response_threshold, empirical, 'Whether mRNA toxicity has a dose threshold or is binary').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(delivery_modality_toxicity, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dmt_theater_initial, delivery_modality_toxicity, theater_ratio, 0, 0.15).
narrative_ontology:measurement(dmt_theater_mid, delivery_modality_toxicity, theater_ratio, 2, 0.12).
narrative_ontology:measurement(dmt_theater_current, delivery_modality_toxicity, theater_ratio, 5, 0.08).

% Extraction over time
narrative_ontology:measurement(dmt_extract_initial, delivery_modality_toxicity, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(dmt_extract_mid, delivery_modality_toxicity, base_extractiveness, 2, 0.15).
narrative_ontology:measurement(dmt_extract_current, delivery_modality_toxicity, base_extractiveness, 5, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(delivery_modality_toxicity, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is structurally independent — it is not downstream of other genome editing constraints and does not have sibling constraints with different epsilon values. The delivery modality toxicity is a single, stable phenomenon with one epsilon value regardless of how it is measured (blastocyst rate, cell count, developmental stage). If future work identifies multiple distinct RNA-sensing pathways with different toxicity profiles, those would be separate constraints requiring decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
