% ============================================================================
% CONSTRAINT STORY: dna_repair_substrate_difference
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dna_repair_substrate_difference, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dna_repair_substrate_difference
 *   human_readable: DNA Repair Substrate Difference in Human Embryos
 *   domain: biomedical_research/genome_editing/reproductive_medicine
 *
 * SUMMARY:
 *   The substrate-specific DNA repair competence in human embryos is a
 *   biological constraint discovered through genome editing experiments.
 *   Human embryos efficiently repair single-strand breaks and mismatches
 *   (induced by adenine base editors) but catastrophically fail to repair
 *   double-strand breaks (induced by Cas9 nuclease), producing large
 *   deletions, chromosomal rearrangements, and segmental aneuploidy. This
 *   differential repair competence reflects the developmental maturation
 *   schedule of DNA repair pathways: mismatch repair and base excision repair
 *   are active in early embryos, but homologous recombination and
 *   non-homologous end joining are not yet functional. The constraint is
 *   measured via SNP arrays (detecting segmental aneuploidy), long-range PCR
 *   (detecting large deletions), and karyotyping (detecting chromosomal
 *   rearrangements). The constraint appears to be a genuine biological limit
 *   — it is reproducible across labs, conserved across mammalian species, and
 *   independent of experimental protocol. However, identifiable actors
 *   benefit from the constraint's operation: base editing research programs
 *   gain a safety advantage over Cas9-based approaches, and fertility clinics
 *   can offer ABE services as a safer alternative. This creates a false
 *   summit candidate: the constraint is presented as natural law, but
 *   beneficiaries exist. The omega variables address whether the 'natural
 *   law' framing is accurate or whether it naturalizes a contingent state of
 *   affairs.
 *
 * KEY AGENTS:
 *   - The Edited Embryo: Primary subject (powerless/trapped) — cannot choose its repair machinery; experiences the constraint as an immutable developmental limit
 *   - The Clinical Embryologist: Practitioner (moderate/constrained) — must work within the biological constraint; can choose editing tools but cannot change repair competence
 *   - The Fertility Clinic: Service provider (institutional/arbitrage) — benefits from offering ABE as a safer alternative; does not create the constraint but profits from its existence
 *   - The Base Editing Research Program: Technology developer (institutional/mobile) — benefits substantially from the substrate difference (their tool is safer); did not create the constraint but gains competitive advantage from its discovery
 *   - The Analytical Observer: Civilizational perspective (analytical/analytical) — evaluates whether the constraint is a genuine natural law or a false summit (naturalized contingent arrangement)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dna_repair_substrate_difference, 0.08).
domain_priors:suppression_score(dna_repair_substrate_difference, 0.02).
domain_priors:theater_ratio(dna_repair_substrate_difference, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dna_repair_substrate_difference, extractiveness, 0.08).
narrative_ontology:constraint_metric(dna_repair_substrate_difference, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(dna_repair_substrate_difference, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dna_repair_substrate_difference, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(dna_repair_substrate_difference, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dna_repair_substrate_difference, mountain).
narrative_ontology:human_readable(dna_repair_substrate_difference, "DNA Repair Substrate Difference in Human Embryos").
narrative_ontology:topic_domain(dna_repair_substrate_difference, "biomedical_research/genome_editing/reproductive_medicine").

domain_priors:emerges_naturally(dna_repair_substrate_difference).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dna_repair_substrate_difference, base_editing_research_programs).
narrative_ontology:constraint_beneficiary(dna_repair_substrate_difference, fertility_clinics_offering_abe_services).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE EDITED EMBRYO (MOUNTAIN) — The embryo cannot choose its repair machinery. The substrate-specific repair competence is a fixed biological property of early human development. Single-strand breaks are repaired efficiently because the mismatch repair and base excision repair pathways are active; double-strand breaks trigger catastrophic failure because homologous recombination and non-homologous end joining are not yet mature. This is not a policy choice or an institutional arrangement — it is a developmental constraint that would persist regardless of who attempts genome editing or what regulatory framework governs the attempt.
constraint_indexing:constraint_classification(dna_repair_substrate_difference, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE CLINICAL EMBRYOLOGIST (MOUNTAIN) — The embryologist faces a fixed biological constraint. They can choose which editing tool to use (ABE vs Cas9) but cannot change the embryo's repair competence. The substrate difference is discovered through empirical observation (SNP arrays showing segmental aneuploidy after Cas9 but not after ABE) and is reproducible across labs, species, and experimental conditions. The constraint is changeable only at civilizational timescales (if synthetic biology eventually produces artificial repair pathways) but is immutable at biographical timescales.
constraint_indexing:constraint_classification(dna_repair_substrate_difference, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: THE FERTILITY CLINIC (MOUNTAIN) — The clinic benefits from the substrate difference (can offer base editing as a safer alternative to Cas9) but does not create or enforce the constraint. The repair competence difference exists independently of the clinic's business model. The clinic has arbitrage-level exit (can choose not to offer editing services) but this does not change the underlying biological constraint. The clinic's beneficiary status makes this a false summit candidate — the constraint appears as natural law but identifiable actors benefit from its operation.
constraint_indexing:constraint_classification(dna_repair_substrate_difference, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE BASE EDITING RESEARCH PROGRAM (MOUNTAIN) — Research programs developing ABE technology benefit substantially from the substrate difference (their tool is safer than the alternative) but do not create the constraint. The repair competence difference was discovered, not invented. The research program has mobile exit options (can pivot to other gene therapy applications) and substantial power (can shape research priorities and funding allocation) but cannot change the embryonic repair machinery. Beneficiary status with high power and mobility — classic false summit profile.
constraint_indexing:constraint_classification(dna_repair_substrate_difference, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: THE ANALYTICAL OBSERVER (MOUNTAIN) — From the analytical perspective, the substrate-specific repair competence is a genuine biological constraint. The embryo's repair pathways mature on a developmental schedule that is conserved across mammals and is not contingent on human institutions. The constraint is falsifiable (could be refuted by finding embryos that repair DSBs efficiently) and has been tested across multiple labs and species. However, the presence of identifiable beneficiaries (base editing programs, fertility clinics) requires investigation: is the 'natural law' framing naturalizing a contingent institutional arrangement, or is the beneficiary structure genuinely incidental to an immutable biological constraint?
constraint_indexing:constraint_classification(dna_repair_substrate_difference, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dna_repair_substrate_difference_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(dna_repair_substrate_difference, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dna_repair_substrate_difference, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(dna_repair_substrate_difference, ExtMetricName, E),
    domain_priors:suppression_score(dna_repair_substrate_difference, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(dna_repair_substrate_difference),
    narrative_ontology:constraint_metric(dna_repair_substrate_difference, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(dna_repair_substrate_difference, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(dna_repair_substrate_difference_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The constraint is a biological limit, not an extraction mechanism. The modest extractiveness reflects that base editing research programs and fertility clinics benefit from the constraint's existence (they can market ABE as safer than Cas9), but this benefit is incidental to the constraint's operation — they discovered the limit, they did not create it. The extractiveness has increased slightly over the interval (0.05 → 0.08) as commercial actors have begun to capitalize on the safety differential, but remains near the Boltzmann floor for information standards. Suppression (0.02): Negligible. The constraint does not suppress alternatives through coercion — it is a biological fact that makes certain editing approaches (Cas9-based germline editing) unsafe. Researchers are free to attempt DSB-inducing edits; the embryos simply fail to repair them correctly. The minimal suppression reflects regulatory restrictions on germline editing (which are policy choices, not biological constraints) but these are orthogonal to the repair competence difference itself. Theater ratio (0.05): Very low. The constraint is measured via direct empirical observation (SNP arrays, long-range PCR, karyotyping). There is minimal performative content — the assays directly detect the repair failures (large deletions, segmental aneuploidy, chromosomal rearrangements). The modest theater reflects that some experimental protocols involve indirect inference (e.g., inferring repair pathway activity from editing outcomes rather than directly observing repair intermediates), but the core measurement is functional. Accessibility collapse (0.92): Very high. Once the substrate-specific repair competence is understood, alternative approaches (using DSB-inducing editors in early embryos) collapse almost completely — they are not forbidden by policy but are biologically unsafe. The high collapse reflects that the constraint is a genuine biological limit, not a contingent institutional arrangement. Resistance (0.03): Very low. The constraint meets almost no active resistance because it is an empirical fact. Some researchers initially questioned whether the repair failures were artifacts of experimental conditions (e.g., electroporation stress, culture conditions) but cross-lab replication and cross-species conservation have resolved these doubts. The minimal resistance reflects that the constraint is falsifiable and has been tested.
 *
 * PERSPECTIVAL GAP:
 *   All five perspectives classify the constraint as Mountain, but the beneficiary structure creates a false summit candidate. The edited embryo, clinical embryologist, and analytical observer see an immutable biological constraint. The fertility clinic and base editing research program also see a biological constraint but benefit from its existence. The perspectival gap is not between different constraint types but between 'genuine natural law' and 'naturalized contingent arrangement.' The false summit detector will flag this constraint because it has Mountain classification + declared beneficiaries + low extraction + high accessibility collapse. The omega variables document the irreducible uncertainty: is the substrate-specific repair competence truly immutable (conserved across species, independent of developmental stage, resistant to synthetic biology interventions), or is the 'natural law' framing premature? If alternative repair pathways can compensate for the HR/NHEJ deficiency, the constraint is contingent and the beneficiary structure is extractive (actors benefit from treating a solvable problem as unsolvable). If no alternative pathway works, the constraint is a genuine biological limit and the beneficiary structure is incidental (they benefit from discovering a constraint, not from creating it).
 *
 * DIRECTIONALITY LOGIC:
 *   The edited embryo is the primary subject of the constraint and has no beneficiary or victim status in the structural sense (it is not an agent that collects rents or bears extraction). The embryo's directionality is derived from its powerless/trapped position: maximum structural vulnerability, but the constraint is not extractive (it is a biological limit, not an extraction mechanism). The clinical embryologist is constrained by the biological limit but is not a victim (they can choose which editing tool to use). The fertility clinic and base editing research program are beneficiaries: they gain competitive advantage from the substrate difference (can market ABE as safer than Cas9). Their directionality is low (they experience the constraint as coordination or opportunity, not extraction). The analytical observer has no structural relationship to the constraint (analytical/analytical context) and experiences it as a natural law to be investigated. The beneficiary structure creates a false summit risk: actors who benefit from the 'no DSB' rule have an incentive to treat the constraint as immutable even if alternative repair pathways (synthetic biology, exogenous repair factors, delayed editing) could eventually overcome the limit. The omega variables address this ambiguity.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not exhibit mandatrophy in the traditional sense (a coordination mechanism whose original function has been superseded). The substrate-specific repair competence is a biological constraint, not a policy or institutional arrangement. However, the false summit framing introduces a related risk: if the 'natural law' framing is premature (if alternative repair pathways could eventually overcome the DSB repair deficiency), then treating the constraint as immutable serves the interests of actors who benefit from the current state of affairs (base editing programs, fertility clinics offering ABE services). The mandatrophy-adjacent risk is that the 'biological limit' framing forecloses research into alternative repair mechanisms (synthetic biology, exogenous repair factors, delayed editing) that could eventually make DSB-inducing editors safe. The omega variables address this by asking whether the constraint is genuinely immutable or whether the immutability claim is a false summit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_beneficiary,
    'Is the substrate-specific repair competence a genuine natural law (immutable biological constraint), or does the framing naturalize a contingent state of affairs that benefits identifiable actors (base editing research programs, fertility clinics)?',
    'Cross-species comparative analysis: if the repair competence difference is conserved across all mammalian embryos and is independent of experimental context, it is a natural law. If the difference is contingent on specific developmental conditions that could be altered (e.g., by pre-loading repair factors, by using later-stage embryos, by synthetic biology interventions), the ''natural law'' framing is premature and the beneficiary structure is extractive.',
    'If genuine natural law: Mountain classification is correct and beneficiary structure is incidental (they benefit from discovering a constraint, not from creating it). If contingent: the constraint is a Tangled Rope (coordination around a discovered biological limit + extraction by actors who benefit from the limit being treated as immutable).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_beneficiary, empirical, 'Whether substrate-specific repair competence is immutable or contingent').

omega_variable(
    repair_pathway_maturation_timeline,
    'At what developmental stage do homologous recombination and NHEJ pathways become competent in human embryos? Is the DSB repair failure specific to pre-implantation embryos, or does it persist into later stages?',
    'Developmental time-course analysis: test DSB repair competence at multiple stages (zygote, 2-cell, 4-cell, 8-cell, morula, blastocyst, post-implantation). If competence emerges at a specific stage, the constraint has a natural sunset (editing could be delayed until repair pathways mature). If competence never fully emerges during the editing window, the constraint is immutable for germline editing applications.',
    'If repair competence emerges early (by blastocyst stage): the constraint is a Scaffold (temporary limitation with a developmental sunset). If competence emerges late or never: the constraint is a Mountain (immutable for germline editing).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(repair_pathway_maturation_timeline, empirical, 'Developmental timeline of DSB repair pathway maturation').

omega_variable(
    alternative_repair_pathway_sufficiency,
    'Could alternative repair pathways (microhomology-mediated end joining, single-strand annealing, or synthetic repair factors) compensate for the HR/NHEJ deficiency in early embryos?',
    'Experimental intervention: pre-load embryos with exogenous repair factors (RAD51, DNA ligase IV, etc.) or use prime editing (which avoids DSBs entirely). If alternative pathways can restore DSB repair competence, the constraint is contingent on the endogenous repair machinery and is not a fundamental limit. If no alternative pathway works, the constraint is a genuine biological barrier.',
    'If alternative pathways work: the constraint is a Tangled Rope (coordination around current repair machinery + extraction by actors who benefit from the ''no DSB'' rule). If no alternative works: the constraint is a Mountain (immutable biological limit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_repair_pathway_sufficiency, empirical, 'Whether alternative repair pathways can compensate for HR/NHEJ deficiency').

omega_variable(
    cross_species_conservation,
    'Is the substrate-specific repair competence conserved across all mammalian species, or is it specific to humans (or primates)?',
    'Comparative embryology: test DSB repair competence in mouse, rat, rabbit, pig, cow, and non-human primate embryos at equivalent developmental stages. If the pattern is conserved, it is a fundamental mammalian constraint. If it is human-specific or primate-specific, the constraint may be contingent on specific evolutionary adaptations and could be altered by synthetic biology.',
    'If conserved across mammals: Mountain classification is strongly supported (universal biological constraint). If human-specific: the constraint may be contingent and the ''natural law'' framing is weaker.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cross_species_conservation, empirical, 'Cross-species conservation of substrate-specific repair competence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dna_repair_substrate_difference, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dna_repair_theater_discovery, dna_repair_substrate_difference, theater_ratio, 0, 0.02).
narrative_ontology:measurement(dna_repair_theater_early_adoption, dna_repair_substrate_difference, theater_ratio, 3, 0.03).
narrative_ontology:measurement(dna_repair_theater_clinical_translation, dna_repair_substrate_difference, theater_ratio, 6, 0.04).
narrative_ontology:measurement(dna_repair_theater_current, dna_repair_substrate_difference, theater_ratio, 10, 0.05).

% Extraction over time
narrative_ontology:measurement(dna_repair_extract_discovery, dna_repair_substrate_difference, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(dna_repair_extract_early_adoption, dna_repair_substrate_difference, base_extractiveness, 3, 0.06).
narrative_ontology:measurement(dna_repair_extract_clinical_translation, dna_repair_substrate_difference, base_extractiveness, 6, 0.07).
narrative_ontology:measurement(dna_repair_extract_current, dna_repair_substrate_difference, base_extractiveness, 10, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(dna_repair_suppress_discovery, dna_repair_substrate_difference, suppression_requirement, 0, 0.01).
narrative_ontology:measurement(dna_repair_suppress_early_adoption, dna_repair_substrate_difference, suppression_requirement, 3, 0.01).
narrative_ontology:measurement(dna_repair_suppress_clinical_translation, dna_repair_substrate_difference, suppression_requirement, 6, 0.02).
narrative_ontology:measurement(dna_repair_suppress_current, dna_repair_substrate_difference, suppression_requirement, 10, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dna_repair_substrate_difference, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is a biological limit, not a decomposed family of institutional constraints. It does not have sibling stories with different epsilon values. However, it is upstream of policy constraints (e.g., regulatory restrictions on germline editing, clinical guidelines for embryo editing) that treat the substrate difference as a fixed input. Those policy constraints are separate stories with their own extractiveness values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
