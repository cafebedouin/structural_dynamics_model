% ============================================================================
% CONSTRAINT STORY: notability_guidelines__inclusionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_notability_guidelines__inclusionist_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: notability_guidelines__inclusionist_reading
 *   human_readable: WP:N Notability Guidelines â Inclusionist Reading (Structural Gatekeeping)
 *   domain: digital_commons_governance/knowledge_infrastructure
 *
 * SUMMARY:
 *   WP:N (Wikipedia Notability Guidelines) as interpreted through the
 *   inclusionist reading functions as a structural gatekeeping apparatus. It
 *   requires topics to be covered by 'reliable sources' â a category
 *   overwhelmingly populated by institutional knowledge producers (academic
 *   presses, mainstream media) â thereby systematically excluding knowledge
 *   from marginalized communities that lack such coverage. The constraint is
 *   actively enforced through Articles for Deletion (AfD), speedy deletion,
 *   and editorial oversight. The inclusionist reading frames this not as
 *   neutral quality control but as an extractive snare that reproduces
 *   epistemic inequality under the guise of procedural neutrality.
 *
 * KEY AGENTS:
 *   - institutional_knowledge_producers: Primary beneficiary (institutional/arbitrage) â their outputs are the mandated currency of inclusion.
 *   - marginalized_communities: Primary target (powerless/trapped) â their knowledge is erased from the digital commons.
 *   - editorial_enforcement_community: Agenda setter (organized/constrained) â administers the guideline through deletion processes.
 *   - inclusionist_critics: Analytical observer (moderate/analytical) â sees the systemic bias but lacks power to change the rule.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__inclusionist_reading, 0.82).
domain_priors:suppression_score(notability_guidelines__inclusionist_reading, 0.8).
domain_priors:theater_ratio(notability_guidelines__inclusionist_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__inclusionist_reading, snare).
narrative_ontology:human_readable(notability_guidelines__inclusionist_reading, "WP:N Notability Guidelines â Inclusionist Reading (Structural Gatekeeping)").
narrative_ontology:topic_domain(notability_guidelines__inclusionist_reading, "digital_commons_governance/knowledge_infrastructure").

domain_priors:requires_active_enforcement(notability_guidelines__inclusionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__inclusionist_reading, '53d38c22-31e8-4505-8ed8-5e8062fee9e7').
narrative_ontology:cs_kernel_codification('53d38c22-31e8-4505-8ed8-5e8062fee9e7', formalized).
narrative_ontology:cs_authority_grounding('53d38c22-31e8-4505-8ed8-5e8062fee9e7', practice).
narrative_ontology:cs_interpretation_layer_present('53d38c22-31e8-4505-8ed8-5e8062fee9e7').
narrative_ontology:cs_reading_relation('53d38c22-31e8-4505-8ed8-5e8062fee9e7', notability_guidelines__deletionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('53d38c22-31e8-4505-8ed8-5e8062fee9e7', notability_guidelines__deliberative_reading, coexists_with).
narrative_ontology:cs_axiom('53d38c22-31e8-4505-8ed8-5e8062fee9e7', foundational, inclusion_as_epistemic_justice).
narrative_ontology:cs_axiom_status(inclusion_as_epistemic_justice, holdable).
narrative_ontology:cs_axiom_grounding('53d38c22-31e8-4505-8ed8-5e8062fee9e7', inclusion_as_epistemic_justice, deontological).
narrative_ontology:cs_axiom('53d38c22-31e8-4505-8ed8-5e8062fee9e7', foundational, institutional_source_supremacy_is_exclusionary).
narrative_ontology:cs_axiom_status(institutional_source_supremacy_is_exclusionary, holdable).
narrative_ontology:cs_axiom_grounding('53d38c22-31e8-4505-8ed8-5e8062fee9e7', institutional_source_supremacy_is_exclusionary, empirically_contingent).
narrative_ontology:cs_reference_frame('53d38c22-31e8-4505-8ed8-5e8062fee9e7', inclusive_epistemic_commons).
narrative_ontology:cs_drift_state('53d38c22-31e8-4505-8ed8-5e8062fee9e7', contemporary_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('53d38c22-31e8-4505-8ed8-5e8062fee9e7', '').
narrative_ontology:cs_kernel_id(notability_guidelines__inclusionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, institutional_knowledge_producers).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, marginalized_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce peer-reviewed scholarship, mainstream journalism, and institutional records that Wikipedia's notability guideline treats as the necessary and often sufficient evidence for topic inclusion. Their epistemic authority is amplified when alternative knowledge forms are ruled inadmissible.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, institutional_knowledge_producers, beneficiary,
    institutional, generational, arbitrage, global).

% Hold community knowledge, oral histories, and local practices that are systematically excluded from Wikipedia when no independent institutional source has documented them. They bear the cost of representational erasure in the digital commons.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, marginalized_communities, payer,
    powerless, generational, trapped, global).

% Wikipedia editors and administrators who interpret and enforce notability guidelines through deletion discussions, policy citations, and editorial oversight. Their work is framed as neutral quality control, but in practice it filters content through an institutional source hierarchy.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, editorial_enforcement_community, agenda_setter,
    organized, biographical, constrained, global).

% Editors, researchers, and community advocates who argue that notability standards embed systemic bias and who document the exclusion of marginalized knowledge. They participate in policy debates but are structurally outnumbered in enforcement forums.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, inclusionist_critics, observer,
    moderate, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(notability_guidelines__inclusionist_reading, institutional_knowledge_producers).
narrative_ontology:fixing_cost_class(notability_guidelines__inclusionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Nominally coordinates encyclopedic quality control by establishing a uniform threshold for inclusion, preventing the commons from being overwhelmed by unsourced, self-promotional, or ephemeral content.
% TRANSFER_FUNCTION: Moves epistemic authority and platform visibility from marginalized, non-institutional knowledge holders to institutional knowledge producers (academia, mainstream press), by requiring their outputs as the sole valid evidence of significance.
% ABSENT_VOICES: Marginalized communities whose knowledge is oral, local, or community-verified are structurally absent from deletion debates because they lack the time, access, and discursive fluency to navigate Wikipedia's deliberative processes; their knowledge is represented only when institutional mediators have already documented it.
% DISAPPEARANCE_RATIONALE: If the notability guideline and its enforcement vanished overnight, the encyclopedia's content mix would shift dramatically toward inclusion of marginalized knowledge, oral histories, and local expertise; the epistemic hierarchy that privileges institutional 'reliable sources' would flatten, and the commons would reorganize around broader verifiability standards.
% FOUNDING_PROBLEM: Early Wikipedia faced degradation from vanity pages, self-promotion, and unverifiable content; notability guidelines were built to protect encyclopedic quality and prevent the commons from becoming a directory or advertising platform.
% FOUNDING_PROBLEM_CORROBORATION: Critical data studies scholars, inclusionist Wikipedians, and representatives of marginalized communities attest that the founding quality problem is now managed by scale and tooling, and that the guideline persists primarily as epistemic gatekeeping; this corroboration comes from outside the benefiting parties.
narrative_ontology:disappearance_verdict(notability_guidelines__inclusionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__inclusionist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__inclusionist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(notability_guidelines__inclusionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__inclusionist_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(notability_guidelines__inclusionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(notability_guidelines__inclusionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(notability_guidelines__inclusionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 at interval end) because the constraint transfers epistemic authority from marginalized knowledge to institutional sources. Suppression is substantial (0.80) because persistence depends on active deletion of non-compliant content and the discrediting of alternative epistemic standards. Theater_ratio is elevated (0.58) because the 'quality control' justification is increasingly performative: the enforcement machinery spends more energy defending the source hierarchy than improving verifiable accuracy. Accessibility_collapse is high (0.82) because once inside the framework, alternatives (oral history, community verification) are literally unthinkable as valid evidence. Resistance is moderate (0.45) because inclusionist editors and external critics mount sustained but institutionally marginalized opposition.
 *
 * PERSPECTIVAL GAP:
 *   The institutional knowledge producer seat experiences the constraint as a neutral, even beneficial, quality filter that protects the commons from degradation (low directionality, low effective extraction). The marginalized community seat experiences it as total epistemic erasure: their knowledge cannot enter the archive without an institutional mediator (high directionality, high effective extraction). The editorial enforcement seat experiences it as a legitimate community practice (symmetric to slightly beneficial), while the inclusionist critic sees the same practice as systemic violence. The engine computes these divergences from the structural role and exit data.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional knowledge producers are beneficiaries because the constraint subsidizes their epistemic authority by treating their publications as necessary and sufficient for inclusion (d near 0.0). Marginalized communities are victims/payers because the constraint extracts their representational presence from the commons; their lack of 'reliable sources' is treated as a failure of the topic rather than the source ecology (d near 1.0). The editorial enforcement community sits near symmetric: they do not personally collect the benefit but are identity-locked to the practice of enforcement; their exit is constrained by community norms (d ~0.5). Inclusionist critics are analytical observers with arbitrage-grade exit (they can leave or theorize), giving them low effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The inclusionist reading prevents mislabeling by insisting that the founding problem â preventing vanity spam and unverifiable content â has been solved by the community's scale and tools, and that the current apparatus now functions to exclude. The mandatrophy mismatch (founding_problem_status: dead vs. disappearance_verdict: world_rearranges) flags the constraint as a potential zombie: it persists not because the founding problem is live, but because it serves the interests of institutional knowledge producers. Without this R5 analysis, the constraint might be misread as a rope (coordination) or scaffold (transitional quality control).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gatekeeping_or_quality_control,
    'Does WP:N function primarily as an epistemic quality control mechanism or as a structural gatekeeping apparatus that reproduces institutional knowledge hierarchies?',
    'Cross-reading epsilon comparison across the kernel family; empirical content analysis of AfD outcomes by subject demographic; measurement of source-type bias in kept vs. deleted articles.',
    'If gatekeeping dominates, the inclusionist reading is structurally accurate (snare); if quality control dominates, the deletionist reading is more accurate (rope/mountain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_or_quality_control, empirical, 'Whether the constraint''s primary function is quality control or epistemic gatekeeping.').

omega_variable(
    marginalized_voice_absence_mechanism,
    'Is the absence of marginalized communities in notability deliberations due to structural barriers (access, time, language) or due to genuine lack of interest/engagement?',
    'Ethnographic study of marginalized community members'' attempts to participate in AfD; analysis of participation barriers.',
    'If structural, suppression is higher than procedural neutrality suggests; if lack of interest, the constraint may be less extractive than claimed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalized_voice_absence_mechanism, empirical, 'Structural versus voluntary absence of marginalized voices in enforcement.').

omega_variable(
    kernel_reading_stability,
    'Can the inclusionist and deletionist readings of the notability kernel coexist as live constraints, or does the adoption of one reading logically collapse the other?',
    'Engine computation of cs_axiom_contradiction across the family; analysis of whether a single Wikipedia community can hold both readings simultaneously without policy fork.',
    'If they foreclose each other, the kernel is brittle and the constraint''s persistence depends on which reading dominates institutionally; if they coexist, the constraint is a distributed commitment system with multiple live interpretations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_stability, conceptual, 'Whether sibling readings are mutually exclusive or co-live within the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__inclusionist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(notability_inclusionist_tr_t0, notability_guidelines__inclusionist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(notability_inclusionist_tr_t4, notability_guidelines__inclusionist_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(notability_inclusionist_tr_t8, notability_guidelines__inclusionist_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(notability_inclusionist_tr_t12, notability_guidelines__inclusionist_reading, theater_ratio, 12, 0.45).
narrative_ontology:measurement(notability_inclusionist_tr_t16, notability_guidelines__inclusionist_reading, theater_ratio, 16, 0.52).
narrative_ontology:measurement(notability_inclusionist_tr_t20, notability_guidelines__inclusionist_reading, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(notability_inclusionist_be_t0, notability_guidelines__inclusionist_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(notability_inclusionist_be_t4, notability_guidelines__inclusionist_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(notability_inclusionist_be_t8, notability_guidelines__inclusionist_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(notability_inclusionist_be_t12, notability_guidelines__inclusionist_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(notability_inclusionist_be_t16, notability_guidelines__inclusionist_reading, base_extractiveness, 16, 0.75).
narrative_ontology:measurement(notability_inclusionist_be_t20, notability_guidelines__inclusionist_reading, base_extractiveness, 20, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(notability_inclusionist_su_t0, notability_guidelines__inclusionist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(notability_inclusionist_su_t4, notability_guidelines__inclusionist_reading, suppression_requirement, 4, 0.48).
narrative_ontology:measurement(notability_inclusionist_su_t8, notability_guidelines__inclusionist_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(notability_inclusionist_su_t12, notability_guidelines__inclusionist_reading, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(notability_inclusionist_su_t16, notability_guidelines__inclusionist_reading, suppression_requirement, 16, 0.75).
narrative_ontology:measurement(notability_inclusionist_su_t20, notability_guidelines__inclusionist_reading, suppression_requirement, 20, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, deletionist_reading).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, deliberative_reading).

% DUAL FORMULATION NOTE:
% The notability guidelines kernel decomposes into three structurally distinct constraints: deletionist_reading (quality filter, low epsilon), deliberative_reading (ongoing negotiation, moderate epsilon), and inclusionist_reading (gatekeeping apparatus, high epsilon). Each reading has its own epsilon, stakeholders, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
