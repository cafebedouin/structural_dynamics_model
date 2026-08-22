% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__synchronic_diachronic_seam
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ip_category_emergence__synchronic_diachronic_seam, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: ip_category_emergence__synchronic_diachronic_seam
 *   human_readable: Synchronic-Diachronic Seam in IP Category Emergence
 *   domain: legal/philosophical/historical
 *
 * SUMMARY:
 *   The synchronic-diachronic seam test asserts that IP category emergence
 *   (thinkability: 'expression becomes a legally ownable category') and
 *   occupancy change (first-holding: 'this author becomes the first
 *   rights-holder in that category') are either formally independent
 *   dimensions or a temporal framing artifact — the M4/M5 collapse test. This
 *   reading treats the 1710 Statute of Anne not as the moment of category
 *   emergence (thinkability_reading) nor as the moment of first-holding
 *   (first_holding_reading), but as the stress point where the two dimensions
 *   either separate cleanly or collapse into each other. The constraint
 *   operates as a doctrinal sorting mechanism: courts and scholars use the
 *   seam to classify whether a novel claim asserts a new category
 *   (synchronic) or a new occupancy (diachronic). Over three centuries, the
 *   test has accumulated theater — the distinction is performed in treatises
 *   and judgments even as historical evidence shows 1710 actors did not
 *   recognize it. The beneficiaries are formalist scholars and doctrinal
 *   practitioners who gain a stable analytical grid; the payers are
 *   historical-materialist critics whose reading is structurally excluded.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__synchronic_diachronic_seam, 0.38).
domain_priors:suppression_score(ip_category_emergence__synchronic_diachronic_seam, 0.22).
domain_priors:theater_ratio(ip_category_emergence__synchronic_diachronic_seam, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, extractiveness, 0.38).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__synchronic_diachronic_seam, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__synchronic_diachronic_seam, "Synchronic-Diachronic Seam in IP Category Emergence").
narrative_ontology:topic_domain(ip_category_emergence__synchronic_diachronic_seam, "legal/philosophical/historical").

domain_priors:requires_active_enforcement(ip_category_emergence__synchronic_diachronic_seam).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__synchronic_diachronic_seam, 'cc2f33d3-e13a-4d5b-b51a-4348928ad8c0').
narrative_ontology:cs_kernel_codification('cc2f33d3-e13a-4d5b-b51a-4348928ad8c0', fixed_text).
narrative_ontology:cs_authority_grounding('cc2f33d3-e13a-4d5b-b51a-4348928ad8c0', lineage).
narrative_ontology:cs_interpretation_layer_present('cc2f33d3-e13a-4d5b-b51a-4348928ad8c0').
narrative_ontology:cs_reading_relation('cc2f33d3-e13a-4d5b-b51a-4348928ad8c0', ip_category_emergence__thinkability_reading, coexists_with).
narrative_ontology:cs_reading_relation('cc2f33d3-e13a-4d5b-b51a-4348928ad8c0', ip_category_emergence__first_holding_reading, coexists_with).
narrative_ontology:cs_axiom('cc2f33d3-e13a-4d5b-b51a-4348928ad8c0', foundational, synchronic_diachronic_independence).
narrative_ontology:cs_axiom_status(synchronic_diachronic_independence, holdable).
narrative_ontology:cs_axiom_grounding('cc2f33d3-e13a-4d5b-b51a-4348928ad8c0', synchronic_diachronic_independence, conventional).
narrative_ontology:cs_axiom('cc2f33d3-e13a-4d5b-b51a-4348928ad8c0', secondary, category_emergence_classificatory_utility).
narrative_ontology:cs_axiom_status(category_emergence_classificatory_utility, holdable).
narrative_ontology:cs_axiom_grounding('cc2f33d3-e13a-4d5b-b51a-4348928ad8c0', category_emergence_classificatory_utility, instrumental).
narrative_ontology:cs_reference_frame('cc2f33d3-e13a-4d5b-b51a-4348928ad8c0', formalist_doctrinal_grid_1710).
narrative_ontology:cs_drift_state('cc2f33d3-e13a-4d5b-b51a-4348928ad8c0', contemporary_critical_legal_history, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cc2f33d3-e13a-4d5b-b51a-4348928ad8c0', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, legal_formalist_scholars).
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, copyright_doctrine_practitioners).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, historical_jurisprudence_critics).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, materialist_ip_theorists).
narrative_ontology:constraint_vindicates(ip_category_emergence__synchronic_diachronic_seam, category_emergence_occupancy_independence).
narrative_ontology:constraint_vindicates(ip_category_emergence__synchronic_diachronic_seam, synchronic_diachronic_structural_test).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the synchronic-diachronic distinction as a live analytical tool in copyright doctrine. They benefit from the constraint's utility in adjudicating boundary cases — whether a work instantiates a new category or merely occupies an existing one. Their exit is mobile: they can shift to alternative analytical frameworks without career penalty, and the constraint's coherence sustains a scholarly niche.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, legal_formalist_scholars, beneficiary,
    organized, generational, mobile, global).

% Apply the seam test in litigation and policy: courts and registries use the distinction to decide whether a claim asserts category emergence (new kind of protectable subject matter) or occupancy change (new claimant in an established category). They administer the constraint through doctrinal gatekeeping. Exit is constrained — professional credentials and institutional roles bind them to the framework, but lateral movement to adjacent practice areas is possible.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, copyright_doctrine_practitioners, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__synchronic_diachronic_seam, copyright_doctrine_practitioners, beneficiary).

% Argue that the synchronic-diachronic distinction is a retrospective imposition on 1710 — the Statute of Anne did not separate category emergence from occupancy change; it fused them. They pay the cost of having their historical reading marginalized by the dominant doctrinal framework. Exit is constrained: academic reputation and methodological commitments make full departure costly, but they can publish in critical venues.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, historical_jurisprudence_critics, payer,
    moderate, biographical, constrained, global).

% Contend that the seam test masks the material conditions of IP's emergence — the 1710 moment was a political-economic settlement, not a conceptual discovery. Their critique is structurally excluded from mainstream doctrine because it refuses the formalist premise. Exit is identity_locked: their theoretical identity is constituted through opposition to the formalist framework; leaving it would dissolve the research program itself.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, materialist_ip_theorists, payer,
    moderate, generational, identity_locked, global).

% Observe the constraint's operation when drafting new IP statutes — they must decide whether to codify the synchronic-diachronic distinction or collapse it. They neither collect rents nor pay them directly, but their choices determine whether the constraint hardens into positive law or remains a doctrinal tool.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, legislative_drafting_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable analytical grid for distinguishing two structurally different IP claims: (1) 'this is a new kind of thing that can be owned' (category emergence) vs. (2) 'this is a new owner of an already-ownable thing' (occupancy change). The constraint coordinates judicial, scholarly, and legislative actors around a shared test for classifying IP disputes.
% TRANSFER_FUNCTION: Moves interpretive authority and doctrinal legitimacy from historical-materialist readings (which see 1710 as a fused political settlement) to formalist readings (which see 1710 as a conceptual discovery of ownable expression). The transfer is not monetary but epistemic: the constraint authorizes formalist methodology as the legitimate mode of IP reasoning.
% ABSENT_VOICES: Early modern stationers, authors, and parliamentary actors who lived the 1710 settlement — they would testify that category emergence and occupancy change were practically inseparable in the Stationers' Company's operational logic. Their absence is structural: they cannot speak, and the constraint's coherence depends on their silence.
% DISAPPEARANCE_RATIONALE: If the synchronic-diachronic seam test vanished overnight, IP doctrine would lose its primary tool for sorting boundary cases — AI-generated works, database rights, traditional knowledge claims, and derivative work disputes would all lack the classificatory grid that currently routes them to different doctrinal pathways. Courts and legislatures would improvise ad hoc replacements, likely importing the same distinction under new names.
% FOUNDING_PROBLEM: Post-1710 copyright doctrine needed a principled way to distinguish between extending protection to new subject matter categories (maps, lectures, photographs, software) versus adjudicating competing claims within established categories (who holds the right in this novel, this recording, this compilation). The seam test emerged as the doctrinal solution to this classification problem.
% FOUNDING_PROBLEM_CORROBORATION: Formalist scholars (beneficiaries) attest the problem is live — new technologies constantly raise category-emergence questions. Historical-materialist critics (payers) attest the problem is a retrospective construction: 1710 actors did not distinguish category from occupancy; the distinction was invented by late-19th-century treatise writers to rationalize judicial improvisation. Independent corroboration from legal historians (e.g., Ronan Deazley, Isabella Alexander) supports the critics' reading: the Statute of Anne's text and parliamentary debates show no awareness of a synchronic-diachronic distinction.
narrative_ontology:disappearance_verdict(ip_category_emergence__synchronic_diachronic_seam, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__synchronic_diachronic_seam, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__synchronic_diachronic_seam, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(ip_category_emergence__synchronic_diachronic_seam, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__synchronic_diachronic_seam, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__synchronic_diachronic_seam_tests).
:- end_tests(ip_category_emergence__synchronic_diachronic_seam_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) is moderate: the constraint extracts epistemic legitimacy from historical-materialist readings but provides genuine coordination value in doctrinal classification. Suppression (0.22) is low — the constraint does not legally forbid alternative readings, but doctrinal gatekeeping makes them professionally costly. Theater ratio (0.55) is high: the seam test is increasingly performed as a ritual of formalist method rather than used as a live analytical tool — the 1710 historical record does not support the distinction, yet the constraint persists. Accessibility collapse (0.35) is moderate: alternative readings (historical, materialist) remain available but are marginalized in mainstream doctrine. Resistance (0.45) is moderate: sustained critical literature exists but has not displaced the formalist framework.
 *
 * PERSPECTIVAL GAP:
 *   From the formalist seat, the seam test is a genuine coordination tool (rope-like) that solves real classification problems. From the materialist seat, the same structure operates as a snare — it extracts legitimacy from historical readings while suppressing the evidence that 1710 fused category and occupancy. The engine computes this divergence from the structural data: the same constraint reads as rope for beneficiaries and snare for payers.
 *
 * DIRECTIONALITY LOGIC:
 *   Legal formalist scholars and copyright practitioners are beneficiaries (d ~ 0.15-0.25): the constraint subsidizes their methodological niche and professional authority. Historical jurisprudence critics and materialist theorists are payers (d ~ 0.75-0.85): they bear the epistemic cost of exclusion. Materialist theorists are identity_locked — their theoretical identity fuses with opposition to the formalist framework. Legislative authorities are analytical observers (d ~ 0.5). The agenda-setter role (copyright practitioners) has constrained exit: they administer the constraint but cannot easily abandon it without destabilizing the doctrinal system they operate within.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (classifying novel IP claims) remains live — new technologies constantly raise category-emergence questions. But the seam test's historical justification (that 1710 discovered the distinction) is dead — the historical record shows the distinction was invented later. The constraint persists because it coordinates current doctrine, not because its origin story is true. This is a classic mandatrophy: the mandate (provide a classificatory grid) outlived its founding justification (historical discovery of the synchronic-diachronic distinction). The theater ratio rise tracks this: as the historical justification decayed, the performance of the distinction intensified.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    m4_m5_collapse_empirical,
    'Do the historical materials from 1710 (Statute of Anne text, parliamentary debates, Stationers'' Company records) show any evidence that actors distinguished category emergence from occupancy change, or did they treat them as fused?',
    'Archival research on the 1709-1710 legislative record and Stationers'' Company operational logic. If no actor distinguishes the two dimensions, the seam test is a retrospective imposition.',
    'If fused in 1710, the thinkability and first_holding readings are not independent constraints but two framings of a single fused event — the synchronic_diachronic_seam reading exposes a doctrinal fiction. Classification would shift toward piton (theatrical maintenance of a distinction with no historical referent).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(m4_m5_collapse_empirical, empirical, 'Whether the M4/M5 collapse is empirically grounded in 1710 or a later doctrinal invention.').

omega_variable(
    coordination_extraction_separability,
    'Can the doctrinal coordination function (classifying novel IP claims) be separated from the historical-fiction extraction (legitimizing formalist methodology via a false origin story)?',
    'Counterfactual doctrinal analysis: if courts and scholars used the seam test without the 1710 origin story, would classification accuracy change? If not, the historical fiction is pure extraction riding on real coordination.',
    'If separable, the constraint is a genuine tangled_rope (coordination + asymmetric extraction). If inseparable, the coordination function itself depends on the extraction — the constraint is a snare dressed as coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable or fused.').

omega_variable(
    committer_framing_underdetermination,
    'Does the kernel ''ip_category_emergence'' admit only these three readings, or does the committer frame (kernel + readings) itself impose a structure that excludes other framings (e.g., a purely economic reading of 1710 as rent-seeking settlement)?',
    'Meta-analysis of the kernel''s construction: who defined the kernel boundaries and reading set? If the kernel was constructed by formalist scholars, the reading set may be endogenous to the formalist framework.',
    'If the kernel frame is endogenous, all three readings share a hidden premise (that 1710 is a conceptual event rather than a political-economic one). The true structural alternative is outside the kernel entirely. This would reclassify the entire kernel family as a false summit mountain (presenting a constructed framework as natural law).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_framing_underdetermination, conceptual, 'Whether the kernel-reading decomposition itself encodes a formalist premise that excludes materialist framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__synchronic_diachronic_seam, 1710, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t1710, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 1710, 0.1).
narrative_ontology:measurement(ip_c_tr_t1774, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 1774, 0.18).
narrative_ontology:measurement(ip_c_tr_t1842, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 1842, 0.32).
narrative_ontology:measurement(ip_c_tr_t1911, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 1911, 0.45).
narrative_ontology:measurement(ip_c_tr_t1956, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 1956, 0.52).
narrative_ontology:measurement(ip_c_tr_t1988, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 1988, 0.55).
narrative_ontology:measurement(ip_c_tr_t2024, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 2024, 0.55).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t1710, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 1710, 0.15).
narrative_ontology:measurement(ip_c_be_t1774, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 1774, 0.22).
narrative_ontology:measurement(ip_c_be_t1842, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 1842, 0.31).
narrative_ontology:measurement(ip_c_be_t1911, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 1911, 0.35).
narrative_ontology:measurement(ip_c_be_t1956, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 1956, 0.38).
narrative_ontology:measurement(ip_c_be_t1988, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 1988, 0.38).
narrative_ontology:measurement(ip_c_be_t2024, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 2024, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t1710, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 1710, 0.08).
narrative_ontology:measurement(ip_c_su_t1774, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 1774, 0.12).
narrative_ontology:measurement(ip_c_su_t1842, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 1842, 0.18).
narrative_ontology:measurement(ip_c_su_t1911, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 1911, 0.22).
narrative_ontology:measurement(ip_c_su_t1956, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 1956, 0.22).
narrative_ontology:measurement(ip_c_su_t1988, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 1988, 0.22).
narrative_ontology:measurement(ip_c_su_t2024, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 2024, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__synchronic_diachronic_seam, identity_coordination).
narrative_ontology:boltzmann_floor_override(ip_category_emergence__synchronic_diachronic_seam, 0.08).
narrative_ontology:affects_constraint(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence__thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence__first_holding_reading).

% DUAL FORMULATION NOTE:
% This constraint is the synchronic_diachronic_seam reading of the ip_category_emergence kernel. The thinkability_reading treats category emergence as the primary event (M4); the first_holding_reading treats occupancy change as the primary event (M5). This reading tests whether M4 and M5 are independent or collapse. All three stories share the kernel but instantiate different constraints with different ε, different beneficiary/victim structures, and different claimed types. The thinkability_reading claims mountain (natural law of ownable expression); the first_holding_reading claims scaffold (transitional political settlement); this reading claims tangled_rope (coordination tool with historical-fiction extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ip_category_emergence__synchronic_diachronic_seam, organized, 0.2).
constraint_indexing:directionality_override(ip_category_emergence__synchronic_diachronic_seam, institutional, 0.15).
constraint_indexing:directionality_override(ip_category_emergence__synchronic_diachronic_seam, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
