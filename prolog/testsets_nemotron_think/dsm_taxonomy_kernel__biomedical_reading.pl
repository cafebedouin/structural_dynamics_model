% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__biomedical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dsm_taxonomy_kernel__biomedical_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: dsm_taxonomy_kernel__biomedical_reading
 *   human_readable: DSM Categories as Objective Neurobiological Disease Entities (Biomedical Reading)
 *   domain: medical_epistemology/psychiatric_taxonomy
 *
 * SUMMARY:
 *   The biomedical reading of the DSM taxonomy kernel asserts that
 *   psychiatric diagnostic categories (e.g., major depressive disorder,
 *   schizophrenia, bipolar disorder) correspond to discrete neurobiological
 *   disease entities that exist independently of human classification and are
 *   discoverable through empirical research. This reading underwrites the
 *   DSM's authority since DSM-III (1980). However, four decades of
 *   neuroscience have failed to identify validating biomarkers; categories
 *   show high comorbidity, heterogeneity, and dimensional structure.
 *   Meanwhile, the taxonomy enables involuntary treatment, pharmaceutical
 *   market creation, loss of legal capacity, and institutional gatekeeping —
 *   extracting substantially from diagnosed individuals while benefiting the
 *   psychiatric establishment, pharmaceutical industry, and
 *   conformity-enforcing institutions. The constraint is claimed as a
 *   mountain (natural law) but operates with high extractiveness, active
 *   enforcement, and clear beneficiary/victim structure — a false summit
 *   candidate.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__biomedical_reading, 0.78).
domain_priors:suppression_score(dsm_taxonomy_kernel__biomedical_reading, 0.82).
domain_priors:theater_ratio(dsm_taxonomy_kernel__biomedical_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__biomedical_reading, mountain).
narrative_ontology:human_readable(dsm_taxonomy_kernel__biomedical_reading, "DSM Categories as Objective Neurobiological Disease Entities (Biomedical Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__biomedical_reading, "medical_epistemology/psychiatric_taxonomy").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__biomedical_reading).
domain_priors:emerges_naturally(dsm_taxonomy_kernel__biomedical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__biomedical_reading, '99cc6e31-3c6e-4712-b996-975880ea134e').
narrative_ontology:cs_kernel_codification('99cc6e31-3c6e-4712-b996-975880ea134e', fixed_text).
narrative_ontology:cs_authority_grounding('99cc6e31-3c6e-4712-b996-975880ea134e', expertise).
narrative_ontology:cs_interpretation_layer_present('99cc6e31-3c6e-4712-b996-975880ea134e').
narrative_ontology:cs_reading_relation('99cc6e31-3c6e-4712-b996-975880ea134e', dsm_taxonomy_kernel__neurodiversity_reading, coexists_with).
narrative_ontology:cs_reading_relation('99cc6e31-3c6e-4712-b996-975880ea134e', dsm_taxonomy_kernel__critical_psychiatry_reading, coexists_with).
narrative_ontology:cs_axiom('99cc6e31-3c6e-4712-b996-975880ea134e', foundational, dsm_categories_are_natural_kinds).
narrative_ontology:cs_axiom_status(dsm_categories_are_natural_kinds, holdable).
narrative_ontology:cs_axiom_grounding('99cc6e31-3c6e-4712-b996-975880ea134e', dsm_categories_are_natural_kinds, empirically_contingent).
narrative_ontology:cs_axiom('99cc6e31-3c6e-4712-b996-975880ea134e', foundational, psychiatric_diagnosis_is_medical_science).
narrative_ontology:cs_axiom_status(psychiatric_diagnosis_is_medical_science, holdable).
narrative_ontology:cs_axiom_grounding('99cc6e31-3c6e-4712-b996-975880ea134e', psychiatric_diagnosis_is_medical_science, empirically_contingent).
narrative_ontology:cs_reference_frame('99cc6e31-3c6e-4712-b996-975880ea134e', biomedical_psychiatric_nosology).
narrative_ontology:cs_drift_state('99cc6e31-3c6e-4712-b996-975880ea134e', contemporary_neuroscience_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('99cc6e31-3c6e-4712-b996-975880ea134e', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, institutions_requiring_behavioral_conformity).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, people_meeting_diagnostic_criteria).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__biomedical_reading, psychiatric_disorders_are_brain_diseases).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__biomedical_reading, diagnostic_categories_reflect_natural_kinds).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__biomedical_reading, biomarker_discovery_will_validate_current_taxonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls DSM revision processes, diagnostic criteria, and professional training. Derives authority, reimbursement eligibility, and professional jurisdiction from the claim that categories reflect discoverable neurobiological entities. Can move between academic, clinical, and regulatory roles.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment, beneficiary).

% Develops and markets drugs keyed to DSM categories. Gains regulatory pathways, insurance coverage, and marketing authorization from the taxonomy's claim to medical legitimacy. Can redirect R&D to other therapeutic areas if the taxonomy loses credibility.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_industry, beneficiary,
    powerful, biographical, arbitrage, global).

% Schools, courts, employers, insurers, and disability systems use DSM diagnoses as gatekeeping mechanisms for accommodations, competence determinations, and resource allocation. Benefit from a putatively objective framework that legitimizes coercive interventions. Locked into the taxonomy by legal precedent and administrative infrastructure.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, institutions_requiring_behavioral_conformity, beneficiary,
    institutional, generational, constrained, national).

% Subject to involuntary treatment, loss of legal capacity (guardianship, firearm prohibitions, parental rights), mandatory medication, and stigma. Diagnostic labels become identity-defining; exit requires rejecting a medical framework that controls access to care, disability benefits, and legal protections. Identity-locked because the diagnostic framework constitutes their self-understanding and social recognition.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, people_meeting_diagnostic_criteria, payer,
    powerless, biographical, identity_locked, global).

% Argue that DSM categories pathologize natural neurological variation (autism, ADHD, etc.) that conflicts with institutional norms. Excluded from revision committees and guideline development; their testimony is heard but not incorporated into diagnostic criteria. Would object to the natural-kind claim and the coercive interventions it licenses.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, neurodiversity_advocates, excluded,
    organized, generational, constrained, global).

% Psychiatrists and researchers who argue DSM categories are constructed to serve pharmaceutical markets and institutional control. Excluded from mainstream nosology committees; publish in separate venues. Would object to the biomedical reading's empirical claims and its extraction-enabling function.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, critical_psychiatrists, excluded,
    moderate, biographical, constrained, global).

% Investigate whether DSM categories map onto identifiable neurobiological substrates. After 40+ years of biomarker research, the dominant finding is transdiagnostic overlap and dimensional structure rather than categorical boundaries. Their findings challenge the natural-kind claim but do not directly determine clinical practice.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, independent_neuroscience_researchers, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared diagnostic language enabling clinical communication, insurance reimbursement, treatment guidelines, and research standardization across psychiatry and adjacent fields.
% TRANSFER_FUNCTION: Moves diagnostic authority, involuntary treatment power, pharmaceutical market access, disability resources, and legal competence determinations from diagnosed individuals to psychiatric institutions, pharmaceutical companies, and conformity-enforcing systems.
% ABSENT_VOICES: Neurodiversity advocates and critical psychiatrists are structurally excluded from DSM revision processes; diagnosed individuals have no formal representation in nosology committees; transdiagnostic neuroscience findings are acknowledged but do not alter categorical boundaries.
% DISAPPEARANCE_RATIONALE: If the biomedical reading's authority vanished, insurance reimbursement would collapse, involuntary treatment criteria would require new justification, pharmaceutical indications would lose regulatory anchoring, and disability/legal systems would need alternative gatekeeping mechanisms. The entire psychiatric-legal-pharmaceutical infrastructure would reorganize.
% FOUNDING_PROBLEM: Late 19th/early 20th century psychiatry lacked reliable diagnostic categories, leading to chaotic asylums, unscientific practice, and no basis for treatment development or insurance. The biomedical reading traces to Kraepelin's disease-entity model and the 1980 DSM-III 'atheoretical' descriptive turn that promised empirical validation.
% FOUNDING_PROBLEM_CORROBORATION: The psychiatric establishment attests the problem remains live (diagnostic reliability still imperfect, biomarkers forthcoming). Neurodiversity advocates, critical psychiatrists, and independent neuroscience researchers attest the founding problem was mischaracterized — the 'chaos' was largely iatrogenic and the biomedical solution created new forms of extraction. No disinterested party corroborates the original framing.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__biomedical_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__biomedical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__biomedical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dsm_taxonomy_kernel__biomedical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__biomedical_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dsm_taxonomy_kernel__biomedical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dsm_taxonomy_kernel__biomedical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, ExtMetricName, E),
    domain_priors:suppression_score(dsm_taxonomy_kernel__biomedical_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(dsm_taxonomy_kernel__biomedical_reading),
    narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(dsm_taxonomy_kernel__biomedical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) reflects the taxonomy's role in licensing coercive interventions and pharmaceutical markets decoupled from validated biology. Suppression (0.82) is high because alternatives (dimensional models, psychosocial frameworks, neurodiversity paradigms) are structurally excluded from nosology committees and insurance systems. Theater ratio (0.38) captures the gap between the 'empirical validation forthcoming' narrative and the static categorical architecture. Accessibility collapse (0.72) is elevated because the biomedical frame makes alternatives appear unscientific once internalized. Resistance (0.45) is moderate — survivor movements, critical psychiatry, and neurodiversity advocacy exist but lack institutional power. The measurement series tracks DSM-III (1980) through DSM-5-TR (2022) on a shared grid.
 *
 * PERSPECTIVAL GAP:
 *   From the psychiatric establishment's seat, the constraint is genuine coordination: a shared language enabling clinical care and research. From the diagnosed individual's seat, the same structure operates as enforced extraction: a label that removes liberty, mandates treatment, and constitutes identity. The engine computes this divergence from the declared power/exit/role structure — the biomedical reading's own claim (mountain) is the perspective of the agenda_setter seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Psychiatric establishment (agenda_setter/beneficiary, institutional, arbitrage exit) sits at d≈0.1 — controls the taxonomy and profits from its authority. Pharmaceutical industry (beneficiary, powerful, arbitrage) at d≈0.15 — captures market value but could pivot. Conformity institutions (beneficiary, institutional, constrained) at d≈0.25 — locked into the taxonomy by legal/administrative path dependence. Diagnosed individuals (payer, powerless, identity_locked) at d≈0.95 — the constraint constitutes their identity and legal status; exit means losing care access and protections. Neurodiversity advocates and critical psychiatrists (excluded, organized/moderate, constrained) at d≈0.7 — bear epistemic suppression but not direct extraction. Independent researchers (observer, analytical) at d=0.5.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (diagnostic chaos) was real in 1980. The biomedical reading claims it remains live; siblings argue it was solved by DSM-III's descriptive reliability and the current arrangement persists as rent extraction. The mandatrophy is unresolved: the taxonomy's coordination function (reliability) is real but its validation promise (validity) has failed, while extraction has increased. This is not pure coordination (rope) nor pure extraction (snare) but a tangled_rope masquerading as a mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the DSM taxonomy a genuine discovery of natural disease kinds, or a constructed classification that benefits identifiable agents?',
    'Convergent validation: if biomarkers with specificity/sensitivity >0.9 emerge for current categories, natural-kind claim gains support; if transdiagnostic dimensional structures continue to dominate findings, constructed interpretation gains support.',
    'If constructed, the mountain claim is a false summit; the constraint reclassifies as tangled_rope or snare via FSM signature. Beneficiary/victim structure would then be the primary classifier, not emergent naturalness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, empirical, 'Whether the taxonomy''s natural-kind claim reflects reality or serves as ideological cover for extraction.').

omega_variable(
    committer_structure_kernel_reading,
    'How does this reading''s structural profile change under the neurodiversity_reading and critical_psychiatry_reading framings?',
    'Comparative constraint story generation: author the sibling readings as separate constraints with their own ε, beneficiaries, victims, and claimed_types; compare effective extraction profiles across seats.',
    'If sibling readings produce substantially different χ profiles for the same agents, the kernel''s contestation is structurally consequential — not merely interpretive. The engine''s kernel-level analysis would then track how the contest distributes extraction across readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Committer-frame structural delta: this reading (biomedical) instantiates high extraction on diagnosed individuals; neurodiversity reading would instantiate identity_locked extraction on neurodivergent people; critical psychiatry reading would instantiate pharmaceutical_industry as primary beneficiary with diagnosed individuals as payers.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative frameworks (dimensional, neurodiversity, psychosocial) structural (institutional exclusion, insurance coding) or internalized (clinicians and patients believe the biomedical frame is the only legitimate one)?',
    'Post-reform observation: if insurance systems adopted ICD-11 dimensional specifiers or neurodiversity-affirming codes, would clinical practice shift rapidly (suggesting structural suppression) or resist (suggesting internalized suppression)?',
    'If internalized, effective suppression is higher than structural measures suggest — the constraint travels with the agent after institutional exit. This would increase χ for diagnosed individuals beyond the engine''s structural derivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the psychiatric taxonomy''s enforcement of the biomedical frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__biomedical_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm_biomed_tr_t1980, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(dsm_biomed_tr_t1994, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 1994, 0.22).
narrative_ontology:measurement(dsm_biomed_tr_t2000, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement(dsm_biomed_tr_t2013, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2013, 0.33).
narrative_ontology:measurement(dsm_biomed_tr_t2022, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2022, 0.36).
narrative_ontology:measurement(dsm_biomed_tr_t2024, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(dsm_biomed_be_t1980, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(dsm_biomed_be_t1994, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 1994, 0.58).
narrative_ontology:measurement(dsm_biomed_be_t2000, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(dsm_biomed_be_t2013, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2013, 0.72).
narrative_ontology:measurement(dsm_biomed_be_t2022, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2022, 0.76).
narrative_ontology:measurement(dsm_biomed_be_t2024, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(dsm_biomed_su_t1980, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(dsm_biomed_su_t1994, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 1994, 0.62).
narrative_ontology:measurement(dsm_biomed_su_t2000, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(dsm_biomed_su_t2013, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2013, 0.77).
narrative_ontology:measurement(dsm_biomed_su_t2022, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2022, 0.8).
narrative_ontology:measurement(dsm_biomed_su_t2024, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2024, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__biomedical_reading, information_standard).
narrative_ontology:boltzmann_floor_override(dsm_taxonomy_kernel__biomedical_reading, 0.02).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel__neurodiversity_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel__critical_psychiatry_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the single label 'DSM validity' into three structurally distinct readings with different ε, beneficiary/victim sets, and claimed types. The biomedical reading claims mountain (ε≈0.08 intrinsic, but measured 0.78 under contest); neurodiversity reading claims tangled_rope (coordination of identity + extraction of conformity); critical psychiatry reading claims snare (pharmaceutical market construction). They share the same kernel text but instantiate different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dsm_taxonomy_kernel__biomedical_reading, institutional, 0.1).
constraint_indexing:directionality_override(dsm_taxonomy_kernel__biomedical_reading, powerful, 0.15).
constraint_indexing:directionality_override(dsm_taxonomy_kernel__biomedical_reading, powerless, 0.95).
constraint_indexing:directionality_override(dsm_taxonomy_kernel__biomedical_reading, organized, 0.7).
constraint_indexing:directionality_override(dsm_taxonomy_kernel__biomedical_reading, moderate, 0.7).
constraint_indexing:directionality_override(dsm_taxonomy_kernel__biomedical_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
