% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__neurodiversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dsm_taxonomy_kernel__neurodiversity_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: dsm_taxonomy_kernel__neurodiversity_reading
 *   human_readable: DSM Pathologization of Neurodiversity (Neurodiversity Reading)
 *   domain: medical_epistemology/psychiatric_taxonomy/social_construction_of_illness
 *
 * SUMMARY:
 *   The Diagnostic and Statistical Manual (DSM) is the authoritative taxonomy
 *   of psychiatric disorders. Under the neurodiversity reading — ONE
 *   interpretation of the DSM kernel — the manual functions as a system that
 *   pathologizes natural neurological variation (autism, ADHD, dyslexia,
 *   developmental coordination disorder, and others) when that variation
 *   conflicts with institutional behavioral norms (sitting still, rapid
 *   task-switching, reading fluency, fine motor conformity). The reading
 *   identifies the constraint as a tangled_rope: it solves a genuine
 *   coordination problem (psychiatric communication, treatment research)
 *   while simultaneously extracting from neurodivergent individuals through
 *   coercive normalization, denial of self-determination, and institutional
 *   exclusion justified by psychiatric diagnosis. The beneficiaries are
 *   institutional systems requiring behavioral conformity (schools,
 *   employers, courts, military), the psychiatric establishment that
 *   maintains diagnostic authority, and the pharmaceutical industry that
 *   profits from diagnosis expansion. The victims are neurodivergent
 *   individuals subjected to forced treatment, and marginalized groups
 *   disproportionately diagnosed. The authoring seat is the neurodiversity
 *   reading explicitly; sibling readings (biomedical and critical-psychiatry)
 *   would instantiate different constraints with different ε values and
 *   different beneficiary/victim structures from the same kernel.
 *
 * KEY AGENTS:
 *   - neurodivergent_individuals: powerless targets (identity_locked exit) subjected to coercive normalization; the extraction mechanism is denial of self-determination
 *   - institutional_conformity_systems: institutional beneficiaries (schools, employers, courts, military) that use psychiatric diagnosis to justify excluding or controlling non-conformity
 *   - psychiatric_diagnostic_establishment: institutional agenda-setter that maintains the DSM, conducts diagnosis, and controls the discourse about pathology
 *   - pharmaceutical_industry: institutional beneficiary profiting from diagnosis expansion and psychotropic drug markets
 *   - neurodiversity_advocacy_movement: excluded parties (moderate power) systematically kept out of diagnostic governance
 *   - disability_scholars: excluded knowledge producers (powerful) whose research documents the social constructedness of psychiatric categories
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__neurodiversity_reading, 0.78).
domain_priors:suppression_score(dsm_taxonomy_kernel__neurodiversity_reading, 0.81).
domain_priors:theater_ratio(dsm_taxonomy_kernel__neurodiversity_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__neurodiversity_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__neurodiversity_reading, "DSM Pathologization of Neurodiversity (Neurodiversity Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__neurodiversity_reading, "medical_epistemology/psychiatric_taxonomy/social_construction_of_illness").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__neurodiversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__neurodiversity_reading, 'c8c6610a-c2b9-4b13-9d85-6956485db1af').
narrative_ontology:cs_kernel_codification('c8c6610a-c2b9-4b13-9d85-6956485db1af', formalized).
narrative_ontology:cs_authority_grounding('c8c6610a-c2b9-4b13-9d85-6956485db1af', extraction).
narrative_ontology:cs_interpretation_layer_present('c8c6610a-c2b9-4b13-9d85-6956485db1af').
narrative_ontology:cs_reading_relation('c8c6610a-c2b9-4b13-9d85-6956485db1af', dsm_taxonomy_kernel__biomedical_reading, coexists_with).
narrative_ontology:cs_reading_relation('c8c6610a-c2b9-4b13-9d85-6956485db1af', dsm_taxonomy_kernel__critical_psychiatry_reading, coexists_with).
narrative_ontology:cs_axiom('c8c6610a-c2b9-4b13-9d85-6956485db1af', foundational, neurological_variation_not_pathology).
narrative_ontology:cs_axiom_status(neurological_variation_not_pathology, holdable).
narrative_ontology:cs_axiom_grounding('c8c6610a-c2b9-4b13-9d85-6956485db1af', neurological_variation_not_pathology, deontological).
narrative_ontology:cs_axiom('c8c6610a-c2b9-4b13-9d85-6956485db1af', foundational, neurodivergent_self_determination_right).
narrative_ontology:cs_axiom_status(neurodivergent_self_determination_right, holdable).
narrative_ontology:cs_axiom_grounding('c8c6610a-c2b9-4b13-9d85-6956485db1af', neurodivergent_self_determination_right, deontological).
narrative_ontology:cs_reference_frame('c8c6610a-c2b9-4b13-9d85-6956485db1af', neurodiversity_affirmation_framework).
narrative_ontology:cs_drift_state('c8c6610a-c2b9-4b13-9d85-6956485db1af', contemporary_psychiatric_expansion_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c8c6610a-c2b9-4b13-9d85-6956485db1af', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, institutional_conformity_systems).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, psychiatric_diagnostic_establishment).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_individuals).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, marginalized_groups_overdiagnosed).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, families_of_neurodivergent_people).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, families_of_neurodivergent_people).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Subjected to diagnostic labeling that reframes their neurological difference as pathology and justifies institutional coercion (forced medication, behavioral compliance regimes, institutionalization). Their identity and self-concept become fused with the diagnostic category; exit requires rejecting the framework itself, which is socially costly and materially difficult when institutions (schools, employers, courts) enforce diagnostic enforcement as a condition of access or freedom. The extractive mechanism is not money but forced normalization and denial of self-determination.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_individuals, payer,
    powerless, civilizational, identity_locked, universal).

% Disproportionately diagnosed with psychiatric conditions reflecting institutional bias rather than neurological difference (Black individuals overdiagnosed with schizophrenia; Indigenous children overdiagnosed with ADHD and removed from families; poor children diagnosed as conduct disorder when their behavior reflects trauma or environmental deprivation). The diagnostic machinery simultaneously pathologizes and carcerally controls these populations; exit from the diagnosis means exit from institutional scrutiny that already targets them.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, marginalized_groups_overdiagnosed, payer,
    powerless, civilizational, trapped, universal).

% Schools, workplaces, military, carceral systems benefit from psychiatric categories that justify excluding, controlling, or 'treating' individuals who do not conform to behavioral norms (sitting still, sustained attention to unstimulating tasks, suppressing stimming and self-regulation behaviors, emotional restraint). The diagnostic framework provides legitimacy for coercion as medical intervention rather than social control. These institutions set the norms; psychiatric taxonomy provides the apparatus to enforce them.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, institutional_conformity_systems, beneficiary,
    institutional, generational, arbitrage, universal).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__neurodiversity_reading, institutional_conformity_systems, agenda_setter).

% Maintains the DSM as authoritative, conducts diagnosis, receives professional status and insurance reimbursement tied to diagnostic codes, and controls the discourse about what counts as disorder. Their material interest lies in diagnosing widely; alternatives like neurodiversity affirmation models would collapse the diagnostic revenue stream and professional jurisdiction.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, psychiatric_diagnostic_establishment, agenda_setter,
    institutional, generational, arbitrage, universal).

% Profits directly from the expansion of psychiatric diagnoses, which create markets for psychotropic drugs (stimulants for ADHD, antipsychotics for autism and ADHD when presented as off-label, antidepressants and anti-anxiety medications prescribed widely to neurodivergent people). Funds psychiatric research that supports diagnosis expansion; finances DSM development directly and indirectly.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, pharmaceutical_industry, beneficiary,
    institutional, generational, arbitrage, universal).

% May benefit from access to services (accommodations, special education, medication that reduces distress for the diagnosed individual) that are gatekept behind diagnosis; simultaneously bear the burden of managing institutional compliance, monitoring medication, and internalizing shame narratives about their child's disorder. Their exit from accepting the diagnosis is constrained by institutional pressure (schools require diagnosis for accommodations) and their genuine need for support resources.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, families_of_neurodivergent_people, beneficiary,
    moderate, biographical, constrained, universal).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__neurodiversity_reading, families_of_neurodivergent_people, payer).

% Advocates that neurodivergence is natural neurological variation, not pathology; that pathologization causes harm through coercive normalization and denial of accommodation; and that justice requires affirmation and institutional design around neurodivergent needs rather than forced conformity. They are systematically excluded from DSM revision processes, pharmaceutical regulation, and institutional policy-making; the same diagnostic apparatus they contest controls their access to accommodations and services.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurodiversity_advocacy_movement, excluded,
    moderate, generational, constrained, global).

% Produce research documenting the social constructedness of disability categories, the extractive mechanisms embedded in psychiatric taxonomy, and the harm caused by pathologization. Their work is marginalized in medical education, psychiatric journals, and institutional policy; their presence in DSM and diagnostic system governance is token. They retain power through publication and influence over academic discourse but are excluded from the institutional machinery that enforces diagnosis.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, disability_rights_scholars, excluded,
    powerful, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dsm_taxonomy_kernel__neurodiversity_reading, psychiatric_diagnostic_establishment).
narrative_ontology:fixing_cost_class(dsm_taxonomy_kernel__neurodiversity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: DSM taxonomy coordinates the identification of individuals requiring intervention and justifies resource allocation to psychiatry and pharmaceutical treatment. It solves a genuine institutional coordination problem: how to decide who gets treated, who is excluded from institutional roles, who receives pharmaceutical access, and what counts as evidence for treatment decisions. The coordination happens between schools, employers, courts, and mental health systems around a common diagnostic language.
% TRANSFER_FUNCTION: Transfers self-determination and bodily autonomy from neurodivergent individuals to institutional systems and psychiatric authorities. Neurodivergent people lose the right to define themselves and their experiences; psychiatric professionals and institutional administrators gain authority to interpret their behavior, mandate intervention, and justify coercion (forced medication, institutionalization, exclusion from education or employment) as medical necessity. Pharmaceutical companies gain markets; institutional systems gain compliance justification.
% ABSENT_VOICES: Neurodivergent people themselves, particularly nonspeaking and intellectually disabled neurodivergent people, are systematically excluded from DSM revision processes, diagnostic criteria development, and psychiatric research design. Self-advocates and peer-led organizations are not represented in psychiatric governance. Disability scholars and neurodiversity researchers are excluded from medical journals and institutional policy. The voices heard are psychiatric professionals, pharmaceutical researchers, and institutional administrators — those who benefit from diagnosis expansion.
% DISAPPEARANCE_RATIONALE: If the DSM pathologization framework disappeared overnight, institutional norms would not disappear, but the mechanism for justifying coercion would shift. Schools would need to redesign around neurodivergent learning styles rather than diagnosing and medicating difference. Employers would accommodate rather than exclude. The carceral system would lose a major justification for incarceration of neurodivergent people. Pharmaceutical markets would contract. Psychiatric professional jurisdiction would shrink. The world would reorganize around accommodation and neurodiversity affirmation rather than normalization and treatment.
% FOUNDING_PROBLEM: Early-to-mid twentieth-century psychiatry faced a genuine diagnostic challenge: how to distinguish different types of mental distress and dysfunction, and how to offer treatment. The founding problem was the need for a shared nomenclature and classification system so that diagnosis in one location could be understood in another, and so that treatment research could be cumulative. The DSM solved this coordination problem.
% FOUNDING_PROBLEM_CORROBORATION: The founding coordination problem is attested by the history of psychiatry and the genuine utility of early diagnostic categories for severe mental illness (e.g., schizophrenia, severe depression). However, disability scholars, neurodiversity advocates, and critical psychiatry researchers attest that the founding problem has been solved: basic psychiatric communication and research taxonomy exists. The contemporary DSM expansion into normal neurological variation (ADHD, autism, social anxiety, developmental coordination disorder) does not solve the founding problem; it transforms diagnosis into a tool for institutional control of non-conformity. The problem-status shift is documented in comparative research on DSM edition expansions, pharmaceutical industry influence on diagnostic criteria, and longitudinal studies of diagnostic prevalence inflation uncoupled from any change in underlying neurology.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__neurodiversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__neurodiversity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__neurodiversity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dsm_taxonomy_kernel__neurodiversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__neurodiversity_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dsm_taxonomy_kernel__neurodiversity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dsm_taxonomy_kernel__neurodiversity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dsm_taxonomy_kernel__neurodiversity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 (1952) to 0.78 (2025) because diagnostic expansion outpaced genuine clinical need. The DSM-I (1952) contained ~100 diagnoses; by DSM-5 (2013) it contained ~300. This expansion tracks not with neurobiology but with pharmaceutical development (stimulants for ADHD, SSRIs for social anxiety) and institutional demand for diagnostic gatekeeping (schools requiring ADHD diagnosis for accommodations, employers using psychiatric diagnoses to deny employment). Theater ratio rises sharply (0.15 to 0.62) because the proportion of DSM activity devoted to coordinating communication about severe mental illness (the founding function) decreases while the proportion devoted to diagnosing and medicalizing normal neurological variation increases; increasing psychiatric activity looks like increasing service delivery but masks transformation into social control. Suppression requirement stays high (0.55–0.81) and increasing because the constraint's persistence depends on actively preventing neurodiversity framing from gaining institutional legitimacy, excluding disability scholars from psychiatric governance, and maintaining identity-lock fusion so neurodivergent people internalize the pathology narrative rather than recognizing it as institutional norm-enforcement. The measurement series use one shared time grid (shared at every point) so the engine samples all metrics at each interval point without misalignment.
 *
 * PERSPECTIVAL GAP:
 *   The psychiatric establishment (agenda-setter seat) experiences this constraint as genuine coordination and evidence-based practice: the DSM maps to neurobiology, diagnosis enables treatment, and expansion reflects improved detection of previously under-diagnosed conditions. Neurodivergent individuals (powerless payer seat) experience the same structure as pathologization justifying coercion and denial of self-determination. Institutional systems requiring conformity (beneficiary/agenda-setter seats) experience it as rational institutional design: diagnosis enables identification of people who need 'treatment' to fit institutional roles. Neurodiversity advocates (excluded seat) experience it as a system of social control operating under a medical legitimacy cover. The engine computes these divergent classifications from the structural data — the beneficiary/victim declarations, the exit options (identity_locked vs. arbitrage), the power differentials. This reading's claim (tangled_rope) asserts that both the coordination function AND the asymmetric extraction are structurally real; the claim does not privilege the psychiatric framing or the neurodiversity framing, but rather records that the constraint solves a real problem while simultaneously extracting from identifiable victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Neurodivergent individuals face maximum directionality toward the target end (d → 1.0): they are the named victims, their exit is identity_locked (the diagnosis becomes fused with self-concept; accepting psychiatric authority over interpretation of their own neurology), they bear the extraction (forced medication, behavioral compliance, institutional exclusion), and they have powerless power atoms with no alternative institutions to appeal to. The psychiatric establishment and institutional systems face d near the beneficiary end (d → 0.0): they set the rules, collect the rents (psychiatric billing, institutional compliance, pharmaceutical markets), and have arbitrage-grade exit (they can shift diagnostic categories or institutional norms if power shifts). Families are intermediate: they benefit from diagnostic gatekeeping to accommodations but also bear burden of institutional compliance and internalized shame; d sits near 0.5. Neurodiversity advocates are structurally trapped in a bind: they would exit the system entirely if they could, but institutional gatekeeping of accommodations (schools require diagnosis for special education) forces them to navigate it; they have constrained exit, moderate power, and the same victim interests as neurodivergent individuals but some organizational capacity that neurodivergent powerless individuals lack. The directionality differences across seats drive the per-seat classification divergence the engine computes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is 'dead' and disappearance_verdict is 'world_rearranges,' which flags a mandatrophy candidate. The founding problem (coordinating psychiatric communication and classification) was solved by DSM-I and has remained solved through successive editions. However, the DSM has evolved from solving that problem to primarily serving as a gatekeeping and legitimacy mechanism for institutional control. The problem it solves now is not psychiatric coordination but institutional conformity enforcement. This is mandatrophy: the constraint's original mandate has become obsolete, but the constraint persists due to institutional inertia, professional jurisdiction protection, and pharmaceutical industry incentives. The classification (tangled_rope) prevents mislabeling this as pure snare (which would require the coordination function to be purely rhetorical — it is not; real psychiatric communication happens through DSM categories) or as rope (which would require extraction to be coordination overhead — it is not; the extraction of neurodivergent autonomy exceeds what coordination requires). The tangled_rope classification properly captures the mandatrophy: a constraint that began as genuine coordination has evolved into a hybrid that solves the original problem less and extracts more, while institutional lock-in and beneficiary capture prevent organizational alignment with the actual mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    neurobiology_vs_social_construction_boundary,
    'To what extent do DSM categories map to objective neurobiological entities versus socially constructed categories that reify institutional norms into psychiatric nosology?',
    'Comparative neuroscience research across cultures with different institutional norms (do neurodivergent characteristics manifest differently in cultures with different behavioral expectations?); longitudinal studies of diagnostic criteria change against neurobiological evidence; historical analysis of how pharmaceutical availability shaped diagnostic category creation.',
    'If purely neurobiological, the pathologization narrative is weakened and the coordination function dominates — classification shifts toward Rope. If primarily socially constructed, extraction narrative dominates — classification shifts toward Snare. The mixed reality (some categories have neurobiological correlates; most expansion reflects social norm enforcement) supports Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neurobiology_vs_social_construction_boundary, empirical, 'The neurobiology/social construction boundary for DSM categories').

omega_variable(
    identity_lock_mechanism_suppression_internalization,
    'Is the measured suppression primarily structural (institutional gatekeeping of accommodations behind diagnosis) or internalized (neurodivergent people accepting pathologization narratives and believing normalization is necessary)?',
    'Post-institutional-exit trajectory studies: when neurodivergent people leave schools or institutional contexts and encounter neurodiversity-affirming communities, does suppression persist (evidence of internalization) or dissolve (evidence of structural gatekeeping)?',
    'If primarily internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the constraint with them after exit. If primarily structural, the constraint would weaken if institutional gatekeeping broke. This informs whether reform requires changing individual consciousness (treating internalized pathology narratives) or institutional design (decoupling accommodations from diagnosis).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_mechanism_suppression_internalization, empirical, 'Structural vs. internalized suppression in DSM pathologization').

omega_variable(
    beneficiary_capture_vs_coordination_necessity,
    'Would the coordination function of psychiatric communication persist if the diagnostic system were reformed to remove pathologizing language and remove gatekeeping of accommodations from diagnosis?',
    'Natural experiments from regions or institutions that have decoupled diagnosis from accommodation gatekeeping or adopted neurodiversity-affirming frameworks; analysis of whether psychiatric communication and research remain possible without DSM pathologization language.',
    'If coordination persists without pathologization, the current tangled_rope is revealed as unnecessary bundling and becomes a candidate for straightforward disentanglement (two separate constraints: genuine coordination + institutional control). If coordination requires pathologization, the tangled_rope is structurally necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_vs_coordination_necessity, empirical, 'Whether DSM pathologization is necessary for psychiatric coordination or whether beneficiary capture has bundled them artificially').

omega_variable(
    reading_contest_over_kernel_interpretation,
    'Which reading of the DSM kernel is correct: biomedical (pathology discovery), critical-psychiatry (pharmaceutical profit), or neurodiversity (institutional conformity enforcement)?',
    'Not resolvable by empirical data alone — this is a conceptual/interpretive contest. The kernel (DSM text) is stable, but parties interpret it through different normative frameworks. Resolution requires choosing between interpretive frameworks (medical, economic, social-justice), not measuring facts.',
    'The classification of the constraint depends entirely on which reading is adopted. Biomedical reading → Rope or Scaffold. Critical-psychiatry reading → Snare. Neurodiversity reading → Tangled Rope (this file). The corpus instantiates all three readings as separate constraint stories to enable comparative analysis and let the divergence in computed types surface the interpretive contest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_over_kernel_interpretation, conceptual, 'The kernel-level interpretive contest among three readings of DSM authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__neurodiversity_reading, 1952, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t1952, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 1952, 0.15).
narrative_ontology:measurement(dsm__tr_t1968, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 1968, 0.22).
narrative_ontology:measurement(dsm__tr_t1980, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 1980, 0.35).
narrative_ontology:measurement(dsm__tr_t1994, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 1994, 0.48).
narrative_ontology:measurement(dsm__tr_t2013, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 2013, 0.58).
narrative_ontology:measurement(dsm__tr_t2025, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 2025, 0.62).

% Extraction over time
narrative_ontology:measurement(dsm__be_t1952, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 1952, 0.35).
narrative_ontology:measurement(dsm__be_t1968, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 1968, 0.42).
narrative_ontology:measurement(dsm__be_t1980, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 1980, 0.48).
narrative_ontology:measurement(dsm__be_t1994, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 1994, 0.61).
narrative_ontology:measurement(dsm__be_t2013, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 2013, 0.72).
narrative_ontology:measurement(dsm__be_t2025, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t1952, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 1952, 0.55).
narrative_ontology:measurement(dsm__su_t1968, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 1968, 0.62).
narrative_ontology:measurement(dsm__su_t1980, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 1980, 0.68).
narrative_ontology:measurement(dsm__su_t1994, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 1994, 0.74).
narrative_ontology:measurement(dsm__su_t2013, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 2013, 0.79).
narrative_ontology:measurement(dsm__su_t2025, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 2025, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__neurodiversity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(dsm_taxonomy_kernel__neurodiversity_reading, 0.12).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel__biomedical_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel__critical_psychiatry_reading).

% DUAL FORMULATION NOTE:
% This constraint (neurodiversity reading) is one of three readings of the DSM kernel. The three readings share the same kernel text (the DSM itself, a stabilized formal document with recognized revision process and claimed authority) but differ in their normative interpretation of what it means and whose interests it serves. The biomedical reading interprets the DSM as a disease-discovery system; the critical-psychiatry reading as a pharmaceutical profit mechanism; the neurodiversity reading as an institutional conformity enforcement system. Each reading instantiates a different constraint with different epsilon values (0.35–0.45 for biomedical rope, 0.72–0.85 for critical-psychiatry snare, 0.78 for neurodiversity tangled_rope). The three stories are linked via network.affects_constraints to signal they are members of the same kernel family and enable comparative analysis of how different readings of the same stabilized document produce different structural and classificatory outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
