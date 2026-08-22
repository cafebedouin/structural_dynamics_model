% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__biomedical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: dsm_taxonomy_kernel__biomedical_reading
 *   human_readable: DSM Biomedical Taxonomy Kernel — Disease Discovery Reading
 *   domain: medical_epistemology/psychiatric_taxonomy
 *
 * SUMMARY:
 *   The DSM (Diagnostic and Statistical Manual) is a contested kernel: a
 *   standardized psychiatric taxonomy defended as a discovered classification
 *   of objective neurobiological disease entities. Under the biomedical
 *   reading instantiated here, DSM categories map to real, discoverable
 *   disorders (depression, schizophrenia, ADHD, etc.) grounded in
 *   neurobiology; diagnosis is the recognition of an objective fact;
 *   treatment—especially pharmacological—is the appropriate response. This
 *   reading justifies involuntary commitment, coercive medication, and loss
 *   of legal capacity for diagnosed individuals. It benefits the psychiatric
 *   establishment (gatekeeping authority), pharmaceutical manufacturers
 *   (diagnostic expansion creates patient populations), and institutional
 *   conformity enforcers (behavioral noncompliance becomes medicatable rather
 *   than disciplinable). The reading extracts from those meeting diagnostic
 *   criteria, who become subject to coercive intervention, permanent record
 *   annotation, and identity-lock as diseased. This story instantiates ONLY
 *   the biomedical reading; the neurodiversity reading and critical
 *   psychiatry reading are sibling constraints in the same kernel family,
 *   with different ε values and beneficiary/victim structures. The
 *   claim/metric gap is by design: the constraint is CLAIMED as tangled_rope
 *   (coordination + enforcement) under the biomedical reading's own frame;
 *   the authored metrics (high extractiveness, high suppression, rising
 *   theater_ratio over time) describe a constraint that increasingly operates
 *   as extraction despite coordination justification—the engine measures that
 *   divergence.
 *
 * KEY AGENTS:
 *   - Psychiatric establishment: institutional agenda-setter, defines diagnostic categories, revises DSM every 5–10 years, controls professional legitimacy and training gatekeeping.
 *   - Pharmaceutical manufacturers: institutional beneficiary and secondary payer, funds research and medical education, benefits directly from category expansion and diagnostic threshold preservation.
 *   - Diagnostic threshold population: powerless victims subject to pharmacological intervention, loss of legal capacity, permanent record annotations, and identity-lock preventing exit from diagnosed category.
 *   - Involuntary treatment subjects: trapped victims subject to coercive medication and restraint justified by DSM diagnosis; exit requires proving non-dangerousness (controlled by the treatment system).
 *   - Institutional conformity enforcers (schools, workplaces, prisons): institutional beneficiaries using DSM diagnoses to legitimize behavioral intervention without legal authorization for punishment.
 *   - Research community: institutional beneficiary and secondary agenda-setter; career advancement depends on discovering biological markers for DSM categories; funding follows disease discovery, not social construction.
 *   - Neurodiversity advocates: excluded powerless seat that would dispute the disease framing and argue for natural variation acceptance instead of pathology.
 *   - Critical psychiatry scholars: excluded moderate-power seat that would argue pharmaceutical influence shapes category construction and that alternative readings exist.
 *   - Service users seeking explanation: powerless beneficiary/payer who gains relief from disease narrative (externalized blame) but bears cost of living inside constraining framing.
 *   - Competition regulators: observer institutional seat evaluating whether DSM authority is independent or captured, whether gatekeeping is anticompetitive.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__biomedical_reading, 0.78).
domain_priors:suppression_score(dsm_taxonomy_kernel__biomedical_reading, 0.72).
domain_priors:theater_ratio(dsm_taxonomy_kernel__biomedical_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__biomedical_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__biomedical_reading, "DSM Biomedical Taxonomy Kernel — Disease Discovery Reading").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__biomedical_reading, "medical_epistemology/psychiatric_taxonomy").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__biomedical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__biomedical_reading, 'c2ad58cb-b5c7-458f-b0ca-86f6418f778b').
narrative_ontology:cs_kernel_codification('c2ad58cb-b5c7-458f-b0ca-86f6418f778b', fixed_text).
narrative_ontology:cs_authority_grounding('c2ad58cb-b5c7-458f-b0ca-86f6418f778b', extraction).
narrative_ontology:cs_interpretation_layer_present('c2ad58cb-b5c7-458f-b0ca-86f6418f778b').
narrative_ontology:cs_reading_relation('c2ad58cb-b5c7-458f-b0ca-86f6418f778b', dsm_taxonomy_kernel__neurodiversity_reading, coexists_with).
narrative_ontology:cs_reading_relation('c2ad58cb-b5c7-458f-b0ca-86f6418f778b', dsm_taxonomy_kernel__critical_psychiatry_reading, coexists_with).
narrative_ontology:cs_axiom('c2ad58cb-b5c7-458f-b0ca-86f6418f778b', foundational, dsm_categories_map_neurobiological_entities).
narrative_ontology:cs_axiom_status(dsm_categories_map_neurobiological_entities, holdable).
narrative_ontology:cs_axiom_grounding('c2ad58cb-b5c7-458f-b0ca-86f6418f778b', dsm_categories_map_neurobiological_entities, empirically_contingent).
narrative_ontology:cs_axiom('c2ad58cb-b5c7-458f-b0ca-86f6418f778b', foundational, diagnosis_justifies_medical_intervention).
narrative_ontology:cs_axiom_status(diagnosis_justifies_medical_intervention, holdable).
narrative_ontology:cs_axiom_grounding('c2ad58cb-b5c7-458f-b0ca-86f6418f778b', diagnosis_justifies_medical_intervention, deontological).
narrative_ontology:cs_reference_frame('c2ad58cb-b5c7-458f-b0ca-86f6418f778b', objective_neurobiological_disease_classification).
narrative_ontology:cs_drift_state('c2ad58cb-b5c7-458f-b0ca-86f6418f778b', contemporary_neuroscience_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c2ad58cb-b5c7-458f-b0ca-86f6418f778b', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, institutional_conformity_enforcers).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, diagnostic_threshold_population).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, involuntary_treatment_subjects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, research_community).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, service_users_seeking_explanation).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_manufacturers).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, service_users_seeking_explanation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% DSM committees authoritatively define diagnostic categories, set threshold criteria, and revise the manual every ~5–10 years. Frames categories as discovered disease entities grounded in emerging neurobiology. Controls research funding, training curricula, and professional legitimacy through gatekeeping who gets credentialed to diagnose and treat. Collects prestige, career advancement, and gatekeeping power over the category system itself.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment, agenda_setter,
    institutional, generational, arbitrage, global).

% Each newly formalized DSM category creates a patient population for targeted drug development and marketing. Fund psychiatric research, sponsor medical education, and shape diagnostic framing toward pharmacological intervention. Their business model depends on DSM categories remaining stable enough to support treatment indications while new categories emerge to open new markets. Direct beneficiary of the category expansion trajectory.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_manufacturers, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_manufacturers, payer).

% Individuals who meet diagnostic criteria (or are borderline) under current DSM thresholds. Once diagnosed, subject to pharmacological intervention, loss of legal capacity (driver's license, custody, employment), mandatory treatment in institutional settings, and permanent record annotations that shape access to employment, housing, and social participation. Exit from the diagnosed category requires disproving the diagnosis itself—a structural impossibility if the category is treated as objective fact.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, diagnostic_threshold_population, payer,
    powerless, biographical, identity_locked, global).

% Individuals involuntarily committed, medicated without consent, or subjected to restraint/seclusion on grounds that their diagnosed condition justifies coercive intervention. The DSM diagnosis is the legal and clinical justification for removing bodily autonomy. Exit requires proving non-dangerousness or restoration of competency—thresholds the treatment system itself controls.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, involuntary_treatment_subjects, payer,
    powerless, immediate, trapped, national).

% Schools, workplaces, military, prisons, family courts use DSM diagnoses to justify behavioral intervention: medicalization of noncompliance, allowing coercive correction without legal authorization as punishment. A child diagnosed with ADHD or ODD becomes medicatable rather than disciplinable; a worker with depression or anxiety becomes treatable rather than resistant; a prisoner with antisocial personality disorder becomes incapacitating rather than rebellious. The DSM provides a legitimacy frame for conformity enforcement.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, institutional_conformity_enforcers, beneficiary,
    institutional, generational, analytical, national).

% Neuroscientists, geneticists, and neuroimaging researchers whose funding, publication prestige, and career advancement depend on finding biological markers for DSM categories. Frames categories as yet-to-be-fully-understood neurobiological disorders amenable to empirical discovery. Publishes studies claiming to locate neural correlates, genetic loci, or biomarkers. Career incentive is maximum: funding follows disease discovery; funding is unavailable for social construction hypotheses.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, research_community, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__biomedical_reading, research_community, agenda_setter).

% Individuals diagnosed with autism, ADHD, dyslexia, and other neurodevelopmental variations who reject the disease framing and argue for acceptance of neurological diversity as natural human variation rather than pathology. Their frame (natural variation) contradicts the biomedical reading's core premise (objective disease). Systematically excluded from DSM revision committees and research agendas; their testimony appears as advocacy, not science.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, neurodiversity_advocates, excluded,
    powerless, biographical, constrained, global).

% Psychiatrists and social scientists who argue DSM categories are reverse-engineered from drug availability (pharmaceutical influence) rather than discovered through biology; that the category system serves market expansion and institutional control more than patient welfare. Systematically underfunded; excluded from major psychiatric publications and professional leadership; reframed as ideological rather than scientific.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, critical_psychiatry_scholars, excluded,
    moderate, biographical, constrained, global).

% Individuals experiencing suffering, distress, or behavioral difficulty who seek explanation and intervention. The DSM reading offers a coherent narrative: your suffering is a disease, it has a biological basis, treatment can fix it. This framing can be genuinely relieving (disease externalizes blame, reduces stigma) and genuinely harmful (disease legitimizes coercive intervention, permanence of diagnosis, side effects of medication). They bear the cost of living inside the framing whether it helps or harms.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, service_users_seeking_explanation, beneficiary,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__biomedical_reading, service_users_seeking_explanation, payer).

% Pharmaceutical regulatory agencies (FDA, EMA) and antitrust bodies. Evaluate whether DSM categories are scientifically grounded or marketing tools; whether psychiatric diagnostic authority is independent or captured by pharmaceutical interests; whether exclusion of alternative readings from professional legitimacy constitutes anticompetitive gatekeeping.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, competition_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment).
narrative_ontology:fixing_cost_class(dsm_taxonomy_kernel__biomedical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified, standardized language for clinicians, researchers, and institutions to communicate about mental distress and behavioral difficulty; enables research coordination across sites; centralizes training in psychiatric diagnosis under one authoritative framework rather than dozens of idiosyncratic local approaches.
% TRANSFER_FUNCTION: Moves bodily autonomy, legal capacity, and freedom from coercive intervention FROM diagnosed individuals and involuntary treatment subjects TO the psychiatric establishment, pharmaceutical manufacturers, and institutional conformity enforcers. Moves research funding and prestige FROM funding bodies TO the psychiatric research community and DSM-aligned investigators. Moves market share FROM generic behavioral support TO branded pharmaceutical products.
% ABSENT_VOICES: Neurodiversity advocates and critical psychiatry scholars are structurally excluded from DSM revision committees and major psychiatric publications. They would testify that the biomedical reading misframes social suffering as individual disease, that pharmaceutical influence shapes category construction, and that alternative readings exist with different implications for treatment and social policy. Their absence from the table means disagreement is defined out of science.
% DISAPPEARANCE_RATIONALE: If the DSM biomedical reading disappeared overnight—replaced by explicit acknowledgment that categories are socially constructed, that alternative readings exist (neurodiversity, critical psychiatry), and that diagnosis does not warrant involuntary treatment—psychiatry would bifurcate: voluntary supportive interventions for those seeking help, and profound contraction of involuntary commitment and coercive medication. Pharmaceutical markets would shrink as diagnostic expansion slowed. Institutional conformity enforcement would lose its medical legitimacy frame. The world would rearrange around explicit negotiation of treatment goals rather than default-to-pharmacology-as-cure.
% FOUNDING_PROBLEM: In mid-20th century, psychiatry lacked a unified diagnostic framework: clinicians used different terminology, hospitals operated under different classification systems, research was fragmented and incomparable. The DSM was created to solve this coordination problem—a single, authoritative manual that would enable training, research, and clinical communication across institutions.
% FOUNDING_PROBLEM_CORROBORATION: The psychiatric establishment attests the founding problem (fragmentation) was real and remains partially unsolved; coordination via DSM is essential for modern psychiatry. However, neurodiversity advocates, critical psychiatry scholars, and independent epidemiologists outside the psychiatric discipline attest that (1) the founding problem of fragmentation has been substantially solved (ICD codes provide sufficient clinical communication), and (2) the DSM has evolved beyond coordination toward market expansion and institutional control—the founding problem is no longer the primary function the system serves.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__biomedical_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__biomedical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__biomedical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dsm_taxonomy_kernel__biomedical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__biomedical_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dsm_taxonomy_kernel__biomedical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dsm_taxonomy_kernel__biomedical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dsm_taxonomy_kernel__biomedical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness reaches 0.78 at interval end because the biomedical reading justifies removing autonomy (involuntary treatment, coercive medication, loss of driver's license/custody/employment access) from individuals deemed to meet diagnostic criteria—the extraction is authorized by the diagnosis itself. The constraint is active enforcement: thresholds must be maintained, alternatives excluded, diagnostic expansion controlled to sustain beneficiary rents. Suppression is high (0.72) because the reading persists despite contradictory evidence (neural correlates appear post-diagnosis, not pre-diagnosis; alternative frameworks exist but are excluded from professional legitimacy; pharmaceutical influence shapes category boundaries) and despite harm documented by excluded voices. Theater_ratio rises from 0.18 to 0.41 over the interval because scientific justification (biomarker discovery) increasingly fails to match the category expansion trajectory—the system continues category proliferation and diagnostic broadening despite lack of neurobiological discovery, suggesting theatrical maintenance (going through the motions of biological research while the actual function is institutional control and pharmaceutical market expansion). The measurement series track a constraint whose extractive function increasingly dominates its coordination function. All metrics authored on a single shared time grid (intervals 0, 5, 10, 15, 20, 25, 30, 40) so temporal analysis has coherent data across all three series.
 *
 * PERSPECTIVAL GAP:
 *   From the psychiatric establishment and research community seats, the DSM is genuine coordination (unified language for communication, standardized training, comparable research)—they experience it as a professional achievement and increasingly accurate description of neurobiological reality. From the diagnostic threshold population and involuntary treatment subjects, the same structure operates as enforced extraction: diagnosis → loss of autonomy → coercive intervention → permanent status → identity-lock. The engine computes this divergence from the structural data: different power atoms (institutional vs. powerless), different exit_options (arbitrage vs. identity_locked/trapped), and opposite roles (agenda_setter vs. payer) automatically produce different directionality values, hence different per-seat type computations. The payer seat experiences snare; the agenda_setter seat experiences rope. This divergence is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Psychiatric establishment and research community: d near beneficiary end (0.1–0.2 range). They set the agenda, collect prestige and resources, face no extraction. Their exit_options are arbitrage (can leave psychiatry and join other profitable fields; no identity-lock). Power is institutional. Pharmaceutical manufacturers: d near beneficiary end (0.15–0.25). They collect directly from market expansion driven by DSM categories; their exit is arbitrage (can leave pharmaceuticals for other markets). Power is institutional. Diagnostic threshold population: d near target end (0.8–0.9). They are subject to coercive intervention authorized by diagnosis; their exit_options are identity_locked (diagnosis becomes their permanent status; disproving it is structurally impossible if the category is treated as objective fact). Power is powerless. Involuntary treatment subjects: d at full target end (0.95–1.0). They face immediate coercive removal of bodily autonomy; their exit_options are trapped (legal and institutional barriers to leaving treatment, with exit conditions controlled by the treatment system). Power is powerless. Institutional conformity enforcers: d in moderate-beneficiary range (0.25–0.35). They benefit from DSM diagnosis as a legitimacy frame for behavioral intervention without legal authorization for punishment; they are not direct extractors but secondary beneficiaries of the constraint's enforcement machinery. Power is institutional. Research community and neurodiversity advocates: moderate-to-low power; moderate exit_options (constrained mobility in their fields if they adopt alternative readings). Service users: d in symmetric range (0.45–0.55). They receive genuine benefit (explanation, relief from blame externalization) and genuine cost (living inside disease narrative, exposure to pharmacological side effects, permanent record annotations). Power is powerless but exit_options are somewhat more mobile than the diagnostic threshold population because they may still have choice about treatment uptake (not always true for involuntary subjects). No directionality overrides are needed; the structural derivation from beneficiary/victim + exit + power produces accurate directionality values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (diagnostic fragmentation across institutions) was real and justified DSM creation. However, founding_problem_status is contested: the psychiatric establishment claims the problem is live (fragmentation persists, standardization is ongoing), while critical psychiatry scholars and epidemiologists outside the discipline attest the problem is substantially solved (ICD codes provide sufficient coordination; DSM expansion beyond coordination toward market expansion and institutional control is the contemporary function). The disappearance_verdict is world_rearranges—the constraint is not natural law; it is a social/institutional arrangement whose persistence depends on active enforcement by the psychiatric establishment and pharmaceutical industry. This divergence (founding problem status=contested, disappearance_verdict=world_rearranges) indicates the constraint may be experiencing mandatrophy: its original justification no longer tracks its actual operation. The theater_ratio rise from 0.18 to 0.41 supports this: more of the constraint's operation is theatrical (going through the motions of biological research, revision process, professional legitimacy maintenance) rather than functional (solving the founding coordination problem). The classification remains tangled_rope (genuine coordination function + asymmetric extraction) rather than piton (atrophied function maintained purely theatrically) because the coordination benefit to the psychiatric establishment and research community is substantial and real—it is not purely theatrical for them. But from the payer seats (diagnostic threshold population, involuntary treatment subjects), the constraint increasingly operates as snare (extraction disguised as coordination). This per-seat divergence is exactly what the mandatrophy analysis should detect: a constraint whose stated function (coordination) no longer explains its persistence for certain seats, while other seats continue collecting real benefits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biomarker_discovery_frontier,
    'Are DSM categories mapping to discoverable neurobiological entities, or are researchers finding correlates of the social construction (diagnosis → behavior change → neural adaptation → observed correlation)?',
    'Prospective neuroimaging studies on pre-diagnosis, undiagnosed individuals meeting symptom criteria who receive no intervention; comparison of neural patterns across cultures where the same behaviors are not medicalized; longitudinal tracking of whether neural differences precede diagnosis or follow it.',
    'If neurobiological differences precede diagnosis, the biomedical reading''s core claim holds. If neural correlates emerge post-diagnosis (via behavioral feedback, medication effects, identity integration), the reading conflates observation with causation and becomes a critical psychiatry reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(biomarker_discovery_frontier, empirical, 'Whether observed neural correlates represent discovered disease entities or constructed artifacts of the diagnostic process itself.').

omega_variable(
    kernel_contest_framing,
    'Is the DSM a discovered taxonomy of objective disease entities, a socially constructed category system serving institutional and pharmaceutical interests, or a natural-variation framework pathologizing human diversity?',
    'This is the irreducible committer-frame ambiguity: three different readings of the same kernel (DSM manual) instantiate three different constraints with different ε, beneficiary/victim structures, and policy implications. No empirical test resolves which reading is ''correct''—the contest is over what counts as disease, who gets to decide, and whether diagnosis warrants coercion.',
    'The biomedical reading alone justifies involuntary treatment, pharmaceutical intervention, and loss of legal capacity. The neurodiversity reading justifies accommodations and support without pathology framing. The critical psychiatry reading justifies regulatory scrutiny of pharmaceutical influence and DSM revision process transparency. The reading adopted shapes institutional power, harm distribution, and treatment policy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_framing, conceptual, 'The kernel contest: which reading of the DSM—biomedical, neurodiversity, or critical psychiatry—instantiates the true constraint.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.72) structural—external barriers to alternative treatment, loss of legal capacity—or internalized—diagnosed individuals internalizing disease identity, absorbing shame, losing belief in alternative possibilities?',
    'Post-diagnosis qualitative research on how long internalized suppression persists after individuals exit the diagnostic category or gain access to alternative frameworks; longitudinal tracking of self-efficacy and identity shift in individuals who reject DSM diagnosis after initial acceptance.',
    'Structural suppression is imposed externally and can be removed by changing institutions. Internalized suppression persists after external barriers dissolve; it represents a deeper form of identity-lock and would require identity-reconstruction work to reverse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression in the diagnostic context is structural (legal/institutional) or internalized (identity/belief).').

omega_variable(
    pharmaceutical_capture_degree,
    'To what degree does pharmaceutical industry influence shape DSM category definitions, threshold criteria, and revision priorities?',
    'Financial disclosure analysis (researcher funding from manufacturers), publication bias analysis (likelihood of publication by finding direction), natural experiment from jurisdictions that exclude pharmaceutical industry from DSM-equivalent processes, longitudinal tracking of category expansion aligned with drug availability.',
    'High pharmaceutical influence would reclassify the constraint from tangled_rope (genuine coordination + extraction) to snare (extraction disguised as coordination). Low influence would support the biomedical reading''s independence claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pharmaceutical_capture_degree, empirical, 'Degree of pharmaceutical industry capture in DSM category construction and revision.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__biomedical_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t0, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(dsm__tr_t0, projected).
narrative_ontology:measurement(dsm__tr_t5, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement_basis(dsm__tr_t5, observed).
narrative_ontology:measurement(dsm__tr_t10, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement_basis(dsm__tr_t10, observed).
narrative_ontology:measurement(dsm__tr_t15, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 15, 0.33).
narrative_ontology:measurement_basis(dsm__tr_t15, observed).
narrative_ontology:measurement(dsm__tr_t20, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 20, 0.37).
narrative_ontology:measurement_basis(dsm__tr_t20, observed).
narrative_ontology:measurement(dsm__tr_t25, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 25, 0.39).
narrative_ontology:measurement_basis(dsm__tr_t25, observed).
narrative_ontology:measurement(dsm__tr_t30, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement_basis(dsm__tr_t30, observed).
narrative_ontology:measurement(dsm__tr_t40, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(dsm__tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(dsm__be_t0, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(dsm__be_t0, projected).
narrative_ontology:measurement(dsm__be_t5, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(dsm__be_t5, observed).
narrative_ontology:measurement(dsm__be_t10, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(dsm__be_t10, observed).
narrative_ontology:measurement(dsm__be_t15, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement_basis(dsm__be_t15, observed).
narrative_ontology:measurement(dsm__be_t20, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 20, 0.73).
narrative_ontology:measurement_basis(dsm__be_t20, observed).
narrative_ontology:measurement(dsm__be_t25, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 25, 0.75).
narrative_ontology:measurement_basis(dsm__be_t25, observed).
narrative_ontology:measurement(dsm__be_t30, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 30, 0.77).
narrative_ontology:measurement_basis(dsm__be_t30, observed).
narrative_ontology:measurement(dsm__be_t40, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement_basis(dsm__be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t0, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(dsm__su_t0, projected).
narrative_ontology:measurement(dsm__su_t5, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement_basis(dsm__su_t5, observed).
narrative_ontology:measurement(dsm__su_t10, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(dsm__su_t10, observed).
narrative_ontology:measurement(dsm__su_t15, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(dsm__su_t15, observed).
narrative_ontology:measurement(dsm__su_t20, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(dsm__su_t20, observed).
narrative_ontology:measurement(dsm__su_t25, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(dsm__su_t25, observed).
narrative_ontology:measurement(dsm__su_t30, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(dsm__su_t30, observed).
narrative_ontology:measurement(dsm__su_t40, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(dsm__su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__biomedical_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(dsm_taxonomy_kernel__biomedical_reading, 0.12).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel__neurodiversity_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel__critical_psychiatry_reading).

% DUAL FORMULATION NOTE:
% The DSM taxonomy kernel decomposes into three constraint stories per ε-invariance principle: biomedical_reading (objective disease discovery, high extractiveness), neurodiversity_reading (natural variation pathologized, high institutional control), critical_psychiatry_reading (pharmaceutical market construction, high capture). These are not the same constraint viewed from different angles—they are different claims about what the kernel IS, with structurally different beneficiary/victim configurations and ε values. The three readings coexist in academic/clinical discourse but instantiate incompatible constraints. Network edges link them as a constraint family; each story's cs_structure.reading_relations entries document the structural relationships between readings (coexists_with, influences, forecloses).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dsm_taxonomy_kernel__biomedical_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
