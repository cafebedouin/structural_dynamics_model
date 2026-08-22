% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__biomedical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: DSM Biomedical Taxonomy (Objective Disease Mapping Reading)
 *   domain: medical/psychiatric/epistemological
 *
 * SUMMARY:
 *   The DSM (Diagnostic and Statistical Manual) is presented by the
 *   psychiatric establishment as a scientifically grounded taxonomy mapping
 *   observable symptom clusters to underlying neurobiological disease
 *   entities. This is the biomedical reading: DSM categories are discoveries,
 *   not inventions; they map to objective pathology. The constraint operates
 *   by embedding this reading into clinical practice, insurance
 *   reimbursement, involuntary commitment law, pharmaceutical markets, and
 *   institutional conformity enforcement. The reading enables
 *   high-extractiveness operation because it justifies intervention (drug
 *   treatment, hospitalization, legal incapacity) as medical necessity rather
 *   than social control. However, alternative readings exist: critical
 *   psychiatry scholars document pharmaceutical influence on diagnostic
 *   expansion, and neurodiversity advocates argue DSM categories pathologize
 *   neurotype variation rather than discover disease. This constraint story
 *   instantiates ONLY the biomedical reading as a clean ε-invariant
 *   constraint. The alternative readings are separate constraint stories in
 *   the same kernel family, with their own ε values and beneficiary/victim
 *   structures.
 *
 * KEY AGENTS:
 *   - dsm_category_diagnosed_persons — structural target bearing labeling, medication, legal incapacity, identity fusion; exit is identity-locked (professional, relational, ideological identity fused with disease status)
 *   - psychiatric_establishment — institutional agenda-setter and beneficiary; controls diagnostic criteria, trains practitioners, validates the biomedical frame; existence depends on reading's persistence
 *   - pharmaceutical_industry — powerful beneficiary; each DSM diagnosis is a market segment; revenue tied to diagnostic expansion and threshold-lowering
 *   - institutional_conformity_enforcers — organized beneficiary (schools, workplaces, military, child services); biomedical frame allows them to pathologize noncompliance as disease rather than engage institutional legitimacy questions
 *   - neurodiversity_advocates — excluded from diagnosis-setting, positioned as patients rather than knowledge-producers; alternative reading rejected at institutional level
 *   - critical_psychiatry_scholars — observer seats with mobility; research documenting pharmaceutical influence and diagnostic validity challenges faces funding and publication barriers
 *   - regulatory_authorities — observer-agenda-setter hybrids; embed DSM categories into law and reimbursement, treating them as objective; changes cascade into legal and economic systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__biomedical_reading, 0.81).
domain_priors:suppression_score(dsm_taxonomy_kernel__biomedical_reading, 0.76).
domain_priors:theater_ratio(dsm_taxonomy_kernel__biomedical_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, resistance, 0.57).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__biomedical_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__biomedical_reading, "DSM Biomedical Taxonomy (Objective Disease Mapping Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__biomedical_reading, "medical/psychiatric/epistemological").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__biomedical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__biomedical_reading, '223ea1ff-5e01-42ae-bc94-31c27c782b49').
narrative_ontology:cs_kernel_codification('223ea1ff-5e01-42ae-bc94-31c27c782b49', fixed_text).
narrative_ontology:cs_authority_grounding('223ea1ff-5e01-42ae-bc94-31c27c782b49', extraction).
narrative_ontology:cs_interpretation_layer_present('223ea1ff-5e01-42ae-bc94-31c27c782b49').
narrative_ontology:cs_reading_relation('223ea1ff-5e01-42ae-bc94-31c27c782b49', dsm_taxonomy_kernel__critical_psychiatry_reading, coexists_with).
narrative_ontology:cs_reading_relation('223ea1ff-5e01-42ae-bc94-31c27c782b49', dsm_taxonomy_kernel__neurodiversity_reading, coexists_with).
narrative_ontology:cs_axiom('223ea1ff-5e01-42ae-bc94-31c27c782b49', foundational, dsm_categories_map_neurobiological_entities).
narrative_ontology:cs_axiom_status(dsm_categories_map_neurobiological_entities, holdable).
narrative_ontology:cs_axiom_grounding('223ea1ff-5e01-42ae-bc94-31c27c782b49', dsm_categories_map_neurobiological_entities, empirically_contingent).
narrative_ontology:cs_axiom('223ea1ff-5e01-42ae-bc94-31c27c782b49', foundational, psychiatric_diagnosis_justifies_medical_intervention).
narrative_ontology:cs_axiom_status(psychiatric_diagnosis_justifies_medical_intervention, holdable).
narrative_ontology:cs_axiom_grounding('223ea1ff-5e01-42ae-bc94-31c27c782b49', psychiatric_diagnosis_justifies_medical_intervention, instrumental).
narrative_ontology:cs_reference_frame('223ea1ff-5e01-42ae-bc94-31c27c782b49', objective_neurobiological_disease_discovery).
narrative_ontology:cs_drift_state('223ea1ff-5e01-42ae-bc94-31c27c782b49', contemporary_diagnostic_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('223ea1ff-5e01-42ae-bc94-31c27c782b49', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, institutional_conformity_enforcers).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, dsm_category_diagnosed_persons).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, involuntary_treatment_subjects).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, medication_exposed_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals whose behavior or experience meets DSM diagnostic criteria. They receive psychiatric labels, pharmacological interventions (often involuntary), and legal/occupational consequences (loss of custody, voting rights, professional licensure). Their identity becomes fused with disease status—a teacher diagnosed with depression cannot separate 'I experience sadness' from 'I am a depressed person' without risking identity incoherence. Exit from the diagnosed category requires either denying reported experience (which is psychologically costly) or mounting expensive legal challenge to psychiatric authority (which is administratively difficult). The reading treats them as disease-bearers requiring medical intervention.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, dsm_category_diagnosed_persons, payer,
    powerless, biographical, identity_locked, global).

% Academic psychiatry, psychiatric professional associations, psychiatric training institutions, and the broader mental-health research establishment. They set diagnostic criteria through DSM task forces, validate the biomedical frame through peer-reviewed research, control access to professional credentials, determine which diagnoses insurance will reimburse, and influence regulatory policy. They benefit from institutional authority (psychiatry is medicine, not social control), research funding (NIH funds assume DSM validity), and educational infrastructure (every psychiatric trainee learns the DSM as scientific fact). Their authority depends on the reading's persistence: if DSM categories are revealed as administrative conveniences rather than discoveries, psychiatric authority shifts from medical science to institutional enforcement.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment, beneficiary).

% Companies developing and marketing psychotropic medications (SSRIs, antipsychotics, stimulants, anxiolytics). Each DSM diagnosis is a market segment and potential revenue stream. They benefit from: diagnostic expansion (wider criteria = larger addressable market), threshold-lowering (milder cases → more patients), and new diagnoses (each added condition creates a new market). Their pharmaceutical sales data show revenue correlates with diagnostic prevalence expansion—as major depression prevalence estimates rose from 2% to 15%, antidepressant revenue grew proportionally. The biomedical reading—that DSM categories are objective disease entities requiring pharmaceutical treatment—directly justifies their market expansion and blocks competing narratives (that the same conditions could be addressed through social, behavioral, or community interventions).
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_industry, beneficiary,
    powerful, biographical, arbitrage, global).

% Schools, workplaces, military, criminal justice, child protective services, and other institutions managing populations. They benefit from the biomedical frame by converting noncompliance, inattention, or norm deviation into medical diagnoses requiring intervention. A child who challenges authority can be diagnosed with oppositional defiant disorder; a soldier who questions orders can be diagnosed with an adjustment disorder; a prisoner who resists institutional control can be diagnosed with antisocial personality disorder. The biomedical reading allows these institutions to pathologize resistance as disease rather than as legitimate objection or institutional malfunction. They are constrained because their institutional logic now depends on the frame (they've built conformity enforcement on top of diagnosis).
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, institutional_conformity_enforcers, beneficiary,
    organized, generational, constrained, global).

% Communities of autistic, ADHD, deaf, and otherwise neurodivergent people. They assert that neurological variation is not disease and that DSM categories pathologize difference rather than discover pathology. They are excluded from: official DSM revision task forces (which are composed of academic psychiatrists, not neurodivergent people); major psychiatric research funding streams (NIH money assumes DSM validity, not variation); and professional credential systems (psychiatrists diagnose variation as disorder). They occupy a structural position as diagnosed patients rather than as knowledge-producers. Their alternative reading—that variation is natural and institutional accommodation is the solution, not medical treatment—directly contradicts the biomedical reading and is not represented in official taxonomy-setting.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, neurodiversity_advocates, excluded,
    moderate, biographical, constrained, global).

% Researchers and clinicians (psychiatrists, psychologists, sociologists, historians of medicine) who document pharmaceutical influence on DSM construction, critique the neurobiological validity of diagnostic categories, and challenge the expansion of psychiatric jurisdiction. They occupy a precarious position: their research directly contradicts beneficiary interests (the psychiatric establishment, pharmaceutical companies), making funding and publication difficult. They retain some mobility because academia provides alternative institutional shelter (sociology, history departments, some psychology programs), but advancement in psychiatry or psychology proper is blocked. Their work corroborates the critical_psychiatry_reading but is systematically marginalized in mainstream psychiatry.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, critical_psychiatry_scholars, observer,
    moderate, biographical, mobile, global).

% U.S. National Institutes of Mental Health, National Science Foundation, European funding councils, and pharmaceutical company research divisions. They fund psychiatric research conditional on accepting DSM framework—grant applications assume DSM categories are valid biological entities and fund research to discover their neurobiological basis. This funding structure creates a selection effect: research assumes the biomedical reading is true and funds the search for biomarkers that would confirm it. Alternative readings (that DSM categories are administrative or socially constructed) are less fundable because they don't promise the discovery of biological targets for intervention. Their funding allocation reinforces the biomedical reading by making it the default research paradigm.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, research_funding_bodies, agenda_setter,
    institutional, generational, analytical, global).

% FDA, EMA, national health authorities, and insurance regulators. They treat DSM categories as objective disease entities for drug approval, insurance reimbursement, involuntary commitment, and disability determination. A diagnosis automatically triggers: drug eligibility (FDA approves drugs for DSM diagnoses), insurance coverage (treatment of DSM diagnoses is reimbursable), and legal consequences (involuntary hospitalization requires DSM diagnosis). Their regulatory embedding makes DSM categories functionally objective—whether or not they reflect real neurobiological entities, they determine material consequences. Changes to DSM criteria flow directly into law and policy. They are observer seats because they don't set the DSM criteria (psychiatry does), but they operationalize those criteria into law.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, regulatory_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment).
narrative_ontology:fixing_cost_class(dsm_taxonomy_kernel__biomedical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared diagnostic language enabling communication between clinicians, researchers, insurance systems, and institutional authorities about mental illness. Without a common taxonomy, different clinicians might label the same presentation differently; standardization solves the coordination problem of 'what counts as depression' across jurisdictions and practitioners.
% TRANSFER_FUNCTION: Moves institutional authority, professional status, treatment access, and pharmaceutical profit from diagnosed individuals to the psychiatric establishment, pharmaceutical industry, and conformity-enforcing institutions. The constraint extracts legitimacy from those diagnosed (their self-description as diseased) and clinical autonomy from practitioners (who must diagnose using the DSM frame to bill insurance and justify treatment).
% ABSENT_VOICES: Neurodiversity advocates and people with lived experience of being diagnosed should shape diagnostic criteria but are systematically excluded from DSM revision task forces (which are dominated by academic psychiatrists and pharmaceutical-affiliated researchers). Critical psychiatry scholars document pharmaceutical influence on diagnosis but are marginalized in mainstream psychiatry and face funding and publication barriers. Alternative frameworks—disability justice, neurodiversity, social constructionist psychiatry—are not seated in the official taxonomy-building process.
% DISAPPEARANCE_RATIONALE: If the biomedical reading and the DSM disappeared overnight, psychiatric practice would reorganize: involuntary hospitalization would lose its scientific justification (no diagnosis = no commitment grounds); pharmaceutical markets would collapse (no diagnosis = no indication for drugs); institutional conformity enforcement would require explicit behavioral control rather than medical camouflage; research would fragment into local, theory-specific frameworks. The world would rearrange because the constraint's removal would expose the administrative and economic systems that currently hide behind diagnosis.
% FOUNDING_PROBLEM: Early psychiatry lacked a systematic way to distinguish mental illness from moral failing, crime, or simple eccentricity. A patient presenting with persistent sadness and anhedonia needed a name, a classification, and a rationale for treatment distinct from punishment or moral correction. DSM provided that language: disease, not sin.
% FOUNDING_PROBLEM_CORROBORATION: The psychiatric establishment attests the founding problem is still live, citing continued diagnostic uncertainty and the need for refined criteria. Neurodiversity advocates and critical psychiatry scholars attest the founding problem is substantially solved—clinicians can now identify genuine neurological distress—but that the biomedical reading has metastasized beyond its legitimate scope to pathologize variation and normal human suffering. Legislative and judicial bodies investigating psychiatric overreach (Canada's recent hearings on psychiatric hospitalization, UK court challenges to autism diagnosis) corroborate the shifted-function reading: the constraint now does more administrative conformity enforcement than disease identification.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__biomedical_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__biomedical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__biomedical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dsm_taxonomy_kernel__biomedical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__biomedical_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness measures how much the constraint extracts from those it governs. At 0.81 (interval end), it is high because the biomedical reading justifies interventions (involuntary hospitalization, forced medication, loss of legal capacity) presented as medical necessity. The reading transforms social control into healthcare. The 72-year measurement series documents accumulating extraction: in 1952, DSM-I was narrower, diagnostic thresholds higher, and institutional expansion slower; by 2024, DSM criteria have expanded dramatically (major depression lifetime prevalence went from ~2% in 1980s to 15%+ today; ADHD went from a childhood disorder affecting ~5% of children to an adult diagnosis affecting 4%+ of adults), pharmaceutical markets have grown correspondingly, and institutional dependence on diagnosis for conformity enforcement is pervasive. Suppression measures coercion and lack of alternatives. At 0.76 it is very high because: (1) once diagnosed, exit is nearly impossible without denying reported experience or mounting expensive legal challenge; (2) institutional systems (insurance, schools, workplaces) treat DSM categories as obligatory facts, not optional framings; (3) the biomedical reading monopolizes professional legitimacy—alternative frameworks are funded poorly, published slowly, and excluded from official taxonomy-setting. Theater ratio at 0.42 indicates substantial performative activity: diagnostic manuals are updated with ceremonies of scientific rigor (task forces, literature reviews, voting), but documented history shows the process is influenced by pharmaceutical marketing, insurance reimbursement incentives, and institutional interest in expanding the disease net. The core disease-discovery function is real, but a growing share of activity defends the biomedical frame against alternatives. The measurement series show monotonic increase in extractiveness, theater, and suppression over 72 years—the constraint's extractive footprint has grown as the DSM has expanded and become more embedded in institutional practice.
 *
 * PERSPECTIVAL GAP:
 *   This constraint should compute very differently across seats. From the diagnosed person's seat: high extraction, high suppression, low exit options, identity-locked closure. From the psychiatric establishment's seat: genuine coordination (shared language for clinicians), coupled with real professional benefit and institutional expansion. From the pharmaceutical beneficiary seat: a highly extractive, actively maintained market. From the regulatory authority seat: a useful organizational tool, apparently neutral technical standard. The engine should compute these divergences from the structural data: beneficiary vs. victim declarations, power asymmetries, exit-option constraints. The authored claim (tangled_rope) reflects the reading's own commitment: there IS real coordination (shared diagnostic language) coupled with asymmetric extraction (diagnosis justifies interventions that flow toward institutional authority and away from diagnosed persons). Disagree, and you have a different reading (the critical reading sees mostly extraction with coordination as cover; the neurodiversity reading sees no coordination, only pathologization).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations and exit constraints. Diagnosed persons are victims because: (1) they are explicitly named in victims[]; (2) they bear the interventions; (3) their exit is identity-locked (professional, relational, or ideological identity fused with disease status—if diagnosed, the self-concept becomes incoherent without the disease frame). Their d-value should be near 1.0 (full target). The psychiatric establishment is beneficiary-agenda-setter: (1) explicit beneficiary; (2) sets the rules; (3) exit is high (they could shift to alternative frameworks, but the current reading sustains institutional resources). Their d-value should be near 0.0 (full beneficiary). The pharmaceutical industry is powerful beneficiary: explicit beneficiary, arbitrage exit options (they could exit by developing non-pharmaceutical treatments, but the biomedical reading guarantees drug markets). The institutional conformity enforcers are organized beneficiaries with constrained exit: they benefit from the reading (it allows them to pathologize noncompliance as disease) but cannot easily exit (their institutional logic depends on the biomedical frame now). Neurodiversity advocates are excluded: they would dispute the entire reading and are locked out of agenda-setting, creating high suppression for their alternative.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy (mandate outliving function) is not clearly present. The founding problem—distinguishing genuine mental illness from moral failing—remains live for genuine cases. However, the constraint shows signs of mandate slippage: the founding problem justifies diagnosing someone with severe, persistent depression; it does NOT justify expanding criteria to include 'minor depression' (sadness + low motivation for 2 weeks) or diagnosing ADHD in inattentive children whose inattention reflects normal developmental variation or institutional mismatch (a child bored by an unstimulating school is not necessarily diseased). The measurement series document expansion beyond the founding problem's scope: diagnostic thresholds have lowered, age-of-onset criteria have dropped, and symptom criteria have broadened. This is mandate creep: the same justification (discovering neurobiological disease) now applies to populations where the founding problem does not actually exist. The constraint persists through: (1) institutional embedding (insurance, law, education depend on DSM categories now); (2) pharmaceutical profit (expansion = new markets); (3) institutional conformity benefit (pathologizing noncompliance is easier than addressing institutional legitimacy). Tangled Rope classification is appropriate: there IS real coordination (shared language), but the active enforcement (suppressing alternatives, embedding in law, expanding criteria) is increasingly visible and increasingly asymmetric.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biomedical_validity_of_dsm_categories,
    'Do DSM categories map to discrete neurobiological disease entities, or are they pragmatic administrative groupings without clear neurobiological boundaries?',
    'Neurobiological research discovering biomarkers, genetic loci, or neural circuit pathology specific to DSM categories; or alternatively, demonstration that DSM categories do not cluster on neural or genetic dimensions (that they are administratively convenient but neurobiologically arbitrary).',
    'If discrete biomarkers exist, the biomedical reading is vindicated and extraction is genuinely coordinating medical science around discovery. If not, the reading becomes a false natural law (mountain → tangled_rope / snare shift), and the constraint''s extraction lacks epistemic justification. High-impact for foundation of the constraint''s claimed type.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(biomedical_validity_of_dsm_categories, empirical, 'Whether DSM categories reflect objective neurobiological entities or pragmatic administrative groupings.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'To what degree is the suppression of alternative readings (neurodiversity, critical psychiatry) structural (funding barriers, publication gatekeeping, exclusion from task forces) versus internalized (clinicians and researchers have adopted the biomedical frame as epistemic fact)?',
    'Post-removal suppression trajectory: if alternative frameworks gain rapid traction after institutional barriers are lowered (funding opens, publication gates ease), suppression was primarily structural. If alternative frameworks remain marginal despite open institutional access, suppression is primarily internalized (clinicians treat biomedical frame as obviously true, not as a reading).',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests—the frame persists through belief, not coercion. If structural, removing funding and publication barriers would enable competing readings. Affects assessment of whether constraint is ''natural'' (internalized = appears natural) versus ''constructed'' (structural = removable by policy change).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural versus internalized suppression of alternative diagnostic readings.').

omega_variable(
    pharmaceutical_influence_on_diagnostic_expansion,
    'To what degree have diagnostic thresholds and symptom criteria been influenced by pharmaceutical industry marketing and profit incentives, versus what degree reflects genuine clinical observation of broader disease prevalence?',
    'Historical analysis comparing DSM revisions to pharmaceutical market development timelines, funding sources of DSM revision task forces, and comparative analysis across countries with different pharmaceutical regulatory regimes (does major depression prevalence differ systematically where antidepressant markets are regulated differently?).',
    'Evidence of pharmaceutical influence would support the critical_psychiatry_reading and increase ε (constraint is reverse-engineered from drug availability, not disease discovery). Evidence of genuine clinical expansion would support the biomedical reading. The relationship between market incentives and diagnostic expansion is a key contested point between readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pharmaceutical_influence_on_diagnostic_expansion, empirical, 'Degree of pharmaceutical industry influence on diagnostic threshold and expansion decisions.').

omega_variable(
    founding_problem_mandate_slippage,
    'The founding problem is distinguishing genuine mental illness from moral failing. Does the contemporary DSM application match this founding scope, or has the mandate expanded to include behavioral noncompliance, institutional mismatch, and neurotype variation that do not fit the founding problem?',
    'Historical comparison: map diagnostic categories and thresholds to the original founding problem (mid-20th-century distinction between pathology and character flaw). Identify which contemporary diagnoses apply to populations the founding problem did not address (e.g., childhood ADHD, internet addiction, gender dysphoria). Document whether expansion was justified by new neurobiological discovery or by institutional/pharmaceutical interest.',
    'Evidence of mandate slippage would support mandatrophy diagnosis and piton-tendency. It would not change the constraint''s type (tangled_rope is still appropriate; the constraint remains extractive and enforced), but would clarify that part of the extraction now serves institutional conformity enforcement rather than disease treatment. Would sharpen distinction between the biomedical reading''s legitimate scope and its over-reach.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_mandate_slippage, empirical, 'Whether DSM application has expanded beyond the founding problem''s scope (distinguishing illness from character flaw).').

omega_variable(
    alternative_reading_foreclosure,
    'Does the biomedical reading logically foreclose the neurodiversity reading (they cannot both be true in one framework), or do they coexist with different referents (biomedical reading applies to some conditions, neurodiversity reading applies to others)?',
    'Conceptual analysis: does asserting ''autism is a neurotype variation, not a disease'' contradict ''autism maps to objective neurobiological entities''? Or can both statements be true if we allow that some neurobiological variation is benign (not disease) while other variation is pathological (disease)? If coexistence is possible, which reading would frame which conditions?',
    'If foreclosure, only one reading can be institutionally dominant—the other must be suppressed. If coexistence, the readings could carve out different domains (biomedical for severe conditions like psychosis, neurodiversity for variation-based diagnoses like autism/ADHD). Affects the reading_relations value (forecloses vs. coexists_with).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_reading_foreclosure, conceptual, 'Whether the biomedical and neurodiversity readings logically foreclose each other or can coexist.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__biomedical_reading, 1952, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t1952, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 1952, 0.12).
narrative_ontology:measurement(dsm__tr_t1975, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 1975, 0.18).
narrative_ontology:measurement(dsm__tr_t1994, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 1994, 0.28).
narrative_ontology:measurement(dsm__tr_t2005, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2005, 0.36).
narrative_ontology:measurement(dsm__tr_t2013, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2013, 0.4).
narrative_ontology:measurement(dsm__tr_t2024, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(dsm__be_t1952, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 1952, 0.35).
narrative_ontology:measurement(dsm__be_t1975, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 1975, 0.48).
narrative_ontology:measurement(dsm__be_t1994, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 1994, 0.61).
narrative_ontology:measurement(dsm__be_t2005, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2005, 0.72).
narrative_ontology:measurement(dsm__be_t2013, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2013, 0.77).
narrative_ontology:measurement(dsm__be_t2024, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2024, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t1952, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 1952, 0.44).
narrative_ontology:measurement(dsm__su_t1975, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 1975, 0.55).
narrative_ontology:measurement(dsm__su_t1994, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 1994, 0.62).
narrative_ontology:measurement(dsm__su_t2005, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2005, 0.69).
narrative_ontology:measurement(dsm__su_t2013, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2013, 0.73).
narrative_ontology:measurement(dsm__su_t2024, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2024, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__biomedical_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(dsm_taxonomy_kernel__biomedical_reading, 0.18).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel__critical_psychiatry_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel__neurodiversity_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, involuntary_psychiatric_commitment).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, psychotropic_pharmaceutical_markets).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the DSM-taxonomy kernel. The kernel is the standing DSM classification system. The biomedical reading instantiates a constraint where the core claim is that DSM categories map to objective neurobiological entities—this reading justifies medical intervention. The critical_psychiatry_reading instantiates a different constraint where the core claim is that DSM categories are reverse-engineered from pharmaceutical availability to construct markets. The neurodiversity_reading instantiates a third constraint where the core claim is that DSM categories pathologize natural neurological variation. Each reading has a different ε (extractiveness), different beneficiary/victim structure, and different type. They are not different measurements of one constraint; they are different constraints grounded in the same kernel (the DSM text). The ε-invariance principle requires decomposing the kernel contest into separate stories: the biomedical reading's ε (0.81) reflects how the reading justifies extraction; the critical reading's ε would be higher (the same mechanisms are now framed as pure profit-extraction rather than disease-treatment); the neurodiversity reading's ε would also be high but with a victim set of neurodivergent people rather than 'diseased' persons. The kernel remains; the constraint stories differ.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dsm_taxonomy_kernel__biomedical_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
