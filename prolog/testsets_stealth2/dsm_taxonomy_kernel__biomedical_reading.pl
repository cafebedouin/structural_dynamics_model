% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__biomedical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: dsm_taxonomy_kernel__biomedical_reading
 *   human_readable: Biomedical Reading of the DSM Taxonomy Kernel: Categories as Discoverable Neurobiological Disease Entities
 *   domain: medical_epistemology/psychiatric_taxonomy/social_construction_of_illness
 *
 * SUMMARY:
 *   The operative claim that DSM categories map to objective neurobiological
 *   disease entities structures who may define disordered mind, on what
 *   evidence, and with what powers. Under this reading, a diagnosis is not a
 *   negotiated description but the detection of a disease: that framing is
 *   what converts distress into a billable entity, behavior into grounds for
 *   commitment or capacity removal, and category expansion into market
 *   expansion. This file generates ONE reading of the dsm_taxonomy_kernel as
 *   a clean, epsilon-invariant constraint: the referent of every metric is
 *   the standing disease-entity-governed classification regime as this
 *   reading constitutes it, and nothing is averaged over the sibling
 *   readings, which are separate constraint files linked through the network
 *   block. The claim/metric independence rule is observed deliberately: the
 *   claimed type is authored from the structure (a genuine coordination
 *   function coexisting with asymmetric extraction under active enforcement),
 *   while the metrics are authored from the constraint's observed operation,
 *   including its accumulating extraction and rising theatrical maintenance.
 *   KEY AGENTS (by structural relationship): -
 *   psychiatric_professional_establishment: agenda-setter and principal
 *   collector (institutional/identity_locked) — administers the taxonomy,
 *   holds statutory powers, collects jurisdiction, legitimacy, and income -
 *   psychopharmaceutical_industry: secondary beneficiary
 *   (institutional/arbitrage) — monetizes category expansion without
 *   administering the taxonomy - behavioral_conformity_institutions:
 *   incidental beneficiary (institutional/constrained) — consume
 *   administrable classifications they did not build -
 *   threshold_diagnosed_persons: primary target (powerless/trapped) — bears
 *   records, medication burdens, coercion exposure, capacity loss -
 *   diagnostic_dissenters: primary target whose resistance is recoded as
 *   symptom (powerless/trapped) - service_user_survivor_movements: organized
 *   target-side coalition (organized/constrained) -
 *   alternative_framework_clinicians: excluded voice (moderate/constrained) -
 *   research_funding_agencies: secondary agenda-setter (institutional/mobile)
 *   — steers the epistemic field through grant priorities
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__biomedical_reading, 0.72).
domain_priors:suppression_score(dsm_taxonomy_kernel__biomedical_reading, 0.7).
domain_priors:theater_ratio(dsm_taxonomy_kernel__biomedical_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__biomedical_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__biomedical_reading, "Biomedical Reading of the DSM Taxonomy Kernel: Categories as Discoverable Neurobiological Disease Entities").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__biomedical_reading, "medical_epistemology/psychiatric_taxonomy/social_construction_of_illness").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__biomedical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__biomedical_reading, 'ed58364e-989a-429c-be86-37529036f727').
narrative_ontology:cs_kernel_codification('ed58364e-989a-429c-be86-37529036f727', formalized).
narrative_ontology:cs_authority_grounding('ed58364e-989a-429c-be86-37529036f727', expertise).
narrative_ontology:cs_interpretation_layer_present('ed58364e-989a-429c-be86-37529036f727').
narrative_ontology:cs_reading_relation('ed58364e-989a-429c-be86-37529036f727', dsm_taxonomy_kernel__critical_psychiatry_reading, forecloses).
narrative_ontology:cs_reading_relation('ed58364e-989a-429c-be86-37529036f727', dsm_taxonomy_kernel__neurodiversity_reading, coexists_with).
narrative_ontology:cs_axiom('ed58364e-989a-429c-be86-37529036f727', foundational, mental_disorders_are_discoverable_neurobiological_disease_entities).
narrative_ontology:cs_axiom_status(mental_disorders_are_discoverable_neurobiological_disease_entities, holdable).
narrative_ontology:cs_axiom_grounding('ed58364e-989a-429c-be86-37529036f727', mental_disorders_are_discoverable_neurobiological_disease_entities, empirically_contingent).
narrative_ontology:cs_axiom('ed58364e-989a-429c-be86-37529036f727', secondary, medical_classification_entitles_patients_to_care_parity).
narrative_ontology:cs_axiom_status(medical_classification_entitles_patients_to_care_parity, holdable).
narrative_ontology:cs_axiom_grounding('ed58364e-989a-429c-be86-37529036f727', medical_classification_entitles_patients_to_care_parity, instrumental).
narrative_ontology:cs_reference_frame('ed58364e-989a-429c-be86-37529036f727', neo_kraepelinian_disease_entity_program).
narrative_ontology:cs_drift_state('ed58364e-989a-429c-be86-37529036f727', post_rdoc_contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ed58364e-989a-429c-be86-37529036f727', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, psychiatric_professional_establishment).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, psychopharmaceutical_industry).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, behavioral_conformity_institutions).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, threshold_diagnosed_persons).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, diagnostic_dissenters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, service_user_survivor_movements).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__biomedical_reading, neo_kraepelinian_research_program).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__biomedical_reading, disease_entity_nosology_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and revises the diagnostic manual through association workgroups, trains residents in its categories, staffs the editorial boards that review competing frameworks, and holds statutory powers — civil commitment certification, capacity determination, forensic reporting — that presuppose the categories name illnesses. Members' careers, board certifications, and departmental standing are built on the medical-model identity; abandoning the disease-entity framing would mean redefining the profession itself. Income flows from insured diagnostic encounters and prescriber roles.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, psychiatric_professional_establishment, agenda_setter,
    institutional, generational, identity_locked, global).

% Develops and sells psychotropics whose approved indications, labels, and prescriber uptake depend on official diagnostic categories, and funds a large share of the trial literature and continuing education that circulates the disease-entity framing. When a category expands, the addressable market expands with it. Capital is mobile: portfolios can pivot to new indications or reformulations if any single category loses official standing.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, psychopharmaceutical_industry, beneficiary,
    institutional, biographical, arbitrage, global).

% Schools, courts, employers, militaries, and disability systems use diagnostic codes to sort people — excusing, penalizing, accommodating, medicating, or removing them. A code converts contested behavior into an administrable fact: a truancy case becomes a disorder, a workplace conflict becomes an impairment determination. These institutions did not build the taxonomy but depend on it for defensible decisions and would need replacement procedures if it lost official standing.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, behavioral_conformity_institutions, beneficiary,
    institutional, generational, constrained, national).

% People whose distress or behavior crosses a diagnostic threshold acquire a coded record that follows them across systems, prescription regimens whose justification rests on the disease-entity claim, exposure to involuntary treatment where statutes allow, and capacity determinations that can reach voting, parenting, driving, or financial autonomy. Declining the diagnosis rarely ends the encounter: refusal is documented, sometimes escalated. Many come to understand themselves through the category.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, threshold_diagnosed_persons, payer,
    powerless, biographical, trapped, global).

% Diagnosed people who reject their category find their dissent absorbed into the framework: disagreement with the diagnosis is charted as impaired insight or poor compliance, which can itself justify continued or intensified treatment. Their exit attempts meet the same records, statutes, and gatekeepers as everyone else's, with the added burden that their testimony is pre-classified as symptomatic.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, diagnostic_dissenters, payer,
    powerless, biographical, trapped, national).

% Networks of current and former patients — peer support, hearing-voices groups, survivor researchers, rights campaigns — document coercive and iatrogenic harm, campaign against compulsory treatment, and build non-diagnostic support infrastructures. They carry the system's costs collectively and supply its main organized opposition; their leverage is limited because funding, licensure, and legal recognition route through the professional bodies they contest.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, service_user_survivor_movements, payer,
    organized, generational, constrained, continental).

% Therapists and psychiatrists working with idiographic formulation, psychosocial causation, or open-dialogue-style approaches find their work unreimbursable under diagnosis-keyed billing, difficult to publish in leading journals, and exposed at license review. They would argue for multi-causal, negotiable classification but sit outside the committees and editorial boards where the manual is made.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, alternative_framework_clinicians, excluded,
    moderate, biographical, constrained, national).

% Public funders set grant priorities that reward biomarker and circuit-level proposals framed as validating or replacing diagnostic categories; laboratories and careers orient toward fundable framings. The largest public funder publicly declined the categories' neurobiological validity in 2013 while leaving them in clinical force — an acknowledgment from inside the funding apparatus that the disease-entity warrant remains undelivered, issued without disturbing day-to-day practice.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, research_funding_agencies, agenda_setter,
    institutional, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dsm_taxonomy_kernel__biomedical_reading, psychiatric_professional_establishment).
narrative_ontology:fixing_cost_class(dsm_taxonomy_kernel__biomedical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, reimbursable, legally actionable diagnostic vocabulary: clinicians communicate reliably about presentations, insurers standardize payment against codes, researchers aggregate subjects across sites, and courts and schools act on uniform categories instead of ad hoc judgment.
% TRANSFER_FUNCTION: Moves decision authority over mind and behavior from diagnosed individuals and their communities to credentialed professionals and the institutions that consume classifications; moves money (reimbursement, prescription revenue, grant funds) toward prescribers, manufacturers, and biomarker programs; moves legitimacy and jurisdiction to psychiatry as a branch of medicine.
% ABSENT_VOICES: Service users, survivors, and formulation-based clinicians are largely outside criterion-writing workgroups and editorial boards; diagnosed people's testimony enters the process mainly as symptom data rather than as argument. They are located in peer movements, survivor scholarship, and marginal clinical networks — present in the world, absent from the rooms where the categories are set.
% DISAPPEARANCE_RATIONALE: If the disease-entity constraint vanished overnight, civil commitment statutes, capacity doctrine, diagnosis-keyed reimbursement, drug indication regimes, forensic standards, and professional training pipelines would all lose their warrant simultaneously; the psychiatric enterprise would have to rebuild its legal powers and payment infrastructure on some other basis, and millions of coded records would need reinterpretation.
% FOUNDING_PROBLEM: Mid-twentieth-century psychiatry had unreliable, school-fractured classification: competing doctrines produced different diagnoses for the same person, excluding psychiatry from insurance, from medicine's prestige, and from cumulative research. The 1980 manual rebuilt the taxonomy around explicit operational criteria, and the disease-entity claim supplied the ontological warrant that the categories name real illnesses awaiting discovery.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the field's largest public funder stated in 2013 that the categories lack neurobiological validity while remaining clinically necessary — an external-institutional attestation that the warrant was never delivered; historians of the 1980 revision document that its designers targeted reliability, not validity, and treated disease-entity claims as aspirational; cross-national and replication studies show category boundaries shifting across cohorts and cultures. The establishment's counter-position — that the science simply has not caught up yet — is self-asserted by the beneficiary set and corroborated by no one outside it.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__biomedical_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__biomedical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__biomedical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dsm_taxonomy_kernel__biomedical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__biomedical_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.72 because the warrant does real transfer work: it justifies involuntary treatment statutes, capacity determinations reaching fundamental liberties, lifelong coded records, and pharmaceutical imposition, all resting on an entity claim whose validating evidence has not arrived. Suppression is authored at 0.70 as a raw structural property (unscaled by power or scope): statutory enforcement, reimbursement gating, journal and licensure gatekeeping, and the insight-recoding loop in which a patient's rejection of the diagnosis is charted as impaired insight — a mechanism that converts dissent into confirming evidence. Theater is 0.50: the clinical and communicative functions are real, but a growing share of activity defends the warrant rhetorically (the long-persisting chemical-imbalance narrative, 'bridge to future neuroscience' framing) after repeated validation failures — proxy maintenance replacing the promised discovery program. Accessibility collapse is 0.62: inside clinical, legal, and insurance settings the entity frame closes alternatives almost completely once in place; outside them, peer, survivor, and formulation-based alternatives persist. Resistance is 0.58: organized survivor movements, critical clinician networks, and occasional funder defection meet the constraint continuously without displacing it. The temporal series run on one shared grid (1980, 1990, 2000, 2010, 2019, 2025) with every tracked metric authored at every point; all three trajectories rise, modeling extraction accumulation and enforcement hardening alongside theatrical drift. The suppression_requirement series is included because the story specifically tracks enforcement-capacity growth: commitment statutes, outpatient commitment expansions, coding infrastructure, and gatekeeping matured over the interval rather than holding static.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different types from identical structure. From the establishment seat the arrangement is its own professional competence and ethical mission: reliable diagnosis, care parity, destigmatization through 'real illness' — a coordination achievement it built and maintains. From the payer seats the same structure operates as records that follow them, medication justified by an undelivered warrant, coercion exposure, and dissent that confirms the diagnosis. Among beneficiaries the exits differ sharply: industry holds arbitrage-grade mobility (portfolios pivot if a category falls), conformity institutions are constrained consumers, and the establishment is identity_locked — its members' selves and livelihoods are fused with the medical-model identity, so the seat that administers the constraint also cannot leave it. Among the targets, individually powerless diagnosed persons differ from the organized survivor coalitions only in collective leverage, not in exposure; the coalition check is answered by seating the movements explicitly as an organized payer.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: the establishment sits nearest the beneficiary end (collects jurisdiction and income, administers the rules), industry lower still in effective burden because arbitrage exit dampens whatever costs reach it, conformity institutions modestly subsidized through administrability they receive without paying for. Victim declarations drive high directionality: trapped exit pushes threshold_diagnosed_persons and diagnostic_dissenters toward the full-target end — their records, statutes, and recoded dissent remove arbitrage or mobility; the organized survivor movements remain targets with somewhat damped effective extraction through collective leverage. Excluded clinicians sit nearer symmetric: they forfeit reimbursement and publication rather than liberty. Global spatial scope amplifies effective extraction modestly for the targets, since verification of the warrant's failure is hardest at the scale where the categories operate. Suppression is reported unscaled; only extractiveness is scaled by directionality and scope in the engine's computation. Gains demonstrably accrue to the establishment seat — jurisdiction, statutory powers, insured diagnostic income, and the legitimacy that reproduces the profession — which is why gain_flow names it; industry captures a large monetized downstream share but does not administer the arrangement, and no other seat captures gains.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unreliable, school-fractured, illegitimate classification — was substantially addressed by the operational criteria themselves, which delivered inter-rater reliability regardless of whether any entity lies beneath a category. The disease-entity warrant was the legitimacy rider attached to that repair, and it is the part whose evidentiary basis has eroded while its jurisdictional work continues. Mandatrophy status is therefore contested rather than resolved: the coordination half of the structure is alive (communication, billing, research aggregation still depend on shared codes), while the warrant half increasingly performs discovery it cannot deliver (theater_ratio climbing toward 0.5). The classification prevents symmetrical mislabeling: calling the structure pure extraction erases the genuine coordination that millions of clinical encounters and studies rely on; calling it pure coordination erases who bears commitment, capacity loss, and medication burden while others collect. The rising extraction and theater series mark drift toward extraction-dominance without completed atrophy — the structure still does real work, which is what separates this from a piton verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint instantiates the biomedical reading of the dsm_taxonomy_kernel; what happens to the victim set and the extraction profile if a sibling reading becomes the governing framework?',
    'Observation of which reading is adopted as governing in classification policy — professional-body revision cycles, statutory definitions of mental disorder, funder mandates — and re-authoring of the affected seats under the adopted reading.',
    'Under the neurodiversity reading the victim set shifts to neurological minorities pressured toward conformity and extraction concentrates on conformity enforcement; under the critical-psychiatry reading the victim set widens to the prescribing public and extraction concentrates on pharmaceutical market construction. The establishment and industry seats invert toward target seats in both cases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: this story is one reading of a contested kernel; sibling readings instantiate different constraints with different victim sets.').

omega_variable(
    biomarker_validation_failure,
    'Will any DSM category achieve biomarker-grounded validity — the outcome this reading''s own foundational axiom predicts and requires?',
    'Preregistered prospective validation programs per category (molecular genetics, imaging endophenotypes, biospecimen panels) with pre-committed success criteria, assessed independently of the professions whose jurisdiction rides on the result.',
    'Universal failure collapses the vindicated proposition and transfers warrant-weight to the sibling readings; partial success would split the kernel into disease versus variation sub-kernels, changing this constraint''s victim set and epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biomarker_validation_failure, empirical, 'Whether the reading''s core empirical prediction (discoverable disease entities) is ever delivered.').

omega_variable(
    suppression_mechanism_split,
    'Is the measured suppression structural (statutory powers, reimbursement gating, records, licensure) or internalized (self-stigma, identity fusion with the diagnosis, acceptance of the insight-recoding of one''s own dissent)?',
    'Post-exit suppression trajectory: track people who leave the diagnostic system entirely (dropped records, informal support only); if self-supervision, shame, and symptom-language persist after all structural barriers are removed, the internalized share is substantial.',
    'If internalized, effective suppression exceeds the structural measure and the constraint would persist in weakened form even under legal reform; if structural, dismantling the enforcement machinery would release most of the measured suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural versus internalized composition of the constraint''s suppressive force.').

omega_variable(
    coordination_warrant_separability,
    'Is the genuine coordination function (shared communicable vocabulary, reimbursement coding, research subject aggregation) separable from the disease-entity warrant that licenses coercion and market expansion?',
    'Jurisdictions or pilot systems using descriptive, negotiated, or functioning-based documentation that preserves billing and communication while dropping the entity claim; compare coercion rates, capacity determinations, and prescribing volume against entity-warranted systems.',
    'If separable, the extraction rides on top of a real coordination function and the structure is hybrid by construction; if inseparable, part of the measured extraction is irreducible cost of the coordination itself and the effective excess extraction falls accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_warrant_separability, conceptual, 'Whether the constraint''s coordination and warrant components can be structurally unbundled.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__biomedical_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t1980, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement_basis(dsm__tr_t1980, observed).
narrative_ontology:measurement(dsm__tr_t1990, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 1990, 0.26).
narrative_ontology:measurement_basis(dsm__tr_t1990, observed).
narrative_ontology:measurement(dsm__tr_t2000, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2000, 0.33).
narrative_ontology:measurement_basis(dsm__tr_t2000, observed).
narrative_ontology:measurement(dsm__tr_t2010, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2010, 0.42).
narrative_ontology:measurement_basis(dsm__tr_t2010, observed).
narrative_ontology:measurement(dsm__tr_t2019, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2019, 0.47).
narrative_ontology:measurement_basis(dsm__tr_t2019, observed).
narrative_ontology:measurement(dsm__tr_t2025, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2025, 0.5).
narrative_ontology:measurement_basis(dsm__tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(dsm__be_t1980, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement_basis(dsm__be_t1980, observed).
narrative_ontology:measurement(dsm__be_t1990, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 1990, 0.53).
narrative_ontology:measurement_basis(dsm__be_t1990, observed).
narrative_ontology:measurement(dsm__be_t2000, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement_basis(dsm__be_t2000, observed).
narrative_ontology:measurement(dsm__be_t2010, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2010, 0.66).
narrative_ontology:measurement_basis(dsm__be_t2010, observed).
narrative_ontology:measurement(dsm__be_t2019, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2019, 0.7).
narrative_ontology:measurement_basis(dsm__be_t2019, observed).
narrative_ontology:measurement(dsm__be_t2025, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2025, 0.72).
narrative_ontology:measurement_basis(dsm__be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t1980, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement_basis(dsm__su_t1980, observed).
narrative_ontology:measurement(dsm__su_t1990, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 1990, 0.57).
narrative_ontology:measurement_basis(dsm__su_t1990, observed).
narrative_ontology:measurement(dsm__su_t2000, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2000, 0.63).
narrative_ontology:measurement_basis(dsm__su_t2000, observed).
narrative_ontology:measurement(dsm__su_t2010, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2010, 0.67).
narrative_ontology:measurement_basis(dsm__su_t2010, observed).
narrative_ontology:measurement(dsm__su_t2019, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2019, 0.69).
narrative_ontology:measurement_basis(dsm__su_t2019, observed).
narrative_ontology:measurement(dsm__su_t2025, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2025, 0.7).
narrative_ontology:measurement_basis(dsm__su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__biomedical_reading, information_standard).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel__neurodiversity_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel__critical_psychiatry_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'what DSM categories are' decomposes into three structurally distinct constraints — one per reading of dsm_taxonomy_kernel — per the epsilon-invariance principle. This story authors the biomedical reading only: epsilon is assessed for the disease-entity regime as this reading constitutes it, with its own beneficiaries (establishment, industry, conformity institutions) and victims (threshold-diagnosed persons, dissenters). Family links run to both siblings; the dominant downstream pressure runs outward from this reading, since its institutional hold sets the legitimacy conditions under which the siblings operate as dissent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
