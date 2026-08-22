% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__biomedical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
 *   human_readable: DSM Biomedical Reading: Categories as Discoverable Disease Entities
 *   domain: medical_epistemology/psychiatric_taxonomy/social_construction_of_illness
 *
 * SUMMARY:
 *   This story instantiates the biomedical reading of the DSM taxonomy
 *   kernel: the claim that DSM categories map to objective neurobiological
 *   disease entities discoverable through empirical research. The standing
 *   arrangement under contest is the DSM-based psychiatric diagnostic regime
 *   — committee-set diagnostic thresholds, disease-framed records,
 *   category-keyed reimbursement and commitment law — assessed here by the
 *   biomedical reading's own lights. Those lights are empirical: the
 *   reading's own standards require that a category's disease-entity status
 *   be established by discovery, and by those standards the regime's warrant
 *   runs ahead of its validation. No biomarker-based entity has been
 *   validated for most categories, the DSM-5 field trials regressed on
 *   reliability, and the field's own federal funder publicly broke with the
 *   categories in 2013 while the regime continued to draw medical authority,
 *   prescription channeling, and coercive legal power from them. The reading
 *   therefore authors a substantially extractive but not maximal epsilon: the
 *   arrangement genuinely coordinates (a shared nomenclature its own critics
 *   use) and delivers real treatment, while its coercive and market apparatus
 *   draws on credit the research program has not yet earned. This is one of
 *   three linked readings over the same kernel; per the epsilon-invariance
 *   principle the readings are separate constraints with separate epsilon
 *   over one shared referent, and this story's claim and metrics are authored
 *   independently — the engine computes per-seat classifications from the
 *   structural data. KEY AGENTS (by structural relationship): -
 *   psychiatric_professional_establishment: agenda-setter
 *   (institutional/identity_locked) — sets criteria, collects jurisdiction -
 *   pharmaceutical_industry: primary beneficiary (institutional/mobile) —
 *   collects the prescription channel - behavioral_conformity_institutions:
 *   secondary beneficiary (institutional/constrained) — converts
 *   accommodation demands into pathology management -
 *   individuals_meeting_diagnostic_thresholds: primary target
 *   (powerless/trapped) — bears labels, records, coercion, capacity loss -
 *   practicing_clinicians: constrained intermediary
 *   (moderate/beneficiary+payer) - service_user_survivor_movements: excluded
 *   voice (organized/constrained) - medical_epistemology_scholars: analytical
 *   observer — sees the full structure
 *
 * KEY AGENTS:
 *   - psychiatric_professional_establishment: agenda-setter (institutional/identity_locked) — sets criteria via APA task forces and panel votes, collects the jurisdictional authority and reimbursement-keyed income the disease framing confers; its professional identity is constituted by that framing
 *   - pharmaceutical_industry: primary beneficiary (institutional/mobile) — collects the prescription channel the categories open; capital can reallocate out of psychiatry within a product cycle
 *   - behavioral_conformity_institutions: secondary beneficiary (institutional/constrained) — schools, courts, disability and employment systems that convert what they would otherwise owe individuals as accommodation into properties of the individual to be managed
 *   - individuals_meeting_diagnostic_thresholds: primary target (powerless/trapped) — anyone crossing a committee-set threshold bears the label, the record, the treatment obligations, and in commitment contexts the loss of legal capacity
 *   - practicing_clinicians: constrained intermediary (moderate/beneficiary+payer) — paid and standing-elevated by the coding system, bound by it to categorical practice
 *   - service_user_survivor_movements: excluded voice (organized/constrained) — organized bearers of diagnoses with no seat in the criteria-setting process
 *   - medical_epistemology_scholars: analytical observer — tracks the validity/reliability gap and the natural-kind question; no enforcement power, no revenue
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__biomedical_reading, 0.64).
domain_priors:suppression_score(dsm_taxonomy_kernel__biomedical_reading, 0.62).
domain_priors:theater_ratio(dsm_taxonomy_kernel__biomedical_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__biomedical_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__biomedical_reading, "DSM Biomedical Reading: Categories as Discoverable Disease Entities").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__biomedical_reading, "medical_epistemology/psychiatric_taxonomy/social_construction_of_illness").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__biomedical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__biomedical_reading, '4070111d-9250-4d53-9f78-7c018eb21b19').
narrative_ontology:cs_kernel_codification('4070111d-9250-4d53-9f78-7c018eb21b19', formalized).
narrative_ontology:cs_authority_grounding('4070111d-9250-4d53-9f78-7c018eb21b19', expertise).
narrative_ontology:cs_interpretation_layer_present('4070111d-9250-4d53-9f78-7c018eb21b19').
narrative_ontology:cs_reading_relation('4070111d-9250-4d53-9f78-7c018eb21b19', dsm_taxonomy_kernel__critical_psychiatry_reading, coexists_with).
narrative_ontology:cs_reading_relation('4070111d-9250-4d53-9f78-7c018eb21b19', dsm_taxonomy_kernel__neurodiversity_reading, forecloses).
narrative_ontology:cs_axiom('4070111d-9250-4d53-9f78-7c018eb21b19', foundational, dsm_categories_map_to_disease_entities).
narrative_ontology:cs_axiom_status(dsm_categories_map_to_disease_entities, holdable).
narrative_ontology:cs_axiom_grounding('4070111d-9250-4d53-9f78-7c018eb21b19', dsm_categories_map_to_disease_entities, empirically_contingent).
narrative_ontology:cs_axiom('4070111d-9250-4d53-9f78-7c018eb21b19', secondary, disease_status_warrants_medical_authority).
narrative_ontology:cs_axiom_status(disease_status_warrants_medical_authority, holdable).
narrative_ontology:cs_axiom_grounding('4070111d-9250-4d53-9f78-7c018eb21b19', disease_status_warrants_medical_authority, instrumental).
narrative_ontology:cs_reference_frame('4070111d-9250-4d53-9f78-7c018eb21b19', neo_kraepelin_research_program).
narrative_ontology:cs_drift_state('4070111d-9250-4d53-9f78-7c018eb21b19', post_rdoc_acknowledgment_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('4070111d-9250-4d53-9f78-7c018eb21b19', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, psychiatric_professional_establishment).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, behavioral_conformity_institutions).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, individuals_meeting_diagnostic_thresholds).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, practicing_clinicians).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, practicing_clinicians).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__biomedical_reading, psychiatry_as_branch_of_medicine).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__biomedical_reading, disease_entity_hypothesis_of_mental_disorder).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the diagnostic criteria through APA task forces and panel votes, trains and credentials the practitioners who apply them, and controls the journals and review channels through which the research program is adjudicated. The profession's jurisdiction — the recognized authority to define and treat mental disorder, with the reimbursement and legal standing that follow — flows from the disease framing of its categories. Members' careers, training pipelines, and institutional positions are constituted inside that framing; abandoning it would dissolve the profession's boundary against psychology, social work, and peer support rather than relocate its members.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, psychiatric_professional_establishment, agenda_setter,
    institutional, generational, identity_locked, global).

% Sells psychotropic medications whose prescription volumes are keyed to diagnostic category prevalence; funds trials, key-opinion leaders, and — per disclosed conflict-of-interest studies — a substantial share of DSM panel members. Each category expansion enlarges the addressable population. Its capital is not bound to psychiatry: research portfolios can be reallocated to oncology, neurology, or metabolic disease within a product cycle.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_industry, beneficiary,
    institutional, biographical, mobile, global).

% Schools, courts, disability systems, and employers use diagnostic categories to process people whose behavior they cannot otherwise accommodate: special-education placement rests on a diagnosis, insanity and capacity defenses require one, disability benefits and workplace accommodations are gated by codes. The categories convert demands these institutions would otherwise face as their own obligations — to adapt to the person — into properties of the individual. They need some such instrument and the diagnostic system is the one the law and funding recognize; switching to non-categorical frameworks would require rebuilding legal and administrative machinery.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, behavioral_conformity_institutions, beneficiary,
    institutional, generational, constrained, national).

% Anyone whose distress or behavior crosses a committee-set diagnostic threshold acquires a disease classification that follows them through records, insurance, custody proceedings, and employment screening. The classification brings access to treatment and legal protection, and it also brings stigma, medication side effects, and — in commitment and guardianship contexts — loss of bodily autonomy and legal capacity on the strength of categories whose disease-entity status the research program itself has not established. Refusing the classification forfeits treatment access and legal recognition; once assigned, it persists in the record. Service-user movements organize some of this population collectively, but the threshold is crossed alone.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, individuals_meeting_diagnostic_thresholds, payer,
    powerless, biographical, trapped, global).

% Must assign a diagnostic code for nearly every billed encounter — the code is the reimbursement key in insurance systems, public health systems, and EHR templates. The system pays them and confers medical standing, while requiring them to render heterogeneous distress in categorical form and to practice inside the categories' boundaries; formulation-based or non-categorical practice is done off the reimbursement grid at personal cost.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, practicing_clinicians, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__biomedical_reading, practicing_clinicians, payer).

% Organizations of people who have carried diagnoses — the Hearing Voices Network, neurodiversity self-advocacy, survivor movements — contest the disease framing from lived experience and build alternative supports outside clinical channels. They hold no seats in the DSM revision process where criteria are set; their route to influence runs through protest, litigation, and building parallel institutions rather than through the table where the thresholds are written.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, service_user_survivor_movements, excluded,
    organized, generational, constrained, global).

% Philosophers of psychiatry, historians of medicine, and science-studies researchers track the validity/reliability gap, the natural-kind status of the categories, and the conflict-of-interest record of the revision panels. They hold no enforcement power and collect no revenue from the arrangement; their analyses are cited by every other seat when useful and set aside when not.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, medical_epistemology_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dsm_taxonomy_kernel__biomedical_reading, psychiatric_professional_establishment).
narrative_ontology:fixing_cost_class(dsm_taxonomy_kernel__biomedical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A shared diagnostic nomenclature: DSM categories give clinicians, researchers, insurers, courts, and patients a common vocabulary for describing, counting, studying, and reimbursing mental distress — inter-clinician communication, epidemiological surveillance, treatment planning, and research subject-matching are solved once, centrally, instead of ad hoc per practitioner.
% TRANSFER_FUNCTION: Moves jurisdictional authority over the definition and treatment of mental disorder to the psychiatric establishment; moves prescription volume and research funding toward the categories and the products keyed to them; converts what institutions would otherwise owe individuals as accommodation into individual properties to be treated; and moves legal capacity and bodily autonomy from individuals who cross a diagnostic threshold to medical and judicial decision-makers.
% ABSENT_VOICES: The people the thresholds are written about are largely absent from the table where they are written: DSM-5 panels included minimal lived-experience representation, service-user and neurodiversity organizations hold no seats in the revision process, and those subject to involuntary commitment — the sharpest consequence of the disease framing — are by definition not in a position to contest the category at the moment it is applied to them. Their objections enter only after the fact, through protest, litigation, and parallel institutions.
% DISAPPEARANCE_RATIONALE: Commitment statutes, insurance reimbursement, disability determination, forensic capacity and insanity defenses, and special-education placement all key to the disease framing of the categories. If the warrant that the categories are disease entities vanished overnight, involuntary treatment would revert to coercion requiring independent justification, reimbursement codes would lose their medical rationale, psychiatry's jurisdictional boundary against psychology, social work, and peer support would become contestable, and prescription channels would reorganize around indications rather than diagnostic categories. The coordination core — a shared vocabulary — would survive in weakened form; the coercive and market superstructure built on the warrant would not.
% FOUNDING_PROBLEM: Psychiatry through the 1970s had no shared, reliable classification: diagnostic agreement between clinicians and institutions was poor, research could not accumulate across sites, and the field's claim to a place in academic medicine, insurance, and the law required a taxonomy comparable to the rest of medicine. DSM-III's operationalized criteria — and the neo-Kraepelin promise that the categories would eventually be validated as disease entities by neuroscience — were built to solve that communication-and-legitimacy problem.
% FOUNDING_PROBLEM_CORROBORATION: Historians of medicine corroborate the founding communication and legitimacy problem from outside the benefiting parties (the DSM-III revolution literature records the pre-1980 reliability failure). The NIMH's 2013 RDoC announcement — issued by the federal funder, not a beneficiary of category stability — attests from outside that the categories' disease-entity mapping remains unrealized four decades on. The British Psychological Society's 2011 response to the DSM-5 drafts and service-user scholarship corroborate from outside as well. No source outside the benefiting parties attests that the founding problem is fully solved: the coordination half is conceded even by critics, the validation half is conceded by no one.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__biomedical_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__biomedical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__biomedical_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dsm_taxonomy_kernel__biomedical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__biomedical_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.64 from this reading's own lights: the coercive apparatus (involuntary commitment, capacity determinations) and the prescription channel draw their warrant from the claim that the categories are disease entities, while the reading's own empirical standards show the entities undiscovered for most categories — the warrant runs ahead of the validation. It sits below what the critical reading would author over the same referent because the reading credits the arrangement's genuine coordination and real treatment effects. Suppression (0.62) is authored as a raw structural property — commitment statutes, category-keyed reimbursement, licensing and EHR hard-coding penalize non-categorical practice without erasing it; per the framework only extractiveness is scaled by directionality and scope, never suppression. Theater_ratio (0.45) tracks the performed share of the warrant: low when the taxonomy was honestly provisional (DSM-III era), rising through the chemical-imbalance marketing era, peaking at the 2013 DSM-5 launch, which performed established authority in the same season the NIMH publicly disavowed the categories' validity, then partially receding as acknowledgment spread. Accessibility_collapse (0.42): alternatives — formulation-based practice, neurodiversity-affirming and social-model approaches, critical psychiatry — remain visible and practicable but are structurally penalized off the reimbursement and legal grid. Resistance (0.60): survivor and neurodiversity movements, the British Psychological Society's critique, the RDoC internal challenge, and the DSM-5 revision protests. Coordination type is information_standard by the dominant-function test: the founding problem was a shared classification, and failure of the nomenclature — not of the patient-role boundary maintenance or the reimbursement allocation that ride on it — is what would collapse the coordination the constraint exists to provide; a decomposed story could treat the coding-and-reimbursement apparatus separately. All three tracked metrics run on one shared time grid (1980-2026, seven points) so no metric's series borrows another's end-state values.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from the same structure. From the establishment's seat the arrangement is the research program itself — coordination it built and adjudicates, the categories provisional tools, extraction near zero. From a diagnosed individual's seat the same threshold is the moment legal capacity and self-concept are reassigned by committee vote. From pharma's seat it is market infrastructure; from a clinician's seat a reimbursement grid that both pays and constrains; from the scholar's seat an unresolved natural-kind claim with a documented conflict-of-interest record. The engine computes these per-seat classifications from power, exit, and role; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: pharmaceutical_industry (mobile exit, arbitrage-grade) sits nearest the beneficiary end; behavioral_conformity_institutions (constrained exit) slightly higher; the establishment is low-d but identity_locked — its professional identity is constituted by the disease framing, so it cannot abandon the constraint without dissolving the profession's boundary against psychology, social work, and peer support, which is why enforcement persists even as the validation gap widens. Victim declarations drive high directionality: individuals_meeting_diagnostic_thresholds are powerless with trapped exit — the label persists in records, refusal forfeits treatment access and legal recognition — placing them near the full-target end and amplifying effective extraction; coalition capacity exists (the survivor movements) but operates outside the criteria-setting table. One override: the derivation keys on primary role and would read practicing_clinicians (moderate power, primary role beneficiary) at low d near 0.15; their dual position — paid by the grid, bound to categorical practice, with alternatives available only off-grid at personal cost — warrants d = 0.3, authored as an override on the moderate power atom, the only moderate-power agent in the story.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a shared, reliable classification that could carry psychiatry's claim to medical legitimacy — is half delivered: the nomenclature communicates, and even the readings that contest the kernel use its categories. The other half, the biomedical validation the reading itself promises, remains undelivered after four decades and two framework revisions. The mandatrophy risk is precisely this split: an arrangement persisting on its delivered coordination half while drawing coercive and market warrant from its undelivered validation half. Classifying the constraint as pure coordination would erase the extraction (coercion on unvalidated categories); classifying it as pure extraction would erase the coordination (a working shared language). The tangled-rope claim holds both halves apart. R5 mismatch check: founding_problem_status is contested (not dead) and the disappearance verdict is world_rearranges, so no dead-mandate zombie flag fires — but the contested status is itself the signal that the warrant's credit line is the live question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dsm_kernel_sibling_reading_delta,
    'This story instantiates the biomedical_reading of the dsm_taxonomy_kernel; the kernel''s disagreement is located in what the diagnostic categories track — disease structure, market structure, or institutional conformity demands. How would the sibling readings (critical_psychiatry_reading, neurodiversity_reading) restructure the victim set, beneficiary set, and extraction mechanism over the same standing arrangement?',
    'Compile the sibling stories (dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel__neurodiversity_reading) over the same referent and compare authored epsilon, victim sets, and capture seats; the divergence across the linked family is the kernel''s contest made measurable.',
    'Under the critical reading, epsilon rises toward market-construction levels with pharmaceutical_industry as the capture seat; under the neurodiversity reading the victim set narrows to neurologically atypical individuals and the coordination function re-reads as behavioral-conformity enforcement. This reading''s own-lights epsilon (0.64) is the floor of the kernel''s contested range, not its verdict.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dsm_kernel_sibling_reading_delta, conceptual, 'Committer structure: one reading of the DSM taxonomy kernel; sibling readings restructure victims, beneficiaries, and mechanism over the same referent.').

omega_variable(
    disease_entity_discovery_gap,
    'Will empirical research discover neurobiological entities whose boundaries align with the current DSM categories, or will discovered structure (if any) cross-cut the categories and force reclassification?',
    'Biomarker research programs — RDoC-style dimensional neuroscience, genomics, connectomics: validated neural signatures aligning with category boundaries would close the warrant gap; signatures that cross-cut boundaries would reveal the categories as conventional groupings whose disease warrant was drawn in advance of the evidence.',
    'Alignment collapses the constraint''s extraction toward coordination cost and converts the coercive apparatus into validated medicine; cross-cutting structure exposes four decades of warrant drawn on unearned credit and pushes the constraint''s classification toward pure extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(disease_entity_discovery_gap, empirical, 'Whether the categories'' disease warrant will ever be earned by discovery, and whether discovered entities will respect category boundaries.').

omega_variable(
    diagnosis_identity_internalization,
    'How much of the diagnosed population''s constrained exit is structural (records, reimbursement rules, commitment law) versus internalized (disease identity fused with self-concept, so the classification is experienced as a discovery about oneself rather than an assignment)?',
    'Natural experiments in category retirement and reclassification — homosexuality''s 1973 removal, Asperger''s folding into the autism spectrum: if identity disruption and help-seeking patterns persist after the category falls, internalization carries part of the constraint that the structural measure does not.',
    'Substantial internalization means effective suppression exceeds the structural measure — the constraint travels with the diagnosed after formal barriers fall — and the diagnosed population''s exit options sit closer to identity-locked than the records-and-statutes picture alone suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diagnosis_identity_internalization, empirical, 'Structural versus internalized binding of the diagnosed to the patient role.').

omega_variable(
    diagnostic_threshold_conventionality,
    'Do the diagnostic thresholds that define the victim set track boundaries in nature, or are they committee conventions whose location determines who bears the arrangement''s costs?',
    'Taxometric and latent-structure research on the underlying constructs: taxonic structure would give the thresholds a natural anchor; dimensional structure would make the threshold a pure convention and the victim set a population drawn by vote.',
    'Dimensional findings would mean the incidence of the arrangement''s costs is set by the threshold''s location rather than by any natural boundary — the victim set is a policy choice dressed as a discovery — which raises the arrangement''s extractiveness and shifts classification toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diagnostic_threshold_conventionality, empirical, 'Whether diagnostic thresholds discover natural boundaries or constitute the victim set by convention.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__biomedical_reading, 1980, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t1980, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement_basis(dsm__tr_t1980, observed).
narrative_ontology:measurement(dsm__tr_t1987, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 1987, 0.28).
narrative_ontology:measurement_basis(dsm__tr_t1987, observed).
narrative_ontology:measurement(dsm__tr_t1994, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 1994, 0.33).
narrative_ontology:measurement_basis(dsm__tr_t1994, observed).
narrative_ontology:measurement(dsm__tr_t2000, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement_basis(dsm__tr_t2000, observed).
narrative_ontology:measurement(dsm__tr_t2013, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2013, 0.5).
narrative_ontology:measurement_basis(dsm__tr_t2013, observed).
narrative_ontology:measurement(dsm__tr_t2019, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2019, 0.48).
narrative_ontology:measurement_basis(dsm__tr_t2019, observed).
narrative_ontology:measurement(dsm__tr_t2026, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2026, 0.45).
narrative_ontology:measurement_basis(dsm__tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(dsm__be_t1980, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement_basis(dsm__be_t1980, observed).
narrative_ontology:measurement(dsm__be_t1987, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 1987, 0.5).
narrative_ontology:measurement_basis(dsm__be_t1987, observed).
narrative_ontology:measurement(dsm__be_t1994, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 1994, 0.58).
narrative_ontology:measurement_basis(dsm__be_t1994, observed).
narrative_ontology:measurement(dsm__be_t2000, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement_basis(dsm__be_t2000, observed).
narrative_ontology:measurement(dsm__be_t2013, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2013, 0.66).
narrative_ontology:measurement_basis(dsm__be_t2013, observed).
narrative_ontology:measurement(dsm__be_t2019, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2019, 0.66).
narrative_ontology:measurement_basis(dsm__be_t2019, observed).
narrative_ontology:measurement(dsm__be_t2026, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2026, 0.64).
narrative_ontology:measurement_basis(dsm__be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t1980, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 1980, 0.52).
narrative_ontology:measurement_basis(dsm__su_t1980, observed).
narrative_ontology:measurement(dsm__su_t1987, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 1987, 0.55).
narrative_ontology:measurement_basis(dsm__su_t1987, observed).
narrative_ontology:measurement(dsm__su_t1994, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 1994, 0.6).
narrative_ontology:measurement_basis(dsm__su_t1994, observed).
narrative_ontology:measurement(dsm__su_t2000, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement_basis(dsm__su_t2000, observed).
narrative_ontology:measurement(dsm__su_t2013, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2013, 0.66).
narrative_ontology:measurement_basis(dsm__su_t2013, observed).
narrative_ontology:measurement(dsm__su_t2019, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2019, 0.64).
narrative_ontology:measurement_basis(dsm__su_t2019, observed).
narrative_ontology:measurement(dsm__su_t2026, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2026, 0.62).
narrative_ontology:measurement_basis(dsm__su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__biomedical_reading, information_standard).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel__critical_psychiatry_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel__neurodiversity_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, involuntary_commitment_law).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, psychotropic_reimbursement_coding).

% DUAL FORMULATION NOTE:
% This story is one member of a three-reading family over the dsm_taxonomy_kernel, decomposed per the epsilon-invariance principle: the colloquial label 'what the DSM categories are' covers structurally distinct claims with different epsilon, victim sets, and failure modes. The biomedical reading (this story) authors epsilon = 0.64 by its own lights — the warrant runs ahead of the validation its own empirical standards require. The critical_psychiatry_reading authors higher epsilon over the same referent (market construction; pharmaceutical_industry as capture seat), and the neurodiversity_reading authors high epsilon with a narrowed victim set (pathologized variation; conformity institutions as the demand side). The biomedical reading is upstream in the family: its validation promise is what the sibling readings contest, and its warrant is what the downstream legal and reimbursement arrangements (involuntary_commitment_law, psychotropic_reimbursement_coding) draw on. Each member is a clean single-epsilon constraint; the kernel's contest is measurable only as divergence across the linked family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dsm_taxonomy_kernel__biomedical_reading, moderate, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
