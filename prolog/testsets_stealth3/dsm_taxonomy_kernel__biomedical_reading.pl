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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: dsm_taxonomy_kernel__biomedical_reading
 *   human_readable: DSM Categories as Objective Neurobiological Disease Entities (Biomedical Reading)
 *   domain: medical_epistemology/psychiatric_taxonomy/social_construction_of_illness
 *
 * SUMMARY:
 *   The biomedical reading holds that DSM categories are provisional
 *   descriptions of objective neurobiological disease entities that empirical
 *   research will progressively validate. As an operative constraint, the
 *   claim structures psychiatric practice: diagnosis through DSM criteria is
 *   the gateway to treatment, reimbursement, legal accommodation, and
 *   research participation, and the claim's authority licenses involuntary
 *   treatment and capacity adjudication for anyone meeting threshold
 *   criteria. The standing arrangement under contest is the DSM-governed
 *   diagnostic regime as operated since DSM-III (1980). KEY AGENTS (by
 *   structural relationship): - psychiatric_professional_establishment:
 *   agenda-setter (institutional/identity_locked) — writes the criteria,
 *   adjudicates disorder, collects authority; - pharmaceutical_industry:
 *   primary beneficiary (institutional/arbitrage) — collects diagnosis-keyed
 *   revenue without running the taxonomy; -
 *   behavioral_conformity_institutions: secondary beneficiary
 *   (institutional/constrained) — schools, courts, employers, disability
 *   systems gain a legible handle on behavior; -
 *   individuals_meeting_diagnostic_thresholds: primary target
 *   (powerless/trapped) — bear labeling, coercion risk, and capacity loss,
 *   with conditional care access; - psychiatric_survivor_advocacy_networks:
 *   organized contesting seat (organized/constrained) — bear the costs and
 *   contest the frame from outside criteria-setting; -
 *   philosophers_of_psychiatry: analytical observer — sees the full structure
 *   and collects nothing.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__biomedical_reading, 0.63).
domain_priors:suppression_score(dsm_taxonomy_kernel__biomedical_reading, 0.68).
domain_priors:theater_ratio(dsm_taxonomy_kernel__biomedical_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, extractiveness, 0.63).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__biomedical_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__biomedical_reading, "DSM Categories as Objective Neurobiological Disease Entities (Biomedical Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__biomedical_reading, "medical_epistemology/psychiatric_taxonomy/social_construction_of_illness").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__biomedical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__biomedical_reading, '115fd6b8-9552-4025-9f3c-5a9c0076d750').
narrative_ontology:cs_kernel_codification('115fd6b8-9552-4025-9f3c-5a9c0076d750', formalized).
narrative_ontology:cs_authority_grounding('115fd6b8-9552-4025-9f3c-5a9c0076d750', expertise).
narrative_ontology:cs_interpretation_layer_present('115fd6b8-9552-4025-9f3c-5a9c0076d750').
narrative_ontology:cs_reading_relation('115fd6b8-9552-4025-9f3c-5a9c0076d750', dsm_taxonomy_kernel__neurodiversity_reading, forecloses).
narrative_ontology:cs_reading_relation('115fd6b8-9552-4025-9f3c-5a9c0076d750', dsm_taxonomy_kernel__critical_psychiatry_reading, coexists_with).
narrative_ontology:cs_axiom('115fd6b8-9552-4025-9f3c-5a9c0076d750', foundational, mental_disorders_are_objective_neurobiological_disease_entities).
narrative_ontology:cs_axiom_status(mental_disorders_are_objective_neurobiological_disease_entities, holdable).
narrative_ontology:cs_axiom_grounding('115fd6b8-9552-4025-9f3c-5a9c0076d750', mental_disorders_are_objective_neurobiological_disease_entities, empirically_contingent).
narrative_ontology:cs_axiom('115fd6b8-9552-4025-9f3c-5a9c0076d750', foundational, operational_criteria_are_the_ladder_to_biological_validity).
narrative_ontology:cs_axiom_status(operational_criteria_are_the_ladder_to_biological_validity, holdable).
narrative_ontology:cs_axiom_grounding('115fd6b8-9552-4025-9f3c-5a9c0076d750', operational_criteria_are_the_ladder_to_biological_validity, empirically_contingent).
narrative_ontology:cs_reference_frame('115fd6b8-9552-4025-9f3c-5a9c0076d750', neo_kraepelinian_validity_program).
narrative_ontology:cs_drift_state('115fd6b8-9552-4025-9f3c-5a9c0076d750', post_rdoc_genomic_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('115fd6b8-9552-4025-9f3c-5a9c0076d750', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, psychiatric_professional_establishment).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, behavioral_conformity_institutions).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, individuals_meeting_diagnostic_thresholds).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, individuals_meeting_diagnostic_thresholds).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, psychiatric_survivor_advocacy_networks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and periodically revises the diagnostic manual through appointed workgroups, controls residency training, board certification, and the flagship journals, and adjudicates disputes over what counts as a mental disorder. Members' careers, standing, and daily practice are constituted inside the medical-model framework; a senior figure who publicly renounced the framework would forfeit standing within it. Collects deference, definitional authority over disordered behavior, and the social authority that attaches to medical legitimacy.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, psychiatric_professional_establishment, agenda_setter,
    institutional, generational, identity_locked, global).

% Develops, patents, and markets medications keyed to diagnostic categories; funds registration trials, key-opinion-speaker networks, journal supplements, and continuing education. Revenue arrives whenever a category is diagnosed and treated. Portfolio strategy can reposition compounds across neighboring categories or withdraw from failing ones, so exposure to any single category is hedged.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_industry, beneficiary,
    institutional, biographical, arbitrage, global).

% Schools, courts, employers, disability agencies, and militaries use recognized diagnostic categories as a legible, legally defensible instrument for routing behavior: accommodations, excused absence, competency adjudication, fitness-for-duty determinations, treatment courts. They did not build the classification and could adapt to a successor, but their procedures, forms, and accumulated case law are built around the current one, making switching costly.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, behavioral_conformity_institutions, beneficiary,
    institutional, generational, constrained, national).

% Receives a diagnosis on meeting threshold criteria and thereafter carries the label through employment, insurance, custody, professional licensing, and background checks. Through the same label they may obtain medication, therapy, accommodations, disability status, and freedom from blame. Declining the label can mean losing access to care; in acute states it can invite evaluation and treatment they did not seek. Individually they hold little leverage; collectively they have begun organizing.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, individuals_meeting_diagnostic_thresholds, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__biomedical_reading, individuals_meeting_diagnostic_thresholds, beneficiary).

% Networks of people who have received diagnoses and experienced detention, forced medication, or seclusion, organized to contest the framework that authorized their treatment. They publish, litigate under disability-rights conventions, and advise some guideline bodies, but hold no seats on the manual's criteria workgroups and no veto over revisions; their testimony enters the process late and advisory.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, psychiatric_survivor_advocacy_networks, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__biomedical_reading, psychiatric_survivor_advocacy_networks, excluded).

% Scholars of classification, validity, and disease concepts who analyze whether the categories carve nature or project practice. They collect no revenue and bear no labels; their publications circulate among the disputants and occasionally supply the vocabulary in which crises of validity are articulated.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, philosophers_of_psychiatry, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_industry).
narrative_ontology:fixing_cost_class(dsm_taxonomy_kernel__biomedical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single operationalized vocabulary for mental disorder: clinicians communicate cases, insurers reimburse episodes, researchers aggregate samples, regulators approve indications, and patients obtain access to care and legal accommodation through recognized categories. It solves the collective-action problem of reliable, shared classification across a distributed care system.
% TRANSFER_FUNCTION: Moves diagnosis-keyed prescription and service revenue from patients and public payers to pharmaceutical firms and provider institutions; moves consent, liberty, and decision-making capacity from individuals meeting diagnostic thresholds to professionals, courts, and designated guardians; moves definitional authority over disordered behavior to the psychiatric establishment.
% ABSENT_VOICES: Individuals undergoing emergency commitment proceedings cannot effectively object while subject to the machinery the criteria activate. Sub-threshold populations affected by criterion creep have no seat in the workgroups. Non-Western communities whose idioms of distress are translated into categories are minimally represented among criterion authors. Practitioners of non-diagnostic care modalities are kept out by the reimbursement architecture rather than by argument.
% DISAPPEARANCE_RATIONALE: If the claim and its regime vanished overnight, reimbursement for psychiatric care would collapse until a successor classification emerged, thousands of trials would lose construct validity, legal accommodation and disability frameworks would lose their sorting instrument, and involuntary-treatment authority would lose its current warrant. Care would reorganize around whatever replacement nosology or practice standard consolidated fastest.
% FOUNDING_PROBLEM: Pre-1980 psychiatric diagnosis was unreliable: the US-UK cross-national project showed agreement between psychiatrists scarcely above chance for the major categories, psychoanalytic formulations resisted falsification, and research could not aggregate heterogeneous samples. DSM-III was built to solve reliability through operational criteria, carrying the promissory note (Robins and Guze) that valid disease entities would be discovered beneath the descriptions.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated from outside the benefiting parties by the archived cross-national reliability studies that motivated DSM-III and by the profession's own published acknowledgment that reliability was achieved. Its unresolved half — validity — is attested as open by the national research funder's 2013 announcement abandoning the categories as a basis for basic research, and by the psychometric literature on within-category heterogeneity; no party outside the beneficiary set attests that the validity promissory note has been paid.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__biomedical_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__biomedical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__biomedical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dsm_taxonomy_kernel__biomedical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__biomedical_reading, 0.63, 'stealth/ox-alpha', 'none', direct).

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
 *   Claim and metrics are authored independently. The claimed type is tangled_rope because the structure shows a genuine coordination function (shared vocabulary, care access, research aggregation), asymmetric extraction (coercion, capacity adjudication, diagnosis-keyed revenue concentrated on the diagnosed), and active enforcement (commitment statutes, capacity law, reimbursement gatekeeping, licensure) — all three canonical requirements are declared. Extractiveness is 0.63: the ε referent is the standing arrangement under contest — the DSM-governed regime as operated — assessed by this reading's own lights, never the completed biomarker-grounded nosology the reading endorses. By its own lights the reading must count as extraction every burden the arrangement imposes beyond what validated-disease status would justify, and with category-specific biomarkers still largely missing after forty-five years, that unjustified residue is substantial but not total (crisis intervention, symptom relief, and the communication infrastructure are real). Suppression is 0.68, authored as a raw structural property and deliberately NOT scaled by power or scope — only extractiveness is scaled downstream by directionality and scope. Theater is 0.42: laboratory-medicine performance without laboratories, and biomarker programs persisting past repeated null results, offset by genuinely functional clinical activity. Accessibility_collapse is 0.55: alternatives (Open Dialogue, Soteria-style care, hearing-voices networks, peer support, psychosocial formulation) persist but are reimbursement-marginalized. Resistance is 0.60: organized survivor movements, critical-psychiatry networks, and disability-rights litigation. The temporal series run on one shared grid (1980, 1990, 2000, 2010, 2018, 2025) with every tracked metric authored at every point. The trajectories are not cyclical: extraction and enforcement rose monotonically to a 2010 peak, then partially corrected under the research funder's validity critique and rights-based pressure — a correction driven by external challenge, not intermittent reinforcement.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical structural inputs. The establishment seat sits at the agenda-setting position with identity-locked exit: the lock is institutional-professional identity fusion — the profession's self-concept is constituted by the medical model, so challenges register as attacks on medicine itself, and revision is absorbed by interpretive layers (text revisions, dimensional appendices) rather than surfaced as kernel change. Were that identity frame to break, the establishment's computed position would shift sharply toward the payer-side experience of its own constraint. The pharmaceutical seat holds arbitrage exit: it needs the categories only as demand-generating instruments and can reposition around any of them, computing the mildest attachment in the set. The diagnosed seat is trapped: the same label that burdens them is their only reliable key to care and accommodation, producing an ambivalence a simple victim frame misses. The survivor-network seat converts lived cost into organized contestation — the coalition channel through which individually powerless diagnosed persons acquire leverage. The engine derives these divergent per-seat classifications from the power, exit, and role data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations place the establishment, the industry, and the conformity institutions on the subsidized side (low d): the establishment collects the authority it administers, the industry collects revenue without running the taxonomy, and the conformity institutions collect a ready-made sorting instrument. The victim declaration places individuals meeting diagnostic thresholds on the target side (high d); their trapped exit amplifies effective extraction toward the full-target end, while their secondary beneficiary position (conditional care access through the label) damps it partway back — the net sits near but not at the full-target pole. Survivor networks derive high d moderated by organized power. The philosophical observers sit analytically outside the flow. The derivation chain from these declarations plus exit options produces accurate directionalities, so no directionality overrides are authored.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem's reliability half was achieved; the validity promissory note remains unpaid after forty-five years, keeping founding_problem_status contested rather than dead — so the status-times-verdict consumer finds no zombie flag (contested status with a world_rearranges verdict is the honest live-contest cell, cross-checked against a theater_ratio of 0.42 that is elevated but not piton-range). The tangled_rope classification prevents two opposite mislabels: reading the arrangement as pure extraction would erase the real coordination function a replacement would have to rebuild; reading it as pure coordination would erase the asymmetric burdens that ride on the claim's authority beyond its current evidence. Trajectory watch: if category-specific biomarkers validate, the constraint drifts toward rope; if the validity program is formally abandoned while enforcement persists, it drifts toward snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_kind_or_constructed_classification,
    'Are the DSM categories a discovered natural-kind structure awaiting biological articulation, as this reading asserts, or a socially maintained classification regime whose apparent naturality is an artifact of its own enforcement?',
    'Outcome of preregistered biomarker validation programs and genomic consortium analyses: progressive category-specific biological convergence would vindicate natural-kind status; continued cross-category polygenic overlap and within-category heterogeneity would indicate a constructed structure.',
    'Natural-kind vindication drives the constraint toward rope (burdens become justified treatment of real disease); a constructed verdict triggers false-summit reclassification toward extracted coordination types and reassigns the victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_kind_or_constructed_classification, empirical, 'Whether the taxonomy''s naturality is discovered or enacted.').

omega_variable(
    sibling_neurodiversity_victim_set_delta,
    'This constraint is the biomedical_reading of kernel dsm_taxonomy_kernel; the neurodiversity_reading instantiates a different constraint in which the categories pathologize natural neurological variation — which structural elements change under that sibling?',
    'Conceptual comparison with the sibling story''s authored victim set (neurologically atypical individuals whose variation conflicts with institutional norms) and its inverted justification structure; no empirical resolution — the readings assign different ontologies to the same objects.',
    'Under the sibling reading the victim set widens to include sub-threshold and non-distressed atypical individuals, the coordination function relocates to norm-enforcement, and epsilon rises; classification of this story is unaffected unless a framework adopts the sibling ontology.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_neurodiversity_victim_set_delta, conceptual, 'Committer delta versus the neurodiversity sibling reading.').

omega_variable(
    sibling_critical_psychiatry_genealogy_delta,
    'The critical_psychiatry_reading reads the same categories as reverse-engineered from available treatments to construct markets — does the historical record support discovery-driven or market-driven category formation?',
    'Archival analysis of criterion-workgroup industry ties, ghost-authorship records, and the temporal ordering of drug approval versus criterion broadening (pediatric bipolar, adult attention-deficit dosing thresholds).',
    'Strong market-driven evidence shifts the family''s center of gravity toward the sibling''s higher-epsilon reading and dates this reading''s axiom-overriding drift earlier; discovery-driven evidence stabilizes this reading''s warrant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_critical_psychiatry_genealogy_delta, empirical, 'Genealogy dispute location versus the critical-psychiatry sibling.').

omega_variable(
    biomarker_discovery_epsilon_coupling,
    'If robust category-specific biomarkers were validated tomorrow, would the arrangement''s extraction collapse to coordination cost, or do the burdens (coercion, capacity loss, stigma) persist independently of the claim''s truth?',
    'Compare burden intensity across categories with differing current biological grounding (narcolepsy with verified biomarkers versus borderline personality disorder without): does validated biology reduce coercion and burden proportionally?',
    'If burdens persist despite validation, part of epsilon is claim-independent and the constraint stays extractive even under vindicated naturality; if burdens track validation, epsilon is fully coupled to the foundational axiom''s empirical fate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biomarker_discovery_epsilon_coupling, empirical, 'Whether extraction is contingent on the disease-model claim''s truth.').

omega_variable(
    suppression_structural_internalized_split,
    'Of the measured suppression, how much is structural (commitment statutes, reimbursement gatekeeping, licensure control) and how much internalized (clinician identity fusion with the medical model, patient self-pathologization that persists after external barriers lift)?',
    'Post-liberalization trajectory analysis: in jurisdictions that loosened commitment law or decoupled reimbursement from diagnosis, does dissenting practice and help-seeking normalize, or do internalized frames sustain the old pattern?',
    'A large internalized share raises effective suppression above the structural measure and predicts slow decay of enforcement even after statute reform; a structural share predicts rapid relaxation if the legal architecture changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_internalized_split, empirical, 'Composition of suppression between external barriers and internalized frames.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__biomedical_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t1980, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(dsm__tr_t1990, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 1990, 0.28).
narrative_ontology:measurement(dsm__tr_t2000, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2000, 0.33).
narrative_ontology:measurement(dsm__tr_t2010, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2010, 0.44).
narrative_ontology:measurement(dsm__tr_t2018, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2018, 0.46).
narrative_ontology:measurement(dsm__tr_t2025, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(dsm__be_t1980, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(dsm__be_t1990, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 1990, 0.52).
narrative_ontology:measurement(dsm__be_t2000, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(dsm__be_t2010, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2010, 0.66).
narrative_ontology:measurement(dsm__be_t2018, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2018, 0.63).
narrative_ontology:measurement(dsm__be_t2025, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2025, 0.63).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t1980, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(dsm__su_t1990, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(dsm__su_t2000, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement(dsm__su_t2010, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(dsm__su_t2018, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2018, 0.67).
narrative_ontology:measurement(dsm__su_t2025, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2025, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__biomedical_reading, information_standard).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel__neurodiversity_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel__critical_psychiatry_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, involuntary_treatment_authority).

% DUAL FORMULATION NOTE:
% One kernel (dsm_taxonomy_kernel), three readings, three constraints. This file instantiates the biomedical_reading: categories as discoverable disease entities, epsilon 0.63, victims = individuals meeting diagnostic thresholds. The neurodiversity_reading assigns a different ontology (natural variation pathologized by institutional norms) and a wider victim set; the critical_psychiatry_reading assigns a market-construction genealogy and a near-snare epsilon. Per DP-001 the readings are separate files with separate epsilon values; this reading holds the upstream position (dominant institutional warrant), so it creates structural pressure on both siblings' operating environment while the siblings contest its foundational axiom.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
