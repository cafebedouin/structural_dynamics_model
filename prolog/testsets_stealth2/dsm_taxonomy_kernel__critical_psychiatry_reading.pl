% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__critical_psychiatry_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dsm_taxonomy_kernel__critical_psychiatry_reading, []).

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
 *   constraint_id: dsm_taxonomy_kernel__critical_psychiatry_reading
 *   human_readable: DSM Categories as Treatment-Derived Market Construction (Critical Psychiatry Reading)
 *   domain: medical epistemology / psychiatric taxonomy / social construction of illness
 *
 * SUMMARY:
 *   This story instantiates the critical_psychiatry_reading of the
 *   dsm_taxonomy_kernel: the claim that DSM categories are reverse-engineered
 *   from available pharmaceutical treatments to construct markets for
 *   psychotropic drugs. The standing arrangement under contest — and the
 *   epsilon referent, assessed by this reading's own lights — is the
 *   classification-and-reimbursement apparatus as it has actually operated
 *   since DSM-III (1980): operationalized criteria that define prescribable
 *   populations, workgroups populated by psychiatrists with manufacturer
 *   funding ties, category boundary expansions that tracked newly patentable
 *   compounds (the SSRI era's broadened mood and anxiety categories,
 *   atypical-antipsychotic-era bipolar and pediatric bipolar expansion), and
 *   reimbursement systems that make a manual code the price of treatment,
 *   research, and legal standing. The reading does not deny that the manual
 *   coordinates clinical communication — that genuine coordination function
 *   is precisely what makes the structure a hybrid rather than pure
 *   extraction. Sibling readings (biomedical_reading, neurodiversity_reading)
 *   are separate constraints with their own epsilon and victim sets; they are
 *   not averaged into this story. Assumptions stated: interval t0 = DSM-III
 *   publication (1980), t46 = 2026; claimed_type and metrics are
 *   independently authored from this reading's seat, and the engine computes
 *   per-seat classifications from the structural data.
 *
 * KEY AGENTS:
 *   - psychopharmaceutical_manufacturers: Primary beneficiary (institutional/arbitrage) — receives the prescription revenue the category-treatment alignment channels; portfolio breadth makes classification shift a repositioning problem, not an existential one
 *   - industry_funded_psychiatrists: Secondary beneficiary (powerful/identity_locked) — key opinion leaders and guideline authors whose authority and income ride on the pharmacological frame they help write
 *   - apa_publishing_operation: Agenda setter and royalty collector (institutional/identity_locked) — convenes the workgroups, owns the text, collects per-edition revenue; its standing is fused to the artifact it administers
 *   - overprescribed_patients: Primary target (powerless/trapped) — bear adverse effects, metabolic and sexual burden, and discontinuation syndromes; care access runs through a diagnostic code
 *   - psychosocial_care_displaced_patients: Secondary target (powerless/trapped) — distress routed to medication first while therapy, housing, and peer support are deferred or uncovered
 *   - community_psychiatrists: Constrained same-level actor (moderate/constrained) — take no industry money, doubt the categories, and must code to be paid
 *   - service_user_movements: Excluded voice (organized/constrained) — survivor and peer-led networks outside the criteria-writing conversation, building parallel support infrastructure
 *   - fda_regulators: Institutional observer (institutional/analytical) — approve drugs against trial populations the categories define, giving the frame regulatory force without authoring it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.68).
domain_priors:suppression_score(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.6).
domain_priors:theater_ratio(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__critical_psychiatry_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__critical_psychiatry_reading, "DSM Categories as Treatment-Derived Market Construction (Critical Psychiatry Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__critical_psychiatry_reading, "medical epistemology / psychiatric taxonomy / social construction of illness").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__critical_psychiatry_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__critical_psychiatry_reading, '384b3587-02e7-4b27-9339-1ece8b82519e').
narrative_ontology:cs_kernel_codification('384b3587-02e7-4b27-9339-1ece8b82519e', fixed_text).
narrative_ontology:cs_authority_grounding('384b3587-02e7-4b27-9339-1ece8b82519e', extraction).
narrative_ontology:cs_interpretation_layer_present('384b3587-02e7-4b27-9339-1ece8b82519e').
narrative_ontology:cs_reading_relation('384b3587-02e7-4b27-9339-1ece8b82519e', dsm_taxonomy_kernel__biomedical_reading, forecloses).
narrative_ontology:cs_reading_relation('384b3587-02e7-4b27-9339-1ece8b82519e', dsm_taxonomy_kernel__neurodiversity_reading, influences).
narrative_ontology:cs_axiom('384b3587-02e7-4b27-9339-1ece8b82519e', foundational, categories_reverse_engineered_from_treatments).
narrative_ontology:cs_axiom_status(categories_reverse_engineered_from_treatments, holdable).
narrative_ontology:cs_axiom_grounding('384b3587-02e7-4b27-9339-1ece8b82519e', categories_reverse_engineered_from_treatments, empirically_contingent).
narrative_ontology:cs_axiom('384b3587-02e7-4b27-9339-1ece8b82519e', secondary, pharmaco_commercial_frame_displaces_psychosocial_care).
narrative_ontology:cs_axiom_status(pharmaco_commercial_frame_displaces_psychosocial_care, holdable).
narrative_ontology:cs_axiom_grounding('384b3587-02e7-4b27-9339-1ece8b82519e', pharmaco_commercial_frame_displaces_psychosocial_care, empirically_contingent).
narrative_ontology:cs_reference_frame('384b3587-02e7-4b27-9339-1ece8b82519e', treatment_derived_market_nosology).
narrative_ontology:cs_drift_state('384b3587-02e7-4b27-9339-1ece8b82519e', contemporary_transparency_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('384b3587-02e7-4b27-9339-1ece8b82519e', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, psychopharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, industry_funded_psychiatrists).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, apa_publishing_operation).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, overprescribed_patients).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, psychosocial_care_displaced_patients).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, community_psychiatrists).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, community_psychiatrists).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__critical_psychiatry_reading, operationalized_diagnostic_reliability).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmacological_first_line_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Owns and publishes the diagnostic manual, convenes the workgroups that write category criteria, and collects royalties from each edition's sales. Its authority standing in American psychiatry is bound to the manual it administers, and each revision renews both the text and the publishing revenue. Operating outside the diagnostic framework it defines would mean dissolving the artifact its institutional standing and revenue rest on.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, apa_publishing_operation, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__critical_psychiatry_reading, apa_publishing_operation, beneficiary).

% Develop, patent, and market psychotropic compounds. Revenue depends on the indications a diagnosis licenses: each category defines a prescribable population, and boundary expansion widens the market for the compounds aligned with it. They fund trials, place and ghost-write publications, and cultivate prescriber relationships. Their portfolios span many therapeutic areas, so a shift in psychiatric classification is a repositioning problem rather than an existential threat.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, psychopharmaceutical_manufacturers, beneficiary,
    institutional, biographical, arbitrage, global).

% Key opinion leaders, guideline authors, and trial investigators who receive consulting fees, speaker-bureau income, and research funding from manufacturers. Their professional standing, income, and research programs are built on the pharmacological framing of the categories they help define. Leaving that framing would mean abandoning the career structure, authorship networks, and authority position they occupy.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, industry_funded_psychiatrists, beneficiary,
    powerful, biographical, identity_locked, national).

% Receive prescriptions under categories whose criteria they meet on paper, often at doses and combinations driven by formulary habit and prescriber volume rather than measured need. They bear sedation, weight gain, metabolic and sexual side effects, and discontinuation syndromes; stopping is medically risky and non-drug alternatives are rarely offered first. Their access to any care at all runs through a diagnostic code.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, overprescribed_patients, payer,
    powerless, biographical, trapped, national).

% Present with distress that gets routed to medication first because the diagnostic encounter is organized around prescribable categories. Therapy, housing support, employment help, and peer support are unavailable, uncovered, or deferred behind pharmacological treatment; their problems are re-described as chemical states their conduct and circumstances cannot touch.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, psychosocial_care_displaced_patients, payer,
    powerless, biographical, trapped, national).

% Clinicians in public and private practice who take no industry money and often doubt the categories they must use. Reimbursement, medical records, disability determinations, and court proceedings all require a manual code, so they code first and treat second. They gain a shared professional language and lose the ability to bill, refer, or defend a case outside it.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, community_psychiatrists, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__critical_psychiatry_reading, community_psychiatrists, beneficiary).

% Survivor and peer-led networks organizing around lived experience of diagnosis and medication — hearing-voices groups, peer-support collectives, survivor researchers. They campaign for non-diagnostic understandings of distress and have built parallel support infrastructure, but hold no seats on the workgroups that draft criteria and are consulted, if at all, after categories are written.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, service_user_movements, excluded,
    organized, generational, constrained, global).

% Review psychotropic drug applications whose trial populations are defined by the manual's categories and issue labeling that loops back into prescribing behavior. They audit evidence within the frame the categories set; they do not author the categories, but their approval decisions give the frame regulatory force.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, fda_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dsm_taxonomy_kernel__critical_psychiatry_reading, psychopharmaceutical_manufacturers).
narrative_ontology:fixing_cost_class(dsm_taxonomy_kernel__critical_psychiatry_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The manual supplies a shared operationalized vocabulary that lets clinicians communicate diagnoses across settings, lets insurers process claims, lets researchers define and compare trial populations, and lets courts and agencies make standardized determinations about disability, competence, and commitment.
% TRANSFER_FUNCTION: Moves prescription revenue from patients and health systems to drug manufacturers; moves consulting, speaker, and research money from manufacturers to influential psychiatrists; moves royalty income from manual sales to the publishing operation; and moves patients' distress into pharmacological treatment channels while displacing non-drug alternatives behind reimbursement gates.
% ABSENT_VOICES: Service users and survivors, peer-support practitioners, and critical clinicians hold no seats on the workgroups that draft criteria; they would contest the pharmacological framing and the boundary-drawing that licenses prescribing populations. They sit outside the revision process — organized, but consulted after drafting if at all — so the consensus around each category reflects the room it was written in, not the people it is applied to.
% DISAPPEARANCE_RATIONALE: Prescribing patterns, reimbursement processing, research programs, guideline authority, and the APA's institutional standing all run through the manual. An overnight disappearance would force medicine to rebuild a classification from scratch, insurers to retool claims processing, manufacturers to re-justify every indication, and courts to reconstruct competency and disability standards — a wholesale reorganization, not a return to some pre-existing natural order.
% FOUNDING_PROBLEM: Before 1980, psychiatric diagnosis was unreliable across clinicians and schools — the same patient received different diagnoses from different psychiatrists — undermining clinical communication, research replication, and the profession's claim to scientific standing.
% FOUNDING_PROBLEM_CORROBORATION: Psychiatric historians (Shorter, Grob) and former task-force leadership turned critic (Frances) attest from outside the current benefiting parties that operationalized criteria achieved the reliability goal by the mid-1980s and that later revisions served boundary expansion rather than reliability; service-user scholarship independently corroborates that the manual's current function is not the one it was founded for. The APA does not attest the founding problem is dead — it continues to cite reliability and clinical utility in defense of each revision — which is itself the expected position of the seat that collects from the arrangement.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__critical_psychiatry_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__critical_psychiatry_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__critical_psychiatry_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dsm_taxonomy_kernel__critical_psychiatry_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dsm_taxonomy_kernel__critical_psychiatry_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dsm_taxonomy_kernel__critical_psychiatry_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dsm_taxonomy_kernel__critical_psychiatry_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.68 is moderate-high per this reading: the transfer is real (blockbuster-era revenues tied to category expansion, industry payments to the psychiatrists who write criteria, per-edition royalties), but the manual also performs genuine coordination work, so the value is not maximal. Suppression 0.60 is structural, not overt coercion: reimbursement codes, formulary placement, guideline compliance, and licensure expectations gate practice, and dissenting frameworks are marginalized rather than banned. Theater 0.50: the validity apparatus (biomarker research programs, entity language) performs more science than it delivers per this reading, while the reliability function is real — hence half, not more. Accessibility_collapse 0.55: alternatives (ICD variants, RDoC, psychodynamic formulation, peer support) persist but reimbursement collapse is strong; a clinician can think outside the manual but rarely bill outside it. Resistance 0.60: critical psychiatry networks, service-user movements, internal dissent from former task-force leadership, and the 2013 RDoC defection. The measurement series run on one shared grid (t = 0, 8, 16, 24, 32, 40, 46). The suppression_requirement series is authored because this story specifically tracks enforcement-capacity change: enforcement machinery matured through the managed-care and formulary era (rising to t24), then partially eroded under transparency reforms (Open Payments), the RDoC alternative, and generic erosion of blockbuster economics — a plateau with mild decline, not a static picture. The mild late-interval decline in base_extractiveness reflects those same transparency and patent-cliff pressures; it is not a cycle, so no cyclical-pattern machinery is invoked.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats should compute as coordination-subsidized or mildly burdened: from inside the APA and the funded KOL network, the manual is the profession's scientific achievement and the frame is simply what psychiatry is. The payer seats compute extraction-heavy: from a polypharmacy patient's position the same structure is enforced coding followed by pharmacological routing. The sharpest divergence is same-level lateral: industry_funded_psychiatrists and community_psychiatrists hold the same profession, the same licensure, and nominally the same standing, yet experience opposite constraints — one collects from the frame, the other pays into it — differentiated by funding ties, identity lock, and exit. Coalition potential for the powerless seats exists and partially materializes: service_user_movements are the organized form of the payer coalition, which is why the enforcement machinery's marginalization of that seat (excluded rather than merely ignored) is structurally load-bearing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: psychopharmaceutical_manufacturers (beneficiary, arbitrage exit) derive near the full-beneficiary end — the constraint subsidizes them and they can reposition if it shifts; industry_funded_psychiatrists (beneficiary, identity_locked) derive near-beneficiary but less extremely, since their fused identity means they cannot cheaply collect elsewhere; apa_publishing_operation (agenda_setter with secondary beneficiary, identity_locked) derives low d — it administers the structure it collects royalties from. Victims drive the target end: overprescribed_patients and psychosocial_care_displaced_patients (powerless, trapped) derive near full-target, amplified by their trapped exit. community_psychiatrists (payer with secondary beneficiary, constrained) sit mid-range: they pay in coding compliance and collect a shared professional language. service_user_movements are excluded rather than coordinated — their exclusion from the criteria-writing conversation is part of what the enforcement machinery maintains. fda_regulators take the analytical seat. No directionality overrides are authored: the beneficiary/victim plus exit data already produce the correct d for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — inter-clinician diagnostic unreliability — was the genuine coordination crisis the manual was built to solve, and DSM-III's operationalized criteria substantially solved it by the mid-1980s. That achievement is why this story claims tangled_rope rather than snare: the constraint entered the world doing real coordination work, and the corpus should not retro-read the DSM-III era as pure extraction. But the founding problem is dead and the arrangement persists with rising extraction across the blockbuster era — the founding_problem_status x disappearance_verdict mismatch (dead + world_rearranges) is exactly the capture signal this reading exists to assert. The mandatrophy framing prevents both failure modes: it blocks the beneficiary seats' cover story (the manual is not still justified by the reliability problem it solved) and it blocks the reverse error of reading the whole history as snare (the coordination function was and remains real, which is what makes the post-solution persistence capture rather than mere fraud).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dsm_kernel_reading_contingency,
    'This constraint is one reading (critical_psychiatry_reading) of the dsm_taxonomy_kernel; how would the sibling readings (biomedical_reading, neurodiversity_reading) restructure the constraint if adopted?',
    'Comparison across the constraint family: each sibling is authored as its own story with its own epsilon, beneficiary/victim structure, and classification; the disagreement is located in the epistemic direction of category formation (world-to-category discovery, treatment-to-category commercial construction, or norm-to-category pathologizing enforcement).',
    'Adopting the biomedical reading collapses extractiveness toward zero (categories as neutral discovered science); adopting the neurodiversity reading shifts the victim set to pathologized neurological variation and changes the transfer function from revenue flow to norm enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dsm_kernel_reading_contingency, conceptual, 'Committer structure: this story instantiates one reading of a contested kernel; sibling readings are separate constraints, not averaged into this one.').

omega_variable(
    category_formation_causation,
    'Are DSM category boundaries actually reverse-engineered from treatments by commercial causation, or do effective treatments legitimately inform category refinement without market construction?',
    'Litigation-released industry internal documents, timeline correlation of category revisions against patent cycles and launch schedules, ghostwriting and publication-planning archives, and cross-jurisdiction comparison of categories where commercial incentives differ.',
    'Confirmed commercial causation supports the authored extractiveness; a therapeutic-feedback account would drop epsilon sharply and trend the classification toward a coordination arrangement with localized capture rather than market construction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_formation_causation, empirical, 'Whether the treatment-to-category alignment is commercial causation or benign therapeutic feedback.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression structural (reimbursement codes, licensure and formulary gates) or internalized (clinician identity fusion with the pharmacological frame; patients'' internalized chemical-imbalance self-concept), or both?',
    'Post-decoupling trajectory: observe coding behavior and treatment expectations in jurisdictions or pilots that decouple reimbursement from manual codes, and in patient cohorts offered non-diagnostic care pathways; persistence after the structural gate is removed indicates internalized carryover.',
    'If substantially internalized, effective suppression persists after structural reform, the constraint outlives its enforcement machinery, and the theater component of the manual''s scientific framing is higher than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural vs internalized mechanism of the constraint''s suppressive force.').

omega_variable(
    coordination_extraction_separability,
    'Is a shared operationalized classification separable from its commercial alignment — could a manual without treatment-derived category boundaries coordinate clinical communication, reimbursement, and research equally well?',
    'Natural experiments: ICD primary-care and non-commercial classification variants, the NIMH RDoC research framework, and open/non-proprietary diagnostic efforts; compare coordination outcomes where commercial alignment is weaker or absent.',
    'If separable, the commercial alignment is pure overhead riding on a real coordination function; if inseparable, part of the measured extraction is the price of the coordination itself and the tangled_rope reading overstates the extraction component.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the coordination and market-construction components are structurally separable.').

omega_variable(
    apa_revenue_dependence,
    'How much of the APA''s operating revenue and institutional standing depends on the manual and its revision cycle?',
    'APA financial disclosures, royalty reporting, and revision-cycle budgeting documents.',
    'High dependence locks the agenda-setting seat into the frame and raises persistence of the current structure; low dependence would make reform cheaper for the seat that could fix it and weaken the capture reading of its resistance to revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(apa_revenue_dependence, empirical, 'Magnitude of the agenda-setter''s revenue dependence on the kernel text.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__critical_psychiatry_reading, 0, 46).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm_crit_read_tr_t0, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(dsm_crit_read_tr_t0, observed).
narrative_ontology:measurement(dsm_crit_read_tr_t8, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement_basis(dsm_crit_read_tr_t8, observed).
narrative_ontology:measurement(dsm_crit_read_tr_t16, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement_basis(dsm_crit_read_tr_t16, observed).
narrative_ontology:measurement(dsm_crit_read_tr_t24, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 24, 0.45).
narrative_ontology:measurement_basis(dsm_crit_read_tr_t24, observed).
narrative_ontology:measurement(dsm_crit_read_tr_t32, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 32, 0.5).
narrative_ontology:measurement_basis(dsm_crit_read_tr_t32, observed).
narrative_ontology:measurement(dsm_crit_read_tr_t40, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 40, 0.52).
narrative_ontology:measurement_basis(dsm_crit_read_tr_t40, observed).
narrative_ontology:measurement(dsm_crit_read_tr_t46, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 46, 0.5).
narrative_ontology:measurement_basis(dsm_crit_read_tr_t46, observed).

% Extraction over time
narrative_ontology:measurement(dsm_crit_read_be_t0, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(dsm_crit_read_be_t0, observed).
narrative_ontology:measurement(dsm_crit_read_be_t8, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement_basis(dsm_crit_read_be_t8, observed).
narrative_ontology:measurement(dsm_crit_read_be_t16, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement_basis(dsm_crit_read_be_t16, observed).
narrative_ontology:measurement(dsm_crit_read_be_t24, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement_basis(dsm_crit_read_be_t24, observed).
narrative_ontology:measurement(dsm_crit_read_be_t32, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 32, 0.7).
narrative_ontology:measurement_basis(dsm_crit_read_be_t32, observed).
narrative_ontology:measurement(dsm_crit_read_be_t40, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 40, 0.69).
narrative_ontology:measurement_basis(dsm_crit_read_be_t40, observed).
narrative_ontology:measurement(dsm_crit_read_be_t46, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 46, 0.68).
narrative_ontology:measurement_basis(dsm_crit_read_be_t46, observed).

% Suppression requirement over time
narrative_ontology:measurement(dsm_crit_read_su_t0, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(dsm_crit_read_su_t0, observed).
narrative_ontology:measurement(dsm_crit_read_su_t8, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement_basis(dsm_crit_read_su_t8, observed).
narrative_ontology:measurement(dsm_crit_read_su_t16, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement_basis(dsm_crit_read_su_t16, observed).
narrative_ontology:measurement(dsm_crit_read_su_t24, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 24, 0.62).
narrative_ontology:measurement_basis(dsm_crit_read_su_t24, observed).
narrative_ontology:measurement(dsm_crit_read_su_t32, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement_basis(dsm_crit_read_su_t32, observed).
narrative_ontology:measurement(dsm_crit_read_su_t40, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement_basis(dsm_crit_read_su_t40, observed).
narrative_ontology:measurement(dsm_crit_read_su_t46, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 46, 0.6).
narrative_ontology:measurement_basis(dsm_crit_read_su_t46, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__critical_psychiatry_reading, information_standard).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel__biomedical_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel__neurodiversity_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the natural-language label 'the DSM' covers three structurally distinct claims with different epsilon values and different victim sets. The biomedical reading authors epsilon near zero (categories as neutral discovered science); this critical reading authors moderate-high epsilon over the standing classification-and-reimbursement arrangement (categories as treatment-derived market construction, victims = overprescribed and care-displaced patients); the neurodiversity reading authors a different victim set (pathologized neurological variation) and a different transfer function (norm enforcement rather than revenue). The upstream biomedical claim is the one the other two contest: its authority is what the critical reading's market-construction evidence undermines and what the neurodiversity reading's pathologization claim rejects. Each reading is a separate file; this story links to both siblings and documents the decomposition here and in its own kernel_context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
