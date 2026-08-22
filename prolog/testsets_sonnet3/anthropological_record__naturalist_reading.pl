% ============================================================================
% CONSTRAINT STORY: anthropological_record__naturalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_anthropological_record__naturalist_reading, []).

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
 *   constraint_id: anthropological_record__naturalist_reading
 *   human_readable: Naturalist Reading of the Anthropological Record (Evolution/Migration via Scientific Method)
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   This constraint instantiates the naturalist reading of the contested
 *   anthropological-record kernel: the claim that the fossil, genetic, and
 *   archaeological record reveals materialist human origins — evolution by
 *   natural selection, population migration reconstructed from genomics and
 *   stratigraphy — knowable through scientific method. This reading is
 *   generated as a clean, ε-invariant constraint in its own right, separate
 *   from the creationist reading (divine creation event(s)) and the
 *   indigenous epistemology reading (relational continuity via oral
 *   tradition), which are authored as sibling constraints with their own ε
 *   and stakeholder structures. The naturalist reading has a genuine,
 *   high-confidence coordination function (falsifiable, cumulative,
 *   cross-checked knowledge production) but also an increasingly consolidated
 *   credentialing and publication gatekeeping apparatus that extracts
 *   institutional rents and excludes non-credentialed interpreters as a
 *   matter of definitional policy rather than individual evidentiary
 *   assessment — hence tangled_rope rather than a clean rope or mountain.
 *
 * KEY AGENTS:
 *   - credentialed_academic_paleoanthropologists: agenda-setter and beneficiary — controls peer review and interpretive default
 *   - university_anthropology_departments: beneficiary — institutionalizes the framework via curriculum and accreditation
 *   - scientific_publishing_institutions: beneficiary/agenda-setter — gatekeeps publication access
 *   - non_credentialed_field_interpreters, creationist_researchers, indigenous_knowledge_keepers, amateur_fossil_discoverers: payers — bear exclusion costs regardless of evidentiary merit
 *   - general_public_science_consumers: beneficiary — receives predictive, cumulative knowledge without bearing gatekeeping costs
 *   - philosophy_of_science_observers: analytical seat — sees both the coordination function and the extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__naturalist_reading, 0.58).
domain_priors:suppression_score(anthropological_record__naturalist_reading, 0.62).
domain_priors:theater_ratio(anthropological_record__naturalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__naturalist_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__naturalist_reading, "Naturalist Reading of the Anthropological Record (Evolution/Migration via Scientific Method)").
narrative_ontology:topic_domain(anthropological_record__naturalist_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__naturalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__naturalist_reading, '53032d0f-61d0-47f8-a2a3-10257025df16').
narrative_ontology:cs_kernel_codification('53032d0f-61d0-47f8-a2a3-10257025df16', distributed).
narrative_ontology:cs_authority_grounding('53032d0f-61d0-47f8-a2a3-10257025df16', expertise).
narrative_ontology:cs_interpretation_layer_present('53032d0f-61d0-47f8-a2a3-10257025df16').
narrative_ontology:cs_reading_relation('53032d0f-61d0-47f8-a2a3-10257025df16', anthropological_record__creationist_reading, coexists_with).
narrative_ontology:cs_reading_relation('53032d0f-61d0-47f8-a2a3-10257025df16', anthropological_record__indigenous_epistemology_reading, influences).
narrative_ontology:cs_axiom('53032d0f-61d0-47f8-a2a3-10257025df16', foundational, supernatural_causation_categorically_excluded).
narrative_ontology:cs_axiom_status(supernatural_causation_categorically_excluded, holdable).
narrative_ontology:cs_axiom_grounding('53032d0f-61d0-47f8-a2a3-10257025df16', supernatural_causation_categorically_excluded, conventional).
narrative_ontology:cs_axiom('53032d0f-61d0-47f8-a2a3-10257025df16', foundational, credentialed_peer_review_is_sole_legitimate_arbiter).
narrative_ontology:cs_axiom_status(credentialed_peer_review_is_sole_legitimate_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('53032d0f-61d0-47f8-a2a3-10257025df16', credentialed_peer_review_is_sole_legitimate_arbiter, instrumental).
narrative_ontology:cs_reference_frame('53032d0f-61d0-47f8-a2a3-10257025df16', post_darwinian_scientific_consensus).
narrative_ontology:cs_drift_state('53032d0f-61d0-47f8-a2a3-10257025df16', contemporary_credentialing_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('53032d0f-61d0-47f8-a2a3-10257025df16', '').
narrative_ontology:cs_kernel_id(anthropological_record__naturalist_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, credentialed_academic_paleoanthropologists).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, university_anthropology_departments).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, scientific_publishing_institutions).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, non_credentialed_field_interpreters).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, creationist_researchers_seeking_journal_access).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, indigenous_knowledge_keepers_excluded_from_peer_review).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, amateur_fossil_discoverers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, general_public_science_consumers).
narrative_ontology:constraint_vindicates(anthropological_record__naturalist_reading, methodological_naturalism).
narrative_ontology:constraint_vindicates(anthropological_record__naturalist_reading, common_descent_hypothesis).
narrative_ontology:constraint_vindicates(anthropological_record__naturalist_reading, out_of_africa_migration_model).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control peer review, museum curation, excavation permits, and grant allocation. Their interpretive framework — evolution via natural selection, human migration reconstructed from genetics and stratigraphy — is treated as the default lens through which any fossil or genetic find must be described to count as knowledge. They collect career advancement, funding, and institutional prestige from maintaining this as the sole legitimate reading.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, credentialed_academic_paleoanthropologists, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(anthropological_record__naturalist_reading, credentialed_academic_paleoanthropologists, beneficiary).

% Structure curricula, tenure tracks, and accreditation entirely around methodological naturalism. Departments that entertained alternative interpretive frameworks would risk accreditation and grant eligibility, so the arrangement is self-reinforcing at the institutional level.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, university_anthropology_departments, beneficiary,
    institutional, civilizational, arbitrage, global).

% Journals gatekeep what counts as a publishable interpretation of the record; manuscripts framed outside methodological naturalism are rejected at the desk, not on the merits of the underlying data. This concentrates citation currency and impact-factor prestige with journals enforcing the naturalist frame.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, scientific_publishing_institutions, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(anthropological_record__naturalist_reading, scientific_publishing_institutions, agenda_setter).

% Amateur archaeologists, self-taught researchers, and local historians who find and interpret material evidence but lack institutional affiliation. Their interpretations are excluded from professional discourse regardless of evidentiary quality; they can publish informally but cannot enter the credentialed record.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, non_credentialed_field_interpreters, payer,
    moderate, biographical, constrained, regional).

% Researchers who accept the same fossil and genetic data but interpret it within a young-earth or designed-origins framework are excluded from mainstream journals as a matter of definitional policy (methodological naturalism as an entry condition), not because their specific claims have been individually falsified. They must publish in parallel institutions with no cross-citation into the mainstream record.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, creationist_researchers_seeking_journal_access, payer,
    moderate, biographical, trapped, national).

% Oral-tradition accounts of ancestral origin and continuity with place are treated by the naturalist reading as folklore or, at best, as data points to be reinterpreted through migration models — never as a standing epistemic authority on the same record. Their knowledge is extracted for corroborating detail (e.g. matching oral accounts to migration dates) without being credited as an independent way of knowing.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, indigenous_knowledge_keepers_excluded_from_peer_review, payer,
    powerless, civilizational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(anthropological_record__naturalist_reading, indigenous_knowledge_keepers_excluded_from_peer_review, excluded).

% Individuals who physically locate specimens but must hand them to credentialed institutions for description and naming rights; their labor feeds the record but their names and interpretive input are rarely part of the permanent scientific account.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, amateur_fossil_discoverers, payer,
    powerless, immediate, constrained, local).

% Receive a stable, cumulative, falsifiable account of human origins that supports medicine, education, and technology built on evolutionary biology and population genetics. Genuinely benefit from methodological naturalism's track record of predictive success, largely without bearing the credentialing costs directly.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, general_public_science_consumers, beneficiary,
    moderate, generational, mobile, global).

% Study the demarcation problem — what distinguishes science from non-science — and can see both the genuine epistemic coordination function of methodological naturalism and the credentialing structure's gatekeeping effects on non-mainstream interpreters.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, philosophy_of_science_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(anthropological_record__naturalist_reading, credentialed_academic_paleoanthropologists).
narrative_ontology:fixing_cost_class(anthropological_record__naturalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Methodological naturalism solves a genuine epistemic coordination problem: it gives independent researchers a shared, testable, falsifiable standard for evaluating claims about the fossil and genetic record, enabling cumulative, self-correcting knowledge rather than a proliferation of unfalsifiable competing origin stories.
% TRANSFER_FUNCTION: Moves interpretive authority, publication access, funding, and institutional prestige from non-credentialed and alternative-framework interpreters toward credentialed academic institutions and their gatekeeping apparatus (journals, tenure committees, grant panels), in exchange for maintaining a stable, verifiable account of the record.
% ABSENT_VOICES: Creationist researchers and indigenous knowledge keepers would object that methodological naturalism's exclusion of non-materialist causation and non-textual/non-quantitative epistemologies is a definitional move, not a purely evidentiary one — but they are structurally outside the peer-review and credentialing apparatus that would let them raise the objection inside the discourse it governs.
% DISAPPEARANCE_RATIONALE: From the credentialed-science seat, if methodological naturalism vanished as the record's governing standard, cumulative scientific knowledge about human origins would fragment into unfalsifiable competing narratives and centuries of self-correcting progress would be lost. From the excluded-interpreter seats, the arrangement's disappearance would primarily just open journal access and funding to interpretive frameworks currently barred at the gate — the underlying physical evidence (fossils, genomes, strata) would remain exactly as it is.
% FOUNDING_PROBLEM: Prior to methodological naturalism's consolidation, claims about human origins were adjudicated by scriptural authority, philosophical speculation, or untested tradition, with no shared standard for resolving disputes between competing origin narratives using the physical record itself.
% FOUNDING_PROBLEM_CORROBORATION: Historians and philosophers of science outside the paleoanthropology profession (e.g. in science studies and STS) corroborate that methodological naturalism solved a real 17th-19th century problem of non-adjudicable origin claims. The same outside observers, however, increasingly document that the credentialing apparatus built to enforce the standard now also gatekeeps against non-naturalist framings independent of the individual evidentiary merit of specific claims — a shift from adjudicating evidence to adjudicating who may present evidence at all.
narrative_ontology:disappearance_verdict(anthropological_record__naturalist_reading, contested).
narrative_ontology:founding_problem_status(anthropological_record__naturalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__naturalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(anthropological_record__naturalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__naturalist_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anthropological_record__naturalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(anthropological_record__naturalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(anthropological_record__naturalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects a real and growing divergence between evidentiary evaluation (which the naturalist method does well) and definitional exclusion (methodological naturalism as an entry condition for the conversation, not merely an evaluative standard applied evenhandedly to submitted evidence). Suppression (0.62) is higher than extractiveness because the exclusion of non-naturalist framings from credentialed discourse is close to absolute — it does not depend on how good the excluded argument is. Accessibility collapse (0.68) is high: once methodological naturalism is institutionally established as the entry price for the professional conversation, workable alternative institutional paths for those excluded have largely disappeared. Resistance (0.55) is moderate — creationist and indigenous-epistemology communities maintain robust parallel institutions and public constituencies contesting the naturalist reading's exclusivity, even though they cannot contest it from inside the credentialed discourse.
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialed academic institutions sit at the beneficiary end: they set the terms, collect the institutional and career rents, and can move fluidly between sub-fields (arbitrage exit). Non-credentialed interpreters, creationist researchers, and indigenous knowledge keepers sit at the target end: they bear exclusion costs structurally, with exit options ranging from constrained (can publish elsewhere, but disconnected from the credentialed record) to trapped (creationist researchers specifically face a categorical bar, since methodological naturalism is a definitional entry condition, not merely one hypothesis competing on the evidence). The general public benefits from the epistemic product without directly bearing the gatekeeping costs, which is why it is coded as a genuine (if diffuse) beneficiary rather than a payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — adjudicating origin claims via a shared falsifiable standard rather than untested authority — remains partially live (the tangled_rope classification, not snare, is warranted because there is still a real coordination function: shared method prevents the field from fragmenting into unfalsifiable competing narratives). But the credentialing apparatus built to serve that function has hardened into a gate that excludes on framework membership rather than on individual claim quality, which is the mandatrophy signature: the original problem (lack of adjudicable standards) is substantially solved for evidentiary matters that fit within materialist causation, yet the exclusionary machinery persists and has expanded scope to definitional gatekeeping. This is why the type is authored as tangled_rope, not rope: both a genuine coordination function AND an asymmetric extraction machinery are present simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalism_as_method_vs_metaphysics,
    'Is methodological naturalism''s exclusion of supernatural causation a purely procedural convenience for testability, or does it function as a metaphysical commitment that forecloses entire categories of explanation regardless of evidence?',
    'Philosophy-of-science analysis distinguishing methodological naturalism (a working constraint for testability) from philosophical/metaphysical naturalism (a substantive claim about what exists); examine whether credentialed institutions treat the distinction as live or collapsed in practice.',
    'If naturalism functions purely methodologically, the exclusion of rival frameworks is a legitimate coordination cost of falsifiability (pushes toward rope). If it has collapsed into an unstated metaphysical commitment enforced through credentialing, the exclusion is closer to pure gatekeeping (pushes toward snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalism_as_method_vs_metaphysics, conceptual, 'Whether methodological naturalism''s exclusivity is procedural or metaphysical.').

omega_variable(
    credentialing_necessity_vs_capture,
    'Is peer-review credentialing a necessary quality-control mechanism for a field with high interpretive complexity, or has it been captured by institutional actors who benefit from restricting entry beyond what quality control requires?',
    'Compare rejection reasons for non-credentialed but methodologically sound naturalist submissions versus non-naturalist submissions with comparable evidentiary rigor; track whether journals reject on evidentiary grounds or on framework-membership grounds.',
    'If rejections track evidentiary quality regardless of author credentials, extraction is lower than authored. If rejections systematically track framework/credential membership independent of evidentiary quality, extraction is understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credentialing_necessity_vs_capture, empirical, 'Whether credentialing gatekeeping tracks quality control or institutional capture.').

omega_variable(
    kernel_framing_underdetermination,
    'Is ''the anthropological record'' best understood as a single physical evidentiary record with three competing interpretive overlays (this story''s framing), or are the three readings actually disputing what counts as admissible evidence in the first place (a prior, deeper disagreement about evidence itself, not just interpretation of shared evidence)?',
    'Examine whether creationist and indigenous-epistemology readings dispute the naturalist reading''s conclusions using the same evidentiary base, or reject portions of the evidentiary base itself (e.g., radiometric dating assumptions, or the epistemic status of oral tradition as evidence) — this determines whether the kernel is genuinely shared or only nominally shared.',
    'If evidence itself is shared and only interpretation differs, coexists_with is the correct relation to all siblings. If the readings dispute what counts as evidence at all, some sibling pairs may actually be foreclosing rather than coexisting, since they could not jointly operate within one evidentiary standard.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the three kernel readings share an evidentiary base or dispute it at a deeper level.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__naturalist_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t0, anthropological_record__naturalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(anth_tr_t40, anthropological_record__naturalist_reading, theater_ratio, 40, 0.14).
narrative_ontology:measurement(anth_tr_t80, anthropological_record__naturalist_reading, theater_ratio, 80, 0.18).
narrative_ontology:measurement(anth_tr_t120, anthropological_record__naturalist_reading, theater_ratio, 120, 0.21).
narrative_ontology:measurement(anth_tr_t160, anthropological_record__naturalist_reading, theater_ratio, 160, 0.25).
narrative_ontology:measurement(anth_tr_t200, anthropological_record__naturalist_reading, theater_ratio, 200, 0.28).

% Extraction over time
narrative_ontology:measurement(anth_be_t0, anthropological_record__naturalist_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(anth_be_t40, anthropological_record__naturalist_reading, base_extractiveness, 40, 0.34).
narrative_ontology:measurement(anth_be_t80, anthropological_record__naturalist_reading, base_extractiveness, 80, 0.42).
narrative_ontology:measurement(anth_be_t120, anthropological_record__naturalist_reading, base_extractiveness, 120, 0.49).
narrative_ontology:measurement(anth_be_t160, anthropological_record__naturalist_reading, base_extractiveness, 160, 0.54).
narrative_ontology:measurement(anth_be_t200, anthropological_record__naturalist_reading, base_extractiveness, 200, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t0, anthropological_record__naturalist_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(anth_su_t40, anthropological_record__naturalist_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement(anth_su_t80, anthropological_record__naturalist_reading, suppression_requirement, 80, 0.48).
narrative_ontology:measurement(anth_su_t120, anthropological_record__naturalist_reading, suppression_requirement, 120, 0.53).
narrative_ontology:measurement(anth_su_t160, anthropological_record__naturalist_reading, suppression_requirement, 160, 0.58).
narrative_ontology:measurement(anth_su_t200, anthropological_record__naturalist_reading, suppression_requirement, 200, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, anthropological_record__creationist_reading).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, anthropological_record__indigenous_epistemology_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'the anthropological record reveals human origins' per the ε-invariance principle. The naturalist_reading (this story) has a genuine coordination function (falsifiable, cumulative method) plus a growing credentialing-extraction layer (tangled_rope). The creationist_reading and indigenous_epistemology_reading are authored as separate constraints with their own ε and stakeholder structures, since their beneficiary/victim sets and extraction mechanisms differ structurally from this one (e.g., the creationist reading's extraction runs through different institutional gatekeepers — denominational and parachurch — while the indigenous reading's suppression runs through colonial-era epistemic erasure rather than credentialing). All three link via affects_constraints to preserve the kernel-family relationship without merging their distinct ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
