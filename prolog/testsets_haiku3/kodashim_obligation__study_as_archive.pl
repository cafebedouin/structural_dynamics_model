% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_archive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_archive, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: kodashim_obligation__study_as_archive
 *   human_readable: Kodashim Study as Historical Preservation
 *   domain: religious/textual
 *
 * SUMMARY:
 *   Kodashim (the orders of the Mishnah and Talmud dealing with sacrificial
 *   law) documents a detailed legal system for Temple service that has been
 *   unperformable for nearly 2,000 years. Yet the Jewish halakhic tradition
 *   has maintained an obligation to study these texts continuously. THIS
 *   READING frames that obligation as archival—Kodashim study preserves
 *   historical continuity and communal identity through a textual anchor, not
 *   as a means to actual performance or as cosmic function. The study
 *   extracts intellectual resources from applied law (the jurisprudence that
 *   actually governs contemporary practice) in exchange for maintaining
 *   collective memory. The constraint is neither pure coordination (identity
 *   is real but the tradition also carries inherited obligation structure)
 *   nor pure extraction (the identity function is genuine, not merely cover).
 *   Kodashim study sits in the middle: a Tangled Rope where coordination
 *   (preservation of shared textual continuity) and asymmetric extraction
 *   (diversion of scholarly effort from applicable law) coexist in the same
 *   institutional practice.
 *
 * KEY AGENTS:
 *   - Jewish communal identity: the real beneficiary, maintained through textual continuity even absent performance
 *   - Talmudic scholars / yeshiva system: the agenda-setter, enforcing the obligation through institutional expectation and identity-lock
 *   - Normative legal study / applied rabbinical jurisprudence: the victim set, resources diverted from development of contemporary law
 *   - Conservative reformists: excluded from the framework, argue for de-emphasis of Kodashim in favor of applied law
 *   - Messianic literalists: observer seat holding the 'preparation' reading, accept Kodashim study as obligatory under different reasoning
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_archive, 0.58).
domain_priors:suppression_score(kodashim_obligation__study_as_archive, 0.41).
domain_priors:theater_ratio(kodashim_obligation__study_as_archive, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, extractiveness, 0.58).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, theater_ratio, 0.64).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_archive, tangled_rope).
narrative_ontology:human_readable(kodashim_obligation__study_as_archive, "Kodashim Study as Historical Preservation").
narrative_ontology:topic_domain(kodashim_obligation__study_as_archive, "religious/textual").

domain_priors:requires_active_enforcement(kodashim_obligation__study_as_archive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_archive, '810813ad-67d1-43b2-8c79-2cc9798c17f1').
narrative_ontology:cs_kernel_codification('810813ad-67d1-43b2-8c79-2cc9798c17f1', fixed_text).
narrative_ontology:cs_authority_grounding('810813ad-67d1-43b2-8c79-2cc9798c17f1', lineage).
narrative_ontology:cs_interpretation_layer_present('810813ad-67d1-43b2-8c79-2cc9798c17f1').
narrative_ontology:cs_reading_relation('810813ad-67d1-43b2-8c79-2cc9798c17f1', kodashim_obligation__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('810813ad-67d1-43b2-8c79-2cc9798c17f1', kodashim_obligation__study_as_preparation, coexists_with).
narrative_ontology:cs_axiom('810813ad-67d1-43b2-8c79-2cc9798c17f1', foundational, temple_restoration_impossible).
narrative_ontology:cs_axiom_status(temple_restoration_impossible, holdable).
narrative_ontology:cs_axiom_grounding('810813ad-67d1-43b2-8c79-2cc9798c17f1', temple_restoration_impossible, empirically_contingent).
narrative_ontology:cs_axiom('810813ad-67d1-43b2-8c79-2cc9798c17f1', foundational, study_preserves_identity_without_function).
narrative_ontology:cs_axiom_status(study_preserves_identity_without_function, holdable).
narrative_ontology:cs_axiom_grounding('810813ad-67d1-43b2-8c79-2cc9798c17f1', study_preserves_identity_without_function, deontological).
narrative_ontology:cs_reference_frame('810813ad-67d1-43b2-8c79-2cc9798c17f1', textual_continuity_post_diaspora).
narrative_ontology:cs_drift_state('810813ad-67d1-43b2-8c79-2cc9798c17f1', contemporary_applied_focus, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('810813ad-67d1-43b2-8c79-2cc9798c17f1', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_archive, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, jewish_communal_identity).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_archive, normative_legal_study).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_archive, applied_rabbinical_jurisprudence).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_archive, talmudic_scholars).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_archive, textual_immutability_doctrine).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_archive, collective_memory_preservation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The constraint maintains a textual archive that anchors Jewish continuity across diaspora and historical rupture. Kodashim study preserves a shared reference point—the legal framework of the Temple system—that marks identity even though the system cannot be performed. The 'benefit' is communal cohesion through inherited textual practice, not executable law.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, jewish_communal_identity, beneficiary,
    organized, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(kodashim_obligation__study_as_archive, jewish_communal_identity).

% Maintain and teach the Kodashim corpus as a required curricular center; enforce its study through religious education, yeshiva expectation, and textual commentary production. They invest intellectual and institutional resources in preservation. Their identity as transmitters of tradition depends on treating the corpus as binding study obligation, regardless of performability. Exit would mean abandoning a core institutional commitment.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, talmudic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(kodashim_obligation__study_as_archive, talmudic_scholars, payer).

% The field that develops legal rulings from Talmudic sources to guide contemporary Jewish practice. It competes with Kodashim study for intellectual resources, curricular time, and scholarly attention. Devotion of effort to studying a defunct system diverts capacity from developing answers to live halakhic questions.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, applied_rabbinical_jurisprudence, payer,
    organized, biographical, analytical, global).
narrative_ontology:stakeholder_non_agent(kodashim_obligation__study_as_archive, applied_rabbinical_jurisprudence).

% The body of Talmudic law that directly governs contemporary practice—ritual, dietary, family law, interpersonal ethics. Study time and scholarly attention devoted to the defunct sacrificial system is unavailable for developing and refining applicable jurisprudence.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, normative_legal_study, payer,
    moderate, biographical, analytical, national).
narrative_ontology:stakeholder_non_agent(kodashim_obligation__study_as_archive, normative_legal_study).

% Parties within Jewish tradition who argue that Kodashim study should be de-emphasized or reframed as historical rather than obligatory, or that intellectual resources should prioritize applicable law. They are epistemically excluded from the framework that treats Kodashim as binding study; their objection is not admitted as a live position within the halakhic establishment.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, conservative_reformists, excluded,
    moderate, biographical, constrained, global).

% Hold the reading that Kodashim preserves technical knowledge for future Temple restoration; they see the study constraint differently—not as archive-for-identity but as preparation-for-function. Their position is structurally minoritized within the community; they accept Kodashim study as obligatory under their own reasoning, distinct from the archive reading.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, messianic_literalists, observer,
    powerless, civilizational, identity_locked, regional).

% Academic discipline that studies Kodashim as historical documentation, not as binding law. They analyze the corpus from outside the obligation framework—as source material for reconstructing Temple practice. They occupy a seat where Kodashim is neither obligatory nor extractive.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, secular_jewish_studies, observer,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_obligation__study_as_archive, jewish_communal_identity).
narrative_ontology:fixing_cost_class(kodashim_obligation__study_as_archive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a shared textual archive that anchors Jewish identity and collective memory across 2,000 years of diaspora and historical disruption. The study practice coordinates a common reference point—the legal framework of the Temple system—that marks continuity even though the system itself is not performable and is not claimed to be.
% TRANSFER_FUNCTION: Moves scholarly and curricular resources (attention, time, institutional investment, intellectual effort) from the study of applicable law into the preservation and transmission of a defunct legal system. The transfer is from 'normative jurisprudence' and 'applied halakhic study' to 'preservation of historical continuity.'
% ABSENT_VOICES: Conservative reformists who argue that Kodashim should be de-emphasized or reframed as optional historical study rather than binding obligation are epistemically excluded from the halakhic establishment; their position is not admitted as a live position within the framework that treats study as obligatory. Academic secular Jewish studies analysts are structurally outside the obligation framework itself.
% DISAPPEARANCE_RATIONALE: If the obligation to study Kodashim disappeared, Jewish communal practice would not collapse, but Jewish textual identity would narrow; scholars would redirect effort to applicable law, curricular emphasis would shift, and the archive function would be deliberately separated from the obligation framework. The constraint's disappearance would not alter observable ritual practice (the Temple is not being rebuilt regardless) but would reorganize how the community relates to its textual inheritance.
% FOUNDING_PROBLEM: After the Temple's destruction in 70 CE, Jewish law was faced with a collection of texts that had been the operational framework of the central religious institution and now could not be performed. The constraint emerged to answer: how does a textual tradition preserve itself across rupture? How does a people maintain identity when the system that defined its practice is gone?
% FOUNDING_PROBLEM_CORROBORATION: Historians of Jewish law and textual scholars outside the obligation framework attest that the founding problem—preservation of collective identity through textual continuity after institutional collapse—is a live and ongoing concern for diaspora Jewish communities. The halakhic establishment attests the problem lives through continued Kodashim study requirements. Secular Jewish studies scholars corroborate that the textual archive function is documented and central to how Jewish communities construct continuity across historical rupture.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_archive, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_archive, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_archive, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_obligation__study_as_archive, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_archive, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_archive_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_obligation__study_as_archive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_obligation__study_as_archive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects moderate asymmetric resource flow: scholarly and curricular effort flows FROM applied jurisprudence TO preservation of a defunct system, but the flow is not coercive or deceptive—the obligation is transparent and textually grounded. Suppression (0.41) is moderate: the halakhic establishment enforces the study obligation through institutional expectation and educational mandate, but there is no violence or violent threat; exit is possible but carries institutional and identity cost (identity-lock rather than trapped). Theater ratio (0.64) is the key diagnostic: two-thirds of Kodashim study activity is performative—the study does not produce executable law or alter practice, it enacts and preserves a textual continuity. This is exactly the Piton signature (activity divorced from function, maintained by inertia and theatrical cover), but the constraint is not Piton because the identity-preservation function is real and primary—the theater is a feature, not a symptom of atrophied purpose. The temporal arc shows theater_ratio rising from 0.15 to 0.64 over the interval: in the early post-Temple period, the hope of restoration was alive and study had latent preparation function; by the medieval period, as restoration became structurally impossible and undesired, the identity-preservation frame became explicit and study became increasingly performative. Extractiveness rose and then stabilized as the resource-diversion dynamic settled into institutional equilibrium. The one-time-grid rule: all three metrics are measured at the same six time points (70, 500, 1100, 1700, 1900, 2026) so temporal analysis has a shared reference grid.
 *
 * PERSPECTIVAL GAP:
 *   From the scholarly/institutional seat (talmudic scholars, yeshiva administrators), the constraint is essential coordination—the textual tradition cannot maintain itself without Kodashim study; the obligation is a structural feature of how Jewish identity persists across diaspora. From the applied-law seat (rabbinical jurisprudence scholars working on contemporary halakhic problems), the constraint is extraction—Kodashim diverts intellectual resources from solving live legal questions. From the communal identity seat, the constraint is beneficent (preserves continuity) but from the applied-legal-function seat, it is costly. The engine computes different d values for each seat: the institutional beneficiary (identity) gets d near 0.0 (beneficiary), the applied-law function gets d near 1.0 (target), the scholar sits in the middle (both benefiting from the identity function and paying the resource cost). The authored claim is Tangled Rope (genuine coordination + asymmetric extraction), and the metrics support it—moderate extractiveness and suppression, not negligible and not overwhelming.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish communal identity is the structural beneficiary: Kodashim study preserves continuity, maintains a shared textual reference, and anchors identity claims. This entity collects from the constraint (the coordination benefit), so its directionality is near 0.0 (beneficiary end). Normative legal study and applied jurisprudence are the victims: their intellectual resources are diverted to preserve a defunct system, and they receive no benefit (no executable law, no guidance for contemporary practice comes from Kodashim study when studied-as-archive). Their directionality is near 1.0 (target end). Talmudic scholars and institutional yeshiva administrators are the agenda-setters: they maintain and enforce the obligation, but they are also identity-locked payers (their own identity as transmitters of tradition requires the obligation). Their power (institutional) and exit-options (identity_locked) place them between beneficiary and payer—they are structurally complicit in the extraction but do not collect its gains.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits the classic Tangled Rope signature that mandatrophy resolution tracks: the founding problem (preservation of textual identity after institutional collapse) is LIVE—Jewish communal identity continuously depends on this archive. The coordination function is REAL (the identity preservation is not cover for extraction; it is the genuine structural point). AND there is ASYMMETRIC EXTRACTION (resources diverted from applied law). The constraint does not slip into Piton because the identity function is the actual driver, not a theater hiding atrophied purpose. Mandatrophy resolution: the constraint is not mandatrophic because the founding problem it solves is still actively needed. Its theater ratio is high (0.64) but that is diagnostic of the reading, not of institutional decay—the study is performative because preservation-of-continuity is the function and performance of continuity is how it is accomplished.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    archive_vs_preparation_distinction,
    'Is Kodashim study maintained as historical archive (identity-preserving but functionally defunct) or as preparation for messianic restoration (functionally latent but operationally binding)?',
    'Textual analysis of halakhic commentary and institutional statements over the interval: do scholars frame Kodashim as ''preserving what was'' or as ''preparing what will be''? Survey of contemporary halakhic authorities and yeshiva curricula on the binding status of sacrificial law.',
    'If archive framing predominates, the constraint is Tangled Rope with moderate extractiveness (coordination of identity, extraction of applied-law resources). If preparation framing is dominant, the constraint shifts toward Rope (coordination of latent function, minimal extraction). The reading-identity of THIS constraint depends on this distinction being clarified as ''archive''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(archive_vs_preparation_distinction, empirical, 'Semantic distinction between archive-preservation and messianic-preparation framings of Kodashim study.').

omega_variable(
    voluntary_vs_enforced_study,
    'To what degree is Kodashim study maintained voluntarily by scholars committed to textual preservation versus enforced by halakhic institutional obligation?',
    'Comparative analysis of study patterns in different Jewish communities: sectors where study is institutionally mandated versus those with discretionary emphasis. Historical analysis of periods when the obligation was contested (19th-20th century reformist critiques) and their impact on study rates.',
    'If study is substantially voluntary (commitment to archive), suppression is lower and the constraint approximates Rope. If enforced (institutional mandate), suppression is higher and the constraint is closer to Snare for the ''victims'' (normative legal study) in whose domain resources are diverted. The authored suppression (0.41) reflects moderate institutional enforcement without violent coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_vs_enforced_study, empirical, 'Degree of voluntary commitment versus institutional enforcement in Kodashim study practice.').

omega_variable(
    identity_benefit_reality,
    'Does Kodashim study demonstrably strengthen Jewish communal identity and continuity, or is the ''identity benefit'' a retrospective rationalization layered onto an obligation inherited from precomputed function?',
    'Ethnographic and historical study of how different Jewish communities articulate the identity function of Kodashim study; comparison with communities where Kodashim emphasis is lower and analysis of whether identity/continuity outcomes differ. Textual analysis of how the identity-preservation rationale emerged in halakhic discourse (was it present early, or added later as a response to criticism?)',
    'If the identity benefit is real and primary, the constraint''s beneficiary (Jewish communal identity) is the actual structural driver and extraction-of-applied-resources is secondary. If the identity benefit is post-hoc rationalization, the constraint is closer to Piton (performance of obligation without functional driver, maintained by inertia and theological cover). The reading-declaration of THIS constraint commits to treating identity-preservation as the real coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_benefit_reality, conceptual, 'Whether identity-benefit is the primary driver of Kodashim study or a retroactive justification for inherited obligation.').

omega_variable(
    kernel_reading_contested,
    'Which reading of the kodashim_obligation kernel is structurally correct—archive (this reading), performance (spiritual function of study enacts sacrifice), or preparation (binding law preserved for future performance)?',
    'Textual genealogy of how the halakhic tradition has justified Kodashim study across centuries; comparison with the semantic evolution of the obligation in rabbinic and medieval sources. Analysis of whether the three readings are internally coherent with historical halakhic reasoning or whether some represent modern reinterpretations imposed on the text.',
    'If the archive reading is correct, this constraint is Tangled Rope. If preparation is correct, it shifts toward Rope (coordination of latent law). If performance is correct, it shifts toward pure Rope (spiritual coordination function). The three readings cannot be simultaneously true; the distinction is not merely perspectival—one must be the structurally accurate account. This omega names the irreducible uncertainty about which reading of the kernel is right.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contested, conceptual, 'Kernel reading contest: archive vs. performance vs. preparation framings of Kodashim obligation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_archive, 70, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t70, kodashim_obligation__study_as_archive, theater_ratio, 70, 0.15).
narrative_ontology:measurement_basis(koda_tr_t70, projected).
narrative_ontology:measurement(koda_tr_t500, kodashim_obligation__study_as_archive, theater_ratio, 500, 0.35).
narrative_ontology:measurement_basis(koda_tr_t500, projected).
narrative_ontology:measurement(koda_tr_t1100, kodashim_obligation__study_as_archive, theater_ratio, 1100, 0.48).
narrative_ontology:measurement_basis(koda_tr_t1100, projected).
narrative_ontology:measurement(koda_tr_t1700, kodashim_obligation__study_as_archive, theater_ratio, 1700, 0.61).
narrative_ontology:measurement_basis(koda_tr_t1700, observed).
narrative_ontology:measurement(koda_tr_t1900, kodashim_obligation__study_as_archive, theater_ratio, 1900, 0.66).
narrative_ontology:measurement_basis(koda_tr_t1900, observed).
narrative_ontology:measurement(koda_tr_t2026, kodashim_obligation__study_as_archive, theater_ratio, 2026, 0.64).
narrative_ontology:measurement_basis(koda_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(koda_be_t70, kodashim_obligation__study_as_archive, base_extractiveness, 70, 0.35).
narrative_ontology:measurement_basis(koda_be_t70, projected).
narrative_ontology:measurement(koda_be_t500, kodashim_obligation__study_as_archive, base_extractiveness, 500, 0.48).
narrative_ontology:measurement_basis(koda_be_t500, projected).
narrative_ontology:measurement(koda_be_t1100, kodashim_obligation__study_as_archive, base_extractiveness, 1100, 0.54).
narrative_ontology:measurement_basis(koda_be_t1100, projected).
narrative_ontology:measurement(koda_be_t1700, kodashim_obligation__study_as_archive, base_extractiveness, 1700, 0.58).
narrative_ontology:measurement_basis(koda_be_t1700, observed).
narrative_ontology:measurement(koda_be_t1900, kodashim_obligation__study_as_archive, base_extractiveness, 1900, 0.62).
narrative_ontology:measurement_basis(koda_be_t1900, observed).
narrative_ontology:measurement(koda_be_t2026, kodashim_obligation__study_as_archive, base_extractiveness, 2026, 0.58).
narrative_ontology:measurement_basis(koda_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t70, kodashim_obligation__study_as_archive, suppression_requirement, 70, 0.28).
narrative_ontology:measurement_basis(koda_su_t70, projected).
narrative_ontology:measurement(koda_su_t500, kodashim_obligation__study_as_archive, suppression_requirement, 500, 0.35).
narrative_ontology:measurement_basis(koda_su_t500, projected).
narrative_ontology:measurement(koda_su_t1100, kodashim_obligation__study_as_archive, suppression_requirement, 1100, 0.38).
narrative_ontology:measurement_basis(koda_su_t1100, projected).
narrative_ontology:measurement(koda_su_t1700, kodashim_obligation__study_as_archive, suppression_requirement, 1700, 0.41).
narrative_ontology:measurement_basis(koda_su_t1700, observed).
narrative_ontology:measurement(koda_su_t1900, kodashim_obligation__study_as_archive, suppression_requirement, 1900, 0.42).
narrative_ontology:measurement_basis(koda_su_t1900, observed).
narrative_ontology:measurement(koda_su_t2026, kodashim_obligation__study_as_archive, suppression_requirement, 2026, 0.41).
narrative_ontology:measurement_basis(koda_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_archive, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_obligation__study_as_archive, 0.12).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_archive, kodashim_obligation__study_as_performance).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_archive, kodashim_obligation__study_as_preparation).

% DUAL FORMULATION NOTE:
% The kodashim_obligation kernel is decomposed into three structurally distinct constraints per the ε-invariance principle. Each reading instantiates a different constraint with different ε, different victim/beneficiary structures, and different classifications. The archive reading treats Kodashim as a historical preservation mechanism that extracts resources from applied law; the performance reading treats Kodashim as a spiritual coordination mechanism with negligible extraction; the preparation reading treats Kodashim as latent legal preservation for messianic scenarios with negligible extraction. All three readings are live positions in the tradition; they are not observables of one constraint but distinct constraints unified by the contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kodashim_obligation__study_as_archive, institutional, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
