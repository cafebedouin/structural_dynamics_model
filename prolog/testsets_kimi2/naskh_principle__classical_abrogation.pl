% ============================================================================
% CONSTRAINT STORY: naskh_principle__classical_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__classical_abrogation, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: naskh_principle__classical_abrogation
 *   human_readable: Classical Quranic Abrogation (Naskh)
 *   domain: religious/legal/hermeneutic
 *
 * SUMMARY:
 *   The classical abrogation (naskh) reading of the Quranic text holds that
 *   where later verses address the same legal or theological topic as earlier
 *   verses, the later revelation supersedes and invalidates the earlier
 *   ruling. This principle emerged in the second-third centuries AH as the
 *   dominant hermeneutical mechanism within usul al-fiqh for resolving
 *   apparent textual contradictions, concentrating interpretive authority in
 *   the classical jurists and the legal schools (madhhabs). It is one reading
 *   of the contested naskh_principle kernel; the contextual_harmonization
 *   reading rejects chronological invalidation in favor of situational
 *   specification, and the progressive_restriction reading recasts the
 *   textual sequence as divine pedagogy rather than legal supersession. This
 *   story authors the classical_abrogation reading as a distinct constraint
 *   with its own structural data: it provides genuine coordination (legal
 *   certainty, systematic contradiction-resolution) while asymmetrically
 *   extracting interpretive flexibility from reformist exegetes and imposing
 *   theological coherence costs on theologians. The claim is tangled_rope
 *   because the same structure that coordinates also extracts, and its
 *   persistence depends on active enforcement by classical institutional
 *   authority.
 *
 * KEY AGENTS:
 *   - classical_jurists: Primary agenda_setter (institutional/identity_locked) â administers the abrogation framework through usul al-fiqh
 *   - state_judiciary: Primary beneficiary (institutional/constrained) â applies the fixed rulings the framework produces
 *   - theologians: Primary payer (organized/constrained) â bears the theological cost of defending divine coherence against apparent textual invalidation
 *   - reformist_exegetes: Secondary payer (moderate/constrained) â loses interpretive flexibility as earlier verses are declared legally void
 *   - modern_academic_critics: Excluded seat (organized/mobile) â historical-critical scholars whose alternative methods are outside classical deliberation
 *   - islamic_legal_historians: Analytical observer â tracks the institutional formation and modern erosion of the principle
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__classical_abrogation, 0.62).
domain_priors:suppression_score(naskh_principle__classical_abrogation, 0.58).
domain_priors:theater_ratio(naskh_principle__classical_abrogation, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, extractiveness, 0.62).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__classical_abrogation, tangled_rope).
narrative_ontology:human_readable(naskh_principle__classical_abrogation, "Classical Quranic Abrogation (Naskh)").
narrative_ontology:topic_domain(naskh_principle__classical_abrogation, "religious/legal/hermeneutic").

domain_priors:requires_active_enforcement(naskh_principle__classical_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__classical_abrogation, 'e003c63e-13a7-437a-8ed6-a0fea666783b').
narrative_ontology:cs_kernel_codification('e003c63e-13a7-437a-8ed6-a0fea666783b', fixed_text).
narrative_ontology:cs_authority_grounding('e003c63e-13a7-437a-8ed6-a0fea666783b', lineage).
narrative_ontology:cs_interpretation_layer_present('e003c63e-13a7-437a-8ed6-a0fea666783b').
narrative_ontology:cs_reading_relation('e003c63e-13a7-437a-8ed6-a0fea666783b', naskh_principle__contextual_harmonization, forecloses).
narrative_ontology:cs_reading_relation('e003c63e-13a7-437a-8ed6-a0fea666783b', naskh_principle__progressive_restriction, forecloses).
narrative_ontology:cs_axiom('e003c63e-13a7-437a-8ed6-a0fea666783b', foundational, chronological_order_determines_legal_validity).
narrative_ontology:cs_axiom_status(chronological_order_determines_legal_validity, holdable).
narrative_ontology:cs_axiom_grounding('e003c63e-13a7-437a-8ed6-a0fea666783b', chronological_order_determines_legal_validity, conventional).
narrative_ontology:cs_axiom('e003c63e-13a7-437a-8ed6-a0fea666783b', foundational, divine_legislation_is_temporally_indexed).
narrative_ontology:cs_axiom_status(divine_legislation_is_temporally_indexed, holdable).
narrative_ontology:cs_axiom_grounding('e003c63e-13a7-437a-8ed6-a0fea666783b', divine_legislation_is_temporally_indexed, theological).
narrative_ontology:cs_created_at('e003c63e-13a7-437a-8ed6-a0fea666783b', '').
narrative_ontology:cs_kernel_id(naskh_principle__classical_abrogation, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, state_judiciary).
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, classical_jurists).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, theologians).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, reformist_exegetes).
narrative_ontology:constraint_vindicates(naskh_principle__classical_abrogation, chronological_revelation_supremacy).
narrative_ontology:constraint_vindicates(naskh_principle__classical_abrogation, classical_usul_coherence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the naskh apparatus through usul al-fiqh scholarship, determine which verses abrogate which via chronology reports and asbab al-nuzul literature, and transmit the hermeneutical framework through madrasa and textual commentary. Their authority derives from mastery of this specific interpretive architecture; abandoning it would dissolve their institutional role and the madhhab identity they inhabit.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, classical_jurists, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Apply fixed rulings derived through abrogation analysis in qadi courts and statutory codification. Benefits from legal certainty and a closed canon of applicable texts; does not need to adjudicate between contradictory verses because the abrogation framework resolves them hierarchically.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, state_judiciary, beneficiary,
    institutional, generational, constrained, continental).

% Bear the apologetic burden of explaining why the Divine would reveal rulings only to later invalidate them. Must construct doctrines of temporal sovereignty, progressive pedagogy, or omniscient legislative strategy to preserve theological coherence, while working within a framework that formally removes earlier verses from legal operation.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, theologians, payer,
    organized, civilizational, constrained, global).

% Seek to recover the ethical or legal force of earlier verses on topics such as warfare, gender, or plural marriage, but are constrained by the classical abrogation framework that declares those verses superseded. Their interpretive proposals are ruled out of bounds by the hermeneutical gatekeeping of the classical institutions.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, reformist_exegetes, payer,
    moderate, biographical, constrained, global).

% Historical and literary scholars who argue for contextual or synchronic reading methods and question the reliability of chronology ascriptions. They would dismantle the abrogation category entirely, but are structurally excluded from classical fiqh deliberation and seminary authority.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, modern_academic_critics, excluded,
    organized, biographical, mobile, global).

% Study the historical development of naskh theory as an artifact of second-third century AH legal consolidation. They observe how the principle stabilized the legal system while generating its own theological costs and modern resistances.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, islamic_legal_historians, observer,
    analytical, civilizational, analytical, global).

narrative_ontology:fixing_cost_class(naskh_principle__classical_abrogation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves apparent contradictions in the Quranic text by establishing a chronological hierarchy of rulings, enabling a fixed and predictable legal derivation system across generations of jurists.
% TRANSFER_FUNCTION: Moves interpretive authority from the apparent plain meaning of earlier verses to the jurist-determined later ruling; transfers theological coherence risk to the apologetic tradition and removes interpretive flexibility from reformist readers.
% ABSENT_VOICES: Modern historical-critical scholars and progressive restriction advocates argue that apparent contradictions reflect contextual specification or divine pedagogy rather than supersession; they are excluded from classical institutional authority and seminary pedagogy.
% DISAPPEARANCE_RATIONALE: If the abrogation principle vanished, classical fiqh would lose its primary mechanism for resolving textual contradiction; thousands of derived rulings would require re-derivation, legal certainty would collapse, and the interpretive field would reopen to competing harmonization and restriction methods.
% FOUNDING_PROBLEM: The Quran contains verses with apparently divergent rulings on the same topics such as alcohol, warfare, and inheritance; the early Muslim community needed a systematic method to determine which ruling to apply in law.
% FOUNDING_PROBLEM_CORROBORATION: Classical jurists attest the problem is still live, citing ongoing need for legal certainty. Modern Islamic studies scholars and progressive restriction readings attest the problem was misdiagnosed as contradiction rather than contextual specification; the genealogy is contested by seats outside the beneficiary set.
narrative_ontology:disappearance_verdict(naskh_principle__classical_abrogation, world_rearranges).
narrative_ontology:founding_problem_status(naskh_principle__classical_abrogation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__classical_abrogation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(naskh_principle__classical_abrogation, 'none', 1).
narrative_ontology:epsilon_provenance(naskh_principle__classical_abrogation, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__classical_abrogation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(naskh_principle__classical_abrogation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(naskh_principle__classical_abrogation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.62) reflects the substantial cost of removing verses from active legal force and concentrating the authority to determine chronology in a specialized class. Suppression (0.58) captures the degree to which alternative hermeneutics (harmonization, restriction) are structurally excluded from classical fiqh pedagogy and authority. Theater ratio (0.30) acknowledges that some contemporary application of naskh is performative maintenance of school identity rather than active legal necessity. Accessibility collapse (0.70) is high because once inside the classical framework, the abrogation principle appears as the only coherent method for handling contradiction; resistance (0.45) reflects modernist and reformist challenges that have not yet breached institutional enforcement. The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (classical jurists) experiences the constraint as the indispensable architecture of legal reasoning; the payer seats (theologians, reformists) experience the same structure as a forced choice between hermeneutical rigidity and institutional exclusion. The engine computes this divergence from the structural data: identical verses produce opposite directionality depending on whether the agent administers the chronology or bears its theological and apologetic costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical jurists derive low directionality (beneficiary side) because the constraint subsidizes their institutional authority and creates demand for their expertise. State judiciary derives low-to-mid directionality because it receives a stream of fixed, non-contradictory rulings. Theologians and reformist exegetes derive high directionality because the constraint imposes costs on themâapologetic burden and interpretive closure respectivelyâwithout commensurate benefit. Modern academic critics are analytically outside the directionality computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâresolving apparent contradictions in a revealed legal textâwas genuinely live in the second century AH. The classical abrogation principle solved it, but its institutionalization created a beneficiary class (jurists, state courts) whose authority now depends on maintaining the apparatus even where harmonization or historical-contextual methods would suffice. The Tangled Rope classification prevents mislabeling the principle as pure coordination (Rope) because victims are structurally present, and prevents mislabeling it as pure extraction (Snare) because the coordination function in legal certainty is real and not merely cover. The theater ratio and temporal measurements capture the drift toward performative maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    abrogation_empirical_scope,
    'How many Quranic verses are genuinely abrogated versus claimed as abrogated by classical jurists?',
    'Comprehensive philological and historical analysis of each abrogation claim against early tafsir and asbab al-nuzul reports.',
    'If the count is near zero, the extraction is far higher than the coordination (the apparatus operates on empty claims); if substantial, the coordination function is more grounded.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(abrogation_empirical_scope, empirical, 'Empirical scope of claimed versus demonstrable abrogation instances').

omega_variable(
    theological_coherence_as_cost,
    'Is the theological tension produced by abrogation an internalized cost borne by believers or a structural feature managed by the interpretive class?',
    'Survey of theological literature and believer attitude studies; observe whether theological tension persists in communities that abandon classical abrogation.',
    'If internalized, effective extraction extends beyond jurists to the broader community; if purely structural, extraction is concentrated on the interpretive community.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_coherence_as_cost, conceptual, 'Whether theological coherence costs are internalized or structural').

omega_variable(
    kernel_reading_alternatives,
    'Would adopting contextual harmonization or progressive restriction dissolve the classical legal apparatus or merely redistribute authority within it?',
    'Comparative study of legal systems that operate without abrogation (e.g., some modernist fiqh approaches).',
    'If the apparatus dissolves, classical_abrogation is load-bearing; if authority redistributes, the extraction is the authority concentration rather than the principle itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_alternatives, conceptual, 'Structural load-bearing status of the classical abrogation principle').

omega_variable(
    naskh_naturalness,
    'Is the abrogation principle a natural inference from the Quranic text or a constructed hermeneutical device of the classical period?',
    'Historical-form criticism of second-third century AH legal texts tracing the emergence of naskh as a systematic principle.',
    'If constructed, the constraint''s coordination function is historically contingent and its extraction (authority concentration) is a feature of the construction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naskh_naturalness, empirical, 'Whether naskh is textually natural or classically constructed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__classical_abrogation, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nask_tr_t0, naskh_principle__classical_abrogation, theater_ratio, 0, 0.08).
narrative_ontology:measurement(nask_tr_t15, naskh_principle__classical_abrogation, theater_ratio, 15, 0.12).
narrative_ontology:measurement(nask_tr_t30, naskh_principle__classical_abrogation, theater_ratio, 30, 0.18).
narrative_ontology:measurement(nask_tr_t45, naskh_principle__classical_abrogation, theater_ratio, 45, 0.23).
narrative_ontology:measurement(nask_tr_t60, naskh_principle__classical_abrogation, theater_ratio, 60, 0.27).
narrative_ontology:measurement(nask_tr_t75, naskh_principle__classical_abrogation, theater_ratio, 75, 0.3).

% Extraction over time
narrative_ontology:measurement(nask_be_t0, naskh_principle__classical_abrogation, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(nask_be_t15, naskh_principle__classical_abrogation, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(nask_be_t30, naskh_principle__classical_abrogation, base_extractiveness, 30, 0.56).
narrative_ontology:measurement(nask_be_t45, naskh_principle__classical_abrogation, base_extractiveness, 45, 0.6).
narrative_ontology:measurement(nask_be_t60, naskh_principle__classical_abrogation, base_extractiveness, 60, 0.61).
narrative_ontology:measurement(nask_be_t75, naskh_principle__classical_abrogation, base_extractiveness, 75, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(nask_su_t0, naskh_principle__classical_abrogation, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(nask_su_t15, naskh_principle__classical_abrogation, suppression_requirement, 15, 0.42).
narrative_ontology:measurement(nask_su_t30, naskh_principle__classical_abrogation, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(nask_su_t45, naskh_principle__classical_abrogation, suppression_requirement, 45, 0.57).
narrative_ontology:measurement(nask_su_t60, naskh_principle__classical_abrogation, suppression_requirement, 60, 0.58).
narrative_ontology:measurement(nask_su_t75, naskh_principle__classical_abrogation, suppression_requirement, 75, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, contextual_harmonization).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, progressive_restriction).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the naskh_principle kernel. The classical_abrogation reading treats chronological supersession as legal invalidation; the contextual_harmonization and progressive_restriction readings reject this structural move and instantiate different constraints from the same textual kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
