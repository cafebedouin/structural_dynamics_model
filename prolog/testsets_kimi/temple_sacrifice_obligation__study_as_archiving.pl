% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__study_as_archiving
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__study_as_archiving, []).

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
 *   constraint_id: temple_sacrifice_obligation__study_as_archiving
 *   human_readable: Study as Archiving of Unperformable Sacrificial Law
 *   domain: religious/historical/commitment_system
 *
 * SUMMARY:
 *   This constraint instantiates the 'study_as_archiving' reading of the
 *   temple_sacrifice_obligation kernel. Following the destruction of the
 *   Second Temple in 70 CE, rabbinic authority reconceived the study of
 *   sacrificial law as an archival practice that preserves technical
 *   knowledge for future messianic restoration while explicitly denying that
 *   study discharges the original obligation. The constraint coordinates
 *   genuine generational knowledge transfer but asymmetrically extracts
 *   authority and institutional resources from a community held in permanent
 *   non-compliance with a divine commandment it cannot perform. The
 *   claim/metric independence is maintained: the constraint is claimed as
 *   tangled_rope (genuine coordination plus asymmetric extraction) while
 *   metrics reflect moderate but persistent extractiveness and rising
 *   theatricality over two millennia of deferred restoration.
 *
 * KEY AGENTS:
 *   - rabbinic_authority: Agenda-setter (institutional/constrained) â maintains binding status of unperformable law through interpretive authority and halakhic adjudication.
 *   - torah_study_institutions: Beneficiary (organized/constrained) â receive institutional purpose, curricular centrality, and communal resources from the perpetual study of sacrificial law.
 *   - diaspora_jewish_communities: Primary target (moderate/identity_locked) â bear the burden of unfulfilled divine command and intensive study without ritual relief; exit is fused with communal identity.
 *   - kohanim_descendants: Excluded voice (moderate/constrained) â hereditary priests sidelined by a study-centric interim that indefinitely defers restoration of their ritual function.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_archiving, 0.62).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_archiving, 0.6).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_archiving, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, extractiveness, 0.62).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_archiving, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_archiving, "Study as Archiving of Unperformable Sacrificial Law").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_archiving, "religious/historical/commitment_system").

domain_priors:requires_active_enforcement(temple_sacrifice_obligation__study_as_archiving).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_archiving, '5158e753-351b-4f8f-9ab4-1986890b0f81').
narrative_ontology:cs_kernel_codification('5158e753-351b-4f8f-9ab4-1986890b0f81', fixed_text).
narrative_ontology:cs_authority_grounding('5158e753-351b-4f8f-9ab4-1986890b0f81', lineage).
narrative_ontology:cs_interpretation_layer_present('5158e753-351b-4f8f-9ab4-1986890b0f81').
narrative_ontology:cs_reading_relation('5158e753-351b-4f8f-9ab4-1986890b0f81', temple_sacrifice_obligation__study_as_occupation, forecloses).
narrative_ontology:cs_reading_relation('5158e753-351b-4f8f-9ab4-1986890b0f81', temple_sacrifice_obligation__messianic_suspension, coexists_with).
narrative_ontology:cs_axiom('5158e753-351b-4f8f-9ab4-1986890b0f81', foundational, study_preserves_without_fulfilling).
narrative_ontology:cs_axiom_status(study_preserves_without_fulfilling, holdable).
narrative_ontology:cs_axiom_grounding('5158e753-351b-4f8f-9ab4-1986890b0f81', study_preserves_without_fulfilling, conventional).
narrative_ontology:cs_axiom('5158e753-351b-4f8f-9ab4-1986890b0f81', foundational, unperformable_law_remains_binding).
narrative_ontology:cs_axiom_status(unperformable_law_remains_binding, holdable).
narrative_ontology:cs_axiom_grounding('5158e753-351b-4f8f-9ab4-1986890b0f81', unperformable_law_remains_binding, deontological).
narrative_ontology:cs_reference_frame('5158e753-351b-4f8f-9ab4-1986890b0f81', temple_era_binding_obligation).
narrative_ontology:cs_drift_state('5158e753-351b-4f8f-9ab4-1986890b0f81', post_temple_diaspora, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('5158e753-351b-4f8f-9ab4-1986890b0f81', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, rabbinic_authority).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, torah_study_institutions).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, diaspora_jewish_communities).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_archiving, eternal_binding_of_torah).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_archiving, rabbinic_interpretive_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the halakhic ruling that sacrificial law remains binding despite the Temple's destruction; administers the interpretive shift from performance to study while explicitly denying that study discharges the obligation. Derives institutional legitimacy from being the custodian of unperformable but binding law.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, rabbinic_authority, agenda_setter,
    institutional, generational, constrained, global).

% Gain curricular centrality, student enrollment, and communal funding from the authoritative designation of sacrificial law as currently binding subject matter. Their institutional purpose depends on the perpetual study of texts that cannot be enacted.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, torah_study_institutions, beneficiary,
    organized, generational, constrained, global).

% Live under the halakhic determination that Temple sacrifices remain obligatory; bear the spiritual burden of permanent non-compliance with a divine commandment and the intensive cognitive demands of mastering laws they cannot practice. Exit from the constraint is fused with exit from Jewish communal identity.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, diaspora_jewish_communities, payer,
    moderate, biographical, identity_locked, global).

% Hereditary priests whose ritual function is suspended by the destruction of the Temple; they are structurally marginalized by a study-centric interim that defers restoration indefinitely, and their advocacy for active priestly preparation is treated as premature or irrelevant.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, kohanim_descendants, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_obligation__study_as_archiving, rabbinic_authority).
narrative_ontology:fixing_cost_class(temple_sacrifice_obligation__study_as_archiving, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the technical knowledge of Temple sacrifice across generations so that the ritual can be restored immediately upon messianic rebuilding, preventing generational amnesia of complex priestly procedures.
% TRANSFER_FUNCTION: Moves the locus of religious obligation from ritual performance to textual study; transfers communal prestige and institutional resources to study academies and rabbinic courts while leaving the community in a state of permanent non-compliance with the original commandment.
% ABSENT_VOICES: Kohanim seeking immediate restoration of sacrificial practice, messianic activists who would expedite Temple rebuilding, and modernist voices arguing for the obsolescence of Temple law are structurally marginalized; their positions are treated as heterodox or dangerous rather than halakhically live options.
% DISAPPEARANCE_RATIONALE: If the binding status of unperformable sacrificial law were lifted, the community would either shift to alternative readings (occupation, suspension) or face a theological crisis; the rabbinic authority's custodial role and the study academies' curricular centrality would collapse; the religious economy would rearrange around performable commandments.
% FOUNDING_PROBLEM: The destruction of the Second Temple eliminated the physical site for divine worship and sacrifice, creating an existential crisis for a religion centered on ritual performance; the community needed a way to maintain continuity with its covenantal obligations and textual knowledge while exiled from its cultic center.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians and archaeologists attest the destruction event and the Roman exile; internal rabbinic literature (Mishna, Talmud) documents the crisis and the turn to study. However, the specific claim that study preserves rather than replaces sacrifice is attested primarily by the benefiting parties (rabbinic authorities); dissenting voices (Karaite, Sadducean, or modern critical) are excluded from the halakhic consensus and would contest the framing.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__study_as_archiving, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__study_as_archiving, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__study_as_archiving, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__study_as_archiving, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__study_as_archiving, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__study_as_archiving_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_archiving, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temple_sacrifice_obligation__study_as_archiving_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate: the study function is genuine (preserving complex knowledge) but the maintenance of binding status for unperformable law creates sustained asymmetric extraction of authority and institutional resources. Suppression (0.60) reflects the active exclusion of alternative readings (occupation, suspension, obsolescence) through halakhic interpretive authority. Theater_ratio (0.52) has risen over two millennia as the practical likelihood of restoration has diminished while the performative apparatus of study has expanded. Accessibility_collapse (0.75) is high because once inside the rabbinic framework, the alternative of declaring the obligation suspended or fulfilled is nearly inaccessible. Resistance (0.30) is low but persistent from marginal messianic and modernist movements.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (rabbinic authority, study institutions) experience the constraint as necessary custodianship of a deferred covenant; the payer seat (diaspora communities) experiences it as a burden of permanent non-compliance. The engine computes this divergence from the structural data: low directionality for beneficiaries who collect authority and resources, high directionality for identity-locked payers who cannot exit without exiting the covenant itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority and study institutions are structural beneficiaries: they collect institutional legitimacy and resources from the maintenance of binding unperformable law (low d, subsidized by the constraint). Diaspora communities are structural targets: they bear the spiritual and cognitive costs of an obligation that cannot be discharged (high d, amplified by identity-locked exit). Kohanim sit nearer the target end than neutral because the constraint specifically excludes their hereditary function, though their exclusion is a secondary effect rather than the primary extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resists mandatrophy mislabeling because it possesses a genuine coordination function (knowledge preservation) that would be necessary even in a world without authority extraction; it is not a pure snare because the archival function is real and would be needed for restoration. It is not a mountain because its bindingness is maintained by active interpretive authority, not by irreducible physical law. The tangled_rope classification captures the hybrid: the same structure that preserves knowledge also maintains a power asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    archiving_vs_occupation_validity,
    'Does the study of sacrifice law genuinely preserve operational knowledge for restoration, or does it function as a theological evasion that sustains rabbinic authority over a permanently deferred practice?',
    'Comparative analysis of historical restoration scenarios and assessment of whether the accumulated textual corpus could enable valid sacrificial performance without practical apprenticeship.',
    'If the study corpus is operationally non-viable for restoration, the coordination claim is cover for authority maintenance, pushing classification toward snare. If viable, the coordination function is genuine, supporting tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(archiving_vs_occupation_validity, conceptual, 'Whether the archival knowledge is functionally preservative or performatively decorative.').

omega_variable(
    binding_status_extraction,
    'Who captures the surplus generated by maintaining the binding status of unperformable law â the rabbinic authority as institutional power, the study academies as economic actors, or the Jewish people as covenantal continuity?',
    'Institutional analysis of resource flows to yeshivot and rabbinic courts; sociological study of authority claims in communities with and without Temple-sacrifice curricula.',
    'If authority and resources concentrate in the study institutions, extraction is asymmetric and directed. If the community genuinely internalizes the bindingness as its own continuity, directionality shifts toward symmetric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binding_status_extraction, empirical, 'Whether the constraint''s extraction accrues to institutional seats or diffuses as collective identity.').

omega_variable(
    kernel_reading_relationship,
    'Is the study_as_archiving reading logically foreclosed by either sibling reading within a unified halakhic framework, or can it coexist as a legitimate minority position?',
    'Talmudic jurisprudential analysis: are these positions recorded as compatible opinions (shitat ha-Rambam vs. shitat ha-Rashba) or as mutually exclusive rulings?',
    'If foreclosed by a dominant sibling reading, this constraint''s authority grounding is weak. If coexisting, the extraction is moderated by interpretive pluralism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_relationship, conceptual, 'Structural relationship between sibling readings of the same kernel.').

omega_variable(
    natural_law_vs_constructed_obligation,
    'Is the persistence of sacrificial obligation after Temple destruction a natural feature of an eternal covenant, or a constructed constraint maintained by institutional interpretive choice?',
    'Historical-comparative analysis: did other ancient Near Eastern religions with destroyed temples similarly preserve unperformable ritual obligations, or did they declare suspension or obsolescence?',
    'If unique to rabbinic interpretive construction, the constraint is revealed as institutional artifact rather than natural law, with implications for the legitimacy of its bindingness claims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_obligation, conceptual, 'Whether the obligation''s persistence is natural or constructed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_archiving, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 0, 0.1).
narrative_ontology:measurement(temp_tr_t100, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 100, 0.15).
narrative_ontology:measurement(temp_tr_t500, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 500, 0.25).
narrative_ontology:measurement(temp_tr_t1000, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1000, 0.35).
narrative_ontology:measurement(temp_tr_t1500, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1500, 0.42).
narrative_ontology:measurement(temp_tr_t1800, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1800, 0.48).
narrative_ontology:measurement(temp_tr_t2000, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 2000, 0.52).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(temp_be_t100, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 100, 0.38).
narrative_ontology:measurement(temp_be_t500, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 500, 0.48).
narrative_ontology:measurement(temp_be_t1000, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1000, 0.55).
narrative_ontology:measurement(temp_be_t1500, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1500, 0.58).
narrative_ontology:measurement(temp_be_t1800, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1800, 0.6).
narrative_ontology:measurement(temp_be_t2000, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 2000, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(temp_su_t100, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 100, 0.4).
narrative_ontology:measurement(temp_su_t500, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 500, 0.55).
narrative_ontology:measurement(temp_su_t1000, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1000, 0.6).
narrative_ontology:measurement(temp_su_t1500, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1500, 0.65).
narrative_ontology:measurement(temp_su_t1800, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1800, 0.62).
narrative_ontology:measurement(temp_su_t2000, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 2000, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__study_as_archiving, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_archiving, study_as_occupation).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_archiving, messianic_suspension).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the temple_sacrifice_obligation kernel, decomposed from the colloquial label 'Temple sacrifice obligation' which conflates three structurally distinct claims: whether study fulfills the obligation (occupation), whether study archives it for restoration (archiving), and whether the obligation is messianically suspended (suspension). Each reading has a distinct epsilon, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
