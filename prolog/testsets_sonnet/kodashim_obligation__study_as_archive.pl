% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_archive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Kodashim Study as Historical Archive (Defunct-System Reading)
 *   domain: religious/textual/identity
 *
 * SUMMARY:
 *   This constraint captures one specific reading of the perennial
 *   rabbinic-Jewish debate over why Kodashim (the order of the Mishnah/Talmud
 *   governing Temple sacrifices) continues to be studied intensively despite
 *   the Temple's destruction nearly two millennia ago. Under this reading,
 *   the sacrificial system it documents is genuinely defunct — not suspended,
 *   not spiritually operative in the present, and not realistically pending
 *   restoration. Study functions as historical preservation and
 *   identity-maintenance: it keeps the textual record whole and anchors
 *   communal continuity, but it does not discharge any live legal obligation,
 *   does not enact any cosmic function, and does not prepare practitioners
 *   for an operative resumption. This is structurally distinct from the
 *   study_as_performance reading (which holds that study itself enacts the
 *   sacrificial function regardless of physical Temple absence) and the
 *   study_as_preparation reading (which holds the law remains binding and
 *   study is instrumentally preparatory for messianic restoration). Each
 *   reading is a separate constraint with its own ε and stakeholder structure
 *   per the ε-invariance principle; this file models only study_as_archive.
 *
 * KEY AGENTS:
 *   - yeshiva_institutions: agenda-setting beneficiary, sets curricular allocation
 *   - traditionalist_educators: career-beneficiary of archive-completeness legitimacy
 *   - students_of_applicable_law: payer, opportunity cost of diverted study time
 *   - restorationist_scholars: excluded voice, hold a rival theological premise
 *   - academic_observers: analytical seat, documents the sociology without adjudicating theology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_archive, 0.42).
domain_priors:suppression_score(kodashim_obligation__study_as_archive, 0.28).
domain_priors:theater_ratio(kodashim_obligation__study_as_archive, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, extractiveness, 0.42).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_archive, tangled_rope).
narrative_ontology:human_readable(kodashim_obligation__study_as_archive, "Kodashim Study as Historical Archive (Defunct-System Reading)").
narrative_ontology:topic_domain(kodashim_obligation__study_as_archive, "religious/textual/identity").

domain_priors:requires_active_enforcement(kodashim_obligation__study_as_archive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_archive, '6834d3b6-389c-4259-a2ca-eb33695fa95b').
narrative_ontology:cs_kernel_codification('6834d3b6-389c-4259-a2ca-eb33695fa95b', fixed_text).
narrative_ontology:cs_authority_grounding('6834d3b6-389c-4259-a2ca-eb33695fa95b', lineage).
narrative_ontology:cs_interpretation_layer_present('6834d3b6-389c-4259-a2ca-eb33695fa95b').
narrative_ontology:cs_reading_relation('6834d3b6-389c-4259-a2ca-eb33695fa95b', kodashim_obligation__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('6834d3b6-389c-4259-a2ca-eb33695fa95b', kodashim_obligation__study_as_preparation, coexists_with).
narrative_ontology:cs_axiom('6834d3b6-389c-4259-a2ca-eb33695fa95b', foundational, temple_cult_is_historically_closed).
narrative_ontology:cs_axiom_status(temple_cult_is_historically_closed, holdable).
narrative_ontology:cs_axiom_grounding('6834d3b6-389c-4259-a2ca-eb33695fa95b', temple_cult_is_historically_closed, empirically_contingent).
narrative_ontology:cs_axiom('6834d3b6-389c-4259-a2ca-eb33695fa95b', foundational, study_value_is_identity_and_continuity_not_efficacy).
narrative_ontology:cs_axiom_status(study_value_is_identity_and_continuity_not_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('6834d3b6-389c-4259-a2ca-eb33695fa95b', study_value_is_identity_and_continuity_not_efficacy, conventional).
narrative_ontology:cs_reference_frame('6834d3b6-389c-4259-a2ca-eb33695fa95b', temple_era_operative_cult).
narrative_ontology:cs_drift_state('6834d3b6-389c-4259-a2ca-eb33695fa95b', post_destruction_rabbinic_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('6834d3b6-389c-4259-a2ca-eb33695fa95b', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_archive, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, communal_identity_maintenance).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, yeshiva_institutions).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, traditionalist_educators).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_archive, students_of_applicable_law).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_archive, practical_halakhic_scholarship).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_archive, continuity_of_transmission_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set curricular priority, allocating substantial study hours to Kodashim tractates as part of the full Talmudic cycle (daf yomi and yeshiva curricula). Justify the allocation as preserving unbroken transmission of the complete Oral Law. Collect prestige, donor support, and institutional continuity from being seen as guardians of the full textual tradition, independent of whether the sacrificial system it describes could ever operate again.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, yeshiva_institutions, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(kodashim_obligation__study_as_archive, yeshiva_institutions, beneficiary).

% Build careers, reputational standing, and communal authority on mastery of the full Talmudic corpus including Kodashim. Their expertise is legitimated by the archive's completeness rather than by any applicable output; abandoning the study obligation would devalue decades of accumulated specialized knowledge.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, traditionalist_educators, beneficiary,
    organized, biographical, constrained, national).

% Not an actor but a collective good: the sense of unbroken continuity with a pre-exilic sacrificial order is sustained by ongoing study, functioning as an identity anchor for diaspora communities regardless of restoration prospects.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, communal_identity_maintenance, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(kodashim_obligation__study_as_archive, communal_identity_maintenance).

% Devote years of study time to sacrificial procedures with no possibility of application (no Temple, no altar, no priesthood in functioning form), time that could instead deepen mastery of civil law, family law, or other actionable halakhic domains. Their exit is constrained by curricular requirements set by the institutions they depend on for ordination and standing.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, students_of_applicable_law, payer,
    moderate, biographical, constrained, national).

% The field of applicable law bears an opportunity cost: intellectual resources, canonical attention, and scholarly prestige that could concentrate on contemporary applicable questions are instead partly allocated to a domain with no operative referent.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, practical_halakhic_scholarship, payer,
    moderate, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(kodashim_obligation__study_as_archive, practical_halakhic_scholarship).

% Hold that the study is preparatory for actual restoration and object to the archive framing as defeatist or theologically deficient; their view is present in the tradition but is not the operative premise of this reading and is treated here as a sibling position, not as evidence against this reading's internal coherence.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, restorationist_scholars, excluded,
    organized, generational, trapped, national).

% Study the sociology and history of rabbinic curriculum formation, documenting how and why a defunct ritual system remains a core object of study across communities, without adjudicating the community's own theological claims.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, academic_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_obligation__study_as_archive, diffuse).
narrative_ontology:fixing_cost_class(kodashim_obligation__study_as_archive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains continuity of the complete textual tradition across generations, ensuring no portion of the received corpus is lost, and anchors communal identity in an unbroken chain of transmission independent of practical applicability.
% TRANSFER_FUNCTION: Moves study time, curricular hours, and scholarly prestige toward a textually complete but functionally inert domain, and away from domains of applicable law; in return it produces communal continuity, institutional legitimacy, and credentialing value for those who master the full corpus.
% ABSENT_VOICES: Restorationist scholars who hold that the study is preparatory rather than archival are present in the broader tradition but are not consulted within this reading's own framing, since this reading's premise is that restoration is neither imminent nor the study's purpose.
% DISAPPEARANCE_RATIONALE: If Kodashim study were dropped from standard curricula, applicable-law scholarship would gain hours and attention, but institutions and educators dispute how much communal identity and transmission-continuity would actually erode versus merely relocate to other symbolic anchors; the parties genuinely disagree on the magnitude of rearrangement.
% FOUNDING_PROBLEM: The original obligation to study sacrificial law existed to maintain competence in an operative Temple cult; the historical corpus was compiled while restoration remained a live communal expectation and, in earlier centuries, a near-term political possibility.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of rabbinic Judaism and comparative religion, external to the yeshiva system, attest that the operative Temple cult ended in 70 CE and that no institutional infrastructure for restoration currently exists or is being built; this corroboration comes from outside the beneficiary institutions, which themselves largely decline to characterize the founding problem as dead.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_archive, contested).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_archive, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_archive, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_obligation__study_as_archive, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_archive, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_archive_tests).
:- end_tests(kodashim_obligation__study_as_archive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) because the reading involves a genuine transfer — study time, curricular hours, and career-building attention move away from applicable law and toward a domain the reading itself holds to be functionally inert — while the receiving side (communal identity, institutional prestige) is real but diffuse rather than a concentrated rent. Suppression is comparatively low (0.28) because no one is coerced into believing the archive framing; students who prefer applicable law can and do specialize elsewhere, though curricular structures create friction. Theater ratio is the most notable metric (0.55, rising over the interval) because a growing share of the study's justificatory language performs continuity and reverence toward a system that, under this reading, has no operative referent — the study looks increasingly like maintained ritual performance rather than functional legal education, which is precisely the archive reading's diagnostic claim.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (yeshiva institutions), the arrangement looks like coordination — preserving a shared inheritance that would otherwise erode. From the payer seat (students oriented toward applicable law), the same curricular requirement looks like an enforced diversion of scarce study time toward material with no practical payoff. The engine computes this divergence from the structural data; the archive reading does not resolve it, it only specifies which premise (defunctness) generates the divergence in this particular reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Yeshiva institutions and traditionalist educators sit near the beneficiary end: they collect prestige, donor confidence, and professional standing from mastery of the complete corpus, and they set the curricular rules that sustain the allocation. Students of applicable law and the field of practical scholarship sit nearer the target end: they bear a real opportunity cost in study time and professional specialization that could otherwise go toward domains with operative referents. Communal identity maintenance is authored as a non-agent beneficiary (a collective good, not an actor) and is excluded from directionality computation accordingly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — competence for an operative Temple cult — is dead under this reading, corroborated by historians external to the beneficiary institutions, while the institutional practice persists at full or growing intensity (rising theater_ratio). This is the classic mandatrophy signature: an arrangement whose original function has lapsed continuing to be justified, increasingly, by performance and continuity claims rather than by the original functional warrant. Classifying this as tangled_rope rather than snare or pure piton respects that the arrangement also does real, non-fabricated coordination work (transmission of a genuinely complete and valuable historical-legal corpus) alongside the diversion cost — collapsing it to pure extraction would erase the coordination function that keeps this reading distinct from a bad-faith archive framing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    restoration_possibility_ambiguity,
    'Is Temple restoration genuinely structurally impossible/foreclosed as a live communal project, or merely dormant pending conditions (political, theological, or messianic) that this reading treats as closed but that other readings treat as open?',
    'Track whether any organized restorationist institution-building (e.g., red heifer breeding programs, Temple vessel reconstruction, priestly genealogy verification) achieves operational milestones versus remains purely symbolic; sustained operational progress would undermine the archive reading''s defunctness premise.',
    'If restoration proves to be an active institutional project rather than a closed historical question, this reading''s core premise weakens and the constraint''s structure converges toward the study_as_preparation reading instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_possibility_ambiguity, conceptual, 'Whether the defunctness premise underlying the archive reading is stable or contested by ongoing restorationist activity.').

omega_variable(
    identity_value_measurement,
    'How would one measure the actual magnitude of communal-identity benefit produced by Kodashim study, as opposed to asserting it exists?',
    'Comparative sociological study of communities that de-emphasize Kodashim study (e.g., some Reform or Reconstructionist curricula) versus those that emphasize it, measured against retention, communal cohesion, and continuity indicators.',
    'If de-emphasizing communities show no meaningful erosion in cohesion or continuity, the beneficiary claim (communal_identity_maintenance) weakens substantially and the extraction/benefit ratio shifts unfavorably, pushing the classification closer to snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_value_measurement, empirical, 'Whether the claimed identity-maintenance benefit is empirically substantiated or largely asserted.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the choice to treat ''defunctness'' as the operative premise (rather than ''suspension pending restoration'' or ''ongoing spiritual efficacy'') itself a contestable interpretive act, or is it the historically dominant and textually supported reading?',
    'Textual and historical analysis of how classical and modern authorities (Rambam''s own stated rationale in Sefer HaMitzvot versus contemporary Haredi curricular justifications) frame the purpose of Kodashim study; divergence between historical and contemporary framings would indicate the archive reading is a modern reconstruction rather than a continuous traditional position.',
    'If the archive framing is a recent development rather than a continuous traditional stance, the claimed_type and vindicated_propositions here describe a newer constraint than the kernel''s apparent age suggests, which would need documenting explicitly rather than assumed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the archive framing is the traditionally dominant reading or itself a contested modern interpretive choice among the three sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_archive, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_obligation__study_as_archive, theater_ratio, 0, 0.2).
narrative_ontology:measurement(koda_tr_t20, kodashim_obligation__study_as_archive, theater_ratio, 20, 0.28).
narrative_ontology:measurement(koda_tr_t40, kodashim_obligation__study_as_archive, theater_ratio, 40, 0.36).
narrative_ontology:measurement(koda_tr_t60, kodashim_obligation__study_as_archive, theater_ratio, 60, 0.44).
narrative_ontology:measurement(koda_tr_t80, kodashim_obligation__study_as_archive, theater_ratio, 80, 0.5).
narrative_ontology:measurement(koda_tr_t100, kodashim_obligation__study_as_archive, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_obligation__study_as_archive, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(koda_be_t20, kodashim_obligation__study_as_archive, base_extractiveness, 20, 0.22).
narrative_ontology:measurement(koda_be_t40, kodashim_obligation__study_as_archive, base_extractiveness, 40, 0.3).
narrative_ontology:measurement(koda_be_t60, kodashim_obligation__study_as_archive, base_extractiveness, 60, 0.35).
narrative_ontology:measurement(koda_be_t80, kodashim_obligation__study_as_archive, base_extractiveness, 80, 0.39).
narrative_ontology:measurement(koda_be_t100, kodashim_obligation__study_as_archive, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_obligation__study_as_archive, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(koda_su_t20, kodashim_obligation__study_as_archive, suppression_requirement, 20, 0.14).
narrative_ontology:measurement(koda_su_t40, kodashim_obligation__study_as_archive, suppression_requirement, 40, 0.18).
narrative_ontology:measurement(koda_su_t60, kodashim_obligation__study_as_archive, suppression_requirement, 60, 0.22).
narrative_ontology:measurement(koda_su_t80, kodashim_obligation__study_as_archive, suppression_requirement, 80, 0.25).
narrative_ontology:measurement(koda_su_t100, kodashim_obligation__study_as_archive, suppression_requirement, 100, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_archive, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_obligation__study_as_archive, 0.1).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_archive, kodashim_obligation__study_as_performance).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_archive, kodashim_obligation__study_as_preparation).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the kodashim_obligation kernel, decomposed per the ε-invariance principle because the natural-language question 'why study Kodashim' conflates structurally distinct claims with different ε values: study_as_archive (this file, moderate extraction from opportunity-cost diversion, no restoration premise), study_as_performance (extraction profile driven by claims of ongoing cosmic efficacy independent of physical Temple state), and study_as_preparation (extraction profile driven by binding-but-unperformable legal status and messianic anticipation). All three are linked bidirectionally via affects_constraints; each carries its own beneficiary/victim structure and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
