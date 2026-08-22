% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_archive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-27
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
 *   constraint_id: kodashim_obligation__study_as_archive
 *   human_readable: Kodashim Study as Historical Archive and Identity Maintenance
 *   domain: religious_studies/jewish_law/textual_preservation
 *
 * SUMMARY:
 *   Kodashim (the Talmudic order on sacrificial law) documents a system that
 *   has been inoperative for nearly two millennia. The study_as_archive
 *   reading holds that engaging with these texts serves historical
 *   preservation and communal identity maintenance — not legal obligation (no
 *   Temple exists to perform the laws) and not cosmic function (study does
 *   not enact sacrifice). This reading emerged prominently in the Haskalah
 *   and early Wissenschaft des Judentums, and persists in academic talmud
 *   departments and liberal yeshivot. The constraint is the institutional
 *   requirement to allocate significant curriculum time to Kodashim despite
 *   its zero applicability to contemporary halakhic practice. Beneficiaries
 *   are communal identity maintainers and traditionalist institutions that
 *   derive legitimacy from preserving the full canonical corpus. Victims are
 *   practical halakhic scholars and students whose intellectual resources are
 *   diverted from living-law domains. The constraint requires active
 *   enforcement (curriculum mandates, ordination requirements) and extracts
 *   moderate legitimacy from historical continuity without functional output.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_archive, 0.38).
domain_priors:suppression_score(kodashim_obligation__study_as_archive, 0.22).
domain_priors:theater_ratio(kodashim_obligation__study_as_archive, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, extractiveness, 0.38).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_archive, tangled_rope).
narrative_ontology:human_readable(kodashim_obligation__study_as_archive, "Kodashim Study as Historical Archive and Identity Maintenance").
narrative_ontology:topic_domain(kodashim_obligation__study_as_archive, "religious_studies/jewish_law/textual_preservation").

domain_priors:requires_active_enforcement(kodashim_obligation__study_as_archive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_archive, 'ec73da62-a4fe-4061-8484-12d442793ee1').
narrative_ontology:cs_kernel_codification('ec73da62-a4fe-4061-8484-12d442793ee1', fixed_text).
narrative_ontology:cs_authority_grounding('ec73da62-a4fe-4061-8484-12d442793ee1', lineage).
narrative_ontology:cs_interpretation_layer_present('ec73da62-a4fe-4061-8484-12d442793ee1').
narrative_ontology:cs_reading_relation('ec73da62-a4fe-4061-8484-12d442793ee1', kodashim_obligation__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('ec73da62-a4fe-4061-8484-12d442793ee1', kodashim_obligation__study_as_preparation, coexists_with).
narrative_ontology:cs_axiom('ec73da62-a4fe-4061-8484-12d442793ee1', foundational, kodashim_as_historical_archive_only).
narrative_ontology:cs_axiom_status(kodashim_as_historical_archive_only, holdable).
narrative_ontology:cs_axiom_grounding('ec73da62-a4fe-4061-8484-12d442793ee1', kodashim_as_historical_archive_only, empirically_contingent).
narrative_ontology:cs_axiom('ec73da62-a4fe-4061-8484-12d442793ee1', foundational, temple_restoration_structurally_impossible_or_undesired).
narrative_ontology:cs_axiom_status(temple_restoration_structurally_impossible_or_undesired, holdable).
narrative_ontology:cs_axiom_grounding('ec73da62-a4fe-4061-8484-12d442793ee1', temple_restoration_structurally_impossible_or_undesired, empirically_contingent).
narrative_ontology:cs_reference_frame('ec73da62-a4fe-4061-8484-12d442793ee1', canonical_corpus_closure).
narrative_ontology:cs_drift_state('ec73da62-a4fe-4061-8484-12d442793ee1', post_enlightenment_haskalah, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ec73da62-a4fe-4061-8484-12d442793ee1', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_archive, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, communal_identity_maintainers).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, traditionalist_institutions).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_archive, practical_halakhic_scholars).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_archive, students_diverted_from_applicable_law).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_archive, continuity_of_torah_study_as_end_in_itself).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_archive, historical_preservation_as_sacred_obligation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communal leaders, educators, and institutions that derive legitimacy and boundary-maintenance from preserving the full canonical corpus. They control curriculum standards, ordination requirements, and communal prestige markers. They can redefine what counts as 'core curriculum' (arbitrage-grade exit) but benefit from the current arrangement's identity-signaling function.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, communal_identity_maintainers, beneficiary,
    institutional, generational, arbitrage, global).

% Major yeshivot, rabbinic courts, and denominational bodies that set and enforce Kodashim study requirements. They administer the constraint (curriculum mandates, examination gates) and benefit from the legitimacy it confers. Their exit is constrained: abandoning the requirement signals departure from tradition, risking communal authority.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, traditionalist_institutions, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_obligation__study_as_archive, traditionalist_institutions, beneficiary).

% Scholars and decisors (poskim) whose primary work is living halakha (Shabbat, kashrut, family law, business ethics, medical ethics). They bear the opportunity cost: time and cognitive bandwidth spent mastering Kodashim is unavailable for applicable law. Their exit is constrained — ordination and communal recognition require demonstrating Kodashim mastery.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, practical_halakhic_scholars, payer,
    organized, biographical, constrained, global).

% Advanced students in yeshiva/kollel tracks who must allocate 15-20% of study years to Kodashim. They bear the direct diversion cost: less time for living-law fluency. Exit is constrained — leaving the institutional track means losing communal recognition, stipend support, and marriage-market positioning in traditional communities.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, students_diverted_from_applicable_law, payer,
    moderate, biographical, constrained, global).

% University departments studying Kodashim as historical/philological material without canonical obligation. They see the full structure: the texts' genuine historical value, the institutional enforcement in traditional settings, and the resource diversion. They neither collect nor pay — their seat is analytical.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, academic_talmud_departments, observer,
    organized, generational, analytical, global).

% Institutions that have made Kodashim elective rather than mandatory. They would object to the claim that mandatory study is necessary for Jewish continuity, but they are structurally excluded from the traditionalist standard-setting conversation. Their mobility demonstrates the constraint is not a natural law — alternatives exist and function.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, liberal_yeshivot_elective_track, excluded,
    moderate, biographical, mobile, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_obligation__study_as_archive, communal_identity_maintainers).
narrative_ontology:fixing_cost_class(kodashim_obligation__study_as_archive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the canonical integrity of the Talmudic corpus as a closed, complete textual tradition — ensuring no order is 'lost' or treated as optional, which maintains the canon's authority as a unified whole.
% TRANSFER_FUNCTION: Moves scholarly attention, curriculum time, and cognitive bandwidth from living halakhic domains (Shabbat, kashrut, family law, ethics) to the study of a defunct sacrificial system. The transfer is mandatory in traditionalist institutions; the gains (legitimacy, boundary-maintenance, communal identity signaling) accrue to identity maintainers and traditionalist institutions.
% ABSENT_VOICES: Practicing halakhic decisors overwhelmed by contemporary questions (agunah, conversion, medical ethics, technology) who would argue that scholarly resources should prioritize living law. They are excluded because the traditionalist standard-setting bodies are composed of those who benefit from the current curriculum structure.
% DISAPPEARANCE_RATIONALE: If mandatory Kodashim study vanished overnight, traditionalist yeshivot would reallocate ~15-20% of advanced curriculum to living law within 1-2 years. Communal identity signaling would shift to other markers. The canon's textual preservation would continue in academic and elective settings. The institutional structure that enforces the mandate would lose a key legitimacy pillar.
% FOUNDING_PROBLEM: Preserving the technical knowledge of sacrificial law for a future Temple restoration, and maintaining the Talmudic canon as a complete, closed textual unity.
% FOUNDING_PROBLEM_CORROBORATION: The Temple has not stood for ~1950 years; no serious messianic movement has achieved restoration; the 'preparation' function is acknowledged as dead even by traditionalist scholars who now justify study as 'torah lishma' or identity maintenance. Corroboration from outside beneficiaries: academic historians (Safrai, Fraade, Klawans), liberal rabbinic bodies (CCAR, RA), and early maskilim (Krochmal, Geiger) all document the founding problem's obsolescence. The traditionalist institutions themselves have shifted justification from 'preparation' to 'identity' — confirming the founding problem is dead while the arrangement persists.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_archive, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_archive, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_archive, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(kodashim_obligation__study_as_archive, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_archive, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.38) is moderate: the constraint diverts ~15-20% of advanced curriculum time from applicable law to a defunct system, but the texts are genuine canonical heritage requiring preservation labor. Suppression (0.22) is low-moderate: alternatives exist (elective study, academic departments), but institutional pressure (ordination gates, communal prestige) makes exit costly. Theater ratio (0.45) is significant: much study is performative — demonstrating commitment to the full corpus rather than engaging substantively with sacrificial mechanics. Accessibility collapse (0.35) is partial: the texts are available and studied voluntarily by some; the constraint does not fully collapse the alternative of not studying them. Resistance (0.28) is modest: critique exists but rarely translates to curriculum reform.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat, the constraint is genuine coordination — preserving the Torah's integrity as a closed canon. From the victim seat, it is extraction — a mandatory tax on scholarly attention that serves institutional self-preservation more than communal need. The engine computes this divergence from the structural data; the claimed_type (tangled_rope) reflects the authoring seat's assessment that both functions are real and inseparable in current operation.
 *
 * DIRECTIONALITY LOGIC:
 *   Communal identity maintainers and traditionalist institutions are structural beneficiaries (d ~ 0.15-0.25): they collect legitimacy, curriculum control, and boundary-maintenance from the constraint. Practical halakhic scholars and diverted students are structural targets (d ~ 0.7-0.85): they bear the opportunity cost of mastering inapplicable material. The analytical observer sees both the coordination function (canon preservation) and the extraction (resource diversion). Directionality is derived from beneficiary/victim declarations + exit options: beneficiaries have arbitrage-grade exit (can redefine curriculum); victims are constrained (ordination requires compliance).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserving sacrificial law knowledge for a Temple that might be rebuilt) is dead on this reading — Temple restoration is structurally impossible/undesired. The arrangement persists because it was repurposed for identity maintenance. The constraint is not a piton (theatrical maintenance of a dead function) because the identity-maintenance function is live and valued by beneficiaries; it is a tangled_rope where the coordination function (canon preservation) and extraction (diversion from living law) are fused.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the contested kernel ''kodashim_obligation'' (study_as_archive), distinct from study_as_performance and study_as_preparation?',
    'Structural comparison: if the constraint''s beneficiary is communal identity via historical preservation (not cosmic function or messianic readiness), and its victim set is intellectual resources diverted from applicable law, the reading is structurally isolated. The kernel context documents the decomposition.',
    'Confirms ε-invariance: this constraint has a single stable ε (0.38) assessed from the archive-reading''s lights. Sibling readings instantiate different constraints with different ε values.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment that this constraint is the study_as_archive reading of the kodashim_obligation kernel').

omega_variable(
    naturalness_vs_construction,
    'Is the obligation to study Kodashim a natural feature of Jewish legal tradition (emerging from the text''s canonical status) or a constructed constraint maintained by institutions that benefit from identity-preservation labor?',
    'Trace institutional enforcement: if yeshiva curricula, rabbinic ordination requirements, and communal prestige structures would collapse without mandatory Kodashim study, the constraint is maintained by active enforcement serving identifiable beneficiaries. If study persists voluntarily at comparable intensity without institutional pressure, naturalness is supported.',
    'If constructed, the claimed ''tangled_rope'' (coordination + extraction) is validated; if natural, the constraint may be a false summit mountain (coordination without extraction) or genuine rope. FSM evaluation triggered by beneficiaries on mountain claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_vs_construction, empirical, 'Natural-law vs. constructed ambiguity for a constraint declaring beneficiaries').

omega_variable(
    extraction_mechanism_ambiguity,
    'Does the measured extractiveness (0.38) represent legitimate coordination cost (preserving a 2000-year textual corpus) or asymmetric extraction (diverting scholars from living halakhic domains that serve current communal needs)?',
    'Comparative curriculum analysis: measure scholarly output in applicable law (Shabbat, kashrut, family law, business ethics) in institutions with mandatory vs. elective Kodashim requirements. If mandatory institutions show measurable deficits in living-law responsa production, extraction is asymmetric.',
    'If asymmetric extraction is confirmed, the tangled_rope classification holds. If extraction is purely coordination overhead, the constraint may be rope. The engine computes per-seat χ from the structural data; this omega documents the interpretive ambiguity in ε itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_ambiguity, empirical, 'Whether ε represents coordination cost or extractive diversion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_archive, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_obligation__study_as_archive, theater_ratio, 0, 0.2).
narrative_ontology:measurement(koda_tr_t50, kodashim_obligation__study_as_archive, theater_ratio, 50, 0.25).
narrative_ontology:measurement(koda_tr_t100, kodashim_obligation__study_as_archive, theater_ratio, 100, 0.3).
narrative_ontology:measurement(koda_tr_t150, kodashim_obligation__study_as_archive, theater_ratio, 150, 0.35).
narrative_ontology:measurement(koda_tr_t200, kodashim_obligation__study_as_archive, theater_ratio, 200, 0.4).
narrative_ontology:measurement(koda_tr_t250, kodashim_obligation__study_as_archive, theater_ratio, 250, 0.45).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_obligation__study_as_archive, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(koda_be_t50, kodashim_obligation__study_as_archive, base_extractiveness, 50, 0.28).
narrative_ontology:measurement(koda_be_t100, kodashim_obligation__study_as_archive, base_extractiveness, 100, 0.31).
narrative_ontology:measurement(koda_be_t150, kodashim_obligation__study_as_archive, base_extractiveness, 150, 0.34).
narrative_ontology:measurement(koda_be_t200, kodashim_obligation__study_as_archive, base_extractiveness, 200, 0.36).
narrative_ontology:measurement(koda_be_t250, kodashim_obligation__study_as_archive, base_extractiveness, 250, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_obligation__study_as_archive, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(koda_su_t50, kodashim_obligation__study_as_archive, suppression_requirement, 50, 0.16).
narrative_ontology:measurement(koda_su_t100, kodashim_obligation__study_as_archive, suppression_requirement, 100, 0.18).
narrative_ontology:measurement(koda_su_t150, kodashim_obligation__study_as_archive, suppression_requirement, 150, 0.2).
narrative_ontology:measurement(koda_su_t200, kodashim_obligation__study_as_archive, suppression_requirement, 200, 0.21).
narrative_ontology:measurement(koda_su_t250, kodashim_obligation__study_as_archive, suppression_requirement, 250, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_archive, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_obligation__study_as_archive, 0.1).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_archive, kodashim_obligation__study_as_performance).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_archive, kodashim_obligation__study_as_preparation).

% DUAL FORMULATION NOTE:
% The kodashim_obligation kernel decomposes into three constraint stories linked by affects_constraints. study_as_archive (this story) has moderate extraction (ε=0.38) from identity-maintenance diversion. study_as_performance claims near-zero extraction (cosmic function, no diversion). study_as_preparation claims low extraction (technical preservation for future use). The decomposition follows ε-invariance: each reading instantiates a different constraint with different ε, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kodashim_obligation__study_as_archive, institutional, 0.2).
constraint_indexing:directionality_override(kodashim_obligation__study_as_archive, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
