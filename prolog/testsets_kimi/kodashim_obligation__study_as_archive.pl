% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_archive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: kodashim_obligation__study_as_archive
 *   human_readable: Kodashim Study as Historical Archive and Identity Maintenance
 *   domain: religious/law/textual_preservation
 *
 * SUMMARY:
 *   Kodashim, the Talmudic order treating sacrifices and Temple worship,
 *   documents a system defunct since the Second Temple's destruction. In the
 *   study_as_archive reading, ongoing obligation to master Kodashim is framed
 *   as historical preservation and identity-maintenance: the rabbinic
 *   curriculum keeps the full textual corpus intact to sustain communal
 *   continuity, but the study yields no actionable legal output. The
 *   constraint extracts intellectual resources from students diverted from
 *   applicable law, while benefiting the institutional authority that derives
 *   legitimacy from an unbroken chain of Torah study. Temple restoration is
 *   treated as structurally impossible or undesired, distinguishing this
 *   reading from sibling readings that treat the same texts as cosmic
 *   performance or messianic preparation.
 *
 * KEY AGENTS:
 *   - rabbinic_curriculum_authorities (institutional/constrained): Agenda-setter and beneficiary who enforce Kodashim study and capture legitimacy from historical continuity
 *   - advanced_talmudic_students (powerless/identity_locked): Primary target whose intellectual labor is diverted to non-applicable texts
 *   - contemporary_halakhic_communities (moderate/mobile): Excluded voice that loses applied scholarly attention
 *   - temple_restoration_advocates (moderate/mobile): Excluded voice marginalized by the impossibility/undesirability premise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_archive, 0.55).
domain_priors:suppression_score(kodashim_obligation__study_as_archive, 0.35).
domain_priors:theater_ratio(kodashim_obligation__study_as_archive, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, extractiveness, 0.55).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_archive, tangled_rope).
narrative_ontology:human_readable(kodashim_obligation__study_as_archive, "Kodashim Study as Historical Archive and Identity Maintenance").
narrative_ontology:topic_domain(kodashim_obligation__study_as_archive, "religious/law/textual_preservation").

domain_priors:requires_active_enforcement(kodashim_obligation__study_as_archive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_archive, '34b9f41d-5d88-4e13-9a65-2bdfaad150f9').
narrative_ontology:cs_kernel_codification('34b9f41d-5d88-4e13-9a65-2bdfaad150f9', fixed_text).
narrative_ontology:cs_authority_grounding('34b9f41d-5d88-4e13-9a65-2bdfaad150f9', lineage).
narrative_ontology:cs_interpretation_layer_present('34b9f41d-5d88-4e13-9a65-2bdfaad150f9').
narrative_ontology:cs_reading_relation('34b9f41d-5d88-4e13-9a65-2bdfaad150f9', kodashim_obligation__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('34b9f41d-5d88-4e13-9a65-2bdfaad150f9', kodashim_obligation__study_as_preparation, influences).
narrative_ontology:cs_axiom('34b9f41d-5d88-4e13-9a65-2bdfaad150f9', foundational, temple_cult_defunct_and_unrestorable).
narrative_ontology:cs_axiom_status(temple_cult_defunct_and_unrestorable, holdable).
narrative_ontology:cs_axiom_grounding('34b9f41d-5d88-4e13-9a65-2bdfaad150f9', temple_cult_defunct_and_unrestorable, empirically_contingent).
narrative_ontology:cs_axiom('34b9f41d-5d88-4e13-9a65-2bdfaad150f9', foundational, historical_preservation_sustains_communal_identity).
narrative_ontology:cs_axiom_status(historical_preservation_sustains_communal_identity, holdable).
narrative_ontology:cs_axiom_grounding('34b9f41d-5d88-4e13-9a65-2bdfaad150f9', historical_preservation_sustains_communal_identity, conventional).
narrative_ontology:cs_reference_frame('34b9f41d-5d88-4e13-9a65-2bdfaad150f9', rabbinic_archival_framework).
narrative_ontology:cs_drift_state('34b9f41d-5d88-4e13-9a65-2bdfaad150f9', contemporary_yeshiva_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('34b9f41d-5d88-4e13-9a65-2bdfaad150f9', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_archive, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, rabbinic_curriculum_authorities).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_archive, advanced_talmudic_students).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the yeshiva curriculum requiring advanced study of Kodashim across the Talmudic corpus. Derives institutional legitimacy from the claim of unbroken Torah study that encompasses even defunct Temple law. Cannot abandon Kodashim without rupturing the continuity narrative that grounds their authority.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, rabbinic_curriculum_authorities, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_obligation__study_as_archive, rabbinic_curriculum_authorities, beneficiary).

% Spend years mastering tractates Kodashim that produce no actionable legal rulings in contemporary Jewish life. Their intensive intellectual labor sustains communal identity and rabbinic legitimacy, but diverts capacity from tractates governing daily practice. Exit from this track means forfeiting status within the Torah-study prestige economy.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, advanced_talmudic_students, payer,
    powerless, biographical, identity_locked, national).

% Communities seeking practical guidance on contemporary halakhic problems. They receive less scholarly attention and fewer original rulings because the elite study pipeline is channelled into non-performative historical texts rather than applicable law.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, contemporary_halakhic_communities, excluded,
    moderate, biographical, mobile, local).

% Theologians and activists who read Kodashim study as technical preparation for Third Temple restoration. They are marginalized in this reading because the framework treats restoration as structurally impossible or undesired, rendering their preparatory rationale moot.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, temple_restoration_advocates, excluded,
    moderate, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_obligation__study_as_archive, rabbinic_curriculum_authorities).
narrative_ontology:fixing_cost_class(kodashim_obligation__study_as_archive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the textual integrity of the entire Talmudic corpus and sustains Jewish communal identity through symbolic continuity with the defunct Second Temple cult, solving the collective problem of cohesion after the center of worship was destroyed.
% TRANSFER_FUNCTION: Moves intellectual labor and rabbinic credibility from advanced students and applicable-law communities to the institutional authority that claims legitimacy through an unbroken chain of Torah study encompassing the defunct.
% ABSENT_VOICES: Temple restoration advocates and messianic theologians who would argue for preparatory study; contemporary halakhic communities who would demand more applied jurisprudential attention. Both are structurally absent from curricular decision-making.
% DISAPPEARANCE_RATIONALE: If the obligation to study Kodashim vanished, yeshiva curricula would reorganize toward applicable law, the institutional claim of unbroken continuity would weaken, and communal self-understanding would shift away from Temple-centric identity maintenance.
% FOUNDING_PROBLEM: The destruction of the Second Temple and the need to maintain Jewish legal continuity and communal cohesion without a functioning sacrificial system.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of Second Temple Judaism and critical Talmudic scholars attest to the destruction and the rabbinic pivot to non-sacrificial Judaism. The curriculum authorities themselves acknowledge that sacrificial law is currently unperformable, corroborating from outside the pure beneficiary circle that the founding crisis is historically resolved.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_archive, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_archive, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_archive, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_obligation__study_as_archive, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_archive, 0.55, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is moderate (0.55) because the diversion of intellectual labor is real but bounded by a genuine identity-maintenance function. Suppression is moderate-low (0.35) because enforcement operates through curricular gatekeeping and identity fusion rather than overt coercion. Theater_ratio is moderate (0.55) because the absence of functional legal output means much study activity sustains legitimacy performance rather than practical jurisprudence. Accessibility_collapse is moderate (0.40): alternatives (focusing on applicable tractates) exist but carry status penalties within the yeshiva world. Resistance is moderate (0.30) because applied-law advocates periodically push for curricular reform but remain marginalized. The measurement series share one time grid so temporal analysis avoids spurious transition dating.
 *
 * PERSPECTIVAL GAP:
 *   From the curriculum authority's seat, Kodashim study is the indispensable archive of a people; from the student's seat, it is a costly ritual of mastery with no courtroom; from the applied-law community's seat, it is a drainage of scholarly talent. The engine computes these divergences from the structural data rather than adjudicating which perception is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinic_curriculum_authorities sit near the beneficiary end (low d): the constraint subsidizes their legitimacy claim by making them the guardians of an unbroken textual chain. Advanced_talmudic_students sit near the target end (high d): their cognitive labor is the extracted resource. Contemporary_halakhic_communities sit at moderate-high d through indirect diversion of scholarly output. The engine will compute divergent per-seat classifications from this structural asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâmaintaining legal continuity after Temple destructionâis dead. The arrangement persists because it has been repurposed for identity maintenance. This classification resists mislabeling as pure extraction because a genuine coordination function (communal identity) remains, and resists mislabeling as pure coordination because the intellectual cost is borne asymmetrically by students who gain no applicable skill. The dead founding problem plus ongoing rearrangement-dependent presence places the constraint in the tangled_rope zone rather than scaffold or piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the Kodashim obligation is structurally dominantâarchive, performance, or preparationâand does the choice depend on theological community affiliation rather than textual fact?',
    'Cross-communal curriculum survey measuring which reading is dominant in which institutions, paired with textual analysis to see if the source corpus underdetermines the reading selection.',
    'If dominance tracks community boundary rather than textual evidence, the constraint''s classification as tangled_rope reflects institutional power rather than inherent textual structure; a single uncontested reading would shift the constraint toward rope or mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Contested kernel reading ambiguity for Kodashim obligation').

omega_variable(
    hidden_functional_output,
    'Does study of Kodashim transfer hermeneutical or logical skills that indirectly enhance applicable-law reasoning, reducing the true extractiveness of the diversion?',
    'Controlled comparison of juridical reasoning between scholars with and without intensive Kodashim training, evaluated by blind halakhic adjudication panels.',
    'Significant skill transfer would lower effective extraction and push the constraint toward rope; absence of transfer would confirm the moderate extraction score.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hidden_functional_output, empirical, 'Whether Kodashim study yields hidden applicable-law competencies').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the curricular enforcement structural (yeshiva admission and advancement gatekeeping) or internalized (students fuse their scholarly identity with the full corpus)?',
    'Post-exit trajectory analysis: if students who leave the Kodashim track continue to feel obligation to master it, suppression is partially internalized; if pressure drops immediately upon institutional departure, it is structural.',
    'Internalized suppression raises effective extraction above the structural measure because students carry the constraint with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_archive, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kodashim_archive_tr_t0, kodashim_obligation__study_as_archive, theater_ratio, 0, 0.2).
narrative_ontology:measurement(kodashim_archive_tr_t10, kodashim_obligation__study_as_archive, theater_ratio, 10, 0.28).
narrative_ontology:measurement(kodashim_archive_tr_t20, kodashim_obligation__study_as_archive, theater_ratio, 20, 0.35).
narrative_ontology:measurement(kodashim_archive_tr_t30, kodashim_obligation__study_as_archive, theater_ratio, 30, 0.42).
narrative_ontology:measurement(kodashim_archive_tr_t40, kodashim_obligation__study_as_archive, theater_ratio, 40, 0.48).
narrative_ontology:measurement(kodashim_archive_tr_t50, kodashim_obligation__study_as_archive, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(kodashim_archive_be_t0, kodashim_obligation__study_as_archive, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(kodashim_archive_be_t10, kodashim_obligation__study_as_archive, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(kodashim_archive_be_t20, kodashim_obligation__study_as_archive, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(kodashim_archive_be_t30, kodashim_obligation__study_as_archive, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(kodashim_archive_be_t40, kodashim_obligation__study_as_archive, base_extractiveness, 40, 0.53).
narrative_ontology:measurement(kodashim_archive_be_t50, kodashim_obligation__study_as_archive, base_extractiveness, 50, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(kodashim_archive_su_t0, kodashim_obligation__study_as_archive, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(kodashim_archive_su_t10, kodashim_obligation__study_as_archive, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(kodashim_archive_su_t20, kodashim_obligation__study_as_archive, suppression_requirement, 20, 0.25).
narrative_ontology:measurement(kodashim_archive_su_t30, kodashim_obligation__study_as_archive, suppression_requirement, 30, 0.3).
narrative_ontology:measurement(kodashim_archive_su_t40, kodashim_obligation__study_as_archive, suppression_requirement, 40, 0.33).
narrative_ontology:measurement(kodashim_archive_su_t50, kodashim_obligation__study_as_archive, suppression_requirement, 50, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_archive, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
