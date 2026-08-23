% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__study_as_performance, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: sacrifice_obligation_continuity__study_as_performance
 *   human_readable: Study as Fulfillment of Sacrifice Obligation
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   This constraint story models the 'study as performance' reading of the
 *   sacrifice obligation continuity kernel — the rabbinic claim that textual
 *   engagement with sacrificial law (especially through Talmudic tractates
 *   Zevachim, Menachot, Tamid) constitutes fulfillment of the biblical
 *   commandment to offer sacrifices. The reading emerged after the Second
 *   Temple's destruction (70 CE) as the primary rabbinic solution to the
 *   existential crisis of a Temple-centered religion without a Temple. It
 *   operates as a pure coordination mechanism (rope): it solves the
 *   collective action problem of obligation continuity with minimal coercive
 *   overhead, no victim class, and accessible participation. The constraint's
 *   extraction is low (0.15) — study is open to all, requires no material
 *   tribute, and the primary 'cost' is time devoted to learning. Suppression
 *   is minimal (0.1) — social pressure exists but alternatives (other
 *   readings) remain live and uncontested by force. Theater ratio is low
 *   (0.1) — the study practice is genuinely functional, not performative. The
 *   measurement series (0–1950 CE) shows gradual metric drift as the practice
 *   institutionalized: extractiveness, theater, and suppression all rise
 *   modestly as yeshiva systems formalized and communal support structures
 *   developed.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__study_as_performance, 0.15).
domain_priors:suppression_score(sacrifice_obligation_continuity__study_as_performance, 0.1).
domain_priors:theater_ratio(sacrifice_obligation_continuity__study_as_performance, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, extractiveness, 0.15).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__study_as_performance, rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__study_as_performance, "Study as Fulfillment of Sacrifice Obligation").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__study_as_performance, "religious_law/ritual_studies/textual_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__study_as_performance, '19956fd2-0110-49e4-844d-eab97c6d3fcd').
narrative_ontology:cs_kernel_codification('19956fd2-0110-49e4-844d-eab97c6d3fcd', fixed_text).
narrative_ontology:cs_authority_grounding('19956fd2-0110-49e4-844d-eab97c6d3fcd', lineage).
narrative_ontology:cs_interpretation_layer_present('19956fd2-0110-49e4-844d-eab97c6d3fcd').
narrative_ontology:cs_reading_relation('19956fd2-0110-49e4-844d-eab97c6d3fcd', sacrifice_obligation_continuity__archival_preservation, forecloses).
narrative_ontology:cs_reading_relation('19956fd2-0110-49e4-844d-eab97c6d3fcd', sacrifice_obligation_continuity__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('19956fd2-0110-49e4-844d-eab97c6d3fcd', sacrifice_obligation_continuity__performance_only, forecloses).
narrative_ontology:cs_axiom('19956fd2-0110-49e4-844d-eab97c6d3fcd', foundational, study_constitutes_ritual_fulfillment).
narrative_ontology:cs_axiom_status(study_constitutes_ritual_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('19956fd2-0110-49e4-844d-eab97c6d3fcd', study_constitutes_ritual_fulfillment, deontological).
narrative_ontology:cs_axiom('19956fd2-0110-49e4-844d-eab97c6d3fcd', foundational, textual_engagement_sustains_obligation).
narrative_ontology:cs_axiom_status(textual_engagement_sustains_obligation, holdable).
narrative_ontology:cs_axiom_grounding('19956fd2-0110-49e4-844d-eab97c6d3fcd', textual_engagement_sustains_obligation, conventional).
narrative_ontology:cs_reference_frame('19956fd2-0110-49e4-844d-eab97c6d3fcd', rabbinic_substitution_framework).
narrative_ontology:cs_drift_state('19956fd2-0110-49e4-844d-eab97c6d3fcd', contemporary_post_denominational_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('19956fd2-0110-49e4-844d-eab97c6d3fcd', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, traditional_scholars).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, student_practitioners).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, ritual_community).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__study_as_performance, study_as_ritual_fulfillment).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__study_as_performance, textual_engagement_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rabbinic authorities (poskim, roshei yeshiva) who maintain and transmit the ruling that textual study of sacrifice laws constitutes fulfillment of the biblical obligation. They authoritatively adjudicate the boundaries of what counts as valid study, which texts are authoritative, and how the obligation applies across changing circumstances. Their position depends on the continuity of the interpretive tradition; exit would mean abandoning their role in the chain of transmission.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, interpretive_authorities, agenda_setter,
    institutional, generational, constrained, global).

% Full-time Torah scholars whose primary occupation is the study of sacrificial law (kodashim). They receive communal support (stipends, honored status) precisely because their study is recognized as fulfilling the sacrifice obligation. The constraint structures their vocational identity and material sustenance. Exit is mobile — they could pursue other occupations — but identity-locked for those whose self-concept is constituted through this study.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, traditional_scholars, beneficiary,
    organized, biographical, mobile, global).

% Yeshiva students and laypeople who engage in regular study of sacrificial laws as part of their religious practice. They experience the study as both obligation and privilege — the constraint gives their learning ritual significance. Exit is constrained by communal expectations and internalized commitment; leaving the practice would mean losing a primary mode of religious fulfillment.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, student_practitioners, beneficiary,
    moderate, biographical, constrained, global).

% The broader observant community that participates in and supports the study enterprise — through funding institutions, attending public study sessions, and treating scholars as fulfilling a communal need. The constraint provides a shared framework for collective continuity when the Temple is absent. Exit is constrained by communal belonging; the practice is woven into communal identity.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, ritual_community, beneficiary,
    organized, generational, constrained, global).

% Groups (e.g., Temple Institute, certain messianic factions) who hold that only physical performance on the Temple Mount fulfills the obligation. They view study as preparation, not fulfillment. They observe the study-as-performance reading from outside, often criticizing it as a substitute that delays restoration. Their situation is analytical — they track the constraint's operation to argue against its sufficiency.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, performance_only_adherents, observer,
    organized, generational, analytical, global).

% Communities (e.g., certain Hasidic groups, Religious Zionist factions) who hold the obligation is suspended pending messianic restoration; study maintains readiness but does not fulfill. They observe the study-as-performance reading as a parallel live position. Their situation is analytical — they engage with the constraint's logic to distinguish their reading.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, messianic_suspension_adherents, observer,
    organized, generational, analytical, global).

% Academic scholars, liberal religious movements, and cultural preservationists who treat sacrificial law as historical text without normative force. They observe the study-as-performance reading as a cultural phenomenon to document. Their situation is analytical — they study the constraint from outside its normative claims.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, archival_preservation_adherents, observer,
    moderate, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains continuity of the biblical sacrifice obligation across the historical rupture of Temple destruction by transmuting physical performance into textual engagement, providing a collective practice that sustains communal identity and obligation without requiring the Temple's physical infrastructure.
% TRANSFER_FUNCTION: Moves interpretive authority and communal cohesion from the priestly caste performing physical rituals to the scholarly caste performing textual study; transfers the locus of fulfillment from the Temple courtyard to the study hall (beit midrash). No material transfer — the transfer is of ritual valence and communal recognition.
% ABSENT_VOICES: Priestly families (kohanim) who would perform the physical sacrifices if the Temple stood — their specific ritual role has been absorbed into the generalized scholar role. Also absent: those who would reject the obligation entirely (secular Jews, apostates) — they are not in the conversation because the constraint only operates within the normative community.
% DISAPPEARANCE_RATIONALE: If the study-as-performance reading vanished overnight, the observant community would lose its primary mode of fulfilling the sacrifice obligation. The obligation would either collapse (no fulfillment possible), shift to performance_only activism (Temple Mount movements), shift to messianic_suspension (passive waiting), or shift to archival_preservation (cultural memory only) — each rearrangement restructuring communal practice and authority.
% FOUNDING_PROBLEM: After the Roman destruction of the Second Temple (70 CE), the biblical system of sacrificial worship — central to Israelite religion — became physically impossible. The founding problem: how does the covenantal obligation to offer sacrifices persist when the Temple, priesthood, and altar no longer exist?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the Mishnah (Zevachim, Menachot) and Talmud (Bavli Menachot 110a: 'Whoever studies the laws of sacrifice is as if he offered a sacrifice'), which explicitly frame study as the rabbinic solution to Temple destruction. Contemporary historians of religion (e.g., Jacob Neusner, Haym Soloveitchik) corroborate from outside the tradition that this interpretive move solved an existential continuity crisis for rabbinic Judaism.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__study_as_performance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__study_as_performance, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__study_as_performance_tests).
:- end_tests(sacrifice_obligation_continuity__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed type 'rope' reflects the structural reality: a genuine coordination function (obligation continuity without Temple) with voluntary participation, no extraction from a victim class, and no active enforcement machinery. The low extractiveness (0.15) captures that study is accessible — texts are public, no gatekeeper controls entry, and the 'cost' is time investment that participants value intrinsically. Suppression (0.1) reflects mild social expectation, not coercion. Accessibility collapse (0.35) is moderate — alternatives (performance_only, messianic_suspension, archival_preservation) exist and are intellectually accessible, though communally marginal. Resistance (0.2) is low — the reading dominates the traditionalist sector but faces intellectual contestation from other readings, not active resistance.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute similar rope classifications across all beneficiary seats because the structural asymmetry is minimal — all beneficiaries experience low extraction, accessible exit, and genuine coordination benefit. The observer seats will also compute rope (they are not subject to the constraint). The perspectival gap is narrow: the constraint is experienced as coordination by all who engage it. The divergence appears only if one compares this reading's computed type against sibling readings' types — performance_only computes as tangled_rope (coordination + extraction from those who cannot perform), messianic_suspension as scaffold (transitional), archival_preservation as mountain (no normative force).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary declarations: interpretive_authorities (agenda_setter) sit near d=0.15 (they administer and benefit from the system's continuity); traditional_scholars and student_practitioners (beneficiaries) sit near d=0.1 (they receive status/support/fulfillment); ritual_community (beneficiary) sits near d=0.2 (collective benefit, diffuse cost of supporting institutions). The observer seats (other readings' adherents) sit at d=0.5 (analytical, symmetric). No victim/payer seats exist — the constraint extracts from no one. This beneficiary-only structure is why effective extraction χ remains near base ε for all seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Temple destruction) remains live — the Temple is not rebuilt, the physical obligation cannot be performed. The arrangement has not outlived its function; it continues to solve the continuity problem it was built for. Mandatrophy is not resolved. The constraint persists because its founding problem persists. The gradual metric drift (rising extractiveness/theater/suppression) reflects institutionalization, not mandatrophy — the core coordination function remains live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the ''study as performance'' reading a distinct constraint with its own ε, or a measurement basis within a single sacrifice obligation constraint?',
    'Apply ε-invariance test: if measuring the constraint via ''study fulfillment'' yields ε≈0.15 but measuring via ''physical performance required'' yields ε≈0.7 (for those unable to access Temple Mount), the ε differs by observable — therefore two constraints, not one. The decomposition into separate constraint stories (this file plus siblings) resolves the ambiguity.',
    'If ε-invariance holds, each reading is a separate constraint story with its own classification. If violated, the kernel is a single constraint with reading-dependent classification — which the framework rejects. This omega documents the committer-frame commitment to decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s readings are structurally distinct constraints (ε-invariance) or observational perspectives on one constraint.').

omega_variable(
    foreclosure_boundary_performance_only,
    'Does the study_as_performance reading logically foreclose the performance_only reading within a single normative framework, or can a party hold both?',
    'Analyze the core premises: study_as_performance claims ''study = fulfillment''; performance_only claims ''study ≠ fulfillment (preparation only)''. If a single party''s framework cannot simultaneously affirm and deny that study constitutes fulfillment, the relation is forecloses. If the party can hold ''study fulfills now, performance fulfills later'' as a staged fulfillment, the relation is coexists_with.',
    'If forecloses, the readings cannot coexist in one community''s authoritative teaching — one must be rejected. If coexists_with, a community could teach both as complementary stages. This determines the reading_relations declaration and the engine''s foreclosure computation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreclosure_boundary_performance_only, conceptual, 'Logical relationship between study-as-fulfillment and performance-only readings within a single normative framework.').

omega_variable(
    authority_erosion_measurement,
    'How should the substantial authority_erosion drift (denominational fragmentation) be measured when the interpretive authorities themselves do not acknowledge it?',
    'Track institutional markers: number of recognized poskim across denominations, citation networks in responsa literature, enrollment in non-Orthodox yeshivot studying kodashim, public opinion surveys on sacrifice obligation beliefs. The gap between acknowledged (false) and measured (substantial) drift is the signal.',
    'If drift is substantial and unacknowledged, the constraint may be approaching a codification_collapse or practice_drift transition — the reference frame (rabbinic substitution framework) is fracturing. This affects the terminal attractor computation (t2) in the commitment system engine.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authority_erosion_measurement, empirical, 'Measurement of unacknowledged authority erosion in a living interpretive tradition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__study_as_performance, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacrifice_obligation_continuity__study_as_performance_tr_t0, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 0, 0.05).
narrative_ontology:measurement(sacrifice_obligation_continuity__study_as_performance_tr_t500, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 500, 0.07).
narrative_ontology:measurement(sacrifice_obligation_continuity__study_as_performance_tr_t1000, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 1000, 0.08).
narrative_ontology:measurement(sacrifice_obligation_continuity__study_as_performance_tr_t1500, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 1500, 0.09).
narrative_ontology:measurement(sacrifice_obligation_continuity__study_as_performance_tr_t1950, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 1950, 0.1).

% Extraction over time
narrative_ontology:measurement(sacrifice_obligation_continuity__study_as_performance_be_t0, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(sacrifice_obligation_continuity__study_as_performance_be_t500, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 500, 0.08).
narrative_ontology:measurement(sacrifice_obligation_continuity__study_as_performance_be_t1000, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 1000, 0.1).
narrative_ontology:measurement(sacrifice_obligation_continuity__study_as_performance_be_t1500, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 1500, 0.12).
narrative_ontology:measurement(sacrifice_obligation_continuity__study_as_performance_be_t1950, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 1950, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(sacrifice_obligation_continuity__study_as_performance_su_t0, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(sacrifice_obligation_continuity__study_as_performance_su_t500, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 500, 0.07).
narrative_ontology:measurement(sacrifice_obligation_continuity__study_as_performance_su_t1000, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 1000, 0.08).
narrative_ontology:measurement(sacrifice_obligation_continuity__study_as_performance_su_t1500, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 1500, 0.09).
narrative_ontology:measurement(sacrifice_obligation_continuity__study_as_performance_su_t1950, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 1950, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__study_as_performance, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_continuity__study_as_performance, 0.08).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__archival_preservation).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the sacrifice_obligation_continuity kernel. The ε values differ substantially: study_as_performance ε≈0.15 (rope), performance_only ε≈0.7 (tangled_rope — coordinates Temple Mount access but extracts from those denied), messianic_suspension ε≈0.3 (scaffold — transitional with sunset at messianic arrival), archival_preservation ε≈0.02 (mountain — no normative force, negligible extraction). The family is linked via affects_constraints; the upstream reading (study_as_performance) historically influenced the downstream readings as the dominant rabbinic solution that later readings reacted to.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
