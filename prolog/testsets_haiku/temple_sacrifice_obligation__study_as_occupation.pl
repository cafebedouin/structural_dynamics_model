% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__study_as_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__study_as_occupation, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: temple_sacrifice_obligation__study_as_occupation
 *   human_readable: Study of Sacrifice Law as Fulfillment of Temple Obligation
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   The study-as-occupation reading claims that engagement with sacrifice law
 *   in the Jewish intellectual tradition constitutes a legitimate fulfillment
 *   of the perpetual obligation to make daily offerings. After the Temple's
 *   destruction in 70 CE, halakhic authorities developed this interpretation:
 *   textual study and recitation of sacrificial procedures replace the
 *   performance they describe. The constraint operates through a hermeneutic
 *   system that absorbs the impossibility of literal performance without
 *   requiring that the obligation be revised or abandoned. Low extractiveness
 *   reflects that no party bears an asymmetric burden—the obligation is
 *   fulfilled through study participation, which scholars value
 *   intrinsically. No victim class emerges because the constraint redefines
 *   what counts as fulfillment rather than imposing an unfulfillable burden.
 *
 * KEY AGENTS:
 *   - Talmudic scholars and halakhic authorities: agenda-setters and primary beneficiaries; their institutional identity is constituted through perpetual engagement with sacrifice texts.
 *   - Jewish community: collective beneficiary; the constraint preserves living connection to Temple obligation through study practice.
 *   - Halakhic tradition: non-agent authority structure; codifies study-as-occupation as legitimate fulfillment mechanism.
 *   - Messianic restoration contingent: observers and implicit dissenters; holds that study preserves but does not fulfill.
 *   - Temple restoration movements: excluded voices; would argue study displaces urgency of literal restoration.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_occupation, 0.15).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_occupation, 0.05).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_occupation, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, extractiveness, 0.15).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_occupation, rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_occupation, "Study of Sacrifice Law as Fulfillment of Temple Obligation").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_occupation, "religious/halakhic").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_occupation, 'acdb246d-06f9-451c-a528-e7beed8d265c').
narrative_ontology:cs_kernel_codification('acdb246d-06f9-451c-a528-e7beed8d265c', fixed_text).
narrative_ontology:cs_authority_grounding('acdb246d-06f9-451c-a528-e7beed8d265c', lineage).
narrative_ontology:cs_interpretation_layer_present('acdb246d-06f9-451c-a528-e7beed8d265c').
narrative_ontology:cs_reading_relation('acdb246d-06f9-451c-a528-e7beed8d265c', temple_sacrifice_obligation__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('acdb246d-06f9-451c-a528-e7beed8d265c', temple_sacrifice_obligation__study_as_archiving, influences).
narrative_ontology:cs_axiom('acdb246d-06f9-451c-a528-e7beed8d265c', foundational, study_constitutes_legitimate_fulfillment).
narrative_ontology:cs_axiom_status(study_constitutes_legitimate_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('acdb246d-06f9-451c-a528-e7beed8d265c', study_constitutes_legitimate_fulfillment, deontological).
narrative_ontology:cs_axiom('acdb246d-06f9-451c-a528-e7beed8d265c', foundational, perpetual_obligation_doctrine).
narrative_ontology:cs_axiom_status(perpetual_obligation_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('acdb246d-06f9-451c-a528-e7beed8d265c', perpetual_obligation_doctrine, conventional).
narrative_ontology:cs_reference_frame('acdb246d-06f9-451c-a528-e7beed8d265c', study_as_valid_obligation_discharge).
narrative_ontology:cs_drift_state('acdb246d-06f9-451c-a528-e7beed8d265c', contemporary_halakhic_consensus, gap(stable, minor, true)).
narrative_ontology:cs_created_at('acdb246d-06f9-451c-a528-e7beed8d265c', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, talmudic_scholars).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, halakhic_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, jewish_community).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, substitutionary_intellectual_performance).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, perpetual_obligation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Perform the obligation through daily study and recitation of sacrifice law. The arrangement legitimizes intellectual engagement with Temple ritual as equivalent to ritual performance itself. Their entire institutional identity is constituted through perpetual engagement with sacrificial texts.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, talmudic_scholars, beneficiary,
    institutional, generational, identity_locked, universal).

% The community preserves connection to Temple obligation through study practice even without the Temple's physical existence. The obligation remains alive in textual and intellectual form rather than lapsing. Participation is structured through synagogue study and daily prayer-integrated recitation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, jewish_community, beneficiary,
    organized, generational, constrained, universal).

% The system of legal interpretation and transmission that codified study-as-occupation as a valid fulfillment mechanism. Adjudicates what counts as legitimate obligation-discharge. The tradition itself absorbs the impossibility of Temple performance without surfacing the need for fundamental revision.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, halakhic_tradition, agenda_setter,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_obligation__study_as_occupation, halakhic_tradition).

% Holds that study is preservation only, not fulfillment; that the obligation remains unfulfilled pending Temple restoration. They contest the adequacy of the study-as-occupation solution but remain embedded in the same institutional framework.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, messianic_restoration_contingent, observer,
    moderate, civilizational, identity_locked, universal).

% Would argue that study-as-occupation displaces the urgent necessity of actual restoration; that intellectual substitution produces complacency about literal Temple reconstitution. Their dissent from the study-occupation framing is structurally suppressed by the mainstream interpretive authority.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, temple_restoration_movements, excluded,
    moderate, civilizational, identity_locked, regional).

% External vantage point from which to analyze how the constraint operates: what it coordinates, what closure it establishes, how authority structures absorb the impossibility of literal performance.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains alive and binding a perpetual obligation whose original performance context (the Temple) no longer exists. Study preserves the obligation in functional form, keeping the Jewish legal tradition's continuity intact across the rupture of 70 CE and onwards. The constraint solves the problem of how a commandment can remain normatively binding when its performance is structurally impossible.
% TRANSFER_FUNCTION: Moves the locus of obligation fulfillment from Temple performance (impossible) to intellectual engagement with sacrificial texts (possible). No transfer of material goods or status occurs; the transfer is of the obligation's discharge vehicle itself.
% ABSENT_VOICES: Temple restoration advocates, messianic-activation theologians, and those who read the obligation as genuinely suspended rather than transmuted into study. They would object that study-as-occupation eliminates the pressure for literal Temple return and substitutes comfortable textual engagement for urgent restoration. Their dissent is structurally de-emphasized in mainstream halakhic discourse.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared—if study were declared NOT to fulfill the obligation—the Jewish community would face immediate bifurcation: some would pursue literal Temple restoration as the only lawful path; others would declare the obligation genuinely void. The authority structure currently prevents this contestation from surfacing as a live question by treating study-as-occupation as settled law, not as substitute arrangement awaiting Temple return.
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE made performance of the daily sacrifice obligation impossible. The constraint developed as a halakhic solution: how can a binding obligation persist when its performance condition is no longer met? Talmudic authorities interpreted scriptural and early rabbinic sources to establish that textual engagement with sacrifice law constitutes legitimate fulfillment.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream halakhic authorities (Maimonides, Karo, contemporary decisors) attest the problem remains solved by the study arrangement and requires no further action. Messianic and restoration-focused theological voices attest the problem is NOT solved—that study is occupation, not resolution, and the obligation truly awaits Temple return. Historical-critical scholars and some contemporary Jewish philosophers attest the foundational problem is theological, not practical: the constraint preserves a narrative of perpetual obligation without acknowledging that the original performance context is gone.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__study_as_occupation, contested).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__study_as_occupation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__study_as_occupation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__study_as_occupation, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__study_as_occupation, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__study_as_occupation_tests).
:- end_tests(temple_sacrifice_obligation__study_as_occupation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.15 final) because the constraint coordinates a genuine obligation through a intellectually accessible medium, and no identified party bears asymmetric burden. The community fulfills its obligation by studying; scholars pursue intellectual engagement that they intrinsically value. Theater is also low (0.12) because the study practice is substantive—Talmudic texts on sacrifice are lengthy, complex, and studied seriously for their own legal content, not merely performed to maintain ritual form. Resistance is modest (0.22) because dissenting voices exist (messianic, restoration-focused) but remain minority positions within the halakhic consensus; the interpretive tradition has largely settled the matter. Accessibility collapse is moderate (0.35) because once the study-as-occupation doctrine is internalized, alternatives become hard to perceive—the intellectual tradition legitimizes this path and de-emphasizes messianic or suspensionist alternatives—but because the Temple does not physically exist, the gap between study and performance remains theoretically visible to any observer. The time-series is near-flat because the constraint stabilized centuries ago; modest upward drift in theater_ratio reflects increased scholastic systematization (Maimonidean codification, Shulchan Aruch formalization) which formalized study obligations but did not fundamentally change extractiveness.
 *
 * PERSPECTIVAL GAP:
 *   All seats experience this as a coordination solution, not as extraction. Scholars and community are beneficiaries not because they collect rents but because study fulfills an obligation they accept as binding. No victim seats exist under this reading. The potential perspectival divide emerges between the study-as-occupation seat (this constraint) and the messianic-suspension or archiving readings: if a speaker holds messianic_suspension, they experience this constraint as intellectually suppressing the acknowledged unfulfillment of the obligation; if they hold study-as-archiving, they experience study as preservation-only and thus as perpetuating a false closure. But WITHIN the study-as-occupation frame, no asymmetry emerges.
 *
 * DIRECTIONALITY LOGIC:
 *   All named agents sit near the beneficiary end (d near 0.0) because none are trapped or identity-locked into bearing costs against their values. Talmudic scholars are identity-locked, but their lock is TOWARD the study practice, not against it—their professional and spiritual identity is constituted through text engagement they affirm. The Jewish community experiences low d (beneficiary-side) because study is experienced as fulfilling an obligation they accept. Even the messianic contingent, who contest this reading, are identity-locked within the halakhic tradition itself—they hold an alternative reading, not an external position. No party is structurally pushed into this obligation against their values (the constraint is not imposed on the powerless); it is internally held by a committed tradition.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT exhibit mandatrophy. The founding problem (Temple destruction, need for perpetual obligation to survive the rupture) remains live—the Jewish legal and spiritual tradition continues to treat the obligation as binding. The study mechanism remains the operative discharge vehicle. No gap emerges between the declared purpose (keeping the obligation alive in perpetually binding form) and the actual operation (study as legitimate fulfillment). The absence of victimhood and the low theater ratio confirm that the constraint is not persisting as theatrical zombie after its function atrophied. If mandatrophy were present, we would expect high theater_ratio (study performed as ritual form without substantive legal engagement) and either disappearance of the constraint from halakhic discourse or explicit redefinition. Neither has occurred.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substitutionary_adequacy_of_study,
    'Does textual engagement with sacrificial law genuinely constitute fulfillment of the obligation, or is it a hermeneutically creative substitute that perpetuates obligation while eliminating performance pressure?',
    'Philosophical or theological analysis of the justificatory structure: does the halakhic tradition provide principled grounds for the equivalence of study and performance, or does it rest on authority-assertion? Comparative analysis with other substitutionary obligation-discharge mechanisms in Jewish law (prayer as substitute for Temple sacrifice itself, for example).',
    'If study is genuinely adequate per the tradition''s own logic, the constraint operates as pure coordination. If the adequacy rests on authority-closure rather than principled equivalence, the constraint begins to accumulate low-level extractiveness (the authority structure extracts deference by foreclosing the question).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitutionary_adequacy_of_study, conceptual, 'Whether the equivalence of study to performance is logically grounded or authority-imposed.').

omega_variable(
    messianic_obligation_status_under_study,
    'If the Messiah comes and the Temple is rebuilt, does the obligation shift from study back to performance immediately, or does study-as-occupation retain status as an acceptable alternative form?',
    'Examination of halakhic sources that address the post-messianic scenario; theological traditions that envision Temple restoration; contemporary decisors'' treatment of the question.',
    'If study is TRULY a substitute and retains legitimacy post-restoration, the constraint is reading-invariant (orthogonal to messianic status). If study is merely a stopgap—legitimate only in the Temple''s absence—the constraint is contingent on a specific historical state and would require reclassification in a restored scenario. This determines whether the constraint should be modeled as a permanent halakhic principle or a historically-bound accommodation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(messianic_obligation_status_under_study, conceptual, 'Whether study-as-occupation is a permanent principle or contingent on Temple absence.').

omega_variable(
    authority_closure_mechanism,
    'Does the authority structure actively suppress messianic and restoration-focused interpretations of the obligation, or do those alternatives remain genuinely open positions within the halakhic discourse?',
    'Analysis of how contemporary halakhic literature treats messianic_suspension and archiving readings; whether dissenting voices are engaged or dismissed; degree to which Temple restoration theology is integrated into or marginalized from mainstream halakhic study.',
    'If alternatives are actively suppressed, a low-level suppression metric (0.05) may underestimate the constraint''s social force—the authority structure is quietly de-emphasizing rival readings. If alternatives remain genuinely open, the low suppression reflects accurate situation. This affects whether the constraint should carry omega on suppression mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_closure_mechanism, empirical, 'Degree to which the halakhic authority structure allows alternative readings to surface in discourse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_occupation, 70, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t70, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 70, 0.05).
narrative_ontology:measurement_basis(temp_tr_t70, projected).
narrative_ontology:measurement(temp_tr_t500, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 500, 0.08).
narrative_ontology:measurement_basis(temp_tr_t500, projected).
narrative_ontology:measurement(temp_tr_t1000, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 1000, 0.1).
narrative_ontology:measurement_basis(temp_tr_t1000, projected).
narrative_ontology:measurement(temp_tr_t1500, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 1500, 0.12).
narrative_ontology:measurement_basis(temp_tr_t1500, projected).
narrative_ontology:measurement(temp_tr_t1900, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 1900, 0.13).
narrative_ontology:measurement_basis(temp_tr_t1900, observed).
narrative_ontology:measurement(temp_tr_t2026, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 2026, 0.12).
narrative_ontology:measurement_basis(temp_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(temp_be_t70, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 70, 0.08).
narrative_ontology:measurement_basis(temp_be_t70, projected).
narrative_ontology:measurement(temp_be_t500, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 500, 0.12).
narrative_ontology:measurement_basis(temp_be_t500, projected).
narrative_ontology:measurement(temp_be_t1000, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 1000, 0.14).
narrative_ontology:measurement_basis(temp_be_t1000, projected).
narrative_ontology:measurement(temp_be_t1500, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 1500, 0.15).
narrative_ontology:measurement_basis(temp_be_t1500, projected).
narrative_ontology:measurement(temp_be_t1900, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 1900, 0.16).
narrative_ontology:measurement_basis(temp_be_t1900, observed).
narrative_ontology:measurement(temp_be_t2026, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 2026, 0.15).
narrative_ontology:measurement_basis(temp_be_t2026, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(temple_sacrifice_obligation__study_as_occupation, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__study_as_occupation, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_obligation__study_as_occupation, 0.08).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation__messianic_suspension).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation__study_as_archiving).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the temple_sacrifice_obligation kernel. The kernel is the standing Jewish obligation to make daily sacrificial offerings, which became impossible after the Temple's destruction in 70 CE. Three structurally distinct constraint stories instantiate three readings: (1) study_as_occupation (this file) models study as legitimate fulfillment, low extractiveness, rope-type; (2) messianic_suspension models the obligation as suspended pending restoration, minimal coordination function, mountain-type; (3) study_as_archiving models study as preservation without fulfillment, moderate extractiveness, tangled_rope-type (the tradition extracts deference while perpetuating the unfulfilled obligation). Each reading has its own ε value, victim/beneficiary structure, and authority relationship. They coexist within the Jewish halakhic tradition, held by different parties and communities. The network links represent how each reading influences the others' framing conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
