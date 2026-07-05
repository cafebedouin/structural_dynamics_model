% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__study_as_exercise_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__study_as_exercise_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: sacrifice_obligation_kernel__study_as_exercise_reading
 *   human_readable: Study of Sacrifice Law as Fulfillment of the Mitzvah (Talmud Torah k'neged korban)
 *   domain: religious_law/halakhic_authority
 *
 * SUMMARY:
 *   This story instantiates ONE reading within the
 *   sacrifice_obligation_kernel: the claim that study of sacrificial law is
 *   not preparatory or substitutionary but IS the mitzvah, fully discharged
 *   through intellectual engagement, under present conditions. This is a
 *   longstanding rabbinic doctrine (frequently traced to Talmudic statements
 *   equating Torah study of sacrificial procedure with the offering itself)
 *   that resolved the acute post-Temple problem of an entire
 *   commandment-category becoming unfulfillable. Under this reading there is
 *   no victim set: nothing is extracted from anyone, because the obligation
 *   is treated as genuinely occupied by the intellectual act, not merely
 *   deferred or symbolically gestured at. The beneficiary is
 *   rabbinic/academic authority, which gains an interpretive monopoly over
 *   what counts as adequate 'exercise' of the mitzvah — depth, method, and
 *   lineage of study become the site of ongoing adjudicative control, even
 *   though no coercive extraction occurs. This story does NOT describe the
 *   sibling readings (performance_only, messianic_suspension,
 *   symbolic_archive) — those are separate constraints with their own ε
 *   values, linked via network.affects_constraints, per the ε-invariance
 *   principle. Averaging across readings or hedging ε to accommodate the
 *   sibling positions would violate Rule 1.
 *
 * KEY AGENTS:
 *   - rabbinic_academies: agenda_setter/beneficiary (institutional/arbitrage) — certify study as fulfillment, secure institutional centrality
 *   - yeshiva_students: beneficiary (moderate/mobile) — discharge the mitzvah through study labor, gain status
 *   - halakhic_decisors: beneficiary/agenda_setter (organized/arbitrage) — control what counts as adequate engagement
 *   - lay_community_members: observer (powerless/constrained) — receive downstream legitimacy effects
 *   - performance_only_traditionalists: excluded (organized/constrained) — hold a live but institutionally marginalized rival reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__study_as_exercise_reading, 0.08).
domain_priors:suppression_score(sacrifice_obligation_kernel__study_as_exercise_reading, 0.22).
domain_priors:theater_ratio(sacrifice_obligation_kernel__study_as_exercise_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__study_as_exercise_reading, rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel__study_as_exercise_reading, "Study of Sacrifice Law as Fulfillment of the Mitzvah (Talmud Torah k'neged korban)").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__study_as_exercise_reading, "religious_law/halakhic_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__study_as_exercise_reading, 'db1bdcd4-222b-4208-a7a7-2cf334e2580c').
narrative_ontology:cs_kernel_codification('db1bdcd4-222b-4208-a7a7-2cf334e2580c', fixed_text).
narrative_ontology:cs_authority_grounding('db1bdcd4-222b-4208-a7a7-2cf334e2580c', lineage).
narrative_ontology:cs_interpretation_layer_present('db1bdcd4-222b-4208-a7a7-2cf334e2580c').
narrative_ontology:cs_reading_relation('db1bdcd4-222b-4208-a7a7-2cf334e2580c', sacrifice_obligation_kernel__performance_only_reading, forecloses).
narrative_ontology:cs_reading_relation('db1bdcd4-222b-4208-a7a7-2cf334e2580c', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('db1bdcd4-222b-4208-a7a7-2cf334e2580c', sacrifice_obligation_kernel__symbolic_archive_reading, forecloses).
narrative_ontology:cs_axiom('db1bdcd4-222b-4208-a7a7-2cf334e2580c', foundational, study_constitutes_full_present_discharge).
narrative_ontology:cs_axiom_status(study_constitutes_full_present_discharge, holdable).
narrative_ontology:cs_axiom_grounding('db1bdcd4-222b-4208-a7a7-2cf334e2580c', study_constitutes_full_present_discharge, conventional).
narrative_ontology:cs_axiom('db1bdcd4-222b-4208-a7a7-2cf334e2580c', secondary, obligation_remains_ontologically_live_under_current_conditions).
narrative_ontology:cs_axiom_status(obligation_remains_ontologically_live_under_current_conditions, holdable).
narrative_ontology:cs_axiom_grounding('db1bdcd4-222b-4208-a7a7-2cf334e2580c', obligation_remains_ontologically_live_under_current_conditions, deontological).
narrative_ontology:cs_reference_frame('db1bdcd4-222b-4208-a7a7-2cf334e2580c', temple_era_operative_sacrificial_practice).
narrative_ontology:cs_drift_state('db1bdcd4-222b-4208-a7a7-2cf334e2580c', post_destruction_rabbinic_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('db1bdcd4-222b-4208-a7a7-2cf334e2580c', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_academies).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, yeshiva_students).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, halakhic_decisors).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__study_as_exercise_reading, talmud_torah_keneged_kol_hamitzvot).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__study_as_exercise_reading, study_occupies_the_obligation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the yeshiva curriculum in which Seder Kodashim (the sacrificial order) occupies a central place of study despite having no operative Temple to apply it to. They certify that sustained analytic engagement with the sacrificial statutes constitutes fulfillment of the underlying commandment, which secures the institutional centrality of study itself as the primary religious activity and the primary credentialing pathway for rabbinic authority.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_academies, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_academies, beneficiary).

% Spend years analyzing tractates on sacrificial procedure that can never be physically performed under current conditions. Under this reading, that analytical labor is not preparatory or symbolic but the mitzvah itself, discharged in full through the act of study — which converts what could otherwise be experienced as an unfulfillable obligation into an actively dischargeable one, with real standing and communal esteem attached to depth of engagement.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, yeshiva_students, beneficiary,
    moderate, biographical, mobile, regional).

% Issue and transmit the doctrine (rooted in Talmudic and later authorities) that intellectual engagement with sacrificial law substitutes for performance. Their interpretive authority over WHAT COUNTS as adequate engagement — depth, method, lineage of transmission — is the actual site of ongoing control; the doctrine gives them a durable adjudicative role that persists independent of any Temple.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, halakhic_decisors, beneficiary,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__study_as_exercise_reading, halakhic_decisors, agenda_setter).

% Are not expected to personally discharge the obligation through deep study (that role is delegated to scholarly specialists) but benefit derivatively from the doctrine's claim that the tradition's obligations remain fully alive and dischargeable through study conducted on the community's behalf. They have no direct stake in adjudicating the reading and mostly receive its downstream religious-legitimacy effects.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, lay_community_members, observer,
    powerless, biographical, constrained, local).

% Hold that only actual sacrificial performance discharges the obligation and that study, however rigorous, remains preparatory. They are not silenced but are structurally marginal within institutions where the study-as-exercise doctrine underwrites the primary economic and status structure of the yeshiva world; their reading gets less institutional oxygen because it does not vindicate study as sufficient in itself.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, performance_only_traditionalists, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent, actionable religious practice for maintaining ongoing engagement with commandments whose physical performance is currently impossible (no Temple), preventing an entire body of law from becoming either dead letter or a source of unresolvable religious anxiety about unfulfillable duty.
% TRANSFER_FUNCTION: Converts what would otherwise be inert legal material into the basis for institutional status, curricular centrality, and interpretive authority — moving communal esteem, funding, and adjudicative standing toward those who study and teach Kodashim at depth, and toward the rabbinic bodies who certify that this study counts.
% ABSENT_VOICES: Performance-only traditionalists and those who hold the messianic-suspension view are present in the tradition's literature but marginalized within institutions organized around the study-as-fulfillment premise; a lay practitioner skeptical that reading Talmudic tractates 'counts' as anything comparable to a real sacrifice is rarely represented in the adjudicating bodies.
% DISAPPEARANCE_RATIONALE: If the study-as-exercise doctrine were withdrawn, the intensive curricular focus on Seder Kodashim would lose its normative justification (study of an unfulfillable law with no substitute status would revert to antiquarian or preparatory), the credentialing pathways built on mastery of these tractates would need new justification, and the psychological and communal resolution the doctrine provides for the suspended-Temple problem would have to be replaced by an alternative account (suspension or symbolic archive).
% FOUNDING_PROBLEM: After the Temple's destruction, the sacrificial commandments became physically impossible to perform, creating a body of divine law with no operative object and a live theological problem: is the obligation void, permanently pending, or dischargeable by another means?
% FOUNDING_PROBLEM_CORROBORATION: The doctrine is affirmed within its own tradition by classical sources (attributed to Talmudic dicta equating Torah study with sacrificial offering) and transmitted by the same rabbinic authorities who benefit from it. Outside corroboration is thin: historians of religion and adherents of the performance-only and messianic-suspension readings — including within Orthodox halakhic literature itself — treat the founding problem as still substantively open, arguing the study equivalence is a rabbinic innovation responding to loss rather than a resolution attested by any source outside the rabbinic tradition itself.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__study_as_exercise_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__study_as_exercise_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__study_as_exercise_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__study_as_exercise_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__study_as_exercise_reading, 0.08, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__study_as_exercise_reading_tests).
:- end_tests(sacrifice_obligation_kernel__study_as_exercise_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near-zero (0.08 at interval end) because under this reading nothing is taken from anyone who is worse off for it — no victim group is identifiable, and the doctrine converts an otherwise-frustrated obligation into a dischargeable one, which is a net gain for those it covers. Suppression is low-moderate (0.22): there is no coercive enforcement compelling anyone to accept the study-as-exercise premise, though social and institutional pressure within yeshiva culture does marginalize dissenting readings. Theater ratio stays low (0.15) because the study activity is substantively real, sustained, technically rigorous engagement — not hollow performance; the 'theater' concern would apply more to symbolic_archive_reading, a different constraint. Accessibility collapse is moderate (0.35): alternative readings (performance_only, messianic_suspension) remain visible and citable within the same textual tradition; they have not been erased, only institutionally outcompeted. Resistance is low (0.20), consistent with a doctrine that functions as genuine coordination rather than felt extraction — the traditionalists who dissent are a minority voice, not a suppressed victim class.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic academies and halakhic decisors sit near the beneficiary end: they set the terms of what counts as adequate study, and that interpretive monopoly is durable institutional capital independent of any Temple. Yeshiva students are also beneficiaries under this reading — their labor is reframed from preparatory/deferred into fully efficacious, which is a status upgrade, not an extraction. There is no stakeholder positioned as a payer/victim in this specific reading, which is the expected structural delta: this reading, unlike a snare or tangled_rope, has nothing extracted from anyone identifiable. Performance-only traditionalists experience marginalization (a reputational/institutional cost) but that is a cost of losing an interpretive contest, not extraction through this constraint's own operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (an entire commandment-category rendered physically unfulfillable by Temple destruction) is genuinely resolved by this reading's own logic — study substitutes cleanly and the mandate is not obviously outlived. But founding_problem_status is authored as 'contested' rather than 'live' or 'dead' because sibling readings dispute whether the substitution is real (study_as_exercise) or whether the obligation is instead merely suspended pending restoration (messianic_suspension) or archived without live legal force (symbolic_archive). This is precisely the committer structure that belongs in omegas, not resolved here: the mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges is the honest signal that the doctrine's function is real and load-bearing for its adherents even though its ultimate theological correctness remains disputed outside the benefiting institutions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_substitution_authenticity,
    'Is the equation of Torah study with sacrificial performance a genuine halakhic transformation of the obligation, or a rabbinic accommodation that reframes an unfulfillable duty as fulfilled for institutional and psychological reasons?',
    'No empirical resolution is possible within the tradition''s own terms; the question is adjudicated by which authorities and communities a given practitioner treats as binding. Comparative analysis of how the doctrine is invoked (as settled law vs. as pastoral consolation) across different rabbinic corpora could at least clarify how contested the reading actually is within the tradition, without resolving the underlying theological claim.',
    'If treated as genuine transformation, this constraint remains a clean rope: real coordination (resolving an unfulfillable-obligation crisis), beneficiary (rabbinic authority) without extraction. If treated as accommodation dressed as doctrine, the same structure could be reread as a mild false-summit pattern — a mountain-flavored claim (''this is simply what the mitzvah requires now'') that in fact serves the interpretive authority''s institutional interest. This is exactly the ambiguity FSM-style scrutiny is built to surface, though this story is authored as rope (not mountain) precisely because the constraint does not claim to be natural law — it claims interpretive derivation, which is a different structural posture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(study_substitution_authenticity, conceptual, 'Whether the study-fulfillment equivalence is a real transformation of the obligation or an institutionally convenient reframing.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the four sibling readings of sacrifice_obligation_kernel disagree — is it about the ONTOLOGICAL status of the obligation (void, suspended, transformed, archived) or about the EPISTEMIC question of what evidence would settle it?',
    'This is the committer-structure question the kernel framing exists to surface, not something this single reading resolves. Cross-reading analysis (comparing this story''s axioms against performance_only_reading, messianic_suspension_reading, and symbolic_archive_reading, once authored) would locate the precise structural element the readings diverge on.',
    'Determines whether the four readings are truly forecloses-incompatible (only one can be correct within a single legal framework) or coexists_with-compatible (different communities can hold different readings without direct legal contradiction, because they belong to different institutional jurisdictions). This story''s reading_relations declarations reflect a considered judgment on this, but the judgment is itself contestable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locating the precise structural disagreement among the kernel''s sibling readings.').

omega_variable(
    beneficiary_naturalness_ambiguity,
    'Rabbinic academies and decisors are declared as beneficiaries of a rope-classified constraint — does this beneficiary presence indicate incipient extraction (interpretive authority converting into rent-seeking over time) even though the current metrics show near-zero extractiveness?',
    'Longitudinal tracking of theater_ratio and base_extractiveness (already authored above, showing mild upward drift from 0.05/0.10 to 0.08/0.15 over 1900 years) — a continued rise would support a drift-toward-extraction hypothesis; a plateau supports genuine stable coordination.',
    'If extraction were found to be rising meaningfully, the constraint could migrate from rope toward tangled_rope as institutional capture of the interpretive function intensifies; at current authored values it remains squarely rope-shaped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_naturalness_ambiguity, empirical, 'Whether beneficiary concentration in rabbinic authority signals slow drift toward extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__study_as_exercise_reading, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sacr_tr_t300, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 300, 0.12).
narrative_ontology:measurement(sacr_tr_t700, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 700, 0.13).
narrative_ontology:measurement(sacr_tr_t1100, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 1100, 0.14).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 1500, 0.15).
narrative_ontology:measurement(sacr_tr_t1900, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 1900, 0.15).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(sacr_be_t300, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 300, 0.06).
narrative_ontology:measurement(sacr_be_t700, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 700, 0.07).
narrative_ontology:measurement(sacr_be_t1100, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 1100, 0.07).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 1500, 0.08).
narrative_ontology:measurement(sacr_be_t1900, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 1900, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_kernel__study_as_exercise_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__study_as_exercise_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_kernel__study_as_exercise_reading, 0.1).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% This story is one of four constraint-family members instantiating sacrifice_obligation_kernel, each a structurally distinct reading with its own ε per the ε-invariance principle. study_as_exercise_reading (this story) claims zero extraction and full present discharge of the obligation via study. performance_only_reading would claim the obligation remains unfulfilled (likely higher accessibility_collapse around the unfulfillability problem, and a live tension/anxiety dynamic). messianic_suspension_reading treats the obligation as paused rather than transformed (likely framed with beneficiaries being those maintaining readiness rather than those claiming fulfillment). symbolic_archive_reading denies live halakhic force altogether (likely lowest stakes of all four, closest to a rope/mountain-adjacent cultural-preservation function with minimal contested authority). The four are linked bidirectionally as siblings under the shared kernel; this file does not attempt to average or reconcile their ε values — each is authored independently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
