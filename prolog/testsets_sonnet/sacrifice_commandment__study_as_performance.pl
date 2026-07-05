% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__study_as_performance, []).

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
 *   constraint_id: sacrifice_commandment__study_as_performance
 *   human_readable: Study of Sacrificial Law as Fulfillment of the Commandment (Talmud Torah ke-neged Korbanot Reading)
 *   domain: religious_studies/halakhic_theory
 *
 * SUMMARY:
 *   This story instantiates one reading — study_as_performance — of the
 *   contested kernel around the sacrifice commandment's status after the
 *   Temple's destruction. The natural-language label 'the commandment to
 *   offer sacrifices, post-Temple' covers at least three structurally
 *   distinct claims: that the commandment is suspended pending physical
 *   restoration (performance_only), that study merely preserves technical
 *   knowledge for a future restoration (archive_maintenance), and that study
 *   itself IS the exercise of the commandment, fulfilling it intellectually
 *   (study_as_performance — this story). These are not the same constraint
 *   viewed three ways; each has a different beneficiary structure, a
 *   different relationship to the founding problem's live/dead status, and
 *   would be measured with a different epsilon. This file addresses ONLY the
 *   study_as_performance reading, cleanly, per the epsilon-invariance
 *   principle. The sibling readings are separate constraint files, linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - scholar_worshippers: beneficiary (moderate/mobile) — derive devotional fulfillment from study itself
 *   - yeshiva_institutions: beneficiary/agenda_setter (organized/mobile) — structure curricula on the doctrine's premise
 *   - post_temple_jewish_communities: beneficiary (organized/mobile) — inherit a live, executable path to fulfillment
 *   - adherents_of_performance_only_reading: excluded — hold a competing, sidelined position
 *   - halakhic_authorities: observer (institutional/analytical) — administer and adjudicate the interpretive tradition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__study_as_performance, 0.03).
domain_priors:suppression_score(sacrifice_commandment__study_as_performance, 0.05).
domain_priors:theater_ratio(sacrifice_commandment__study_as_performance, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, extractiveness, 0.03).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__study_as_performance, rope).
narrative_ontology:human_readable(sacrifice_commandment__study_as_performance, "Study of Sacrificial Law as Fulfillment of the Commandment (Talmud Torah ke-neged Korbanot Reading)").
narrative_ontology:topic_domain(sacrifice_commandment__study_as_performance, "religious_studies/halakhic_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__study_as_performance, 'fceae059-289c-47dd-b87a-f0a458caf751').
narrative_ontology:cs_kernel_codification('fceae059-289c-47dd-b87a-f0a458caf751', fixed_text).
narrative_ontology:cs_authority_grounding('fceae059-289c-47dd-b87a-f0a458caf751', lineage).
narrative_ontology:cs_interpretation_layer_present('fceae059-289c-47dd-b87a-f0a458caf751').
narrative_ontology:cs_reading_relation('fceae059-289c-47dd-b87a-f0a458caf751', sacrifice_commandment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('fceae059-289c-47dd-b87a-f0a458caf751', sacrifice_commandment__archive_maintenance, influences).
narrative_ontology:cs_axiom('fceae059-289c-47dd-b87a-f0a458caf751', foundational, intellectual_engagement_constitutes_divine_service).
narrative_ontology:cs_axiom_status(intellectual_engagement_constitutes_divine_service, holdable).
narrative_ontology:cs_axiom_grounding('fceae059-289c-47dd-b87a-f0a458caf751', intellectual_engagement_constitutes_divine_service, theological).
narrative_ontology:cs_axiom('fceae059-289c-47dd-b87a-f0a458caf751', secondary, commandment_fulfillment_does_not_require_physical_execution).
narrative_ontology:cs_axiom_status(commandment_fulfillment_does_not_require_physical_execution, holdable).
narrative_ontology:cs_axiom_grounding('fceae059-289c-47dd-b87a-f0a458caf751', commandment_fulfillment_does_not_require_physical_execution, theological).
narrative_ontology:cs_reference_frame('fceae059-289c-47dd-b87a-f0a458caf751', temple_era_sacrificial_practice).
narrative_ontology:cs_drift_state('fceae059-289c-47dd-b87a-f0a458caf751', post_destruction_rabbinic_consolidation, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('fceae059-289c-47dd-b87a-f0a458caf751', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__study_as_performance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__study_as_performance, scholar_worshippers).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__study_as_performance, yeshiva_institutions).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__study_as_performance, post_temple_jewish_communities).
narrative_ontology:constraint_vindicates(sacrifice_commandment__study_as_performance, torah_study_equivalence_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_commandment__study_as_performance, divine_service_through_intellect_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who study the laws of Temple sacrifice (Seder Kodashim, Mishnah Zevachim/Menachot) as an act of worship in itself. They report that the intellectual engagement produces the same devotional fulfillment the physical rite once produced, sourced from talmudic statements (e.g. Menachot 110a) equating recitation/study of sacrificial law with the offering. They are free to study any tractate; nothing coerces them into this specific practice beyond communal custom and personal devotion.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, scholar_worshippers, beneficiary,
    moderate, biographical, mobile, global).

% Academies that structure curricula around Seder Kodashim on the premise that study constitutes fulfillment of the commandment. They set which tractates carry devotional weight and transmit the interpretive tradition to students. They benefit reputationally and institutionally from a doctrine that makes their core activity (textual study) equivalent to Temple service, but the doctrine long predates any single modern institution and is not their invention.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, yeshiva_institutions, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_commandment__study_as_performance, yeshiva_institutions, agenda_setter).

% Communities without access to the physical Temple who inherit a religious obligation that has no available physical mode of execution. The study-as-performance reading gives them a live, currently-executable way to be understood as fulfilling the commandment rather than living under an unfulfillable one. No one is coerced into believing the doctrine; it is transmitted through voluntary religious education and liturgy.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, post_temple_jewish_communities, beneficiary,
    organized, civilizational, mobile, global).

% Those who hold that the commandment requires physical execution and is presently suspended, not fulfilled. They are not victims of this reading — no material transfer runs against them — but their position is structurally sidelined wherever the study-as-performance doctrine becomes the operative communal norm, since it renders their 'suspended, not fulfilled' framing devotionally deflationary by comparison.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, adherents_of_performance_only_reading, excluded,
    moderate, generational, mobile, global).

% Rabbinic decisors and textual authorities (from talmudic sages through later commentators) who articulate and transmit the study-as-performance doctrine, adjudicate its scope, and situate it relative to competing readings. They neither collect material rents from the doctrine nor bear its costs; they administer the interpretive tradition.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, halakhic_authorities, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides post-Temple Jewish communities a currently-executable devotional practice that satisfies a commandment whose original physical mode (animal sacrifice at the Temple) has been structurally unavailable for nearly two millennia — coordinating collective religious continuity around study rather than leaving the obligation permanently unfulfillable.
% TRANSFER_FUNCTION: No material transfer occurs. The doctrine reallocates devotional/spiritual credit: intellectual engagement with sacrificial law is credited as equivalent religious performance, without moving money, labor, or physical goods from any party to another.
% ABSENT_VOICES: Adherents of the performance_only reading are structurally present in the discourse (this is an internal halakhic dispute, not an external exclusion) but are sidelined in communities where the study-as-performance doctrine becomes dominant practice, since their framing implies an unfulfilled obligation that the dominant reading claims to resolve.
% DISAPPEARANCE_RATIONALE: If the doctrine that study constitutes performance vanished, post-Temple communities would be left holding a commandment with no available mode of fulfillment at all — yeshiva curricula built around Seder Kodashim would lose their devotional justification (though not necessarily their pedagogical one), and religious life would need to reorganize around either the performance_only suspension framing or the archive_maintenance framing, both of which carry different psychological and communal consequences.
% FOUNDING_PROBLEM: The destruction of the Second Temple (70 CE) eliminated the only sanctioned site and mechanism for offering sacrifices, leaving a scripturally mandated commandment with no physical means of execution and no fixed date for restoration.
% FOUNDING_PROBLEM_CORROBORATION: The absence of the Temple is an uncontested historical and archaeological fact attested by Roman historical sources (Josephus, Tacitus) entirely outside the Jewish interpretive tradition, and by the continuous absence of a rebuilt Temple to the present day — it is not a claim that depends on any party benefiting from the study-as-performance doctrine.
narrative_ontology:disappearance_verdict(sacrifice_commandment__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__study_as_performance, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_commandment__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__study_as_performance, 0.03, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__study_as_performance_tests).
:- end_tests(sacrifice_commandment__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near zero (0.03) because no material transfer runs from any party to another under this reading — the entire content of the arrangement is a devotional/intellectual claim, not an allocation mechanism. Suppression is low (0.05) because no party is coerced into holding this reading; it competes openly with performance_only and archive_maintenance within halakhic discourse, and adherents move freely between communities that emphasize different readings. Theater ratio is low (0.08): the study itself is the substantive activity, not a performance standing in for some other suppressed function — there is no atrophied 'real' function this doctrine masks, since study is asserted (by its own terms) to BE the function. Accessibility collapse is low-moderate (0.15): alternative theological framings (performance_only, archive_maintenance) remain fully articulable and are in fact actively held by other communities — the study-as-performance reading has not collapsed its alternatives, it coexists with them. Resistance is low (0.1): there is genuine internal debate but no active suppression campaign against dissenting readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (scholar_worshippers, yeshiva_institutions, post_temple_jewish_communities) sit near the full-beneficiary end of directionality because the doctrine supplies them with something they did not have without it: a currently-executable path to religious fulfillment. There is no victim group under this reading because no cost is imposed on any party — the reading redistributes devotional standing (from the suspended-physical-rite framing to the study framing) without extracting resources, labor, or standing from anyone. This is the structural core of the expected delta: zero extractiveness, no victim set, beneficiary is the scholar-worshipper.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Temple destruction, no site for sacrifice) is corroborated as live by evidence entirely outside the doctrine's beneficiaries (Roman historiography, continuous absence of a rebuilt Temple). Because the founding problem remains live and the arrangement (study as devotional substitute) directly and currently addresses it without requiring anyone's continued belief to extract value from anyone else, this reading does not exhibit mandatrophy — it is not an arrangement that outlived its function; if anything it is a function invented specifically because the original function became permanently unavailable. Compare to a genuine mandatrophy case: an arrangement whose founding problem is dead but which persists by capturing rents from a still-obligated population. That is absent here because no rents are collected.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_as_performance_vs_archive_maintenance_boundary,
    'Is the devotional weight scholars attach to studying sacrificial law genuinely equivalent to worship (study_as_performance), or is it functionally indistinguishable from technical preparation for a hoped-for future restoration (archive_maintenance) dressed in devotional language?',
    'Compare how practitioners themselves describe the phenomenology of the practice across communities — first-person accounts, liturgical framing, and whether study of OTHER non-sacrificial legal material receives the same devotional credit (if yes, the equivalence claim is doctrine-general rather than sacrifice-specific, weakening the study_as_performance reading''s distinctiveness).',
    'If the practice is better described as archive_maintenance, this constraint''s beneficiary structure changes: the benefit shifts from ''present fulfillment'' to ''preparedness insurance,'' and the doctrine''s claim to satisfy a LIVE obligation weakens, since preparedness for a future event does not fulfill a present one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_as_performance_vs_archive_maintenance_boundary, conceptual, 'Whether study genuinely constitutes present worship or functions as future-oriented technical preservation.').

omega_variable(
    doctrinal_naturalness_vs_institutional_convenience,
    'Is the study-as-performance doctrine a genuine theological insight independently arrived at, or does it persist partly because it conveniently validates the yeshiva/text-study-centered model of Jewish religious life that emerged after the Temple''s destruction?',
    'Trace the textual history of the doctrine''s earliest articulations (e.g., Talmud Menachot 110a, later codifiers) relative to the institutional consolidation of rabbinic academies as the center of religious authority; assess whether the doctrine predates or postdates the institutional interest it now serves.',
    'If the doctrine substantially postdates and served the interests of an emerging rabbinic/academic class, this would support treating yeshiva_institutions as a more concentrated beneficiary than the diffuse scholar_worshipper population, though it would still not establish a victim class or material extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrinal_naturalness_vs_institutional_convenience, conceptual, 'Whether the doctrine''s origin is independent of the institutional interests it currently supports.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__study_as_performance, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__study_as_performance, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(sacr_tr_t0, observed).
narrative_ontology:measurement(sacr_tr_t400, sacrifice_commandment__study_as_performance, theater_ratio, 400, 0.06).
narrative_ontology:measurement_basis(sacr_tr_t400, observed).
narrative_ontology:measurement(sacr_tr_t800, sacrifice_commandment__study_as_performance, theater_ratio, 800, 0.07).
narrative_ontology:measurement_basis(sacr_tr_t800, observed).
narrative_ontology:measurement(sacr_tr_t1200, sacrifice_commandment__study_as_performance, theater_ratio, 1200, 0.07).
narrative_ontology:measurement_basis(sacr_tr_t1200, observed).
narrative_ontology:measurement(sacr_tr_t1600, sacrifice_commandment__study_as_performance, theater_ratio, 1600, 0.08).
narrative_ontology:measurement_basis(sacr_tr_t1600, observed).
narrative_ontology:measurement(sacr_tr_t1950, sacrifice_commandment__study_as_performance, theater_ratio, 1950, 0.08).
narrative_ontology:measurement_basis(sacr_tr_t1950, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__study_as_performance, base_extractiveness, 0, 0.02).
narrative_ontology:measurement_basis(sacr_be_t0, observed).
narrative_ontology:measurement(sacr_be_t400, sacrifice_commandment__study_as_performance, base_extractiveness, 400, 0.02).
narrative_ontology:measurement_basis(sacr_be_t400, observed).
narrative_ontology:measurement(sacr_be_t800, sacrifice_commandment__study_as_performance, base_extractiveness, 800, 0.03).
narrative_ontology:measurement_basis(sacr_be_t800, observed).
narrative_ontology:measurement(sacr_be_t1200, sacrifice_commandment__study_as_performance, base_extractiveness, 1200, 0.03).
narrative_ontology:measurement_basis(sacr_be_t1200, observed).
narrative_ontology:measurement(sacr_be_t1600, sacrifice_commandment__study_as_performance, base_extractiveness, 1600, 0.03).
narrative_ontology:measurement_basis(sacr_be_t1600, observed).
narrative_ontology:measurement(sacr_be_t1950, sacrifice_commandment__study_as_performance, base_extractiveness, 1950, 0.03).
narrative_ontology:measurement_basis(sacr_be_t1950, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_commandment__study_as_performance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__study_as_performance, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_commandment__study_as_performance, 0.08).
narrative_ontology:affects_constraint(sacrifice_commandment__study_as_performance, sacrifice_commandment__performance_only).
narrative_ontology:affects_constraint(sacrifice_commandment__study_as_performance, sacrifice_commandment__archive_maintenance).

% DUAL FORMULATION NOTE:
% Part of the sacrifice_commandment kernel family (3 stories). performance_only holds the commandment is suspended, not fulfilled, absent physical execution — structurally in tension with this reading's claim of present fulfillment (coexists_with: both are live positions held by different communities, neither has foreclosed the other within the broader tradition). archive_maintenance treats study as future-oriented technical preparation rather than present worship; this reading exerts influences pressure on it by offering a competing account of what the SAME activity (studying Seder Kodashim) accomplishes devotionally, without foreclosing it, since a given scholar's actual motivational state can plausibly be read either way. Each story carries its own epsilon: this reading is authored near-zero extraction (0.03) with beneficiaries and no victims; performance_only and archive_maintenance are expected to carry different beneficiary/victim structures and should be authored independently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
