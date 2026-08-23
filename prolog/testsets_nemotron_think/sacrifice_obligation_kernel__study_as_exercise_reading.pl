% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__study_as_exercise_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: sacrifice_obligation_kernel__study_as_exercise_reading
 *   human_readable: Study as Fulfillment of Sacrifice Obligation (Halakhic Reading)
 *   domain: religious_law/halakhic_authority
 *
 * SUMMARY:
 *   This constraint story captures one reading of the
 *   sacrifice_obligation_kernel: the position that intellectual engagement
 *   with sacrificial law (study of Kodashim, Temple service, korbanot)
 *   constitutes genuine fulfillment of the biblical mitzvah under post-Temple
 *   conditions. The reading draws on sources like Hosea 14:3 ('ונשלמה פרים
 *   שפתינו' — 'we will render the bulls with our lips') and Talmudic
 *   statements (Menachot 110a, Ta'anit 27b) that equate study with sacrifice.
 *   The constraint coordinates a dispersed community around a portable
 *   practice, transferring legitimating authority from the Temple priesthood
 *   to the rabbinic interpretive tradition. The claimed type is 'rope' — pure
 *   coordination with minimal coercion — but the engine will compute per-seat
 *   classifications from the structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__study_as_exercise_reading, 0.12).
domain_priors:suppression_score(sacrifice_obligation_kernel__study_as_exercise_reading, 0.15).
domain_priors:theater_ratio(sacrifice_obligation_kernel__study_as_exercise_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__study_as_exercise_reading, rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel__study_as_exercise_reading, "Study as Fulfillment of Sacrifice Obligation (Halakhic Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__study_as_exercise_reading, "religious_law/halakhic_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__study_as_exercise_reading, '30b15fe7-6136-461e-b0cb-fa63cf168983').
narrative_ontology:cs_kernel_codification('30b15fe7-6136-461e-b0cb-fa63cf168983', fixed_text).
narrative_ontology:cs_authority_grounding('30b15fe7-6136-461e-b0cb-fa63cf168983', lineage).
narrative_ontology:cs_interpretation_layer_present('30b15fe7-6136-461e-b0cb-fa63cf168983').
narrative_ontology:cs_reading_relation('30b15fe7-6136-461e-b0cb-fa63cf168983', sacrifice_obligation_kernel__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('30b15fe7-6136-461e-b0cb-fa63cf168983', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('30b15fe7-6136-461e-b0cb-fa63cf168983', sacrifice_obligation_kernel__symbolic_archive_reading, coexists_with).
narrative_ontology:cs_axiom('30b15fe7-6136-461e-b0cb-fa63cf168983', foundational, study_fulfills_sacrifice_obligation).
narrative_ontology:cs_axiom_status(study_fulfills_sacrifice_obligation, holdable).
narrative_ontology:cs_axiom_grounding('30b15fe7-6136-461e-b0cb-fa63cf168983', study_fulfills_sacrifice_obligation, conventional).
narrative_ontology:cs_axiom('30b15fe7-6136-461e-b0cb-fa63cf168983', secondary, rabbinic_authority_defines_valid_study).
narrative_ontology:cs_axiom_status(rabbinic_authority_defines_valid_study, holdable).
narrative_ontology:cs_axiom_grounding('30b15fe7-6136-461e-b0cb-fa63cf168983', rabbinic_authority_defines_valid_study, conventional).
narrative_ontology:cs_reference_frame('30b15fe7-6136-461e-b0cb-fa63cf168983', temple_era_fulfillment).
narrative_ontology:cs_drift_state('30b15fe7-6136-461e-b0cb-fa63cf168983', post_temple_rabbinic_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('30b15fe7-6136-461e-b0cb-fa63cf168983', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, scholars_and_students).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__study_as_exercise_reading, torah_study_as_avodah).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__study_as_exercise_reading, halakhic_continuity_through_interpretation).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__study_as_exercise_reading, authorized_transformation_of_obligation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines the corpus of sacrificial law, determines what constitutes valid study, authorizes the interpretive framework that makes study fulfill the obligation. Maintains the chain of transmission from Temple-era practice to post-Temple study. Collects no direct material rents but holds interpretive monopoly over the obligation's fulfillment conditions.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_authority, agenda_setter,
    institutional, generational, analytical, global).

% Engage in study of sacrificial law (Kodashim tractates, Temple service codes) as their primary mode of fulfilling the mitzvah. Experience the study as genuine religious obligation-fulfillment, not mere preparation. Can enter or leave the interpretive community, but within it the study is the legitimate act.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, scholars_and_students, beneficiary,
    organized, biographical, mobile, global).

% Hold that only physical sacrifice on the Temple Mount fulfills the biblical obligation; study is valuable preparation but not fulfillment. Their position is marginalized in mainstream halakhic discourse since the Temple's destruction. They would object to the claim that study occupies the obligation.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, performance_only_adherents, excluded,
    organized, generational, constrained, global).

% Hold that the obligation is divinely suspended until messianic restoration; study maintains readiness but does not fulfill. Their reading coexists in liturgical and theological discourse but is excluded from the halakhic decision-framework that treats study as fulfillment.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, messianic_suspension_adherents, excluded,
    organized, generational, constrained, global).

% Academic, cultural, or secularly-identified Jews who engage sacrificial texts as historical archive and identity-preservation. Make no halakhic claim. Excluded from the halakhic conversation by definition, but their engagement shapes public understanding of the texts.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, symbolic_archive_adherents, excluded,
    moderate, biographical, arbitrage, global).

% Observes the constraint system from outside the commitment: sees the interpretive move that transforms a physical obligation into an intellectual one, notes the beneficiary structure (rabbinic authority defines the terms), and tracks how the reading relates to its siblings.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of maintaining a unified religious obligation across two millennia without its physical infrastructure (Temple, priesthood, altar). Study provides a portable, reproducible, text-anchored practice that every community can perform identically, preserving the obligation's continuity despite the loss of its material conditions.
% TRANSFER_FUNCTION: Moves interpretive authority from the Temple priesthood (who performed the physical acts) to the rabbinic sages (who define the intellectual acts). The transfer is not of material resources but of legitimating power: who decides what counts as fulfilling the mitzvah. The scholars and students receive the ability to fulfill the obligation through accessible means; the rabbinic authority retains the monopoly on defining those means.
% ABSENT_VOICES: The performance-only adherents (who would insist the obligation remains physical and unfulfilled), the messianic-suspension adherents (who would insist the obligation is paused not transformed), and the symbolic-archive adherents (who would deny any halakhic force). All three are structurally excluded from the halakhic decision-framework that treats study as fulfillment, though they exist in the broader discourse.
% DISAPPEARANCE_RATIONALE: If the study-as-fulfillment reading vanished overnight, the halakhic system would lose its primary mechanism for occupying the sacrifice obligation in the Temple's absence. Communities would either adopt performance-only positions (requiring Temple rebuilding), messianic-suspension positions (passive waiting), or symbolic-archive positions (cultural preservation without obligation). The religious practice of millions would reorganize around a different account of what the obligation demands.
% FOUNDING_PROBLEM: After the Temple's destruction (70 CE), the biblical obligation to offer sacrifices became physically impossible to perform. The founding problem was how to maintain the obligation's force and the community's covenantal relationship with it without its material infrastructure.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested by the historical record (Josephus, Talmudic sources documenting the crisis) and by the continued existence of the obligation in liturgy and law. The status 'live' is corroborated by the ongoing centrality of Temple-restoration prayers and the fact that no halakhic authority has declared the sacrifice obligation abrogated — only transformed. The transformation itself is the rabbinic tradition's answer to the live problem.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__study_as_exercise_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__study_as_exercise_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__study_as_exercise_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__study_as_exercise_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__study_as_exercise_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low (0.12) because the reading presents study as authorized transformation, not extraction; no material transfer occurs, and participants experience the study as genuine fulfillment. The slight non-zero value reflects the interpretive monopoly: rabbinic authority defines what counts as valid study, which is a structural advantage. Suppression is low (0.15) because sibling readings coexist in the broader discourse; they are excluded from the halakhic decision-framework but not actively silenced. Theater ratio is very low (0.08) — the study practice is substantive, not performative. Accessibility collapse is moderate (0.35) — alternatives exist but the halakhic framework makes them non-viable for committed participants. Resistance is low (0.18) — this is an accepted mainstream position.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic authority seat, the constraint is genuine coordination solving an existential continuity problem. From the performance-only seat, the same structure looks like an unauthorized substitution that empties the obligation of its physical content. From the analytical seat, the interpretive monopoly is visible as a structural benefit to the authority that defines the terms. The engine computes these divergences from the declared power, exit_options, and spatial_scope values.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority sits near the beneficiary end (d ~ 0.15): they hold the interpretive monopoly that defines the constraint's terms, but they do not extract material resources. Scholars and students sit near symmetric (d ~ 0.5): they both contribute to and benefit from the coordination. Excluded seats (performance-only, messianic-suspension, symbolic-archive) are not coordinated by this constraint — their exclusion is structural, not extractive. The analytical observer sees the full structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (physical impossibility of sacrifice) remains live — the Temple has not been rebuilt. The arrangement has not outlived its function; it continues to occupy the obligation for communities that accept the rabbinic framework. No mandatrophy is resolved here. The constraint would become a piton only if the Temple were rebuilt and the study-reading persisted as the primary mode despite physical performance being possible again.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'How does this reading''s claim of zero extractiveness interact with the rabbinic authority''s interpretive monopoly over what counts as valid fulfillment?',
    'Compare the study_as_exercise_reading''s beneficiary structure against its siblings: if the rabbinic authority''s monopoly is exercised to exclude competing fulfillments (e.g., Temple Mount prayer groups, alternative study frameworks), the monopoly functions as extraction even if the reading claims authorized transformation.',
    'If the interpretive monopoly operates as exclusionary control rather than coordination, the constraint''s effective extraction for the agenda_setter seat rises, potentially reclassifying from rope to tangled_rope for that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Whether the rabbinic authority''s interpretive monopoly constitutes extraction or pure coordination.').

omega_variable(
    authorized_transformation_vs_institutional_interest,
    'Is the ''authorized transformation'' framing a genuine halakhic innovation or a cover for institutional preservation of authority?',
    'Historical analysis of early rabbinic sources: did the study-as-fulfillment doctrine emerge from exegetical necessity (textual evidence) or from institutional need (preserving rabbinic relevance after Temple loss)? Track whether the doctrine''s boundaries expand when rabbinic authority is challenged.',
    'If institutional interest drives the doctrine''s scope, the constraint carries hidden extraction; if exegetically constrained, it remains low-extraction coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authorized_transformation_vs_institutional_interest, conceptual, 'Whether the transformation doctrine serves textual fidelity or institutional survival.').

omega_variable(
    suppression_mechanism_exclusion,
    'Is the exclusion of sibling readings from the halakhic decision-framework structural (institutional gatekeeping) or internalized (communities self-select into the framework)?',
    'Survey communities that hold performance-only or messianic-suspension positions: do they experience active exclusion from halakhic discourse, or do they voluntarily remain outside the rabbinic framework?',
    'If exclusion is structural, suppression is higher than measured; if internalized, the constraint''s effective suppression includes the communities'' own boundary-maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_exclusion, empirical, 'Whether sibling-reading exclusion is enforced or chosen.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__study_as_exercise_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 500, 0.06).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 1000, 0.07).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 1500, 0.075).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 2000, 0.08).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 500, 0.08).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 1000, 0.1).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 1500, 0.11).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 2000, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(sacr_su_t500, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 500, 0.12).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 1000, 0.14).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 1500, 0.145).
narrative_ontology:measurement(sacr_su_t2000, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 2000, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__study_as_exercise_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_kernel__study_as_exercise_reading, 0.08).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__symbolic_archive_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, halakhic_authority_structure).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, torah_study_obligation).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the sacrifice_obligation_kernel. The kernel is the biblical obligation to offer sacrifices (Leviticus 1-7, Numbers 28-29). Each reading instantiates a different constraint with different beneficiary/victim structures and different extractiveness. This reading claims zero extractiveness via authorized transformation; performance_only_reading claims the obligation remains physical (higher suppression, different beneficiary structure); messianic_suspension_reading claims divine pause (different time_horizon, different authority_grounding); symbolic_archive_reading denies halakhic force entirely (different kernel_codification).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_obligation_kernel__study_as_exercise_reading, institutional, 0.15).
constraint_indexing:directionality_override(sacrifice_obligation_kernel__study_as_exercise_reading, organized, 0.48).
constraint_indexing:directionality_override(sacrifice_obligation_kernel__study_as_exercise_reading, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
