% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__study_as_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: temple_sacrifice_obligation__study_as_occupation
 *   human_readable: Study of Sacrifice Law as Covenantal Occupation in Temple's Absence
 *   domain: religious/historical
 *
 * SUMMARY:
 *   In rabbinic Judaism following the destruction of the Second Temple, the
 *   biblical commandment of animal sacrifice lacks a physical site for
 *   performance. This constraint instantiates the reading that Torah study of
 *   sacrificial law constitutes a legitimate 'occupation' (kiyum) of the
 *   obligation, preserving covenantal continuity without requiring the
 *   Temple. It is one reading of the contested kernel
 *   temple_sacrifice_obligation; siblings study_as_archiving and
 *   messianic_suspension treat the same textual kernel differently. The
 *   authority structure (rabbinic halakha) absorbs the impossibility of
 *   performance by routing the commandment through the interpretive
 *   tradition, avoiding both the collapse of biblical authority and the
 *   admission of suspended obligation. Low extractiveness is expected because
 *   study is distributed, non-monopolistic, and framed as beneficiary
 *   fulfillment rather than target extraction.
 *
 * KEY AGENTS:
 *   - rabbinic_authority (agenda_setter/institutional/identity_locked): Administers the interpretive framework equating study with sacrifice; benefits from systemic stability.
 *   - torah_study_community (beneficiary/moderate/identity_locked): Performs the study that constitutes fulfillment; gains religious standing.
 *   - diaspora_community_laity (beneficiary/moderate/constrained): Benefits from covenantal continuity without being in a state of suspended obligation.
 *   - kohanic_lineage (excluded/moderate/identity_locked): Hereditary priests displaced by the shift from altar to study hall.
 *   - messianic_restorationists (excluded/moderate/constrained): Argue for suspension or immediate Temple rebuilding; structurally marginalized by this reading.
 *   - critical_historical_scholars (observer/analytical/analytical): External analytical seat examining the doctrine's historical development.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_occupation, 0.15).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_occupation, 0.08).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_occupation, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, extractiveness, 0.15).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_occupation, rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_occupation, "Study of Sacrifice Law as Covenantal Occupation in Temple's Absence").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_occupation, "religious/historical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_occupation, 'ba247bce-68d9-4b71-accd-5cbe3f55c796').
narrative_ontology:cs_kernel_codification('ba247bce-68d9-4b71-accd-5cbe3f55c796', fixed_text).
narrative_ontology:cs_authority_grounding('ba247bce-68d9-4b71-accd-5cbe3f55c796', lineage).
narrative_ontology:cs_interpretation_layer_present('ba247bce-68d9-4b71-accd-5cbe3f55c796').
narrative_ontology:cs_reading_relation('ba247bce-68d9-4b71-accd-5cbe3f55c796', temple_sacrifice_obligation__study_as_archiving, coexists_with).
narrative_ontology:cs_reading_relation('ba247bce-68d9-4b71-accd-5cbe3f55c796', temple_sacrifice_obligation__messianic_suspension, coexists_with).
narrative_ontology:cs_axiom('ba247bce-68d9-4b71-accd-5cbe3f55c796', foundational, study_occupies_sacrificial_commandment).
narrative_ontology:cs_axiom_status(study_occupies_sacrificial_commandment, holdable).
narrative_ontology:cs_axiom_grounding('ba247bce-68d9-4b71-accd-5cbe3f55c796', study_occupies_sacrificial_commandment, conventional).
narrative_ontology:cs_axiom('ba247bce-68d9-4b71-accd-5cbe3f55c796', foundational, commandment_not_suspended_in_exile).
narrative_ontology:cs_axiom_status(commandment_not_suspended_in_exile, holdable).
narrative_ontology:cs_axiom_grounding('ba247bce-68d9-4b71-accd-5cbe3f55c796', commandment_not_suspended_in_exile, deontological).
narrative_ontology:cs_reference_frame('ba247bce-68d9-4b71-accd-5cbe3f55c796', sacrificial_obligation_active).
narrative_ontology:cs_drift_state('ba247bce-68d9-4b71-accd-5cbe3f55c796', post_second_temple_destruction, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('ba247bce-68d9-4b71-accd-5cbe3f55c796', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, rabbinic_authority).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, torah_study_community).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, diaspora_community_laity).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, rabbinic_interpretive_authority).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, oral_torah_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains halakhic continuity by ruling that study of sacrifice law occupies the biblical obligation; absorbs the structural impossibility of Temple worship without declaring the commandments suspended or obsolete; authority derives from interpretive lineage and the stability of the halakhic system.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, rabbinic_authority, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_obligation__study_as_occupation, rabbinic_authority, beneficiary).

% Engages in daily study of Torah including sacrificial tractates; under this constraint, that study counts as active fulfillment of the commandment rather than mere historical curiosity; gains religious meaning and standing from the equivalence.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, torah_study_community, beneficiary,
    moderate, biographical, identity_locked, national).

% Lives under a framework where the covenant remains fully operative despite the Temple's absence; participates in the study norm indirectly through communal support and liturgical references; benefits from not being in a state of suspended religious obligation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, diaspora_community_laity, beneficiary,
    moderate, biographical, constrained, national).

% Hereditary priests whose sacrificial role is rendered latent by the Temple's absence; they retain ritual status and blessings but the core function is displaced by study; not the administrators or primary beneficiaries of the current arrangement.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, kohanic_lineage, excluded,
    moderate, generational, identity_locked, national).

% Hold that the obligation is either suspended pending restoration or requires active political effort to rebuild the Temple; their position is structurally marginalized by a framework that treats study as sufficient fulfillment.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, messianic_restorationists, excluded,
    moderate, civilizational, constrained, global).

% Analyze the development of the study-as-occupation doctrine as a post-destruction rabbinic innovation; view it as a successful mechanism of religious preservation and authority stabilization; do not participate in the halakhic system as insiders.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, critical_historical_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains collective religious continuity and covenantal legitimacy for a dispersed community that lacks the central sacrificial infrastructure; solves the problem of how to keep Temple-era commandments operative and meaningful after the destruction.
% TRANSFER_FUNCTION: Moves the locus of religious obligation-fulfillment from physical sacrificial performance in Jerusalem to intellectual study of sacrificial law in dispersed academies and study halls.
% ABSENT_VOICES: Kohanim whose hereditary function is displaced; messianic restorationists who argue for suspension or immediate rebuilding; secular and critical scholars who treat the obligation as historically contingent rather than metaphysically continuous.
% DISAPPEARANCE_RATIONALE: If the study-as-occupation framework vanished, the sacrificial commandments would stand as unfulfilled obligations for a community without a Temple, forcing either a theological crisis, a shift to messianic suspension, or a radical restructuring of rabbinic authority's relationship to biblical law.
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE eliminated the physical and priestly infrastructure for biblical sacrificial worship, leaving a set of explicit commandments without a mechanism for performance.
% FOUNDING_PROBLEM_CORROBORATION: Roman and archaeological corroboration of the Temple's destruction is external and uncontested. The rabbinic tradition attests the problem and the study solution internally through the Talmud and subsequent responsa. However, Karaite Jewish movements and some modern historians dispute that study constitutes fulfillment, arguing instead for suspension or historical obsolescence; no fully external, non-interested corroboration exists for the study-as-fulfillment claim.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__study_as_occupation, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__study_as_occupation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__study_as_occupation, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__study_as_occupation, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__study_as_occupation, 0.15, 'kimi-k2.6', 'none', direct).

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
 *   Metrics are authored low across the board: extractiveness 0.15 because no agent is systematically deprived of resources or autonomy; the 'cost' is study effort, which is simultaneously framed as religious benefit. Suppression 0.08 because there is no coercive machinery enforcing study; alternatives (non-study, conversion, secularization) are not actively suppressed, though social and theological costs exist. Theater ratio 0.12 because the study is functionally central to the tradition, not performative maintenance. Accessibility collapse 0.65 reflects that once inside the rabbinic frame, the study-as-fulfillment reading appears necessary and alternatives collapse, but from outside the frame alternatives are visible. Resistance 0.15 is low because the reading is broadly accepted within Orthodox Judaism, with only marginal movements contesting it.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic authority seat and the study community seat, this constraint is experienced as genuine coordination preserving a covenant. From the excluded kohanic and messianic restorationist seats, the same structure appears as a displacement of their roles and theological preferences. The engine computes this divergence from the structural data: the agenda-setter and beneficiaries have identity_locked or constrained exit but receive coordination benefits, while excluded seats have no voice and bear latent role displacement.
 *
 * DIRECTIONALITY LOGIC:
 *   All declared beneficiaries (rabbinic_authority, torah_study_community, diaspora_community_laity) are positioned at low directionality: the constraint subsidizes their religious continuity and identity. No victims are declared, so no high-d targets are structurally derived. Excluded seats (kohanim, restorationists) are not in the beneficiary/victim arrays, so they do not drive extraction metrics; their exclusion is captured in the absent_voices and stakeholder role fields.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as rope rather than piton or snare is supported by the absence of concentrated extraction, the absence of performative maintenance (low theater_ratio), and the live founding problem (Temple still absent). If the Temple were rebuilt and this constraint persisted despite the return of sacrificial possibility, it would drift toward piton or snare territory; currently it is structurally justified by the genuine coordination problem of maintaining covenant without a Temple.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Is this constraint one reading of a contested kernel, and how would sibling readings alter its classification?',
    'Comparison with compiled sibling stories temple_sacrifice_obligation__study_as_archiving and temple_sacrifice_obligation__messianic_suspension to see if they share structural properties or diverge in epsilon and victimhood.',
    'If sibling readings show higher extraction or victim sets, this reading''s low extraction is confirmed as structurally specific; if they are similar, the kernel may be mis-decomposed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Structural position of this reading within the contested kernel').

omega_variable(
    authority_benefit_nature,
    'Does the rabbinic authority''s gain from this constraint constitute coordination surplus (maintaining community) or extractive rent (authority preservation)?',
    'Historical analysis of whether authority would resist Temple restoration to maintain the study-based system; sociological study of power distribution.',
    'If authority actively resists restoration or suppresses alternative readings, classification shifts toward tangled_rope or snare; if authority is indifferent, rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authority_benefit_nature, empirical, 'Whether authority benefit is coordination or extraction').

omega_variable(
    study_sacrifice_equivalence_grounding,
    'Is the equivalence of study and sacrifice a theological truth, a conventional legal fiction, or a sociological functional equivalence?',
    'Textual analysis of Talmudic passages and comparative religion studies on ritual substitution.',
    'A purely conventional grounding suggests the constraint is a scaffold or rope; a claimed theological grounding might suggest mountain-like treatment within the tradition, affecting accessibility_collapse metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_sacrifice_equivalence_grounding, conceptual, 'Epistemic grounding of the study-sacrifice equivalence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_occupation, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0, 0.05).
narrative_ontology:measurement(temp_tr_t25, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 25, 0.06).
narrative_ontology:measurement(temp_tr_t50, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 50, 0.08).
narrative_ontology:measurement(temp_tr_t75, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 75, 0.1).
narrative_ontology:measurement(temp_tr_t100, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 100, 0.12).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(temp_be_t25, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 25, 0.11).
narrative_ontology:measurement(temp_be_t50, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(temp_be_t75, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 75, 0.13).
narrative_ontology:measurement(temp_be_t100, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(temp_su_t25, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 25, 0.05).
narrative_ontology:measurement(temp_su_t50, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 50, 0.05).
narrative_ontology:measurement(temp_su_t75, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 75, 0.06).
narrative_ontology:measurement(temp_su_t100, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation__study_as_archiving).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation__messianic_suspension).

% DUAL FORMULATION NOTE:
% The natural-language concept 'temple sacrifice obligation' decomposes into three structurally distinct constraints: study_as_occupation (this file, low extraction, study fulfills), study_as_archiving (study preserves only), and messianic_suspension (obligation suspended). Each has a different epsilon, beneficiary/victim structure, and classification. They form a constraint family linked by network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
