% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__study_as_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: temple_sacrifice_obligation__study_as_occupation
 *   human_readable: Temple Sacrifice Obligation: Study as Occupation
 *   domain: religious/halakhic/commitment_system
 *
 * SUMMARY:
 *   This constraint represents a dominant halakhic (Jewish legal)
 *   interpretation that, in the absence of the Temple, the divine obligation
 *   for animal sacrifices is fulfilled through the diligent study of the laws
 *   pertaining to those sacrifices. It provides a mechanism for religious
 *   continuity and meaning, transforming an impossible physical commandment
 *   into an accessible intellectual and spiritual one. This reading is a
 *   'Rope' because it solves a genuine collective-action problem (how to
 *   fulfill a central commandment when the means are unavailable) with
 *   minimal extraction, as study is considered a direct fulfillment, not a
 *   substitute.
 *
 * KEY AGENTS:
 *   - Halakhic scholars: Primary agenda-setters and beneficiaries, defining and transmitting this interpretation.
 *   - Observant Jews: Beneficiaries who fulfill their religious obligations through study, deeply identity-locked into this framework.
 *   - Messianic suspension adherents: Excluded voices who hold a different theological stance.
 *   - Study as archiving adherents: Excluded voices who view study as preservation, not fulfillment.
 *   - Analytical observers: External academic analysts.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_occupation, 0.15).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_occupation, 0.2).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_occupation, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, extractiveness, 0.15).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_occupation, rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_occupation, "Temple Sacrifice Obligation: Study as Occupation").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_occupation, "religious/halakhic/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_occupation, '9eb8eff2-7973-4544-ae07-e633beb2abcf').
narrative_ontology:cs_kernel_codification('9eb8eff2-7973-4544-ae07-e633beb2abcf', fixed_text).
narrative_ontology:cs_authority_grounding('9eb8eff2-7973-4544-ae07-e633beb2abcf', lineage).
narrative_ontology:cs_interpretation_layer_present('9eb8eff2-7973-4544-ae07-e633beb2abcf').
narrative_ontology:cs_reading_relation('9eb8eff2-7973-4544-ae07-e633beb2abcf', temple_sacrifice_obligation__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('9eb8eff2-7973-4544-ae07-e633beb2abcf', temple_sacrifice_obligation__study_as_archiving, forecloses).
narrative_ontology:cs_axiom('9eb8eff2-7973-4544-ae07-e633beb2abcf', foundational, torah_study_equivalent_to_performance).
narrative_ontology:cs_axiom_status(torah_study_equivalent_to_performance, holdable).
narrative_ontology:cs_axiom_grounding('9eb8eff2-7973-4544-ae07-e633beb2abcf', torah_study_equivalent_to_performance, theological).
narrative_ontology:cs_reference_frame('9eb8eff2-7973-4544-ae07-e633beb2abcf', halakhic_continuity_through_study).
narrative_ontology:cs_drift_state('9eb8eff2-7973-4544-ae07-e633beb2abcf', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9eb8eff2-7973-4544-ae07-e633beb2abcf', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, halakhic_scholars).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, observant_jews).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, torah_study_as_divine_fulfillment).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, halakhic_continuity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary interpreters and transmitters of Jewish law. They define and uphold the reading that study fulfills the obligation, thereby maintaining the continuity of religious practice and their own authority within the tradition. Their identity is deeply fused with this interpretive role.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, halakhic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Adhere to this interpretation, finding a legitimate path to fulfill the divine commandment of sacrifices in the absence of the Temple. Their religious life is structured around this understanding, and their sense of obligation is satisfied through study. Exiting this framework would mean abandoning a core tenet of their religious identity.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, observant_jews, beneficiary,
    organized, biographical, identity_locked, global).

% Hold the view that the obligation for sacrifices is suspended until the Messianic era and the rebuilding of the Temple. Their perspective is not the dominant one within mainstream Halakha, and they are largely outside the interpretive consensus that this constraint represents.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, messianic_suspension_adherents, excluded,
    moderate, generational, constrained, global).

% Believe that study of sacrifice law is important for preserving knowledge for future restoration, but does not *fulfill* the present obligation. Their view is distinct from the 'study as occupation' reading and is not the prevailing interpretation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, study_as_archiving_adherents, excluded,
    moderate, generational, constrained, global).

% Academics and scholars of religion who analyze the historical, theological, and sociological implications of this halakhic interpretation, without being bound by its religious obligations.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_obligation__study_as_occupation, halakhic_scholars).
narrative_ontology:fixing_cost_class(temple_sacrifice_obligation__study_as_occupation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent and legitimate framework for observant Jews to fulfill the divine commandment regarding Temple sacrifices, by re-channeling the obligation into the intellectual and devotional act of study, in the absence of the physical Temple.
% TRANSFER_FUNCTION: Transfers the spiritual merit and fulfillment of the sacrifice obligation from the physically impossible act of animal sacrifice to the accessible and continuous act of Torah study, thereby maintaining religious continuity and meaning.
% ABSENT_VOICES: Adherents of the 'messianic suspension' reading (who believe the obligation is simply suspended) and the 'study as archiving' reading (who believe study preserves knowledge but does not fulfill the present obligation) are largely excluded from the dominant interpretive consensus.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, observant Jews would face an unfulfillable divine commandment, leading to a profound crisis of religious obligation, practice, and identity. The entire structure of post-Temple Judaism would be destabilized, requiring a fundamental theological reorganization.
% FOUNDING_PROBLEM: The destruction of the Second Temple (70 CE) rendered the central divine commandment of animal sacrifices impossible to perform, creating an existential crisis for Jewish religious life and the continuity of Halakha.
% FOUNDING_PROBLEM_CORROBORATION: Historical rabbinic texts (e.g., Talmudic discussions), the widespread acceptance of this interpretation within Orthodox Judaism for nearly two millennia, and the continued absence of the Temple itself, all corroborate the problem's ongoing relevance and the interpretation's function.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__study_as_occupation, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__study_as_occupation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__study_as_occupation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(temple_sacrifice_obligation__study_as_occupation, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__study_as_occupation, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is low because the act of study is genuinely considered a fulfillment of the obligation, not a burdensome substitute. There are no identifiable victims, as the constraint provides a solution rather than imposing costs. Suppression is low because adherence is voluntary, driven by internal religious commitment rather than external coercion. Theater ratio is low as the study is a sincere and functional religious act. Accessibility collapse is high because the physical Temple is absent, making the original form of the obligation impossible. Resistance is low because this interpretation is widely accepted within mainstream Orthodox Judaism.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of halakhic scholars and observant Jews, this constraint is a vital 'Rope' that enables religious life. From the perspective of those who believe the obligation is suspended or merely archived, it might be seen as a 'Tangled Rope' or even a 'Snare' that misdirects religious energy, but this constraint story focuses on the dominant 'study as occupation' reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic scholars are beneficiaries and agenda-setters, as they maintain the interpretive framework that allows for continuity and reinforces their authority. Observant Jews are beneficiaries, as they gain a path to fulfill their religious duties. Adherents of alternative readings are excluded, as their interpretations are not the dominant ones that structure practice.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''study_as_occupation'' reading of the ''temple_sacrifice_obligation'' kernel, distinct from its sibling readings?',
    'Analysis of primary halakhic texts and theological discourse to confirm the specific interpretive claims and their divergence from ''messianic_suspension'' and ''study_as_archiving''.',
    'If misidentified, the classification would apply to a different structural claim, leading to incorrect assessment of extractiveness and coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the precise identity of this specific reading within the kernel.').

omega_variable(
    fulfillment_vs_substitution_ambiguity,
    'Is the act of study truly considered a direct fulfillment of the sacrifice obligation, or is it a substitution that carries a hidden cost of non-ideal performance?',
    'Deep theological and phenomenological inquiry into the lived experience and textual justifications for ''fulfillment'' versus ''substitution'' within the tradition. This is a conceptual distinction within the framework itself.',
    'If study is a mere substitution, the effective extractiveness might be higher (a cost of non-ideal performance), potentially shifting the classification towards a ''Tangled Rope'' for some seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fulfillment_vs_substitution_ambiguity, conceptual, 'Distinguishes between genuine fulfillment and a compensatory substitution.').

omega_variable(
    impact_of_temple_rebuilding,
    'How would the rebuilding of the Temple and the resumption of sacrifices impact the validity and function of the ''study as occupation'' reading?',
    'Hypothetical analysis of halakhic responses to a rebuilt Temple. Would the ''study as occupation'' reading be superseded, integrated, or continue to hold independent validity?',
    'If the reading is entirely superseded, it would transition to a ''Piton'' (atrophied function). If integrated, its extractiveness might shift as its role changes. This would represent a major lifecycle drift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(impact_of_temple_rebuilding, empirical, 'Examines the future lifecycle of the constraint under changed conditions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_occupation, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0, 0.1).
narrative_ontology:measurement(temp_tr_t25, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 25, 0.1).
narrative_ontology:measurement(temp_tr_t50, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 50, 0.1).
narrative_ontology:measurement(temp_tr_t75, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 75, 0.1).
narrative_ontology:measurement(temp_tr_t100, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(temp_be_t25, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 25, 0.15).
narrative_ontology:measurement(temp_be_t50, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(temp_be_t75, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 75, 0.15).
narrative_ontology:measurement(temp_be_t100, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(temp_su_t25, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 25, 0.2).
narrative_ontology:measurement(temp_su_t50, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 50, 0.2).
narrative_ontology:measurement(temp_su_t75, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 75, 0.2).
narrative_ontology:measurement(temp_su_t100, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 100, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__study_as_occupation, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
