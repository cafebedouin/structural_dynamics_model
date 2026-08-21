% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__performance_only, []).

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
 *   constraint_id: sacrifice_commandment__performance_only
 *   human_readable: Sacrifice Commandment: Performance-Only Reading
 *   domain: religious/halakhic_theory/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint represents the 'performance-only' reading of the
 *   sacrifice commandment kernel within Halakhic theory. It asserts that the
 *   divine commandment for sacrifice requires physical execution in the
 *   Temple, and without the Temple, the commandment is suspended, not
 *   fulfilled by other means. This reading leads to a high extraction of
 *   scholarly attention and spiritual focus towards an unperformable act,
 *   with rabbinic authority maintaining this interpretation. The claimed type
 *   is 'snare' because the coordination story (preserving the commandment's
 *   integrity) serves as cover for the substantial extraction of intellectual
 *   and spiritual labor from its victims (scholarly attention, lay adherents)
 *   towards an unperformable act, with suppressed alternatives for
 *   fulfillment.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__performance_only, 0.85).
domain_priors:suppression_score(sacrifice_commandment__performance_only, 0.78).
domain_priors:theater_ratio(sacrifice_commandment__performance_only, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, extractiveness, 0.85).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__performance_only, snare).
narrative_ontology:human_readable(sacrifice_commandment__performance_only, "Sacrifice Commandment: Performance-Only Reading").
narrative_ontology:topic_domain(sacrifice_commandment__performance_only, "religious/halakhic_theory/commitment_system_analysis").

domain_priors:requires_active_enforcement(sacrifice_commandment__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__performance_only, '55f535d9-3c8f-4233-9be8-e096c1c1b730').
narrative_ontology:cs_kernel_codification('55f535d9-3c8f-4233-9be8-e096c1c1b730', fixed_text).
narrative_ontology:cs_authority_grounding('55f535d9-3c8f-4233-9be8-e096c1c1b730', lineage).
narrative_ontology:cs_interpretation_layer_present('55f535d9-3c8f-4233-9be8-e096c1c1b730').
narrative_ontology:cs_reading_relation('55f535d9-3c8f-4233-9be8-e096c1c1b730', sacrifice_commandment__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('55f535d9-3c8f-4233-9be8-e096c1c1b730', sacrifice_commandment__archive_maintenance, coexists_with).
narrative_ontology:cs_axiom('55f535d9-3c8f-4233-9be8-e096c1c1b730', foundational, physical_execution_is_sine_qua_non_for_fulfillment).
narrative_ontology:cs_axiom_status(physical_execution_is_sine_qua_non_for_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('55f535d9-3c8f-4233-9be8-e096c1c1b730', physical_execution_is_sine_qua_non_for_fulfillment, deontological).
narrative_ontology:cs_reference_frame('55f535d9-3c8f-4233-9be8-e096c1c1b730', halakhic_literalism_framework).
narrative_ontology:cs_drift_state('55f535d9-3c8f-4233-9be8-e096c1c1b730', post_second_temple_destruction, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('55f535d9-3c8f-4233-9be8-e096c1c1b730', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__performance_only, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, rabbinic_authority).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, halakhic_scholars).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, scholarly_attention).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, lay_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The central interpretive and enforcement body for Halakha. They maintain the doctrine that the sacrifice commandment requires physical execution in the Temple, thereby suspending its fulfillment in the present. This position reinforces their role as custodians of an immutable divine law, even if currently unperformable.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, rabbinic_authority, agenda_setter,
    institutional, generational, identity_locked, global).

% Engage in extensive study of the laws of sacrifice, even though the acts cannot be performed. This intellectual domain provides a rich field for scholarly endeavor and maintains their professional identity within the tradition. However, their intellectual labor is directed towards an unperformable act, diverting attention from living law.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, halakhic_scholars, beneficiary,
    organized, biographical, constrained, global).

% Are unable to directly fulfill the sacrifice commandment due to the absence of the Temple. They are taught that the commandment is suspended, not fulfilled by other means, which can lead to a sense of spiritual incompleteness or a diversion of their religious energies towards other forms of worship or study that are deemed 'substitutes' rather than 'fulfillments'.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, lay_adherents, payer,
    powerless, biographical, identity_locked, global).

% Represents the collective intellectual and spiritual labor of the halakhic community. Under this reading, a significant portion of this attention is directed towards the intricate details of an unperformable commandment, rather than towards contemporary halakhic challenges or other forms of religious engagement that could be actively fulfilled.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, scholarly_attention, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(sacrifice_commandment__performance_only, scholarly_attention).

% Advocate for the rebuilding of the Temple and the immediate resumption of sacrifices, often challenging the mainstream rabbinic interpretation of suspension. They are largely excluded from mainstream halakhic discourse and their views are often marginalized by the established rabbinic authority.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, messianic_activists, excluded,
    moderate, generational, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the integrity and literal meaning of the divine commandment for sacrifice by insisting on its original, physical requirements, thereby preventing reinterpretation that might dilute its significance or lead to premature, unauthorized fulfillment.
% TRANSFER_FUNCTION: Transfers significant scholarly intellectual labor and spiritual focus from potentially actionable religious practice and contemporary halakhic issues to the study of an unperformable commandment, reinforcing the authority of the interpretive tradition.
% ABSENT_VOICES: Those who advocate for alternative forms of fulfillment (e.g., study as fulfillment, prayer as substitute) or for immediate resumption of sacrifices are largely excluded from the dominant discourse that upholds the 'performance-only' suspension.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the entire framework of post-Temple Judaism would be profoundly re-evaluated. The nature of divine commandments, the role of rabbinic authority, and the spiritual practices of adherents would undergo a fundamental reorganization, as the central unperformable commandment would either be reinterpreted as fulfillable or its suspension would lose its theological weight.
% FOUNDING_PROBLEM: How to maintain the sanctity and literal meaning of the sacrifice commandment after the destruction of the Second Temple, when its physical performance became impossible, without either abandoning the commandment or allowing its premature, unauthorized fulfillment.
% FOUNDING_PROBLEM_CORROBORATION: Historical rabbinic texts (e.g., Talmudic discussions, medieval commentaries) and ongoing halakhic discourse consistently attest to the persistence and centrality of this problem in Jewish law and theology. This is corroborated by the continuous scholarly output dedicated to the topic, even from outside the immediate beneficiaries of the current interpretive framework.
narrative_ontology:disappearance_verdict(sacrifice_commandment__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__performance_only, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__performance_only, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(sacrifice_commandment__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__performance_only, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_commandment__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_commandment__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because 1,900 years of intellectual and spiritual labor are directed towards a commandment that cannot be fulfilled, representing a significant diversion of resources. Suppression is high (0.78) due to the strong theological and halakhic framework that enforces this interpretation, effectively collapsing alternatives for fulfillment and marginalizing dissenting views. Theater ratio is low (0.1) as the study itself is not performative in a theatrical sense, but rather a serious intellectual endeavor within its own framework. Accessibility collapse is high (0.9) because the physical impossibility of Temple sacrifice means no genuine alternative for direct fulfillment exists. Resistance is low (0.15) because this interpretation is widely accepted within mainstream Judaism.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinic authority and halakhic scholars, this constraint is a necessary mechanism for preserving the integrity and literal meaning of divine law, even if it means suspension. From the perspective of scholarly attention and lay adherents, it functions as a snare, diverting their efforts and spiritual aspirations towards an unperformable act, with limited avenues for alternative fulfillment.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority and halakhic scholars are beneficiaries/agenda-setters, as they maintain interpretive control and a rich intellectual domain, even if it's focused on suspension. Scholarly attention (as a collective resource) and lay adherents are victims, as their efforts are directed towards an unperformable act, and their spiritual needs for fulfillment are left in suspension. Messianic activists are excluded, as their attempts to challenge the suspension are outside the accepted interpretive framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_as_fulfillment_ambiguity,
    'Is the extensive study of sacrifice laws a form of spiritual fulfillment in itself, or merely a preparation for a future, currently impossible, physical performance?',
    'Theological re-evaluation by a widely accepted halakhic authority, or a shift in communal practice and understanding that redefines ''fulfillment'' in the absence of the Temple.',
    'If study is reclassified as fulfillment, the extractiveness from scholarly attention would decrease significantly, and the constraint might shift towards a ''rope'' or ''tangled_rope'' by providing a viable, albeit intellectual, path to fulfillment. If not, the current ''snare'' classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_as_fulfillment_ambiguity, conceptual, 'Ambiguity regarding the nature of fulfillment for the sacrifice commandment.').

omega_variable(
    mandate_obsolescence_vs_suspension,
    'Has the mandate for physical sacrifice become obsolete due to historical circumstances, or is it merely suspended, awaiting a future restoration?',
    'A definitive theological ruling on the eschatological status of the commandment, or the actual rebuilding of the Temple and resumption of sacrifices.',
    'If deemed obsolete, the constraint''s persistence would be purely inertial, shifting it towards a ''piton''. If confirmed as suspended, the ''snare'' classification holds, emphasizing the ongoing extraction of resources for a deferred mandate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandate_obsolescence_vs_suspension, preference, 'Whether the commandment''s mandate is obsolete or merely suspended.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__performance_only, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__performance_only, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sacr_tr_t390, sacrifice_commandment__performance_only, theater_ratio, 390, 0.1).
narrative_ontology:measurement(sacr_tr_t780, sacrifice_commandment__performance_only, theater_ratio, 780, 0.1).
narrative_ontology:measurement(sacr_tr_t1170, sacrifice_commandment__performance_only, theater_ratio, 1170, 0.1).
narrative_ontology:measurement(sacr_tr_t1560, sacrifice_commandment__performance_only, theater_ratio, 1560, 0.1).
narrative_ontology:measurement(sacr_tr_t1950, sacrifice_commandment__performance_only, theater_ratio, 1950, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__performance_only, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(sacr_be_t390, sacrifice_commandment__performance_only, base_extractiveness, 390, 0.78).
narrative_ontology:measurement(sacr_be_t780, sacrifice_commandment__performance_only, base_extractiveness, 780, 0.8).
narrative_ontology:measurement(sacr_be_t1170, sacrifice_commandment__performance_only, base_extractiveness, 1170, 0.82).
narrative_ontology:measurement(sacr_be_t1560, sacrifice_commandment__performance_only, base_extractiveness, 1560, 0.84).
narrative_ontology:measurement(sacr_be_t1950, sacrifice_commandment__performance_only, base_extractiveness, 1950, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_commandment__performance_only, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(sacr_su_t390, sacrifice_commandment__performance_only, suppression_requirement, 390, 0.72).
narrative_ontology:measurement(sacr_su_t780, sacrifice_commandment__performance_only, suppression_requirement, 780, 0.74).
narrative_ontology:measurement(sacr_su_t1170, sacrifice_commandment__performance_only, suppression_requirement, 1170, 0.76).
narrative_ontology:measurement(sacr_su_t1560, sacrifice_commandment__performance_only, suppression_requirement, 1560, 0.77).
narrative_ontology:measurement(sacr_su_t1950, sacrifice_commandment__performance_only, suppression_requirement, 1950, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
