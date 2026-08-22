% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__messianic_suspension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__messianic_suspension, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: temple_sacrifice_obligation__messianic_suspension
 *   human_readable: Temple Sacrifice Obligation â Messianic Suspension Reading
 *   domain: religious/halakhic/commitment_systems
 *
 * SUMMARY:
 *   This constraint is the messianic_suspension reading of the
 *   temple_sacrifice_obligation kernel. Within Rabbinic Judaism, the biblical
 *   commandment to offer sacrifices at the Jerusalem Temple persists as a
 *   fixed textual kernel. Because the Temple has been destroyed, the rabbinic
 *   authority structure has interpreted the kernel as suspendedâneither
 *   fulfilled nor violatedâuntil a messianic restoration. Study of
 *   sacrificial law continues not as preparation, not as substitute
 *   fulfillment, but as maintenance of knowledge-in-waiting. Sibling readings
 *   include study_as_occupation (study fulfills the obligation now) and
 *   study_as_archiving (study preserves knowledge for the future). This
 *   reading is distinguished by its claim that the obligation itself is in
 *   abeyance.
 *
 * KEY AGENTS:
 *   - rabbinic_authority (institutional/identity_locked): agenda_setter that administers the suspension ruling and defers adjudication to messianic time
 *   - halakhic_community (organized/identity_locked): beneficiary of coordinated clarity around an unperformable commandment
 *   - torah_students (moderate/identity_locked): beneficiary who maintains the dormant legal corpus through study
 *   - restorationist_movements (moderate/constrained): excluded voice advocating immediate sacrifice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__messianic_suspension, 0.05).
domain_priors:suppression_score(temple_sacrifice_obligation__messianic_suspension, 0.05).
domain_priors:theater_ratio(temple_sacrifice_obligation__messianic_suspension, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, extractiveness, 0.05).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__messianic_suspension, rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__messianic_suspension, "Temple Sacrifice Obligation â Messianic Suspension Reading").
narrative_ontology:topic_domain(temple_sacrifice_obligation__messianic_suspension, "religious/halakhic/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__messianic_suspension, '8fa8e69a-eb0e-49a8-b893-5502f08aad7d').
narrative_ontology:cs_kernel_codification('8fa8e69a-eb0e-49a8-b893-5502f08aad7d', fixed_text).
narrative_ontology:cs_authority_grounding('8fa8e69a-eb0e-49a8-b893-5502f08aad7d', lineage).
narrative_ontology:cs_interpretation_layer_present('8fa8e69a-eb0e-49a8-b893-5502f08aad7d').
narrative_ontology:cs_reading_relation('8fa8e69a-eb0e-49a8-b893-5502f08aad7d', temple_sacrifice_obligation__study_as_occupation, forecloses).
narrative_ontology:cs_reading_relation('8fa8e69a-eb0e-49a8-b893-5502f08aad7d', temple_sacrifice_obligation__study_as_archiving, coexists_with).
narrative_ontology:cs_axiom('8fa8e69a-eb0e-49a8-b893-5502f08aad7d', foundational, temple_destruction_suspends_sacrificial_obligation).
narrative_ontology:cs_axiom_status(temple_destruction_suspends_sacrificial_obligation, holdable).
narrative_ontology:cs_axiom_grounding('8fa8e69a-eb0e-49a8-b893-5502f08aad7d', temple_destruction_suspends_sacrificial_obligation, deontological).
narrative_ontology:cs_axiom('8fa8e69a-eb0e-49a8-b893-5502f08aad7d', foundational, messianic_restoration_reactivates_temple_practice).
narrative_ontology:cs_axiom_status(messianic_restoration_reactivates_temple_practice, holdable).
narrative_ontology:cs_axiom_grounding('8fa8e69a-eb0e-49a8-b893-5502f08aad7d', messianic_restoration_reactivates_temple_practice, deontological).
narrative_ontology:cs_reference_frame('8fa8e69a-eb0e-49a8-b893-5502f08aad7d', temple_era_operative).
narrative_ontology:cs_drift_state('8fa8e69a-eb0e-49a8-b893-5502f08aad7d', contemporary_diaspora, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('8fa8e69a-eb0e-49a8-b893-5502f08aad7d', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, halakhic_community).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, torah_students).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the halakhic framework that governs biblical commandments in the absence of the Temple. Has ruled that the sacrificial obligation is suspendedâneither fulfilled nor violatedâpending a future messianic restoration. Defers active adjudication of sacrifice to that restoration event and maintains the legal corpus through transmission and study.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, rabbinic_authority, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Lives under the coordinated ruling that sacrifice is presently impossible and not required. Benefits from the communal clarity that prevents schism over unperformable commandments. Practice orients around prayer, study, and ethical observance rather than sacrificial anxiety.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, halakhic_community, beneficiary,
    organized, generational, identity_locked, global).

% Study sacrificial tractates not as immediate preparation for performance, nor as substitute fulfillment, but as maintenance of a knowledge system held in suspension. Their labor preserves executable legal architecture for a deferred future.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, torah_students, beneficiary,
    moderate, biographical, identity_locked, global).

% Advocate for immediate restoration of sacrifice through political or priestly initiative. Structurally excluded from normative halakhic conversation because the authority framework defers the issue to messianic time; their proposals are treated as premature or illegitimate.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, restorationist_movements, excluded,
    moderate, biographical, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates communal practice around the physical absence of the Temple by establishing a shared halakhic status for the biblical sacrificial commandments: neither violated nor fulfilled, but held in abeyance until a future restoration event.
% TRANSFER_FUNCTION: No material transfer occurs. The arrangement channels cognitive and pedagogical labor from the community into maintenance of a dormant legal corpus, without extracting wealth or coerced compliance from any party.
% ABSENT_VOICES: Priestly advocates of immediate restoration, Karaite literalists who reject rabbinic suspension, and activist messianic movements are excluded; they would contest the deferral but are kept outside the halakhic consensus.
% DISAPPEARANCE_RATIONALE: If the suspension ruling vanished, the community would face an immediate halakhic crisis regarding the biblical commandments to sacrifice. Competing factions would contest whether to attempt illicit altar-building, treat the commandments as violated, or adopt alternative fulfillment theologies. The present communal calm depends on this coordinate.
% FOUNDING_PROBLEM: The destruction of the Second Temple rendered the biblical sacrificial commandments technically unperformable, creating a crisis of halakhic continuity, communal identity, and scriptural fidelity.
% FOUNDING_PROBLEM_CORROBORATION: Roman and Christian historiography document the Temple's destruction and the Jewish community's non-sacrificial pivot; contemporary archaeologists and historians corroborate the physical absence of the Temple as the ongoing condition. Rabbinic sources attest the problem from within, but extra-halakhic historiography confirms the material circumstance.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__messianic_suspension, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__messianic_suspension, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__messianic_suspension, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__messianic_suspension, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__messianic_suspension, 0.05, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__messianic_suspension_tests).
:- end_tests(temple_sacrifice_obligation__messianic_suspension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.05) because the constraint explicitly negates any current obligation; no party is forced to sacrifice, and no party extracts wealth or labor through the suspension itself. Suppression is minimal (0.05) because persistence relies on consensus and identity rather than coercion. Theater is low (0.15) and drifting only slightly upward over two millennia: study is framed as genuine preservation of executable knowledge, though increasing temporal distance from practice introduces minor performative risk. Accessibility collapse is high (0.80) because, once the halakhic framework is accepted, the alternative of performing sacrifice now collapses as a live option; resistance is negligible (0.05) because the ruling is broadly internalized.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (rabbinic authority) and beneficiary seats (community, students) largely converge in experiencing this constraint as coordination: it prevents schism and preserves identity across an indefinite exile. The excluded restorationist seat would experience it as a barrier to their preferred practice, but they are structurally outside the constraint's domain. The engine will compute low directionality for all internal seats because no seat is structurally targeted for extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   All named internal stakeholders are either agenda_setters or beneficiaries. No victim group is declared because the suspension relieves a would-be burden rather than imposing one. The rabbinic authority sits near d=0.5 as symmetric administrator; the community and students sit near the beneficiary end because the suspension spares them from an impossible obligation and gives their study meaning. Restorationist movements would compute near the target end, but they are excluded from the constraint's operating consensus rather than governed by it.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy mislabeling because its founding problem (Temple destruction) remains live, and the suspension genuinely coordinates the community around that ongoing absence. There is no administrator who profits from maintaining the suspension, and no diffuse victim who pays. Were the Temple restored, the constraint would dissolve naturally because its coordination function would become obsolete; this is the signature of a genuine rope rather than a piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    messianic_trigger_empirical_status,
    'Is the messianic restoration that would reactivate the sacrificial obligation an empirically contingent historical event, or a metaphysical commitment outside historical verification?',
    'Historical occurrence or non-occurrence of a restoration event recognized by the halakhic community as triggering the obligation.',
    'If empirically contingent and never occurs, the suspension is effectively permanent; if metaphysical, the constraint''s persistence depends entirely on faith commitment and is not falsifiable by history.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_trigger_empirical_status, conceptual, 'Epistemic status of the messianic restoration trigger').

omega_variable(
    study_performative_drift,
    'Does study of sacrificial law function as genuine preservation of executable knowledge, or has it become increasingly performative maintenance of rabbinic identity over two millennia of non-practice?',
    'Pedagogical outcome assessment: testing whether trained students could operationalize the law if a Temple were restored tomorrow.',
    'If performative, theater_ratio is understated and long-term piton dynamics may be present; if genuinely functional, the low theater reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_performative_drift, empirical, 'Functional versus performative character of suspended-law study').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__messianic_suspension, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 0, 0.1).
narrative_ontology:measurement(temp_tr_t500, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 500, 0.12).
narrative_ontology:measurement(temp_tr_t1000, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1000, 0.14).
narrative_ontology:measurement(temp_tr_t1500, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1500, 0.16).
narrative_ontology:measurement(temp_tr_t2000, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 2000, 0.18).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(temp_be_t500, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 500, 0.05).
narrative_ontology:measurement(temp_be_t1000, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1000, 0.05).
narrative_ontology:measurement(temp_be_t1500, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1500, 0.05).
narrative_ontology:measurement(temp_be_t2000, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 2000, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(temp_su_t500, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 500, 0.05).
narrative_ontology:measurement(temp_su_t1000, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 1000, 0.05).
narrative_ontology:measurement(temp_su_t1500, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 1500, 0.05).
narrative_ontology:measurement(temp_su_t2000, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 2000, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__messianic_suspension, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation__study_as_occupation).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation__study_as_archiving).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the temple_sacrifice_obligation kernel. The three sibling readings (messianic_suspension, study_as_occupation, study_as_archiving) share the same referentâthe biblical sacrificial commandmentsâbut assign different halakhic statuses to study and obligation in the Temple's absence. They are structurally distinct constraints per the Îµ-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
