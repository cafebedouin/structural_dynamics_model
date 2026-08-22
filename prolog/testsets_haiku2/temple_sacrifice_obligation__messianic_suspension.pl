% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__messianic_suspension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: temple_sacrifice_obligation__messianic_suspension
 *   human_readable: Temple Sacrifice Obligation (Messianic Suspension Reading)
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   The temple sacrifice obligation is grounded in Torah and was the central
 *   religious obligation of Jewish practice until the Temple's destruction in
 *   70 CE. The obligation has never been formally abrogated in Jewish law,
 *   yet diaspora Jews cannot fulfill it. This reading instantiates one
 *   authoritative response: the obligation is suspended, not violated or
 *   fulfilled, pending messianic restoration when the Temple will be rebuilt
 *   and sacrifice can resume. Study of sacrifice law is neither fulfillment
 *   (the obligation is not currently being met) nor violation (the obligation
 *   is not currently demanded), but maintenance of knowledge-in-waiting. The
 *   constraint operates almost entirely in the legitimacy register — it
 *   creates no current extraction, no victim set, no enforced transfer. Its
 *   operation is theatrical: the obligation is theoretically live but
 *   practically inert, maintained through scholarly engagement and ceremonial
 *   acknowledgment rather than actual performance. The reading is one of
 *   three coherent interpretations of the same kernel
 *   (temple_sacrifice_obligation); the sibling readings (study_as_occupation,
 *   study_as_archiving) offer structurally distinct solutions to the same
 *   founding problem.
 *
 * KEY AGENTS:
 *   - jewish_legal_authority_messianic_school — institutional agenda setter, maintains the reading that obligation is suspended
 *   - jewish_legal_scholars — institutional beneficiaries of the suspension reading's maintenance, preserve interpretive authority over obligation deferral
 *   - diaspora_jewish_communities — organized observers, live under the suspended obligation as legitimate background structure
 *   - alternative_halakhic_readings — institutional excluded voices, would argue study constitutes occupation or archiving of obligation
 *   - messianic_restoration_event — non-agent reference point that triggers the constraint's sunset clause
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__messianic_suspension, 0.08).
domain_priors:suppression_score(temple_sacrifice_obligation__messianic_suspension, 0.12).
domain_priors:theater_ratio(temple_sacrifice_obligation__messianic_suspension, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, extractiveness, 0.08).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__messianic_suspension, scaffold).
narrative_ontology:human_readable(temple_sacrifice_obligation__messianic_suspension, "Temple Sacrifice Obligation (Messianic Suspension Reading)").
narrative_ontology:topic_domain(temple_sacrifice_obligation__messianic_suspension, "religious/halakhic").

narrative_ontology:has_sunset_clause(temple_sacrifice_obligation__messianic_suspension).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__messianic_suspension, 'b26194c5-71d2-447d-a649-526c68ff40e3').
narrative_ontology:cs_kernel_codification('b26194c5-71d2-447d-a649-526c68ff40e3', fixed_text).
narrative_ontology:cs_authority_grounding('b26194c5-71d2-447d-a649-526c68ff40e3', lineage).
narrative_ontology:cs_interpretation_layer_present('b26194c5-71d2-447d-a649-526c68ff40e3').
narrative_ontology:cs_reading_relation('b26194c5-71d2-447d-a649-526c68ff40e3', temple_sacrifice_obligation__study_as_occupation, coexists_with).
narrative_ontology:cs_reading_relation('b26194c5-71d2-447d-a649-526c68ff40e3', temple_sacrifice_obligation__study_as_archiving, coexists_with).
narrative_ontology:cs_axiom('b26194c5-71d2-447d-a649-526c68ff40e3', foundational, obligation_never_abrogated).
narrative_ontology:cs_axiom_status(obligation_never_abrogated, holdable).
narrative_ontology:cs_axiom_grounding('b26194c5-71d2-447d-a649-526c68ff40e3', obligation_never_abrogated, deontological).
narrative_ontology:cs_axiom('b26194c5-71d2-447d-a649-526c68ff40e3', foundational, restoration_event_adjudicates_status).
narrative_ontology:cs_axiom_status(restoration_event_adjudicates_status, holdable).
narrative_ontology:cs_axiom_grounding('b26194c5-71d2-447d-a649-526c68ff40e3', restoration_event_adjudicates_status, theological).
narrative_ontology:cs_reference_frame('b26194c5-71d2-447d-a649-526c68ff40e3', post_destruction_halakhic_consensus).
narrative_ontology:cs_drift_state('b26194c5-71d2-447d-a649-526c68ff40e3', contemporary_extended_diaspora, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b26194c5-71d2-447d-a649-526c68ff40e3', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, jewish_legal_scholars_halakhic_discourse).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__messianic_suspension, messianic_restoration_premise).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__messianic_suspension, obligation_deferral_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the reading that temple sacrifice obligation is suspended, not abrogated, pending messianic restoration. Administers the scholarly consensus that study of sacrifice law keeps the knowledge intact and the obligation technically live, though unfulfillable in diaspora. Bears the authority cost of holding open the obligation across generations without active enforcement or current payers.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, jewish_legal_authority_messianic_school, agenda_setter,
    institutional, civilizational, analytical, global).

% Maintain interpretive authority over the boundary between fulfilled, violated, and suspended obligations. The suspension reading preserves their role as the adjudicators of what constitutes legitimate deferral of a standing obligation. Their interpretive labor is justified by the obligation's theoretical persistence.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, jewish_legal_scholars_halakhic_discourse, beneficiary,
    institutional, civilizational, analytical, global).

% Live under the constraint that the obligation is suspended but not eliminated. They are neither targets (nothing extracted from them currently) nor active beneficiaries, but they experience the constraint as background legitimacy structure: obligation exists, suspension is justified, restoration is the reference frame. Their acceptance of the suspension is what allows the constraint to persist without enforcement or explicit victims.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, diaspora_jewish_communities, observer,
    organized, generational, constrained, global).

% Would argue that study of sacrifice law constitutes legitimate occupation of the obligation (study_as_occupation reading) or that study merely archives knowledge without fulfilling the obligation (study_as_archiving reading). Their exclusion from this particular interpretation is not enforced but is maintained through scholarly consensus and institutional channeling toward the suspension reading.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, alternative_halakhic_readings, excluded,
    institutional, civilizational, analytical, global).

% The non-agent reference point that adjudicates the constraint. Restoration is the moment at which the constraint's sunset clause triggers and the obligation moves from suspended to either active-and-fulfillable or voided. The constraint's entire structure is oriented toward this future event.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, messianic_restoration_event, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_obligation__messianic_suspension, messianic_restoration_event).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains institutional legitimacy for deferring an obligation that cannot be fulfilled in the diaspora without delegitimizing the obligation itself or requiring constant violation acknowledgment. The suspension reading coordinates scholarly, communal, and religious practice around a unified answer to 'why is this obligation not being carried out?'
% TRANSFER_FUNCTION: No active transfer. The constraint moves no goods, services, or status currently. It preserves the potential future obligation by freezing its status at 'suspended' rather than allowing it to drift into 'abrogated' or 'violated.' The constraint's operation is entirely in the legitimacy register, not in material transfer.
% ABSENT_VOICES: Scholars and communities who hold the study_as_occupation or study_as_archiving readings are structurally positioned as minority voices within the larger halakhic discourse. They are not excluded from participation, but the institutional consensus channels interpretation toward the suspension reading, marginalizing alternative framings.
% DISAPPEARANCE_RATIONALE: If the suspension reading vanished and were replaced by a universal acceptance of study_as_occupation, the obligation would reorganize into active scholarly practice and institutional structure would shift to treat study itself as fulfillment. If replaced by study_as_archiving, the obligation would be acknowledged as deferred but unfulfilled, requiring different legitimacy justifications. The constraint's disappearance would not eliminate the obligation itself, but it would force a different relationship to it.
% FOUNDING_PROBLEM: After the destruction of the Second Temple (70 CE), sacrifice obligation became impossible to fulfill. The founding problem was: how to maintain an obligation that cannot be carried out without either abrogating it entirely (which would deny its divine origin) or constantly acknowledging violation. The suspension reading solved this by holding the obligation open but suspended until restoration.
% FOUNDING_PROBLEM_CORROBORATION: Halakhic texts and rabbinic authority from the geonic period forward attest the founding problem and its persistence: the obligation remains unrealized. External observers (religious studies scholars, historians) attest that the suspension reading has become institutionalized as the dominant interpretation across Jewish legal schools. No corroborating voice outside the halakhic tradition itself, but the reading's persistence across nearly 2,000 years and its institutional embedding across all major legal schools constitutes corroboration by practice.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__messianic_suspension, contested).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__messianic_suspension, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__messianic_suspension, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__messianic_suspension, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__messianic_suspension, 0.08, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is very low (0.08) because no current party collects from the constraint, no goods or services flow, and no asymmetric transfer occurs. The obligation's suspension benefits no identifiable agent and harms none currently — it is a pure deferral in the legitimacy register. Suppression is low (0.12) because the constraint persists through scholarly consensus and communal acceptance, not through coercion or suppression of alternatives. Alternative readings (study_as_occupation, study_as_archiving) are held by minority voices but are not actively suppressed — they coexist in the halakhic conversation, which is why suppression stays low. Theater ratio is high and rising (0.45 → 0.65 over the interval) because the constraint's primary operation is maintaining the appearance of an obligation that is theoretically live but practically inert. As diaspora duration extends, the performative character intensifies: ceremonial acknowledgment of the obligation (in liturgy, study, remembrance) becomes increasingly theatrical as actual restoration recedes. The measurements track the slow accumulation of theater as the suspension becomes less an expectation of near-term restoration and more a permanent background condition. Accessibility collapse is high (0.78) because once the founding problem is understood (obligation exists but cannot be fulfilled), alternatives become structurally unavailable without either abrogating the obligation (impossible without denying Torah) or accepting constant violation. The three readings (suspension, occupation, archiving) are the only coherent alternatives within the constraint of respecting the obligation's legal standing. Resistance is low (0.22) because no party actively resists the suspension reading — it is the institutional consensus. The absence of resistance is precisely what allows the constraint to persist without enforcement.
 *
 * PERSPECTIVAL GAP:
 *   Different seats experience the constraint differently in terms of meaning, not extractiveness. From the halakhic authority seat, the suspension reading is the coherent solution to an unsolvable founding problem. From the study_as_occupation seat (alternative reading), the same constraint is an under-realization of opportunity: study should be treated as fulfillment, elevating scholarly practice to active obligation-meeting. From the study_as_archiving seat, the constraint is a misdescription: study preserves knowledge but does not live the obligation, which should be acknowledged as deferred without the fiction of suspension. These are perspectival differences in how the obligation is read, not differences in d or χ. No seat computes higher or lower extraction; they compute different understandings of the obligation's current status. The engine will compute the same low-extraction profile from all seats because the structural data (no current transfer, no victims, no enforcement) is shared.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint has no d-weighted seats because it has no victims and no beneficiaries in the structural sense required for extraction. The scholarly authority (agenda_setter) maintains interpretive authority over the obligation, which might appear as beneficiary positioning, but the 'benefit' is purely in the legitimacy register — no material flows, no asymmetric transfer. The communities (observers) live under the constraint without being harmed or helped in ways that would shift directionality toward extractiveness. The excluded voices (alternative readings) are not victims of the constraint; they are alternative interpretations held by minority parties. If any seat has elevated directionality, it is the halakhic authority's role in maintaining the reading itself, but this is administrative/interpretive work, not extraction. The constraint's claim as scaffold (sunset clause) reflects the reading's structural assumption that restoration is the terminating event that will force obligation-status adjudication.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (obligation cannot be fulfilled in diaspora) is live and likely to remain live indefinitely if restoration never occurs. The constraint resolves the mandatrophy by deferring obligation-adjudication to the messianic event. However, if restoration is indefinitely delayed, the constraint faces mandatrophy risk: the obligation may become so disconnected from any possible restoration that it functionally becomes either study_as_occupation (study absorbs the obligation's meaning) or study_as_archiving (study preserves knowledge without obligation-fulfillment). The current scaffolding (sunset clause triggered by restoration) assumes restoration is a coherent future event; if that assumption collapses, the constraint drifts from scaffold into piton (maintained by institutional inertia and theater rather than by clear sunset mechanism). This is captured in the omegas.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_vs_archiving_boundary,
    'Does study of sacrifice law constitute maintenance of a live-but-suspended obligation (suspension reading), or merely archival preservation of knowledge without fulfilling the obligation (archiving reading)?',
    'Messianic restoration event: if restoration triggers immediate implementation of sacrifice law based on knowledge preserved during diaspora, the suspension reading is vindicated; if restoration requires new revelation or reestablishment of alternative fulfillment mechanisms, the archiving reading becomes more plausible.',
    'If the boundary dissolves into archiving, the obligation''s current status shifts from ''suspended-and-live'' to ''deferred-with-knowledge-preserved,'' which would change the legitimacy justification for scholarly engagement and potentially elevate alternative readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suspension_vs_archiving_boundary, conceptual, 'Structural ambiguity between suspension (obligation remains live) and archiving (knowledge preserved but obligation not currently active).').

omega_variable(
    messianic_event_counterfactual,
    'If messianic restoration never occurs (or if the temporal horizon extends indefinitely), at what point does the suspension reading collapse into either perpetual deferral or effective abrogation?',
    'Empirical: historical observation of whether institutional commitment to the suspension reading persists across centuries of non-restoration. Conceptual: internal halakhic debate over whether an obligation can remain suspended indefinitely without approaching practical abrogation.',
    'If restoration remains indefinitely deferred, the suspension reading may face pressure to redefine into study_as_occupation (study becomes fulfillment) or study_as_archiving (obligation acknowledged as effectively suspended until restoration). The constraint''s claimed_type shifts from scaffold (sunset clause) to piton (persistent deferral maintained theatrically).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_event_counterfactual, empirical, 'Whether indefinite non-restoration collapses the suspension reading or forces redefinition.').

omega_variable(
    authority_grounding_reconstruction,
    'What authority structure maintains the suspension reading — is it lineage (transmitted halakhic consensus), practice (centuries of communities accepting the reading), or extraction (institutional benefit to maintaining the obligation as suspended)?',
    'Genealogical analysis of how the reading was transmitted and institutionalized; examination of whether alternative readings are suppressed because they threaten institutional authority.',
    'If authority is primarily lineage/practice, the reading is likely robust to counterfactual pressure. If extraction plays a role (institutional authority strengthened by maintaining the live-but-suspended status), the reading may face pressure to shift toward study_as_occupation or archiving if that extraction weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_reconstruction, conceptual, 'Epistemic status and authority grounding of the suspension reading.').

omega_variable(
    kernel_readings_mutual_exclusivity,
    'Can a single Jewish legal framework coherently hold the suspension reading (obligation is suspended), study_as_occupation (study fulfills the obligation), and study_as_archiving (study preserves knowledge without fulfilling) simultaneously, or are they mutually exclusive interpretations?',
    'Detailed analysis of how contemporary halakhic schools treat the three readings — do they allow parallel coexistence in different legal traditions, or does adoption of one preclude the others?',
    'If mutually exclusive, the constraint family should model as forecloses relationships. If coexistent across different communities, the modeling should reflect coexists_with. This determines the network topology of the constraint family.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_readings_mutual_exclusivity, conceptual, 'Logical structure of the three readings relative to each other.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__messianic_suspension, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 0, 0.45).
narrative_ontology:measurement_basis(temp_tr_t0, projected).
narrative_ontology:measurement(temp_tr_t200, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 200, 0.52).
narrative_ontology:measurement_basis(temp_tr_t200, observed).
narrative_ontology:measurement(temp_tr_t500, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 500, 0.58).
narrative_ontology:measurement_basis(temp_tr_t500, observed).
narrative_ontology:measurement(temp_tr_t1000, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1000, 0.63).
narrative_ontology:measurement_basis(temp_tr_t1000, observed).
narrative_ontology:measurement(temp_tr_t1500, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1500, 0.67).
narrative_ontology:measurement_basis(temp_tr_t1500, observed).
narrative_ontology:measurement(temp_tr_t2000, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 2000, 0.65).
narrative_ontology:measurement_basis(temp_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 0, 0.05).
narrative_ontology:measurement_basis(temp_be_t0, projected).
narrative_ontology:measurement(temp_be_t200, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 200, 0.06).
narrative_ontology:measurement_basis(temp_be_t200, observed).
narrative_ontology:measurement(temp_be_t500, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 500, 0.07).
narrative_ontology:measurement_basis(temp_be_t500, observed).
narrative_ontology:measurement(temp_be_t1000, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1000, 0.08).
narrative_ontology:measurement_basis(temp_be_t1000, observed).
narrative_ontology:measurement(temp_be_t1500, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1500, 0.08).
narrative_ontology:measurement_basis(temp_be_t1500, observed).
narrative_ontology:measurement(temp_be_t2000, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 2000, 0.08).
narrative_ontology:measurement_basis(temp_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(temp_su_t0, projected).
narrative_ontology:measurement(temp_su_t200, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 200, 0.1).
narrative_ontology:measurement_basis(temp_su_t200, observed).
narrative_ontology:measurement(temp_su_t500, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 500, 0.11).
narrative_ontology:measurement_basis(temp_su_t500, observed).
narrative_ontology:measurement(temp_su_t1000, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 1000, 0.11).
narrative_ontology:measurement_basis(temp_su_t1000, observed).
narrative_ontology:measurement(temp_su_t1500, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 1500, 0.12).
narrative_ontology:measurement_basis(temp_su_t1500, observed).
narrative_ontology:measurement(temp_su_t2000, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 2000, 0.12).
narrative_ontology:measurement_basis(temp_su_t2000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__messianic_suspension, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_obligation__messianic_suspension, 0.05).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation__study_as_occupation).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation__study_as_archiving).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel (temple_sacrifice_obligation) that decomposes into three structurally distinct constraints: messianic_suspension (obligation suspended, restoration-contingent); study_as_occupation (study fulfills obligation); study_as_archiving (study preserves knowledge without fulfillment). The readings share the kernel (fixed Torah text) but differ in ε (obligation_satisfaction referent) and authority-grounding structure. ε values differ: suspension has near-zero ε (no current obligation to extract from); occupation has higher ε if study practices are mandatory (extraction from those required to study); archiving has variable ε depending on whether knowledge-preservation is compulsory. All three coexist in the halakhic tradition; none forecloses the others within the diversity of Jewish legal schools. They are linked by network.affects_constraints as sibling readings, not as variants of a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
