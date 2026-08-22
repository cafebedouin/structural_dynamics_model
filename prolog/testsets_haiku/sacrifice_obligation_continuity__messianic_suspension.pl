% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__messianic_suspension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__messianic_suspension, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: sacrifice_obligation_continuity__messianic_suspension
 *   human_readable: Sacrifice Obligation Suspended: Messianic Readiness Maintenance
 *   domain: religious/textual/ritual
 *
 * SUMMARY:
 *   After the destruction of the Second Temple (70 CE), the Jewish tradition
 *   faced a normative crisis: animal sacrifice was no longer physically
 *   possible, yet the Biblical commandment to sacrifice was seemingly
 *   perpetual and binding. The messianic_suspension reading resolves this by
 *   declaring that the obligation is suspended (not fulfilled, not violated)
 *   pending the restoration of the Temple in the messianic age. In the
 *   interim, study of the sacrificial law and maintenance of readiness became
 *   the operative form of observance. This reading competes with three
 *   siblings: archival_preservation (the obligation has no binding force;
 *   study preserves cultural memory only), study_as_performance (study itself
 *   fulfills the obligation; performance is unnecessary), and
 *   performance_only (the obligation requires physical sacrifice; study is
 *   mere preparation). Each reading has different implications for who
 *   benefits, who bears costs, and whether the obligation is truly suspended
 *   or has been silently reframed. This JSON instantiates the
 *   messianic_suspension reading as a clean, ε-invariant constraint.
 *
 * KEY AGENTS:
 *   - jewish_covenant_community: obligated to study and maintain readiness; identity_locked exit; moderate to organized power; civilizational time horizon — bears the readiness burden without guilt of non-performance
 *   - study_authority_institutions: agenda-setter; administers study protocols and certifies readiness; institutional power; arbitrage exit; civilizational horizon — benefits from institutional continuity and resource flows
 *   - diaspora_practitioners: constrained by geography; unable to access sites of potential performance; moderate power; biographical horizon; bears localized readiness burden
 *   - messianically_urgent_interpreters: excluded from mainstream authority; claim the obligation is reactivated now or imminently; powerful; constrained exit — would reject the suspension frame
 *   - sibling_reading_communities: excluded from this reading's authority structure; trapped in structured competition; powerful/organized; civilizational horizon
 *   - textual_scholars: analytical observers; document the doctrine's emergence and function but cannot settle normative questions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__messianic_suspension, 0.48).
domain_priors:suppression_score(sacrifice_obligation_continuity__messianic_suspension, 0.22).
domain_priors:theater_ratio(sacrifice_obligation_continuity__messianic_suspension, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, extractiveness, 0.48).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__messianic_suspension, scaffold).
narrative_ontology:human_readable(sacrifice_obligation_continuity__messianic_suspension, "Sacrifice Obligation Suspended: Messianic Readiness Maintenance").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__messianic_suspension, "religious/textual/ritual").

narrative_ontology:has_sunset_clause(sacrifice_obligation_continuity__messianic_suspension).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__messianic_suspension, '5b23cc84-67c8-49b2-ba11-4ffa63059874').
narrative_ontology:cs_kernel_codification('5b23cc84-67c8-49b2-ba11-4ffa63059874', fixed_text).
narrative_ontology:cs_authority_grounding('5b23cc84-67c8-49b2-ba11-4ffa63059874', lineage).
narrative_ontology:cs_interpretation_layer_present('5b23cc84-67c8-49b2-ba11-4ffa63059874').
narrative_ontology:cs_reading_relation('5b23cc84-67c8-49b2-ba11-4ffa63059874', sacrifice_obligation_continuity__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('5b23cc84-67c8-49b2-ba11-4ffa63059874', sacrifice_obligation_continuity__performance_only, influences).
narrative_ontology:cs_reading_relation('5b23cc84-67c8-49b2-ba11-4ffa63059874', sacrifice_obligation_continuity__archival_preservation, influences).
narrative_ontology:cs_axiom('5b23cc84-67c8-49b2-ba11-4ffa63059874', foundational, obligation_suspended_not_canceled).
narrative_ontology:cs_axiom_status(obligation_suspended_not_canceled, holdable).
narrative_ontology:cs_axiom_grounding('5b23cc84-67c8-49b2-ba11-4ffa63059874', obligation_suspended_not_canceled, deontological).
narrative_ontology:cs_axiom('5b23cc84-67c8-49b2-ba11-4ffa63059874', foundational, study_maintains_readiness_for_reactivation).
narrative_ontology:cs_axiom_status(study_maintains_readiness_for_reactivation, holdable).
narrative_ontology:cs_axiom_grounding('5b23cc84-67c8-49b2-ba11-4ffa63059874', study_maintains_readiness_for_reactivation, conventional).
narrative_ontology:cs_reference_frame('5b23cc84-67c8-49b2-ba11-4ffa63059874', sacrificial_obligation_suspended_pending_restoration).
narrative_ontology:cs_drift_state('5b23cc84-67c8-49b2-ba11-4ffa63059874', contemporary_2000_year_interval, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5b23cc84-67c8-49b2-ba11-4ffa63059874', '2026-06-12T14:33:22Z').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, study_authority_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__messianic_suspension, jewish_covenant_community).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__messianic_suspension, diaspora_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Carries the obligation to study sacrifice law and maintain readiness for restoration, even though the obligation's performance is suspended. Bears the cognitive and liturgical burden of remembrance without the cathartic function of actual sacrifice. Identity is constituted through the covenant; exit from the obligation would be exit from the tradition itself.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, jewish_covenant_community, payer,
    organized, civilizational, identity_locked, global).

% Administers the study protocols, interprets the legal tradition, and certifies readiness. Gains institutional continuity, hermeneutic authority, and resource flows (students, donations, intellectual status) from being the keeper of the tradition. Could in principle dissolve the readiness requirement or reclassify the obligation; retains the option to reframe.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, study_authority_institutions, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__messianic_suspension, study_authority_institutions, beneficiary).

% Participate in study and remembrance practices without access to the physical sites (Temple, altar, animal sacrifice sites) where the obligation would be performed if reactivated. Bear the burden of readiness maintenance without the normative clarity of performance or the option of literal fulfillment. Geographic constraint is absolute.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, diaspora_practitioners, payer,
    moderate, biographical, constrained, global).

% Interpret the messianic moment as imminent or already-begun, and therefore the obligation as already reactivated or imminently reactivated. Are excluded from the mainstream interpretive institutions by virtue of this claim. Would reject the 'suspension' framing and assert that the obligation is binding NOW, not in readiness state.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, messianically_urgent_interpreters, excluded,
    powerful, biographical, constrained, global).

% Hold alternative readings of the kernel (archival_preservation: no binding force; study_as_performance: study fulfills the obligation; performance_only: waiting for physical restoration without suspension language). Are excluded from this reading's authority structure; the readings are in structured competition for interpretive legitimacy within the same tradition.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, sibling_reading_communities, excluded,
    organized, civilizational, trapped, global).

% Analyze the legal texts and historical practice from outside the identity-locked commitment. Can document how the suspension doctrine emerged, what textual bases support it, and how it functions institutionally — but cannot settle the normative question of whether the obligation is truly suspended or merely dormant.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, textual_scholars, observer,
    powerful, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains textual knowledge, interpretive competence, and institutional readiness for the ritual order (sacrifice system) in the event of messianic restoration. Coordinates community continuity of practice and belief across diaspora, without requiring physical performance where performance is impossible or prohibited.
% TRANSFER_FUNCTION: Moves the burden of study, remembrance, liturgical recitation, and institutional support from the covenant community to dedicated study-authority institutions in exchange for certifying that readiness is maintained and the obligation is not violated by non-performance.
% ABSENT_VOICES: Messianically urgent interpreters (who believe the moment has come and the obligation is reactivated) and sibling-reading communities (study_as_performance, performance_only, archival_preservation) are excluded from the authority structure that defines 'suspension.' They would argue for different framings of the obligation's status.
% DISAPPEARANCE_RATIONALE: If the suspension doctrine disappeared overnight, the community would face a binary choice: either sacrifice law is binding NOW (requiring performance or formal renunciation), or it is no longer binding (shifting to archival or performance-only readings). The suspension frame postpones this choice indefinitely. Its disappearance would force reinterpretation.
% FOUNDING_PROBLEM: After the destruction of the Temple (70 CE), the sacrificial system could not be performed. The covenant community faced a normative crisis: is the obligation violated? Transgressed? Impossible? The suspension doctrine resolved this by declaring the obligation dormant but not cancelled — maintained in readiness for restoration when the Temple is rebuilt.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic texts and legal authorities attest the suspension doctrine as a response to the destruction. Messianically urgent interpreters attest that the founding problem is no longer live (the messianic moment is here or near). Textual scholars document the historical emergence of the doctrine. Sibling-reading communities attest alternative frameworks that reject or reframe the suspension.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__messianic_suspension, contested).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__messianic_suspension, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__messianic_suspension, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__messianic_suspension, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__messianic_suspension, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__messianic_suspension_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_continuity__messianic_suspension, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_obligation_continuity__messianic_suspension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48 at interval end) because the readiness burden is genuine but its termination condition (messianic restoration) is indefinitely deferred. The constraint extracts study labor, institutional resource flows, and psychological burden from the covenant community without providing the cathartic function of actual performance. However, extractiveness is not high because the suspension frame is (in this reading) genuinely deferral rather than permanent capture — the obligation is not violated, merely suspended. Theater_ratio is rising (0.42 → 0.58 over 2000 years), indicating that the performative aspect of study and remembrance is increasing relative to actual readiness functionality — institutions invest more in ritual commemoration and textual elaboration than in maintaining technical knowledge that would enable quick reactivation if the moment came. Suppression is low (0.22) because the suspension is framed as legitimate theology, not coerced silence — the payer community accepts the frame (though some reject it). Accessibility_collapse is low (0.35) because alternative readings of the kernel remain live and available; exit is theoretically possible through renouncing covenant identity, though this is psychologically and socially costly (identity_locked, not trapped). Resistance is moderate (0.42) because messianically urgent interpreters and sibling-reading communities actively contest the suspension frame. The measurement series shows slow drift upward in extractiveness and theater_ratio over 2000 years, consistent with institutional drift: as immediate expectation of restoration fades, institutions invest more in elaborate commemoration and less in functional readiness, extracting increasing rents from the suspended obligation.
 *
 * DIRECTIONALITY LOGIC:
 *   The covenant community bears costs without direct benefits from the deferral arrangement — directionality is toward full target (~0.75 if the suspension is permanent, ~0.35 if genuinely preparatory). Study authorities benefit from institutional continuity, resource flows, and hermeneutic control — directionality is toward beneficiary (~0.15). The key structural asymmetry is that authorities can reframe (if interpretation shifts to study_as_performance, they capture the function entirely; if it shifts to archival_preservation, the obligation dissolves) while the payer community is locked into the identity that defines obligation. This asymmetry is captured in the override: powerless payers with identity_locked exit and moderate payers with constrained exit have directionality 0.60-0.70 (targets), while institutional authorities with arbitrage exit have directionality 0.20 (beneficiaries). The messianically urgent interpreters who reject suspension have directionality 0.80 (they would be targets if the obligation were reactivated and they were forbidden from performing it).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading claims the constraint is a scaffold — temporary deferral of obligation pending reactivation. Mandatrophy would arise if the founding problem (Temple destroyed, sacrifice impossible) is no longer live, but the deferral persists. The founding_problem_status is 'contested' because authorities attest the problem is still live (readiness is real value), while messianically urgent interpreters attest it is dead (restoration is imminent or begun), and sibling-reading communities attest it has been reframed (study is now the true obligation, not deferral). The constraint avoids a clear mandatrophy signal because no canonical authority yet declares the founding problem solved — the deferral can indefinitely extend. However, the rising theater_ratio (0.42 → 0.58) is consistent with incipient theater-drift: if the obligation is truly suspended, functional readiness should be stable or declining (fewer people need to know how to actually perform sacrifice); if theater is increasing faster than function, the constraint may be drifting toward piton (performance as theater, without expectation of reactivation). The omegas document this ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint is one reading of a contested kernel. Is the sacrifice obligation genuinely suspended (awaiting reactivation) or is it merely deferred through a rhetorical frame that allows ongoing non-performance without guilt?',
    'Resolution depends on what happens if messianic restoration occurs: if the community immediately reactivates animal sacrifice, the suspension reading is vindicated; if reinterpretation occurs (study-as-performance doctrine, alternative sacrifice forms), the suspension frame was rhetorical cover for reframing.',
    'If suspension is genuine, the constraint is correctly classified as scaffold (temporary deferral pending reactivation); if it is rhetorical cover for permanent reframing, the constraint is misclassified and should be reclassified as a snare (the frame conceals that the obligation has been silently replaced with study).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether suspension is structural (obligation truly dormant) or rhetorical (obligation reframed as study).').

omega_variable(
    sibling_reading_competition,
    'Which reading of the sacrifice_obligation_continuity kernel is the authoritative interpretation within the Jewish tradition?',
    'Authority is contestable and contestable-forever (the tradition permits simultaneous, non-resolved readings). However, empirical markers include: which reading is taught in mainstream institutions, which is required for legal reasoning, which alternative readings are marginalized or excluded.',
    'This story instantiates the messianic_suspension reading. If study_as_performance or performance_only become dominant, this reading''s authority is downgraded and institutional extractiveness may increase (study institutions gain more direct control). If archival_preservation dominates, the readiness-maintenance burden disappears and extractiveness drops.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_competition, empirical, 'Which reading of the kernel holds institutional authority.').

omega_variable(
    readiness_burden_vs_obligation,
    'Is the study and readiness-maintenance burden genuinely part of the fulfillment of the obligation (obligation-preserving), or is it a performative substitute that allows the community to defer the true obligation indefinitely (extraction masked as readiness)?',
    'Listen to what the community members and authorities say they are doing: are they maintaining readiness for eventual performance, or have they redefined study-as-performance? Post-exit narratives from those who leave the tradition can also clarify whether the readiness burden felt like deferral or substitution.',
    'If readiness is obligation-preserving, extractiveness is moderate (0.48) and the burden is justified; if it is substitution, extractiveness is higher (closer to 0.65-0.75) and the constraint functions as a snare masquerading as a scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(readiness_burden_vs_obligation, empirical, 'Whether readiness maintenance is genuinely preparatory or is a substitute for the obligation.').

omega_variable(
    identity_locked_exit_mechanism,
    'For the covenant community, how binding is the identity-locked exit option? Can a member renounce the obligation by renouncing the covenant identity, or is the covenant identity non-voluntary (binding regardless of choice)?',
    'Historical and ethnographic analysis: can members of the covenant community formally exit the tradition and thereby exit the obligation? If yes, exit is identity_locked but available; if no, it is trapped.',
    'If exit is genuinely available (identity_locked), directionality is moderate and the constraint classifies as a legitimate scaffold for those who choose it; if exit is not available (trapped), directionality is higher and the constraint includes an extractive element even with genuine readiness function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_locked_exit_mechanism, empirical, 'Whether the identity-lock permits exit from the obligation.').

omega_variable(
    institutional_authority_drift,
    'Is study-authority institutional control increasing over time (theater_ratio rising, extractiveness creeping upward), or is the institutional burden stable?',
    'Longitudinal analysis of institutional resource flows, hermeneutic authority concentration, and the ratio of study-maintenance labor to performance-preparation labor over the 2000-year interval.',
    'If institutional control is increasing, the constraint is drifting toward snare (institutions capturing increasing rents from the readiness burden without reactivation pressure); if stable, the deferral function is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_authority_drift, empirical, 'Whether study institutions are capturing increasing rents from the suspension arrangement.').

omega_variable(
    messianic_timing_ambiguity,
    'What counts as the messianic restoration that would reactivate the obligation? Is it a binary event or a process? Can it be contested?',
    'Historical record of messianic claims and how the community responded: did reactivation attempts occur? Were they rejected or accepted?',
    'If messianic restoration is binary and unambiguous, suspension is a clear deferral; if it is ambiguous or processual, the suspension can be indefinitely extended (indefinite deferral becomes indistinguishable from permanent reframing), and the constraint becomes more extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(messianic_timing_ambiguity, conceptual, 'Whether the deferral condition (messianic restoration) is sufficiently determinate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__messianic_suspension, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 500, 0.48).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1000, 0.54).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1500, 0.57).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 2000, 0.58).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 500, 0.42).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1000, 0.46).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1500, 0.47).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 2000, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(sacr_su_t500, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 500, 0.19).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 1000, 0.2).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 1500, 0.21).
narrative_ontology:measurement(sacr_su_t2000, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 2000, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__messianic_suspension, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_continuity__messianic_suspension, 0.12).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity__performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity__archival_preservation).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the sacrifice_obligation_continuity kernel. All four share the same referent (the binding force of the sacrificial law post-Temple-destruction) but instantiate different ε values and beneficiary/victim structures based on which reading is adopted. The messianic_suspension reading (this story) asserts genuine suspension with readiness maintenance; study_as_performance asserts study fulfills the obligation directly; performance_only asserts suspension without readiness language; archival_preservation asserts the obligation is no longer binding. Each reading is a separate constraint story with its own classification and stakeholder structure. They are linked in a kernel family: suspension influences and coexists with the other three, depending on which authority structure is consulted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_obligation_continuity__messianic_suspension, organized, 0.68).
constraint_indexing:directionality_override(sacrifice_obligation_continuity__messianic_suspension, moderate, 0.62).
constraint_indexing:directionality_override(sacrifice_obligation_continuity__messianic_suspension, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
