% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__military_custodian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__military_custodian_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: july_charter_sovereign_legitimacy__military_custodian_reading
 *   human_readable: July Charter — Military Custodianship Reading
 *   domain: constitutional_law/political_transitions/post_revolutionary_state_building
 *
 * SUMMARY:
 *   The July Charter is the founding constitutional document of a
 *   post-revolutionary state. Read through the military-custodian lens, the
 *   Charter does not merely permit but affirmatively ratifies the armed
 *   forces as a permanent institutional guardian holding veto authority over
 *   civilian governance decisions judged destabilizing. This reading treats
 *   the guardian clause as the load-bearing structure of the document:
 *   civilian institutions operate inside a boundary the security apparatus
 *   draws and redraws. This is one of three structurally distinct readings of
 *   the same kernel — the secular_democratic_reading treats the identical
 *   text as mandating military subordination to civilian authority, and the
 *   guided_nationalism_reading grounds sovereignty in religious-national
 *   identity rather than institutional guardianship. The three readings are
 *   not three interpretations of one constraint; they are three different
 *   constraints sharing a contested founding text, each with its own
 *   beneficiary/victim structure and its own epsilon.
 *
 * KEY AGENTS:
 *   - senior_officer_corps: agenda_setter/beneficiary (institutional/arbitrage) — holds the ratified veto
 *   - military_owned_conglomerates: beneficiary (institutional/arbitrage) — commercial rents tied to guardian status
 *   - security_apparatus_leadership: beneficiary/agenda_setter (institutional/arbitrage) — administers boundary enforcement
 *   - autonomous_political_parties: payer (moderate/constrained) — contest office within security-drawn limits
 *   - student_movement_organizers: payer (powerless/trapped) — bear the sharpest enforcement costs
 *   - independent_press: payer (moderate/constrained) — self-censors under licensing and legal pressure
 *   - constitutional_court: observer (institutional/analytical) — reviews within a structure that pre-authorizes what it might check
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__military_custodian_reading, 0.71).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__military_custodian_reading, 0.79).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__military_custodian_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__military_custodian_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__military_custodian_reading, "July Charter — Military Custodianship Reading").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__military_custodian_reading, "constitutional_law/political_transitions/post_revolutionary_state_building").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__military_custodian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__military_custodian_reading, 'ed486c5c-94a4-4b1f-9bec-efb44155d119').
narrative_ontology:cs_kernel_codification('ed486c5c-94a4-4b1f-9bec-efb44155d119', formalized).
narrative_ontology:cs_authority_grounding('ed486c5c-94a4-4b1f-9bec-efb44155d119', extraction).
narrative_ontology:cs_interpretation_layer_present('ed486c5c-94a4-4b1f-9bec-efb44155d119').
narrative_ontology:cs_reading_relation('ed486c5c-94a4-4b1f-9bec-efb44155d119', july_charter_sovereign_legitimacy__secular_democratic_reading, coexists_with).
narrative_ontology:cs_reading_relation('ed486c5c-94a4-4b1f-9bec-efb44155d119', july_charter_sovereign_legitimacy__guided_nationalism_reading, coexists_with).
narrative_ontology:cs_axiom('ed486c5c-94a4-4b1f-9bec-efb44155d119', foundational, military_guardianship_is_sovereign_not_delegated).
narrative_ontology:cs_axiom_status(military_guardianship_is_sovereign_not_delegated, holdable).
narrative_ontology:cs_axiom_grounding('ed486c5c-94a4-4b1f-9bec-efb44155d119', military_guardianship_is_sovereign_not_delegated, conventional).
narrative_ontology:cs_axiom('ed486c5c-94a4-4b1f-9bec-efb44155d119', secondary, stability_designation_overrides_civilian_due_process).
narrative_ontology:cs_axiom_status(stability_designation_overrides_civilian_due_process, holdable).
narrative_ontology:cs_axiom_grounding('ed486c5c-94a4-4b1f-9bec-efb44155d119', stability_designation_overrides_civilian_due_process, instrumental).
narrative_ontology:cs_reference_frame('ed486c5c-94a4-4b1f-9bec-efb44155d119', post_collapse_emergency_guardianship).
narrative_ontology:cs_drift_state('ed486c5c-94a4-4b1f-9bec-efb44155d119', second_decade_post_ratification, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ed486c5c-94a4-4b1f-9bec-efb44155d119', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, senior_officer_corps).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, military_owned_conglomerates).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, security_apparatus_leadership).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_political_parties).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, student_movement_organizers).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, independent_press).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, general_citizenry).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, general_citizenry).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__military_custodian_reading, national_stability_doctrine).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__military_custodian_reading, guardian_state_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sits atop a constitutionally ratified veto structure over legislation, cabinet appointments, and constitutional amendment. Frames its role as guaranteeing continuity and preventing factional collapse. Retains the capacity to suspend civilian decisions it labels destabilizing, and faces no institutional check capable of overturning that judgment.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, senior_officer_corps, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__military_custodian_reading, senior_officer_corps, beneficiary).

% Hold preferential access to state contracts, land grants, and regulatory exemptions that trace directly to the Charter's guardian clause. Their commercial position depends on the same institutional arrangement that grants the officer corps its veto; a civilian-supremacy reading of the Charter would dissolve their preferential access along with the veto.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, military_owned_conglomerates, beneficiary,
    institutional, generational, arbitrage, national).

% Administers the enforcement machinery — surveillance, detention, permit denial — that keeps political contestation inside boundaries the guardian clause defines. Determines in practice which parties and organizers are treated as destabilizing, with the Charter supplying legal cover for actions that would otherwise require judicial or legislative authorization.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, security_apparatus_leadership, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__military_custodian_reading, security_apparatus_leadership, agenda_setter).

% Compete for office within limits set by the guardian clause: registration can be revoked, candidates disqualified, and coalitions dissolved if the security apparatus judges them destabilizing. Cannot appeal past the same institutional structure that made the determination; leaving politics means abandoning the only channel of contestation available.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_political_parties, payer,
    moderate, biographical, constrained, national).

% Mobilize protest and civic organizing that the Charter's stability clause treats as a categorical risk to be pre-empted rather than a legitimate exercise of contestation. Face arrest, expulsion, and blacklisting under authority the Charter provides without requiring the ordinary due-process findings a civilian criminal framework would demand.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, student_movement_organizers, payer,
    powerless, biographical, trapped, local).

% Reports on the guardian clause's operation and faces licensing review, defamation suits routed through security-adjacent courts, and informal pressure on advertisers. Continued operation depends on not triggering the stability designation; self-censorship is the primary observable adaptation.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, independent_press, payer,
    moderate, biographical, constrained, national).

% Receives whatever order and predictability the guardian arrangement genuinely delivers after a period of institutional collapse, while also bearing the diffuse cost of narrowed political choice and periodic crackdowns that ripple beyond their direct targets.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, general_citizenry, beneficiary,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__military_custodian_reading, general_citizenry, payer).

% Reviews challenges to the guardian clause's application but operates within a Charter that pre-authorizes the veto it might otherwise be asked to check, limiting its practical independence from the structure it is nominally positioned to review.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, constitutional_court, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(july_charter_sovereign_legitimacy__military_custodian_reading, senior_officer_corps).
narrative_ontology:fixing_cost_class(july_charter_sovereign_legitimacy__military_custodian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, legally settled locus of authority capable of preventing the fragmentation and factional violence that followed the collapse of the prior order — a real coordination problem in the immediate post-revolutionary period when no civilian institution commanded broad enough trust to arbitrate between contending factions.
% TRANSFER_FUNCTION: Moves effective sovereign authority — veto power over legislation, appointments, and constitutional change, plus commercial preference in state contracting — from elected civilian institutions and the organized public to the officer corps and the conglomerates tied to it.
% ABSENT_VOICES: The dissolved constituent assembly delegates who negotiated the Charter under military oversight and were not permitted to submit a civilian-supremacy alternative to referendum; autonomous party leaders currently under registration review; student organizers held without the due-process findings a civilian court would require.
% DISAPPEARANCE_RATIONALE: If the guardian clause were removed, the veto authority currently held by the officer corps would revert to the elected legislature and courts, military-linked conglomerates would lose their preferential contracting position, and political parties and student organizations currently subject to security-based dissolution or detention would operate under ordinary civilian legal standards instead — a substantial redistribution of who can act and who can be stopped.
% FOUNDING_PROBLEM: The prior civilian government collapsed amid factional violence and institutional breakdown; the Charter was drafted to establish an authority capable of preventing renewed collapse during the transition period.
% FOUNDING_PROBLEM_CORROBORATION: The officer corps and allied constitutional drafters attest the instability risk remains live and cite continuing factional tension as justification. Independent legal scholars, the dissolved constituent assembly's civilian delegates, and comparative-transition researchers outside the military's institutional orbit attest the acute collapse risk that justified the original guardian clause has substantially receded, and that the clause now functions primarily to preserve the officer corps' post-transition institutional position rather than to prevent a recurrence of the founding crisis.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__military_custodian_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__military_custodian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__military_custodian_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__military_custodian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__military_custodian_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__military_custodian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__military_custodian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__military_custodian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 to 0.71 over the interval as the guardian clause's application shifts from crisis-era emergency use toward routine political management — party disqualifications, press licensing actions, and student detentions accumulate as the acute post-revolutionary collapse risk recedes. Theater ratio climbs alongside it (0.18 to 0.42) as security actions increasingly perform stability-maintenance rather than respond to it. Suppression is authored as a raw structural property, rising independently as enforcement infrastructure (registration review boards, security courts, licensing bodies) matures and hardens — this is not scaled by scope or power; it is the constraint's own coercive architecture solidifying.
 *
 * PERSPECTIVAL GAP:
 *   From the officer corps' seat, the Charter is coordination: a necessary continuity mechanism preventing renewed collapse. From the student organizer's seat, the identical clause is enforced extraction with no available appeal. The engine should compute divergent per-seat types from this same structural data — that divergence is exactly what distinguishes this reading from a genuine emergency-coordination scaffold, and is the measurement the story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The officer corps, military-linked conglomerates, and security leadership sit near the full-beneficiary end: they collect the veto authority and its commercial and institutional spillovers, and their exit options are effectively arbitrage-grade (they can reshape the rules they operate under). Autonomous parties, student organizers, and independent press sit near the full-target end: they bear the transfer directly, cannot appeal past the structure that judges them, and have constrained or trapped exit. The general citizenry sits closer to symmetric — real order-provision benefit against diffuse cost from narrowed contestation and periodic crackdown exposure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — acute post-collapse factional violence — was real at ratification but the corroboration record is contested: independent legal scholars and the excluded constituent-assembly delegates attest the acute risk has substantially receded while the guardian clause persists and has hardened (rising suppression_requirement, rising theater_ratio). This is the classic mandatrophy signature: a mandate whose founding function has decayed while its institutional apparatus continues to operate, now serving a partly different function (institutional self-preservation and commercial rent-collection) than the one that justified its creation. Classifying this as tangled_rope rather than snare preserves the historically real coordination function at founding while flagging the asymmetric extraction that has grown alongside it — collapsing it to pure snare would erase the genuine crisis-era coordination basis; leaving it at rope would erase the victim set the metrics document.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    military_custodian_kernel_reading_scope,
    'Is the military-custodian reading the textually dominant reading of the July Charter, or is it one contested reading among three that different factions invoke selectively depending on the political moment?',
    'Comparative textual and drafting-history analysis: examine constituent assembly records, minority drafting objections, and subsequent judicial citation patterns to determine whether the guardian clause was drafted as the operative supremacy clause or as a bounded transitional security provision later expanded through practice rather than text.',
    'If the drafting record supports the custodian reading as textually dominant, this constraint''s classification as the primary operative reading strengthens; if the record shows the custodian reading emerged through post-ratification practice diverging from drafted intent, the secular_democratic_reading''s claim to textual fidelity strengthens correspondingly, and this reading is better characterized as a captured/drifted interpretation rather than a faithful one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_custodian_kernel_reading_scope, conceptual, 'Whether the military-custodian reading is the Charter''s dominant textual reading or a contested, practice-driven expansion.').

omega_variable(
    guardian_clause_natural_vs_constructed,
    'Is the guardian clause''s persistence a natural consequence of genuine, ongoing institutional fragility (a real coordination necessity), or a constructed arrangement maintained because it benefits identifiable institutional actors regardless of whether the fragility persists?',
    'Track independent measures of institutional fragility (electoral administration capacity, judicial independence indices, factional violence incidence) against the guardian clause''s enforcement intensity over the same interval; divergence between falling fragility and rising enforcement would support the constructed reading.',
    'Resolving toward ''constructed'' would support reclassifying portions of this constraint toward snare as the coordination justification thins further; resolving toward ''natural necessity'' would support a scaffold reading contingent on an eventual sunset that has not yet been authored into the Charter''s text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guardian_clause_natural_vs_constructed, empirical, 'Whether the guardian clause tracks genuine ongoing instability or has decoupled from it.').

omega_variable(
    sibling_reading_foreclosure_test,
    'Does the military-custodian reading''s core premise (standing institutional veto) logically foreclose the secular_democratic_reading''s core premise (military subordination to civilian authority) within a single constitutional framework, or can both be held as contested interpretations of an underdetermined text simultaneously?',
    'Formal analysis of whether the Charter''s text contains an explicit supremacy ordering between the guardian clause and the civilian-authority clauses, versus genuine textual ambiguity that different courts or political coalitions could resolve either way without amendment.',
    'If the text genuinely forecloses one reading, the reading_relations should record ''forecloses'' rather than ''coexists_with''; this story currently treats the readings as coexisting because no single adjudicating body has settled the ordering and both readings are actively held by different factions in practice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_test, conceptual, 'Whether the custodian and democratic readings are logically incompatible or merely factionally contested.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__military_custodian_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(july_tr_t8, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(july_tr_t16, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(july_tr_t24, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(july_tr_t32, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(july_tr_t40, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(july_be_t8, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(july_be_t16, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(july_be_t24, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(july_be_t32, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 32, 0.69).
narrative_ontology:measurement(july_be_t40, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 40, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(july_su_t8, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(july_su_t16, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(july_su_t24, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(july_su_t32, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 32, 0.76).
narrative_ontology:measurement(july_su_t40, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 40, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__military_custodian_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy__secular_democratic_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy__guided_nationalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the july_charter_sovereign_legitimacy kernel, decomposed per the epsilon-invariance principle because the three readings produce structurally distinct beneficiary/victim sets and distinct extraction profiles from the same founding text. secular_democratic_reading treats the guardian clause as transitional and subordinate to civilian authority (low sustained extraction, scaffold-leaning); guided_nationalism_reading grounds legitimacy in religious-national identity rather than institutional guardianship (different beneficiary structure entirely, centered on religious-nationalist authorities rather than the officer corps); military_custodian_reading (this story) treats the guardian clause as the Charter's operative supremacy structure, producing substantial and rising extraction concentrated on autonomous political parties, student organizers, and independent press. All three link via affects_constraints; none averages the others' epsilon into itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
