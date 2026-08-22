% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__christianized_pacification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__christianized_pacification_reading, []).

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
 *   constraint_id: feud_obligation_kernel__christianized_pacification_reading
 *   human_readable: Christianized Pacification Monopoly on Legitimate Violence
 *   domain: legal_anthropology/medieval_history/political_systems
 *
 * SUMMARY:
 *   This constraint instantiates the christianized_pacification_reading of
 *   the feud_obligation_kernel. The standing arrangement is the medieval
 *   Church's claimâbacked by penitential discipline and royal
 *   delegationâthat blood-feud obligations violate divine law and that
 *   legitimate violence authority resides solely with God and his delegated
 *   ecclesiastical/royal institutions. The constraint operates by converting
 *   kinship-based vengeance from a customary legal duty into a sin requiring
 *   Church adjudication, thereby expanding ecclesiastical jurisdiction while
 *   ostensibly pacifying Christian territory.
 *
 * KEY AGENTS:
 *   - avenging_kinship_groups (payer, identity-locked) â bear spiritual peril, penance, and honor loss
 *   - ecclesiastical_hierarchy (agenda_setter/beneficiary, institutional) â administers penance and claims interpretive monopoly
 *   - royal_authority (beneficiary, institutional) â receives delegated legitimacy for territorial adjudication
 *   - local_customary_elites (excluded, moderate) â previously managed settlement, now silenced
 *   - canon_law_jurists (observer, organized) â provide theological-legal justification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, 0.78).
domain_priors:suppression_score(feud_obligation_kernel__christianized_pacification_reading, 0.88).
domain_priors:theater_ratio(feud_obligation_kernel__christianized_pacification_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__christianized_pacification_reading, tangled_rope).
narrative_ontology:human_readable(feud_obligation_kernel__christianized_pacification_reading, "Christianized Pacification Monopoly on Legitimate Violence").
narrative_ontology:topic_domain(feud_obligation_kernel__christianized_pacification_reading, "legal_anthropology/medieval_history/political_systems").

domain_priors:requires_active_enforcement(feud_obligation_kernel__christianized_pacification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__christianized_pacification_reading, 'f5439982-2f24-4e95-9953-105c26f8be0f').
narrative_ontology:cs_kernel_codification('f5439982-2f24-4e95-9953-105c26f8be0f', formalized).
narrative_ontology:cs_authority_grounding('f5439982-2f24-4e95-9953-105c26f8be0f', lineage).
narrative_ontology:cs_interpretation_layer_present('f5439982-2f24-4e95-9953-105c26f8be0f').
narrative_ontology:cs_reading_relation('f5439982-2f24-4e95-9953-105c26f8be0f', feud_obligation_kernel__stateless_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('f5439982-2f24-4e95-9953-105c26f8be0f', feud_obligation_kernel__extraction_cycle_reading, influences).
narrative_ontology:cs_axiom('f5439982-2f24-4e95-9953-105c26f8be0f', foundational, divine_prohibition_of_vengeance).
narrative_ontology:cs_axiom_status(divine_prohibition_of_vengeance, holdable).
narrative_ontology:cs_axiom_grounding('f5439982-2f24-4e95-9953-105c26f8be0f', divine_prohibition_of_vengeance, theological).
narrative_ontology:cs_axiom('f5439982-2f24-4e95-9953-105c26f8be0f', foundational, ecclesiastical_violence_delegation).
narrative_ontology:cs_axiom_status(ecclesiastical_violence_delegation, holdable).
narrative_ontology:cs_axiom_grounding('f5439982-2f24-4e95-9953-105c26f8be0f', ecclesiastical_violence_delegation, theological).
narrative_ontology:cs_reference_frame('f5439982-2f24-4e95-9953-105c26f8be0f', divine_vengeance_monopoly).
narrative_ontology:cs_drift_state('f5439982-2f24-4e95-9953-105c26f8be0f', medieval_institutional_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f5439982-2f24-4e95-9953-105c26f8be0f', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_hierarchy).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, royal_authority).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, avenging_kinship_groups).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__christianized_pacification_reading, divine_vengeance_monopoly).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_jurisdictional_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bound by kinship obligation to avenge homicides and grave injuries. Under the christianized pacification regime, fulfilling these obligations incurs excommunication, penitential fines, and declared spiritual peril. Their customary law and honor code are recoded as sin, trapping them between identity-fused duty and salvation anxiety.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, avenging_kinship_groups, payer,
    moderate, biographical, identity_locked, regional).

% Administers penitential discipline and claims interpretive monopoly on legitimate violence. Expands jurisdictional reach by adjudicating disputes previously handled by kinship groups. Collects penitential revenue and moral authority while delegating some enforcement capacity to royal partners.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_hierarchy, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_hierarchy, beneficiary).

% Receives delegated legitimacy for violence from the Church's theological framework. Uses ecclesiastical endorsement to consolidate territorial adjudication and weaken autonomous kinship jurisdictions. Benefits from pacification but remains partially checked by noble customary privilege.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, royal_authority, beneficiary,
    institutional, generational, mobile, national).

% Elders and kinship leaders who previously managed feud settlement through customary law. Their voices are excluded from penitential manuals and conciliar decrees; their practices are reclassified as sinful vengeance rather than legitimate legal process.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, local_customary_elites, excluded,
    moderate, biographical, constrained, regional).

% Develop and interpret the penitential codes, conciliar decrees, and theological arguments that classify feud violence as usurpation of divine prerogative. Provide the intellectual machinery for enforcement without themselves bearing its costs or collecting its jurisdictional rents.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, canon_law_jurists, observer,
    organized, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_hierarchy).
narrative_ontology:fixing_cost_class(feud_obligation_kernel__christianized_pacification_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaces decentralized kin-based vengeance with a centralized sacred order in which legitimate violence is monopolized by divine and delegated ecclesiastical/royal authority, reducing chronic bloodshed and territorial instability.
% TRANSFER_FUNCTION: Transfers jurisdiction over homicide and injury disputes from kinship groups to ecclesiastical courts and royal adjudication; transfers material wealth and honor from avenging kin to the Church through penitential fines, donations, and jurisdictional fees.
% ABSENT_VOICES: Local customary-law elders and kinship leaders who previously managed feud settlement are structurally excluded from penitential manuals and conciliar decrees; their frameworks are recoded as sin rather than law, and they have no seat in the theological discourse that determines their status.
% DISAPPEARANCE_RATIONALE: If the divine-law prohibition and penitential enforcement vanished overnight, kinship obligations to avenge would resurge, ecclesiastical jurisdiction over violence would contract, and the Church's interpretive monopoly would fracture as secular and customary law reclaimed authority over dispute resolution.
% FOUNDING_PROBLEM: Chronic blood-feuding among the faithful destabilizes Christian territories, depletes the lay nobility, and usurps God's exclusive right to vengeance.
% FOUNDING_PROBLEM_CORROBORATION: Secular chroniclers and lay noble complaint records attest to the material and demographic costs of feuding from outside the Church's benefiting circle; archaeological evidence of violent death patterns corroborates the scale of pre-pacification violence, though the causal attribution to kinship obligation alone is contested.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__christianized_pacification_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__christianized_pacification_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__christianized_pacification_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(feud_obligation_kernel__christianized_pacification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__christianized_pacification_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__christianized_pacification_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__christianized_pacification_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the Church captures substantial jurisdictional reach, interpretive monopoly, and penitential revenue; suppression is higher (0.88) because the arrangement depends on active penitential discipline and threat of excommunication to suppress customary vengeance. Accessibility collapse is very high (0.85): once the divine-law frame is accepted, kin vengeance becomes spiritually unthinkable, collapsing customary alternatives. Resistance is moderate (0.55): feud customs persist sub rosa and resurface during weak enforcement phases, but open resistance is suppressed by spiritual terror. Theater ratio rises to 0.50 as the Church's enforcement apparatus matures and an increasing share of activity becomes performative maintenance of authority rather than functional pacification.
 *
 * PERSPECTIVAL GAP:
 *   The ecclesiastical seat perceives a divine order restoring peace and salvaging souls; the avenging kin seat perceives spiritual extortion that criminalizes their honor obligations and channels their wealth into Church coffers. The engine computes this divergence from the structural asymmetry: one seat collects jurisdiction and revenue while the other bears spiritual and material cost under identity-locked exit.
 *
 * DIRECTIONALITY LOGIC:
 *   The ecclesiastical hierarchy is the primary beneficiary (low d): it collects interpretive monopoly, expanded jurisdiction, and penitential revenue. Royal authority is a secondary beneficiary (low-moderate d): it gains delegated legitimacy but remains partially independent. Avenging kinship groups are the primary targets (high d): they bear the extraction directly through penance and honor loss, with identity-locked exit amplifying effective extraction. Local customary elites are excluded rather than coordinated; their exclusion is the enforcement precondition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâchronic blood-feudingâwas materially live in the early medieval period, so the constraint is not a clear case of mandatrophy within the authored interval. However, the reading contains the seeds of later mandatrophy: as royal institutions mature and develop autonomous enforcement capacity, the Church's divine-law mediation may outlive its functional necessity, becoming theatrical maintenance of jurisdictional claims. The Tangled Rope classification prevents mislabeling the genuine pacification coordination as pure extraction, while acknowledging that the same institutional structure channels substantial asymmetric benefit to the Church.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer,
    'This constraint is one reading of the feud_obligation_kernel; how would classification change if the stateless_coordination_reading or extraction_cycle_reading were adopted instead?',
    'Compare the three compiled constraint stories in the kernel family; divergence in beneficiary/victim structure and Îµ values measures the kernel''s structural under-determination.',
    'Adopting the stateless_coordination_reading would flip the victim/beneficiary structure and lower Îµ substantially; adopting the extraction_cycle_reading would retain high extraction but shift the beneficiary to territorial consolidators and remove the coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer, conceptual, 'Kernel reading committer frame for feud obligation').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (penitential fines, excommunication, royal enforcement) or internalized (genuine fear of hell, identity fusion with Christian pacifism)?',
    'Post-exit suppression trajectory: if avenging kin continue to experience prohibition anxiety after structural enforcement is removed, reclassify as partially internalized.',
    'If internalized, effective suppression exceeds the structural measure; the constraint operates as cognitive capture rather than merely institutional coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    coordination_extraction_boundary,
    'Is the Church''s monopoly on legitimate violence structurally separable from the genuine pacification function, or is the extraction inseparable from the coordination?',
    'Comparative analysis of regions where royal authority asserted violence monopoly without ecclesiastical mediation: if pacification holds without Church jurisdictional expansion, the functions are separable.',
    'If separable, the constraint is extraction riding on genuine coordination; if inseparable, the extraction is the necessary cost of the coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Coordination and extraction separability in divine-law monopoly').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__christianized_pacification_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(feud_tr_t20, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(feud_tr_t40, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(feud_tr_t60, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 60, 0.45).
narrative_ontology:measurement(feud_tr_t80, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 80, 0.48).
narrative_ontology:measurement(feud_tr_t100, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 100, 0.5).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(feud_be_t20, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(feud_be_t40, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(feud_be_t60, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 60, 0.72).
narrative_ontology:measurement(feud_be_t80, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 80, 0.75).
narrative_ontology:measurement(feud_be_t100, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 100, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(feud_su_t20, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(feud_su_t40, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 40, 0.78).
narrative_ontology:measurement(feud_su_t60, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 60, 0.82).
narrative_ontology:measurement(feud_su_t80, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 80, 0.85).
narrative_ontology:measurement(feud_su_t100, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 100, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__christianized_pacification_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, stateless_coordination_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, extraction_cycle_reading).

% DUAL FORMULATION NOTE:
% The feud_obligation_kernel decomposes into three structurally distinct constraints: stateless_coordination_reading (self-enforcing justice), extraction_cycle_reading (destructive depletion), and christianized_pacification_reading (divine-law monopoly). Each carries a distinct Îµ, beneficiary/victim structure, and classification. Network edges link the family members for contamination and coupling analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
