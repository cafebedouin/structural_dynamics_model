% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__incoherence_reading, []).

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
 *   constraint_id: shinbutsu_ontological_commitment__incoherence_reading
 *   human_readable: Shinbutsu-shugo as Institutionally Tolerated Ontological Incoherence
 *   domain: religious_studies/japanese_history/ontology_of_practice
 *
 * SUMMARY:
 *   This story instantiates the 'incoherence' reading of the shinbutsu-shugo
 *   kernel: across the medieval and Edo periods, no stable ontological
 *   commitment ever fixed the relationship between kami and buddhas within
 *   combinatory institutions. Practice accommodated multiple, mutually
 *   inconsistent framings (identity, emanation, mutual dependence,
 *   separateness-in-fact) simultaneously, and the institutional apparatus
 *   tolerated this rather than resolving it, because resolving it was not
 *   necessary for the coordination the institutions actually needed to
 *   perform. The theater_ratio rises over the interval because as the
 *   practical arrangement matured, the performative gestures toward doctrinal
 *   coherence (honji-suijaku formulas cited in liturgy, ritual texts
 *   asserting unity) increasingly outran any actual settled commitment behind
 *   them — the language of unity persisted long after any functional need to
 *   mean it. This reading treats the standing arrangement (tolerated
 *   incoherence) as the referent for epsilon, not the state-building outcome
 *   Meiji separation eventually produced.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__incoherence_reading, 0.42).
domain_priors:suppression_score(shinbutsu_ontological_commitment__incoherence_reading, 0.38).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__incoherence_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__incoherence_reading, piton).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__incoherence_reading, "Shinbutsu-shugo as Institutionally Tolerated Ontological Incoherence").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__incoherence_reading, "religious_studies/japanese_history/ontology_of_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__incoherence_reading, 'c39e4666-f86f-4731-9367-ae855a84acaf').
narrative_ontology:cs_kernel_codification('c39e4666-f86f-4731-9367-ae855a84acaf', distributed).
narrative_ontology:cs_authority_grounding('c39e4666-f86f-4731-9367-ae855a84acaf', practice).
narrative_ontology:cs_interpretation_layer_present('c39e4666-f86f-4731-9367-ae855a84acaf').
narrative_ontology:cs_reading_relation('c39e4666-f86f-4731-9367-ae855a84acaf', shinbutsu_ontological_commitment__syncretic_reading, coexists_with).
narrative_ontology:cs_reading_relation('c39e4666-f86f-4731-9367-ae855a84acaf', shinbutsu_ontological_commitment__partition_reading, coexists_with).
narrative_ontology:cs_axiom('c39e4666-f86f-4731-9367-ae855a84acaf', foundational, no_stable_kami_buddha_ontological_settlement_existed).
narrative_ontology:cs_axiom_status(no_stable_kami_buddha_ontological_settlement_existed, holdable).
narrative_ontology:cs_axiom_grounding('c39e4666-f86f-4731-9367-ae855a84acaf', no_stable_kami_buddha_ontological_settlement_existed, empirically_contingent).
narrative_ontology:cs_axiom('c39e4666-f86f-4731-9367-ae855a84acaf', secondary, institutional_toleration_of_metaphysical_inconsistency_is_the_operative_structure).
narrative_ontology:cs_axiom_status(institutional_toleration_of_metaphysical_inconsistency_is_the_operative_structure, holdable).
narrative_ontology:cs_axiom_grounding('c39e4666-f86f-4731-9367-ae855a84acaf', institutional_toleration_of_metaphysical_inconsistency_is_the_operative_structure, empirically_contingent).
narrative_ontology:cs_reference_frame('c39e4666-f86f-4731-9367-ae855a84acaf', pre_meiji_combinatory_practice_without_doctrinal_settlement).
narrative_ontology:cs_drift_state('c39e4666-f86f-4731-9367-ae855a84acaf', meiji_shinbutsu_bunri_1868, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('c39e4666-f86f-4731-9367-ae855a84acaf', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, shrine_temple_administrative_complexes).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, local_ritual_specialists).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, doctrinal_clarity_seekers).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, meiji_state_builders_ex_post).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, meiji_state_builders_ex_post).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__incoherence_reading, pragmatic_syncretism_without_metaphysical_settlement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jingu-ji and combinatory shrine-temple complexes operate joint ritual calendars, landholdings, and personnel without ever resolving whether kami are buddhas, emanations of buddhas, or something else. The absence of a settled metaphysics lets them absorb whichever framing a given patron, domain, or moment requires; a forced ontological settlement would require redividing property, personnel, and ritual authority.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, shrine_temple_administrative_complexes, beneficiary,
    organized, generational, constrained, national).

% Shugenja, shrine priests, and temple clergy who move between kami ritual and Buddhist liturgy as occasion demands. They administer the day-to-day toleration of incoherence — performing whichever rite the situation calls for without being required to reconcile the underlying claims. Their livelihood depends on the flexibility the unsettled ontology provides.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, local_ritual_specialists, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__incoherence_reading, local_ritual_specialists, agenda_setter).

% Individual practitioners, scholar-monks, and Confucian-influenced reformers across the Edo period who wanted a coherent answer to what kami actually are relative to buddhas. They repeatedly raised the incoherence as a problem (Yoshida Shinto's counter-systematization, kokugaku critiques) and were absorbed back into institutional practice or marginalized rather than given resolution; the institutional structure had no mechanism to settle their question because settling it was not what the arrangement was for.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, doctrinal_clarity_seekers, payer,
    powerless, biographical, trapped, regional).

% Retrospectively, the Meiji state found the unsettled ontology a liability for building a unified national Shinto separated from Buddhism (shinbutsu bunri, 1868). They paid a real cost in that no clean doctrinal seam existed to cut along — centuries of joint property, ritual, and personnel had to be forcibly disentangled, producing violence against Buddhist institutions (haibutsu kishaku). But the same absence of settled metaphysics also meant no entrenched counter-doctrine existed to resist the state's new framing, which benefited the project of rapid reconstruction once underway.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, meiji_state_builders_ex_post, payer,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__incoherence_reading, meiji_state_builders_ex_post, beneficiary).

% Clergy of combinatory institutions who, at the moment of forced separation, had no doctrinal record establishing their side of the arrangement as ontologically primary or even coherent — the toleration of incoherence left them without grounds to contest dispossession when the state later insisted on a separation the tradition itself had never required them to defend.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, buddhist_clergy_displaced_at_bunri, excluded,
    powerless, biographical, trapped, regional).

% Later historians and religious studies scholars examining the documentary record to determine whether shinbutsu-shugo reflects genuine metaphysical synthesis, functional partition, or the absence of any stable commitment at all. This reading is their analytical conclusion: the sources show accommodation without resolution.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_commitment__incoherence_reading, diffuse).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_commitment__incoherence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Institutional actors across shrine-temple complexes needed to conduct joint ritual, administrative, and economic life without first resolving a metaphysical dispute that would have fractured that cooperation. Tolerating incoherence let ritual specialists perform whichever framing served the occasion, avoiding a costly doctrinal settlement that no faction had the power or need to force.
% TRANSFER_FUNCTION: The arrangement transfers interpretive burden away from any single authoritative voice and onto local, occasion-specific practice — no metaphysical consensus is produced or paid for by any party; instead, the cost of non-settlement is deferred and eventually falls, sharply and retroactively, on Buddhist clergy at Meiji separation and on anyone across the centuries who wanted doctrinal clarity and got institutional absorption instead.
% ABSENT_VOICES: Voices demanding systematic ontological resolution — Yoshida Shinto systematizers, kokugaku scholars, and rigorist Buddhist doctrinal schools — repeatedly surface in the historical record and are repeatedly folded back into practical accommodation rather than answered. They are in the documentary record but not in the decision-making that perpetuates the toleration.
% DISAPPEARANCE_RATIONALE: If institutional toleration of ontological incoherence had not existed — if shrine-temple complexes had been forced at any point in the Edo period to settle whether kami and buddhas were identical, separate, or hierarchically related — the entire administrative, ritual, and property structure of combinatory religion would have had to reorganize around whichever settlement won, and the later Meiji separation would have proceeded along an existing seam rather than cutting a new one through entangled institutions.
% FOUNDING_PROBLEM: Heterogeneous local kami cults and an imported Buddhist tradition needed to coexist within shared ritual sites, patronage networks, and state recognition systems without a central authority capable of, or interested in, forcing metaphysical uniformity.
% FOUNDING_PROBLEM_CORROBORATION: Meiji-era state Shinto ideologues attest the problem is dead and resolved by declaring kami ontologically primary and separate from Buddhism — but this attestation comes from the beneficiary of the new settlement. Independent corroboration comes from historians of religion (e.g. studies of jingu-ji institutional records and haibutsu kishaku documentation) who read the absence of pre-Meiji doctrinal settlement as evidence the founding coordination problem was solved practically, never metaphysically, and that its 'death' in 1868 was an imposed political fact rather than an internal theological resolution.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__incoherence_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__incoherence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__incoherence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__incoherence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__incoherence_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__incoherence_reading_tests).
:- end_tests(shinbutsu_ontological_commitment__incoherence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the toleration of incoherence redistributes interpretive labor and forecloses doctrinal accountability rather than directly extracting material resources — but it does extract something real: it externalizes the cost of non-settlement onto whoever eventually needs a clean answer (doctrinal seekers denied resolution across centuries; Meiji state-builders and Buddhist clergy who pay the cost of forced separation without a pre-existing seam). Suppression is moderate (0.38): there is no active enforcement apparatus suppressing alternative ontological claims, but repeated absorption of systematizing movements (Yoshida Shinto, kokugaku) back into practical accommodation functions as a soft suppression of resolution-seeking. Accessibility collapse is deliberately kept lower (0.35) because alternatives to the incoherent arrangement were never fully foreclosed — the record shows repeated, serious attempts at systematization; they simply never won institutional purchase, which is different from being made inaccessible.
 *
 * DIRECTIONALITY LOGIC:
 *   Shrine-temple administrative complexes and local ritual specialists sit near the beneficiary end: they collect the flexibility of the unsettled ontology and would bear real reorganization costs if it were settled either way. Doctrinal clarity seekers sit near the target end: trapped inside an institutional structure with no mechanism to answer their question, they pay in unresolved uncertainty across generations with no exit — the tradition simply had nowhere else to go. Meiji state-builders are the most structurally interesting case: I give them dual role because they pay the cost of the incoherence (no clean separation seam) but ultimately benefit from the absence of an entrenched countervailing doctrine once they impose their own. Buddhist clergy displaced at bunri are excluded rather than payers in the ordinary sense — their exclusion from doctrinal self-defense at the critical moment is itself a downstream effect of the toleration.
 *
 * MANDATROPHY ANALYSIS:
 *   The 'founding problem' (coordinating heterogeneous cults and an imported tradition without central doctrinal authority) is dead by 1868 in the specific sense that the state supplied an external, political resolution — but the institutional arrangement itself never resolved anything; it was simply supplanted. This prevents mislabeling the pre-Meiji arrangement as either successful coordination (it never solved the ontological question it is sometimes retroactively credited with solving, e.g. honji-suijaku as achieved synthesis) or as pure extraction (no concentrated beneficiary captured rents from the incoherence; the benefit was diffuse institutional flexibility). The piton classification captures this: the constraint persisted by inertia, administered without any single agenda-setter who could resolve it and without a payer hurt badly enough, in real time, to force resolution — until an external actor (the Meiji state) unilaterally ended the toleration for its own reasons.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incoherence_vs_synthesis_vs_partition,
    'Is the honji-suijaku textual record best read as evidence of genuine (if locally variable) metaphysical synthesis, as evidence of stable functional partition between life-cycle and afterlife domains, or as evidence that no stable ontological commitment existed at all and institutions simply tolerated the resulting incoherence?',
    'Systematic comparison of doctrinal texts, ritual manuals, and property/administrative records across regions and centuries for internal consistency in kami-buddha relational claims; convergence toward a single stable claim across sites and time would favor the syncretic or partition reading, while persistent site-specific and occasion-specific inconsistency favors this incoherence reading.',
    'If the record supports genuine synthesis or stable partition, this constraint''s beneficiary/victim structure dissolves — there would be a real settled claim being coordinated around rather than an absence being tolerated, and the piton classification would likely shift toward rope (successful, low-extraction coordination around a genuine synthesis) or scaffold (a stable functional division awaiting eventual formalization).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incoherence_vs_synthesis_vs_partition, conceptual, 'Whether the historical record supports incoherence, synthesis, or partition as the true structure of shinbutsu-shugo.').

omega_variable(
    meiji_benefit_retrospective_framing,
    'Did the absence of settled pre-Meiji doctrine genuinely benefit Meiji state-builders by removing entrenched resistance, or does this benefit only appear real because the state''s success in separation is being read backward into the prior arrangement''s function?',
    'Comparative analysis of separation processes in regions/domains where combinatory institutions had produced stronger local doctrinal settlements versus regions where incoherence was more thoroughgoing — differential resistance to bunri would indicate whether settled doctrine actually impeded state action.',
    'If stronger prior doctrinal settlement predicts stronger resistance to Meiji separation, the ex-post benefit to state-builders is real and structural, supporting the dual beneficiary/payer role assigned here. If resistance was uniform regardless of prior doctrinal clarity, the benefit attribution is largely a retrospective narrative and should be weakened or removed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(meiji_benefit_retrospective_framing, empirical, 'Whether Meiji state-building actually benefited from prior ontological incoherence or whether this is retrospective narrative construction.').

omega_variable(
    reading_framing_undetermination,
    'Given that all three kernel readings (incoherence, partition, syncretic) can each cite portions of the same textual and institutional record, is the choice among them primarily an empirical historiographic question or a framing choice driven by which modern scholarly tradition (comparative religion skepticism toward folk-syncretism narratives vs. religious-studies attention to honji-suijaku as systematic theology vs. functionalist anthropology of ritual domains) the observer inherits?',
    'This is likely irreducibly conceptual: no single archival find would settle it, since the underlying practice may genuinely have varied by site, period, and social class in ways that make all three readings locally true and none universally true.',
    'If the choice is substantially framing-driven, this reading''s claimed_type (piton) and its metrics are best understood as one coherent, internally consistent analytical stance rather than as the single correct historical verdict — the sibling readings are not errors this reading corrects, but genuinely different constraints instantiated from the same ambiguous historical material.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_undetermination, conceptual, 'Whether the three kernel readings reflect genuine regional/temporal variation or an irreducible framing choice by the observer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__incoherence_reading, 0, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement_basis(shin_tr_t0, projected).
narrative_ontology:measurement(shin_tr_t400, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 400, 0.4).
narrative_ontology:measurement_basis(shin_tr_t400, projected).
narrative_ontology:measurement(shin_tr_t800, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 800, 0.48).
narrative_ontology:measurement_basis(shin_tr_t800, projected).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1200, 0.55).
narrative_ontology:measurement_basis(shin_tr_t1200, observed).
narrative_ontology:measurement(shin_tr_t1600, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1600, 0.58).
narrative_ontology:measurement_basis(shin_tr_t1600, observed).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1868, 0.61).
narrative_ontology:measurement_basis(shin_tr_t1868, observed).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement_basis(shin_be_t0, projected).
narrative_ontology:measurement(shin_be_t400, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 400, 0.28).
narrative_ontology:measurement_basis(shin_be_t400, projected).
narrative_ontology:measurement(shin_be_t800, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 800, 0.33).
narrative_ontology:measurement_basis(shin_be_t800, projected).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1200, 0.37).
narrative_ontology:measurement_basis(shin_be_t1200, observed).
narrative_ontology:measurement(shin_be_t1600, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1600, 0.4).
narrative_ontology:measurement_basis(shin_be_t1600, observed).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1868, 0.42).
narrative_ontology:measurement_basis(shin_be_t1868, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(shinbutsu_ontological_commitment__incoherence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__incoherence_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__incoherence_reading, meiji_shinbutsu_bunri_separation_policy).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the shinbutsu_ontological_commitment kernel (incoherence_reading, partition_reading, syncretic_reading). Each reading shares the same historical practice record but authors a different structural claim about what that record shows, with different ontological commitments, different beneficiary/victim structures, and different classifications. This story (incoherence_reading) is linked forward to meiji_shinbutsu_bunri_separation_policy because the ex-post cost/benefit to Meiji state-builders analyzed here depends on the specific claim that no prior settlement existed to be disentangled cleanly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
