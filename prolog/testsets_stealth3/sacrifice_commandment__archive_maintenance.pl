% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__archive_maintenance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__archive_maintenance, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: sacrifice_commandment__archive_maintenance
 *   human_readable: Kodashim Study as Restoration Archive (Archive-Maintenance Reading)
 *   domain: religious/halakhic/commitment-system
 *
 * SUMMARY:
 *   A dispersed religious community maintains continuous, institutionally
 *   organized study of its ancient sacrificial code — tractates of ritual
 *   procedure, measurements, disqualifications, and sanctuary architecture —
 *   on a single justification: the knowledge must survive intact until the
 *   Temple is restored and the service resumes. This story instantiates the
 *   archive_maintenance reading of the sacrifice_commandment kernel, in which
 *   that study is messianic preparation and explicitly NOT present worship:
 *   its entire value is deferred to a restoration no one can schedule. The
 *   standing arrangement under contest is therefore a cross-temporal transfer
 *   — present students and funders supply years of attention and budget; a
 *   contingent future generation collects the payoff if the theological
 *   premise holds. Per the epsilon-invariance principle, the colloquial label
 *   'studying the sacrifices' decomposes into three structurally distinct
 *   constraints (this reading; study_as_performance, where study fulfills the
 *   obligation now; performance_only, where the commandment is suspended and
 *   study fulfills nothing), linked in network.affects_constraints; this
 *   file's epsilon (0.55, rising) is authored for THIS reading's arrangement
 *   only, and the sibling files would author different values over their own
 *   arrangements. Claim and metrics are independent: the tangled_rope claim
 *   states what I take the structure to be; the metrics describe its observed
 *   operation. KEY AGENTS (by structural relationship): -
 *   kodashim_curriculum_authorities: agenda-setting seat
 *   (institutional/constrained) — assigns the study, articulates the
 *   preservation rationale, bears negligible cost -
 *   present_day_kodashim_students: primary present-tense payer
 *   (moderate/constrained) — years of study time for a payoff they may never
 *   see - future_restoration_generation: declared beneficiary
 *   (powerless/trapped) — collects the entire payoff if the premise holds;
 *   cannot consent or object - temple_preparatory_institutes: present-tense
 *   collector and co-administrator (organized/identity_locked) — funding,
 *   staffing, and standing flow to them now - community_education_funders:
 *   resource payer (organized/mobile) — budgets allocated on the restoration
 *   hope; the easiest exit in the structure - messianic_activist_networks:
 *   frame beneficiary and pressure arm (organized/identity_locked) -
 *   halakhic_skeptics_within_tradition: excluded voice (moderate/constrained)
 *   — objections archived in journals, not curricula -
 *   comparative_religion_analysts: analytical observer
 *   (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__archive_maintenance, 0.55).
domain_priors:suppression_score(sacrifice_commandment__archive_maintenance, 0.42).
domain_priors:theater_ratio(sacrifice_commandment__archive_maintenance, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, extractiveness, 0.55).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, resistance, 0.32).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__archive_maintenance, tangled_rope).
narrative_ontology:human_readable(sacrifice_commandment__archive_maintenance, "Kodashim Study as Restoration Archive (Archive-Maintenance Reading)").
narrative_ontology:topic_domain(sacrifice_commandment__archive_maintenance, "religious/halakhic/commitment-system").

domain_priors:requires_active_enforcement(sacrifice_commandment__archive_maintenance).
narrative_ontology:has_sunset_clause(sacrifice_commandment__archive_maintenance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__archive_maintenance, '0f01b23f-df88-4044-85e1-ce18e8d8a760').
narrative_ontology:cs_kernel_codification('0f01b23f-df88-4044-85e1-ce18e8d8a760', formalized).
narrative_ontology:cs_authority_grounding('0f01b23f-df88-4044-85e1-ce18e8d8a760', lineage).
narrative_ontology:cs_interpretation_layer_present('0f01b23f-df88-4044-85e1-ce18e8d8a760').
narrative_ontology:cs_reading_relation('0f01b23f-df88-4044-85e1-ce18e8d8a760', sacrifice_commandment__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('0f01b23f-df88-4044-85e1-ce18e8d8a760', sacrifice_commandment__performance_only, influences).
narrative_ontology:cs_axiom('0f01b23f-df88-4044-85e1-ce18e8d8a760', foundational, study_preserves_restoration_capability).
narrative_ontology:cs_axiom_status(study_preserves_restoration_capability, holdable).
narrative_ontology:cs_axiom_grounding('0f01b23f-df88-4044-85e1-ce18e8d8a760', study_preserves_restoration_capability, instrumental).
narrative_ontology:cs_axiom('0f01b23f-df88-4044-85e1-ce18e8d8a760', foundational, study_is_not_present_worship).
narrative_ontology:cs_axiom_status(study_is_not_present_worship, holdable).
narrative_ontology:cs_axiom_grounding('0f01b23f-df88-4044-85e1-ce18e8d8a760', study_is_not_present_worship, deontological).
narrative_ontology:cs_axiom('0f01b23f-df88-4044-85e1-ce18e8d8a760', secondary, readiness_must_be_continuous).
narrative_ontology:cs_axiom_status(readiness_must_be_continuous, holdable).
narrative_ontology:cs_axiom_grounding('0f01b23f-df88-4044-85e1-ce18e8d8a760', readiness_must_be_continuous, instrumental).
narrative_ontology:cs_reference_frame('0f01b23f-df88-4044-85e1-ce18e8d8a760', suspended_service_pending_restoration).
narrative_ontology:cs_drift_state('0f01b23f-df88-4044-85e1-ce18e8d8a760', contemporary_institutionalized_waiting, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0f01b23f-df88-4044-85e1-ce18e8d8a760', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__archive_maintenance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, future_restoration_generation).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, temple_preparatory_institutes).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, messianic_activist_networks).
narrative_ontology:constraint_victim(sacrifice_commandment__archive_maintenance, present_day_kodashim_students).
narrative_ontology:constraint_victim(sacrifice_commandment__archive_maintenance, community_education_funders).
narrative_ontology:constraint_vindicates(sacrifice_commandment__archive_maintenance, temple_service_resumability).
narrative_ontology:constraint_vindicates(sacrifice_commandment__archive_maintenance, unbroken_halakhic_transmission_chain).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Roshei yeshiva, halakhic decisors, and institute heads who set the study calendar, assign the sacrificial-law tractates, and articulate why the material matters now. Their standing, livelihoods, and institutional identities are bound to the preparatory mission; redirecting curricula toward presently applicable law would undercut the very rationale they teach. Leaving the role would mean abandoning institutions they built.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, kodashim_curriculum_authorities, agenda_setter,
    institutional, generational, constrained, global).

% Students assigned to sacrificial-law tracks spend years on material whose stated payoff lies in a restoration they may never see. They can request track changes, switch institutions, or leave the study world entirely, but each step carries communal and familial cost, and many have absorbed the preparatory rationale as their own personal duty.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, present_day_kodashim_students, payer,
    moderate, biographical, constrained, global).

% The generation that would perform a restored service, inheriting either a usable procedural archive or its gaps. It bears none of the present costs, cannot consent to or refuse the bargain made on its behalf, and exists only contingently: its collection of the arrangement's payoff depends on premises settled entirely by others.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, future_restoration_generation, beneficiary,
    powerless, generational, trapped, global).

% Research bodies that maintain vessels, garments, architectural plans, and priestly training programs ahead of a possible restoration. Donations, staffing, and public standing flow to them because of the preservation mission; their institutional identity is inseparable from it, and they additionally certify what counts as adequate preparation.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, temple_preparatory_institutes, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(sacrifice_commandment__archive_maintenance, temple_preparatory_institutes, agenda_setter).

% Donors and community boards that allocate education budgets to preparatory study programs. They can redirect giving comparatively easily, but the solicitations they respond to tie their generosity to the restoration hope, and withdrawal reads outwardly as diminished faith.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, community_education_funders, payer,
    organized, biographical, mobile, global).

% Organized groups pressing Temple Mount access, red-heifer breeding, and service rehearsals. The preservation arrangement supplies their warrant, since continuous preparation presupposes arrival, and their agitation in turn pressures institutions to expand preparatory programming. Their members' identities are fused with the mission's urgency.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, messianic_activist_networks, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_commandment__archive_maintenance, messianic_activist_networks, agenda_setter).

% Scholars inside the observant world who doubt that dialectical study preserves usable technique, who prefer reading the study as fulfillment or as suspension, or who would redirect the hours to presently applicable law. They publish and teach but hold no curricular authority, and their objections surface mainly in journals and side rooms.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, halakhic_skeptics_within_tradition, excluded,
    moderate, biographical, constrained, global).

% Academic observers of post-destruction knowledge-preservation projects across traditions. They document how communities maintain unusable procedure corpora and what happens when restoration expectations persist across generations; they neither pay nor collect.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, comparative_religion_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_commandment__archive_maintenance, temple_preparatory_institutes).
narrative_ontology:fixing_cost_class(sacrifice_commandment__archive_maintenance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves specialized procedural knowledge (orders of service, measurements, disqualifications, vessel specifications) across generations of non-use, solving the collective-action problem of intergenerational knowledge decay: no single generation has adequate incentive to maintain what it will not use, so the community institutionalizes continuous transmission.
% TRANSFER_FUNCTION: Moves present scholarly time, institutional funding, and curricular priority from living participants to an unbuilt future: effort spent now on material with no operational use is exchanged for the possibility that a later generation inherits a resumable service. Direction: present-day students and funders to the future restoration generation, with present-tense side-flows of funding and standing to the preparatory institutes.
% ABSENT_VOICES: The future restoration generation is structurally absent: it cannot consent, refuse, or renegotiate terms made entirely by present parties, and its interests are voiced only by those who benefit from defining them. Also absent from curricular decision-making are intra-tradition skeptics who would redirect the same hours to presently applicable law, and adherents of rival framings for whom the archive rationale is beside the point.
% DISAPPEARANCE_RATIONALE: Curricula would reallocate within a generation to presently applicable law; preparatory institutes would lose mandate and funding; activist networks would lose their warrant. The printed codified core would survive, but living facility — trained hands, rehearsed sequences, institutional memory — would thin within decades, and a restoration, if it came, would start from books rather than practice.
% FOUNDING_PROBLEM: After 70 CE destroyed the Second Temple, the service's procedures began decaying immediately; Mishnah-era sages disputed details within living memory of operation. The arrangement was built to solve intergenerational preservation: keeping a service restorable across an indefinite interval in which nobody may perform it.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration from outside the beneficiary set is mixed. Academic historiography attests the founding problem was real: the post-exilic reconstruction depended on transmitted procedure, and post-70 decay is documented in the tradition's own sources. Attestation that the problem REMAINS live comes almost entirely from the arrangement's own authorities and institutes; intra-tradition skeptics and the practical-replication bodies, which find texts alone insufficient and build physical replicas, implicitly attest that textual preservation no longer settles the question.
narrative_ontology:disappearance_verdict(sacrifice_commandment__archive_maintenance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__archive_maintenance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__archive_maintenance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_commandment__archive_maintenance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__archive_maintenance, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__archive_maintenance_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_commandment__archive_maintenance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_commandment__archive_maintenance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.55: the arrangement moves present, certain costs (study-years, budgets, curricular priority) against a future, contingent benefit whose sole intended collectors cannot be consulted; the transfer is real but rides a genuine preservation function and largely voluntary participation, placing it mid-range rather than high. Suppression 0.42 is authored as a raw structural property — unscaled by power or scope — reflecting curricular authority, communal expectation, and the familial cost of leaving the study world, bounded by real if costly exits. Theater_ratio 0.55: the operational core (service sequence, dimensions, disqualifications) has been codified and stable for centuries, so marginal dialectical study adds little archival yield while consuming most study-hours; a majority of activity performs preservation more than it practices it. Accessibility_collapse 0.30: rival framings remain socially live — alternatives are debated, not foreclosed. Resistance 0.32: periodic student restlessness and intra-tradition critique, dampened by the material's prestige and the pull of the restoration hope. has_sunset_clause is authored true because the arrangement carries a genuine termination condition — completed restoration ends the archive's purpose — though its trigger is exogenous and unfalsifiable from inside (see omega unfalsifiable_sunset_condition). All three series share one eight-point grid (interval approximately 1967-2023, the post-1967 institutionalization era). suppression_requirement is authored because enforcement capacity visibly matured across the interval — new institutes, dedicated study cycles, expanded curricular mandates — not merely because extraction moved. The rising trajectories encode extraction accumulation and enforcement ratcheting as the promised horizon recedes: each decade of delay raises the discount on the deferred benefit while institutions grown around the mission defend their budgets.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently, and the engine computes that divergence from the structural data. From the curriculum-authority seat the arrangement is fidelity to a received trust — the same hours look like covenantal duty. From the student seat the identical hours are compulsory labor on material with no present application, and exit costs family and belonging. From the funder seat it is a charitable wager on an unknowable date, and exit costs reputation. The future-generation seat is unreachable: every term of the bargain was set by parties with opposite incentives. Same-level divergence appears between students (constrained exit, absorbed duty) and funders (mobile exit, reputational cost only) — comparable communal standing, unequal lock-in, because the arrangement binds identity for one seat and only budget for the other.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation: future_restoration_generation is the pure beneficiary (d near 0 — collects everything, bears nothing, exits nothing); temple_preparatory_institutes and messianic_activist_networks sit near the beneficiary pole on present-tense flows (funding, mandate, mobilization warrant); present_day_kodashim_students sit near the target pole, pushed toward full-target by constrained exit; community_education_funders are targets moderated by mobile exit. One override is authored: the institutional power atom (held in this story only by kodashim_curriculum_authorities) is set to d=0.15 because the automatic derivation would treat a pure agenda-setter as symmetric, while structurally these authorities derive standing, livelihood, and institutional identity from administering the arrangement and bear negligible cost — a near-beneficiary position the beneficiary/victim arrays cannot express because their gain is positional rather than material.
 *
 * MANDATROPHY ANALYSIS:
 *   Mislabeling risks cut both ways. Reading the arrangement as pure rope (knowledge preservation!) erases the cross-temporal transfer: the people who pay are not the people who collect, and the collecting generation is contingent. Reading it as pure snare erases the genuine collective-action function: procedural knowledge demonstrably decays without coordinated transmission — the post-70 decay is documented in the tradition's own sources — so the coordination claim is not cover. Tangled_rope holds both halves. The mandatrophy watch-point is completion, not obsolescence: the codified core of the archive has been stable for centuries, so the founding problem's operational content thins even while its formal status stays live inside the tradition, and institutions persist past the point where marginal study adds archival value. The R5 mismatch consumer should watch founding_problem_status=contested against disappearance_verdict=world_rearranges: the world WOULD rearrange (curricula, institutes, and activist networks all depend on the arrangement), which combined with a contested founding problem flags zombie risk rather than settled function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    one_reading_of_sacrifice_kernel,
    'This constraint is one reading of the sacrifice_commandment kernel (archive_maintenance): what would the sibling readings change structurally?',
    'Not resolvable by data: the dispute is located in the predicate of present study — fulfillment (study_as_performance), suspension-plus-prudence (performance_only), or preparation (this reading). Resolution comes only from an authoritative ruling or a party''s framework choice.',
    'Under study_as_performance the present participant collects worship-value now, the present generation joins the beneficiaries, and epsilon falls with a rope-leaning profile. Under performance_only the archive loses its necessity claim and the arrangement reduces to optional prudence with a thinning coordination function. This file''s epsilon and beneficiary set are valid only within this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(one_reading_of_sacrifice_kernel, conceptual, 'Committer structure: one reading of the sacrifice_commandment kernel; sibling readings alter the beneficiary set and epsilon.').

omega_variable(
    unfalsifiable_sunset_condition,
    'Does the arrangement''s termination condition (restoration of the Temple) function as a binding sunset that makes the arrangement transitional, or is it structurally unfalsifiable from inside the tradition and therefore non-binding, leaving a persistent arrangement with indefinitely deferred benefits?',
    'No in-framework resolution exists — arrival timing is exogenous to participants. External behavioral evidence: whether institutions maintain termination plans, handover documentation, or wind-down scenarios; none observed to date.',
    'If the sunset binds, the arrangement is transitional support and its extraction is bridge-financing toward a completion point. If it cannot bind, deferred-benefit extraction compounds indefinitely and the tangled_rope reading hardens over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unfalsifiable_sunset_condition, conceptual, 'Whether the declared sunset condition is structurally binding or unfalsifiable.').

omega_variable(
    archive_operational_adequacy,
    'Does the accumulated corpus of study actually preserve operationally sufficient technique for restoring the service, or has study drifted into dialectic whose archival yield is marginal?',
    'Expert audit mapping curriculum outcomes onto the operational checklist (sequence, measurements, disqualifications, vessel specifications), triangulated with practical-replication programs that report texts alone insufficient.',
    'If inadequate, theater_ratio understates the dysfunction, the coordination claim weakens, and the arrangement drifts toward inertia-maintenance. If adequate, the coordination function is genuine and the measured extraction prices a real insurance policy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(archive_operational_adequacy, empirical, 'Whether the study corpus constitutes a usable technical archive or performative dialectic.').

omega_variable(
    deferred_beneficiary_consent,
    'Can a transfer whose counterparty is a contingent, unconsultable future generation be assessed by consent standards at all, and does voluntary participation inside an inherited covenantal framework count as consent to the deferral?',
    'No empirical resolution; turns on the evaluative framework — liberal consent accounting versus covenantal-obligation accounting in which membership precedes choice.',
    'Under consent accounting, effective extraction rises above the authored measure (a non-consensual cross-temporal transfer). Under covenantal accounting, the deferral is a constitutive duty rather than a taking, and extraction falls.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deferred_beneficiary_consent, preference, 'Whether consent standards apply to a bargain whose counterparty is a future generation.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is student continuation in sacrificial-law tracks driven by institutional assignment and curricular authority (structural), or by internalized preparatory duty that would persist outside the assigning institution?',
    'Post-exit trajectory: examine track alumni who left the study world — if preparatory study habits and obligation-feeling persist without institutional enforcement, the internalized share is substantial.',
    'If largely internalized, effective suppression exceeds the structural measure and survives curricular reform. If largely structural, curricular pluralism would release it quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized mechanism sustaining participation in the preparatory tracks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__archive_maintenance, 0, 56).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__archive_maintenance, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sacr_tr_t8, sacrifice_commandment__archive_maintenance, theater_ratio, 8, 0.38).
narrative_ontology:measurement(sacr_tr_t16, sacrifice_commandment__archive_maintenance, theater_ratio, 16, 0.41).
narrative_ontology:measurement(sacr_tr_t24, sacrifice_commandment__archive_maintenance, theater_ratio, 24, 0.44).
narrative_ontology:measurement(sacr_tr_t32, sacrifice_commandment__archive_maintenance, theater_ratio, 32, 0.47).
narrative_ontology:measurement(sacr_tr_t40, sacrifice_commandment__archive_maintenance, theater_ratio, 40, 0.5).
narrative_ontology:measurement(sacr_tr_t48, sacrifice_commandment__archive_maintenance, theater_ratio, 48, 0.53).
narrative_ontology:measurement(sacr_tr_t56, sacrifice_commandment__archive_maintenance, theater_ratio, 56, 0.55).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__archive_maintenance, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(sacr_be_t8, sacrifice_commandment__archive_maintenance, base_extractiveness, 8, 0.43).
narrative_ontology:measurement(sacr_be_t16, sacrifice_commandment__archive_maintenance, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(sacr_be_t24, sacrifice_commandment__archive_maintenance, base_extractiveness, 24, 0.47).
narrative_ontology:measurement(sacr_be_t32, sacrifice_commandment__archive_maintenance, base_extractiveness, 32, 0.49).
narrative_ontology:measurement(sacr_be_t40, sacrifice_commandment__archive_maintenance, base_extractiveness, 40, 0.51).
narrative_ontology:measurement(sacr_be_t48, sacrifice_commandment__archive_maintenance, base_extractiveness, 48, 0.53).
narrative_ontology:measurement(sacr_be_t56, sacrifice_commandment__archive_maintenance, base_extractiveness, 56, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_commandment__archive_maintenance, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(sacr_su_t8, sacrifice_commandment__archive_maintenance, suppression_requirement, 8, 0.25).
narrative_ontology:measurement(sacr_su_t16, sacrifice_commandment__archive_maintenance, suppression_requirement, 16, 0.28).
narrative_ontology:measurement(sacr_su_t24, sacrifice_commandment__archive_maintenance, suppression_requirement, 24, 0.31).
narrative_ontology:measurement(sacr_su_t32, sacrifice_commandment__archive_maintenance, suppression_requirement, 32, 0.34).
narrative_ontology:measurement(sacr_su_t40, sacrifice_commandment__archive_maintenance, suppression_requirement, 40, 0.37).
narrative_ontology:measurement(sacr_su_t48, sacrifice_commandment__archive_maintenance, suppression_requirement, 48, 0.4).
narrative_ontology:measurement(sacr_su_t56, sacrifice_commandment__archive_maintenance, suppression_requirement, 56, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__archive_maintenance, information_standard).
narrative_ontology:affects_constraint(sacrifice_commandment__archive_maintenance, sacrifice_commandment__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_commandment__archive_maintenance, sacrifice_commandment__performance_only).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'studying the laws of sacrifices' conflates three structurally distinct claims with different epsilon, different beneficiary sets, and different failure modes. performance_only is the baseline suspension claim (execution or nothing). archive_maintenance (this file) builds a preparatory program atop suspension: study preserves capability for eventual execution, transferring present cost to a contingent future beneficiary. study_as_performance dissolves the archive's necessity by locating fulfillment in the study itself. Upstream/downstream: performance_only's suspension logic is presupposed by archive_maintenance (an archive is only needed because execution is impossible), while study_as_performance, if adopted, removes the archive rationale entirely. Linked per the epsilon-invariance principle; each file documents the decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_commandment__archive_maintenance, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
