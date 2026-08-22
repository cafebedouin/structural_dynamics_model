% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__hybrid_atrophy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__hybrid_atrophy_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: catastrophe_memory_preservation__hybrid_atrophy_reading
 *   human_readable: Catastrophe Memory Preservation Ritual (Hybrid Atrophy Reading)
 *   domain: religious/cultural/collective_memory
 *
 * SUMMARY:
 *   This is one reading of the catastrophe_memory_preservation kernel — the
 *   hybrid_atrophy_reading. The kernel is a contested practice: ritual
 *   transmission of survival knowledge across generations after catastrophic
 *   threat. This reading holds that the ritual ONCE performed genuine
 *   threat-recognition training (the survival-competence referent) but has
 *   atrophied to perform primarily identity-maintenance and mourning (the
 *   contemporary function). The constraint extracts present-generation time
 *   and emotional labor; the beneficiary has migrated from 'survival of the
 *   community' to 'continuity of collective identity and religious
 *   authority.' The theater ratio rises sharply over the interval as the
 *   performative maintenance of cultural continuity becomes more central to
 *   the practice's own justification — the ritual has become about 'keeping
 *   memory alive' rather than 'keeping people safe.' This reading is
 *   instantiated as a Piton: the original function is substantially gone, but
 *   the constraint persists through institutional inertia and identity-fusion
 *   among practitioners.
 *
 * KEY AGENTS:
 *   - ritual_practitioners: present-generation participants who bear the time/emotional cost without receiving the claimed adaptive benefit; identity-locked into perpetuation
 *   - in_group_identity_maintainers: religious authorities and elders who benefit from the ritual as an in-group marker and whose institutional role depends on its perpetuation; can change it but do not
 *   - threat_recognition_community: historians, academics, modern risk-management specialists who have evidence about the founding problem's status and modern threat recognition — structurally excluded from the ritual's authority structure
 *   - secular_members: in-group members who no longer believe in the ritual's adaptive function but remain embedded in the community; their voices are treated as disloyalty rather than dissent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.58).
domain_priors:suppression_score(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.42).
domain_priors:theater_ratio(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__hybrid_atrophy_reading, piton).
narrative_ontology:human_readable(catastrophe_memory_preservation__hybrid_atrophy_reading, "Catastrophe Memory Preservation Ritual (Hybrid Atrophy Reading)").
narrative_ontology:topic_domain(catastrophe_memory_preservation__hybrid_atrophy_reading, "religious/cultural/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__hybrid_atrophy_reading, 'c83d91f2-e22a-49ca-89d5-97d06ba95b0c').
narrative_ontology:cs_kernel_codification('c83d91f2-e22a-49ca-89d5-97d06ba95b0c', distributed).
narrative_ontology:cs_authority_grounding('c83d91f2-e22a-49ca-89d5-97d06ba95b0c', lineage).
narrative_ontology:cs_interpretation_layer_present('c83d91f2-e22a-49ca-89d5-97d06ba95b0c').
narrative_ontology:cs_reading_relation('c83d91f2-e22a-49ca-89d5-97d06ba95b0c', catastrophe_memory_preservation__survival_competence_reading, influences).
narrative_ontology:cs_reading_relation('c83d91f2-e22a-49ca-89d5-97d06ba95b0c', catastrophe_memory_preservation__mourning_practice_reading, coexists_with).
narrative_ontology:cs_axiom('c83d91f2-e22a-49ca-89d5-97d06ba95b0c', foundational, founding_problem_functionally_obsolete).
narrative_ontology:cs_axiom_status(founding_problem_functionally_obsolete, holdable).
narrative_ontology:cs_axiom_grounding('c83d91f2-e22a-49ca-89d5-97d06ba95b0c', founding_problem_functionally_obsolete, empirically_contingent).
narrative_ontology:cs_axiom('c83d91f2-e22a-49ca-89d5-97d06ba95b0c', foundational, identity_continuity_legitimizes_inertial_practice).
narrative_ontology:cs_axiom_status(identity_continuity_legitimizes_inertial_practice, holdable).
narrative_ontology:cs_axiom_grounding('c83d91f2-e22a-49ca-89d5-97d06ba95b0c', identity_continuity_legitimizes_inertial_practice, deontological).
narrative_ontology:cs_reference_frame('c83d91f2-e22a-49ca-89d5-97d06ba95b0c', ritual_as_survival_knowledge_transmission).
narrative_ontology:cs_drift_state('c83d91f2-e22a-49ca-89d5-97d06ba95b0c', contemporary_institutional_risk_management, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c83d91f2-e22a-49ca-89d5-97d06ba95b0c', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity_maintainers).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__hybrid_atrophy_reading, ritual_practitioners).
narrative_ontology:constraint_vindicates(catastrophe_memory_preservation__hybrid_atrophy_reading, collective_survival_through_memory).
narrative_ontology:constraint_vindicates(catastrophe_memory_preservation__hybrid_atrophy_reading, intergenerational_knowledge_transfer).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Perform the ritual annually or at specified intervals, bearing the time cost, emotional labor, and material expense. They participate because leaving the practice would rupture their identity within the community and signal disrespect to ancestors. The practice no longer trains survival skills they would use; instead it performs continuity and belonging. Exit is theoretically possible but identity-prohibitive.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, ritual_practitioners, payer,
    moderate, biographical, identity_locked, local).

% Religious authorities, elders, and cultural stewards who interpret the ritual's meaning, administer its performance, and enforce its perpetuation as a marker of group membership and continuity. They benefit from the ritual's operation as a boundary-maintenance and identity-reproduction mechanism. They could change or retire the practice but do not, because its persistence vindicates their role as custodians of tradition.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity_maintainers, agenda_setter,
    institutional, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity_maintainers, beneficiary).

% The ancestors whose experiences the ritual memorializes. They are not agents in the present constraint, but their survival — and the adaptive practices that enabled it — is the narratively claimed referent for the ritual's legitimacy.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, historical_survivors, beneficiary,
    analytical, biographical, analytical, universal).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_preservation__hybrid_atrophy_reading, historical_survivors).

% Academics, historians, and practitioners from other communities who could contribute updated threat-recognition knowledge or challenge the ritual's claimed survival function. They are largely absent from the ritual's internal adjudication; the ritual's authority structure does not systematically incorporate external evidence about what threats actually persist or how they are best recognized.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, threat_recognition_community, excluded,
    organized, generational, constrained, regional).

% Members of the in-group who have reduced their belief in the ritual's adaptive function or its supernatural efficacy but remain structurally embedded in the community. They would advocate for retiring or lightening the practice but are not in the conversation that governs its perpetuation — their voices are treated as disrespect or weak commitment rather than legitimate dissent.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, secular_members, excluded,
    moderate, biographical, constrained, local).

% Families, schools, and cultural institutions that teach the next generation the ritual's performance and meaning. They are invested in the constraint's perpetuation as a core part of their educational mission and identity reproduction. They interpret the ritual to new practitioners and enforce participation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, intergenerational_transmission_apparatus, agenda_setter,
    institutional, generational, mobile, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity_maintainers).
narrative_ontology:fixing_cost_class(catastrophe_memory_preservation__hybrid_atrophy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally: preserves operational threat-recognition knowledge across generations — survivors encode their survival strategies into ritual performance so descendants learn threat-identification and response without direct experience. Currently: no functional threat-recognition transfer occurs; the coordination problem it solved has been superseded by institutional risk management and secular education.
% TRANSFER_FUNCTION: Moves the present generation's time, emotional attention, and identity-commitment from alternative activities into the annual (or periodic) ritual performance. The constraint extracts the cost of participation; beneficiaries are in-group identity continuity and the institutional authority structures that depend on the ritual for legitimacy.
% ABSENT_VOICES: Secular members and external threat-recognition specialists are structurally excluded. They would argue the ritual's claimed survival function is obsolete and the practice should be retired or radically simplified. Historical survivors (the ancestors whose experience grounds the ritual) cannot speak to whether their strategies are still relevant or whether the modern performance accurately transmits them.
% DISAPPEARANCE_RATIONALE: Advocates for the survival-competence reading argue: if the ritual disappeared, threat-recognition knowledge would be lost and communities would become vulnerable (the world rearranges via knowledge loss). Advocates for the mourning-practice reading argue: if the ritual disappeared, the same people would find alternative identity-maintenance mechanisms; the loss is symbolic, not functional (world rearranges via identity reorganization). The hybrid atrophy reading contests both: the functional loss is real but the actual knowledge transfer has already atrophied; disappearance would hurt collective memory performance more than adaptive capacity, but the hurt to present practitioners would ease significantly because the identity-lock would break and the extraction would cease.
% FOUNDING_PROBLEM: Catastrophic threat (war, genocide, displacement, plague, famine) required survivors to encode and transmit survival knowledge to the next generation. The ritual was the technology — performance encoded knowledge; watching/participating embedded it in memory and muscle.
% FOUNDING_PROBLEM_CORROBORATION: Historians and anthropologists who study threat-recognition systems confirm the founding problem was real and the ritual was adaptive when the threat was live. Modern institutional risk management (early warning systems, disaster planning, epidemiology) handles most historical threat categories outside the ritual. The ritual authorities themselves no longer claim the practice is operational threat-training; they reframe it as identity-continuity and symbolic remembrance. Only historians and fundamentalist interpreters still assert the survival-competence reading; mainstream religious authorities have largely migrated to the mourning-practice reading, but the ritual structure persists unchanged — a Piton characteristic.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__hybrid_atrophy_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__hybrid_atrophy_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__hybrid_atrophy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__hybrid_atrophy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__hybrid_atrophy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_preservation__hybrid_atrophy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_preservation__hybrid_atrophy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts high (0.72) because the ritual imposes real time/emotional cost on practitioners without delivering the claimed survival function. It declines over the interval (to 0.48 by endpoint) as two forces operate: first, younger practitioners increasingly understand the ritual as identity-performance rather than survival-training, so the extraction/coordination boundary becomes clearer and fewer practitioners experience it as imposed; second, competing institutions (secular schools, disaster management) have already captured the threat-recognition function, so the ritual's extractive asymmetry relative to genuine alternatives has weakened. Theater ratio rises sharply (0.45→0.72) because the ritual's performance of its OWN cultural continuity becomes the central justification — the actors are increasingly aware they are performing the fact of memory, not transmitting actionable knowledge. Suppression holds steady (0.38→0.43) because the extraction is maintained primarily through identity-lock, not through overt coercion; secular members who want to exit cannot do so without rupturing their identity within the community, so suppression is internalized rather than structural.
 *
 * PERSPECTIVAL GAP:
 *   The in-group identity maintainers (agenda-setter seat) experience the ritual as genuine coordination: a mechanism that reproduces cultural identity and transmits meaning across generations — a valuable function they see as threatened by secularization. The present-generation practitioners (payer seat) experience it as extraction: they inherit the obligation without having chosen it, they no longer believe in its adaptive function, and they are structurally prevented from exiting because departure would signal disrespect and community abandonment. The hybrid atrophy reading predicts this gap will widen as institutional alternatives to threat-recognition become more salient and younger practitioners increasingly define their exit as ideological rather than practical — the identity-lock mechanism becomes the primary suppressive force. The threat-recognition community (excluded seat) would add a third frame: the ritual is neither effective coordination nor authentic knowledge transfer, and its perpetuation misinforms younger generations about how actual threats are recognized and managed.
 *
 * DIRECTIONALITY LOGIC:
 *   In-group identity maintainers hold institutional power and mobility (d ≈ 0.2, near beneficiary end): they benefit from the constraint's operation and could change it if they chose. Present-generation practitioners hold moderate power but are identity-locked (d ≈ 0.75, near target end): they bear the cost, lack meaningful exit, and have constrained agency within the practice. Secular members are trapped in an identity-constrained way: they could physically leave the community but would bear the cost of identity rupture, making their exit 'trapped' rather than 'mobile' — their directionality is high (d ≈ 0.8) because they are targets who cannot exit without severe identity loss. The constraint's effective extraction is amplified for identity-locked agents and damped for mobile institutional authorities.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is classified as Piton — atrophied former function maintained by inertia and theatrical performance. The founding problem (threat transmission to next generation) was real and the ritual was adaptive when the threat was live. The founding problem is now dead: institutional risk management, secular education, and communication technology have captured the threat-recognition function, and present-generation practitioners no longer learn survival skills from the ritual. The constraint persists because: (1) the ritual's institutional authority structure benefits from its perpetuation (identity maintenance, role legitimacy); (2) present-generation practitioners are identity-locked into participation; (3) the ritual's meaning has been reframed from 'survival training' to 'cultural continuity,' which makes it harder to contest on functional grounds. The theater ratio's sharp rise marks the transition: the ritual is increasingly about 'being the kind of person who remembers' rather than 'learning skills.' A snare would be characterized by identifiable victims and active coercion to prevent exit; a Piton is characterized by diffuse costs, institutional inertia, and identity-fusion that makes exit feel like self-rupture. This reading treats the present performers as victims not through force but through identity-lock: they inherit an obligation that extracts their time while delivering identity-performance rather than adaptive knowledge.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_transfer_hypothesis_testing,
    'Does the ritual actually transmit threat-recognition knowledge that practitioners use in contemporary threat response? If so, what kind of knowledge and in what contexts?',
    'Ethnographic study of practitioners'' actual behavior during modern threat (pandemic, civil emergency, natural disaster): do they reference ritual-learned patterns? Cognitive testing of practitioners'' threat-recognition performance versus non-practitioners with similar institutional risk-management training.',
    'If knowledge transfer is occurring, the constraint''s extractiveness is modulated by genuine coordination function and the classification may shift toward Tangled Rope rather than Piton. If no knowledge transfer occurs, the Piton classification is confirmed and the founding_problem_status = dead verdict is supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(functional_transfer_hypothesis_testing, empirical, 'Whether actual threat-recognition knowledge is transmitted through the ritual or only symbolic performance.').

omega_variable(
    identity_lock_mechanism_internalization,
    'Is the suppression measured in the ritual practitioners'' participation structural (enforced by community sanctions) or internalized (practitioners have fused their identity with the practice such that exit feels like self-rupture)?',
    'Post-exit narrative analysis: practitioners who leave the community are interviewed about whether suppression persists (internalized) or disappears (structural). Comparison of exit rates and ease across younger cohorts (declining internalization) versus older cohorts (higher internalization).',
    'If suppression is primarily structural, remediation (community sanctions relaxed) would enable exit. If internalized, remediation requires identity-reframing work outside the constraint''s system. The Piton classification assumes identity-lock suppression; discovery of primarily structural suppression might indicate a Snare classification instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_internalization, empirical, 'The mechanism of suppression in the ritual obligation: structural barriers or identity fusion.').

omega_variable(
    sibling_reading_foreclosure_test,
    'Do the three readings of the catastrophe_memory_preservation kernel coexist as live positions or does one reading logically foreclose the others within the same authority framework?',
    'Textual analysis of religious authority statements: can a single religious institution endorse both survival-competence and mourning-practice readings simultaneously, or does adoption of one reading require rejection of the other?',
    'If readings coexist (different authorities hold different readings), the network relation is coexists_with. If one reading logically rules out the others, the relation is forecloses. This affects how the constraint family is structured and what the engine predicts about institutional pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_test, conceptual, 'Whether sibling readings are logically compatible within a single authority framework.').

omega_variable(
    generational_belief_shift_trajectory,
    'What is the trajectory of younger practitioners'' belief in the ritual''s adaptive function relative to older practitioners? Is secularization or functional skepticism accelerating?',
    'Cohort-stratified survey of ritual participants across age groups: belief in ritual''s threat-recognition function, participation motivation (obligatory vs. chosen), and exit intention. Repeat surveys over 10-year intervals.',
    'Accelerating disbelief would suggest the constraint is approaching a critical transition where identity-lock breaks and participation collapses (late-stage Piton). Stable or decelerating disbelief would suggest the constraint has reached equilibrium between institutional maintenance and practitioner skepticism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_belief_shift_trajectory, empirical, 'Trajectory of generational belief-shift in the ritual''s adaptive function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__hybrid_atrophy_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 20, 0.51).
narrative_ontology:measurement_basis(cata_tr_t20, observed).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 40, 0.57).
narrative_ontology:measurement_basis(cata_tr_t40, observed).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 60, 0.63).
narrative_ontology:measurement_basis(cata_tr_t60, observed).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 80, 0.68).
narrative_ontology:measurement_basis(cata_tr_t80, observed).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 100, 0.7).
narrative_ontology:measurement_basis(cata_tr_t100, observed).
narrative_ontology:measurement(cata_tr_t120, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 120, 0.72).
narrative_ontology:measurement_basis(cata_tr_t120, projected).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(cata_be_t20, observed).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement_basis(cata_be_t40, observed).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 60, 0.59).
narrative_ontology:measurement_basis(cata_be_t60, observed).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 80, 0.55).
narrative_ontology:measurement_basis(cata_be_t80, observed).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 100, 0.52).
narrative_ontology:measurement_basis(cata_be_t100, observed).
narrative_ontology:measurement(cata_be_t120, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 120, 0.48).
narrative_ontology:measurement_basis(cata_be_t120, projected).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement_basis(cata_su_t20, observed).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 40, 0.41).
narrative_ontology:measurement_basis(cata_su_t40, observed).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 60, 0.42).
narrative_ontology:measurement_basis(cata_su_t60, observed).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 80, 0.43).
narrative_ontology:measurement_basis(cata_su_t80, observed).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 100, 0.43).
narrative_ontology:measurement_basis(cata_su_t100, observed).
narrative_ontology:measurement(cata_su_t120, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 120, 0.42).
narrative_ontology:measurement_basis(cata_su_t120, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__hybrid_atrophy_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation__mourning_practice_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_preservation kernel decomposes into three structurally distinct constraint stories sharing the same ritual practice. The survival_competence_reading treats the ritual as a mechanism that preserves operational threat-recognition knowledge (a Mountain or Rope, depending on the evidence); the mourning_practice_reading treats it as pure symbolic continuity without functional knowledge transfer (a Rope); the hybrid_atrophy_reading treats it as an atrophied former function now maintained by institutional inertia and identity-fusion (a Piton). Each reading instantiates a different epsilon and a different victim/beneficiary structure. The readings are linked via network.affects_constraints to model how evidence about the ritual's actual knowledge-transfer capacity influences which reading is empirically supported. The upstream story (survival_competence_reading, most established) influences the downstream stories (mourning_practice_reading and hybrid_atrophy_reading, more contested, more extractive).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_preservation__hybrid_atrophy_reading, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
