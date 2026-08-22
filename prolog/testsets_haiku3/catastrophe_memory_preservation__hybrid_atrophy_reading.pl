% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__hybrid_atrophy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   This reading instantiates the hybrid_atrophy frame within the contested
 *   kernel of catastrophe memory preservation. The constraint describes a
 *   ritual practice that historically encoded
 *   survival-competence—threat-recognition patterns, collective response
 *   protocols, distributed knowledge preservation—in ceremonial form. Under
 *   modernity (institutional safety systems, geographic mobility,
 *   professional crisis management), the functional referent of the ritual
 *   has substantially atrophied. The ritual persists, but its justification
 *   has migrated from adaptive function ('this teaches us to recognize and
 *   respond to threats') to identity and memory preservation ('this is who we
 *   are, and we remember'). Present-generation practitioners inherit the
 *   practice at substantial cost (time, emotional labor, social obligation)
 *   without receiving the adaptive payoff their ancestors did. The constraint
 *   is classified as piton: an atrophied former function maintained through
 *   institutional inertia and identity-fusion rather than genuine ongoing
 *   payoff to current payers. The reading is distinct from the
 *   survival_competence_reading (which would assess the ritual as
 *   legitimately encoding still-live threat-recognition) and from the
 *   mourning_practice_reading (which would strip away the adaptive-function
 *   frame entirely and assess the ritual purely as symbolic continuity work,
 *   with no claim about historical function).
 *
 * KEY AGENTS:
 *   - in_group_identity_carriers — the collective group whose boundaries and self-understanding are constituted through participation in the ritual (moderate power, generational horizon, identity-locked to the practice)
 *   - present_generation_practitioners — individuals who perform the ritual at cost (moderate power, biographical horizon, identity-locked exit, bearing the present burden)
 *   - ritual_administrators — organized actors who schedule and lead (organized power, generational horizon, constrained exit due to institutional role)
 *   - younger_generation_evaluators — powerless and excluded stakeholders who inherit without choice and lack voice in modification (powerless, biographical horizon, identity-locked)
 *   - external_institutional_actors — state safety, public health, education systems that have taken over the protective functions the ritual historically served (institutional power, generational horizon, analytical perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.48).
domain_priors:suppression_score(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.22).
domain_priors:theater_ratio(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.67).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0.67).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__hybrid_atrophy_reading, piton).
narrative_ontology:human_readable(catastrophe_memory_preservation__hybrid_atrophy_reading, "Catastrophe Memory Preservation Ritual (Hybrid Atrophy Reading)").
narrative_ontology:topic_domain(catastrophe_memory_preservation__hybrid_atrophy_reading, "religious/cultural/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__hybrid_atrophy_reading, 'd0280c94-0f57-4784-80dd-8511a0d99e4f').
narrative_ontology:cs_kernel_codification('d0280c94-0f57-4784-80dd-8511a0d99e4f', implicit).
narrative_ontology:cs_authority_grounding('d0280c94-0f57-4784-80dd-8511a0d99e4f', lineage).
narrative_ontology:cs_interpretation_layer_present('d0280c94-0f57-4784-80dd-8511a0d99e4f').
narrative_ontology:cs_reading_relation('d0280c94-0f57-4784-80dd-8511a0d99e4f', catastrophe_memory_preservation__survival_competence_reading, influences).
narrative_ontology:cs_reading_relation('d0280c94-0f57-4784-80dd-8511a0d99e4f', catastrophe_memory_preservation__mourning_practice_reading, coexists_with).
narrative_ontology:cs_axiom('d0280c94-0f57-4784-80dd-8511a0d99e4f', foundational, protective_functions_institutionally_replaced).
narrative_ontology:cs_axiom_status(protective_functions_institutionally_replaced, holdable).
narrative_ontology:cs_axiom_grounding('d0280c94-0f57-4784-80dd-8511a0d99e4f', protective_functions_institutionally_replaced, empirically_contingent).
narrative_ontology:cs_axiom('d0280c94-0f57-4784-80dd-8511a0d99e4f', secondary, identity_persistence_through_ritual_continuation).
narrative_ontology:cs_axiom_status(identity_persistence_through_ritual_continuation, holdable).
narrative_ontology:cs_axiom_grounding('d0280c94-0f57-4784-80dd-8511a0d99e4f', identity_persistence_through_ritual_continuation, deontological).
narrative_ontology:cs_reference_frame('d0280c94-0f57-4784-80dd-8511a0d99e4f', ritual_as_adaptive_survival_encoding).
narrative_ontology:cs_drift_state('d0280c94-0f57-4784-80dd-8511a0d99e4f', contemporary_institutional_modernity, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d0280c94-0f57-4784-80dd-8511a0d99e4f', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity_carriers).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain continuity of collective identity and group boundaries through ritualized commemoration. The practice anchors 'who we are' to 'what happened to our ancestors' — participating in the ritual vindicates membership and reproduces the group's self-understanding as survivors of catastrophe. They do not directly administer the ritual but its continuation serves their identity preservation interest.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity_carriers, beneficiary,
    moderate, generational, identity_locked, local).

% Inherit and perform the ritual at substantial cost in time, emotional labor, and social obligation. The ritual's historical function—encoding and transmitting survival-critical threat-recognition skills—has atrophied under modernity (structural threats have changed, institutional safety systems have replaced community vigilance). They bear the cost of performance without receiving adaptive benefit. Exit means identity rupture within the group, not merely choice; the practice is inseparable from accepted membership.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_practitioners, payer,
    moderate, biographical, identity_locked, local).

% The structural threat the ritual originally encoded for (localized catastrophe risk, seasonal scarcity, inter-community violence) has been substantially mitigated by institutional development, geographic mobility, and modern safety systems. This is not a contestable matter but a changed condition; the ritual persists in form after its functional referent has largely disappeared.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, historical_threat_environment, observer,
    analytical, civilizational, analytical, local).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_preservation__hybrid_atrophy_reading, historical_threat_environment).

% Organize, schedule, and lead the ritual. They justify its continuation on identity/memory grounds ('we do this to remember') rather than on threat-recognition or survival-skill transfer (which were the historical justifications). They could unilaterally alter the ritual's content or intensity but face institutional resistance to change — group members regard alterations as dilution of authenticity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, ritual_administrators, agenda_setter,
    organized, generational, constrained, local).

% Are required to participate but are not formally consulted about the ritual's relevance or cost-benefit tradeoff. They inherit the practice without having chosen it and lack the institutional power to reshape it. Their objections or indifference are treated as cultural erosion rather than legitimate feedback about functional obsolescence.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, younger_generation_evaluators, excluded,
    powerless, biographical, identity_locked, local).

% State safety apparatus, educational systems, and secular institutional infrastructure have taken over the protective and knowledge-transfer functions the ritual historically served. They do not directly participate in the ritual but their presence and effectiveness render much of the ritual's adaptive logic obsolete. They regard the ritual as heritage preservation, not survival competence.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, external_institutional_actors, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity_carriers).
narrative_ontology:fixing_cost_class(catastrophe_memory_preservation__hybrid_atrophy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Encodes and transmits threat-recognition patterns, collective memory of catastrophe, and coordinated response protocols across generations — historically essential for community survival under conditions of repeated localized threat and distributed knowledge.
% TRANSFER_FUNCTION: Moves emotional investment, time, and social obligation from present practitioners to the sustenance of group identity and the commemoration of historical catastrophe. Under the historical reading (survival_competence_reading) the transfer would be justified by adaptive payoff; under this reading (hybrid_atrophy) the transfer persists but its justification has shifted to identity rather than function.
% ABSENT_VOICES: Younger practitioners who find the ritual burdensome or obsolete, members from outside the group who see the continuation as costly theater rather than meaningful memory-work, and external institutions (public safety, education) that have taken over the survival-competence functions the ritual historically performed.
% DISAPPEARANCE_RATIONALE: If the ritual vanished, institutional memory of the historical catastrophe would persist in archives and education; group identity would fracture because the ritual is the primary embodied carrier of continuity; and the distribution of defensive knowledge and collective vigilance would further centralize in state institutions. Whether this counts as loss depends on whether one values the ritual's identity function (irreplaceable loss) or evaluates it against its historical survival-competence function (functional obsolescence already complete). This is the constitutional disagreement this reading instantiates.
% FOUNDING_PROBLEM: Localized catastrophe (war, famine, persecution, genocide) created urgent need to preserve survival-critical knowledge, threat-recognition patterns, and collective response capacity across generations in the absence of institutional safety systems.
% FOUNDING_PROBLEM_CORROBORATION: Historical archives and academic study confirm the founding problem was acute and real (e.g., records of specific catastrophe events). External institutional actors (state safety apparatus, public health systems, educational institutions) attest that the founding problem is substantially solved — modern systems provide threat-monitoring, early warning, and coordinated response that replace distributed vigilance. Ritual administrators contest this on the grounds that institutional systems are fragile and identity-based transmission is irreplaceable; this disagreement is documented in oral testimony and religious-authority statements.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__hybrid_atrophy_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__hybrid_atrophy_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__hybrid_atrophy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__hybrid_atrophy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.48 at interval end) and declining because the constraint extracts time and obligation from present practitioners, but that extraction is not captured by an identifiable institutional beneficiary—it circulates within the in-group identity sphere. Theater ratio is high and rising (0.35→0.67 over the interval) because the constraint's justification has shifted from functional (threat-recognition, survival skill) to performative (memory, identity, continuity)—an increasing share of the ritual's social function is maintained through the performance of continuity rather than through adaptive payoff. Suppression is low and stable (0.22) because the constraint's persistence does not require heavy coercion—identity-fusion and cultural obligation sustain participation more effectively than direct suppression. Accessibility_collapse is moderate (0.38 overall) because present practitioners, especially younger generations, do have alternative exit routes (geographic mobility, institutional safety provision, secular life paths)—the collapse is primarily at the individual identity level (high) and less at structural/systemic level (low). Resistance is moderate (0.52) and rising because as the functional justification atrophies, practitioner resistance increases ('why are we doing this?'), while the organizational and structural levels show declining pressure to maintain the practice. The measurement series track one shared grid across all three metrics (shared time points t=0,20,40,60,80,100), with base_extractiveness declining over time as the historical adaptive rationale loses relevance, theater_ratio rising as performative justification becomes the primary frame, and suppression remaining flat (no increase in coercive enforcement needed—cultural obligation carries the constraint). The coercion grid tracks how pressure distributes across individual/organizational/class/structural levels: individual practitioners face highest accessibility_collapse (most identity-locked) and highest stakes_inflation (personal cost), while structural-level pressure to maintain the ritual is weak (external institutional systems have replaced its function). The divergence between t0 and t100 shows the constraint tightening at the individual identity level while loosening at structural levels—the ritual persists because individuals are trapped in it, not because institutional pressures sustain it.
 *
 * PERSPECTIVAL GAP:
 *   The perspective gap here is generational and functional: from the administrator's/in-group-identity seat, the ritual is essential identity-work and cultural transmission—objectively necessary continuation. From the present-generation-practitioner seat, the same practice is increasingly burdensome performance maintenance without adaptive payoff—the functional referent is gone, and the identity frame, while real, does not fully compensate for the cost. From the younger-generation seat (excluded), the constraint is inherited obligation with no voice—they did not consent to the practice and lack institutional power to reshape it. The engine should compute these seats differently: the administrators as more beneficiary-proximate (role=agenda_setter), practitioners as closer to target (role=payer), younger as maximally constrained target (role=excluded, identity_locked). The claim/metric gap is deliberate: the constraint is CLAIMED as piton (atrophied function, theatrical maintenance) while the metrics describe moderate, declining extractiveness with rising theater—the engine measures whether the claimed type fits the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The in_group_identity_carriers are the structural beneficiary (d near 0.1-0.2) because the constraint's continuation sustains their collective identity and group boundaries—they benefit from other people performing the ritual. Present-generation practitioners are targets (d near 0.8-0.9) because they bear the cost (time, obligation, emotional labor) without receiving adaptive benefit—their ancestors bore the same cost but received survival-competence transfer; they receive only the identity/continuity justification, which does not compensate for the burden under conditions of modernity. The directionality asymmetry between generations is the core of this reading: the same practice distributes costs and benefits differently across time. Ritual_administrators have moderate directionality (d~0.5) because they collect some benefit (professional role maintenance, cultural authority) and bear some cost (responsibility for the practice's preservation, institutional pressure to justify it). Younger_generation_evaluators are excluded stakeholders (not in the formal role set but affected): they have very high directionality toward the target end (d~0.95) because they inherit the obligation without choice and lack voice in modification. The external_institutional_actors have analytical directionality (d=0.5 by definition) because they observe rather than participate, though their presence and effectiveness (having taken over protective functions) creates structural pressure toward the constraint's redundancy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (localized catastrophe risk, need to preserve threat-recognition and response capacity) is DEAD under this reading: external institutional systems (state safety, public health, early warning infrastructure) have taken over the protective functions the ritual historically served. The constraint PERSISTS because (1) identity-fusion makes exit costly, (2) ritual administrators have institutional interest in continuation, (3) the practice's symbolic and memory functions are real even if the adaptive function is gone. This is the classic piton condition: a former coordination/adaptation mechanism now maintained primarily through inertia, identity obligation, and the theater of continuity rather than through active payoff. The measurement series show extractiveness declining (as the historical adaptive rationale becomes less credible) and theater rising (as performative justification becomes the primary frame)—this is the signature of mandatrophy resolution: the founding problem is dead, the constraint persists, and the justification has migrated from function to performance. The constraint qualifies as mandatrophy-resolved because the founding problem is demonstrably dead (external institutional systems provide the protective functions) while the practice persists—the gap between founding rationale and current reality is the definition of mandatrophy. Declaring `base_properties.mandatrophy_resolved: true` would signal this reading's interpretation; however, since this is one reading of a contested kernel, the resolution is not final—the survival_competence_reading would contest the claim that the founding problem is dead, arguing that institutional systems are fragile and the ritual's threat-recognition encoding is still live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_fragility_vs_atrophy,
    'Are institutional safety systems genuinely robust enough that threat-recognition encoded in the ritual is no longer adaptively necessary, or is institutional fragility high enough that the distributed vigilance the ritual preserves remains adaptive insurance?',
    'Historical analysis of institutional failure modes and cascades; scenarios testing institutional collapse and recovery in the constraint''s cultural context; long-term institutional reliability data.',
    'If institutional systems are robust: the ritual is functionally atrophied and the constraint qualifies as piton (atrophied former function). If institutional systems are fragile: the ritual remains partially adaptive and the constraint qualifies as rope or tangled_rope (coordination with declining but nonzero adaptive payoff). This determines whether present practitioners are bearing justified coordination costs or pure identity-extraction costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_fragility_vs_atrophy, empirical, 'Whether institutional replacement of protective functions makes ritual threat-encoding obsolete or essential-as-backup.').

omega_variable(
    identity_fusion_mechanism_structural_vs_internalized,
    'Is the identity-locking that keeps practitioners in the ritual structurally imposed (community enforcement, reputation cost of defection) or internalized (practitioners have fused their self-concept with the role)?',
    'Post-exit trajectories: survey practitioners who left the ritual and community; measure suppression/shame/identity-fracture persistence; compare to practitioners in communities that have formally modified or dropped the ritual.',
    'If structural: practitioners could exit if community enforcement were lifted; the constraint''s persistence is contingent on active group enforcement. If internalized: practitioners carry the suppression with them after exit; the constraint has achieved identity capture such that even structural exit leaves the obligation intact. This affects whether the constraint qualifies as snare (with active group enforcement) or as a more durable identity-fusion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_fusion_mechanism_structural_vs_internalized, empirical, 'Whether identity-locking is structural or internalized.').

omega_variable(
    competing_sibling_reading_resolution,
    'Which sibling reading correctly characterizes this constraint: survival_competence_reading (threat-recognition remains adaptive), mourning_practice_reading (adaptive function is irrelevant, memory is the point), or this hybrid_atrophy_reading (historical function atrophied but identity persists)?',
    'Cross-party attestation: what do administrators, practitioners, external institutional actors, and group members outside the immediate in-group say about the ritual''s present adaptive role? Empirical assessment of threat-environment change in the specific cultural context. Historical documentation of the ritual''s content change over time.',
    'The sibling reading that is corroborated determines the constraint''s terminal classification and mandatrophy status: survival_competence reading would classify the ritual as rope (coordination still adaptive); mourning_practice reading would classify it as identity-coordination scaffold or rope; hybrid_atrophy reading (this one) classifies it as piton. The three readings coexist in live public discourse within and around the group; resolving which is structurally correct requires empirical and historical work outside the reading''s own premises.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competing_sibling_reading_resolution, empirical, 'Which of the three sibling readings correctly characterizes the constraint''s present structural role.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__hybrid_atrophy_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(cata_tr_t20, observed).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 40, 0.51).
narrative_ontology:measurement_basis(cata_tr_t40, observed).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 60, 0.61).
narrative_ontology:measurement_basis(cata_tr_t60, observed).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 80, 0.65).
narrative_ontology:measurement_basis(cata_tr_t80, observed).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 100, 0.67).
narrative_ontology:measurement_basis(cata_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(cata_be_t20, observed).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 40, 0.53).
narrative_ontology:measurement_basis(cata_be_t40, observed).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 60, 0.49).
narrative_ontology:measurement_basis(cata_be_t60, observed).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 80, 0.48).
narrative_ontology:measurement_basis(cata_be_t80, observed).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 100, 0.48).
narrative_ontology:measurement_basis(cata_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 20, 0.19).
narrative_ontology:measurement_basis(cata_su_t20, observed).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 40, 0.2).
narrative_ontology:measurement_basis(cata_su_t40, observed).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 60, 0.22).
narrative_ontology:measurement_basis(cata_su_t60, observed).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 80, 0.22).
narrative_ontology:measurement_basis(cata_su_t80, observed).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 100, 0.22).
narrative_ontology:measurement_basis(cata_su_t100, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=100
narrative_ontology:measurement(cata_grid_01, catastrophe_memory_preservation__hybrid_atrophy_reading, accessibility_collapse(class), 0, 0.35).
narrative_ontology:measurement(cata_grid_02, catastrophe_memory_preservation__hybrid_atrophy_reading, accessibility_collapse(class), 100, 0.28).
narrative_ontology:measurement(cata_grid_03, catastrophe_memory_preservation__hybrid_atrophy_reading, accessibility_collapse(individual), 0, 0.72).
narrative_ontology:measurement(cata_grid_04, catastrophe_memory_preservation__hybrid_atrophy_reading, accessibility_collapse(individual), 100, 0.68).
narrative_ontology:measurement(cata_grid_05, catastrophe_memory_preservation__hybrid_atrophy_reading, accessibility_collapse(organizational), 0, 0.55).
narrative_ontology:measurement(cata_grid_06, catastrophe_memory_preservation__hybrid_atrophy_reading, accessibility_collapse(organizational), 100, 0.48).
narrative_ontology:measurement(cata_grid_07, catastrophe_memory_preservation__hybrid_atrophy_reading, accessibility_collapse(structural), 0, 0.15).
narrative_ontology:measurement(cata_grid_08, catastrophe_memory_preservation__hybrid_atrophy_reading, accessibility_collapse(structural), 100, 0.12).
narrative_ontology:measurement(cata_grid_09, catastrophe_memory_preservation__hybrid_atrophy_reading, resistance(class), 0, 0.45).
narrative_ontology:measurement(cata_grid_10, catastrophe_memory_preservation__hybrid_atrophy_reading, resistance(class), 100, 0.48).
narrative_ontology:measurement(cata_grid_11, catastrophe_memory_preservation__hybrid_atrophy_reading, resistance(individual), 0, 0.35).
narrative_ontology:measurement(cata_grid_12, catastrophe_memory_preservation__hybrid_atrophy_reading, resistance(individual), 100, 0.42).
narrative_ontology:measurement(cata_grid_13, catastrophe_memory_preservation__hybrid_atrophy_reading, resistance(organizational), 0, 0.58).
narrative_ontology:measurement(cata_grid_14, catastrophe_memory_preservation__hybrid_atrophy_reading, resistance(organizational), 100, 0.52).
narrative_ontology:measurement(cata_grid_15, catastrophe_memory_preservation__hybrid_atrophy_reading, resistance(structural), 0, 0.62).
narrative_ontology:measurement(cata_grid_16, catastrophe_memory_preservation__hybrid_atrophy_reading, resistance(structural), 100, 0.68).
narrative_ontology:measurement(cata_grid_17, catastrophe_memory_preservation__hybrid_atrophy_reading, stakes_inflation(class), 0, 0.32).
narrative_ontology:measurement(cata_grid_18, catastrophe_memory_preservation__hybrid_atrophy_reading, stakes_inflation(class), 100, 0.28).
narrative_ontology:measurement(cata_grid_19, catastrophe_memory_preservation__hybrid_atrophy_reading, stakes_inflation(individual), 0, 0.48).
narrative_ontology:measurement(cata_grid_20, catastrophe_memory_preservation__hybrid_atrophy_reading, stakes_inflation(individual), 100, 0.42).
narrative_ontology:measurement(cata_grid_21, catastrophe_memory_preservation__hybrid_atrophy_reading, stakes_inflation(organizational), 0, 0.58).
narrative_ontology:measurement(cata_grid_22, catastrophe_memory_preservation__hybrid_atrophy_reading, stakes_inflation(organizational), 100, 0.54).
narrative_ontology:measurement(cata_grid_23, catastrophe_memory_preservation__hybrid_atrophy_reading, stakes_inflation(structural), 0, 0.08).
narrative_ontology:measurement(cata_grid_24, catastrophe_memory_preservation__hybrid_atrophy_reading, stakes_inflation(structural), 100, 0.06).
narrative_ontology:measurement(cata_grid_25, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression(class), 0, 0.12).
narrative_ontology:measurement(cata_grid_26, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression(class), 100, 0.14).
narrative_ontology:measurement(cata_grid_27, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression(individual), 0, 0.28).
narrative_ontology:measurement(cata_grid_28, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression(individual), 100, 0.25).
narrative_ontology:measurement(cata_grid_29, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression(organizational), 0, 0.18).
narrative_ontology:measurement(cata_grid_30, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression(organizational), 100, 0.22).
narrative_ontology:measurement(cata_grid_31, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression(structural), 0, 0.05).
narrative_ontology:measurement(cata_grid_32, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression(structural), 100, 0.06).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__hybrid_atrophy_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation__mourning_practice_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-story decomposition of the catastrophe_memory_preservation kernel. The three readings decompose because they instantiate structurally different claims about the same ritual practice: (1) survival_competence_reading asserts the ritual still encodes adaptive threat-recognition; (2) mourning_practice_reading asserts the ritual's adaptive-function status is irrelevant and strips the frame to pure identity/memory work; (3) hybrid_atrophy_reading (this one) asserts the ritual historically encoded genuine function but that function has atrophied under modernity. Each reading has a different epsilon (adaptive function vs. pure identity work), different victim/beneficiary structure, and different terminal type. All three share the same referent (the ritual practice itself) but evaluate it by different epistemic frames. The ε-invariance principle requires three separate constraint stories because the readings' premises would yield different ε measurements—one cannot merge them into a single constraint. The network links capture structural dependency: hybrid_atrophy_reading's claim that the founding problem is dead influences both siblings (undermines survival_competence reading's premise, shapes mourning_practice reading's frame), while each sibling coexists with the others as live positions held by different parties and interpretive communities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_preservation__hybrid_atrophy_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
