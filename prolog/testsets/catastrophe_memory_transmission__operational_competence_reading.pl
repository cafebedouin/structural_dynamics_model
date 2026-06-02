% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__operational_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__operational_competence_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_memory_transmission__operational_competence_reading
 *   human_readable: Catastrophe Memory Transmission as Operational Competence Encoding
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   This constraint story models the OPERATIONAL COMPETENCE READING of
 *   catastrophe-memory transmission rituals — ritual as a functional
 *   knowledge-transmission system encoding survival-critical operational
 *   patterns. In this reading, Passover ritual encodes rapid-departure
 *   logistics and supply-chain discipline; Tisha B'Av encodes
 *   resource-scarcity response and threat-recognition patterns. The
 *   constraint coordinates distributed knowledge of 'how to survive crisis'
 *   across generations, with particular emphasis on behavioral rehearsal,
 *   resource rationing, collective mobilization, and threat assessment. This
 *   is distinct from the SYMBOL CONTINUITY READING (sibling constraint story)
 *   which emphasizes identity-constitutive and covenant-maintenance functions
 *   of the same rituals. The operational competence reading produces ε=0.22
 *   (low extractiveness, pure coordination) while the symbol reading is
 *   expected to produce higher extractiveness with Tangled Rope or Piton
 *   classification — different constraint types from the same
 *   natural-language phenomenon, decomposed per the ε-invariance principle.
 *   The temporal measurements show slow accumulation of theater_ratio over
 *   1000 years (0.15→0.35), indicating that operational function has
 *   partially degraded toward symbolic performance as catastrophic threat
 *   receded and stability increased. The extractiveness remains low
 *   throughout because the coordination function persists even as theater
 *   increases — the constraint never fully crosses into snare territory.
 *
 * KEY AGENTS:
 *   - Future Survival Capacity: Primary beneficiary (powerful/mobile) — the constraint solves coordination problem of transmitting survival competence across demographic discontinuity
 *   - Participating Community: Secondary beneficiary (moderate/mobile) — members who participate in rituals gain operational knowledge and coordination capacity
 *   - Institutional Custodian: Coordinator/extracting actor (institutional/constrained) — maintains ritual form and knowledge transmission but exercises gatekeeping power over interpretation
 *   - Compelled Participant: Trapped actor (powerless/trapped) — children and dependent community members experience obligatory participation without full agency or perceived benefit
 *   - Historical Lineage: Civilizational observer (powerful/mobile) — those who maintained ritual survived; those who lost it failed — retrospective validation of rope function
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks false summit by naturalizing contingent institutional arrangement as immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__operational_competence_reading, 0.22).
domain_priors:suppression_score(catastrophe_memory_transmission__operational_competence_reading, 0.18).
domain_priors:theater_ratio(catastrophe_memory_transmission__operational_competence_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__operational_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__operational_competence_reading, "Catastrophe Memory Transmission as Operational Competence Encoding").
narrative_ontology:topic_domain(catastrophe_memory_transmission__operational_competence_reading, "religious_studies/collective_memory/ritual_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__operational_competence_reading, 'ee361451-d179-4b33-82cb-2256e3c626c8').
narrative_ontology:cs_kernel_codification('ee361451-d179-4b33-82cb-2256e3c626c8', fixed_text).
narrative_ontology:cs_authority_grounding('ee361451-d179-4b33-82cb-2256e3c626c8', lineage).
narrative_ontology:cs_interpretation_layer_present('ee361451-d179-4b33-82cb-2256e3c626c8').
narrative_ontology:cs_reading_relation('ee361451-d179-4b33-82cb-2256e3c626c8', catastrophe_memory_transmission__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('ee361451-d179-4b33-82cb-2256e3c626c8', catastrophe_memory_transmission__hybrid_embedded_reading, influences).
narrative_ontology:cs_axiom('ee361451-d179-4b33-82cb-2256e3c626c8', foundational, ritual_encodes_operational_survival_competence).
narrative_ontology:cs_axiom_status(ritual_encodes_operational_survival_competence, holdable).
narrative_ontology:cs_axiom_grounding('ee361451-d179-4b33-82cb-2256e3c626c8', ritual_encodes_operational_survival_competence, empirically_contingent).
narrative_ontology:cs_axiom('ee361451-d179-4b33-82cb-2256e3c626c8', foundational, transmitted_competence_improves_crisis_survival).
narrative_ontology:cs_axiom_status(transmitted_competence_improves_crisis_survival, holdable).
narrative_ontology:cs_axiom_grounding('ee361451-d179-4b33-82cb-2256e3c626c8', transmitted_competence_improves_crisis_survival, empirically_contingent).
narrative_ontology:cs_reference_frame('ee361451-d179-4b33-82cb-2256e3c626c8', survival_competence_transmission_framework).
narrative_ontology:cs_drift_state('ee361451-d179-4b33-82cb-2256e3c626c8', contemporary_low_threat_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ee361451-d179-4b33-82cb-2256e3c626c8', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, future_survival_capacity).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, participating_community).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRACTICING COMMUNITY (ROPE) — Community members experience the ritual as genuine coordination mechanism: Passover's rapid-departure choreography rehearses supply-chain discipline, portion-control training, and threat-response readiness. No extraction occurs — the constraint solves a real collective action problem (how to maintain survival competence across demographic turnover). The community can exit by abandoning ritual, but faces cultural/identity cost. Classification is Rope: pure coordination, low extraction, genuine collective benefit.
constraint_indexing:constraint_classification(catastrophe_memory_transmission__operational_competence_reading, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: HISTORICAL SURVIVAL LINEAGE (ROPE) — From a civilizational timescale, ritual transmits operational competence (resource rationing, threat assessment, collective mobilization) across generations spanning catastrophes. This perspective verifies the rope classification: the constraint coordinates distributed knowledge (which behaviors prevent starvation, how to recognize danger, how to organize rapid movement) without extractive overhead. Lineages that maintained ritual competence survived; those that lost it failed. Pure coordination function, no asymmetric extraction.
constraint_indexing:constraint_classification(catastrophe_memory_transmission__operational_competence_reading, rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: INSTITUTIONAL CUSTODIAN (TANGLED ROPE) — Religious authority (rabbinical councils, institutional remembrancers) has genuine interest in transmitting survival competence but also faces institutional constraints on interpretation flexibility. The custodian experiences the constraint as mixed: genuine coordination function (encoding survival knowledge) layered with institutional extraction (monopoly on interpretation, control of knowledge transmission, gatekeeping who can lead ritual, enforcement of canonical form). Active enforcement of 'correct' ritual performance derives institutional power. Extraction is moderate and justified by coordination function, making Tangled Rope appropriate.
constraint_indexing:constraint_classification(catastrophe_memory_transmission__operational_competence_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: COMPELLED PARTICIPANT (SNARE) — For agents trapped in ritual obligation (children, dependent community members, those under social enforcement of participation), the constraint appears as extraction with minimal perceived coordination benefit. They experience obligatory participation, restricted choice, enforcement via shame/exclusion, and must absorb the constraint's suppression without agency. At this perspective, the constraint appears as Snare: high suppression (obligation + social enforcement), limited exit (trapped by family/community ties), minimal perceived coordination benefit from the agent's internal view.
constraint_indexing:constraint_classification(catastrophe_memory_transmission__operational_competence_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — From a universal/civilizational analytical view, ritual encoding of survival competence may appear as immutable natural law: all societies facing recurring catastrophic threat must transmit operational knowledge somehow; ritual is the discovered solution that works at scale. The constraint appears as an irreducible feature of how human communities maintain competence across demographic discontinuity. However, this classification is a FALSE SUMMIT CANDIDATE — the structural data reveals identifiable beneficiaries (custodian authority, lineage continuity, institutional power), suggesting the naturalization of a contingent institutional arrangement. The mountain perspective instantiates the oracle gap: the analytical observer's native instruments cannot detect extractive structure that cross-position analysis reveals.
constraint_indexing:constraint_classification(catastrophe_memory_transmission__operational_competence_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: DEGRADED CUSTODIAN (PITON) — In contemporary stable conditions (centuries without recurrence of originating catastrophe), institutional custodians maintain the ritual form through theater: the connection to survival function has atrophied, replaced by symbolic/theological justification. Participants recite rapid-departure narratives without actual supply-chain constraints, perform portion-control through symbolic portions rather than scarcity training, rehearse threat-response in stylized form. The institution persists because it has become identity-constitutive, not because the original coordination function is active. Theater ratio is high (symbolic rather than functional); extraction is low but institution is inertial. Piton classification fits: degraded but maintained through institutional continuity.
constraint_indexing:constraint_classification(catastrophe_memory_transmission__operational_competence_reading, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__operational_competence_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(catastrophe_memory_transmission__operational_competence_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(catastrophe_memory_transmission__operational_competence_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(catastrophe_memory_transmission__operational_competence_reading, TR),
    TR >= 0.70.

:- end_tests(catastrophe_memory_transmission__operational_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.22): Low. The constraint's primary function is coordination — encoding and transmitting survival-critical knowledge across generations without asymmetric extraction. The original catastrophe created genuine coordination problem (how do you transmit survival competence to people who have never experienced the threat?). Ritual solves this by encoding operational patterns (rapid departure, resource rationing, threat recognition) into symbolic/behavioral form. No agent systematically extracts surplus from others — beneficiaries are the entire community and future generations. The institutional custodian does exercise interpretive power (tangled_rope perspective) but this is coordinated extraction justified by knowledge-preservation function. Suppression (0.18): Low. Participation is generally voluntary at the community level; enforcement is primarily social (identity/belonging) rather than coercive. Some compelled participants (children, dependent family members) experience higher suppression, but the constraint's structural suppression is low because exit is available (apostasy, community departure). Theater ratio (0.35): Moderate. Operational competence encoding retains functional meaning even when actual threat is distant. A community that rehearses rapid departure maintains logistical readiness and collective-action capability even if the threat has not materialized for centuries. Theater increases over time as actual catastrophic threat recedes and communities lack recent experience, but the functional core persists.
 *
 * PERSPECTIVAL GAP:
 *   This reading produces a tight perspectival cluster: rope, rope, tangled_rope (with justifiable extraction), snare (compelled participants), mountain (false summit), piton (degraded custodian). The gap between the practicing community's view (rope: genuine coordination) and the compelled participant's view (snare: obligation with extraction) reveals the difference between voluntary coordinators and trapped agents. The gap between rope/tangled_rope (community and custodian experience) and snare (compelled participation) shows how the same constraint appears extractive when exit options are removed. The false summit mountain perspective reveals the analytical observer's risk of naturalizing what is a contingent institutional arrangement. The piton perspective shows how the constraint degrades as functional threat recedes and theater increases, but the functional core persists (unlike a true piton where function has fully atrophied).
 *
 * DIRECTIONALITY LOGIC:
 *   The operational competence reading derives directionality from the flow of survival knowledge, not from asymmetric extraction. Beneficiaries are those who gain operational competence (future generations, participating community); the constraint enables their survival and coordination capacity. Victims are primarily those who mistake symbol for substance, or compelled participants who experience suppression without perceived benefit. The custodian's directionality is split: beneficiary of institutional power (arbitrage exit, ability to interpret), but also coordinator of genuine knowledge transmission. The compelled participant's directionality is high (d≈0.85) because they face trapped exit and bear suppression cost without full participation benefit. The analytical observer's directionality is compromised (the native analytical frame naturalizes what structural analysis reveals as contingent), placing them at the false summit threshold.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy by committing to the operational competence function and measuring extractiveness relative to that function. If the constraint were claimed as pure coordination without the institutional custodian's gatekeeping power, it would be Rope (low extraction). If the custodian's power is included, it becomes Tangled Rope (mixed coordination and extraction). The operational reading does not claim pure Rope — it acknowledges the custodian's extractive element — but evaluates it as justified by coordination function. The low extractiveness (0.22) reflects that the extraction is modest and functionally necessary, not that it is absent. The constraint is not mandatrophic because it does not collapse the rope/snare distinction — the rope function is real and dominates the institutional extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_competence_artifact_threshold,
    'At what point does symbolic performance become decoupled from operational learning? When does ritual theater replace actual competence transmission?',
    'Empirical measurement of behavioral change in participants: comparison of resource-management decisions, threat-response latency, coordination capacity between high-ritual and low-ritual populations controlling for other education variables',
    'If strong coupling (ritual participants show measurable competence advantage): rope classification holds across perspectives. If weak/zero coupling: rope degrades toward piton, and constraint becomes theater rather than coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_competence_artifact_threshold, empirical, 'Decoupling point between symbolic ritual and operational competence').

omega_variable(
    kernel_reading_ambiguity,
    'Is the PRIMARY function of ritual to encode and transmit survival-critical operational knowledge, or to maintain symbolic continuity with past generations and divine covenant?',
    'This omega documents the kernel ambiguity: two readings of the same ritual practice produce different constraint types and different beneficiary/victim assignments. OPERATIONAL COMPETENCE READING (this story): function is knowledge transmission; constraint type Rope (community coordination); beneficiary is future survival capacity. SYMBOL CONTINUITY READING (sibling story): function is identity/covenant maintenance; constraint type may be Piton (degraded) or Tangled Rope (identity coordination with extraction). Different ε values, different perspectives, different victim sets.',
    'The reading split controls downstream classification: operational reading emphasizes measurable competence outcomes and treats theater as degradation; symbol reading emphasizes identity-constitutive function and treats theater as the actual mechanism. The readings coexist in contemporary practice — communities hold both frames simultaneously, which produces the perspectival gap.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Kernel split: operational competence vs. symbol continuity reading of same ritual').

omega_variable(
    identity_locked_vs_mobile_exit,
    'For participants in catastrophe-memory rituals, is the constraint on exit-option classification primarily structural (community/family economic/social barriers) or identity-constitutive (the participant''s self-concept is fused with ritual participation and lineage membership)?',
    'Longitudinal ethnographic study or interview analysis: participants who leave ritual practice and tracking whether suppression persists post-exit (indicator of internalized identity lock) or resolves (indicator of structural exit barriers). Post-exit psychological reintegration trajectories.',
    'If primarily structural: exit_options classify as trapped or constrained (material barriers), and the snare perspective holds. If primarily identity-locked: exit_options classify as identity_locked (cognitive/identity fusion), and classification shifts to rope at biographical horizon (the agent perceives the constraint as changeable in principle but cannot change it), revealing identity frame as the actual binding mechanism. This affects whether suppression is treated as structural or internalized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_vs_mobile_exit, empirical, 'Whether exit barriers are structural or identity-constitutive for compelled participants').

omega_variable(
    institutional_extraction_legitimacy,
    'Is the institutional custodian''s control of ritual interpretation and performance a justified coordination cost, or does it constitute asymmetric extraction justified only by monopoly?',
    'Comparative analysis: societies with centralized ritual custodianship vs. decentralized/peer-led ritual practice. Measurement of knowledge preservation accuracy, survival-competence outcomes, and institutional power concentration. Do communities with distributed interpretation maintain competence as well as centralized ones?',
    'If decentralized interpretation is equally effective at competence transmission: custodian extraction is contingent, not necessary. Constraint reclassifies toward rope and away from tangled_rope. If centralized custodianship demonstrates superior knowledge preservation: extraction is justified as coordination cost, tangled_rope classification holds. This affects whether institutional custodian is viewed as beneficiary or coordinator.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_extraction_legitimacy, empirical, 'Legitimacy of institutional custodian power in ritual knowledge transmission').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__operational_competence_reading, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmt_op_theater_t0, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cmt_op_theater_t500, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 500, 0.28).
narrative_ontology:measurement(cmt_op_theater_t1000, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 1000, 0.35).

% Extraction over time
narrative_ontology:measurement(cmt_op_extract_t0, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(cmt_op_extract_t500, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 500, 0.18).
narrative_ontology:measurement(cmt_op_extract_t1000, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 1000, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__operational_competence_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_transmission__operational_competence_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission__hybrid_embedded_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of catastrophe_memory_transmission kernel. The sibling readings (symbol_continuity, hybrid_embedded) interpret the same ritual phenomena differently, producing different constraint types and ε values. The three stories form a constraint family linked by kernel identity and structural codependence. The operational reading emphasizes measured competence outcomes and treats theater as degradation signal; the symbol reading emphasizes identity-constitutive function and treats theater as the mechanism; the hybrid reading models both simultaneously with potential conflict. Decomposition per ε-invariance principle: operational reading ε≈0.22 (rope), symbol reading ε≈0.40+ (tangled_rope/piton), hybrid reading ε≈0.30-0.45 (intermediate). The readings coexist in practice — contemporary communities hold multiple frames — which produces the rich perspectival structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_transmission__operational_competence_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
