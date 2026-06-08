% ============================================================================
% CONSTRAINT STORY: trauma_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trauma_encoding_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: trauma_encoding_reading
 *   human_readable: Ritual Trauma Encoding as Intergenerational Warning System
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   Trauma-encoding rituals transmit catastrophe memory across generations
 *   through embodied re-enactment, creating an intergenerational warning
 *   system against threat recurrence. This reading of the catastrophe memory
 *   kernel focuses on the psychological mechanism: ritual practice inscribes
 *   ancestral trauma into descendant identity, producing hypervigilance as
 *   collective threat-detection capacity. The constraint exhibits genuine
 *   coordination function (early-warning system, group cohesion under
 *   existential threat) alongside substantial extraction (psychological
 *   burden imposed on descendants who did not experience the original
 *   catastrophe). Extractiveness increases over generational distance as the
 *   original threat recedes but the trauma-transmission mechanism persists.
 *   Theater ratio is low initially (ritual is functionally adaptive in
 *   immediate post-catastrophe generations) but rises as threat recedes and
 *   ritual becomes increasingly performative maintenance of memory rather
 *   than active threat-response training. Suppression requirement decreases
 *   slightly over the interval as geographic mobility and secular
 *   alternatives reduce the community's capacity to enforce participation,
 *   but remains substantial due to identity-lock mechanism.
 *
 * KEY AGENTS:
 *   - Descendant Bearer: Primary victim (powerless/identity_locked) — born into ritual obligation, cannot exit without self-dissolution, bears maximum psychological burden
 *   - Ritual Participant: Mixed position (moderate/constrained) — adult community member experiencing both coordination benefit (threat-awareness) and extraction cost (trauma re-enactment)
 *   - Ritual Authority: Primary beneficiary (institutional/arbitrage) — religious leadership whose authority is grounded in catastrophe memory administration
 *   - Collective Threat Vigilance: Abstract beneficiary (powerless/trapped) — the community's early-warning capacity, which cannot organize or exit but benefits from trauma-encoding mechanism
 *   - Trauma-Informed Reform Coalition: Organized agents (organized/mobile) — building alternative memory practices with sunset logic, seeing trauma-encoding as temporary necessity
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees irreducible structural hybridity, both coordination and extraction are real and neither eliminable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trauma_encoding_reading, 0.58).
domain_priors:suppression_score(trauma_encoding_reading, 0.62).
domain_priors:theater_ratio(trauma_encoding_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trauma_encoding_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(trauma_encoding_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(trauma_encoding_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trauma_encoding_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(trauma_encoding_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trauma_encoding_reading, tangled_rope).
narrative_ontology:human_readable(trauma_encoding_reading, "Ritual Trauma Encoding as Intergenerational Warning System").
narrative_ontology:topic_domain(trauma_encoding_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(trauma_encoding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trauma_encoding_reading, 'f6180eaa-c59d-42f4-8ce1-cee6b5bba8f0').
narrative_ontology:cs_kernel_codification('f6180eaa-c59d-42f4-8ce1-cee6b5bba8f0', distributed).
narrative_ontology:cs_authority_grounding('f6180eaa-c59d-42f4-8ce1-cee6b5bba8f0', lineage).
narrative_ontology:cs_interpretation_layer_present('f6180eaa-c59d-42f4-8ce1-cee6b5bba8f0').
narrative_ontology:cs_reading_relation('f6180eaa-c59d-42f4-8ce1-cee6b5bba8f0', trauma_encoding_reading__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('f6180eaa-c59d-42f4-8ce1-cee6b5bba8f0', trauma_encoding_reading__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('f6180eaa-c59d-42f4-8ce1-cee6b5bba8f0', trauma_encoding_reading__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('f6180eaa-c59d-42f4-8ce1-cee6b5bba8f0', foundational, trauma_transmission_preserves_vigilance).
narrative_ontology:cs_axiom_status(trauma_transmission_preserves_vigilance, holdable).
narrative_ontology:cs_axiom_grounding('f6180eaa-c59d-42f4-8ce1-cee6b5bba8f0', trauma_transmission_preserves_vigilance, empirically_contingent).
narrative_ontology:cs_axiom('f6180eaa-c59d-42f4-8ce1-cee6b5bba8f0', foundational, descendant_burden_justified_by_collective_benefit).
narrative_ontology:cs_axiom_status(descendant_burden_justified_by_collective_benefit, holdable).
narrative_ontology:cs_axiom_grounding('f6180eaa-c59d-42f4-8ce1-cee6b5bba8f0', descendant_burden_justified_by_collective_benefit, deontological).
narrative_ontology:cs_reference_frame('f6180eaa-c59d-42f4-8ce1-cee6b5bba8f0', post_catastrophe_immediate_generation).
narrative_ontology:cs_drift_state('f6180eaa-c59d-42f4-8ce1-cee6b5bba8f0', contemporary_fourth_generation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f6180eaa-c59d-42f4-8ce1-cee6b5bba8f0', '').
narrative_ontology:cs_kernel_id(trauma_encoding_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trauma_encoding_reading, collective_threat_vigilance).
narrative_ontology:constraint_beneficiary(trauma_encoding_reading, ritual_authority_structure).
narrative_ontology:constraint_victim(trauma_encoding_reading, descendant_psychological_burden).
narrative_ontology:constraint_victim(trauma_encoding_reading, present_generation_children).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(trauma_encoding_reading, ritual_participant).
narrative_ontology:constraint_beneficiary(trauma_encoding_reading, ritual_authority).
narrative_ontology:constraint_victim(trauma_encoding_reading, descendant_bearer).
narrative_ontology:constraint_victim(trauma_encoding_reading, ritual_participant).
narrative_ontology:constraint_vindicates(trauma_encoding_reading, transgenerational_trauma_transmission_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Born into ritual community with obligation to carry ancestral trauma as identity-constituting memory. Participates in annual re-enactment rituals that inscribe catastrophe narrative into embodied experience. Cannot exit without abandoning kinship bonds and community identity — leaving would require dissolving the self-concept constructed through ritual participation. Bears psychological burden (hypervigilance, anxiety, trauma symptoms) without having experienced the original catastrophe.
narrative_ontology:constraint_stakeholder(trauma_encoding_reading, descendant_bearer, payer,
    powerless, biographical, identity_locked, local).

% Adult community member who both benefits from collective threat-awareness (early-warning capacity against recurrence of historical catastrophe) and bears cost of trauma re-enactment (psychological burden of repeated exposure to catastrophe narrative through ritual). Can leave community but at cost of social penalty and loss of support structures. Experiences ritual as both meaningful identity practice and psychologically demanding obligation.
narrative_ontology:constraint_stakeholder(trauma_encoding_reading, ritual_participant, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(trauma_encoding_reading, ritual_participant, payer).

% Religious leadership that administers trauma-encoding rituals and interprets catastrophe memory tradition. Authority is grounded in stewardship of catastrophe narrative and ritual practice. Can reframe or modify ritual with institutional legitimacy. Benefits from authority position while experiencing constraint as coordination mechanism for collective vigilance.
narrative_ontology:constraint_stakeholder(trauma_encoding_reading, ritual_authority, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(trauma_encoding_reading, ritual_authority, beneficiary).

% Abstract collective good: the community's capacity to recognize and respond to threats analogous to the historical catastrophe. Cannot organize or exit. Benefits from trauma-encoding mechanism's production of hypervigilance and threat-detection capacity across generations.
narrative_ontology:constraint_stakeholder(trauma_encoding_reading, collective_threat_vigilance, beneficiary,
    powerless, civilizational, trapped, regional).
narrative_ontology:stakeholder_non_agent(trauma_encoding_reading, collective_threat_vigilance).

% Mental health professionals, progressive religious scholars, and survivor advocacy groups working to transform catastrophe memory practices. Building alternative memory mechanisms (testimony archives, educational curricula, commemorative art) that preserve warning function without imposing descendant psychological burden. See trauma-encoding as temporary necessity requiring sunset as threat recedes.
narrative_ontology:constraint_stakeholder(trauma_encoding_reading, reform_coalition, observer,
    organized, civilizational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Collective threat-detection and early-warning capacity against recurrence of historical catastrophe. Ritual practice transmits threat-recognition patterns and appropriate response behaviors across generations, maintaining vigilance when original survivors are no longer present.
% TRANSFER_FUNCTION: Psychological burden (trauma symptoms, hypervigilance, anxiety) flows from ritual authority structure to descendant bearers. Authority and legitimacy flow from catastrophe memory stewardship to ritual leadership. Threat-awareness capacity flows from trauma-encoding mechanism to collective.
% ABSENT_VOICES: Descendants who left the community (exit narratives are systematically excluded from ritual discourse). Mental health professionals outside the tradition (clinical perspective on transgenerational trauma is not represented in ritual authority structure). Children too young to consent to ritual participation (their future psychological burden is not weighed in ritual design).
% DISAPPEARANCE_RATIONALE: If trauma-encoding rituals disappeared, the community's threat-detection capacity would require reconstruction through alternative mechanisms. Descendant bearers would lose identity-constituting practice (for better or worse). Ritual authority structure would lose legitimacy grounding. Reform coalition's alternative memory practices would become primary rather than supplementary. The rearrangement would be substantial — multiple stakeholder arrangements depend on this constraint's persistence.
% FOUNDING_PROBLEM: Immediate post-catastrophe generation faced the problem of transmitting threat-recognition capacity to descendants who would not directly experience the catastrophe. Without transmission mechanism, future generations would be vulnerable to recurrence. Trauma-encoding ritual was adaptive solution: inscribe catastrophe memory into descendant identity through embodied re-enactment, producing hypervigilance as early-warning system.
% FOUNDING_PROBLEM_CORROBORATION: Ritual authority structure attests founding problem remains live (threats analogous to historical catastrophe persist, vigilance is necessary). Reform coalition attests founding problem is substantially resolved (immediate threat has receded, alternative memory practices can preserve warning function without psychological burden). Historical analysis (external scholars) confirms founding problem was live in immediate post-catastrophe generations but shows threat has substantially receded over generational distance. No consensus on current status — the dispute is genuine.
narrative_ontology:disappearance_verdict(trauma_encoding_reading, world_rearranges).
narrative_ontology:founding_problem_status(trauma_encoding_reading, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DESCENDANT BEARER (SNARE) — Born into ritual obligation to carry ancestral trauma as identity-constituting memory. Cannot exit without abandoning community identity and kinship bonds. Experiences maximum extraction: psychological burden imposed without consent, framed as sacred duty. Identity-locked rather than trapped because exit is structurally possible (geographic mobility exists) but cognitively foreclosed — leaving would require dissolving the self-concept constructed through ritual participation.
constraint_indexing:constraint_classification(trauma_encoding_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: RITUAL PARTICIPANT (TANGLED ROPE) — Adult community member who both benefits from collective threat-awareness and bears cost of trauma re-enactment. Constrained exit: leaving is possible but carries social penalty and loss of community support structures. Experiences genuine coordination (early-warning system against recurrence of historical catastrophe) alongside extraction (psychological cost of repeated trauma exposure through ritual). Mixed experience — neither pure beneficiary nor pure victim.
constraint_indexing:constraint_classification(trauma_encoding_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RITUAL AUTHORITY (ROPE) — Religious leadership that administers trauma-encoding rituals. Benefits from institutional authority grounded in catastrophe memory; experiences constraint as coordination mechanism for collective vigilance. Arbitrage exit: can reframe or modify ritual practice with institutional legitimacy. Low effective extraction — the constraint subsidizes this agent's authority position.
constraint_indexing:constraint_classification(trauma_encoding_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: TRAUMA-INFORMED REFORM COALITION (SCAFFOLD) — Mental health professionals, progressive religious scholars, and survivor advocacy groups working to transform catastrophe memory practices toward healing rather than re-traumatization. See the trauma-encoding mechanism as temporary: necessary in immediate post-catastrophe generations when threat was live, but requiring sunset as threat recedes and psychological costs accumulate. Building alternative memory practices (testimony archives, educational curricula, commemorative art) that preserve warning function without imposing descendant burden.
constraint_indexing:constraint_classification(trauma_encoding_reading, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, trauma-encoding rituals exhibit genuine coordination function (collective threat-detection, group cohesion under existential threat) alongside substantial extraction (psychological burden on descendants, potential for maladaptive hypervigilance). The constraint is not a false summit — the coordination function is real and historically adaptive. But the extraction is also real and increases over generational distance from the original catastrophe. Tangled rope classification reflects irreducible structural hybridity: both functions are present and neither can be eliminated without destroying the other.
constraint_indexing:constraint_classification(trauma_encoding_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trauma_encoding_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(trauma_encoding_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trauma_encoding_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(trauma_encoding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trauma_encoding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Descendants bear substantial psychological burden (hypervigilance, anxiety, trauma symptoms) for collective threat-detection benefit. The extraction is real but not maximal — some descendants report the ritual as identity-constituting and meaningful despite its psychological cost. The value reflects that trauma transmission imposes genuine costs on non-consenting future generations while providing genuine coordination benefit to the collective. Suppression (0.62): Moderate-high. Identity-lock mechanism creates cognitive barrier to exit; community enforcement through kinship bonds and social penalty; geographic mobility provides some structural exit but at high relational cost. Suppression decreases slightly over the interval as secular alternatives emerge and enforcement capacity erodes. Theater ratio (0.35): Low-moderate. Ritual retains substantial functional content (trauma transmission genuinely produces threat-vigilance) but performative component increases over generational distance as original threat recedes. The ritual becomes increasingly about memory maintenance rather than active threat-response training. Accessibility collapse (0.15) and resistance (0.45): Not a natural law — alternatives exist (testimony archives, educational curricula) and face moderate resistance from ritual authority structure.
 *
 * PERSPECTIVAL GAP:
 *   The descendant bearer sees snare (identity-locked extraction with no coordination benefit visible from their biographical horizon). The ritual participant sees tangled rope (genuine coordination alongside genuine extraction, both irreducible). The ritual authority sees rope (coordination mechanism for collective vigilance, with authority benefits experienced as legitimate reward for stewardship). The reform coalition sees scaffold (temporary necessity with sunset logic as threat recedes and alternatives mature). The analytical observer sees tangled rope at civilizational scope (both functions are real, neither eliminable without destroying the other). The gap reveals how generational distance and structural position determine whether the trauma-encoding mechanism appears as adaptive coordination or extractive burden.
 *
 * DIRECTIONALITY LOGIC:
 *   Descendant bearers are full victims with identity-locked exit, producing high directionality toward full target (d approaching 1.0). Ritual participants are mixed — declared as both beneficiaries (threat-vigilance) and victims (psychological burden), with constrained exit, producing moderate directionality (d around 0.5). Ritual authority is pure beneficiary with arbitrage exit, producing low directionality toward full beneficiary (d approaching 0.0). The reform coalition is beneficiary (building alternatives) with mobile exit, producing low directionality. Analytical observer has no structural relationship to extraction flow, producing neutral directionality. The directionality derivation captures the structural asymmetry: those born into the ritual bear costs they did not choose, while those administering it collect authority benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that trauma-encoding rituals are genuinely hybrid: the coordination function (collective threat-detection) and the extraction function (descendant psychological burden) are structurally inseparable within this mechanism. The ritual authority's rope perspective is not false consciousness — they genuinely experience coordination. The descendant bearer's snare perspective is not misperception — they genuinely experience extraction. The analytical tangled rope classification captures the irreducible structural reality: both functions coexist and neither can be eliminated without destroying the mechanism itself. The scaffold perspective introduces temporal dynamics: what is adaptive coordination in immediate post-catastrophe generations becomes increasingly extractive as generational distance increases and threat recedes. Mandatrophy is resolved not by choosing one type but by recognizing that the constraint's type changes across perspectives and across time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is catastrophe memory ritual primarily a trauma-transmission mechanism (this reading), a symbol-continuity system (sibling reading), a survival-competence training protocol (sibling reading), or a boundary-maintenance mechanism (sibling reading)?',
    'Cross-reading comparison: measure psychological burden on descendants (trauma reading), measure symbol recognition and transmission fidelity (continuity reading), measure behavioral preparedness for threat recurrence (competence reading), measure in-group/out-group boundary strength (boundary reading). Each reading predicts different primary effects.',
    'If trauma transmission is primary: high extractiveness confirmed, reform coalition''s sunset logic validated. If symbol continuity is primary: lower extractiveness, ritual is coordination with minimal psychological cost. If survival competence is primary: extractiveness is instrumental cost for adaptive benefit. If boundary maintenance is primary: extractiveness serves in-group cohesion rather than threat-detection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Which reading of the catastrophe memory kernel is structurally primary').

omega_variable(
    generational_distance_threshold,
    'At what generational distance from the original catastrophe does the trauma-encoding mechanism''s extraction cost exceed its coordination benefit?',
    'Longitudinal study of ritual communities: measure threat-detection accuracy, measure psychological burden (PTSD symptoms, anxiety disorders, hypervigilance), compare communities at different generational distances from founding catastrophe. Identify inflection point where costs exceed benefits.',
    'If threshold < 3 generations: trauma encoding is maladaptive for most contemporary communities, scaffold perspective confirmed. If threshold > 5 generations: trauma encoding remains adaptive longer than reform coalition assumes, extraction is justified coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(generational_distance_threshold, empirical, 'Generational distance threshold where extraction exceeds coordination benefit').

omega_variable(
    alternative_memory_efficacy,
    'Do alternative memory practices (testimony archives, educational curricula, commemorative art) preserve threat-detection function without imposing descendant psychological burden?',
    'Comparative study: communities using trauma-encoding rituals vs communities using alternative memory practices. Measure threat-detection accuracy (recognition of warning signs, appropriate response to analogous threats) and psychological burden (trauma symptoms in descendants).',
    'If alternatives are equally effective: scaffold sunset is structurally feasible, extraction is unnecessary. If alternatives are less effective: trauma encoding''s psychological cost is irreducible price of vigilance, tangled rope persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_memory_efficacy, empirical, 'Whether alternative memory practices preserve coordination function without extraction').

omega_variable(
    identity_lock_mechanism,
    'Is the descendant''s identity-lock primarily cognitive (internalized framing that exit is unthinkable) or relational (exit would sever kinship bonds that constitute identity)?',
    'Qualitative analysis of exit narratives: interview individuals who left ritual communities. Distinguish cognitive barriers (could not imagine leaving while inside the frame) from relational barriers (leaving required severing constitutive relationships). Measure which barrier was primary in preventing earlier exit.',
    'If primarily cognitive: identity-lock is perceptual filter on structural mobility, therapeutic intervention could shift frame. If primarily relational: identity-lock is structural fact of kinship-constituted identity, exit requires literal self-dissolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether identity-lock is cognitive framing or relational structure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trauma_encoding_reading, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trauma_enc_theater_t0, trauma_encoding_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(trauma_enc_theater_t1, trauma_encoding_reading, theater_ratio, 1, 0.2).
narrative_ontology:measurement(trauma_enc_theater_t2, trauma_encoding_reading, theater_ratio, 2, 0.25).
narrative_ontology:measurement(trauma_enc_theater_t3, trauma_encoding_reading, theater_ratio, 3, 0.3).
narrative_ontology:measurement(trauma_enc_theater_t4, trauma_encoding_reading, theater_ratio, 4, 0.33).
narrative_ontology:measurement(trauma_enc_theater_t5, trauma_encoding_reading, theater_ratio, 5, 0.35).

% Extraction over time
narrative_ontology:measurement(trauma_enc_extract_t0, trauma_encoding_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(trauma_enc_extract_t1, trauma_encoding_reading, base_extractiveness, 1, 0.35).
narrative_ontology:measurement(trauma_enc_extract_t2, trauma_encoding_reading, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(trauma_enc_extract_t3, trauma_encoding_reading, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(trauma_enc_extract_t4, trauma_encoding_reading, base_extractiveness, 4, 0.56).
narrative_ontology:measurement(trauma_enc_extract_t5, trauma_encoding_reading, base_extractiveness, 5, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(trauma_enc_suppress_t0, trauma_encoding_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(trauma_enc_suppress_t1, trauma_encoding_reading, suppression_requirement, 1, 0.68).
narrative_ontology:measurement(trauma_enc_suppress_t2, trauma_encoding_reading, suppression_requirement, 2, 0.66).
narrative_ontology:measurement(trauma_enc_suppress_t3, trauma_encoding_reading, suppression_requirement, 3, 0.64).
narrative_ontology:measurement(trauma_enc_suppress_t4, trauma_encoding_reading, suppression_requirement, 4, 0.63).
narrative_ontology:measurement(trauma_enc_suppress_t5, trauma_encoding_reading, suppression_requirement, 5, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trauma_encoding_reading, identity_coordination).
narrative_ontology:affects_constraint(trauma_encoding_reading, symbol_continuity_reading).
narrative_ontology:affects_constraint(trauma_encoding_reading, survival_competence_reading).
narrative_ontology:affects_constraint(trauma_encoding_reading, boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_kernel decomposes into four structurally distinct readings with different ε values. The trauma_encoding_reading (this constraint) has the highest extractiveness (0.58) because it foregrounds descendant psychological burden. The symbol_continuity_reading has lower ε (~0.30) because semiotic transmission imposes lower psychological cost. The survival_competence_reading has moderate ε (~0.40) because behavioral training imposes instrumental cost. The boundary_maintenance_reading has moderate-high ε (~0.50) because social boundary enforcement imposes exclusion costs. Each reading is a distinct constraint linked via network.affects_constraints, not one constraint with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
