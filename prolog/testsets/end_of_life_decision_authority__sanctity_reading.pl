% ============================================================================
% CONSTRAINT STORY: end_of_life_decision_authority__sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_decision_authority__sanctity_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: end_of_life_decision_authority__sanctity_reading
 *   human_readable: End-of-Life Decision Authority (Sanctity Reading)
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   The sanctity-of-life reading of end-of-life decision authority represents
 *   one axis of a three-way contest over who possesses legitimate authority
 *   to decide when human life should end. This reading declares that human
 *   life possesses intrinsic value independent of individual will or
 *   preference, and that intentional life-ending violates that value
 *   categorically, regardless of circumstances or consent. The constraint
 *   creates a tangled coordination-and-extraction hybrid: it coordinates
 *   meaning-making around human dignity within doctrine-holding communities
 *   (rope-like coordination function) while simultaneously extracting from
 *   pressured-vulnerable patients by denying them decision authority over
 *   their own death (snare-like asymmetric extraction). The structural delta
 *   — that pressured-vulnerable individuals enter the victim set specifically
 *   when euthanasia becomes available, that the physician is constrained to
 *   healer-only roles, and that individual suffering is externalized as an
 *   acceptable cost of doctrine maintenance — reveals how this reading
 *   operationalizes its metaphysical claim into institutional practice. The
 *   measurement trajectory shows increasing suppression (0.55 → 0.68) as
 *   enforcement mechanisms strengthen in response to countervailing
 *   movements, and modest increase in extractiveness (0.42 → 0.58) reflecting
 *   accumulating burden on pressured populations as autonomy-friendly
 *   jurisdictions create geographic arbitrage opportunities. Theater ratio
 *   remains low (0.38 → 0.44) because the sanctity doctrine's enforcement is
 *   substantially structural (legal prohibition, physician liability,
 *   resource allocation) rather than performative.
 *
 * KEY AGENTS:
 *   - Sanctity-doctrine communities: Beneficiaries (organized/constrained) — maintain institutional coherence around meaning-making frameworks grounded in human dignity doctrine; experience the constraint as coordination rather than extraction
 *   - Pressured-vulnerable patients: Primary victims (powerless/trapped) — terminal diagnosis, intractable suffering, economic burden on family; denied decision authority and trapped within the constraint's framework
 *   - Individuals with intractable suffering: Primary victims (powerless/trapped) — bear unbearable pain that the constraint frameworks (both sanctity and institutional enforcement) define as non-negotiable cost of doctrine maintenance
 *   - Healthcare institutions: Secondary beneficiary-victims (institutional/constrained) — required by law and professional liability to enforce sanctity doctrine; bear regulatory burden while ceding decision authority to doctrine holders
 *   - Compassionate physicians: Secondary victims (moderate/constrained) — constrained by both sanctity doctrine obligation and duty to relieve suffering; experience identity strain between healing mandate and prohibited actions
 *   - Analytical observer: Civilizational context (analytical/analytical) — risks naturalizing the contingent institutional commitment (sanctity doctrine) as an irreducible law of ethics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__sanctity_reading, 0.58).
domain_priors:suppression_score(end_of_life_decision_authority__sanctity_reading, 0.68).
domain_priors:theater_ratio(end_of_life_decision_authority__sanctity_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, theater_ratio, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__sanctity_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_decision_authority__sanctity_reading, "End-of-Life Decision Authority (Sanctity Reading)").
narrative_ontology:topic_domain(end_of_life_decision_authority__sanctity_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__sanctity_reading, '62b923f3-e824-4668-91cb-ed7757882fb8').
narrative_ontology:cs_kernel_codification('62b923f3-e824-4668-91cb-ed7757882fb8', formalized).
narrative_ontology:cs_authority_grounding('62b923f3-e824-4668-91cb-ed7757882fb8', lineage).
narrative_ontology:cs_interpretation_layer_present('62b923f3-e824-4668-91cb-ed7757882fb8').
narrative_ontology:cs_reading_relation('62b923f3-e824-4668-91cb-ed7757882fb8', end_of_life_decision_authority__autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('62b923f3-e824-4668-91cb-ed7757882fb8', end_of_life_decision_authority__vulnerability_protection_reading, influences).
narrative_ontology:cs_axiom('62b923f3-e824-4668-91cb-ed7757882fb8', foundational, human_life_intrinsic_value_independent_of_will).
narrative_ontology:cs_axiom_status(human_life_intrinsic_value_independent_of_will, holdable).
narrative_ontology:cs_axiom_grounding('62b923f3-e824-4668-91cb-ed7757882fb8', human_life_intrinsic_value_independent_of_will, deontological).
narrative_ontology:cs_axiom('62b923f3-e824-4668-91cb-ed7757882fb8', foundational, intentional_life_ending_categorically_impermissible).
narrative_ontology:cs_axiom_status(intentional_life_ending_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('62b923f3-e824-4668-91cb-ed7757882fb8', intentional_life_ending_categorically_impermissible, deontological).
narrative_ontology:cs_reference_frame('62b923f3-e824-4668-91cb-ed7757882fb8', sacred_life_preservation_mandate).
narrative_ontology:cs_drift_state('62b923f3-e824-4668-91cb-ed7757882fb8', contemporary_jurisdictional_pluralism, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('62b923f3-e824-4668-91cb-ed7757882fb8', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, sanctity_doctrine_holders).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, pressured_vulnerable_patients).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, individuals_with_intractable_suffering).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRESSURED VULNERABLE PATIENT (SNARE) — Terminal diagnosis, intractable suffering, economic burden on family, explicit prohibition of life-ending creates maximum extraction: the patient bears unbearable suffering while the constraint denies the only exit. Powerless, trapped, biographical horizon. No alternatives exist within the constraint's framework.
constraint_indexing:constraint_classification(end_of_life_decision_authority__sanctity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMPASSIONATE PHYSICIAN (TANGLED ROPE) — Constrained by both sanctity doctrine and duty to relieve suffering. Genuine coordination function: protecting life and maintaining trust in the healing relationship. Asymmetric extraction: the physician is forbidden from action while bearing witness to suffering, creating professional identity strain. Not fully trapped (can practice palliative care, advocate for doctrine revision) but constrained by institutional enforcement.
constraint_indexing:constraint_classification(end_of_life_decision_authority__sanctity_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SANCTITY-DOCTRINE COMMUNITY (ROPE) — Organized religious and philosophical communities derive coherence and identity from the sanctity doctrine. Genuine coordination function: maintaining a shared framework for meaning-making around death and human dignity. Experiences the constraint as coordination — shared commitment to protecting inherent human value. Constrained by countervailing movements toward autonomy-based frameworks but able to sustain doctrine through institutional reproduction.
constraint_indexing:constraint_classification(end_of_life_decision_authority__sanctity_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: HEALTHCARE INSTITUTION (TANGLED ROPE) — Required by law and professional liability to enforce the sanctity doctrine prohibition. Genuine coordination function: standardized end-of-life protocols prevent inconsistency and arbitrary decision-making. Asymmetric extraction: institutions bear regulatory burden while ceding decision authority to doctrine holders. Constrained by liability law, professional ethics boards, and shifting public opinion but unable to unilaterally reframe doctrine.
constraint_indexing:constraint_classification(end_of_life_decision_authority__sanctity_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL DOCTRINE ENFORCEMENT (PITON) — The ritual of sanctity-based end-of-life protocols (advance directives framed around 'do not resuscitate' rather than positive choice, spiritual counseling, family mediation through sanctity lens) persists largely through institutional inertia. The enforcement mechanism has degraded as consent-based decision frameworks have matured: the doctrine persists because it was codified and institutions have not fully reorganized around alternatives, not because it remains functionally dominant. Theater ratio reflects that much of the enforcement is performative (ritual language, theological justification) rather than structural necessity.
constraint_indexing:constraint_classification(end_of_life_decision_authority__sanctity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, human life's intrinsic value is foundational to ethical reasoning itself; any framework that permits intentional life-ending violates this irreducible commitment. Appears as an invariant across all ethical systems — a mountain. However, the structural data contradicts this: identifiable beneficiaries (doctrine-holding communities), victims (pressured vulnerable patients), and enforcement requirements reveal this as a constructed institutional arrangement, not a natural law. The engine's false summit detector will identify this as naturalization.
constraint_indexing:constraint_classification(end_of_life_decision_authority__sanctity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_decision_authority__sanctity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(end_of_life_decision_authority__sanctity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(end_of_life_decision_authority__sanctity_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_decision_authority__sanctity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(end_of_life_decision_authority__sanctity_reading, TR),
    TR >= 0.70.

:- end_tests(end_of_life_decision_authority__sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The sanctity reading extracts substantially from pressured-vulnerable populations by denying them decision authority while bearing the cost (unbearable suffering) of doctrine maintenance. The extraction is not maximal (0.70+) because some agents (compassionate physicians, healthcare institutions) experience only constrained rather than trapped conditions, and because the doctrine is sustained through institutional rather than purely coercive mechanisms. Suppression (0.68): High. Significant barriers to exercise of alternative end-of-life options include legal prohibition in sanctity-based jurisdictions, physician liability and conscience protection framed as enforcing sanctity doctrine, religious institutional pressure, family dynamics shaped by doctrine, and the absence of institutional infrastructure for autonomous decision-making. Suppression is not total (not 0.85+) because some jurisdictions have created legal pathways (advanced directives, medical assistance in dying in Canada/Netherlands/Belgium) that lower suppression for some populations, creating geographic arbitrage. Theater ratio (0.44): Moderate-low. The sanctity doctrine's enforcement is substantially structural — law, institutional rules, physician training — rather than performative. The piton perspective classification reflects that some institutional enforcement (ritual language around sanctity, spiritual counseling, family mediation through doctrine lens) has degraded as consent-based frameworks have matured, though the core legal and institutional structures remain intact. The modest theater ratio indicates the constraint retains genuine structural force (not merely performative) distinguishing it from piton-level degradation.
 *
 * PERSPECTIVAL GAP:
 *   The sanctity reading produces maximum perspectival divergence across positions. Doctrine-holder sees coordination (Rope); pressured-vulnerable sees pure extraction (Snare); compassionate physician sees mixed hybrid (Tangled Rope); institutional enforcement sees degraded ritual (Piton); analytical observer risks naturalizing (Mountain). This gap is not noise — it is diagnostic of the constraint's structure as a tangled coordination-extraction hybrid where institutional beneficiaries use coherent meaning-making language to justify extraction from vulnerable populations.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) follows from beneficiary/victim status and exit options. Sanctity-doctrine communities are beneficiaries with constrained exit options (ability to maintain doctrine within their institutional sphere, but constrained by countervailing movements toward autonomy readings) — derive moderate-to-low d, producing low-to-negative chi. Pressured-vulnerable patients are victims with trapped exit — derive high d (0.95+), producing maximum chi via f(d). Healthcare institutions are dual-positioned: beneficiaries of standardized protocols (institutional/constrained), victims of liability enforcement (institutional/constrained) — derive moderate d (~0.50). Physicians are victims with constrained exit — derive moderate-to-high d. The engine's derivation chain operates on these declared relationships, producing the perspectival gap: beneficiaries perceive low extractiveness while victims perceive maximum extraction from identical structural data.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in end-of-life authority is irreducible: the three readings cannot be held simultaneously in a single framework because they make incompatible claims about the locus of authority (doctrine, individual, institution). This constraint resolves mandatrophy by showing that the SANCTITY READING has specific structural consequences (pressured-vulnerable become victims, physician role narrows, suffering is externalized) that distinguish it from alternative readings. The tangled_rope classification captures that the constraint is genuinely coordinative for doctrine-holding communities while genuinely extractive for pressured-vulnerable populations — not a category mistake, but a structural fact that coordination and extraction coexist in this constraint. The false-summit perspective (mountain) reveals the analytical risk: the SANCTITY READING naturalizes its contingent institutional commitment (intrinsic human value) as an irreducible law of ethics, foreclosing the alternative readings at the level of logic rather than acknowledging them as live interpretive options within the contested kernel.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intrinsic_value_foundation,
    'Is the sanctity doctrine grounded in genuine metaphysical properties of human life, or does it represent a contingent institutional and cultural commitment?',
    'Cross-cultural analysis of how different traditions ground human dignity; historical tracking of when sanctity doctrine codification occurred relative to pre-existing medical practices; examination of whether the doctrine''s core claims (that ending life is categorically impermissible) survive removal of the institutional enforcement structure.',
    'If metaphysically grounded: mountain classification is correct and the reading reflects genuine constraint on all ethical systems. If contingent: false-summit signature should trigger and classification should downgrade to tangled_rope. Alternative readings (autonomy, vulnerability) would be interpretations of the same kernel, not falsifications of the foundation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intrinsic_value_foundation, conceptual, 'Whether sanctity is metaphysical or institutional').

omega_variable(
    suffering_externalization_mechanism,
    'By restricting life-ending options, does the sanctity reading externalize unbearable suffering costs onto patients, or does it prevent a different harm (normalization of lethal decision-making that eventually captures vulnerable populations)?',
    'Comparative harm analysis: jurisdictions with vs without sanctity-based restrictions; longitudinal tracking of whether removal of restrictions leads to coercive practice (Netherlands, Belgium data); analysis of palliative-care access and quality across regimes to determine whether suffering is truly unavoidable or a function of resource allocation.',
    'If suffering is genuinely unavoidable and imposed: victim set is correct (pressured vulnerable enter at high magnitude). If suffering is resource-dependent and removal of restrictions creates new coercive dynamics: victim set may be incomplete or mis-identified, and tangled_rope classification obscures the true harm structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suffering_externalization_mechanism, empirical, 'Whether sanctity doctrine externalizes unavoidable or remediable suffering').

omega_variable(
    physician_role_definition_authority,
    'Who has authority to define the physician''s role — the healing mandate, the boundary between healing and harm? Does the sanctity reading inherit this authority from medical tradition (physicians as life-preservers) or impose it via doctrine over physician practice?',
    'Historical analysis of medical role definitions pre- and post-sanctity codification; examination of physician statements of conscience and role conflict; comparative analysis of how autonomy and vulnerability readings reframe the physician''s mandate and whether physicians accept or resist those reframings.',
    'If physicians inherit the healing-only role from tradition: the sanctity reading is continuous with medical practice, not imposed against it. If the sanctity reading is doctrine imposed over physician judgment: the constraint is more extractive (physicians are victims of doctrine enforcement) and the classification may need revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physician_role_definition_authority, conceptual, 'Authority over physician role definition').

omega_variable(
    kernel_reading_contest_structure,
    'How do the three readings of the end-of-life decision authority kernel logically relate to one another? Can a single framework hold two readings simultaneously, or does commitment to one reading logically foreclose others?',
    'Philosophical analysis of the core premises: Does autonomy-reading (individual sovereignty over death) logically contradict sanctity-reading (intrinsic value independent of will)? Does vulnerability-protection-reading (distributed institutional checkpoints) depend on accepting or rejecting either autonomy or sanctity premises? Examination of actual jurisdictions that have attempted to hold multiple readings simultaneously (e.g., legally permitting assisted dying while codifying sanctity-based safeguards).',
    'If readings foreclose one another: the kernel contest is zero-sum and mandatrophy is irreducible. If readings coexist: the constraint landscape is multiplex and the engine should classify the set of readings as a three-point presheaf, not pick a single type. If one reading influences others: causality runs upstream to downstream and the network effects become visible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, conceptual, 'Logical structure of kernel reading relationships').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__sanctity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eol_sanctity_tr_t0, end_of_life_decision_authority__sanctity_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(eol_sanctity_tr_t15, end_of_life_decision_authority__sanctity_reading, theater_ratio, 15, 0.41).
narrative_ontology:measurement(eol_sanctity_tr_t30, end_of_life_decision_authority__sanctity_reading, theater_ratio, 30, 0.44).

% Extraction over time
narrative_ontology:measurement(eol_sanctity_be_t0, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(eol_sanctity_be_t15, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(eol_sanctity_be_t30, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(eol_sanctity_su_t0, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(eol_sanctity_su_t15, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(eol_sanctity_su_t30, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__sanctity_reading, identity_coordination).
narrative_ontology:affects_constraint(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority__vulnerability_protection_reading).

% DUAL FORMULATION NOTE:
% The end-of-life decision authority kernel decomposes into three structurally distinct readings: SANCTITY_READING (this constraint, ε=0.58, Tangled Rope); AUTONOMY_READING (separate story, autonomy-centered, different victim/beneficiary set, likely different ε); VULNERABILITY_PROTECTION_READING (separate story, institutional-distribution-centered, likely different ε and type). All three link to one another via network.affects_constraints. Each reading instantiates the same kernel but produces different classifications and different structural deltas because they make incommensurable claims about authority locus.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
