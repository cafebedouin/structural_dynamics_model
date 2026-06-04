% ============================================================================
% CONSTRAINT STORY: royal_assent_convention__advice_bound_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_royal_assent_convention__advice_bound_reading, []).

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
 *   constraint_id: royal_assent_convention__advice_bound_reading
 *   human_readable: Royal Assent Convention: Advice-Bound Reading
 *   domain: constitutional_law/executive_governance
 *
 * KEY AGENTS:
 *   - Monarch: Primary victim (powerless/trapped) — structurally erased of discretion under this reading; even refusal would be the government's act
 *   - Government of the Day: Primary beneficiary (institutional/arbitrage) — concentrates legislative power by ensuring royal assent cannot be arbitrarily withheld
 *   - Opposition Parties and Civil Society: Secondary victim/beneficiary (moderate/constrained) — benefit from predictability but lose countervailing discretion from Crown
 *   - Constitutional Doctrine: Institutional curator (institutional/constrained) — formulates and maintains the advice-bound reading as settled doctrine through legal education and practice
 *   - Constitutional Interpretation Community: Analytical observer (analytical/analytical) — debates whether the reading is settled principle or contingent institutional arrangement
 *   - Parliament and Ministers: Beneficiary agents (institutional/arbitrage) — profit from the reading's concentration of legislative power in the executive
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(royal_assent_convention__advice_bound_reading, 0.58).
domain_priors:suppression_score(royal_assent_convention__advice_bound_reading, 0.72).
domain_priors:theater_ratio(royal_assent_convention__advice_bound_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(royal_assent_convention__advice_bound_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(royal_assent_convention__advice_bound_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(royal_assent_convention__advice_bound_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(royal_assent_convention__advice_bound_reading, tangled_rope).
narrative_ontology:human_readable(royal_assent_convention__advice_bound_reading, "Royal Assent Convention: Advice-Bound Reading").
narrative_ontology:topic_domain(royal_assent_convention__advice_bound_reading, "constitutional_law/executive_governance").

domain_priors:requires_active_enforcement(royal_assent_convention__advice_bound_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(royal_assent_convention__advice_bound_reading, 'cd4584e2-2803-4745-87cd-221afd0f8499').
narrative_ontology:cs_kernel_codification('cd4584e2-2803-4745-87cd-221afd0f8499', formalized).
narrative_ontology:cs_authority_grounding('cd4584e2-2803-4745-87cd-221afd0f8499', lineage).
narrative_ontology:cs_interpretation_layer_present('cd4584e2-2803-4745-87cd-221afd0f8499').
narrative_ontology:cs_reading_relation('cd4584e2-2803-4745-87cd-221afd0f8499', royal_assent_convention__dead_letter_reading, forecloses).
narrative_ontology:cs_reading_relation('cd4584e2-2803-4745-87cd-221afd0f8499', royal_assent_convention__reserve_power_reading, coexists_with).
narrative_ontology:cs_axiom('cd4584e2-2803-4745-87cd-221afd0f8499', foundational, monarch_constitutional_conduit).
narrative_ontology:cs_axiom_status(monarch_constitutional_conduit, holdable).
narrative_ontology:cs_axiom_grounding('cd4584e2-2803-4745-87cd-221afd0f8499', monarch_constitutional_conduit, conventional).
narrative_ontology:cs_axiom('cd4584e2-2803-4745-87cd-221afd0f8499', secondary, discretion_incompatible_with_parliamentary_sovereignty).
narrative_ontology:cs_axiom_status(discretion_incompatible_with_parliamentary_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('cd4584e2-2803-4745-87cd-221afd0f8499', discretion_incompatible_with_parliamentary_sovereignty, instrumental).
narrative_ontology:cs_reference_frame('cd4584e2-2803-4745-87cd-221afd0f8499', parliamentary_supremacy_with_nominal_royal_dignity).
narrative_ontology:cs_drift_state('cd4584e2-2803-4745-87cd-221afd0f8499', contemporary_2020s, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('cd4584e2-2803-4745-87cd-221afd0f8499', '').
narrative_ontology:cs_kernel_id(royal_assent_convention__advice_bound_reading, royal_assent_convention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(royal_assent_convention__advice_bound_reading, executive_government).
narrative_ontology:constraint_victim(royal_assent_convention__advice_bound_reading, monarch_discretionary_agency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE MONARCH-AS-ACTOR (SNARE) — Under the advice-bound reading, the monarch has no exit from the ministerial instruction. Even refusal would be the government's act, not the monarch's. Personal discretion is structurally erased; the monarch is a conduit with no meaningful choice. Maximum suppression and extraction: the monarch's potential agency is extracted and relocated to ministers. The monarch cannot exit this framework without ceasing to be a constitutional actor.
constraint_indexing:constraint_classification(royal_assent_convention__advice_bound_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: OPPOSITION PARTIES / CIVIL SOCIETY (TANGLED ROPE) — These actors benefit from some genuine coordination: the advice-bound convention removes one class of arbitrary power (capricious royal refusal) and stabilizes executive authority through a clear rule. They are also victims of extraction: the convention concentrates executive power in the cabinet by stripping countervailing discretion from the Crown. They can mobilize (organize) to challenge executive action but cannot exit the convention without constitutional amendment.
constraint_indexing:constraint_classification(royal_assent_convention__advice_bound_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE GOVERNMENT OF THE DAY (ROPE) — Primary beneficiary. The advice-bound reading is experienced as pure coordination: it removes uncertainty about royal refusal and provides a stable mechanism for legislation. The government can exit this convention only via constitutional amendment (arbitrage option — high cost but available in principle). The convention serves their interests by concentrating legislative power in the executive and ministers.
constraint_indexing:constraint_classification(royal_assent_convention__advice_bound_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL DOCTRINE (PITON) — Legal formalism teaches the advice-bound reading as settled doctrine: the Crown acts on ministerial advice, the monarch is a conduit. This perspective sees the reading as a stabilized, formalized principle. But the reading is partly theatrical: doctrine asserts the monarch is without discretion, yet three centuries of disuse and the reserve-power shadow suggest latent discretion persists. The doctrine maintains itself through repetition and inertia, not through functional necessity.
constraint_indexing:constraint_classification(royal_assent_convention__advice_bound_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the advice-bound reading can appear as an immutable feature of how parliamentary democracy must function: executive power requires unity of decision, and the Crown must be a conduit to prevent arbitrary veto. Discretion in the Crown would paralyze government. This perspective risks naturalizing a contingent doctrinal choice as a law of institutional mechanics.
constraint_indexing:constraint_classification(royal_assent_convention__advice_bound_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: CONSTITUTIONAL INTERPRETATION COMMUNITY (TANGLED ROPE) — Scholars and jurists experience the advice-bound reading as both a coordination mechanism (it provides a stable legal doctrine) and as an extraction mechanism: the reading forecloses competing interpretations (reserve power, dead letter) and concentrates interpretive authority in the executive-centered constitutional consensus. The reading benefits the professional establishment (stable doctrine) while extracting from alternative theoretical frameworks.
constraint_indexing:constraint_classification(royal_assent_convention__advice_bound_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(royal_assent_convention__advice_bound_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(royal_assent_convention__advice_bound_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(royal_assent_convention__advice_bound_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(royal_assent_convention__advice_bound_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(royal_assent_convention__advice_bound_reading, TR),
    TR >= 0.70.

:- end_tests(royal_assent_convention__advice_bound_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The advice-bound reading extracts discretion from the monarch and relocates it to the executive, concentrating legislative power in the government of the day. This is significant but not maximal (0.58 rather than 0.70+) because the extraction is mediated by constitutional formality and parliamentary constraint—the government cannot act entirely without ministers' advice. Suppression (0.72): High. The reading actively suppresses the monarch's potential agency by doctrinal negation: the monarch is declared to be a conduit with no personal discretion. This suppression is structural (embedded in constitutional formality) and enforced through legal culture. The trajectory from 1707 to 1950 shows suppression increasing as the reading becomes more thoroughly formalized and the reserve-power shadow recedes from political consciousness. Theater ratio (0.65): Moderate-high. The advice-bound reading is partly performative: it asserts the monarch is without discretion, yet the three-century silence creates ambiguity about whether discretion is genuinely absent or merely dormant. Constitutional doctrine performs the reading through repetition—law books declare it settled—while the deeper constitutional reality (does the Crown have a true emergency power?) remains unresolved. As political practice moved from active royal participation (1707) to near-total disuse (1950), theater increased because the doctrine's force depends on the fiction that the rule explains what would happen, when in fact the rule is validated only by non-use.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits profound perspectival divergence. The monarch (powerless/trapped) experiences pure snare: no exit from the ministerial instruction framework. The government (institutional/arbitrage) experiences pure rope: stable coordination mechanism. Opposition parties (moderate/constrained) experience tangled rope: benefits from predictability but harmed by concentration of power. Constitutional doctrine (institutional/constrained) experiences piton: the reading is maintained through formal assertion and legal repetition despite uncertainty about functional necessity. Scholars (analytical/analytical) experience tangled rope: they benefit from a stable doctrine to teach but are extractively constrained by the reading's dominance, which forecloses alternative interpretations. The analytical observer risks a false-summit mountain view—seeing the advice-bound principle as a structural necessity of parliamentary democracy—when the reading is actually a formalized doctrinal choice with live alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are determined by structural position. The monarch (primary victim of the reading) has d ≈ 0.95 (full target of extraction). The government of the day (primary beneficiary) has d ≈ 0.05 (full beneficiary). Opposition parties (constrained, mixed position) have d ≈ 0.60 (moderate extraction despite some coordination benefits). Constitutional doctrine as an institution (enforcer of the reading) has d ≈ 0.20 (beneficiary of stable doctrine, with arbitrage options via amendment). The perspectival gap reflects these different d values: a beneficiary with arbitrage options experiences low chi (rope); a trapped agent experiences maximum chi (snare). The same extraction coefficient ε=0.58 produces wildly different experienced extractiveness χ depending on the observer's power level and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING STRUCTURE: The advice-bound reading resolves mandatrophy at the kernel level by asserting a single, deterministic constitutional relationship: the monarch has no discretion, the government determines assent, the monarch is a conduit. This forecloses the reserve-power reading's claim that latent power exists. But mandatrophy reappears at the meta-level: does the advice-bound reading reflect a structural necessity (making it a near-mountain principle of parliamentary democracy) or a formalized contingency (making it a tangled rope that could be reformed)? The omega variables address this irreducible uncertainty. The constraint does not resolve mandatrophy; it localizes it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discretion_location_ambiguity,
    'Is suppression of the monarch''s discretion a genuine structural feature of the advice-bound reading, or is suppression relocated to advisers such that discretion persists in advising capacity?',
    'Historical analysis of ministerial advice patterns: do ministers exercise genuine discretion in their advice, or do they mechanically follow party/cabinet consensus? If ministers genuinely deliberate, discretion is not eliminated but relocated.',
    'If discretion is relocated (not eliminated): the advice-bound reading is a Tangled Rope even from the monarch''s perspective—suppression is real but distributed. If discretion is truly eliminated system-wide: the reading is more snare-like, and the extraction is more severe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discretion_location_ambiguity, conceptual, 'Whether suppression eliminates discretion or relocates it to the advising machinery').

omega_variable(
    reserve_power_latency_status,
    'Does three centuries of disuse extinguish the reserve power (dead-letter reading), or does disuse preserve latent power through non-exercise (reserve-power reading), or does the advice-bound reading claim power is constitutionally nullified (not merely dormant)?',
    'Examination of constitutional amendment language, statutory codification attempts, and the logical status of non-use: does silence mean the power is gone, or does it mean the power exists but is not exercised under normal conditions?',
    'If reserve power is latent (not dead): the advice-bound reading is a contingent institutional arrangement rather than a structural necessity, and the mountain perspective becomes more clearly a false summit. If the power is truly dead: the advice-bound reading claims constitutional fact (via disuse), not just doctrinal interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reserve_power_latency_status, conceptual, 'Whether reserve power is constitutionally extinct or latently preserved through non-exercise').

omega_variable(
    doctrinal_foundation_empirical_claim,
    'Is the advice-bound reading grounded in a timeless principle of constitutional structure (e.g., parliamentary democracy requires executive unity), or is it an empirical claim about what has happened to work for three centuries in specific institutional contexts?',
    'Comparative constitutional law: do all parliamentary democracies require monarchs to be conduits, or have some maintained genuine royal discretion? Historical counterfactual: what would have happened if a monarch had refused assent in 1780, 1850, 1950?',
    'If structural principle: the mountain perspective is justified—the reading reflects a genuine institutional necessity. If empirical contingency: the reading is a contingent doctrine, and alternative readings (reserve power) remain logically live even if politically inactive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrinal_foundation_empirical_claim, empirical, 'Whether advice-bound principle is structural necessity or contingent historical practice').

omega_variable(
    reading_contest_status,
    'Does the existence of competing readings (dead-letter, reserve-power) mean the advice-bound reading is merely one interpretive option, or has the advice-bound reading become formalized doctrine such that alternatives are excluded from legitimate constitutional discourse?',
    'Survey of constitutional law textbooks, judicial language, and legislative practice: are alternative readings taught as live options or as superseded/rejected views? Is the advice-bound reading treated as settled or contested?',
    'If advice-bound is settled doctrine: the reading has interpretive authority and the other readings must overcome a high bar to be reinstated. If advice-bound is one live option among several: the reading''s legitimacy is weaker and the constraint''s extractiveness may be overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_status, conceptual, 'Whether advice-bound reading is settled doctrine or one contested option').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(royal_assent_convention__advice_bound_reading, 1707, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roya_tr_t1707, royal_assent_convention__advice_bound_reading, theater_ratio, 1707, 0.35).
narrative_ontology:measurement(roya_tr_t1850, royal_assent_convention__advice_bound_reading, theater_ratio, 1850, 0.52).
narrative_ontology:measurement(roya_tr_t1950, royal_assent_convention__advice_bound_reading, theater_ratio, 1950, 0.65).

% Extraction over time
narrative_ontology:measurement(roya_be_t1707, royal_assent_convention__advice_bound_reading, base_extractiveness, 1707, 0.42).
narrative_ontology:measurement(roya_be_t1850, royal_assent_convention__advice_bound_reading, base_extractiveness, 1850, 0.52).
narrative_ontology:measurement(roya_be_t1950, royal_assent_convention__advice_bound_reading, base_extractiveness, 1950, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(roya_su_t1707, royal_assent_convention__advice_bound_reading, suppression_requirement, 1707, 0.55).
narrative_ontology:measurement(roya_su_t1850, royal_assent_convention__advice_bound_reading, suppression_requirement, 1850, 0.68).
narrative_ontology:measurement(roya_su_t1950, royal_assent_convention__advice_bound_reading, suppression_requirement, 1950, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(royal_assent_convention__advice_bound_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(royal_assent_convention__advice_bound_reading, royal_assent_convention__dead_letter_reading).
narrative_ontology:affects_constraint(royal_assent_convention__advice_bound_reading, royal_assent_convention__reserve_power_reading).

% DUAL FORMULATION NOTE:
% The royal assent convention constraint family decomposes into three structurally distinct readings of the same kernel. Each reading has its own ε value, own beneficiary/victim structure, and own interpretive authority grounding. The advice-bound reading (this constraint, ε≈0.58) treats the convention as a formalized doctrine of executive governance. The dead-letter reading (ε≈0.12) treats the convention as a historical artifact with no force. The reserve-power reading (ε≈0.35) treats the convention as a surface rule masking deeper constitutional potential. These are not three perspectives on one constraint; they are three structurally distinct constraints grounded in different readings of the contested kernel 'royal assent.' Each instantiates a different relationship between the Crown and Parliament. They are linked via network.affects_constraints to indicate they are sibling readings of the same kernel and may compete for institutional authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
