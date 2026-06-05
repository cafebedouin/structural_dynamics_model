% ============================================================================
% CONSTRAINT STORY: royal_assent_convention__dead_letter_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_royal_assent_convention__dead_letter_reading, []).

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
 *   constraint_id: royal_assent_convention__dead_letter_reading
 *   human_readable: Royal Assent as Dead Letter (Desuetude Reading)
 *   domain: constitutional_law/parliamentary_sovereignty
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of a contested kernel: the
 *   constitutional status of royal assent (the monarch's formal approval
 *   required for legislation to become law). The dead-letter reading claims
 *   that refusal of assent is constitutionally extinct — three centuries of
 *   continuous disuse have extinguished the veto power, and any attempt to
 *   exercise it would either be ignored by parliament or trigger forced
 *   abdication. Under this reading, the constraint is a piton: the ceremony
 *   of royal assent persists as pure theater (0.88 theater ratio), but the
 *   underlying veto capacity is completely dead. Extractiveness is minimal
 *   (0.05) because there is no extant power to extract — the veto has been
 *   legally extinguished by desuetude, not merely suppressed. Suppression is
 *   complete (0.92) because the potential veto right is treated as absent
 *   rather than as a dormant reserve power. This reading directly contests
 *   two sibling readings: (1) the advice-bound reading, which holds that
 *   assent is exercised only through ministerial advice and the monarch is a
 *   conduit with no personal discretion at all; (2) the reserve-power
 *   reading, which holds that assent remains a true reserve power — dormant
 *   but available in constitutional extremity. The kernel is the formal
 *   constitutional status of the Crown's assent right; the three readings
 *   offer incompatible accounts of whether that right exists, in what form,
 *   and under what conditions it could be exercised.
 *
 * KEY AGENTS:
 *   - Parliament: Primary beneficiary (institutional/arbitrage) — secures legislative certainty by doctrine that veto is legally extinct
 *   - Reserve-Power Theorists: Primary victim (moderate/constrained) — their foundational premise (veto exists as emergency backstop) is buried and treated as legally dead
 *   - The Monarch: Performer (institutional/arbitrage) — conducts ceremonial assent ritual with zero functional discretion; theatrical role maintained through institutional inertia
 *   - Constitutional Reform Movement: Organized challengers (organized/mobile) — must work within the dead-letter fiction to formally abolish or clarify the veto; locked in by doctrine that treats it as already extinct
 *   - The Extinguished Veto Right: Normative ghost (powerless/trapped) — absent from all practical contexts yet phantom in doctrine; extraction mechanism is doctrinal certainty (parliament knows veto cannot happen) maintained through absence of legal clarity
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a contingent political development (disuse over three centuries) as an immutable constitutional fact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(royal_assent_convention__dead_letter_reading, 0.05).
domain_priors:suppression_score(royal_assent_convention__dead_letter_reading, 0.92).
domain_priors:theater_ratio(royal_assent_convention__dead_letter_reading, 0.88).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(royal_assent_convention__dead_letter_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(royal_assent_convention__dead_letter_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(royal_assent_convention__dead_letter_reading, theater_ratio, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(royal_assent_convention__dead_letter_reading, piton).
narrative_ontology:human_readable(royal_assent_convention__dead_letter_reading, "Royal Assent as Dead Letter (Desuetude Reading)").
narrative_ontology:topic_domain(royal_assent_convention__dead_letter_reading, "constitutional_law/parliamentary_sovereignty").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(royal_assent_convention__dead_letter_reading, '26e33b1d-73ca-448e-acbc-0a3425355180').
narrative_ontology:cs_kernel_codification('26e33b1d-73ca-448e-acbc-0a3425355180', formalized).
narrative_ontology:cs_authority_grounding('26e33b1d-73ca-448e-acbc-0a3425355180', lineage).
narrative_ontology:cs_interpretation_layer_present('26e33b1d-73ca-448e-acbc-0a3425355180').
narrative_ontology:cs_reading_relation('26e33b1d-73ca-448e-acbc-0a3425355180', royal_assent_convention__advice_bound_reading, influences).
narrative_ontology:cs_reading_relation('26e33b1d-73ca-448e-acbc-0a3425355180', royal_assent_convention__reserve_power_reading, forecloses).
narrative_ontology:cs_axiom('26e33b1d-73ca-448e-acbc-0a3425355180', foundational, desuetude_legally_extinguishes_power).
narrative_ontology:cs_axiom_status(desuetude_legally_extinguishes_power, holdable).
narrative_ontology:cs_axiom_grounding('26e33b1d-73ca-448e-acbc-0a3425355180', desuetude_legally_extinguishes_power, conventional).
narrative_ontology:cs_axiom('26e33b1d-73ca-448e-acbc-0a3425355180', secondary, three_centuries_disuse_is_irreversible).
narrative_ontology:cs_axiom_status(three_centuries_disuse_is_irreversible, holdable).
narrative_ontology:cs_axiom_grounding('26e33b1d-73ca-448e-acbc-0a3425355180', three_centuries_disuse_is_irreversible, empirically_contingent).
narrative_ontology:cs_reference_frame('26e33b1d-73ca-448e-acbc-0a3425355180', parliamentary_sovereignty_absolute).
narrative_ontology:cs_drift_state('26e33b1d-73ca-448e-acbc-0a3425355180', contemporary_constitutional_practice, gap(stable, minor, true)).
narrative_ontology:cs_created_at('26e33b1d-73ca-448e-acbc-0a3425355180', '').
narrative_ontology:cs_kernel_id(royal_assent_convention__dead_letter_reading, royal_assent_convention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(royal_assent_convention__dead_letter_reading, parliament).
narrative_ontology:constraint_beneficiary(royal_assent_convention__dead_letter_reading, constitutional_certainty).
narrative_ontology:constraint_victim(royal_assent_convention__dead_letter_reading, reserve_power_theorists).
narrative_ontology:constraint_victim(royal_assent_convention__dead_letter_reading, monarchical_discretion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PARLIAMENT (ROPE) — Experiences royal assent as pure coordination: a ceremonial conduit for validating legislation. The constraint enables legislation to become law with constitutional certainty. Parliamentary sovereignty is secured through the dead-letter framing — no threat of refusal, zero extraction. Net beneficiary.
constraint_indexing:constraint_classification(royal_assent_convention__dead_letter_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 2: RESERVE POWER THEORIST (MOUNTAIN) — Sees the dead-letter claim as a false naturalization of what is actually a contingent political fact: disuse has extinguished the power, but this is treated as an immutable constitutional law rather than a reversible political development. The theorist is trapped in a frame where their foundational premise (veto exists as a reserve) meets an irreversible suppression claim.
constraint_indexing:constraint_classification(royal_assent_convention__dead_letter_reading, mountain,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE MONARCH (PITON) — Experiences royal assent as pure theater: the ceremony of giving assent to bills is preserved as a constitutional ritual, but the monarch has zero functional discretion and zero veto capacity. The ritual persists through institutional inertia — the symbolic role is maintained because alternatives (direct parliamentary enactment without royal ceremony) haven't been adopted, not because the assent mechanism functions. Theater ratio is high (0.88) because the entire performance is symbolic.
constraint_indexing:constraint_classification(royal_assent_convention__dead_letter_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE EXTINGUISHED VETO RIGHT (SNARE) — A constitutive fiction: the veto right is absent from all practical contexts (zero veto capacity), yet it persists as a phantom in constitutional theory because doctrine refuses to explicitly abolish it. The right is completely suppressed (any attempted exercise would be ignored or end the institution) yet treated as extant rather than dead. This creates the extractive condition: legal doctrine extracts certainty (parliament knows it cannot be checked) from the absence of legal clarity (the veto is not formally abolished, merely buried).
constraint_indexing:constraint_classification(royal_assent_convention__dead_letter_reading, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL REFORM MOVEMENT (TANGLED ROPE) — Organized agents (republicans, constitutional modernizers) see the dead-letter framing as both coordination and extraction. It coordinates parliamentary expectations (parliament knows the veto is gone), but it extracts doctrinal lock-in (the reform movement cannot simply abolish the veto formally because doctrine insists it is already dead via desuetude). The movement must work within the fiction to exit it — a mixed constraint with some active enforcement (defending the desuetude claim) and some coordination (shared understanding that the veto is gone).
constraint_indexing:constraint_classification(royal_assent_convention__dead_letter_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (PITON) — The dead-letter reading from a civilizational/universal standpoint is a piton: it is maintained through doctrinal theater (citing 300 years of disuse as proof of extinction) rather than through active legal mechanism. The claim is performative — treating desuetude as legal repeal without formal amendment. Theater ratio 0.88 reflects that the constraint's entire enforcement mechanism is doctrinal assertion plus institutional inertia.
constraint_indexing:constraint_classification(royal_assent_convention__dead_letter_reading, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(royal_assent_convention__dead_letter_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(royal_assent_convention__dead_letter_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(royal_assent_convention__dead_letter_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(royal_assent_convention__dead_letter_reading, TR),
    TR >= 0.70.

:- end_tests(royal_assent_convention__dead_letter_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.05): Minimal. Under the dead-letter reading, the veto power is treated as legally extinct rather than suppressed. There is no extant power that can be exercised or withheld — the constraint is the absence of the veto, not its exercise. The small residual extractiveness reflects the minor benefit parliament gains from doctrinal certainty (knowing the veto is legally gone, rather than merely unlikely). Suppression (0.92): Near-total. The veto is completely suppressed — any attempt to exercise it would be ignored or trigger forced abdication. But this is not suppression through active enforcement; it is suppression through legal death. The constraint is maintained by treating the veto as extinct, not as forbidden. Theater ratio (0.88): Very high. The ceremony of royal assent is preserved as a constitutional ritual, but it is almost entirely performative. The monarch goes through the motions of giving assent because the role is ceremonially preserved, not because the assent mechanism has functional content. The ritual persists through institutional inertia — alternative mechanisms (direct parliamentary enactment without royal ceremony) have not been formally adopted, so the theater continues. Over the 300-year interval, theater ratio rises (0.72 → 0.88) as the assent ceremony becomes increasingly divorced from any functional veto capacity and increasingly ritualized. Extractiveness declines (0.15 → 0.05) as disuse becomes complete and doctrine solidifies the veto as extinct. Suppression rises (0.45 → 0.92) as the requirement to treat the veto as dead (to prevent any claim of reserve power) intensifies.
 *
 * PERSPECTIVAL GAP:
 *   The dead-letter reading produces a perspectival gap between agents who benefit from the certainty it provides (Parliament, the monarchy as ceremonial institution) and agents who contest its foundational premise (reserve-power theorists, constitutional modernizers). Parliament sees rope: royal assent is coordination (the ceremony validates legislation). The monarch sees piton: the role is theatrical preservation. Reserve-power theorists see mountain: they are trapped by an irreversible legal development (desuetude treated as extinction) that contradicts their foundational premise that a veto exists. Constitutional reformers see tangled rope: the dead-letter framing both coordinates parliamentary expectations and extracts doctrinal lock-in (they cannot simply assert the veto exists; they must work within the fiction that it is already dead). The analytical observer sees piton: the entire constraint is maintained through doctrinal theater, treating a contingent political development (three centuries of disuse) as an immutable constitutional fact.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality (d) is derived from their structural relationship to the veto power and their exit options. Parliament benefits from the veto being dead (d ≈ 0.15, low extraction toward parliament). Reserve-power theorists are victimized by the dead-letter framing (d ≈ 0.85, high extraction from theorists) because their core premise is legally buried. The monarch has arbitrage options (ceremonial role is secure; can conduct assent theater indefinitely) and benefits from institutional preservation (d ≈ 0.20, low extraction). Constitutional reformers are constrained (cannot simply assert the veto exists; must work within the dead-letter fiction) and victimized by doctrinal lock-in (d ≈ 0.70, moderate-high extraction). The normative ghost (the extinguished veto right) is completely suppressed and has no exit (d ≈ 1.0, maximum theoretical extraction, but extraction of nothing from nothing).
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING EXEMPLAR: The dead-letter reading avoids mandatrophy by accepting complete suppression and zero veto capacity as the structural baseline. It does not claim both that parliament is coordinated through the veto and that the veto is extinct — it claims the veto is extinct and parliament gains certainty from this extinction. The extraction is minimal because there is nothing to extract (no veto power remains). The theater is high because the assent ceremony persists despite the underlying power being dead. No tension between coordination function and extraction — the constraint is pure absence maintained through ritual. The mandatrophy would arise if the dead-letter reading tried to claim both that the veto is a genuine reserve power (coordination benefit: emergency backstop) and that it is legally extinct (extraction benefit: parliamentary certainty). The three sibling readings distribute the contradiction: the reserve-power reading takes the genuine reserve premise; the dead-letter reading takes the legal extinction premise; the advice-bound reading takes a middle position (assent exists but is not the monarch's own discretion). The architecture of three readings resolves the contradiction by making each reading coherent within its own framework.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    desuetude_as_legal_death,
    'Can disuse alone extinguish a constitutional power without formal amendment or explicit repeal?',
    'Comparative constitutional law analysis across Westminster systems; examination of whether other powers have been extinguished by desuetude; determination of the formal legal status (dead vs. dormant vs. suspended) required by the constitution to make desuetude legally binding',
    'If desuetude alone suffices: the dead-letter reading is structurally sound and extractiveness remains near zero. If desuetude requires formal endorsement or if dormancy is reversible: the reading is aspirational rather than actual, and extractiveness rises (the constraint is maintained by suppression of formal amendment, not natural constitutional law).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(desuetude_as_legal_death, conceptual, 'Whether desuetude alone extinguishes constitutional powers').

omega_variable(
    boundary_between_readings,
    'What structural facts would distinguish the dead-letter reading (power is extinct) from the advice-bound reading (power is exercised only through government conduit) from the reserve-power reading (power exists dormant)?',
    'Identify the explicit factual or legal claim each reading rests on: (1) dead-letter: disuse has legally extinguished the veto; (2) advice-bound: the monarch has no independent discretion at all; (3) reserve: the veto survives as an emergency backstop. These are logically distinct predictions about what would happen in a constitutional crisis (e.g., if parliament passed a bill abolishing elections).',
    'Clarifying the boundary reveals which reading is correct or which are coexistent. If all three could be true in different factual scenarios (different magnitudes of constitutional crisis), they coexist. If one directly contradicts the others (veto is gone vs. veto exists), foreclosure applies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(boundary_between_readings, conceptual, 'Structural differentiation between the three readings of royal assent').

omega_variable(
    crisis_counterfactual,
    'In a genuine constitutional extremity — e.g., parliament passing a bill abolishing elections or the Crown''s prerogatives — what would actually happen if the monarch attempted to refuse assent?',
    'Scenario analysis: game-theoretic modeling of agent behavior in constitutional crisis; historical analysis of prior crises to infer behavioral predictions; expert constitutional opinion from monarchy theorists, parliamentary scholars, and comparative law specialists',
    'If refusal would be ignored or trigger forced abdication, the dead-letter reading is correct (extractiveness 0.05, piton). If refusal would be accepted as binding, the reserve-power reading is correct and extractiveness is higher (the veto is latent, not dead). If parliament retains control of the outcome, the advice-bound reading is correct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(crisis_counterfactual, empirical, 'What would occur if the monarch refused assent in constitutional crisis').

omega_variable(
    three_centuries_premise,
    'Is 300 years of disuse sufficient to legally extinguish a power that was never formally abolished, or does disuse require supplementary legal or political recognition to count as extinction?',
    'Doctrinal history of the royal assent: examination of when disuse became complete (which era?); whether any modern constitutional scholar or court has explicitly endorsed desuetude as the binding legal mechanism; whether any formal document (bills of rights, constitutional amendments, parliamentary resolution) acknowledges the extinction',
    'If desuetude alone is sufficient: extractiveness remains 0.05 (the constraint is natural constitutional law arising from long disuse). If supplementary recognition is required: extractiveness rises (the dead-letter reading is maintained by active suppression of formal amendment).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(three_centuries_premise, empirical, 'Whether 300 years of disuse legally extinguishes without supplementary recognition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(royal_assent_convention__dead_letter_reading, 1726, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roya_tr_t0, royal_assent_convention__dead_letter_reading, theater_ratio, 0, 0.72).
narrative_ontology:measurement(roya_tr_t150, royal_assent_convention__dead_letter_reading, theater_ratio, 150, 0.8).
narrative_ontology:measurement(roya_tr_t300, royal_assent_convention__dead_letter_reading, theater_ratio, 300, 0.88).

% Extraction over time
narrative_ontology:measurement(roya_be_t0, royal_assent_convention__dead_letter_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(roya_be_t150, royal_assent_convention__dead_letter_reading, base_extractiveness, 150, 0.08).
narrative_ontology:measurement(roya_be_t300, royal_assent_convention__dead_letter_reading, base_extractiveness, 300, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(roya_su_t0, royal_assent_convention__dead_letter_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(roya_su_t150, royal_assent_convention__dead_letter_reading, suppression_requirement, 150, 0.7).
narrative_ontology:measurement(roya_su_t300, royal_assent_convention__dead_letter_reading, suppression_requirement, 300, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(royal_assent_convention__dead_letter_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(royal_assent_convention__dead_letter_reading, royal_assent_convention__advice_bound_reading).
narrative_ontology:affects_constraint(royal_assent_convention__dead_letter_reading, royal_assent_convention__reserve_power_reading).
narrative_ontology:affects_constraint(royal_assent_convention__dead_letter_reading, parliamentary_sovereignty_doctrine).
narrative_ontology:affects_constraint(royal_assent_convention__dead_letter_reading, ceremonial_monarchy_performance).

% DUAL FORMULATION NOTE:
% The royal-assent-convention kernel has three structurally distinct constraint stories corresponding to three readings. The dead-letter reading (this file) is the claim that the veto is legally extinct through desuetude. The sibling readings (advice-bound, reserve-power) make different structural claims about what the veto is and under what conditions it could be exercised. These are not perspectives on a single constraint — they are separate constraints tied to the same contested kernel. Each reading has its own extractiveness value reflecting what the veto would do under that reading's premises: dead-letter (ε=0.05, no veto to extract), advice-bound (ε varies, depending on whether ministerial advice can extract from the monarch), reserve-power (ε=0.10-0.20, depending on the likelihood of emergency invocation). Network linking enables the system to model the constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
