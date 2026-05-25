% ============================================================================
% CONSTRAINT STORY: interpersonal_narrative_monopoly
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_interpersonal_narrative_monopoly, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: interpersonal_narrative_monopoly
 *   human_readable: Interpersonal Narrative Monopoly
 *   domain: interpersonal_dynamics/relationships
 *
 * SUMMARY:
 *   Interpersonal narrative monopoly is a structural constraint in intimate
 *   relationships where one party controls the authoritative account of
 *   shared experience, and the other party's competing narrative is
 *   systematically suppressed, invalidated, or rendered unspeakable. Unlike
 *   external authority (where a third party enforces the dominant narrative),
 *   narrative monopoly is sustained through internalized identity fusion: the
 *   silenced party's self-concept becomes constituted through compliance with
 *   the dominant narrative, making exit literally unthinkable from within
 *   their adopted frame. The constraint exhibits mixed coordination and
 *   extraction: it does solve the legitimate problem of co-constructing
 *   shared reality, but the solution is radically asymmetric, and the
 *   suppression mechanism operates through both material costs (relational,
 *   social, economic consequences of challenging the narrative) and
 *   internalized costs (identity fusion that makes the agent's own narrative
 *   feel illegitimate from within). The theater ratio is high because much of
 *   the enforcement is performative — the dominant narrative holder must
 *   continuously enact and reenact the authorized story, and the silenced
 *   party must continuously perform acceptance and internalized agreement,
 *   even as both parties experience the narrative gap.
 *
 * KEY AGENTS:
 *   - Narrative Holder: Primary beneficiary (institutional/arbitrage) — controls authoritative account; captures framing advantage and social validation during relationship duration
 *   - Silenced Party: Primary victim (powerless/identity_locked) — structurally mobile but identity-fused; cannot assert their own narrative without experiencing self-dissolution
 *   - Witness/Third Party: Secondary actor (moderate/constrained) — faces pressure to affirm dominant narrative or remain silent; experiences coordination benefit (clarity) alongside suppression cost
 *   - Therapeutic Intervention: Organized external agent (organized/mobile) — therapists, mediators, support networks building alternative pathways for narrative co-construction
 *   - Cultural Norms: Institutional sustainer (institutional/arbitrage) — gendered expectations, deference patterns, relationship role norms that make narrative monopoly seem inevitable
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing contingent power asymmetry as inherent feature of consciousness
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(interpersonal_narrative_monopoly, 0.58).
domain_priors:suppression_score(interpersonal_narrative_monopoly, 0.65).
domain_priors:theater_ratio(interpersonal_narrative_monopoly, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(interpersonal_narrative_monopoly, extractiveness, 0.58).
narrative_ontology:constraint_metric(interpersonal_narrative_monopoly, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(interpersonal_narrative_monopoly, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(interpersonal_narrative_monopoly, tangled_rope).
narrative_ontology:human_readable(interpersonal_narrative_monopoly, "Interpersonal Narrative Monopoly").
narrative_ontology:topic_domain(interpersonal_narrative_monopoly, "interpersonal_dynamics/relationships").

domain_priors:requires_active_enforcement(interpersonal_narrative_monopoly).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(interpersonal_narrative_monopoly, narrative_holder).
narrative_ontology:constraint_victim(interpersonal_narrative_monopoly, silenced_party).
narrative_ontology:constraint_victim(interpersonal_narrative_monopoly, relationship_epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SILENCED PARTY (SNARE) — Structurally mobile but identity-fused with the relationship. Their identity has been constituted through the relational frame imposed by the narrative holder. Exit would require abandoning not just the relationship but the self-concept (loyalty, devotion, identity as 'the understanding one') that the constraint has constructed. High suppression through internalized framing that makes their own narrative literally unthinkable from within.
constraint_indexing:constraint_classification(interpersonal_narrative_monopoly, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: WITNESS/INTIMATE THIRD PARTY (TANGLED ROPE) — Experiences the constraint as both coordination and extraction. The narrative monopoly does coordinate shared reality ('we agree on what happened'), but the coordination is asymmetric — the witness benefits from clarity and social alignment but also bears suppression cost through pressure to affirm the dominant narrative or remain silent. Constrained by social/relational costs of challenging the story.
constraint_indexing:constraint_classification(interpersonal_narrative_monopoly, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: NARRATIVE HOLDER (ROPE) — Experiences the constraint as pure coordination. They are solving the legitimate problem of creating a coherent shared reality. The monopoly is their tool for making the relationship intelligible and navigable. They perceive significant mutual benefit — the other party 'understands me better when we share my framing.' Net beneficiary, able to exit or renegotiate the narrative if they choose.
constraint_indexing:constraint_classification(interpersonal_narrative_monopoly, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: THERAPEUTIC INTERVENTION (SCAFFOLD) — Organized external agents (therapists, mediators, support networks) see narrative monopoly as a temporary coordination failure with a sunset. Evidence-based interventions (narrative therapy, externalization techniques, couples communication training) are building alternative pathways where both parties can hold valid narratives. The intervention classifies as scaffold because it explicitly carries a sunset clause: successful therapy means narrative co-construction replaces monopoly, or exit from the relationship becomes possible.
constraint_indexing:constraint_classification(interpersonal_narrative_monopoly, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: CULTURAL NORMS (PITON) — Broader institutional norms (gendered caregiving expectations, deference norms, cultural authority patterns) sustain the narrative monopoly through inertia and theater rather than direct enforcement. The constraint persists because 'this is how relationships work' narratively rather than because the mechanism is currently functional. Theater ratio reflects performative conformity to relationship role expectations.
constraint_indexing:constraint_classification(interpersonal_narrative_monopoly, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZED VIEW (MOUNTAIN) — From a civilizational/universal perspective, the dominance of one party's narrative is inherent to human relationships: cognitive biases (egocentric bias, backfire effects, belief perseverance) mean each party will always experience their own narrative as more valid. Power asymmetries in relationship narratives are immutable features of consciousness itself. However, this naturalizes what is actually a socially maintained pattern — the mountain classification is a false summit revealing how narrative monopoly uses inevitability claims to prevent alternatives from being seen.
constraint_indexing:constraint_classification(interpersonal_narrative_monopoly, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(interpersonal_narrative_monopoly_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(interpersonal_narrative_monopoly, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(interpersonal_narrative_monopoly, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(interpersonal_narrative_monopoly, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(interpersonal_narrative_monopoly, TR),
    TR >= 0.70.

:- end_tests(interpersonal_narrative_monopoly_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and increasing over the measurement interval. Initial extractiveness (0.35) reflects genuine coordination at relationship origin — both parties benefit from shared narrative coherence. Extractiveness increases over time as the dominant narrative holder accumulates social validation and the silenced party internalizes the frame, making challenge increasingly costly. By endpoint (0.58), the constraint has shifted from coordination toward extraction. Suppression (0.65): High and structural. Multiple barriers prevent the silenced party from asserting their narrative: relational cost (fear of rejection, abandonment); social cost (if the relationship is publicly defined by the dominant narrative, challenging it risks social standing); material cost (economic dependency, shared children, housing); and internalized cost (the agent's identity has fused with compliant role). Theater ratio (0.68): High. Much of the enforcement is performative — the relationship must continuously enact and reenact the authorized narrative. Therapy outcomes show that when this performative layer is interrupted, agents often recognize the narrative gap immediately, suggesting theater has been masking rather than legitimating.
 *
 * PERSPECTIVAL GAP:
 *   The acute perspectival gap between the narrative holder's Rope and the silenced party's Snare reveals the mechanism: the coordination function is real (both parties do benefit from shared narrative clarity), but the asymmetry is also real (the benefits accrue primarily to the dominant party, and the costs are concentrated on the silenced party). The narrative holder experiences the constraint as solving a genuine problem and cannot easily see the extraction because, from their perspective, the shared narrative is accurate and beneficial. The silenced party experiences suppression but cannot contest the narrative without risking the relationship and their identity. The witness sees both layers clearly but faces pressure to choose: affirm the dominant narrative and maintain peace, or validate the silenced party and risk destabilization. The therapeutic perspective reframes the constraint as a coordination failure amenable to renegotiation — not inevitable, not immutable, but a contingent institutional arrangement that can be unwound. The piton perspective shows that broader cultural norms (gendered deference, authority patterns) sustain the monopoly through performative role conformity. The false mountain reveals how 'human nature' arguments are deployed to prevent seeing alternatives: 'people always experience their own narrative as more valid' naturalizes what is actually a maintained asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim declarations and exit options. Narrative holder: beneficiary + arbitrage exit → d ≈ 0.15 → f(d) ≈ -0.01 (extraction runs away from them, toward them). Silenced party: victim + identity_locked exit → d ≈ 0.89 → f(d) ≈ 1.28 (extraction runs strongly toward them). Witness: mixed status + constrained exit → d ≈ 0.60 → f(d) ≈ 0.80. Analytical observer: observer status + analytical exit → d ≈ 0.72 → f(d) ≈ 1.15 (canonical analytical). The identity_locked exit option for the silenced party is diagnostic: they have structural mobility (could physically leave, have income, housing options available) but cannot exercise it because their identity is constituted through the relationship role. This produces a perspectival gap between identity_locked (rope at biographical time = perceives constraint as changeable in principle) and trapped (mountain at biographical time = perceives constraint as unchangeable). The silenced party in narrative monopoly often falls between: they have constrained exit capacity but identity_locked binding. This combination (victim + identity_locked) produces a perspectival gap rich with diagnostic insight — exit is theoretically possible but perceptually unthinkable.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the tangled rope classification captures what the false mountain misses: narrative monopoly is neither immutable nor pure coordination. It is a coordination mechanism (shared narrative clarity, relationship stability) that is maintained through asymmetric extraction and suppression. The narrative holder's perspective (Rope) legitimately sees coordination. The silenced party's perspective (Snare) legitimately sees extraction. The tangled rope perspective (moderate agent's view) captures the hybrid: genuine coordination function alongside genuine asymmetric extraction. The false mountain ('human nature makes narratives monopolistic') attempts to naturalize the constraint, preventing alternatives from being visible. The scaffold perspective (therapeutic intervention) reveals the falsity: narrative co-construction is empirically viable (evidence from narrative therapy, couples counseling, mediation outcomes), meaning the constraint is contingent rather than immutable. The mandatrophy resolves by recognizing that the same structural mechanism (narrative authority) serves both coordination and extraction functions, and that these functions can be decoupled: one can maintain relationship stability and shared reality without maintaining narrative monopoly, but the decoupling requires active intervention (therapy, mediation, renegotiation) because the path of least resistance is monopoly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_mechanism,
    'Is the silenced party''s inability to contest the narrative rooted in internalized identity fusion or in material/relational costs of exit?',
    'Post-exit trajectory: if suppression persists after the relationship ends (internalized voice-loss, inability to assert own narrative in new relationships), the lock is partially internalized. If suppression declines rapidly post-exit, the mechanism was primarily external/structural.',
    'If internalized: the constraint''s effective suppression is higher than measured, and therapeutic intervention must address identity reconstruction. If external: constraint is more amenable to rapid structural change (exit, mediation, narrative co-construction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Internalized vs external suppression mechanism').

omega_variable(
    narrative_validity_asymmetry,
    'Does the dominant narrative holder''s account actually track reality more accurately, or is narrative monopoly creating systematically distorted shared memory?',
    'Comparison of both parties'' accounts against objective evidence (contemporaneous records, witness corroboration, forensic timeline analysis). Measurement of memory divergence correlation with power asymmetry.',
    'If dominant narrative is more accurate: constraint is serving coordination function legitimately (Rope classification correct). If systematically distorted: monopoly is pure extraction disguised as clarification (Snare from silenced party perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(narrative_validity_asymmetry, empirical, 'Whether dominant narrative is more accurate or systematically distorted').

omega_variable(
    suppression_cyclical_reinforcement,
    'Does the narrative monopoly operate through continuous coercion or through intermittent reinforcement that creates psychological lock-in?',
    'Temporal measurement of suppression intensity and challenge-attempts: if suppression shows regular cycles of enforcement-relaxation-incident, mechanism is intermittent reinforcement (trauma bonding pattern). If continuous, mechanism is ongoing coercion.',
    'If intermittent: extractiveness may be lower than measurements suggest because the mechanism''s power derives from unpredictability and hope-extinction, not constant pressure. If continuous: extractiveness reflects genuine ongoing burden.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_cyclical_reinforcement, empirical, 'Whether suppression operates through continuous coercion or intermittent reinforcement').

omega_variable(
    alternative_narrative_viability,
    'Can the relationship sustain genuine narrative co-construction, or would accepting multiple valid narratives destabilize the core coordination function?',
    'Therapeutic trials of narrative externalization and co-construction: can both parties hold their own narratives as valid without the relationship collapsing? What happens to intimacy, sexual/emotional satisfaction, trust if narrative monopoly is relaxed?',
    'If co-construction is viable: constraint is contingent (Scaffold sunset is real). If relationship fails with narrative liberalization: the extraction may be structurally necessary for the relationship''s stability, shifting classification toward Snare (extraction serves a hidden coordination function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_narrative_viability, empirical, 'Whether narrative co-construction can sustain the relationship').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(interpersonal_narrative_monopoly, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(narr_tr_t0, interpersonal_narrative_monopoly, theater_ratio, 0, 0.52).
narrative_ontology:measurement(narr_tr_t5, interpersonal_narrative_monopoly, theater_ratio, 5, 0.6).
narrative_ontology:measurement(narr_tr_t10, interpersonal_narrative_monopoly, theater_ratio, 10, 0.68).
narrative_ontology:measurement(narr_tr_t3, interpersonal_narrative_monopoly, theater_ratio, 3, 0.56).

% Extraction over time
narrative_ontology:measurement(narr_be_t0, interpersonal_narrative_monopoly, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(narr_be_t5, interpersonal_narrative_monopoly, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(narr_be_t10, interpersonal_narrative_monopoly, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(narr_be_t3, interpersonal_narrative_monopoly, base_extractiveness, 3, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(interpersonal_narrative_monopoly, attachment_coordination).
narrative_ontology:boltzmann_floor_override(interpersonal_narrative_monopoly, 0.12).
narrative_ontology:affects_constraint(interpersonal_narrative_monopoly, gaslighting_mechanism).
narrative_ontology:affects_constraint(interpersonal_narrative_monopoly, emotional_manipulation_intermittent_reinforcement).
narrative_ontology:affects_constraint(interpersonal_narrative_monopoly, relational_identity_fusion).

% DUAL FORMULATION NOTE:
% Interpersonal narrative monopoly is the root constraint affecting downstream emotional dynamics. The gaslighting mechanism (ε≈0.72, Snare) operates specifically through narrative contestation and invalidation. Intermittent reinforcement (ε≈0.68, Snare) uses cyclical suppression-relaxation patterns. Relational identity fusion (ε≈0.45, Tangled Rope) is the cognitive mechanism enabling the monopoly. All three downstream constraints decompose from this core structural dynamic. The ε values differ because they isolate distinct observables: narrative authority vs emotional manipulation vs identity constitution. Each story gets its own perspectives and measurements, but all are linked to this upstream narrative monopoly constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(interpersonal_narrative_monopoly, powerless, 0.89).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
