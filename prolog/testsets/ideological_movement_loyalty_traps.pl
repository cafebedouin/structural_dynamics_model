% ============================================================================
% CONSTRAINT STORY: ideological_movement_loyalty_traps
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ideological_movement_loyalty_traps, []).

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
 *   constraint_id: ideological_movement_loyalty_traps
 *   human_readable: Ideological Movement Loyalty Traps
 *   domain: social/ideological/organizational
 *
 * SUMMARY:
 *   Ideological movements create powerful loyalty traps by fusing members'
 *   identities with movement ideology, creating suppression mechanisms that
 *   operate at multiple levels simultaneously: material (career, community,
 *   social status), psychological (internalized shame, epistemic closure,
 *   cognitive dissonance management), and structural (institutional
 *   gatekeeping, information control, boundary enforcement). The constraint
 *   exhibits a systematic extractiveness increase over its lifecycle: during
 *   ascending phase (t=0-10), the movement genuinely coordinates collective
 *   action and member satisfaction is high despite emerging suppression;
 *   during established phase (t=10-20), extractiveness accelerates as
 *   leadership consolidates power and dissent becomes taboo; during decline
 *   phase (t=20-30), extractiveness plateaus as the movement faces external
 *   pressures and members begin exit, but leadership intensifies enforcement
 *   to maintain dwindling membership. The theater_ratio increases across the
 *   interval, indicating that performative loyalty signaling (public
 *   declarations, ritual participation, ideological purity testing) gradually
 *   replaces genuine coordination as the movement's core function. The
 *   constraint satisfies the Snare classification from the victim
 *   perspectives (trapped, identity_locked, constrained adherents all
 *   experience χ ≥ 0.66) while leadership's Rope perspective represents
 *   genuine coordination benefits from their vantage point. Post-movement
 *   institutions (universities, think tanks, political parties) inherit and
 *   perpetuate loyalty enforcement mechanisms long after their functional
 *   necessity erodes, creating the Piton perspective: degraded extraction
 *   through institutional inertia and theater.
 *
 * KEY AGENTS:
 *   - Rank-and-File Adherents (Faithful): Primary victims (powerless/identity_locked) — identity fused with movement; experience maximum suppression because exit means self-annihilation
 *   - Moderate Dissident Members: Secondary victims (moderate/constrained) — developing doubts but facing high social/economic costs to exit; constrained by community embeddedness and career dependencies
 *   - Heretical Intellectuals: Tertiary victims (powerful/trapped) — intellectually skilled members whose critiques are suppressed through institutional mechanisms; doubly trapped by material dependencies and epistemic closure
 *   - Movement Leadership: Primary beneficiary (institutional/arbitrage) — experiences loyalty mechanism as coordination function enabling collective action; benefits from member conformity without bearing identity costs
 *   - Post-Movement Institutions: Institutional actors (organized/constrained) — inherit loyalty enforcement structures from movement's declining phase; maintain gatekeeping and ideological conformity through administrative means
 *   - Analytical Observers (Identity-Locked): Meta-victims (analytical/identity_locked) — scholars socialized in movement traditions whose analytical tools are partly shaped by movement epistemology; face oracle gap in detecting structural constraints
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ideological_movement_loyalty_traps, 0.62).
domain_priors:suppression_score(ideological_movement_loyalty_traps, 0.68).
domain_priors:theater_ratio(ideological_movement_loyalty_traps, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ideological_movement_loyalty_traps, extractiveness, 0.62).
narrative_ontology:constraint_metric(ideological_movement_loyalty_traps, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ideological_movement_loyalty_traps, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ideological_movement_loyalty_traps, snare).
narrative_ontology:human_readable(ideological_movement_loyalty_traps, "Ideological Movement Loyalty Traps").
narrative_ontology:topic_domain(ideological_movement_loyalty_traps, "social/ideological/organizational").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ideological_movement_loyalty_traps, movement_leadership).
narrative_ontology:constraint_victim(ideological_movement_loyalty_traps, rank_and_file_adherents).
narrative_ontology:constraint_victim(ideological_movement_loyalty_traps, dissenting_members).
narrative_ontology:constraint_victim(ideological_movement_loyalty_traps, intellectual_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FAITHFUL ADHERENT (SNARE) — The rank-and-file member experiences the movement as structurally inescapable because their identity is constituted through ideological commitment. Structural mobility exists (they have economic resources, legal rights, geographic freedom) but identity-lock prevents exercise of exit capacity. Questioning the movement means abandoning the self-narrative that gives life coherence. Maximum experienced extraction because the binding is internal — the agent carries the suppression with them even if they physically depart.
constraint_indexing:constraint_classification(ideological_movement_loyalty_traps, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: MODERATE DISSIDENT (SNARE) — Members with doubts face high costs to exit: social isolation, loss of community, career damage within movement-aligned institutions, potential ostracism by family members still in the movement. Exit is materially possible but so expensive that suppression remains near-maximum. This agent perceives the constraint as extraction masquerading as communion.
constraint_indexing:constraint_classification(ideological_movement_loyalty_traps, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HERETICAL INTELLECTUAL (SNARE) — Intellectually talented members who develop critiques of core ideology face the most severe trap. They are valuable to the movement (ideological sophistication, publication capacity, institutional credibility) so leadership has maximum incentive to suppress dissent. They are trapped by both material dependencies (career within movement institutions, social network) and epistemic closure (all frameworks for understanding criticism are provided by the movement itself). The constraint appears as pure extraction from this vantage — their intellectual labor is harvested and their critique is suppressed.
constraint_indexing:constraint_classification(ideological_movement_loyalty_traps, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 4: MOVEMENT LEADERSHIP (ROPE) — Leadership experiences the constraint as pure coordination: maintaining member loyalty enables the movement's collective action, fund-raising, institutional growth, and cultural influence. From this perspective, loyalty mechanisms are not extraction but the necessary social technology for sustaining group identity and mission. Leadership benefits from arbitrage options — they can exit the movement, start new ventures, or shift ideological positioning without bearing the identity costs that bind adherents.
constraint_indexing:constraint_classification(ideological_movement_loyalty_traps, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: POST-MOVEMENT ORGANIZATION (PITON) — Institutional structures that survive the movement's decline (universities, think tanks, publishing platforms, political parties founded during the movement's ascendancy) maintain the movement's loyalty mechanisms long after their functional justification erodes. These institutions enforce ideological conformity through administrative structures, peer review gatekeeping, and credentialing systems, but the mechanisms have become largely performative — the original coordinating function is gone, replaced by institutional inertia and theater. Theater ratio (0.55) is moderate because some genuine coordination persists (shared research agenda, institutional stability) alongside degraded extraction (ideological gatekeeping with minimal function).
constraint_indexing:constraint_classification(ideological_movement_loyalty_traps, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER - IDENTITY_LOCKED (SNARE) — The analyst who has themselves undergone socialization within the movement and whose analytical tools were partly shaped by movement epistemology faces an oracle gap: their capacity to see the constraint is limited by the frame that the constraint itself installs. They possess analytical power (can apply frameworks, conduct scholarship) but are identity-locked through professional identity fusion with movement intellectual traditions. They see the trap from the inside but cannot fully step outside it to measure from elsewhere. This perspective instantiates Theorem 4 (Classical Oracle Gap) — the native instruments cannot detect the structure that cross-position analysis reveals.
constraint_indexing:constraint_classification(ideological_movement_loyalty_traps, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(identity_locked),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ideological_movement_loyalty_traps_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ideological_movement_loyalty_traps, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ideological_movement_loyalty_traps, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ideological_movement_loyalty_traps, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ideological_movement_loyalty_traps, TR),
    TR >= 0.70.

:- end_tests(ideological_movement_loyalty_traps_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High and increasing. The movement extracts conformity, intellectual labor (coded as ideological development), social surplus (community building with movement benefits), and temporal investment. The extractiveness increases during established phase (t=10-20) as leadership consolidates power and suppression intensifies. The moderate initial extractiveness (0.35) reflects that during ascending phase, the movement provides genuine coordination benefits (shared purpose, community, institutional advancement) that partially compensate for extraction. By established phase (0.50), benefits degrade as hierarchy crystallizes. By decline phase (0.62), extraction outweighs coordination but leadership intensifies mechanisms rather than relaxing them. Suppression (0.68): High and structural. Multiple overlapping suppression mechanisms: material (loss of career, community, status upon exit); psychological (identity fusion makes exit unthinkable); informational (movement epistemology limits available critiques); social (public dissent triggers ostracism). The suppression operates at multiple temporal scales: immediate (public criticism triggers immediate social penalty), biographical (long-term career/community losses), generational (second-generation members socialized entirely within movement epistemology). Theater ratio (0.55): Moderate and increasing. During ascending phase, loyalty mechanisms genuinely serve coordination (rallies build momentum, ideological education creates shared understanding). By established phase, loyalty mechanisms become increasingly performative: public rituals of ideological conformity, purity testing, symbolic gestures replace substantive collective action. The theater increase from 0.25 to 0.55 reflects Goodhart drift — loyalty metrics (participation rate, public declarations, ideological purity) become goals in themselves, replacing the original coordinating function.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival gap because it appears as pure coordination (Rope) from leadership and as pure extraction (Snare) from adherents. Leadership genuinely benefits from loyalty mechanisms and experiences them as necessary coordination technology. Adherents genuinely suffer from the same mechanisms and experience them as extraction. This is not a disagreement about facts but a structural disagreement about the constraint's function — both parties are correct about their experience. The Piton perspective (post-movement institutions) reveals how loyalty mechanisms persist through institutional inertia long after their coordinating function erodes. The identity_locked Analytical Observer perspective reveals the oracle gap: analysts who were socialized in movement epistemology may be unable to detect the extractive structure of the very mechanisms they are studying, because the framework for critique has itself been shaped by those mechanisms. The emergence of the heretical intellectual perspective (powerful/trapped) is diagnostic: movement elites who develop capacity for criticism discover that their structural position makes suppression most severe, not least severe. This inverts the typical power hierarchy and reveals that the trap is structural, not merely institutional.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural position relative to the extraction flow. Powerless adherents with identity_locked exit face maximum d (0.92-0.98): their identity constitutes the constraint, their exit means self-dissolution, their options are none. Moderate members with constrained exit face high d (0.80-0.88): material barriers are real and external, but cognitive/psychological barriers are internalized. Powerful intellectuals with trapped exit face high d (0.85-0.95) despite their power: material dependencies and epistemic closure cancel power advantage. Leadership with institutional power and arbitrage options faces low d (0.08-0.20): they can exit, they benefit from the constraint, they experience it as coordination. The directionality derivation chain follows: victim status + identity_locked or trapped or constrained exit → high d → high f(d) → high experienced extractiveness (χ). Beneficiary status + arbitrage exit → low d → low/negative f(d) → low/negative experienced extractiveness (χ). Post-movement institutional actors with constrained exit but ambiguous victim/beneficiary status derive d ≈ 0.45-0.55 reflecting their institutional position outside the original extraction flow but still enforcing its mechanisms. The analytical observer with identity_locked exit derives d ≈ 0.72: analytical power does not escape the bind imposed by identity-lock, so experienced extractiveness remains high despite cognitive capacity to map the structure.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The mandatrophy is resolved by recognizing that ideological movement loyalty traps are genuine Snare constraints (not misclassified coordination mechanisms) whose extraction persists through identity-lock and institutionalization long after their coordinating function erodes or becomes secondary. Leadership's Rope classification is not a competing claim about the constraint's 'true' nature but a legitimate perspectival reading from the beneficiary position. The constraint is simultaneously Snare (from victim perspectives), Rope (from leadership), and Piton (from post-movement institutions). The analytical observer's risk is not false classification but oracle gap: socialization within movement epistemology prevents the analyst from fully stepping outside the frame needed to measure the constraint from multiple positions. The measurement data supports Snare classification: extractiveness increases faster than theater_ratio during middle phase, suggesting that extraction outpaces performative activity (ruling out pure theater/Piton classification). Suppression remains high and structural across all phases, supporting Snare over Rope. The heretical intellectual perspective clinches the classification: the agent with maximum power to resist (intellectual capacity, institutional credibility) discovers maximum suppression (epistemic closure plus material dependency), indicating that extraction is structural rather than contingent on agent power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_material_trap,
    'Is the binding mechanism primarily identity-based (agent believes they cannot be themselves outside the movement) or primarily material (agent faces genuine economic/social/legal barriers to exit)?',
    'Post-exit longitudinal tracking: Do members who physically leave report persistent suppression (suggesting internalized identity lock) or relief/freedom (suggesting primary barrier was external)? Comparison of exit barriers for different agent types.',
    'If primarily identity-lock: constraint persists even after external barriers are removed; reclassify as identity_locked throughout. If primarily material: removing barriers (alternative communities, economic opportunities, information access) would reduce suppression; reclassify toward constrained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_material_trap, empirical, 'Relative weight of identity-based vs material binding mechanisms').

omega_variable(
    extraction_vs_coordination_function,
    'Does the loyalty mechanism genuinely coordinate collective action (authentic coordination function) or primarily extract conformity and suppress dissent (extraction masquerading as coordination)?',
    'Comparative analysis: Do movements with reduced loyalty enforcement (more permissive dissent, lower suppression) lose coordination effectiveness or maintain it? Do post-movement organizations that retain loyalty enforcement (Piton perspective) show functional deterioration?',
    'If primarily coordination: reclassify from Snare toward Tangled Rope (mixed function). If primarily extraction: Snare classification confirmed; leadership''s Rope perspective reflects ideological capture, not objective coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_function, conceptual, 'Whether loyalty enforcement serves genuine coordination or extraction').

omega_variable(
    movement_lifecycle_stage_sensitivity,
    'Does the extractiveness of loyalty mechanisms vary systematically with movement lifecycle (ascending/established/declining), and is the variation driven by functional necessity or by leadership entrenchment?',
    'Comparative case analysis across movements at different lifecycle stages. Measurement of suppression, theater_ratio, and member satisfaction during each phase. Analysis of whether suppression increases during decline (suggesting extraction persistence) or decreases (suggesting functional erosion).',
    'If suppression persists during decline: extractive mechanism confirmed (Piton with degraded function). If suppression decreases: loyalty mechanism may be genuinely functional during ascendancy but becomes vestigial (supporting Piton classification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(movement_lifecycle_stage_sensitivity, empirical, 'Correlation between movement lifecycle stage and loyalty mechanism extractiveness').

omega_variable(
    intellectual_capture_depth,
    'To what extent are movement members'' critical faculties genuinely captured (unable to conceive critiques within their existing frameworks) versus strategically suppressed (able to conceive critiques but unwilling to voice them)?',
    'Cognitive anthropology: interviews with exited members about when/how they developed critiques. Did critique emerge suddenly upon exit (genuine capture) or gradually accumulate as suppressed doubt? Analysis of intellectual work produced by members — does complexity and nuance increase upon exit?',
    'If genuinely captured: suppression and trap depth are higher than material barriers suggest; reclassify toward maximum Snare. If strategically suppressed: members retain intellectual capacity but fear exit costs; reclassify toward Snare but with lower internal binding strength.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intellectual_capture_depth, empirical, 'Depth of cognitive capture vs strategic suppression in movement members').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ideological_movement_loyalty_traps, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ideol_tr_t0, ideological_movement_loyalty_traps, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ideol_tr_t10, ideological_movement_loyalty_traps, theater_ratio, 10, 0.4).
narrative_ontology:measurement(ideol_tr_t20, ideological_movement_loyalty_traps, theater_ratio, 20, 0.55).
narrative_ontology:measurement(ideol_tr_t30, ideological_movement_loyalty_traps, theater_ratio, 30, 0.62).

% Extraction over time
narrative_ontology:measurement(ideol_be_t0, ideological_movement_loyalty_traps, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ideol_be_t10, ideological_movement_loyalty_traps, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(ideol_be_t20, ideological_movement_loyalty_traps, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(ideol_be_t30, ideological_movement_loyalty_traps, base_extractiveness, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ideological_movement_loyalty_traps, identity_coordination).
narrative_ontology:boltzmann_floor_override(ideological_movement_loyalty_traps, 0.12).
narrative_ontology:affects_constraint(ideological_movement_loyalty_traps, epistemic_closure_mechanisms).
narrative_ontology:affects_constraint(ideological_movement_loyalty_traps, organizational_cult_dynamics).
narrative_ontology:affects_constraint(ideological_movement_loyalty_traps, ideological_capture_in_institutions).

% DUAL FORMULATION NOTE:
% Ideological movement loyalty traps decompose into three structurally distinct constraints: (1) identity_coordination at movement level (low ε during ascendancy, high ε during decline); (2) epistemic_closure at individual cognitive level (ε ≈ 0.55, moderate but persistent); (3) institutional gatekeeping in post-movement organizations (ε ≈ 0.45, Piton). Each constraint has its own measurement trajectory and benefits from separate story. This story covers the primary loyalty trap constraint at movement scale; linked stories address cognitive and institutional mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ideological_movement_loyalty_traps, powerful, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
