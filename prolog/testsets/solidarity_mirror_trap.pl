% ============================================================================
% CONSTRAINT STORY: solidarity_mirror_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_solidarity_mirror_trap, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: solidarity_mirror_trap
 *   human_readable: Solidarity Mirror Trap in Epistemic Communities
 *   domain: epistemology/social_psychology/discourse
 *
 * SUMMARY:
 *   The solidarity mirror trap emerges when epistemic communities prioritize
 *   group cohesion over critical inquiry, creating a structural tension
 *   between relational belonging and truth-seeking. Communities facing
 *   external threat or internal fragility often adopt solidarity norms that
 *   suppress dissent, initially as a coordination mechanism for survival.
 *   Over time, these norms become self-reinforcing: dissenting members exit
 *   or self-censor, consensus maintainers accumulate social capital, and the
 *   community loses capacity to identify its own blind spots. The constraint
 *   exhibits genuine coordination function (preventing destructive conflict,
 *   enabling collective action under threat) alongside asymmetric extraction
 *   (suppression of epistemic diversity, concentration of agenda-setting
 *   power). The theater_ratio (0.58) reflects that solidarity performances
 *   (public affirmations, loyalty signals, consensus rituals) have partially
 *   decoupled from epistemic function — the rituals maintain group identity
 *   but no longer reliably track truth. The constraint is downstream of
 *   consensus_as_cognitive_cost (the mountain constraint that achieving
 *   consensus requires cognitive work) but adds an extractive layer: the
 *   community weaponizes the cognitive cost of dissent to suppress challenges
 *   to consensus.
 *
 * KEY AGENTS:
 *   - Dissenting Members: Primary victims (powerless/identity_locked) — face choice between epistemic integrity and relational belonging; identity-locked within community; exit requires abandoning self-concept as loyal member
 *   - Peripheral Members: Secondary victims (moderate/constrained) — experience mixed coordination and extraction; can exit at significant social cost; not identity-locked but face career and relational penalties for dissent
 *   - Consensus Maintainers: Primary beneficiaries (institutional/arbitrage) — high-status members and identity entrepreneurs who benefit from solidarity norms; concentrate social capital and agenda-setting power; can exit to other communities if dysfunction becomes severe
 *   - Truth-Seeking Capacity: Abstract victim (powerless/trapped) — epistemic commons that cannot exit or organize; bears full cost of blind spots and false consensus; no advocate within the community structure
 *   - Meta-Community Reformers: Organized agents (organized/mobile) — building alternative discourse architectures (steelmanning, adversarial collaboration, red-teaming); see solidarity trap as temporary coordination failure with institutional design solution
 *   - Ritual Solidarity Performers: Institutional actors (institutional/arbitrage) — recognize that solidarity performances have become decoupled from epistemic function; maintain rituals through inertia and identity maintenance rather than belief in their effectiveness
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(solidarity_mirror_trap, 0.48).
domain_priors:suppression_score(solidarity_mirror_trap, 0.62).
domain_priors:theater_ratio(solidarity_mirror_trap, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(solidarity_mirror_trap, extractiveness, 0.48).
narrative_ontology:constraint_metric(solidarity_mirror_trap, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(solidarity_mirror_trap, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(solidarity_mirror_trap, tangled_rope).
narrative_ontology:human_readable(solidarity_mirror_trap, "Solidarity Mirror Trap in Epistemic Communities").
narrative_ontology:topic_domain(solidarity_mirror_trap, "epistemology/social_psychology/discourse").

domain_priors:requires_active_enforcement(solidarity_mirror_trap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(solidarity_mirror_trap, consensus_maintainers).
narrative_ontology:constraint_beneficiary(solidarity_mirror_trap, high_status_members).
narrative_ontology:constraint_beneficiary(solidarity_mirror_trap, identity_entrepreneurs).
narrative_ontology:constraint_victim(solidarity_mirror_trap, truth_seeking_capacity).
narrative_ontology:constraint_victim(solidarity_mirror_trap, dissenting_members).
narrative_ontology:constraint_victim(solidarity_mirror_trap, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISSENTING MEMBER (SNARE) — Identity-locked within the community; challenging consensus threatens both social bonds and self-concept as loyal member. Exit would require abandoning the identity constructed through group membership. Experiences maximum extraction: must choose between epistemic integrity and relational belonging. The solidarity norm extracts their capacity for independent judgment while offering minimal coordination benefit — the 'coordination' is one-way conformity.
constraint_indexing:constraint_classification(solidarity_mirror_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: PERIPHERAL MEMBER (TANGLED ROPE) — Constrained by social costs but not identity-locked; can exit at significant relational price. Experiences genuine coordination benefit (access to community resources, shared knowledge, mutual support) alongside extraction (pressure to suppress doubts, career risk of dissent). The mixed experience reveals the constraint's hybrid structure: real coordination function embedded with asymmetric extraction.
constraint_indexing:constraint_classification(solidarity_mirror_trap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CONSENSUS MAINTAINER (ROPE) — High-status member or identity entrepreneur who benefits from solidarity norms. Experiences the constraint as pure coordination: maintaining group cohesion, preventing destructive conflict, enabling collective action. Can exit to other communities if this one becomes dysfunctional. Net beneficiary — the solidarity norm concentrates social capital and agenda-setting power in their position.
constraint_indexing:constraint_classification(solidarity_mirror_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: META-COMMUNITY REFORMERS (SCAFFOLD) — Organized agents building alternative discourse norms (steelmanning protocols, adversarial collaboration frameworks, red-teaming practices, dissent protection mechanisms). See the solidarity trap as a temporary coordination failure with a sunset: as communities adopt structured disagreement protocols, the trade-off between solidarity and truth-seeking dissolves. Low effective extraction because they have agency and see an exit path through institutional design.
constraint_indexing:constraint_classification(solidarity_mirror_trap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: RITUAL SOLIDARITY PERFORMER (PITON) — Long-time member who recognizes that solidarity performances (public affirmations, loyalty signals, consensus rituals) have become decoupled from actual epistemic function. The rituals persist through institutional inertia and identity maintenance rather than because they produce knowledge or prevent genuine harm. Theater ratio is high but not maximal — some residual coordination function remains in preventing destructive conflict, even as the epistemic function has atrophied.
constraint_indexing:constraint_classification(solidarity_mirror_trap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a universal/civilizational perspective, the solidarity-truth trade-off exhibits genuine coordination function (communities need cohesion to survive, especially under external threat) alongside structural extraction (suppression of dissent degrades collective epistemic capacity). The constraint is not a natural law — alternative discourse architectures exist — but neither is it pure extraction. Tangled rope classification reflects the irreducible tension between two legitimate coordination goals that the current institutional form fails to reconcile.
constraint_indexing:constraint_classification(solidarity_mirror_trap, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(solidarity_mirror_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(solidarity_mirror_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(solidarity_mirror_trap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(solidarity_mirror_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(solidarity_mirror_trap, TR),
    TR >= 0.70.

:- end_tests(solidarity_mirror_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The solidarity norm extracts epistemic capacity from dissenting members and the collective truth-seeking function, concentrating social capital and agenda-setting power in consensus maintainers. The extraction is not maximal because genuine coordination function exists — communities do need cohesion mechanisms, especially under external threat. But the coordination benefit is asymmetrically distributed: consensus maintainers capture most of the benefit while dissenting members bear most of the cost. The value reflects that roughly half the constraint's effect is legitimate coordination and half is extractive suppression. Suppression (0.62): High. Significant barriers to dissent include identity-lock (self-concept tied to group membership), social penalties (ostracism, status loss), career risk (professional communities), and internalized norms (self-censorship persists even after structural barriers are removed). Suppression is not total — some members do dissent and some do exit — but the barriers are substantial enough that most potential dissent is suppressed before expression. Theater ratio (0.58): Moderate-high. Solidarity performances (loyalty signals, consensus affirmations, ritual agreement) have substantially decoupled from epistemic function. The performances maintain group identity and prevent overt conflict but do not reliably track truth or identify blind spots. The theater has increased over the interval as the community has selected for members who perform solidarity and selected against members who challenge consensus, creating a ratchet effect where each generation of members is more performative and less epistemically diverse than the last.
 *
 * PERSPECTIVAL GAP:
 *   The solidarity mirror trap demonstrates how the same structural phenomenon — prioritizing group cohesion over critical inquiry — appears as pure extraction (snare) to identity-locked dissenting members who must choose between epistemic integrity and belonging; as mixed coordination and extraction (tangled rope) to peripheral members who experience both community benefits and suppression costs; as pure coordination (rope) to consensus maintainers who benefit from the solidarity norm; as a temporary problem with an institutional design solution (scaffold) to meta-community reformers building alternative discourse architectures; as a degraded ritual (piton) to long-time members who recognize the performative decoupling; and as an irreducible tension between two legitimate coordination goals (tangled rope) to the analytical observer. The perspectival gap reveals that 'solidarity' is not a single thing but a structural position: beneficiaries experience it as coordination, victims experience it as extraction, and the analytical view sees both functions coexisting in tension.
 *
 * DIRECTIONALITY LOGIC:
 *   Dissenting members are victims with identity_locked exit options, producing high directionality (d ≈ 0.89) and high experienced extraction. Their identity is constituted through group membership, making exit psychologically costly even when structurally possible. Peripheral members are victims with constrained exit options, producing moderate-high directionality (d ≈ 0.70) — they face significant social and career costs but are not identity-locked. Consensus maintainers are beneficiaries with arbitrage exit options, producing low directionality (d ≈ 0.05) and negative experienced extraction — they capture social capital and can exit to other communities if this one becomes dysfunctional. The truth-seeking capacity is an abstract victim with trapped exit options (cannot leave or organize), producing maximum directionality (d ≈ 0.95). Meta-community reformers are organized agents with mobile exit options who see themselves as neither pure beneficiaries nor pure victims, producing moderate directionality (d ≈ 0.45). The directionality spread reveals the constraint's hybrid structure: extraction flows from dissenting members and epistemic commons toward consensus maintainers, while genuine coordination function flows in multiple directions.
 *
 * MANDATROPHY ANALYSIS:
 *   The solidarity mirror trap resolves the mandatrophy by demonstrating that the solidarity-truth trade-off is neither pure coordination (rope) nor pure extraction (snare) but an irreducible hybrid (tangled rope from the analytical perspective). The constraint has genuine coordination function: communities facing external threat or internal fragility do need cohesion mechanisms to survive. But the coordination function is embedded with asymmetric extraction: the solidarity norm suppresses dissent, concentrates power in consensus maintainers, and degrades collective epistemic capacity. The mandatrophy question 'Is this coordination or extraction?' has the answer 'Both, and the ratio depends on your structural position.' Dissenting members experience it as extraction because they bear the cost (suppressed voice, identity conflict) without capturing the benefit (social capital, agenda-setting power). Consensus maintainers experience it as coordination because they capture the benefit without bearing the cost. The analytical observer sees both functions and recognizes that alternative institutional designs (structured disagreement protocols, dissent protection mechanisms) could preserve the coordination function while reducing the extraction — but such designs are not yet widely adopted, so the tangled rope classification reflects the current equilibrium rather than the theoretical optimum.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    solidarity_threshold_ambiguity,
    'What level of solidarity is necessary for community survival vs what level becomes extractive suppression of dissent?',
    'Longitudinal study of communities with varying solidarity norms; correlation between dissent tolerance and community longevity, epistemic accuracy, and member satisfaction',
    'If threshold is low: most solidarity norms are extractive. If threshold is high: most communities are legitimately coordinating survival needs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(solidarity_threshold_ambiguity, empirical, 'Threshold distinguishing necessary solidarity from extractive suppression').

omega_variable(
    identity_lock_reversibility,
    'Is identity-lock from solidarity norms reversible through exposure to alternative communities, or does it persist as internalized suppression?',
    'Post-exit interviews with former members; measurement of continued self-censorship patterns after leaving high-solidarity communities; comparison with members who left low-solidarity communities',
    'If reversible: suppression is structural and removable. If persistent: suppression is internalized and the effective extraction is higher than structural measures suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Whether identity-lock persists after exit from solidarity community').

omega_variable(
    external_threat_calibration,
    'Do communities accurately calibrate solidarity norms to actual external threat levels, or do they systematically overestimate threat to justify suppression?',
    'Comparison of perceived vs measured external threat across communities; correlation between threat perception and dissent suppression; analysis of threat narratives used to justify solidarity norms',
    'If calibrated: solidarity norms are responsive coordination. If systematically biased: threat narratives are cover stories for extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(external_threat_calibration, empirical, 'Whether solidarity norms track actual external threat').

omega_variable(
    structured_disagreement_effectiveness,
    'Do structured disagreement protocols (steelmanning, adversarial collaboration, red-teaming) actually preserve both solidarity and truth-seeking, or do they merely formalize the suppression?',
    'Comparison of epistemic outcomes and member retention between communities using structured disagreement vs traditional solidarity norms; measurement of dissent expression rates and quality under each regime',
    'If effective: scaffold perspective confirmed — institutional design can resolve the trade-off. If ineffective: the tension is irreducible and tangled rope is the stable equilibrium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structured_disagreement_effectiveness, empirical, 'Whether structured disagreement protocols resolve solidarity-truth tension').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(solidarity_mirror_trap, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(solidarity_tr_t0, solidarity_mirror_trap, theater_ratio, 0, 0.35).
narrative_ontology:measurement(solidarity_tr_t3, solidarity_mirror_trap, theater_ratio, 3, 0.42).
narrative_ontology:measurement(solidarity_tr_t6, solidarity_mirror_trap, theater_ratio, 6, 0.51).
narrative_ontology:measurement(solidarity_tr_t9, solidarity_mirror_trap, theater_ratio, 9, 0.58).

% Extraction over time
narrative_ontology:measurement(solidarity_be_t0, solidarity_mirror_trap, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(solidarity_be_t3, solidarity_mirror_trap, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(solidarity_be_t6, solidarity_mirror_trap, base_extractiveness, 6, 0.44).
narrative_ontology:measurement(solidarity_be_t9, solidarity_mirror_trap, base_extractiveness, 9, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(solidarity_mirror_trap, identity_coordination).

% DUAL FORMULATION NOTE:
% The solidarity mirror trap is downstream of consensus_as_cognitive_cost (the mountain constraint that achieving consensus requires cognitive work). The upstream constraint establishes that consensus is inherently costly; the downstream constraint adds an extractive layer by weaponizing that cost to suppress dissent. The two constraints have different ε values because they measure different structural phenomena: consensus_as_cognitive_cost measures the inherent difficulty of agreement (ε ≈ 0.08, mountain), while solidarity_mirror_trap measures the asymmetric distribution of that cost when communities prioritize cohesion over inquiry (ε = 0.48, tangled rope).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
