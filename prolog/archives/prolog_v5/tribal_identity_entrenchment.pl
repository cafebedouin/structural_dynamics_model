% ============================================================================
% CONSTRAINT STORY: tribal_identity_entrenchment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tribal_identity_entrenchment, []).

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
 *   constraint_id: tribal_identity_entrenchment
 *   human_readable: Tribal Identity Entrenchment
 *   domain: social/political/cognitive
 *
 * SUMMARY:
 *   Tribal identity entrenchment represents a structural constraint where
 *   individual cognitive autonomy and openness to alternative perspectives
 *   are subordinated to group identity maintenance and boundary
 *   reinforcement. Unlike coordination mechanisms that merely align behavior,
 *   tribal entrenchment operates through identity fusion — the agent's sense
 *   of self becomes constituted through tribal membership, making exit not
 *   just costly but psychologically unthinkable from within the identity
 *   frame. The constraint exhibits measurable growth over generational
 *   timescales (extractiveness rising from 0.35 to 0.58, theater ratio from
 *   0.38 to 0.64), indicating that theatrical and enforcement components have
 *   intensified even as original coordination functions have atrophied. This
 *   pattern is diagnostically distinct from a pure Rope (which would show
 *   stable metrics) or a Scaffold (which would show declining metrics as the
 *   sunset approach succeeds). The rising theater ratio indicates that tribal
 *   identity is increasingly sustained through performative ritual —
 *   reenactment of historical narratives, ceremonial boundary maintenance,
 *   symbolic identity markers — rather than through genuine coordination
 *   solutions to material problems. The rising extractiveness indicates that
 *   enforcement mechanisms have become more sophisticated, capturing larger
 *   portions of individual agency and epistemic autonomy. The constraint
 *   illustrates how indexical classification reveals the tension between
 *   coordination function (which may be genuine and valuable) and extraction
 *   mechanism (which concentrates benefits on leadership and identity
 *   gatekeepers). It is a canonical Tangled Rope at the institutional level,
 *   masquerading as a Mountain (natural property of human cognition) at the
 *   analytical/civilizational level.
 *
 * KEY AGENTS:
 *   - Tribal Members (powerless/identity_locked): Primary victims — identity constituted through tribalism; exit requires becoming a different person
 *   - Tribal Leadership (institutional/arbitrage): Primary beneficiaries — consolidate authority, extract loyalty, control narrative
 *   - Identity Gatekeepers (institutional/arbitrage): Secondary beneficiaries — enforce boundaries, police conformity, maintain purity narratives
 *   - Dissidents (moderate/constrained): Secondary victims — recognize constraint but face high exit costs; experiencing perspectival gap between born members and conscious dissenters
 *   - Privileged Tribe Members (powerful/mobile): Intermediate position — benefit from network capital while retaining some exit options; risk deepening capture if mobility is illusory
 *   - Cosmopolitan Movement (organized/constrained): Structural counter-force — building alternative identity frameworks with lower extraction, longer timescale
 *   - Children (powerless/trapped): Highest vulnerability — socialized during critical periods before alternative frames accessible; identity imprinting during sensitive windows creates developmental lock-in
 *   - Analytical Observer (analytical/analytical): Risks false summit — naturalizes contingent institutional arrangement as immutable human nature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tribal_identity_entrenchment, 0.58).
domain_priors:suppression_score(tribal_identity_entrenchment, 0.68).
domain_priors:theater_ratio(tribal_identity_entrenchment, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tribal_identity_entrenchment, extractiveness, 0.58).
narrative_ontology:constraint_metric(tribal_identity_entrenchment, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(tribal_identity_entrenchment, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tribal_identity_entrenchment, tangled_rope).
narrative_ontology:human_readable(tribal_identity_entrenchment, "Tribal Identity Entrenchment").
narrative_ontology:topic_domain(tribal_identity_entrenchment, "social/political/cognitive").

domain_priors:requires_active_enforcement(tribal_identity_entrenchment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tribal_identity_entrenchment, tribal_leadership).
narrative_ontology:constraint_beneficiary(tribal_identity_entrenchment, identity_gatekeepers).
narrative_ontology:constraint_victim(tribal_identity_entrenchment, individual_agency).
narrative_ontology:constraint_victim(tribal_identity_entrenchment, epistemic_openness).
narrative_ontology:constraint_victim(tribal_identity_entrenchment, cross_tribe_cooperation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRIBAL MEMBER (SNARE) — Identity is constituted through tribal membership. Exit requires becoming a different person — abandoning the self-concept built within the tribe, severing bonds that define belonging, adopting an alien worldview. Structurally mobile (could physically leave) but psychologically trapped by identity fusion. Experiences maximum extraction: cognitive capture constrains all choices; tribal authority claims dominion over belief, association, and values. No exit path visible from within the identity frame.
constraint_indexing:constraint_classification(tribal_identity_entrenchment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: DISSIDENT (TANGLED ROPE) — Constrained by social cost (expulsion, ostracism, economic pressure) but not materially trapped. Recognizes some coordination function (shared values, mutual aid, cultural continuity) alongside asymmetric extraction (enforcement of conformity, punishment for deviation). Can theoretically exit by paying high social/economic cost, but identity loss is partial — dissident has begun to see the constraint. Moderate extraction; genuine perspectival gap from tribal member.
constraint_indexing:constraint_classification(tribal_identity_entrenchment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: TRIBAL LEADERSHIP (ROPE) — Benefits from coordination: consolidates authority, aligns member behavior, reinforces cultural continuity, maintains group cohesion for collective action. Experiences the constraint as solving a coordination problem — members require narrative unity and identity salience to maintain group integrity. Leadership has exit options (can leverage power externally, can adapt rules). Extraction flows toward them but they perceive the mechanism as coordination.
constraint_indexing:constraint_classification(tribal_identity_entrenchment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: TRIBAL IDENTITY INSTITUTION (PITON) — Over generational and civilizational timescales, tribal identity operates increasingly through theater and inertia. Original function (collective survival, coordination of resources in harsh environments) has been replaced by performative ritual: historical narratives about tribal origins, ceremonial reenactment of identity markers, symbolic enforcement of boundary maintenance. The institution maintains itself through theater — members perform tribalism to sustain the structure, not because the original coordination problems remain. Theater ratio is high (0.64); functionality has atrophied.
constraint_indexing:constraint_classification(tribal_identity_entrenchment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: COSMOPOLITAN MOVEMENT (SCAFFOLD) — Organized agents (educators, diaspora networks, cross-cultural initiatives) are building alternative identity frameworks that contextualize tribal identity within larger structures (national identity, humanity, shared values across groups). See tribal entrenchment as a temporary coordination failure being solved by institutional structures with sunset logic: as education, economic integration, and cross-group contact increase, tribal identity becomes optional rather than mandatory. Suppression is declining through exposure. Extraction mechanism loses force as members discover exit pathways.
constraint_indexing:constraint_classification(tribal_identity_entrenchment, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: PRIVILEGED TRIBAL MEMBER (TANGLED ROPE) — High-status member (intellectual, entrepreneur, diplomat) benefits from tribal identity as a network asset while retaining exit options (education abroad, external employment, cosmopolitan access). Experiences genuine coordination (tribal network provides opportunities, cultural capital, belonging) alongside asymmetric extraction (expected conformity, restrictions on public criticism, loyalty demands). Can exercise some exit options (travel, external careers) without full identity loss. Intermediate extraction — benefits and costs both present.
constraint_indexing:constraint_classification(tribal_identity_entrenchment, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE SUMMIT (MOUNTAIN) — At the civilizational horizon, tribal identity appears as an immutable natural law: humans have evolved to recognize in-groups, tribalism is inherent to our psychology, identity fusion with groups is a universal feature of human cognition. From this view, tribal entrenchment is not a constraint but a natural property of how minds work. However, the structural data contradicts this: the constraint exhibits measurable extraction (0.58), active enforcement, and measurable suppression. The mountain classification reveals a false summit — the 'natural' framing naturalizes what is actually a contingent institutional arrangement maintained through psychological capture and social enforcement.
constraint_indexing:constraint_classification(tribal_identity_entrenchment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: CHILD (SNARE) — Socialized from birth into tribal identity during the critical period when identity frames are being constructed. Has no opportunity to compare worldviews before identity fusion occurs. Trapped by developmental psychology as much as by social enforcement — identity has been imprinted during sensitive periods when alternative frames are not accessible. Maximum extraction: childhood socialization patterns prevent even internal questioning of tribal boundaries. Exit is developmentally locked, not just socially locked.
constraint_indexing:constraint_classification(tribal_identity_entrenchment, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tribal_identity_entrenchment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tribal_identity_entrenchment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tribal_identity_entrenchment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(tribal_identity_entrenchment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(tribal_identity_entrenchment, TR),
    TR >= 0.70.

:- end_tests(tribal_identity_entrenchment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts individual agency (conformity demands), epistemic autonomy (beliefs are policed), and social freedom (association is constrained). The extraction is genuine and measurable through behavioral restrictions and social costs of deviation. However, it is not maximal because some coordination function remains authentic — tribal structures do solve genuine problems of collective action, mutual aid, and cultural continuity in contexts where alternative institutions are weak or unavailable. The value reflects both real extraction and real coordination present simultaneously. Suppression (0.68): High. Multiple reinforcement mechanisms operate: social enforcement (ostracism, reputation damage), economic dependency (preferential access to resources flows through tribal authority), developmental lock-in (identity imprinting during childhood before alternative frames are accessible), and cognitive capture (epistemic closure prevents even internal questioning). Suppression is not total (exit is technically possible) but is severe enough that most members experience it as irreversible. Theater ratio (0.64): Elevated and increasing. Contemporary tribal identity operates increasingly through ritualized performance and symbolic boundary maintenance rather than coordination of material problems. Original functions (coordinating mutual defense, resource distribution, knowledge preservation in harsh environments) have been substantially displaced by modern institutions (nation-states, markets, education systems). Tribal identity persists through theater — reenactment of historical narratives, ceremonial identity affirmation, public performance of group loyalty — rather than through functional necessity. The rising theater ratio over the 30-year interval indicates intensifying performative content and declining functional content.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces extreme perspectival divergence across observation points. The tribal member (powerless/identity_locked) experiences Snare — pure extraction with no visible exit path — because their identity frame makes questioning tribal authority literally unthinkable from within. The dissident (moderate/constrained) experiences Tangled Rope — recognizing both genuine coordination and asymmetric extraction — because conscious deviation has partially broken the identity frame and revealed the constraint's enforced nature. The tribal leadership (institutional/arbitrage) experiences Rope — pure coordination — because their structural position benefits from the mechanism and they have exit options (can leverage power externally, can reframe identity to suit circumstances). The cosmopolitan movement (organized/constrained) experiences Scaffold — a temporary problem being solved — because they are building alternative identity frameworks (national identity, cosmopolitan values, human commonality) that contextualize tribal identity within larger structures. The analytical observer at civilizational scope risks experiencing Mountain — naturalizing the constraint as an immutable property of human psychology — but this is a false summit; the structural data reveals contingent institutional arrangement, not natural law. The privileged member (powerful/mobile) experiences Tangled Rope with asymmetric structure: they benefit more from the coordination function (network capital, social status) and retain more exit options (mobility), but they also remain partially captured (expected conformity, loyalty demands). The perspectival gap between tribal member and leadership is maximal — same constraint, completely opposite experienced extractiveness — revealing that the constraint's true function is extraction masked as coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from structural position within the constraint: beneficiaries have low d (they experience negative extraction — the constraint subsidizes them), while victims have high d (they experience positive extraction — the constraint extracts from them). Tribal leadership and identity gatekeepers are beneficiaries (d ≈ 0.1–0.2, low d, negative χ) — they benefit from consolidated authority, loyalty extraction, and narrative control. Their exit options are high (arbitrage — they can leverage power externally), which keeps d low. Tribal members are victims (d ≈ 0.85–0.95, high d, high χ) — the constraint extracts agency, autonomy, and epistemic freedom. Their exit options are severely constrained by identity fusion (identity_locked), which raises d toward maximum. Dissidents occupy intermediate position (d ≈ 0.60–0.70) — they have begun to escape identity lock but face significant social costs (constrained exit options), keeping d elevated. Privileged members have lower d than ordinary members (d ≈ 0.45–0.55) despite being victims, because their exit options are higher (mobile) and they enjoy some benefits (network capital). Children have highest d (d ≈ 0.95) because they are trapped both by developmental stage (no alternative frames accessible) and by socialization (identity imprinted before choice possible), and they have no recognized exit options. The analytical observer's d (≈0.72, derived from canonical analytical value) reveals that the false summit classification is a perspectival artifact, not a structural truth.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint avoids mandatrophy (Theorem 3 false-positive risk) by explicitly declaring its coordination function (tribal identity genuinely coordinates collective action, mutual aid, cultural continuity) while maintaining high extraction metrics. The tangled_rope classification correctly identifies that both functions are structurally present. The rising theater ratio and extractiveness over the 30-year interval reveal the dynamic: as original coordination problems become less acute (modern institutions increasingly provide mutual defense, resource distribution, education), the constraint's functional component decays into theater while its extractive component intensifies. This is the Piton signature at the civilizational level (function atrophies, theater rises, inertia maintains) but Tangled Rope at the biographical level (both coordination and extraction still experienced as real). The mandatrophy would arise if the observer claimed pure Rope (ignoring extraction) or pure Snare (ignoring genuine coordination). The indexed classification prevents this by showing that different time horizons and power positions produce different apparent types — all are legitimate observations of the same constraint from different angles. The false summit (Mountain classification) reveals the oracle gap (Theorem 4): the analytical position's own 'natural law' framing prevents it from seeing the institutional contingency that other perspectives reveal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_fusion_depth,
    'What proportion of tribal identity entrenchment is cognitive (identity fusion, epistemic closure) vs. social (enforcement, ostracism, economic dependency)?',
    'Comparative analysis of exit behavior: track members who leave the tribe and measure retention of tribal identity markers post-exit. If suppression is primarily social, identity should relax after social enforcement stops. If suppression is primarily cognitive, identity markers persist despite absence of enforcement.',
    'If primarily cognitive: constraint persists even after social enforcement declines; reclassify toward Snare from all perspectives. If primarily social: constraint should loosen with enforcement reduction; Scaffold perspective gains credibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_depth, empirical, 'Cognitive vs. social basis of identity entrenchment').

omega_variable(
    enforcement_mechanism_opacity,
    'Is tribal enforcement experienced by members as explicit coercion or as internalized normativity?',
    'Ethnographic interviews examining member phenomenology: do members report feeling coerced or do they report wanting to conform? Compare external observers'' assessment of enforcement severity with members'' own perception.',
    'If externally coercive but internally invisible: member consciousness does not register the constraint''s extractive nature; suppression is higher than members perceive. If internalized as normativity: enforcement has succeeded completely in its goal — members police themselves and experience the constraint as natural/necessary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_mechanism_opacity, empirical, 'Phenomenological experience of tribal enforcement').

omega_variable(
    childhood_socialization_reversibility,
    'Can adult-acquired cosmopolitan identity displace childhood-imprinted tribal identity, or does childhood imprinting create indelible neural patterns that persist even when cognitive frameworks change?',
    'Longitudinal neuroscience and behavioral tracking of members who leave tribe in adulthood: MRI studies of neural response to tribal symbols in leavers vs. never-members; measurement of automaticity of tribal cultural responses (implicit association tests, emotional congruence tasks)',
    'If reversible: identity_locked exit option becomes mobile/constrained with time and exposure; Scaffold perspective is realistic. If irreversible: childhood imprinting creates permanent vulnerability to re-capture; identity_locked persists even in leavers and constrains second-generation members.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(childhood_socialization_reversibility, empirical, 'Reversibility of childhood tribal identity imprinting').

omega_variable(
    coordination_function_authenticity,
    'Does tribal identity coordination serve genuine collective action problems (mutual defense, resource sharing, knowledge preservation), or has the coordination function been replaced entirely by extraction?',
    'Historical and institutional analysis: identify specific coordination problems the tribal structure solves that would not be solved by non-tribal alternatives (national legal systems, markets, civic associations). Measure whether those problems remain unresolved if tribal identity is loosened.',
    'If genuine coordination problems remain: Tangled Rope classification is accurate; constraint has real social value alongside extraction. If coordination function is fully internalized in non-tribal institutions: Snare classification is more accurate; tribal identity now exists purely as extraction with theatrical coordination cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_authenticity, empirical, 'Residual coordination function of tribal identity').

omega_variable(
    generational_decay_rate,
    'Is tribal identity entrenchment weakening, maintaining, or strengthening across generational cohorts?',
    'Longitudinal demographic and survey data: measure tribal identity salience, language retention, endogamy rates, occupational segregation by tribe across age cohorts over 20+ year period. Identify inflection points where trajectory changes.',
    'If weakening: Scaffold sunset is real; constraint is in decay phase. If maintaining: Piton classification is accurate; inertia dominates. If strengthening: threat response is activating deeper entrenchment; suppression may be increasing despite apparent modernization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(generational_decay_rate, empirical, 'Generational trajectory of tribal identity strength').

omega_variable(
    mobility_illusion,
    'Do economically or educationally mobile tribe members who retain tribal identity represent genuine arbitrage exit or deepened identity capture (using external resources to reinforce tribal authority)?',
    'Behavioral tracking of high-status members: measure likelihood of public tribal criticism, internal reform advocacy, or outmarriage. Compare to baseline for non-tribal-identified peers at same status level. If high-status members systematize tribal authority using external resources, they represent deepened capture, not arbitrage exit.',
    'If arbitrage exit is real: privileged member perspective (mobile/powerful) is accurate; some agents escape extraction. If capture deepens with external resources: mobility is illusory; privileged members become enforcement proxies; Snare classification applies to all members regardless of status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mobility_illusion, empirical, 'Whether mobility represents exit or deepened capture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tribal_identity_entrenchment, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tribe_tr_t0, tribal_identity_entrenchment, theater_ratio, 0, 0.38).
narrative_ontology:measurement(tribe_tr_t15, tribal_identity_entrenchment, theater_ratio, 15, 0.51).
narrative_ontology:measurement(tribe_tr_t30, tribal_identity_entrenchment, theater_ratio, 30, 0.64).
narrative_ontology:measurement(tribe_tr_t10, tribal_identity_entrenchment, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(tribe_be_t0, tribal_identity_entrenchment, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(tribe_be_t15, tribal_identity_entrenchment, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(tribe_be_t30, tribal_identity_entrenchment, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(tribe_be_t10, tribal_identity_entrenchment, base_extractiveness, 10, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tribal_identity_entrenchment, identity_coordination).
narrative_ontology:boltzmann_floor_override(tribal_identity_entrenchment, 0.12).
narrative_ontology:affects_constraint(tribal_identity_entrenchment, epistemic_closure_in_groups).
narrative_ontology:affects_constraint(tribal_identity_entrenchment, intergroup_hostility_escalation).
narrative_ontology:affects_constraint(tribal_identity_entrenchment, identity_politics_polarization).
narrative_ontology:affects_constraint(tribal_identity_entrenchment, cultural_continuity_paradox).

% DUAL FORMULATION NOTE:
% Tribal identity entrenchment is upstream of several derived constraints: epistemic closure (when tribal identity prevents updating worldviews based on external evidence), intergroup hostility (when tribal identity creates us/them frames that escalate conflict), and identity politics polarization (when tribal entrenchment becomes politicized). Each downstream constraint has higher extractiveness values because they represent amplification of the base entrenchment through additional institutional layers. The upstream constraint is the identity fusion mechanism itself; downstream constraints show its social and political consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tribal_identity_entrenchment, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
