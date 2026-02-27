% ============================================================================
% CONSTRAINT STORY: tear_gas_repression_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tear_gas_repression_2026, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: tear_gas_repression_2026
 *   human_readable: The Tear Gas Riot-Incentive Loop
 *   domain: political/technological/social
 *
 * SUMMARY:
 *   The tear gas riot-incentive loop represents a structural constraint
 *   operating between state security apparatus and protest participants,
 *   where a technology ostensibly designed for crowd management functionally
 *   generates the violent conditions it claims to prevent. This creates a
 *   feedback mechanism: initial deployment creates panic and pain responses
 *   in participants, those responses justify escalated force and legal
 *   accountability for 'rioting,' which in turn justifies expanded tear gas
 *   capabilities and normalized use, which increases exposure and trauma in
 *   future protests, which increases defensive/aggressive responses. The
 *   constraint operates as a snare for protest participants and bystanders
 *   (forced exposure, health harm, chemical trauma with no exit) while
 *   operating as coordination and opportunity for the state apparatus (public
 *   order management, funding justification, legal cover). The critical
 *   structural feature is the incentive asymmetry: riot behavior justifies
 *   budget escalation and tactical expansion for the apparatus, so the
 *   apparatus has structural incentive to deploy in ways that generate rather
 *   than prevent riot behavior. The constraint's extractiveness has increased
 *   over the measurement interval (0.45 → 0.68) as deployment frequency has
 *   increased and counter-tactics have plateaued. The theater ratio (0.68)
 *   reflects that a significant portion of tear gas deployment is
 *   performative—demonstrating state action and control rather than actually
 *   resolving public order problems.
 *
 * KEY AGENTS:
 *   - Protest Participants: Primary victims (powerless/trapped) — experience chemical trauma, panic responses, health harm; no exit from public assembly right without abandoning protest
 *   - Civilian Bystanders: Collateral victims (powerless/trapped) — non-participants exposed to drift, residual contamination; zero power to avoid constraint
 *   - State Security Apparatus: Primary beneficiary (institutional/arbitrage) — gains operational control, funding justification, legal cover; experiences constraint as coordination mechanism
 *   - Civil Rights Coalition: Organized victims (organized/constrained) — can document harm and pursue litigation but face institutional barriers (qualified immunity, normalization); partial exit through legal/legislative action
 *   - Legislative Reform Movement: Reform actors (organized/constrained) — pursuing chemical weapons restrictions with measurable progress; see sunset clause (scaffold perspective)
 *   - International Police Training Complex: Institutional inertia (institutional/arbitrage) — maintains tear gas in academies and doctrine through ritual; declining functional utility but high theater
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tear_gas_repression_2026, 0.68).
domain_priors:suppression_score(tear_gas_repression_2026, 0.72).
domain_priors:theater_ratio(tear_gas_repression_2026, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tear_gas_repression_2026, extractiveness, 0.68).
narrative_ontology:constraint_metric(tear_gas_repression_2026, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(tear_gas_repression_2026, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tear_gas_repression_2026, snare).
narrative_ontology:human_readable(tear_gas_repression_2026, "The Tear Gas Riot-Incentive Loop").
narrative_ontology:topic_domain(tear_gas_repression_2026, "political/technological/social").

domain_priors:requires_active_enforcement(tear_gas_repression_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tear_gas_repression_2026, state_security_apparatus).
narrative_ontology:constraint_victim(tear_gas_repression_2026, protest_participants).
narrative_ontology:constraint_victim(tear_gas_repression_2026, civilian_bystanders).
narrative_ontology:constraint_victim(tear_gas_repression_2026, public_order_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROTEST PARTICIPANT (SNARE) — Faces tear gas deployed ostensibly to disperse crowds, but the agent (tear gas) creates chemical trauma, panic responses, and aggressive self-defense behaviors that justify escalated state force and mass detention. No meaningful exit: attending public assembly becomes hazardous even when protest remains nonviolent. Chemical restraint creates the violent response it claims to prevent, then that response legitimates further restraint. The participant is trapped in the mechanism.
constraint_indexing:constraint_classification(tear_gas_repression_2026, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: CIVILIAN BYSTANDER (SNARE) — Not a protest participant but exposed to tear gas drift, contaminated water, residual chemical irritants in public spaces. No power to avoid the constraint and no organizational capacity to exit the affected zone. Bears extraction (physical harm, respiratory damage, property contamination) with zero beneficiary status. The bystander is maximally trapped — zero entry choice, forced bearing of costs.
constraint_indexing:constraint_classification(tear_gas_repression_2026, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: STATE SECURITY APPARATUS (ROPE) — Experiences tear gas deployment as coordination mechanism: manages crowd distribution, establishes zones of control, enables arrest operations, and justifies security spending. From this perspective, the tear gas solves a collective action problem (public order) through a mechanism (chemical dispersal) that maintains state capacity. The apparatus benefits from: (a) operational control, (b) justified funding escalation, (c) legal cover for use of force, (d) demonstrable 'action' responding to public order crises. High arbitrage: the apparatus can shift tactics, funding, or deployment zones without losing function. Experiences the constraint as enabling rather than extractive.
constraint_indexing:constraint_classification(tear_gas_repression_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CIVIL RIGHTS COALITION (TANGLED ROPE) — Organized actors with legal capacity and documentation authority see a hybrid: the tear gas system does coordinate some legitimate crowd management (actual traffic safety, zone isolation) while simultaneously extracting through health harm, legal liability, and suppressed assembly rights. The coalition has partial exit (litigation, legislative action, public testimony) but faces institutional barriers (qualified immunity, police union protections, normalization of chemical weapons in policing). Moderate extraction — the coalition can fight back but pays continuous costs. Benefits from the visibility the coalition gains when documenting harm; bears costs of legal battles and continued chemical exposure in their communities.
constraint_indexing:constraint_classification(tear_gas_repression_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGISLATIVE REFORM MOVEMENT (SCAFFOLD) — Sees tear gas deployment as a temporary institutional practice with a genuine sunset clause: chemical-weapons restrictions in domestic policing have passed at the city and state level, restrictions on riot gear procurement are building, and international pressure (chemical weapons conventions) is gradually constraining state capacity to deploy. The scaffold is real — the constraint is declining through legal policy changes. The reform movement experiences lower extraction because it has a clear exit path (legislative victory) and measurable progress toward the sunset. Theater remains high (police agencies maintain contingency deployments) but the constraint's functional basis is atrophying.
constraint_indexing:constraint_classification(tear_gas_repression_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL POLICE TRAINING COMPLEX (PITON) — Tear gas is taught as 'riot control' in police academies globally, deployed in contingency plans, and maintained in equipment inventories despite declining functional utility and rising evidence of long-term respiratory harm. The international training regime persists through institutional inertia: academies teach it because they always have, police departments stock it because it is standard, legal frameworks permit it because restrictions are new. Theater ratio is high (contingency deployments, training exercises, policy justifications) but the primary function (effective crowd management) has eroded as protest movements develop counter-tactics (gas masks, milk washes, coordinated dispersal). The piton classification reflects that the constraint persists through ritual maintenance, not because it works as intended.
constraint_indexing:constraint_classification(tear_gas_repression_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / CHEMICAL PHYSICS (MOUNTAIN) — From a pure chemistry perspective, tear gas is a chemical irritant with predictable physiological effects: CN, CS, and OC compounds bind to TRPV1 nociceptors, producing pain and inflammation. This chemical law is immutable — the irritant effect cannot be negotiated or reformed away. However, the structural data contradicts the mountain classification. The extractive force (the 'riot-incentive loop') is not the chemistry — it is the institutional choice to deploy chemical weapons against civilian protesters. The chemistry itself is morally neutral; the extraction emerges from policy, training doctrine, and legal frameworks that institutionalize its use. The engine's false summit detector identifies this as naturalization of a contingent policy choice.
constraint_indexing:constraint_classification(tear_gas_repression_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tear_gas_repression_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tear_gas_repression_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tear_gas_repression_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(tear_gas_repression_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(tear_gas_repression_2026, TR),
    TR >= 0.70.

:- end_tests(tear_gas_repression_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68): High, increased from initial 0.45 over the interval. This reflects the growing body of evidence that tear gas deployment generates rather than prevents violent responses. Initial deployment creates panic, pain, and disorientation in crowds; participants respond with self-defense behaviors (throwing objects, attempting to escape barriers); apparatus interprets these responses as 'riots' requiring escalated force; legal accountability falls on participants for 'rioting,' not apparatus for chemical escalation. The mechanism is extractive: apparatus gains security expansion and legal cover while participants bear health costs. Suppression (0.72): High. Multiple suppression mechanisms operate: (a) chemical incapacitation (physiological impossibility of coordinated nonviolent action during tear gas exposure), (b) legal suppression (mass arrests under 'rioting' charges following tear gas deployment), (c) informational suppression (apparatus narrative frames chemical deployment as 'necessary' and violent response as 'proof' of threat), (d) exit suppression (participation in public assembly becomes too costly due to chemical hazard). Theater ratio (0.68): High and increasing. Significant portion of deployment is performative: training exercises, contingency deployments, media-visible riot gear that demonstrates state capacity. Police academies teach tear gas deployment as standard doctrine not because it is most effective but because it is traditional. International treaties classify tear gas as a chemical weapon in warfare but permit domestic policing use, creating a legal fiction that masks the same technology's dual function.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a profound perspectival gap between victim and beneficiary. Protest participants and bystanders see pure extraction (Snare): chemical trauma, panic responses, health harm, no coordination benefit. The state apparatus sees coordination and opportunity (Rope): tear gas solves the real problem of managing large crowds and provides operational control. The civil rights coalition sees a hybrid (Tangled Rope): the apparatus does manage some crowd distribution but does so through mechanisms that extract health and assembly rights. The reform movement sees a temporal decline (Scaffold): legislative restrictions are building and the constraint's functional basis is eroding. The international training complex sees institutional inertia (Piton): tear gas persists in doctrine and equipment through ritual, not because it works. The analytical observer risks naturalizing the constraint as inherent to crowd dynamics (Mountain), but the structural data reveals it as contingent on policy choices and institutional incentives. The perspectival gap between apparatus and victims is the defining feature—the same technology appears as enabling coordination to one and as coercive extraction to the other, depending entirely on structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) reflect each agent's structural relationship to the constraint. Protest participants are victims with no exit (trapped exit options) → high d (0.90-0.95) → high f(d) → high experienced extraction. The apparatus is beneficiary with arbitrage options (can shift tactics, funding, deployment zones) → low d (0.10-0.20) → low/negative f(d) → experiences constraint as enabling. Civil rights coalition is organized victim with constrained exit (litigation possible but costly) → moderate d (0.50-0.65) → moderate f(d) → moderate experienced extraction. Reform movement is organized actor with constrained exit and measurable progress toward legislative change → slightly lower d (0.40-0.55) due to visible exit path → lower experienced extraction. The piton classification (institutional/arbitrage perspective) derives not from d (which would be low for the apparatus) but from theater_ratio and the degradation of functional efficacy. International training maintains the constraint through inertia rather than active design, which distinguishes piton from rope.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint resolves the mandatrophy by distinguishing coordination function from extraction function. The apparatus narrative frames tear gas as a coordination mechanism ('crowd management,' 'public order') but the structural evidence indicates the primary function is extraction: generating the violent conditions that justify security expansion. The mandatrophy is resolved by recognizing that the constraint can perform BOTH functions simultaneously—it does manage some crowd distribution (coordination) while extracting health harm and assembly rights (extraction). The question is not 'is this coordination or extraction?' but 'which function is primary and which is secondary?' The measurement trajectory (extractiveness increasing from 0.45 to 0.68, theater increasing from 0.52 to 0.68) and the omega questions establish that extraction is increasingly primary as deployment frequency increases and as evidence accumulates that tear gas generates rather than prevents riot behavior. The constraint is a snare masquerading as rope, not a rope that has degraded into piton. The apparatus intentionally maintains the loop because it generates the conditions that justify continued deployment, funding, and expansion. This is not institutional inertia (piton) but deliberate structural incentive alignment (snare). The civil rights coalition's perspective (tangled_rope) and the reform movement's perspective (scaffold) are both valid but represent marginal corrections to a fundamentally extractive mechanism. The mandatrophy is resolved by acknowledging that multiple perspectives are legitimate but the analytical observer's task is to identify the primary extraction mechanism and the incentive asymmetry that sustains it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tear_gas_causal_efficacy_riot_generation,
    'Does tear gas deployment causally generate riot behavior in initially nonviolent crowds, or does it disperse preexisting latent violence?',
    'Longitudinal video analysis of protest dynamics before/after tear gas deployment; crowd movement modeling; medical records correlating tear gas exposure with injury patterns consistent with self-defense vs coordination',
    'If tear gas generates: the loop is functionally extractive (Snare). If tear gas disperses latent violence: the apparatus''s rope perspective is valid and extraction is secondary. This determines whether the constraint is primarily a coordination failure or a deliberate repression mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tear_gas_causal_efficacy_riot_generation, empirical, 'Whether tear gas deployment generates riot behavior in nonviolent crowds').

omega_variable(
    state_apparatus_riot_incentive_threshold,
    'What level of public disorder does the state security apparatus require to justify tear gas deployment budgets and tactical expansion?',
    'Historical analysis of deployment frequency vs. public disorder metrics; budget cycle correlation with high-profile riot events; post-deployment tactical/equipment expansion patterns',
    'If apparatus requires riots to justify budgets: the loop is deliberately structurally extractive. If apparatus would deploy regardless: tear gas is independent policy, not contingent on riot generation. Determines whether the snare is deliberate or emergent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_apparatus_riot_incentive_threshold, empirical, 'Whether state apparatus requires public disorder to justify tear gas budgets').

omega_variable(
    chemical_incapacitation_vs_tactical_theater,
    'Is tear gas deployment primarily motivated by actual crowd management efficacy, or by demonstrable state action and visual control?',
    'Comparison of tear gas use rates in monitored vs. unmonitored protests; frequency of deployment in small-scale or geographically isolated protests; expert analysis of alternative crowd management techniques used in same jurisdiction',
    'If motivated by efficacy: the apparatus is solving a real coordination problem (rope perspective). If motivated by theater: the extraction is primary and coordination is secondary (snare perspective). Determines piton vs. rope classification prominence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chemical_incapacitation_vs_tactical_theater, empirical, 'Whether tear gas deployment is motivated by efficacy or by demonstrable state action').

omega_variable(
    legislative_restriction_enforcement_gap,
    'When tear gas restrictions are enacted, do police agencies comply or find technical/legal workarounds to maintain deployment capacity?',
    'Tracking of police agency compliance with chemical weapons restrictions; analysis of new tactics (riot gear procurement, ''pepper balls'', kinetic impact munitions) as substitutes; legal challenges to restrictions',
    'If compliance: the scaffold sunset is real and the constraint is declining. If workarounds: the restriction is theatrical and the snare persists. Determines whether reform perspective (scaffold) or institutional persistence perspective (piton) accurately describes the constraint''s trajectory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legislative_restriction_enforcement_gap, empirical, 'Whether tear gas restrictions are enforced or enable police agency workarounds').

omega_variable(
    protest_participant_behavioral_shift_tear_gas_exposure,
    'Do individuals who experience tear gas in one protest become more aggressive in subsequent protests, more defensive and prepared, or more risk-averse and withdrawn?',
    'Longitudinal tracking of protest participant behavior across multiple events; psychological assessments of trauma and behavioral adaptation; analysis of counter-tactics adoption (gas masks, dispersal protocols) vs. escalation',
    'If more aggressive: the loop creates a reinforcing escalation (snare feedback). If more defensive/prepared: participants are adapting and the loop''s extractive power is declining. If more withdrawn: the constraint suppresses dissent (higher suppression metric but different mechanism). Determines whether the loop is self-reinforcing or stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protest_participant_behavioral_shift_tear_gas_exposure, empirical, 'How tear gas exposure affects protest participant behavior in subsequent events').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tear_gas_repression_2026, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tgr_tr_t0, tear_gas_repression_2026, theater_ratio, 0, 0.52).
narrative_ontology:measurement(tgr_tr_t10, tear_gas_repression_2026, theater_ratio, 10, 0.6).
narrative_ontology:measurement(tgr_tr_t20, tear_gas_repression_2026, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(tgr_be_t0, tear_gas_repression_2026, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(tgr_be_t10, tear_gas_repression_2026, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(tgr_be_t20, tear_gas_repression_2026, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tear_gas_repression_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(tear_gas_repression_2026, police_union_qualified_immunity).
narrative_ontology:affects_constraint(tear_gas_repression_2026, protest_risk_premium_political_participation).
narrative_ontology:affects_constraint(tear_gas_repression_2026, riot_gear_procurement_militarization).

% DUAL FORMULATION NOTE:
% The tear gas loop decomposes into mechanistic and institutional constraint families. The mechanistic constraint (tear gas chemical properties → panic response → riot behavior) has ε ≈ 0.35 and is closer to tangled rope (coordination with side effects). The institutional constraint (apparatus incentive alignment + deployment doctrines + legal cover) has ε ≈ 0.68 and is snare. This story addresses the institutional constraint. The mechanistic constraint is upstream; apparatus policy choices determine whether the chemical mechanism is deployed in ways that generate or prevent riots.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tear_gas_repression_2026, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
