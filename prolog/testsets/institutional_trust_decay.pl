% ============================================================================
% CONSTRAINT STORY: institutional_trust_decay
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_trust_decay, []).

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
 *   constraint_id: institutional_trust_decay
 *   human_readable: The Legitimacy Void: Institutional Trust Decay
 *   domain: social/political/institutional
 *
 * SUMMARY:
 *   Institutional trust decay manifests as the evaporation of shared belief
 *   in the competence and integrity of public institutions. This constraint
 *   exhibits properties of a pure extraction mechanism (snare) from the
 *   perspective of those dependent on institutional services, combined with
 *   theatrical degradation (piton) as formal structures persist despite
 *   functional collapse. The legitimacy void operates across multiple
 *   institutional domains simultaneously — healthcare, law enforcement,
 *   education, governance — creating a systemic condition where citizens
 *   remain legally and economically dependent on institutions they no longer
 *   trust. The constraint is driven by both active extraction (elite capture,
 *   regulatory abuse, corruption) and passive institutional failure
 *   (incompetence, obsolescence, underfunding). The theater ratio (0.81)
 *   reflects that much institutional activity has become performative:
 *   compliance rituals substitute for competent service delivery, strategic
 *   communications displace operational transparency, and
 *   legitimacy-restoration theater (commissions, reforms, accountability
 *   statements) cycles without restoring actual trustworthiness. The primary
 *   extractive mechanism operates through suppression: citizens cannot
 *   meaningfully exit dependency on institutions (taxation, law enforcement,
 *   essential services) even as they lose confidence in institutional
 *   integrity. The constraint traps vulnerable populations
 *   (service-dependent, legally enmeshed, economically precarious) while
 *   allowing elite exit through private alternatives and jurisdictional
 *   arbitrage.
 *
 * KEY AGENTS:
 *   - Disenfranchised Citizens: Primary victims (powerless/trapped) — cannot exit institutional dependence; bear full cost of legitimacy collapse
 *   - Service-Dependent Populations: Primary victims (moderate/constrained) — healthcare, welfare, housing reliance creates life-or-death institutional dependence; suppression takes form of bureaucratic friction and under-resourcing
 *   - Institutional Elite: Primary beneficiaries (institutional/arbitrage) — retain high-status positions during transition; have exit options and alternative support networks
 *   - Institutional Reformers: Secondary actors (organized/mobile) — see both coordination and extraction functions; navigate both pathways
 *   - Institutional Bureaucracy: Inertial actor (institutional/constrained) — maintains functional appearance through theater despite competence erosion
 *   - Alternative Institutional Movement: Emerging actors (organized/mobile) — building decentralized legitimacy structures; represent potential sunset mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_trust_decay, 0.58).
domain_priors:suppression_score(institutional_trust_decay, 0.68).
domain_priors:theater_ratio(institutional_trust_decay, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_trust_decay, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_trust_decay, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(institutional_trust_decay, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_trust_decay, snare).
narrative_ontology:human_readable(institutional_trust_decay, "The Legitimacy Void: Institutional Trust Decay").
narrative_ontology:topic_domain(institutional_trust_decay, "social/political/institutional").

domain_priors:requires_active_enforcement(institutional_trust_decay).
% --- Structural relationships ---
narrative_ontology:constraint_victim(institutional_trust_decay, institutional_functioning).
narrative_ontology:constraint_victim(institutional_trust_decay, civic_participation).
narrative_ontology:constraint_victim(institutional_trust_decay, collective_action_capacity).
narrative_ontology:constraint_victim(institutional_trust_decay, vulnerable_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE DISENFRANCHISED CITIZEN (SNARE) — Trapped within institutional dependency (taxation, regulation, legal recourse) but has lost confidence in institutional integrity. No meaningful exit: must navigate institutions even while believing them compromised. Bears full cost of institutional degradation (inadequate public services, unresolved grievances, abandoned communities) while suppression prevents exit or alternative coordination.
constraint_indexing:constraint_classification(institutional_trust_decay, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE SERVICE-DEPENDENT POPULATION (SNARE) — Healthcare-dependent, welfare-reliant, or housing-insecure populations experience institutional decay as existential risk. Constrained exit: private alternatives exist but are financially inaccessible. Suppression takes the form of stigma, bureaucratic friction, and systematic under-resourcing. High experienced extraction — survival depends on institutions they no longer trust.
constraint_indexing:constraint_classification(institutional_trust_decay, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE INSTITUTIONAL REFORMERS (TANGLED ROPE) — Professional advocates, oversight bodies, audit agencies, and reform movements see genuine coordination function (audit trails, transparency mechanisms, performance standards) that simultaneously creates extraction (bureaucratic expansion, control mechanisms, surveillance infrastructure). Organized agents have exit options (move to reform-friendly jurisdictions, shift focus) but are also dependent on institutional pathways for legitimacy. Mixed classification reflects both real coordination benefits and parasitic extraction.
constraint_indexing:constraint_classification(institutional_trust_decay, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: THE INSTITUTIONAL ELITE (ROPE) — High-status actors (executives, politicians, credentialed professionals) experience trust decay as coordination problem: without shared institutional belief, elite status becomes uncertain. But they have arbitrage options (private security, exit to sympathetic jurisdictions, financial independence). See legitimacy as a coordination good they collectively produce. Extraction runs away from them — they are net beneficiaries of institutional order even as it decays.
constraint_indexing:constraint_classification(institutional_trust_decay, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: THE INSTITUTIONAL BUREAUCRACY (PITON) — The administrative apparatus persists through inertia and lack of alternatives, not functional capacity. Theater ratio is exceptionally high: compliance rituals, performance metrics, audits, and strategic communications dominate actual institutional function. Bureaucratic self-preservation mechanisms (union rules, tenure, regulatory capture of oversight) maintain structures even as public confidence erodes. Theater prevents collapse but does not restore function — classic degraded constraint.
constraint_indexing:constraint_classification(institutional_trust_decay, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: THE ALTERNATIVE INSTITUTIONAL MOVEMENT (SCAFFOLD) — Community organizations, mutual aid networks, decentralized governance experiments, and local trust-building initiatives represent a temporary coordination structure designed to sunset traditional institutional dependence. Low effective extraction because agents have genuine exit options and see functional alternatives emerging. Suppression is present (legal barriers, resource constraints, regulatory obstacles) but diminishing as norms shift toward decentralization. Sunset mechanism: as alternative institutions mature (10-30 year horizon), traditional institutional dependence decreases.
constraint_indexing:constraint_classification(institutional_trust_decay, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: THE ANALYTICAL OBSERVER / NATURAL LAW (FALSE SUMMIT) — From a civilizational perspective, one might argue that institutional collapse is inevitable (Schumpeter, Tainter, Weingast). Complex societies inherently generate legitimacy problems as the gap between institutional performance and public expectation widows. This perspective risks naturalizing what is actually a contingent social choice (transparency norms, governance structures, legitimacy-building investments) as an inescapable law. The false summit detection: institutional trust is not a law of nature but a managed public good.
constraint_indexing:constraint_classification(institutional_trust_decay, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_trust_decay_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_trust_decay, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_trust_decay, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_trust_decay, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_trust_decay, TR),
    TR >= 0.70.

:- end_tests(institutional_trust_decay_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High but not maximal. The constraint is not a pure rent-extraction mechanism (which would require χ ≥ 0.66 and ε ≥ 0.46). Instead, extractiveness flows from systematic under-provision of public goods (suppression prevents exit and alternatives), combined with active capture of institutional mechanisms by elite interests. The gap between obligations (taxation, compliance, legal exposure) and services (declining quality, declining accessibility, increasing unresponsiveness) creates the extraction. Theater ratio (0.81): Exceptionally high, indicating that institutional activity is largely performative. Compliance audits, reform announcements, strategic communications, and legitimacy theater dominate actual service delivery. This suggests a piton is also present (degraded institutional function maintained by inertia). Suppression (0.68): High. Citizens cannot exit institutional dependence; private alternatives are financially inaccessible for most; legal mechanisms for accountability are controlled by the same institutions being distrusted; cultural suppression (shame, blame-the-victim narratives) discourages collective action.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximized. For dependent populations, the constraint is a snare: trapped, extractive, suppressive. For institutional elites, it is a rope: a coordination problem with shared interests in legitimacy maintenance. For reformers, it is a tangled_rope: genuine oversight mechanisms coupled with bureaucratic expansion. For the institutional apparatus itself, it is a piton: theater maintains the appearance of function despite actual collapse. For alternative institutional movements, it is a scaffold: a temporary coordinating structure designed to sunset traditional dependence. For the analytical observer, the temptation to classify as mountain (institutional collapse is inevitable) represents a false summit — a naturalization of contingent social choices. The gap is not an observational ambiguity but a structural reality: institutional trust decay creates entirely different constraints for different actor positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) flows from structural position relative to institutional dependence and institutional capture. Service-dependent populations have d ≈ 0.92 (full targets): they cannot exit, bear full suppression, receive diminishing services. Disenfranchised citizens have d ≈ 0.88 (near-full targets): trapped by legal/economic dependence. Institutional elites have d ≈ 0.08 (near-full beneficiaries): arbitrage options, private alternatives, status preservation through institutional titles. Reformers have d ≈ 0.50 (symmetric): genuine accountability functions but also depend on institutional pathways for legitimacy. The sigmoid f(d) converts these into experienced extraction multipliers: powerless agents see χ amplified by 1.42×; elites see χ dampened by -0.12× (negative extraction). Scope (national) applies σ = 1.0, neutral modifier. The result: suppression is uniform (0.68) across all perspectives, but experienced extractiveness varies dramatically by structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: Institutional trust decay avoids the coordinate/extract misclassification through the following resolution: (1) The snare classification is the primary structure — institutions function as extraction mechanisms (suppression of exit, coercion of compliance, unequal service provision). (2) The rope and scaffold perspectives represent real coordination functions (shared interest in legitimacy, functioning oversight) but these are secondary features layered onto the primary extractive structure. (3) The piton perspective captures the theater-driven degradation: institutional function is maintained through ritual and appearance, not competence. (4) The mountain perspective is identified as a FALSE SUMMIT — institutional collapse is not inevitable but contingent on policy choices regarding transparency, accountability, and legitimacy investment. The mandatrophy is resolved by clarifying that what appears to be a pure coordination problem (institutional legitimacy) is actually a snare with secondary coordination features. The snare dominates from the perspective of powerless agents; the coordination functions are real but subsidiary. This prevents the misclassification of an extraction mechanism as 'just coordination gone wrong.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cascading_vs_distributed_collapse,
    'Does institutional trust decay propagate as a cascading collapse (one sector''s failure triggering others) or as distributed atrophy (each sector degrading independently)?',
    'Network analysis of institutional failure correlations; temporal tracking of trust loss across sectors (healthcare, law enforcement, education, finance); identification of contagion pathways',
    'If cascading: snare classification strengthens (single system failure traps all dependents). If distributed: multiple snares with different exit architectures — some may degrade to pitons while others retain rope characteristics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cascading_vs_distributed_collapse, empirical, 'Whether institutional trust collapses cascade or degrade independently').

omega_variable(
    legitimacy_recovery_threshold,
    'Is there a threshold of trust loss below which institutional recovery becomes impossible without structural replacement?',
    'Historical case analysis (institutional recovery post-scandal, post-corruption revelation); measurement of trust regeneration trajectories across democracies and autocracies; identification of inflection points',
    'If threshold is real and currently exceeded: snare classification is permanent (no escape path). If threshold is far higher than current decay: scaffold/rope perspectives are viable (recovery possible). If no threshold exists: classification is observational (depends on whether recovery efforts are invested).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_recovery_threshold, conceptual, 'Whether institutional trust loss is reversible below some threshold').

omega_variable(
    alternative_legitimacy_viability,
    'Can decentralized, community-based, or networked legitimacy structures actually replace centralized institutional trust at scale?',
    'Comparative analysis of alternative governance structures (mutual aid networks, local assemblies, blockchain governance, cooperative platforms); tracking of scale limitations and failure modes; analysis of legitimacy maintenance without formal authority',
    'If viable: scaffold/alternative institutional perspectives are structural (not aspirational) — sunset mechanism is real. If limited: alternative institutions are pitons (theater-dependent) — traditional institutions remain the only functional path, strengthening snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_legitimacy_viability, conceptual, 'Whether alternative legitimacy structures can scale beyond local communities').

omega_variable(
    trust_extraction_distinction,
    'Is institutional trust decay best classified as a snare (extraction mechanism) or as a piton (theatrical degradation of formerly functional rope)?',
    'Causal analysis of trust loss: Is it driven by extractive behavior (resource capture, coercion, fraud) or by performance failure (incompetence, neglect, obsolescence)? Identification of whether agents are being actively harmed (snare) or passively abandoned (piton).',
    'If extractive: snare confirmed — the legitimacy void is a feature of the system, not a bug. If degradation: piton confirmed — institutional theater masks non-functionality. If mixed: tangled_rope confirmed — some sectors extractive, others merely degraded.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(trust_extraction_distinction, empirical, 'Whether trust decay results from extraction or from institutional degradation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_trust_decay, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(itdecay_tr_t0, institutional_trust_decay, theater_ratio, 0, 0.52).
narrative_ontology:measurement(itdecay_tr_t10, institutional_trust_decay, theater_ratio, 10, 0.68).
narrative_ontology:measurement(itdecay_tr_t20, institutional_trust_decay, theater_ratio, 20, 0.81).

% Extraction over time
narrative_ontology:measurement(itdecay_be_t0, institutional_trust_decay, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(itdecay_be_t10, institutional_trust_decay, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(itdecay_be_t20, institutional_trust_decay, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_trust_decay, enforcement_mechanism).
narrative_ontology:affects_constraint(institutional_trust_decay, regulatory_capture).
narrative_ontology:affects_constraint(institutional_trust_decay, political_polarization).
narrative_ontology:affects_constraint(institutional_trust_decay, elite_coordination).
narrative_ontology:affects_constraint(institutional_trust_decay, alternative_governance_emergence).

% DUAL FORMULATION NOTE:
% Institutional trust decay decomposes into multiple constraint stories representing different causal mechanisms: (1) regulatory_capture (ε ≈ 0.62, snare) — elite control of institutions drives extraction; (2) institutional_trust_decay (ε ≈ 0.58, snare) — systemic loss of faith in institutional legitimacy creates suppression; (3) alternative_governance_emergence (ε ≈ 0.28, scaffold) — decentralized structures building legitimate alternatives. The three stories are linked: regulatory capture causes institutional failure, which causes trust decay, which drives adoption of alternative institutions. Each has a distinct ε based on different measurement basis (capture measured by regulatory alignment with elite interests; decay measured by public confidence metrics; emergence measured by decentralized coordination capacity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_trust_decay, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
