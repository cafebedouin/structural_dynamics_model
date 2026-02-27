% ============================================================================
% CONSTRAINT STORY: reputational_cascade_failure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reputational_cascade_failure, []).

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
 *   constraint_id: reputational_cascade_failure
 *   human_readable: The Social Liquidity Trap
 *   domain: social/economic/informational
 *
 * SUMMARY:
 *   The reputational cascade failure constraint models a self-reinforcing
 *   loop where unverified but high-fitness negative information triggers
 *   coordinated social disinvestment from a target agent. The constraint
 *   combines genuine coordination (peers organizing to protect domain
 *   reputation) with asymmetric extraction (the target bears all cost while
 *   other agents gain value through alignment signaling). The theater ratio
 *   reflects the gap between public justifications for dissociation
 *   (protecting domain standards) and the actual mechanism (reputation
 *   laundering through distance). Early disseminators and narrative
 *   gatekeepers benefit from narrative control and engagement metrics; the
 *   target and the epistemic commons (which loses nuanced information and
 *   trust in verification) bear costs. The constraint's severity is not
 *   determined by information accuracy but by information fitness — emotional
 *   salience, cognitive consonance, and narrative elegance. Cascades form
 *   faster around well-crafted stories than true ones. This makes the
 *   mechanism partially exploitable: actors can deliberately craft
 *   high-fitness false narratives knowing that cascade velocity will outrun
 *   verification capacity. The epistemic resilience movement represents a
 *   scaffold intervention — distributed verification systems, community
 *   notes, and epistemic communities with reputational stakes can compete
 *   with cascade velocity if sufficiently resourced and architected. The
 *   cascade represents both a coordination problem (how do we prevent false
 *   information from destroying trust?) and an extraction opportunity (how
 *   can we eliminate a rival or secure reputation advantage by initiating a
 *   cascade?).
 *
 * KEY AGENTS:
 *   - Target Agent: Primary victim (powerless/trapped) — once cascade ignites, exit is impossible; reputation destroyed regardless of factual defense
 *   - Early Disseminators: Primary beneficiary (institutional/arbitrage) — gain credibility, narrative authority, first-mover advantage; can pivot to next target without cost
 *   - Narrative Gatekeepers: Secondary beneficiary (institutional/arbitrage) — media outlets, platform algorithms, institutional communications coordinate and amplify; benefit from engagement and narrative control
 *   - Credibility-Seeking Peers: Secondary victim and partial beneficiary (moderate/constrained) — face coordination problem but extract value through public dissociation; career safety via alignment
 *   - Epistemic Commons: Structural victim (powerless/trapped) — loses nuanced information, truth-tracking capacity, and epistemic trust; no agent represents its interests
 *   - Institutional Due Process: Performer (institutional/arbitrage) — maintains appearance of review but remediation timeline cannot compete with cascade; theater rather than function
 *   - Epistemic Resilience Movement: Organizer (organized/constrained) — building verification infrastructure and community notes; competing with cascade velocity; faces resource and adoption barriers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reputational_cascade_failure, 0.58).
domain_priors:suppression_score(reputational_cascade_failure, 0.68).
domain_priors:theater_ratio(reputational_cascade_failure, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reputational_cascade_failure, extractiveness, 0.58).
narrative_ontology:constraint_metric(reputational_cascade_failure, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(reputational_cascade_failure, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reputational_cascade_failure, tangled_rope).
narrative_ontology:human_readable(reputational_cascade_failure, "The Social Liquidity Trap").
narrative_ontology:topic_domain(reputational_cascade_failure, "social/economic/informational").

domain_priors:requires_active_enforcement(reputational_cascade_failure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reputational_cascade_failure, early_disseminators).
narrative_ontology:constraint_beneficiary(reputational_cascade_failure, narrative_gatekeepers).
narrative_ontology:constraint_victim(reputational_cascade_failure, target_agent).
narrative_ontology:constraint_victim(reputational_cascade_failure, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REPUTATIONALLY TARGETED AGENT (SNARE) — Once negative information circulates, the target has no viable exit. Attempts to counter the narrative amplify its spread. Social disinvestment accelerates regardless of factual accuracy. The target experiences maximum extraction: isolated, defunded, professionally eliminated. No coordination benefit, only suppression and cascade damage.
constraint_indexing:constraint_classification(reputational_cascade_failure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CREDIBILITY-SEEKING PEER GROUP (TANGLED ROPE) — Colleagues face a coordination problem (preventing damage to shared domain reputation) but also asymmetric extraction (their own reputations gain value by distancing from the target). They benefit from the cascade by demonstrating alignment with emerging consensus while coordinating a defensive posture for their domain. Constrained exit because abandoning peers or openly defending the target carries reputational cost.
constraint_indexing:constraint_classification(reputational_cascade_failure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NARRATIVE GATEKEEPER INSTITUTION (ROPE) — Media outlets, platform algorithms, and institutional statements coordinate audience attention around the narrative. They experience the cascade as a pure coordination mechanism: aggregating emerging consensus, directing discourse, maintaining relevance. Their exit options are unrestricted — they can pivot narratives, suppress or amplify stories, move on to the next target. Benefits from cascade through engagement metrics and narrative control.
constraint_indexing:constraint_classification(reputational_cascade_failure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL DUE PROCESS SYSTEM (PITON) — Formal dispute resolution, appeals, and legal remedies exist but function theatrically. Discovery lags cascade formation by months to years. By the time legal judgment arrives, social damage is permanent and irreversible. The system performs due diligence but its procedural timeline cannot compete with information cascade velocity. Theater ratio reflects the gap between the appearance of impartial review and its actual remedial capacity.
constraint_indexing:constraint_classification(reputational_cascade_failure, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: EPISTEMIC RESILIENCE MOVEMENT (SCAFFOLD) — Organized efforts to build information verification systems (fact-checking networks, community notes, epistemic communities with reputational stakes) create alternative pathways for cascade prevention. These systems have a sunset: as verification infrastructure matures, cascade formation becomes detectable and correctable before social disinvestment becomes irreversible. Constrained exit because the movement must work within existing institutional frameworks while building alternatives.
constraint_indexing:constraint_classification(reputational_cascade_failure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / INFORMATION FLOW LIMIT (MOUNTAIN) — From a civilizational view, some cascade risk is inherent to information systems: any distributed network with fast information flow and uncertain verification will exhibit cascade vulnerability. The speed of communication exceeds the speed of verification. This appears as a natural law of information dynamics. However, the structural data contradicts this classification — cascade severity depends on institutional choice variables (platform algorithms, verification investment, narrative concentration), not immutable limits. The engine will identify this as a false summit.
constraint_indexing:constraint_classification(reputational_cascade_failure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reputational_cascade_failure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(reputational_cascade_failure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reputational_cascade_failure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(reputational_cascade_failure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(reputational_cascade_failure, TR),
    TR >= 0.70.

:- end_tests(reputational_cascade_failure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting the substantial career and reputational costs borne by the target but tempering the initial assessment because some degree of verification delay is inherent to any information system. The increase from 0.32 to 0.58 over the 6-unit interval reflects cascade acceleration — early skepticism gives way to consensus as bandwagon effects dominate. Suppression (0.68): High. Multiple barriers prevent the target from countering: (a) information paradox — correcting false narratives amplifies them; (b) algorithmic amplification of novel/negative information; (c) institutional pressure on platforms to moderate without transparency; (d) publication bias against exculpatory information. Suppression is not total because some targets do mount successful defenses, typically through institutional backing or legal remedies that arrive too late to prevent social damage. Theater ratio (0.65): Moderate-high. The constraint operates through a combination of genuine coordination (protecting domain reputation from contamination) and theatrical performance (public dissociation as reputation management). Institutional statements about 'standards' and 'due diligence' mask the extraction mechanism — reputation laundering through distance. Theater increases over time as justifications become more elaborate and less tethered to specific facts.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap in this constraint is extreme. The target sees a snare — irreversible damage driven by information spread beyond their control. The peers see a tangled rope — a genuine coordination problem (preventing domain contamination) that also happens to extract value through alignment signaling. The gatekeepers see a rope — pure coordination of audience attention and narrative coherence. The due process system sees a piton — the maintenance of procedural rituals that no longer function in real time. The epistemic resilience movement sees a scaffold — a temporary problem with a sunset as verification infrastructure matures. The civilizational analyst risks seeing a mountain — cascade vulnerability as inherent to information flow. These are not disagreements about facts but about structural relationships to the constraint. The same information spread appears as an immutable trap to the target, a coordination tool to the peers, a narrative resource to gatekeepers, a performative ritual to institutions, and a solvable infrastructure problem to the resilience movement. The perspectival variance is a diagnostic feature: it reveals that the constraint is not about information quality (which would produce more convergent judgments) but about structural power asymmetries in determining whose version of the cascade (intentional extraction vs accidental coordination vs natural information flow) becomes canonical.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective is derived from the agent's structural relationship to the cascade mechanism. The target agent (powerless/trapped) experiences maximum extraction: d ≈ 0.95, f(d) ≈ 1.42. The epistemic commons (powerless/trapped, abstract actor with no voice) also derives high d despite not appearing in perspectives because it bears costs with no benefit mechanism. Early disseminators (institutional/arbitrage) experience low d ≈ 0.00 because they benefit (arbitrage exit means they can move on; institutional power means the cascade serves their narrative control). Credibility-seeking peers (moderate/constrained) derive mid-range d ≈ 0.55 because they face mixed costs (career risk of defending the target) and mixed benefits (reputation gain from alignment). Peers with exit to another domain or movement would have lower d; peers with tight coupling to shared domain reputation have higher d. The institutional due process system (institutional/arbitrage) derives low d despite appearance of neutrality because its function (remediation) is subordinate to cascade velocity — it experiences the constraint as coordinating other agents' actions but not controlling outcomes. The epistemic resilience movement (organized/constrained) derives mid-range d ≈ 0.40 because it must work within existing power structures to build alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY STRUCTURE: The reputational cascade exhibits the full mandatrophy trap. Is it a coordination problem (Rope/Tangled Rope) or pure extraction (Snare)? The answer is context-dependent in a way that cannot be resolved by adding measurement axes or observables. From the target's perspective, it is pure extraction: the cascade serves no coordination function, only damage. From the peers' perspective, it is genuinely coordinating a response to threat — preventing contamination of domain reputation. Both readings are structurally accurate; they are not contradictory perspectives on the same fact pattern but perspectives from different agent positions where the constraint has different functions. The mandatrophy resolution lies in declaring the constraint as Tangled Rope (hybrid coordination-extraction) and recognizing that the extractive component is not an abuse of the coordination mechanism but its inherent asymmetry. Early disseminators coordinate the response while extracting value through narrative control. Peers coordinate domain defense while extracting reputation gain through public dissociation. The constraint cannot be classified as pure Rope because the coordination benefits are not evenly distributed; they concentrate on those who initiate or amplify the cascade. The constraint cannot be classified as pure Snare because there is a genuine coordination function — preventing false information from permanently contaminating domain reputation is a legitimate collective action problem. The tension between these truths is the mandatrophy. Resolving it requires accepting that the same constraint operates as both coordination and extraction, that the two functions are entangled rather than separable, and that no observable-dependent measure can dissolve this tension. The constraint is Tangled Rope precisely because the mandatrophy cannot be escaped — it is structural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cascade_ignition_threshold,
    'What minimum social distribution of negative information triggers irreversible cascade formation?',
    'Empirical measurement of cascade velocity vs correction capacity across multiple domains; identification of critical mass thresholds by platform type and information fitness',
    'If threshold is low (< 0.5% initial adoption): cascades are nearly inevitable. If threshold is high (> 5% adoption): intervention windows remain open. Determines whether the constraint is snare (predetermined) or tangled rope (preventable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cascade_ignition_threshold, empirical, 'Minimum information distribution triggering irreversible cascade').

omega_variable(
    information_fitness_vs_accuracy,
    'Does narrative fitness (emotional salience, cognitive consonance, sharability) correlate with cascade severity independently of factual accuracy?',
    'Comparative analysis of true vs false negative information cascades; measure cascade velocity and amplitude against information fitness scores controlling for accuracy',
    'If fitness dominates accuracy: cascades are driven by memetic properties, not truth. Constraint is extraction mechanism (Snare/Tangled Rope). If accuracy is predictive: institutional and individual verification barriers are the constraint. Enables distinction between cascade-as-natural-consequence vs cascade-as-exploitable-mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_fitness_vs_accuracy, empirical, 'Relationship between narrative fitness and cascade severity independent of accuracy').

omega_variable(
    verification_infrastructure_sunset,
    'Can distributed epistemic communities (fact-checkers, verification networks, community notes) scale to compete with cascade velocity?',
    'Time-series analysis of correction lag vs cascade lag across platforms and claim types; measurement of collective action efficiency in epistemic communities',
    'If verification scales: Scaffold perspective is structural (sunset is real). If verification lags: cascade velocity is quasi-immutable and the constraint remains Snare/Tangled Rope. Determines trajectory toward epistemic resilience.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_infrastructure_sunset, empirical, 'Scalability of verification infrastructure to match cascade velocity').

omega_variable(
    institutional_reputation_vs_target_reputation,
    'Do peer institutions benefit net-positively or net-negatively from dissociation with a targeted agent?',
    'Longitudinal measurement of institutional reputation metrics (funding, partnerships, hiring, publication citation) before/after public dissociation; comparison with control institutions',
    'If dissociation is net-beneficial: tangled rope classification confirmed (active enforcement pays). If neutral or net-negative: cascade is driven purely by damage-prevention, not extraction. Determines beneficiary intentionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_reputation_vs_target_reputation, empirical, 'Net reputational benefit to institutions dissociating from cascade target').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reputational_cascade_failure, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reputational_tr_t0, reputational_cascade_failure, theater_ratio, 0, 0.4).
narrative_ontology:measurement(reputational_tr_t3, reputational_cascade_failure, theater_ratio, 3, 0.55).
narrative_ontology:measurement(reputational_tr_t6, reputational_cascade_failure, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(reputational_be_t0, reputational_cascade_failure, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(reputational_be_t3, reputational_cascade_failure, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(reputational_be_t6, reputational_cascade_failure, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reputational_cascade_failure, information_standard).
narrative_ontology:affects_constraint(reputational_cascade_failure, institutional_reputation_laundering).
narrative_ontology:affects_constraint(reputational_cascade_failure, algorithmic_amplification_bias).
narrative_ontology:affects_constraint(reputational_cascade_failure, verification_lag_asymmetry).

% DUAL FORMULATION NOTE:
% The reputational cascade failure decomposes into three related constraints with distinct epsilon values. (1) Institutional reputation laundering (ε ≈ 0.35) focuses on the peer coordination mechanism and how dissociation signals status. (2) Algorithmic amplification bias (ε ≈ 0.42) focuses on platform architecture favoring novel/negative information independent of accuracy. (3) Verification lag asymmetry (ε ≈ 0.28) focuses on the structural velocity gap between information spread and verification. The reputational cascade (ε = 0.58) is downstream of all three and emerges when all three align.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reputational_cascade_failure, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
