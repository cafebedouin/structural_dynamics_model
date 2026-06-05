% ============================================================================
% CONSTRAINT STORY: regional_autonomy_suspension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regional_autonomy_suspension, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: regional_autonomy_suspension
 *   human_readable: Regional Autonomy Suspension
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   Regional autonomy suspension occurs when a central government
 *   unilaterally removes the governing authority and electoral processes of a
 *   subnational region, typically justified by emergency conditions
 *   (separatism, terrorism, governance failure) but often extended beyond the
 *   stated crisis period. This constraint exemplifies the boundary between
 *   legitimate emergency coordination and permanent extraction of political
 *   power. A suspension may begin as genuine crisis response (Rope or
 *   Scaffold perspective) but degrade over time into an extraction mechanism
 *   (Snare perspective) as the stated emergency justification becomes
 *   performative while the power consolidation persists. The increasing
 *   theater_ratio (0.28 → 0.55 over the interval) indicates that the
 *   suspension's justifying rationale becomes increasingly theatrical
 *   relative to its actual governance function, while extractiveness
 *   increases as the mechanism shifts from crisis-response to rent
 *   extraction. Regional populations face direct coercion (military/police
 *   presence), loss of electoral voice, and inability to exit the
 *   jurisdiction without abandoning property and identity. Central government
 *   benefits from consolidated authority, reduced need to negotiate with
 *   elected regional leaders, and ability to redirect resources without
 *   regional scrutiny.
 *
 * KEY AGENTS:
 *   - Regional Population: Primary victim (powerless/trapped) — cannot exit jurisdiction; bears direct coercion and loss of political voice
 *   - Regional Elected Officials: Secondary victim (moderate/constrained) — career disrupted; subject to potential prosecution; constrained by uncertainty about restoration timing
 *   - Central Government Authority: Primary beneficiary (institutional/arbitrage) — consolidates power; eliminates need for negotiation; unilaterally determines policy
 *   - Constitutional Oversight Bodies: Organized actors (organized/constrained) — courts, human rights monitors, international observers; have structural mandate but limited enforcement capacity during suspension
 *   - Emergency Administration Officials: Tertiary beneficiary (powerful/arbitrage) — appointed administrators gain discretionary authority; no electoral accountability
 *   - Analytical Observer: Detached perspective (analytical/analytical) — risks naturalizing suspension as inherent state capacity rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regional_autonomy_suspension, 0.68).
domain_priors:suppression_score(regional_autonomy_suspension, 0.72).
domain_priors:theater_ratio(regional_autonomy_suspension, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regional_autonomy_suspension, extractiveness, 0.68).
narrative_ontology:constraint_metric(regional_autonomy_suspension, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(regional_autonomy_suspension, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regional_autonomy_suspension, snare).
narrative_ontology:human_readable(regional_autonomy_suspension, "Regional Autonomy Suspension").
narrative_ontology:topic_domain(regional_autonomy_suspension, "political/constitutional").

domain_priors:requires_active_enforcement(regional_autonomy_suspension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regional_autonomy_suspension, central_government).
narrative_ontology:constraint_victim(regional_autonomy_suspension, regional_population).
narrative_ontology:constraint_victim(regional_autonomy_suspension, regional_elected_officials).
narrative_ontology:constraint_victim(regional_autonomy_suspension, local_institutional_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUSPENDED REGIONAL POPULATION (SNARE) — Cannot exit the jurisdiction without abandoning property, social ties, and cultural identity. Faces direct coercion through military/police presence and loss of electoral voice. No meaningful alternatives to central authority's unilateral decisions. Maximum extraction — population bears full cost of suspension with no countervailing coordination benefit.
constraint_indexing:constraint_classification(regional_autonomy_suspension, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: REGIONAL POLITICAL ELITES (TANGLED ROPE) — Constrained by electoral cycle restoration uncertainty and career dependence on central government approval. Benefit from selective resource transfers and administrative coordination during suspension. Mixed extraction and coordination — some genuine institutional function (emergency administration) alongside asymmetric power consolidation.
constraint_indexing:constraint_classification(regional_autonomy_suspension, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CENTRAL GOVERNMENT AUTHORITY (ROPE) — Experiences suspension as pure coordination mechanism: consolidated emergency authority enables rapid crisis response and unified policy implementation. Net beneficiary with arbitrage option (can lift suspension unilaterally). Sees constraint as legitimate institutional function with minimal coercive overhead from their position.
constraint_indexing:constraint_classification(regional_autonomy_suspension, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL OVERSIGHT BODIES (SCAFFOLD) — Organized actors (courts, international observers, human rights monitors) see suspension as temporary emergency measure with explicit sunset clause in constitutional frameworks. Constrained by the political pressure to defer during crisis, but with institutional mandate for eventual restoration. Lower effective extraction because oversight has structural power and normative exit path, even if politically difficult to exercise.
constraint_indexing:constraint_classification(regional_autonomy_suspension, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: EMERGENCY POWERS ARCHITECTURE (PITON) — The constitutional apparatus for suspending autonomy persists through institutional inertia and performative use of emergency justifications. What began as a genuine coordination mechanism for acute crises has become a degraded tool used for chronic governance problems. Theater ratio high (0.55+) because suspension is maintained through emergency declarations that may exceed the factual crisis scope. The power to suspend accrues more symbolic than functional value — the architecture remains because alternatives haven't fully displaced it, not because it solves current problems.
constraint_indexing:constraint_classification(regional_autonomy_suspension, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, state sovereignty and the capacity to suspend subnational autonomy during existential crisis may appear as an immutable structural feature of state organization itself. This perspective risks naturalizing what is a contingent institutional arrangement — treating the power to suspend autonomy as inherent to statehood rather than a specific constitutional design choice. The engine's false summit detector will flag this as naturalization of institutional contingency.
constraint_indexing:constraint_classification(regional_autonomy_suspension, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regional_autonomy_suspension_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regional_autonomy_suspension, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regional_autonomy_suspension, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regional_autonomy_suspension, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regional_autonomy_suspension, TR),
    TR >= 0.70.

:- end_tests(regional_autonomy_suspension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, increasing over time. The suspension begins at 0.42 (justifiable as crisis response with genuine coordination function) and increases to 0.68 (predominantly extractive mechanism). The increase reflects the degradation of crisis-response rationale into permanent power consolidation. Victim population bears full cost of suspension with no offsetting coordination benefit. Suppression (0.72): High. Multiple barriers prevent exit: geographic/cultural/property attachment (trapped), legal prohibition on political organization (coercion), military presence (direct force), loss of electoral channels (institutional closure), and lack of international intervention capacity. Suppression is both structural and potentially internalized — regional identity may prevent psychological exit even where material exit becomes technically possible. Theater ratio (0.55): Moderate-high, increasing. Early in suspension, emergency justification has credibility (real crisis framing). Over time, the justifying conditions diminish while suspension persists, creating theater — the emergency powers apparatus is maintained through performative emergency declarations rather than genuine crisis response. The central government maintains the suspension not because it solves current problems but because it has accrued to institutional use and dislodging it would require political capital.
 *
 * PERSPECTIVAL GAP:
 *   This constraint reveals a radical perspectival divide between institutional positions. The central government's institutional perspective sees the suspension as solving a genuine coordination problem: unified authority, rapid crisis response, elimination of veto points. They genuinely believe in the emergency framing and experience the constraint as coordination. The regional population sees the opposite — their freedoms suspended, their government removed, their alternatives closed. Constitutional oversight bodies see a temporary mechanism with sunset language in constitutional frameworks, but they lack enforcement power during the suspension itself. Regional political elites see a mixed situation: their formal authority is suspended, but some may benefit from selection into central government's appointed structure. The historical pattern is that this perspectival gap closes only when suspension is lifted — the central government's 'temporary' measure becomes permanent not through explicit decision but through gradual normalization and institutional inertia, precisely the degradation the Piton perspective captures.
 *
 * DIRECTIONALITY LOGIC:
 *   Central government derives d ≈ 0.05–0.15 (strong beneficiary, arbitrage exit) → negative f(d) → negative χ, experiencing the constraint as coordination/benefit. Regional population derives d ≈ 0.92–0.98 (full target, trapped exit) → high f(d) ≈ 1.38–1.42 → high χ, experiencing maximum extraction. Regional elites occupy intermediate position: d ≈ 0.65–0.75 (victim status due to electoral disruption, but constrained rather than trapped exit; possibility of restoration creates residual exit option) → f(d) ≈ 1.05–1.20 → moderate-high χ. The perspectival gap is massive: the central government sees coordination; the population sees extraction. Regional elites occupy a mixed position where they are partly victims (career disruption) and partly beneficiaries (if they cooperate with central authority, they may retain local power through appointment rather than election).
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE classification resolves the mandatrophy by establishing that this constraint is not legitimate coordination (which would be Rope) but extraction disguised as emergency response. The high suppression (0.72) and increasing extractiveness (0.42 → 0.68) confirm the Snare classification: the central government is not solving a collective action problem in which all parties benefit, but rather consolidating power asymmetrically with the regional population bearing the full cost. The increasing theater_ratio (0.28 → 0.55) confirms the degradation mechanism: the emergency justification becomes increasingly performative as the actual crisis conditions diminish. If the suspension were genuinely Rope (pure coordination), extractiveness would decline as the crisis resolved and the suspension was lifted. If it were Scaffold (temporary with sunset), theater_ratio would decline as restoration mechanisms became operationalized. Instead, both metrics move toward Snare territory: extractiveness rising and theater increasing, indicating that the institutional mechanism is shifting from crisis response to rent extraction. The mandatrophy is resolved by recognizing that early suspension may have had legitimate Rope-like coordination function, but the structural analysis must be based on the current state (0.68 extractiveness, 0.72 suppression), which classifies unambiguously as Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    emergency_threshold_ambiguity,
    'What constitutes an emergency sufficient to justify autonomy suspension? When does the justifying crisis end?',
    'Comparative constitutional analysis of suspension triggers; historical examination of how long suspensions persist after stated crisis resolution',
    'If threshold is narrow and enforced: suspension remains temporary (Scaffold). If threshold is broad or unenforced: suspension becomes permanent extraction tool (Snare). Current ambiguity allows central authority to determine when suspension ends.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(emergency_threshold_ambiguity, conceptual, 'Constitutional definition and enforcement of emergency thresholds').

omega_variable(
    coercion_vs_coordination_mechanism,
    'Does the suspension primarily function as crisis coordination (legitimate emergency authority) or as institutional extraction (consolidating central power)?',
    'Policy outcome analysis: Do suspension powers produce crisis-responsive outcomes (food distribution, disaster relief, disease control) or rent extraction outcomes (corruption increases, opposition persecution, resource seizure)? Comparison with non-suspension crisis management in peer states.',
    'If coordination dominant: lower χ, Rope or Scaffold. If extraction dominant: higher χ, Snare or Tangled Rope. Current high theater_ratio (0.55) suggests extraction mechanism disguised as coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_coordination_mechanism, empirical, 'Whether suspension mechanism functions as coordination or extraction').

omega_variable(
    restoration_probability,
    'What is the actual probability that suspended autonomy will be restored? Do constitutional promises of restoration have credible enforcement?',
    'Historical data on similar suspensions globally; analysis of judicial enforcement capacity; examination of whether central government has incentive to maintain suspension indefinitely',
    'If restoration probability > 0.70: agents perceive exit path (Scaffold or Tangled Rope). If probability < 0.30: agents perceive permanent entrapment (Snare or Mountain). Current uncertainty allows both beneficiaries and victims to maintain incompatible beliefs about permanence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_probability, empirical, 'Actual probability and credibility of autonomy restoration').

omega_variable(
    identity_locked_vs_trapped,
    'For regional population, is the binding mechanism material (trapped: cannot physically exit jurisdiction) or cognitive (identity_locked: cannot psychologically exit regional identity)? Or both?',
    'Post-suspension migration analysis: if populations leave at high rates, binding was material (trapped). If populations remain despite material mobility, binding includes identity fusion (identity_locked). Language/cultural/historical attachment cannot be externally measured — relies on self-report and ethnographic analysis.',
    'If trapped: exit_options properly classified as trapped. If identity_locked: exit_options should be reclassified as identity_locked, changing baseline classification and requiring commentary on cognitive capture. If both: suppression is both structural and internalized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_vs_trapped, empirical, 'Whether regional binding is material or cognitive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regional_autonomy_suspension, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(regi_tr_t0, regional_autonomy_suspension, theater_ratio, 0, 0.28).
narrative_ontology:measurement(regi_tr_t6, regional_autonomy_suspension, theater_ratio, 6, 0.42).
narrative_ontology:measurement(regi_tr_t12, regional_autonomy_suspension, theater_ratio, 12, 0.55).

% Extraction over time
narrative_ontology:measurement(regi_be_t0, regional_autonomy_suspension, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(regi_be_t6, regional_autonomy_suspension, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(regi_be_t12, regional_autonomy_suspension, base_extractiveness, 12, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regional_autonomy_suspension, enforcement_mechanism).
narrative_ontology:affects_constraint(regional_autonomy_suspension, constitutional_democracy_enforcement).
narrative_ontology:affects_constraint(regional_autonomy_suspension, regional_separatism_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
