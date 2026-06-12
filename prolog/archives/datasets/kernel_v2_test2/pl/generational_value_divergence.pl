% ============================================================================
% CONSTRAINT STORY: generational_value_divergence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_generational_value_divergence, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: generational_value_divergence
 *   human_readable: Generational Value Divergence in Party Coalitions
 *   domain: political_science/public_opinion/democratic_theory
 *
 * SUMMARY:
 *   Generational value divergence within party coalitions describes the
 *   systematic age-based differences in policy preferences and ideological
 *   positioning that create temporal instability in coalition composition. In
 *   the Democratic coalition, 26% of young Democrats identify as Leftward
 *   Progressives compared to only 6% of older Democrats, while 25% of older
 *   Democrats are Loyal Liberals compared to 16% of young Democrats. These
 *   differences extend to policy preferences, particularly on cultural
 *   issues, immigration, and the scope of government intervention. The
 *   constraint coordinates the coexistence of distinct generational value
 *   profiles within a single coalition structure, enabling both cohorts to
 *   maintain their preferences while participating in collective political
 *   action. The divergence is not extraction in itself — it is the mechanism
 *   by which democratic systems update to reflect evolving public values as
 *   cohorts replace each other. However, the measurements show modest
 *   increases in extractiveness, theater, and suppression over the interval,
 *   suggesting potential drift toward more extractive dynamics if
 *   institutional gatekeeping by older cohorts blocks younger cohort policy
 *   priorities.
 *
 * KEY AGENTS:
 *   - Younger Cohort Members: Primary beneficiary (moderate/mobile) — the divergence creates organizational space for their distinct policy preferences; can exit to movement organizing or third parties if coalition becomes unresponsive
 *   - Older Cohort Members: Primary beneficiary (moderate/mobile) — the divergence allows them to maintain distinct identity and policy commitments within coalition rather than being forced to adopt younger preferences wholesale; can exit to breakaway factions if coalition shifts too rapidly
 *   - Party Coalition Leadership: Organized agents (organized/constrained) — manage coordination challenges created by divergence through platform construction, candidate selection, and generational succession planning; constrained exit because abandoning role risks organizational collapse
 *   - Democratic Responsiveness Mechanism: Institutional structure (institutional/constrained) — the divergence is the updating mechanism by which democratic systems reflect evolving values; scaffold logic because each cohort's divergence is temporary (they age into next bracket) but the updating function is permanent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(generational_value_divergence, 0.18).
domain_priors:suppression_score(generational_value_divergence, 0.22).
domain_priors:theater_ratio(generational_value_divergence, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(generational_value_divergence, extractiveness, 0.18).
narrative_ontology:constraint_metric(generational_value_divergence, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(generational_value_divergence, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(generational_value_divergence, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(generational_value_divergence, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(generational_value_divergence, rope).
narrative_ontology:human_readable(generational_value_divergence, "Generational Value Divergence in Party Coalitions").
narrative_ontology:topic_domain(generational_value_divergence, "political_science/public_opinion/democratic_theory").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(generational_value_divergence, younger_cohort_members).
narrative_ontology:constraint_beneficiary(generational_value_divergence, older_cohort_members).
narrative_ontology:constraint_beneficiary(generational_value_divergence, democratic_responsiveness_mechanism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Younger Democrats (under 50) with distinct policy preferences, particularly on cultural issues and scope of government intervention. Disproportionately identify as Leftward Progressives (26% vs 6% of older Democrats). Benefit from generational divergence because it creates organizational space for their preferences within coalition structure rather than forcing assimilation to older cohort values. Can exit to movement organizing, third parties, or independent political action if coalition becomes unresponsive. Face lower sunk costs in party infrastructure than older cohorts.
narrative_ontology:constraint_stakeholder(generational_value_divergence, younger_cohort_members, beneficiary,
    moderate, biographical, mobile, national).

% Older Democrats (50+) with distinct policy preferences reflecting different formative political experiences. Disproportionately identify as Loyal Liberals (25% vs 16% of younger Democrats). Benefit from generational divergence because it allows them to maintain their established policy commitments and coalition identity without being forced to adopt younger cohort preferences wholesale. Can exit to breakaway factions or switch parties if coalition shifts too rapidly toward younger preferences. Face higher sunk costs in party infrastructure and donor networks than younger cohorts, but exit remains feasible.
narrative_ontology:constraint_stakeholder(generational_value_divergence, older_cohort_members, beneficiary,
    moderate, biographical, mobile, national).

% Party officials, elected representatives, and organizational leaders managing coalition stability across generational divides. Set agenda through platform construction, candidate selection, resource allocation, and messaging strategy. Face genuine coordination challenges created by divergence: must balance competing generational priorities in platform, manage succession planning, and maintain big tent unity while accommodating distinct values. Constrained exit because abandoning coalition management role risks organizational collapse and loss of institutional position. Use standard coalition management techniques (issue prioritization, generational outreach, incremental platform evolution) to coordinate across divergence.
narrative_ontology:constraint_stakeholder(generational_value_divergence, party_coalition_leadership, agenda_setter,
    organized, generational, constrained, national).

% The institutional structure enabling generational value divergence to translate into policy change over time. Not a real actor but an abstract systemic function. Benefits from the divergence because it is the mechanism by which democratic systems update to reflect evolving public values as cohorts replace each other. Each cohort's specific divergence is temporary (they age into next bracket) but the updating function is permanent. Constrained exit because abandoning representative function would trigger democratic legitimacy crisis.
narrative_ontology:constraint_stakeholder(generational_value_divergence, democratic_responsiveness_mechanism, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_non_agent(generational_value_divergence, democratic_responsiveness_mechanism).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Generational value divergence coordinates the coexistence of distinct age-based policy preferences within a single coalition structure, enabling both younger and older cohorts to maintain their values while participating in collective political action. It solves the collective action problem of how to maintain coalition stability and electoral viability while accommodating value evolution across cohorts with different formative experiences.
% TRANSFER_FUNCTION: The divergence transfers organizational space and agenda-setting influence between cohorts over time as demographic replacement occurs. Younger cohorts gain increasing representation and policy influence as they grow as share of coalition; older cohorts maintain influence through institutional position and accumulated political capital. The transfer is bidirectional and generational rather than extractive: each cohort benefits during its period of coalition participation.
% ABSENT_VOICES: Cohorts not yet politically active (future generations whose values will diverge from current younger cohort) and cohorts that have aged out of active participation. These voices would object if the current generational divergence calcifies into permanent factional structure that prevents future value updating. They are absent because they are not yet (or no longer) in the coalition.
% DISAPPEARANCE_RATIONALE: If generational value divergence disappeared (all age cohorts held identical policy preferences), coalition management would simplify dramatically but democratic responsiveness would collapse. The system would lose its primary mechanism for updating to reflect evolving public values. Platform construction, candidate selection, and succession planning would become trivial coordination problems rather than genuine negotiation across distinct preferences. The coalition would become brittle and unresponsive to demographic and normative change, likely triggering realignment or third-party formation when suppressed generational differences eventually erupted.
% FOUNDING_PROBLEM: The founding problem is how democratic systems with age-structured populations and evolving social norms can maintain coalition stability while remaining responsive to changing public values across generations. Generational divergence emerged as the solution: rather than forcing value homogeneity (which would require suppressing natural cohort differences) or fragmenting into age-segregated parties (which would sacrifice coalition stability), parties accommodate divergence within big tent structures.
% FOUNDING_PROBLEM_CORROBORATION: Democratic theorists and political scientists studying party systems and coalition dynamics corroborate that generational value divergence serves an ongoing coordination function. The problem of maintaining coalition stability while accommodating value evolution remains live in all democratic systems with age-structured populations. Corroboration comes from outside beneficiaries: academic research on party systems, comparative politics literature on coalition management, and empirical studies of generational replacement effects on policy outcomes.
narrative_ontology:disappearance_verdict(generational_value_divergence, world_rearranges).
narrative_ontology:founding_problem_status(generational_value_divergence, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: YOUNGER COHORT MEMBERS (ROPE) — Experience generational divergence as coordination mechanism enabling authentic representation of evolving values. Mobile exit options (can switch parties, form new coalitions, or exit formal politics for movement organizing). Net beneficiaries: the divergence creates space for their distinct policy preferences to be articulated and organized within the coalition structure.
constraint_indexing:constraint_classification(generational_value_divergence, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: OLDER COHORT MEMBERS (ROPE) — Experience generational divergence as coordination mechanism preserving their established policy commitments while accommodating new voices. Mobile exit options (can switch parties or form breakaway factions). Net beneficiaries: the divergence allows them to maintain distinct identity within coalition rather than being forced to adopt younger cohort preferences wholesale.
constraint_indexing:constraint_classification(generational_value_divergence, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: PARTY COALITION LEADERSHIP (ROPE) — Organized agents managing coalition stability see generational divergence as coordination challenge with workable solutions. Constrained exit (cannot abandon coalition management role without organizational collapse). Experience as rope: the divergence creates genuine coordination problems (platform construction, candidate selection, messaging) but these are solvable through standard coalition-management techniques (big tent strategy, issue prioritization, generational succession planning).
constraint_indexing:constraint_classification(generational_value_divergence, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DEMOCRATIC RESPONSIVENESS MECHANISM (SCAFFOLD) — The institutional structure enabling value divergence to translate into policy change sees itself as transitional coordination. Constrained exit (cannot abandon representative function without democratic legitimacy crisis). Scaffold logic: generational divergence is the mechanism by which democratic systems update to reflect evolving public values; the divergence itself is temporary (each cohort ages into the next bracket) but the updating function is permanent. The constraint coordinates the transition from one value equilibrium to the next.
constraint_indexing:constraint_classification(generational_value_divergence, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE) — From civilizational scope, generational value divergence is a coordination mechanism inherent to democratic systems with age-structured populations and evolving social norms. Low extraction: no identifiable group systematically captures rents from the divergence itself (distinct from extraction within specific policy outcomes the divergence enables). Low suppression: alternatives exist (age-blind party structures, mandatory value homogeneity within coalitions) but are not suppressed; they are simply less adaptive to demographic and normative change. The divergence solves a genuine collective action problem: how to maintain coalition stability while accommodating value evolution across cohorts.
constraint_indexing:constraint_classification(generational_value_divergence, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(generational_value_divergence_tests).
:- end_tests(generational_value_divergence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The divergence itself does not systematically extract from either cohort — both benefit from the ability to maintain distinct preferences within a coalition structure. The modest extractiveness reflects coordination costs (platform negotiation, candidate selection disputes, messaging complexity) and potential for institutional gatekeeping by entrenched older cohorts, but these are not severe. The upward trajectory (0.12 → 0.22 projected) reflects increasing coordination costs as divergence widens and potential for gatekeeping to intensify. Suppression (0.22): Low. Alternatives to generational divergence exist (age-blind party structures, mandatory value homogeneity, generationally segregated parties) but are not actively suppressed — they are simply less adaptive to demographic and normative change. The modest suppression reflects institutional inertia and sunk costs in existing coalition structures. The upward trajectory (0.18 → 0.25 projected) reflects potential for institutional entrenchment to raise exit costs for younger cohorts. Theater ratio (0.15): Very low. Coalition management activities (platform construction, big tent messaging, generational outreach) are largely functional rather than performative. The modest theater reflects some performative unity signaling that papers over genuine divergence, but most coordination activity serves real coalition maintenance functions. The upward trajectory (0.10 → 0.18 projected) reflects potential for performative unity rhetoric to increase as divergence widens. Accessibility collapse (0.35): Moderate-low. Alternatives to generational divergence (age-blind structures, value homogeneity mandates) remain conceptually and practically accessible — many organizations and parties attempt them. The divergence does not collapse alternatives as completely as a natural law would. Resistance (0.28): Low-moderate. The constraint meets some resistance from actors who prefer age-blind structures or who experience coordination costs as excessive, but resistance is not severe. Most coalition members accept generational divergence as normal feature of democratic politics.
 *
 * PERSPECTIVAL GAP:
 *   All five perspectives classify as rope or scaffold, reflecting genuine consensus that generational value divergence is primarily a coordination mechanism rather than an extraction mechanism. The perspectival variation is in time horizon and exit options rather than in type classification. Younger and older cohorts both see rope at biographical time with mobile exit — they experience the divergence as enabling their distinct preferences. Party leadership sees rope at generational time with constrained exit — they experience coordination challenges but have tools to manage them. The democratic responsiveness mechanism sees scaffold at generational time — the divergence is transitional coordination enabling value updating. The analytical observer sees rope at civilizational time — the divergence is a permanent feature of democratic systems, not a temporary problem. The uniformity of rope/scaffold classification across perspectives is itself diagnostic: it suggests the constraint is genuinely low-extraction coordination rather than naturalized extraction. The omega variables identify the empirical tests that would reveal extraction if present: Does institutional gatekeeping by older cohorts systematically block younger cohort priorities? Is cohort replacement fast enough to prevent calcification? Are exit options truly symmetric or does institutional entrenchment create asymmetry?
 *
 * DIRECTIONALITY LOGIC:
 *   Both younger and older cohort members are declared beneficiaries because both gain from the divergence: younger cohorts get organizational space for their distinct preferences, older cohorts maintain their established commitments without forced adoption of new values. The democratic responsiveness mechanism is also a beneficiary because the divergence is the updating function that maintains system legitimacy. No victims are declared because the divergence itself does not systematically extract from any group — it is a coordination mechanism. The modest extractiveness (0.18) reflects coordination costs and potential for institutional gatekeeping, not extraction from the divergence itself. Both cohorts have mobile exit options (can switch parties, form new coalitions, exit to movement organizing) which produces low directionality values and low effective extraction. Party leadership has constrained exit (cannot abandon role without organizational collapse) but is organized, which moderates effective extraction. The analytical observer sees rope from civilizational scope: generational divergence is inherent to democratic systems with age-structured populations and evolving norms, and it solves a genuine collective action problem (maintaining coalition stability while accommodating value evolution).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates rope classification without mandatrophy risk because the coordination function is genuine and ongoing. Generational value divergence solves a real collective action problem: how to maintain coalition stability while accommodating value evolution across cohorts with different formative experiences and policy preferences. The divergence enables both cohorts to participate in collective political action without forcing either to abandon their distinct values. The scaffold perspective (democratic responsiveness mechanism) identifies the transitional element: each cohort's specific divergence is temporary (they age into the next bracket and are replaced by new cohorts with their own distinct values), but the updating function is permanent. The constraint is not a degraded coordination mechanism maintained through inertia (piton) — the coordination function remains active and necessary. The modest upward drift in extractiveness and suppression over the measurement interval suggests potential for future degradation if institutional gatekeeping intensifies, but current levels remain consistent with rope classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divergence_vs_polarization_boundary,
    'At what threshold does generational value divergence within coalitions transition from coordination (enabling representation of diverse preferences) to extraction (older cohorts blocking younger cohort policy priorities through institutional gatekeeping)?',
    'Longitudinal analysis of policy adoption rates for younger-cohort-preferred policies; measurement of age-based representation gaps in party leadership and candidate selection; tracking of younger cohort exit rates from formal party structures to movement organizing',
    'If divergence remains coordination: rope classification holds across perspectives. If divergence enables systematic blocking: reclassify to tangled_rope (coordination function present but asymmetric extraction via institutional control). If blocking is severe: some perspectives reclassify to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divergence_vs_polarization_boundary, empirical, 'Threshold distinguishing coordination from extraction in generational divergence').

omega_variable(
    cohort_replacement_rate_sufficiency,
    'Is the rate of cohort replacement (younger members entering, older members aging out) sufficient to prevent the divergence from calcifying into permanent factional conflict?',
    'Demographic projection of coalition composition over 20-year horizon; comparison of value convergence rates (do cohorts converge as they age?) vs divergence persistence rates (do cohorts maintain distinct values across lifespan?); analysis of historical coalition realignments triggered by generational turnover',
    'If replacement rate is sufficient and cohorts converge: scaffold perspective confirmed (transitional coordination). If replacement is slow and divergence persists: rope classification holds but temporal instability increases. If divergence calcifies: potential reclassification to tangled_rope as coordination costs rise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cohort_replacement_rate_sufficiency, empirical, 'Whether cohort replacement prevents calcification of generational divergence').

omega_variable(
    exit_option_asymmetry,
    'Do younger and older cohorts have symmetric exit options from party coalitions, or does institutional entrenchment create asymmetric exit costs?',
    'Comparison of exit costs: career politicians (older, institutionally embedded) vs movement organizers (younger, less institutionally embedded); analysis of third-party formation and movement-to-party pathways; measurement of sunk costs in party infrastructure and donor networks by age cohort',
    'If exit options are symmetric: rope classification holds for both cohorts. If exit is asymmetric (older cohorts face higher costs): younger cohorts may experience constraint as more extractive (constrained rather than mobile exit options), shifting their perspective toward tangled_rope. If asymmetry is severe: older cohorts may be identity_locked (professional identity fused with party institutional role).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_asymmetry, empirical, 'Whether exit options from party coalitions are symmetric across age cohorts').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(generational_value_divergence, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(genval_tr_t0, generational_value_divergence, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(genval_tr_t0, observed).
narrative_ontology:measurement(genval_tr_t3, generational_value_divergence, theater_ratio, 3, 0.12).
narrative_ontology:measurement_basis(genval_tr_t3, observed).
narrative_ontology:measurement(genval_tr_t6, generational_value_divergence, theater_ratio, 6, 0.15).
narrative_ontology:measurement_basis(genval_tr_t6, observed).
narrative_ontology:measurement(genval_tr_t10, generational_value_divergence, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(genval_tr_t10, projected).

% Extraction over time
narrative_ontology:measurement(genval_be_t0, generational_value_divergence, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(genval_be_t0, observed).
narrative_ontology:measurement(genval_be_t3, generational_value_divergence, base_extractiveness, 3, 0.15).
narrative_ontology:measurement_basis(genval_be_t3, observed).
narrative_ontology:measurement(genval_be_t6, generational_value_divergence, base_extractiveness, 6, 0.18).
narrative_ontology:measurement_basis(genval_be_t6, observed).
narrative_ontology:measurement(genval_be_t10, generational_value_divergence, base_extractiveness, 10, 0.22).
narrative_ontology:measurement_basis(genval_be_t10, projected).

% Suppression requirement over time
narrative_ontology:measurement(genval_su_t0, generational_value_divergence, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(genval_su_t0, observed).
narrative_ontology:measurement(genval_su_t3, generational_value_divergence, suppression_requirement, 3, 0.2).
narrative_ontology:measurement_basis(genval_su_t3, observed).
narrative_ontology:measurement(genval_su_t6, generational_value_divergence, suppression_requirement, 6, 0.22).
narrative_ontology:measurement_basis(genval_su_t6, observed).
narrative_ontology:measurement(genval_su_t10, generational_value_divergence, suppression_requirement, 10, 0.25).
narrative_ontology:measurement_basis(genval_su_t10, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(generational_value_divergence, identity_coordination).

% DUAL FORMULATION NOTE:
% Generational value divergence is downstream of intra_party_fragmentation (the upstream constraint describes the broader typology-based fragmentation of which generational divergence is one structural component). The divergence has its own extractiveness value (0.18) reflecting coordination costs and potential gatekeeping, distinct from the upstream fragmentation's extractiveness (which reflects the full cost of managing multiple typology groups across all dimensions, not just age).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
