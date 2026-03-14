% ============================================================================
% CONSTRAINT STORY: international_recognition_architecture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_international_recognition_architecture, []).

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
 *   constraint_id: international_recognition_architecture
 *   human_readable: International Recognition Architecture
 *   domain: political/institutional/international_relations
 *
 * SUMMARY:
 *   The international recognition architecture defines which entities are
 *   admitted as states in the international system and therefore can exercise
 *   sovereign rights (sign treaties, raise capital via bonds, maintain
 *   diplomatic corps, access international law mechanisms). This constraint
 *   exhibits deep ambiguity about whether it is a natural coordination
 *   requirement or a contingent institutional arrangement that extracts
 *   compliance and legitimacy from those excluded. The Westphalian
 *   state-recognition system (originating 1648, formalized post-WWII through
 *   the UN and Montevideo Convention) provides stability and clear membership
 *   rules, solving a genuine coordination problem: any multipolar system
 *   needs to know which entities have standing. But the same system excludes
 *   stateless peoples, indigenous nations, failed-state regions, and
 *   post-conflict territories, trapping them in subordinate status. The
 *   architecture has become increasingly performative as real recognition now
 *   happens through alternative channels (trade blocs, tech platforms, credit
 *   ratings, military alliances) while the formal UN system maintains the
 *   ritual without the function. The measurement trajectory shows
 *   extractiveness and theater increasing together (0.42 → 0.62 and 0.35 →
 *   0.62 over the 75-year interval), indicating that the system is slowly
 *   degrading into pure theater while the extraction of compliance persists.
 *
 * KEY AGENTS:
 *   - Unrecognized Polities: Primary victims (powerless/trapped) — denied diplomatic standing, cannot access credit markets, cannot sign treaties; excluded from the system entirely without recourse
 *   - Established Great Powers: Primary beneficiaries (institutional/arbitrage) — use recognition as a foreign policy tool to reward allies and isolate rivals; can recognize or delist entities based on strategic interest
 *   - Regional Neighbor States: Secondary actors (moderate/constrained) — benefit from predictability (knowing which entities have standing) but constrained by security dilemmas (cannot recognize breakaway regions without diplomatic isolation)
 *   - UN and Formal Recognition System: Institutional actor (institutional/mobile) — maintains the Montevideo criteria as the official recognition gate; now largely performative as real recognition happens through trade, credit, technology, and military cooperation
 *   - International Reform Coalition: Organized actors (organized/constrained) — NGOs, progressive states, academic institutions pushing toward polycentric recognition systems (indigenous sovereignty, environmental governance, non-state actors); building alternative pathways
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the Westphalian state-centric model as the only possible recognition architecture when multiple alternatives are viable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(international_recognition_architecture, 0.58).
domain_priors:suppression_score(international_recognition_architecture, 0.65).
domain_priors:theater_ratio(international_recognition_architecture, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(international_recognition_architecture, extractiveness, 0.58).
narrative_ontology:constraint_metric(international_recognition_architecture, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(international_recognition_architecture, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(international_recognition_architecture, tangled_rope).
narrative_ontology:human_readable(international_recognition_architecture, "International Recognition Architecture").
narrative_ontology:topic_domain(international_recognition_architecture, "political/institutional/international_relations").

domain_priors:requires_active_enforcement(international_recognition_architecture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(international_recognition_architecture, established_state_institutions).
narrative_ontology:constraint_beneficiary(international_recognition_architecture, incumbent_diplomatic_corps).
narrative_ontology:constraint_victim(international_recognition_architecture, emerging_polities).
narrative_ontology:constraint_victim(international_recognition_architecture, stateless_groups).
narrative_ontology:constraint_victim(international_recognition_architecture, post_conflict_entities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNRECOGNIZED POLITY (SNARE) — Structurally trapped. Lacks diplomatic representation, cannot access international credit markets, cannot import weapons or sign treaties. Recognition architecture defines which entities exist in the international system; exclusion is total and requires explicit approval from the same powers that benefit from the exclusion. No alternatives exist; exit is impossible without recognition.
constraint_indexing:constraint_classification(international_recognition_architecture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL NEIGHBOR STATE (TANGLED ROPE) — Constrained by security dilemmas and diplomatic isolation risk. Benefits from recognition architecture through predictability (know which entities have standing, can plan alliances). But also bears costs: cannot recognize breakaway regions without risking international isolation, cannot shift alliances easily without diplomatic consequences. Mixed coordination and extraction — the system both stabilizes the region and prevents adaptive responses to new circumstances.
constraint_indexing:constraint_classification(international_recognition_architecture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ESTABLISHED GREAT POWER (ROPE) — Net beneficiary with arbitrage capacity. Uses recognition selectively as a tool (recognizing aligned states, withholding from rivals). The recognition architecture enables coordination of international norms and provides leverage over which entities can participate in the system. Can recognize or delist entities based on strategic interest; has exit options (withdrawal from the UN, alternative diplomatic frameworks).
constraint_indexing:constraint_classification(international_recognition_architecture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL REFORM COALITION (SCAFFOLD) — Organized actors (NGOs, progressive states, academic institutions) see the recognition architecture as a temporary constraint with a generational sunset. Movements toward indigenous sovereignty, environmental federalism, and non-state actor participation in international governance suggest the binary state-recognition model is being gradually replaced by polycentric recognition systems. High suppression due to incumbent resistance, but the coalition has agency and sees an exit path through institutional evolution.
constraint_indexing:constraint_classification(international_recognition_architecture, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: UN RECOGNITION SYSTEM (PITON) — The formal recognition system (Montevideo Convention criteria: defined territory, permanent population, government, capacity for international relations) persists through institutional inertia despite low functional capacity to address contemporary polities. De facto recognition through trade, currency, and military cooperation now matters more than formal UN seat status. The ritual of recognition persists (theater_ratio ≈ 0.58) while real power has shifted to alternative recognition mechanisms (credit rating agencies, tech platform terms of service, trade blocs). The system is degraded but maintained because alternatives haven't fully replaced it.
constraint_indexing:constraint_classification(international_recognition_architecture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / WESTPHALIAN NATURAL LAW (MOUNTAIN) — From a civilizational perspective, some form of mutual recognition architecture appears as an irreducible requirement for interstate coordination: any system of multiple independent actors requires some mechanism to establish which actors are members of the system. This perspective sees recognition as a natural law of politics. However, the structural data contradicts the mountain classification — the engine will compute this as a false summit, revealing that the specific form (state-centric Westphalian recognition) is contingent, not necessary. Alternative recognition architectures (indigenous governance networks, corporate charter systems, supra-national unions, stateless organizing) demonstrate that the current form is one option among many, not a natural law.
constraint_indexing:constraint_classification(international_recognition_architecture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(international_recognition_architecture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(international_recognition_architecture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(international_recognition_architecture, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(international_recognition_architecture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(international_recognition_architecture, TR),
    TR >= 0.70.

:- end_tests(international_recognition_architecture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The recognition architecture extracts compliance with Westphalian state norms from applicants (they must have defined territory, permanent population, government, capacity for international relations). It also extracts legitimacy — being recognized is prestigious, and non-recognition is stigmatizing. But extractiveness is not total (0.70+) because alternative recognition mechanisms are functioning: entities can trade, raise capital, and coordinate without formal UN recognition. The measurement trajectory (0.42 → 0.62) reflects that extractiveness is increasing as the architecture becomes more purely extractive of legitimacy and less functional for actual coordination. Suppression (0.65): High. Barriers to exit include legal prohibition on alternative recognition frameworks, cultural delegitimation of non-state governance, military enforcement against unrecognized entities, and institutional lock-in (once excluded, extremely difficult to gain admission). But suppression is not total (0.75+) because some unrecognized entities function effectively via alternative recognition. Theater ratio (0.58): Moderate-high and rising. The formal recognition system (UN membership, diplomatic standing) is increasingly performative while real recognition happens through trade blocs, credit rating validation, technology platform governance, and military alliances. The theater has increased over the interval as the mismatch between formal and functional recognition has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint's perspectival gap is extreme — it appears as mountain (natural law) from one perspective and as a highly extractive snare from another. The gap reveals that the Westphalian recognition system naturalizes contingent institutional arrangements as inevitable. The established powers' rope perspective ('this is just how international coordination works') becomes the default frame, while the unrecognized polities' snare perspective ('we are structurally locked out with no alternatives') is treated as a policy problem rather than a structural constraint. The analytical observer's mountain perspective risks reifying this naturalization — the claim that 'any multipolar system must have recognition criteria' is true, but the specific form (state-centric, zero-sum, centralized UN gate) is not necessary. Polycentric recognition systems show that alternative architectures can solve the coordination problem without the extraction. The perspectival gap is not an analytic artifact — it reflects real asymmetry in who can see the system as contingent vs. necessary.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position relative to the extraction flow. Established great powers (d ≈ 0.10) can recognize or delist entities based on strategic interest; they are beneficiaries with high exit capacity (arbitrage option). Unrecognized polities (d ≈ 0.95) are trapped with no alternative; every capacity they need requires recognition. Regional neighbors (d ≈ 0.55) are partially trapped by security dilemmas — recognizing a breakaway region creates diplomatic isolation risk. The reform coalition (d ≈ 0.45) has agency and sees an exit path through institutional evolution. The UN system (d ≈ 0.25) is institutionally embedded; it can shift recognition policy but cannot easily exit the system it maintains. The analytical observer's d is derived from pure observation (analytical exit option), placing them at (d ≈ 0.72) where they see the structure but cannot change it from outside the international system. The piton classification for the UN system derives from the theater gate (theater_ratio ≥ 0.70 at t=75 projects to 0.65 current) combined with low functional extraction (ε ≤ 0.25 when measuring capacity-denial separately from legitimacy-extraction), indicating the system is maintained by institutional inertia rather than actual function.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED via perspectival decomposition. The constraint is genuinely a tangled rope (ε=0.58, suppression=0.65, χ≈0.58-0.62 across perspectives) because it combines real coordination function (establishing which entities have standing in international law) with asymmetric extraction (denying capacity to unrecognized entities). The false mountain perspective from the analytical observer is correctly identified as a false summit: while some recognition mechanism is necessary for any multipolar system (mountain), the specific Westphalian form is contingent (tangled rope at ε=0.58, not mountain at ε≤0.25). The measurement trajectory (theater increasing from 0.35 to 0.62, extractiveness increasing from 0.42 to 0.62) shows the system degrading toward pure theater and legitimacy extraction as alternative recognition mechanisms handle the actual coordination function. The classification resolves the tension by acknowledging that the coordination requirement is structural (some recognition mechanism must exist) while the Westphalian instantiation is extractive (this specific architecture extracts disproportionately from the excluded). This is precisely what tangled rope means: genuine coordination function combined with asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    recognition_fungibility,
    'To what extent are alternative recognition mechanisms (trade bloc membership, credit rating agency validation, digital platform status) functionally equivalent to state-level diplomatic recognition?',
    'Comparative analysis of which recognition form grants which capacities: can an entity trade without UN seat? Raise capital? Defend territory? Access technology standards? The fungibility of recognition mechanisms determines whether the Westphalian system is truly mandatory or contingent.',
    'If high fungibility: piton classification confirmed — the UN system is theater, real recognition happens elsewhere. If low fungibility: Westphalian system is genuinely necessary for some capacities; mountain classification holds for those specific functions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recognition_fungibility, empirical, 'Functional equivalence of alternative recognition mechanisms').

omega_variable(
    coalition_capacity_threshold,
    'What organizational threshold must reform coalitions reach before they can credibly establish alternative recognition architectures (e.g., biregional governance, polycentric legitimacy)?',
    'Historical case studies of successful institutional replacement: GATT→WTO transition, colonial→independent recognition transition, state→EU member transition. Identify what coalition power density was necessary.',
    'If threshold is reachable in current geopolitical context: scaffold sunset is empirically grounded. If threshold is very high: scaffold classification is aspirational rather than structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coalition_capacity_threshold, empirical, 'Organizational threshold for credible alternative recognition architecture').

omega_variable(
    suppression_mechanism_internalization,
    'How much of the measured suppression (0.65) is structural (legal barriers, physical isolation, inability to sign treaties) vs. internalized (unrecognized entities accepting delegitimacy, internalizing exclusion)?',
    'Post-recognition trajectory analysis: when entities gain recognition, do they maintain their prior networks and capacities, or do they show institutional dependency on the recognition grantors? If dependency is high, internalization was also high.',
    'If highly internalized: the suppression persists after structural barriers are removed; recognition alone is insufficient for genuine exit. If structural: removing legal barriers enables immediate exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression in recognition exclusion').

omega_variable(
    extractiveness_measurement_basis,
    'Should extractiveness be measured by denied capacity (economic, military, diplomatic) or by extraction of compliance/legitimacy (the unrecognized entity must accept the recognition regime''s criteria or remain excluded)?',
    'Two separate constraint stories with different ε values: capacity_denial_constraint (ε≈0.35) vs. legitimacy_extraction_constraint (ε≈0.62). The empirical distinction: can an entity function without recognition but with limited capacity, or is recognition itself the extraction mechanism?',
    'If capacity-denial primary: classification more snare-like (extraction through denial). If legitimacy-extraction primary: classification more tangled-rope-like (coordination through enforcement of criteria).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extractiveness_measurement_basis, conceptual, 'Primary extractiveness mechanism: capacity denial or legitimacy extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(international_recognition_architecture, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(intl_recog_tr_t0, international_recognition_architecture, theater_ratio, 0, 0.35).
narrative_ontology:measurement(intl_recog_tr_t25, international_recognition_architecture, theater_ratio, 25, 0.48).
narrative_ontology:measurement(intl_recog_tr_t50, international_recognition_architecture, theater_ratio, 50, 0.58).
narrative_ontology:measurement(intl_recog_tr_t75, international_recognition_architecture, theater_ratio, 75, 0.62).

% Extraction over time
narrative_ontology:measurement(intl_recog_be_t0, international_recognition_architecture, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(intl_recog_be_t25, international_recognition_architecture, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(intl_recog_be_t50, international_recognition_architecture, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(intl_recog_be_t75, international_recognition_architecture, base_extractiveness, 75, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(international_recognition_architecture, enforcement_mechanism).
narrative_ontology:affects_constraint(international_recognition_architecture, sovereign_debt_access).
narrative_ontology:affects_constraint(international_recognition_architecture, refugee_legal_status).
narrative_ontology:affects_constraint(international_recognition_architecture, indigenous_governance_autonomy).
narrative_ontology:affects_constraint(international_recognition_architecture, failed_state_intervention_legitimacy).

% DUAL FORMULATION NOTE:
% The international recognition architecture decomposes into two structurally distinct constraints: capacity_denial (ε≈0.35, which entities can trade/raise capital/sign treaties) and legitimacy_extraction (ε≈0.62, which entities are treated as legitimate international actors). The current story focuses on the combined constraint; downstream constraints inherit the recognition barrier differently depending on whether they are capacity-dependent or legitimacy-dependent. Capacity-denial has lower ε and resembles a technical coordination problem; legitimacy-extraction has higher ε and resembles pure extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(international_recognition_architecture, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
