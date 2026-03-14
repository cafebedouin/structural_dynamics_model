% ============================================================================
% CONSTRAINT STORY: unclos_dispute_resolution_capacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_dispute_resolution_capacity, []).

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
 *   constraint_id: unclos_dispute_resolution_capacity
 *   human_readable: UNCLOS Dispute Resolution Capacity Constraint
 *   domain: international_law/maritime_governance
 *
 * SUMMARY:
 *   The United Nations Convention on the Law of the Sea (UNCLOS) established
 *   an ambitious framework for peaceful maritime dispute resolution,
 *   including the International Tribunal for the Law of the Sea (ITLOS) and
 *   binding arbitration procedures. However, the practical capacity of this
 *   system exhibits a structural tension between its coordination function
 *   (preventing maritime conflicts, establishing shared rules for ocean use)
 *   and its extraction mechanism (privileging states with legal capacity,
 *   resources, and geopolitical power to navigate complex procedures). Small
 *   island states and developing coastal nations face high barriers to
 *   dispute access despite formal legal equality; powerful maritime states
 *   can selectively engage with tribunal jurisdiction, using the system
 *   strategically while bypassing it when disadvantageous. The constraint
 *   demonstrates tangled rope classification: genuine coordination (peaceful
 *   dispute resolution) coexists with asymmetric extraction (access barriers
 *   that advantage wealthy states). Theater has increased over the 30-year
 *   interval as procedural complexity has accumulated, while the underlying
 *   extractiveness (cost barriers for smaller states) has grown
 *   proportionally. This story models how international law institutions can
 *   simultaneously solve coordination problems and create extractive
 *   hierarchies.
 *
 * KEY AGENTS:
 *   - Small Island States: Primary victims (powerless/trapped) — existential maritime interests but lack legal/financial capacity to access formal dispute resolution
 *   - Developing Coastal Nations: Secondary victims (moderate/constrained) — face resource barriers and complex procedures; participate under legitimacy pressure rather than genuine choice
 *   - Powerful Maritime States: Primary beneficiaries (institutional/arbitrage) — leverage UNCLOS procedures selectively, navigate complexity effectively, benefit from asymmetric access
 *   - ITLOS Institutional Interests: Secondary beneficiary (institutional/constrained) — tribunal interests (caseload justification, institutional expansion, procedural authority) partially diverge from neutral dispute resolution
 *   - Dispute Resolution Accessibility: Tertiary victim (powerless/identity_locked) — abstract collective good constituted through UNCLOS participation; states internalize obligation to participate despite inaccessibility
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing state sovereignty constraints as inherent to maritime law rather than contingent institutional design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_dispute_resolution_capacity, 0.54).
domain_priors:suppression_score(unclos_dispute_resolution_capacity, 0.62).
domain_priors:theater_ratio(unclos_dispute_resolution_capacity, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_dispute_resolution_capacity, extractiveness, 0.54).
narrative_ontology:constraint_metric(unclos_dispute_resolution_capacity, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(unclos_dispute_resolution_capacity, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_dispute_resolution_capacity, tangled_rope).
narrative_ontology:human_readable(unclos_dispute_resolution_capacity, "UNCLOS Dispute Resolution Capacity Constraint").
narrative_ontology:topic_domain(unclos_dispute_resolution_capacity, "international_law/maritime_governance").

domain_priors:requires_active_enforcement(unclos_dispute_resolution_capacity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_dispute_resolution_capacity, powerful_maritime_states).
narrative_ontology:constraint_beneficiary(unclos_dispute_resolution_capacity, tribunal_institutional_interests).
narrative_ontology:constraint_victim(unclos_dispute_resolution_capacity, smaller_island_states).
narrative_ontology:constraint_victim(unclos_dispute_resolution_capacity, developing_coastal_nations).
narrative_ontology:constraint_victim(unclos_dispute_resolution_capacity, dispute_resolution_accessibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL ISLAND STATE CLAIMANT (SNARE) — Cannot exit UNCLOS framework; maritime interests are existential (EEZ, fishing rights, marine resources). Faces maximum suppression: costs of formal dispute resolution (legal experts, tribunal fees, prolonged timelines) are prohibitive. No alternative dispute mechanism with equivalent legitimacy. Experiences pure extraction: powerful states use ITLOS/ISDS procedures selectively while smaller states absorb costs without access.
constraint_indexing:constraint_classification(unclos_dispute_resolution_capacity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING COASTAL NATION (TANGLED ROPE) — Constrained exit: can theoretically opt for bilateral negotiation but faces pressure to use formal UNCLOS mechanisms for legitimacy and enforcement. Genuine coordination function exists (dispute procedures prevent maritime conflict escalation). But asymmetric extraction: costs of participation are unequally distributed. Procedural complexity advantages wealthy nations with legal capacity.
constraint_indexing:constraint_classification(unclos_dispute_resolution_capacity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: POWERFUL MARITIME STATE (ROPE) — Experiences the dispute resolution framework as pure coordination. Has capacity to navigate ITLOS procedures, leverage arbitration, and shape outcomes. Arbitrage options abundant: can use UNCLOS procedures when favorable, bilateral negotiation when advantageous, can delay or withdraw strategically. Net beneficiary — dispute resolution system coordinates claims while allowing selective exploitation of procedural complexity.
constraint_indexing:constraint_classification(unclos_dispute_resolution_capacity, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ITLOS INSTITUTIONAL ACTOR (TANGLED ROPE) — Constrained by treaty obligations and legitimacy dependence on voluntary state participation. Genuine coordination function: resolves maritime disputes and develops consistent jurisprudence. But institutional extraction: tribunal interests (caseload, budget, institutional expansion) create incentive to adopt procedures that advantage repeat players (powerful states with legal capacity) and disadvantage first-time users. Compulsory jurisdiction for select dispute types creates obligation but also protects institutional workload.
constraint_indexing:constraint_classification(unclos_dispute_resolution_capacity, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: DISPUTE RESOLUTION ACCESSIBILITY (SNARE, IDENTITY_LOCKED) — The abstract collective commitment to 'peaceful dispute resolution' is constituted through UNCLOS framework participation. Smaller states cannot exit without abandoning identity as 'rule-of-law participant' or losing legitimacy in multilateral forums. Identity lock: internalized norm that formal UNCLOS participation is morally required, even when procedurally inaccessible. Bears costs of maintaining system legitimacy while experiencing it as extractive.
constraint_indexing:constraint_classification(unclos_dispute_resolution_capacity, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 6: UNCLOS DISPUTE RESOLUTION THEATER (PITON) — The elaborate procedural architecture (compulsory jurisdiction, prompt release procedures, sea-to-land linkages) is substantially performative: states with geopolitical interests circumvent formal procedures through strategic non-participation, diplomatic pressure, or delayed acknowledgment of tribunal authority. Theater persists because UNCLOS legitimacy depends on appearing to offer neutral dispute resolution; actual function (capacity to resolve contested maritime claims) is degraded. Maintained through institutional inertia — alternatives (UN General Assembly dispute mechanisms, bilateral arbitration) exist but are perceived as less legitimate.
constraint_indexing:constraint_classification(unclos_dispute_resolution_capacity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational/universal perspective, maritime dispute resolution capacity is structurally limited by the absence of supranational enforcement authority. States cannot be compelled to accept tribunal jurisdiction; compliance depends on reciprocal interest and reputation. This perspective risks treating contingent institutional limitations (state sovereignty doctrine, enforcement mechanisms) as inherent to international law itself. Engine's false summit detector will reveal this as naturalization of political choice, not natural law.
constraint_indexing:constraint_classification(unclos_dispute_resolution_capacity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_dispute_resolution_capacity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(unclos_dispute_resolution_capacity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(unclos_dispute_resolution_capacity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_dispute_resolution_capacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(unclos_dispute_resolution_capacity, TR),
    TR >= 0.70.

:- end_tests(unclos_dispute_resolution_capacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.54): Moderate-high. The dispute resolution system extracts from smaller states through access barriers (legal expertise costs, prolonged timelines, tribunal fee structures) while benefiting powerful states through asymmetric capacity. But extractiveness is not maximal (≥0.66) because the system does function — disputes are occasionally resolved, precedent develops, and some smaller states do participate successfully. The extraction is real but not complete predation. Suppression (0.62): High. Barriers include specialized legal capacity requirements, costs of representation before international tribunal, procedural complexity, and alternative mechanism uncertainty. States face strong legitimacy pressure to participate in formal UNCLOS mechanisms, reducing perceived exit options. But suppression is not total (0.60-1.0 band) because some states do negotiate bilaterally and manage without formal adjudication. Theater ratio (0.58): Moderate-high. Elaborate procedural architecture (compulsory jurisdiction declarations, prompt release procedures, coastal state rights balancing) is partly performative: geopolitically powerful states frequently circumvent formal mechanisms through strategic non-engagement, and tribunal authority depends on voluntary compliance rather than enforcement. But theater is not dominant (≥0.70) because some portion of procedural complexity reflects genuine coordination challenges.
 *
 * PERSPECTIVAL GAP:
 *   The gap between beneficiary (powerful state, institutional/arbitrage) and victim (small state, powerless/trapped) perspectives is maximal. Powerful states experience the constraint as pure coordination (Rope) because they have capacity to navigate procedures and strategic options to opt out. Small states experience it as snare (pure extraction) because they face high suppression (cost/complexity barriers) and no meaningful exit (maritime interests are existential). The tribunal (institutional/constrained) occupies a middle position: it genuinely coordinates disputes but also has institutional interests (caseload, authority, budget justification) that create subtle biases toward cases that expand its jurisdiction or establish favorable precedent. The moderate player (developing nation with some capacity but limited resources) sees tangled rope — mixed coordination and extraction. The identity-locked victim (the abstract 'rule-of-law commitment') sees snare — they cannot exit without abandoning their international legitimacy, so they absorb costs of an inaccessible system. The piton perspective (institutional theater) notes that formal UNCLOS procedures persist partly through legitimacy inertia — they perform the function of 'peaceful dispute resolution' but actual conflict prevention increasingly occurs through bilateral and regional channels that bypass ITLOS entirely.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from power level, exit options, and beneficiary/victim status. Small island states as trapped victims get maximum d (~0.95) → maximum f(d) (~1.42) → experience high χ despite moderate ε (0.54). Powerful maritime states as institutional beneficiaries with arbitrage options get low d (~0.10) → negative f(d) (~-0.05) → experience effectively negative χ (system subsidizes them). Developing coastal nations as moderate constrained victims get d ~0.60 → f(d) ~0.95 → experience moderate χ. The tribunal as institutional actor with partial beneficiary status and constrained exit gets d ~0.35 → f(d) ~0.35. The group-level victim (accessibility as collective good) experiences d close to powerless/trapped (~0.93) → very high f(d). The analytical observer uses canonical d for analytical power level (~0.73) but risks false summit classification if treating institutional constraints as natural law. Scope modifier σ(S)=1.2 for global scope applies to all — maritime disputes are inherently large-scope, increasing χ across all agents. The tangled rope classification is sustained by: (1) beneficiaries exist (powerful states, tribunal institutional interests), (2) victims exist (smaller states, accessibility), (3) asymmetric extraction is real (barriers to victim access), and (4) genuine coordination function (dispute resolution prevents some maritime conflicts).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing between (a) the coordination function that UNCLOS genuinely serves (preventing maritime conflict escalation, establishing shared ocean governance rules), and (b) the extraction mechanism that arises from procedural complexity asymmetries (powerful states navigate easily, smaller states face prohibitive costs). A pure snare classification would miss the real coordination — without UNCLOS, maritime disputes might escalate to conflict. A pure rope classification would miss the real extraction — the system's accessibility is genuinely unequal. Tangled rope is the correct classification because both mechanisms are structural and both are detected in the metrics: suppression (0.62) indicates real barriers, but extractiveness (0.54) is moderate rather than maximal because the system does coordinate. The theater ratio (0.58) indicates that procedures are partially performative but not purely theatrical. The perspectival gap (rope for beneficiaries, snare for victims, piton for the system itself) reveals that the constraint's classification depends on structural position — no single 'true' type exists, but the tangled rope is the analytical observer's best single assessment. The engine's detection of this constraint will prevent false labeling of geopolitical extraction as 'inherent to maritime law' and instead identify it as a contingent institutional arrangement that could be modified (e.g., by reducing legal representation costs, streamlining procedures, funding access for smaller states) to shift toward pure rope. Alternatively, the scaffold perspective suggests that alternative dispute mechanisms (regional courts, bilateral arbitration) are gradually reducing dependence on ITLOS for some dispute categories — this suggests a slow sunset dynamic where procedural complexity eventually becomes less extractive because alternatives mature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    access_cost_quantification,
    'What is the precise cost threshold that determines accessibility for smaller states — legal representation, tribunal fees, prolonged litigation timelines?',
    'Empirical analysis of case participation by state GDP, legal capacity, and case outcomes; correlation between state resources and dispute success rates',
    'If threshold is low (< 2% of state GDP): accessibility barrier is overstated; more states should participate. If threshold is high (> 5% GDP): suppression metric is correct and many states are structurally unable to access the system.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(access_cost_quantification, empirical, 'Cost threshold determining accessibility for smaller states').

omega_variable(
    selective_jurisdiction_exploitation,
    'Do powerful states systematically avoid compulsory jurisdiction for dispute types disadvantageous to them (e.g., continental shelf delimitation vs. deep-sea fishing)?',
    'Analysis of jurisdiction declarations, strategic opt-outs, and dispute type participation rates by state power level',
    'If systematic: extraction classification is correct (snare/tangled rope from victim perspective). If random: dispute system is functioning as intended (rope from all perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_jurisdiction_exploitation, empirical, 'Whether powerful states systematically exploit jurisdiction selectivity').

omega_variable(
    alternative_resolution_sufficiency,
    'Are bilateral negotiations, regional dispute mechanisms, and UNCLOS-external arbitration producing equivalent outcomes to formal ITLOS adjudication?',
    'Comparison of dispute outcomes (enforcement, longevity, state satisfaction) across ITLOS cases vs. bilateral settlements vs. regional mechanism decisions',
    'If alternatives are equivalent: scaffold perspective is correct (sunset toward decentralized resolution). If ITLOS is superior: institutional dependence is real and constrains smaller states to formal mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_resolution_sufficiency, empirical, 'Whether alternative dispute mechanisms produce equivalent outcomes').

omega_variable(
    identity_lock_reversibility,
    'Can states abandon UNCLOS dispute resolution participation without losing legitimacy in international forums?',
    'Analysis of diplomatic responses to states declaring non-participation in specific UNCLOS mechanisms; tracking reputational costs',
    'If reversible (low diplomatic cost): exit_options should be ''constrained'' not ''identity_locked''; smaller states have more agency than analysis suggests. If irreversible (high legitimacy loss): identity lock is real and constrains even when cost-benefit analysis suggests exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Whether identity lock to UNCLOS participation is reversible').

omega_variable(
    tribunal_institutional_bias,
    'Does ITLOS procedural evolution (jurisdiction expansion, docket management, jurisprudence development) systematically advantage powerful states or repeat institutional players?',
    'Longitudinal analysis of tribunal decisions, case selection, procedural rulings by plaintiff state power level; tracking of institutional preference shifts',
    'If bias is systematic: tangled rope classification is correct (tribunal extracts institutional value while coordinating). If neutral: ITLOS functions as pure coordinator from all perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tribunal_institutional_bias, empirical, 'Whether ITLOS procedurally advantages powerful states').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_dispute_resolution_capacity, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unclos_tr_t0, unclos_dispute_resolution_capacity, theater_ratio, 0, 0.42).
narrative_ontology:measurement(unclos_tr_t15, unclos_dispute_resolution_capacity, theater_ratio, 15, 0.52).
narrative_ontology:measurement(unclos_tr_t30, unclos_dispute_resolution_capacity, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(unclos_be_t0, unclos_dispute_resolution_capacity, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(unclos_be_t15, unclos_dispute_resolution_capacity, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(unclos_be_t30, unclos_dispute_resolution_capacity, base_extractiveness, 30, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_dispute_resolution_capacity, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_dispute_resolution_capacity, maritime_zone_delimitation_asymmetry).
narrative_ontology:affects_constraint(unclos_dispute_resolution_capacity, small_state_maritime_rights_accessibility).

% DUAL FORMULATION NOTE:
% UNCLOS dispute resolution capacity is structurally upstream of specific maritime claims disputes (continental shelf delimitation, EEZ boundary disputes) and downstream of the broader international law architecture that constitutes state sovereignty. The dispute resolution system itself is a distinct constraint with its own extractiveness value reflecting procedural access asymmetries, rather than being a property of the underlying substantive maritime law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_dispute_resolution_capacity, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
