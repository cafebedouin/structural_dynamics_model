% ============================================================================
% CONSTRAINT STORY: exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exogenous_override_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: exogenous_override_reading
 *   human_readable: Exogenous State Override: Top-Down Commitment Displacement Without Fringe Adoption
 *   domain: historical_sociology/state_formation/commitment_systems
 *
 * SUMMARY:
 *   The exogenous override reading instantiates a state capacity mechanism in
 *   which commitment systems (calendars, dress codes, naming conventions,
 *   kinship terminologies) are displaced through top-down decree with NO
 *   fringe adoption pathway. Unlike gradual commitment transitions where
 *   early adopters create a visible utility gradient that encourages broader
 *   adoption (endogenous climb), the exogenous override relies on enforcement
 *   machinery and legal prohibition to achieve compliance. Historical
 *   exemplar: Japan's Meiji calendar reform (1873) and Western dress mandates
 *   for official functions. The state did not wait for Buddhist communities
 *   or merchants to voluntarily shift calendar systems; it decreed the
 *   Gregorian calendar mandatory and prohibited the old calendrical system in
 *   official records. Similarly, Western dress codes were imposed on the
 *   state apparatus and military through direct command, not through
 *   demonstration of utility. This reading emphasizes that such transitions
 *   are distinct structural mechanisms requiring a separate M-set cell in
 *   state capacity models — they are not endogenous adoption processes, and
 *   they cannot be modeled by adding a 'faster threshold' parameter to
 *   emergent-adoption frameworks. The exogenous override is coercive
 *   displacement of competing commitment infrastructure, generating high
 *   extraction and suppression.
 *
 * KEY AGENTS:
 *   - State Administrative Apparatus: Primary beneficiary (institutional/arbitrage) — achieves unified commitment infrastructure enabling tax collection, military conscription, census management, and bureaucratic rationalization. No cost to standardization once enforcement is deployed.
 *   - Subject Populations: Primary victim (powerless/trapped) — face displacement of existing calendar systems, dress norms, naming conventions. No exit option and no voluntary adoption pathway. Enforcement is total.
 *   - Existing Commitment Keepers: Secondary victim (moderate/constrained) — monasteries (calendar specialists), dress guilds, kinship authorities lose functional role and institutional authority. Some displacement can be absorbed (internal practice continues) but authority over public commitment systems is eliminated.
 *   - Fringe Adopters (Counterfactual): Would-be agents (never exist in this reading) — if the state had allowed or encouraged early Western-dress wearers or calendar experimenters, they would have faced no such opportunity. The exogenous override pre-empts the fringe pathway.
 *   - Analytical Observer (Comparative State Capacity): Institutional actor (analytical/analytical) — sees both genuine coordination problem (unified commitment infrastructure) and coercive mechanism. Must classify as tangled rope at civilizational horizon, not as pure rope (coordination) or pure snare (extraction).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exogenous_override_reading, 0.68).
domain_priors:suppression_score(exogenous_override_reading, 0.72).
domain_priors:theater_ratio(exogenous_override_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exogenous_override_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(exogenous_override_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(exogenous_override_reading, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exogenous_override_reading, snare).
narrative_ontology:human_readable(exogenous_override_reading, "Exogenous State Override: Top-Down Commitment Displacement Without Fringe Adoption").
narrative_ontology:topic_domain(exogenous_override_reading, "historical_sociology/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exogenous_override_reading, 'fac73d50-46bc-4a58-b4e6-cca6e3416b05').
narrative_ontology:cs_kernel_codification('fac73d50-46bc-4a58-b4e6-cca6e3416b05', formalized).
narrative_ontology:cs_authority_grounding('fac73d50-46bc-4a58-b4e6-cca6e3416b05', extraction).
narrative_ontology:cs_interpretation_layer_present('fac73d50-46bc-4a58-b4e6-cca6e3416b05').
narrative_ontology:cs_reading_relation('fac73d50-46bc-4a58-b4e6-cca6e3416b05', exogenous_override_reading__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('fac73d50-46bc-4a58-b4e6-cca6e3416b05', exogenous_override_reading__hybrid_cascade_reading, coexists_with).
narrative_ontology:cs_axiom('fac73d50-46bc-4a58-b4e6-cca6e3416b05', foundational, state_exogenous_authority_necessary).
narrative_ontology:cs_axiom_status(state_exogenous_authority_necessary, holdable).
narrative_ontology:cs_axiom_grounding('fac73d50-46bc-4a58-b4e6-cca6e3416b05', state_exogenous_authority_necessary, instrumental).
narrative_ontology:cs_axiom('fac73d50-46bc-4a58-b4e6-cca6e3416b05', foundational, enforcement_substitutes_for_adoption_gradient).
narrative_ontology:cs_axiom_status(enforcement_substitutes_for_adoption_gradient, holdable).
narrative_ontology:cs_axiom_grounding('fac73d50-46bc-4a58-b4e6-cca6e3416b05', enforcement_substitutes_for_adoption_gradient, empirically_contingent).
narrative_ontology:cs_reference_frame('fac73d50-46bc-4a58-b4e6-cca6e3416b05', state_rationalization_through_commitment_unification).
narrative_ontology:cs_drift_state('fac73d50-46bc-4a58-b4e6-cca6e3416b05', contemporary_globalization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fac73d50-46bc-4a58-b4e6-cca6e3416b05', '2026-02-27T14:32:00Z').
narrative_ontology:cs_kernel_id(exogenous_override_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exogenous_override_reading, state_administrative_apparatus).
narrative_ontology:constraint_victim(exogenous_override_reading, subject_populations).
narrative_ontology:constraint_victim(exogenous_override_reading, existing_commitment_systems).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(exogenous_override_reading, existing_commitment_keepers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The state apparatus decrees the new commitment system (calendar, dress codes, naming conventions) and enforces compliance through legal penalties and institutional mandates. Officials adopt the new system immediately; enforcement machinery is deployed to suppress the old system in official records. The apparatus benefits from unified commitment infrastructure for tax collection, conscription, and census management.
narrative_ontology:constraint_stakeholder(exogenous_override_reading, state_administrative_apparatus, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(exogenous_override_reading, state_administrative_apparatus, beneficiary).

% Subject populations face legal prohibition of the old calendar system in official records and are compelled to use new naming conventions and dress codes for state interactions. No voluntary adoption pathway exists; utility of the new system is not demonstrated before enforcement begins. Transition costs (relearning calendar arithmetic, acquiring new dress items, registering names in new conventions) are borne entirely by subjects. No exit option from state jurisdiction is available.
narrative_ontology:constraint_stakeholder(exogenous_override_reading, subject_populations, payer,
    powerless, biographical, trapped, national).

% Monasteries, calendrical specialists, dress guilds, and kinship authorities lose their functional role in administering public commitment systems. Monks can continue internal calendrical notation but lose authority over official calendar. Dress guilds lose influence over state costume codes. Kinship authorities lose influence over naming conventions registered with the state. Some can migrate to private or internal practice; some face economic displacement as their expertise becomes administratively irrelevant.
narrative_ontology:constraint_stakeholder(exogenous_override_reading, existing_commitment_keepers, payer,
    moderate, biographical, constrained, national).

% In an endogenous adoption pathway (sibling reading), early adopters of the new calendar or Western dress would demonstrate utility through commercial success, bureaucratic efficiency, or social prestige, creating a visible adoption gradient. In the exogenous override mechanism, no such fringe pathway exists before the decree. This agent is counterfactual and excluded from this reading's narrative — it appears only in the sibling endogenous_climb_reading. Its absence is structurally significant: the exogenous override pre-empts the fringe adoption mechanism entirely.
narrative_ontology:constraint_stakeholder(exogenous_override_reading, fringe_adopters_counterfactual, excluded,
    moderate, biographical, mobile, local).
narrative_ontology:stakeholder_non_agent(exogenous_override_reading, fringe_adopters_counterfactual).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unify commitment infrastructure (calendar systems, dress codes, naming conventions, kinship terminologies) across state territory to enable uniform administration, tax collection, military conscription, census management, and bureaucratic communication. The state solves a genuine coordination problem: dispersed local calendars and naming systems create transaction costs for state-level administration.
% TRANSFER_FUNCTION: The state transfers compliance burden from subject populations and existing commitment keepers to itself. Subjects must learn new calendar arithmetic and naming conventions; commitment keepers lose administrative authority. In exchange, the state receives a unified commitment system and reduced administrative complexity. The transfer is asymmetric: subjects and keepers lose institutional authority and incur transition costs; the state gains coordination benefits and administrative power.
% ABSENT_VOICES: Merchants and traders who might have voluntarily adopted the new calendar for commercial efficiency (absent from this reading because no fringe adoption occurs); religious minorities whose commitment systems are displaced (temples, mosques, synagogues whose calendars are officially prohibited); artisans and craft workers whose expertise in old dress codes becomes administratively irrelevant. These groups would object if included in the decree process but are not consulted. In the exogenous override mechanism, their objections are suppressed by enforcement, not addressed through negotiation.
% DISAPPEARANCE_RATIONALE: If the exogenous override decree were suddenly revoked (commitment system reverted to pre-decree state), the world would rearrange substantially. Tax collection and census management would require re-translation across local calendars. Military conscription and bureaucratic administration would face increased complexity. However, populations would not immediately revert to old commitment systems (habituation and vestigial practice dynamics); the rearrangement would be partial and asymmetric. The constraint is not a natural law — it is a state choice that produces observable coordination benefits but extracts them coercively.
% FOUNDING_PROBLEM: State administrative capacity and territorial coherence in the face of dispersed local commitment systems. The Meiji state faced a fragmented calendar landscape (Buddhist, Shinto, regional variants) and diverse dress codes across domains. Unifying these systems was a genuine problem for rational-bureaucratic state formation.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by Meiji state documents (decrees, administrative correspondence) and secondary historical sources on state modernization. However, the problem's urgency is contested: some historians argue the state exaggerated the administrative burden to justify cultural dominance (alternative reading). The founding problem exists, but its necessity (whether unification HAD to occur via exogenous override, or could have occurred via endogenous adoption pathways) is the core contestation between this reading and the sibling readings.
narrative_ontology:disappearance_verdict(exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(exogenous_override_reading, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBJECT POPULATION (SNARE) — Faces coercive displacement of existing commitment infrastructure (calendar reckoning, dress codes, naming conventions, kinship terminology) with no exit option and no fringe adoption pathway. Enforcement is direct and total; alternatives are suppressed by state decree. The displacement extracts compliance and eliminates competing loyalty structures without offering a voluntary adoption gradient.
constraint_indexing:constraint_classification(exogenous_override_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EXISTING COMMITMENT KEEPERS (SNARE) — Monasteries, calendrical specialists, kinship authorities, dress guilds — face displacement of their functional authority without compensation or gradual transition. The new commitment system is imposed by state fiat; the old system is not dismantled through superior performance but through legal prohibition. High extraction: authority is removed, practitioners are displaced, knowledge systems are abandoned.
constraint_indexing:constraint_classification(exogenous_override_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE ADMINISTRATIVE APPARATUS (ROPE) — Experiences the override as pure coordination. The state decree creates a unified commitment infrastructure (calendar, dress, name formats) that enables uniform administration, tax collection, military conscription, and census management. Compliance is total and enforced; the state apparatus benefits from standardization without competitive alternatives. Low experienced extraction because enforcement is automatic and alternatives cannot organize.
constraint_indexing:constraint_classification(exogenous_override_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DISPLACED COMMITMENT SYSTEM (PITON) — In the long term (generational horizon), the old commitment infrastructure (Buddhist calendar, courtier dress codes, aristocratic naming conventions) persists as vestigial performance within new institutional structures. Monasteries continue calendrical notation for internal use; families maintain hidden genealogies in old reckoning systems; elite fashion preserves suppressed dress codes in private contexts. Theater ratio is high: the old system survives as inert practice maintained by institutional inertia, divorced from its original coordination function.
constraint_indexing:constraint_classification(exogenous_override_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / COMPARATIVE VIEW (TANGLED ROPE) — From a civilizational vantage, the exogenous override reveals a genuine coordination problem (unifying commitment infrastructure for state administration) alongside undeniable coercive displacement. The state solves a real coordination problem — shared calendar, shared dress codes, shared naming — but does so via extraction from populations without voluntary adoption pathways. The constraint is not pure snare (it does solve coordination) nor pure rope (it does coerce displacement). The analytical observer must classify this as tangled rope: genuine coordination function bundled with coercive extraction.
constraint_indexing:constraint_classification(exogenous_override_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: FALSE SUMMIT NATURALIZATION — A competing analytical perspective treats the exogenous override as an inevitable feature of state modernization: all states that achieve bureaucratic capacity must unify commitment infrastructure; this is a natural law of state formation, not a contingent coercive mechanism. However, the structural data contradicts this mountain classification. The exogenous override is distinguished precisely by the ABSENCE of a fringe adoption pathway — alternative readings (endogenous_climb_reading, hybrid_cascade_reading) show that adoption CAN be emergent and gradual. That endogenous pathways exist and produce different outcomes refutes the naturalness claim. The false summit here naturalizes contingent state strategy as inevitable modernization.
constraint_indexing:constraint_classification(exogenous_override_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exogenous_override_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(exogenous_override_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(exogenous_override_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(exogenous_override_reading, TR),
    TR >= 0.70.

:- end_tests(exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The state extracts compliance with the new commitment system and eliminates competing loyalty structures (temples maintain old calendars, families preserve hidden genealogies). Subjects bear the cost of relearning systems without voluntary adoption phase. The extractiveness is not at maximum (0.85+) because the new system does provide genuine coordination benefits — unified calendar reduces transaction costs for commerce and administration. But those benefits are claimed by the state apparatus; subjects experience only the costs of transition. Suppression (0.72): High. The state decree prohibits use of old calendars in official records, mandates Western dress for state actors, and eliminates naming conventions tied to old kinship systems. Non-compliance carries legal penalties. Suppression remains at 0.72 rather than 0.85+ because long-term habituation (by t=20) reduces the active enforcement burden; the suppressed system persists as vestigial practice but no longer as visible alternative. Theater ratio (0.25): Low. The exogenous override is structurally transparent. The state makes the decree explicit; there is no facade of voluntary adoption or organic emergence. Theater would be HIGH if the state pretended the new calendar naturally emerged through utility demonstration; but the decree-and-enforce mechanism is theatrically straightforward. Theater rises to 0.35 by t=20 when the old system survives as inert practice (piton dynamics), but remains low at the moment of imposition.
 *
 * PERSPECTIVAL GAP:
 *   The subject population sees pure coercive displacement (snare). The existing commitment keepers see loss of functional authority (snare). The state apparatus sees unified coordination infrastructure (rope). The displaced system, at generational timescale, sees its own degradation into vestigial performance (piton). The analytical observer at civilizational scope recognizes that this constraint solves a genuine coordination problem (unified commitment systems for state administration) but through coercive mechanisms with no emergent-adoption phase — hence tangled rope, not rope alone. The false-summit perspective risks naturalizing the exogenous override as inevitable modernization, but comparative historical evidence shows alternative pathways (endogenous climb, hybrid cascade) are structurally possible. The gap between snare (subject view) and rope (state view) is maximal precisely because there is no fringe adoption pathway that could gradually align incentives; the override is entirely non-consensual.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus experiences d ≈ 0.1 (full beneficiary): the constraint delivers unified commitment infrastructure at minimal cost (enforcement is automatic, alternatives cannot organize). The subject population experiences d ≈ 0.95 (full target): they bear all transition costs, face legal prohibition of alternatives, and gain no voice in the new system's design. The existing commitment keepers experience d ≈ 0.85 (strong target): their functional authority is eliminated and their knowledge systems are displaced. The analytical observer experiences d ≈ 0.5 (symmetric) at civilizational horizon: the constraint solves real coordination but uses coercive mechanism. The engine derives d from beneficiary status (state) vs victim status (population) and exit options (trapped vs arbitrage), producing the directionality gradient that explains perspectival classification divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading RESOLVES the mandatrophy of commitment displacement by showing that exogenous override is a DISTINCT mechanism from endogenous adoption pathways. The mandate for unified commitment infrastructure is genuine (solved by either exogenous or endogenous pathways). But the exogenous override mechanism — top-down decree with enforcement, no fringe gradient — represents a choice by the state to pay enforment costs rather than wait for emergent adoption. The mandatrophy is resolved by refusing to collapse this choice into 'inevitable modernization' (the false-summit trap). The constraint's classification differs across readings (snare here vs rope in endogenous_climb_reading vs tangled_rope in hybrid_cascade_reading) precisely because the mandate is achieved through different mechanisms. All three readings solve the mandate; this reading emphasizes that the STATE CHOOSES the coercive pathway, which is NOT inevitable and IS extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fringe_adoption_counterfactual,
    'Would the Meiji calendar and dress changes have been adopted without state decree if a smaller fringe had demonstrated their utility first?',
    'Comparative analysis of commitment transitions with vs without fringe adoption pathways (e.g., metric system adoption in 18th-century Europe vs Japanese calendar reform); historical reconstruction of pre-decree adoption rates in comparable populations; ethnographic evidence of spontaneous vs coerced shifts.',
    'If fringe adoption would have occurred: this reading''s core claim (exogenous override is distinct from endogenous climb) is falsified; the constraint would classify as rope or hybrid_cascade. If fringe adoption would NOT have occurred: the exogenous override mechanism is validated as structurally distinct and ε-invariant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fringe_adoption_counterfactual, empirical, 'Whether fringe adoption pathways were available for calendar/dress changes').

omega_variable(
    enforcement_cost_substitution,
    'Did the state incur higher enforcement costs imposing the decree without a fringe adoption gradient, compared to a hypothetical enforcement strategy allowing voluntary early adoption?',
    'Historical cost accounting of enforcement machinery (conscription, punishment, surveillance); comparative analysis of enforcement effort in exogenous vs endogenous transitions; measurement of non-compliance rates and suppression intensity.',
    'If exogenous override had lower enforcement costs: the reading is structurally optimal for the state and the constraint moves closer to rope (efficient coordination). If exogenous override had substantially higher enforcement costs: the reading reveals a state choosing coercion over efficiency, classifying deeper into snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_cost_substitution, empirical, 'Enforcement cost comparison: exogenous decree vs fringe-gradient pathways').

omega_variable(
    kernel_commitment_system_reading,
    'Is this constraint one reading of a contested kernel about how commitment systems are transformed — exogenous override (this reading) vs endogenous climb vs hybrid cascade — or is ''commitment displacement'' itself the kernel with multiple empirical readings?',
    'Axiom charting: if foundational disagreement is NORMATIVE (should displacement be top-down? is coercion legitimate?), kernel structure applies. If foundational disagreement is EMPIRICAL (can displacement be endogenous? do adoption gradients exist?), sibling readings are alternative empirical claims, not committer-system readings. Prolog compilation will route to reading_relations (axioms/normative) or network/affects_constraints (empirical sibling hypotheses).',
    'If kernel interpretation: this story''s cs_structure carries axioms and reading_relations; sibling stories must coordinate their reading_relations to match. If empirical sibling hypothesis: cs_structure is omitted; network.affects_constraints links the stories as competing models of the same constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_commitment_system_reading, conceptual, 'Kernel structure vs empirical sibling hypothesis classification').

omega_variable(
    false_summit_modernization_claim,
    'Is the false-summit ''naturalizing modernization'' perspective (PERSPECTIVE 6) claiming that exogenous override is universal across state modernizations, or only that it is COMMON? If universal claim: the perspective is refuted if any modernizing state achieved commitment unification via endogenous pathways.',
    'Historical survey of commitment transitions in state formation: metric system (France vs other nations), calendar reform (Japan vs Ottoman vs Russian empires), naming convention standardization, dress code unification. Identify cases of endogenous adoption and compare enforcement burdens, adoption rates, and fringe participation.',
    'If any endogenous pathway succeeded in comparable modernization context: the false-summit naturalization fails; the exogenous override is a strategy choice, not a natural law. If all modernizations required exogenous override: the mountain classification gains force (though remains falsifiable).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_modernization_claim, empirical, 'Whether exogenous override is universal or contingent across state modernizations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exogenous_override_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exog_override_theater_t0_pre_decree, exogenous_override_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(exog_override_theater_t1_decree_announcement, exogenous_override_reading, theater_ratio, 1, 0.18).
narrative_ontology:measurement(exog_override_theater_t5_consolidation, exogenous_override_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(exog_override_theater_t20_vestigial_practice, exogenous_override_reading, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(exog_override_extractiveness_t0_pre_decree, exogenous_override_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(exog_override_extractiveness_t1_decree_enforcement, exogenous_override_reading, base_extractiveness, 1, 0.68).
narrative_ontology:measurement(exog_override_extractiveness_t5_consolidation, exogenous_override_reading, base_extractiveness, 5, 0.71).
narrative_ontology:measurement(exog_override_extractiveness_t20_long_term, exogenous_override_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(exog_override_suppression_t0_pre_decree, exogenous_override_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(exog_override_suppression_t1_decree_enforcement, exogenous_override_reading, suppression_requirement, 1, 0.72).
narrative_ontology:measurement(exog_override_suppression_t5_consolidation, exogenous_override_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(exog_override_suppression_t10_habituation, exogenous_override_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(exog_override_suppression_t20_long_term, exogenous_override_reading, suppression_requirement, 20, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exogenous_override_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(exogenous_override_reading, 0.12).
narrative_ontology:affects_constraint(exogenous_override_reading, endogenous_climb_reading).
narrative_ontology:affects_constraint(exogenous_override_reading, hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% The imposition_pathway_kernel contains three structurally distinct readings of how commitment systems are transformed. This story (exogenous_override_reading) models top-down decree with enforcement and zero fringe adoption pathway. The endogenous_climb_reading models gradual adoption through utility demonstration and fringe adoption with no state decree. The hybrid_cascade_reading models initial fringe adoption amplified by state legitimation. These are NOT alternative measurements of one constraint — they have different ε values and different structural data. The network links them as sibling readings of the same kernel (commitment system transformation) with different mechanisms. A commitment system that transitions via exogenous override extracts more and has higher suppression than one that transitions via endogenous climb, even if the final unified state is identical. The mechanism choice is the independent variable driving classification difference.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
