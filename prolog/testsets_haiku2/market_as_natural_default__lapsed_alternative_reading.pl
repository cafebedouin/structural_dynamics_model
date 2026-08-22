% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__lapsed_alternative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__lapsed_alternative_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: market_as_natural_default__lapsed_alternative_reading
 *   human_readable: Market as Natural Default (Lapsed Alternative Reading)
 *   domain: political_economy/ideology_studies/economic_history
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   'market-as-natural-default.' The reading authored here holds that market
 *   dominance results from historical forgetting of alternatives, not from
 *   active closure or beneficiary defense. Under this reading, the constraint
 *   exhibits very low extractiveness (ε = 0.09) because no identifiable party
 *   actively maintains or profits from the naturalization — the forgetting is
 *   structural and lapsed, not functional and defended. The constraint
 *   approaches mountain status (natural law) while remaining contestable
 *   through historical recovery. This reading is distinct from the
 *   beneficiary_maintained_reading (in which identifiable winners actively
 *   defend market naturalization post-hoc and ε is substantially higher) and
 *   the hybrid_amnesia_reading (in which forgetting creates the conditions
 *   for beneficiary capture). Apply Rule 1: this story generates ONLY this
 *   reading, with its own ε, stakeholder structure, and beneficiary claim.
 *   The committer-axis dynamics (which reading is true, where the
 *   disagreement is located) route through the omegas and the
 *   cs_structure.reading_relations/axioms blocks, not through prose hedging.
 *
 * KEY AGENTS:
 *   - Economic historians: recover suppressed alternatives and challenge inevitability claims
 *   - Contemporary economic agents: inherit market-as-default as background without defending it
 *   - Policy scholars: analyze institutional displacement and forgetting mechanisms
 *   - Practitioners of suppressed alternatives (non-agent): would benefit from memory recovery but lack institutional access
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__lapsed_alternative_reading, 0.09).
domain_priors:suppression_score(market_as_natural_default__lapsed_alternative_reading, 0.12).
domain_priors:theater_ratio(market_as_natural_default__lapsed_alternative_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, extractiveness, 0.09).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__lapsed_alternative_reading, mountain).
narrative_ontology:human_readable(market_as_natural_default__lapsed_alternative_reading, "Market as Natural Default (Lapsed Alternative Reading)").
narrative_ontology:topic_domain(market_as_natural_default__lapsed_alternative_reading, "political_economy/ideology_studies/economic_history").

domain_priors:emerges_naturally(market_as_natural_default__lapsed_alternative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__lapsed_alternative_reading, '83534048-f6ba-43e0-836c-f8c652b77322').
narrative_ontology:cs_kernel_codification('83534048-f6ba-43e0-836c-f8c652b77322', implicit).
narrative_ontology:cs_authority_grounding('83534048-f6ba-43e0-836c-f8c652b77322', practice).
narrative_ontology:cs_interpretation_layer_present('83534048-f6ba-43e0-836c-f8c652b77322').
narrative_ontology:cs_reading_relation('83534048-f6ba-43e0-836c-f8c652b77322', market_as_natural_default__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_reading_relation('83534048-f6ba-43e0-836c-f8c652b77322', market_as_natural_default__hybrid_amnesia_reading, influences).
narrative_ontology:cs_axiom('83534048-f6ba-43e0-836c-f8c652b77322', foundational, market_naturalization_as_passive_historical_amnesia).
narrative_ontology:cs_axiom_status(market_naturalization_as_passive_historical_amnesia, holdable).
narrative_ontology:cs_axiom_grounding('83534048-f6ba-43e0-836c-f8c652b77322', market_naturalization_as_passive_historical_amnesia, empirically_contingent).
narrative_ontology:cs_axiom('83534048-f6ba-43e0-836c-f8c652b77322', secondary, suppressed_alternatives_are_recoverable).
narrative_ontology:cs_axiom_status(suppressed_alternatives_are_recoverable, holdable).
narrative_ontology:cs_axiom_grounding('83534048-f6ba-43e0-836c-f8c652b77322', suppressed_alternatives_are_recoverable, empirically_contingent).
narrative_ontology:cs_reference_frame('83534048-f6ba-43e0-836c-f8c652b77322', market_as_historical_contingency).
narrative_ontology:cs_drift_state('83534048-f6ba-43e0-836c-f8c652b77322', late_twentieth_to_contemporary_neoliberalism, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('83534048-f6ba-43e0-836c-f8c652b77322', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(market_as_natural_default__lapsed_alternative_reading, economic_agents_contemporary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Document and recover evidence of alternative economic arrangements that existed historically but are no longer accessible to contemporary actors. Their research demonstrates that current market dominance was neither inevitable nor uncontested at earlier moments.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, economic_historians, observer,
    analytical, generational, analytical, global).

% Operate within a world where market coordination is the default frame and alternatives are invisible or unthinkable. They benefit from the coordination function of market price signals and property rules, but do not collectively maintain the naturalization — they inherit it as background.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, economic_agents_contemporary, beneficiary,
    organized, biographical, constrained, global).

% Analyze how earlier institutional arrangements (guild production, commons management, state provisioning, mixed tenure) were displaced or forgotten, leaving markets as the only remembered option.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, policy_scholars, observer,
    analytical, generational, analytical, global).

% Groups who would benefit from recovering suppressed alternatives (decentralized production networks, commons-based allocation, mutual aid) but lack access to the historical record or institutional resources to reconstruct them. Their benefit is unrealized, not extracted.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, memory_gap_beneficiaries, beneficiary,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_non_agent(market_as_natural_default__lapsed_alternative_reading, memory_gap_beneficiaries).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_as_natural_default__lapsed_alternative_reading, diffuse).
narrative_ontology:fixing_cost_class(market_as_natural_default__lapsed_alternative_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Market price signals coordinate dispersed knowledge of supply and demand without central planning; property rights allocate resources through voluntary exchange; this mechanism solves the knowledge problem for complex economies.
% TRANSFER_FUNCTION: The constraint transfers nothing; it is a cognitive/institutional frame that makes certain coordination mechanisms visible (markets, prices, individual choice) and others invisible (commons, non-monetary exchange, collective provision).
% ABSENT_VOICES: Practitioners of suppressed alternatives (guild workers, commons managers, cooperative economists, mutualist traditions) are absent because their institutional knowledge was not transmitted, not because they are actively excluded. Historical recovery and oral tradition communities would contest the inevitability frame if their voices were in the room.
% DISAPPEARANCE_RATIONALE: If market-as-default disappeared, contemporary coordination would not collapse because price signals and property rules are real mechanisms — but they would no longer be treated as the only thinkable option. Alternative arrangements could be experimentally recovered or reconstructed from historical evidence. The disappearance would be a reinstatement of remembered possibility, not chaos.
% FOUNDING_PROBLEM: Early modern economies needed a coordination mechanism that could operate at scale without feudal hierarchy or absolute monarchy direction — markets and exchange provided that mechanism.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and institutional economists (Polanyi, Ingham, Beckert, Granovetter) document that market coordination solved a genuine problem for 18th–19th-century expansion. The problem — coordination at industrial scale — persists, but is now conflated with the assumption that only market mechanisms solve it. No corroborating voice from outside the market economy exists in contemporary discourse, by construction: the alternatives are forgotten, not remembered and rejected.
narrative_ontology:disappearance_verdict(market_as_natural_default__lapsed_alternative_reading, contested).
narrative_ontology:founding_problem_status(market_as_natural_default__lapsed_alternative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__lapsed_alternative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(market_as_natural_default__lapsed_alternative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__lapsed_alternative_reading, 0.09, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__lapsed_alternative_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, ExtMetricName, E),
    domain_priors:suppression_score(market_as_natural_default__lapsed_alternative_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(market_as_natural_default__lapsed_alternative_reading),
    narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(market_as_natural_default__lapsed_alternative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely low (0.09) because under this reading, NO IDENTIFIABLE ACTOR actively collects from or maintains the naturalization. Contemporary agents benefit incidentally from market coordination (the genuine function), not from the forgetting itself. Suppression is equally low (0.12) because the forgetting is not actively enforced — alternatives are simply unavailable in institutional memory, not defended against. Theater is minimal (0.08) because there is no performative maintenance; the constraint just sits as unremarked background. Accessibility of alternatives is moderately high (0.72) because historical scholarship CAN recover what was forgotten — the alternatives are not structurally impossible, just unremembered. Resistance is very low (0.18) because there is little organized opposition to market dominance when alternatives are not remembered as options. The measurement series shows slight drift upward in theater and suppression as neoliberal ideology explicitly defends market naturalization (time points 16–40), suggesting that passive forgetting may be giving way to active maintenance — a signal that the constraint may be transitioning from this reading to the beneficiary_maintained_reading. This is exactly the kind of transition the corpus is built to detect.
 *
 * PERSPECTIVAL GAP:
 *   The analytical observer seat (historians, policy scholars) perceives the constraint as contingent and recoverable. Contemporary economic agents perceive it as background/inevitable. Beneficiary seats (if they exist in OTHER readings) would perceive it as the foundation of their power. This gap is not asymmetric extraction — it is a difference in information and framing. The engine computes per-seat classification; under this reading, the observer and agent seats should both compute as mountain or rope because no active extraction is occurring. The perspectival gap is epistemic, not structural.
 *
 * DIRECTIONALITY LOGIC:
 *   Under this reading, there is no clear beneficiary class because the naturalization is not actively maintained by anyone. Contemporary economic agents benefit from the COORDINATION function of markets (genuine, low-cost function), not from the forgetting. The forgetting itself is a side effect of institutional history, not something anyone collects from. Therefore, directionality is near symmetric or slightly beneficiary-leaning for all agents: they inherit a working coordination mechanism (benefit) with the incidental side effect of suppressed alternatives (cost), but the cost is not actively extracted. This differs radically from the beneficiary_maintained_reading, where powerful institutional actors would show high d-values (close to targets, extracting the benefit of the naturalization). The absence of a beneficiary class is STRUCTURAL to this reading and should prevent it from reclassifying as snare or tangled_rope unless new evidence shows that beneficiaries emerged and began to actively maintain the naturalization.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading has NO mandatrophy: the founding problem (coordination at industrial scale) is still live, the constraint still serves that function, and the persistence is explained by function, not by zombie institutional inertia. If the measurement series show a shift toward higher suppression_requirement and theater_ratio (which they do in time points 24–40), that shift would signal that the constraint may be transitioning toward a state where it requires active defense and selective forgetting to persist — which would be a transition from this reading (lapsed_alternative) toward the hybrid_amnesia or beneficiary_maintained readings. No mandatrophy exists in the lapsed_alternative reading itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_vs_passive_naturalization,
    'Is market dominance sustained by active post-hoc defense by beneficiaries, or by passive lapse of alternative institutional memory?',
    'Historical analysis of archival records, institutional defense rhetoric, and educational curriculum design: if market naturalization is actively defended when alternatives surface, it is not passive amnesia; if alternatives go undefended because they are simply unknown, it is lapsed memory.',
    'If active defense: the constraint reclassifies from mountain (natural law) to snare or tangled_rope (beneficiary-maintained extraction). If passive amnesia: the classification holds as mountain, but the foundational axiom shifts from ''market inevitability'' to ''market as forgotten contingency.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_vs_passive_naturalization, empirical, 'Whether naturalization is actively defended or passively inherited.').

omega_variable(
    alternative_recoverability,
    'Are suppressed alternatives genuinely unrecoverable, or merely forgotten and recoverable through historical scholarship and institutional experimentation?',
    'Documentation of successful reconstruction or revival of pre-market coordination mechanisms (commons, guild, cooperative, mutual aid networks) in contemporary contexts; evidence from historical scholarship of detailed institutional knowledge preserved but inaccessible.',
    'If alternatives are genuinely unrecoverable (institutional knowledge lost irretrievably), the constraint approaches mountain status more firmly. If recoverable but unremembered, the constraint sits at the boundary between mountain and a weak form of extraction: the forgetting itself may be functional for current beneficiaries even if not actively maintained.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_recoverability, empirical, 'Whether suppressed alternatives are structurally unrecoverable or merely forgotten.').

omega_variable(
    beneficiary_class_emergence,
    'Does market naturalization create an identifiable beneficiary class (beneficiaries who profit from the constraint''s persistence), or is the constraint purely a cognitive/historical artifact with diffuse incidental beneficiaries?',
    'Comparative institutional analysis: document whether contemporary actors (finance, corporate, state institutions) actively work to maintain market naturalization when alternatives are proposed, and whether their power position depends on the naturalization.',
    'If an identifiable beneficiary class emerges and actively maintains market naturalization, the constraint migrates from mountain to snare or tangled_rope (beneficiary_maintained_reading or hybrid_amnesia_reading). If only incidental benefits accrue and no class actively defends, the mountain classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_class_emergence, conceptual, 'Whether the constraint''s persistence is actively defended by beneficiaries or merely inherited as background.').

omega_variable(
    kernel_contest_framing_ambiguity,
    'Which reading of the ''market-as-natural-default'' kernel is the most parsimonious: that naturalization is a passive result of D3 forgetting (this reading), that identifiable beneficiaries actively defend it post-hoc (beneficiary_maintained_reading), or that initial forgetting creates enabling conditions for beneficiary capture (hybrid_amnesia_reading)?',
    'Process tracing of historical moments when market alternatives were explicitly debated and rejected (19th-century socialism, 1930s planning debates, 1960s–70s alternative economics movements): evidence of active defense vs. passive non-engagement.',
    'The three readings have distinct ε values: lapsed_alternative (this reading) ≤ 0.15; beneficiary_maintained ~0.50–0.70; hybrid_amnesia 0.35–0.55. Classification divergence between readings indicates where the kernel dispute is located structurally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_framing_ambiguity, conceptual, 'The relationship between the three readings of market naturalization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__lapsed_alternative_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement(mark_tr_t8, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 8, 0.04).
narrative_ontology:measurement(mark_tr_t16, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 16, 0.06).
narrative_ontology:measurement(mark_tr_t24, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 24, 0.08).
narrative_ontology:measurement(mark_tr_t32, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 32, 0.08).
narrative_ontology:measurement(mark_tr_t40, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 40, 0.08).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 0, 0.04).
narrative_ontology:measurement(mark_be_t8, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 8, 0.06).
narrative_ontology:measurement(mark_be_t16, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 16, 0.08).
narrative_ontology:measurement(mark_be_t24, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 24, 0.08).
narrative_ontology:measurement(mark_be_t32, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 32, 0.09).
narrative_ontology:measurement(mark_be_t40, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 40, 0.09).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(mark_su_t8, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 8, 0.08).
narrative_ontology:measurement(mark_su_t16, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 16, 0.1).
narrative_ontology:measurement(mark_su_t24, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 24, 0.11).
narrative_ontology:measurement(mark_su_t32, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 32, 0.12).
narrative_ontology:measurement(mark_su_t40, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 40, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__lapsed_alternative_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_as_natural_default__lapsed_alternative_reading, 0.12).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default__hybrid_amnesia_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel about market naturalization. The kernel 'market-as-natural-default' admits three structurally distinct readings with different ε values, stakeholder structures, and beneficiary claims. This reading (lapsed_alternative) posits naturalization as passive D3 forgetting with no active beneficiary maintenance, yielding ε ≤ 0.15 and mountain classification. The sibling readings (beneficiary_maintained and hybrid_amnesia) argue for active defense and beneficiary consolidation, yielding substantially higher ε. The three are linked via network.affects_constraints because changes in historical scholarship (e.g., recovery of suppressed alternatives) and political discourse (e.g., emergence of benefit claimants) can shift which reading is most empirically adequate. Process tracing of moments when alternatives were explicitly debated (19th-century socialism, 1930s planning, 1960s counterculture) will disambiguate the readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
