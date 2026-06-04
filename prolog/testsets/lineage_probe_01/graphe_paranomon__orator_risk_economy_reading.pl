% ============================================================================
% CONSTRAINT STORY: graphe_paranomon__orator_risk_economy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_graphe_paranomon__orator_risk_economy_reading, []).

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
 *   constraint_id: graphe_paranomon__orator_risk_economy_reading
 *   human_readable: Graphe Paranomon: Orator Risk Economy Reading
 *   domain: legal/doctrinal/ancient_athenian_governance
 *
 * SUMMARY:
 *   The graphe paranomon (action against unlawful decree) created a
 *   distinctive Athenian mechanism: every proposal brought to the assembly
 *   carried personal liability for the proposer. If the proposal was later
 *   challenged as paranomon (contrary to existing law or constitutional
 *   principle), the proposer faced prosecution, potential fines, and
 *   reputational damage. This reading instantiates the constraint as a risk
 *   economy: the mechanism priced political voice by making the proposer bear
 *   the cost of challenge. Wealth determined capacity to absorb that cost —
 *   wealthy and confident orators could afford prosecution and factional
 *   protection; cautious and non-wealthy citizens faced suppression through
 *   fear. The mechanism selected for speakers and suppressed others, creating
 *   an aristocratic filtering effect on democratic participation. This
 *   reading focuses on the extractive dimension: how the liability structure
 *   transferred resources (time, money, risk-bearing) from cautious proposers
 *   to wealthy-confident ones, and how it deterred initiative from those who
 *   could not afford the personal bond. The suppression is not incidental —
 *   it is structural to the mechanism. The rival readings emphasize
 *   self-binding (the assembly's own decrees challengeable as a
 *   constitutional safeguard) and factional weaponization (prosecution as
 *   continuation of political competition). This reading does not deny those
 *   dynamics but interprets the primary extractive effect as wealth-dependent
 *   voice suppression.
 *
 * KEY AGENTS:
 *   - Wealthy Confident Orators: Primary beneficiary (powerful/arbitrage) — their ability to absorb liability makes them the trusted proposers; they monopolize the rostrum.
 *   - Cautious Citizens: Primary victim (powerless/trapped) — fear of personal liability deters them from proposing. Suppressed by the mechanism's design.
 *   - Non-Wealthy Initiative Seekers: Secondary victim (moderate/constrained) — theoretically free to propose but face compounded barriers (financial exposure without reserves, factional vulnerability, reputational fragility).
 *   - Established Aristocratic Factions: Secondary beneficiary (organized/constrained) — the risk structure enables them to coordinate internally (serious proposals only) while extracting from rival factions through prosecution threats.
 *   - The Assembly Collective: Structural target (analytical/analytical) — the mechanism frames collective deliberation as requiring speaker accountability, but the accountability is priced by wealth.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(graphe_paranomon__orator_risk_economy_reading, 0.58).
domain_priors:suppression_score(graphe_paranomon__orator_risk_economy_reading, 0.72).
domain_priors:theater_ratio(graphe_paranomon__orator_risk_economy_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(graphe_paranomon__orator_risk_economy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(graphe_paranomon__orator_risk_economy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(graphe_paranomon__orator_risk_economy_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(graphe_paranomon__orator_risk_economy_reading, snare).
narrative_ontology:human_readable(graphe_paranomon__orator_risk_economy_reading, "Graphe Paranomon: Orator Risk Economy Reading").
narrative_ontology:topic_domain(graphe_paranomon__orator_risk_economy_reading, "legal/doctrinal/ancient_athenian_governance").

domain_priors:requires_active_enforcement(graphe_paranomon__orator_risk_economy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(graphe_paranomon__orator_risk_economy_reading, 'c6e50dbd-3ef7-4799-9ee5-0e32d33a6410').
narrative_ontology:cs_kernel_codification('c6e50dbd-3ef7-4799-9ee5-0e32d33a6410', formalized).
narrative_ontology:cs_authority_grounding('c6e50dbd-3ef7-4799-9ee5-0e32d33a6410', lineage).
narrative_ontology:cs_interpretation_layer_present('c6e50dbd-3ef7-4799-9ee5-0e32d33a6410').
narrative_ontology:cs_reading_relation('c6e50dbd-3ef7-4799-9ee5-0e32d33a6410', graphe_paranomon__self_binding_mechanism_reading, coexists_with).
narrative_ontology:cs_reading_relation('c6e50dbd-3ef7-4799-9ee5-0e32d33a6410', graphe_paranomon__weapon_of_faction_reading, coexists_with).
narrative_ontology:cs_axiom('c6e50dbd-3ef7-4799-9ee5-0e32d33a6410', foundational, voice_pricing_by_liability_creates_wealth_based_suppression).
narrative_ontology:cs_axiom_status(voice_pricing_by_liability_creates_wealth_based_suppression, holdable).
narrative_ontology:cs_axiom_grounding('c6e50dbd-3ef7-4799-9ee5-0e32d33a6410', voice_pricing_by_liability_creates_wealth_based_suppression, empirically_contingent).
narrative_ontology:cs_axiom('c6e50dbd-3ef7-4799-9ee5-0e32d33a6410', foundational, rostrum_access_correlates_with_capacity_to_bear_personal_risk).
narrative_ontology:cs_axiom_status(rostrum_access_correlates_with_capacity_to_bear_personal_risk, holdable).
narrative_ontology:cs_axiom_grounding('c6e50dbd-3ef7-4799-9ee5-0e32d33a6410', rostrum_access_correlates_with_capacity_to_bear_personal_risk, empirically_contingent).
narrative_ontology:cs_reference_frame('c6e50dbd-3ef7-4799-9ee5-0e32d33a6410', wealth_independent_voice_access).
narrative_ontology:cs_drift_state('c6e50dbd-3ef7-4799-9ee5-0e32d33a6410', mature_graphe_paranomon_tradition, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c6e50dbd-3ef7-4799-9ee5-0e32d33a6410', '').
narrative_ontology:cs_kernel_id(graphe_paranomon__orator_risk_economy_reading, graphe_paranomon).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(graphe_paranomon__orator_risk_economy_reading, wealthy_confident_orators).
narrative_ontology:constraint_beneficiary(graphe_paranomon__orator_risk_economy_reading, established_aristocratic_factions).
narrative_ontology:constraint_victim(graphe_paranomon__orator_risk_economy_reading, cautious_proposers).
narrative_ontology:constraint_victim(graphe_paranomon__orator_risk_economy_reading, risk_averse_citizens).
narrative_ontology:constraint_victim(graphe_paranomon__orator_risk_economy_reading, non_wealthy_initiative_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE CAUTIOUS CITIZEN (SNARE) — Cannot propose without bearing personal liability. Faces maximum suppression: fear of prosecution, financial exposure, reputational damage. No exit option. Experiences pure extraction — the threat disciplines them into silence while wealthy rivals monopolize the rostrum. The mechanism selects against their voice entirely.
constraint_indexing:constraint_classification(graphe_paranomon__orator_risk_economy_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE WEALTHY ORATOR (ROPE) — Carries personal liability but experiences it as coordination mechanism. The risk bond ensures serious deliberation (culpability deters frivolous proposals). Can afford prosecution and has allies to defend; arbitrage available (legal and factional protection). Experiences the constraint as enforcing collective deliberative norms — their ability to absorb the cost is what makes them trustworthy proposers. Net beneficiary.
constraint_indexing:constraint_classification(graphe_paranomon__orator_risk_economy_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: THE FACTIONAL COALITION (TANGLED ROPE) — Organized rival factions coordinate through the graphe paranomon mechanism. Each faction coordinates internally (the risk bond ensures only serious proposals advance within the faction) while extracting from competing factions (the threat of prosecution is weaponized). Mixed coordination (internal discipline) and extraction (factional competition). Constrained exit because leaving the factional structure means losing protection from prosecution.
constraint_indexing:constraint_classification(graphe_paranomon__orator_risk_economy_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER / NATURALIZATION VIEW (MOUNTAIN) — From a civilizational perspective, the risk bond appears as an immutable property of collective decision-making: any assembly needs to ensure proposals are serious, and liability is the natural lever for that screening. This reading naturalizes the mechanism as inherent democratic architecture. The engine will identify this as a false summit candidate: the structural data reveals the 'natural screening' framing conceals wealth-dependent access to the rostrum.
constraint_indexing:constraint_classification(graphe_paranomon__orator_risk_economy_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: THE NON-WEALTHY INITIATIVE SEEKER (SNARE) — Can theoretically propose but faces compounded barriers: personal liability (financial exposure without reserves), factional vulnerability (no allies to provide legal defense), reputational fragility (a failed prosecution claim creates lasting damage to non-wealthy status). Constrained in theory but effectively trapped in practice. Experiences the mechanism as weaponized suppression.
constraint_indexing:constraint_classification(graphe_paranomon__orator_risk_economy_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(graphe_paranomon__orator_risk_economy_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(graphe_paranomon__orator_risk_economy_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(graphe_paranomon__orator_risk_economy_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(graphe_paranomon__orator_risk_economy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(graphe_paranomon__orator_risk_economy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The mechanism transfers political voice from risk-averse to risk-tolerant speakers, and that transfer correlates with wealth. The extractiveness is not total (the mechanism is not a pure wealth tax) but it is substantial — it redistributes political access and opportunity. The measurement trajectory (0.45 → 0.58 → 0.68) shows that as Athens accumulated legal precedents, the risk to proposers increased (more ways a decree could be challenged as paranomon), and the suppressive effect intensified. Suppression (0.72): High. The barrier to cautious proposers is substantial: personal financial liability, prosecution risk, reputational damage, factional vulnerability. The mechanism works through suppressing alternatives (silence becomes safer than speaking). The trajectory (0.62 → 0.72 → 0.78) shows that suppression increased as the graphe paranomon tradition matured and more decrees faced challenge. Theater ratio (0.35): Low-moderate. The mechanism is functionally focused — it genuinely deters frivolous proposals and ensures serious deliberation. The proposer's personal stake creates real accountability, not theatrical appearance. The low theater reflects that this is not a piton (degraded ritual); it is an active extractive mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The wealthy orator experiences coordination (rope) — their liability is the price of being trusted with power. The cautious citizen experiences extraction (snare) — their silence is the suppression the mechanism extracts. The factional coalition experiences mixed dynamics (tangled rope) — internal coordination discipline paired with external factional weaponization. The analytical observer from outside risks naturalizing the mechanism as inherent democratic architecture (mountain) — but the structural data reveals this as a false summit: the suppression is not natural law but wealth-correlated selection. The gap between rope and snare perspectives reveals the constraint's core structure: the same liability rule produces opposite effects depending on whether you can afford to bear it.
 *
 * DIRECTIONALITY LOGIC:
 *   The mechanism's directionality derives from beneficiary/victim declarations and exit structure. Wealthy orators have arbitrage exit (legal connections, factional protection, ability to absorb costs) paired with beneficiary status (they monopolize the rostrum), yielding low d → low/negative experienced extraction. Cautious citizens have trapped exit (no legal protection, no financial buffers, reputational vulnerability) paired with victim status (they are suppressed), yielding high d → high experienced extraction. Organized factions have constrained exit (leaving the faction structure means losing protection) paired with mixed beneficiary-victim status (they coordinate internally and extract from rivals), yielding moderate d → moderate experienced extraction. The directional asymmetry is fundamental: the mechanism looks like coordination to those who can afford it (rope) and extraction to those who cannot (snare).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy by identifying extractiveness as wealth-dependent voice suppression rather than pure coordination (self-binding) or factional weaponization. The snare classification derives from the suppression of cautious proposers and the barrier created by personal liability — this is not coordination with side effects, it is extraction with a coordination facade. The tangled rope classification for organized factions captures the genuine mixed structure: they do coordinate internally, but they extract from rivals through the same mechanism. The rope classification for wealthy orators reflects their genuine experience of the mechanism as deliberative accountability rather than suppression. The false summit mountain classification reveals the danger of naturalizing the mechanism as inherent democratic necessity — this frames wealth-correlated suppression as a structural law of governance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liability_asymmetry_intentional_or_artifact,
    'Is the suppression of cautious proposers an intentional design feature of the graphe paranomon, or an unintended consequence of risk-neutral liability allocation that happens to correlate with wealth?',
    'Constitutional debates about the graphe paranomon''s purpose; comparison with alternatives that Athens could have adopted (wealth-tiered liability, collective insurance mechanisms, guild-based proposal systems); analysis of whether early Athenian lawgivers explicitly discussed deterrence of cautious speakers',
    'If intentional: the constraint is a snare by design — the suppression is extractive, benefiting wealthy-confident speakers. If artifact: the constraint is more like a scaffold (temporary coordination mechanism with unintended side effects) — the extraction could be remedied by redistributing liability. If intentional: supports this reading''s classification. If artifact: suggests the self_binding_mechanism_reading is closer to the architects'' intent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(liability_asymmetry_intentional_or_artifact, empirical, 'Whether liability suppression of cautious proposers was intentional design or unintended consequence').

omega_variable(
    factional_prosecution_patterns,
    'Do prosecution patterns under graphe paranomon show random judicial outcomes (suggesting genuine collective deliberation), or systematic victory for proposals from wealthy-backed factions (suggesting weaponized use)?',
    'Statistical analysis of surviving graphe paranomon cases: win rates by faction affiliation, socioeconomic status of successful vs prosecuted orators, temporal clustering of prosecutions around factional competitions',
    'If random outcomes: mechanism supports genuine deliberative screening (rope or tangled_rope). If systematic faction-dependent outcomes: mechanism is weaponized suppression (snare from the non-wealthy perspective). The weapon_of_faction_reading predicts systematic patterns; this reading predicts suppression correlates with wealth rather than faction success rate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(factional_prosecution_patterns, empirical, 'Whether prosecution outcomes correlate with wealth or factional dynamics').

omega_variable(
    comparative_cost_of_proposal_entry,
    'How does the cost of proposing under graphe paranomon (liability + prosecution risk + legal fees) compare to the wealth threshold below which a citizen cannot absorb these costs?',
    'Reconstruction of Athenian wealth distribution; estimation of typical prosecution costs and personal liability exposure from fragmentary sources; identification of the wealth percentile at which proposal-bearing becomes personally risky',
    'If cost ≈ median citizen income: most citizens are effectively suppressed. If cost ≈ top 10% income: mechanism selects for wealthy but not as severely. Determines whether suppression value of 0.72 is accurate or underestimated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(comparative_cost_of_proposal_entry, empirical, 'Cost threshold of proposing relative to wealth distribution').

omega_variable(
    alternative_framings_coexist_in_tradition,
    'Do Athenian sources (Aristotle, orators, inscriptions) themselves oscillate between the three readings (risk economy, self-binding, factional weapon), or does the tradition consistently endorse one reading?',
    'Textual analysis of ancient sources discussing graphe paranomon''s purpose and effects; examination of whether different sources or different historical periods emphasize different justifications',
    'If sources consistently endorse risk-economy framing: this reading reflects the tradition''s own self-understanding. If sources oscillate or emphasize self-binding/factional dynamics: the readings coexist as live interpretive options within the Athenian tradition itself. Affects whether ''forecloses'' relations are warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_framings_coexist_in_tradition, conceptual, 'Whether tradition consistently endorses one reading or oscillates among competing framings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(graphe_paranomon__orator_risk_economy_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(grap_tr_t0, graphe_paranomon__orator_risk_economy_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(grap_tr_t50, graphe_paranomon__orator_risk_economy_reading, theater_ratio, 50, 0.35).
narrative_ontology:measurement(grap_tr_t100, graphe_paranomon__orator_risk_economy_reading, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(grap_be_t0, graphe_paranomon__orator_risk_economy_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(grap_be_t50, graphe_paranomon__orator_risk_economy_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(grap_be_t100, graphe_paranomon__orator_risk_economy_reading, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(grap_su_t0, graphe_paranomon__orator_risk_economy_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(grap_su_t50, graphe_paranomon__orator_risk_economy_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement(grap_su_t100, graphe_paranomon__orator_risk_economy_reading, suppression_requirement, 100, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(graphe_paranomon__orator_risk_economy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(graphe_paranomon__orator_risk_economy_reading, graphe_paranomon__self_binding_mechanism_reading).
narrative_ontology:affects_constraint(graphe_paranomon__orator_risk_economy_reading, graphe_paranomon__weapon_of_faction_reading).

% DUAL FORMULATION NOTE:
% The graphe paranomon is a contested kernel with three structurally distinct constraint readings. This story (orator_risk_economy_reading) interprets the mechanism as creating extractive suppression of cautious voices through risk-bearing liability. The self_binding_mechanism_reading interprets it as constitutional self-restraint. The weapon_of_faction_reading interprets it as factional competition. All three readings are linked via the common kernel. Each has its own ε value and its own beneficiary/victim structure. The readings coexist as live interpretive options within Athenian constitutional tradition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
