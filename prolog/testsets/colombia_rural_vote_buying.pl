% ============================================================================
% CONSTRAINT STORY: colombia_rural_vote_buying
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_colombia_rural_vote_buying, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: colombia_rural_vote_buying
 *   human_readable: Rural Vote Buying in Colombian Elections
 *   domain: political_economy/electoral_capture
 *
 * SUMMARY:
 *   Rural vote buying in Colombia operates as a mechanism of political
 *   extraction embedded within a system of material dependency and geographic
 *   isolation. Small-holder farmers and rural laborers, lacking access to
 *   credit, healthcare, and public services through formal state channels,
 *   depend on local political patrons for survival-critical goods and
 *   services. During election cycles, patrons convert this dependency into
 *   electoral control through direct payments, material gifts, and
 *   conditional access to services. The mechanism creates a structural bind:
 *   voters cannot refuse without risking loss of patron access to essential
 *   resources, yet the transaction is typically framed as voluntary
 *   reciprocity or political support. The constraint exhibits the full
 *   taxonomy of extraction types depending on observer position: pure snare
 *   from the powerless voter's perspective; tangled rope from the patron's
 *   perspective (genuine coordination of local resource mobilization
 *   alongside coercive extraction); scaffold from the institutional
 *   reformer's perspective (temporary problem solvable by strengthening state
 *   service delivery and electoral monitoring); piton from the constitutional
 *   court's perspective (formal prohibition with weak enforcement). The
 *   theater ratio (0.55) reflects the gap between the formal legal
 *   prohibition of vote buying and the practical persistence of the mechanism
 *   in regions where state capacity remains weak and alternative survival
 *   mechanisms absent.
 *
 * KEY AGENTS:
 *   - Rural voters (small-holder farmers, agricultural laborers): Primary victims (powerless/trapped or identity_locked) — bear the extraction directly through loss of political autonomy while experiencing material benefit
 *   - Political patrons (local candidates, party operatives, regional elites): Primary beneficiaries (organized/constrained) — aggregate votes and political power through the mechanism, constrained by dependence on maintaining clientelist networks
 *   - Rural communities (as collective identity): Secondary victims (organized/identity_locked) — internalize vote buying as normal political reciprocity, making exit cognitively difficult despite structural mobility improvements
 *   - Electoral authority (CNE and departmental electoral bodies): Institutional reformer (organized/constrained) — attempts to build monitoring capacity and alternative service delivery mechanisms with finite budget
 *   - State welfare apparatus (departmental health, education, social programs): Institutional ally (institutional/arbitrage) — decouples survival needs from political conditionality, reducing patron monopoly
 *   - Constitutional Court: Institutional degradation (institutional/arbitrage) — maintains formal prohibition with declining enforcement capacity (piton characteristic)
 *   - Analytical observer: Sees pure extraction mechanism with no coordination function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(colombia_rural_vote_buying, 0.68).
domain_priors:suppression_score(colombia_rural_vote_buying, 0.75).
domain_priors:theater_ratio(colombia_rural_vote_buying, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(colombia_rural_vote_buying, extractiveness, 0.68).
narrative_ontology:constraint_metric(colombia_rural_vote_buying, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(colombia_rural_vote_buying, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(colombia_rural_vote_buying, snare).
narrative_ontology:human_readable(colombia_rural_vote_buying, "Rural Vote Buying in Colombian Elections").
narrative_ontology:topic_domain(colombia_rural_vote_buying, "political_economy/electoral_capture").

domain_priors:requires_active_enforcement(colombia_rural_vote_buying).
% --- Structural relationships ---
narrative_ontology:constraint_victim(colombia_rural_vote_buying, rural_voters).
narrative_ontology:constraint_victim(colombia_rural_vote_buying, electoral_legitimacy).
narrative_ontology:constraint_victim(colombia_rural_vote_buying, democratic_accountability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL VOTER (SNARE) — Trapped by material poverty, geographic isolation, and absence of viable alternatives. The voter faces immediate survival pressures (food security, medical access, credit access) and has zero capacity to exit. Vote buying preys on this structural immobility. Maximum experienced extraction.
constraint_indexing:constraint_classification(colombia_rural_vote_buying, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: RURAL COMMUNITY (SNARE, IDENTITY-LOCKED) — Structurally mobile (some members could migrate; some could organize) but identity-locked into accepting vote buying as 'how politics works.' Internalized framing treats electoral exchange as normal reciprocity rather than extractive capture. The community's identity as 'loyal to the patron' makes exit unthinkable even where material barriers have weakened. High suppression through cognitive capture.
constraint_indexing:constraint_classification(colombia_rural_vote_buying, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 3: POLITICAL MACHINE (TANGLED ROPE) — Benefits from vote aggregation and reduced electoral uncertainty. Coordinates voter mobilization and resource distribution — genuine coordination function exists. But coordination is built on coercion: voters cannot refuse without losing access to critical services. Asymmetric extraction embedded within the coordination mechanism. The machine constrains its own exit (cannot abandon clientelist networks without losing electoral base).
constraint_indexing:constraint_classification(colombia_rural_vote_buying, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ELECTORAL AUTHORITY (SCAFFOLD) — Attempts to build alternative verification mechanisms: voter verification systems, observer networks, digital voting, direct service delivery decoupled from voting. These reforms target a sunset — as rural economies develop and direct service provision improves, vote-buying loses mechanism. But the authority itself is constrained by limited budget, geographic coverage gaps, and resistance from entrenched machines. High suppression during the transition window.
constraint_indexing:constraint_classification(colombia_rural_vote_buying, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE WELFARE APPARATUS (ROPE) — Benefits from centralized delivery of services (healthcare, education, rural infrastructure) that reduces the patron's monopoly on local survival goods. Pure coordination function with minimal extraction — the state coordinates the distribution of public goods. High arbitrage capacity (state can withdraw and redeploy resources). Vote buying succeeds precisely where state welfare is weak; improves where state provision strengthens.
constraint_indexing:constraint_classification(colombia_rural_vote_buying, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: CONSTITUTIONAL COURT (PITON) — Maintains theoretical prohibition on vote buying and clientelism through case law and declarations. The mechanism is largely performative — constitutional rulings rarely penetrate rural zones where enforcement capacity is absent and alternative dispute resolution (patronage networks) has higher legitimacy. Court maintains ritual of legality while the actual mechanism persists. Theater ratio 0.55 reflects this performance-reality gap.
constraint_indexing:constraint_classification(colombia_rural_vote_buying, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — Vote buying is a pure extraction mechanism: the voter receives material benefit but surrenders political autonomy with no coordination function that requires this surrender. The mechanism works *because* it separates material necessity (food, credit, medicine) from political choice, then weaponizes that separation. No genuine coordination problem requires vote buying — it persists because it redistributes power from the many (voters) to the few (patrons and elites). The analytical classification is unambiguous: Snare.
constraint_indexing:constraint_classification(colombia_rural_vote_buying, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(colombia_rural_vote_buying_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(colombia_rural_vote_buying, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(colombia_rural_vote_buying, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(colombia_rural_vote_buying, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(colombia_rural_vote_buying, TR),
    TR >= 0.70.

:- end_tests(colombia_rural_vote_buying_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Vote buying is a mechanism of transferring electoral labor (the voter's political choice) to the patron in exchange for material benefit. The extraction is severe because the voter faces material necessity and has no alternative mechanism to access critical goods. Extractiveness increased from 0.52 to 0.68 over the interval (2000-2020) as rural marginalization deepened and state service provision failed to expand into isolated departments — the extraction became *more* severe even as formal incomes nominally improved, because welfare coverage gaps persisted. Suppression (0.75): Very high. Structural barriers include: geographic isolation reducing information access and alternative employment; poverty creating dependency on patron-controlled credit and services; limited education reducing capacity to organize alternative mechanisms; weak state enforcement making patronage networks more reliable than formal institutions; and cultural-historical normalization of clientelism. Suppression is not total (some rural voters do refuse, some communities have broken patron dependence) but remains near-maximum. Theater ratio (0.55): Moderate-high. The constitutional prohibition on vote buying is widely known but functionally absent in enforcement. Media coverage, court cases, and election observer reports create performance of legality while the mechanism persists. Theater increased over the interval as formal anti-corruption rhetoric expanded while actual enforcement remained capacity-constrained.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a fundamental DR insight: the same extraction mechanism appears as different types from different positions. The powerless rural voter and the identity-locked rural community both experience snare-level extraction but through different binding mechanisms (material vs cognitive). The patron experiences tangled rope because they genuinely coordinate resource mobilization while simultaneously extracting votes. The electoral reformer experiences scaffold because they see institutional pathways toward terminating the mechanism. The constitutional court experiences piton because the formal mechanism is performative (the court makes rulings; the mechanism persists). The analytical observer sees snare because they see no genuine coordination function — the mechanism could be replaced by state service provision without losing any functionality. These are not measurement disagreements (all observers agree on the extraction magnitude). They are structural disagreements about what the constraint *is*: immutable natural law, temporary coordination failure, extractive coercion, or institutional theater. The perspectival gap is the core analytical value — it reveals that a single structural phenomenon can be simultaneously all six constraint types depending on who is measuring.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation chain operates on explicit structural data: each perspective declares who benefits (beneficiaries) and who bears costs (victims), then applies the power atom and exit option to compute d. Rural voters are explicitly victims (trapped exit) producing d ≈ 0.92 and f(d) ≈ 1.42, yielding high chi. Patrons are explicitly beneficiaries (constrained exit) producing d ≈ 0.35 and f(d) ≈ 0.40, yielding low chi relative to their nominal power. Electoral authority is constrained but neither pure beneficiary nor victim — the structural data produces d ≈ 0.45 (moderate) reflecting mixed position. Identity-locked rural community derives d from victim status + identity_locked exit ≈ 0.89, producing f(d) ≈ 1.28 — higher than trapped because the binding is cognitive (could break with identity shift) but still near-maximum because cognitive escape is as difficult as material escape from within the frame. The analytical observer gets d ≈ 0.72 (moderate-high) reflecting that the observer is orthogonal to the primary extraction but not embedded within it, producing moderate experienced extractiveness. The directionality logic captures why the constraint persists: beneficiaries and constrained institutional actors experience manageable extraction levels and see reform as disrupting useful coordination; trapped and identity_locked victims experience maximum extraction but lack power to reform; the analytical observer sees snare-level extraction but typically has no direct influence on rural electoral systems.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the classification paradox by showing that vote buying is a pure snare from the analytical/powerless perspectives but a genuine tangled rope from the patron's perspective. The mandatrophy would incorrectly ask 'is this REALLY coordination or REALLY extraction?' The DR answer is: it is both, depending on the observer's structural position. For the rural voter, it is pure extraction (snare) because they face coercive necessity. For the patron, it is coordination + extraction (tangled rope) because they are genuinely solving the collective action problem of resource mobilization in dispersed rural areas while simultaneously extracting votes. For the electoral reformer, it is a temporary problem with a sunset (scaffold) because welfare-state development and electoral monitoring can obsolete the mechanism. No single answer is 'wrong' — each is the true classification from that structural position. The framework prevents mandatrophy by insisting that the observer's power, exit capacity, and time horizon MUST be part of the classification, not held constant. The snare classification (primary) holds when power = powerless and exit = trapped. The tangled rope classification (secondary) holds when power = organized and exit = constrained. Both are DR-valid; neither falsifies the other. The mandatrophy is resolved by acknowledging that a pure snare and a genuine tangled rope can be the same structural mechanism viewed from opposite ends of the extraction flow.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rural_economy_development_timeline,
    'At what level of rural income per capita and welfare state service coverage does vote buying become unsustainable as a political mechanism?',
    'Longitudinal analysis of vote-buying prevalence in Colombian departments correlated with departmental income growth, rural school enrollment, healthcare coverage, and pension expansion. Cross-country comparison with Brazil, Mexico, Philippines where similar transitions occurred.',
    'If threshold is low (e.g., $3,000 per capita): scaffold sunset is near-term, mechanism will collapse within one generation. If threshold is high (e.g., >$8,000): vote buying persists despite nominal development, revealing identity-lock persistence beyond material necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rural_economy_development_timeline, empirical, 'Income/welfare threshold for vote-buying sustainability').

omega_variable(
    identity_lock_vs_material_trap,
    'Is rural acceptance of vote buying driven by material immobility (trapped) or by cognitive internalization that survives improved material conditions (identity_locked)?',
    'Panel study comparing voting behavior in rural communities that experienced welfare improvements (pension expansion, rural electrification) vs control communities. Measurement: persistence of vote-buying correlations after controlling for income changes. Qualitative interviews probing self-narratives about voting autonomy.',
    'If predominantly material (trapped): welfare expansion reduces prevalence sharply. If predominantly identity-locked: welfare expansion shows weak impact; political education and norm-shifting are necessary. True answer likely mixed — measuring the proportion determines intervention strategy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_material_trap, empirical, 'Relative weight of material vs cognitive binding mechanisms').

omega_variable(
    alternative_patron_functions,
    'Does the political patron provide genuine public goods (dispute resolution, collective action coordination) that would be lost under electoral reform, or is the patron purely extractive?',
    'Institutional analysis of patron-provided services in areas with strong clientelism: adjudication, infrastructure coordination, group lending, public safety. Comparison with patron-weak areas where state/market provides equivalents. Measurement: voter satisfaction with public goods access before/after patron relationship dissolution.',
    'If patron provides genuine coordination: electoral reform must replace patron functions, else scaffold sunset fails. If purely extractive: electoral reform only requires removal of coercive mechanism, enabling swift transition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_patron_functions, empirical, 'Whether patron relationship provides genuine coordination function').

omega_variable(
    observer_position_bias,
    'Does the analytical observer''s classification as ''pure Snare'' depend on the observer''s own position outside the vote-buying network, or would a participant observer nested in the patronage system see genuine coordination benefits?',
    'Ethnographic research recording beneficiary and patron justifications for vote-buying arrangement; interviews with past patrons and reformed voters; analysis of peasant organizational literature from periods of patronage breakdown.',
    'If the snare classification persists from participant perspective: confirms pure extraction (mountain-quality snare signal). If participants perceive coordination: reveals that snare/tangled_rope distinction depends on observer position — the extractive frame is observer-dependent rather than observer-invariant.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(observer_position_bias, conceptual, 'Whether snare classification is observer-invariant or position-relative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(colombia_rural_vote_buying, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(crvb_tr_t0, colombia_rural_vote_buying, theater_ratio, 0, 0.38).
narrative_ontology:measurement(crvb_tr_t10, colombia_rural_vote_buying, theater_ratio, 10, 0.48).
narrative_ontology:measurement(crvb_tr_t20, colombia_rural_vote_buying, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(crvb_be_t0, colombia_rural_vote_buying, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(crvb_be_t10, colombia_rural_vote_buying, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(crvb_be_t20, colombia_rural_vote_buying, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(colombia_rural_vote_buying, resource_allocation).
narrative_ontology:boltzmann_floor_override(colombia_rural_vote_buying, 0.2).
narrative_ontology:affects_constraint(colombia_rural_vote_buying, colombian_rural_poverty).
narrative_ontology:affects_constraint(colombia_rural_vote_buying, state_capacity_deficit).
narrative_ontology:affects_constraint(colombia_rural_vote_buying, informal_credit_dependency).

% DUAL FORMULATION NOTE:
% Vote buying is downstream of rural economic marginalization and upstream of electoral legitimacy degradation. The constraint family includes: rural_poverty (ε≈0.15, mountain) — poverty itself as immutable structural condition, vote_buying (ε≈0.68, snare) — the specific extraction mechanism preying on poverty, and electoral_legitimacy (ε≈0.42, tangled_rope) — the broader political system coupling. Vote buying's extractiveness increased as state capacity failed to expand into rural departments, making the poverty constraint more binding and the vote-buying mechanism more severe. Welfare-state development would operate as an upstream intervention on the rural_poverty constraint, automatically weakening the vote_buying mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(colombia_rural_vote_buying, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
