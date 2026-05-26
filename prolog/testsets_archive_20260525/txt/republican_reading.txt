% ============================================================================
% CONSTRAINT STORY: republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_republican_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: republican_reading
 *   human_readable: Republican Legitimacy via Electoral Mandate
 *   domain: political_philosophy/constitutional_authority
 *
 * SUMMARY:
 *   The republican reading of sovereign legitimacy grounds state authority in
 *   electoral mandate: rulers are selected by and remain accountable to the
 *   governed through periodic elections. This constraint operates at the
 *   intersection of coordination (solving the succession problem) and
 *   extraction (majority rule overrides minorities, present overrides future,
 *   institutional actors accumulate power during term). The republican
 *   mechanism solves the collective action problem of selecting rulers
 *   without dynastic succession or civil war — a genuine coordination
 *   function. Simultaneously, the mechanism enables systematic extraction:
 *   the electoral majority dictates policy to minorities, the present
 *   electorate commits future generations, and executive actors consolidate
 *   power between elections while claiming ongoing mandate. The constraint's
 *   theater ratio (0.48) reflects moderate performative content: electoral
 *   campaigns involve theatrical mobilization and mandate claims, but the
 *   mechanism retains genuine functional elements (elections do remove failed
 *   rulers, electoral pressure does shift policy). The extractiveness
 *   trajectory (0.35 → 0.58 over a 4-year cycle) captures power accumulation
 *   during a term of office: initial election establishes mandate with lower
 *   de facto extraction, accumulation occurs as executive consolidates
 *   administrative power, and the trajectory resets if electoral renewal
 *   occurs (or entrenches if renewal is subverted).
 *
 * KEY AGENTS:
 *   - Electoral Majority Coalition: Primary beneficiary (institutional/arbitrage) — preferences translated into policy; experiences republican mechanism as coordination
 *   - Electoral Minorities: Primary victim (powerless/trapped) — structurally overridden; zero exit options; highest suppression
 *   - Future Generations: Secondary victim (powerless/trapped) — temporally excluded from electoral process that binds them; future-discounting enables present extraction
 *   - Swing Voters / Marginal Coalition Members: Mixed (moderate/constrained) — benefit from coordination while bearing extraction and mobilization costs
 *   - State Executive / Delegated Authority: Institutional actor (institutional/constrained) — receives legitimacy from electoral mandate but constrained by it; incentivized to entrench power during term
 *   - Constitutional Order: Institutional perspective (institutional/arbitrage) — maintains republican system through inertia and theater; resists alternatives
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees authentic hybrid structure without naturalizing to pure law or pure extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(republican_reading, 0.52).
domain_priors:suppression_score(republican_reading, 0.58).
domain_priors:theater_ratio(republican_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(republican_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(republican_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(republican_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(republican_reading, tangled_rope).
narrative_ontology:human_readable(republican_reading, "Republican Legitimacy via Electoral Mandate").
narrative_ontology:topic_domain(republican_reading, "political_philosophy/constitutional_authority").

domain_priors:requires_active_enforcement(republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(republican_reading, fixed_text).
narrative_ontology:cs_authority_grounding(republican_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(republican_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(republican_reading, electoral_majority).
narrative_ontology:constraint_victim(republican_reading, electoral_minorities).
narrative_ontology:constraint_victim(republican_reading, future_generations).
narrative_ontology:constraint_victim(republican_reading, non_voting_subjects).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ELECTORAL MINORITY (SNARE) — Structurally powerless within the mechanism; bound by the outcome of an electoral process they opposed. High suppression: exit options are severely constrained (exit the polity, accept minority status, or attempt extra-institutional override). No coordination benefit; pure extraction of compliance with majority decision. The minority experiences the republican constraint as coercive mechanism with no reciprocal benefit.
constraint_indexing:constraint_classification(republican_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FUTURE GENERATIONS (SNARE) — Cannot participate in the electoral process that binds them; constrained by decisions made by current electorate. High suppression: future agents have zero exit options and zero say in the originating mandate. Theater low (no pretense of their consent). Pure extraction: present electorate captures benefits of present resource use, future generations bear costs. Temporal dimension of the snare.
constraint_indexing:constraint_classification(republican_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: SWING VOTER (TANGLED ROPE) — Experiences genuine coordination function (electoral system aggregates preferences into collective decisions) alongside extraction (campaign pressure, voter suppression tactics, gerrymandering of districts). Constrained exit: can theoretically exit the polity but at high cost. Can organize as a voting bloc. Benefits from the coordination mechanism (their vote matters; policies are responsive to electoral pressure) while bearing extraction costs (strategic mobilization, disenfranchisement threats, candidate quality constraints). Mixed structure.
constraint_indexing:constraint_classification(republican_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ELECTORAL MAJORITY COALITION (ROPE) — Benefits from the coordination mechanism (their preferences are aggregated into policy). Experiences the constraint as pure coordination: the electoral system solves the collective action problem of selecting rulers and aggregating interests. Arbitrage options: can switch parties, form new coalitions, exit by emigration (low cost for institutional actors). Net beneficiary. The republican mechanism is experienced as transparent coordination with no extraction perceived.
constraint_indexing:constraint_classification(republican_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE EXECUTIVE (TANGLED ROPE) — Receives power from electoral mandate but constrained by it. Genuine coordination benefit: electoral process provides legitimacy foundation and periodic renewal. Extraction mechanism: executive captures resources during term of office, accumulates power through administrative apparatus, faces incentive to entrench mandate against future electoral challenges. Theater present: campaigns performatively claim mandate continuity. Constrained exit: executive cannot simply leave office without constitutional crisis. Mixed experience of coordination (legitimacy through election) and extraction (power consolidation during term).
constraint_indexing:constraint_classification(republican_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CONSTITUTIONAL ORDER (PITON) — From the civilizational perspective, the republican electoral system persists largely through institutional inertia and theatrical legitimacy rather than robust functional verification. The original coordination problem (succession without civil war) has been solved; the mechanism now persists because alternatives haven't fully displaced it and because constitutional theory naturalizes electoral legitimacy without continuously re-testing its functional necessity. Theater high: constitutional ceremonies, election pageantry, appeals to 'the people's mandate' function performatively to maintain the system rather than as active verification. Extracted benefit (resource flows to institutional maintenance) accumulates without continuous functional renewal.
constraint_indexing:constraint_classification(republican_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/universal perspective, the republican constraint embodies genuine coordination (solving the succession problem, aggregating preferences) paired with asymmetric extraction (majority rules minorities, present extracts from future, institutional power accumulates during term). The mechanism is neither pure law of nature nor pure extraction mechanism — it is a hybrid that solves a real collective action problem while enabling systematic extraction from those structurally overridden by the majority rule principle. No false summit: the mechanism is neither natural law nor pure extraction, but authentically hybrid.
constraint_indexing:constraint_classification(republican_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(republican_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(republican_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(republican_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(republican_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(republican_reading, TR),
    TR >= 0.70.

:- end_tests(republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, escalating over time. The republican mechanism begins with genuine electoral authorization (ε ≈ 0.35 at point of election), but executive power accumulation during the term of office raises extractiveness as the original mandate becomes temporally distant and the executive relies on administrative power rather than electoral constraint (ε ≈ 0.58 by term midpoint or end). The mechanism includes genuine coordination benefit (succession without violence, preference aggregation) but extraction is substantial: minorities have no veto, future generations cannot participate, and institutional actors accumulate resources and power. Suppression (0.58): Moderate-high and structural. Electoral minorities face high barriers to exit (territorial embeddedness, citizenship, relocation costs) and to override the majority decision (voting bloc formation requires time and resources; constitutional amendment is deliberately difficult). Exit options exist theoretically (emigration, non-compliance, constitutional convention) but at high cost. Suppression is partly procedural (majority-rule voting mechanism prevents veto) and partly coercive (enforcement of majority decisions against minorities). Theater ratio (0.48): Moderate. Electoral campaigns and constitutional rhetoric involve performative elements (appeals to 'mandate,' 'the people's will,' constitutional ceremonies), but the mechanism retains functional elements that distinguish it from pure theater (elections do remove failed rulers, electoral competition does constrain executive policy, periodic renewal does reset authority). Higher theater than pure coordination (rope ≈ 0.20) but much lower than degraded institutional mechanisms (piton ≈ 0.70).
 *
 * PERSPECTIVAL GAP:
 *   The electoral majority and electoral minority perceive the same institutional structure as fundamentally opposite constraint types: the majority sees rope (their preferences are coordinated into policy; the mechanism works), while the minority sees snare (their preferences are overridden; the mechanism coerces them). This gap is not an observational disagreement or a measurement difference — it reflects a genuine structural asymmetry. The mechanism solves a coordination problem (succession without violence) that benefits majorities by definition (their preference is chosen) but extracts from minorities by definition (their preference is not chosen, yet they must comply). No neutral vantage point erases this asymmetry. The analytical observer can see both simultaneously, but from the minority's vantage point, the mechanism is pure extraction (snare), and from the majority's vantage point, it is pure coordination (rope). This is not a failure of indexical classification — it is indexical classification correctly identifying that the same mechanism has opposite structural consequences for different agents.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from the agent's structural position relative to the electoral constraint. Electoral majorities hold d ≈ 0.15 (beneficiaries with arbitrage options — can form coalitions, switch voting patterns, exit if needed). Electoral minorities hold d ≈ 0.88 (victims with trapped exit — high suppression, no veto, coercive enforcement of majority decisions). Future generations hold d ≈ 0.95 (victims with zero participation and zero exit — present electorate makes binding decisions; future agents cannot renegotiate or reverse). Swing voters hold d ≈ 0.52 (symmetric costs and benefits — their vote is valuable in coalition formation, but they face mobilization costs and policy exposure to majority outcomes). Executive actors hold d ≈ 0.48 (mixed: benefit from electoral legitimacy but constrained by periodic renewal mandate). The constitutional order holds d ≈ 0.32 (institutional beneficiary of legitimacy provision, but constrained by the fact that electoral renewal theoretically can remove the system). The sigmoid f(d) transforms these d values into experienced extractiveness; combined with spatial scope (national, so σ=1.0), the chi formula produces the indexed classifications. Minorities' high d → high f(d) → high χ → snare. Majorities' low d → negative f(d) → low χ → rope. Mixed positions → moderate χ → tangled_rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The republican reading does not suffer mandatrophy because the beneficiary/victim structure is explicit and asymmetric: majorities are beneficiaries, minorities are victims, and the mechanism's primary function is to aggregate majority preference into binding policy. The mechanism is neither a pure coordination mechanism (rope) nor a pure extraction mechanism (snare) because it solves a real collective action problem (succession) while enabling systematic extraction from minorities and future actors. The tangled_rope classification captures this authentically hybrid structure. The mandatrophy that appears to threaten the reading is resolved by recognizing that the democratic principle — majority rule — IS the constraint's core mechanism, and that this principle structurally enables extraction from those structurally overridden by majority decision. The constraint is not mislabeled coordination masquerading as extraction; it is genuinely hybrid with both functions present and asymmetrically distributed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_expiration_crisis,
    'What mechanisms constrain executive power accumulation after the original electoral mandate expires? Does periodic renewal actually reset power or merely theatrically legitimize accumulated power?',
    'Historical analysis of re-election patterns; measurement of executive power (budget control, appointments, regulatory authority) before vs after electoral transitions; comparison with constitutional term limits that force succession',
    'If renewal genuinely resets power: republican mechanism is rope-dominant (coordination). If renewal only legitimizes entrenchment: mechanism is snare-dominant (extraction), piton-secondary (theater masks power consolidation). Current assessment assumes partial reset (tangled_rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_expiration_crisis, empirical, 'Whether electoral renewal resets or merely legitimizes accumulated executive power').

omega_variable(
    minority_structural_override,
    'Is the structural override of minority consent an inherent feature of majority-rule republicanism, or a contingent design flaw remediable by constitutional adjustment (supermajority requirements, minority vetoes, federalism)?',
    'Comparative constitutional analysis of mechanisms that weaken simple majority tyranny (supermajority gates, bicameralism, veto powers, proportional representation); empirical measurement of minority welfare outcomes across different republican architectures',
    'If inherent: snare classification for minorities is structural and unchanged by constitutional tweaks. If remediable: snare classification reflects this specific instantiation of republicanism, and alternative designs shift toward rope for minorities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_structural_override, conceptual, 'Whether majority override of minorities is inherent to republicanism or design-contingent').

omega_variable(
    electoral_mandate_legitimacy,
    'Does popular electoral mandate confer genuine legitimacy on state authority, or is electoral legitimacy a procedural fiction that naturalizes whatever the majority prefers?',
    'Philosophical analysis and empirical testing: do majorities actually constrain their own power based on fairness principles, or only when threatened with exit/defection? Do minorities accept majority decisions based on procedural fairness, or based on coercion + ideological capture?',
    'If genuine: coordinate problem is authentically solved by republicanism (rope-dominant). If fictional: mandate is pure extraction theater (snare-dominant, piton-secondary). Current assessment assumes partial/contingent legitimacy (tangled_rope).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(electoral_mandate_legitimacy, conceptual, 'Whether electoral legitimacy confers genuine authority or naturalizes majority power').

omega_variable(
    sovereign_legitimacy_kernel_reading,
    'This constraint instantiates the REPUBLICAN READING of the sovereign_legitimacy kernel. Sibling readings (monarchical, mixed_constitutional) would structure legitimacy differently. What observable or empirical claim distinguishes the republican reading from its siblings?',
    'Identify the specific mechanism that makes THIS reading distinct: republican reading claims legitimacy derives from periodic electoral renewal by the governed; monarchical reading grounds legitimacy in hereditary/divine succession; mixed reading distributes legitimacy across multiple sources (election, tradition, expertise). Empirical test: where does each reading locate the SOURCE of authority (electoral consent vs. tradition vs. distributed)?',
    'This reading''s extractiveness (0.52) and suppression (0.58) reflect the specific structural facts of electoral delegation. A monarchical reading would have different beneficiaries (dynastic line, court nobility) and victims (disenfranchised subjects). A mixed reading would distribute extraction differently. The epsilon and suppression values are specific to THIS reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereign_legitimacy_kernel_reading, conceptual, 'Kernel structure: republican reading vs siblings (monarchical, mixed_constitutional)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(republican_reading, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(repub_tr_t0, republican_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(repub_tr_t2, republican_reading, theater_ratio, 2, 0.42).
narrative_ontology:measurement(repub_tr_t4, republican_reading, theater_ratio, 4, 0.48).

% Extraction over time
narrative_ontology:measurement(repub_be_t0, republican_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(repub_be_t2, republican_reading, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(repub_be_t4, republican_reading, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(republican_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(republican_reading, monarchical_reading).
narrative_ontology:affects_constraint(republican_reading, mixed_constitutional_reading).

% DUAL FORMULATION NOTE:
% The republican_reading is one reading of the sovereign_legitimacy kernel. Two sibling readings (monarchical_reading, mixed_constitutional_reading) are structurally distinct constraints with different epsilon values, different beneficiary/victim structures, and different mechanisms for legitimacy grounding. All three stories should be authored and linked via network.affects_constraints. The differences are not perspectival variations on a single constraint but substantively different constraint families reflecting different answers to the fundamental question: what makes authority legitimate?

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
