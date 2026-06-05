% ============================================================================
% CONSTRAINT STORY: ostracism_institution__elite_competition_regulator_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ostracism_institution__elite_competition_regulator_reading, []).

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
 *   constraint_id: ostracism_institution__elite_competition_regulator_reading
 *   human_readable: Ostracism as Elite Competition Regulator (Reading: Competition Mechanism)
 *   domain: legal/doctrinal/athenian_politics
 *
 * SUMMARY:
 *   Ostracism in 5th-century Athens functioned as a formalized mechanism for
 *   regulating elite competition, according to this reading. The institution
 *   allowed aristocratic factions to mobilize the demos against their rivals,
 *   exiling them for ten years without legal charge, property confiscation,
 *   or loss of citizenship rights (upon return). The demos provided the
 *   formal decision-making authority — voters in the assembly cast ostraka
 *   (pottery shards) — but the framing of choices, the mobilization of
 *   sentiment, and the strategic targeting of specific rivals were controlled
 *   by competing aristocratic factions. This reading treats ostracism as a
 *   structural regulator of elite tournament dynamics: the winning faction
 *   benefits through removal of rivals; the losing faction bears suppression
 *   (exile); the demos bears extraction through instrumentalization — their
 *   vote is framed as patriotic duty rather than as participation in
 *   factional struggle. The constraint exhibits genuine coordination (the
 *   demos can prevent tyranny through collective exile decisions) alongside
 *   extraction (the demos loses control over the framing and purpose of their
 *   own power). Suppression increases over the interval (from 0.60 to 0.71)
 *   as the institution transitions from legitimating functional mechanism to
 *   performative ritual. Theater ratio rises (0.45 to 0.72) as ostracism
 *   loses credibility and becomes divorced from actual elite competition
 *   (which moves to other arenas: military command, jury service, rhetoric).
 *   By the piton stage (late 5th century), the institution persists largely
 *   through ceremonial inertia, despite visible arbitrariness (Aristides
 *   exiled for being called 'the Just' once too often). The reading's core
 *   claim — that ostracism regulated elite competition with the demos as tool
 *   — enters tension with the safety_valve reading (which emphasizes
 *   ostracism as tyranny-prevention) and the arbitrary_exile reading (which
 *   emphasizes the absence of principled process).
 *
 * KEY AGENTS:
 *   - Winning Aristocratic Faction (institutional/arbitrage): Beneficiary. Controls mobilization rhetoric; activates ostracism as strategic tool; benefits from removal of rivals with no reputational cost (ten-year exile is reversible, framed as restoration rather than punishment).
 *   - Losing Aristocratic Faction (institutional/constrained): Primary victim. Experiences suppression: mandatory ten-year exile, loss of political and economic power, no appeal or reversal within term. Cannot negotiate or resist once mobilized against.
 *   - The Demos / General Assembly (powerless/trapped as instrumentalized tool; moderate/constrained as institutional actor): Secondary victim and coordinating body. Trapped by the role of decision-maker once factions invoke ostracism; instrumentalized through rhetorical framing that presents factional struggle as collective security. Bears extraction through loss of control over deliberation framing and purpose.
 *   - Institutional Reformers (organized/constrained): Observers recognizing ostracism as temporary solution to elite competition in a not-yet-mature democracy. See sunset pathway as democratic institutions strengthen.
 *   - The Ostracism Procedure (institutional/arbitrage): Itself degraded over time into performative ritual divorced from substantive regulation.
 *   - Analytical Observer (analytical/analytical): Risks naturalizing the contingent institutional arrangement as universal law of elite dynamics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ostracism_institution__elite_competition_regulator_reading, 0.55).
domain_priors:suppression_score(ostracism_institution__elite_competition_regulator_reading, 0.68).
domain_priors:theater_ratio(ostracism_institution__elite_competition_regulator_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ostracism_institution__elite_competition_regulator_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(ostracism_institution__elite_competition_regulator_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ostracism_institution__elite_competition_regulator_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ostracism_institution__elite_competition_regulator_reading, tangled_rope).
narrative_ontology:human_readable(ostracism_institution__elite_competition_regulator_reading, "Ostracism as Elite Competition Regulator (Reading: Competition Mechanism)").
narrative_ontology:topic_domain(ostracism_institution__elite_competition_regulator_reading, "legal/doctrinal/athenian_politics").

domain_priors:requires_active_enforcement(ostracism_institution__elite_competition_regulator_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ostracism_institution__elite_competition_regulator_reading, 'ca6a819b-1b7d-4364-a6bb-074b9382bf2f').
narrative_ontology:cs_kernel_codification('ca6a819b-1b7d-4364-a6bb-074b9382bf2f', formalized).
narrative_ontology:cs_authority_grounding('ca6a819b-1b7d-4364-a6bb-074b9382bf2f', extraction).
narrative_ontology:cs_interpretation_layer_present('ca6a819b-1b7d-4364-a6bb-074b9382bf2f').
narrative_ontology:cs_reading_relation('ca6a819b-1b7d-4364-a6bb-074b9382bf2f', ostracism_institution__arbitrary_exile_reading, coexists_with).
narrative_ontology:cs_reading_relation('ca6a819b-1b7d-4364-a6bb-074b9382bf2f', ostracism_institution__safety_valve_reading, influences).
narrative_ontology:cs_axiom('ca6a819b-1b7d-4364-a6bb-074b9382bf2f', foundational, ostracism_is_factional_tool).
narrative_ontology:cs_axiom_status(ostracism_is_factional_tool, holdable).
narrative_ontology:cs_axiom_grounding('ca6a819b-1b7d-4364-a6bb-074b9382bf2f', ostracism_is_factional_tool, empirically_contingent).
narrative_ontology:cs_axiom('ca6a819b-1b7d-4364-a6bb-074b9382bf2f', foundational, demos_are_instrumentalized_referee).
narrative_ontology:cs_axiom_status(demos_are_instrumentalized_referee, holdable).
narrative_ontology:cs_axiom_grounding('ca6a819b-1b7d-4364-a6bb-074b9382bf2f', demos_are_instrumentalized_referee, empirically_contingent).
narrative_ontology:cs_reference_frame('ca6a819b-1b7d-4364-a6bb-074b9382bf2f', aristocratic_faction_regulatory_system).
narrative_ontology:cs_drift_state('ca6a819b-1b7d-4364-a6bb-074b9382bf2f', late_fifth_century_degradation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ca6a819b-1b7d-4364-a6bb-074b9382bf2f', '').
narrative_ontology:cs_kernel_id(ostracism_institution__elite_competition_regulator_reading, ostracism_institution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ostracism_institution__elite_competition_regulator_reading, winning_aristocratic_faction).
narrative_ontology:constraint_victim(ostracism_institution__elite_competition_regulator_reading, losing_aristocratic_faction).
narrative_ontology:constraint_victim(ostracism_institution__elite_competition_regulator_reading, demos_as_mobilized_tool).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOSING ARISTOCRATIC FACTION (SNARE) — Trapped by the mobilization once rival aristocrats have activated popular sentiment against them. The institution suppresses exit: exile is mandatory, ten years' removal from politics and property management. The losing faction cannot negotiate, appeal, or reverse the decision; they bear the extraction (loss of power, income, influence) without recourse. Maximum suppression + no alternatives = snare from the victim's perspective.
constraint_indexing:constraint_classification(ostracism_institution__elite_competition_regulator_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: DEMOS AS MOBILIZED TOOL (SNARE) — The demos cannot exit the role of decision-maker once invoked. They are trapped in the position of 'referee' for rivalries they did not originate and cannot refuse to arbitrate. The institution suppresses their agency: they must vote in the assembly; the vote is final and binding; voting is framed as patriotic duty rather than as instrumental participation in elite rivalry. They experience extraction through loss of control over their own collective decision-making — the frame of the choice is set by aristocratic actors, not by the demos. Suppression manifests as normative capture (voting is duty, not choice) and structural constraint (refusal to participate would be political suicide or sacrilege).
constraint_indexing:constraint_classification(ostracism_institution__elite_competition_regulator_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: DEMOS AS INSTITUTIONAL ACTOR (TANGLED ROPE) — The assembly coordinates its own power — the authority to exile rivals is genuinely vested in the demos collectively. This is a coordination function: the demos, through ostracism, can prevent any single faction from monopolizing state power. But the mechanism is also extractive: the demos pays the cost of instability (political uncertainty, economic damage during exile of key figures, manipulation by aristocratic factions). The institution provides genuine coordination benefit (prevention of tyranny) alongside extraction (the demos becomes the battlefield, loses control over the framing of choices).
constraint_indexing:constraint_classification(ostracism_institution__elite_competition_regulator_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: WINNING FACTION (ROPE) — Perceives ostracism as pure coordination mechanism. The tool is neutral; the faction that wins mobilization did so through legitimate persuasion. Extraction flows toward the beneficiary (removal of rivals, consolidation of power), but the faction experiences this as successful competition within a rule-bound system. The beneficiary has arbitrage options: they could refuse to mobilize ostracism, compete through other means, or negotiate settlements. They choose ostracism because it is the most efficient coordination mechanism for eliminating rivals. From this perspective, suppression is low (the faction has options) and extractiveness is reframed as 'legitimate competition.' No enforcement burden is perceived — the demos cooperates voluntarily.
constraint_indexing:constraint_classification(ostracism_institution__elite_competition_regulator_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 5: INSTITUTIONAL REFORMERS / SUNSET LOGIC (SCAFFOLD) — Organized critics (Aristotle, later democrats) recognize ostracism as a temporary institution designed to prevent tyranny in an era of high elite faction risk. The sunset is structural: as democratic institutions mature and the demos' political consciousness develops, ostracism becomes unnecessary. The reformers see it as chi ≤ 0.30 because the coordination function is being superseded by stronger democratic institutions (property law, ostracism abolition, democratic norms). The institution has a built-in sunset: it fades as the demos gains real power and aristocratic dominance becomes untenable.
constraint_indexing:constraint_classification(ostracism_institution__elite_competition_regulator_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 6: OSTRACISM AS DEGRADED RITUAL (PITON) — By the late 5th century, ostracism persists largely through ceremonial inertia. The assembly still votes; the procedure is still followed; but the institution has lost its functional credibility. Theater_ratio ≥ 0.70: the ostracism ritual is performed, but the real elite competition happens through other mechanisms (military command, jury duty, legislative rhetoric). Ostracism becomes a vestigial threat that rarely activates. When it does, it appears arbitrary and mob-driven (the Aristides example: exiled for being called 'the Just' once too often, suggesting the reason is aesthetic excess rather than political necessity). The piton reflects the institution's atrophy into theatrical performance.
constraint_indexing:constraint_classification(ostracism_institution__elite_competition_regulator_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 7: CIVILIZATIONAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, elite competition for power is inherent to any hierarchical society. Ostracism appears as a natural, inevitable mechanism for regulating unavoidable rivalries. The institution is 'just how human societies work' — a formal name for the suppression of rivals that occurs in every political system. This perspective risks naturalizing a contingent institutional arrangement (Athenian ostracism, a specific legal procedure) as a universal law. The engine's false-summit detector will evaluate whether this naturalization conceals the beneficiary structure (the winning faction benefits) and the extractive mechanism (the demos is instrumentalized).
constraint_indexing:constraint_classification(ostracism_institution__elite_competition_regulator_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ostracism_institution__elite_competition_regulator_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ostracism_institution__elite_competition_regulator_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ostracism_institution__elite_competition_regulator_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ostracism_institution__elite_competition_regulator_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ostracism_institution__elite_competition_regulator_reading, TR),
    TR >= 0.70.

:- end_tests(ostracism_institution__elite_competition_regulator_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-high. The constraint extracts from two groups: (1) the losing faction (loss of power and income for ten years, though reversible); (2) the demos (loss of control over their own deliberation, though framed as empowerment). The extraction is not total because ostracism is reversible, property rights are maintained (unlike confiscation), and the institution's stated purpose is democratically legitimate (preventing tyranny). But the asymmetry is severe: winning factions benefit consistently; losing factions and the demos bear costs consistently. Suppression (0.68): Moderate-high. Structural barriers include: legal finality of ostracism votes, mandatory exile, loss of active political participation. Normative barriers include: framing votes as patriotic duty, integrating ostracism into democratic ideology, suppressing the recognition that the demos are being used as a tool in aristocratic rivalry. Theater ratio (0.65): Moderate-high. The procedure is formal and regularized (theater ≥ 0.45) but remains functionally tied to actual elite regulation (theater < 0.75). By the late 5th century, ostracism becomes more theatrical as actual elite competition moves to other mechanisms. The measurement trajectory shows extractiveness and theater both rising: as the functional regulation weakens, the performative content increases (classic sign of institutional degradation toward piton). Suppression also rises over the interval, suggesting that as the institution's legitimacy erodes, more normative/ideological pressure is required to maintain compliance.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between beneficiary and victim is extreme. The winning faction sees rope (pure coordination, neutral tool, successful competition within rules). The demos as tool see snare (trapped, no exit, no control). The demos as institutional actor see tangled_rope (genuine coordination function of preventing tyranny, but also extraction through manipulation). The losing faction sees snare (complete suppression, no alternatives, exile mandatory). The reformers see scaffold (temporary institution with sunset as democracy matures). The ritual sees piton (degraded procedure persisting through inertia). The civilizational observer risks mountain (naturalizing elite competition as law of nature). This spread across all six types from a single metric base signals that ostracism is a constraint whose legitimacy rests entirely on the observer's structural position relative to it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim declarations and exit options. Winning faction: beneficiary + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 (negative chi, constraint subsidizes them). Losing faction: victim + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 (high chi, constraint extracts from them). Demos as tool: victim + trapped exit (cannot refuse vote) → d ≈ 0.95 → f(d) ≈ 1.42. Demos as institution: victim + constrained exit (can organize reform pressure) + beneficiary (prevents tyranny) → d ≈ 0.50 → f(d) ≈ 0.65 (moderate chi, mixed experience). The formula χ = ε × f(d) × σ(S) produces: winning faction χ ≈ 0.55 × (-0.12) × 0.80 ≈ -0.05 (subsidized); losing faction χ ≈ 0.55 × 1.42 × 0.80 ≈ 0.62 (extracted). Scope σ(S) = 0.8 for local; the constraint operates only in Athens, reducing the effective scaling.
 *
 * MANDATROPHY ANALYSIS:
 *   READING RESOLUTION: This reading resolves mandatrophy by declaring the specific structural claim: ostracism is a tool for regulating aristocratic competition, with the demos as instrumentalized referee. This forecloses the arbitrary_exile reading's claim that ostracism is unprincipled (if it regulates competition, it has principle, even if the principle serves factional interests). But it coexists with the safety_valve reading because preventing elite deadlock is compatible with regulating elite competition — the safety valve may be the intended function from the perspective of institutional designers, while elite regulation is the actual function from the perspective of those using the tool. The reading's beneficiary structure (winning faction, not the demos) and victim structure (losing faction + demos as tool) make explicit the extraction hidden in the 'neutral procedure' framing. The mandatrophy is resolved by recognizing that all three readings describe real structural features of ostracism: it IS arbitrary (arbitrary_exile reading), it DOES prevent deadlock (safety_valve reading), and it DOES regulate elite competition (this reading). The false summit risk is the mountain perspective's naturalization of elite competition as inevitable — the reading denies this by showing the demos' specific structural role in the institution, which is contingent and reversible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demos_agency_vs_instrumentalization,
    'Did the demos exercise genuine collective deliberation in ostracism votes, or were they instrumentalized by aristocratic factions through rhetorical manipulation?',
    'Historical analysis of assembly debates, rhetoric preserved in forensic speeches, patterns of ostracism voting (do votes follow factional lines or reflect independent judgment?); comparison with other assembly decisions to identify consistent voting patterns.',
    'If genuine agency: demos perception shifts toward tangled_rope (mixed coordination/extraction). If instrumentalized: demos perception remains snare (pure extraction via normative capture). Changes the reading''s claim that demos are ''referee'' — refereeing requires agency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demos_agency_vs_instrumentalization, empirical, 'Whether demos deliberation was genuine or rhetorically manipulated').

omega_variable(
    aristocratic_sincerity_in_competition,
    'Did aristocratic factions genuinely use ostracism as a competition regulator, or was ostracism a pretext to eliminate rivals while claiming neutrality?',
    'Pattern analysis of ostracism targets: are they genuine threats to stability (chronological proximity to tyrants, excessive power accumulation) or personal rivals? Cross-reference with non-ostracism exiles, murders, and ostracizations reversed after the exile period.',
    'If genuine competition mechanism: reading''s tangled_rope classification holds; suppression is real but purpose is coordination. If pretext: suppression is the purpose; the constraint is pure snare, and the ''regulator'' framing is naturalization of tyrannical tool.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aristocratic_sincerity_in_competition, empirical, 'Sincerity of aristocratic competition framing versus disguised tyranny').

omega_variable(
    kernel_contest_boundary,
    'What structural features distinguish THIS reading (elite competition regulator) from the sibling readings (arbitrary_exile and safety_valve)?',
    'Examine the reading_relations: does this reading foreclose the arbitrary_exile reading''s core claim that ostracism was unprincipled? Does it coexist with the safety_valve reading''s claim that ostracism prevented tyranny? Or does it influence both by reframing ostracism as fundamentally a factional tool rather than a democratic institution?',
    'Clarifies whether the readings are genuinely different constraint models (different ε values, different beneficiary structures) or merely different narrative frames on the same underlying mechanism. If different models, the constraint family is properly decomposed. If same mechanism with different frames, the family structure collapses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_boundary, conceptual, 'Structural distinctness of elite_competition_regulator from sibling readings').

omega_variable(
    suppression_mechanism_structural_vs_normative,
    'Is suppression in this constraint primarily structural (legal barriers to reversing ostracism, property confiscation, legal exile) or normative (framing exile as duty, integrating it into democratic ideology)?',
    'Comparison of: (1) legal statutes on ostracism reversibility, property rights of exiles, and recall mechanisms; (2) rhetorical sources portraying ostracism as patriotic, inevitable, just. Measure the ratio of legal to ideological suppression.',
    'If primarily structural: suppression score remains high (0.68). If primarily normative: the constraint exhibits high theater and operates through cognitive capture; reclassify to piton or scaffold depending on historical phase.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_normative, empirical, 'Suppression mechanism: structural barriers vs. normative capture').

omega_variable(
    reading_kernel_grounding_ambiguity,
    'What is the kernel that grounds THIS reading''s legitimacy claim? Is it the Solonian law text? The assembly''s authority? Democratic ideology? Or the practical necessity of elite regulation?',
    'Trace the reading''s authority grounding: what source (text, practice, ideology) does it rely on? Does it claim the kernel is the written law (Solon''s institution) or the demos'' will (reinterpreted institution)? Does it ground legitimacy in preventing tyranny (functional claim) or in democratic assembly authority (normative claim)?',
    'Determines cs_structure.authority_grounding value: if text-based, use ''lineage'' (transmitted law); if practice-based, use ''practice'' (democratic custom); if extraction-based (the reading conceals that winning factions benefit), use ''extraction''. Changes how the reading relates to the kernel and how drift_state evaluates departure from t0 reference frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_grounding_ambiguity, conceptual, 'Authority grounding for the elite_competition_regulator reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ostracism_institution__elite_competition_regulator_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ostracism_elite_tr_t0, ostracism_institution__elite_competition_regulator_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(ostracism_elite_tr_t25, ostracism_institution__elite_competition_regulator_reading, theater_ratio, 25, 0.58).
narrative_ontology:measurement(ostracism_elite_tr_t50, ostracism_institution__elite_competition_regulator_reading, theater_ratio, 50, 0.72).

% Extraction over time
narrative_ontology:measurement(ostracism_elite_be_t0, ostracism_institution__elite_competition_regulator_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ostracism_elite_be_t25, ostracism_institution__elite_competition_regulator_reading, base_extractiveness, 25, 0.55).
narrative_ontology:measurement(ostracism_elite_be_t50, ostracism_institution__elite_competition_regulator_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ostracism_elite_su_t0, ostracism_institution__elite_competition_regulator_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(ostracism_elite_su_t25, ostracism_institution__elite_competition_regulator_reading, suppression_requirement, 25, 0.68).
narrative_ontology:measurement(ostracism_elite_su_t50, ostracism_institution__elite_competition_regulator_reading, suppression_requirement, 50, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ostracism_institution__elite_competition_regulator_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ostracism_institution__elite_competition_regulator_reading, ostracism_institution__arbitrary_exile_reading).
narrative_ontology:affects_constraint(ostracism_institution__elite_competition_regulator_reading, ostracism_institution__safety_valve_reading).

% DUAL FORMULATION NOTE:
% The ostracism_institution kernel decomposes into three constraint stories with different epsilon values and beneficiary structures. The elite_competition_regulator reading (this story, ε=0.55) emphasizes the asymmetric power distribution and factional instrumentalization of the demos. The safety_valve reading (ε=0.35) emphasizes the coordination function and prevention of tyranny. The arbitrary_exile reading (ε=0.72) emphasizes the absence of principled process and maximal extraction. All three are structural claims about the same historical institution. They are linked by network.affects_constraints because the reading you hold determines how you interpret the same set of historical facts (voting patterns, target selection, outcomes). Each reading is a complete ε-invariant constraint story because changing the reading changes which structural features you attend to and how you weight beneficiary/victim/coordination functions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ostracism_institution__elite_competition_regulator_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
