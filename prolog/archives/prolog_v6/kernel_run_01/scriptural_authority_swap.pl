% ============================================================================
% CONSTRAINT STORY: scriptural_authority_swap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_scriptural_authority_swap, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: scriptural_authority_swap
 *   human_readable: Scriptural Authority Swap in the Protestant Reformation (1517-1648)
 *   domain: historical_sociology/religious_studies/political_economy
 *
 * SUMMARY:
 *   The Protestant Reformation (1517-1648) represents a compound constraint
 *   exhibiting simultaneous climb, drop, swap, and emergence patterns across
 *   multiple institutional actors. The primary structural shift is an
 *   authority transfer from institutional-hierarchical (the Church's
 *   interpretive monopoly) to textual-democratic (direct access to scripture
 *   via print in vernacular languages). This single natural-language event
 *   decomposes into multiple structurally distinct constraints — the
 *   authority shift is not monolithic. The scriptural_authority_swap
 *   constraint models the redistributive dimension: how authority flows from
 *   ecclesiastical hierarchy to reformist clergy, literate merchants, and
 *   territorially sovereign states, while suppressing the illiterate
 *   majority. The constraint's extractiveness climbs during the period
 *   1517-1580 (the active reformation phase), peaks around 1548-1580
 *   (Counter-Reformation intensification), then declines through 1600-1648
 *   (settlement toward coexistence) as doctrinal positions stabilize and the
 *   new authority structure becomes institutionalized. Theater ratio peaks
 *   during Counter-Reformation as the ecclesiastical hierarchy mounts
 *   elaborate doctrinal responses (Council of Trent, Jesuit theology) that do
 *   not restore lost authority but perform authority. By Peace of Westphalia
 *   (1648), the constraint has transitioned from extractive imposition to
 *   normalized coordination between competing religious-political frameworks.
 *   The five perspectives demonstrate how a single constraint appears as
 *   immutable law (Mountain), coordinated benefit (Rope), mixed
 *   extraction/coordination (Tangled Rope), degraded ritual (Piton), and
 *   strategic opportunity (from the sovereigns' position). The false-summit
 *   detector would flag the Mountain perspective: the naturalization of
 *   institutional authority transfer as inevitable consequence of literacy
 *   and print obscures the historical contingency of the specific actors who
 *   captured the authority reorganization (reformist clergy, territorial
 *   princes, print merchants).
 *
 * KEY AGENTS:
 *   - The Ecclesiastical Hierarchy (Roman Church): Primary victim (institutional/arbitrage, but constrained by authority loss) — loses interpretive monopoly, forced into defensive Counter-Reformation. Museum of degraded authority (Piton perspective).
 *   - Reformist Clergy (Luther, Calvin, Zwingli, and successors): Primary beneficiary (institutional/constrained-then-arbitrage) — gains interpretive authority and doctrinal leadership; also constrained by requirement for systematic scriptural justification and institutional enforcement.
 *   - Printing Industry & Merchant Class: Secondary beneficiary (institutional/arbitrage) — captures massive market for vernacular Bibles, prints, and devotional materials. Pure coordination perspective.
 *   - Illiterate Majority (peasants, urban lower class, women): Primary victim (powerless/trapped) — cannot access the new authority form (textual/printed); forced to accept legitimacy of scriptural authority via preaching and institutional enforcement.
 *   - Territorial Sovereigns (princes, kings): Tertiary beneficiary with constraint (powerful/mobile) — gains religious legitimacy independent of Rome; constrained by need to police doctrinal extremism (Anabaptism, radicalism) within their realms.
 *   - Scholastic Interpretive Tradition: Structural victim (organized/constrained) — textual authority challenges scholastic authority from university/cathedral schools; interpretive categories (universals, necessity, adequatio intellectus et rei) become targets of reformist criticism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(scriptural_authority_swap, 0.58).
domain_priors:suppression_score(scriptural_authority_swap, 0.65).
domain_priors:theater_ratio(scriptural_authority_swap, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(scriptural_authority_swap, extractiveness, 0.58).
narrative_ontology:constraint_metric(scriptural_authority_swap, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(scriptural_authority_swap, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(scriptural_authority_swap, tangled_rope).
narrative_ontology:human_readable(scriptural_authority_swap, "Scriptural Authority Swap in the Protestant Reformation (1517-1648)").
narrative_ontology:topic_domain(scriptural_authority_swap, "historical_sociology/religious_studies/political_economy").

domain_priors:requires_active_enforcement(scriptural_authority_swap).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(scriptural_authority_swap, '6820286a-8d1e-4ff6-8e67-469481d0ca41').
narrative_ontology:cs_created_at('6820286a-8d1e-4ff6-8e67-469481d0ca41', '').
narrative_ontology:cs_kernel_codification('6820286a-8d1e-4ff6-8e67-469481d0ca41', fixed_text).
narrative_ontology:cs_authority_grounding('6820286a-8d1e-4ff6-8e67-469481d0ca41', lineage).
narrative_ontology:cs_interpretation_layer_present('6820286a-8d1e-4ff6-8e67-469481d0ca41').
narrative_ontology:cs_reading_relation('6820286a-8d1e-4ff6-8e67-469481d0ca41', ecclesiastical_authority_monopoly_reading, forecloses).
narrative_ontology:cs_reading_relation('6820286a-8d1e-4ff6-8e67-469481d0ca41', radical_reformation_scriptural_immediacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('6820286a-8d1e-4ff6-8e67-469481d0ca41', catholic_scripture_tradition_reading, coexists_with).
narrative_ontology:cs_axiom('6820286a-8d1e-4ff6-8e67-469481d0ca41', foundational, scripture_accessible_to_vernacular_literate).
narrative_ontology:cs_axiom_status(scripture_accessible_to_vernacular_literate, holdable).
narrative_ontology:cs_axiom('6820286a-8d1e-4ff6-8e67-469481d0ca41', foundational, individual_conscience_supreme_authority).
narrative_ontology:cs_axiom_status(individual_conscience_supreme_authority, holdable).
narrative_ontology:cs_axiom('6820286a-8d1e-4ff6-8e67-469481d0ca41', secondary, ecclesiastical_mediation_unnecessary_to_salvation).
narrative_ontology:cs_axiom_status(ecclesiastical_mediation_unnecessary_to_salvation, holdable).
narrative_ontology:cs_reference_frame('6820286a-8d1e-4ff6-8e67-469481d0ca41', scriptural_immediate_access).
narrative_ontology:cs_drift_state('6820286a-8d1e-4ff6-8e67-469481d0ca41', post_westphalia_settlement, gap(practice_drift, substantial, true)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(scriptural_authority_swap, reformist_clergy).
narrative_ontology:constraint_beneficiary(scriptural_authority_swap, print_merchants).
narrative_ontology:constraint_beneficiary(scriptural_authority_swap, vernacular_literate_merchants).
narrative_ontology:constraint_victim(scriptural_authority_swap, ecclesiastical_hierarchy).
narrative_ontology:constraint_victim(scriptural_authority_swap, illiterate_majority).
narrative_ontology:constraint_victim(scriptural_authority_swap, scholastic_interpretive_tradition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

constraint_indexing:constraint_classification(scriptural_authority_swap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

constraint_indexing:constraint_classification(scriptural_authority_swap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

constraint_indexing:constraint_classification(scriptural_authority_swap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

constraint_indexing:constraint_classification(scriptural_authority_swap, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

constraint_indexing:constraint_classification(scriptural_authority_swap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

constraint_indexing:constraint_classification(scriptural_authority_swap, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(scriptural_authority_swap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(scriptural_authority_swap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(scriptural_authority_swap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(scriptural_authority_swap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(scriptural_authority_swap, TR),
    TR >= 0.70.

:- end_tests(scriptural_authority_swap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant structural advantage for reformist clergy, print merchants, and territorial sovereigns (who gain authority claims without Rome), while suppressing the illiterate majority's interpretive voice and the Church's institutional legitimacy. But extractiveness is not total (would be 0.72+) because: (1) the transfer of authority is partly coordinated — the new scriptural framework does solve a genuine coordination problem (interpretation no longer mediated solely through priests, but accessible via text); (2) reformist clergy face genuine constraints (risk of ex-communication, intellectual requirement for systematic exegesis) that moderate their beneficiary status; (3) by mid-17th century, the extracted value begins declining as the new authority structure becomes normalized and the dramatic period of extraction (1517-1580) gives way to equilibrium. Suppression (0.65): High. Multiple populations and institutions are forcibly incorporated into the new authority structure: illiterate populations cannot opt out (trapped); the Church is forced to respond (no arbitrage exit); scholastic tradition is displaced; radical interpretations must be suppressed to prevent authority framework collapse. Theater ratio (0.68): High. Counter-Reformation represents massive performative theater — doctrinal clarifications, council sessions, Jesuit theological elaborations — that do not restore ecclesiastical authority but perform its continuation. The theater also includes reformist preaching and scriptural exegesis performed to demonstrate systematic defense against Catholic critique. The performance is more prominent during the peak extraction period (1540-1580) when authority is most contested.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits the maximum perspectival divergence possible in the six-type system. The Ecclesiastical Hierarchy experiences Piton (degraded ritual theater); the Illiterate Majority experiences Snare (trapped extraction); the Reformist Clergy experience Tangled Rope (both enabled and constrained); the Printing Industry experiences Rope (pure coordination benefit); the Territorial Sovereigns experience Tangled Rope (strategic opportunity with internal constraint); the Analytical Observer risks seeing Mountain (naturalizing the shift as inevitable consequence of literacy). The gap reflects genuine structural divergence: the same institutional reorganization is simultaneously enabling for some agents (beneficiaries of textual authority) and extractive for others (populations lacking literacy, institutions losing authority). The gap cannot be closed by refining metrics — it is a real feature of the constraint's structure that different agents occupy incommensurable positions relative to the authority transfer.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from each agent's structural position. Reformist clergy occupy beneficiary + constrained exit (high d from constraint derivation, moderated by biographical vulnerability to ecclesiastical sanction). Ecclesiastical hierarchy occupies victim + arbitrage exit (high d, modified by the fact that arbitrage means they can theoretically reposition, but their institutional identity prevents this — a case for directionality_override if we model identity lock). Illiterate majority occupies victim + trapped exit (maximum d, f(d) ≈ 1.42). Printing merchants occupy beneficiary + arbitrage (low d, f(d) ≈ -0.12). Territorial sovereigns occupy beneficiary-victim hybrid (they benefit strategically but face internal constraints from managing religious radicalism) — split directionality. The canonical derivation chain produces appropriate d values without overrides for most agents; ecclesiastical hierarchy could use an override to capture identity-locked inability to exit despite theoretical mobility.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that the six-type system is adequate to model a complex historical event without type collapse. The constraint is genuinely Tangled Rope at the systemic level: it contains both coordination function (solving the interpretation problem via textual access) and asymmetric extraction (benefiting reformist clergy, merchants, and sovereigns while suppressing illiteracy-dependent populations). The Piton classification for the Ecclesiastical Hierarchy is not a contradiction but a separate perspectival result from the institution's viewpoint: their own authority has become performative theater. The Snare classification for the illiterate majority is not a contradiction but reflects their powerless/trapped position. The false-summit mountain perspective reveals that 'inevitable historical progress' narratives often naturalize contingent extraction patterns. The system does not require adding types or fuzzing boundaries; it requires multiple perspectives to capture the overdetermined structure of the event.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    print_technology_sufficiency,
    'Was print technology a sufficient cause for the authority swap, or merely an enabling condition for a shift driven by pre-existing theological tensions?',
    'Counterfactual analysis: did manuscript-era reformism (Wycliffe, Hus, Pico) show the same authority claims without print? Historical correlation between print arrival and reformation intensity by region.',
    'If sufficient: the constraint is primarily technological (Mountain-adjacent). If enabling condition only: the constraint is primarily theological/political (Tangled Rope confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(print_technology_sufficiency, empirical, 'Whether print technology is sufficient cause vs. enabling condition for authority swap').

omega_variable(
    illiteracy_suppression_mechanism,
    'Is the suppression experienced by illiterate populations structural (lack of access technology) or internalized (belief in the legitimacy of textual authority even without access)?',
    'Analysis of peasant and urban lower-class responses to reformist preaching: do they accept the new scriptural-authority frame, resist it, or show mixed adoption? Post-Reformation literacy campaigns and enforcement mechanisms.',
    'If structural only: snare persists but Scaffold may emerge as literacy improves. If internalized: suppression persists even after literacy reaches them — the constraint is more binding than the Snare classification suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(illiteracy_suppression_mechanism, empirical, 'Suppression mechanism: structural vs. internalized for illiterate populations').

omega_variable(
    ecclesiastical_authority_recovery_window,
    'Was the Ecclesiastical Hierarchy''s authority irreversibly lost by the late 16th century, or did Counter-Reformation measures genuinely restore institutional legitimacy?',
    'Comparison of ecclesiastical authority metrics before/after Council of Trent: enforcement mechanisms deployed (Inquisition), doctrinal clarification (indulgences regulation), institutional reform (seminaries). Measurement of regional religious compliance in Catholic vs. Protestant territories post-1648.',
    'If recovery genuine: Piton classification is temporary, and the constraint transitions toward a stabilized Rope (competing but coexisting authorities). If authority irreversibly lost: Piton persists and may degrade further toward Inertial decay.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ecclesiastical_authority_recovery_window, empirical, 'Whether ecclesiastical authority recovery was genuine or performative').

omega_variable(
    radical_reformation_foreclosure,
    'Does the mainline Protestant scriptural-authority frame foreclose or merely coexist with radical Reformation readings (Anabaptist, Spiritualist, Libertine)?',
    'Analysis of magisterial Reformation responses to radical readings: are they logically incompatible within a single scriptural framework, or are they competing interpretations of the same text? Institutional suppression dynamics (execution of Anabaptists) as evidence of foreclosure threat.',
    'If foreclosure: the mainline authority swap requires suppression of alternative readings — Snare component is defensive (defending against radical implications). If coexistence: multiple readings proliferate without logical contradiction — the constraint is less extractive than suppression metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(radical_reformation_foreclosure, conceptual, 'Logical relationship between mainline and radical Reformation readings of scripture').

omega_variable(
    kernel_substrate_existence,
    'Is there a single shared kernel (Scripture, Religious Authority, Salvation) that grounds all competing readings, or are the readings so structurally divergent that they presuppose different kernels?',
    'Formal analysis of reading presuppositions: what must be true about scripture for each reading to hold? Do the readings share axioms or presuppose incompatible premises about textuality, interpretation, and authority?',
    'If shared kernel: readings form a legitimate kernel family (multiple readings of Scripture, Authority, Salvation). If no shared substrate: constraint resists kernel decomposition — multiple independent causal chains converging without common origin.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_substrate_existence, conceptual, 'Whether Reformation exhibits kernel-family structure or resists kernel decomposition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(scriptural_authority_swap, 0, 131).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scrip_tr_t0, scriptural_authority_swap, theater_ratio, 0, 0.25).
narrative_ontology:measurement(scrip_tr_t20, scriptural_authority_swap, theater_ratio, 20, 0.55).
narrative_ontology:measurement(scrip_tr_t40, scriptural_authority_swap, theater_ratio, 40, 0.68).
narrative_ontology:measurement(scrip_tr_t80, scriptural_authority_swap, theater_ratio, 80, 0.62).
narrative_ontology:measurement(scrip_tr_t100, scriptural_authority_swap, theater_ratio, 100, 0.48).
narrative_ontology:measurement(scrip_tr_t131, scriptural_authority_swap, theater_ratio, 131, 0.35).

% Extraction over time
narrative_ontology:measurement(scrip_be_t0, scriptural_authority_swap, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(scrip_be_t20, scriptural_authority_swap, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(scrip_be_t40, scriptural_authority_swap, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(scrip_be_t80, scriptural_authority_swap, base_extractiveness, 80, 0.51).
narrative_ontology:measurement(scrip_be_t100, scriptural_authority_swap, base_extractiveness, 100, 0.45).
narrative_ontology:measurement(scrip_be_t131, scriptural_authority_swap, base_extractiveness, 131, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(scriptural_authority_swap, identity_coordination).
narrative_ontology:affects_constraint(scriptural_authority_swap, print_technology_access_bottleneck).
narrative_ontology:affects_constraint(scriptural_authority_swap, ecclesiastical_authority_recovery).
narrative_ontology:affects_constraint(scriptural_authority_swap, radical_reformation_containment).

% DUAL FORMULATION NOTE:
% Scriptural_authority_swap is upstream of multiple downstream constraints. Print technology created access bottleneck (constraint on who could produce and distribute Bibles). Ecclesiastical hierarchy's response mechanisms (Council of Trent, Counter-Reformation institutions) form a separate constraint with their own extractiveness and theater properties. Radical Reformation movements (Anabaptism, Spiritualism) create a containment constraint distinct from the mainline authority swap. All three are structurally linked: the authority swap enables print access bottleneck; both enable radical reformism; hierarchy's response targets radical containment. But each has distinct ε values and structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(scriptural_authority_swap, institutional, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
