% ============================================================================
% CONSTRAINT STORY: border_normative_status__qualified_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__qualified_sovereignty, []).

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
 *   constraint_id: border_normative_status__qualified_sovereignty
 *   human_readable: Border Control Authority with Proportionality Constraints (Qualified Sovereignty Reading)
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   The qualified sovereignty reading of the border normative status kernel
 *   asserts that states retain legitimate border control authority but must
 *   exercise it proportionately to genuine state interests and in consistency
 *   with binding human rights obligations. This reading occupies the middle
 *   position in a three-way normative contest: between sovereignty_primary
 *   (borders are foundational instruments of collective self-determination;
 *   exclusion is inherent), freedom_primary (freedom of movement is a
 *   fundamental human right that borders impermissibly restrict), and
 *   qualified_sovereignty (borders are legitimate tools subject to
 *   proportionality constraints and human rights review). The reading creates
 *   a complex tangled rope structure: genuine coordination functions exist
 *   (states do solve collective action problems by defining membership and
 *   pooling resources), but these coexist with asymmetric extraction
 *   (migrants and displaced persons bear the costs of border enforcement;
 *   states receive the security and resource-distribution benefits). The
 *   proportionality standard is presented as a governance tool balancing
 *   these interests, but empirical analysis reveals the standard
 *   systematically advantages state enforcement interests — proportionality
 *   determinations favor border reinforcement far more often than border
 *   relaxation. The theater ratio (0.58) reflects that extensive legal
 *   scholarship and international human rights machinery appear to perform
 *   robust normative work on proportionality, but this interpretive layer
 *   largely ratifies state border claims rather than meaningfully
 *   constraining them. The suppression trajectory (0.58 → 0.65) shows rising
 *   enforcement burden over the measurement interval as migration pressures
 *   increase and states tighten border controls in response to security
 *   framing.
 *
 * KEY AGENTS:
 *   - Nation-states (institutional/arbitrage): Primary beneficiaries — retain border control authority, extract security benefits and resource distribution advantages, experience constraints as legitimate governance framework
 *   - Security apparatus (institutional/arbitrage): Secondary beneficiary — police, border enforcement, immigration courts expand in authority and resources under qualified sovereignty framing
 *   - Excluded migrants (powerless/trapped): Primary victim — cannot exit the constraint; face deportation, exclusion from entry, suppressed asylum claims
 *   - Displaced citizens within border (moderate/constrained): Secondary victim — experience internal displacement pressure and state-controlled movement restrictions; also benefit from state-provided security
 *   - International human rights coalition (organized/constrained): Adjudicatory institution — enforces proportionality standard but constrained by state sovereignty claims and asymmetric adjudication burden
 *   - International legal scholarship (institutional/arbitrage): Interpretive layer — performs legitimacy work on proportionality but systematically favors state interests; theater ratio 0.58 reflects gap between normative discourse and adjudicatory outcomes
 *   - Analytical observer (analytical/analytical): Risks naturalizing the nation-state system as inevitable rather than contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__qualified_sovereignty, 0.58).
domain_priors:suppression_score(border_normative_status__qualified_sovereignty, 0.65).
domain_priors:theater_ratio(border_normative_status__qualified_sovereignty, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, extractiveness, 0.58).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__qualified_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_normative_status__qualified_sovereignty, "Border Control Authority with Proportionality Constraints (Qualified Sovereignty Reading)").
narrative_ontology:topic_domain(border_normative_status__qualified_sovereignty, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_normative_status__qualified_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__qualified_sovereignty, 'd1506ad0-466d-47b3-bcc7-ece01d385e5d').
narrative_ontology:cs_kernel_codification('d1506ad0-466d-47b3-bcc7-ece01d385e5d', formalized).
narrative_ontology:cs_authority_grounding('d1506ad0-466d-47b3-bcc7-ece01d385e5d', extraction).
narrative_ontology:cs_interpretation_layer_present('d1506ad0-466d-47b3-bcc7-ece01d385e5d').
narrative_ontology:cs_reading_relation('d1506ad0-466d-47b3-bcc7-ece01d385e5d', border_normative_status__sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('d1506ad0-466d-47b3-bcc7-ece01d385e5d', border_normative_status__freedom_primary, coexists_with).
narrative_ontology:cs_axiom('d1506ad0-466d-47b3-bcc7-ece01d385e5d', foundational, proportionality_constrained_authority).
narrative_ontology:cs_axiom_status(proportionality_constrained_authority, holdable).
narrative_ontology:cs_axiom_grounding('d1506ad0-466d-47b3-bcc7-ece01d385e5d', proportionality_constrained_authority, deontological).
narrative_ontology:cs_axiom('d1506ad0-466d-47b3-bcc7-ece01d385e5d', foundational, human_rights_obligations_binding).
narrative_ontology:cs_axiom_status(human_rights_obligations_binding, holdable).
narrative_ontology:cs_axiom_grounding('d1506ad0-466d-47b3-bcc7-ece01d385e5d', human_rights_obligations_binding, conventional).
narrative_ontology:cs_reference_frame('d1506ad0-466d-47b3-bcc7-ece01d385e5d', bounded_state_authority).
narrative_ontology:cs_drift_state('d1506ad0-466d-47b3-bcc7-ece01d385e5d', contemporary_migration_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d1506ad0-466d-47b3-bcc7-ece01d385e5d', '').
narrative_ontology:cs_kernel_id(border_normative_status__qualified_sovereignty, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, nation_states).
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, security_apparatus).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, displaced_persons).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, asylum_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED MIGRANTS (SNARE) — Cannot exit the constraint; territorial borders are enforced with violence and legal prohibition. Suppression is absolute — alternative routes are closed, asylum adjudication systems are designed to maximize denial, and deportation machinery is highly efficient. Maximum experienced extraction with minimal coordination benefit. Theater ratio reflects that formal proportionality review exists on paper but is systematized to produce border reinforcement, not genuine balancing.
constraint_indexing:constraint_classification(border_normative_status__qualified_sovereignty, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DISPLACED CITIZENS WITHIN BORDER (TANGLED ROPE) — Citizens face internal displacement pressure (eviction, environmental catastrophe, communal violence) and experience border control as both protective (state maintains internal order facilitating exit to other regions) and extractive (state controls movement, restricts relocation rights, privileges certain population groups in redistribution). Mixed experience — genuine coordination function (state provision of safe passage, coordination of resettlement) coexists with asymmetric extraction (some citizens preferred over others, extraction of loyalty in exchange for security).
constraint_indexing:constraint_classification(border_normative_status__qualified_sovereignty, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NATION-STATE (ROPE) — State experiences border control as coordination mechanism solving collective action problems: defining membership, pooling defense resources, distributing public goods to members. Proportionality constraints are perceived as legitimate governance tools, not extractive overhead. The state has arbitrage options — it can negotiate bilateral agreements, participate in regional coordination, or exit international human rights regimes. Low effective extraction from the state's perspective because it benefits from border control and experiences constraints as coordination rather than coercion.
constraint_indexing:constraint_classification(border_normative_status__qualified_sovereignty, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: INTERNATIONAL HUMAN RIGHTS COALITION (TANGLED ROPE) — Organized agents (UN bodies, regional courts, NGO networks) see qualified sovereignty as a coordination mechanism with genuine human rights protection function, but also as extracted legitimacy for state border enforcement. The proportionality standard creates adjudication burden on states while systematically favoring state interests in ambiguous cases (see omega: proportionality_directionality). These agents are constrained by their reliance on state cooperation for enforcement and by the fundamental asymmetry between the right-holders (dispersed migrants, weak bargaining power) and the duty-bearers (sovereign states with police power). Measured chi reflects that the proportionality framework provides some protection while still enabling systematic extraction.
constraint_indexing:constraint_classification(border_normative_status__qualified_sovereignty, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL LEGAL SCHOLARSHIP (PITON) — Academic discourse on qualified sovereignty appears to perform robust normative work (balancing, proportionality analysis, human rights integration) but largely ratifies state interests in border enforcement. Scholarship has high theater ratio — extensive discussion of proportionality principles, rights frameworks, and state obligations exists, but proportionality determinations systematically favor state exclusion claims. The interpretive layer (legal scholarship) persists through institutional inertia (law schools, journals, professional networks) while the actual adjudicatory power resides with states and regional courts that apply proportionality with minimal constraint on border enforcement.
constraint_indexing:constraint_classification(border_normative_status__qualified_sovereignty, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN? / FALSE SUMMIT CANDIDATE) — From a civilizational perspective, borders are sometimes presented as natural, inevitable features of political organization — territorial boundaries are inherent to any collective self-governance project, and exclusion is simply the logical corollary of inclusion. This perspective risks naturalizing what is actually a contingent institutional arrangement (the territorial nation-state system, the Westphalian settlement). The qualified sovereignty reading is offered as a naturalization candidate: 'States must retain border authority; proportionality is the natural compromise.' However, the structural data reveals this is contingent: the nation-state system is historically recent, alternatives exist (open borders, global citizenship, fluid jurisdictions), and beneficiaries (states, security apparatus) exist. False summit detection will apply.
constraint_indexing:constraint_classification(border_normative_status__qualified_sovereignty, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__qualified_sovereignty_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(border_normative_status__qualified_sovereignty, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(border_normative_status__qualified_sovereignty, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__qualified_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(border_normative_status__qualified_sovereignty, TR),
    TR >= 0.70.

:- end_tests(border_normative_status__qualified_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. States extract significant benefits from border control — security, resource distribution control, membership privileges — while migrants bear maximum costs of exclusion. The proportionality standard creates the appearance of balanced governance while systematically protecting state interests in empirical adjudication (omega: proportionality_directionality). The measured value reflects the hybrid nature: genuine coordination functions exist (defining membership does solve collective action problems), but these coexist with substantial asymmetric extraction. Without the coordination function, this would be a snare (ε ≥ 0.66); with it, tangled_rope (0.40 ≤ χ ≤ 0.90). Suppression (0.65): High. Borders are enforced with police power and legal prohibition. Asylum systems are systematized to maximize denial (adjudication burden omega). Alternative routes are closed. Deportation machinery is efficient. Suppression rises over the interval as migration pressures increase and states tighten enforcement in response to security narratives. Theater ratio (0.58): Moderate-high. The proportionality framework creates extensive legal discourse, scholarly analysis, and international human rights review — all appear to perform normative work. But adjudication systematically favors state enforcement claims. The piton perspective (perspective 5) captures this: the interpretive layer (scholarship, human rights bodies) persists through institutional channels while actual constraints on state action remain minimal.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exemplifies deep perspectival divergence. Excluded migrants (powerless/trapped) experience a snare — enforcement with no exit and no meaningful proportionality review that constrains their exclusion. The state (institutional/arbitrage) experiences rope — border control as legitimate coordination solving membership and resource problems. The international human rights coalition (organized/constrained) experiences tangled rope — both coordination (defining asylum standards) and extraction (adjudication burdens fall on migrants, proportionality favors states). International legal scholarship (institutional/arbitrage) appears to perform robust normative work but is actually piton — the discourse persists through institutional inertia while substantive constraints on state enforcement remain minimal. The analytical observer risks the false summit: naturalizing borders as inherent to governance rather than recognizing them as contingent institutional arrangements whose qualification through proportionality is itself a form of extraction legitimation. The reading contest (omega: kernel_reading_contest) is located in which framing of 'what borders are' prevails in international law adjudication.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural positions in the border control constraint. Excluded migrants have maximum d (0.95 — full extraction targets) because they are victims with trapped exit options; they experience the highest f(d) ≈ 1.42. Nation-states have low d (0.15 — beneficiaries with arbitrage options) because they design and benefit from borders while retaining exit paths (bilateral negotiations, treaty withdrawal); they experience negative or minimal f(d). Displaced citizens have moderate d (0.60 — mixed victim/benefit status with constrained exit) because they experience both protection (state security) and extraction (movement control); moderate f(d) ≈ 0.90. International human rights coalition has moderate-high d (0.68 — nominally advocates for migrants but constrained by state cooperation) because they are organized but lack enforcement power; f(d) ≈ 1.10. The perspectival gap flows from these different d values and the structural relationships they encode — the constraint appears radically different depending on whether you occupy the position of a trapped migrant or an arbitrage-capable state.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_directionality,
    'Does the proportionality standard systematically advantage state border enforcement interests over migrant entry interests?',
    'Quantitative analysis of proportionality determinations: count of human rights bodies (courts, committees) ruling in favor of migrants vs. states when proportionality is the controlling standard; track how often proportionality is found to require border relaxation vs. border reinforcement',
    'If systematically favoring states: the proportionality framework is itself an extraction mechanism (legitimacy cover for border enforcement), raising extractiveness from 0.58 to 0.68+ and potentially reclassifying snare perspectives from victims'' view. If genuinely balanced: extractiveness drops to 0.45 and perspectives shift toward tangled_rope stability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_directionality, empirical, 'Whether proportionality standard is neutral or systematically favors state border enforcement').

omega_variable(
    adjudication_burden_asymmetry,
    'Who bears the burden and cost of adjudicating proportionality determinations, and does this asymmetry constitute extraction?',
    'Institutional capacity analysis: resources available to international human rights bodies vs. state legal systems for conducting proportionality review; timeline and cost barriers for migrants to access adjudication vs. states'' resources for border enforcement litigation',
    'If migrants bear disproportionate adjudication burden: suppression rises (difficulty accessing review mechanisms), extraction rises (the framework requires migrants to prove disproportionality rather than states to justify proportionality), reclassifying tangled_rope toward snare. If burden is shared: framework retains tangled_rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adjudication_burden_asymmetry, empirical, 'Asymmetry in who bears adjudication costs for proportionality review').

omega_variable(
    alternative_framing_feasibility,
    'If the freedom_primary reading were institutionalized (freedom of movement as presumptive right, exclusion as extraordinary exception requiring state proof), would border control remain functionally viable or would the constraint collapse?',
    'Counterfactual policy analysis: design a migration regime where movement is presumptively free and exclusion is the exception; model state capacity to maintain security and social service provision under this regime; compare to empirical data from open-border or highly permissive migration contexts',
    'If functionally viable: the qualified_sovereignty reading''s claim that border control is a necessary coordination mechanism is empirically contingent, not inherent. If not viable: the reading''s core premise (states can simultaneously enforce borders and respect proportionality) may be illusory, and the constraint is better classified as snare disguised as tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_framing_feasibility, empirical, 'Feasibility of freedom-primary alternative framing').

omega_variable(
    kernel_reading_contest,
    'Which reading of the border normative status kernel — freedom_primary, qualified_sovereignty, or sovereignty_primary — correctly describes the actual normative status of border control in contemporary international law?',
    'This is a kernel contest omega, not empirically resolvable. Different readings capture distinct framings of what borders ARE and SHOULD BE. The reading contest is located in how international law institutions (courts, treaty bodies, scholarly consensus) adjudicate claims of legitimate exclusion.',
    'This omega records the irreducible normative contest. The qualified_sovereignty reading asserts that proportionality balancing is the right framework; freedom_primary asserts that movement rights are presumptive; sovereignty_primary asserts that territorial authority is foundational. The engine''s reading_relations and axioms fields encode the structural relationships between readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading correctly characterizes the border normative status kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__qualified_sovereignty, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(border_qs_tr_t0, border_normative_status__qualified_sovereignty, theater_ratio, 0, 0.42).
narrative_ontology:measurement(border_qs_tr_t3, border_normative_status__qualified_sovereignty, theater_ratio, 3, 0.5).
narrative_ontology:measurement(border_qs_tr_t6, border_normative_status__qualified_sovereignty, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(border_qs_be_t0, border_normative_status__qualified_sovereignty, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(border_qs_be_t3, border_normative_status__qualified_sovereignty, base_extractiveness, 3, 0.54).
narrative_ontology:measurement(border_qs_be_t6, border_normative_status__qualified_sovereignty, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(border_qs_su_t0, border_normative_status__qualified_sovereignty, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(border_qs_su_t3, border_normative_status__qualified_sovereignty, suppression_requirement, 3, 0.62).
narrative_ontology:measurement(border_qs_su_t6, border_normative_status__qualified_sovereignty, suppression_requirement, 6, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__qualified_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, border_normative_status__freedom_primary).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, border_normative_status__sovereignty_primary).

% DUAL FORMULATION NOTE:
% The border_normative_status kernel has three distinct structural readings with different ε values and victim/beneficiary structures. The qualified_sovereignty reading (this constraint) has ε=0.58 and treats both excluded migrants and displaced citizens as victims. The freedom_primary reading will have higher ε (0.62–0.70) with movement freedom as the baseline and border control as extraction. The sovereignty_primary reading will have lower ε (0.35–0.45) with state authority as baseline and constraints as coordination burden. These are not three views of one constraint — they are three structurally distinct constraints sharing a contested kernel. All three stories must be authored as separate JSON files and linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
