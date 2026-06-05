% ============================================================================
% CONSTRAINT STORY: governance__direct_democracy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_governance__direct_democracy, []).

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
 *   constraint_id: governance__direct_democracy
 *   human_readable: Direct Democracy Governance (Assembled Citizens Authority)
 *   domain: political/legal
 *
 * SUMMARY:
 *   Direct democracy as a governance principle asserts that legitimate
 *   authority derives solely from the assembled citizens themselves and that
 *   any delegation of that authority constitutes a usurpation to be
 *   minimized. This reading of the governance kernel instantiates a
 *   constraint that exhibits the full spectrum of Deferential Realism types
 *   depending on observer position. The constraint coordinates the majority's
 *   collective will while systematically suppressing protection for
 *   persistent minorities. It benefits the mobilized majority and civic
 *   entrepreneurs who control agenda-setting, while imposing high costs on
 *   those with no effective voice in the assembly — excluded populations,
 *   historically marginalized minorities, and those unable to participate in
 *   constant civic engagement. The extractiveness rises over time as the
 *   polity scales from face-to-face deliberation to mass voting, the theater
 *   ratio increases as ritual deliberation replaces functional deliberation,
 *   and suppression intensifies as the majority finds mechanisms to insulate
 *   itself from counter-majoritarian challenge.
 *
 * KEY AGENTS:
 *   - Mobilized Majority: Primary beneficiary (organized/mobile) — experiences pure coordination through assembly mechanism; captures binding authority over collective decisions
 *   - Persistent Minorities: Primary victim (powerless/trapped) — no institutional shelter from majority will; no counter-majoritarian veto or constitutional protection; systematic suppression of voice and protection
 *   - Excluded Populations: Secondary victim (powerless/trapped) — populations unable to participate in assembly (non-residents, non-citizens, disenfranchised, those without civic capacity) bear costs of majority decisions without voice
 *   - Civic Entrepreneur / Rhetor: Powerful beneficiary (powerful/mobile) — shapes agenda, controls framing, concentrates influence through rhetorical mastery; benefits from both coordination and extraction
 *   - Episodic Participant: Moderate agent (moderate/constrained) — experiences both benefit (voice in assembly) and cost (participation burden, majority tyranny when not mobilized); constrained by attention limitations and majority dominance
 *   - Deliberative Institution (Historical): Institutional actor (institutional/constrained) — maintains the assembly form for legitimacy even as actual deliberation degrades at scale; constrained by the need to preserve appearance of popular participation while managing actual governance through delegation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(governance__direct_democracy, 0.58).
domain_priors:suppression_score(governance__direct_democracy, 0.72).
domain_priors:theater_ratio(governance__direct_democracy, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(governance__direct_democracy, extractiveness, 0.58).
narrative_ontology:constraint_metric(governance__direct_democracy, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(governance__direct_democracy, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(governance__direct_democracy, tangled_rope).
narrative_ontology:human_readable(governance__direct_democracy, "Direct Democracy Governance (Assembled Citizens Authority)").
narrative_ontology:topic_domain(governance__direct_democracy, "political/legal").

domain_priors:requires_active_enforcement(governance__direct_democracy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(governance__direct_democracy, '6bc4d3b3-d915-4733-b0b3-d8dec90919d8').
narrative_ontology:cs_kernel_codification('6bc4d3b3-d915-4733-b0b3-d8dec90919d8', formalized).
narrative_ontology:cs_authority_grounding('6bc4d3b3-d915-4733-b0b3-d8dec90919d8', extraction).
narrative_ontology:cs_reading_relation('6bc4d3b3-d915-4733-b0b3-d8dec90919d8', governance__autocratic_rule, forecloses).
narrative_ontology:cs_reading_relation('6bc4d3b3-d915-4733-b0b3-d8dec90919d8', governance__constitutional_government, coexists_with).
narrative_ontology:cs_reading_relation('6bc4d3b3-d915-4733-b0b3-d8dec90919d8', governance__customary_rule, coexists_with).
narrative_ontology:cs_reading_relation('6bc4d3b3-d915-4733-b0b3-d8dec90919d8', governance__theocratic_rule, forecloses).
narrative_ontology:cs_axiom('6bc4d3b3-d915-4733-b0b3-d8dec90919d8', foundational, authority_from_assembled_citizens_only).
narrative_ontology:cs_axiom_status(authority_from_assembled_citizens_only, holdable).
narrative_ontology:cs_axiom_grounding('6bc4d3b3-d915-4733-b0b3-d8dec90919d8', authority_from_assembled_citizens_only, deontological).
narrative_ontology:cs_axiom('6bc4d3b3-d915-4733-b0b3-d8dec90919d8', foundational, delegation_is_usurpation).
narrative_ontology:cs_axiom_status(delegation_is_usurpation, holdable).
narrative_ontology:cs_axiom_grounding('6bc4d3b3-d915-4733-b0b3-d8dec90919d8', delegation_is_usurpation, deontological).
narrative_ontology:cs_reference_frame('6bc4d3b3-d915-4733-b0b3-d8dec90919d8', popular_sovereignty_principle).
narrative_ontology:cs_drift_state('6bc4d3b3-d915-4733-b0b3-d8dec90919d8', industrial_scale_modernity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6bc4d3b3-d915-4733-b0b3-d8dec90919d8', '').
narrative_ontology:cs_kernel_id(governance__direct_democracy, governance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(governance__direct_democracy, mobilized_majority).
narrative_ontology:constraint_beneficiary(governance__direct_democracy, participating_citizens).
narrative_ontology:constraint_victim(governance__direct_democracy, persistent_minorities).
narrative_ontology:constraint_victim(governance__direct_democracy, excluded_populations).
narrative_ontology:constraint_victim(governance__direct_democracy, deliberative_capacity_constraints).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERSISTENT MINORITY (SNARE) — Trapped by direct majoritarian rule with no constitutional shelter or counter-majoritarian institution. The assembled citizens can vote away minority rights, protections, or resources. High suppression of exit (no appeal to higher law); high extraction (majority appetite is the only limit). Cannot exit the jurisdiction without material loss; cannot block decisions through institutional design.
constraint_indexing:constraint_classification(governance__direct_democracy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EPISODICALLY MOBILIZED CITIZEN (TANGLED ROPE) — Experiences genuine coordination (assembly makes collective decisions binding on all) but also asymmetric extraction: participation requires constant attention and preparation; non-participation costs voice; the majority's preference becomes binding. Benefits from direct voice when mobilized but bears cost of perpetual civic engagement or systematic exclusion from decisions.
constraint_indexing:constraint_classification(governance__direct_democracy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ORGANIZED MAJORITY COALITION (ROPE) — Experiences the constraint as pure coordination: assembly enables the majority to aggregate preferences and enforce binding decisions. No barrier to exit (can defect to alternative governing arrangements if majority loses cohesion). High agency; experiences extraction only if internal coalition discipline fails. Direct democracy is a coordination mechanism for this agent — it solves the collective action problem of who governs.
constraint_indexing:constraint_classification(governance__direct_democracy, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: DELIBERATIVE INSTITUTION / HISTORICAL VIEW (PITON) — The assembly ritual persists as the legitimacy mechanism for governance even when actual deliberation is impossible at scale. As the polity grows beyond face-to-face assembly (scaling from 500 to 500,000 citizens), the direct democracy form remains but the substance degrades: voting replaces deliberation, media cycles replace collective reasoning, abstraction replaces presence. The institutional form is maintained for legitimacy (it looks like the people govern) while the actual mechanism is delegation. Theater ratio: high. Functional deliberation: minimal. The assembly survives through inertia and symbolic power, not through restored deliberative capacity.
constraint_indexing:constraint_classification(governance__direct_democracy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CIVIC ENTREPRENEUR / RHETOR (TANGLED ROPE) — Coordinating role: organizes the assembly, frames issues, enables collective decision-making. Extractive role: controls the agenda, shapes the frame, and benefits disproportionately from influence over what the majority votes on. Has mobile exit (can withdraw from politics) and powerful position (can shape majority preference). The constraint both enables and extracts: it coordinates the majority's will AND concentrates power in those who control persuasion and agenda-setting.
constraint_indexing:constraint_classification(governance__direct_democracy, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, direct democracy is an immutable principle: legitimate authority can only flow from those subject to it, and any other principle (hereditary rule, constitutional limits, divine delegation, custom) is a violation of that axiom. This perspective naturalizes the direct democracy principle as a logical necessity. However, the structural data contradicts the mountain classification: the high suppression of minorities, the extractiveness from rhetorical control, and the beneficiary status of the mobilized majority reveal that 'pure popular sovereignty' is a contingent institutional arrangement, not a natural law. The engine's false summit detector will flag this as naturalization.
constraint_indexing:constraint_classification(governance__direct_democracy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(governance__direct_democracy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(governance__direct_democracy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(governance__direct_democracy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(governance__direct_democracy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(governance__direct_democracy, TR),
    TR >= 0.70.

:- end_tests(governance__direct_democracy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint benefits the mobilized majority while extracting from persistent minorities and excluded populations. The extractiveness is not maximal (0.72+) because some genuine coordination benefit flows from the assembly mechanism itself — collective decisions are binding and binding decisions do solve coordination problems. But the benefit concentrates in the majority while costs concentrate in minorities, creating significant asymmetric extraction. The measurement trajectory shows extractiveness rising from 0.42 (early assembly phase with small, homogeneous polity) to 0.68 (large-scale polity where deliberation is impossible and voting replaces reasoning). Suppression (0.72): High. The direct democracy reading explicitly minimizes delegation and thus provides no counter-majoritarian shelter — no constitutional limits, no minority veto, no institutional refuge from majority will. The core axiom requires suppression of institutional alternatives that might protect minorities. Suppression is both structural (no institutions exist to protect minorities) and enforced (the majoritarian assembly will actively resist creation of such institutions as 'usurpation'). Suppression intensity rises over time as the majority develops mechanisms to formalize its control. Theater ratio (0.55–0.72): Moderate to high. At small scale (assembly of 500), deliberation is functional and theater is lower. At large scale (voting of millions), the assembly ritual persists for legitimacy while actual policy is made through delegation and bureaucracy; theater rises as the form disconnects from the function.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between the mobilized majority's rope experience and the persistent minority's snare experience is not a measurement disagreement — it is the structural reality of direct democracy. Both perspectives are correct within their observational contexts. The majority genuinely experiences coordination (the assembly binds everyone and makes collective decisions possible). The minority genuinely experiences extraction with no exit (the assembled majority can vote away their protections and they have no constitutional appeal). The gap is not a communication problem — it is the logical consequence of the reading's core axiom (no delegation = no counter-majoritarian shelter = minority suppression). The piton perspective adds a temporal dimension: the assembly form persists as legitimacy theater even when actual deliberation is impossible, revealing a gap between the reading's aspirations (assembled citizens themselves) and the reading's actual function (delegated decision-making through voting rituals).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural position in the extraction flow. The mobilized majority are beneficiaries with mobile exit (can abandon direct democracy for alternative forms) — low d → negative χ. Persistent minorities are victims with trapped exit (cannot exit without migration) — high d → high χ. The civic entrepreneur is a powerful beneficiary with mobile exit but also partially a victim of their own rhetorical competition — moderate d reflecting mixed position. The episodic participant is constrained both by participation burden and by majority dominance — d reflects simultaneous benefit (voice) and cost (tyranny). The analytical observer is at d=0.73 (canonical for analytical position), but the false summit flag suggests that this observer risks naturalizing a contingent reading as a universal law.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by clarifying what direct democracy actually coordinates and what it actually extracts. It coordinates the majority's binding authority and enables aggregate preference enforcement. It extracts from minorities and excluded populations by denying them counter-majoritarian shelter. The tangled rope classification is stable: the constraint has a genuine coordination function (enabling the majority to make binding collective decisions) AND a genuine extraction mechanism (systematically suppressing minority protection). The classical mandatrophy confusion — is this a coordination tool or an extraction mechanism? — is resolved by recognizing that it IS both, and the beneficiary set (majority) overlaps only partially with the victim set (minorities), making the net classification tangled rope rather than rope or snare. The piton perspective captures a second mandatrophy: the assembly form survives for legitimacy but degraded function, creating a tension between 'we the people assembled' and 'actually we the people voting' that the constraint partially occludes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scale_deliberation_tradeoff,
    'Can direct democracy at scale (nation-state populations) maintain authentic deliberation, or does assembly become a ritual legitimating delegation?',
    'Comparison of actual deliberation time, reasoning depth, and minority voice integration in face-to-face assemblies (500–5,000 participants) vs mass voting systems (5,000+ participants); measurement of information quality and preference formation patterns',
    'If authentic deliberation persists at scale: majority extraction is constrained by collective reasoning and minority perspective integration. If assembly becomes ritual: constraint reclassifies toward snare or piton (high suppression, high theater, low functional deliberation). This determines whether the tangled_rope classification is stable across temporal horizons.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scale_deliberation_tradeoff, empirical, 'Whether authentic deliberation scales with direct democracy').

omega_variable(
    minority_protection_mechanism,
    'Is suppression of minority protection (no counter-majoritarian shelter) inherent to direct democracy, or is it a design choice within the reading?',
    'Historical/comparative analysis: do direct democracies with internal protections (super-majority requirements for rights-affecting decisions, minority veto triggers, deliberative cooling-off periods) remain direct democracies? What mechanisms preserve minority voice without foreclosing majority rule?',
    'If protection is compatible: suppression can be engineered downward; constraint reclassifies toward rope or scaffold. If protection forecloses pure majoritarianism: the ''any delegation is usurpation'' axiom forecloses compromise forms. This determines whether the direct_democracy reading logically excludes designs with minority shelter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_protection_mechanism, conceptual, 'Whether minority protection is compatible with direct democracy axiom').

omega_variable(
    majority_appetite_extraction_relationship,
    'Does extractiveness track majority appetite (more extraction when mobilized majority is hungry for redistribution/oppression) or is it constant structural property of the form?',
    'Longitudinal measurement of majority coalition composition, redistribution appetite, and minority burden across election cycles; correlation between majority sentiment and actual minority extraction; distinction between structural extraction (always present) and appetite-driven extraction (varies with majority preference)',
    'If appetite-driven: extractiveness is observable-dependent (different under different majority coalitions); constraint family should decompose into separate stories per coalition type. If structural: extractiveness is stable and independent of majority sentiment. This determines whether ε=0.58 is invariant or observable-relative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majority_appetite_extraction_relationship, empirical, 'Whether extraction is appetite-driven or structurally constant').

omega_variable(
    delegation_minimization_mechanism,
    'How does direct democracy institutionally minimize delegation without collapsing into role differentiation? If some citizens must coordinate at scale (agenda-setting, vote administration, implementation), at what point does delegation become necessary rather than corrupt?',
    'Study of actual delegation patterns in functioning direct democracies; distinction between legitimate functional roles (vote counting, agenda compilation) and usurpation (policy interpretation, selective enforcement, agenda control); identification of the minimum delegation required for scale.',
    'If delegation is minimizable: constraint is stable; civic entrepreneurs can be constrained. If delegation is unavoidable at scale: the reading''s core axiom (delegation is usurpation) becomes incompatible with large-scale polities; the constraint reclassifies toward constitutional government or scaffold with sunset clause at the scale transition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(delegation_minimization_mechanism, empirical, 'Minimization of delegation vs functional necessity').

omega_variable(
    kernel_contest_foreclosure,
    'Does the direct_democracy reading logically foreclose the other readings (autocratic, constitutional, customary, theocratic) or do they coexist as live options held by different parties?',
    'Philosophical and historical analysis: can a single polity hold both direct democracy (no delegation) and constitutional government (delegation bounded by higher law) within the same legitimacy framework? Can citizens choose theocratic authority while reserving popular sovereignty? Do these readings compete within polities or across them?',
    'If the reading forecloses siblings: reading_relations should use ''forecloses''. If coexistence is possible: use ''coexists_with''. This determines the strength of the contest and whether the sibling readings are genuine alternatives or incoherent positions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_foreclosure, conceptual, 'Logical relationship between direct democracy and sibling readings').

omega_variable(
    false_summit_popular_sovereignty_axiom,
    'Is the ''all authority from the assembled citizens'' axiom a natural law of legitimate government, or is it a contingent commitment that some polities have chosen and others can legitimately reject?',
    'Meta-analysis of how different governance traditions (theocratic, hereditary, customary, constitutional) justify their own legitimacy; examination of whether direct democracy is universally recognized as the foundation or one option among contested claims to legitimacy; study of whether rejection of direct democracy is understood as delegitimizing or merely alternative.',
    'If natural law: the mountain perspective is correct and the structure naturalizes a contingent claim. If contingent: the constraint is a false summit — the reading''s core axiom is a choice, not a law, and the engine''s false summit detector should flag the naturalization. This determines whether the analytical perspective''s mountain classification is validly grounded or is itself a form of extraction (naturalizing a particular reading''s axiom).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_summit_popular_sovereignty_axiom, conceptual, 'Natural law vs contingent choice status of popular sovereignty').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(governance__direct_democracy, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gove_tr_t0, governance__direct_democracy, theater_ratio, 0, 0.35).
narrative_ontology:measurement(gove_tr_t10, governance__direct_democracy, theater_ratio, 10, 0.55).
narrative_ontology:measurement(gove_tr_t20, governance__direct_democracy, theater_ratio, 20, 0.72).

% Extraction over time
narrative_ontology:measurement(gove_be_t0, governance__direct_democracy, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(gove_be_t10, governance__direct_democracy, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(gove_be_t20, governance__direct_democracy, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gove_su_t0, governance__direct_democracy, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(gove_su_t10, governance__direct_democracy, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(gove_su_t20, governance__direct_democracy, suppression_requirement, 20, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(governance__direct_democracy, enforcement_mechanism).
narrative_ontology:affects_constraint(governance__direct_democracy, governance__constitutional_government).
narrative_ontology:affects_constraint(governance__direct_democracy, governance__autocratic_rule).
narrative_ontology:affects_constraint(governance__direct_democracy, governance__customary_rule).
narrative_ontology:affects_constraint(governance__direct_democracy, governance__theocratic_rule).

% DUAL FORMULATION NOTE:
% The direct_democracy constraint is one reading of the governance kernel. Five constraint stories instantiate the five readings: each has its own base_properties, perspectives, and ε value. They are linked via network.affects_constraints to mark their membership in the constraint family. Do NOT combine them into a single story with measurement parameters or observable-dependent ε values. Each reading is a distinct constraint story with a stable ε representing how that reading actually functions as a governance mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(governance__direct_democracy, powerful, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
