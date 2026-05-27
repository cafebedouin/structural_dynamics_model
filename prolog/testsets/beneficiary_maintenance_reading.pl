% ============================================================================
% CONSTRAINT STORY: beneficiary_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_beneficiary_maintenance_reading, []).

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
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: beneficiary_maintenance_reading
 *   human_readable: Naturalization as Post-Hoc Ideological Defense by Incumbent Beneficiaries
 *   domain: political_economy/economic_ideology/institutional_design
 *
 * SUMMARY:
 *   This constraint models the use of naturalization discourse as a post-hoc
 *   ideological mechanism by which incumbent beneficiaries of existing market
 *   rules defend those rules against alternative designs without explicit
 *   enforcement or recognition of the defense mechanism. The reading
 *   instantiates one specific causal narrative: that 'market naturalism' is
 *   not an independent discovery about economic coordination, but a strategic
 *   deployment of essentialism by those who benefit from current property
 *   distributions. The constraint operates through the ideological apparatus
 *   (economics departments, policy institutions, business schools) which
 *   produces intellectual authority for the claim that markets and their
 *   current rules are natural/inevitable/unchangeable. This reading competes
 *   with two sibling readings in the market_as_natural_default kernel: (1)
 *   the spontaneous_order_reading, which sees market naturalism as an
 *   accurate description of emergent coordination properties without
 *   intentional beneficiary shaping, and (2) the
 *   engineered_infrastructure_reading, which sees markets as deliberately
 *   designed by explicitly acknowledged architect-agents (states,
 *   corporations, regulators) for specific distributional goals. The
 *   beneficiary_maintenance_reading occupies the middle ground: markets have
 *   both emergent and designed properties, but the contemporary consensus
 *   naturalizes what is actually designed, and this naturalization serves
 *   incumbent interests. The constraint exhibits high theater (0.78) because
 *   the ideological apparatus performs scholarly legitimacy while actually
 *   functioning as post-hoc rationalization. Both extractiveness (0.58) and
 *   suppression (0.65) are substantial but not maximal — the constraint has
 *   real coordination benefits (shared frameworks, analytical rigor) but also
 *   real asymmetric costs (suppression of alternatives, dispossession
 *   justified as natural).
 *
 * KEY AGENTS:
 *   - Incumbent Capital Holders: Primary beneficiary (institutional/arbitrage) — benefit from naturalization because it legitimizes current property distributions without requiring continuous explicit enforcement
 *   - Beneficiary Intellectual Apparatus: Primary beneficiary (institutional/arbitrage) — economics departments, policy think tanks, business schools that produce and enforce the naturalization narrative; benefit through funding flows, prestige, publication authority
 *   - Dispossessed Agents: Primary victim (powerless/trapped) — structurally excluded from market participation; told exclusion is natural law rather than contingent institutional choice; cannot exit because exit requires resources
 *   - Epistemic Commons / Market Alternatives: Secondary victim (analytical/trapped) — intellectual space where alternative market designs could be explored is suppressed; heterodox frameworks are excluded from peer review, funding, and graduate training
 *   - Reform-Minded Economists: Moderate actor (moderate/constrained) — face career barriers for adopting heterodox positions within mainstream institutions; benefit from some analytical coordination but bear suppression costs
 *   - Alternative Economics Coalition: Organized actors (organized/constrained) — heterodox economists, ecological economists, institutional economists building parallel intellectual infrastructure with genuine sunset logic
 *   - Analytical Observer: Neutral observer position (analytical/analytical) — risks false summitry by seeing naturalization as inevitable/universal rather than contingent/enforced
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beneficiary_maintenance_reading, 0.58).
domain_priors:suppression_score(beneficiary_maintenance_reading, 0.65).
domain_priors:theater_ratio(beneficiary_maintenance_reading, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beneficiary_maintenance_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(beneficiary_maintenance_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(beneficiary_maintenance_reading, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beneficiary_maintenance_reading, tangled_rope).
narrative_ontology:human_readable(beneficiary_maintenance_reading, "Naturalization as Post-Hoc Ideological Defense by Incumbent Beneficiaries").
narrative_ontology:topic_domain(beneficiary_maintenance_reading, "political_economy/economic_ideology/institutional_design").

domain_priors:requires_active_enforcement(beneficiary_maintenance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beneficiary_maintenance_reading, '2f4e071f-502b-4b9d-8bb1-1099cc5328bf').
narrative_ontology:cs_created_at('2f4e071f-502b-4b9d-8bb1-1099cc5328bf', '').
narrative_ontology:cs_kernel_codification('2f4e071f-502b-4b9d-8bb1-1099cc5328bf', fixed_text).
narrative_ontology:cs_authority_grounding('2f4e071f-502b-4b9d-8bb1-1099cc5328bf', extraction).
narrative_ontology:cs_interpretation_layer_present('2f4e071f-502b-4b9d-8bb1-1099cc5328bf').
narrative_ontology:cs_kernel_id(beneficiary_maintenance_reading, market_as_natural_default).
narrative_ontology:cs_reading_relation('2f4e071f-502b-4b9d-8bb1-1099cc5328bf', spontaneous_order_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f4e071f-502b-4b9d-8bb1-1099cc5328bf', engineered_infrastructure_reading, influences).
narrative_ontology:cs_axiom('2f4e071f-502b-4b9d-8bb1-1099cc5328bf', foundational, market_naturalism_is_post_hoc_rationalization).
narrative_ontology:cs_axiom_status(market_naturalism_is_post_hoc_rationalization, holdable).
narrative_ontology:cs_axiom_grounding('2f4e071f-502b-4b9d-8bb1-1099cc5328bf', market_naturalism_is_post_hoc_rationalization, empirically_contingent).
narrative_ontology:cs_axiom('2f4e071f-502b-4b9d-8bb1-1099cc5328bf', foundational, suppression_of_alternatives_enables_naturalization).
narrative_ontology:cs_axiom_status(suppression_of_alternatives_enables_naturalization, holdable).
narrative_ontology:cs_axiom_grounding('2f4e071f-502b-4b9d-8bb1-1099cc5328bf', suppression_of_alternatives_enables_naturalization, empirically_contingent).
narrative_ontology:cs_reference_frame('2f4e071f-502b-4b9d-8bb1-1099cc5328bf', market_naturalism_as_discovery).
narrative_ontology:cs_drift_state('2f4e071f-502b-4b9d-8bb1-1099cc5328bf', contemporary_heterodox_resurgence, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beneficiary_maintenance_reading, incumbent_capital_holders).
narrative_ontology:constraint_beneficiary(beneficiary_maintenance_reading, beneficiary_intellectual_apparatus).
narrative_ontology:constraint_victim(beneficiary_maintenance_reading, epistemic_commons_market_alternatives).
narrative_ontology:constraint_victim(beneficiary_maintenance_reading, dispossessed_agents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE DISPOSSESSED AGENT (SNARE) — Structurally excluded from market participation; told the exclusion is natural law, not contingent design. Cannot exit the constraint because exit requires resources to enter markets. Experiences maximum extraction: material deprivation + ideological suppression of alternatives. No coordination benefit — the naturalization actively forecloses seeing the constraint as solvable.
constraint_indexing:constraint_classification(beneficiary_maintenance_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE REFORM-MINDED ECONOMIST (TANGLED ROPE) — Constrained by disciplinary norms, funding sources, and publication gates that enforce naturalization as orthodoxy. Benefits from some coordination function (shared analytical frameworks enable collaboration) but also bears extraction: genuine alternatives are suppressed in the peer review and funding apparatus. Can theoretically exit by adopting heterodox position, but faces severe career cost. Mixed experience of the constraint.
constraint_indexing:constraint_classification(beneficiary_maintenance_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE INCUMBENT BENEFICIARY (ROPE) — Institutional actor (large capital holders, finance sector, established firms) that benefits from market naturalization as coordination mechanism. The constraint solves the coordination problem: 'How do we maintain legitimacy for current property distributions without continuous enforcement?' Answer: convince everyone the distribution is natural. Net beneficiary — experiences the constraint as coordination rather than extraction. Can arbitrage between different institutional contexts.
constraint_indexing:constraint_classification(beneficiary_maintenance_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE IDEOLOGICAL APPARATUS AS INSTITUTION (PITON) — Economics departments, think tanks, policy boards claiming to study markets neutrally are substantially performative. The theater_ratio is high: much of academic and policy economics is post-hoc rationalization of incumbent preferences, dressed in mathematical formalism. The apparatus maintains itself through institutional inertia (prestigious journals, tenured positions, funded centers) despite degraded epistemic function. Reviewers and editors police orthodoxy as 'rigor,' not recognizing they are enforcing a reading.
constraint_indexing:constraint_classification(beneficiary_maintenance_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: THE ALTERNATIVE ECONOMICS COALITION (SCAFFOLD) — Organized agents (heterodox economists, institutional economics movements, post-Keynesian schools, ecological economics) see naturalization as a temporary institutional arrangement, not an eternal law. Open-access journals, graduate summer schools, and international networks are building parallel epistemic infrastructure with genuine sunset logic: as heterodox analysis becomes more rigorous and visible, the naturalization story loses force. High agency; sees exit path.
constraint_indexing:constraint_classification(beneficiary_maintenance_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: THE NATURALIZATION APPARENT (FALSE SUMMIT) — At the civilizational timescale and universal scope, one might frame the emergence of market systems and the subsequent ideological defense as inevitable features of human coordination at scale. Markets are 'natural' in the sense that they emerge from decentralized exchange; ideological defense is 'natural' in the sense that institutions always rationalize themselves. But this perspective risks false summitry: it naturalizes what is actually a contingent institutional choice (to prevent collective re-examination of market design) and what is actually an enforced orthodoxy (to suppress heterodox alternatives). The engine detects beneficiary presence and reclassifies as tangled_rope.
constraint_indexing:constraint_classification(beneficiary_maintenance_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beneficiary_maintenance_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(beneficiary_maintenance_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(beneficiary_maintenance_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(beneficiary_maintenance_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(beneficiary_maintenance_reading, TR),
    TR >= 0.70.

:- end_tests(beneficiary_maintenance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts by suppressing cognitive and institutional alternatives — heterodox economists face publication barriers, funding constraints, and disciplinary penalties. The dispossessed are told their dispossession is natural, not designed. But extractiveness is not maximal because markets do have genuine emergent/coordination properties; the naturalization is not pure fabrication, just selective emphasis and alternative-suppression. Suppression (0.65): Substantial. The constraint maintains itself through multiple mechanisms: (1) Peer review policing of orthodoxy as 'rigor,' (2) Funding concentration in mainstream institutions, (3) Graduate training that makes heterodoxy literally unthinkable to students, (4) Symbolic violence of mathematical formalism that appears neutral but embeds beneficiary-favorable assumptions, (5) Textbook repetition of 'fundamental theorems' that are actually contingent policy choices. Suppression is not absolute — heterodox alternatives exist and grow — but they bear much higher barriers. Theater ratio (0.78): High and rising. The measurement trajectory shows increasing theater over 50 years. At t=0 (post-Keynesian emergence), naturalization was less explicit; scholars openly debated whether markets were optimal. By t=20 (Chicago school dominance), naturalization became stronger — free market fundamentalism presented itself as scientific truth. At t=40 (contemporary), theater peaked at 0.78 — mainstream journals publish pages of regression analysis and mathematical modeling that appear rigorous but rest on unstated assumptions that favor incumbent beneficiaries (e.g., competitive rationality, stable preferences, frictionless information). At t=45 (recent heterodox resurgence), slight decline (0.54) as alternative frameworks gain some visibility and younger scholars explicitly challenge naturalization.
 *
 * PERSPECTIVAL GAP:
 *   Each perspective experiences the constraint fundamentally differently based on structural position. The dispossessed agent experiences pure snare: they are excluded and told the exclusion is natural. The reform-minded economist experiences tangled_rope: they benefit from analytical frameworks but face suppression for heterodoxy. The incumbent beneficiary experiences rope: the constraint solves their legitimacy problem by naturalizing their advantage. The ideological apparatus sees itself as piton: degraded ritual that persists through inertia. The alternative coalition sees scaffold: temporary institutional arrangement being bypassed by heterodox networks with sunset logic. The analytical observer risks false summitry by naturalizing what is actually contingent. The perspectival gap is maximal — from snare (powerless) to rope (institutional) — revealing that the classification depends entirely on structural position relative to the naturalization mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality is structured by who benefits from naturalization suppressing alternatives. Incumbent beneficiaries (d ≈ 0.10, low extraction experienced) use naturalization as coordination: 'How do we maintain property distributions without violence?' Answer: convince people it's natural. Beneficiary institutions (d ≈ 0.15) benefit from being the sole legitimate authority on 'natural' market operation. Dispossessed agents (d ≈ 0.92, high extraction) bear full cost: material deprivation + epistemic suppression of 'it could be different.' Reform-minded economists (d ≈ 0.68) are constrained but not trapped: they can theoretically adopt heterodoxy, but career costs are severe. The derivation chain yields high f(d) for victims and low f(d) for beneficiaries, producing asymmetric effective extraction chi despite moderate base extractiveness ε. Suppression is unscaled — it is a structural property (0.65) affecting all agents, not varying by power or scope.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy (coordination vs extraction ambiguity) by being explicitly hybrid: tangled_rope in its core reading. The naturalization does solve a genuine coordination problem (how to maintain property distributions) AND extracts asymmetrically (dispossesses while telling them it's natural). Both functions are real. The tension is not resolved in favor of one — it's structural. The false summit perspective (mountain/analytical) is diagnostic: if naturalization were truly a natural law, beneficiary presence would not matter. But beneficiary presence is documented (incumbent power enters the constraint as an agent, not a background condition). The engine's false summit detector identifies this as FSM candidate and reclassifies as tangled_rope, confirming the intentional reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalization_mechanism_contingency,
    'Is the naturalization of markets a necessary feature of market economies, or a contingent ideological choice by beneficiaries to maintain legitimacy?',
    'Historical analysis of pre-market societies with non-naturalized resource allocation (commons, feudal, gift economies) showing explicit social construction; analysis of contemporary heterodox economic systems showing naturalization is optional; study of periods when incumbent beneficiaries did NOT naturalize (e.g., mercantilist justifications based on explicit state goals rather than natural law)',
    'If necessary: the snare classification is overstated — naturalization is coordination cost, not pure extraction. If contingent: the snare classification is correct — naturalization is chosen by beneficiaries specifically to suppress alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(naturalization_mechanism_contingency, empirical, 'Whether naturalization is necessary or contingent to market legitimacy').

omega_variable(
    alternative_epistemic_viability,
    'Can heterodox economic frameworks produce equivalent or superior predictive and explanatory power compared to neoclassical naturalization?',
    'Comparative analysis of heterodox models on standard empirical benchmarks (business cycle prediction, inequality dynamics, financial stability); meta-analysis of heterodox vs orthodox publication outcomes on novel phenomena; real-time forecasting competitions',
    'If viable: the piton classification is correct — mainstream apparatus suppresses superior alternatives. If not viable: mainstream dominance reflects legitimate epistemic superiority, and the constraint is weaker coordination problem than snare/tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_epistemic_viability, empirical, 'Whether heterodox economics can match orthodox performance on empirical benchmarks').

omega_variable(
    reading_foreclosure_ambiguity,
    'Does the beneficiary_maintenance_reading strictly foreclose the spontaneous_order_reading, or do they coexist as different institutional framings?',
    'Logical analysis of core premises: does spontaneous_order reading require that no one actively naturalizes markets (i.e., that naturalization is accidental emergence)? Or can it accommodate intentional beneficiary narrative-shaping within markets that also have emergent properties? If latter, readings coexist; if former, beneficiary reading forecloses spontaneous order reading within the same institutional framework.',
    'If foreclose: the reading_relations entry is forecloses, not coexists_with. If coexist: both readings remain live and the constraint kernel is genuinely contested across different institutional positions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_ambiguity, conceptual, 'Whether beneficiary_maintenance reading logically forecloses spontaneous_order reading').

omega_variable(
    ideological_apparatus_capture_scope,
    'How much of mainstream economics teaching and publication is post-hoc defense of beneficiary interests versus genuine analytical advance neutral to incumbent advantage?',
    'Content analysis of economics textbooks and journal articles: what proportion assert beneficiary-favorable claims as ''natural'' vs ''policy choice''? Citation patterns: are heterodox alternatives cited proportionally to their use by other scholars, or systematically undercited? Biographical analysis: career trajectories of economists who adopt heterodox positions within mainstream institutions',
    'High capture (>70%): theater_ratio might be even higher than 0.78. Low capture (<30%): mainstream dominance is intellectual merit, not enforcement; constraint is weaker than snare/tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ideological_apparatus_capture_scope, empirical, 'What proportion of mainstream economics is post-hoc beneficiary defense versus analytical neutrality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beneficiary_maintenance_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_t0_postkeynes_emergence, beneficiary_maintenance_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(theater_t20_chicago_school_dominance, beneficiary_maintenance_reading, theater_ratio, 20, 0.72).
narrative_ontology:measurement(theater_t40_contemporary, beneficiary_maintenance_reading, theater_ratio, 40, 0.78).

% Extraction over time
narrative_ontology:measurement(extract_t0_early_formalization, beneficiary_maintenance_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(extract_t20_neoliberal_ascendancy, beneficiary_maintenance_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(extract_t40_contemporary, beneficiary_maintenance_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(extract_t45_slight_rebound_heterodox, beneficiary_maintenance_reading, base_extractiveness, 45, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beneficiary_maintenance_reading, identity_coordination).
narrative_ontology:affects_constraint(beneficiary_maintenance_reading, spontaneous_order_reading).
narrative_ontology:affects_constraint(beneficiary_maintenance_reading, engineered_infrastructure_reading).
narrative_ontology:affects_constraint(beneficiary_maintenance_reading, ideological_closure_accumulation).
narrative_ontology:affects_constraint(beneficiary_maintenance_reading, disciplinary_paradigm_capture).

% DUAL FORMULATION NOTE:
% The beneficiary_maintenance_reading is part of a constraint family decomposing the contested kernel market_as_natural_default into three structurally distinct claims. Each sibling has different ε, different beneficiary/victim structure, and different measurement properties. The beneficiary reading (ε=0.58) sits between the spontaneous order reading (low extraction, mountain-like) and engineered infrastructure reading (high extraction, snare-like). All three are linked bidirectionally in network.affects_constraints because a shift in any one reading alters the epistemic viability of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(beneficiary_maintenance_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
