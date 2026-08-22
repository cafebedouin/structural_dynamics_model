% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__broad_effects_test
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__broad_effects_test, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: commerce_clause_scope__broad_effects_test
 *   human_readable: Commerce Clause — Substantial Effects / Aggregation Doctrine (Broad Reading)
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   This story instantiates the broad-effects/aggregation reading of the
 *   Commerce Clause kernel — the doctrinal position that federal commerce
 *   power reaches any economic activity that, in the aggregate with similarly
 *   situated conduct nationwide, substantially affects interstate commerce,
 *   and that 'regulate' encompasses prohibition and comprehensive federal
 *   control of the regulated field. This is the reading associated with
 *   Wickard v. Filburn and its progeny, and it is one of three structurally
 *   distinct readings of the same constitutional text; the narrow originalist
 *   reading (trade across state lines, 'regulate' as facilitation) and the
 *   intermediate channels/instrumentalities reading (three-category test with
 *   a jurisdictional-element limiting principle) are separate constraints
 *   with their own ε values, not variations to be averaged into this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__broad_effects_test, 0.71).
domain_priors:suppression_score(commerce_clause_scope__broad_effects_test, 0.62).
domain_priors:theater_ratio(commerce_clause_scope__broad_effects_test, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, extractiveness, 0.71).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__broad_effects_test, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__broad_effects_test, "Commerce Clause — Substantial Effects / Aggregation Doctrine (Broad Reading)").
narrative_ontology:topic_domain(commerce_clause_scope__broad_effects_test, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_scope__broad_effects_test).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__broad_effects_test, 'ec82d54c-ed60-407f-92eb-abb1b9983118').
narrative_ontology:cs_kernel_codification('ec82d54c-ed60-407f-92eb-abb1b9983118', fixed_text).
narrative_ontology:cs_authority_grounding('ec82d54c-ed60-407f-92eb-abb1b9983118', lineage).
narrative_ontology:cs_interpretation_layer_present('ec82d54c-ed60-407f-92eb-abb1b9983118').
narrative_ontology:cs_reading_relation('ec82d54c-ed60-407f-92eb-abb1b9983118', commerce_clause_scope__narrow_originalist, forecloses).
narrative_ontology:cs_reading_relation('ec82d54c-ed60-407f-92eb-abb1b9983118', commerce_clause_scope__intermediate_channels, influences).
narrative_ontology:cs_axiom('ec82d54c-ed60-407f-92eb-abb1b9983118', foundational, aggregation_of_economic_activity_is_commerce).
narrative_ontology:cs_axiom_status(aggregation_of_economic_activity_is_commerce, holdable).
narrative_ontology:cs_axiom_grounding('ec82d54c-ed60-407f-92eb-abb1b9983118', aggregation_of_economic_activity_is_commerce, conventional).
narrative_ontology:cs_axiom('ec82d54c-ed60-407f-92eb-abb1b9983118', foundational, regulate_includes_prohibition_and_comprehensive_control).
narrative_ontology:cs_axiom_status(regulate_includes_prohibition_and_comprehensive_control, holdable).
narrative_ontology:cs_axiom_grounding('ec82d54c-ed60-407f-92eb-abb1b9983118', regulate_includes_prohibition_and_comprehensive_control, conventional).
narrative_ontology:cs_reference_frame('ec82d54c-ed60-407f-92eb-abb1b9983118', post_new_deal_aggregation_doctrine).
narrative_ontology:cs_drift_state('ec82d54c-ed60-407f-92eb-abb1b9983118', post_lopez_morrison_retrenchment, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('ec82d54c-ed60-407f-92eb-abb1b9983118', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__broad_effects_test, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, federal_regulatory_agencies).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, national_interest_coalitions).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, civil_rights_enforcement_bodies).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, national_uniformity_seeking_industries).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, state_governments).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, local_economic_experimentation).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, wholly_intrastate_small_producers).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, state_police_power_domains).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and enforce rules reaching activity that is, on its face, purely intrastate — wheat grown for home consumption, local labor arrangements, local drug possession — by characterizing it as part of an aggregate national market. The broader the reading of 'substantially affects,' the more regulatory jurisdiction these agencies hold; they have no structural incentive to narrow the doctrine.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, federal_regulatory_agencies, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__broad_effects_test, federal_regulatory_agencies, agenda_setter).

% Advocacy groups seeking uniform national policy (environmental, labor, consumer protection) use the broad effects test to bypass the need to win fifty separate state legislative fights. A single federal rule, justified by aggregate economic effect, achieves in one stroke what decades of state-by-state advocacy could not guarantee.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, national_interest_coalitions, beneficiary,
    organized, generational, arbitrage, national).

% Rely on the Commerce Clause's broad reach (as in public accommodations enforcement) to reach discriminatory conduct that resists purely state-level remedy, particularly where state governments are themselves the source of the discriminatory regime. For this seat, the doctrine's breadth is what makes national civil rights enforcement possible at all.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, civil_rights_enforcement_bodies, beneficiary,
    institutional, generational, arbitrage, national).

% Lose the practical ability to set distinct local economic policy once an activity is characterized as having aggregate interstate effect, because virtually any economic activity can be so characterized. They retain nominal police powers but cannot rely on them once a federal statute claims the same field; litigation to preserve local control is expensive and rarely successful against the aggregation logic.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, state_governments, payer,
    institutional, generational, constrained, national).

% The practice of states trying different regulatory approaches to the same economic problem and comparing outcomes — the 'laboratories of democracy' function — is foreclosed wherever federal law occupies the field under the broad effects test, because a single national answer preempts the comparison.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, local_economic_experimentation, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_non_agent(commerce_clause_scope__broad_effects_test, local_economic_experimentation).

% A farmer growing wheat for home consumption, a local homegrown-cannabis user, a small intrastate manufacturer — each engages in activity with no direct interstate transaction, yet is reached by federal regulation because their conduct, aggregated with everyone similarly situated, is deemed to affect the national market. They have no meaningful legal or practical exit: compliance or federal enforcement are the only options.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, wholly_intrastate_small_producers, payer,
    powerless, biographical, trapped, local).

% The traditional constitutional domain reserved to states — health, safety, morals, general welfare regulation — is not itself an actor but is the doctrinal territory that shrinks every time the aggregation principle recharacterizes local conduct as national economic activity.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, state_police_power_domains, payer,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(commerce_clause_scope__broad_effects_test, state_police_power_domains).

% Adjudicate the boundary of the doctrine case by case, articulating (and periodically narrowing or widening) the test for what counts as substantial effect. Their own precedent is the mechanism by which the reading either hardens or erodes.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, constitutional_courts, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_scope__broad_effects_test, diffuse).
narrative_ontology:fixing_cost_class(commerce_clause_scope__broad_effects_test, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables uniform national economic and regulatory policy — a single labor standard, a single environmental rule, a single civil-rights floor — instead of fifty potentially conflicting state regimes, solving genuine collective-action problems in markets that are in fact interconnected (agricultural pricing, pollution, wage competition races-to-the-bottom).
% TRANSFER_FUNCTION: Moves regulatory authority and the practical capacity to set locally-tailored rules from state governments and local economic actors to federal agencies and national interest coalitions, justified by aggregate economic characterization rather than direct interstate transaction.
% ABSENT_VOICES: Wholly local producers and consumers whose transactions never cross a state line are rarely represented in the national political process that enacts the federal statute reaching them; state legislatures that would have experimented with alternative approaches are foreclosed before they can generate the comparative evidence that might have informed national policy.
% DISAPPEARANCE_RATIONALE: If the broad effects/aggregation test were replaced overnight by a narrower reading, a large share of existing federal regulatory schemes (labor standards, environmental rules, portions of civil rights and criminal law reaching local conduct) would lose their constitutional predicate; states would reassert independent regulatory authority over intrastate economic activity, and national interest groups would lose their primary lever for uniform policy without fifty separate legislative campaigns.
% FOUNDING_PROBLEM: The New Deal-era problem: a national economic collapse that state-by-state regulation could not address, combined with a national market so integrated that no single state's regulation of 'local' production or labor could be insulated from its effects on interstate commerce as a whole.
% FOUNDING_PROBLEM_CORROBORATION: Federal agencies and national interest coalitions attest the founding problem remains live — modern economic integration is, if anything, deeper than in 1937. Federalism scholars, several sitting appellate judges in dissent, and state attorneys general outside the beneficiary set attest that the doctrine has drifted from remedying genuine interstate spillovers toward a general federal police power exercised through economic characterization, corroborated by academic tracking of the expanding subject-matter reach of Commerce Clause legislation since Wickard.
narrative_ontology:disappearance_verdict(commerce_clause_scope__broad_effects_test, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__broad_effects_test, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__broad_effects_test, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_scope__broad_effects_test, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__broad_effects_test, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__broad_effects_test_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_scope__broad_effects_test, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_scope__broad_effects_test_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71) because, under this reading's own operation, the aggregation principle has no natural stopping point short of the entire economy — Wickard's own facts (wheat never sold, never crossing a state line) establish that virtually any recurring economic conduct can be aggregated into a national-effects claim. Suppression is authored substantial (0.62) but below extractiveness because enforcement operates primarily through ordinary federal statutory machinery rather than extraordinary coercive apparatus — the suppression is doctrinal foreclosure of state alternatives, not physical coercion. Theater ratio is modest (0.28): the doctrine performs a genuine coordination function (national labor and environmental standards are not merely performative) even as its extractive reach exceeds what coordination alone requires.
 *
 * PERSPECTIVAL GAP:
 *   From the federal agency and national-coalition seats, the doctrine is functioning exactly as intended — solving genuinely national problems that state fragmentation could not. From the state government and local producer seats, the same structure operates as an open-ended doctrinal solvent that dissolves the reserved domain the Tenth Amendment was meant to protect, with no textual limiting principle internal to this reading's own test. The engine is expected to compute these as different seat-level classifications from the same structural facts; the divergence is not an error in the story.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal regulatory agencies and national interest coalitions sit at the beneficiary end: they gain jurisdiction and policy uniformity without needing to win the argument state by state. Civil rights enforcement bodies are a genuine, distinct beneficiary — for this seat the doctrine's breadth is not incidental extraction but the very mechanism that makes federal remedies for state-level discrimination possible, which is why this reading is authored as tangled_rope rather than pure snare. State governments, local economic experimentation, and wholly intrastate small producers sit at the target end: their exit options are trapped or constrained because the doctrine forecloses the state-level and purely-local alternatives they would otherwise rely on. State police power itself (a non-agent doctrinal domain) is listed as a payer to register that the shrinking of reserved state authority is a structural cost independent of any single state actor's experience.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is deliberately chosen over snare because the coordination function is real and independently valuable (interstate labor competition races-to-the-bottom, cross-border pollution, and national civil rights enforcement against state-level discrimination are genuine collective-action problems this reading solves) and not merely a cover story. Classifying it as pure snare would erase the civil-rights and market-integration coordination function that gives the doctrine its historical legitimacy; classifying it as pure rope would erase the asymmetric cost imposed on state governments and local producers who never consented to and cannot exit the aggregation logic. The tangled_rope frame holds both facts without collapsing either.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection,
    'Is the broad-effects/aggregation reading the constitutionally correct reading of ''Commerce ... among the several States,'' or is it a reading that itself needs a limiting principle the narrow and intermediate readings supply?',
    'This is not resolvable by further doctrinal elaboration within this reading alone — it is a live inter-reading dispute tracked across three separate constraint stories (broad_effects_test, intermediate_channels, narrow_originalist) linked via network.affects_constraints. Resolution, if any, occurs at the level of which reading a controlling judicial majority adopts, which is itself contingent and historically has shifted (Lochner era -> New Deal -> Lopez/Morrison partial retrenchment).',
    'If the intermediate or narrow reading displaces this one as controlling doctrine, the beneficiary/victim structure authored here reverses substantially: federal agencies lose reach, state governments and local producers regain autonomy, and extractiveness for this reading''s own referent (the standing broad-effects arrangement) would need to be re-measured at a lower level or the story retired as historically superseded rather than currently operative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Which of the three sibling readings of the Commerce Clause kernel is or should be controlling doctrine.').

omega_variable(
    aggregation_limiting_principle_existence,
    'Does the broad-effects/aggregation test, taken on its own terms, contain any internal limiting principle, or does its stopping point depend entirely on judicial willingness to draw a line the text and doctrine do not themselves supply?',
    'Examine whether post-Wickard cases (Lopez, Morrison, NFIB v. Sebelius) that appeared to draw limits did so by reasoning internal to the aggregation test itself or by importing considerations (non-economic nature of conduct, absence of jurisdictional element) that actually belong to the intermediate reading''s framework.',
    'If the apparent limits in Lopez/Morrison are best read as quiet borrowing from the intermediate reading, then broad_effects_test as authored here has no genuine internal limiting principle, which would support the high extractiveness and accessibility_collapse scores; if they are internal, this reading is less totalizing than authored and ε should be revised downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregation_limiting_principle_existence, empirical, 'Whether the broad reading''s occasional judicial limits are internally generated or borrowed from a sibling reading.').

omega_variable(
    civil_rights_beneficiary_weighting,
    'How much of this reading''s aggregate legitimacy rests on its use to reach state-sponsored discrimination that no narrower reading could remedy, versus its use for ordinary federal economic regulation with no civil-rights dimension?',
    'Comparative accounting of federal statutes and enforcement actions relying on the broad-effects predicate: proportion serving anti-discrimination purposes versus general economic regulation, and whether civil-rights-specific applications could survive under the intermediate reading instead (e.g. via a distinct constitutional hook such as Section 5 enforcement power).',
    'If most civil-rights applications of Commerce power could equally be sustained under a narrower doctrine or a different constitutional provision, the beneficiary weight currently given to civil_rights_enforcement_bodies in this story overstates how much the broad reading specifically, as opposed to federal power generally, is doing that work.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civil_rights_beneficiary_weighting, empirical, 'Whether civil rights enforcement genuinely depends on this specific reading''s breadth or could be sustained under narrower doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__broad_effects_test, 1937, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_scope__broad_effects_test, theater_ratio, 1937, 0.1).
narrative_ontology:measurement(comm_tr_t1954, commerce_clause_scope__broad_effects_test, theater_ratio, 1954, 0.14).
narrative_ontology:measurement(comm_tr_t1970, commerce_clause_scope__broad_effects_test, theater_ratio, 1970, 0.18).
narrative_ontology:measurement(comm_tr_t1990, commerce_clause_scope__broad_effects_test, theater_ratio, 1990, 0.22).
narrative_ontology:measurement(comm_tr_t2005, commerce_clause_scope__broad_effects_test, theater_ratio, 2005, 0.25).
narrative_ontology:measurement(comm_tr_t2024, commerce_clause_scope__broad_effects_test, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(comm_be_t1937, commerce_clause_scope__broad_effects_test, base_extractiveness, 1937, 0.42).
narrative_ontology:measurement(comm_be_t1954, commerce_clause_scope__broad_effects_test, base_extractiveness, 1954, 0.5).
narrative_ontology:measurement(comm_be_t1970, commerce_clause_scope__broad_effects_test, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(comm_be_t1990, commerce_clause_scope__broad_effects_test, base_extractiveness, 1990, 0.66).
narrative_ontology:measurement(comm_be_t2005, commerce_clause_scope__broad_effects_test, base_extractiveness, 2005, 0.69).
narrative_ontology:measurement(comm_be_t2024, commerce_clause_scope__broad_effects_test, base_extractiveness, 2024, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1937, commerce_clause_scope__broad_effects_test, suppression_requirement, 1937, 0.35).
narrative_ontology:measurement(comm_su_t1954, commerce_clause_scope__broad_effects_test, suppression_requirement, 1954, 0.44).
narrative_ontology:measurement(comm_su_t1970, commerce_clause_scope__broad_effects_test, suppression_requirement, 1970, 0.52).
narrative_ontology:measurement(comm_su_t1990, commerce_clause_scope__broad_effects_test, suppression_requirement, 1990, 0.57).
narrative_ontology:measurement(comm_su_t2005, commerce_clause_scope__broad_effects_test, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement(comm_su_t2024, commerce_clause_scope__broad_effects_test, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__broad_effects_test, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, commerce_clause_scope__intermediate_channels).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, commerce_clause_scope__narrow_originalist).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the commerce_clause_scope kernel. broad_effects_test (this file) authors the aggregation-doctrine reading with an expansive victim set and high federal extractiveness from state sovereignty. intermediate_channels authors the three-category test with jurisdictional-element limiting principles, producing a narrower victim set and lower extractiveness. narrow_originalist authors the trade-facilitation reading, under which federal commerce power is closer to a genuine rope with minimal victim set. Each story carries its own ε, beneficiaries, and victims; none averages over the others. The three are mutually exclusive as controlling doctrine at any given moment but coexist as live positions argued by different judicial and academic factions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
