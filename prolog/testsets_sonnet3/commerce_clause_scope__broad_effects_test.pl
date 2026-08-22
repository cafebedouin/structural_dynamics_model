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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Commerce Clause Scope — Substantial Effects / Aggregation Doctrine (Wickard-Raich Line)
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   This story authors ONE reading of the commerce clause kernel: the
 *   substantial-effects/aggregation reading (Wickard v. Filburn, 1942;
 *   Gonzales v. Raich, 2005) under which any economic activity that,
 *   aggregated across all similarly situated actors nationally, substantially
 *   affects interstate commerce falls within federal reach, and 'regulate'
 *   includes outright prohibition and comprehensive control. This reading is
 *   not the whole of 'the commerce clause' — the narrow originalist reading
 *   (trade crossing state lines, regulate-as-facilitate) and the intermediate
 *   channels/instrumentalities/limited-aggregation reading (Lopez, Morrison,
 *   requiring a jurisdictional element for non-economic conduct) are
 *   structurally distinct constraints with different ε, different
 *   beneficiary/victim sets, and different classifications, authored as
 *   separate stories and linked via network.affects_constraints. Under the
 *   broad-effects reading's own lights, the standing arrangement (the
 *   doctrine as it currently operates, post-Raich) is what ε describes here —
 *   not any narrower alternative this reading would reject.
 *
 * KEY AGENTS:
 *   - federal_regulatory_agencies: Primary beneficiary/agenda-setter (institutional/arbitrage) — expands jurisdiction via aggregation precedent
 *   - state_governments: Primary institutional victim (institutional/constrained) — loses police-power authority whenever economic characterization succeeds
 *   - individual_intrastate_actors: Primary individual victim (powerless/trapped) — cannot disaggregate self from the regulated class
 *   - supreme_court: Analytical observer and kernel-holder (institutional/analytical) — sets and periodically revisits the doctrinal test across all three readings
 *   - national_civil_rights_enforcement: Secondary beneficiary (organized/mobile) — uses broad power to reach private discrimination beyond state law's historical reach
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__broad_effects_test, 0.68).
domain_priors:suppression_score(commerce_clause_scope__broad_effects_test, 0.62).
domain_priors:theater_ratio(commerce_clause_scope__broad_effects_test, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, extractiveness, 0.68).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__broad_effects_test, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__broad_effects_test, "Commerce Clause Scope — Substantial Effects / Aggregation Doctrine (Wickard-Raich Line)").
narrative_ontology:topic_domain(commerce_clause_scope__broad_effects_test, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_scope__broad_effects_test).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__broad_effects_test, '02683ffc-8eb2-43b9-8837-5b06607b010d').
narrative_ontology:cs_kernel_codification('02683ffc-8eb2-43b9-8837-5b06607b010d', fixed_text).
narrative_ontology:cs_authority_grounding('02683ffc-8eb2-43b9-8837-5b06607b010d', lineage).
narrative_ontology:cs_interpretation_layer_present('02683ffc-8eb2-43b9-8837-5b06607b010d').
narrative_ontology:cs_reading_relation('02683ffc-8eb2-43b9-8837-5b06607b010d', commerce_clause_scope__narrow_originalist, forecloses).
narrative_ontology:cs_reading_relation('02683ffc-8eb2-43b9-8837-5b06607b010d', commerce_clause_scope__intermediate_channels, influences).
narrative_ontology:cs_axiom('02683ffc-8eb2-43b9-8837-5b06607b010d', foundational, aggregation_of_individually_trivial_conduct_is_regulable).
narrative_ontology:cs_axiom_status(aggregation_of_individually_trivial_conduct_is_regulable, holdable).
narrative_ontology:cs_axiom_grounding('02683ffc-8eb2-43b9-8837-5b06607b010d', aggregation_of_individually_trivial_conduct_is_regulable, conventional).
narrative_ontology:cs_axiom('02683ffc-8eb2-43b9-8837-5b06607b010d', foundational, regulate_includes_prohibition_and_comprehensive_control).
narrative_ontology:cs_axiom_status(regulate_includes_prohibition_and_comprehensive_control, holdable).
narrative_ontology:cs_axiom_grounding('02683ffc-8eb2-43b9-8837-5b06607b010d', regulate_includes_prohibition_and_comprehensive_control, conventional).
narrative_ontology:cs_reference_frame('02683ffc-8eb2-43b9-8837-5b06607b010d', new_deal_economic_emergency_framework).
narrative_ontology:cs_drift_state('02683ffc-8eb2-43b9-8837-5b06607b010d', post_raich_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('02683ffc-8eb2-43b9-8837-5b06607b010d', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__broad_effects_test, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, federal_regulatory_agencies).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, national_civil_rights_enforcement).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, national_interest_advocacy_groups).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, national_business_seeking_uniform_rules).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, state_governments).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, local_economic_experimentation).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, individual_intrastate_actors).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, federalism_structural_constraint).
narrative_ontology:constraint_vindicates(commerce_clause_scope__broad_effects_test, national_economic_uniformity_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_scope__broad_effects_test, aggregation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and enforce regulations reaching activity previously understood as beyond federal reach — wheat grown for home consumption, local drug cultivation, workplace conditions at single-site employers — by characterizing it as part of a national aggregate market. Each successful assertion of jurisdiction becomes precedent expanding the next one. The agency both writes the rules and litigates to defend the scope that lets it write more.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, federal_regulatory_agencies, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__broad_effects_test, federal_regulatory_agencies, agenda_setter).

% Legislates using the aggregation and substantial-effects doctrines as the operative test for what it may reach, rarely constrained by any judicially enforced outer limit. Holds the power to characterize almost any regulated conduct as economic activity with cumulative national impact, and to declare the findings that support that characterization.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, congress, agenda_setter,
    institutional, generational, arbitrage, national).

% Relies on the broad commerce power (as used in Heart of Atlanta Motel and similar cases) to reach discriminatory conduct by private actors that a narrower reading would leave to state law alone, where enforcement was historically absent or hostile. Gains a uniform national floor that does not depend on any single state's willingness to act.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, national_civil_rights_enforcement, beneficiary,
    organized, generational, mobile, national).

% Lobby for federal standards precisely because the broad-effects doctrine lets one national campaign displace fifty separate state fights. A single federal rule, once justified under aggregation, forecloses the need to win in unfavorable state legislatures.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, national_interest_advocacy_groups, beneficiary,
    organized, generational, mobile, national).

% Lose authority to set distinct local policy on activity that could plausibly be characterized as having a cumulative economic effect — which, under aggregation doctrine, is nearly any recurring activity performed by many people. Can litigate against particular applications but cannot exit the doctrine itself; each unsuccessful challenge (Wickard, Raich) further narrows what remains identifiably a state matter.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, state_governments, payer,
    institutional, generational, constrained, national).

% The practice of states serving as policy laboratories — trying divergent regulatory approaches that federal preemption under broad commerce power forecloses once Congress or an agency asserts uniform national jurisdiction. Not an actor itself, but a capacity that is foreclosed each time the doctrine is applied broadly.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, local_economic_experimentation, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(commerce_clause_scope__broad_effects_test, local_economic_experimentation).

% A farmer growing wheat for his own consumption, a patient cultivating medical marijuana under state law, a small business operating entirely within one state — each subject to federal regulation because their individual conduct, aggregated with everyone else engaged in similar conduct nationally, is deemed to substantially affect an interstate market. They have no exit from the aggregate; their own conduct alone would never trigger federal jurisdiction, but they cannot disaggregate themselves from the class the doctrine defines.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, individual_intrastate_actors, payer,
    powerless, biographical, trapped, local).

% Sets and periodically revisits the doctrinal test (Wickard, Lopez, Morrison, Raich, NFIB) — has both expanded the aggregation principle to its widest point (Wickard, Raich) and occasionally pulled back (Lopez, Morrison) when the regulated conduct is judged non-economic. Its own doctrine is the kernel under contest across all three sibling readings.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, supreme_court, observer,
    institutional, civilizational, analytical, national).

% Multi-state firms prefer one federal standard to fifty divergent state regimes and lobby for broad commerce power precisely to preempt state-level variation that raises compliance costs. Benefits from the same doctrine that traps smaller intrastate actors.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, national_business_seeking_uniform_rules, beneficiary,
    powerful, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_scope__broad_effects_test, federal_regulatory_agencies).
narrative_ontology:fixing_cost_class(commerce_clause_scope__broad_effects_test, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents a race-to-the-bottom / collective-action failure among states on matters where uniform national standards are genuinely more efficient than fifty divergent regimes — labor conditions, civil rights floors, environmental spillovers, and markets where a single state's non-participation would undermine the whole scheme (the original wheat-price stabilization rationale in Wickard).
% TRANSFER_FUNCTION: Moves regulatory authority and enforcement discretion from state governments and individual local actors to federal agencies and Congress; moves the practical capacity to set policy at variance with a national norm away from states and toward whichever national coalition can characterize its preferred rule as addressing a cumulative economic effect.
% ABSENT_VOICES: State legislatures acting in a genuinely deliberative, non-litigating capacity are effectively absent from the doctrinal conversation — the doctrine is made and remade in federal courts between the federal government and individual litigants (a farmer, a patient), not in a forum where states as such can renegotiate the boundary. Originalist and intermediate-channels adherents object that the doctrine has no limiting principle, but their objection is heard as a losing argument in the same forum that expanded the doctrine, not as a structural check.
% DISAPPEARANCE_RATIONALE: If the substantial-effects/aggregation reading of federal commerce power vanished overnight, wide swathes of existing federal regulation — much of the modern administrative state's environmental, labor, healthcare, and civil-rights enforcement — would lose their principal constitutional basis. States would immediately reassert authority over intrastate activity currently subject to federal rules; national interest groups would lose their preferred forum and be forced back into fifty separate state campaigns; federal agencies would need Congress to re-ground authority in enumerated powers or constitutional amendment.
% FOUNDING_PROBLEM: The New Deal-era problem of a national economic collapse that fragmented, state-by-state regulatory responses could not address — agricultural overproduction, collapsing wages, and interstate competition to weaken labor and safety standards that no single state could unilaterally fix without being undercut by its neighbors.
% FOUNDING_PROBLEM_CORROBORATION: Federal regulators and civil rights enforcement bodies attest the founding problem remains live in updated form (national markets, national civil rights floors, need for regulatory uniformity). Independent legal historians and originalist scholars — outside the beneficiary set — attest that the doctrine's aggregation logic has been extended far past the original economic-emergency rationale to reach conduct (Raich's home cultivation, Morrison's gender-motivated violence claim, initially) with no serious link to interstate market function, and that no judicially administrable limiting principle currently constrains it; Lopez and Morrison represent the Court's own acknowledgment, from outside any beneficiary's interest, that the test as applied lacked a stopping point.
narrative_ontology:disappearance_verdict(commerce_clause_scope__broad_effects_test, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__broad_effects_test, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__broad_effects_test, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_scope__broad_effects_test, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__broad_effects_test, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.68) is authored high because the doctrine, taken to its Raich-era limit, subjects virtually all recurring economic conduct — including conduct with no realistic connection to any actual interstate transaction — to federal jurisdiction on the strength of an aggregation argument that individual conduct cannot rebut. Suppression (0.62) reflects that the doctrine forecloses state-level alternatives once federal jurisdiction is asserted; there is no meaningful exit for a state or individual actor once conduct is characterized as economic and aggregable. Theater ratio is modest (0.28) — the doctrine performs genuine coordination work (national civil rights enforcement, agricultural stabilization, labor standards) alongside its extractive expansion, so it is not purely performative. Accessibility collapse (0.58) is moderate-high: once a court accepts the aggregation characterization, alternative readings become practically unavailable to the individual litigant, though the doctrine itself remains actively contested at the level of Congress and the Court (hence resistance at 0.55, not low) — Lopez and Morrison show the doctrine is not immune to pushback, just rarely successfully challenged by individual actors.
 *
 * PERSPECTIVAL GAP:
 *   From the federal regulatory agency's seat, the doctrine is coordination: a single national rule solving problems that fragmented state action cannot solve. From the state government's seat, the identical structure is extraction of authority: police powers that would otherwise remain with the state are absorbed whenever a federal actor can construct an aggregation argument, and the state has no comparable doctrine available to reclaim ground. From the individual intrastate actor's seat (a farmer, a patient), the doctrine is total: their own conduct is legally irrelevant except as a unit in someone else's aggregate.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal regulatory agencies and national interest groups are declared beneficiaries: the doctrine's expansion directly increases what they can accomplish through a single national forum. State governments, local experimentation capacity, and individual intrastate actors are declared victims: each successful application of the doctrine narrows what remains available to them, with no reciprocal expansion of their own authority. National civil rights enforcement is a beneficiary in this reading specifically because the broad power gave it reach into private discriminatory conduct that state law historically failed to address — this is the strongest normative case FOR the broad reading and is authored honestly as such, not minimized.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fragmented state responses to a genuinely national economic collapse in 1937) is contested as to whether it remains live. Federal agencies and rights-enforcement beneficiaries argue continuity of function under modern conditions (national markets, need for civil-rights floors). Independent legal historians and the Court's own retreat in Lopez/Morrison corroborate, from outside the beneficiary set, that the doctrine's application has drifted well past addressing genuine collective-action failures — Raich's application to home cultivation for personal use under a state medical exception shares little structurally with Wickard's wartime price-stabilization rationale. This is exactly the tangled_rope signature: a genuine coordination function (national economic and civil-rights uniformity) persists alongside asymmetric extraction (near-total displacement of state authority) that requires active judicial and agency enforcement to sustain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aggregation_limiting_principle,
    'Is there any judicially administrable limiting principle within the substantial-effects/aggregation doctrine, or does the doctrine as applied (post-Raich) reach any recurring human activity whatsoever given sufficiently creative aggregation?',
    'Track whether any future Supreme Court case identifies a category of intrastate conduct the aggregation principle cannot reach, distinct from the narrow non-economic-conduct carve-out in Lopez/Morrison, which itself remains contested as a real limit versus a rarely-invoked exception.',
    'If no limiting principle exists, this reading''s classification approaches snare (near-total extraction of state police power with only nominal remaining coordination function); if a real limiting principle is consistently applied, the tangled_rope classification with a genuine coordination core is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregation_limiting_principle, conceptual, 'Whether the broad-effects reading has any real outer boundary or is functionally unlimited.').

omega_variable(
    civil_rights_beneficiary_asymmetry,
    'Does the civil-rights enforcement benefit of the broad reading (reaching private discrimination federal law would otherwise not reach) justify the doctrine''s overall extraction from federalism, or is it a separable good achievable through a narrower doctrinal hook (e.g., the Fourteenth Amendment enforcement power) that does not require the unlimited aggregation logic?',
    'Comparative doctrinal analysis of whether Heart of Atlanta Motel-style outcomes are achievable under Section 5 enforcement power or a narrower commerce theory without extending to Wickard/Raich-level reach.',
    'If separable, the civil-rights beneficiary case does not require the full breadth of aggregation doctrine, weakening the coordination justification for the doctrine''s broader reach into unrelated economic regulation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civil_rights_beneficiary_asymmetry, conceptual, 'Whether the doctrine''s strongest beneficiary case requires its full extractive breadth.').

omega_variable(
    reading_selection_as_political_outcome,
    'Is the choice among the three kernel readings (broad_effects_test, intermediate_channels, narrow_originalist) itself substantially a function of which political coalition controls Court appointments at a given time, rather than a stable interpretive methodology?',
    'Track correlation between doctrinal shifts (1937 shift, 1995-2000 Lopez/Morrison retrenchment, 2005 Raich re-expansion) and composition of the Court''s appointing coalitions.',
    'If reading selection tracks political composition closely, the kernel''s ''contest'' is less a jurisprudential disagreement than a proxy fight for which the commerce clause text is largely a vehicle — this would apply symmetrically to all three sibling readings and does not privilege this reading''s account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_as_political_outcome, empirical, 'Whether reading selection tracks judicial political composition rather than stable doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__broad_effects_test, 1937, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_scope__broad_effects_test, theater_ratio, 1937, 0.1).
narrative_ontology:measurement(comm_tr_t1955, commerce_clause_scope__broad_effects_test, theater_ratio, 1955, 0.14).
narrative_ontology:measurement(comm_tr_t1970, commerce_clause_scope__broad_effects_test, theater_ratio, 1970, 0.18).
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_scope__broad_effects_test, theater_ratio, 1995, 0.22).
narrative_ontology:measurement(comm_tr_t2005, commerce_clause_scope__broad_effects_test, theater_ratio, 2005, 0.25).
narrative_ontology:measurement(comm_tr_t2024, commerce_clause_scope__broad_effects_test, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(comm_be_t1937, commerce_clause_scope__broad_effects_test, base_extractiveness, 1937, 0.35).
narrative_ontology:measurement(comm_be_t1955, commerce_clause_scope__broad_effects_test, base_extractiveness, 1955, 0.45).
narrative_ontology:measurement(comm_be_t1970, commerce_clause_scope__broad_effects_test, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(comm_be_t1995, commerce_clause_scope__broad_effects_test, base_extractiveness, 1995, 0.6).
narrative_ontology:measurement(comm_be_t2005, commerce_clause_scope__broad_effects_test, base_extractiveness, 2005, 0.66).
narrative_ontology:measurement(comm_be_t2024, commerce_clause_scope__broad_effects_test, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1937, commerce_clause_scope__broad_effects_test, suppression_requirement, 1937, 0.3).
narrative_ontology:measurement(comm_su_t1955, commerce_clause_scope__broad_effects_test, suppression_requirement, 1955, 0.4).
narrative_ontology:measurement(comm_su_t1970, commerce_clause_scope__broad_effects_test, suppression_requirement, 1970, 0.48).
narrative_ontology:measurement(comm_su_t1995, commerce_clause_scope__broad_effects_test, suppression_requirement, 1995, 0.52).
narrative_ontology:measurement(comm_su_t2005, commerce_clause_scope__broad_effects_test, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement(comm_su_t2024, commerce_clause_scope__broad_effects_test, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__broad_effects_test, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, commerce_clause_scope__intermediate_channels).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, commerce_clause_scope__narrow_originalist).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language label 'the commerce clause' into structurally distinct constraints, per the ε-invariance principle: narrow_originalist (minimal federal reach, near-mountain/rope character, low ε), intermediate_channels (bounded categories with a genuine limiting principle, moderate ε), and broad_effects_test (this story — aggregation without a stable limiting principle, high ε, tangled_rope). Each reading has its own beneficiary/victim structure and its own stable ε; they are linked here rather than merged because measuring 'the commerce clause' under different doctrinal tests yields different extraction values — exactly the signal that two constraints, not one, are present.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commerce_clause_scope__broad_effects_test, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
