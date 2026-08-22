% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__expansive_federal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__expansive_federal_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: commerce_clause_text__expansive_federal_reading
 *   human_readable: Expansive Federal Reading of the Commerce Clause (Aggregate Substantial Effects Doctrine)
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   This story instantiates the expansive federal reading of the Commerce
 *   Clause kernel: the doctrine, crystallized in Wickard v. Filburn (1942)
 *   and reaffirmed in Gonzales v. Raich (2005), that Congress may regulate
 *   purely intrastate, even non-commercial, activity when that activity
 *   aggregated with similar conduct nationwide has a substantial effect on
 *   interstate commerce. This is the reading that grounds the modern federal
 *   administrative and regulatory state's reach into agriculture, labor,
 *   environment, drugs, and (contested) civil rights and criminal law. It is
 *   ONE of three readings of the same constitutional text; the originalist
 *   narrow reading and the substantial-effects-limited reading are separate
 *   constraint stories with their own epsilon values, beneficiary/victim
 *   sets, and classification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__expansive_federal_reading, 0.58).
domain_priors:suppression_score(commerce_clause_text__expansive_federal_reading, 0.62).
domain_priors:theater_ratio(commerce_clause_text__expansive_federal_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__expansive_federal_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__expansive_federal_reading, "Expansive Federal Reading of the Commerce Clause (Aggregate Substantial Effects Doctrine)").
narrative_ontology:topic_domain(commerce_clause_text__expansive_federal_reading, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_text__expansive_federal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__expansive_federal_reading, '439b322d-d06d-43b2-a6c3-76362c765afd').
narrative_ontology:cs_kernel_codification('439b322d-d06d-43b2-a6c3-76362c765afd', fixed_text).
narrative_ontology:cs_authority_grounding('439b322d-d06d-43b2-a6c3-76362c765afd', lineage).
narrative_ontology:cs_interpretation_layer_present('439b322d-d06d-43b2-a6c3-76362c765afd').
narrative_ontology:cs_reading_relation('439b322d-d06d-43b2-a6c3-76362c765afd', commerce_clause_text__originalist_narrow_reading, forecloses).
narrative_ontology:cs_reading_relation('439b322d-d06d-43b2-a6c3-76362c765afd', commerce_clause_text__substantial_effects_limited_reading, influences).
narrative_ontology:cs_axiom('439b322d-d06d-43b2-a6c3-76362c765afd', foundational, aggregate_economic_effects_confer_federal_jurisdiction).
narrative_ontology:cs_axiom_status(aggregate_economic_effects_confer_federal_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('439b322d-d06d-43b2-a6c3-76362c765afd', aggregate_economic_effects_confer_federal_jurisdiction, instrumental).
narrative_ontology:cs_axiom('439b322d-d06d-43b2-a6c3-76362c765afd', secondary, economic_non_economic_distinction_is_not_a_hard_limit).
narrative_ontology:cs_axiom_status(economic_non_economic_distinction_is_not_a_hard_limit, holdable).
narrative_ontology:cs_axiom_grounding('439b322d-d06d-43b2-a6c3-76362c765afd', economic_non_economic_distinction_is_not_a_hard_limit, conventional).
narrative_ontology:cs_reference_frame('439b322d-d06d-43b2-a6c3-76362c765afd', new_deal_functionalist_settlement).
narrative_ontology:cs_drift_state('439b322d-d06d-43b2-a6c3-76362c765afd', post_lopez_morrison_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('439b322d-d06d-43b2-a6c3-76362c765afd', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__expansive_federal_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, federal_administrative_agencies).
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, national_policy_coherence_advocates).
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, national_scale_regulated_industries).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, state_legislatures).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, local_economic_arrangements).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, intrastate_small_producers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, national_scale_regulated_industries).
narrative_ontology:constraint_vindicates(commerce_clause_text__expansive_federal_reading, national_market_unity_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_text__expansive_federal_reading, aggregate_effects_rationality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and enforce regulations reaching activity that is nominally local (wages at a single farm, wheat grown for home consumption, gun possession near a school) by aggregating it into a national economic class. Their jurisdictional reach, budget, and institutional relevance expand with the breadth of the reading; they litigate to defend the aggregation logic whenever it is challenged.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, federal_administrative_agencies, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__expansive_federal_reading, federal_administrative_agencies, agenda_setter).

% Legal scholars, national labor and consumer organizations, and civil-rights coalitions who rely on the expansive reading to secure uniform federal floors (minimum wage, workplace safety, anti-discrimination) rather than a patchwork of fifty state regimes. They benefit from litigation outcomes but do not administer the doctrine themselves.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, national_policy_coherence_advocates, beneficiary,
    organized, generational, mobile, national).

% Large firms operating across state lines prefer one federal rulebook to fifty conflicting state rulebooks, even when that rulebook is more restrictive than some states would impose. They lobby to shape the federal standard rather than exit to a single state, since a single-state exit does not escape federal reach once the aggregation logic applies.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, national_scale_regulated_industries, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__expansive_federal_reading, national_scale_regulated_industries, payer).

% Pass regulatory or economic-policy statutes tailored to local conditions, only to have them preempted or their regulatory space narrowed once a federal court or agency characterizes the local activity as part of an interstate economic class. They cannot exit the arrangement; the Supremacy Clause plus the expansive reading forecloses a state-law alternative once federal aggregate-effects jurisdiction attaches.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, state_legislatures, payer,
    organized, biographical, trapped, regional).

% A farmer growing wheat for personal use, a home-based caregiver, a small local cooperative — activity with no direct interstate transaction — becomes federally regulable because their conduct, aggregated with millions of similar actors, is deemed to have a substantial effect on the national market. They have essentially no capacity to litigate the aggregation theory and no exit: the doctrine reaches them wherever they are.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, intrastate_small_producers, payer,
    powerless, biographical, trapped, local).

% Locally-calibrated economic practices, guild-like arrangements, and regionally distinct commercial customs that depend on variation from a national norm are foreclosed once federal aggregate-effects regulation displaces the space in which such variation could persist. Not an actor itself, but the diffuse local-variation capacity that the doctrine's breadth eliminates.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, local_economic_arrangements, payer,
    powerless, generational, trapped, local).
narrative_ontology:stakeholder_non_agent(commerce_clause_text__expansive_federal_reading, local_economic_arrangements).

% Argue the founding-era text limited 'commerce' to trade and navigation across borders, not all economic activity with attenuated aggregate effects. They file briefs and dissent from the bench but the expansive reading's doctrinal momentum since the New Deal era means their textual argument rarely controls outcomes in cases the aggregation framework reaches.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, originalist_state_sovereignty_advocates, excluded,
    organized, generational, constrained, national).

% Adjudicates the boundary of the aggregation theory case by case, occasionally pulling back (Lopez, Morrison) but never dismantling the core doctrine (Wickard, Raich remain controlling). Sits above the contest, deciding where the aggregate-effects line falls without being a party who gains or loses from the doctrine's reach.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, federal_judiciary, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_text__expansive_federal_reading, federal_administrative_agencies).
narrative_ontology:fixing_cost_class(commerce_clause_text__expansive_federal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single national regulatory floor and prevents a race-to-the-bottom or a balkanized patchwork of fifty state economic regimes for goods, labor, and capital that in fact move across state lines in an integrated national economy.
% TRANSFER_FUNCTION: Moves regulatory authority over economic activity — including activity with no direct interstate transaction — from state legislatures and localities to federal agencies and federal courts, on the theory that aggregated local conduct affects the national market as a whole.
% ABSENT_VOICES: Local communities whose economic arrangements depended on interstate variation are rarely named parties in commerce-clause litigation; the doctrinal contest is fought between the federal government and a single regulated party (a farmer, a gun owner, a domestic-violence victim seeking a federal remedy), not the diffuse local arrangements displaced by the outcome either way.
% DISAPPEARANCE_RATIONALE: If the expansive reading were overnight replaced by the originalist narrow reading, most federal economic, labor, environmental, and civil-rights legislation enacted since the New Deal would lose its constitutional predicate; states would immediately reassert authority over wages, agriculture, controlled substances, and much environmental and antidiscrimination law, and interstate firms would face fifty divergent regimes again.
% FOUNDING_PROBLEM: The Articles of Confederation era saw states erecting trade barriers against each other and undermining a coherent national market; the Commerce Clause was built to give the federal government power to prevent state-level economic warfare and enable a unified national economy.
% FOUNDING_PROBLEM_CORROBORATION: Federal agencies and national policy advocates attest the founding problem remains live — a national economy still requires uniform floors and coordinated regulation of capital and labor mobility. Originalist scholars and state legislatures, from outside the beneficiary set, attest the founding problem (interstate trade barriers between states) was substantially different in kind and scope from the current doctrine's reach into wholly intrastate, non-commercial conduct, and that the aggregation theory answers a problem the framers did not contemplate.
narrative_ontology:disappearance_verdict(commerce_clause_text__expansive_federal_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__expansive_federal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__expansive_federal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_text__expansive_federal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__expansive_federal_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__expansive_federal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_text__expansive_federal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_text__expansive_federal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.58) but not maximal: the doctrine funds real coordination value (national labor standards, environmental floors, a unified market) while also displacing state and local regulatory autonomy that has no direct remedy once the aggregation theory attaches. Suppression (0.62) reflects that the doctrine's persistence depends on continuing judicial and executive-branch enforcement against state and local counter-assertions of authority — the doctrine has been actively defended in litigation since 1937, not merely accepted by consensus. Theater ratio is modest but rising (0.10 to 0.28) as more of the doctrine's application in recent decades has drifted toward post-hoc rationalization of federal reach that was originally justified by concrete economic integration concerns.
 *
 * PERSPECTIVAL GAP:
 *   From the federal agency and national-advocate seats, this doctrine reads as coordination: a necessary tool against economic balkanization and a race to the regulatory bottom. From the state-legislature and intrastate-producer seats, the identical doctrinal structure reads as extraction of regulatory sovereignty enforced through an aggregation theory that can, in principle, reach almost any economic activity anywhere. The engine computing tangled_rope at the story level reflects that both a genuine coordination function (uniform national floors) and asymmetric extraction (trapped state/local losers with no exit) are simultaneously present and structurally necessary to the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal administrative agencies and national-policy-coherence advocates sit near the beneficiary end: the broader the aggregation theory, the more regulatory ground they command, without bearing the compliance costs directly. National-scale regulated industries occupy a mixed position — they benefit from single-standard uniformity but pay compliance costs, hence beneficiary+payer. State legislatures and intrastate small producers sit near the full-target end: they are trapped (no exit — the doctrine reaches wherever the aggregated economic class exists) and bear the loss of regulatory or operational autonomy directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification (rather than snare) is deliberately chosen because the coordination function — preventing destructive interstate trade wars and enabling uniform national regulation of an integrated economy — remains genuinely live, distinguishing this from a pure extraction mechanism dressed in coordination language. But the founding problem (interstate trade barriers between states, 1780s-style) is narrower than the doctrine's current reach (wholly intrastate non-commercial conduct), which is why founding_problem_status is authored as contested rather than live: the doctrine has not been repurposed from nothing, but it has expanded well past its founding predicate, which is exactly the divergence a tangled_rope classification is built to hold without collapsing into either 'purely good coordination' or 'purely captured extraction.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aggregation_theory_scope_indeterminacy,
    'Is there a principled stopping point to the ''aggregate substantial effects'' theory, or does its own logic permit reaching any economic activity whatsoever given a sufficiently broad aggregation class?',
    'Track whether the federal judiciary (post-Lopez, post-Morrison) has articulated a workable limiting principle that survives subsequent case law, versus whether every attempted limit (economic/non-economic distinction, jurisdictional-nexus requirement) has itself eroded under later aggregation-theory litigation.',
    'If no principled stopping point exists, the expansive reading is structurally unbounded and the extraction figure likely understates the doctrine''s true reach; if a durable limiting principle holds, the doctrine is closer to the substantial_effects_limited_reading sibling and this story''s high suppression score should be revisited.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregation_theory_scope_indeterminacy, conceptual, 'Whether the aggregate-effects doctrine has an internal limiting principle or is unbounded in scope.').

omega_variable(
    originalist_text_vs_functionalist_reading,
    'Does the constitutional text ''commerce among the several states'' admit a plain original meaning that forecloses the aggregation theory, or is the text genuinely underdetermined such that functionalist and originalist readings are equally textually available?',
    'Historical linguistic and legal analysis of founding-era usage of ''commerce'' (trade/navigation vs. broader economic intercourse) cross-checked against ratification-era debates and early Congressional practice (e.g., the 1790s Coasting Act, early tariff and navigation acts).',
    'If the original meaning was narrow and univocal, the expansive reading''s textual claim weakens considerably relative to the originalist_narrow_reading sibling, strengthening an omega-level case that this reading''s beneficiaries capture political rather than textual legitimacy; if founding-era usage was itself broad and included manufacturing and agriculture affecting interstate trade, the expansive reading''s textual claim is substantially stronger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(originalist_text_vs_functionalist_reading, empirical, 'Whether founding-era textual meaning of ''commerce'' constrains or permits the aggregate-effects doctrine.').

omega_variable(
    national_market_integration_empirical_predicate,
    'How economically integrated must the national market actually be, empirically, for the aggregation theory''s premise (that any given local activity meaningfully affects the national market in aggregate) to hold as a factual matter rather than a legal fiction?',
    'Empirical economic analysis of actual market integration and price transmission for the specific commodities/activities at issue in landmark cases (wheat markets in Wickard''s era vs. cannabis markets in Raich''s era) to assess whether the aggregation premise was factually well-grounded or a legal construct doing work the facts did not support.',
    'Where market integration is empirically weak, the aggregation theory functions more as a jurisdictional fiction serving federal beneficiaries than as a factually grounded coordination mechanism, pushing the classification toward snare; where integration is empirically strong, the tangled_rope coordination function is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(national_market_integration_empirical_predicate, empirical, 'Whether the factual premise of national market integration underlying the aggregation theory holds for the specific activities the doctrine reaches.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__expansive_federal_reading, 1937, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_text__expansive_federal_reading, theater_ratio, 1937, 0.1).
narrative_ontology:measurement_basis(comm_tr_t1937, observed).
narrative_ontology:measurement(comm_tr_t1955, commerce_clause_text__expansive_federal_reading, theater_ratio, 1955, 0.14).
narrative_ontology:measurement_basis(comm_tr_t1955, observed).
narrative_ontology:measurement(comm_tr_t1975, commerce_clause_text__expansive_federal_reading, theater_ratio, 1975, 0.18).
narrative_ontology:measurement_basis(comm_tr_t1975, observed).
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_text__expansive_federal_reading, theater_ratio, 1995, 0.22).
narrative_ontology:measurement_basis(comm_tr_t1995, observed).
narrative_ontology:measurement(comm_tr_t2010, commerce_clause_text__expansive_federal_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement_basis(comm_tr_t2010, observed).
narrative_ontology:measurement(comm_tr_t2024, commerce_clause_text__expansive_federal_reading, theater_ratio, 2024, 0.28).
narrative_ontology:measurement_basis(comm_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t1937, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1937, 0.35).
narrative_ontology:measurement_basis(comm_be_t1937, observed).
narrative_ontology:measurement(comm_be_t1955, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1955, 0.45).
narrative_ontology:measurement_basis(comm_be_t1955, observed).
narrative_ontology:measurement(comm_be_t1975, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1975, 0.52).
narrative_ontology:measurement_basis(comm_be_t1975, observed).
narrative_ontology:measurement(comm_be_t1995, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1995, 0.48).
narrative_ontology:measurement_basis(comm_be_t1995, observed).
narrative_ontology:measurement(comm_be_t2010, commerce_clause_text__expansive_federal_reading, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement_basis(comm_be_t2010, observed).
narrative_ontology:measurement(comm_be_t2024, commerce_clause_text__expansive_federal_reading, base_extractiveness, 2024, 0.58).
narrative_ontology:measurement_basis(comm_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1937, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1937, 0.4).
narrative_ontology:measurement_basis(comm_su_t1937, observed).
narrative_ontology:measurement(comm_su_t1955, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1955, 0.5).
narrative_ontology:measurement_basis(comm_su_t1955, observed).
narrative_ontology:measurement(comm_su_t1975, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1975, 0.58).
narrative_ontology:measurement_basis(comm_su_t1975, observed).
narrative_ontology:measurement(comm_su_t1995, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1995, 0.53).
narrative_ontology:measurement_basis(comm_su_t1995, observed).
narrative_ontology:measurement(comm_su_t2010, commerce_clause_text__expansive_federal_reading, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement_basis(comm_su_t2010, observed).
narrative_ontology:measurement(comm_su_t2024, commerce_clause_text__expansive_federal_reading, suppression_requirement, 2024, 0.62).
narrative_ontology:measurement_basis(comm_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, commerce_clause_text__originalist_narrow_reading).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, commerce_clause_text__substantial_effects_limited_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language 'Commerce Clause' concept per the ε-invariance principle. Each sibling reading of the commerce_clause_text kernel has its own ε, beneficiary/victim structure, and classification: expansive_federal_reading (this story, tangled_rope, ε=0.58) authorizes federal reach into all activity with substantial aggregate effects; originalist_narrow_reading confines federal power to cross-border trade and instrumentalities; substantial_effects_limited_reading requires jurisdictional nexus and non-pretextual economic character. The three are not the same constraint measured three ways — they are structurally distinct claims with different victim sets, linked here via affects_constraints rather than merged into one averaged epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
