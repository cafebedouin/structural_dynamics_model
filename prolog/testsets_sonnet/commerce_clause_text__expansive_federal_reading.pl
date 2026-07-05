% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__expansive_federal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Expansive Federal Reading of the Commerce Clause (Aggregate Substantial-Effects Doctrine)
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   This story instantiates the expansive-federal reading of the Commerce
 *   Clause kernel: interstate commerce power reaches any economic activity
 *   whose aggregate national effects are substantial, even where no
 *   individual instance of the activity crosses a state line (the Wickard v.
 *   Filburn logic, extended through the New Deal settlement and
 *   Lopez/Morrison/Raich line of cases). This reading is NOT the originalist
 *   narrow reading (trade crossing borders and instrumentalities of movement)
 *   and NOT the substantial-effects-limited reading (which requires
 *   jurisdictional nexus elements and screens out non-economic or pretextual
 *   regulation) — those are separate constraints with their own ε and their
 *   own stakeholder structures, linked here only by network reference. This
 *   reading alone is authored: federal agencies as primary regulator, state
 *   autonomy and local economic variation as the structural cost.
 *
 * KEY AGENTS:
 *   - federal_administrative_agencies: Primary agenda-setter (institutional/arbitrage) — administers and expands the doctrine it benefits from
 *   - national_policy_coherence_advocates: Beneficiary (organized/mobile) — favors uniform national standards over state patchwork
 *   - national_market_regulated_industries: Beneficiary/payer (powerful/mobile) — trades compliance cost for regulatory uniformity
 *   - state_legislatures: Payer (institutional/constrained) — loses independent policy authority once federal occupation is sustained
 *   - intrastate_small_producers: Payer (powerless/trapped) — reached via aggregation with no individual-conduct defense
 *   - federal_judiciary: Analytical observer (institutional/analytical) — calibrates doctrinal reach case by case
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
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__expansive_federal_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__expansive_federal_reading, "Expansive Federal Reading of the Commerce Clause (Aggregate Substantial-Effects Doctrine)").
narrative_ontology:topic_domain(commerce_clause_text__expansive_federal_reading, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_text__expansive_federal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__expansive_federal_reading, '4faaf013-3a2d-4513-86da-2891c6afc116').
narrative_ontology:cs_kernel_codification('4faaf013-3a2d-4513-86da-2891c6afc116', fixed_text).
narrative_ontology:cs_authority_grounding('4faaf013-3a2d-4513-86da-2891c6afc116', lineage).
narrative_ontology:cs_interpretation_layer_present('4faaf013-3a2d-4513-86da-2891c6afc116').
narrative_ontology:cs_reading_relation('4faaf013-3a2d-4513-86da-2891c6afc116', commerce_clause_text__originalist_narrow_reading, forecloses).
narrative_ontology:cs_reading_relation('4faaf013-3a2d-4513-86da-2891c6afc116', commerce_clause_text__substantial_effects_limited_reading, influences).
narrative_ontology:cs_axiom('4faaf013-3a2d-4513-86da-2891c6afc116', foundational, aggregate_economic_effects_are_the_relevant_unit_of_analysis).
narrative_ontology:cs_axiom_status(aggregate_economic_effects_are_the_relevant_unit_of_analysis, holdable).
narrative_ontology:cs_axiom_grounding('4faaf013-3a2d-4513-86da-2891c6afc116', aggregate_economic_effects_are_the_relevant_unit_of_analysis, instrumental).
narrative_ontology:cs_axiom('4faaf013-3a2d-4513-86da-2891c6afc116', foundational, national_economic_integration_supersedes_textual_border_crossing_requirement).
narrative_ontology:cs_axiom_status(national_economic_integration_supersedes_textual_border_crossing_requirement, holdable).
narrative_ontology:cs_axiom_grounding('4faaf013-3a2d-4513-86da-2891c6afc116', national_economic_integration_supersedes_textual_border_crossing_requirement, empirically_contingent).
narrative_ontology:cs_reference_frame('4faaf013-3a2d-4513-86da-2891c6afc116', new_deal_settlement_framework).
narrative_ontology:cs_drift_state('4faaf013-3a2d-4513-86da-2891c6afc116', contemporary_administrative_state_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4faaf013-3a2d-4513-86da-2891c6afc116', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__expansive_federal_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, federal_administrative_agencies).
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, national_policy_coherence_advocates).
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, national_market_regulated_industries).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, state_legislatures).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, local_regulatory_autonomy).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, intrastate_small_producers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, national_market_regulated_industries).
narrative_ontology:constraint_vindicates(commerce_clause_text__expansive_federal_reading, national_economic_unity_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_text__expansive_federal_reading, aggregation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and enforce regulations reaching activity with only aggregate, indirect connection to interstate trade (wage floors, crop quotas, workplace conditions, environmental controls) by invoking the substantial-effects-in-aggregate doctrine. Their jurisdictional reach and budget grow with each expansion of what counts as 'affecting' interstate commerce; they administer the doctrine they also benefit from.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, federal_administrative_agencies, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__expansive_federal_reading, federal_administrative_agencies, beneficiary).

% Labor unions, national trade associations, and public-interest coalitions favor uniform national standards over a patchwork of state rules. They lobby for and litigate to preserve the expansive reading because it lets them win once nationally rather than fifty times locally; they can exit into other advocacy venues if the doctrine narrows.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, national_policy_coherence_advocates, beneficiary,
    organized, generational, mobile, national).

% Large interstate firms benefit from a single federal regulatory regime that preempts a patchwork of divergent state rules, lowering compliance costs even as it subjects them to federal jurisdiction; they can lobby federal agencies directly and have the scale to absorb compliance costs that smaller intrastate competitors cannot.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, national_market_regulated_industries, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__expansive_federal_reading, national_market_regulated_industries, payer).

% Lose the ability to set independent economic policy in domains once considered purely intrastate — wages, agriculture, local labor markets — because federal regulation, once triggered by the aggregate-effects doctrine, preempts conflicting state law. They can litigate or lobby Congress but cannot unilaterally reclaim the regulatory space once federal occupation is judicially sustained.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, state_legislatures, payer,
    institutional, generational, constrained, regional).

% The capacity of local communities to calibrate economic rules to local conditions (cost of living, local industry structure, local political consensus) is displaced wherever the aggregate-effects doctrine finds a national interest. This is a structural condition, not an actor, but its erosion is the direct cost the doctrine imposes.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, local_regulatory_autonomy, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_non_agent(commerce_clause_text__expansive_federal_reading, local_regulatory_autonomy).

% A farmer growing wheat for home consumption, a local artisan, or a small intrastate business can be reached by federal regulation because their individual conduct, aggregated with millions of similarly situated actors, is deemed to substantially affect the national market — even though no single instance of their conduct crosses a state line or measurably moves national prices. They have no realistic exit: compliance is mandatory, litigation is expensive, and the aggregation logic forecloses the 'my conduct alone doesn't matter' defense.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, intrastate_small_producers, payer,
    powerless, biographical, trapped, local).

% Adjudicates the boundary of the aggregate-effects doctrine case by case, deciding how far the 'substantial effects' logic extends before it swallows the enumerated-powers structure entirely. Its rulings do not remove the doctrine but calibrate its reach.
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
% COORDINATION_FUNCTION: Provides a single national regulatory floor for economic activity that is genuinely interconnected across state lines — labor markets, agricultural commodity pricing, and industrial production do aggregate into national effects that no single state can internalize or manage alone.
% TRANSFER_FUNCTION: Moves regulatory authority and enforcement discretion from state legislatures and localities to federal agencies, and moves compliance-cost advantage toward large multi-state firms (who can absorb uniform federal rules) and away from small intrastate producers (who lose the local-calibration defense and face the same compliance burden without the aggregation benefit).
% ABSENT_VOICES: Small intrastate producers and local communities whose economic arrangements are swept in by aggregation rarely have standing or resources to litigate the doctrine's boundary; their objection — that their individual conduct does not in fact affect interstate commerce — is foreclosed by the aggregation principle itself before it can be heard on its own terms.
% DISAPPEARANCE_RATIONALE: If the expansive reading were abandoned overnight in favor of a narrow originalist reading, most federal labor law, agricultural regulation, environmental law, and large portions of federal criminal law reaching economic conduct would lose their constitutional basis; federal agencies would lose jurisdiction over vast regulatory domains and states would reclaim primary regulatory authority over activity currently treated as national.
% FOUNDING_PROBLEM: The New Deal-era problem of a fragmented national economy in which state-by-state regulation could not address economic collapse, agricultural overproduction, and a race-to-the-bottom in labor standards driven by interstate competition for capital.
% FOUNDING_PROBLEM_CORROBORATION: Federal agencies and national labor/trade coalitions attest the founding problem remains live — that modern supply chains and labor markets are even more nationally integrated than in 1937. Federalism scholars, several state attorneys general, and originalist jurists (outside the coalition that benefits from federal jurisdiction) attest that the doctrine has drifted far past its founding justification, reaching conduct (e.g., purely local, non-commercial activity aggregated only by hypothesis) that the New Deal crisis never contemplated.
narrative_ontology:disappearance_verdict(commerce_clause_text__expansive_federal_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__expansive_federal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__expansive_federal_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.58) reflects substantial but not total transfer: the doctrine genuinely solves a real coordination problem (interconnected national labor and commodity markets) while also transferring regulatory authority away from states and imposing uniform compliance costs on actors whose individual conduct has no measurable interstate effect. Suppression (0.62) is higher than extraction because the aggregation principle structurally forecloses the 'my conduct alone doesn't affect commerce' defense — this is a doctrinal foreclosure mechanism, not merely a cost transfer. Theater ratio is moderate-low and rising (0.10 to 0.28) reflecting increasing use of jurisdictional-hook boilerplate (findings clauses, jurisdictional elements added to statutes) that formally invokes commerce power without demonstrating genuine substantial effects in every application. Accessibility collapse (0.60) is substantial but not mountain-grade: states retain some residual police-power domains and the doctrine has been narrowed at the margins (Lopez, Morrison) — genuine alternative framings persist in live litigation, which is why this is tangled_rope rather than snare.
 *
 * PERSPECTIVAL GAP:
 *   From the federal agency seat, the doctrine is functioning coordination solving a real national-market problem. From the intrastate small producer seat, the same doctrine is an inescapable extraction mechanism that forecloses their strongest defense (individual immateriality) via aggregation logic before it is even raised. The engine should compute these as structurally different experiences of the identical constraint, driven by the power/exit_options divergence, not by different facts about the doctrine.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal agencies sit at the beneficiary/agenda-setter pole: they administer and their jurisdiction grows with the doctrine's reach — d near the beneficiary end, with institutional exit options amounting to arbitrage across regulatory domains. State legislatures and intrastate small producers sit near the target pole: constrained-to-trapped exit, generational-to-biographical cost horizon, no individual escape from aggregation once triggered. National market regulated industries are genuinely dual-positioned (secondary_role: payer) — they pay compliance costs but benefit from preemption of a fifty-state patchwork, which is why their d sits closer to symmetric than the small-producer seat despite nominally sharing 'regulated party' status.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (New Deal economic fragmentation, race-to-the-bottom competition among states) is genuinely contested as live vs. dead: national supply chains argue for continued relevance, but the doctrine's current reach (e.g., non-economic activity aggregated only by hypothetical connection to a regulatory scheme) extends well past what a New-Deal-crisis justification would require. This is precisely the tangled_rope diagnosis: coordination function is real (national economic integration exists and needs some governance), but the extraction (state autonomy loss, small-producer capture with no individual defense) rides on the same doctrinal structure and cannot be separated from it without dismantling the coordination function too — hence 'requires_active_enforcement: true' and both beneficiary and victim declarations are structurally required, not incidental.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aggregation_principle_naturalness,
    'Is the aggregation principle (that individually trivial conduct becomes federally regulable when aggregated across millions of similarly situated actors) a necessary implication of a genuinely national economy, or a constructed doctrinal expansion that primarily benefits federal administrative capacity and national interest-group coalitions?',
    'Compare economic integration metrics (interstate trade dependency, supply chain concentration) across the doctrine''s expansion period against the marginal doctrinal reach claimed in each expansion case; if reach consistently outpaces measurable integration, the constructed reading gains support.',
    'If the aggregation principle is a natural implication of economic integration, the tangled_rope classification undercounts genuine coordination; if it is a constructed expansion serving federal agency and coalition interests, this story''s beneficiary declarations understate the extraction and the classification should move toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregation_principle_naturalness, conceptual, 'Whether aggregate-effects reasoning is structurally necessary or an extractive doctrinal choice.').

omega_variable(
    kernel_reading_boundary_indeterminacy,
    'Where exactly does the expansive_federal_reading end and the substantial_effects_limited_reading begin — is the boundary a bright line (jurisdictional-element requirement) or a continuum that courts have moved along without a stable doctrinal marker?',
    'Trace the post-Lopez/Morrison case law to determine whether jurisdictional-nexus requirements function as a genuine categorical constraint or as a formalistic add-on that agencies satisfy with boilerplate findings clauses while substantive reach remains expansive.',
    'If the boundary is formalistic rather than substantive, the expansive_federal_reading and substantial_effects_limited_reading may be less structurally distinct than the kernel decomposition assumes, which would argue for re-examining whether these are genuinely two constraints or one constraint with a cosmetic overlay.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_indeterminacy, conceptual, 'Whether the expansive and limited-effects readings are structurally distinct or a single doctrine with formalistic variation.').

omega_variable(
    state_autonomy_victim_status,
    'Are state legislatures genuine victims of this reading, or partial beneficiaries who trade autonomy for federal cost-shifting (unfunded mandate absorption, federal preemption shielding them from politically costly local regulatory choices)?',
    'Examine state legislative behavior: do states actively litigate to reclaim regulatory authority when given the opportunity, or do they acquiesce to federal occupation because it removes politically difficult choices from state ballots?',
    'If states are partial beneficiaries of federal occupation (blame-shifting), the victim declaration for state_legislatures should be qualified or the directionality override toward a more symmetric d should be considered.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_autonomy_victim_status, empirical, 'Whether state legislatures are pure victims of preemption or partial beneficiaries of blame-shifting.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__expansive_federal_reading, 1937, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_text__expansive_federal_reading, theater_ratio, 1937, 0.1).
narrative_ontology:measurement(comm_tr_t1955, commerce_clause_text__expansive_federal_reading, theater_ratio, 1955, 0.14).
narrative_ontology:measurement(comm_tr_t1970, commerce_clause_text__expansive_federal_reading, theater_ratio, 1970, 0.18).
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_text__expansive_federal_reading, theater_ratio, 1995, 0.22).
narrative_ontology:measurement(comm_tr_t2012, commerce_clause_text__expansive_federal_reading, theater_ratio, 2012, 0.25).
narrative_ontology:measurement(comm_tr_t2025, commerce_clause_text__expansive_federal_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(comm_be_t1937, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1937, 0.35).
narrative_ontology:measurement(comm_be_t1955, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1955, 0.42).
narrative_ontology:measurement(comm_be_t1970, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(comm_be_t1995, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement(comm_be_t2012, commerce_clause_text__expansive_federal_reading, base_extractiveness, 2012, 0.56).
narrative_ontology:measurement(comm_be_t2025, commerce_clause_text__expansive_federal_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1937, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1937, 0.4).
narrative_ontology:measurement(comm_su_t1955, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1955, 0.48).
narrative_ontology:measurement(comm_su_t1970, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(comm_su_t1995, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1995, 0.58).
narrative_ontology:measurement(comm_su_t2012, commerce_clause_text__expansive_federal_reading, suppression_requirement, 2012, 0.6).
narrative_ontology:measurement(comm_su_t2025, commerce_clause_text__expansive_federal_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__expansive_federal_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_text__expansive_federal_reading, 0.12).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, originalist_narrow_reading).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, substantial_effects_limited_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language 'Commerce Clause' concept per the epsilon-invariance principle: expansive_federal_reading (this story, tangled_rope, epsilon 0.58), originalist_narrow_reading (expected mountain-leaning, low epsilon — commerce power as textually bounded to cross-border trade), and substantial_effects_limited_reading (expected rope-leaning, intermediate epsilon — jurisdictional-nexus-gated federal power). Each carries its own claimed_type, its own stakeholder set, and its own metrics; they are linked here via affects_constraints rather than merged, because measuring 'the Commerce Clause' under each reading yields materially different epsilon values, which is precisely the signal that these are three constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
