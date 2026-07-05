% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__broad_effects_test
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Commerce Clause — Substantial Effects / Aggregation Doctrine Reading
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   This story instantiates the broad-effects/aggregation reading of the
 *   Commerce Clause kernel: 'commerce' extends to any economic activity that,
 *   in the aggregate, substantially affects interstate markets, and
 *   'regulate' includes outright prohibition and comprehensive federal
 *   control. Under this reading — traceable through Wickard v. Filburn's
 *   wheat-quota logic and reaffirmed selectively in Gonzales v. Raich — a
 *   single farmer's home-consumed wheat crop, or an individual's home-grown
 *   marijuana, becomes federally regulable because the aggregate class of
 *   similar conduct nationwide would undermine a federal regulatory scheme if
 *   left unregulated. This is one reading among three live readings of the
 *   same constitutional kernel (the enumerated Commerce Power). The
 *   narrow_originalist reading confines 'commerce' to interstate trade itself
 *   and 'regulate' to facilitation, not restriction. The
 *   intermediate_channels reading (Lopez/Morrison) preserves three categories
 *   of federal reach but insists non-economic activity needs a jurisdictional
 *   hook and rejects attenuated causal-chain reasoning. This story is the
 *   broad_effects_test reading only — it does not average across readings or
 *   describe the contest internally; sibling readings are separate constraint
 *   files linked via network edges.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__broad_effects_test, 0.68).
domain_priors:suppression_score(commerce_clause_scope__broad_effects_test, 0.6).
domain_priors:theater_ratio(commerce_clause_scope__broad_effects_test, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, extractiveness, 0.68).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__broad_effects_test, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__broad_effects_test, "Commerce Clause — Substantial Effects / Aggregation Doctrine Reading").
narrative_ontology:topic_domain(commerce_clause_scope__broad_effects_test, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_scope__broad_effects_test).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__broad_effects_test, 'a651601d-d41b-4a0c-830a-69d84b2b761e').
narrative_ontology:cs_kernel_codification('a651601d-d41b-4a0c-830a-69d84b2b761e', fixed_text).
narrative_ontology:cs_authority_grounding('a651601d-d41b-4a0c-830a-69d84b2b761e', lineage).
narrative_ontology:cs_interpretation_layer_present('a651601d-d41b-4a0c-830a-69d84b2b761e').
narrative_ontology:cs_reading_relation('a651601d-d41b-4a0c-830a-69d84b2b761e', commerce_clause_scope__narrow_originalist, forecloses).
narrative_ontology:cs_reading_relation('a651601d-d41b-4a0c-830a-69d84b2b761e', commerce_clause_scope__intermediate_channels, influences).
narrative_ontology:cs_axiom('a651601d-d41b-4a0c-830a-69d84b2b761e', foundational, aggregate_cumulative_effect_establishes_jurisdiction).
narrative_ontology:cs_axiom_status(aggregate_cumulative_effect_establishes_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('a651601d-d41b-4a0c-830a-69d84b2b761e', aggregate_cumulative_effect_establishes_jurisdiction, instrumental).
narrative_ontology:cs_axiom('a651601d-d41b-4a0c-830a-69d84b2b761e', foundational, regulate_includes_prohibition_and_comprehensive_control).
narrative_ontology:cs_axiom_status(regulate_includes_prohibition_and_comprehensive_control, holdable).
narrative_ontology:cs_axiom_grounding('a651601d-d41b-4a0c-830a-69d84b2b761e', regulate_includes_prohibition_and_comprehensive_control, conventional).
narrative_ontology:cs_reference_frame('a651601d-d41b-4a0c-830a-69d84b2b761e', new_deal_integrated_economy_doctrine).
narrative_ontology:cs_drift_state('a651601d-d41b-4a0c-830a-69d84b2b761e', post_lopez_morrison_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a651601d-d41b-4a0c-830a-69d84b2b761e', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__broad_effects_test, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, federal_regulatory_agencies).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, national_interest_advocacy_groups).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, civil_rights_enforcement_bodies).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, national_industry_lobbies_seeking_uniformity).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, state_legislatures).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, local_economic_experimentation).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, intrastate_small_producers).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, dissenting_states_policy_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and enforce regulations reaching intrastate activity by invoking cumulative national economic effect. Sets the doctrinal boundary through litigation posture and rulemaking; benefits directly from an expansive jurisdictional base because it enlarges the agency's own regulatory reach and staffing justification.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, federal_regulatory_agencies, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__broad_effects_test, federal_regulatory_agencies, beneficiary).

% Prefer a single national policy target over fifty separate state fights. The broad-effects reading lets a single federal statute or rule achieve what would otherwise require decades of state-by-state advocacy; they can redirect resources to one legislative or regulatory venue.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, national_interest_advocacy_groups, beneficiary,
    organized, generational, mobile, national).

% Rely on the aggregation doctrine's reach into ostensibly local activity (public accommodations, employment) to impose uniform anti-discrimination baselines that state governments would not adopt uniformly on their own. The doctrine is instrumental to enforcement capacity here, not merely rent extraction.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, civil_rights_enforcement_bodies, beneficiary,
    institutional, generational, arbitrage, national).

% Lose the ability to set distinct policy on activity now reachable as 'economic in the aggregate' — agriculture, labor, land use, healthcare mandates. Can litigate or lobby for statutory carve-outs but cannot exit the doctrine itself; sovereign immunity does not extend to jurisdictional scope.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, state_legislatures, payer,
    institutional, generational, constrained, national).

% The capacity of states to trial divergent regulatory approaches (drug policy, minimum wage floors, agricultural quotas) is foreclosed wherever a federal aggregation claim can be asserted; policy variation that would generate comparative evidence never occurs.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, local_economic_experimentation, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_non_agent(commerce_clause_scope__broad_effects_test, local_economic_experimentation).

% A farmer growing wheat for personal consumption, a home-based producer, or a purely local service business becomes subject to federal regulation because their conduct, aggregated across all similarly situated actors, is deemed to affect the national market. They have essentially no individual exit — their conduct is too small to litigate and too diffuse to organize around alone.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, intrastate_small_producers, payer,
    powerless, biographical, trapped, local).

% The value of federalism as a check on centralized power — the idea that states should be free to diverge, even wrongly, as insurance against national policy error — has no seat at the table once the aggregation doctrine treats state divergence itself as an obstacle to comprehensive federal control.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, dissenting_states_policy_autonomy, excluded,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(commerce_clause_scope__broad_effects_test, dissenting_states_policy_autonomy).

% Track the doctrinal trajectory from Wickard through Lopez, Morrison, and Raich, documenting where the aggregation principle has been applied, narrowed, or resisted, and assess whether the 'substantial effects' standard retains a limiting principle at all.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_scope__broad_effects_test, diffuse).
narrative_ontology:fixing_cost_class(commerce_clause_scope__broad_effects_test, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables uniform national policy on matters where local economic decisions, taken individually, are trivial but collectively determine national market conditions (commodity prices, labor standards, environmental externalities) — solving a genuine collective-action problem that fifty independent state regimes cannot solve alone.
% TRANSFER_FUNCTION: Moves regulatory authority and enforcement discretion from state legislatures and local actors to federal agencies and national interest coalitions; moves compliance costs from a negotiated state-level baseline to a federally set floor or ceiling that individual local actors did not vote on and cannot exit.
% ABSENT_VOICES: Individual intrastate producers whose conduct is aggregated into a national statistic never appear as parties — they are represented, if at all, by trade associations with their own agendas. The abstract federalism interest (diversity of policy as a check on error) has no institutional advocate; it appears only in dissenting opinions and academic commentary.
% DISAPPEARANCE_RATIONALE: If the aggregation/substantial-effects doctrine were narrowed to channels-and-instrumentalities only, wide swaths of federal labor, environmental, healthcare, and civil-rights legislation would lose their jurisdictional basis overnight, forcing either constitutional amendment, a patchwork of fifty state regimes, or a scramble to re-ground existing statutes in alternative enumerated powers (taxing/spending, treaty power).
% FOUNDING_PROBLEM: The New Deal-era doctrine was built to prevent a formalist 'production vs. commerce' distinction from disabling federal response to an integrated national economy in crisis — Congress needed to reach conduct that was locally structured but nationally consequential (wage deflation, agricultural overproduction, labor unrest with interstate spillover).
% FOUNDING_PROBLEM_CORROBORATION: Federal agencies and civil rights bodies attest the founding problem remains live — modern supply chains and labor markets are, if anything, more integrated than in 1937. Independent legal historians and several sitting appellate judges (outside the beneficiary set) corroborate that the doctrine has since been extended well past the integrated-national-economy rationale into domains (non-economic conduct, purely local transactions) the New Deal Court never contemplated reaching, per the Lopez and Morrison opinions' own historical accounting.
narrative_ontology:disappearance_verdict(commerce_clause_scope__broad_effects_test, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__broad_effects_test, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__broad_effects_test, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored high (0.68) because the doctrine, at its broadest application, subsumes essentially all local economic conduct into federal jurisdiction — the victim set (state autonomy, local experimentation, small intrastate actors) bears a cost with no comparable exit. Suppression is authored moderate-high (0.6): the doctrine does not physically coerce compliance but its jurisdictional reach forecloses the alternative of state-level divergence once a federal aggregation claim is credibly asserted, and enforcement (agency rulemaking, DOJ litigation posture) actively defends the boundary against narrowing. Theater ratio is moderate-low (0.3): the coordination function (uniform national economic policy, civil rights enforcement) is real and substantial, not primarily performative, though the aggregation logic in its most attenuated form (Wickard's home-wheat holding) functions partly as doctrinal cover for reach that the 'substantial effects' language does not on its face compel.
 *
 * PERSPECTIVAL GAP:
 *   From the federal agency seat, the doctrine reads as a Rope — solving a genuine coordination problem (national market integration) that individual states cannot solve by uncoordinated local action. From the state legislature or intrastate small-producer seat, the identical doctrine reads as a Tangled Rope shading toward Snare — a coordination story providing cover for extraction of policy autonomy with no meaningful exit. The engine should compute these divergently from the same structural data; the claimed_type of tangled_rope reflects the coexistence of the genuine coordination function (civil rights uniformity, integrated market regulation) with the asymmetric extraction (state sovereignty and local experimentation bear costs enforced by active federal litigation posture).
 *
 * DIRECTIONALITY LOGIC:
 *   Federal regulatory agencies and civil rights enforcement bodies sit at the beneficiary end: the doctrine directly enlarges their jurisdiction and enforcement capacity, and they actively defend its scope in litigation. National interest advocacy groups benefit by substituting one national venue for fifty state fights. State legislatures and intrastate small producers sit at the target end: they bear the loss of policy autonomy and compliance burden without a comparable exit — a wheat farmer cannot 'exit' the national wheat market to escape the aggregation logic that reaches him. Local economic experimentation and dissenting states' policy autonomy are non-agent structural values (marked agent:false) harmed by the doctrine's operation but incapable of bearing costs as parties in the ordinary sense; they are included for completeness of the victim structure, not as directionality-bearing actors.
 *
 * MANDATROPHY ANALYSIS:
 *   The 1937 founding problem (an integrated national economy that formalist doctrine could not reach) is genuinely dead in its original crisis form but the doctrine's application has not narrowed correspondingly — it has instead been extended into non-economic domains (Morrison's gender-violence statute, Lopez's school-gun-zone statute — both ultimately checked) demonstrating the aggregation logic's tendency to drift past its founding rationale unless actively cabined by countervailing doctrine. This is precisely the mandatrophy pattern: a mandate whose founding crisis has resolved but whose administrative apparatus and doctrinal momentum persist and expand.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aggregation_limiting_principle_ambiguity,
    'Does the substantial-effects/aggregation doctrine retain any judicially enforceable limiting principle, or does it in practice reach any conduct Congress chooses to regulate given sufficiently creative aggregation?',
    'Track post-2024 circuit splits and Supreme Court certiorari grants on Commerce Clause challenges to federal statutes reaching non-economic or purely local conduct; a consistent pattern of Court-enforced jurisdictional-element requirements would indicate a live limiting principle, while consistent deference would indicate the doctrine is functionally unlimited.',
    'If no limiting principle survives in practice, this reading''s classification shifts further toward snare (federal power over state autonomy with no genuine remaining check); if a limiting principle is actively enforced, the intermediate_channels reading effectively displaces this one in practice even where broad_effects language is retained rhetorically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregation_limiting_principle_ambiguity, conceptual, 'Whether the aggregation doctrine has a real ceiling or is unlimited in practice.').

omega_variable(
    genuine_vs_constructed_national_market,
    'Is the premise that virtually all economic activity is ''truly'' part of an integrated national market a genuine empirical description of modern economic interdependence, or a constructed rationale that beneficiary institutions (federal agencies, national advocacy groups) have an interest in maintaining regardless of its empirical accuracy for any given regulated activity?',
    'Case-by-case economic analysis of the actual interstate spillover magnitude for specific regulated conduct (e.g., a single wheat farmer''s home consumption vs. national agricultural policy scale effects), compared against the doctrinal claim that aggregation alone establishes substantiality without such case-specific showing.',
    'If the aggregation premise is substantially constructed rather than empirically demonstrated case-by-case, this strengthens the tangled_rope/snare reading; if genuinely demonstrable at the level the doctrine claims, the coordination function is more robust than the extraction framing suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(genuine_vs_constructed_national_market, empirical, 'Whether aggregation-based national-market claims are empirically grounded or doctrinally assumed.').

omega_variable(
    reading_selection_committer_ambiguity,
    'Given that three structurally distinct readings of the Commerce Clause kernel are simultaneously live in federal case law depending on which line of precedent a court chooses to emphasize, what determines which reading a given court or era actually applies, and is that selection itself principled or outcome-driven?',
    'Trace citation patterns and doctrinal framing choices across circuits and eras to determine whether reading selection correlates with case outcome preference (suggesting outcome-driven selection) or with a stable jurisprudential commitment independent of outcome.',
    'If reading selection is outcome-driven, all three sibling constraints (this one, narrow_originalist, intermediate_channels) coexist not as competing legal theories resolved by argument but as a toolkit selected post hoc, which would deepen this reading''s tangled_rope character (the coordination story providing cover for whichever extraction a given case''s beneficiaries seek).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_committer_ambiguity, conceptual, 'Whether the choice among the three kernel readings in actual adjudication is principled or results-driven.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__broad_effects_test, 1937, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_scope__broad_effects_test, theater_ratio, 1937, 0.1).
narrative_ontology:measurement(comm_tr_t1955, commerce_clause_scope__broad_effects_test, theater_ratio, 1955, 0.15).
narrative_ontology:measurement(comm_tr_t1975, commerce_clause_scope__broad_effects_test, theater_ratio, 1975, 0.22).
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_scope__broad_effects_test, theater_ratio, 1995, 0.25).
narrative_ontology:measurement(comm_tr_t2005, commerce_clause_scope__broad_effects_test, theater_ratio, 2005, 0.28).
narrative_ontology:measurement(comm_tr_t2024, commerce_clause_scope__broad_effects_test, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(comm_be_t1937, commerce_clause_scope__broad_effects_test, base_extractiveness, 1937, 0.35).
narrative_ontology:measurement(comm_be_t1955, commerce_clause_scope__broad_effects_test, base_extractiveness, 1955, 0.48).
narrative_ontology:measurement(comm_be_t1975, commerce_clause_scope__broad_effects_test, base_extractiveness, 1975, 0.6).
narrative_ontology:measurement(comm_be_t1995, commerce_clause_scope__broad_effects_test, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement(comm_be_t2005, commerce_clause_scope__broad_effects_test, base_extractiveness, 2005, 0.63).
narrative_ontology:measurement(comm_be_t2024, commerce_clause_scope__broad_effects_test, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1937, commerce_clause_scope__broad_effects_test, suppression_requirement, 1937, 0.4).
narrative_ontology:measurement(comm_su_t1955, commerce_clause_scope__broad_effects_test, suppression_requirement, 1955, 0.5).
narrative_ontology:measurement(comm_su_t1975, commerce_clause_scope__broad_effects_test, suppression_requirement, 1975, 0.58).
narrative_ontology:measurement(comm_su_t1995, commerce_clause_scope__broad_effects_test, suppression_requirement, 1995, 0.45).
narrative_ontology:measurement(comm_su_t2005, commerce_clause_scope__broad_effects_test, suppression_requirement, 2005, 0.55).
narrative_ontology:measurement(comm_su_t2024, commerce_clause_scope__broad_effects_test, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__broad_effects_test, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, commerce_clause_scope__intermediate_channels).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, commerce_clause_scope__narrow_originalist).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposing the natural-language 'Commerce Clause scope' label per the ε-invariance principle. broad_effects_test (this file) carries the highest ε and the most expansive victim set; intermediate_channels carries a moderate ε with an explicit jurisdictional-element limiting principle; narrow_originalist carries the lowest ε, confining federal reach to interstate trade facilitation. The three are linked bidirectionally in intent (each is a live doctrinal option courts select among) though this file only declares the outbound edges per schema convention.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
