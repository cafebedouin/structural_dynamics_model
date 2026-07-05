% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__commons_conservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__commons_conservation, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ost_article_ii_non_appropriation__commons_conservation
 *   human_readable: Article II Non-Appropriation as Wall Against Extraction (Commons-Conservation Reading)
 *   domain: international_space_law/treaty_interpretation/commons_governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested Article II 'use or
 *   occupation' kernel: the commons-conservation reading, under which
 *   non-appropriation prohibits de facto appropriation via resource
 *   extraction and binds both states and private actors absent multilateral
 *   authorization. It is written as a self-contained, ε-invariant constraint
 *   — the sibling readings (extraction_permissive: bars only sovereign
 *   claims, not private ownership of extracted resources;
 *   international_regime: defers the question entirely to a future Article
 *   XI-analogue framework) are NOT described here as alternatives within this
 *   constraint; they are separate constraint stories linked via
 *   network.affects_constraints. Under this reading, unilateral extraction by
 *   any actor — state or private — constitutes a treaty violation,
 *   first-mover investments are legally exposed rather than protected, and
 *   non-spacefaring states retain a structural veto over any future enclosure
 *   because their consent is required for the multilateral authorization this
 *   reading demands.
 *
 * KEY AGENTS:
 *   - non_spacefaring_states: Primary beneficiary/agenda_setter (organized/constrained) — converts capability gap into veto power
 *   - first_mover_mining_investors: Primary target (powerful/trapped) — stranded capital under this reading
 *   - spacefaring_states_with_extraction_capability: Institutional payer (institutional/constrained) — domestic legislation does not satisfy this reading's multilateral requirement
 *   - multilateral_regime_advocates: Agenda_setter (organized/analytical) — actively maintains and litigates this interpretation
 *   - commercial_asteroid_ventures: Secondary payer (moderate/trapped) — financing chilled by interpretive uncertainty
 *   - future_generations_of_claimants: Non-agent beneficiary (analytical) — optionality preserved for actors not yet in existence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__commons_conservation, 0.28).
domain_priors:suppression_score(ost_article_ii_non_appropriation__commons_conservation, 0.58).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__commons_conservation, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, extractiveness, 0.28).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__commons_conservation, tangled_rope).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__commons_conservation, "Article II Non-Appropriation as Wall Against Extraction (Commons-Conservation Reading)").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__commons_conservation, "international_space_law/treaty_interpretation/commons_governance").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__commons_conservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__commons_conservation, '1a7c5d15-7b4b-4310-9cb5-9852bfac31b7').
narrative_ontology:cs_kernel_codification('1a7c5d15-7b4b-4310-9cb5-9852bfac31b7', fixed_text).
narrative_ontology:cs_authority_grounding('1a7c5d15-7b4b-4310-9cb5-9852bfac31b7', distributed).
narrative_ontology:cs_reading_relation('1a7c5d15-7b4b-4310-9cb5-9852bfac31b7', ost_article_ii_non_appropriation__extraction_permissive, forecloses).
narrative_ontology:cs_reading_relation('1a7c5d15-7b4b-4310-9cb5-9852bfac31b7', ost_article_ii_non_appropriation__international_regime, coexists_with).
narrative_ontology:cs_axiom('1a7c5d15-7b4b-4310-9cb5-9852bfac31b7', foundational, extraction_constitutes_de_facto_appropriation).
narrative_ontology:cs_axiom_status(extraction_constitutes_de_facto_appropriation, holdable).
narrative_ontology:cs_axiom_grounding('1a7c5d15-7b4b-4310-9cb5-9852bfac31b7', extraction_constitutes_de_facto_appropriation, conventional).
narrative_ontology:cs_axiom('1a7c5d15-7b4b-4310-9cb5-9852bfac31b7', foundational, non_appropriation_binds_private_actors).
narrative_ontology:cs_axiom_status(non_appropriation_binds_private_actors, holdable).
narrative_ontology:cs_axiom_grounding('1a7c5d15-7b4b-4310-9cb5-9852bfac31b7', non_appropriation_binds_private_actors, conventional).
narrative_ontology:cs_reference_frame('1a7c5d15-7b4b-4310-9cb5-9852bfac31b7', commons_conservation_1967_negotiating_intent).
narrative_ontology:cs_drift_state('1a7c5d15-7b4b-4310-9cb5-9852bfac31b7', post_commercial_space_resource_acts_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('1a7c5d15-7b4b-4310-9cb5-9852bfac31b7', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_states).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, future_generations_of_claimants).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, multilateral_regime_advocates).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, first_mover_mining_investors).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, commercial_asteroid_ventures).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, spacefaring_states_with_extraction_capability).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__commons_conservation, common_heritage_of_mankind_doctrine).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__commons_conservation, treaty_text_plain_meaning_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lack independent launch or extraction capability but hold equal votes in UN COPUOS and treaty-amendment forums. Under this reading, their consent is required before any multilateral authorization of extraction could occur, giving them an effective veto over enclosure. They benefit by converting a capability gap into a negotiating position: nothing can be legally extracted without their participation in whatever distributive framework eventually forms.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_states, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_states, agenda_setter).

% Have sunk capital into asteroid or lunar resource extraction technology and mission planning on the bet that extraction is or will become legally permissible. Under the commons-conservation reading, their activity constitutes prohibited de facto appropriation unless a multilateral regime authorizes it — a regime whose terms they do not control and whose formation timeline is indefinite. Their investment is effectively stranded: they cannot exit into a jurisdiction, only lobby for treaty reinterpretation or domestic legislation that conflicts with this reading.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, first_mover_mining_investors, payer,
    powerful, biographical, trapped, global).

% Possess the technical capacity to extract space resources and have in some cases passed domestic legislation (e.g., commercial space resource acts) asserting a right to retain extracted material. Under this reading, such domestic authorization does nothing to satisfy Article II's multilateral requirement — the state remains in violation of a treaty obligation it may have ratified. Their exit options are limited to withdrawal from the treaty (high diplomatic cost) or continued extraction under legal contestation.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, spacefaring_states_with_extraction_capability, payer,
    institutional, generational, constrained, global).

% International lawyers, diplomats, and developing-world coalitions who actively promote and enforce the reading that Article II bars de facto appropriation through extraction. They draft position papers, bring the interpretation into UN working groups, and treat unilateral extraction claims as treaty violations requiring collective response. They administer the interpretive apparatus that keeps this reading alive as a live legal claim.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, multilateral_regime_advocates, agenda_setter,
    organized, civilizational, analytical, universal).

% Smaller commercial entities without the political weight of major spacefaring states, dependent on legal certainty to raise capital. This reading's persistence as a live legal claim (rather than a settled question) makes their investment thesis contingent on unresolved treaty interpretation, chilling financing and insurance availability regardless of which reading eventually prevails.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, commercial_asteroid_ventures, payer,
    moderate, biographical, trapped, global).

% Not yet existing states, populations, or private actors who would inherit whatever distributive framework eventually governs space resources. This reading preserves optionality for them by preventing the outer space commons from being carved up under first-come-first-served logic before a distributive regime exists; they are not agents capable of asserting their own interests today.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, future_generations_of_claimants, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(ost_article_ii_non_appropriation__commons_conservation, future_generations_of_claimants).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents any single state or firm from converting technical or financial first-mover advantage into a permanent, uncontested property claim over space resources, preserving the question of allocation for collective resolution rather than unilateral fact-creation.
% TRANSFER_FUNCTION: Moves decision-rights over space resource allocation from whoever has extraction capability today to the collective body of treaty parties (weighted toward non-spacefaring states with equal votes), at the cost of stranding capital already committed by capable actors.
% ABSENT_VOICES: Private extraction firms and their financiers are not parties to the Outer Space Treaty and have no standing in the interstate forums where this reading is asserted and defended; their objection — that the treaty text does not clearly reach private resource extraction, only state territorial claims — is litigated through their home states' diplomatic posture, not directly.
% DISAPPEARANCE_RATIONALE: If the commons-conservation reading were abandoned, spacefaring states and firms would proceed with extraction under domestic authorization (as several already have via national space resource acts), and the practical world of asteroid/lunar mining would likely accelerate. Non-spacefaring states dispute that the reading is merely aspirational — they treat it as the operative legal constraint today, so from their seat its disappearance would remove a real veto, not a theoretical one. Whether the world 'rearranges' or 'was already rearranging around it' is precisely what the kernel contest is about.
% FOUNDING_PROBLEM: In 1967, no state had resource extraction capability in space, but the drafters anticipated that unregulated appropriation — whether through sovereign claims or economic exploitation — could recreate colonial-era enclosure dynamics on a cosmic scale, disadvantaging states that would develop capability later.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and COPUOS working-group records from the treaty's negotiation corroborate that non-appropriation was drafted with an eye toward economic exploitation, not merely flag-planting sovereignty claims — this comes from negotiating-history scholarship outside the current beneficiary coalition. However, states with active extraction programs and their domestic legislatures corroborate a narrower reading, and no international judicial body has adjudicated the dispute, so no authoritative outside umpire exists.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__commons_conservation, contested).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__commons_conservation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__commons_conservation, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__commons_conservation, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__commons_conservation, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__commons_conservation_tests).
:- end_tests(ost_article_ii_non_appropriation__commons_conservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-to-moderate (0.28 by interval end) because this reading, taken alone, primarily withholds a right (extraction absent authorization) rather than actively extracting value from a payer class — the cost to first-movers is opportunity cost and stranded investment, not a rent flowing to a collecting party in the ordinary sense. Suppression is moderate-to-high (0.58) because enforcing this reading against a determined spacefaring state with domestic legislation requires real diplomatic and legal pressure, and that pressure has intensified as commercial extraction capability has become real rather than hypothetical. Theater ratio rises over the interval (0.10 to 0.32) reflecting that much of the reading's contemporary defense occurs through position papers, working-group statements, and non-binding resolutions rather than binding adjudication — the interpretive apparatus performs enforcement without a tribunal that can actually stop a launch.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-spacefaring states are the structural beneficiaries: this reading converts their lack of capability into leverage, since their participation becomes legally necessary before any authorization can occur. First-mover investors and capable spacefaring states are the targets: they bear the cost of stranded capital and continued legal exposure. The directionality here is somewhat inverted from typical extraction stories — the 'beneficiary' does not collect a flow so much as retain a veto, and the 'victim' does not have wealth extracted so much as a valuable option foreclosed. This is why claimed_type sits at tangled_rope rather than snare: there is a genuine coordination function (preventing unilateral enclosure of a genuine commons) alongside asymmetric cost-bearing (capable actors pay for a rule that non-capable actors did nothing to earn the benefit of, beyond holding equal treaty votes).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing unregulated appropriation from recreating colonial enclosure dynamics — remains genuinely live in the sense that no distributive framework has been agreed and extraction capability is now real rather than theoretical, which is exactly the scenario the drafters worried about. This blocks the mandatrophy read where the constraint would be pure inertial performance: the underlying coordination problem the treaty text addresses has not gone away, even though the specific text's applicability to private commercial actors is contested. Founding_problem_status is authored as 'contested' rather than 'dead' precisely because the empirical question (has extraction capability outrun the governance framework the treaty anticipated?) is answered yes by most observers, but whether THIS reading is the correct legal response to that gap is what the kernel contest is about.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    private_actor_treaty_coverage,
    'Does Article II''s prohibition, addressed textually to ''national appropriation,'' extend to private commercial actors who are not themselves states, or does it bind only state parties (leaving private extraction to be regulated, if at all, through domestic law and state responsibility doctrines)?',
    'An authoritative international judicial ruling (e.g., ICJ advisory opinion) or a binding multilateral protocol explicitly extending or limiting Article II''s reach to non-state actors. Absent that, state practice and the reaction of other treaty parties to domestic space resource acts (US, Luxembourg, UAE, Japan) serves as the closest available evidence.',
    'If private actors are covered, this reading''s wall-constraint character is strongly reinforced — no domestic legislative workaround can cure the violation. If private actors are not covered, this reading collapses substantially toward the extraction_permissive sibling for all practical commercial purposes, since states could route extraction through nominally private ventures.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(private_actor_treaty_coverage, conceptual, 'Whether Article II''s non-appropriation principle textually and legally reaches private (non-state) commercial actors.').

omega_variable(
    commons_vs_constructed_veto,
    'Is the veto held by non-spacefaring states under this reading a genuine expression of a real collective-ownership commons principle, or a constructed leverage point that lets capability-poor states extract negotiating rents from capability-rich states and firms in any eventual distributive settlement?',
    'Examine whether non-spacefaring states'' negotiating positions in COPUOS and related forums are structured around principled distributive justice claims (e.g., common heritage of mankind doctrine applied consistently across other commons regimes) versus ad hoc bargaining postures that shift with anticipated payoff share.',
    'If genuine commons principle, the tangled_rope classification with a real coordination function is well-supported. If primarily constructed leverage, the reading tips closer to snare — extraction of concessions from capable actors dressed in commons language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_vs_constructed_veto, conceptual, 'Whether the non-spacefaring veto reflects principled commons governance or constructed rent-extraction via treaty leverage.').

omega_variable(
    reading_correctness_underdetermination,
    'Given that no international tribunal has authoritatively adjudicated among the commons_conservation, extraction_permissive, and international_regime readings of Article II, is the current state of affairs one where a determinate correct legal answer exists but has not yet been pronounced, or one where the treaty text is genuinely indeterminate and the eventual answer will be settled by power and practice rather than interpretation?',
    'Comparative analysis of how analogous open-textured treaty terms have been resolved historically (via ICJ adjudication, subsequent practice under VCLT Article 31(3)(b), or formal amendment) versus left permanently unresolved and settled by capability.',
    'If determinate-but-unpronounced, sustained legal advocacy for this reading is a legitimate project of clarifying existing law. If genuinely indeterminate, this reading''s persistence as a ''live legal claim'' functions more as a bargaining chip than a legal argument awaiting vindication — which would push the theater_ratio interpretation higher.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_correctness_underdetermination, conceptual, 'Whether the kernel''s underdetermination reflects unpronounced determinate law or genuinely open texture to be settled by practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__commons_conservation, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t0, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ost__tr_t11, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 11, 0.14).
narrative_ontology:measurement(ost__tr_t22, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 22, 0.19).
narrative_ontology:measurement(ost__tr_t33, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 33, 0.24).
narrative_ontology:measurement(ost__tr_t44, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 44, 0.29).
narrative_ontology:measurement(ost__tr_t55, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 55, 0.32).

% Extraction over time
narrative_ontology:measurement(ost__be_t0, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(ost__be_t11, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 11, 0.15).
narrative_ontology:measurement(ost__be_t22, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 22, 0.18).
narrative_ontology:measurement(ost__be_t33, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 33, 0.22).
narrative_ontology:measurement(ost__be_t44, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 44, 0.26).
narrative_ontology:measurement(ost__be_t55, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 55, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t0, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(ost__su_t11, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 11, 0.4).
narrative_ontology:measurement(ost__su_t22, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 22, 0.46).
narrative_ontology:measurement(ost__su_t33, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 33, 0.51).
narrative_ontology:measurement(ost__su_t44, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 44, 0.55).
narrative_ontology:measurement(ost__su_t55, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 55, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__commons_conservation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ost_article_ii_non_appropriation__commons_conservation, 0.12).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation__extraction_permissive).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation__international_regime).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the ost_article_ii_non_appropriation kernel, all sharing the same treaty text but instantiating structurally distinct constraints with different ε profiles and different victim/beneficiary sets. commons_conservation (this story) treats the prohibition as already operative and extending to private actors, producing a wall constraint against extraction absent multilateral authorization. extraction_permissive treats only sovereign claims as barred, permitting private resource ownership, and would show materially lower extractiveness against mining ventures and near-zero suppression against them. international_regime treats the entire question as deferred to a not-yet-existent framework, producing a constraint whose primary effect is regulatory uncertainty rather than either prohibition or permission. Per the ε-invariance principle, these are not one constraint measured three ways — they are three constraints because the beneficiary/victim structure and the operative legal claim differ substantively across readings. Do not average or reconcile ε across the three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
