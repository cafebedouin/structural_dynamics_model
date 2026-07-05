% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__expansive_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_maritime_sovereignty__expansive_construction_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: unclos_maritime_sovereignty__expansive_construction_reading
 *   human_readable: Expansive Construction Reading of Maritime Sovereignty (Artificial Island Territorial Sea Generation)
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This story instantiates the expansive construction reading of the
 *   contested UNCLOS maritime sovereignty kernel: the claim that building
 *   infrastructure atop submerged features or low-tide elevations, followed
 *   by sustained administrative and military presence, generates a de facto
 *   territorial sea regardless of the feature's natural geological status.
 *   This is a distinct constraint from the strict-geographic reading (which
 *   holds the codified UNCLOS text closely and treats construction as legally
 *   inert) and from the hybrid effective-control reading (which grants
 *   artificial features only limited safety zones absent prolonged,
 *   unchallenged control). Each reading has a different beneficiary/victim
 *   structure and a different epsilon; they are not one constraint viewed
 *   from three angles but three distinct constraints linked through the
 *   shared kernel.
 *
 * KEY AGENTS:
 *   - island_constructing_states: primary beneficiary and agenda-setter (institutional/arbitrage) — converts construction into sovereignty claim
 *   - neighboring_claimant_states: primary target (moderate/constrained) — loses overlapping claims to occupied facts on the water
 *   - freedom_of_navigation_states: secondary target (powerful/mobile) — bears contested-transit costs but retains global exit
 *   - regional_fishing_communities: most powerless target (powerless/trapped) — loses livelihood access with no standing
 *   - international_arbitral_tribunals: excluded authoritative voice — rules against the reading but cannot enforce
 *   - maritime_law_scholars: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__expansive_construction_reading, 0.79).
domain_priors:suppression_score(unclos_maritime_sovereignty__expansive_construction_reading, 0.71).
domain_priors:theater_ratio(unclos_maritime_sovereignty__expansive_construction_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__expansive_construction_reading, snare).
narrative_ontology:human_readable(unclos_maritime_sovereignty__expansive_construction_reading, "Expansive Construction Reading of Maritime Sovereignty (Artificial Island Territorial Sea Generation)").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__expansive_construction_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__expansive_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__expansive_construction_reading, '525f03af-de6f-4907-9684-fa13dd9af4ea').
narrative_ontology:cs_kernel_codification('525f03af-de6f-4907-9684-fa13dd9af4ea', fixed_text).
narrative_ontology:cs_authority_grounding('525f03af-de6f-4907-9684-fa13dd9af4ea', extraction).
narrative_ontology:cs_interpretation_layer_present('525f03af-de6f-4907-9684-fa13dd9af4ea').
narrative_ontology:cs_reading_relation('525f03af-de6f-4907-9684-fa13dd9af4ea', unclos_maritime_sovereignty__strict_geographic_reading, forecloses).
narrative_ontology:cs_reading_relation('525f03af-de6f-4907-9684-fa13dd9af4ea', unclos_maritime_sovereignty__hybrid_effective_control_reading, coexists_with).
narrative_ontology:cs_axiom('525f03af-de6f-4907-9684-fa13dd9af4ea', foundational, effective_occupation_generates_territorial_status).
narrative_ontology:cs_axiom_status(effective_occupation_generates_territorial_status, holdable).
narrative_ontology:cs_axiom_grounding('525f03af-de6f-4907-9684-fa13dd9af4ea', effective_occupation_generates_territorial_status, conventional).
narrative_ontology:cs_axiom('525f03af-de6f-4907-9684-fa13dd9af4ea', secondary, administrative_continuity_cures_natural_feature_deficiency).
narrative_ontology:cs_axiom_status(administrative_continuity_cures_natural_feature_deficiency, holdable).
narrative_ontology:cs_axiom_grounding('525f03af-de6f-4907-9684-fa13dd9af4ea', administrative_continuity_cures_natural_feature_deficiency, instrumental).
narrative_ontology:cs_reference_frame('525f03af-de6f-4907-9684-fa13dd9af4ea', unclos_codified_natural_feature_test).
narrative_ontology:cs_drift_state('525f03af-de6f-4907-9684-fa13dd9af4ea', post_2016_arbitral_ruling, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('525f03af-de6f-4907-9684-fa13dd9af4ea', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, neighboring_claimant_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, regional_fishing_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dredges and builds military-capable infrastructure atop submerged reefs and low-tide elevations, then administers the resulting features as if they were naturally formed islands — issuing permits, stationing personnel, and treating a 12nm territorial sea (or broader claimed zone) as attached to the construction. Justifies the claim as effective occupation and administrative continuity rather than the strict-geographic reading's above-water-at-high-tide test. Bears the capital cost of construction but converts it into a durable sovereignty claim that outlives the concrete.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states, beneficiary).

% Hold overlapping or prior claims to the same submerged features or nearby waters under the strict-geographic reading, but find their claims practically foreclosed once construction, permanent occupation, and administrative infrastructure are in place. Diplomatic protest and arbitration rulings exist but lack an enforcement mechanism to reverse physical occupation; their exit options are limited to legal filings, coalition-building, or acquiescence.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, neighboring_claimant_states, payer,
    moderate, biographical, constrained, regional).

% Rely on high-seas and innocent-passage rights through the contested waters for commercial and military transit. The expansive reading shrinks the zone of unimpeded passage and forces a choice between conducting contested freedom-of-navigation operations (risking incident) or accepting a de facto expanded territorial sea. Their global reach gives them more exit than the neighboring claimants, but each transit through the zone is now a contestable act rather than a routine one.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_states, payer,
    powerful, generational, mobile, global).

% Have fished the waters around the submerged features for generations under customary or strict-geographic-reading rules. Once the constructing state's coast guard and militia enforce the newly claimed zone, traditional fishing grounds become contested or off-limits, with vessels subject to boarding, ramming, or confiscation. They have no standing in interstate arbitration and no meaningful capacity to relocate their livelihood.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, regional_fishing_communities, payer,
    powerless, biographical, trapped, local).

% Bodies such as UNCLOS Annex VII tribunals can rule (as in the 2016 South China Sea arbitration) that submerged features and low-tide elevations cannot generate territorial sea regardless of construction. Their rulings are binding in principle but the tribunal has no enforcement arm; the constructing state can simply decline to participate or to comply, so the tribunal's authoritative reading is structurally excluded from altering the facts on the water.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, international_arbitral_tribunals, excluded,
    institutional, civilizational, analytical, global).

% Document the gap between the codified UNCLOS text (which the strict-geographic reading tracks closely) and the operational reality of expansive construction claims. They analyze state practice, protest patterns, and enforcement asymmetries without being able to compel any party to adopt a particular reading.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, maritime_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states).
narrative_ontology:fixing_cost_class(unclos_maritime_sovereignty__expansive_construction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its own terms, this reading claims to coordinate a genuine gap in customary international law: how effective, continuous state administration over a physically improved feature should be recognized when the feature's natural geological status is ambiguous or contested. It offers a bright-line administrative test (occupation plus infrastructure equals recognized control) in place of an unresolved geographic dispute.
% TRANSFER_FUNCTION: Moves fishing grounds, transit rights, and adjacent seabed/resource access from neighboring claimant states, freedom-of-navigation states, and local fishing communities to the constructing state, converting a capital expenditure on dredging and construction into a durable claim over maritime space and the resources within it.
% ABSENT_VOICES: Regional fishing communities have no seat in interstate arbitration or bilateral negotiation and are rarely named parties to the diplomatic protests filed on their behalf. International arbitral tribunals have ruled against this reading's core premise but their rulings are excluded from altering practice because the constructing state can decline compliance without a compliance-forcing mechanism.
% DISAPPEARANCE_RATIONALE: If the expansive construction reading were abandoned overnight and construction-based claims reverted to a strict-geographic standard, contested reefs would revert to submerged-feature status generating no territorial sea, previously excluded fishing fleets would return, freedom-of-navigation transits would no longer require contestation, and the constructing state would lose the administrative and military footprint it has built on the claim — a substantial rearrangement of regional maritime access and military posture.
% FOUNDING_PROBLEM: Genuinely ambiguous cases existed in customary law before UNCLOS codification: some coastal features shift between submerged and exposed states with tide and erosion, and long-standing administrative presence has historically been one factor (among others) in resolving genuinely disputed sovereignty. The reading extends this pre-codification ambiguity-resolution logic into a categorical entitlement generated by construction itself.
% FOUNDING_PROBLEM_CORROBORATION: UNCLOS Article 60(8) and the 2016 Permanent Court of Arbitration ruling in Philippines v. China explicitly state that artificial islands, installations, and structures do not possess the status of islands and have no territorial sea of their own — a determination made by a body outside the constructing state and not contested by any neutral international law authority. Maritime law scholars outside the constructing state's institutions corroborate that the codified problem (ambiguous natural feature status) was resolved by UNCLOS text itself; the construction-based reading persists as an assertion against, not a resolution of, that codified answer.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__expansive_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__expansive_construction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__expansive_construction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unclos_maritime_sovereignty__expansive_construction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__expansive_construction_reading, 0.79, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__expansive_construction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_maritime_sovereignty__expansive_construction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_maritime_sovereignty__expansive_construction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.35 to 0.79) tracking the escalation from initial dredging to fully militarized, administratively entrenched features — the claim strengthens as facts on the ground accumulate, independent of any change in the underlying legal text. Theater ratio also rises (0.25 to 0.62) because an increasing share of the constructing state's activity is dedicated to performing sovereignty (flag-raising, civilian settlement, tourism visits, administrative signage) rather than to any function genuinely requiring the disputed 12nm zone. Suppression is substantial and rising (0.40 to 0.71): the reading persists only because coast guard and militia presence actively excludes rival claimants' vessels and deters freedom-of-navigation transits from being routine. All three metrics share one time grid, honoring the alignment rule.
 *
 * PERSPECTIVAL GAP:
 *   From the constructing state's seat, this reading looks like coordination: a stable administrative rule resolving genuine geographic ambiguity, defensible on continuity-of-occupation grounds long recognized in customary law. From the neighboring claimants', fishing communities', and freedom-of-navigation states' seats, the identical structure computes as extraction backed by suppression — the same infrastructure that stabilizes the constructing state's claim is what excludes them. The engine's per-seat computation should reproduce this asymmetry precisely because the beneficiary/victim declarations and exit-option data differ sharply across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The constructing state is the clear structural beneficiary: it bears the one-time construction cost but converts that cost into a recurring claim over fishing grounds, seabed resources, and strategic waters — d sits near the full-beneficiary end, especially given its arbitrage-grade exit (it can simply decline arbitral jurisdiction). Neighboring claimant states and regional fishing communities sit near the full-target end: their prior access and legal claims are functionally overridden by occupation, and their exit options (constrained, trapped respectively) give them little leverage. Freedom-of-navigation states are targets too, but their global mobility and powerful status keep their directionality less extreme than the fishing communities' — they can reroute or contest, where the fishing communities cannot.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem this reading claims to solve (genuine ambiguity about how to treat features that shift status with tide and erosion) is corroborated as dead by UNCLOS Article 60(8) and the 2016 arbitral ruling, both external to the constructing state's own institutions. The reading's persistence after its founding problem was resolved by codified text is the signature of a live-mandate-turned-extraction structure rather than genuine unresolved coordination — this is why the claimed type is snare rather than tangled_rope: there is no active party being coordinated in a mutually beneficial sense, only administrative continuity substituting for a coordination function that codification already discharged.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    construction_as_evidence_of_occupation_vs_status_change,
    'Does administrative infrastructure on a submerged feature function merely as EVIDENCE of a state''s occupation intent (a longstanding, more modest customary-law role), or does this reading claim it AFFIRMATIVELY CHANGES the feature''s legal status from submerged/low-tide to island-equivalent? The three kernel readings disagree precisely here.',
    'Comparative analysis of pre-UNCLOS customary state practice (where occupation evidence played a role in genuinely ambiguous natural-feature disputes) versus post-UNCLOS Article 60(8) and Article 121(1) text, which explicitly separates artificial structures from natural islands. A tribunal or ICJ ruling directly addressing whether construction can cure a feature''s submerged/low-tide status would resolve this.',
    'If construction is merely evidentiary, this reading collapses toward the hybrid_effective_control_reading (limited safety zones, possible slow maturation). If it is a status-changing claim in its own right, it remains structurally distinct and maximally extractive relative to the other two readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(construction_as_evidence_of_occupation_vs_status_change, conceptual, 'Whether construction evidences occupation or changes legal status — the core distinguishing premise versus the sibling readings.').

omega_variable(
    kernel_disagreement_location,
    'Where exactly does the three-way kernel disagreement sit — is it a dispute over the FACTS (what UNCLOS Article 121 and 60(8) actually require) or over the AUTHORITY to enforce a reading against a non-complying state?',
    'Track whether the constructing state''s public justifications shift from legal-textual argument (contesting what the text means) toward pure fait-accompli argument (asserting the fact of occupation regardless of text) over time — a shift would indicate the disagreement has moved from interpretive to purely power-based.',
    'If the disagreement is purely textual, future codification amendments or a definitive ICJ ruling could resolve it entirely. If it is an authority/enforcement dispute, no clarification of the text will change practice absent a change in the constructing state''s cost-benefit calculus (e.g., costly countermeasures or coalition sanctions).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_disagreement_location, conceptual, 'Locating the kernel disagreement between the three readings as textual-interpretive versus enforcement-authority based.').

omega_variable(
    prescriptive_maturation_threshold,
    'Is there a duration or intensity of unchallenged effective control after which even the strict-geographic and hybrid readings would concede the expansive reading has become customary law through acquiescence?',
    'Historical comparative study of prescriptive title doctrine in territorial disputes (e.g. the length of unchallenged occupation courts have treated as legally significant in past ICJ rulings) applied to the specific features under construction.',
    'If such a threshold exists and is approaching or has passed, this reading''s classification may shift toward tangled_rope as its claim gains genuine (if contested) legal traction rather than remaining purely extractive fait accompli; if no such threshold is recognized in maritime law specifically (as opposed to land border disputes), the reading remains snare indefinitely absent countermeasures.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(prescriptive_maturation_threshold, empirical, 'Whether prolonged unchallenged occupation could eventually legitimate this reading via prescriptive title doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__expansive_construction_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(uncl_tr_t4, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 4, 0.34).
narrative_ontology:measurement(uncl_tr_t8, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 8, 0.44).
narrative_ontology:measurement(uncl_tr_t12, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 12, 0.52).
narrative_ontology:measurement(uncl_tr_t16, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 16, 0.58).
narrative_ontology:measurement(uncl_tr_t20, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 20, 0.62).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(uncl_be_t4, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(uncl_be_t8, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(uncl_be_t12, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 12, 0.69).
narrative_ontology:measurement(uncl_be_t16, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 16, 0.75).
narrative_ontology:measurement(uncl_be_t20, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 20, 0.79).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(uncl_su_t4, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 4, 0.5).
narrative_ontology:measurement(uncl_su_t8, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(uncl_su_t12, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 12, 0.64).
narrative_ontology:measurement(uncl_su_t16, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(uncl_su_t20, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 20, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__expansive_construction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unclos_maritime_sovereignty__expansive_construction_reading, 0.05).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, strict_geographic_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, hybrid_effective_control_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the unclos_maritime_sovereignty kernel. strict_geographic_reading holds the codified UNCLOS text (Articles 60(8), 121) as dispositive and treats construction as legally inert — it should classify closer to rope/mountain given its low extraction and tribunal corroboration. hybrid_effective_control_reading occupies a middle position, granting artificial features only limited safety zones with possible slow maturation into fuller claims. This story (expansive_construction_reading) is the most extractive and most actively suppressed of the three, because it must actively defend a claim that both codified text and arbitral authority reject. The three stories share beneficiary/victim structures inverted relative to which reading a given state seat holds, and are linked here so contamination/coupling analysis can trace how instability in one reading's legitimacy propagates to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
