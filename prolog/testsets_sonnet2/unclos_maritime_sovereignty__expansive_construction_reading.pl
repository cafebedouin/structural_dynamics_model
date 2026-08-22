% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__expansive_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Expansive Construction Reading of Maritime Sovereignty (Artificial Islands Generate Territorial Waters)
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This story instantiates the expansive construction reading of the
 *   maritime sovereignty kernel: the claim that dredging submerged reefs and
 *   low-tide elevations into artificial islands, then garrisoning and
 *   administering them, generates de facto territorial waters (12nm
 *   territorial sea or broader) through effective occupation regardless of
 *   the feature's pre-construction legal status. This is a specific,
 *   contested claim about how sovereignty attaches to constructed land, not a
 *   description of UNCLOS as a whole. Two sibling constraints exist: the
 *   strict_geographic_reading (only naturally formed high-tide features
 *   generate territorial sea; construction changes nothing) and the
 *   hybrid_effective_control_reading (artificial features get only a 500m
 *   safety zone but may mature into territorial claims through prolonged
 *   uncontested control). Each reading has its own ε, its own
 *   beneficiary/victim structure, and its own classification — this file
 *   authors only the expansive_construction_reading.
 *
 * KEY AGENTS:
 *   - island_constructing_state: agenda_setter/beneficiary (institutional/arbitrage) — builds, garrisons, administers, and collects the resulting maritime resource and strategic value
 *   - neighboring_claimant_states: payer (moderate/constrained) — lose overlapping EEZ and continental shelf entitlements to the expansive claim
 *   - freedom_of_navigation_states: payer (powerful/constrained) — bear ongoing operational and diplomatic cost of contesting the claim in practice
 *   - regional_fishing_communities: payer (powerless/trapped) — lose traditional fishing access and face interception
 *   - unclos_arbitral_tribunals: excluded (institutional/analytical) — ruled against the expansive reading but has no enforcement pathway
 *   - maritime_law_scholars: observer (analytical/analytical) — document the codified-text-vs-operational-reality gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__expansive_construction_reading, 0.79).
domain_priors:suppression_score(unclos_maritime_sovereignty__expansive_construction_reading, 0.71).
domain_priors:theater_ratio(unclos_maritime_sovereignty__expansive_construction_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__expansive_construction_reading, snare).
narrative_ontology:human_readable(unclos_maritime_sovereignty__expansive_construction_reading, "Expansive Construction Reading of Maritime Sovereignty (Artificial Islands Generate Territorial Waters)").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__expansive_construction_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__expansive_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__expansive_construction_reading, '2060b061-8685-438d-91d3-8658330e40b9').
narrative_ontology:cs_kernel_codification('2060b061-8685-438d-91d3-8658330e40b9', fixed_text).
narrative_ontology:cs_authority_grounding('2060b061-8685-438d-91d3-8658330e40b9', extraction).
narrative_ontology:cs_interpretation_layer_present('2060b061-8685-438d-91d3-8658330e40b9').
narrative_ontology:cs_reading_relation('2060b061-8685-438d-91d3-8658330e40b9', unclos_maritime_sovereignty__strict_geographic_reading, forecloses).
narrative_ontology:cs_reading_relation('2060b061-8685-438d-91d3-8658330e40b9', unclos_maritime_sovereignty__hybrid_effective_control_reading, coexists_with).
narrative_ontology:cs_axiom('2060b061-8685-438d-91d3-8658330e40b9', foundational, effective_occupation_generates_entitlement).
narrative_ontology:cs_axiom_status(effective_occupation_generates_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('2060b061-8685-438d-91d3-8658330e40b9', effective_occupation_generates_entitlement, conventional).
narrative_ontology:cs_axiom('2060b061-8685-438d-91d3-8658330e40b9', secondary, administrative_control_substitutes_for_natural_formation).
narrative_ontology:cs_axiom_status(administrative_control_substitutes_for_natural_formation, holdable).
narrative_ontology:cs_axiom_grounding('2060b061-8685-438d-91d3-8658330e40b9', administrative_control_substitutes_for_natural_formation, instrumental).
narrative_ontology:cs_reference_frame('2060b061-8685-438d-91d3-8658330e40b9', unclos_natural_formation_baseline).
narrative_ontology:cs_drift_state('2060b061-8685-438d-91d3-8658330e40b9', post_2016_arbitration_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('2060b061-8685-438d-91d3-8658330e40b9', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_state).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, neighboring_claimant_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, regional_fishing_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dredges submerged reefs and low-tide elevations into artificial islands, garrisons them, builds runways and administrative facilities, and then asserts that the resulting features generate a 12nm territorial sea and associated maritime entitlements. Enforces the claim with naval and coast guard patrols, exclusion of foreign vessels, and administrative acts (permits, resource licensing) styled as sovereign governance. Collects fishing, resource, and strategic-basing value from waters that would otherwise be high seas or another state's EEZ.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_state, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_state, beneficiary).

% Hold overlapping or prior claims to the same reefs and surrounding waters under conventional EEZ/continental-shelf reasoning. Watch their fishing grounds, hydrocarbon exploration blocks, and transit routes absorbed into a rival's claimed territorial sea. Their options are diplomatic protest, arbitration (with uncertain enforcement), or costly naval posturing they mostly cannot sustain against a more powerful neighbor.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, neighboring_claimant_states, payer,
    moderate, generational, constrained, regional).

% Rely on unimpeded transit through the contested waters for commercial shipping and naval movement. Conduct freedom-of-navigation operations to contest the expansive reading in practice, incurring diplomatic friction and escalation risk each time. Cannot simply exit the sea lanes without absorbing major rerouting costs, so they bear the ongoing cost of contesting a claim they do not recognize.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_states, payer,
    powerful, biographical, constrained, global).

% Small-scale and commercial fishers from multiple coastal states who traditionally worked the reef waters now patrolled and licensed by the constructing state. Face seizure of boats, fines, or violent interception if they fish grounds now claimed as territorial waters. Have essentially no individual leverage and depend on their home governments' diplomatic capacity, which is often outmatched.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, regional_fishing_communities, payer,
    powerless, biographical, trapped, local).

% International tribunals (e.g., under UNCLOS Annex VII) have ruled that artificial construction on submerged features or rocks does not upgrade their legal status. The constructing state does not recognize tribunal jurisdiction over the dispute and does not comply with adverse rulings, so the tribunal's determination exists as a legal fact with no enforcement pathway into the constraint's actual operation.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, unclos_arbitral_tribunals, excluded,
    institutional, civilizational, analytical, global).

% Study the gap between codified UNCLOS text (Article 60, Article 121) and the operational reality of effective occupation. Document the divergence between the strict geographic reading and the expansive construction reading playing out on the water, without power to compel either party.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, maritime_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In principle, clear rules for what generates territorial sea would coordinate expectations among all maritime states about where sovereign jurisdiction begins and high seas freedoms apply, reducing costly disputes over ambiguous features.
% TRANSFER_FUNCTION: Moves fishing rights, hydrocarbon and mineral access, strategic basing value, and transit control from neighboring claimant states, freedom-of-navigation states, and local fishing communities to the island-constructing state, via unilateral construction plus administrative and military enforcement rather than negotiated boundary agreement.
% ABSENT_VOICES: Regional fishing communities have essentially no seat in the diplomatic or legal processes shaping the claim; smaller neighboring states often lack the naval or legal capacity to be heard as equals; the arbitral tribunal's ruling exists but the constructing state excludes it from the operative process by refusing jurisdiction and non-compliance.
% DISAPPEARANCE_RATIONALE: If the expansive construction reading were abandoned and the strict geographic reading enforced instead, the constructed features would revert to generating at most a 500m safety zone (or nothing), returning the surrounding waters to shared high seas, neighboring EEZs, or contested-but-unresolved status. Naval patrol patterns, fishing access, resource licensing, and freedom-of-navigation operations would all reorganize around the narrower entitlement.
% FOUNDING_PROBLEM: UNCLOS Article 121 and Article 60 were built to resolve genuine ambiguity about what physical features (natural islands vs. rocks vs. artificial installations) generate what maritime zones, so states would not need to fight over every reef and shoal.
% FOUNDING_PROBLEM_CORROBORATION: The constructing state attests the founding problem (unclear administrative and security jurisdiction over remote features) remains live and its construction program is a legitimate exercise of sovereignty. Neighboring states, the 2016 South China Sea Arbitration tribunal (an institution outside the constructing state's benefit set), and independent international-law scholarship attest that the text's plain answer (artificial construction does not upgrade a feature's status) was already settled and that persistence of the expansive claim past that ruling is annexation dressed as interpretation, not genuine unresolved ambiguity.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__expansive_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__expansive_construction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__expansive_construction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored high (0.79 at interval end) because the reading transfers fishing, hydrocarbon, and strategic value away from neighboring states and the high-seas commons to a single constructing state, on the strength of unilateral construction rather than negotiated or adjudicated entitlement — and that transfer has grown over the interval as more features were built out and garrisoned. Suppression is high (0.71) because the claim's persistence depends on active naval/coast-guard interdiction of rival claimants' vessels and refusal to submit to or comply with international arbitration. Theater ratio is elevated (0.58) because much of the administrative apparatus (permits, local government designations, civilian settlement) functions primarily to perform sovereignty for legal and propaganda purposes rather than to deliver governance services that could not otherwise be delivered from the mainland. All three series share one time grid (T=0 to T=20, six points), consistent with the alignment rule.
 *
 * PERSPECTIVAL GAP:
 *   From the constructing state's seat, this reads as legitimate exercise of sovereign development rights over its own claimed territory — construction is treated as consolidating a pre-existing entitlement, not creating one from nothing. From the neighboring claimant, freedom-of-navigation, and fishing-community seats, the identical structure reads as unilateral annexation enforced through administrative theater and naval coercion. The engine computes these as different seat-level classifications from the same structural data; the divergence is not an error to reconcile but the object of measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   The island-constructing state is the clear structural beneficiary: it sets the rules, administers the space, and captures the resulting resource and strategic value, so its derived directionality sits near the full-beneficiary end. Neighboring claimant states and freedom-of-navigation states are declared victims bearing a direct transfer of maritime entitlement and operational cost, pushing their directionality toward the full-target end — freedom-of-navigation states somewhat less trapped than neighboring claimants because they retain global-scale leverage and alternative routing, however costly. Regional fishing communities are the most target-locked: powerless, trapped, and bearing the most concrete, immediate cost (seizure, exclusion) with no meaningful exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — genuine ambiguity about what physical features generate what maritime zones — was substantially resolved by the 2016 South China Sea Arbitration's plain reading of Article 121 (artificial construction does not upgrade a feature's legal status). The expansive construction reading's continued operation past that point is a mismatch case: founding_problem_status is authored contested (constructing state says live; outside corroborators say resolved) while disappearance_verdict is world_rearranges (real arrangements — patrols, licensing, fishing access — depend on it). That mismatch is exactly the signal the R5 consumer is built to catch: a reading that keeps operating as if the underlying ambiguity were unresolved, after an outside authority found otherwise, is functioning as extraction dressed as ongoing coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    construction_vs_natural_feature_status_ambiguity,
    'Does effective occupation and administrative control of a constructed feature create genuine legal entitlement over time (an evolving customary-law claim), or is it permanently barred by Article 60''s plain text regardless of duration or lack of challenge?',
    'Track whether other states'' persistent, unchallenged non-response to the constructing state''s administrative acts crystallizes into a customary international law claim, versus whether repeated arbitral and diplomatic rejection (protest, non-recognition, FONOPs) keeps the claim permanently unripened. Decades-long state practice and opinio juris data would be dispositive.',
    'If effective occupation can mature into entitlement absent sustained challenge, the expansive reading gains real long-run legal traction and this constraint drifts toward a genuinely contested hybrid; if not, the reading remains a pure assertion sustained only by unilateral force, closer to snare with no coordination residue at all.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(construction_vs_natural_feature_status_ambiguity, conceptual, 'Whether construction-based effective control can ripen into genuine sovereignty absent challenge, or is permanently foreclosed by treaty text.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the expansive_construction_reading better understood as a distinct legal theory some states genuinely hold, or as a strategic post-hoc justification adopted only after construction was already underway for other (military/resource) reasons?',
    'Compare the constructing state''s public legal position over time against the sequencing of construction and legal argumentation — did the effective-occupation theory precede or follow the physical construction program?',
    'If the reading is genuinely held prior to construction, it functions as an interpretive commitment with its own internal logic (closer to a live doctrinal dispute); if adopted only after the fact to justify a fait accompli, the ''reading'' is better understood as rationalization layered onto pure extraction, which would push suppression and extractiveness even higher than authored here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the expansive reading is a genuine prior legal commitment or a retrofitted justification for construction already undertaken.').

omega_variable(
    enforcement_durability_ambiguity,
    'Does the constructing state''s enforcement capacity (naval presence, coast guard patrols, administrative control) remain durable enough over the coming decades to sustain the expansive claim, or does it erode as rival capability and coalition-based freedom-of-navigation pressure grows?',
    'Track relative naval capability trends, frequency and intensity of FONOPs, and whether any negotiated resolution or multilateral non-recognition regime emerges.',
    'If enforcement capacity holds or grows, the constraint stabilizes as a durable snare; if it erodes under external pressure, the constraint may drift toward scaffold (a temporary assertion eventually rolled back) or toward tangled_rope if a negotiated joint-development compromise emerges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_durability_ambiguity, empirical, 'Whether the enforcement basis for the expansive claim is durable or subject to erosion under external pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__expansive_construction_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(uncl_tr_t4, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 4, 0.36).
narrative_ontology:measurement(uncl_tr_t8, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 8, 0.42).
narrative_ontology:measurement(uncl_tr_t12, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 12, 0.48).
narrative_ontology:measurement(uncl_tr_t16, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 16, 0.54).
narrative_ontology:measurement(uncl_tr_t20, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(uncl_be_t4, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(uncl_be_t8, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(uncl_be_t12, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 12, 0.68).
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
narrative_ontology:boltzmann_floor_override(unclos_maritime_sovereignty__expansive_construction_reading, 0.1).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, strict_geographic_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, hybrid_effective_control_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the unclos_maritime_sovereignty kernel. strict_geographic_reading (tribunal-endorsed, low ε, closer to rope/mountain from the perspective of treaty text) and hybrid_effective_control_reading (moderate ε, time-erosion compromise between the two poles) are separate files. The expansive_construction_reading carries the highest authored ε of the three because it authorizes the broadest transfer (full territorial sea and associated entitlements) from the narrowest triggering fact (mere construction plus administrative control). Each reading shares the same underlying kernel text (UNCLOS Articles 60 and 121) but diverges entirely in what triggering facts generate what maritime zones — the disagreement is located precisely at the legal significance of artificial construction, which is why they are authored as three separate constraints rather than one constraint with a contested value.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
