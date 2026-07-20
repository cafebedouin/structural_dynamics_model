% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__strict_eez_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__strict_eez_reading, []).

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
 *   constraint_id: unclos_sovereignty_boundary__strict_eez_reading
 *   human_readable: Strict UNCLOS EEZ Exclusivity Reading (Article 57)
 *   domain: international_law/maritime_geopolitics
 *
 * SUMMARY:
 *   This constraint instantiates the strict_eez_reading of the
 *   unclos_sovereignty_boundary kernel. It treats UNCLOS Article 57 as the
 *   exclusive source of EEZ entitlement and denies validity to overlay claims
 *   such as historical usage or non-ratifier customary exceptions. Coastal
 *   states ratifying UNCLOS gain a legal monopoly over resource extraction
 *   within 200 nautical miles, while overlapping claimants and historical
 *   users are structurally excluded. The arrangement presents itself as a
 *   neutral legal coordination mechanism, but its strict reading operates as
 *   enforceable resource capture backed by naval and coast-guard suppression
 *   of alternative sovereignty frameworks.
 *
 * KEY AGENTS:
 *   - ratifier_coastal_states (institutional/arbitrage): Primary agenda-setters and beneficiaries â administer EEZ enforcement and capture resource rents
 *   - overlapping_claimants (powerful/trapped): Primary payers â lose access to contested waters under strict EEZ enforcement
 *   - non_ratifier_coastal_states (powerful/constrained): Secondary payers â subjected to EEZ claims without treaty protections
 *   - historical_rights_holders (powerless/trapped): Tertiary payers â traditional maritime communities excluded from historical fishing grounds
 *   - unclos_dispute_settlement (institutional/analytical): Observer seat â interprets and adjudicates EEZ delimitation but lacks direct enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__strict_eez_reading, 0.72).
domain_priors:suppression_score(unclos_sovereignty_boundary__strict_eez_reading, 0.78).
domain_priors:theater_ratio(unclos_sovereignty_boundary__strict_eez_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__strict_eez_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__strict_eez_reading, "Strict UNCLOS EEZ Exclusivity Reading (Article 57)").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__strict_eez_reading, "international_law/maritime_geopolitics").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__strict_eez_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__strict_eez_reading, '898f6168-eae0-45fb-9507-acb225184475').
narrative_ontology:cs_kernel_codification('898f6168-eae0-45fb-9507-acb225184475', formalized).
narrative_ontology:cs_authority_grounding('898f6168-eae0-45fb-9507-acb225184475', lineage).
narrative_ontology:cs_interpretation_layer_present('898f6168-eae0-45fb-9507-acb225184475').
narrative_ontology:cs_reading_relation('898f6168-eae0-45fb-9507-acb225184475', unclos_sovereignty_boundary__historical_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('898f6168-eae0-45fb-9507-acb225184475', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coexists_with).
narrative_ontology:cs_axiom('898f6168-eae0-45fb-9507-acb225184475', foundational, eez_exclusivity_overrides_historical_usage).
narrative_ontology:cs_axiom_status(eez_exclusivity_overrides_historical_usage, holdable).
narrative_ontology:cs_axiom_grounding('898f6168-eae0-45fb-9507-acb225184475', eez_exclusivity_overrides_historical_usage, conventional).
narrative_ontology:cs_axiom('898f6168-eae0-45fb-9507-acb225184475', foundational, ratification_as_threshold_for_enforceable_eez).
narrative_ontology:cs_axiom_status(ratification_as_threshold_for_enforceable_eez, holdable).
narrative_ontology:cs_axiom_grounding('898f6168-eae0-45fb-9507-acb225184475', ratification_as_threshold_for_enforceable_eez, conventional).
narrative_ontology:cs_reference_frame('898f6168-eae0-45fb-9507-acb225184475', unclos_positivist_delimitation).
narrative_ontology:cs_drift_state('898f6168-eae0-45fb-9507-acb225184475', post_south_china_sea_arbitration, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('898f6168-eae0-45fb-9507-acb225184475', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, ratifier_coastal_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, overlapping_claimants).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, non_ratifier_coastal_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, historical_rights_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ratified UNCLOS and assert exclusive sovereign rights over fisheries, hydrocarbons, and seabed minerals within 200 nautical miles. Administer enforcement through coast guards and naval patrols, license foreign fishing and extraction, and litigate boundary delimitation before UNCLOS dispute settlement bodies. Collect resource rents directly through licensing and state-owned enterprise concessions.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, ratifier_coastal_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__strict_eez_reading, ratifier_coastal_states, beneficiary).

% Hold maritime claims that overlap with ratifier states' EEZ boundaries, often based on historical usage, proximity, or alternative delimitation principles. Lose legal access to contested fisheries and hydrocarbon deposits when the strict EEZ reading is enforced. Unable to exit the dispute without ceding territorial assertions, which carries domestic political and nationalist costs.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, overlapping_claimants, payer,
    powerful, generational, trapped, regional).

% Possess coastlines and maritime interests but never ratified or acceded to UNCLOS. Subjected to EEZ claims by neighboring ratifiers while denied the treaty's formal dispute-settlement protections. May assert customary law or naval presence as countermeasures, but lack the institutional leverage to challenge licensed exclusions within 200-nautical-mile zones.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, non_ratifier_coastal_states, payer,
    powerful, generational, constrained, global).

% Coastal and island communities whose pre-UNCLOS traditional fishing grounds and navigational usage patterns cross modern EEZ boundaries. Excluded from waters they historically accessed by coastal state licensing regimes and enforcement patrols. Lack legal standing in interstate dispute settlement and possess no institutional pathway to reclaim usage rights.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, historical_rights_holders, payer,
    powerless, biographical, trapped, regional).

% ITLOS, Annex VII arbitral tribunals, and the CLCS that interpret UNCLOS provisions and delimit maritime boundaries. They produce binding or persuasive awards on EEZ entitlement, but lack direct enforcement power and depend on state compliance. Their legitimacy rests on treaty text and consensual jurisdiction.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, unclos_dispute_settlement, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_sovereignty_boundary__strict_eez_reading, ratifier_coastal_states).
narrative_ontology:fixing_cost_class(unclos_sovereignty_boundary__strict_eez_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform, text-based method for allocating exclusive coastal state rights over marine resources, replacing ad hoc historical and bilateral claims with a single distance-based rule applicable to all coastlines.
% TRANSFER_FUNCTION: Transfers exclusive exploitation rights over fisheries, hydrocarbons, and seabed minerals from the international commons, overlapping neighbors, and historical users to the coastal state within a 200-nautical-mile envelope measured from baselines.
% ABSENT_VOICES: Traditional maritime communities whose usage predates Westphalian statehood; indigenous sea peoples without UN representation; pre-colonial polities whose historical maps and voyaging routes are not recognized as legal title under the convention; and non-state actors who would advocate for open-ocean commons access.
% DISAPPEARANCE_RATIONALE: Without the strict EEZ exclusivity, coastal states would lose their legal monopoly over 200nm resource zones. Overlapping claimants would activate historical rights and proximity arguments; resource licensing systems would collapse into contested multilateral scrambles; and the global maritime order would revert toward bilateral power-balancing rather than treaty-based allocation.
% FOUNDING_PROBLEM: Pre-UNCLOS maritime jurisdiction was fragmented and uncertain: unlimited freedom to fish and extract led to overcapitalization, Tragedy-of-the-Commons pressures, and frequent naval incidents due to overlapping bilateral claims.
% FOUNDING_PROBLEM_CORROBORATION: UNCLOS negotiators and the International Seabed Authority attest that unregulated high-seas exploitation threatened resource stocks. Critics from non-ratifier naval powers and historical-rights traditions attest that the 200nm solution was a political compromise favoring geographically advantaged states, not the only available coordination mechanism; independent legal historians note that bilateral treaties and regional fisheries bodies had already begun addressing the problem before UNCLOS entered into force.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__strict_eez_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__strict_eez_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__strict_eez_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unclos_sovereignty_boundary__strict_eez_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__strict_eez_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__strict_eez_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__strict_eez_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__strict_eez_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the 200nm exclusivity transfers vast marine resource rents to coastal states while marginalizing historical and overlapping users. Suppression (0.78) is higher than extraction because the constraint's persistence depends on active naval and legal suppression of overlay claims, not on consent from excluded parties. Theater ratio (0.28) is moderate-low: much enforcement is functionally real, but a growing share involves performative patrols and legal posturing that serves sovereignty signaling more than resource protection. Accessibility collapse (0.75) is high because, once UNCLOS ratification is accepted, alternative delimitation frameworks collapse for member states. Resistance (0.60) reflects ongoing non-compliance by major historical-rights claimants and non-ratifier naval assertions.
 *
 * PERSPECTIVAL GAP:
 *   From the ratifier coastal state seat, the strict EEZ reading is a rule-of-law achievement that replaced maritime anarchy with predictable boundaries. From the overlapping claimant and historical rights seats, the same structure is experienced as enclosure of the commons and dispossession of pre-existing usage. The engine computes this divergence from the structural data: identical legal text produces opposed classifications depending on whether the seat is inside or outside the 200nm envelope.
 *
 * DIRECTIONALITY LOGIC:
 *   Ratifier coastal states are the structural beneficiaries and agenda-setters: they write the licensing rules, enforce boundaries, and collect resource rents, placing them near the beneficiary end of directionality. Overlapping claimants and non-ratifier states are targets: they bear the cost of exclusion from waters they contest or use, with trapped or constrained exit options, placing them near the full-target end. Historical rights holders sit at the extreme target end due to powerlessness and identity-locked dependence on traditional waters. The UNCLOS dispute settlement bodies are analytical observers with no extraction or payment stake.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as Tangled Rope prevents mislabeling the constraint as pure coordination (Rope) because the victim set is non-empty and the extraction is asymmetric: coastal states capture resource rents that do not flow back to excluded parties. It prevents mislabeling as Snare because the coordination function is genuine and not merely cover: uniform distance-based delimitation did reduce interstate friction and overfishing relative to the pre-UNCLOS era. The Tangled Rope capture registers the coexistence of real coordination and real extraction within the same institutional structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_rights_legal_status,
    'Do historical usage patterns and pre-UNCLOS occupation constitute independent sovereign rights that the strict Article 57 reading improperly extinguishes?',
    'Comparative case-law analysis across ITLOS, PCIJ, and municipal decisions in littoral states recognizing indigenous or historical maritime title.',
    'If historical rights survive as independent legal entitlements, the strict EEZ reading becomes a Snare rather than a Tangled Rope, because its coordination function would serve as cover for dispossession.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_rights_legal_status, conceptual, 'Whether historical rights are legally extinguished by UNCLOS EEZ provisions.').

omega_variable(
    non_ratifier_bindingness_gap,
    'To what extent does the strict UNCLOS reading bind non-party states, and does customary international law replicate or diverge from the 200nm exclusivity rule?',
    'State-practice surveys and opinio juris analysis examining whether non-ratifiers'' EEZ claims and enforcement behaviors match UNCLOS Article 57 or follow narrower customary limits.',
    'If non-ratifiers are not bound but are still subjected to EEZ enforcement by ratifiers, the suppression metric understates the coercion involved and the victim set is larger than the treaty framework suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_ratifier_bindingness_gap, empirical, 'Customary law status of strict EEZ exclusivity for non-parties.').

omega_variable(
    suppression_mechanism_naval_vs_legal,
    'Is the suppression of alternative sovereignty frameworks achieved primarily through UNCLOS legal institutions or through bilateral naval deterrence and coast-guard enforcement?',
    'Quantitative comparison of boundary delimitation cases resolved by courts versus unresolved disputes maintained by patrol presence and interdiction.',
    'If naval deterrence dominates, the constraint''s suppression is raw power projection dressed in legal form, raising theater_ratio and shifting computed classification toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_naval_vs_legal, empirical, 'Legal versus naval suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__strict_eez_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(uncl_tr_t8, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(uncl_tr_t16, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(uncl_tr_t24, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(uncl_tr_t32, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 32, 0.25).
narrative_ontology:measurement(uncl_tr_t40, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(uncl_be_t8, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(uncl_be_t16, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(uncl_be_t24, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(uncl_be_t32, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(uncl_be_t40, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(uncl_su_t8, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(uncl_su_t16, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(uncl_su_t24, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(uncl_su_t32, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 32, 0.75).
narrative_ontology:measurement(uncl_su_t40, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__strict_eez_reading, resource_allocation).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, historical_rights_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, non_ratifier_enforcement_reading).

% DUAL FORMULATION NOTE:
% This constraint is the formalized UNCLOS positivist reading of the maritime sovereignty boundary kernel. Sibling readings assign authority to pre-existing historical usage or to customary naval enforcement independent of treaty ratification. The strict reading's high extractiveness derives from its foreclosure of overlay claims, while its coordination function derives from uniform distance-based delimitation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
