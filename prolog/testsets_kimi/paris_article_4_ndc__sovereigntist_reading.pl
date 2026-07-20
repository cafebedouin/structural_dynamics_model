% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__sovereigntist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__sovereigntist_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: paris_article_4_ndc__sovereigntist_reading
 *   human_readable: Paris Agreement Article 4 NDC Regime â Sovereigntist Reading
 *   domain: international_climate_governance
 *
 * SUMMARY:
 *   This constraint instantiates the sovereigntist reading of the Paris
 *   Agreement Article 4 NDC kernel: a regime of voluntary, self-determined
 *   national pledges that preserves Westphalian energy sovereignty. Under
 *   this reading, the NDC system functions as a low-extraction coordination
 *   mechanism enabling broad state participation by foreclosing binding
 *   enforcement. The structural beneficiaries are fossil-dependent and
 *   rapidly industrializing economies that retain development flexibility.
 *   Climate-vulnerable states and non-state actors bear the diffuse
 *   consequences of weak ambition but are not the targets of extractive
 *   transfer by the constraint itself. The sibling supranational reading
 *   (binding ratcheting net-zero with international accountability) is
 *   foreclosed by this interpretation, while the equity reading
 *   (CBDR-structured differentiation) coexists as a live but non-binding
 *   normative frame.
 *
 * KEY AGENTS:
 *   - state_parties: Agenda-setter (institutional/mobile) â draft and revise NDCs under Article 4
 *   - fossil_fuel_dependent_economies: Beneficiary (powerful/mobile) â preserve fossil production and avoid binding caps
 *   - rapidly_industrializing_economies: Beneficiary (moderate/mobile) â retain policy space for industrial energy growth
 *   - climate_vulnerable_states: Observer (moderate/constrained) â participate but lack power to alter voluntarist architecture
 *   - non_state_climate_coalitions: Excluded (organized/constrained) â advocate binding targets but lack seat at sovereign table
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__sovereigntist_reading, 0.2).
domain_priors:suppression_score(paris_article_4_ndc__sovereigntist_reading, 0.05).
domain_priors:theater_ratio(paris_article_4_ndc__sovereigntist_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__sovereigntist_reading, rope).
narrative_ontology:human_readable(paris_article_4_ndc__sovereigntist_reading, "Paris Agreement Article 4 NDC Regime â Sovereigntist Reading").
narrative_ontology:topic_domain(paris_article_4_ndc__sovereigntist_reading, "international_climate_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__sovereigntist_reading, '9b08f0a2-80ec-4db4-bfb1-0097460f7cc2').
narrative_ontology:cs_kernel_codification('9b08f0a2-80ec-4db4-bfb1-0097460f7cc2', fixed_text).
narrative_ontology:cs_authority_grounding('9b08f0a2-80ec-4db4-bfb1-0097460f7cc2', lineage).
narrative_ontology:cs_interpretation_layer_present('9b08f0a2-80ec-4db4-bfb1-0097460f7cc2').
narrative_ontology:cs_reading_relation('9b08f0a2-80ec-4db4-bfb1-0097460f7cc2', paris_article_4_ndc__supranational_reading, forecloses).
narrative_ontology:cs_reading_relation('9b08f0a2-80ec-4db4-bfb1-0097460f7cc2', paris_article_4_ndc__equity_reading, coexists_with).
narrative_ontology:cs_axiom('9b08f0a2-80ec-4db4-bfb1-0097460f7cc2', foundational, state_sovereignty_over_energy_policy).
narrative_ontology:cs_axiom_status(state_sovereignty_over_energy_policy, holdable).
narrative_ontology:cs_axiom_grounding('9b08f0a2-80ec-4db4-bfb1-0097460f7cc2', state_sovereignty_over_energy_policy, conventional).
narrative_ontology:cs_axiom('9b08f0a2-80ec-4db4-bfb1-0097460f7cc2', foundational, voluntary_pledge_non_binding).
narrative_ontology:cs_axiom_status(voluntary_pledge_non_binding, holdable).
narrative_ontology:cs_axiom_grounding('9b08f0a2-80ec-4db4-bfb1-0097460f7cc2', voluntary_pledge_non_binding, conventional).
narrative_ontology:cs_reference_frame('9b08f0a2-80ec-4db4-bfb1-0097460f7cc2', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('9b08f0a2-80ec-4db4-bfb1-0097460f7cc2', contemporary_cop_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('9b08f0a2-80ec-4db4-bfb1-0097460f7cc2', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, fossil_fuel_dependent_economies).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, rapidly_industrializing_economies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft, submit, and revise their own NDCs under Article 4; control the scope and ambition of pledges without external override; may withdraw from the Agreement entirely.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, state_parties, agenda_setter,
    institutional, generational, mobile, national).

% Retain domestic fossil fuel production and export capacity; avoid binding caps that would strand reserves; use voluntarism to defer energy transition.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, fossil_fuel_dependent_economies, beneficiary,
    powerful, generational, mobile, national).

% Preserve policy space to increase energy consumption during industrial catch-up; resist externally imposed emissions trajectories that mirror developed-state pathways.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, rapidly_industrializing_economies, beneficiary,
    moderate, generational, mobile, national).

% Participate in negotiations but lack structural power to alter the voluntarist architecture; depend on the same NDC registry for transparency but bear the physical consequences of weak collective ambition.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, climate_vulnerable_states, observer,
    moderate, generational, constrained, national).

% Advocate for binding targets and higher ambition but are not seated at the sovereign-state decision table where the voluntarist frame is maintained; their preferred accountability mechanisms are structurally excluded.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, non_state_climate_coalitions, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables information-sharing and mutual signaling among sovereign states about intended climate actions without surrendering national policy autonomy or requiring binding harmonization.
% TRANSFER_FUNCTION: Moves information, reputational stakes, and transparency expectations from individual states to the international registry; no direct resource extraction or binding obligation transfer.
% ABSENT_VOICES: Future generations and non-state actors demanding binding mitigation are structurally sidelined in a state-sovereignty framework; they would object to the atrophy of enforcement but are not seated at the sovereign decision table.
% DISAPPEARANCE_RATIONALE: The disappearance of the NDC registry and transparency framework would eliminate the primary venue for sovereign climate pledges, forcing states to revert to fragmented unilateralism or negotiate an entirely new coordination architecture.
% FOUNDING_PROBLEM: The need for a globally inclusive climate coordination mechanism that could secure participation from all major emitters, including those that would reject binding emissions caps or supranational enforcement.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars and diplomatic historians outside the fossil-dependent bloc attest that the Paris architecture was designed explicitly to avoid the Kyoto Protocol's binding-target model, which failed to secure US and emerging-economy participation; independent treaty-law scholarship corroborates the sovereignty-preserving design intent.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__sovereigntist_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__sovereigntist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__sovereigntist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(paris_article_4_ndc__sovereigntist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__sovereigntist_reading, 0.2, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__sovereigntist_reading_tests).
:- end_tests(paris_article_4_ndc__sovereigntist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.20) because the constraint does not compel resource transfer or regulatory surrender; it operates through transparency and reputational signaling. Suppression is minimal (0.05) because exit is legally preserved (Article 28 withdrawal, continuous revision). Theater ratio is elevated (0.55) because the gap between pledged ambition and implemented action has widened over the interval, turning the NDC registry into a performance of climate action rather than a binding operational mechanism. Accessibility collapse is low (0.20): alternatives (binding multilateral treaties, unilateral action, subnational carbon markets) remain open. Resistance is moderate-low (0.25): climate-vulnerable states and civil society resist the voluntarist frame, but their resistance is channeled into COP rhetoric rather than structural override.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of fossil-dependent economies, the constraint is a rope preserving necessary policy flexibility; from the seat of climate-vulnerable states, the same structure appears as a coordination failure that externalizes climate risk. The engine will compute divergent per-seat classifications: the powerful beneficiary seat sees low effective extraction, while the constrained observer seat experiences higher effective harm (though not extraction by the constraint itself, but by the collective action failure it enables). The sovereigntist reading authors the claim as rope because the constraint itself does not coerce; the harm arises from what the constraint fails to prevent, not from what it enforces.
 *
 * DIRECTIONALITY LOGIC:
 *   State parties are agenda-setters with mobile exit (d near beneficiary end). Fossil-dependent and industrializing economies are explicit beneficiaries (low d). Climate-vulnerable states are observers with constrained exit: they are structurally positioned between beneficiary and target â they participate in the regime but suffer from its inadequacy. However, because the constraint does not actively extract from them (it merely fails to protect them), they are not declared victims. Non-state coalitions are excluded (high d if considered, but they are not governed by the constraint). No directionality overrides are needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as rope prevents mislabeling the NDC regime as snare: there is no identifiable coercive extraction, no trapped population from whom rents are taken, and no suppression of alternatives. The risk of mislabeling is to call it a snare because of its weak environmental outcomes; mandatrophy analysis checks whether the constraint has outlived its function. Here, the founding problem (broad participation without binding caps) remains live, so mandatrophy is not declared. The elevated theater ratio signals Goodhart drift (pledges substituting for action), which could eventually trigger scaffold or piton dynamics if the coordination function atrophies completely, but not yet.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereigntist_extraction_ambiguity,
    'Does the sovereigntist reading describe a genuine coordination mechanism, or does it function as a constructed veil allowing major emitters to avoid binding climate obligations?',
    'Comparative analysis of NDC ambition relative to binding alternative baselines; if voluntarism systematically produces weaker targets than would emerge under binding rules, the sovereigntist framing is extractive cover.',
    'Would reclassify from rope to tangled_rope or snare if voluntarism is shown to serve concentrated beneficiary interests in fossil-dependent economies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereigntist_extraction_ambiguity, conceptual, 'Whether voluntarism is genuine coordination or cover for extraction').

omega_variable(
    supranational_feasibility,
    'Is the Paris Agreement''s kernel structurally capable of supporting the supranational reading, or does the text inherently foreclose binding enforcement?',
    'Treaty interpretation by authoritative international legal bodies; amendment of Article 4; or state practice establishing binding customary norms.',
    'If the kernel inherently forecloses binding enforcement, the supranational reading is legally impossible and the sovereigntist reading is the only operable interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supranational_feasibility, conceptual, 'Whether the treaty kernel can structurally support binding commitments').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__sovereigntist_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t0, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(pari_tr_t2, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 2, 0.35).
narrative_ontology:measurement(pari_tr_t4, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 4, 0.4).
narrative_ontology:measurement(pari_tr_t6, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 6, 0.45).
narrative_ontology:measurement(pari_tr_t8, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 8, 0.5).
narrative_ontology:measurement(pari_tr_t10, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(pari_be_t0, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(pari_be_t2, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 2, 0.12).
narrative_ontology:measurement(pari_be_t4, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 4, 0.14).
narrative_ontology:measurement(pari_be_t6, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 6, 0.16).
narrative_ontology:measurement(pari_be_t8, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 8, 0.18).
narrative_ontology:measurement(pari_be_t10, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 10, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(pari_su_t0, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(pari_su_t2, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 2, 0.13).
narrative_ontology:measurement(pari_su_t4, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 4, 0.11).
narrative_ontology:measurement(pari_su_t6, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 6, 0.09).
narrative_ontology:measurement(pari_su_t8, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 8, 0.07).
narrative_ontology:measurement(pari_su_t10, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 10, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
