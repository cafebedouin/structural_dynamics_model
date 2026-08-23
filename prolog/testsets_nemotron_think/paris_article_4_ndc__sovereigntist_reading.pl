% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__sovereigntist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: paris_article_4_ndc__sovereigntist_reading
 *   human_readable: Paris Article 4 NDCs as Voluntary Sovereign Pledges
 *   domain: international_climate_governance
 *
 * SUMMARY:
 *   The Paris Agreement's Article 4 establishes Nationally Determined
 *   Contributions (NDCs) as the core mitigation mechanism. The sovereigntist
 *   reading interprets NDCs as voluntary, self-determined pledges that
 *   preserve national energy sovereignty — states choose their own targets,
 *   timelines, and policies without international prescription. This reading
 *   emphasizes Article 4.11 (right to adjust NDCs) and the absence of
 *   enforcement mechanisms as deliberate design features, not bugs. The
 *   constraint operates as a coordination mechanism: universal transparency
 *   and periodic stocktaking replace binding targets. Extraction is low
 *   because no state is compelled to act against its perceived interest;
 *   suppression is minimal because exit (withdrawal or low ambition) is
 *   always available. Theater ratio is modest but rising as the gap between
 *   voluntary pledges and physical necessity becomes more visible.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__sovereigntist_reading, 0.18).
domain_priors:suppression_score(paris_article_4_ndc__sovereigntist_reading, 0.12).
domain_priors:theater_ratio(paris_article_4_ndc__sovereigntist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__sovereigntist_reading, rope).
narrative_ontology:human_readable(paris_article_4_ndc__sovereigntist_reading, "Paris Article 4 NDCs as Voluntary Sovereign Pledges").
narrative_ontology:topic_domain(paris_article_4_ndc__sovereigntist_reading, "international_climate_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__sovereigntist_reading, 'd4f55461-287b-43cb-8f10-eb01b4634129').
narrative_ontology:cs_kernel_codification('d4f55461-287b-43cb-8f10-eb01b4634129', formalized).
narrative_ontology:cs_authority_grounding('d4f55461-287b-43cb-8f10-eb01b4634129', lineage).
narrative_ontology:cs_interpretation_layer_present('d4f55461-287b-43cb-8f10-eb01b4634129').
narrative_ontology:cs_reading_relation('d4f55461-287b-43cb-8f10-eb01b4634129', paris_article_4_ndc__supranational_reading, coexists_with).
narrative_ontology:cs_reading_relation('d4f55461-287b-43cb-8f10-eb01b4634129', paris_article_4_ndc__equity_reading, coexists_with).
narrative_ontology:cs_axiom('d4f55461-287b-43cb-8f10-eb01b4634129', foundational, national_sovereignty_preserved_in_mitigation).
narrative_ontology:cs_axiom_status(national_sovereignty_preserved_in_mitigation, holdable).
narrative_ontology:cs_axiom_grounding('d4f55461-287b-43cb-8f10-eb01b4634129', national_sovereignty_preserved_in_mitigation, conventional).
narrative_ontology:cs_axiom('d4f55461-287b-43cb-8f10-eb01b4634129', foundational, voluntary_pledge_revisable_at_will).
narrative_ontology:cs_axiom_status(voluntary_pledge_revisable_at_will, holdable).
narrative_ontology:cs_axiom_grounding('d4f55461-287b-43cb-8f10-eb01b4634129', voluntary_pledge_revisable_at_will, conventional).
narrative_ontology:cs_reference_frame('d4f55461-287b-43cb-8f10-eb01b4634129', paris_agreement_adoption_2015).
narrative_ontology:cs_drift_state('d4f55461-287b-43cb-8f10-eb01b4634129', post_first_global_stocktake_2023, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d4f55461-287b-43cb-8f10-eb01b4634129', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, sovereign_states).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, fossil_dependent_economies).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, national_governments).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__sovereigntist_reading, national_energy_sovereignty).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__sovereigntist_reading, common_but_differentiated_responsibilities_sovereigntist).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__sovereigntist_reading, voluntary_cooperation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set their own NDC targets through domestic political processes; retain the right to adjust NDCs under Article 4.11; face no binding enforcement for non-achievement; use the voluntary framework to preserve policy space for national energy decisions.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, sovereign_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefit from the absence of mandated phase-out timelines; can submit NDCs that accommodate continued fossil development; use sovereignty argument to resist external pressure for stricter targets; revision freedom allows alignment with domestic economic cycles.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, fossil_dependent_economies, beneficiary,
    powerful, biographical, mobile, global).

% Control the NDC formulation process domestically; gain international legitimacy from participation without binding commitments; face domestic political constraints but no international sanctions for ambition gaps.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, national_governments, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__sovereigntist_reading, national_governments, agenda_setter).

% Depend on collective mitigation ambition that the voluntary system does not guarantee; lack leverage to compel higher ambition from major emitters; their survival interests are not structurally represented in the sovereignty-preserving framework.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, climate_vulnerable_states, excluded,
    moderate, generational, trapped, global).

% Bear the long-term costs of insufficient aggregate mitigation; have no voice in current NDC formulation; the voluntary framework provides no mechanism to represent their interests against present sovereign preferences.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, future_generations, excluded,
    powerless, civilizational, trapped, universal).

% Administers the NDC registry and global stocktake; facilitates transparency but has no enforcement authority; produces synthesis reports that reveal ambition gaps without power to close them.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, unfccc_secretariat, observer,
    institutional, generational, analytical, global).

% Monitor NDC ambition and implementation; advocate for stronger commitments through naming and shaming; rely on reputational pressure as the only accountability mechanism in the voluntary system.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, civil_society_observers, observer,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal participation framework where all states communicate mitigation intentions transparently, enabling collective awareness of aggregate effort without imposing binding targets that would trigger sovereignty objections and non-participation.
% TRANSFER_FUNCTION: Moves the burden of ambition-setting from international negotiation to domestic political processes; transfers the cost of non-achievement from the international system (which has no enforcement) to domestic constituencies and future generations.
% ABSENT_VOICES: Climate-vulnerable states and future generations are structurally excluded from the NDC formulation process; they would object to the low aggregate ambition but have no seat at the sovereign drafting table.
% DISAPPEARANCE_RATIONALE: If the voluntary NDC framework vanished, the Paris Agreement's universal participation architecture would collapse; states would revert to the pre-2015 bifurcated regime (Kyoto-style binding targets for developed countries only) or to purely domestic climate policy with no international transparency mechanism.
% FOUNDING_PROBLEM: The Kyoto Protocol's binding-targets-for-developed-countries-only approach failed to achieve universal participation; major emitters (US, China, India) either withdrew or never ratified; the founding problem was designing a framework that all sovereign states would join.
% FOUNDING_PROBLEM_CORROBORATION: The universal participation outcome (195 parties) is attested by the UNFCCC secretariat and independent international law scholars outside the benefiting sovereign states; however, climate-vulnerable state representatives and civil society attest that the participation solution created a new problem of collectively insufficient ambition.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__sovereigntist_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__sovereigntist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__sovereigntist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(paris_article_4_ndc__sovereigntist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__sovereigntist_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.18) reflects that the constraint imposes minimal mandatory costs — states incur only reporting burdens, not mitigation costs they haven't chosen. Suppression (0.12) is low because the treaty explicitly preserves exit and revision rights. Theater ratio (0.25) captures the growing performative gap: the global stocktake and transparency framework create an appearance of accountability while the sovereign core remains untouchable. Accessibility collapse (0.35) is moderate — alternatives (binding targets, carbon clubs, border adjustments) exist but are politically suppressed by the sovereignty norm. Resistance (0.15) is low because the constraint is designed to avoid triggering sovereignty resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the sovereign state seat, this is a genuine coordination achievement (rope): universal participation achieved without sovereignty sacrifice. From the climate-vulnerable seat, the same structure operates as a snare: the voluntary framework legitimizes collectively suicidal ambition levels while blocking stronger mechanisms. The engine computes this divergence from the structural power/exit asymmetry — the sovereigntist claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Sovereign states and fossil-dependent economies sit at the beneficiary end (d ~ 0.1-0.2): they gain policy freedom and development pathway preservation. National governments are dual-positioned (agenda_setter + beneficiary). Climate-vulnerable states and future generations are at the target end (d ~ 0.8-0.9) but lack structural power to register as victims in this reading's framing — their costs are externalized. The UNFCCC secretariat and civil society are analytical observers (d ~ 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (universal participation) remains live — the voluntary design succeeded in achieving 195-party participation where binding designs failed. However, a secondary mandatrophy dynamic is emerging: the participation mandate has been fulfilled, but the mitigation mandate is failing. The constraint persists because no sovereign coalition exists to replace it, and the sovereignty norm prevents evolution toward bindingness. This is not classic mandatrophy (original function atrophied) but a function-shift: the coordination function succeeded, the extraction-avoidance function persists, the mitigation function atrophies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading,
    'Is this constraint a single reading of the contested paris_article_4_ndc kernel, and how does the reading_id map to structural parameters?',
    'Committee metadata: kernel_id=paris_article_4_ndc, reading_id=sovereigntist_reading, sibling_readings=[supranational_reading, equity_reading]. Structural delta: low epsilon, retained exit/revision, fossil pathway preservation, enforcement atrophy.',
    'Confirms this JSON represents one instantiation of a kernel; other readings are separate constraint stories with their own epsilon and stakeholder structures. Prevents conflation of readings into one constraint with variable metrics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading, conceptual, 'Committee frame: this is one reading of a contested kernel, not the kernel itself.').

omega_variable(
    sovereignty_vs_mitigation_tradeoff,
    'Does the sovereignty-preserving design structurally guarantee insufficient aggregate mitigation, or can voluntary pledges plus transparency achieve 1.5C through iterative ratcheting?',
    'Empirical test: track NDC ambition trajectory across global stocktake cycles (2023, 2028, 2033). If aggregate ambition converges to 1.5C without binding enforcement, the coordination function suffices. If gap persists or widens, the sovereignty constraint is structurally incompatible with the physical target.',
    'If incompatible, the sovereigntist reading''s claimed_type (rope) masks a snare for vulnerable parties; if compatible, the reading''s low-epsilon claim is validated. Determines whether the constraint is a genuine coordination success or a coordinated failure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_vs_mitigation_tradeoff, empirical, 'Whether voluntary sovereignty-preserving NDCs can physically deliver the Paris temperature goal.').

omega_variable(
    article_4_11_revision_scope,
    'Does Article 4.11''s ''may adjust'' language permit only upward revision, or does it equally permit downward revision (lowering ambition)?',
    'Legal interpretation: text says ''may adjust... with a view to enhancing its level of ambition'' — the ''enhancing'' qualifier suggests directional intent. State practice: no party has formally lowered ambition; some have submitted unchanged or marginally updated NDCs. Tribunal/ICJ advisory opinion could clarify.',
    'If downward revision is permitted, the constraint is even lower extraction (pure optionality). If only upward revision is legally permissible, the constraint has a ratchet structure that increases extraction over time — moving toward tangled_rope or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_4_11_revision_scope, conceptual, 'Whether the revision freedom is bidirectional or a one-way ratchet.').

omega_variable(
    enforcement_atrophy_measurement,
    'Is the absence of enforcement mechanisms a stable equilibrium (states prefer it) or a transitional state (enforcement will emerge through custom, trade measures, or litigation)?',
    'Track emergence of enforcement proxies: CBAM (EU), climate litigation (Urgenda, Neubauer, Shell), trade agreements with climate chapters, ICJ advisory opinion. If these create de facto enforcement, the ''no enforcement'' claim describes a moment, not a structure.',
    'If enforcement proxies accumulate, the constraint drifts toward tangled_rope (coordination + asymmetric extraction via border measures) or scaffold (transitional toward binding regime). The current low suppression metric would be a snapshot, not a structural invariant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_atrophy_measurement, empirical, 'Whether enforcement atrophy is permanent or transitional.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__sovereigntist_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(paris_ndc_sov_tr_t0, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(paris_ndc_sov_tr_t0, observed).
narrative_ontology:measurement(paris_ndc_sov_tr_t3, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 3, 0.22).
narrative_ontology:measurement_basis(paris_ndc_sov_tr_t3, observed).
narrative_ontology:measurement(paris_ndc_sov_tr_t6, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 6, 0.24).
narrative_ontology:measurement_basis(paris_ndc_sov_tr_t6, observed).
narrative_ontology:measurement(paris_ndc_sov_tr_t10, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement_basis(paris_ndc_sov_tr_t10, observed).

% Extraction over time
narrative_ontology:measurement(paris_ndc_sov_be_t0, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(paris_ndc_sov_be_t0, observed).
narrative_ontology:measurement(paris_ndc_sov_be_t3, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 3, 0.16).
narrative_ontology:measurement_basis(paris_ndc_sov_be_t3, observed).
narrative_ontology:measurement(paris_ndc_sov_be_t6, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 6, 0.17).
narrative_ontology:measurement_basis(paris_ndc_sov_be_t6, observed).
narrative_ontology:measurement(paris_ndc_sov_be_t10, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 10, 0.18).
narrative_ontology:measurement_basis(paris_ndc_sov_be_t10, observed).

% Suppression requirement over time
narrative_ontology:measurement(paris_ndc_sov_su_t0, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(paris_ndc_sov_su_t0, observed).
narrative_ontology:measurement(paris_ndc_sov_su_t3, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 3, 0.11).
narrative_ontology:measurement_basis(paris_ndc_sov_su_t3, observed).
narrative_ontology:measurement(paris_ndc_sov_su_t6, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 6, 0.12).
narrative_ontology:measurement_basis(paris_ndc_sov_su_t6, observed).
narrative_ontology:measurement(paris_ndc_sov_su_t10, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 10, 0.12).
narrative_ontology:measurement_basis(paris_ndc_sov_su_t10, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__sovereigntist_reading, information_standard).
narrative_ontology:boltzmann_floor_override(paris_article_4_ndc__sovereigntist_reading, 0.02).
narrative_ontology:affects_constraint(paris_article_4_ndc__sovereigntist_reading, paris_article_6_market_mechanisms).
narrative_ontology:affects_constraint(paris_article_4_ndc__sovereigntist_reading, paris_article_13_transparency_framework).
narrative_ontology:affects_constraint(paris_article_4_ndc__sovereigntist_reading, paris_article_14_global_stocktake).

% DUAL FORMULATION NOTE:
% This is the sovereigntist_reading of the paris_article_4_ndc kernel. The supranational_reading and equity_reading are separate constraint stories with distinct epsilon, stakeholder structures, and claimed_types. All three form the paris_article_4_ndc constraint family linked by mutual affects_constraints references.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
