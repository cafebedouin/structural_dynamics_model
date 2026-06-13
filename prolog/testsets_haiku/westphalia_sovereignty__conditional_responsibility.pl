% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__conditional_responsibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__conditional_responsibility, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: westphalia_sovereignty__conditional_responsibility
 *   human_readable: Conditional Sovereignty via Responsibility to Protect Doctrine
 *   domain: international_law/political_theory
 *
 * SUMMARY:
 *   The conditional responsibility reading interprets Westphalian sovereignty
 *   as a status contingent on state performance in protecting populations
 *   from mass atrocities. Under this reading, the Responsibility to Protect
 *   (R2P) doctrine operationalizes sovereignty loss: states that commit or
 *   tolerate genocide, ethnic cleansing, or crimes against humanity forfeit
 *   the territorial inviolability that is sovereignty's core protection. This
 *   lowers the threshold for external intervention and vests adjudicative
 *   authority in the international community (UN bodies, coalitions of
 *   willing states) rather than in the targeted state. The reading claims to
 *   solve the post-Cold War legitimacy problem—how to block humanitarian
 *   catastrophe without returning to pure interventionism—but instantiates a
 *   tangled_rope constraint: it provides genuine coordination (a threshold
 *   for when the international community collectively intervenes) and genuine
 *   extraction (it vests unprecedented adjudicative power in the intervening
 *   coalitions and allows them to leverage that power for strategic
 *   repositioning). This is a single reading of a contested kernel; the
 *   sibling readings (absolute_non_intervention and graded_sovereignty) are
 *   different constraints entirely and are authored in separate story files.
 *
 * KEY AGENTS:
 *   - Atrocity-vulnerable populations: trapped, powerless, theoretically benefit from the doctrine but have no seat in its application
 *   - Humanitarian intervention coalitions: typically Western military alliances and UN permanent members; set the agenda, define thresholds, execute interventions; collect legitimacy cover and strategic advantage
 *   - Global governance institutions: UN, ICJ, human rights bodies; gain adjudicative authority and funding under the doctrine
 *   - Non-intervening states: Global South, non-aligned nations; bear precedent costs, constrained exit, lack capacity to counter-intervene
 *   - Sovereignty-claiming regimes: moderate power, identity-locked to sovereignty claims, stripped of categorical immunity by the doctrine
 *   - Atrocity documentation networks: observer seat, provide epistemic foundation for thresholds
 *   - Alternate sovereignty regimes: excluded, cannot be incorporated without fundamentally different constraint story
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__conditional_responsibility, 0.68).
domain_priors:suppression_score(westphalia_sovereignty__conditional_responsibility, 0.72).
domain_priors:theater_ratio(westphalia_sovereignty__conditional_responsibility, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, extractiveness, 0.68).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__conditional_responsibility, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__conditional_responsibility, "Conditional Sovereignty via Responsibility to Protect Doctrine").
narrative_ontology:topic_domain(westphalia_sovereignty__conditional_responsibility, "international_law/political_theory").

domain_priors:requires_active_enforcement(westphalia_sovereignty__conditional_responsibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__conditional_responsibility, 'e42788e6-fcfa-4617-afa2-9ff2f60798e5').
narrative_ontology:cs_kernel_codification('e42788e6-fcfa-4617-afa2-9ff2f60798e5', formalized).
narrative_ontology:cs_authority_grounding('e42788e6-fcfa-4617-afa2-9ff2f60798e5', extraction).
narrative_ontology:cs_interpretation_layer_present('e42788e6-fcfa-4617-afa2-9ff2f60798e5').
narrative_ontology:cs_reading_relation('e42788e6-fcfa-4617-afa2-9ff2f60798e5', westphalia_sovereignty__absolute_non_intervention, coexists_with).
narrative_ontology:cs_reading_relation('e42788e6-fcfa-4617-afa2-9ff2f60798e5', westphalia_sovereignty__graded_sovereignty, influences).
narrative_ontology:cs_axiom('e42788e6-fcfa-4617-afa2-9ff2f60798e5', foundational, sovereignty_contingent_on_protection_capacity).
narrative_ontology:cs_axiom_status(sovereignty_contingent_on_protection_capacity, holdable).
narrative_ontology:cs_axiom_grounding('e42788e6-fcfa-4617-afa2-9ff2f60798e5', sovereignty_contingent_on_protection_capacity, deontological).
narrative_ontology:cs_axiom('e42788e6-fcfa-4617-afa2-9ff2f60798e5', foundational, international_community_adjudicates_capacity).
narrative_ontology:cs_axiom_status(international_community_adjudicates_capacity, holdable).
narrative_ontology:cs_axiom_grounding('e42788e6-fcfa-4617-afa2-9ff2f60798e5', international_community_adjudicates_capacity, empirically_contingent).
narrative_ontology:cs_reference_frame('e42788e6-fcfa-4617-afa2-9ff2f60798e5', categorical_non_intervention_post_westphalia).
narrative_ontology:cs_drift_state('e42788e6-fcfa-4617-afa2-9ff2f60798e5', post_rwanda_responsibility_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('e42788e6-fcfa-4617-afa2-9ff2f60798e5', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, global_governance_institutions).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, atrocity_vulnerable_populations).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, non_intervening_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, sovereignty_claiming_regimes).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__conditional_responsibility, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(westphalia_sovereignty__conditional_responsibility, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__conditional_responsibility_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__conditional_responsibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__conditional_responsibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.68 terminal value) and rising steadily (0.42 at t=2000) because the doctrine's application has become increasingly selective and strategically motivated. Early application (Kosovo, East Timor) was defensible as genuine atrocity response; later cases (Libya 2011 as NATO regime-change cover, Syria non-intervention despite documented atrocity, Yemen bombardment with no atrocity intervention framing) show extraction: the coalitions use the doctrine when it serves them and abandon it when it doesn't. Suppression is high (0.72) because maintaining the doctrine's legitimacy requires actively suppressing alternate framings (the absolute_non_intervention framing, regional sovereignty claims, challenges to intervention selective application). Theater ratio rises from 0.35 to 0.58, indicating that as extraction increases, the doctrine's performative function (legitimacy theater for intervention decisions already made on strategic grounds) grows. Accessibility collapse is low (0.41) because alternatives are visible and actively defended: Russia, China, and non-aligned states continue to invoke absolute non-intervention and reject conditional readings. Resistance is very high (0.79), reflecting sustained opposition from sovereignty-claiming states, the documented cases where the doctrine was selectively applied or abandoned, and the scholarly/diplomatic debate challenging the doctrine's legitimacy. Measurements are authored on a shared 24-year grid (t=2000 [pre-R2P codification], 2005 [World Summit adoption], 2010 [Libya building], 2015 [Libya aftermath, Syria stalemate], 2020 [Myanmar crisis, COVID disruption], 2024 [terminal measurement]). The measurement series tracks extraction rising as application becomes more selective; theater rising as legitimacy function dominates actual intervention thresholds; suppression plateauing at high level as opposition stabilizes.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (intervention coalitions, global institutions) perceives this constraint as genuine coordination: it solves the post-Rwanda legitimacy problem by creating a threshold for when sovereignty yields. They read high extractiveness as necessary institutional overhead and justify theater ratio as public education. The payer seats (non-intervening states, sovereignty-claiming regimes) perceive this as extraction: adjudicative authority over their legitimacy has been vested in external coalitions that apply it strategically. From the victim population's seat, the doctrine is coordination on paper but fails delivery—it lowers intervention thresholds without ensuring intervention occurs at the lowest-atrocity cases (absent voice problem). The engine computes these divergences from the structural data: the coalition's arbitrage exit options and institutional power produce beneficiary-type directionality; the non-intervening states' constrained exit and moderate power produce target-type directionality; the victim populations' powerlessness and trap produces high target directionality with no mitigation. These are the grounds for seat divergence in classification, independent of the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanitarian intervention coalitions (institutional, arbitrage, global scope) derive low directionality (d~0.15–0.25): they benefit from expanded mandate, collect legitimacy cover, can exit the doctrine if it becomes constraining by simply not invoking it (see Syria). Global governance institutions (institutional, analytical, universal scope) derive near-beneficiary directionality (d~0.10–0.20): they gain funding, authority, and career incentives from R2P implementation. Non-intervening states (organized, constrained exit, global scope by implication) derive high target directionality (d~0.70–0.85): they bear precedent costs without benefit, cannot exit, have their sovereignty status externally judged. Sovereignty-claiming regimes (moderate power, identity-locked exit, regional scope) derive very high target directionality (d~0.80–0.95): they lose categorical immunity, exit means abandoning state-identity, regional scope makes them visible to coalition intervention. Atrocity-vulnerable populations (powerless, trapped, regional scope) derive maximum target directionality despite nominal beneficiary role (d~0.95): they are the constraint's stated subjects but have zero agency in its application and no guaranteed benefit. No directionality overrides are needed; the derivation chain (beneficiary/victim declarations + exit options + power atoms) produces the correct spread from the structural data.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-genocide legitimacy crisis, need for threshold to block mass atrocity) is declared as 'contested' status in the six_questions. This is accurate: the humanitarian intervention coalitions attest the problem is live and the doctrine solves it; skeptics and non-intervening states attest the problem was overstated or that other mechanisms (courts, sanctions, refugee support) address it better. The selective application pattern (Kosovo intervened, Syria not; Libya intervened as regime change, Yemen bombardment not framed as R2P response) provides evidence for the skeptics' reading. The mandatrophy condition would be: the doctrine persists because it benefits the intervening coalitions and governance institutions (who maintain it), but the founding problem it was meant to solve is either dead (atrocity prevention through other means) or contested (whether R2P actually prevents atrocities or just provides legitimacy cover for strategic intervention). The rising theater_ratio (0.35→0.58) and selective application pattern support mandatrophy: the doctrine's primary function has shifted from atrocity threshold-setting to legitimacy theater for intervention decisions made on strategic grounds. This does not automatically reclassify it from tangled_rope (which allows asymmetric extraction and enforcement), but it marks the constraint as a candidate for mandatrophy drift detection: if theater_ratio continues to rise while measured atrocity-prevention outcomes remain flat or decline, the engine's T17 abductive trigger (mountain_extraction_accumulation) would flag for investigation whether the mandate has outlived its function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    atrocity_threshold_definition,
    'Who defines what counts as mass atrocity for purposes of sovereignty loss? Is the threshold set by objective evidence or by political negotiation among intervening powers?',
    'Track intervention invocations: compare stated atrocity thresholds to actual intervention triggers. If interventions occur below stated thresholds (Syria, Myanmar) or non-occur above them (Yemen), the threshold is politically contingent, not objective.',
    'If threshold is politically contingent, the doctrine functions as extraction—justifying power-political intervention with humanitarian language. If threshold is evidence-anchored and applied consistently, the doctrine functions as genuine coordination. Current evidence suggests the former.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(atrocity_threshold_definition, empirical, 'Whether atrocity thresholds are objective or strategically applied.').

omega_variable(
    intervention_coalition_incentives,
    'Do humanitarian intervention coalitions intervene where atrocities are worst, or where intervention serves their strategic interests?',
    'Geographic analysis: compare atrocity severity (deaths, displacement, documented crimes) to intervention deployment and resource commitment. Examine geographic distribution of intervening powers'' strategic assets (military bases, resource access, regional alignment).',
    'If interventions correlate with severity, the doctrine operationalizes genuine protection. If interventions correlate with strategic interest, the doctrine is extractive cover for geopolitical repositioning, and beneficiary status of intervention coalitions shifts from genuine humanitarian gain to political rent collection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intervention_coalition_incentives, empirical, 'Whether intervention selection tracks atrocity severity or strategic interest.').

omega_variable(
    conditional_vs_extractive_framing,
    'Is this constraint genuinely a coordination device for humanitarian response to atrocities, or is it a snare using humanitarian language to legitimize power-political intervention?',
    'Examine: (1) Cases where intervention occurred without atrocity certification (Kosovo, Iraq 2003). (2) Cases where severe atrocities were documented but no intervention occurred (Rwanda, Syria, Yemen). (3) Post-intervention institutional change: did the intervening coalition install governance they could influence, or did they empower local democratic processes? (4) Selectivity pattern: are non-aligned or rival powers'' atrocities intervened into at the same threshold as aligned powers''?',
    'If selectivity is random or severity-driven, this is tangled rope (genuine protection with some extraction). If selectivity is strategic, this is snare (extraction disguised as humanitarian).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditional_vs_extractive_framing, empirical, 'Whether the conditional responsibility doctrine operationalizes coordination or extraction.').

omega_variable(
    sovereignty_loss_vs_capacity_degradation,
    'Does the doctrine measure sovereignty loss against an objective standard (protection capacity) or does it allow political judgment to reframe sovereignty as lost whenever intervention is desired?',
    'Compare the ''sovereignty loss'' judgments across similar cases: identical atrocity severity and regime capacity in cases where one was intervened-into and one was not. If the judgments differ, sovereignty loss is political rather than structural.',
    'If sovereignty loss is structural, the doctrine is rule-based. If it is political, the doctrine is extractive—it vests conditional sovereignty judgment in the intervening coalition, which can then leverage that judgment for strategic gain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_loss_vs_capacity_degradation, empirical, 'Whether sovereignty loss is objectively measured or politically contingent.').

omega_variable(
    kernel_reading_contest_non_intervention_vs_conditional,
    'Between the absolute_non_intervention reading (categorical territorial inviolability) and this conditional_responsibility reading, which core premise is true: that sovereignty is categorical and exogenous to state conduct, or that sovereignty is conditional on state performance?',
    'Comparative institutional analysis: examine which reading''s authority structure is actually being enforced in practice. Track: (1) How often intervention coalitions cite sovereignty loss as grounds for intervention (conditional framing) vs. citing humanitarian exception to sovereignty (non-intervention framing). (2) Which framing appears in UN Security Council resolutions, ICJ opinions, state practice. (3) Whether the conditionality is applied symmetrically (all states) or selectively (only rivals).',
    'If the conditional framing is genuinely being instantiated—atrocity documentation triggers sovereignty-loss judgments that are applied symmetrically—then conditional_responsibility is winning the reading contest and the kernel has shifted. If the non-intervention framing is used as cover and intervention occurs without sovereignty-loss judgments (or with selective application), then absolute_non_intervention is still the operative reading and conditional_responsibility is performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_non_intervention_vs_conditional, conceptual, 'Which kernel reading (categorical vs. conditional sovereignty) is actually operative in international practice.').

omega_variable(
    committer_frame_reading_relations,
    'How does this conditional_responsibility reading relate structurally to its sibling readings (absolute_non_intervention, graded_sovereignty)? Which relations are accurate: forecloses, coexists_with, or influences?',
    'Examine whether parties actually hold only one reading or whether they code-switch between readings strategically. Do absolute_non_intervention defenders simultaneously invoke graded_sovereignty when it serves them? Can a single framework hold both conditional_responsibility and absolute_non_intervention?',
    'If readings truly foreclose each other, one will eventually dominate. If they coexist, the kernel remains contested and each reading retains legitimacy. If they influence rather than foreclose, downstream dynamics will shift under this reading''s pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_reading_relations, conceptual, 'The structural relationship between conditional_responsibility and its sibling kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__conditional_responsibility, 2000, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t2000, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(west_tr_t2005, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2005, 0.42).
narrative_ontology:measurement(west_tr_t2010, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2010, 0.5).
narrative_ontology:measurement(west_tr_t2015, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2015, 0.55).
narrative_ontology:measurement(west_tr_t2020, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2020, 0.57).
narrative_ontology:measurement(west_tr_t2024, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2024, 0.58).

% Extraction over time
narrative_ontology:measurement(west_be_t2000, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(west_be_t2005, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2005, 0.51).
narrative_ontology:measurement(west_be_t2010, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(west_be_t2015, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2015, 0.63).
narrative_ontology:measurement(west_be_t2020, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement(west_be_t2024, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t2000, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2000, 0.48).
narrative_ontology:measurement(west_su_t2005, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement(west_su_t2010, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(west_su_t2015, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(west_su_t2020, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2020, 0.72).
narrative_ontology:measurement(west_su_t2024, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__conditional_responsibility, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalia_sovereignty__conditional_responsibility, 0.12).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty__absolute_non_intervention).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty__graded_sovereignty).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, international_humanitarian_law__armed_conflict).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, transnational_justice__crimes_against_humanity).

% DUAL FORMULATION NOTE:
% Constraint family: westphalia_sovereignty kernel with three reading instantiations. conditional_responsibility is the middle reading in extractiveness: absolute_non_intervention instantiates pure coordination (low extraction, mountain-side); graded_sovereignty instantiates hierarchical calibration (moderate extraction). The conditional_responsibility reading sits between them: it coordinates intervention thresholds (coordination function) while vesting adjudicative authority in intervening coalitions (extraction function). Each story is ε-invariant; the differences are in the beneficiary/victim framing and the structural mechanism, not in observable-switching. The conditional reading influences both siblings: it pressures absolute_non_intervention by raising the threshold for sovereignty defense, and it pressures graded_sovereignty by anchoring the capacity-measurement axis to atrocity triggers rather than to institutional development indices.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalia_sovereignty__conditional_responsibility, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
