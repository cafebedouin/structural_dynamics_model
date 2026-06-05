% ============================================================================
% CONSTRAINT STORY: permissive_license_text__copyleft_counterfactual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__copyleft_counterfactual_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: permissive_license_text__copyleft_counterfactual_reading
 *   human_readable: Permissive License Extraction (Copyleft Counterfactual Reading)
 *   domain: software_licensing/intellectual_property/technology_governance
 *
 * SUMMARY:
 *   Permissive software licensing (MIT, Apache 2.0, BSD) removes reciprocity
 *   requirements that copyleft licenses (GPL) impose. This constraint story
 *   instantiates the copyleft counterfactual reading: the reading that claims
 *   permissive licensing WITHOUT reciprocity obligation structurally enables
 *   value extraction from the open-source commons by proprietary software
 *   firms. Contributors donate labor expecting benefit from network effects
 *   and community improvement; instead, corporations close derivative works,
 *   capture value, and provide no obligation to contribute back. The
 *   constraint exhibits tangled-rope structure from the aggregate
 *   institutional view: permissive licensing does solve a genuine
 *   coordination problem (components can be freely combined), but it
 *   simultaneously enables asymmetric extraction (beneficiaries extract
 *   without obligation). The sister readings — commons_coordination_reading
 *   (emphasizing the voluntary-benefit model) and corporate_moat_reading
 *   (emphasizing proprietary firms' legitimate product differentiation) —
 *   represent live competing positions in open-source governance communities.
 *   This reading claims the extraction structure is real and contingent on
 *   the license choice, not inevitable.
 *
 * KEY AGENTS:
 *   - Open Source Commons Contributors: Primary victim (powerless/trapped) — donate labor under permissive licenses expecting reciprocal benefit; extraction has no retroactive remedy
 *   - Proprietary Software Builders: Primary beneficiary (institutional/arbitrage) — extract code, close derivatives, capture value without reciprocal obligation
 *   - Platform Companies: Secondary beneficiary (powerful/mobile) — embed permissive libraries, capture network effects, maintain extraction by avoiding GPL adoption
 *   - GPL Adopting Projects: Mixed actor (moderate/constrained) — gain reciprocal coordination but lose proprietary product flexibility; experience the constraint as enforcement obligation
 *   - Open Source Governance Bodies: Institutional observer (institutional/arbitrage) — debate whether permissive or copyleft is the optimal licensing model; maintain the kernel-level contest
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks treating the constraint as an immutable property of software economics rather than a contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, 0.62).
domain_priors:suppression_score(permissive_license_text__copyleft_counterfactual_reading, 0.68).
domain_priors:theater_ratio(permissive_license_text__copyleft_counterfactual_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__copyleft_counterfactual_reading, tangled_rope).
narrative_ontology:human_readable(permissive_license_text__copyleft_counterfactual_reading, "Permissive License Extraction (Copyleft Counterfactual Reading)").
narrative_ontology:topic_domain(permissive_license_text__copyleft_counterfactual_reading, "software_licensing/intellectual_property/technology_governance").

domain_priors:requires_active_enforcement(permissive_license_text__copyleft_counterfactual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__copyleft_counterfactual_reading, 'd501d6d3-7ff3-47b2-8704-df8d6f920c91').
narrative_ontology:cs_kernel_codification('d501d6d3-7ff3-47b2-8704-df8d6f920c91', formalized).
narrative_ontology:cs_authority_grounding('d501d6d3-7ff3-47b2-8704-df8d6f920c91', distributed).
narrative_ontology:cs_reading_relation('d501d6d3-7ff3-47b2-8704-df8d6f920c91', permissive_license_text__commons_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('d501d6d3-7ff3-47b2-8704-df8d6f920c91', permissive_license_text__corporate_moat_reading, coexists_with).
narrative_ontology:cs_axiom('d501d6d3-7ff3-47b2-8704-df8d6f920c91', foundational, reciprocity_absent_enables_asymmetric_extraction).
narrative_ontology:cs_axiom_status(reciprocity_absent_enables_asymmetric_extraction, holdable).
narrative_ontology:cs_axiom_grounding('d501d6d3-7ff3-47b2-8704-df8d6f920c91', reciprocity_absent_enables_asymmetric_extraction, empirically_contingent).
narrative_ontology:cs_axiom('d501d6d3-7ff3-47b2-8704-df8d6f920c91', foundational, commons_contributors_lack_exit_capacity).
narrative_ontology:cs_axiom_status(commons_contributors_lack_exit_capacity, holdable).
narrative_ontology:cs_axiom_grounding('d501d6d3-7ff3-47b2-8704-df8d6f920c91', commons_contributors_lack_exit_capacity, empirically_contingent).
narrative_ontology:cs_reference_frame('d501d6d3-7ff3-47b2-8704-df8d6f920c91', reciprocal_obligation_licensing_regime).
narrative_ontology:cs_drift_state('d501d6d3-7ff3-47b2-8704-df8d6f920c91', contemporary_permissive_dominance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d501d6d3-7ff3-47b2-8704-df8d6f920c91', '').
narrative_ontology:cs_kernel_id(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, proprietary_software_builders).
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, platform_companies).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, open_source_commons).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, reciprocal_obligation_adopters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OPEN SOURCE COMMONS (SNARE) — The software commons lacks exit capacity. Contributors donate labor under permissive licenses expecting reciprocal benefit; instead, proprietary firms extract the labor, close the derivative works, and capture the value. The commons cannot organize collective withdrawal or enforce reciprocity retroactively. No alternative distribution channel exists at scale. Maximum extraction with zero degrees of freedom.
constraint_indexing:constraint_classification(permissive_license_text__copyleft_counterfactual_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GPL ADOPTING PROJECT (TANGLED ROPE) — Projects that adopt GPL experience constraints: derivative builders must release their code, limiting proprietary product strategies. But GPL also coordinates their community: GPL enforcement ensures that improvements flow back, creating a genuine coordination benefit alongside extraction (enforcement obligation on others). The constraint is mixed — coordination + asymmetric enforcement.
constraint_indexing:constraint_classification(permissive_license_text__copyleft_counterfactual_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: PROPRIETARY SOFTWARE COMPANY (ROPE) — Institutional actors experience permissive licensing as pure coordination: they can freely use, modify, and commercialize open-source code without obligation. The license communicates a boundary (reusable components) and a rule (attribution or no obligation at all). No extraction is perceived because the company bears no cost and faces no constraint. Net beneficiary with full exit capacity.
constraint_indexing:constraint_classification(permissive_license_text__copyleft_counterfactual_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SOFTWARE LICENSE ECOSYSTEM (PITON) — Treating permissive licenses as a technical governance mechanism has largely failed at scale. The licensing system was designed to allocate rights; it cannot prevent value extraction because the system depends on voluntary compliance without enforcement. The theater of 'attribution' and 'free use' persists while the functional coordination goal (reciprocal contribution) has atrophied. Measured performance vs declared purpose shows the gap: the license exists but does not accomplish what the open-source movement intended.
constraint_indexing:constraint_classification(permissive_license_text__copyleft_counterfactual_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PLATFORM MONOPOLIST (TANGLED ROPE) — Large platform companies derive genuine coordination benefits from permissive-licensed libraries (they solve common problems; the platform improves). But they also extract value: the platform embeds library code, captures network effects, and raises barriers to entry for competitors. The platform has mobility (could adopt GPL) but chooses not to — the extraction benefit exceeds the coordination cost. Mixed but asymmetrically favorable to the platform.
constraint_indexing:constraint_classification(permissive_license_text__copyleft_counterfactual_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal view, the asymmetry is an inevitable feature of intellectual property systems: code is a public good once created; any licensing system that allows use without reciprocity will be exploited by those with extraction capacity. This is structurally immutable — a property of information economics, not a contingent social choice. However, GPL as an alternative demonstrates this is not a mountain: the extraction is contingent on the permissive license choice, not inherent to code itself.
constraint_indexing:constraint_classification(permissive_license_text__copyleft_counterfactual_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__copyleft_counterfactual_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(permissive_license_text__copyleft_counterfactual_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(permissive_license_text__copyleft_counterfactual_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(permissive_license_text__copyleft_counterfactual_reading, TR),
    TR >= 0.70.

:- end_tests(permissive_license_text__copyleft_counterfactual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High-moderate. Permissive licenses enable value capture by proprietary firms without reciprocal obligation. The extractiveness has accumulated over the measurement interval as platform consolidation increased (time 0 → 10: 0.35 → 0.62), reflecting that the value asymmetry compounds as a small number of firms capture increasing market share while the commons remains fragmented and uncompensated. The measure is not at the snare floor (≥0.66) because GPL alternatives exist and some commons projects thrive (partial mitigation). Suppression (0.68): High. Contributors face multiple barriers to organizing or switching: network effects lock them into ecosystems built on permissive licenses, proprietary derivatives prevent forking, and the commons lacks formal governance to negotiate reciprocity. Suppression has risen over the interval (0.45 → 0.68) as platform dominance increased, making it harder to defect to alternatives. Theater ratio (0.38): Moderate-low. The licensing mechanism is functional but not theatrical — the license itself accurately communicates the terms (permissive, no obligation). The low theater reflects that the extraction happens through the license's literal terms, not through performative compliance or ritualistic enforcement. The flatness over the interval (0.42 → 0.38) shows this is structural, not drift.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full perspectival range from a single structural phenomenon. The proprietary firm perceives coordination (Rope); the commons perceives extraction (Snare); the GPL project perceives mixed (Tangled Rope); the analytical observer risks naturalizing as law (Mountain). None of these perspectives is 'wrong' — each is accurate from within that position. The gap reveals that 'permissive licensing is good' is true from the beneficiary's perspective (lower development costs, larger addressable market) and false from the victim's perspective (donated labor captured without return). The analytical observer's mountain classification is a false summit — GPL proves the extraction is contingent on the license choice, not immutable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality in this constraint is determined by each agent's position in the value-flow asymmetry. The commons contributors are victims with no exit (trapped → d ≈ 0.95 → high f(d)). The proprietary firms are beneficiaries with full exit (arbitrage → d ≈ 0.05 → negative f(d)). The GPL-adopting projects are constrained victims with partial exit (constrained → d ≈ 0.70 → moderate f(d)). The platform companies are beneficiaries with exit capacity but choose not to exercise it (mobile but beneficiary → d ≈ 0.25 → low f(d)). The directionality is stable and structural: it is determined by who owns the derivative works and who captures the value, not by time horizon or scope. The engine's derivation chain computes d from beneficiary/victim declaration + exit options, producing the measurable extraction asymmetry in chi.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by the kernel-level reading contest. This reading (copyleft counterfactual) claims that permissive licensing enables extraction because reciprocity is absent. The corporate_moat_reading (sibling) claims proprietary builders have the right to build on open-source code without reciprocal obligation because they add genuine value (infrastructure, support, market access). The commons_coordination_reading (sibling) claims permissive licensing benefits the entire ecosystem through network effects and widespread adoption, so the 'extraction' is actually a common good. All three readings describe the same structural phenomenon (proprietary firm uses permissive-licensed code, closes the derivative, extracts value). The contest is about whether this is extraction (this reading), legitimate differentiation (corporate_moat_reading), or beneficial coordination (commons_coordination_reading). The readings coexist because open-source governance currently hosts all three positions in active communities.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocity_measurability,
    'Can meaningful reciprocity be measured and enforced at scale without turning enforcement into its own extraction mechanism?',
    'Comparative analysis of GPL enforcement actions: do copyleft projects successfully extract reciprocal contribution, or does enforcement become a barrier to legitimate reuse?',
    'If enforcement succeeds: GPL is a genuine alternative (constraint reclassifies as coordination-heavy Tangled Rope or Rope). If enforcement becomes predatory: GPL shifts the extraction target to proprietary builders, flipping the victim/beneficiary roles without resolving the extraction structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reciprocity_measurability, empirical, 'Whether copyleft enforcement yields genuine reciprocal contribution or becomes predatory').

omega_variable(
    commons_sustainability_without_reciprocity,
    'Is the open-source commons sustainable indefinitely under permissive licensing, or does extractive value capture eventually degrade the commons?',
    'Longitudinal analysis of open-source project health: contributor diversity, maintenance burden, feature stagnation, and fork prevalence in permissive vs copyleft projects over 10+ year windows.',
    'If permissive commons degrade: the snare classification is validated. If permissive commons remain healthy: the commons may have coordination benefits (direct benefit from widespread adoption, improved libraries, recruitment) that offset extraction losses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_sustainability_without_reciprocity, empirical, 'Whether open-source commons sustainability requires reciprocity enforcement').

omega_variable(
    kernel_reading_contest,
    'What is the contested kernel underlying permissive licensing — the foundational claim that sibling readings disagree on?',
    'Analysis of licensing debates in open-source governance bodies and academic IP scholarship: what core assumption do permissive-license advocates defend that copyleft advocates reject?',
    'Clarifies whether the readings are in genuine logical foreclosure (one rules out the other), coexistence (both can be true simultaneously for different communities), or sequential influence (one reading creates conditions that pressure the other).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural relationship between permissive and copyleft readings of software licensing').

omega_variable(
    value_extraction_vs_allocation,
    'Is the constraint about asymmetric value extraction (proprietary firm captures value created by commons contributors) or about allocation of development costs (who bears the burden of derivative work maintenance)?',
    'Distinguish mechanism: trace the direction of labor flow and benefit flow separately. Does the proprietary firm reduce its development cost? Does the open-source contributor lose financial benefit they would have captured?',
    'If extraction framing is correct: Snare is the accurate classification for the commons agent. If allocation framing is correct: the constraint is a coordination problem about shared maintenance burden — Tangled Rope or pure Rope depending on whether the burden is genuinely shared.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(value_extraction_vs_allocation, conceptual, 'Whether the constraint mechanisms are asymmetric value extraction or shared cost allocation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__copyleft_counterfactual_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_copyleft_tr_t0, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(perm_copyleft_tr_t5, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 5, 0.39).
narrative_ontology:measurement(perm_copyleft_tr_t10, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(perm_copyleft_be_t0, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(perm_copyleft_be_t5, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(perm_copyleft_be_t10, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 10, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(perm_copyleft_su_t0, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(perm_copyleft_su_t5, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(perm_copyleft_su_t10, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__copyleft_counterfactual_reading, information_standard).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text__commons_coordination_reading).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text__corporate_moat_reading).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, open_source_contributor_labor_asymmetry).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, platform_consolidation_commons_capture).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'permissive_license_text' kernel. The sibling readings (commons_coordination_reading and corporate_moat_reading) are separate constraint stories with different epsilon values, different beneficiary/victim structures, and different classifications. The kernel-level contest is about the foundational claim: does permissive licensing enable extraction, or does it enable sustainable commons? Each reading is a structurally distinct constraint with its own epistemology. Network links preserve the reading relationships without collapsing them into a single story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
