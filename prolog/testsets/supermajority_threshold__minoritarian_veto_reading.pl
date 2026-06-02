% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__minoritarian_veto_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supermajority_threshold__minoritarian_veto_reading, []).

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
 *   constraint_id: supermajority_threshold__minoritarian_veto_reading
 *   human_readable: Supermajority Threshold as Minoritarian Veto Lock-in (Minoritarian Veto Reading)
 *   domain: constitutional_theory/political_economy
 *
 * SUMMARY:
 *   The supermajority threshold for constitutional amendment creates a
 *   fundamental structural constraint on majoritarian democratic will:
 *   proposals commanding 50-66% of public support and legislative majorities
 *   remain blocked from constitutional incorporation. This constraint is ONE
 *   READING of a contested kernel — the supermajority_threshold — that
 *   different political communities read differently. The
 *   minoritarian_veto_reading interprets the threshold as a snare: a
 *   mechanism that converts historical privilege (the coalitions that
 *   authored the constitution) into permanent veto power against reform,
 *   regardless of contemporary majoritarian support. Under this reading, the
 *   threshold is not a functional minority-rights protection but rather an
 *   entrenchment device that blocks necessary constitutional evolution while
 *   laundering its function through a 'consensus-safeguard' narrative. The
 *   beneficiary bloc (status quo beneficiaries, entrenched elites, historical
 *   privilege holders) experiences the threshold as pure coordination that
 *   solves their collective action problem. The victim bloc (contemporary
 *   majorities seeking constitutional reform on issues like voting rights,
 *   campaign finance, economic redistribution) experiences systematic
 *   extraction: their majority support is rendered powerless by rules
 *   authored by coalitions that no longer command contemporary legitimacy.
 *   The measurements show extraction and suppression accumulating over
 *   constitutional history — as the original coalition's privilege
 *   consolidates and becomes entrenched, the threshold's extractive function
 *   intensifies. Theater ratio remains low because the mechanism is not
 *   performative; it is functionally effective at blocking reform.
 *
 * KEY AGENTS:
 *   - Status Quo Beneficiary Bloc (institutional/arbitrage): Coalition whose existing power distribution is protected by supermajority requirement; benefits from coordination function without facing extraction. Includes: established economic interests, regional power distributions locked into founding constitutional compromises, demographic groups whose relative power has increased since founding.
 *   - Contemporary Reform Majorities (powerless/trapped): Multiple successive waves of majority-seeking constituencies (voting rights expansion advocates, campaign finance reformers, economic redistribution movements, climate action proponents) blocked by supermajority barrier. Structurally defeated across generations.
 *   - Entrenched Elites (institutional/arbitrage): Historical privilege holders whose advantages are constitutionally locked in; benefit from inertia.
 *   - Swing Minorities at 55-65% Threshold (moderate/constrained): Agents and coalitions positioned near the supermajority line; experience mixed coordination and extraction as they negotiate between protection of their own interests and blocking of broader reforms.
 *   - Constitutional Framers' Legitimacy Apparatus (institutional/arbitrage): The institutional narrative that justifies the threshold as protecting minority rights against tyranny; persists as piton despite changed function.
 *   - Analytical Observer (analytical/analytical): Risks naturalizing the threshold as immutable constitutional law rather than contingent institutional design.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, 0.68).
domain_priors:suppression_score(supermajority_threshold__minoritarian_veto_reading, 0.72).
domain_priors:theater_ratio(supermajority_threshold__minoritarian_veto_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__minoritarian_veto_reading, snare).
narrative_ontology:human_readable(supermajority_threshold__minoritarian_veto_reading, "Supermajority Threshold as Minoritarian Veto Lock-in (Minoritarian Veto Reading)").
narrative_ontology:topic_domain(supermajority_threshold__minoritarian_veto_reading, "constitutional_theory/political_economy").

domain_priors:requires_active_enforcement(supermajority_threshold__minoritarian_veto_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__minoritarian_veto_reading, '7f207f97-f850-4ebf-9dab-ae82d651a912').
narrative_ontology:cs_kernel_codification('7f207f97-f850-4ebf-9dab-ae82d651a912', formalized).
narrative_ontology:cs_authority_grounding('7f207f97-f850-4ebf-9dab-ae82d651a912', lineage).
narrative_ontology:cs_interpretation_layer_present('7f207f97-f850-4ebf-9dab-ae82d651a912').
narrative_ontology:cs_reading_relation('7f207f97-f850-4ebf-9dab-ae82d651a912', supermajority_threshold__consensus_safeguard_reading, coexists_with).
narrative_ontology:cs_reading_relation('7f207f97-f850-4ebf-9dab-ae82d651a912', supermajority_threshold__adaptive_gradient_reading, influences).
narrative_ontology:cs_axiom('7f207f97-f850-4ebf-9dab-ae82d651a912', foundational, threshold_predominantly_entrenches_rather_than_protects).
narrative_ontology:cs_axiom_status(threshold_predominantly_entrenches_rather_than_protects, holdable).
narrative_ontology:cs_axiom_grounding('7f207f97-f850-4ebf-9dab-ae82d651a912', threshold_predominantly_entrenches_rather_than_protects, empirically_contingent).
narrative_ontology:cs_axiom('7f207f97-f850-4ebf-9dab-ae82d651a912', foundational, democratic_legitimacy_requires_majoritarian_constitutional_responsiveness).
narrative_ontology:cs_axiom_status(democratic_legitimacy_requires_majoritarian_constitutional_responsiveness, holdable).
narrative_ontology:cs_axiom_grounding('7f207f97-f850-4ebf-9dab-ae82d651a912', democratic_legitimacy_requires_majoritarian_constitutional_responsiveness, deontological).
narrative_ontology:cs_reference_frame('7f207f97-f850-4ebf-9dab-ae82d651a912', founding_coalition_constitutional_authority).
narrative_ontology:cs_drift_state('7f207f97-f850-4ebf-9dab-ae82d651a912', contemporary_demographic_pluralism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7f207f97-f850-4ebf-9dab-ae82d651a912', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, status_quo_beneficiaries).
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, entrenched_elites).
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, historical_privilege_holders).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, contemporary_majorities).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, reform_constituencies).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, systemic_change_constituencies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REFORM CONSTITUENCY (SNARE) — Contemporary majorities commanding majority support for constitutional amendment (e.g., campaign finance reform, voting rights expansion, economic redistribution) face insurmountable threshold. No exit: they cannot withdraw from the polity; cannot override supermajority requirement; cannot change rules without the supermajority they lack. Maximum extraction and suppression experienced by those blocked from reform.
constraint_indexing:constraint_classification(supermajority_threshold__minoritarian_veto_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STATUS QUO BENEFICIARY BLOC (ROPE) — Entrenched elites, historical privilege holders, and incumbent power structures benefit from the supermajority threshold as a coordination mechanism: it coordinates their defense against majoritarian change without requiring constant active mobilization. The threshold's enforcement is structural (embedded in constitutional text) rather than performative. Pure coordination from this perspective — the constraint solves the collective action problem of defending existing distributions.
constraint_indexing:constraint_classification(supermajority_threshold__minoritarian_veto_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: TRANSITIONAL SWING MINORITY (TANGLED ROPE) — Agents positioned near the supermajority threshold (at 55-65% support for an amendment) experience the constraint as mixed. They face genuine coordination problems: supermajority thresholds protect minority rights against fleeting majoritarian impulses, and some protection is functional. But they also face extraction: their structural position as swing voters gives entrenched minorities disproportionate power to block reforms even when support is well above 50%. Constrained exit — leaving the polity, exit is high-cost; but swing position offers some agency.
constraint_indexing:constraint_classification(supermajority_threshold__minoritarian_veto_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / CONSENSUS SAFEGUARD VIEW (MOUNTAIN) — From a civilizational perspective emphasizing constitutional stability, supermajority thresholds appear as natural law: stable constitutions require supramajority support for radical change to prevent tyranny of temporary majorities. The threshold appears immutable — built into the logic of federalism and constitutional design itself. DIAGNOSTIC: This perspective is a false summit candidate. The structural data (identifiable beneficiaries, clear extraction flow, active enforcement requirement) contradicts the natural-law framing.
constraint_indexing:constraint_classification(supermajority_threshold__minoritarian_veto_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: HISTORICAL JUSTIFICATION APPARATUS (PITON) — The original constitutional framers' justification (protecting minority rights, preventing tyranny of majorities, ensuring deliberation) persists as a legitimacy narrative even when the threshold has become a mechanism for entrenching historical privilege rather than protecting genuine minority rights. Theater ratio is low (the mechanism is not performative — it actually blocks reform), but the institutional narrative maintaining the threshold's legitimacy is degraded: institutions invoke 'minority protection' as cover for veto of majoritarian reform. Inertial persistence of original justification despite changed function.
constraint_indexing:constraint_classification(supermajority_threshold__minoritarian_veto_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: SUCCESSOR MAJORITIES ACROSS GENERATIONS (SNARE) — Even organized reform coalitions that achieve 55-60% support face the supermajority barrier. Unlike individual powerless agents, they have organizational capacity, but the structural constraint is extractive: successive majorities across generations are blocked by constitutional rules authored by coalitions that no longer command support. The constraint accumulates extraction across time as each new majoritarian impulse for reform is blocked. Organized but structurally defeated — the high suppression reflects that overcoming the threshold requires not just majority mobilization but sustained, increasing supermajorities.
constraint_indexing:constraint_classification(supermajority_threshold__minoritarian_veto_reading, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supermajority_threshold__minoritarian_veto_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(supermajority_threshold__minoritarian_veto_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(supermajority_threshold__minoritarian_veto_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(supermajority_threshold__minoritarian_veto_reading, TR),
    TR >= 0.70.

:- end_tests(supermajority_threshold__minoritarian_veto_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and accumulating. Base extractiveness measures the asymmetry between beneficiary and victim positions. At founding (t=0), extractiveness is moderate (0.35) because the threshold genuinely coordinates among multiple competing factions and protects against some tyranny risks. But as the founding coalition's privilege consolidates and becomes entrenched across generations (t=100), the same threshold mechanism increasingly functions to block majoritarian reform rather than to protect legitimate minority interests. Contemporary value (0.68) reflects that the threshold now blocks broad-based majoritarian preferences on major issues (voting rights, campaign finance, economic policy) while protecting a narrower beneficiary bloc. The upward trajectory is crucial: supermajority thresholds exhibit extraction_accumulation — a core diagnostic for degraded institutional function. Suppression (0.72): High. Measured by the cost imposed on reform constituencies to overcome the threshold. Requires not just majority coalition-building but a supermajority exceeding 66%, which means sustained support that can survive political cycles. Suppression includes: blocking of widely-supported reforms, blocking of necessary constitutional evolution in response to changed conditions, concentration of power in 33%+ blocking minorities. Suppression has accumulated as polity has become more pluralistic and diverse — coalitions that author constitutions are smaller and less representative than contemporary majorities. Theater ratio (0.35): Low-to-moderate. The supermajority threshold is not primarily performative — it actually blocks amendment and functions as an effective veto mechanism. Low theater means the mechanism is structurally real, not theater. The slight increase over time (0.25 → 0.35) reflects modest increase in performative elements: increasingly frequent invocation of 'minority protection' narrative even as the threshold's actual function is entrenchment.
 *
 * PERSPECTIVAL GAP:
 *   The gap between beneficiary and victim perspectives is maximum: one experiences pure coordination, the other pure extraction, from the identical structural mechanism. The analytical observer's mountain classification is a false summit — the threshold is not an immutable law but a contingent constitutional design. The piton perspective reveals degraded legitimacy narratives. The tangled rope and organized snare perspectives show how mixed and compounded extraction emerges at different organizational scales.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from the agent's structural relationship to the extraction flow. Status quo beneficiaries have d ≈ 0.10 (full beneficiaries with arbitrage options to exit if necessary — they can exit by accepting majoritarian change, but choose not to because they benefit from the status quo). Reform majorities have d ≈ 0.95 (full targets with no exit option — they cannot withdraw from the polity, cannot override the supermajority requirement, cannot change rules without the supermajority they lack). Swing minorities have d ≈ 0.55 (symmetric: they benefit from minority protection of their own interests while also bearing costs from blocked reforms that would benefit broader coalitions they partially belong to). The sigmoid f(d) converts these directionality values to effective power modifiers. Beneficiaries with low d experience negative or minimal χ; trapped majorities with high d experience χ ≈ 1.42 (powerless level). The accumulated extraction across generations is modeled through the measurements: as suppression increases and extractiveness accumulates, the derived d values for subsequent generations shift upward (trapped minorities become more trapped) while beneficiary d values remain stable (entrenchment is self-reinforcing).
 *
 * MANDATROPHY ANALYSIS:
 *   The minoritarian_veto_reading resolves mandatrophy by showing that the supermajority threshold IS a snare, not a coordination mechanism, when measured from the structural perspective of contemporary majorities blocked from constitutional reform. The mandatrophy is not 'is it coordination or extraction?' but rather 'for whom does it coordinate, and whom does it extract from?' The threshold coordinates the defense of existing distributions (Rope from beneficiary perspective) while extracting from those seeking reform (Snare from majority perspective). The false summit in the analytical observer's perspective is essential: the 'constitutional stability' framing naturalizes what is actually a choice to prioritize entrenchment of existing power over majoritarian responsiveness. Resolving mandatrophy requires acknowledging that the same mechanism is both genuinely functional (prevents some forms of tyranny) AND genuinely extractive (blocks majoritarian will across generations). The reading declares that extraction dominates: the threshold is primarily a snare dressed in coordination language.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_genesis_intent_vs_function_drift,
    'Was the supermajority threshold originally intended to protect discrete minority rights (e.g., religious minorities, small states), or was it designed to entrench the coalition that authored the constitution (historical privilege holders)?',
    'Framers'' intent analysis (Madison, Federalist papers); comparison of stated purpose vs. actual beneficiary bloc over constitutional history; analysis of which minorities the threshold has actually protected vs. which coalitions have used it to block reform',
    'If original intent was discrete minority protection: threshold may be legitimately functional (Rope from consensus perspective, not Snare). If original intent was entrenchment: threshold is snare by design, and ''minority protection'' framing is false summit. If both: threshold has drifted from protection to entrenchment over time as original minority constituencies gained power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threshold_genesis_intent_vs_function_drift, empirical, 'Whether supermajority threshold was designed for minority protection or entrenchment of founding coalition').

omega_variable(
    majoritarian_impulse_stability_empirics,
    'How often do documented majoritarian impulses for constitutional amendment (>50% sustained support) represent fleeting preferences vs. stable reform demands?',
    'Longitudinal polling data on proposed amendments; historical tracking of public opinion on specific reform proposals (voting rights, campaign finance, economic policy) over decades; analysis of which blocked reforms have later achieved supermajority support vs. which have receded',
    'If majorities are typically fleeting: supermajority threshold protects against temporal tyranny (Rope/Tangled Rope perspectives more defensible). If majorities are typically stable: blocking 55-65% supermajorities represents pure extraction (Snare classification robust). If pattern is mixed: threshold''s function depends on domain (minority rights vs. economic redistribution may differ in temporal stability).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_impulse_stability_empirics, empirical, 'Temporal stability of documented majoritarian preferences for constitutional reform').

omega_variable(
    alternative_constitutional_threshold_comparison,
    'Jurisdictions using simple majority or different supermajority thresholds (e.g., 60%, 55%) — do they show different patterns of minority protection vs. majority entrenchment compared to the 66% threshold?',
    'Comparative institutional analysis: cross-national and cross-state data on amendment success rates, minority protection track record, and reform lag by threshold level; analysis of whether lower thresholds correlate with more frequent reform or with degraded minority protection',
    'If lower thresholds show similar minority protection with higher reform rates: 66% threshold is extractive choice, not necessary protection (Snare reading robust). If lower thresholds show degraded minority protection: threshold level is functionally justified (Rope reading more defensible). Threshold sensitivity analysis would reveal whether minority protection flattens after 55% or requires 66%.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_constitutional_threshold_comparison, empirical, 'Comparative institutional outcomes across different constitutional amendment thresholds').

omega_variable(
    entrenchment_coalition_demographic_evolution,
    'As the original constitutional coalition''s demographic composition changes (e.g., descendant minorities gaining power, new ethnic majorities emerging), does the beneficiary bloc of the supermajority threshold shift, or does the threshold remain locked to the original beneficiary distribution?',
    'Demographic tracking of which groups have used supermajority threshold as a veto against reform over time; analysis of whether descendant minorities of original beneficiary coalition now block reforms affecting their interests; comparison of beneficiary coalitions at constitution''s founding vs. contemporary moment',
    'If beneficiary bloc shifts with demographic change: threshold remains functional at protecting contemporary minorities (Rope reading). If beneficiary bloc locks to historical privilege even as power distributions change: threshold is pure entrenchment mechanism (Snare reading robust). If pattern is mixed (some beneficiaries shift, some lock): threshold enables entrenchment by earlier coalitions while creating new veto power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entrenchment_coalition_demographic_evolution, empirical, 'Whether supermajority threshold beneficiary coalition shifts with demographic evolution').

omega_variable(
    reading_committer_ambiguity,
    'This is ONE READING of the supermajority_threshold kernel: the minoritarian_veto_reading. The sibling readings (consensus_safeguard_reading, adaptive_gradient_reading) advance different structural interpretations. Which reading''s core premise should govern if all three cannot be simultaneously true within a single constitutional framework?',
    'Framing resolution: empirical evidence on threshold function (does it protect minorities or entrench status quo?) guides which reading''s foundational axiom is operative. But readings may coexist across different parties'' commitments — consensus framers hold safeguard reading; reform movements hold veto reading; designers seeking optimization hold gradient reading. Resolution requires adjudication at the level of political power, not logical necessity.',
    'If minoritarian_veto_reading forecloses consensus_safeguard_reading: amendment authority structure is fundamentally delegitimated (requires constitutional replacement or revolution). If readings coexist: threshold persists despite contested legitimacy (current structural state). If adaptive_gradient_reading forecloses both: threshold can be made functionally responsive to contingent historical conditions (reform path exists).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_committer_ambiguity, preference, 'Committer-frame ambiguity: which reading''s axioms should govern the supermajority threshold''s legitimacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__minoritarian_veto_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(smaj_theater_founding, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(smaj_theater_mid, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement(smaj_theater_contemporary, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 100, 0.35).

% Extraction over time
narrative_ontology:measurement(smaj_base_extract_founding, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(smaj_base_extract_mid, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 50, 0.55).
narrative_ontology:measurement(smaj_base_extract_contemporary, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(smaj_suppression_founding, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(smaj_suppression_mid, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 50, 0.58).
narrative_ontology:measurement(smaj_suppression_contemporary, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 100, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__minoritarian_veto_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold__consensus_safeguard_reading).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold__adaptive_gradient_reading).

% DUAL FORMULATION NOTE:
% The supermajority_threshold is a single kernel read by three structurally distinct constraint stories. Each reading instantiates a different ε and classification: consensus_safeguard_reading (ε ≈ 0.20, Rope) emphasizes coordination; minoritarian_veto_reading (ε ≈ 0.68, Snare) emphasizes extraction; adaptive_gradient_reading (ε ≈ 0.40, Tangled Rope) emphasizes design flexibility. These are not alternative measurements of the same constraint but three genuinely distinct claims about what function the threshold performs in constitutional reality. The network edges record which reading influences or constrains which others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(supermajority_threshold__minoritarian_veto_reading, institutional, 0.08).
constraint_indexing:directionality_override(supermajority_threshold__minoritarian_veto_reading, powerless, 0.93).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
