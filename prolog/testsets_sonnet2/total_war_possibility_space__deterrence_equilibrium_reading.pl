% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__deterrence_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__deterrence_equilibrium_reading, []).

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
 *   constraint_id: total_war_possibility_space__deterrence_equilibrium_reading
 *   human_readable: Nuclear Deterrence Equilibrium (Total War Remains Reachable, Deterred by Mutual Vulnerability)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This story instantiates the deterrence-equilibrium reading of the
 *   total-war-possibility-space kernel: total war has never left the space of
 *   strategically reachable outcomes; what prevents it is the continuous,
 *   costly maintenance of mutual second-strike vulnerability, not a normative
 *   prohibition (nuclear_taboo_reading) and not a cognitive removal of the
 *   option from thinkability (space_contraction_reading). Under this reading,
 *   doctrine development, counterforce targeting, and escalation-ladder
 *   theorization are not vestigial theater but the load-bearing mechanism
 *   itself — war-fighting capability IS the deterrent signal. The ε referent
 *   is the standing arrangement of maintained mutual-vulnerability deterrence
 *   as this reading itself understands it, not a disarmed alternative.
 *
 * KEY AGENTS:
 *   - nuclear_weapons_states: agenda_setter/beneficiary (institutional/arbitrage) — set doctrine, capture strategic standing
 *   - defense_industrial_base: beneficiary (organized/arbitrage) — captures modernization spending
 *   - strategic_studies_establishment: beneficiary/agenda_setter (institutional/arbitrage) — produces the doctrine that legitimizes continued investment
 *   - domestic_taxpayers_of_nuclear_states and populations_within_counterforce_target_zones: payer (powerless/trapped) — bear cost and physical risk with no doctrinal voice
 *   - non_nuclear_states: payer (moderate/trapped) — bear strategic subordination
 *   - policy_historians: observer (analytical) — assess whether the equilibrium framing is descriptively accurate or self-serving retrospection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__deterrence_equilibrium_reading, 0.58).
domain_priors:suppression_score(total_war_possibility_space__deterrence_equilibrium_reading, 0.44).
domain_priors:theater_ratio(total_war_possibility_space__deterrence_equilibrium_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__deterrence_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__deterrence_equilibrium_reading, "Nuclear Deterrence Equilibrium (Total War Remains Reachable, Deterred by Mutual Vulnerability)").
narrative_ontology:topic_domain(total_war_possibility_space__deterrence_equilibrium_reading, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_possibility_space__deterrence_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__deterrence_equilibrium_reading, '77be91d0-3a80-4a43-b539-2152812d4d85').
narrative_ontology:cs_kernel_codification('77be91d0-3a80-4a43-b539-2152812d4d85', distributed).
narrative_ontology:cs_authority_grounding('77be91d0-3a80-4a43-b539-2152812d4d85', distributed).
narrative_ontology:cs_reading_relation('77be91d0-3a80-4a43-b539-2152812d4d85', total_war_possibility_space__space_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('77be91d0-3a80-4a43-b539-2152812d4d85', total_war_possibility_space__nuclear_taboo_reading, coexists_with).
narrative_ontology:cs_axiom('77be91d0-3a80-4a43-b539-2152812d4d85', foundational, war_remains_in_active_planning_space).
narrative_ontology:cs_axiom_status(war_remains_in_active_planning_space, holdable).
narrative_ontology:cs_axiom_grounding('77be91d0-3a80-4a43-b539-2152812d4d85', war_remains_in_active_planning_space, empirically_contingent).
narrative_ontology:cs_axiom('77be91d0-3a80-4a43-b539-2152812d4d85', foundational, credible_warfighting_capability_is_the_deterrent_mechanism).
narrative_ontology:cs_axiom_status(credible_warfighting_capability_is_the_deterrent_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('77be91d0-3a80-4a43-b539-2152812d4d85', credible_warfighting_capability_is_the_deterrent_mechanism, instrumental).
narrative_ontology:cs_reference_frame('77be91d0-3a80-4a43-b539-2152812d4d85', mutual_assured_destruction_stability).
narrative_ontology:cs_drift_state('77be91d0-3a80-4a43-b539-2152812d4d85', post_cold_war_multipolar_proliferation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('77be91d0-3a80-4a43-b539-2152812d4d85', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_weapons_states).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, defense_industrial_base).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, strategic_studies_establishment).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, non_nuclear_states).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, domestic_taxpayers_of_nuclear_states).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, populations_within_counterforce_target_zones).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and modernize arsenals, doctrine, and delivery systems as the deterrent signal the reading treats as load-bearing. They set escalation ladders, targeting doctrine, and declaratory policy, and their standing in the international order is partly constituted by possessing this capability. They can adjust posture unilaterally within broad limits and bear none of the downside of the arrangement's persistence.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_weapons_states, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_weapons_states, beneficiary).

% Supplies the continuous modernization cycle the deterrence-equilibrium reading requires as evidence of credible resolve — new delivery platforms, warhead life-extension programs, command-and-control upgrades. Revenue depends on the reading remaining institutionally dominant over rival readings that would treat the arsenal as sufficient or excessive.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, defense_industrial_base, beneficiary,
    organized, generational, arbitrage, national).

% Produces the escalation-ladder theorization, counterforce targeting doctrine, and cost-benefit calculus that gives the deterrence-equilibrium reading its intellectual infrastructure. Careers, journals, and government advisory seats are built on treating total war as a live strategic possibility requiring continuous modeling.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, strategic_studies_establishment, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__deterrence_equilibrium_reading, strategic_studies_establishment, agenda_setter).

% Live inside a system where nuclear possession by others confers strategic weight they cannot match through conventional means. They fund extended-deterrence arrangements, tolerate basing agreements, or seek their own weapons — all costs of a possibility space they did not choose to keep open. Exit means either proliferation (further destabilizing) or permanent subordination in the strategic hierarchy.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, non_nuclear_states, payer,
    moderate, generational, trapped, global).

% Fund multi-decade modernization programs costing hundreds of billions, justified by the deterrence-equilibrium logic that credibility requires continuous investment. They have no direct voice in doctrine or targeting decisions and cannot opt out of the tax base that sustains the arsenal.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, domestic_taxpayers_of_nuclear_states, payer,
    powerless, biographical, trapped, national).

% Live near silo fields, command bunkers, or military-industrial installations that counterforce doctrine designates as targets precisely because the war-fighting capability is maintained as a credible option, not merely a rhetorical one. Their exposure is a direct structural consequence of keeping total war strategically reachable rather than foreclosed.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, populations_within_counterforce_target_zones, payer,
    powerless, civilizational, trapped, regional).

% Would press for verified reductions or no-first-use doctrines that treat the possibility space as something to be narrowed, not maintained as a deterrent signal. Their proposals are heard but structurally subordinated whenever modernization budgets and doctrine reviews are decided by the states holding the arsenals.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, arms_control_negotiators, excluded,
    organized, generational, constrained, global).

% Study the record of near-misses, doctrine shifts, and crisis behavior to assess whether the deterrence-equilibrium framing accurately describes what keeps total war from occurring, or whether it retrospectively rationalizes outcomes that had other causes.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, policy_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides mutual second-strike assurance among nuclear-armed states, in principle preventing any single actor from calculating that a first strike is survivable or advantageous — a genuine collective-action solution to the security dilemma among peer nuclear powers.
% TRANSFER_FUNCTION: Moves enormous recurring budgetary resources from general publics to arsenal modernization and strategic-doctrine production, and moves strategic autonomy from non-nuclear states to nuclear-armed ones, in exchange for the claimed stability the arrangement provides.
% ABSENT_VOICES: Populations living near targeted installations and non-nuclear states bearing the coercive weight of the strategic hierarchy are not parties to doctrine reviews or modernization decisions; arms control advocates are consulted but hold no veto over force posture.
% DISAPPEARANCE_RATIONALE: If mutual-vulnerability deterrence doctrine and the war-fighting capability it requires vanished overnight, defense budgets would collapse, strategic hierarchies among states would flatten or reorganize around conventional or economic power, extended-deterrence alliance structures would need renegotiation, and the entire escalation-ladder research apparatus would lose its object of study.
% FOUNDING_PROBLEM: Following the development of thermonuclear weapons, strategists needed a framework to prevent any nuclear-armed state from concluding that striking first was rational, given that neither side could disarm the other's retaliatory capacity.
% FOUNDING_PROBLEM_CORROBORATION: Strategic studies scholars within the establishment attest the problem remains live given ongoing arsenal modernization by multiple states. Independent arms-control researchers and historians outside the beneficiary set (e.g., disarmament-movement scholarship, declassified crisis-management archives) argue the marginal deterrent value of continued modernization beyond existing second-strike capacity is unestablished, and that the doctrine now sustains itself partly through institutional momentum rather than demonstrated necessity.
narrative_ontology:disappearance_verdict(total_war_possibility_space__deterrence_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__deterrence_equilibrium_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__deterrence_equilibrium_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_possibility_space__deterrence_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__deterrence_equilibrium_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__deterrence_equilibrium_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_possibility_space__deterrence_equilibrium_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_possibility_space__deterrence_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that this reading legitimates a real, ongoing resource transfer (modernization budgets, strategic subordination of non-nuclear states) as the necessary price of stability, while producing genuine coordination value (mutual second-strike assurance reduces first-strike incentive). Suppression (0.44) is moderate: the arrangement is maintained through alliance structures, extended-deterrence commitments, and doctrinal consensus-enforcement within strategic establishments, rather than through direct coercion of dissenting populations — though populations near target zones and non-nuclear states have essentially no exit. Theater ratio (0.4) is meaningfully high under this reading precisely because the reading's own logic requires visible investment (new platforms, doctrine reviews, exercises) as the deterrent signal itself, blurring the line between necessary demonstration and self-perpetuating institutional theater. Accessibility collapse (0.35) is moderate-low: alternative security arrangements (arms control regimes, no-first-use pledges, conventional deterrence) remain discussed and partially implemented, so the possibility space has not fully closed around this arrangement. Resistance (0.5) reflects substantial and organized arms-control opposition that has achieved real treaty successes (INF, New START) without dislodging the underlying doctrine.
 *
 * PERSPECTIVAL GAP:
 *   From the nuclear-weapons-states seat, this looks like prudent, load-bearing coordination — the arsenal IS the peace. From the powerless-payer seats (taxpayers, target-zone populations), the same structure computes as an imposed, unconsentable risk-and-cost burden sustained by institutions with no accountability to them. The engine should register this divergence structurally rather than resolve it in either direction.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapons states and their strategic establishments sit near the beneficiary end: they set the doctrine, control modernization timelines, and derive strategic standing from arsenal possession — d low. Defense industrial actors similarly benefit from continuous investment cycles the doctrine mandates. Domestic taxpayers and populations near target installations sit near the target end: they fund and physically bear the arrangement with no meaningful exit (trapped) and no doctrinal voice. Non-nuclear states occupy an intermediate-high d: they are structurally subordinated but retain some strategic agency (alliance-seeking, latent proliferation option) that a fully trapped agent would lack.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing rational first-strike calculation between peer nuclear powers) plausibly remains partially live — mutual vulnerability is a real deterrent mechanism under this reading's own premises. But the founding_problem_status is authored as contested because the corroboration is split: the benefiting institutions (states, strategic studies establishment) assert continuous necessity, while independent arms-control scholarship argues that marginal modernization beyond existing second-strike sufficiency serves institutional and industrial momentum rather than demonstrated deterrent need. Classifying this as tangled_rope rather than snare or mountain preserves the genuine coordination function (mutual deterrence lowering first-strike incentive) while registering the asymmetric extraction (budgetary transfer, strategic subordination, unconsented physical risk) that rides on it — collapsing this into pure extraction would erase the real security-dilemma logic; collapsing it into a mountain would erase the identifiable beneficiaries and the doctrinal choice involved in maintaining, rather than reducing, the arsenal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_versus_taboo_causal_priority,
    'Is the absence of total war since 1945 better explained by the material deterrence-equilibrium mechanism (mutual vulnerability, credible war-fighting capability) or by a normatively constructed taboo against nuclear use that operates independent of capability levels?',
    'Comparative case analysis of crisis behavior (Cuban Missile Crisis, Able Archer, India-Pakistan crises) examining whether decision-makers'' stated reasoning centered on capability-based cost-benefit calculation or on normative/taboo-based reasoning; declassified deliberation records are the primary evidence class.',
    'If taboo-based reasoning dominates decision records, this reading''s claim that continuous war-fighting investment is the operative deterrent mechanism weakens substantially, supporting reclassification of much of the modernization apparatus as theater rather than functional deterrent signal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_versus_taboo_causal_priority, empirical, 'Committer ambiguity: which kernel reading (deterrence_equilibrium vs nuclear_taboo) better explains observed restraint.').

omega_variable(
    reachability_versus_thinkability,
    'Is total war genuinely reachable as a strategic outcome (this reading''s premise) or has it been removed from the space of options that decision-makers can coherently entertain, such that ''deterrence'' is not actively preventing a live option but rather policing an option that no longer exists in practice (the space_contraction_reading''s premise)?',
    'Elicitation studies and doctrine-document analysis of current strategic planners: do escalation-ladder exercises and counterforce targeting reviews reflect genuine contingency planning for a reachable outcome, or ritualized maintenance of a framework whose object has become strategically unthinkable?',
    'If total war is no longer genuinely reachable, the continuous investment this reading treats as necessary deterrent signaling would be reclassified as institutional theater maintaining a possibility space that has already effectively closed — shifting this constraint toward piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reachability_versus_thinkability, conceptual, 'Committer ambiguity: located at the reachability premise distinguishing this reading from space_contraction_reading.').

omega_variable(
    modernization_marginal_necessity,
    'Does continued arsenal modernization beyond existing assured second-strike capacity provide additional deterrent value, or does it exceed what mutual-vulnerability logic requires and instead serve defense-industrial and institutional interests?',
    'Independent strategic-stability modeling comparing deterrent sufficiency thresholds against actual modernization spending trajectories; audit of defense-industrial lobbying influence on doctrine review processes.',
    'If modernization spending substantially exceeds deterrent sufficiency, the beneficiary asymmetry (defense industrial base, strategic studies establishment) strengthens relative to the coordination function, pushing the classification further from tangled_rope toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(modernization_marginal_necessity, empirical, 'Whether current investment levels reflect deterrent necessity or captured institutional interest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__deterrence_equilibrium_reading, 1949, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1949, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1949, 0.2).
narrative_ontology:measurement(tota_tr_t1962, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1962, 0.25).
narrative_ontology:measurement(tota_tr_t1979, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1979, 0.32).
narrative_ontology:measurement(tota_tr_t1991, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1991, 0.38).
narrative_ontology:measurement(tota_tr_t2008, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 2008, 0.36).
narrative_ontology:measurement(tota_tr_t2025, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(tota_be_t1949, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1949, 0.35).
narrative_ontology:measurement(tota_be_t1962, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1962, 0.5).
narrative_ontology:measurement(tota_be_t1979, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1979, 0.55).
narrative_ontology:measurement(tota_be_t1991, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1991, 0.45).
narrative_ontology:measurement(tota_be_t2008, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 2008, 0.5).
narrative_ontology:measurement(tota_be_t2025, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1949, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1949, 0.3).
narrative_ontology:measurement(tota_su_t1962, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1962, 0.55).
narrative_ontology:measurement(tota_su_t1979, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1979, 0.5).
narrative_ontology:measurement(tota_su_t1991, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1991, 0.35).
narrative_ontology:measurement(tota_su_t2008, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 2008, 0.38).
narrative_ontology:measurement(tota_su_t2025, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 2025, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__deterrence_equilibrium_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_possibility_space__deterrence_equilibrium_reading, 0.12).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_taboo_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, space_contraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings decomposing the natural-language 'total war possibility space' kernel per the ε-invariance principle: deterrence_equilibrium_reading (this file, tangled_rope — mutual vulnerability as active coordination-plus-extraction mechanism), nuclear_taboo_reading (normative prohibition independent of capability), and space_contraction_reading (removal from strategic thinkability, not merely preferability). The three share a persisting empirical referent (no total war among nuclear powers since 1945) but assign structurally different causal mechanisms, different ε values, and different beneficiary/victim structures, so they are authored as three separate constraint stories linked via affects_constraints rather than as one story with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
