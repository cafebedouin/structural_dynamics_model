% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__graduated_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__graduated_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__graduated_sovereignty
 *   human_readable: Graduated Sovereignty Doctrine (Capacity/Legitimacy-Indexed Sovereignty)
 *   domain: international_law/political_philosophy/global_governance
 *
 * SUMMARY:
 *   This story instantiates the 'graduated_sovereignty' reading of the
 *   Westphalian sovereignty kernel: sovereignty is treated not as a binary
 *   status but as a spectrum whose position for any given state is determined
 *   by external assessment of that state's governance capacity and
 *   legitimacy. Unlike the absolute_sovereignty reading (unconditional
 *   domestic authority, external interference categorically illegitimate) or
 *   the conditional_sovereignty reading (sovereignty survives intact unless
 *   systematic rights violations trigger a specific, bounded intervention
 *   threshold), the graduated reading installs a continuous,
 *   externally-administered scale that determines, at any moment, how much
 *   practical sovereign discretion a state is treated as retaining. This is
 *   the reading with the widest discretionary latitude for external
 *   classifiers, and the story authors it as substantially extractive: the
 *   same capacity/legitimacy vocabulary that could calibrate genuine
 *   assistance also functions as a standing mechanism by which dominant
 *   states and institutions can recharacterize disfavored governments as
 *   sovereignty-deficient to justify conditionality, oversight, or
 *   intervention without meeting either the absolute reading's bar (never) or
 *   the conditional reading's bar (systematic rights violations).
 *
 * KEY AGENTS:
 *   - dominant_states_and_blocs: agenda_setter (institutional/arbitrage) — set and apply classification criteria
 *   - international_financial_institutions: agenda_setter/beneficiary (institutional/arbitrage) — operationalize the spectrum through conditionality
 *   - fragile_and_postcolonial_states: payer (moderate/constrained) — bear the classification's practical consequences
 *   - populations_of_reclassified_states: payer (powerless/trapped) — live under resulting policy without consent
 *   - non_aligned_governments_seeking_alternative_development_paths: payer/excluded (moderate/constrained) — penalized for deviating from the template used to define legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, 0.62).
domain_priors:suppression_score(westphalian_sovereignty__graduated_sovereignty, 0.58).
domain_priors:theater_ratio(westphalian_sovereignty__graduated_sovereignty, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, extractiveness, 0.62).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__graduated_sovereignty, snare).
narrative_ontology:human_readable(westphalian_sovereignty__graduated_sovereignty, "Graduated Sovereignty Doctrine (Capacity/Legitimacy-Indexed Sovereignty)").
narrative_ontology:topic_domain(westphalian_sovereignty__graduated_sovereignty, "international_law/political_philosophy/global_governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__graduated_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__graduated_sovereignty, '7d3e3363-4e72-47a1-9d82-e56312c86056').
narrative_ontology:cs_kernel_codification('7d3e3363-4e72-47a1-9d82-e56312c86056', distributed).
narrative_ontology:cs_authority_grounding('7d3e3363-4e72-47a1-9d82-e56312c86056', extraction).
narrative_ontology:cs_interpretation_layer_present('7d3e3363-4e72-47a1-9d82-e56312c86056').
narrative_ontology:cs_reading_relation('7d3e3363-4e72-47a1-9d82-e56312c86056', westphalian_sovereignty__absolute_sovereignty, forecloses).
narrative_ontology:cs_reading_relation('7d3e3363-4e72-47a1-9d82-e56312c86056', westphalian_sovereignty__conditional_sovereignty, influences).
narrative_ontology:cs_axiom('7d3e3363-4e72-47a1-9d82-e56312c86056', foundational, sovereignty_is_continuously_scalar_not_binary).
narrative_ontology:cs_axiom_status(sovereignty_is_continuously_scalar_not_binary, holdable).
narrative_ontology:cs_axiom_grounding('7d3e3363-4e72-47a1-9d82-e56312c86056', sovereignty_is_continuously_scalar_not_binary, conventional).
narrative_ontology:cs_axiom('7d3e3363-4e72-47a1-9d82-e56312c86056', foundational, external_capacity_assessment_is_a_legitimate_basis_for_calibrating_sovereign_standing).
narrative_ontology:cs_axiom_status(external_capacity_assessment_is_a_legitimate_basis_for_calibrating_sovereign_standing, holdable).
narrative_ontology:cs_axiom_grounding('7d3e3363-4e72-47a1-9d82-e56312c86056', external_capacity_assessment_is_a_legitimate_basis_for_calibrating_sovereign_standing, instrumental).
narrative_ontology:cs_reference_frame('7d3e3363-4e72-47a1-9d82-e56312c86056', post_cold_war_state_collapse_calibration).
narrative_ontology:cs_drift_state('7d3e3363-4e72-47a1-9d82-e56312c86056', contemporary_governance_indicator_regime, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7d3e3363-4e72-47a1-9d82-e56312c86056', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, dominant_states_and_blocs).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, international_financial_institutions).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, intervention_contractors_and_consultants).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, fragile_and_postcolonial_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, populations_of_reclassified_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, non_aligned_governments_seeking_alternative_development_paths).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, well_governed_small_states).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__graduated_sovereignty, capacity_based_sovereignty_gradation).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__graduated_sovereignty, legitimacy_as_precondition_for_full_sovereign_standing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and apply the criteria by which a state's 'capacity' and 'governance legitimacy' are assessed, through bilateral leverage, seats on multilateral bodies, credit-rating influence, and control of aid conditionality. They classify other states along the sovereignty spectrum and adjust their own conduct toward those states accordingly, while facing no reciprocal classification of their own governance quality by outside parties.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, dominant_states_and_blocs, agenda_setter,
    institutional, generational, arbitrage, global).

% Operationalize the capacity/legitimacy spectrum through governance indicators, conditionality frameworks, and technical assistance programs that determine loan terms and market access. Their assessments feed directly into how much sovereign discretion a borrowing state is treated as retaining.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, international_financial_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__graduated_sovereignty, international_financial_institutions, beneficiary).

% Supply the capacity-building missions, governance audits, security-sector reform programs, and technical advisory contracts that graduated sovereignty generates as a matter of course once a state is classified as capacity-deficient. Their revenue depends on the classification apparatus persisting and expanding.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, intervention_contractors_and_consultants, beneficiary,
    organized, biographical, mobile, global).

% Are assessed against externally-defined capacity and legitimacy benchmarks that determine how much of their domestic authority is recognized as inviolable versus subject to trusteeship-like oversight, conditional lending, or supervised governance reform. Contesting the classification risks credit downgrades, aid suspension, or diplomatic isolation; accepting it risks entrenched external tutelage.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, fragile_and_postcolonial_states, payer,
    moderate, generational, constrained, national).

% Live under domestic policy shaped by conditionalities imposed once their state is graded low on the sovereignty spectrum — austerity terms, security-sector restructuring, or supervised elections — without having consented to or participated in the classification process that produced those terms.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, populations_of_reclassified_states, payer,
    powerless, biographical, trapped, national).

% Attempt development or governance models outside the dominant template (state-led planning, alternative political structures, regional currency arrangements) and find these models scored as evidence of low legitimacy or capacity, narrowing their access to trade, credit, and diplomatic recognition regardless of demonstrated domestic effectiveness.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, non_aligned_governments_seeking_alternative_development_paths, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__graduated_sovereignty, non_aligned_governments_seeking_alternative_development_paths, excluded).

% Track how capacity/legitimacy assessments are applied across states, documenting whether the criteria are applied consistently or selectively. Their findings could either legitimate or undermine the graduated framework depending on what the record shows.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, human_rights_and_governance_monitoring_bodies, observer,
    organized, generational, analytical, global).

% States that score well on conventional capacity/legitimacy metrics (stable institutions, rule-of-law indices, market-friendly governance) receive fuller practical sovereignty recognition than the Westphalian baseline would guarantee them on population or military-power grounds alone — the spectrum can reward some who were previously disadvantaged by raw-power comparisons.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, well_governed_small_states, beneficiary,
    moderate, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalian_sovereignty__graduated_sovereignty, dominant_states_and_blocs).
narrative_ontology:fixing_cost_class(westphalian_sovereignty__graduated_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides international actors a shared vocabulary for calibrating engagement with weak or fragile states — distinguishing genuine coordination needs (post-conflict reconstruction support, technical assistance requested by a functioning government) from cases where blanket non-interference would abandon a population to state collapse.
% TRANSFER_FUNCTION: Moves practical sovereign discretion — control over domestic policy, borrowing terms, security-sector design, electoral processes — from states classified as capacity- or legitimacy-deficient to the external actors and institutions empowered to assess and act on that classification, along with the associated advisory, lending, and monitoring revenue.
% ABSENT_VOICES: Populations of reclassified states and non-aligned governments testing alternative governance models have no seat in defining what counts as 'capacity' or 'legitimacy'; the criteria are set by the same dominant states and institutions that benefit from applying them, and no comparable graduated framework is proposed to assess the governance quality of the classifying powers themselves.
% DISAPPEARANCE_RATIONALE: If the graduated sovereignty framework vanished, conditional lending, security-sector reform contracting, and governance-indicator-driven diplomacy would lose their doctrinal justification; states currently scored as capacity-deficient would face fewer externally-imposed policy conditions, and the consulting/monitoring industry built around capacity assessment would contract sharply.
% FOUNDING_PROBLEM: Post-Cold War state collapse (Somalia, Rwanda, the Balkans) exposed cases where treating all states as equally sovereign regardless of actual governing capacity left populations without protection during genuine institutional failure, and left international actors without a principled basis for calibrated engagement short of full intervention or total abstention.
% FOUNDING_PROBLEM_CORROBORATION: Development economists and some post-conflict governance scholars outside the intervening institutions corroborate that genuine capacity gaps existed in specific collapsed-state cases in the 1990s. However, scholars of the Global South governance-indicator literature (e.g., critiques of World Governance Indicators methodology) and diplomats from graduated states attest, from outside the beneficiary set, that the framework has since been applied far beyond genuine collapse cases to routinely discipline states pursuing non-aligned development paths, well past any case where institutional collapse was actually present.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__graduated_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__graduated_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__graduated_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(westphalian_sovereignty__graduated_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__graduated_sovereignty, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__graduated_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__graduated_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored in the mid-high range (0.62 at interval end) reflecting the reading's core mechanism: classification discretion sits with the same actors who benefit from downgrading a target state's practical sovereignty. Suppression (0.58) is lower than extraction because the mechanism operates more through conditionality, market access, and diplomatic leverage than through direct coercive force — though it hardens over time as governance-indicator regimes become institutionalized. Theater ratio rises from 0.22 to 0.40 across the interval, reflecting a documented pattern: capacity-assessment and legitimacy-monitoring infrastructure increasingly performs technical neutrality (indices, scorecards, peer-review missions) whose actual predictive and remedial value has not kept pace with its proliferation. All three series share the single time grid required by the story.
 *
 * DIRECTIONALITY LOGIC:
 *   Dominant states and IFIs sit at the beneficiary end: they set criteria, are never themselves classified, and capture the diplomatic and financial leverage the spectrum generates. Intervention contractors and well-governed small states are secondary beneficiaries — the former through contract flow, the latter through favorable scoring under the dominant template. Fragile/postcolonial states and their populations sit near the target end: constrained or trapped exit, no voice in setting criteria, and direct exposure to the policy consequences of classification. Non-aligned governments experiencing penalization for alternative models occupy a similar target position despite sometimes-comparable institutional capacity to favorably-scored peers, which is the clearest evidence the spectrum measures conformity to a template as much as it measures capacity per se.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (genuine 1990s state-collapse cases lacking a principled basis for calibrated engagement) was real and specific. The graduated sovereignty apparatus has since generalized far past that narrow founding case to a standing classification regime applied to states showing no institutional collapse at all. The founding_problem_status is authored as contested rather than dead, because collapse-adjacent cases still occur; but the mismatch between a status of 'contested/partially live' and a disappearance_verdict of 'world_rearranges' is exactly the capture-flag signature the six-questions battery is designed to surface — the apparatus has outgrown the narrow case that justified its founding and now does load-bearing work (conditionality, contracting, diplomatic leverage) unrelated to that founding case.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    graduated_reading_as_kernel_committer_structure,
    'Is graduated sovereignty a defensible refinement of the sovereignty concept for handling genuine capacity variation, or is ''capacity/legitimacy'' vocabulary structurally indistinguishable from a discretionary tool for reclassifying disfavored states, given that the same actors set criteria and benefit from downgrades?',
    'Cross-case audit of capacity/legitimacy classifications against independent, criteria-blind assessments of actual institutional collapse or rights violation severity: if classifications track independently-verified collapse/violation indicators closely, the reading functions closer to its coordination framing; if classifications track alignment with the classifying powers'' preferred political-economic model independent of collapse/violation indicators, the reading functions closer to a reclassification tool.',
    'Resolution toward the coordination framing would lower the appropriate ε and move this reading toward tangled_rope; resolution toward the reclassification-tool framing would confirm the snare classification and the neo-colonial extraction hypothesis in the source material.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(graduated_reading_as_kernel_committer_structure, conceptual, 'Whether capacity/legitimacy criteria track genuine institutional variation or classifier preference.').

omega_variable(
    sibling_reading_relationship_to_graduated,
    'Does the graduated reading''s spectrum model logically foreclose the binary absolute-sovereignty reading, or can both persist as live positions held by different parties (e.g., dominant states invoking graduated sovereignty selectively while still invoking absolute sovereignty defensively for their own conduct)?',
    'Examine whether any single state or bloc invokes both readings simultaneously in different contexts (graduated sovereignty to justify intervention abroad, absolute sovereignty to resist accountability at home) — if so, the readings coexist rather than foreclose, revealing instrumental rather than principled commitment to either.',
    'If dominant states demonstrably invoke both readings opportunistically, this strengthens the case that graduated_sovereignty functions as a discretionary tool rather than a principled doctrinal advance, reinforcing the snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_relationship_to_graduated, empirical, 'Whether the graduated and absolute readings are held consistently or opportunistically by the same actors.').

omega_variable(
    criteria_transparency_and_reversibility,
    'Are the capacity/legitimacy criteria published, stable, and appealable, or are they opaque, shifting, and unilaterally applied?',
    'Institutional audit of the specific indicators (World Governance Indicators, IMF Article IV assessments, credit-rating sovereign methodologies) for public availability, consistency of application across states of comparable institutional profile, and existence of a genuine appeal mechanism for a downgraded state.',
    'Transparent, stable, appealable criteria would support a coordination reading and reduce the extraction estimate; opaque or unilaterally-revisable criteria would confirm the mechanism functions as unaccountable discretion, supporting the current high-ε snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(criteria_transparency_and_reversibility, empirical, 'Whether the classification apparatus has due-process characteristics or is purely discretionary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__graduated_sovereignty, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 0, 0.22).
narrative_ontology:measurement(west_tr_t6, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 6, 0.27).
narrative_ontology:measurement(west_tr_t12, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 12, 0.31).
narrative_ontology:measurement(west_tr_t18, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 18, 0.34).
narrative_ontology:measurement(west_tr_t24, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 24, 0.37).
narrative_ontology:measurement(west_tr_t30, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(west_be_t6, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 6, 0.46).
narrative_ontology:measurement(west_be_t12, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(west_be_t18, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 18, 0.55).
narrative_ontology:measurement(west_be_t24, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 24, 0.59).
narrative_ontology:measurement(west_be_t30, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(west_su_t6, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 6, 0.44).
narrative_ontology:measurement(west_su_t12, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 12, 0.49).
narrative_ontology:measurement(west_su_t18, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 18, 0.52).
narrative_ontology:measurement(west_su_t24, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(west_su_t30, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__graduated_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty__absolute_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty__conditional_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language 'Westphalian sovereignty' concept per the ε-invariance principle: absolute_sovereignty (binary, non-interference, low external discretion), conditional_sovereignty (threshold-triggered, bounded external discretion), and graduated_sovereignty (continuous spectrum, maximal external discretion — this file). Each carries a distinct ε because each grants classifying external actors a structurally different amount of discretion over a target state's practical sovereignty. This file authors the highest ε of the three because the spectrum model has no bright-line trigger analogous to conditional_sovereignty's rights-violation threshold, leaving classification almost entirely at the discretion of the classifying power.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
