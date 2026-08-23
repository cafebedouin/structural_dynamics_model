% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__endogenous_reinterpretation_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
 *   human_readable: The Manifesto as Genuine Prophetic Revelation (Endogenous Reinterpretation Reading)
 *   domain: religious_institutional_history/political_theology/commitment_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the
 *   endogenous_reinterpretation_reading of the marriage_commitment_legitimacy
 *   kernel. The reading holds that the 1890 Manifesto (Official Declaration
 *   1) was a genuine prophetic revelation — God commanded the cessation of
 *   plural marriage to preserve the Church for higher purposes. From this
 *   frame, the constraint (the legitimacy of the post-Manifesto marriage
 *   covenant) is a Mountain: it emerges from divine authority, not human
 *   negotiation. Federal pressure was the catalyst that occasioned the
 *   revelation, not its cause. Theological continuity is maintained by
 *   reframing monogamy as a new covenant stage, not a renunciation of prior
 *   doctrine. The reading claims low extractiveness because the primary
 *   beneficiary is divine authority maintaining prophetic succession
 *   legitimacy; the costs borne by dissident fundamentalists are real but
 *   secondary to the divine purpose.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.12).
domain_priors:suppression_score(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.08).
domain_priors:theater_ratio(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, mountain).
narrative_ontology:human_readable(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "The Manifesto as Genuine Prophetic Revelation (Endogenous Reinterpretation Reading)").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "religious_institutional_history/political_theology/commitment_systems").

domain_priors:emerges_naturally(marriage_commitment_legitimacy__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'f5f722a5-ae95-454b-bde0-69e5a2dc2195').
narrative_ontology:cs_kernel_codification('f5f722a5-ae95-454b-bde0-69e5a2dc2195', formalized).
narrative_ontology:cs_authority_grounding('f5f722a5-ae95-454b-bde0-69e5a2dc2195', lineage).
narrative_ontology:cs_interpretation_layer_present('f5f722a5-ae95-454b-bde0-69e5a2dc2195').
narrative_ontology:cs_reading_relation('f5f722a5-ae95-454b-bde0-69e5a2dc2195', marriage_commitment_legitimacy__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('f5f722a5-ae95-454b-bde0-69e5a2dc2195', marriage_commitment_legitimacy__hybrid_pragmatic_reading, coexists_with).
narrative_ontology:cs_axiom('f5f722a5-ae95-454b-bde0-69e5a2dc2195', foundational, manifesto_is_genuine_prophetic_revelation).
narrative_ontology:cs_axiom_status(manifesto_is_genuine_prophetic_revelation, holdable).
narrative_ontology:cs_axiom_grounding('f5f722a5-ae95-454b-bde0-69e5a2dc2195', manifesto_is_genuine_prophetic_revelation, theological).
narrative_ontology:cs_axiom('f5f722a5-ae95-454b-bde0-69e5a2dc2195', foundational, monogamy_as_new_covenant_stage).
narrative_ontology:cs_axiom_status(monogamy_as_new_covenant_stage, holdable).
narrative_ontology:cs_axiom_grounding('f5f722a5-ae95-454b-bde0-69e5a2dc2195', monogamy_as_new_covenant_stage, theological).
narrative_ontology:cs_reference_frame('f5f722a5-ae95-454b-bde0-69e5a2dc2195', prophetic_succession_continuity).
narrative_ontology:cs_drift_state('f5f722a5-ae95-454b-bde0-69e5a2dc2195', contemporary_correlation_era, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('f5f722a5-ae95-454b-bde0-69e5a2dc2195', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prophetic_leadership).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, mainstream_membership).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, dissident_fundamentalists).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, divine_authority_of_prophetic_succession).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theological_continuity_through_covenant_stages).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, monogamy_as_new_covenant_stage).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate source of the revelation; the constraint's operation vindicates divine authority's ongoing guidance of the Church through prophetic succession. Not a human actor but the theological referent whose legitimacy is maintained by the reading.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, divine_authority, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, divine_authority).

% Receives and announces the Manifesto as divine command; administers the transition from plural marriage to monogamy as a new covenant stage. Their authority depends on the revelation's authenticity being accepted. They hold institutional power to define doctrine and discipline dissenters.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prophetic_leadership, agenda_setter,
    institutional, generational, arbitrage, global).

% Ordinary Latter-day Saints who accept the Manifesto as God's will. They gain theological coherence (the Church is preserved, prophetic succession unbroken) and practical survival (federal pressure lifts, statehood achieved). Exit is constrained by community ties, identity, and belief in the revelation's authenticity.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, mainstream_membership, beneficiary,
    organized, biographical, constrained, global).

% Members who reject the Manifesto as a capitulation, not a revelation. They bear the cost of schism: excommunication, loss of community, legal persecution, and the burden of maintaining 'the fullness' alone. Their identity is fused to plural marriage as an unchangeable requirement; exit from the fundamentalist position is identity-locked.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, dissident_fundamentalists, payer,
    moderate, biographical, identity_locked, regional).

% Applied coercive pressure (Edmunds Act, Edmunds-Tucker Act, disincorporation threats) that the endogenous reading treats as catalyst, not cause. From this reading's frame, the federal role is incidental — God would have commanded the reversal regardless. The federal government is not a party to the theological settlement.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, federal_government, excluded,
    institutional, generational, arbitrage, national).

% Sees the full structural field: the endogenous claim of genuine revelation, the exogenous claim of coercion, the hybrid claim of strategic adaptation. Evaluates the constraint's classification from outside any single belief frame.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the existential crisis of the Church (1887-1890): federal disincorporation, asset seizure, leadership imprisonment, and the threat of total institutional destruction. The Manifesto coordinates a unified theological reframing that preserves the Church as a viable institution while maintaining prophetic authority's legitimacy.
% TRANSFER_FUNCTION: Moves the burden of theological innovation from the prophetic office (which would lose legitimacy if seen as merely reacting to pressure) to divine authority (which acts sovereignly). The cost of abandoning plural marriage is borne by dissident fundamentalists; the benefit of institutional survival accrues to mainstream membership and prophetic leadership.
% ABSENT_VOICES: The pre-Manifesto faithful who invested their eternal salvation in plural marriage as an absolute requirement — their voices are silenced by the revelation's declaration that God changed the requirement. Also absent: the federal officials who saw the Manifesto as a political surrender, not a theological event.
% DISAPPEARANCE_RATIONALE: If the endogenous reading's claim (genuine revelation) vanished overnight, the Church's claim to prophetic continuity would fracture. Mainstream membership would face a legitimacy crisis: either the Manifesto was a capitulation (exogenous reading) or a strategic adaptation (hybrid reading). The theological architecture supporting current LDS doctrine and identity would collapse or require radical reconstruction.
% FOUNDING_PROBLEM: The Church faced existential destruction by the U.S. federal government (1887-1890): the Edmunds-Tucker Act disincorporated the Church, seized its assets, imprisoned its leaders, and threatened its complete eradication. Plural marriage, previously taught as an eternal requirement for exaltation, had become the lever of destruction.
% FOUNDING_PROBLEM_CORROBORATION: Federal congressional records (Edmunds-Tucker Act, 1887), Church financial records showing asset seizure, Wilford Woodruff's journal documenting the 'thus saith the Lord' moment, and non-LDS historical scholarship (e.g., Sarah Barringer Gordon, Kathleen Flake) all corroborate the existential crisis. The status 'dead' is corroborated by Utah statehood (1896), the Church's corporate restoration, and the cessation of federal anti-polygamy prosecution against the institutional Church.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, ExtMetricName, E),
    domain_priors:suppression_score(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(marriage_commitment_legitimacy__endogenous_reinterpretation_reading),
    narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because the constraint's operation, from this reading, serves divine purpose rather than extracting resources from a victim class. The dissident fundamentalists' suffering is a consequence of their rejection of the revelation, not the revelation's design. Suppression is minimal (0.08) — the Manifesto was accepted by the vast majority without coercion; excommunication of dissenters follows from their rejection of prophetic authority, not from the constraint's enforcement machinery. Theater ratio is near zero (0.05) — the coordination function (institutional survival via divine command) is the genuine function. Accessibility collapse is high (0.92) — once the revelation is accepted as genuine, alternatives (continuing plural marriage) are theologically impossible. Resistance is near zero (0.03) — the reading itself meets almost no resistance within the believing community; resistance comes from the excluded fundamentalist frame, which this reading treats as a separate constraint.
 *
 * PERSPECTIVAL GAP:
 *   The endogenous reading's Mountain claim diverges sharply from the exogenous reading's Snare claim (federal coercion) and the hybrid reading's Tangled Rope claim (strategic adaptation). From the endogenous seat, the constraint is a divine Mountain with near-zero extraction. From the dissident fundamentalist seat, the same constraint is a Snare — extraction of their theological identity under threat of excommunication. The engine computes this divergence from the stakeholder structural data; the claimed_type declares only the endogenous reading's frame.
 *
 * DIRECTIONALITY LOGIC:
 *   Divine authority (vindicated proposition, not agent) is the structural beneficiary — the constraint's operation confirms ongoing revelation and prophetic succession. Prophetic leadership (agenda_setter) benefits institutionally but does not extract; they administer the revelation. Mainstream membership (beneficiary) gains institutional survival and theological coherence. Dissident fundamentalists (payer) bear the full cost of schism, but their position is identity-locked — they cannot exit without abandoning their self-concept as keepers of the 'fullness.' Federal government (excluded) is the catalyst, not a party to the theological settlement. The engine will compute per-seat effective extraction from these structural declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (existential federal destruction) is dead — Utah achieved statehood, the Church was legally restored, federal persecution ended. Yet the constraint (the Manifesto's authority) persists. The endogenous reading resolves the mandatrophy by declaring the problem was never merely political survival but divine preparation for a higher covenant stage (monogamy as the new law of the priesthood). The arrangement persists not because the old problem lingers but because the revelation redefined the problem's telos. This is not a piton (inertial persistence) but a Mountain (ongoing divine authority). The corroboration from non-LDS historians that the existential crisis was real and ended supports the 'dead' status for the political problem while the theological reframing remains 'live' within the reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_authenticity_ambiguity,
    'Is the Manifesto''s status as genuine revelation empirically distinguishable from a strategic institutional adaptation that successfully presents itself as revelation?',
    'Comparative analysis of prophetic language before/during/after crisis; internal deliberation records (Woodruff''s journal, Quorum minutes); whether the ''thus saith the Lord'' formulation appears before or after the political decision.',
    'If the revelation language post-dates the political calculation, the endogenous reading''s Mountain claim collapses to at best a Scaffold or Tangled Rope. If the revelation language is contemporaneous with or precedes the political resolution, the Mountain claim gains empirical support.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revelation_authenticity_ambiguity, empirical, 'Whether the endogenous reading''s core claim (genuine revelation) is historically verifiable or intrinsically unfalsifiable.').

omega_variable(
    divine_beneficiary_ontology,
    'Does ''divine authority as beneficiary'' name a real structural beneficiary in the DR sense, or is it a theological placeholder that masks human institutional benefit?',
    'Trace the material benefits of the Manifesto: who gained property, legal status, political access, institutional survival? If prophetic leadership and mainstream membership are the material beneficiaries, ''divine authority'' functions as a vindicated proposition, not a beneficiary.',
    'If divine authority is a vindicated proposition rather than a beneficiary, the constraint''s extractiveness profile shifts — the human beneficiaries (leadership, membership) become visible, potentially raising ε. The current low ε (0.12) assumes divine authority absorbs the beneficiary role.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_beneficiary_ontology, conceptual, 'Whether the beneficiary declaration ''divine authority'' is structurally coherent or a category error that obscures human extraction.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the marriage_commitment_legitimacy kernel admit a single legitimate framing, or do the three readings represent irreducibly different kernels?',
    'Test whether the readings share the same constraint_id referent. If exogenous_override_reading and endogenous_reinterpretation_reading evaluate different arrangements (suspended practice vs. new covenant), they are different constraints per ε-invariance. If they evaluate the same arrangement with different ε, the kernel is underdetermined.',
    'If the readings are different constraints, the kernel_id is a linguistic convenience, not a structural unit. The network.affects_constraints links would be mis-specified. If they are genuine siblings, the reading_relations (coexists_with) are correctly authored.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the three declared readings share a single ε-referent or constitute a constraint family requiring decomposition.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the low suppression (0.08) structural (genuine consensus) or internalized (dissidents silenced by identity-lock before resistance forms)?',
    'Examine the trajectory of fundamentalist resistance: was it crushed by enforcement (structural) or did it never form because the revelation''s authenticity was identity-constitutive for the majority (internalized)? Post-exit suppression trajectory of fundamentalist groups indicates mechanism.',
    'If internalized, the constraint''s effective suppression is higher than measured — the target population carries the suppression with them. This would raise the Mountain''s resistance metric (currently 0.03) and potentially trigger FSM evaluation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the endogenous reading''s frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0, 134).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mcl_endogenous_reinterp_tr_t0, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(mcl_endogenous_reinterp_tr_t30, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 30, 0.06).
narrative_ontology:measurement(mcl_endogenous_reinterp_tr_t60, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 60, 0.05).
narrative_ontology:measurement(mcl_endogenous_reinterp_tr_t90, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 90, 0.05).
narrative_ontology:measurement(mcl_endogenous_reinterp_tr_t134, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 134, 0.05).

% Extraction over time
narrative_ontology:measurement(mcl_endogenous_reinterp_be_t0, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(mcl_endogenous_reinterp_be_t30, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 30, 0.12).
narrative_ontology:measurement(mcl_endogenous_reinterp_be_t60, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 60, 0.1).
narrative_ontology:measurement(mcl_endogenous_reinterp_be_t90, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 90, 0.11).
narrative_ontology:measurement(mcl_endogenous_reinterp_be_t134, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 134, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(mcl_endogenous_reinterp_su_t0, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(mcl_endogenous_reinterp_su_t30, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 30, 0.08).
narrative_ontology:measurement(mcl_endogenous_reinterp_su_t60, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 60, 0.06).
narrative_ontology:measurement(mcl_endogenous_reinterp_su_t90, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 90, 0.07).
narrative_ontology:measurement(mcl_endogenous_reinterp_su_t134, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 134, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.08).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the 'Manifesto legitimacy' label into three structurally distinct claims with different ε values. Endogenous reading: ε≈0.12 (Mountain, genuine revelation). Exogenous reading: ε≈0.75 (Snare, federal coercion). Hybrid reading: ε≈0.45 (Tangled Rope, strategic adaptation). The ε-invariance principle requires separate stories; the kernel_id is a linguistic convenience, not a structural unity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, moderate, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
