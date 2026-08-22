% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__strategic_culture_drift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__strategic_culture_drift, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: total_war_winnability_post1945__strategic_culture_drift
 *   human_readable: Total War as Unreachable Elite Discourse Category (Strategic Culture Drift Reading)
 *   domain: international_relations_theory/strategic_studies
 *
 * SUMMARY:
 *   This story reads the post-1945 disappearance of total war from elite
 *   strategic discourse as an ideational drift internal to strategic culture
 *   — not a physical removal of the option (structural_contraction_reading,
 *   which locates the change in nuclear weapons making total war unreachable)
 *   and not a normative delegitimation through international law
 *   (normative_reading_drop, which locates the change in Article 2(4) and
 *   humanitarian law). On this reading, total war remains materially
 *   reachable — the capability, the industrial base, the escalatory pathways
 *   all still exist in principle — but the professional class that produces
 *   strategic vocabulary (defense intellectuals, doctrine writers,
 *   arms-control specialists) has built careers, institutions, and journals
 *   around limited-war frameworks, and total-war planning has atrophied as a
 *   discursive category through decades of institutional forgetting rather
 *   than through any hard constraint. The claimed type is piton: what
 *   persists is not active suppression of total-war thinking but the diffuse,
 *   uncorrected drift of an entire professional discourse away from a
 *   capability nobody meaningfully profits from suppressing and nobody is
 *   hurt badly enough, in the near term, to fix.
 *
 * KEY AGENTS:
 *   - limited_war_defense_intellectuals: institutional beneficiaries who administer the discourse
 *   - arms_control_epistemic_community: adjacent institutional beneficiaries
 *   - future_crisis_planners: trapped payers who inherit whatever planning capacity remains
 *   - strategic_flexibility: the non-agent capacity being diminished
 *   - rising_revisionist_powers: excluded from the ideational shift, may not share it
 *   - strategic_culture_theorists: analytical observers of the mechanism itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__strategic_culture_drift, 0.42).
domain_priors:suppression_score(total_war_winnability_post1945__strategic_culture_drift, 0.31).
domain_priors:theater_ratio(total_war_winnability_post1945__strategic_culture_drift, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, extractiveness, 0.42).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__strategic_culture_drift, piton).
narrative_ontology:human_readable(total_war_winnability_post1945__strategic_culture_drift, "Total War as Unreachable Elite Discourse Category (Strategic Culture Drift Reading)").
narrative_ontology:topic_domain(total_war_winnability_post1945__strategic_culture_drift, "international_relations_theory/strategic_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__strategic_culture_drift, '6140afac-30f4-4b9e-92b1-a7e9215c7ad3').
narrative_ontology:cs_kernel_codification('6140afac-30f4-4b9e-92b1-a7e9215c7ad3', distributed).
narrative_ontology:cs_authority_grounding('6140afac-30f4-4b9e-92b1-a7e9215c7ad3', practice).
narrative_ontology:cs_interpretation_layer_present('6140afac-30f4-4b9e-92b1-a7e9215c7ad3').
narrative_ontology:cs_reading_relation('6140afac-30f4-4b9e-92b1-a7e9215c7ad3', total_war_winnability_post1945__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('6140afac-30f4-4b9e-92b1-a7e9215c7ad3', total_war_winnability_post1945__normative_reading_drop, coexists_with).
narrative_ontology:cs_axiom('6140afac-30f4-4b9e-92b1-a7e9215c7ad3', foundational, capability_persists_absent_discursive_maintenance).
narrative_ontology:cs_axiom_status(capability_persists_absent_discursive_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('6140afac-30f4-4b9e-92b1-a7e9215c7ad3', capability_persists_absent_discursive_maintenance, empirically_contingent).
narrative_ontology:cs_axiom('6140afac-30f4-4b9e-92b1-a7e9215c7ad3', foundational, professional_discourse_shapes_perceived_option_space_independent_of_material_constraint).
narrative_ontology:cs_axiom_status(professional_discourse_shapes_perceived_option_space_independent_of_material_constraint, holdable).
narrative_ontology:cs_axiom_grounding('6140afac-30f4-4b9e-92b1-a7e9215c7ad3', professional_discourse_shapes_perceived_option_space_independent_of_material_constraint, empirically_contingent).
narrative_ontology:cs_reference_frame('6140afac-30f4-4b9e-92b1-a7e9215c7ad3', cold_war_escalation_management_consensus).
narrative_ontology:cs_drift_state('6140afac-30f4-4b9e-92b1-a7e9215c7ad3', post_cold_war_unipolar_and_multipolar_transition, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6140afac-30f4-4b9e-92b1-a7e9215c7ad3', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__strategic_culture_drift, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, limited_war_defense_intellectuals).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, arms_control_epistemic_community).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, strategic_flexibility).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, future_crisis_planners).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__strategic_culture_drift, strategic_culture_theory).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__strategic_culture_drift, ideational_constructivism_in_security_studies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Populate the think tanks, war colleges, and doctrine-writing bodies that produce the vocabulary elites use to reason about force. Their careers, journals, and institutional prestige are built on limited-war frameworks — flexible response, graduated escalation, proportionality doctrine. They administer the discourse that has quietly stopped treating total war as a live planning category, and they benefit from that narrowing because it is the frame in which their expertise is the expertise that matters.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, limited_war_defense_intellectuals, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(total_war_winnability_post1945__strategic_culture_drift, limited_war_defense_intellectuals, agenda_setter).

% Diplomats, treaty negotiators, and academic security specialists whose professional and institutional standing depends on total war remaining discursively unthinkable rather than merely regulated. They collect prestige, funding, and access from a world in which the total-war option is treated as having receded from serious strategic conversation, independent of whether the underlying capability has actually gone anywhere.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, arms_control_epistemic_community, beneficiary,
    institutional, generational, mobile, global).

% Military planners and civilian leaders who will face a genuine great-power crisis in conditions where the intellectual and institutional muscle for reasoning about total-war escalation has atrophied. They cannot exit the situation — they inherit whatever conceptual toolkit the current discourse leaves them — and pay the cost of institutional forgetting in the form of degraded planning capacity should reachability ever matter again.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, future_crisis_planners, payer,
    moderate, civilizational, trapped, global).

% Not an actor but the capacity itself: the range of strategic options a state or alliance could in principle exercise. This capacity is narrowed as an artifact of what elites find it professionally and socially comfortable to discuss, not because the underlying physical or organizational capability for total war has been removed. Listed for completeness as the thing that is diminished, though it collects nothing and can advocate for nothing on its own behalf.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, strategic_flexibility, payer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(total_war_winnability_post1945__strategic_culture_drift, strategic_flexibility).

% States whose own strategic cultures may not share the post-1945 Western elite discourse's aversion to total-war framing. They are excluded from the conversation that produced this ideational shift and may not be bound by it, meaning the dropped discourse could leave Western planners unprepared for an adversary who never underwent the same ideational narrowing.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, rising_revisionist_powers, excluded,
    powerful, generational, constrained, global).

% Scholars of strategic culture who study the ideational-shift mechanism itself — documenting how discourse, doctrine, and professional norms diverged from underlying material capability. They have no stake in whether total war is reachable, only in accurately describing why elites stopped talking about it as though it were.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, strategic_culture_theorists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_winnability_post1945__strategic_culture_drift, diffuse).
narrative_ontology:fixing_cost_class(total_war_winnability_post1945__strategic_culture_drift, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely reduces catastrophic miscalculation risk by giving elites a shared professional vocabulary — flexible response, escalation control, limited war doctrine — that lets great powers signal restraint and interpret each other's signals without invoking total-war framing at every crisis.
% TRANSFER_FUNCTION: Moves intellectual authority, funding, and institutional prestige away from total-war strategic planning and toward limited-war and arms-control expertise; moves preparedness cost onto whoever eventually needs total-war-grade strategic reasoning that the discourse no longer maintains.
% ABSENT_VOICES: Military historians and strategists who study total-war escalation dynamics have been marginalized within the field; rising powers whose own strategic cultures were not party to the post-1945 Western ideational shift are not in the room when the discourse's assumptions are set.
% DISAPPEARANCE_RATIONALE: If the dropped-discourse pattern reversed overnight and elites resumed treating total war as a live planning category, the limited-war intellectual establishment's relevance would be disrupted, funding streams would shift, and doctrine-writing bodies would need new expertise. Whether the underlying strategic reality would 'rearrange' is contested: some argue capability was never actually constrained by discourse (so nothing rearranges materially, only institutionally); others argue that atrophied planning muscle takes years to rebuild and the world's crisis-response capacity would in fact be materially different.
% FOUNDING_PROBLEM: Cold War planners needed a way to reason about and communicate escalation control short of total war, so that crises (Cuban Missile Crisis-style confrontations) could be managed without every disagreement defaulting to total-war logic.
% FOUNDING_PROBLEM_CORROBORATION: Limited-war defense intellectuals themselves attest the problem remains live (proliferation, regional conflicts, gray-zone competition all require limited-war vocabulary). Independent military historians and some retired flag officers, writing outside the limited-war epistemic community, attest that the founding problem of escalation management has been solved well enough that the current near-total absence of total-war planning capacity reflects institutional drift rather than an ongoing functional need — this is the corroborating source outside the beneficiary set.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__strategic_culture_drift, contested).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__strategic_culture_drift, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__strategic_culture_drift, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_winnability_post1945__strategic_culture_drift, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__strategic_culture_drift, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__strategic_culture_drift_tests).
:- end_tests(total_war_winnability_post1945__strategic_culture_drift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.42 at 2025) and rising slowly across the interval — this is not a hard extraction mechanism but a diffuse cost imposed on future planning capacity as institutional muscle memory fades. Suppression is authored low-moderate (0.31): nothing actively forbids total-war strategic thought, but the accessibility_collapse (0.58) captures how thoroughly the professional infrastructure for producing it has hollowed out, making it harder to reconstitute even though no one is stopping anyone. Theater ratio rises sharply across the interval (0.10 to 0.68) because an increasing share of strategic-studies activity — doctrine conferences, war-gaming exercises, escalation-ladder scholarship — performs rigor about limited war while the underlying total-war planning competence it once complemented has withered; the performance has outgrown the function it was meant to support.
 *
 * DIRECTIONALITY LOGIC:
 *   Limited-war defense intellectuals and the arms-control epistemic community are coded as beneficiaries because the narrowed discourse is the frame within which their specific expertise retains institutional value — this is a low-d, subsidized position even though no one is cutting them a check labeled 'extraction.' Future crisis planners are coded as payers with trapped exit because they cannot choose the intellectual inheritance they receive when a genuine great-power crisis arrives; strategic_flexibility itself is listed as a non-agent payer to keep the diminished-capacity fact visible without smuggling agency into an abstraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — Cold War escalation management — is authored as contested-status rather than flatly dead, which is the correct piton signature: the mandate has partially outlived its function (crisis management skill is still needed) even as the institutional apparatus built around it has drifted into performing a category error (treating total war as discursively unreachable when it is not). This prevents mislabeling the constraint as pure extraction (no one is meaningfully profiting from suppressing total-war thought — the defense intellectuals benefit from the frame, not from actively blocking alternatives) or as pure coordination (the drift genuinely degrades a capability that may be needed again, which a clean rope reading would ignore).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reachability_vs_discourse_gap,
    'Is total war actually still reachable in a meaningful operational sense, or has the discursive drift this reading documents been accompanied by (and perhaps caused) real degradation of the industrial, doctrinal, and organizational capacity that reachability requires — collapsing this reading into the structural_contraction_reading it claims to be distinct from?',
    'War-gaming exercises that stress-test whether current force structures, industrial mobilization capacity, and command doctrine could actually execute total-war-scale operations if ordered to; comparison against Cold War-era mobilization benchmarks.',
    'If capacity has degraded to the point of genuine unreachability, this reading''s premise (discourse dropped, capability intact) fails and the constraint collapses into the structural_contraction_reading — they would no longer be distinct constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reachability_vs_discourse_gap, empirical, 'Whether discursive drift has produced real capability erosion, threatening this reading''s distinctness from the structural reading.').

omega_variable(
    kernel_reading_indeterminacy,
    'Which of the three kernel readings (structural, normative, ideational) best explains the observed post-1945 discourse pattern, and could all three be simultaneously true as overlapping causal contributors rather than competing explanations?',
    'Comparative historical analysis of strategic discourse in nuclear and non-nuclear great powers, and in states with varying degrees of exposure to the international humanitarian law regime, to isolate which factor best predicts discourse change independent of the others.',
    'If all three readings are jointly true and mutually reinforcing rather than competing, the network of sibling constraints should be understood as co-causal rather than as alternative hypotheses about a single underlying fact — this would not change any single reading''s authored epsilon but would change how the family''s disappearance_verdict should be interpreted collectively.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the three kernel readings are genuine competitors or jointly-operating co-causes.').

omega_variable(
    revisionist_power_asymmetry,
    'Do rising powers outside the post-1945 Western strategic-culture tradition actually share the ideational drift this reading documents, or have they retained total-war planning as a live discursive category — creating an asymmetric preparedness gap?',
    'Comparative content analysis of military doctrine publications, war college curricula, and official strategic guidance documents across major and rising powers over the interval.',
    'If the drift is asymmetric, the piton classification understates the risk: what looks like inert institutional forgetting to the beneficiary community is an active preparedness liability relative to actors who never underwent the same discourse shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revisionist_power_asymmetry, empirical, 'Whether the ideational drift is a universal or Western-specific phenomenon, with preparedness implications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__strategic_culture_drift, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(tota_tr_t1962, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1962, 0.22).
narrative_ontology:measurement(tota_tr_t1975, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1975, 0.38).
narrative_ontology:measurement(tota_tr_t1991, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1991, 0.52).
narrative_ontology:measurement(tota_tr_t2001, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 2001, 0.58).
narrative_ontology:measurement(tota_tr_t2014, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 2014, 0.63).
narrative_ontology:measurement(tota_tr_t2025, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 2025, 0.68).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1945, 0.12).
narrative_ontology:measurement(tota_be_t1962, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1962, 0.18).
narrative_ontology:measurement(tota_be_t1975, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1975, 0.25).
narrative_ontology:measurement(tota_be_t1991, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1991, 0.31).
narrative_ontology:measurement(tota_be_t2001, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 2001, 0.35).
narrative_ontology:measurement(tota_be_t2014, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 2014, 0.39).
narrative_ontology:measurement(tota_be_t2025, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 2025, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(total_war_winnability_post1945__strategic_culture_drift, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__strategic_culture_drift, identity_coordination).
narrative_ontology:boltzmann_floor_override(total_war_winnability_post1945__strategic_culture_drift, 0.08).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, structural_contraction_reading).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, normative_reading_drop).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the kernel total_war_winnability_post1945. structural_contraction_reading locates the post-1945 change in nuclear-weapons-driven physical impossibility (a mountain-flavored claim); normative_reading_drop locates it in the delegitimation of total war under international humanitarian law (a normative/legal claim); this reading (strategic_culture_drift) locates it in ideational drift within elite defense-intellectual discourse, treating both the physical capability and the legal permissibility as substantially intact while the professional vocabulary for reasoning about total war atrophied through institutional forgetting. The three readings share no single epsilon — each is authored independently per the ε-invariance principle — and are linked here so that contamination or credibility shifts in one reading's evidentiary basis can be tracked against the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
