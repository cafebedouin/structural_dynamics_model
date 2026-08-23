% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__inherent_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__inherent_right_reading, []).

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
 *   constraint_id: article_9_war_renunciation__inherent_right_reading
 *   human_readable: Article 9 Inherent Right Reading: Minimum Necessary Defense Threshold
 *   domain: constitutional/law/security
 *
 * SUMMARY:
 *   The inherent_right_reading of Article 9 holds that sovereign states
 *   possess an inherent right of self-defense under international law, and
 *   Article 9's renunciation of 'war' (senso) prohibits only aggressive war,
 *   not the minimum necessary force for territorial defense. This reading,
 *   adopted by the Japanese government in 1954 to legitimize the SDF,
 *   converts the constitutional text from a categorical prohibition into a
 *   proportionality threshold. The constraint it creates — 'military capacity
 *   limited to minimum necessary for self-defense' — has governed Japanese
 *   security policy for seven decades, expanding incrementally through
 *   cabinet legislation and reinterpretation rather than formal amendment.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__inherent_right_reading, 0.42).
domain_priors:suppression_score(article_9_war_renunciation__inherent_right_reading, 0.28).
domain_priors:theater_ratio(article_9_war_renunciation__inherent_right_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__inherent_right_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__inherent_right_reading, "Article 9 Inherent Right Reading: Minimum Necessary Defense Threshold").
narrative_ontology:topic_domain(article_9_war_renunciation__inherent_right_reading, "constitutional/law/security").

domain_priors:requires_active_enforcement(article_9_war_renunciation__inherent_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__inherent_right_reading, '8afdc87d-dfc4-4074-b5a9-672170b03c0d').
narrative_ontology:cs_kernel_codification('8afdc87d-dfc4-4074-b5a9-672170b03c0d', fixed_text).
narrative_ontology:cs_authority_grounding('8afdc87d-dfc4-4074-b5a9-672170b03c0d', lineage).
narrative_ontology:cs_interpretation_layer_present('8afdc87d-dfc4-4074-b5a9-672170b03c0d').
narrative_ontology:cs_reading_relation('8afdc87d-dfc4-4074-b5a9-672170b03c0d', article_9_war_renunciation__strict_pacifist_reading, forecloses).
narrative_ontology:cs_reading_relation('8afdc87d-dfc4-4074-b5a9-672170b03c0d', article_9_war_renunciation__collective_self_defense_reading, coexists_with).
narrative_ontology:cs_axiom('8afdc87d-dfc4-4074-b5a9-672170b03c0d', foundational, inherent_right_of_self_defense_survives_article_9).
narrative_ontology:cs_axiom_status(inherent_right_of_self_defense_survives_article_9, holdable).
narrative_ontology:cs_axiom_grounding('8afdc87d-dfc4-4074-b5a9-672170b03c0d', inherent_right_of_self_defense_survives_article_9, deontological).
narrative_ontology:cs_axiom('8afdc87d-dfc4-4074-b5a9-672170b03c0d', secondary, proportionality_threshold_defines_legitimate_force).
narrative_ontology:cs_axiom_status(proportionality_threshold_defines_legitimate_force, holdable).
narrative_ontology:cs_axiom_grounding('8afdc87d-dfc4-4074-b5a9-672170b03c0d', proportionality_threshold_defines_legitimate_force, conventional).
narrative_ontology:cs_reference_frame('8afdc87d-dfc4-4074-b5a9-672170b03c0d', article_9_as_proportionality_threshold).
narrative_ontology:cs_drift_state('8afdc87d-dfc4-4074-b5a9-672170b03c0d', post_2014_reinterpretation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8afdc87d-dfc4-4074-b5a9-672170b03c0d', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, japanese_state).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, self_defense_forces).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, japanese_taxpayers).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, regional_neighbors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, japanese_government).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__inherent_right_reading, sovereign_self_defense_right).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__inherent_right_reading, proportionality_principle_in_constitutional_interpretation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and applies Article 9 through cabinet legislation and constitutional interpretation. Maintains that 'minimum necessary' defensive capacity is constitutionally permissible. Controls SDF budget, missions, and capability development. Benefits from legitimate defense posture and institutional continuity.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, japanese_government, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__inherent_right_reading, japanese_government, beneficiary).

% Japan's military organization established under the inherent right reading. Gains organizational legitimacy, budget, and institutional permanence from this interpretation. Personnel identity is fused with the 'exclusively defense-oriented' self-concept. Exit would mean institutional dissolution or radical restructuring.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, self_defense_forces, beneficiary,
    institutional, generational, identity_locked, national).

% Fund SDF through taxes (approx. 1% GDP annually). No direct say in force posture or capability decisions. Exit options limited to political advocacy or emigration. Bear opportunity costs of defense spending vs. social programs.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, japanese_taxpayers, payer,
    organized, biographical, constrained, national).

% Northeast Asian states (China, Korea, others) that bear security externalities from Japanese military normalization. No formal voice in Japanese constitutional interpretation. Exit options limited to diplomatic pressure, arms buildup, or alliance restructuring. Experience the constraint as imposed risk.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, regional_neighbors, payer,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__inherent_right_reading, regional_neighbors, excluded).

% Supreme Court and academic community that adjudicate constitutional meaning. Supreme Court has avoided direct ruling on SDF constitutionality (political question doctrine). Scholars debate textualism vs. living constitution. Their analysis shapes legitimacy but does not determine policy.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, constitutional_scholars_courts, observer,
    analytical, civilizational, analytical, national).

% Citizen groups (e.g., Article 9 Association) holding strict pacifist reading. Mobilize public opinion, litigation, and electoral pressure. Structurally excluded from security policymaking. Identity fused with constitutional pacifism — exit means abandoning core political identity.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, pacifist_civic_groups, excluded,
    moderate, biographical, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Defines the legitimate threshold of Japanese military capacity: 'minimum necessary for self-defense' converts an absolute textual prohibition into a proportionality standard, enabling a stable defense posture without constitutional amendment.
% TRANSFER_FUNCTION: Moves resources (tax revenue, strategic risk) from taxpayers and regional neighbors to the Japanese state and SDF, in exchange for territorial defense capability and alliance credibility.
% ABSENT_VOICES: Okinawan residents bearing disproportionate base burden; future generations who inherit the security posture; North Korean and Chinese civilian populations affected by regional arms dynamics. These voices are structurally excluded from Japanese constitutional interpretation.
% DISAPPEARANCE_RATIONALE: If the inherent right reading vanished, Japan would face a binary choice: strict pacifist disarmament (SDF dissolution) or constitutional amendment to legitimize current capabilities. The security architecture, alliance commitments, and regional deterrence posture would fundamentally reorganize.
% FOUNDING_PROBLEM: Post-occupation Japan needed a constitutional basis for defensive capacity without violating Article 9's war renunciation. The Yoshida Doctrine required minimal defense spending under US umbrella while maintaining sovereignty. The inherent right reading solved this by distinguishing 'war' (aggression) from 'self-defense measures' (minimum necessary force).
% FOUNDING_PROBLEM_CORROBORATION: Government officials and SDF leadership attest the founding problem remains live (evolving threats require maintained capacity). Pacifist scholars and opposition parties attest the problem is dead (US alliance and regional diplomacy suffice; SDF exceeds 'minimum necessary'). Independent international legal scholars (e.g., Hamburg School) corroborate the reading was a pragmatic compromise, not textual necessity.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__inherent_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__inherent_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__inherent_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_9_war_renunciation__inherent_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__inherent_right_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__inherent_right_reading_tests).
:- end_tests(article_9_war_renunciation__inherent_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects the resource transfer from taxpayers to SDF and the security externalities imposed on neighbors, moderate because the constraint also provides genuine coordination (stable defense posture, alliance credibility). Suppression (0.28) is low-moderate: the constraint operates through constitutional interpretation and political consensus, not heavy coercion, though pacifist alternatives are structurally marginalized. Theater ratio (0.18) is low: the 'exclusively defense-oriented' posture has real operational meaning (no power projection, no nuclear weapons), though the gap between 'minimum necessary' and actual capabilities has widened. Accessibility collapse (0.45) is moderate: strict pacifist and collective self-defense readings remain live in discourse but are excluded from official policy. Resistance (0.52) is significant: sustained civic opposition, litigation, and scholarly critique.
 *
 * PERSPECTIVAL GAP:
 *   From the government/SDF seat, this is a genuine coordination constraint (rope-like) enabling defense within constitutional order. From taxpayer and neighbor seats, it extracts resources and risk with limited accountability (snare-like). From pacifist seat, it is a violated prohibition (mountain claim broken). The engine computes this divergence from structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Japanese government and SDF are structural beneficiaries (d near 0.0-0.2): they gain legitimacy, budget, and institutional permanence. Taxpayers are payers (d near 0.7-0.8): they fund the system with constrained exit. Regional neighbors are payers/excluded (d near 0.8-0.9): they bear risk with no voice. Constitutional scholars are observers (d=0.5). Pacifist groups are identity-locked excluded (d near 1.0): their core identity requires the constraint's rejection, yet they cannot exit the polity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-occupation defense legitimacy under US umbrella) has partially mutated: the US alliance persists but the threat environment has shifted from invasion to missile/coercion scenarios. The 'minimum necessary' threshold has expanded from light infantry to Aegis destroyers, F-35s, and power-projection-capable platforms. The constraint persists not because the original problem is live, but because no political coalition exists for either strict pacifism or formal amendment — institutional inertia maintains the interpretive compromise.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the inherent_right_reading a distinct constraint from the collective_self_defense_reading, or a historical phase of the same interpretive trajectory?',
    'Analyze whether the 2014 cabinet decision reinterpretation constitutes a new constraint (collective_self_defense) or an extension of the existing inherent_right constraint. Trace whether ''minimum necessary'' retains independent operative force or has been subsumed.',
    'If distinct constraints, each requires separate ε and classification. If phases of one constraint, the temporal measurements should model the unified trajectory. Affects whether network.affects_constraints links them as family or as temporal evolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether sibling readings are structurally distinct constraints or temporal phases.').

omega_variable(
    minimum_necessary_vagueness,
    'Does ''minimum necessary for self-defense'' have determinate content, or is it an empty vessel filled by whatever capabilities the government decides to acquire?',
    'Compare SDF capability acquisitions (1954-present) against contemporaneous government statements of ''minimum necessary.'' Identify cases where capabilities were acquired first and justification constructed after.',
    'If determinate, the constraint has genuine coordination function (rope/tangled_rope). If empty vessel, the coordination story is cover for unconstrained militarization (snare). The extractiveness metric would need upward revision.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minimum_necessary_vagueness, empirical, 'Whether the proportionality threshold has binding content or is manipulable.').

omega_variable(
    regional_security_externality,
    'To what extent do regional neighbors'' threat perceptions respond to Japanese capabilities vs. Japanese constitutional interpretations?',
    'Track neighbor defense spending and doctrinal shifts correlated with Japanese capability milestones (e.g., helicopter destroyers, Tomahawk acquisition) vs. interpretive milestones (1954, 2014, 2022).',
    'If neighbors respond to capabilities, the extraction on neighbors is real and capability-driven. If they respond to interpretations, the constitutional discourse itself is the extraction mechanism. Affects victim classification and spatial scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_security_externality, empirical, 'Whether security externalities on neighbors are driven by capabilities or legal discourse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__inherent_right_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1954, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1954, 0.08).
narrative_ontology:measurement(arti_tr_t1970, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(arti_tr_t1990, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(arti_tr_t2001, article_9_war_renunciation__inherent_right_reading, theater_ratio, 2001, 0.16).
narrative_ontology:measurement(arti_tr_t2014, article_9_war_renunciation__inherent_right_reading, theater_ratio, 2014, 0.17).
narrative_ontology:measurement(arti_tr_t2024, article_9_war_renunciation__inherent_right_reading, theater_ratio, 2024, 0.18).

% Extraction over time
narrative_ontology:measurement(arti_be_t1954, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1954, 0.15).
narrative_ontology:measurement(arti_be_t1970, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1970, 0.22).
narrative_ontology:measurement(arti_be_t1990, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1990, 0.28).
narrative_ontology:measurement(arti_be_t2001, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2001, 0.33).
narrative_ontology:measurement(arti_be_t2014, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2014, 0.38).
narrative_ontology:measurement(arti_be_t2024, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1954, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1954, 0.12).
narrative_ontology:measurement(arti_su_t1970, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1970, 0.18).
narrative_ontology:measurement(arti_su_t1990, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1990, 0.22).
narrative_ontology:measurement(arti_su_t2001, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2001, 0.25).
narrative_ontology:measurement(arti_su_t2014, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2014, 0.27).
narrative_ontology:measurement(arti_su_t2024, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2024, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__inherent_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_9_war_renunciation__inherent_right_reading, 0.12).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation__collective_self_defense_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation__strict_pacifist_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, us_japan_security_treaty_obligations).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, japanese_defense_budget_trajectory).

% DUAL FORMULATION NOTE:
% This reading and collective_self_defense_reading form a constraint family: the latter extends the former's logic to allied defense. Both share the kernel 'article_9_war_renunciation' but instantiate different ε values (inherent_right ε≈0.42, collective_self_defense ε≈0.55 estimated). The strict_pacifist_reading is the excluded counter-constraint with near-zero extractiveness but high suppression on its own adherents.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_9_war_renunciation__inherent_right_reading, institutional, 0.15).
constraint_indexing:directionality_override(article_9_war_renunciation__inherent_right_reading, organized, 0.75).
constraint_indexing:directionality_override(article_9_war_renunciation__inherent_right_reading, powerful, 0.85).
constraint_indexing:directionality_override(article_9_war_renunciation__inherent_right_reading, moderate, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
