% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Divine Command Reinterpretation of Marriage (Endogenous Reading)
 *   domain: religious_institutional_history/political_theology/commitment_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the 'endogenous reinterpretation'
 *   reading of the marriage_commitment_legitimacy kernel. In this reading,
 *   the Manifesto (a declaration ending the practice of plural marriage) is
 *   understood as a genuine prophetic revelation, a direct command from God
 *   to preserve the Church for higher purposes. Federal pressure is seen as a
 *   catalyst, not the ultimate cause, and the theological continuity of the
 *   Church is maintained through a reframing of monogamy as a new stage in
 *   the covenant. The constraint's low extractiveness reflects its framing as
 *   a divine mandate for the collective good, rather than a coercive
 *   imposition.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.15).
domain_priors:suppression_score(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.78).
domain_priors:theater_ratio(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, mountain).
narrative_ontology:human_readable(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "Divine Command Reinterpretation of Marriage (Endogenous Reading)").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "religious_institutional_history/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__endogenous_reinterpretation_reading).
domain_priors:emerges_naturally(marriage_commitment_legitimacy__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'ab5bd2be-d6b2-4fe0-a796-f748d2f4feda').
narrative_ontology:cs_kernel_codification('ab5bd2be-d6b2-4fe0-a796-f748d2f4feda', fixed_text).
narrative_ontology:cs_authority_grounding('ab5bd2be-d6b2-4fe0-a796-f748d2f4feda', lineage).
narrative_ontology:cs_interpretation_layer_present('ab5bd2be-d6b2-4fe0-a796-f748d2f4feda').
narrative_ontology:cs_reading_relation('ab5bd2be-d6b2-4fe0-a796-f748d2f4feda', marriage_commitment_legitimacy__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('ab5bd2be-d6b2-4fe0-a796-f748d2f4feda', marriage_commitment_legitimacy__hybrid_pragmatic_reading, coexists_with).
narrative_ontology:cs_axiom('ab5bd2be-d6b2-4fe0-a796-f748d2f4feda', foundational, prophetic_revelation_is_continuous).
narrative_ontology:cs_axiom_status(prophetic_revelation_is_continuous, holdable).
narrative_ontology:cs_axiom_grounding('ab5bd2be-d6b2-4fe0-a796-f748d2f4feda', prophetic_revelation_is_continuous, theological).
narrative_ontology:cs_axiom('ab5bd2be-d6b2-4fe0-a796-f748d2f4feda', foundational, divine_will_guides_church_preservation).
narrative_ontology:cs_axiom_status(divine_will_guides_church_preservation, holdable).
narrative_ontology:cs_axiom_grounding('ab5bd2be-d6b2-4fe0-a796-f748d2f4feda', divine_will_guides_church_preservation, theological).
narrative_ontology:cs_reference_frame('ab5bd2be-d6b2-4fe0-a796-f748d2f4feda', continuous_prophetic_guidance_framework).
narrative_ontology:cs_drift_state('ab5bd2be-d6b2-4fe0-a796-f748d2f4feda', post_manifesto_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ab5bd2be-d6b2-4fe0-a796-f748d2f4feda', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, church_leadership).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, faithful_members).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prophetic_succession_legitimacy).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, divine_guidance_in_crisis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and promulgates the Manifesto as a genuine prophetic revelation, a divine command to preserve the Church. Benefits from the continued legitimacy of prophetic succession and institutional integrity. Their identity is fused with the Church's divine mandate.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, church_leadership, agenda_setter,
    institutional, generational, identity_locked, global).

% Accept the Manifesto as divine will, reinterpreting their understanding of marriage in accordance with the new revelation. They benefit from the Church's preservation and continued access to divine guidance, seeing their obedience as a path to higher purposes. Their social and spiritual identity is deeply tied to the Church.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, faithful_members, beneficiary,
    moderate, biographical, identity_locked, global).

% Exerted external pressure through anti-polygamy laws, but from this reading's perspective, they were merely the catalyst or occasion for a divinely commanded reinterpretation, not the ultimate cause of the change. They observe the Church's compliance with secular law.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, federal_government, observer,
    institutional, generational, analytical, national).

% Analyze and articulate the theological coherence of the reinterpretation, demonstrating its continuity with past doctrine and its necessity for the Church's divine mission. Their careers and intellectual identity are bound to the tradition.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theological_scholars_within_tradition, observer,
    organized, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Realigns the Church's practices and theological understanding of marriage with a new prophetic revelation, ensuring institutional survival and the continuity of divine guidance in the face of existential external threats.
% TRANSFER_FUNCTION: Transfers the burden of adapting to external legal pressures from the core, immutable theological doctrine to the interpretive flexibility of prophetic authority, thereby preserving the legitimacy of the Church's leadership and mission.
% ABSENT_VOICES: Those who left the Church due to the Manifesto, or who interpret it as a capitulation to secular power rather than divine command, are structurally excluded from this reading's internal justification. Their dissent is framed as a failure of faith or understanding.
% DISAPPEARANCE_RATIONALE: If this endogenous reinterpretation were not accepted as divinely commanded, the entire theological framework, institutional identity, and claim to continuous prophetic revelation of the Church would be undermined. The Church's historical narrative and current legitimacy would collapse, leading to a profound reorganization of its structure and membership.
% FOUNDING_PROBLEM: The Church faced an existential threat from federal anti-polygamy laws, which jeopardized its property, leadership, and the freedom of its members, threatening the very existence of the divinely established institution.
% FOUNDING_PROBLEM_CORROBORATION: Church historians and theologians within the tradition corroborate that the threat of institutional dissolution was real and that the Manifesto was a divinely mandated response. External historians acknowledge the federal pressure but typically dispute the divine command aspect, focusing on socio-political factors.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   From this reading's perspective, the constraint is a divine command, hence its classification as a 'mountain' (emerges_naturally: true). Extractiveness is low (0.15) because the reinterpretation is framed as a necessary, divinely guided adaptation for the Church's preservation, benefiting all faithful members. Suppression is high (0.78) due to the strong doctrinal enforcement required to shift a deeply ingrained practice and belief, but this is seen as righteous enforcement of divine will. Theater ratio is low (0.12) as the reinterpretation is considered a genuine, functional act of revelation, not a performance. Accessibility collapse is high (0.88) and resistance low (0.08) because divine command leaves little room for legitimate alternatives or internal resistance within the faithful framework.
 *
 * PERSPECTIVAL GAP:
 *   Other readings (exogenous override, hybrid pragmatic) would compute significantly higher extractiveness and suppression, identifying victims and seeing the federal government as a primary coercive agent. This reading, however, emphasizes divine agency and institutional continuity, leading to a 'mountain' claim with low extraction from its internal perspective. The engine's classification will measure the divergence between this claim and the structural metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Church leadership is a primary beneficiary (d near 0.0) as their prophetic authority and institutional legitimacy are preserved and reinforced by this reading. Faithful members are also beneficiaries (d near 0.0) as they receive divine guidance and benefit from the Church's continued existence and mission. The federal government is an observer, as their actions are merely the occasion for divine intervention, not the source of the constraint's legitimacy. No victims are identified from this reading's internal perspective, as the change is for 'higher purposes'.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading actively prevents mandatrophy by reinterpreting the mandate itself. Instead of the original mandate (plural marriage) becoming obsolete, the divine mandate is understood to have evolved, preserving the Church's core function (prophetic guidance, covenant community) through adaptation. The constraint's persistence is justified by its renewed divine purpose, not by inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_vs_human_agency_ambiguity,
    'To what extent was the Manifesto a genuine divine command versus a human adaptation to overwhelming external pressure?',
    'Comparative theological analysis of prophetic claims under duress, historical examination of internal deliberations versus external pressures, and analysis of subsequent theological justifications.',
    'If primarily human adaptation, the ''emerges_naturally'' claim would be undermined, shifting the constraint from a ''mountain'' towards a ''tangled_rope'' or ''snare'' from an external perspective, with higher extractiveness from those who felt coerced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_vs_human_agency_ambiguity, conceptual, 'Ambiguity of divine vs. human agency in the Manifesto''s origin.').

omega_variable(
    coercion_vs_revelation_causality,
    'Was federal pressure merely the occasion for a pre-existing divine will to be revealed, or was it the direct cause of the change, subsequently rationalized as revelation?',
    'Analysis of internal Church records predating the Manifesto for evidence of theological shifts, and comparison with other religious groups'' responses to similar external pressures.',
    'If federal pressure was the direct cause, the ''low extractiveness'' claim would be challenged, as the change would be seen as imposed rather than divinely beneficial, increasing the effective extraction for those who complied under duress.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_revelation_causality, empirical, 'Causality ambiguity: federal coercion vs. divine revelation.').

omega_variable(
    theological_continuity_vs_rupture,
    'Does this reinterpretation genuinely preserve theological continuity, or does it represent a fundamental rupture in the Church''s understanding of marriage and covenant?',
    'Longitudinal study of theological discourse and member beliefs post-Manifesto, comparing with pre-Manifesto doctrine and practices.',
    'If a rupture, the ''beneficiary'' status of faithful members would be challenged, as the change would represent a loss of prior covenant understanding, increasing their effective extraction and potentially shifting the constraint towards a ''snare'' for those who felt their prior commitments were invalidated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theological_continuity_vs_rupture, conceptual, 'Ambiguity of theological continuity vs. rupture in reinterpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 1890, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1890, 0.12).
narrative_ontology:measurement(marr_tr_t1900, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1900, 0.11).
narrative_ontology:measurement(marr_tr_t1910, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1910, 0.1).
narrative_ontology:measurement(marr_tr_t1920, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1920, 0.1).
narrative_ontology:measurement(marr_tr_t1930, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1930, 0.11).
narrative_ontology:measurement(marr_tr_t1940, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1940, 0.11).
narrative_ontology:measurement(marr_tr_t1950, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1950, 0.12).

% Extraction over time
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1890, 0.15).
narrative_ontology:measurement(marr_be_t1900, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1900, 0.14).
narrative_ontology:measurement(marr_be_t1910, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1910, 0.13).
narrative_ontology:measurement(marr_be_t1920, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1920, 0.13).
narrative_ontology:measurement(marr_be_t1930, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1930, 0.14).
narrative_ontology:measurement(marr_be_t1940, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1940, 0.14).
narrative_ontology:measurement(marr_be_t1950, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1950, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1890, 0.78).
narrative_ontology:measurement(marr_su_t1900, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1900, 0.79).
narrative_ontology:measurement(marr_su_t1910, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1910, 0.79).
narrative_ontology:measurement(marr_su_t1920, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1920, 0.78).
narrative_ontology:measurement(marr_su_t1930, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1930, 0.78).
narrative_ontology:measurement(marr_su_t1940, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1940, 0.77).
narrative_ontology:measurement(marr_su_t1950, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1950, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'marriage_commitment_legitimacy' kernel. This 'endogenous reinterpretation' reading emphasizes divine command and theological continuity, contrasting with the 'exogenous override' (federal coercion) and 'hybrid pragmatic' (strategic adaptation) readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
