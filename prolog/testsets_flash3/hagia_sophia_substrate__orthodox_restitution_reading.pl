% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__orthodox_restitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_substrate__orthodox_restitution_reading, []).

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
 *   constraint_id: hagia_sophia_substrate__orthodox_restitution_reading
 *   human_readable: Hagia Sophia: Orthodox Restitution Claim
 *   domain: cultural_heritage/sovereignty/religious_authority
 *
 * SUMMARY:
 *   This constraint story instantiates the 'Orthodox Restitution' reading of
 *   the Hagia Sophia's status. It claims the site's legitimacy derives from
 *   its founding as a Christian cathedral and advocates for its return to
 *   Orthodox ecclesiastical control or a neutral status. While framed as a
 *   'mountain' due to its appeal to an immutable historical origin, its
 *   practical extractiveness is low (0.15) because it lacks any realistic
 *   enforcement mechanism against Turkish sovereignty. Its persistence is
 *   largely theatrical (theater_ratio 0.85), serving as a symbolic claim in
 *   ongoing geopolitical and cultural disputes. Suppression is negligible
 *   (0.05) as the claim itself does not actively suppress other parties, but
 *   rather exists as an external normative pressure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__orthodox_restitution_reading, 0.15).
domain_priors:suppression_score(hagia_sophia_substrate__orthodox_restitution_reading, 0.05).
domain_priors:theater_ratio(hagia_sophia_substrate__orthodox_restitution_reading, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 0.85).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__orthodox_restitution_reading, mountain).
narrative_ontology:human_readable(hagia_sophia_substrate__orthodox_restitution_reading, "Hagia Sophia: Orthodox Restitution Claim").
narrative_ontology:topic_domain(hagia_sophia_substrate__orthodox_restitution_reading, "cultural_heritage/sovereignty/religious_authority").

domain_priors:emerges_naturally(hagia_sophia_substrate__orthodox_restitution_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__orthodox_restitution_reading, '2a0d909e-c684-4110-9d93-07d0bf3ce55e').
narrative_ontology:cs_kernel_codification('2a0d909e-c684-4110-9d93-07d0bf3ce55e', fixed_text).
narrative_ontology:cs_authority_grounding('2a0d909e-c684-4110-9d93-07d0bf3ce55e', lineage).
narrative_ontology:cs_interpretation_layer_present('2a0d909e-c684-4110-9d93-07d0bf3ce55e').
narrative_ontology:cs_reading_relation('2a0d909e-c684-4110-9d93-07d0bf3ce55e', hagia_sophia_substrate__islamic_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('2a0d909e-c684-4110-9d93-07d0bf3ce55e', hagia_sophia_substrate__universal_heritage_reading, coexists_with).
narrative_ontology:cs_axiom('2a0d909e-c684-4110-9d93-07d0bf3ce55e', foundational, original_ecclesiastical_purpose_is_immutable).
narrative_ontology:cs_axiom_status(original_ecclesiastical_purpose_is_immutable, holdable).
narrative_ontology:cs_axiom_grounding('2a0d909e-c684-4110-9d93-07d0bf3ce55e', original_ecclesiastical_purpose_is_immutable, deontological).
narrative_ontology:cs_axiom('2a0d909e-c684-4110-9d93-07d0bf3ce55e', secondary, byzantine_heritage_supersedes_conquest_claims).
narrative_ontology:cs_axiom_status(byzantine_heritage_supersedes_conquest_claims, holdable).
narrative_ontology:cs_axiom_grounding('2a0d909e-c684-4110-9d93-07d0bf3ce55e', byzantine_heritage_supersedes_conquest_claims, deontological).
narrative_ontology:cs_reference_frame('2a0d909e-c684-4110-9d93-07d0bf3ce55e', byzantine_ecclesiastical_control).
narrative_ontology:cs_drift_state('2a0d909e-c684-4110-9d93-07d0bf3ce55e', contemporary_turkish_sovereignty, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('2a0d909e-c684-4110-9d93-07d0bf3ce55e', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, eastern_orthodox_diaspora).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, greek_state).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, turkish_sovereignty).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, islamic_worship_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives symbolic validation and a rallying point for cultural and religious identity. The claim reinforces their historical narrative and provides diplomatic leverage for the Greek state. Their connection is deeply rooted in religious and cultural heritage, making exit unthinkable.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, eastern_orthodox_diaspora, beneficiary,
    organized, generational, identity_locked, global).

% Gains diplomatic and ideological leverage in its historical tensions with Turkey. The claim is a tool in its foreign policy, even if direct restitution is not realistically achievable. It benefits from the symbolic weight of the claim without direct material gain.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, greek_state, beneficiary,
    institutional, generational, constrained, national).

% Bears the cost of an external normative claim on its national territory and cultural policy. The claim challenges its historical narrative and its right to determine the site's use. It cannot 'exit' the claim as it is directed at its sovereign actions.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, turkish_sovereignty, payer,
    institutional, generational, trapped, national).

% Would be interrupted if the site returned to Orthodox control or became neutral, reversing the 2020 conversion to a mosque. This group experiences the claim as a threat to their religious practice and historical continuity at the site.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, islamic_worship_continuity, payer,
    organized, biographical, identity_locked, local).

% Observe and comment on the dispute, often advocating for universal heritage status or preservation. They do not directly benefit or pay but their pronouncements can influence diplomatic pressure.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, international_cultural_organizations, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading attempts to coordinate a return to a historical status quo, aligning the site's use with its original Christian purpose and the identity of the Eastern Orthodox community.
% TRANSFER_FUNCTION: Symbolically transfers legitimacy and control over the site from Turkish state/Islamic authority back to Orthodox ecclesiastical control or a neutral status, affirming a specific historical narrative.
% ABSENT_VOICES: The current Turkish government and local Islamic communities are present but reject this claim; their voices are actively suppressed by the claim's premise of external authority over sovereign territory. Byzantine historians and archaeologists might offer a more nuanced view of the site's complex history, but their input is often instrumentalized by all sides.
% DISAPPEARANCE_RATIONALE: If this specific claim for Orthodox restitution vanished, the physical status of Hagia Sophia would remain unchanged, as the claim lacks practical enforcement. The underlying historical and religious tensions would persist, but this particular framing of the solution would no longer be a diplomatic or ideological tool.
% FOUNDING_PROBLEM: The problem of the Hagia Sophia's status originated with its conversion from a cathedral to a mosque in 1453, and its subsequent secularization in 1934, and reconversion to a mosque in 2020, creating a continuous dispute over its rightful religious and cultural identity.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested as live by the Eastern Orthodox Church, the Greek state, and various international cultural heritage organizations, all of whom view the current status as a deviation from its original purpose or universal heritage. This corroboration comes from outside the current benefiting parties (Turkish state/Islamic community).
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__orthodox_restitution_reading, world_unchanged).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__orthodox_restitution_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__orthodox_restitution_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hagia_sophia_substrate__orthodox_restitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__orthodox_restitution_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_substrate__orthodox_restitution_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, ExtMetricName, E),
    domain_priors:suppression_score(hagia_sophia_substrate__orthodox_restitution_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hagia_sophia_substrate__orthodox_restitution_reading),
    narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hagia_sophia_substrate__orthodox_restitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness reflects the lack of direct material gain or coercive power associated with this claim; it is primarily symbolic. The high theater ratio indicates that the claim's maintenance is more about ideological performance and diplomatic posturing than achieving a practical outcome. The claim's 'mountain' status is derived from its appeal to an unchangeable historical origin and its perceived natural right, despite its lack of current enforceability. Accessibility collapse is high (0.95) because, from this reading's perspective, the 'correct' status is self-evident and alternatives are illegitimate. Resistance is low (0.02) because the claim itself is not actively resisted, but rather the actions it opposes (Turkish sovereignty over the site) are resisted by the claim.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Orthodox diaspora, this is a just and natural claim for historical restitution. From the perspective of Turkish sovereignty, it is an illegitimate external interference in national affairs. The engine will compute these divergent classifications based on the declared roles and structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The Eastern Orthodox diaspora and the Greek state are beneficiaries, gaining symbolic and diplomatic leverage respectively. Turkish sovereignty and Islamic worship continuity are victims, as the claim directly challenges their control and practice at the site. The claim's directionality is primarily outward, asserting a normative right against an existing sovereign reality.
 *
 * MANDATROPHY ANALYSIS:
 *   The claim itself is not subject to mandatrophy in the traditional sense, as its 'mandate' is a timeless historical right. However, its practical irrelevance (low extractiveness, high theater) suggests a form of 'symbolic mandatrophy' where the claim persists as a cultural artifact rather than a functional constraint. The classification as a 'mountain' with beneficiaries and high theater ratio helps to identify this as a potential 'false summit' – a claim of naturalness that serves specific interests.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_claim,
    'Is the claim for Orthodox restitution a genuine natural law (a timeless historical right) or a constructed claim serving contemporary geopolitical and identity interests?',
    'Analysis of the claim''s historical evolution and its instrumentalization in modern diplomatic discourse, rather than its inherent historical ''truth''.',
    'If primarily constructed, the ''mountain'' classification would be challenged, potentially reclassifying it as a ''snare'' (if actively extractive) or ''piton'' (if purely inertial/theatrical).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_claim, conceptual, 'Ambiguity between a timeless historical right and a contemporary political instrument.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.05) truly negligible, or is there an internalized suppression among those who might otherwise support the claim but fear geopolitical repercussions?',
    'Qualitative sociological research among relevant communities to uncover latent support or self-censorship regarding the claim.',
    'If internalized suppression is significant, the constraint''s effective suppression is higher than the structural measure suggests, indicating a more coercive environment for expressing dissenting views.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for expressing support for the claim.').

omega_variable(
    framing_under_determination_hagia_sophia,
    'Does framing Hagia Sophia''s status as a ''restitution claim'' (this reading) obscure a more fundamental ''universal heritage'' framing, or vice-versa?',
    'Analysis of which framing generates more effective international consensus and action for the site''s preservation and accessibility, independent of national or religious claims.',
    'If the ''universal heritage'' framing is more robust, this ''restitution'' reading might be seen as a narrower, more extractive (in terms of excluding other claims) interpretation, potentially shifting its classification from a ''mountain'' to a ''tangled_rope'' or ''snare'' due to its exclusionary nature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_under_determination_hagia_sophia, conceptual, 'Alternative framings of Hagia Sophia''s status (restitution vs. universal heritage) and their impact on classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__orthodox_restitution_reading, 1923, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t1923, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 1923, 0.7).
narrative_ontology:measurement(hagi_tr_t1950, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 1950, 0.75).
narrative_ontology:measurement(hagi_tr_t1980, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 1980, 0.8).
narrative_ontology:measurement(hagi_tr_t2000, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 2000, 0.82).
narrative_ontology:measurement(hagi_tr_t2020, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 2020, 0.85).
narrative_ontology:measurement(hagi_tr_t2024, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 2024, 0.85).

% Extraction over time
narrative_ontology:measurement(hagi_be_t1923, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 1923, 0.1).
narrative_ontology:measurement(hagi_be_t1950, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 1950, 0.12).
narrative_ontology:measurement(hagi_be_t1980, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 1980, 0.13).
narrative_ontology:measurement(hagi_be_t2000, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 2000, 0.14).
narrative_ontology:measurement(hagi_be_t2020, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 2020, 0.15).
narrative_ontology:measurement(hagi_be_t2024, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t1923, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 1923, 0.05).
narrative_ontology:measurement(hagi_su_t1950, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 1950, 0.05).
narrative_ontology:measurement(hagi_su_t1980, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 1980, 0.05).
narrative_ontology:measurement(hagi_su_t2000, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 2000, 0.05).
narrative_ontology:measurement(hagi_su_t2020, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 2020, 0.05).
narrative_ontology:measurement(hagi_su_t2024, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 2024, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__orthodox_restitution_reading, identity_coordination).
narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate__islamic_sovereignty_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate__universal_heritage_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'hagia_sophia_substrate' kernel, each representing a distinct claim about the site's legitimate status. This 'orthodox_restitution_reading' focuses on its Christian origins and ecclesiastical control.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
