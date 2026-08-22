% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__liturgical_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_living_language__liturgical_continuity_reading, []).

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
 *   constraint_id: hebrew_living_language__liturgical_continuity_reading
 *   human_readable: Hebrew as Living Language via Unbroken Liturgical Recitation and Textual Study
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint story captures the liturgical continuity reading of the
 *   Hebrew living language kernel: the claim that Hebrew remained a living
 *   language across two millennia of diaspora through unbroken liturgical
 *   recitation and textual study, without requiring native daily speech. The
 *   constraint is the standing arrangement of liturgical Hebrew as the
 *   vehicle of continuity — assessed by this reading's own lights.
 *   Extractiveness is low (0.15) because participation is voluntary, the
 *   symbolic capital circulates within the community, and no party extracts
 *   rents from the arrangement. Suppression is negligible (0.05) because
 *   alternatives (vernaculars, other liturgical languages) were never
 *   actively suppressed by this constraint; the constraint's persistence
 *   derives from internal communal valuation, not coercion. Theater ratio is
 *   low (0.1) because the liturgical function is genuine, not performative
 *   cover. The mountain claim asserts this continuity is a structural
 *   property of the language-tradition system, not a human policy choice —
 *   hence emerges_naturally: true.
 *
 * KEY AGENTS:
 *   - liturgical_communities: Primary beneficiary (organized/identity_locked) — maintains the tradition, receives identity and continuity
 *   - traditional_scholars: Primary beneficiary (organized/identity_locked) — custodians of textual study, interpretive authority
 *   - diaspora_jewish_populations: Secondary beneficiary (organized/constrained) — inherits the symbolic capital without direct liturgical obligation
 *   - linguistic_observers: Observer (analytical/analytical) — analyzes the continuity claim from outside the tradition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__liturgical_continuity_reading, 0.15).
domain_priors:suppression_score(hebrew_living_language__liturgical_continuity_reading, 0.05).
domain_priors:theater_ratio(hebrew_living_language__liturgical_continuity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__liturgical_continuity_reading, mountain).
narrative_ontology:human_readable(hebrew_living_language__liturgical_continuity_reading, "Hebrew as Living Language via Unbroken Liturgical Recitation and Textual Study").
narrative_ontology:topic_domain(hebrew_living_language__liturgical_continuity_reading, "historical_linguistics/language_revitalization/commitment_systems").

domain_priors:emerges_naturally(hebrew_living_language__liturgical_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__liturgical_continuity_reading, 'e058cb11-07d6-4ddd-a41b-d07b37be7546').
narrative_ontology:cs_kernel_codification('e058cb11-07d6-4ddd-a41b-d07b37be7546', fixed_text).
narrative_ontology:cs_authority_grounding('e058cb11-07d6-4ddd-a41b-d07b37be7546', lineage).
narrative_ontology:cs_interpretation_layer_present('e058cb11-07d6-4ddd-a41b-d07b37be7546').
narrative_ontology:cs_reading_relation('e058cb11-07d6-4ddd-a41b-d07b37be7546', hebrew_living_language__native_generation_reading, coexists_with).
narrative_ontology:cs_reading_relation('e058cb11-07d6-4ddd-a41b-d07b37be7546', hebrew_living_language__literary_revival_reading, coexists_with).
narrative_ontology:cs_axiom('e058cb11-07d6-4ddd-a41b-d07b37be7546', foundational, liturgical_recitation_sustains_generative_liveness).
narrative_ontology:cs_axiom_status(liturgical_recitation_sustains_generative_liveness, holdable).
narrative_ontology:cs_axiom_grounding('e058cb11-07d6-4ddd-a41b-d07b37be7546', liturgical_recitation_sustains_generative_liveness, deontological).
narrative_ontology:cs_axiom('e058cb11-07d6-4ddd-a41b-d07b37be7546', foundational, textual_continuity_requires_no_native_speech).
narrative_ontology:cs_axiom_status(textual_continuity_requires_no_native_speech, holdable).
narrative_ontology:cs_axiom_grounding('e058cb11-07d6-4ddd-a41b-d07b37be7546', textual_continuity_requires_no_native_speech, deontological).
narrative_ontology:cs_reference_frame('e058cb11-07d6-4ddd-a41b-d07b37be7546', sinaitic_revelation_continuity).
narrative_ontology:cs_drift_state('e058cb11-07d6-4ddd-a41b-d07b37be7546', modern_secular_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e058cb11-07d6-4ddd-a41b-d07b37be7546', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, liturgical_communities).
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, traditional_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, diaspora_jewish_populations).
narrative_ontology:constraint_vindicates(hebrew_living_language__liturgical_continuity_reading, textual_continuity_suffices_for_liveness).
narrative_ontology:constraint_vindicates(hebrew_living_language__liturgical_continuity_reading, liturgical_recitation_preserves_generative_capacity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities that maintain daily and festival liturgical recitation in Hebrew across diaspora. The language is the medium of prayer, lifecycle ritual, and communal cohesion. Participation is voluntary but identity-constituting; exit means leaving the community's self-understanding. They receive symbolic capital (continuity, covenantal identity) and bear devotional costs (study, transmission).
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, liturgical_communities, beneficiary,
    organized, generational, identity_locked, global).

% Custodians of textual study (Torah, Talmud, responsa, piyyut) who interpret, transmit, and innovate within the liturgical language. They set the standards of correct recitation and textual interpretation. Their authority derives from mastery of the tradition; exit would mean abandoning the epistemic community that constitutes their role.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, traditional_scholars, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__liturgical_continuity_reading, traditional_scholars, agenda_setter).

% Broader populations who inherit Hebrew as symbolic heritage — liturgical literacy varies, but the language functions as a boundary marker and continuity claim. They benefit from the tradition's maintenance without direct liturgical obligation. Exit is constrained by communal belonging; the language is part of the collective identity package.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, diaspora_jewish_populations, beneficiary,
    organized, generational, constrained, global).

% Scholars of historical linguistics, language revitalization, and commitment systems who analyze the Hebrew case from outside the tradition. They neither collect nor pay; they evaluate the continuity claim against comparative evidence (Ge'ez, Syriac, Sanskrit, etc.). Their exit is analytical — they can change frameworks without personal cost.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, linguistic_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective identity and covenantal continuity across diaspora and time through a shared liturgical language that requires no central enforcement — the text and its recitation are the coordination mechanism.
% TRANSFER_FUNCTION: Moves devotional effort (study, recitation, transmission) from participants into symbolic capital (continuity, identity, interpretive authority) that circulates within the community. No material extraction; the transfer is internal to the symbolic economy.
% ABSENT_VOICES: Those who experienced Hebrew only as an opaque liturgical barrier (e.g., uneducated members of traditional communities, women historically excluded from advanced textual study) — their absence from the custodial conversation is structural, not incidental. Also absent: the native_generation_reading proponents who would argue liturgical Hebrew is not 'living' in the linguistic sense.
% DISAPPEARANCE_RATIONALE: If the liturgical continuity constraint vanished overnight, the primary vehicle of Hebrew's textual transmission across two millennia would dissolve. The language might survive as academic object or national symbol, but the unbroken chain of recitation and study that this reading identifies as the source of its liveness would be severed — the world of the tradition rearranges.
% FOUNDING_PROBLEM: Preserving the covenantal language of revelation and prayer across exile, dispersion, and vernacular shift — maintaining a textual standard that could survive without a territorial base or native speech community.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the tradition's own self-understanding (liturgical communities, traditional scholars) AND by external scholars of language preservation (e.g., Joshua Fishman on Hebrew as unique case of revival via liturgical maintenance; sociolinguistic literature on 'sacred language' as survival mechanism). The external corroboration confirms the problem's structural reality, not just the tradition's self-narrative.
narrative_ontology:disappearance_verdict(hebrew_living_language__liturgical_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__liturgical_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__liturgical_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(hebrew_living_language__liturgical_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__liturgical_continuity_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__liturgical_continuity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, ExtMetricName, E),
    domain_priors:suppression_score(hebrew_living_language__liturgical_continuity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hebrew_living_language__liturgical_continuity_reading),
    narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hebrew_living_language__liturgical_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics describe a constraint with minimal extraction, negligible suppression, and low theatricality — consistent with a mountain. Accessibility_collapse (0.3) is moderate: alternatives (vernacular shift, language death) existed but the liturgical channel remained open; the constraint did not collapse them. Resistance (0.1) is low: the arrangement meets little active opposition because participation is voluntary and the costs are internalized as devotion. The claimed_type mountain and the authored metrics are independent — the claim is structural (this is a natural-law-like continuity), the metrics are descriptive (this is how the system operates). FSM is triggered by beneficiary declarations on a mountain; the omegas document the ambiguity.
 *
 * PERSPECTIVAL GAP:
 *   From inside the liturgical community, the constraint is experienced as gift and birthright — directionality near 0 (beneficiary). From the analytical seat, it appears as a self-sustaining symbolic system with near-zero extraction. The engine will compute per-seat types from the structural data; the divergence between the community's lived experience (mountain-as-gift) and the analytical classification (mountain-as-structure) is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: liturgical_communities and traditional_scholars — they maintain the system and receive its primary symbolic returns (identity, authority, continuity). Their exit is identity_locked: leaving means abandoning the self-concept constituted through the tradition. No victims declared: participation is voluntary, costs are devotional not extractive. Vindicated propositions are the continuity claim itself and the generative-capacity thesis — these are doctrines, not actors, and correctly belong in vindicated_propositions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserving covenantal language across exile) remains live per this reading — the mandate has not atrophied. The constraint is not a degraded institution; it is the active vehicle of the tradition's self-understanding. Mandatrophy is not resolved because the problem persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_continuity,
    'Is Hebrew''s continuity through liturgy a genuine natural-law-like property of the language system, or a constructed constraint benefiting identifiable communities?',
    'Comparative analysis of other liturgical-only languages that did not revive (e.g., Ge''ez, Classical Syriac) vs. Hebrew''s unique trajectory; test whether the continuity claim predicts revival outcomes.',
    'If constructed, the mountain claim is a false summit masking beneficiary capture; if natural, the low extractiveness is intrinsic to the linguistic structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_continuity, conceptual, 'Whether the constraint''s naturalness is intrinsic or conferred by beneficiary communities').

omega_variable(
    kernel_reading_identity,
    'This constraint is one reading (liturgical_continuity_reading) of the contested kernel hebrew_living_language. What would the sibling readings (native_generation_reading, literary_revival_reading) change structurally?',
    'Formalize each reading as a separate constraint story with its own ε, stakeholder set, and cs_structure; compare computed per-seat types.',
    'Sibling readings instantiate different constraints: native_generation_reading declares a victim set (those without native acquisition) and higher ε; literary_revival_reading declares different beneficiaries (maskilim, Hebrew writers). The kernel_id links them; the engine computes divergence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment structure: this reading of hebrew_living_language kernel; siblings are native_generation_reading and literary_revival_reading').

omega_variable(
    generative_capacity_from_recitation,
    'Does liturgical recitation and textual study alone sustain generative linguistic competence, or does it only preserve receptive/performative capacity?',
    'Psycholinguistic studies of liturgical communities'' productive vs. receptive abilities; historical evidence of generative innovation within liturgical Hebrew (piyyut, responsa, new lexical formations).',
    'If generative capacity is not sustained, the continuity claim is performative — the constraint would compute as piton (theatrical maintenance of a degraded function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generative_capacity_from_recitation, empirical, 'Whether liturgical practice sustains full generative competence or only ritual performance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__liturgical_continuity_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(hebr_tr_t500, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 500, 0.08).
narrative_ontology:measurement(hebr_tr_t1000, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(hebr_tr_t1500, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1500, 0.1).
narrative_ontology:measurement(hebr_tr_t2000, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 2000, 0.1).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(hebr_be_t500, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 500, 0.08).
narrative_ontology:measurement(hebr_be_t1000, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1000, 0.1).
narrative_ontology:measurement(hebr_be_t1500, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1500, 0.12).
narrative_ontology:measurement(hebr_be_t2000, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 2000, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 0, 0.02).
narrative_ontology:measurement(hebr_su_t500, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 500, 0.03).
narrative_ontology:measurement(hebr_su_t1000, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 1000, 0.04).
narrative_ontology:measurement(hebr_su_t1500, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 1500, 0.05).
narrative_ontology:measurement(hebr_su_t2000, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 2000, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__liturgical_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language__native_generation_reading).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language__literary_revival_reading).

% DUAL FORMULATION NOTE:
% The hebrew_living_language kernel decomposes into three constraint stories: this liturgical continuity reading (mountain, low ε), the native generation reading (likely tangled_rope or snare, higher ε, victims = non-native-acquirers), and the literary revival reading (rope or scaffold, beneficiaries = maskilim/Hebrew writers). They are linked by network.affects_constraints. Each has distinct ε, stakeholders, and cs_structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
