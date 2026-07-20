% ============================================================================
% CONSTRAINT STORY: kjv_text_1611__exclusive_inspiration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kjv_text_1611__exclusive_inspiration_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: kjv_text_1611__exclusive_inspiration_reading
 *   human_readable: KJV Exclusive Inspiration and Inerrancy Claim
 *   domain: religious_studies_textual_criticism_theology
 *
 * SUMMARY:
 *   The constraint claims that the King James Version of 1611 (and its
 *   revisions) is the exclusively inspired, inerrant English Bible, and that
 *   all other translations are corrupted or inferior. This claim functions as
 *   a commitment system that transfers textual authority from the broader
 *   church and biblical scholarship to a narrow circle of KJV-Only
 *   gatekeepers, who become the sole arbiters of what counts as legitimate
 *   scripture. Modern translation communities are suppressed as illegitimate,
 *   while KJV-Only community members are identity-locked into the constraint.
 *   The reading forecloses alternative bibliologies (revisable and
 *   functional-equivalence readings) and extracts authority through active
 *   gate-keeping.
 *
 * KEY AGENTS:
 *   - kjv_only_gatekeepers: Primary beneficiary (organized/identity_locked) â captures textual authority and institutional control
 *   - kjv_only_community_members: Primary target (powerless/identity_locked) â bears interpretive restriction and cognitive capture
 *   - modern_translation_communities: Excluded victim (organized/constrained) â suppressed as illegitimate and excluded from conversation
 *   - biblical_scholarship: Analytical observer (analytical) â attests manuscript evidence against exclusive claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_text_1611__exclusive_inspiration_reading, 0.82).
domain_priors:suppression_score(kjv_text_1611__exclusive_inspiration_reading, 0.88).
domain_priors:theater_ratio(kjv_text_1611__exclusive_inspiration_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_text_1611__exclusive_inspiration_reading, snare).
narrative_ontology:human_readable(kjv_text_1611__exclusive_inspiration_reading, "KJV Exclusive Inspiration and Inerrancy Claim").
narrative_ontology:topic_domain(kjv_text_1611__exclusive_inspiration_reading, "religious_studies_textual_criticism_theology").

domain_priors:requires_active_enforcement(kjv_text_1611__exclusive_inspiration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kjv_text_1611__exclusive_inspiration_reading, 'ae8c47d0-d0da-40a9-b2c4-e877dee94d2f').
narrative_ontology:cs_kernel_codification('ae8c47d0-d0da-40a9-b2c4-e877dee94d2f', fixed_text).
narrative_ontology:cs_authority_grounding('ae8c47d0-d0da-40a9-b2c4-e877dee94d2f', extraction).
narrative_ontology:cs_interpretation_layer_present('ae8c47d0-d0da-40a9-b2c4-e877dee94d2f').
narrative_ontology:cs_reading_relation('ae8c47d0-d0da-40a9-b2c4-e877dee94d2f', kjv_text_1611__revisable_translation_reading, forecloses).
narrative_ontology:cs_reading_relation('ae8c47d0-d0da-40a9-b2c4-e877dee94d2f', kjv_text_1611__functional_equivalence_reading, forecloses).
narrative_ontology:cs_axiom('ae8c47d0-d0da-40a9-b2c4-e877dee94d2f', foundational, kjv_english_inerrancy).
narrative_ontology:cs_axiom_status(kjv_english_inerrancy, holdable).
narrative_ontology:cs_axiom_grounding('ae8c47d0-d0da-40a9-b2c4-e877dee94d2f', kjv_english_inerrancy, theological).
narrative_ontology:cs_axiom('ae8c47d0-d0da-40a9-b2c4-e877dee94d2f', foundational, modern_translation_corruption_doctrine).
narrative_ontology:cs_axiom_status(modern_translation_corruption_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('ae8c47d0-d0da-40a9-b2c4-e877dee94d2f', modern_translation_corruption_doctrine, theological).
narrative_ontology:cs_reference_frame('ae8c47d0-d0da-40a9-b2c4-e877dee94d2f', kjv_perfected_preservation).
narrative_ontology:cs_drift_state('ae8c47d0-d0da-40a9-b2c4-e877dee94d2f', modern_translation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ae8c47d0-d0da-40a9-b2c4-e877dee94d2f', '').
narrative_ontology:cs_kernel_id(kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_text_1611__exclusive_inspiration_reading, kjv_only_gatekeepers).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, kjv_only_community_members).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, modern_translation_communities).
narrative_ontology:constraint_vindicates(kjv_text_1611__exclusive_inspiration_reading, textus_receptus_priority).
narrative_ontology:constraint_vindicates(kjv_text_1611__exclusive_inspiration_reading, divine_preservation_in_english).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% They set and enforce the doctrinal boundary that declares the KJV the exclusively inspired English Bible and all modern translations corrupted or inferior. They derive institutional authority, publishing revenue, conference platforms, and congregational loyalty from being the sole arbiters of textual legitimacy.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, kjv_only_gatekeepers, agenda_setter,
    organized, generational, identity_locked, national).

% They accept the KJV as their only legitimate scripture and depend on KJV-Only institutions for teaching. They bear the costs of restricted access to biblical scholarship, social shunning for questioning the claim, and cognitive capture that makes using non-KJV translations feel spiritually dangerous. Exit means leaving their faith community and often their family social network.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, kjv_only_community_members, payer,
    powerless, biographical, identity_locked, local).

% Communities and churches using NIV, ESV, NASB, and other modern translations. They are declared illegitimate and corrupted by the KJV-Only gatekeepers, excluded from the conversation as apostate or ignorant, and their translations are suppressed within KJV-Only institutions.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, modern_translation_communities, excluded,
    organized, biographical, constrained, global).

% Textual critics and biblical scholars who attest that manuscript evidence supports eclectic text traditions rather than Textus Receptus priority, and who observe that the exclusive-inspiration claim functions as an authority-consolidation mechanism rather than a descriptive bibliology.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, biblical_scholarship, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kjv_text_1611__exclusive_inspiration_reading, kjv_only_gatekeepers).
narrative_ontology:fixing_cost_class(kjv_text_1611__exclusive_inspiration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, stable, unchanging English text around which a religious community can unify doctrine and practice without ambiguity or revision.
% TRANSFER_FUNCTION: Moves textual authority and interpretive legitimacy from modern translation communities and individual believers to KJV-Only gatekeepers, who become the sole arbiters of scriptural correctness.
% ABSENT_VOICES: Biblical scholars, textual critics, and mainstream evangelical translators who advocate for multiple reliable translations based on eclectic manuscript evidence; they are excluded from the KJV-Only conversation and dismissed as corrupt or apostate.
% DISAPPEARANCE_RATIONALE: If the exclusive inspiration claim vanished, the KJV-Only community's boundary against the broader church would collapse, members would lose the doctrinal justification for rejecting modern translations, and gatekeepers would lose their unique claim to textual authority â the social architecture of the movement would reorganize around either general evangelical bibliology or the broader KJV-as-preference position.
% FOUNDING_PROBLEM: The proliferation of modern Bible translations in the 20th century created perceived doctrinal instability and erosion of confidence in the authority of scripture among some conservative Protestant communities.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream evangelical historians and textual critics attest that translation plurality reflects advancing knowledge rather than doctrinal instability, and that the crisis narrative was constructed by KJV-Only leadership to consolidate authority; no independent corroboration from outside the benefiting party supports the founding crisis framing.
narrative_ontology:disappearance_verdict(kjv_text_1611__exclusive_inspiration_reading, world_rearranges).
narrative_ontology:founding_problem_status(kjv_text_1611__exclusive_inspiration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kjv_text_1611__exclusive_inspiration_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kjv_text_1611__exclusive_inspiration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(kjv_text_1611__exclusive_inspiration_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_text_1611__exclusive_inspiration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kjv_text_1611__exclusive_inspiration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kjv_text_1611__exclusive_inspiration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the constraint concentrates scriptural authority in a narrow gatekeeper class, extracting interpretive submission and institutional loyalty from members. Suppression (0.88) is higher still because the constraint's persistence depends on actively delegitimizing all alternative translations and scholarship. Theater_ratio (0.70) is substantial: a large share of KJV-Only institutional activity (publishing, conference rhetoric, 'defense' ministries) performs loyalty to the KJV rather than advancing textual knowledge. Accessibility_collapse (0.85) is high because, within the community, the legitimacy of non-KJV texts collapses almost completely once the premise is accepted. Resistance (0.55) is moderate: mainstream evangelicals and scholars oppose the claim, but the identity-locked community is insulated from this resistance.
 *
 * PERSPECTIVAL GAP:
 *   The gatekeeper seat experiences the constraint as a rope preserving doctrinal purity and unity; the community-member seat experiences it as a snare restricting access to scholarship and binding their identity to a single translation. The modern-translation seat experiences it as an illegitimate suppression. The engine computes this divergence from the structural data â beneficiary/victim declarations, identity_locked exit, and active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   kjv_only_gatekeepers are declared beneficiaries (low d, subsidized by authority concentration). kjv_only_community_members are declared victims/payers with identity_locked exit (high d, amplified extraction). modern_translation_communities are excluded victims (high d, structural suppression). The identity_lock on members is critical: their exit is not merely constrained but fused with religious identity, pushing effective extraction toward the maximum.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â translation plurality as doctrinal crisis â is contested and likely a constructed narrative. Textual plurality is a normal feature of biblical history, not a novel instability. The constraint persists beyond any genuine coordination need, functioning primarily to maintain gatekeeper authority. This prevents mislabeling it as coordination: the unity it provides is sectarian separation, not genuine collective-action solving.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    manuscript_evidence_ambiguity,
    'Does the manuscript evidence actually support Textus Receptus priority and KJV exclusivity, or are these claims empirically unsustainable?',
    'Comprehensive review of papyri, uncials, and patristic citations by neutral textual criticism bodies; if Alexandrian and eclectic traditions are shown to carry superior empirical weight, the KJV-Only empirical premise collapses.',
    'If the manuscript claims are empirically falsified, the constraint loses its theological cover and reclassifies more firmly as pure extraction (snare); if supported, some tangled_rope element remains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manuscript_evidence_ambiguity, empirical, 'Whether the Textus Receptus priority claim is empirically sustainable.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (community shunning, institutional barriers) or internalized (identity fusion, cognitive patterns that persist after barrier removal)?',
    'Post-exit trajectory study of individuals who leave KJV-Only communities: if suppression of non-KJV texts persists after social removal, the mechanism is partially internalized.',
    'If internalized, effective extraction exceeds the structural measure because members carry the suppression with them; this would raise the computed extractiveness for the member seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism.').

omega_variable(
    coordination_or_cover,
    'Does the exclusive claim solve a genuine coordination problem (doctrinal unity), or is the unity story entirely cover for authority extraction?',
    'Compare doctrinal stability and community health metrics between KJV-Only and non-KJV-Only conservative communities; if no stability advantage is found, the coordination story is cover.',
    'If no genuine coordination is demonstrated, the constraint is a pure snare; if some unity is genuinely produced, the classification edges toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_or_cover, conceptual, 'Whether the constraint''s coordination story is genuine or cover.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_text_1611__exclusive_inspiration_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv__tr_t1950, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 1950, 0.25).
narrative_ontology:measurement(kjv__tr_t1965, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 1965, 0.4).
narrative_ontology:measurement(kjv__tr_t1980, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 1980, 0.62).
narrative_ontology:measurement(kjv__tr_t1995, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 1995, 0.68).
narrative_ontology:measurement(kjv__tr_t2010, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 2010, 0.7).
narrative_ontology:measurement(kjv__tr_t2024, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 2024, 0.7).

% Extraction over time
narrative_ontology:measurement(kjv__be_t1950, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 1950, 0.45).
narrative_ontology:measurement(kjv__be_t1965, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 1965, 0.58).
narrative_ontology:measurement(kjv__be_t1980, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 1980, 0.72).
narrative_ontology:measurement(kjv__be_t1995, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 1995, 0.78).
narrative_ontology:measurement(kjv__be_t2010, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 2010, 0.8).
narrative_ontology:measurement(kjv__be_t2024, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 2024, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(kjv__su_t1950, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 1950, 0.55).
narrative_ontology:measurement(kjv__su_t1965, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 1965, 0.7).
narrative_ontology:measurement(kjv__su_t1980, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 1980, 0.88).
narrative_ontology:measurement(kjv__su_t1995, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 1995, 0.9).
narrative_ontology:measurement(kjv__su_t2010, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 2010, 0.88).
narrative_ontology:measurement(kjv__su_t2024, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(kjv_text_1611__exclusive_inspiration_reading, revisable_translation_reading).
narrative_ontology:affects_constraint(kjv_text_1611__exclusive_inspiration_reading, functional_equivalence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kjv_text_1611 kernel. The exclusive_inspiration_reading claims the KJV is the only inspired English text; the revisable_translation_reading and functional_equivalence_reading are sibling readings that treat the KJV as improvable or as one complementary text among many. They are modeled as separate constraints because their epsilon values, beneficiary structures, and victim sets differ structurally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
