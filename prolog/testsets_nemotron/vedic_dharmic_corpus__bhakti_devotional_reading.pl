% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__bhakti_devotional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_dharmic_corpus__bhakti_devotional_reading, []).

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
 *   constraint_id: vedic_dharmic_corpus__bhakti_devotional_reading
 *   human_readable: Bhakti Devotional Access — Direct Divine Access Bypasses Caste
 *   domain: religious/social/interpretive
 *
 * SUMMARY:
 *   The bhakti devotional reading asserts that sincere devotion (bhakti) to
 *   the divine grants direct spiritual access and authority, bypassing the
 *   caste-based qualifications of the hereditary Brahmin monopoly.
 *   Historically, this reading powered movements from the Alvars and Nayanars
 *   in South India to the Sant traditions in the North, authorizing
 *   non-Brahmin saints, women poets, and Dalit devotees as teachers and
 *   theologians. The constraint operates as a coordination rope: it solves a
 *   genuine collective-action problem (how diverse communities access
 *   legitimate spiritual authority) with moderate extraction (some ritual
 *   functions remain gatekept) and active enforcement (temple institutions
 *   police the boundary between devotional teaching and ritual priesthood).
 *   The victim set is real but partial — orthodox Brahmin authority loses
 *   interpretive monopoly but retains ritual control; Dalit devotees gain
 *   theological warrant but not full institutional inclusion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__bhakti_devotional_reading, 0.4).
domain_priors:suppression_score(vedic_dharmic_corpus__bhakti_devotional_reading, 0.25).
domain_priors:theater_ratio(vedic_dharmic_corpus__bhakti_devotional_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__bhakti_devotional_reading, rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__bhakti_devotional_reading, "Bhakti Devotional Access — Direct Divine Access Bypasses Caste").
narrative_ontology:topic_domain(vedic_dharmic_corpus__bhakti_devotional_reading, "religious/social/interpretive").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__bhakti_devotional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__bhakti_devotional_reading, 'b90a2250-0010-480a-b838-9e235b030729').
narrative_ontology:cs_kernel_codification('b90a2250-0010-480a-b838-9e235b030729', fixed_text).
narrative_ontology:cs_authority_grounding('b90a2250-0010-480a-b838-9e235b030729', lineage).
narrative_ontology:cs_interpretation_layer_present('b90a2250-0010-480a-b838-9e235b030729').
narrative_ontology:cs_reading_relation('b90a2250-0010-480a-b838-9e235b030729', vedic_dharmic_corpus__hereditary_monopoly_reading, coexists_with).
narrative_ontology:cs_reading_relation('b90a2250-0010-480a-b838-9e235b030729', vedic_dharmic_corpus__reformist_egalitarian_reading, coexists_with).
narrative_ontology:cs_axiom('b90a2250-0010-480a-b838-9e235b030729', foundational, devotion_supersedes_birth_for_spiritual_authority).
narrative_ontology:cs_axiom_status(devotion_supersedes_birth_for_spiritual_authority, holdable).
narrative_ontology:cs_axiom_grounding('b90a2250-0010-480a-b838-9e235b030729', devotion_supersedes_birth_for_spiritual_authority, deontological).
narrative_ontology:cs_axiom('b90a2250-0010-480a-b838-9e235b030729', foundational, divine_grace_is_universally_accessible).
narrative_ontology:cs_axiom_status(divine_grace_is_universally_accessible, holdable).
narrative_ontology:cs_axiom_grounding('b90a2250-0010-480a-b838-9e235b030729', divine_grace_is_universally_accessible, theological).
narrative_ontology:cs_reference_frame('b90a2250-0010-480a-b838-9e235b030729', classical_varnashrama_dharma).
narrative_ontology:cs_drift_state('b90a2250-0010-480a-b838-9e235b030729', medieval_bhakti_movements, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b90a2250-0010-480a-b838-9e235b030729', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, devotional_practitioners).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, non_brahmin_teachers).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, orthodox_brahmin_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, temple_institutions).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__bhakti_devotional_reading, devotional_equality).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__bhakti_devotional_reading, divine_grace_universality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Practitioners from all castes who adopt sincere bhakti as their path; they gain direct spiritual authority and access to divine without requiring Brahmin mediation or ritual orthodoxy. Their exit is mobile — they can practice individually or form communities without institutional permission.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, devotional_practitioners, beneficiary,
    moderate, biographical, mobile, regional).

% Teachers and saints from non-Brahmin backgrounds who gain spiritual authority and following through demonstrated devotion rather than lineage; they still face social resistance from orthodox structures but the devotional framework legitimizes their position.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, non_brahmin_teachers, beneficiary,
    moderate, biographical, constrained, regional).

% Traditional Brahmin priesthood and scholarly lineages whose hereditary monopoly on ritual authority, Vedic interpretation, and spiritual gatekeeping is challenged by the bhakti claim that devotion supersedes birth. Their identity is fused with the hereditary claim; exit would mean abandoning the self-concept of being the ordained custodians of dharma.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, orthodox_brahmin_authority, payer,
    institutional, generational, identity_locked, regional).

% Established temple complexes and mathas that administer ritual, control endowments, and authenticate lineages; they incorporate bhakti saints into their fold (coordination) while defending hereditary ritual rights (extraction). They could reform but the cost of restructuring endowment governance and priestly succession is prohibitive.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, temple_institutions, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__bhakti_devotional_reading, temple_institutions, payer).

% Modern reformers and constitutional scholars who view bhakti as a historical precedent for equality but argue it does not go far enough — they would replace devotional authority with rational-legal equality. They are not bound by the constraint but study its dynamics.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, reformist_intellectuals, observer,
    organized, biographical, arbitrage, national).

% Dalit communities who adopt bhakti sincerely but still face temple entry bans, separate worship spaces, and ritual pollution doctrines; the devotional reading's promise of universal access is not fully honored in practice. They would object to the gap between doctrine and enforcement but are not seated in the interpretive conversation.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, excluded_dalit_devotees, excluded,
    powerless, biographical, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal spiritual path that coordinates diverse communities around shared devotional practice, resolving the coordination problem of how non-Brahmin groups access legitimate spiritual authority without dismantling the entire ritual order.
% TRANSFER_FUNCTION: Transfers interpretive authority and spiritual legitimacy from hereditary Brahmin lineages to sincere devotees regardless of birth; the transfer is partial — ritual priesthood remains largely hereditary while devotional teaching authority opens.
% ABSENT_VOICES: Dalit devotees who experience the gap between bhakti's universal promise and persistent temple exclusion; women devotees whose spiritual authority is recognized in poetry but not in institutional leadership; both are structurally excluded from the seats that authenticate the reading.
% DISAPPEARANCE_RATIONALE: If the bhakti reading vanished, millions of devotees would lose their primary legitimizing framework for spiritual authority outside birth; non-Brahmin teacher lineages would lose scriptural warrant; temple institutions would revert to unchallenged hereditary control; the devotional literary and musical traditions would lose their theological anchor.
% FOUNDING_PROBLEM: The rigid varna hierarchy excluded the vast majority from direct spiritual authority and ritual participation; bhakti offered a path where sincere devotion, not birth, qualified one for divine access and the right to teach.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the devotional traditions themselves (Alvars, Nayanars, Sant traditions) and by modern scholars outside the beneficiary set (e.g., Eleanor Zelliot on Dalit bhakti, David Lorenzen on bhakti's social morphology) who document the historical exclusion and the devotional response without sharing the theological commitments of the traditions.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__bhakti_devotional_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__bhakti_devotional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__bhakti_devotional_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(vedic_dharmic_corpus__bhakti_devotional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__bhakti_devotional_reading, 0.4, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__bhakti_devotional_reading_tests).
:- end_tests(vedic_dharmic_corpus__bhakti_devotional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.40) because the reading transfers significant authority (teaching, theological interpretation, community leadership) away from birth-based claimants, but leaves the core ritual priesthood and temple endowment control intact. Suppression is low-moderate (0.25) because the reading's persistence does not depend on coercion — devotional communities form voluntarily — though temple institutions do suppress the full institutional implications (e.g., ordaining non-Brahmin priests). Theater ratio (0.30) reflects that temple institutions perform inclusivity (honoring bhakti saints) while structurally preserving hereditary ritual rights. Accessibility collapse (0.45) is moderate: the devotional path is genuinely open, but social and ritual barriers persist for full institutional participation. Resistance (0.55) is significant from orthodox institutions defending hereditary prerogatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Devotional practitioners and non-Brahmin teachers are beneficiaries (d near 0.0–0.2): they gain authority and access without paying extraction. Orthodox Brahmin authority is the primary payer (d near 0.8–0.9): their hereditary monopoly is the extraction source. Temple institutions are dual-positioned (agenda_setter + payer): they administer the constraint and absorb some extraction (loss of interpretive control) while retaining ritual control. Excluded Dalit devotees are trapped (d near 1.0): they bear the cost of the gap between doctrine and practice without the full benefit. Reformist intellectuals are analytical observers (d = 0.5): they study the dynamic without structural stakes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (exclusion from spiritual authority) remains live — caste hierarchy persists, and devotional access remains a primary legitimizing framework for non-Brahmin spiritual leadership. The constraint has not atrophied into a piton; its coordination function is active and its extraction is stable, not accumulating. Mandatrophy is not resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    devotional_institutional_capture,
    'To what extent have temple institutions captured the bhakti reading by domesticating its saints into the hereditary priesthood structure, neutralizing its extractive challenge?',
    'Historical analysis of saint lineage succession — whether mathas appoint hereditary successors to bhakti gurus, and whether temple endowments control devotional teaching appointments.',
    'If capture is extensive, the constraint''s effective extractiveness is lower than authored (the coordination function is real but the extraction challenge is contained); if capture is limited, the bhakti reading remains an active destabilizer of hereditary authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(devotional_institutional_capture, empirical, 'Whether institutional domestication has neutralized the bhakti challenge to hereditary monopoly.').

omega_variable(
    bhakti_reading_kernel_identity,
    'Is this constraint one reading of the contested vedic_dharmic_corpus kernel, and how does its structural profile differ from the hereditary_monopoly_reading and reformist_egalitarian_reading?',
    'Compare the three readings'' beneficiary/victim structures, extractiveness profiles, and coordination functions across the same historical interval; the kernel''s contested elements are the locus of structural delta.',
    'Confirms this reading''s ε-invariance: its extractiveness (~0.40), coordination rope profile, and partial victim set are properties of THIS reading, not of the kernel as a whole. The hereditary reading would show higher extraction and clearer victims; the reformist reading would show lower extraction and different coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bhakti_reading_kernel_identity, conceptual, 'Committers-frame omega: this constraint instantiates the bhakti_devotional_reading of kernel vedic_dharmic_corpus; sibling readings are hereditary_monopoly_reading and reformist_egalitarian_reading.').

omega_variable(
    ritual_priesthood_vs_devotional_authority_separability,
    'Are ritual priesthood (hereditary) and devotional teaching authority (meritocratic) structurally separable functions, or does the bhakti reading''s claim to the latter inevitably undermine the former?',
    'Track whether traditions that grant devotional authority to non-Brahmins simultaneously open priestly ordination, or maintain the priesthood as a separate hereditary track.',
    'If separable, the constraint is a genuine coordination rope with moderate extraction; if inseparable, the bhakti reading is a scaffold whose sunset is the full opening of priesthood — the current moderate extraction is transitional.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ritual_priesthood_vs_devotional_authority_separability, conceptual, 'Whether the two authority tracks can coexist or whether devotional opening necessarily collapses ritual heredity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__bhakti_devotional_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bhakti_dev_tr_t0, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(bhakti_dev_tr_t10, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(bhakti_dev_tr_t20, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(bhakti_dev_tr_t30, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(bhakti_dev_tr_t40, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(bhakti_dev_tr_t50, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 50, 0.3).

% Extraction over time
narrative_ontology:measurement(bhakti_dev_be_t0, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bhakti_dev_be_t10, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(bhakti_dev_be_t20, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 20, 0.39).
narrative_ontology:measurement(bhakti_dev_be_t30, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 30, 0.4).
narrative_ontology:measurement(bhakti_dev_be_t40, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(bhakti_dev_be_t50, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 50, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(bhakti_dev_su_t0, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(bhakti_dev_su_t10, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 10, 0.18).
narrative_ontology:measurement(bhakti_dev_su_t20, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement(bhakti_dev_su_t30, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 30, 0.25).
narrative_ontology:measurement(bhakti_dev_su_t40, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 40, 0.25).
narrative_ontology:measurement(bhakti_dev_su_t50, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 50, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__bhakti_devotional_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_dharmic_corpus__bhakti_devotional_reading, 0.08).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus__hereditary_monopoly_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus__reformist_egalitarian_reading).

% DUAL FORMULATION NOTE:
% The vedic_dharmic_corpus kernel decomposes into three constraint stories. The bhakti reading coordinates identity (devotional community membership) with moderate extraction (partial authority transfer). The hereditary reading extracts substantially from non-Brahmins to maintain ritual monopoly. The reformist reading coordinates rational-legal equality with low extraction but faces high suppression from traditional authority. All three share the same textual corpus but instantiate different constraints with different ε, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vedic_dharmic_corpus__bhakti_devotional_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
