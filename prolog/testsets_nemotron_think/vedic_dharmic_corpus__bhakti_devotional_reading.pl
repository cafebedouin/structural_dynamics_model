% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__bhakti_devotional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   human_readable: Bhakti Devotional Access Overrides Caste Requirements
 *   domain: religious/social/interpretive
 *
 * SUMMARY:
 *   The bhakti devotional reading of the Vedic/dharmic corpus asserts that
 *   sincere devotion (bhakti) to the divine supersedes birth-based caste
 *   requirements for spiritual authority, ritual eligibility, and scriptural
 *   interpretation. This reading emerged historically from poet-saints
 *   (Alvars, Nayanars, Varkaris, Gaudiya Vaishnavas, etc.) who composed
 *   vernacular devotional poetry claiming direct divine access for all
 *   regardless of varna. The constraint coordinates spiritual community
 *   around devotional sincerity rather than ritual purity. However, the
 *   victim set shrinks but does not eliminate caste hierarchy: hereditary
 *   priesthood loses exclusive interpretive authority (payer), and residually
 *   excluded lower castes remain barred from full social participation
 *   despite devotional recognition (payer). No concentrated beneficiary class
 *   extracts rents — benefits are diffuse among devotees. The claimed type is
 *   rope (genuine coordination), but metrics show moderate extractiveness
 *   (0.40) from the partial capture of devotional discourse by dominant-caste
 *   institutions and the persistence of exclusion for the most marginalized.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__bhakti_devotional_reading, 0.4).
domain_priors:suppression_score(vedic_dharmic_corpus__bhakti_devotional_reading, 0.35).
domain_priors:theater_ratio(vedic_dharmic_corpus__bhakti_devotional_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__bhakti_devotional_reading, rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__bhakti_devotional_reading, "Bhakti Devotional Access Overrides Caste Requirements").
narrative_ontology:topic_domain(vedic_dharmic_corpus__bhakti_devotional_reading, "religious/social/interpretive").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__bhakti_devotional_reading, '4741ad99-35a8-4eb6-b099-d131743d373e').
narrative_ontology:cs_kernel_codification('4741ad99-35a8-4eb6-b099-d131743d373e', fixed_text).
narrative_ontology:cs_authority_grounding('4741ad99-35a8-4eb6-b099-d131743d373e', lineage).
narrative_ontology:cs_interpretation_layer_present('4741ad99-35a8-4eb6-b099-d131743d373e').
narrative_ontology:cs_reading_relation('4741ad99-35a8-4eb6-b099-d131743d373e', vedic_dharmic_corpus__hereditary_monopoly_reading, coexists_with).
narrative_ontology:cs_reading_relation('4741ad99-35a8-4eb6-b099-d131743d373e', vedic_dharmic_corpus__reformist_egalitarian_reading, coexists_with).
narrative_ontology:cs_axiom('4741ad99-35a8-4eb6-b099-d131743d373e', foundational, devotion_supersedes_birth).
narrative_ontology:cs_axiom_status(devotion_supersedes_birth, holdable).
narrative_ontology:cs_axiom_grounding('4741ad99-35a8-4eb6-b099-d131743d373e', devotion_supersedes_birth, theological).
narrative_ontology:cs_axiom('4741ad99-35a8-4eb6-b099-d131743d373e', secondary, divine_grace_universally_accessible).
narrative_ontology:cs_axiom_status(divine_grace_universally_accessible, holdable).
narrative_ontology:cs_axiom_grounding('4741ad99-35a8-4eb6-b099-d131743d373e', divine_grace_universally_accessible, theological).
narrative_ontology:cs_reference_frame('4741ad99-35a8-4eb6-b099-d131743d373e', bhakti_devotional_framework).
narrative_ontology:cs_drift_state('4741ad99-35a8-4eb6-b099-d131743d373e', contemporary_constitutional_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4741ad99-35a8-4eb6-b099-d131743d373e', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, bhakti_devotees).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, lower_caste_aspirants).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, hereditary_priesthood).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, residually_excluded_lower_castes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, lower_caste_aspirants).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__bhakti_devotional_reading, divine_grace_transcends_birth).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__bhakti_devotional_reading, devotional_sincerity_as_spiritual_qualification).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Articulate and transmit the devotional path; compose texts, establish lineages (sampradayas), and define what counts as sincere bhakti. Their authority derives from recognized spiritual attainment rather than institutional office. They can move between traditions or found new ones.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, bhakti_saints_teachers, agenda_setter,
    organized, generational, arbitrage, continental).

% Gain direct devotional access to the divine and spiritual community regardless of birth caste. The constraint coordinates their spiritual practice around shared devotion rather than ritual purity. Exit is easy — they can intensify, modify, or abandon devotional practice without structural penalty.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, bhakti_devotees, beneficiary,
    moderate, biographical, mobile, continental).

% Access spiritual authority and community through bhakti that would be denied by birth-based hierarchy. However, they still face residual social exclusion (temple entry barriers, marriage restrictions, occupational stigma) that the devotional reading does not fully dissolve. They pay continuing social costs while gaining spiritual recognition.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, lower_caste_aspirants, beneficiary,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__bhakti_devotional_reading, lower_caste_aspirants, payer).

% Lose exclusive control over ritual mediation and scriptural interpretation. Their birth-based authority is structurally challenged by the claim that devotion supersedes lineage. Exit from priestly identity is nearly impossible — it is fused with family continuity, ritual obligation, and community expectation. They bear the cost of the constraint's coordination function without collecting its new benefits.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, hereditary_priesthood, payer,
    institutional, generational, identity_locked, continental).

% Remain excluded from full participation despite the devotional opening — denied temple entry, priestly roles, or social equality because local power structures maintain caste barriers that the bhakti reading's textual authority does not reach. They cannot exit the constraint (it governs their social reality) and cannot access its benefits.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, residually_excluded_lower_castes, payer,
    powerless, biographical, trapped, local).

% Argue that textual meaning must conform to constitutional equality; they read the bhakti opening as incomplete and push for full egalitarian reinterpretation. They neither collect benefits nor pay costs from the devotional arrangement itself but shape its public reception and legal implications.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, reformist_interpreters, observer,
    organized, generational, analytical, national).

% Maintain that varna hierarchy is divinely ordained and textually prescribed; they would object to the bhakti reading's claim that devotion bypasses caste requirements. Their exclusion is structural — the devotional reading's legitimacy framework does not include birth-based authority as a valid interpretive seat.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, traditionalist_orthodox, excluded,
    institutional, generational, identity_locked, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a spiritual access pathway coordinated around sincere devotional practice (bhakti) rather than birth-ascribed ritual qualification, allowing diverse participants to organize worship, community, and textual interpretation without hereditary gatekeeping.
% TRANSFER_FUNCTION: Moves interpretive authority and ritual eligibility from hereditary priesthood to devotional practitioners; moves social recognition from birth-status to devotional sincerity. The transfer is partial — residual caste hierarchy persists in social domains the devotional reading does not govern.
% ABSENT_VOICES: Dalit and Adivasi communities who experience the devotional opening as rhetorically inclusive but materially incomplete — they would object that bhakti theology has been co-opted to legitimize hierarchy rather than dismantle it. They are excluded from the interpretive conversation by literacy barriers, linguistic marginalization, and the dominance of Sanskritized bhakti discourse.
% DISAPPEARANCE_RATIONALE: If the bhakti devotional reading vanished overnight, millions of devotees would lose their primary framework for spiritual legitimacy outside caste hierarchy; hereditary priesthood would reassert exclusive ritual authority; reformist movements would lose a key textual resource for egalitarian claims. The religious field would reorganize around either hardened birth-based orthodoxy or secular constitutional equality.
% FOUNDING_PROBLEM: The Vedic ritual system restricted spiritual authority and salvific access to twice-born males of the upper three varnas, excluding women, Shudras, and outcastes from direct divine relationship and scriptural knowledge. The bhakti movement emerged to solve this exclusion by positing devotion as a universal qualification.
% FOUNDING_PROBLEM_CORROBORATION: Bhakti traditions (Alvars, Nayanars, Varkaris, Gaudiya Vaishnavas) attest the founding problem is live — caste exclusion persists and devotion remains the primary bypass. Hereditary priesthood and traditionalist orthodox attest the problem is misdiagnosed — the varna system was never exclusionary but functional. Modern Dalit intellectuals (Ambedkar, contemporary scholars) corroborate from outside the beneficiary set that the founding exclusion was real and that the bhakti solution, while genuine, was historically contained and co-opted.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__bhakti_devotional_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__bhakti_devotional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__bhakti_devotional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
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
 *   Extractiveness is moderate (0.40) because the constraint does transfer authority from hereditary priesthood to devotional practitioners, but this transfer is incomplete and the devotional framework itself has been partially institutionalized by dominant castes. Suppression is low-moderate (0.35) — the constraint does not actively coerce; its persistence relies on textual authority and devotional commitment rather than enforcement. Theater ratio is low (0.25) — devotional practice is functionally real, not performative, though some institutional bhakti organizations exhibit performative compliance. Accessibility collapse is moderate (0.45) — alternatives (hereditary monopoly, secular equality) remain cognitively available. Resistance is moderate-high (0.55) — from hereditary priesthood defending birth-based authority and from reformists pushing for full egalitarian reinterpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the bhakti saint/seeker seat, the constraint is genuine coordination (rope) — a universal spiritual pathway. From the hereditary priesthood seat, it is extraction (snare) — their birthright authority is transferred without consent. From the residually excluded lower caste seat, it is a false promise — spiritual inclusion without social liberation. The engine computes this divergence from the structural data; the authored claim (rope) reflects the devotional reading's self-understanding.
 *
 * DIRECTIONALITY LOGIC:
 *   Bhakti saints/teachers are agenda_setters with arbitrage exit — they define the devotional path and can shift traditions. Bhakti devotees are beneficiaries with mobile exit — they gain spiritual access at low cost. Lower caste aspirants are dual-positioned: beneficiaries of spiritual recognition but payers of residual social exclusion, with constrained exit. Hereditary priesthood are payers with identity_locked exit — they lose exclusive authority but cannot exit priestly identity. Residually excluded lower castes are payers with trapped exit — they bear costs without benefits and cannot exit the constraint. Reformist interpreters are analytical observers. Traditionalist orthodox are excluded — their interpretive seat is not recognized by the devotional framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (caste exclusion from spiritual authority) remains contested — not dead, because caste hierarchy persists in social domains the devotional reading does not govern. The constraint has not become a piton: devotional practice remains functionally vibrant, not theatrically maintained. However, the partial capture of bhakti discourse by dominant-caste institutions creates a tangled_rope dynamic at the organizational level — genuine coordination at the devotional level, asymmetric extraction at the institutional level.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'How does the bhakti_devotional_reading''s structural classification change when evaluated against its sibling readings of the same kernel?',
    'Cross-reading comparison of beneficiary/victim sets, extractiveness values, and coordination functions across the three readings of vedic_dharmic_corpus.',
    'If the hereditary_monopoly_reading computes as snare and reformist_egalitarian_reading as scaffold, the bhakti reading''s rope classification is contextual — it occupies a coordination middle ground. If all three compute as extractive, the kernel itself may be a constraint family with shared extraction structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer-frame structural delta across sibling readings of the vedic_dharmic_corpus kernel.').

omega_variable(
    devotional_institutional_capture,
    'To what extent have dominant-caste institutions captured the bhakti devotional framework, converting its coordination function into extraction?',
    'Historical analysis of bhakti sampradaya institutionalization: temple management, guru succession, textual canonization, and their caste composition over time.',
    'High capture would reclassify the constraint toward tangled_rope (genuine coordination at devotional level, extraction at institutional level). Low capture supports the rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(devotional_institutional_capture, empirical, 'Whether the devotional reading''s coordination function has been partially captured by the same social groups it structurally challenges.').

omega_variable(
    residual_exclusion_mechanism,
    'Is the residual exclusion of lower castes from full participation a feature of the bhakti reading''s textual logic or a failure of its social implementation?',
    'Textual analysis of bhakti corpora (do they explicitly extend to social equality or only spiritual?) combined with sociological data on temple entry, intermarriage, and occupational mobility in bhakti-dominant regions.',
    'If textual, the bhakti reading itself contains an extraction structure (tangled_rope). If implementational, the reading is rope but operates in a hostile social environment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_exclusion_mechanism, conceptual, 'Whether the victim set (residually_excluded_lower_castes) is produced by the constraint or by external factors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__bhakti_devotional_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedic_bhakti_tr_t0, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vedic_bhakti_tr_t20, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(vedic_bhakti_tr_t40, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(vedic_bhakti_tr_t60, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement(vedic_bhakti_tr_t80, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 80, 0.24).
narrative_ontology:measurement(vedic_bhakti_tr_t100, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 100, 0.25).

% Extraction over time
narrative_ontology:measurement(vedic_bhakti_be_t0, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(vedic_bhakti_be_t20, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement(vedic_bhakti_be_t40, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 40, 0.3).
narrative_ontology:measurement(vedic_bhakti_be_t60, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 60, 0.35).
narrative_ontology:measurement(vedic_bhakti_be_t80, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 80, 0.38).
narrative_ontology:measurement(vedic_bhakti_be_t100, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 100, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(vedic_bhakti_su_t0, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(vedic_bhakti_su_t20, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 20, 0.25).
narrative_ontology:measurement(vedic_bhakti_su_t40, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 40, 0.3).
narrative_ontology:measurement(vedic_bhakti_su_t60, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 60, 0.32).
narrative_ontology:measurement(vedic_bhakti_su_t80, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 80, 0.34).
narrative_ontology:measurement(vedic_bhakti_su_t100, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 100, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__bhakti_devotional_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_dharmic_corpus__bhakti_devotional_reading, 0.08).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus__hereditary_monopoly_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus__reformist_egalitarian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the vedic_dharmic_corpus kernel. The hereditary_monopoly_reading claims birth-based authority (epsilon ~0.70, snare). The reformist_egalitarian_reading claims constitutional equality (epsilon ~0.25, scaffold with sunset). This bhakti_devotional_reading claims devotional access (epsilon ~0.40, rope). The three readings form a constraint family linked by shared textual kernel but divergent beneficiary/victim structures and extractiveness profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vedic_dharmic_corpus__bhakti_devotional_reading, institutional, 0.75).
constraint_indexing:directionality_override(vedic_dharmic_corpus__bhakti_devotional_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
