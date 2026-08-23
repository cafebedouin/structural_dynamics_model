% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__reformist_egalitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_dharmic_corpus__reformist_egalitarian_reading, []).

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
 *   constraint_id: vedic_dharmic_corpus__reformist_egalitarian_reading
 *   human_readable: Reformist Egalitarian Reading of Vedic-Dharmic Corpus
 *   domain: religious/social/legal
 *
 * SUMMARY:
 *   The reformist egalitarian reading of the Vedic-Dharmic corpus asserts
 *   that caste hierarchy is a historical accretion, not scriptural essence,
 *   and that textual meaning must conform to constitutional equality
 *   principles. It is instantiated through Supreme Court jurisprudence
 *   (essential religious practices test, basic structure doctrine) and
 *   legislative acts (Temple Entry Acts, anti-untouchability laws). The
 *   reading operates as a tangled rope: it coordinates a pluralistic
 *   religious field under constitutional supremacy (genuine coordination
 *   function) while extracting interpretive authority and material control
 *   from hereditary priesthood and orthodox institutions (asymmetric
 *   extraction). Enforcement depends on state apparatus — courts, police,
 *   bureaucracy — making it actively enforced.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.45).
domain_priors:suppression_score(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.65).
domain_priors:theater_ratio(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__reformist_egalitarian_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__reformist_egalitarian_reading, "Reformist Egalitarian Reading of Vedic-Dharmic Corpus").
narrative_ontology:topic_domain(vedic_dharmic_corpus__reformist_egalitarian_reading, "religious/social/legal").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__reformist_egalitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__reformist_egalitarian_reading, '5897e845-3bd2-4fa0-8f91-56c28e8d929b').
narrative_ontology:cs_kernel_codification('5897e845-3bd2-4fa0-8f91-56c28e8d929b', fixed_text).
narrative_ontology:cs_authority_grounding('5897e845-3bd2-4fa0-8f91-56c28e8d929b', extraction).
narrative_ontology:cs_interpretation_layer_present('5897e845-3bd2-4fa0-8f91-56c28e8d929b').
narrative_ontology:cs_reading_relation('5897e845-3bd2-4fa0-8f91-56c28e8d929b', vedic_dharmic_corpus__hereditary_monopoly_reading, forecloses).
narrative_ontology:cs_reading_relation('5897e845-3bd2-4fa0-8f91-56c28e8d929b', vedic_dharmic_corpus__bhakti_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('5897e845-3bd2-4fa0-8f91-56c28e8d929b', foundational, textual_meaning_conforms_to_constitutional_equality).
narrative_ontology:cs_axiom_status(textual_meaning_conforms_to_constitutional_equality, holdable).
narrative_ontology:cs_axiom_grounding('5897e845-3bd2-4fa0-8f91-56c28e8d929b', textual_meaning_conforms_to_constitutional_equality, conventional).
narrative_ontology:cs_axiom('5897e845-3bd2-4fa0-8f91-56c28e8d929b', foundational, caste_hierarchy_is_historical_accretion_not_scriptural_essence).
narrative_ontology:cs_axiom_status(caste_hierarchy_is_historical_accretion_not_scriptural_essence, holdable).
narrative_ontology:cs_axiom_grounding('5897e845-3bd2-4fa0-8f91-56c28e8d929b', caste_hierarchy_is_historical_accretion_not_scriptural_essence, empirically_contingent).
narrative_ontology:cs_axiom('5897e845-3bd2-4fa0-8f91-56c28e8d929b', secondary, rational_critique_supersedes_traditional_authority).
narrative_ontology:cs_axiom_status(rational_critique_supersedes_traditional_authority, holdable).
narrative_ontology:cs_axiom_grounding('5897e845-3bd2-4fa0-8f91-56c28e8d929b', rational_critique_supersedes_traditional_authority, instrumental).
narrative_ontology:cs_reference_frame('5897e845-3bd2-4fa0-8f91-56c28e8d929b', constitutional_equality_framework).
narrative_ontology:cs_drift_state('5897e845-3bd2-4fa0-8f91-56c28e8d929b', contemporary_hindu_nationalist_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5897e845-3bd2-4fa0-8f91-56c28e8d929b', '2026-06-12T12:00:00Z').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__reformist_egalitarian_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_movements).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, reformist_scholars).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, egalitarian_activists).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_institutions).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, hereditary_priesthood).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, traditional_authorities).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_equality_principle).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__reformist_egalitarian_reading, rational_critique_supersedes_tradition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforces constitutional equality through court rulings that reinterpret scriptural authority; can strike down caste-based exclusions in temple entry, ritual participation, and institutional leadership. The judiciary's legitimacy rests on constitutional supremacy, not textual tradition.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, state_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Gain legal recognition and material access (temple entry, priesthood, education) when the reading is enforced. Their mobilization drives litigation and legislative amendment. Exit means abandoning hard-won legal protections and returning to untouchability practices.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_movements, beneficiary,
    organized, biographical, constrained, national).

% Produce the hermeneutic tools (historical-critical method, constitutional hermeneutics) that legitimize the reading. They occupy academic and institutional positions that depend on the reading's viability. Can exit to secular academia or alternative interpretive communities.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, reformist_scholars, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__reformist_egalitarian_reading, reformist_scholars, agenda_setter).

% Lose control over ritual authority, temple administration, and educational endowments when courts mandate equality. Their identity is fused with custodianship of tradition; exit would dissolve the institution's self-understanding. They resist through litigation, social pressure, and parallel institutions.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_institutions, payer,
    institutional, generational, identity_locked, national).

% Face direct displacement from ritual offices and hereditary privileges. Birth-based entitlement is their sole claim to livelihood and status; the reading renders that claim legally void. Exit requires abandoning caste identity and vocational training — effectively impossible for most.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, hereditary_priesthood, payer,
    organized, biographical, identity_locked, local).

% Include matha heads, smarta councils, and sectarian leaders who derive authority from textual orthodoxy. They lose interpretive monopoly and face state oversight of religious endowments. Some adapt by co-opting reform language; others harden opposition.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, traditional_authorities, payer,
    powerful, generational, constrained, regional).

% Hold a rival reading (devotional access bypasses caste) that is neither fully aligned with reformist egalitarianism nor with hereditary monopoly. They are not consulted in constitutional litigation and their devotional theology is treated as politically irrelevant by both state and orthodox sides.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, bhakti_practitioners, excluded,
    moderate, biographical, mobile, national).

% Analyze the jurisprudence of essential religious practices, the basic structure doctrine, and the tension between Articles 25-26 and 14-17. They do not collect rents from the constraint nor bear its costs; their output shapes future enforcement.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legally enforceable hermeneutic that aligns religious text with constitutional equality, enabling state intervention against caste hierarchy without abolishing religion itself. Solves the coordination problem of how a secular state can regulate religious institutions while claiming neutrality.
% TRANSFER_FUNCTION: Transfers interpretive authority and material control (temple assets, ritual offices, educational endowments) from hereditary priesthood and orthodox institutions to state-supervised bodies and historically excluded groups. Transfers status and livelihood from birth-based claimants to merit/constitutional claimants.
% ABSENT_VOICES: Bhakti devotional communities who claim spiritual authority derives from sincere devotion rather than either birth or constitutional law. They are excluded because their reading does not map onto the state's equality/rights framework and offers no leverage for litigation. Also excluded: tribal and folk traditions that operate outside the Vedic-Dharmic corpus entirely.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, caste-based exclusions in temples, priesthood, and religious endowments would revert to hereditary monopoly; Dalit movements would lose their primary legal instrument; state jurisprudence on essential religious practices would collapse; the constitutional project of reforming Hinduism from within would be abandoned.
% FOUNDING_PROBLEM: How to reconcile the Vedic-Dharmic textual tradition — which contains both egalitarian and hierarchical strands — with the constitutional mandate of equality, without either abandoning the tradition or accepting caste hierarchy as scripturally essential.
% FOUNDING_PROBLEM_CORROBORATION: Ambedkar's writings (Annihilation of Caste, Riddles in Hinduism) and the Constituent Assembly debates corroborate that the founding problem was recognized by the Constitution's framers. Orthodox institutions dispute the problem's framing, arguing hierarchy is essential. The Supreme Court's essential religious practices doctrine (Shirur Mutt, Sabarimala) shows the problem remains live and contested in jurisprudence.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__reformist_egalitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__reformist_egalitarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__reformist_egalitarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vedic_dharmic_corpus__reformist_egalitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__reformist_egalitarian_reading_tests).
:- end_tests(vedic_dharmic_corpus__reformist_egalitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because the reading transfers authority and assets but also provides a coordination framework that prevents religious fragmentation and communal violence. Suppression is significant (0.65) because compliance requires state coercion against resistant institutions. Theater ratio is low-moderate (0.3) — the equality rhetoric is not mere performance; it drives real redistribution of ritual rights. Accessibility collapse is moderate (0.5) because alternative readings (hereditary monopoly, bhakti devotional) persist despite legal pressure. Resistance is high (0.7) from orthodox institutions and hereditary priesthood whose identity and livelihood are threatened.
 *
 * PERSPECTIVAL GAP:
 *   From the state judiciary's seat, the constraint appears as rope (coordination under constitutional supremacy). From hereditary priesthood's seat, it appears as snare (extraction of birthright). From Dalit movements' seat, it appears as scaffold (temporary support until caste annihilation). The engine computes these divergences from the structural data; the claimed_type (tangled_rope) reflects the author's assessment that both coordination and extraction are structurally real and neither reduces to the other.
 *
 * DIRECTIONALITY LOGIC:
 *   State judiciary is agenda_setter with analytical exit (d~0.05) — it benefits from expanded constitutional jurisdiction. Dalit movements and reformist scholars are beneficiaries with constrained/mobile exit (d~0.15-0.25) — they gain rights but remain vulnerable to political reversal. Orthodox institutions, hereditary priesthood, traditional authorities are payers with identity_locked/constrained exit (d~0.85-0.95) — they bear costs and cannot leave without self-dissolution. Bhakti practitioners are excluded (d~0.5) — they are neither coordinated nor extracted from directly, but their reading is marginalized. Legal scholars are observers (d~0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling text with equality) remains contested — orthodox institutions argue hierarchy is essential; reformists argue it is accretional. The reading has not resolved into pure coordination (rope) because extraction from hereditary structures is ongoing and contested. It has not become pure extraction (snare) because the coordination function (preventing religious fragmentation, enabling state neutrality) is genuine and valued by beneficiaries. Mandatrophy is unresolved: the arrangement persists because the founding problem is live, but the extraction component has grown over time (rising extractiveness measurements).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'How does the reformist egalitarian reading''s structural relationship to the vedic_dharmic_corpus kernel differ from its sibling readings, and does this reading instantiate a distinct constraint or a variant of a shared constraint?',
    'Compare the ε-invariant structural profiles (beneficiaries, victims, enforcement, extractiveness) of all three readings. If each reading has a stable, distinct ε and beneficiary/victim structure, they are separate constraints linked by network.affects_constraints. If ε varies only by measurement basis, they are one constraint with measurement ambiguity.',
    'If distinct constraints, each gets its own classification and the kernel is a family. If one constraint, the reading differences are perspectival and the engine''s per-seat classification captures the divergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Whether the kernel''s readings are separate constraints or perspectival variants of one constraint.').

omega_variable(
    constitutional_equality_as_coordination_or_extraction,
    'Is the constitutional equality principle functioning as a genuine coordination standard (like a rope) or as an extraction tool against traditional authorities (like a snare)?',
    'Track whether state enforcement targets only caste-based exclusions (coordination) or also redistributes temple assets and educational endowments to state-controlled boards (extraction). Measure the ratio of regulatory to redistributive interventions over time.',
    'If primarily regulatory, the reading trends toward rope. If primarily redistributive, it trends toward snare. The current tangled_rope classification assumes both are substantial.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutional_equality_as_coordination_or_extraction, empirical, 'Whether the equality principle coordinates or extracts in practice.').

omega_variable(
    bhakti_reading_structural_position,
    'Does the bhakti devotional reading occupy a distinct structural position (neither beneficiary nor payer of this constraint) or is it a suppressed variant of the reformist reading?',
    'Examine whether bhakti communities litigate under Article 25 (freedom of religion) independently of equality claims, and whether courts treat devotional essential practices as compatible with or subordinate to equality rulings.',
    'If bhakti reading is structurally excluded (neither coordinated nor extracted from), it remains an independent reading. If it is a suppressed variant, the reformist reading''s coordination function is narrower than claimed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bhakti_reading_structural_position, empirical, 'Structural position of the bhakti devotional reading relative to this constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__reformist_egalitarian_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedic_dharmic_corpus__reformist_egalitarian_reading_tr_t1950, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(vedic_dharmic_corpus__reformist_egalitarian_reading_tr_t1970, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(vedic_dharmic_corpus__reformist_egalitarian_reading_tr_t1990, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(vedic_dharmic_corpus__reformist_egalitarian_reading_tr_t2000, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(vedic_dharmic_corpus__reformist_egalitarian_reading_tr_t2010, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 2010, 0.28).
narrative_ontology:measurement(vedic_dharmic_corpus__reformist_egalitarian_reading_tr_t2024, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(vedic_dharmic_corpus__reformist_egalitarian_reading_be_t1950, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 1950, 0.15).
narrative_ontology:measurement(vedic_dharmic_corpus__reformist_egalitarian_reading_be_t1970, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement(vedic_dharmic_corpus__reformist_egalitarian_reading_be_t1990, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(vedic_dharmic_corpus__reformist_egalitarian_reading_be_t2000, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(vedic_dharmic_corpus__reformist_egalitarian_reading_be_t2010, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement(vedic_dharmic_corpus__reformist_egalitarian_reading_be_t2024, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(vedic_dharmic_corpus__reformist_egalitarian_reading_su_t1950, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 1950, 0.3).
narrative_ontology:measurement(vedic_dharmic_corpus__reformist_egalitarian_reading_su_t1970, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 1970, 0.4).
narrative_ontology:measurement(vedic_dharmic_corpus__reformist_egalitarian_reading_su_t1990, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(vedic_dharmic_corpus__reformist_egalitarian_reading_su_t2000, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(vedic_dharmic_corpus__reformist_egalitarian_reading_su_t2010, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement(vedic_dharmic_corpus__reformist_egalitarian_reading_su_t2024, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__reformist_egalitarian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.08).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, vedic_dharmic_corpus__hereditary_monopoly_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, vedic_dharmic_corpus__bhakti_devotional_reading).

% DUAL FORMULATION NOTE:
% The vedic_dharmic_corpus kernel decomposes into three constraint stories: hereditary_monopoly_reading (claimed mountain, low extractiveness, high accessibility collapse), bhakti_devotional_reading (claimed rope, low extractiveness, devotional coordination), and reformist_egalitarian_reading (claimed tangled_rope, moderate extractiveness, equality coordination with extraction from hereditary structures). The reformist reading forecloses hereditary_monopoly within constitutional frameworks and influences bhakti by shaping the legal essential-practices test. All three share the kernel but have distinct ε, beneficiaries, and enforcement structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vedic_dharmic_corpus__reformist_egalitarian_reading, institutional, 0.05).
constraint_indexing:directionality_override(vedic_dharmic_corpus__reformist_egalitarian_reading, organized, 0.85).
constraint_indexing:directionality_override(vedic_dharmic_corpus__reformist_egalitarian_reading, powerful, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
