% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__reformist_egalitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   domain: religious/social/political
 *
 * SUMMARY:
 *   The reformist egalitarian reading of the Vedic-Dharmic corpus asserts
 *   that textual meaning must conform to constitutional equality principles,
 *   that caste hierarchy is historical accretion rather than scriptural
 *   essence, and that rational critique supersedes traditional authority.
 *   This reading emerged from 19th-century reform movements (Brahmo Samaj,
 *   Arya Samaj, Phule-Ambedkar lineage) and was institutionalized in the
 *   Indian Constitution (Articles 14-17, 25-28). It operates as a tangled
 *   rope: it coordinates a genuine collective-action problem (dismantling
 *   caste-based exclusion, enabling Dalit political agency) while extracting
 *   from orthodox institutions through state enforcement of anti-caste
 *   legislation, affirmative action, and temple-entry judgments. The
 *   beneficiary structure is inverted relative to the
 *   hereditary_monopoly_reading: Dalit movements and progressive scholars are
 *   the primary beneficiaries; hereditary Brahmin authorities and orthodox
 *   institutions bear the costs of lost interpretive monopoly and ritual
 *   prerogatives. The constraint is entangled with state apparatus — courts,
 *   legislatures, and electoral politics are the enforcement machinery.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.45).
domain_priors:suppression_score(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.35).
domain_priors:theater_ratio(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__reformist_egalitarian_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__reformist_egalitarian_reading, "Reformist Egalitarian Reading of Vedic-Dharmic Corpus").
narrative_ontology:topic_domain(vedic_dharmic_corpus__reformist_egalitarian_reading, "religious/social/political").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__reformist_egalitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__reformist_egalitarian_reading, 'b1ba01ee-6960-4e2d-944e-acea3a270fa7').
narrative_ontology:cs_kernel_codification('b1ba01ee-6960-4e2d-944e-acea3a270fa7', fixed_text).
narrative_ontology:cs_authority_grounding('b1ba01ee-6960-4e2d-944e-acea3a270fa7', extraction).
narrative_ontology:cs_interpretation_layer_present('b1ba01ee-6960-4e2d-944e-acea3a270fa7').
narrative_ontology:cs_reading_relation('b1ba01ee-6960-4e2d-944e-acea3a270fa7', vedic_dharmic_corpus__hereditary_monopoly_reading, forecloses).
narrative_ontology:cs_reading_relation('b1ba01ee-6960-4e2d-944e-acea3a270fa7', vedic_dharmic_corpus__bhakti_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('b1ba01ee-6960-4e2d-944e-acea3a270fa7', foundational, textual_meaning_subordinate_to_constitutional_equality).
narrative_ontology:cs_axiom_status(textual_meaning_subordinate_to_constitutional_equality, holdable).
narrative_ontology:cs_axiom_grounding('b1ba01ee-6960-4e2d-944e-acea3a270fa7', textual_meaning_subordinate_to_constitutional_equality, conventional).
narrative_ontology:cs_axiom('b1ba01ee-6960-4e2d-944e-acea3a270fa7', foundational, caste_hierarchy_is_historical_accretion_not_scriptural_essence).
narrative_ontology:cs_axiom_status(caste_hierarchy_is_historical_accretion_not_scriptural_essence, holdable).
narrative_ontology:cs_axiom_grounding('b1ba01ee-6960-4e2d-944e-acea3a270fa7', caste_hierarchy_is_historical_accretion_not_scriptural_essence, empirically_contingent).
narrative_ontology:cs_axiom('b1ba01ee-6960-4e2d-944e-acea3a270fa7', foundational, rational_critique_supersedes_traditional_authority).
narrative_ontology:cs_axiom_status(rational_critique_supersedes_traditional_authority, holdable).
narrative_ontology:cs_axiom_grounding('b1ba01ee-6960-4e2d-944e-acea3a270fa7', rational_critique_supersedes_traditional_authority, instrumental).
narrative_ontology:cs_reference_frame('b1ba01ee-6960-4e2d-944e-acea3a270fa7', reformist_constitutional_equality_framework).
narrative_ontology:cs_drift_state('b1ba01ee-6960-4e2d-944e-acea3a270fa7', contemporary_constitutional_morality_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b1ba01ee-6960-4e2d-944e-acea3a270fa7', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__reformist_egalitarian_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_movements).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, progressive_scholars).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_legal_actors).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_institutions).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, hereditary_brahmin_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, bhakti_practitioners).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, state_enforcement_apparatus).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_equality_principle).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__reformist_egalitarian_reading, rational_critique_over_traditional_authority).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__reformist_egalitarian_reading, caste_as_historical_accretion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organize around constitutional equality guarantees to claim political representation, educational access, and public employment through reservations. Their identity is constituted by caste — they cannot exit the identity the constraint transforms. They gain enforceable rights and state resources; they bear the cost of political mobilization and intra-community stratification (creamy layer debates). The constraint is the legal architecture of their claim-making.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_movements, beneficiary,
    organized, generational, identity_locked, national).

% Produce the hermeneutic and historical arguments that caste is historical accretion, not scriptural essence. Their academic legitimacy and funding streams align with this reading. Exit means adopting a different interpretive framework — possible but costly in a field where this reading dominates progressive discourse. They benefit from the reading's institutionalization but do not control its enforcement.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, progressive_scholars, beneficiary,
    moderate, biographical, constrained, national).

% Courts, legislatures, and election commissions that enact and enforce anti-caste legislation, reservation policies, and temple-entry judgments. They set the agenda for how constitutional equality applies to religious practice. They can shift interpretive frameworks (e.g., essential religious practices test vs. constitutional morality) but their institutional role binds them to the constraint's enforcement logic. They collect no direct rents but wield the coercive apparatus.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_legal_actors, agenda_setter,
    institutional, generational, arbitrage, national).

% Temple administrations, matha lineages, and traditional educational institutions that lose control over ritual access, priestly appointments, and textual interpretation. They can adapt liturgically (e.g., accepting non-Brahmin priests under court order) but lose structural privilege and revenue. Their exit is constrained by the need to maintain institutional continuity while complying with law. They bear the compliance costs of the reading's enforcement.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_institutions, payer,
    organized, generational, constrained, national).

% Individuals whose ritual and interpretive authority derives entirely from birth into Brahmin lineages. The reading directly targets their authority ground — they cannot exit the birth-ascribed role, and the constraint's enforcement (court judgments, legislative reform, social pressure) erodes the material and symbolic basis of that role. They bear concentrated costs with no exit option.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, hereditary_brahmin_authorities, payer,
    moderate, biographical, trapped, local).

% Devotional communities that claim direct access to the divine bypassing caste mediation. They benefit from the reading's anti-caste thrust but maintain independent theological grounds (bhakti_devotional_reading). They can exit the constraint's framework by emphasizing devotional autonomy over constitutional equality — mobile exit. They are beneficiaries of the coordination function but not primary targets of the extraction function.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, bhakti_practitioners, beneficiary,
    organized, biographical, mobile, national).

% Police, bureaucracy, and judicial machinery that implement anti-caste legislation and reservations. They gain institutional mandate and resources from enforcement; they also bear implementation costs. Their secondary role as beneficiary reflects the constraint expanding state capacity into religious-social domains. They are analytical observers of the constraint's operation from within the enforcement loop.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, state_enforcement_apparatus, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__reformist_egalitarian_reading, state_enforcement_apparatus, beneficiary).

% Political movements that claim to represent Hindu unity but oppose caste-based reservations and judicial intervention in religious practice. They would object to the reading's egalitarian thrust and its entanglement with state power, but they are structurally excluded from the constraint's beneficiary set — their exclusion is part of the constraint's political logic. They are trapped because their political identity requires engaging the constraint they oppose.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, hindutva_political_actors, excluded,
    powerful, biographical, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of dismantling caste-based exclusion by providing a constitutional-legal framework that transforms scriptural interpretation from a hereditary monopoly into a publicly contestable domain, enabling Dalit political agency and state-enforced equality.
% TRANSFER_FUNCTION: Moves interpretive authority, ritual control, and state-enforced resources (reservations, legal protections, temple access) from hereditary Brahmin authorities and orthodox institutions to Dalit movements and constitutional-legal actors, using state coercion as the transfer mechanism.
% ABSENT_VOICES: Pre-colonial textual traditions that neither map onto hereditary monopoly nor reformist egalitarian frames; Adivasi cosmologies outside the varna framework; women within Dalit movements whose gendered experience is subsumed under caste category; the bhakti_devotional_reading's claim that devotional sincerity supersedes both birth and constitutional categories — these voices are not seated in the constraint's enforcement architecture.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, the constitutional-legal architecture of anti-caste enforcement (reservations, SC/ST Act, temple-entry judgments) would lose its hermeneutic foundation. The state would retain coercive capacity but lose the legitimating narrative that caste oppression is a violation of constitutional equality grounded in a reformed reading of the corpus. Dalit movements would lose their primary legal-theological warrant. Orthodox institutions would reclaim interpretive monopoly de facto. The social world would rearrange significantly.
% FOUNDING_PROBLEM: Caste hierarchy justified as divinely ordained scriptural essence (varna-jati system) producing hereditary ritual monopoly, untouchability, and exclusion of Shudras and Dalits from textual authority, spiritual knowledge, and public resources — a problem identified by 19th-century reformers (Phule, Ambedkar, Brahmo Samaj) as both theological error and material oppression.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: colonial ethnography (Risley, Census reports) documents caste as administered category, not scriptural essence; Ambedkar's 'Annihilation of Caste' (1936) provides internal critique from the victim seat; Constituent Assembly debates (1946-49) record the founding problem as live and contested across orthodox, reformist, and socialist framings; contemporary Dalit Studies scholarship (Jodhka, Thorat, Guru) corroborates the problem's persistence from a non-beneficiary academic seat. No single corroborator is neutral — the contestation is the signal.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__reformist_egalitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__reformist_egalitarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__reformist_egalitarian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
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
 *   Extractiveness (0.45 at interval end) reflects the reading's reliance on state coercion to override traditional authority: reservations, anti-atrocity legislation, and judicial review of religious practices impose compliance costs on orthodox institutions. Suppression (0.35) is moderate — the reading suppresses the hereditary_monopoly_reading's claim to interpretive authority but does not eliminate devotional practice or textual study; bhakti_devotional_reading coexists. Theater ratio (0.25) is low-moderate: the coordination function (constitutional equality, Dalit representation) is real and measurable, but performative invocation of 'constitutional morality' sometimes substitutes for material redistribution. Accessibility collapse (0.3) is low: alternative readings (hereditary monopoly, bhakti) remain live and practiced. Resistance (0.55) is significant: orthodox institutions resist through litigation, social boycott, political mobilization, and narrative counter-claims (e.g., 'merit' vs. 'reservation').
 *
 * PERSPECTIVAL GAP:
 *   From the dalit_movements seat, the constraint is a rope (genuine coordination against caste oppression, minimal extraction from them). From the hereditary_brahmin_authorities seat, it is a snare (pure extraction of their ritual monopoly and interpretive authority). From the constitutional_legal_actors seat, it is a tangled rope (they administer the enforcement that coordinates equality but extracts from traditional authorities). The engine computes this seat divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: dalit_movements (identity_locked exit — caste identity cannot be shed; the reading's promise is precisely to transform the terms of that identity), progressive_scholars (constrained exit — academic legitimacy depends on this reading's dominance), constitutional_legal_actors (arbitrage exit — they can shift interpretive frameworks but institutional role binds them). Victims: orthodox_institutions (constrained exit — they can adapt liturgically but lose structural privilege), hereditary_brahmin_authorities (trapped exit — birth-ascribed role cannot be exited; the reading directly targets their authority ground). The inverted beneficiary structure relative to the hereditary_monopoly_reading is the constraint's defining feature.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (caste oppression justified by scriptural misreading) remains live (status: contested). The arrangement has not atrophied — enforcement intensity and beneficiary claims have grown. However, a mandatrophy risk exists if the coordination function (substantive equality) is displaced by performative inclusion (theater ratio rising) while extraction from orthodox institutions continues without delivering material gains to Dalit communities. The theater_ratio trajectory (0.1→0.25) warrants monitoring.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of a contested kernel (vedic_dharmic_corpus) rather than a standalone constraint?',
    'The constraint is explicitly generated as the reformist_egalitarian_reading of the vedic_dharmic_corpus kernel. Sibling readings (hereditary_monopoly_reading, bhakti_devotional_reading) instantiate different constraints from the same kernel. The ε value, beneficiary structure, and authority grounding are reading-specific.',
    'Confirms the committer frame: this JSON is one ε-invariant constraint for this reading only. Other readings are separate constraint files linked via network.affects_constraints and cs_structure.reading_relations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment that this constraint is a kernel reading, not a flat constraint.').

omega_variable(
    extraction_measurement_boundary,
    'Does the measured extractiveness (~0.45) capture the reading''s own enforcement costs, or does it include extraction performed by sibling readings that this reading opposes?',
    'Empirical separation of enforcement expenditures: state enforcement of anti-caste legislation (this reading''s enforcement) vs. social enforcement of caste norms (hereditary_monopoly_reading''s enforcement). Legal budgets, court caseloads, and civil society expenditure tracking can isolate the reading-specific enforcement surface.',
    'If ε conflates across readings, the tangled_rope classification for this reading is inflated by the sibling''s extraction. If separable, the reading''s own enforcement of equality principles is moderately extractive — it uses state coercion to override traditional authority, creating its own payer class (orthodox institutions).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_measurement_boundary, empirical, 'Whether the ε referent is properly isolated to this reading''s enforcement surface.').

omega_variable(
    beneficiary_inversion_stability,
    'Is the inverted beneficiary structure (Dalit movements as beneficiaries, orthodox institutions as victims) stable over the interval, or does it reflect a transitional power shift that could revert?',
    'Track beneficiary/victim declarations across the interval: 1850-1900 (orthodox dominant, Dalit movements nascent), 1900-1950 (Ambedkarite movements, constitutional drafting), 1950-present (legal enforcement, quota politics, continued contestation). If the beneficiary set has remained constant in structural position despite power shifts, the inversion is stable.',
    'If the inversion is transitional, the constraint may be a scaffold (temporary support for a transition) rather than a tangled_rope. If stable, the coordination function (constitutional equality) and extraction function (overriding traditional authority) are locked together durably.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_inversion_stability, empirical, 'Durability of the inverted beneficiary structure across the constraint''s lifecycle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__reformist_egalitarian_reading, 1850, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t1850, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 1850, 0.1).
narrative_ontology:measurement(vedi_tr_t1900, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 1900, 0.15).
narrative_ontology:measurement(vedi_tr_t1950, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 1950, 0.2).
narrative_ontology:measurement(vedi_tr_t1975, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 1975, 0.22).
narrative_ontology:measurement(vedi_tr_t2000, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 2000, 0.24).
narrative_ontology:measurement(vedi_tr_t2025, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(vedi_be_t1850, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 1850, 0.15).
narrative_ontology:measurement(vedi_be_t1900, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 1900, 0.25).
narrative_ontology:measurement(vedi_be_t1950, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 1950, 0.4).
narrative_ontology:measurement(vedi_be_t1975, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 1975, 0.42).
narrative_ontology:measurement(vedi_be_t2000, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 2000, 0.44).
narrative_ontology:measurement(vedi_be_t2025, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t1850, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 1850, 0.1).
narrative_ontology:measurement(vedi_su_t1900, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 1900, 0.2).
narrative_ontology:measurement(vedi_su_t1950, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 1950, 0.35).
narrative_ontology:measurement(vedi_su_t1975, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 1975, 0.33).
narrative_ontology:measurement(vedi_su_t2000, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement(vedi_su_t2025, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 2025, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__reformist_egalitarian_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.1).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, vedic_dharmic_corpus__hereditary_monopoly_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, vedic_dharmic_corpus__bhakti_devotional_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_equality_enforcement).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, reservation_policy_framework).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, anti_caste_legislation).

% DUAL FORMULATION NOTE:
% Kernel decomposition: vedic_dharmic_corpus splits into three constraint stories by ε-invariance. hereditary_monopoly_reading (ε≈0.15, beneficiaries: hereditary_brahmin_authorities, victims: dalit_movements) claims mountain-like natural law status but has beneficiaries → false summit candidate. bhakti_devotional_reading (ε≈0.1, coordination-dominant, beneficiaries: devotional_practitioners) is a rope. reformist_egalitarian_reading (this story, ε≈0.45, tangled_rope) is the enforcement-heavy equality reading. All three link via affects_constraints. The hereditary_monopoly_reading is upstream (cited as 'tradition'); bhakti_devotional_reading is parallel (coexists); this reading is downstream (uses state to override both).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vedic_dharmic_corpus__reformist_egalitarian_reading, institutional, 0.15).
constraint_indexing:directionality_override(vedic_dharmic_corpus__reformist_egalitarian_reading, organized, 0.2).
constraint_indexing:directionality_override(vedic_dharmic_corpus__reformist_egalitarian_reading, moderate, 0.75).
constraint_indexing:directionality_override(vedic_dharmic_corpus__reformist_egalitarian_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
