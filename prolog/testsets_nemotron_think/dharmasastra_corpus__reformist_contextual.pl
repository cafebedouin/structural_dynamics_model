% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__reformist_contextual
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__reformist_contextual, []).

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
 *   constraint_id: dharmasastra_corpus__reformist_contextual
 *   human_readable: Dharmasastra Reformist Contextual Reading
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   The reformist contextual reading of Dharmasastra emerges from
 *   19th-century Hindu reform movements (Brahmo Samaj, Arya Samaj, Prarthana
 *   Samaj) and crystallizes in post-independence India's project of 'Hindu
 *   law reform' — codifying personal law while claiming continuity with
 *   shastric tradition. The reading asserts that dharma (righteous conduct)
 *   is the eternal ethical core, while varna/jati hierarchy, patriarchal
 *   family structures, and ritual exclusions are time-bound social
 *   prescriptions (yuga-dharma) superseded by modern conditions. This reading
 *   currently dominates state-recognized Hindu institutions, academic
 *   discourse, and constitutional jurisprudence, but coexists uneasily with
 *   orthodox literalism (which retains ritual authority and grassroots
 *   loyalty) and abolitionist rejection (which drives constitutional equality
 *   jurisprudence). The constraint is the interpretive framework itself: the
 *   rule that Dharmasastra must be read contextually, preserving textual
 *   authority while discarding caste prescriptions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__reformist_contextual, 0.45).
domain_priors:suppression_score(dharmasastra_corpus__reformist_contextual, 0.35).
domain_priors:theater_ratio(dharmasastra_corpus__reformist_contextual, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, extractiveness, 0.45).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__reformist_contextual, tangled_rope).
narrative_ontology:human_readable(dharmasastra_corpus__reformist_contextual, "Dharmasastra Reformist Contextual Reading").
narrative_ontology:topic_domain(dharmasastra_corpus__reformist_contextual, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__reformist_contextual).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__reformist_contextual, 'd870b853-6a16-4f96-90b1-fca3157f56ac').
narrative_ontology:cs_kernel_codification('d870b853-6a16-4f96-90b1-fca3157f56ac', fixed_text).
narrative_ontology:cs_authority_grounding('d870b853-6a16-4f96-90b1-fca3157f56ac', lineage).
narrative_ontology:cs_interpretation_layer_present('d870b853-6a16-4f96-90b1-fca3157f56ac').
narrative_ontology:cs_reading_relation('d870b853-6a16-4f96-90b1-fca3157f56ac', dharmasastra_corpus__orthodox_literalist, coexists_with).
narrative_ontology:cs_reading_relation('d870b853-6a16-4f96-90b1-fca3157f56ac', dharmasastra_corpus__abolitionist_rejection, coexists_with).
narrative_ontology:cs_axiom('d870b853-6a16-4f96-90b1-fca3157f56ac', foundational, dharma_separable_from_caste).
narrative_ontology:cs_axiom_status(dharma_separable_from_caste, holdable).
narrative_ontology:cs_axiom_grounding('d870b853-6a16-4f96-90b1-fca3157f56ac', dharma_separable_from_caste, deontological).
narrative_ontology:cs_axiom('d870b853-6a16-4f96-90b1-fca3157f56ac', foundational, textual_authority_preserved_through_reinterpretation).
narrative_ontology:cs_axiom_status(textual_authority_preserved_through_reinterpretation, holdable).
narrative_ontology:cs_axiom_grounding('d870b853-6a16-4f96-90b1-fca3157f56ac', textual_authority_preserved_through_reinterpretation, conventional).
narrative_ontology:cs_reference_frame('d870b853-6a16-4f96-90b1-fca3157f56ac', classical_dharmasastra_authority).
narrative_ontology:cs_drift_state('d870b853-6a16-4f96-90b1-fca3157f56ac', post_colonial_reform_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d870b853-6a16-4f96-90b1-fca3157f56ac', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, reformist_interpreters).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, institutional_authorities).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, lower_caste_groups).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, women_under_patriarchal_interpretations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, institutional_authorities).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__reformist_contextual, dharma_as_righteous_conduct).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__reformist_contextual, textual_authority_preservation).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__reformist_contextual, ethical_core_separability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars and religious leaders who reinterpret Dharmasastra texts to separate ethical dharma from caste prescriptions. They gain intellectual authority and institutional positions by offering a modernized Hinduism, but must navigate orthodox backlash and maintain textual fidelity to remain legitimate.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, reformist_interpreters, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__reformist_contextual, reformist_interpreters, beneficiary).

% Temple administrations, monastic orders, and personal law boards that derive legitimacy from Dharmasastra. They adopt reformist readings to retain social relevance and state recognition, but lose the clear hierarchical authority of literalist readings and face litigation over caste discrimination.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, institutional_authorities, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__reformist_contextual, institutional_authorities, payer).

% Dalit and OBC communities who still experience symbolic hierarchy through ritual exclusion, marriage restrictions, and social stigma justified by 'tradition.' Reformist readings soften but rarely eliminate these mechanisms; legal protections exist but enforcement is uneven and social mobility remains constrained by symbolic caste capital.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, lower_caste_groups, payer,
    powerless, biographical, constrained, national).

% Women across castes who face patriarchal family law, inheritance rules, and ritual exclusions grounded in Dharmasastra interpretations. Reformist readings have improved some provisions (e.g., Hindu Succession Act amendments) but symbolic subordination persists in marriage, adoption, and religious participation norms.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, women_under_patriarchal_interpretations, payer,
    moderate, biographical, constrained, national).

% Traditional pandits, Vedic schools, and Hindutva organizations that insist on eternal, literal varna/jati hierarchy. They are excluded from mainstream legal and academic discourse but retain grassroots influence, control over ritual pipelines, and political mobilization capacity. Their exit from the reformist framework is identity-locked — abandoning literalism dissolves their vocational and communal identity.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, orthodox_literalists, excluded,
    organized, generational, identity_locked, national).

% Ambedkarite movements, rationalist groups, and caste abolitionists who reject Dharmasastra entirely as irredeemably oppressive. They are excluded from the reformist project's textual authority game but drive constitutional and statutory reform from outside. Their exit is mobile — they operate in legal/political arenas not dependent on textual legitimacy.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, abolitionist_rejectionists, excluded,
    moderate, biographical, mobile, national).

% Scholars of religion, law, and South Asian studies who analyze the contest without institutional stake. They document the genealogy of readings, track legislative impacts, and provide expert testimony. Their analytical seat is unconstrained by identity or institutional loyalty.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, academic_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a shared ethical vocabulary (dharma as righteous conduct) and textual continuity for Hindu communities navigating modernity, colonialism, and constitutional democracy — solving the coordination problem of maintaining collective identity without literal caste enforcement.
% TRANSFER_FUNCTION: Moves interpretive authority from hereditary priestly literalists to educated reformist scholars and state-recognized institutions; transfers symbolic hierarchy costs (ritual exclusion, social stigma) onto lower castes and women at reduced but non-zero intensity; transfers legislative initiative to constitutional courts and parliament.
% ABSENT_VOICES: Dalit women at the intersection of caste and gender oppression; Adivasi communities outside the varna framework entirely; queer/trans Hindus excluded by all three readings' heteronormative assumptions. These voices are structurally absent from the reformist project's textual negotiation — they appear as objects of reform, not authors of it.
% DISAPPEARANCE_RATIONALE: If the reformist reading vanished overnight, institutional authorities would revert to either orthodox literalism (reviving explicit caste hierarchy in personal law) or state-imposed secular uniformity (abolishing religious personal law entirely). Hindu collective identity would lose its primary ethical-textual anchor, triggering either communal polarization or accelerated secularization. The symbolic hierarchy's soft enforcement would harden or dissolve — either way, the world rearranges.
% FOUNDING_PROBLEM: How to preserve Hindu textual authority and communal cohesion under colonial rule and later constitutional democracy, when the literal Dharmasastra prescriptions (varna hierarchy, patriarchal family law, ritual inequality) became legally indefensible and morally untenable.
% FOUNDING_PROBLEM_CORROBORATION: Reformist scholars (Gandhi, Radhakrishnan, Ambedkar's early engagement) attest the problem was genuine. Orthodox literalists attest it was a colonial imposition — the 'problem' was manufactured by foreign rule. Ambedkarite abolitionists attest the problem was always the text itself, not its interpretation. No single corroboration exists outside the beneficiary set; the founding problem's status is itself the central dispute.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__reformist_contextual, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__reformist_contextual, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__reformist_contextual, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dharmasastra_corpus__reformist_contextual, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__reformist_contextual, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__reformist_contextual_tests).
:- end_tests(dharmasastra_corpus__reformist_contextual_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is medium: the reformist reading extracts compliance through symbolic hierarchy (ritual purity norms, marriage endogamy, temple hierarchies) rather than legal enforcement, but this symbolic extraction is pervasive and legitimated by textual authority. Suppression (0.35) is moderate: legal barriers are largely removed, but social enforcement of endogamy and ritual exclusion persists through community pressure. Theater ratio (0.40) is significant: reformist institutions perform caste-blind rhetoric while maintaining caste-based priesthood, marriage networks, and social capital — the coordination function (ethical community) is real but the hierarchy it supposedly transcends is performatively maintained. Accessibility collapse (0.50) reflects that alternative frameworks (secular law, Ambedkarite Buddhism, Christian conversion) exist but carry high identity costs. Resistance (0.55) is high: the reading is contested from both orthodox (too little hierarchy) and abolitionist (too much hierarchy) directions.
 *
 * PERSPECTIVAL GAP:
 *   From the reformist interpreter seat, the constraint is a rope: genuine coordination of Hindu ethical identity in modernity. From the lower caste/women payer seats, it is a tangled rope: coordination function real but extraction persists symbolically. From the orthodox literalist seat, it is a snare: their authority extracted, their framework colonized. From the abolitionist seat, it is a snare with better marketing: the kernel itself is the problem. The engine will compute these divergences from the structural data — the authored claim (tangled_rope) reflects the analytical observer's synthesis.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist interpreters and institutional authorities are beneficiaries (d ~ 0.2-0.3): they gain interpretive monopoly, state recognition, and intellectual capital. Lower caste groups and women are payers (d ~ 0.7-0.8): they bear residual symbolic hierarchy costs with constrained exit (conversion costs, family rupture, social capital loss). Orthodox literalists are identity-locked excluded (d ~ 0.9): their entire vocational identity depends on literalism; they cannot exit without self-dissolution. Abolitionist rejectionists are mobile excluded (d ~ 0.1): they operate outside the textual framework entirely. Academic observers are analytical (d = 0.5). The reformist reading's claim to reduce victim set is structurally true relative to orthodox literalism but false relative to abolitionist rejection — the victim set is reduced, not eliminated.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (colonial/modern legitimacy crisis for Hindu law) is contested — orthodox say it was manufactured, abolitionists say it was the text itself. The reformist reading resolves the mandatrophy by redefining the mandate: not 'enforce Dharmasastra' but 'preserve dharma through Dharmasastra.' This prevents mislabeling coordination as pure extraction (the ethical vocabulary IS coordinative) but also prevents mislabeling extraction as pure coordination (symbolic hierarchy IS extractive). The tangled_rope classification captures this duality: the constraint coordinates AND extracts, and the reformist project is the active enforcement mechanism holding the hybrid together.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the reformist contextual reading a genuine structural alternative within the dharmasastra_corpus kernel, or a strategic reinterpretation that preserves the kernel''s authority while discarding its most oppressive prescriptions?',
    'Track whether reformist interpretations gain independent institutional footing (separate seminaries, distinct personal law codes, autonomous ritual pipelines) or remain parasitic on orthodox infrastructure. Longitudinal study of institutional fission/fusion.',
    'If parasitic, the reformist reading is a scaffold for the orthodox kernel — extraction persists through symbolic hierarchy. If independent, it constitutes a new kernel with its own constraint story (lower ε, different victim set).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether this reading is a distinct constraint or a modulation of the orthodox kernel.').

omega_variable(
    symbolic_vs_material_extraction,
    'How much of the measured extraction (ε=0.45) is material (legal disability, economic exclusion) versus symbolic (ritual stigma, social capital differentials)?',
    'Disaggregate suppression metrics by mechanism: legal (personal law provisions), economic (occupational networks), social (marriage markets, temple access), psychological (internalized hierarchy). Survey and ethnographic work across caste/gender intersections.',
    'If material extraction dominates, the constraint trends toward snare despite reformist framing. If symbolic dominates, it remains tangled_rope — coordination function genuine, extraction softened but persistent. Determines whether ''medium extraction'' is a stable equilibrium or a transition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(symbolic_vs_material_extraction, empirical, 'Decomposition of extraction into material vs. symbolic components.').

omega_variable(
    textual_authority_preservation,
    'Can Dharmasastra''s textual authority be preserved once the caste prescriptions that constitute most of its concrete content are declared time-bound?',
    'Observe whether reformist institutions produce new authoritative commentaries that generate binding norms, or merely issue advisory opinions. Track citation patterns in courts, temple governance, and family disputes.',
    'If authority collapses without caste prescriptions, the reformist reading is a scaffold with implicit sunset — the kernel''s legitimacy was inseparable from its hierarchy. If authority regenerates through ethical dharma alone, the reading is a stable tangled_rope with genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_authority_preservation, conceptual, 'Whether textual authority survives the excision of caste hierarchy.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by lower_caste_groups and women_under_patriarchal_interpretations primarily structural (legal barriers, economic exclusion) or internalized (ritual stigma, identity-fused subordination)?',
    'Post-reform trajectory analysis: where legal disabilities are removed (e.g., temple entry, anti-discrimination law), does suppression persist at similar levels? If yes, internalized component is significant. Compare suppression metrics across jurisdictions with different legal regimes.',
    'If internalized suppression dominates, the constraint''s effective suppression is higher than structural measures suggest — victims carry the constraint with them after legal exit. This would increase χ for payer seats and push classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanisms for payer seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__reformist_contextual, 1800, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dharmasastra_reformist_tr_t1800, dharmasastra_corpus__reformist_contextual, theater_ratio, 1800, 0.2).
narrative_ontology:measurement(dharmasastra_reformist_tr_t1850, dharmasastra_corpus__reformist_contextual, theater_ratio, 1850, 0.25).
narrative_ontology:measurement(dharmasastra_reformist_tr_t1900, dharmasastra_corpus__reformist_contextual, theater_ratio, 1900, 0.3).
narrative_ontology:measurement(dharmasastra_reformist_tr_t1950, dharmasastra_corpus__reformist_contextual, theater_ratio, 1950, 0.35).
narrative_ontology:measurement(dharmasastra_reformist_tr_t1980, dharmasastra_corpus__reformist_contextual, theater_ratio, 1980, 0.38).
narrative_ontology:measurement(dharmasastra_reformist_tr_t2000, dharmasastra_corpus__reformist_contextual, theater_ratio, 2000, 0.39).
narrative_ontology:measurement(dharmasastra_reformist_tr_t2024, dharmasastra_corpus__reformist_contextual, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(dharmasastra_reformist_be_t1800, dharmasastra_corpus__reformist_contextual, base_extractiveness, 1800, 0.75).
narrative_ontology:measurement(dharmasastra_reformist_be_t1850, dharmasastra_corpus__reformist_contextual, base_extractiveness, 1850, 0.7).
narrative_ontology:measurement(dharmasastra_reformist_be_t1900, dharmasastra_corpus__reformist_contextual, base_extractiveness, 1900, 0.6).
narrative_ontology:measurement(dharmasastra_reformist_be_t1950, dharmasastra_corpus__reformist_contextual, base_extractiveness, 1950, 0.5).
narrative_ontology:measurement(dharmasastra_reformist_be_t1980, dharmasastra_corpus__reformist_contextual, base_extractiveness, 1980, 0.48).
narrative_ontology:measurement(dharmasastra_reformist_be_t2000, dharmasastra_corpus__reformist_contextual, base_extractiveness, 2000, 0.46).
narrative_ontology:measurement(dharmasastra_reformist_be_t2024, dharmasastra_corpus__reformist_contextual, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(dharmasastra_reformist_su_t1800, dharmasastra_corpus__reformist_contextual, suppression_requirement, 1800, 0.8).
narrative_ontology:measurement(dharmasastra_reformist_su_t1850, dharmasastra_corpus__reformist_contextual, suppression_requirement, 1850, 0.7).
narrative_ontology:measurement(dharmasastra_reformist_su_t1900, dharmasastra_corpus__reformist_contextual, suppression_requirement, 1900, 0.55).
narrative_ontology:measurement(dharmasastra_reformist_su_t1950, dharmasastra_corpus__reformist_contextual, suppression_requirement, 1950, 0.4).
narrative_ontology:measurement(dharmasastra_reformist_su_t1980, dharmasastra_corpus__reformist_contextual, suppression_requirement, 1980, 0.35).
narrative_ontology:measurement(dharmasastra_reformist_su_t2000, dharmasastra_corpus__reformist_contextual, suppression_requirement, 2000, 0.33).
narrative_ontology:measurement(dharmasastra_reformist_su_t2024, dharmasastra_corpus__reformist_contextual, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__reformist_contextual, identity_coordination).
narrative_ontology:boltzmann_floor_override(dharmasastra_corpus__reformist_contextual, 0.08).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, hindu_personal_law_codification).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, temple_entry_movements).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, caste_based_reservation_policy).

% DUAL FORMULATION NOTE:
% Part of dharmasastra_corpus constraint family with orthodox_literalist and abolitionist_rejection readings. This reading's ε (0.45) differs from orthodox_literalist (ε ~ 0.75, explicit hierarchy enforcement) and abolitionist_rejection (ε ~ 0.1 for the rejection constraint itself, but the kernel's persistence extracts at ε ~ 0.6). The reformist reading's coordination function (identity_coordination) is the kernel's ethical core; its extraction is the residual symbolic hierarchy it cannot fully excise without losing textual authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dharmasastra_corpus__reformist_contextual, institutional, 0.25).
constraint_indexing:directionality_override(dharmasastra_corpus__reformist_contextual, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
