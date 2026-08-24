% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__orthodox_varna_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_corpus_social_prescription__orthodox_varna_reading, []).

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
 *   constraint_id: vedic_corpus_social_prescription__orthodox_varna_reading
 *   human_readable: Orthodox Varna Reading: Vedic Texts Prescribe Divine Caste Hierarchy
 *   domain: religious/social_stratification/hermeneutics
 *
 * SUMMARY:
 *   The orthodox varna reading treats Vedic texts (particularly Purusha Sukta
 *   RV 10.90, Dharmashastras) as literally prescribing a four-fold
 *   hierarchical division of humanity — Brahmin, Kshatriya, Vaishya, Shudra —
 *   with a fifth avarna (Dalit) outside the system, all as divinely mandated
 *   cosmic order (rita). This reading is instantiated through guru-shishya
 *   transmission, matha institutions, and ritual practice. It operates as a
 *   high-epsilon snare: Shudra and Dalit communities bear severe extraction
 *   (labor, dignity, mobility) through occupational, marital, and ritual
 *   restrictions actively enforced by social and historically state power;
 *   Brahmins occupy the agenda-setter/beneficiary seat monopolizing
 *   interpretation and ritual authority. The constraint's persistence depends
 *   on suppressing alternative readings and enforcing boundary maintenance
 *   through pollution/purity norms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__orthodox_varna_reading, 0.82).
domain_priors:suppression_score(vedic_corpus_social_prescription__orthodox_varna_reading, 0.88).
domain_priors:theater_ratio(vedic_corpus_social_prescription__orthodox_varna_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, accessibility_collapse, 0.87).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__orthodox_varna_reading, snare).
narrative_ontology:human_readable(vedic_corpus_social_prescription__orthodox_varna_reading, "Orthodox Varna Reading: Vedic Texts Prescribe Divine Caste Hierarchy").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__orthodox_varna_reading, "religious/social_stratification/hermeneutics").

domain_priors:requires_active_enforcement(vedic_corpus_social_prescription__orthodox_varna_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__orthodox_varna_reading, '3b85a770-b821-4699-96f5-8594bd741ac2').
narrative_ontology:cs_kernel_codification('3b85a770-b821-4699-96f5-8594bd741ac2', fixed_text).
narrative_ontology:cs_authority_grounding('3b85a770-b821-4699-96f5-8594bd741ac2', lineage).
narrative_ontology:cs_interpretation_layer_present('3b85a770-b821-4699-96f5-8594bd741ac2').
narrative_ontology:cs_reading_relation('3b85a770-b821-4699-96f5-8594bd741ac2', vedic_corpus_social_prescription__reformist_spiritual_reading, forecloses).
narrative_ontology:cs_reading_relation('3b85a770-b821-4699-96f5-8594bd741ac2', vedic_corpus_social_prescription__colonial_orientalist_reading, influences).
narrative_ontology:cs_axiom('3b85a770-b821-4699-96f5-8594bd741ac2', foundational, varna_hierarchy_divinely_mandated).
narrative_ontology:cs_axiom_status(varna_hierarchy_divinely_mandated, holdable).
narrative_ontology:cs_axiom_grounding('3b85a770-b821-4699-96f5-8594bd741ac2', varna_hierarchy_divinely_mandated, theological).
narrative_ontology:cs_axiom('3b85a770-b821-4699-96f5-8594bd741ac2', foundational, birth_ascribed_ritual_purity).
narrative_ontology:cs_axiom_status(birth_ascribed_ritual_purity, holdable).
narrative_ontology:cs_axiom_grounding('3b85a770-b821-4699-96f5-8594bd741ac2', birth_ascribed_ritual_purity, theological).
narrative_ontology:cs_axiom('3b85a770-b821-4699-96f5-8594bd741ac2', secondary, shudra_service_to_upper_varnas).
narrative_ontology:cs_axiom_status(shudra_service_to_upper_varnas, holdable).
narrative_ontology:cs_axiom_grounding('3b85a770-b821-4699-96f5-8594bd741ac2', shudra_service_to_upper_varnas, conventional).
narrative_ontology:cs_reference_frame('3b85a770-b821-4699-96f5-8594bd741ac2', vedic_revealed_cosmic_order).
narrative_ontology:cs_drift_state('3b85a770-b821-4699-96f5-8594bd741ac2', post_ambedkar_constitutional_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3b85a770-b821-4699-96f5-8594bd741ac2', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, shudra_caste).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, dalit_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, kshatriya_caste).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, vaishya_caste).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, kshatriya_caste).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, vaishya_caste).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__orthodox_varna_reading, varna_dharma_doctrine).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__orthodox_varna_reading, divine_cosmic_order_claim).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__orthodox_varna_reading, birth_ascribed_ritual_purity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Monopolizes Vedic interpretation, ritual authority, and educational transmission through guru-shishya parampara. Claims exclusive right to perform sacrifices, teach Vedas, and adjudicate dharma. Extracts material support (dakshina, land grants, state patronage) and social deference. Exit from this role requires abandoning caste identity and hereditary vocation — structurally near-impossible within the framework.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste, agenda_setter,
    institutional, civilizational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste, beneficiary).

% Holds political/military authority legitimated by Brahmin ritual coronation (rajasuya). Benefits from hierarchical rank above Vaishya/Shudra but pays through ritual dependence on Brahmins, obligatory patronage, and constraints on autonomy (must protect Brahmins, enforce varna boundaries). Exit means losing sovereign legitimacy within the system.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, kshatriya_caste, beneficiary,
    powerful, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__orthodox_varna_reading, kshatriya_caste, payer).

% Engages in commerce, agriculture, cattle-keeping — permitted economic activities. Pays taxes/tithes to state and ritual fees to Brahmins. Gains protection of property rights and ritual inclusion (upayana samskara) denied to Shudras. Exit downward is blocked by pollution norms; upward mobility is theoretically impossible (birth-ascribed).
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, vaishya_caste, payer,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__orthodox_varna_reading, vaishya_caste, beneficiary).

% Assigned service (seva) to the three upper varnas as sole dharma. Barred from Vedic study, sacraments (upayana), ritual participation, and most property ownership. Labor value extracted through obligatory service, low-wage occupational niches, and ritual disability. Resistance historically met with severe punishment (Manusmriti 8.270-272). Exit options: Sanskritization (mimic upper-varna customs over generations), conversion, or flight — all structurally constrained.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, shudra_caste, payer,
    moderate, biographical, constrained, continental).

% Placed outside varna system entirely (avarna/antyaja). Subjected to untouchability: residential segregation, denial of water/temple/education access, forced degrading occupations (manual scavenging, corpse disposal), violence for boundary transgression. Extractive intensity highest — labor coerced, dignity denied, resistance met with collective punishment. Exit nearly nonexistent within framework: conversion offers partial relief but caste stigma persists; spatial mobility limited by kinship/economic ties.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, dalit_communities, payer,
    powerless, biographical, trapped, continental).

% Bhakti saints (Ravidas, Kabir, Chokhamela), Arya Samaj, Brahmo Samaj, Gandhi (varnashrama reform), Ambedkar (initial reform then rejection). Argue for spiritual equality, textual reinterpretation, or annihilation of caste. Systematically marginalized by orthodox institutions: denied platform in mathas, excluded from shastrartha debates, labeled heretics. Their exclusion is structural — the reading's coherence depends on silencing internal dissent.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, reformist_hindu_voices, excluded,
    moderate, generational, constrained, continental).

% British colonial state (late 18th–20th c.) codified 'Hindu law' from Dharmashastra texts via pandits, censuses, and courts. Treated orthodox reading as authentic 'Hinduism' for governance. Their observation hardened the text-practice gap into positive law, amplifying enforcement. Not a participant in the religious framework but shaped its modern institutional form.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, colonial_administrators, observer,
    institutional, generational, analytical, continental).

% Ambedkarite movement, Dalit Panthers, contemporary Dalit Bahujan intellectuals. Analyze the constraint as Brahminical enforcement mechanism. Produce counter-readings (Navayana Buddhism, constitutionalism). Their analytical seat is won through struggle — not granted by the system. They document extraction, organize resistance, and demand annihilation of the constraint.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, modern_dalit_activists, observer,
    organized, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social order through divinely mandated occupational specialization and ritual hierarchy, claiming to maintain cosmic harmony (rita) through proper role fulfillment by birth-ascribed groups.
% TRANSFER_FUNCTION: Moves ritual purity, educational access, land ownership, political authority, and labor value from lower varnas (especially Shudra/Dalit) to upper varnas (especially Brahmin), mediated through birth-ascribed status, ritual gatekeeping, and scriptural authority.
% ABSENT_VOICES: Dalit communities historically excluded from textual interpretation and shastrartha; women across varnas whose gendered subordination is compounded by varna (Manusmriti 5.147-148); anti-caste movements (Phule, Ambedkar, Periyar, Ravidas, Kabir) whose readings were suppressed or appropriated; Buddhist/Jain/Shramanic traditions that rejected varna from outside.
% DISAPPEARANCE_RATIONALE: If the divine mandate claim vanished, the theological justification for birth-ascribed hierarchy would collapse, enabling legal/constitutional equality frameworks (Articles 14-17, 21) to operate without religious legitimacy barriers; occupational mobility, intermarriage, temple entry, and ritual access would restructure Indian social life fundamentally — the constraint is the keystone of the caste architecture.
% FOUNDING_PROBLEM: Maintain social cohesion and cosmic order (rita) in a diverse, stratified society by assigning each group a divinely ordained function, preventing chaos (anrita) through structured hierarchy where each varna's dharma sustains the whole.
% FOUNDING_PROBLEM_CORROBORATION: Traditionalist Shankaracharyas and orthodox mathas attest the problem is live (cosmic order requires varna). Ambedkar, Phule, Periyar, and modern Dalit Bahujan movements attest it is dead/constructed — the 'problem' was always power, not chaos. Colonial ethnographers (Risley, Census Commissioners) document it as administered structure for revenue/control. No neutral corroboration exists; all attestations are positioned.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__orthodox_varna_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__orthodox_varna_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__orthodox_varna_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vedic_corpus_social_prescription__orthodox_varna_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__orthodox_varna_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_corpus_social_prescription__orthodox_varna_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vedic_corpus_social_prescription__orthodox_varna_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vedic_corpus_social_prescription__orthodox_varna_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is very high (0.82) because the constraint transfers material resources (land, labor, education access) and symbolic capital (ritual purity, spiritual authority) from lower to upper varnas through birth-ascribed status with no exit. Suppression is extreme (0.88) — the system deploys violence, social boycott, legal disability, and internalized pollution norms to maintain boundaries. Theater ratio is moderate (0.38): ritual performance (yajnas, pujas) has genuine coordination function for participants, but a substantial fraction of enforcement activity (policing inter-dining, intermarriage, temple entry, occupational boundaries) serves extraction, not coordination. Accessibility collapse is near-total (0.87) — the divine mandate claim renders alternatives (equality, mobility) cosmologically illegitimate. Resistance is moderate (0.45) — continuous but structurally constrained (Bhakti, Sikhism, Buddhism, Ambedkarism, constitutional challenges).
 *
 * PERSPECTIVAL GAP:
 *   From the Brahmin seat, the constraint is genuine coordination (rope-like): it structures society, preserves Vedic knowledge, maintains cosmic order. From the Dalit seat, it is pure extraction (snare): every dimension of life is constrained for others' benefit. The engine computes this divergence from the structural data — the authored snare claim reflects the dominant operational reality for the majority of those governed.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmins are structural beneficiaries (d ≈ 0.1): they collect ritual fees, land grants, state patronage, and epistemic authority; their identity is fused with the constraint (identity_locked). Kshatriyas and Vaishyas sit near symmetric (d ≈ 0.4-0.5): they gain rank/property protections but pay ritual dependence and patronage. Shudras are targets (d ≈ 0.8): constrained exit, obligatory service, barred from Vedic knowledge. Dalits are extreme targets (d ≈ 0.95): trapped, subjected to untouchability, maximum extraction. Reformist voices are excluded (d undefined — not in the game). Colonial administrators and modern activists are analytical observers (d ≈ 0.0).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (cosmic order through hierarchy) is contested: traditionalists say live; reformists say dead (the problem was power, not chaos). The constraint persists despite constitutional abolition of untouchability (Art. 17) and anti-discrimination law because the theological mandate outlives legal sanction — mandatrophy unresolved. The reading's authority derives from lineage transmission, not problem-solving efficacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of a contested kernel (vedic_corpus_social_prescription) rather than a standalone constraint?',
    'Committer-frame analysis: the SCOPE manifest identifies kernel_id and reading_id; the structural delta (victim set, beneficiary, ε) differs across declared readings. This is not an empirical question but a framing commitment — the JSON declares this as a kernel reading per Rule 1.',
    'If treated as standalone, the ε-invariance principle is violated: the same textual corpus would appear to have variable ε depending on interpretive lens. Kernel framing preserves ε-invariance by assigning each reading its own constraint_id and ε.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment that this is a kernel reading, not a standalone constraint').

omega_variable(
    sibling_reading_delta,
    'How do the sibling readings structurally differ in ε, victim set, and enforcement logic?',
    'Author the sibling constraints separately and compare: reformist_spiritual_reading should have ε ≈ 0.1 (no prescriptive hierarchy), victim set empty or minimal; colonial_orientalist_reading should have ε ≈ 0.6 (administrative extraction via codification), victim set includes all colonized subjects, enforcement is state law not ritual. The deltas are structural, not perspectival.',
    'If sibling deltas are small, the kernel may be artificial — the readings would be variants of one constraint. Large deltas confirm genuine kernel contestation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_delta, empirical, 'Structural differences between this reading and its siblings').

omega_variable(
    suppression_mechanism_internalized,
    'Is the measured suppression (0.88) primarily structural (external barriers) or internalized (pollution norms, identity fusion, self-policing)?',
    'Post-exit suppression trajectory: track Dalit converts to Buddhism/Christianity/Sikhism — does varna-based suppression persist after formal exit? If yes, internalized component is significant. Compare with Shudra Sanskritization outcomes.',
    'If internalized, effective suppression exceeds structural measure — the constraint travels with the agent. This would increase χ for identity_locked/trapped agents beyond the engine''s structural derivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Structural vs. internalized suppression in caste boundary maintenance').

omega_variable(
    divine_mandate_epistemic_status,
    'Does the orthodox reading''s claim of ''literal divine mandate'' function as an empirical claim (falsifiable by textual/historical evidence) or a deontological commitment (immune to evidence)?',
    'Analyze orthodox responses to historical-critical scholarship (linguistic dating, textual layers, archaeological evidence against Vedic antiquity of varna). If responses shift to ''shraddha (faith) transcends history'', the claim is deontological. If they engage evidentially, it is empirically_contingent.',
    'If empirically_contingent and evidence accumulates against Vedic antiquity of rigid varna, the axiom_overriding drift direction in cs_structure becomes active — foreclosure risk rises. If deontological, drift cannot foreclose regardless of evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_mandate_epistemic_status, conceptual, 'Epistemic status of the divine mandate claim under evidentiary challenge').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__orthodox_varna_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedic_orthodox_varna_tr_t0, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(vedic_orthodox_varna_tr_t100, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 100, 0.3).
narrative_ontology:measurement(vedic_orthodox_varna_tr_t200, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 200, 0.35).
narrative_ontology:measurement(vedic_orthodox_varna_tr_t300, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 300, 0.42).
narrative_ontology:measurement(vedic_orthodox_varna_tr_t400, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 400, 0.38).
narrative_ontology:measurement(vedic_orthodox_varna_tr_t500, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 500, 0.38).

% Extraction over time
narrative_ontology:measurement(vedic_orthodox_varna_be_t0, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(vedic_orthodox_varna_be_t100, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 100, 0.72).
narrative_ontology:measurement(vedic_orthodox_varna_be_t200, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 200, 0.78).
narrative_ontology:measurement(vedic_orthodox_varna_be_t300, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 300, 0.85).
narrative_ontology:measurement(vedic_orthodox_varna_be_t400, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 400, 0.75).
narrative_ontology:measurement(vedic_orthodox_varna_be_t500, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 500, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(vedic_orthodox_varna_su_t0, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(vedic_orthodox_varna_su_t100, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 100, 0.8).
narrative_ontology:measurement(vedic_orthodox_varna_su_t200, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 200, 0.85).
narrative_ontology:measurement(vedic_orthodox_varna_su_t300, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 300, 0.9).
narrative_ontology:measurement(vedic_orthodox_varna_su_t400, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 400, 0.82).
narrative_ontology:measurement(vedic_orthodox_varna_su_t500, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 500, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__orthodox_varna_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_corpus_social_prescription__orthodox_varna_reading, 0.08).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription__reformist_spiritual_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription__colonial_orientalist_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the colloquial label 'Vedic varna system' into three structurally distinct readings with divergent ε (0.82 vs ~0.1 vs ~0.6), victim sets (Shudra/Dalit vs none/minimal vs all colonized), and enforcement logics (ritual/social vs none vs state law). The orthodox reading forecloses the reformist reading within a single framework; the colonial reading influenced the orthodox reading's institutional hardening.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vedic_corpus_social_prescription__orthodox_varna_reading, institutional, 0.1).
constraint_indexing:directionality_override(vedic_corpus_social_prescription__orthodox_varna_reading, powerless, 0.95).
constraint_indexing:directionality_override(vedic_corpus_social_prescription__orthodox_varna_reading, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
