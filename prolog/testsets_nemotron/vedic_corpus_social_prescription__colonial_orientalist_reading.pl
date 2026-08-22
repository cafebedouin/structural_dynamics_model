% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__colonial_orientalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_corpus_social_prescription__colonial_orientalist_reading, []).

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
 *   constraint_id: vedic_corpus_social_prescription__colonial_orientalist_reading
 *   human_readable: Colonial Orientalist Reading: Vedic/Dharmashastra as Unified Hindu Law for Administrative Codification
 *   domain: religious_studies/social_stratification/hermeneutics/colonial_governance
 *
 * SUMMARY:
 *   This constraint story models the colonial orientalist reading of
 *   Vedic/Dharmashastra texts as a unified, timeless 'Hindu law' system
 *   constructed for administrative codification in British India (1772–1947).
 *   The reading was instantiated by Warren Hastings' 1772 judicial plan,
 *   developed through the work of orientalist scholars (Jones, Colebrooke,
 *   Wilkins) and colonial legal officials, and enforced through the
 *   Anglo-Hindu law applied in colonial courts. It crystallized fluid,
 *   context-dependent social practices and diverse textual traditions into
 *   fixed legal categories — especially caste status — to create legible
 *   subjects for census, taxation, and adjudication. The beneficiary is the
 *   colonial administration; the victims are colonized legal subjects
 *   subjected to codified caste law. The constraint is a scaffold: it was
 *   explicitly transitional in colonial rhetoric (ruling 'by their own laws'
 *   until 'civilization' allowed uniform civil code), carried a de facto
 *   sunset in the form of promised legal reform, but persisted for 175 years
 *   and its categories outlived the colonial state.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.58).
domain_priors:suppression_score(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.62).
domain_priors:theater_ratio(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__colonial_orientalist_reading, scaffold).
narrative_ontology:human_readable(vedic_corpus_social_prescription__colonial_orientalist_reading, "Colonial Orientalist Reading: Vedic/Dharmashastra as Unified Hindu Law for Administrative Codification").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__colonial_orientalist_reading, "religious_studies/social_stratification/hermeneutics/colonial_governance").

domain_priors:requires_active_enforcement(vedic_corpus_social_prescription__colonial_orientalist_reading).
narrative_ontology:has_sunset_clause(vedic_corpus_social_prescription__colonial_orientalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__colonial_orientalist_reading, '473a4306-7572-42e7-8c68-6d091d376d0e').
narrative_ontology:cs_kernel_codification('473a4306-7572-42e7-8c68-6d091d376d0e', fixed_text).
narrative_ontology:cs_authority_grounding('473a4306-7572-42e7-8c68-6d091d376d0e', extraction).
narrative_ontology:cs_interpretation_layer_present('473a4306-7572-42e7-8c68-6d091d376d0e').
narrative_ontology:cs_reading_relation('473a4306-7572-42e7-8c68-6d091d376d0e', vedic_corpus_social_prescription__orthodox_varna_reading, influences).
narrative_ontology:cs_reading_relation('473a4306-7572-42e7-8c68-6d091d376d0e', vedic_corpus_social_prescription__reformist_spiritual_reading, forecloses).
narrative_ontology:cs_axiom('473a4306-7572-42e7-8c68-6d091d376d0e', foundational, dharmashastra_texts_constitute_single_legal_code).
narrative_ontology:cs_axiom_status(dharmashastra_texts_constitute_single_legal_code, holdable).
narrative_ontology:cs_axiom_grounding('473a4306-7572-42e7-8c68-6d091d376d0e', dharmashastra_texts_constitute_single_legal_code, conventional).
narrative_ontology:cs_axiom('473a4306-7572-42e7-8c68-6d091d376d0e', foundational, colonial_state_authorized_to_codify_religious_law).
narrative_ontology:cs_axiom_status(colonial_state_authorized_to_codify_religious_law, overridden).
narrative_ontology:cs_axiom_grounding('473a4306-7572-42e7-8c68-6d091d376d0e', colonial_state_authorized_to_codify_religious_law, instrumental).
narrative_ontology:cs_reference_frame('473a4306-7572-42e7-8c68-6d091d376d0e', pre_colonial_customary_law_plurality).
narrative_ontology:cs_drift_state('473a4306-7572-42e7-8c68-6d091d376d0e', high_court_era_1862, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('473a4306-7572-42e7-8c68-6d091d376d0e', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__colonial_orientalist_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_administration).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_legal_officials).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, indigenous_intermediaries_codifiers).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, colonized_legal_subjects).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, local_communities_subjected_to_codified_caste_law).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, traditional_panchayat_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, indigenous_intermediaries_codifiers).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__colonial_orientalist_reading, legal_uniformity_enables_imperial_governance).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__colonial_orientalist_reading, textual_fixity_creates_administrative_legibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiates and funds the codification of 'Hindu law' from Dharmashastra texts to create a uniform legal code for revenue collection, property adjudication, and social control across British India. The administration selects which texts count as authoritative, appoints pandits to interpret them, and enforces the resulting Anglo-Hindu law through colonial courts. It benefits from legible categories for taxation and census but does not bear the social costs of the rigidified system.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_administration, agenda_setter,
    institutional, generational, arbitrage, continental).

% British judges and legal officers who administer the codified Hindu law in colonial courts. They gain professional authority, career advancement, and intellectual prestige from mastering and applying the constructed legal system. Their exit is mobile — they can transfer to other postings or return to Britain — but within the system they are invested in its coherence.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_legal_officials, beneficiary,
    organized, biographical, mobile, regional).

% Brahmin pandits and scribes employed by the colonial state to translate, interpret, and codify Sanskrit texts. They gain patronage, status, and material reward as authorized interpreters, but their authority is contingent on colonial approval and their interpretations are constrained by the administration's demand for fixed, citable rules. They pay with the distortion of their own textual traditions and the alienation from community-based disputation.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, indigenous_intermediaries_codifiers, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__colonial_orientalist_reading, indigenous_intermediaries_codifiers, payer).

% The vast population of Indians subjected to the newly codified 'Hindu law' in matters of inheritance, marriage, adoption, and caste status. Previously governed by fluid, context-dependent customary practices and local councils (panchayats), they now face rigid textual categories enforced by colonial courts. Exit is effectively trapped — the colonial legal system claims universal jurisdiction, and customary alternatives are systematically displaced or rendered legally invisible.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, colonized_legal_subjects, payer,
    powerless, biographical, trapped, local).

% Village and jati communities whose internal governance, status negotiations, and dispute resolution are overridden by the colonial court's reference to fixed textual caste categories. The codification transforms negotiated social relations into immutable legal identities. Their exclusion is structural: they are not consulted in the codification, and their customary law is treated as 'corruption' of the true text. Identity lock operates because caste status becomes a legal fact recorded in colonial registers, making exit from the categorization nearly impossible.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, local_communities_subjected_to_codified_caste_law, payer,
    powerless, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__colonial_orientalist_reading, local_communities_subjected_to_codified_caste_law, excluded).

% Local councils and customary authorities that previously adjudicated disputes through deliberation, precedent, and community consensus. Their jurisdiction is eroded as colonial courts assert supremacy and reference codified texts rather than living custom. They are excluded from the codification process and their decisions carry no weight in the new legal hierarchy. Exit is constrained — some adapt by becoming intermediaries, but most lose functional authority.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, traditional_panchayat_authorities, excluded,
    moderate, generational, constrained, local).

% European philologists and administrators (Jones, Colebrooke, Wilkins, etc.) who 'discover,' translate, and frame the Dharmashastras as a unified legal code. They provide the intellectual architecture for the constraint, claiming to recover an ancient system while actually constructing it for colonial use. Their analytical seat is privileged — they define what the texts 'really say' — but they do not directly administer or bear the law's consequences.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, orientalist_scholars, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the colonial administration's problem of governing a vast, diverse population with minimal personnel: by positing a single, textually grounded 'Hindu law,' it creates a uniform, cite-able legal framework that British judges can apply without learning thousands of local customs.
% TRANSFER_FUNCTION: Transfers interpretive authority and legal determinacy from local communities and customary councils to colonial courts and their appointed pandit-interpreters. Moves the power to define caste status, inheritance rights, and marriage validity from negotiated social practice to fixed textual categories enforced by state courts. The colonial state gains legibility and control; colonized subjects lose the flexibility of customary law.
% ABSENT_VOICES: The millions of non-literate, non-Brahmin, and lower-caste subjects whose lives were governed by the codified categories but who had no representation in the textual selection, translation, or codification process. Also absent: the diverse customary laws of tribal, regional, and minority communities that were subsumed under the blanket category 'Hindu law' or ignored entirely.
% DISAPPEARANCE_RATIONALE: If the colonial codification vanished overnight, the legal uniformity it imposed would dissolve. Customary law, panchayat adjudication, and fluid caste negotiations would likely re-emerge, but the colonial state's capacity to govern through law — and the textual fixation of caste identity in legal records — would be severely disrupted. The categories created (e.g., legal caste status as inheritable property) have persisting effects even after formal repeal.
% FOUNDING_PROBLEM: The East India Company needed a coherent legal system for its expanding territorial governance in Bengal after 1772. Warren Hastings' plan for 'the laws of the Koran and the Shaster' aimed to rule Indians by their own laws to secure legitimacy and reduce administrative cost, but the 'laws of the Shaster' had to be found, fixed, and made administrable.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem — colonial governance needing a uniform legal code — is dead: the British Empire ended in 1947. The corroborating sources outside the beneficiary set are the nationalist legal reformers (e.g., Rau, Ambedkar) and postcolonial historians (e.g., Derrett, Rocher, Cohn) who document that the Anglo-Hindu law was a colonial construction, not a recovery of pre-existing unity. The beneficiary set (colonial officials, orientalist scholars) maintained until the end that they were discovering, not making, the law.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__colonial_orientalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__colonial_orientalist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__colonial_orientalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(vedic_corpus_social_prescription__colonial_orientalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_corpus_social_prescription__colonial_orientalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vedic_corpus_social_prescription__colonial_orientalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vedic_corpus_social_prescription__colonial_orientalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate but significant: the constraint extracts interpretive authority and social flexibility from colonized communities and transfers it to colonial courts, but the colonial state also incurs real administrative costs in maintaining the legal machinery. Suppression (0.62) is substantial: the constraint's persistence depends on actively displacing customary law, panchayat jurisdiction, and alternative textual readings, enforced through the colonial court hierarchy. Theater ratio (0.45) is moderately high: the stated justification ('recovering ancient Hindu law') increasingly diverges from the actual operation (constructing a rigid legal code for governance), especially as nationalist and reformist challenges mount in the late 19th century. Accessibility collapse (0.7) is high: once the colonial courts establish textual citations as the sole legal authority, customary alternatives become legally invisible — but not socially extinct, hence not maximal. Resistance (0.55) is moderate: there is sustained resistance from traditional authorities, nationalist reformers, and affected communities, but it is channeled into the colonial legal framework (petitioning, legislative advocacy) rather than overturning it.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (colonial administration) experiences the constraint as a genuine coordination solution — it solves the problem of governing diversity with minimal personnel. The payer seats (colonized subjects, local communities) experience it as extraction — their social fluidity is frozen into legal categories for state convenience. The beneficiary seats (colonial officials, pandits) experience it as both: they gain real authority and resources, but their authority is derivative and contingent. The engine computes this divergence from the structural data; the authored claim (scaffold) captures the transitional intent but the metrics capture the extractive persistence.
 *
 * DIRECTIONALITY LOGIC:
 *   The colonial administration and its legal officials are structural beneficiaries (d near 0.0–0.2): they gain governance capacity, revenue legibility, and professional authority from the constraint. Indigenous intermediaries (pandits) are dual-positioned: they gain patronage and status as authorized interpreters (beneficiary) but lose textual autonomy and communal embeddedness (payer), with constrained exit. Colonized legal subjects and local communities are full targets (d near 0.8–1.0): they bear the costs of rigidified categories, loss of customary flexibility, and identity fixation in legal records, with trapped or identity-locked exit. Traditional panchayat authorities are excluded (no seat at the codification table) with constrained exit — some adapt, most are displaced. Orientalist scholars occupy the analytical observer seat (d = 0.5 by definition): they construct the reading but do not administer or bear its consequences.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was founded to solve a specific colonial governance problem (uniform law for revenue and order). That problem died with the empire in 1947 — founding_problem_status = dead. Yet the constraint's categories (codified caste law, textual fixation of personal law) persisted into postcolonial Indian law via the Hindu Code Bills and the Constitution's personal law framework. The mandatrophy is resolved in the sense that the original administrative mandate is gone, but unresolved in the sense that the constraint's structural effects (legal caste categories, textual authority over custom) continue to extract from the same victim populations under a new sovereign. The scaffold became a tangled rope: the coordination function (colonial governance) atrophied, but the extraction function (state control over personal law via fixed texts) persisted and was repurposed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    colonial_construction_vs_recovery_ambiguity,
    'To what extent did the colonial administration and orientalist scholars genuinely believe they were recovering a pre-existing unified Hindu law, versus consciously constructing it for governance?',
    'Archival research on private correspondence, draft legislation, and pandit consultations — especially evidence of deliberate textual selection, suppression of variant manuscripts, and editorial choices that produced uniformity where diversity existed.',
    'If conscious construction, the scaffold''s transitional justification is fraudulent from inception — the constraint is a snare disguised as a scaffold. If genuine recovery belief, the scaffold framing is sincere but the extraction is structural (epistemic violence). Either way, the beneficiary/victim structure holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_construction_vs_recovery_ambiguity, conceptual, 'Whether the scaffold''s founding rationale was sincere recovery or deliberate construction.').

omega_variable(
    customary_law_persistence_vs_displacement,
    'How completely did the Anglo-Hindu law actually displace customary law in practice, versus creating a dual system where colonial courts applied textual law while communities continued customary practice?',
    'Empirical studies of court records vs. ethnographic accounts of village-level dispute resolution across the period, especially post-1860 when High Courts were established.',
    'If displacement was partial, the accessibility_collapse metric overstates the constraint''s reach — the constraint operates primarily in the colonial legal sphere, not the social sphere. If displacement was near-total, the metric is accurate and the constraint''s extraction extends deep into social life.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(customary_law_persistence_vs_displacement, empirical, 'The actual reach of codified law vs. persistence of customary practice.').

omega_variable(
    postcolonial_continuity_mechanism,
    'By what mechanism did the colonial codification''s categories (especially legal caste status) persist into postcolonial Indian personal law, and does this constitute the same constraint or a new one?',
    'Legal-historical analysis of the Hindu Code Bills debates (1948–1956), the Constitution''s Article 44 (uniform civil code) vs. Article 25 (religious freedom), and the Supreme Court''s ''essential religious practices'' doctrine.',
    'If the same constraint persists, the scaffold''s sunset was illusory and the extraction continues under new sovereignty — the constraint family extends beyond the colonial interval. If a new constraint, the colonial scaffold genuinely ended and a postcolonial tangled_rope replaced it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(postcolonial_continuity_mechanism, conceptual, 'Whether the colonial scaffold''s categories persist as the same constraint or a successor constraint.').

omega_variable(
    pandit_agency_vs_capture,
    'Were the indigenous pandit-intermediaries active co-constructors of the codified law (exercising agency within constraints) or passive instruments of colonial epistemic extraction?',
    'Analysis of pandit correspondence, dissenting opinions in digest compilations, and the evolution of their interpretive strategies across the period.',
    'If active co-constructors, the indigenous_intermediaries_codifiers seat has more beneficiary weight and the constraint''s extraction is partially endogenous. If passive instruments, their secondary_role as payer dominates and the constraint is more purely colonial extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pandit_agency_vs_capture, conceptual, 'The degree of indigenous agency in the codification process.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__colonial_orientalist_reading, 1772, 1947).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedic_colonial_orientalist_tr_t1772, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1772, 0.2).
narrative_ontology:measurement(vedic_colonial_orientalist_tr_t1800, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1800, 0.25).
narrative_ontology:measurement(vedic_colonial_orientalist_tr_t1830, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1830, 0.3).
narrative_ontology:measurement(vedic_colonial_orientalist_tr_t1860, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1860, 0.38).
narrative_ontology:measurement(vedic_colonial_orientalist_tr_t1890, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1890, 0.42).
narrative_ontology:measurement(vedic_colonial_orientalist_tr_t1920, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1920, 0.44).
narrative_ontology:measurement(vedic_colonial_orientalist_tr_t1947, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1947, 0.45).

% Extraction over time
narrative_ontology:measurement(vedic_colonial_orientalist_be_t1772, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1772, 0.25).
narrative_ontology:measurement(vedic_colonial_orientalist_be_t1800, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1800, 0.35).
narrative_ontology:measurement(vedic_colonial_orientalist_be_t1830, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1830, 0.45).
narrative_ontology:measurement(vedic_colonial_orientalist_be_t1860, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1860, 0.52).
narrative_ontology:measurement(vedic_colonial_orientalist_be_t1890, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1890, 0.55).
narrative_ontology:measurement(vedic_colonial_orientalist_be_t1920, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1920, 0.57).
narrative_ontology:measurement(vedic_colonial_orientalist_be_t1947, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1947, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(vedic_colonial_orientalist_su_t1772, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1772, 0.35).
narrative_ontology:measurement(vedic_colonial_orientalist_su_t1800, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1800, 0.42).
narrative_ontology:measurement(vedic_colonial_orientalist_su_t1830, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1830, 0.5).
narrative_ontology:measurement(vedic_colonial_orientalist_su_t1860, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1860, 0.55).
narrative_ontology:measurement(vedic_colonial_orientalist_su_t1890, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1890, 0.58).
narrative_ontology:measurement(vedic_colonial_orientalist_su_t1920, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1920, 0.6).
narrative_ontology:measurement(vedic_colonial_orientalist_su_t1947, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1947, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__colonial_orientalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.12).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, anglo_hindu_law_codification).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_census_caste_classification).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, postcolonial_hindu_code_bills).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, vedic_corpus_social_prescription__orthodox_varna_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, vedic_corpus_social_prescription__reformist_spiritual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the vedic_corpus_social_prescription kernel. The orthodox_varna_reading treats the texts as divinely mandated cosmic order (higher extractiveness on spiritual authority, lower on administrative coordination). The reformist_spiritual_reading treats the texts as metaphorical cosmology with no prescriptive content (near-zero extractiveness, mountain-like). All three share the same textual referent but instantiate different constraints with different beneficiary/victim structures and epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vedic_corpus_social_prescription__colonial_orientalist_reading, moderate, 0.35).
constraint_indexing:directionality_override(vedic_corpus_social_prescription__colonial_orientalist_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
