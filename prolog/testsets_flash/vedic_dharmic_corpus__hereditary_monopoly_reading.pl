% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__hereditary_monopoly_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_dharmic_corpus__hereditary_monopoly_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vedic_dharmic_corpus__hereditary_monopoly_reading
 *   human_readable: Hereditary Monopoly on Vedic-Dharmic Ritual Authority
 *   domain: religious/social_stratification
 *
 * SUMMARY:
 *   This constraint describes the 'hereditary monopoly' reading of the
 *   Vedic-Dharmic corpus, where ritual and interpretive authority are
 *   exclusively derived from birth into the Brahmin lineage, and the varna
 *   (caste) hierarchy is considered divinely ordained. This reading is
 *   actively enforced through social norms, institutional control of temples,
 *   and the ritual economy, leading to significant extraction from lower
 *   castes and women. It is one of several contested readings of the broader
 *   Vedic-Dharmic kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.68).
domain_priors:suppression_score(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.75).
domain_priors:theater_ratio(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__hereditary_monopoly_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__hereditary_monopoly_reading, "Hereditary Monopoly on Vedic-Dharmic Ritual Authority").
narrative_ontology:topic_domain(vedic_dharmic_corpus__hereditary_monopoly_reading, "religious/social_stratification").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__hereditary_monopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__hereditary_monopoly_reading, '2d1714c2-7fdc-41df-beaa-9da26ceaaf9d').
narrative_ontology:cs_kernel_codification('2d1714c2-7fdc-41df-beaa-9da26ceaaf9d', fixed_text).
narrative_ontology:cs_authority_grounding('2d1714c2-7fdc-41df-beaa-9da26ceaaf9d', lineage).
narrative_ontology:cs_interpretation_layer_present('2d1714c2-7fdc-41df-beaa-9da26ceaaf9d').
narrative_ontology:cs_reading_relation('2d1714c2-7fdc-41df-beaa-9da26ceaaf9d', vedic_dharmic_corpus__bhakti_devotional_reading, coexists_with).
narrative_ontology:cs_reading_relation('2d1714c2-7fdc-41df-beaa-9da26ceaaf9d', vedic_dharmic_corpus__reformist_egalitarian_reading, coexists_with).
narrative_ontology:cs_axiom('2d1714c2-7fdc-41df-beaa-9da26ceaaf9d', foundational, ritual_authority_by_birth).
narrative_ontology:cs_axiom_status(ritual_authority_by_birth, holdable).
narrative_ontology:cs_axiom_grounding('2d1714c2-7fdc-41df-beaa-9da26ceaaf9d', ritual_authority_by_birth, theological).
narrative_ontology:cs_axiom('2d1714c2-7fdc-41df-beaa-9da26ceaaf9d', foundational, varna_hierarchy_divinely_ordained).
narrative_ontology:cs_axiom_status(varna_hierarchy_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('2d1714c2-7fdc-41df-beaa-9da26ceaaf9d', varna_hierarchy_divinely_ordained, theological).
narrative_ontology:cs_reference_frame('2d1714c2-7fdc-41df-beaa-9da26ceaaf9d', ancient_vedic_tradition).
narrative_ontology:cs_drift_state('2d1714c2-7fdc-41df-beaa-9da26ceaaf9d', contemporary_social_reform_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2d1714c2-7fdc-41df-beaa-9da26ceaaf9d', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, lower_castes).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, women_in_ritual_contexts).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, non_brahmin_devotees).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and interprets Vedic rituals, texts, and dharma. Benefits from exclusive access to ritual fees, social prestige, and interpretive authority, which are inherited by birth. Actively enforces lineage-based exclusion from ritual roles and scriptural interpretation.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class, agenda_setter,
    institutional, generational, identity_locked, regional).

% Are ritually excluded from direct participation in many Vedic ceremonies and from interpreting sacred texts. Bear the social and spiritual costs of this exclusion, often relying on Brahmin priests for mediation with the divine. Exit is severely constrained by social structure and internalized norms.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, lower_castes, payer,
    powerless, generational, trapped, local).

% Are generally excluded from performing Vedic rituals or holding priestly roles, regardless of caste. Their spiritual agency is often mediated through male family members or Brahmin priests. Identity-locked by traditional gender roles within the religious framework.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, women_in_ritual_contexts, payer,
    powerless, generational, identity_locked, local).

% Seek spiritual guidance and ritual services but are denied direct access to priestly roles or interpretive authority due to birth. They must rely on the Brahmin class for mediation, incurring social and sometimes financial costs. Their options are limited to accepting the system or seeking alternative devotional paths.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, non_brahmin_devotees, payer,
    moderate, biographical, constrained, regional).

% Advocate for direct devotional access to the divine, bypassing caste and lineage requirements. Their alternative spiritual paths challenge the Brahmin monopoly but are often marginalized or dismissed by traditional authorities. They represent a historical and ongoing source of resistance.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, bhakti_movement_adherents, excluded,
    organized, generational, mobile, continental).

% Analyze the Vedic-Dharmic corpus through the lens of modern constitutional law and egalitarian principles. Argue that caste hierarchy is a social construct, not a divine mandate, and advocate for legal reforms to dismantle discriminatory practices. Their work challenges the interpretive authority of the Brahmin class.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, reformist_legal_scholars, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, authoritative framework for the performance of complex Vedic rituals and the interpretation of sacred texts, ensuring continuity of tradition and spiritual practice across generations.
% TRANSFER_FUNCTION: Transfers exclusive ritual and interpretive authority, social prestige, and economic benefits (ritual fees) from the broader community to the Brahmin priestly class, based on birthright.
% ABSENT_VOICES: Historically, marginalized groups and women who were denied access to education and ritual roles were absent from the interpretive discourse. Today, their descendants and advocates (e.g., Dalit activists, feminist theologians) would challenge the divine ordination of hierarchy and demand inclusive access to spiritual authority.
% DISAPPEARANCE_RATIONALE: If the hereditary monopoly on ritual and interpretive authority vanished, the entire structure of traditional Hindu society would undergo profound rearrangement. Rituals might become more accessible, new interpretive traditions would emerge, and the social hierarchy would lose a key legitimizing pillar, leading to a reordering of social and spiritual power.
% FOUNDING_PROBLEM: To preserve the integrity and efficacy of complex Vedic rituals and the correct interpretation of sacred texts, believed to be essential for cosmic order and individual spiritual well-being, by entrusting them to a specially trained and ritually pure class.
% FOUNDING_PROBLEM_CORROBORATION: The Brahmin priestly class asserts the problem is still live, emphasizing the need for ritual purity and specialized knowledge to maintain cosmic order. Reformist scholars and social activists, however, contend that the original problem of ritual preservation has been superseded by the problem of social injustice and that the hereditary monopoly now serves primarily to maintain power and privilege, with corroboration from historical sociological studies and contemporary human rights reports.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__hereditary_monopoly_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__hereditary_monopoly_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__hereditary_monopoly_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vedic_dharmic_corpus__hereditary_monopoly_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__hereditary_monopoly_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vedic_dharmic_corpus__hereditary_monopoly_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vedic_dharmic_corpus__hereditary_monopoly_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it provides a coordination function (stable ritual practice, textual interpretation) but couples it with asymmetric extraction (exclusive benefits for Brahmins, exclusion and costs for others). Extractiveness is high (0.68) due to the concentration of spiritual and social capital. Suppression (0.75) is also high, maintained through social ostracism, denial of access, and the suppression of alternative interpretations. The theater ratio (0.20) is relatively low, as the ritual functions are genuinely performed, but a portion of the maintenance effort is directed at defending the exclusive access rather than the ritual itself.
 *
 * PERSPECTIVAL GAP:
 *   The Brahmin priestly class experiences this as a legitimate, divinely sanctioned system for maintaining dharma and cosmic order (a form of Rope or even Mountain). Lower castes and women experience it as a Snare, a system of enforced exclusion and extraction. The engine's classification as Tangled Rope reflects this hybrid nature, acknowledging the coordination function while highlighting the asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The Brahmin priestly class is the primary beneficiary and agenda-setter (d=0.0-0.1), directly profiting from the monopoly. Lower castes, women, and non-Brahmin devotees are targets (d=0.8-1.0), bearing the costs of exclusion and dependence. Bhakti movement adherents and reformist legal scholars are excluded or analytical observers, challenging the system from outside.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preserving ritual purity and cosmic order) is contested. While the ritual function persists, its justification has shifted from universal benefit to maintaining a hereditary power structure. The classification as Tangled Rope prevents mislabeling it as a pure Rope (ignoring extraction) or a pure Snare (ignoring the genuine, albeit captured, coordination function). The persistence of the constraint is due to the concentrated benefits for the Brahmin class and the diffuse, internalized costs for others, making collective action to dismantle it difficult.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hereditary_vs_merit_authority,
    'Is ritual and interpretive authority inherently tied to birth (hereditary) or can it be acquired through merit, training, or devotion, regardless of lineage?',
    'Empirical observation of successful, widely accepted ritual performances and scriptural interpretations by non-Brahmins, or formal theological re-interpretations by influential religious bodies.',
    'If merit-based authority gains widespread acceptance, the constraint''s extractiveness and suppression would decrease, potentially reclassifying it towards a Rope or even a Scaffold (if transitional). If hereditary claims are reaffirmed, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hereditary_vs_merit_authority, empirical, 'Ambiguity regarding the basis of legitimate religious authority.').

omega_variable(
    divine_mandate_vs_social_construct,
    'Is the varna hierarchy a divinely ordained, immutable structure, or a historical social construct maintained through human agency?',
    'Consensus among independent theological scholars and historical sociologists, or a definitive, widely accepted re-interpretation of foundational texts that explicitly refutes divine ordination.',
    'If proven a social construct, the moral legitimacy of the constraint collapses, increasing resistance and potentially reclassifying it as a Snare. If divine mandate is reaffirmed, the constraint''s perceived legitimacy (from the agenda-setter''s seat) remains high.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_mandate_vs_social_construct, conceptual, 'Ambiguity regarding the origin and immutability of social hierarchy.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the Vedic-Dharmic corpus, or is it merely a historical practice that has diverged from the core kernel?',
    'Analysis of textual interpretation and historical theological debates: if this reading can be shown to be a coherent, albeit selective, interpretation of the kernel, it remains a distinct reading. If it''s a clear departure, it becomes a practice-drift scenario.',
    'If a distinct reading, the contest is over interpretation. If a drift, the contest is over fidelity to the kernel, potentially leading to a reclassification of the kernel itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is the ''hereditary_monopoly_reading'' of the ''vedic_dharmic_corpus'' kernel. Sibling readings include ''bhakti_devotional_reading'' and ''reformist_egalitarian_reading''. This reading differs from siblings on the source of authority and the nature of hierarchy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__hereditary_monopoly_reading, 1000, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t1000, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(vedi_tr_t1500, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 1500, 0.15).
narrative_ontology:measurement(vedi_tr_t1800, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 1800, 0.2).
narrative_ontology:measurement(vedi_tr_t1900, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 1900, 0.25).
narrative_ontology:measurement(vedi_tr_t2000, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(vedi_tr_t2024, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(vedi_be_t1000, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 1000, 0.6).
narrative_ontology:measurement(vedi_be_t1500, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 1500, 0.7).
narrative_ontology:measurement(vedi_be_t1800, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 1800, 0.75).
narrative_ontology:measurement(vedi_be_t1900, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 1900, 0.72).
narrative_ontology:measurement(vedi_be_t2000, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(vedi_be_t2024, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t1000, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 1000, 0.7).
narrative_ontology:measurement(vedi_su_t1500, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 1500, 0.8).
narrative_ontology:measurement(vedi_su_t1800, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 1800, 0.85).
narrative_ontology:measurement(vedi_su_t1900, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 1900, 0.8).
narrative_ontology:measurement(vedi_su_t2000, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(vedi_su_t2024, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__hereditary_monopoly_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'vedic_dharmic_corpus' kernel. The 'hereditary_monopoly_reading' emphasizes birthright and divine ordination of hierarchy, leading to high extraction. The 'bhakti_devotional_reading' emphasizes direct devotional access, bypassing caste. The 'reformist_egalitarian_reading' interprets texts through constitutional equality. Each reading constitutes a distinct constraint with different epsilon values and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
