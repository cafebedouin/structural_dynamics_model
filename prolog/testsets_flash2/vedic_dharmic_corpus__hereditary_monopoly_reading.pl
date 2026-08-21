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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vedic_dharmic_corpus__hereditary_monopoly_reading
 *   human_readable: Vedic Dharmic Corpus: Hereditary Monopoly Reading
 *   domain: religious/social_stratification
 *
 * SUMMARY:
 *   This constraint represents the 'hereditary monopoly' reading of the Vedic
 *   Dharmic Corpus, where ritual and interpretive authority are exclusively
 *   derived from birth into Brahmin lineage, and the varna hierarchy is
 *   considered divinely ordained. This reading is one of several competing
 *   interpretations of the same foundational texts. The metrics reflect the
 *   high extractiveness and suppression inherent in maintaining this
 *   exclusive authority, despite increasing resistance from alternative
 *   readings and social movements. The claimed type 'tangled_rope'
 *   acknowledges a coordination function (maintaining ritual order) alongside
 *   significant asymmetric extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.65).
domain_priors:suppression_score(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.78).
domain_priors:theater_ratio(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__hereditary_monopoly_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__hereditary_monopoly_reading, "Vedic Dharmic Corpus: Hereditary Monopoly Reading").
narrative_ontology:topic_domain(vedic_dharmic_corpus__hereditary_monopoly_reading, "religious/social_stratification").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__hereditary_monopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__hereditary_monopoly_reading, '1b31a4f7-2323-4022-9198-291c3b4308e3').
narrative_ontology:cs_kernel_codification('1b31a4f7-2323-4022-9198-291c3b4308e3', fixed_text).
narrative_ontology:cs_authority_grounding('1b31a4f7-2323-4022-9198-291c3b4308e3', lineage).
narrative_ontology:cs_interpretation_layer_present('1b31a4f7-2323-4022-9198-291c3b4308e3').
narrative_ontology:cs_reading_relation('1b31a4f7-2323-4022-9198-291c3b4308e3', vedic_dharmic_corpus__bhakti_devotional_reading, coexists_with).
narrative_ontology:cs_reading_relation('1b31a4f7-2323-4022-9198-291c3b4308e3', vedic_dharmic_corpus__reformist_egalitarian_reading, coexists_with).
narrative_ontology:cs_axiom('1b31a4f7-2323-4022-9198-291c3b4308e3', foundational, ritual_authority_by_birth).
narrative_ontology:cs_axiom_status(ritual_authority_by_birth, holdable).
narrative_ontology:cs_axiom_grounding('1b31a4f7-2323-4022-9198-291c3b4308e3', ritual_authority_by_birth, conventional).
narrative_ontology:cs_axiom('1b31a4f7-2323-4022-9198-291c3b4308e3', foundational, varna_divinely_ordained).
narrative_ontology:cs_axiom_status(varna_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('1b31a4f7-2323-4022-9198-291c3b4308e3', varna_divinely_ordained, theological).
narrative_ontology:cs_reference_frame('1b31a4f7-2323-4022-9198-291c3b4308e3', traditional_brahminical_orthodoxy).
narrative_ontology:cs_drift_state('1b31a4f7-2323-4022-9198-291c3b4308e3', contemporary_post_independence_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('1b31a4f7-2323-4022-9198-291c3b4308e3', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, lower_castes).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, women_in_ritual_contexts).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, non_brahmin_scholars).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__hereditary_monopoly_reading, divine_origin_of_varna).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__hereditary_monopoly_reading, textual_infallibility_of_vedas).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds exclusive rights to perform certain rituals, interpret sacred texts, and officiate religious ceremonies based on birth. Benefits from offerings and social status derived from this monopoly. Actively enforces adherence to traditional interpretations.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class, agenda_setter,
    institutional, generational, arbitrage, regional).

% Excluded from direct participation in many rituals and denied access to interpretive authority. Must rely on Brahmin priests for spiritual services, incurring social and economic costs. Identity is often deeply intertwined with the varna system, making exit difficult.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, lower_castes, payer,
    powerless, generational, identity_locked, local).

% Restricted from performing certain rituals or holding positions of spiritual authority, regardless of caste. Their spiritual access is mediated through male family members or Brahmin priests. Cultural and social identity makes direct challenge or exit highly constrained.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, women_in_ritual_contexts, payer,
    powerless, biographical, identity_locked, local).

% May possess deep knowledge of sacred texts but are denied formal interpretive authority or recognition within traditional institutions due to birth. Their scholarship is often marginalized or dismissed by the Brahmin establishment, limiting career and influence.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, non_brahmin_scholars, payer,
    moderate, biographical, constrained, regional).

% Seek direct, unmediated spiritual connection through devotion, often bypassing traditional ritual and priestly authority. While not directly paying the Brahmin class, their alternative spiritual path is often viewed as heterodox or illegitimate by the hereditary monopoly reading.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, bhakti_devotees, excluded,
    organized, biographical, mobile, regional).

% Advocate for an egalitarian interpretation of religious texts and constitutional principles, challenging the legitimacy of caste-based ritual authority. Their efforts are actively resisted by the Brahmin priestly class, but they operate within a broader legal and social framework.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, reformist_activists, excluded,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a stable, divinely sanctioned social and ritual order by assigning specific roles and duties (dharma) based on birth, ensuring the continuity of Vedic traditions and the performance of complex rituals.
% TRANSFER_FUNCTION: Transfers ritual authority, interpretive legitimacy, social status, and economic resources (offerings, patronage) from lower castes, women, and non-Brahmin scholars to the Brahmin priestly class, in exchange for spiritual services and maintenance of cosmic order.
% ABSENT_VOICES: Historically, the voices of lower castes and women were structurally excluded from religious discourse and interpretive authority. Today, while legally protected, their perspectives are still marginalized within traditional institutions that uphold this reading. Bhakti and reformist movements represent these excluded voices.
% DISAPPEARANCE_RATIONALE: If the hereditary monopoly on ritual and interpretive authority vanished overnight, the entire social and religious structure would undergo profound reorganization. Lower castes and women would gain direct access to spiritual practices, the ritual economy would collapse for the Brahmin class, and new forms of spiritual leadership would emerge, fundamentally altering the fabric of society.
% FOUNDING_PROBLEM: To establish and maintain a stable, divinely ordained social order (varna system) and ensure the correct performance of complex Vedic rituals necessary for cosmic harmony, by assigning specific roles and duties based on birth.
% FOUNDING_PROBLEM_CORROBORATION: The Brahmin priestly class and traditionalists attest the problem is live, emphasizing the need for ritual purity and textual fidelity. Reformist scholars and social activists, citing constitutional equality and historical evidence, attest the founding problem is a justification for social hierarchy and that the arrangement persists as a mechanism of extraction; their corroboration comes from legal challenges, sociological studies, and alternative theological interpretations.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__hereditary_monopoly_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__hereditary_monopoly_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__hereditary_monopoly_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vedic_dharmic_corpus__hereditary_monopoly_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.65) due to the exclusive control over spiritual services and the associated economic and social benefits for the Brahmin class. Suppression (0.78) is also high, as the system relies on social ostracism, denial of access, and historical enforcement to maintain the hierarchy and prevent challenges to interpretive authority. Theater ratio is moderate (0.20), indicating that while genuine ritual functions exist, a portion of the activity serves to reinforce the hereditary claim rather than purely spiritual ends. Accessibility collapse is high (0.70) because alternatives for direct spiritual access or interpretive authority are severely limited within this framework. Resistance is moderate (0.45) due to ongoing challenges from reformist movements and alternative devotional paths.
 *
 * PERSPECTIVAL GAP:
 *   The Brahmin priestly class experiences this as a legitimate, divinely ordained system for maintaining cosmic order and tradition. The victim groups experience it as an extractive and suppressive hierarchy that denies them agency and spiritual access. The engine's classification will highlight this divergence, showing a 'tangled_rope' for the system as a whole, but potentially a 'snare' from the perspective of the most disempowered seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The Brahmin priestly class is the primary beneficiary and agenda-setter, deriving power and resources from this reading. Lower castes, women, and non-Brahmin scholars are victims, bearing the costs of exclusion and mediated access. Bhakti devotees and reformist activists are excluded, representing alternative readings that challenge the constraint's legitimacy. The 'identity_locked' exit option for lower castes and women reflects the deep cultural and social integration of the varna system, making individual exit from its strictures extremely difficult.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to maintain cosmic harmony through ritual is still claimed as live by its beneficiaries. However, the 'contested' status of the founding problem and the high extractiveness suggest that the original coordination function has been significantly overlaid by rent-seeking. The classification as a tangled_rope, rather than a pure rope, prevents mislabeling the coordination as purely beneficial when it clearly involves asymmetric extraction and suppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_vs_social_construct,
    'Is the varna hierarchy a divinely ordained, immutable structure as claimed by this reading, or a historically contingent social construct maintained for the benefit of a specific class?',
    'Comparative historical and sociological analysis of other stratified societies, and theological re-interpretations that challenge the divine origin claim from within the tradition.',
    'If a social construct, the ''emerges_naturally'' claim would be false, reclassifying the constraint from a ''tangled_rope'' towards a ''snare'' or ''piton'' depending on the degree of active enforcement vs. inertia.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_mandate_vs_social_construct, conceptual, 'Ambiguity between divine mandate and social construction of hierarchy.').

omega_variable(
    interpretive_authority_legitimacy,
    'Is interpretive authority legitimately derived solely from birth, or can it be earned through scholarship, spiritual realization, or democratic consensus?',
    'Acceptance of alternative interpretive traditions within the broader Dharmic discourse, and the establishment of non-hereditary institutions for textual scholarship and ritual leadership.',
    'If authority can be earned, the ''brahmin_priestly_class'' would lose its exclusive ''agenda_setter'' role, significantly reducing extractiveness and suppression, potentially shifting the constraint towards a ''rope'' or even dissolving it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_legitimacy, preference, 'Legitimacy of hereditary interpretive authority.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (e.g., institutional exclusion, economic dependency) or internalized (e.g., belief in one''s own lower status, fear of social ostracism)?',
    'Post-exit suppression trajectory: if individuals from lower castes or women continue to self-limit their spiritual access or interpretive claims even after external barriers are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the constraint more resilient to external challenges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the varna system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__hereditary_monopoly_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vedi_tr_t10, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(vedi_tr_t20, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(vedi_tr_t30, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(vedi_tr_t40, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(vedi_tr_t50, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(vedi_be_t10, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(vedi_be_t20, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(vedi_be_t30, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(vedi_be_t40, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(vedi_be_t50, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(vedi_su_t10, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 10, 0.82).
narrative_ontology:measurement(vedi_su_t20, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 20, 0.79).
narrative_ontology:measurement(vedi_su_t30, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(vedi_su_t40, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 40, 0.78).
narrative_ontology:measurement(vedi_su_t50, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__hereditary_monopoly_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus__bhakti_devotional_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus__reformist_egalitarian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Vedic Dharmic Corpus kernel. Its structural claims about hereditary authority and varna hierarchy are distinct from, and in tension with, the bhakti devotional and reformist egalitarian readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
