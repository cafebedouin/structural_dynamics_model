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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: vedic_dharmic_corpus__hereditary_monopoly_reading
 *   human_readable: Hereditary Brahminical Authority over Vedic Dharma
 *   domain: religious/social_interpretive
 *
 * SUMMARY:
 *   This constraint describes the 'hereditary_monopoly_reading' of the
 *   'vedic_dharmic_corpus' kernel. It asserts that ritual and interpretive
 *   authority are derived exclusively from birth into Brahmin lineage, and
 *   that the varna (caste) hierarchy is divinely ordained and textually
 *   prescribed. This reading functions as a Tangled Rope, providing a
 *   framework for social and ritual order while simultaneously enforcing
 *   significant asymmetric extraction and suppression, primarily benefiting
 *   the Brahmin priestly class at the expense of lower castes, women, and
 *   non-Brahmin scholars.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.68).
domain_priors:suppression_score(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.75).
domain_priors:theater_ratio(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__hereditary_monopoly_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__hereditary_monopoly_reading, "Hereditary Brahminical Authority over Vedic Dharma").
narrative_ontology:topic_domain(vedic_dharmic_corpus__hereditary_monopoly_reading, "religious/social_interpretive").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__hereditary_monopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__hereditary_monopoly_reading, 'db66d242-a07c-4d3a-abe5-5a4228400068').
narrative_ontology:cs_kernel_codification('db66d242-a07c-4d3a-abe5-5a4228400068', fixed_text).
narrative_ontology:cs_authority_grounding('db66d242-a07c-4d3a-abe5-5a4228400068', lineage).
narrative_ontology:cs_interpretation_layer_present('db66d242-a07c-4d3a-abe5-5a4228400068').
narrative_ontology:cs_reading_relation('db66d242-a07c-4d3a-abe5-5a4228400068', vedic_dharmic_corpus__bhakti_devotional_reading, coexists_with).
narrative_ontology:cs_reading_relation('db66d242-a07c-4d3a-abe5-5a4228400068', vedic_dharmic_corpus__reformist_egalitarian_reading, forecloses).
narrative_ontology:cs_axiom('db66d242-a07c-4d3a-abe5-5a4228400068', foundational, varna_by_birth_divinely_ordained).
narrative_ontology:cs_axiom_status(varna_by_birth_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('db66d242-a07c-4d3a-abe5-5a4228400068', varna_by_birth_divinely_ordained, theological).
narrative_ontology:cs_axiom('db66d242-a07c-4d3a-abe5-5a4228400068', foundational, brahmin_exclusive_ritual_authority).
narrative_ontology:cs_axiom_status(brahmin_exclusive_ritual_authority, holdable).
narrative_ontology:cs_axiom_grounding('db66d242-a07c-4d3a-abe5-5a4228400068', brahmin_exclusive_ritual_authority, conventional).
narrative_ontology:cs_reference_frame('db66d242-a07c-4d3a-abe5-5a4228400068', ancient_vedic_social_order).
narrative_ontology:cs_drift_state('db66d242-a07c-4d3a-abe5-5a4228400068', contemporary_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('db66d242-a07c-4d3a-abe5-5a4228400068', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, lower_castes).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, women).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, non_brahmin_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds exclusive rights to perform certain rituals, interpret sacred texts, and mediate spiritual knowledge, deriving social status, patronage, and economic benefits from this position. Actively enforces ritual purity rules and caste distinctions.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class, agenda_setter,
    institutional, generational, constrained, national).

% Are ritually excluded from certain sacred spaces and practices, denied access to scriptural knowledge, and subjected to social discrimination based on their birth. Bear the social and economic costs of a divinely ordained hierarchy.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, lower_castes, payer,
    powerless, generational, trapped, local).

% Are generally excluded from priestly roles and direct Vedic study, with their spiritual path often mediated through male relatives or devotional practices. Their identity and social role are often defined within the framework of this hereditary authority.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, women, payer,
    powerless, generational, identity_locked, local).

% Despite intellectual merit, are often denied full recognition or authority in traditional Vedic interpretation and ritual leadership due to their birth. They may pursue alternative scholarly paths or challenge the monopoly, but face institutional barriers.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, non_brahmin_scholars, payer,
    moderate, biographical, constrained, national).

% Seek direct devotional access to the divine, often bypassing traditional ritual and caste hierarchies. While their practices offer an alternative, they are often marginalized or seen as less authoritative by the hereditary priestly class.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, bhakti_devotees, excluded,
    moderate, biographical, mobile, regional).

% Actively challenge the hereditary monopoly and caste hierarchy on ethical, social, and constitutional grounds. They advocate for egalitarian interpretations and practices, but are often dismissed as external or non-traditional by proponents of this reading.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, reformist_activists, excluded,
    organized, generational, mobile, national).

% Operate under a constitutional framework that formally prohibits caste discrimination, often conflicting with the social practices upheld by this constraint. They observe, legislate, and sometimes intervene, but face challenges in altering deeply embedded religious and social norms.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, secular_state_institutions, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class).
narrative_ontology:fixing_cost_class(vedic_dharmic_corpus__hereditary_monopoly_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, authoritative framework for ritual practice, scriptural interpretation, and social order, ensuring the continuity of Vedic traditions and a perceived cosmic balance through the designated priestly class.
% TRANSFER_FUNCTION: Transfers ritual authority, social status, and economic benefits (e.g., donations, patronage, exclusive access to sacred knowledge) to the Brahmin priestly class, while imposing ritual exclusion, social subordination, and limited access to spiritual agency on lower castes and women.
% ABSENT_VOICES: Bhakti devotees (who emphasize direct devotion over ritual hierarchy), reformist egalitarians (who challenge caste on ethical and constitutional grounds), and those historically excluded from scriptural access and priestly roles would object to the exclusivity and extraction inherent in this system.
% DISAPPEARANCE_RATIONALE: If the hereditary monopoly on ritual and interpretive authority vanished overnight, the entire social and ritual structure built around it would need to be re-established or replaced. The traditional ritual economy would collapse, social hierarchies would be destabilized, and new forms of religious leadership and interpretation would emerge, leading to significant upheaval in religious and social life.
% FOUNDING_PROBLEM: To establish a stable, divinely sanctioned social and ritual order, ensuring the correct performance of Vedic rites for cosmic well-being, maintaining social harmony through clear roles, and preserving sacred knowledge through a dedicated lineage.
% FOUNDING_PROBLEM_CORROBORATION: Proponents within the Brahminical tradition attest that the founding problem of maintaining cosmic order and preserving dharma is still live and requires their hereditary role. Critics (e.g., reformist movements, secular scholars, lower-caste activists) argue that the original problem is either solved, was a justification for power, or is superseded by modern ethical and constitutional principles; independent historical and sociological analyses from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__hereditary_monopoly_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__hereditary_monopoly_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__hereditary_monopoly_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(vedic_dharmic_corpus__hereditary_monopoly_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.68) due to the exclusive control over spiritual capital, ritual services, and social status, which translates into economic and social benefits for the Brahmin class. Suppression is also high (0.75) as the system relies on deeply embedded social norms, religious injunctions, and historical enforcement to maintain caste distinctions and restrict access to knowledge and ritual roles. Accessibility collapse is very high (0.80) because alternatives to this birth-based authority are structurally and culturally difficult to access for those born outside the Brahmin lineage. Resistance is moderate-high (0.70), reflecting ongoing challenges from reform movements and secular law, which are met with strong traditional counter-arguments and social inertia. Theater ratio is moderate (0.40): while genuine ritual and interpretive functions exist, a significant portion of the activity serves to performatively reinforce the legitimacy of the hereditary monopoly itself.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Brahmin priestly class, this constraint is a divinely ordained, necessary structure for cosmic and social harmony, a Rope or even a Mountain. From the perspective of lower castes and women, it is a Snare, enforcing systemic discrimination and extraction. The engine's computation, based on the authored metrics and structural data, will reveal this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Brahmin priestly class is the primary beneficiary and agenda-setter, directly profiting from and enforcing the constraint. Lower castes, women, and non-Brahmin scholars are the primary targets, bearing the costs of exclusion, discrimination, and limited agency. Bhakti devotees and reformist activists are largely excluded from the formal system but represent alternative paths or active resistance, experiencing the constraint as a barrier to their preferred spiritual or social order. Secular state institutions act as external observers, attempting to mitigate the constraint's extractive aspects through legal means.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_ordination_vs_social_construction,
    'Is the varna hierarchy genuinely divinely ordained and textually prescribed, or is it a social construct that has been reified through religious texts and interpretive traditions?',
    'Comparative theological and historical analysis of early Vedic texts versus later Dharmashastras, alongside sociological studies of caste formation and evolution. Resolution would depend on the interpretive framework adopted.',
    'If primarily a social construct, the constraint''s ''naturalness'' claim collapses, reclassifying it more firmly as an extractive construct (Snare or Tangled Rope) rather than a divinely sanctioned order. If genuinely divine, its legitimacy claim is strengthened, though its extractiveness remains.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_ordination_vs_social_construction, conceptual, 'Ambiguity of divine vs. social origin of caste hierarchy.').

omega_variable(
    ritual_efficacy_vs_power_maintenance,
    'To what extent does the Brahminical monopoly on ritual genuinely ensure cosmic well-being and spiritual efficacy, versus primarily serving to maintain the social and economic power of the priestly class?',
    'Empirical studies of social outcomes in communities adhering to or departing from this ritual structure, alongside theological debates on the nature of ritual efficacy and its dependence on lineage. This is a complex, multi-faceted question.',
    'If ritual efficacy is largely independent of hereditary lineage, the justification for the monopoly weakens, exposing its extractive function more clearly. If efficacy is strongly tied to lineage, the coordination function is strengthened, potentially rebalancing the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_efficacy_vs_power_maintenance, empirical, 'Ambiguity of ritual function vs. power maintenance.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.75) primarily structural (e.g., legal exclusion, economic dependency) or internalized (e.g., belief in karma, social conditioning, identity fusion with caste role)?',
    'Post-exit suppression trajectory: if individuals from lower castes continue to self-limit or face internal barriers to participation even after structural barriers are removed (e.g., through legal reforms), it suggests a significant internalized component. Longitudinal studies of social mobility and identity formation.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them after exit, making the constraint more resilient to external challenges. If purely structural, legal and social reforms would be more immediately effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in caste hierarchy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__hereditary_monopoly_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(vedi_tr_t200, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 200, 0.36).
narrative_ontology:measurement(vedi_tr_t400, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 400, 0.37).
narrative_ontology:measurement(vedi_tr_t800, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 800, 0.38).
narrative_ontology:measurement(vedi_tr_t1200, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 1200, 0.39).
narrative_ontology:measurement(vedi_tr_t1600, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 1600, 0.4).
narrative_ontology:measurement(vedi_tr_t2000, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 2000, 0.4).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(vedi_be_t200, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 200, 0.58).
narrative_ontology:measurement(vedi_be_t400, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 400, 0.6).
narrative_ontology:measurement(vedi_be_t800, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 800, 0.63).
narrative_ontology:measurement(vedi_be_t1200, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 1200, 0.65).
narrative_ontology:measurement(vedi_be_t1600, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 1600, 0.67).
narrative_ontology:measurement(vedi_be_t2000, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 2000, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(vedi_su_t200, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 200, 0.63).
narrative_ontology:measurement(vedi_su_t400, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 400, 0.66).
narrative_ontology:measurement(vedi_su_t800, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 800, 0.69).
narrative_ontology:measurement(vedi_su_t1200, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 1200, 0.71).
narrative_ontology:measurement(vedi_su_t1600, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 1600, 0.73).
narrative_ontology:measurement(vedi_su_t2000, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 2000, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__hereditary_monopoly_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus__bhakti_devotional_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus__reformist_egalitarian_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, temple_governance_structures).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, caste_based_social_discrimination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'vedic_dharmic_corpus' kernel, focusing on hereditary authority. Its structural properties differ significantly from the bhakti and reformist readings, necessitating separate constraint stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
