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
 *   human_readable: Hereditary Brahminical Authority over Vedic-Dharmic Interpretation
 *   domain: religious/social_stratification/interpretive_legitimacy
 *
 * SUMMARY:
 *   This constraint describes the reading of the Vedic-Dharmic corpus that
 *   asserts ritual and interpretive authority are derived from birth into
 *   Brahmin lineage, and that the varna (caste) hierarchy is divinely
 *   ordained and textually prescribed. It functions as a Tangled Rope,
 *   providing a framework for social and ritual order (coordination) while
 *   simultaneously enforcing a highly asymmetric distribution of power,
 *   status, and resources (extraction). The constraint is actively enforced
 *   through social norms, temple practices, and traditional educational
 *   systems, suppressing alternatives and resistance from lower castes and
 *   reformist movements.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.65).
domain_priors:suppression_score(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.75).
domain_priors:theater_ratio(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__hereditary_monopoly_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__hereditary_monopoly_reading, "Hereditary Brahminical Authority over Vedic-Dharmic Interpretation").
narrative_ontology:topic_domain(vedic_dharmic_corpus__hereditary_monopoly_reading, "religious/social_stratification/interpretive_legitimacy").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__hereditary_monopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__hereditary_monopoly_reading, '077f154e-2aa5-4707-a05b-f761efc282b2').
narrative_ontology:cs_kernel_codification('077f154e-2aa5-4707-a05b-f761efc282b2', fixed_text).
narrative_ontology:cs_authority_grounding('077f154e-2aa5-4707-a05b-f761efc282b2', lineage).
narrative_ontology:cs_interpretation_layer_present('077f154e-2aa5-4707-a05b-f761efc282b2').
narrative_ontology:cs_reading_relation('077f154e-2aa5-4707-a05b-f761efc282b2', vedic_dharmic_corpus__bhakti_devotional_reading, forecloses).
narrative_ontology:cs_reading_relation('077f154e-2aa5-4707-a05b-f761efc282b2', vedic_dharmic_corpus__reformist_egalitarian_reading, forecloses).
narrative_ontology:cs_axiom('077f154e-2aa5-4707-a05b-f761efc282b2', foundational, varna_hierarchy_divinely_ordained).
narrative_ontology:cs_axiom_status(varna_hierarchy_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('077f154e-2aa5-4707-a05b-f761efc282b2', varna_hierarchy_divinely_ordained, theological).
narrative_ontology:cs_axiom('077f154e-2aa5-4707-a05b-f761efc282b2', foundational, brahmin_birth_confers_authority).
narrative_ontology:cs_axiom_status(brahmin_birth_confers_authority, holdable).
narrative_ontology:cs_axiom_grounding('077f154e-2aa5-4707-a05b-f761efc282b2', brahmin_birth_confers_authority, conventional).
narrative_ontology:cs_reference_frame('077f154e-2aa5-4707-a05b-f761efc282b2', ancient_vedic_social_order).
narrative_ontology:cs_drift_state('077f154e-2aa5-4707-a05b-f761efc282b2', contemporary_secular_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('077f154e-2aa5-4707-a05b-f761efc282b2', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, lower_castes).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, women).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, dalits).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds exclusive rights to perform certain rituals, interpret sacred texts, and receive patronage, based on birth. They administer temples and educational institutions that perpetuate this authority. Benefits directly from the social and economic structure maintained by this constraint.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class, agenda_setter,
    institutional, generational, arbitrage, regional).

% Are denied access to higher ritual roles and interpretive authority, often relegated to subordinate social and economic positions. They bear the social stigma and economic disadvantage of the varna hierarchy, with limited avenues for upward mobility within the traditional system.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, lower_castes, payer,
    powerless, generational, trapped, local).

% Are largely excluded from priestly roles and formal scriptural study, regardless of caste, within this traditional reading. Their spiritual and social roles are often defined in relation to male family members, limiting their autonomy and direct access to religious authority.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, women, payer,
    powerless, generational, identity_locked, local).

% Are at the lowest rung of the social hierarchy, historically subjected to severe discrimination and exclusion from all forms of ritual and social participation. They face the most extreme forms of extraction and suppression under this constraint.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, dalits, payer,
    powerless, generational, trapped, local).

% Enforce the traditional rules of ritual purity and access, often upholding the hereditary monopoly of Brahmins. They manage temple resources and patronage, which reinforces the existing hierarchy.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, temple_authorities, agenda_setter,
    organized, biographical, constrained, local).

% Operate outside the religious framework but increasingly challenge caste-based discrimination through constitutional and anti-discrimination laws. They observe and sometimes intervene in the social effects of this constraint, but do not directly adjudicate religious doctrine.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, secular_legal_systems, observer,
    institutional, generational, analytical, national).

% Seek direct devotional access to the divine, often bypassing traditional caste-based ritual intermediaries. While they may not directly challenge the Brahminical monopoly, their practice offers an alternative path to spiritual authority that undermines its exclusivity.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, bhakti_devotees, excluded,
    moderate, biographical, constrained, regional).

% Actively challenge the caste system and hereditary authority, advocating for social equality and inclusive religious practices. They operate through social movements, legal challenges, and reinterpretations of sacred texts, but are excluded from the traditional interpretive authority structure.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, reformist_activists, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class).
narrative_ontology:fixing_cost_class(vedic_dharmic_corpus__hereditary_monopoly_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable social and ritual order by defining roles, responsibilities, and access to sacred knowledge and practices based on birth, believed to maintain cosmic balance and social harmony.
% TRANSFER_FUNCTION: Transfers ritual authority, social status, and economic benefits (e.g., donations, patronage, educational access) from lower castes, women, and the general populace to the Brahmin priestly class, in exchange for performing rituals and interpreting sacred texts.
% ABSENT_VOICES: Lower castes, women, and Dalits are structurally excluded from interpretive authority and would object to the divine ordination of hierarchy, demanding equal access to ritual and spiritual leadership. Bhakti devotees and reformist activists also represent excluded voices who offer alternative framings.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight, the entire social and religious structure of many Hindu communities would undergo a profound transformation. Ritual practices, social status, and economic flows tied to caste would collapse, leading to a reordering of religious leadership and social power, and a re-evaluation of scriptural interpretation.
% FOUNDING_PROBLEM: To establish a stable, divinely sanctioned social order and ensure the correct performance of complex Vedic rituals, believed to maintain cosmic balance and social harmony, by assigning specific roles and duties based on birth.
% FOUNDING_PROBLEM_CORROBORATION: Traditional Brahminical institutions and their adherents attest the problem of maintaining cosmic and social order is still live and the hereditary system is essential. Reformist movements, secular scholars, and lower-caste activists argue the founding problem has been superseded by modern ethical and constitutional principles, and the arrangement now primarily serves to maintain social inequality and privilege; legislative-hearing testimony and independent sociological analysis from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__hereditary_monopoly_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__hereditary_monopoly_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__hereditary_monopoly_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extractiveness is high (0.65) due to the significant social, economic, and spiritual advantages conferred exclusively by birth. Suppression is also high (0.75) as the system relies on deeply embedded social conditioning, historical violence, and institutional exclusion to maintain its structure against challenges. Theater ratio is moderate (0.45): while ritual performance is a genuine function, the justification for *exclusive* hereditary authority is increasingly performative in the face of modern ethical and legal challenges. Accessibility collapse is very high (0.80) for non-Brahmins to gain ritual authority, and resistance is substantial (0.60) from various reform movements.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Brahmin priestly class, this constraint is a divinely ordained, essential framework for maintaining cosmic and social order. From the perspective of lower castes, women, and Dalits, it is a system of profound injustice and extraction. The engine's classification as a Tangled Rope captures this dual nature, where a claimed coordination function masks significant asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The Brahmin priestly class is the primary beneficiary and agenda-setter, directly collecting the gains of this system. Lower castes, women, and Dalits are the primary targets and victims, bearing the costs of exclusion and discrimination. Temple authorities act as agenda-setters, enforcing the rules. Secular legal systems and reformist activists are observers or excluded parties, challenging the constraint from outside its internal logic.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_ordination_vs_social_construct,
    'Is the varna hierarchy a divinely ordained, immutable structure, or a historically contingent social construct maintained for the benefit of a specific class?',
    'Comparative historical and sociological analysis of other stratified societies, textual criticism examining the evolution of interpretive traditions, and empirical studies of the social and economic impacts of caste-based discrimination.',
    'If primarily a social construct, the constraint''s ''emerges_naturally'' claim is false, strengthening its classification as an extractive construct (Snare or Tangled Rope). If genuinely divine, its naturalness claim would be stronger, though still subject to FSM if beneficiaries exist.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_ordination_vs_social_construct, conceptual, 'Ambiguity between divine mandate and social construction of hierarchy.').

omega_variable(
    interpretive_authority_legitimacy,
    'Does interpretive authority genuinely derive from hereditary lineage, or is it a claim used to monopolize access to sacred knowledge and its associated benefits?',
    'Analysis of the intellectual and spiritual contributions of non-Brahmin scholars and saints, and the impact of open access to scriptural education on religious understanding and practice.',
    'If hereditary lineage is not a necessary condition for valid interpretation, the constraint''s justification for exclusive Brahminical authority weakens, further exposing its extractive nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_legitimacy, empirical, 'Legitimacy of hereditary interpretive authority.').

omega_variable(
    kernel_contest_bhakti_devotional_reading,
    'How would the structural properties of this ''hereditary_monopoly_reading'' change if the ''bhakti_devotional_reading'' were to gain dominant institutional recognition?',
    'Observing the institutionalization of bhakti-based spiritual leadership and its impact on traditional temple structures and patronage networks in regions where bhakti movements are strong.',
    'If the bhakti reading became dominant, the extractiveness and suppression of the hereditary monopoly reading would likely decrease significantly, as direct devotional access would bypass the need for hereditary intermediaries, potentially reclassifying it towards a Piton or even Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_bhakti_devotional_reading, empirical, 'Impact of bhakti devotional reading on hereditary monopoly.').

omega_variable(
    kernel_contest_reformist_egalitarian_reading,
    'How would the structural properties of this ''hereditary_monopoly_reading'' change if the ''reformist_egalitarian_reading'' were to gain dominant institutional recognition?',
    'Analyzing the effects of constitutional reforms and anti-discrimination laws on traditional religious institutions and practices, particularly regarding temple entry and access to priestly roles for all castes and genders.',
    'If the reformist egalitarian reading became dominant, the hereditary monopoly reading would face severe legal and social repudiation, leading to a drastic reduction in its extractiveness and suppression, likely reclassifying it as a Piton or even a historical artifact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_reformist_egalitarian_reading, empirical, 'Impact of reformist egalitarian reading on hereditary monopoly.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__hereditary_monopoly_reading, 1000, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t1000, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 1000, 0.3).
narrative_ontology:measurement(vedi_tr_t1200, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 1200, 0.35).
narrative_ontology:measurement(vedi_tr_t1400, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 1400, 0.4).
narrative_ontology:measurement(vedi_tr_t1600, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 1600, 0.42).
narrative_ontology:measurement(vedi_tr_t1800, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 1800, 0.44).
narrative_ontology:measurement(vedi_tr_t2020, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 2020, 0.45).

% Extraction over time
narrative_ontology:measurement(vedi_be_t1000, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 1000, 0.55).
narrative_ontology:measurement(vedi_be_t1200, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 1200, 0.58).
narrative_ontology:measurement(vedi_be_t1400, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 1400, 0.6).
narrative_ontology:measurement(vedi_be_t1600, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 1600, 0.62).
narrative_ontology:measurement(vedi_be_t1800, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 1800, 0.63).
narrative_ontology:measurement(vedi_be_t2020, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t1000, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 1000, 0.7).
narrative_ontology:measurement(vedi_su_t1200, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 1200, 0.72).
narrative_ontology:measurement(vedi_su_t1400, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 1400, 0.73).
narrative_ontology:measurement(vedi_su_t1600, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 1600, 0.74).
narrative_ontology:measurement(vedi_su_t1800, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 1800, 0.75).
narrative_ontology:measurement(vedi_su_t2020, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__hereditary_monopoly_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus__bhakti_devotional_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus__reformist_egalitarian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'vedic_dharmic_corpus' kernel, focusing on hereditary Brahminical authority. It is structurally distinct from the 'bhakti_devotional_reading' (direct devotion) and the 'reformist_egalitarian_reading' (constitutional equality), which are modeled as separate constraints due to their differing epsilon values and stakeholder structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
