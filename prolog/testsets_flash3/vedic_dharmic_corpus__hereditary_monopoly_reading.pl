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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: vedic_dharmic_corpus__hereditary_monopoly_reading
 *   human_readable: Vedic Dharmic Corpus: Hereditary Monopoly Reading
 *   domain: religious/social_stratification
 *
 * SUMMARY:
 *   This constraint represents a specific reading of the Vedic Dharmic
 *   Corpus, asserting that ritual and interpretive authority are exclusively
 *   derived from birth into Brahmin lineage, and that the varna (caste)
 *   hierarchy is divinely ordained. This reading functions as a Tangled Rope,
 *   providing a coordination function (stable social/ritual order) while
 *   simultaneously enabling significant, actively enforced extraction from
 *   lower castes and women. The metrics reflect this: high extractiveness and
 *   suppression are required to maintain the hereditary monopoly against
 *   internal and external challenges. This is one reading of a contested
 *   kernel, with sibling readings offering alternative interpretations.
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
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__hereditary_monopoly_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__hereditary_monopoly_reading, "Vedic Dharmic Corpus: Hereditary Monopoly Reading").
narrative_ontology:topic_domain(vedic_dharmic_corpus__hereditary_monopoly_reading, "religious/social_stratification").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__hereditary_monopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__hereditary_monopoly_reading, '94ca7357-b0ff-4f88-841d-1c284de9b913').
narrative_ontology:cs_kernel_codification('94ca7357-b0ff-4f88-841d-1c284de9b913', fixed_text).
narrative_ontology:cs_authority_grounding('94ca7357-b0ff-4f88-841d-1c284de9b913', lineage).
narrative_ontology:cs_interpretation_layer_present('94ca7357-b0ff-4f88-841d-1c284de9b913').
narrative_ontology:cs_reading_relation('94ca7357-b0ff-4f88-841d-1c284de9b913', vedic_dharmic_corpus__bhakti_devotional_reading, coexists_with).
narrative_ontology:cs_reading_relation('94ca7357-b0ff-4f88-841d-1c284de9b913', vedic_dharmic_corpus__reformist_egalitarian_reading, forecloses).
narrative_ontology:cs_axiom('94ca7357-b0ff-4f88-841d-1c284de9b913', foundational, birth_determines_ritual_authority).
narrative_ontology:cs_axiom_status(birth_determines_ritual_authority, holdable).
narrative_ontology:cs_axiom_grounding('94ca7357-b0ff-4f88-841d-1c284de9b913', birth_determines_ritual_authority, theological).
narrative_ontology:cs_axiom('94ca7357-b0ff-4f88-841d-1c284de9b913', foundational, varna_hierarchy_divinely_ordained).
narrative_ontology:cs_axiom_status(varna_hierarchy_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('94ca7357-b0ff-4f88-841d-1c284de9b913', varna_hierarchy_divinely_ordained, theological).
narrative_ontology:cs_reference_frame('94ca7357-b0ff-4f88-841d-1c284de9b913', traditional_vedic_social_order).
narrative_ontology:cs_drift_state('94ca7357-b0ff-4f88-841d-1c284de9b913', contemporary_secular_india, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('94ca7357-b0ff-4f88-841d-1c284de9b913', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, lower_castes).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, women_in_traditional_roles).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, non_brahmin_devotees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__hereditary_monopoly_reading, non_brahmin_devotees).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__hereditary_monopoly_reading, divine_origin_of_varna).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__hereditary_monopoly_reading, textual_infallibility_of_vedas).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds exclusive rights to perform certain rituals, interpret sacred texts, and officiate religious ceremonies. Benefits from offerings and social prestige derived from this hereditary authority. Actively enforces adherence to traditional varna roles and ritual purity.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class, agenda_setter,
    institutional, generational, arbitrage, regional).

% Excluded from direct participation in many rituals and denied access to interpretive authority. Bear the social and economic costs of their ascribed status, often internalizing the hierarchy as divinely ordained. Exit means abandoning community and spiritual identity.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, lower_castes, payer,
    powerless, generational, identity_locked, local).

% Subject to gender-specific ritual restrictions and interpretive limitations, often mediated through male family members. Their spiritual access and social standing are defined by their adherence to prescribed roles within the varna system. Exit carries severe social penalties.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, women_in_traditional_roles, payer,
    powerless, biographical, identity_locked, local).

% Participate in devotional practices but are dependent on Brahmin priests for formal rituals and scriptural guidance. They derive spiritual benefit but pay through deference and offerings, with limited avenues for independent spiritual authority. Exit is possible but means losing traditional community ties.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, non_brahmin_devotees, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__hereditary_monopoly_reading, non_brahmin_devotees, beneficiary).

% Advocate for direct, personal devotion to the divine, bypassing caste and ritual intermediaries. Their interpretive authority is often dismissed or suppressed by the hereditary Brahmin class, though their numbers and influence can be substantial. They represent an alternative spiritual path.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, bhakti_movement_adherents, excluded,
    organized, generational, mobile, regional).

% Critique the hereditary system from a modern, egalitarian perspective, often drawing on constitutional principles and rational ethics. They analyze the historical and social impact of the varna hierarchy and advocate for its dismantling, but lack direct ritual authority.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, reformist_intellectuals, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, divinely sanctioned social and ritual order, ensuring the correct performance of Vedic rites and the transmission of sacred knowledge through an established lineage, which is believed to maintain cosmic balance.
% TRANSFER_FUNCTION: Transfers ritual authority, social prestige, and economic resources (offerings, patronage) to the Brahmin priestly class, in exchange for their exclusive performance of religious duties and interpretation of sacred texts. It transfers social and spiritual subordination to lower castes and women.
% ABSENT_VOICES: Historically, many marginalized groups and dissenting spiritual traditions (e.g., early Buddhist and Jain movements, various folk traditions) have been excluded or suppressed. Today, secular human rights advocates and many modern feminists would object to the inherent inequality and lack of agency imposed by this reading.
% DISAPPEARANCE_RATIONALE: If the hereditary monopoly on ritual and interpretive authority vanished, the entire social and religious structure would undergo profound reorganization. Lower castes and women would gain direct access to spiritual leadership, the ritual economy would be democratized, and the Brahmin class would lose its exclusive status, leading to a redefinition of religious practice and social hierarchy.
% FOUNDING_PROBLEM: To establish a stable social order and ensure the precise transmission and performance of sacred Vedic rituals, believed to be essential for cosmic harmony and human prosperity, by assigning specific roles and duties based on birth.
% FOUNDING_PROBLEM_CORROBORATION: The Brahmin priestly class and traditionalists attest the problem is still live, emphasizing the need for ritual purity and lineage-based transmission. Reformist intellectuals and many lower-caste organizations attest the founding problem has been superseded by modern ethical and constitutional principles, and the arrangement now primarily serves to maintain social inequality and extract rents; historical analysis and sociological studies from outside the benefiting parties support this shifted-function reading.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__hereditary_monopoly_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__hereditary_monopoly_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__hereditary_monopoly_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.65) due to the significant social, economic, and spiritual costs imposed on non-Brahmin groups, who are denied direct access to ritual and interpretive power. Suppression (0.78) is also high, reflecting the active social, religious, and sometimes legal enforcement mechanisms (e.g., traditional village councils, social ostracism) used to maintain the varna hierarchy and Brahminical authority. Theater ratio is low (0.20) because the ritual performances and interpretive functions are genuinely believed to be efficacious by many adherents, even if the underlying structure is extractive. The constraint's persistence relies on both genuine belief in its coordination function and active suppression of alternatives.
 *
 * PERSPECTIVAL GAP:
 *   From the Brahmin priestly class's perspective, this is a legitimate, divinely ordained system for maintaining cosmic order and transmitting sacred knowledge (a Rope or even Mountain). From the perspective of lower castes and women, it is a system of enforced social and spiritual extraction (a Snare). The engine's computation will highlight this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The Brahmin priestly class is the primary beneficiary and agenda-setter, collecting resources and prestige (d near 0.0). Lower castes and women are clear targets, bearing the costs of exclusion and subordination (d near 1.0). Non-Brahmin devotees are payers who also derive some benefit from participation, placing them in a constrained position (d around 0.7). Bhakti movement adherents are excluded, their alternative spiritual path suppressed by the dominant reading's enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (maintaining cosmic order through ritual purity and social stability) is contested. While the Brahmin class asserts its continued relevance, critics argue that the original coordination problem has either been superseded by modern ethics or that the current structure primarily serves to maintain the hereditary group's power. The classification as Tangled Rope prevents mislabeling it as pure coordination (Rope) by acknowledging the asymmetric extraction, while also not reducing it to pure extraction (Snare) by recognizing the genuine coordination function it provides for its beneficiaries and some adherents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_vs_social_construct,
    'Is the varna hierarchy a divinely ordained, immutable structure, or a historically contingent social construct maintained for the benefit of a specific class?',
    'Comparative historical and sociological analysis of other stratified societies, textual criticism examining the evolution of interpretive traditions, and theological arguments from within the tradition that challenge the immutability of birth-based status.',
    'If divinely ordained, the constraint''s ''emerges_naturally'' component would be higher, potentially shifting it towards a False Summit Mountain. If a social construct, its extractiveness and suppression would be more clearly understood as human-imposed, reinforcing its Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_mandate_vs_social_construct, conceptual, 'Ambiguity between divine mandate and social construction of hierarchy.').

omega_variable(
    internalized_suppression_of_lower_castes,
    'To what extent is the suppression experienced by lower castes and women structural (external barriers) versus internalized (self-concept, belief in divine ordination)?',
    'Longitudinal studies of individuals who exit traditional communities or engage in reform movements, observing the persistence of self-limiting beliefs or social anxieties after external barriers are reduced. Analysis of narratives from within marginalized communities regarding their agency and self-perception.',
    'If internalized suppression is a significant factor, the effective suppression for these groups is higher than the structural measure suggests, as they carry the constraint''s effects even in less restrictive environments. This would amplify their effective extraction (chi).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_of_lower_castes, empirical, 'Structural vs. internalized suppression mechanism for marginalized groups.').

omega_variable(
    legitimacy_of_bhakti_alternatives,
    'Does the ''bhakti_devotional_reading'' offer a genuinely viable and widely accepted alternative spiritual path that undermines the hereditary monopoly, or is it largely contained and marginalized by the dominant reading?',
    'Sociological surveys of religious practice, analysis of temple governance and funding, and examination of the social mobility and spiritual authority of non-Brahmin spiritual leaders within the broader Dharmic landscape.',
    'If bhakti is a strong, unsuppressed alternative, the ''hereditary_monopoly_reading''s'' effective suppression and extractiveness would be lower, as exit options are more robust. If bhakti is contained, the current metrics are accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_of_bhakti_alternatives, empirical, 'Viability and impact of alternative spiritual paths on the hereditary monopoly.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__hereditary_monopoly_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(vedi_tr_t10, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(vedi_tr_t20, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(vedi_tr_t30, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 30, 0.19).
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
narrative_ontology:measurement(vedi_su_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(vedi_su_t10, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 10, 0.73).
narrative_ontology:measurement(vedi_su_t20, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(vedi_su_t30, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 30, 0.77).
narrative_ontology:measurement(vedi_su_t40, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 40, 0.78).
narrative_ontology:measurement(vedi_su_t50, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__hereditary_monopoly_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
