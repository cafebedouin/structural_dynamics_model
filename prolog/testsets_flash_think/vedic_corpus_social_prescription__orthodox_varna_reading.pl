% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__orthodox_varna_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: vedic_corpus_social_prescription__orthodox_varna_reading
 *   human_readable: Vedic Texts Prescribing Varna Hierarchy (Orthodox Reading)
 *   domain: religious_studies/social_stratification/hermeneutics
 *
 * SUMMARY:
 *   This constraint represents the orthodox reading of Vedic texts that
 *   literally prescribe the Varna (caste) hierarchy as a divinely mandated
 *   cosmic order. This reading asserts that social roles, ritual status, and
 *   occupational restrictions are fixed by birth and are essential for
 *   societal and spiritual well-being. It functions as a snare, extracting
 *   labor, deference, and resources from lower castes (Shudras and Dalits)
 *   for the benefit of upper castes (Brahmins, Kshatriyas, Vaishyas),
 *   maintained through active religious, social, and often violent
 *   enforcement. The high extractiveness and suppression reflect the
 *   historical and ongoing impact of this system.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__orthodox_varna_reading, 0.88).
domain_priors:suppression_score(vedic_corpus_social_prescription__orthodox_varna_reading, 0.92).
domain_priors:theater_ratio(vedic_corpus_social_prescription__orthodox_varna_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__orthodox_varna_reading, snare).
narrative_ontology:human_readable(vedic_corpus_social_prescription__orthodox_varna_reading, "Vedic Texts Prescribing Varna Hierarchy (Orthodox Reading)").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__orthodox_varna_reading, "religious_studies/social_stratification/hermeneutics").

domain_priors:requires_active_enforcement(vedic_corpus_social_prescription__orthodox_varna_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__orthodox_varna_reading, '9baf6509-77d5-4422-8e9a-b8ea90fdd149').
narrative_ontology:cs_kernel_codification('9baf6509-77d5-4422-8e9a-b8ea90fdd149', fixed_text).
narrative_ontology:cs_authority_grounding('9baf6509-77d5-4422-8e9a-b8ea90fdd149', lineage).
narrative_ontology:cs_interpretation_layer_present('9baf6509-77d5-4422-8e9a-b8ea90fdd149').
narrative_ontology:cs_reading_relation('9baf6509-77d5-4422-8e9a-b8ea90fdd149', vedic_corpus_social_prescription__reformist_spiritual_reading, forecloses).
narrative_ontology:cs_reading_relation('9baf6509-77d5-4422-8e9a-b8ea90fdd149', vedic_corpus_social_prescription__colonial_orientalist_reading, influences).
narrative_ontology:cs_axiom('9baf6509-77d5-4422-8e9a-b8ea90fdd149', foundational, varna_divinely_ordained_social_order).
narrative_ontology:cs_axiom_status(varna_divinely_ordained_social_order, holdable).
narrative_ontology:cs_axiom_grounding('9baf6509-77d5-4422-8e9a-b8ea90fdd149', varna_divinely_ordained_social_order, theological).
narrative_ontology:cs_axiom('9baf6509-77d5-4422-8e9a-b8ea90fdd149', foundational, ritual_purity_hierarchy).
narrative_ontology:cs_axiom_status(ritual_purity_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('9baf6509-77d5-4422-8e9a-b8ea90fdd149', ritual_purity_hierarchy, conventional).
narrative_ontology:cs_reference_frame('9baf6509-77d5-4422-8e9a-b8ea90fdd149', ancient_vedic_social_order).
narrative_ontology:cs_drift_state('9baf6509-77d5-4422-8e9a-b8ea90fdd149', contemporary_secular_india, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('9baf6509-77d5-4422-8e9a-b8ea90fdd149', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, kshatriya_caste).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, vaishya_caste).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, shudra_caste).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, dalit_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and transmits the Vedic texts, performs rituals, and holds primary authority in defining and enforcing the Varna hierarchy. Benefits from ritual purity, social deference, and economic support from lower castes.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste, agenda_setter,
    institutional, generational, arbitrage, national).

% Traditionally rulers and warriors, they benefit from the social order and their position above Vaishyas and Shudras, maintaining political and military power. Their role is to protect the Dharma, including the Varna system.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, kshatriya_caste, beneficiary,
    powerful, generational, constrained, national).

% Merchants and farmers, they benefit from a stable social order that facilitates trade and agriculture, and their position above Shudras. They contribute economically to the system.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, vaishya_caste, beneficiary,
    moderate, biographical, constrained, regional).

% Traditionally laborers and service providers, they are assigned to serve the upper three Varnas. They bear the burden of social and ritual exclusion, limited occupational mobility, and economic exploitation, with few avenues for upward movement or exit.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, shudra_caste, payer,
    powerless, generational, trapped, local).

% Considered outside the Varna system ('untouchable'), they face extreme social ostracization, ritual impurity, and severe economic and physical violence. Their identity is deeply intertwined with their ascribed status, making exit profoundly difficult and dangerous.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, dalit_communities, payer,
    powerless, generational, identity_locked, local).

% Advocate for the abolition of the Varna system, reinterpreting Vedic texts to emphasize spiritual equality or rejecting their social prescriptive authority. They face significant resistance from orthodox elements but have achieved legal and social reforms.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, reformist_movements, excluded,
    organized, generational, constrained, national).

% Modern state bodies that legally prohibit caste discrimination and promote equality. They observe the persistence of the Varna system in practice, often clashing with traditional religious authorities, and implement policies to counteract its effects.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, secular_state_institutions, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste).
narrative_ontology:fixing_cost_class(vedic_corpus_social_prescription__orthodox_varna_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a rigid social hierarchy and division of labor, purportedly to maintain cosmic order, ritual purity, and societal stability by assigning roles and duties based on birth.
% TRANSFER_FUNCTION: Transfers labor, ritual services, social deference, and economic resources from Shudra and Dalit communities to the Brahmin, Kshatriya, and Vaishya castes, in exchange for spiritual guidance, protection, and economic organization.
% ABSENT_VOICES: The voices of those historically and currently oppressed by the Varna system (Shudras, Dalits) are structurally excluded from its interpretive authority. They would articulate the profound suffering, injustice, and lack of agency imposed by the hierarchy, advocating for its complete dismantling.
% DISAPPEARANCE_RATIONALE: If the orthodox interpretation and enforcement of the Varna hierarchy vanished overnight, the entire social, economic, and ritual structure of traditional Hindu society would undergo a profound and immediate reorganization. Power dynamics, land ownership, occupational roles, and ritual practices would be fundamentally challenged, leading to widespread social upheaval and the emergence of new forms of social organization.
% FOUNDING_PROBLEM: To establish a divinely ordained social order that ensures cosmic harmony, ritual purity, and a stable, functional society through a hierarchical division of labor and spiritual roles.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox religious authorities and traditionalists assert that the founding problem of maintaining cosmic and social order through Varna is still live and essential. However, reformist movements, secular scholars, and the lived experience of marginalized communities widely contest this, arguing that the system primarily serves to perpetuate inequality and extraction, and that the original 'problem' is either solved or was a justification for an extractive structure from its inception. Legal and sociological analyses from outside the benefiting parties corroborate the contested status.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__orthodox_varna_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__orthodox_varna_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__orthodox_varna_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(vedic_corpus_social_prescription__orthodox_varna_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__orthodox_varna_reading, 0.88, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is very high (0.88) due to the systemic transfer of wealth, labor, and social capital from lower to upper castes, enforced by religious doctrine and social custom. Suppression is extremely high (0.92) because exit options are severely limited by birth, social ostracization, and historical violence, often reinforced by religious injunctions. Accessibility collapse is high (0.85) as the system is presented as natural and divinely ordained, making alternatives seem unthinkable within the orthodox framework. Resistance is also high (0.70), reflecting centuries of struggle against the caste system. Theater ratio is moderate (0.40): while there is genuine belief in the divine mandate, a significant portion of the maintenance involves performative rituals and social policing that primarily serve to reinforce the hierarchy and its extractive functions.
 *
 * PERSPECTIVAL GAP:
 *   The Brahmin agenda-setter seat experiences this constraint as a sacred, beneficial order, ensuring spiritual and social harmony. The Shudra and Dalit payer seats experience it as an oppressive, inescapable trap that extracts their labor and dignity, denying them fundamental rights and mobility. The engine's per-seat classification will reflect this profound divergence, with the orthodox reading computing as a snare for the victims and potentially a tangled rope or even a rope for the beneficiaries, depending on their specific structural position and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The Brahmin, Kshatriya, and Vaishya castes are beneficiaries, with Brahmins acting as the primary agenda-setters, deriving significant social, ritual, and economic benefits. Shudra and Dalit communities are the primary targets and victims, bearing the brunt of the extraction and suppression. Reformist movements are excluded, actively challenging the constraint's legitimacy and seeking its dismantling. Secular state institutions act as observers, legally opposing the system but often struggling to fully dismantle its deeply entrenched social practices.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a snare prevents mislabeling the Varna hierarchy as a legitimate coordination mechanism. While proponents claim it coordinates society, the high extractiveness, severe suppression, and identifiable victims reveal its true nature as a system designed for asymmetric benefit, not collective good. The persistence of the system, despite legal prohibitions and widespread resistance, highlights its reliance on coercion and the suppression of alternatives, rather than genuine social consensus or functional necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_vs_social_construction,
    'Is the Varna hierarchy a divinely mandated cosmic order, as claimed by this reading, or a human-constructed social system that evolved for power and resource control?',
    'Comparative historical and sociological analysis of other ancient societies'' social stratification, textual criticism of Vedic interpolations, and anthropological studies of caste-like systems globally, independent of theological claims.',
    'If purely human-constructed, the ''divine mandate'' justification collapses, exposing the constraint as a pure snare with no legitimate coordination function beyond maintaining extraction. If a genuine divine mandate could be established (a conceptual impossibility for empirical analysis), the classification would shift towards a mountain for believers, though still extractive for non-believers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_mandate_vs_social_construction, conceptual, 'Ambiguity between divine mandate and social construction of Varna.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (economic dependency, social ostracization, physical violence) or internalized (belief in one''s ascribed status, fear of karmic retribution)?',
    'Post-exit suppression trajectory: if individuals who physically exit the traditional system (e.g., migrate to urban areas, convert) still exhibit self-limiting behaviors or psychological burdens related to caste, it indicates internalized suppression. Sociological studies on identity formation and resilience among marginalized groups.',
    'If internalized suppression is a significant component, the constraint''s effective suppression is higher than structural measures alone suggest, as targets carry the suppression with them even after external barriers are reduced. This would make dismantling the constraint more complex, requiring cultural and psychological interventions in addition to legal and economic ones.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the Varna system.').

omega_variable(
    orthodox_vs_reformist_structural_delta,
    'How would the structural properties (extractiveness, suppression, beneficiary/victim sets) of this ''orthodox_varna_reading'' differ if the ''reformist_spiritual_reading'' were adopted as the dominant interpretation?',
    'Analysis of historical periods or communities where reformist interpretations gained traction, and their resulting social structures. Counterfactual modeling of a society governed by the reformist reading''s principles.',
    'The reformist reading, by denying prescriptive social content, would drastically reduce or eliminate the victim set (Shudras, Dalits) and the extractiveness, potentially transforming the constraint into a rope (coordinating spiritual practice) or even a mountain (spiritual truths) for its adherents, with minimal social extraction. This highlights the profound impact of hermeneutic choice on social structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(orthodox_vs_reformist_structural_delta, conceptual, 'Structural differences between orthodox and reformist readings of Vedic texts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__orthodox_varna_reading, 1000, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t1000, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 1000, 0.2).
narrative_ontology:measurement(vedi_tr_t1500, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 1500, 0.25).
narrative_ontology:measurement(vedi_tr_t1800, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 1800, 0.3).
narrative_ontology:measurement(vedi_tr_t1900, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 1900, 0.35).
narrative_ontology:measurement(vedi_tr_t1950, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 1950, 0.4).
narrative_ontology:measurement(vedi_tr_t2000, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(vedi_tr_t2024, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(vedi_be_t1000, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 1000, 0.8).
narrative_ontology:measurement(vedi_be_t1500, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 1500, 0.85).
narrative_ontology:measurement(vedi_be_t1800, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 1800, 0.9).
narrative_ontology:measurement(vedi_be_t1900, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 1900, 0.92).
narrative_ontology:measurement(vedi_be_t1950, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 1950, 0.9).
narrative_ontology:measurement(vedi_be_t2000, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 2000, 0.89).
narrative_ontology:measurement(vedi_be_t2024, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 2024, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t1000, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 1000, 0.85).
narrative_ontology:measurement(vedi_su_t1500, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 1500, 0.9).
narrative_ontology:measurement(vedi_su_t1800, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 1800, 0.95).
narrative_ontology:measurement(vedi_su_t1900, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 1900, 0.98).
narrative_ontology:measurement(vedi_su_t1950, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 1950, 0.95).
narrative_ontology:measurement(vedi_su_t2000, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 2000, 0.93).
narrative_ontology:measurement(vedi_su_t2024, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__orthodox_varna_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, hindu_marriage_laws).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, temple_entry_rules).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, occupational_inheritance_norms).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription__reformist_spiritual_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription__colonial_orientalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'vedic_corpus_social_prescription' kernel. Its structural properties (high extraction, specific victim/beneficiary sets) are distinct from sibling readings, which are modeled as separate constraints. All readings are linked via 'affects_constraints'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
