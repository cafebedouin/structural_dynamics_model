% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__orthodox_literalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__orthodox_literalist, []).

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
 *   constraint_id: dharmasastra_corpus__orthodox_literalist
 *   human_readable: Dharmasastra: Orthodox Literalist Interpretation
 *   domain: religious_law/social_hierarchy
 *
 * SUMMARY:
 *   This constraint represents the orthodox literalist interpretation of
 *   Dharmasastra, which asserts that its prescriptions, particularly the
 *   varna/jati hierarchy, are eternal, divinely revealed truths requiring
 *   strict observance. This reading is one of several competing
 *   interpretations of the Dharmasastra corpus. The constraint is claimed as
 *   a 'mountain' by its adherents (eternal truth) but is structurally
 *   classified as a 'snare' due to its high extraction, expansive victim set,
 *   and active suppression, as measured by the model.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, 0.88).
domain_priors:suppression_score(dharmasastra_corpus__orthodox_literalist, 0.92).
domain_priors:theater_ratio(dharmasastra_corpus__orthodox_literalist, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, extractiveness, 0.88).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__orthodox_literalist, snare).
narrative_ontology:human_readable(dharmasastra_corpus__orthodox_literalist, "Dharmasastra: Orthodox Literalist Interpretation").
narrative_ontology:topic_domain(dharmasastra_corpus__orthodox_literalist, "religious_law/social_hierarchy").

domain_priors:requires_active_enforcement(dharmasastra_corpus__orthodox_literalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__orthodox_literalist, '1a822acc-47f8-4fb4-a687-3e7da4c48c28').
narrative_ontology:cs_kernel_codification('1a822acc-47f8-4fb4-a687-3e7da4c48c28', fixed_text).
narrative_ontology:cs_authority_grounding('1a822acc-47f8-4fb4-a687-3e7da4c48c28', lineage).
narrative_ontology:cs_interpretation_layer_present('1a822acc-47f8-4fb4-a687-3e7da4c48c28').
narrative_ontology:cs_reading_relation('1a822acc-47f8-4fb4-a687-3e7da4c48c28', dharmasastra_corpus__reformist_contextual, forecloses).
narrative_ontology:cs_reading_relation('1a822acc-47f8-4fb4-a687-3e7da4c48c28', dharmasastra_corpus__abolitionist_rejection, forecloses).
narrative_ontology:cs_axiom('1a822acc-47f8-4fb4-a687-3e7da4c48c28', foundational, dharmasastra_as_eternal_revelation).
narrative_ontology:cs_axiom_status(dharmasastra_as_eternal_revelation, holdable).
narrative_ontology:cs_axiom_grounding('1a822acc-47f8-4fb4-a687-3e7da4c48c28', dharmasastra_as_eternal_revelation, theological).
narrative_ontology:cs_axiom('1a822acc-47f8-4fb4-a687-3e7da4c48c28', foundational, varna_jati_hierarchy_as_divine_order).
narrative_ontology:cs_axiom_status(varna_jati_hierarchy_as_divine_order, holdable).
narrative_ontology:cs_axiom_grounding('1a822acc-47f8-4fb4-a687-3e7da4c48c28', varna_jati_hierarchy_as_divine_order, theological).
narrative_ontology:cs_reference_frame('1a822acc-47f8-4fb4-a687-3e7da4c48c28', vedic_social_order).
narrative_ontology:cs_drift_state('1a822acc-47f8-4fb4-a687-3e7da4c48c28', contemporary_secular_human_rights_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('1a822acc-47f8-4fb4-a687-3e7da4c48c28', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, brahmins).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, kshatriyas).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, vaishyas).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, orthodox_religious_authorities).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, dalits).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, shudras).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, women).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__orthodox_literalist, divine_revelation_of_dharma).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__orthodox_literalist, varna_jati_hierarchy_as_natural_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the highest ritual and social status, benefiting from exclusive access to sacred knowledge, ritual performance, and traditional education. They are often the primary interpreters and enforcers of Dharmasastra, though not the sole institutional authority.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, brahmins, beneficiary,
    powerful, generational, arbitrage, global).

% Benefit from their position in the traditional warrior and ruling class, maintaining social order and receiving deference. Their status is legitimized by the Dharmasastra, providing a stable social structure for their power.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, kshatriyas, beneficiary,
    powerful, biographical, mobile, national).

% Benefit from their role in trade, commerce, and agriculture, with their economic activities and social standing defined and protected within the varna system, albeit below Brahmins and Kshatriyas.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, vaishyas, beneficiary,
    moderate, biographical, constrained, regional).

% Are assigned roles of service to the upper castes, with limited access to education, ritual participation, and social mobility. They bear significant social and economic costs under the literalist interpretation.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, shudras, payer,
    powerless, generational, trapped, local).

% Are considered outside the varna system, historically subjected to severe discrimination, untouchability, and exclusion from public life, education, and religious spaces. They bear the highest costs of this constraint.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, dalits, payer,
    powerless, generational, trapped, local).

% Are assigned subordinate roles within the family and society, with restrictions on education, property rights, and ritual independence. Their identity is often fused with their prescribed roles, making exit difficult.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, women, payer,
    powerless, biographical, identity_locked, local).

% Institutional bodies and traditional scholars who uphold and propagate the literalist interpretation of Dharmasastra, actively enforcing its prescriptions through religious discourse, social pressure, and community sanctions. Their authority is derived from this interpretation.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, orthodox_religious_authorities, agenda_setter,
    institutional, generational, identity_locked, global).

% Advocate for reinterpreting or rejecting caste-based and gender-discriminatory aspects of Dharmasastra, often facing social ostracization or religious condemnation from orthodox circles. Their voices are actively marginalized within the literalist framework.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, reformist_activists, excluded,
    organized, generational, constrained, national).

% Study the Dharmasastra texts and their historical and contemporary impact from an academic, critical perspective, analyzing their social function and effects without necessarily adhering to their prescriptions.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, analytical_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dharmasastra_corpus__orthodox_literalist, brahmins).
narrative_ontology:fixing_cost_class(dharmasastra_corpus__orthodox_literalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a rigid, divinely ordained social and ritual order (varna/jati hierarchy), defining roles, duties, and interactions for all members of society to maintain cosmic balance and social stability.
% TRANSFER_FUNCTION: Transfers ritual purity, social status, economic advantage, and access to knowledge from lower castes (Dalits, Shudras) and women to upper castes (Brahmins, Kshatriyas, Vaishyas) and men, enforced through religious sanction, social custom, and traditional legal systems.
% ABSENT_VOICES: Dalits, Shudras, and women are structurally excluded from positions of interpretive authority and their historical grievances are dismissed within the orthodox framework. Reformist and abolitionist voices, which challenge the literalist interpretation, are actively marginalized by orthodox religious authorities.
% DISAPPEARANCE_RATIONALE: If the Dharmasastra's authority as eternal, revealed truth requiring literal observance vanished overnight, the entire social, ritual, and legal fabric of orthodox Hindu society would undergo profound and rapid reorganization. The varna/jati hierarchy would lose its divine justification, leading to widespread challenges to existing power structures, social roles, and ritual practices, fundamentally altering community dynamics and individual identities.
% FOUNDING_PROBLEM: To establish a divinely ordained cosmic and social order (dharma) for human society, ensuring ritual purity, social stability, and moral conduct, believed to be essential for individual salvation and societal well-being.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox adherents and traditional institutions attest that the founding problem of maintaining cosmic and social order through prescribed dharma is still live and eternally relevant. Reformist and abolitionist movements, along with secular legal systems and human rights organizations, attest that the founding problem (as interpreted by literalists to justify hierarchy) is either obsolete, unjust, or a cover for extraction, and that the constraint persists due to institutional inertia and power dynamics rather than genuine necessity.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__orthodox_literalist, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__orthodox_literalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__orthodox_literalist, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(dharmasastra_corpus__orthodox_literalist, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__orthodox_literalist, 0.88, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__orthodox_literalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__orthodox_literalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is very high (0.88) because the literalist interpretation systematically channels social, ritual, and economic benefits to upper castes and men, while imposing severe disadvantages on lower castes and women. Suppression is also very high (0.92) as this interpretation is enforced through deep-seated religious authority, social ostracization, and historical legal frameworks that actively suppress dissent and alternatives. Theater ratio is low (0.20) because the constraint's primary function is indeed the maintenance of this hierarchical order, with little performative maintenance of an atrophied function. Accessibility collapse is high (0.90) as the religious framework condemns alternatives, making exit extremely difficult. Resistance is high (0.75) due to ongoing challenges from reformist and abolitionist movements.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of orthodox religious authorities and upper castes, this constraint is a divinely ordained, immutable social order (a Mountain) that ensures cosmic harmony and individual dharma. From the perspective of Dalits, Shudras, women, and reformist activists, it is a deeply extractive and oppressive system (a Snare) that perpetuates inequality through religious sanction and social coercion. The engine's classification reflects the latter structural reality, measuring the divergence from the claimed type.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmins, Kshatriyas, and Vaishyas are structural beneficiaries, receiving status and advantage. Orthodox religious authorities are agenda-setters, actively enforcing the system and benefiting from its perpetuation. Dalits, Shudras, and women are clear targets/victims, bearing the brunt of social exclusion and economic disadvantage. Reformist activists are excluded, as their challenges are not recognized as legitimate within the orthodox framework. Analytical scholars are observers, studying the system without direct participation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Snare, despite the claim of 'eternal truth,' prevents mislabeling a deeply extractive and suppressive social hierarchy as a natural or benign coordination mechanism. The high extractiveness and suppression, coupled with an expansive victim set, clearly indicate that the constraint's persistence relies on coercion and the suppression of alternatives, rather than genuine collective benefit or natural inevitability. The 'contested' status of the founding problem further highlights the divergence between the claimed purpose and actual function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_ambiguity,
    'Is the Dharmasastra''s varna/jati hierarchy truly an eternal, divinely revealed truth, or a human-constructed social system legitimized by religious texts?',
    'Comparative historical-critical textual analysis across diverse religious traditions, and sociological studies of caste systems in practice versus scriptural ideals.',
    'If constructed, the constraint''s claim to naturalness collapses, reclassifying it more firmly as a Snare. If genuinely revealed, its persistence is rooted in theological commitment rather than pure extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_vs_constructed_ambiguity, conceptual, 'Ambiguity between divine revelation and human construction of social hierarchy.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the high suppression primarily structural (social ostracization, economic exclusion, legal discrimination) or internalized (belief in one''s karma, divine will, or prescribed dharma)?',
    'Post-exit suppression trajectory: if individuals from lower castes or women continue to self-limit or face internal conflict after leaving orthodox communities, it suggests internalized suppression. Sociological studies on identity formation and resilience.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest, as targets carry the suppression with them. This complicates efforts to dismantle the constraint through external legal or social reforms alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in maintaining social hierarchy.').

omega_variable(
    interpretive_authority_legitimacy,
    'Who legitimately holds the authority to interpret Dharmasastra: traditional Brahminical scholars, broader community consensus, or individual conscience?',
    'Analysis of historical shifts in interpretive power, and contemporary legal challenges to traditional authority. Examination of how different communities resolve interpretive disputes.',
    'If interpretive authority is successfully decentralized, the literalist reading''s power to enforce its prescriptions diminishes, potentially leading to a reclassification towards a more contested or degraded type. If authority remains concentrated, the Snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_legitimacy, conceptual, 'Contested legitimacy of interpretive authority over sacred texts.').

omega_variable(
    kernel_reading_difference,
    'What are the precise structural differences between the orthodox_literalist reading and its sibling readings (reformist_contextual, abolitionist_rejection) of the Dharmasastra corpus kernel?',
    'Direct comparison of core axioms, beneficiary/victim sets, and proposed enforcement mechanisms across all three readings, as articulated by their proponents.',
    'Clarifies the specific points of contention and how each reading instantiates a distinct constraint, enabling precise analysis of their respective classifications and network effects.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_difference, conceptual, 'Structural differences between Dharmasastra kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__orthodox_literalist, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__orthodox_literalist, theater_ratio, 0, 0.25).
narrative_ontology:measurement(dhar_tr_t20, dharmasastra_corpus__orthodox_literalist, theater_ratio, 20, 0.23).
narrative_ontology:measurement(dhar_tr_t40, dharmasastra_corpus__orthodox_literalist, theater_ratio, 40, 0.21).
narrative_ontology:measurement(dhar_tr_t60, dharmasastra_corpus__orthodox_literalist, theater_ratio, 60, 0.2).
narrative_ontology:measurement(dhar_tr_t80, dharmasastra_corpus__orthodox_literalist, theater_ratio, 80, 0.2).
narrative_ontology:measurement(dhar_tr_t100, dharmasastra_corpus__orthodox_literalist, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(dhar_be_t20, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 20, 0.86).
narrative_ontology:measurement(dhar_be_t40, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 40, 0.87).
narrative_ontology:measurement(dhar_be_t60, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 60, 0.88).
narrative_ontology:measurement(dhar_be_t80, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 80, 0.88).
narrative_ontology:measurement(dhar_be_t100, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 100, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(dhar_su_t20, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 20, 0.91).
narrative_ontology:measurement(dhar_su_t40, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 40, 0.92).
narrative_ontology:measurement(dhar_su_t60, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 60, 0.92).
narrative_ontology:measurement(dhar_su_t80, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 80, 0.92).
narrative_ontology:measurement(dhar_su_t100, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 100, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__orthodox_literalist, identity_coordination).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus__reformist_contextual).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus__abolitionist_rejection).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Dharmasastra corpus kernel. Each reading instantiates a separate constraint with its own structural properties and classification, linked here to reflect their shared textual origin and ongoing contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
