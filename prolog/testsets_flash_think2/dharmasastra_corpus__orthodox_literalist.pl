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
 *   human_readable: Dharmasastra: Orthodox Literalist Reading of Varna/Jati Hierarchy
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   This constraint represents the orthodox literalist reading of the
 *   Dharmasastra corpus, which asserts that its prescriptions, particularly
 *   regarding the varna/jati (caste) hierarchy, are eternal, divinely
 *   revealed truths requiring strict, literal observance. This reading forms
 *   the basis for a highly stratified social order with significant
 *   implications for ritual, social, and economic life. This is one reading
 *   of the 'dharmasastra_corpus' kernel, with sibling readings
 *   'reformist_contextual' and 'abolitionist_rejection'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, 0.88).
domain_priors:suppression_score(dharmasastra_corpus__orthodox_literalist, 0.92).
domain_priors:theater_ratio(dharmasastra_corpus__orthodox_literalist, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, extractiveness, 0.88).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__orthodox_literalist, snare).
narrative_ontology:human_readable(dharmasastra_corpus__orthodox_literalist, "Dharmasastra: Orthodox Literalist Reading of Varna/Jati Hierarchy").
narrative_ontology:topic_domain(dharmasastra_corpus__orthodox_literalist, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__orthodox_literalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__orthodox_literalist, '28b664bc-3444-450c-be8a-9a45aae8884c').
narrative_ontology:cs_kernel_codification('28b664bc-3444-450c-be8a-9a45aae8884c', fixed_text).
narrative_ontology:cs_authority_grounding('28b664bc-3444-450c-be8a-9a45aae8884c', lineage).
narrative_ontology:cs_interpretation_layer_present('28b664bc-3444-450c-be8a-9a45aae8884c').
narrative_ontology:cs_reading_relation('28b664bc-3444-450c-be8a-9a45aae8884c', dharmasastra_corpus__reformist_contextual, forecloses).
narrative_ontology:cs_reading_relation('28b664bc-3444-450c-be8a-9a45aae8884c', dharmasastra_corpus__abolitionist_rejection, forecloses).
narrative_ontology:cs_axiom('28b664bc-3444-450c-be8a-9a45aae8884c', foundational, dharmasastra_is_eternal_shruti).
narrative_ontology:cs_axiom_status(dharmasastra_is_eternal_shruti, holdable).
narrative_ontology:cs_axiom_grounding('28b664bc-3444-450c-be8a-9a45aae8884c', dharmasastra_is_eternal_shruti, theological).
narrative_ontology:cs_axiom('28b664bc-3444-450c-be8a-9a45aae8884c', foundational, varna_jati_hierarchy_is_divine_order).
narrative_ontology:cs_axiom_status(varna_jati_hierarchy_is_divine_order, holdable).
narrative_ontology:cs_axiom_grounding('28b664bc-3444-450c-be8a-9a45aae8884c', varna_jati_hierarchy_is_divine_order, deontological).
narrative_ontology:cs_reference_frame('28b664bc-3444-450c-be8a-9a45aae8884c', vedic_revealed_truth).
narrative_ontology:cs_drift_state('28b664bc-3444-450c-be8a-9a45aae8884c', contemporary_secular_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('28b664bc-3444-450c-be8a-9a45aae8884c', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, brahmins).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, kshatriyas).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, vaishyas).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, orthodox_scholars).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, dalits).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, shudras).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, women).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As traditional interpreters and custodians of Dharmasastra, they define and enforce its literal observance, benefiting from ritual authority, social status, and economic privileges derived from the varna/jati hierarchy. They are the primary beneficiaries and administrators of the constraint.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, brahmins, agenda_setter,
    institutional, civilizational, arbitrage, universal).

% Benefit from the social order that assigns them roles of protection and governance, reinforcing their power and status within the hierarchy. Their adherence to Dharmasastra legitimizes their traditional authority.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, kshatriyas, beneficiary,
    powerful, generational, constrained, national).

% Benefit from the stability and economic roles assigned within the varna system, which often grants them control over trade and commerce, albeit with ritual subordination to Brahmins and Kshatriyas.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, vaishyas, beneficiary,
    powerful, biographical, constrained, national).

% Bear the costs of social and ritual exclusion, limited access to education and religious texts, and are assigned roles of service. Their social mobility is severely restricted by the literal interpretation of Dharmasastra.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, shudras, payer,
    powerless, generational, trapped, national).

% Are subjected to the most severe forms of social exclusion, ritual impurity, and economic exploitation, often relegated to stigmatized occupations. Their situation is one of extreme extraction with virtually no exit options within the orthodox framework.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, dalits, payer,
    powerless, generational, trapped, national).

% Are excluded from certain rituals, Vedic study, and often denied autonomy in marriage, property, and public life, with their roles primarily defined by domestic and reproductive functions. Their identity is often deeply intertwined with the prescribed roles, making exit difficult.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, women, payer,
    powerless, biographical, identity_locked, local).

% Are the intellectual custodians and propagators of the orthodox literalist reading, investing their careers and social standing in its defense and transmission. Their identity is fused with the interpretive tradition.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, orthodox_scholars, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Advocate for contextual interpretations of Dharmasastra, seeking to separate its ethical core from caste-based prescriptions. They are excluded from the internal interpretive authority of orthodox institutions but exert external pressure.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, reformist_activists, excluded,
    organized, biographical, constrained, national).

% Seek the complete rejection of Dharmasastra's authority and the dismantling of the caste system. They are fundamentally opposed to the orthodox literalist reading and are actively suppressed by its adherents.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, abolitionist_movements, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dharmasastra_corpus__orthodox_literalist, brahmins).
narrative_ontology:fixing_cost_class(dharmasastra_corpus__orthodox_literalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a divinely ordained social hierarchy (varna/jati) that defines roles, duties (dharma), and ritual purity for all members of society, ensuring social order and cosmic harmony according to its adherents.
% TRANSFER_FUNCTION: Transfers ritual authority, social status, economic opportunities, and political power from lower castes (Shudras, Dalits) and women to upper castes (Brahmins, Kshatriyas, Vaishyas) and men, while enforcing strict social boundaries.
% ABSENT_VOICES: Dalits, Shudras, and women are structurally excluded from the interpretive and authoritative discourse, their perspectives on the hierarchy and its impacts systematically marginalized. Reformist and abolitionist voices are actively suppressed or dismissed as illegitimate.
% DISAPPEARANCE_RATIONALE: If the Dharmasastra's authority as eternal, revealed truth vanished overnight, the entire social, ritual, and legal fabric of orthodox Hindu society would undergo a profound and rapid reorganization, challenging centuries of established hierarchy and power structures.
% FOUNDING_PROBLEM: To establish and maintain a stable, divinely sanctioned social order, ritual purity, and cosmic harmony by assigning specific roles and duties (dharma) to different social groups (varna/jati).
% FOUNDING_PROBLEM_CORROBORATION: Orthodox adherents and traditional institutions assert the founding problem of maintaining dharma and social order is still live and essential. Reformist and abolitionist movements, along with secular legal frameworks, argue that the 'problem' is a justification for oppression, and the original intent has been corrupted or was inherently flawed; independent historical and sociological analyses support the latter view.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__orthodox_literalist, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__orthodox_literalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__orthodox_literalist, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is very high (0.88) due to the severe and systemic transfer of status, resources, and opportunities from lower castes and women to upper castes and men. Suppression is extremely high (0.92) as the system relies on active social, ritual, and sometimes physical enforcement, backed by religious authority, to maintain its rigid boundaries and prevent dissent or exit. Accessibility collapse is high (0.85) because the constraint is presented as an immutable, divinely ordained truth, making alternatives seem unthinkable within the orthodox framework. Resistance is also high (0.70), reflecting centuries of social movements and contemporary activism against the caste system, despite the severe suppression. Theater ratio is low (0.10) because the constraint is genuinely believed and actively enforced by its beneficiaries, with little performative maintenance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Brahmins and orthodox scholars, the Dharmasastra provides a divinely ordained, harmonious social order. From the perspective of Dalits, Shudras, and women, the same structure is a system of profound and inescapable extraction and oppression. The engine's classification will highlight this divergence, showing a Snare from the victims' seats and a perceived Rope/Mountain from the beneficiaries' seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmins, Kshatriyas, and Vaishyas are clear beneficiaries, deriving status, power, and economic advantage from the hierarchy. Orthodox scholars, often Brahmins, also benefit from their role as interpreters and custodians. Dalits, Shudras, and women are the primary targets, bearing the brunt of social, ritual, and economic exclusion. Their exit options are severely limited, often identity-locked due to deeply internalized social roles and religious beliefs.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is classified as a Snare because its coordination story (divine order, cosmic harmony) serves as a cover for substantial, asymmetric extraction from identifiable victims. It is not a Piton because it is actively enforced and provides clear, concentrated benefits to its agenda-setters and beneficiaries, who have a strong incentive to maintain it. The founding problem's status is contested, but the persistence of the arrangement is clearly tied to the benefits it provides, not merely inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_vs_social_construct,
    'Is the varna/jati hierarchy truly a divinely ordained, eternal truth as claimed by the orthodox literalist reading, or is it a human social construct that has been sacralized?',
    'Comparative religious studies, historical sociology, and critical textual analysis that examine the evolution of caste systems and their relationship to religious texts across different cultures and time periods.',
    'If proven to be a human construct, the constraint''s claim to ''emerges_naturally'' (if it were a Mountain) would be falsified, and its extractiveness would be re-evaluated as purely human-imposed, strengthening its Snare classification. If genuinely divine, its resistance would be re-framed as resistance to divine will.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_mandate_vs_social_construct, conceptual, 'Ambiguity between divine mandate and social construction of hierarchy.').

omega_variable(
    internalized_vs_structural_suppression,
    'To what extent is the measured suppression internalized by the victims (e.g., through belief in karma, dharma, or social conditioning) versus being purely structural (e.g., legal, economic, or physical barriers)?',
    'Longitudinal studies of individuals who exit orthodox communities or receive education challenging these beliefs: if suppression persists after structural barriers are removed, it indicates internalized components. Analysis of psychological and sociological impacts of caste-based discrimination.',
    'If suppression is significantly internalized, the effective suppression for victims is higher than structural measures alone suggest, as they carry the constraint with them even after physical exit. This would deepen the Snare classification by highlighting the difficulty of true liberation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural vs. internalized suppression mechanism in caste hierarchy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__orthodox_literalist, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__orthodox_literalist, theater_ratio, 0, 0.1).
narrative_ontology:measurement(dhar_tr_t400, dharmasastra_corpus__orthodox_literalist, theater_ratio, 400, 0.1).
narrative_ontology:measurement(dhar_tr_t800, dharmasastra_corpus__orthodox_literalist, theater_ratio, 800, 0.1).
narrative_ontology:measurement(dhar_tr_t1200, dharmasastra_corpus__orthodox_literalist, theater_ratio, 1200, 0.1).
narrative_ontology:measurement(dhar_tr_t1600, dharmasastra_corpus__orthodox_literalist, theater_ratio, 1600, 0.1).
narrative_ontology:measurement(dhar_tr_t2000, dharmasastra_corpus__orthodox_literalist, theater_ratio, 2000, 0.1).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(dhar_be_t400, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 400, 0.86).
narrative_ontology:measurement(dhar_be_t800, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 800, 0.87).
narrative_ontology:measurement(dhar_be_t1200, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 1200, 0.87).
narrative_ontology:measurement(dhar_be_t1600, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 1600, 0.88).
narrative_ontology:measurement(dhar_be_t2000, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 2000, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(dhar_su_t400, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 400, 0.91).
narrative_ontology:measurement(dhar_su_t800, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 800, 0.91).
narrative_ontology:measurement(dhar_su_t1200, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 1200, 0.92).
narrative_ontology:measurement(dhar_su_t1600, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 1600, 0.92).
narrative_ontology:measurement(dhar_su_t2000, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 2000, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__orthodox_literalist, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
