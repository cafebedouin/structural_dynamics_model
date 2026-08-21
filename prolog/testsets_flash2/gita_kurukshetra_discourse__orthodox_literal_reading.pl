% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__orthodox_literal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__orthodox_literal_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: gita_kurukshetra_discourse__orthodox_literal_reading
 *   human_readable: Gita Kurukshetra Discourse: Orthodox Literal Reading
 *   domain: religious_studies/textual_hermeneutics/ethical_philosophy
 *
 * SUMMARY:
 *   This constraint represents the orthodox literal reading of the Bhagavad
 *   Gita's Kurukshetra discourse, which mandates caste-based duty
 *   (varnashrama dharma) and legitimates righteous violence in dharmic war.
 *   It is one reading of a contested kernel, where other readings offer
 *   allegorical or universalist interpretations. This reading structurally
 *   benefits the Brahmin priestly class and Kshatriya warrior class by
 *   providing divine sanction for their roles and the existing social
 *   hierarchy, while extracting heavily from lower castes and those who
 *   become casualties of 'dharmic' conflict. The high suppression reflects
 *   the interpretive monopoly and social enforcement mechanisms that maintain
 *   this reading's dominance.
 *
 * KEY AGENTS:
 *   - brahmin_priestly_class: Primary agenda_setter (institutional/identity_locked) — benefits from interpretive monopoly and social order.
 *   - kshatriya_warrior_class: Primary beneficiary (powerful/constrained) — benefits from legitimation of their role and violence.
 *   - lower_castes: Primary payer (powerless/trapped) — bears the costs of rigid hierarchy and limited agency.
 *   - those_killed_in_dharmic_war: Primary payer (powerless/trapped) — direct victims of legitimated violence.
 *   - dissenting_interpretations: Excluded (moderate/constrained) — marginalized by orthodox establishment.
 *   - traditional_social_order: Beneficiary (institutional/identity_locked) — abstract entity representing the system maintained by this reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, 0.85).
domain_priors:suppression_score(gita_kurukshetra_discourse__orthodox_literal_reading, 0.92).
domain_priors:theater_ratio(gita_kurukshetra_discourse__orthodox_literal_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__orthodox_literal_reading, tangled_rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__orthodox_literal_reading, "Gita Kurukshetra Discourse: Orthodox Literal Reading").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__orthodox_literal_reading, "religious_studies/textual_hermeneutics/ethical_philosophy").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__orthodox_literal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__orthodox_literal_reading, 'a7b40f7e-6f59-4b30-bd62-0caba0345404').
narrative_ontology:cs_kernel_codification('a7b40f7e-6f59-4b30-bd62-0caba0345404', fixed_text).
narrative_ontology:cs_authority_grounding('a7b40f7e-6f59-4b30-bd62-0caba0345404', lineage).
narrative_ontology:cs_interpretation_layer_present('a7b40f7e-6f59-4b30-bd62-0caba0345404').
narrative_ontology:cs_reading_relation('a7b40f7e-6f59-4b30-bd62-0caba0345404', gita_kurukshetra_discourse__gandhian_allegorical_reading, forecloses).
narrative_ontology:cs_reading_relation('a7b40f7e-6f59-4b30-bd62-0caba0345404', gita_kurukshetra_discourse__universalist_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('a7b40f7e-6f59-4b30-bd62-0caba0345404', foundational, varnashrama_dharma_divinely_ordained).
narrative_ontology:cs_axiom_status(varnashrama_dharma_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('a7b40f7e-6f59-4b30-bd62-0caba0345404', varnashrama_dharma_divinely_ordained, theological).
narrative_ontology:cs_axiom('a7b40f7e-6f59-4b30-bd62-0caba0345404', foundational, kshatriya_duty_includes_righteous_violence).
narrative_ontology:cs_axiom_status(kshatriya_duty_includes_righteous_violence, holdable).
narrative_ontology:cs_axiom_grounding('a7b40f7e-6f59-4b30-bd62-0caba0345404', kshatriya_duty_includes_righteous_violence, deontological).
narrative_ontology:cs_reference_frame('a7b40f7e-6f59-4b30-bd62-0caba0345404', traditional_dharmic_social_order).
narrative_ontology:cs_drift_state('a7b40f7e-6f59-4b30-bd62-0caba0345404', contemporary_globalized_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a7b40f7e-6f59-4b30-bd62-0caba0345404', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_priestly_class).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_class).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, traditional_social_order).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, lower_castes).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, those_killed_in_dharmic_war).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, dissenting_interpretations).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__orthodox_literal_reading, varnashrama_dharma_doctrine).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__orthodox_literal_reading, karma_yoga_doctrine_orthodox_interpretation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the interpretive monopoly on sacred texts, including the Gita. Benefits from the legitimation of a hierarchical social order where their spiritual authority is paramount. Their identity is fused with the preservation of this orthodox reading.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_priestly_class, agenda_setter,
    institutional, generational, identity_locked, regional).

% Legitimates their role in governance and warfare as divinely ordained duty (dharma). The text provides a moral framework for violence in the service of maintaining social order, which benefits their political and military power.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_class, beneficiary,
    powerful, biographical, constrained, regional).

% Are assigned fixed, often subservient, duties within the varnashrama dharma system, with limited social mobility or agency. They bear the costs of a rigid hierarchy justified by this reading of the text, with no legitimate means of exit or resistance within the framework.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, lower_castes, payer,
    powerless, generational, trapped, regional).

% Are the direct victims of violence legitimated by the concept of 'dharmic war' and caste-based duty. Their lives are sacrificed in conflicts deemed righteous by the ruling classes, with no recourse.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, those_killed_in_dharmic_war, payer,
    powerless, immediate, trapped, local).

% Alternative readings (e.g., allegorical, universalist) are actively suppressed or marginalized by the orthodox establishment. While they exist, their influence within the dominant discourse is constrained by the interpretive monopoly.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, dissenting_interpretations, excluded,
    moderate, biographical, constrained, global).

% The abstract concept of a divinely ordained, hierarchical social structure that is maintained and legitimated by this reading. It 'benefits' by its continued existence and perceived naturalness.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, traditional_social_order, beneficiary,
    institutional, civilizational, identity_locked, regional).
narrative_ontology:stakeholder_non_agent(gita_kurukshetra_discourse__orthodox_literal_reading, traditional_social_order).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social roles and duties within a hierarchical system, providing a framework for governance, spiritual practice, and societal stability based on perceived divine mandate.
% TRANSFER_FUNCTION: Transfers spiritual authority and social privilege to the Brahmin and Kshatriya classes, while transferring fixed duties and limited agency to lower castes, all legitimated by textual interpretation.
% ABSENT_VOICES: Those advocating for social equality, non-violence, or individual spiritual paths outside of caste-based duty are excluded from the interpretive authority. They would challenge the literal interpretation of violence and hierarchy.
% DISAPPEARANCE_RATIONALE: If this orthodox literal reading vanished, the legitimacy of caste-based duties and 'righteous' violence would collapse, leading to a profound reordering of social, political, and religious authority within dharmic traditions. The power structures it underpins would be fundamentally challenged.
% FOUNDING_PROBLEM: To establish a clear framework for righteous action (dharma) and social order in a time of moral confusion and conflict, particularly for the warrior class facing ethical dilemmas.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox religious institutions and traditional social structures continue to assert the problem is live, citing the need for moral guidance and social stability. Critics from within and outside the tradition argue the 'problem' has shifted to one of maintaining an extractive hierarchy, but the original justification is still invoked.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__orthodox_literal_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__orthodox_literal_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__orthodox_literal_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gita_kurukshetra_discourse__orthodox_literal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__orthodox_literal_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.85) due to the significant social and spiritual costs imposed on lower castes and the justification of violence. Suppression is very high (0.92) because this reading is maintained through a powerful interpretive monopoly, social ostracization, and historical enforcement of caste norms, making exit or dissent extremely difficult. Theater ratio is low (0.1) because the constraint's function (legitimating hierarchy and violence) is directly served by its literal interpretation; there is little performative maintenance masking an atrophied function. Accessibility collapse is high (0.8) as alternatives are largely suppressed within the traditional framework. Resistance is moderate (0.7) reflecting historical and ongoing challenges to caste and violence, but these are often met with strong counter-enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The Brahmin and Kshatriya classes experience this as a legitimate, divinely ordained framework for social order and duty, providing clear guidance and stability. Lower castes and victims of war experience it as a deeply extractive and suppressive system that denies their agency and justifies their suffering. The engine's per-seat classification will reflect this divergence based on their structural positions and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The Brahmin priestly class and Kshatriya warrior class are clear beneficiaries, with their power and roles legitimated by this reading (low directionality). Lower castes and those killed in 'dharmic war' are direct targets, bearing the primary costs (high directionality). Dissenting interpretations are excluded, meaning the constraint actively works against their existence. The 'traditional_social_order' is an abstract beneficiary, representing the system itself.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Tangled Rope because it presents a coordination function (social order, duty) but is deeply intertwined with asymmetric extraction (caste hierarchy, legitimated violence) and requires active enforcement (interpretive monopoly, social sanctions). It prevents mislabeling by highlighting that the 'coordination' is not universally beneficial but comes at a severe cost to specific groups, sustained by suppression rather than voluntary participation. The founding problem is still 'live' for its proponents, but its status is 'contested' by those who see its function as having drifted to pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_monopoly_legitimacy,
    'Is the Brahmin priestly class''s interpretive monopoly on the Gita a legitimate spiritual authority or a mechanism for maintaining social power?',
    'Historical analysis of interpretive shifts, comparative study of other dharmic traditions with different interpretive structures, and sociological studies of power dynamics within religious institutions.',
    'If primarily a power mechanism, the suppression metric for ''dissenting_interpretations'' would be re-evaluated as even more structurally enforced, and the extractiveness would be seen as more directly tied to institutional control rather than spiritual guidance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_monopoly_legitimacy, conceptual, 'Ambiguity regarding the nature of interpretive authority.').

omega_variable(
    violence_as_metaphor_vs_literal,
    'To what extent is the ''righteous violence'' described in the Kurukshetra discourse intended as a literal call to arms versus an allegorical representation of internal spiritual struggle?',
    'Textual analysis of other dharmic scriptures, historical context of the Gita''s composition, and the lived experiences of practitioners who interpret it allegorically versus literally.',
    'If predominantly allegorical, the ''victims'' of dharmic war would be reclassified as conceptual rather than literal, significantly lowering the base extractiveness and suppression related to physical violence. This would shift the constraint closer to a Rope or even a Mountain (if the internal struggle is seen as an unchangeable human condition).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(violence_as_metaphor_vs_literal, empirical, 'Ambiguity in the literal vs. allegorical interpretation of violence.').

omega_variable(
    caste_as_duty_vs_oppression,
    'Is the varnashrama dharma system, as interpreted by this reading, a divinely ordained system of complementary duties or a human-constructed system of oppression?',
    'Sociological studies of caste discrimination, historical analysis of social mobility and economic disparities, and theological arguments from within dharmic traditions that challenge caste as a basis for social organization.',
    'If primarily a system of oppression, the extractiveness and suppression metrics for ''lower_castes'' would be confirmed as extremely high, reinforcing the Snare-like qualities of this aspect of the constraint. If genuinely seen as complementary duties by all participants, the extractiveness would be lower, but this is highly unlikely given historical evidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(caste_as_duty_vs_oppression, empirical, 'Ambiguity in the moral status of caste-based duty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__orthodox_literal_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gita_tr_t10, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(gita_tr_t20, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(gita_tr_t30, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(gita_tr_t40, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(gita_tr_t50, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(gita_be_t10, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 10, 0.82).
narrative_ontology:measurement(gita_be_t20, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 20, 0.84).
narrative_ontology:measurement(gita_be_t30, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 30, 0.85).
narrative_ontology:measurement(gita_be_t40, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 40, 0.85).
narrative_ontology:measurement(gita_be_t50, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t0, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0, 0.88).
narrative_ontology:measurement(gita_su_t10, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 10, 0.9).
narrative_ontology:measurement(gita_su_t20, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 20, 0.92).
narrative_ontology:measurement(gita_su_t30, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 30, 0.92).
narrative_ontology:measurement(gita_su_t40, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 40, 0.92).
narrative_ontology:measurement(gita_su_t50, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 50, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__orthodox_literal_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
