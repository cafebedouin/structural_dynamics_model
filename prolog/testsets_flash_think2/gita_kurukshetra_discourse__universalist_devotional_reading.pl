% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__universalist_devotional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__universalist_devotional_reading, []).

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
 *   constraint_id: gita_kurukshetra_discourse__universalist_devotional_reading
 *   human_readable: Bhagavad Gita: Universalist Devotional Interpretation
 *   domain: religious_studies/ethical_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'universalist devotional' reading of the
 *   Bhagavad Gita, which interprets the text as teaching a path-independent
 *   devotion (bhakti) accessible to all, regardless of caste. It redefines
 *   dharma as surrender to divine will rather than adherence to prescribed
 *   social roles, thereby dissolving caste as a spiritual barrier and
 *   promoting egalitarian access to salvation. This reading actively
 *   challenges traditional Brahminical gatekeeping authority and literal
 *   interpretations that legitimate social hierarchy and violence.
 *
 * KEY AGENTS:
 *   - universal_devotees: Primary beneficiary (moderate/mobile) — gains spiritual access
 *   - marginalized_castes: Primary beneficiary (powerless/mobile) — gains spiritual access and challenges marginalization
 *   - gita_scholars_universalist_devotional: Agenda setter (organized/mobile) — promotes and interprets this reading
 *   - traditional_brahminical_authority: Payer (institutional/constrained) — loses exclusive spiritual authority
 *   - orthodox_scholars: Payer (organized/constrained) — loses interpretive dominance
 *   - gita_scholars_orthodox_literal: Excluded (organized/constrained) — interpretive claims rendered invalid by this reading's logic
 *   - gita_scholars_gandhian_allegorical: Observer (organized/mobile) — finds common ground but differs in emphasis
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__universalist_devotional_reading, 0.15).
domain_priors:suppression_score(gita_kurukshetra_discourse__universalist_devotional_reading, 0.1).
domain_priors:theater_ratio(gita_kurukshetra_discourse__universalist_devotional_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__universalist_devotional_reading, rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__universalist_devotional_reading, "Bhagavad Gita: Universalist Devotional Interpretation").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__universalist_devotional_reading, "religious_studies/ethical_philosophy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__universalist_devotional_reading, '461846a2-722f-4bf3-8c3b-d918081d0d76').
narrative_ontology:cs_kernel_codification('461846a2-722f-4bf3-8c3b-d918081d0d76', fixed_text).
narrative_ontology:cs_authority_grounding('461846a2-722f-4bf3-8c3b-d918081d0d76', lineage).
narrative_ontology:cs_interpretation_layer_present('461846a2-722f-4bf3-8c3b-d918081d0d76').
narrative_ontology:cs_reading_relation('461846a2-722f-4bf3-8c3b-d918081d0d76', gita_kurukshetra_discourse__orthodox_literal_reading, forecloses).
narrative_ontology:cs_reading_relation('461846a2-722f-4bf3-8c3b-d918081d0d76', gita_kurukshetra_discourse__gandhian_allegorical_reading, coexists_with).
narrative_ontology:cs_axiom('461846a2-722f-4bf3-8c3b-d918081d0d76', foundational, bhakti_universal_access).
narrative_ontology:cs_axiom_status(bhakti_universal_access, holdable).
narrative_ontology:cs_axiom_grounding('461846a2-722f-4bf3-8c3b-d918081d0d76', bhakti_universal_access, deontological).
narrative_ontology:cs_axiom('461846a2-722f-4bf3-8c3b-d918081d0d76', foundational, dharma_divine_surrender).
narrative_ontology:cs_axiom_status(dharma_divine_surrender, holdable).
narrative_ontology:cs_axiom_grounding('461846a2-722f-4bf3-8c3b-d918081d0d76', dharma_divine_surrender, deontological).
narrative_ontology:cs_reference_frame('461846a2-722f-4bf3-8c3b-d918081d0d76', egalitarian_devotional_path).
narrative_ontology:cs_drift_state('461846a2-722f-4bf3-8c3b-d918081d0d76', contemporary_pluralistic_scholarship, gap(stable, minor, true)).
narrative_ontology:cs_created_at('461846a2-722f-4bf3-8c3b-d918081d0d76', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, universal_devotees).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, marginalized_castes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__universalist_devotional_reading, traditional_brahminical_authority).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__universalist_devotional_reading, orthodox_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals from any background who find spiritual liberation and purpose through direct devotion (bhakti) as taught by this interpretation, unmediated by traditional hierarchies. They benefit from the dissolution of spiritual barriers.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, universal_devotees, beneficiary,
    moderate, biographical, mobile, global).

% Groups historically excluded from spiritual practices and knowledge due to their birth. This reading offers them direct, egalitarian access to spiritual paths, challenging their traditional marginalization.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, marginalized_castes, beneficiary,
    powerless, generational, mobile, regional).

% Scholars, teachers, and spiritual leaders who actively promote and interpret the Bhagavad Gita through a universalist devotional lens, emphasizing egalitarian access and the redefinition of dharma. They shape the discourse around this reading.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, gita_scholars_universalist_devotional, agenda_setter,
    organized, generational, mobile, global).

% Hereditary priestly classes and institutions whose authority and social standing are historically tied to caste-based ritual and exclusive access to sacred knowledge. This reading undermines their traditional gatekeeping role, forcing them to cede spiritual authority.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, traditional_brahminical_authority, payer,
    institutional, generational, constrained, national).

% Academics and religious leaders who adhere to and propagate a more literal, caste-affirming, and duty-bound interpretation of the Gita. They experience a loss of interpretive dominance and face intellectual and social resistance from the universalist reading.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, orthodox_scholars, payer,
    organized, biographical, constrained, national).

% Scholars who champion the orthodox literal reading, which is directly challenged and effectively 'excluded' from the framework of the universalist devotional interpretation. Their interpretive claims are rendered invalid within this reading's logic.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, gita_scholars_orthodox_literal, excluded,
    organized, biographical, constrained, national).

% Scholars and practitioners of the Gandhian allegorical reading, which also challenges literal violence and caste hierarchy, but primarily through metaphor. They observe the universalist devotional reading, finding common ground in its anti-literalism and anti-caste stance, but differing in its emphasis on direct devotion over internal struggle.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, gita_scholars_gandhian_allegorical, observer,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent framework for individuals to pursue spiritual devotion (bhakti) and understand their ethical duties (dharma) as surrender to divine will, fostering a community of universal spiritual seekers unconstrained by social hierarchy.
% TRANSFER_FUNCTION: Transfers spiritual authority and access from traditional, caste-based gatekeepers to individual devotees; redefines dharma from rigid social role to an inner spiritual commitment, thereby transferring agency to the individual's devotional path.
% ABSENT_VOICES: Those who benefit from the traditional caste hierarchy and its associated interpretive authority are actively challenged by this reading. They would object to the dissolution of their exclusive spiritual claims and the redefinition of dharma, but their voices are structurally marginalized within the universalist framework.
% DISAPPEARANCE_RATIONALE: If this universalist devotional reading vanished, the spiritual landscape and social dynamics surrounding the Gita would shift significantly. Older, more hierarchical and caste-affirming interpretations would likely regain unchallenged dominance, impacting the spiritual access and social standing of marginalized groups, and potentially re-legitimizing rigid social duties over individual spiritual freedom.
% FOUNDING_PROBLEM: The problem of spiritual exclusion based on birth (caste) and the rigid, often violent, interpretation of dharma as fixed social duty, which limited access to salvation and perpetuated social injustice.
% FOUNDING_PROBLEM_CORROBORATION: Modern religious movements, social reform organizations, and academic scholars of religion (outside traditional Brahminical institutions) corroborate the historical and ongoing nature of these problems, highlighting the persistent tension between universalist ideals and hierarchical social structures in South Asian religious contexts.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__universalist_devotional_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__universalist_devotional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(gita_kurukshetra_discourse__universalist_devotional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__universalist_devotional_reading_tests).
:- end_tests(gita_kurukshetra_discourse__universalist_devotional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint's metrics reflect its nature as an interpretive framework that liberates rather than extracts. Extractiveness (0.15) and suppression (0.10) are low because the reading itself does not impose costs or coerce; instead, it offers a path of spiritual freedom. Theater ratio is negligible (0.05) as its function is genuine theological reinterpretation, not performance. Accessibility collapse is low (0.10) because it actively expands, rather than restricts, spiritual alternatives. However, resistance is high (0.80) because this reading directly challenges deeply entrenched social and religious hierarchies, provoking strong opposition from those who benefit from traditional interpretations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of universal devotees and marginalized castes, this reading is a liberating 'rope' that offers spiritual access and redefines duty in an empowering way. For traditional Brahminical authority and orthodox scholars, it is a profound threat that undermines their institutional power and interpretive legitimacy, effectively forcing them to 'pay' in terms of lost influence. The engine's classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Universal devotees and marginalized castes are clear beneficiaries, gaining spiritual access and validation (low d). Gita scholars promoting this reading act as agenda-setters, shaping the discourse to benefit these groups. Traditional Brahminical authority and orthodox scholars are effectively 'payers' or targets of this reading's challenge, as it diminishes their exclusive claims and interpretive power (high d). Rival interpretive communities, like the Gandhian allegorical scholars, are observers who find partial alignment.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the typical sense, as it is a living interpretive tradition. Its mandate is to provide a spiritual path and ethical framework. The contestation it faces is not due to its function atrophying, but rather due to its function actively challenging an existing, extractive social order.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_authority_legitimacy,
    'Does the universalist devotional reading successfully displace traditional Brahminical authority as the primary legitimate interpreter of the Gita, or does it merely create a parallel, contested interpretive stream?',
    'Sociological studies of religious practice and authority over time, tracking adherence to different interpretive schools and their influence on social structures.',
    'If it achieves displacement, the ''payer'' status of traditional authority is more severe, indicating a fundamental shift in religious power. If it remains a parallel stream, the contestation is ongoing, and the ''payer'' status reflects a continuous struggle for legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_legitimacy, empirical, 'The extent to which this reading reconfigures religious authority.').

omega_variable(
    dharma_redefinition_acceptance,
    'To what extent is the redefinition of dharma as ''surrender to divine will'' (rather than social role) genuinely adopted and practiced by adherents, particularly those from traditionally privileged castes?',
    'Qualitative and quantitative studies of religious ethics and practice among diverse Hindu communities, examining how individuals articulate and enact their understanding of dharma.',
    'If widely adopted across all social strata, the reading''s transformative power is confirmed, reinforcing its ''rope'' classification. If adoption is limited, particularly among privileged groups, its impact on social structures is less profound, and its ''resistance'' metric might be higher due to unacknowledged adherence to older norms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dharma_redefinition_acceptance, empirical, 'The practical acceptance of dharma''s redefinition.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct, coherent reading of the Gita kernel, or is it merely a variant of another reading (e.g., the Gandhian allegorical reading)?',
    'Detailed textual analysis comparing the core hermeneutical principles and ethical implications of this reading against its siblings, identifying unique foundational axioms.',
    'If it is a distinct reading, its independent classification as a ''rope'' is robust. If it is a variant, its classification might be subsumed under a broader ''parent'' reading, or its ''coexists_with'' relation to the Gandhian reading might shift to ''influences'' or ''forecloses'' depending on the degree of overlap.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms the distinct identity of this Gita reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__universalist_devotional_reading, 1920, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t1920, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 1920, 0.05).
narrative_ontology:measurement(gita_tr_t1940, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 1940, 0.05).
narrative_ontology:measurement(gita_tr_t1960, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(gita_tr_t1980, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(gita_tr_t2000, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(gita_tr_t2020, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 2020, 0.05).

% Extraction over time
narrative_ontology:measurement(gita_be_t1920, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 1920, 0.15).
narrative_ontology:measurement(gita_be_t1940, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 1940, 0.15).
narrative_ontology:measurement(gita_be_t1960, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 1960, 0.15).
narrative_ontology:measurement(gita_be_t1980, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 1980, 0.15).
narrative_ontology:measurement(gita_be_t2000, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(gita_be_t2020, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 2020, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t1920, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 1920, 0.1).
narrative_ontology:measurement(gita_su_t1940, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 1940, 0.1).
narrative_ontology:measurement(gita_su_t1960, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 1960, 0.1).
narrative_ontology:measurement(gita_su_t1980, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 1980, 0.1).
narrative_ontology:measurement(gita_su_t2000, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 2000, 0.1).
narrative_ontology:measurement(gita_su_t2020, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 2020, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__universalist_devotional_reading, identity_coordination).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse__orthodox_literal_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse__gandhian_allegorical_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'gita_kurukshetra_discourse' kernel. Each reading instantiates a different constraint with unique structural properties and ε values. They are linked here to reflect their shared textual origin and interpretive contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
