% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__reformist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__reformist_reading, []).

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
 *   constraint_id: constitutional_secularism__reformist_reading
 *   human_readable: Reformist Reading of Constitutional Secularism: Affirmative Duty to Eliminate Oppressive Religious Practices
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint represents the 'reformist reading' of constitutional
 *   secularism, where the state has an affirmative and overriding duty to
 *   intervene in religious practices that oppress marginalized groups. This
 *   reading prioritizes social justice and equality over claims of religious
 *   autonomy, leading to significant extraction from traditionalist religious
 *   communities and leaders. It is one reading of the
 *   'constitutional_secularism' kernel, distinct from
 *   'strict_neutrality_reading' and 'principled_intervention_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, 0.85).
domain_priors:suppression_score(constitutional_secularism__reformist_reading, 0.75).
domain_priors:theater_ratio(constitutional_secularism__reformist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__reformist_reading, snare).
narrative_ontology:human_readable(constitutional_secularism__reformist_reading, "Reformist Reading of Constitutional Secularism: Affirmative Duty to Eliminate Oppressive Religious Practices").
narrative_ontology:topic_domain(constitutional_secularism__reformist_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__reformist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__reformist_reading, '4e5e854c-81f2-4439-bf08-9daff35f4497').
narrative_ontology:cs_kernel_codification('4e5e854c-81f2-4439-bf08-9daff35f4497', formalized).
narrative_ontology:cs_authority_grounding('4e5e854c-81f2-4439-bf08-9daff35f4497', lineage).
narrative_ontology:cs_interpretation_layer_present('4e5e854c-81f2-4439-bf08-9daff35f4497').
narrative_ontology:cs_reading_relation('4e5e854c-81f2-4439-bf08-9daff35f4497', constitutional_secularism__strict_neutrality_reading, forecloses).
narrative_ontology:cs_reading_relation('4e5e854c-81f2-4439-bf08-9daff35f4497', constitutional_secularism__principled_intervention_reading, influences).
narrative_ontology:cs_axiom('4e5e854c-81f2-4439-bf08-9daff35f4497', foundational, state_has_affirmative_duty_to_reform_religion).
narrative_ontology:cs_axiom_status(state_has_affirmative_duty_to_reform_religion, holdable).
narrative_ontology:cs_axiom_grounding('4e5e854c-81f2-4439-bf08-9daff35f4497', state_has_affirmative_duty_to_reform_religion, deontological).
narrative_ontology:cs_axiom('4e5e854c-81f2-4439-bf08-9daff35f4497', foundational, social_justice_supersedes_religious_autonomy).
narrative_ontology:cs_axiom_status(social_justice_supersedes_religious_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('4e5e854c-81f2-4439-bf08-9daff35f4497', social_justice_supersedes_religious_autonomy, deontological).
narrative_ontology:cs_reference_frame('4e5e854c-81f2-4439-bf08-9daff35f4497', constitutional_equality_mandate).
narrative_ontology:cs_drift_state('4e5e854c-81f2-4439-bf08-9daff35f4497', contemporary_global_human_rights_discourse, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('4e5e854c-81f2-4439-bf08-9daff35f4497', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__reformist_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, marginalized_religious_groups).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, women_within_religious_communities).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, scheduled_castes).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, religious_conservative_leaders).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, traditionalist_religious_communities).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, religious_autonomy_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the constitutional duty to eliminate oppressive religious practices, often through legislation or judicial rulings that override religious personal laws or customs. Bears the political cost of resistance from traditionalist groups.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, state_judiciary_legislature, agenda_setter,
    institutional, generational, constrained, national).

% Are the primary beneficiaries of state intervention, as it aims to dismantle discriminatory practices within their communities, such as caste-based exclusion or gender inequality in religious institutions. Their agency is often limited, relying on state action for reform.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, marginalized_religious_groups, beneficiary,
    powerless, biographical, trapped, local).

% Benefit from reforms targeting gender-discriminatory religious practices (e.g., unequal inheritance, divorce laws). Their identity is often deeply tied to their religious community, making exit difficult, so state intervention is a crucial lever for change.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, women_within_religious_communities, beneficiary,
    powerless, biographical, identity_locked, local).

% Benefit from state action against caste-based discrimination and untouchability practices, which are often religiously sanctioned. Their historical oppression makes state protection vital, but social mobility remains constrained.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, scheduled_castes, beneficiary,
    powerless, generational, constrained, regional).

% Bear the direct costs of state intervention, as their authority and traditional practices are challenged or overturned. They mobilize resistance, claiming infringement on religious freedom and cultural autonomy. Their power is rooted in community adherence to tradition.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, religious_conservative_leaders, payer,
    organized, biographical, constrained, national).

% Experience the constraint as an imposition on their way of life and religious identity. They may resist reforms, viewing them as external interference in sacred matters. Their identity is often fused with their religious practices, making compliance a deep personal cost.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, traditionalist_religious_communities, payer,
    moderate, generational, identity_locked, local).

% Argue for a broader interpretation of religious freedom that prioritizes non-interference, even if it means tolerating some internal inequalities. They are often sidelined in the reformist discourse, which prioritizes social justice over absolute religious autonomy.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, religious_autonomy_advocates, excluded,
    powerful, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate diverse religious communities under a common framework of constitutional values, ensuring that religious practices do not violate fundamental rights, particularly for vulnerable groups. It seeks to establish a baseline of social justice across religious lines.
% TRANSFER_FUNCTION: Transfers authority over certain aspects of religious practice from religious institutions and traditional leaders to the secular state, in exchange for enhanced rights and protections for marginalized individuals within those communities.
% ABSENT_VOICES: Strict religious autonomy advocates, who believe the state should not interfere in religious matters even for social reform, are largely excluded from the policy-making process, as their core premise is superseded by the reformist duty. Their arguments are heard in courts but often overridden by the state's affirmative duty.
% DISAPPEARANCE_RATIONALE: If this affirmative duty vanished, many oppressive religious practices would likely reassert themselves, particularly against women and scheduled castes, leading to a rollback of social reforms and a significant increase in internal community inequalities. The state's role in protecting fundamental rights within religious spheres would collapse.
% FOUNDING_PROBLEM: Religious personal laws and traditional practices often enshrined discrimination against women and lower castes, creating systems of oppression that contradicted the constitutional guarantees of equality and dignity.
% FOUNDING_PROBLEM_CORROBORATION: Human rights organizations, women's rights activists, and scholars of social justice consistently corroborate that the founding problems of religious discrimination and oppression remain live, requiring ongoing state intervention. Reports from UN bodies and national commissions also attest to the persistence of these issues, providing corroboration from outside the direct beneficiaries.
narrative_ontology:disappearance_verdict(constitutional_secularism__reformist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__reformist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__reformist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(constitutional_secularism__reformist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__reformist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__reformist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__reformist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.85) because this reading actively reconfigures religious authority and practice, imposing secular constitutional norms. Suppression (0.75) is also high, as the state must actively enforce these reforms against significant resistance from traditionalist groups. Resistance (0.9) is very high, reflecting the deep-seated opposition to state interference in religious matters. Accessibility collapse is moderate (0.4) because while the state imposes reforms, religious communities still retain significant autonomy in other areas, and exit from religious identity is complex. Theater ratio is low (0.1) as the state's actions are direct and functional, not performative, in their aim to achieve social reform.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiaries experience this as a liberating force, while the victims experience it as an oppressive imposition. The state, as agenda-setter, views it as fulfilling a constitutional mandate. The engine's per-seat classification will reflect these divergent experiences based on their structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The state (judiciary/legislature) acts as the agenda-setter, directing the constraint. Marginalized religious groups, women, and scheduled castes are clear beneficiaries, as the constraint aims to liberate them from oppressive practices. Religious conservative leaders and traditionalist communities are the primary victims, experiencing direct extraction of their traditional authority and practices. Religious autonomy advocates are excluded, as their core premise is superseded by this reading's emphasis on social justice.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_state_intervention,
    'Does the state''s affirmative duty to eliminate oppressive religious practices genuinely derive from constitutional principles, or is it an overreach driven by a particular ideological interpretation?',
    'Comparative constitutional analysis across diverse secular states, historical legal scholarship on the evolution of secularism, and public discourse analysis on the acceptance of such interventions.',
    'If deemed an overreach, the constraint''s legitimacy would be undermined, potentially increasing resistance and reducing its effective suppression. If strongly affirmed, it would solidify the state''s authority in this domain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_state_intervention, conceptual, 'Ambiguity regarding the foundational legitimacy of the state''s expansive role in religious reform.').

omega_variable(
    unintended_consequences_of_reform,
    'Does state intervention, even with reformist intent, lead to unintended consequences such as the alienation of religious minorities, the strengthening of fundamentalist factions, or the erosion of genuine religious pluralism?',
    'Longitudinal sociological studies of communities affected by reforms, analysis of political mobilization patterns among religious groups, and comparative studies of state-religion models.',
    'If significant negative unintended consequences are demonstrated, it could lead to a re-evaluation of the reformist reading, potentially shifting towards a more cautious ''principled_intervention'' approach or even a ''strict_neutrality'' stance, thereby reducing extractiveness and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unintended_consequences_of_reform, empirical, 'Potential for state-led religious reform to produce adverse social and political outcomes.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (legal prohibitions, state enforcement) or internalized (social pressure, fear of state reprisal) within traditionalist communities?',
    'Post-reform community studies: if traditional practices persist covertly despite legal prohibitions, it suggests a higher degree of internalized suppression that is harder to dislodge.',
    'If internalized suppression is dominant, the constraint''s effective suppression is higher than the structural measure suggests, indicating deeper resistance and the need for more nuanced, community-led reform rather than top-down enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in religious communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__reformist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_secularism__reformist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cons_tr_t10, constitutional_secularism__reformist_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(cons_tr_t20, constitutional_secularism__reformist_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(cons_tr_t30, constitutional_secularism__reformist_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement(cons_tr_t40, constitutional_secularism__reformist_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(cons_tr_t50, constitutional_secularism__reformist_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_secularism__reformist_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(cons_be_t10, constitutional_secularism__reformist_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(cons_be_t20, constitutional_secularism__reformist_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(cons_be_t30, constitutional_secularism__reformist_reading, base_extractiveness, 30, 0.83).
narrative_ontology:measurement(cons_be_t40, constitutional_secularism__reformist_reading, base_extractiveness, 40, 0.84).
narrative_ontology:measurement(cons_be_t50, constitutional_secularism__reformist_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_secularism__reformist_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(cons_su_t10, constitutional_secularism__reformist_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(cons_su_t20, constitutional_secularism__reformist_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(cons_su_t30, constitutional_secularism__reformist_reading, suppression_requirement, 30, 0.73).
narrative_ontology:measurement(cons_su_t40, constitutional_secularism__reformist_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(cons_su_t50, constitutional_secularism__reformist_reading, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
