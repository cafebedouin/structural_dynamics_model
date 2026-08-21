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
 *   constraint_id: constitutional_secularism__reformist_reading
 *   human_readable: Reformist Reading of Constitutional Secularism: Affirmative Duty to Eliminate Oppressive Religious Practices
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint represents the 'reformist reading' of constitutional
 *   secularism, which posits an affirmative duty for the state to intervene
 *   in and eliminate religious practices that oppress marginalized groups,
 *   even if it supersedes claims of religious autonomy. This reading is
 *   highly extractive from religious conservative leaders and traditional
 *   institutions, while benefiting marginalized groups. It is a Snare due to
 *   its high extraction and active suppression of alternative religious
 *   autonomy claims, despite its stated coordination function of social
 *   justice. The claimed type 'snare' reflects the structural reality of this
 *   reading, independent of its normative goals.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, 0.85).
domain_priors:suppression_score(constitutional_secularism__reformist_reading, 0.78).
domain_priors:theater_ratio(constitutional_secularism__reformist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__reformist_reading, snare).
narrative_ontology:human_readable(constitutional_secularism__reformist_reading, "Reformist Reading of Constitutional Secularism: Affirmative Duty to Eliminate Oppressive Religious Practices").
narrative_ontology:topic_domain(constitutional_secularism__reformist_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__reformist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__reformist_reading, 'd2183c7a-b519-4cf6-a1a5-0d933f92111d').
narrative_ontology:cs_kernel_codification('d2183c7a-b519-4cf6-a1a5-0d933f92111d', formalized).
narrative_ontology:cs_authority_grounding('d2183c7a-b519-4cf6-a1a5-0d933f92111d', lineage).
narrative_ontology:cs_interpretation_layer_present('d2183c7a-b519-4cf6-a1a5-0d933f92111d').
narrative_ontology:cs_reading_relation('d2183c7a-b519-4cf6-a1a5-0d933f92111d', constitutional_secularism__strict_neutrality_reading, forecloses).
narrative_ontology:cs_reading_relation('d2183c7a-b519-4cf6-a1a5-0d933f92111d', constitutional_secularism__principled_intervention_reading, influences).
narrative_ontology:cs_axiom('d2183c7a-b519-4cf6-a1a5-0d933f92111d', foundational, state_has_affirmative_duty_to_reform_religion).
narrative_ontology:cs_axiom_status(state_has_affirmative_duty_to_reform_religion, holdable).
narrative_ontology:cs_axiom_grounding('d2183c7a-b519-4cf6-a1a5-0d933f92111d', state_has_affirmative_duty_to_reform_religion, deontological).
narrative_ontology:cs_axiom('d2183c7a-b519-4cf6-a1a5-0d933f92111d', foundational, social_justice_supersedes_religious_autonomy).
narrative_ontology:cs_axiom_status(social_justice_supersedes_religious_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('d2183c7a-b519-4cf6-a1a5-0d933f92111d', social_justice_supersedes_religious_autonomy, deontological).
narrative_ontology:cs_reference_frame('d2183c7a-b519-4cf6-a1a5-0d933f92111d', constitutional_commitment_to_social_equality).
narrative_ontology:cs_drift_state('d2183c7a-b519-4cf6-a1a5-0d933f92111d', contemporary_global_human_rights_discourse, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('d2183c7a-b519-4cf6-a1a5-0d933f92111d', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__reformist_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, marginalized_religious_groups).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, women_within_religious_communities).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, scheduled_castes).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, religious_conservative_leaders).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, traditional_religious_institutions).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, religious_autonomy_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the constitution to mandate state intervention in religious practices deemed oppressive. Issues rulings that compel changes in religious personal laws and community norms. Bears the political cost of resistance from religious groups.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, state_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Are the primary beneficiaries of state intervention, as it aims to dismantle discriminatory practices within their communities. Their ability to exit oppressive structures is enhanced by state action, but they remain vulnerable to backlash.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, marginalized_religious_groups, beneficiary,
    powerless, biographical, trapped, local).

% Benefit from reforms to personal laws and social norms that grant them greater equality and autonomy. Their agency is increased, but they often face social pressure and resistance from traditional elements within their communities.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, women_within_religious_communities, beneficiary,
    moderate, biographical, constrained, local).

% Benefit from state action against caste-based discrimination and exclusion within religious institutions and practices. Their historical oppression is directly addressed, but systemic change is slow and contested.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, scheduled_castes, beneficiary,
    organized, generational, constrained, regional).

% Bear the direct costs of state intervention, as their authority and traditional practices are challenged or overturned. They resist these reforms through legal challenges, political lobbying, and community mobilization, viewing them as an infringement on religious freedom.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, religious_conservative_leaders, payer,
    powerful, generational, constrained, national).

% Are forced to alter long-standing practices and doctrines to comply with state mandates. Their institutional identity is often deeply intertwined with these traditions, making compliance a profound challenge to their self-conception and legitimacy. Exit means dissolution or loss of influence.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, traditional_religious_institutions, payer,
    institutional, civilizational, identity_locked, national).

% Argue for a broad interpretation of religious freedom that limits state interference. They bear the cost of diminished religious autonomy and mobilize public opinion against state overreach, often seeking international support.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, religious_autonomy_advocates, payer,
    organized, generational, mobile, global).

% Would argue that the state's affirmative duty violates the principle of equal distance from all religions, leading to preferential treatment or discrimination. Their voice is often marginalized in the reformist discourse, which prioritizes social justice over strict non-interference.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, strict_neutrality_advocates, excluded,
    moderate, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate diverse religious communities under a common framework of constitutional values, ensuring that religious practices do not violate fundamental rights or perpetuate social hierarchies.
% TRANSFER_FUNCTION: Transfers authority over certain religious practices from religious institutions and leaders to the state, and transfers social power from dominant groups within religious communities to marginalized groups.
% ABSENT_VOICES: Advocates for strict state neutrality or minimal intervention are often excluded from the reformist discourse, which frames non-intervention as complicity in oppression. Their arguments for religious freedom as a primary value are sidelined.
% DISAPPEARANCE_RATIONALE: If the state's affirmative duty vanished, many oppressive religious practices would likely reassert themselves or continue unchecked, leading to a reversal of social reforms and a re-entrenchment of marginalization for vulnerable groups. The legal and social landscape would shift significantly.
% FOUNDING_PROBLEM: Religious practices and personal laws in a diverse society often perpetuate discrimination, inequality, and oppression against marginalized groups, particularly women and lower castes, under the guise of religious freedom.
% FOUNDING_PROBLEM_CORROBORATION: Human rights organizations, women's rights activists, and caste-based advocacy groups consistently attest that the problem of oppressive religious practices remains live and requires state intervention. Academic studies and judicial pronouncements also corroborate the ongoing nature of these issues, from outside the directly benefiting parties.
narrative_ontology:disappearance_verdict(constitutional_secularism__reformist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__reformist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__reformist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.85) is high because it compels significant changes in deeply held religious practices and transfers substantial authority from religious bodies to the state. Suppression (0.78) is also high, as the state actively enforces these reforms, often against strong resistance, by legal and administrative means. The theater ratio is low (0.1) because the state's actions are genuinely aimed at reform, not merely performative. Accessibility collapse is moderate (0.65) as religious groups still have avenues for resistance and legal challenge, but direct non-compliance is not an option. Resistance is high (0.8) due to the profound impact on religious identity and autonomy.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of beneficiaries, this is a necessary intervention for justice. From the perspective of religious conservatives, it is an oppressive overreach by the state. The engine's classification as a Snare reflects the structural reality of high extraction and suppression, regardless of the normative justification.
 *
 * DIRECTIONALITY LOGIC:
 *   The state judiciary (agenda_setter) is the primary enforcer. Marginalized groups, women, and scheduled castes are clear beneficiaries, experiencing reduced oppression. Religious conservative leaders and traditional institutions are direct targets, bearing the costs of enforced change. Religious autonomy advocates are also targets, as their core principle is superseded. Strict neutrality advocates are excluded, as their framing is deemed insufficient to address the problem of oppression.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (eliminating oppression) is very much live, and its persistence is driven by the ongoing need for social reform, not by inertia. The classification as a Snare prevents mislabeling it as a Rope or Scaffold, which would imply a more balanced coordination or a temporary support structure, neither of which accurately describes the coercive and extractive nature of this specific reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_oppression_definition,
    'How broadly or narrowly is ''oppressive religious practice'' defined, and who holds the authority to define it?',
    'Analysis of judicial precedents and legislative definitions over time; examination of which social groups'' interpretations are privileged in state policy.',
    'A broad, state-centric definition increases extractiveness and suppression, potentially leading to overreach. A narrow, community-negotiated definition might reduce extraction but risk under-addressing oppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_oppression_definition, conceptual, 'Ambiguity in the definition of ''oppressive religious practice'' and the locus of definitional authority.').

omega_variable(
    unintended_consequences_of_intervention,
    'Does state intervention, even with good intentions, inadvertently strengthen conservative elements or lead to new forms of social fragmentation and resistance?',
    'Longitudinal sociological studies tracking community responses, internal power dynamics, and the emergence of new forms of resistance or identity consolidation post-intervention.',
    'If unintended negative consequences are substantial, the effective cost of the constraint (and thus its extractiveness) might be higher than intended, potentially shifting its classification towards a more entrenched Snare or even a Piton if the original mandate is undermined.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unintended_consequences_of_intervention, empirical, 'Potential for state intervention to produce unintended negative social or political consequences.').

omega_variable(
    balancing_religious_autonomy_and_social_justice,
    'Is there an irreducible tension between robust religious autonomy and the state''s affirmative duty to ensure social justice within religious communities, or can a harmonious balance be achieved?',
    'Philosophical and legal scholarship exploring theoretical frameworks for reconciliation; comparative analysis of constitutional models that attempt to integrate these values.',
    'If the tension is irreducible, the constraint will always operate with high extractiveness on one side or the other, making a ''Rope'' classification unlikely. If reconciliation is possible, pathways to reduce suppression and extraction might emerge.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(balancing_religious_autonomy_and_social_justice, preference, 'Fundamental tension between religious autonomy and social justice as constitutional values.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__reformist_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(cons_be_t1950, constitutional_secularism__reformist_reading, base_extractiveness, 1950, 0.6).
narrative_ontology:measurement(cons_be_t1970, constitutional_secularism__reformist_reading, base_extractiveness, 1970, 0.7).
narrative_ontology:measurement(cons_be_t1990, constitutional_secularism__reformist_reading, base_extractiveness, 1990, 0.78).
narrative_ontology:measurement(cons_be_t2010, constitutional_secularism__reformist_reading, base_extractiveness, 2010, 0.82).
narrative_ontology:measurement(cons_be_t2024, constitutional_secularism__reformist_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1950, constitutional_secularism__reformist_reading, suppression_requirement, 1950, 0.5).
narrative_ontology:measurement(cons_su_t1970, constitutional_secularism__reformist_reading, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(cons_su_t1990, constitutional_secularism__reformist_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(cons_su_t2010, constitutional_secularism__reformist_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(cons_su_t2024, constitutional_secularism__reformist_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__reformist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, constitutional_secularism__strict_neutrality_reading).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, constitutional_secularism__principled_intervention_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'constitutional_secularism' kernel. This 'reformist reading' is the most assertive, positing an affirmative duty to intervene, which leads to the highest extraction on religious autonomy claims among the three. It directly influences the operational space of the other readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
