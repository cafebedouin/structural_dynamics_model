% ============================================================================
% CONSTRAINT STORY: udhr_article_3__positive_entitlement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__positive_entitlement_reading, []).

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
 *   constraint_id: udhr_article_3__positive_entitlement_reading
 *   human_readable: UDHR Article 3: Positive Entitlement to Life and Security
 *   domain: constitutional_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'positive entitlement' reading of Article
 *   3 of the Universal Declaration of Human Rights (UDHR), which interprets
 *   'life, liberty and security of person' as obligating states to actively
 *   provide material conditions such as welfare, healthcare, and housing.
 *   This reading leads to high extraction through wealth redistribution and
 *   restrictions on certain liberties, primarily benefiting vulnerable groups
 *   and human rights advocates, while imposing costs on property owners and
 *   those advocating for unrestricted expression. The constraint is claimed
 *   as a 'tangled_rope' due to its genuine coordination function (social
 *   safety net) intertwined with significant asymmetric extraction and active
 *   enforcement.
 *
 * KEY AGENTS:
 *   - vulnerable_citizens: Primary beneficiary (powerless/trapped) — receives state provisions.
 *   - human_rights_advocates: Agenda setter (organized/constrained) — pushes for this interpretation and its enforcement.
 *   - property_rights_advocates: Primary payer (powerful/constrained) — bears costs of redistribution.
 *   - unrestricted_expression_advocates: Payer (moderate/constrained) — faces restrictions on speech.
 *   - taxpayers: Payer (organized/mobile) — funds state provisions.
 *   - state_institutions: Agenda setter (institutional/identity_locked) — implements and enforces policies.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, 0.78).
domain_priors:suppression_score(udhr_article_3__positive_entitlement_reading, 0.65).
domain_priors:theater_ratio(udhr_article_3__positive_entitlement_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__positive_entitlement_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__positive_entitlement_reading, "UDHR Article 3: Positive Entitlement to Life and Security").
narrative_ontology:topic_domain(udhr_article_3__positive_entitlement_reading, "constitutional_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__positive_entitlement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__positive_entitlement_reading, '75d8f4be-f232-49ad-836c-5b428845108a').
narrative_ontology:cs_kernel_codification('75d8f4be-f232-49ad-836c-5b428845108a', fixed_text).
narrative_ontology:cs_authority_grounding('75d8f4be-f232-49ad-836c-5b428845108a', lineage).
narrative_ontology:cs_interpretation_layer_present('75d8f4be-f232-49ad-836c-5b428845108a').
narrative_ontology:cs_reading_relation('75d8f4be-f232-49ad-836c-5b428845108a', udhr_article_3__negative_liberty_reading, coexists_with).
narrative_ontology:cs_reading_relation('75d8f4be-f232-49ad-836c-5b428845108a', udhr_article_3__procedural_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('75d8f4be-f232-49ad-836c-5b428845108a', foundational, state_has_positive_obligations).
narrative_ontology:cs_axiom_status(state_has_positive_obligations, holdable).
narrative_ontology:cs_axiom_grounding('75d8f4be-f232-49ad-836c-5b428845108a', state_has_positive_obligations, deontological).
narrative_ontology:cs_axiom('75d8f4be-f232-49ad-836c-5b428845108a', foundational, material_conditions_are_rights).
narrative_ontology:cs_axiom_status(material_conditions_are_rights, holdable).
narrative_ontology:cs_axiom_grounding('75d8f4be-f232-49ad-836c-5b428845108a', material_conditions_are_rights, deontological).
narrative_ontology:cs_reference_frame('75d8f4be-f232-49ad-836c-5b428845108a', post_wwii_social_justice_framework).
narrative_ontology:cs_drift_state('75d8f4be-f232-49ad-836c-5b428845108a', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('75d8f4be-f232-49ad-836c-5b428845108a', '').
narrative_ontology:cs_kernel_id(udhr_article_3__positive_entitlement_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, vulnerable_citizens).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, human_rights_advocates).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, property_rights_advocates).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, unrestricted_expression_advocates).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives state-provided welfare, healthcare, and housing, which are deemed necessary for their life and security. Their well-being is directly tied to the state's active provision of these conditions.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, vulnerable_citizens, beneficiary,
    powerless, immediate, trapped, national).

% Actively lobby for the interpretation and enforcement of Article 3 as a positive entitlement, pushing for legislative and judicial action to expand state provision of material conditions. They benefit from the expansion of human rights discourse and legal frameworks.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, human_rights_advocates, agenda_setter,
    organized, generational, constrained, global).

% Bear the costs of wealth redistribution and state intervention in markets required to fund positive entitlements. They argue that such measures infringe on fundamental property rights and economic liberties.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, property_rights_advocates, payer,
    powerful, generational, constrained, national).

% Experience restrictions on certain forms of expression (e.g., hate speech) justified by the need to protect the security and dignity of vulnerable groups. They argue for broader free speech protections.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, unrestricted_expression_advocates, payer,
    moderate, biographical, constrained, national).

% Fund the state's provision of welfare, healthcare, and housing through taxation. While some may support these provisions, others bear the financial burden without directly benefiting or actively resisting.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, taxpayers, payer,
    organized, biographical, mobile, national).

% Are obligated to implement policies and allocate resources to fulfill the positive entitlements. They interpret and enforce the reading, balancing competing claims and managing the administrative burden. Their legitimacy is tied to upholding human rights norms.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, state_institutions, agenda_setter,
    institutional, civilizational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state action and resource allocation to ensure a baseline of material conditions (welfare, healthcare, housing) for all citizens, preventing destitution and promoting social stability.
% TRANSFER_FUNCTION: Transfers wealth and resources from higher-income individuals and corporations (via taxation) to vulnerable citizens, and restricts certain liberties (e.g., property use, speech) to secure collective well-being.
% ABSENT_VOICES: Those who believe in minimal state intervention and absolute individual liberty are often marginalized in the discourse, arguing that such entitlements create dependency and stifle individual initiative.
% DISAPPEARANCE_RATIONALE: If this reading of Article 3 vanished, states would likely retract many welfare provisions, leading to increased poverty, health crises, and social instability for vulnerable populations. The legal and political landscape around human rights would fundamentally shift, requiring a complete reorganization of social safety nets.
% FOUNDING_PROBLEM: The historical problem of widespread poverty, lack of access to basic necessities, and vulnerability to economic and social shocks, particularly after major conflicts, which undermined human dignity and security.
% FOUNDING_PROBLEM_CORROBORATION: International human rights bodies, NGOs, and academic researchers consistently attest to the ongoing global challenges of poverty, inadequate healthcare, and housing, corroborating that the founding problem remains live. While some economists and political theorists from outside the advocate groups contest the efficacy of state-led provision, the existence of the problem itself is widely acknowledged.
narrative_ontology:disappearance_verdict(udhr_article_3__positive_entitlement_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__positive_entitlement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__positive_entitlement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(udhr_article_3__positive_entitlement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__positive_entitlement_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__positive_entitlement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_article_3__positive_entitlement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.78) reflects the significant transfer of resources and restrictions on liberties required to fulfill positive entitlements. Suppression (0.65) is necessary to overcome resistance from those whose property or expressive freedoms are curtailed. The theater ratio (0.40) indicates that while genuine provision occurs, a substantial portion of the discourse and enforcement is performative, defending the expansive interpretation against challenges. The rising extractiveness and suppression over time reflect the increasing scope and enforcement of this reading since 1948.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of vulnerable citizens and human rights advocates, this reading is a vital 'rope' ensuring basic dignity and security. From the perspective of property rights and unrestricted expression advocates, it operates as a 'snare' that extracts wealth and curtails fundamental freedoms. State institutions, while acting as agenda setters, are identity-locked into upholding human rights norms, making exit from this interpretative framework difficult.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable citizens are full beneficiaries (d=0.0) as they receive direct provisions. Human rights advocates are also beneficiaries (d low) as their agenda is advanced. Property rights and unrestricted expression advocates are targets (d high) as they bear the direct costs and restrictions. Taxpayers are payers (d high-moderate). State institutions, while enforcing, are also identity-locked into the framework, making their directionality complex but leaning towards beneficiary of the legitimacy it confers.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure 'snare' by acknowledging its genuine coordination function in providing a social safety net. However, it also prevents mislabeling it as a pure 'rope' by highlighting the significant, actively enforced extraction from specific groups. The 'tangled_rope' classification captures the hybrid nature where the coordination story (ensuring basic rights) is used to justify substantial transfers and restrictions that benefit some at the expense of others, requiring continuous enforcement to maintain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_positive_entitlement,
    'What is the precise scope of ''material conditions necessary for life and security'' and how does it evolve with societal development?',
    'Judicial precedent, legislative action, and international consensus on minimum standards for human dignity and well-being.',
    'A narrower interpretation would reduce extractiveness and suppression, potentially shifting the classification towards a ''rope'' or ''scaffold''. A broader interpretation would increase extraction and suppression, pushing it closer to a ''snare''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_positive_entitlement, conceptual, 'Ambiguity in the definition of positive entitlements.').

omega_variable(
    efficacy_of_state_provision,
    'Are state-provided material conditions the most effective and least extractive means to ensure life and security, compared to market-based or community-led alternatives?',
    'Comparative empirical studies of different welfare models across jurisdictions, assessing outcomes for vulnerable populations and overall economic efficiency.',
    'If alternatives are found to be more effective and less extractive, the justification for the current level of state intervention would weaken, potentially leading to policy shifts that reduce the constraint''s extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_of_state_provision, empirical, 'Whether state provision is the optimal mechanism for achieving the stated goals.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine positive entitlement, or a constructed interpretation that benefits identifiable agents?',
    'Analysis of the historical drafting records of the UDHR, philosophical arguments regarding natural rights vs. positive rights, and the political economy of human rights advocacy.',
    'If primarily a constructed interpretation, the ''tangled_rope'' classification would be reinforced, highlighting the political contestation over its meaning. If genuinely inherent, it would lean towards a ''mountain'' of moral philosophy, though still with beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is one reading of the UDHR Article 3 kernel. This ''positive_entitlement_reading'' asserts an active state obligation. A sibling ''negative_liberty_reading'' would focus on freedom from state interference, while a ''procedural_hybrid_reading'' would emphasize due process. The disagreement is located in the fundamental nature of ''rights'' (positive vs. negative) and the role of the state.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__positive_entitlement_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t1948, udhr_article_3__positive_entitlement_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(udhr_tr_t1968, udhr_article_3__positive_entitlement_reading, theater_ratio, 1968, 0.2).
narrative_ontology:measurement(udhr_tr_t1988, udhr_article_3__positive_entitlement_reading, theater_ratio, 1988, 0.3).
narrative_ontology:measurement(udhr_tr_t2008, udhr_article_3__positive_entitlement_reading, theater_ratio, 2008, 0.35).
narrative_ontology:measurement(udhr_tr_t2024, udhr_article_3__positive_entitlement_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(udhr_be_t1948, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1948, 0.4).
narrative_ontology:measurement(udhr_be_t1968, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1968, 0.55).
narrative_ontology:measurement(udhr_be_t1988, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1988, 0.68).
narrative_ontology:measurement(udhr_be_t2008, udhr_article_3__positive_entitlement_reading, base_extractiveness, 2008, 0.75).
narrative_ontology:measurement(udhr_be_t2024, udhr_article_3__positive_entitlement_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t1948, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1948, 0.3).
narrative_ontology:measurement(udhr_su_t1968, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1968, 0.45).
narrative_ontology:measurement(udhr_su_t1988, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1988, 0.58).
narrative_ontology:measurement(udhr_su_t2008, udhr_article_3__positive_entitlement_reading, suppression_requirement, 2008, 0.62).
narrative_ontology:measurement(udhr_su_t2024, udhr_article_3__positive_entitlement_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__positive_entitlement_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, udhr_article_3__negative_liberty_reading).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, udhr_article_3__procedural_hybrid_reading).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, national_welfare_legislation).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, hate_speech_laws).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the UDHR Article 3 kernel. Each reading has a different structural profile and classification. This 'positive_entitlement_reading' emphasizes state obligation for material conditions, contrasting with 'negative_liberty_reading' (freedom from state interference) and 'procedural_hybrid_reading' (due process guarantees).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
