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
 *   constraint_id: udhr_article_3__positive_entitlement_reading
 *   human_readable: UDHR Article 3: Positive Entitlement to Material Conditions
 *   domain: constitutional_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'positive entitlement' reading of Article
 *   3 of the Universal Declaration of Human Rights (UDHR), which interprets
 *   'life, liberty and security of person' as obligating states to actively
 *   provide material conditions such as welfare, healthcare, and housing.
 *   This reading also justifies restrictions on certain liberties (e.g., hate
 *   speech) to ensure collective security. The high extractiveness and
 *   suppression reflect the significant state intervention and resource
 *   redistribution required, as well as the curtailment of competing rights
 *   claims. The claimed type is 'tangled_rope' due to its genuine
 *   coordination function (ensuring basic welfare) coupled with substantial,
 *   actively enforced extraction from taxpayers and property owners.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, 0.75).
domain_priors:suppression_score(udhr_article_3__positive_entitlement_reading, 0.7).
domain_priors:theater_ratio(udhr_article_3__positive_entitlement_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__positive_entitlement_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__positive_entitlement_reading, "UDHR Article 3: Positive Entitlement to Material Conditions").
narrative_ontology:topic_domain(udhr_article_3__positive_entitlement_reading, "constitutional_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__positive_entitlement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__positive_entitlement_reading, '0eedfb59-c6e9-4827-aff9-108edc6358e4').
narrative_ontology:cs_kernel_codification('0eedfb59-c6e9-4827-aff9-108edc6358e4', fixed_text).
narrative_ontology:cs_authority_grounding('0eedfb59-c6e9-4827-aff9-108edc6358e4', lineage).
narrative_ontology:cs_interpretation_layer_present('0eedfb59-c6e9-4827-aff9-108edc6358e4').
narrative_ontology:cs_reading_relation('0eedfb59-c6e9-4827-aff9-108edc6358e4', udhr_article_3__negative_liberty_reading, forecloses).
narrative_ontology:cs_reading_relation('0eedfb59-c6e9-4827-aff9-108edc6358e4', udhr_article_3__procedural_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('0eedfb59-c6e9-4827-aff9-108edc6358e4', foundational, human_dignity_requires_material_conditions).
narrative_ontology:cs_axiom_status(human_dignity_requires_material_conditions, holdable).
narrative_ontology:cs_axiom_grounding('0eedfb59-c6e9-4827-aff9-108edc6358e4', human_dignity_requires_material_conditions, deontological).
narrative_ontology:cs_axiom('0eedfb59-c6e9-4827-aff9-108edc6358e4', foundational, state_has_positive_obligations_for_welfare).
narrative_ontology:cs_axiom_status(state_has_positive_obligations_for_welfare, holdable).
narrative_ontology:cs_axiom_grounding('0eedfb59-c6e9-4827-aff9-108edc6358e4', state_has_positive_obligations_for_welfare, conventional).
narrative_ontology:cs_reference_frame('0eedfb59-c6e9-4827-aff9-108edc6358e4', post_udhr_social_contract).
narrative_ontology:cs_drift_state('0eedfb59-c6e9-4827-aff9-108edc6358e4', contemporary_global_challenges, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('0eedfb59-c6e9-4827-aff9-108edc6358e4', '').
narrative_ontology:cs_kernel_id(udhr_article_3__positive_entitlement_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, vulnerable_groups).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, state_welfare_agencies).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, taxpayers).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, property_owners).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, absolute_free_speech_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, human_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive essential state-provided services like healthcare, housing, and welfare, which are deemed necessary for life and security. Their well-being is directly dependent on the state fulfilling these obligations.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, vulnerable_groups, beneficiary,
    powerless, immediate, trapped, national).

% Responsible for designing, funding, and administering the programs and policies that fulfill the state's positive obligations under this reading of Article 3. They manage the redistribution of resources and enforcement of related regulations.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, state_welfare_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Bear the financial costs of state provision through various taxes. While some may support the underlying principles, the direct financial burden is a form of extraction, with limited individual exit options from the tax system.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, taxpayers, payer,
    organized, biographical, constrained, national).

% May face wealth redistribution policies, higher property taxes, or regulations on property use (e.g., housing mandates) to ensure material conditions for all. Their property rights are subordinated to the collective good of security and welfare.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, property_owners, payer,
    organized, biographical, constrained, national).

% Experience limitations on their claims for unlimited free expression, particularly in areas like hate speech, which are restricted to ensure the 'security' and dignity of vulnerable groups. This represents a curtailment of their preferred liberty.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, absolute_free_speech_advocates, payer,
    organized, biographical, constrained, national).

% Actively promote and defend the interpretation of Article 3 as a positive entitlement, seeing its implementation as crucial for human dignity and social justice. They benefit from the legal and political traction this reading provides for their agenda.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, human_rights_advocates, beneficiary,
    organized, generational, analytical, global).

% Interpret and enforce Article 3 within national legal frameworks, often expanding the scope of positive obligations through jurisprudence. Their rulings shape the practical implementation and limits of this reading.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_article_3__positive_entitlement_reading, vulnerable_groups).
narrative_ontology:fixing_cost_class(udhr_article_3__positive_entitlement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a societal baseline of material conditions (welfare, healthcare, housing) and security for all, aiming to prevent social instability, ensure human dignity, and foster collective well-being by coordinating state action and resource allocation.
% TRANSFER_FUNCTION: Transfers financial resources and services from the general tax base (taxpayers, property owners) to vulnerable populations, and reallocates certain individual liberties (e.g., absolute free speech) to prioritize collective security and dignity, all mediated by state mechanisms.
% ABSENT_VOICES: Libertarian economists and proponents of minimal state intervention would object, arguing that extensive state provision distorts markets, infringes on individual liberty, and creates dependency. Advocates for absolute individual property rights or unlimited free expression would also object to the curtailment of their claims.
% DISAPPEARANCE_RATIONALE: If this reading of Article 3 vanished, states would no longer be obligated to provide welfare, healthcare, or housing. This would lead to a dramatic increase in poverty, social inequality, and insecurity, fundamentally altering the social contract and potentially leading to widespread unrest and humanitarian crises.
% FOUNDING_PROBLEM: The historical experience of widespread poverty, lack of access to basic necessities, and social insecurity, particularly in the aftermath of major conflicts, which demonstrated the inadequacy of purely negative rights to ensure human dignity and a stable society.
% FOUNDING_PROBLEM_CORROBORATION: International human rights bodies, UN reports, and numerous non-governmental organizations (NGOs) consistently document ongoing global challenges in ensuring material conditions for all, corroborating the continued relevance and urgency of this founding problem.
narrative_ontology:disappearance_verdict(udhr_article_3__positive_entitlement_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__positive_entitlement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__positive_entitlement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(udhr_article_3__positive_entitlement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__positive_entitlement_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.75) is high because this reading mandates significant state expenditure and wealth redistribution, which is extracted from taxpayers and property owners. Suppression (0.70) is also high, as it actively curtails claims of absolute individual property rights or unlimited free expression in favor of collective welfare and security, requiring robust legal and administrative enforcement. The theater ratio is low (0.10) because the state's actions are direct and functional, not merely performative. Resistance (0.65) is moderate-to-high, reflecting ongoing political and legal contestation over the scope and implementation of these positive rights.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of vulnerable groups and human rights advocates, this constraint is a vital 'rope' or 'scaffold' ensuring fundamental human dignity and social stability. However, from the perspective of taxpayers, property owners, and absolute free speech advocates, it operates as a 'snare' or 'tangled_rope', extracting resources and curtailing liberties through state coercion. The state welfare agencies, as agenda-setters, view it as a necessary coordination mechanism for societal well-being.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable groups are full beneficiaries (d near 0.0) as they directly receive essential services. State welfare agencies are also beneficiaries (d near 0.0-0.15) as their mandate and power are affirmed. Taxpayers, property owners, and absolute free speech advocates are targets (d near 1.0) as they bear the financial costs and experience limitations on their rights. Human rights advocates are beneficiaries (d near 0.0) as their core mission is advanced. Constitutional courts, as interpreters, have a more symmetric role (d near 0.5) but lean towards beneficiary as their institutional power is enhanced by expanding state obligations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem of widespread poverty and insecurity remains 'live', indicating that the constraint's mandate has not atrophied. Therefore, it is not a 'piton'. The constraint genuinely addresses a coordination problem (ensuring basic welfare), but the significant and actively enforced extraction from specific groups, coupled with the suppression of alternative claims, prevents it from being a pure 'rope'. This structural asymmetry, where some are coordinated and others pay through the same structure, firmly places it as a 'tangled_rope'.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    positive_vs_negative_rights_ambiguity,
    'Is Article 3 primarily a guarantee of negative liberty (freedom from state interference) or a positive entitlement (state obligation to provide)?',
    'Further international jurisprudence and state practice, particularly in jurisdictions with strong constitutional welfare provisions, clarifying the scope of state obligations versus prohibitions.',
    'If resolved towards negative liberty, the constraint''s extractiveness and suppression would decrease significantly, reclassifying it closer to a ''rope'' or ''mountain'' (if natural law). If resolved towards positive entitlement, the current classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(positive_vs_negative_rights_ambiguity, conceptual, 'Fundamental conceptual ambiguity in human rights interpretation.').

omega_variable(
    scope_of_security_ambiguity,
    'Does ''security of person'' primarily refer to freedom from physical violence and arbitrary detention, or does it extend to security from want (economic and social security)?',
    'Empirical studies on the causal links between material deprivation and physical insecurity, alongside evolving international legal interpretations of ''security''.',
    'If limited to physical security, the justification for state welfare provision weakens, reducing extractiveness. If extended to security from want, the current classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_security_ambiguity, empirical, 'Ambiguity in the definition and scope of ''security of person''.').

omega_variable(
    state_provision_efficacy_vs_market,
    'Are state-provided material conditions (welfare, healthcare, housing) more effective and equitable than market-based or private provision in ensuring ''life and security'' for all?',
    'Comparative economic and social policy research across different national models, assessing outcomes for vulnerable populations, efficiency, and equity.',
    'If market-based solutions are demonstrably more effective and equitable, the instrumental justification for high state extraction and suppression weakens, potentially reclassifying the constraint as less extractive. If state provision is superior, the current classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_provision_efficacy_vs_market, empirical, 'Empirical debate on the optimal mechanism for ensuring material conditions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__positive_entitlement_reading, 1948, 2018).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t1948, udhr_article_3__positive_entitlement_reading, theater_ratio, 1948, 0.05).
narrative_ontology:measurement(udhr_tr_t1958, udhr_article_3__positive_entitlement_reading, theater_ratio, 1958, 0.07).
narrative_ontology:measurement(udhr_tr_t1968, udhr_article_3__positive_entitlement_reading, theater_ratio, 1968, 0.08).
narrative_ontology:measurement(udhr_tr_t1978, udhr_article_3__positive_entitlement_reading, theater_ratio, 1978, 0.09).
narrative_ontology:measurement(udhr_tr_t1988, udhr_article_3__positive_entitlement_reading, theater_ratio, 1988, 0.1).
narrative_ontology:measurement(udhr_tr_t1998, udhr_article_3__positive_entitlement_reading, theater_ratio, 1998, 0.1).
narrative_ontology:measurement(udhr_tr_t2008, udhr_article_3__positive_entitlement_reading, theater_ratio, 2008, 0.1).
narrative_ontology:measurement(udhr_tr_t2018, udhr_article_3__positive_entitlement_reading, theater_ratio, 2018, 0.1).

% Extraction over time
narrative_ontology:measurement(udhr_be_t1948, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1948, 0.5).
narrative_ontology:measurement(udhr_be_t1958, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1958, 0.58).
narrative_ontology:measurement(udhr_be_t1968, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1968, 0.65).
narrative_ontology:measurement(udhr_be_t1978, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1978, 0.7).
narrative_ontology:measurement(udhr_be_t1988, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1988, 0.72).
narrative_ontology:measurement(udhr_be_t1998, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1998, 0.73).
narrative_ontology:measurement(udhr_be_t2008, udhr_article_3__positive_entitlement_reading, base_extractiveness, 2008, 0.74).
narrative_ontology:measurement(udhr_be_t2018, udhr_article_3__positive_entitlement_reading, base_extractiveness, 2018, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t1948, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1948, 0.45).
narrative_ontology:measurement(udhr_su_t1958, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1958, 0.55).
narrative_ontology:measurement(udhr_su_t1968, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1968, 0.62).
narrative_ontology:measurement(udhr_su_t1978, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1978, 0.67).
narrative_ontology:measurement(udhr_su_t1988, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1988, 0.69).
narrative_ontology:measurement(udhr_su_t1998, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1998, 0.7).
narrative_ontology:measurement(udhr_su_t2008, udhr_article_3__positive_entitlement_reading, suppression_requirement, 2008, 0.7).
narrative_ontology:measurement(udhr_su_t2018, udhr_article_3__positive_entitlement_reading, suppression_requirement, 2018, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__positive_entitlement_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, udhr_article_25__right_to_adequate_standard_of_living).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, udhr_article_22__right_to_social_security).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, national_welfare_state_legislation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the UDHR Article 3 kernel. It is structurally distinct from the 'negative liberty' and 'procedural hybrid' readings, which emphasize different aspects of the article and lead to different classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
