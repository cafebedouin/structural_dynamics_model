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
 *   human_readable: UDHR Article 3: Positive Entitlement Reading (State Obligation for Material Conditions)
 *   domain: constitutional_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'positive entitlement' reading of Article
 *   3 of the Universal Declaration of Human Rights (UDHR), which interprets
 *   'life, liberty and security of person' as obligating states to actively
 *   provide material conditions (welfare, healthcare, housing) necessary for
 *   these rights. This reading entails significant state intervention,
 *   resource redistribution, and restrictions on competing claims (e.g.,
 *   absolute property rights, unrestricted speech). It is a contested
 *   interpretation within human rights discourse, contrasting sharply with
 *   more minimalist readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, 0.75).
domain_priors:suppression_score(udhr_article_3__positive_entitlement_reading, 0.8).
domain_priors:theater_ratio(udhr_article_3__positive_entitlement_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__positive_entitlement_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__positive_entitlement_reading, "UDHR Article 3: Positive Entitlement Reading (State Obligation for Material Conditions)").
narrative_ontology:topic_domain(udhr_article_3__positive_entitlement_reading, "constitutional_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__positive_entitlement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__positive_entitlement_reading, '0488373c-38d6-4256-96cd-c987df396020').
narrative_ontology:cs_kernel_codification('0488373c-38d6-4256-96cd-c987df396020', fixed_text).
narrative_ontology:cs_authority_grounding('0488373c-38d6-4256-96cd-c987df396020', lineage).
narrative_ontology:cs_interpretation_layer_present('0488373c-38d6-4256-96cd-c987df396020').
narrative_ontology:cs_reading_relation('0488373c-38d6-4256-96cd-c987df396020', udhr_article_3__negative_liberty_reading, forecloses).
narrative_ontology:cs_reading_relation('0488373c-38d6-4256-96cd-c987df396020', udhr_article_3__procedural_hybrid_reading, forecloses).
narrative_ontology:cs_axiom('0488373c-38d6-4256-96cd-c987df396020', foundational, material_conditions_as_inherent_rights).
narrative_ontology:cs_axiom_status(material_conditions_as_inherent_rights, holdable).
narrative_ontology:cs_axiom_grounding('0488373c-38d6-4256-96cd-c987df396020', material_conditions_as_inherent_rights, deontological).
narrative_ontology:cs_axiom('0488373c-38d6-4256-96cd-c987df396020', foundational, state_as_primary_guarantor_of_welfare).
narrative_ontology:cs_axiom_status(state_as_primary_guarantor_of_welfare, holdable).
narrative_ontology:cs_axiom_grounding('0488373c-38d6-4256-96cd-c987df396020', state_as_primary_guarantor_of_welfare, conventional).
narrative_ontology:cs_reference_frame('0488373c-38d6-4256-96cd-c987df396020', post_wwii_social_contract).
narrative_ontology:cs_drift_state('0488373c-38d6-4256-96cd-c987df396020', contemporary_neoliberal_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0488373c-38d6-4256-96cd-c987df396020', '').
narrative_ontology:cs_kernel_id(udhr_article_3__positive_entitlement_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, vulnerable_groups).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, state_institutions_administering_welfare).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, property_owners).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, taxpayers).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, unrestricted_speech_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals and communities who rely on state provision for basic material conditions like welfare, healthcare, and housing to ensure their life and security. Their exit options are severely limited without these provisions.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, vulnerable_groups, beneficiary,
    powerless, immediate, trapped, national).

% Government agencies and departments responsible for designing, funding, and implementing social welfare programs, healthcare systems, and housing initiatives. They gain legitimacy and power through their role as guarantors of these rights.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, state_institutions_administering_welfare, agenda_setter,
    institutional, generational, analytical, national).

% Individuals and entities whose property rights may be curtailed or whose wealth is redistributed through taxation or regulation to fund state provisions. Their options are to comply, litigate, or exit the jurisdiction.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, property_owners, payer,
    powerful, biographical, constrained, national).

% Citizens and residents who fund state welfare and security provisions through various forms of taxation. They bear the direct financial costs of the positive entitlements.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, taxpayers, payer,
    moderate, biographical, constrained, national).

% Groups and individuals who advocate for expansive freedom of expression, including speech that might be deemed harmful or incite violence, and thus face restrictions under a reading that prioritizes collective security and dignity. They 'pay' through curtailed liberty.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, unrestricted_speech_advocates, payer,
    organized, biographical, constrained, national).

% Organizations like the UN Human Rights Committee that monitor state compliance with the UDHR and interpret its articles. They provide an external analytical perspective on the implementation and impact of this reading.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, international_human_rights_bodies, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal baseline of material conditions and security for all individuals, preventing extreme deprivation, fostering social cohesion, and mitigating risks of instability that arise from unmet basic needs.
% TRANSFER_FUNCTION: Transfers financial resources from the general populace (taxpayers, property owners) to vulnerable groups to fund essential services (welfare, healthcare, housing). It also transfers certain individual liberties (e.g., absolute property rights, unrestricted speech) to the state for the sake of collective security and dignity.
% ABSENT_VOICES: Libertarian advocates for minimal state intervention, proponents of absolute property rights, and those who believe in unrestricted freedom of expression would object to the expansive role of the state and the curtailment of individual liberties inherent in this reading. Their perspectives are often marginalized in policy debates driven by this interpretation.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, states would no longer be obligated to provide welfare, healthcare, or housing. This would lead to widespread social upheaval, increased poverty, and a fundamental redefinition of security away from material provision, reorganizing the social contract around individual responsibility rather than collective guarantees.
% FOUNDING_PROBLEM: The historical experience of widespread poverty, social insecurity, and state-sanctioned violence, particularly in the aftermath of World War II, which highlighted the urgent need for states to actively ensure basic human dignity and prevent such atrocities.
% FOUNDING_PROBLEM_CORROBORATION: International human rights organizations, social justice advocates, and many national constitutions continue to corroborate the ongoing relevance of ensuring material conditions for life and security, citing persistent global inequalities and vulnerabilities. This corroboration comes from outside the direct beneficiaries of state welfare programs.
narrative_ontology:disappearance_verdict(udhr_article_3__positive_entitlement_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__positive_entitlement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__positive_entitlement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   The high extractiveness (0.75) reflects the substantial resources transferred from taxpayers and property owners to fund state welfare provisions. The high suppression (0.80) stems from the state's active enforcement of these obligations, which often involves curtailing individual liberties (e.g., property use, certain forms of expression) that conflict with collective welfare. The low theater ratio (0.10) indicates that state actions under this reading are largely functional, directly aimed at providing services and enforcing regulations, rather than being performative. Resistance is high (0.70) due to ongoing political and philosophical opposition to the expansive role of the state and the associated costs and restrictions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of vulnerable groups, this constraint is a vital Rope or even a subsidy, providing essential support. From the perspective of property owners and taxpayers, it operates as a Snare or Tangled Rope, extracting resources and imposing restrictions. The state institutions, while administering the system, also benefit from the expanded mandate and legitimacy. The engine's per-seat classification will highlight these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable groups are clear beneficiaries, receiving essential services. State institutions administering welfare act as agenda-setters, gaining legitimacy and power from their role. Property owners and taxpayers are primary payers, bearing the financial costs. Advocates for unrestricted speech also act as payers, experiencing limitations on their liberty. International human rights bodies serve as observers, analyzing the constraint's implementation and impact.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    positive_vs_negative_rights_ambiguity,
    'Is Article 3 primarily a positive right (state obligation to provide) or a negative right (state prohibition from depriving)?',
    'Analysis of state practice and international jurisprudence: if states consistently act to provide, it supports the positive reading; if they primarily focus on non-interference, it supports the negative reading.',
    'If resolved as primarily a negative right, the extractiveness and suppression metrics for this reading would be significantly lower, potentially reclassifying it as a Rope or even a Mountain (if interpreted as a natural limit on state power).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(positive_vs_negative_rights_ambiguity, conceptual, 'The fundamental conceptual ambiguity between positive and negative interpretations of human rights.').

omega_variable(
    substantive_vs_procedural_rights_ambiguity,
    'Does Article 3 guarantee substantive material conditions, or merely procedural protections for life and security?',
    'Judicial interpretation and legislative action: if courts mandate specific welfare provisions or legislatures enact comprehensive social rights, it supports the substantive reading.',
    'If resolved as purely procedural, the state''s obligations would be limited to due process, drastically reducing the measured extractiveness and suppression, likely reclassifying this constraint as a Rope or even a Mountain (if due process is seen as a fundamental, non-extractive limit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substantive_vs_procedural_rights_ambiguity, conceptual, 'Ambiguity regarding the substantive vs. procedural nature of rights in Article 3.').

omega_variable(
    coordination_vs_extraction_balance,
    'To what extent do state provisions for welfare, healthcare, and housing genuinely solve coordination problems versus primarily serving as mechanisms for extraction and control?',
    'Comparative analysis of alternative provision models (e.g., market-based, community-led) and their efficiency/equity outcomes, alongside detailed cost-benefit analysis of state programs.',
    'If found to be predominantly extractive, the ''tangled_rope'' classification would lean more towards ''snare''; if genuinely coordinative, it would lean more towards ''rope'', with lower effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_balance, empirical, 'The balance between coordination and extraction in state-mandated welfare provisions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__positive_entitlement_reading, 1948, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t1948, udhr_article_3__positive_entitlement_reading, theater_ratio, 1948, 0.05).
narrative_ontology:measurement(udhr_tr_t1963, udhr_article_3__positive_entitlement_reading, theater_ratio, 1963, 0.07).
narrative_ontology:measurement(udhr_tr_t1978, udhr_article_3__positive_entitlement_reading, theater_ratio, 1978, 0.09).
narrative_ontology:measurement(udhr_tr_t1993, udhr_article_3__positive_entitlement_reading, theater_ratio, 1993, 0.1).
narrative_ontology:measurement(udhr_tr_t2008, udhr_article_3__positive_entitlement_reading, theater_ratio, 2008, 0.1).
narrative_ontology:measurement(udhr_tr_t2023, udhr_article_3__positive_entitlement_reading, theater_ratio, 2023, 0.1).

% Extraction over time
narrative_ontology:measurement(udhr_be_t1948, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1948, 0.5).
narrative_ontology:measurement(udhr_be_t1963, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1963, 0.6).
narrative_ontology:measurement(udhr_be_t1978, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1978, 0.68).
narrative_ontology:measurement(udhr_be_t1993, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1993, 0.72).
narrative_ontology:measurement(udhr_be_t2008, udhr_article_3__positive_entitlement_reading, base_extractiveness, 2008, 0.74).
narrative_ontology:measurement(udhr_be_t2023, udhr_article_3__positive_entitlement_reading, base_extractiveness, 2023, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t1948, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1948, 0.55).
narrative_ontology:measurement(udhr_su_t1963, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1963, 0.65).
narrative_ontology:measurement(udhr_su_t1978, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1978, 0.73).
narrative_ontology:measurement(udhr_su_t1993, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1993, 0.77).
narrative_ontology:measurement(udhr_su_t2008, udhr_article_3__positive_entitlement_reading, suppression_requirement, 2008, 0.79).
narrative_ontology:measurement(udhr_su_t2023, udhr_article_3__positive_entitlement_reading, suppression_requirement, 2023, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__positive_entitlement_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
