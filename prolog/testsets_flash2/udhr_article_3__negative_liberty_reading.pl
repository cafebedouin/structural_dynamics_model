% ============================================================================
% CONSTRAINT STORY: udhr_article_3__negative_liberty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__negative_liberty_reading, []).

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
 *   constraint_id: udhr_article_3__negative_liberty_reading
 *   human_readable: UDHR Article 3: Negative Liberty Reading (Freedom from State Violence)
 *   domain: constitutional_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'negative liberty' reading of Article 3 of
 *   the Universal Declaration of Human Rights (UDHR), which emphasizes
 *   freedom from state interference and arbitrary deprivation of life and
 *   liberty, secured through strict procedural justice. This reading is
 *   characterized by its advocacy for capital punishment abolition,
 *   restrictive self-defense doctrines for states, and expansive due process
 *   rights for individuals. It is one reading of the broader 'udhr_article_3'
 *   kernel, distinct from 'positive_entitlement_reading' and
 *   'procedural_hybrid_reading'.
 *
 * KEY AGENTS:
 *   - individuals_against_state_overreach: Primary beneficiary (powerless/trapped) — protected from state violence.
 *   - state_security_apparatus: Primary target/payer (institutional/constrained) — bears costs of procedural limits.
 *   - collective_security_measures: Secondary target/payer (organized/constrained) — constrained by individual rights.
 *   - human_rights_advocates: Agenda-setter (organized/mobile) — actively promotes this reading.
 *   - states_prioritizing_order: Excluded (institutional/constrained) — would object to restrictive interpretation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__negative_liberty_reading, 0.85).
domain_priors:suppression_score(udhr_article_3__negative_liberty_reading, 0.7).
domain_priors:theater_ratio(udhr_article_3__negative_liberty_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__negative_liberty_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__negative_liberty_reading, "UDHR Article 3: Negative Liberty Reading (Freedom from State Violence)").
narrative_ontology:topic_domain(udhr_article_3__negative_liberty_reading, "constitutional_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__negative_liberty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__negative_liberty_reading, 'a9438325-5b5a-4389-bcd4-90599c5a5757').
narrative_ontology:cs_kernel_codification('a9438325-5b5a-4389-bcd4-90599c5a5757', fixed_text).
narrative_ontology:cs_authority_grounding('a9438325-5b5a-4389-bcd4-90599c5a5757', lineage).
narrative_ontology:cs_interpretation_layer_present('a9438325-5b5a-4389-bcd4-90599c5a5757').
narrative_ontology:cs_reading_relation('a9438325-5b5a-4389-bcd4-90599c5a5757', udhr_article_3__positive_entitlement_reading, coexists_with).
narrative_ontology:cs_reading_relation('a9438325-5b5a-4389-bcd4-90599c5a5757', udhr_article_3__procedural_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('a9438325-5b5a-4389-bcd4-90599c5a5757', foundational, state_non_interference_is_primary_liberty).
narrative_ontology:cs_axiom_status(state_non_interference_is_primary_liberty, holdable).
narrative_ontology:cs_axiom_grounding('a9438325-5b5a-4389-bcd4-90599c5a5757', state_non_interference_is_primary_liberty, deontological).
narrative_ontology:cs_axiom('a9438325-5b5a-4389-bcd4-90599c5a5757', foundational, procedural_justice_is_sole_legitimate_deprivation_path).
narrative_ontology:cs_axiom_status(procedural_justice_is_sole_legitimate_deprivation_path, holdable).
narrative_ontology:cs_axiom_grounding('a9438325-5b5a-4389-bcd4-90599c5a5757', procedural_justice_is_sole_legitimate_deprivation_path, conventional).
narrative_ontology:cs_reference_frame('a9438325-5b5a-4389-bcd4-90599c5a5757', post_wwii_individual_protection_paradigm).
narrative_ontology:cs_drift_state('a9438325-5b5a-4389-bcd4-90599c5a5757', contemporary_global_security_challenges, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a9438325-5b5a-4389-bcd4-90599c5a5757', '').
narrative_ontology:cs_kernel_id(udhr_article_3__negative_liberty_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, individuals_against_state_overreach).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, state_security_apparatus).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, collective_security_measures).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These individuals benefit from the constraint's prohibition on arbitrary state deprivation of life and liberty, receiving protection from state violence and ensuring due process. Their 'powerless' status reflects their vulnerability without such protections, and 'trapped' exit options highlight the lack of alternatives to state authority.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, individuals_against_state_overreach, beneficiary,
    powerless, biographical, trapped, universal).

% This entity bears the cost of the constraint by being limited in its use of force, requiring adherence to strict procedural justice, and facing scrutiny over capital punishment and self-defense doctrines. Its 'constrained' exit reflects the difficulty of operating outside international human rights norms.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, state_security_apparatus, payer,
    institutional, generational, constrained, national).

% These measures, such as broad surveillance programs or preventative detention, are constrained by the emphasis on individual negative liberty. They bear the cost of needing to justify their existence and methods against a high bar of individual rights, leading to 'constrained' exit options as they must adapt or face legal challenge.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, collective_security_measures, payer,
    organized, generational, constrained, national).

% These advocates actively promote and enforce this negative liberty reading, pushing for stricter interpretations of due process and limitations on state power. Their 'mobile' exit reflects their ability to shift focus and resources across different jurisdictions and issues.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, human_rights_advocates, agenda_setter,
    organized, generational, mobile, global).

% These states would argue for a broader interpretation of 'security' that allows for more expansive state powers to maintain public order, even at the expense of some individual liberties. They are 'excluded' from the dominant discourse of this reading, which prioritizes individual protection.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, states_prioritizing_order, excluded,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal baseline for individual protection from arbitrary state power, coordinating international legal frameworks and national constitutional norms around a shared understanding of negative liberty and due process.
% TRANSFER_FUNCTION: Transfers the burden of proof and justification for deprivation of life and liberty from the individual to the state, requiring strict adherence to procedural justice and limiting state discretion.
% ABSENT_VOICES: States prioritizing collective security or national order would object, arguing that an overly restrictive negative liberty reading hampers effective governance and defense against non-state threats. Their voices are often marginalized in human rights discourse that emphasizes individual protections.
% DISAPPEARANCE_RATIONALE: If this reading of Article 3 vanished, states would face fewer constraints on their use of force, potentially leading to increased arbitrary detentions, extrajudicial killings, and erosion of due process. The international human rights framework would lose a foundational pillar, and individual protections would significantly diminish, forcing a global re-evaluation of state-citizen relations.
% FOUNDING_PROBLEM: The problem of arbitrary state power, totalitarian regimes, and widespread human rights abuses witnessed during the World Wars, where individuals lacked fundamental protections against their own governments.
% FOUNDING_PROBLEM_CORROBORATION: Human rights organizations, international legal bodies, and numerous academic scholars outside of state security apparatuses consistently attest that arbitrary state power remains a live problem, citing ongoing abuses globally. This corroboration supports the continued relevance of the constraint's founding purpose.
narrative_ontology:disappearance_verdict(udhr_article_3__negative_liberty_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__negative_liberty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__negative_liberty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(udhr_article_3__negative_liberty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__negative_liberty_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__negative_liberty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_article_3__negative_liberty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_article_3__negative_liberty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the significant limitations placed on state power, which is 'extracted' from the state's capacity for unconstrained action. Suppression (0.7) is also high, as states are actively compelled to adhere to these procedural safeguards, often against their preference for more expedient security measures. The theater ratio (0.2) is relatively low, indicating that while there's some performative adherence, the core function of limiting state violence is genuinely pursued. The increasing trend in extractiveness and suppression over time reflects the growing international pressure and legal precedents that have strengthened this reading's application.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of individuals, this constraint is a vital protection, a 'rope' or even a 'mountain' of fundamental rights. From the perspective of the state security apparatus, it is a 'snare' that unduly restricts their ability to ensure collective safety. The engine's computation will highlight this divergence, showing how the same structure is experienced as coordination by beneficiaries and extraction by targets.
 *
 * DIRECTIONALITY LOGIC:
 *   Individuals are full beneficiaries (d=0.0) as the constraint directly protects them. The state security apparatus and collective security measures are full targets (d=1.0) as they bear the costs of compliance. Human rights advocates, while promoting the constraint, are not direct beneficiaries of its extraction but rather its enforcers, hence their 'mobile' exit and 'agenda_setter' role. States prioritizing order are excluded, meaning their directionality is not directly computed but their structural opposition is noted.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling genuine protection as mere state overreach by clearly defining the boundaries of legitimate state action. The high extractiveness is not from a coordination failure but from the intentional limitation of state power, which is the core function of this reading. It avoids becoming a 'piton' because human rights advocates actively enforce it, and the 'founding_problem_status' is 'live', indicating ongoing relevance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    negative_vs_positive_liberty_boundary,
    'Is the distinction between negative liberty (freedom from) and positive liberty (freedom to) a fundamental conceptual divide or a matter of policy emphasis?',
    'Philosophical consensus on the nature of liberty, or a legal framework that successfully integrates both concepts without internal contradiction.',
    'If a fundamental divide, this reading remains distinct. If a matter of emphasis, it could be reclassified as a component of a broader ''hybrid'' constraint, reducing its perceived extractiveness by acknowledging a wider scope of state responsibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(negative_vs_positive_liberty_boundary, conceptual, 'Conceptual boundary between negative and positive liberty interpretations of Article 3.').

omega_variable(
    state_self_defense_doctrine_scope,
    'What constitutes a legitimate ''self-defense'' for a state that would justify deprivation of life or liberty, and how does this balance against individual rights?',
    'International legal precedents, UN Security Council resolutions, and evolving jus ad bellum/jus in bello doctrines that clarify the scope of state self-defense in relation to human rights.',
    'A broader interpretation of state self-defense would reduce the constraint''s extractiveness on the state security apparatus; a narrower one would increase it, potentially pushing it closer to a ''snare'' for states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_self_defense_doctrine_scope, empirical, 'Empirical and legal scope of state self-defense doctrines under Article 3.').

omega_variable(
    procedural_justice_effectiveness,
    'How effective are existing procedural justice mechanisms (e.g., fair trial, habeas corpus) in preventing arbitrary deprivation of life and liberty in practice across diverse jurisdictions?',
    'Empirical studies on judicial independence, access to legal aid, and rates of wrongful conviction/detention in various states, compared against international standards.',
    'If mechanisms are widely ineffective, the ''theater_ratio'' would increase, and the constraint might be reclassified as a ''piton'' or ''snare'' due to performative compliance masking ongoing extraction. If highly effective, it reinforces the ''rope'' aspect of genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_justice_effectiveness, empirical, 'Empirical effectiveness of procedural justice safeguards.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__negative_liberty_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t0, udhr_article_3__negative_liberty_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(udhr_tr_t15, udhr_article_3__negative_liberty_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement(udhr_tr_t30, udhr_article_3__negative_liberty_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(udhr_tr_t45, udhr_article_3__negative_liberty_reading, theater_ratio, 45, 0.18).
narrative_ontology:measurement(udhr_tr_t60, udhr_article_3__negative_liberty_reading, theater_ratio, 60, 0.19).
narrative_ontology:measurement(udhr_tr_t75, udhr_article_3__negative_liberty_reading, theater_ratio, 75, 0.2).

% Extraction over time
narrative_ontology:measurement(udhr_be_t0, udhr_article_3__negative_liberty_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(udhr_be_t15, udhr_article_3__negative_liberty_reading, base_extractiveness, 15, 0.75).
narrative_ontology:measurement(udhr_be_t30, udhr_article_3__negative_liberty_reading, base_extractiveness, 30, 0.8).
narrative_ontology:measurement(udhr_be_t45, udhr_article_3__negative_liberty_reading, base_extractiveness, 45, 0.83).
narrative_ontology:measurement(udhr_be_t60, udhr_article_3__negative_liberty_reading, base_extractiveness, 60, 0.84).
narrative_ontology:measurement(udhr_be_t75, udhr_article_3__negative_liberty_reading, base_extractiveness, 75, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t0, udhr_article_3__negative_liberty_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(udhr_su_t15, udhr_article_3__negative_liberty_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(udhr_su_t30, udhr_article_3__negative_liberty_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(udhr_su_t45, udhr_article_3__negative_liberty_reading, suppression_requirement, 45, 0.68).
narrative_ontology:measurement(udhr_su_t60, udhr_article_3__negative_liberty_reading, suppression_requirement, 60, 0.69).
narrative_ontology:measurement(udhr_su_t75, udhr_article_3__negative_liberty_reading, suppression_requirement, 75, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__negative_liberty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, udhr_article_3__positive_entitlement_reading).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, udhr_article_3__procedural_hybrid_reading).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, iccpr_article_6__right_to_life).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, cat__prohibition_of_torture).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the UDHR Article 3 kernel. Each reading has a different ε and stakeholder structure, reflecting the contested nature of the article's interpretation. This negative liberty reading influences and coexists with its siblings, shaping the broader human rights discourse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
