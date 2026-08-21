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
 *   constraint_id: udhr_article_3__negative_liberty_reading
 *   human_readable: UDHR Article 3: Negative Liberty Reading (Freedom from State Violence)
 *   domain: constitutional_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents a 'negative liberty' reading of Article 3 of
 *   the Universal Declaration of Human Rights (UDHR), focusing on the state's
 *   obligation to refrain from arbitrary deprivation of life and liberty, and
 *   defining security as freedom from state violence. It emphasizes expansive
 *   due process and limits on state power, including implications for capital
 *   punishment and self-defense doctrines. This reading is one of several
 *   interpretations of the UDHR Article 3 kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__negative_liberty_reading, 0.85).
domain_priors:suppression_score(udhr_article_3__negative_liberty_reading, 0.75).
domain_priors:theater_ratio(udhr_article_3__negative_liberty_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__negative_liberty_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__negative_liberty_reading, "UDHR Article 3: Negative Liberty Reading (Freedom from State Violence)").
narrative_ontology:topic_domain(udhr_article_3__negative_liberty_reading, "constitutional_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__negative_liberty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__negative_liberty_reading, 'c8768b96-45db-4407-892d-af6d2467ff50').
narrative_ontology:cs_kernel_codification('c8768b96-45db-4407-892d-af6d2467ff50', fixed_text).
narrative_ontology:cs_authority_grounding('c8768b96-45db-4407-892d-af6d2467ff50', lineage).
narrative_ontology:cs_interpretation_layer_present('c8768b96-45db-4407-892d-af6d2467ff50').
narrative_ontology:cs_reading_relation('c8768b96-45db-4407-892d-af6d2467ff50', udhr_article_3__positive_entitlement_reading, coexists_with).
narrative_ontology:cs_reading_relation('c8768b96-45db-4407-892d-af6d2467ff50', udhr_article_3__procedural_hybrid_reading, influences).
narrative_ontology:cs_axiom('c8768b96-45db-4407-892d-af6d2467ff50', foundational, individual_sovereignty_over_body).
narrative_ontology:cs_axiom_status(individual_sovereignty_over_body, holdable).
narrative_ontology:cs_axiom_grounding('c8768b96-45db-4407-892d-af6d2467ff50', individual_sovereignty_over_body, deontological).
narrative_ontology:cs_axiom('c8768b96-45db-4407-892d-af6d2467ff50', secondary, state_power_is_derivative).
narrative_ontology:cs_axiom_status(state_power_is_derivative, holdable).
narrative_ontology:cs_axiom_grounding('c8768b96-45db-4407-892d-af6d2467ff50', state_power_is_derivative, conventional).
narrative_ontology:cs_reference_frame('c8768b96-45db-4407-892d-af6d2467ff50', post_wwii_individual_rights_framework).
narrative_ontology:cs_drift_state('c8768b96-45db-4407-892d-af6d2467ff50', contemporary_global_security_challenges, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c8768b96-45db-4407-892d-af6d2467ff50', '').
narrative_ontology:cs_kernel_id(udhr_article_3__negative_liberty_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, individuals).
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, human_rights_advocates).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, state_security_apparatus).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, authoritarian_regimes).
narrative_ontology:constraint_vindicates(udhr_article_3__negative_liberty_reading, rule_of_law).
narrative_ontology:constraint_vindicates(udhr_article_3__negative_liberty_reading, individual_autonomy).
narrative_ontology:constraint_vindicates(udhr_article_3__negative_liberty_reading, limited_government).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the prohibition against arbitrary state deprivation of life and liberty, relying on legal and political systems to enforce these protections. Their 'exit' from this protection would mean abandoning their fundamental rights, which is identity-locked.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, individuals, beneficiary,
    powerless, biographical, identity_locked, global).

% Bears the cost of operating within strict procedural justice limits, which restricts their ability to act unilaterally or with broad discretion in matters of life and liberty. They are constrained by legal frameworks and international scrutiny.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, state_security_apparatus, payer,
    institutional, immediate, constrained, national).

% Actively work to promote, monitor, and enforce the negative liberty principles of Article 3, often through litigation, advocacy, and reporting. They benefit from the existence of a clear legal standard to champion.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, human_rights_advocates, agenda_setter,
    organized, generational, mobile, global).

% Are targeted by this constraint, as it directly challenges their ability to exercise arbitrary power over their populations. While they may nominally sign treaties, they often resist or ignore enforcement, but face international pressure and sanctions, making their 'exit' from the norm difficult.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, authoritarian_regimes, payer,
    institutional, biographical, trapped, national).

% Interpret and apply Article 3 in cases of alleged state violations, contributing to the evolving jurisprudence of human rights. They observe the implementation and contestation of the constraint without directly bearing its costs or receiving its primary benefits.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, international_courts, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_article_3__negative_liberty_reading, diffuse).
narrative_ontology:fixing_cost_class(udhr_article_3__negative_liberty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal standard for state conduct regarding individual life and liberty, coordinating international expectations and domestic legal frameworks to prevent arbitrary state violence and deprivation.
% TRANSFER_FUNCTION: Transfers power and discretion from the state (particularly its security and punitive apparatus) to individuals, by limiting the conditions under which life and liberty can be deprived, and by requiring robust procedural justice.
% ABSENT_VOICES: Those who advocate for unlimited state sovereignty or 'national security' exceptionalism are often excluded from the core human rights discourse, or their arguments are framed as illegitimate within this framework. They would argue for greater state discretion.
% DISAPPEARANCE_RATIONALE: If Article 3's negative liberty protections vanished, states would face fewer constraints on arbitrary detention, extrajudicial killings, and other forms of violence, leading to a rapid deterioration of individual security and a reorganization of international relations around power rather than rights.
% FOUNDING_PROBLEM: Preventing arbitrary state deprivation of life and liberty, especially in the wake of the atrocities of World War II, and establishing a baseline of human dignity against state power.
% FOUNDING_PROBLEM_CORROBORATION: International human rights organizations, legal scholars, and victims of state violence consistently attest that the founding problem remains live, citing ongoing violations globally. While some states may claim the problem is resolved within their borders, external corroboration points to persistent challenges.
narrative_ontology:disappearance_verdict(udhr_article_3__negative_liberty_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__negative_liberty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__negative_liberty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The extractiveness (0.85) is high because this reading imposes significant limitations on state power, requiring extensive procedural justice and restricting actions like capital punishment or broad 'self-defense' claims by the state. Suppression (0.75) is also high, as states often resist these limitations, requiring active enforcement by international bodies and domestic legal systems. The theater ratio (0.30) reflects that while some states pay lip service to these rights, their actual practices may diverge, creating a performative gap. The metrics reflect the constraint's operation from the perspective of those it constrains (states).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of individuals, this constraint is a vital protection and a clear 'rope' or even 'mountain' of rights. From the perspective of states, particularly those with authoritarian tendencies or expansive security doctrines, it is a highly extractive 'snare' or 'tangled rope' that limits their sovereign power. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Individuals and human rights advocates are the primary beneficiaries, gaining protection and a legal framework for their work. The state security apparatus and authoritarian regimes are the primary targets/victims, as their power to act arbitrarily is curtailed. International courts serve as observers and enforcers, mediating the constraint's application.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_security_vs_individual_liberty,
    'Is the state''s claim of ''collective security'' a genuine coordination problem requiring limitations on individual liberty, or a cover for expanding state power?',
    'Empirical analysis of security outcomes in states with varying degrees of individual liberty protection; legal review of ''security'' justifications against proportionality and necessity principles.',
    'If ''collective security'' is primarily a cover, the constraint''s effective extractiveness from individuals is higher, and the state''s role as a ''victim'' is reclassified as an ''agenda_setter'' extracting from its citizens. If genuine, the constraint''s coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_security_vs_individual_liberty, empirical, 'Ambiguity regarding the true nature of state security claims.').

omega_variable(
    substantive_vs_procedural_justice,
    'To what extent can procedural justice alone guarantee ''life and liberty'' without substantive limitations on state power, as implied by the procedural_hybrid_reading?',
    'Comparative legal analysis of jurisdictions that prioritize procedural over substantive rights, examining outcomes for vulnerable populations and the incidence of arbitrary state action.',
    'If procedural justice is insufficient without substantive limits, this negative liberty reading gains stronger empirical grounding, and the procedural_hybrid_reading''s classification would shift towards higher extractiveness from individuals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substantive_vs_procedural_justice, conceptual, 'The conceptual boundary between procedural and substantive guarantees of rights.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__negative_liberty_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t1948, udhr_article_3__negative_liberty_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(udhr_tr_t1968, udhr_article_3__negative_liberty_reading, theater_ratio, 1968, 0.15).
narrative_ontology:measurement(udhr_tr_t1988, udhr_article_3__negative_liberty_reading, theater_ratio, 1988, 0.2).
narrative_ontology:measurement(udhr_tr_t2008, udhr_article_3__negative_liberty_reading, theater_ratio, 2008, 0.25).
narrative_ontology:measurement(udhr_tr_t2024, udhr_article_3__negative_liberty_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(udhr_be_t1948, udhr_article_3__negative_liberty_reading, base_extractiveness, 1948, 0.6).
narrative_ontology:measurement(udhr_be_t1968, udhr_article_3__negative_liberty_reading, base_extractiveness, 1968, 0.7).
narrative_ontology:measurement(udhr_be_t1988, udhr_article_3__negative_liberty_reading, base_extractiveness, 1988, 0.78).
narrative_ontology:measurement(udhr_be_t2008, udhr_article_3__negative_liberty_reading, base_extractiveness, 2008, 0.82).
narrative_ontology:measurement(udhr_be_t2024, udhr_article_3__negative_liberty_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t1948, udhr_article_3__negative_liberty_reading, suppression_requirement, 1948, 0.55).
narrative_ontology:measurement(udhr_su_t1968, udhr_article_3__negative_liberty_reading, suppression_requirement, 1968, 0.65).
narrative_ontology:measurement(udhr_su_t1988, udhr_article_3__negative_liberty_reading, suppression_requirement, 1988, 0.7).
narrative_ontology:measurement(udhr_su_t2008, udhr_article_3__negative_liberty_reading, suppression_requirement, 2008, 0.73).
narrative_ontology:measurement(udhr_su_t2024, udhr_article_3__negative_liberty_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__negative_liberty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, udhr_article_3__positive_entitlement_reading).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, udhr_article_3__procedural_hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of UDHR Article 3, each with different structural properties and ε values. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
