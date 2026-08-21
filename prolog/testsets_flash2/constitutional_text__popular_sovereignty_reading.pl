% ============================================================================
% CONSTRAINT STORY: constitutional_text__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__popular_sovereignty_reading, []).

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
 *   constraint_id: constitutional_text__popular_sovereignty_reading
 *   human_readable: Constitutional Text: Popular Sovereignty Reading
 *   domain: constitutional_theory/political_philosophy/comparative_law
 *
 * SUMMARY:
 *   This constraint represents the 'popular sovereignty' reading of
 *   constitutional text, where the ultimate authority for constitutional
 *   interpretation and amendment rests with the constituent power of the
 *   people (the demos), rather than with courts or legislatures. This reading
 *   emphasizes extra-institutional democratic expression, such as conventions
 *   or even revolution, as legitimate mechanisms for constitutional change.
 *   It is one of several competing readings of the 'constitutional_text'
 *   kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__popular_sovereignty_reading, 0.3).
domain_priors:suppression_score(constitutional_text__popular_sovereignty_reading, 0.2).
domain_priors:theater_ratio(constitutional_text__popular_sovereignty_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__popular_sovereignty_reading, rope).
narrative_ontology:human_readable(constitutional_text__popular_sovereignty_reading, "Constitutional Text: Popular Sovereignty Reading").
narrative_ontology:topic_domain(constitutional_text__popular_sovereignty_reading, "constitutional_theory/political_philosophy/comparative_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__popular_sovereignty_reading, '08f6196a-1494-4814-8d7c-ca56299aa128').
narrative_ontology:cs_kernel_codification('08f6196a-1494-4814-8d7c-ca56299aa128', fixed_text).
narrative_ontology:cs_authority_grounding('08f6196a-1494-4814-8d7c-ca56299aa128', lineage).
narrative_ontology:cs_interpretation_layer_present('08f6196a-1494-4814-8d7c-ca56299aa128').
narrative_ontology:cs_reading_relation('08f6196a-1494-4814-8d7c-ca56299aa128', constitutional_text__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('08f6196a-1494-4814-8d7c-ca56299aa128', constitutional_text__legislative_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('08f6196a-1494-4814-8d7c-ca56299aa128', foundational, constituent_power_supremacy).
narrative_ontology:cs_axiom_status(constituent_power_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('08f6196a-1494-4814-8d7c-ca56299aa128', constituent_power_supremacy, deontological).
narrative_ontology:cs_axiom('08f6196a-1494-4814-8d7c-ca56299aa128', secondary, institutional_subordination_to_demos).
narrative_ontology:cs_axiom_status(institutional_subordination_to_demos, holdable).
narrative_ontology:cs_axiom_grounding('08f6196a-1494-4814-8d7c-ca56299aa128', institutional_subordination_to_demos, conventional).
narrative_ontology:cs_reference_frame('08f6196a-1494-4814-8d7c-ca56299aa128', founding_moment_of_popular_ratification).
narrative_ontology:cs_drift_state('08f6196a-1494-4814-8d7c-ca56299aa128', contemporary_institutional_entrenchment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('08f6196a-1494-4814-8d7c-ca56299aa128', '').
narrative_ontology:cs_kernel_id(constitutional_text__popular_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, the_demos).
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, democratic_participation).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, institutional_stability).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, judicial_expertise).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, legislature).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate source of constitutional authority, capable of amending, re-writing, or revolutionizing the constitutional order. Benefits from the recognition of its ultimate interpretive power.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, the_demos, agenda_setter,
    institutional, generational, mobile, national).

% The active engagement of citizens in political processes, which is legitimized and empowered by the popular sovereignty reading. Benefits from the expanded scope for direct popular action.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, democratic_participation, beneficiary,
    moderate, biographical, mobile, local).

% The predictability and continuity of governmental structures. Bears the cost of potential disruption from extra-institutional popular action, which can challenge established norms and procedures.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, institutional_stability, payer,
    institutional, generational, constrained, national).

% The specialized knowledge and interpretive authority of courts. Bears the cost of being subordinated to popular will, potentially undermining the perceived neutrality and technical competence of legal interpretation.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, judicial_expertise, payer,
    institutional, biographical, constrained, national).

% The elected representative body. Bears the cost of not being the ultimate authority, as its constitutional interpretations can be challenged or overridden by the demos.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, legislature, payer,
    institutional, biographical, constrained, national).

% Analyze and debate the theoretical underpinnings and practical implications of different constitutional readings. Their role is to articulate and critique these frameworks.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the ultimate source of constitutional authority, ensuring that all institutional actors derive their legitimacy from the collective will of the people, preventing any single branch from claiming absolute interpretive supremacy.
% TRANSFER_FUNCTION: Transfers ultimate interpretive authority from institutional bodies (courts, legislature) to the constituent power of the people, legitimizing popular mobilization and direct democratic action as constitutional mechanisms.
% ABSENT_VOICES: Those who prioritize institutional stability, judicial independence, or legislative efficiency might argue that this reading introduces too much uncertainty and potential for disruption, but their concerns are subordinated to the ultimate authority of the demos.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the constitutional order would likely default to either judicial or legislative supremacy, fundamentally altering the balance of power and the avenues for popular influence. The very concept of ultimate popular authority would be lost, leading to a re-evaluation of institutional roles and legitimacy.
% FOUNDING_PROBLEM: To prevent any single branch of government or elite group from usurping ultimate constitutional authority, ensuring that the foundational power remains with the people.
% FOUNDING_PROBLEM_CORROBORATION: Political theorists, historians of constitutional conventions, and movements advocating for direct democracy or constitutional reform corroborate that the problem of institutional overreach and the need for popular checks remains live. This is attested by historical examples of popular uprisings and ongoing debates about constitutional amendment processes.
narrative_ontology:disappearance_verdict(constitutional_text__popular_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__popular_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__popular_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(constitutional_text__popular_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__popular_sovereignty_reading, 0.3, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__popular_sovereignty_reading_tests).
:- end_tests(constitutional_text__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.3) because this reading primarily empowers the demos rather than extracting from it, though it imposes costs on institutional stability. Suppression is low (0.2) as it actively resists institutional suppression of popular will. Theater ratio is low (0.1) because the claim of popular sovereignty, while often invoked rhetorically, is also genuinely expressed through various forms of popular mobilization and amendment processes. The metrics reflect the empowering nature of this reading for the demos, while acknowledging the friction it creates for established institutions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the demos, this is a pure rope, coordinating their collective power. From the perspective of institutional stability or judicial expertise, it might appear as a tangled rope or even a snare, as it extracts their autonomy and introduces uncertainty. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The demos and democratic participation are clear beneficiaries, as this reading grants them ultimate authority and legitimizes their actions. Institutional stability, judicial expertise, and the legislature are payers, as their claims to final authority are subordinated to the popular will. This creates a structural tension where the constraint benefits popular power at the cost of institutional autonomy.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    popular_will_definition,
    'How is ''the will of the people'' or ''the demos'' legitimately ascertained, beyond formal amendment processes?',
    'Empirical study of historical constitutional moments (e.g., conventions, revolutions, mass movements) to identify patterns of legitimate popular expression, or theoretical consensus on criteria for constituent power.',
    'If a clear, non-disruptive mechanism for discerning popular will is identified, the perceived costs to institutional stability would decrease, potentially shifting the classification towards a purer rope. If not, the ambiguity itself contributes to the ''extraction'' from institutional stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(popular_will_definition, conceptual, 'Ambiguity in defining and expressing the ''will of the people'' outside formal channels.').

omega_variable(
    institutional_subordination_impact,
    'What are the long-term consequences for institutional effectiveness and democratic governance when courts and legislatures are explicitly subordinated to extra-institutional popular authority?',
    'Comparative analysis of constitutional systems that explicitly embrace popular sovereignty versus those that prioritize institutional supremacy, examining metrics like policy stability, protection of minority rights, and governmental efficiency.',
    'If subordination leads to chronic instability or erosion of rights, the ''extraction'' from institutional stability would be re-evaluated as more severe, potentially pushing the constraint towards a tangled rope or snare from an institutional perspective. If it leads to more responsive and legitimate governance, the costs would be seen as justified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_subordination_impact, empirical, 'The actual impact of popular sovereignty on institutional functioning.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__popular_sovereignty_reading, 1787, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1787, constitutional_text__popular_sovereignty_reading, theater_ratio, 1787, 0.05).
narrative_ontology:measurement(cons_tr_t1865, constitutional_text__popular_sovereignty_reading, theater_ratio, 1865, 0.1).
narrative_ontology:measurement(cons_tr_t1937, constitutional_text__popular_sovereignty_reading, theater_ratio, 1937, 0.08).
narrative_ontology:measurement(cons_tr_t1968, constitutional_text__popular_sovereignty_reading, theater_ratio, 1968, 0.12).
narrative_ontology:measurement(cons_tr_t2024, constitutional_text__popular_sovereignty_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(cons_be_t1787, constitutional_text__popular_sovereignty_reading, base_extractiveness, 1787, 0.2).
narrative_ontology:measurement(cons_be_t1865, constitutional_text__popular_sovereignty_reading, base_extractiveness, 1865, 0.3).
narrative_ontology:measurement(cons_be_t1937, constitutional_text__popular_sovereignty_reading, base_extractiveness, 1937, 0.25).
narrative_ontology:measurement(cons_be_t1968, constitutional_text__popular_sovereignty_reading, base_extractiveness, 1968, 0.35).
narrative_ontology:measurement(cons_be_t2024, constitutional_text__popular_sovereignty_reading, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1787, constitutional_text__popular_sovereignty_reading, suppression_requirement, 1787, 0.1).
narrative_ontology:measurement(cons_su_t1865, constitutional_text__popular_sovereignty_reading, suppression_requirement, 1865, 0.2).
narrative_ontology:measurement(cons_su_t1937, constitutional_text__popular_sovereignty_reading, suppression_requirement, 1937, 0.15).
narrative_ontology:measurement(cons_su_t1968, constitutional_text__popular_sovereignty_reading, suppression_requirement, 1968, 0.25).
narrative_ontology:measurement(cons_su_t2024, constitutional_text__popular_sovereignty_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__popular_sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, constitutional_text__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, constitutional_text__legislative_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three competing readings of the 'constitutional_text' kernel, each representing a different locus of ultimate interpretive authority. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
