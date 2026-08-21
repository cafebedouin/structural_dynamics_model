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
 *   the Universal Declaration of Human Rights (UDHR), which prohibits states
 *   from arbitrarily depriving individuals of life and liberty, emphasizing
 *   freedom from state violence and requiring strict procedural justice. This
 *   reading is distinct from 'positive entitlement' (state provision of
 *   welfare) and 'procedural hybrid' (due process without substantive
 *   resolution) readings. The high extractiveness reflects the significant
 *   limitations placed on state power, particularly regarding capital
 *   punishment, self-defense doctrines, and due process requirements, which
 *   are seen as 'costs' by state security apparatuses and collective security
 *   measures.
 *
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
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__negative_liberty_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__negative_liberty_reading, "UDHR Article 3: Negative Liberty Reading (Freedom from State Violence)").
narrative_ontology:topic_domain(udhr_article_3__negative_liberty_reading, "constitutional_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__negative_liberty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__negative_liberty_reading, 'daf12196-163a-4c71-ba28-e40603bb054e').
narrative_ontology:cs_kernel_codification('daf12196-163a-4c71-ba28-e40603bb054e', fixed_text).
narrative_ontology:cs_authority_grounding('daf12196-163a-4c71-ba28-e40603bb054e', lineage).
narrative_ontology:cs_interpretation_layer_present('daf12196-163a-4c71-ba28-e40603bb054e').
narrative_ontology:cs_reading_relation('daf12196-163a-4c71-ba28-e40603bb054e', udhr_article_3__positive_entitlement_reading, coexists_with).
narrative_ontology:cs_reading_relation('daf12196-163a-4c71-ba28-e40603bb054e', udhr_article_3__procedural_hybrid_reading, influences).
narrative_ontology:cs_axiom('daf12196-163a-4c71-ba28-e40603bb054e', foundational, individual_autonomy_from_state_interference).
narrative_ontology:cs_axiom_status(individual_autonomy_from_state_interference, holdable).
narrative_ontology:cs_axiom_grounding('daf12196-163a-4c71-ba28-e40603bb054e', individual_autonomy_from_state_interference, deontological).
narrative_ontology:cs_axiom('daf12196-163a-4c71-ba28-e40603bb054e', secondary, state_as_primary_threat_to_liberty).
narrative_ontology:cs_axiom_status(state_as_primary_threat_to_liberty, holdable).
narrative_ontology:cs_axiom_grounding('daf12196-163a-4c71-ba28-e40603bb054e', state_as_primary_threat_to_liberty, conventional).
narrative_ontology:cs_reference_frame('daf12196-163a-4c71-ba28-e40603bb054e', post_wwii_individual_protection_paradigm).
narrative_ontology:cs_drift_state('daf12196-163a-4c71-ba28-e40603bb054e', contemporary_counter_terrorism_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('daf12196-163a-4c71-ba28-e40603bb054e', '').
narrative_ontology:cs_kernel_id(udhr_article_3__negative_liberty_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, individuals_facing_state_power).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, state_security_apparatus).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, collective_security_measures).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These individuals are the primary beneficiaries of the constraint, as it aims to protect their life and liberty from arbitrary state deprivation. Their 'exit' from state power is often non-existent, making them highly vulnerable.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, individuals_facing_state_power, beneficiary,
    powerless, immediate, trapped, universal).

% This entity bears the cost of the constraint by being limited in its use of force and surveillance. It must adhere to strict procedural justice, which can be seen as an impediment to its operational efficiency and broad security mandates.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, state_security_apparatus, payer,
    institutional, generational, constrained, national).

% These measures, often enacted in response to perceived threats, are constrained by the negative liberty reading. They may be deemed illegitimate if they infringe on individual rights without sufficient procedural safeguards, leading to their curtailment or abolition.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, collective_security_measures, payer,
    organized, biographical, constrained, national).

% These groups actively champion and enforce the negative liberty interpretation of Article 3, pushing for stricter adherence to due process and limitations on state power. They shape the discourse and legal challenges around the constraint.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, human_rights_advocates, agenda_setter,
    organized, generational, mobile, global).

% These regimes fundamentally reject or ignore the negative liberty principles of Article 3, viewing individual rights as subordinate to state power or collective will. They are excluded from the legitimate discourse around the constraint's application.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, authoritarian_regimes, excluded,
    institutional, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal baseline for individual protection against arbitrary state power, fostering a shared understanding of fundamental human dignity and limiting state overreach.
% TRANSFER_FUNCTION: Transfers the burden of proof and justification for deprivation of life/liberty from the individual to the state, requiring the state to expend resources on due process and legal safeguards.
% ABSENT_VOICES: Authoritarian regimes and proponents of expansive state security powers are structurally excluded from the legitimate interpretation of this constraint; they would argue for a more permissive view of state action in the name of order or collective good.
% DISAPPEARANCE_RATIONALE: If this reading of Article 3 vanished, states would face fewer constraints on capital punishment, detention without trial, and other forms of state violence. Individual protections would erode, leading to a significant rearrangement of the relationship between citizens and the state, particularly for vulnerable populations.
% FOUNDING_PROBLEM: The problem of arbitrary state violence, torture, and extrajudicial killings, particularly prevalent during and after World War II, where states routinely deprived individuals of life and liberty without due process.
% FOUNDING_PROBLEM_CORROBORATION: Human rights organizations, international legal bodies, and numerous independent reports from outside state security apparatuses consistently attest that arbitrary state violence remains a live problem globally, despite the existence of Article 3. The problem persists in various forms, from police brutality to political imprisonment.
narrative_ontology:disappearance_verdict(udhr_article_3__negative_liberty_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__negative_liberty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__negative_liberty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness is high (0.85) because this reading demands substantial changes to traditional state prerogatives, such as capital punishment abolition and expansive due process, which are 'extracted' from state power. Suppression (0.7) is also high, as states must actively suppress their own impulses for arbitrary action and enforce strict legal frameworks. The theater ratio (0.2) is relatively low, indicating that while some states pay lip service to Article 3, the core enforcement mechanisms are real, though often contested. Resistance (0.8) is high due to ongoing state efforts to circumvent or weaken these protections, particularly in times of perceived crisis.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of individuals, this constraint is a vital Rope or Scaffold, providing essential protection. From the perspective of the state security apparatus, it can feel like a Snare, imposing burdensome restrictions on their ability to maintain order. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Individuals facing state power are clear beneficiaries, as the constraint directly protects them. The state security apparatus and collective security measures are victims, as their operational freedom is curtailed. Human rights advocates act as agenda-setters, pushing for stricter interpretations and enforcement. Authoritarian regimes are excluded, as their practices fundamentally contradict this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_state_violence,
    'Does ''state violence'' in this reading encompass structural violence (e.g., economic policies leading to deprivation) or only direct physical coercion?',
    'Legal precedent and international jurisprudence explicitly expanding the definition of state violence to include structural forms, or a new UN General Assembly resolution clarifying the scope.',
    'If structural violence is included, the extractiveness from the state would increase significantly, as it would be constrained by a broader range of its policies. This could shift the classification towards a more severe Snare for the state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_state_violence, conceptual, 'Ambiguity in the definition of ''state violence'' and its inclusion of structural forms.').

omega_variable(
    capital_punishment_abolition_status,
    'Is capital punishment abolition a necessary implication of this negative liberty reading, or a policy choice separable from it?',
    'A definitive ruling by an international court (e.g., ICC, ICJ) or a widely adopted General Comment by the Human Rights Committee explicitly stating that capital punishment is a per se violation of Article 3.',
    'If abolition is a necessary implication, the extractiveness from states that retain capital punishment would be higher, and their non-compliance more severe. If separable, the constraint''s impact on such states would be less direct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capital_punishment_abolition_status, empirical, 'Whether capital punishment is inherently prohibited by the negative liberty reading.').

omega_variable(
    natural_law_vs_construct,
    'Is the prohibition against arbitrary deprivation of life/liberty a natural law, or a constructed norm?',
    'Philosophical consensus on the existence of inherent, pre-political rights, or empirical evidence of universal, cross-cultural adherence to these principles independent of legal codification.',
    'If a natural law, the constraint would be closer to a Mountain, with lower extractiveness (as it''s ''just the way things are''). If a construct, its persistence depends more on active enforcement and political will, reinforcing its Tangled Rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_construct, conceptual, 'The ontological status of the constraint: natural law or human construct.').


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
narrative_ontology:measurement(udhr_tr_t1988, udhr_article_3__negative_liberty_reading, theater_ratio, 1988, 0.18).
narrative_ontology:measurement(udhr_tr_t2008, udhr_article_3__negative_liberty_reading, theater_ratio, 2008, 0.2).
narrative_ontology:measurement(udhr_tr_t2024, udhr_article_3__negative_liberty_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(udhr_be_t1948, udhr_article_3__negative_liberty_reading, base_extractiveness, 1948, 0.7).
narrative_ontology:measurement(udhr_be_t1968, udhr_article_3__negative_liberty_reading, base_extractiveness, 1968, 0.75).
narrative_ontology:measurement(udhr_be_t1988, udhr_article_3__negative_liberty_reading, base_extractiveness, 1988, 0.8).
narrative_ontology:measurement(udhr_be_t2008, udhr_article_3__negative_liberty_reading, base_extractiveness, 2008, 0.83).
narrative_ontology:measurement(udhr_be_t2024, udhr_article_3__negative_liberty_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t1948, udhr_article_3__negative_liberty_reading, suppression_requirement, 1948, 0.5).
narrative_ontology:measurement(udhr_su_t1968, udhr_article_3__negative_liberty_reading, suppression_requirement, 1968, 0.58).
narrative_ontology:measurement(udhr_su_t1988, udhr_article_3__negative_liberty_reading, suppression_requirement, 1988, 0.65).
narrative_ontology:measurement(udhr_su_t2008, udhr_article_3__negative_liberty_reading, suppression_requirement, 2008, 0.68).
narrative_ontology:measurement(udhr_su_t2024, udhr_article_3__negative_liberty_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__negative_liberty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, udhr_article_5__torture_prohibition).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, icpr_article_6__right_to_life).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
