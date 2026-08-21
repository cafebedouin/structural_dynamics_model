% ============================================================================
% CONSTRAINT STORY: second_amendment_text__originalist_civic_virtue_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__originalist_civic_virtue_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: second_amendment_text__originalist_civic_virtue_reading
 *   human_readable: Second Amendment: Originalist Civic Virtue Reading
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint represents an 'originalist civic virtue' reading of the
 *   Second Amendment, where the 'militia' is understood as the universal
 *   armed citizenry, and the right to bear arms protects this citizen-soldier
 *   capacity for the security of a free state. It is a 'rope' because it
 *   coordinates the civic responsibility of the citizenry with the state's
 *   need for defense, with minimal extraction. The metrics reflect a
 *   relatively stable, low-extraction interpretation, where the primary
 *   'cost' is the civic duty itself, not an imposed burden. This reading
 *   emphasizes the collective political community as the beneficiary, rather
 *   than individual self-defense or state-controlled security forces.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__originalist_civic_virtue_reading, 0.15).
domain_priors:suppression_score(second_amendment_text__originalist_civic_virtue_reading, 0.05).
domain_priors:theater_ratio(second_amendment_text__originalist_civic_virtue_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__originalist_civic_virtue_reading, rope).
narrative_ontology:human_readable(second_amendment_text__originalist_civic_virtue_reading, "Second Amendment: Originalist Civic Virtue Reading").
narrative_ontology:topic_domain(second_amendment_text__originalist_civic_virtue_reading, "constitutional_law/political_theory/firearms_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__originalist_civic_virtue_reading, '707d9094-5853-4760-b47d-3e6b68973426').
narrative_ontology:cs_kernel_codification('707d9094-5853-4760-b47d-3e6b68973426', fixed_text).
narrative_ontology:cs_authority_grounding('707d9094-5853-4760-b47d-3e6b68973426', lineage).
narrative_ontology:cs_interpretation_layer_present('707d9094-5853-4760-b47d-3e6b68973426').
narrative_ontology:cs_reading_relation('707d9094-5853-4760-b47d-3e6b68973426', second_amendment_text__collective_security_reading, coexists_with).
narrative_ontology:cs_reading_relation('707d9094-5853-4760-b47d-3e6b68973426', second_amendment_text__individual_right_reading, coexists_with).
narrative_ontology:cs_axiom('707d9094-5853-4760-b47d-3e6b68973426', foundational, militia_is_universal_citizenry).
narrative_ontology:cs_axiom_status(militia_is_universal_citizenry, holdable).
narrative_ontology:cs_axiom_grounding('707d9094-5853-4760-b47d-3e6b68973426', militia_is_universal_citizenry, conventional).
narrative_ontology:cs_axiom('707d9094-5853-4760-b47d-3e6b68973426', foundational, arms_bearing_for_civic_virtue).
narrative_ontology:cs_axiom_status(arms_bearing_for_civic_virtue, holdable).
narrative_ontology:cs_axiom_grounding('707d9094-5853-4760-b47d-3e6b68973426', arms_bearing_for_civic_virtue, deontological).
narrative_ontology:cs_reference_frame('707d9094-5853-4760-b47d-3e6b68973426', founding_era_civic_republicanism).
narrative_ontology:cs_drift_state('707d9094-5853-4760-b47d-3e6b68973426', contemporary_legal_discourse, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('707d9094-5853-4760-b47d-3e6b68973426', '').
narrative_ontology:cs_kernel_id(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, the_citizenry_qua_political_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, individual_citizens).
narrative_ontology:constraint_victim(second_amendment_text__originalist_civic_virtue_reading, individual_citizens).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, civic_republicanism).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, popular_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the preservation of a well-regulated militia, understood as the armed citizenry, which is seen as essential for civic virtue and the security of a free state. The right is not about individual self-defense primarily, but about the collective capacity for self-governance and defense against tyranny.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, the_citizenry_qua_political_community, beneficiary,
    institutional, generational, identity_locked, national).

% Have the responsibility to organize and regulate the militia, ensuring its effectiveness. This reading grants states significant authority to regulate arms in service of the militia's function, but not to disarm the citizenry entirely. They are constrained by the underlying right of the people to keep and bear arms for this civic purpose.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, state_governments, agenda_setter,
    institutional, generational, constrained, national).

% Bear the responsibility of being part of the armed citizenry, potentially including militia service. They benefit from the security and civic participation this reading implies, but their individual right to bear arms is subordinate to the collective civic purpose, allowing for regulation that might restrict personal preferences.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, individual_citizens, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(second_amendment_text__originalist_civic_virtue_reading, individual_citizens, beneficiary).

% Observes and interprets the Second Amendment, but this reading emphasizes state and local control over the militia. Its role is to ensure the states do not infringe upon the fundamental right of the people to maintain their citizen-soldier capacity.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, federal_government, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the capacity of the citizenry to act as a collective defense force and a check on potential tyranny, ensuring a 'free state' through the civic virtue of an armed populace.
% TRANSFER_FUNCTION: Transfers the responsibility for collective security and civic participation to the armed citizenry, rather than a standing army, fostering a sense of shared duty and republican ideals.
% ABSENT_VOICES: Those who advocate for an unrestricted individual right to bear arms for any purpose, or those who seek complete disarmament, would object. Their perspectives are excluded by the civic-republican framing of this reading.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the understanding of the Second Amendment would shift dramatically, likely towards either an individual self-defense right or a purely state-controlled militia. This would alter the balance of power between citizens and the state, and change the civic expectations of armed citizenry.
% FOUNDING_PROBLEM: The problem of maintaining a free state, preventing tyranny, and ensuring popular sovereignty through a virtuous, armed citizenry capable of self-defense and civic participation, without relying on a potentially oppressive standing army.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the founding era and political theorists specializing in civic republicanism corroborate the historical context and philosophical underpinnings of this problem. Legal scholars who adhere to originalist methodologies also attest to its continued relevance in constitutional interpretation.
narrative_ontology:disappearance_verdict(second_amendment_text__originalist_civic_virtue_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__originalist_civic_virtue_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__originalist_civic_virtue_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(second_amendment_text__originalist_civic_virtue_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__originalist_civic_virtue_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__originalist_civic_virtue_reading_tests).
:- end_tests(second_amendment_text__originalist_civic_virtue_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the 'cost' is primarily the civic duty of an armed citizenry, which is framed as a benefit to the political community. Suppression is low (0.05) as the constraint is understood as a fundamental right and civic responsibility, not something actively enforced against a resisting populace. Theater ratio is low (0.1) because the civic function, while debated in modern context, is genuinely central to this reading's justification. Accessibility collapse is high (0.7) because the idea of a disarmed citizenry is largely incompatible with this reading's core tenets. Resistance is low (0.1) because, within this interpretive framework, the constraint is seen as a foundational principle, not an imposition.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between this reading and those that emphasize either an individual right to self-defense or a purely state-controlled militia. This reading's focus on collective civic virtue means that individual preferences for arms or state regulatory power are subordinated to the broader republican ideal, leading to different classifications from those other perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   The citizenry qua political community is the primary beneficiary, as the constraint is understood to secure their freedom and self-governance. Individual citizens are both beneficiaries (of a free state) and payers (of civic duty), but the overall framing is one of collective benefit. State governments are agenda-setters, tasked with organizing the militia, but their power is constrained by the underlying right. There are no specific 'victims' in this reading, as the constraint is framed as a foundational civic good.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by focusing on the original civic purpose. If the 'militia' were reinterpreted as solely a state-controlled entity, or the right as purely individual self-defense, the constraint's function would shift, and its classification would change. By adhering to the founding-era understanding of the militia as the armed citizenry, it maintains a 'rope' classification tied to a genuine, if historically specific, coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_accuracy_of_universal_militia,
    'To what extent was the ''universal armed citizenry'' truly the dominant understanding of the militia across all founding-era contexts, and how did this understanding evolve?',
    'Further historical and legal scholarship examining state militia laws, contemporary commentaries, and debates across different regions and time periods within the founding era.',
    'If the ''universal armed citizenry'' was less universal or more contested than assumed, the ''rope'' classification might be challenged, potentially revealing elements of ''tangled rope'' if certain groups were systematically excluded from this civic ideal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_accuracy_of_universal_militia, empirical, 'Ambiguity regarding the historical scope and consistency of the ''universal armed citizenry'' interpretation of the militia.').

omega_variable(
    relevance_of_civic_virtue_in_modern_context,
    'Is the concept of ''civic virtue'' as tied to an armed citizenry still a live and relevant coordination function in a modern, complex society with professionalized military and law enforcement?',
    'Philosophical and political theory analysis of contemporary republicanism, empirical studies on civic engagement and public safety, and judicial interpretations of the Second Amendment''s purpose in modern cases.',
    'If the civic virtue function is deemed largely obsolete, the constraint might drift towards a ''piton'' (if maintained purely by inertia) or a ''snare'' (if used as cover for other forms of extraction), as its original coordination justification would be weakened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relevance_of_civic_virtue_in_modern_context, conceptual, 'The conceptual relevance and functional ''liveness'' of the civic virtue aspect of the militia in contemporary society.').

omega_variable(
    relationship_to_individual_self_defense,
    'How does this civic virtue reading accommodate or subordinate the individual right to self-defense, and is this subordination consistent with other constitutional rights?',
    'Comparative constitutional analysis, legal scholarship on the hierarchy of rights, and judicial rulings that explicitly address the tension between collective civic purpose and individual self-defense within the Second Amendment.',
    'If the subordination of individual self-defense is found to be overly extractive or inconsistent with other rights, this reading''s ''rope'' classification could be challenged, potentially revealing ''tangled rope'' elements where individual liberties are unduly constrained for a collective good.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relationship_to_individual_self_defense, conceptual, 'The conceptual tension and structural relationship between the collective civic purpose and individual self-defense within this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__originalist_civic_virtue_reading, 0, 240).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(seco_tr_t60, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(seco_tr_t120, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 120, 0.1).
narrative_ontology:measurement(seco_tr_t180, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 180, 0.1).
narrative_ontology:measurement(seco_tr_t240, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 240, 0.1).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(seco_be_t60, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 60, 0.15).
narrative_ontology:measurement(seco_be_t120, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 120, 0.15).
narrative_ontology:measurement(seco_be_t180, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 180, 0.15).
narrative_ontology:measurement(seco_be_t240, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 240, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(seco_su_t60, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 60, 0.05).
narrative_ontology:measurement(seco_su_t120, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 120, 0.05).
narrative_ontology:measurement(seco_su_t180, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 180, 0.05).
narrative_ontology:measurement(seco_su_t240, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 240, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
