% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__security_maximization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__security_maximization_reading, []).

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
 *   constraint_id: geneva_conventions_1949__security_maximization_reading
 *   human_readable: Geneva Conventions (Security Maximization Reading)
 *   domain: international_humanitarian_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents a reading of the Geneva Conventions that
 *   prioritizes state security and operational necessity, particularly in
 *   asymmetric conflicts. It views the conventions as flexible guidelines
 *   that must yield to the imperative of maximizing state security, leading
 *   to a significant degradation of protections for 'unlawful combatants' and
 *   civilian populations. The reading justifies practices like indefinite
 *   detention, coercive interrogation, and expanded definitions of collateral
 *   damage. The claimed type is 'snare' because the coordination story
 *   (effective security) is a cover for systematic extraction of rights and
 *   protections from identifiable victims, maintained through active
 *   suppression and the suppression of alternative interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, 0.92).
domain_priors:suppression_score(geneva_conventions_1949__security_maximization_reading, 0.95).
domain_priors:theater_ratio(geneva_conventions_1949__security_maximization_reading, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, theater_ratio, 0.75).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__security_maximization_reading, snare).
narrative_ontology:human_readable(geneva_conventions_1949__security_maximization_reading, "Geneva Conventions (Security Maximization Reading)").
narrative_ontology:topic_domain(geneva_conventions_1949__security_maximization_reading, "international_humanitarian_law/political_philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__security_maximization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__security_maximization_reading, 'facc48c3-186c-4a2f-a441-7f9373cdbb1d').
narrative_ontology:cs_kernel_codification('facc48c3-186c-4a2f-a441-7f9373cdbb1d', fixed_text).
narrative_ontology:cs_authority_grounding('facc48c3-186c-4a2f-a441-7f9373cdbb1d', extraction).
narrative_ontology:cs_interpretation_layer_present('facc48c3-186c-4a2f-a441-7f9373cdbb1d').
narrative_ontology:cs_reading_relation('facc48c3-186c-4a2f-a441-7f9373cdbb1d', geneva_conventions_1949__humanitarian_ceiling_reading, forecloses).
narrative_ontology:cs_reading_relation('facc48c3-186c-4a2f-a441-7f9373cdbb1d', geneva_conventions_1949__conditional_reciprocity_reading, influences).
narrative_ontology:cs_axiom('facc48c3-186c-4a2f-a441-7f9373cdbb1d', foundational, state_security_is_supreme_norm).
narrative_ontology:cs_axiom_status(state_security_is_supreme_norm, holdable).
narrative_ontology:cs_axiom_grounding('facc48c3-186c-4a2f-a441-7f9373cdbb1d', state_security_is_supreme_norm, deontological).
narrative_ontology:cs_axiom('facc48c3-186c-4a2f-a441-7f9373cdbb1d', foundational, asymmetric_conflict_renders_ihl_obsolete).
narrative_ontology:cs_axiom_status(asymmetric_conflict_renders_ihl_obsolete, holdable).
narrative_ontology:cs_axiom_grounding('facc48c3-186c-4a2f-a441-7f9373cdbb1d', asymmetric_conflict_renders_ihl_obsolete, empirically_contingent).
narrative_ontology:cs_reference_frame('facc48c3-186c-4a2f-a441-7f9373cdbb1d', unconstrained_sovereign_power).
narrative_ontology:cs_drift_state('facc48c3-186c-4a2f-a441-7f9373cdbb1d', post_9_11_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('facc48c3-186c-4a2f-a441-7f9373cdbb1d', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, state_security_apparatus).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, political_executive).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, unlawful_combatants).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, detainees_without_trial).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, civilian_populations_in_conflict_zones).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the conventions to prioritize state security, expanding categories like 'unlawful combatant' to justify actions that would otherwise be prohibited. Benefits from reduced legal constraints on operations and intelligence gathering.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, state_security_apparatus, agenda_setter,
    institutional, biographical, arbitrage, national).

% Benefits from the flexibility to respond to perceived threats without being unduly constrained by international law, particularly in asymmetric conflicts. Uses this reading to justify policy decisions to a domestic audience.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, political_executive, beneficiary,
    institutional, immediate, mobile, national).

% Denied prisoner of war status, habeas corpus, and other protections under this reading. Subject to indefinite detention and coercive interrogation without legal recourse.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, unlawful_combatants, payer,
    powerless, immediate, trapped, local).

% Held indefinitely without charges or access to legal process, often in extra-territorial facilities. Their rights are systematically eroded by this interpretation of operational necessity.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, detainees_without_trial, payer,
    powerless, biographical, trapped, local).

% Experience degraded immunity from harm due to expanded 'collateral damage' acceptance and the 'human shields' doctrine. Their safety is subordinated to state security objectives.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, civilian_populations_in_conflict_zones, payer,
    powerless, immediate, constrained, local).

% Advocate for strict adherence to the Geneva Conventions and universal human rights. Their calls for accountability and protection are often dismissed as impractical or naive by proponents of this reading.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, international_humanitarian_organizations, excluded,
    organized, generational, constrained, global).

% Attempt to adjudicate violations of international law, but their jurisdiction and enforcement mechanisms are often challenged or circumvented by states operating under this security maximization reading.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, international_courts_and_tribunals, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claims to coordinate state actions in conflict to ensure maximum security outcomes by removing perceived legal impediments, thereby allowing for more effective counter-insurgency and anti-terrorism operations.
% TRANSFER_FUNCTION: Transfers legal protections and human rights from individuals (detainees, civilians) to the state (security apparatus, political executive) in the name of national security, effectively reallocating the burden of conflict.
% ABSENT_VOICES: Victims of state violence, human rights advocates, and international legal scholars who uphold a more robust interpretation of IHL are systematically marginalized or dismissed. Their perspectives are excluded from the policy-making process that adopts this reading.
% DISAPPEARANCE_RATIONALE: If this reading vanished, states would face immediate and significant legal challenges to their current operational doctrines. The categories of 'unlawful combatant' and 'human shields' would lose their legal force, forcing a re-evaluation of detention policies, interrogation techniques, and rules of engagement. This would fundamentally alter the legal and ethical landscape of asymmetric conflict.
% FOUNDING_PROBLEM: The perceived inability of traditional international law to effectively address the challenges posed by non-state actors, asymmetric warfare, and global terrorism, leading to a perceived need for greater state flexibility.
% FOUNDING_PROBLEM_CORROBORATION: Proponents within state security and political executive branches consistently attest to the problem's live status, citing ongoing threats. Critics, including international legal bodies and human rights organizations, argue that the problem is exaggerated or misframed to justify overreach, but acknowledge the existence of new conflict dynamics.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__security_maximization_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__security_maximization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__security_maximization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(geneva_conventions_1949__security_maximization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__security_maximization_reading, 0.92, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__security_maximization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__security_maximization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.92) because this reading systematically strips away fundamental protections from vulnerable populations and individuals, transferring significant power and flexibility to the state. Suppression is also very high (0.95) as this reading requires active legal and political efforts to deny alternative interpretations and to enforce its expansive view of state power. Theater ratio is high (0.75) because while some security functions are real, a substantial portion of the justification and enforcement is performative, aimed at legitimizing actions that would otherwise be clear violations of international law. Accessibility collapse is high (0.88) as this reading effectively closes off legal and practical avenues for victims to assert their rights. Resistance is high (0.70) from international bodies and human rights groups, but this resistance is largely ineffective against the power of states adopting this reading.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state security apparatus, this reading is a necessary adaptation to modern conflict, a 'rope' that enables effective defense. From the perspective of the victims, it is a 'snare' that systematically denies their rights. The engine's classification will reflect the latter due to the high extractiveness and suppression, highlighting the divergence from the claimed 'rope' function.
 *
 * DIRECTIONALITY LOGIC:
 *   The state security apparatus and political executive are clear beneficiaries, gaining operational flexibility and reduced accountability (low directionality). 'Unlawful combatants,' detainees, and civilian populations are the primary targets, bearing the full cost of degraded protections and rights (high directionality). International humanitarian organizations and courts are excluded or observers, attempting to uphold alternative readings but lacking the power to enforce them against states committed to this interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling extraction as coordination by exposing how the 'operational necessity' justification serves to extract rights and protections. The original mandate of the Geneva Conventions was to set humanitarian limits on warfare. This reading actively subverts that mandate, transforming a potential 'rope' (humanitarian ceiling) or 'tangled rope' (conditional reciprocity) into a 'snare' by prioritizing state security above all else. The persistence of this reading is not due to a genuine coordination problem it solves for all parties, but due to the concentrated benefits it provides to powerful state actors at the expense of vulnerable populations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_necessity_scope,
    'What are the objective, empirically verifiable limits of ''operational necessity'' in asymmetric conflict, beyond which actions constitute clear violations of IHL?',
    'Independent, multi-disciplinary expert commissions (military, legal, ethical) establishing clear thresholds and criteria for ''necessity'' that are not self-serving for state actors.',
    'A narrower definition of ''operational necessity'' would significantly reduce the extractiveness and suppression of this reading, potentially shifting its classification towards a ''tangled rope'' or even ''rope'' if genuine coordination benefits could be isolated. A broad, undefined scope maintains its ''snare'' characteristics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_necessity_scope, empirical, 'The true scope of ''operational necessity'' versus its rhetorical expansion.').

omega_variable(
    unlawful_combatant_legitimacy,
    'Is the category of ''unlawful combatant'' a legitimate legal innovation to address new conflict realities, or a legal fiction designed to circumvent IHL protections?',
    'International legal consensus (e.g., ICJ advisory opinions, widespread state practice beyond a few powerful actors) on the precise definition, rights, and obligations of such a category, ensuring it does not create a ''rights vacuum''.',
    'If deemed a legitimate, well-defined category with residual protections, the extractiveness from these individuals would decrease. If deemed a legal fiction, the current high extractiveness and suppression would be further validated, reinforcing the ''snare'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unlawful_combatant_legitimacy, conceptual, 'Legitimacy of ''unlawful combatant'' category.').

omega_variable(
    civilian_protection_vs_collateral_damage,
    'What is the actual proportionality threshold for ''collateral damage'' in asymmetric conflict, and how is it applied in practice versus the ''human shields'' doctrine?',
    'Transparent, independent investigations of specific incidents, coupled with public disclosure of targeting criteria and post-strike assessments, to determine if civilian harm is genuinely unavoidable or systematically accepted.',
    'If civilian harm is found to consistently exceed reasonable proportionality, or the ''human shields'' doctrine is used to justify otherwise unlawful attacks, the extractiveness from civilian populations would be confirmed as severe. If genuine efforts at proportionality are demonstrated, the extractiveness might be slightly mitigated, though still high.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civilian_protection_vs_collateral_damage, empirical, 'Proportionality of collateral damage and use of ''human shields'' doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__security_maximization_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1990, geneva_conventions_1949__security_maximization_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(gene_tr_t1995, geneva_conventions_1949__security_maximization_reading, theater_ratio, 1995, 0.45).
narrative_ontology:measurement(gene_tr_t2001, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2001, 0.6).
narrative_ontology:measurement(gene_tr_t2008, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2008, 0.7).
narrative_ontology:measurement(gene_tr_t2015, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2015, 0.73).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2024, 0.75).

% Extraction over time
narrative_ontology:measurement(gene_be_t1990, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(gene_be_t1995, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 1995, 0.7).
narrative_ontology:measurement(gene_be_t2001, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2001, 0.85).
narrative_ontology:measurement(gene_be_t2008, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2008, 0.9).
narrative_ontology:measurement(gene_be_t2015, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2015, 0.91).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2024, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1990, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(gene_su_t1995, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 1995, 0.75).
narrative_ontology:measurement(gene_su_t2001, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2001, 0.9).
narrative_ontology:measurement(gene_su_t2008, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2008, 0.93).
narrative_ontology:measurement(gene_su_t2015, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2015, 0.94).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__security_maximization_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, humanitarian_ceiling_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, conditional_reciprocity_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, international_criminal_court_jurisdiction).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Geneva Conventions (1949 kernel). This 'security_maximization_reading' directly influences the operational space and legitimacy of the 'humanitarian_ceiling_reading' and 'conditional_reciprocity_reading' by asserting a competing framework for interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
