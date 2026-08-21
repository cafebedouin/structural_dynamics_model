% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__overdetermined_composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__overdetermined_composite_reading, []).

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
 *   constraint_id: dueling_disappearance_mechanism__overdetermined_composite_reading
 *   human_readable: Dueling's Overdetermined Disappearance (Composite Reading)
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint models the overdetermined decline of dueling, positing
 *   that multiple independent factors (legal prohibition, institutional
 *   modernization, cultural shifts, and Civil War trauma) converged to make
 *   the practice obsolete. It is a reading of the
 *   'dueling_disappearance_mechanism' kernel, emphasizing the composite and
 *   simultaneous nature of these causal pathways. The claimed type is
 *   'tangled_rope' because the decline involved both genuine coordination
 *   towards a new social order and extraction from those whose honor-based
 *   practices were suppressed.
 *
 * KEY AGENTS:
 *   - modern_legal_system: Agenda setter (institutional/mobile) — enforced prohibitions
 *   - bourgeois_cultural_elites: Beneficiary (powerful/arbitrage) — promoted new norms
 *   - post_civil_war_social_order: Beneficiary (institutional/constrained) — trauma shifted values
 *   - honor_culture_adherents: Payer (powerless/identity_locked) — lost means of honor defense
 *   - traditional_aristocracy: Payer (moderate/constrained) — saw status mechanism erode
 *   - historical_sociologists: Observer (analytical/analytical) — analyze the complex interplay
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.6).
domain_priors:suppression_score(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.7).
domain_priors:theater_ratio(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__overdetermined_composite_reading, "Dueling's Overdetermined Disappearance (Composite Reading)").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__overdetermined_composite_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:requires_active_enforcement(dueling_disappearance_mechanism__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__overdetermined_composite_reading, '1de3172a-8df6-4501-a1a3-286aa100eb42').
narrative_ontology:cs_kernel_codification('1de3172a-8df6-4501-a1a3-286aa100eb42', implicit).
narrative_ontology:cs_authority_grounding('1de3172a-8df6-4501-a1a3-286aa100eb42', distributed).
narrative_ontology:cs_reading_relation('1de3172a-8df6-4501-a1a3-286aa100eb42', dueling_disappearance_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('1de3172a-8df6-4501-a1a3-286aa100eb42', dueling_disappearance_mechanism__institutional_displacement_reading, coexists_with).
narrative_ontology:cs_axiom('1de3172a-8df6-4501-a1a3-286aa100eb42', foundational, multiple_independent_sufficient_causes).
narrative_ontology:cs_axiom_status(multiple_independent_sufficient_causes, holdable).
narrative_ontology:cs_axiom_grounding('1de3172a-8df6-4501-a1a3-286aa100eb42', multiple_independent_sufficient_causes, empirically_contingent).
narrative_ontology:cs_axiom('1de3172a-8df6-4501-a1a3-286aa100eb42', foundational, simultaneous_causal_action).
narrative_ontology:cs_axiom_status(simultaneous_causal_action, holdable).
narrative_ontology:cs_axiom_grounding('1de3172a-8df6-4501-a1a3-286aa100eb42', simultaneous_causal_action, empirically_contingent).
narrative_ontology:cs_reference_frame('1de3172a-8df6-4501-a1a3-286aa100eb42', pre_decline_dueling_legitimacy).
narrative_ontology:cs_drift_state('1de3172a-8df6-4501-a1a3-286aa100eb42', post_civil_war_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('1de3172a-8df6-4501-a1a3-286aa100eb42', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, modern_legal_system).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, bourgeois_cultural_elites).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, post_civil_war_social_order).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, honor_culture_adherents).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, traditional_aristocracy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefited from the decline of dueling by consolidating its monopoly on legitimate violence and dispute resolution. Actively enforced legal prohibitions against dueling, gradually making it untenable.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, modern_legal_system, agenda_setter,
    institutional, generational, mobile, national).

% Promoted new cultural norms emphasizing self-control, rationality, and dignity over honor, which undermined the social legitimacy of dueling. Their rising influence coincided with dueling's decline.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, bourgeois_cultural_elites, beneficiary,
    powerful, biographical, arbitrage, regional).

% The trauma and scale of the Civil War rendered individual acts of violence like dueling trivial or inappropriate, contributing to a broader cultural shift away from such practices. This new order benefited from a more unified and less internally violent society.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, post_civil_war_social_order, beneficiary,
    institutional, generational, constrained, national).

% Lost a central mechanism for defending their honor and social standing. They were increasingly marginalized by legal prohibitions and cultural shifts, finding fewer avenues to resolve disputes according to their traditional code. Their identity was deeply tied to the practice.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, honor_culture_adherents, payer,
    powerless, biographical, identity_locked, local).

% Saw their social and political influence wane as dueling, a hallmark of their status and a means of maintaining social hierarchy, became illegal and culturally disfavored. They faced pressure to conform to new norms or risk legal and social ostracization.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, traditional_aristocracy, payer,
    moderate, generational, constrained, national).

% Analyze the complex interplay of legal, cultural, and social factors that led to dueling's decline, seeking to understand the causal mechanisms and their relative contributions. They are outside the direct influence of the constraint but study its historical effects.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The composite of mechanisms coordinated a shift in societal norms and legal frameworks, moving away from private violence as a legitimate means of dispute resolution towards state-sanctioned legal processes and new forms of social arbitration.
% TRANSFER_FUNCTION: Transferred the authority for dispute resolution and the maintenance of social order from individual honor codes and aristocratic practices to the modern state and bourgeois cultural institutions. It also transferred the social cost of violence from private duels to state-controlled legal and penal systems.
% ABSENT_VOICES: Those who continued to adhere strictly to honor culture, particularly in isolated or traditional communities, found their voices increasingly silenced or dismissed by the dominant legal and cultural narratives. Their resistance was often met with legal sanction or social ridicule.
% DISAPPEARANCE_RATIONALE: If the composite of legal, cultural, and social pressures that led to dueling's decline had not emerged, honor culture might have persisted longer, legal systems might have faced greater challenges to their authority, and social interactions might have retained a different, more confrontational character. The world would have rearranged around a different set of dispute resolution mechanisms.
% FOUNDING_PROBLEM: The problem was the perceived need to transition from a society where private violence (dueling) was a legitimate means of dispute resolution to one where the state held a monopoly on legitimate force and social order was maintained through legal and cultural means.
% FOUNDING_PROBLEM_CORROBORATION: Historical legal records, sociological analyses of cultural shifts, and accounts of post-Civil War societal changes corroborate the existence and resolution of this problem. Scholars from various disciplines attest to the multifaceted nature of the transition, confirming that the problem of establishing state authority over private violence was indeed addressed by these converging factors.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__overdetermined_composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__overdetermined_composite_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__overdetermined_composite_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dueling_disappearance_mechanism__overdetermined_composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__overdetermined_composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dueling_disappearance_mechanism__overdetermined_composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dueling_disappearance_mechanism__overdetermined_composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.6) because the decline of dueling involved a significant transfer of social power and legitimacy from traditional honor-based systems to modern state and cultural institutions. Suppression is high (0.7) due to the active legal prohibitions and the pervasive cultural shift that made dueling socially unacceptable. Theater ratio is low (0.1) because the decline was a genuine, multifaceted process, not merely performative; the mechanisms actively dismantled the practice. The extractiveness peaks around the Civil War era, reflecting the intensified pressure against dueling, then slightly declines as the new social order solidifies and the 'extraction' becomes less about active suppression and more about the established order.
 *
 * PERSPECTIVAL GAP:
 *   The 'modern_legal_system' and 'bourgeois_cultural_elites' would experience this as a beneficial coordination towards a more rational and orderly society, while 'honor_culture_adherents' and 'traditional_aristocracy' would experience it as a loss of status and a suppression of their traditional means of maintaining honor. The engine's per-seat classification will reflect these divergent experiences based on their declared power, exit options, and roles.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'modern_legal_system' and 'bourgeois_cultural_elites' are beneficiaries, as the decline of dueling consolidated their power and cultural norms. The 'post_civil_war_social_order' also benefited from a reduction in internal conflict. 'Honor_culture_adherents' and 'traditional_aristocracy' are victims, as their social practices and status were directly undermined. The 'identity_locked' exit option for honor culture adherents reflects the deep personal and social cost of abandoning dueling for those whose identity was intertwined with it.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the decline as purely a 'rope' (simple coordination) by acknowledging the significant extraction from those whose cultural practices were suppressed. It also avoids mislabeling it as a 'snare' (pure extraction) by recognizing the genuine coordination function of establishing a new, less violent social order. The overdetermined nature means no single mechanism was solely responsible, making it a 'tangled_rope' of converging forces.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_weight_of_mechanisms,
    'What was the relative causal weight of each contributing factor (legal prohibition, cultural shift, institutional modernization, Civil War trauma) in dueling''s decline?',
    'Counterfactual historical analysis, comparative studies across different regions/nations where some factors were absent or weaker, or quantitative historical sociology if data permits.',
    'Resolving this would refine the understanding of which beneficiaries/victims were most directly affected by which mechanism, potentially shifting the balance of extractiveness or suppression attributed to specific actors.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(causal_weight_of_mechanisms, empirical, 'Uncertainty regarding the precise contribution of each overdetermining cause.').

omega_variable(
    separability_of_causal_pathways,
    'Are the identified causal pathways truly independent, or do they share deeper underlying causes that would simplify the composite into a more fundamental mechanism?',
    'Deeper theoretical work in historical sociology and cultural theory to identify common generative principles or meta-mechanisms that link the seemingly independent factors.',
    'If a deeper, unifying cause is found, the constraint might be reclassified as a simpler type (e.g., a ''rope'' if the underlying cause is pure coordination, or a ''snare'' if it''s a single, hidden extractive force).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separability_of_causal_pathways, conceptual, 'Ambiguity regarding the true independence of the overdetermining factors.').

omega_variable(
    victim_set_clarity,
    'Given the overdetermined nature, is the victim set truly ''honor_culture_adherents'' and ''traditional_aristocracy'', or are there other, less obvious groups who bore costs from the composite decline?',
    'Micro-historical studies of specific communities or social strata during the period of decline, focusing on unrecorded or marginalized groups.',
    'A broader victim set would increase the overall extractiveness and potentially shift the classification towards a ''snare'' if the newly identified victims are more numerous or more severely impacted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_clarity, empirical, 'Uncertainty about the full scope of those negatively impacted by dueling''s decline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__overdetermined_composite_reading, 1800, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t1800, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1800, 0.2).
narrative_ontology:measurement(duel_tr_t1820, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1820, 0.18).
narrative_ontology:measurement(duel_tr_t1840, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1840, 0.15).
narrative_ontology:measurement(duel_tr_t1860, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1860, 0.1).
narrative_ontology:measurement(duel_tr_t1880, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1880, 0.08).
narrative_ontology:measurement(duel_tr_t1900, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1900, 0.1).

% Extraction over time
narrative_ontology:measurement(duel_be_t1800, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1800, 0.4).
narrative_ontology:measurement(duel_be_t1820, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1820, 0.48).
narrative_ontology:measurement(duel_be_t1840, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1840, 0.55).
narrative_ontology:measurement(duel_be_t1860, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1860, 0.6).
narrative_ontology:measurement(duel_be_t1880, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1880, 0.65).
narrative_ontology:measurement(duel_be_t1900, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1900, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t1800, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1800, 0.3).
narrative_ontology:measurement(duel_su_t1820, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1820, 0.45).
narrative_ontology:measurement(duel_su_t1840, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1840, 0.6).
narrative_ontology:measurement(duel_su_t1860, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1860, 0.7).
narrative_ontology:measurement(duel_su_t1880, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1880, 0.75).
narrative_ontology:measurement(duel_su_t1900, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1900, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__overdetermined_composite_reading, identity_coordination).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism__contraction_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism__institutional_displacement_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'dueling_disappearance_mechanism' kernel. This 'overdetermined_composite_reading' emphasizes the simultaneous action of multiple causal factors, in contrast to the 'contraction_reading' (cultural shift) and 'institutional_displacement_reading' (institutional substitution).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
