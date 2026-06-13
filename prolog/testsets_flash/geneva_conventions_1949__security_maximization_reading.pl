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
 *   This constraint represents the 'security maximization' reading of the
 *   1949 Geneva Conventions, which posits that international humanitarian law
 *   must yield to operational necessity in asymmetric conflict. It justifies
 *   suspending most protections to maximize state security, leading to an
 *   expansive 'unlawful combatant' category, degraded civilian immunity,
 *   indefinite detention, and normalized coercive interrogation. This reading
 *   is a Snare, as it systematically extracts rights and protections from
 *   identifiable victims under the guise of state security, requiring active
 *   enforcement and suppression of alternative interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, 0.85).
domain_priors:suppression_score(geneva_conventions_1949__security_maximization_reading, 0.92).
domain_priors:theater_ratio(geneva_conventions_1949__security_maximization_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__security_maximization_reading, snare).
narrative_ontology:human_readable(geneva_conventions_1949__security_maximization_reading, "Geneva Conventions (Security Maximization Reading)").
narrative_ontology:topic_domain(geneva_conventions_1949__security_maximization_reading, "international_humanitarian_law/political_philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__security_maximization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__security_maximization_reading, 'de36770d-d6fe-4880-aa45-ff5eeae4e4e6').
narrative_ontology:cs_kernel_codification('de36770d-d6fe-4880-aa45-ff5eeae4e4e6', fixed_text).
narrative_ontology:cs_authority_grounding('de36770d-d6fe-4880-aa45-ff5eeae4e4e6', extraction).
narrative_ontology:cs_interpretation_layer_present('de36770d-d6fe-4880-aa45-ff5eeae4e4e6').
narrative_ontology:cs_reading_relation('de36770d-d6fe-4880-aa45-ff5eeae4e4e6', geneva_conventions_1949__humanitarian_ceiling_reading, forecloses).
narrative_ontology:cs_reading_relation('de36770d-d6fe-4880-aa45-ff5eeae4e4e6', geneva_conventions_1949__conditional_reciprocity_reading, influences).
narrative_ontology:cs_axiom('de36770d-d6fe-4880-aa45-ff5eeae4e4e6', foundational, state_security_is_paramount).
narrative_ontology:cs_axiom_status(state_security_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('de36770d-d6fe-4880-aa45-ff5eeae4e4e6', state_security_is_paramount, instrumental).
narrative_ontology:cs_axiom('de36770d-d6fe-4880-aa45-ff5eeae4e4e6', foundational, asymmetric_conflict_exceptionalism).
narrative_ontology:cs_axiom_status(asymmetric_conflict_exceptionalism, holdable).
narrative_ontology:cs_axiom_grounding('de36770d-d6fe-4880-aa45-ff5eeae4e4e6', asymmetric_conflict_exceptionalism, empirically_contingent).
narrative_ontology:cs_reference_frame('de36770d-d6fe-4880-aa45-ff5eeae4e4e6', unconstrained_sovereign_power).
narrative_ontology:cs_drift_state('de36770d-d6fe-4880-aa45-ff5eeae4e4e6', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('de36770d-d6fe-4880-aa45-ff5eeae4e4e6', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, state_security_apparatus).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, political_leadership).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, unlawful_combatants).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, detainees).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, civilian_populations_in_conflict_zones).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, human_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets international law to prioritize state security above all other considerations, justifying expanded categories of 'unlawful combatants,' indefinite detention, and coercive interrogation. Benefits from maximal operational flexibility and minimal accountability.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, state_security_apparatus, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefits from the perceived ability to protect the state by any means necessary, avoiding domestic and international legal constraints. Uses this reading to legitimize controversial security policies and actions.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, political_leadership, beneficiary,
    institutional, biographical, mobile, national).

% Denied prisoner of war status, habeas corpus, and other fundamental protections. Subject to indefinite detention and coercive interrogation without trial. Bears the full weight of the constraint's suspension of rights.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, unlawful_combatants, payer,
    powerless, immediate, trapped, local).

% Individuals captured in conflict zones, often without clear combatant status, who are subject to prolonged detention without judicial review and potentially coercive interrogation. Their rights are systematically eroded by this reading.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, detainees, payer,
    powerless, immediate, trapped, local).

% Suffer degraded immunity from harm due to doctrines like 'human shields' and expanded acceptance of 'collateral damage.' Their protection is subordinated to military objectives, increasing their vulnerability.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, civilian_populations_in_conflict_zones, payer,
    powerless, immediate, trapped, local).

% Actively resist the erosion of international humanitarian law, documenting abuses and advocating for adherence to established norms. They bear the cost of constant struggle against the constraint's expansive interpretation.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, human_rights_advocates, payer,
    organized, generational, constrained, global).

% Would uphold a more restrictive interpretation of state powers in conflict, but their authority is often challenged or circumvented by states adopting the security maximization reading. Their judgments are frequently dismissed as infringing on national sovereignty.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, international_legal_bodies, excluded,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claims to coordinate state actions in asymmetric conflict by providing a framework that allows for effective counter-terrorism operations, ensuring state survival and security in novel threat environments.
% TRANSFER_FUNCTION: Transfers protections and rights from individuals (detainees, civilians, 'unlawful combatants') to the state (security apparatus, political leadership) in the name of maximizing security. It also transfers the burden of proof for necessity onto the state, which then defines necessity broadly.
% ABSENT_VOICES: Victims of indefinite detention, coercive interrogation, and excessive collateral damage are systematically silenced or dismissed as 'enemy combatants' or 'terrorist sympathizers.' International legal bodies and human rights organizations are often excluded from effective oversight or their findings are rejected as illegitimate.
% DISAPPEARANCE_RATIONALE: If this reading vanished, states would face immediate pressure to re-evaluate their detention policies, interrogation techniques, and rules of engagement. The legal landscape for asymmetric conflict would shift dramatically towards greater protection for individuals, forcing a rearrangement of state security practices and accountability mechanisms.
% FOUNDING_PROBLEM: The perceived inability of traditional international humanitarian law to adequately address the challenges of asymmetric warfare and non-state actors, particularly after 9/11, leading to a perceived 'gap' in legal frameworks for counter-terrorism.
% FOUNDING_PROBLEM_CORROBORATION: State security agencies and political leaders consistently attest that the problem of asymmetric threats is live and evolving, requiring flexible interpretations of international law. Human rights organizations and international legal scholars, however, contest this, arguing that existing frameworks are sufficient and that the 'problem' is a pretext for rights erosion.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__security_maximization_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__security_maximization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__security_maximization_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(geneva_conventions_1949__security_maximization_reading, 'none', 1).

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
 *   The extractiveness (0.85) is high because fundamental rights are systematically denied to large classes of individuals. Suppression (0.92) is very high, as this reading actively suppresses alternative legal interpretations and dissent through legal and political means, often by labeling critics as undermining national security. The theater ratio (0.65) is also high, reflecting that while some aspects of the Conventions are nominally upheld, the core protections are often circumvented or reinterpreted to serve security objectives, making compliance largely performative. The claimed type is 'snare' because the coordination story (effective counter-terrorism) is a cover for systematic extraction from identifiable victims.
 *
 * PERSPECTIVAL GAP:
 *   The state security apparatus and political leadership experience this as a necessary and legitimate framework for national defense, providing essential flexibility. Conversely, 'unlawful combatants,' detainees, and civilian populations experience it as a severe and arbitrary denial of fundamental rights, with no effective recourse. Human rights advocates perceive it as a dangerous erosion of international law. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The state security apparatus and political leadership are clear beneficiaries (d=0.0-0.2), gaining maximal operational flexibility and reduced accountability. 'Unlawful combatants,' detainees, and civilian populations are direct targets (d=0.9-1.0), bearing the full cost of denied protections. Human rights advocates are also targets (d=0.7-0.8), as their efforts to uphold humanitarian law are actively resisted and undermined. International legal bodies are excluded, their authority often bypassed.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not a case of mandatrophy, but rather a contested interpretation of an existing mandate. The original mandate of the Geneva Conventions was to protect victims of armed conflict. This reading actively redefines the 'problem' to justify a suspension of that mandate, transforming a potential Rope (humanitarian law as coordination) into a Snare by leveraging perceived 'necessity.' The classification as Snare prevents mislabeling this as a legitimate coordination mechanism or a degraded Piton; it is an active, extractive reinterpretation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_vs_pretext,
    'Is the ''operational necessity'' claimed by this reading a genuine, irreducible constraint of asymmetric conflict, or a pretext for expanding state power and reducing accountability?',
    'Independent, retrospective analysis of conflict outcomes in jurisdictions that adopted this reading versus those that maintained stricter adherence to humanitarian law. Examination of whether expanded powers demonstrably led to greater security or merely increased human rights violations.',
    'If a pretext, the constraint''s extractiveness and suppression are even more egregious, and its coordination function is entirely theatrical. If genuine, the classification might shift towards a Tangled Rope, acknowledging a difficult coordination problem with unavoidable costs, though still highly extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_vs_pretext, empirical, 'Whether ''operational necessity'' is a genuine constraint or a justification for extraction.').

omega_variable(
    interpretation_legitimacy,
    'Does this ''security maximization'' reading represent a legitimate evolution of international law in response to new threats, or a unilateral reinterpretation that undermines the foundational principles of humanitarian law?',
    'Consensus among international legal scholars, rulings by international courts (if accepted), and the adoption of new, universally ratified treaties that explicitly incorporate or reject this reading''s tenets.',
    'If deemed a legitimate evolution, its claimed coordination function gains more weight, potentially shifting it towards a Tangled Rope. If deemed illegitimate, its Snare classification is reinforced, highlighting its reliance on coercion and suppression of dissent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpretation_legitimacy, conceptual, 'Legitimacy of reinterpreting humanitarian law for security maximization.').

omega_variable(
    kernel_reading_difference,
    'This constraint is the ''security_maximization_reading'' of the ''geneva_conventions_1949'' kernel. How would the classification change if the ''humanitarian_ceiling_reading'' were adopted?',
    'Adopting the ''humanitarian_ceiling_reading'' would mean prioritizing absolute humanitarian minimums. This would drastically reduce extractiveness and suppression, likely reclassifying the constraint as a Rope or even a Mountain (for core, non-negotiable protections).',
    'A shift to the humanitarian ceiling would invert the beneficiary/victim structure, making individuals the primary beneficiaries and states the constrained parties, leading to a much lower extractiveness and suppression profile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_difference, conceptual, 'Impact of adopting the humanitarian ceiling reading on classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__security_maximization_reading, 2001, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t2001, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2001, 0.4).
narrative_ontology:measurement(gene_tr_t2007, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2007, 0.6).
narrative_ontology:measurement(gene_tr_t2013, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2013, 0.7).
narrative_ontology:measurement(gene_tr_t2018, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2018, 0.68).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2024, 0.65).

% Extraction over time
narrative_ontology:measurement(gene_be_t2001, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2001, 0.7).
narrative_ontology:measurement(gene_be_t2007, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2007, 0.8).
narrative_ontology:measurement(gene_be_t2013, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2013, 0.88).
narrative_ontology:measurement(gene_be_t2018, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2018, 0.86).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t2001, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2001, 0.75).
narrative_ontology:measurement(gene_su_t2007, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2007, 0.9).
narrative_ontology:measurement(gene_su_t2013, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2013, 0.95).
narrative_ontology:measurement(gene_su_t2018, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2018, 0.93).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__security_maximization_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949__humanitarian_ceiling_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949__conditional_reciprocity_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, international_criminal_court_jurisdiction).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, habeas_corpus_protections).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 1949 Geneva Conventions kernel. Its high extractiveness and suppression contrast sharply with the 'humanitarian_ceiling_reading' (lower extraction, higher protection) and the 'conditional_reciprocity_reading' (variable extraction based on adversary compliance). All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
