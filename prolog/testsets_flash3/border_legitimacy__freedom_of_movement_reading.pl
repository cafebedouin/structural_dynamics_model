% ============================================================================
% CONSTRAINT STORY: border_legitimacy__freedom_of_movement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__freedom_of_movement_reading, []).

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
 *   constraint_id: border_legitimacy__freedom_of_movement_reading
 *   human_readable: Border Restrictions (Freedom of Movement Reading)
 *   domain: political_philosophy/migration_studies/international_law
 *
 * SUMMARY:
 *   This constraint story analyzes border restrictions from the 'freedom of
 *   movement' reading of the border legitimacy kernel. From this perspective,
 *   freedom of movement is a fundamental human right, and national borders
 *   represent presumptively illegitimate restrictions on this right. The
 *   constraint is classified as a Snare due to its high extractiveness and
 *   suppression, primarily targeting migrants seeking entry, but also
 *   indirectly impacting displaced workers and welfare recipients in
 *   destination states by framing migrants as a threat. The claimed type
 *   'snare' reflects this reading's interpretation of the structural reality,
 *   independent of the official justifications.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__freedom_of_movement_reading, 0.85).
domain_priors:suppression_score(border_legitimacy__freedom_of_movement_reading, 0.92).
domain_priors:theater_ratio(border_legitimacy__freedom_of_movement_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__freedom_of_movement_reading, snare).
narrative_ontology:human_readable(border_legitimacy__freedom_of_movement_reading, "Border Restrictions (Freedom of Movement Reading)").
narrative_ontology:topic_domain(border_legitimacy__freedom_of_movement_reading, "political_philosophy/migration_studies/international_law").

domain_priors:requires_active_enforcement(border_legitimacy__freedom_of_movement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__freedom_of_movement_reading, '271a818f-5f48-4226-81da-449a3ca783f8').
narrative_ontology:cs_kernel_codification('271a818f-5f48-4226-81da-449a3ca783f8', formalized).
narrative_ontology:cs_authority_grounding('271a818f-5f48-4226-81da-449a3ca783f8', extraction).
narrative_ontology:cs_interpretation_layer_present('271a818f-5f48-4226-81da-449a3ca783f8').
narrative_ontology:cs_reading_relation('271a818f-5f48-4226-81da-449a3ca783f8', border_legitimacy__sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('271a818f-5f48-4226-81da-449a3ca783f8', border_legitimacy__humanitarian_obligation_reading, influences).
narrative_ontology:cs_axiom('271a818f-5f48-4226-81da-449a3ca783f8', foundational, freedom_of_movement_is_a_universal_human_right).
narrative_ontology:cs_axiom_status(freedom_of_movement_is_a_universal_human_right, holdable).
narrative_ontology:cs_axiom_grounding('271a818f-5f48-4226-81da-449a3ca783f8', freedom_of_movement_is_a_universal_human_right, deontological).
narrative_ontology:cs_axiom('271a818f-5f48-4226-81da-449a3ca783f8', foundational, borders_are_presumptively_illegitimate_restrictions).
narrative_ontology:cs_axiom_status(borders_are_presumptively_illegitimate_restrictions, holdable).
narrative_ontology:cs_axiom_grounding('271a818f-5f48-4226-81da-449a3ca783f8', borders_are_presumptively_illegitimate_restrictions, deontological).
narrative_ontology:cs_reference_frame('271a818f-5f48-4226-81da-449a3ca783f8', universal_human_rights_framework).
narrative_ontology:cs_drift_state('271a818f-5f48-4226-81da-449a3ca783f8', contemporary_global_politics, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('271a818f-5f48-4226-81da-449a3ca783f8', '').
narrative_ontology:cs_kernel_id(border_legitimacy__freedom_of_movement_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, state_security_apparatus).
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, domestic_labor_market_insiders).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, migrants_seeking_entry).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, displaced_workers_in_destination_states).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, welfare_recipients_in_destination_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals attempting to cross borders for economic opportunity, safety, or family reunification. They face legal barriers, physical dangers, and often exploitation, bearing the direct costs of border enforcement through denied entry, detention, or forced return. Their freedom of movement is directly curtailed.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, migrants_seeking_entry, payer,
    powerless, immediate, trapped, global).

% Government agencies responsible for border control, surveillance, and enforcement. They benefit from expanded budgets, personnel, and technological capabilities justified by the need to 'secure' borders. They actively enforce the restrictions.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, state_security_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Workers in destination countries who perceive their wages and job security as protected by restricted immigration. They benefit from reduced competition for certain jobs and potentially higher bargaining power, though this benefit is often contested by economic analysis.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, domestic_labor_market_insiders, beneficiary,
    organized, biographical, mobile, national).

% Citizens or legal residents in destination countries whose jobs are displaced by automation or economic shifts, and who are then told that migrants are the cause of their economic insecurity. They are victims of a narrative that deflects responsibility for structural economic problems onto migrants, leading to social division and political instability.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, displaced_workers_in_destination_states, payer,
    powerless, biographical, constrained, national).

% Citizens or legal residents who rely on social welfare programs and are told that migrants are a drain on public resources. They are victims of a narrative that frames social support as a zero-sum game, fostering resentment and diverting attention from systemic issues.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, welfare_recipients_in_destination_states, payer,
    powerless, biographical, constrained, national).

% Organizations and individuals who champion the universal right to freedom of movement. They document abuses, lobby governments, and challenge restrictive policies in international forums, but lack direct enforcement power.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, international_human_rights_advocates, observer,
    moderate, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading's perspective, the constraint primarily coordinates the exclusion of non-citizens to maintain existing national social and economic structures, rather than solving a genuine collective action problem for all humanity.
% TRANSFER_FUNCTION: Transfers security and perceived stability to existing citizens (especially those in the domestic labor market and those who benefit from the narrative of scarcity) by extracting freedom, safety, and opportunity from migrants seeking entry, and by deflecting blame for domestic economic issues onto migrants from displaced workers and welfare recipients.
% ABSENT_VOICES: The voices of future generations, who might inherit a world with more fluid movement and less nationalistic division, are absent. Also, the voices of those who would benefit from the economic and cultural contributions of migrants, but whose interests are currently overshadowed by protectionist narratives.
% DISAPPEARANCE_RATIONALE: If border restrictions vanished overnight, global migration patterns would shift dramatically, leading to significant demographic, economic, and cultural reorganization in many states. Labor markets would rebalance, and the concept of national citizenship would be fundamentally altered.
% FOUNDING_PROBLEM: The constraint of border restrictions was historically established to define and protect national sovereignty, control populations, and manage resources within defined territories.
% FOUNDING_PROBLEM_CORROBORATION: States and their security apparatuses attest that the founding problem of maintaining national integrity and security is live. International human rights advocates and some economists argue that the problem is largely superseded by universal human rights principles and global economic interdependence, and that the constraint now serves primarily extractive functions for specific domestic groups.
narrative_ontology:disappearance_verdict(border_legitimacy__freedom_of_movement_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__freedom_of_movement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__freedom_of_movement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(border_legitimacy__freedom_of_movement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__freedom_of_movement_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__freedom_of_movement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__freedom_of_movement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__freedom_of_movement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because the constraint imposes severe costs on migrants (denial of rights, danger, exploitation) while benefiting specific domestic groups (state security apparatus, certain labor market segments) and narratives. Suppression (0.92) is extremely high, as states employ significant coercive force (physical barriers, legal penalties, surveillance) to enforce borders, with very limited exit options for those seeking entry. The theater ratio (0.1) is low, indicating that border enforcement is largely functional in its suppressive aim, with little performative maintenance for a defunct purpose; it is actively maintained for its current extractive function. Accessibility collapse (0.75) is high because legal and safe alternatives to migration are severely limited once the constraint is understood. Resistance (0.8) is high, reflecting the ongoing efforts of migrants and advocates to challenge and circumvent these restrictions.
 *
 * PERSPECTIVAL GAP:
 *   The state security apparatus and domestic labor market insiders would experience this as a legitimate, necessary constraint (perhaps a Rope or even a Mountain of national security). Migrants, displaced workers, and welfare recipients, however, experience it as a highly extractive Snare. The engine's per-seat classification will reflect this divergence based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Migrants seeking entry are full targets (high d) as they bear the direct costs. The state security apparatus is a clear beneficiary (low d) due to increased power and resources. Domestic labor market insiders are beneficiaries (low d) as they perceive protection from competition. Displaced workers and welfare recipients are victims (high d) as they are used as rhetorical justification for the constraint, bearing the social and economic costs of division. International human rights advocates are observers (analytical d).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading argues that the original mandate of borders (national protection) has been superseded by universal human rights, and the constraint now functions primarily as an extractive mechanism. The high extractiveness and suppression, coupled with the 'contested' status of the founding problem, suggest a Snare rather than a Rope or Piton. The constraint is actively maintained and benefits identifiable parties, preventing it from being a Piton. It lacks a genuine, universally beneficial coordination function, distinguishing it from a Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_right_vs_social_construct,
    'Is freedom of movement an inherent, natural human right, or a socially constructed right contingent on state recognition and capacity?',
    'Philosophical consensus on the foundations of human rights, or a global legal framework that universally codifies and enforces freedom of movement independent of state borders.',
    'If a natural right, the extractiveness of border restrictions is inherently higher and their legitimacy lower. If a social construct, the extractiveness is contingent on the terms of the social contract, potentially lowering the classification to a Tangled Rope or even a Rope under certain conditions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_right_vs_social_construct, conceptual, 'The fundamental nature of freedom of movement as a right.').

omega_variable(
    economic_impact_on_destination_states,
    'What is the net economic impact of open borders on destination states, considering both costs (e.g., welfare, infrastructure) and benefits (e.g., labor, innovation, taxes)?',
    'Comprehensive, long-term empirical studies across diverse economic contexts, accounting for both direct and indirect effects, and avoiding selection bias.',
    'If net positive, the justification for border restrictions based on economic burden is undermined, increasing the perceived extractiveness. If net negative, it could lend some legitimacy to restrictions, potentially lowering the extractiveness from this reading''s perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_impact_on_destination_states, empirical, 'The true economic consequences of migration for destination countries.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of migrants structural (external barriers, legal penalties) or internalized (fear, hopelessness, identity fusion with ''illegal'' status)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed (e.g., migrants who gain legal status still self-censor or fear authorities), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the Snare more insidious.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for migrants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__freedom_of_movement_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1948, border_legitimacy__freedom_of_movement_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(bord_tr_t1970, border_legitimacy__freedom_of_movement_reading, theater_ratio, 1970, 0.18).
narrative_ontology:measurement(bord_tr_t1990, border_legitimacy__freedom_of_movement_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(bord_tr_t2010, border_legitimacy__freedom_of_movement_reading, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(bord_tr_t2024, border_legitimacy__freedom_of_movement_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(bord_be_t1948, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 1948, 0.7).
narrative_ontology:measurement(bord_be_t1970, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 1970, 0.75).
narrative_ontology:measurement(bord_be_t1990, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 1990, 0.8).
narrative_ontology:measurement(bord_be_t2010, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 2010, 0.83).
narrative_ontology:measurement(bord_be_t2024, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1948, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 1948, 0.75).
narrative_ontology:measurement(bord_su_t1970, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 1970, 0.8).
narrative_ontology:measurement(bord_su_t1990, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 1990, 0.85).
narrative_ontology:measurement(bord_su_t2010, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 2010, 0.9).
narrative_ontology:measurement(bord_su_t2024, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__freedom_of_movement_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, humanitarian_aid_distribution).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, international_labor_laws).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'border_legitimacy' kernel. This 'freedom_of_movement_reading' emphasizes universal human rights and views borders as presumptively illegitimate. It is linked to the 'sovereignty_reading' and 'humanitarian_obligation_reading' as competing interpretations of the same underlying kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
