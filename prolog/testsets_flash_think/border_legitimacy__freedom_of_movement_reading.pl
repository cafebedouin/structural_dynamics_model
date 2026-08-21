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
 *   constraint_id: border_legitimacy__freedom_of_movement_reading
 *   human_readable: Border Restrictions (Freedom of Movement Reading)
 *   domain: political_philosophy/migration_studies/international_law
 *
 * SUMMARY:
 *   This constraint story instantiates the 'freedom of movement' reading of
 *   the 'border legitimacy' kernel. From this perspective, national borders,
 *   when enforced to restrict human movement, are presumptively illegitimate
 *   and constitute a violation of a fundamental human right. The constraint
 *   is viewed as a coercive mechanism that extracts freedom and opportunity
 *   from migrants and displaced persons, while benefiting state apparatuses
 *   and certain segments of the citizenry who perceive advantages from
 *   exclusion. The high extractiveness and suppression reflect the severe
 *   impact on those whose movement is restricted and the active, often
 *   violent, enforcement required to maintain these restrictions.
 *
 * KEY AGENTS:
 *   - migrants_and_asylum_seekers: Primary target (powerless/trapped) — bears direct costs of exclusion.
 *   - displaced_workers_in_origin_countries: Secondary target (powerless/trapped) — denied economic opportunity.
 *   - state_security_apparatus: Primary beneficiary/agenda_setter (institutional/arbitrage) — gains power and resources from enforcement.
 *   - current_citizens_who_benefit_from_exclusion: Secondary beneficiary (organized/mobile) — perceives benefits from restricted immigration.
 *   - human_rights_advocates: Analytical observer (moderate/analytical) — challenges the constraint's legitimacy.
 *   - global_poor: Excluded voice (powerless/trapped) — most impacted but absent from discourse.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__freedom_of_movement_reading, 0.85).
domain_priors:suppression_score(border_legitimacy__freedom_of_movement_reading, 0.9).
domain_priors:theater_ratio(border_legitimacy__freedom_of_movement_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__freedom_of_movement_reading, snare).
narrative_ontology:human_readable(border_legitimacy__freedom_of_movement_reading, "Border Restrictions (Freedom of Movement Reading)").
narrative_ontology:topic_domain(border_legitimacy__freedom_of_movement_reading, "political_philosophy/migration_studies/international_law").

domain_priors:requires_active_enforcement(border_legitimacy__freedom_of_movement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__freedom_of_movement_reading, '03fe6e8c-e642-4a48-9a0d-8317f43e9ac1').
narrative_ontology:cs_kernel_codification('03fe6e8c-e642-4a48-9a0d-8317f43e9ac1', formalized).
narrative_ontology:cs_authority_grounding('03fe6e8c-e642-4a48-9a0d-8317f43e9ac1', lineage).
narrative_ontology:cs_interpretation_layer_present('03fe6e8c-e642-4a48-9a0d-8317f43e9ac1').
narrative_ontology:cs_reading_relation('03fe6e8c-e642-4a48-9a0d-8317f43e9ac1', border_legitimacy__sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('03fe6e8c-e642-4a48-9a0d-8317f43e9ac1', border_legitimacy__humanitarian_obligation_reading, coexists_with).
narrative_ontology:cs_axiom('03fe6e8c-e642-4a48-9a0d-8317f43e9ac1', foundational, freedom_of_movement_is_human_right).
narrative_ontology:cs_axiom_status(freedom_of_movement_is_human_right, holdable).
narrative_ontology:cs_axiom_grounding('03fe6e8c-e642-4a48-9a0d-8317f43e9ac1', freedom_of_movement_is_human_right, deontological).
narrative_ontology:cs_axiom('03fe6e8c-e642-4a48-9a0d-8317f43e9ac1', foundational, borders_presumptively_illegitimate).
narrative_ontology:cs_axiom_status(borders_presumptively_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('03fe6e8c-e642-4a48-9a0d-8317f43e9ac1', borders_presumptively_illegitimate, deontological).
narrative_ontology:cs_reference_frame('03fe6e8c-e642-4a48-9a0d-8317f43e9ac1', universal_human_rights_framework).
narrative_ontology:cs_drift_state('03fe6e8c-e642-4a48-9a0d-8317f43e9ac1', contemporary_border_hardening, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('03fe6e8c-e642-4a48-9a0d-8317f43e9ac1', '').
narrative_ontology:cs_kernel_id(border_legitimacy__freedom_of_movement_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, state_security_apparatus).
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, current_citizens_who_benefit_from_exclusion).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, migrants_and_asylum_seekers).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, displaced_workers_in_origin_countries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals seeking to cross borders for safety, economic opportunity, or family reunification. They bear the direct costs of border enforcement, including detention, deportation, and often life-threatening journeys. Their freedom of movement is severely restricted.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, migrants_and_asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Workers in countries with limited economic opportunities who are unable to access international labor markets due to border restrictions. They are denied the opportunity to improve their livelihoods and are effectively trapped by the constraint.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, displaced_workers_in_origin_countries, payer,
    powerless, biographical, trapped, regional).

% Government agencies (border patrol, immigration enforcement, intelligence services) responsible for enforcing border controls. They benefit from increased budgets, expanded powers, and a narrative of protecting national security and sovereignty, which justifies their existence and growth.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, state_security_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Segments of the citizenry who perceive economic or social benefits from restricted immigration, such as reduced competition for jobs, stable wages, or preservation of cultural homogeneity. They support border enforcement as a means to protect their perceived interests.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, current_citizens_who_benefit_from_exclusion, beneficiary,
    organized, biographical, mobile, national).

% Organizations and individuals who monitor border policies, document human rights violations, and advocate for the recognition and enforcement of freedom of movement as a fundamental human right. They challenge the legitimacy of current border regimes.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, human_rights_advocates, observer,
    moderate, generational, analytical, global).

% The vast population living in poverty worldwide, whose opportunities for economic advancement are severely limited by national borders. They are largely absent from policy discussions but would be primary beneficiaries of open borders.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, global_poor, excluded,
    powerless, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_legitimacy__freedom_of_movement_reading, state_security_apparatus).
narrative_ontology:fixing_cost_class(border_legitimacy__freedom_of_movement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading's perspective, the constraint primarily serves an extractive function, rather than a genuine coordination function. Any 'coordination' (e.g., managing population flows) is secondary to the primary goal of exclusion and control.
% TRANSFER_FUNCTION: Transfers perceived security, economic stability, and cultural homogeneity to current citizens and increased power/resources to state security apparatus, at the cost of migrants' fundamental human right to freedom of movement and their economic opportunities.
% ABSENT_VOICES: The global poor and future generations of migrants are largely excluded from the discourse, despite being the most impacted by border restrictions. Their voices would unequivocally advocate for open borders and challenge the legitimacy of current regimes.
% DISAPPEARANCE_RATIONALE: If border restrictions vanished overnight, global labor markets would undergo massive rebalancing, leading to significant demographic shifts, new social structures, and a redistribution of wealth and opportunity worldwide. The current international system, built on national sovereignty and territorial control, would fundamentally transform.
% FOUNDING_PROBLEM: The historical problem borders were built to solve was the definition and defense of national territories, populations, and resources, often in the context of interstate conflict and competition.
% FOUNDING_PROBLEM_CORROBORATION: While states and some citizens argue the founding problem (national security, economic protection) is still live, human rights advocates and many migration scholars contend that the problem is either illegitimate (from a universal rights perspective) or has been reframed to justify ongoing extraction, with independent academic research supporting this shifted-function reading.
narrative_ontology:disappearance_verdict(border_legitimacy__freedom_of_movement_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__freedom_of_movement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__freedom_of_movement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extractiveness is high (0.85) because the constraint denies a fundamental right and imposes severe costs (economic, social, physical) on a large, vulnerable population. Suppression is very high (0.90) due to the active, often militarized, enforcement of borders, including physical barriers, surveillance, detention, and deportation, which effectively traps victims. Theater ratio is low (0.10) because border enforcement is a very real and often brutal activity, not primarily performative; the stated justifications (security, economic protection) are seen as covers for the underlying extraction of freedom. Accessibility collapse is high (0.80) as legal and practical alternatives to unauthorized border crossing are severely limited. Resistance is moderate (0.70) from migrants themselves and their advocates, but it faces overwhelming state power.
 *
 * PERSPECTIVAL GAP:
 *   The state security apparatus and citizens who benefit from exclusion perceive borders as legitimate and necessary for national security and economic stability. In contrast, migrants, displaced workers, and human rights advocates experience borders as profoundly unjust, coercive, and extractive, violating fundamental rights. The engine's per-seat classification will highlight this divergence, with beneficiaries experiencing a 'rope-like' coordination function (perceived order, stability) and victims experiencing a 'snare-like' extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The state security apparatus benefits directly from increased budgets and expanded powers associated with border enforcement, placing them at the beneficiary end. Current citizens who perceive benefits from exclusion also sit at the beneficiary end, as they believe the constraint protects their interests. Migrants and displaced workers are clear targets, bearing the direct and indirect costs of exclusion, placing them firmly at the target end. Human rights advocates act as analytical observers, documenting the extractive nature of the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading argues that the original mandate of borders (territorial defense) has atrophied in its moral legitimacy when applied to human movement, and its current function is primarily extractive. The classification as a Snare prevents mislabeling this as a legitimate coordination mechanism, highlighting the coercive nature and identifiable victims. The 'contested' status of the founding problem further supports this analysis, indicating a significant divergence between claimed purpose and actual operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''freedom of movement'' reading of the ''border_legitimacy'' kernel, or does it conflate distinct normative claims?',
    'Detailed philosophical analysis of the logical independence of ''freedom of movement'' from other claims (e.g., ''right to exit'' vs. ''right to enter'').',
    'If conflated, the constraint would need decomposition into more granular readings, each with its own ε and structural properties, potentially altering its classification and network relationships.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Verifies the precise scope and distinctness of this kernel reading.').

omega_variable(
    legitimacy_of_exclusion_principle,
    'Is the principle of state sovereignty, which grants the right to exclude, morally legitimate when it conflicts with individual freedom of movement?',
    'Philosophical debate and evolving international legal norms regarding the hierarchy of rights and state powers.',
    'If state sovereignty''s right to exclude is deemed morally illegitimate, the constraint''s extractiveness and suppression would be universally recognized as unjust, strengthening the Snare classification and increasing pressure for reform. If deemed legitimate, the constraint might be re-evaluated as a Tangled Rope or even a Rope from a different normative frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_exclusion_principle, preference, 'Fundamental moral legitimacy of border exclusion.').

omega_variable(
    economic_impact_of_open_borders,
    'What would be the actual economic and social impacts of significantly more open borders on receiving and sending countries, beyond current projections?',
    'Longitudinal studies of large-scale migration events, economic modeling with fewer restrictive assumptions, and empirical data from regions with high internal mobility.',
    'If empirical evidence strongly suggests net positive economic and social outcomes for all parties, the ''economic protection'' justification for borders would be undermined, further exposing the extractive nature of the constraint. If negative impacts are confirmed, it could lend some (instrumental) legitimacy to certain restrictions, potentially shifting the classification towards a Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(economic_impact_of_open_borders, empirical, 'Uncertainty regarding the real-world consequences of open borders.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__freedom_of_movement_reading, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1990, border_legitimacy__freedom_of_movement_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(bord_tr_t1996, border_legitimacy__freedom_of_movement_reading, theater_ratio, 1996, 0.11).
narrative_ontology:measurement(bord_tr_t2002, border_legitimacy__freedom_of_movement_reading, theater_ratio, 2002, 0.1).
narrative_ontology:measurement(bord_tr_t2008, border_legitimacy__freedom_of_movement_reading, theater_ratio, 2008, 0.1).
narrative_ontology:measurement(bord_tr_t2014, border_legitimacy__freedom_of_movement_reading, theater_ratio, 2014, 0.09).
narrative_ontology:measurement(bord_tr_t2020, border_legitimacy__freedom_of_movement_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(bord_be_t1990, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement(bord_be_t1996, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 1996, 0.75).
narrative_ontology:measurement(bord_be_t2002, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 2002, 0.8).
narrative_ontology:measurement(bord_be_t2008, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 2008, 0.83).
narrative_ontology:measurement(bord_be_t2014, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 2014, 0.84).
narrative_ontology:measurement(bord_be_t2020, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 2020, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1990, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 1990, 0.75).
narrative_ontology:measurement(bord_su_t1996, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 1996, 0.8).
narrative_ontology:measurement(bord_su_t2002, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 2002, 0.85).
narrative_ontology:measurement(bord_su_t2008, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 2008, 0.88).
narrative_ontology:measurement(bord_su_t2014, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 2014, 0.89).
narrative_ontology:measurement(bord_su_t2020, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 2020, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__freedom_of_movement_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, national_labor_markets).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, welfare_state_sustainability).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, global_inequality_regime).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, sovereignty_reading).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, humanitarian_obligation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'border_legitimacy' kernel, focusing on freedom of movement as a human right. It is structurally distinct from the 'sovereignty' and 'humanitarian_obligation' readings, which emphasize different normative foundations and produce different structural outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
