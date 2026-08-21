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
 *   human_readable: Border Restrictions as Illegitimate Infringement on Freedom of Movement
 *   domain: political_philosophy/migration_studies/international_law
 *
 * SUMMARY:
 *   This constraint story instantiates the 'freedom_of_movement_reading' of
 *   the 'border_legitimacy' kernel. From this perspective, freedom of
 *   movement is a fundamental human right, and state borders, by restricting
 *   this movement, are presumptively illegitimate. The constraint describes
 *   the active enforcement of these borders as a highly extractive and
 *   suppressive mechanism, targeting not only migrants but also, indirectly,
 *   current citizens through moral and economic costs. The high
 *   extractiveness and suppression reflect the significant harms and coercion
 *   involved in maintaining these restrictions.
 *
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
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__freedom_of_movement_reading, snare).
narrative_ontology:human_readable(border_legitimacy__freedom_of_movement_reading, "Border Restrictions as Illegitimate Infringement on Freedom of Movement").
narrative_ontology:topic_domain(border_legitimacy__freedom_of_movement_reading, "political_philosophy/migration_studies/international_law").

domain_priors:requires_active_enforcement(border_legitimacy__freedom_of_movement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__freedom_of_movement_reading, 'f5be8b87-36e8-4ea6-a9c5-eecf7362e856').
narrative_ontology:cs_kernel_codification('f5be8b87-36e8-4ea6-a9c5-eecf7362e856', formalized).
narrative_ontology:cs_authority_grounding('f5be8b87-36e8-4ea6-a9c5-eecf7362e856', extraction).
narrative_ontology:cs_interpretation_layer_present('f5be8b87-36e8-4ea6-a9c5-eecf7362e856').
narrative_ontology:cs_reading_relation('f5be8b87-36e8-4ea6-a9c5-eecf7362e856', border_legitimacy__sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('f5be8b87-36e8-4ea6-a9c5-eecf7362e856', border_legitimacy__humanitarian_obligation_reading, coexists_with).
narrative_ontology:cs_axiom('f5be8b87-36e8-4ea6-a9c5-eecf7362e856', foundational, freedom_of_movement_is_human_right).
narrative_ontology:cs_axiom_status(freedom_of_movement_is_human_right, holdable).
narrative_ontology:cs_axiom_grounding('f5be8b87-36e8-4ea6-a9c5-eecf7362e856', freedom_of_movement_is_human_right, deontological).
narrative_ontology:cs_axiom('f5be8b87-36e8-4ea6-a9c5-eecf7362e856', foundational, state_borders_presumptively_illegitimate).
narrative_ontology:cs_axiom_status(state_borders_presumptively_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('f5be8b87-36e8-4ea6-a9c5-eecf7362e856', state_borders_presumptively_illegitimate, deontological).
narrative_ontology:cs_reference_frame('f5be8b87-36e8-4ea6-a9c5-eecf7362e856', universal_human_rights_framework).
narrative_ontology:cs_drift_state('f5be8b87-36e8-4ea6-a9c5-eecf7362e856', contemporary_migration_crises, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('f5be8b87-36e8-4ea6-a9c5-eecf7362e856', '').
narrative_ontology:cs_kernel_id(border_legitimacy__freedom_of_movement_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, state_apparatus).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, migrants).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, asylum_seekers).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, displaced_persons).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, current_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals seeking to cross borders for economic opportunity, family reunification, or personal choice, who face legal, physical, and financial barriers, often at great personal risk and cost. They are the primary targets of border enforcement.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, migrants, payer,
    powerless, immediate, trapped, global).

% Individuals seeking protection from persecution or serious harm in another country, whose right to seek asylum is often impeded by border restrictions, leading to prolonged detention, refoulement, or dangerous irregular crossings.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Individuals forced to leave their homes due to conflict, violence, human rights violations, or natural disasters, who are often unable to cross international borders safely or legally, becoming trapped in precarious situations.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, displaced_persons, payer,
    powerless, immediate, trapped, global).

% Citizens of states who, from the perspective of this reading, are victims of border restrictions through the moral compromise of their state, economic inefficiencies of restricted labor markets, and the financial cost of enforcement. While some may perceive benefits from exclusion, the reading frames the restriction itself as a net harm.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, current_citizens, payer,
    moderate, biographical, constrained, national).

% The governmental bodies (border patrol, immigration agencies, defense ministries) that design, implement, and enforce border restrictions. They derive power, resources, and perceived legitimacy from their role in controlling movement across national boundaries.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Organizations and individuals who monitor, document, and challenge human rights violations at borders, advocating for policies that uphold the right to freedom of movement and the rights of migrants and asylum seekers.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, human_rights_advocates, observer,
    organized, biographical, mobile, global).

% Academics and legal experts who analyze border regimes through the lens of international human rights law, often critiquing state practices that conflict with established norms of freedom of movement and non-discrimination.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, international_law_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading's perspective, the constraint's primary function is restriction, not coordination. Any perceived coordination (e.g., managing population flows) is secondary to its extractive and suppressive nature.
% TRANSFER_FUNCTION: Transfers the perceived benefits of national stability and resource protection to a subset of current citizens, while transferring the costs of restricted movement, human rights violations, and economic inefficiency to migrants, asylum seekers, displaced persons, and ultimately, all citizens.
% ABSENT_VOICES: Global labor market advocates, cosmopolitans, and those seeking family reunification across borders are structurally excluded from the policy-making process, and their perspectives on the benefits of open borders are systematically marginalized.
% DISAPPEARANCE_RATIONALE: If border restrictions vanished overnight, global demographics, labor markets, and state functions would fundamentally reorganize. People would move to areas of greater opportunity or safety, leading to significant shifts in population distribution, economic activity, and the very concept of national sovereignty.
% FOUNDING_PROBLEM: Historically, states established borders to define territorial sovereignty, control populations, manage resources, and maintain internal order and security.
% FOUNDING_PROBLEM_CORROBORATION: State apparatuses and some citizens claim the founding problems (security, resource management, cultural preservation) are still live. Human rights advocates and international law scholars argue that these problems are often exaggerated or can be addressed through less restrictive means, and that the persistence of borders primarily serves to maintain existing power structures and extract from vulnerable populations.
narrative_ontology:disappearance_verdict(border_legitimacy__freedom_of_movement_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__freedom_of_movement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__freedom_of_movement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.85) because the constraint imposes severe costs on individuals seeking to move, including loss of life, liberty, and economic opportunity, without providing commensurate benefits to those from whom it extracts. Suppression is very high (0.90) due to the active, often violent, enforcement mechanisms employed by states to prevent unauthorized crossings, including physical barriers, surveillance, detention, and deportation. Theater ratio is low (0.10) because the enforcement is genuinely functional in preventing movement, rather than merely performative. Accessibility collapse is high (0.75) as legal pathways are severely limited, forcing many into dangerous irregular routes. Resistance is high (0.70) from migrants, advocates, and some citizens, reflecting ongoing challenges to border regimes.
 *
 * PERSPECTIVAL GAP:
 *   The state apparatus and some citizens (who align with the 'sovereignty_reading') perceive border restrictions as legitimate and necessary for national security and resource management. In contrast, this 'freedom_of_movement_reading' views these same restrictions as illegitimate, highly extractive, and suppressive, imposing severe costs on a broad range of individuals, including citizens. The engine's classification as a Snare for this reading highlights this fundamental divergence from a 'Rope' or 'Mountain' framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Migrants, asylum seekers, and displaced persons are the primary targets (payers) of this constraint, bearing the direct costs of restricted movement. Current citizens are also identified as payers, as the reading posits that the illegitimate restriction of movement imposes moral, economic, and social costs on them. The state apparatus is the primary beneficiary and agenda-setter, gaining power, resources, and perceived legitimacy from its role in controlling borders.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the ''border_legitimacy'' kernel, or is it merely a strong critique of the ''sovereignty_reading''?',
    'Analysis of the foundational axioms: if this reading''s axioms are irreconcilable with the ''sovereignty_reading'' within a single coherent framework, it confirms distinct kernel identity. If they are merely policy disagreements, it''s a critique.',
    'If a distinct reading, it validates the decomposition into separate constraints. If a critique, the ''sovereignty_reading'' would need to incorporate this perspective as an internal tension rather than a separate constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms this constraint as a distinct reading of the border legitimacy kernel.').

omega_variable(
    citizen_victimhood_empirical_basis,
    'To what extent do border restrictions genuinely harm current citizens (e.g., via economic inefficiency, moral compromise, or enforcement costs), as claimed by this reading?',
    'Comprehensive economic studies on the impact of migration restrictions on GDP, labor markets, and innovation; sociological studies on the moral and social costs of exclusionary policies; and fiscal analyses of border enforcement budgets versus perceived benefits.',
    'Strong empirical evidence of citizen harm would reinforce the ''snare'' classification and the broad victim set. Weak evidence would suggest citizens are primarily beneficiaries, shifting the classification towards a ''tangled_rope'' or ''rope'' from their perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(citizen_victimhood_empirical_basis, empirical, 'Empirical validation of citizen victimhood under border restrictions.').

omega_variable(
    sovereignty_vs_human_rights_priority,
    'Does state sovereignty inherently grant an absolute right to control borders, or is it fundamentally constrained by universal human rights, including freedom of movement?',
    'Philosophical and legal analysis of the hierarchy of international law, the nature of human rights as jus cogens norms, and the historical evolution of sovereignty concepts. This is a conceptual and normative debate.',
    'If human rights are deemed to constrain sovereignty, this reading''s premise of presumptive illegitimacy is strengthened. If sovereignty is absolute, this reading''s claims are fundamentally undermined.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_vs_human_rights_priority, conceptual, 'Conceptual priority of state sovereignty versus human rights in border control.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__freedom_of_movement_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1948, border_legitimacy__freedom_of_movement_reading, theater_ratio, 1948, 0.15).
narrative_ontology:measurement(bord_tr_t1968, border_legitimacy__freedom_of_movement_reading, theater_ratio, 1968, 0.12).
narrative_ontology:measurement(bord_tr_t1988, border_legitimacy__freedom_of_movement_reading, theater_ratio, 1988, 0.1).
narrative_ontology:measurement(bord_tr_t2008, border_legitimacy__freedom_of_movement_reading, theater_ratio, 2008, 0.09).
narrative_ontology:measurement(bord_tr_t2024, border_legitimacy__freedom_of_movement_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(bord_be_t1948, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 1948, 0.7).
narrative_ontology:measurement(bord_be_t1968, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 1968, 0.75).
narrative_ontology:measurement(bord_be_t1988, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 1988, 0.8).
narrative_ontology:measurement(bord_be_t2008, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 2008, 0.83).
narrative_ontology:measurement(bord_be_t2024, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1948, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 1948, 0.75).
narrative_ontology:measurement(bord_su_t1968, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 1968, 0.8).
narrative_ontology:measurement(bord_su_t1988, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 1988, 0.85).
narrative_ontology:measurement(bord_su_t2008, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 2008, 0.88).
narrative_ontology:measurement(bord_su_t2024, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__freedom_of_movement_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, national_sovereignty_doctrine).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, international_human_rights_law).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, border_legitimacy__sovereignty_reading).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, border_legitimacy__humanitarian_obligation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'border_legitimacy' kernel. Its ε value differs significantly from the 'sovereignty_reading' and 'humanitarian_obligation_reading' due to its foundational premise of presumptive illegitimacy of borders.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
