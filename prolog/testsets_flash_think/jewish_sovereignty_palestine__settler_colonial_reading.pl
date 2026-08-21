% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__settler_colonial_reading, []).

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
 *   constraint_id: jewish_sovereignty_palestine__settler_colonial_reading
 *   human_readable: Jewish Sovereignty in Palestine: Settler-Colonial Reading
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'settler-colonial reading' of
 *   Jewish sovereignty in Palestine. From this perspective, Zionism is
 *   understood as a project that, regardless of the intentions of individual
 *   participants, structurally functions as a European settler-colonial
 *   enterprise. Jewish immigration and state-building are seen as
 *   constituting a displacement regime for the indigenous Palestinian
 *   population, leading to high extraction and suppression. The constraint's
 *   persistence relies on active enforcement and the suppression of
 *   alternatives for Palestinians.
 *
 * KEY AGENTS:
 *   - Palestinian Indigenous Population: Primary victims of dispossession (powerless/trapped)
 *   - Palestinian Diaspora: Secondary victims, denied return (powerless/identity_locked)
 *   - Zionist Movement Leadership: Agenda-setters and enforcers of the regime (institutional/arbitrage)
 *   - Jewish Immigrants: Structural beneficiaries of the displacement (moderate/constrained)
 *   - Colonial Metropole: Geopolitical beneficiaries (institutional/arbitrage)
 *   - International Law Bodies: Analytical observers (institutional/analytical)
 *   - Anti-Colonial Solidarity Movements: Excluded voices (organized/mobile)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__settler_colonial_reading, 0.88).
domain_priors:suppression_score(jewish_sovereignty_palestine__settler_colonial_reading, 0.92).
domain_priors:theater_ratio(jewish_sovereignty_palestine__settler_colonial_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__settler_colonial_reading, snare).
narrative_ontology:human_readable(jewish_sovereignty_palestine__settler_colonial_reading, "Jewish Sovereignty in Palestine: Settler-Colonial Reading").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__settler_colonial_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__settler_colonial_reading, '7d54ff74-8339-443a-89c8-38f0fb22b774').
narrative_ontology:cs_kernel_codification('7d54ff74-8339-443a-89c8-38f0fb22b774', formalized).
narrative_ontology:cs_authority_grounding('7d54ff74-8339-443a-89c8-38f0fb22b774', extraction).
narrative_ontology:cs_interpretation_layer_present('7d54ff74-8339-443a-89c8-38f0fb22b774').
narrative_ontology:cs_reading_relation('7d54ff74-8339-443a-89c8-38f0fb22b774', jewish_sovereignty_palestine__liberal_nationalist_reading, forecloses).
narrative_ontology:cs_reading_relation('7d54ff74-8339-443a-89c8-38f0fb22b774', jewish_sovereignty_palestine__religious_zionist_reading, forecloses).
narrative_ontology:cs_reading_relation('7d54ff74-8339-443a-89c8-38f0fb22b774', jewish_sovereignty_palestine__cultural_zionist_reading, forecloses).
narrative_ontology:cs_reading_relation('7d54ff74-8339-443a-89c8-38f0fb22b774', jewish_sovereignty_palestine__post_zionist_reading, coexists_with).
narrative_ontology:cs_axiom('7d54ff74-8339-443a-89c8-38f0fb22b774', foundational, zionism_as_settler_colonial_project).
narrative_ontology:cs_axiom_status(zionism_as_settler_colonial_project, holdable).
narrative_ontology:cs_axiom_grounding('7d54ff74-8339-443a-89c8-38f0fb22b774', zionism_as_settler_colonial_project, empirically_contingent).
narrative_ontology:cs_axiom('7d54ff74-8339-443a-89c8-38f0fb22b774', foundational, indigenous_dispossession_as_structural).
narrative_ontology:cs_axiom_status(indigenous_dispossession_as_structural, holdable).
narrative_ontology:cs_axiom_grounding('7d54ff74-8339-443a-89c8-38f0fb22b774', indigenous_dispossession_as_structural, empirically_contingent).
narrative_ontology:cs_reference_frame('7d54ff74-8339-443a-89c8-38f0fb22b774', european_colonial_expansion_pattern).
narrative_ontology:cs_drift_state('7d54ff74-8339-443a-89c8-38f0fb22b774', contemporary_postcolonial_critique, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('7d54ff74-8339-443a-89c8-38f0fb22b774', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, zionist_movement_leadership).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, jewish_immigrants).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, colonial_metropole).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_indigenous_population).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_diaspora).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The indigenous population of Palestine, subjected to ongoing dispossession, displacement, military occupation, and denial of self-determination. They bear the direct costs of land loss, resource control, and political subjugation.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_indigenous_population, payer,
    powerless, generational, trapped, regional).

% Palestinians displaced from their homeland, denied the right of return. Their identity is deeply tied to their ancestral land, and their exclusion is a core component of the displacement regime.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_diaspora, payer,
    powerless, generational, identity_locked, global).

% The political and institutional leadership that orchestrated Jewish immigration, land acquisition, and state-building in Palestine. They frame these actions as national liberation and self-defense, while actively enforcing the displacement regime.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, zionist_movement_leadership, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Individuals who immigrated to Palestine/Israel, regardless of their personal intent or refugee status. They structurally benefit from the displacement regime by gaining land, housing, and citizenship within the new state, often on land dispossessed from Palestinians.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, jewish_immigrants, beneficiary,
    moderate, biographical, constrained, regional).

% Initially the British Empire, and later successive global powers (e.g., U.S. imperial interests) that supported or benefited from the establishment and maintenance of the Zionist project, gaining geopolitical influence and strategic alliances in the region.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, colonial_metropole, beneficiary,
    institutional, generational, arbitrage, global).

% Organizations and legal frameworks (e.g., UN, ICJ) that document violations of international law, issue resolutions, and provide legal opinions, but often lack direct enforcement power over the constraint.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, international_law_bodies, observer,
    institutional, generational, analytical, global).

% Global movements and organizations advocating for Palestinian rights, decolonization, and an end to the settler-colonial project. They are often marginalized or dismissed by dominant political and media narratives that legitimize the constraint.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, anti_colonial_solidarity_movements, excluded,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint coordinates the establishment and maintenance of a Jewish-majority state in Palestine, including the systematic acquisition of land, demographic engineering, and the development of security apparatus to control the indigenous population.
% TRANSFER_FUNCTION: Transfers land, resources, and sovereignty from the indigenous Palestinian population to Jewish settlers and the nascent/established state, backed by colonial powers. It also transfers geopolitical influence and strategic advantage to the supporting colonial metropole.
% ABSENT_VOICES: Palestinian voices, particularly those advocating for the right of return, self-determination, and decolonization, are systematically excluded from the decision-making processes that shape and legitimize the constraint. Anti-colonial solidarity movements are also largely excluded from mainstream political discourse.
% DISAPPEARANCE_RATIONALE: If the settler-colonial pattern and its enforcement mechanisms vanished overnight, the entire political, demographic, and territorial arrangement would collapse. Land ownership, citizenship, and national identity would undergo a fundamental reorganization, leading to a decolonized future for the region.
% FOUNDING_PROBLEM: European Jewish communities faced severe antisemitism and persecution, culminating in the Holocaust, leading to a perceived existential need for a safe haven and self-determination in their ancestral homeland.
% FOUNDING_PROBLEM_CORROBORATION: The Zionist movement and its supporters attest to the ongoing need for a Jewish state as a refuge from antisemitism. However, postcolonial scholars, Palestinian historians, and human rights organizations (from outside the benefiting parties) argue that while antisemitism was a real historical force, the solution chosen instantiated a colonial framework, making the 'founding problem' for the indigenous population one of dispossession and ethnic cleansing, not safety.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__settler_colonial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__settler_colonial_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jewish_sovereignty_palestine__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__settler_colonial_reading, 0.88, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.88) due to the systematic transfer of land, resources, and sovereignty from Palestinians to the settler state. Suppression is also very high (0.92) as the constraint's existence and expansion depend on active military, legal, and political suppression of Palestinian resistance and the denial of their rights. Theater ratio is moderate (0.45), reflecting that while narratives of self-defense and development are present, a significant portion of the state's activity is performative in masking the underlying colonial logic. Accessibility collapse is high for Palestinians, as their alternatives (e.g., self-determination, return) are systematically foreclosed. Resistance is high, reflecting the ongoing struggle against this regime.
 *
 * PERSPECTIVAL GAP:
 *   This settler-colonial reading fundamentally diverges from liberal nationalist or religious Zionist readings, which frame the project as legitimate self-determination or divine fulfillment. From the perspective of the Palestinian victims, the constraint is a snare of dispossession; from the perspective of the Zionist leadership, it is a necessary, actively enforced rope for national survival. The engine's computation of per-seat types will highlight this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The Zionist movement leadership and the colonial metropole are clear beneficiaries, actively shaping and profiting from the constraint. Jewish immigrants are structural beneficiaries, gaining land and citizenship within the new system. The Palestinian indigenous population and diaspora are the primary targets and victims, bearing the full costs of dispossession and exclusion. International law bodies observe and critique, while anti-colonial solidarity movements are actively excluded from the dominant discourse.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Snare prevents mislabeling the constraint as a legitimate 'Rope' of national self-determination or a 'Mountain' of historical inevitability. It highlights that the coordination function (Jewish state-building) is a cover for asymmetric extraction (Palestinian dispossession), and its persistence relies on coercion rather than mutual benefit or natural law. The 'contested' status of the founding problem further supports this, indicating that the original justification is no longer universally accepted as valid for the current operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a valid and coherent reading of the ''Jewish Sovereignty in Palestine'' kernel, or is it an external imposition?',
    'Analysis of historical evidence, legal frameworks, and lived experiences through a postcolonial theoretical lens to assess the consistency of the settler-colonial framework with the empirical record.',
    'If validated, this reading provides a robust framework for understanding the constraint''s extractive nature. If invalidated, the constraint''s classification would need to be re-evaluated under a different theoretical framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms this constraint as a specific reading of the ''Jewish Sovereignty in Palestine'' kernel.').

omega_variable(
    settler_intent_vs_structural_effect,
    'To what extent does the individual intent or refugee status of Jewish immigrants alter the structural settler-colonial nature of the project?',
    'Sociological and historical analysis comparing the structural outcomes of land acquisition, demographic change, and political control with the stated intentions of individual immigrants or the movement''s leadership.',
    'If structural effects are dominant regardless of intent, the classification as a Snare is reinforced. If individual intent is found to significantly mitigate structural outcomes, the extractiveness might be slightly lower, or the ''victim'' category might require further nuance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settler_intent_vs_structural_effect, empirical, 'Examines the role of individual intent versus structural outcomes in settler-colonial analysis.').

omega_variable(
    metropole_beneficiary_evolution,
    'How has the identity and specific benefits accrued by the ''colonial_metropole'' evolved from the British Mandate era to contemporary global power dynamics?',
    'Historical and geopolitical analysis tracing the shifting alliances, strategic interests, and material benefits (e.g., military aid, intelligence sharing, regional influence) for successive global powers supporting the constraint.',
    'A clearer understanding of the metropole''s evolving role would refine the ''beneficiary'' status and potentially adjust its directionality, but the overall settler-colonial classification would likely remain stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metropole_beneficiary_evolution, empirical, 'Traces the evolution of the colonial metropole''s role and benefits over time.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__settler_colonial_reading, 1917, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1917, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1917, 0.2).
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1948, 0.3).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1967, 0.4).
narrative_ontology:measurement(jewi_tr_t1993, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1993, 0.43).
narrative_ontology:measurement(jewi_tr_t2023, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 2023, 0.45).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1917, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1917, 0.7).
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1948, 0.8).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1967, 0.85).
narrative_ontology:measurement(jewi_be_t1993, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1993, 0.87).
narrative_ontology:measurement(jewi_be_t2023, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 2023, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1917, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1917, 0.75).
narrative_ontology:measurement(jewi_su_t1948, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1948, 0.85).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1967, 0.9).
narrative_ontology:measurement(jewi_su_t1993, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1993, 0.91).
narrative_ontology:measurement(jewi_su_t2023, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 2023, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__settler_colonial_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__post_zionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five distinct readings of the 'Jewish Sovereignty in Palestine' kernel. Each reading offers a different structural interpretation, leading to different ε values and classifications. This settler-colonial reading emphasizes dispossession and extraction, contrasting with other readings that focus on national liberation, religious fulfillment, or cultural revival.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
