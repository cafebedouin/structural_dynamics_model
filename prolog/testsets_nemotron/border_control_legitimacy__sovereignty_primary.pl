% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__sovereignty_primary, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: border_control_legitimacy__sovereignty_primary
 *   human_readable: Sovereignty-Primary Border Exclusion Authority
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   The sovereignty-primary reading of border control legitimacy holds that a
 *   state's territorial sovereignty entails absolute discretion to exclude
 *   non-citizens, and that border control is constitutive of statehood
 *   itself. This reading treats human rights constraints on exclusion
 *   (non-refoulement, asylum obligations, family unity) as external limits
 *   imposed on legitimate sovereign authority rather than as constitutive of
 *   what legitimate authority means. The constraint operates as a tangled
 *   rope: it solves a genuine coordination problem (defining the bounded
 *   political community for democratic self-governance) while extracting
 *   asymmetrically from excluded non-citizens who have no voice in the
 *   sovereign decision. Enforcement is active and intensifying (detention
 *   infrastructure, externalization agreements, pushback operations). The
 *   claimed type is tangled_rope — the coordination function is real but the
 *   extraction is substantial and actively enforced.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__sovereignty_primary, 0.78).
domain_priors:suppression_score(border_control_legitimacy__sovereignty_primary, 0.85).
domain_priors:theater_ratio(border_control_legitimacy__sovereignty_primary, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, extractiveness, 0.78).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(border_control_legitimacy__sovereignty_primary, "Sovereignty-Primary Border Exclusion Authority").
narrative_ontology:topic_domain(border_control_legitimacy__sovereignty_primary, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_control_legitimacy__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__sovereignty_primary, '75db3897-4a21-49bc-bf74-33d4aa49b2b4').
narrative_ontology:cs_kernel_codification('75db3897-4a21-49bc-bf74-33d4aa49b2b4', formalized).
narrative_ontology:cs_authority_grounding('75db3897-4a21-49bc-bf74-33d4aa49b2b4', extraction).
narrative_ontology:cs_interpretation_layer_present('75db3897-4a21-49bc-bf74-33d4aa49b2b4').
narrative_ontology:cs_reading_relation('75db3897-4a21-49bc-bf74-33d4aa49b2b4', border_control_legitimacy__freedom_of_movement_primary, forecloses).
narrative_ontology:cs_reading_relation('75db3897-4a21-49bc-bf74-33d4aa49b2b4', border_control_legitimacy__jurisdictional_sovereignty, coexists_with).
narrative_ontology:cs_axiom('75db3897-4a21-49bc-bf74-33d4aa49b2b4', foundational, absolute_exclusion_discretion_constitutive_of_statehood).
narrative_ontology:cs_axiom_status(absolute_exclusion_discretion_constitutive_of_statehood, holdable).
narrative_ontology:cs_axiom_grounding('75db3897-4a21-49bc-bf74-33d4aa49b2b4', absolute_exclusion_discretion_constitutive_of_statehood, conventional).
narrative_ontology:cs_axiom('75db3897-4a21-49bc-bf74-33d4aa49b2b4', foundational, human_rights_constraints_are_external_limits_not_constitutive).
narrative_ontology:cs_axiom_status(human_rights_constraints_are_external_limits_not_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('75db3897-4a21-49bc-bf74-33d4aa49b2b4', human_rights_constraints_are_external_limits_not_constitutive, conventional).
narrative_ontology:cs_reference_frame('75db3897-4a21-49bc-bf74-33d4aa49b2b4', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('75db3897-4a21-49bc-bf74-33d4aa49b2b4', contemporary_human_rights_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('75db3897-4a21-49bc-bf74-33d4aa49b2b4', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__sovereignty_primary, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, state_territorial_authorities).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, national_citizen_electorates).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, border_enforcement_agencies).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, excluded_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, asylum_seekers).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, stateless_persons).
narrative_ontology:constraint_vindicates(border_control_legitimacy__sovereignty_primary, westphalian_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(border_control_legitimacy__sovereignty_primary, statehood_constitutive_border_control).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercises absolute discretion to exclude non-citizens through border control apparatus. Claims this authority is constitutive of statehood itself. Controls legislative, executive, and judicial mechanisms that enact and enforce exclusion. Collects political legitimacy and territorial integrity from the arrangement.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, state_territorial_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefits from border exclusion through perceived security, labor market protection, welfare state sustainability, and cultural continuity. Votes for parties that maintain restrictive border policies. Exit from the beneficiary position requires emigrating to another polity, which is constrained by other states' border controls.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, national_citizen_electorates, beneficiary,
    organized, biographical, constrained, national).

% Receives budget, personnel, legal authority, and institutional prestige from border enforcement mandate. Shapes enforcement priorities and interprets exclusion criteria. Career advancement depends on enforcement metrics. Exit means leaving the agency, which is constrained by specialized skill investment.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, border_enforcement_agencies, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__sovereignty_primary, border_enforcement_agencies, agenda_setter).

% Bears the full cost of exclusion: denied entry, detention, deportation, family separation, loss of livelihood, exposure to danger in transit or return. No effective voice in the political systems that exclude them. Exit from exclusion requires either successful irregular entry (high risk) or waiting for policy change (indefinite).
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Flees persecution or violence only to confront exclusionary border regimes that treat protection claims as discretionary exceptions to sovereign discretion. Bears acute physical danger, legal limbo, and psychological trauma. Exit options are nearly nonexistent — return means persecution, forward means exclusion.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Excluded from all state territories because no state claims them. The sovereignty-primary reading provides no structural remedy — each state's absolute discretion to exclude compounds into total exclusion. Exit from statelessness requires a state to voluntarily grant nationality, which sovereign discretion permits but does not require.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, stateless_persons, payer,
    powerless, generational, trapped, global).

% Monitors, documents, and adjudicates state compliance with human rights obligations that constrain border discretion. Issues judgments and recommendations that states treat as external limits rather than constitutive of legitimate authority. No enforcement power beyond reputational pressure.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, international_human_rights_bodies, observer,
    institutional, generational, analytical, universal).

% Organizes legal challenges, public campaigns, and direct aid to contest exclusionary border policies. Would object to sovereignty-primary framing as legitimating rights violations. Excluded from sovereign decision-making forums where border policy is set. Influence limited to external pressure and litigation in domestic/international courts.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, transnational_migrant_advocacy_networks, excluded,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of territorial governance: defines the bounded political community within which law, rights distribution, and democratic self-determination can operate. Border control coordinates who is inside the circle of mutual obligation and who is outside.
% TRANSFER_FUNCTION: Moves the burden of global inequality, displacement, and demographic pressure from the territorial state and its citizenry onto excluded non-citizens — specifically, the costs of denied entry, denied protection, and denied membership are transferred from the polity to the outsider.
% ABSENT_VOICES: The excluded migrants, asylum seekers, and stateless persons who bear the constraint's extraction are structurally absent from the sovereign decision-making that authors it. They cannot vote, lobby, or litigate as constituents of the excluding state. Their voices appear only as external pressure, never as internal deliberation.
% DISAPPEARANCE_RATIONALE: If absolute sovereign discretion to exclude vanished overnight, states would lose the legal basis for unilateral border closure. Migration flows would reorganize around rights-based entry criteria. Labor markets, welfare systems, and demographic balances would shift. The Westphalian state system would transform toward a post-Westphalian order where human mobility is a protected right rather than a sovereign concession.
% FOUNDING_PROBLEM: The post-Westphalian order needed a principle to settle which authority governs which territory and people after centuries of overlapping imperial, dynastic, and religious claims. Absolute territorial sovereignty — including the right to decide who enters — provided a clear, mutually recognizable boundary that ended wars of recognition and enabled the interstate system.
% FOUNDING_PROBLEM_CORROBORATION: International relations scholars (Krasner, Jackson) corroborate that Westphalian sovereignty solved the recognition problem for statehood. Migration scholars (Carens, Betts) and human rights bodies (UNHCR, IACHR) attest the founding problem is substantially solved — the interstate system is stable — but the border authority it legitimated has become a vehicle for extracting compliance from the globally displaced rather than coordinating peaceful coexistence.
narrative_ontology:disappearance_verdict(border_control_legitimacy__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__sovereignty_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(border_control_legitimacy__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__sovereignty_primary, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_control_legitimacy__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the burden of global displacement falls almost entirely on the excluded, while the coordinating benefits (political community definition, democratic self-determination) accrue to citizens and state authorities. Suppression (0.85) is very high because the constraint's persistence depends on active enforcement — physical barriers, detention, deportation, carrier sanctions, externalization — not on voluntary compliance. Theater ratio (0.25) is moderate-low: the security and sovereignty justifications have genuine coordination content, but a growing share of enforcement activity serves deterrence and exclusion rather than the stated coordination function. Accessibility collapse (0.45) is moderate: alternatives (open borders, regional free movement, rights-based entry) are conceptually available and practiced in limited contexts (Schengen, ECOWAS), but structurally excluded by the sovereignty-primary frame. Resistance (0.6) is significant: legal challenges, migrant caravans, sanctuary cities, and international adjudication contest the constraint, but from positions of structural disadvantage.
 *
 * PERSPECTIVAL GAP:
 *   From the state authority seat, the constraint appears as genuine coordination: defining the political community is prerequisite for law and democracy. From the excluded migrant seat, the same structure appears as pure extraction: their life chances are sacrificed for a coordination they cannot join and did not consent to. The engine computes this divergence from the structural data — the beneficiary/victim declarations and exit options encode the asymmetry. The sovereignty-primary reading's core move is treating this divergence as legitimate (sovereignty entails the right to exclude) rather than as a defect requiring remedy.
 *
 * DIRECTIONALITY LOGIC:
 *   State territorial authorities are the primary beneficiaries (d ~ 0.1) — they collect political legitimacy, territorial control, and the coercive apparatus itself. National citizen electorates are secondary beneficiaries (d ~ 0.2) — they receive security and labor market protection but bear some fiscal costs of enforcement. Border enforcement agencies are beneficiaries with agenda-setting power (d ~ 0.15) — they gain resources and institutional mission. Excluded migrants, asylum seekers, and stateless persons are the targets (d ~ 0.95) — they bear the full extraction with trapped exit. International human rights bodies are analytical observers (d ~ 0.0). Transnational advocacy networks are excluded (d ~ 0.6) — they bear advocacy costs with constrained influence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (settling territorial authority after imperial overlap) is substantially solved — the interstate system is stable. But the border authority that solved it has accumulated extraction far beyond the coordination function. The mandate has not atrophied; it has mutated. The coordination function (defining the political community) persists, but the extraction function (displacing the costs of global inequality onto the globally displaced) has become the dominant operational logic. This is not a piton — the constraint is actively maintained and expanded, not theatrically preserved. It is a tangled rope whose extraction has grown while its coordination justification has narrowed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_coordination_vs_extraction_boundary,
    'Where does the genuine coordination function of territorial sovereignty end and the extractive function of border exclusion begin? Is the coordination of a bounded political community structurally inseparable from the absolute discretion to exclude, or can the coordination be achieved with rights-constrained borders?',
    'Comparative analysis of political communities that maintain democratic self-governance with rights-constrained borders (e.g., EU internal borders, regional free movement zones) versus those asserting absolute exclusion. Historical analysis of whether Westphalian sovereignty required absolute exclusion or merely mutual recognition of territorial authority.',
    'If coordination and exclusion are separable, the sovereignty-primary reading is a false summit — it claims the coordination function to legitimate extraction that is not structurally necessary. If inseparable, the extraction is the price of the coordination, and the tangled rope classification is structurally accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_coordination_vs_extraction_boundary, conceptual, 'Whether the coordination function requires absolute exclusion authority or can operate with rights-constrained borders.').

omega_variable(
    excluded_migrant_suppression_mechanism,
    'Is the suppression experienced by excluded migrants primarily structural (physical barriers, legal prohibitions, enforcement apparatus) or does it include a significant internalized component (migrants internalizing their excludability as legitimate, self-censoring mobility aspirations)?',
    'Longitudinal studies of migrant decision-making: do mobility aspirations persist despite exclusion, or do they adapt to the constraint''s logic? Comparative analysis of migrant communities with different exposure to rights-based framing versus sovereignty-primary framing.',
    'If suppression is significantly internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them. This would amplify effective extraction for powerless agents with trapped exit, reinforcing the snare-like characteristics within the tangled rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(excluded_migrant_suppression_mechanism, empirical, 'Structural vs. internalized suppression mechanism for excluded migrants.').

omega_variable(
    kernel_reading_foreclosure_structure,
    'Does the sovereignty_primary reading''s core premise (absolute discretion to exclude is constitutive of statehood) logically foreclose the freedom_of_movement_primary reading (freedom of movement is a fundamental human right) within any single normative framework, or do they merely coexist as competing claims across different frameworks?',
    'Analysis of whether any coherent political-legal framework can simultaneously hold that (a) statehood constitutively requires absolute exclusion discretion and (b) freedom of movement is a fundamental human right that binds states. Examination of state practice: do states that ratify human rights treaties with mobility provisions (ICCPR, regional conventions) treat them as overriding sovereign discretion or as discretionary exceptions?',
    'If forecloses: the kernel contains a genuine logical contradiction between readings — no single framework can hold both, making the contest structural rather than perspectival. If coexists_with: the readings are competing but compatible positions in a pluralistic normative landscape, and the engine''s coexistence relation is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_structure, conceptual, 'Whether sovereignty_primary logically forecloses freedom_of_movement_primary or merely coexists with it.').

omega_variable(
    mandatrophy_displacement_vs_mutation,
    'Has the founding problem (territorial authority recognition) been genuinely solved such that the border authority''s persistence is mandate mutation (new extraction layered on old coordination), or does the founding problem persist in mutated form (ongoing contests over territorial authority, irredentism, secession) such that the coordination function remains live?',
    'Analysis of contemporary territorial disputes: are they contests over border authority per se, or over which sovereign exercises it? Examination of whether new border regimes (externalization, maritime interdiction, digital borders) solve coordination problems or create new extraction surfaces.',
    'If the founding problem is dead and the constraint persists: mandatrophy_resolved should be declared and the constraint trends toward piton or snare. If the founding problem is live in mutated form: the tangled rope classification holds, but the coordination-extraction balance requires continuous reassessment as the coordination target shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_displacement_vs_mutation, conceptual, 'Whether the founding problem is dead (mandatrophy) or live in mutated form (ongoing tangled rope).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__sovereignty_primary, 1648, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1648, border_control_legitimacy__sovereignty_primary, theater_ratio, 1648, 0.05).
narrative_ontology:measurement(bord_tr_t1815, border_control_legitimacy__sovereignty_primary, theater_ratio, 1815, 0.08).
narrative_ontology:measurement(bord_tr_t1919, border_control_legitimacy__sovereignty_primary, theater_ratio, 1919, 0.12).
narrative_ontology:measurement(bord_tr_t1945, border_control_legitimacy__sovereignty_primary, theater_ratio, 1945, 0.15).
narrative_ontology:measurement(bord_tr_t1951, border_control_legitimacy__sovereignty_primary, theater_ratio, 1951, 0.18).
narrative_ontology:measurement(bord_tr_t1990, border_control_legitimacy__sovereignty_primary, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(bord_tr_t2001, border_control_legitimacy__sovereignty_primary, theater_ratio, 2001, 0.22).
narrative_ontology:measurement(bord_tr_t2015, border_control_legitimacy__sovereignty_primary, theater_ratio, 2015, 0.24).
narrative_ontology:measurement(bord_tr_t2025, border_control_legitimacy__sovereignty_primary, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(bord_be_t1648, border_control_legitimacy__sovereignty_primary, base_extractiveness, 1648, 0.3).
narrative_ontology:measurement(bord_be_t1815, border_control_legitimacy__sovereignty_primary, base_extractiveness, 1815, 0.35).
narrative_ontology:measurement(bord_be_t1919, border_control_legitimacy__sovereignty_primary, base_extractiveness, 1919, 0.45).
narrative_ontology:measurement(bord_be_t1945, border_control_legitimacy__sovereignty_primary, base_extractiveness, 1945, 0.5).
narrative_ontology:measurement(bord_be_t1951, border_control_legitimacy__sovereignty_primary, base_extractiveness, 1951, 0.55).
narrative_ontology:measurement(bord_be_t1990, border_control_legitimacy__sovereignty_primary, base_extractiveness, 1990, 0.62).
narrative_ontology:measurement(bord_be_t2001, border_control_legitimacy__sovereignty_primary, base_extractiveness, 2001, 0.7).
narrative_ontology:measurement(bord_be_t2015, border_control_legitimacy__sovereignty_primary, base_extractiveness, 2015, 0.75).
narrative_ontology:measurement(bord_be_t2025, border_control_legitimacy__sovereignty_primary, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1648, border_control_legitimacy__sovereignty_primary, suppression_requirement, 1648, 0.4).
narrative_ontology:measurement(bord_su_t1815, border_control_legitimacy__sovereignty_primary, suppression_requirement, 1815, 0.5).
narrative_ontology:measurement(bord_su_t1919, border_control_legitimacy__sovereignty_primary, suppression_requirement, 1919, 0.65).
narrative_ontology:measurement(bord_su_t1945, border_control_legitimacy__sovereignty_primary, suppression_requirement, 1945, 0.7).
narrative_ontology:measurement(bord_su_t1951, border_control_legitimacy__sovereignty_primary, suppression_requirement, 1951, 0.72).
narrative_ontology:measurement(bord_su_t1990, border_control_legitimacy__sovereignty_primary, suppression_requirement, 1990, 0.78).
narrative_ontology:measurement(bord_su_t2001, border_control_legitimacy__sovereignty_primary, suppression_requirement, 2001, 0.82).
narrative_ontology:measurement(bord_su_t2015, border_control_legitimacy__sovereignty_primary, suppression_requirement, 2015, 0.84).
narrative_ontology:measurement(bord_su_t2025, border_control_legitimacy__sovereignty_primary, suppression_requirement, 2025, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__sovereignty_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(border_control_legitimacy__sovereignty_primary, 0.12).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, border_control_legitimacy__freedom_of_movement_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, border_control_legitimacy__jurisdictional_sovereignty).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, asylum_non_refoulement_obligation).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, migrant_detention_regime).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, externalization_of_border_enforcement).

% DUAL FORMULATION NOTE:
% This constraint (sovereignty_primary) is one of three readings of the border_control_legitimacy kernel. The freedom_of_movement_primary reading treats mobility as a fundamental right that constrains sovereignty. The jurisdictional_sovereignty reading treats sovereignty as regulatory authority within territory but denies that border closure is constitutive. All three share the kernel's commitment system structure but instantiate different authority groundings and axiom sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_control_legitimacy__sovereignty_primary, institutional, 0.1).
constraint_indexing:directionality_override(border_control_legitimacy__sovereignty_primary, organized, 0.2).
constraint_indexing:directionality_override(border_control_legitimacy__sovereignty_primary, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
