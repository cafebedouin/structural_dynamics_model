% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__expansive_humanitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__expansive_humanitarian_reading, []).

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
 *   constraint_id: refugee_convention_text__expansive_humanitarian_reading
 *   human_readable: Refugee Convention: Expansive Humanitarian Reading
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   This constraint story represents the 'expansive humanitarian reading' of
 *   the 1951 Refugee Convention and its 1967 Protocol. This reading
 *   interprets 'well-founded fear of persecution' to include generalized
 *   violence and persecution by non-state actors, and 'particular social
 *   group' to encompass gender, LGBTQ+, and clan-based persecution. It views
 *   the Convention as an unbendable humanitarian mandate requiring broad
 *   protection, often clashing with state sovereignty concerns. The metrics
 *   reflect the costs borne by states in upholding this broad interpretation,
 *   and the ongoing resistance from states seeking to limit their
 *   obligations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__expansive_humanitarian_reading, 0.3).
domain_priors:suppression_score(refugee_convention_text__expansive_humanitarian_reading, 0.4).
domain_priors:theater_ratio(refugee_convention_text__expansive_humanitarian_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__expansive_humanitarian_reading, rope).
narrative_ontology:human_readable(refugee_convention_text__expansive_humanitarian_reading, "Refugee Convention: Expansive Humanitarian Reading").
narrative_ontology:topic_domain(refugee_convention_text__expansive_humanitarian_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__expansive_humanitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__expansive_humanitarian_reading, 'e2652ee2-fb9d-4cdc-bd0a-66ceb8b4504a').
narrative_ontology:cs_kernel_codification('e2652ee2-fb9d-4cdc-bd0a-66ceb8b4504a', fixed_text).
narrative_ontology:cs_authority_grounding('e2652ee2-fb9d-4cdc-bd0a-66ceb8b4504a', lineage).
narrative_ontology:cs_interpretation_layer_present('e2652ee2-fb9d-4cdc-bd0a-66ceb8b4504a').
narrative_ontology:cs_reading_relation('e2652ee2-fb9d-4cdc-bd0a-66ceb8b4504a', refugee_convention_text__restrictive_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('e2652ee2-fb9d-4cdc-bd0a-66ceb8b4504a', refugee_convention_text__procedural_integrity_reading, coexists_with).
narrative_ontology:cs_axiom('e2652ee2-fb9d-4cdc-bd0a-66ceb8b4504a', foundational, non_refoulement_absolute).
narrative_ontology:cs_axiom_status(non_refoulement_absolute, holdable).
narrative_ontology:cs_axiom_grounding('e2652ee2-fb9d-4cdc-bd0a-66ceb8b4504a', non_refoulement_absolute, deontological).
narrative_ontology:cs_axiom('e2652ee2-fb9d-4cdc-bd0a-66ceb8b4504a', foundational, humanitarian_protection_broadly_construed).
narrative_ontology:cs_axiom_status(humanitarian_protection_broadly_construed, holdable).
narrative_ontology:cs_axiom_grounding('e2652ee2-fb9d-4cdc-bd0a-66ceb8b4504a', humanitarian_protection_broadly_construed, deontological).
narrative_ontology:cs_reference_frame('e2652ee2-fb9d-4cdc-bd0a-66ceb8b4504a', universal_human_dignity_framework).
narrative_ontology:cs_drift_state('e2652ee2-fb9d-4cdc-bd0a-66ceb8b4504a', contemporary_migration_crises, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('e2652ee2-fb9d-4cdc-bd0a-66ceb8b4504a', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, refugee_claimants).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, human_rights_advocates).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, states_seeking_to_limit_migration).
narrative_ontology:constraint_vindicates(refugee_convention_text__expansive_humanitarian_reading, non_refoulement_principle).
narrative_ontology:constraint_vindicates(refugee_convention_text__expansive_humanitarian_reading, universal_human_rights).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals seeking protection from persecution, including generalized violence or non-state actors, and those persecuted based on gender, sexual orientation, or clan affiliation. Their lives depend on this expansive interpretation.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, refugee_claimants, beneficiary,
    powerless, immediate, trapped, global).

% Organizations and legal professionals who champion the broad application of the Refugee Convention, providing legal aid and advocating for policies consistent with this reading. They benefit from its legitimacy and use it as a tool for protection.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, human_rights_advocates, beneficiary,
    organized, generational, constrained, global).

% National governments that bear the costs of providing protection and processing claims under an expansive interpretation, often clashing with domestic political pressures to reduce immigration. They face legal and reputational costs for non-compliance.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, states_seeking_to_limit_migration, payer,
    institutional, biographical, constrained, national).

% Bodies that interpret and apply international law, including the Refugee Convention, often issuing rulings that reinforce or expand this humanitarian reading. They shape the legal landscape for states and claimants.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, international_courts_and_tribunals, agenda_setter,
    institutional, generational, analytical, global).

% National courts that apply the Convention in domestic law, often influenced by international jurisprudence and human rights principles, leading to decisions that align with or further this expansive reading.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, domestic_judiciaries, agenda_setter,
    institutional, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common framework for states to identify and protect individuals fleeing persecution, ensuring a baseline of humanitarian treatment and preventing refoulement across borders.
% TRANSFER_FUNCTION: Transfers the burden of protection from individuals fleeing persecution to signatory states, requiring states to provide asylum, legal process, and basic rights, often at significant financial and social cost.
% ABSENT_VOICES: Populations within host states who perceive refugees as a burden on resources or a threat to cultural identity; their concerns are often marginalized in international legal discourse but drive domestic political resistance.
% DISAPPEARANCE_RATIONALE: If this expansive reading vanished, states would likely revert to more restrictive interpretations, leading to increased refoulement, greater suffering for asylum seekers, and a collapse of international cooperation on refugee protection. The global humanitarian landscape would fundamentally shift.
% FOUNDING_PROBLEM: The post-WWII displacement crisis and the failure of states to protect vulnerable populations from persecution, leading to a need for a legally binding international framework for refugee protection.
% FOUNDING_PROBLEM_CORROBORATION: UNHCR reports, human rights organizations, and independent academic studies consistently corroborate that the problem of persecution and forced displacement remains live and urgent, often exacerbated by new forms of conflict and climate change. This corroboration comes from sources outside the direct beneficiaries of the Convention.
narrative_ontology:disappearance_verdict(refugee_convention_text__expansive_humanitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__expansive_humanitarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__expansive_humanitarian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(refugee_convention_text__expansive_humanitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__expansive_humanitarian_reading, 0.3, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__expansive_humanitarian_reading_tests).
:- end_tests(refugee_convention_text__expansive_humanitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.3) is moderate, reflecting the significant, but not overwhelming, costs imposed on states by this reading. Suppression (0.4) is also moderate, as states often attempt to circumvent or resist this interpretation, requiring active enforcement by international and domestic courts. Theater ratio (0.1) is low, as the humanitarian mandate is genuinely pursued, though sometimes with performative elements by states. Accessibility collapse (0.6) is moderate, as alternatives for refugees are severely limited, but not entirely absent. Resistance (0.7) is high, reflecting the ongoing political and legal challenges from states seeking to limit their obligations.
 *
 * PERSPECTIVAL GAP:
 *   Refugee claimants and human rights advocates experience this reading as a vital protective 'rope' or even a 'mountain' of moral imperative, offering a lifeline. States seeking to limit migration, however, experience it as a 'snare' or 'tangled rope' that imposes significant, unwanted burdens and restricts their sovereign control over borders. International courts and domestic judiciaries, as agenda-setters, navigate these competing perspectives, often leaning towards the humanitarian interpretation.
 *
 * DIRECTIONALITY LOGIC:
 *   Refugee claimants and human rights advocates are clear beneficiaries (d near 0.0), as the reading directly grants them protection and legal standing. States seeking to limit migration are the primary victims/targets (d near 1.0), as they bear the costs and face restrictions on their sovereign actions. International and domestic judiciaries, while enforcing the constraint, also benefit from the legitimacy derived from upholding human rights, placing them closer to the symmetric end (d near 0.5) but with a strong agenda-setting influence.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling genuine humanitarian protection as pure extraction by recognizing the fundamental coordination problem of protecting vulnerable populations. However, the ongoing contestation over its scope and the resistance from states indicate a constant tension between the humanitarian mandate and sovereign interests, suggesting it is a 'rope' that requires continuous defense and interpretation, rather than a settled 'mountain'.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_well_founded_fear,
    'To what extent does ''well-founded fear'' genuinely encompass generalized violence and persecution by non-state actors, as opposed to individualized state-sponsored persecution?',
    'Further jurisprudence from international and domestic courts, and empirical studies on the nature of contemporary conflicts and persecution.',
    'If generalized violence is consistently excluded, the victim set would narrow, and the constraint''s effective protection would decrease, potentially shifting its classification towards a more restrictive type from the claimant''s perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_well_founded_fear, empirical, 'Ambiguity in the definition of ''well-founded fear'' in contemporary contexts.').

omega_variable(
    particular_social_group_definition,
    'Is the inclusion of gender, LGBTQ+, and clan-based persecution within ''particular social group'' a legitimate evolution of the Convention or an overreach?',
    'Continued legal scholarship, state practice, and international consensus-building, potentially leading to explicit amendments or authoritative interpretations.',
    'If these categories are widely rejected, the constraint''s protective scope would shrink, increasing extraction from these vulnerable groups and potentially reclassifying it as a ''snare'' for them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(particular_social_group_definition, conceptual, 'Contestation over the evolving definition of ''particular social group''.').

omega_variable(
    interdiction_and_offshore_processing_legality,
    'Are interdiction at sea and offshore processing mechanisms compatible with the non-refoulement principle under this expansive reading?',
    'Rulings from international human rights bodies and courts, and state practice in implementing these policies.',
    'If deemed incompatible, states employing these tactics would face increased legal and reputational costs, potentially leading to a re-evaluation of their compliance and the constraint''s effective enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interdiction_and_offshore_processing_legality, empirical, 'Legality of border control measures under the non-refoulement principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__expansive_humanitarian_reading, 1951, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t1951, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 1951, 0.05).
narrative_ontology:measurement(refu_tr_t1970, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 1970, 0.08).
narrative_ontology:measurement(refu_tr_t1990, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(refu_tr_t2010, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(refu_tr_t2024, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(refu_be_t1951, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 1951, 0.2).
narrative_ontology:measurement(refu_be_t1970, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement(refu_be_t1990, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(refu_be_t2010, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 2010, 0.35).
narrative_ontology:measurement(refu_be_t2024, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t1951, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 1951, 0.3).
narrative_ontology:measurement(refu_su_t1970, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 1970, 0.35).
narrative_ontology:measurement(refu_su_t1990, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(refu_su_t2010, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 2010, 0.45).
narrative_ontology:measurement(refu_su_t2024, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__expansive_humanitarian_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, international_humanitarian_law).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, national_asylum_laws).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'refugee_convention_text' kernel. Its expansive humanitarian interpretation contrasts with more restrictive readings, influencing national asylum laws and international humanitarian law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
