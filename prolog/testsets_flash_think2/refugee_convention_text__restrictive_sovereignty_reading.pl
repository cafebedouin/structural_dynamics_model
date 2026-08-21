% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__restrictive_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__restrictive_sovereignty_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: refugee_convention_text__restrictive_sovereignty_reading
 *   human_readable: Refugee Convention: Restrictive Sovereignty Reading
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   This constraint represents a restrictive interpretation of the 1951
 *   Refugee Convention, emphasizing state sovereignty and limiting the scope
 *   of protection. It requires asylum seekers to prove individualized
 *   persecution and restricts the definition of 'particular social group' to
 *   immutable characteristics, often excluding generalized violence or
 *   non-state persecution. This reading allows for high admissibility
 *   screening and offshore processing, effectively narrowing the victim set
 *   and increasing the burden on those seeking asylum. The claimed type is
 *   'tangled_rope' because it maintains a nominal coordination function
 *   (international framework for refugee protection) but with significant
 *   asymmetric extraction from asylum seekers by sovereign states.
 *
 * KEY AGENTS:
 *   - sovereign_states: Agenda-setter/Beneficiary (institutional/arbitrage)
 *   - border_control_agencies: Agenda-setter/Beneficiary (organized/constrained)
 *   - asylum_seekers: Payer (powerless/trapped)
 *   - refugee_advocacy_groups: Payer/Excluded (moderate/constrained)
 *   - international_human_rights_bodies: Observer (institutional/analytical)
 *   - national_courts_and_tribunals: Agenda-setter (institutional/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, 0.78).
domain_priors:suppression_score(refugee_convention_text__restrictive_sovereignty_reading, 0.85).
domain_priors:theater_ratio(refugee_convention_text__restrictive_sovereignty_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__restrictive_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__restrictive_sovereignty_reading, "Refugee Convention: Restrictive Sovereignty Reading").
narrative_ontology:topic_domain(refugee_convention_text__restrictive_sovereignty_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__restrictive_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__restrictive_sovereignty_reading, '94415695-1633-4f4f-ab7d-fcabee7b3db7').
narrative_ontology:cs_kernel_codification('94415695-1633-4f4f-ab7d-fcabee7b3db7', fixed_text).
narrative_ontology:cs_authority_grounding('94415695-1633-4f4f-ab7d-fcabee7b3db7', lineage).
narrative_ontology:cs_interpretation_layer_present('94415695-1633-4f4f-ab7d-fcabee7b3db7').
narrative_ontology:cs_reading_relation('94415695-1633-4f4f-ab7d-fcabee7b3db7', refugee_convention_text__expansive_humanitarian_reading, forecloses).
narrative_ontology:cs_reading_relation('94415695-1633-4f4f-ab7d-fcabee7b3db7', refugee_convention_text__procedural_integrity_reading, coexists_with).
narrative_ontology:cs_axiom('94415695-1633-4f4f-ab7d-fcabee7b3db7', foundational, state_sovereignty_primacy).
narrative_ontology:cs_axiom_status(state_sovereignty_primacy, holdable).
narrative_ontology:cs_axiom_grounding('94415695-1633-4f4f-ab7d-fcabee7b3db7', state_sovereignty_primacy, conventional).
narrative_ontology:cs_axiom('94415695-1633-4f4f-ab7d-fcabee7b3db7', foundational, individualized_persecution_proof_requirement).
narrative_ontology:cs_axiom_status(individualized_persecution_proof_requirement, holdable).
narrative_ontology:cs_axiom_grounding('94415695-1633-4f4f-ab7d-fcabee7b3db7', individualized_persecution_proof_requirement, conventional).
narrative_ontology:cs_reference_frame('94415695-1633-4f4f-ab7d-fcabee7b3db7', post_westphalian_state_sovereignty).
narrative_ontology:cs_drift_state('94415695-1633-4f4f-ab7d-fcabee7b3db7', contemporary_migration_crises, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('94415695-1633-4f4f-ab7d-fcabee7b3db7', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, sovereign_states).
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, border_control_agencies).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, refugee_advocacy_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret the Convention to maximize their discretion over who qualifies for protection, limiting the scope of 'well-founded fear' and 'particular social group' to reduce national obligations and maintain border control. They benefit from reduced intake and control over migration flows.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, sovereign_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__restrictive_sovereignty_reading, sovereign_states, beneficiary).

% Actively enforce the restrictive interpretations at borders and within national territory, implementing policies like high admissibility screening, individualized persecution proof requirements, and offshore processing. They benefit from clear mandates to limit entry and process claims efficiently (from their perspective).
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, border_control_agencies, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__restrictive_sovereignty_reading, border_control_agencies, beneficiary).

% Bear the primary costs of this interpretation, facing high burdens of proof, prolonged detention, limited access to territory, and a narrow pathway to protection. Their alternatives are often non-existent, leaving them trapped between persecution and denial of asylum.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Expend significant resources on legal aid, lobbying, and public awareness campaigns to challenge restrictive interpretations and assist asylum seekers. They are largely excluded from the policy-making process where these interpretations are solidified.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, refugee_advocacy_groups, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__restrictive_sovereignty_reading, refugee_advocacy_groups, excluded).

% Monitor state compliance with international law, issue reports, and provide recommendations, often critiquing restrictive interpretations. Their influence is primarily normative and advisory, lacking direct enforcement power.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% Apply and further refine the restrictive interpretations through case law, often balancing national security and sovereignty concerns against individual protection claims. While independent, they operate within the legal framework set by the state.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, national_courts_and_tribunals, agenda_setter,
    institutional, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(refugee_convention_text__restrictive_sovereignty_reading, sovereign_states).
narrative_ontology:fixing_cost_class(refugee_convention_text__restrictive_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a minimum international legal floor for states to coordinate their responses to refugee flows, ensuring a baseline of protection while allowing states maximum discretion in implementation, thereby managing perceived burdens and maintaining national sovereignty.
% TRANSFER_FUNCTION: Transfers the burden of proof, the risk of non-protection, and the costs of displacement onto asylum seekers, while transferring discretion, control, and reduced obligations to sovereign states and their enforcement agencies.
% ABSENT_VOICES: Asylum seekers themselves, who are directly impacted but lack a formal voice in the interpretation and application of the Convention; also, human rights lawyers and scholars advocating for more expansive, protection-oriented readings.
% DISAPPEARANCE_RATIONALE: If this restrictive interpretation vanished overnight, states would face immediate and significant pressure to broaden protection criteria, potentially leading to a surge in asylum claims, a re-evaluation of border policies, and a fundamental shift in international migration governance, or a breakdown of the Convention if states withdrew from a more expansive regime.
% FOUNDING_PROBLEM: To establish an international legal framework for the protection of individuals fleeing persecution, ensuring non-refoulement, while also acknowledging and respecting the sovereign rights of states to control their borders and populations.
% FOUNDING_PROBLEM_CORROBORATION: Sovereign states and their legal representatives assert that the problem of balancing national security and sovereignty with humanitarian obligations remains live. Refugee advocacy groups and international legal experts argue that while the original problem of protecting the persecuted is live, this interpretation prioritizes state discretion to the detriment of protection, effectively undermining the Convention's core purpose.
narrative_ontology:disappearance_verdict(refugee_convention_text__restrictive_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__restrictive_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__restrictive_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(refugee_convention_text__restrictive_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__restrictive_sovereignty_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because this reading significantly limits access to protection, imposing high costs on asylum seekers while benefiting states through reduced obligations. Suppression is very high (0.85) due to active enforcement mechanisms (border controls, detention, legal hurdles) designed to deter and deny claims under this interpretation. Theater ratio is moderate (0.45) as states maintain the rhetoric of humanitarian protection while their practices, driven by this interpretation, often contradict the spirit of the Convention. Accessibility collapse is high (0.70) because this reading actively closes off avenues for protection for many who would qualify under broader interpretations. Resistance is moderate (0.60) from advocacy groups and some international bodies, but often insufficient to shift state practice.
 *
 * PERSPECTIVAL GAP:
 *   Sovereign states and their agencies perceive this reading as a necessary balance between humanitarian obligations and national security/sovereignty, viewing it as a functional 'rope' for international cooperation. Asylum seekers and their advocates, however, experience it as a 'snare' due to the high barriers to protection and the severe consequences of denial. The engine's classification as 'tangled_rope' reflects the presence of both a claimed coordination function and substantial, actively enforced extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Sovereign states and border control agencies are clear beneficiaries (low d) as they gain discretion and control. Asylum seekers are the primary targets (high d), bearing the costs of exclusion and denial. Refugee advocacy groups are also targets, expending resources to mitigate the constraint's effects. International human rights bodies are observers, providing analytical input without direct benefit or cost from this specific constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling by acknowledging the stated coordination function (a minimum floor for international cooperation) while highlighting the significant, actively enforced extraction. It avoids treating the 'sovereign discretion' aspect as a neutral coordination cost, instead identifying it as a source of asymmetric benefit for states and cost for asylum seekers. The 'contested' status of the founding problem further supports the idea that the constraint's function has drifted from its original intent, accumulating extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a stable, independent entity, or one reading of the ''refugee_convention_text'' kernel?',
    'Analysis of legal scholarship and state practice: if interpretations consistently diverge along identifiable axes, it confirms multiple readings.',
    'If confirmed as a reading, its classification is understood in relation to sibling readings, highlighting the contest over the kernel''s meaning. If treated as independent, the contest is obscured.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is the ''restrictive_sovereignty_reading'' of the ''refugee_convention_text'' kernel.').

omega_variable(
    persecution_definition_ambiguity,
    'Is ''well-founded fear'' inherently limited to individualized persecution, or can it encompass generalized violence and non-state actors?',
    'International legal consensus development, or a landmark ruling by a high international court that redefines the scope.',
    'If generalized violence is included, the victim set expands significantly, reducing extractiveness and suppression for asylum seekers. If strictly individualized, the current high extraction persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persecution_definition_ambiguity, conceptual, 'Ambiguity in the definition of ''persecution'' under the Convention.').

omega_variable(
    psg_definition_ambiguity,
    'Is ''particular social group'' strictly limited to immutable characteristics, or can it evolve to include gender, LGBTQ+, or clan-based persecution?',
    'Evolution of international human rights law and jurisprudence, or a shift in state practice and recognition.',
    'An expansive definition would broaden the scope of protection, reducing extractiveness for vulnerable groups. A narrow definition maintains the current restrictive regime.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(psg_definition_ambiguity, conceptual, 'Ambiguity in the definition of ''particular social group'' under the Convention.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__restrictive_sovereignty_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t1980, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(refu_tr_t1990, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(refu_tr_t2000, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(refu_tr_t2010, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(refu_tr_t2020, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 2020, 0.43).
narrative_ontology:measurement(refu_tr_t2025, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(refu_be_t1980, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(refu_be_t1990, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 1990, 0.68).
narrative_ontology:measurement(refu_be_t2000, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 2000, 0.72).
narrative_ontology:measurement(refu_be_t2010, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(refu_be_t2020, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 2020, 0.77).
narrative_ontology:measurement(refu_be_t2025, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t1980, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(refu_su_t1990, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 1990, 0.72).
narrative_ontology:measurement(refu_su_t2000, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 2000, 0.78).
narrative_ontology:measurement(refu_su_t2010, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 2010, 0.81).
narrative_ontology:measurement(refu_su_t2020, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 2020, 0.84).
narrative_ontology:measurement(refu_su_t2025, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 2025, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__restrictive_sovereignty_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'refugee_convention_text' kernel, alongside 'expansive_humanitarian_reading' and 'procedural_integrity_reading'. Each reading instantiates a distinct constraint with its own structural properties and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
