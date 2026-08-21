% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__living_document_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__living_document_reading, []).

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
 *   constraint_id: magna_carta_1215__living_document_reading
 *   human_readable: Magna Carta as Living Constitutional Document
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates the 'living document' reading of the Magna
 *   Carta kernel, which posits that its original meaning is legitimately
 *   superseded by an adaptive interpretive tradition, with precedential
 *   accumulation constituting constitutional development. This reading stands
 *   in contrast to the 'baronial privilege' reading (Magna Carta as a feudal
 *   contract for a limited elite) and the 'universal rights' reading (Magna
 *   Carta as a transhistorical source of universal human rights). The
 *   constraint itself is the interpretive process that allows for this
 *   adaptation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__living_document_reading, 0.45).
domain_priors:suppression_score(magna_carta_1215__living_document_reading, 0.55).
domain_priors:theater_ratio(magna_carta_1215__living_document_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__living_document_reading, rope).
narrative_ontology:human_readable(magna_carta_1215__living_document_reading, "Magna Carta as Living Constitutional Document").
narrative_ontology:topic_domain(magna_carta_1215__living_document_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_1215__living_document_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__living_document_reading, '8e67faf6-2271-4307-8c80-5fd931d63ce3').
narrative_ontology:cs_kernel_codification('8e67faf6-2271-4307-8c80-5fd931d63ce3', fixed_text).
narrative_ontology:cs_authority_grounding('8e67faf6-2271-4307-8c80-5fd931d63ce3', lineage).
narrative_ontology:cs_interpretation_layer_present('8e67faf6-2271-4307-8c80-5fd931d63ce3').
narrative_ontology:cs_reading_relation('8e67faf6-2271-4307-8c80-5fd931d63ce3', magna_carta_1215__baronial_privilege_reading, coexists_with).
narrative_ontology:cs_reading_relation('8e67faf6-2271-4307-8c80-5fd931d63ce3', magna_carta_1215__universal_rights_reading, coexists_with).
narrative_ontology:cs_axiom('8e67faf6-2271-4307-8c80-5fd931d63ce3', foundational, constitutional_meaning_evolves).
narrative_ontology:cs_axiom_status(constitutional_meaning_evolves, holdable).
narrative_ontology:cs_axiom_grounding('8e67faf6-2271-4307-8c80-5fd931d63ce3', constitutional_meaning_evolves, empirically_contingent).
narrative_ontology:cs_axiom('8e67faf6-2271-4307-8c80-5fd931d63ce3', foundational, precedent_shapes_constitutional_law).
narrative_ontology:cs_axiom_status(precedent_shapes_constitutional_law, holdable).
narrative_ontology:cs_axiom_grounding('8e67faf6-2271-4307-8c80-5fd931d63ce3', precedent_shapes_constitutional_law, conventional).
narrative_ontology:cs_reference_frame('8e67faf6-2271-4307-8c80-5fd931d63ce3', dynamic_constitutional_development).
narrative_ontology:cs_drift_state('8e67faf6-2271-4307-8c80-5fd931d63ce3', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8e67faf6-2271-4307-8c80-5fd931d63ce3', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__living_document_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, judiciary).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, legislature).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, legal_profession).
narrative_ontology:constraint_victim(magna_carta_1215__living_document_reading, originalist_scholars).
narrative_ontology:constraint_victim(magna_carta_1215__living_document_reading, citizens_affected_by_evolving_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary institutional interpreter of constitutional meaning, responsible for applying and evolving the principles of Magna Carta through case law and precedent. Benefits from the flexibility to adapt law to modern contexts.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from the interpretive tradition's ability to allow for statutory development and reform without constant formal constitutional amendment. Its laws are interpreted within this evolving constitutional framework.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, legislature, beneficiary,
    institutional, biographical, constrained, national).

% Thrives on the dynamic nature of constitutional law, engaging in argument and scholarship that shapes its evolution. Benefits from the intellectual and professional opportunities presented by an adaptive legal system.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, legal_profession, beneficiary,
    organized, biographical, mobile, national).

% Bear the cost of their interpretive method being largely superseded in mainstream legal practice. While their scholarship contributes to debate, their preferred mode of constitutional interpretation is not the dominant one, leading to a diminished influence on legal outcomes.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, originalist_scholars, payer,
    powerful, biographical, constrained, national).

% Experience the direct impact of constitutional evolution, which can sometimes lead to outcomes that diminish their rights or interests, or that they perceive as departing from fundamental principles. Their recourse is often through political action or litigation within the existing framework.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, citizens_affected_by_evolving_law, payer,
    powerless, biographical, constrained, local).

% Analyze the historical development of Magna Carta's interpretation and its impact on legal and political systems. They provide critical context but do not directly participate in the legal interpretive process.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, historians, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_1215__living_document_reading, diffuse).
narrative_ontology:fixing_cost_class(magna_carta_1215__living_document_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for the continuous adaptation of constitutional principles to changing societal needs and values, ensuring the law remains relevant, legitimate, and capable of addressing unforeseen challenges without constant formal amendment.
% TRANSFER_FUNCTION: Transfers interpretive authority from a strictly fixed historical meaning to an ongoing process of precedential accumulation and judicial/legislative interpretation, effectively moving the locus of constitutional development from past generations to current legal institutions.
% ABSENT_VOICES: Strict originalists or those who believe in a fixed, immutable constitutional text might feel their voices are absent from the dominant interpretive tradition, as their methodology is often marginalized in favor of adaptive approaches. Citizens whose rights are negatively impacted by specific evolutions of law may also feel unrepresented.
% DISAPPEARANCE_RATIONALE: If this interpretive tradition vanished overnight, constitutional law would either become static and irrelevant to modern society, or descend into perpetual, ungrounded contestation over meaning, leading to severe legal instability, a crisis of legitimacy, and potentially the collapse of the constitutional order.
% FOUNDING_PROBLEM: To prevent constitutional texts from becoming obsolete or tyrannical by allowing for their organic development and adaptation over time, avoiding the need for constant formal amendment and ensuring the constitution's enduring relevance and legitimacy across generations.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, political theorists, and many citizens attest to the ongoing need for constitutional adaptability to address unforeseen challenges (e.g., technological change, new social norms) and evolving societal values, supporting the view that a 'living' constitution is essential for modern governance. This is corroborated by the continuous evolution of legal precedent in common law systems.
narrative_ontology:disappearance_verdict(magna_carta_1215__living_document_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__living_document_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__living_document_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(magna_carta_1215__living_document_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__living_document_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__living_document_reading_tests).
:- end_tests(magna_carta_1215__living_document_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Rope because its primary function is to coordinate the ongoing interpretation and adaptation of constitutional meaning, providing a stable yet flexible framework for legal development. While there are costs (extractiveness 0.45) associated with maintaining this complex legal system and the suppression (0.55) of alternative interpretive methods, the overall benefit is a functional, adaptive constitution. Theater ratio is low (0.15) because the interpretive process is genuinely active and consequential, not merely performative. Extractiveness and suppression have gradually increased over time as the interpretive tradition has solidified and become more institutionalized, concentrating interpretive power.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the judiciary and legislature, this constraint is a vital mechanism for good governance, ensuring the constitution remains relevant. From the perspective of originalist scholars, it represents a departure from foundational principles and a loss of interpretive fidelity. The engine's per-seat classification will reflect these divergent experiences based on the declared structural relationships and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary, legislature, and legal profession are beneficiaries, as they gain authority, flexibility, and professional opportunities from an adaptive constitutional framework. Originalist scholars and citizens affected by evolving law are payers, bearing the costs of a system that may depart from fixed principles or produce outcomes they disagree with. The 'living document' approach provides a mechanism for the legal system to remain relevant, which is a diffuse benefit to society, but the interpretive authority itself is concentrated.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine ''living document'' interpretive tradition, or is it a cover for judicial activism or legislative overreach?',
    'Analysis of judicial decisions and legislative acts against a neutral standard of constitutional fidelity and adaptive necessity, as well as public and scholarly consensus on the legitimacy of specific evolutions.',
    'If it''s primarily a cover, the effective extractiveness would be higher, and the classification might shift towards a Tangled Rope or Snare, reflecting the imposition of preferences under the guise of adaptation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ambiguity between legitimate adaptation and disguised power projection.').

omega_variable(
    baronial_privilege_reading_impact,
    'What would be the structural impact if the ''baronial_privilege_reading'' of Magna Carta were to become the dominant interpretive framework?',
    'Counterfactual legal and political analysis, examining historical periods where such a reading held sway and projecting its implications for modern legal systems.',
    'It would drastically narrow the scope of constitutional protections, re-legitimizing feudal or class-based distinctions and likely leading to a more extractive and suppressive legal system for non-elites.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(baronial_privilege_reading_impact, conceptual, 'Impact of a historically restrictive interpretation.').

omega_variable(
    universal_rights_reading_impact,
    'What would be the structural impact if the ''universal_rights_reading'' of Magna Carta were to become the dominant interpretive framework?',
    'Analysis of legal systems that explicitly adopt universal rights frameworks derived from historical documents, comparing their scope of protection and mechanisms of enforcement.',
    'It would broaden the scope of constitutional protections to all persons, potentially leading to a less extractive and more inclusive legal system, but might also introduce new tensions regarding the source and limits of such rights.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(universal_rights_reading_impact, conceptual, 'Impact of a universalist interpretation.').

omega_variable(
    interpretive_authority_locus,
    'Where is the ultimate authority for constitutional interpretation truly located: in the judiciary, the legislature, or a broader societal consensus?',
    'Empirical study of how constitutional crises are resolved, how amendments are adopted, and how shifts in public opinion influence legal outcomes over long time horizons.',
    'If authority is more diffuse than currently assumed, the effective suppression of alternative readings might be lower, and the constraint''s resilience to challenge might be higher or lower depending on the nature of that diffuse authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_locus, empirical, 'Locus of interpretive authority in an adaptive constitutional system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__living_document_reading, 1700, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1700, magna_carta_1215__living_document_reading, theater_ratio, 1700, 0.1).
narrative_ontology:measurement(magn_tr_t1780, magna_carta_1215__living_document_reading, theater_ratio, 1780, 0.12).
narrative_ontology:measurement(magn_tr_t1860, magna_carta_1215__living_document_reading, theater_ratio, 1860, 0.13).
narrative_ontology:measurement(magn_tr_t1940, magna_carta_1215__living_document_reading, theater_ratio, 1940, 0.14).
narrative_ontology:measurement(magn_tr_t2020, magna_carta_1215__living_document_reading, theater_ratio, 2020, 0.15).

% Extraction over time
narrative_ontology:measurement(magn_be_t1700, magna_carta_1215__living_document_reading, base_extractiveness, 1700, 0.3).
narrative_ontology:measurement(magn_be_t1780, magna_carta_1215__living_document_reading, base_extractiveness, 1780, 0.35).
narrative_ontology:measurement(magn_be_t1860, magna_carta_1215__living_document_reading, base_extractiveness, 1860, 0.4).
narrative_ontology:measurement(magn_be_t1940, magna_carta_1215__living_document_reading, base_extractiveness, 1940, 0.43).
narrative_ontology:measurement(magn_be_t2020, magna_carta_1215__living_document_reading, base_extractiveness, 2020, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1700, magna_carta_1215__living_document_reading, suppression_requirement, 1700, 0.4).
narrative_ontology:measurement(magn_su_t1780, magna_carta_1215__living_document_reading, suppression_requirement, 1780, 0.45).
narrative_ontology:measurement(magn_su_t1860, magna_carta_1215__living_document_reading, suppression_requirement, 1860, 0.5).
narrative_ontology:measurement(magn_su_t1940, magna_carta_1215__living_document_reading, suppression_requirement, 1940, 0.53).
narrative_ontology:measurement(magn_su_t2020, magna_carta_1215__living_document_reading, suppression_requirement, 2020, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__living_document_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
