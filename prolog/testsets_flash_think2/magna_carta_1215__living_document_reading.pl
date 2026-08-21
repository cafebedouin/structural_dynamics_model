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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: magna_carta_1215__living_document_reading
 *   human_readable: Magna Carta as Living Constitutional Document (Interpretive Tradition)
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This constraint models the 'living document' reading of Magna Carta
 *   (1215), where its original meaning is legitimately superseded by an
 *   evolving interpretive tradition and precedential accumulation,
 *   constituting ongoing constitutional development. This reading views Magna
 *   Carta not as a static text, but as a foundational substrate for an
 *   adaptive legal system. The constraint functions as a Tangled Rope because
 *   it coordinates legal evolution and stability, but also enables extraction
 *   through interpretations that can benefit certain political and legal
 *   actors while limiting the claims of others.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__living_document_reading, 0.65).
domain_priors:suppression_score(magna_carta_1215__living_document_reading, 0.55).
domain_priors:theater_ratio(magna_carta_1215__living_document_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__living_document_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_1215__living_document_reading, "Magna Carta as Living Constitutional Document (Interpretive Tradition)").
narrative_ontology:topic_domain(magna_carta_1215__living_document_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_1215__living_document_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__living_document_reading, '3e2138d7-db8b-41ae-908c-434f2ff3b115').
narrative_ontology:cs_kernel_codification('3e2138d7-db8b-41ae-908c-434f2ff3b115', fixed_text).
narrative_ontology:cs_authority_grounding('3e2138d7-db8b-41ae-908c-434f2ff3b115', lineage).
narrative_ontology:cs_interpretation_layer_present('3e2138d7-db8b-41ae-908c-434f2ff3b115').
narrative_ontology:cs_reading_relation('3e2138d7-db8b-41ae-908c-434f2ff3b115', magna_carta_1215__baronial_privilege_reading, coexists_with).
narrative_ontology:cs_reading_relation('3e2138d7-db8b-41ae-908c-434f2ff3b115', magna_carta_1215__universal_rights_reading, coexists_with).
narrative_ontology:cs_axiom('3e2138d7-db8b-41ae-908c-434f2ff3b115', foundational, constitutional_adaptability_axiom).
narrative_ontology:cs_axiom_status(constitutional_adaptability_axiom, holdable).
narrative_ontology:cs_axiom_grounding('3e2138d7-db8b-41ae-908c-434f2ff3b115', constitutional_adaptability_axiom, conventional).
narrative_ontology:cs_axiom('3e2138d7-db8b-41ae-908c-434f2ff3b115', foundational, precedential_development_axiom).
narrative_ontology:cs_axiom_status(precedential_development_axiom, holdable).
narrative_ontology:cs_axiom_grounding('3e2138d7-db8b-41ae-908c-434f2ff3b115', precedential_development_axiom, conventional).
narrative_ontology:cs_reference_frame('3e2138d7-db8b-41ae-908c-434f2ff3b115', common_law_evolution).
narrative_ontology:cs_drift_state('3e2138d7-db8b-41ae-908c-434f2ff3b115', contemporary_legal_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3e2138d7-db8b-41ae-908c-434f2ff3b115', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__living_document_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, judiciary).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, legal_scholars).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, political_actors).
narrative_ontology:constraint_victim(magna_carta_1215__living_document_reading, citizens_whose_rights_are_limited).
narrative_ontology:constraint_victim(magna_carta_1215__living_document_reading, rights_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, citizens).
narrative_ontology:constraint_victim(magna_carta_1215__living_document_reading, citizens).
narrative_ontology:constraint_vindicates(magna_carta_1215__living_document_reading, constitutional_evolution_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_1215__living_document_reading, judicial_review_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and applies the constitutional principles derived from Magna Carta, adapting them to contemporary circumstances through precedential accumulation. Benefits from the flexibility and authority this interpretive role grants.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Contribute to and shape the interpretive tradition, providing academic justification and historical context for constitutional development. Their work legitimizes the 'living document' approach and influences judicial decisions.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, legal_scholars, beneficiary,
    organized, biographical, mobile, global).

% Utilize the adaptive nature of the constitution to justify policy changes and legislative initiatives, framing them as consistent with evolving constitutional principles. Benefit from the flexibility to address new challenges without formal amendment.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, political_actors, beneficiary,
    powerful, immediate, mobile, national).

% Benefit from a constitution that remains relevant and adaptable to modern society, providing a stable framework for governance. However, they also bear the costs of legal complexity and may find their specific rights limited by interpretations that prioritize other societal goals.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, citizens, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__living_document_reading, citizens, payer).

% Advocate for a strict interpretation based on the original intent or public meaning of constitutional texts. Their interpretive methodology is explicitly superseded by the 'living document' reading, placing them outside the dominant interpretive framework.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, originalists, excluded,
    organized, generational, identity_locked, national).

% Seek to expand or protect specific rights through constitutional interpretation. While the 'living document' approach can be a vehicle for rights expansion, conservative interpretations within this tradition can also limit their claims, forcing them into prolonged legal and political struggles.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, rights_advocates, payer,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, yet adaptable, framework for legal and constitutional development, allowing for the evolution of rights and governance structures without requiring constant, disruptive re-founding or formal amendment processes.
% TRANSFER_FUNCTION: Transfers interpretive authority from the fixed original meaning of constitutional texts to an ongoing process of precedential accumulation and judicial interpretation. It also transfers the burden of constitutional adaptation from legislative overhaul to the judiciary and legal tradition.
% ABSENT_VOICES: Strict originalists or those advocating for a fixed, non-evolving constitutional text are structurally excluded from the core premise of this reading, as their interpretive methodology is explicitly superseded. They would argue for a more constrained judicial role and a return to original intent.
% DISAPPEARANCE_RATIONALE: If the concept of Magna Carta as an adaptive constitutional substrate vanished, legal systems would either become rigidly static, unable to address new societal challenges and leading to ossification, or would require constant, disruptive re-founding, leading to instability and a loss of historical legitimacy.
% FOUNDING_PROBLEM: The problem of how to maintain constitutional relevance and legitimacy across centuries of profound societal, technological, and political change, avoiding both ossification (where the constitution becomes irrelevant) and revolutionary instability (where the constitution is constantly overthrown).
% FOUNDING_PROBLEM_CORROBORATION: Legal historians, political scientists, and comparative constitutional scholars attest to the ongoing challenge of constitutional adaptation, citing examples from other nations and historical periods where rigid constitutions failed or led to conflict. This corroborates the continued relevance of the problem this reading addresses.
narrative_ontology:disappearance_verdict(magna_carta_1215__living_document_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__living_document_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__living_document_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(magna_carta_1215__living_document_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__living_document_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__living_document_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_1215__living_document_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_1215__living_document_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.65) because while the interpretive tradition provides necessary adaptation, it can also be wielded to justify existing power structures or to limit rights in ways that benefit the legal and political establishment. Suppression is moderate (0.55) as the weight of precedent and institutional authority can suppress radical alternative interpretations or challenges to the established legal order. Theater ratio is low (0.20) because the interpretive process is largely functional, genuinely adapting the law, though some arguments might be performative justifications for pre-determined outcomes. The temporal measurements reflect a gradual increase in both extractiveness and suppression as the interpretive tradition has solidified and expanded over centuries, leading to greater institutional control over constitutional meaning.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the judiciary and legal scholars, this constraint is a necessary and beneficial mechanism for constitutional evolution. From the perspective of originalists or those whose rights are limited by specific interpretations, it can appear as an extractive mechanism that justifies the status quo or limits fundamental claims under the guise of 'adaptation'. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary, legal scholars, and political actors are beneficiaries (low d) as they gain authority, influence, and flexibility from this adaptive framework. Citizens are both beneficiaries (of stability) and payers (of legal complexity and potentially limited rights). Originalists are structurally excluded (high d) as their core interpretive premise is superseded. Rights advocates are payers (high d) when their claims are limited by conservative interpretations within the 'living document' framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_drift_vs_fidelity,
    'At what point does ''adaptive interpretation'' become ''unmoored re-invention,'' and who legitimately adjudicates that boundary?',
    'Comparative analysis of constitutional systems with different amendment processes and interpretive traditions, examining their long-term stability and perceived legitimacy. Public opinion surveys on constitutional fidelity.',
    'If the boundary is consistently crossed without broad public or institutional consensus, the constraint''s legitimacy erodes, increasing resistance and potentially shifting its classification towards a Snare or Piton if the interpretive function becomes purely self-serving.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_drift_vs_fidelity, conceptual, 'The conceptual boundary between legitimate adaptation and unconstrained re-interpretation.').

omega_variable(
    power_concentration_through_interpretation,
    'To what extent does the ''living document'' reading concentrate power in the judiciary or other interpretive bodies, allowing them to shape constitutional meaning without sufficient democratic accountability?',
    'Empirical studies on judicial activism, legislative override rates, and the impact of judicial appointments on constitutional outcomes. Analysis of public trust in judicial institutions.',
    'If power concentration is significant and unaccountable, the effective extraction from citizens and political actors increases, potentially pushing the constraint closer to a Snare by demonstrating a lack of checks on interpretive authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(power_concentration_through_interpretation, empirical, 'The degree to which interpretive flexibility leads to unaccountable power concentration.').

omega_variable(
    kernel_reading_ambiguity,
    'Is Magna Carta fundamentally a fixed text (baronial_privilege_reading) or an adaptive substrate (living_document_reading)?',
    'Historical and legal scholarship examining the intent of the original drafters versus the subsequent historical reception and application of the document. Analysis of the ''founding problem'' it was meant to solve.',
    'If resolved towards a fixed text, this ''living document'' reading would be reclassified as a Snare, as its adaptive claims would be seen as a cover for extraction from the original intent. If resolved towards an adaptive substrate, this reading''s claims are strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity regarding Magna Carta''s fundamental nature as a fixed text versus an adaptive substrate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__living_document_reading, 1215, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_1215__living_document_reading, theater_ratio, 1215, 0.1).
narrative_ontology:measurement(magn_tr_t1688, magna_carta_1215__living_document_reading, theater_ratio, 1688, 0.12).
narrative_ontology:measurement(magn_tr_t1832, magna_carta_1215__living_document_reading, theater_ratio, 1832, 0.15).
narrative_ontology:measurement(magn_tr_t1945, magna_carta_1215__living_document_reading, theater_ratio, 1945, 0.18).
narrative_ontology:measurement(magn_tr_t2000, magna_carta_1215__living_document_reading, theater_ratio, 2000, 0.19).
narrative_ontology:measurement(magn_tr_t2025, magna_carta_1215__living_document_reading, theater_ratio, 2025, 0.2).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_1215__living_document_reading, base_extractiveness, 1215, 0.4).
narrative_ontology:measurement(magn_be_t1688, magna_carta_1215__living_document_reading, base_extractiveness, 1688, 0.5).
narrative_ontology:measurement(magn_be_t1832, magna_carta_1215__living_document_reading, base_extractiveness, 1832, 0.58).
narrative_ontology:measurement(magn_be_t1945, magna_carta_1215__living_document_reading, base_extractiveness, 1945, 0.62).
narrative_ontology:measurement(magn_be_t2000, magna_carta_1215__living_document_reading, base_extractiveness, 2000, 0.64).
narrative_ontology:measurement(magn_be_t2025, magna_carta_1215__living_document_reading, base_extractiveness, 2025, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_1215__living_document_reading, suppression_requirement, 1215, 0.3).
narrative_ontology:measurement(magn_su_t1688, magna_carta_1215__living_document_reading, suppression_requirement, 1688, 0.4).
narrative_ontology:measurement(magn_su_t1832, magna_carta_1215__living_document_reading, suppression_requirement, 1832, 0.48).
narrative_ontology:measurement(magn_su_t1945, magna_carta_1215__living_document_reading, suppression_requirement, 1945, 0.52).
narrative_ontology:measurement(magn_su_t2000, magna_carta_1215__living_document_reading, suppression_requirement, 2000, 0.54).
narrative_ontology:measurement(magn_su_t2025, magna_carta_1215__living_document_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__living_document_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, magna_carta_1215__baronial_privilege_reading).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, magna_carta_1215__universal_rights_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'magna_carta_1215' kernel. Each reading instantiates a different constraint with its own ε and classification, linked here as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
