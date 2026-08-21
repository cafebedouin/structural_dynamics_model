% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__entangled_event_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statute_of_anne_ip_foundation__entangled_event_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: statute_of_anne_ip_foundation__entangled_event_reading
 *   human_readable: Statute of Anne: Entangled Conceptual and Institutional Event
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   This constraint instantiates the 'entangled event' reading of the Statute
 *   of Anne kernel, which posits that the statute's conceptual innovation
 *   (defining copyright as a limited right) and institutional reallocation
 *   (shifting rights from the Stationers' Company to authors) occurred
 *   simultaneously and are inseparable. This contrasts with the 'conceptual
 *   emergence' reading (focus on new ideas) and the 'institutional
 *   reallocation' reading (focus on power shift). The statute established a
 *   new legal framework for literary property, actively enforcing a limited
 *   term and new rights, while simultaneously curtailing existing perpetual
 *   claims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__entangled_event_reading, 0.65).
domain_priors:suppression_score(statute_of_anne_ip_foundation__entangled_event_reading, 0.75).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__entangled_event_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__entangled_event_reading, tangled_rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__entangled_event_reading, "Statute of Anne: Entangled Conceptual and Institutional Event").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__entangled_event_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__entangled_event_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__entangled_event_reading, 'cd861a2c-5404-4782-b342-352bb6ef5a89').
narrative_ontology:cs_kernel_codification('cd861a2c-5404-4782-b342-352bb6ef5a89', fixed_text).
narrative_ontology:cs_authority_grounding('cd861a2c-5404-4782-b342-352bb6ef5a89', lineage).
narrative_ontology:cs_interpretation_layer_present('cd861a2c-5404-4782-b342-352bb6ef5a89').
narrative_ontology:cs_reading_relation('cd861a2c-5404-4782-b342-352bb6ef5a89', statute_of_anne_ip_foundation__conceptual_emergence_reading, coexists_with).
narrative_ontology:cs_reading_relation('cd861a2c-5404-4782-b342-352bb6ef5a89', statute_of_anne_ip_foundation__institutional_reallocation_reading, coexists_with).
narrative_ontology:cs_axiom('cd861a2c-5404-4782-b342-352bb6ef5a89', foundational, conceptual_institutional_inseparability).
narrative_ontology:cs_axiom_status(conceptual_institutional_inseparability, holdable).
narrative_ontology:cs_axiom_grounding('cd861a2c-5404-4782-b342-352bb6ef5a89', conceptual_institutional_inseparability, conventional).
narrative_ontology:cs_reference_frame('cd861a2c-5404-4782-b342-352bb6ef5a89', statutory_ip_foundation).
narrative_ontology:cs_drift_state('cd861a2c-5404-4782-b342-352bb6ef5a89', contemporary_ip_debates, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('cd861a2c-5404-4782-b342-352bb6ef5a89', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__entangled_event_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__entangled_event_reading, authors).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__entangled_event_reading, public).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, stationers_company).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, conceptual_clarity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Nominally granted a new statutory right to control copying for a limited term, which was a significant shift from prior common law. However, in practice, they often assigned these rights to publishers, maintaining a dependent relationship.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, authors, beneficiary,
    moderate, biographical, constrained, national).

% Lost their claim to perpetual common law rights over books, which they had previously asserted. While they adapted to the new statutory term, the initial shift represented a significant curtailment of their prior power and a cost to their established business model.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, stationers_company, payer,
    powerful, generational, constrained, national).

% Enacted the Statute of Anne, establishing a new legal framework for literary property. They balanced competing interests of authors, publishers, and the public good of learning.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, parliament, agenda_setter,
    institutional, generational, analytical, national).

% Benefited from the eventual entry of works into the public domain after the statutory term, theoretically increasing access to knowledge. However, during the term, they paid for the limited monopoly granted.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, public, beneficiary,
    powerless, generational, constrained, national).

% Analyze the historical and legal impact of the Statute, grappling with its dual nature as both a conceptual innovation and an institutional reallocation. Their work often highlights the inseparability of these dimensions.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% Suffers as a 'victim' due to the inherent ambiguity and entanglement of the Statute's conceptual and institutional dimensions, making a clean, singular interpretation difficult for subsequent analysis.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, conceptual_clarity, excluded,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(statute_of_anne_ip_foundation__entangled_event_reading, conceptual_clarity).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Established a statutory framework for literary property, providing a clear (though limited) term for rights and defining who held them, thereby coordinating the production and dissemination of books in a more structured manner than prior common law.
% TRANSFER_FUNCTION: Transferred the primary right to control copying from the Stationers' Company's perpetual common law claim to authors for a limited term, with the practical effect of re-entrenching publishers' control for that term, while also ensuring eventual public access.
% ABSENT_VOICES: Future digital rights advocates, who would argue for even shorter terms or public domain defaults, are structurally absent. The concept of a 'public domain' as a positive right was nascent, and its full implications were not yet articulated.
% DISAPPEARANCE_RATIONALE: If the Statute of Anne had never existed, the development of intellectual property law would have taken a fundamentally different path, likely remaining under common law or evolving into a different statutory form, profoundly altering the economics of publishing and authorship and the very concept of copyright.
% FOUNDING_PROBLEM: To address the 'piracy' of books (unauthorized copying) and to encourage learning by granting authors a limited right to control their works, while simultaneously limiting the perpetual monopolies of the Stationers' Company and ensuring public access after a set term.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and economists widely corroborate the dual problem of publisher monopoly and author incentive, citing parliamentary records and contemporary pamphlets. The tension between author rights, publisher control, and public access remains central to contemporary IP debates, indicating the problem is still live.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__entangled_event_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__entangled_event_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__entangled_event_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__entangled_event_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__entangled_event_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statute_of_anne_ip_foundation__entangled_event_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(statute_of_anne_ip_foundation__entangled_event_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(statute_of_anne_ip_foundation__entangled_event_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the creation of a new, enforceable monopoly right, which, while limited, still allows for rent collection. Suppression (0.75) is high due to the active legislative act that overrode existing common law claims and required enforcement to maintain the new statutory regime. The theater ratio is low (0.1) because the statute was a genuine, functional legislative intervention with clear, intended effects, not primarily performative maintenance. Accessibility collapse (0.7) reflects the shift from a less defined common law landscape to a structured, albeit limited, statutory framework for IP.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of authors, the statute was a beneficial recognition of their rights, even if practically constrained by publishers. From the Stationers' Company's perspective, it was a loss of their traditional, perpetual control. Legal scholars, particularly those adhering to this 'entangled event' reading, view the statute as a complex, irreducible historical phenomenon, where the conceptual and institutional aspects cannot be cleanly separated for analysis.
 *
 * DIRECTIONALITY LOGIC:
 *   Authors are nominal beneficiaries, gaining a new right, but often remain constrained by publishers. The public is a long-term beneficiary (public domain access) but a short-term payer (monopoly prices). The Stationers' Company is a payer, losing perpetual rights, though they adapted to become beneficiaries of the new statutory monopoly. Conceptual clarity is a victim, as the entangled nature of the event resists simple categorization.
 *
 * MANDATROPHY ANALYSIS:
 *   The core problem the Statute of Anne addressed—balancing author incentives, publisher control, and public access to knowledge—remains a live and contested issue in intellectual property law today. Therefore, the constraint has not suffered mandatrophy; its underlying mandate continues to be relevant, albeit subject to ongoing reinterpretation and legislative updates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint best understood as an ''entangled event'' where conceptual and institutional changes are inseparable, or can these dimensions be cleanly separated as argued by sibling readings?',
    'Further historical and legal scholarship that either reinforces the inseparability through new evidence or successfully disentangles the dimensions with a compelling alternative framework.',
    'If the dimensions are found to be separable, the classification might shift towards a ''conceptual emergence'' (more Rope-like) or ''institutional reallocation'' (more Snare-like) reading, each with distinct beneficiaries and victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ambiguity regarding the fundamental nature of the Statute of Anne as an entangled event.').

omega_variable(
    beneficiary_ambiguity_authors_vs_publishers,
    'Are authors the true beneficiaries of the Statute of Anne, or did publishers become the de facto practical beneficiaries by acquiring and leveraging the new statutory rights?',
    'Economic analysis of historical contracts and market power dynamics between authors and publishers post-1710, assessing the actual distribution of economic gains.',
    'If publishers are found to be the primary practical beneficiaries, the constraint''s extractiveness would be more clearly directed towards them, potentially strengthening its Snare-like qualities from the author''s perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_ambiguity_authors_vs_publishers, empirical, 'Uncertainty over who truly benefited from the new IP rights established by the Statute.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__entangled_event_reading, 1710, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1710, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1710, 0.1).
narrative_ontology:measurement(stat_tr_t1750, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1750, 0.1).
narrative_ontology:measurement(stat_tr_t1800, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(stat_tr_t1850, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1850, 0.1).
narrative_ontology:measurement(stat_tr_t1900, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(stat_tr_t1950, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(stat_tr_t2024, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(stat_be_t1710, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1710, 0.5).
narrative_ontology:measurement(stat_be_t1750, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1750, 0.55).
narrative_ontology:measurement(stat_be_t1800, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1800, 0.6).
narrative_ontology:measurement(stat_be_t1850, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1850, 0.62).
narrative_ontology:measurement(stat_be_t1900, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1900, 0.63).
narrative_ontology:measurement(stat_be_t1950, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1950, 0.64).
narrative_ontology:measurement(stat_be_t2024, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1710, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1710, 0.7).
narrative_ontology:measurement(stat_su_t1750, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1750, 0.75).
narrative_ontology:measurement(stat_su_t1800, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1800, 0.75).
narrative_ontology:measurement(stat_su_t1850, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1850, 0.75).
narrative_ontology:measurement(stat_su_t1900, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1900, 0.75).
narrative_ontology:measurement(stat_su_t1950, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1950, 0.75).
narrative_ontology:measurement(stat_su_t2024, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__entangled_event_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__entangled_event_reading, copyright_term_extension).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__entangled_event_reading, fair_use_doctrine).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__entangled_event_reading, berne_convention_adherence).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Statute of Anne kernel, focusing on the inseparability of its conceptual and institutional dimensions. It is linked to sibling readings that emphasize either conceptual emergence or institutional reallocation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
