% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_freedom_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_freedom_reading, []).

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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_freedom_reading
 *   human_readable: GPL Reciprocity Obligation (Copyleft as Freedom Reading)
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   This constraint represents the GPL reciprocity obligation as interpreted
 *   by the 'copyleft as freedom' reading. From this perspective, the GPL's
 *   'viral' nature is a necessary mechanism to preserve user freedoms by
 *   preventing proprietary software from incorporating and then closing off
 *   improvements to free software. It is seen as a protective measure for the
 *   open-source commons, ensuring that contributions remain accessible to
 *   all. The constraint is claimed as a Tangled Rope because it coordinates
 *   user freedom while actively extracting from and suppressing proprietary
 *   business models that would otherwise enclose the commons.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.7).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.85).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "GPL Reciprocity Obligation (Copyleft as Freedom Reading)").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_freedom_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'b510b5a3-f7cd-403e-b994-3371b553f396').
narrative_ontology:cs_kernel_codification('b510b5a3-f7cd-403e-b994-3371b553f396', fixed_text).
narrative_ontology:cs_authority_grounding('b510b5a3-f7cd-403e-b994-3371b553f396', lineage).
narrative_ontology:cs_interpretation_layer_present('b510b5a3-f7cd-403e-b994-3371b553f396').
narrative_ontology:cs_reading_relation('b510b5a3-f7cd-403e-b994-3371b553f396', gpl_reciprocity_obligation__copyleft_as_restriction_reading, coexists_with).
narrative_ontology:cs_reading_relation('b510b5a3-f7cd-403e-b994-3371b553f396', gpl_reciprocity_obligation__copyleft_as_commons_reading, coexists_with).
narrative_ontology:cs_axiom('b510b5a3-f7cd-403e-b994-3371b553f396', foundational, software_freedom_is_paramount).
narrative_ontology:cs_axiom_status(software_freedom_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('b510b5a3-f7cd-403e-b994-3371b553f396', software_freedom_is_paramount, deontological).
narrative_ontology:cs_axiom('b510b5a3-f7cd-403e-b994-3371b553f396', foundational, proprietary_capture_is_a_threat_to_freedom).
narrative_ontology:cs_axiom_status(proprietary_capture_is_a_threat_to_freedom, holdable).
narrative_ontology:cs_axiom_grounding('b510b5a3-f7cd-403e-b994-3371b553f396', proprietary_capture_is_a_threat_to_freedom, empirically_contingent).
narrative_ontology:cs_reference_frame('b510b5a3-f7cd-403e-b994-3371b553f396', free_software_ecosystem_integrity).
narrative_ontology:cs_drift_state('b510b5a3-f7cd-403e-b994-3371b553f396', contemporary_software_development, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b510b5a3-f7cd-403e-b994-3371b553f396', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, open_source_community).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_integrators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, software_developers).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, software_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive software with guaranteed freedoms to use, study, modify, and distribute. They benefit from the open nature of the code and the prevention of proprietary lock-in, ensuring their control over the software they use.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users, beneficiary,
    moderate, biographical, mobile, global).

% Cannot incorporate GPL-licensed code into proprietary products without being obligated to release their entire derived work under a compatible copyleft license. This 'viral' aspect restricts their business models and forces them to contribute back or avoid GPL code.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_integrators, payer,
    powerful, biographical, constrained, global).

% Actively promotes and defends the GPL, ensuring its terms are met. They benefit from the expansion of the free software commons and the prevention of proprietary enclosure, which aligns with their ideological commitment to software freedom.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, open_source_community, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_freedom_reading, open_source_community, beneficiary).

% Contribute to and build upon GPL-licensed projects, benefiting from a large pool of free code. They must, however, adhere to the copyleft terms for any derived works they distribute, which can constrain their choice of licensing for their own contributions.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, software_developers, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_freedom_reading, software_developers, payer).

% Analyze the legal enforceability, economic impact, and philosophical underpinnings of copyleft licenses. They provide commentary and interpretation without directly participating in the enforcement or payment structure.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_reciprocity_obligation__copyleft_as_freedom_reading, open_source_community).
narrative_ontology:fixing_cost_class(gpl_reciprocity_obligation__copyleft_as_freedom_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective development and distribution of software by ensuring that all derived works remain free and open, preventing proprietary enclosure and fostering a shared digital commons.
% TRANSFER_FUNCTION: Transfers the obligation to share source code and user freedoms from upstream developers to downstream users and integrators, effectively preventing the privatization of collective software efforts.
% ABSENT_VOICES: Proprietary software vendors and business models that seek to leverage open-source components without contributing back their own modifications are structurally excluded from certain integration paths. They would argue for less restrictive licensing terms.
% DISAPPEARANCE_RATIONALE: If the GPL's reciprocity obligation vanished, a significant portion of open-source software would likely be integrated into proprietary products without corresponding contributions back to the commons. This would lead to a substantial reduction in the pool of free software and a shift towards more closed, vendor-controlled ecosystems, fundamentally altering the software development landscape.
% FOUNDING_PROBLEM: The risk of proprietary software companies taking open-source code, improving it, and then closing the source, thereby privatizing collective effort and limiting user freedoms, leading to a 'tragedy of the commons' for software.
% FOUNDING_PROBLEM_CORROBORATION: The Free Software Foundation and numerous open-source advocates consistently attest to the ongoing threat of proprietary enclosure. Legal precedents and industry practices continue to demonstrate the persistent tension between open and closed source models, corroborating the founding problem's continued relevance.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_freedom_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_freedom_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_freedom_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_freedom_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_freedom_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_reciprocity_obligation__copyleft_as_freedom_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.70) because the GPL imposes a significant 'cost' on proprietary integrators, forcing them to either open-source their derived work or avoid GPL code entirely. Suppression is very high (0.85) as the license actively and legally prevents the proprietary capture of derived works, effectively suppressing an entire class of alternative licensing strategies for those works. Theater ratio is low (0.10) because the license's terms are directly functional and actively enforced, with little performative overhead. Resistance is high (0.75) from proprietary interests who view the GPL as overly restrictive.
 *
 * PERSPECTIVAL GAP:
 *   This reading emphasizes user freedom and the prevention of proprietary capture. Other readings, such as 'copyleft as restriction' (which views the GPL as limiting business choices) or 'copyleft as commons' (which focuses on the institutional technology for collective resource management), offer different perspectives on the same underlying mechanism. The engine's classification will reflect the structural realities authored here, independent of these alternative framings.
 *
 * DIRECTIONALITY LOGIC:
 *   Downstream users and the open_source_community are the primary beneficiaries, gaining guaranteed freedoms and an expanding commons (low directionality). Proprietary_integrators are the clear targets, facing significant restrictions on their business models (high directionality). Software_developers have a mixed role, benefiting from the commons but also constrained by the reciprocity obligation for their own derived works.
 *
 * MANDATROPHY ANALYSIS:
 *   From the 'copyleft as freedom' perspective, the mandate of the GPL is very much alive. The founding problem of proprietary enclosure remains a persistent threat in the software industry, and the GPL continues to serve its function of protecting user freedoms. There is no evidence of mandatrophy from this reading's viewpoint; the constraint's purpose is actively pursued and relevant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint accurately represented as the ''copyleft_as_freedom_reading'' of the ''gpl_reciprocity_obligation'' kernel?',
    'Analysis of legal and philosophical texts from the Free Software Foundation and related advocacy groups to confirm alignment with their stated goals and interpretations.',
    'If the reading is misidentified, the classification of the constraint''s purpose and beneficiary structure would be inaccurate, potentially leading to a different claimed_type and metric profile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms the specific interpretation of the GPL being modeled.').

omega_variable(
    freedom_definition_ambiguity,
    'Does ''user freedom'' as defined by this reading genuinely encompass all relevant forms of freedom, or does it implicitly restrict other forms (e.g., business model freedom)?',
    'Comparative analysis with alternative definitions of ''freedom'' in software licensing, including those that prioritize commercial flexibility or developer choice.',
    'If the definition of freedom is found to be narrowly construed, the ''beneficiary'' status of downstream users might be re-evaluated, potentially increasing the perceived extractiveness from other parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(freedom_definition_ambiguity, conceptual, 'Examines the scope and potential biases in the definition of ''user freedom''.').

omega_variable(
    enforcement_effectiveness_vs_adoption,
    'To what extent does the GPL''s ''viral'' enforcement mechanism actually prevent proprietary capture versus merely deterring adoption by proprietary integrators?',
    'Empirical studies tracking the integration patterns of GPL-licensed code in both open and closed-source projects, and the legal outcomes of alleged GPL violations.',
    'If the GPL primarily deters rather than actively ''converts'' proprietary projects, its suppression metric might be re-evaluated to reflect a different mechanism of influence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_effectiveness_vs_adoption, empirical, 'Assesses the practical impact of GPL''s enforcement on proprietary software development.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 1989, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gpl__tr_t7, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 7, 0.1).
narrative_ontology:measurement(gpl__tr_t14, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 14, 0.1).
narrative_ontology:measurement(gpl__tr_t21, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 21, 0.1).
narrative_ontology:measurement(gpl__tr_t28, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 28, 0.1).
narrative_ontology:measurement(gpl__tr_t35, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 35, 0.1).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(gpl__be_t7, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 7, 0.63).
narrative_ontology:measurement(gpl__be_t14, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 14, 0.66).
narrative_ontology:measurement(gpl__be_t21, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 21, 0.68).
narrative_ontology:measurement(gpl__be_t28, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 28, 0.69).
narrative_ontology:measurement(gpl__be_t35, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 35, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(gpl__su_t7, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 7, 0.82).
narrative_ontology:measurement(gpl__su_t14, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 14, 0.83).
narrative_ontology:measurement(gpl__su_t21, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 21, 0.84).
narrative_ontology:measurement(gpl__su_t28, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 28, 0.85).
narrative_ontology:measurement(gpl__su_t35, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 35, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_freedom_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, open_source_software_development).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, software_intellectual_property_regime).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gpl_reciprocity_obligation' kernel, each representing a distinct structural interpretation of the GPL's function and impact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
