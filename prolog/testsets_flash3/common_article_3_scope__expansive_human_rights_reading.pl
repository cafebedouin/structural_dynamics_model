% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__expansive_human_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__expansive_human_rights_reading, []).

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
 *   constraint_id: common_article_3_scope__expansive_human_rights_reading
 *   human_readable: Common Article 3 Scope: Expansive Human Rights Reading
 *   domain: international_humanitarian_law/human_rights
 *
 * SUMMARY:
 *   This constraint represents the 'expansive human rights reading' of Common
 *   Article 3 (CA3) of the Geneva Conventions, which asserts that CA3's
 *   minimum humanitarian standards apply to any organized armed violence,
 *   regardless of its formal classification as an international or
 *   non-international armed conflict. This reading significantly broadens the
 *   scope of international humanitarian law, bringing more actors and
 *   situations under its protective umbrella, but also increasing the burden
 *   and potential liability for states and non-state armed groups. The
 *   constraint is claimed as a 'tangled_rope' because it genuinely
 *   coordinates a universal humanitarian floor while simultaneously
 *   extracting compliance and accountability from actors who resist this
 *   broad interpretation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__expansive_human_rights_reading, 0.65).
domain_priors:suppression_score(common_article_3_scope__expansive_human_rights_reading, 0.7).
domain_priors:theater_ratio(common_article_3_scope__expansive_human_rights_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__expansive_human_rights_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__expansive_human_rights_reading, "Common Article 3 Scope: Expansive Human Rights Reading").
narrative_ontology:topic_domain(common_article_3_scope__expansive_human_rights_reading, "international_humanitarian_law/human_rights").

domain_priors:requires_active_enforcement(common_article_3_scope__expansive_human_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__expansive_human_rights_reading, 'e05f2faa-aae6-4e7c-825f-ef3c0f812e17').
narrative_ontology:cs_kernel_codification('e05f2faa-aae6-4e7c-825f-ef3c0f812e17', fixed_text).
narrative_ontology:cs_authority_grounding('e05f2faa-aae6-4e7c-825f-ef3c0f812e17', lineage).
narrative_ontology:cs_interpretation_layer_present('e05f2faa-aae6-4e7c-825f-ef3c0f812e17').
narrative_ontology:cs_reading_relation('e05f2faa-aae6-4e7c-825f-ef3c0f812e17', common_article_3_scope__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('e05f2faa-aae6-4e7c-825f-ef3c0f812e17', common_article_3_scope__icrc_customary_reading, influences).
narrative_ontology:cs_axiom('e05f2faa-aae6-4e7c-825f-ef3c0f812e17', foundational, universal_human_dignity_in_conflict).
narrative_ontology:cs_axiom_status(universal_human_dignity_in_conflict, holdable).
narrative_ontology:cs_axiom_grounding('e05f2faa-aae6-4e7c-825f-ef3c0f812e17', universal_human_dignity_in_conflict, deontological).
narrative_ontology:cs_axiom('e05f2faa-aae6-4e7c-825f-ef3c0f812e17', foundational, minimum_humanitarian_floor_regardless_of_classification).
narrative_ontology:cs_axiom_status(minimum_humanitarian_floor_regardless_of_classification, holdable).
narrative_ontology:cs_axiom_grounding('e05f2faa-aae6-4e7c-825f-ef3c0f812e17', minimum_humanitarian_floor_regardless_of_classification, conventional).
narrative_ontology:cs_reference_frame('e05f2faa-aae6-4e7c-825f-ef3c0f812e17', post_wwii_human_rights_consensus).
narrative_ontology:cs_drift_state('e05f2faa-aae6-4e7c-825f-ef3c0f812e17', contemporary_asymmetric_warfare_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e05f2faa-aae6-4e7c-825f-ef3c0f812e17', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, human_rights_advocates).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, international_courts).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, state_security_forces).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, non_state_armed_groups).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, detainees).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, affected_civilians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, detainees).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, affected_civilians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the broad application of CA3, as it provides a legal framework to monitor and challenge state and non-state actors' conduct, expanding their advocacy and litigation opportunities. They actively promote this expansive reading.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, human_rights_advocates, beneficiary,
    organized, generational, mobile, global).

% Their jurisdiction and legitimacy are enhanced by an expansive reading of CA3, allowing them to prosecute a wider range of actors and situations. They interpret and apply CA3 in line with this reading, though facing resistance from states.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, international_courts, agenda_setter,
    institutional, generational, constrained, global).

% Bear significant costs from this reading, as it subjects their operations, even in low-intensity conflicts or law enforcement contexts, to stringent international humanitarian law standards, increasing legal risks and operational constraints. They resist this interpretation.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, state_security_forces, payer,
    powerful, immediate, constrained, national).

% Are brought under the purview of international humanitarian law regardless of their level of organization or intensity of violence, increasing their accountability and potential for prosecution, which they largely lack the capacity or will to meet.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, non_state_armed_groups, payer,
    moderate, immediate, trapped, local).

% Benefit directly from the expansive application of CA3, as it guarantees minimum humanitarian protections (e.g., humane treatment, prohibition of torture) regardless of the conflict's formal classification or their status.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, detainees, beneficiary,
    powerless, immediate, trapped, local).

% Benefit from the broader protection framework, as it aims to reduce suffering and ensure basic rights even in situations not traditionally classified as armed conflict, subjecting more actors to humanitarian standards.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, affected_civilians, beneficiary,
    powerless, immediate, constrained, local).

% Observes and documents the application of CA3, often advocating for interpretations that ensure maximum protection for victims. While generally aligned with humanitarian principles, their official position on scope is often more cautious, focusing on customary law and state practice.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, icrc, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal floor of minimum humanitarian standards applicable to all forms of organized armed violence, ensuring a baseline of protection for individuals regardless of the legal classification of the conflict.
% TRANSFER_FUNCTION: Transfers accountability and legal obligations for humane treatment and non-discrimination from states and non-state armed groups to international legal bodies and human rights mechanisms, expanding the scope of international oversight.
% ABSENT_VOICES: States that prioritize national sovereignty and security over expansive international oversight, and non-state armed groups that reject international legal frameworks, are often absent from the interpretive discourse, but their actions are directly constrained by this reading.
% DISAPPEARANCE_RATIONALE: If this expansive reading of CA3 vanished, the legal protections for individuals in many forms of organized violence would significantly diminish, particularly in 'grey zone' conflicts or internal disturbances. State and non-state actors would face fewer constraints, leading to a likely increase in human rights abuses and a re-fragmentation of humanitarian standards.
% FOUNDING_PROBLEM: The original problem was to ensure a minimum standard of humanity in armed conflicts not of an international character, where traditional Geneva Conventions did not fully apply, preventing a 'legal black hole' for victims.
% FOUNDING_PROBLEM_CORROBORATION: Human rights organizations, international legal scholars, and reports from UN bodies consistently attest that the problem of ensuring minimum humanitarian standards in all forms of organized violence remains live, citing ongoing conflicts and abuses that fall outside traditional conflict classifications. This corroboration comes from outside the direct beneficiaries of the expansive reading.
narrative_ontology:disappearance_verdict(common_article_3_scope__expansive_human_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__expansive_human_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__expansive_human_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(common_article_3_scope__expansive_human_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__expansive_human_rights_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__expansive_human_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_article_3_scope__expansive_human_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_article_3_scope__expansive_human_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because this reading imposes significant legal and operational costs on states and non-state groups, forcing them to adhere to standards they might otherwise avoid. Suppression (0.7) is high due to the active efforts by international courts and human rights advocates to enforce this broad interpretation against resistant state practices and claims of sovereignty. Theater ratio (0.4) reflects that while some compliance is genuine, a notable portion involves performative adherence or rhetorical concessions without full operational integration. Resistance (0.75) is high, primarily from states and armed groups who view this as an overreach into their internal affairs or a blurring of IHL and IHRL.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of human rights advocates, this reading is a necessary 'rope' for universal protection. From the perspective of many states, it is a 'snare' that erodes sovereignty and complicates security operations. The engine's classification as 'tangled_rope' reflects the hybrid nature: a genuine coordination function (universal humanitarian floor) coupled with asymmetric extraction (from resistant state and non-state actors).
 *
 * DIRECTIONALITY LOGIC:
 *   Human rights advocates and international courts are beneficiaries, as this reading expands their mandate and influence. Detainees and affected civilians are also beneficiaries, receiving enhanced protections. State security forces and non-state armed groups are payers, as they bear the costs of increased accountability and operational restrictions. The ICRC acts as an observer, documenting and influencing, but not directly benefiting or paying in the same way as the other parties.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by acknowledging both the coordination function (ensuring a humanitarian floor) and the extraction (from those who resist its broad application). It avoids the 'piton' trap by recognizing active enforcement and resistance, indicating it is not merely inertial. It avoids being a 'snare' by identifying clear beneficiaries (detainees, civilians) and a genuine coordination problem (universal standards).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_organized_armed_violence,
    'What constitutes ''organized armed violence'' sufficient to trigger CA3 under this reading, particularly in contexts like policing or counter-terrorism operations?',
    'Further jurisprudence from international courts or authoritative interpretations from UN bodies that provide clear criteria for ''organized'' and ''violence'' in non-traditional conflict settings.',
    'Clearer definitions would reduce ambiguity for state actors, potentially increasing compliance but also narrowing the scope if definitions become too restrictive. Ambiguity currently allows for both expansive claims and state resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_organized_armed_violence, conceptual, 'Ambiguity in the threshold for CA3 application under an expansive reading.').

omega_variable(
    state_resistance_vs_compliance,
    'To what extent does state resistance to this expansive reading translate into actual non-compliance versus performative compliance or rhetorical opposition?',
    'Empirical studies tracking state practice in ''grey zone'' conflicts, comparing declared policies with on-the-ground conduct, and analyzing prosecution rates for alleged violations.',
    'If non-compliance is widespread, the effective extractiveness and suppression of this reading are lower than measured, indicating a ''piton'' or ''snare'' for victims. If compliance is higher than perceived, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_resistance_vs_compliance, empirical, 'Gap between state rhetoric and actual practice regarding CA3''s expansive scope.').

omega_variable(
    human_rights_vs_ihl_overlap,
    'Is the expansive human rights reading of CA3 blurring the distinct legal regimes of International Humanitarian Law (IHL) and International Human Rights Law (IHRL) in a way that creates legal uncertainty or undermines IHL''s specificity?',
    'Legal scholarship and judicial decisions that clarify the precise relationship and potential tensions between IHL and IHRL in situations of armed violence, particularly concerning lex specialis principles.',
    'If the blurring creates unresolvable conflicts, it could undermine the coherence of both regimes. If the overlap is seen as complementary, it strengthens the overall protection framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(human_rights_vs_ihl_overlap, conceptual, 'Conceptual tension between IHL and IHRL in the expansive CA3 reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__expansive_human_rights_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1949, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 1949, 0.1).
narrative_ontology:measurement(comm_tr_t1970, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(comm_tr_t1990, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(comm_tr_t2010, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(comm_tr_t2024, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(comm_be_t1949, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 1949, 0.3).
narrative_ontology:measurement(comm_be_t1970, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(comm_be_t1990, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(comm_be_t2010, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(comm_be_t2024, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1949, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 1949, 0.4).
narrative_ontology:measurement(comm_su_t1970, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(comm_su_t1990, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(comm_su_t2010, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(comm_su_t2024, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
