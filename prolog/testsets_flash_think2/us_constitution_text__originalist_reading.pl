% ============================================================================
% CONSTRAINT STORY: us_constitution_text__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__originalist_reading, []).

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
 *   constraint_id: us_constitution_text__originalist_reading
 *   human_readable: Originalist Reading of the US Constitution
 *   domain: legal/political/interpretive_theory
 *
 * SUMMARY:
 *   This constraint represents the originalist reading of the US
 *   Constitution, which posits that constitutional meaning is fixed at the
 *   time of its ratification and must be interpreted by recovering the
 *   original public understanding. This interpretive method is actively
 *   enforced by a segment of the judiciary and legal academy, leading to high
 *   suppression of adaptive interpretations and the denial of rights claims
 *   not historically grounded. The claimed type is 'tangled_rope' because it
 *   provides a coordination function (predictability, fidelity to text) for
 *   its beneficiaries while simultaneously extracting from and suppressing
 *   others.
 *
 * KEY AGENTS:
 *   - conservative_legal_movement: Primary beneficiary/agenda_setter (institutional/arbitrage)
 *   - judges_originalist: Agenda_setter (institutional/constrained)
 *   - rights_claimants_not_historically_grounded: Primary target/payer (powerless/trapped)
 *   - living_constitutionalists: Excluded (organized/constrained)
 *   - general_public: Beneficiary/Payer (moderate/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__originalist_reading, 0.75).
domain_priors:suppression_score(us_constitution_text__originalist_reading, 0.85).
domain_priors:theater_ratio(us_constitution_text__originalist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_text__originalist_reading, "Originalist Reading of the US Constitution").
narrative_ontology:topic_domain(us_constitution_text__originalist_reading, "legal/political/interpretive_theory").

domain_priors:requires_active_enforcement(us_constitution_text__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__originalist_reading, '9b9e6097-0fdc-443f-bd11-48ba97408696').
narrative_ontology:cs_kernel_codification('9b9e6097-0fdc-443f-bd11-48ba97408696', fixed_text).
narrative_ontology:cs_authority_grounding('9b9e6097-0fdc-443f-bd11-48ba97408696', lineage).
narrative_ontology:cs_interpretation_layer_present('9b9e6097-0fdc-443f-bd11-48ba97408696').
narrative_ontology:cs_reading_relation('9b9e6097-0fdc-443f-bd11-48ba97408696', us_constitution_text__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('9b9e6097-0fdc-443f-bd11-48ba97408696', us_constitution_text__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('9b9e6097-0fdc-443f-bd11-48ba97408696', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('9b9e6097-0fdc-443f-bd11-48ba97408696', constitutional_meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('9b9e6097-0fdc-443f-bd11-48ba97408696', foundational, judicial_role_is_to_discover_not_create_law).
narrative_ontology:cs_axiom_status(judicial_role_is_to_discover_not_create_law, holdable).
narrative_ontology:cs_axiom_grounding('9b9e6097-0fdc-443f-bd11-48ba97408696', judicial_role_is_to_discover_not_create_law, deontological).
narrative_ontology:cs_reference_frame('9b9e6097-0fdc-443f-bd11-48ba97408696', framers_intent_supremacy).
narrative_ontology:cs_drift_state('9b9e6097-0fdc-443f-bd11-48ba97408696', contemporary_legal_discourse, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('9b9e6097-0fdc-443f-bd11-48ba97408696', '').
narrative_ontology:cs_kernel_id(us_constitution_text__originalist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, conservative_legal_movement).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, rights_claimants_not_historically_grounded).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, general_public).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promotes and enforces originalist interpretation, benefiting from its institutional dominance in judicial appointments and the stability it provides to their ideological positions. They shape legal education and public discourse to entrench this interpretive method.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, conservative_legal_movement, agenda_setter,
    institutional, generational, arbitrage, national).

% Judges who adhere to originalist principles, applying historical evidence to determine constitutional meaning. Their careers, professional legitimacy, and influence within certain legal circles depend on this adherence. They actively suppress alternative interpretive methods in their rulings.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, judges_originalist, agenda_setter,
    institutional, biographical, constrained, national).

% Individuals or groups whose asserted rights (e.g., privacy, evolving equality standards, environmental protections) are not explicitly or implicitly recognized by the original public understanding of the Constitution, and thus face suppression and denial under this interpretive method. Their claims are often dismissed as 'judicial activism'.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, rights_claimants_not_historically_grounded, payer,
    powerless, immediate, trapped, national).

% Legal scholars, advocates, and judges who argue for an evolving constitutional meaning that adapts to contemporary societal circumstances. Their arguments are often marginalized, dismissed, or actively countered by originalist courts and legal institutions, limiting their influence on constitutional outcomes.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, living_constitutionalists, excluded,
    organized, biographical, constrained, national).

% Benefits from perceived stability, predictability, and fidelity to foundational texts in constitutional law, which can foster trust in the legal system. However, they may bear costs through the denial of evolving rights, social progress, or the inability of the Constitution to address modern challenges without formal amendment.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, general_public, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_text__originalist_reading, general_public, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, fixed framework for constitutional interpretation, aiming to prevent judicial activism and ensure consistency with the framers' original intent, thereby coordinating legal expectations around a historical understanding.
% TRANSFER_FUNCTION: Transfers interpretive authority from contemporary societal values and evolving norms to historical evidence and the original public understanding, effectively denying certain rights claims and consolidating power for those who control historical narratives and judicial appointments.
% ABSENT_VOICES: Future generations and marginalized groups whose experiences and rights were not contemplated or protected by the original public understanding are structurally excluded from shaping constitutional meaning under this framework. Their perspectives are deemed irrelevant to the 'fixed' meaning.
% DISAPPEARANCE_RATIONALE: If originalism vanished overnight, constitutional interpretation would immediately shift to other methods (e.g., living constitutionalism, pragmatism), leading to a re-evaluation of numerous precedents, a potential expansion of rights, and a significant reordering of legal and political power dynamics, particularly concerning civil rights, privacy, and federal power.
% FOUNDING_PROBLEM: To prevent judicial overreach and ensure that constitutional meaning remains tethered to a fixed, ascertainable source, thereby preserving democratic self-governance and the rule of law against subjective judicial preferences and transient political majorities.
% FOUNDING_PROBLEM_CORROBORATION: Originalist proponents (e.g., Federalist Society, conservative judges) assert the problem of judicial activism is still live and that originalism is the only legitimate solution. Critics (e.g., progressive legal scholars, civil rights advocates) argue that while judicial overreach is a concern, originalism's solution creates new problems of democratic deficit and injustice, making the status of the founding problem contested by independent legal analysis and historical scholarship.
narrative_ontology:disappearance_verdict(us_constitution_text__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__originalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(us_constitution_text__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__originalist_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_text__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_text__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75) because the interpretive method systematically denies certain rights and limits governmental adaptation, imposing significant costs on those whose claims are not historically recognized. Suppression is very high (0.85) due to the active institutional enforcement by courts and legal bodies that dismiss or marginalize alternative interpretations. Theater ratio is low (0.15) as the method is genuinely applied, not merely performative, though its claims of pure objectivity are contested. Accessibility collapse is high (0.70) as it severely limits the viable pathways for constitutional arguments. Resistance is moderate (0.60) due to ongoing academic, legal, and political opposition from living constitutionalists and civil rights advocates.
 *
 * PERSPECTIVAL GAP:
 *   The conservative legal movement and originalist judges experience this constraint as a legitimate and necessary framework for constitutional governance, providing stability and fidelity. In contrast, rights claimants and living constitutionalists experience it as an extractive and suppressive force that freezes constitutional meaning in a past era, denying contemporary justice and adaptation. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The conservative legal movement and originalist judges are clear beneficiaries and agenda-setters, as the constraint entrenches their ideological positions and institutional power (low directionality). Rights claimants not historically grounded are direct targets, bearing the costs of denied rights and limited legal recourse (high directionality). Living constitutionalists are excluded, their interpretive framework actively suppressed. The general public is a mixed seat, benefiting from perceived stability but potentially paying through denied social progress.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling originalism as a pure Rope (which would ignore its substantial extraction and suppression) or a pure Snare (which would ignore its genuine coordination function for its beneficiaries in providing a stable interpretive framework). The 'contested' status of the founding problem, coupled with high extractiveness and suppression, indicates that while a coordination problem was initially addressed, the mechanism has evolved to serve extractive ends for its beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_meaning_ascertainability,
    'Is it empirically possible to reliably and objectively recover the ''original public understanding'' of the Constitution, given historical distance, linguistic drift, and the diversity of views at the time of ratification?',
    'Further advancements in historical linguistics, digital humanities, and historical methodology, or a consensus among historians and legal scholars on the limits of such recovery.',
    'If original meaning is found to be largely unascertainable or highly subjective, the constraint''s legitimacy as an objective interpretive method would collapse, potentially reclassifying it as a Snare or Piton, as its coordination function would be revealed as theatrical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_meaning_ascertainability, empirical, 'The empirical possibility of recovering original public understanding.').

omega_variable(
    legitimacy_of_judicial_adaptation,
    'Is judicial adaptation of constitutional principles to contemporary circumstances inherently illegitimate, or is it a necessary function of a living constitution in a democratic society?',
    'A shift in societal consensus regarding the proper role of the judiciary in a modern democracy, or a formal constitutional amendment clarifying interpretive authority.',
    'If judicial adaptation is deemed legitimate, the suppression of living constitutionalism would be seen as an illegitimate extraction of interpretive authority, strengthening the Snare aspects of this constraint. If adaptation is deemed illegitimate, the constraint''s Rope aspects (preventing activism) would be reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_of_judicial_adaptation, conceptual, 'The conceptual legitimacy of judicial adaptation versus fixed meaning.').

omega_variable(
    suppression_of_evolving_rights,
    'To what extent does the originalist framework genuinely protect against judicial overreach versus systematically suppress the recognition of evolving rights and social justice claims?',
    'Longitudinal studies comparing judicial outcomes under originalist and non-originalist regimes across various rights categories, coupled with public opinion shifts on constitutional interpretation.',
    'If the framework is found to primarily suppress evolving rights rather than prevent overreach, its extractiveness and suppression metrics would be re-evaluated upwards, pushing it closer to a Snare. If it demonstrably prevents arbitrary judicial power, its coordination function would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_evolving_rights, empirical, 'Balance between preventing overreach and suppressing evolving rights.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__originalist_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1970, us_constitution_text__originalist_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(us_c_tr_t1980, us_constitution_text__originalist_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(us_c_tr_t1990, us_constitution_text__originalist_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(us_c_tr_t2000, us_constitution_text__originalist_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(us_c_tr_t2010, us_constitution_text__originalist_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(us_c_tr_t2020, us_constitution_text__originalist_reading, theater_ratio, 2020, 0.15).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1970, us_constitution_text__originalist_reading, base_extractiveness, 1970, 0.65).
narrative_ontology:measurement(us_c_be_t1980, us_constitution_text__originalist_reading, base_extractiveness, 1980, 0.68).
narrative_ontology:measurement(us_c_be_t1990, us_constitution_text__originalist_reading, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement(us_c_be_t2000, us_constitution_text__originalist_reading, base_extractiveness, 2000, 0.72).
narrative_ontology:measurement(us_c_be_t2010, us_constitution_text__originalist_reading, base_extractiveness, 2010, 0.74).
narrative_ontology:measurement(us_c_be_t2020, us_constitution_text__originalist_reading, base_extractiveness, 2020, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1970, us_constitution_text__originalist_reading, suppression_requirement, 1970, 0.75).
narrative_ontology:measurement(us_c_su_t1980, us_constitution_text__originalist_reading, suppression_requirement, 1980, 0.78).
narrative_ontology:measurement(us_c_su_t1990, us_constitution_text__originalist_reading, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(us_c_su_t2000, us_constitution_text__originalist_reading, suppression_requirement, 2000, 0.82).
narrative_ontology:measurement(us_c_su_t2010, us_constitution_text__originalist_reading, suppression_requirement, 2010, 0.84).
narrative_ontology:measurement(us_c_su_t2020, us_constitution_text__originalist_reading, suppression_requirement, 2020, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, us_constitution_text__living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, us_constitution_text__positivist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'us_constitution_text' kernel. Its fixed-meaning premise structurally influences and is influenced by other interpretive readings, such as living constitutionalism and positivism, which offer alternative frameworks for constitutional meaning.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
