% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__substitutionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__substitutionist_reading, []).

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
 *   constraint_id: divine_marriage_command__substitutionist_reading
 *   human_readable: Divine Marriage Command (Substitutionist Reading)
 *   domain: religious_authority/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint represents the 'substitutionist' reading of a divine
 *   marriage command, where a new 'Manifesto' is interpreted as a superseding
 *   revelation that doctrinally requires monogamy, replacing prior commands
 *   that permitted polygamy. This reading is central to the institutional
 *   leadership's strategy for survival and legitimacy in the face of external
 *   pressure. The constraint operates as a Tangled Rope, coordinating the
 *   institution's external relations while extracting from internal
 *   dissenters.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, 0.65).
domain_priors:suppression_score(divine_marriage_command__substitutionist_reading, 0.78).
domain_priors:theater_ratio(divine_marriage_command__substitutionist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__substitutionist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__substitutionist_reading, "Divine Marriage Command (Substitutionist Reading)").
narrative_ontology:topic_domain(divine_marriage_command__substitutionist_reading, "religious_authority/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(divine_marriage_command__substitutionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__substitutionist_reading, 'b3ad6395-9d01-4689-8b45-7633fc923869').
narrative_ontology:cs_kernel_codification('b3ad6395-9d01-4689-8b45-7633fc923869', formalized).
narrative_ontology:cs_authority_grounding('b3ad6395-9d01-4689-8b45-7633fc923869', lineage).
narrative_ontology:cs_interpretation_layer_present('b3ad6395-9d01-4689-8b45-7633fc923869').
narrative_ontology:cs_reading_relation('b3ad6395-9d01-4689-8b45-7633fc923869', divine_marriage_command__continuationist_reading, forecloses).
narrative_ontology:cs_reading_relation('b3ad6395-9d01-4689-8b45-7633fc923869', divine_marriage_command__coercion_visibility_reading, influences).
narrative_ontology:cs_axiom('b3ad6395-9d01-4689-8b45-7633fc923869', foundational, new_revelation_supersedes_prior_command).
narrative_ontology:cs_axiom_status(new_revelation_supersedes_prior_command, holdable).
narrative_ontology:cs_axiom_grounding('b3ad6395-9d01-4689-8b45-7633fc923869', new_revelation_supersedes_prior_command, theological).
narrative_ontology:cs_axiom('b3ad6395-9d01-4689-8b45-7633fc923869', secondary, monogamy_is_eternal_principle).
narrative_ontology:cs_axiom_status(monogamy_is_eternal_principle, holdable).
narrative_ontology:cs_axiom_grounding('b3ad6395-9d01-4689-8b45-7633fc923869', monogamy_is_eternal_principle, theological).
narrative_ontology:cs_reference_frame('b3ad6395-9d01-4689-8b45-7633fc923869', post_manifesto_doctrinal_clarity).
narrative_ontology:cs_drift_state('b3ad6395-9d01-4689-8b45-7633fc923869', contemporary_fundamentalist_dissent, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b3ad6395-9d01-4689-8b45-7633fc923869', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__substitutionist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, institutional_leadership).
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, monogamous_members).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, polygamous_fundamentalists).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, dissenting_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The central authority that issued the Manifesto, framing it as new revelation. They benefit from maintaining institutional legitimacy and avoiding federal persecution, but are constrained by internal dissent and historical precedent. They actively enforce the new monogamous doctrine.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, institutional_leadership, agenda_setter,
    institutional, generational, constrained, global).

% Members who either already practiced monogamy or readily adopted it. They benefit from social acceptance, reduced legal risk, and alignment with the institutional mainstream. Their commitment is reinforced by the new doctrine.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, monogamous_members, beneficiary,
    moderate, biographical, mobile, local).

% Members who believe polygamy is a divine command and reject the Manifesto as a doctrinal shift. They face excommunication, social ostracization, and loss of community. Their identity is deeply tied to the prior practice, making exit extremely costly.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, polygamous_fundamentalists, payer,
    powerless, generational, identity_locked, local).

% Members who struggle with the doctrinal shift, feeling it contradicts prior revelation, but are not necessarily fundamentalists. They bear the cost of cognitive dissonance and potential social friction if they voice concerns, but may eventually conform or quietly exit.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, dissenting_members, payer,
    moderate, biographical, constrained, local).

% Exerted legal and political pressure against polygamy, leading to the Manifesto. They observe the institution's compliance and maintain legal frameworks that prohibit polygamy, effectively reinforcing the new doctrine.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, federal_government, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the institution's marriage practices with external legal norms, ensuring its survival and social acceptance within the broader society, while maintaining internal doctrinal coherence through a new 'revelation'.
% TRANSFER_FUNCTION: Transfers institutional legitimacy and social acceptance to the leadership and conforming members, while transferring the cost of apostasy and excommunication to those who adhere to the prior polygamous practice.
% ABSENT_VOICES: Historical figures who established polygamy as a divine command would object to its doctrinal rescission. Their voices are silenced by the framing of the Manifesto as superseding revelation, effectively rewriting history.
% DISAPPEARANCE_RATIONALE: If the substitutionist reading of the divine marriage command vanished, the institution would face a severe legitimacy crisis, potentially fracturing into multiple factions. Polygamous practices might re-emerge among fundamentalists, and the institution's relationship with the federal government would be destabilized.
% FOUNDING_PROBLEM: The institution faced existential threat from federal anti-polygamy laws, including confiscation of property and imprisonment of leaders, jeopardizing its very existence and the freedom of its members.
% FOUNDING_PROBLEM_CORROBORATION: The institutional leadership attests the problem of external pressure and the need for adaptation remains live. External historians and legal scholars corroborate the historical pressure but often frame the Manifesto as a pragmatic concession rather than a pure doctrinal shift, which is contested by the substitutionist reading.
narrative_ontology:disappearance_verdict(divine_marriage_command__substitutionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__substitutionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__substitutionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(divine_marriage_command__substitutionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__substitutionist_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__substitutionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__substitutionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because the doctrinal shift imposes significant costs on those who adhere to the prior practice, forcing them to abandon deeply held beliefs or face excommunication. Suppression is also high (0.78) due to the active enforcement by institutional leadership, including excommunication and social ostracization of fundamentalists. The theater ratio (0.40) reflects the performative aspect of framing a pragmatic shift as a 'new revelation' to maintain internal legitimacy, while the underlying function is institutional survival.
 *
 * PERSPECTIVAL GAP:
 *   The institutional leadership perceives this as a necessary adaptation and a genuine revelation, ensuring the survival and continued divine favor for the institution. Polygamous fundamentalists, however, experience it as a betrayal of core doctrine and a coercive act, leading to their marginalization and excommunication. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership and monogamous members are beneficiaries, gaining legitimacy and social acceptance. Polygamous fundamentalists and dissenting members are victims, bearing the costs of excommunication and identity conflict. The federal government acts as an external observer whose pressure indirectly reinforces the constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_vs_pragmatism,
    'Is the Manifesto a genuine new revelation, or a pragmatic institutional adaptation to external legal and social pressure?',
    'Analysis of internal theological discourse and external historical records, particularly focusing on the timing and content of the ''revelation'' in relation to federal anti-polygamy enforcement.',
    'If primarily pragmatic, the ''divine command'' aspect of this reading becomes theatrical, increasing the constraint''s effective theater_ratio and potentially reclassifying it closer to a Snare for those who believe in the prior command. If genuine revelation, the extraction is framed as a legitimate divine requirement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_vs_pragmatism, conceptual, 'Ambiguity between divine revelation and institutional pragmatism.').

omega_variable(
    internalized_suppression_of_dissent,
    'To what extent is the suppression of polygamous practice internalized by members, beyond active institutional enforcement?',
    'Sociological studies of former members and current members in private settings, assessing the psychological costs of adherence to the new doctrine versus the fear of excommunication.',
    'If suppression is highly internalized, the effective suppression is higher than the structural measure suggests, as members self-censor and police their own beliefs, making exit even more difficult. This would amplify the Snare-like qualities of the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_suppression_of_dissent, empirical, 'Structural vs. internalized suppression mechanism for doctrinal dissent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__substitutionist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_marriage_command__substitutionist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(divi_tr_t10, divine_marriage_command__substitutionist_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(divi_tr_t20, divine_marriage_command__substitutionist_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(divi_tr_t30, divine_marriage_command__substitutionist_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(divi_tr_t40, divine_marriage_command__substitutionist_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(divi_tr_t50, divine_marriage_command__substitutionist_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_marriage_command__substitutionist_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(divi_be_t10, divine_marriage_command__substitutionist_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(divi_be_t20, divine_marriage_command__substitutionist_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(divi_be_t30, divine_marriage_command__substitutionist_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(divi_be_t40, divine_marriage_command__substitutionist_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(divi_be_t50, divine_marriage_command__substitutionist_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_marriage_command__substitutionist_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(divi_su_t10, divine_marriage_command__substitutionist_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(divi_su_t20, divine_marriage_command__substitutionist_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(divi_su_t30, divine_marriage_command__substitutionist_reading, suppression_requirement, 30, 0.77).
narrative_ontology:measurement(divi_su_t40, divine_marriage_command__substitutionist_reading, suppression_requirement, 40, 0.78).
narrative_ontology:measurement(divi_su_t50, divine_marriage_command__substitutionist_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__substitutionist_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_marriage_command__substitutionist_reading, divine_marriage_command__continuationist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__substitutionist_reading, divine_marriage_command__coercion_visibility_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'divine_marriage_command' kernel. This 'substitutionist' reading asserts a new revelation superseding prior commands, leading to the doctrinal requirement of monogamy. It is linked to the 'continuationist' reading (Manifesto as temporary suspension) and the 'coercion_visibility' reading (Manifesto as pragmatic response to federal pressure), as they all interpret the same historical event and doctrinal shift.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
