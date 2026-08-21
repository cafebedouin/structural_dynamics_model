% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__symbol_survival_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__symbol_survival_reading, []).

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
 *   constraint_id: catastrophe_memory_survival__symbol_survival_reading
 *   human_readable: Catastrophe Memory Survival: Symbolic Continuity Reading
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint represents one reading of how ritual preserves collective
 *   memory and identity, specifically focusing on the continuity of symbolic
 *   practice itself as the mechanism for survival. It is a reading of the
 *   'catastrophe_memory_survival' kernel, which explores how communities
 *   transmit the memory of traumatic events. This 'symbol_survival_reading'
 *   emphasizes the intrinsic value of ritual form and symbolic experience,
 *   often enforced by traditional authorities, leading to high extraction
 *   from those who deviate or assimilate.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__symbol_survival_reading, 0.78).
domain_priors:suppression_score(catastrophe_memory_survival__symbol_survival_reading, 0.85).
domain_priors:theater_ratio(catastrophe_memory_survival__symbol_survival_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__symbol_survival_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__symbol_survival_reading, "Catastrophe Memory Survival: Symbolic Continuity Reading").
narrative_ontology:topic_domain(catastrophe_memory_survival__symbol_survival_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__symbol_survival_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__symbol_survival_reading, 'aa36b83b-72a2-4202-b876-d613dafb5c85').
narrative_ontology:cs_kernel_codification('aa36b83b-72a2-4202-b876-d613dafb5c85', formalized).
narrative_ontology:cs_authority_grounding('aa36b83b-72a2-4202-b876-d613dafb5c85', lineage).
narrative_ontology:cs_interpretation_layer_present('aa36b83b-72a2-4202-b876-d613dafb5c85').
narrative_ontology:cs_reading_relation('aa36b83b-72a2-4202-b876-d613dafb5c85', catastrophe_memory_survival__competence_transmission_reading, coexists_with).
narrative_ontology:cs_reading_relation('aa36b83b-72a2-4202-b876-d613dafb5c85', catastrophe_memory_survival__hybrid_encoding_reading, coexists_with).
narrative_ontology:cs_axiom('aa36b83b-72a2-4202-b876-d613dafb5c85', foundational, continuity_of_practice_is_survival).
narrative_ontology:cs_axiom_status(continuity_of_practice_is_survival, holdable).
narrative_ontology:cs_axiom_grounding('aa36b83b-72a2-4202-b876-d613dafb5c85', continuity_of_practice_is_survival, deontological).
narrative_ontology:cs_axiom('aa36b83b-72a2-4202-b876-d613dafb5c85', foundational, symbolic_experience_defines_identity).
narrative_ontology:cs_axiom_status(symbolic_experience_defines_identity, holdable).
narrative_ontology:cs_axiom_grounding('aa36b83b-72a2-4202-b876-d613dafb5c85', symbolic_experience_defines_identity, conventional).
narrative_ontology:cs_reference_frame('aa36b83b-72a2-4202-b876-d613dafb5c85', unbroken_ritual_chain).
narrative_ontology:cs_drift_state('aa36b83b-72a2-4202-b876-d613dafb5c85', contemporary_secularization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aa36b83b-72a2-4202-b876-d613dafb5c85', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, observant_jews).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, secularized_jews).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, assimilated_jews).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains interpretive control over ritual forms and their symbolic meaning, ensuring continuity of practice. Benefits from the social capital and legitimacy derived from preserving tradition, but is constrained by the tradition itself.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority, agenda_setter,
    institutional, generational, constrained, global).

% Derive a strong sense of collective identity, belonging, and meaning from participation in and adherence to traditional ritual practices. Their self-concept is deeply intertwined with this continuity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, observant_jews, beneficiary,
    organized, generational, identity_locked, global).

% Experience a loss of connection to collective memory and identity as they drift from traditional ritual forms. They pay the cost of cultural alienation or the burden of maintaining practices they no longer fully understand or believe in.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, secularized_jews, payer,
    powerless, biographical, constrained, local).

% Have largely exited traditional ritual practice, often losing direct transmission of symbolic meaning and collective memory. They bear the cost of a severed link to heritage, though their exit options are more open than those still within the community.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, assimilated_jews, payer,
    powerless, biographical, mobile, local).

% Analyze the historical development and sociological function of ritual in preserving identity and memory, often from an external, academic perspective. They do not participate in the ritual's internal economy of meaning.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, historical_scholars, observer,
    analytical, generational, analytical, universal).

% Would challenge the exclusive focus on symbolic continuity, arguing for more adaptive or less formal expressions of identity that might better serve contemporary needs, but are often marginalized by traditional authorities.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, cultural_critics, excluded,
    moderate, biographical, mobile, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority).
narrative_ontology:fixing_cost_class(catastrophe_memory_survival__symbol_survival_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective identity, group cohesion, and the transmission of shared memory through adherence to specific symbolic ritual practices and boundary-norms, especially in the face of external pressures.
% TRANSFER_FUNCTION: Transfers symbolic meaning, collective memory, and social belonging to participants, reinforcing group identity. It also transfers interpretive authority and social capital to rabbinic leadership, who are seen as custodians of the tradition.
% ABSENT_VOICES: Secularized and assimilated Jews, as well as cultural critics, who might advocate for more flexible or less formal approaches to identity and memory, are often excluded from the authoritative discourse on ritual's purpose.
% DISAPPEARANCE_RATIONALE: If the constraint of symbolic ritual continuity vanished, the collective identity and boundary-norms of the Jewish people would rapidly fragment, leading to widespread assimilation and the dissolution of distinct communal structures. The memory of catastrophe would lose its primary symbolic anchor.
% FOUNDING_PROBLEM: The existential threat of cultural and religious assimilation, the loss of distinct identity in diaspora, and the imperative to transmit the collective memory of historical catastrophes (e.g., the Exodus, the Holocaust) across generations.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic authorities and many observant Jews attest that the founding problem of assimilation and memory loss remains live. Some historical scholars and sociologists also corroborate the ongoing role of ritual in maintaining group boundaries, though they may dispute its exclusive symbolic function.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__symbol_survival_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__symbol_survival_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__symbol_survival_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(catastrophe_memory_survival__symbol_survival_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__symbol_survival_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__symbol_survival_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_survival__symbol_survival_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_survival__symbol_survival_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the emphasis on strict symbolic continuity can impose significant burdens on individuals, demanding adherence to forms that may feel anachronistic or irrelevant, and extracting conformity as the price of belonging. Suppression is very high (0.85) due to the active enforcement of ritual norms by rabbinic authority and the social pressure within observant communities to maintain 'purity' of practice, effectively suppressing alternative interpretations or less formal expressions of identity. Theater ratio is moderate (0.45) because while the symbolic experience is genuinely functional for identity, there is also a performative aspect to maintaining tradition, where the outward form can sometimes overshadow deeper meaning for some participants.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinic authority and observant Jews, this constraint is a vital Rope or even a Mountain, representing the natural and necessary means of collective survival. From the perspective of secularized or assimilated Jews, it operates as a Snare or Tangled Rope, extracting conformity and imposing costs for deviation, while suppressing alternative paths to identity or memory.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority and observant Jews are beneficiaries, as they gain social cohesion, identity, and interpretive control from the constraint. Secularized and assimilated Jews are victims, bearing the cost of cultural alienation or loss of heritage due to the rigid adherence to symbolic forms. The constraint subsidizes the continuity of a specific identity at the expense of those who cannot or choose not to conform.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this as a pure Rope. While it genuinely coordinates identity and memory, the high extractiveness and active suppression, coupled with identifiable victims, reveal the asymmetric costs and enforcement required to maintain this specific mode of 'survival'. The coordination story (identity preservation) serves as a cover for the extraction of conformity and interpretive control.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine, independent mechanism of memory transmission, or primarily one specific reading of the ''catastrophe_memory_survival'' kernel?',
    'Comparative analysis with sibling readings (''competence_transmission_reading'', ''hybrid_encoding_reading'') to identify unique structural elements and their independent causal efficacy, rather than shared features of the broader kernel.',
    'If primarily a reading, its classification is contingent on the kernel''s overall stability and the contestation among readings. If independent, its classification stands on its own merits, with network links to the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Clarifies the structural relationship between this constraint and its kernel.').

omega_variable(
    symbolic_vs_practical_function,
    'To what extent does ritual''s ''symbolic experience'' genuinely preserve identity and boundary-norms, versus encoding practical survival knowledge (e.g., resource management, social networks)?',
    'Ethnographic studies and historical analysis comparing communities that emphasize symbolic purity versus those that prioritize practical knowledge transmission, assessing long-term survival and adaptation outcomes.',
    'If practical knowledge is a dominant factor, the extractiveness of purely symbolic adherence might be higher than warranted, suggesting a Snare. If symbolic experience is indeed primary, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_vs_practical_function, empirical, 'Ambiguity in the primary function of ritual for survival.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.85) primarily structural (e.g., social exclusion, lack of alternative communal structures) or internalized (e.g., guilt, identity-fusion making deviation unthinkable)?',
    'Post-exit trajectory analysis: if individuals who leave the observant community continue to experience internal conflict or a sense of loss, it suggests a significant internalized component. If external barriers are the primary deterrent, structural suppression is dominant.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as individuals carry the suppression with them after physical exit. This would amplify the Snare-like qualities of the Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in ritual adherence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__symbol_survival_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t1945, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 1945, 0.4).
narrative_ontology:measurement(cata_tr_t1960, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 1960, 0.42).
narrative_ontology:measurement(cata_tr_t1975, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 1975, 0.43).
narrative_ontology:measurement(cata_tr_t1990, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 1990, 0.44).
narrative_ontology:measurement(cata_tr_t2005, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 2005, 0.45).
narrative_ontology:measurement(cata_tr_t2025, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(cata_be_t1945, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 1945, 0.65).
narrative_ontology:measurement(cata_be_t1960, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 1960, 0.7).
narrative_ontology:measurement(cata_be_t1975, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 1975, 0.73).
narrative_ontology:measurement(cata_be_t1990, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 1990, 0.75).
narrative_ontology:measurement(cata_be_t2005, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 2005, 0.77).
narrative_ontology:measurement(cata_be_t2025, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t1945, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 1945, 0.7).
narrative_ontology:measurement(cata_su_t1960, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 1960, 0.75).
narrative_ontology:measurement(cata_su_t1975, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 1975, 0.8).
narrative_ontology:measurement(cata_su_t1990, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 1990, 0.82).
narrative_ontology:measurement(cata_su_t2005, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 2005, 0.84).
narrative_ontology:measurement(cata_su_t2025, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 2025, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__symbol_survival_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival__competence_transmission_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival__hybrid_encoding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'catastrophe_memory_survival' kernel. This 'symbol_survival_reading' emphasizes the continuity of symbolic practice itself, while 'competence_transmission_reading' focuses on practical knowledge, and 'hybrid_encoding_reading' integrates both. Each reading has distinct ε values and stakeholder dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
