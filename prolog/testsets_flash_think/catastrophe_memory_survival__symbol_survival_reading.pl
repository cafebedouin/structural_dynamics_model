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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Catastrophe Memory Survival: Ritual as Symbolic Continuity
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes the role of ritual in preserving Jewish
 *   identity and boundary-norms through symbolic experience, where survival
 *   is equated with the continuity of practice itself. It is presented as the
 *   'symbol_survival_reading' of the 'catastrophe_memory_survival' kernel.
 *   The constraint's high extractiveness stems from the rigid enforcement of
 *   ritual form by rabbinic authority, which benefits traditional communities
 *   by reinforcing their identity, but extracts conformity from those who
 *   might seek adaptation or find traditional forms alienating. The claimed
 *   type is 'tangled_rope' because it genuinely coordinates identity and
 *   memory, but does so with significant asymmetric extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__symbol_survival_reading, 0.82).
domain_priors:suppression_score(catastrophe_memory_survival__symbol_survival_reading, 0.78).
domain_priors:theater_ratio(catastrophe_memory_survival__symbol_survival_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__symbol_survival_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__symbol_survival_reading, "Catastrophe Memory Survival: Ritual as Symbolic Continuity").
narrative_ontology:topic_domain(catastrophe_memory_survival__symbol_survival_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__symbol_survival_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__symbol_survival_reading, '9c348a2e-547e-466c-9b71-63eaf245d3eb').
narrative_ontology:cs_kernel_codification('9c348a2e-547e-466c-9b71-63eaf245d3eb', formalized).
narrative_ontology:cs_authority_grounding('9c348a2e-547e-466c-9b71-63eaf245d3eb', lineage).
narrative_ontology:cs_interpretation_layer_present('9c348a2e-547e-466c-9b71-63eaf245d3eb').
narrative_ontology:cs_reading_relation('9c348a2e-547e-466c-9b71-63eaf245d3eb', catastrophe_memory_survival__competence_transmission_reading, coexists_with).
narrative_ontology:cs_reading_relation('9c348a2e-547e-466c-9b71-63eaf245d3eb', catastrophe_memory_survival__hybrid_encoding_reading, coexists_with).
narrative_ontology:cs_axiom('9c348a2e-547e-466c-9b71-63eaf245d3eb', foundational, ritual_form_is_identity).
narrative_ontology:cs_axiom_status(ritual_form_is_identity, holdable).
narrative_ontology:cs_axiom_grounding('9c348a2e-547e-466c-9b71-63eaf245d3eb', ritual_form_is_identity, deontological).
narrative_ontology:cs_axiom('9c348a2e-547e-466c-9b71-63eaf245d3eb', foundational, continuity_of_practice_is_survival).
narrative_ontology:cs_axiom_status(continuity_of_practice_is_survival, holdable).
narrative_ontology:cs_axiom_grounding('9c348a2e-547e-466c-9b71-63eaf245d3eb', continuity_of_practice_is_survival, conventional).
narrative_ontology:cs_reference_frame('9c348a2e-547e-466c-9b71-63eaf245d3eb', halakhic_purity_and_continuity).
narrative_ontology:cs_drift_state('9c348a2e-547e-466c-9b71-63eaf245d3eb', contemporary_secularization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9c348a2e-547e-466c-9b71-63eaf245d3eb', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, traditional_jewish_communities).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, secularized_jews).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, diaspora_jews_seeking_adaptation).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__symbol_survival_reading, halakhic_continuity_doctrine).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__symbol_survival_reading, ritual_purity_as_identity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains interpretive control over ritual practice and doctrine, enforcing adherence to traditional forms. Benefits from the stability and continuity of the ritual system, which reinforces its own legitimacy and authority. Views deviations as threats to collective identity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority, agenda_setter,
    institutional, generational, identity_locked, global).

% Derives strong collective identity, social cohesion, and a sense of historical continuity from strict adherence to traditional ritual forms. Benefits from the clarity and stability provided by rabbinic enforcement, seeing it as essential for survival.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, traditional_jewish_communities, beneficiary,
    organized, generational, identity_locked, global).

% Experience alienation or exclusion due to the rigid demands of traditional ritual, which may not resonate with their modern worldview or lifestyle. Bear the cost of non-participation through loss of community ties or a sense of cultural belonging, or the burden of performing rituals they find meaningless.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, secularized_jews, payer,
    powerless, biographical, constrained, local).

% Seek to adapt ritual practices to contemporary contexts, often emphasizing personal meaning over strict adherence to form. Face resistance and sometimes condemnation from traditional authorities, bearing the cost of internal conflict, communal division, or being labeled as 'less authentic'.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, diaspora_jews_seeking_adaptation, payer,
    moderate, biographical, constrained, global).

% Study the historical, sociological, and psychological functions of ritual in preserving identity and memory, including the dynamics of adaptation and resistance. Their analysis provides an external perspective on the constraint's operation without direct participation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, analytical_scholars_of_religion, observer,
    analytical, biographical, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority).
narrative_ontology:fixing_cost_class(catastrophe_memory_survival__symbol_survival_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective identity and memory across generations and geographies by providing a shared symbolic language and set of practices, ensuring group cohesion and continuity in the face of external pressures.
% TRANSFER_FUNCTION: Transfers symbolic capital, a sense of belonging, and interpretive authority from the rabbinic establishment to traditional communities, while extracting conformity and adherence to form from all participants, especially those seeking adaptation.
% ABSENT_VOICES: Those who have fully disaffiliated from Jewish life due to the perceived rigidity or irrelevance of traditional ritual, or those who have created entirely new, non-traditional forms of Jewish expression that are not recognized by the established authorities. They would argue for radical autonomy in identity formation.
% DISAPPEARANCE_RATIONALE: If the constraint of ritual as symbolic continuity vanished, the primary mechanism for transmitting Jewish identity and memory would dissolve. Traditional communities would lose their core organizing principle, and the collective memory of catastrophe would fragment, leading to a profound reorganization of Jewish communal life and self-understanding.
% FOUNDING_PROBLEM: The existential threat of cultural and religious annihilation following historical catastrophes (e.g., exile, Holocaust), necessitating a robust mechanism to preserve Jewish identity, memory, and communal bonds across generations.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic authorities and traditional communities universally attest that the founding problem of identity survival remains live, citing ongoing assimilation and external pressures. Sociological studies of Jewish identity and collective memory, from outside the benefiting parties, corroborate the continued salience of identity preservation as a communal challenge, even if they dispute the efficacy or necessity of strict ritual adherence as the sole solution.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__symbol_survival_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__symbol_survival_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__symbol_survival_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(catastrophe_memory_survival__symbol_survival_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__symbol_survival_reading, 0.82, 'gemini-2.5-flash', 'none', direct).

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
 *   Base extractiveness is high (0.82) because the emphasis on strict ritual form imposes significant costs on those who deviate or seek adaptation, often leading to exclusion or internal conflict. Suppression (0.78) is also high, reflecting the social and theological pressure to conform, enforced by rabbinic authority and communal norms. Accessibility collapse (0.70) is substantial, as leaving the traditional ritual framework means losing a primary avenue for Jewish identity. Resistance (0.60) is moderate, manifesting as internal debates, reform movements, and individual disaffiliation rather than overt rebellion. Theater ratio (0.45) is moderate; while the symbolic experience is genuinely functional for identity, a portion of the effort goes into maintaining the 'performance' of continuity even when the underlying meaning may be contested or attenuated for some participants. The metrics show a slight increase in extractiveness and suppression over time, reflecting the hardening of positions in response to modern challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinic authority and traditional communities, the constraint is a vital 'rope' for collective survival, ensuring the transmission of a sacred heritage. They perceive the costs as necessary for maintaining authenticity and continuity. From the perspective of secularized Jews or those seeking adaptation, it operates as a 'snare' or 'tangled_rope', extracting conformity and alienating those who cannot or will not adhere to rigid forms, with the coordination function serving primarily the interests of the established authority.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority and traditional Jewish communities are the primary beneficiaries, as the constraint reinforces their legitimacy, social cohesion, and sense of identity. Secularized Jews and those seeking adaptation are the victims/payers, bearing the costs of exclusion, alienation, or the burden of conformity. Their exit options are constrained by the deep identity ties to Jewish peoplehood, making full disaffiliation a high-cost choice.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is to ensure the survival of Jewish identity and memory after catastrophe. While this problem remains live, the 'symbol_survival_reading' risks mandatrophy by prioritizing the continuity of ritual form over its adaptive function or the evolving needs of diverse Jewish populations. The high extraction and suppression suggest that the mechanism for 'survival' may have become an end in itself, potentially alienating those it purports to save. The engine's classification as 'tangled_rope' (rather than the claimed 'rope') would highlight this tension between coordination and extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbolic_vs_practical_survival,
    'Is the ''survival'' achieved by ritual primarily symbolic (identity, memory) or does it also encode and transmit practical, adaptive knowledge for group resilience?',
    'Comparative analysis with the ''competence_transmission_reading'' and ''hybrid_encoding_reading'' of the same kernel, examining historical and ethnographic evidence for the practical efficacy of ritual forms in crisis contexts.',
    'If practical knowledge is a significant component, the constraint''s coordination function is broader, potentially lowering effective extraction by justifying more of the ''cost'' as functional. If purely symbolic, the extraction for form preservation is more salient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_vs_practical_survival, conceptual, 'Ambiguity regarding the nature of ''survival'' facilitated by ritual.').

omega_variable(
    internalized_suppression_of_adaptation,
    'To what extent is the suppression of ritual adaptation structural (rabbinic authority, communal pressure) versus internalized (self-censorship, identity-locked adherence to tradition even without external enforcement)?',
    'Longitudinal studies of individuals who leave traditional communities: if the internal pressure to conform persists after structural barriers are removed, it indicates a higher degree of internalized suppression.',
    'If internalized suppression is high, the effective suppression for individuals is greater than the structural measure suggests, making exit more psychologically costly and reinforcing the constraint''s persistence even in weakening external enforcement contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_of_adaptation, empirical, 'Structural vs. internalized suppression mechanism for ritual conformity.').

omega_variable(
    kernel_reading_distinction,
    'Is this ''symbol_survival_reading'' sufficiently distinct from its sibling readings (''competence_transmission_reading'', ''hybrid_encoding_reading'') to warrant separate constraint stories, or do they represent different facets of a single, more complex constraint?',
    'Further analysis of the ε-invariance principle: if a single constraint could genuinely yield widely divergent ε values depending on whether symbolic or practical aspects are emphasized, then decomposition is warranted. If the ε values are consistently similar across framings, they might be better modeled as a single constraint with nuanced commentary.',
    'If not sufficiently distinct, the corpus would contain redundant or artificially separated constraints, potentially skewing aggregate classification data. If distinct, it validates the decomposition strategy for complex cultural phenomena.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Confirms the validity of decomposing the ''catastrophe_memory_survival'' kernel into distinct readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__symbol_survival_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 10, 0.41).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 20, 0.43).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 30, 0.44).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 10, 0.74).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 30, 0.8).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 40, 0.81).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 50, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 10, 0.69).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 20, 0.73).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 40, 0.77).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__symbol_survival_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival__competence_transmission_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival__hybrid_encoding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'catastrophe_memory_survival' kernel, each focusing on a different aspect of how ritual contributes to group survival. They are linked to capture their shared origin and interdependencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
