% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__competence_transmission_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__competence_transmission_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_memory_survival__competence_transmission_reading
 *   human_readable: Ritual as Competence Transmission: Survival Knowledge Embedded in Religious Practice
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   Religious ritual, particularly in cultures marked by catastrophic
 *   disruption or diaspora, encodes and transmits practical survival
 *   knowledge: agricultural timing, resource management, family protocols,
 *   adaptation strategies to ecological stress. This reading treats ritual as
 *   a vehicle for embedded competence — the practical knowledge is encoded in
 *   symbolic form, and maintenance of the ritual practice preserves the
 *   knowledge for communities that have access to interpretation. The
 *   constraint operates as tangled_rope: genuine coordination function
 *   (survival knowledge is preserved and transmitted through ritual), but
 *   asymmetric extraction (tradition-bearing authorities control
 *   interpretation while younger generations or dispersed communities may
 *   maintain form without understanding embedded content). The claim/metric
 *   divergence is deliberate: this reading is authored from the
 *   competence-transmission vantage point, not the symbolic-preservation or
 *   hybrid-function vantage points of the sibling readings. The engine will
 *   compute how each seat (tradition-bearer, diaspora community, younger
 *   generation, analytical observer) experiences this constraint; that
 *   per-seat classification is where the structural divergence across
 *   readings becomes empirically visible.
 *
 * KEY AGENTS:
 *   - tradition_bearers_with_ritual_authority: Elders, ritual specialists, custodians of interpretive knowledge — they control the encoding and can decode the practical content embedded in ritual performance. High power, high authority, low exit (identity-fused with the role).
 *   - diaspora_communities_with_ritual_access: Communities separated from the home territory but maintaining ritual practice — they receive the coordination benefit (survival knowledge encoded) but depend on tradition-bearers for full interpretation.
 *   - communities_losing_practical_content: Communities where ritual form persists but the competence-interpretation chain is broken (assimilation, language loss, disruption of transmission) — they perform the ritual but do not understand the embedded practical knowledge.
 *   - younger_generations_in_origin_communities: Facing accelerating cultural change, education systems prioritizing different knowledge, economic restructuring — they may inherit ritual form but lack the context to extract its practical content.
 *   - diaspora_organizational_structures: Immigrant communities, refugee organizations, religious institutions in diaspora — they may enforce ritual maintenance as identity boundary without explicit framing of the practical-knowledge function.
 *   - analytical_observer: Ethnographer, historian, comparative religionist studying the constraint structure — can see both the practical knowledge embedded in ritual and the authority-dependent extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__competence_transmission_reading, 0.62).
domain_priors:suppression_score(catastrophe_memory_survival__competence_transmission_reading, 0.58).
domain_priors:theater_ratio(catastrophe_memory_survival__competence_transmission_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__competence_transmission_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__competence_transmission_reading, "Ritual as Competence Transmission: Survival Knowledge Embedded in Religious Practice").
narrative_ontology:topic_domain(catastrophe_memory_survival__competence_transmission_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__competence_transmission_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__competence_transmission_reading, 'bfcc73b0-0ef0-416b-bc10-a0319ca79e2c').
narrative_ontology:cs_kernel_codification('bfcc73b0-0ef0-416b-bc10-a0319ca79e2c', distributed).
narrative_ontology:cs_authority_grounding('bfcc73b0-0ef0-416b-bc10-a0319ca79e2c', lineage).
narrative_ontology:cs_interpretation_layer_present('bfcc73b0-0ef0-416b-bc10-a0319ca79e2c').
narrative_ontology:cs_reading_relation('bfcc73b0-0ef0-416b-bc10-a0319ca79e2c', catastrophe_memory_survival__symbol_survival_reading, coexists_with).
narrative_ontology:cs_reading_relation('bfcc73b0-0ef0-416b-bc10-a0319ca79e2c', catastrophe_memory_survival__hybrid_encoding_reading, influences).
narrative_ontology:cs_axiom('bfcc73b0-0ef0-416b-bc10-a0319ca79e2c', foundational, practical_knowledge_is_primary_survival_function).
narrative_ontology:cs_axiom_status(practical_knowledge_is_primary_survival_function, holdable).
narrative_ontology:cs_axiom_grounding('bfcc73b0-0ef0-416b-bc10-a0319ca79e2c', practical_knowledge_is_primary_survival_function, empirically_contingent).
narrative_ontology:cs_axiom('bfcc73b0-0ef0-416b-bc10-a0319ca79e2c', foundational, authority_asymmetry_enables_extraction).
narrative_ontology:cs_axiom_status(authority_asymmetry_enables_extraction, holdable).
narrative_ontology:cs_axiom_grounding('bfcc73b0-0ef0-416b-bc10-a0319ca79e2c', authority_asymmetry_enables_extraction, deontological).
narrative_ontology:cs_reference_frame('bfcc73b0-0ef0-416b-bc10-a0319ca79e2c', intact_transmission_with_competence).
narrative_ontology:cs_drift_state('bfcc73b0-0ef0-416b-bc10-a0319ca79e2c', contemporary_diaspora_and_secularization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bfcc73b0-0ef0-416b-bc10-a0319ca79e2c', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, diaspora_communities_with_ritual_access).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, tradition_bearers_with_interpretive_authority).
narrative_ontology:constraint_victim(catastrophe_memory_survival__competence_transmission_reading, communities_losing_practical_content).
narrative_ontology:constraint_victim(catastrophe_memory_survival__competence_transmission_reading, younger_generations_receiving_form_without_competence).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__competence_transmission_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_survival__competence_transmission_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__competence_transmission_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_survival__competence_transmission_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_survival__competence_transmission_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is moderate-to-high (0.62 at interval end) because the constraint creates asymmetric knowledge access: tradition-bearers control interpretation and younger/diaspora communities must rely on them or lose the embedded practical content. The extraction is not simple rent-collection but selective knowledge access — those without interpretive authority cannot independently extract the practical wisdom encoded in the ritual. Suppression is substantial (0.58) because the constraint persists through internalized commitment to tradition (identity-locked attachment to ritual form) and the real difficulty of reconstructing competence from broken transmission chains. Theater rises over the interval (0.28 to 0.50) because as communities urbanize and practical conditions change (agriculture becomes less central, family structures shift, ecological adaptation becomes less visible), ritual performance increasingly becomes ceremonial maintenance of identity rather than active survival practice — the functional content erodes while the form persists. This trajectory is empirically grounded in diaspora acculturation studies and post-industrial secularization patterns. Suppression_requirement stays elevated because active enforcement (teaching, correction, social sanction for non-participation) is required to maintain the practice even when its practical function is attenuated. The measurement series one shared grid so every metric is authored at every examined time point (0, 10, 20, 30, 40, 50).
 *
 * PERSPECTIVAL GAP:
 *   The tradition-bearer seat and the younger-generation seat should compute distinctly. From the tradition-bearer's position, ritual is a coordination mechanism they steward — they benefit from the knowledge being preserved and see themselves as serving community survival. From the younger-generation seat, the constraint appears more extractive: they are required to participate in a practice whose practical purpose they do not understand, and their exit options (assimilation, selective adoption) carry identity costs. A diaspora community with intact transmission computes differently from one where the authority chain is broken — the first retains access to the competence; the second maintains form-without-content. The engine computes these divergences from the structural data; the interpretation comes from seat-specific directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Tradition-bearers sit near the beneficiary end (d~0.2): they control the knowledge, maintain authority through its interpretation, benefit from communities depending on them for transmission, but also carry the burden of stewardship. Younger generations and communities losing competence sit near the target end (d~0.75-0.85): they are required to participate in ritual practice, their autonomy is constrained by social expectation, and they lose access to practical knowledge if transmission is broken. Diaspora communities with ritual access sit near symmetric (d~0.50-0.60): they receive genuine coordination benefit (knowledge preserved in ritual form) but depend on tradition-bearers for its interpretation and may lose it if authority relationships are disrupted. The accessibility_collapse value (0.71) reflects that once a community is embedded in ritual practice, exit is psychologically costly (identity-fused) even when the practical knowledge is no longer functionally necessary for survival — alternatives (secular knowledge systems, simplified practice) exist but carry identity costs. Resistance is lower (0.42) because the constraint's legitimacy rests on genuine practical value and real need during genuine catastrophe — in periods when survival pressure is acute, maintenance of ritual knowledge is valued; resistance arises primarily when the practical function becomes attenuated and the form is maintained theatrically.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids the mandatrophy problem (constraint persists after founding problem is solved) by treating the founding problem narrowly: the problem is 'transmission of survival knowledge across catastrophic disruption,' not 'preservation of cultural identity through symbolic practice.' As long as actual survival knowledge is being effectively transmitted, the constraint is live. Mandatrophy occurs when ritual persists in form after the competence transmission function has atrophied — this is visible in the measurement trajectory: theater_ratio rises from 0.28 to 0.50 as the practical function erodes relative to ceremonial maintenance. At intervals 40-50, the constraint begins to show signs of mandatrophy: theater_ratio plateaus near 0.50 (half the activity is performative maintenance rather than competence transmission), yet suppression_requirement remains high (0.58) because communities still enforce participation through social sanction. The theater rise and plateau are the empirical signatures that the founding problem (knowledge transmission) is degrading while the enforcement infrastructure persists. A hybrid_encoding_reading would read the same measurements differently: it would see the theater rise as evidence that the symbolic function (identity/boundary maintenance) is intact even as the practical function attenuates — the readings diverge on what counts as constraint failure. This reading treats theater rise as evidence of functional degradation; the hybrid reading treats it as evidence of repurposing into a different (but still vital) coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deliberate_vs_emergent_encoding,
    'Is the survival knowledge embedded in ritual the result of deliberate, conscious encoding by communities aware they were transmitting competence, or has practical wisdom become entangled with symbolic practice through long evolution without explicit intentionality?',
    'Ethnographic evidence of knowledge-holders'' metalinguistic awareness (do practitioners describe themselves as encoding survival information, or only as maintaining tradition?); historical reconstruction of encoding decisions in crisis moments; comparison of ritual content with documented survival strategies from the same period and place.',
    'If deliberate: the reading treats ritual as a functional container for practical knowledge, and communities that maintain form without understanding content have lost a resource. If emergent: the boundary between practical and symbolic becomes blurred, and the reading may misidentify which parts of ritual are functional knowledge versus cultural crystallization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberate_vs_emergent_encoding, empirical, 'Whether survival-knowledge embedding in ritual is intentional or emergent.').

omega_variable(
    competence_loss_asymmetry,
    'Do communities that lose the practical competence embedded in their ritual actually experience worse survival outcomes, or does the symbolic envelope preserve adaptive capacity even without explicit knowledge of the mechanisms?',
    'Comparative study of diaspora populations separated from ritual authority (loss of both form and content) versus those maintaining form without interpretive access (loss of competence, retention of practice); measurement of health, family stability, resource management outcomes; longitudinal data from acculturation studies.',
    'If competence loss causes measurable harm: ritual acts as a genuine extraction mechanism, because some communities preserve form while losing the practical knowledge that makes it adaptive. If symbolic maintenance suffices: the practical-knowledge reading overestimates what is at stake, and ritual functions primarily as identity boundary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competence_loss_asymmetry, empirical, 'Whether loss of practical competence produces measurable adaptive harm.').

omega_variable(
    hybrid_kernel_integration,
    'This reading parses the kernel (catastrophe_memory_survival) as primarily encoding mechanism for practical knowledge. The sibling hybrid_encoding_reading claims ritual operates on dual registers (symbolic AND practical), and both are essential. Can the practical-knowledge reading accommodate dual registers, or does commitment to explicit competence transmission foreclose the hybrid reading?',
    'Examine whether this reading''s core premise (ritual as competence transmission vehicle) logically requires that non-practical (symbolic, identity, boundary-maintenance) functions are secondary or contingent. Identify whether recognizing dual function degrades the practical-knowledge reading''s explanatory power or merely adds a layer of complexity.',
    'If the practical-knowledge reading requires symbolic functions to be secondary: the two readings foreclose one another, and communities must choose which register to prioritize. If practical and symbolic can be held simultaneously without contradiction: the readings coexist, and the constraint can be analyzed from either vantage point depending on what is at stake in the analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hybrid_kernel_integration, conceptual, 'Logical relationship between practical-competence and dual-function readings of the same kernel.').

omega_variable(
    authority_loss_transmission,
    'When a diaspora community loses access to ritual authorities (elders, teachers, interpreters), is the loss of practical knowledge downstream of the loss of authority, or is competence preserved through distributed transmission in the community itself?',
    'Ethnographic study of diaspora communities: track which competencies persist in the absence of formal authority structures and which attenuate; distinguish transmission through kinship channels, peer learning, and documentation versus formal apprenticeship.',
    'If competence depends on sustained authority: the constraint is enforcement-dependent (requires active teaching relationships), and diaspora dispersal is its primary failure mode. If competence distributes through community networks: the constraint is more robust to authority loss, and failure occurs only when community practice itself is disrupted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_loss_transmission, empirical, 'Whether transmission of practical knowledge requires formal ritual authority or distributes through community networks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__competence_transmission_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 40, 0.5).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 50, 0.47).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0, 0.41).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 20, 0.54).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__competence_transmission_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_survival__competence_transmission_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival__symbol_survival_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival__hybrid_encoding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_memory_survival kernel. The competence_transmission_reading treats ritual as an encoding mechanism for practical survival knowledge; extraction arises through selective knowledge access by tradition-bearers. The symbol_survival_reading treats ritual as identity-boundary maintenance; survival is continuity of practice. The hybrid_encoding_reading claims both functions are co-essential and inseparable. These are three distinct constraints with different ε values and beneficiary/victim structures. The readings coexist across different stakeholder positions: a tradition-bearer adopts the competence-transmission frame; a community experiencing diaspora pressure may adopt the symbol-survival frame; an integrative analysis adopts the hybrid frame. The network links them because determining which reading applies changes what counts as constraint success and where extraction occurs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
