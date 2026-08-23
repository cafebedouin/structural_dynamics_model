% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__symbol_survival_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: catastrophe_memory_survival__symbol_survival_reading
 *   human_readable: Ritual Form Preservation as Survival Mechanism (Symbolic Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint story captures the 'symbol_survival_reading' of the
 *   catastrophe_memory_survival kernel. It models the claim that Jewish
 *   survival depends on the continuity of ritual form itself — that the
 *   symbolic experience of ritual (Shabbat, kashrut, lifecycle events) is the
 *   primary vessel of identity and boundary-maintenance. The reading is
 *   championed by traditional rabbinic authority, which presents it as a
 *   natural law of Jewish persistence. The metrics, however, describe a
 *   constraint that has become substantially extractive: rabbinic authority
 *   maintains interpretive control over ritual form, secularized Jews bear
 *   the cost of conformity without belief, and the coordination function
 *   (identity preservation) is real but increasingly serves to legitimize the
 *   authority structure. The claimed_type is tangled_rope because the
 *   constraint coordinates identity (genuine function) while extracting
 *   interpretive monopoly (asymmetric benefit) and requires active
 *   enforcement (halakhic policing, communal sanctions).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__symbol_survival_reading, 0.75).
domain_priors:suppression_score(catastrophe_memory_survival__symbol_survival_reading, 0.7).
domain_priors:theater_ratio(catastrophe_memory_survival__symbol_survival_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__symbol_survival_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__symbol_survival_reading, "Ritual Form Preservation as Survival Mechanism (Symbolic Reading)").
narrative_ontology:topic_domain(catastrophe_memory_survival__symbol_survival_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__symbol_survival_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__symbol_survival_reading, '7c243820-b7a7-440e-860d-dc867250d1f8').
narrative_ontology:cs_kernel_codification('7c243820-b7a7-440e-860d-dc867250d1f8', fixed_text).
narrative_ontology:cs_authority_grounding('7c243820-b7a7-440e-860d-dc867250d1f8', extraction).
narrative_ontology:cs_interpretation_layer_present('7c243820-b7a7-440e-860d-dc867250d1f8').
narrative_ontology:cs_reading_relation('7c243820-b7a7-440e-860d-dc867250d1f8', catastrophe_memory_survival__competence_transmission_reading, coexists_with).
narrative_ontology:cs_reading_relation('7c243820-b7a7-440e-860d-dc867250d1f8', catastrophe_memory_survival__hybrid_encoding_reading, coexists_with).
narrative_ontology:cs_axiom('7c243820-b7a7-440e-860d-dc867250d1f8', foundational, ritual_form_preservation_is_sufficient_for_survival).
narrative_ontology:cs_axiom_status(ritual_form_preservation_is_sufficient_for_survival, holdable).
narrative_ontology:cs_axiom_grounding('7c243820-b7a7-440e-860d-dc867250d1f8', ritual_form_preservation_is_sufficient_for_survival, deontological).
narrative_ontology:cs_axiom('7c243820-b7a7-440e-860d-dc867250d1f8', secondary, interpretive_authority_derives_from_form_preservation).
narrative_ontology:cs_axiom_status(interpretive_authority_derives_from_form_preservation, holdable).
narrative_ontology:cs_axiom_grounding('7c243820-b7a7-440e-860d-dc867250d1f8', interpretive_authority_derives_from_form_preservation, conventional).
narrative_ontology:cs_reference_frame('7c243820-b7a7-440e-860d-dc867250d1f8', rabbinic_traditionalist_framework).
narrative_ontology:cs_drift_state('7c243820-b7a7-440e-860d-dc867250d1f8', modern_secular_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7c243820-b7a7-440e-860d-dc867250d1f8', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, secularized_jews).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, traditional_practitioners).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__symbol_survival_reading, continuity_of_practice_ensures_survival).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__symbol_survival_reading, ritual_form_preserves_identity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces ritual forms through halakhic interpretation, controls the definition of legitimate practice, and derives authority and material support from maintaining the symbolic boundary between Jewish and non-Jewish life. Can shift interpretations but is incentivized to preserve the form that sustains its interpretive monopoly.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority, beneficiary).

% Experience pressure to conform to ritual forms they no longer believe in or face communal exclusion and loss of identity recognition. Their Jewish identity is structurally tied to a ritual framework they cannot authentically inhabit, yet leaving the identity is psychologically and socially costly.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, secularized_jews, payer,
    moderate, biographical, identity_locked, global).

% Find genuine meaning and communal belonging in the preserved ritual forms. They benefit from the coordination function (shared practice, identity continuity) and voluntarily participate, but their participation also reinforces the constraint's enforcement structure.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, traditional_practitioners, beneficiary,
    moderate, biographical, mobile, global).

% Advocate for ritual adaptation and inclusive boundaries but are denied authoritative voice in the traditional framework. Their alternative interpretations are structurally excluded from the halakhic conversation that defines the constraint.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, reform_jewish_movements, excluded,
    organized, generational, constrained, global).

% Analyze the historical development and social function of ritual preservation from outside the commitment system. They document the constraint's operation without being subject to its enforcement or benefiting from its coordination.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, scholars_of_jewish_studies, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ritual preserves Jewish identity and boundary-norms through symbolic experience, ensuring collective survival by maintaining continuity of practice across generations and catastrophes.
% TRANSFER_FUNCTION: Moves interpretive authority and communal belonging from secularized Jews to rabbinic authorities, who control the definition and enforcement of ritual forms. The transfer is not monetary but symbolic: the right to define what counts as Jewish survival.
% ABSENT_VOICES: Secular Jewish movements, cultural Jews, and progressive rabbinic voices who seek alternative forms of continuity are structurally excluded from the authoritative definition of ritual. They would object to the equation of survival with ritual form preservation but are kept out by the same interpretive monopoly the constraint protects.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, rabbinic authority would lose its central coordinating role, the boundary between Jewish and non-Jewish would become porous, secularized Jews would no longer face pressure to conform to rituals they don't believe in, and Jewish identity would reorganize around voluntary association, cultural memory, or new symbolic forms.
% FOUNDING_PROBLEM: After the catastrophe (destruction of the Temple, exile, and later the Holocaust), Jewish survival was threatened by assimilation and loss of collective identity; ritual continuity was instituted as the primary survival mechanism when political sovereignty and territorial rootedness were lost.
% FOUNDING_PROBLEM_CORROBORATION: Traditional rabbinic authorities attest the problem is live, citing ongoing assimilation and intermarriage rates. Secular Jewish historians and sociologists (e.g., Simon Rawidowicz, Barbara Kirshenblatt-Gimblett) attest the founding problem has shifted: survival is no longer existential but cultural, and the constraint now serves institutional maintenance more than survival. The Israeli Ministry of Diaspora Affairs acknowledges both continuity and change in its policy documents.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__symbol_survival_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__symbol_survival_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__symbol_survival_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_survival__symbol_survival_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__symbol_survival_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is high (0.75) because the ritual form is preserved at the cost of excluding secularized Jews from authentic participation, and the rabbinic authority captures the interpretive rents. Suppression is high (0.7) because the constraint depends on communal sanctions (exclusion from minyan, marriage, burial) and internalized guilt to maintain conformity. Theater ratio (0.4) reflects that the coordination function (shared practice creating belonging) is real but a growing share of enforcement energy defends the authority's interpretive monopoly rather than the practice itself. Accessibility collapse (0.8) is high because the symbolic framework defines Jewishness so completely that alternatives (secular Jewish culture, cultural Judaism) are treated as inauthentic or transient. Resistance (0.5) is moderate: secularized Jews resist internally and through exit (disaffiliation), but the identity-locked exit option makes resistance costly.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic seat, the constraint is a Mountain (divine command, natural law of survival). From the secularized Jew seat, it is a Snare (coercive extraction of conformity). From the traditional practitioner seat, it is a Rope (voluntary coordination). The engine will compute these per-seat types from the structural data; the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinic authority is the structural beneficiary (d near 0.0): it collects interpretive authority, communal legitimacy, and material support from the constraint. Secularized Jews are the primary targets (d near 1.0): they pay the conformity cost, have identity-locked exit (leaving Judaism is existentially costly), and cannot access the coordination benefits without accepting the extraction. Traditional practitioners sit near symmetric (d ~0.5): they gain genuine coordination benefits and voluntarily pay the conformity cost. Reform movements are excluded (d undefined): they would be beneficiaries of a different constraint but are structurally barred from this one. Scholars are analytical observers (d = 0.5 by default).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (existential survival after catastrophe) is contested: traditionalists say it persists, secular scholars say it has shifted. The constraint persists with high extraction because the rabbinic authority's interpretive control depends on maintaining the original framing. The mandate has atrophied (survival is no longer existentially threatened in the same way) but the constraint has not been revised because the authority that could revise it benefits from its stasis. This is a classic mandatrophy pattern: the arrangement continues to solve a problem that has changed, while the beneficiaries of the arrangement control the revision process.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbolic_vs_practical_sufficiency,
    'Is the preservation of ritual form (symbolic experience) genuinely sufficient for collective survival, or does survival require the practical knowledge transmission claimed by the competence reading?',
    'Longitudinal comparative study of Jewish communities that maintained ritual form but lost practical knowledge (e.g., some diaspora communities) vs. those that maintained both. Measure assimilation rates, identity retention, and demographic continuity over 3-4 generations.',
    'If symbolic form alone is insufficient, the constraint''s coordination function is overstated and its extraction is less justified; the hybrid reading would be empirically vindicated. If sufficient, the symbol_survival reading''s claim gains empirical support (though extraction asymmetry remains).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(symbolic_vs_practical_sufficiency, empirical, 'Whether the coordination function claimed by this reading is empirically adequate for the survival outcome it promises.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.7) primarily structural (communal sanctions, halakhic enforcement) or internalized (guilt, identity fusion, belief that non-observance is betrayal)?',
    'Post-exit trajectory study: track secularized Jews who leave observant communities. If suppression persists (guilt, anxiety, identity conflict) after structural sanctions are removed, the internalized component is significant. Compare with those who never internalized the framework.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression after exit, making the constraint more snare-like. If primarily structural, the constraint''s extraction is more reversible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in an identity-locked interpersonal/collective constraint.').

omega_variable(
    kernel_reading_framing,
    'Does the symbol_survival_reading represent a genuine distinct constraint, or is it a strategic framing deployed by rabbinic authority to protect interpretive control?',
    'Discourse analysis of rabbinic responsa across the interval: when challenged by competence or hybrid readings, does the symbol_survival reading adapt (suggesting strategic deployment) or hold invariant (suggesting genuine doctrinal commitment)?',
    'If strategic, the constraint is a Snare disguised as a Mountain/Tangled Rope. If genuine doctrinal commitment, the Tangled Rope classification stands: real coordination function coexists with asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether the reading''s framing is a stable doctrinal position or a contingent legitimation strategy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__symbol_survival_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 80, 0.38).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 80, 0.72).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 100, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 60, 0.65).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 80, 0.68).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 100, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__symbol_survival_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_survival__symbol_survival_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival__competence_transmission_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival__hybrid_encoding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the catastrophe_memory_survival kernel. The kernel decomposes because the label 'ritual ensures survival' conflates structurally distinct claims: (1) symbolic boundary-maintenance (this reading, high ε, rabbinic beneficiary), (2) practical knowledge transmission (competence reading, lower ε, different beneficiary/victim structure), (3) dual-register operation (hybrid reading, intermediate ε). Each reading has its own ε, stakeholders, and classification. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_survival__symbol_survival_reading, institutional, 0.1).
constraint_indexing:directionality_override(catastrophe_memory_survival__symbol_survival_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
