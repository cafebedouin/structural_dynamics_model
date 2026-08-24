% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_linguistic_life__liturgical_preservation_reading, []).

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
 *   constraint_id: hebrew_linguistic_life__liturgical_preservation_reading
 *   human_readable: Liturgical Preservation Definition of Hebrew Vitality
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   The liturgical preservation reading asserts that Hebrew never died
 *   because its sacred texts were continuously recited, studied, and
 *   transmitted in an unbroken chain from antiquity to the present. This
 *   reading functions as a constraint on what counts as a 'living language':
 *   it delegitimizes the Zionist revival (Ben-Yehuda's project) as
 *   desecration rather than resurrection, and it defines the victim of that
 *   desecration as the sacred tradition itself. The constraint is actively
 *   enforced by religious authorities who control Hebrew education, textual
 *   authorization, and communal norms in traditional sectors. It coordinates
 *   community identity around a frozen textual canon while extracting
 *   legitimacy from any secular use. The claimed type is tangled_rope because
 *   the constraint genuinely coordinates diaspora survival (coordination
 *   function) but also asymmetrically extracts authority for religious
 *   gatekeepers and suppresses vernacular vitality (extraction function).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__liturgical_preservation_reading, 0.72).
domain_priors:suppression_score(hebrew_linguistic_life__liturgical_preservation_reading, 0.78).
domain_priors:theater_ratio(hebrew_linguistic_life__liturgical_preservation_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__liturgical_preservation_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__liturgical_preservation_reading, "Liturgical Preservation Definition of Hebrew Vitality").
narrative_ontology:topic_domain(hebrew_linguistic_life__liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__liturgical_preservation_reading, '2b682235-4287-45ab-91b5-525ec9c1e382').
narrative_ontology:cs_kernel_codification('2b682235-4287-45ab-91b5-525ec9c1e382', fixed_text).
narrative_ontology:cs_authority_grounding('2b682235-4287-45ab-91b5-525ec9c1e382', lineage).
narrative_ontology:cs_interpretation_layer_present('2b682235-4287-45ab-91b5-525ec9c1e382').
narrative_ontology:cs_reading_relation('2b682235-4287-45ab-91b5-525ec9c1e382', hebrew_linguistic_life__native_generational_reading, forecloses).
narrative_ontology:cs_reading_relation('2b682235-4287-45ab-91b5-525ec9c1e382', hebrew_linguistic_life__marketplace_pidgin_reading, forecloses).
narrative_ontology:cs_axiom('2b682235-4287-45ab-91b5-525ec9c1e382', foundational, sacred_transmission_suffices_for_language_life).
narrative_ontology:cs_axiom_status(sacred_transmission_suffices_for_language_life, holdable).
narrative_ontology:cs_axiom_grounding('2b682235-4287-45ab-91b5-525ec9c1e382', sacred_transmission_suffices_for_language_life, theological).
narrative_ontology:cs_axiom('2b682235-4287-45ab-91b5-525ec9c1e382', foundational, secular_use_is_desecration).
narrative_ontology:cs_axiom_status(secular_use_is_desecration, holdable).
narrative_ontology:cs_axiom_grounding('2b682235-4287-45ab-91b5-525ec9c1e382', secular_use_is_desecration, theological).
narrative_ontology:cs_reference_frame('2b682235-4287-45ab-91b5-525ec9c1e382', liturgical_continuity_framework).
narrative_ontology:cs_drift_state('2b682235-4287-45ab-91b5-525ec9c1e382', modern_secular_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2b682235-4287-45ab-91b5-525ec9c1e382', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, religious_authorities).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, sacred_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, zionist_revivalists).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__liturgical_preservation_reading, liturgical_preservation_defines_language_life).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__liturgical_preservation_reading, secular_revival_is_desecration).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rabbinic courts, yeshiva leadership, and communal bodies that define and enforce the liturgical standard. They control Hebrew education in traditional settings, authorize texts, and adjudicate what counts as legitimate transmission. They benefit from the constraint because it secures their interpretive monopoly and communal authority. Their exit is constrained by institutional role and theological commitment.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, religious_authorities, agenda_setter,
    institutional, generational, constrained, global).

% The body of liturgical texts, oral traditions, and ritual practices that constitute the chain of transmission. The constraint treats this tradition as a static object to be guarded rather than a living heritage that can evolve. It bears the cost of fossilization: the tradition cannot adapt to new circumstances without being labeled desecration, and its vitality is reduced to mechanical recitation.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, sacred_tradition, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(hebrew_linguistic_life__liturgical_preservation_reading, sacred_tradition).

% Speakers of Modern Hebrew who use the language for daily life, science, arts, and commerce. They are structurally excluded from the liturgical definition of vitality because their use is deemed profane. They would argue that a language lives through vernacular creativity, but their voice is absent from the religious framework that sets the agenda.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, secular_hebrew_users, excluded,
    moderate, biographical, constrained, national).

% The historical Ben-Yehuda circle and subsequent Hebrew revival movements. They bear the cost of the constraint's delegitimization: their project is framed as desecration rather than resurrection, and they must fight for recognition against the religious monopoly on authenticity. They have mobile exit (they built a new linguistic reality) but remain targets of the constraint's moral condemnation.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, zionist_revivalists, payer,
    organized, generational, mobile, national).

% Academic linguists, sociolinguists, and historians who study Hebrew's trajectory. They analyze the constraint from outside, documenting how the liturgical definition operates as a boundary-maintenance mechanism. They neither collect nor pay; they provide the analytical seat.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, linguistic_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains an unbroken chain of textual transmission and ritual recitation across diaspora and persecution, providing a stable identity anchor for Jewish communities without a shared vernacular.
% TRANSFER_FUNCTION: Moves interpretive authority and communal legitimacy from the vernacular sphere to the religious establishment; moves the burden of proof onto any non-liturgical use of Hebrew, which must justify itself against the charge of desecration.
% ABSENT_VOICES: Secular Hebrew speakers, Yiddishists who opposed Hebrew revival, and non-religious Jewish communities who see the language as a national-cultural asset rather than a sacred trust. They are absent because the constraint's framing defines them out of the conversation — their use of Hebrew is by definition not 'life'.
% DISAPPEARANCE_RATIONALE: If the liturgical definition vanished, the religious monopoly on Hebrew authenticity would dissolve. Modern Hebrew would be universally recognized as a legitimate continuation rather than a desecration. Religious authorities would lose their unique claim to guardianship. The sacred tradition itself would face pressure to articulate its value without the shield of 'only we keep the language alive'.
% FOUNDING_PROBLEM: After the Temple's destruction and the loss of sovereignty, Jewish communities needed a portable, non-territorial definition of collective continuity. The liturgical chain provided a measurable, enforceable standard that could survive exile.
% FOUNDING_PROBLEM_CORROBORATION: Traditional historians (e.g., Salo Baron) attest the liturgical chain was a survival strategy. Modern sociolinguists (e.g., Joshua Fishman) argue the founding problem — portable identity maintenance — is substantially solved by modern institutions (state, education, media) and the constraint now serves a different function. No corroboration from outside the religious establishment supports the claim that the founding problem remains live in its original form.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__liturgical_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__liturgical_preservation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__liturgical_preservation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_linguistic_life__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__liturgical_preservation_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_linguistic_life__liturgical_preservation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_linguistic_life__liturgical_preservation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_linguistic_life__liturgical_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) reflects the constraint's capture of the 'language life' definition for sectarian benefit. Suppression (0.78) is high because the constraint's persistence depends on actively excluding competing definitions (native generational, marketplace pidgin) and stigmatizing secular speakers. Theater ratio (0.38) indicates that a significant portion of the enforcement apparatus (textual policing, educational control) serves the extraction function rather than the coordination function. Accessibility collapse (0.82) is high because once the liturgical definition is accepted, alternative vitality metrics become epistemically inaccessible within the framework. Resistance (0.55) is moderate: secular revivalists and linguists contest the definition, but the constraint's institutional base remains robust.
 *
 * PERSPECTIVAL GAP:
 *   From the religious authority seat, the constraint is a rope: it solves the genuine coordination problem of preserving identity without territory. From the sacred tradition seat (if it could speak), it is a snare: it freezes the tradition into a museum piece. From the secular user seat, it is a snare: it denies their linguistic reality. The engine computes these divergences from the structural data; the authored claim (tangled_rope) reflects the generator's assessment that both coordination and extraction are structurally present.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious authorities are the structural beneficiaries (d ≈ 0.15): they collect interpretive rents and communal control. The sacred tradition is the victim (d ≈ 0.85): it bears the cost of fossilization and instrumentalization. Secular Hebrew users are excluded (d ≈ 0.7): they are not directly extracted from but are structurally silenced. Zionist revivalists are payers (d ≈ 0.8): they bear the moral and political cost of delegitimization. Linguistic scholars are observers (d = 0.5): they analyze without stake.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (portable identity maintenance without sovereignty) was live in 70 CE and remained live through most of the diaspora. By the late 19th century, the emergence of modern nationalism, print capitalism, and the Zionist movement created alternative solutions. The constraint persists because the religious establishment extracts authority from it, not because the founding problem requires it. The mandatrophy is unresolved: the constraint's mandate has outlived its function, but the extraction machinery maintains it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    victim_nature_ambiguity,
    'Is the sacred tradition a legitimate victim (an entity that can bear costs) or a rhetorical device that masks extraction from secular users?',
    'Analyze whether the tradition''s ''fossilization'' produces measurable harm to the tradition''s own stated goals (transmission fidelity, communal cohesion) or whether the harm falls entirely on excluded human agents.',
    'If the tradition is not a genuine victim, the constraint''s victim set shifts to secular_hebrew_users and zionist_revivalists, altering the extraction profile and potentially reclassifying the constraint as snare rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_nature_ambiguity, conceptual, 'Whether the declared victim (sacred tradition) is a structural victim or a cover for extraction from human agents.').

omega_variable(
    natural_law_vs_constructed_definition,
    'Does the liturgical definition reflect a natural law of language vitality, or is it a constructed boundary-maintenance tool?',
    'Compare the definition''s empirical adequacy: does it predict language survival better than competing definitions? Historical test: languages with liturgical transmission but no vernacular use (e.g., Classical Syriac, Coptic) — are they ''alive'' by any independent metric?',
    'If natural law, the constraint is a mountain (or false summit mountain given beneficiaries). If constructed, it is a tangled_rope or snare. The presence of beneficiaries on a mountain claim already triggers FSM; this omega documents the core ambiguity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_definition, empirical, 'Whether the constraint''s claimed naturalness is empirically grounded or a constructed cover for extraction.').

omega_variable(
    foreclosure_symmetry,
    'Does this reading genuinely foreclose the sibling readings, or do they coexist in practice as complementary vitality metrics?',
    'Examine actual discourse: do religious authorities explicitly deny that Modern Hebrew is a living language, or do they accept it as a separate category? Do secular linguists accept the liturgical definition as one valid metric among others?',
    'If foreclosure is only rhetorical (authorities accept Modern Hebrew''s vitality but deny its legitimacy), the reading_relations should be ''coexists_with'' or ''influences'' rather than ''forecloses''. This changes the kernel''s structural topology.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foreclosure_symmetry, empirical, 'Whether the logical foreclosure declared in reading_relations matches the empirical discourse topology.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__liturgical_preservation_reading, 70, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t70, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 70, 0.15).
narrative_ontology:measurement(hebr_tr_t500, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 500, 0.22).
narrative_ontology:measurement(hebr_tr_t1000, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 1000, 0.28).
narrative_ontology:measurement(hebr_tr_t1500, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 1500, 0.33).
narrative_ontology:measurement(hebr_tr_t1800, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 1800, 0.36).
narrative_ontology:measurement(hebr_tr_t1900, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 1900, 0.38).
narrative_ontology:measurement(hebr_tr_t2024, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(hebr_be_t70, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 70, 0.35).
narrative_ontology:measurement(hebr_be_t500, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 500, 0.42).
narrative_ontology:measurement(hebr_be_t1000, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 1000, 0.55).
narrative_ontology:measurement(hebr_be_t1500, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 1500, 0.62).
narrative_ontology:measurement(hebr_be_t1800, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 1800, 0.68).
narrative_ontology:measurement(hebr_be_t1900, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 1900, 0.72).
narrative_ontology:measurement(hebr_be_t2024, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t70, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 70, 0.4).
narrative_ontology:measurement(hebr_su_t500, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 500, 0.5).
narrative_ontology:measurement(hebr_su_t1000, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 1000, 0.6).
narrative_ontology:measurement(hebr_su_t1500, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 1500, 0.7).
narrative_ontology:measurement(hebr_su_t1800, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 1800, 0.75).
narrative_ontology:measurement(hebr_su_t1900, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 1900, 0.78).
narrative_ontology:measurement(hebr_su_t2024, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__liturgical_preservation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_linguistic_life__liturgical_preservation_reading, 0.08).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life__native_generational_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life__marketplace_pidgin_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the hebrew_linguistic_life kernel. The three readings decompose the colloquial label 'Hebrew is a living language' into structurally distinct constraints with different ε, beneficiaries, and victims. This reading (liturgical_preservation) has ε=0.72, beneficiaries=religious_authorities, victims=sacred_tradition. The native_generational_reading has ε≈0.3, beneficiaries=secular_nationalists, victims=diaspora_communities. The marketplace_pidgin_reading has ε≈0.2, beneficiaries=traders/communal_intermediaries, victims=purist_language_planners. They are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_linguistic_life__liturgical_preservation_reading, institutional, 0.15).
constraint_indexing:directionality_override(hebrew_linguistic_life__liturgical_preservation_reading, powerless, 0.85).
constraint_indexing:directionality_override(hebrew_linguistic_life__liturgical_preservation_reading, moderate, 0.7).
constraint_indexing:directionality_override(hebrew_linguistic_life__liturgical_preservation_reading, organized, 0.8).
constraint_indexing:directionality_override(hebrew_linguistic_life__liturgical_preservation_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
