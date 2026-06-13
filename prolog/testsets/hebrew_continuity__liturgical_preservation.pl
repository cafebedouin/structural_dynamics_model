% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__liturgical_preservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__liturgical_preservation, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: hebrew_continuity__liturgical_preservation
 *   human_readable: Hebrew Continuity Through Liturgical Preservation and Textual Transmission
 *   domain: sociolinguistics/commitment_systems/religious_authority
 *
 * SUMMARY:
 *   Hebrew persists as a living language through two competing mechanisms:
 *   (1) liturgical preservation by religious institutions through fixed
 *   textual recitation and canonical interpretation, and (2) native speaker
 *   revival and generative use, primarily in Israel and diaspora communities.
 *   This constraint story instantiates the LITURGICAL_PRESERVATION reading —
 *   the claim that Hebrew lives through institutional control of textual
 *   transmission, not through native speaker intuition. This reading coexists
 *   with the NATIVE_GENERATIVE reading (a sibling constraint in the same
 *   kernel family) which claims Hebrew lives through daily generative use by
 *   native speakers. The two readings occupy different institutional seats
 *   and compete for explanatory authority over the same language substrate.
 *   The liturgical-preservation reading frames religious authority as the
 *   keeper and carrier; the native-generative reading frames speakers as the
 *   bearers. Both are structurally true of contemporary Hebrew, but they
 *   attribute different causal primacy and different extraction mechanisms.
 *
 * KEY AGENTS:
 *   - religious_authority_structures: agenda_setter, controls textual canon and interpretive authority
 *   - textual_tradition_stewards: beneficiary, scholars and rabbinical commentators preserving fixed texts
 *   - secular_hebrew_speakers: payer (moderate power), experience diglossia between spoken innovation and enforced standard
 *   - generative_language_users: payer (powerless, identity_locked), native speakers whose intuitions conflict with fixed standard
 *   - secular_jewish_intellectuals: excluded (would advocate for living, evolving Hebrew)
 *   - linguistic_reform_movements: excluded (periodically challenge the liturgical mandate)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__liturgical_preservation, 0.68).
domain_priors:suppression_score(hebrew_continuity__liturgical_preservation, 0.72).
domain_priors:theater_ratio(hebrew_continuity__liturgical_preservation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, extractiveness, 0.68).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__liturgical_preservation, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__liturgical_preservation, "Hebrew Continuity Through Liturgical Preservation and Textual Transmission").
narrative_ontology:topic_domain(hebrew_continuity__liturgical_preservation, "sociolinguistics/commitment_systems/religious_authority").

domain_priors:requires_active_enforcement(hebrew_continuity__liturgical_preservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__liturgical_preservation, '75cc8c25-64ae-4efa-ba90-185e8f40fb40').
narrative_ontology:cs_kernel_codification('75cc8c25-64ae-4efa-ba90-185e8f40fb40', fixed_text).
narrative_ontology:cs_authority_grounding('75cc8c25-64ae-4efa-ba90-185e8f40fb40', extraction).
narrative_ontology:cs_interpretation_layer_present('75cc8c25-64ae-4efa-ba90-185e8f40fb40').
narrative_ontology:cs_reading_relation('75cc8c25-64ae-4efa-ba90-185e8f40fb40', hebrew_continuity__native_generative, coexists_with).
narrative_ontology:cs_reading_relation('75cc8c25-64ae-4efa-ba90-185e8f40fb40', hebrew_continuity__bridge_pidginized, influences).
narrative_ontology:cs_axiom('75cc8c25-64ae-4efa-ba90-185e8f40fb40', foundational, hebrew_lives_through_unchanging_text).
narrative_ontology:cs_axiom_status(hebrew_lives_through_unchanging_text, holdable).
narrative_ontology:cs_axiom_grounding('75cc8c25-64ae-4efa-ba90-185e8f40fb40', hebrew_lives_through_unchanging_text, deontological).
narrative_ontology:cs_axiom('75cc8c25-64ae-4efa-ba90-185e8f40fb40', foundational, religious_authority_preserves_meaning_fidelity).
narrative_ontology:cs_axiom_status(religious_authority_preserves_meaning_fidelity, overridden).
narrative_ontology:cs_axiom_grounding('75cc8c25-64ae-4efa-ba90-185e8f40fb40', religious_authority_preserves_meaning_fidelity, empirically_contingent).
narrative_ontology:cs_reference_frame('75cc8c25-64ae-4efa-ba90-185e8f40fb40', textual_fixity_as_sacred_preservation).
narrative_ontology:cs_drift_state('75cc8c25-64ae-4efa-ba90-185e8f40fb40', contemporary_israel_and_diaspora, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('75cc8c25-64ae-4efa-ba90-185e8f40fb40', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__liturgical_preservation, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, religious_authority_structures).
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, textual_tradition_stewards).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, secular_hebrew_speakers).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, generative_language_users).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__liturgical_preservation, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(hebrew_continuity__liturgical_preservation, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__liturgical_preservation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_continuity__liturgical_preservation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_continuity__liturgical_preservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.68 at interval end) because the constraint enforces a linguistic standard decoupled from speaker intuition: speakers must maintain the written standard even as their spoken language diverges, and they are penalized for deviating. Suppression is similarly high (0.72-0.75) because the enforcement machinery is substantial: schools police the standard, media institutions reinforce it, formal language councils are influenced by religious authorities, and speakers internalize shame around non-standard usage. Theater ratio is high and rising (0.42 → 0.67), signaling mandatrophy: as the founding problem (language transmission) has been solved by native revival, the constraint's primary function is increasingly performative — maintaining institutional authority and textual interpretive monopoly, not solving a coordination problem. The measurement series show rising extractiveness and suppression requirement over the 50-year interval, with theater ratio rising faster, consistent with a constraint whose original justification has atrophied while its enforcement infrastructure hardened. All metrics are authored on a single shared time grid (every metric at every time point) to enable coherent lifecycle analysis.
 *
 * PERSPECTIVAL GAP:
 *   Religious authorities and textual stewards (beneficiary seats) experience the constraint as necessary coordination and legitimate cultural preservation. Secular speakers and generative users (payer seats) experience it as enforced conformity to an outdated standard that does not match their linguistic reality. The analytical observer seat sees the divergence: the constraint solved a real problem (transmission across exile) but its continuation serves institutional control, not coordination. The engine should compute these as fundamentally different types from different seats — the beneficiary seat may compute as rope (genuine coordination), while the payer seats compute as snare (enforced conformity with no real alternative). This perspectival gap is precisely the diagnostic signature of a tangled_rope: same structure, opposite experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious authorities derive d ≈ 0.1–0.2 (low extraction, high benefit from the arrangement they control). Secular speakers and generative users derive d ≈ 0.75–0.85 (high extraction, conformity costs, constrained exit because Hebrew identity is bound to national/ethnic identity in Israel, and identity_locked elsewhere in diaspora). The beneficiary seat has arbitrage-grade exit (they control the arrangement and can modify it unilaterally). The payer seats have constrained or identity-locked exit: abandoning Hebrew means abandoning Jewish identity or national belonging; staying means accepting the linguistic standard. Directionality overrides are not needed here — the structural derivation (beneficiary/victim + exit → d) captures the true asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows clear mandatrophy: the founding problem (how Hebrew survives across exile) was solved by native speaker revival in the 20th century. The constraint should have dissolved or transformed into coordination among native speakers. Instead, it persists and intensifies (theater_ratio rising, suppression_requirement rising) because the institutional actors benefit from maintaining textual control. The constraint no longer exists to solve the founding problem; it exists to preserve the institutional authority structures that claim to be its stewards. The rising theater_ratio (0.42 → 0.67) is the smoking gun: enforcement activity is increasingly devoted to defending the standard against generative innovation, not to solving a coordination problem. The base_extractiveness rise (0.45 → 0.75) shows the constraint transforming from a coordination solution into an extraction mechanism. This is mandatrophy in real time — a constraint whose mandate has outlived its function and whose persistence serves power, not coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liturgical_vs_generative_kernel,
    'Does Hebrew persist THROUGH liturgical preservation, or does liturgical preservation RIDE ON native speaker revival that occurred independently?',
    'Counterfactual historical analysis: would Hebrew have survived to 1880 (pre-native-revival) without liturgical transmission? Did native-speaker revival require religious authority permission or did it reorganize Hebrew outside religious control? Did religious authorities initially resist or welcome the shift to generative use?',
    'If liturgical preservation was the primary carrier, the constraint is structurally foundational (necessary for survival). If revival occurred despite religious authority resistance, the constraint becomes a secondary control mechanism protecting institutional authority AFTER the language had already been secured — classification shifts from rope to snare or tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_vs_generative_kernel, conceptual, 'Whether liturgical preservation enabled language survival or merely rode upon it.').

omega_variable(
    sacred_vs_instrumental_closure,
    'Is the closure of Hebrew to generative innovation a necessary feature of preserving sacredness, or a means by which religious institutions maintain interpretive monopoly?',
    'Comparative analysis of other sacred languages (Latin, Classical Arabic, Sanskrit): do all require linguistic fixity, or do some permit innovation within sacred contexts? Do secular speakers using Hebrew report that the constraint strengthens or weakens their connection to tradition?',
    'If fixity is intrinsic to sacredness, the constraint is a genuine coordination mechanism for religious continuity. If fixity is instrumental (a tool for maintaining institutional control), the extraction component becomes separable and the constraint reclassifies toward pure snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sacred_vs_instrumental_closure, preference, 'Whether linguistic fixity is necessary for sacred meaning or a choice that serves institutional power.').

omega_variable(
    suppression_internalization,
    'Is the measured suppression (0.72) structural (external institutional barriers to generative use) or internalized (Hebrew speakers have internalized the shame/illegitimacy of deviation)?',
    'Post-constraint scenarios: if Hebrew speakers in secular contexts stop policing their own generative innovation, does the suppression persist or dissolve? Do speakers'' own resistance to innovation come from internalized authority or external institutional enforcement?',
    'If internalized, the constraint carries its suppression with it even after institutional enforcement relaxes — speakers would still perceive generative use as illegitimate. If structural, enforcement decay would release the suppression quickly. This determines the true cost of exit and the post-exit trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Structural vs. internalized suppression of generative language use.').

omega_variable(
    kernel_reading_divergence,
    'Can Hebrew be understood as ONLY a liturgically-preserved language, or must any complete account acknowledge the native-speaker revival as a sibling reading that competes for the same substrate?',
    'This reading (liturgical_preservation) claims ε as the constraint that carries Hebrew through institutional recitation. The native_generative reading claims ε as the constraint of native speaker intuition and daily use. Both cannot be the full constraint if they apply to the same language simultaneously — the question is whether this reading''s ε-value is robust when native speakers actively exist and innovate.',
    'If native speakers are already present and generative, this reading''s extraction (0.68) may be artificially low — the constraint is NOT carrying Hebrew alone; it is fighting native speaker innovation to suppress it. The ''victim'' set would need to include more of the generative speaker base. The constraint may reclassify to higher extractiveness or shift toward pure snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Whether the liturgical-preservation reading is complete or partially obscured by the coexistence of native-speaker generativity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__liturgical_preservation, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_continuity__liturgical_preservation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(hebr_tr_t10, hebrew_continuity__liturgical_preservation, theater_ratio, 10, 0.46).
narrative_ontology:measurement(hebr_tr_t20, hebrew_continuity__liturgical_preservation, theater_ratio, 20, 0.52).
narrative_ontology:measurement(hebr_tr_t30, hebrew_continuity__liturgical_preservation, theater_ratio, 30, 0.58).
narrative_ontology:measurement(hebr_tr_t40, hebrew_continuity__liturgical_preservation, theater_ratio, 40, 0.63).
narrative_ontology:measurement(hebr_tr_t50, hebrew_continuity__liturgical_preservation, theater_ratio, 50, 0.67).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_continuity__liturgical_preservation, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(hebr_be_t10, hebrew_continuity__liturgical_preservation, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(hebr_be_t20, hebrew_continuity__liturgical_preservation, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(hebr_be_t30, hebrew_continuity__liturgical_preservation, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(hebr_be_t40, hebrew_continuity__liturgical_preservation, base_extractiveness, 40, 0.72).
narrative_ontology:measurement(hebr_be_t50, hebrew_continuity__liturgical_preservation, base_extractiveness, 50, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_continuity__liturgical_preservation, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(hebr_su_t10, hebrew_continuity__liturgical_preservation, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(hebr_su_t20, hebrew_continuity__liturgical_preservation, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(hebr_su_t30, hebrew_continuity__liturgical_preservation, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(hebr_su_t40, hebrew_continuity__liturgical_preservation, suppression_requirement, 40, 0.8).
narrative_ontology:measurement(hebr_su_t50, hebrew_continuity__liturgical_preservation, suppression_requirement, 50, 0.83).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__liturgical_preservation, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_continuity__liturgical_preservation, 0.12).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, hebrew_continuity__native_generative).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, hebrew_continuity__bridge_pidginized).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, jewish_diaspora_cohesion__linguistic_markers).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, yiddish_language_subordination).

% DUAL FORMULATION NOTE:
% Hebrew continuity decomposes into three constraint stories instantiating different readings of the same kernel: liturgical_preservation (this story, institutional preservation through textual control), native_generative (language lives through native speaker intuition), and bridge_pidginized (language lives as diaspora contact medium). Each has distinct ε, distinct beneficiary/victim structure, and distinct type. The three stories are linked via network.affects_constraints as a constraint family. ε-invariance principle applied: measuring Hebrew's persistence through liturgical mechanics yields different structural analysis than measuring through native speaker birth and childhood acquisition. Rather than force both mechanisms into one constraint (which would require inventing a measurement parameter), the family decomposes them. Each story models a distinct mechanism and a distinct extraction profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_continuity__liturgical_preservation, powerless, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
