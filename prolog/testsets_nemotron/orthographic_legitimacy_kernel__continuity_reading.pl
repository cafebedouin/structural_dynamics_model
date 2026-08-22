% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_legitimacy_kernel__continuity_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: orthographic_legitimacy_kernel__continuity_reading
 *   human_readable: Orthographic Continuity: Script as Guardian of Historical-Literary Access
 *   domain: political_linguistics/state_formation/commitment_systems
 *
 * SUMMARY:
 *   The continuity reading holds that orthographic legitimacy derives from
 *   preserving access to historical, religious, and literary tradition —
 *   specifically the vast corpus of texts written in Arabic script before the
 *   1928 Turkish alphabet reform. The constraint is the structural fact of
 *   script incompatibility: post-reform generations cannot read pre-1928
 *   texts without specialized training. This is a mountain-like constraint
 *   because script incompatibility is a physical-linguistic fact, not a
 *   policy choice. The victims are post-reform Turkish generations severed
 *   from their literary, religious, and historical inheritance. The only
 *   identifiable beneficiary is the institutional complex around Arabic
 *   script (religious education, calligraphic traditions, manuscript
 *   preservation), which persists but does not extract rents from the
 *   constraint's operation. The reading emphasizes irreversible loss rather
 *   than active extraction.
 *
 * KEY AGENTS:
 *   - post_reform_generations: Primary victim (powerless/identity_locked) — severed from pre-1928 textual heritage
 *   - arabic_script_institutions: Residual beneficiary (organized/biographical) — religious education and manuscript preservation complexes that maintain Arabic script literacy
 *   - turkish_state: Agenda setter of the reform (institutional/generational) — enacted the alphabet change but not the constraint itself
 *   - analytical_observer: Observer seat (analytical/universal) — sees the structural fact of script incompatibility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__continuity_reading, 0.15).
domain_priors:suppression_score(orthographic_legitimacy_kernel__continuity_reading, 0.35).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__continuity_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__continuity_reading, mountain).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__continuity_reading, "Orthographic Continuity: Script as Guardian of Historical-Literary Access").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__continuity_reading, "political_linguistics/state_formation/commitment_systems").

domain_priors:emerges_naturally(orthographic_legitimacy_kernel__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__continuity_reading, '8fb0eaaa-7a97-403f-ba5d-e9ffce123262').
narrative_ontology:cs_kernel_codification('8fb0eaaa-7a97-403f-ba5d-e9ffce123262', fixed_text).
narrative_ontology:cs_authority_grounding('8fb0eaaa-7a97-403f-ba5d-e9ffce123262', lineage).
narrative_ontology:cs_interpretation_layer_present('8fb0eaaa-7a97-403f-ba5d-e9ffce123262').
narrative_ontology:cs_reading_relation('8fb0eaaa-7a97-403f-ba5d-e9ffce123262', orthographic_legitimacy_kernel__instrumentalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('8fb0eaaa-7a97-403f-ba5d-e9ffce123262', orthographic_legitimacy_kernel__modernist_reading, coexists_with).
narrative_ontology:cs_axiom('8fb0eaaa-7a97-403f-ba5d-e9ffce123262', foundational, historical_text_access_requires_orthographic_continuity).
narrative_ontology:cs_axiom_status(historical_text_access_requires_orthographic_continuity, holdable).
narrative_ontology:cs_axiom_grounding('8fb0eaaa-7a97-403f-ba5d-e9ffce123262', historical_text_access_requires_orthographic_continuity, empirically_contingent).
narrative_ontology:cs_axiom('8fb0eaaa-7a97-403f-ba5d-e9ffce123262', secondary, script_reform_severance_is_irreversible_loss).
narrative_ontology:cs_axiom_status(script_reform_severance_is_irreversible_loss, holdable).
narrative_ontology:cs_axiom_grounding('8fb0eaaa-7a97-403f-ba5d-e9ffce123262', script_reform_severance_is_irreversible_loss, deontological).
narrative_ontology:cs_reference_frame('8fb0eaaa-7a97-403f-ba5d-e9ffce123262', ottoman_script_continuity_framework).
narrative_ontology:cs_drift_state('8fb0eaaa-7a97-403f-ba5d-e9ffce123262', post_reform_century, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('8fb0eaaa-7a97-403f-ba5d-e9ffce123262', '2026-08-05T14:30:00Z').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__continuity_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__continuity_reading, arabic_script_institutions).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__continuity_reading, post_reform_generations).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__continuity_reading, script_continuity_preserves_cultural_memory).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__continuity_reading, historical_text_access_requires_orthographic_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Turkish citizens born after the 1928 alphabet reform who cannot read the vast corpus of Ottoman Turkish texts (literature, religious commentary, legal records, family documents) without specialized philological training. Their linguistic identity was constituted by the Latin-script reform; exit from this constraint would require acquiring a skill (Arabic script literacy) that the education system does not provide and that marks them as outside the national linguistic consensus. The cost is diffuse but total: loss of direct access to their own history, religion, and literature.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, post_reform_generations, payer,
    powerless, biographical, identity_locked, national).

% Religious education establishments (medreses, ilahiyat faculties), manuscript libraries (Süleymaniye, Millet), calligraphic guilds, and Sufi orders that maintain Arabic script literacy. They retain the specialized capability to read and transmit pre-1928 texts. They do not control the constraint (the script barrier exists regardless of them) but their skill becomes scarce and therefore institutionally valuable. They can teach the script to interested individuals; their exit options are mobile because their expertise is portable across contexts.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, arabic_script_institutions, beneficiary,
    organized, generational, mobile, national).

% The early Republican state that enacted the 1928 alphabet reform. It set the agenda for script change but did not create the constraint of script incompatibility — that is a structural consequence. The state could mitigate the constraint (fund transliteration, parallel-script education) but has largely chosen not to. Its exit options are arbitrage-grade: it controls education policy and could change the constraint's severity at any time.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, turkish_state, agenda_setter,
    institutional, generational, arbitrage, national).

% The structural analyst who sees the constraint as a fact of script incompatibility — a physical-linguistic barrier created by a historical policy choice. Bears no costs, collects no benefits, evaluates the constraint's operation from outside the national linguistic consensus.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_legitimacy_kernel__continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(orthographic_legitimacy_kernel__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the capability to access historical, religious, and literary tradition written in Arabic script by maintaining specialized institutional knowledge (Arabic script literacy in religious and scholarly institutions) that would otherwise be lost entirely.
% TRANSFER_FUNCTION: Moves the capacity for direct textual access from the general population (pre-reform: widespread Arabic script literacy) to a specialized institutional stratum (post-reform: religious/scholarly institutions), creating a structural dependency where the population must rely on mediated translations/interpretations.
% ABSENT_VOICES: Pre-reform generations who experienced the transition (now deceased) — they would testify to the lived experience of the severance. Kurdish, Armenian, Greek, and other minority communities whose textual traditions were also mediated through Arabic script — their specific losses are subsumed under the national narrative. Diaspora communities that maintained Arabic script literacy outside Turkey — their continuity was not severed but they are excluded from the national conversation about the reform's consequences.
% DISAPPEARANCE_RATIONALE: If the script incompatibility constraint vanished overnight (e.g., through universal Arabic script literacy or perfect AI transliteration), the relationship between Turkish society and its pre-1928 textual heritage would fundamentally reorganize: religious authority would decentralize, historical narratives would be directly contested, family and property records would become directly accessible, and the state's monopoly on interpreting the Ottoman past would dissolve. The world rearranges because the constraint currently structures who can speak authoritatively about the past.
% FOUNDING_PROBLEM: The Ottoman Turkish script (Arabic script adapted for Turkish) was structurally inadequate for Turkish phonology: it represented vowels poorly, required complex orthographic rules, and was difficult to learn. This created a genuine coordination problem — low literacy rates, administrative inefficiency, and a script that could not serve a modern mass-education system.
% FOUNDING_PROBLEM_CORROBORATION: Linguistic consensus: Ottoman Turkish script's phonological inadequacy for Turkish is attested by Turkologists outside the Turkish nationalist tradition (e.g., Lars Johanson, Éva Ágnes Csató). The problem is dead — modern Turkish in Latin script solves the phonological representation problem completely. The continuity reading's corroboration comes from the fact that even the reform's architects acknowledged the script's technical inadequacy; the dispute is about whether total replacement was the only solution.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__continuity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__continuity_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__continuity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, ExtMetricName, E),
    domain_priors:suppression_score(orthographic_legitimacy_kernel__continuity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(orthographic_legitimacy_kernel__continuity_reading),
    narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(orthographic_legitimacy_kernel__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the constraint is a structural fact of script incompatibility — no party collects rents from the inability to read old texts. Suppression is modest (0.35) because the barrier is structural (learning a new script) rather than coercive enforcement. Theater ratio is low (0.12) because there is minimal performative maintenance of the barrier. Accessibility collapse is extremely high (0.92) because the script change created a near-total barrier: pre-1928 texts are effectively inaccessible without specialized philological training. Resistance is near-zero (0.08) because the constraint is not actively enforced — it is a structural consequence of a past policy that cannot be reversed. The claimed_type is mountain because the constraint's core (script incompatibility) is a natural/physical fact that would persist regardless of human enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The victim seat (post-reform generations) experiences this as a mountain of loss — an irreversible structural severance from heritage. The beneficiary seat (Arabic script institutions) experiences it as a rope of preservation — their specialized skill maintains access. The engine should compute mountain for the victim seat (high accessibility_collapse, near-zero resistance) and potentially rope for the beneficiary seat (genuine coordination function of preserving texts). This seat divergence is the measurement: the same structural fact is mountain from below, coordination from above.
 *
 * DIRECTIONALITY LOGIC:
 *   Post-reform generations are full targets (d ≈ 1.0) — they bear the full cost of the accessibility barrier with no exit (identity_locked: their linguistic identity was constituted by the reform). Arabic script institutions are beneficiaries (d ≈ 0.0) — they retain the specialized skill to access the tradition, but they do not extract from the constraint; they merely preserve a capability. The Turkish state is the agenda_setter of the reform but not of the constraint itself — the constraint is the structural aftermath, not the policy. The analytical observer sees the full structure without bearing costs or collecting benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Ottoman script's inadequacy for Turkish phonology) was real but the solution (total script replacement) created a new constraint — the accessibility collapse. The constraint is not mandatrophic because no institution maintains it for extractive purposes; it persists by structural inertia. The continuity reading correctly identifies that the constraint's persistence is not due to active maintenance but to the irreversibility of the historical break.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_severance,
    'Is the script incompatibility a genuine natural/physical fact (mountain) or a constructed severance that could have been mitigated (e.g., by parallel-script education, transliteration programs)?',
    'Counterfactual analysis: did the reform architecture include any mitigation for textual continuity? Historical records of early republican education policy; comparison with other script reforms (Mongolian, Kazakh, Azerbaijani) that implemented transitional bilingual periods.',
    'If mitigable, the high accessibility_collapse is partly constructed — the constraint would reclassify toward tangled_rope (coordination of modernization + extraction of heritage access). If genuinely irreducible, mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_severance, conceptual, 'Whether the accessibility collapse is a natural law of script change or a policy choice amplified by lack of mitigation').

omega_variable(
    beneficiary_extraction_ambiguity,
    'Do Arabic script institutions genuinely benefit from the constraint, or do they merely bear the cost of preserving access that the broader society lost?',
    'Institutional analysis: do religious foundations, manuscript libraries, and calligraphic guilds receive state funding, prestige, or monopolistic control over textual interpretation because of the script barrier? Or are they under-resourced custodians of a capability the state abandoned?',
    'If they extract rents (control over religious interpretation, state subsidies for ''preserving heritage''), the constraint has a beneficiary extraction component. If they are net-cost bearers, the beneficiary declaration is a false summit trigger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_extraction_ambiguity, empirical, 'Whether the declared beneficiary (arabic_script_institutions) is a genuine rent-collector or a net-cost custodian').

omega_variable(
    committer_frame_kernel_relations,
    'How does the continuity reading structurally relate to the instrumentalist and modernist readings of the orthographic_legitimacy_kernel?',
    'Structural comparison of the three readings'' axioms and drift states. The continuity reading''s foundational axiom (historical_text_access_requires_orthographic_continuity) coexists with the instrumentalist''s axiom (literacy_maximization_justifies_script_choice) and the modernist''s axiom (western_alignment_legitimizes_rupture) — different parties hold them simultaneously; none logically forecloses the others within a single framework. But the continuity reading influences the modernist reading by maintaining a living counter-narrative that contests the modernist''s rupture narrative.',
    'Documents the committer-frame structure for the kernel family. Enables cross-reading contamination analysis: if the modernist reading''s authority erodes, the continuity reading''s axioms gain legitimacy pressure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_kernel_relations, conceptual, 'Structural relations between this reading and its sibling readings in the orthographic_legitimacy_kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__continuity_reading, 1928, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orthographic_legitimacy_kernel__continuity_reading_tr_t1928, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 1928, 0.05).
narrative_ontology:measurement(orthographic_legitimacy_kernel__continuity_reading_tr_t1950, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 1950, 0.07).
narrative_ontology:measurement(orthographic_legitimacy_kernel__continuity_reading_tr_t1975, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 1975, 0.09).
narrative_ontology:measurement(orthographic_legitimacy_kernel__continuity_reading_tr_t2000, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 2000, 0.11).
narrative_ontology:measurement(orthographic_legitimacy_kernel__continuity_reading_tr_t2024, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 2024, 0.12).

% Extraction over time
narrative_ontology:measurement(orthographic_legitimacy_kernel__continuity_reading_be_t1928, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 1928, 0.08).
narrative_ontology:measurement(orthographic_legitimacy_kernel__continuity_reading_be_t1950, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 1950, 0.1).
narrative_ontology:measurement(orthographic_legitimacy_kernel__continuity_reading_be_t1975, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 1975, 0.12).
narrative_ontology:measurement(orthographic_legitimacy_kernel__continuity_reading_be_t2000, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 2000, 0.14).
narrative_ontology:measurement(orthographic_legitimacy_kernel__continuity_reading_be_t2024, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 2024, 0.15).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(orthographic_legitimacy_kernel__continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(orthographic_legitimacy_kernel__continuity_reading, 0.08).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__continuity_reading, orthographic_legitimacy_kernel__instrumentalist_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__continuity_reading, orthographic_legitimacy_kernel__modernist_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__continuity_reading, turkish_language_reform_1928).

% DUAL FORMULATION NOTE:
% This constraint is the continuity_reading of the orthographic_legitimacy_kernel. It decomposes the kernel's 'orthographic legitimacy' claim into the specific structural claim: script incompatibility is a mountain-like constraint whose primary structural fact is the near-total collapse of access to pre-1928 texts. The instrumentalist_reading and modernist_reading are sibling constraints with different ε values and different beneficiary/victim structures, linked via network.affects_constraints. The ε-invariance principle requires separate stories because the kernel's label conflates three structurally distinct legitimacy grounds.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
