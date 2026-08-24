% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Orthographic Legitimacy from Tradition Access (Continuity Reading)
 *   domain: political_linguistics/state_formation/commitment_systems
 *
 * SUMMARY:
 *   The 1928 Turkish alphabet reform replaced the Arabic-script Ottoman
 *   Turkish orthography with a Latin-script system. The continuity reading of
 *   the orthographic legitimacy kernel asserts that legitimate orthography
 *   must preserve unmediated access to the historical, religious, and
 *   literary tradition written in the prior script. The constraint is the
 *   physical incompatibility between the two scripts: texts written in one
 *   cannot be read by those literate only in the other. This is claimed as a
 *   mountain — a natural law of graphic systems. The victim is post-reform
 *   generations severed from direct access to pre-1928 texts. The claimed
 *   extraction is low (0.12) because the constraint is framed as loss rather
 *   than active extraction. However, the state's active maintenance of the
 *   Latin-script monopoly (suppression_requirement declining from 0.35 to
 *   0.08 over the interval) suggests the constraint's persistence is not
 *   purely natural.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__continuity_reading, 0.12).
domain_priors:suppression_score(orthographic_legitimacy_kernel__continuity_reading, 0.08).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__continuity_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__continuity_reading, mountain).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__continuity_reading, "Orthographic Legitimacy from Tradition Access (Continuity Reading)").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__continuity_reading, "political_linguistics/state_formation/commitment_systems").

domain_priors:emerges_naturally(orthographic_legitimacy_kernel__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__continuity_reading, '78976a85-3ce8-4376-b17b-227c0f2395a8').
narrative_ontology:cs_kernel_codification('78976a85-3ce8-4376-b17b-227c0f2395a8', fixed_text).
narrative_ontology:cs_authority_grounding('78976a85-3ce8-4376-b17b-227c0f2395a8', lineage).
narrative_ontology:cs_interpretation_layer_present('78976a85-3ce8-4376-b17b-227c0f2395a8').
narrative_ontology:cs_reading_relation('78976a85-3ce8-4376-b17b-227c0f2395a8', orthographic_legitimacy_kernel__instrumentalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('78976a85-3ce8-4376-b17b-227c0f2395a8', orthographic_legitimacy_kernel__modernist_reading, coexists_with).
narrative_ontology:cs_axiom('78976a85-3ce8-4376-b17b-227c0f2395a8', foundational, orthographic_legitimacy_requires_tradition_access).
narrative_ontology:cs_axiom_status(orthographic_legitimacy_requires_tradition_access, holdable).
narrative_ontology:cs_axiom_grounding('78976a85-3ce8-4376-b17b-227c0f2395a8', orthographic_legitimacy_requires_tradition_access, deontological).
narrative_ontology:cs_axiom('78976a85-3ce8-4376-b17b-227c0f2395a8', secondary, script_continuity_is_legitimacy_condition).
narrative_ontology:cs_axiom_status(script_continuity_is_legitimacy_condition, holdable).
narrative_ontology:cs_axiom_grounding('78976a85-3ce8-4376-b17b-227c0f2395a8', script_continuity_is_legitimacy_condition, deontological).
narrative_ontology:cs_reference_frame('78976a85-3ce8-4376-b17b-227c0f2395a8', ottoman_islamic_script_continuity).
narrative_ontology:cs_drift_state('78976a85-3ce8-4376-b17b-227c0f2395a8', post_1928_latin_reform, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('78976a85-3ce8-4376-b17b-227c0f2395a8', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__continuity_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__continuity_reading, post_reform_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__continuity_reading, ottoman_script_scholars).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__continuity_reading, script_continuity_enables_tradition_access).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__continuity_reading, legitimacy_derives_from_historical_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Generations born after the 1928 alphabet reform who cannot read pre-1928 Ottoman Turkish texts in Arabic script without specialized training. They bear the cost of severed direct access to historical, religious, and literary tradition — requiring mediation through transliteration, translation, or academic intermediaries to reach their own heritage. Exit from this constraint would require learning a dead script system, which is institutionally unsupported and practically constrained by time and resource costs.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, post_reform_generations, payer,
    organized, generational, constrained, national).

% Academic specialists who retain reading knowledge of Ottoman Arabic script. They benefit professionally from the constraint's operation — their expertise becomes a gatekeeping credential for access to primary sources. They do not administer the constraint but collect status and career capital from the scarcity they help maintain.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, ottoman_script_scholars, beneficiary,
    moderate, biographical, mobile, national).

% The Turkish Language Association (TDK) and state education apparatus that implemented and maintains the Latin-script monopoly. They set the agenda by controlling curriculum, publishing standards, and official orthography. They have arbitrage-grade exit — they could reintroduce Arabic-script education but choose not to, as the constraint serves their modernization narrative.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, state_language_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Communities (e.g., Sufi orders, traditional madrasa networks) for whom Arabic script is liturgically and identity-constitutive. They are excluded from the official legitimacy framework — their script practice is tolerated privately but denied public/institutional recognition. Their exit is identity-locked: abandoning Arabic script would dissolve the self-understanding that sustains their communal continuity.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, religious_tradition_bearers, excluded,
    organized, generational, identity_locked, national).

% External analyst evaluating the constraint across the kernel's readings. Sees the structural divergence: continuity reading frames script change as severance (mountain-like physical incompatibility); instrumentalist reading frames it as literacy gain (rope-like coordination); modernist reading frames it as civilizational rupture (scaffold with sunset). Computes per-seat types from the structural data authored here.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, comparative_linguist_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a community's direct, unmediated access to its own historical, religious, and literary corpus by maintaining the script in which that corpus was written. The coordination problem solved: how to ensure each generation can read its ancestors without specialist intermediaries.
% TRANSFER_FUNCTION: Moves the burden of script acquisition from the collective (universal education in the heritage script) to the individual (specialized training required to access pre-1928 texts). The cost of tradition access shifts from a public good to a private investment.
% ABSENT_VOICES: Pre-reform generations (dead, cannot object); rural Anatolian populations of 1928 who were largely illiterate in both scripts and had no say in the reform; contemporary Kurdish and Arabic-script minority communities whose liturgical traditions are similarly severed but not recognized in the Turkish continuity narrative.
% DISAPPEARANCE_RATIONALE: If the Latin-script monopoly vanished overnight, the Turkish state would lose its primary orthographic legitimacy anchor. Educational curricula, legal publishing, and public signage would require immediate restructuring. Religious communities would reclaim public script use. The symbolic architecture of the republic's rupture from the Ottoman past would collapse — the constraint is load-bearing for the modernist state's self-legitimation.
% FOUNDING_PROBLEM: The Ottoman Arabic script was poorly suited to Turkish phonology (vowel harmony, eight vowels vs. three matres lectionis), creating a literacy barrier that limited mass education and administrative efficiency in the late Ottoman period. The founding problem was functional: how to make writing accessible to the population.
% FOUNDING_PROBLEM_CORROBORATION: Literacy rates in Turkey rose from ~10% (1927) to >90% (2000) under Latin script — corroborated by UNESCO and Turkish Statistical Institute data from outside the continuity reading's beneficiary set. The phonological mismatch problem is empirically resolved. The continuity reading's claim that legitimacy *derives from* tradition access (not literacy) is a normative re-framing of a solved functional problem, attested by the reform's own architects (Atatürk, TDK founders) who explicitly framed the change as solving the literacy problem.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__continuity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__continuity_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low because the primary mechanism is script incompatibility (a physical fact), not active rent collection. Suppression is low at interval end because the Latin script is now normalized; the active enforcement phase (1928-1950) has passed. Theater ratio is near zero — no performative maintenance of a dead function. Accessibility collapse is very high (0.92): without learning Ottoman script, pre-1928 texts are completely inaccessible. Resistance is low (0.15) because the constraint is not actively resisted — it is accepted as the cost of modernization. The metrics describe a mountain; the claim asserts a mountain. The divergence (if any) will emerge from the engine's computation of whether the beneficiary structure (state_language_authority, ottoman_script_scholars) triggers false_summit_mountain.
 *
 * PERSPECTIVAL GAP:
 *   From the state_language_authority seat, the constraint is a solved coordination problem (literacy achieved) — rope-like. From the post_reform_generations seat, it is a severed inheritance — mountain-like loss with no compensation. From the religious_tradition_bearers seat, it is an active exclusion — snare-like. The engine computes these divergences from the structural data; the authored claim (mountain) reflects only the continuity reading's self-understanding.
 *
 * DIRECTIONALITY LOGIC:
 *   Post-reform generations are payers: they bear the cost of severed tradition access (d high). Ottoman script scholars are beneficiaries: they collect professional rents from the scarcity they help maintain (d low). State language authority is agenda_setter: it administers the constraint and could change it but chooses not to (d near 0). Religious tradition bearers are excluded: they are identity-locked into the severed tradition, their exit would dissolve their communal self-understanding (d very high, modulated by identity_locked). The analytical observer sits at d=0.5 (symmetric).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Ottoman script's phonological mismatch with Turkish) is dead — literacy rates confirm the Latin script solved it. Yet the constraint (Latin-script monopoly) persists. The continuity reading diagnoses this as mandatrophy: the arrangement outlived its function. However, the reading's own claim (legitimacy derives from tradition access) is a post-hoc legitimization — the reform's architects explicitly rejected tradition access as a value. The mandatrophy is real but the continuity reading's framing of it is itself a constructed legitimacy claim, not a natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_severance,
    'Is the script incompatibility between Arabic and Latin orthographies a genuine natural law (physical fact of graphic non-interoperability) or a constructed severance maintained by institutional choice?',
    'Counterfactual test: if the state tomorrow mandated bilingual education in both scripts, would the ''physical incompatibility'' dissolve as a practical barrier? If yes, the mountain claim is a false summit — the constraint is maintained by policy, not physics.',
    'If constructed, the constraint reclassifies from mountain to tangled_rope (coordination of modern literacy + extraction of tradition access from post-reform generations). The false_summit_mountain signature would trigger via beneficiary presence (state_language_authority, ottoman_script_scholars).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_severance, conceptual, 'Whether script incompatibility is irreducible physics or institutional choice').

omega_variable(
    continuity_reading_kernel_location,
    'Does this continuity reading inherit the kernel''s authority from the Ottoman-Islamic textual tradition itself (lineage), or from the modern republican state''s selective appropriation of that tradition (extraction)?',
    'Trace the authority chain: does the reading''s legitimacy claim rest on an unbroken chain of transmission from pre-1928 authorities, or on the republican state''s post-hoc endorsement of ''tradition'' as a legitimizing resource?',
    'If lineage, the reading''s authority is continuous with the kernel''s pre-reform grounding. If extraction, the reading is a modernist instrumentalization of tradition — the kernel''s authority_grounding shifts from lineage to extraction, altering the CS classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(continuity_reading_kernel_location, conceptual, 'Authority grounding of the continuity reading itself').

omega_variable(
    suppression_mechanism_post_reform,
    'Is the suppression experienced by post-reform generations structural (state monopoly on script education) or internalized (generations no longer desire access to pre-1928 texts)?',
    'Measure demand for Ottoman-script education in contemporary Turkey: if demand exists but is institutionally blocked, suppression is structural. If demand is negligible despite access options, suppression is partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the severance as identity. This would increase χ for the payer seat beyond the engine''s structural derivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_post_reform, empirical, 'Structural vs internalized suppression for post-reform generations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__continuity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orthographic_legitimacy_kernel__continuity_reading_tr_t0, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement(orthographic_legitimacy_kernel__continuity_reading_tr_t25, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 25, 0.03).
narrative_ontology:measurement(orthographic_legitimacy_kernel__continuity_reading_tr_t50, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 50, 0.04).
narrative_ontology:measurement(orthographic_legitimacy_kernel__continuity_reading_tr_t75, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 75, 0.05).
narrative_ontology:measurement(orthographic_legitimacy_kernel__continuity_reading_tr_t100, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(orthographic_legitimacy_kernel__continuity_reading_be_t0, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(orthographic_legitimacy_kernel__continuity_reading_be_t25, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 25, 0.1).
narrative_ontology:measurement(orthographic_legitimacy_kernel__continuity_reading_be_t50, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 50, 0.11).
narrative_ontology:measurement(orthographic_legitimacy_kernel__continuity_reading_be_t75, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 75, 0.12).
narrative_ontology:measurement(orthographic_legitimacy_kernel__continuity_reading_be_t100, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 100, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(orthographic_legitimacy_kernel__continuity_reading_su_t0, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(orthographic_legitimacy_kernel__continuity_reading_su_t25, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 25, 0.18).
narrative_ontology:measurement(orthographic_legitimacy_kernel__continuity_reading_su_t50, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 50, 0.1).
narrative_ontology:measurement(orthographic_legitimacy_kernel__continuity_reading_su_t75, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 75, 0.07).
narrative_ontology:measurement(orthographic_legitimacy_kernel__continuity_reading_su_t100, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(orthographic_legitimacy_kernel__continuity_reading, 0.08).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__continuity_reading, orthographic_legitimacy_kernel__instrumentalist_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__continuity_reading, orthographic_legitimacy_kernel__modernist_reading).

% DUAL FORMULATION NOTE:
% Orthographic legitimacy kernel decomposes into three readings with divergent ε: continuity (0.12, mountain-claimed), instrumentalist (~0.05, rope-claimed), modernist (~0.25, scaffold-claimed). The continuity reading's ε is higher than instrumentalist because it counts tradition-severance as extraction; instrumentalist counts only literacy-cost. Modernist reading's ε is higher because it acknowledges the rupture as intentional extraction from the Ottoman past. All three share the referent (the 1928 reform and its persistence) but differ in what they count as the constraint's function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(orthographic_legitimacy_kernel__continuity_reading, institutional, 0.1).
constraint_indexing:directionality_override(orthographic_legitimacy_kernel__continuity_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
