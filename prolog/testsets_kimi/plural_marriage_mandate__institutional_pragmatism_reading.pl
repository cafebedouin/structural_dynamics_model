% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__institutional_pragmatism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__institutional_pragmatism_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: plural_marriage_mandate__institutional_pragmatism_reading
 *   human_readable: 1890 Manifesto as Institutional Survival Mechanism
 *   domain: religious_institutional_history/political_theology
 *
 * SUMMARY:
 *   The 1890 Manifesto suspended plural marriage in the LDS Church under
 *   intense federal coercion. This constraint story adopts the
 *   institutional_pragmatism_reading: the Manifesto was a strategic
 *   adaptation in which doctrinal claims (revelation narrative) served to
 *   legitimate survival-driven capitulation to superior coercive power. The
 *   kernel is the plural_marriage_mandate; this reading decomposes from
 *   siblings endogenous_reinterpretation_reading and
 *   exogenous_override_reading because the same historical event admits
 *   multiple structurally distinct framings with different epsilon values and
 *   directionalities. The primary observable is the M-set gap: doctrine
 *   nominally unchanged, practice publicly suspended, with secret
 *   continuations authorized by leadership from 1890-1904.
 *
 * KEY AGENTS:
 *   - church_leadership: Agenda-setter and beneficiary (institutional/powerful/identity_locked) â orchestrates the Manifesto, enforces compliance, and captures institutional survival and restored political rights.
 *   - coerced_polygamists: Primary target (powerless/trapped) â bears the direct cost of abandoning families and going underground under threat of imprisonment and excommunication.
 *   - deceived_monogamists: Secondary target (moderate/identity_locked) â bears the cost of doctrinal deception, trusting the Manifesto as genuine revelation while secret exceptions continued.
 *   - mormon_studies_scholar: Analytical observer (analytical) â evaluates the structural gap between performed revelation and institutional survival imperatives.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__institutional_pragmatism_reading, 0.62).
domain_priors:suppression_score(plural_marriage_mandate__institutional_pragmatism_reading, 0.68).
domain_priors:theater_ratio(plural_marriage_mandate__institutional_pragmatism_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__institutional_pragmatism_reading, tangled_rope).
narrative_ontology:human_readable(plural_marriage_mandate__institutional_pragmatism_reading, "1890 Manifesto as Institutional Survival Mechanism").
narrative_ontology:topic_domain(plural_marriage_mandate__institutional_pragmatism_reading, "religious_institutional_history/political_theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__institutional_pragmatism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__institutional_pragmatism_reading, 'c3ffefa5-ece2-4c47-8a92-c954f218bb93').
narrative_ontology:cs_kernel_codification('c3ffefa5-ece2-4c47-8a92-c954f218bb93', fixed_text).
narrative_ontology:cs_authority_grounding('c3ffefa5-ece2-4c47-8a92-c954f218bb93', extraction).
narrative_ontology:cs_interpretation_layer_present('c3ffefa5-ece2-4c47-8a92-c954f218bb93').
narrative_ontology:cs_reading_relation('c3ffefa5-ece2-4c47-8a92-c954f218bb93', plural_marriage_mandate__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('c3ffefa5-ece2-4c47-8a92-c954f218bb93', plural_marriage_mandate__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('c3ffefa5-ece2-4c47-8a92-c954f218bb93', foundational, manifesto_revelation_was_strategic_legitimation).
narrative_ontology:cs_axiom_status(manifesto_revelation_was_strategic_legitimation, holdable).
narrative_ontology:cs_axiom_grounding('c3ffefa5-ece2-4c47-8a92-c954f218bb93', manifesto_revelation_was_strategic_legitimation, empirically_contingent).
narrative_ontology:cs_axiom('c3ffefa5-ece2-4c47-8a92-c954f218bb93', foundational, institutional_survival_drives_doctrinal_instrumentalization).
narrative_ontology:cs_axiom_status(institutional_survival_drives_doctrinal_instrumentalization, holdable).
narrative_ontology:cs_axiom_grounding('c3ffefa5-ece2-4c47-8a92-c954f218bb93', institutional_survival_drives_doctrinal_instrumentalization, instrumental).
narrative_ontology:cs_reference_frame('c3ffefa5-ece2-4c47-8a92-c954f218bb93', active_plural_marriage_mandate).
narrative_ontology:cs_drift_state('c3ffefa5-ece2-4c47-8a92-c954f218bb93', post_1890_manifesto_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c3ffefa5-ece2-4c47-8a92-c954f218bb93', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, church_leadership).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, coerced_polygamists).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, deceived_monogamists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issued the 1890 Manifesto under existential federal pressure, authorized secret continuations of plural marriage for select members while publicly enforcing compliance, secured Utah statehood and returned church property, and maintains the revelatory framing in institutional memory.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, church_leadership, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__institutional_pragmatism_reading, church_leadership, beneficiary).

% Were commanded to abandon plural families or face excommunication and federal imprisonment; some entered secret unions with leadership authorization but lived under threat of exposure; bore the direct cost of the Manifesto's public enforcement.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, coerced_polygamists, payer,
    powerless, biographical, trapped, national).

% Accepted the Manifesto as a genuine revelation suspending plural marriage; were unaware of post-1890 authorized plural marriages; their trust in prophetic transparency was consumed to maintain institutional solidarity.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, deceived_monogamists, payer,
    moderate, generational, identity_locked, national).

% Analyzes the documentary and structural record to distinguish between performed revelation and institutional survival imperatives; operates outside the constraint's identity demands.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, mormon_studies_scholar, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__institutional_pragmatism_reading, church_leadership).
narrative_ontology:fixing_cost_class(plural_marriage_mandate__institutional_pragmatism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the institutional continuity of the church and its membership under existential federal threat by publicly suspending a practice that had become politically untenable, allowing political rights and property to be restored.
% TRANSFER_FUNCTION: Transfers compliance costs from church leadership to practicing polygamists (abandonment of families, secrecy, exile) and transfers legitimacy costs to general membership (acceptance of a doctrinally anomalous suspension presented as revelation); in exchange, leadership secures institutional survival and federal reintegration.
% ABSENT_VOICES: Practicing polygamists who rejected the Manifesto and were excommunicated or driven underground; federal prosecutors who were deceived about the scope of post-1890 compliance; later fundamentalist movements that claimed the true doctrine was betrayed.
% DISAPPEARANCE_RATIONALE: If the Manifesto constraint vanished (i.e., if the church had not issued a public suspension and instead openly defied federal power), the institutional structure would likely have been dismantled by federal seizure, Utah statehood would have been delayed or prevented, and the modern church would not exist in its current form. Conversely, if the revelation narrative were exposed as pure pragmatism in 1890, internal schism would have accelerated. The constraint's disappearance would fundamentally rearrange Mormon institutional history.
% FOUNDING_PROBLEM: The church faced existential federal pressure (Poland Act, Edmunds-Tucker Act, property seizure, disincorporation) that threatened institutional survival unless plural marriage was publicly abandoned.
% FOUNDING_PROBLEM_CORROBORATION: Federal legislative records and Supreme Court decisions (Reynolds, Late Corp.) corroborate the existential legal pressure from outside the beneficiary set. Independent historians (e.g., Arrington, Quinn, Alexander) attest to the political crisis; dissenting fundamentalist traditions attest that the problem was not theological but political.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__institutional_pragmatism_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__institutional_pragmatism_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__institutional_pragmatism_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(plural_marriage_mandate__institutional_pragmatism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__institutional_pragmatism_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__institutional_pragmatism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(plural_marriage_mandate__institutional_pragmatism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(plural_marriage_mandate__institutional_pragmatism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the arrangement externalized the full cost of federal compliance onto practicing polygamists and the trust of the general membership while leadership captured the benefit of institutional survival and political rehabilitation. Suppression (0.68) is high because persistence depended on church disciplinary enforcement, federal marshals, and the active exclusion of fundamentalist dissent. Theater_ratio (0.55) reflects that more than half of the constraint's energy was performative: maintaining the revelation narrative in public while privately authorizing exceptions. Accessibility_collapse (0.72) is high because once inside the church's identity framework, exit to non-Mormon social structures was extremely costly. Resistance (0.48) is moderate: underground polygamist networks and later fundamentalist schisms mounted significant but fragmented opposition. Measurements track the widening M-set gap from 1890 to the 1904 Second Manifesto, after which theater slowly declined as secret practice ended.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (church_leadership) experiences the constraint as necessary institutional coordination under impossible political pressure; the target seats (coerced_polygamists and deceived_monogamists) experience the same structure as asymmetric extraction where their families, trust, and doctrinal integrity were consumed to purchase institutional survival. The engine computes this divergence from the structural data: identical events read as coordination from one power/exit position and as extraction from another.
 *
 * DIRECTIONALITY LOGIC:
 *   Church_leadership is declared as beneficiary and agenda_setter with identity_locked exit, producing a strongly beneficiary-biased directionality. Coerced_polygamists are declared victims with trapped exit and powerless status, producing strongly target-biased directionality. Deceived_monogamists are declared victims with identity_locked exit and moderate power, producing moderately target-biased directionality. The gain_flow is explicitly authored to church_leadership because the extraction (political rights, institutional continuity, property restoration) demonstrably accrued to that seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (existential federal anti-polygamy pressure) is dead by 1904-1910, yet the constraint persists in doctrinal memory and institutional narrative. This would trigger a piton/theater reading except that active enforcement continues: the church still disciplines modern polygamists and suppresses fundamentalist movements to maintain the Manifesto's legitimacy. The active enforcement requirement for tangled_rope is met by ongoing boundary maintenance, preventing the constraint from degrading into pure inertial piton despite its obsolesced founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_pragmatism_kernel_location,
    'This constraint is the institutional_pragmatism_reading of the plural_marriage_mandate kernel. How would sibling readings restructure the beneficiary/victim distribution?',
    'Comparative analysis of the three readings: endogenous_reinterpretation_reading recasts beneficiaries as humanity/salvific mission and removes victims by treating compliance as voluntary obedience; exogenous_override_reading removes the doctrinal beneficiary layer entirely and treats all compliance as raw coercion. The structural disagreement is located at whether church leadership exercised strategic agency in crafting the revelation narrative.',
    'If the endogenous reading is correct, the constraint reclassifies toward rope (legitimate coordination under prophetic authority). If the exogenous reading is correct, the constraint loses its tangled rope character and becomes a simpler snare of federal coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_pragmatism_kernel_location, conceptual, 'Committer-frame location of this reading within the plural_marriage_mandate kernel').

omega_variable(
    secret_continuation_magnitude,
    'What was the actual scope of authorized post-1890 plural marriages, and how does this affect the extraction and theater metrics?',
    'Archival documentary discovery of post-1890 plural marriage authorizations by church leadership, quantitative estimates of marriages performed 1890-1904.',
    'A large scope of secret continuations raises theater_ratio and confirms high extraction (coerced public compliance with private exception). A negligible scope would lower extraction and suggest the Manifesto was more genuinely enforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secret_continuation_magnitude, empirical, 'Empirical uncertainty about post-Manifesto secret practice scope').

omega_variable(
    leadership_intentionality,
    'Did church leadership experience the Manifesto as genuine revelation, as pragmatic necessity, or as a fused combination â and does this distinction change the constraint classification?',
    'Close historical analysis of private correspondence, diary records, and secondhand accounts of First Presidency discussions in 1889-1890.',
    'If leadership sincerely experienced divine mandate for suspension, the constraint shifts toward endogenous reinterpretation (legitimate coordination). If leadership explicitly discussed political survival as the sole motive, the extraction layer strengthens. Ambiguity sustains the current tangled_rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(leadership_intentionality, conceptual, 'Ambiguity about the subjective intentionality of the authority structure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__institutional_pragmatism_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t0, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(plur_tr_t4, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 4, 0.48).
narrative_ontology:measurement(plur_tr_t8, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 8, 0.55).
narrative_ontology:measurement(plur_tr_t12, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 12, 0.62).
narrative_ontology:measurement(plur_tr_t16, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 16, 0.6).
narrative_ontology:measurement(plur_tr_t20, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 20, 0.58).
narrative_ontology:measurement(plur_tr_t24, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 24, 0.56).
narrative_ontology:measurement(plur_tr_t30, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(plur_be_t0, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(plur_be_t4, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(plur_be_t8, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(plur_be_t12, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(plur_be_t16, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(plur_be_t20, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(plur_be_t24, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 24, 0.63).
narrative_ontology:measurement(plur_be_t30, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 30, 0.62).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(plural_marriage_mandate__institutional_pragmatism_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__institutional_pragmatism_reading, identity_coordination).
narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, exogenous_override_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the plural_marriage_mandate kernel, decomposed from endogenous_reinterpretation_reading and exogenous_override_reading because the same historical event (the 1890 Manifesto) admits multiple structurally distinct framings that assign different epsilon values, beneficiary sets, and directionalities. This reading assigns a tangled_rope classification; siblings assign different types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
