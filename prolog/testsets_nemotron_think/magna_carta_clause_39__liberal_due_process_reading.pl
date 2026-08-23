% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__liberal_due_process_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_clause_39__liberal_due_process_reading, []).

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
 *   constraint_id: magna_carta_clause_39__liberal_due_process_reading
 *   human_readable: Magna Carta Clause 39 — Liberal Due Process Reading
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   Clause 39 of Magna Carta (1215) — 'No free man shall be seized or
 *   imprisoned, or stripped of his rights or possessions, or outlawed or
 *   exiled, or deprived of his standing in any other way, nor will we proceed
 *   with force against him, or send others to do so, except by the lawful
 *   judgment of his equals or by the law of the land' — is the kernel. The
 *   liberal due process reading instantiates this as a universal, substantive
 *   constraint on arbitrary state power: 'no person' (not just free men),
 *   'deprived of life, liberty, or property' (expanding the protected
 *   interests), 'without due process of law' (reading 'law of the land' as a
 *   qualitative standard, not mere legislative enactment). This reading
 *   powers the Anglo-American due process tradition, the Fifth and Fourteenth
 *   Amendments, and Article 6 ECHR. It claims the constraint is a genuine
 *   coordination mechanism protecting all subjects from state arbitrariness,
 *   while extracting the sovereign's discretionary power to act outside law.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__liberal_due_process_reading, 0.75).
domain_priors:suppression_score(magna_carta_clause_39__liberal_due_process_reading, 0.65).
domain_priors:theater_ratio(magna_carta_clause_39__liberal_due_process_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__liberal_due_process_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_clause_39__liberal_due_process_reading, "Magna Carta Clause 39 — Liberal Due Process Reading").
narrative_ontology:topic_domain(magna_carta_clause_39__liberal_due_process_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_clause_39__liberal_due_process_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__liberal_due_process_reading, '980dbc15-1914-471a-af20-57f2bfb8785b').
narrative_ontology:cs_kernel_codification('980dbc15-1914-471a-af20-57f2bfb8785b', fixed_text).
narrative_ontology:cs_authority_grounding('980dbc15-1914-471a-af20-57f2bfb8785b', lineage).
narrative_ontology:cs_interpretation_layer_present('980dbc15-1914-471a-af20-57f2bfb8785b').
narrative_ontology:cs_reading_relation('980dbc15-1914-471a-af20-57f2bfb8785b', magna_carta_clause_39__feudal_prerogative_reading, forecloses).
narrative_ontology:cs_reading_relation('980dbc15-1914-471a-af20-57f2bfb8785b', magna_carta_clause_39__originalist_limitation_reading, coexists_with).
narrative_ontology:cs_axiom('980dbc15-1914-471a-af20-57f2bfb8785b', foundational, due_process_as_universal_right).
narrative_ontology:cs_axiom_status(due_process_as_universal_right, holdable).
narrative_ontology:cs_axiom_grounding('980dbc15-1914-471a-af20-57f2bfb8785b', due_process_as_universal_right, deontological).
narrative_ontology:cs_axiom('980dbc15-1914-471a-af20-57f2bfb8785b', foundational, executive_subjection_to_law).
narrative_ontology:cs_axiom_status(executive_subjection_to_law, holdable).
narrative_ontology:cs_axiom_grounding('980dbc15-1914-471a-af20-57f2bfb8785b', executive_subjection_to_law, conventional).
narrative_ontology:cs_axiom('980dbc15-1914-471a-af20-57f2bfb8785b', secondary, law_of_land_as_substantive_standard).
narrative_ontology:cs_axiom_status(law_of_land_as_substantive_standard, holdable).
narrative_ontology:cs_axiom_grounding('980dbc15-1914-471a-af20-57f2bfb8785b', law_of_land_as_substantive_standard, instrumental).
narrative_ontology:cs_reference_frame('980dbc15-1914-471a-af20-57f2bfb8785b', charter_of_universal_liberties_1215).
narrative_ontology:cs_drift_state('980dbc15-1914-471a-af20-57f2bfb8785b', contemporary_constitutional_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('980dbc15-1914-471a-af20-57f2bfb8785b', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, all_citizens).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, legal_subjects).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, the_governed).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, executive_authority).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, royal_officials).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, state_actors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, royal_officials).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, parliament).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__liberal_due_process_reading, due_process_clause).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__liberal_due_process_reading, rule_of_law).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__liberal_due_process_reading, habeas_corpus).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__liberal_due_process_reading, legality_principle).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__liberal_due_process_reading, executive_subjection_to_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the universal protection against arbitrary arrest, imprisonment, disseisin, outlawry, and exile. The constraint guarantees that no person shall be deprived of life, liberty, or property except by lawful judgment of peers or by the law of the land. Exit from state jurisdiction is practically impossible; the constraint's protection is the primary shield against state overreach.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, all_citizens, beneficiary,
    organized, generational, constrained, national).

% The Crown and its successor executive institutions bear the constraint's extractive force: they lose the discretionary power to imprison, seize property, or banish subjects without legal process. The constraint cannot be exited by the executive — it is the constitutive limit on sovereign power. Compliance is enforced by courts; resistance manifests as claims of prerogative, emergency powers, or national security exceptions.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, executive_authority, payer,
    institutional, biographical, trapped, national).

% Sheriffs, judges, and administrative officers are directly constrained in their daily operations — they must follow legal process rather than executive command. They also benefit from the constraint's legitimation of their authority: acting 'by the law of the land' confers legitimacy that naked command lacks. Exit means leaving public office; the constraint travels with the office.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, royal_officials, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__liberal_due_process_reading, royal_officials, beneficiary).

% Courts are the primary enforcement mechanism — they interpret 'lawful judgment' and 'law of the land,' issue writs of habeas corpus, and review executive detention. The constraint constitutes judicial power: without it, courts have no purchase on executive action. Judges cannot exit the constraint without ceasing to be judges; their institutional identity is fused with its enforcement.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% The legislature benefits from the constraint's establishment of 'law of the land' as superior to executive will — it makes parliamentary statute the measure of executive legitimacy. Parliament also bears the constraint when it legislates: it must provide the 'law' that authorizes deprivation. The constraint is the foundation of legislative supremacy over executive prerogative.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, parliament, beneficiary,
    institutional, generational, analytical, national).

% Produce the interpretive tradition that reads Clause 39 as a universal due process guarantee. They trace the lineage from 1215 through Coke, Blackstone, the Fifth and Fourteenth Amendments, and international human rights law. Their analytical seat is not bound by the constraint's jurisdiction but shapes its evolving meaning across jurisdictions.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% In practice, the liberal reading's universalism stops at the border and the prison gate: immigration detainees, enemy combatants, extraterritorial prisoners, and undocumented persons often find the constraint's protections suspended or unavailable. They would object to their exclusion if they could access the forum; their absence from the conversation is structural.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, non_citizens_and_detainees, excluded,
    powerless, immediate, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the relationship between state power and individual liberty by establishing that no person shall be deprived of life, liberty, or property without lawful judgment of peers or by the law of the land — a procedural guarantee against arbitrary executive action that makes state power legible, contestable, and bounded.
% TRANSFER_FUNCTION: Moves discretionary power from the executive (the Crown, royal officials, modern administrative agencies) to the legal process (courts, juries, established law, parliamentary statute), constraining arbitrary arrest, imprisonment, disseisin, outlawry, exile, and their modern equivalents (indefinite detention, asset forfeiture, administrative removal).
% ABSENT_VOICES: In 1215: serfs, women, Jews, and non-free persons were excluded from 'free men.' In modern application: non-citizens, enemy combatants, immigration detainees, and those in extraterritorial detention (Guantánamo, black sites) often fall outside the constraint's practical protection. The excluded are structurally absent — the constraint's enforcement mechanisms (courts, habeas corpus) are precisely what the executive denies them.
% DISAPPEARANCE_RATIONALE: The constraint is the foundational textual anchor for due process, habeas corpus, and the principle that the executive is subject to law. Its removal would eliminate the constitutional basis for judicial review of executive detention and property deprivation. Executive power would expand to its pre-1215 default: the sovereign's will as law. The rule of law would collapse into rule by decree.
% FOUNDING_PROBLEM: The arbitrary exercise of royal power — imprisonment, disseisin, outlawry, and exile without judgment — threatened the baronage and free men's security of person and property. The King's courts and officers acted as instruments of his will rather than as independent adjudicators.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (J.C. Holt, David Carpenter) document the 1215 context as a baronial peace treaty addressing specific grievances. Constitutional scholars (A.V. Dicey, modern due process theorists from Hurtado to Matthews v. Eldridge) attest the structural problem of executive overreach persists in new forms (administrative detention, national security exceptions, immigration plenary power). The constraint's own institutional beneficiaries (the Crown's successors, Parliament) have historically resisted its expansion — corroboration from outside the beneficiary set is the historical record of resistance, not assent.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__liberal_due_process_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__liberal_due_process_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__liberal_due_process_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_clause_39__liberal_due_process_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__liberal_due_process_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__liberal_due_process_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_clause_39__liberal_due_process_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_clause_39__liberal_due_process_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75) is high because the constraint substantially restricts executive discretion across detention, property, and banishment — the core prerogatives of sovereignty. Suppression (0.65) is moderate-high: the executive cannot exit the constraint (it is constitutional bedrock), but enforcement depends on judicial willingness to confront the executive, which varies. Theater ratio (0.25) is low-moderate: the constraint's procedural guarantees (habeas corpus, fair hearing, notice) are genuinely functional, but national security and immigration contexts show performative compliance where process is stripped to its skeleton. Accessibility collapse (0.60) reflects that alternative protections (common law writs, political checks) have been largely absorbed into or displaced by the due process framework. Resistance (0.55) captures centuries of executive pushback: Stuart prerogative, wartime suspensions, administrative state deference, war-on-terror exceptions.
 *
 * PERSPECTIVAL GAP:
 *   From the executive seat, the constraint appears as an illegitimate intrusion on sovereign discretion — a snare that prevents necessary action (national security, immigration control, emergency response). From the citizen seat, it appears as a mountain — the fundamental law that makes freedom possible. From the judicial seat, it appears as a tangled rope — genuine coordination of the state-subject relationship that requires constant active enforcement against executive resistance. The engine computes this divergence from the structural data: same constraint, three different χ values, three different experienced types.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive authority (Crown, President, Prime Minister, agencies) is the structural target (d ≈ 0.9): it bears the full extractive force — loss of arbitrary power, obligation to provide process, judicial review of its actions. Exit is trapped: the constraint constitutes the office. All citizens/legal subjects are beneficiaries (d ≈ 0.1): they receive the constraint's protection without administering it. Their exit is constrained (cannot leave state jurisdiction easily). Royal officials/state agents are payers (d ≈ 0.7): they lose discretionary authority but gain legitimacy. Judiciary is agenda_setter (d ≈ 0.2): it administers the constraint and benefits from the institutional power it confers. Parliament is beneficiary (d ≈ 0.3): it gains legislative supremacy but is also bound when it legislates deprivations. Non-citizens/detainees are excluded (d ≈ 0.95): they bear the constraint's absence without its protection.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (arbitrary royal power over baronage) is historically dead — those specific abuses are gone. But the structural problem (executive overreach unchecked by law) is live in new forms: administrative detention without trial, civil asset forfeiture, immigration plenary power, drone strikes on citizens, pandemic emergency powers. The constraint has not atrophied; its mandate has metastasized. The liberal reading prevents mislabeling this as pure extraction (snare) because the coordination function — making state power legible and contestable — is genuine and actively maintained by courts. It prevents mislabeling as pure coordination (rope) because the executive genuinely loses power it would otherwise exercise, and the constraint's persistence depends on active judicial enforcement against executive resistance. The mandatrophy is unresolved: the constraint's original mandate is dead, but its evolved mandate is live and expanding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading_structure,
    'This constraint is one reading (liberal_due_process_reading) of the contested kernel magna_carta_clause_39. Sibling readings are feudal_prerogative_reading and originalist_limitation_reading. What structural elements differ across readings?',
    'Comparative constraint story generation for each reading: each reading authors its own ε, beneficiary/victim structure, and type. The kernel''s ε-invariance is tested by whether the readings decompose into distinct constraints with stable ε values.',
    'If the readings produce divergent ε values and classifications, the kernel label ''Clause 39'' conflates structurally distinct constraints — confirming the ε-invariance principle. If they converge, the dispute is interpretive, not structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading_structure, conceptual, 'Commitment-system structure: kernel identity, reading relations, axiom divergence').

omega_variable(
    natural_right_vs_positive_law,
    'Does the liberal reading''s ''law of the land'' refer to a pre-political natural law standard (which the constraint discovers) or a positive legal standard (which the constraint constitutes)?',
    'Historical analysis of Coke, Locke, and the American founding vs. legal positivist readings (Bentham, Austin, Hart). Test: if Parliament enacted a statute authorizing arbitrary detention, would the liberal reading treat it as ''law of the land'' (positive) or void as against natural law (natural)?',
    'Natural law grounding → constraint is mountain-like (emerges_naturally, low extractiveness from the reading''s view). Positive law grounding → constraint is tangled_rope (constructed, extractive against executive, requires enforcement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_right_vs_positive_law, conceptual, 'Epistemic status of the constraint''s normative standard').

omega_variable(
    universalism_boundary,
    'Where does the liberal reading''s ''no person'' universality actually stop? Does it extend to enemy combatants, unauthorized entrants, extraterritorial detainees, corporate persons?',
    'Case law survey: Boumediene v. Bush (habeas for Guantánamo), Zadvydas v. Davis (indefinite detention of removable aliens), Hamdi v. Rumsfeld (citizen enemy combatant), Jennings v. Rodriguez (bond hearings for detainees). Track the boundary''s movement over the interval.',
    'If the boundary is shrinking (more exclusions), the constraint''s extractiveness against the executive is decreasing (executive regains discretion) — ε drifts down. If expanding, ε drifts up. The omega tracks whether ''universal'' is a stable structural claim or an aspirational marker.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universalism_boundary, empirical, 'Empirical boundary of the constraint''s victim/beneficiary set').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__liberal_due_process_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mc39_liberal_tr_t0, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(mc39_liberal_tr_t10, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(mc39_liberal_tr_t20, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(mc39_liberal_tr_t30, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(mc39_liberal_tr_t40, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(mc39_liberal_tr_t50, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 50, 0.25).
narrative_ontology:measurement(mc39_liberal_tr_t60, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement(mc39_liberal_tr_t70, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 70, 0.24).
narrative_ontology:measurement(mc39_liberal_tr_t80, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 80, 0.25).

% Extraction over time
narrative_ontology:measurement(mc39_liberal_be_t0, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mc39_liberal_be_t10, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(mc39_liberal_be_t20, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(mc39_liberal_be_t30, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(mc39_liberal_be_t40, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(mc39_liberal_be_t50, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(mc39_liberal_be_t60, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 60, 0.71).
narrative_ontology:measurement(mc39_liberal_be_t70, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 70, 0.73).
narrative_ontology:measurement(mc39_liberal_be_t80, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 80, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(mc39_liberal_su_t0, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(mc39_liberal_su_t10, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(mc39_liberal_su_t20, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(mc39_liberal_su_t30, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(mc39_liberal_su_t40, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(mc39_liberal_su_t50, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 50, 0.63).
narrative_ontology:measurement(mc39_liberal_su_t60, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement(mc39_liberal_su_t70, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 70, 0.64).
narrative_ontology:measurement(mc39_liberal_su_t80, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 80, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__liberal_due_process_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_clause_39__liberal_due_process_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39__feudal_prerogative_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39__originalist_limitation_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, fifth_amendment_due_process).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, fourteenth_amendment_due_process).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, echr_article_6_fair_trial).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, habeas_corpus_act_1679).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, administrative_procedure_act).

% DUAL FORMULATION NOTE:
% This constraint is the liberal_due_process_reading of the magna_carta_clause_39 kernel. The feudal_prerogative_reading (narrow, hierarchical) and originalist_limitation_reading (historically bounded) are sibling constraints. All three share the kernel_id but instantiate different ε values, beneficiary/victim structures, and claimed types. The liberal reading's ε (0.75) is substantially higher than the feudal reading's (est. 0.25, narrow coordination) and the originalist reading's (est. 0.40, limited historical restraint). The ε-invariance principle requires separate stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_clause_39__liberal_due_process_reading, institutional, 0.85).
constraint_indexing:directionality_override(magna_carta_clause_39__liberal_due_process_reading, organized, 0.15).
constraint_indexing:directionality_override(magna_carta_clause_39__liberal_due_process_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
