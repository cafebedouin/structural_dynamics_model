% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__hybrid_proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__hybrid_proportionality_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: geneva_conventions_protective_scope__hybrid_proportionality_reading
 *   human_readable: Geneva Protections Scaled by Conflict Type (Hybrid Proportionality Reading)
 *   domain: legal/humanitarian/armed_conflict
 *
 * SUMMARY:
 *   The Geneva Conventions and their Additional Protocols establish a tiered
 *   protection framework that scales humanitarian law coverage by conflict
 *   type. This constraint instantiates ONE READING of the Geneva protective
 *   scope kernel: the hybrid_proportionality_reading. This reading holds that
 *   protections scale by conflict classification (international vs.
 *   non-international), that proportionality analysis determines when
 *   civilian protection may be overridden, and that ambiguity about conflict
 *   type is resolved by the stronger party's legal interpretation. The core
 *   tension: the graduated approach theoretically solves the coverage gap
 *   between international and non-international conflicts, but in practice it
 *   permits the stronger party to argue for minimal protection standards
 *   while exercising proportionality calculus with minimal external review.
 *   The constraint is CLAIMED as tangled_rope because it provides
 *   coordination (graduated legal framework for diverse conflict types) while
 *   enabling extraction (stronger parties gain discretion to classify
 *   conflicts downward and apply proportionality permissively to civilians).
 *   The measurement series shows extractiveness and suppression rising from
 *   1977 to 2001, then plateauing: the initial rise reflects post-AP I/II
 *   period where hybrid conflicts accumulated and ambiguity about
 *   classification deepened; the plateau reflects institutionalization of
 *   stronger parties' interpretive practice.
 *
 * KEY AGENTS:
 *   - State parties (institutional): control conflict classification and proportionality determinations; benefit from classification discretion
 *   - Military establishments of stronger parties (powerful): benefit from proportionality calculus; can argue unprivileged belligerent status for opponents
 *   - Combatants in non-international conflicts (moderate/trapped): receive only Common Article 3 floor; no combatant immunity or POW status
 *   - Civilian populations (powerless): protection depends on conflict classification; bear cost of proportionality analysis
 *   - Unprivileged belligerents (powerless): occupy legal limbo; not eligible for POW status, subject to criminal prosecution
 *   - ICRC and humanitarian bodies (institutional/observer): interpret treaties, issue guidance; lack enforcement power
 *   - Non-state armed groups (excluded): minimal voice in classification; excluded from framework-setting conversation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.68).
domain_priors:suppression_score(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.71).
domain_priors:theater_ratio(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__hybrid_proportionality_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__hybrid_proportionality_reading, "Geneva Protections Scaled by Conflict Type (Hybrid Proportionality Reading)").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__hybrid_proportionality_reading, "legal/humanitarian/armed_conflict").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__hybrid_proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__hybrid_proportionality_reading, 'b8e7ac5c-f1cf-4ecd-8a08-fc9b49ba6b5a').
narrative_ontology:cs_kernel_codification('b8e7ac5c-f1cf-4ecd-8a08-fc9b49ba6b5a', fixed_text).
narrative_ontology:cs_authority_grounding('b8e7ac5c-f1cf-4ecd-8a08-fc9b49ba6b5a', extraction).
narrative_ontology:cs_interpretation_layer_present('b8e7ac5c-f1cf-4ecd-8a08-fc9b49ba6b5a').
narrative_ontology:cs_reading_relation('b8e7ac5c-f1cf-4ecd-8a08-fc9b49ba6b5a', geneva_conventions_protective_scope__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('b8e7ac5c-f1cf-4ecd-8a08-fc9b49ba6b5a', geneva_conventions_protective_scope__universal_rights_reading, influences).
narrative_ontology:cs_axiom('b8e7ac5c-f1cf-4ecd-8a08-fc9b49ba6b5a', foundational, conflict_type_determines_protection_floor).
narrative_ontology:cs_axiom_status(conflict_type_determines_protection_floor, holdable).
narrative_ontology:cs_axiom_grounding('b8e7ac5c-f1cf-4ecd-8a08-fc9b49ba6b5a', conflict_type_determines_protection_floor, conventional).
narrative_ontology:cs_axiom('b8e7ac5c-f1cf-4ecd-8a08-fc9b49ba6b5a', foundational, proportionality_permits_graduated_civilian_protection).
narrative_ontology:cs_axiom_status(proportionality_permits_graduated_civilian_protection, holdable).
narrative_ontology:cs_axiom_grounding('b8e7ac5c-f1cf-4ecd-8a08-fc9b49ba6b5a', proportionality_permits_graduated_civilian_protection, instrumental).
narrative_ontology:cs_reference_frame('b8e7ac5c-f1cf-4ecd-8a08-fc9b49ba6b5a', ap_i_ap_ii_scaled_framework).
narrative_ontology:cs_drift_state('b8e7ac5c-f1cf-4ecd-8a08-fc9b49ba6b5a', contemporary_hybrid_conflict_proliferation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b8e7ac5c-f1cf-4ecd-8a08-fc9b49ba6b5a', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, state_parties_controlling_classification).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, military_establishments_of_stronger_parties).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, combatants_in_non_international_conflicts).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilian_populations_in_ambiguous_conflict_zones).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, unprivileged_belligerents_and_irregular_forces).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilian_populations_in_ambiguous_conflict_zones).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__hybrid_proportionality_reading, conflict_type_determines_legal_floor).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__hybrid_proportionality_reading, proportionality_permits_graduated_protection).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States parties to the Geneva Conventions interpret and classify the nature of armed conflicts occurring within or affecting their territories. They determine whether a conflict is international (triggering AP I full protections) or non-international (Common Article 3 minimum floor only). This classification power allows stronger parties to argue their conflicts fall outside full AP I scope, reducing their legal obligations. The state controls the apparatus that makes this determination and enforces it through military command structures and legal offices.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, state_parties_controlling_classification, agenda_setter,
    institutional, generational, arbitrage, national).

% Armed forces of technologically and organizationally advanced parties benefit from the ambiguity: they can argue their opponents are unprivileged belligerents (not entitled to combatant immunity) or that the conflict is sufficiently non-international to justify lower protection thresholds. The proportionality calculus permits them to claim civilian casualties are proportionate to military advantage, with minimal external verification. Their exit option is superior military capability and capacity to define the conflict narrative.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, military_establishments_of_stronger_parties, beneficiary,
    powerful, biographical, arbitrage, national).

% Non-international armed conflict combatants (insurgents, rebels, non-state armed groups meeting Article 1(1) thresholds) receive only Common Article 3 protections: prohibition of torture, arbitrary execution, and hostage-taking, but NOT combatant immunity (prisoner-of-war status). They bear asymmetric legal risk: capture may result in criminal prosecution rather than POW detention, and proportionality calculations apply less favorably to their actions. They cannot exit the conflict classification applied to them.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, combatants_in_non_international_conflicts, payer,
    moderate, immediate, trapped, local).

% Civilians in zones of ambiguous conflict type bear the cost of proportionality analysis: their protection depends on whether the conflict is classified as international (full AP I civilian protections) or non-international (Article 3 baseline). In hybrid or unclear situations, they lack clarity on their protected status. They benefit nominally from Common Article 3 but suffer from the proportionality calculus that permits military advantage to override their protection in graduated fashion. Geographic and economic constraints prevent exit.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilian_populations_in_ambiguous_conflict_zones, payer,
    powerless, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilian_populations_in_ambiguous_conflict_zones, beneficiary).

% Fighters who do not meet Article 4 criteria (responsible command, fixed distinctive signs, open carrying of arms, disciplined conduct) occupy legal limbo: they are not entitled to combatant immunity or POW status, yet the proportionality reading permits some graduated protection if they are sufficiently organized. They bear the highest legal risk: criminal prosecution for any act of violence, combined with minimal procedural protection. Escape or legal clarification is not available to them.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, unprivileged_belligerents_and_irregular_forces, payer,
    powerless, immediate, trapped, local).

% ICRC, International Court of Justice, International Criminal Court, and humanitarian NGOs monitor implementation and interpret the treaties. They issue guidance on proportionality and conflict classification but lack enforcement power. Their interpretations feed back into the constraint's operation: stronger parties may cite or ignore these bodies depending on whether guidance supports their classification interests.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, international_humanitarian_law_bodies, observer,
    institutional, generational, analytical, global).

% Non-state armed groups have minimal voice in the conflict classification process. They may argue their organization meets Article 1(1) thresholds for non-international conflict status, but the state party controls the legal classification apparatus. They would benefit from universal protections (the universal_rights_reading) but are structurally excluded from the framework-setting conversation that determines whether that reading applies.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, weaker_parties_and_non_state_armed_groups, excluded,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_protective_scope__hybrid_proportionality_reading, state_parties_controlling_classification).
narrative_ontology:fixing_cost_class(geneva_conventions_protective_scope__hybrid_proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a graduated legal framework scaling humanitarian protections by conflict type, creating predictability and legal clarity for parties to armed conflict: all parties know that international armed conflicts trigger AP I comprehensive protections, while non-international conflicts operate under Common Article 3 baseline, eliminating the need for conflict-specific negotiation on protection standards for each engagement.
% TRANSFER_FUNCTION: Moves legal clarity, enforcement discretion, and protection status from weaker parties (non-state forces, civilians in ambiguous zones) to stronger parties (state militaries, entities controlling conflict classification). The proportionality calculus permits the stronger party to trade off civilian protection against military advantage in graduated fashion, with minimal external verification. Ambiguity about who qualifies as a protected combatant transfers legal risk from the state to the irregular force.
% ABSENT_VOICES: Non-state armed groups, civilians from conflict zones, and universal human rights advocates would argue for categorical protections irrespective of conflict classification and would reject proportionality as a mechanism for permits reducing civilian protection. They are excluded from the treaty-drafting and state-party interpretation apparatus that determines how the Geneva framework operates in practice.
% DISAPPEARANCE_RATIONALE: If the hybrid proportionality reading vanished and were replaced by either the state_centric_reading (stricter: only uniformed combatants) or universal_rights_reading (broader: all affected persons), the legal status of millions of combatants and civilians would shift, military strategies would change to accommodate new protection floors, and the burden of legal compliance would redistribute across state and non-state parties. States currently benefiting from the ambiguity would lose the discretion to classify their conflicts as non-international when militarily advantageous.
% FOUNDING_PROBLEM: Early Geneva treaties (1949 GC) applied uniformly to international armed conflicts but left non-international conflicts largely unaddressed, creating a protection gap. AP I (1977) and AP II (1977) attempted to scale protections: AP I expanded international conflict protections, AP II created a separate framework for non-international conflicts, and Common Article 3 set a universal baseline. The founding problem was the treaty gap: not all armed conflicts fit the international conflict template, so graduated protection by conflict type was seen as a way to extend humanitarian law coverage without imposing uniform rules on all conflict types.
% FOUNDING_PROBLEM_CORROBORATION: States parties (stronger parties especially) attest the founding problem is live and the graduated approach is necessary to accommodate diverse conflict types. ICRC and humanitarian organizations attest the founding problem is only partially solved: while protection coverage has expanded, the proportionality calculus permits systematic reduction of civilian protection in non-international conflicts, and ambiguous conflicts (hybrid, transnational) still lack clarity. Academic literature and UN bodies document the protection gap has shifted, not closed: universal rights advocates argue the classification-based approach creates more protection uncertainty than it solves.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__hybrid_proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__hybrid_proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__hybrid_proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_protective_scope__hybrid_proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at endpoint) is high because the graduated framework creates legal ambiguity that stronger parties exploit: they classify conflicts downward (as non-international rather than international) to reduce their protection obligations, and proportionality permits them to override civilian protection in graduated fashion. Suppression (0.71) is higher because maintaining the classification ambiguity requires active suppression of alternative readings (especially the universal_rights_reading), and the framework itself suppresses non-state voices in the interpretation process. Theater ratio (0.52 at endpoint, rising from 0.28) indicates an increasing share of the constraint's operation is devoted to interpretive performance (legitimating proportionality calculations, issuing ICRC guidance, conducting internal reviews) rather than implementation of actual protections — the rise reflects institutionalization of protective ambiguity. Accessibility_collapse at the individual level (0.64 at endpoint) is high for trapped parties (unprivileged belligerents, civilians with no exit); organizational-level collapse (0.68) reflects how non-state armed groups cannot exit their legal classification. Resistance peaked in 1977-1990 (when AP II was new and non-state voices advocated for stronger protections) and has declined as stronger parties consolidated their interpretive authority. Stakes_inflation for individuals (0.71 at endpoint) is high because misclassification can result in criminal prosecution rather than POW status; for the state (0.52) it remains moderate because states control the classification process. The one shared time grid ensures measurements are authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The state_parties_controlling_classification and military_establishments_of_stronger_parties seats experience this constraint as genuine coordination: they see the graduated framework as necessary and the proportionality calculus as proportionate. Combatants_in_non_international_conflicts and unprivileged_belligerents see extraction: they experience the classification system as denying them protection status and the proportionality calculus as a legitimation of civilian harm. Civilians experience asymmetry: nominal protection under Common Article 3 combined with practical reduction of that protection via proportionality analysis. The engine computes these per-seat types from the structural data — the state and military establishments have low directionality (beneficiaries), combatants and belligerents have high directionality (targets), civilians are near-symmetric but directionally tilted toward target.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are identified by who benefits from classification discretion and proportionality latitude: state_parties_controlling_classification (d near 0.0 — they set the rules, their legal status is unambiguous, they control interpretation) and military_establishments_of_stronger_parties (d near 0.2 — they benefit from proportionality calculus and can argue unprivileged belligerent status for opponents, but they do face some ICRC scrutiny and potential International Criminal Court investigation). Victims are those who bear the cost of ambiguity and permissive proportionality: combatants_in_non_international_conflicts (d near 0.85 — no POW status, criminal prosecution risk, no voice in classification), unprivileged_belligerents (d near 1.0 — maximum legal risk, no protection status, trapped exit), civilian_populations (d near 0.75 — nominal protection overridden by proportionality, powerless exit, depend on conflict classification made by others). Weaker_parties_and_non_state_armed_groups are excluded by the framework-setting process itself — they would argue differently if in the room.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (treaty gap: international conflicts had protections, non-international conflicts did not) has been partially solved by the graduated framework — AP II and Common Article 3 now provide a floor for non-international conflicts. However, the constraint persists with HIGH extractiveness (0.68) not because the founding problem requires it, but because the proportionality reading permits stronger parties to extract legal discretion from the ambiguity. The theater ratio rising from 0.28 to 0.52 indicates increasingly performative operation: protective review procedures, ICRC guidance, internal legal reviews are conducted but proportionality conclusions systematically favor the stronger party. A non-extractive solution (either state_centric reading: strict combatant immunity rule, or universal_rights_reading: categorical protections irrespective of conflict type) would eliminate the classification ambiguity and reduce the extractiveness significantly. The fact that proportionality remains high suggests mandatrophy: the founding problem is largely solved, but the framework persists because stronger parties benefit from the classification ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conflict_type_classification_ambiguity,
    'Who determines whether a specific armed conflict is international or non-international, and by what criteria? Is the determination made ex ante by states, ex post by international bodies, or continuously renegotiated by the parties?',
    'Systematic analysis of how classification has been made in specific conflicts (Syrian conflict, Yemen, Nagorno-Karabakh, Ukraine) — which party claimed which classification, which international bodies recognized which, and how classification changed over time.',
    'If classification is made by stronger parties ex ante with minimal external review, the extraction mechanism is consolidated and the constraint operates as pure rent-seeking via legal interpretation. If international bodies have genuine authority to reclassify, the constraint''s extractiveness would be substantially lower (d values would shift downward for state beneficiaries).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conflict_type_classification_ambiguity, empirical, 'Whether conflict-type classification is under sole state control or subject to external review.').

omega_variable(
    proportionality_calculus_opaqueness,
    'Is proportionality analysis conducted with transparent criteria (defined threshold of military advantage, disclosed civilian impact calculations) or with opaque military judgment subject to minimal review?',
    'Examination of military proportionality determinations in specific conflicts and international court reviews of those determinations. Count how many proportionality assessments conducted by armed forces have been independently reviewed and reversed by courts or tribunals.',
    'If proportionality is transparent and subject to external review, suppression is lower and theater_ratio lower (genuine protection balance, not performative). If proportionality is opaque and rarely reviewed, suppression and theater are high (the calculation is a cover story for permitting civilian harm).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_calculus_opaqueness, empirical, 'Whether proportionality analysis is transparent and externally reviewed or opaque and self-determined.').

omega_variable(
    universal_rights_reading_foreclosure,
    'Does the hybrid_proportionality_reading logically foreclose the universal_rights_reading (i.e., can a single legal framework hold both graduated-proportionality AND categorical-protection commitments), or do they coexist as live positions held by different parties?',
    'Examine whether any state party or international body has authoritatively integrated both readings into a single coherent legal framework, or whether they remain in persistent tension. Look for judicial or diplomatic attempts to reconcile them.',
    'If the readings logically coexist (different parties hold different readings without foreclosure), the relationship is coexists_with. If one genuinely forecloses the other (holding both is logically impossible), the relationship is forecloses. If the proportionality reading creates pressure that undermines categorical protections (making them harder to enforce), the relationship is influences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_rights_reading_foreclosure, conceptual, 'Logical relationship between scaled-proportionality and universal-protection readings.').

omega_variable(
    state_interpretation_authority_vs_international_oversight,
    'To what extent do states retain sole authority to interpret proportionality and conflict classification, versus International Criminal Court, International Court of Justice, and humanitarian bodies having genuine interpretive authority?',
    'Track decisions where state interpretation was overridden, cases where international bodies imposed different classifications or proportionality findings, and examine consent patterns (do states comply with international determinations they disagree with, or do they maintain unilateral positions?).',
    'If states retain effective sole authority, the extraction mechanism is consolidated (d values for state beneficiaries remain near 0, victim d values remain high). If international bodies have genuine override authority, extraction is modulated and theater ratio is lower (less performative protection machinery). Suppression requirement would decline if international oversight is genuine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_interpretation_authority_vs_international_oversight, empirical, 'Whether state interpretation authority is sole or subject to genuine international override.').

omega_variable(
    reading_committer_identity,
    'Which parties endorse the hybrid_proportionality_reading, which endorse state_centric_reading, and which endorse universal_rights_reading? Do the endorsements align with power/capabilities (stronger parties prefer narrower or scaled readings)?',
    'Map contemporary state positions on conflict classification and proportionality (UN statements, ICRC positions, military manuals, academic influence) to each reading''s core premises. Examine whether stronger militaries cluster on hybrid/state_centric readings while weaker parties and human rights bodies cluster on universal_rights_reading.',
    'This is diagnostic for understanding the reading''s structural position: if stronger parties endorse hybrid/proportionality while weaker parties and advocates endorse universal, the hybrid reading''s extraction mechanism is confirmed (the reading benefits those who control interpretation). If readings are distributed differently, the power dynamics may be less directly tied to the reading choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_committer_identity, empirical, 'Alignment between party endorsement of each reading and their institutional power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__hybrid_proportionality_reading, 1977, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1977, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 1977, 0.28).
narrative_ontology:measurement_basis(gene_tr_t1977, observed).
narrative_ontology:measurement(gene_tr_t1990, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement_basis(gene_tr_t1990, observed).
narrative_ontology:measurement(gene_tr_t2001, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 2001, 0.42).
narrative_ontology:measurement_basis(gene_tr_t2001, observed).
narrative_ontology:measurement(gene_tr_t2011, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 2011, 0.48).
narrative_ontology:measurement_basis(gene_tr_t2011, observed).
narrative_ontology:measurement(gene_tr_t2020, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 2020, 0.51).
narrative_ontology:measurement_basis(gene_tr_t2020, observed).
narrative_ontology:measurement(gene_tr_t2026, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 2026, 0.52).
narrative_ontology:measurement_basis(gene_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(gene_be_t1977, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 1977, 0.45).
narrative_ontology:measurement_basis(gene_be_t1977, observed).
narrative_ontology:measurement(gene_be_t1990, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 1990, 0.52).
narrative_ontology:measurement_basis(gene_be_t1990, observed).
narrative_ontology:measurement(gene_be_t2001, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 2001, 0.61).
narrative_ontology:measurement_basis(gene_be_t2001, observed).
narrative_ontology:measurement(gene_be_t2011, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 2011, 0.65).
narrative_ontology:measurement_basis(gene_be_t2011, observed).
narrative_ontology:measurement(gene_be_t2020, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement_basis(gene_be_t2020, observed).
narrative_ontology:measurement(gene_be_t2026, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(gene_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1977, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 1977, 0.55).
narrative_ontology:measurement_basis(gene_su_t1977, observed).
narrative_ontology:measurement(gene_su_t1990, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 1990, 0.61).
narrative_ontology:measurement_basis(gene_su_t1990, observed).
narrative_ontology:measurement(gene_su_t2001, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 2001, 0.66).
narrative_ontology:measurement_basis(gene_su_t2001, observed).
narrative_ontology:measurement(gene_su_t2011, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 2011, 0.69).
narrative_ontology:measurement_basis(gene_su_t2011, observed).
narrative_ontology:measurement(gene_su_t2020, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 2020, 0.71).
narrative_ontology:measurement_basis(gene_su_t2020, observed).
narrative_ontology:measurement(gene_su_t2026, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 2026, 0.71).
narrative_ontology:measurement_basis(gene_su_t2026, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1977, tn=2026
narrative_ontology:measurement(gene_grid_01, geneva_conventions_protective_scope__hybrid_proportionality_reading, accessibility_collapse(class), 1977, 0.55).
narrative_ontology:measurement(gene_grid_02, geneva_conventions_protective_scope__hybrid_proportionality_reading, accessibility_collapse(class), 2026, 0.62).
narrative_ontology:measurement(gene_grid_03, geneva_conventions_protective_scope__hybrid_proportionality_reading, accessibility_collapse(individual), 1977, 0.48).
narrative_ontology:measurement(gene_grid_04, geneva_conventions_protective_scope__hybrid_proportionality_reading, accessibility_collapse(individual), 2026, 0.64).
narrative_ontology:measurement(gene_grid_05, geneva_conventions_protective_scope__hybrid_proportionality_reading, accessibility_collapse(organizational), 1977, 0.52).
narrative_ontology:measurement(gene_grid_06, geneva_conventions_protective_scope__hybrid_proportionality_reading, accessibility_collapse(organizational), 2026, 0.68).
narrative_ontology:measurement(gene_grid_07, geneva_conventions_protective_scope__hybrid_proportionality_reading, accessibility_collapse(structural), 1977, 0.58).
narrative_ontology:measurement(gene_grid_08, geneva_conventions_protective_scope__hybrid_proportionality_reading, accessibility_collapse(structural), 2026, 0.58).
narrative_ontology:measurement(gene_grid_09, geneva_conventions_protective_scope__hybrid_proportionality_reading, resistance(class), 1977, 0.64).
narrative_ontology:measurement(gene_grid_10, geneva_conventions_protective_scope__hybrid_proportionality_reading, resistance(class), 2026, 0.52).
narrative_ontology:measurement(gene_grid_11, geneva_conventions_protective_scope__hybrid_proportionality_reading, resistance(individual), 1977, 0.62).
narrative_ontology:measurement(gene_grid_12, geneva_conventions_protective_scope__hybrid_proportionality_reading, resistance(individual), 2026, 0.48).
narrative_ontology:measurement(gene_grid_13, geneva_conventions_protective_scope__hybrid_proportionality_reading, resistance(organizational), 1977, 0.68).
narrative_ontology:measurement(gene_grid_14, geneva_conventions_protective_scope__hybrid_proportionality_reading, resistance(organizational), 2026, 0.55).
narrative_ontology:measurement(gene_grid_15, geneva_conventions_protective_scope__hybrid_proportionality_reading, resistance(structural), 1977, 0.55).
narrative_ontology:measurement(gene_grid_16, geneva_conventions_protective_scope__hybrid_proportionality_reading, resistance(structural), 2026, 0.58).
narrative_ontology:measurement(gene_grid_17, geneva_conventions_protective_scope__hybrid_proportionality_reading, stakes_inflation(class), 1977, 0.45).
narrative_ontology:measurement(gene_grid_18, geneva_conventions_protective_scope__hybrid_proportionality_reading, stakes_inflation(class), 2026, 0.68).
narrative_ontology:measurement(gene_grid_19, geneva_conventions_protective_scope__hybrid_proportionality_reading, stakes_inflation(individual), 1977, 0.42).
narrative_ontology:measurement(gene_grid_20, geneva_conventions_protective_scope__hybrid_proportionality_reading, stakes_inflation(individual), 2026, 0.71).
narrative_ontology:measurement(gene_grid_21, geneva_conventions_protective_scope__hybrid_proportionality_reading, stakes_inflation(organizational), 1977, 0.38).
narrative_ontology:measurement(gene_grid_22, geneva_conventions_protective_scope__hybrid_proportionality_reading, stakes_inflation(organizational), 2026, 0.55).
narrative_ontology:measurement(gene_grid_23, geneva_conventions_protective_scope__hybrid_proportionality_reading, stakes_inflation(structural), 1977, 0.5).
narrative_ontology:measurement(gene_grid_24, geneva_conventions_protective_scope__hybrid_proportionality_reading, stakes_inflation(structural), 2026, 0.52).
narrative_ontology:measurement(gene_grid_25, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression(class), 1977, 0.58).
narrative_ontology:measurement(gene_grid_26, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression(class), 2026, 0.72).
narrative_ontology:measurement(gene_grid_27, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression(individual), 1977, 0.48).
narrative_ontology:measurement(gene_grid_28, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression(individual), 2026, 0.74).
narrative_ontology:measurement(gene_grid_29, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression(organizational), 1977, 0.61).
narrative_ontology:measurement(gene_grid_30, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression(organizational), 2026, 0.76).
narrative_ontology:measurement(gene_grid_31, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression(structural), 1977, 0.55).
narrative_ontology:measurement(gene_grid_32, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression(structural), 2026, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__hybrid_proportionality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.12).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope__state_centric_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope__universal_rights_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, international_criminal_court_jurisdiction__mode_of_warfare).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, distinction_principle__civilian_targeting).

% DUAL FORMULATION NOTE:
% The geneva_conventions_protective_scope kernel decomposes into three constraint stories: hybrid_proportionality_reading (this one), state_centric_reading, and universal_rights_reading. Each reading instantiates a structurally distinct constraint with different ε (extraction magnitude), different victim/beneficiary sets, different directionality profiles, and different typology. The readings coexist in contemporary legal and military practice — no single reading has foreclosed the others. The hybrid_proportionality_reading (this story) sits downstream of the kernel's codification and upstream of its application in specific conflicts; it influences the state_centric and universal_rights readings by establishing the interpretive baseline that those readings either narrow (state-centric) or expand (universal) from.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
