% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__contraction_reading, []).

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
 *   constraint_id: honor_violence_legitimacy__contraction_reading
 *   human_readable: Honor Redefinition Excluding Violence (Contraction Reading)
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   This constraint story captures the contraction_reading of the
 *   honor_violence_legitimacy kernel: the claim that dueling became
 *   structurally unthinkable because honor itself was conceptually redefined
 *   to exclude violence. The reading asserts a genuine conceptual contraction
 *   — the semantic space of 'honor' shrank so that violence no longer fit
 *   inside it — rather than merely external pressures making dueling costly.
 *   This is one of three contested readings of the same historical kernel;
 *   the drop_reading attributes dueling's decline to external costs (legal,
 *   social), while the composite_reading sees both mechanisms operating
 *   simultaneously. This story authors only the contraction_reading as a
 *   clean ε-invariant constraint, with the kernel contest routed to omega
 *   variables and cs_structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__contraction_reading, 0.68).
domain_priors:suppression_score(honor_violence_legitimacy__contraction_reading, 0.72).
domain_priors:theater_ratio(honor_violence_legitimacy__contraction_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__contraction_reading, tangled_rope).
narrative_ontology:human_readable(honor_violence_legitimacy__contraction_reading, "Honor Redefinition Excluding Violence (Contraction Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__contraction_reading, "historical_sociology/legal_anthropology/commitment_systems").

domain_priors:requires_active_enforcement(honor_violence_legitimacy__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__contraction_reading, '0918b07f-8507-4dcc-9304-8b9eb40515a0').
narrative_ontology:cs_kernel_codification('0918b07f-8507-4dcc-9304-8b9eb40515a0', fixed_text).
narrative_ontology:cs_authority_grounding('0918b07f-8507-4dcc-9304-8b9eb40515a0', lineage).
narrative_ontology:cs_interpretation_layer_present('0918b07f-8507-4dcc-9304-8b9eb40515a0').
narrative_ontology:cs_reading_relation('0918b07f-8507-4dcc-9304-8b9eb40515a0', honor_violence_legitimacy__drop_reading, forecloses).
narrative_ontology:cs_reading_relation('0918b07f-8507-4dcc-9304-8b9eb40515a0', honor_violence_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('0918b07f-8507-4dcc-9304-8b9eb40515a0', foundational, honor_excludes_violence_by_definition).
narrative_ontology:cs_axiom_status(honor_excludes_violence_by_definition, holdable).
narrative_ontology:cs_axiom_grounding('0918b07f-8507-4dcc-9304-8b9eb40515a0', honor_excludes_violence_by_definition, deontological).
narrative_ontology:cs_axiom('0918b07f-8507-4dcc-9304-8b9eb40515a0', secondary, dueling_structurally_unthinkable_under_modern_honor).
narrative_ontology:cs_axiom_status(dueling_structurally_unthinkable_under_modern_honor, holdable).
narrative_ontology:cs_axiom_grounding('0918b07f-8507-4dcc-9304-8b9eb40515a0', dueling_structurally_unthinkable_under_modern_honor, conventional).
narrative_ontology:cs_reference_frame('0918b07f-8507-4dcc-9304-8b9eb40515a0', aristocratic_violence_honor).
narrative_ontology:cs_drift_state('0918b07f-8507-4dcc-9304-8b9eb40515a0', bourgeois_reputation_honor_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0918b07f-8507-4dcc-9304-8b9eb40515a0', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, state_legal_authorities).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, moral_religious_reformers).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, bourgeois_commercial_classes).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, aristocratic_duelists).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, military_officer_corps).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, military_officer_corps).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__contraction_reading, state_monopoly_on_legitimate_violence).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__contraction_reading, christian_pacifist_ethic).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__contraction_reading, commercial_reputation_as_honor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Aristocratic men for whom dueling was the legitimate response to honor insults. As honor is redefined to exclude violence, their entire honor-practice becomes illegitimate. Their identity is fused with the duelist code — exit means abandoning their self-conception as men of honor. They resist through secret duels, code evolution, and ideological defense of 'true honor.'
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, aristocratic_duelists, payer,
    powerful, biographical, identity_locked, continental).

% Courts, police, and legislatures that criminalize dueling and prosecute participants. They benefit from consolidating the state's monopoly on legitimate violence. They administer the legal constraint, set penalties, and control enforcement intensity. Their exit is arbitrage-grade — they can adjust enforcement up or down without personal cost.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, state_legal_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Clergy, pietist movements, and moral philosophers who campaign against dueling as unchristian and barbaric. They benefit when honor is redefined to align with pacifist ethics — their moral authority expands. They can exit the debate by shifting focus to other reforms; their position is not identity-fused.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, moral_religious_reformers, beneficiary,
    organized, generational, mobile, continental).

% Merchants, professionals, and officials whose honor conception centers on commercial reputation, contractual reliability, and civic standing. They benefit when aristocratic violence-honor is displaced by bourgeois reputation-honor — their status system becomes dominant. They have mobile exit: they never participated in dueling culture and can ignore it.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, bourgeois_commercial_classes, beneficiary,
    organized, generational, mobile, continental).

% Officers for whom dueling persisted longest as a professional honor code. They bear costs as the redefinition makes their traditional practice illegal and dishonorable, yet they also benefit from the new honor conception (professional competence, state service) that replaces it. Their exit is constrained by institutional loyalty and professional identity.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, military_officer_corps, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__contraction_reading, military_officer_corps, beneficiary).

% Wives, mothers, sisters of duelists who bear the consequences — widowhood, family ruin, social stigma — but have no voice in honor codes or dueling decisions. They would object to the violence but are structurally excluded from the male honor discourse. Their exit is trapped by patriarchal family structure.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, women_aristocratic_families, excluded,
    powerless, biographical, trapped, local).

% Analytical seat examining the constraint from outside the historical moment. Sees the full structural transformation: how conceptual redefinition, legal suppression, and class interest alignment jointly made dueling unthinkable. Neither collects nor pays; provides the classification frame.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, historical_sociologist_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The honor system coordinated aristocratic male status competition by channeling violence into ritualized, rule-bound duels — preventing feud escalation, providing clear status resolution, and maintaining class boundaries against bourgeois incursion.
% TRANSFER_FUNCTION: Moves the legitimate right to use violence in status disputes from aristocratic individuals (who lose this right) to the state (which claims monopoly) and to moral/commercial norms (which redefine honor as non-violent). The aristocratic class pays the cost of disarmament; state and bourgeois norms capture the legitimating authority.
% ABSENT_VOICES: Women of the aristocratic families who suffered dueling's consequences without participation; commoners who were never part of the honor system but were subject to its spillover violence; colonized peoples for whom European honor violence was exported as imperial practice. They are absent because the honor discourse was explicitly male, aristocratic, and European.
% DISAPPEARANCE_RATIONALE: If the honor redefinition constraint vanished overnight — if violence were suddenly readmitted as legitimate honor-response — aristocratic dueling would not automatically return (material conditions changed), but the conceptual barrier would collapse. Contemporary honor disputes (gang violence, honor killings, online reputation defense) would gain new legitimating vocabulary. The state's monopoly on violence would face renewed ideological challenge.
% FOUNDING_PROBLEM: The aristocratic honor system's founding problem was regulating status competition among men who claimed the right to private violence — preventing uncontrolled feuding while preserving aristocratic autonomy from state interference.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is dead: the aristocratic class that needed this regulation has lost its political autonomy and its monopoly on status definition. State monopoly on violence is now established; bourgeois reputation-honor has displaced aristocratic violence-honor. Corroborated by historical consensus (Elias, Frevert, Kiernan, McAleer) — not by the benefiting parties (state/bourgeoisie) alone but by scholars across political traditions who agree the aristocratic honor system's functional context has vanished.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_violence_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__contraction_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__contraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_violence_legitimacy__contraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_violence_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is substantial because the redefinition extracts the legitimate violence option from aristocrats without their consent, transferring legitimating authority to state and bourgeois norms. Suppression (0.72) is high because the constraint's persistence depends on both legal enforcement (dueling bans) and internalized moral suppression (violence experienced as dishonorable). Theater ratio (0.41) is moderate: the honor system's performative aspects (dueling rituals, codes of honor) persist in literature and military culture but are decoupled from actual violence. Accessibility collapse (0.78) is high — once honor excludes violence, dueling becomes conceptually inaccessible as a legitimate response, not just legally risky. Resistance (0.55) is significant: aristocrats resisted through secret duels, evolved codes, and ideological defenses into the late 19th century. The claimed type is tangled_rope because the redefinition has a genuine coordination function (channeling status competition into non-violent channels, aligning with state monopoly) AND asymmetric extraction (aristocrats lose their distinctive status-practice; state and bourgeoisie gain legitimating authority).
 *
 * PERSPECTIVAL GAP:
 *   The aristocratic duelist seat experiences this as a snare — their identity-defining practice is extracted from them by a redefinition they did not choose. The state/bourgeois seats experience it as a rope — a coordination solution that channels conflict into law and commerce. The military officer seat experiences it as a tangled_rope — they lose dueling but gain professional honor. The engine computes this divergence from the structural data; the authored claim (tangled_rope) reflects the analytical seat's view of the overall structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Aristocratic duelists are full targets (d near 1.0): identity-locked, bear the full extraction of their legitimate violence right. State legal authorities are full beneficiaries (d near 0.0): they collect the monopoly on violence, control enforcement. Moral reformers and bourgeois classes are beneficiaries (d ~0.2): they gain moral/cultural authority without bearing enforcement costs. Military officers are partial targets (d ~0.6): constrained exit, dual position as both payers (lose dueling) and beneficiaries (gain professional honor). Women/families are excluded (no directionality computed): trapped, no voice. The analytical observer sits at d=0.5 (symmetric).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (regulating aristocratic private violence) is dead — the aristocratic class and its autonomy are gone. Yet the constraint persists: the conceptual exclusion of violence from honor remains the dominant framework. This is mandatrophy — the mandate (honor redefinition) has outlived its founding function. The constraint is not a piton because it still has active beneficiaries (state, bourgeois norms) who maintain it; it is not a scaffold because it lacks a sunset clause and its justification is treated as permanent moral progress rather than transitional. The tangled_rope classification captures this: genuine coordination (non-violent status competition) persists alongside extraction (aristocratic disempowerment), and the constraint requires active enforcement (legal + moral) to hold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contraction_vs_drop_primacy,
    'Was dueling''s decline driven primarily by conceptual redefinition of honor (contraction) or by external practical costs (drop), or are they inseparable (composite)?',
    'Counterfactual historical analysis: if legal penalties were removed but honor conception stayed violence-exclusive, would dueling return? If honor conception stayed violence-inclusive but legal penalties remained, would dueling persist underground? Comparative study of societies where one factor changed without the other.',
    'If contraction is primary, the constraint is a genuine conceptual transformation (tangled_rope with high accessibility_collapse). If drop is primary, the constraint is a practical suppression with ideological cover (snare). If composite, the constraint family structure is validated — each reading captures a real component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contraction_vs_drop_primacy, empirical, 'Primary causal mechanism of dueling''s decline: conceptual vs. practical.').

omega_variable(
    honor_redefinition_agency,
    'Was the honor redefinition driven by genuine moral evolution (internal to the honor discourse) or by external power interests (state/bourgeoisie) using moral language?',
    'Intellectual history of honor discourse: trace whether anti-dueling arguments emerged from within aristocratic honor codes (e.g., Christian honor, enlightened honor) or were imposed from outside. Analyze who authored the new honor definitions and whose interests they served.',
    'If internal moral evolution, the constraint is more rope-like (genuine coordination). If external power imposition, more snare-like (extraction with coordination cover). This directly affects extractiveness assessment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(honor_redefinition_agency, conceptual, 'Agency and motivation behind the honor redefinition.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal bans, state enforcement) or internalized (moral redefinition making violence feel dishonorable), and what is the proportion of each?',
    'Post-legalization suppression trajectory: in jurisdictions where dueling bans were lifted (or never enforced), did dueling return? If suppression persists after legal barriers removal, the internalized component is dominant. Historical comparison of regions with different legal/moral trajectories.',
    'If internalized suppression dominates, the constraint''s effective suppression is higher than legal measures suggest — the target carries the suppression internally. This affects classification: internalized suppression with high accessibility_collapse suggests mountain-like naturalization of the redefinition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the honor redefinition.').

omega_variable(
    kernel_contest_structure,
    'Do the three kernel readings (contraction, drop, composite) represent genuinely distinct structural constraints, or are they observational perspectives on a single constraint?',
    'Apply ε-invariance test: does each reading author a stable ε for its claimed referent? If changing the reading changes ε, they are distinct constraints (constraint family). If ε is stable across readings, they are perspectives on one constraint.',
    'If distinct constraints, the family structure is validated and each reading gets independent classification. If one constraint, the kernel framing is analytical overlay and the contest is perspectival, not structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_structure, conceptual, 'Whether the kernel readings instantiate distinct constraints per ε-invariance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__contraction_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(honor_violence_legitimacy__contraction_reading_tr_t0, honor_violence_legitimacy__contraction_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(honor_violence_legitimacy__contraction_reading_tr_t30, honor_violence_legitimacy__contraction_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(honor_violence_legitimacy__contraction_reading_tr_t60, honor_violence_legitimacy__contraction_reading, theater_ratio, 60, 0.31).
narrative_ontology:measurement(honor_violence_legitimacy__contraction_reading_tr_t90, honor_violence_legitimacy__contraction_reading, theater_ratio, 90, 0.38).
narrative_ontology:measurement(honor_violence_legitimacy__contraction_reading_tr_t120, honor_violence_legitimacy__contraction_reading, theater_ratio, 120, 0.41).
narrative_ontology:measurement(honor_violence_legitimacy__contraction_reading_tr_t150, honor_violence_legitimacy__contraction_reading, theater_ratio, 150, 0.41).

% Extraction over time
narrative_ontology:measurement(honor_violence_legitimacy__contraction_reading_be_t0, honor_violence_legitimacy__contraction_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(honor_violence_legitimacy__contraction_reading_be_t30, honor_violence_legitimacy__contraction_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(honor_violence_legitimacy__contraction_reading_be_t60, honor_violence_legitimacy__contraction_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(honor_violence_legitimacy__contraction_reading_be_t90, honor_violence_legitimacy__contraction_reading, base_extractiveness, 90, 0.65).
narrative_ontology:measurement(honor_violence_legitimacy__contraction_reading_be_t120, honor_violence_legitimacy__contraction_reading, base_extractiveness, 120, 0.68).
narrative_ontology:measurement(honor_violence_legitimacy__contraction_reading_be_t150, honor_violence_legitimacy__contraction_reading, base_extractiveness, 150, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(honor_violence_legitimacy__contraction_reading_su_t0, honor_violence_legitimacy__contraction_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(honor_violence_legitimacy__contraction_reading_su_t30, honor_violence_legitimacy__contraction_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(honor_violence_legitimacy__contraction_reading_su_t60, honor_violence_legitimacy__contraction_reading, suppression_requirement, 60, 0.65).
narrative_ontology:measurement(honor_violence_legitimacy__contraction_reading_su_t90, honor_violence_legitimacy__contraction_reading, suppression_requirement, 90, 0.7).
narrative_ontology:measurement(honor_violence_legitimacy__contraction_reading_su_t120, honor_violence_legitimacy__contraction_reading, suppression_requirement, 120, 0.72).
narrative_ontology:measurement(honor_violence_legitimacy__contraction_reading_su_t150, honor_violence_legitimacy__contraction_reading, suppression_requirement, 150, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__contraction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_violence_legitimacy__contraction_reading, 0.08).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, state_monopoly_violence_consolidation).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, bourgeois_reputation_honor_formation).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, military_professional_honor_codification).

% DUAL FORMULATION NOTE:
% This constraint (contraction_reading) is one member of the honor_violence_legitimacy constraint family. The drop_reading and composite_reading are sibling constraints with different ε values and stakeholder structures. The contraction_reading has higher accessibility_collapse (conceptual barrier) and lower resistance (internalized suppression) than the drop_reading would have. The composite_reading would show intermediate metrics. All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_violence_legitimacy__contraction_reading, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
