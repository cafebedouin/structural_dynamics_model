% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__inherent_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__inherent_right_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: article_9_war_renunciation__inherent_right_reading
 *   human_readable: Article 9 Inherent Right to Minimum Necessary Self-Defense
 *   domain: constitutional law/security policy
 *
 * SUMMARY:
 *   This constraint instantiates the 'inherent right' reading of Article 9 of
 *   the Japanese Constitution: the claim that sovereign states retain an
 *   inherent right to self-defense, that Article 9 renounces aggressive war
 *   but not minimum necessary defensive capacity, and that the Self-Defense
 *   Forces are constitutionally legitimate within that proportionality
 *   threshold. It is one of three structurally distinct constraints
 *   decomposed from the Article 9 kernel, alongside the strict pacifist
 *   reading (categorical prohibition) and the collective self-defense reading
 *   (extension to allied defense). This reading functions as a threshold
 *   constraint rather than a prohibition, permitting the SDF while limiting
 *   its scope. The generated metrics describe the reading's actual operation:
 *   extraction rises over time as the 'minimum necessary' standard stretches,
 *   suppression intensifies to maintain the reading against both pacifist
 *   textualism and hawkish expansion, and theater grows as reinterpretation
 *   becomes increasingly performative. The claimed type is tangled_rope
 *   because the reading coordinates a genuine middle-ground security posture
 *   while asymmetrically constraining both pacifist abolitionists and
 *   military normalization advocates.
 *
 * KEY AGENTS:
 *   - japanese_government: Agenda-setter (institutional/constrained) â administers the interpretation
 *   - sdf: Primary beneficiary (organized/identity_locked) â gains legitimacy but is scope-limited
 *   - japanese_public: Diffuse beneficiary (organized/constrained) â receives security without full remilitarization
 *   - us_alliance: Secondary beneficiary (institutional/analytical) â gains predictable alliance contribution
 *   - security_hawks: Primary payer (moderate/constrained) â blocked from full normalization
 *   - pacifist_constitutionalists: Secondary payer (moderate/identity_locked) â SDF legitimized against their textual reading
 *   - constitutional_scholars: Analytical observer (analytical/analytical) â supplies interpretive framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__inherent_right_reading, 0.65).
domain_priors:suppression_score(article_9_war_renunciation__inherent_right_reading, 0.7).
domain_priors:theater_ratio(article_9_war_renunciation__inherent_right_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__inherent_right_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__inherent_right_reading, "Article 9 Inherent Right to Minimum Necessary Self-Defense").
narrative_ontology:topic_domain(article_9_war_renunciation__inherent_right_reading, "constitutional law/security policy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__inherent_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__inherent_right_reading, 'c79708a9-5fc4-4cd4-ae2c-bab9965fa382').
narrative_ontology:cs_kernel_codification('c79708a9-5fc4-4cd4-ae2c-bab9965fa382', fixed_text).
narrative_ontology:cs_authority_grounding('c79708a9-5fc4-4cd4-ae2c-bab9965fa382', lineage).
narrative_ontology:cs_interpretation_layer_present('c79708a9-5fc4-4cd4-ae2c-bab9965fa382').
narrative_ontology:cs_reading_relation('c79708a9-5fc4-4cd4-ae2c-bab9965fa382', article_9_war_renunciation__strict_pacifist_reading, forecloses).
narrative_ontology:cs_reading_relation('c79708a9-5fc4-4cd4-ae2c-bab9965fa382', article_9_war_renunciation__collective_self_defense_reading, influences).
narrative_ontology:cs_axiom('c79708a9-5fc4-4cd4-ae2c-bab9965fa382', foundational, inherent_self_defense_right_preserved).
narrative_ontology:cs_axiom_status(inherent_self_defense_right_preserved, holdable).
narrative_ontology:cs_axiom_grounding('c79708a9-5fc4-4cd4-ae2c-bab9965fa382', inherent_self_defense_right_preserved, conventional).
narrative_ontology:cs_axiom('c79708a9-5fc4-4cd4-ae2c-bab9965fa382', foundational, minimum_necessary_proportionality).
narrative_ontology:cs_axiom_status(minimum_necessary_proportionality, holdable).
narrative_ontology:cs_axiom_grounding('c79708a9-5fc4-4cd4-ae2c-bab9965fa382', minimum_necessary_proportionality, conventional).
narrative_ontology:cs_reference_frame('c79708a9-5fc4-4cd4-ae2c-bab9965fa382', postwar_pacifist_constitutional_order).
narrative_ontology:cs_drift_state('c79708a9-5fc4-4cd4-ae2c-bab9965fa382', contemporary_security_expansion, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c79708a9-5fc4-4cd4-ae2c-bab9965fa382', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, japanese_government).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, sdf).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, japanese_public).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, us_alliance).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, security_hawks).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, pacifist_constitutionalists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the constitutional interpretation through the Cabinet Legislation Bureau and defense guidelines. Seeks to expand operational scope while maintaining the 'minimum necessary' constitutional facade. Bears political and legal costs when reinterpretation triggers constitutional crises or mass protest.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, japanese_government, agenda_setter,
    institutional, generational, constrained, national).

% Gains constitutional legitimacy and budgetary authorization under this reading, but is strictly confined to territorial defense and disaster relief. Organizational identity and career paths depend on the reading's continued validity; cannot pursue offensive capability or full military normalization without existential legal risk.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, sdf, beneficiary,
    organized, biographical, identity_locked, national).

% Receives territorial defense and disaster response without full remilitarization or constitutional instability. Bears fiscal costs and the moral hazard of gradual militarization accepted under constitutional cover. Exit is constrained by the lack of viable political alternatives that avoid both abolition and expansion.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, japanese_public, beneficiary,
    organized, biographical, constrained, national).

% Benefits from a predictable, scope-limited Japanese defense contribution within the alliance framework. Avoids the strategic unpredictability of full Japanese military normalization or the burden of total Japanese demilitarization. The reading stabilizes burden-sharing expectations.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, us_alliance, beneficiary,
    institutional, generational, analytical, global).

% Advocate for constitutional revision, full military normalization, and unrestricted collective self-defense. Their agenda is blocked by the 'minimum necessary' threshold and the war-renunciation language, forcing them to pursue incremental reinterpretation rather than open constitutional amendment.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, security_hawks, payer,
    moderate, biographical, constrained, national).

% Hold that Article 9's text categorically prohibits any military force. This reading legitimizes the SDF's existence, which they view as unconstitutional and a betrayal of the constitutional text. Their political identity is fused with absolute pacifism; exit from the constraint means abandoning a core normative commitment.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, pacifist_constitutionalists, payer,
    moderate, biographical, identity_locked, national).

% Supply the interpretive frameworks that sustain or challenge the reading. They analyze textual history, Diet debates, and comparative law. Their classifications do not determine institutional outcomes but shape the vocabulary in which the constraint is defended or attacked.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

narrative_ontology:fixing_cost_class(article_9_war_renunciation__inherent_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Japan's postwar security posture between absolute pacifism and full remilitarization by permitting only military force deemed necessary for territorial self-defense, reconciling constitutional text with sovereign survival and alliance commitments.
% TRANSFER_FUNCTION: Transfers constitutional legitimacy, budgetary authorization, and administrative discretion to the Japanese government and SDF; transfers political and legal constraint to expansionists who want full normalization and to pacifists who want abolition.
% ABSENT_VOICES: Absolute pacifists rejecting any military force are marginalized in official interpretation but present in civil society; security hawks seeking unilateral remilitarization are present in political discourse but legally constrained. Both are in the conversation but neither determines the authoritative reading.
% DISAPPEARANCE_RATIONALE: Without this reading, the SDF loses constitutional legitimacy, forcing an immediate choice between constitutional amendment, abolition, or overt constitutional violation. The US alliance's operating assumptions, defense budgets, and the postwar domestic political order all depend on this intermediate interpretive settlement.
% FOUNDING_PROBLEM: The 1947 Constitution's text appeared to prohibit all military force, but Cold War and post-occupation realities required a security capacity; the government needed to reconcile constitutional text with existential defense needs without formal amendment.
% FOUNDING_PROBLEM_CORROBORATION: US occupation and diplomatic archives attest to the original security vacuum and American encouragement of rearmament; independent constitutional historians attest to the textual contradiction requiring interpretation; pacifist civic groups attest the problem was manufactured to legitimate militarization rather than genuine.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__inherent_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__inherent_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__inherent_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_9_war_renunciation__inherent_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__inherent_right_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__inherent_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_9_war_renunciation__inherent_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_9_war_renunciation__inherent_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects that the reading extracts political constraint from both flanks: pacifists lose the textual prohibition, hawks lose full military sovereignty. Suppression (0.70) is high because the reading's persistence requires active institutional enforcement (Cabinet Legislation Bureau reinterpretations, Diet security legislation) against both strict textualism and expansionist lobbying. Theater ratio (0.50) captures the growing gap between the 'minimum necessary territorial defense' frame and actual practice, which since 2015 includes collective self-defense and power-projection enabling legislation. Accessibility collapse (0.60) is moderate-high: once inside this interpretive frame, both absolute pacifism and unilateral remilitarization appear legally unreachable. Resistance (0.75) is high and bidirectional, coming from pacifist civil society and revisionist political factions. Measurements share one time grid (0â70) to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (government) experiences the constraint as a source of administrative authority and interpretive flexibility; the engine should compute a moderate directionality. The beneficiary seats (SDF, public, alliance) experience low directionality â the constraint subsidizes their security and stability. The payer seats (hawks, pacifists) experience high directionality: the constraint structurally targets their political agendas. The pacifist seat is identity_locked, amplifying effective extraction relative to the hawk seat's merely constrained exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (government, SDF, public, US alliance) feed low directionality. Victim declarations (security hawks, pacifist constitutionalists) feed high directionality. The pacifists' identity_locked exit amplifies their effective extraction. The government's dual position as agenda-setter and constrained actor produces a derived d near 0.4 â partly beneficiary (authority to interpret), partly target (cannot fully remilitarize without abandoning the reading).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling in both directions. Against strict pacifism, it identifies that the constraint is not a Mountain of absolute textual prohibition (the text is contested, beneficiaries exist, enforcement is active). Against hawkish normalization, it identifies that the constraint is not a pure Snare: the SDF's legitimacy and the public's security are genuine coordination products, not cover stories. The tangled_rope classification captures that the same structure simultaneously coordinates (security within constitutional bounds) and extracts (political constraint from partisans of both extremes).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inherent_right_grounding,
    'Is the ''inherent right'' to self-defense a pre-constitutional natural law feature preserved by Article 9, or a post-hoc juridical construction to legitimate the SDF?',
    'Archival analysis of Diet debates 1947-1954, occupation records, and comparative constitutional review of whether unamended texts can preserve pre-existing rights not explicitly named.',
    'If constructed, the reading is a tangled rope or snare extracting legitimacy for militarization; if natural, it trends toward a constitutional mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inherent_right_grounding, conceptual, 'Natural law vs constructed origin of the inherent right').

omega_variable(
    minimum_necessary_scope,
    'Does the ''minimum necessary'' standard for defensive capacity possess a principled limiting function, or does it functionally ratchet toward expansion?',
    'Track SDF budget, equipment acquisitions, and rules of engagement against strictly territorial defense needs over the interval; compare to threat environment.',
    'If it ratchets, the reading has scaffolded into a snare-like extraction of constitutional legitimacy for expansion; if it holds, the coordination function remains genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minimum_necessary_scope, empirical, 'Whether the proportionality threshold limits or enables expansion').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of strict pacifist and expansionist alternatives enforced through structural legal barriers or internalized political consensus?',
    'Observe whether Article 9 reinterpretation triggers institutional resistance and litigation (structural) or public acquiescence driven by threat perception and political culture (internalized).',
    'If internalized, effective suppression exceeds the structural measure; the constraint operates through political culture as well as law, amplifying extraction for identity-locked agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__inherent_right_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_9_war_renunciation__inherent_right_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(arti_tr_t15, article_9_war_renunciation__inherent_right_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(arti_tr_t30, article_9_war_renunciation__inherent_right_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(arti_tr_t45, article_9_war_renunciation__inherent_right_reading, theater_ratio, 45, 0.4).
narrative_ontology:measurement(arti_tr_t60, article_9_war_renunciation__inherent_right_reading, theater_ratio, 60, 0.45).
narrative_ontology:measurement(arti_tr_t70, article_9_war_renunciation__inherent_right_reading, theater_ratio, 70, 0.5).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(arti_be_t15, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(arti_be_t30, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(arti_be_t45, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 45, 0.55).
narrative_ontology:measurement(arti_be_t60, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 60, 0.6).
narrative_ontology:measurement(arti_be_t70, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 70, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(arti_su_t15, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 15, 0.45).
narrative_ontology:measurement(arti_su_t30, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement(arti_su_t45, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 45, 0.55).
narrative_ontology:measurement(arti_su_t60, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 60, 0.65).
narrative_ontology:measurement(arti_su_t70, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 70, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, strict_pacifist_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, collective_self_defense_reading).

% DUAL FORMULATION NOTE:
% The Article 9 kernel decomposes into three structurally distinct constraints: strict_pacifist_reading (Mountain/Snare depending on enforcement seat), inherent_right_reading (Tangled Rope, this file), and collective_self_defense_reading (Scaffold or Tangled Rope). The inherent right reading is upstream of the collective self-defense reading, which extends its foundational premise.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
