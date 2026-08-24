% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__icrc_customary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__icrc_customary_reading, []).

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
 *   constraint_id: common_article_3_scope__icrc_customary_reading
 *   human_readable: CA3 Scope via Customary International Law Evolution (ICRC Reading)
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   Common Article 3 of the 1949 Geneva Conventions applies to 'armed
 *   conflict not of an international character' but leaves the threshold
 *   undefined. The ICRC customary reading asserts that CA3 scope is
 *   determined not by fixed intensity/organization thresholds but by evolving
 *   state practice and opinio juris tracked through customary international
 *   law formation. This reading, articulated prominently in the ICRC's 2005
 *   Customary IHL Study (Rule 139), treats the customary process as a
 *   coordination mechanism: states, courts, and the ICRC collectively update
 *   the scope of CA3 through practice without requiring treaty amendment. The
 *   constraint is procedural — it governs *how* scope is determined — rather
 *   than substantive. It solves a genuine coordination problem: how to adapt
 *   a 1949 treaty provision to conflict forms (e.g., transnational
 *   non-international armed conflicts, counter-terrorism operations) the
 *   drafters did not anticipate. The ICRC acts as the primary agenda-setter,
 *   tracking and articulating practice; international courts (ICTY, ICC,
 *   regional courts) adopt and legitimize the customary rules; states
 *   participate in practice formation but lose predictability. The claimed
 *   type is rope: a coordination mechanism with minimal coercion where
 *   participants (ICRC, courts, many states) are net beneficiaries.
 *   Extraction is low but non-zero: states seeking legal certainty bear
 *   interpretive instability; non-state armed groups face expanding
 *   obligations without reciprocal participation in the customary process.
 *
 * KEY AGENTS:
 *   - icrc: Primary agenda-setter (institutional/analytical) — tracks, articulates, and promotes customary rules; collects institutional authority
 *   - international_courts: Primary beneficiary (institutional/analytical) — gain interpretive flexibility and legitimizing authority from customary rules
 *   - states_seeking_certainty: Payer (institutional/constrained) — lose predictable thresholds; practice binds them without explicit consent
 *   - non_state_armed_groups: Payer (organized/constrained) — face expanding obligations without voice in customary formation
 *   - humanitarian_actors: Beneficiary (organized/mobile) — gain protective framework adaptable to new conflict forms
 *   - legal_scholars: Observer (analytical/analytical) — analyze and critique the customary process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__icrc_customary_reading, 0.28).
domain_priors:suppression_score(common_article_3_scope__icrc_customary_reading, 0.15).
domain_priors:theater_ratio(common_article_3_scope__icrc_customary_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__icrc_customary_reading, rope).
narrative_ontology:human_readable(common_article_3_scope__icrc_customary_reading, "CA3 Scope via Customary International Law Evolution (ICRC Reading)").
narrative_ontology:topic_domain(common_article_3_scope__icrc_customary_reading, "international_humanitarian_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__icrc_customary_reading, '1b709339-fe06-4986-84b9-4031f0ddd512').
narrative_ontology:cs_kernel_codification('1b709339-fe06-4986-84b9-4031f0ddd512', fixed_text).
narrative_ontology:cs_authority_grounding('1b709339-fe06-4986-84b9-4031f0ddd512', practice).
narrative_ontology:cs_interpretation_layer_present('1b709339-fe06-4986-84b9-4031f0ddd512').
narrative_ontology:cs_reading_relation('1b709339-fe06-4986-84b9-4031f0ddd512', common_article_3_scope__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('1b709339-fe06-4986-84b9-4031f0ddd512', common_article_3_scope__expansive_human_rights_reading, influences).
narrative_ontology:cs_axiom('1b709339-fe06-4986-84b9-4031f0ddd512', foundational, customary_law_primary_determinant).
narrative_ontology:cs_axiom_status(customary_law_primary_determinant, holdable).
narrative_ontology:cs_axiom_grounding('1b709339-fe06-4986-84b9-4031f0ddd512', customary_law_primary_determinant, conventional).
narrative_ontology:cs_axiom('1b709339-fe06-4986-84b9-4031f0ddd512', secondary, progressive_scope_expansion_through_practice).
narrative_ontology:cs_axiom_status(progressive_scope_expansion_through_practice, holdable).
narrative_ontology:cs_axiom_grounding('1b709339-fe06-4986-84b9-4031f0ddd512', progressive_scope_expansion_through_practice, empirically_contingent).
narrative_ontology:cs_reference_frame('1b709339-fe06-4986-84b9-4031f0ddd512', customary_law_interpretive_framework).
narrative_ontology:cs_drift_state('1b709339-fe06-4986-84b9-4031f0ddd512', contemporary_post_icrc_study, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1b709339-fe06-4986-84b9-4031f0ddd512', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__icrc_customary_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, icrc).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, international_courts).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, humanitarian_actors).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, states_seeking_certainty).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, non_state_armed_groups).
narrative_ontology:constraint_vindicates(common_article_3_scope__icrc_customary_reading, customary_law_adaptability).
narrative_ontology:constraint_vindicates(common_article_3_scope__icrc_customary_reading, progressive_development_of_ihl).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Tracks state practice and opinio juris through its customary IHL study; articulates and promotes customary rules governing CA3 scope; gains institutional authority and relevance as the authoritative identifier of customary law. Does not bear costs of the constraint; collects interpretive authority.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, icrc, agenda_setter,
    institutional, generational, analytical, global).

% Adopt and apply ICRC-articulated customary rules in jurisprudence (e.g., ICTY Tadić, ICC, regional courts); gain interpretive flexibility to address novel conflict forms without treaty amendment; legitimize their authority through customary law methodology. Collect protective authority without running the customary formation process.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, international_courts, beneficiary,
    institutional, generational, analytical, global).

% Participate in state practice that forms customary law but lose predictable thresholds for CA3 applicability; bound by evolving customary rules they did not explicitly consent to (persistent objector doctrine contested for humanitarian norms); powerful states (US, Russia, China) resist but cannot unilaterally halt customary evolution. Bear interpretive instability and loss of classificatory control.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, states_seeking_certainty, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__icrc_customary_reading, states_seeking_certainty, agenda_setter).

% Face expanding CA3 obligations as customary scope widens (e.g., to transnational NIACs, lower intensity thresholds); have no formal voice in customary law formation process; cannot exit the constraint as they are bound by IHL regardless of consent. Bear obligations without participatory access to the coordination mechanism.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, non_state_armed_groups, payer,
    organized, biographical, constrained, regional).

% Gain adaptive protective framework that extends CA3 to emerging conflict forms; use customary rules to negotiate access and protection in contexts where treaty thresholds are contested; operate in interpretive complexity but benefit from expanded humanitarian space. Collect protection dividends without bearing authority costs.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, humanitarian_actors, beneficiary,
    organized, biographical, mobile, global).

% Analyze, critique, and document the customary process; provide intellectual infrastructure for all three readings; do not collect rents or bear costs from the constraint's operation. Pure analytical seat.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_article_3_scope__icrc_customary_reading, icrc).
narrative_ontology:fixing_cost_class(common_article_3_scope__icrc_customary_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the adaptation problem: how to apply a 1949 treaty threshold (CA3) to conflict forms the drafters did not anticipate (transnational NIACs, counter-terrorism, cyber-enabled violence) without requiring universal treaty amendment. The customary process coordinates state practice, judicial decisions, and ICRC articulation into an evolving interpretive framework.
% TRANSFER_FUNCTION: Moves interpretive authority over CA3 scope from states (sovereign classifiers) to the ICRC and international courts (customary law articulators and adjudicators). States lose unilateral classificatory control; ICRC and courts gain authority to define the moving threshold. Non-state armed groups receive obligations without receiving authority.
% ABSENT_VOICES: Non-state armed groups are structurally excluded from the customary law formation process — they cannot contribute to state practice or opinio juris as formal subjects. Affected civilian populations in novel conflict forms (e.g., drone warfare, cyber operations) have no voice in whether CA3 applies to their situation. Both would object to the asymmetry but are not in the customary law conversation.
% DISAPPEARANCE_RATIONALE: If the customary process for CA3 scope vanished, states would revert to fixed treaty thresholds (state-centric reading) or courts would impose humanitarian floors (expansive reading). The coordination mechanism for gradual adaptation would disappear, creating either rigidity (states) or judicial activism (courts). The ICRC would lose its primary methodological tool for IHL development. The NIAC classification landscape would fragment.
% FOUNDING_PROBLEM: CA3 (1949) established a threshold ('armed conflict not of an international character') without defining it. By the 1970s, new conflict forms (wars of national liberation, internal conflicts with external involvement) made the undefined threshold unworkable. States refused to amend the Conventions. The customary process emerged as the only viable adaptation mechanism.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC attests the problem is live (2005 Study, 2024 updates cite new conflict forms). The International Law Commission's work on identification of customary law (2018) corroborates the procedural necessity. States opposing expansive customary rules (US, Israel, Russia) implicitly confirm the problem is live by contesting the customary methodology rather than claiming the threshold is settled. No major actor claims the adaptation problem is solved.
narrative_ontology:disappearance_verdict(common_article_3_scope__icrc_customary_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__icrc_customary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__icrc_customary_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(common_article_3_scope__icrc_customary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__icrc_customary_reading, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__icrc_customary_reading_tests).
:- end_tests(common_article_3_scope__icrc_customary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28) reflects the moderate transfer of interpretive authority from states to the ICRC/courts via the customary process — not resource extraction but authority reallocation. Suppression (0.15) is low: the constraint operates through legal persuasion and judicial adoption, not coercion; states can and do reject customary rules (persistent objector doctrine, though contested for jus cogens). Theater ratio (0.12) is low: the ICRC study and judicial citations perform real coordination work, not ritual. Accessibility collapse (0.45) is moderate: fixed-threshold alternatives remain conceptually available and are argued by some states, but the customary framework dominates judicial and institutional discourse. Resistance (0.40) is moderate: powerful states (US, Russia, China) resist expansive customary readings, but the constraint persists through broad institutional uptake. The measurement series shows gradual extraction accumulation as customary rules crystallize (1949-2024), with theater and suppression rising slightly as the ICRC study (2005) and subsequent jurisprudence create a self-reinforcing interpretive framework.
 *
 * PERSPECTIVAL GAP:
 *   From the ICRC/court seat, the constraint is a rope: a functional coordination mechanism solving the adaptation problem. From the state-certainty seat, it approaches tangled_rope: coordination exists but asymmetric authority transfer occurs without consent. From the non-state armed group seat, it is a snare: obligations expand through a process they cannot access or influence. The engine will compute these divergences from the structural data; the claim (rope) reflects the generating model's assessment of the *primary* structural character.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICRC and international courts are structural beneficiaries (d ≈ 0.15): they gain authority, relevance, and interpretive control. States seeking certainty are targets (d ≈ 0.7): their sovereign prerogative to define conflict classification erodes through practice they cannot unilaterally control. Non-state armed groups are deep targets (d ≈ 0.85): they bear obligations without participatory voice in the customary process. Humanitarian actors are near-symmetric beneficiaries (d ≈ 0.3): they gain adaptive protection tools but operate in the resulting interpretive complexity. Legal scholars are analytical observers (d = 0.5). The customary process itself is the coordination mechanism — it does not suppress alternatives (threshold arguments persist in state practice) but creates a gravitational center for interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (adapting CA3 to unforeseen conflict forms without treaty amendment) remains live — new conflict forms (cyber, autonomous weapons, transnational NIACs) continue to emerge. The customary process has not atrophied; it actively incorporates new practice. No mandatrophy: the coordination function is sustained and the mechanism has not become inertial or theatrical. The theater ratio rise reflects institutionalization, not functional decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the ICRC customary reading of CA3 scope a distinct constraint from the state-centric and expansive human rights readings, or a methodological stance within a single interpretive framework?',
    'Track whether state practice and opinio juris converge on customary law as the *exclusive* determinant of CA3 scope, or whether threshold and expansive positions persist as live interpretive options.',
    'If readings are distinct constraints, each gets its own ε and classification. If methodological stances within one framework, they share a constraint with observer-dependent classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three declared readings instantiate separate constraints or are framings of one constraint.').

omega_variable(
    customary_process_extraction,
    'Does the customary law formation process for CA3 scope extract interpretive authority from states and concentrate it in the ICRC and international courts?',
    'Analyze whether states'' freedom to define conflict classification is progressively constrained by ICRC-articulated customary rules they did not explicitly consent to.',
    'If extraction is significant, the constraint shifts from rope toward tangled_rope (coordination + asymmetric extraction). If minimal, rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_process_extraction, empirical, 'Whether the coordination mechanism carries hidden extraction toward institutional interpreters.').

omega_variable(
    natural_law_vs_constructed_process,
    'Is the customary international law process for CA3 scope a discovered natural legal order or a constructed coordination mechanism maintained by institutional actors?',
    'Examine whether the ''evolution'' of customary rules follows logically necessary trajectories or reflects contingent institutional advocacy by the ICRC and supportive states.',
    'If constructed, the constraint is a rope/tangled_rope with identifiable agenda-setters. If natural, it approaches mountain status for participants.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_process, conceptual, 'Natural-law vs constructed ambiguity for the customary process itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__icrc_customary_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ca3_icrc_customary_tr_t0, common_article_3_scope__icrc_customary_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ca3_icrc_customary_tr_t15, common_article_3_scope__icrc_customary_reading, theater_ratio, 15, 0.07).
narrative_ontology:measurement(ca3_icrc_customary_tr_t30, common_article_3_scope__icrc_customary_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement(ca3_icrc_customary_tr_t45, common_article_3_scope__icrc_customary_reading, theater_ratio, 45, 0.1).
narrative_ontology:measurement(ca3_icrc_customary_tr_t60, common_article_3_scope__icrc_customary_reading, theater_ratio, 60, 0.11).
narrative_ontology:measurement(ca3_icrc_customary_tr_t75, common_article_3_scope__icrc_customary_reading, theater_ratio, 75, 0.12).

% Extraction over time
narrative_ontology:measurement(ca3_icrc_customary_be_t0, common_article_3_scope__icrc_customary_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(ca3_icrc_customary_be_t15, common_article_3_scope__icrc_customary_reading, base_extractiveness, 15, 0.18).
narrative_ontology:measurement(ca3_icrc_customary_be_t30, common_article_3_scope__icrc_customary_reading, base_extractiveness, 30, 0.22).
narrative_ontology:measurement(ca3_icrc_customary_be_t45, common_article_3_scope__icrc_customary_reading, base_extractiveness, 45, 0.25).
narrative_ontology:measurement(ca3_icrc_customary_be_t60, common_article_3_scope__icrc_customary_reading, base_extractiveness, 60, 0.27).
narrative_ontology:measurement(ca3_icrc_customary_be_t75, common_article_3_scope__icrc_customary_reading, base_extractiveness, 75, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(ca3_icrc_customary_su_t0, common_article_3_scope__icrc_customary_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(ca3_icrc_customary_su_t15, common_article_3_scope__icrc_customary_reading, suppression_requirement, 15, 0.1).
narrative_ontology:measurement(ca3_icrc_customary_su_t30, common_article_3_scope__icrc_customary_reading, suppression_requirement, 30, 0.12).
narrative_ontology:measurement(ca3_icrc_customary_su_t45, common_article_3_scope__icrc_customary_reading, suppression_requirement, 45, 0.13).
narrative_ontology:measurement(ca3_icrc_customary_su_t60, common_article_3_scope__icrc_customary_reading, suppression_requirement, 60, 0.14).
narrative_ontology:measurement(ca3_icrc_customary_su_t75, common_article_3_scope__icrc_customary_reading, suppression_requirement, 75, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__icrc_customary_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(common_article_3_scope__icrc_customary_reading, 0.1).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, common_article_3_scope__state_centric_reading).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, common_article_3_scope__expansive_human_rights_reading).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, additional_protocol_ii_threshold).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, niac_classification_customary_rules).

% DUAL FORMULATION NOTE:
% The common_article_3_scope kernel decomposes into three constraint stories with distinct ε values: state_centric_reading (low extraction, mountain-like threshold claim), icrc_customary_reading (moderate extraction, rope coordination mechanism), expansive_human_rights_reading (higher extraction, tangled_rope/snare depending on enforcement). This reading (icrc_customary) provides the procedural mechanism that the expansive reading utilizes and the state-centric reading resists. The ε-invariance principle requires separate stories because measuring 'CA3 scope' via fixed thresholds vs. customary evolution vs. humanitarian floor yields structurally different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(common_article_3_scope__icrc_customary_reading, institutional, 0.15).
constraint_indexing:directionality_override(common_article_3_scope__icrc_customary_reading, organized, 0.75).
constraint_indexing:directionality_override(common_article_3_scope__icrc_customary_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
