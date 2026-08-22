% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__positivist_reading, []).

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
 *   constraint_id: us_constitution_meaning__positivist_reading
 *   human_readable: Constitutional Validity via Formal Enactment (Positivist Reading)
 *   domain: constitutional law / legal theory / political philosophy
 *
 * SUMMARY:
 *   This constraint story captures the legal-positivist reading of U.S.
 *   constitutional meaning: constitutional validity is determined exclusively
 *   by formal enactment procedures (ratification, amendment) and
 *   institutional pedigree, not by external moral principles. Under this
 *   reading, judges are constrained to enforce the enacted text and its
 *   formal outputs; substantive justice claims that lack textual support are
 *   structurally excluded. The constraint coordinates legal actors by
 *   supplying a determinate rule of recognition, but it also asymmetrically
 *   extracts from unenumerated-rights claimants and moral-reform movements by
 *   blocking their only avenue of constitutional vindication when the
 *   amendment process is gridlocked. The story claims tangled_rope because
 *   the arrangement possesses a genuine coordination function (stability,
 *   predictability) alongside active, asymmetric extraction.
 *
 * KEY AGENTS:
 *   - federal_judiciary (institutional/constrained): administers the rule of recognition, enforces formal enactment supremacy, and gains procedural clarity
 *   - substantive_justice_claimants (powerless/trapped): bear the cost of having moral and evolving justice claims ruled constitutionally out of bounds
 *   - political_branches (powerful/constrained): benefit from insulation of their enacted products from substantive moral challenge
 *   - living_constitutionalist_jurists (moderate/constrained): structurally excluded from validity determination under this reading
 *   - comparative_legal_observers (analytical/analytical): external analytical seat observing alternative validity regimes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__positivist_reading, 0.62).
domain_priors:suppression_score(us_constitution_meaning__positivist_reading, 0.45).
domain_priors:theater_ratio(us_constitution_meaning__positivist_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__positivist_reading, "Constitutional Validity via Formal Enactment (Positivist Reading)").
narrative_ontology:topic_domain(us_constitution_meaning__positivist_reading, "constitutional law / legal theory / political philosophy").

domain_priors:requires_active_enforcement(us_constitution_meaning__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__positivist_reading, '592a6ffe-3402-42c4-963c-2b4e686e8cd9').
narrative_ontology:cs_kernel_codification('592a6ffe-3402-42c4-963c-2b4e686e8cd9', formalized).
narrative_ontology:cs_authority_grounding('592a6ffe-3402-42c4-963c-2b4e686e8cd9', lineage).
narrative_ontology:cs_interpretation_layer_present('592a6ffe-3402-42c4-963c-2b4e686e8cd9').
narrative_ontology:cs_reading_relation('592a6ffe-3402-42c4-963c-2b4e686e8cd9', us_constitution_meaning__originalist_reading, influences).
narrative_ontology:cs_reading_relation('592a6ffe-3402-42c4-963c-2b4e686e8cd9', us_constitution_meaning__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_axiom('592a6ffe-3402-42c4-963c-2b4e686e8cd9', foundational, validity_from_formal_enactment_only).
narrative_ontology:cs_axiom_status(validity_from_formal_enactment_only, holdable).
narrative_ontology:cs_axiom_grounding('592a6ffe-3402-42c4-963c-2b4e686e8cd9', validity_from_formal_enactment_only, conventional).
narrative_ontology:cs_axiom('592a6ffe-3402-42c4-963c-2b4e686e8cd9', foundational, moral_principles_excluded_from_validity).
narrative_ontology:cs_axiom_status(moral_principles_excluded_from_validity, holdable).
narrative_ontology:cs_axiom_grounding('592a6ffe-3402-42c4-963c-2b4e686e8cd9', moral_principles_excluded_from_validity, conventional).
narrative_ontology:cs_reference_frame('592a6ffe-3402-42c4-963c-2b4e686e8cd9', formal_enactment_supremacy).
narrative_ontology:cs_drift_state('592a6ffe-3402-42c4-963c-2b4e686e8cd9', contemporary_amendment_gridlock, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('592a6ffe-3402-42c4-963c-2b4e686e8cd9', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__positivist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, political_branches).
narrative_ontology:constraint_victim(us_constitution_meaning__positivist_reading, substantive_justice_claimants).
narrative_ontology:constraint_vindicates(us_constitution_meaning__positivist_reading, legal_positivism).
narrative_ontology:constraint_vindicates(us_constitution_meaning__positivist_reading, procedural_legitimacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the constraint by treating constitutional validity as exhausted by formal enactment, institutional pedigree, and the written text. Judges enforce this rule of recognition on themselves and the political branches, excluding external moral principles from validity determination. They gain procedural clarity, reduced direct political accountability, and institutional stability, but sacrifice interpretive flexibility.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, federal_judiciary, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_meaning__positivist_reading, federal_judiciary, beneficiary).

% Congress and the Executive benefit from a validity rule that treats their formally enacted products as constitutionally binding without requiring ongoing substantive moral justification. Their authority is insulated from direct moral challenge so long as procedural enactment requirements are satisfied, even when the amendment process is gridlocked.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, political_branches, beneficiary,
    powerful, generational, constrained, national).

% Groups and individuals asserting unenumerated rights, evolving equality norms, or other justice claims that lack explicit textual or formal procedural support. Their claims are ruled constitutionally invalid regardless of moral weight because they cannot point to a formal enactment. They cannot exit the constitutional system; their only path to change is the formal amendment process, which is structurally inaccessible.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, substantive_justice_claimants, payer,
    powerless, generational, trapped, national).

% Judges and scholars who hold that constitutional application should evolve with social attitudes and substantive moral principles. Under the positivist reading, their arguments are categorically excluded from validity determination; they are not in the room where constitutional meaning is authoritatively fixed.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, living_constitutionalist_jurists, excluded,
    moderate, biographical, constrained, national).

% Constitutional scholars and jurists from other democratic systems who observe that many jurisdictions incorporate substantive moral principles or international human rights law directly into constitutional review. They analyze the U.S. positivist reading as one possible rule of recognition among many, noting its distinctive formalism.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, comparative_legal_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_meaning__positivist_reading, diffuse).
narrative_ontology:fixing_cost_class(us_constitution_meaning__positivist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a determinate, publicly verifiable rule of constitutional validityâa rule of recognitionâso that citizens and officials can identify binding norms without resorting to contested moral philosophy or discretionary value judgments.
% TRANSFER_FUNCTION: Moves interpretive authority from substantive moral argumentation and evolving social ethics to formal enactment records and institutional pedigree; moves constitutional protection away from groups asserting unenumerated or evolving justice claims and toward interests with textual or procedural footholds.
% ABSENT_VOICES: Living constitutionalist jurists and natural law theorists are structurally excluded from validity determination; they would argue that moral principles and evolving social understandings should inform constitutional meaning, but the positivist rule of recognition treats such arguments as extra-legal by definition.
% DISAPPEARANCE_RATIONALE: If the positivist constraint on validity sources vanished overnight, constitutional adjudication would lose its procedural anchor; judges would openly incorporate moral reasoning and substantive justice; the boundary between constitutional law and political philosophy would shift dramatically, and the institutional monopoly of the formal amendment process would be broken.
% FOUNDING_PROBLEM: How to establish a reliable, non-subjective criterion for determining which norms are constitutionally valid and binding on government actors, avoiding the interpretive chaos of conflicting moral intuitions and unbounded judicial discretion.
% FOUNDING_PROBLEM_CORROBORATION: Legal positivist scholars attest the problem remains live, citing the need for objective validity criteria. Critical legal scholars and natural law theorists outside the benefiting judicial and political branches attest the problem is superseded by the need for substantive justice in a pluralistic society, corroborating that the arrangement now functions to insulate existing power from moral critique.
narrative_ontology:disappearance_verdict(us_constitution_meaning__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__positivist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_meaning__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__positivist_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__positivist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_meaning__positivist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_meaning__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the constraint systematically transfers constitutional protection away from textually unsupported justice claims; suppression (0.45) reflects institutional and professional discipline rather than raw coercion. Accessibility collapse (0.78) is high because, once the positivist frame is accepted, natural-law or living-constitutionalist alternatives become legally invisible. Theater ratio (0.35) captures the performative aspect of formalist reasoning that masks policy choices. Resistance (0.40) is moderateâliving constitutionalism and critical legal studies contest the frame, but largely from outside the bench. The measurement series tracks rising extraction and theater as formal amendment has become more gridlocked and substantive claims more common.
 *
 * PERSPECTIVAL GAP:
 *   From the judicial seat, the constraint appears as necessary coordinationâthe only alternative is subjective moral chaos. From the substantive-justice seat, the same structure appears as an active suppression of morally urgent claims. The engine will compute this divergence from the structural data: same constraint, radically different d values and effective extraction profiles.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal judiciary and political branches are structural beneficiaries (low d): they gain procedural legitimacy, reduced political accountability, and institutional stability. Substantive justice claimants are structural targets (high d): they bear the cost of exclusion with no exit from the constitutional system. Living constitutionalist jurists are excluded rather than coordinated; their absence is a structural feature of the constraint. The comparative observer sits at analytical distance with no directional stake.
 *
 * MANDATROPHY ANALYSIS:
 *   The positivist reading risks mandatrophy if the founding problem (indeterminacy of moral reasoning) is dead but the arrangement persists. Here the founding_problem_status is contested, not dead, so mandatrophy is not declared resolved. However, the drift toward originalism under gridlock signals a partial functional substitution: the constraint's coordination rationale (formal enactment) is partially performed by originalist meaning-fixation when enactment is unavailable. This prevents a simple dead-mandate verdict while signaling incipient drift that could eventually push the constraint toward piton status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    amendment_gridlock_collapse,
    'Does the positivist reading collapse into originalism in practice when the formal amendment process is gridlocked, functionally merging two distinct constraints?',
    'Comparative analysis of positivist courts in high-amendment-capacity versus gridlocked systems; doctrinal tracing of positivist judges who adopt originalist methods when amendment is unavailable.',
    'If collapse occurs, the positivist reading''s extraction profile is understated because it inherits originalism''s constraints on meaning, and the effective constraint becomes a hybrid not captured by either reading alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_gridlock_collapse, conceptual, 'Whether gridlock functionally merges positivist validity with originalist meaning').

omega_variable(
    concealed_moral_reasoning,
    'Does the positivist exclusion of external moral principles from validity produce genuinely neutral adjudication, or does it merely displace moral reasoning into covert institutional practice?',
    'Empirical content-analysis of judicial opinions under positivist regimes, coding for covert moral or policy premises; sociological study of judicial behavior and clerkship networks.',
    'If moral reasoning is inescapable, the positivist reading presents a false summitâclaimed procedural neutrality masking substantive extraction by the institutional status quo.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(concealed_moral_reasoning, empirical, 'Whether positivist neutrality is achievable or a concealment mechanism').

omega_variable(
    procedural_legitimacy_recipient,
    'Does the procedural legitimacy generated by this constraint accrue to the constitutional system as a whole, or is it captured by specific dominant political coalitions?',
    'Political-science analysis of which coalitions benefit from judicial deference to enacted text; tracking of judicial review patterns against partisan alignment over time.',
    'If captured by dominant coalitions, the constraint is more extractive than its coordination framing suggests; if diffuse, the coordination benefit is broadly shared and the extraction is lower.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(procedural_legitimacy_recipient, empirical, 'Whether legitimacy benefits are system-wide or coalition-captured').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__positivist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uscp_pos_tr_t0, us_constitution_meaning__positivist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(uscp_pos_tr_t10, us_constitution_meaning__positivist_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(uscp_pos_tr_t20, us_constitution_meaning__positivist_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(uscp_pos_tr_t30, us_constitution_meaning__positivist_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(uscp_pos_tr_t40, us_constitution_meaning__positivist_reading, theater_ratio, 40, 0.31).
narrative_ontology:measurement(uscp_pos_tr_t50, us_constitution_meaning__positivist_reading, theater_ratio, 50, 0.33).
narrative_ontology:measurement(uscp_pos_tr_t60, us_constitution_meaning__positivist_reading, theater_ratio, 60, 0.35).

% Extraction over time
narrative_ontology:measurement(uscp_pos_be_t0, us_constitution_meaning__positivist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(uscp_pos_be_t10, us_constitution_meaning__positivist_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(uscp_pos_be_t20, us_constitution_meaning__positivist_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(uscp_pos_be_t30, us_constitution_meaning__positivist_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(uscp_pos_be_t40, us_constitution_meaning__positivist_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(uscp_pos_be_t50, us_constitution_meaning__positivist_reading, base_extractiveness, 50, 0.6).
narrative_ontology:measurement(uscp_pos_be_t60, us_constitution_meaning__positivist_reading, base_extractiveness, 60, 0.62).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(us_constitution_meaning__positivist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(us_constitution_meaning__positivist_reading, originalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__positivist_reading, living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the us_constitution_meaning kernel, decomposed per the epsilon-invariance principle because the kernel label conflates three structurally distinct claims about constitutional validity and meaning. This reading addresses the formal-enactment validity claim; siblings address fixation-of-meaning and evolving-application claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
