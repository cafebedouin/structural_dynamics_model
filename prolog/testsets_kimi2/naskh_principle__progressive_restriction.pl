% ============================================================================
% CONSTRAINT STORY: naskh_principle__progressive_restriction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__progressive_restriction, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: naskh_principle__progressive_restriction
 *   human_readable: Quranic Progressive Restriction Hermeneutic
 *   domain: religious/legal/hermeneutic
 *
 * SUMMARY:
 *   This constraint instantiates the progressive_restriction reading of the
 *   naskh_principle kernel in Islamic legal theory. The reading holds that
 *   Quranic revelation did not abrogate (invalidate) earlier permissive
 *   verses but progressively restricted them as part of a divine pedagogy.
 *   Earlier permissions on warfare, slavery, gender relations, and ritual law
 *   are treated as transitional accommodations; later restrictions represent
 *   final divine intent. The constraint functions as a hermeneutic gate: it
 *   coordinates legal development by resolving apparent contradictions
 *   without invoking classical abrogation, while extracting interpretive
 *   authority from those who would cite early permissive texts as current
 *   law. Sibling readings include classical_abrogation (later verses
 *   invalidate earlier ones) and contextual_harmonization (all verses remain
 *   valid in situational context).
 *
 * KEY AGENTS:
 *   - developmental_jurists: Primary agenda_setter (institutional/analytical) â formulates and enforces the progressive restriction framework
 *   - legal_modernists: Primary beneficiary (organized/mobile) â gains legal authority for reform positions without technical abrogation
 *   - permissive_practice_advocates: Primary payer (moderate/constrained) â loses standing to cite early Quranic permissions
 *   - classical_abrogationists: Secondary payer (institutional/constrained) â classical naskh framework displaced by progressive reading
 *   - academic_observers: Analytical observer (analytical/analytical) â studies the hermeneutic contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__progressive_restriction, 0.62).
domain_priors:suppression_score(naskh_principle__progressive_restriction, 0.48).
domain_priors:theater_ratio(naskh_principle__progressive_restriction, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, extractiveness, 0.62).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__progressive_restriction, tangled_rope).
narrative_ontology:human_readable(naskh_principle__progressive_restriction, "Quranic Progressive Restriction Hermeneutic").
narrative_ontology:topic_domain(naskh_principle__progressive_restriction, "religious/legal/hermeneutic").

domain_priors:requires_active_enforcement(naskh_principle__progressive_restriction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__progressive_restriction, 'ec9d8f5a-36fc-4ec2-b7f8-7da90e04a1e8').
narrative_ontology:cs_kernel_codification('ec9d8f5a-36fc-4ec2-b7f8-7da90e04a1e8', fixed_text).
narrative_ontology:cs_authority_grounding('ec9d8f5a-36fc-4ec2-b7f8-7da90e04a1e8', lineage).
narrative_ontology:cs_interpretation_layer_present('ec9d8f5a-36fc-4ec2-b7f8-7da90e04a1e8').
narrative_ontology:cs_reading_relation('ec9d8f5a-36fc-4ec2-b7f8-7da90e04a1e8', naskh_principle__classical_abrogation, forecloses).
narrative_ontology:cs_reading_relation('ec9d8f5a-36fc-4ec2-b7f8-7da90e04a1e8', naskh_principle__contextual_harmonization, coexists_with).
narrative_ontology:cs_axiom('ec9d8f5a-36fc-4ec2-b7f8-7da90e04a1e8', foundational, divine_pedagogy_progressive_restriction).
narrative_ontology:cs_axiom_status(divine_pedagogy_progressive_restriction, holdable).
narrative_ontology:cs_axiom_grounding('ec9d8f5a-36fc-4ec2-b7f8-7da90e04a1e8', divine_pedagogy_progressive_restriction, theological).
narrative_ontology:cs_axiom('ec9d8f5a-36fc-4ec2-b7f8-7da90e04a1e8', foundational, later_restrictions_final_intent).
narrative_ontology:cs_axiom_status(later_restrictions_final_intent, holdable).
narrative_ontology:cs_axiom_grounding('ec9d8f5a-36fc-4ec2-b7f8-7da90e04a1e8', later_restrictions_final_intent, theological).
narrative_ontology:cs_reference_frame('ec9d8f5a-36fc-4ec2-b7f8-7da90e04a1e8', quranic_developmental_pedagogy).
narrative_ontology:cs_drift_state('ec9d8f5a-36fc-4ec2-b7f8-7da90e04a1e8', post_classical_jurisprudence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ec9d8f5a-36fc-4ec2-b7f8-7da90e04a1e8', '').
narrative_ontology:cs_kernel_id(naskh_principle__progressive_restriction, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, legal_modernists).
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, developmental_jurists).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, permissive_practice_advocates).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, classical_abrogationists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Formulate and teach the progressive restriction hermeneutic, arguing that Quranic revelation pedagogically narrowed permissions over time rather than abrogating them. They publish in academic and reformist religious venues, train students in this framework, and adjudicate disputes between classical and modern readings by privileging later restrictive verses as final divine intent.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, developmental_jurists, agenda_setter,
    institutional, generational, analytical, global).

% Benefit from a framework that allows later restrictive verses to govern contemporary law without invoking the technical apparatus of classical abrogation. They gain legitimacy for reform positions on gender relations, criminal law, and slavery by treating earlier permissions as transitional divine accommodations rather than permanently valid rulings.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, legal_modernists, beneficiary,
    organized, generational, mobile, global).

% Cite early Quranic permissions on inheritance, dress, warfare, or social conduct as current legal authority. Under the progressive restriction reading, these citations are delegitimized as references to temporary pedagogical stages rather than binding law. Their interpretive options are bounded by the hermeneutic framework and they lack comparable institutional backing in reformist venues.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, permissive_practice_advocates, payer,
    moderate, biographical, constrained, global).

% Maintain the classical naskh framework that later verses formally abrogate earlier ones in specific instances. The progressive restriction reading displaces their technical apparatus by claiming no true abrogation occurred, only progressive narrowing. They retain institutional bases in traditional seminaries but face epistemic marginalization in reformist academic and policy discourse.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, classical_abrogationists, payer,
    institutional, generational, constrained, global).

% Study the hermeneutic contest as an instance of legal development and scriptural interpretation, neither advancing nor constrained by either framework. They document how the progressive reading alters the authority structure of Quranic legal argumentation and track its diffusion across transnational Islamic reform movements.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, academic_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(naskh_principle__progressive_restriction, developmental_jurists).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves apparent contradictions between early permissive and later restrictive Quranic verses by positing a developmental divine pedagogy, thereby preserving theological consistency without invoking classical abrogation.
% TRANSFER_FUNCTION: Moves interpretive authority from early-text-citers and classical abrogationists to progressive hermeneuts and modernist reformers by reframing earlier permissions as transitional rather than permanently valid.
% ABSENT_VOICES: Classical seminary students and traditional jurists in non-reformist institutions are largely absent from the venues where progressive restriction is developed and promulgated; their objections to the loss of classical naskh categories are systematically unheard in modernist academic and policy forums.
% DISAPPEARANCE_RATIONALE: If the progressive restriction reading vanished, reformist jurists would lose a major framework for privileging later restrictive verses without invoking classical abrogation; permissive-practice advocates would regain epistemic standing for early citations; classical abrogation theory would reassert dominance in the hermeneutic space; and the legal repertoire of modernist reform would contract significantly.
% FOUNDING_PROBLEM: Classical abrogation (naskh) created theological tension by positing that God invalidates earlier revelations; apparent contradictions in Quranic legal verses required a framework that preserved divine consistency while allowing chronological development toward restriction.
% FOUNDING_PROBLEM_CORROBORATION: Progressive jurists attest the problem is live, citing theological discomfort with divine self-contradiction. Classical abrogationists attest the problem was solved adequately by classical naskh theory and did not need revision. Western academic Islamicists and historians of Islamic law outside both camps document that the 'problem' is itself a product of later hermeneutic assumptions imposed on the text, corroborating that the founding problem is contested rather than self-evident.
narrative_ontology:disappearance_verdict(naskh_principle__progressive_restriction, world_rearranges).
narrative_ontology:founding_problem_status(naskh_principle__progressive_restriction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__progressive_restriction, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(naskh_principle__progressive_restriction, 'none', 1).
narrative_ontology:epsilon_provenance(naskh_principle__progressive_restriction, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__progressive_restriction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(naskh_principle__progressive_restriction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(naskh_principle__progressive_restriction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the reading systematically removes a large body of early Quranic permissions from active legal circulation without declaring them abrogated. Suppression (0.48) is moderate: the constraint operates primarily through epistemic authority and institutional pedagogy rather than direct coercion, but alternative readings are structurally disadvantaged in reformist venues. Theater_ratio (0.35) reflects a growing performative element as the reading is invoked to authorize modern reform positions that may be only loosely tethered to textual chronology. Accessibility_collapse (0.70) is high because once the progressive pedagogy framework is accepted, early-text alternatives become nearly unintelligible as legal arguments within that framework. Resistance (0.55) is moderate-to-high: classical institutions and traditional practitioners actively contest the reading, though often from outside reformist discourse.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter and beneficiary seats (developmental jurists, legal modernists) experience the constraint as a recovery of coherent divine pedagogy that preserves God's consistency; the payer seats (permissive-practice advocates, classical abrogationists) experience it as an epistemic seizure that invalidates their textual and methodological heritage. The engine computes this divergence from the same structural data: low directionality for those who gain hermeneutic authority, high directionality for those stripped of it.
 *
 * DIRECTIONALITY LOGIC:
 *   Developmental jurists are structural beneficiaries (low d): the constraint amplifies their authority as indispensable interpreters of revelatory chronology. Legal modernists collect diffuse legal advantage (low d). Permissive-practice advocates are structural targets (high d): the constraint specifically removes their ability to deploy early texts as current authority. Classical abrogationists are secondary targets (moderate-high d): their technical framework is marginalized, though they retain some institutional refuge in traditional seminaries. Scope is global because Quranic hermeneutics operates across the transnational Islamic legal sphere.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by maintaining a genuine coordination function: it resolves a real theological tension in classical abrogation theory and provides a coherent narrative of legal development that allows reform without explicit rejection of scripture. Without that coordination component, the reading would be a snare (pure extraction from early-text users). With it, the constraint is a tangled rope: it coordinates by solving contradiction while simultaneously extracting from those whose interpretive repertoire depends on the earlier permissions. The founding problem â theological tension over divine self-contradiction â is contested, meaning the coordination claim may partly serve as cover for reformist legal outcomes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naskh_kernel_reading_position,
    'Is the progressive restriction reading a historically accurate account of early Islamic legal pedagogy, or a modern reformist construct projected backward onto the revelatory process?',
    'Historical-critical analysis of early Islamic legal sources and pre-classical jurisprudence to determine whether the progressive-restriction model operated before the crystallization of classical naskh theory.',
    'If historically inaccurate as a universal principle, the constraint is a modern scaffold or snare rather than a recovery of authentic juristic method; this would reclassify from tangled_rope toward either scaffold (transitional modernist tool) or snare (reformist extraction from traditional text-users).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naskh_kernel_reading_position, empirical, 'Historical accuracy of progressive restriction as authentic pedagogy').

omega_variable(
    sibling_reading_foreclosure,
    'Does the progressive restriction reading logically foreclose classical abrogation within a single jurist''s framework, or can both operate as compartmentalized heuristics for different verse pairs?',
    'Juristic autobiography and methodological texts: do proponents of progressive restriction explicitly reject technical naskh al-hukm wa al-tilawa categorically, or do they retain classical abrogation for specific verse pairs while applying progressive restriction elsewhere?',
    'If compartmentalized, the relation to classical_abrogation should be coexists_with rather than forecloses, and the constraint''s extraction profile is lower because it does not fully displace the alternative framework.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Logical relationship between progressive restriction and classical abrogation').

omega_variable(
    textual_continuity_versus_invalidation,
    'Does the distinction between ''restricted'' and ''abrogated'' earlier verses produce materially different legal outcomes, or is it primarily a theological difference about textual status with identical practical effects?',
    'Comparative fiqh analysis across domains where progressive restriction, classical abrogation, and contextual harmonization are applied: identify cases where the three readings yield divergent practical rulings.',
    'If outcomes are identical across readings, the constraint is largely theological theater (high theater_ratio) with low effective extraction; if outcomes diverge systematically, the constraint has genuine legal extraction and its tangled_rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_continuity_versus_invalidation, empirical, 'Material legal difference between restriction and abrogation readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__progressive_restriction, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naskh_prog_tr_t0, naskh_principle__progressive_restriction, theater_ratio, 0, 0.22).
narrative_ontology:measurement(naskh_prog_tr_t10, naskh_principle__progressive_restriction, theater_ratio, 10, 0.24).
narrative_ontology:measurement(naskh_prog_tr_t20, naskh_principle__progressive_restriction, theater_ratio, 20, 0.27).
narrative_ontology:measurement(naskh_prog_tr_t30, naskh_principle__progressive_restriction, theater_ratio, 30, 0.3).
narrative_ontology:measurement(naskh_prog_tr_t40, naskh_principle__progressive_restriction, theater_ratio, 40, 0.32).
narrative_ontology:measurement(naskh_prog_tr_t50, naskh_principle__progressive_restriction, theater_ratio, 50, 0.35).

% Extraction over time
narrative_ontology:measurement(naskh_prog_be_t0, naskh_principle__progressive_restriction, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(naskh_prog_be_t10, naskh_principle__progressive_restriction, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(naskh_prog_be_t20, naskh_principle__progressive_restriction, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(naskh_prog_be_t30, naskh_principle__progressive_restriction, base_extractiveness, 30, 0.56).
narrative_ontology:measurement(naskh_prog_be_t40, naskh_principle__progressive_restriction, base_extractiveness, 40, 0.59).
narrative_ontology:measurement(naskh_prog_be_t50, naskh_principle__progressive_restriction, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(naskh_prog_su_t0, naskh_principle__progressive_restriction, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(naskh_prog_su_t10, naskh_principle__progressive_restriction, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(naskh_prog_su_t20, naskh_principle__progressive_restriction, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(naskh_prog_su_t30, naskh_principle__progressive_restriction, suppression_requirement, 30, 0.44).
narrative_ontology:measurement(naskh_prog_su_t40, naskh_principle__progressive_restriction, suppression_requirement, 40, 0.46).
narrative_ontology:measurement(naskh_prog_su_t50, naskh_principle__progressive_restriction, suppression_requirement, 50, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
