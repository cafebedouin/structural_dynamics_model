% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__endogenous_reinterpretation_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: marriage_commitment_reversal__endogenous_reinterpretation_reading
 *   human_readable: Marriage Practice Reversal via Divine Revelation Reinterpretation
 *   domain: religious/institutional/political theology
 *
 * SUMMARY:
 *   A religious institution faces federal legal pressure to abandon a core
 *   doctrinal practice (polygyny). Rather than capitulate openly, the
 *   institutional leadership claims a new divine revelation that reinterprets
 *   God's will, reversing the prior teaching without denying its
 *   authenticity. The revelation is framed as God's adaptation to changed
 *   circumstances, not as human response to external coercion. This reading
 *   instantiates the constraint as it operates under the revelation frame: a
 *   tangled rope combining the coordination function (preserving
 *   institutional legitimacy and continuity through a reinterpretation
 *   mechanism) with extractive asymmetry (active practitioners and
 *   theological consistency bear the cost of the reversal while institutional
 *   authority is preserved). The constraint is CLAIMED as tangled_rope
 *   because it genuinely coordinates a real institutional problem (how to
 *   reverse practice without losing authority) while extracting from those
 *   who must reorganize their lives and from the coherence of the doctrinal
 *   corpus itself.
 *
 * KEY AGENTS:
 *   - institutional_leadership: agenda-setter, controls reinterpretation authority, preserves legitimacy through revelation narrative (institutional power, arbitrage exit)
 *   - active_practitioners: identity-locked payers, bear disruption cost, cannot exit without losing community standing (moderate power, biographical horizon)
 *   - next_generation_adherents: powerless beneficiaries, inherit clarity without prior-practice confusion, constrained but not disrupted (powerless, constrained exit)
 *   - federal_authorities: structurally excluded, apply the coercive pressure that occasions the revelation but cannot be named as such
 *   - competing_doctrinal_interpreters: excluded, reject the new revelation, identity-locked into dissent that institutional authority suppresses
 *   - theological_consistency: non-agent victim, bears the cost of explaining doctrine-practice reversal without denying either revelation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.68).
domain_priors:suppression_score(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.71).
domain_priors:theater_ratio(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__endogenous_reinterpretation_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__endogenous_reinterpretation_reading, "Marriage Practice Reversal via Divine Revelation Reinterpretation").
narrative_ontology:topic_domain(marriage_commitment_reversal__endogenous_reinterpretation_reading, "religious/institutional/political theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'e4e20b16-8530-4d10-b7ae-dbf7d6c820d0').
narrative_ontology:cs_kernel_codification('e4e20b16-8530-4d10-b7ae-dbf7d6c820d0', fixed_text).
narrative_ontology:cs_authority_grounding('e4e20b16-8530-4d10-b7ae-dbf7d6c820d0', lineage).
narrative_ontology:cs_interpretation_layer_present('e4e20b16-8530-4d10-b7ae-dbf7d6c820d0').
narrative_ontology:cs_reading_relation('e4e20b16-8530-4d10-b7ae-dbf7d6c820d0', marriage_commitment_reversal__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('e4e20b16-8530-4d10-b7ae-dbf7d6c820d0', marriage_commitment_reversal__practice_doctrine_gap, influences).
narrative_ontology:cs_axiom('e4e20b16-8530-4d10-b7ae-dbf7d6c820d0', foundational, continuous_revelation_principle).
narrative_ontology:cs_axiom_status(continuous_revelation_principle, holdable).
narrative_ontology:cs_axiom_grounding('e4e20b16-8530-4d10-b7ae-dbf7d6c820d0', continuous_revelation_principle, deontological).
narrative_ontology:cs_axiom('e4e20b16-8530-4d10-b7ae-dbf7d6c820d0', secondary, divine_will_adaptation_to_circumstance).
narrative_ontology:cs_axiom_status(divine_will_adaptation_to_circumstance, holdable).
narrative_ontology:cs_axiom_grounding('e4e20b16-8530-4d10-b7ae-dbf7d6c820d0', divine_will_adaptation_to_circumstance, theological).
narrative_ontology:cs_reference_frame('e4e20b16-8530-4d10-b7ae-dbf7d6c820d0', section_132_as_eternal_divine_will).
narrative_ontology:cs_drift_state('e4e20b16-8530-4d10-b7ae-dbf7d6c820d0', post_federal_threat_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e4e20b16-8530-4d10-b7ae-dbf7d6c820d0', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, institutional_leadership).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, doctrinal_authority_seat).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, active_practitioners).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_consistency).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, next_generation_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains interpretive authority over doctrine and practice. Frames the reversal as obedience to new divine revelation rather than capitulation to federal pressure. Controls the narrative of what God's will is and when it changes. Preserves institutional legitimacy by anchoring the practice shift to a claimed prophetic revelation rather than external coercion. Benefits from the revelation framing by avoiding the appearance of institutional compromise.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, institutional_leadership, agenda_setter,
    institutional, generational, arbitrage, global).

% Were living under the prior practice understanding (polygyny as religiously mandated or permissible). Face immediate material and relational disruption: existing plural marriages must be reorganized, theological justifications they relied on are withdrawn, their lived experience is reframed as erroneous. Cannot exit the institutional identity frame without losing community, family structure, and religious standing. Must accept the revelation narrative as authoritative despite having organized their entire lives around the prior teaching.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, active_practitioners, payer,
    moderate, biographical, identity_locked, national).

% Inherit the monogamous practice as the newly revealed norm without the prior doctrinal confusion. Are protected from the identity-lock cost that current practitioners bear. Benefit from institutional clarity and from legal/social alignment with surrounding society. Their exit options are constrained by childhood socialization and community embeddedness, but they do not bear the disruption cost of practice reversal.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, next_generation_adherents, beneficiary,
    powerless, biographical, constrained, national).

% Threatened legal action and social sanction that created the pressure context. Are structurally excluded from the institutional narrative of divine will, yet their coercive threat is the material occasion that prompted the revelation. Cannot be acknowledged as the true driver without invalidating the revelation's claimed source.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, federal_authorities, excluded,
    institutional, biographical, trapped, national).

% The abstraction that bears the cost of the doctrine-practice inversion: a non-actor entity kept for narrative completeness. If Section 132 was a revelation of God's will, what explains the reversal without calling God's judgment into question? Theological coherence is the victim here — it cannot be preserved intact without either denying the prior revelation or denying the new one.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_consistency, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_consistency).

% Members of the same religious tradition who reject the new revelation and maintain the prior practice as divinely mandated. Are structurally excluded from the institutional narrative now promulgated by leadership. Would argue that God's will does not reverse and that the revelation is a cover story for institutional capitulation. Their dissent is suppressed by institutional authority and by the identity-lock cost of breaking away.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, competing_doctrinal_interpreters, excluded,
    organized, generational, identity_locked, national).

% Sees the constraint from outside the institutional frame: observes the causal chain (federal pressure → revelation claim → practice reversal) and notes the structural fit between external coercion and revelation timing. Notes that the revelation's content (reversing the prior practice) solves the leadership's political problem without denying the prior revelation's authenticity — a convenient alignment that invites skepticism about the revelation's claimed source.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, historical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Secures institutional continuity and theological authority in the face of external legal threat: allows the institution to reverse a foundational practice while maintaining the claim that it obeys God's will (not human pressure). Coordinates internal members around a new understanding of divine will without requiring them to acknowledge the pressure that prompted it.
% TRANSFER_FUNCTION: Transfers the disruption cost and identity-lock burden from institutional leadership to active practitioners (who must reorganize their lives) and to theological consistency itself (which bears the cost of explaining why God's revealed will changed). Transfers institutional legitimacy preservation to the leadership (which gains the authority to reinterpret revelation under changed circumstances).
% ABSENT_VOICES: Federal authorities are structurally excluded — their coercive threat is the material occasion but cannot be named as such without invalidating the revelation claim. Competing doctrinal interpreters who reject the new revelation are suppressed by institutional authority and by the identity-lock that makes dissent costly. Historical observers outside the tradition are not in the conversation.
% DISAPPEARANCE_RATIONALE: If the revelation-framed reversal disappeared and the prior practice remained institutionally endorsed, the institution would face immediate legal jeopardy and would need to defend the practice through secular argument rather than prophetic authority. The institutional-leadership seat would lose its primary mechanism for navigating the coercion-legitimacy bind. The practice reversal and its revelation framing are structurally linked to the institution's survival in a hostile legal environment.
% FOUNDING_PROBLEM: A religious institution maintains a practice (polygyny) that is mandated or permitted by its core doctrine (Section 132) but is criminalized and socially condemned by the surrounding polity. The institution must find a way to reverse or abandon the practice without losing doctrinal authority or institutional legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the institutional leadership (who framed the reversal as revelation), by federal authorities (who applied legal pressure), and by historians and scholars of the period (independent of the institution) who document both the legal threat and the revelation claim. The structural correlation between the federal pressure and the revelation timing is testified by external observers and by dissenting members within the tradition who see the correlation as evidence that the revelation is a cover story.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__endogenous_reinterpretation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__endogenous_reinterpretation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__endogenous_reinterpretation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.68 by interval end) because the institutional leadership benefits from a mechanism (revelation reinterpretation) that preserves its authority in the face of a threat that would normally delegitimize it. The revelation frame allows the institution to reverse practice while claiming obedience to God's will, which is a sophisticated form of extraction — the cost of maintaining institutional legitimacy is borne by practitioners who must reorganize their lives and by theological consistency, which cannot be preserved intact. Suppression is high (0.71) because the constraint's persistence depends on actively suppressing the alternative reading (that federal pressure, not God's will, drove the reversal). The revelation narrative must be maintained as authoritative despite the convenient timing correlation with legal threat. Theater is elevated (0.58) because a growing share of the constraint's operation involves performative maintenance of the revelation narrative's credibility — defending it against external skepticism and internal doubt. Accessibility_collapse is moderate (0.62) because practitioners embedded in the identity-lock cannot readily exit, and the revelation narrative does collapse alternatives once accepted, but external observers can and do see through the correlation between pressure and revelation. Resistance is moderate (0.55) because active practitioners resist the practice reversal materially (some maintain plural marriages covertly), competing interpreters reject the revelation, and external observers express skepticism, yet the institutional machinery maintains enforcement through authority claims and identity-lock costs. The measurement series shows extractiveness and suppression ramping sharply in the 0–20 time window (period of revelation announcement and implementation enforcement) and stabilizing thereafter (once the new practice becomes normative), while theater_ratio rises proportionally, indicating the constraint requires increasing performative effort to maintain the revelation narrative's credibility.
 *
 * PERSPECTIVAL GAP:
 *   The institutional-leadership seat and the active-practitioner seat should compute dramatically differently. From the leadership perspective, the constraint is genuine coordination — a mechanism that solves a real institutional survival problem while honoring God's will (the new revelation). The leadership sees the reversal as obedience to changed divine guidance, not capitulation. From the practitioner perspective, the same structure is coercive: they are forced to reorganize their lives, their prior understanding of doctrine is invalidated, and the revelation narrative is a cover story for institutional response to federal pressure. They see the constraint as extractive because they bear the burden while leadership gains authority-preservation benefit. The engine computes this divergence from the structural data (practitioner identity-lock and burden, leadership arbitrage and authority-gain) — the computed type will diverge per seat, and that divergence is exactly the measurement the corpus exists to take. The revelation frame itself is a perspectival accomplishment: it makes the constraint appear coordinated from the leadership seat (we are obeying God) while operating extractively from the practitioner seat (we are absorbing costs of institutional survival). This is the defining structure of tangled_rope when one party controls the legitimacy narrative.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership has arbitrage exit (could theoretically acknowledge federal pressure openly or defend the prior practice; instead chooses the revelation frame) and powerful position (controls interpretive authority), yielding low directionality (d ~ 0.15–0.25) — they are the beneficiary seat. Active practitioners have identity-locked exit (cannot leave without losing family, community, religious standing) and moderate power (numerous but organizationally uncoordinated), yielding high directionality (d ~ 0.75–0.85) — they are the target seat. Next-generation adherents have constrained exit but benefit from institutional clarity, yielding mid-range directionality (d ~ 0.45–0.55). Theological consistency is not an agent and does not have directionality, but it bears the cost of the reversal (d ~ 1.0 if it were an agent, as it has no choice and no exit). Federal authorities are excluded from the framework and do not have a seat; they are the material cause but not a party to the constraint as the constraint is internally narrated.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live — the institution still must navigate the tension between doctrinal teaching and legal/social pressure. The mandatrophy analysis turns on whether the revelation is endogenous (God's actual will changed) or a cover story (federal pressure occasioned a convenient reframing). This reading authorizes the revelation as genuine — God adapted divine will to changed circumstances, which is consistent with the doctrine of continuous revelation. Under this reading, mandatrophy is NOT present: the constraint solves the founding problem (institutional survival under pressure) and does so by a mechanism (revelation reinterpretation) that the doctrine explicitly permits (continuous revelation). However, the high theater_ratio (0.58) and the rising suppression_requirement indicate that the revelation narrative requires increasing performative defense, which is a sign of cognitive strain within the framework. The exogenous_override_reading and practice_doctrine_gap readings would show mandatrophy more clearly — they frame the constraint as solving a political problem rather than a theological one, which invites the question of why the constraint persists when the federal pressure is resolved. This reading avoids that question by anchoring the constraint to divine will rather than political circumstance, but the structural data (theater and suppression rising) suggest the frame is under stress.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_authenticity,
    'Is the September 23 vision a genuine revelation of changed divine will, or is it a post-hoc framing of institutional response to federal coercion?',
    'Structural analysis of the revelation''s timing (immediate correlation with federal threat), content (reverses prior practice in exactly the way that solves the institution''s legal problem), and subsequent institutional narratives about divine guidance. Testimony from dissenting interpreters who accept the authority of revelation but reject this revelation. Comparison with other institutional reversals and their revelation framings.',
    'If the vision is genuine (revelation itself), the constraint''s extraction is the cost of obedience to changed will, and mandatrophy is not present. If it is a post-hoc frame (political pressure occasioning convenient reframing), the constraint is pure extraction with a coordination cover, and mandatrophy becomes central to classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revelation_authenticity, conceptual, 'Whether the revelation is authentic or a cover story for institutional response to coercion.').

omega_variable(
    doctrine_practice_preservability,
    'Can the prior doctrine (Section 132 as polygyny mandate/permission) and the new practice (monogamy mandate) be coherently held within a single theological framework, or does the reversal require denying one revelation or the other?',
    'Doctrinal analysis and theological commentary from interpreters within the tradition (both those accepting and those rejecting the new revelation). Examination of how the institution''s authorities resolve the apparent contradiction — whether they preserve the prior revelation as still valid (just superseded) or deny that it was ever a true revelation.',
    'If the doctrines can be coherently integrated (e.g., via a ''continuous revelation'' principle that permits revocation of prior teachings), theological consistency is preserved and extraction is lower. If integration fails, theological consistency is a clear victim, and the constraint''s extractiveness is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_practice_preservability, conceptual, 'Whether the doctrine-practice reversal preserves theological coherence or requires denying one revelation.').

omega_variable(
    revelation_framing_necessity,
    'Could the institutional leadership have achieved the same practice reversal through alternative frames (e.g., scriptural reinterpretation, adaptation to changed social conditions, gradual phase-out) that would preserve less of the revelation-authority structure?',
    'Comparative analysis of how other religious institutions reversed contested practices. Examination of whether the revelation frame was uniquely effective in maintaining institutional authority or merely one option among several.',
    'If alternative frames were available but the revelation frame was chosen, that choice reveals a preference for preserving prophetic authority over transparency about causal drivers. If the revelation frame was the only politically viable option, it is less a choice than a structural constraint. This affects the degree to which the constraint is extractive (leveraging authority) vs. necessary (no alternatives available).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_framing_necessity, empirical, 'Whether the revelation frame was necessary for institutional survival or a choice among alternatives.').

omega_variable(
    kernel_contest_framing,
    'Is this constraint most fundamentally about obedience to God''s revealed will (the endogenous_reinterpretation frame), about resistance to federal coercion (the exogenous_override frame), or about an unresolved doctrine-practice gap (the practice_doctrine_gap frame)?',
    'Analysis of which frame the institutional leadership emphasizes in different contexts (internal teaching vs. external defense); examination of which frame explains the constraint''s persistence and enforcement machinery most parsimoniously; identification of which frame would be most threatened by falsifying evidence (revealing the pressures that occasioned the reversal, or discovering that the revelation claim is widely disbelieved internally).',
    'This omega encodes the choice of this reading over its siblings. If the endogenous_reinterpretation frame is the most defensible, this constraint should compute as this reading''s classification; if the exogenous_override frame is more parsimonious, the constraint should be reclassified under that reading; if the practice_doctrine_gap frame is more accurate, it should be reclassified under the structural-ambiguity reading. The choice of frame is not neutral — it shapes which agents bear costs and which benefit from the narrative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_framing, preference, 'Which kernel-reading frame best captures the constraint''s actual structure and function.').

omega_variable(
    suppression_identity_lock_internalization,
    'Is the suppression of dissenting voices and alternative interpretations primarily structural (institutional authority prevents public expression) or internalized (practitioners have accepted the revelation frame and suppress alternative thoughts themselves)?',
    'Longitudinal study of practitioners who leave the institution: do they retain belief in the new revelation when they are outside the institutional environment, or do they revert to skepticism? Examination of private vs. public discourse within the tradition — are there signs of internal doubt coexisting with public affirmation? Analysis of how suppression mechanisms change over generational time as the revelation frame becomes normative.',
    'If suppression is primarily structural, institutional authority can relax enforcement if that authority is challenged. If suppression is primarily internalized, the constraint is more stable and more extractive because the cost is fully borne by practitioners'' own cognitive frames. Internalized suppression also indicates that the theater_ratio may be undershooting the true performative cost — practitioners are performing the acceptance internally, not just publicly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_identity_lock_internalization, empirical, 'Whether suppression is maintained through institutional authority or internalized by practitioners.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(marr_tr_t5, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(marr_tr_t10, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(marr_tr_t15, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 15, 0.52).
narrative_ontology:measurement(marr_tr_t20, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 20, 0.56).
narrative_ontology:measurement(marr_tr_t25, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 25, 0.58).
narrative_ontology:measurement(marr_tr_t30, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 30, 0.58).
narrative_ontology:measurement(marr_tr_t40, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(marr_be_t5, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(marr_be_t10, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(marr_be_t15, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(marr_be_t20, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(marr_be_t25, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(marr_be_t30, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(marr_be_t40, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(marr_su_t5, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(marr_su_t10, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(marr_su_t15, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement(marr_su_t20, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(marr_su_t25, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(marr_su_t30, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(marr_su_t40, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.12).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal__practice_doctrine_gap).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel ('marriage_commitment_reversal') shared with two sibling constraints: exogenous_override_reading (federal coercion without doctrinal revision) and practice_doctrine_gap (structural ambiguity between preserved doctrine and suspended practice). All three stories share the same referent (marriage practice reversal) but assign different causal and normative status to the reversal. The three constraints have different ε values, beneficiary/victim structures, and classifications because they instantiate genuinely distinct structural arrangements under the same institutional event. Decomposition is driven by ε-invariance: measuring the same institutional reversal under different causal frames yields materially different extractiveness profiles. The endogenous_reinterpretation_reading authorizes the reversal as divinely guided (moderate extractiveness, leadership legitimacy preserved). The exogenous_override_reading frames it as coerced (higher extractiveness, leadership authority compromised). The practice_doctrine_gap frames it as unresolved (extractiveness ambiguous, doctrinal authority in tension with practice). Each reading has its own stakeholder set, six-questions battery, and temporal dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_reversal__endogenous_reinterpretation_reading, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
