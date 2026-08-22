% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__abrogating_universal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__abrogating_universal, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: quran_9_5_scope__abrogating_universal
 *   human_readable: Verse 9:5 Abrogating Universal Offensive Jihad Mandate
 *   domain: religious/political/hermeneutic
 *
 * SUMMARY:
 *   Verse 9 (al-Tawbah) is the final Medinan surah, revealed late in
 *   Muhammad's Medina period. Verse 9:5, known as the 'Verse of the Sword'
 *   (Ayat al-Sayf), commands: 'Then, when the sacred months have passed, slay
 *   the idolaters wherever ye find them, and take them (captive), and besiege
 *   them, and prepare for them each ambush.' The abrogating-universal reading
 *   treats this verse as abrogating (nasikh) all prior verses that counsel
 *   peace, tolerance, and defensive warfare only, establishing a permanent,
 *   universal obligation (fard ayn) for Muslim believers to conduct offensive
 *   jihad against all non-Muslims until conversion or submission. This
 *   reading justifies expansionist military movements, suppresses
 *   coexistence-affirming theology, and treats non-Muslim populations as
 *   standing objects of legitimate violence. The constraint embodies a
 *   hermeneutical claim (about Quranic meaning) that has institutional and
 *   coercive consequences (it legitimates violence, suppresses alternative
 *   interpretations, and positions non-Muslims as perpetual victims). The
 *   reading instantiates a Snare: high extraction from non-Muslim victim
 *   populations (violence, subordination), high suppression (alternative
 *   readings are institutionally marginalized, coexistence frameworks are
 *   delegitimated), active enforcement (the reading is taught, preached, and
 *   wielded to justify military action), and concentrated benefit
 *   (expansionist movements gain theological warrant and organizational
 *   legitimacy). The constraint's persistence depends on institutional
 *   authority enforcing the reading as canonical, not on participant
 *   agreement.
 *
 * KEY AGENTS:
 *   - Expansionist jihadi movements: organized networks (ISIS, al-Qaeda, Islamic state movements) that endorse the reading and conduct operations justified by it; their organizational identity is fused to the reading's theological warrant
 *   - Non-Muslim populations: globally dispersed, institutionally powerless; face targeting, forced subordination, displacement, or forced conversion under this reading's logic
 *   - Moderate Islamic scholars: institutional figures holding alternative readings (contextual, defensive, progressive); systematically marginalized when the abrogating reading achieves institutional dominance
 *   - Islamic jurisprudential authority: madhab institutions, state-affiliated scholars, grand muftis; their legitimacy partly depends on settling canonical interpretation; some endorse the reading, others suppress it
 *   - Coexistence frameworks: institutional arrangements (pluralist theology, interfaith dialogue, mutual recognition treaties); delegitimated as haram (forbidden) under the reading's logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__abrogating_universal, 0.89).
domain_priors:suppression_score(quran_9_5_scope__abrogating_universal, 0.91).
domain_priors:theater_ratio(quran_9_5_scope__abrogating_universal, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, extractiveness, 0.89).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, resistance, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__abrogating_universal, snare).
narrative_ontology:human_readable(quran_9_5_scope__abrogating_universal, "Verse 9:5 Abrogating Universal Offensive Jihad Mandate").
narrative_ontology:topic_domain(quran_9_5_scope__abrogating_universal, "religious/political/hermeneutic").

domain_priors:requires_active_enforcement(quran_9_5_scope__abrogating_universal).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__abrogating_universal, 'afebad1d-bc54-42c7-a538-e31efdf89e15').
narrative_ontology:cs_kernel_codification('afebad1d-bc54-42c7-a538-e31efdf89e15', fixed_text).
narrative_ontology:cs_authority_grounding('afebad1d-bc54-42c7-a538-e31efdf89e15', lineage).
narrative_ontology:cs_interpretation_layer_present('afebad1d-bc54-42c7-a538-e31efdf89e15').
narrative_ontology:cs_reading_relation('afebad1d-bc54-42c7-a538-e31efdf89e15', quran_9_5_scope__contextual_defensive, forecloses).
narrative_ontology:cs_reading_relation('afebad1d-bc54-42c7-a538-e31efdf89e15', quran_9_5_scope__progressive_synthesis, forecloses).
narrative_ontology:cs_axiom('afebad1d-bc54-42c7-a538-e31efdf89e15', foundational, verse_9_5_eternally_binding_mandate).
narrative_ontology:cs_axiom_status(verse_9_5_eternally_binding_mandate, holdable).
narrative_ontology:cs_axiom_grounding('afebad1d-bc54-42c7-a538-e31efdf89e15', verse_9_5_eternally_binding_mandate, deontological).
narrative_ontology:cs_axiom('afebad1d-bc54-42c7-a538-e31efdf89e15', foundational, polytheism_perpetual_threat_justifying_offensive_force).
narrative_ontology:cs_axiom_status(polytheism_perpetual_threat_justifying_offensive_force, holdable).
narrative_ontology:cs_axiom_grounding('afebad1d-bc54-42c7-a538-e31efdf89e15', polytheism_perpetual_threat_justifying_offensive_force, empirically_contingent).
narrative_ontology:cs_reference_frame('afebad1d-bc54-42c7-a538-e31efdf89e15', divine_command_immutability_and_perpetual_non_muslim_threat).
narrative_ontology:cs_drift_state('afebad1d-bc54-42c7-a538-e31efdf89e15', contemporary_international_coexistence_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('afebad1d-bc54-42c7-a538-e31efdf89e15', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__abrogating_universal, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, expansionist_jihadi_movements).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, non_muslim_populations).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, coexistence_frameworks).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, alternative_quranic_interpretations).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, moderate_islamic_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Endorses and acts on the reading that Verse 9:5 abrogates all prior peaceful injunctions, creating a standing obligation for offensive jihad against non-Muslims until conversion or submission. This reading legitimates their expansionist military operations and recruitment. Their organizational identity depends on the literal permanence of this obligation; exiting the reading would dissolve the theological warrant for their operations.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, expansionist_jihadi_movements, agenda_setter,
    organized, civilizational, identity_locked, global).

% Face systematic targeting as legitimate objects of offensive violence under this reading's logic. Their choice set is constrained to submission (conversion or dhimmi status with reduced rights), flight, or armed resistance. They bear the extraction through violence, displacement, enslavement, or forced status subordination.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, non_muslim_populations, payer,
    powerless, biographical, trapped, global).

% Bear costs from being systematically suppressed or marginalized within Islamic discourse when the abrogating reading dominates institutional authority. Their alternative interpretations (contextual, progressive, coexistence-affirming) are treated as heretical deviation or naïve modernism. Their professional standing and institutional position depend on accommodation to the dominant reading.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, moderate_islamic_scholars, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__abrogating_universal, moderate_islamic_scholars, excluded).

% Institutional and intellectual arrangements grounded in pluralism, treaty obligations, and mutual recognition between Muslims and non-Muslims are suppressed as illegitimate under this reading. The reading's logic treats coexistence as either temporary tactical arrangement or forbidden shirk (divine partnership). Coexistence frameworks lose doctrinal grounding and political protection.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, coexistence_frameworks, payer,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(quran_9_5_scope__abrogating_universal, coexistence_frameworks).

% Verses prioritizing mercy, forgiveness, religious freedom, and defensive-only warfare (2:256, 8:61, 60:8-9) are treated as abrogated and void. Their doctrinal authority is suppressed; they cannot ground legal rulings or ethical frameworks. The abrogating reading establishes a single hierarchy of Quranic authority that erases textual plurality.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, alternative_quranic_interpretations, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(quran_9_5_scope__abrogating_universal, alternative_quranic_interpretations).

% The reading's 7th-century historical targets (treaty-breaking Meccan tribes, pagan Arabia). Structured out of the conversation by historicity; the reading claims the verse's scope extends beyond them to all non-Muslims in perpetuity. This expansion—from specific context to universal application—is the hermeneutical pivot point of contention.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, pre_islamic_arabian_polytheists, excluded,
    powerless, immediate, trapped, local).

% Authority structures claiming to adjudicate Quranic meaning (madhabs, state religious institutions, scholarly councils) either endorse or suppress this reading through curricula, fatwas, and institutional hierarchy. Their legitimacy depends partly on settling canonical interpretation; endorsing the abrogating reading cements institutional control over coercive authority.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, islamic_jurisprudential_authority, agenda_setter,
    institutional, civilizational, analytical, global).

% Examines the reading as one among multiple internally coherent jurisprudential framings of the Quranic text. Identifies structural differences: the abrogating reading treats Verse 9:5 as an eternally binding legal mandate, while contextual and progressive readings treat it as historically bound or theologically superseded by an ethical trajectory.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, comparative_theological_analysis, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_9_5_scope__abrogating_universal, expansionist_jihadi_movements).
narrative_ontology:fixing_cost_class(quran_9_5_scope__abrogating_universal, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Nominal only: the reading claims to provide unified, divinely-mandated action coordination for believers. In structure, it produces no genuine coordination benefit—it is a hierarchy imposing a coercive mandate, not participants solving a collective-action problem.
% TRANSFER_FUNCTION: Transfers authority, legitimacy, organizational capital, and recruitment capacity to expansionist movements. Transfers territory, status, wealth, and forced religious identity from non-Muslim populations to Muslim authority structures. Transfers doctrinal legitimacy from coexistence frameworks to exclusion frameworks.
% ABSENT_VOICES: Non-Muslims subject to the mandate are completely excluded—they have no voice in the interpretation that justifies their targeting. Moderate Islamic scholars are institutionally excluded when the reading achieves dominance. Historical context (the specific 7th-century tribes the verse originally addressed) is excluded from contemporary interpretive discourse. Scholars of Quranic ethics and internal textual contradiction are excluded from authority when the literal-permanence reading dominates.
% DISAPPEARANCE_RATIONALE: If this reading's institutional authority disappeared, the theological warrant for expansionist movements would collapse, their recruitment and organizational legitimacy would plummet, international military campaigns justified by it would lose theological cover. Alternative readings (contextual, progressive, coexistence-affirming) would resurface. Interfaith and political arrangements between Muslim-majority regions and non-Muslim actors would shift from subordination toward mutual recognition. Non-Muslim populations would cease to be treated as standing objects of legitimate violence.
% FOUNDING_PROBLEM: 7th-century Medina: the early Muslim polity faced military threat from Meccan pagan Arab tribes (Quraysh, allies) who had broken treaties and conducted raids. Verse 9:5 establishes a military response to this specific, delimited threat.
% FOUNDING_PROBLEM_CORROBORATION: Islamic historical scholarship (al-Tabari, Ibn Hisham, modern Islamic historians) confirms the 7th-century Arabian tribal warfare as the original context. The reading claims the founding problem persists eternally (non-Muslim polytheism as perpetual threat), but this requires extending 'polytheism' to include all non-Muslim belief-systems globally and eternally. Historical jurisprudence across all madhabs shows centuries of treaty-making with non-Muslim states, which contradicts the claim that non-Muslims are perpetual threatening targets. Contemporary moderate Islamic scholars and historians attest the founding problem is historically bounded; expansionist movements and fundamentalist scholars attest it is eternal. No external corroborating voice (neither historical analysis nor contemporary theological scholarship outside the benefiting parties) supports the eternal-threat extension.
narrative_ontology:disappearance_verdict(quran_9_5_scope__abrogating_universal, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__abrogating_universal, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__abrogating_universal, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_9_5_scope__abrogating_universal, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__abrogating_universal, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__abrogating_universal_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_9_5_scope__abrogating_universal, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_9_5_scope__abrogating_universal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at the current interval-end state (0.89) reflecting sustained, irreversible harm extraction from non-Muslim populations—violence, forced status, displacement, enslavement. The temporal series (0.62 → 0.89 over 1400 years) models the reading's institutional consolidation: early doctrinal contestation (century 0–200: nasikh doctrine itself was still developing, multiple interpretations competed) to later institutional dominance (century 800–1400: the abrogating reading became canonical in most madhabs, alternative readings were marginalized, expansionist movements claimed it as warrant). Suppression is high (0.91) because the reading's persistence requires active, continuous institutional enforcement: curricula must teach it, alternative readings must be excluded or reframed as deviation, non-Muslim coexistence must be suppressed as theologically illegitimate. Theater is low (0.22) because the reading's function is not performative—it directly legitimates violence. The small theatrical component reflects institutional ritual (formal fatwas, scholarly debates, sermon performances) that reinforces the reading but is not its primary mechanism. Accessibility collapse is high (0.78) because once the reading enters institutional discourse, alternatives become intellectually and institutionally inaccessible—you cannot hold alternative readings without professional penalty, community rupture, or identity dissolution. Resistance is high (0.82) because the reading has always faced counter-evidence and counter-interpretation: historical scholarship showing 7th-century context, ethical verses affirming religious freedom, centuries of jurisprudential treaties with non-Muslims, modernity producing coexistence arrangements that work—all generate sustained resistance, yet the reading persists through institutional gatekeeping.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (expansionist movements, some institutional authorities) experiences the reading as legitimate theological ground for commanded action—world-aligning, duty-fulfilling, divinely mandated. The payer seats (non-Muslims, moderate scholars, coexistence frameworks) experience it as violent subjugation backed by theological claims they contest. The observer seat (comparative theological analysis) sees it as one internally coherent reading among multiple coherent readings of the same text, none of which is empirically settled. The engine computes per-seat types: the agenda-setter seat likely computes rope (benefits from coordination of believers under a shared mandate); the payer seats compute snare (extraction with no exit, suppression of alternatives, coercion-dependent persistence); the observer seat computes tangled_rope (coordination function for believers, extraction from non-believers, asymmetric). This perspectival gap is not resolved in the authored story—it is the gap the constraint creates and the engine measures.
 *
 * DIRECTIONALITY LOGIC:
 *   Expansionist movements are full beneficiaries (d = 0.0 to 0.1): they gain theological warrant, organizational legitimacy, and control over authority-setting; exit is identity-locked (leaving the reading dissolves organizational identity). Non-Muslim populations are full targets (d = 0.9 to 1.0): they are extraction victims, face violence and subordination, and have no exit (trapped: conversion is forced, flight is displacement, resistance is framed as apostasy). Moderate Islamic scholars sit near the target end (d = 0.6 to 0.8): they are suppressed, institutionally marginalized, cannot teach alternative readings without penalty; exit is identity-locked (leaving Islam as a professional identity means losing career, community standing, institutional access). Islamic jurisprudential authority institutions are mixed (d = 0.3 to 0.6): they benefit from setting canonical interpretation (which centralizes their authority), but also bear cost if the reading generates political blowback, international sanctions, or internal dissent. Their exit is constrained (they cannot simply abandon the reading without institutional collapse), but they retain some degree of freedom in how strictly they enforce it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (7th-century Medinan conflict with specific polytheist tribes) is dead—that specific threat was resolved 1400 years ago. But the reading claims the obligation persists eternally because non-Muslim polytheism is a perpetual condition. This is the mandatrophy: the original warrant (remove a specific threat) has expired, but the constraint persists through extension of the threat-category to all non-Muslims perpetually. The classification as Snare (not Piton) reflects that the constraint is not yet inert—it actively legitimates violence and suppresses alternatives. But the combination (dead founding problem + persistent constraint + high theater ratio relative to function) marks mandatrophy as an unresolved question: if the threat is gone, what function does the constraint serve? The reading answers: the function is eternal obligation independent of contingent threat. But this answer is exactly what the alternative readings contest, so mandatrophy remains the central axis of disagreement among the kernel's readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    abrogating_vs_coexisting_axioms,
    'Is the fundamental axiom of the abrogating-universal reading that Quranic legal imperatives are immutable and eternal (once commanded, forever binding), or is it that non-Muslim submission/conversion is an eternal condition (the threat never ends because polytheism never ends)?',
    'Textual analysis of how classical jurisprudents justify the reading''s permanence. Do they argue from the logical form of divine commands, or from the perpetual condition of non-Muslim existence?',
    'If grounded in command-form immutability, the axiom is deontological and cannot be refuted by evidence. If grounded in perpetual non-Muslim existence, the axiom is empirically contingent and could be challenged by evidence that non-Muslims have abandoned polytheism or accepted Islamic governance peacefully.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abrogating_vs_coexisting_axioms, empirical, 'Whether the permanence claim rests on logical form or empirical condition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__abrogating_universal, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_9_5_scope__abrogating_universal, theater_ratio, 0, 0.08).
narrative_ontology:measurement(qura_tr_t200, quran_9_5_scope__abrogating_universal, theater_ratio, 200, 0.12).
narrative_ontology:measurement(qura_tr_t400, quran_9_5_scope__abrogating_universal, theater_ratio, 400, 0.15).
narrative_ontology:measurement(qura_tr_t800, quran_9_5_scope__abrogating_universal, theater_ratio, 800, 0.18).
narrative_ontology:measurement(qura_tr_t1200, quran_9_5_scope__abrogating_universal, theater_ratio, 1200, 0.21).
narrative_ontology:measurement(qura_tr_t1400, quran_9_5_scope__abrogating_universal, theater_ratio, 1400, 0.22).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_9_5_scope__abrogating_universal, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(qura_be_t200, quran_9_5_scope__abrogating_universal, base_extractiveness, 200, 0.71).
narrative_ontology:measurement(qura_be_t400, quran_9_5_scope__abrogating_universal, base_extractiveness, 400, 0.78).
narrative_ontology:measurement(qura_be_t800, quran_9_5_scope__abrogating_universal, base_extractiveness, 800, 0.84).
narrative_ontology:measurement(qura_be_t1200, quran_9_5_scope__abrogating_universal, base_extractiveness, 1200, 0.87).
narrative_ontology:measurement(qura_be_t1400, quran_9_5_scope__abrogating_universal, base_extractiveness, 1400, 0.89).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_9_5_scope__abrogating_universal, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(qura_su_t200, quran_9_5_scope__abrogating_universal, suppression_requirement, 200, 0.76).
narrative_ontology:measurement(qura_su_t400, quran_9_5_scope__abrogating_universal, suppression_requirement, 400, 0.82).
narrative_ontology:measurement(qura_su_t800, quran_9_5_scope__abrogating_universal, suppression_requirement, 800, 0.87).
narrative_ontology:measurement(qura_su_t1200, quran_9_5_scope__abrogating_universal, suppression_requirement, 1200, 0.89).
narrative_ontology:measurement(qura_su_t1400, quran_9_5_scope__abrogating_universal, suppression_requirement, 1400, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__abrogating_universal, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(quran_9_5_scope__abrogating_universal, 0.18).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, quran_9_5_scope__contextual_defensive).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, quran_9_5_scope__progressive_synthesis).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, islamic_coexistence_doctrine).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, jihad_legitimation_doctrine).

% DUAL FORMULATION NOTE:
% The kernel quran_9_5_scope decomposes into three distinct constraints, each instantiating a different reading of Verse 9:5. The abrogating-universal reading (this story) treats the verse as eternally binding, establishing a standing obligation for offensive jihad. The contextual-defensive reading treats it as historically bounded to 7th-century Arabia, with no abrogating force over peaceful verses. The progressive-synthesis reading treats it as a time-bound directive superseded by Quranic ethical trajectory. Each reading authorizes a different scope of violence and produces different victim-sets, thus different extractiveness values and different beneficiary structures. The three stories are linked via network.affects_constraints: the abrogating-universal reading forecloses coexistence doctrine and influences the scope of the contextual-defensive reading (if universal abrogation is true, contextual readings cannot limit the obligation's duration). The readings instantiate the same kernel text (Verse 9:5) but are structurally independent constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_9_5_scope__abrogating_universal, organized, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
