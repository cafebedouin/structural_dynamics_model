% ============================================================================
% CONSTRAINT STORY: orthographic_kernel__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_kernel__continuity_reading, []).

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
 *   constraint_id: orthographic_kernel__continuity_reading
 *   human_readable: Arabic Script as Guarantor of Ottoman-Islamic Textual Continuity
 *   domain: political_linguistics/state_formation/religious_institutions
 *
 * SUMMARY:
 *   This story instantiates the continuity reading of the late Ottoman
 *   orthographic kernel: the claim that retaining Arabic script is necessary
 *   to preserve unbroken access to Islamic textual and Ottoman administrative
 *   tradition. As the theological-institutional establishment reads it, the
 *   arrangement is a genuine coordination device protecting continuity of
 *   interpretation across generations. But the same structure that anchors
 *   continuity also entrenches the ulema's interpretive monopoly and the
 *   bureaucratic-literate class's scarce-skill rents, while foreclosing state
 *   modernization reform paths (mass literacy, printing cost reduction,
 *   telegraphy efficiency) that the sibling modernization_reading and
 *   rupture_reading constraints would open. This is a tangled rope: real
 *   coordination function (textual continuity) plus asymmetric extraction
 *   (literate class capture, blocked reform) sustained by active theological
 *   and institutional enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_kernel__continuity_reading, 0.61).
domain_priors:suppression_score(orthographic_kernel__continuity_reading, 0.55).
domain_priors:theater_ratio(orthographic_kernel__continuity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_kernel__continuity_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_kernel__continuity_reading, "Arabic Script as Guarantor of Ottoman-Islamic Textual Continuity").
narrative_ontology:topic_domain(orthographic_kernel__continuity_reading, "political_linguistics/state_formation/religious_institutions").

domain_priors:requires_active_enforcement(orthographic_kernel__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_kernel__continuity_reading, 'ecf60f02-7755-4ae8-945c-bb9cd6d4b5b2').
narrative_ontology:cs_kernel_codification('ecf60f02-7755-4ae8-945c-bb9cd6d4b5b2', fixed_text).
narrative_ontology:cs_authority_grounding('ecf60f02-7755-4ae8-945c-bb9cd6d4b5b2', lineage).
narrative_ontology:cs_interpretation_layer_present('ecf60f02-7755-4ae8-945c-bb9cd6d4b5b2').
narrative_ontology:cs_reading_relation('ecf60f02-7755-4ae8-945c-bb9cd6d4b5b2', orthographic_kernel__modernization_reading, coexists_with).
narrative_ontology:cs_reading_relation('ecf60f02-7755-4ae8-945c-bb9cd6d4b5b2', orthographic_kernel__rupture_reading, forecloses).
narrative_ontology:cs_axiom('ecf60f02-7755-4ae8-945c-bb9cd6d4b5b2', foundational, script_is_constitutive_of_sacred_textual_meaning).
narrative_ontology:cs_axiom_status(script_is_constitutive_of_sacred_textual_meaning, holdable).
narrative_ontology:cs_axiom_grounding('ecf60f02-7755-4ae8-945c-bb9cd6d4b5b2', script_is_constitutive_of_sacred_textual_meaning, theological).
narrative_ontology:cs_axiom('ecf60f02-7755-4ae8-945c-bb9cd6d4b5b2', foundational, continuity_with_ottoman_administrative_tradition_is_a_legitimacy_requirement).
narrative_ontology:cs_axiom_status(continuity_with_ottoman_administrative_tradition_is_a_legitimacy_requirement, holdable).
narrative_ontology:cs_axiom_grounding('ecf60f02-7755-4ae8-945c-bb9cd6d4b5b2', continuity_with_ottoman_administrative_tradition_is_a_legitimacy_requirement, conventional).
narrative_ontology:cs_reference_frame('ecf60f02-7755-4ae8-945c-bb9cd6d4b5b2', classical_islamic_textual_authority).
narrative_ontology:cs_drift_state('ecf60f02-7755-4ae8-945c-bb9cd6d4b5b2', late_tanzimat_reform_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('ecf60f02-7755-4ae8-945c-bb9cd6d4b5b2', '').
narrative_ontology:cs_kernel_id(orthographic_kernel__continuity_reading, orthographic_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_kernel__continuity_reading, ulema_class).
narrative_ontology:constraint_beneficiary(orthographic_kernel__continuity_reading, ottoman_literate_bureaucracy).
narrative_ontology:constraint_beneficiary(orthographic_kernel__continuity_reading, religious_endowment_institutions).
narrative_ontology:constraint_victim(orthographic_kernel__continuity_reading, mass_literacy_aspirants).
narrative_ontology:constraint_victim(orthographic_kernel__continuity_reading, provincial_populations).
narrative_ontology:constraint_victim(orthographic_kernel__continuity_reading, state_modernization_reformers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls interpretive authority over religious and legal texts written in Arabic script; their social standing, income from religious education, and monopoly on Quranic exegesis depend on Arabic script remaining the medium of literacy. They actively argue script continuity is theologically necessary, not merely customary, and use fatwa and pedagogical control to resist alternatives.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, ulema_class, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel__continuity_reading, ulema_class, agenda_setter).

% Has invested years mastering Arabic-script Ottoman Turkish, a system requiring memorization of thousands of ligatures and irregular vowel representation; this scarce skill is the entry barrier that secures their administrative positions. A script change would devalue their accumulated capital overnight.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, ottoman_literate_bureaucracy, beneficiary,
    powerful, generational, constrained, national).

% Waqf-funded madrasas and mosque schools structure their curricula entirely around Arabic-script literacy; their funding streams and social legitimacy are built on being the sole gatekeepers of sacred and legal textual access.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, religious_endowment_institutions, beneficiary,
    institutional, civilizational, arbitrage, national).

% Ordinary subjects face a script system with inconsistent vowel marking and dozens of contextual letterforms, producing literacy rates estimated below 10 percent. They bear the cost of exclusion from written civic life with no meaningful alternative while the script is treated as sacrosanct.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, mass_literacy_aspirants, payer,
    powerless, biographical, trapped, national).

% Non-Turkish and rural Turkish speakers face compounding barriers: distance from urban madrasas, dialect mismatch with formal Ottoman orthography, and no printed vernacular material, deepening dependency on local religious authorities for any textual mediation.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, provincial_populations, payer,
    powerless, generational, trapped, regional).

% Tanzimat-era and later reform-minded bureaucrats and officers see the script as a structural bottleneck for printing, telegraphy, and mass education, and repeatedly propose reform; their proposals are blocked or diluted by religious-institutional resistance framed as defense of continuity, foreclosing the reform path this reading exists to hold shut.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, state_modernization_reformers, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel__continuity_reading, state_modernization_reformers, excluded).

% Early printers and would-be publishers face vastly higher typesetting costs and technical friction from Arabic script's contextual letterforms; they would benefit from simplification but have no institutional voice against the ulema's theological framing.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, print_capital_investors, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_kernel__continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(orthographic_kernel__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely coordinates continuity of access to a millennium of Islamic jurisprudence, Quranic recitation, and Ottoman administrative archive by keeping the reading community anchored to the script those texts were written in — a real interpretive-continuity problem for any literate tradition built on canonical texts.
% TRANSFER_FUNCTION: Moves interpretive authority, administrative gatekeeping power, and endowment revenue toward the ulema and the entrenched bureaucratic-literate class, at the cost of mass literacy access and state administrative modernization capacity, which are suppressed or delayed.
% ABSENT_VOICES: Provincial vernacular speakers, women excluded from madrasa education, and print-capital investors have no formal channel to contest the script; their objections surface only indirectly through later reformist polemics, not through direct participation in the continuity debate.
% DISAPPEARANCE_RATIONALE: If Arabic-script literacy monopoly vanished overnight, the ulema's interpretive gatekeeping would lose its structural anchor, the literate bureaucracy's scarce-skill advantage would evaporate, mass literacy campaigns could proceed unimpeded, and printing costs would collapse — precisely the rearrangement that did occur, on a delayed and coercive timeline, when the 1928 Turkish alphabet reform was later imposed by rupture.
% FOUNDING_PROBLEM: How to preserve unbroken access to the accumulated Islamic legal, theological, and Ottoman administrative textual corpus for a state whose legitimacy was substantially grounded in continuity with that corpus.
% FOUNDING_PROBLEM_CORROBORATION: The ulema and religious endowments attest the problem remains live indefinitely, framing any script change as sacrilege against textual transmission. European-trained Ottoman bureaucrats, foreign literacy surveys of the period, and post-1928 Turkish state historiography attest from outside the beneficiary set that the underlying continuity concern was real but had become substantially decoupled from its stated function by the late Ottoman period, serving primarily to entrench the literate class's monopoly rather than genuinely protect textual access, which could have been secured through parallel transliteration efforts.
narrative_ontology:disappearance_verdict(orthographic_kernel__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_kernel__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_kernel__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(orthographic_kernel__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_kernel__continuity_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_kernel__continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_kernel__continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_kernel__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.61) reflects substantial but not extreme rent-capture: the coordination function is real, not fabricated, but the literate class's resistance to even parallel transliteration schemes (which could have preserved continuity without blocking modernization) indicates the extraction component dominates by the late measurement window. Suppression (0.55) is moderate-high, driven by fatwa-based delegitimization of reform proposals and control of madrasa curricula rather than direct coercive force. Theater ratio rises across the measured interval (0.20 to 0.42) as the continuity justification increasingly serves to defend accumulated bureaucratic and religious privilege rather than genuine textual-access concerns, consistent with the founding_problem mismatch flagged in six_questions.
 *
 * DIRECTIONALITY LOGIC:
 *   The ulema, literate bureaucracy, and endowment institutions sit near the full-beneficiary end: they collect interpretive authority, administrative rents, and endowment revenue directly from script continuity, and their exit options are effectively arbitrage-grade (they can always retreat further into religious authority even if state modernization proceeds elsewhere). Mass literacy aspirants and provincial populations sit near the full-target end: trapped, powerless, bearing the cost of exclusion from written civic participation with no realistic alternative. State modernization reformers are payers in a different register — organized and powerful but structurally blocked, their reform proposals repeatedly foreclosed by the coordination-continuity framing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserving textual continuity) was and remains partly live — the theological corpus genuinely requires script-literate access. But by the late Ottoman period the arrangement had drifted from serving that function toward serving the literate class's positional advantage: parallel transliteration systems existed and could have decoupled continuity-preservation from literacy-gatekeeping, but were not pursued. This classification as tangled_rope rather than snare or mountain captures that the coordination function has not fully atrophied (distinguishing it from a pure snare) while the extraction is too structurally embedded and enforced to be a genuine mountain or pure rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_vs_capture_disentanglement,
    'Could textual continuity with the Islamic and Ottoman corpus have been preserved through parallel transliteration or dual-script scholarly infrastructure, decoupled from mass literacy policy — meaning the extraction component was separable from the coordination component all along?',
    'Comparative study of contemporaneous societies (e.g. later Republic-era Ottoman-script archival preservation efforts, or Persian/Urdu parallel-script scholarly traditions) that maintained textual continuity without gatekeeping mass literacy through the same script.',
    'If separable, the ulema/bureaucracy''s resistance to any script accommodation reveals the continuity claim as substantially a cover for positional rent-protection, strengthening the tangled_rope-to-snare drift already visible in the theater_ratio trend. If inseparable, more of the measured extraction is properly attributed to genuine coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_vs_capture_disentanglement, conceptual, 'Whether continuity and literacy-gatekeeping were structurally separable functions.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the orthographic kernel best framed as a single contested claim about ''the right script for the Ottoman/Turkish state,'' or does it decompose into genuinely independent sub-kernels (liturgical/interpretive continuity vs. administrative/print efficiency vs. national-identity construction) each with its own authority structure?',
    'Trace whether historical actors who held the continuity position on liturgical grounds also held modernization or rupture positions on administrative grounds — if actors'' positions varied independently across these dimensions, the kernel is multiple kernels wearing one label.',
    'If the kernel decomposes further, this story''s continuity_reading itself may need splitting into a liturgical-continuity reading (very high ε for literacy exclusion, near-mountain framing among ulema) and an administrative-continuity reading (lower ε, more genuinely rope-like coordination around archival consistency). Signals possible further decomposition beyond the three-reading split already declared.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the three declared readings fully exhaust the kernel''s structurally distinct claims.').

omega_variable(
    naturalness_of_sacred_script_claim,
    'Is the claim that Arabic script is intrinsically bound to Quranic sacred meaning (such that transliteration constitutes a theological loss, not merely a practical one) a defensible theological position independent of institutional interest, or is it itself a constructed doctrine that emerged to serve the literate class''s position?',
    'Comparative theological history: examine whether mainstream Islamic jurisprudence elsewhere (e.g. non-Arab Muslim-majority regions using Arabic script for liturgy while using other scripts for vernacular administration) treated script and sacred meaning as separable, which would suggest the strong fusion claim in the Ottoman case was locally constructed rather than doctrinally necessary.',
    'If the fusion is theologically necessary, part of this reading''s high ε is a genuine mountain-like religious constraint wrapped inside the tangled_rope''s coordination layer. If constructed, it strengthens the case that the continuity_reading functions substantially as beneficiary cover.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(naturalness_of_sacred_script_claim, empirical, 'Whether script-sacredness fusion is theologically load-bearing or institutionally constructed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_kernel__continuity_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t0, orthographic_kernel__continuity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(orth_tr_t10, orthographic_kernel__continuity_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(orth_tr_t20, orthographic_kernel__continuity_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement(orth_tr_t30, orthographic_kernel__continuity_reading, theater_ratio, 30, 0.33).
narrative_ontology:measurement(orth_tr_t40, orthographic_kernel__continuity_reading, theater_ratio, 40, 0.37).
narrative_ontology:measurement(orth_tr_t50, orthographic_kernel__continuity_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement(orth_tr_t60, orthographic_kernel__continuity_reading, theater_ratio, 60, 0.42).

% Extraction over time
narrative_ontology:measurement(orth_be_t0, orthographic_kernel__continuity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(orth_be_t10, orthographic_kernel__continuity_reading, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(orth_be_t20, orthographic_kernel__continuity_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(orth_be_t30, orthographic_kernel__continuity_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(orth_be_t40, orthographic_kernel__continuity_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(orth_be_t50, orthographic_kernel__continuity_reading, base_extractiveness, 50, 0.6).
narrative_ontology:measurement(orth_be_t60, orthographic_kernel__continuity_reading, base_extractiveness, 60, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t0, orthographic_kernel__continuity_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(orth_su_t10, orthographic_kernel__continuity_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(orth_su_t20, orthographic_kernel__continuity_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(orth_su_t30, orthographic_kernel__continuity_reading, suppression_requirement, 30, 0.49).
narrative_ontology:measurement(orth_su_t40, orthographic_kernel__continuity_reading, suppression_requirement, 40, 0.51).
narrative_ontology:measurement(orth_su_t50, orthographic_kernel__continuity_reading, suppression_requirement, 50, 0.53).
narrative_ontology:measurement(orth_su_t60, orthographic_kernel__continuity_reading, suppression_requirement, 60, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_kernel__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(orthographic_kernel__continuity_reading, 0.1).
narrative_ontology:affects_constraint(orthographic_kernel__continuity_reading, orthographic_kernel__modernization_reading).
narrative_ontology:affects_constraint(orthographic_kernel__continuity_reading, orthographic_kernel__rupture_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the orthographic_kernel (Ottoman/Turkish script contest). continuity_reading (this file) authors high ε for the Ottoman literate class's captured continuity claim and low ε for the blocked state-modernization path it forecloses. modernization_reading authors the Latin-script transition as substantially rope-like (coordination benefit for mass literacy and technical infrastructure, moderate extraction from the displaced literate class). rupture_reading authors the same 1928 transition as substantially tangled_rope-or-snare-leaning from the perspective of those whose religious/cultural continuity was severed by state fiat. All three share the same underlying historical event (the Ottoman-to-Latin script transition) but are structurally distinct constraints per the ε-invariance principle: each has a different victim set, a different beneficiary set, and a different ε, because each reading evaluates a different claim about what the arrangement is FOR.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(orthographic_kernel__continuity_reading, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
