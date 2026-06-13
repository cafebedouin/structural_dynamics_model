% ============================================================================
% CONSTRAINT STORY: correct_latin__discontinuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin__discontinuity_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: correct_latin__discontinuity_reading
 *   human_readable: Classical Latin Discontinuity Doctrine
 *   domain: historical_linguistics/intellectual_history
 *
 * SUMMARY:
 *   The discontinuity reading is one of three competing hermeneutics of
 *   'correct Latin' during the Renaissance and early modern period. This
 *   reading asserts that Classical Latin (the form preserved in ancient texts
 *   like Cicero, Virgil, Livy) is the one true standard; medieval Latin is a
 *   corrupt degradation that departed from Classical purity through centuries
 *   of linguistic drift, scribal error, and institutional isolation from the
 *   Classical canon. Humanist philologists developed sophisticated textual
 *   methods to recover Classical forms from manuscript evidence, treating
 *   medieval usage as a deviant path requiring correction. This reading
 *   shaped educational institutions, editorial standards, and the prestige
 *   structures of Renaissance learning. The constraint ENFORCES a hierarchy
 *   of legitimacy: Classical forms occupy the ceiling; medieval forms are
 *   labeled corruption; recovery via textual evidence is declared the proper
 *   scholarly task. The kernel contest is fundamental: continuity_reading
 *   treats medieval Latin as legitimate evolution of Classical Latin;
 *   hybrid_reading accepts discontinuity but permits medieval texts as
 *   sources for reconstruction; discontinuity_reading declares medieval forms
 *   simply wrong and textual recovery the sole path to authority.
 *
 * KEY AGENTS:
 *   - humanist_philologists: institutional power — set editorial standards, define correctness, control university curricula; arbitrage exit
 *   - classical_education_institutions: institutional beneficiaries — prestige and institutional identity accrue; mobile exit
 *   - medieval_scribes: powerless victims — retroactively judged corrupt; trapped
 *   - medieval_scholars: moderate victims — scholarly authority shifts away from medieval exemplars; constrained exit
 *   - church_latin_practitioners: organized payers — maintain two registers, absorb curriculum reform costs; constrained
 *   - historical_linguists: analytical observers — evidence suggests continuity and transmission, not corruption; analytical exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__discontinuity_reading, 0.68).
domain_priors:suppression_score(correct_latin__discontinuity_reading, 0.72).
domain_priors:theater_ratio(correct_latin__discontinuity_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__discontinuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin__discontinuity_reading, "Classical Latin Discontinuity Doctrine").
narrative_ontology:topic_domain(correct_latin__discontinuity_reading, "historical_linguistics/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin__discontinuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__discontinuity_reading, 'faaa0845-4082-48a0-81e7-1a79858c571e').
narrative_ontology:cs_kernel_codification('faaa0845-4082-48a0-81e7-1a79858c571e', fixed_text).
narrative_ontology:cs_authority_grounding('faaa0845-4082-48a0-81e7-1a79858c571e', expertise).
narrative_ontology:cs_interpretation_layer_present('faaa0845-4082-48a0-81e7-1a79858c571e').
narrative_ontology:cs_reading_relation('faaa0845-4082-48a0-81e7-1a79858c571e', correct_latin__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('faaa0845-4082-48a0-81e7-1a79858c571e', correct_latin__hybrid_reading, influences).
narrative_ontology:cs_axiom('faaa0845-4082-48a0-81e7-1a79858c571e', foundational, classical_textual_purity_is_normative).
narrative_ontology:cs_axiom_status(classical_textual_purity_is_normative, holdable).
narrative_ontology:cs_axiom_grounding('faaa0845-4082-48a0-81e7-1a79858c571e', classical_textual_purity_is_normative, deontological).
narrative_ontology:cs_axiom('faaa0845-4082-48a0-81e7-1a79858c571e', foundational, medieval_linguistic_forms_are_corrupt_deviation).
narrative_ontology:cs_axiom_status(medieval_linguistic_forms_are_corrupt_deviation, holdable).
narrative_ontology:cs_axiom_grounding('faaa0845-4082-48a0-81e7-1a79858c571e', medieval_linguistic_forms_are_corrupt_deviation, empirically_contingent).
narrative_ontology:cs_reference_frame('faaa0845-4082-48a0-81e7-1a79858c571e', classical_latin_perfection).
narrative_ontology:cs_drift_state('faaa0845-4082-48a0-81e7-1a79858c571e', renaissance_humanist_recovery, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('faaa0845-4082-48a0-81e7-1a79858c571e', '').
narrative_ontology:cs_kernel_id(correct_latin__discontinuity_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, humanist_philologists).
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, classical_education_institutions).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, medieval_scribes).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, medieval_scholars).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, church_latin_practitioners).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__discontinuity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(correct_latin__discontinuity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__discontinuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin__discontinuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin__discontinuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.38 (1350, pre-humanist) to 0.68 (1700, institutionalized) because the constraint's enforcement machinery hardens over the interval. Early (1350–1425): humanists recover Classical texts and articulate the superiority doctrine — extraction is still contested, resistance high. Mid (1425–1575): universities adopt humanist curricula, Classical editions become standard, medieval texts are relegated to examples of corruption — extraction intensifies as institutional authority consolidates, suppression rises to enforce the new standard. Late (1575–1700): discontinuity reading is institutionalized; generation of scholars trained only in Classical standards; medieval Latin is thoroughly delegitimized; further extraction gains flatten as institutional capture completes. Theater_ratio rises from 0.22 to 0.44 because the core function (coordinate on a unified written standard) is real but increasingly overlaid with performative validation of the humanist agenda: emendation of medieval texts becomes a showcase of scholarly rigor rather than a practical necessity for communication. Suppression requirement rises with institutional capture: the constraint requires active enforcement of editorial standards, curriculum control, and deprecation of medieval exemplars because alternatives (continuity frameworks, medieval legitimate usage) persist in living practice. Accessibility_collapse rises steeply at all levels: by 1700, a medieval scribe or Church practitioner has no socially acceptable option but to learn Classical standards in formal education, even if medieval practice remains embedded in liturgy. Resistance declines as the generational turnover produces scholars trained only in discontinuity framing. One shared measurement grid throughout: every metric sampled at 1350, 1425, 1500, 1575, 1650, 1700.
 *
 * PERSPECTIVAL GAP:
 *   Humanist philologists experience the constraint as enabling genuine correction and intellectual progress — they solve the communication problem by fixing on Classical purity, and they gain institutional prestige. Medieval practitioners experience it as retroactive condemnation — their linguistic choices, legitimate in their own time, are reclassified as error by external textual standards. Church Latin maintainers experience it as costly curriculum burden — they must teach Classical standards while medieval practice remains embedded in liturgy and institutional routine. The engine should compute divergent types across these seats: humanist agenda-setter seats may compute as rope (beneficial coordination), while victim and payer seats compute as snare or tangled_rope (enforced extraction). The structural data support this divergence: the beneficiaries have arbitrage and mobile exits; the victims have trapped or constrained exits.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanist_philologists: beneficiary role, institutional power, arbitrage exit → d low, benefits subsidized. Classical_education_institutions: beneficiary role, institutional power, mobile exit → d low to symmetric, gain prestige without being locked in. Medieval_scribes: victim role, powerless, trapped → d high, bear retroactive delegitimation. Medieval_scholars: victim/payer roles, moderate power, constrained → d high, authority shifts away. Church_latin_practitioners: payer role, organized power, constrained → d high, forced curriculum reform. The directionality profile is asymmetric by design: beneficiaries have exit and power; victims have neither. This asymmetry is encoded in exit_options and power atoms; the engine derives directionality from these data without override needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The discontinuity doctrine declares as its founding problem the need to recover Classical authenticity from textual evidence. By 1700, the founding problem status is contested: humanists attest it remains live (Classical texts still need expert recovery); continuity advocates attest it is dead (medieval transmission already preserves Classical Latin adequately, just evolved). The disappearance verdict is world_rearranges because if the discontinuity constraint vanished, institutional authority would shift toward medieval texts as legitimate exemplars, and the recovery enterprise would reorient. The mismatch (status=contested, verdict=world_rearranges) is the gateway to mandatrophy questions: the constraint persists not because the founding problem is universally live, but because humanist institutions benefit from treating it as live. Theater_ratio rising from 0.22 to 0.44 suggests increasing performative maintenance — the editorial precision and textual recovery machinery becomes an end in itself, theater for humanist prestige, not a means to practical communication goals. The constraint shows piton-adjacent dynamics (performative maintenance of an atrophied function) without full piton classification because genuine beneficiaries (humanist institutions) still profit from it and actively defend it. This is Tangled Rope: coordination function (unified standard) exists; extraction function (prestige and authority transfer) is asymmetric; active enforcement maintains the medieval delegitimation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    corruption_vs_evolution,
    'Is the difference between Classical and medieval Latin forms best characterized as corruption (deviation from a fixed standard) or as natural linguistic evolution (regular sound change, morphological simplification, lexical replacement)?',
    'Comparative phonology and morphology: if medieval forms show regular correspondences to Classical forms (e.g., systematic vowel reductions, predictable case conflations), they are evolution; if they are sporadic, unsystematic, and reversible only via external textual authority, they are corruption.',
    'If evolution, the continuity_reading is structurally correct and discontinuity_reading loses its foundational premise; the constraint would reclassify as snare (pure extraction with no coordination function). If corruption, discontinuity_reading is validated. Historical linguistics evidence supports evolution; the discontinuity doctrine persists despite this evidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(corruption_vs_evolution, empirical, 'Whether medieval Latin is corrupt or evolved.').

omega_variable(
    textual_authority_closure,
    'Does the discontinuity reading''s anchoring legitimacy in manuscript sources exclude living linguistic practice and continuity witnesses as valid evidence by methodological rule, or is this exclusion empirically justified?',
    'Epistemological audit: if the exclusion of living testimony or practice evidence is declared a priori (methodological rule of the discontinuity framework), it is a closure mechanism enforcing the reading; if it is empirically justified (living users cannot be consulted because they are dead), the exclusion is epistemic necessity, not enforcement.',
    'If closure mechanism, the constraint''s suppression is higher than measured — it actively suppresses alternative evidence classes. If epistemic necessity, the constraint''s enforced character is weaker. The discontinuity reading explicitly adopts textual sources as the sole arbiter, which suggests methodological closure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_authority_closure, conceptual, 'Whether the discontinuity reading''s evidentiary closure is enforced or empirically necessary.').

omega_variable(
    transmission_chain_visibility,
    'Does the discontinuity reading acknowledge the chain of medieval copyists and practitioners as legitimate transmitters of Classical Latin, or does it treat them as sources of corruption obscuring the true form?',
    'Historiographical analysis: if medieval scribes are credited with faithful transmission (even if imperfect), the continuity link is acknowledged and the corruption thesis is softened; if they are treated as sources of error, the transmission chain is delegitimized and the rupture is declared absolute.',
    'If transmission chain is acknowledged, the discontinuity reading would need to explain how corruption arose in transmission yet Classical forms survived. This weakens the claim to absolute rupture. If transmission chain is delegitimized, the reading depends on treating medieval practitioners as failed guardians, which supports extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_chain_visibility, empirical, 'Whether medieval transmitters are credited or blamed in the discontinuity narrative.').

omega_variable(
    reading_kernel_contest,
    'This constraint instantiates ONE reading of a contested kernel: the question of what counts as ''correct Latin.'' Are the three sibling readings (continuity_reading, hybrid_reading, discontinuity_reading) logically foreclosed by each other, or do they coexist as live positions held by different institutional actors?',
    'Institutional and epistemological analysis: if a scholar adopting continuity_reading''s core premise (medieval Latin is legitimate evolution) must logically reject discontinuity_reading''s core premise (medieval Latin is corrupt deviation), the readings foreclose each other in a single framework. If different institutions hold the readings simultaneously without logical contradiction (just disagreement), they coexist.',
    'If foreclosed, the constraint represents a zero-sum contest with a winner and loser; the extraction is part of winning the contest. If coexistent, the constraint is one framework among live alternatives; its extraction is sustained by institutional power, not logical necessity. Historical evidence shows all three readings active in different scholarly and ecclesial traditions; they coexist rather than foreclose.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_contest, conceptual, 'Whether sibling readings of the correct_latin kernel logically foreclose or coexist.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__discontinuity_reading, 1350, 1700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t1350, correct_latin__discontinuity_reading, theater_ratio, 1350, 0.22).
narrative_ontology:measurement(corr_tr_t1425, correct_latin__discontinuity_reading, theater_ratio, 1425, 0.29).
narrative_ontology:measurement(corr_tr_t1500, correct_latin__discontinuity_reading, theater_ratio, 1500, 0.36).
narrative_ontology:measurement(corr_tr_t1575, correct_latin__discontinuity_reading, theater_ratio, 1575, 0.41).
narrative_ontology:measurement(corr_tr_t1650, correct_latin__discontinuity_reading, theater_ratio, 1650, 0.44).
narrative_ontology:measurement(corr_tr_t1700, correct_latin__discontinuity_reading, theater_ratio, 1700, 0.44).

% Extraction over time
narrative_ontology:measurement(corr_be_t1350, correct_latin__discontinuity_reading, base_extractiveness, 1350, 0.38).
narrative_ontology:measurement(corr_be_t1425, correct_latin__discontinuity_reading, base_extractiveness, 1425, 0.48).
narrative_ontology:measurement(corr_be_t1500, correct_latin__discontinuity_reading, base_extractiveness, 1500, 0.58).
narrative_ontology:measurement(corr_be_t1575, correct_latin__discontinuity_reading, base_extractiveness, 1575, 0.65).
narrative_ontology:measurement(corr_be_t1650, correct_latin__discontinuity_reading, base_extractiveness, 1650, 0.68).
narrative_ontology:measurement(corr_be_t1700, correct_latin__discontinuity_reading, base_extractiveness, 1700, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t1350, correct_latin__discontinuity_reading, suppression_requirement, 1350, 0.45).
narrative_ontology:measurement(corr_su_t1425, correct_latin__discontinuity_reading, suppression_requirement, 1425, 0.55).
narrative_ontology:measurement(corr_su_t1500, correct_latin__discontinuity_reading, suppression_requirement, 1500, 0.62).
narrative_ontology:measurement(corr_su_t1575, correct_latin__discontinuity_reading, suppression_requirement, 1575, 0.68).
narrative_ontology:measurement(corr_su_t1650, correct_latin__discontinuity_reading, suppression_requirement, 1650, 0.71).
narrative_ontology:measurement(corr_su_t1700, correct_latin__discontinuity_reading, suppression_requirement, 1700, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__discontinuity_reading, information_standard).
narrative_ontology:boltzmann_floor_override(correct_latin__discontinuity_reading, 0.08).
narrative_ontology:affects_constraint(correct_latin__discontinuity_reading, correct_latin__continuity_reading).
narrative_ontology:affects_constraint(correct_latin__discontinuity_reading, correct_latin__hybrid_reading).

% DUAL FORMULATION NOTE:
% The correct_latin kernel generates three structurally distinct constraints corresponding to three readings of what counts as 'correct Latin': discontinuity_reading (this story, Classical form preserved in texts, medieval forms corrupt), continuity_reading (medieval Latin is legitimate evolution of Classical), and hybrid_reading (Classical form accessible via medieval transmission with targeted textual correction). Each reading instantiates a different constraint because the ε-invariance principle requires: a constraint whose legitimacy source changes (textual authority vs. continuity vs. hybrid) with the reading has different extraction profiles, different suppression mechanisms, and different victim/beneficiary structures. The three constraints form a family linked by the kernel_id; they share no ε value. All three must be generated independently; this story is discontinuity_reading only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(correct_latin__discontinuity_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
